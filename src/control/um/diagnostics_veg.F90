#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Subroutine diagnostics_veg ----------------------------------------
!
! Purpose : Calculates diagnostics for dynamic vegetation and
!           outputs them.
!
! -----------------------------------------------------------------
!
! Subroutine diagnostics_veg
!
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
! This file belongs in TECHNICAL, VEGETATION
MODULE diagnostics_veg_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName='DIAGNOSTICS_VEG_MOD'

CONTAINS

SUBROUTINE diagnostics_veg(                                                    &
              row_length, rows,                                                &
              land_pts, sm_levels,                                             &
              land_index,                                                      &
              ntype,npft,                                                      &
              c_veg,cv,g_leaf_phen,                                            &
              lit_c,lit_c_mn,g_leaf_day,                                       &
              lai_phen,g_leaf_dr_out,npp_dr_out,                               &
              resp_w_dr_out,frac_disturb,                                      &
              disturb_veg_prev,                                                &
              frac,                                                            &
              stashwork,                                                       &
              !JULES TYPEs
              trif_vars,progs,trifctltype)

! Purpose:
!          Calculates diagnostics and outputs them.
!
USE stash_array_mod, ONLY:                                                     &
    sf, si, stlist, stindex, len_stlist, stash_pseudo_levels,                  &
    num_stash_pseudo, stash_levels, num_stash_levels, si_last
USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
USE missing_data_mod, ONLY: rmdi

USE jules_vegetation_mod, ONLY: triffid_period

USE conversions_mod, ONLY: rsec_per_day

USE set_pseudo_list_mod, ONLY: set_pseudo_list

USE copydiag_3d_mod, ONLY: copydiag_3d
USE copydiag_mod, ONLY: copydiag

!JULES TYPEs
USE trif_vars_mod, ONLY: trif_vars_type
USE prognostics, ONLY: progs_type
USE trifctl, ONLY: trifctl_type

IMPLICIT NONE

! Arguments with Intent IN. ie: Input variables.

INTEGER ::                                                                     &
  row_length                                                                   &
                   ! number of points on a row
, rows
                   ! number of rows in a theta field

INTEGER ::                                                                     &
  land_pts                                                                     &
           ! No.of land points being processed, can be 0.
, sm_levels                                                                    &
           ! No of soil carbon levels if l_layeredc=TRUE in UM
, ntype                                                                        &
              ! Max. No. of land surface tiles
, npft
              ! No. of plant functional types

! Primary Arrays used in all models
INTEGER ::                                                                     &
  land_index(land_pts)      ! set from land_sea_mask


REAL ::                                                                        &
 c_veg(land_pts,npft)                                                          &
                      ! Total carbon content of vegetation
!                              ! (kg C/m2).
,cv(land_pts)                                                                  &
                      ! Gridbox mean veg carbon (kg C/m2).
,lit_c(land_pts,npft)                                                          &
                      ! Carbon Litter (kg C/m2/360days).
,lit_c_mn(land_pts)                                                            &
                      ! Gridbox mean carbon litter
!                              ! (kg C/m2/360days)
,g_leaf_day(land_pts,npft)                                                     &
                             ! Mean leaf turnover rate for
!                                     ! input to PHENOL (/360days).
,g_leaf_phen(land_pts,npft)                                                    &
                             ! Mean leaf turnover rate over
!                                     ! phenology period (/360days).
,g_leaf_dr_out(land_pts,npft)                                                  &
                             ! Mean leaf turnover rate for
!                                     ! driving TRIFFID (/360days).
,lai_phen(land_pts,npft)                                                       &
                             ! LAI of PFTs after phenology.
,npp_dr_out(land_pts,npft)                                                     &
                             ! Mean NPP for driving TRIFFID
!                                     ! (kg C/m2/360days).
,resp_w_dr_out(land_pts,npft)                                                  &
                             ! Mean wood respiration for
!                                     ! driving TRIFFID
!                                     ! (kg C/m2/360days).
,frac_disturb(land_pts)                                                        &
                             ! Fraction of gridbox in which
!                                     !    vegetation is disturbed.
,disturb_veg_prev(land_pts)                                                    &
                             ! Previous disturbed fraction
,frac(land_pts,ntype)
                             ! Fractions of surface types.

! Diagnostic variables
REAL ::                                                                        &
 stashwork( * )    ! STASH workspace

!JULES TYPEs
TYPE(trif_vars_type), INTENT(IN OUT) :: trif_vars
TYPE(progs_type), INTENT(IN) :: progs
TYPE(trifctl_type), INTENT(IN OUT) :: trifctltype

! Local variables

LOGICAL ::                                                                     &
 plltype(ntype)                                                                &
                    ! pseudolevel list for surface types
,pllpft(npft)       ! pseudolevel list for PFTs

INTEGER ::                                                                     &
 pslevel                                                                       &
               !  loop counter for pseudolevels
,pslevel_out   !  index for pseudolevels sent to STASH

INTEGER :: i,j,k,l
INTEGER :: npoints_ij     !row_length * rows
INTEGER :: si_start, si_stop !Start and stop indices for stashwork when required

CHARACTER(LEN=*) :: routinename
PARAMETER ( routinename='DIAGNOSTICS_VEG')

INTEGER ::                                                                     &
  im_index        ! internal model index

REAL ::                                                                        &
  interp_data(row_length,rows),                                                &
  interp_data_3d(row_length,rows,sm_levels)

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

! ----------------------------------------------------------------------
! Section 1.  Initialisation.
! ----------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
im_index = 1

! row_length * rows is calculated ~80 times
npoints_ij = row_length * rows

! ----------------------------------------------------------------------

!  Item 1: VEGETATION CARBON ON PLANT FUNCTIONAL TYPES

IF (sf(1,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,1,19,im_index)),                                     &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = c_veg(l,pslevel_out)
      END DO

      si_start = si(1,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(1,19)


!  Item 2: GRIDBOX MEAN VEGETATION CARBON

IF (sf(2,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = cv(l)
  END DO

  CALL copydiag (stashwork(si(2,19,im_index):si_last(2,19,im_index)),          &
       interp_data,row_length,rows)
END IF     !   sf(2,19)

!  Item 3: PHENOLOGICAL LEAF TURNOVER RATE PFTS

IF (sf(3,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,3,19,im_index)),                                     &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = g_leaf_phen(l,pslevel_out)
      END DO

      si_start = si(3,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(3,19)


!  Item 4: LITTER CARBON ON PLANT FUNCTIONAL TYPES

IF (sf(4,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,4,19,im_index)),                                     &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = lit_c(l,pslevel_out)
      END DO

      si_start = si(4,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(4,19)


!  Item 5: GRIDBOX MEAN LITTER CARBON

IF (sf(5,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = lit_c_mn(l)
  END DO

  CALL copydiag (stashwork(si(5,19,im_index):si_last(5,19,im_index)),          &
    interp_data,row_length,rows)
END IF     !   sf(5,19)

!  Item 6: MEAN LEAF TURNOVER RATE ON PFTS FOR PHENOLOGY

IF (sf(6,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,6,19,im_index)),                                     &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = g_leaf_day(l,pslevel_out)
      END DO

      si_start = si(6,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(6,19)


!  Item 7: LEAF AREA INDEX ON PLANT FUNCTIONAL TYPES AFTER PHENOLOGY

IF (sf(7,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,7,19,im_index)),                                     &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = lai_phen(l,pslevel_out)
      END DO

      si_start = si(7,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(7,19)


!  Item 8: MEAN LEAF TURNOVER RATE ON PFTS FOR TRIFFID

IF (sf(8,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,8,19,im_index)),                                     &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = g_leaf_dr_out(l,pslevel_out)
      END DO

      si_start = si(8,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(8,19)


!  Item 9: MEAN NPP ON PFTS FOR TRIFFID

IF (sf(9,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,9,19,im_index)),                                     &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = npp_dr_out(l,pslevel_out)
      END DO

      si_start = si(9,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(9,19)


!  Item 10: MEAN WOOD RESPIRATION ON PFTS FOR TRIFFID

IF (sf(10,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,10,19,im_index)),                                    &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = resp_w_dr_out(l,pslevel_out)
      END DO

      si_start = si(10,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(10,19)


!  Item 11: MEAN SOIL RESPIRATION FOR TRIFFID

IF (sf(11,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM( trifctltype%resp_s_dr_out_gb(l,:,5) )
  END DO

  CALL copydiag (stashwork(si(11,19,im_index):si_last(11,19,im_index))         &
       ,interp_data,row_length,rows)
END IF     !   sf(11,19)

!  Item 12: DISTURBED FRACTION OF VEGETATION

IF (sf(12,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = frac_disturb(l)
  END DO

  CALL copydiag (stashwork(si(12,19,im_index):si_last(12,19,im_index))         &
       ,interp_data,row_length,rows)
END IF     !   sf(12,19)

!  Item 13: SURFACE TYPE FRACTIONS AFTER TRIFFID

IF (sf(13,19)) THEN
  CALL set_pseudo_list(ntype,len_stlist,                                       &
       stlist(1,stindex(1,13,19,im_index)),                                    &
       plltype,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,ntype
    IF (plltype(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = frac(l,pslevel_out)
      END DO

      si_start = si(13,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(13,19)


!  Item 14: LEAF AREA INDEX ON PLANT FUNCTIONAL TYPES AFTER TRIFFID

IF (sf(14,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,14,19,im_index)),                                    &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = progs%lai_pft(l,pslevel_out)
      END DO

      si_start = si(14,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(14,19)


!  Item 15: CANOPY HEIGHT ON PLANT FUNCTIONAL TYPES AFTER TRIFFID

IF (sf(15,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,15,19,im_index)),                                    &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = progs%canht_pft(l,pslevel_out)
      END DO

      si_start = si(15,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(15,19)

!  Item 16: SOIL CARBON CONTENT AFTER TRIFFID

IF (sf(16,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(progs%cs_pool_soilt(l,1,:,1)) +                     &
                   SUM(progs%cs_pool_soilt(l,1,:,2)) +                         &
                   SUM(progs%cs_pool_soilt(l,1,:,3)) +                         &
                   SUM(progs%cs_pool_soilt(l,1,:,4))
  END DO

  CALL copydiag (stashwork(si(16,19,im_index):si_last(16,19,im_index))         &
       ,interp_data,row_length,rows)
END IF     !   sf(16,19)


!  Item 17-20: MEAN SOIL RESPIRATION FOR TRIFFID, INDIVID. POOLS
! 17: DPM
IF (sf(17,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM( trifctltype%resp_s_dr_out_gb(l,:,1) )
  END DO

  CALL copydiag (stashwork(si(17,19,im_index):si_last(17,19,im_index))         &
       ,interp_data,row_length,rows)
END IF

! 18: RPM
IF (sf(18,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM( trifctltype%resp_s_dr_out_gb(l,:,2) )
  END DO

  CALL copydiag (stashwork(si(18,19,im_index):si_last(18,19,im_index))         &
       ,interp_data,row_length,rows)
END IF

! 19: BIO
IF (sf(19,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM( trifctltype%resp_s_dr_out_gb(l,:,3) )
  END DO

  CALL copydiag (stashwork(si(19,19,im_index):si_last(19,19,im_index))         &
       ,interp_data,row_length,rows)
END IF

! 20: HUM
IF (sf(20,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM( trifctltype%resp_s_dr_out_gb(l,:,4) )
  END DO

  CALL copydiag (stashwork(si(20,19,im_index):si_last(20,19,im_index))         &
       ,interp_data,row_length,rows)
END IF

!  Item 21-24: SOIL CARBON CONTENT AFTER TRIFFID, INDIVID. POOLS kgC/m2
! 21: DPM
IF (sf(21,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(progs%cs_pool_soilt(l,1,:,1))
  END DO

  CALL copydiag (stashwork(si(21,19,im_index):si_last(21,19,im_index))         &
       ,interp_data,row_length,rows)
END IF

! 22: RPM
IF (sf(22,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(progs%cs_pool_soilt(l,1,:,2))
  END DO

  CALL copydiag (stashwork(si(22,19,im_index):si_last(22,19,im_index))         &
       ,interp_data,row_length,rows)
END IF

! 23: BIO
IF (sf(23,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(progs%cs_pool_soilt(l,1,:,3))
  END DO

  CALL copydiag (stashwork(si(23,19,im_index):si_last(23,19,im_index))         &
       ,interp_data,row_length,rows)
END IF

! 24: HUM
IF (sf(24,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(progs%cs_pool_soilt(l,1,:,4))
  END DO

  CALL copydiag (stashwork(si(24,19,im_index):si_last(24,19,im_index))         &
       ,interp_data,row_length,rows)
END IF

! 19025 LEAF CARBON on PFTS, kgC/m2

IF (sf(25,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,25,19,im_index)),                                    &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%leafc_pft(l,pslevel_out)
      END DO

      si_start = si(25,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(25,19)

! Item 19 026 LEAF CARBON (GBM) kgC/m2

IF (sf(26,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%leafc_gbm(l)
  END DO

  CALL copydiag (stashwork(si(26,19,im_index):si_last(26,19,im_index))         &
       ,interp_data,row_length,rows)
END IF     !   sf(26,19)



! Item 19027 WOOD CARBON on PFTS, kgC/m2

IF (sf(27,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,27,19,im_index)),                                    &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%woodc_pft(l,pslevel_out)
      END DO

      si_start = si(27,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(27,19)

! Item 19 028 WOOD CARBON (GBM) kgC/m2

IF (sf(28,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%woodc_gbm(l)
  END DO

  CALL copydiag (stashwork(si(28,19,im_index):si_last(28,19,im_index))         &
       ,interp_data,row_length,rows)
END IF     !   sf(28,19)


! Item 19029 ROOT CARBON on PFTS, kgC/m2

IF (sf(29,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,29,19,im_index)),                                    &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%rootc_pft(l,pslevel_out)
      END DO

      si_start = si(29,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(29,19)

! Item 19 030 ROOT CARBON (GBM) kgC/m2

IF (sf(30,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%rootc_gbm(l)
  END DO

  CALL copydiag (stashwork(si(30,19,im_index):si_last(30,19,im_index))         &
       ,interp_data,row_length,rows)
END IF     !   sf(30,19)



! Item 19031: Previous agricultural fraction
! Agricultural fraction from previous call to TRIFFID.

IF (sf(31,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = disturb_veg_prev(l)
  END DO

  CALL copydiag (stashwork(si(31,19,im_index):si_last(31,19,im_index))         &
       ,interp_data,row_length,rows)
END IF     !   sf(31,19)


! Item 19032: Wood product pool Carbon (FAST turnover rate pool), kgC/m2
! Fast-turnover wood product carbon pool from cleared vegetation

IF (sf(32,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = progs%wood_prod_fast_gb(l)
  END DO

  CALL copydiag (stashwork(si(32,19,im_index):si_last(32,19,im_index))         &
       ,interp_data,row_length,rows)
END IF     !   sf(32,19)


! Item 19033: Wood product pool Carbon (MEDIUM turnover rate pool), kgC/m2
! Medium-turnover wood product carbon pool from cleared vegetation

IF (sf(33,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = progs%wood_prod_med_gb(l)
  END DO

  CALL copydiag (stashwork(si(33,19,im_index):si_last(33,19,im_index))         &
       ,interp_data,row_length,rows)
END IF     !   sf(33,19)

! Item 19034: Wood product pool Carbon (SLOW turnover rate pool), kgC/m2
! Slow-turnover wood product carbon pool from cleared vegetation

IF (sf(34,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = progs%wood_prod_slow_gb(l)
  END DO

  CALL copydiag (stashwork(si(34,19,im_index):si_last(34,19,im_index))         &
       ,interp_data,row_length,rows)
END IF     !   sf(34,19)


! Item 19036:  LIT C FLUX TO FAST POOL kgC/m2/YR
! Carbon flux into the fast-turnover wood product pool from cleared vegetation

IF (sf(36,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%wp_fast_in_gb(l)
  END DO

  CALL copydiag (stashwork(si(36,19,im_index):si_last(36,19,im_index))         &
       ,interp_data,row_length,rows)
END IF     !   sf(36,19)

! Item 19037:  LIT C FLUX TO MED POOL kgC/m2/YR
! Carbon flux into the medium-turnover wood product pool from cleared vegetation

IF (sf(37,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%wp_med_in_gb(l)
  END DO

  CALL copydiag (stashwork(si(37,19,im_index):si_last(37,19,im_index))         &
       ,interp_data,row_length,rows)
END IF     !   sf(37,19)


! Item 19038:  LIT C FLUX TO SLOW POOL kgC/m2/YR
! Carbon flux into the slow-turnover wood product pool from cleared vegetation

IF (sf(38,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%wp_slow_in_gb(l)
  END DO

  CALL copydiag (stashwork(si(38,19,im_index):si_last(38,19,im_index))         &
       ,interp_data,row_length,rows)
END IF     !   sf(38,19)


! Item 19039: FAST WP POOL DECOMP C FLUX kgC/m2/YR
! CO2 flux from decomposition of the fast-turnover wood product pool

IF (sf(39,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%wp_fast_out_gb(l)
  END DO

  CALL copydiag (stashwork(si(39,19,im_index):si_last(39,19,im_index))         &
       ,interp_data,row_length,rows)
END IF     !   sf(39,19)


! Item 19040: MED WP POOL DECOMP C FLUX kgC/m2/YR
! CO2 flux from decomposition of the medium-turnover wood product pool

IF (sf(40,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%wp_med_out_gb(l)
  END DO

  CALL copydiag (stashwork(si(40,19,im_index):si_last(40,19,im_index))         &
       ,interp_data,row_length,rows)
END IF     !   sf(40,19)


! Item 19041: SLOW WP POOL DECOMP C FLUX kgC/m2/YR
! CO2 flux from decomposition of the slow-turnover wood product pool

IF (sf(41,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%wp_slow_out_gb(l)
  END DO

  CALL copydiag (stashwork(si(41,19,im_index):si_last(41,19,im_index))         &
       ,interp_data,row_length,rows)
END IF     !   sf(41,19)


! Item 19042: TOTAL WP POOL DECOMP C FLUX kgC/m2/YR
! Total CO2 flux from decomposition of all three wood product pools combined

IF (sf(42,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%wp_fast_out_gb(l) +                           &
                       trif_vars%wp_med_out_gb(l) +                            &
                       trif_vars%wp_slow_out_gb(l)
  END DO

  CALL copydiag (stashwork(si(42,19,im_index):si_last(42,19,im_index))         &
       ,interp_data,row_length,rows)
END IF     !   sf(42,19)

! 19043: Harvest carbon flux on PFTs kgC/m2/yr
! Carbon flux from harvested crop PFTs, available when
! L_TRIF_CROP=.TRUE.

IF (sf(43,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,43,19,im_index)),                                    &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%harvest_pft(l,pslevel_out)
      END DO

      si_start = si(43,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(43,19)

! 19044: Harvest carbon flux (GBM) kgC/m2/yr
! Carbon flux from harvested crop PFTs (gridbox mean),
! available when L_TRIF_CROP=.TRUE.

IF (sf(44,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%harvest_gb(l)
  END DO

  CALL copydiag (STASHwork(si(44,19,im_index):si_last(44,19,im_index))         &
       ,interp_data,row_length,rows)
END IF     !   sf(44,19)


! Item 19 45 LANDUSE C TO SOIL ON PFTS kg/m2/360d

IF (sf(45,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,45,19,im_index)),                                    &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%root_abandon_pft(l,pslevel_out)
      END DO

      si_start = si(45,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(45,19)

! Item 19 46 LANDUSE C TO SOIL (GBM) kgC/m2/360d

IF (sf(46,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%root_abandon_gb(l)
  END DO

  CALL copydiag (stashwork(si(46,19,im_index):si_last(46,19,im_index)),        &
       interp_data,row_length,rows)
END IF     !   sf(46,19)

! 19047: CO2 correction term added to the atmosphere's 3D CO2 tracer from the
! land surface in emissions-driven runs. Consists of the sum of the
! crop harvest flux, exudates (the portion of NPP not assimilable by plants
! due to nitrogen limitation) and the total wood product pool flux.
! UNITS: kgC/m2/yr
!
IF (sf(47,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = progs%triffid_co2_gb(l)
  END DO

  CALL copydiag (STASHwork(si(47,19,im_index):si_last(47,19,im_index)),        &
       interp_data,row_length,rows)
END IF     !   sf(47,19)

! Item 19 48 carbon error in veg2 kg m-2

IF (sf(48,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%cnsrv_carbon_veg2_gb(l)
  END DO

  CALL copydiag (stashwork(si(48,19,im_index):si_last(48,19,im_index)),        &
       interp_data,row_length,rows)
END IF     !   sf(48,19)

! Item 19 49 carbon error in triffid kg m-2

IF (sf(49,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%cnsrv_carbon_triffid_gb(l)
  END DO

  CALL copydiag (stashwork(si(49,19,im_index):si_last(49,19,im_index))         &
       ,interp_data,row_length,rows)
END IF     !   sf(49,19)

! Item 19 50 veg carbon error in triffid kg m-2

IF (sf(50,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%cnsrv_veg_triffid_gb(l)
  END DO

  CALL copydiag (stashwork(si(50,19,im_index):si_last(50,19,im_index))         &
       ,interp_data,row_length,rows)
END IF     !   sf(50,19)

! Item 19 51 soil carbon error in triffid kg m-2

IF (sf(51,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%cnsrv_soil_triffid_gb(l)
  END DO

  CALL copydiag (stashwork(si(51,19,im_index):si_last(51,19,im_index))         &
       ,interp_data,row_length,rows)
END IF     !   sf(51,19)

! Item 19 52 wood product carbon error in triffid kg m-2

IF (sf(52,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%cnsrv_prod_triffid_gb(l)
  END DO

  CALL copydiag (stashwork(si(52,19,im_index):si_last(52,19,im_index))         &
       ,interp_data,row_length,rows)
END IF     !   sf(52,19)

! Item 19 53 soil to atmosphere respiration flux kg m-2 (360 day)-1

IF (sf(53,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM( trif_vars%resp_s_to_atmos_gb(l,:) )
  END DO

  CALL copydiag (stashwork(si(53,19,im_index):si_last(53,19,im_index))         &
       ,interp_data,row_length,rows)
END IF     !   sf(53,19)

! Item 19054: Fraction of total litter going to Decomposable Plant
!             Material (DPM) pool.

IF (sf(54,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%dpm_ratio_gb(l)
  END DO

  CALL copydiag (stashwork(si(54,19,im_index):si_last(54,19,im_index))         &
       ,interp_data,row_length,rows)
END IF     !   sf(54,19)

! Stashcode (19,101):  NPP after N limitation on PFTS (KGC/M2/YR)
! NPP after the removal of the exudates term as a result of Nitrogen
! limitation (on PFTs)

IF (sf(101,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
                       stlist(1,stindex(1,101,19,im_index)),                   &
                       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1,rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1,land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%npp_n(l,pslevel_out)
      END DO

      si_start = si(101,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(101,19)

! Stashcode (19,102):  NPP after N limitation (GBM) KGC/M2/YR
! NPP after the removal of the exudates term as a result of Nitrogen
! limitation (gridbox mean)

IF (sf(102,19)) THEN
  DO j = 1,rows
    DO i = 1,row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1,land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%npp_n_gb(l)
  END DO

  CALL copydiag (STASHwork(si(102,19,im_index):si_last(102,19,im_index))       &
       ,interp_data,row_length,rows)

END IF     !   sf(102,19)

! Stashcode (19,103): Exudates on PFTs (KGC/M2/YR)
! Exudates term - the amount by which NPP is reduced due to Nitrogen limitation
! (on PFTs)

IF (sf(103,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
                       stlist(1,stindex(1,103,19,im_index)),                   &
                       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1,rows
        DO i = 1,row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1,land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%exudates_pft(l,pslevel_out)
      END DO

      si_start = si(103,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(103,19)

! Stashcode (19,104): Gridbox mean exudates term (KGC/M2/YR)
! Exudates term - the amount by which NPP is reduced due to Nitrogen
! limitation (gridbox mean)

IF (sf(104,19)) THEN
  DO j = 1,rows
    DO i = 1,row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1,land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%exudates_gb(l)
  END DO

  CALL copydiag (STASHwork(si(104,19,im_index):si_last(104,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(104,19)

! Item 19 111 NITROGEN DEPOSITION kgN/m2/360d

IF (sf(111,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    ! Convert to per 360 days
    interp_data(i,j) = trif_vars%deposition_n_gb(l) * rsec_per_day * 360.0
  END DO

  CALL copydiag (stashwork(si(111,19,im_index):si_last(111,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(111,19)

! Item 19 112 NITROGEN FIXATION PFTS kgC/m2/360d

IF (sf(112,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,112,19,im_index)),                                   &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%n_fix_pft(l,pslevel_out)
      END DO

      si_start = si(112,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(112,19)

! Item 19 113 NITROGEN FIXATION (GBM) kgN/m2/360d

IF (sf(113,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%n_fix_gb(l)
  END DO

  CALL copydiag (stashwork(si(113,19,im_index):si_last(113,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(113,19)

! Stashcode (19,114) NITROGEN LEACHING (GBM) KGN/M2/YR
! This is calculated every timestep in JULES subroutine HYDROL and
! accumulated between calls to TRIFFID so that it can be
! sampled and meaned over TRIFFID timesteps like all other section 19
! diagnostics. The * 360.0/REAL(triffid_period) converts the
! accumulation since the last TRIFFID call to units of kgN/m2/yr;
! TRIFFID is constrained to a 360 day calendar.

IF (sf(114,19)) THEN

  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%n_leach_gb_acc(l) * 360.0 /                   &
            REAL(triffid_period)
  END DO

  CALL copydiag (STASHwork(si(114,19,im_index):si_last(114,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(114,19)

! Item 19 115 NITROGEN MINERAL GAS kgN/m2/360d

IF (sf(115,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM( trif_vars%n_gas_gb(l,:) )
  END DO

  CALL copydiag (stashwork(si(115,19,im_index):si_last(115,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(115,19)

! Item 19 116 INORG NITROGEN LOSS kgN/m2/360d

IF (sf(116,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%n_loss_gb(l)
  END DO

  CALL copydiag (stashwork(si(116,19,im_index):si_last(116,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(116,19)

! Item 19 117 NITROGEN ATM GAS LOSS kgN/m2/360d

IF (sf(117,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%n_loss_gb(l) + SUM(trif_vars%n_gas_gb(l,:))
  END DO

  CALL copydiag (stashwork(si(117,19,im_index):si_last(117,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(117,19)

! Item 19 118 NITROGEN TOTAL LOSS kgN/m2/360d

IF (sf(118,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%n_loss_gb(l) + SUM(trif_vars%n_gas_gb(l,:)) + &
                       trif_vars%n_leach_gb_acc(l) * 360.0 /                   &
                       REAL(triffid_period)
  END DO

  CALL copydiag (stashwork(si(118,19,im_index):si_last(118,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(118,19)

! Item 19 119 HARVEST N ON PFTS kgN/m2/360d

IF (sf(119,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,119,19,im_index)),                                   &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%harvest_n_pft(l,pslevel_out)
      END DO

      si_start = si(119,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(119,19)

! Item 19 120 HARVEST N (GBM) kgN/m2/360d

IF (sf(120,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%harvest_n_gb(l)
  END DO

  CALL copydiag (stashwork(si(120,19,im_index):si_last(120,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(120,19)

! Item 19 121 NITROGEN LUC ON PFTS kgN/m2/360d

IF (sf(121,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,121,19,im_index)),                                   &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%lit_n_ag_pft_diag(l,pslevel_out)
      END DO

      si_start = si(121,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(121,19)

! Item 19 122 NITROGEN LUC (GBM) kgN/m2/360d

IF (sf(122,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%n_luc(l)
  END DO

  CALL copydiag (stashwork(si(122,19,im_index):si_last(122,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(122,19)

! Item 19 123 NITROGEN LITTER ON PFTS kgN/m2/360d

IF (sf(123,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,123,19,im_index)),                                   &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%lit_n_pft_diag(l,pslevel_out)
      END DO

      si_start = si(123,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(123,19)

! Item 19 124 NITROGEN LITTER (GBM) kgN/m2/360d

IF (sf(124,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%lit_n_t_gb(l)
  END DO

  CALL copydiag (stashwork(si(124,19,im_index):si_last(124,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(124,19)

! Item 19 125 NITROGEN FERTILISER ON PFTS kgN/m2/360d

IF (sf(125,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,125,19,im_index)),                                   &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%n_fertiliser_pft(l,pslevel_out)
      END DO

      si_start = si(125,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(125,19)

! Item 19 126 NITROGEN FERTILISER (GBM) kgN/m2/360d

IF (sf(126,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%n_fertiliser_gb(l)
  END DO

  CALL copydiag (stashwork(si(126,19,im_index):si_last(126,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(126,19)

! Item 19 127 LANDUSE N TO SOIL ON PFTS kg/m2/360d

IF (sf(127,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,127,19,im_index)),                                   &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%root_abandon_n_pft(l,pslevel_out)
      END DO

      si_start = si(127,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(127,19)

! Item 19 128 LANDUSE N TO SOIL (GBM) kgC/m2/360d

IF (sf(128,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%root_abandon_n_gb(l)
  END DO

  CALL copydiag (stashwork(si(128,19,im_index):si_last(128,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(128,19)

! Item 19 131 NITROGEN STEM/WOOD kgN/m2

IF (sf(131,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,131,19,im_index)),                                   &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%n_stem_trif_pft(l,pslevel_out)
      END DO

      si_start = si(131,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(131,19)

! Item 19 132 NITROGEN LEAF kgN/m2

IF (sf(132,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,132,19,im_index)),                                   &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%n_leaf_trif_pft(l,pslevel_out)
      END DO

      si_start = si(132,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(132,19)

! Item 19 133 NITROGEN ROOT kgN/m2

IF (sf(133,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,133,19,im_index)),                                   &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%n_root_trif_pft(l,pslevel_out)
      END DO

      si_start = si(133,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(133,19)

! Item 19 134 NITROGEN LEAF LABILE kgN/m2

IF (sf(134,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,134,19,im_index)),                                   &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%n_leaf_labile_trif_pft(l,pslevel_out)
      END DO

      si_start = si(134,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(134,19)

! Item 19 135 NITROGEN LEAF ALLOCATED kgN/m2

IF (sf(135,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,135,19,im_index)),                                   &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%n_leaf_alloc_trif_pft(l,pslevel_out)
      END DO

      si_start = si(135,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(135,19)

! Item 19 136 NITROGEN VEG PFT kgN/m2

IF (sf(136,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,136,19,im_index)),                                   &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%n_veg_pft(l,pslevel_out)
      END DO

      si_start = si(136,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(136,19)

! Item 19 137 NITROGEN VEG (GBM) kgN/m2

IF (sf(137,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%n_veg_gb(l)
  END DO

  CALL copydiag (stashwork(si(137,19,im_index):si_last(137,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(137,19)

! Item 19 141 NITROGEN SOIL RPM kgN/m2

IF (sf(141,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(progs%ns_pool_gb(l,:,1))
  END DO

  CALL copydiag (stashwork(si(141,19,im_index):si_last(141,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(141,19)

! Item 19 142 NITROGEN SOIL DPM kgN/m2

IF (sf(142,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(progs%ns_pool_gb(l,:,2))
  END DO

  CALL copydiag (stashwork(si(142,19,im_index):si_last(142,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(142,19)

! Item 19 143 NITROGEN SOIL BIO kgN/m2

IF (sf(143,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(progs%ns_pool_gb(l,:,3))
  END DO

  CALL copydiag (stashwork(si(143,19,im_index):si_last(143,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(143,19)

! Item 19 144 NITROGEN SOIL HUM kgN/m2

IF (sf(144,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(progs%ns_pool_gb(l,:,4))
  END DO

  CALL copydiag (stashwork(si(144,19,im_index):si_last(144,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(144,19)

! Item 19 145 NITROGEN SOIL TOTAL kgN/m2

IF (sf(145,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(progs%ns_pool_gb(l,:,:))
  END DO

  CALL copydiag (stashwork(si(145,19,im_index):si_last(145,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(145,19)

! Item 19 146 INORGANIC NITROGEN kgN/m2

IF (sf(146,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = progs%n_inorg_gb(l)
  END DO

  CALL copydiag (stashwork(si(146,19,im_index):si_last(146,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(146,19)

! Item 19 147 NITROGEN TOTAL ECOSYSTEM kgN/m2

IF (sf(147,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) =                                                         &
         SUM(progs%ns_pool_gb(l,:,:)) + progs%n_inorg_gb(l) +                  &
                 trif_vars%n_veg_gb(l)
  END DO

  CALL copydiag (stashwork(si(147,19,im_index):si_last(147,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(147,19)

! Item 19 152 N DEMAND GROWTH kgN/m2/360d

IF (sf(152,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,152,19,im_index)),                                   &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%n_demand_growth_pft(l,pslevel_out)
      END DO

      si_start = si(152,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(152,19)

! Item 19 153 N DEMAND SPREAD kgN/m2/360d

IF (sf(153,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,153,19,im_index)),                                   &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%n_demand_spread_pft(l,pslevel_out)
      END DO

      si_start = si(153,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(153,19)

! Item 19 154 N DEMAND TOTAL kgN/m2/360d

IF (sf(154,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,154,19,im_index)),                                   &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%n_demand_pft(l,pslevel_out)
      END DO

      si_start = si(154,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(154,19)

! Item 19 155 N DEMAND TOTAL (GBM) kgN/m2/360D

IF (sf(155,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%n_demand_gb(l)
  END DO

  CALL copydiag (stashwork(si(155,19,im_index):si_last(155,19,im_index)),      &
        interp_data,row_length,rows)
END IF     !   sf(155,19)

! Item 19 156 N UPTAKE TURNOVER kgN/m2/360d

IF (sf(156,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,156,19,im_index)),                                   &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%n_demand_lit_pft(l,pslevel_out)
      END DO

      si_start = si(156,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(156,19)

! Item 19 157 N UPTAKE GROWTH kgN/m2/360d

IF (sf(157,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,157,19,im_index)),                                   &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%n_uptake_growth_pft(l,pslevel_out)
      END DO

      si_start = si(157,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(157,19)

! Item 19 158 N UPTAKE SPREAD kgN/m2/360d

IF (sf(158,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,158,19,im_index)),                                   &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%n_uptake_spread_pft(l,pslevel_out)
      END DO

      si_start = si(158,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(158,19)

! Item 19 159 N UPTAKE TOTAL kgN/m2/360d

IF (sf(159,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
       stlist(1,stindex(1,159,19,im_index)),                                   &
       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%n_uptake_pft(l,pslevel_out)
      END DO

      si_start = si(159,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(159,19)

! Item 19 160 N UPTAKE TOTAL (GBM) kgN/m2/360D

IF (sf(160,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%n_uptake_gb(l)
  END DO

  CALL copydiag (stashwork(si(160,19,im_index):si_last(160,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(160,19)

! Item 19 161 N SOIL DECOMPOSITION RATE MODIFIER
! If l_layeredc=FALSE, get bulk soil value
! If l_layeredc=TRUE, get top layer soil value

IF (sf(161,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%fn_gb(l,1)
    ! either top soil layer (l_layeredc=true) or bulk soil (l_layeredc=false)
  END DO

  CALL copydiag (stashwork(si(161,19,im_index):si_last(161,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(161,19)

! Item 19 162 N IMMOBILIS POTEN DPM kgN/m2/360D

IF (sf(162,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(trif_vars%immob_n_pot_gb(l,:,1))
  END DO

  CALL copydiag (stashwork(si(162,19,im_index):si_last(162,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(162,19)

! Item 19 163 N IMMOBILIS POTEN RPM kgN/m2/360D

IF (sf(163,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(trif_vars%immob_n_pot_gb(l,:,2))
  END DO

  CALL copydiag (stashwork(si(163,19,im_index):si_last(163,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(163,19)

! Item 19 164 N IMMOBILIS POTEN BIO kgN/m2/360D

IF (sf(164,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(trif_vars%immob_n_pot_gb(l,:,3))
  END DO

  CALL copydiag (stashwork(si(164,19,im_index):si_last(164,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(164,19)

! Item 19 165 N IMMOBILIS POTEN HUM kgN/m2/360D

IF (sf(165,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(trif_vars%immob_n_pot_gb(l,:,4))
  END DO

  CALL copydiag (stashwork(si(165,19,im_index):si_last(165,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(165,19)

! Item 19 166 N IMMOBILIS POTEN TOT kgN/m2/360D

IF (sf(166,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(trif_vars%immob_n_pot_gb(l,:,5))
  END DO

  CALL copydiag (stashwork(si(166,19,im_index):si_last(166,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(166,19)

! Item 19 167 N IMMOBILIS DPM kgN/m2/360D

IF (sf(167,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(trif_vars%immob_n_gb(l,:,1))
  END DO

  CALL copydiag (stashwork(si(167,19,im_index):si_last(167,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(167,19)

! Item 19 168 N IMMOBILIS RPM kgN/m2/360D

IF (sf(168,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(trif_vars%immob_n_gb(l,:,2))
  END DO

  CALL copydiag (stashwork(si(168,19,im_index):si_last(168,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(168,19)

! Item 19 169 N IMMOBILIS BIO kgN/m2/360D

IF (sf(169,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(trif_vars%immob_n_gb(l,:,3))
  END DO

  CALL copydiag (stashwork(si(169,19,im_index):si_last(169,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(169,19)

! Item 19 170 N IMMOBILIS HUM kgN/m2/360D

IF (sf(170,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(trif_vars%immob_n_gb(l,:,4))
  END DO

  CALL copydiag (stashwork(si(170,19,im_index):si_last(170,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(170,19)

! Item 19 171 N IMMOBILIS TOT kgN/m2/360D

IF (sf(171,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(trif_vars%immob_n_gb(l,:,5))
  END DO

  CALL copydiag (stashwork(si(171,19,im_index):si_last(171,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(171,19)

! Item 19 172 N MINERALIS POTEN DPM kgN/m2/360D

IF (sf(172,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(trif_vars%minl_n_pot_gb(l,:,1))
  END DO

  CALL copydiag (stashwork(si(172,19,im_index):si_last(172,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(172,19)

! Item 19 173 N MINERALIS POTEN RPM kgN/m2/360D

IF (sf(173,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(trif_vars%minl_n_pot_gb(l,:,2))
  END DO

  CALL copydiag (stashwork(si(173,19,im_index):si_last(173,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(173,19)

! Item 19 174 N MINERALIS POTEN BIO kgN/m2/360D

IF (sf(174,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(trif_vars%minl_n_pot_gb(l,:,3))
  END DO

  CALL copydiag (stashwork(si(174,19,im_index):si_last(174,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(174,19)

! Item 19 175 N MINERALIS POTEN HUM kgN/m2/360D

IF (sf(175,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(trif_vars%minl_n_pot_gb(l,:,4))
  END DO

  CALL copydiag (stashwork(si(175,19,im_index):si_last(175,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(175,19)

! Item 19 176 N MINERALIS POTEN TOT kgN/m2/360D

IF (sf(176,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(trif_vars%minl_n_pot_gb(l,:,5))
  END DO

  CALL copydiag (stashwork(si(176,19,im_index):si_last(176,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(176,19)

! Item 19 177 N MINERALIS DPM kgN/m2/360D

IF (sf(177,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(trif_vars%minl_n_gb(l,:,1))
  END DO

  CALL copydiag (stashwork(si(177,19,im_index):si_last(177,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(177,19)

! Item 19 178 N MINERALIS RPM kgN/m2/360D

IF (sf(178,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(trif_vars%minl_n_gb(l,:,2))
  END DO

  CALL copydiag (stashwork(si(178,19,im_index):si_last(178,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(178,19)

! Item 19 179 N MINERALIS BIO kgN/m2/360D

IF (sf(179,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(trif_vars%minl_n_gb(l,:,3))
  END DO

  CALL copydiag (stashwork(si(179,19,im_index):si_last(179,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(179,19)

! Item 19 180 N MINERALIS HUM kgN/m2/360D

IF (sf(180,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(trif_vars%minl_n_gb(l,:,4))
  END DO

  CALL copydiag (stashwork(si(180,19,im_index):si_last(180,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(180,19)

! Item 19 181 N MINERALIS TOT kgN/m2/360D

IF (sf(181,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(trif_vars%minl_n_gb(l,:,5))
  END DO

  CALL copydiag (stashwork(si(181,19,im_index):si_last(181,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(181,19)

! Stashcode (19,182): Gross Primary Productivity on PFTs (KGC/M2/YR)
! GPP on PFTs: total amount of Carbon taken up during photosynthesis, some of
! which will be respired back into the atmosphere, so the net plant carbon
! uptake is the Net Primary Productivity. Computed by the surface code but
! copied here for output via section 19 to aid carbon budget calculations on
! TRIFFID timesteps. The * 360.0/REAL(triffid_period) converts the
! accumulation since the last TRIFFID call to units of kgC/m2/yr; TRIFFID
! is constrained to a 360 day calendar.

IF (sf(182,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
                       stlist(1,stindex(1,182,19,im_index)),                   &
                       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1,rows
        DO i = 1,row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1,land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%gpp_pft_acc(l,pslevel_out) *              &
                           360.0 / REAL(triffid_period)
      END DO

      si_start = si(182,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(182,19)


! Stashcode (19,183): Gross Primary Productivity, GBM (KGC/M2/YR)
! GPP (gridbox mean): total amount of Carbon taken up during photosynthesis,
! some of which will be respired back into the atmosphere, so the net plant
! carbon uptake is the Net Primary Productivity. Computed by the surface code
! but copied here for output via section 19 to aid carbon budget calculations
! on TRIFFID timesteps. The * 360.0/REAL(triffid_period) converts the
! accumulation since the last TRIFFID call to units of kgC/m2/yr; TRIFFID is
! constrained to a 360 day calendar.

IF (sf(183,19)) THEN

  DO j = 1,rows
    DO i = 1,row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1,land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%gpp_gb_acc(l) * 360.0 / REAL(triffid_period)
  END DO

  CALL copydiag (STASHwork(si(183,19,im_index):si_last(183,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(183,19)


! Stashcode (19,184): Actual plant respiration on PFTs (KGC/M2/YR)
! This is plant respiration adjusted to take into account the exudates term,
! i.e. the amount by which potential Net Primary Productivity is reduced to
! account for limitation by Nitrogen availablity.

IF (sf(184,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
                       stlist(1,stindex(1,184,19,im_index)),                   &
                       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1,rows
        DO i = 1,row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1,land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%resp_p_actual_pft(l,pslevel_out)
      END DO

      si_start = si(184,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(184,19)


! Stashcode (19,185): Actual plant respiration, gridbox mean  (KGC/M2/YR)
! This is plant respiration adjusted to take into account the exudates term,
! i.e. the amount by which potential Net Primary Productivity is reduced to
! account for limitation by Nitrogen availablity.

IF (sf(185,19)) THEN
  DO j = 1,rows
    DO i = 1,row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1,land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%resp_p_actual_gb(l)
  END DO

  CALL copydiag (STASHwork(si(185,19,im_index):si_last(185,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(185,19)

! Item 19 186 veg nitrogen error in triffid kg m-2

IF (sf(186,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%cnsrv_vegn_triffid_gb(l)
  END DO

  CALL copydiag (stashwork(si(186,19,im_index):si_last(186,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(186,19)

! Item 19 187 soil nitrogen error in triffid kg m-2

IF (sf(187,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%cnsrv_soiln_triffid_gb(l)
  END DO

  CALL copydiag (stashwork(si(187,19,im_index):si_last(187,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(49,19)

! Item 19 188 soil nitrogen error in triffid kg m-2

IF (sf(188,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%cnsrv_n_inorg_triffid_gb(l)
  END DO

  CALL copydiag (stashwork(si(188,19,im_index):si_last(188,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(188,19)

! Item 19 189 nitrogen error in triffid kg m-2

IF (sf(189,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%cnsrv_nitrogen_triffid_gb(l)
  END DO

  CALL copydiag (stashwork(si(189,19,im_index):si_last(189,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(189,19)

! Stashcode (19,190)
! Carbon loss from vegetation due to fire on PFTs (KGC/M2/YR)
! Loss of Carbon from vegetation due to fire on PFTs. Assumed to be lost as CO2,
! and is passed to the atmosphere when in CO2-emissions-driven mode (UKESM)

IF (sf(190,19)) THEN
  CALL set_pseudo_list(npft,len_stlist,                                        &
                       stlist(1,stindex(1,190,19,im_index)),                   &
                       pllpft,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1,rows
        DO i = 1,row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1,land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = trif_vars%veg_c_fire_emission_pft(l,pslevel_out)
      END DO

      si_start = si(190,19,im_index) + (pslevel_out-1) * npoints_ij
      si_stop  = si_start + npoints_ij - 1
      CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
    END IF
  END DO

END IF     !   sf(190,19)

! Stashcode (19,191)
! Gridbox mean Carbon loss from vegetation due to fire (KGC/M2/YR)
! Loss of C from vegetation due to fire (gridbox mean). Assumed to be lost as
! CO2, and is passed to the atmosphere when in CO2-emissions-driven mode (UKESM)

IF (sf(191,19)) THEN
  DO j = 1,rows
    DO i = 1,row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1,land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%veg_c_fire_emission_gb(l)
  END DO

  CALL copydiag (STASHwork(si(191,19,im_index):si_last(191,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(191,19)

! Stashcode (19,192): Carbon loss from DPM soil carbon pool due to fire
!                    (KGC/M2/YR)

IF (sf(192,19)) THEN
  DO j = 1,rows
    DO i = 1,row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1,land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%burnt_carbon_dpm(l)
  END DO

  CALL copydiag (STASHwork(si(192,19,im_index):si_last(192,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(192,19)


! Stashcode (19,193): Carbon loss from RPM soil carbon pool due to fire
!                     (KGC/M2/YR)

IF (sf(193,19)) THEN
  DO j = 1,rows
    DO i = 1,row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1,land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%burnt_carbon_rpm(l)
  END DO

  CALL copydiag (STASHwork(si(193,19,im_index):si_last(193,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(193,19)


! Stashcode (19,194): Carbon loss from soil (DPM+RPM) due to fire (KGC/M2/YR)
! Assumed to be lost as CO2, and is passed to the atmosphere when in
! CO2-emissions-driven mode (UKESM)

IF (sf(194,19)) THEN
  DO j = 1,rows
    DO i = 1,row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1,land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%burnt_carbon_dpm(l) +                         &
                       trif_vars%burnt_carbon_rpm(l)
  END DO

  CALL copydiag (STASHwork(si(194,19,im_index):si_last(194,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(194,19)

! Stashcode (19,195): Total carbon loss from land (soil+vegetation) due
! to fire (KGC/M2/YR). Assumed to be lost as CO2, and is passed to the
! atmosphere when in CO2-emissions-driven mode (UKESM).

IF (sf(195,19)) THEN
  DO j = 1,rows
    DO i = 1,row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1,land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = trif_vars%veg_c_fire_emission_gb(l)   +                 &
                       trif_vars%burnt_carbon_dpm(l)         +                 &
                       trif_vars%burnt_carbon_rpm(l)
  END DO

  CALL copydiag (STASHwork(si(195,19,im_index):si_last(195,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !   sf(195,19)

! Stashcode (19,196): Net Biosphere Productivity (NBP) KGC/M2/YR

! This is the net land carbon uptake, i.e. net primary productivity minus the
! sum of all fluxes out of the land, and is comprised of the following
! stashcodes / variables. Any additional loss terms developed in future should
! be included to maintain the accuracy of this diagnostic as the net flux of
! carbon into land.

! 19102: NPP after N limitation (GBM) KGC/M2/YR
! 19053: Soil to atmosphere respiration flux (KG/M2/YR)
! 19039: FAST WP POOL DECOMP C FLUX kgC/m2/YR
! 19040: MED WP POOL DECOMP C FLUX kgC/m2/YR
! 19041: SLOW WP POOL DECOMP C FLUX kgC/m2/YR
! 19044: Harvest carbon flux (GBM) kgC/m2/yr
! 19191: Gridbox mean C loss from vegetation due to fire (KGC/M2/YR)
! 19192: Carbon loss from DPM soil pool due to fire (KGC/M2/YR)
! 19193: Carbon loss from RPM soil carbon pool due to fire (KGC/M2/YR)

IF (sf(196,19)) THEN
  DO j = 1,rows
    DO i = 1,row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1,land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) =  trif_vars%npp_n_gb(l)                    -             &
                        SUM( trif_vars%resp_s_to_atmos_gb(l,:) ) -             &
                        trif_vars%wp_fast_out_gb(l)              -             &
                        trif_vars%wp_med_out_gb(l)               -             &
                        trif_vars%wp_slow_out_gb(l)              -             &
                        trif_vars%harvest_gb(l)                  -             &
                        trif_vars%veg_c_fire_emission_gb(l)      -             &
                        trif_vars%burnt_carbon_dpm(l)            -             &
                        trif_vars%burnt_carbon_rpm(l)
  END DO

  CALL copydiag (STASHwork(si(196,19,im_index):si_last(196,19,im_index))       &
       ,interp_data,row_length,rows)
END IF     !

! Layered soil carbon and nitrogen diagnostics
! Item 19 201-204: LAYERED SOIL CARBON CONTENT AFTER TRIFFID, INDIVID. POOLS kgC/m2
! 19 201: LAYERED SOIL CARBON DPM kgC/m2
IF (sf(201,19)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,k,l)                                  &
!$OMP SHARED(rows,row_length,interp_data_3d,land_pts,land_index,progs,sm_levels)
  DO k = 1, sm_levels
!$OMP DO SCHEDULE(STATIC)
    DO j= 1, rows
      DO i = 1, row_length
        interp_data_3d(i,j,k) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_pts
      j=(land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data_3d(i,j,k) = progs%cs_pool_soilt(l,1,k,1)
    END DO
!$OMP END DO

  END DO
!$OMP END PARALLEL

  CALL copydiag_3d(stashwork(si(201,19,im_index):si_last(201,19,im_index)),    &
       interp_data_3d,                                                         &
       row_length,rows,sm_levels,                                              &
       stlist(:,stindex(1,201,19,im_index)),len_stlist,                        &
       stash_levels,num_stash_levels+1)
END IF


! 19 202: LAYERED SOIL CARBON RPM kgC/m2
IF (sf(202,19)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,k,l)                                  &
!$OMP SHARED(rows,row_length,interp_data_3d,land_pts,land_index,progs,sm_levels)
  DO k = 1, sm_levels
!$OMP DO SCHEDULE(STATIC)
    DO j= 1, rows
      DO i = 1, row_length
        interp_data_3d(i,j,k) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_pts
      j=(land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data_3d(i,j,k) = progs%cs_pool_soilt(l,1,k,2)
    END DO
!$OMP END DO

  END DO
!$OMP END PARALLEL

  CALL copydiag_3d(stashwork(si(202,19,im_index):si_last(202,19,im_index)),    &
       interp_data_3d,                                                         &
       row_length,rows,sm_levels,                                              &
       stlist(:,stindex(1,202,19,im_index)),len_stlist,                        &
       stash_levels,num_stash_levels+1)
END IF


! 19 203: LAYERED SOIL CARBON BIO kgC/m2
IF (sf(203,19)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,k,l)                                  &
!$OMP SHARED(rows,row_length,interp_data_3d,land_pts,land_index,progs,sm_levels)
  DO k = 1, sm_levels
!$OMP DO SCHEDULE(STATIC)
    DO j= 1, rows
      DO i = 1, row_length
        interp_data_3d(i,j,k) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_pts
      j=(land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data_3d(i,j,k) = progs%cs_pool_soilt(l,1,k,3)
    END DO
!$OMP END DO

  END DO
!$OMP END PARALLEL

  CALL copydiag_3d(stashwork(si(203,19,im_index):si_last(203,19,im_index)),    &
       interp_data_3d,                                                         &
       row_length,rows,sm_levels,                                              &
       stlist(:,stindex(1,203,19,im_index)),len_stlist,                        &
       stash_levels,num_stash_levels+1)
END IF


! 19 204: LAYERED SOIL CARBON HUM kgC/m2
IF (sf(204,19)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,k,l)                                  &
!$OMP SHARED(rows,row_length,interp_data_3d,land_pts,land_index,progs,sm_levels)
  DO k = 1, sm_levels
!$OMP DO SCHEDULE(STATIC)
    DO j= 1, rows
      DO i = 1, row_length
        interp_data_3d(i,j,k) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_pts
      j=(land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data_3d(i,j,k) = progs%cs_pool_soilt(l,1,k,4)
    END DO
!$OMP END DO

  END DO
!$OMP END PARALLEL

  CALL copydiag_3d(stashwork(si(204,19,im_index):si_last(204,19,im_index)),    &
       interp_data_3d,                                                         &
       row_length,rows,sm_levels,                                              &
       stlist(:,stindex(1,204,19,im_index)),len_stlist,                        &
       stash_levels,num_stash_levels+1)
END IF


! Item 19 205-208: LAYERED SOIL NITROGEN CONTENT AFTER TRIFFID, INDIVID. POOLS kgN/m2
! 19 205: LAYERED SOIL NITROGEN CONTENT DPM kgN/m2
IF (sf(205,19)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,k,l)                                  &
!$OMP SHARED(rows,row_length,interp_data_3d,land_pts,land_index,progs,sm_levels)
  DO k = 1, sm_levels
!$OMP DO SCHEDULE(STATIC)
    DO j= 1, rows
      DO i = 1, row_length
        interp_data_3d(i,j,k) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_pts
      j=(land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data_3d(i,j,k) = progs%ns_pool_gb(l,k,1)
    END DO
!$OMP END DO

  END DO
!$OMP END PARALLEL

  CALL copydiag_3d(stashwork(si(205,19,im_index):si_last(205,19,im_index)),    &
       interp_data_3d,                                                         &
       row_length,rows,sm_levels,                                              &
       stlist(:,stindex(1,205,19,im_index)),len_stlist,                        &
       stash_levels,num_stash_levels+1)
END IF


! 19 206: LAYERED SOIL NITROGEN CONTENT RPM kgN/m2
IF (sf(206,19)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,k,l)                                  &
!$OMP SHARED(rows,row_length,interp_data_3d,land_pts,land_index,progs,sm_levels)
  DO k = 1, sm_levels
!$OMP DO SCHEDULE(STATIC)
    DO j= 1, rows
      DO i = 1, row_length
        interp_data_3d(i,j,k) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_pts
      j=(land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data_3d(i,j,k) = progs%ns_pool_gb(l,k,2)
    END DO
!$OMP END DO

  END DO
!$OMP END PARALLEL

  CALL copydiag_3d(stashwork(si(206,19,im_index):si_last(206,19,im_index)),    &
       interp_data_3d,                                                         &
       row_length,rows,sm_levels,                                              &
       stlist(:,stindex(1,206,19,im_index)),len_stlist,                        &
       stash_levels,num_stash_levels+1)
END IF


! 19 207: LAYERED SOIL NITROGEN CONTENT BIO kgN/m2
IF (sf(207,19)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,k,l)                                  &
!$OMP SHARED(rows,row_length,interp_data_3d,land_pts,land_index,progs,sm_levels)
  DO k = 1, sm_levels
!$OMP DO SCHEDULE(STATIC)
    DO j= 1, rows
      DO i = 1, row_length
        interp_data_3d(i,j,k) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_pts
      j=(land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data_3d(i,j,k) = progs%ns_pool_gb(l,k,3)
    END DO
!$OMP END DO

  END DO
!$OMP END PARALLEL

  CALL copydiag_3d(stashwork(si(207,19,im_index):si_last(207,19,im_index)),    &
       interp_data_3d,                                                         &
       row_length,rows,sm_levels,                                              &
       stlist(:,stindex(1,207,19,im_index)),len_stlist,                        &
       stash_levels,num_stash_levels+1)
END IF


! 19 208: LAYERED SOIL NITROGEN CONTENT HUM kgN/m2
IF (sf(208,19)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,k,l)                                  &
!$OMP SHARED(rows,row_length,interp_data_3d,land_pts,land_index,progs,sm_levels)
  DO k = 1, sm_levels
!$OMP DO SCHEDULE(STATIC)
    DO j= 1, rows
      DO i = 1, row_length
        interp_data_3d(i,j,k) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_pts
      j=(land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data_3d(i,j,k) = progs%ns_pool_gb(l,k,4)
    END DO
!$OMP END DO

  END DO
!$OMP END PARALLEL

  CALL copydiag_3d(stashwork(si(208,19,im_index):si_last(208,19,im_index)),    &
       interp_data_3d,                                                         &
       row_length,rows,sm_levels,                                              &
       stlist(:,stindex(1,208,19,im_index)),len_stlist,                        &
       stash_levels,num_stash_levels+1)
END IF


! 19 209: Layered inorganic nitrogen, kgN/m2
IF (sf(209,19)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,k,l)                                  &
!$OMP SHARED(rows,row_length,interp_data_3d,land_pts,land_index,progs,sm_levels)
  DO k = 1, sm_levels
!$OMP DO SCHEDULE(STATIC)
    DO j= 1, rows
      DO i = 1, row_length
        interp_data_3d(i,j,k) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_pts
      j=(land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data_3d(i,j,k) = progs%n_inorg_soilt_lyrs(l,1,k)
    END DO
!$OMP END DO

  END DO
!$OMP END PARALLEL

  CALL copydiag_3d(stashwork(si(209,19,im_index):si_last(209,19,im_index)),    &
       interp_data_3d,                                                         &
       row_length,rows,sm_levels,                                              &
       stlist(:,stindex(1,209,19,im_index)),len_stlist,                        &
       stash_levels,num_stash_levels+1)
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE diagnostics_veg
END MODULE diagnostics_veg_mod
#endif
