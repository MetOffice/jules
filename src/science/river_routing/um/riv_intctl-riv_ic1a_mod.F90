#if defined(UM_JULES)
!Huw Lewis (MO), Jan 2015
!DEPRECATED CODE
!This code was transferred from the UM repository at UM vn9.2 / JULES vn 4.1.
!Future developments will supercede these subroutines, and as such they
!should be considered deprecated. They will be retained in the codebase to
!maintain backward compatibility with functionality prior to
!UM vn10.0 / JULES vn 4.2, until such time as they become redundant.
!
!This code is not to be used with soil tiling
!It is possible, with significant effort to make the required modifications,
!but this routine is marked as deprecated.
!
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: River Routing
MODULE riv_intctl_mod_1A

USE riv_rout_1a_wrapper_mod, ONLY: riv_rout_1a_wrapper

USE errormessagelength_mod, ONLY: errormessagelength

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE


CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='RIV_INTCTL_MOD_1A'

CONTAINS

SUBROUTINE riv_intctl_1a(                                                      &
xpa, xua, xva, ypa, yua, yva,                                                  &
g_p_field, g_r_field,                                                          &
land_points, n_wtrac_jls, land_index,                                          &
invert_atmos, row_length, rows,                                                &
global_row_length, global_rows,                                                &
river_row_length, river_rows,                                                  &
global_river_row_length, global_river_rows,                                    &
flandg, riv_step,                                                              &
trivdir, trivseq, twatstor, riverout_rgrid, a_boxareas,                        &
twatstor_wtrac,                                                                &
! Used for UM-RFM implementation
delta_phi,                                                                     &
r_area, slope, flowobs1, r_inext, r_jnext,                                     &
r_land,  substore, surfstore, flowin, bflowin,                                 &
! IN accumulated runoff
tot_surf_runoff_gb, tot_sub_runoff_gb,                                         &
tot_surf_runoff_gb_wtrac, tot_sub_runoff_gb_wtrac,                             &
! OUT
box_outflow, box_inflow, riverout_atmos,                                       &
! Add new arguments for inland basin outflow
! OUT INLAND BASINS
inlandout_atmos,inlandout_riv, inlandout_atmos_wtrac,                          &
! Required for soil moisture correction for water conservation
dsm_levels, acc_lake_evap, smvcst, smvcwt, smcl, sthu,                         &
acc_lake_evap_wtrac, smcl_wtrac, sthu_wtrac)

! Purpose:
! New Control routine for River routing for Global Model.
! Parallel River routing
!

! Code Description:
!   Language: FORTRAN 77 + common extensions.
!   This code is written to UMDP3 v6 programming standards.
!-----------------------------------------------------------------

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
USE ereport_mod, ONLY: ereport
USE UM_ParVars,   ONLY: lasize, glsize, gc_all_proc_group
USE UM_ParParams, ONLY: halo_type_no_halo
USE Field_Types,  ONLY: fld_type_p
USE all_gather_field_mod, ONLY: all_gather_field
USE water_constants_mod, ONLY: rho_water
USE jules_soil_mod,      ONLY: dzsoil

USE jules_water_tracers_mod,  ONLY: l_wtrac_jls
USE lake_evap_global_avg_mod, ONLY: lake_evap_global_avg
USE lake_evap_apply_soil_mod, ONLY: lake_evap_apply_soil

IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
  row_length                                                                   &
                            ! No. of columns in atmosphere
, rows                                                                         &
                            ! No. of rows in atmosphere
, global_row_length                                                            &
                            ! No. of points on a global row
, global_rows                                                                  &
                            ! No. of global rows
, land_points                                                                  &
                            ! No. of landpoints
, river_row_length                                                             &
                            ! No. of columns in river grid
, river_rows                                                                   &
                            ! No. of rows in river grid
, global_river_row_length                                                      &
                            ! No. of global river row length
, global_river_rows                                                            &
                            ! No. of global river rows
, dsm_levels                                                                   &
                            ! No. of deep soil moisture levels
, n_wtrac_jls
                            ! No. of water tracers used in JULES

INTEGER, INTENT(IN) ::                                                         &
  g_p_field                                                                    &
                            ! IN Size of global ATMOS field
, g_r_field                                                                    &
                            ! IN Size of global river field
, land_index (land_points)
                            ! IN Index of land to global points

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 tot_surf_runoff_gb(land_points)                                               &
                            ! IN Surf RUNOFF on land pts(KG/M2/S)
,tot_sub_runoff_gb(land_points)                                                &
                            ! IN Subsurf.RUNOFF (KG/M2/S)
,smvcwt(land_points)                                                           &
                            ! IN Volumetric wilting point (used to remove
                            ! global lake evap from soil moisture store)
,smvcst(land_points)                                                           &
                            ! IN Volumetric saturation point(used to remove
                            ! global lake evap from soil moisture store)
,xua(0:global_row_length)                                                      &
                            ! IN Atmosphere UV longitude coords
,yua(global_rows)                                                              &
                            ! IN Atmosphere latitude coords
,xpa(global_row_length+1)                                                      &
                            ! IN Atmosphere longitude coords
,ypa(global_rows)                                                              &
                            ! IN Atmosphere latitude coords
,xva(global_row_length+1)                                                      &
                            ! IN Atmosphere longitude coords
,yva(0:global_rows)                                                            &
                            ! IN Atmosphere latitude coords
,a_boxareas(row_length, rows)                                                  &
                            ! IN ATMOS gridbox areas
,flandg(row_length, rows)
                            ! IN Land fraction on global field.

! *** The following are used for the UM-RFM Implmentation ***
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
r_area(row_length, rows)                                                       &
                            ! IN accumalated areas file
,slope(row_length, rows)                                                       &
                            ! IN slopes
,r_inext(row_length, rows)                                                     &
                            ! IN X-cordinate of downstream grid pt
,r_jnext(row_length, rows)                                                     &
                            ! IN Y-cordinate of downstream grid pt
,r_land(row_length, rows)                                                      &
                         ! IN land/river depends on value of a_thresh
,flowobs1(row_length, rows)                                                    &
                         ! IN initialisation for flows
,substore(row_length, rows)                                                    &
                         ! IN routing subsurface store
,surfstore(row_length, rows)                                                   &
                         ! IN routing surface store
,flowin(row_length, rows)                                                      &
                         ! IN surface lateral inflow
,bflowin (row_length, rows)                                                    &
                         ! IN subsurface lateral inflow
,delta_phi
                         ! IN RCM gridsize (radians)
! *** End of fields only used for the UM-RFM implementation

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 trivdir(river_row_length, river_rows)                                         &
                            ! IN river direction
,trivseq(river_row_length, river_rows)                                         &
                            ! IN river sequence
,riv_step
                            ! IN river timestep (secs)

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 tot_surf_runoff_gb_wtrac(land_points,n_wtrac_jls)                             &
                            ! IN Surf water tracer runoff on land pts (kg/m2/s)
,tot_sub_runoff_gb_wtrac(land_points,n_wtrac_jls)
                            ! IN Subsurf. water tracer runoff (kg/m2/s)


REAL(KIND=real_jlslsm), INTENT(IN OUT) :: twatstor(river_row_length, river_rows)
                            ! IN OUT water store(Kg)

REAL(KIND=real_jlslsm), INTENT(IN OUT) :: twatstor_wtrac(river_row_length,     &
                                                      river_rows, n_wtrac_jls)
                            ! IN OUT water tracer store(Kg)

LOGICAL, INTENT(IN) ::                                                         &
 invert_atmos               ! IN True if ATMOS fields are S->N
                            !    for regridding runoff from ATMOS.

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 riverout_atmos(row_length,rows)                                               &
                            ! river flow out from each  gridbox(KG/m2/S)
,riverout_rgrid(river_row_length, river_rows)                                  &
                            ! river flow out from TRIP grid to ocean (Kg/s)
,box_outflow(river_row_length, river_rows)                                     &
                            ! gridbox outflow river grid (Kg/s)
,box_inflow(river_row_length, river_rows)                                      &
                            ! gridbox runoff river grid(Kg/s)

! Declare new variables for inland basin outflow

,inlandout_riv(river_row_length,river_rows)                                    &
                            ! Outflow from inland basins on TRIP grid Kg/s

,inlandout_atmos(row_length,rows)
                            ! Outflow from inland basins on ATMOS grid Kg/m2/s


REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 inlandout_atmos_wtrac(row_length,rows,n_wtrac_jls)
                            ! Water tracer outflow from inland basins on
                            !  ATMOS grid (kg/m2/s)

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 acc_lake_evap(row_length,rows)
                            ! Accumulated lake evap over
                            ! river routing timestep (Kg/m2)

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 acc_lake_evap_wtrac(row_length,rows,n_wtrac_jls)
                            ! Accumulated water tracer lake evap over
                            ! river routing timestep (Kg/m2)

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  sthu(land_points)                                                            &
                            ! Unfrozen soil moisture content of
                            !    bottom layer as a frac of saturation
 ,smcl(land_points)
                            ! Total soil moisture contents
                            !      of bottom layer (kg/m2).

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  sthu_wtrac(land_points,1,dsm_levels,n_wtrac_jls)                             &
                            ! Unfrozen soil moisture content as a frac of
                            ! saturation. (Note, unlike the water equivalent,
                            ! this is the full field for all soil levels)
, smcl_wtrac(land_points,1,dsm_levels,n_wtrac_jls)
                            ! Water tracer total soil moisture contents
                            ! (kg/m2). (Note, unlike the water equivalent,
                            ! this is the full field for all soil levels)

! local variables

INTEGER :: i,j,l,i_wt

INTEGER :: n_wt             ! No. of water fields passed into
                            !  lake_evap_global_avg

INTEGER ::                                                                     &
 info                                                                          &
                            ! Return code from MPP
, icode                     ! Return code :0 Normal Exit :>0 Error

INTEGER :: mask_for_lake_evap_corr(row_length,rows)
                            ! Mask used to apply the lake evaporation to soil
                            ! = 1 apply to point, = 0 do not apply

REAL(KIND=real_jlslsm) :: total_soil_area
                            ! Global sum of soil area over which to apply
                            !  the lake evaporation correction

REAL(KIND=real_jlslsm) ::  acc_lake_evap_avg(1)
                            ! Globally accumulated total lake evaporation
                            !  averaged over total_soil_area
                            ! (Array size 1 needed so that both water and water
                            !  tracers can use routine lake_evap_global_avg)

REAL(KIND=real_jlslsm) ::  acc_lake_evap_avg_wtrac(n_wtrac_jls)
                            ! Global accumulated total lake evaporation
                            ! for water tracers averaged over total_soil_area

REAL(KIND=real_jlslsm) ::                                                      &
 smvcst_ij(row_length,rows)                                                    &
                            ! SMVCST on local ij grid
,smvcwt_ij(row_length,rows)                                                    &
                            ! SMVCWT on local ij grid
,smcl_dsm_ij(row_length,rows)                                                  &
                            ! SMCL on dsm_levels layer on local ij grid
,sthu_dsm_ij(row_length,rows)                                                  &
                            ! STHU on dsm_levels layer on local ij grid
,soil_sat_ij(row_length,rows)
                            ! Soil saturation point (in kg/m2) on local ij grid


REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
 smcl_dsm_ij_wtrac(:,:)                                                        &
                            ! Water tracer SMCL on dsm_levels layer on
                            !  local ij grid
,sthu_dsm_ij_wtrac(:,:)
                            ! Water tracer STHU on dsm_levels layer on
                            ! local ij grid


! Water tracer diagnostics:
! These are not currently output from this routine so are dummy fields.
! If they are required in the future, they will require an additional
! n_wtrac_jls dimension and initialising to zero.
REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
 riverout_atmos_wtrac_dum(:,:)                                                 &
                            ! Water tracer river flow out from each atmos
                            !  gridbox (kg/m2/s). Equivalent to (26,004)
,riverout_rgrid_wtrac_dum(:,:)                                                 &
                            ! Water tracer river flow out from TRIP grid
                            !  to ocean (kg/s). Equivalent to (26,005)
,box_outflow_wtrac_dum(:,:)                                                    &
                            ! Water tracer gridbox outflow on river grid (kg/s)
                            ! Equivalent to (26,002)
,box_inflow_wtrac_dum(:,:)                                                     &
                            ! Water tracer gridbox runoff on river grid(kg/s)
                            ! Equivalent to (26,003)
,inlandout_riv_wtrac_dum(:,:)
                            ! Water tracer outflow from inland basins on
                            ! TRIP grid (kg/s). Equivalent to (26,006)

REAL(KIND=real_jlslsm), DIMENSION(:,:), SAVE, ALLOCATABLE :: land_frac
                            ! land mask for global regridding weights
                            ! calculation

LOGICAL, SAVE :: first = .TRUE.
                            ! TRUE if this is the first entry to river routing

CHARACTER(LEN=errormessagelength) :: CMessage
                            ! Error message if return code >0

CHARACTER(LEN=*),PARAMETER :: RoutineName = 'RIV_INTCTL_1A'

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,                 &
                        zhook_handle)
info = 0

!-------------------------------------------------------------------------
!  1. Remove amount of lake evaporation from deep soil layer to conserve
!     moisture
!-------------------------------------------------------------------------

! Copy soil fields to full fields array
DO j = 1,rows
  DO i = 1,row_length
    smvcst_ij(i,j) = 0.0
    smvcwt_ij(i,j) = 0.0
    smcl_dsm_ij(i,j) = 0.0
    sthu_dsm_ij(i,j) = 0.0
    mask_for_lake_evap_corr(i,j) = 0
    soil_sat_ij(i,j) = 0.0
  END DO
END DO

DO l = 1, land_points
  j=(land_index(l) - 1) / row_length + 1
  i = land_index(l) - (j-1) * row_length

  !Note for soil tiling- these varaibles cannot be arithmetically
  !averaged, making conversion to be compatible with nsoilt > 1
  !rather difficult
  smvcst_ij(i,j)      = smvcst(l)
  smvcwt_ij(i,j)      = smvcwt(l)
  smcl_dsm_ij(i,j)    = smcl(l)
  sthu_dsm_ij(i,j)    = sthu(l)

  ! Calculate soil saturation point in kg/m2
  soil_sat_ij(i,j)    = smvcst_ij(i,j) * rho_water * dzsoil(dsm_levels)
END DO

! Set mask of gridboxes of where to apply the lake evaporation correction.
! This applies to both normal water & water tracers.
WHERE ( sthu_dsm_ij(:,:) * smvcst_ij(:,:) >  smvcwt_ij(:,:) )                  &
                         mask_for_lake_evap_corr(:,:) = 1


! Calculate the global average lake evaporation accumulated over river routing
! timestep

total_soil_area = 0.0

n_wt = 1  ! No. of water fields
CALL lake_evap_global_avg(row_length, rows, global_row_length, global_rows,    &
                          n_wt, mask_for_lake_evap_corr,                       &
                          a_boxareas, flandg, acc_lake_evap, 'water',          &
                          total_soil_area, acc_lake_evap_avg)

! Remove global average lake evaporation from deepest soil layer

CALL lake_evap_apply_soil                                                      &
               (row_length, rows, land_points, land_index,                     &
                mask_for_lake_evap_corr, soil_sat_ij,                          &
                acc_lake_evap_avg(1), 'water',                                 &
                sthu_dsm_ij, smcl_dsm_ij, sthu, smcl)


!-------------------------------------------------------------------------
! 2. Now do river routing
!-------------------------------------------------------------------------

! If first call to this routine, then gather land fraction to a global field
IF (first) THEN
  ALLOCATE(land_frac(global_row_length, global_rows))

  CALL all_gather_field(flandg, land_frac,                                     &
       lasize(1,fld_type_p,halo_type_no_halo),                                 &
       lasize(2,fld_type_p,halo_type_no_halo),                                 &
       glsize(1,fld_type_p), glsize(2,fld_type_p),                             &
       fld_type_p,halo_type_no_halo,                                           &
       gc_all_proc_group,info,cmessage)
  IF (info /= 0) THEN      ! Check return code
    cmessage='ATMPHB2 : ERROR in gather of flandg'
    icode = 5

    CALL Ereport(RoutineName,icode,Cmessage)
  END IF

END IF

! River routing

CALL riv_rout_1a_wrapper(global_row_length, global_rows, land_points,          &
      land_index, tot_surf_runoff_gb, tot_sub_runoff_gb,                       &
      row_length, rows, invert_atmos, xua, yva,                                &
      river_row_length, river_rows, flandg, riv_step, a_boxareas,              &
      twatstor, riverout_rgrid, trivdir, trivseq,                              &
      box_outflow, box_inflow, riverout_atmos, inlandout_riv,                  &
      inlandout_atmos, global_river_row_length, global_river_rows,             &
      land_frac, first, cmessage)

first = .FALSE.

!------------------------------------------------------------------------
! Repeat steps 1 and 2 for water tracers if present
!------------------------------------------------------------------------

IF (l_wtrac_jls) THEN

  ! Set up required water tracer fields

  ! Currently, there are no water tracer diagnostics output from this
  ! routine.  So set fields to dummy fields.
  ALLOCATE(riverout_atmos_wtrac_dum(row_length,rows))
  ALLOCATE(riverout_rgrid_wtrac_dum(river_row_length,river_rows))
  ALLOCATE(box_outflow_wtrac_dum(river_row_length,river_rows))
  ALLOCATE(box_inflow_wtrac_dum(river_row_length,river_rows))
  ALLOCATE(inlandout_riv_wtrac_dum(river_row_length,river_rows))

  ALLOCATE(smcl_dsm_ij_wtrac(row_length,rows))
  ALLOCATE(sthu_dsm_ij_wtrac(row_length,rows))

  DO j = 1, rows
    DO i = 1, row_length
      smcl_dsm_ij_wtrac(i,j) = 0.0
      sthu_dsm_ij_wtrac(i,j) = 0.0
    END DO
  END DO

  ! Calculate the global average lake evaporation accumulated over river routing
  ! timestep
  ! (Pass all water tracers into this routine so global_2d_sums only needs to
  !  be called once)
  n_wt = n_wtrac_jls  ! No. of water fields
  CALL lake_evap_global_avg(row_length, rows, global_row_length, global_rows,  &
                            n_wt, mask_for_lake_evap_corr,                     &
                            a_boxareas, flandg, acc_lake_evap_wtrac, 'wtrac',  &
                            total_soil_area, acc_lake_evap_avg_wtrac)

  DO i_wt = 1, n_wtrac_jls

    ! Set up water tracer soil fields on ij grid
    DO l = 1, land_points
      j=(land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length

      smcl_dsm_ij_wtrac(i,j)    = smcl_wtrac(l,1,dsm_levels,i_wt)
      sthu_dsm_ij_wtrac(i,j)    = sthu_wtrac(l,1,dsm_levels,i_wt)
    END DO

    ! Remove water tracer lake evaporation from water tracer in deep soil layer
    CALL lake_evap_apply_soil                                                  &
               (row_length, rows, land_points, land_index,                     &
                mask_for_lake_evap_corr, soil_sat_ij,                          &
                acc_lake_evap_avg_wtrac(i_wt), 'wtrac',                        &
                sthu_dsm_ij_wtrac, smcl_dsm_ij_wtrac,                          &
                sthu_wtrac(:,1,dsm_levels,i_wt),                               &
                smcl_wtrac(:,1,dsm_levels,i_wt))

    ! Call river routing routine with water tracer inflows and outflows
    CALL riv_rout_1a_wrapper(global_row_length, global_rows, land_points,      &
      land_index,                                                              &
      tot_surf_runoff_gb_wtrac(:,i_wt), tot_sub_runoff_gb_wtrac(:,i_wt),       &
      row_length, rows, invert_atmos, xua, yva,                                &
      river_row_length, river_rows, flandg, riv_step, a_boxareas,              &
      twatstor_wtrac(:,:,i_wt), riverout_rgrid_wtrac_dum(:,:),                 &
      trivdir, trivseq,                                                        &
      box_outflow_wtrac_dum(:,:), box_inflow_wtrac_dum(:,:),                   &
      riverout_atmos_wtrac_dum(:,:),                                           &
      inlandout_riv_wtrac_dum(:,:), inlandout_atmos_wtrac(:,:,i_wt),           &
      global_river_row_length, global_river_rows, land_frac, first, cmessage)

  END DO   ! i_wt

  DEALLOCATE(sthu_dsm_ij_wtrac)
  DEALLOCATE(smcl_dsm_ij_wtrac)

  DEALLOCATE(inlandout_riv_wtrac_dum)
  DEALLOCATE(box_inflow_wtrac_dum)
  DEALLOCATE(box_outflow_wtrac_dum)
  DEALLOCATE(riverout_rgrid_wtrac_dum)
  DEALLOCATE(riverout_atmos_wtrac_dum)

END IF     ! l_wtrac_jls

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,                &
                        zhook_handle)
RETURN
END SUBROUTINE riv_intctl_1a
END MODULE riv_intctl_mod_1A
#endif
