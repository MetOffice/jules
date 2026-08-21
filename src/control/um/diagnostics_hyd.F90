#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine diagnostics_hyd
!
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
! This file belongs in TECHNICAL
!
MODULE diagnostics_hyd_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName='DIAGNOSTICS_HYD_MOD'

CONTAINS

SUBROUTINE diagnostics_hyd(                                                    &
              row_length, rows,                                                &
              land_points, dsm_levels,                                         &
              land_index,inlandout_atm,                                        &
              smc, surf_roff, sub_surf_roff,                                   &
              snow_depth_land, snow_melt,  t_soil,                             &
              snow_soil_htf,                                                   &
              soil_layer_moisture,                                             &
              nsurft, snomlt_surf_htf, sthu, sthf,                             &
              tot_tfall, melt_surft,                                           &
              land_sea_mask,                                                   &
              dun_roff, drain, qbase, qbase_zw,                                &
              fch4_wetl,fch4_wetl_cs,fch4_wetl_npp,                            &
              fch4_wetl_resps,                                                 &
              fexp, gamtot, ti_mean, ti_sig,                                   &
              fsat, fwetl, zw, sthzw,                                          &
              timestep, stashwork, sf_diag,                                    &
              !TYPES containing field data
              fire_vars,progs)

! Description:
!   Calculates hydrology-related diagnostics (held in STASH section 8).
!
! Method:
!   Required level lists and logical switches are determined by the
!   calling routine from STASH requests and STASHflags.
!   Intercepted arrays and diagnostic arrays are input from the
!   hydrology routines called previously. Each diagnostic is simply
!   copied into the stashwork array to be passed on to STASH for
!   output processing.
!
!   Diagnostics currently available (in order calculated):
!   Item  Description
!    208  Soil moisture content
!     23  Snow depth
!    201  Snow melt
!    234  Surface run-off
!    235  Sub-surface run-off
!    225  Soil temperature
!    223  Soil layer moisture
!    204  Surface run-off (accumulated)
!    205  Sub-surface run-off (accumulated)
!    209  Canopy water content
!    202  Land snow melt heat flux
!    231  Land snow melt rate
!    233  Canopy throughfall rate
!    236  Snow amount on tiles
!    237  Snow melt rate on tiles
!    238  Snow grain size on tiles
!    229  Unfrozen soil moisture fraction
!    230  Frozen soil moisture fraction
!    239  Baseflow
!    240  Dunne Runoff
!    241  Baseflow from deep LSH/TOPMODEL layer
!    242  Wetland methane flux
!    252  Drainage out of "nshyd"th model layer
!    245  Inland basin outflow on atmos grid
!    252  Drainage out of bottom "nshyd"th soil layer (currently layer 4)
!    376  Snow depth on tiles.
!------------------------------------------------------------
!  interactive fire and emissions scheme (INFERNO)
!    700 tile total burnt area fraction (1/s)
!    701 gridcell-integrated biomass burning emitted carbon (kgC/m2/s)
!    702 gridcell-integrated DPM biomass burning emitted carbon (kgC/m2/s)
!    703 gridcell-integrated RPM biomass burning emitted carbon (kgC/m2/s)
!    710 per PFT burnt area fraction (1/s)
!    711 per PFT biomass burning emitted carbon (kgC/m2/s)
!    720 gridcell-integrated biomass burning emitted CO2 (kgC/m2/s)
!    721 gridcell-integrated biomass burning emitted CO (kgC/m2/s)
!    722 gridcell-integrated biomass burning emitted CH4 (kgC/m2/s)
!    723 gridcell-integrated biomass burning emitted NOx (kgC/m2/s)
!    735 per PFT biomass burning emitted CO2 (kgC/m2/s)
!    736 per PFT biomass burning emitted CO (kgC/m2/s)
!    737 per PFT biomass burning emitted CH4 (kgC/m2/s)
!    738 per PFT biomass burning emitted NOx (kgC/m2/s)
!    750 gridcell-integrated biomass burning emitted SO2 (kgC/m2/s)
!    751 gridcell-integrated biomass burning emitted BC (kgC/m2/s)
!    752 gridcell-integrated biomass burning emitted OC (kgC/m2/s)
!    765 per PFT biomass burning emitted SO2 (kgC/m2/s)
!    766 per PFT biomass burning emitted BC (kgC/m2/s)
!    767 per PFT biomass burning emitted OC (kgC/m2/s)
!    780 gridcell-integrated biomass burning emitted C2H4 (kgC/m2/s)
!    781 gridcell-integrated biomass burning emitted C2H6 (kgC/m2/s)
!    782 gridcell-integrated biomass burning emitted C3H8 (kgC/m2/s)
!    783 gridcell-integrated biomass burning emitted HCHO (kgC/m2/s)
!    784 gridcell-integrated biomass burning emitted MeCHO (kgC/m2/s)
!    785 gridcell-integrated biomass burning emitted NH3 (kgC/m2/s)
!    786 gridcell-integrated biomass burning emitted DMS (kgC/m2/s)
!    790 per PFT biomass burning emitted C2H4 (kgC/m2/s)
!    791 per PFT biomass burning emitted C2H6 (kgC/m2/s)
!    792 per PFT biomass burning emitted C3H8 (kgC/m2/s)
!    793 per PFT biomass burning emitted HCHO (kgC/m2/s)
!    794 per PFT biomass burning emitted MeCHO (kgC/m2/s)
!    795 per PFT biomass burning emitted NH3 (kgC/m2/s)
!    796 per PFT biomass burning emitted DMS (kgC/m2/s)

USE mask_compression, ONLY:  expand_from_mask
USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

USE stash_array_mod, ONLY:                                                     &
    sf, si, stlist, stindex, len_stlist, stash_pseudo_levels,                  &
    num_stash_pseudo, stash_levels, num_stash_levels, si_last

USE sf_diags_mod, ONLY: strnewsfdiag

USE missing_data_mod, ONLY: rmdi

!     INFERNO diagnostics
USE jules_surface_types_mod,  ONLY: npft
USE jules_inferno_mod, ONLY: l_inferno

USE set_levels_list_mod, ONLY: set_levels_list
USE set_pseudo_list_mod, ONLY: set_pseudo_list

USE copydiag_3d_mod, ONLY: copydiag_3d
USE copydiag_mod,    ONLY: copydiag

!TYPE definitions
USE fire_vars_mod, ONLY: fire_vars_type
USE prognostics, ONLY: progs_type

IMPLICIT NONE

! Arguments with Intent IN. ie: Input variables.

INTEGER ::                                                                     &
  row_length                                                                   &
                   ! number of points on a row
, rows
                   ! number of rows in a theta field

INTEGER ::                                                                     &
  land_points                                                                  &
              ! No.of land points being processed, can be 0.
, dsm_levels                                                                   &
, nsurft      ! No. of land-surface tiles ( MOSES II )

REAL ::                                                                        &
  timestep

! Primary Arrays used in all models
INTEGER ::                                                                     &
  land_index(land_points)      ! set from land_sea_mask

REAL ::                                                                        &
  snow_depth_land(land_points)                                                 &
                               !
, snow_melt(land_points)                                                       &
                          ! snowmelt (kg/m2/s)
, smc(land_points)                                                             &
                      ! available soil moisture in the
!                                 rootzone (kg/m2).
      , surf_roff(land_points)                                                 &
                                ! surface runoff (kg/m2/s).
      , sub_surf_roff(land_points)                                             &
                                    ! sub-surface runoff
! Declare inland basin outflow variable
      , inlandout_atm(land_points)                                             &
                                    !inland basin outflow

      , t_soil(land_points,dsm_levels)                                         &
      , snow_soil_htf(land_points,nsurft)                                      &
      , soil_layer_moisture(land_points,dsm_levels)                            &
      , snomlt_surf_htf(land_points)                                           &
      , sthu(land_points,dsm_levels)                                           &
                                       ! Unfrozen soil moisture
!                content of each layer as a fraction of saturation
      , sthf(land_points,dsm_levels)                                           &
                                       ! Frozen soil moisture content
!                of each layer as a fraction of saturation
      , tot_tfall(land_points)                                                 &
                                       ! total throughfall (kg/m2/s)
      , melt_surft(land_points,nsurft)                                         &
                                       ! Snowmelt on tiles (kg/m2/s)

! Additional variables required for large-scale hydrology:
      , qbase(land_points)                                                     &
                                    ! Baseflow (kg/m2/s).
      , dun_roff(land_points)                                                  &
                                    ! Dunne runoff (kg/m2/s).
      , qbase_zw(land_points)                                                  &
                                    ! Baseflow from deep LSH/TOPMODEL layer
!                                   ! (kg/m2/s).
      , drain(land_points)                                                     &
                                    ! Drainage out of "nshyd" later
!                                   ! (kg/m2/s).
      , fch4_wetl(land_points)                                                 &
                                    ! Wetland methane flux
!                                   ! (default substrate) for atmos
!                                   ! chemistry (10^-9 kg C/m2/s).
      ,fch4_wetl_cs(land_points)                                               &
                                    ! Wetland methane flux (soil carbon
!                                   ! substrate) (kg C/m2/s)
      ,fch4_wetl_npp(land_points)                                              &
                                    ! Wetland methane flux (npp
!                                   ! substrate) (kg C/m2/s)
      ,fch4_wetl_resps(land_points)                                            &
                                    ! Wetland methane flux (soil respiration
!                                   ! substrate) (kg C/m2/s)
      , ti_mean(land_points)                                                   &
                                    ! Mean topographic index.
      , ti_sig(land_points)                                                    &
                                    ! Std. dev. of topographic index.
      , fexp(land_points)                                                      &
                                    ! Decay factor in Sat. Conductivity
!                                   !   in deep LSH/TOPMODEL layer.
      , gamtot(land_points)                                                    &
                                    ! Integrated complete Gamma
!                                   !   function.
      , fsat(land_points)                                                      &
                                    ! Surface saturation fraction.
      , fwetl(land_points)                                                     &
                                    ! Wetland fraction.
      , zw(land_points)                                                        &
                                    ! Water table depth (m).
      , sthzw(land_points)          ! Sat. fraction in deep LSH/TOPMODEL layer.


LOGICAL ::                                                                     &
  land_sea_mask(row_length, rows)

INTEGER ::                                                                     &
 pslevel                                                                       &
               !  loop counter for pseudolevels
,pslevel_out                                                                   &
               !  index for pseudolevels sent to STASH
,level                                                                         &
               !  loop counter for levels
,level_out     !  index for levels sent to STASH

INTEGER :: land_points_expand
! To catch the output maskPoints variable in calls to expand_from_mask()

LOGICAL ::                                                                     &
 plltile(nsurft)                                                               &
                    ! pseudolevel list for surface types
,pllpft(npft)                                                                  &
                    ! pseudolevel list for pft types
,list(dsm_levels)   ! level list for soil levels

TYPE (strnewsfdiag) :: sf_diag ! contains snow diagnostic arrays

!JULES TYPEs
TYPE(fire_vars_type), INTENT(IN OUT) :: fire_vars
TYPE(progs_type), INTENT(IN OUT) :: progs

! Local variables

INTEGER ::                                                                     &
  i, j, k, l

INTEGER :: npoints_ij     !row_length * rows
INTEGER :: si_start, si_stop !Start and stop indices for stashwork when required

CHARACTER(LEN=*) :: RoutineName
PARAMETER ( RoutineName='DIAGNOSTICS_HYD')

INTEGER ::                                                                     &
  im_index        ! internal model index

REAL ::                                                                        &
  interp_data(row_length,rows)                                                 &
, interp_data_3(row_length,rows,dsm_levels)

! Diagnostic variables
REAL ::                                                                        &
 stashwork( * )    ! STASH workspace

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

! ----------------------------------------------------------------------
! Section 1.  Initialisation.
! ----------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
im_index = 1

! row_length * rows is calculated ~50 times
npoints_ij = row_length * rows

! ----------------------------------------------------------------------
! Soil moisture content
! ----------------------------------------------------------------------
! Item 8 208  smc

IF (sf(208,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l)                                    &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index,smc)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = smc(l)
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag (stashwork(si(208,8,im_index):si_last(208,8,im_index))         &
       ,interp_data,row_length,rows)
END IF


! ----------------------------------------------------------------------
! Snow depth
! ----------------------------------------------------------------------
! Item 8 023 snow_depth_land

IF (sf(023,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(snow_depth_land)            &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = snow_depth_land(l)
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag(stashwork(si(023,8,im_index):si_last(023,8,im_index))          &
       ,interp_data,row_length,rows)
END IF

! ----------------------------------------------------------------------
! Snow melt
! ----------------------------------------------------------------------
! Item 201

IF (sf(201,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(snow_melt,timestep)         &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = snow_melt(l) * timestep
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag(stashwork(si(201,8,im_index):si_last(201,8,im_index))          &
       ,interp_data,row_length,rows)
END IF


! ----------------------------------------------------------------------
! Surface run-off.
! ----------------------------------------------------------------------
! Item 234  surf_roff

IF (sf(234,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(surf_roff)                  &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = surf_roff(l)
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag(stashwork(si(234,8,im_index):si_last(234,8,im_index))          &
       ,interp_data,row_length,rows)
END IF

! ----------------------------------------------------------------------
! Sub-surface run-off.
! ----------------------------------------------------------------------
! Item 235  sub_surf_roff

IF (sf(235,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(sub_surf_roff)              &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = sub_surf_roff(l)
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag(stashwork(si(235,8,im_index):si_last(235,8,im_index))          &
       ,interp_data,row_length,rows)
END IF


! ----------------------------------------------------------------------
!  Soil temperature
! ----------------------------------------------------------------------
! Item 8 225 t_soil

IF (sf(225,8)) THEN

!$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(i,j,k,l) SHARED(t_soil,dsm_levels)     &
!$OMP SHARED(rows,row_length,interp_data_3,land_points,land_index)             &
!$OMP SCHEDULE(STATIC)
  DO k = 1, dsm_levels
    DO j= 1, rows
      DO i = 1, row_length
        interp_data_3(i,j,k) = rmdi
      END DO
    END DO

    DO l = 1, land_points
      j=(land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data_3(i,j,k) = t_soil(l,k)
    END DO

  END DO
!$OMP END PARALLEL DO

  CALL copydiag_3d(stashwork(si(225,8,im_index):si_last(225,8,im_index)),      &
       interp_data_3,                                                          &
       row_length,rows,dsm_levels,                                             &
       stlist(:,stindex(1,225,8,im_index)),len_stlist,                         &
       stash_levels,num_stash_levels+1)
END IF

! ----------------------------------------------------------------------
! Snow depth on ground on tiles (m)
! ----------------------------------------------------------------------
! Item 376 hsnow_surft

IF (sf(376,8)) THEN
  CALL set_pseudo_list(nsurft,len_stlist,                                      &
       stlist(1,stindex(1,376,8,im_index)),                                    &
       plltile,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0
  DO pslevel = 1,nsurft
    IF (plltile(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      CALL expand_from_mask (                                                  &
          stashwork(si(376,8,im_index) + (pslevel_out-1)                       &
          *npoints_ij),progs%snowdepth_surft(:,pslevel_out),                   &
          land_sea_mask,npoints_ij,land_points_expand)
    END IF
  END DO
END IF

! ----------------------------------------------------------------------
! Temperature of elevated sub-surface
! ----------------------------------------------------------------------
! Item 576 tsurf_elev_surft

IF (sf(576,8)) THEN
  CALL set_pseudo_list(nsurft,len_stlist,                                      &
       stlist(1,stindex(1,576,8,im_index)),                                    &
       plltile,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0
  DO pslevel = 1,nsurft
    IF (plltile(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      CALL expand_from_mask (                                                  &
          stashwork(si(576,8,im_index) + (pslevel_out-1)                       &
          *npoints_ij),progs%tsurf_elev_surft(:,pslevel_out),                  &
          land_sea_mask,npoints_ij,land_points_expand)
    END IF
  END DO
END IF

! ----------------------------------------------------------------------
! Heat flux to elevated sub-surface
! ----------------------------------------------------------------------
! Item 577 snow_soil_htf

IF (sf(577,8)) THEN
  CALL set_pseudo_list(nsurft,len_stlist,                                      &
       stlist(1,stindex(1,577,8,im_index)),                                    &
       plltile,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0
  DO pslevel = 1,nsurft
    IF (plltile(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      CALL expand_from_mask (                                                  &
          stashwork(si(577,8,im_index) + (pslevel_out-1)                       &
          *npoints_ij),snow_soil_htf(:,pslevel_out),                           &
          land_sea_mask,npoints_ij,land_points_expand)
    END IF
  END DO
END IF
!
! ----------------------------------------------------------------------
! Tiled snowpack mass balance
! ----------------------------------------------------------------------
! Item 578 snice_smb_surft

IF (sf(578,8)) THEN
  CALL set_pseudo_list(nsurft,len_stlist,                                      &
       stlist(1,stindex(1,578,8,im_index)),                                    &
       plltile,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0
  DO pslevel = 1,nsurft
    IF (plltile(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      CALL expand_from_mask (                                                  &
          stashwork(si(578,8,im_index) + (pslevel_out-1)                       &
          *npoints_ij),                                                        &
          sf_diag%snice_smb_surft(:,pslevel_out),                              &
          land_sea_mask,npoints_ij,land_points_expand)
    END IF
  END DO
END IF
!
! ----------------------------------------------------------------------
! Tiled snowpack total melt rate
! ----------------------------------------------------------------------
! Item 579 snice_m_surft

IF (sf(579,8)) THEN
  CALL set_pseudo_list(nsurft,len_stlist,                                      &
       stlist(1,stindex(1,579,8,im_index)),                                    &
       plltile,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0
  DO pslevel = 1,nsurft
    IF (plltile(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      CALL expand_from_mask (                                                  &
          stashwork(si(579,8,im_index) + (pslevel_out-1)                       &
          *npoints_ij),sf_diag%snice_m_surft(:,pslevel_out),                   &
          land_sea_mask,npoints_ij,land_points_expand)
    END IF
  END DO
END IF

!
! ----------------------------------------------------------------------
! Tiled snowpack total refreeze rate
! ----------------------------------------------------------------------
! Item 580 snice_freez_surft

IF (sf(580,8)) THEN
  CALL set_pseudo_list(nsurft,len_stlist,                                      &
       stlist(1,stindex(1,580,8,im_index)),                                    &
       plltile,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0
  DO pslevel = 1,nsurft
    IF (plltile(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      CALL expand_from_mask (                                                  &
          stashwork(si(580,8,im_index) + (pslevel_out-1)                       &
          *npoints_ij),                                                        &
          sf_diag%snice_freez_surft(:,pslevel_out),                            &
          land_sea_mask,npoints_ij,land_points_expand)
    END IF
  END DO
END IF

!
! ----------------------------------------------------------------------
! Tiled snowpack rate of change in solid mass
! ----------------------------------------------------------------------
! Item 581 snice_sicerate_surft

IF (sf(581,8)) THEN
  CALL set_pseudo_list(nsurft,len_stlist,                                      &
       stlist(1,stindex(1,581,8,im_index)),                                    &
       plltile,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0
  DO pslevel = 1,nsurft
    IF (plltile(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      CALL expand_from_mask (                                                  &
          stashwork(si(581,8,im_index) + (pslevel_out-1)                       &
          *npoints_ij),                                                        &
          sf_diag%snice_sicerate_surft(:,pslevel_out),                         &
          land_sea_mask,npoints_ij,land_points_expand)
    END IF
  END DO
END IF

!
! ----------------------------------------------------------------------
! Tiled snowpack rate of change in liquid mass
! ----------------------------------------------------------------------
! Item 582 snice_sliqrate_surft

IF (sf(582,8)) THEN
  CALL set_pseudo_list(nsurft,len_stlist,                                      &
       stlist(1,stindex(1,582,8,im_index)),                                    &
       plltile,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0
  DO pslevel = 1,nsurft
    IF (plltile(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      CALL expand_from_mask (                                                  &
          stashwork(si(582,8,im_index) + (pslevel_out-1)                       &
          *npoints_ij),                                                        &
          sf_diag%snice_sliqrate_surft(:,pslevel_out),                         &
          land_sea_mask,npoints_ij,land_points_expand)
    END IF
  END DO
END IF

!
! ----------------------------------------------------------------------
! Tiled snowpack net runoff rate
! ----------------------------------------------------------------------
! Item 583 snice_runoff_surft

IF (sf(583,8)) THEN
  CALL set_pseudo_list(nsurft,len_stlist,                                      &
       stlist(1,stindex(1,583,8,im_index)),                                    &
       plltile,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0
  DO pslevel = 1,nsurft
    IF (plltile(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      CALL expand_from_mask (                                                  &
          stashwork(si(583,8,im_index) + (pslevel_out-1)                       &
          *npoints_ij),                                                        &
          sf_diag%snice_runoff_surft(:,pslevel_out),                           &
          land_sea_mask,npoints_ij,land_points_expand)
    END IF
  END DO
END IF


!
! ----------------------------------------------------------------------
! Soil layer moisture
! ----------------------------------------------------------------------
! Item 8 223 soil_layer_moisture

IF (sf(223,8)) THEN

!$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(i,j,l,k) SHARED(soil_layer_moisture)   &
!$OMP SHARED(rows,row_length,interp_data_3,land_points,land_index)             &
!$OMP SCHEDULE(STATIC) SHARED(dsm_levels)
  DO k = 1, dsm_levels
    DO j= 1, rows
      DO i = 1, row_length
        interp_data_3(i,j,k) = rmdi
      END DO
    END DO
    DO l = 1, land_points
      j=(land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data_3(i,j,k) =                                                   &
           soil_layer_moisture(l,k)
    END DO
  END DO
!$OMP END PARALLEL DO

  CALL copydiag_3d(stashwork(si(223,8,im_index):si_last(223,8,im_index)),      &
       interp_data_3,                                                          &
       row_length,rows,dsm_levels,                                             &
       stlist(:,stindex(1,223,8,im_index)),len_stlist,                         &
       stash_levels,num_stash_levels+1)
END IF

! ----------------------------------------------------------------------
! Surface run-off (accumulated)
! ----------------------------------------------------------------------
! Item 204  surf_roff

IF (sf(204,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(surf_roff,timestep)         &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = surf_roff(l) * timestep
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag(stashwork(si(204,8,im_index):si_last(204,8,im_index))          &
       ,interp_data,row_length,rows)
END IF

! ----------------------------------------------------------------------
! Sub-surface run-off (accumulated)
! ----------------------------------------------------------------------
! Item 205  sub_surf_roff

IF (sf(205,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(sub_surf_roff,timestep)     &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = sub_surf_roff(l)                                        &
  * timestep
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag(stashwork(si(205,8,im_index):si_last(205,8,im_index))          &
       ,interp_data,row_length,rows)
END IF

! ----------------------------------------------------------------------
! Canopy water content
! ----------------------------------------------------------------------
! Item 209

IF (sf(209,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(progs)                      &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = progs%canopy_gb(l)
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag(stashwork(si(209,8,im_index):si_last(209,8,im_index))          &
       ,interp_data,row_length,rows)
END IF


! ----------------------------------------------------------------------
! Land snow melt heat flux (W/m2)
! ----------------------------------------------------------------------
! Item 202 SNOMLT_SURF_HTF

IF (sf(202,8)) THEN
  CALL expand_from_mask (                                                      &
      stashwork(si(202,8,im_index)),                                           &
      snomlt_surf_htf,                                                         &
      land_sea_mask,npoints_ij,land_points_expand)
END IF


! ----------------------------------------------------------------------
! Land snow melt heat rate (Kg/m2/s)
! ----------------------------------------------------------------------
! Item 231 snow_melt

IF (sf(231,8)) THEN
  CALL expand_from_mask (                                                      &
      stashwork(si(231,8,im_index)),                                           &
      snow_melt,                                                               &
      land_sea_mask,npoints_ij,land_points_expand)
END IF


! ----------------------------------------------------------------------
! Canopy throughfall rate (Kg/m2/s)
! ----------------------------------------------------------------------
! Item 233 tot_tfall

IF (sf(233,8)) THEN
  CALL expand_from_mask (                                                      &
      stashwork(si(233,8,im_index)),                                           &
      tot_tfall,                                                               &
      land_sea_mask,npoints_ij,land_points_expand)
END IF


! ----------------------------------------------------------------------
! Snow amount on tiles (Kg/m2)
! ----------------------------------------------------------------------
! Item 236 snow_tile

IF (sf(236,8)) THEN
  CALL set_pseudo_list(nsurft,len_stlist,                                      &
       stlist(1,stindex(1,236,8,im_index)),                                    &
       plltile,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0
  DO pslevel = 1,nsurft
    IF (plltile(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      CALL expand_from_mask (                                                  &
          stashwork(si(236,8,im_index) + (pslevel_out-1)                       &
          *npoints_ij),progs%snow_surft(:,pslevel_out),                        &
          land_sea_mask,npoints_ij,land_points_expand)
    END IF
  END DO
END IF



! ----------------------------------------------------------------------
! Snow melt rate on tiles (Kg/m2)
! ----------------------------------------------------------------------
! Item 237 melt_tile

IF (sf(237,8)) THEN
  CALL set_pseudo_list(nsurft,len_stlist,                                      &
       stlist(1,stindex(1,237,8,im_index)),                                    &
       plltile,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0
  DO pslevel = 1,nsurft
    IF (plltile(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      CALL expand_from_mask (                                                  &
          stashwork(si(237,8,im_index) + (pslevel_out-1)                       &
          *npoints_ij),melt_surft(:,pslevel_out),                              &
          land_sea_mask,npoints_ij,land_points_expand)
    END IF
  END DO
END IF



! ----------------------------------------------------------------------
! Snow grain size on tiles
! ----------------------------------------------------------------------
! Item 238 rgrain

IF (sf(238,8)) THEN
  CALL set_pseudo_list(nsurft,len_stlist,                                      &
       stlist(1,stindex(1,238,8,im_index)),                                    &
       plltile,stash_pseudo_levels,num_stash_pseudo)
  pslevel_out = 0
  DO pslevel = 1,nsurft
    IF (plltile(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      CALL expand_from_mask (                                                  &
          stashwork(si(238,8,im_index) + (pslevel_out-1)                       &
          *npoints_ij),progs%rgrain_surft(:,pslevel_out),                      &
          land_sea_mask,npoints_ij,land_points_expand)
    END IF
  END DO
END IF



! ----------------------------------------------------------------------
! Unfrozen soil moisture fraction
! ----------------------------------------------------------------------
! Item 229 sthu

IF (sf(229,8)) THEN
  CALL set_levels_list(dsm_levels,len_stlist,                                  &
       stlist(1,stindex(1,229,8,im_index)),                                    &
       list,stash_levels,num_stash_levels+1)
  level_out = 0
  DO level = 1,dsm_levels
    IF (list(level)) THEN
      level_out = level_out + 1
      CALL expand_from_mask (                                                  &
          stashwork(si(229,8,im_index) + (level_out-1)                         &
          *npoints_ij),sthu(:,level_out),                                      &
          land_sea_mask,npoints_ij,land_points_expand)
    END IF
  END DO
END IF


! ----------------------------------------------------------------------
! Frozen soil moisture fraction
! ----------------------------------------------------------------------
! Item 230 sthf

IF (sf(230,8)) THEN
  CALL set_levels_list(dsm_levels,len_stlist,                                  &
       stlist(1,stindex(1,230,8,im_index)),                                    &
       list,stash_levels,num_stash_levels+1)
  level_out = 0
  DO level = 1,dsm_levels
    IF (list(level)) THEN
      level_out = level_out + 1
      CALL expand_from_mask (                                                  &
          stashwork(si(230,8,im_index) + (level_out-1)                         &
          *npoints_ij),sthf(:,level_out),                                      &
          land_sea_mask,npoints_ij,land_points_expand)
    END IF
  END DO
END IF


! ----------------------------------------------------------------------
! Baseflow
! ----------------------------------------------------------------------
! Item 239  baseflow

IF (sf(239,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(qbase)                      &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = qbase(l)
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag(stashwork(si(239,8,im_index):si_last(239,8,im_index))          &
       ,interp_data,row_length,rows)
END IF

! ----------------------------------------------------------------------
! Dunne Runoff
! ----------------------------------------------------------------------
! Item 240 Dunne Runoff

IF (sf(240,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(dun_roff)                   &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = dun_roff(l)
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag(stashwork(si(240,8,im_index):si_last(240,8,im_index))          &
       ,interp_data,row_length,rows)
END IF

! ----------------------------------------------------------------------
! Baseflow from deep LSH/TOPMODEL layer
! ----------------------------------------------------------------------
! Item 241  baseflow from deep LSH/TOPMODEL layer

IF (sf(241,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(qbase_zw)                   &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = qbase_zw(l)
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag(stashwork(si(241,8,im_index):si_last(241,8,im_index))          &
       ,interp_data,row_length,rows)
END IF

! ----------------------------------------------------------------------
! Wetland methane flux used as input into UKCA
! ----------------------------------------------------------------------
! Item 242  Wetland methane flux used as input into UKCA

IF (sf(242,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fch4_wetl)                  &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = fch4_wetl(l)
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag(stashwork(si(242,8,im_index):si_last(242,8,im_index))          &
       ,interp_data,row_length,rows)
END IF

IF (sf(243,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(ti_mean)                    &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = ti_mean(l)
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag(stashwork(si(243,8,im_index):si_last(243,8,im_index))          &
       ,interp_data,row_length,rows)
END IF

IF (sf(244,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(ti_sig)                     &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = ti_sig(l)
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag(stashwork(si(244,8,im_index):si_last(244,8,im_index))          &
       ,interp_data,row_length,rows)
END IF

IF (sf(251,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fexp)                       &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = fexp(l)
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag(stashwork(si(251,8,im_index):si_last(251,8,im_index))          &
       ,interp_data,row_length,rows)
END IF

IF (sf(246,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(gamtot)                     &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = gamtot(l)
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag(stashwork(si(246,8,im_index):si_last(246,8,im_index))          &
       ,interp_data,row_length,rows)
END IF

IF (sf(247,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fsat)                       &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = fsat(l)
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag(stashwork(si(247,8,im_index):si_last(247,8,im_index))          &
       ,interp_data,row_length,rows)
END IF

IF (sf(248,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fwetl)                      &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = fwetl(l)
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag(stashwork(si(248,8,im_index):si_last(248,8,im_index))          &
       ,interp_data,row_length,rows)
END IF

IF (sf(249,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(zw)                         &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = zw(l)
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag(stashwork(si(249,8,im_index):si_last(249,8,im_index))          &
       ,interp_data,row_length,rows)
END IF

IF (sf(250,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(sthzw)                      &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = sthzw(l)
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag(stashwork(si(250,8,im_index):si_last(250,8,im_index))          &
       ,interp_data,row_length,rows)
END IF

! ----------------------------------------------------------------------
! Drainage out of  "nshyd"th model layer
! ----------------------------------------------------------------------
! Item 252  drainage

IF (sf(252,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(drain)                      &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = drain(l)
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag(stashwork(si(252,8,im_index):si_last(252,8,im_index))          &
       ,interp_data,row_length,rows)
END IF

! ----------------------------------------------------------------------
! Output inland basin outflow on atmosphere grid

! --------------------------------------------------------------------
! Inland basin outflow (atmos grid)
! --------------------------------------------------------------------
! Item 245  Inland basin outflow

IF (sf(245,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(inlandout_atm)              &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = inlandout_atm(l)
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag(stashwork(si(245,8,im_index):si_last(245,8,im_index))          &
       ,interp_data,row_length,rows)
END IF
! ----------------------------------------------------------------------
! Wetland methane flux using soil carbon-based substrate
! ----------------------------------------------------------------------
! Item 260  Wetland methane flux using soil carbon-based substrate

IF (sf(260,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fch4_wetl_cs)               &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = fch4_wetl_cs(l)
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag(STASHwork(si(260,8,im_index):si_last(260,8,im_index))          &
       ,interp_data,row_length,rows)
END IF
! ----------------------------------------------------------------------
! Wetland methane flux using NPP-based substrate
! ----------------------------------------------------------------------
! Item 261  Wetland methane flux using NPP-based substrate

IF (sf(261,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fch4_wetl_npp)              &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = fch4_wetl_npp(l)
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag(STASHwork(si(261,8,im_index):si_last(261,8,im_index))          &
       ,interp_data,row_length,rows)
END IF
! ----------------------------------------------------------------------
! Wetland methane flux using soil-respiration-based substrate
! ----------------------------------------------------------------------
! Item 262  Wetland methane flux using soil-respiration-based substrate

IF (sf(262,8)) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fch4_wetl_resps)            &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_points
    j=(land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = fch4_wetl_resps(l)
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL copydiag(STASHwork(si(262,8,im_index):si_last(262,8,im_index))          &
       ,interp_data,row_length,rows)
END IF

! ---------------------------------------------------------------------
! ------------- Fires and emissions diagnostics (INFERNO) -------------
! ---------------------------------------------------------------------

IF ( l_inferno ) THEN

  ! -------------------------------------------------------------------
  ! INFERNO (interactive fire) total burnt area
  ! -------------------------------------------------------------------
  ! Item 700  INFERNO (interactive fire) total burnt area

  IF ( sf(700, 8) ) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fire_vars)                  &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
    DO j = 1, rows
      DO i = 1, row_length
        interp_data(i, j) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_points
      j = (land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data(i, j) = fire_vars%burnt_area(l)
    END DO
!$OMP END DO
!$OMP END PARALLEL

    CALL copydiag( stashwork(si(700, 8, im_index):si_last(700, 8, im_index)),  &
          interp_data,row_length, rows)
  END IF ! sf(700, 8)

  ! --------------------------------------------------------------------
  ! Total biomass burning emitted carbon
  ! --------------------------------------------------------------------
  ! Item 701  total biomass burning emitted carbon

  IF ( sf(701, 8) ) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fire_vars)                  &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
    DO j = 1, rows
      DO i = 1, row_length
        interp_data(i, j) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_points
      j = (land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data(i, j) = fire_vars%emitted_carbon(l)
    END DO
!$OMP END DO
!$OMP END PARALLEL

    CALL copydiag(stashwork(si(701, 8, im_index):si_last(701, 8, im_index))    &
         , interp_data,row_length,rows)

  END IF ! sf(701, 8)

  ! --------------------------------------------------------------------
  ! DPM (soil carbon pool) biomass burning emitted carbon
  ! --------------------------------------------------------------------
  ! Item 702  DPM (soil carbon pool) biomass burning emitted carbon

  IF ( sf(702, 8) ) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fire_vars)                  &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
    DO j = 1, rows
      DO i = 1, row_length
        interp_data(i, j) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_points
      j = (land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data(i, j) = fire_vars%emitted_carbon_dpm(l)
    END DO
!$OMP END DO
!$OMP END PARALLEL

    CALL copydiag(stashwork(si(702, 8, im_index):si_last(702, 8, im_index)),   &
          interp_data, row_length, rows)

  END IF ! sf(702, 8)

  ! --------------------------------------------------------------------
  ! RPM (soil carbon pool) biomass burning emitted carbon
  ! --------------------------------------------------------------------
  ! Item 703  RPM (soil carbon pool) biomass burning emitted carbon

  IF ( sf(703, 8) ) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fire_vars)                  &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
    DO j = 1, rows
      DO i = 1, row_length
        interp_data(i, j) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_points
      j = (land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data(i, j) = fire_vars%emitted_carbon_rpm(l)
    END DO
!$OMP END DO
!$OMP END PARALLEL

    CALL copydiag(stashwork(si(703, 8, im_index):si_last(703, 8, im_index)),   &
         interp_data, row_length, rows)
  END IF ! sf(703, 8)

  ! --------------------------------------------------------------------
  ! fire burnt area on PFTs
  ! --------------------------------------------------------------------
  ! Item 710  fire burnt area on PFTs

  IF ( sf(710, 8) ) THEN
    CALL set_pseudo_list(npft, len_stlist,                                     &
         stlist(1, stindex(1, 710, 8, im_index)),                              &
         pllpft, stash_pseudo_levels, num_stash_pseudo)
    pslevel_out = 0

    DO pslevel = 1, npft
      IF (pllpft(pslevel)) THEN
        pslevel_out = pslevel_out + 1
        DO j = 1, rows
          DO i = 1, row_length
            interp_data(i, j) = rmdi
          END DO
        END DO

        DO l = 1, land_points
          j = (land_index(l) - 1) / row_length + 1
          i = land_index(l) - (j-1) * row_length
          interp_data(i, j) = fire_vars%burnt_area_ft(l, pslevel_out)
        END DO

        si_start = si(710,8,im_index) + (pslevel_out-1) * npoints_ij
        si_stop  = si_start + npoints_ij - 1
        CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
      END IF
    END DO

  END IF ! sf(710, 8)

  ! --------------------------------------------------------------------
  ! biomass burning emitted carbon on PFTs
  ! --------------------------------------------------------------------
  ! Item 711  biomass burning emitted carbon on PFTs

  IF ( sf(711, 8) ) THEN
    CALL set_pseudo_list(npft, len_stlist,                                     &
         stlist(1, stindex(1, 711, 8, im_index)),                              &
         pllpft, stash_pseudo_levels, num_stash_pseudo)
    pslevel_out = 0

    DO pslevel = 1, npft
      IF (pllpft(pslevel)) THEN
        pslevel_out = pslevel_out + 1
        DO j = 1, rows
          DO i = 1, row_length
            interp_data(i, j) = rmdi
          END DO
        END DO

        DO l = 1, land_points
          j = (land_index(l) - 1) / row_length + 1
          i = land_index(l) - (j-1) * row_length
          interp_data(i, j) = fire_vars%emitted_carbon_ft(l, pslevel_out)
        END DO

        si_start = si(711,8,im_index) + (pslevel_out-1) * npoints_ij
        si_stop  = si_start + npoints_ij - 1
        CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)

      END IF
    END DO

  END IF ! sf(711, 8)

  ! --------------------------------------------------------------------
  ! biomass burning total CO2 emissions
  ! --------------------------------------------------------------------
  ! Item 720  biomass burning total CO2 emissions

  IF ( sf(720, 8) ) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fire_vars)                  &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
    DO j = 1, rows
      DO i = 1, row_length
        interp_data(i, j) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_points
      j = (land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data(i, j) = fire_vars%fire_em_co2(l)
    END DO
!$OMP END DO
!$OMP END PARALLEL

    CALL copydiag(stashwork(si(720, 8, im_index):si_last(720, 8, im_index)),   &
          interp_data, row_length, rows)
  END IF ! sf(720, 8)

  ! --------------------------------------------------------------------
  ! biomass burning total CO emissions
  ! --------------------------------------------------------------------
  ! Item 721  biomass burning total CO emissions

  IF ( sf(721, 8) ) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fire_vars)                  &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
    DO j = 1, rows
      DO i = 1, row_length
        interp_data(i, j) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_points
      j = (land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data(i, j) = fire_vars%fire_em_co(l)
    END DO
!$OMP END DO
!$OMP END PARALLEL

    CALL copydiag(stashwork(si(721, 8, im_index):si_last(721, 8, im_index)),   &
          interp_data, row_length, rows)
  END IF ! sf(721, 8)

  ! ----------------------------------------------------------------------
  ! biomass burning total CH4 emissions
  ! ----------------------------------------------------------------------
  ! Item 722  biomass burning total CH4 emissions

  IF ( sf(722, 8) ) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fire_vars)                  &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
    DO j = 1, rows
      DO i = 1, row_length
        interp_data(i, j) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_points
      j = (land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data(i, j) = fire_vars%fire_em_ch4(l)
    END DO
!$OMP END DO
!$OMP END PARALLEL

    CALL copydiag(stashwork(si(722, 8, im_index):si_last(722, 8, im_index)),   &
          interp_data, row_length, rows)
  END IF ! sf(722, 8)

  ! --------------------------------------------------------------------
  ! biomass burning total NOx emissions
  ! --------------------------------------------------------------------
  ! Item 723  biomass burning total NOx emissions

  IF ( sf(723, 8) ) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fire_vars)                  &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
    DO j = 1, rows
      DO i = 1, row_length
        interp_data(i, j) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_points
      j = (land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data(i, j) = fire_vars%fire_em_nox(l)
    END DO
!$OMP END DO
!$OMP END PARALLEL

    CALL copydiag(stashwork(si(723, 8, im_index):si_last(723, 8, im_index)),   &
          interp_data, row_length, rows)
  END IF ! sf(723, 8)

  ! --------------------------------------------------------------------
  ! biomass burning emitted CO2 on PFTs
  ! --------------------------------------------------------------------
  !  Item 735  biomass burning emitted CO2 on PFTs

  IF ( sf(735, 8) ) THEN
    CALL set_pseudo_list(npft, len_stlist,                                     &
         stlist(1, stindex(1, 735, 8, im_index)),                              &
         pllpft, stash_pseudo_levels, num_stash_pseudo)
    pslevel_out = 0

    DO pslevel = 1, npft
      IF (pllpft(pslevel)) THEN
        pslevel_out = pslevel_out + 1
        DO j = 1, rows
          DO i = 1, row_length
            interp_data(i, j) = rmdi
          END DO
        END DO

        DO l = 1, land_points
          j = (land_index(l) - 1) / row_length + 1
          i = land_index(l) - (j-1) * row_length
          interp_data(i, j) = fire_vars%fire_em_co2_ft(l, pslevel_out)
        END DO

        si_start = si(735,8,im_index) + (pslevel_out-1) * npoints_ij
        si_stop  = si_start + npoints_ij - 1
        CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
      END IF
    END DO

  END IF ! sf(735, 8)

  ! --------------------------------------------------------------------
  ! biomass burning emitted CO on PFTs
  ! --------------------------------------------------------------------
  !  Item 736  biomass burning emitted CO on PFTs

  IF ( sf(736, 8) ) THEN
    CALL set_pseudo_list(npft, len_stlist,                                     &
         stlist(1, stindex(1, 736, 8, im_index)),                              &
         pllpft, stash_pseudo_levels, num_stash_pseudo)
    pslevel_out = 0

    DO pslevel = 1, npft
      IF (pllpft(pslevel)) THEN
        pslevel_out = pslevel_out + 1
        DO j = 1, rows
          DO i = 1, row_length
            interp_data(i, j) = rmdi
          END DO
        END DO

        DO l = 1, land_points
          j = (land_index(l) - 1) / row_length + 1
          i = land_index(l) - (j-1) * row_length
          interp_data(i, j) = fire_vars%fire_em_co_ft(l, pslevel_out)
        END DO

        si_start = si(736,8,im_index) + (pslevel_out-1) * npoints_ij
        si_stop  = si_start + npoints_ij - 1
        CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
      END IF
    END DO

  END IF ! sf(736, 8)

  ! --------------------------------------------------------------------
  ! biomass burning emitted CH4 on PFTs
  ! --------------------------------------------------------------------
  !  Item 737  biomass burning emitted CH4 on PFTs

  IF ( sf(737, 8) ) THEN
    CALL set_pseudo_list(npft, len_stlist,                                     &
         stlist(1, stindex(1, 737, 8, im_index)),                              &
         pllpft, stash_pseudo_levels, num_stash_pseudo)
    pslevel_out = 0

    DO pslevel = 1, npft
      IF (pllpft(pslevel)) THEN
        pslevel_out = pslevel_out + 1
        DO j = 1, rows
          DO i = 1, row_length
            interp_data(i, j) = rmdi
          END DO
        END DO

        DO l = 1, land_points
          j = (land_index(l) - 1) / row_length + 1
          i = land_index(l) - (j-1) * row_length
          interp_data(i, j) = fire_vars%fire_em_ch4_ft(l, pslevel_out)
        END DO

        si_start = si(737,8,im_index) + (pslevel_out-1) * npoints_ij
        si_stop  = si_start + npoints_ij - 1
        CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
      END IF
    END DO

  END IF ! sf(737, 8)

  ! --------------------------------------------------------------------
  ! biomass burning emitted NOx on PFTs
  ! --------------------------------------------------------------------
  !  Item 738  biomass burning emitted NOx on PFTs

  IF ( sf(738, 8) ) THEN
    CALL set_pseudo_list(npft, len_stlist,                                     &
         stlist(1, stindex(1, 738, 8, im_index)),                              &
         pllpft, stash_pseudo_levels, num_stash_pseudo)
    pslevel_out = 0

    DO pslevel = 1, npft
      IF (pllpft(pslevel)) THEN
        pslevel_out = pslevel_out + 1
        DO j = 1, rows
          DO i = 1, row_length
            interp_data(i, j) = rmdi
          END DO
        END DO

        DO l = 1, land_points
          j = (land_index(l) - 1) / row_length + 1
          i = land_index(l) - (j-1) * row_length
          interp_data(i, j) = fire_vars%fire_em_nox_ft(l, pslevel_out)
        END DO

        si_start = si(738,8,im_index) + (pslevel_out-1) * npoints_ij
        si_stop  = si_start + npoints_ij - 1
        CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
      END IF
    END DO

  END IF ! sf(738, 8)

  ! --------------------------------------------------------------------
  ! biomass burning total SO2 emissions
  ! --------------------------------------------------------------------
  ! Item 750  biomass burning total SO2 emissions

  IF ( sf(750, 8) ) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fire_vars)                  &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
    DO j = 1, rows
      DO i = 1, row_length
        interp_data(i, j) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_points
      j = (land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data(i, j) = fire_vars%fire_em_so2(l)
    END DO
!$OMP END DO
!$OMP END PARALLEL

    CALL copydiag(stashwork(si(750, 8, im_index):si_last(750, 8, im_index)),   &
          interp_data, row_length, rows)
  END IF ! sf(750, 8)

  ! --------------------------------------------------------------------
  ! biomass burning total BC (Black Carbon) emissions
  ! --------------------------------------------------------------------
  ! Item 751  biomass burning total BC (Black Carbon) emissions

  IF ( sf(751, 8) ) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fire_vars)                  &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
    DO j = 1, rows
      DO i = 1, row_length
        interp_data(i, j) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_points
      j = (land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data(i, j) = fire_vars%fire_em_bc(l)
    END DO
!$OMP END DO
!$OMP END PARALLEL

    CALL copydiag(stashwork(si(751, 8, im_index):si_last(751, 8, im_index)),   &
          interp_data, row_length, rows)
  END IF ! sf(751, 8)

  ! ----------------------------------------------------------------------
  ! biomass burning total OC (Organic Carbon) emissions
  ! ----------------------------------------------------------------------
  ! Item 752  biomass burning total OC (Organic Carbon) emissions

  IF ( sf(752, 8) ) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fire_vars)                  &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
    DO j = 1, rows
      DO i = 1, row_length
        interp_data(i, j) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_points
      j = (land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data(i, j) = fire_vars%fire_em_oc(l)
    END DO
!$OMP END DO
!$OMP END PARALLEL

    CALL copydiag(stashwork(si(752, 8, im_index):si_last(752, 8, im_index)),   &
          interp_data, row_length, rows)
  END IF ! sf(752, 8)

  ! --------------------------------------------------------------------
  ! biomass burning emitted SO2 on PFTs
  ! --------------------------------------------------------------------
  !  Item 765  biomass burning emitted SO2 on PFTs

  IF ( sf(765, 8) ) THEN
    CALL set_pseudo_list(npft, len_stlist,                                     &
         stlist(1, stindex(1, 765, 8, im_index)),                              &
         pllpft, stash_pseudo_levels, num_stash_pseudo)
    pslevel_out = 0

    DO pslevel = 1, npft
      IF (pllpft(pslevel)) THEN
        pslevel_out = pslevel_out + 1
        DO j = 1, rows
          DO i = 1, row_length
            interp_data(i, j) = rmdi
          END DO
        END DO

        DO l = 1, land_points
          j = (land_index(l) - 1) / row_length + 1
          i = land_index(l) - (j-1) * row_length
          interp_data(i, j) = fire_vars%fire_em_so2_ft(l, pslevel_out)
        END DO

        si_start = si(765,8,im_index) + (pslevel_out-1) * npoints_ij
        si_stop  = si_start + npoints_ij - 1
        CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
      END IF
    END DO

  END IF ! sf(765, 8)

  ! --------------------------------------------------------------------
  ! biomass burning emitted BC on PFTs
  ! --------------------------------------------------------------------
  !  Item 766  biomass burning emitted BC on PFTs

  IF ( sf(766, 8) ) THEN
    CALL set_pseudo_list(npft, len_stlist,                                     &
         stlist(1, stindex(1, 766, 8, im_index)),                              &
         pllpft, stash_pseudo_levels, num_stash_pseudo)
    pslevel_out = 0

    DO pslevel = 1, npft
      IF (pllpft(pslevel)) THEN
        pslevel_out = pslevel_out + 1
        DO j = 1, rows
          DO i = 1, row_length
            interp_data(i, j) = rmdi
          END DO
        END DO

        DO l = 1, land_points
          j = (land_index(l) - 1) / row_length + 1
          i = land_index(l) - (j-1) * row_length
          interp_data(i, j) = fire_vars%fire_em_bc_ft(l, pslevel_out)
        END DO

        si_start = si(766,8,im_index) + (pslevel_out-1) * npoints_ij
        si_stop  = si_start + npoints_ij - 1
        CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
      END IF
    END DO

  END IF ! sf(766, 8)

  ! --------------------------------------------------------------------
  ! biomass burning emitted OC on PFTs
  ! --------------------------------------------------------------------
  !  Item 767  biomass burning emitted OC on PFTs

  IF ( sf(767, 8) ) THEN
    CALL set_pseudo_list(npft, len_stlist,                                     &
         stlist(1, stindex(1, 767, 8, im_index)),                              &
         pllpft, stash_pseudo_levels, num_stash_pseudo)
    pslevel_out = 0

    DO pslevel = 1, npft
      IF (pllpft(pslevel)) THEN
        pslevel_out = pslevel_out + 1
        DO j = 1, rows
          DO i = 1, row_length
            interp_data(i, j) = rmdi
          END DO
        END DO

        DO l = 1, land_points
          j = (land_index(l) - 1) / row_length + 1
          i = land_index(l) - (j-1) * row_length
          interp_data(i, j) = fire_vars%fire_em_oc_ft(l, pslevel_out)
        END DO

        si_start = si(767,8,im_index) + (pslevel_out-1) * npoints_ij
        si_stop  = si_start + npoints_ij - 1
        CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
      END IF
    END DO

  END IF ! sf(767, 8)

  ! ----------------------------------------------------------------------
  ! biomass burning total C2H4 emissions
  ! ----------------------------------------------------------------------
  ! Item 780  biomass burning total C2H4 emissions

  IF ( sf(780, 8) ) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fire_vars)                  &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
    DO j = 1, rows
      DO i = 1, row_length
        interp_data(i, j) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_points
      j = (land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data(i, j) = fire_vars%fire_em_c2h4(l)
    END DO
!$OMP END DO
!$OMP END PARALLEL

    CALL copydiag(stashwork(si(780, 8, im_index):si_last(780, 8, im_index)),   &
          interp_data, row_length, rows)
  END IF ! sf(780, 8)

  ! ----------------------------------------------------------------------
  ! biomass burning total C2H6 emissions
  ! ----------------------------------------------------------------------
  ! Item 781  biomass burning total C2H6 emissions

  IF ( sf(781, 8) ) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fire_vars)                  &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
    DO j = 1, rows
      DO i = 1, row_length
        interp_data(i, j) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_points
      j = (land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data(i, j) = fire_vars%fire_em_c2h6(l)
    END DO
!$OMP END DO
!$OMP END PARALLEL

    CALL copydiag(stashwork(si(781, 8, im_index):si_last(781, 8, im_index)),   &
          interp_data, row_length, rows)
  END IF ! sf(781, 8)

  ! ----------------------------------------------------------------------
  ! biomass burning total C3H8 emissions
  ! ----------------------------------------------------------------------
  ! Item 782  biomass burning total C3H8 emissions

  IF ( sf(782, 8) ) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fire_vars)                  &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
    DO j = 1, rows
      DO i = 1, row_length
        interp_data(i, j) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_points
      j = (land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data(i, j) = fire_vars%fire_em_c3h8(l)
    END DO
!$OMP END DO
!$OMP END PARALLEL

    CALL copydiag(stashwork(si(782, 8, im_index):si_last(782, 8, im_index)),   &
          interp_data, row_length, rows)
  END IF ! sf(782, 8)

  ! ----------------------------------------------------------------------
  ! biomass burning total HCHO emissions
  ! ----------------------------------------------------------------------
  ! Item 783  biomass burning total HCHO emissions

  IF ( sf(783, 8) ) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fire_vars)                  &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
    DO j = 1, rows
      DO i = 1, row_length
        interp_data(i, j) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_points
      j = (land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data(i, j) = fire_vars%fire_em_hcho(l)
    END DO
!$OMP END DO
!$OMP END PARALLEL

    CALL copydiag(stashwork(si(783, 8, im_index):si_last(783, 8, im_index)),   &
          interp_data, row_length, rows)
  END IF ! sf(783, 8)

  ! ----------------------------------------------------------------------
  ! biomass burning total MeCHO emissions
  ! ----------------------------------------------------------------------
  ! Item 784  biomass burning total MeCHO emissions

  IF ( sf(784, 8) ) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fire_vars)                  &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
    DO j = 1, rows
      DO i = 1, row_length
        interp_data(i, j) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_points
      j = (land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data(i, j) = fire_vars%fire_em_mecho(l)
    END DO
!$OMP END DO
!$OMP END PARALLEL

    CALL copydiag(stashwork(si(784, 8, im_index):si_last(784, 8, im_index)),   &
          interp_data, row_length, rows)
  END IF ! sf(784, 8)

  ! ----------------------------------------------------------------------
  ! biomass burning total NH3 emissions
  ! ----------------------------------------------------------------------
  ! Item 785  biomass burning total NH3 emissions

  IF ( sf(785, 8) ) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fire_vars)                  &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
    DO j = 1, rows
      DO i = 1, row_length
        interp_data(i, j) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_points
      j = (land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data(i, j) = fire_vars%fire_em_nh3(l)
    END DO
!$OMP END DO
!$OMP END PARALLEL

    CALL copydiag(stashwork(si(785, 8, im_index):si_last(785, 8, im_index)),   &
          interp_data, row_length, rows)
  END IF ! sf(785, 8)

  ! ----------------------------------------------------------------------
  ! biomass burning total DMS emissions
  ! ----------------------------------------------------------------------
  ! Item 786  biomass burning total DMS emissions

  IF ( sf(786, 8) ) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,l) SHARED(fire_vars)                  &
!$OMP SHARED(rows,row_length,interp_data,land_points,land_index)
!$OMP DO SCHEDULE(STATIC)
    DO j = 1, rows
      DO i = 1, row_length
        interp_data(i, j) = rmdi
      END DO
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_points
      j = (land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      interp_data(i, j) = fire_vars%fire_em_dms(l)
    END DO
!$OMP END DO
!$OMP END PARALLEL

    CALL copydiag(stashwork(si(786, 8, im_index):si_last(786, 8, im_index)),   &
          interp_data, row_length, rows)
  END IF ! sf(786, 8)

  ! --------------------------------------------------------------------
  ! biomass burning emitted C2H4 on PFTs
  ! --------------------------------------------------------------------
  !  Item 790  biomass burning emitted C2H4 on PFTs

  IF ( sf(790, 8) ) THEN
    CALL set_pseudo_list(npft, len_stlist,                                     &
         stlist(1, stindex(1, 790, 8, im_index)),                              &
         pllpft, stash_pseudo_levels, num_stash_pseudo)
    pslevel_out = 0

    DO pslevel = 1, npft
      IF (pllpft(pslevel)) THEN
        pslevel_out = pslevel_out + 1
        DO j = 1, rows
          DO i = 1, row_length
            interp_data(i, j) = rmdi
          END DO
        END DO

        DO l = 1, land_points
          j = (land_index(l) - 1) / row_length + 1
          i = land_index(l) - (j-1) * row_length
          interp_data(i, j) = fire_vars%fire_em_c2h4_ft(l, pslevel_out)
        END DO

        si_start = si(790,8,im_index) + (pslevel_out-1) * npoints_ij
        si_stop  = si_start + npoints_ij - 1
        CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
      END IF
    END DO

  END IF ! sf(790, 8)

  ! --------------------------------------------------------------------
  ! biomass burning emitted C2H6 on PFTs
  ! --------------------------------------------------------------------
  !  Item 791  biomass burning emitted C2H6 on PFTs

  IF ( sf(791, 8) ) THEN
    CALL set_pseudo_list(npft, len_stlist,                                     &
         stlist(1, stindex(1, 791, 8, im_index)),                              &
         pllpft, stash_pseudo_levels, num_stash_pseudo)
    pslevel_out = 0

    DO pslevel = 1, npft
      IF (pllpft(pslevel)) THEN
        pslevel_out = pslevel_out + 1
        DO j = 1, rows
          DO i = 1, row_length
            interp_data(i, j) = rmdi
          END DO
        END DO

        DO l = 1, land_points
          j = (land_index(l) - 1) / row_length + 1
          i = land_index(l) - (j-1) * row_length
          interp_data(i, j) = fire_vars%fire_em_c2h6_ft(l, pslevel_out)
        END DO

        si_start = si(791,8,im_index) + (pslevel_out-1) * npoints_ij
        si_stop  = si_start + npoints_ij - 1
        CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
      END IF
    END DO

  END IF ! sf(791, 8)

  ! --------------------------------------------------------------------
  ! biomass burning emitted C3H8 on PFTs
  ! --------------------------------------------------------------------
  !  Item 792  biomass burning emitted C2H4 on PFTs

  IF ( sf(792, 8) ) THEN
    CALL set_pseudo_list(npft, len_stlist,                                     &
         stlist(1, stindex(1, 792, 8, im_index)),                              &
         pllpft, stash_pseudo_levels, num_stash_pseudo)
    pslevel_out = 0

    DO pslevel = 1, npft
      IF (pllpft(pslevel)) THEN
        pslevel_out = pslevel_out + 1
        DO j = 1, rows
          DO i = 1, row_length
            interp_data(i, j) = rmdi
          END DO
        END DO

        DO l = 1, land_points
          j = (land_index(l) - 1) / row_length + 1
          i = land_index(l) - (j-1) * row_length
          interp_data(i, j) = fire_vars%fire_em_c3h8_ft(l, pslevel_out)
        END DO

        si_start = si(792,8,im_index) + (pslevel_out-1) * npoints_ij
        si_stop  = si_start + npoints_ij - 1
        CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
      END IF
    END DO

  END IF ! sf(792, 8)

  ! --------------------------------------------------------------------
  ! biomass burning emitted HCHO on PFTs
  ! --------------------------------------------------------------------
  !  Item 793  biomass burning emitted HCHO on PFTs

  IF ( sf(793, 8) ) THEN
    CALL set_pseudo_list(npft, len_stlist,                                     &
         stlist(1, stindex(1, 793, 8, im_index)),                              &
         pllpft, stash_pseudo_levels, num_stash_pseudo)
    pslevel_out = 0

    DO pslevel = 1, npft
      IF (pllpft(pslevel)) THEN
        pslevel_out = pslevel_out + 1
        DO j = 1, rows
          DO i = 1, row_length
            interp_data(i, j) = rmdi
          END DO
        END DO

        DO l = 1, land_points
          j = (land_index(l) - 1) / row_length + 1
          i = land_index(l) - (j-1) * row_length
          interp_data(i, j) = fire_vars%fire_em_hcho_ft(l, pslevel_out)
        END DO

        si_start = si(793,8,im_index) + (pslevel_out-1) * npoints_ij
        si_stop  = si_start + npoints_ij - 1
        CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
      END IF
    END DO

  END IF ! sf(793, 8)

  ! --------------------------------------------------------------------
  ! biomass burning emitted MeCHO on PFTs
  ! --------------------------------------------------------------------
  !  Item 794  biomass burning emitted MeCHO on PFTs

  IF ( sf(794, 8) ) THEN
    CALL set_pseudo_list(npft, len_stlist,                                     &
         stlist(1, stindex(1, 794, 8, im_index)),                              &
         pllpft, stash_pseudo_levels, num_stash_pseudo)
    pslevel_out = 0

    DO pslevel = 1, npft
      IF (pllpft(pslevel)) THEN
        pslevel_out = pslevel_out + 1
        DO j = 1, rows
          DO i = 1, row_length
            interp_data(i, j) = rmdi
          END DO
        END DO

        DO l = 1, land_points
          j = (land_index(l) - 1) / row_length + 1
          i = land_index(l) - (j-1) * row_length
          interp_data(i, j) = fire_vars%fire_em_mecho_ft(l, pslevel_out)
        END DO

        si_start = si(794,8,im_index) + (pslevel_out-1) * npoints_ij
        si_stop  = si_start + npoints_ij - 1
        CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
      END IF
    END DO

  END IF ! sf(794, 8)

  ! --------------------------------------------------------------------
  ! biomass burning emitted NH3 on PFTs
  ! --------------------------------------------------------------------
  !  Item 795  biomass burning emitted NH3 on PFTs

  IF ( sf(795, 8) ) THEN
    CALL set_pseudo_list(npft, len_stlist,                                     &
         stlist(1, stindex(1, 795, 8, im_index)),                              &
         pllpft, stash_pseudo_levels, num_stash_pseudo)
    pslevel_out = 0

    DO pslevel = 1, npft
      IF (pllpft(pslevel)) THEN
        pslevel_out = pslevel_out + 1
        DO j = 1, rows
          DO i = 1, row_length
            interp_data(i, j) = rmdi
          END DO
        END DO

        DO l = 1, land_points
          j = (land_index(l) - 1) / row_length + 1
          i = land_index(l) - (j-1) * row_length
          interp_data(i, j) = fire_vars%fire_em_nh3_ft(l, pslevel_out)
        END DO

        si_start = si(795,8,im_index) + (pslevel_out-1) * npoints_ij
        si_stop  = si_start + npoints_ij - 1
        CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
      END IF
    END DO

  END IF ! sf(796, 8)

  ! --------------------------------------------------------------------
  ! biomass burning emitted DMS on PFTs
  ! --------------------------------------------------------------------
  !  Item 796  biomass burning emitted DMS on PFTs

  IF ( sf(796, 8) ) THEN
    CALL set_pseudo_list(npft, len_stlist,                                     &
         stlist(1, stindex(1, 796, 8, im_index)),                              &
         pllpft, stash_pseudo_levels, num_stash_pseudo)
    pslevel_out = 0

    DO pslevel = 1, npft
      IF (pllpft(pslevel)) THEN
        pslevel_out = pslevel_out + 1
        DO j = 1, rows
          DO i = 1, row_length
            interp_data(i, j) = rmdi
          END DO
        END DO

        DO l = 1, land_points
          j = (land_index(l) - 1) / row_length + 1
          i = land_index(l) - (j-1) * row_length
          interp_data(i, j) = fire_vars%fire_em_dms_ft(l, pslevel_out)
        END DO

        si_start = si(796,8,im_index) + (pslevel_out-1) * npoints_ij
        si_stop  = si_start + npoints_ij - 1
        CALL copydiag(STASHwork(si_start:si_stop),interp_data,row_length,rows)
      END IF
    END DO

  END IF ! sf(796, 8)

END IF ! if l_inferno

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE diagnostics_hyd
END MODULE diagnostics_hyd_mod
#endif
