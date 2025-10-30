#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE ecosse_source_sink_mod

!-----------------------------------------------------------------------------
! Description:
!   Deals with sources (e.g. deposition) and sinks (e.g. plant uptake) of
!   soil C and N.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGEOCHEMISTRY
!
! Code Description:
!   Language: Fortran 90.
!-----------------------------------------------------------------------------

USE ancil_info, ONLY:                                                          &
  ! imported scalars
  nz_soilc => dim_cslayer

USE ecosse_utils_mod, ONLY:                                                    &
  ! imported parameters
  nt

USE jules_soil_ecosse_mod, ONLY:                                               &
  ! imported scalars
  l_soil_N

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

PRIVATE
PUBLIC ecosse_source_sink

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName = 'ECOSSE_SOURCE_SINK_MOD'

CONTAINS

!#############################################################################
!#############################################################################

SUBROUTINE ecosse_source_sink( land_pts, vs_pts, dt_input, l_add_plant_inputs, &
                         l_extract_n, vs_index,                                &
                         plant_input_c_dpm, plant_input_c_rpm,                 &
                         plant_input_n_dpm, plant_input_n_rpm, residual_n,     &
                         c_dpm, c_rpm, n_amm, n_dpm, n_nit, n_rpm,             &
                         n_fertiliser_add, n_fix_add, n_uptake_extract,        &
                         !TYPES
                         soilecosse)
! Description:
!   Deals with sources (e.g. deposition) and sinks (e.g. plant uptake) of
!   soil C and N.

USE ancil_info, ONLY:                                                          &
  ! imported scalars
  nsoilt

USE ereport_mod, ONLY:                                                         &
  ! imported procedures
  ereport

USE jules_plant_n_uptake_mod, ONLY:                                            &
  ! imported scalar parameters
  n_model_triffid,                                                             &
  ! imported scalars
  n_uptake_model

USE jules_surface_types_mod, ONLY:                                             &
  ! imported scalars
  npft, ntype

USE soil_ecosse_vars_mod, ONLY: soil_ecosse_vars_type


IMPLICIT NONE

!Type Arguments
TYPE(soil_ecosse_vars_type), INTENT(IN OUT) :: soilecosse

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! The number of land points.
  vs_pts
    ! The number of points with veg and/or soil.

REAL, INTENT(IN) ::                                                            &
  dt_input,                                                                    &
    ! Timestep length for litter input and N uptake (s).
  n_fertiliser_add(:,:,:),                                                     &
    ! Nitrogen added to each soil layer by fertiliser (kg m-2).
  n_fix_add(:,:,:),                                                            &
    ! Nitrogen added to each soil layer by fixation (kg m-2).
  n_uptake_extract(:,:,:)
    ! Uptake of N over the veg model timestep (kg m-2).

LOGICAL, INTENT(IN) ::                                                         &
  l_add_plant_inputs,                                                          &
    ! Flag indicating if plant inputs are to be added this timestep.
  l_extract_n
    ! Flag indicating if plant uptake of N is removed from soil this
    ! timestep. Also used for addition of fixation with TRIFFID-based model.

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  vs_index(land_pts)
    ! Indices of veg/soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  plant_input_c_dpm(land_pts,nz_soilc),                                        &
    ! Carbon added to DPM pool by plant inputs (kg m-2).
  plant_input_c_rpm(land_pts,nz_soilc),                                        &
    ! Carbon added to RPM pool by plant inputs (kg m-2).
  plant_input_n_dpm(land_pts,nz_soilc),                                        &
    ! Nitrogen added to DPM pool by plant inputs (kg m-2).
  plant_input_n_rpm(land_pts,nz_soilc),                                        &
    ! Nitrogen added to RPM pool by plant inputs (kg m-2).
  residual_n(land_pts,nz_soilc)
    ! Minimum-allowed (residual) inorganic N amount (kg m-2).

!-----------------------------------------------------------------------------
! Array arguments with intent(inout).
! Note these are dimensioned with nt but at present only a single time level
! is required. All nt levels are provided for future use.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  c_dpm(land_pts,nz_soilc,nt),                                                 &
    ! C in decomposable plant material (kg m-2).
  c_rpm(land_pts,nz_soilc,nt),                                                 &
    ! C in resistant plant material (kg m-2).
  n_amm(land_pts,nz_soilc,nt),                                                 &
    ! N in ammonium (kg m-2).
  n_dpm(land_pts,nz_soilc,nt),                                                 &
    ! N in decomposable plant material (kg m-2).
  n_nit(land_pts,nz_soilc,nt),                                                 &
    ! N in nitrate (kg m-2).
  n_rpm(land_pts,nz_soilc,nt)
    ! N in resistant plant material (kg m-2).

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
INTEGER, PARAMETER :: s = 1
    ! Soil tile number. ECOSSE is coded assuming a single soil tile. Extension
    ! to multiple soil tiles would require at least a loop over the tiles and
    ! plant fluxes (e.g. litterfall input, N uptake) consistent with the
    ! tiling.

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'ECOSSE_SOURCE_SINK'

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  errorstatus
    ! Value used for error.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! ECOSSE is coded assuming a single soil tile.
!-----------------------------------------------------------------------------
IF ( nsoilt > 1 ) THEN
  errorstatus = 101
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
                "ECOSSE is only coded for a single soil tile." )
END IF

!-----------------------------------------------------------------------------
! Remove plant uptake of N.
!-----------------------------------------------------------------------------
IF ( l_extract_n ) THEN
  CALL remove_plant_uptake( land_pts, vs_pts,                                  &
                              dt_input, vs_index, n_uptake_extract(:,s,:),     &
                              residual_n,                                      &
                              n_amm(:,:,nt), n_nit(:,:,nt),                    &
                              !TYPES
                              soilecosse)
END IF

!-----------------------------------------------------------------------------
! Add fixed N to soil, if fixation was calculated by TRIFFID.
! Because both uptake and fixation were calculated by TRIFFID, we can reuse
! the l_extract_n_flag.
!-----------------------------------------------------------------------------
IF ( n_uptake_model == n_model_triffid .AND. l_extract_n ) THEN
  CALL add_n_fix( land_pts, vs_pts, vs_index, n_fix_add(:,s,:),                &
                  n_amm(:,:,nt) )
END IF

!-----------------------------------------------------------------------------
! Add fertiliser N to soil, if it was calculated by TRIFFID.
! Because both uptake and fertilisaer were calculated by TRIFFID, we can reuse
! the l_extract_n_flag.
!-----------------------------------------------------------------------------
IF ( n_uptake_model == n_model_triffid .AND. l_extract_n ) THEN
  CALL add_fert( land_pts, vs_pts, vs_index, n_fertiliser_add(:,s,:),          &
                   n_amm(:,:,nt), n_nit(:,:,nt) )
END IF

!-----------------------------------------------------------------------------
! Add inputs from atmospheric deposition.
!-----------------------------------------------------------------------------
IF ( l_soil_N ) THEN
  CALL add_deposition( land_pts, vs_pts, vs_index,                             &
                       soilecosse%deposition_n_driver,                         &
                       n_amm(:,:,nt), n_nit(:,:,nt) )
END IF

!-----------------------------------------------------------------------------
! Add plant litterfall inputs when required.
!-----------------------------------------------------------------------------
IF ( l_add_plant_inputs ) THEN

  CALL add_plant_inputs( land_pts, vs_pts, vs_index,                           &
                         plant_input_c_dpm, plant_input_c_rpm,                 &
                         plant_input_n_dpm, plant_input_n_rpm,                 &
                         c_dpm(:,:,nt), c_rpm(:,:,nt),                         &
                         n_dpm(:,:,nt), n_rpm(:,:,nt) )

  ! If nt=1, our copy of the prognostic variables can potentially now
  ! include values <= 0 (if the litter flux into a small pool was < 0). This
  ! would likely need to be dealt with here, unless the rest of the code can
  ! deal with these values.
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE ecosse_source_sink

!#############################################################################
!#############################################################################

SUBROUTINE remove_plant_uptake( land_pts, vs_pts,                              &
                     dt_input, vs_index, n_uptake_extract, residual_n,         &
                     n_amm, n_nit,                                             &
                     !TYPES
                     soilecosse)

! Control-level code to remove N taken up by plants from the soil stores.

USE ancil_info, ONLY:                                                          &
  ! imported scalars
  nz_soilc => dim_cslayer

USE jules_plant_n_uptake_mod, ONLY:                                            &
  ! imported scalar parameters
  n_model_triffid,                                                             &
  ! imported scalars
  n_uptake_model

USE soil_ecosse_vars_mod, ONLY: soil_ecosse_vars_type

IMPLICIT NONE

! Description:
!   Control-level code controlling the removal of Nitrogen from soil stores to
!   match uptake by plants.

!Type Arguments
TYPE(soil_ecosse_vars_type), INTENT(IN OUT) :: soilecosse

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! The number of land points.
  vs_pts
    !  The number of points with veg and/or soil.

REAL, INTENT(IN) ::                                                            &
  dt_input
    ! Timestep length for N uptake (s).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN)  :: vs_index(land_pts)
       !  Indices of veg/soil points.

REAL, INTENT(IN) ::                                                            &
  n_uptake_extract(land_pts,nz_soilc),                                         &
    ! Uptake of N over the veg model timestep (kg m-2).
  residual_n(land_pts,nz_soilc)
    ! Minimum allowed nitrate amount in layer (kg m-2).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
REAL, INTENT(IN OUT) ::                                                        &
  n_amm(land_pts,nz_soilc),                                                    &
   !  Soil ammonium-N in a soil layer (kg m-2).
 n_nit(land_pts,nz_soilc)
   !  Soil nitrate-N in a soil layer (kg m-2).

!-----------------------------------------------------------------------------
! Local parameters and variables.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'REMOVE_PLANT_UPTAKE'

INTEGER :: i,l      !  work

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Plant uptake of N should be extracted on this timestep.
! The calculations required potentially vary according to which plant model
! has been used and hence what information is available.
! At present only TRIFFID-based Nuptake is possible.
!-----------------------------------------------------------------------------
SELECT CASE ( n_uptake_model )

CASE ( n_model_triffid )
  ! We know total N uptake for each layer.
  CALL plant_uptake_layer( land_pts, vs_pts,                                   &
                   dt_input, vs_index, n_uptake_extract, residual_n,           &
                   n_amm, n_nit,                                               &
                 ! TYPES
                  soilecosse)
END SELECT

!-----------------------------------------------------------------------------
! Calculate column-total diagnostics.
!-----------------------------------------------------------------------------
soilecosse%no3_uptake_gb(:) = 0.0
soilecosse%nh4_uptake_gb(:) = 0.0
DO i = 1,vs_pts
  l = vs_index(i)
  soilecosse%nh4_uptake_gb(l) = SUM( soilecosse%nh4_uptake_layer_gb(l,:) )
  soilecosse%no3_uptake_gb(l) = SUM( soilecosse%no3_uptake_layer_gb(l,:) )
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE remove_plant_uptake

!#############################################################################
!#############################################################################

SUBROUTINE plant_uptake_layer( land_pts, vs_pts,                               &
                     dt_input, vs_index, n_uptake_extract, residual_n,         &
                     n_amm, n_nit,                                             &
                   ! TYPES
                     soilecosse)
USE ancil_info, ONLY:                                                          &
  ! imported scalars
  nz_soilc => dim_cslayer

USE soil_ecosse_vars_mod, ONLY: soil_ecosse_vars_type

IMPLICIT NONE

! Description:
!   Remove Nitrogen from soil stores to match uptake by plants. This routine
!   was orignally designed for use with ECOSSE (2 inorganic soil N pools) with
!   TRIFFID-based calculation of N uptake (total uptake per soil column), but
!   in principle could be reused with other models.
!
!   Ammonium is used preferentially, before nitrate.

!Type Arguments
TYPE(soil_ecosse_vars_type), INTENT(IN OUT) :: soilecosse

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! The number of land points.
  vs_pts
    !  The number of points with veg and/or soil.

REAL, INTENT(IN) ::                                                            &
  dt_input
    ! Timestep length for N uptake (s).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN)  :: vs_index(land_pts)
    !  Indices of veg/soil points.

REAL, INTENT(IN) ::                                                            &
  n_uptake_extract(land_pts,nz_soilc),                                         &
    ! Uptake of N over the veg model timestep (kg m-2).
  residual_n(land_pts,nz_soilc)
    ! Minimum allowed nitrate amount in layer (kg m-2).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
REAL, INTENT(IN OUT) ::                                                        &
  n_amm(land_pts,nz_soilc),                                                    &
     ! Soil ammonium-N in a soil layer (kg m-2).
  n_nit(land_pts,nz_soilc)
     ! Soil nitrate-N in a soil layer (kg m-2).

!-----------------------------------------------------------------------------
! Local scalar parameters.
!-----------------------------------------------------------------------------
REAL, PARAMETER ::                                                             &
  negligible_mass = 1.0e-20
    ! A negligibly small amount of N (kg m-2), beneath which various
    ! calculations proceed differently.

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'PLANT_UPTAKE_LAYER'

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: i, iz, l, p  ! Indices.

REAL ::                                                                        &
  avail_nh4,                                                                   &
    ! Plant-available N in ammonium (kg m-2).
  avail_no3,                                                                   &
    ! Plant-available N in nitrate (kg m-2).
  dN,                                                                          &
    ! An increment of soil N (kg m-2).
  dNH4,                                                                        &
    ! A decrement of soil ammonium (kg m-2).
  dNO3,                                                                        &
    ! A decrement of soil nitrate (kg m-2).
  total_n,                                                                     &
    ! Total available N in a layer (kg m-2).
  wt_nh4,                                                                      &
    ! Fraction of uptake that is from ammonium.
  wt_no3
    ! Fraction of uptake that is from soil nitrate.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Initialise diagnostics.
!-----------------------------------------------------------------------------
soilecosse%nh4_uptake_layer_gb(:,:) = 0.0
soilecosse%no3_uptake_layer_gb(:,:) = 0.0

!-----------------------------------------------------------------------------
! Update soil stores.
!-----------------------------------------------------------------------------
DO i = 1,vs_pts
  l = vs_index(i)

  DO iz = 1,nz_soilc

    !-------------------------------------------------------------------------
    ! Get available N in this layer.
    ! This is only used to partition the total uptake between the soil N
    ! pools. As such we can define the available N and the weights however we
    ! want as that only changes the partition, not the total. However for full
    ! consistency with the N uptake model we might choose to follow any
    ! assumptions made there.
    !-------------------------------------------------------------------------
    avail_no3 = n_nit(l,iz) - residual_n(l,iz)
    avail_nh4 = n_amm(l,iz) - residual_n(l,iz)

    ! Get fraction to be extracted from each pool.
    total_n = avail_no3 + avail_nh4
    IF ( total_n > negligible_mass ) THEN
      wt_nh4 = avail_nh4 / total_n
      wt_no3 = avail_no3 / total_n
    ELSE
      ! There is negligible available N in the column.
      ! Use equal weights for each species.
      wt_nh4 = 0.5
      wt_no3 = 0.5
    END IF

    ! Get amount to be extracted from each pool.
    dNH4 = n_uptake_extract(l,iz) * wt_nh4
    dNO3 = n_uptake_extract(l,iz) * wt_no3

    ! Reduce uptakes if store would be exhausted.
    IF ( dNH4 > avail_nh4 ) THEN
      dN   = dNH4 - avail_nh4
      dNH4 = avail_nh4
      ! Try to get the remaining N from nitrate.
      dNO3 = dNO3 + dN
    END IF
    IF ( dNO3 > avail_no3 ) THEN
      dN   = dNO3 - avail_no3
      dNO3 = avail_no3
      IF ( dN > negligible_mass ) THEN
        ! Deal with the fact that uptake is greater than the available N.
        ! For now we just record this fact and magic up some N.
        ! Alternatively we could look to access other layers, then look for
        ! global conservation.
        soilecosse%soil_n_add(l) = soilecosse%soil_n_add(l) + dN
      END IF
    END IF

    ! Update stores.
    ! Note that if total uptake is <0 this is augmenting the store.
    n_amm(l,iz) = n_amm(l,iz) - dNH4
    n_nit(l,iz) = n_nit(l,iz) - dNO3

    ! Convert mass of uptake to a rate over the uptake timestep.
    soilecosse%nh4_uptake_layer_gb(l,iz) = dNH4 / dt_input
    soilecosse%no3_uptake_layer_gb(l,iz) = dNO3 / dt_input

  END DO  !  layers

END DO  !  points

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE plant_uptake_layer

!#############################################################################
!#############################################################################

SUBROUTINE add_n_fix( land_pts, vs_pts, vs_index, n_fix_add,                   &
                      n_amm )

IMPLICIT NONE

! Description:
!   Add N that was fixed by plants to the soil.

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! The number of land points.
  vs_pts
    ! The number of points with veg and/or soil.

!-----------------------------------------------------------------------------
! Array arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  vs_index(land_pts)
    ! Indices of veg/soil points.

REAL, INTENT(IN) ::                                                            &
  n_fix_add(land_pts,nz_soilc)
    ! Nitrogen added to each soil layer by fixation (kg m-2).

!-----------------------------------------------------------------------------
! Array arguments with intent(inout).
!-----------------------------------------------------------------------------
REAL, INTENT(IN OUT) ::                                                        &
  n_amm(land_pts,nz_soilc)
    ! Soil ammonium-N in a soil layer (kg m-2).

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'ADD_N_FIX'

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: i, l  !  Indices.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DO i = 1,vs_pts
  l = vs_index(i)
  n_amm(l,:) = n_amm(l,:) + n_fix_add(l,:)
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE add_n_fix

!#############################################################################
!#############################################################################

SUBROUTINE add_fert( land_pts, vs_pts, vs_index, n_fertiliser_add,             &
                       n_amm, n_nit )

USE jules_soil_ecosse_mod, ONLY:                                               &
  ! imported scalars
  depo_nit_frac

IMPLICIT NONE

! Description:
!   Add N that was added via fertiliser.

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! The number of land points.
  vs_pts
    ! The number of points with veg and/or soil.

!-----------------------------------------------------------------------------
! Array arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  vs_index(land_pts)
    ! Indices of veg/soil points.

REAL, INTENT(IN) ::                                                            &
  n_fertiliser_add(land_pts,nz_soilc)
    ! Nitrogen added to each soil layer by fertiliser (kg m-2).

!-----------------------------------------------------------------------------
! Array arguments with intent(inout).
!-----------------------------------------------------------------------------
REAL, INTENT(IN OUT) ::                                                        &
  n_amm(land_pts,nz_soilc),                                                    &
    ! Soil ammonium-N in a soil layer (kg m-2).
  n_nit(land_pts,nz_soilc)
    ! Soil nitrate-N in a soil layer (kg m-2).

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'ADD_FERT'

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: i, l  !  Indices.

REAL ::                                                                        &
  amm_fac, nit_fac  ! Work.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Resuse depo_nit_frac to partition between species. This is a gross misuse
! of that variable, but will suffice until something better is coded!
amm_fac = ( 1.0 - depo_nit_frac )
nit_fac = depo_nit_frac

DO i = 1,vs_pts
  l = vs_index(i)
  n_amm(l,:) = n_amm(l,:) + n_fertiliser_add(l,:) * amm_fac
  n_nit(l,:) = n_nit(l,:) + n_fertiliser_add(l,:) * nit_fac
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE add_fert

!#############################################################################
!#############################################################################

SUBROUTINE add_deposition( land_pts, vs_pts, vs_index, deposition_n_driver,    &
                           n_amm_sfc, n_nit_sfc )

USE jules_soil_ecosse_mod, ONLY:                                               &
  ! imported scalars
  depo_nit_frac, dt_soilc

IMPLICIT NONE

! Description:
!   Adds atmospheric deposition of Nitrogen to topmost soil nitrate and/or
!   ammonium stores.

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! The number of land points.
   vs_pts
    ! The number of points with veg and/or soil.

!-----------------------------------------------------------------------------
! Array arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN)  :: vs_index(land_pts)
    ! Indices of veg/soil points.

REAL, INTENT(IN) :: deposition_n_driver(land_pts)
    ! Atmospheric deposition of N to land surface (kg m-2 s-1).

!-----------------------------------------------------------------------------
! Array arguments with intent(inout).
!-----------------------------------------------------------------------------
REAL, INTENT(IN OUT) ::                                                        &
  n_amm_sfc(land_pts),                                                         &
    ! Soil ammonium N in surface layer (kg m-2).
  n_nit_sfc(land_pts)
    ! Soil nitrate N in surface layer (kg m-2).

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'ADD_DEPOSITION'

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: i,l   ! Work.

REAL ::                                                                        &
  amm_fac, nit_fac  ! Work.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle
!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

amm_fac = ( 1.0 - depo_nit_frac ) * dt_soilc
nit_fac = depo_nit_frac * dt_soilc

DO i = 1,vs_pts
  l = vs_index(i)
  n_amm_sfc(l) = n_amm_sfc(l) + deposition_n_driver(l) * amm_fac
  n_nit_sfc(l) = n_nit_sfc(l) + deposition_n_driver(l) * nit_fac
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE add_deposition

!#############################################################################
!#############################################################################

SUBROUTINE add_plant_inputs( land_pts, vs_pts,vs_index,                        &
               plant_input_c_dpm, plant_input_c_rpm,                           &
               plant_input_n_dpm, plant_input_n_rpm,                           &
               c_dpm, c_rpm, n_dpm, n_rpm )

IMPLICIT NONE

! Description:
!   Adds plant inputs of organic matter to soil.
!   Note that this subroutine can, at least in theory, result in a soil store
!   <0 if a store is depleted because the veg model has calculated
!   "litterfall"<0. That will be dealt with by future developments.

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! The number of land points.
  vs_pts
    ! The number of points with veg and/or soil.
    ! The number of land points.

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN)  :: vs_index(land_pts)
       ! Indices of veg/soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  plant_input_c_dpm(land_pts,nz_soilc),                                        &
    ! Carbon added to DPM pool by plant inputs (kg m-2).
  plant_input_c_rpm(land_pts,nz_soilc),                                        &
    ! Carbon added to RPM pool by plant inputs (kg m-2).
  plant_input_n_dpm(land_pts,nz_soilc),                                        &
    ! Nitrogen added to DPM pool by plant inputs (kg m-2).
  plant_input_n_rpm(land_pts,nz_soilc)
    ! Nitrogen added to RPM pool by plant inputs (kg m-2).

!-----------------------------------------------------------------------------
! Array arguments with intent(inout).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  c_dpm(land_pts,nz_soilc),                                                    &
    ! C in DPM (kg m-2).
  c_rpm(land_pts,nz_soilc),                                                    &
    ! C in RPM (kg m-2).
  n_dpm(land_pts,nz_soilc),                                                    &
    ! N in DPM (kg m-2).
  n_rpm(land_pts,nz_soilc)
    ! N in RPM (kg m-2).

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'ADD_PLANT_INPUTS'

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER :: i, j   ! work

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Add plant material to soil pools.
!-----------------------------------------------------------------------------
! Carbon.
DO j = 1,vs_pts
  i = vs_index(j)
  c_dpm(i,:) = c_dpm(i,:) + plant_input_c_dpm(i,:)
  c_rpm(i,:) = c_rpm(i,:) + plant_input_c_rpm(i,:)
END DO

! Nitrogen.
IF ( l_soil_N ) THEN
  DO j = 1,vs_pts
    i = vs_index(j)
    n_dpm(i,:) = n_dpm(i,:) + plant_input_n_dpm(i,:)
    n_rpm(i,:) = n_rpm(i,:) + plant_input_n_rpm(i,:)
  END DO
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE add_plant_inputs

!#############################################################################
!#############################################################################

END MODULE ecosse_source_sink_mod
#endif
