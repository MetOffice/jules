!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology, 2017.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE soil_ecosse_vars_mod


USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Module holding variables for the ECOSSE soil model.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Module variables
!-----------------------------------------------------------------------------

PUBLIC  ! Everything is public.

!-----------------------------------------------------------------------------
! Module parameters.
!-----------------------------------------------------------------------------
! Indices that give the location in n_soil_pool_soilt of different species.
! At present there is only one configuration possible, hence these are
! parameters.
INTEGER, PARAMETER ::                                                          &
  i_amm = 5,                                                                   &
    ! Index for ammonium.
  i_nit = 6
    ! Index for nitrate.

!-----------------------------------------------------------------------------
! Implementation for field variables
! Each variable is declared in both the 'data' TYPE and the 'pointer' type.
! Instances of these types are declared at at high level as required
! This is to facilitate advanced memory management features, which are generally
! not visible in the science code.
! Checklist for adding a new variable:
! -add to data_type
! -add to pointer_type
! -add to the allocate routine, passing in any new dimension sizes required
!  by argument (not via USE statement)
! -add to the deallocate routine
! -add to the assoc and nullify routines

!===============================================================================
TYPE :: soil_ecosse_vars_data_type
  ! Contains LOGICALs, INTEGERs and REALs

  !-----------------------------------------------------------------------------
  ! Prognostic variable.
  ! Note that ECOSSE also uses organic C from the prognostics module.
  ! In future this could be used by other models, but currently is only for
  ! ECOSSE.
  !-----------------------------------------------------------------------------
  REAL(KIND=real_jlslsm), ALLOCATABLE ::                                       &
    n_soil_pool_soilt(:,:,:,:)
      ! N in soil pools (kg m-2).

  !-----------------------------------------------------------------------------
  ! Time-averaged driver variables.
  !-----------------------------------------------------------------------------
  REAL(KIND=real_jlslsm), ALLOCATABLE ::                                       &
    deposition_n_driver(:),                                                    &
      ! Atmospheric deposition of N to land surface (kg m-2 s-1).
    qbase_l_driver(:,:,:),                                                     &
      ! Lateral flux of water from each soil layer (kg m-2 s-1).
    sthf_driver(:,:,:),                                                        &
      ! Frozen soil moisture content as a fraction of saturation.
    sthu_driver(:,:,:),                                                        &
      ! Unfrozen soil moisture content as a fraction of saturation.
    tsoil_driver(:,:,:),                                                       &
      ! Soil temperature (K).
    wflux_driver(:,:,:)
      ! Downward water flux at bottom of each soil layer (kg m-2 s-1).

  !-----------------------------------------------------------------------------
  ! Diagnostics.
  !-----------------------------------------------------------------------------
  REAL(KIND=real_jlslsm), ALLOCATABLE ::                                       &
    !---------------------------------------------------------------------------
    ! CO2 fluxes.
    !---------------------------------------------------------------------------
    co2_soil_gb(:),                                                            &
      ! C in CO2 flux from soil to atmosphere (kg m-2 s-1).
    !---------------------------------------------------------------------------
    ! Nitrification.
    !---------------------------------------------------------------------------
    n_nitrification_gb(:),                                                     &
      ! Rate of nitrification, expressed as N (kg m-2 s-1).
    n2o_nitrif_gb(:),                                                          &
      ! N in N2O lost during nitrification, including partial nitrification
      ! (kg m-2 s-1).
    n2o_partial_nitrif_gb(:),                                                  &
      ! N in N2O lost by partial nitrification (kg m-2 s-1).
    !---------------------------------------------------------------------------
    ! Denitrification.
    !---------------------------------------------------------------------------
    n_denitrification_gb(:),                                                   &
      ! Rate of denitrification, expressed as N (kg m-2 s-1).
    n2o_denitrif_gb(:),                                                        &
      ! N in N2O lost during denitrification (kg m-2 s-1).
    n2_denitrif_gb(: ),                                                        &
      ! N in N2 lost from column by denitrification (kg m-2 s-1).
    !---------------------------------------------------------------------------
    ! Other gas fluxes.
    !---------------------------------------------------------------------------
    no_soil_gb(:),                                                             &
      ! N in NO flux from soil to atmosphere (kg m-2 s-1)
    n2o_soil_gb(:),                                                            &
      ! N in N2O flux from soil to atmosphere (kg m-2 s-1)
    !---------------------------------------------------------------------------
    ! Leaching.
    !---------------------------------------------------------------------------
    n_leach_nit_gb(:),                                                         &
      ! N lost from column through leaching of nitrate (kg m-2 s-1).
    n_leach_amm_gb(:),                                                         &
      ! N lost from column through leaching of ammonium (kg m-2 s-1).
    !---------------------------------------------------------------------------
    ! Plant N uptake diagnostics.
    !---------------------------------------------------------------------------
    nh4_uptake_gb(:),                                                          &
      ! N in gridbox uptake of NH4 by plants (kg m-2 s-1)
    nh4_uptake_layer_gb(:,:),                                                  &
      ! Gridbox N in uptake of ammomium from each layer (kg m-2 s-1).
    no3_uptake_gb(:),                                                          &
      ! N in gridbox uptake of NO3 by plants (kg m-2 s-1).
    no3_uptake_layer_gb(:,:),                                                  &
      ! Gridbox N in uptake of nitrate from each layer (kg m-2 s-1).
    !---------------------------------------------------------------------------
    ! Litter input diagnostics.
    !---------------------------------------------------------------------------
    plant_input_c_gb(:),                                                       &
      ! Plant input of carbon to soil by litterfall (kg m-2 s-1).
    plant_input_n_gb(:),                                                       &
      ! Plant input of nitrogen to soil by litterfall (kg m-2 s-1).
    !---------------------------------------------------------------------------
    ! Diagnostics of soil adjustments.
    ! At present these variables could be local to the routines that use them
    ! but they are added here in anticipation of future use as diagnostics.
    !---------------------------------------------------------------------------
    soil_c_add(:),                                                             &
      ! C added to soil during adjustments (kg m-2).
    soil_n_add(:)
      ! N added to soil during adjustments (kg m-2).
END TYPE

!===============================================================================
TYPE :: soil_ecosse_vars_type
  ! Contains LOGICALs, INTEGERs and REALs

  !-----------------------------------------------------------------------------
  ! Prognostic variable.
  ! Note that ECOSSE also uses organic C from the prognostics module.
  ! In future this could be used by other models, but currently is only for
  ! ECOSSE.
  !-----------------------------------------------------------------------------
  REAL(KIND=real_jlslsm), POINTER ::                                           &
    n_soil_pool_soilt(:,:,:,:)
  REAL(KIND=real_jlslsm), POINTER ::                                           &
    deposition_n_driver(:),                                                    &
    qbase_l_driver(:,:,:),                                                     &
    sthf_driver(:,:,:),                                                        &
    sthu_driver(:,:,:),                                                        &
    tsoil_driver(:,:,:),                                                       &
    wflux_driver(:,:,:)
  REAL(KIND=real_jlslsm), POINTER ::                                           &
    co2_soil_gb(:),                                                            &
    n_nitrification_gb(:),                                                     &
    n2o_nitrif_gb(:),                                                          &
    n2o_partial_nitrif_gb(:),                                                  &
    n_denitrification_gb(:),                                                   &
    n2o_denitrif_gb(:),                                                        &
    n2_denitrif_gb(:),                                                         &
    no_soil_gb(:),                                                             &
    n2o_soil_gb(:),                                                            &
    n_leach_nit_gb(:),                                                         &
    n_leach_amm_gb(:),                                                         &
    nh4_uptake_gb(:),                                                          &
    nh4_uptake_layer_gb(:,:),                                                  &
    no3_uptake_gb(:),                                                          &
    no3_uptake_layer_gb(:,:),                                                  &
    plant_input_c_gb(:),                                                       &
    plant_input_n_gb(:),                                                       &
    soil_c_add(:),                                                             &
    soil_n_add(:)
END TYPE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SOIL_ECOSSE_VARS_MOD'

CONTAINS

!#############################################################################

SUBROUTINE soil_ecosse_vars_alloc(land_pts,                                    &
                            nsoilt,dim_cslayer,dim_soil_n_pool,sm_levels,      &
                            soil_bgc_model,soil_model_ecosse,                  &
                            soil_ecosse_vars_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: land_pts,                                               &
                       nsoilt,dim_cslayer,dim_soil_n_pool,sm_levels

INTEGER, INTENT(IN) :: soil_bgc_model,soil_model_ecosse
TYPE(soil_ecosse_vars_data_type), INTENT(IN OUT) :: soil_ecosse_vars_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SOIL_ECOSSE_VARS_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF ( soil_bgc_model == soil_model_ecosse ) THEN
  ! Note that at present we allocate all ECOSSE variables even if this
  ! is a carbon-only run that does not require the nitrogen variables.

  ! ECOSSE prognostics.
  ALLOCATE(soil_ecosse_vars_data%n_soil_pool_soilt(land_pts,nsoilt,            &
          dim_cslayer,dim_soil_n_pool))

  ! ECOSSE time-averaged drivers.
  ALLOCATE(soil_ecosse_vars_data%deposition_n_driver(land_pts))
  ALLOCATE(soil_ecosse_vars_data%qbase_l_driver(land_pts,nsoilt,sm_levels))
  ALLOCATE(soil_ecosse_vars_data%sthf_driver(land_pts,nsoilt,sm_levels))
  ALLOCATE(soil_ecosse_vars_data%sthu_driver(land_pts,nsoilt,sm_levels))
  ALLOCATE(soil_ecosse_vars_data%tsoil_driver(land_pts,nsoilt,sm_levels))
  ALLOCATE(soil_ecosse_vars_data%wflux_driver(land_pts,nsoilt,0:sm_levels))

  ! Diagnostics.
  ALLOCATE(soil_ecosse_vars_data%co2_soil_gb(land_pts))
  ALLOCATE(soil_ecosse_vars_data%n2_denitrif_gb(land_pts))
  ALLOCATE(soil_ecosse_vars_data%n2o_denitrif_gb(land_pts))
  ALLOCATE(soil_ecosse_vars_data%n2o_nitrif_gb(land_pts))
  ALLOCATE(soil_ecosse_vars_data%n2o_partial_nitrif_gb(land_pts))
  ALLOCATE(soil_ecosse_vars_data%n2o_soil_gb(land_pts))
  ALLOCATE(soil_ecosse_vars_data%n_denitrification_gb(land_pts))
  ALLOCATE(soil_ecosse_vars_data%n_leach_amm_gb(land_pts))
  ALLOCATE(soil_ecosse_vars_data%n_leach_nit_gb(land_pts))
  ALLOCATE(soil_ecosse_vars_data%n_nitrification_gb(land_pts))
  ALLOCATE(soil_ecosse_vars_data%nh4_uptake_gb(land_pts))
  ALLOCATE(soil_ecosse_vars_data%nh4_uptake_layer_gb(land_pts,dim_cslayer))
  ALLOCATE(soil_ecosse_vars_data%no3_uptake_gb(land_pts))
  ALLOCATE(soil_ecosse_vars_data%no3_uptake_layer_gb(land_pts,dim_cslayer))
  ALLOCATE(soil_ecosse_vars_data%no_soil_gb(land_pts))
  ALLOCATE(soil_ecosse_vars_data%plant_input_c_gb(land_pts))
  ALLOCATE(soil_ecosse_vars_data%plant_input_n_gb(land_pts))
  ALLOCATE(soil_ecosse_vars_data%soil_c_add(land_pts))
  ALLOCATE(soil_ecosse_vars_data%soil_n_add(land_pts))

  soil_ecosse_vars_data%n_soil_pool_soilt(:,:,:,:) = 0.0
  soil_ecosse_vars_data%deposition_n_driver(:)   = 0.0
  soil_ecosse_vars_data%qbase_l_driver(:,:,:)    = 0.0
  soil_ecosse_vars_data%sthf_driver(:,:,:)       = 0.0
  soil_ecosse_vars_data%sthu_driver(:,:,:)       = 0.0
  soil_ecosse_vars_data%tsoil_driver(:,:,:)      = 0.0
  soil_ecosse_vars_data%wflux_driver(:,:,:)      = 0.0
  soil_ecosse_vars_data%co2_soil_gb(:)           = 0.0
  soil_ecosse_vars_data%n2_denitrif_gb(:)        = 0.0
  soil_ecosse_vars_data%n2o_denitrif_gb(:)       = 0.0
  soil_ecosse_vars_data%n2o_nitrif_gb(:)         = 0.0
  soil_ecosse_vars_data%n2o_partial_nitrif_gb(:) = 0.0
  soil_ecosse_vars_data%n2o_soil_gb(:)           = 0.0
  soil_ecosse_vars_data%n_denitrification_gb(:)  = 0.0
  soil_ecosse_vars_data%n_leach_amm_gb(:)        = 0.0
  soil_ecosse_vars_data%n_leach_nit_gb(:)        = 0.0
  soil_ecosse_vars_data%n_nitrification_gb(:)    = 0.0
  soil_ecosse_vars_data%nh4_uptake_gb(:)         = 0.0
  soil_ecosse_vars_data%nh4_uptake_layer_gb(:,:) = 0.0
  soil_ecosse_vars_data%no3_uptake_gb(:)         = 0.0
  soil_ecosse_vars_data%no3_uptake_layer_gb(:,:) = 0.0
  soil_ecosse_vars_data%no_soil_gb(:)            = 0.0
  soil_ecosse_vars_data%plant_input_c_gb(:)      = 0.0
  soil_ecosse_vars_data%plant_input_n_gb(:)      = 0.0
  soil_ecosse_vars_data%soil_c_add(:)            = 0.0
  soil_ecosse_vars_data%soil_n_add(:)            = 0.0
ELSE
  ! ECOSSE prognostics.
  ALLOCATE(soil_ecosse_vars_data%n_soil_pool_soilt(1,1,1,1))

  ! ECOSSE time-averaged drivers.
  ALLOCATE(soil_ecosse_vars_data%deposition_n_driver(1))
  ALLOCATE(soil_ecosse_vars_data%qbase_l_driver(1,1,1))
  ALLOCATE(soil_ecosse_vars_data%sthf_driver(1,1,1))
  ALLOCATE(soil_ecosse_vars_data%sthu_driver(1,1,1))
  ALLOCATE(soil_ecosse_vars_data%tsoil_driver(1,1,1))
  ALLOCATE(soil_ecosse_vars_data%wflux_driver(1,1,1))

  ! Diagnostics.
  ALLOCATE(soil_ecosse_vars_data%co2_soil_gb(1))
  ALLOCATE(soil_ecosse_vars_data%n2_denitrif_gb(1))
  ALLOCATE(soil_ecosse_vars_data%n2o_denitrif_gb(1))
  ALLOCATE(soil_ecosse_vars_data%n2o_nitrif_gb(1))
  ALLOCATE(soil_ecosse_vars_data%n2o_partial_nitrif_gb(1))
  ALLOCATE(soil_ecosse_vars_data%n2o_soil_gb(1))
  ALLOCATE(soil_ecosse_vars_data%n_denitrification_gb(1))
  ALLOCATE(soil_ecosse_vars_data%n_leach_amm_gb(1))
  ALLOCATE(soil_ecosse_vars_data%n_leach_nit_gb(1))
  ALLOCATE(soil_ecosse_vars_data%n_nitrification_gb(1))
  ALLOCATE(soil_ecosse_vars_data%nh4_uptake_gb(1))
  ALLOCATE(soil_ecosse_vars_data%nh4_uptake_layer_gb(1,1))
  ALLOCATE(soil_ecosse_vars_data%no3_uptake_gb(1))
  ALLOCATE(soil_ecosse_vars_data%no3_uptake_layer_gb(1,1))
  ALLOCATE(soil_ecosse_vars_data%no_soil_gb(1))
  ALLOCATE(soil_ecosse_vars_data%plant_input_c_gb(1))
  ALLOCATE(soil_ecosse_vars_data%plant_input_n_gb(1))
  ALLOCATE(soil_ecosse_vars_data%soil_c_add(1))
  ALLOCATE(soil_ecosse_vars_data%soil_n_add(1))

END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE soil_ecosse_vars_alloc

!===============================================================================
SUBROUTINE soil_ecosse_vars_dealloc(soil_ecosse_vars_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(soil_ecosse_vars_data_type), INTENT(IN OUT) :: soil_ecosse_vars_data

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SOIL_ECOSSE_VARS_DEALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! ECOSSE prognostics.
DEALLOCATE(soil_ecosse_vars_data%n_soil_pool_soilt)
! ECOSSE time-averaged drivers.
DEALLOCATE(soil_ecosse_vars_data%deposition_n_driver)
DEALLOCATE(soil_ecosse_vars_data%qbase_l_driver)
DEALLOCATE(soil_ecosse_vars_data%sthf_driver)
DEALLOCATE(soil_ecosse_vars_data%sthu_driver)
DEALLOCATE(soil_ecosse_vars_data%tsoil_driver)
DEALLOCATE(soil_ecosse_vars_data%wflux_driver)
! Diagnostics.
DEALLOCATE(soil_ecosse_vars_data%co2_soil_gb)
DEALLOCATE(soil_ecosse_vars_data%n2_denitrif_gb)
DEALLOCATE(soil_ecosse_vars_data%n2o_denitrif_gb)
DEALLOCATE(soil_ecosse_vars_data%n2o_nitrif_gb)
DEALLOCATE(soil_ecosse_vars_data%n2o_partial_nitrif_gb)
DEALLOCATE(soil_ecosse_vars_data%n2o_soil_gb)
DEALLOCATE(soil_ecosse_vars_data%n_denitrification_gb)
DEALLOCATE(soil_ecosse_vars_data%n_leach_amm_gb)
DEALLOCATE(soil_ecosse_vars_data%n_leach_nit_gb)
DEALLOCATE(soil_ecosse_vars_data%n_nitrification_gb)
DEALLOCATE(soil_ecosse_vars_data%nh4_uptake_gb)
DEALLOCATE(soil_ecosse_vars_data%nh4_uptake_layer_gb)
DEALLOCATE(soil_ecosse_vars_data%no3_uptake_gb)
DEALLOCATE(soil_ecosse_vars_data%no3_uptake_layer_gb)
DEALLOCATE(soil_ecosse_vars_data%no_soil_gb)
DEALLOCATE(soil_ecosse_vars_data%plant_input_c_gb)
DEALLOCATE(soil_ecosse_vars_data%plant_input_n_gb)
DEALLOCATE(soil_ecosse_vars_data%soil_c_add)
DEALLOCATE(soil_ecosse_vars_data%soil_n_add)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE soil_ecosse_vars_dealloc

!===============================================================================
SUBROUTINE soil_ecosse_vars_assoc(soil_ecosse_vars,soil_ecosse_vars_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(soil_ecosse_vars_data_type), TARGET, INTENT(IN OUT) ::                    &
soil_ecosse_vars_data
! Instance of the data type we are associtating to
TYPE(soil_ecosse_vars_type), INTENT(IN OUT) :: soil_ecosse_vars
  ! Instance of the pointer type we are associating

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SOIL_ECOSSE_VARS_ASSOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL soil_ecosse_vars_nullify(soil_ecosse_vars)

! ECOSSE prognostics.
soil_ecosse_vars%n_soil_pool_soilt => soil_ecosse_vars_data%n_soil_pool_soilt
! ECOSSE time-averaged drivers.
soil_ecosse_vars%deposition_n_driver =>                                        &
        soil_ecosse_vars_data%deposition_n_driver
soil_ecosse_vars%qbase_l_driver => soil_ecosse_vars_data%qbase_l_driver
soil_ecosse_vars%sthf_driver => soil_ecosse_vars_data%sthf_driver
soil_ecosse_vars%sthu_driver => soil_ecosse_vars_data%sthu_driver
soil_ecosse_vars%tsoil_driver => soil_ecosse_vars_data%tsoil_driver
soil_ecosse_vars%wflux_driver => soil_ecosse_vars_data%wflux_driver

! Diagnostics.
soil_ecosse_vars%co2_soil_gb => soil_ecosse_vars_data%co2_soil_gb
soil_ecosse_vars%n2_denitrif_gb => soil_ecosse_vars_data%n2_denitrif_gb
soil_ecosse_vars%n2o_denitrif_gb => soil_ecosse_vars_data%n2o_denitrif_gb
soil_ecosse_vars%n2o_nitrif_gb => soil_ecosse_vars_data%n2o_nitrif_gb
soil_ecosse_vars%n2o_partial_nitrif_gb =>                                      &
        soil_ecosse_vars_data%n2o_partial_nitrif_gb
soil_ecosse_vars%n2o_soil_gb => soil_ecosse_vars_data%n2o_soil_gb
soil_ecosse_vars%n_denitrification_gb =>                                       &
        soil_ecosse_vars_data%n_denitrification_gb
soil_ecosse_vars%n_leach_amm_gb => soil_ecosse_vars_data%n_leach_amm_gb
soil_ecosse_vars%n_leach_nit_gb => soil_ecosse_vars_data%n_leach_nit_gb
soil_ecosse_vars%n_nitrification_gb => soil_ecosse_vars_data%n_nitrification_gb
soil_ecosse_vars%nh4_uptake_gb => soil_ecosse_vars_data%nh4_uptake_gb
soil_ecosse_vars%nh4_uptake_layer_gb =>                                        &
        soil_ecosse_vars_data%nh4_uptake_layer_gb
soil_ecosse_vars%no3_uptake_gb => soil_ecosse_vars_data%no3_uptake_gb
soil_ecosse_vars%no3_uptake_layer_gb =>                                        &
        soil_ecosse_vars_data%no3_uptake_layer_gb
soil_ecosse_vars%no_soil_gb => soil_ecosse_vars_data%no_soil_gb
soil_ecosse_vars%plant_input_c_gb => soil_ecosse_vars_data%plant_input_c_gb
soil_ecosse_vars%plant_input_n_gb => soil_ecosse_vars_data%plant_input_n_gb
soil_ecosse_vars%soil_c_add => soil_ecosse_vars_data%soil_c_add
soil_ecosse_vars%soil_n_add => soil_ecosse_vars_data%soil_n_add

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE soil_ecosse_vars_assoc

!===============================================================================
SUBROUTINE soil_ecosse_vars_nullify(soil_ecosse_vars)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(soil_ecosse_vars_type), INTENT(IN OUT) :: soil_ecosse_vars
  ! Instance of the pointer type we are associating

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SOIL_ECOSSE_VARS_NULLIFY'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! ECOSSE prognostics.
NULLIFY(soil_ecosse_vars%n_soil_pool_soilt)
! ECOSSE time-averaged drivers.
NULLIFY(soil_ecosse_vars%deposition_n_driver)
NULLIFY(soil_ecosse_vars%qbase_l_driver)
NULLIFY(soil_ecosse_vars%sthf_driver)
NULLIFY(soil_ecosse_vars%sthu_driver)
NULLIFY(soil_ecosse_vars%tsoil_driver)
NULLIFY(soil_ecosse_vars%wflux_driver)
! Diagnostics.
NULLIFY(soil_ecosse_vars%co2_soil_gb)
NULLIFY(soil_ecosse_vars%n2_denitrif_gb)
NULLIFY(soil_ecosse_vars%n2o_denitrif_gb)
NULLIFY(soil_ecosse_vars%n2o_nitrif_gb)
NULLIFY(soil_ecosse_vars%n2o_partial_nitrif_gb)
NULLIFY(soil_ecosse_vars%n2o_soil_gb)
NULLIFY(soil_ecosse_vars%n_denitrification_gb)
NULLIFY(soil_ecosse_vars%n_leach_amm_gb)
NULLIFY(soil_ecosse_vars%n_leach_nit_gb)
NULLIFY(soil_ecosse_vars%n_nitrification_gb)
NULLIFY(soil_ecosse_vars%nh4_uptake_gb)
NULLIFY(soil_ecosse_vars%nh4_uptake_layer_gb)
NULLIFY(soil_ecosse_vars%no3_uptake_gb)
NULLIFY(soil_ecosse_vars%no3_uptake_layer_gb)
NULLIFY(soil_ecosse_vars%no_soil_gb)
NULLIFY(soil_ecosse_vars%plant_input_c_gb)
NULLIFY(soil_ecosse_vars%plant_input_n_gb)
NULLIFY(soil_ecosse_vars%soil_c_add)
NULLIFY(soil_ecosse_vars%soil_n_add)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE soil_ecosse_vars_nullify

!-------------------------------------------------------------------------------

END MODULE soil_ecosse_vars_mod
