#if !defined(UM_JULES)
MODULE flake_init_mod
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

IMPLICIT NONE

PRIVATE

PUBLIC :: flake_init

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='FLAKE_INIT_MOD'

CONTAINS

!===============================================================================
! Public subroutine
!===============================================================================
SUBROUTINE flake_init(ainfo, progs,                                            &
                          !fluxes, &
                          lake_vars                                            &
                          !forcing, &
                          !rivers, &
                          !veg3_parm, &
                          !veg3_field, &
                          !chemvars, &
                          )

!Module imports

USE ancil_info, ONLY: land_pts, nsoilt

USE conversions_mod, ONLY: pi_over_180

USE water_constants_mod, ONLY: tm

USE jules_soil_mod, ONLY: sm_levels, dzsoil

USE jules_surface_types_mod, ONLY: lake

USE jules_snow_mod, ONLY: nsmax, rho_snow_const

USE lake_mod, ONLY: h_ice_min_flk, lake_h_scale, h_ice_max,                    &
                    lake_h_mxl_0,                                              &
                    lake_h_deltaT, nusselt_0, lake_fetch_0, lake_shape_0,      &
                    g_dt_0

USE model_grid_mod, ONLY: latitude

USE theta_field_sizes,    ONLY: t_i_length

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

USE ancil_info, ONLY: ainfo_type
USE prognostics, ONLY: progs_type
! USE fluxes, ONLY: fluxes_type
USE lake_mod, ONLY: lake_type
! USE forcing, ONLY: forcing_type
! USE jules_rivers_mod, ONLY: rivers_type
! USE veg3_parm_mod, ONLY: in_dev
! USE veg3_field_mod, ONLY: in_dev
! USE jules_chemvars_mod, ONLY: chemvars_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Private subroutine for initialising FLake.
!
! Current Code Owner: See ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
!Arguments
TYPE(ainfo_type), INTENT(IN OUT) :: ainfo
TYPE(progs_type), INTENT(IN OUT) :: progs
!TYPE(fluxes_type), INTENT(IN OUT) :: fluxes
TYPE(lake_type), INTENT(IN OUT) :: lake_vars
!TYPE(forcing_type), INTENT(IN OUT) :: forcing
!TYPE(rivers_type), INTENT(IN OUT) :: rivers
!TYPE(in_dev), INTENT(IN OUT) :: veg3_parm
!TYPE(in_dev), INTENT(IN OUT) :: veg3_field
!TYPE(chemvars_type), INTENT(IN OUT) :: chemvars


! Local variables
REAL                                 :: soil_temp(land_pts, sm_levels)
REAL                                 :: lake_t_mxl(land_pts)
REAL                                 :: lake_t_mean(land_pts)
REAL                                 :: lake_h_ice(land_pts)
REAL                                 :: lake_h_mxl(land_pts)
REAL                                 :: lake_t_ice(land_pts)
REAL                                 :: soil_mean_temp
REAL                                 :: lake_ice_thickness
REAL                                 :: lake_snow_depth

INTEGER                              :: l    ! looper
INTEGER                              :: m    ! looper
INTEGER                              :: i    ! work variable
INTEGER                              :: j    ! work variable

CHARACTER(LEN=*), PARAMETER  :: RoutineName='FLAKE_INIT'

!Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!-----------------------------------------------------------------------------
!End of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!==============================================================================
! *NOTICE REGARDING SOIL TILING**
!
!The following section facilitates the use of soil tiling. As implemented,
!there are two soil tiling options:
!
!nsoilt == 1
!Operate as with a single soil tile, functionally identical to JULES upto
! at least vn4.7 (Oct 2016)
! This means that a soilt variable being passed 'up' to the surface is
! broadcast to the surft variable (with weighting by frac if requred)
!
!nsoilt > 1
!Operate with nsoilt = nsurft, with a direct mapping between them
! This means that a soilt variable being passed 'up' to the surface is simply
! copied into the surft variable
!
! This will need to be refactored for other tiling approaches. This note
! will be replicated elsewhere in the code as required
!
!These comments apply until **END NOTICE REGARDING SOIL TILING**
!==============================================================================

! Convert soil temperature from three dimensions to two
! (There will only be one soil tile in use on the lake surface tile)
IF ( nsoilt == 1 ) THEN
  soil_temp(:,:) = progs%t_soil_soilt(:,1,:)
ELSE
  soil_temp(:,:) = progs%t_soil_soilt(:,lake,:)
END IF

!----------------------------------------------------------------------
! Perform FLake init calculations
!----------------------------------------------------------------------
! Is performed five times in UM recon (once for each FLake variable read from
! dump) but I think only needs to be done once.

DO l = 1, land_pts

  ! calculate mean soil temperature
  soil_mean_temp = 0.0
  DO m = 1, sm_levels
    soil_mean_temp = soil_mean_temp + soil_temp(l,m) * dzsoil(m)
  END DO
  soil_mean_temp= soil_mean_temp / SUM(dzsoil(:))


  ! estimate the snow depth
  ! using snowdepth_surft should work for both zero layer and multilayer snow
  ! *check that snowdepth_surft has already been initialised here.
  lake_snow_depth = progs%snowdepth_surft(land_pts,lake)

  ! 1) empirical estimate of lake ice thickness
  !    based on the soil column temperature

  lake_ice_thickness = 0.0
  IF ( soil_mean_temp <= tm ) THEN
    lake_ice_thickness = SUM( dzsoil(:)) * MIN(1.0,(tm - soil_mean_temp +      &
                         0.05) / lake_h_deltaT)
  ELSE IF ( soil_temp(l,1) <= tm ) THEN
    lake_ice_thickness = dzsoil(1) * MIN(1.0,(tm - soil_temp(l,1) + 0.05)      &
                         / lake_h_deltaT)
  ELSE IF ( progs%tstar_surft(l,lake) <= tm ) THEN
    lake_ice_thickness = 2.0 * h_ice_min_flk
  ELSE
    lake_ice_thickness = 0.0
  END IF

  ! 2) empirical estimate of lake ice thickness
  !    based on the snow depth
  lake_ice_thickness = MAX( lake_ice_thickness,                                &
                        lake_snow_depth / lake_h_scale )

  ! ...and bound by the Mironov & Ritter (2004) maximum
  !    to avoid SQRT(-ve) in FLake_driver
  lake_ice_thickness = MIN( lake_ice_thickness,h_ice_max )

  ! set mixed-layer T based on the temperature of the 1st soil level
  lake_t_mxl(l) = MAX(soil_temp(l,1), tm + 0.10)

  ! set mean T to the mean temperature over the soil column,
  ! BUT constrain to between the mixed-layer temperature and freezing.
  lake_t_mean(l) = MIN(soil_mean_temp, lake_t_mxl(l) - 0.05)
  lake_t_mean(l) = MAX(lake_t_mean(l), tm + 0.05)

  ! initialise the ice thickness
  lake_h_ice(l) = lake_ice_thickness

  ! bound the mixed-layer depth by the available unfrozen depth
  lake_h_mxl(l) = MIN(lake_h_mxl_0,                                            &
                         lake_vars%lake_depth_gb(l) - lake_h_ice(l))
  lake_h_mxl(l) = MAX(lake_h_mxl(l),0.0)

  ! linear interpolation between T* and Tm
  IF ( lake_ice_thickness + lake_snow_depth > EPSILON(1.0) ) THEN
    lake_t_ice(l) = progs%tstar_surft(l,lake)                                  &
                    + ( tm - progs%tstar_surft(l,lake) )                       &
                    * lake_snow_depth / ( lake_ice_thickness + lake_snow_depth )

    ! take the soil temperature instead if it is lower
    lake_t_ice(l) = MIN( lake_t_ice(l), soil_temp(l,1) )
  ELSE
    lake_t_ice(l) = progs%tstar_surft(l,lake)
  END IF

  ! put an upper bound on the ice T
  lake_t_ice(l) = MIN(lake_t_ice(l),tm-0.05)

  !----------------------------------------------------------------------
  ! write the correct data to the output field
  !----------------------------------------------------------------------

  lake_vars%lake_t_mxl_gb(l)  = lake_t_mxl(l)
  lake_vars%lake_t_mean_gb(l) = lake_t_mean(l)
  lake_vars%lake_t_ice_gb(l)  = lake_t_ice(l)
  lake_vars%lake_h_ice_gb(l)  = lake_h_ice(l)
  lake_vars%lake_h_mxl_gb(l)  = lake_h_mxl(l)

END DO ! loop over land_pts

!----------------------------------------------------------------------
! Copy of FLake initialiation stuff in jules_init (used for UM)
!----------------------------------------------------------------------

! initialise the Nusselt number
lake_vars%nusselt_gb(:) = nusselt_0

DO l = 1,land_pts

  j=(ainfo%land_index(l) - 1) / t_i_length + 1
  i = ainfo%land_index(l) - (j-1) * t_i_length

  WRITE(6,*) 'flake_init: l = ', l
  WRITE(6,*) 'flake_init: land_index(l) = ', ainfo%land_index(l)
  WRITE(6,*) 'flake_init: t_i_length = ', t_i_length
  WRITE(6,*) 'flake_init: latitude(i,j) = ', latitude(i,j)

  ! set the Coriolis parameter : ABSOLUTE VALUE
  lake_vars%coriolis_param_gb(l) = ABS( SIN(latitude(i,j) * pi_over_180) )

END DO

!----------------------------------------------------------------------
! Initialise fetch, shape factor and g_dt_0.
! These are constants that are set in rcf_set_data_source in the UM.
!----------------------------------------------------------------------

lake_vars%lake_fetch_gb(:) = lake_fetch_0
lake_vars%lake_shape_factor_gb(:) = lake_shape_0
lake_vars%g_dt_gb(:) = g_dt_0

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE flake_init
END MODULE flake_init_mod
#endif
