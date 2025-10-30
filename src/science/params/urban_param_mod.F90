! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
! This file belongs in section: Land

MODULE urban_param_mod

USE um_types, ONLY: real_jlslsm
USE missing_data_mod, ONLY: rmdi

IMPLICIT NONE

!===============================================================================

! Urban anthropogenic heat source taken from the digest of UK energy
! statistics (1995 - 2003), monthly averaged over 9 years, converted
! to W/m2 and adjusted to fraction dissipated in urban areas.
! Used in anthrop_heat_option == 0 (dukes)

REAL(KIND=real_jlslsm), PARAMETER ::                                           &
   urban_month(12) = [ 25.1447, 23.4529, 24.6175, 20.7802, 18.8677, 18.1567,   &
                       17.1881, 16.6732, 18.5490, 20.7435, 22.9870, 26.1500 ]
                     ! Monthly values of anthropogenic
                     ! contribution to surface heat fluxes W/m2

!===============================================================================

! Parameters associated with the urban anthropogenic heat algorithms
! from Flanner 2009 Geophysical Research Letters 36: L02801. Table A1.
! Used in anthrop_heat_option == 1 (flanner)

! Diurnal cycle parameters
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
   fl_b1    = 0.451,     & ! Daytime amplification
   fl_b2    = 0.8,       & ! Daytime amplification
   fl_sigma = 0.18,      & ! Slope of evening tapering
   fl_mu    = 0.5,       & ! Timing of morning and evening ramps
   fl_a1    = -0.3,      & ! Amplitude of morning and evening traffic maxima
   fl_f     = 2.0,       & ! Timing of morning and evening traffic maxima
   fl_alpha = 10.0,      & ! Slope of morning increase
   fl_eps   = 0.25,      & ! Timing of morning and evening ramps
   fl_norm  = 1.21235      ! Normalisation term calculated as integral of
                           ! diurnal weighting over 24 hours, used as a
                           ! normalisation to ensure the diurnal weighting
                           ! means to 1. This term would need to be
                           ! recalculated if any of the above fl_ parameter
                           ! values are modified.

! Seasonal cycle parameters
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
   fl_latlim = 33.0,     & ! Only apply seasonality to latitudes further from
                           ! the equator than this latitude value
   fl_n      = 25.0,     & ! Seasonal amplification term
   fl_off    = 0.25        ! Offset applied to day of year. 0.25 gives
                           ! peak/trough on day 0 in NH and SH respectively

!===============================================================================

! Parameters for the MacDonald et al. (2008) formulation of displacement height
! and effective roughness length for momentum
REAL(KIND=real_jlslsm), PARAMETER :: a       = 4.43
REAL(KIND=real_jlslsm), PARAMETER :: cdz     = 1.2     ! Drag coefficient
REAL(KIND=real_jlslsm), PARAMETER :: kappa2  = 0.16    ! Von Karman constant**2
REAL(KIND=real_jlslsm), PARAMETER :: z0m_mat = 0.05
                                     ! Material roughness length for momentum

! Note: z0m_mat has a value of 0.005 in original UM version, but 0.05 in
! original JULES version. z0m_mat = 0.05 was used in inter-comparison for VL92

REAL(KIND=real_jlslsm), PARAMETER ::                                           &
   emiss       = 1.0,           & ! Emissivity sky
   omega_day   = 7.272e-5         ! Angular velocity of the Earth
                                  ! wrt sun( s-1 )

! At the moment set urban materials here. Could be re-written to be read from
! a look up table for different fabrics. If this was done these would need to
! be made into arrays(land_points) to store the values here and the code
! re-written to enable this.

REAL(KIND=real_jlslsm), PARAMETER ::                                           &
   ! Road material = asphalt
   diffus_road  = 0.38e-6,     & ! Road: Thermal diffusivity (m2 s-1)
   cap_road     = 1.94e6,      & ! Road: Volumetric heat capacity (J K-1 m-3)

   ! Wall material = brick
   diffus_wall = 0.61e-6,      & ! Wall: Thermal diffusivity (m2 s-1)
   cap_wall    = 1.37e6,       & ! Wall: Volumetric heat capacity (J K-1 m-3)

   ! Roof material = clay
   diffus_roof = 0.47e-6,      & ! Roof: Thermal diffusivity (m2 s-1)
   cap_roof    = 1.77e6,       & ! Roof: Volumetric heat capacity (J K-1 m-3)
   dz_roof_p   = 0.02            ! Physical depth of roof as opposed to

TYPE :: urban_param_data_type
  REAL(KIND=real_jlslsm), ALLOCATABLE :: hgt_gb(:)   ! Building height
  REAL(KIND=real_jlslsm), ALLOCATABLE :: hwr_gb(:)   ! Height to width ratio
  REAL(KIND=real_jlslsm), ALLOCATABLE :: wrr_gb(:)   ! Width ratio
  REAL(KIND=real_jlslsm), ALLOCATABLE :: disp_gb(:)  ! Displacemnet height
  REAL(KIND=real_jlslsm), ALLOCATABLE :: ztm_gb(:)   ! Roughness length
  REAL(KIND=real_jlslsm), ALLOCATABLE :: albwl_gb(:) ! Wall albedo
  REAL(KIND=real_jlslsm), ALLOCATABLE :: albrd_gb(:) ! Road albedo
  REAL(KIND=real_jlslsm), ALLOCATABLE :: emisw_gb(:) ! Wall emissivity
  REAL(KIND=real_jlslsm), ALLOCATABLE :: emisr_gb(:) ! Road emissivity
END TYPE urban_param_data_type

TYPE :: urban_param_type
  REAL(KIND=real_jlslsm), POINTER :: hgt_gb(:)   ! Building height
  REAL(KIND=real_jlslsm), POINTER :: hwr_gb(:)   ! Height to width ratio
  REAL(KIND=real_jlslsm), POINTER :: wrr_gb(:)   ! Width ratio
  REAL(KIND=real_jlslsm), POINTER :: disp_gb(:)  ! Displacemnet height
  REAL(KIND=real_jlslsm), POINTER :: ztm_gb(:)   ! Roughness length
  REAL(KIND=real_jlslsm), POINTER :: albwl_gb(:) ! Wall albedo
  REAL(KIND=real_jlslsm), POINTER :: albrd_gb(:) ! Road albedo
  REAL(KIND=real_jlslsm), POINTER :: emisw_gb(:) ! Wall emissivity
  REAL(KIND=real_jlslsm), POINTER :: emisr_gb(:) ! Road emissivity
END TYPE urban_param_type

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='URBAN_PARAM_MOD'

CONTAINS

!===============================================================================
SUBROUTINE urban_param_alloc(land_pts,                                         &
                       l_urban2t, l_moruses, urban_param_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: land_pts
TYPE(urban_param_data_type), INTENT(IN OUT) :: urban_param_data

LOGICAL, INTENT(IN) :: l_urban2t, l_moruses

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='URBAN_PARAM_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF ( l_urban2t .OR. l_moruses ) THEN
  ALLOCATE(urban_param_data%wrr_gb(land_pts))
  ALLOCATE(urban_param_data%hgt_gb(land_pts))
  ALLOCATE(urban_param_data%hwr_gb(land_pts))
  ALLOCATE(urban_param_data%disp_gb(land_pts))
  ALLOCATE(urban_param_data%ztm_gb(land_pts))
  ALLOCATE(urban_param_data%albwl_gb(land_pts))
  ALLOCATE(urban_param_data%albrd_gb(land_pts))
  ALLOCATE(urban_param_data%emisw_gb(land_pts))
  ALLOCATE(urban_param_data%emisr_gb(land_pts))

  urban_param_data%wrr_gb(:)   = 0.0
  urban_param_data%hgt_gb(:)   = 0.0
  urban_param_data%hwr_gb(:)   = 0.0
  urban_param_data%albwl_gb(:) = 0.0
  urban_param_data%albrd_gb(:) = 0.0
  urban_param_data%emisw_gb(:) = 0.0
  urban_param_data%emisr_gb(:) = 0.0
  urban_param_data%ztm_gb(:)   = 0.0
  urban_param_data%disp_gb(:)  = 0.0
ELSE
  ALLOCATE(urban_param_data%wrr_gb(1))
  ALLOCATE(urban_param_data%hgt_gb(1))
  ALLOCATE(urban_param_data%hwr_gb(1))
  ALLOCATE(urban_param_data%disp_gb(1))
  ALLOCATE(urban_param_data%ztm_gb(1))
  ALLOCATE(urban_param_data%albwl_gb(1))
  ALLOCATE(urban_param_data%albrd_gb(1))
  ALLOCATE(urban_param_data%emisw_gb(1))
  ALLOCATE(urban_param_data%emisr_gb(1))
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE urban_param_alloc

!===============================================================================
SUBROUTINE urban_param_dealloc(urban_param_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
TYPE(urban_param_data_type), INTENT(IN OUT) :: urban_param_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='URBAN_PARAM_DEALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DEALLOCATE(urban_param_data%wrr_gb)
DEALLOCATE(urban_param_data%hgt_gb)
DEALLOCATE(urban_param_data%hwr_gb)
DEALLOCATE(urban_param_data%disp_gb)
DEALLOCATE(urban_param_data%ztm_gb)
DEALLOCATE(urban_param_data%albwl_gb)
DEALLOCATE(urban_param_data%albrd_gb)
DEALLOCATE(urban_param_data%emisw_gb)
DEALLOCATE(urban_param_data%emisr_gb)


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE urban_param_dealloc

!===============================================================================
SUBROUTINE urban_param_assoc(urban_param, urban_param_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
TYPE(urban_param_type), INTENT(IN OUT) :: urban_param
TYPE(urban_param_data_type), INTENT(IN OUT), TARGET :: urban_param_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='URBAN_PARAM_ASSOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL urban_param_nullify(urban_param)

urban_param%wrr_gb => urban_param_data%wrr_gb
urban_param%hgt_gb => urban_param_data%hgt_gb
urban_param%hwr_gb => urban_param_data%hwr_gb
urban_param%disp_gb => urban_param_data%disp_gb
urban_param%ztm_gb => urban_param_data%ztm_gb
urban_param%albwl_gb => urban_param_data%albwl_gb
urban_param%albrd_gb => urban_param_data%albrd_gb
urban_param%emisw_gb => urban_param_data%emisw_gb
urban_param%emisr_gb => urban_param_data%emisr_gb


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE urban_param_assoc

!===============================================================================
SUBROUTINE urban_param_nullify(urban_param)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
TYPE(urban_param_type), INTENT(IN OUT) :: urban_param

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='URBAN_PARAM_NULLIFY'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

NULLIFY(urban_param%wrr_gb)
NULLIFY(urban_param%hgt_gb)
NULLIFY(urban_param%hwr_gb)
NULLIFY(urban_param%disp_gb)
NULLIFY(urban_param%ztm_gb)
NULLIFY(urban_param%albwl_gb)
NULLIFY(urban_param%albrd_gb)
NULLIFY(urban_param%emisw_gb)
NULLIFY(urban_param%emisr_gb)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE urban_param_nullify

END MODULE urban_param_mod
