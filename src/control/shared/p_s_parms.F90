! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE p_s_parms

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Module containing plant and soil variables (plus a few others)

! Note that all variables with suffix _soilt have soil tiles as their
! 2nd dimension. Cross-reference allocate_jules_arrays for more details.
!-----------------------------------------------------------------------------

! Implementation:
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

TYPE :: psparms_data_type
  REAL(KIND=real_jlslsm), ALLOCATABLE :: bexp_soilt(:,:,:)
    ! Exponent for soil moisture characteristic functions
    ! Brooks & Corey model: b=1/lambda
    ! van Genuchten model: b=1/(n-1)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: sathh_soilt(:,:,:)
    ! Parameter for soil moisture characteristic functions
    ! Brooks & Corey model: sathh is the saturated soil water pressure (m)
    ! van Genuchten model: sathh=1/alpha (m)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: hcap_soilt(:,:,:)
    ! Soil heat capacity (J/K/m3)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: hcon_soilt(:,:,:)
    ! Soil thermal conductivity (W/m/K)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: satcon_soilt(:,:,:)
    ! Saturated hydraulic conductivity (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: smvccl_soilt(:,:,:)
    ! Critical volumetric SMC (m3 water / m3 of soil)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: smvcst_soilt(:,:,:)
    ! Volumetric saturation point (m3 water / m3 of soil))
  REAL(KIND=real_jlslsm), ALLOCATABLE :: smvcwt_soilt(:,:,:)
    ! Volumetric wilting point (m3 water / m3 of soil))
  REAL(KIND=real_jlslsm), ALLOCATABLE :: v_close_pft(:,:,:)
    ! Volumetric soil moisture
    ! concentration below which stomata are fully closed.
    ! (m3 water / m3 of soil)).
    ! If l_use_pft_psi=F, this variable is not used, and
    ! the soil ancil variable sm_wilt is used instead.
  REAL(KIND=real_jlslsm), ALLOCATABLE :: v_open_pft(:,:,:)
    ! concentration above which stomatal aperture is not limited
    ! by soil water (m3 water / m3 of soil)).
    ! If l_use_pft_psi=F, this variable is not used, and
    ! the soil ancillary variable sm_crit and
    ! the pft variable fsmc_p0 are used instead.
  REAL(KIND=real_jlslsm), ALLOCATABLE :: clay_soilt(:,:,:)
    ! Fraction of clay (kg clay/kg soil).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: albsoil_soilt(:,:)
    ! Soil albedo
  REAL(KIND=real_jlslsm), ALLOCATABLE :: albobs_sw_gb(:)
    ! Obs SW albedo
  REAL(KIND=real_jlslsm), ALLOCATABLE :: albobs_vis_gb(:)
    ! Obs VIS albedo
  REAL(KIND=real_jlslsm), ALLOCATABLE :: albobs_nir_gb(:)
    ! Obs NIR albedo
  REAL(KIND=real_jlslsm), ALLOCATABLE :: catch_surft(:,:)
    ! Surface/canopy water capacity of snow-free land tiles (kg/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: catch_snow_surft(:,:)
    ! Snow interception capacity (kg/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: cosz_ij(:,:)
    ! Cosine of the zenith angle
  REAL(KIND=real_jlslsm), ALLOCATABLE :: infil_surft(:,:)
    ! Maximum possible surface infiltration for tiles (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: z0_surft(:,:)
    ! Surface roughness on tiles (m).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: z0h_bare_surft(:,:)
    ! Surface thermal roughness on tiles before allowance for snow
    ! cover (m).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: z0m_soil_gb(:)
    ! Bare soil roughness, for momentum (m).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: sthu_soilt(:,:,:)
    ! Unfrozen soil moisture content of the layers as a fraction of saturation
  REAL(KIND=real_jlslsm), ALLOCATABLE :: sthf_soilt(:,:,:)
    ! Frozen soil moisture content of the layers as a fraction of saturation.
  REAL(KIND=real_jlslsm), ALLOCATABLE :: sthu_min_soilt(:,:,:)
    ! Minimum unfrozen water content for each layer. Used to normalise
    ! thaw depth calculation based on unfrozen water content fraction.
  REAL(KIND=real_jlslsm), ALLOCATABLE :: soil_ph_soilt(:,:,:)
    ! Soil pH, defined on soil layers.
END TYPE

!================================

TYPE :: psparms_type
  REAL(KIND=real_jlslsm), POINTER :: bexp_soilt(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: sathh_soilt(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: hcap_soilt(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: hcon_soilt(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: satcon_soilt(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: smvccl_soilt(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: smvcst_soilt(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: smvcwt_soilt(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: v_close_pft(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: v_open_pft(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: clay_soilt(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: albsoil_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: albobs_sw_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: albobs_vis_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: albobs_nir_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: catch_surft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: catch_snow_surft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: cosz_ij(:,:)
  REAL(KIND=real_jlslsm), POINTER :: infil_surft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: z0_surft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: z0h_bare_surft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: z0m_soil_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: sthu_soilt(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: sthf_soilt(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: sthu_min_soilt(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: soil_ph_soilt(:,:,:)
END TYPE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='P_S_PARMS'

CONTAINS

SUBROUTINE psparms_alloc(land_pts,t_i_length,t_j_length,                       &
                     nsoilt,sm_levels,dim_cslayer,nsurft,npft,                 &
                     soil_bgc_model,soil_model_ecosse,l_use_pft_psi,           &
                     psparms_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: land_pts,t_i_length,t_j_length,                         &
                       nsoilt,sm_levels,dim_cslayer,nsurft,npft

INTEGER, INTENT(IN) :: soil_bgc_model,soil_model_ecosse

LOGICAL, INTENT(IN) :: l_use_pft_psi

TYPE(psparms_data_type), INTENT(IN OUT) :: psparms_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='PSPARMS_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

ALLOCATE(psparms_data%bexp_soilt(land_pts,nsoilt,sm_levels))
ALLOCATE(psparms_data%sathh_soilt(land_pts,nsoilt,sm_levels))
ALLOCATE(psparms_data%hcap_soilt(land_pts,nsoilt,sm_levels))
ALLOCATE(psparms_data%hcon_soilt(land_pts,nsoilt,0:sm_levels))
ALLOCATE(psparms_data%satcon_soilt(land_pts,nsoilt,0:sm_levels))
ALLOCATE(psparms_data%smvccl_soilt(land_pts,nsoilt,sm_levels))
ALLOCATE(psparms_data%smvcwt_soilt(land_pts,nsoilt,sm_levels))
ALLOCATE(psparms_data%smvcst_soilt(land_pts,nsoilt,sm_levels))
ALLOCATE(psparms_data%clay_soilt(land_pts,nsoilt,dim_cslayer))

psparms_data%bexp_soilt(:,:,:)   = 0.0
psparms_data%sathh_soilt(:,:,:)  = 0.0
psparms_data%hcap_soilt(:,:,:)   = 0.0
psparms_data%hcon_soilt(:,:,:)   = 0.0
psparms_data%satcon_soilt(:,:,:) = 0.0
psparms_data%smvccl_soilt(:,:,:) = 0.0
psparms_data%smvcwt_soilt(:,:,:) = 0.0
psparms_data%smvcst_soilt(:,:,:) = 0.0
psparms_data%clay_soilt(:,:,:)   = 0.0

! Plant and soil parameters
ALLOCATE(psparms_data%albsoil_soilt(land_pts,nsoilt))
ALLOCATE(psparms_data%albobs_sw_gb(land_pts))
ALLOCATE(psparms_data%albobs_vis_gb(land_pts))
ALLOCATE(psparms_data%albobs_nir_gb(land_pts))
ALLOCATE(psparms_data%catch_surft(land_pts,nsurft))
ALLOCATE(psparms_data%catch_snow_surft(land_pts,nsurft))
ALLOCATE(psparms_data%cosz_ij(t_i_length,t_j_length))
ALLOCATE(psparms_data%infil_surft(land_pts,nsurft))
ALLOCATE(psparms_data%z0_surft(land_pts,nsurft))
ALLOCATE(psparms_data%z0h_bare_surft(land_pts,nsurft))
ALLOCATE(psparms_data%z0m_soil_gb(land_pts))
ALLOCATE(psparms_data%sthu_soilt(land_pts,nsoilt,sm_levels))
ALLOCATE(psparms_data%sthf_soilt(land_pts,nsoilt,sm_levels))
ALLOCATE(psparms_data%sthu_min_soilt(land_pts,nsoilt,sm_levels))

psparms_data%albsoil_soilt(:,:)    = 0.0
psparms_data%albobs_sw_gb(:)       = 0.0
psparms_data%albobs_vis_gb(:)      = 0.0
psparms_data%albobs_nir_gb(:)      = 0.0
psparms_data%catch_surft(:,:)      = 0.0
psparms_data%catch_snow_surft(:,:) = 0.0
psparms_data%cosz_ij(:,:)          = 0.0
psparms_data%infil_surft(:,:)      = 0.0
psparms_data%z0_surft(:,:)         = 0.0
psparms_data%z0h_bare_surft(:,:)   = 0.0
psparms_data%z0m_soil_gb(:)        = 0.0
psparms_data%sthu_soilt(:,:,:)     = 0.0
psparms_data%sthf_soilt(:,:,:)     = 0.0
psparms_data%sthu_min_soilt(:,:,:) = 0.0

! Soil ancillaries on soil carbon layers. Only used with ECOSSE.
IF ( soil_bgc_model == soil_model_ecosse ) THEN
  ALLOCATE(psparms_data%soil_ph_soilt(land_pts,nsoilt,dim_cslayer))
  psparms_data%soil_ph_soilt(:,:,:)   = 0.0
ELSE
  ALLOCATE(psparms_data%soil_ph_soilt(1,1,1))
END IF

IF ( l_use_pft_psi ) THEN
  ALLOCATE(psparms_data%v_close_pft(land_pts,sm_levels,npft))
  ALLOCATE(psparms_data%v_open_pft(land_pts,sm_levels,npft))

  psparms_data%v_close_pft(:,:,:) = 0.0
  psparms_data%v_open_pft(:,:,:) = 0.0
ELSE
  ALLOCATE(psparms_data%v_close_pft(1,1,1))
  ALLOCATE(psparms_data%v_open_pft(1,1,1))
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE psparms_alloc

!===============================================================================
SUBROUTINE psparms_dealloc(psparms_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
TYPE(psparms_data_type), INTENT(IN OUT) :: psparms_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='PSPARMS_DEALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DEALLOCATE(psparms_data%bexp_soilt)
DEALLOCATE(psparms_data%sathh_soilt)
DEALLOCATE(psparms_data%hcap_soilt)
DEALLOCATE(psparms_data%hcon_soilt)
DEALLOCATE(psparms_data%satcon_soilt)
DEALLOCATE(psparms_data%smvccl_soilt)
DEALLOCATE(psparms_data%smvcwt_soilt)
DEALLOCATE(psparms_data%smvcst_soilt)
DEALLOCATE(psparms_data%clay_soilt)
DEALLOCATE(psparms_data%albsoil_soilt)
DEALLOCATE(psparms_data%albobs_sw_gb)
DEALLOCATE(psparms_data%albobs_vis_gb)
DEALLOCATE(psparms_data%albobs_nir_gb)
DEALLOCATE(psparms_data%catch_surft)
DEALLOCATE(psparms_data%catch_snow_surft)
DEALLOCATE(psparms_data%cosz_ij)
DEALLOCATE(psparms_data%infil_surft)
DEALLOCATE(psparms_data%z0_surft)
DEALLOCATE(psparms_data%z0h_bare_surft)
DEALLOCATE(psparms_data%z0m_soil_gb)
DEALLOCATE(psparms_data%sthu_soilt)
DEALLOCATE(psparms_data%sthf_soilt)
DEALLOCATE(psparms_data%sthu_min_soilt)
DEALLOCATE(psparms_data%soil_ph_soilt)
DEALLOCATE(psparms_data%v_close_pft)
DEALLOCATE(psparms_data%v_open_pft)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE psparms_dealloc

!==============================================================================
SUBROUTINE psparms_assoc(psparms,psparms_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

TYPE(psparms_type), INTENT(IN OUT) :: psparms
  !Instance of the pointer type we are associating

TYPE(psparms_data_type), INTENT(IN OUT), TARGET :: psparms_data
  !Instance of the data type we are associtating to

!Local variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='PSPARMS_ASSOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL psparms_nullify(psparms)

psparms%bexp_soilt => psparms_data%bexp_soilt
psparms%sathh_soilt => psparms_data%sathh_soilt
psparms%hcap_soilt => psparms_data%hcap_soilt
psparms%hcon_soilt => psparms_data%hcon_soilt
psparms%satcon_soilt => psparms_data%satcon_soilt
psparms%smvccl_soilt => psparms_data%smvccl_soilt
psparms%smvcwt_soilt => psparms_data%smvcwt_soilt
psparms%smvcst_soilt => psparms_data%smvcst_soilt
psparms%clay_soilt => psparms_data%clay_soilt
psparms%albsoil_soilt => psparms_data%albsoil_soilt
psparms%albobs_sw_gb => psparms_data%albobs_sw_gb
psparms%albobs_vis_gb => psparms_data%albobs_vis_gb
psparms%albobs_nir_gb => psparms_data%albobs_nir_gb
psparms%catch_surft => psparms_data%catch_surft
psparms%catch_snow_surft => psparms_data%catch_snow_surft
psparms%cosz_ij => psparms_data%cosz_ij
psparms%infil_surft => psparms_data%infil_surft
psparms%z0_surft => psparms_data%z0_surft
psparms%z0h_bare_surft => psparms_data%z0h_bare_surft
psparms%z0m_soil_gb => psparms_data%z0m_soil_gb
psparms%sthu_soilt => psparms_data%sthu_soilt
psparms%sthf_soilt => psparms_data%sthf_soilt
psparms%sthu_min_soilt => psparms_data%sthu_min_soilt
psparms%soil_ph_soilt => psparms_data%soil_ph_soilt
psparms%v_close_pft => psparms_data%v_close_pft
psparms%v_open_pft => psparms_data%v_open_pft

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE psparms_assoc

!==============================================================================

SUBROUTINE psparms_nullify(psparms)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

TYPE(psparms_type), INTENT(IN OUT) :: psparms
  !Instance of the pointer type we are nullifying

!Local variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='PSPARMS_NULLIFY'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

NULLIFY(psparms%bexp_soilt)
NULLIFY(psparms%sathh_soilt)
NULLIFY(psparms%hcap_soilt)
NULLIFY(psparms%hcon_soilt)
NULLIFY(psparms%satcon_soilt)
NULLIFY(psparms%smvccl_soilt)
NULLIFY(psparms%smvcwt_soilt)
NULLIFY(psparms%smvcst_soilt)
NULLIFY(psparms%clay_soilt)
NULLIFY(psparms%albsoil_soilt)
NULLIFY(psparms%albobs_sw_gb)
NULLIFY(psparms%albobs_vis_gb)
NULLIFY(psparms%albobs_nir_gb)
NULLIFY(psparms%catch_surft)
NULLIFY(psparms%catch_snow_surft)
NULLIFY(psparms%cosz_ij)
NULLIFY(psparms%infil_surft)
NULLIFY(psparms%z0_surft)
NULLIFY(psparms%z0h_bare_surft)
NULLIFY(psparms%z0m_soil_gb)
NULLIFY(psparms%sthu_soilt)
NULLIFY(psparms%sthf_soilt)
NULLIFY(psparms%sthu_min_soilt)
NULLIFY(psparms%soil_ph_soilt)
NULLIFY(psparms%v_close_pft)
NULLIFY(psparms%v_open_pft)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE psparms_nullify

END MODULE p_s_parms
