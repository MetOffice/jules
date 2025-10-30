MODULE trifctl

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Module containing the variables for TRIFFID and plant phenology
! (not parameter values)
!-----------------------------------------------------------------------------

INTEGER :: asteps_since_triffid
    ! Number of atmospheric timesteps since last call to TRIFFID

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='TRIFCTL'

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
TYPE :: trifctl_data_type
  ! Contains LOGICALs, INTEGERs and REALs
  REAL(KIND=real_jlslsm), ALLOCATABLE :: g_leaf_acc_pft(:,:)
    ! Accumulated leaf turnover rate (/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: npp_acc_pft(:,:)
    ! Accumulated NPP_FT (kg m-2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: g_leaf_phen_acc_pft(:,:)
    ! Accumulated leaf turnover rate including phenology (/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: resp_w_acc_pft(:,:)
    ! Accumulated RESP_W_FT (kg m-2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: resp_s_acc_soilt(:,:,:,:)
    ! Accumulated RESP_S (kg m-2).

  REAL(KIND=real_jlslsm), ALLOCATABLE :: gpp_gb(:)
    ! Gross primary productivity (kg C/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: npp_gb(:)
    ! Net primary productivity (kg C/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: resp_p_gb(:)
    ! Plant respiration (kg C/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: g_leaf_pft(:,:)
    ! Leaf turnover rate (/360days)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: g_leaf_phen_pft(:,:)
    ! Mean leaf turnover rate over phenology period (/360days)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: gpp_pft(:,:)
    ! Gross primary productivity on PFTs (kg C/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: npp_pft(:,:)
    ! Net primary productivity on PFTs (kg C/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: resp_p_pft(:,:)
    ! Plant respiration on PFTs (kg C/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: resp_s_soilt(:,:,:,:)
    ! Soil respiration (kg C/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: resp_w_pft(:,:)
    ! Wood maintenance respiration (kg C/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: lai_phen_pft(:,:)
    ! LAI of PFTs after phenology.
    ! Required as separate variable for top-level argument list matching
    ! with VEG_IC2A
  REAL(KIND=real_jlslsm), ALLOCATABLE :: c_veg_pft(:,:)
    ! Total carbon content of the vegetation (kg C/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: cv_gb(:)
    ! Gridbox mean vegetation carbon (kg C/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: g_leaf_day_pft(:,:)
    ! Mean leaf turnover rate for input to PHENOL (/360days)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: g_leaf_dr_out_pft(:,:)
    ! Mean leaf turnover rate for driving TRIFFID (/360days)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: lit_c_pft(:,:)
    ! Carbon Litter (kg C/m2/360days)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: lit_c_mn_gb(:)
    ! Gridbox mean carbon litter (kg C/m2/360days)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: npp_dr_out_pft(:,:)
    ! Mean NPP for driving TRIFFID (kg C/m2/360days)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: resp_w_dr_out_pft(:,:)
    ! Mean wood respiration for driving TRIFFID (kg C/m2/360days)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: resp_s_dr_out_gb(:,:,:)
    ! Mean soil respiration for driving TRIFFID (kg C/m2/360days)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: frac_agr_gb(:)
    ! Fraction of agriculture
  REAL(KIND=real_jlslsm), ALLOCATABLE :: growth_sug_gb(:)
    ! Structural carbon growth rate (kg C/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: growth_sug_pft(:,:)
    ! Structural carbon growth rate on PFT (kg C/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: lwp_c_pft(:,:)
    ! Diagnostic for canopy leaf water potential
    ! in the SOX stomatal model (MPa)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: psi_root_zone_pft(:,:)
    ! (Negative) soil water potential in the root zone (Pa)
END TYPE

!===============================================================================
TYPE :: trifctl_type
  ! Contains LOGICALs, INTEGERs and REALs
  REAL(KIND=real_jlslsm), POINTER :: g_leaf_acc_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: npp_acc_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: g_leaf_phen_acc_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: resp_w_acc_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: resp_s_acc_soilt(:,:,:,:)

  REAL(KIND=real_jlslsm), POINTER :: gpp_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: npp_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: resp_p_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: g_leaf_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: g_leaf_phen_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: gpp_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: npp_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: resp_p_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: resp_s_soilt(:,:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: resp_w_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: lai_phen_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: c_veg_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: cv_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: g_leaf_day_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: g_leaf_dr_out_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: lit_c_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: lit_c_mn_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: npp_dr_out_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: resp_w_dr_out_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: resp_s_dr_out_gb(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: frac_agr_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: growth_sug_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: growth_sug_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: lwp_c_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: psi_root_zone_pft(:,:)
END TYPE

CONTAINS

SUBROUTINE trifctl_alloc(land_pts,npft,dim_cslayer,dim_cs1,nsoilt, trifctl_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: land_pts,                                               &
                   npft,dim_cslayer,dim_cs1,nsoilt
TYPE(trifctl_data_type), INTENT(IN OUT) :: trifctl_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='TRIFCTL_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!  ====trifctl module common====
! Triffid variables
ALLOCATE(trifctl_data%g_leaf_acc_pft(land_pts,npft))
ALLOCATE(trifctl_data%npp_acc_pft(land_pts,npft))
ALLOCATE(trifctl_data%resp_w_acc_pft(land_pts,npft))
ALLOCATE(trifctl_data%resp_s_acc_soilt(land_pts,nsoilt,dim_cslayer,dim_cs1))
ALLOCATE(trifctl_data%g_leaf_phen_acc_pft(land_pts,npft))
ALLOCATE(trifctl_data%gpp_gb(land_pts))
ALLOCATE(trifctl_data%npp_gb(land_pts))
ALLOCATE(trifctl_data%resp_p_gb(land_pts))
ALLOCATE(trifctl_data%g_leaf_pft(land_pts,npft))
ALLOCATE(trifctl_data%g_leaf_phen_pft(land_pts,npft))
ALLOCATE(trifctl_data%gpp_pft(land_pts,npft))
ALLOCATE(trifctl_data%npp_pft(land_pts,npft))
ALLOCATE(trifctl_data%resp_p_pft(land_pts,npft))
ALLOCATE(trifctl_data%resp_s_soilt(land_pts,nsoilt,dim_cslayer,dim_cs1))
ALLOCATE(trifctl_data%resp_w_pft(land_pts,npft))
ALLOCATE(trifctl_data%lai_phen_pft(land_pts,npft))
ALLOCATE(trifctl_data%c_veg_pft(land_pts,npft))
ALLOCATE(trifctl_data%cv_gb(land_pts))
ALLOCATE(trifctl_data%g_leaf_day_pft(land_pts,npft))
ALLOCATE(trifctl_data%g_leaf_dr_out_pft(land_pts,npft))
ALLOCATE(trifctl_data%lit_c_pft(land_pts,npft))
ALLOCATE(trifctl_data%lit_c_mn_gb(land_pts))
ALLOCATE(trifctl_data%npp_dr_out_pft(land_pts,npft))
ALLOCATE(trifctl_data%resp_w_dr_out_pft(land_pts,npft))
ALLOCATE(trifctl_data%resp_s_dr_out_gb(land_pts,dim_cslayer,5))
ALLOCATE(trifctl_data%frac_agr_gb(land_pts))
ALLOCATE(trifctl_data%growth_sug_gb(land_pts))
ALLOCATE(trifctl_data%growth_sug_pft(land_pts,npft))
ALLOCATE(trifctl_data%lwp_c_pft(land_pts,npft))
ALLOCATE(trifctl_data%psi_root_zone_pft(land_pts,npft))

trifctl_data%g_leaf_acc_pft(:,:)       = 0.0
trifctl_data%npp_acc_pft(:,:)          = 0.0
trifctl_data%resp_w_acc_pft(:,:)       = 0.0
trifctl_data%resp_s_acc_soilt(:,:,:,:) = 0.0
trifctl_data%g_leaf_phen_acc_pft(:,:)  = 0.0
trifctl_data%gpp_gb(:)                 = 0.0
trifctl_data%npp_gb(:)                 = 0.0
trifctl_data%resp_p_gb(:)              = 0.0
trifctl_data%g_leaf_pft(:,:)           = 0.0
trifctl_data%g_leaf_phen_pft(:,:)      = 0.0
trifctl_data%gpp_pft(:,:)              = 0.0
trifctl_data%npp_pft(:,:)              = 0.0
trifctl_data%resp_p_pft(:,:)           = 0.0
trifctl_data%resp_s_soilt(:,:,:,:)     = 0.0
trifctl_data%resp_w_pft(:,:)           = 0.0
trifctl_data%lai_phen_pft(:,:)         = 0.0
trifctl_data%c_veg_pft(:,:)            = 0.0
trifctl_data%cv_gb(:)                  = 0.0
trifctl_data%g_leaf_day_pft(:,:)       = 0.0
trifctl_data%g_leaf_dr_out_pft(:,:)    = 0.0
trifctl_data%lit_c_pft(:,:)            = 0.0
trifctl_data%lit_c_mn_gb(:)            = 0.0
trifctl_data%npp_dr_out_pft(:,:)       = 0.0
trifctl_data%resp_w_dr_out_pft(:,:)    = 0.0
trifctl_data%resp_s_dr_out_gb(:,:,:)   = 0.0
trifctl_data%frac_agr_gb(:)            = 0.0
trifctl_data%growth_sug_gb(:)          = 0.0
trifctl_data%growth_sug_pft(:,:)       = 0.0
trifctl_data%lwp_c_pft(:,:)            = 0.0
trifctl_data%psi_root_zone_pft(:,:)    = 0.0

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE trifctl_alloc

!===============================================================================
SUBROUTINE trifctl_dealloc(trifctl_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(trifctl_data_type), INTENT(IN OUT) :: trifctl_data

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='TRIFCTL_DEALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DEALLOCATE(trifctl_data%g_leaf_acc_pft)
DEALLOCATE(trifctl_data%npp_acc_pft)
DEALLOCATE(trifctl_data%resp_w_acc_pft)
DEALLOCATE(trifctl_data%resp_s_acc_soilt)
DEALLOCATE(trifctl_data%g_leaf_phen_acc_pft)
DEALLOCATE(trifctl_data%gpp_gb)
DEALLOCATE(trifctl_data%npp_gb)
DEALLOCATE(trifctl_data%resp_p_gb)
DEALLOCATE(trifctl_data%g_leaf_pft)
DEALLOCATE(trifctl_data%g_leaf_phen_pft)
DEALLOCATE(trifctl_data%gpp_pft)
DEALLOCATE(trifctl_data%npp_pft)
DEALLOCATE(trifctl_data%resp_p_pft)
DEALLOCATE(trifctl_data%resp_s_soilt)
DEALLOCATE(trifctl_data%resp_w_pft)
DEALLOCATE(trifctl_data%lai_phen_pft)
DEALLOCATE(trifctl_data%c_veg_pft)
DEALLOCATE(trifctl_data%cv_gb)
DEALLOCATE(trifctl_data%g_leaf_day_pft)
DEALLOCATE(trifctl_data%g_leaf_dr_out_pft)
DEALLOCATE(trifctl_data%lit_c_pft)
DEALLOCATE(trifctl_data%lit_c_mn_gb)
DEALLOCATE(trifctl_data%npp_dr_out_pft)
DEALLOCATE(trifctl_data%resp_w_dr_out_pft)
DEALLOCATE(trifctl_data%resp_s_dr_out_gb)
DEALLOCATE(trifctl_data%frac_agr_gb)
DEALLOCATE(trifctl_data%growth_sug_gb)
DEALLOCATE(trifctl_data%growth_sug_pft)
DEALLOCATE(trifctl_data%lwp_c_pft)
DEALLOCATE(trifctl_data%psi_root_zone_pft)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE trifctl_dealloc

!===============================================================================
SUBROUTINE trifctl_assoc(trifctl,trifctl_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(trifctl_data_type), TARGET, INTENT(IN OUT) :: trifctl_data
  ! Instance of the data type we are associtating to
TYPE(trifctl_type), INTENT(IN OUT) :: trifctl
  ! Instance of the pointer type we are associating

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='TRIFCTL_ASSOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL trifctl_nullify(trifctl)

trifctl%g_leaf_acc_pft       => trifctl_data%g_leaf_acc_pft
trifctl%npp_acc_pft          => trifctl_data%npp_acc_pft
trifctl%resp_w_acc_pft       => trifctl_data%resp_w_acc_pft
trifctl%resp_s_acc_soilt => trifctl_data%resp_s_acc_soilt
trifctl%g_leaf_phen_acc_pft  => trifctl_data%g_leaf_phen_acc_pft
trifctl%gpp_gb                 => trifctl_data%gpp_gb
trifctl%npp_gb                 => trifctl_data%npp_gb
trifctl%resp_p_gb              => trifctl_data%resp_p_gb
trifctl%g_leaf_pft           => trifctl_data%g_leaf_pft
trifctl%g_leaf_phen_pft      => trifctl_data%g_leaf_phen_pft
trifctl%gpp_pft              => trifctl_data%gpp_pft
trifctl%npp_pft              => trifctl_data%npp_pft
trifctl%resp_p_pft           => trifctl_data%resp_p_pft
trifctl%resp_s_soilt     => trifctl_data%resp_s_soilt
trifctl%resp_w_pft           => trifctl_data%resp_w_pft
trifctl%lai_phen_pft         => trifctl_data%lai_phen_pft
trifctl%c_veg_pft            => trifctl_data%c_veg_pft
trifctl%cv_gb                  => trifctl_data%cv_gb
trifctl%g_leaf_day_pft       => trifctl_data%g_leaf_day_pft
trifctl%g_leaf_dr_out_pft    => trifctl_data%g_leaf_dr_out_pft
trifctl%lit_c_pft            => trifctl_data%lit_c_pft
trifctl%lit_c_mn_gb            => trifctl_data%lit_c_mn_gb
trifctl%npp_dr_out_pft       => trifctl_data%npp_dr_out_pft
trifctl%resp_w_dr_out_pft    => trifctl_data%resp_w_dr_out_pft
trifctl%resp_s_dr_out_gb   => trifctl_data%resp_s_dr_out_gb
trifctl%frac_agr_gb            => trifctl_data%frac_agr_gb
trifctl%growth_sug_gb        => trifctl_data%growth_sug_gb
trifctl%growth_sug_pft       => trifctl_data%growth_sug_pft
trifctl%lwp_c_pft            => trifctl_data%lwp_c_pft
trifctl%psi_root_zone_pft    => trifctl_data%psi_root_zone_pft

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE trifctl_assoc

!===============================================================================
SUBROUTINE trifctl_nullify(trifctl)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(trifctl_type), INTENT(IN OUT) :: trifctl
  ! Instance of the pointer type we are associating

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='TRIFCTL_NULLIFY'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

NULLIFY(trifctl%g_leaf_acc_pft)
NULLIFY(trifctl%npp_acc_pft)
NULLIFY(trifctl%resp_w_acc_pft)
NULLIFY(trifctl%resp_s_acc_soilt)
NULLIFY(trifctl%g_leaf_phen_acc_pft)
NULLIFY(trifctl%gpp_gb)
NULLIFY(trifctl%npp_gb)
NULLIFY(trifctl%resp_p_gb)
NULLIFY(trifctl%g_leaf_pft)
NULLIFY(trifctl%g_leaf_phen_pft)
NULLIFY(trifctl%gpp_pft)
NULLIFY(trifctl%npp_pft)
NULLIFY(trifctl%resp_p_pft)
NULLIFY(trifctl%resp_s_soilt)
NULLIFY(trifctl%resp_w_pft)
NULLIFY(trifctl%lai_phen_pft)
NULLIFY(trifctl%c_veg_pft)
NULLIFY(trifctl%cv_gb)
NULLIFY(trifctl%g_leaf_day_pft)
NULLIFY(trifctl%g_leaf_dr_out_pft)
NULLIFY(trifctl%lit_c_pft)
NULLIFY(trifctl%lit_c_mn_gb)
NULLIFY(trifctl%npp_dr_out_pft)
NULLIFY(trifctl%resp_w_dr_out_pft)
NULLIFY(trifctl%resp_s_dr_out_gb)
NULLIFY(trifctl%frac_agr_gb)
NULLIFY(trifctl%growth_sug_gb)
NULLIFY(trifctl%growth_sug_pft)
NULLIFY(trifctl%lwp_c_pft)
NULLIFY(trifctl%psi_root_zone_pft)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE trifctl_nullify

!-------------------------------------------------------------------------------

END MODULE trifctl
