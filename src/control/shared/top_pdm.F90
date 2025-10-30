! Module containing the variables for Topmodel and PDM.

! DBC Arguably sthzw and zw should be stored in PROGNOSTICS since they
! are indeed prognostics. fsat needs to persist between timesteps (but
! can be initialised (recalculated) from soil moisture).

MODULE top_pdm

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

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
TYPE :: top_pdm_data_type
  ! Contains LOGICALs, INTEGERs and REALs
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fexp_soilt(:,:)
    ! Decay factor in Sat. Conductivity in deep LSH/TOPMODEL layer
  REAL(KIND=real_jlslsm), ALLOCATABLE :: gamtot_soilt(:,:)
    ! Integrated complete Gamma function
    ! DBC gamtot doesn't need to be in a module in this version, but left there
    ! for now for compatability.
  REAL(KIND=real_jlslsm), ALLOCATABLE :: ti_mean_soilt(:,:)
    ! Mean topographic index
  REAL(KIND=real_jlslsm), ALLOCATABLE :: ti_sig_soilt(:,:)
    ! Standard dev. of topographic index
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fsat_soilt(:,:)
    ! Surface saturation fraction
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fwetl_soilt(:,:)
    ! Wetland fraction
  REAL(KIND=real_jlslsm), ALLOCATABLE :: zw_soilt(:,:)
    ! Water table depth (m)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: drain_soilt(:,:)
    ! Drainage out of bottom (nshyd) soil layer (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: dun_roff_soilt(:,:)
    ! Dunne part of sfc runoff (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: qbase_soilt(:,:)
    ! Base flow (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: qbase_zw_soilt(:,:)
    ! Base flow from deep LSH/TOPMODEL layer (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fch4_wetl_soilt(:,:)
    ! Scaled wetland methane flux, as
    ! used in atmospheric chemistry.
    ! The substrate is set by parameter ch4_substrate.
    ! (Note different units: 10^-9 kg C/m2/s).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fch4_wetl_cs_soilt(:,:)
    ! Scaled wetland methane flux using
    ! soil carbon as substrate (kg C/m2/s).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: cs_ch4_soilt(:,:)
    ! soil carbon used in wetland CH4 emissions model if TRIFFID
    ! is switched off
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fch4_wetl_npp_soilt(:,:)
    ! Scaled wetland methane flux using
    ! NPP as substrate (kg C/m2/s).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fch4_wetl_resps_soilt(:,:)
    ! Scaled wetland methane flux using
    ! soil respiration as substrate (kg C/m2/s).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fch4_wetl_acc_soilt(:,:)
    ! Accum scaled wetland methane flux (kg C/m2)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: inlandout_atm_gb(:)
    ! TRIP inland basin outflow (for land points only)(kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: sthzw_soilt(:,:)
    ! soil moist fraction in deep LSH/TOPMODEL layer.
  REAL(KIND=real_jlslsm), ALLOCATABLE :: a_fsat_soilt(:,:)
    ! Fitting parameter for Fsat in LSH model
  REAL(KIND=real_jlslsm), ALLOCATABLE :: c_fsat_soilt(:,:)
    ! Fitting parameter for Fsat in LSH model
  REAL(KIND=real_jlslsm), ALLOCATABLE :: a_fwet_soilt(:,:)
    ! Fitting parameter for Fwet in LSH model
  REAL(KIND=real_jlslsm), ALLOCATABLE :: c_fwet_soilt(:,:)
    ! Fitting parameter for Fwet in LSH model
  ! From pdm_vars_alloc
  REAL(KIND=real_jlslsm), ALLOCATABLE :: slope_gb(:)
    ! Terrain slope: to be used in spatial varying b_pdm calculation
END TYPE

!===============================================================================
TYPE :: top_pdm_type
  ! Contains LOGICALs, INTEGERs and REALs
  REAL(KIND=real_jlslsm), POINTER :: fexp_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: gamtot_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: ti_mean_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: ti_sig_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: fsat_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: fwetl_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: zw_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: drain_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: dun_roff_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: qbase_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: qbase_zw_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: fch4_wetl_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: fch4_wetl_cs_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: cs_ch4_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: fch4_wetl_npp_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: fch4_wetl_resps_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: fch4_wetl_acc_soilt(:,:)

  REAL(KIND=real_jlslsm), POINTER :: inlandout_atm_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: sthzw_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: a_fsat_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: c_fsat_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: a_fwet_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: c_fwet_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: slope_gb(:)
END TYPE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='TOP_PDM'

CONTAINS

SUBROUTINE top_pdm_alloc(land_pts,nsoilt, top_pdm_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: land_pts, nsoilt
TYPE(top_pdm_data_type), INTENT(IN OUT) :: top_pdm_data

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='TOP_PDM_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

ALLOCATE(top_pdm_data%a_fsat_soilt(land_pts,nsoilt))
ALLOCATE(top_pdm_data%a_fwet_soilt(land_pts,nsoilt))
ALLOCATE(top_pdm_data%c_fsat_soilt(land_pts,nsoilt))
ALLOCATE(top_pdm_data%c_fwet_soilt(land_pts,nsoilt))
ALLOCATE(top_pdm_data%drain_soilt(land_pts,nsoilt))
ALLOCATE(top_pdm_data%dun_roff_soilt(land_pts,nsoilt))
ALLOCATE(top_pdm_data%fch4_wetl_soilt(land_pts,nsoilt))
ALLOCATE(top_pdm_data%fch4_wetl_acc_soilt(land_pts,nsoilt))
ALLOCATE(top_pdm_data%fch4_wetl_cs_soilt(land_pts,nsoilt))
ALLOCATE(top_pdm_data%cs_ch4_soilt(land_pts,nsoilt))
ALLOCATE(top_pdm_data%fch4_wetl_npp_soilt(land_pts,nsoilt))
ALLOCATE(top_pdm_data%fch4_wetl_resps_soilt(land_pts,nsoilt))
ALLOCATE(top_pdm_data%fexp_soilt(land_pts,nsoilt))
ALLOCATE(top_pdm_data%fsat_soilt(land_pts,nsoilt))
ALLOCATE(top_pdm_data%fwetl_soilt(land_pts,nsoilt))
ALLOCATE(top_pdm_data%gamtot_soilt(land_pts,nsoilt))
ALLOCATE(top_pdm_data%qbase_soilt(land_pts,nsoilt))
ALLOCATE(top_pdm_data%qbase_zw_soilt(land_pts,nsoilt))
ALLOCATE(top_pdm_data%sthzw_soilt(land_pts,nsoilt))
ALLOCATE(top_pdm_data%ti_mean_soilt(land_pts,nsoilt))
ALLOCATE(top_pdm_data%ti_sig_soilt(land_pts,nsoilt))
ALLOCATE(top_pdm_data%zw_soilt(land_pts,nsoilt))
ALLOCATE(top_pdm_data%inlandout_atm_gb(land_pts))
! from pdm_vars_alloc
ALLOCATE(top_pdm_data%slope_gb(land_pts))

top_pdm_data%a_fsat_soilt(:,:)          = 0.0
top_pdm_data%a_fwet_soilt(:,:)          = 0.0
top_pdm_data%c_fsat_soilt(:,:)          = 0.0
top_pdm_data%c_fwet_soilt(:,:)          = 0.0
top_pdm_data%drain_soilt(:,:)           = 0.0
top_pdm_data%dun_roff_soilt(:,:)        = 0.0
top_pdm_data%fch4_wetl_soilt(:,:)       = 0.0
top_pdm_data%fch4_wetl_acc_soilt(:,:)   = 0.0
top_pdm_data%fch4_wetl_cs_soilt(:,:)    = 0.0
top_pdm_data%cs_ch4_soilt(:,:)          = 0.0
top_pdm_data%fch4_wetl_resps_soilt(:,:) = 0.0
top_pdm_data%fch4_wetl_npp_soilt(:,:)   = 0.0
top_pdm_data%fexp_soilt(:,:)            = 0.0
top_pdm_data%fsat_soilt(:,:)            = 0.0
top_pdm_data%fwetl_soilt(:,:)           = 0.0
top_pdm_data%gamtot_soilt(:,:)          = 0.0
top_pdm_data%qbase_soilt(:,:)           = 0.0
top_pdm_data%qbase_zw_soilt(:,:)        = 0.0
top_pdm_data%sthzw_soilt(:,:)           = 0.0
top_pdm_data%ti_mean_soilt(:,:)         = 0.0
top_pdm_data%ti_sig_soilt(:,:)          = 0.0
top_pdm_data%zw_soilt(:,:)              = 0.0
top_pdm_data%inlandout_atm_gb(:)        = 0.0
! from pdm_vars_alloc
top_pdm_data%slope_gb(:)                = 0.0

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE top_pdm_alloc


!===============================================================================
SUBROUTINE top_pdm_dealloc(top_pdm_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(top_pdm_data_type), INTENT(IN OUT) :: top_pdm_data

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='TOP_PDM_DEALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DEALLOCATE(top_pdm_data%a_fsat_soilt)
DEALLOCATE(top_pdm_data%a_fwet_soilt)
DEALLOCATE(top_pdm_data%c_fsat_soilt)
DEALLOCATE(top_pdm_data%c_fwet_soilt)
DEALLOCATE(top_pdm_data%drain_soilt)
DEALLOCATE(top_pdm_data%dun_roff_soilt)
DEALLOCATE(top_pdm_data%fch4_wetl_soilt)
DEALLOCATE(top_pdm_data%fch4_wetl_acc_soilt)
DEALLOCATE(top_pdm_data%fch4_wetl_cs_soilt)
DEALLOCATE(top_pdm_data%cs_ch4_soilt)
DEALLOCATE(top_pdm_data%fch4_wetl_npp_soilt)
DEALLOCATE(top_pdm_data%fch4_wetl_resps_soilt)
DEALLOCATE(top_pdm_data%fexp_soilt)
DEALLOCATE(top_pdm_data%fsat_soilt)
DEALLOCATE(top_pdm_data%fwetl_soilt)
DEALLOCATE(top_pdm_data%gamtot_soilt)
DEALLOCATE(top_pdm_data%qbase_soilt)
DEALLOCATE(top_pdm_data%qbase_zw_soilt)
DEALLOCATE(top_pdm_data%sthzw_soilt)
DEALLOCATE(top_pdm_data%ti_mean_soilt)
DEALLOCATE(top_pdm_data%ti_sig_soilt)
DEALLOCATE(top_pdm_data%zw_soilt)
DEALLOCATE(top_pdm_data%inlandout_atm_gb)
! from pdm_vars_alloc
DEALLOCATE(top_pdm_data%slope_gb)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE top_pdm_dealloc

!===============================================================================
SUBROUTINE top_pdm_assoc(top_pdm,top_pdm_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(top_pdm_data_type), TARGET, INTENT(IN OUT) :: top_pdm_data
! Instance of the data type we are associtating to
TYPE(top_pdm_type), INTENT(IN OUT) :: top_pdm
  ! Instance of the pointer type we are associating

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='TOP_PDM_ASSOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL top_pdm_nullify(top_pdm)

top_pdm%a_fsat_soilt => top_pdm_data%a_fsat_soilt
top_pdm%a_fwet_soilt => top_pdm_data%a_fwet_soilt
top_pdm%c_fsat_soilt => top_pdm_data%c_fsat_soilt
top_pdm%c_fwet_soilt => top_pdm_data%c_fwet_soilt
top_pdm%drain_soilt => top_pdm_data%drain_soilt
top_pdm%dun_roff_soilt => top_pdm_data%dun_roff_soilt
top_pdm%fch4_wetl_soilt => top_pdm_data%fch4_wetl_soilt
top_pdm%fch4_wetl_acc_soilt => top_pdm_data%fch4_wetl_acc_soilt
top_pdm%fch4_wetl_cs_soilt => top_pdm_data%fch4_wetl_cs_soilt
top_pdm%cs_ch4_soilt => top_pdm_data%cs_ch4_soilt
top_pdm%fch4_wetl_resps_soilt => top_pdm_data%fch4_wetl_resps_soilt
top_pdm%fch4_wetl_npp_soilt => top_pdm_data%fch4_wetl_npp_soilt
top_pdm%fexp_soilt => top_pdm_data%fexp_soilt
top_pdm%fsat_soilt => top_pdm_data%fsat_soilt
top_pdm%fwetl_soilt => top_pdm_data%fwetl_soilt
top_pdm%gamtot_soilt => top_pdm_data%gamtot_soilt
top_pdm%qbase_soilt => top_pdm_data%qbase_soilt
top_pdm%qbase_zw_soilt => top_pdm_data%qbase_zw_soilt
top_pdm%sthzw_soilt => top_pdm_data%sthzw_soilt
top_pdm%ti_mean_soilt => top_pdm_data%ti_mean_soilt
top_pdm%ti_sig_soilt => top_pdm_data%ti_sig_soilt
top_pdm%zw_soilt => top_pdm_data%zw_soilt
top_pdm%inlandout_atm_gb => top_pdm_data%inlandout_atm_gb
! from pdm_vars_alloc
top_pdm%slope_gb => top_pdm_data%slope_gb

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE top_pdm_assoc

!===============================================================================
SUBROUTINE top_pdm_nullify(top_pdm)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(top_pdm_type), INTENT(IN OUT) :: top_pdm
  ! Instance of the pointer type we are associating

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='TOP_PDM_NULLIFY'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

NULLIFY(top_pdm%a_fsat_soilt)
NULLIFY(top_pdm%a_fwet_soilt)
NULLIFY(top_pdm%c_fsat_soilt)
NULLIFY(top_pdm%c_fwet_soilt)
NULLIFY(top_pdm%drain_soilt)
NULLIFY(top_pdm%dun_roff_soilt)
NULLIFY(top_pdm%fch4_wetl_soilt)
NULLIFY(top_pdm%fch4_wetl_acc_soilt)
NULLIFY(top_pdm%fch4_wetl_cs_soilt)
NULLIFY(top_pdm%cs_ch4_soilt)
NULLIFY(top_pdm%fch4_wetl_npp_soilt)
NULLIFY(top_pdm%fch4_wetl_resps_soilt)
NULLIFY(top_pdm%fexp_soilt)
NULLIFY(top_pdm%fsat_soilt)
NULLIFY(top_pdm%fwetl_soilt)
NULLIFY(top_pdm%gamtot_soilt)
NULLIFY(top_pdm%qbase_soilt)
NULLIFY(top_pdm%qbase_zw_soilt)
NULLIFY(top_pdm%sthzw_soilt)
NULLIFY(top_pdm%ti_mean_soilt)
NULLIFY(top_pdm%ti_sig_soilt)
NULLIFY(top_pdm%zw_soilt)
NULLIFY(top_pdm%inlandout_atm_gb)
! from pdm_vars_alloc
NULLIFY(top_pdm%slope_gb)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE top_pdm_nullify

END MODULE top_pdm
