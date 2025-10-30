! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Module containing variables required for aerosol calculations

MODULE aero

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='AERO'

REAL :: co2_mmr = 5.24100e-4  ! CO2 Mass Mixing Ratio

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
TYPE :: aero_data_type
  ! Contains LOGICALs, INTEGERs and REALs
  REAL, ALLOCATABLE :: co2_3d_ij(:,:)
                                    ! 3D CO2 field if required
  REAL, ALLOCATABLE :: rho_cd_modv1_ij(:,:)
                                    ! Surface air density * drag coef *
                                    ! mod(v1 - v0) before interp
  REAL, ALLOCATABLE :: rho_aresist_ij(:,:)
                                    ! RHOSTAR*CD_STD*VSHR for CLASSIC aerosol
                                    ! scheme
  REAL, ALLOCATABLE :: aresist_ij(:,:)
                                    ! 1/(CD_STD*VSHR) for CLASSIC aerosol
                                    ! scheme
  REAL, ALLOCATABLE :: resist_b_ij(:,:)
                                    ! (1/CH-1/(CD_STD)/VSHR for CLASSIC aerosol
                                    ! scheme
  REAL, ALLOCATABLE :: rho_aresist_surft(:,:)
                                    ! RHOSTAR*CD_STD*VSHR on land tiles for
                                    ! CLASSIC aerosol scheme
  REAL, ALLOCATABLE :: aresist_surft(:,:)
                                    ! 1/(CD_STD*VSHR) on land tiles for CLASSIC
                                    ! aerosol scheme
  REAL, ALLOCATABLE :: resist_b_surft(:,:)
                                    ! (1/CH-1/CD_STD)/VSHR on land tiles for
                                    ! CLASSIC aerosol scheme
  REAL, ALLOCATABLE :: r_b_dust_ij(:,:,:)
                                    ! surf layer res for mineral dust
  REAL, ALLOCATABLE :: cd_std_dust_ij(:,:)
                                    ! Bulk transfer coef. for momentum,
                                    ! excluding orographic effects
  REAL, ALLOCATABLE :: u_s_std_surft(:,:)
                                    ! Surface friction velocity (standard value)
END TYPE

!===============================================================================
TYPE :: aero_type
  ! Contains LOGICALs, INTEGERs and REALs
  REAL, POINTER :: co2_3d_ij(:,:)
  REAL, POINTER :: rho_cd_modv1_ij(:,:)
  REAL, POINTER :: rho_aresist_ij(:,:)
  REAL, POINTER :: aresist_ij(:,:)
  REAL, POINTER :: resist_b_ij(:,:)
  REAL, POINTER :: rho_aresist_surft(:,:)
  REAL, POINTER :: aresist_surft(:,:)
  REAL, POINTER :: resist_b_surft(:,:)
  REAL, POINTER :: r_b_dust_ij(:,:,:)
  REAL, POINTER :: cd_std_dust_ij(:,:)
  REAL, POINTER :: u_s_std_surft(:,:)
END TYPE

CONTAINS

SUBROUTINE aero_alloc(land_pts,t_i_length,t_j_length,                          &
                      nsurft,ndiv, aero_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: land_pts,t_i_length,t_j_length,                         &
                      nsurft,ndiv
TYPE(aero_data_type), INTENT(IN OUT) :: aero_data
!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='AERO_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

ALLOCATE(aero_data%co2_3d_ij(t_i_length,t_j_length))
ALLOCATE(aero_data%rho_cd_modv1_ij(t_i_length,t_j_length))
ALLOCATE(aero_data%rho_aresist_ij(t_i_length,t_j_length))
ALLOCATE(aero_data%aresist_ij(t_i_length,t_j_length))
ALLOCATE(aero_data%resist_b_ij(t_i_length,t_j_length))
ALLOCATE(aero_data%rho_aresist_surft(land_pts,nsurft))
ALLOCATE(aero_data%aresist_surft(land_pts,nsurft))
ALLOCATE(aero_data%resist_b_surft(land_pts,nsurft))
ALLOCATE(aero_data%r_b_dust_ij(t_i_length,t_j_length,ndiv))
ALLOCATE(aero_data%cd_std_dust_ij(t_i_length,t_j_length))
ALLOCATE(aero_data%u_s_std_surft(land_pts,nsurft))

aero_data%co2_3d_ij         = 0.0
aero_data%rho_cd_modv1_ij   = 0.0
aero_data%rho_aresist_ij    = 0.0
aero_data%aresist_ij        = 0.0
aero_data%resist_b_ij       = 0.0
aero_data%rho_aresist_surft = 0.0
aero_data%aresist_surft     = 0.0
aero_data%resist_b_surft    = 0.0
aero_data%r_b_dust_ij       = 0.0
aero_data%cd_std_dust_ij    = 0.0
aero_data%u_s_std_surft     = 0.0


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE aero_alloc

!===============================================================================
SUBROUTINE aero_dealloc(aero_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(aero_data_type), INTENT(IN OUT) :: aero_data

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='AERO_DEALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DEALLOCATE(aero_data%co2_3d_ij)
DEALLOCATE(aero_data%rho_cd_modv1_ij)
DEALLOCATE(aero_data%rho_aresist_ij)
DEALLOCATE(aero_data%aresist_ij)
DEALLOCATE(aero_data%resist_b_ij)
DEALLOCATE(aero_data%rho_aresist_surft)
DEALLOCATE(aero_data%aresist_surft)
DEALLOCATE(aero_data%resist_b_surft)
DEALLOCATE(aero_data%r_b_dust_ij)
DEALLOCATE(aero_data%cd_std_dust_ij)
DEALLOCATE(aero_data%u_s_std_surft)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE aero_dealloc

!===============================================================================
SUBROUTINE aero_assoc(aero,aero_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(aero_data_type), TARGET, INTENT(IN OUT) :: aero_data
  ! Instance of the data type we are associtating to
TYPE(aero_type), INTENT(IN OUT) :: aero
  ! Instance of the pointer type we are associating

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='AERO_ASSOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL aero_nullify(aero)

aero%co2_3d_ij => aero_data%co2_3d_ij
aero%rho_cd_modv1_ij => aero_data%rho_cd_modv1_ij
aero%rho_aresist_ij => aero_data%rho_aresist_ij
aero%aresist_ij => aero_data%aresist_ij
aero%resist_b_ij => aero_data%resist_b_ij
aero%rho_aresist_surft => aero_data%rho_aresist_surft
aero%aresist_surft => aero_data%aresist_surft
aero%resist_b_surft => aero_data%resist_b_surft
aero%r_b_dust_ij => aero_data%r_b_dust_ij
aero%cd_std_dust_ij => aero_data%cd_std_dust_ij
aero%u_s_std_surft => aero_data%u_s_std_surft

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE aero_assoc

!===============================================================================
SUBROUTINE aero_nullify(aero)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(aero_type), INTENT(IN OUT) :: aero
  ! Instance of the pointer type we are associating

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='AERO_NULLIFY'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

NULLIFY(aero%co2_3d_ij)
NULLIFY(aero%rho_cd_modv1_ij)
NULLIFY(aero%rho_aresist_ij)
NULLIFY(aero%aresist_ij)
NULLIFY(aero%resist_b_ij)
NULLIFY(aero%rho_aresist_surft)
NULLIFY(aero%aresist_surft)
NULLIFY(aero%resist_b_surft)
NULLIFY(aero%r_b_dust_ij)
NULLIFY(aero%cd_std_dust_ij)
NULLIFY(aero%u_s_std_surft)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE aero_nullify

!-------------------------------------------------------------------------------

END MODULE aero
