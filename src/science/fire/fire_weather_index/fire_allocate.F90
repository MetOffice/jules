MODULE fire_allocate_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='FIRE_ALLOCATE_MOD'

CONTAINS

SUBROUTINE fire_allocate(land_index)

! < Module imports >
USE fire_mod,           ONLY: fire_cntl, fire_inis, fire_prog, fire_diag

USE theta_field_sizes,  ONLY: t_i_length

USE ancil_info,         ONLY: land_pts
USE model_grid_mod,     ONLY: latitude

USE parkind1,           ONLY: jprb, jpim
USE yomhook,            ONLY: lhook, dr_hook

IMPLICIT NONE
!
! Description:
!   Deals with setting up the module variables.
!
! Method:
!   Must be called before fire_calc_daily()
!   Sets flags for the metstats module
!   Allocates and sets initial values for fire variables
!   Only called by JULES-standalone. The UM does other stuff.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!

  !Arguments

INTEGER, INTENT(IN) :: land_index(land_pts)

  !Counters
INTEGER :: i,j,l

!Local variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='FIRE_ALLOCATE'

! End of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! ------------------------------------------
! Allocate fire module variables
ALLOCATE(fire_prog(1:land_pts))
ALLOCATE(fire_diag(1:land_pts))

IF (fire_cntl%mcarthur%flag) THEN
  ! Set initial values for diagnostic variables.
  ! Prognostics are done via get_default_ic_values
  fire_diag(:)%mcarthur%ffdi = fire_inis%mcarthur_ffdi
END IF

IF (fire_cntl%canadian%flag) THEN
  ! Set initial values for diagnostic variables.
  ! Prognostics are done via get_default_ic_values
  fire_diag(:)%canadian%isi = fire_inis%canadian_isi
  fire_diag(:)%canadian%bui = fire_inis%canadian_bui
  fire_diag(:)%canadian%fwi = fire_inis%canadian_fwi

  !Populate mask for the northern hemisphere
  DO l = 1,land_pts
    j = (land_index(l) - 1) / t_i_length + 1
    i = land_index(l) - (j-1) * t_i_length

    IF (latitude(i,j) >= 0.0) THEN
      fire_prog(l)%canadian%hemi_NtSf = .TRUE.
    ELSE
      fire_prog(l)%canadian%hemi_NtSf = .FALSE.
    END IF
  END DO !Land points

END IF

!Nothing to do for nesterov

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE fire_allocate
END MODULE fire_allocate_mod
