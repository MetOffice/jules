! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module with setting of
! irrigation switch

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.

MODULE c_irrigation_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

INTEGER, ALLOCATABLE ::                                                        &
  irrig_tile(:)         ! Switch to indicate irrigation for each tile
!                         0 - Not irrigated
!                         1 - Irrigated


CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='C_IRRIGATION'

CONTAINS

SUBROUTINE c_irrigation_alloc(ntype)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook
USE missing_data_mod, ONLY: imdi

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: ntype

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='C_IRRIGATION_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

ALLOCATE( irrig_tile(ntype))
! Set irrig_tile to missing data so that it is only set to a value when the
! irrigation tile scheme is switched on (irrig_option = 2)
irrig_tile(:) = imdi

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE c_irrigation_alloc

SUBROUTINE check_irrigation()

USE jules_surface_types_mod, ONLY: c3_irrig, c4_irrig, ntype, npft
USE logging_mod, ONLY: log_info, log_warn, log_error, log_fatal
USE missing_data_mod, ONLY: imdi

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Checks to irrig_tile variable
!-----------------------------------------------------------------------------

! Work variables
INTEGER :: ERROR  ! Error indicator
INTEGER :: i      ! Loop counter.

CHARACTER(LEN=*), PARAMETER :: routinename='CHECK_IRRIGATION'

! ----------------------------------------------------------------------------
! Check to ensure that the values are the allowed values i.e. either 1 or 0
! and check to ensure that c3_irrig and c4_irrig tiles are the only ones to have
! irrig_tile == 1
! ----------------------------------------------------------------------------
ERROR = 0
IF (ANY(irrig_tile(:) /= imdi)) THEN
  IF (ALL((irrig_tile(1:npft) == 0) .OR. (irrig_tile(1:npft) == 1))) THEN
    CALL log_info(routinename, "irrig_pft has allowed values i.e. 1 and 0")
  ELSE
    ERROR = 1
    CALL log_fatal(routinename, "Incorrect values entered for " //             &
                    "irrig_pft, allowed values either 1 or 0")
  END IF
  
  DO i = 1, ntype
    IF (irrig_tile(i) == 1 .AND. (i /= c3_irrig .AND. i /= c4_irrig)) THEN
        ! Can only irrigate c3_irrig and c4_irrig tiles
        ! Generate error if any other tile is selected
      ERROR = 1
      CALL log_fatal(routinename, "Selected tiles cannot be irrigated, " //       &
                    "can only select c3_irrig and c4_irrig")
    ELSE IF (irrig_tile(i) == 1) THEN
      CALL log_info(routinename, "Using tiles c3_irrig and/or c4_irrig")
    END IF
  END DO
ELSE
  CALL log_info(routinename, "Irrigation is not being applied to surface types")
END IF

RETURN
END SUBROUTINE check_irrigation


END MODULE c_irrigation_mod
