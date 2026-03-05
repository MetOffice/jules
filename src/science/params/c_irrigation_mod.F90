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
irrig_tile(:) = 0

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE c_irrigation_alloc

SUBROUTINE check_irrigation()

USE jules_surface_types_mod, ONLY: c3_irrig, c4_irrig, ntype
USE logging_mod, ONLY: log_info, log_warn, log_error, log_fatal

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
! ----------------------------------------------------------------------------
ERROR = 0
DO i = 1, ntype
    IF ((irrig_tile(i) == 0) .or. (irrig_tile(i) == 1)) THEN
        ! Do nothing, the values of irrig_tile are 0 or 1
    ELSE
        ERROR = 1
        CALL log_fatal(routinename, "Incorrect values entered for " // &
                        "irrig_pft, allowed values either 1 or 0")
    END IF
END DO
! ----------------------------------------------------------------------------
! Check to ensure that c3_irrig and c4_irrg tiles are the only ones to have 
! irrig_tile == 1
! ----------------------------------------------------------------------------
ERROR = 0

DO i = 1, ntype
  IF (irrig_tile(i) == 1 .AND. (i /= c3_irrig .AND. i /= c4_irrig)) THEN
      ! Can only irrigate c3_irrig and c4_irrig tiles
      ! Generate error if any other tile is selected
      ERROR = 1
      CALL log_fatal(routinename, "Selected tiles cannot be irrigated," // &
                     "you can only select c3_irrig and c4_irrig")
  ELSE IF (irrig_tile(i) == 1) THEN
      CALL log_info(routinename, "Using tiles c3_irrig and/or c4_irrig")
  END IF
END DO

RETURN
END SUBROUTINE check_irrigation


END MODULE c_irrigation_mod
