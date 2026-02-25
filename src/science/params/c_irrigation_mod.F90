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

END MODULE c_irrigation_mod
