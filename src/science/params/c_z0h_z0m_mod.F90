! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module with setting of
! ratio of roughness lengths

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.

MODULE c_z0h_z0m

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
  z0h_z0m(:)            ! Ratio of roughness length for heat
!                         to roughness length for momentum
!                         for each surface type.

REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
  z0h_z0m_classic(:)    ! Ratio of roughness length for classic
!                         aerosol depostion
!                         to roughness length for momentum
!                         for each surface type.

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='C_Z0H_Z0M'

CONTAINS

SUBROUTINE c_z0h_z0m_alloc(ntype)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: ntype

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='C_Z0H_Z0M_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

ALLOCATE( z0h_z0m(ntype))
ALLOCATE( z0h_z0m_classic(ntype))
z0h_z0m(:)         = 0.0
z0h_z0m_classic(:) = 0.0

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE c_z0h_z0m_alloc


SUBROUTINE c_z0h_z0m_check(ntype)

USE ereport_mod, ONLY: ereport
USE check_jules_nml_values_mod, ONLY: check_jules_nml_values

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: ntype

! Work variables
INTEGER :: errorstatus = 0
CHARACTER(LEN=*), PARAMETER :: RoutineName='C_Z0H_Z0M_CHECK'

IF ( SIZE( z0h_z0m(:) ) > 0 )                                                  &
   CALL check_jules_nml_values ( z0h_z0m(:), 'z0h_z0m', ntype,                 &
   0.0, HUGE(1.0), RoutineName, errorstatus )
IF ( SIZE( z0h_z0m_classic(:) ) > 0 )                                          &
   CALL check_jules_nml_values ( z0h_z0m_classic(:),                           &
   'z0h_z0m_classic', ntype, 0.0, HUGE(1.0), RoutineName, errorstatus )

IF ( errorstatus > 0 )                                                         &
   CALL ereport(RoutineName, errorstatus,                                      &
   ' Error(s) were found in c_z0h_z0m - see job.out for details')

END SUBROUTINE c_z0h_z0m_check


SUBROUTINE c_z0h_z0m_print()

USE jules_print_mgr, ONLY: jules_print

IMPLICIT NONE
CHARACTER(LEN=50000) :: lineBuffer
CHARACTER(LEN=*), PARAMETER :: RoutineName='C_Z0H_Z0M_PRINT'

CALL jules_print(RoutineName, 'Contents of c_z0h_z0m')

WRITE(lineBuffer,*)' z0h_z0m_classic = ',z0h_z0m_classic(:)
CALL jules_print(RoutineName,lineBuffer)
WRITE(lineBuffer,*)' z0h_z0m = ',z0h_z0m(:)
CALL jules_print(RoutineName,lineBuffer)

CALL jules_print(RoutineName,                                                 &
    '- - - - - - end of c_z0h_z0m - - - - - -')

END SUBROUTINE c_z0h_z0m_print

END MODULE c_z0h_z0m
