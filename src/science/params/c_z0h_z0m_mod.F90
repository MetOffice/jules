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

USE missing_data_mod, ONLY: rmdi

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
z0h_z0m(:)         = rmdi
z0h_z0m_classic(:) = rmdi

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE c_z0h_z0m_alloc

#if !defined(RIVERS_ONLY)
SUBROUTINE c_z0h_z0m_check(ntype)

USE ereport_mod, ONLY: ereport
USE check_jules_nml_values_mod, ONLY: check_jules_nml_values

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: ntype

! Work variables
INTEGER :: errorstatus = 0
CHARACTER(LEN=*), PARAMETER :: RoutineName='C_Z0H_Z0M_CHECK'

IF ( SIZE( z0h_z0m_classic(:) ) > 0 )                                          &
   CALL check_jules_nml_values ( z0h_z0m_classic(:),                           &
   'z0h_z0m_classic', ntype, 0.0, HUGE(1.0), RoutineName, errorstatus )
IF ( SIZE( z0h_z0m(:) ) > 0 )                                                  &
   CALL check_jules_nml_values ( z0h_z0m(:), 'z0h_z0m', ntype,                 &
   0.0, HUGE(1.0), RoutineName, errorstatus )

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

CALL jules_print(RoutineName,                                                  &
    '- - - - - - end of c_z0h_z0m - - - - - -')

END SUBROUTINE c_z0h_z0m_print


SUBROUTINE c_z0h_z0m_bcast(ntype)

USE setup_namelist, ONLY: setup_nml_type
USE UM_parcore,     ONLY: mype
USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
USE errormessagelength_mod, ONLY: errormessagelength
USE max_dimensions, ONLY: ntype_max

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: ntype

! Local variables
INTEGER :: my_comm
INTEGER :: mpl_nml_type
INTEGER :: icode

CHARACTER(LEN=*), PARAMETER :: RoutineName='C_Z0H_Z0M_BCAST'
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle
CHARACTER(LEN=errormessagelength) :: iomessage

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 1
INTEGER, PARAMETER :: n_real = 2 * ntype_max

TYPE :: my_namelist
   SEQUENCE
   REAL(KIND=real_jlslsm) :: z0h_z0m_classic(ntype_max)
   REAL(KIND=real_jlslsm) :: z0h_z0m(ntype_max)
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type, n_real_in = n_real)

IF ( mype == 0 ) THEN
  my_nml % z0h_z0m_classic(1:ntype) = z0h_z0m_classic(:)
  my_nml % z0h_z0m(1:ntype)         = z0h_z0m(:)
END IF

CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN
  z0h_z0m_classic(:) = my_nml % z0h_z0m_classic(1:ntype)
  z0h_z0m(:)         = my_nml % z0h_z0m(1:ntype)
END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

 END SUBROUTINE c_z0h_z0m_bcast
#endif

END MODULE c_z0h_z0m
