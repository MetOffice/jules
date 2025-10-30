#if !defined(UM_JULES)

! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!

MODULE vectlib_mod

! Description:
! This routine acts as an interface to vector versions
! of intrinsics functions on a platform.
!
! Supported libraries:
! IBM's VMASS (compile using VMASS def)
! Intel's MKL (compile with MKL def)
!
! Default compiles to equivalent do loop over array
!
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
! This file belongs in section: Misc

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='VECTLIB_MOD'

CONTAINS

SUBROUTINE exp_v(n,x,y)
USE um_types, ONLY: real_64, integer_32
IMPLICIT NONE

! Sets y(i) to the exponential function of x(i), for i=1,..,n

REAL (KIND=real_64) :: y(n), x(n)
INTEGER :: n
INTEGER (KIND=integer_32) :: l_n
INTEGER :: i

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='EXP_V'

#if defined(MKL)
! Interfaces for MKL
INCLUDE 'mkl_vml.f90'
#endif

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
l_n = n

#if defined(VMASS)
CALL vexp (y, x, l_n)

#elif defined(MKL)
CALL vdexp(n, x, y)

#else
DO i = 1, n
  y(i) = EXP(x(i))
END DO
#endif

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE exp_v

!-----------------------------------------------------------

SUBROUTINE powr_v(n, x, power, z)
USE um_types, ONLY: real_64, integer_32
IMPLICIT NONE

! Sets z(i) to x(i) raised to the power y(i), for i=1,..,n

REAL (KIND=real_64) :: z(n), x(n), y(n), power
INTEGER :: n, i
INTEGER (KIND=integer_32) :: l_n

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='POWR_V'

#if defined(MKL)
! Interfaces for MKL
INCLUDE 'mkl_vml.f90'
#endif

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
l_n = n

#if defined(VMASS)
DO i = 1, n
  y(i) = power
END DO

CALL vpow (z, x, y, l_n)

#elif defined(MKL)
CALL vdpowx(n, x, power, y)

#else
DO i = 1, n
  z(i) = x(i)**power
END DO
#endif

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE powr_v

!-----------------------------------------------------------

SUBROUTINE rtor_v(n, x, y, z)
USE um_types, ONLY: real_64, integer_32
IMPLICIT NONE

! Sets z(i) to x(i) raised to the power y(i), for i=1,..,n

REAL (KIND=real_64) :: z(n), x(n), y(n)
INTEGER :: n
INTEGER (KIND=integer_32) :: l_n
INTEGER :: i

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='RTOR_V'

#if defined(MKL)
! Interfaces for MKL
INCLUDE 'mkl_vml.f90'
#endif

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
l_n = n

#if defined(VMASS)
CALL vpow (z, x, y, l_n)

#elif defined(MKL)
CALL vdpow(n, x, y, z)

#else
DO i = 1, n
  z(i) = x(i)**y(i)
END DO
#endif

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE rtor_v

!-----------------------------------------------------------

SUBROUTINE sqrt_v(n, x, y)
USE um_types, ONLY: real_64, integer_32
IMPLICIT NONE

! Sets y(i) to the square root of x(i), for i=1,..,n

REAL (KIND=real_64) :: x(n), y(n)
INTEGER :: n
INTEGER (KIND=integer_32) :: l_n
INTEGER :: i

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SQRT_V'

#if defined(MKL)
! Interfaces for MKL
INCLUDE 'mkl_vml.f90'
#endif

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
l_n = n


#if defined(VMASS)
CALL vsqrt (y, x, l_n)

#elif defined(MKL)
CALL vdsqrt(n, x, y)

#else
DO i = 1, n
  y(i) = SQRT(x(i))
END DO
#endif

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE sqrt_v

!-----------------------------------------------------------

SUBROUTINE oneover_v(n, x, y)
USE um_types, ONLY: real_64, integer_32
IMPLICIT NONE

! Sets y(i) to the reciprocal of x(i), for i=1,..,n

REAL (KIND=real_64) :: x(n), y(n)
INTEGER :: n
INTEGER (KIND=integer_32) :: l_n
INTEGER :: i

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='ONEOVER_V'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
l_n = n

#if defined(VMASS)
CALL vrec (y, x, l_n)

#else
DO i = 1, n
  y(i) = 1 / x(i)
END DO
#endif
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE oneover_v

!-----------------------------------------------------------

SUBROUTINE log_v (n, x, y)
USE um_types, ONLY: real_64, integer_32
IMPLICIT NONE

! Sets y(i) to the natural logarithm of x(i), for i=1,..,n

REAL (KIND=real_64) :: x(n), y(n)
INTEGER :: n
INTEGER (KIND=integer_32) :: l_n
INTEGER :: i

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='LOG_V'

#if defined(MKL)
! Interfaces for MKL
INCLUDE 'mkl_vml.f90'
#endif

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
l_n = n

#if defined(VMASS)
CALL vlog (y, x, l_n)

#elif defined(MKL)
CALL vdln( n, x, y )

#else
DO i = 1, n
  y(i) = LOG(x(i))
END DO
#endif

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE log_v

!-----------------------------------------------------------

SUBROUTINE sin_v(n,x,y)
USE um_types, ONLY: real_64, integer_32
IMPLICIT NONE

! Sets y(i) to the sin function of x(i), for i=1,..,n

REAL (KIND=real_64) :: y(n), x(n)
INTEGER :: n
INTEGER (KIND=integer_32) :: l_n
INTEGER :: i

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SIN_V'

#if defined(MKL)
! Interfaces for MKL
INCLUDE 'mkl_vml.f90'
#endif

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
l_n = n

#if defined(VMASS)
CALL vsin (y, x, l_n)

#elif defined(MKL)
CALL vdsin(n, x, y)

#else
DO i = 1, n
  y(i) = SIN(x(i))
END DO
#endif

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE sin_v

!-----------------------------------------------------------

SUBROUTINE cos_v(n,x,y)
USE um_types, ONLY: real_64, integer_32
IMPLICIT NONE

! Sets y(i) to the cos function of x(i), for i=1,..,n

REAL (KIND=real_64) :: y(n), x(n)
INTEGER :: n
INTEGER (KIND=integer_32) :: l_n
INTEGER :: i

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='COS_V'

#if defined(MKL)
! Interfaces for MKL
INCLUDE 'mkl_vml.f90'
#endif

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
l_n = n

#if defined(VMASS)
CALL vcos (y, x, l_n)

#elif defined(MKL)
CALL vdcos(n, x, y)

#else
DO i = 1, n
  y(i) = COS(x(i))
END DO
#endif

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE cos_v

!-----------------------------------------------------------
SUBROUTINE acos_v(n,x,y)
USE um_types, ONLY: real_64, integer_32
IMPLICIT NONE

! Sets y(i) to the cos function of x(i), for i=1,..,n

REAL (KIND=real_64) :: y(n), x(n)
INTEGER :: n
INTEGER (KIND=integer_32) :: l_n
INTEGER :: i

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='ACOS_V'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
l_n = n

#if defined(VMASS)
CALL vacos (y, x, l_n)
#else
DO i = 1, n
  y(i) = ACOS(x(i))
END DO
#endif

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE acos_v

!-----------------------------------------------------------

SUBROUTINE asin_v(n,x,y)
USE um_types, ONLY: real_64, integer_32
IMPLICIT NONE

! Sets y(i) to the asin function of x(i), for i=1,..,n

REAL (KIND=real_64) :: y(n), x(n)
INTEGER :: n
INTEGER (KIND=integer_32) :: l_n
INTEGER :: i

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='ASIN_V'

#if defined(MKL)
! Interfaces for MKL
INCLUDE 'mkl_vml.f90'
#endif

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
l_n = n

#if defined(VMASS)
CALL vasin (y, x, l_n)

#elif defined(MKL)
CALL vdasin(n, x, y)

#else
DO i = 1, n
  y(i) = ASIN(x(i))
END DO
#endif

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE asin_v

!-----------------------------------------------------------

END MODULE vectlib_mod

#endif
