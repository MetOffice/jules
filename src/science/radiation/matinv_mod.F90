MODULE matinv_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='MATINV_MOD'
CONTAINS
SUBROUTINE matinv(a,n)

! Description:
!   General routine to invert A(n,n) overwrites A as it goes along
!
! (c) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! ------------------------------------------------------------------------------
USE ereport_mod, ONLY: ereport

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

INTEGER, INTENT(IN) :: n                   ! dimension of matrix
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: a(n,n)
                                           ! matrix to be inverted

!work variables
INTEGER :: i, icol, irow, j, k             ! loop variables
INTEGER :: indxc(n), indxr(n), ipiv(n)     ! tracking arrays

REAL(KIND=real_jlslsm) :: big, pivinv, dum ! pivotting vals and tolerance
REAL(KIND=real_jlslsm), PARAMETER :: tol = 1.0e-6
REAL(KIND=real_jlslsm) :: dumr(1,n), dumc(n,1)
                                           ! temporary row/column copies

INTEGER ::              errcode            ! Error code
CHARACTER(LEN=80) :: ERRMSG             ! Error message text

CHARACTER(LEN=*), PARAMETER :: RoutineName='MATINV'

ipiv(:) = 0

DO k = 1, n                 !loop over columns
  big = 0.0               !big is largest element in ith row
  DO i = 1, n
    IF (ipiv(i) /= 1) THEN
      DO j = 1, n           !search over columns on ith row
        IF (ipiv(j) == 0) THEN
          IF (ABS(a(i,j)) >= big) THEN
            big = ABS(a(i,j))
            irow = i
            icol = j       !so pivotal element identified
          END IF
        ELSE IF (ipiv(j) > 1) THEN
          !one column can not pivot twice
          errcode = 1
          ERRMSG  = 'Singular matrix; one column can not pivot twice'
          CALL ereport ('matinv',errcode,ERRMSG)
        END IF
      END DO
    END IF
  END DO
  ipiv(icol) = ipiv(icol) + 1
  !pivot element identified at A(I,J) so now interchange rows
  !keep track of changes using indxr and indxc
  IF (irow /= icol) THEN
    DO j = 1, n
      dumr(1,j) = a(irow,j)
      a(irow,j) = a(icol,j)
      a(icol,j) = dumr(1,j)
    END DO
  END IF

  indxr(k) = irow      !where pivot is now located
  indxc(k) = icol      !where pivot was located

  IF (ABS(a(icol,icol)) <= tol ) THEN
    errcode = 2
    ERRMSG  = 'Close to singular matrix'
    CALL ereport ('matinv',errcode,ERRMSG)
  END IF

  !divide pivotal column by pivot element
  pivinv = 1.0 / a(icol,icol)
  a(icol,icol) = 1.0
  DO j = 1, n
    a(icol,j) = a(icol,j) * pivinv
  END DO

  !now elimate elements either side of A(icol,icol) by GE
  DO i = 1, n
    IF (i /= icol) THEN
      dum = a(i,icol)                      !big is dummy variable
      a(i,icol) = 0.0                      !just to make sure
      DO j = 1, n
        a(i,j) = a(i,j) - a(icol,j) * dum
      END DO
    END IF
  END DO
END DO

!Now need to rearrange columns after pivoting
DO k = n,1,-1
  IF (indxr(k) /= indxc(k)) THEN
    DO i = 1, n
      dumc(i,1) = a(i,indxr(k))
      a(i,indxr(k)) = a(i,indxc(k))
      a(i,indxc(k)) = dumc(i,1)
    END DO
  END IF
END DO

RETURN
END SUBROUTINE matinv
END MODULE matinv_mod
