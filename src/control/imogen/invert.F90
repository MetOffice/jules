#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************
SUBROUTINE invert(                                                             &
  u_old,u_new,p,lambda_old,lambda_new,r1,r2,dz_top,n_olevs                     &
)

USE logging_mod, ONLY: log_fatal

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This routine is designed to invert a tri-diagonal matrix and then test
!     that all is OK.
!   The matrix is of the form ``A u_new = B u_old + k'' Matrix A and B are
!     tri-diagonal
!   The solution is du/dz - pu = lambda
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

INTEGER ::                                                                     &
  n_olevs  !IN Number of layers in main routine above

REAL ::                                                                        &
  p,                                                                           &
           !IN P Value in mixed-boundary condition
  lambda_old,                                                                  &
           !IN Lambda value at old timestep
  lambda_new,                                                                  &
           !IN Lambda value at new timestep
  r1(n_olevs),                                                                 &
           !IN Mesh ratio
  r2(n_olevs),                                                                 &
           !IN Mesh ratio
  dz_top,                                                                      &
           !IN Top layer mesh size
  factor,                                                                      &
           !WORK Local dummy variable
  dummy1,                                                                      &
           !WORK Local dummy variable
  dummy2   !WORK Local dummy variable

REAL ::                                                                        &
  a(n_olevs,n_olevs),                                                          &
           !WORK Matrix A
  b(n_olevs,n_olevs),                                                          &
           !WORK Matrix B
  k(n_olevs),                                                                  &
           !WORK Matrix K
  u_old(1:n_olevs),                                                            &
           !IN Old values of U
  u_new(1:n_olevs)
           !OUT New values of U

REAL ::                                                                        &
  a_l(n_olevs,n_olevs),                                                        &
           !WORK A local working value of A - values ma
  f_l(n_olevs)
           !WORK Local working value of ``B u_old + k''

INTEGER :: i,j     !WORK Looping parameters



! Set everything to zero in the first instance
a(:,:) = 0.0
b(:,:) = 0.0
a_l(:,:) = 0.0
f_l(:) = 0.0
k(:) = 0.0


! Evaluate the values of A,B,K for the particular use here.
a(1,1) = (1.0 + r1(1)) + r1(1) * dz_top * p
a(1,2) = -r1(1)

b(1,1) = (1.0 - r1(1)) - r1(1) * dz_top * p
b(1,2) = r1(1)

k(1)   = -r1(1) * dz_top * (lambda_old + lambda_new)

DO i = 2,n_olevs-1
  a(i,i-1) = -r1(i)
  a(i,i)   = 1.0 + r1(i) + r2(i)
  a(i,i+1) = -r2(i)

  b(i,i-1) = r1(i)
  b(i,i)   = 1.0 - r1(i) - r2(i)
  b(i,i+1) = r2(i)
END DO

a(n_olevs,n_olevs-1) = -r1(n_olevs)
a(n_olevs,n_olevs)   = 1.0 + r1(n_olevs) + r2(n_olevs)

b(n_olevs,n_olevs-1) = r1(n_olevs)
b(n_olevs,n_olevs)   = 1.0 - r1(n_olevs) - r2(n_olevs)


! First evaluate F_L (which is initially `` B u_old + k'')
f_l(1) = b(1,1) * u_old(1) + b(1,2) * u_old(2) + k(1)

DO i = 2,n_olevs-1
  f_l(i) = b(i,i-1) * u_old(i-1) + b(i,i) * u_old(i)                           &
         + b(i,i+1) * u_old(i+1) + k(i)
END DO

f_l(n_olevs) = b(n_olevs,n_olevs-1) * u_old(n_olevs-1)                         &
             + b(n_olevs,n_olevs) * u_old(n_olevs) + k(n_olevs)


! Now set A_L = A
a_l(1,1) = a(1,1)
a_l(1,2) = a(1,2)

DO i = 2,n_olevs-1
  a_l(i,i-1) = a(i,i-1)
  a_l(i,i)   = a(i,i)
  a_l(i,i+1) = a(i,i+1)
END DO

a_l(n_olevs,n_olevs-1) = a(n_olevs,n_olevs-1)
a_l(n_olevs,n_olevs)   = a(n_olevs,n_olevs)

! Now go through loops working back to find U_NEW(1)
factor = a_l(n_olevs-1,n_olevs) / a_l(n_olevs,n_olevs)
a_l(n_olevs,n_olevs-1) = a_l(n_olevs,n_olevs-1) * factor
a_l(n_olevs,n_olevs) = a_l(n_olevs-1,n_olevs)
f_l(n_olevs) = f_l(n_olevs) * factor

a_l(n_olevs-1,n_olevs-1) = a_l(n_olevs-1,n_olevs-1)                            &
                         - a_l(n_olevs,n_olevs-1)
a_l(n_olevs-1,n_olevs) = 0.0
f_l(n_olevs-1) = f_l(n_olevs-1) - f_l(n_olevs)

DO i = n_olevs - 1,2,-1
  factor = a_l(i-1,i) / a_l(i,i)
  a_l(i,i-1) = a_l(i,i-1) * factor
  a_l(i,i) = a_l(i-1,i)
  f_l(i) = f_l(i) * factor

  a_l(i-1,i-1) = a_l(i-1,i-1) - a_l(i,i-1)
  a_l(i-1,i) = 0.0
  f_l(i-1) = f_l(i-1) - f_l(i)
END DO

! Now explicitly calculate the U_NEW values
u_new(1) = f_l(1) / a_l(1,1)

DO i = 2,n_olevs
  u_new(i) = (f_l(i) - (a_l(i,i-1) * u_new(i-1))) / a_l(i,i)
END DO

! Now perform a check
DO i = 1,n_olevs
  dummy1 = 0.0
  dummy2 = 0.0
  DO j = 1,n_olevs
    dummy1 = dummy1 + a(i,j) * u_new(j)
    dummy2 = dummy2 + b(i,j) * u_old(j)
  END DO
  dummy2 = dummy2 + k(i)

  IF (ABS(dummy1 - dummy2) >= 0.0001) THEN
    CALL log_fatal("INVERT", "Invert unsuccessful")
  END IF
END DO

RETURN
END SUBROUTINE invert
#endif
