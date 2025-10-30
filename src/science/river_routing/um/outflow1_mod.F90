#if defined(UM_JULES)
!Huw Lewis (MO), Jan 2015
!DEPRECATED CODE
!This code was transferred from the UM repository at UM vn9.2 / JULES vn 4.1.
!Future developments will supercede these subroutines, and as such they
!should be considered deprecated. They will be retained in the codebase to
!maintain backward compatibility with functionality prior to
!UM vn10.0 / JULES vn 4.2, until such time as they become redundant.
!
!
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Code Owner: Please refer to the UM file CodeOwners.txt
!  This file belongs in section: River Routing
!
MODULE outflow1_mod

USE umPrintMgr, ONLY:                                                          &
    umPrint,                                                                   &
    umMessage
USE mpp_conf_mod, ONLY: swap_field_is_scalar
USE halo_exchange, ONLY: swap_bounds

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='OUTFLOW1_MOD'

CONTAINS

SUBROUTINE outflow1(sto, drunin, rc, dt                                        &
, igrcn, iseq, inextx, inexty, nseqmax, nx, ny, jmax                           &
, sto2, din, dout, drunall, drivall, stoall, sto2all                           &
, doutall, dinall, halo_x, halo_y, nseqlocmax)

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
USE Field_Types, ONLY: fld_type_r

IMPLICIT NONE

!
!  Calculate the storage in the next time step based on the storage, sto,
!  of current time step
!
!  Runoff generated at the grid box is constant during the time step
!  transfer coefficient is also given; rc
!  For 'runoff drunin' in TRIP seapoints just add to TRIP seapoint
!
INTEGER, INTENT(IN) ::                                                         &
 nx                                                                            &
    ! number of columns
, ny                                                                           &
    ! number of rows
, nseqmax                                                                      &
    ! max river sequence globally (needs to be global to in order
    ! to synchronise swap bounds)
, halo_x                                                                       &
    ! halo depth in lat
, halo_y                                                                       &
    ! halo depth in long
, inextx(1 - halo_x:nx + halo_x, 1 - halo_y:ny + halo_y)                       &
    ! x point to advect for a given point
, inexty(1 - halo_x:nx + halo_x, 1 - halo_y:ny + halo_y)                       &
    ! y point to advect for a give y point
, igrcn(1 - halo_x:nx + halo_x, 1 - halo_y:ny + halo_y), jmax                  &
    ! river flow direction
, iseq(1 - halo_x:nx + halo_x, 1 - halo_y:ny + halo_y), nseqlocmax
    ! river sequence

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
     sto(1 - halo_x:nx + halo_x, 1 - halo_y:ny + halo_y)
    ! river storage (t)

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 sto2(1 - halo_x:nx + halo_x, 1 - halo_y:ny + halo_y)                          &
    ! river storage (t+dt)
, din(1 - halo_x:nx + halo_x, 1 - halo_y:ny + halo_y)                          &
    ! river inflow
, dout(1 - halo_x:nx + halo_x, 1 - halo_y:ny + halo_y)                         &
    ! river outflow
, drunall, drivall, stoall, sto2all, doutall, dinall
    ! river flow sums

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 drunin(1 - halo_x:nx + halo_x, 1 - halo_y:ny + halo_y)                        &
    ! runoff
, rc(1 - halo_x:nx + halo_x, 1 - halo_y:ny + halo_y), dt
    ! effective velocity

! local variables

INTEGER :: i, j, nseq
! loop counters

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='OUTFLOW1'

!
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,                 &
                        zhook_handle)

din = 0.0
drivall = 0.0
drunall = 0.0
stoall = 0.0
dinall = 0.0
sto2all = 0.0
doutall = 0.0
sto2 = 0.0

! for each sequence advect the companion river elements
DO nseq = 1, nseqmax

  IF (nseq <= nseqlocmax) THEN
    DO j = 1 - (halo_y-1), jmax + (halo_y-1)
      DO i = 1 - (halo_x-1), nx + (halo_x-1)
        IF (iseq(i,j) == nseq) THEN
          din(i,j) = din(i,j) + drunin(i,j)

          IF ( (i >= 1) .AND. (i <= nx) .AND. (j >= 1) .AND.                   &
                  (j <= jmax) ) drunall = drunall + drunin(i,j)

          IF (rc(i, j) >  0.0) THEN

            sto2(i, j) = sto(i,j) * EXP(-(rc(i,j) * dt))                       &
                 + (1.0 - EXP(-(rc(i,j) * dt))) * din(i,j) / rc(i,j)
            dout(i, j) = (sto(i,j) - sto2(i,j)) / dt + din(i,j)

            ! exclude halos
            IF ( (i >= 1) .AND. (i <= nx) .AND. (j >= 1) .AND.                 &
                  (j <= jmax) ) THEN

              stoall = stoall + sto(i,j)
              sto2all = sto2all + sto2(i,j)
              doutall = doutall + dout(i,j) * dt
              dinall  = dinall  + din(i,j) * dt

            END IF

            ! no contributions more than 2 cells away.
            IF ((igrcn(i,j) >= 1) .AND. (igrcn(i,j) <= 8) .AND.                &
                 ((ABS(inextx(i,j) - i) <= 1)  .AND.  (ABS(                    &
                 inexty(i,j) - j) <= 1)  ) ) THEN

              din(inextx(i,j),inexty(i,j))                                     &
                     = din(inextx(i,j),inexty(i,j)) + dout(i,j)

              ! exclude halos
              IF ( (i >= 1) .AND. (i <= nx) .AND. (j >= 1) .AND.               &
                    (j <= jmax) ) drivall = drivall + dout(i,j)

            END IF
            !
          ELSE
            WRITE(umMessage, '(A,2X,2(I3,2X),2X,F6.2)')                        &
                 '!! rc is not positive ', i, j,rc(i,j)
            CALL umPrint(umMessage,src='outflow1')
          END IF
        END IF
      END DO
    END DO

  END IF

  ! swap bound to pull in halos that potentially have been advected into
  CALL swap_bounds(din, nx, ny, 1, halo_x, halo_y,                             &
        fld_type_r, swap_field_is_scalar)

END DO
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,                &
                        zhook_handle)
RETURN

END SUBROUTINE outflow1

END MODULE outflow1_mod
#endif
