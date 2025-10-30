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
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: River Routing
MODULE wrtwblog_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='WRTWBLOG_MOD'

CONTAINS



SUBROUTINE wrtWBlog(iy, im, id, ih, ndev                                       &
     , stoall, sto2all, dinall, doutall, drunall, drivall                      &
     , dt)

USE jules_print_mgr, ONLY:                                                     &
   jules_print, jules_message,                                                 &
   PrintStatus, PrDiag
USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
IMPLICIT NONE
!
!     write water balance monitoring
!
!     stoall    : Total river channel storage
!     sto2all   : Total river channel storage at the next time step
!     dinall    : Total inflow to the next grid for the next time step
!     doutall   : Total outflow to the next grid for the next time step
!   [ drunall*dt  : Total inflow to the grid from LSM]
!   [ drivall*dt  : Total inflow to the grid from surrounding grids]
!   [(stoall - sto2all + drunall*dt) / (135.3e12) : Mean runoff to the
!                                                   sea
INTEGER :: iy, im, id, ih, ndev
REAL(KIND=real_jlslsm) :: rorder, stoall, sto2all, dinall, doutall             &
     , drunall, drivall, dt

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='WRTWBLOG'


DATA rorder / 1.0e12 /
!
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,                 &
                        zhook_handle)

IF (PrintStatus >= PrDiag) THEN
  CALL jules_print('wrtwblog',                                                 &
       ' River Routing: ' //                                                   &
       'Error = Boxinflow - (sum of river inflow + box runoff)')

  WRITE(jules_message, '(3A)') '         YY/MM/DD/HH ', '  S(t)  S(t+1)'       &
       , '     Din    Dout   Run  Rivflow  Error  Outflow(mm/y) '
  CALL jules_print('wrtwblog',jules_message)

  WRITE(jules_message, '(7X,I2,A,I2,A,I2,A,I2,8(F7.1))')                       &
      iy, "/", im, "/", id, "/", ih,                                           &
      stoall / rorder, sto2all / rorder, dinall / rorder, doutall / rorder     &
      , (drunall * dt) / rorder, (drivall * dt) / rorder                       &
      , (dinall - (drivall + drunall) * dt), (stoall - sto2all +               &
      drunall * dt) / (135.3e12) * 365.0 * REAL(ndev)
  CALL jules_print('wrtwblog',jules_message)

  WRITE(jules_message, '(2A,E9.2,A,E9.2,A)')                                   &
      "10^12 kg: ",  " Total outflow to sea or inland points is ",             &
      (stoall - sto2all + drunall * dt) *  REAL(ndev) / 86400, ' Kg/s or ',    &
      (stoall - sto2all + drunall * dt) / (135.3e12) *  REAL(ndev), ' mm/day'
  CALL jules_print('wrtwblog',jules_message)

  CALL jules_print('wrtwblog',' River routing: Water balance monitoring')
  WRITE(jules_message, '(2A)')                                                 &
       ' (Tot water in - tot water out - diff. water',                         &
       ' storage) summed for all gridboxes'
  CALL jules_print('wrtwblog',jules_message)

  WRITE(jules_message, '(3A)') '          YY/MM/DD/HH ',                       &
       '  S(t)  S(t+1)', '     Din    Dout   Din-Dout-(S(t+1)- S(t))Kg'
  CALL jules_print('wrtwblog',jules_message)

  WRITE(jules_message, '(A,I2,A,I2,A,I2,A,I2,5(1X,F7.1))')                     &
       "10^12 kg: ", iy, "/", im, "/", id, "/", ih,                            &
       stoall / rorder, sto2all / rorder, dinall / rorder, doutall / rorder    &
       , (dinall - doutall -(sto2all - stoall))
  CALL jules_print('wrtwblog',jules_message)
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,                &
                        zhook_handle)
RETURN

END SUBROUTINE wrtWBlog

END MODULE wrtwblog_mod
#endif
