! *****************************COPYRIGHT*******************************
! (c) Crown copyright, Met Office, All Rights Reserved.
! Please refer to file $UMDIR/vn$VN/copyright.txt for further details
! *****************************COPYRIGHT*******************************
MODULE dewpnt_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='DEWPNT_MOD'

CONTAINS

!    L  SUBROUTINE DEWPNT-------------------------------------------------

!    Purpose: Calculates the 1.5 metre dewpoint from 1.5 metre specific
!             humidity, 1.5 metre temperature and 1.5 metre pressure.

!    Suitable for single column usage.

!  Arguments:----------------------------------------------------------
SUBROUTINE dewpnt(q,p,t,p_field,td)

USE water_constants_mod, ONLY: lc, tm
USE planet_constants_mod, ONLY: repsilon, rv

USE qsat_mod, ONLY: qsat_wat

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

INTEGER :: p_field         ! IN Size of field arrays.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 p(p_field)                                                                    &
                        ! IN Pressure.
,q(p_field)                                                                    &
                        ! IN Specific humidity.
,t(p_field)             ! IN Temperature.

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 td(p_field)            ! OUT Dew point.


!----------------------------------------------------------------------
! Workspace - local variables
!----------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
 rl1                                                                           &
                        ! LOCAL Latent heat of evaporation.
,rt                                                                            &
                        ! LOCAL.
,p1(p_field)                                                                   &
                        ! LOCAL Pressure.
!                                     j/Kg at 0 deg C.
,rl(p_field)                                                                   &
                        ! LOCAL.
,q0(p_field)                                                                   &
                        ! LOCAL local SH.
,es0                                                                           &
                        ! LOCAL Saturated vapour pressure.
,v_pres(p_field)        ! LOCAL Vapour pressure.

INTEGER ::                                                                     &
 i                      ! LOCAL loop variable.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='DEWPNT'

PARAMETER ( rl1 = -2.73e3 )

!----------------------------------------------------------------------
!  Calculate P in HPa.
!----------------------------------------------------------------------
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(i)                    &
!$OMP  SHARED(p_field,p,rl,t,v_pres,q,p1,REPSILON)
DO i = 1,p_field
  p1(i) = p(i) / 100.0
  !----------------------------------------------------------------------
  !  Calculate RL - The latent heat of evaporation.
  !----------------------------------------------------------------------
  rl(i) = lc + rl1 * ( t(i) - tm )
  !----------------------------------------------------------------------
  !  Calculate Vapour pressure, and from that the dewpoint in Kelvins.
  !----------------------------------------------------------------------
  v_pres(i) = q(i) * p1(i) / ( repsilon + q(i))
END DO
!$OMP END PARALLEL DO

CALL qsat_wat(q0,t,p,p_field)

!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(i,es0,rt)             &
!$OMP  SHARED(p_field,v_pres,q0,p1,t,rl,td,REPSILON)
DO i = 1,p_field
  IF (v_pres(i) >  0.0) THEN
    es0 = (q0(i) * p1(i)) / (repsilon + q0(i))
    rt = (1 / t(i)) - ( rv * LOG(v_pres(i) / es0) )  / rl(i)
    td(i) = 1.0 / rt
    IF (td(i) >  t(i)) td(i) = t(i)
  ELSE
    td(i) = 0.0
  END IF
END DO
!$OMP END PARALLEL DO
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE dewpnt
END MODULE dewpnt_mod
