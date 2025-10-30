! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    Subroutine VGRAV -------------------------------------------------
!
! Purpose: To calculate the gravitational sedimentation velocity of
!          tracer particles according to Stoke's law, including the
!          Cunningham correction factor.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Documentation: Ref. Pruppacher & Klett
!                    Microphysics of clouds & ppn    1978,1980 edns.
! Code description:
!   Language: Fortran 90
!   This code is written to UMDP3 standards.
!-----------------------------------------------------------------------
MODULE vgrav_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='VGRAV_MOD'

CONTAINS

SUBROUTINE vgrav(diam,rhop,p,t,vstokes,ccf,etaa)

USE atm_fields_bounds_mod, ONLY: tdims

USE dust_param, ONLY: accf, bccf, cccf
USE chemistry_constants_mod, ONLY: mfp_ref, tref_mfp, pref_mfp
USE conversions_mod, ONLY: zerodegc
USE planet_constants_mod, ONLY: g

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
USE um_types, ONLY: real_jlslsm

IMPLICIT NONE


REAL(KIND=real_jlslsm), INTENT(IN) :: diam
                                          !IN particle diameter
REAL(KIND=real_jlslsm), INTENT(IN) :: rhop
                                          !IN particles density
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
                    p(tdims%i_start:tdims%i_end,                               &
                      tdims%j_start:tdims%j_end) !IN pressure
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
                    t(tdims%i_start:tdims%i_end,                               &
                      tdims%j_start:tdims%j_end) !IN temperature

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
                     vstokes( tdims%i_start:tdims%i_end,                       &
                              tdims%j_start:tdims%j_end)
                     !OUT sedimentation velocity

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
                     etaa(    tdims%i_start:tdims%i_end,                       &
                              tdims%j_start:tdims%j_end)
                     !OUT viscosity of air

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
                     ccf(     tdims%i_start:tdims%i_end,                       &
                              tdims%j_start:tdims%j_end)
                     !OUT cunningham correction factor

! local variables
INTEGER :: i, j

REAL(KIND=real_jlslsm) :: tc       !temperature in deg C
REAL(KIND=real_jlslsm) :: lamdaa   !mean free path of particle
REAL(KIND=real_jlslsm) :: alphaccf

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='VGRAV'

! Calculate viscosity of air (Pruppacher & Klett p.323)
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end

    tc = t(i,j) - zerodegc

    IF (tc  >=  0.0) THEN
      etaa(i,j) = (1.718+0.0049 * tc) * 1.0e-5
    ELSE
      etaa(i,j) = (1.718+0.0049 * tc - 1.2e-5 * tc * tc) * 1.0e-5
    END IF

    ! Calculate mean free path of particle (Pruppacher & Klett p.323)
    lamdaa = mfp_ref * pref_mfp * t(i,j) / (p(i,j) * tref_mfp)

    ! Calculate Cunningham correction factor(Pruppacher & Klett p.361)
    alphaccf = accf + bccf * EXP(cccf * diam * 0.5 / lamdaa)
    ccf(i,j) = (1.0 + alphaccf * lamdaa / (0.5 * diam))

    ! Calculate sedimentation velocity (Pruppacher & Klett p.362)
    vstokes(i,j) = ccf(i,j) * (diam * diam * g * rhop) / (18.0 * etaa(i,j))

  END DO !ROW_LENGTH
END DO !ROWS

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE vgrav
END MODULE vgrav_mod
