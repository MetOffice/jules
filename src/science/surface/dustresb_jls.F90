! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE dustresb_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='DUSTRESB_MOD'

CONTAINS
!----------------------------------------------------------------------
! subroutine DUSTRESB
!
! Purpose:
!   To calculate the surface layer resistance for mineral dust
!
! Code Description:
!  Language: FORTRAN77 + common extensions
!  This code is written to UMDP3 v6 programming standards
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Documentation: "Modelling the atmospheric lifecycle..."
!                 Woodward, JGR106, D16, pp18155-18166
!
! Code description:
!   Language: Fortran 90
!   This code is written to UMDP3 standards.
!---------------------------------------------------------------------

SUBROUTINE dustresb(                                                           &
 pstar,tstar,rhostar,vshr,cd_std_dust,                                         &
 r_b_dust                                                                      &
 )

USE vgrav_mod, ONLY: vgrav

USE atm_fields_bounds_mod, ONLY: tdims

USE chemistry_constants_mod, ONLY: boltzmann
USE dust_parameters_mod, ONLY: ndiv, drep, rhop
USE conversions_mod, ONLY: pi
USE planet_constants_mod, ONLY: g
USE jules_science_fixes_mod, ONLY: l_fix_ustar_dust

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

REAL(KIND=real_jlslsm) ::                                                      &
     !IN
 pstar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                              !IN surface pressure
,tstar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                              !IN surface temperature
,rhostar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                              !IN surface air density
,vshr(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                              !IN surface to lowest lev windspeed
!                                   !   difference
,cd_std_dust(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                              !IN surface transfer coeffient for
!                                   !   momentum, excluding orographic
!                                   !   form drag

REAL(KIND=real_jlslsm) ::                                                      &
     !OUT
 r_b_dust(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,ndiv)
                              !OUT surface layer resistance for
!                                     !    mineral dust

!     local variables

INTEGER ::                                                                     &
 idiv                                                                          &
      !loop counter, dust divisions
,i                                                                             &
      !loop counter
,j
      !loop counter

REAL(KIND=real_jlslsm) ::                                                      &
 etaa(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                           !dynamic viscosity of air
,vstokes1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                 &
                           !gravitational settling velocity
,nstokes                                                                       &
                           !stokes number = VstokesVshrVshr/nu g
,nschmidt                                                                      &
                           !schmidt number = nu/diffusivit
,ccf(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                      &
                           !Cunningham correction factor
,stokes_exp                                                                    &
                           !stokes term in R_B_DUST equation
,smallp                    !small +ve number, negligible compared to 1

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='DUSTRESB'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!... epsilon() is defined as almost negligible, so eps/100 is negligible
smallp = EPSILON(1.0) / 100.0

!...calc stokes number, schmidt number and finally resistance

DO idiv = 1,ndiv

  CALL vgrav(drep(idiv),rhop,pstar,tstar,vstokes1,ccf,etaa)

!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE)                               &
!$OMP PRIVATE(i,j,stokes_exp,nschmidt,nstokes)                                 &
!$OMP SHARED(tdims,etaa,drep,idiv,rhostar,tstar,ccf,vstokes1,cd_std_dust,      &
!$OMP        vshr,g,smallp,l_fix_ustar_dust,r_b_dust)
  !CDIR NOVECTOR
  DO j = tdims%j_start,tdims%j_end
    DO i= tdims%i_start,tdims%i_end
      nschmidt = 3.0 * pi * etaa(i,j) * etaa(i,j) * drep(idiv) /               &
                      (rhostar(i,j) * boltzmann * tstar(i,j) * ccf(i,j))
      nstokes = vstokes1(i,j) * cd_std_dust(i,j) * rhostar(i,j) *              &
                     vshr(i,j) * vshr(i,j) / (etaa(i,j) * g)
      ! Avoid underflow in Stokes term by setting to zero if
      ! negligible compared to Schmidt term, i.e., if NSTOKES
      ! is too small.
      IF ( 3.0 / nstokes <                                                     &
           - LOG10( smallp * nschmidt**(-2.0 / 3.0) ) ) THEN
        stokes_exp = 10.0**(-3.0 / nstokes)
      ELSE
        stokes_exp = 0.0
      END IF
      IF (l_fix_ustar_dust) THEN
        r_b_dust(i,j,idiv) = 1.0 / ( vshr(i,j) * SQRT(cd_std_dust(i,j)) *      &
                             (nschmidt**(-2.0 / 3.0) + stokes_exp) )
      ELSE
        r_b_dust(i,j,idiv) = 1.0 / ( SQRT(cd_std_dust(i,j)) *                  &
                             (nschmidt**(-2.0 / 3.0) + stokes_exp) )
      END IF
    END DO !ROW_LENGTH
  END DO !ROWS
!$OMP END PARALLEL DO
END DO !NDIV

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE dustresb
END MODULE dustresb_mod
