! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************


SUBROUTINE develop(n, t_surft, phot, tt_veg, tt_rep, dvi)

USE conversions_mod, ONLY: rsec_per_day
USE cropparm, ONLY: t_opt, t_bse, t_max, crit_pp, pp_sens
USE timestep_mod, ONLY: timestep

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This subroutine increments the plant development index using the thermal
!   time accumulation.
!   Increases dvi by a rate determined by thermal time accumulation. dvi
!   increases from 0 to 2.
!
! Method:
!   Crop should have already emerged when this subroutine is called.
!   An effective temperature (Teff) is calculated from the tile temperature
!   and the increase in crop development index (DVI) is then found.
!   During the first stage of development, the rate of increase in DVI
!   is slowed (for some plants) by multiplying by RPE (the relative
!   photoperiod effect).
!
!   See JULES-crop technical documentation (Tom Osborne, Josh Hooker).
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) :: n      ! crop tile number

REAL(KIND=real_jlslsm), INTENT(IN)    :: t_surft
                               ! temperature (T1P5M) on tile (K).
REAL(KIND=real_jlslsm), INTENT(IN)    :: phot   ! photoperiod (hours).
REAL(KIND=real_jlslsm), INTENT(IN)    :: tt_veg
                              ! thermal requirement of stage 1 of
                              ! crop development (degree days).
REAL(KIND=real_jlslsm), INTENT(IN)    :: tt_rep
                              ! thermal requirement of stage 2 of
                              ! crop development (degree days).

REAL(KIND=real_jlslsm), INTENT(IN OUT) :: dvi    ! crop development index

! Local variables
REAL(KIND=real_jlslsm) :: teff    ! effective temperature (K)
REAL(KIND=real_jlslsm) :: rpe     ! relative photoperiod effect

!-----------------------------------------------------------------------------

IF ( t_surft <= t_opt(n) .AND. t_surft >= t_bse(n) ) THEN

  teff = t_surft - t_bse(n)

ELSE IF ( t_surft > t_opt(n) .AND. t_surft < t_max(n) ) THEN

  teff = t_opt(n) - t_bse(n) - ( ( t_surft - t_opt(n) ) /                      &
         ( t_max(n) - t_opt(n) ) ) * ( t_opt(n) - t_bse(n) )

ELSE IF ( t_surft < t_bse(n) .OR. t_surft >= t_max(n) ) THEN

  teff = 0.0

END IF

teff = teff * ( timestep / rsec_per_day ) ! fraction of full day

IF ( dvi < 1.0 ) THEN  ! Photo-period effect on development
                       ! only during first phase

  rpe = 1.0 - ( phot - crit_pp(n) ) * pp_sens(n)

  rpe = MIN( 1.0, rpe )
  rpe = MAX( 0.0, rpe )
  dvi = dvi + rpe * teff / tt_veg

ELSE

  dvi = dvi + teff / tt_rep     ! No photoperiod effect

END IF

END SUBROUTINE develop
