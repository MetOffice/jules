! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

SUBROUTINE emerge(n, t_surft, dvi)

USE conversions_mod, ONLY: rsec_per_day

USE cropparm, ONLY: t_bse, tt_emr

USE timestep_mod, ONLY: timestep

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Increases DVI based on thermal time, until crop emerges
!   DVI increases from -1 to 0
!
! Method:
!   Crop should already have been sown, but not yet emerged when this
!   subroutine is called.
!   Subroutine calculates thermal time (effective temperature)
!   from tile temperature and uses this to increase crop
!   development index (DVI).
!
!   See JULES-crop technical documentation (Tom Osborne, Josh Hooker).
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) :: n       ! crop tile number

REAL(KIND=real_jlslsm), INTENT(IN)    :: t_surft
                               ! temperature (T1P5M) on tile (K)

REAL(KIND=real_jlslsm), INTENT(IN OUT) :: dvi     ! crop development index

! Local variables
REAL(KIND=real_jlslsm) :: teff    ! effective temperature (K)

!-----------------------------------------------------------------------------

teff = t_surft - t_bse(n)

teff = MAX(teff, 0.0) ! Presume that development can't go back

teff = teff * ( timestep / rsec_per_day ) ! times fraction of full day

dvi  = dvi + ( teff / tt_emr(n) )

END SUBROUTINE emerge
