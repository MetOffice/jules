! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************


SUBROUTINE partition(n, npp_ft_acc, dvi, rootc, harvc, reservec,               &
                     nonyield_diag, stemc, leafc, harv_count, harv_trig)

USE cropparm, ONLY: remob, mu, nu, sen_dvi, initial_carbon

USE crop_utils_mod, ONLY: carbon_fraction_from_dvi,                            &
                          no_carbon_pools_below_minimum

USE jules_vegetation_mod, ONLY: l_prescsow

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Partitions crop tile npp to carbon stores
!
! Method:
!   Updates crop carbon pools using crop DVI and NPP.
!   Checks crops is established.
!   Includes leaf senescence.
!   Doesn't allow carbon pools to drop below zero.
!
!   See JULES-crop technical documentation (Tom Osborne, Josh Hooker).
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) :: n           ! crop tile number

REAL(KIND=real_jlslsm), INTENT(IN)    :: npp_ft_acc  ! Accumulated NPP (kg m-2).

REAL(KIND=real_jlslsm), INTENT(IN OUT) :: dvi         ! crop development index
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: rootc       ! root biomass (kg m-2)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: harvc       ! crop yield (kg m-2)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: reservec
                                   ! stem reserve pool (kg m-2)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: nonyield_diag
  ! carbon that is leaving the crop model, which is not yield (kg m-2)

REAL(KIND=real_jlslsm), INTENT(IN OUT) :: leafc       ! leaf carbon pool (kg m-2)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: stemc       ! stem carbon pool (kg m-2)

INTEGER, INTENT(IN OUT) :: harv_count ! 1 for harvest timestep, 0 otherwise
INTEGER, INTENT(IN OUT) :: harv_trig  ! labels reason for harvest

! Local variables
REAL(KIND=real_jlslsm) :: f_root        ! fraction to roots
REAL(KIND=real_jlslsm) :: f_stem        ! fraction to stem
REAL(KIND=real_jlslsm) :: f_leaf        ! fraction to leaf
REAL(KIND=real_jlslsm) :: f_harv        ! fraction to harvested parts
REAL(KIND=real_jlslsm) :: all_c         ! sum of carbon pools (kg m-2)
REAL(KIND=real_jlslsm) :: extrac
                      ! carbon used to keep pools above their minima (kg m-2)
REAL(KIND=real_jlslsm) :: sen_fac
                      ! fraction of leafc to move to harvc for senescence
!-----------------------------------------------------------------------------

CALL carbon_fraction_from_dvi(n, dvi, f_root, f_stem, f_leaf, f_harv)

!-----------------------------------------------------------------------------
! Update carbon pools according to DVI
! Partition according to crop type and development stage
! units are kgC/m^2
! NB. Reserves taken from STEM growth rate
!-----------------------------------------------------------------------------

rootc    = rootc + npp_ft_acc * f_root

leafc    = leafc + npp_ft_acc * f_leaf

stemc    = stemc + ( npp_ft_acc * f_stem * ( 1.0 - remob(n) ) )

harvc    = harvc + npp_ft_acc * f_harv

reservec = reservec + ( npp_ft_acc * f_stem * remob(n) )

!-----------------------------------------------------------------------------
! Establishment routine
!-----------------------------------------------------------------------------

IF ( .NOT. l_prescsow ) THEN

  IF ( dvi < 1.0 ) THEN

    all_c = rootc + leafc + stemc + reservec

    IF (all_c < initial_carbon(n)) THEN

      dvi = -2.0

    END IF

  END IF

END IF

!-----------------------------------------------------------------------------
! Conservation routine - force harvest if negative Veg C
!-----------------------------------------------------------------------------

IF ( dvi > 1.0 ) THEN

  all_c = rootc + leafc + stemc + reservec

  IF (all_c < initial_carbon(n) .AND. harvc > 0.0 ) THEN
    harv_count = 1
    harv_trig  = 4
  END IF
END IF

!-----------------------------------------------------------------------------
! Re-allocation e.g. from reserves to grain once
! stems have stopped growing
! NB. 0.1 (0.9) is hard-wired
!-----------------------------------------------------------------------------

IF ( f_stem < 0.01 ) THEN

  harvc    = harvc + ( 0.1 * reservec )
  reservec = reservec * 0.9

END IF

!-----------------------------------------------------------------------------
! Leaf senescence
!-----------------------------------------------------------------------------

IF ( dvi > sen_dvi(n) ) THEN
  sen_fac = mu(n) * ( dvi - sen_dvi(n) )** nu(n)
  sen_fac = MIN(sen_fac, 1.0)

  harvc   = harvc + (sen_fac * leafc)
  leafc   = leafc * (1.0 - sen_fac)
END IF

!-----------------------------------------------------------------------------
! don't allow carbon pools to drop below their minimum values
! ----------------------------------------------------------------------------
CALL no_carbon_pools_below_minimum(n, rootc, harvc, reservec, stemc, leafc,    &
                                   extrac)
nonyield_diag = nonyield_diag - extrac


END SUBROUTINE partition
