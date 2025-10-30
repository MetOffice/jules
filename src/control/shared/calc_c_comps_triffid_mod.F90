! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE calc_c_comps_triffid_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

PUBLIC calc_c_comps_triffid

!-----------------------------------------------------------------------------
! Description:
!   Calculates carbon contents from vegetation height
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in VEGETATION
!
! Code Description:
!   Language: Fortran 90.
!   https://code.metoffice.gov.uk/trac/jules/wiki/AddingNewSubroutines
!   17 Jan. 2015
!-----------------------------------------------------------------------------
CONTAINS

SUBROUTINE calc_c_comps_triffid(n, ht, lai_bal_pft, leaf, root, wood, c_veg)

USE jules_vegetation_mod, ONLY: l_trait_phys

USE pftparm, ONLY: a_ws, eta_sl, a_wl, b_wl, lma, sigl

USE jules_surface_mod, ONLY: cmass

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
n                   !  PFT number.

REAL(KIND=real_jlslsm), INTENT(IN)    ::                                       &
ht                  ! IN Vegetation height (m).

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT)   ::                                       &
lai_bal_pft                                                                    &
                      ! OUT Balanced leaf area index
,leaf                                                                          &
                      ! OUT Leaf biomass for balanced LAI (kg C/m2).
,root                                                                          &
                      ! OUT Root biomass (kg C/m2).
,wood                                                                          &
                      ! OUT Woody biomass (kg C/m2).
,c_veg
                      ! OUT Total carbon content of vegetation (kg C/m2).

!-----------------------------------------------------------------------------
!end of header

lai_bal_pft = (a_ws(n) * eta_sl(n) * ht / a_wl(n))**(1.0 / (b_wl(n) - 1.0))
IF (l_trait_phys) THEN
  leaf = cmass * lma(n) * lai_bal_pft
ELSE
  leaf = sigl(n) * lai_bal_pft
END IF
root = leaf
wood = a_wl(n) * (lai_bal_pft**b_wl(n))
c_veg = leaf + root + wood

END SUBROUTINE calc_c_comps_triffid

END MODULE calc_c_comps_triffid_mod
