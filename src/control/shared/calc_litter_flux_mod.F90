! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE calc_litter_flux_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

PUBLIC calc_litter_flux

!-----------------------------------------------------------------------------
! Description:
!   Calculate litter flux as a residual of NPP and change
!   vegetation carbon/nitrogen store. Works for carbon and nitrogen fluxes.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in VEGETATION
!
! Code Description:
!   Language: Fortran 90.
!   https://code.metoffice.gov.uk/trac/jules/wiki/AddingNewSubroutines
!   17 Jan. 2017
!-----------------------------------------------------------------------------
CONTAINS

FUNCTION calc_litter_flux(npp, veg, dveg, frac, dfrac, r_gamma, frac_flux)     &
         RESULT( litter_flux )

! Calculate litter flux from vegetation to soil.
!
! Used in TRIFFID.
! Works for carbon or nitrogen.

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Real arguments with INTENT(IN).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
npp                                                                            &
                            ! IN Net carbon or nitrogen uptake
                            !       (kg/m2/360days).
,veg                                                                           &
                            ! IN carbon or nitrogen content of vegetation
                            !       (kg/m2).
,dveg                                                                          &
                            ! IN change carbon or nitrogen content
                            !       (kg/m2/triffid period)
,frac                                                                          &
                            ! IN Fractional cover of PFT
,dfrac                                                                         &
                            ! IN Change in fraction cover
,r_gamma                                                                       &
                            ! IN Inverse timestep (/360days).
,frac_flux
                            ! IN Fractional cover used in
                            !      equilibrium/standard competition modes
                            !      In equilibrium mode frac_flux is equal
                            !      to frac
                            !      In standard mode frac_flux is equal to
                            !      old_frac

!-----------------------------------------------------------------------------
! Function result.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) :: litter_flux
      !  flux from vegetation to soil [kg/(m-2 PFT)/(360 days)].

!-----------------------------------------------------------------------------
!end of header

! The calculation is equivalent to:
! litter_flux = npp - ( r_gamma/frac_flux )*( veg*frac - old_veg*old_frac )
! Where:
!    old_frac = frac - dfrac
!    frac_flux = frac - (1.0-frac)*dfrac
!    old_veg = veg - dveg

litter_flux = npp - r_gamma / frac_flux *                                      &
              ( veg * frac - (veg - dveg) * (frac - dfrac) )

! Convert units from [kg/(m2 of PFT)/(360 days)]
!                 to [kg/(m2 of land)/(360 days)]
litter_flux = litter_flux * frac_flux

END FUNCTION calc_litter_flux

END MODULE calc_litter_flux_mod
