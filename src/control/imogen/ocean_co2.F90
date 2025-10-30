#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

SUBROUTINE ocean_co2(                                                          &
  iyear,year_co2,co2_atmos_ppmv,co2_atmos_init_ppmv,dt_ocean,                  &
  fa_ocean,ocean_area,grad_co2_atmos_ppmv,year_run,                            &
  t_ocean_init,nfarray,d_ocean_atmos                                           &
)

USE jules_print_mgr, ONLY:                                                     &
  jules_message,                                                               &
  jules_print
IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This subroutine describes a simple uptake by the oceans of atmospheric CO2
!     The work is based upon the paper by Joos.
!   The parameters chosen replicate those of the 3-D model, see Table 2 of Joos
!     et al.
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Written by: Huntingford and Cox (March 1999)
!
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

INTEGER ::                                                                     &
  i,                                                                           &
         !WORK Looping counter
  j,                                                                           &
         !WORK Looping counter
  istep,                                                                       &
         !IN Looping counter from main subroutine that shows
         !   calls to Joos model
  iyear,                                                                       &
         !IN Years since start of run
  year_co2,                                                                    &
         !IN Years between updated atmospheric CO2
         !   concentration (yr)
  ncallyr,                                                                     &
         !IN Number of calls per year the ocean routine
         !   (with CO2 conc. fixed)
  year_run,                                                                    &
         !IN Number of years in simulation (yr)
  nfarray
         !IN Array size for fa_ocean

PARAMETER(ncallyr = 20)

REAL ::                                                                        &
  dt_ocean,                                                                    &
         !IN Mixed-layer temperature anomaly (K)
  co2_atmos_ppmv,                                                              &
         !WORK Atmospheric CO2 concentation during period
         !     YEAR_CO2 (ppm)
  co2_atmos_short,                                                             &
         !WORK Atmospheric CO2 concentation on the shortime
         !     (1/NCALLYR) timescale (ppm)
  co2_atmos_init_ppmv,                                                         &
         !WORK Initial atmospheric concentration
  dco2_atmos,                                                                  &
         !WORK Perturbation in atmospheric concentration (ppm)
  timestep_co2,                                                                &
         !WORK Timestep (yr)
  rs(year_run * ncallyr),                                                      &
         !WORK Response function values
  grad_co2_atmos_ppmv
         !IN Gradient of atmospheric CO2 (ppm/y) during
         !   YEAR_CO2 period

REAL ::                                                                        &
  fa_ocean(nfarray),                                                           &
         !IN/OUT CO2 fluxes from the atmosphere to the ocean
         !       (ie positive downwards) (ppm/m2/yr)
  dco2_ocean,                                                                  &
         !IN/OUT Carbon dioxide concentration perturbation
         !       in mixed-layer (ppm)
  co2_ocean,                                                                   &
         !OUT Carbon dioxide concentration in mixed-layer (ppm)
  co2_ocean_init,                                                              &
         !OUT Initial ocean carbon dioxide concentration (ppm)
  dco2_ocean_mol,                                                              &
         !WORK Carbon dioxide concentration perturbation
         !     of mixed-layer (umol/kg)
  t_mixed_init,                                                                &
         !WORK Initial global mean ocean surface temperature (C)
  t_ocean_init,                                                                &
         !IN Initial global surface temperature of the ocean (K)
  h,                                                                           &
         !WORK Mixed-layer depth (m)
  k_g,                                                                         &
         !WORK Gas exchange coefficient (/m2/yr)
  c,                                                                           &
         !WORK Units conversion parameter (umol m3/ppm/
  ocean_area
         !WORK Ocean area (m2)

REAL ::                                                                        &
  d_ocean_atmos
         ! OUT Change in atmospheric CO2 concentration a
         !     result of ocean-atmosphere feedbacks between
         !     calls to SCENARIO (ppm/"YEAR_CO2")

DATA k_g / 0.1306 /
DATA h / 40.0 /
DATA c / 1.722e17 /



IF (year_run * ncallyr > nfarray) THEN
  WRITE(jules_message,*) 'Array size too small for FA_OCEAN'
  CALL jules_print('ocean_co2',jules_message)
END IF

CALL response(ncallyr,year_run,rs)

d_ocean_atmos = 0.0

!-----------------------------------------------------------------------
! Assume initial mixed-layer temperature identical to diagnosed global
! surface temperature.
!-----------------------------------------------------------------------
t_mixed_init= t_ocean_init - 273.15

timestep_co2 = 1.0 / REAL(ncallyr)

! Loops internally within the model to cover iterations within
! period YEAR_CO2
DO j = 1,ncallyr * year_co2

  istep = ((iyear - year_co2) * ncallyr) + j
  !                             !ISTEP is the integer that indexes calcula
  !                             !using the Joos model. Recall this subrout
  !                             !is called at end of period YEAR_CO2

  co2_ocean_init = co2_atmos_init_ppmv           !Assume starting
          !from equilibrium.

  !-----------------------------------------------------------------------
  ! Introduce linear correction in atmospheric CO2 concentration
  ! down to timescale 1/NCALLYR
  !-----------------------------------------------------------------------
  co2_atmos_short = co2_atmos_ppmv + grad_co2_atmos_ppmv *                     &
               ((REAL(j) / REAL(ncallyr)) - 0.5 * REAL(year_co2))

  dco2_atmos = co2_atmos_short - co2_atmos_init_ppmv

  !-----------------------------------------------------------------------
  ! Calculate perturbation in dissolved inorganic carbon (Eqn (3) of Joos)
  ! for timestep indexed by ISTEP.
  !-----------------------------------------------------------------------
  dco2_ocean_mol = 0.0  ! Initialised for loop

  IF (istep >= 2) THEN
    DO i = 1,istep-1
      dco2_ocean_mol = dco2_ocean_mol +                                        &
                    (c/h) * fa_ocean(i) * rs(istep - i) * timestep_co2
    END DO
  END IF

  !----------------------------------------------------------------------
  ! Relation between DCO2_OCEAN_MOL and DCO2_OCEAN:   Joos et al.
  ! ie Convert from umol/kg to ppm
  !----------------------------------------------------------------------
  dco2_ocean =                                                                 &
     (1.5568 - (1.3993 * 1.0e-2 * t_mixed_init)) * dco2_ocean_mol              &
  + (7.4706 - (0.20207 * t_mixed_init)) * 1.0e-3 * (dco2_ocean_mol**2)         &
  - (1.2748 - (0.12015 * t_mixed_init)) * 1.0e-5 * (dco2_ocean_mol**3)         &
  + (2.4491 - (0.12639 * t_mixed_init)) * 1.0e-7 * (dco2_ocean_mol**4)         &
  - (1.5468 - (0.15326 * t_mixed_init)) * 1.0e-10 * (dco2_ocean_mol**5)

  !----------------------------------------------------------------------
  ! Now incorporate correction suggested by Joos
  !----------------------------------------------------------------------
  co2_ocean = co2_ocean_init + dco2_ocean
  co2_ocean = co2_ocean * EXP(0.0423 * dt_ocean)

  fa_ocean(istep) = (k_g / ocean_area)                                         &
                  * (co2_atmos_short - co2_ocean)

  !----------------------------------------------------------------------
  ! Now calculate D_OCEAN_ATMOS (D_OCEAN_ATMOS is positive when
  ! flux is upwards)
  !----------------------------------------------------------------------
  d_ocean_atmos = d_ocean_atmos                                                &
                - fa_ocean(istep) * ocean_area * timestep_co2
END DO

RETURN

END SUBROUTINE ocean_co2
#endif
