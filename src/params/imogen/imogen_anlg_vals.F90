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

MODULE imogen_anlg_vals

USE missing_data_mod, ONLY: rmdi
USE io_constants, ONLY: max_file_name_len

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Default parameters and variables required for the imogen analogue model.
!     Values can be set in the imogen.nml
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

REAL ::                                                                        &
  q2co2 = rmdi,                                                                &
              ! Radiative forcing due to doubling CO2 (W/m2
  f_ocean = rmdi,                                                              &
              ! Fractional coverage of the ocean
  kappa_o = rmdi,                                                              &
              ! Ocean eddy diffusivity (W/m/K)
  lambda_l = rmdi,                                                             &
              ! Inverse of climate sensitivity over land (W/m2/K)
  lambda_o = rmdi,                                                             &
              ! Inverse of climate sensitivity over ocean (W/m2/K)
  mu = rmdi,                                                                   &
              ! Ratio of land to ocean temperature anomalies
  t_ocean_init = rmdi,                                                         &
              ! Initial ocean temperature (K)
  diff_frac_const_imogen = rmdi
              ! Fraction of SW radiation that is diffuse for IMOGEN

CHARACTER(LEN=max_file_name_len) ::                                            &
  file_patt = '',                                                              &
              ! File containing the patterns
  file_clim = '',                                                              &
              ! File containing initialising climatology.
  file_base_anom = ''
              ! File containing prescribed anomalies ("name")
              ! Actual filename is in the form "name_year.nc"


NAMELIST  / imogen_anlg_vals_list/ q2co2,f_ocean,kappa_o,                      &
                                 lambda_l,lambda_o,mu,                         &
                                 t_ocean_init,file_patt,                       &
                                 file_clim, file_base_anom,                    &
                                 diff_frac_const_imogen

END MODULE imogen_anlg_vals
#endif
