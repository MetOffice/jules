! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module contains variables used for reading in cropparm data

MODULE cropparm_io

USE max_dimensions, ONLY: ncpft_max

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Set up variables to use in IO for crop PFTs (a fixed size version of each
!   array in cropparm that we want to initialise)
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

REAL(KIND=real_jlslsm) ::                                                      &
  t_bse_io(ncpft_max),                                                         &
  t_opt_io(ncpft_max),                                                         &
  t_max_io(ncpft_max),                                                         &
  tt_emr_io(ncpft_max),                                                        &
  crit_pp_io(ncpft_max),                                                       &
  pp_sens_io(ncpft_max),                                                       &
  rt_dir_io(ncpft_max),                                                        &
  alpha1_io(ncpft_max),                                                        &
  alpha2_io(ncpft_max),                                                        &
  alpha3_io(ncpft_max),                                                        &
  beta1_io(ncpft_max),                                                         &
  beta2_io(ncpft_max),                                                         &
  beta3_io(ncpft_max),                                                         &
  gamma_io(ncpft_max),                                                         &
  delta_io(ncpft_max),                                                         &
  remob_io(ncpft_max),                                                         &
  cfrac_s_io(ncpft_max),                                                       &
  cfrac_r_io(ncpft_max),                                                       &
  cfrac_l_io(ncpft_max),                                                       &
  allo1_io(ncpft_max),                                                         &
  allo2_io(ncpft_max),                                                         &
  mu_io(ncpft_max),                                                            &
  nu_io(ncpft_max),                                                            &
  yield_frac_io(ncpft_max),                                                    &
  initial_carbon_io(ncpft_max),                                                &
  initial_c_dvi_io(ncpft_max),                                                 &
  sen_dvi_io(ncpft_max),                                                       &
  t_mort_io(ncpft_max)

!-----------------------------------------------------------------------
! Set up a namelist for reading and writing these arrays
!-----------------------------------------------------------------------
NAMELIST  / jules_cropparm/                                                    &
  t_bse_io, t_opt_io, t_max_io, tt_emr_io, crit_pp_io, pp_sens_io,             &
  rt_dir_io, alpha1_io, alpha2_io, alpha3_io, beta1_io, beta2_io, beta3_io,    &
  gamma_io, delta_io, remob_io, cfrac_s_io, cfrac_r_io, cfrac_l_io,            &
  allo1_io, allo2_io, mu_io, nu_io, yield_frac_io, initial_carbon_io,          &
  initial_c_dvi_io, sen_dvi_io, t_mort_io

END MODULE cropparm_io
