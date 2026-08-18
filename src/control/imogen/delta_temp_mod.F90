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

MODULE delta_temp_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE delta_temp(                                                         &
  n_olevs,f_ocean,kappa_o,lambda_l,lambda_o,mu,dqradf,dtemp_l,dtemp_o, dtemp_g)

USE invert_mod, ONLY: invert

USE veg_param, ONLY: secs_per_360days

USE jules_print_mgr, ONLY: jules_message, jules_print

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This subroutine calls a simple two-box thermal model in order to
!   estimate the global mean land surface temperature.
!   The code uses an implicit solver.
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
! Written by: C. Huntingford, 11/09/98.
!
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) ::                                                         &
  n_olevs  ! Number of ocean thermal layers

REAL, INTENT(IN) ::                                                            &
  dqradf,                                                                      &
           ! Change in global radiative forcing (W/m2)
  lambda_l,                                                                    &
           ! Inverse climate sensitivity over land (W/K/m2)
  lambda_o,                                                                    &
           ! Inverse climate sensitivity over ocean (W/K/m2)
  mu,                                                                          &
           ! Ratio of land-to-ocean temperature anomalies (.)
  f_ocean,                                                                     &
           ! Fractional coverage of planet with ocean
  kappa_o    ! Ocean eddy diffusivity (W/m/K)

REAL, INTENT(IN OUT) ::                                                        &
  dtemp_o(n_olevs)
           ! Ocean temperature anomalies (K)

REAL, INTENT(OUT) ::                                                           &
  dtemp_l,                                                                     &
           ! Land mean temperature anomaly
  dtemp_g
           ! Global mean temperature anomaly

REAL ::                                                                        &
  flux_top,                                                                    &
           ! Flux into the top of the ocean (W/m2)
  flux_bottom,                                                                 &
           ! Flux into the top of the ocean (W/m2)&
  timestep_local,                                                              &
           ! Timestep (s)
  dz(1:n_olevs)
           ! Distance step (m)

REAL ::                                                                        &
  r1(1:n_olevs),                                                               &
           ! Mesh ratio
  r2(1:n_olevs),                                                               &
           ! Mesh ratio
  lambda_old,                                                                  &
           ! Inhomogenous component in implicit scheme (/s)
  lambda_new,                                                                  &
           ! Inhomogenous component in implicit scheme (/s)
  pdirchlet,                                                                   &
           ! ``Dirchlet'' part of mixed boundary condition ocean surface (/m)
  dtemp_o_old(1:n_olevs),                                                      &
           ! Old ocean temperature anomalies
  dtemp_o_new(1:n_olevs),                                                      &
           ! New ocean temperature anomalies
  factor_dep,                                                                  &
           ! Factor by which layers are changed
  depth,                                                                       &
           ! Cumulated depth of layers (m)
  dz_top   ! Depth of the top layer (m)

REAL, PARAMETER :: rhocp = 4.04e6
           ! Rho times cp for the ocean (J/K/m3)

INTEGER ::                                                                     &
  timesteps,                                                                   &
           ! Number of iterations per call to delta_temp
  i,j      ! Looping parameter

INTEGER, PARAMETER :: iter_per_year = 20
           ! Iterations per year

!-----------------------------------------------------------------------
! Set up parameters for the model
!-----------------------------------------------------------------------

timestep_local = secs_per_360days / REAL(iter_per_year)
timesteps = iter_per_year


! Here the variable depths are calculated, along with the "R" values

! The mesh is as follows
!
!      -------------            U(1)
!                  (This is the top layer, called TEM(0) at points)
!          R(1)
!
!      ------------             U(2)
!
!          R(2)
!
!
!           |
!           |
!          \ /
!
!      ------------             U(N_OLEVS) = 0.0

dz_top = 2.0
depth = 0.0

DO i = 1,n_olevs
  factor_dep = 2.71828**(depth / 500.0)
  dz(i) = dz_top * factor_dep
  depth = depth + dz(i)
END DO

! Now arrange that the lowest depth (here depth = u(n_olevs+1)) is at 5000m
dz_top = dz_top * (5000.0 / depth)
DO i = 1,n_olevs
  dz(i) = dz(i) * (5000.0 / depth)
END DO

r1(1) = (kappa_o / rhocp) * (timestep_local / (dz_top * dz_top))
r2(1) = 0.0
DO i = 2,n_olevs
  r1(i) = (kappa_o / rhocp) * (timestep_local / (dz(i-1) * (dz(i) + dz(i-1))))
  r2(i) = (kappa_o / rhocp) * (timestep_local / (dz(i) * (dz(i) + dz(i-1))))
END DO

! Reset the new values of dtemp_o_old for the start of this run.
DO i = 1,n_olevs
  dtemp_o_old(i) = dtemp_o(i)
END DO

DO i = 1,timesteps

  lambda_old = -dqradf / (kappa_o * f_ocean)
  lambda_new = -dqradf / (kappa_o * f_ocean)

  pdirchlet = ((1.0 - f_ocean) * lambda_l * mu) / (f_ocean * kappa_o)          &
    + (lambda_o / kappa_o)

  CALL invert(                                                                 &
    dtemp_o_old,dtemp_o_new,pdirchlet,lambda_old,lambda_new,                   &
    r1,r2,dz_top,n_olevs)

  ! Now check that the flux out of the bottom is not too large.
  flux_top =  - 0.5 * (kappa_o * lambda_old * timestep_local)                  &
              - 0.5 * (kappa_o * lambda_new * timestep_local)                  &
              - 0.5 * (pdirchlet * kappa_o * dtemp_o_old(1) * timestep_local)  &
              - 0.5 * (pdirchlet * kappa_o * dtemp_o_new(1) * timestep_local)
  flux_bottom = (timestep_local / (2.0 * dz(n_olevs))) * kappa_o               &
              * (dtemp_o_old(n_olevs) + dtemp_o_new(n_olevs))

  IF (ABS(flux_bottom / (flux_top+0.0000001)) > 0.01) THEN
    WRITE(jules_message,*) 'Flux at bottom of the ocean is greater ' //        &
               'than 0.01 of top'
    CALL jules_print('delta_temp',jules_message)
  END IF


  ! Set calculated values at now ``old'' values
  DO j = 1,n_olevs
    dtemp_o_old(j) = dtemp_o_new(j)
  END DO

END DO

! At end of this model run, reset values of lambda_l and lambda_o

DO j = 1,n_olevs
  dtemp_o(j) = dtemp_o_new(j)
END DO

! land temperature change
dtemp_l = mu * dtemp_o(1)

! global temperature change
! t_global  = (1-f) * t_land + f*t_ocean.
dtemp_g = mu * dtemp_o(1) * (1.0 - f_ocean) + f_ocean * dtemp_o(1)

RETURN

END SUBROUTINE delta_temp
END MODULE delta_temp_mod
#endif
