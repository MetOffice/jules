! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    SUBROUTINE CH4_MICROBE-----------------------------------------------

! Description:
!     Calculates methane emissions from wetland area.

MODULE ch4_microbe_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='CH4_MICROBE_MOD'

CONTAINS
SUBROUTINE ch4_microbe(npnts, soil_pts, dim_ch4layer, timestep,                &
                       l_ch4_tlayered, soil_index,                             &
                       tsoil, decomp_wetlfrac,                                 &
                       ztot, acclim_ch4, sthu, bexp,                           &
                       mic_ch4, mic_act_ch4, substr_ch4, fch4_wetlfrac )


USE jules_soil_biogeochem_mod, ONLY:                                           &
  t0_ch4, k2_ch4, kd_ch4, rho_ch4, q10_mic_ch4, cue_ch4, mu_ch4, tau_ch4,      &
  frz_ch4, alpha_ch4

USE jules_soil_mod, ONLY: dzsoil

USE parkind1, ONLY: jprb, jpim
USE yomhook,  ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  npnts,                                                                       &
    ! Number of gridpoints.
  soil_pts,                                                                    &
    ! Number of soil points.
  dim_ch4layer
    ! Soil layers for methane calc. Can be either 1 or sm_levels

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  timestep
    ! Model timestep (s).

LOGICAL, INTENT(IN) ::                                                         &
  l_ch4_tlayered

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  soil_index(npnts)
    ! Array of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  tsoil(npnts,dim_ch4layer),                                                   &
    ! Soil temperature (K) on appropriate layers
  decomp_wetlfrac(npnts,dim_ch4layer),                                         &
    ! Anaerobic decomposition of whichever substrate is used for microbial
    ! calculation, over the wetland fraction of the grid box (i.e. this is not
    ! scaled by wetland fraction) (kg C/m2/s).
  ztot(dim_ch4layer),                                                          &
    ! Depths of centres of soil layers in (m) from the surface.
  acclim_ch4(npnts,dim_ch4layer),                                              &
    ! Acclimation factor for microbial trait adaptation.
  sthu(npnts,dim_ch4layer),                                                    &
    ! Unfrozen soil moisture of each layer as a fraction of saturation.
  bexp(npnts,dim_ch4layer)
    ! Brooks & Corey exponent.

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  mic_ch4(npnts,dim_ch4layer),                                                 &
    ! Concentration of active methanogenic biomass (mgC/m3)
  mic_act_ch4(npnts,dim_ch4layer),                                             &
    ! Activity level of methanogenic biomass (fraction)
  substr_ch4(npnts,dim_ch4layer)
    ! Concentration of dissolved substrate that methanogens consume (mgC/m3)

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  fch4_wetlfrac(npnts)
    ! Methane flux produced by microbial scheme, over the wetland fraction of
    ! the grid box (i.e. this is not scaled by wetland fraction) (kg C/m2/s).

!-----------------------------------------------------------------------------
! Local scalar variables:
!-----------------------------------------------------------------------------
INTEGER :: i,j,n

REAL(KIND=real_jlslsm) ::                                                      &
  arh,                                                                         &
  ! Arrhenius function for microbial decomposition
  phi,                                                                         &
  ! Substrate limitation factor for microbial decomposition
  growth_rt,                                                                   &
  ! Growth rate
  substr_prod,                                                                 &
  ! Dissolved substrate production flux
  substr_cons,                                                                 &
  ! Dissolved substrate consumption flux
  b_prod,                                                                      &
  ! Production of microbial biomass
  b_death,                                                                     &
  ! Death of microbial biomass
  b_dorm,                                                                      &
  ! Dormancy of microbial biomass
  frac_ch4,                                                                    &
  ! Fraction of anaerobic decomposition that forms methane.
  tstep_hr
  ! Model timestep in units of hours

!-----------------------------------------------------------------------------
! Local array variables:
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  ch4_prod(npnts,dim_ch4layer),                                                &
  ! Methane production by microbes in each soil layer (before oxidation).
  decomp_tmp(npnts,dim_ch4layer),                                              &
  ! Decomposition in units: mg C/m3/hr WITH ADDITIONAL DECAY TERM REMOVED.
  substr_mg_m3(npnts,dim_ch4layer),                                            &
  ! Substrate concentration in units: mg C/m^3
  mic_mg_m3(npnts,dim_ch4layer)
  ! Methanogenic biomass in units: mg C/m^3

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CH4_MICROBE'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Initialise variables / set parameters
ch4_prod(:,:) = 0.0
frac_ch4    = 0.5


!-----------------------------------------------------------------------------
! Put things in the units used for the calculation: mgC/m3/hour
!-----------------------------------------------------------------------------
DO j = 1,soil_pts
  i = soil_index(j)
  DO n = 1,dim_ch4layer
    decomp_tmp(i,n) = decomp_wetlfrac(i,n) * 1e6 * 3600 *                      &
                                           EXP(tau_ch4 * ztot(n)) / frac_ch4
    substr_mg_m3(i,n) = substr_ch4(i,n) * 1e6
    mic_mg_m3(i,n)    = mic_ch4(i,n) * 1e6
    IF ( l_ch4_tlayered ) THEN
      decomp_tmp(i,n)   = decomp_tmp(i,n) / dzsoil(n)
      substr_mg_m3(i,n) = substr_mg_m3(i,n) / dzsoil(n)
      mic_mg_m3(i,n)    = mic_mg_m3(i,n) / dzsoil(n)
    END IF
  END DO
END DO

tstep_hr = timestep / 3600.0

!-----------------------------------------------------------------------------
! Calculate wetland methane emission.
!-----------------------------------------------------------------------------
!! CURRENTLY ONLY DOING ONE SUBSTRATE BECAUSE I DON'T WANT LOADS OF PROGNOSTICS

! Update the prognostic variables
DO j = 1,soil_pts
  i = soil_index(j)
  DO n = 1,dim_ch4layer
    substr_prod = decomp_tmp(i,n)
    ! Calculate 'arh' (arrhenius function)
    arh = q10_mic_ch4**( 0.1 * (tsoil(i,n) - t0_ch4) * t0_ch4 / tsoil(i,n) )
    ! Calculate 'phi' (substrate limitation curve)
    phi = TANH(( substr_mg_m3(i,n) * rho_ch4 * EXP(-3270.0 / tsoil(i,n))       &
                 / arh )**0.8)

    ! Calculate freezing effect on substrate production
    IF ( sthu(i,n) < (0.02 * bexp(i,n)) ) THEN
      substr_prod = substr_prod * frz_ch4
    END IF

    ! Production and consumption of substrate
    IF ( acclim_ch4(i,n) > 0.0 ) THEN
      substr_cons = arh * phi * k2_ch4 * mic_mg_m3(i,n) * mic_act_ch4(i,n)     &
                        / acclim_ch4(i,n)

      ! Production of biomass
      b_prod    = cue_ch4 * substr_cons
      b_death   = mic_mg_m3(i,n) * alpha_ch4 * k2_ch4 * arh / acclim_ch4(i,n)
      growth_rt = arh * phi * k2_ch4 * cue_ch4 / acclim_ch4(i,n)

      IF (b_prod - b_death < 0.0) THEN
        b_death = mic_mg_m3(i,n) * kd_ch4 * mic_act_ch4(i,n) * arh             &
                  / acclim_ch4(i,n) + b_death
      END IF

      ! Dormancy/reactivation
      IF ( growth_rt < mu_ch4 ) THEN
        b_prod = 0.0
        b_dorm = mic_act_ch4(i,n) * k2_ch4 * cue_ch4 * arh / acclim_ch4(i,n)
      ELSE
        b_dorm = - mic_act_ch4(i,n) * k2_ch4 * cue_ch4 * arh / acclim_ch4(i,n)
      END IF

    ELSE ! Acclimation factor = 0; methanogens die
      substr_cons = 0.0
      b_prod = 0.0
      b_death = mic_mg_m3(i,n) * kd_ch4 * arh
      b_dorm = 0.0
    END IF !acclim_ch4

    ! Update the variables
    substr_mg_m3(i,n) = substr_mg_m3(i,n) + tstep_hr * (substr_prod -          &
                                                        substr_cons + b_death)
    mic_mg_m3(i,n)    = mic_mg_m3(i,n)    + tstep_hr * (b_prod - b_death)
    mic_act_ch4(i,n)  = mic_act_ch4(i,n)  + tstep_hr * (- b_dorm)

    IF (mic_act_ch4(i,n) < alpha_ch4 / cue_ch4)                                &
                                    mic_act_ch4(i,n) = alpha_ch4 / cue_ch4
    IF (mic_act_ch4(i,n) > 1.0)     mic_act_ch4(i,n) = 1.0
    IF (mic_mg_m3(i,n) < 1.0e-6)    mic_mg_m3(i,n) = 1.0e-6
    IF (mic_mg_m3(i,n) > 1.0e7)     mic_mg_m3(i,n) = 1.0e7
    IF (substr_mg_m3(i,n) < 1.0e-6) substr_mg_m3(i,n) = 1.0e-6
    IF (substr_mg_m3(i,n) > 1.0e7)  substr_mg_m3(i,n) = 1.0e7

    ch4_prod(i,n) = substr_cons * frac_ch4
  END DO
END DO

! Unit conversion for prognostic variables
DO j = 1,soil_pts
  i = soil_index(j)
  DO n = 1,dim_ch4layer
    substr_ch4(i,n) = substr_mg_m3(i,n) * 1e-6
    mic_ch4(i,n)   = mic_mg_m3(i,n) * 1e-6
    IF (l_ch4_tlayered) THEN
      substr_ch4(i,n) = substr_ch4(i,n) * dzsoil(n)
      mic_ch4(i,n)    = mic_ch4(i,n) * dzsoil(n)
    END IF
  END DO
END DO

IF ( l_ch4_tlayered ) THEN ! Multiply through by dz to get emissions.
  DO j = 1,soil_pts
    i = soil_index(j)
    DO n = 1,dim_ch4layer
      ch4_prod(i,n) = ch4_prod(i,n) * dzsoil(n)
    END DO
  END DO
END IF

! Calculate total methane emission
DO j = 1,soil_pts
  i = soil_index(j)
  fch4_wetlfrac(i) = SUM( ch4_prod(i,:) * EXP(-tau_ch4 * ztot(:)) * 1e-6       &
                                                                  / 3600.0 )
END DO


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE ch4_microbe
END MODULE ch4_microbe_mod
