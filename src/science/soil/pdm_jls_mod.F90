! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    SUBROUTINE PDM-----------------------------------------------------------

! Description:
!     Calculates the saturation excess runoff using PDM.
!     See Moore, 1985

MODULE pdm_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='PDM_MOD'

CONTAINS

SUBROUTINE pdm(npnts, nshyd, soil_pts, timestep, soil_index,                   &
               slope_gb, tot_tfall, snow_melt, infiltro,                       &
               v_sat, sthu, sthf, satexro)

USE jules_hydrology_mod,  ONLY:                                                &
  b_pdm, dz_pdm, s_pdm, slope_pdm_max,                                         &
  l_spdmvar  ! flag to used slope dependent or fixed s_pdm parameter

USE water_constants_mod,  ONLY:                                                &
  rho_water  ! density of pure water (kg/m3)

USE jules_soil_mod,       ONLY:                                                &
  dzsoil     !  Thicknesses of the soil layers (m)

USE parkind1,             ONLY: jprb, jpim
USE yomhook,              ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  npnts,                                                                       &
    ! Number of gridpoints.
  nshyd,                                                                       &
    ! Number of soil moisture levels.
  soil_pts
    ! Number of soil points

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  timestep
    ! Model timestep (s).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  soil_index(npnts)
    ! Array of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  slope_gb(npnts),                                                             &
    ! Terrain slope.
  tot_tfall(npnts),                                                            &
    ! Cumulative canopy throughfall (kg/m2/s).
  snow_melt(npnts),                                                            &
    ! Snow melt (kg/m2/s).
  infiltro(npnts),                                                             &
    ! Surface runoff by infiltration excess (kg/m2/s).
  v_sat(npnts,nshyd),                                                          &
    ! Volumetric soil moisture concentration at saturation
    ! (m^3 water/m^3 soil).
  sthu(npnts,nshyd),                                                           &
    ! Unfrozen soil moisture content of each layer as fraction of saturation.
  sthf(npnts,nshyd)
    ! Frozen soil moisture content of each layer as fraction of saturation.

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  satexro(npnts)
    ! Saturation excess runoff (kg/m2/s).


!-----------------------------------------------------------------------------
! Local scalar variables:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, j, n

REAL(KIND=real_jlslsm) ::                                                      &
  p_in,                                                                        &
    ! Water reaching soil surface (m^3/m^3).
  thcap_max,                                                                   &
    ! Used in equation for TH_STAR (m^3/m^3).
  th_star,                                                                     &
    ! Critical moisture storage (m^3/m^3).
  th_star_p,                                                                   &
    ! Crit. TH_STAR after rain input (m^3/m^3).
  sth_pdm,                                                                     &
    ! Soil moisture for PDM / sat. value
  sthmax_pdm,                                                                  &
    ! Maximum Possible above threshold soil moisture for PDM / sat. value.
  dz_pdm_t,                                                                    &
    ! Soil depth temporary.
  dz_pdm_b,                                                                    &
    ! Soil depth temporary.
  slope_pdm_min,                                                               &
    ! Minimum topographic slope (deg) in the linear function of slope to
    ! calculate S0/Smax. Slopes of this value and below will produce
    ! S0/Smax=1.
  s_pdm_min,                                                                   &
    ! Minimum soil moisture (as fraction of saturation) for PDM.
  s_pdm_max
    ! Maximum soil moisture (as fraction of saturation) for PDM.

!-----------------------------------------------------------------------------
! Local array variables:
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  s_pdm_grid(npnts)
    ! Minimum soil moisture content as a fraction of saturarion necessary
    ! to produce saturation excess runoff, So/Smax in PDM formulation
    ! (calculated as a function of slope when l_spdmvar is on).

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='PDM'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Calculate the slope-dependent S_PDM field from terrain slope data.
!-----------------------------------------------------------------------------

IF (l_spdmvar) THEN

  slope_pdm_min = 0.0
  s_pdm_min     = 0.0
  s_pdm_max     = 0.99

  DO i = 1,npnts
    IF (slope_gb(i) < slope_pdm_min) THEN
      s_pdm_grid(i) = s_pdm_max
    ELSE IF (slope_gb(i) <= slope_pdm_max) THEN
      s_pdm_grid(i) = s_pdm_min + (1.0 - (slope_gb(i) - slope_pdm_min)         &
                                   / (slope_pdm_max - slope_pdm_min))          &
                                * (s_pdm_max - s_pdm_min)
    ELSE
      s_pdm_grid(i) = s_pdm_min
    END IF
  END DO

ELSE

  s_pdm_grid(:) = MIN(0.99,s_pdm)

END IF

!-----------------------------------------------------------------------------

DO j = 1,soil_pts
  i = soil_index(j)

  p_in = (tot_tfall(i) + snow_melt(i) - infiltro(i))                           &
         * timestep / (dz_pdm * rho_water)

  IF (p_in >  0.0) THEN

    ! DZ_PDM_T is the residual soil depth for the PDM below the top
    ! of the current layer.

    dz_pdm_t = dz_pdm
    sth_pdm  = 0.0
    IF (s_pdm_grid(i) > 0.0) sthmax_pdm = 0.0

    DO n = 1,nshyd

      dz_pdm_b = dz_pdm_t - dzsoil(n)
      ! DZ_PDM_B is the residual soil depth for the PDM below the
      ! bottom of the current layer.

      IF (dz_pdm_b  >=  0.0) THEN
        IF (s_pdm_grid(i)  >  0.0) THEN
          sthmax_pdm = sthmax_pdm + (1.0 - s_pdm_grid(i)) * dzsoil(n)
          sth_pdm    = sth_pdm                                                 &
                       + MAX(sthu(i,n) + sthf(i,n) - s_pdm_grid(i),0.0)        &
                       * dzsoil(n)
        ELSE
          sth_pdm = sth_pdm + (sthu(i,n) + sthf(i,n)) * dzsoil(n)
        END IF
      ELSE
        IF (s_pdm_grid(i)  >  0.0) THEN
          sthmax_pdm = sthmax_pdm + (1.0 - s_pdm_grid(i)) * MAX(0.0,dz_pdm_t)
          sth_pdm    = sth_pdm                                                 &
                       + MAX(sthu(i,n) + sthf(i,n) - s_pdm_grid(i),0.0)        &
                       * MAX(0.0,dz_pdm_t)
        ELSE
          sth_pdm = sth_pdm + (sthu(i,n) + sthf(i,n)) * MAX(0.0,dz_pdm_t)
        END IF
      END IF

      dz_pdm_t = dz_pdm_b

    END DO ! Loop over soil layers

    sth_pdm = sth_pdm / dz_pdm
    IF (s_pdm_grid(i) > 0.0) sthmax_pdm = sthmax_pdm / dz_pdm

    IF (s_pdm_grid(i) > 0.0) THEN
      thcap_max = (b_pdm+1.0) * v_sat(i,1) * sthmax_pdm
    ELSE
      thcap_max = (b_pdm+1.0) * v_sat(i,1)
    END IF

    IF (sth_pdm > 1.0) sth_pdm = 1.0

    IF (s_pdm_grid(i) > 0.0) THEN
      th_star = thcap_max *                                                    &
                (1.0 - (1.0 - sth_pdm / sthmax_pdm)**(1.0 / (b_pdm+1.0)))
    ELSE
      th_star = thcap_max * (1.0 - (1.0 - sth_pdm)**(1.0 / (b_pdm+1.0)))
    END IF
    th_star_p = th_star + p_in

    IF (th_star_p < thcap_max) THEN
      IF (s_pdm_grid(i) > 0.0) THEN
        satexro(i) = p_in - v_sat(i,1) * ((sthmax_pdm) *                       &
                     (1.0 - (1.0 - th_star_p / thcap_max)                      &
                     **(b_pdm+1.0)) - sth_pdm)
      ELSE
        satexro(i) = p_in - v_sat(i,1) * (1.0 - (1.0 - th_star_p / thcap_max)  &
                     **(b_pdm+1.0) - sth_pdm)
      END IF
    ELSE
      IF (s_pdm_grid(i) > 0.0) THEN
        satexro(i) = p_in - v_sat(i,1) * (sthmax_pdm - sth_pdm)
      ELSE
        satexro(i) = p_in - v_sat(i,1) * (1.0 - sth_pdm)
      END IF
    END IF

    IF (satexro(i) < 0.0) satexro(i) = 0.0

    satexro(i) = satexro(i) / timestep * (dz_pdm * rho_water)

  END IF

END DO


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE pdm
END MODULE pdm_mod
