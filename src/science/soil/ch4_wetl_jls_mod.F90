! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    SUBROUTINE CH4_WETL-----------------------------------------------

! Description:
!     Calculates methane emissions from wetland area.

MODULE ch4_wetl_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='CH4_WETL_MOD'

CONTAINS
SUBROUTINE ch4_wetl(npnts, sm_levels, soil_pts, dim_cs1, timestep,             &
                    l_ch4_tlayered, soil_index,                                &
                    tsoil, tsoil_d, cs_ch4, resp_s, npp, f_wetl, sthu, bexp,   &
                    fch4_wetl, fch4_wetl_cs, fch4_wetl_npp, fch4_wetl_resps,   &
                    substr_ch4, mic_ch4, mic_act_ch4, acclim_ch4, cs)

!Use in relevant subroutines
USE ch4_tdep_mod, ONLY: ch4_tdep
USE ch4_tdep_layers_mod, ONLY: ch4_tdep_layers
USE ch4_microbe_mod, ONLY: ch4_microbe

USE jules_soil_biogeochem_mod, ONLY:                                           &
  kaps_4pool, ch4_substrate,                                                   &
  ch4_substrate_npp,  ch4_substrate_soil, ch4_substrate_soil_resp,             &
  t0_ch4, q10_ch4_cs, q10_ch4_npp, q10_ch4_resps, dim_ch4layer, l_ch4_microbe, &
  ch4_cpow, ev_ch4, q10_ev_ch4

USE ancil_info, ONLY: dim_cslayer

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
  sm_levels,                                                                   &
    ! Soil layers
  soil_pts,                                                                    &
    ! Number of soil points.
  dim_cs1
    ! Number of soil carbon pools

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
  tsoil(npnts,sm_levels),                                                      &
    ! Layered soil temperature (K).
  tsoil_d(npnts),                                                              &
    ! Diagnosed soil temp to 1 metre (K).
  cs_ch4(npnts),                                                               &
    ! Soil carbon used in CH4 wetlands if single-pool model is used (kg C/m2).
  resp_s(npnts,dim_cslayer,dim_cs1),                                           &
    ! Soil respiration in pools (kg C/m2/s).
  npp(npnts),                                                                  &
    ! Gridbox mean net primary productivity (kg C/m2/s).
  f_wetl(npnts),                                                               &
    ! Wetland fraction.
  sthu(npnts,sm_levels),                                                       &
    ! Unfrozen soil moisture of each layer as a fraction of saturation.
  bexp(npnts,sm_levels)
    ! Brooks & Corey exponent.

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  fch4_wetl(npnts),                                                            &
    ! Scaled methane flux (as used in atmos chem)
    ! (10^-9 kg C/m2/s).
  fch4_wetl_cs(npnts),                                                         &
    ! Scaled methane flux (soil carbon substrate) (kg C/m2/s).
  fch4_wetl_npp(npnts),                                                        &
    ! Scaled methane flux (npp substrate) (kg C/m2/s).
  fch4_wetl_resps(npnts),                                                      &
    ! Scaled methane flux (soil respiration substrate) (kg C/m2/s).
  substr_ch4(npnts,dim_ch4layer),                                              &
    ! Dissolved substrate that methanogens consume (kg C/m2)
  mic_ch4(npnts,dim_ch4layer),                                                 &
    ! Methanogenic biomass (kg C/m2)
  mic_act_ch4(npnts,dim_ch4layer),                                             &
    ! Activity level of methanogenic biomass (fraction)
  acclim_ch4(npnts,dim_ch4layer),                                              &
    ! Acclimation factor for microbial trait adaptation
  cs(npnts,dim_cslayer,dim_cs1)
    ! Soil carbon
    ! For 4-pool C model (dim_cs1=4), the pools are DPM, RPM, biomass and humus
    ! (kg C/m2).

!-----------------------------------------------------------------------------
! Local scalar variables:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, j, k, n, nn

REAL(KIND=real_jlslsm) ::                                                      &
  const_tdep_cs,                                                               &
    ! T and Q10(0) dependent function.
  const_tdep_npp,                                                              &
    ! T and Q10(0) dependent function.
  const_tdep_resps,                                                            &
    ! T and Q10(0) dependent function.
  sumkaps
    ! Sum of kaps_4pool values.

!-----------------------------------------------------------------------------
! Local array variables:
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  cs_eff(npnts),                                                               &
    ! Effective soil carbon (kg C/m2).
  resp_s_tot(npnts),                                                           &
    ! Soil respiration total (kg C/m2/s).
  ztot(dim_ch4layer),                                                          &
    ! Depth at center of each soil layer (m).
  kaps_weight(dim_cs1),                                                        &
    ! Working variable for weighting by kappas.
  decomp_wetlfrac_cs(npnts,dim_ch4layer),                                      &
    ! Anaerobic decomposition of soil carbon substrate over the wetland
    ! fraction of the grid box (i.e. this is not scaled by wetland fraction)
    ! (kg C/m2/s).
  decomp_wetlfrac_npp(npnts,dim_ch4layer),                                     &
    ! Anaerobic decomposition of NPP substrate over the wetland fraction of
    ! the grid box (i.e. this is not scaled by wetland fraction) (kg C/m2/s).
  decomp_wetlfrac_resps(npnts,dim_ch4layer),                                   &
    ! Anaerobic decomposition of soil respiration substrate over the wetland
    ! fraction of the grid box (i.e. this is not scaled by wetland fraction)
    ! (kg C/m2/s).
  decomp_wetlfrac_mic(npnts,dim_ch4layer),                                     &
    ! Anaerobic decomposition of whichever substrate is used for microbial
    ! calculation, over the wetland fraction of the grid box (i.e. this is not
    ! scaled by wetland fraction) (kg C/m2/s).
  fch4_wetlfrac_mic(npnts),                                                    &
    ! Methane flux produced by microbial scheme, over the wetland fraction of
    ! the grid box (i.e. this is not scaled by wetland fraction) (kg C/m2/s).
  tsoil_ch4lyr(npnts,dim_ch4layer),                                            &
    ! Soil temperature on appropriate number of layers (K)
  sthu_ch4lyr(npnts,dim_ch4layer),                                             &
    ! Soil moisture on appropriate number of layers (fraction of saturation)
  bexp_ch4lyr(npnts,dim_ch4layer)
    ! Brooks & Corey exponents on appropriate number of layers.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CH4_WETL'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Calculate an effective soil carbon for wetland methane emission.
!-----------------------------------------------------------------------------
decomp_wetlfrac_cs(:,:)    = 0.0
decomp_wetlfrac_npp(:,:)   = 0.0
decomp_wetlfrac_resps(:,:) = 0.0
decomp_wetlfrac_mic(:,:) = 0.0
fch4_wetlfrac_mic(:) = 0.0

IF (l_ch4_tlayered) THEN
  ! Calculate soil layer depths from layer thicknesses.
  ztot(1) = dzsoil(1) * 0.5
  DO n = 2,sm_levels
    ztot(n) = ztot(n-1) + 0.5 * ( dzsoil(n-1) + dzsoil(n) )
  END DO
ELSE
  ztot = 0.5
END IF

IF ( dim_cs1 > 1 ) THEN
  sumkaps = 0.0
  DO k = 1,dim_cs1
    sumkaps = sumkaps + kaps_4pool(k)
  END DO
  ! Calculate weighting according to kappas.
  DO k = 1,dim_cs1
    kaps_weight(k) = kaps_4pool(k) / sumkaps
  END DO
  IF ( .NOT. l_ch4_tlayered ) THEN
    ! Weight each pool by specific respiration rate.
    DO j = 1,soil_pts
      i = soil_index(j)
      cs_eff(i) = 0.0
      DO k = 1,dim_cs1
        DO n = 1,dim_cslayer
          cs_eff(i) = cs_eff(i) + (cs(i,n,k)**ch4_cpow) * kaps_weight(k)
        END DO
      END DO
    END DO
  END IF !l_ch4_tlayered

ELSE !dim_cs1==1

  kaps_weight(1) = 1.0
  IF ( .NOT. l_ch4_tlayered ) THEN
    ! Use the single soil carbon pool.
    DO j = 1,soil_pts
      i = soil_index(j)
      cs_eff(i) = cs_ch4(i)
    END DO
  END IF !l_ch4_tlayered

END IF  !dim_cs1

! Calculate total soil respiration
DO j = 1,soil_pts
  i = soil_index(j)
  resp_s_tot(i) = 0.0
  DO k = 1,dim_cs1
    DO n = 1,dim_cslayer
      IF ( resp_s(i,n,k) > 0.0 )                                               &
        resp_s_tot(i) = resp_s_tot(i) + resp_s(i,n,k)
    END DO
  END DO
END DO

! Assign soil temperature and moisture with appropriate dimension
IF ( l_ch4_tlayered ) THEN
  tsoil_ch4lyr = tsoil
  sthu_ch4lyr = sthu
  bexp_ch4lyr = bexp
ELSE
  tsoil_ch4lyr(:,1) = tsoil_d
  ! Find closest point to 0.5m (centre of single 'top 1m' layer)
  nn = 1
  DO n = 1,sm_levels-1
    IF ( ABS( SUM(dzsoil(1:n)) - 0.5 * dzsoil(n) - 0.5 ) >                     &
         ABS( SUM(dzsoil(1:(n+1))) - 0.5 * dzsoil(n+1) - 0.5 ) ) THEN
      nn = nn + 1
    ELSE
      EXIT
    END IF
  END DO
  sthu_ch4lyr(:,1) = sthu(:,nn)
  bexp_ch4lyr(:,1) = bexp(:,nn)
END IF

IF (l_ch4_microbe) THEN
  ! Update microbial acclimation factor
  DO j = 1,soil_pts
    i = soil_index(j)
    DO n = 1,dim_ch4layer
      acclim_ch4(i,n) = acclim_ch4(i,n) +  ( q10_ev_ch4**(0.1 *                &
                        (tsoil_ch4lyr(i,n)  - t0_ch4)) - acclim_ch4(i,n) ) *   &
                                   ( timestep / (86400.0 * 365.0 * ev_ch4) )
    END DO
  END DO
END IF
!-----------------------------------------------------------------------------
! Calculate scaled wetland methane emission.
!-----------------------------------------------------------------------------
const_tdep_cs = t0_ch4 * LOG(q10_ch4_cs)
const_tdep_npp = t0_ch4 * LOG(q10_ch4_npp)
const_tdep_resps = t0_ch4 * LOG(q10_ch4_resps)

IF ( l_ch4_tlayered ) THEN
  CALL ch4_tdep_layers(ch4_substrate, dim_cs1, dim_cslayer, npnts,             &
                       sm_levels, soil_pts, const_tdep_cs, const_tdep_npp,     &
                       const_tdep_resps, timestep, t0_ch4, soil_index,         &
                       tsoil, resp_s, resp_s_tot,                              &
                       npp, f_wetl, ztot, dzsoil, kaps_weight,                 &
                       decomp_wetlfrac_cs,                                     &
                       decomp_wetlfrac_npp, decomp_wetlfrac_resps, cs )
ELSE !NOT l_ch4_tlayered
  CALL ch4_tdep(npnts, soil_pts, const_tdep_cs, const_tdep_npp,                &
                const_tdep_resps, t0_ch4, soil_index, tsoil_d, cs_eff,         &
                resp_s_tot, npp,                                               &
                decomp_wetlfrac_cs, decomp_wetlfrac_npp, decomp_wetlfrac_resps)
END IF !l_ch4_tlayered

!-----------------------------------------------------------------------------
! Include microbial dynamics if enabled
IF ( l_ch4_microbe ) THEN
  IF ( ch4_substrate == ch4_substrate_soil ) THEN
    decomp_wetlfrac_mic = decomp_wetlfrac_cs
  ELSE IF ( ch4_substrate == ch4_substrate_npp ) THEN
    decomp_wetlfrac_mic = decomp_wetlfrac_npp
  ELSE ! ch4_substrate==ch4_substrate_soil_resp
    decomp_wetlfrac_mic = decomp_wetlfrac_resps
  END IF

  CALL ch4_microbe(npnts, soil_pts, dim_ch4layer, timestep,                    &
                   l_ch4_tlayered, soil_index,                                 &
                   tsoil_ch4lyr, decomp_wetlfrac_mic, ztot, acclim_ch4,        &
                   sthu_ch4lyr, bexp_ch4lyr,                                   &
                   mic_ch4, mic_act_ch4, substr_ch4, fch4_wetlfrac_mic)

  ! Fill appropriate fch4 variable
  IF ( ch4_substrate == ch4_substrate_soil ) THEN
    fch4_wetl_cs    = fch4_wetlfrac_mic
    fch4_wetl_npp   = SUM( decomp_wetlfrac_npp, 2 )
    fch4_wetl_resps = SUM( decomp_wetlfrac_resps, 2 )
  ELSE IF ( ch4_substrate == ch4_substrate_npp ) THEN
    fch4_wetl_cs    = SUM( decomp_wetlfrac_cs, 2 )
    fch4_wetl_npp   = fch4_wetlfrac_mic
    fch4_wetl_resps = SUM( decomp_wetlfrac_resps, 2 )
  ELSE ! ch4_substrate==ch4_substrate_soil_resp
    fch4_wetl_cs   = SUM( decomp_wetlfrac_cs, 2 )
    fch4_wetl_npp   = SUM( decomp_wetlfrac_npp, 2 )
    fch4_wetl_resps = fch4_wetlfrac_mic
  END IF

ELSE
  ! Methane emission is just equal to decomposition rate of organic matter to DOC
  fch4_wetl_cs = SUM( decomp_wetlfrac_cs, 2 )
  fch4_wetl_npp   = SUM( decomp_wetlfrac_npp, 2 )
  fch4_wetl_resps = SUM( decomp_wetlfrac_resps, 2 )

END IF !l_ch4_microbe

!-----------------------------------------------------------------------------
! Finally multiply by wetland area
DO j = 1,soil_pts
  i = soil_index(j)
  fch4_wetl_cs(i)    = fch4_wetl_cs(i) * f_wetl(i)
  fch4_wetl_npp(i)   = fch4_wetl_npp(i) * f_wetl(i)
  fch4_wetl_resps(i) = fch4_wetl_resps(i) * f_wetl(i)
END DO

IF ( ch4_substrate == ch4_substrate_soil ) THEN
  fch4_wetl(:) = 1.0e9 * fch4_wetl_cs(:)
ELSE IF ( ch4_substrate == ch4_substrate_npp ) THEN
  fch4_wetl(:) = 1.0e9 * fch4_wetl_npp(:)
ELSE IF ( ch4_substrate == ch4_substrate_soil_resp ) THEN
  fch4_wetl(:) = 1.0e9 * fch4_wetl_resps(:)
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE ch4_wetl
END MODULE ch4_wetl_mod
