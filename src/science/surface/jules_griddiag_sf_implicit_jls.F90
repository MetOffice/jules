! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE jules_griddiag_sf_implicit_mod

USE sf_melt_mod, ONLY: sf_melt
USE screen_tq_mod, ONLY: screen_tq
USE im_sf_pt2_mod, ONLY: im_sf_pt2
USE sice_htf_mod, ONLY: sice_htf

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
                  ModuleName='JULES_GRIDDIAG_SF_IMPLICIT_MOD'

CONTAINS
!  SUBROUTINE JULES_GRIDDIAG_SF_IMPLICIT ----------------------------
!
!  Purpose: Calculate screen level temperature and humidity as well
!           as 10 m winds. Also calculate grid diagnostics required
!           elsewhere in the code
!
!--------------------------------------------------------------------
!    Arguments :-
SUBROUTINE jules_griddiag_sf_implicit (                                        &
! IN values defining field dimensions and subset to be processed :
 land_pts,land_index,nice_use,nsurft,surft_index,surft_pts,                    &
 tile_frac,flandg,l_mr_physics,                                                &
! IN sea/sea-ice data :
 ice_fract,ice_fract_cat_use,u_0,v_0,                                          &
! IN everything not covered so far :
 pstar,qw_1,tl_1,u_1,v_1,du_1,dv_1,                                            &
 resft,rhokh,z1,                                                               &
 z0hssi,z0mssi,z0h_surft,z0m_surft,cdr10m_u,cdr10m_v,                          &
 chr1p5m,chr1p5m_sice,ctctq1,                                                  &
 dqw1_1,dtl1_1,du_star1,dv_star1,cq_cm_u_1,cq_cm_v_1,                          &
 l_correct,emis_surft,                                                         &
 tstar_sice_cat,tstar_ssi,tstar_surft,tstar_sea,fqw_1,                         &
 surf_ht_flux_land,surf_ht_flux_sice_sm,ei_land,ei_sice_sm,                    &
 tstar_land,tstar_ssi_old,tstar_surft_old, taux_1, tauy_1,                     &
! IN variables used to calculate cooling at the screen level
 l_co2_interactive, co2_mmr, co2_3d,rho1, f3_at_p, ustargbm,                   &
! INOUT data :
 ftl_1,olr,                                                                    &
 TScrnDcl_SSI,TScrnDcl_SURFT,tStbTrans,sf_diag,                                &
! OUT Diagnostic not requiring STASH flags :
 surf_ht_flux,                                                                 &
! OUT data required elsewhere in UM system :
 tstar,ei,rhokh_mix                                                            &
 )

USE csigma,                   ONLY:                                            &
 sbcon

USE planet_constants_mod,     ONLY: cp

USE atm_fields_bounds_mod,    ONLY:                                            &
  tdims, udims, vdims, pdims, tdims_s, udims_s, vdims_s

USE theta_field_sizes,        ONLY:                                            &
  t_i_length

USE jules_radiation_mod,      ONLY:                                            &
  l_dolr_land_black

USE jules_sea_seaice_mod,     ONLY:                                            &
  emis_sea, emis_sice

USE sf_diags_mod, ONLY: strnewsfdiag

USE timestep_mod,             ONLY:                                            &
  timestep

USE water_constants_mod,      ONLY:                                            &
  lc, lf

USE parkind1,                 ONLY:                                            &
  jprb, jpim

USE yomhook,                  ONLY:                                            &
  lhook, dr_hook

IMPLICIT NONE
!--------------------------------------------------------------------
!  Inputs :-
! (a) Defining horizontal grid and subset thereof to be processed.
!    Checked for consistency.
!--------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
 land_pts                    ! IN No of land points

! (c) Soil/vegetation/land surface parameters (mostly constant).
INTEGER, INTENT(IN) ::                                                         &
 land_index(land_pts)        ! IN LAND_INDEX(I)=J => the Jth
                             !    point in ROW_LENGTH,ROWS is the
                             !    Ith land point.

INTEGER, INTENT(IN) ::                                                         &
 nsurft                                                                        &
                             ! IN No. of land tiles
,surft_index(land_pts,nsurft)                                                  &
                             ! IN Index of tile points
,surft_pts(nsurft)                                                             &
                             ! IN Number of tile points
,nice_use                    ! IN Number of sea ice categories used
                             !    fully in surface exchange

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 tile_frac(land_pts,nsurft)                                                    &
                             ! IN Tile fractions including
                             ! snow cover in the ice tile.
,flandg(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                             ! IN Land fraction on all pts.
,emis_surft(land_pts,nsurft)
                             ! IN Emissivity for land tiles

! (d) Sea/sea-ice data.
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 ice_fract(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                &
                             ! IN Fraction of gridbox covered by
                             !    sea-ice (decimal fraction).
,ice_fract_cat_use(tdims%i_start:tdims%i_end,                                  &
                   tdims%j_start:tdims%j_end,nice_use)                         &
                             ! IN Sea ice category fractions
                             !    If nice_use=1, this is the total ice
                             !    fraction
,u_0(udims%i_start:udims%i_end,udims%j_start:udims%j_end)                      &
                             ! IN W'ly component of surface
                             !    current (m/s).
,v_0(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)
                             ! IN S'ly component of surface
                             !    current (m/s).

! (f) Atmospheric + any other data not covered so far, incl control.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 pstar(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end)                    &
                             ! IN Surface pressure (Pascals).
,qw_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                             ! IN Total water content
,tl_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                             ! IN Ice/liquid water temperature
,u_1(udims_s%i_start:udims_s%i_end,udims_s%j_start:udims_s%j_end)              &
                             ! IN W'ly wind component (m/s)
,v_1(vdims_s%i_start:vdims_s%i_end,vdims_s%j_start:vdims_s%j_end)
                             ! IN S'ly wind component (m/s)

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 resft(land_pts,nsurft)                                                        &
                             ! IN Total resistance factor.
                             !    fracaero+(1-fracaero)*resfs for
                             !    snow-free land, 1 for snow.
,rhokh(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! IN Grid-box surface exchange
                             !     coefficients
                             !    (not used for JULES)

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
z1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                        &
                             ! IN Height of lowest level (i.e.
                             !    height of middle of lowest
                             !    layer).
,z0hssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
,z0mssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                             ! IN Roughness lengths over sea (m)
,z0h_surft(land_pts,nsurft)                                                    &
                             ! IN Tile roughness lengths for heat
                             !    and moisture (m).
,z0m_surft(land_pts,nsurft)                                                    &
                             ! IN Tile roughness lengths for
                             !    momentum.
,cdr10m_u(udims%i_start:udims%i_end,udims%j_start:udims%j_end)                 &
                             ! IN Ratio of CD's reqd for
                             !    calculation of 10 m wind. On
                             !    U-grid; comments as per RHOKM.
,cdr10m_v(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)                 &
                             ! IN Ratio of CD's reqd for
                             !    calculation of 10 m wind. On
                             !    V-grid; comments as per RHOKM.
,chr1p5m(land_pts,nsurft)                                                      &
                             ! IN Ratio of coefffs for calculation
                             !    of 1.5m temp for land tiles.
,chr1p5m_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)             &
                             ! IN CHR1P5M for sea and sea-ice
                             !    (leads ignored).
,cq_cm_u_1(udims%i_start:udims%i_end,udims%j_start:udims%j_end)                &
                             ! IN Coefficient in U tri-diagonal
                             !    implicit matrix
,cq_cm_v_1(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)                &
                             ! IN Coefficient in V tri-diagonal
                             !    implicit matrix
,du_1(udims_s%i_start:udims_s%i_end,udims_s%j_start:udims_s%j_end)             &
                             ! IN Level 1 increment to u wind
                             !    field
,dv_1(vdims_s%i_start:vdims_s%i_end,vdims_s%j_start:vdims_s%j_end)             &
                             ! IN Level 1 increment to v wind
                             !    field
,ctctq1(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end)                   &
,dqw1_1(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end)                   &
,dtl1_1(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end)                   &
,du_star1(udims_s%i_start:udims_s%i_end,                                       &
          udims_s%j_start:udims_s%j_end)                                       &
,dv_star1(vdims_s%i_start:vdims_s%i_end,                                       &
          vdims_s%j_start:vdims_s%j_end)                                       &
                             ! IN Additional arrays needed by the
                             !    uncond stable BL numerical solver
,tstar_sice_cat(tdims%i_start:tdims%i_end,                                     &
                tdims%j_start:tdims%j_end,nice_use)                            &
                             ! IN   Sea-ice sfc temperature (K).
,tstar_ssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                &
                             ! IN Sea mean sfc temperature (K).
,tstar_surft(land_pts,nsurft)                                                  &
                             ! IN Surface tile temperatures
,tstar_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                &
                             ! IN    Open sea sfc temperature (K).
,fqw_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! IN Moisture flux between layers
                             !    (kg per square metre per sec)
                             !    FQW(,1) is total water flux
                             !    from surface, 'E'.
,surf_ht_flux_land(tdims%i_start:tdims%i_end,                                  &
                   tdims%j_start:tdims%j_end)                                  &
                             ! IN Net downward heat flux at
                             !    surface over land
                             !    fraction of gridbox (W/m2).
,surf_ht_flux_sice_sm(tdims%i_start:tdims%i_end,                               &
                      tdims%j_start:tdims%j_end)                               &
                             ! IN Sea area mean seaice surface heat flux
,ei_land(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                             ! IN Sublimation from lying snow
                             !    (kg/m2/s).
,ei_sice_sm(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)               &
                             ! IN Sea area mean sea ice sublimation
,tstar_land(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)               &
                             ! IN Land mean sfc temperature (K)
,tstar_ssi_old(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)            &
                             ! IN Sea and sea-ice surface temperature
                             !    at beginning of timestep --
                             !    Required only for decoupled diagnosis,
                             !    so allocatable, and local since it is
                             !    used only on the predictor step
,tstar_surft_old(land_pts,nsurft)                                              &
                             ! IN Tile surface temperatures at
                             !    beginning of timestep.
,taux_1(udims%i_start:udims%i_end,udims%j_start:udims%j_end)                   &
                             ! IN   W'ly component of surface
                             !       wind stress (N/sq m). (On
                             !       UV-grid with first and last
                             !       rows undefined or, at
                             !       present, set to missing data
,tauy_1(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)
                             ! IN   S'ly component of surface
                             !       wind stress (N/sq m).  On
                             !       UV-grid; comments as per TAUX

! IN Additional variables for screen-level diagnostics
LOGICAL, INTENT(IN) :: l_co2_interactive
                             ! Flag for interactive 3-D CO2
REAL(KIND=real_jlslsm), INTENT(IN)    :: co2_mmr
                             ! Initial or fixed mass mixing ratio
                             ! of CO2
REAL(KIND=real_jlslsm), INTENT(IN)    ::                                       &
  co2_3d(tdims_s%i_start:tdims_s%i_end,tdims_s%j_start:tdims_s%j_end)
                             ! 3-D field of CO2
REAL(KIND=real_jlslsm), INTENT(IN)    ::                                       &
  rho1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! Density on lowest level
REAL(KIND=real_jlslsm), INTENT(IN)    ::                                       &
  f3_at_p(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! Coriolis parameter
REAL(KIND=real_jlslsm), INTENT(IN)    ::                                       &
  uStarGBM(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! GBM surface friction velocity

LOGICAL, INTENT(IN) ::                                                         &
 l_correct                   ! IN flag used by the new BL solver

LOGICAL, INTENT(IN) ::                                                         &
 l_mr_physics                ! TRUE if mixing ratios used

!--------------------------------------------------------------------
!  In/outs :-
!--------------------------------------------------------------------
TYPE (strnewsfdiag), INTENT(IN OUT) :: sf_diag
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
 ftl_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! INOUT FTL(,K) contains net
                             !       turbulent sensible heat flux
                             !       into layer K from below; so
                             !       FTL(,1) is the surface
                             !       sensible heat, H.(W/m2)
,olr(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! INOUT  TOA - surface upward LW on
                             !        last radiation timestep
                             ! OUT   Corrected TOA outward LW

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  TScrnDcl_SSI(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             !    Decoupled screen-level temperature
                             !    over sea or sea-ice
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  TScrnDcl_SURFT(land_pts,nsurft)
                             !    Decoupled screen-level temperature
                             !    over land tiles
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  tStbTrans(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             !    Time since the transition to stable
                             !    conditions

!--------------------------------------------------------------------
!  Outputs :-
!-1 Diagnostic (or effectively so - includes coupled model requisites):-

!  (a) Calculated anyway (use STASH space from higher level) :-
!--------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 surf_ht_flux(tdims%i_start:tdims%i_end,                                       &
              tdims%j_start:tdims%j_end)                                       &
                             ! OUT Net downward heat flux at
                             !     surface over land and sea-ice
                             !     fraction of gridbox (W/m2).
,tstar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! OUT   GBM surface temperature (K).
,ei(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                       &
                             ! OUT Sublimation from lying snow or
                             !     sea-ice (kg/m2/s).
,rhokh_mix(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! OUT Exchange coeffs for moisture.
                             !     (Not used for JULES)

!--------------------------------------------------------------------
!  Workspace :-
!--------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
 qim_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! Implicit value of first model level
                             ! humidity
,tim_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! Implicit value of first model level
                             ! temperature
,tstar_rad4(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! Effective surface radiative
                             ! temperature for land and sea-ice


!  Local scalars :-

INTEGER ::                                                                     &
 i,j                                                                           &
            ! LOCAL Loop counter (horizontal field index).
,k                                                                             &
            ! LOCAL Tile pointer
,l                                                                             &
            ! LOCAL Land pointer
,n
            ! LOCAL Loop counter (tile index).


INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='JULES_GRIDDIAG_SF_IMPLICIT'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)


  !-----------------------------------------------------------------------
  ! GBM diagnostic calculations
  !-----------------------------------------------------------------------

IF ( .NOT. l_correct ) THEN

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(SHARED)                                                          &
!$OMP PRIVATE(i,j,k,l,n)
!$OMP DO SCHEDULE(STATIC)

  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      qim_1(i,j) = qw_1(i,j) + dqw1_1(i,j) - ctctq1(i,j) * fqw_1(i,j)
      tim_1(i,j) = tl_1(i,j) + dtl1_1(i,j) - ctctq1(i,j) * ftl_1(i,j) / cp
    END DO
  END DO
!$OMP END DO NOWAIT

!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      tstar(i,j) = flandg(i,j) * tstar_land(i,j)                               &
                   + (1.0 - flandg(i,j)) * tstar_ssi(i,j)
      ei(i,j) = flandg(i,j) * ei_land(i,j)                                     &
                + (1.0 - flandg(i,j)) * ei_sice_sm(i,j)
      surf_ht_flux(i,j) = flandg(i,j) * surf_ht_flux_land(i,j)                 &
                          + (1.0 - flandg(i,j)) * surf_ht_flux_sice_sm(i,j)
      rhokh_mix(i,j) = rhokh(i,j)
    END DO
  END DO
!$OMP END DO NOWAIT

  ! TOA outward LW radiation after boundary layer

!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      tstar_rad4(i,j) = 0.0
    END DO
  END DO
!$OMP END DO NOWAIT

!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      !     The contribution from the sea is removed in UM imp_solver to keep it
      !     consistent with the UM radiation scheme (see olr comment there)
      tstar_rad4(i,j) = tstar_rad4(i,j) + (1.0 - flandg(i,j))*                 &
                        (1.0 - ice_fract(i,j)) * emis_sea *                    &
                        tstar_sea(i,j)**4
      DO n = 1,nice_use
        tstar_rad4(i,j) = tstar_rad4(i,j) + (1.0 - flandg(i,j))*               &
                   ice_fract_cat_use(i,j,n) * emis_sice *                      &
                   tstar_sice_cat(i,j,n)**4

      END DO
    END DO
  END DO
!$OMP END DO

  DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
    DO k = 1,surft_pts(n)
      l = surft_index(k,n)
      j=(land_index(l) - 1) / t_i_length + 1
      i = land_index(l) - (j-1) * t_i_length
      !     For historical compatibility, the addjustment of the OLR
      !     may be made with or without the surface emissivity.
      IF (l_dolr_land_black) THEN
        tstar_rad4(i,j) = tstar_rad4(i,j) + flandg(i,j) *                      &
                          tile_frac(l,n) * tstar_surft(l,n)**4
      ELSE
        tstar_rad4(i,j) = tstar_rad4(i,j) + flandg(i,j) *                      &
                          tile_frac(l,n) * emis_surft(l,n) *                   &
                          tstar_surft(l,n)**4
      END IF
    END DO
!$OMP END DO
  END DO

!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      olr(i,j) = olr(i,j) + sbcon * tstar_rad4(i,j)
    END DO
  END DO
!$OMP END DO NOWAIT

!$OMP END PARALLEL

  !-----------------------------------------------------------------------
  !     Specific humidity and temperature at 1.5 metres.
  !-----------------------------------------------------------------------
  CALL screen_tq (                                                             &
    land_pts,nsurft,                                                           &
    land_index,surft_index,surft_pts,flandg,                                   &
    sf_diag,chr1p5m,chr1p5m_sice,pstar,qim_1,resft,                            &
    tile_frac,tim_1,tstar_ssi,tstar_surft,                                     &
    z0hssi,z0h_surft,z0mssi,z0m_surft,z1,                                      &
    timestep,tstar_ssi_old,tstar_surft_old,                                    &
    l_co2_interactive, co2_mmr, co2_3d,                                        &
    f3_at_p, uStarGBM, rho1,                                                   &
    TScrnDcl_SSI,TScrnDcl_SURFT,tStbTrans,                                     &
    l_mr_physics                                                               &
    )

  !-----------------------------------------------------------------------
  ! 9.  Calculate surface latent heat flux.
  !-----------------------------------------------------------------------

  IF (sf_diag%slh) THEN
!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(j,i)                                                             &
!$OMP SHARED(tdims,sf_diag,fqw_1,flandg,ei_land,ei_sice_sm)
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end
        sf_diag%latent_heat(i,j) = lc * fqw_1(i,j)                             &
                                   + lf * (flandg(i,j) * ei_land(i,j) +        &
                                   (1.0 - flandg(i,j)) * ei_sice_sm(i,j))
      END DO
    END DO
!$OMP END PARALLEL DO
  END IF


  !-----------------------------------------------------------------------
  ! Rescale FTL_1 as it should be used to update the botom row of the
  ! discrete equation handled by the new BL solver at the next (2nd)
  ! stage of the scheme.
  !-----------------------------------------------------------------------
!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(j,i)                                                             &
!$OMP SHARED(ftl_1,tdims,cp)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      ftl_1(i,j) = ftl_1(i,j) / cp
    END DO
  END DO
!$OMP END PARALLEL DO

ELSE ! L_correct = true: 2nd stage of the scheme

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(SHARED)                                                          &
!$OMP PRIVATE(j,i)

  !-----------------------------------------------------------------------
  ! Rescale to Watts/m^2 as this is the final call to the imp BL solver
  ! and FTL_1 will be used by stash diagnostics
  !-----------------------------------------------------------------------

!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      ftl_1(i,j) = cp * ftl_1(i,j)
    END DO
  END DO
!$OMP END DO NOWAIT
  !-----------------------------------------------------------------------
  !  U_V will be updated at 2nd stage of the scheme as the equations
  !  providing the implicit surface stresses have been modified
  !  consistently with the new scheme.
  !-----------------------------------------------------------------------
#if !defined(LFRIC)
  ! This code is written for an (i,j) latitude-longitude grid and therefore
  ! not compatible with LFRic, which has its own version
  ! U component of 10m wind
  IF (sf_diag%su10) THEN
!$OMP DO SCHEDULE(STATIC)
    DO j = udims%j_start,udims%j_end
      DO i = udims%i_start,udims%i_end
        sf_diag%u10m(i,j) = (u_1(i,j) + du_star1(i,j) + (du_1(i,j) -           &
                     cq_cm_u_1(i,j) * taux_1(i,j)) -                           &
                     u_0(i,j)) * cdr10m_u(i,j) + u_0(i,j)
      END DO
    END DO
!$OMP END DO NOWAIT
  END IF

  ! V component of 10m wind
  IF (sf_diag%sv10) THEN
!$OMP DO SCHEDULE(STATIC)
    DO j = vdims%j_start,vdims%j_end
      DO i = vdims%i_start,vdims%i_end
        sf_diag%v10m(i,j) = (v_1(i,j) + dv_star1(i,j) + (dv_1(i,j) -           &
                     cq_cm_v_1(i,j) * tauy_1(i,j)) -                           &
                     v_0(i,j)) * cdr10m_v(i,j) + v_0(i,j)
      END DO
    END DO
!$OMP END DO NOWAIT
  END IF

  ! Similar calculations for the neutral winds.
  IF (sf_diag%suv10m_n) THEN
!$OMP DO SCHEDULE(STATIC)
    DO j = udims%j_start,udims%j_end
      DO i = udims%i_start,udims%i_end
        sf_diag%u10m_n(i,j) = (u_1(i,j) + du_star1(i,j) + (du_1(i,j) -         &
                     cq_cm_u_1(i,j) * taux_1(i,j)) -                           &
                     u_0(i,j)) * sf_diag%cdr10m_n_u(i,j) + u_0(i,j)
      END DO
    END DO
!$OMP END DO NOWAIT

!$OMP DO SCHEDULE(STATIC)
    DO j = vdims%j_start,vdims%j_end
      DO i = vdims%i_start,vdims%i_end
        sf_diag%v10m_n(i,j) = (v_1(i,j) + dv_star1(i,j) + (dv_1(i,j) -         &
                     cq_cm_v_1(i,j) * tauy_1(i,j)) -                           &
                     v_0(i,j)) * sf_diag%cdr10m_n_v(i,j) + v_0(i,j)
      END DO
    END DO
!$OMP END DO NOWAIT
  END IF
#endif
!$OMP END PARALLEL

END IF ! IF .NOT. L_correct

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE jules_griddiag_sf_implicit
END MODULE jules_griddiag_sf_implicit_mod
