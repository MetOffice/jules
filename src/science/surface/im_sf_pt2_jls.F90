! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE im_sf_pt2_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='IM_SF_PT2_MOD'

CONTAINS
!  SUBROUTINE IM_SF_PT2 ---------------------------------------------

!  Purpose: Calculate implicit increments to surface variables
!           for the unconditionally stable and non-oscillatory
!           BL numerical solver.

!---------------------------------------------------------------------
!  Arguments :-
SUBROUTINE im_sf_pt2 (                                                         &
 land_pts,land_index,nsurft,surft_index,surft_pts                              &
,flandg,tile_frac,snow_surft,nice_use,ice_fract,ice_fract_cat                  &
,r_gamma,gamma1_in,gamma2_in,alpha1,alpha1_sea,alpha1_sice                     &
,ashtf_prime,ashtf_prime_sea,ashtf_prime_surft                                 &
,resft,dtstar_surft,dtstar_sea,dtstar_sice                                     &
,rhokm_u_1,rhokm_v_1,rhokh_1,rhokh1_sice,rhokh1_sea                            &
,ctctq1,dqw1_1,dtl1_1,cq_cm_u_1                                                &
,cq_cm_v_1,du_1,dv_1,du_star1,dv_star1,flandg_u,flandg_v                       &
,fqw_gb,ftl_gb                                                                 &
,taux_1,taux_land,taux_land_star,taux_ssi,taux_ssi_star,tauy_1                 &
,tauy_land,tauy_land_star,tauy_ssi,tauy_ssi_star                               &
,fqw_surft,epot_surft,ftl_surft,fqw_ice,ftl_ice,e_sea,h_sea                    &
,l_correct                                                                     &
)

#if defined(UM_JULES)
USE model_domain_mod, ONLY: model_type, mt_single_column, mt_lfric
#endif

USE atm_fields_bounds_mod, ONLY: tdims, pdims, udims, vdims, udims_s, vdims_s
USE theta_field_sizes, ONLY: t_i_length

USE planet_constants_mod, ONLY: cp
USE water_constants_mod, ONLY: lc
USE jules_surface_mod, ONLY: ls, l_epot_corr
USE jules_sea_seaice_mod, ONLY: l_use_dtstar_sea, beta_evap
USE jules_science_fixes_mod, ONLY: l_dtcanfix

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

LOGICAL ::                                                                     &
 l_correct                   ! Flag for BL solver

INTEGER ::                                                                     &
 land_pts                                                                      &
                             ! IN Total number of land points.
,land_index(land_pts)                                                          &
                             ! IN Index of land points.
,nsurft                                                                        &
                             ! IN Number of land surface tiles.
,surft_index(land_pts,nsurft)                                                  &
                             ! IN Index of tile points.
,surft_pts(nsurft)                                                             &
                             ! IN Number of tiles.
,nice_use                    ! IN Number of sea ice categories fully
                             !    used in surface exchange


REAL(KIND=real_jlslsm) ::                                                      &
 flandg(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                             ! IN Land fraction
,tile_frac(land_pts,nsurft)                                                    &
                             ! IN Tile fraction
,snow_surft(land_pts,nsurft)                                                   &
                             ! IN Lying snow on land tiles (kg/m2)
,ice_fract(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                &
                             ! IN Fraction of grid-box which is
!                                  !    sea-ice (decimal fraction).
,ice_fract_cat(tdims%i_start:tdims%i_end,                                      &
               tdims%j_start:tdims%j_end,nice_use)                             &
                             ! IN Fraction of grid-box which is
!                            !    sea-ice (decimal fraction) per
                             !    category if nice_use>1
,r_gamma                                                                       &
                             ! IN Implicit weighting.
,gamma1_in(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end)                &
,gamma2_in(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end)                &
                             ! IN Implicit weights for uncond.
                             !    stable non-oscillatory BL solver
,alpha1(land_pts,nsurft)                                                       &
                             ! IN Gradient of saturated specific
!                                  !    humidity with respect to
!                                  !    temperature between the bottom
!                                  !    model layer and the surface.
,alpha1_sea(tdims%i_start:tdims%i_end,                                         &
            tdims%j_start:tdims%j_end)                                         &
                             ! IN ALPHA1 for sea
,alpha1_sice(tdims%i_start:tdims%i_end,                                        &
             tdims%j_start:tdims%j_end,nice_use)                               &
                             ! IN ALPHA1 for sea-ice
,ashtf_prime(tdims%i_start:tdims%i_end,                                        &
             tdims%j_start:tdims%j_end,nice_use)                               &
                             ! IN Coefficient to calculate surface
!                                  !    heat flux into soil or sea-ice
!                                  !    (W/m2/K).
,ashtf_prime_sea(tdims%i_start:tdims%i_end,                                    &
                 tdims%j_start:tdims%j_end)                                    &
                             ! IN Coefficient to calculate surface
!                                  !    heat flux into open sea (W/m2/K).

,ashtf_prime_surft(land_pts,nsurft)                                            &
                             ! IN Coefficient to calculate heat
!                                  !    flux into land tiles (W/m2/K).
,dtstar_surft(land_pts,nsurft)                                                 &
                             ! INOUT Change in TSTAR over timestep
!                                  !    for land tiles
,dtstar_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)               &
                             ! INOUT Change in TSTAR over timestep
!                                  !    for open sea
,dtstar_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,nice_use)     &
                             ! INOUT Change in TSTAR over timestep
!                                  !    for sea-ice
,resft(land_pts,nsurft)                                                        &
                             ! IN Total resistance factor
,epot_surft(land_pts,nsurft)                                                   &
                             ! IN surface tile potential
!                                  !    evaporation
,e_epot_surft(land_pts,nsurft)                                                 &
                             ! Work ratio of explicit
!                                  !      EPOT/E
!                                  !    evaporation

,rhokm_u_1(udims%i_start:udims%i_end,udims%j_start:udims%j_end)                &
                             ! IN Level 1 exchange coefficient for
!                                  !    momentum
,rhokm_v_1(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)                &
                             ! IN Level 1 exchange coefficient for
!                                  !    momentum
,rhokh_1(land_pts,nsurft)                                                      &
                             ! IN Surface exchange coeffs for FTL
,rhokh1_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,              &
                                                        nice_use)              &
                             ! IN Sea-ice surface exchange
,rhokh1_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)               &
                             ! IN Sea surface exchange
,ctctq1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
,cq_cm_u_1(udims%i_start:udims%i_end,udims%j_start:udims%j_end)                &
                             ! IN Coefficient in U and V
!                                  !     tri-diagonal implicit matrix
,cq_cm_v_1(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)                &
                             ! IN Coefficient in U and V
!                                  !     tri-diagonal implicit matrix
,dqw1_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                             ! IN Incr obtained by semi-implicit inte
,dtl1_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                             ! IN Incr obtained by semi-implicit inte
,du_1(udims_s%i_start:udims_s%i_end,                                           &
      udims_s%j_start:udims_s%j_end)                                           &
                             ! IN Level 1 increment to u wind
!                                  !    field
,dv_1(vdims_s%i_start:vdims_s%i_end,                                           &
      vdims_s%j_start:vdims_s%j_end)                                           &
                             ! IN Level 1 increment to v wind
!                                  !    field
,du_star1(udims_s%i_start:udims_s%i_end,                                       &
          udims_s%j_start:udims_s%j_end)                                       &
,dv_star1(vdims_s%i_start:vdims_s%i_end,                                       &
          vdims_s%j_start:vdims_s%j_end)                                       &
,flandg_u(udims%i_start:udims%i_end,udims%j_start:udims%j_end)                 &
                             ! IN Land fraction on U grid.
,flandg_v(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)
                             ! IN Land fraction on V grid.


REAL(KIND=real_jlslsm) ::                                                      &
 fqw_gb(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                             ! INOUT Grid-box value of QW flux at
!                                  !       Kg/sq m/s
,ftl_gb(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                             ! INOUT Grid-box value of TL flux at
!                                  !       i.e. H/Cp where H is sensible
!                                  !       in W per sq m).
,taux_1(udims%i_start:udims%i_end,udims%j_start:udims%j_end)                   &
                             ! OUT   x-component of turbulent
!                                  !       stress at surface.
,taux_land(udims%i_start:udims%i_end,udims%j_start:udims%j_end)                &
                             ! INOUT x-component of turbulent
!                                  !       stress at land surface.
,taux_ssi(udims%i_start:udims%i_end,udims%j_start:udims%j_end)                 &
                             ! INOUT x-component of turbulent
!                                  !       stress at sea surface.
,tauy_1(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)                   &
                             ! OUT   y-component of turbulent
!                                  !       stress at surface.
,tauy_land(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)                &
                             ! INOUT y-component of turbulent
!                                  !       stress at land surface.
,tauy_ssi(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)                 &
                             ! INOUT y-component of turbulent
!                                  !       stress at sea surface.
,taux_land_star(udims%i_start:udims%i_end,                                     &
                udims%j_start:udims%j_end)                                     &
,taux_ssi_star(udims%i_start:udims%i_end,                                      &
               udims%j_start:udims%j_end)                                      &
,tauy_land_star(vdims%i_start:vdims%i_end,                                     &
                vdims%j_start:vdims%j_end)                                     &
,tauy_ssi_star(vdims%i_start:vdims%i_end,                                      &
               vdims%j_start:vdims%j_end)                                      &
                             ! INOUT as above, temporarily needed
                             !       by predictor stage of the
                             !       new uncond stable BL solver
,fqw_surft(land_pts,nsurft)                                                    &
                             ! INOUT Tile flux of QW. Kg/sq m/s
,ftl_surft(land_pts,nsurft)                                                    &
                             ! INOUT Tile flux of TL
,e_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! OUT Evaporation from sea times
!                                  !       leads fraction (kg/m2/s).
!                                  !       Zero over land.
,h_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                                   ! OUT Surface sensible heat flux ov
!                                  !       sea times leads fraction (W/m
!                                  !       Zero over land.
,fqw_ice(tdims%i_start:tdims%i_end,                                            &
         tdims%j_start:tdims%j_end,nice_use)                                   &
                             ! INOUT  surface flux of QW for
!                                  !  sea-ice fraction of gridsquare.
,ftl_ice(tdims%i_start:tdims%i_end,                                            &
         tdims%j_start:tdims%j_end,nice_use)
                             ! INOUT surface flux of TL for
!                                  !  sea-ice fraction of gridsquare.

!  Local and other symbolic constants :-

! Workspace :-
REAL(KIND=real_jlslsm) ::                                                      &
 rhokpm(land_pts,nsurft)                                                       &
                             !  Surface exchange coeff for tiles
,rhokpm_sice(tdims%i_start:tdims%i_end,                                        &
             tdims%j_start:tdims%j_end,nice_use)                               &
                             !  Sea-ice surface exchange coeff
,rhokpm_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)               &
                             !  Sea surface exchange coeff
,lat_ht                                                                        &
          ! Latent heat of evaporation for snow-free land
!               ! or sublimation for snow-covered land and ice.
,apart(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,2)                  &
                             ! Temporary array
,bpart(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,2)                  &
                             ! Temporary array
,recip(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! Temporary array
,ftl_land(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                 &
                             ! Temporary array
,fqw_land(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                 &
                             ! Temporary array
,ftl_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                 &
                             ! Temporary array (sum over all cats)
,fqw_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! Temporary array (sum over all cats)

LOGICAL ::                                                                     &
 epot_calc(land_pts,nsurft)  ! flag to connect first and second
                             ! epot calculations

!  Local scalars :-
INTEGER ::                                                                     &
 i,j                                                                           &
          ! Loop counter (horizontal field index).
,k                                                                             &
          ! Loop counter (tile index).
,l                                                                             &
          ! Loop counter (horizontal land index).
,n                                                                             &
          ! Loop counter (tile counter).
,offset   ! offset

REAL(KIND=real_jlslsm) ::                                                      &
 ftl_old                                                                       &
          ! Used to hold current value of FTL_GB before updating
,gamma1                                                                        &
,gamma2

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='IM_SF_PT2'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!JULES standalone never knew about ENDGAME so no offset.
offset = 0
#if defined(UM_JULES)
IF ((model_type /= mt_single_column) .AND. (model_type /= mt_lfric)) THEN
  offset = 1
END IF
#endif
!-------------------------------------------------------------------------
! initialise epot_calc
!-------------------------------------------------------------------------

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(SHARED)                                                          &
!$OMP PRIVATE(j,i,gamma1,gamma2,n,k,l,lat_ht,ftl_old)

DO n = 1, nsurft
!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_pts
    epot_calc(l,n) = .FALSE.
  END DO
!$OMP END DO NOWAIT
END DO

!-------------------------------------------------------------------------

! Now compute surface stresses for the 2nd stage (predictor) of
! the new scheme (BL solver) using its discretization.

!-------------------------------------------------------------------------

IF ( .NOT. l_correct ) THEN

  !-------------------------------------------------------------------------

  ! Compute surface stresses for the 1st stage (predictor) of
  ! the new scheme (BL solver) using its discretization.
#if !defined(LFRIC)
  ! This code is written for an (i,j) latitude-longitude grid and therefore
  ! not compatible with LFRic, which has its own version
  !-------------------------------------------------------------------------
  ! Copy gamma into an array defined on u-points
  ! In principle, gamma should be interpolated, but sensitivity is expected
  ! to be small so the adjacent p-point is used to avoid message passing
  ! This makes v-at-poles/ENDGAME formulation consistent with the current
  !  formulation
!$OMP DO SCHEDULE(STATIC)
  DO j = udims%j_start,udims%j_end
    DO i = udims%i_start,udims%i_end
      gamma1 = gamma1_in(i + offset,j)
      gamma2 = gamma2_in(i + offset,j)
      IF ( flandg_u(i,j) > 0.0 ) THEN
        taux_land_star(i,j) = ( gamma2 * taux_land(i,j) +                      &
                 gamma1 * rhokm_u_1(i,j) * du_1(i,j) ) /                       &
                ( 1.0 + gamma1 * rhokm_u_1(i,j) * cq_cm_u_1(i,j) )
      ELSE
        taux_land_star(i,j) = 0.0
      END IF
      IF ( flandg_u(i,j) < 1.0 ) THEN
        taux_ssi_star(i,j) = ( gamma2 * taux_ssi(i,j) +                        &
                 gamma1 * rhokm_u_1(i,j) * du_1(i,j) ) /                       &
                ( 1.0 + gamma1 * rhokm_u_1(i,j) * cq_cm_u_1(i,j) )
      ELSE
        taux_ssi_star(i,j) = 0.0
      END IF
      taux_1(i,j) = flandg_u(i,j) * taux_land_star(i,j)                        &
                  + ( 1.0 - flandg_u(i,j)) * taux_ssi_star(i,j)
    END DO
  END DO
!$OMP END DO NOWAIT

  ! Copy gamma into an array defined on u-points
  ! In principle, gamma should be interpolated, but sensitivity is expected
  ! to be small so the adjacent p-point is used to avoid message passing
  ! This makes v-at-poles/ENDGAME formulation consistent with the current
  ! formulation
#if defined(UM_JULES)
  IF ((model_type /= mt_single_column) .AND. (model_type /= mt_lfric)) THEN
!$OMP DO SCHEDULE(STATIC)
    DO j = vdims%j_start,vdims%j_end
      DO i = vdims%i_start,vdims%i_end
        IF ( j == vdims%j_end ) THEN
          gamma1 = gamma1_in(i,udims%j_end)
          gamma2 = gamma2_in(i,udims%j_end)
        ELSE
          gamma1 = gamma1_in(i,j+1)
          gamma2 = gamma2_in(i,j+1)
        END IF
        IF ( flandg_v(i,j) > 0.0 ) THEN
          tauy_land_star(i,j) = ( gamma2 * tauy_land(i,j) +                    &
                   gamma1 * rhokm_v_1(i,j) * dv_1(i,j) ) /                     &
                  ( 1.0 + gamma1 * rhokm_v_1(i,j) * cq_cm_v_1(i,j) )
        ELSE
          tauy_land_star(i,j) = 0.0
        END IF
        IF ( flandg_v(i,j) < 1.0 ) THEN
          tauy_ssi_star(i,j) = ( gamma2 * tauy_ssi(i,j) +                      &
                   gamma1 * rhokm_v_1(i,j) * dv_1(i,j) ) /                     &
                  ( 1.0 + gamma1 * rhokm_v_1(i,j) * cq_cm_v_1(i,j) )
        ELSE
          tauy_ssi_star(i,j) = 0.0
        END IF
        tauy_1(i,j) = flandg_v(i,j) * tauy_land_star(i,j)                      &
                  + ( 1.0 - flandg_v(i,j)) * tauy_ssi_star(i,j)
      END DO
    END DO
!$OMP END DO NOWAIT
  ELSE
#endif
!$OMP DO SCHEDULE(STATIC)
    DO j = vdims%j_start,vdims%j_end
      DO i = vdims%i_start,vdims%i_end
        gamma1 = gamma1_in(i,j)
        gamma2 = gamma2_in(i,j)
        IF ( flandg_v(i,j) > 0.0 ) THEN
          tauy_land_star(i,j) = ( gamma2 * tauy_land(i,j) +                    &
                   gamma1 * rhokm_v_1(i,j) * dv_1(i,j) ) /                     &
                  ( 1.0 + gamma1 * rhokm_v_1(i,j) * cq_cm_v_1(i,j) )
        ELSE
          tauy_land_star(i,j) = 0.0
        END IF
        IF ( flandg_v(i,j) < 1.0 ) THEN
          tauy_ssi_star(i,j) = ( gamma2 * tauy_ssi(i,j) +                      &
                   gamma1 * rhokm_v_1(i,j) * dv_1(i,j) ) /                     &
                  ( 1.0 + gamma1 * rhokm_v_1(i,j) * cq_cm_v_1(i,j) )
        ELSE
          tauy_ssi_star(i,j) = 0.0
        END IF
        tauy_1(i,j) = flandg_v(i,j) * tauy_land_star(i,j)                      &
                  + ( 1.0 - flandg_v(i,j)) * tauy_ssi_star(i,j)
      END DO
    END DO
!$OMP END DO NOWAIT
#if defined(UM_JULES)
  END IF
#endif
#endif
  !-------------------------------------------------------------------------

  ! Compute scalar surface fluxes using the standard scheme described
  ! in MOSES 2.2 technical documentation (hadley centre tecnical note 30).
  ! This needs to be done only in the first stage of the scheme (predictor).
  ! The same fluxes will be used for the 2nd stage (corrector).
  ! NOTE: scalar surface fluxes could be computed using the new scheme
  !       but with a penalty in code complexity.

  !------------------------------------------------------------------------

  ! Initialise APART and BPART to zero
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      apart(i,j,1) = 0.0
      apart(i,j,2) = 0.0
      bpart(i,j,1) = 0.0
      bpart(i,j,2) = 0.0
      ftl_land(i,j) = 0.0
      fqw_land(i,j) = 0.0
      ftl_sice(i,j) = 0.0
      fqw_sice(i,j) = 0.0
    END DO
  END DO
!$OMP END DO NOWAIT

!$OMP BARRIER

  ! Land tiles
  DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
    DO k = 1,surft_pts(n)
      l = surft_index(k,n)
      j=(land_index(l) - 1) / t_i_length + 1
      i = land_index(l) - (j-1) * t_i_length
      lat_ht = lc
      IF (snow_surft(l,n) >  0.0) lat_ht = ls

      rhokpm(l,n) = rhokh_1(l,n) / ( ashtf_prime_surft(l,n) +                  &
               rhokh_1(l,n) * (lat_ht * alpha1(l,n) * resft(l,n) + cp) )

      apart(i,j,1) = apart(i,j,1) - tile_frac(l,n) *                           &
               r_gamma * rhokpm(l,n) *                                         &
            ( lat_ht * resft(l,n) * rhokh_1(l,n) * alpha1(l,n) +               &
                         ashtf_prime_surft(l,n) )
      apart(i,j,2) = apart(i,j,2) + tile_frac(l,n) *                           &
               r_gamma * rhokpm(l,n) *                                         &
               lat_ht * resft(l,n) * rhokh_1(l,n)
      bpart(i,j,1) = bpart(i,j,1) + tile_frac(l,n) *                           &
               r_gamma * resft(l,n) * rhokpm(l,n) *                            &
               cp * rhokh_1(l,n) * alpha1(l,n)
      bpart(i,j,2) = bpart(i,j,2) - tile_frac(l,n) *                           &
               r_gamma * resft(l,n) * rhokpm(l,n) *                            &
               ( cp * rhokh_1(l,n) + ashtf_prime_surft(l,n) )
    END DO
!$OMP END DO
  END DO

  ! Sea points
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      IF ( flandg(i,j) < 1.0 ) THEN
        IF (l_use_dtstar_sea) THEN
          IF (nice_use > 1) THEN
            ! Include leads.
            rhokpm_sea(i,j) = rhokh1_sea(i,j) / ( ashtf_prime_sea(i,j)         &
                    + rhokh1_sea(i,j) * (ls * alpha1_sea(i,j) * beta_evap + cp) )

            apart(i,j,1) = apart(i,j,1)                                        &
            - r_gamma * (1.0 - flandg(i,j)) * (1.0 - ice_fract(i,j))           &
            * rhokpm_sea(i,j) *                                                &
            ( lc * beta_evap * rhokh1_sea(i,j) * alpha1_sea(i,j) +             &
            ashtf_prime_sea(i,j) )

            apart(i,j,2) = apart(i,j,2)                                        &
            + r_gamma * (1.0 - flandg(i,j)) * (1.0 - ice_fract(i,j))           &
            * rhokpm_sea(i,j) * lc * beta_evap  * rhokh1_sea(i,j)

            bpart(i,j,1) = bpart(i,j,1)                                        &
            + r_gamma * (1.0 - flandg(i,j)) * (1.0 - ice_fract(i,j))           &
            *beta_evap * rhokpm_sea(i,j) * cp * rhokh1_sea(i,j) * alpha1_sea(i,j)

            bpart(i,j,2) = bpart(i,j,2)                                        &
            - r_gamma * (1.0 - flandg(i,j)) * (1.0 - ice_fract(i,j))           &
            * beta_evap * rhokpm_sea(i,j) *                                    &
            ( cp * rhokh1_sea(i,j) + ashtf_prime_sea(i,j) )
          ELSE
            ! Do not include leads - use original scheme.
            rhokpm_sea(i,j) = rhokh1_sice(i,j,1) / ( ashtf_prime_sea(i,j)      &
                    + rhokh1_sice(i,j,1) * (ls * alpha1_sea(i,j) * beta_evap + cp) )

            apart(i,j,1) = apart(i,j,1)                                        &
            - r_gamma * (1.0 - flandg(i,j)) * (1.0 - ice_fract(i,j))           &
            * rhokpm_sea(i,j) *                                                &
            ( lc * beta_evap * rhokh1_sice(i,j,1) * alpha1_sea(i,j) +          &
            ashtf_prime_sea(i,j) )

            apart(i,j,2) = apart(i,j,2)                                        &
            + r_gamma * (1.0 - flandg(i,j)) * (1.0 - ice_fract(i,j))           &
            * rhokpm_sea(i,j) * lc * beta_evap  * rhokh1_sice(i,j,1)

            bpart(i,j,1) = bpart(i,j,1)                                        &
            + r_gamma * (1.0 - flandg(i,j)) * (1.0 - ice_fract(i,j))           &
            *beta_evap * rhokpm_sea(i,j) * cp * rhokh1_sice(i,j,1) * alpha1_sea(i,j)

            bpart(i,j,2) = bpart(i,j,2)                                        &
            - r_gamma * (1.0 - flandg(i,j)) * (1.0 - ice_fract(i,j))           &
            * beta_evap * rhokpm_sea(i,j) *                                    &
            ( cp * rhokh1_sice(i,j,1) + ashtf_prime_sea(i,j) )
          END IF
        ELSE
          IF (nice_use > 1) THEN
            ! Include leads.
            apart(i,j,1)= flandg(i,j) * apart(i,j,1)                           &
           - r_gamma * ( 1.0 - flandg(i,j) ) * rhokh1_sea(i,j)                 &
           * ( 1.0 - ice_fract(i,j) )
            apart(i,j,2)= flandg(i,j) * apart(i,j,2)
            bpart(i,j,1)= flandg(i,j) * bpart(i,j,1)
            bpart(i,j,2)= flandg(i,j) * bpart(i,j,2)                           &
           - r_gamma * ( 1.0 - flandg(i,j) ) * rhokh1_sea(i,j)                 &
           * ( 1.0 - ice_fract(i,j) )
          ELSE
            ! Do not include leads - use original scheme.
            apart(i,j,1)= flandg(i,j) * apart(i,j,1)                           &
           - r_gamma * ( 1.0 - flandg(i,j) ) * rhokh1_sice(i,j,1)              &
           * ( 1.0 - ice_fract(i,j) )
            apart(i,j,2)= flandg(i,j) * apart(i,j,2)
            bpart(i,j,1)= flandg(i,j) * bpart(i,j,1)
            bpart(i,j,2)= flandg(i,j) * bpart(i,j,2)                           &
           - r_gamma * ( 1.0 - flandg(i,j) ) * rhokh1_sice(i,j,1)              &
           * ( 1.0 - ice_fract(i,j) )
          END IF
        END IF
      END IF
    END DO
  END DO
!$OMP END DO NOWAIT

  ! Seaice points
  DO n = 1,nice_use
!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end

        IF ( flandg(i,j) < 1.0 .AND. ice_fract_cat(i,j,n) >  0.0 ) THEN

          rhokpm_sice(i,j,n) = rhokh1_sice(i,j,n) / ( ashtf_prime(i,j,n)       &
                 + rhokh1_sice(i,j,n) * (ls * alpha1_sice(i,j,n) + cp) )

          apart(i,j,1) = apart(i,j,1)                                          &
          - r_gamma * (1.0 - flandg(i,j)) * ice_fract_cat(i,j,n)               &
          * rhokpm_sice(i,j,n) *                                               &
          ( ls * rhokh1_sice(i,j,n) * alpha1_sice(i,j,n) + ashtf_prime(i,j,n) )

          apart(i,j,2) = apart(i,j,2)                                          &
          + r_gamma * (1.0 - flandg(i,j)) *ice_fract_cat(i,j,n)                &
          * rhokpm_sice(i,j,n) * ls * rhokh1_sice(i,j,n)

          bpart(i,j,1) = bpart(i,j,1)                                          &
          + r_gamma * ice_fract_cat(i,j,n) * ( 1.0 - flandg(i,j) )             &
          * rhokpm_sice(i,j,n) *cp * rhokh1_sice(i,j,n) * alpha1_sice(i,j,n)

          bpart(i,j,2) = bpart(i,j,2)                                          &
         - r_gamma * ice_fract_cat(i,j,n) * ( 1.0 - flandg(i,j) )              &
         * rhokpm_sice(i,j,n) *                                                &
         ( cp * rhokh1_sice(i,j,n) + ashtf_prime(i,j,n) )

        END IF
      END DO
    END DO
!$OMP END DO NOWAIT
  END DO

  ! Land tiles
  DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
    DO k = 1,surft_pts(n)
      l = surft_index(k,n)
      j=(land_index(l) - 1) / t_i_length + 1
      i = land_index(l) - (j-1) * t_i_length
      e_epot_surft(l,n) = 1.0
      IF (      (epot_surft(l,n) >  0.0)                                       &
          .AND. (fqw_surft(l,n)  > SQRT(TINY(0.0))) ) THEN
        e_epot_surft(l,n) = epot_surft(l,n) / fqw_surft(l,n)
        epot_calc(l,n)=.TRUE.
      END IF
    END DO
!$OMP END DO
  END DO

  ! Calculate grid-box fluxes of heat and moisture
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end

      recip(i,j)=( 1.0 + ctctq1(i,j) * apart(i,j,1) ) *                        &
           ( 1.0 + ctctq1(i,j) * bpart(i,j,2) ) -                              &
           ctctq1(i,j) * apart(i,j,2) * ctctq1(i,j) * bpart(i,j,1)

      ftl_old = ftl_gb(i,j)

      ftl_gb(i,j) = ( ( 1.0 + ctctq1(i,j) * bpart(i,j,2) ) *                   &
               ( ftl_old + apart(i,j,1) * dtl1_1(i,j) +                        &
                 apart(i,j,2) * dqw1_1(i,j)) -                                 &
                 ctctq1(i,j) * apart(i,j,2) * ( fqw_gb(i,j) +                  &
                 bpart(i,j,1) * dtl1_1(i,j) +                                  &
                 bpart(i,j,2) * dqw1_1(i,j)) ) / recip(i,j)

      fqw_gb(i,j) = ( ( 1.0 + ctctq1(i,j) * apart(i,j,1) ) *                   &
                ( fqw_gb(i,j) + bpart(i,j,1) * dtl1_1(i,j) +                   &
                  bpart(i,j,2) * dqw1_1(i,j)) -                                &
                  ctctq1(i,j) * bpart(i,j,1) * ( ftl_old +                     &
                  apart(i,j,1) * dtl1_1(i,j) +                                 &
                  apart(i,j,2) * dqw1_1(i,j)) ) / recip(i,j)

    END DO
  END DO
!$OMP END DO

!$OMP BARRIER

  ! Make implicit correction to tile fluxes

  ! Land tiles
  DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
    DO k = 1,surft_pts(n)
      l = surft_index(k,n)
      j=(land_index(l) - 1) / t_i_length + 1
      i = land_index(l) - (j-1) * t_i_length
      lat_ht = lc
      IF (snow_surft(l,n) >  0.0) lat_ht = ls

      ftl_surft(l,n) = ftl_surft(l,n) -                                        &
               r_gamma * rhokpm(l,n) *                                         &
            ( lat_ht * resft(l,n) * rhokh_1(l,n) * alpha1(l,n) +               &
                       ashtf_prime_surft(l,n) ) *                              &
         ( dtl1_1(i,j) - ctctq1(i,j) * ftl_gb(i,j) ) +                         &
               r_gamma * rhokpm(l,n) *                                         &
               lat_ht * resft(l,n) * rhokh_1(l,n) *                            &
         ( dqw1_1(i,j) - ctctq1(i,j) * fqw_gb(i,j) )

      fqw_surft(l,n) = fqw_surft(l,n) +                                        &
               r_gamma * resft(l,n) * rhokpm(l,n) *                            &
               cp * rhokh_1(l,n) * alpha1(l,n) *                               &
         ( dtl1_1(i,j) - ctctq1(i,j) * ftl_gb(i,j) ) -                         &
               r_gamma * resft(l,n) * rhokpm(l,n) *                            &
               ( cp * rhokh_1(l,n) + ashtf_prime_surft(l,n) ) *                &
         ( dqw1_1(i,j) - ctctq1(i,j) * fqw_gb(i,j) )

      IF (l_epot_corr) THEN
        IF (epot_calc(l,n)) THEN
          epot_surft(l,n) = fqw_surft(l,n) * e_epot_surft(l,n)
        END IF
      ELSE
        epot_surft(l,n) = fqw_surft(l,n) * e_epot_surft(l,n)
      END IF

      fqw_land(i,j) = fqw_land(i,j) + fqw_surft(l,n) * tile_frac(l,n)
      ftl_land(i,j) = ftl_land(i,j) + ftl_surft(l,n) * tile_frac(l,n)

      IF (l_dtcanfix) THEN
        !       Use the corrected version, removing a spurious factor of cp
        !       in the numerator.
        dtstar_surft(l,n) = dtstar_surft(l,n) + r_gamma *                      &
               ( cp * rhokh_1(l,n) *                                           &
                 ( dtl1_1(i,j) - ctctq1(i,j) * ftl_gb(i,j) ) +                 &
                 lat_ht * resft(l,n) * rhokh_1(l,n) *                          &
                 ( dqw1_1(i,j) - ctctq1(i,j) * fqw_gb(i,j) ) ) /               &
               ( rhokh_1(l,n) *                                                &
                 ( cp + lat_ht * resft(l,n) * alpha1(l,n) ) +                  &
                 ashtf_prime_surft(l,n) )
      ELSE
        !       Use the original incorrect version.
        dtstar_surft(l,n) = dtstar_surft(l,n) + r_gamma *                      &
               ( cp * rhokh_1(l,n) *                                           &
                 ( dtl1_1(i,j) - ctctq1(i,j) * ftl_gb(i,j) ) +                 &
                 lat_ht * resft(l,n) * rhokh_1(l,n) *                          &
                 ( dqw1_1(i,j) - ctctq1(i,j) * fqw_gb(i,j) ) ) /               &
               ( cp * rhokh_1(l,n) *                                           &
                 ( cp + lat_ht * resft(l,n) * alpha1(l,n) ) +                  &
                 ashtf_prime_surft(l,n) )
      END IF

    END DO
!$OMP END DO
  END DO

  ! Make implicit correction for sea ice category fluxes
  DO n = 1,nice_use
!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end

        IF (flandg(i,j) <  1.0 .AND. ice_fract_cat(i,j,n) >  0.0) THEN

          ftl_ice(i,j,n) = ftl_ice(i,j,n) -                                    &
               r_gamma * rhokpm_sice(i,j,n) *                                  &
              ( ls * rhokh1_sice(i,j,n) * alpha1_sice(i,j,n) +                 &
                       ashtf_prime(i,j,n) ) *                                  &
              ( dtl1_1(i,j) - ctctq1(i,j) * ftl_gb(i,j) ) +                    &
                       r_gamma * rhokpm_sice(i,j,n) *                          &
                       ls * rhokh1_sice(i,j,n) *                               &
              ( dqw1_1(i,j) - ctctq1(i,j) * fqw_gb(i,j) )

          fqw_ice(i,j,n) = fqw_ice(i,j,n) +                                    &
               r_gamma * rhokpm_sice(i,j,n) *                                  &
               cp * rhokh1_sice(i,j,n) * alpha1_sice(i,j,n) *                  &
              ( dtl1_1(i,j) - ctctq1(i,j) * ftl_gb(i,j) ) -                    &
                   r_gamma  * rhokpm_sice(i,j,n) *                             &
              ( cp * rhokh1_sice(i,j,n) + ashtf_prime(i,j,n) ) *               &
              ( dqw1_1(i,j) - ctctq1(i,j) * fqw_gb(i,j) )

          ! Calculate total sea ice fluxes
          fqw_sice(i,j) = fqw_sice(i,j) + fqw_ice(i,j,n)                       &
                              * ice_fract_cat(i,j,n) / ice_fract(i,j)
          ftl_sice(i,j) = ftl_sice(i,j) + ftl_ice(i,j,n)                       &
                              * ice_fract_cat(i,j,n) / ice_fract(i,j)

          IF (l_dtcanfix) THEN
            !         Use the corrected version, removing a spurious factor of cp
            !         in the numerator.
            dtstar_sice(i,j,n) = dtstar_sice(i,j,n) + r_gamma *                &
                    ( cp * rhokh1_sice(i,j,n) *                                &
                    ( dtl1_1(i,j) - ctctq1(i,j) * ftl_gb(i,j) ) +              &
                       ls * rhokh1_sice(i,j,n) *                               &
                    ( dqw1_1(i,j) - ctctq1(i,j) * fqw_gb(i,j) ) ) /            &
                    ( rhokh1_sice(i,j,n) *                                     &
                    ( cp + ls * alpha1_sice(i,j,n) ) +                         &
                       ashtf_prime(i,j,n) )
          ELSE
            !         Use the original incorrect version.
            dtstar_sice(i,j,n) = dtstar_sice(i,j,n) + r_gamma *                &
                    ( cp * rhokh1_sice(i,j,n) *                                &
                    ( dtl1_1(i,j) - ctctq1(i,j) * ftl_gb(i,j) ) +              &
                       ls * rhokh1_sice(i,j,n) *                               &
                    ( dqw1_1(i,j) - ctctq1(i,j) * fqw_gb(i,j) ) ) /            &
                    ( cp * rhokh1_sice(i,j,n) *                                &
                    ( cp + ls * alpha1_sice(i,j,n) ) +                         &
                       ashtf_prime(i,j,n) )
          END IF

        END IF

      END DO
    END DO
!$OMP END DO NOWAIT
  END DO

  ! Sea points  (= GBM - land fluxes - sea ice fluxes)
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end

      h_sea(i,j) = 0.0
      e_sea(i,j) = 0.0

      IF (flandg(i,j) <  1.0 .AND. (1.0 - ice_fract(i,j)) /= 0.0 ) THEN
        h_sea(i,j) = ( (ftl_gb(i,j)                                            &
                      - ftl_land(i,j) * flandg(i,j)) / (1.0 - flandg(i,j))     &
                      - ftl_sice(i,j) * ice_fract(i,j) ) /                     &
                      (1.0 - ice_fract(i,j))

        e_sea(i,j) = ( (fqw_gb(i,j)                                            &
                     - fqw_land(i,j) * flandg(i,j)) / (1.0 - flandg(i,j))      &
                     - fqw_sice(i,j) * ice_fract(i,j) ) /                      &
                      (1.0 - ice_fract(i,j))

        IF (l_use_dtstar_sea) THEN
          IF (nice_use > 1) THEN
            dtstar_sea(i,j) = dtstar_sea(i,j) + r_gamma *                      &
                    ( cp * rhokh1_sea(i,j) *                                   &
                    ( dtl1_1(i,j) - ctctq1(i,j) * ftl_gb(i,j) ) +              &
                       lc * rhokh1_sea(i,j) *                                  &
                    ( dqw1_1(i,j) - ctctq1(i,j) * fqw_gb(i,j) ) ) /            &
                    ( rhokh1_sea(i,j) *                                        &
                    ( cp + lc * alpha1_sea(i,j) ) +                            &
                       ashtf_prime_sea(i,j) )
          ELSE
            dtstar_sea(i,j) = dtstar_sea(i,j) + r_gamma *                      &
                    ( cp * rhokh1_sice(i,j,1) *                                &
                    ( dtl1_1(i,j) - ctctq1(i,j) * ftl_gb(i,j) ) +              &
                       lc * rhokh1_sice(i,j,1) *                               &
                    ( dqw1_1(i,j) - ctctq1(i,j) * fqw_gb(i,j) ) ) /            &
                    ( rhokh1_sice(i,j,1) *                                     &
                    ( cp + lc * alpha1_sea(i,j) ) +                            &
                       ashtf_prime_sea(i,j) )
          END IF
        END IF

      END IF
    END DO
  END DO
!$OMP END DO NOWAIT

ELSE ! IF L_CORRECT==TRUE THEN:

#if !defined(LFRIC)
  ! This code is written for an (i,j) latitude-longitude grid and therefore
  ! not compatible with LFRic, which has its own version
  !-------------------------------------------------------------------------
  ! Copy gamma into an array defined on u-points
  ! In principle, gamma should be interpolated, but sensitivity is expected
  ! to be small so the adjacent p-point is used to avoid message passing
  ! This makes v-at-poles/ENDGAME formulation consistent with the current
  ! formulation
!$OMP DO SCHEDULE(STATIC)
  DO j = udims%j_start,udims%j_end
    DO i = udims%i_start,udims%i_end
      gamma1 = gamma1_in(i + offset,j)
      gamma2 = gamma2_in(i + offset,j)
      IF ( flandg_u(i,j) >  0.0 ) THEN
        taux_land(i,j) = ( gamma2 * (taux_land(i,j) +                          &
                               rhokm_u_1(i,j) * du_star1(i,j)) +               &
                 gamma1 * rhokm_u_1(i,j) * du_1(i,j) ) /                       &
                ( 1.0 + gamma1 * rhokm_u_1(i,j) * cq_cm_u_1(i,j) )
      ELSE
        taux_land(i,j) = 0.0
      END IF
      IF ( flandg_u(i,j) <  1.0 ) THEN
        taux_ssi(i,j) = ( gamma2 * (taux_ssi(i,j) +                            &
                              rhokm_u_1(i,j) * du_star1(i,j)) +                &
                 gamma1 * rhokm_u_1(i,j) * du_1(i,j) ) /                       &
                ( 1.0 + gamma1 * rhokm_u_1(i,j) * cq_cm_u_1(i,j) )
      ELSE
        taux_ssi(i,j) = 0.0
      END IF
      taux_1(i,j) = flandg_u(i,j) * taux_land(i,j)                             &
                  + ( 1.0 - flandg_u(i,j)) * taux_ssi(i,j)
    END DO
  END DO
!$OMP END DO NOWAIT

  ! Copy gamma into an array defined on u-points
  ! In principle, gamma should be interpolated, but sensitivity is expected
  ! to be small so the adjacent p-point is used to avoid message passing
  ! This makes v-at-poles/ENDGAME formulation consistent with the current
  ! formulation
#if defined(UM_JULES)
  IF ((model_type /= mt_single_column) .AND. (model_type /= mt_lfric)) THEN
!$OMP DO SCHEDULE(STATIC)
    DO j = vdims%j_start,vdims%j_end
      DO i = vdims%i_start,vdims%i_end
        IF ( j == vdims%j_end ) THEN
          gamma1 = gamma1_in(i,udims%j_end)
          gamma2 = gamma2_in(i,udims%j_end)
        ELSE
          gamma1 = gamma1_in(i,j+1)
          gamma2 = gamma2_in(i,j+1)
        END IF
        IF ( flandg_v(i,j) >  0.0 ) THEN
          tauy_land(i,j) = ( gamma2 * (tauy_land(i,j) +                        &
                                 rhokm_v_1(i,j) * dv_star1(i,j)) +             &
                   gamma1 * rhokm_v_1(i,j) * dv_1(i,j) ) /                     &
                  ( 1.0 + gamma1 * rhokm_v_1(i,j) * cq_cm_v_1(i,j) )
        ELSE
          tauy_land(i,j) = 0.0
        END IF
        IF ( flandg_v(i,j) <  1.0 ) THEN
          tauy_ssi(i,j) = ( gamma2 * (tauy_ssi(i,j) +                          &
                                 rhokm_v_1(i,j) * dv_star1(i,j)) +             &
                   gamma1 * rhokm_v_1(i,j) * dv_1(i,j) ) /                     &
                  ( 1.0 + gamma1 * rhokm_v_1(i,j) * cq_cm_v_1(i,j) )
        ELSE
          tauy_ssi(i,j) = 0.0
        END IF
        tauy_1(i,j) = flandg_v(i,j) * tauy_land(i,j)                           &
                    + ( 1.0 - flandg_v(i,j)) * tauy_ssi(i,j)
      END DO
    END DO
!$OMP END DO NOWAIT
  ELSE
#endif
!$OMP DO SCHEDULE(STATIC)
    DO j = vdims%j_start,vdims%j_end
      DO i = vdims%i_start,vdims%i_end
        gamma1 = gamma1_in(i,j)
        gamma2 = gamma2_in(i,j)
        IF ( flandg_v(i,j) >  0.0 ) THEN
          tauy_land(i,j) = ( gamma2 * (tauy_land(i,j) +                        &
                                 rhokm_v_1(i,j) * dv_star1(i,j)) +             &
                   gamma1 * rhokm_v_1(i,j) * dv_1(i,j) ) /                     &
                  ( 1.0 + gamma1 * rhokm_v_1(i,j) * cq_cm_v_1(i,j) )
        ELSE
          tauy_land(i,j) = 0.0
        END IF
        IF ( flandg_v(i,j) <  1.0 ) THEN
          tauy_ssi(i,j) = ( gamma2 * (tauy_ssi(i,j) +                          &
                                 rhokm_v_1(i,j) * dv_star1(i,j)) +             &
                   gamma1 * rhokm_v_1(i,j) * dv_1(i,j) ) /                     &
                  ( 1.0 + gamma1 * rhokm_v_1(i,j) * cq_cm_v_1(i,j) )
        ELSE
          tauy_ssi(i,j) = 0.0
        END IF
        tauy_1(i,j) = flandg_v(i,j) * tauy_land(i,j)                           &
                    + ( 1.0 - flandg_v(i,j)) * tauy_ssi(i,j)
      END DO
    END DO
!$OMP END DO NOWAIT
#if defined(UM_JULES)
  END IF
#endif
#endif
END IF ! L_CORRECT

!$OMP END PARALLEL

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE im_sf_pt2
END MODULE im_sf_pt2_mod
