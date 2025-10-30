! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE jules_grid_update_implicit_mod

USE sf_melt_mod, ONLY: sf_melt
USE screen_tq_mod, ONLY: screen_tq
USE im_sf_pt2_mod, ONLY: im_sf_pt2
USE sice_htf_mod, ONLY: sice_htf

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
                  ModuleName='JULES_GRID_UPDATE_IMPLICIT_MOD'

CONTAINS
!  SUBROUTINE JULES_GRID_UPDATE_IMPLICIT ----------------------------
!
!  Purpose: Calculate gridbox implicit correction to surface fluxes
!           of heat, moisture and momentum to be used by the
!           unconditionally stable and non-oscillatory BL numerical solver.
!
!--------------------------------------------------------------------
!    Arguments :-
SUBROUTINE jules_grid_update_implicit (                                        &
! IN values defining field dimensions and subset to be processed :
 land_pts,land_index,nice,nice_use,nsurft,surft_index,surft_pts,               &
 tile_frac,flandg,                                                             &
! IN sea/sea-ice data :
 ice_fract,ice_fract_ncat,                                                     &
! IN everything not covered so far :
 rhokm_u_1,rhokm_v_1,r_gamma,                                                  &
 gamma1,gamma2,alpha1,alpha1_sea,alpha1_sice,                                  &
 ashtf_prime,ashtf_prime_sea,ashtf_prime_surft,                                &
 du_1,dv_1,resft,rhokh_surft,rhokh_sice,rhokh_sea,ctctq1,                      &
 dqw1_1,dtl1_1,du_star1,dv_star1,cq_cm_u_1,cq_cm_v_1,                          &
 l_correct,flandg_u,flandg_v,snow_surft,                                       &
! INOUT data :
 epot_surft,fqw_ice,ftl_ice,dtstar_surft,dtstar_sea,dtstar_sice,               &
 fqw_surft,fqw_1,ftl_1,ftl_surft,taux_land,taux_ssi,tauy_land,tauy_ssi,        &
 taux_land_star,tauy_land_star,taux_ssi_star, tauy_ssi_star,                   &
! OUT data required elsewhere in UM system :
 e_sea,h_sea,taux_1,tauy_1,ice_fract_cat_use                                   &
 )

USE planet_constants_mod,     ONLY: cp

USE atm_fields_bounds_mod,    ONLY:                                            &
  tdims, udims, vdims, pdims, udims_s, vdims_s

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
 land_pts    ! IN No of land points

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
,nice                                                                          &
                             ! IN Number of sea ice categories
,nice_use                    ! IN Number of sea ice categories used
                             !    fully in surface exchange

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 tile_frac(land_pts,nsurft)                                                    &
                             ! IN Tile fractions including
                             !    snow cover in the ice tile.
,flandg(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                             ! IN Land fraction on all pts.
,flandg_u(udims%i_start:udims%i_end,udims%j_start:udims%j_end)                 &
                             ! IN Land fraction on U grid.
,flandg_v(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)                 &
                             ! IN Land fraction on V grid.
,snow_surft(land_pts,nsurft)
                             ! IN Lying snow on tiles (kg/m2)

! (d) Sea/sea-ice data.
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 ice_fract(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                &
                             ! IN Fraction of gridbox covered by
                             !    sea-ice (decimal fraction).
,ice_fract_ncat(tdims%i_start:tdims%i_end,                                     &
                tdims%j_start:tdims%j_end,nice)
                             ! IN Fraction of gridbox
                             !    covered by sea-ice on categories.

! (f) Atmospheric + any other data not covered so far, incl control.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 rhokm_u_1(udims%i_start:udims%i_end,udims%j_start:udims%j_end)                &
                             ! IN Exchange coefficients for
                             !    momentum (on U-grid, with 1st
                             !    and last rows undefined or, at
                             !    present, set to "missing data")
,rhokm_v_1(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)
                             ! IN Exchange coefficients for
                             !    momentum (on V-grid, with 1st
                             !    and last rows undefined or, at
                             !    present, set to "missing data")

REAL(KIND=real_jlslsm), INTENT(IN) :: r_gamma
                              ! IN implicit weight in level 1

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 gamma1(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end)                   &
                             ! weights for new BL solver
,gamma2(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end)

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 alpha1(land_pts,nsurft)                                                       &
                             ! IN Mean gradient of saturated
                             !    specific humidity with respect
                             !    to temperature between the
                             !    bottom model layer and tile
                             !    surfaces
,alpha1_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)               &
                             ! IN ALPHA1 for sea.
,alpha1_sice(tdims%i_start:tdims%i_end,                                        &
             tdims%j_start:tdims%j_end,nice_use)                               &
                             ! IN ALPHA1 for sea-ice.
,ashtf_prime(tdims%i_start:tdims%i_end,                                        &
             tdims%j_start:tdims%j_end,nice_use)                               &
                             ! IN Adjusted SEB coefficient for
                             !    sea-ice
,ashtf_prime_sea(tdims%i_start:tdims%i_end,                                    &
                 tdims%j_start:tdims%j_end)                                    &
                             ! IN Adjusted SEB coefficient for
                             !    sea
,ashtf_prime_surft(land_pts,nsurft)                                            &
                             ! IN Adjusted SEB coefficient for
                             !    land tiles.
,resft(land_pts,nsurft)                                                        &
                             ! IN Total resistance factor.
                             !    fracaero+(1-fracaero)*resfs for
                             !    snow-free land, 1 for snow.
,rhokh_surft(land_pts,nsurft)                                                  &
                             ! IN Surface exchange coefficients
                             !    for land tiles
,rhokh_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,               &
                                                        nice_use)              &
                             ! IN Surface exchange coefficients
                             !    for sea-ice
,rhokh_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! IN Surface exchange coefficients
                             !    for sea

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 cq_cm_u_1(udims%i_start:udims%i_end,udims%j_start:udims%j_end)                &
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
          vdims_s%j_start:vdims_s%j_end)
                             ! IN Additional arrays needed by the
                             !    uncond stable BL numerical solver

LOGICAL, INTENT(IN) ::                                                         &
 l_correct                   ! IN flag used by the new BL solver


!--------------------------------------------------------------------
!  In/outs :-
!--------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
 epot_surft(land_pts,nsurft)                                                   &
                             ! INOUT surface tile potential
                             !       evaporation
,fqw_ice(tdims%i_start:tdims%i_end,                                            &
         tdims%j_start:tdims%j_end,nice_use)                                   &
                             ! INOUT Surface FQW for sea-ice
,ftl_ice(tdims%i_start:tdims%i_end,                                            &
         tdims%j_start:tdims%j_end,nice_use)                                   &
                             ! INOUT Surface FTL for sea-ice
,dtstar_surft(land_pts,nsurft)                                                 &
                             ! INOUT Change in TSTAR over timestep
                             !       for land tiles
,dtstar_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)               &
                             ! INOUT Change is TSTAR over timestep
                             !       for open sea
,dtstar_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,nice_use)     &
                             ! INOUT Change is TSTAR over timestep
                             !       for sea-ice
,fqw_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! INOUT Moisture flux between layers
                             !       (kg per square metre per sec)
                             !       FQW(,1) is total water flux
                             !       from surface, 'E'.
,ftl_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! INOUT FTL(,K) contains net
                             !       turbulent sensible heat flux
                             !       into layer K from below; so
                             !       FTL(,1) is the surface
                             !       sensible heat, H.(W/m2)
,ftl_surft(land_pts,nsurft)                                                    &
                             ! INOUT Surface FTL for land tiles
,fqw_surft(land_pts,nsurft)                                                    &
                             ! INOUT Surface FQW for land tiles
,taux_land(udims%i_start:udims%i_end,udims%j_start:udims%j_end)                &
                             ! INOUT W'ly component of surface
                             !       wind stress over land
                             !       (N/sq m). (On
                             !       UV-grid with first and last
                             !       rows undefined or, at
                             !       present, set to missing data
,taux_ssi(udims%i_start:udims%i_end,udims%j_start:udims%j_end)                 &
                             ! INOUT W'ly component of surface
                             !       wind stress over mean sea
                             !       (N/sq m). (On
                             !       UV-grid with first and last
                             !       rows undefined or, at
                             !       present, set to missing data
,tauy_land(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)                &
                             ! INOUT S'ly component of land sfc
                             !       wind stress (N/sq m).  On
                             !       UV-grid; comments as per TAUX
,tauy_ssi(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)
                             ! INOUT S'ly component of land sfc
                             !       wind stress (N/sq m).  On
                             !       UV-grid; comments as per TAUX

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
 taux_land_star(udims%i_start:udims%i_end, udims%j_start:udims%j_end)          &
,tauy_land_star(vdims%i_start:vdims%i_end, vdims%j_start:vdims%j_end)          &
,taux_ssi_star( udims%i_start:udims%i_end, udims%j_start:udims%j_end)          &
,tauy_ssi_star( vdims%i_start:vdims%i_end, vdims%j_start:vdims%j_end)

!--------------------------------------------------------------------
!  Outputs :-
!-1 Diagnostic (or effectively so - includes coupled model requisites):-

!  (a) Calculated anyway (use STASH space from higher level) :-
!--------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 e_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! OUT Evaporation from sea times
                             !     leads fraction. Zero over
                             !     land. (kg per square metre
                             !     per sec).
,h_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! OUT Surface sensible heat flux
                             !     over sea times leads fraction
                             !     (W/m2)
,taux_1(udims%i_start:udims%i_end,udims%j_start:udims%j_end)                   &
                             ! OUT  W'ly component of surface
                             !      wind stress (N/sq m). (On
                             !      UV-grid with first and last
                             !      rows undefined or, at
                             !      present, set to missing data
,tauy_1(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)                   &
                             ! OUT  S'ly component of surface
                             !      wind stress (N/sq m).  On
                             !      UV-grid; comments as per TAUX
,ice_fract_cat_use(tdims%i_start:tdims%i_end,                                  &
                   tdims%j_start:tdims%j_end,nice_use)
                             ! OUT Sea ice category fractions
                             !     If nice_use=1, this is the
                             !     total ice fraction


!  Local scalars :-

INTEGER ::                                                                     &
 i,j                                                                           &
              ! LOCAL Loop counter (horizontal field index).
,n            ! LOCAL Loop counter (tile index).

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='JULES_GRID_UPDATE_IMPLICIT'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(n,j,i)                                                           &
!$OMP SHARED(nice_use,tdims,ice_fract_cat_use,ice_fract_ncat,ice_fract)

! Set up sea ice field depending on nice_use
IF (nice_use > 1) THEN
  ! Use all categories fully in surface exchange
  DO n = 1, nice_use
!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end
        ice_fract_cat_use(i,j,n) = ice_fract_ncat(i,j,n)
      END DO
    END DO
!$OMP END DO NOWAIT
  END DO
ELSE  ! nice_use=1
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      ice_fract_cat_use(i,j,1) = ice_fract(i,j)
    END DO
  END DO
!$OMP END DO NOWAIT
END IF

!$OMP END PARALLEL

CALL im_sf_pt2 (                                                               &
 land_pts,land_index,nsurft,surft_index,surft_pts                              &
,flandg,tile_frac,snow_surft,nice_use,ice_fract,ice_fract_cat_use              &
,r_gamma,gamma1,gamma2,alpha1,alpha1_sea,alpha1_sice                           &
,ashtf_prime,ashtf_prime_sea,ashtf_prime_surft                                 &
,resft,dtstar_surft,dtstar_sea,dtstar_sice                                     &
,rhokm_u_1,rhokm_v_1,rhokh_surft,rhokh_sice,rhokh_sea                          &
,ctctq1,dqw1_1,dtl1_1                                                          &
,cq_cm_u_1,cq_cm_v_1,du_1,dv_1,du_star1,dv_star1                               &
,flandg_u,flandg_v                                                             &
,fqw_1,ftl_1                                                                   &
,taux_1,taux_land,taux_land_star,taux_ssi,taux_ssi_star,tauy_1                 &
,tauy_land,tauy_land_star,tauy_ssi,tauy_ssi_star                               &
,fqw_surft,epot_surft,ftl_surft,fqw_ice,ftl_ice,e_sea,h_sea                    &
,l_correct                                                                     &
)

IF ( .NOT.  l_correct ) THEN
!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(j,i)                                                             &
!$OMP SHARED(tdims,ftl_1,cp)

  !-----------------------------------------------------------------------
  ! 6.1 Convert FTL to sensible heat flux in Watts per square metre.
  !-----------------------------------------------------------------------

!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      ftl_1(i,j) = ftl_1(i,j) * cp
    END DO
  END DO
!$OMP END DO NOWAIT

!$OMP END PARALLEL

ELSE ! L_correct = true: 2nd stage of the scheme
#if !defined(LFRIC)
  ! This code is written for an (i,j) latitude-longitude grid and therefore
  ! not compatible with LFRic, which has its own version
!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(SHARED)                                                          &
!$OMP PRIVATE(j,i)

  ! Correct surface stress diagnostics
!$OMP DO SCHEDULE(STATIC)
  DO j = udims%j_start,udims%j_end
    DO i = udims%i_start,udims%i_end
      taux_land(i,j) = taux_land(i,j) + taux_land_star(i,j)
      taux_ssi(i,j)  = taux_ssi(i,j)  + taux_ssi_star(i,j)
    END DO
  END DO
!$OMP END DO NOWAIT

!$OMP DO SCHEDULE(STATIC)
  DO j = vdims%j_start,vdims%j_end
    DO i = vdims%i_start,vdims%i_end
      tauy_land(i,j) = tauy_land(i,j) + tauy_land_star(i,j)
      tauy_ssi(i,j)  = tauy_ssi(i,j)  + tauy_ssi_star(i,j)
    END DO
  END DO
!$OMP END DO NOWAIT

!$OMP END PARALLEL
#endif
END IF ! IF .NOT. L_correct

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE jules_grid_update_implicit
END MODULE jules_grid_update_implicit_mod
