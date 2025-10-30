! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE jules_ssi_sf_implicit_mod

USE sf_melt_mod, ONLY: sf_melt
USE screen_tq_mod, ONLY: screen_tq
USE im_sf_pt2_mod, ONLY: im_sf_pt2
USE sice_htf_mod, ONLY: sice_htf

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
                  ModuleName='JULES_SSI_SF_IMPLICIT_MOD'

CONTAINS
!  SUBROUTINE JULES_SSI_SF_IMPLICIT ---------------------------------
!
!  Purpose: Calculate implicit correction for sea and sea-ice points
!           to surface fluxes of heat,moisture and momentum, to be used
!           by the unconditionally stable and non-oscillatory BL
!           numerical solver.
!
!--------------------------------------------------------------------
!    Arguments :-
SUBROUTINE jules_ssi_sf_implicit (                                             &
! IN values defining field dimensions and subset to be processed :
 nice,nice_use,flandg,                                                         &
! IN values defining water tracer field dimensions :
 n_wtrac_jls,                                                                  &
! IN sea/sea-ice data :
 ice_fract,ice_fract_ncat,ice_fract_cat_use,k_sice,di_ncat,sstfrz,             &
! IN everything not covered so far :
 lw_down,r_gamma,alpha1_sice,ashtf_prime,                                      &
 dtrdz_charney_grid_1,rhokh_sice,l_correct,                                    &
! INOUT data :
 fqw_ice,ftl_ice,                                                              &
 tstar_sice_cat,tstar_ssi,tstar_sea,                                           &
 radnet_sice,fqw_1,ftl_1,ti,sf_diag,                                           &
! OUT Diagnostic not requiring STASH flags :
 ti_gb,sea_ice_htf,                                                            &
 surf_ht_flux_sice,                                                            &
! OUT data required elsewhere in UM system :
 tstar_sice,e_sea,h_sea,ei_sice,                                               &
 dtstar_sea,dtstar_sice,                                                       &
 surf_ht_flux_sice_sm,ei_sice_sm,tstar_ssi_old,                                &
 sice_melt,                                                                    &
 ! ancil_info (IN)
 ssi_index, sice_index, sice_index_ncat, fssi_ij, sice_frac, sice_frac_ncat,   &
 ocn_cpl_point, sice_pts_ncat, sea_index, ssi_pts, sice_pts, sea_pts,          &
 ! fluxes (IN)
 sw_sicat,                                                                     &
 ! water tracers (IN)
 fqw_sea_wtrac, fqw_sicat_wtrac,                                               &
 ! water tracers (IN OUT)
 fqw_1_wtrac, dfqw_wtrac,                                                      &
 ! water tracers (OUT)
 e_sea_wtrac, ei_sice_wtrac)

USE csigma,                   ONLY:                                            &
 sbcon

USE planet_constants_mod,     ONLY: cp

USE atm_fields_bounds_mod,    ONLY: tdims, pdims

USE theta_field_sizes,        ONLY: t_i_length, t_j_length

USE jules_surface_mod,        ONLY:                                            &
  ls, ip_scrndecpl2, ip_scrndecpl3

USE jules_sea_seaice_mod,     ONLY: l_sice_multilayers

USE jules_sea_seaice_mod,     ONLY:                                            &
  l_tstar_sice_new, emis_sice, l_use_dtstar_sea

USE jules_snow_mod,           ONLY: rho_snow_const

USE sf_diags_mod, ONLY: strnewsfdiag

USE timestep_mod,             ONLY: timestep

USE jules_surface_mod,        ONLY:                                            &
  iscrntdiag

USE water_constants_mod,      ONLY:                                            &
  lc, lf, tfs

USE jules_water_tracers_mod,  ONLY: l_wtrac_imp_jls, standard_ratio_wtrac
USE sf_ssi_imp_wtrac_mod,     ONLY: sf_ssi_imp_wtrac

USE parkind1,                 ONLY:                                            &
  jprb, jpim

USE yomhook,                  ONLY:                                            &
  lhook, dr_hook

USE jules_science_fixes_mod,  ONLY: l_fix_lake_ice_temperatures

IMPLICIT NONE
!--------------------------------------------------------------------
!  Inputs :-
! (a) Defining horizontal grid and subset thereof to be processed.
!    Checked for consistency.
!--------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
 ssi_pts, sice_pts, sea_pts,                                                   &
 nice                                                                          &
                             ! IN Number of sea ice categories
,nice_use                    ! IN Number of sea ice categories used
                             !    fully in surface exchange

INTEGER, INTENT(IN) ::                                                         &
 n_wtrac_jls
                             ! IN No. of water tracers in JULES

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 flandg(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! IN Land fraction on all pts.

! (d) Sea/sea-ice data.
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 ice_fract(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                &
                             ! IN Fraction of gridbox covered by
                             !     sea-ice (decimal fraction).
,ice_fract_ncat(tdims%i_start:tdims%i_end,                                     &
                tdims%j_start:tdims%j_end,nice)                                &
                             ! IN Fraction of gridbox
                             !  covered by sea-ice on categories.
,ice_fract_cat_use(tdims%i_start:tdims%i_end,                                  &
                   tdims%j_start:tdims%j_end,nice_use)                         &
                             ! IN Sea ice category fractions
                             !    If nice_use=1, this is the total ice
                             !    fraction
,k_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,nice)              &
                             ! IN sea ice effective conductivity in
                             !     sfc layer on categories (W/m2/k)
,di_ncat(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,nice)             &
                             ! IN "Equivalent thickness" of
                             !    sea-ice (m).
,sstfrz(tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end)
                             ! IN Sea surface freezing temperature (K).

! (f) Atmospheric + any other data not covered so far, incl control.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 lw_down(tdims%i_start:tdims%i_end,                                            &
         tdims%j_start:tdims%j_end)
                             ! IN Surface downward LW radiation
                             !    (W/m2).

REAL(KIND=real_jlslsm), INTENT(IN) :: r_gamma
                             ! IN implicit weight in level 1

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 alpha1_sice(tdims%i_start:tdims%i_end,                                        &
             tdims%j_start:tdims%j_end,nice_use)                               &
                             ! IN ALPHA1 for sea-ice.
,ashtf_prime(tdims%i_start:tdims%i_end,                                        &
             tdims%j_start:tdims%j_end,nice_use)                               &
                             ! IN Adjusted SEB coefficient for
                             !    sea-ice
,dtrdz_charney_grid_1(pdims%i_start:pdims%i_end,                               &
                      pdims%j_start:pdims%j_end)                               &
                             ! IN -g.dt/dp for model layers.
,rhokh_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,               &
                                                        nice_use)
                             ! IN Surface exchange coefficients
                             !    for sea-ice

LOGICAL, INTENT(IN) ::                                                         &
 l_correct                   ! IN flag used by the new BL solver


!--------------------------------------------------------------------
!  In/outs :-
!--------------------------------------------------------------------
TYPE (strnewsfdiag), INTENT(IN OUT) :: sf_diag
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
 fqw_ice(tdims%i_start:tdims%i_end,                                            &
         tdims%j_start:tdims%j_end,nice_use)                                   &
                             ! INOUT Surface FQW for sea-ice
,ftl_ice(tdims%i_start:tdims%i_end,                                            &
         tdims%j_start:tdims%j_end,nice_use)                                   &
                             ! INOUT Surface FTL for sea-ice
,dtstar_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)               &
                             ! INOUT Change is TSTAR over timestep
                             !       for open sea
,dtstar_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,nice_use)     &
                             ! INOUT Change is TSTAR over timestep
                             !       for sea-ice
,tstar_sice_cat(tdims%i_start:tdims%i_end,                                     &
                tdims%j_start:tdims%j_end,nice_use)                            &
                             ! INOUT  Sea-ice sfc temperature (K).
,tstar_ssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                &
                             ! INOUT Sea mean sfc temperature (K).
,tstar_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                &
                             ! IN    Open sea sfc temperature (K).
,radnet_sice(tdims%i_start:tdims%i_end,                                        &
             tdims%j_start:tdims%j_end,nice_use)                               &
                             ! INOUT Surface net radiation on
                             !       sea-ice (W/m2)
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
,ti(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,nice)
                             ! INOUT  Sea-ice surface layer
                             !       temperature (K).

!--------------------------------------------------------------------
!  Outputs :-
!-1 Diagnostic (or effectively so - includes coupled model requisites):-

!  (a) Calculated anyway (use STASH space from higher level) :-
!--------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 ti_gb(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! OUT GBM ice surface temperature (K)
,sea_ice_htf(tdims%i_start:tdims%i_end,                                        &
             tdims%j_start:tdims%j_end,nice)
                             ! OUT Heat flux through sea-ice
                             !     (W/m2, positive downwards).
                             !     (Not used for JULES)

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
 surf_ht_flux_sice(tdims%i_start:tdims%i_end,                                  &
                   tdims%j_start:tdims%j_end,nice)
                             ! OUT Net category downward heat flux at
                             !     surface over sea-ice

!-2 Genuinely output, needed by other atmospheric routines :-

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 tstar_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)               &
                             ! OUT Ice area mean sea ice surface temperature
,ei_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,nice_use)         &
                             ! OUT Sublimation from sea-ice
                             !     (kg/m2/s).
,surf_ht_flux_sice_sm(tdims%i_start:tdims%i_end,                               &
                      tdims%j_start:tdims%j_end)                               &
                             ! OUT Sea area mean seaice surface heat flux
,ei_sice_sm(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)               &
                             ! OUT Sea area mean sea ice sublimation
,sice_melt(tdims%i_start:tdims%i_end,                                          &
           tdims%j_start:tdims%j_end,nice)                                     &
                             !Melt at surface sea-ice category
,tstar_ssi_old(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! OUT Sea and sea-ice surface temperature
                             !     at beginning of timestep --
                             !     Required only for decoupled diagnosis,
                             !     so allocatable, and local since it is
                             !     used only on the predictor step
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
 e_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! INOUT Evaporation from sea times
                             !       leads fraction. Zero over
                             !       land. (kg per square metre
                             !       per sec).
, h_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! INOUT Surface sensible heat flux
                             !       over sea times leads fraction
                             !       (W/m2)

! ancil_info (IN)
INTEGER, INTENT(IN) ::                                                         &
  ssi_index(t_i_length * t_j_length),                                          &
  sice_index(t_i_length * t_j_length),                                         &
  sice_index_ncat(t_i_length * t_j_length, nice),                              &
  sice_pts_ncat(nice),                                                         &
  sea_index(t_i_length * t_j_length)

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  fssi_ij(t_i_length * t_j_length),                                            &
  sice_frac(t_i_length * t_j_length),                                          &
  sice_frac_ncat(t_i_length * t_j_length,nice)

LOGICAL, INTENT(IN) ::                                                         &
 ocn_cpl_point(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                            ! IN Is this point involved in ocean-atmosphere
                            ! coupling

!fluxes (IN)
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  sw_sicat(t_i_length * t_j_length, nice_use)

! Water tracers (IN)
REAL(KIND=real_jlslsm), INTENT(IN) :: fqw_sea_wtrac(                           &
                                               tdims%i_start:tdims%i_end,      &
                                               tdims%j_start:tdims%j_end,      &
                                               n_wtrac_jls)
                            ! Water tracer surface flux over open ocean
                            !  from explicit scheme (kg/m2/s)
REAL(KIND=real_jlslsm), INTENT(IN) :: fqw_sicat_wtrac(                         &
                                               tdims%i_start:tdims%i_end,      &
                                               tdims%j_start:tdims%j_end,      &
                                               nice_use, n_wtrac_jls)
                            ! Water tracer surface flux over sea ice
                            !  from explicit scheme per ice cat (kg/m2/s)
! Water tracers (INOUT)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: fqw_1_wtrac(                         &
                                         tdims%i_start:tdims%i_end,            &
                                         tdims%j_start:tdims%j_end,n_wtrac_jls)
                            ! Water tracer surface moisture flux  (kg/m2/s)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: dfqw_wtrac(                          &
                                                   tdims%i_start:tdims%i_end,  &
                                                   tdims%j_start:tdims%j_end,  &
                                                   n_wtrac_jls)
                            ! Increment to water tracer surface moisture flux
                            !   (kg/m2/s)

! Water tracers (OUT)
REAL(KIND=real_jlslsm), INTENT(OUT) :: e_sea_wtrac(tdims%i_start:tdims%i_end,  &
                                         tdims%j_start:tdims%j_end,n_wtrac_jls)
                            ! Water tracer evaporation from sea times leads
                            !   fraction (kg/m2/s)
REAL(KIND=real_jlslsm), INTENT(OUT) :: ei_sice_wtrac(                          &
                                            tdims%i_start:tdims%i_end,         &
                                            tdims%j_start:tdims%j_end,         &
                                            nice_use,n_wtrac_jls)
                            ! Water tracer sublimation from sea ice per
                            !   ice category (kg/m2/s)

!--------------------------------------------------------------------
!  Workspace :-
!--------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
 tstar_sice_cat_old(tdims%i_start:tdims%i_end,                                 &
           tdims%j_start:tdims%j_end,nice_use)                                 &
                             ! Sea ice surface T at beginning of timestep
,tstar_sea_old(tdims%i_start:tdims%i_end,                                      &
               tdims%j_start:tdims%j_end)                                      &
                             ! Sea surface T at beginning of timestep
,dftl_sice_ncat(tdims%i_start:tdims%i_end,                                     &
                tdims%j_start:tdims%j_end)                                     &
                             ! Increment for ftl_ice from sea-ice
                             ! melt calculated for each category,
                             ! but un-weighted by category fractions
,dfqw_sice_ncat(tdims%i_start:tdims%i_end,                                     &
                tdims%j_start:tdims%j_end)                                     &
                             ! Increment for fqw_ice from sea-ice
                             ! melt calculated for each category,
                             ! but un-weighted by category fractions
,dei_sice_ncat(tdims%i_start:tdims%i_end,                                      &
               tdims%j_start:tdims%j_end)                                      &
                             ! Increment for ei_sice from sea-ice
                             ! melt calculated for each category,
                             ! but un-weighted by category fractions
,snowinc_sice(tdims%i_start:tdims%i_end,                                       &
           tdims%j_start:tdims%j_end,1)
                             ! Snow increment on sea ice category.
                             ! Needed for consistency with the land surface
                             ! but not used directly, so only allocated
                             ! with a size of 1 in the third dimension

REAL(KIND=real_jlslsm), ALLOCATABLE :: tstar_sic(:,:,:)
                             !Ice category surface temperature
                             ! Only used if nice_use EQ 1

REAL(KIND=real_jlslsm), ALLOCATABLE :: sea_frac(:)
                             ! Leads fraction (used for water tracers)

! dummy arrays required for sea and sea-ice to create universal
! routines for all surfaces
REAL(KIND=real_jlslsm) ::                                                      &
 array_one(t_i_length * t_j_length)                                            &
                             ! Array of ones
,array_one_e_six(t_i_length * t_j_length)
                             ! Array of 1.0e6

!  Local scalars :-

INTEGER ::                                                                     &
 i,j                                                                           &
            ! LOCAL Loop counter (horizontal field index).
,k                                                                             &
            ! LOCAL Tile pointer
,l                                                                             &
            ! LOCAL Land pointer
,n                                                                             &
            ! LOCAL Loop counter (tile index).
,i_wt
            ! LOCAL Loop counter (water tracers)
LOGICAL ::                                                                     &
 l_sice_new_code   ! Controls the sea ice temperature calculation
                   ! (See comments in sea ice section below)

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='JULES_SSI_SF_IMPLICIT'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

  !-----------------------------------------------------------------------
  ! Sea and sea-ice surface calculations
  !-----------------------------------------------------------------------

IF ( .NOT. l_correct ) THEN

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(l,n,j,i)                                                         &
!$OMP SHARED(t_i_length,t_j_length,array_one,array_one_e_six)


!$OMP DO SCHEDULE(STATIC)
  DO l = 1, t_i_length * t_j_length
    array_one(l)       = 1.0
    array_one_e_six(l) = 1.0e6
  END DO
!$OMP END DO NOWAIT
!$OMP END PARALLEL

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(n,j,i)                                                           &
!$OMP SHARED(tdims,h_sea,nice_use,ftl_ice,cp)

  !-----------------------------------------------------------------------
  ! 6.1 Convert FTL to sensible heat flux in Watts per square metre.
  !-----------------------------------------------------------------------

!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      h_sea(i,j) = cp * h_sea(i,j)
    END DO
  END DO
!$OMP END DO NOWAIT

  DO n = 1,nice_use
!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end
        ftl_ice(i,j,n) = cp * ftl_ice(i,j,n)
      END DO
    END DO
!$OMP END DO NOWAIT
  END DO
!$OMP END PARALLEL




  ! Allow tstar_sea to be modified by dtstar_sea
  IF (l_use_dtstar_sea) THEN
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end
        IF ( flandg(i,j) < 1.0 ) THEN
          tstar_sea_old(i,j) = tstar_sea(i,j)
          tstar_sea(i,j) = tstar_sea_old(i,j) + dtstar_sea(i,j)
        END IF
      END DO
    END DO
  END IF

  ! Set control logical (l_sice_new_code) to determine how to calculate the
  ! sea ice surface temperature.  l_sice_new_code = T is the method that is
  ! compatible with using the sea ice categories fully in the radiation and
  ! surface exchange code (so nice_use=nice).  l_sice_new_code = F is the
  ! old method that only uses the categories in the implicit surface exchange
  ! code (so nice_use = 1).

  ! NOTE that nice_use=nice=1 can use either scheme and the choice is
  ! determined by the logical l_tstar_sice_new set in switches.F90.  The
  ! difference between the 2 methods is small but causes differences in the
  ! results that are larger than bit level, hence the need to control which
  ! method is used.

  IF (nice_use == 1 .AND. nice == 1) THEN
    ! If nice_use=nice=1 : Choice method determined by logical
    ! l_tstar_sice_new
    l_sice_new_code = l_tstar_sice_new

  ELSE IF (nice_use /= nice) THEN
    ! Old calculation must be used
    l_sice_new_code = .FALSE.

  ELSE IF (nice > 1) THEN
    ! New calculation must be used
    l_sice_new_code = .TRUE.
  END IF

  IF ( .NOT. l_sice_new_code) THEN

    ALLOCATE(tstar_sic(tdims%i_start:tdims%i_end,                              &
                       tdims%j_start:tdims%j_end,nice))

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(n,j,i)                                                           &
!$OMP SHARED(tstar_sic,nice,tdims)
    DO n = 1, nice
!$OMP DO SCHEDULE(STATIC)
      DO j = tdims%j_start,tdims%j_end
        DO i= tdims%i_start,tdims%i_end
          tstar_sic(i,j,n) = 0.0
        END DO
      END DO
!$OMP END DO NOWAIT
    END DO
!$OMP END PARALLEL
  END IF

  !-----------------------------------------------------------------------
  ! Store old surface temperature for sea and sea-ice if using the
  ! decoupled diagnostic.
  !-----------------------------------------------------------------------
  IF ((IScrnTDiag == IP_ScrnDecpl2) .OR. (IScrnTDiag == IP_ScrnDecpl3)) THEN

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(j,i)                                                             &
!$OMP SHARED(tdims,tstar_ssi_old,tstar_ssi)
    DO j = tdims%j_start,tdims%j_end
      DO i= tdims%i_start,tdims%i_end
        tstar_ssi_old(i,j) = tstar_ssi(i,j)
      END DO
    END DO
!$OMP END PARALLEL DO

  END IF

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(n,j,i)                                                           &
!$OMP SHARED(surf_ht_flux_sice_sm,sice_melt,sea_ice_htf,ei_sice,fqw_ice,       &
!$OMP        snowinc_sice,                                                     &
!$OMP        tdims,nice,nice_use,sf_diag)

!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i= tdims%i_start,tdims%i_end
      surf_ht_flux_sice_sm(i,j) = 0.0
    END DO
  END DO
!$OMP END DO NOWAIT

  DO n = 1, nice
!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start,tdims%j_end
      DO i= tdims%i_start,tdims%i_end
        sice_melt(i,j,n)          = 0.0
        sea_ice_htf(i,j,n)        = 0.0
      END DO
    END DO
!$OMP END DO NOWAIT
  END DO

!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i= tdims%i_start,tdims%i_end
      snowinc_sice(i,j,1)       = 0.0
    END DO
  END DO
!$OMP END DO NOWAIT

  DO n = 1, nice_use
!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start,tdims%j_end
      DO i= tdims%i_start,tdims%i_end
        ei_sice(i,j,n)            = fqw_ice(i,j,n)
      END DO
    END DO
!$OMP END DO NOWAIT
  END DO

  IF (sf_diag%simlt) THEN
    DO n = 1, nice_use
!$OMP DO SCHEDULE(STATIC)
      DO j = tdims%j_start,tdims%j_end
        DO i= tdims%i_start,tdims%i_end
          sf_diag%sice_mlt_htf(i,j,n) = 0.0
        END DO
      END DO
!$OMP END DO NOWAIT
    END DO
  END IF

!$OMP END PARALLEL

  !-----------------------------------------------------------------------
  ! Diagnose the surface temperature for points with sea-ice
  ! Note that k_sice = 2.0*thermal conductivity/surface layer thickness
  !-----------------------------------------------------------------------

  IF (l_sice_new_code) THEN

    ! Update tstar_sice_cat using dtstar_sice

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(n,j,i)                                                           &
!$OMP SHARED(nice_use,tdims,flandg,ice_fract_cat_use,tstar_sice_cat_old,       &
!$OMP        tstar_sice_cat,dtstar_sice)
    DO n = 1,nice_use
!$OMP DO SCHEDULE(STATIC)
      DO j = tdims%j_start,tdims%j_end
        DO i = tdims%i_start,tdims%i_end
          IF ( flandg(i,j) < 1.0 .AND. ice_fract_cat_use(i,j,n) > 0 ) THEN
            tstar_sice_cat_old(i,j,n) = tstar_sice_cat(i,j,n)
            tstar_sice_cat(i,j,n) = tstar_sice_cat_old(i,j,n) +                &
                                    dtstar_sice(i,j,n)
          END IF
        END DO
      END DO
!$OMP END DO NOWAIT
    END DO
!$OMP END PARALLEL

  ELSE  ! Use old code

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(l,j,i,n)                                                         &
!$OMP SHARED(tdims,flandg,ice_fract,surf_ht_flux_sice_sm,radnet_sice,          &
!$OMP        tstar_sice_cat,dtstar_sice,ftl_ice,fqw_ice,ice_fract_ncat,        &
!$OMP        tstar_sic,ti,k_sice,emis_sice,nice)
    ! Update tstar_sic using the surface heat equation
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end
        IF ( flandg(i,j) < 1.0 .AND. ice_fract(i,j) > 0.0 ) THEN
          surf_ht_flux_sice_sm(i,j) = radnet_sice(i,j,1) -                     &
             4.0 * emis_sice * sbcon * (tstar_sice_cat(i,j,1)**3.0) *          &
                 dtstar_sice(i,j,1) - ftl_ice(i,j,1) - ls * fqw_ice(i,j,1)
          DO n = 1,nice
            IF ( ice_fract_ncat(i,j,n) > 0.0 ) THEN
              tstar_sic(i,j,n) = ti(i,j,n) +                                   &
                       surf_ht_flux_sice_sm(i,j) / k_sice(i,j,n)
            END IF
          END DO
        END IF
      END DO
    END DO
!$OMP END PARALLEL DO
  END IF

  IF (l_sice_new_code) THEN

    DO n = 1, nice_use
      CALL sf_melt (                                                           &
        ssi_pts,ssi_index,                                                     &
        sice_index_ncat(:,n),sice_pts_ncat(n),fssi_ij,                         &
        alpha1_sice(:,:,n),ashtf_prime(:,:,n),dtrdz_charney_grid_1,            &
        array_one,array_one,rhokh_sice(:,:,n),sice_frac_ncat(:,n),timestep,    &
        r_gamma,                                                               &
        ei_sice(:,:,n),fqw_1,ftl_1,fqw_ice(:,:,n),ftl_ice(:,:,n),              &
        tstar_sice_cat(:,:,n),array_one_e_six,                                 &
        array_one_e_six / rho_snow_const,                                      &
        sice_melt(:,:,n),snowinc_sice(:,:,1)                                   &
        )

      IF (sf_diag%simlt) THEN
!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(k,j,i,l)                                                         &
!$OMP SHARED(sice_pts_ncat,n,sice_index_ncat,ssi_index,t_i_length,sf_diag,     &
!$OMP        sice_melt)
        DO k = 1,sice_pts_ncat(n)
          l = sice_index_ncat(k,n)
          j=(ssi_index(l) - 1) / t_i_length + 1
          i = ssi_index(l) - (j-1) * t_i_length
          sf_diag%sice_mlt_htf(i,j,n) = lf * sice_melt(i,j,n)
        END DO
!$OMP END PARALLEL DO
      END IF

    END DO

  ELSE ! Use old code

    DO n = 1,nice

      ! Since sea-ice categories are not actually tiled for their surface
      ! fluxes here, then the increment to ftl_ice, fqw_ice and
      ! ei_sice are not correctly weighted in sf_melt. Hence need to keep
      ! the increments and update ftl_ice, fqw_ice and ei_sice with
      ! weighted contributions below

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(j,i)                                                             &
!$OMP SHARED(tdims,dftl_sice_ncat,dfqw_sice_ncat,dei_sice_ncat)
      DO j = tdims%j_start,tdims%j_end
        DO i = tdims%i_start,tdims%i_end
          dftl_sice_ncat(i,j) = 0.0
          dfqw_sice_ncat(i,j) = 0.0
          dei_sice_ncat(i,j)  = 0.0
        END DO
      END DO
!$OMP END PARALLEL DO

      CALL sf_melt (                                                           &
        ssi_pts,ssi_index,                                                     &
        sice_index_ncat(:,n),sice_pts_ncat(n),fssi_ij,                         &
        alpha1_sice(:,:,1),ashtf_prime(:,:,1),dtrdz_charney_grid_1,            &
        array_one,array_one,rhokh_sice(:,:,1),sice_frac_ncat(:,n),timestep,    &
        r_gamma,                                                               &
        dei_sice_ncat,fqw_1,ftl_1,dfqw_sice_ncat,dftl_sice_ncat,               &
        tstar_sic(:,:,n),array_one_e_six,                                      &
        array_one_e_six / rho_snow_const,                                      &
        sice_melt(:,:,n),snowinc_sice(:,:,1)                                   &
        )

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(k,l,j,i)                                                         &
!$OMP SHARED(sice_pts_ncat,n,sice_index_ncat,ssi_index,t_i_length,sf_diag,     &
!$OMP        sice_melt,ftl_ice,sice_frac_ncat,sice_frac,dftl_sice_ncat,        &
!$OMP        fqw_ice,dfqw_sice_ncat,ei_sice,dei_sice_ncat)
      DO k = 1,sice_pts_ncat(n)
        l = sice_index_ncat(k,n)
        j=(ssi_index(l) - 1) / t_i_length + 1
        i = ssi_index(l) - (j-1) * t_i_length
        IF (sf_diag%simlt) sf_diag%sice_mlt_htf(i,j,n) = lf * sice_melt(i,j,n)
        ! Add weighted increments to ftl_ice, fqw_ice and ei_sice
        ftl_ice(i,j,1) = ftl_ice(i,j,1)                                        &
           +( sice_frac_ncat(l,n) / sice_frac(l) ) * dftl_sice_ncat(i,j)
        fqw_ice(i,j,1) = fqw_ice(i,j,1)                                        &
           +( sice_frac_ncat(l,n) / sice_frac(l) ) * dfqw_sice_ncat(i,j)
        ei_sice(i,j,1) = ei_sice(i,j,1)                                        &
           +( sice_frac_ncat(l,n) / sice_frac(l) ) * dei_sice_ncat(i,j)
      END DO
!$OMP END PARALLEL DO
    END DO

  END IF

  !--------------------------------------------------------------------------
  ! Update water tracers for implicit calculation
  !--------------------------------------------------------------------------

  IF (l_wtrac_imp_jls) THEN

    ALLOCATE(sea_frac(t_i_length * t_j_length))        ! Leads fraction

    DO i_wt = 1, n_wtrac_jls

      ! Set up leads fraction
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(l)                    &
!$OMP SHARED(sice_frac,sea_frac,t_i_length,t_j_length)
      DO l = 1, t_i_length * t_j_length
        sea_frac(l) = 1.0 - sice_frac(l)
      END DO
!$OMP END PARALLEL DO

      ! Ocean
      CALL sf_ssi_imp_wtrac(                                                   &
        ssi_pts, sea_pts, ssi_index, sea_index,                                &
        standard_ratio_wtrac(i_wt), sea_frac, fssi_ij,  e_sea,                 &
        fqw_sea_wtrac(:,:,i_wt), fqw_sea_wtrac(:,:,1),                         &
        fqw_1_wtrac(:,:,i_wt), dfqw_wtrac(:,:,i_wt),                           &
        e_sea_wtrac(:,:,i_wt))

      ! Sea ice
      DO n = 1, nice
        CALL sf_ssi_imp_wtrac(                                                 &
          ssi_pts, sice_pts_ncat(n), ssi_index, sice_index_ncat(:,n),          &
          standard_ratio_wtrac(i_wt), sice_frac_ncat(:,n), fssi_ij,            &
          fqw_ice(:,:,n),                                                      &
          fqw_sicat_wtrac(:,:,n,i_wt), fqw_sicat_wtrac(:,:,n,1),               &
          fqw_1_wtrac(:,:,i_wt), dfqw_wtrac(:,:,i_wt),                         &
          ei_sice_wtrac(:,:,n,i_wt))
      END DO

    END DO   ! i_wt
    DEALLOCATE(sea_frac)
  END IF  ! l_wtrac_imp_jls

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(SHARED)                                                          &
!$OMP PRIVATE(i,j,k,l,n)

  !-----------------------------------------------------------------------
  !     Gridbox-mean surface temperature and net surface heat fluxes
  !-----------------------------------------------------------------------

  ! Assign SW flux on categories to radnet_sice to avoid cumbersome
  ! indexing in the following loops.

  DO n = 1,nice_use
!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end
        radnet_sice(i,j,n) = 0.0
      END DO
    END DO
!$OMP END DO
  END DO

  IF (nice_use > 1) THEN
    !   In this case, nice_use = nice.
    !   Radiative fluxes are on all categories in use, so use the
    !   full arrays.
    DO n = 1,nice_use
!$OMP DO SCHEDULE(STATIC)
      DO k = 1,sice_pts_ncat(n)
        l = sice_index_ncat(k,n)
        j=(ssi_index(l) - 1) / t_i_length + 1
        i = ssi_index(l) - (j-1) * t_i_length
        radnet_sice(i,j,n) = sw_sicat(l,n)
      END DO
!$OMP END DO
    END DO
  ELSE
    !   In this case n_ice_use must be 1, so indexing is over all sea-ice
    !   points.
!$OMP DO SCHEDULE(STATIC)
    DO k = 1,sice_pts
      l = sice_index(k)
      j=(ssi_index(l) - 1) / t_i_length + 1
      i = ssi_index(l) - (j-1) * t_i_length
      radnet_sice(i,j,1) = sw_sicat(l,1)
    END DO
!$OMP END DO
  END IF


  IF (l_sice_new_code) THEN

!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end
        surf_ht_flux_sice_sm(i,j) = 0.0
        DO n = 1,nice_use
          surf_ht_flux_sice(i,j,n)= 0.0
        END DO
        IF ( flandg(i,j) < 1.0 .AND. ice_fract(i,j) > 0.0 ) THEN
          tstar_sice(i,j)= 0.0
          DO n = 1,nice_use
            IF ( ice_fract_ncat(i,j,n) > 0.0 ) THEN
              tstar_sice(i,j) = tstar_sice(i,j) +                              &
                                (ice_fract_ncat(i,j,n) /                       &
                               ice_fract(i,j)) * tstar_sice_cat(i,j,n)
            ELSE
              tstar_sice_cat(i,j,n) = tfs   ! copy setting in sice_htf
            END IF
          END DO
          tstar_ssi(i,j) = (1.0 - ice_fract(i,j)) * tstar_sea(i,j) +           &
                            ice_fract(i,j) * tstar_sice(i,j)


          DO n = 1,nice_use
            IF ( ice_fract_cat_use(i,j,n) > 0.0 ) THEN

              radnet_sice(i,j,n) = radnet_sice(i,j,n) + emis_sice *            &
                ( lw_down(i,j) - sbcon * tstar_sice_cat(i,j,n)**4 )

              surf_ht_flux_sice(i,j,n) = radnet_sice(i,j,n) -                  &
                               ftl_ice(i,j,n) - ls * fqw_ice(i,j,n) -          &
                               lf * sice_melt(i,j,n)
              surf_ht_flux_sice_sm(i,j) = surf_ht_flux_sice_sm(i,j) +          &
                           (ice_fract_cat_use(i,j,n) / ice_fract(i,j))*        &
                                surf_ht_flux_sice(i,j,n)
            END IF
          END DO

        ELSE IF (flandg(i,j) < 1.0) THEN  ! non-icy ocean point
          tstar_sice_cat(i,j,:) = tfs
          tstar_ssi(i,j) = tstar_sea(i,j)
        END IF
        IF (nice == 1) THEN
          tstar_sice(i,j) = tstar_sice_cat(i,j,1)  ! Ensure these are the
                                                   ! same at all points
        END IF
      END DO
    END DO
!$OMP END DO NOWAIT

  ELSE   ! Use old code

!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end
        surf_ht_flux_sice_sm(i,j) = 0.0
        DO n = 1,nice
          surf_ht_flux_sice(i,j,n) = 0.0
        END DO
        IF ( flandg(i,j) < 1.0 .AND. ice_fract(i,j) > 0.0 ) THEN
          tstar_sice(i,j) = 0.0
          DO n = 1,nice
            IF ( ice_fract_ncat(i,j,n) > 0.0 ) THEN
              tstar_sice(i,j) = tstar_sice(i,j) +                              &
                                (ice_fract_ncat(i,j,n) /                       &
                                 ice_fract(i,j)) * tstar_sic(i,j,n)
            END IF
          END DO
          tstar_ssi(i,j) = (1.0 - ice_fract(i,j)) * tstar_sea(i,j) +           &
                            ice_fract(i,j) * tstar_sice(i,j)

          radnet_sice(i,j,1) = radnet_sice(i,j,1) + emis_sice *                &
            ( lw_down(i,j) - sbcon * tstar_sice(i,j)**4 )

          DO n = 1,nice
            IF ( ice_fract_ncat(i,j,n) > 0.0 ) THEN
              surf_ht_flux_sice(i,j,n) = radnet_sice(i,j,1) -                  &
                               4.0 * emis_sice * sbcon *                       &
                               tstar_sice(i,j)**3 *                            &
                               (tstar_sic(i,j,n) - tstar_sice(i,j)) -          &
                               ftl_ice(i,j,1) - ls * fqw_ice(i,j,1) -          &
                               lf * sice_melt(i,j,n)
              surf_ht_flux_sice_sm(i,j) = surf_ht_flux_sice_sm(i,j) +          &
                           (ice_fract_ncat(i,j,n) / ice_fract(i,j))*           &
                            surf_ht_flux_sice(i,j,n)

            END IF
          END DO

        ELSE IF (flandg(i,j) < 1.0) THEN  ! non-icy ocean point
          tstar_sice(i,j) = tfs
          tstar_ssi(i,j) = tstar_sea(i,j)
        END IF

        tstar_sice_cat(i,j,1) = tstar_sice(i,j)  ! Ensure these are the
                                                 ! same at all points
      END DO
    END DO
!$OMP END DO NOWAIT

  END IF

  ! Calculate sea mean diagnostics (_sm)

!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      h_sea(i,j)=(1.0 - ice_fract(i,j)) * h_sea(i,j)
      e_sea(i,j)=(1.0 - ice_fract(i,j)) * e_sea(i,j)
      surf_ht_flux_sice_sm(i,j) = ice_fract(i,j) * surf_ht_flux_sice_sm(i,j)
      ei_sice_sm(i,j) = 0.0
      DO n = 1,nice_use
        ei_sice_sm(i,j)= ei_sice_sm(i,j) +                                     &
                         ice_fract_cat_use(i,j,n) * ei_sice(i,j,n)
      END DO
    END DO
  END DO
!$OMP END DO NOWAIT

  IF (sf_diag%l_ftl_ice_sm) THEN
!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end
        sf_diag%ftl_ice_sm(i,j) = 0.0
        DO n = 1,nice_use
          sf_diag%ftl_ice_sm(i,j) = sf_diag%ftl_ice_sm(i,j) +                  &
                                    ftl_ice(i,j,n) * ice_fract_cat_use(i,j,n)
        END DO
      END DO
    END DO
!$OMP END DO NOWAIT
  END IF

  IF (sf_diag%l_lh_ssi) THEN
!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end
        sf_diag%lh_ssi(i,j) = lc * (e_sea(i,j) + ei_sice_sm(i,j)) + lf * ei_sice_sm(i,j)
      END DO
    END DO
!$OMP END DO NOWAIT
  END IF

  IF (sf_diag%l_tstar_sice_weighted) THEN
!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end
        sf_diag%tstar_sice_weighted(i,j) = ice_fract(i,j) * tstar_sice(i,j)
      END DO
    END DO
!$OMP END DO NOWAIT
  END IF

  IF (sf_diag%l_tstar_sice_weighted_cat) THEN
!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end
        DO n = 1,nice_use
          sf_diag%tstar_sice_weighted_cat(i,j,n) =                             &
             ice_fract_cat_use(i,j,n) * tstar_sice_cat(i,j,n)
        END DO
      END DO
    END DO
!$OMP END DO NOWAIT
  END IF

  ! Compute sea ice time fraction variable, required for CMIP6.
  IF (sf_diag%l_ice_present_cat) THEN
!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end
        DO n = 1,nice_use
          IF (ice_fract_cat_use(i,j,n) > 0.0) THEN
            sf_diag%ice_present_cat(i,j,n) = 1.0
          ELSE
            sf_diag%ice_present_cat(i,j,n) = 0.0
          END IF
        END DO
      END DO
    END DO
!$OMP END DO NOWAIT
  END IF

  IF (sf_diag%l_ice_present) THEN
!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end
        IF (ice_fract(i,j) > 0.0) THEN
          sf_diag%ice_present(i,j) = 1.0
        ELSE
          sf_diag%ice_present(i,j) = 0.0
        END IF
      END DO
    END DO
!$OMP END DO NOWAIT
  END IF

  ! Calculate surface upward LW over sea ice, weighted by ice fraction.  This
  ! is required for CMIP6.
  IF (sf_diag%l_lw_up_sice_weighted_cat) THEN
!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end
        DO n = 1,nice_use
          IF ((ice_fract_cat_use(i,j,n) > 0.0) .AND. (flandg(i,j) < 1.0)) THEN
            sf_diag%lw_up_sice_weighted_cat(i,j,n) =                           &
                                          ice_fract_cat_use(i,j,n)             &
                                          * (emis_sice                         &
                                          * sbcon * tstar_sice_cat(i,j,n)**4   &
                                          + (1.0 - emis_sice) * lw_down(i,j))
          ELSE
            sf_diag%lw_up_sice_weighted_cat(i,j,n) = 0.0
          END IF ! ice_fract_cat_use
        END DO   ! n
      END DO    ! i
    END DO    ! j
!$OMP END DO NOWAIT
  END IF

  IF (sf_diag%l_lw_up_sice_weighted) THEN
!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end
        IF ((ice_fract(i,j) > 0.0) .AND. (flandg(i,j) < 1.0)) THEN
          sf_diag%lw_up_sice_weighted(i,j) = ice_fract(i,j)                    &
                                             * (emis_sice * sbcon              &
                                                * tstar_sice(i,j)**4           &
                                                + (1.0 - emis_sice)            &
                                                * lw_down(i,j))
        ELSE
          sf_diag%lw_up_sice_weighted(i,j) = 0.0
        END IF    ! flandg, ice_fract
      END DO    ! i
    END DO    ! j
!$OMP END DO NOWAIT
  END IF

!$OMP END PARALLEL

  IF ( .NOT. l_sice_new_code) DEALLOCATE(tstar_sic)

  IF (l_wtrac_imp_jls) THEN
    ! Convert water tracer ocean evaporation into sea mean (to keep it
    ! consistent with e_sea above)
!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,i_wt)                                 &
!$OMP SHARED(n_wtrac_jls,tdims,e_sea_wtrac,ice_fract)
    DO i_wt = 1, n_wtrac_jls
!$OMP DO SCHEDULE(STATIC)
      DO j = tdims%j_start,tdims%j_end
        DO i = tdims%i_start,tdims%i_end
          e_sea_wtrac(i,j,i_wt) = (1.0 - ice_fract(i,j))                       &
                                           * e_sea_wtrac(i,j,i_wt)
        END DO
      END DO
!$OMP END DO NOWAIT
    END DO
!$OMP END PARALLEL
  END IF

ELSE ! L_correct = true: 2nd stage of the scheme

  ! The call to sice_htf should always be called to avoid constant
  ! sea ice layer temperatures at points not coupled to an ocean model.
  ! However to maintain backwards compatibility the temporary logical
  ! l_fix_lake_ice_temperatures (JULES#1161) is used to turn on the
  ! call to sice_htf. Once this backwards compatibility is no longer
  ! needed then the following IF test can be removed.
  IF ( ( .NOT. l_sice_multilayers) .OR. l_fix_lake_ice_temperatures) THEN
    !--------------------------------------------------------------------
    ! Update sea-ice surface layer temperature,
    ! if not coupled to multilayer sea ice model.
    !--------------------------------------------------------------------
    CALL sice_htf(                                                             &
      !IN fields
      flandg,nice,                                                             &
      di_ncat,ice_fract,ice_fract_ncat,surf_ht_flux_sice,sstfrz,               &
      ocn_cpl_point,                                                           &
      !INOUT fields
      ti,sf_diag,                                                              &
      !OUT fields
      ti_gb,sea_ice_htf                                                        &
      )
  ELSE
    sea_ice_htf(:,:,:) = 0.0   ! for safety
  END IF  !l_sice_multilayers

  ! Convert sea and sea-ice fluxes to be fraction of grid-box
  ! (as required by sea and sea-ice modellers)
  DO n = 1, nice
    DO j = pdims%j_start, pdims%j_end
      DO i = pdims%i_start, pdims%i_end
        sea_ice_htf(i,j,n)       =                                             &
          ice_fract_ncat(i,j,n) * sea_ice_htf(i,j,n)
      END DO !i
    END DO !j
  END DO  !n

  IF (sf_diag%simlt) THEN
    DO n = 1, nice
      DO j = pdims%j_start, pdims%j_end
        DO i = pdims%i_start, pdims%i_end
          sf_diag%sice_mlt_htf(i,j,n) = ice_fract_ncat(i,j,n) *                &
                                      sf_diag%sice_mlt_htf(i,j,n)
        END DO
      END DO
    END DO
  END IF

END IF ! IF .NOT. L_correct

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE jules_ssi_sf_implicit
END MODULE jules_ssi_sf_implicit_mod
