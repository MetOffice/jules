! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE screen_tq_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SCREEN_TQ_MOD'

CONTAINS
!   SUBROUTINE SCREEN_TQ----------------------------------------------
!
!  Purpose: Diagnose temperature and/or specific humidity at screen
!           height (1.5 metres), as requested via the STASH flags.
!
!---------------------------------------------------------------------
SUBROUTINE screen_tq (                                                         &
 land_pts,nsurft,                                                              &
 land_index,surft_index,surft_pts,flandg,                                      &
 sf_diag,chr1p5m,chr1p5m_sice,pstar,qw_1,resft,                                &
 tile_frac,tl_1,tstar_ssi,tstar_surft,                                         &
 z0hssi,z0h_surft,z0mssi,z0m_surft,z1,                                         &
 timestep,tstar_ssi_old,tstar_surft_old,                                       &
 l_co2_interactive, co2_mmr, co2_3d,                                           &
 f3_at_p, ustargbm, rho1,                                                      &
 tscrndcl_ssi,tscrndcl_surft,tstbtrans,                                        &
 l_mr_physics                                                                  &
 )

USE atm_fields_bounds_mod, ONLY: tdims, tdims_s
USE theta_field_sizes, ONLY: t_i_length, t_j_length

USE jules_surface_mod, ONLY: ip_scrndecpl2, ip_scrndecpl3,                     &
                              z_obs_wind, z_obs_tq, iscrntdiag

USE planet_constants_mod, ONLY: cp, grcp, g

USE sf_diags_mod, ONLY: strnewsfdiag
USE missing_data_mod, ONLY: rmdi
USE csigma, ONLY: sbcon

USE qsat_mod, ONLY: qsat, qsat_mix

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

INTEGER ::                                                                     &
 land_pts                                                                      &
                      ! IN Number of land points to be processed.
,nsurft                                                                        &
                      ! IN Number of tiles per land point.
,land_index(land_pts)                                                          &
                      ! IN Index of land points.
,surft_index(land_pts,nsurft)                                                  &
!                           ! IN Index of tile points.
,surft_pts(nsurft)
                      ! IN Number of tile points.

LOGICAL ::                                                                     &
 l_mr_physics         ! IN TRUE if mixing ratios used in
!                     !    boundary layer code

REAL(KIND=real_jlslsm) ::                                                      &
 flandg(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
!                           ! IN Fraction of gridbox which is land.
,chr1p5m(land_pts,nsurft)                                                      &
!                           ! IN Ratio of coefficients for  calculation
!                           !    of 1.5 m T.
,chr1p5m_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)             &
!                           ! IN Ratio of coefficients for  calculation
!                           !    of 1.5 m T.
,pstar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                       ! IN Surface pressure (Pa).
,qw_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                       ! IN Total water content of lowest
!                                 atmospheric layer (kg per kg air).
,resft(land_pts,nsurft)                                                        &
!                           ! IN Surface resistance factor.
,tile_frac(land_pts,nsurft)                                                    &
!                           ! IN Tile fractions.
,tl_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                      ! IN Liquid/frozen water temperature for
!                                lowest atmospheric layer (K).
,tstar_ssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                &
!                           ! IN Sea/sea-ice mean sfc temperature (K).
,tstar_surft(land_pts,nsurft)                                                  &
!                           ! IN Tile surface temperatures (K).
,z0hssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                      ! IN Roughness length for heat and
!                           !    moisture (m).
,z0h_surft(land_pts,nsurft)                                                    &
!                           ! IN Tile roughness lengths for heat and
!                           !    moisture (m).
,z0mssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                      ! IN Roughness length for momentum (m).
,z0m_surft(land_pts,nsurft)                                                    &
!                           ! IN Tile roughness lengths for momentum (m)
,z1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                            ! IN Height of lowest atmospheric level (m).

REAL(KIND=real_jlslsm), INTENT(IN)    :: timestep
                      ! Timestep of atmospheric model
REAL(KIND=real_jlslsm), INTENT(IN)    ::                                       &
  tstar_ssi_old(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                      ! Surface temperature at beginning of
                      ! timestep
REAL(KIND=real_jlslsm), INTENT(IN)    :: tstar_surft_old(land_pts,nsurft)
                      !    Surface temperature at beginning of
                      !    timestep

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
  ustargbm(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                      ! GBM surface friction velocity

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
                            !    Time since the transition

! Diagnostics
TYPE (strnewsfdiag), INTENT(IN OUT) :: sf_diag

REAL(KIND=real_jlslsm) ::                                                      &
 cer1p5m                                                                       &
                      ! Ratio of coefficients reqd for
!                           ! calculation of 1.5 m Q.
,pstar_land(land_pts)                                                          &
                      ! Surface pressure for land points.
,qs(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                       &
                      ! Surface saturated sp humidity.
,qs_surft(land_pts)    ! Surface saturated sp humidity.

!     Variables for the decoupled diagnostic

REAL(KIND=real_jlslsm) :: ks
                          ! Temporary in calculation of radiative
                          ! screen temperature
REAL(KIND=real_jlslsm) :: exks
                          ! Temporary in calculation of radiative
                          ! screen temperature
REAL(KIND=real_jlslsm) :: waterpath     ! Water path from the surface to the
                          ! screen-level (kgm-2)
REAL(KIND=real_jlslsm) :: CO2Path       ! Path of CO2 from the surface to the
                          ! screen-level (kgm-2)
REAL(KIND=real_jlslsm) :: TRadCool      ! Temporary variable holding the screen
                          ! temperature due to radiative cooling
                          ! of the previous decoupled value

REAL(KIND=real_jlslsm), PARAMETER ::                                           &
                   cc_c(0:5) = [ 1.25363, 1.56663, 0.763231,                   &
                       0.115082, 0.00789067, 0.000204645 ]
                          ! Fit used to calculate radiative cooling
                          ! due to CO2
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
                   cc_h(0:5) = [ -2.32590, -0.832639, 0.0498829,               &
                       0.0164081, 0.00148810, 5.06995e-05 ]
                          ! Fit used to calculate radiative cooling
                          ! due to water vapour
REAL(KIND=real_jlslsm), PARAMETER :: cc_h_large = 0.06376
                          ! Coefficient of fit to large water paths
                          ! of water vapour
REAL(KIND=real_jlslsm), PARAMETER :: t0_rfit = 288.15
                          ! Reference temperature for fit to
                          ! radiative cooling
REAL(KIND=real_jlslsm), PARAMETER :: dt_rfit = 15.0
                          ! Temperature scale for fit to
                          ! radiative cooling
REAL(KIND=real_jlslsm), PARAMETER :: pl0_h2o_rfit = 0.657638
                          ! Zeroth coefficient in fit to the derivative
                          ! of the Planckian for water vapour
REAL(KIND=real_jlslsm), PARAMETER :: pl1_h2o_rfit = 0.126593
                          ! First order coefficient in fit to the derivative
                          ! of the Planckian for water vapour
REAL(KIND=real_jlslsm), PARAMETER :: pl0_co2_rfit = 0.349957
                          ! Zeroth coefficient in fit to the derivative
                          ! of the Planckian for carbon dioxide
REAL(KIND=real_jlslsm), PARAMETER :: pl1_co2_rfit = 0.135816
                          ! First order coefficient in fit to the derivative
                          ! of the Planckian for carbon dioxide
REAL(KIND=real_jlslsm), PARAMETER :: t_scale_h2o_rfit = 0.015
                          ! Temperature-dependent variation of
                          ! the derivative of the emissivity of water vapour
REAL(KIND=real_jlslsm), PARAMETER :: t_scale_co2_rfit = 0.02
                          ! Temperature-dependent variation of
                          ! the derivative of the emissivity of CO2
LOGICAL, ALLOCATABLE :: LDclDiag(:, :)
                          ! Flag for full calculation of the decoupled
                          ! temperature including radiative cooling
                          ! and interpolation
REAL(KIND=real_jlslsm), ALLOCATABLE :: TStarGBM(:, :)
                          ! Grid-box mean surface temperature


REAL(KIND=real_jlslsm) :: QScrn
REAL(KIND=real_jlslsm) :: CO2Scrn
REAL(KIND=real_jlslsm) :: lp, tx
REAL(KIND=real_jlslsm) :: tRotational_Inv
REAL(KIND=real_jlslsm) :: ThDot, LTrans2
REAL(KIND=real_jlslsm) :: WeightDcl


INTEGER ::                                                                     &
i,j                                                                            &
            ! Loop counter (horizontal field index).
,k                                                                             &
            ! Loop counter (tile point index).
,l                                                                             &
            ! Loop counter (land point field index).
,n           ! Loop counter (tile index).

! introducing i_ind and j_ind arrays saves time on the calculation of indices
! i and j in numerous loops in the subroutine, in the future the same coding
! should be included in the top driver routine to avoid a repeated calculation
! of i and j in a number of JULES routines; the related values of i_ind and
! j_ind should be passed via a module
INTEGER :: i_ind(land_pts)
INTEGER :: j_ind(land_pts)

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SCREEN_TQ'

! Arrays for calculation of exchange coefficient over water
! and land tiles and  temperature at 1.5 m over sea and sea-ice,
! and tiles that are interpolated with Monin-Obukhov Similarity Theory
! (MOST hereafter).
! Units: epsilon_ssi, epsilon_surft [], t1p5m0_ssi [K]
REAL(KIND=real_jlslsm) ::                                                      &
        epsilon_ssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
REAL(KIND=real_jlslsm) ::                                                      &
        t1p5m0_ssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
REAL(KIND=real_jlslsm)                                                         &
     :: epsilon_surft(land_pts,nsurft), t1p5m0_surft(land_pts,nsurft)

! Temporarily used real variables.
REAL(KIND=real_jlslsm) :: term_numx,term_denx
! Minimum value limit for term_denx
REAL(KIND=real_jlslsm), PARAMETER :: term_denx_minlim = -1.0e-3


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)


!-----------------------------------------------------------------------
! Diagnose local and GBM humidities at 1.5 m if requested via SQ1P5
! and 10m if requested by l_q10m
!-----------------------------------------------------------------------
!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(i,j,l,n)                                                         &
!$OMP SHARED(land_pts,j_ind,i_ind,land_index,t_i_length,tdims,epsilon_ssi,     &
!$OMP       t1p5m0_ssi,nsurft,epsilon_surft,t1p5m0_surft)

! Initialise arrays specially for the calculation of specific humidity at 1.5 m
! in the air-surface-decoupled situation, and set their initial values.
!$OMP DO SCHEDULE(STATIC)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    epsilon_ssi(i,j) = 1.0
    t1p5m0_ssi(i,j) = 0.0
  END DO
END DO
!$OMP END DO NOWAIT

!$OMP DO SCHEDULE(STATIC) COLLAPSE(2)
DO n = 1,nsurft
  DO l = 1,land_pts
    epsilon_surft(l,n) = 1.0
    t1p5m0_surft(l,n) = 0.0
  END DO
END DO
!$OMP END DO NOWAIT

!$OMP DO SCHEDULE(STATIC)
DO l = 1,land_pts
  j_ind(l) = (land_index(l) - 1) / t_i_length + 1
  i_ind(l) = land_index(l) - (j_ind(l) - 1) * t_i_length
END DO
!$OMP END DO
!$OMP END PARALLEL

IF (sf_diag%sq1p5 .OR. (IScrnTDiag == IP_ScrnDecpl2) .OR.                      &
                       (IScrnTDiag == IP_ScrnDecpl3) .OR.                      &
                        sf_diag%l_q10m .OR. sf_diag%l_t10m ) THEN

    ! Pass tile number into qsat_mix, since its sea/sea ice there is no tile

  IF (l_mr_physics) THEN
    CALL qsat_mix(qs,tstar_ssi,pstar,t_i_length,t_j_length)
  ELSE
    CALL qsat(qs,tstar_ssi,pstar,t_i_length,t_j_length)
  END IF

END IF

IF (sf_diag%l_q10m .OR. sf_diag%l_t10m) THEN
  ! calculate 10m q over sea/sea-ice only
  ! also required if 10m t requested due to cloud scheme call in diag_bl

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(j,i)                                                             &
!$OMP SHARED(tdims,flandg,sf_diag,qw_1,qs)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      IF (flandg(i,j) <  1.0 ) THEN
        sf_diag%q10m(i,j) = qw_1(i,j) +                                        &
             (sf_diag%chr10m(i,j) - 1.0) * ( qw_1(i,j) - qs(i,j) )
      END IF
    END DO
  END DO
!$OMP END PARALLEL DO
END IF

IF (sf_diag%sq1p5 .OR. (IScrnTDiag == IP_ScrnDecpl2)                           &
                  .OR. (IScrnTDiag == IP_ScrnDecpl3) ) THEN

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(j,i,cer1p5m,l,n,qs_surft,k)                                      &
!$OMP SHARED(tdims,sf_diag,flandg,chr1p5m_sice,qw_1,qs,                        &
!$OMP        i_ind,j_ind,pstar,nsurft,land_pts,tstar_surft,pstar_land,         &
!$OMP        l_mr_physics,surft_pts,surft_index,resft,chr1p5m,tile_frac)

!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      sf_diag%q1p5m(i,j) = 0.0
      sf_diag%q1p5m_ssi(i,j) = 0.0
      IF (flandg(i,j) <  1.0 ) THEN
        cer1p5m = chr1p5m_sice(i,j) - 1.0
        sf_diag%q1p5m_ssi(i,j) =                                               &
          (qw_1(i,j) + cer1p5m * ( qw_1(i,j) - qs(i,j) ))
        sf_diag%q1p5m(i,j) = (1.0 - flandg(i,j)) * sf_diag%q1p5m_ssi(i,j)
      END IF
    END DO
  END DO
!$OMP END DO NOWAIT

!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    i = i_ind(l)
    j = j_ind(l)
    pstar_land(l) = pstar(i,j)
  END DO
!$OMP END DO

  !This loop scoping is not ideal. nsurft is typically 9 so limits OMP
  !scalability. Needs qsat replacing with the new version

!$OMP DO SCHEDULE(STATIC)
  DO n = 1,nsurft
    DO l = 1,land_pts
      sf_diag%q1p5m_surft(l,n) = 0.0
    END DO

    IF (l_mr_physics) THEN
      CALL qsat_mix(qs_surft,tstar_surft(:,n),pstar_land,land_pts)
    ELSE
      CALL qsat(qs_surft,tstar_surft(:,n),pstar_land,land_pts)
    END IF

    DO k = 1,surft_pts(n)
      l = surft_index(k,n)
      i = i_ind(l)
      j = j_ind(l)
      cer1p5m = resft(l,n) * (chr1p5m(l,n) - 1.0)
      sf_diag%q1p5m_surft(l,n) = qw_1(i,j) +                                   &
                                 cer1p5m * ( qw_1(i,j) - qs_surft(l) )
    END DO
  END DO
!$OMP END DO

  DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
    DO k = 1,surft_pts(n)
      l = surft_index(k,n)
      i = i_ind(l)
      j = j_ind(l)
      sf_diag%q1p5m(i,j) = sf_diag%q1p5m(i,j)                                  &
        + flandg(i,j) * tile_frac(l,n) * sf_diag%q1p5m_surft(l,n)
    END DO
!$OMP END DO
  END DO

!$OMP END PARALLEL
END IF

!-----------------------------------------------------------------------
! Diagnose local and GBM temperatures at 1.5 m if requested via ST1P5
! and 10m if requested by l_t10m
! The transitional diagnostic requires that this be done on all
! timesteps as the decoupled screen temperature is prognostic.
!-----------------------------------------------------------------------
IF (sf_diag%l_t10m .OR. sf_diag%l_q10m) THEN
  ! calculate 10m temp over sea/sea-ice only
  ! also required if 10m q requested due to cloud scheme call in diag_bl

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(j,i)                                                             &
!$OMP SHARED(tdims,flandg,sf_diag,tstar_ssi,grcp,tl_1,z1,z0mssi,z0hssi)
  DO j = tdims%j_start, tdims%j_end
    DO i = tdims%i_start, tdims%i_end
      IF (flandg(i,j) <  1.0 ) THEN
        ! using z_obs_wind for 10m level rather than z_obs_tq
        sf_diag%t10m(i,j) = tstar_ssi(i,j) - grcp * z_obs_wind +               &
             sf_diag%chr10m(i,j) * (tl_1(i,j) - tstar_ssi(i,j) +               &
             grcp * (z1(i,j) + z0mssi(i,j) - z0hssi(i,j)))
      END IF
    END DO
  END DO
!$OMP END PARALLEL DO
END IF

IF (sf_diag%st1p5 .OR. (IScrnTDiag == IP_ScrnDecpl2)                           &
                .OR. (IScrnTDiag == IP_ScrnDecpl3) ) THEN

  !       Calculate the screen-level temperature using the standard
  !       interpolation. If using the decoupled diagnosis with
  !       transitional effects (IScrnTDiag=IP_ScrnDecpl2 or
  !       IScrnTDiag=IP_ScrnDecpl3), this will subsequently be adjusted.

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE( n, l, k, i, j )                                                 &
!$OMP SHARED(tdims,sf_diag,flandg,tstar_ssi,chr1p5m_sice,                      &
!$OMP        tl_1,z1,z0mssi,z0hssi,nsurft,land_pts,surft_pts,surft_index,      &
!$OMP        i_ind,j_ind,tstar_surft,chr1p5m,z0m_surft,z0h_surft,tile_frac,    &
!$OMP        grcp,t1p5m0_ssi,t1p5m0_surft,epsilon_ssi,epsilon_surft)
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start, tdims%j_end
    DO i = tdims%i_start, tdims%i_end
      sf_diag%t1p5m_ssi(i,j) = 0.0
      sf_diag%t1p5m(i,j) = 0.0

      IF (flandg(i,j) <  1.0 ) THEN
        !             Calculate the screen-level temperature.
        sf_diag%t1p5m_ssi(i,j) = tstar_ssi(i,j) - grcp * z_obs_tq +            &
          chr1p5m_sice(i,j) * (tl_1(i,j) - tstar_ssi(i,j) +                    &
          grcp * (z1(i,j) + z0mssi(i,j) - z0hssi(i,j)))
        t1p5m0_ssi(i,j) = sf_diag%t1p5m_ssi(i,j)
        ! store temperature at 1.5 m over sea and sea-ice
        ! with one derived following MOST.
        sf_diag%t1p5m(i,j) = (1.0 - flandg(i,j)) * sf_diag%t1p5m_ssi(i,j)
      END IF

    END DO
  END DO
!$OMP END DO NOWAIT

  DO n = 1, nsurft
!$OMP DO SCHEDULE(STATIC)
    DO l = 1,land_pts
      sf_diag%t1p5m_surft(l,n) = 0.0
    END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
    DO k = 1, surft_pts(n)
      l = surft_index(k,n)
      i = i_ind(l)
      j = j_ind(l)
      sf_diag%t1p5m_surft(l,n) = tstar_surft(l,n) - grcp * z_obs_tq +          &
        chr1p5m(l,n) * ( tl_1(i,j) - tstar_surft(l,n) +                        &
        grcp * (z1(i,j) + z0m_surft(l,n) - z0h_surft(l,n)) )
      t1p5m0_surft(l,n) = sf_diag%t1p5m_surft(l,n)
      ! store temperature at 1.5 m over land tiles
      ! with one derived following MOST.
    END DO
!$OMP END DO
  END DO

  DO n = 1, nsurft
!$OMP DO SCHEDULE(STATIC)
    DO k = 1, surft_pts(n)
      l = surft_index(k,n)
      i = i_ind(l)
      j = j_ind(l)
      sf_diag%t1p5m(i,j) = sf_diag%t1p5m(i,j)                                  &
        + flandg(i,j) * tile_frac(l,n) * sf_diag%t1p5m_surft(l,n)
    END DO
!$OMP END DO
  END DO

!$OMP END PARALLEL

  IF ((IScrnTDiag == IP_ScrnDecpl2) .OR. (IScrnTDiag == IP_ScrnDecpl3)) THEN

        !         Allocate and set arrays specific to this diagnostic
    ALLOCATE(LDclDiag(t_i_length,t_j_length))
    ALLOCATE(TStarGBM(t_i_length,t_j_length))

    !         Determine the grid-box mean surface temperatures to check for
    !         changing near-surface stability.

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(SHARED)                                                          &
!$OMP PRIVATE( i, j, k, l, n, WaterPath, QScrn, CO2Path, CO2Scrn, lp, tx, ks,  &
!$OMP           exks, TRadCool, tRotational_Inv, ThDot, LTrans2,               &
!$OMP           WeightDcl, qs_surft, term_numx, term_denx)

!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start, tdims%j_end
      DO i = tdims%i_start, tdims%i_end
        IF ( flandg(i,j) < 1.0 ) THEN
          TStarGBM(i,j) = (1.0 - flandg(i,j)) * tstar_ssi(i,j)
        ELSE
          TStarGBM(i,j) = 0.0
        END IF
      END DO
    END DO
!$OMP END DO

    DO n = 1, nsurft
!$OMP DO SCHEDULE(STATIC)
      DO k = 1, surft_pts(n)
        l = surft_index(k,n)
        i = i_ind(l)
        j = j_ind(l)
        TStarGBM(i,j) = TStarGBM(i,j) + flandg(i,j) *                          &
          tile_frac(l, n) * tstar_surft(l,n)
      END DO
!$OMP END DO
    END DO

!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start, tdims%j_end
      DO i = tdims%i_start, tdims%i_end
        ! The following test for missing data is designed to allow
        ! for last-bit differences.
        IF (ABS(tStbTrans(i,j) - rmdi) < 0.01 * ABS(rmdi)) THEN
          ! The point must have been unstable on the last
          ! time-step. If it remains unstable no action is required
          ! and the flag is set to .FALSE.. If it has just become
          ! stable the decoupled diagnosis must be initialized from
          ! the standard diagnostic. The test for stability used
          ! here is deliberately simple and neglects moisture.
          LDclDiag(i,j) =.FALSE.
          IF (TStarGBM(i,j) < tl_1(i,j) + grcp * z1(i,j))                      &
            tStbTrans(i,j) = 0.0
        ELSE
          ! The point was stable last time: check for a transition
          ! to unstable stratification.
          IF (TStarGBM(i,j) >= tl_1(i,j) + grcp * z1(i,j)) THEN
            tStbTrans(i,j) = rmdi
            LDclDiag(i,j) = .FALSE.
          ELSE
            LDclDiag(i,j) = .TRUE.
            tStbTrans(i,j) = tStbTrans(i,j) + timestep
          END IF
        END IF

        ! The 1.5 m temperature will be recalculated where decoupled
        ! diagnosis is applied: to start the process, it is zeroed at
        ! points marked with LDclDiag.
        IF (LDclDiag(i,j)) sf_diag%t1p5m(i,j) = 0.0

        ! Initialize the decoupled temperatures: separate blocks
        ! are used here because indexing over tiles requires
        ! special treatment. The foregoing code requires that
        ! initialization must take place where the time since the
        ! transition is positive and the logical for the full
        ! decoupled diagnosis is false.
        IF ( (tStbTrans(i,j) >= 0.0) .AND. (flandg(i,j) < 1.0) .AND.           &
             ( .NOT. LDclDiag(i,j)) ) TScrnDcl_SSI(i,j) = sf_diag%t1p5m_ssi(i,j)
      END DO
    END DO
!$OMP END DO

    DO n = 1, nsurft
!$OMP DO SCHEDULE(STATIC)
      DO k = 1, surft_pts(n)
        l = surft_index(k,n)
        i = i_ind(l)
        j = j_ind(l)
        IF ( (tStbTrans(i,j) >= 0) .AND. ( .NOT. LDclDiag(i,j)) )              &
          TScrnDcl_SURFT(l,n) = sf_diag%t1p5m_surft(l,n)
      END DO
!$OMP END DO
    END DO

    !         Calculate the radiative temperatures at valid points.
    !         First sea and sea-ice:

!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start, tdims%j_end
      DO i = tdims%i_start, tdims%i_end

        IF ( (LDclDiag(i,j)) .AND. (flandg(i,j) < 1.0) ) THEN

          !               Calculate the integrated paths of water vapour and
          !               CO2 between the surface and the screen level.
          !               We assume that the mixing ratio varies linearly in
          !               calculating the water path. The cooling time will not
          !               be greatly sensitive to this.

          WaterPath = rho1(i,j) * z_obs_tq *                                   &
            (qs(i,j) + 0.5 * (qw_1(i,j) - qs(i,j)) *                           &
            (z_obs_tq / z1(i,j)))
          QScrn = sf_diag%q1p5m_ssi(i,j)
          IF (l_co2_interactive) THEN
            CO2Path = rho1(i,j) * co2_3d(i,j) * z_obs_tq
            CO2Scrn = co2_3d(i,j)
          ELSE
            CO2Path = rho1(i,j) * co2_mmr * z_obs_tq
            CO2Scrn = co2_mmr
          END IF

          !               Contribution of cooling to the surface due to water
          !               vapour.
          IF (WaterPath < 0.04) THEN
            lp  = LOG(MAX(1.0e-05, WaterPath))
            lp  = EXP( cc_h(0) + lp * ( cc_h(1) + lp *                         &
                  ( cc_h(2) + lp * ( cc_h(3) + lp * ( cc_h(4) +                &
                  lp * cc_h(5) ) ) ) ) )
          ELSE
            lp = cc_h_large / WaterPath
          END IF
          tx  = (tstar_ssi(i,j) - t0_rfit) / dt_rfit
          ks  = QScrn * lp * (1.0 + t_scale_h2o_rfit * tx) *                   &
                (pl0_h2o_rfit + pl1_h2o_rfit * tx)

          !               Contribution of cooling to the surface due to carbon
          !               dioxide.
          lp  = LOG(CO2Path)
          lp  = cc_c(0) + lp * ( cc_c(1) + lp * ( cc_c(2) + lp *               &
                ( cc_c(3) + lp * ( cc_c(4) + lp * cc_c(5) ) ) ) )
          tx  = (tstar_ssi(i,j) - t0_rfit) / dt_rfit
          ks  = ks + CO2Scrn * EXP(lp) * (1.0 - t_scale_co2_rfit * tx) *       &
                (pl0_co2_rfit - pl1_co2_rfit * tx)

          !               Limit on ks in case range of it is exceeded.
          ks  = MAX(4.0 * sbcon * tstar_ssi(i,j)**3 * ks  / cp,                &
                  EPSILON(ks))
          !               ks now holds K such that dT/dt = -K(T - T_surf)

          !               Calculate the impact of radiative cooling through the
          !               current timestep on the decoupled screen temperature.
          exks = EXP( - ks * timestep )
          TRadCool = tstar_ssi(i,j) +                                          &
            (TScrnDcl_SSI(i,j) - tstar_ssi_old(i,j)) * exks -                  &
            (tstar_ssi(i,j) - tstar_ssi_old(i,j)) * (1.0 - exks) /             &
            (ks * timestep )


          !               Calculate the adjustment of the radiatively cooled
          !               decoupled temperature towards that implied by standard
          !               similarity theory.

          tRotational_Inv = 0.4 * ABS(f3_at_p(i,j))

          ThDot = (tstar_ssi(i,j) - tstar_ssi_old(i,j)) / timestep
          LTrans2 = ( uStarGBM(i,j)**3 /                                       &
            ( (g/tstar_ssi(i,j)) * (1.0e-08 + ABS(ThDot) ) ) ) *               &
            ( LOG(1.0 + z_obs_tq / z0mssi(i,j)) /                              &
              LOG(1.0 + z_obs_tq / 0.05) )**2

          WeightDcl = EXP( - timestep * tStbTrans(i,j) *                       &
            tRotational_Inv**2 ) * 1.0 /                                       &
            (1.0 + MIN(0.2 * uStarGBM(i,j) * timestep / z_obs_tq,              &
                       LTrans2 / z_obs_tq**2) * (timestep / 300.0))

          !               Interpolate between the value obtained from surface
          !               similarity theory and the radiative temperature,
          !               provided that the radiative temperature is higher
          !               than the M-O value.
          sf_diag%t1p5m_ssi(i,j) = sf_diag%t1p5m_ssi(i,j) + WeightDcl *        &
            MAX(0.0, (TRadCool - sf_diag%t1p5m_ssi(i,j)))
          sf_diag%t1p5m(i,j) = (1.0 - flandg(i,j)) * sf_diag%t1p5m_ssi(i,j)
          !               Set the decoupled temperature to the newly diagnosed
          !               screen temperature.
          TScrnDcl_SSI(i,j) = sf_diag%t1p5m_ssi(i,j)


          !              Calculate the value for air-sea-decoupled screen height
          !              temperature over sea/sea-ice.
          term_numx = tstar_ssi(i,j) -                                         &
            (sf_diag%t1p5m_ssi(i,j) + grcp * (z_obs_tq + z0mssi(i,j) - z0hssi(i,j)))
          term_denx = tstar_ssi(i,j) -                                         &
            (t1p5m0_ssi(i,j) + grcp * (z_obs_tq + z0mssi(i,j) - z0hssi(i,j)))
          !            if((term_denx.ne.0.).and.(z_obs_tq.lt.z1(i,j))) then
          !            a loose restriction condition, not for use.
          IF ((term_denx <  term_denx_minlim) .AND. (z_obs_tq <  z1(i,j))) THEN
            epsilon_ssi(i,j) = term_numx / term_denx
          ELSE
            epsilon_ssi(i,j) = 1.0
          END IF

        END IF
      END DO
    END DO
!$OMP END DO NOWAIT

    !         PSTAR_LAND will previously have been calculated if the
    !         humidity diagnostic has been requested.
    IF ( .NOT. sf_diag%sq1p5) THEN
!$OMP DO SCHEDULE(STATIC)
      DO l = 1, land_pts
        i = i_ind(l)
        j = j_ind(l)
        pstar_land(l) = pstar(i,j)
      END DO
!$OMP END DO
    END IF

    !This loop scoping is not ideal. nsurft is typically 9 so limits OMP
    !scalability. Needs qsat replacing with the new version

!$OMP DO SCHEDULE(STATIC)
    DO n = 1, nsurft

      IF (l_mr_physics) THEN
        CALL qsat_mix(qs_surft,tstar_surft(:,n),pstar_land,land_pts)
      ELSE
        CALL qsat(qs_surft,tstar_surft(:,n),pstar_land,land_pts)
      END IF

      !           Advance the radiative temperature on tiles.
      DO k = 1, surft_pts(n)

        l = surft_index(k,n)
        i = i_ind(l)
        j = j_ind(l)

        IF (LDclDiag(i,j)) THEN

          ! Calculate the integrated paths of water vapour and CO2
          ! between the surface and the screen level. We assume that
          ! the mixing ratio varies linearly in calculating the
          ! water path. The cooling time will not be greatly
          ! sensitive to this.

          WaterPath = rho1(i,j) * z_obs_tq *                                   &
            (qs_surft(l) + 0.5 * (qw_1(i,j) - qs_surft(l)) *                   &
            (z_obs_tq / z1(i,j)))
          QScrn = sf_diag%q1p5m_surft(l,n)
          IF (l_co2_interactive) THEN
            CO2Path = rho1(i,j) * co2_3d(i,j) * z_obs_tq
            CO2Scrn = co2_3d(i,j)
          ELSE
            CO2Path = rho1(i,j) * co2_mmr * z_obs_tq
            CO2Scrn = co2_mmr
          END IF

          !               Contribution of cooling to the surface due to water
          !               vapour.
          IF (WaterPath < 0.04) THEN
            lp  = LOG(MAX(1.0e-05, WaterPath))
            lp  = EXP( cc_h(0) + lp * ( cc_h(1) + lp *                         &
                  ( cc_h(2) + lp * ( cc_h(3) + lp * ( cc_h(4) +                &
                  lp * cc_h(5) ) ) ) ) )
          ELSE
            lp = cc_h_large / WaterPath
          END IF
          tx    = (tstar_surft(l,n) - t0_rfit) / dt_rfit
          ks    = QScrn * lp * (1.0 + t_scale_h2o_rfit * tx) *                 &
                  (pl0_h2o_rfit + pl1_h2o_rfit * tx)

          !               Contribution of cooling to the surface due to carbon
          !               dioxide.
          lp    = LOG(CO2Path)
          lp    = cc_c(0) + lp * ( cc_c(1) + lp * ( cc_c(2) +                  &
                  lp * ( cc_c(3) + lp * ( cc_c(4) + lp * cc_c(5)               &
                  ) ) ) )
          tx    = (tstar_surft(l,n) - t0_rfit) / dt_rfit
          ks    = ks + CO2Scrn * EXP(lp) *                                     &
                  (1.0 - t_scale_co2_rfit * tx) *                              &
                  (pl0_co2_rfit - pl1_co2_rfit * tx)

          ks    = MAX(4.0 * sbcon * tstar_surft(l,n)**3 * ks  / cp,            &
                    EPSILON(ks))
          !               ks now holds K such that dT/dt = -K(T - T_surf)

          !               Integrate forward in time across the timestep.
          exks = EXP( - ks * timestep )

          !               Advance the decoupled temperature radiatively.
          TRadCool = tstar_surft(l,n) +                                        &
            (TScrnDcl_SURFT(l,n) - tstar_surft_old(l,n)) * exks -              &
            (tstar_surft(l,n) - tstar_surft_old(l,n)) *                        &
            (1.0 - exks) / (ks * timestep )

          !               Calculate the weighting between the M-O and decoupled
          !               values.

          tRotational_Inv = 0.4 * ABS(f3_at_p(i,j))

          ! Only the GBM value of uStar is used here:
          ! the use of different values of uStar on different
          ! tiles would involve significant extra complexity, while
          ! being unlikely to provide much extra benefit.

          ThDot = (tstar_surft(l,n) - tstar_surft_old(l,n)) /                  &
                     timestep
          LTrans2 = ( uStarGBM(i,j)**3 /                                       &
            ( (g/tstar_surft(l,n)) * (1.0e-08 + ABS(ThDot) ) ) ) *             &
            ( LOG(1.0 + z_obs_tq / z0m_surft(l,n)) /                           &
              LOG(1.0 + z_obs_tq / 0.05) )**2

          WeightDcl = EXP( - timestep * tStbTrans(i,j) *                       &
            tRotational_Inv**2) * 1.0 /                                        &
            (1.0 + MIN(0.2 * uStarGBM(i,j) * timestep / z_obs_tq,              &
                       LTrans2 / z_obs_tq**2) * (timestep / 300.0))

          ! Interpolate between the value obtained from surface
          ! similarity theory and the radiative temperature, provided
          ! that the radiative temperature is above the M-O value.
          sf_diag%t1p5m_surft(l,n) = sf_diag%t1p5m_surft(l,n) + WeightDcl *    &
            MAX(0.0, (TRadCool - sf_diag%t1p5m_surft(l,n)))
          ! Reset the decoupled temperature to the newly
          ! diagnosed one.
          TScrnDcl_SURFT(l,n) = sf_diag%t1p5m_surft(l,n)

          ! Calculate the value for air-surface-decoupled screen height
          ! temperature over land tiles.
          term_numx = tstar_surft(l,n) -                                       &
                      (sf_diag%t1p5m_surft(l,n) +                              &
                      grcp * (z_obs_tq + z0m_surft(l,n) - z0h_surft(l,n)))
          term_denx = tstar_surft(l,n) -                                       &
                      (t1p5m0_surft(l,n) +                                     &
                      grcp * (z_obs_tq + z0m_surft(l,n) - z0h_surft(l,n)))
          !             if((term_denx.ne.0.).and.(z_obs_tq.lt.z1(i,j))) then
          !             a loose restriction condition, not for use.
          IF ((term_denx <  term_denx_minlim) .AND. (z_obs_tq <  z1(i,j))) THEN
            epsilon_surft(l,n) = term_numx / term_denx
          ELSE
            epsilon_surft(l,n)  = 1.0
          END IF

        END IF

      END DO
    END DO
!$OMP END DO

    DO n = 1, nsurft
!$OMP DO SCHEDULE(STATIC)
      DO k = 1, surft_pts(n)

        l = surft_index(k,n)
        i = i_ind(l)
        j = j_ind(l)

        IF (LDclDiag(i,j)) THEN
          sf_diag%t1p5m(i,j) = sf_diag%t1p5m(i,j) + flandg(i,j) *              &
            tile_frac(l,n) * sf_diag%t1p5m_surft(l,n)
        END IF

      END DO
!$OMP END DO
    END DO

!$OMP END PARALLEL

    !         Release space no longer required.
    DEALLOCATE(LDclDiag)
    DEALLOCATE(TStarGBM)

    !         re-calculate screen-level humidity with the amended exchange
    !         coefficient.
    IF (IScrnTDiag == IP_ScrnDecpl3) THEN

      ! Pass tile number into qsat_mix, since its sea/sea ice there is no tile

      IF (l_mr_physics) THEN
        CALL qsat_mix(qs,tstar_ssi,pstar,t_i_length,t_j_length)
      ELSE
        CALL qsat(qs,tstar_ssi,pstar,t_i_length,t_j_length)
      END IF

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP SHARED( nsurft, land_pts, sf_diag, tstar_surft, pstar_land, pstar,       &
!$OMP         l_mr_physics, surft_pts, surft_index, i_ind, j_ind, resft,       &
!$OMP         chr1p5m, qw_1, flandg, tile_frac, epsilon_surft,                 &
!$OMP         tdims, epsilon_ssi, chr1p5m_sice, qs)                            &
!$OMP PRIVATE( i, j, k, l, n, cer1p5m, qs_surft)
!$OMP DO SCHEDULE(STATIC)
      DO j = tdims%j_start,tdims%j_end
        DO i = tdims%i_start,tdims%i_end
          sf_diag%q1p5m(i,j) = 0.0
          sf_diag%q1p5m_ssi(i,j) = 0.0
          IF (flandg(i,j) <  1.0 ) THEN
            !           epsilon_ssi(i,j) = max(0.,                            &
            !           min(1./chr1p5m_sice(i,j),epsilon_ssi(i,j)))
            !           a loose restriction condition, not for use.
            epsilon_ssi(i,j) = MAX(1.0,MIN(1.0 / chr1p5m_sice(i,j),epsilon_ssi(i,j)))
            cer1p5m =  epsilon_ssi(i,j) * chr1p5m_sice(i,j) - 1.0
            sf_diag%q1p5m_ssi(i,j) = (qw_1(i,j) + cer1p5m * ( qw_1(i,j) - qs(i,j) ))
            sf_diag%q1p5m(i,j) = (1.0 - flandg(i,j)) * sf_diag%q1p5m_ssi(i,j)
          END IF
        END DO
      END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
      DO l = 1,land_pts
        i = i_ind(l)
        j = j_ind(l)
        pstar_land(l) = pstar(i,j)
      END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
      DO n = 1,nsurft
        DO l = 1,land_pts
          sf_diag%q1p5m_surft(l,n) = 0.0
        END DO

        IF (l_mr_physics) THEN
          CALL qsat_mix(qs_surft,tstar_surft(:,n),pstar_land,land_pts)
        ELSE
          CALL qsat(qs_surft,tstar_surft(:,n),pstar_land,land_pts)
        END IF

        DO k = 1,surft_pts(n)
          l = surft_index(k,n)
          i = i_ind(l)
          j = j_ind(l)
          !         epsilon_surft(l,n) = max(-((1.-resft(l,n))/resft(l,n))*(1./chr1p5m(l,n)),&
          !         min(1./chr1p5m(l,n),epsilon_surft(l,n)))
          !         a loose restriction condition, not for use.
          epsilon_surft(l,n) = MAX(1.0,MIN(1.0 / chr1p5m(l,n),epsilon_surft(l,n)))
          cer1p5m = resft(l,n) * (epsilon_surft(l,n) * chr1p5m(l,n) - 1.0)
          sf_diag%q1p5m_surft(l,n) = qw_1(i,j) + cer1p5m * ( qw_1(i,j) - qs_surft(l) )
        END DO
      END DO
!$OMP END DO

      DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
        DO k = 1,surft_pts(n)
          l = surft_index(k,n)
          i = i_ind(l)
          j = j_ind(l)
          sf_diag%q1p5m(i,j) = sf_diag%q1p5m(i,j)                              &
               + flandg(i,j) * tile_frac(l,n) * sf_diag%q1p5m_surft(l,n)
        END DO
!$OMP END DO
      END DO

!$OMP END PARALLEL
    END IF

  END IF

END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE screen_tq
END MODULE screen_tq_mod
