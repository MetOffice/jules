! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    SUBROUTINE SOIL_HTC------------------------------------------------------

! Description:
!     Updates deep soil temperatures, frozen and unfrozen
!     frozen soil water content.

! Documentation : UM Documentation Paper 25

MODULE soil_htc_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SOIL_HTC_MOD'

CONTAINS

SUBROUTINE soil_htc ( npnts, nshyd, nsurft, soil_pts, timestep, soil_index,    &
                      surft_pts, surft_index, nsnow, bexp, dz, tile_frac,      &
                      non_lake_frac, hcap, hcon,                               &
                      sathh, smcl, snowdepth,                                  &
                      surf_ht_flux, v_sat, w_flux,                             &
                      sthf, sthu, sthu_irr, tsoil, tsoil_deep_gb )

!Use in relevant subroutines
USE bedrock_mod,  ONLY: bedrock
USE gauss_mod,    ONLY: gauss
USE heat_con_mod, ONLY: heat_con

!Use in relevant variables
USE conversions_mod,        ONLY: zerodegc
USE water_constants_mod,    ONLY: dpsidt, hcapi, hcapw, lf, rho_water
USE jules_snow_mod,         ONLY: snow_hcon
USE jules_soil_mod,         ONLY: facur, gamma_t, mmax, tacur, l_bedrock,      &
                                  ns_deep
USE jules_surface_mod,      ONLY: l_flake_model
USE jules_surface_types_mod, ONLY: lake
USE jules_irrig_mod,   ONLY: l_irrig_dmd

USE parkind1,               ONLY: jprb, jpim
USE yomhook,                ONLY: lhook, dr_hook

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
  nsurft,                                                                      &
    ! Number of tiles.
  soil_pts
    ! Number of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  timestep
    ! Model timestep (s).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  soil_index(npnts),                                                           &
    ! Array of soil points.
  surft_pts(nsurft),                                                           &
    ! Number of tile points.
  surft_index(npnts,nsurft),                                                   &
    ! Index of tile points.
  nsnow(npnts,nsurft)
    ! Number of snow layerss

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  bexp(npnts,nshyd),                                                           &
    ! Brooks & Corey exponent.
  dz(nshyd),                                                                   &
    ! Thicknesses of the soil layers (m).
  tile_frac(npnts,nsurft),                                                     &
    ! Tile fractions.
  non_lake_frac(npnts),                                                        &
    ! Sum of non-FLake tile fractions used for weighted average of sifact.
  hcap(npnts,nshyd),                                                           &
    ! Soil heat capacity (J/K/m3).
  hcon(npnts,0:nshyd),                                                         &
    ! Soil thermal conductivity (W/m/K).
  sathh(npnts,nshyd),                                                          &
    ! Saturated soil water pressure (m).
  smcl(npnts,nshyd),                                                           &
    ! Soil moisture content of each layer (kg/m2).
  snowdepth(npnts,nsurft),                                                     &
    ! Snow depth (on ground) (m)
  surf_ht_flux(npnts),                                                         &
    ! Net downward surface heat flux (W/m2).
  v_sat(npnts,nshyd),                                                          &
    ! Volumetric soil moisture concentration at saturation (m3 H2O/m3 soil).
  w_flux(npnts,0:nshyd)
    ! The fluxes of water between layers (kg/m2/s).

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  sthf(npnts,nshyd),                                                           &
    ! Frozen soil moisture content of each layer as a fraction of saturation.
  sthu(npnts,nshyd),                                                           &
    ! Unfrozen soil moisture content of each layer as a fraction of saturation.
  sthu_irr(npnts,nshyd),                                                       &
    ! Unfrozen soil wetness over irrigation
  tsoil(npnts,nshyd),                                                          &
    ! Sub-surface temperatures (K).
  tsoil_deep_gb(npnts,ns_deep)
    ! Deep soil temperature (K).

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
INTEGER, PARAMETER :: j_block = 4 ! this specifies the block length
                                  ! which enables parallelisation over
                                  ! the j dimension. Must be of sufficient
                                  ! granularity to avoid inner loop overhead.

LOGICAL, PARAMETER :: use_lims_gauss = .TRUE.
    ! Whether to apply the dsthumin and dsthumax limits in the Gauss solver.

!-----------------------------------------------------------------------------
! Local scalar variables:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, j, jj, m, n,                                                              &
    ! Loop counters.
  iter_pts
    ! Number of soil points which require iteration.

REAL(KIND=real_jlslsm) ::                                                      &
  gamcon,                                                                      &
    ! Forward timestep weighting constant.
  si_surft,                                                                    &
    ! Tile snow insulation factor
  small_value,                                                                 &
  tiny_0

!-----------------------------------------------------------------------------
! Local array variables:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
 iter_index(npnts)
    ! Array of soil points which require iteration.

REAL(KIND=real_jlslsm) ::                                                      &
  ceacur(npnts),                                                               &
    ! Flux conservation accuracy of the calculation (W/m2)
  dhsl0(npnts,nshyd),                                                          &
    ! Total heat increment to the layer (J/m2/timestep)
  dhsl(npnts,nshyd),                                                           &
    ! The heat available to update the layer temperature (J/m2/timestep)
  dsmclf(npnts,nshyd),                                                         &
    ! The increment to the layer frozen soil moisture (kg/m2/timestep).
  dthu(npnts,nshyd),                                                           &
    ! Rate of change of volumetric unfrozen soil moisture concentration with
    ! temperature (m3 liquid H2O/m3 soil/K)
  dtsl(npnts,nshyd),                                                           &
    ! The increment to the layer temperatur (K/timestep).
  dtslmax(npnts,nshyd),                                                        &
    ! Maximum value of DTSL (K/timestep).
  dtslmin(npnts,nshyd),                                                        &
    ! Minimum value of DTSL (K/timestep).
  hcapt(npnts,nshyd),                                                          &
    ! The total volumetric heat capacity (soil+water) of the layer (J/m3/K).
  hc(npnts,nshyd),                                                             &
    ! The thermal conductivity of each layer (W/m/K).
  hcons(npnts),                                                                &
    ! The thermal conductivity between adjacent soil layers (W/m/K).
  h_flux(npnts,0:nshyd),                                                       &
    ! The fluxes of heat between layers (W/m2).
  hadv(npnts,nshyd),                                                           &
    ! Heat flux due to moisture advection (W/m2).
  sifact(npnts),                                                               &
    ! Snow insulation factor.
  smclf(npnts,nshyd),                                                          &
    ! Frozen moisture content of each soil layer (kg/m2).
  smclf0(npnts,nshyd),                                                         &
    ! Previous value of SMCLF (kg/m2).
  smclsat(npnts,nshyd),                                                        &
    ! The saturation moisture content of each layer (kg/m2).
  smclu(npnts,nshyd),                                                          &
    ! Unfrozen moisture content of each soil layer (kg/m2).
  smclu0(npnts,nshyd),                                                         &
    ! Previous value of SMCLU (kg/m2).
  smcfu(npnts),                                                                &
    ! Fractional saturation (unfrozen water at layer boundaries.
  smcff(npnts),                                                                &
    ! Fractional saturation (frozen water) at layer boundaries.
  tmax(npnts,nshyd),                                                           &
    ! Temperature above which all water is unfrozen (Celsius)
  tsl(npnts,0:nshyd),                                                          &
    ! Soil layer temperatures (Celsius)
    !       TSL(0) temperature of incoming water.
    !       TSL(1:NSHYD) sub-surface soil temperatures .
  tsl0(npnts,0:nshyd),                                                         &
    ! Previous value of TSL (Celsius).
  v_satk(npnts),                                                               &
    ! Saturated volumetric soil moisture at the layer boundary (m3/m3)
  work1(npnts,nshyd),                                                          &
    ! for Tmax
  hflux_base(npnts),                                                           &
    ! heat flux out of bottom soil layer.
  sthu_b(npnts,nshyd)
    ! STHU at start of routine

! Variables required for the implicit calculation.
REAL(KIND=real_jlslsm) ::                                                      &
  dhflux_dtsl1(npnts,0:nshyd),dhflux_dtsl2(npnts,0:nshyd),                     &
  dhadv_dtsl0(npnts,nshyd),dhadv_dtsl1(npnts,nshyd),                           &
  dhadv_dtsl2(npnts,nshyd),                                                    &
    ! Rate of change of the explicit fluxes with the layer temperatures
    ! (W/m2/K).
  a(npnts,nshyd),b(npnts,nshyd),c(npnts,nshyd),d(npnts,nshyd)
    ! Matrix elements.

LOGICAL ::                                                                     &
  iter(npnts)
    ! .T. on points requiring iterations.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SOIL_HTC'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Initialisations
!-----------------------------------------------------------------------------
tiny_0 = TINY(0.0)
small_value = EPSILON(0.0)

DO j = 1,soil_pts
  i = soil_index(j)
  tsl(i,0) = tsoil(i,1) - zerodegc
END DO

DO n = 1,nshyd
  DO j = 1,soil_pts
    i = soil_index(j)
    !-------------------------------------------------------------------------
    ! Define soil layer temperatures TSL (in celsius).
    !-------------------------------------------------------------------------
    tsl(i,n) = tsoil(i,n) - zerodegc
  END DO
END DO

DO n = 1,nshyd
  ! CDIR$ IVDEP here would force vectorization but changes results!
  DO j = 1,soil_pts
    i = soil_index(j)

    IF (l_irrig_dmd) THEN
      sthu_b(i,n) = sthu(i,n)
    END IF
    !-------------------------------------------------------------------------
    ! Diagnose the frozen and unfrozen water.
    !-------------------------------------------------------------------------

    smclsat(i,n) = rho_water * dz(n) * v_sat(i,n)
    smclf(i,n) = smclsat(i,n) * sthf(i,n)
    smclu(i,n) = smcl(i,n) - smclf(i,n)
  END DO                                  !  J=1,SOIL_PTS
END DO                                    ! N=1,NSHYD

!-----------------------------------------------------------------------------
! Initialise the array of points for which calculations are required.
!-----------------------------------------------------------------------------
DO i = 1,npnts
  iter(i)=.FALSE.
END DO

DO j = 1,soil_pts
  i = soil_index(j)
  iter(i)=.TRUE.
END DO

!-----------------------------------------------------------------------------
! Calculate the heat conductivity between adjacent layers
!-----------------------------------------------------------------------------

v_satk(:)=0.0

DO n = 1,nshyd-1
  DO j = 1,soil_pts
    i = soil_index(j)
    smcfu(i)=(dz(n+1) * sthu(i,n) + dz(n) * sthu(i,n+1)) / (dz(n+1) + dz(n))
    smcff(i)=(dz(n+1) * sthf(i,n) + dz(n) * sthf(i,n+1)) / (dz(n+1) + dz(n))
    ! THIS LINE IS REPLACED WITH THE ONE FOLLOWING
    ! IN ORDER TO OBTAIN BIT-COMPARABILITY
    !          V_SATK(I)=(DZ(N+1)*V_SAT(I,N)+DZ(N)*V_SAT(I,N+1))              &
    !     &              /(DZ(N+1)+DZ(N))
    v_satk(i) = v_sat(i,n)
  END DO

  CALL heat_con (npnts, hcon(:,n), smcfu(:), smcff(:), v_satk(:), hcons(:))

  DO j = 1,soil_pts
    i = soil_index(j)
    hc(i,n) = hcons(i)
  END DO

END DO

!-----------------------------------------------------------------------------
! Calculate the snow insulation factor.
!-----------------------------------------------------------------------------
DO i = 1,npnts
  sifact(i) = 0.0
END DO

!$OMP PARALLEL  DEFAULT(NONE) PRIVATE(i, j, si_surft, n)                       &
!$OMP SHARED(nsurft, surft_pts, surft_index, v_sat, nsnow, snowdepth, dz,      &
!$OMP        hc, snow_hcon, sifact, tile_frac, l_flake_model, lake)
DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
  DO j = 1,surft_pts(n)
    i = surft_index(j,n)
    si_surft = 1.0

    IF ( v_sat(i,1) > EPSILON(v_sat(i,1)) .AND.                                &
         nsnow(i,n) == 0 ) THEN
      IF ( snowdepth(i,n) <= 0.5 * dz(1) ) THEN
        si_surft = 1.0 / ( 1.0 + 2.0 * snowdepth(i,n) / (dz(1) + dz(2)) )
      ELSE
        si_surft =(dz(1) + dz(2)) /                                            &
                  ( hc(i,1) * (2.0 * snowdepth(i,n) - dz(1)) / snow_hcon       &
                   + 2.0 * dz(1) + dz(2) )
      END IF
    END IF

    IF ( .NOT. ((l_flake_model) .AND. (n == lake)) ) THEN
      sifact(i) = sifact(i) + tile_frac(i,n) * si_surft
    END IF
  END DO
!$OMP END DO
  ! implicit barrier to ensure lockstep on n
END DO

!$OMP END PARALLEL

IF (l_flake_model) THEN
  DO i = 1,npnts
    IF (non_lake_frac(i) > EPSILON(0.0)) THEN
      sifact(i) = sifact(i) / non_lake_frac(i)
    END IF
  END DO
END IF

!-----------------------------------------------------------------------------
! Calculate heat fluxes across layer boundaries
!-----------------------------------------------------------------------------
! First calculate the heat flux at the bottom of the soil column
IF (l_bedrock) THEN
  CALL heat_con (npnts, hcon(:,nshyd), sthu(:,nshyd), sthf(:,nshyd),           &
                 v_sat(:,nshyd), hc(:,nshyd))

  CALL bedrock (npnts, soil_pts, dz(nshyd), timestep, soil_index,              &
                tsl(:,nshyd), hc(:,nshyd), tsoil_deep_gb, hflux_base)
ELSE
  hflux_base(:) = 0.0
END IF

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(j, i, n)                                  &
!$OMP SHARED(soil_pts, nshyd, soil_index, h_flux, hc, tsl, dz, dhflux_dtsl1,   &
!$OMP        dhflux_dtsl2, hflux_base, surf_ht_flux, sifact, hadv,             &
!$OMP        w_flux, dhadv_dtsl0, dhadv_dtsl1, dhadv_dtsl2, v_sat,             &
!$OMP        tiny_0, smcl, small_value, work1, smclsat, bexp, tmax, sathh,     &
!$OMP        dhsl0, timestep, dhsl)

! Blocking is needed as the nshyd dimension has a spatial
! dependence (n +/-1). Blocking enables parallelising over the
! j (soil points) dimension.
!$OMP DO SCHEDULE(STATIC)
DO j = 1, soil_pts
  DO n = 1,nshyd-1
    i = soil_index(j)
    h_flux(i,n) = -hc(i,n) * 2.0 * (tsl(i,n+1) - tsl(i,n)) / (dz(n+1) + dz(n))
    dhflux_dtsl1(i,n) =  hc(i,n) * 2.0 / (dz(n+1) + dz(n))
    dhflux_dtsl2(i,n) = -hc(i,n) * 2.0 / (dz(n+1) + dz(n))
  END DO
END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
!DIR$ IVDEP
!CDIR NODEP
DO j = 1,soil_pts
  i = soil_index(j)
  h_flux(i,nshyd)       = hflux_base(i)
  h_flux(i,0)           = surf_ht_flux(i)
  h_flux(i,1)           = sifact(i) * h_flux(i,1)
  dhflux_dtsl1(i,nshyd) = 0.0
  dhflux_dtsl2(i,nshyd) = 0.0
  dhflux_dtsl1(i,0)     = 0.0
  dhflux_dtsl2(i,0)     = 0.0
  dhflux_dtsl1(i,1)     = sifact(i) * dhflux_dtsl1(i,1)
  dhflux_dtsl2(i,1)     = sifact(i) * dhflux_dtsl2(i,1)
END DO
!$OMP END DO

!-----------------------------------------------------------------------------
! Calculate the advection of heat by moisture fluxes
!-----------------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
DO j = 1, soil_pts
  DO n = 2,nshyd-1
    i = soil_index(j)
    hadv(i,n) = hcapw * dz(n) *                                                &
                (w_flux(i,n-1) * (tsl(i,n-1) - tsl(i,n)) / (dz(n) + dz(n-1))   &
                 + w_flux(i,n) * (tsl(i,n) - tsl(i,n+1)) / (dz(n) + dz(n+1)))
    dhadv_dtsl0(i,n) = hcapw * dz(n) * w_flux(i,n-1) / (dz(n) + dz(n-1))
    dhadv_dtsl1(i,n) = hcapw * dz(n) *                                         &
                      (-w_flux(i,n-1) / (dz(n) + dz(n-1))                      &
                       + w_flux(i,n) / (dz(n) + dz(n+1)))
    dhadv_dtsl2(i,n) = -hcapw * dz(n) * w_flux(i,n) / (dz(n) + dz(n+1))
  END DO
END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
!DIR$ IVDEP
!CDIR NODEP
DO j = 1,soil_pts
  i = soil_index(j)
  hadv(i,1)        = hcapw * dz(1) *                                           &
                     (w_flux(i,0) * (tsl(i,0) - tsl(i,1)) / dz(1)              &
                     + w_flux(i,1) * (tsl(i,1) - tsl(i,2)) / (dz(1) + dz(2)))
  dhadv_dtsl0(i,1) = 0.0
  dhadv_dtsl1(i,1) = hcapw * dz(1) *                                           &
                     (-w_flux(i,0) / dz(1) + w_flux(i,1) / (dz(1) + dz(2)))
  dhadv_dtsl2(i,1) = -hcapw * dz(1) * w_flux(i,1) / (dz(1) + dz(2))
  hadv(i,nshyd)    = hcapw * dz(nshyd) *                                       &
                      w_flux(i,nshyd-1) * (tsl(i,nshyd-1) - tsl(i,nshyd))      &
                      / (dz(nshyd) + dz(nshyd-1))
  dhadv_dtsl0(i,nshyd) = hcapw * dz(nshyd) * w_flux(i,nshyd-1)                 &
                         / (dz(nshyd) + dz(nshyd-1))
  dhadv_dtsl1(i,nshyd) = -hcapw * dz(nshyd) * w_flux(i,nshyd-1)                &
                         / (dz(nshyd) + dz(nshyd-1))
  dhadv_dtsl2(i,nshyd) = 0.0
END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
DO j = 1, soil_pts
  DO n = 1,nshyd
    i = soil_index(j)
    !-------------------------------------------------------------------------
    ! Calculate TMAX, the temperature above which all soil water is
    ! unfrozen.
    !-------------------------------------------------------------------------
    IF ( (v_sat(i,n) > tiny_0) .AND. (smcl(i,n) > small_value)) THEN
      work1(i,n) = (smcl(i,n) / smclsat(i,n))**(bexp(i,n))
      IF ( work1(i,n) > small_value ) THEN
        tmax(i,n) = -sathh(i,n) / (dpsidt * work1(i,n))
        tmax(i,n) = MAX(tmax(i,n),-zerodegc)
      ELSE
        tmax(i,n) = -zerodegc
      END IF
    ELSE
      tmax(i,n) = -zerodegc
    END IF

    dhsl0(i,n) = timestep * (h_flux(i,n-1) - h_flux(i,n) + hadv(i,n))
    dhsl(i,n)  = dhsl0(i,n)

  END DO  !  n (layers)
END DO  !  j (points)
!$OMP END DO

!$OMP END PARALLEL
!-----------------------------------------------------------------------------
! Iteration loop
!-----------------------------------------------------------------------------
DO m = 1,mmax

  !---------------------------------------------------------------------------
  ! Define the array of points which fail to meet the flux criterion.
  !---------------------------------------------------------------------------

  iter_pts = 0

  DO j = 1,soil_pts
    i = soil_index(j)

    IF (iter(i)) THEN

      iter_pts = iter_pts + 1
      iter_index(iter_pts) = i

    END IF
    iter(i) = .FALSE.

  END DO

  IF ( iter_pts == 0 ) EXIT

  !---------------------------------------------------------------------------
  ! Update calculations at these points.
  !---------------------------------------------------------------------------

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(j, i, gamcon, n)                          &
!$OMP SHARED (iter_pts, nshyd, iter_index, tsl0, tsl, smclf0, smclf, smclu0,   &
!$OMP         smclu, dtslmax, dtslmin, tmax, dthu, dhsl, smclsat, bexp, sathh, &
!$OMP         dz, hcapt, hcap, timestep, a, b, c, d, dhflux_dtsl1, smcl,       &
!$OMP         dhadv_dtsl0, dhflux_dtsl2, dhadv_dtsl1, npnts,dhadv_dtsl2, iter)

!$OMP DO SCHEDULE(STATIC)
  DO j = 1, iter_pts
    DO n = 1,nshyd

      !CDIR NODEP
      i = iter_index(j)

      tsl0(i,n)   = tsl(i,n)
      smclf0(i,n) = smclf(i,n)
      smclu0(i,n) = smclu(i,n)

      dtslmax(i,n) = 1.0e4 - tsl(i,n)
      dtslmin(i,n) = -zerodegc - tsl(i,n)

      IF (tsl(i,n) >  tmax(i,n)) THEN         ! All water unfrozen
        dthu(i,n)    = 0.0
        dtslmin(i,n) = tmax(i,n) - tsl(i,n)
      ELSE IF (tsl(i,n) == tmax(i,n) .AND.                                     &
               ! Remains unfrozen
               dhsl(i,n) >= 0.0) THEN
        dthu(i,n) = 0.0
      ELSE     ! Phase changes
        dthu(i,n)   = dpsidt * smclsat(i,n)                                    &
                      / (bexp(i,n) * sathh(i,n) * rho_water * dz(n))           &
                      * (-dpsidt * tsl(i,n) / sathh(i,n))**                    &
                      (-1.0 / bexp(i,n) - 1.0)
        dtslmax(i,n) = tmax(i,n) - tsl(i,n)
      END IF

      hcapt(i,n) = hcap(i,n) + (hcapw - hcapi) * smclu(i,n) / dz(n)            &
                  + hcapi * smcl(i,n) / dz(n)                                  &
                  + rho_water * dthu(i,n) * ((hcapw - hcapi) * tsl(i,n) + lf)


      !-----------------------------------------------------------------------
      ! Calculate the matrix elements required for the implicit update.
      !-----------------------------------------------------------------------
      gamcon = gamma_t * timestep / (hcapt(i,n) * dz(n))
      a(i,n) = -gamcon * (dhflux_dtsl1(i,n-1) + dhadv_dtsl0(i,n))
      b(i,n) = 1.0 - gamcon * (dhflux_dtsl2(i,n-1) - dhflux_dtsl1(i,n)         &
               + dhadv_dtsl1(i,n))
      c(i,n) = gamcon * (dhflux_dtsl2(i,n) + dhadv_dtsl2(i,n))
      d(i,n) = 1.0 / (hcapt(i,n) * dz(n)) * dhsl(i,n)

    END DO
  END DO
!$OMP END DO

!$OMP END PARALLEL
  !---------------------------------------------------------------------------
  ! Solve the triadiagonal matrix equation.
  !---------------------------------------------------------------------------
  CALL gauss(nshyd, npnts, iter_pts, use_lims_gauss, iter_index, a, b, c, d,   &
             dtslmin, dtslmax, dtsl)


  !---------------------------------------------------------------------------
  ! Diagnose the implicit DHSL
  !---------------------------------------------------------------------------
!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,n)                                    &
!$OMP SHARED(iter_pts,nshyd,iter_index,dhsl,dz,hcapt,a,dtsl,b,c,tsl,tmax,      &
!$OMP dsmclf,smclf,smclu,smcl,sathh,smclsat,bexp,smclf0,hcap,smclu0,tsl0,      &
!$OMP ceacur,timestep,iter)
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, iter_pts
    DO n = 2,nshyd-1
      i = iter_index(j)
      dhsl(i,n) = dhsl(i,n) - dz(n) * hcapt(i,n) * (a(i,n) * dtsl(i,n-1)       &
                  + (b(i,n) - 1.0) * dtsl(i,n) + c(i,n) * dtsl(i,n+1))
    END DO
  END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
  DO j = 1,iter_pts
    i = iter_index(j)
    dhsl(i,1)     = dhsl(i,1) - dz(1) * hcapt(i,1) *                           &
                    ((b(i,1) - 1.0) * dtsl(i,1) + c(i,1) * dtsl(i,2))
    dhsl(i,nshyd) = dhsl(i,nshyd) - dz(nshyd) * hcapt(i,nshyd) *               &
                    (a(i,nshyd) * dtsl(i,nshyd-1)                              &
                     + (b(i,nshyd) - 1.0) * dtsl(i,nshyd))
  END DO
!$OMP END DO

  !---------------------------------------------------------------------------
  ! Update the layer temperatures
  !---------------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, iter_pts
    DO n = 1,nshyd
      !CDIR NODEP
      i = iter_index(j)

      tsl(i,n) = tsl(i,n) + dtsl(i,n)

      !-----------------------------------------------------------------------
      ! If the temperature increment is small and frozen water exists
      ! assume that the excess energy goes into phase change
      !-----------------------------------------------------------------------
      IF (ABS(dtsl(i,n)) <  tacur .AND. tsl(i,n) <= tmax(i,n)) THEN
        dsmclf(i,n) = -dhsl(i,n) / ((hcapw - hcapi) * tsl(i,n) + lf)
        dsmclf(i,n) = MAX(dsmclf(i,n),-smclf(i,n))
        dsmclf(i,n) = MIN(dsmclf(i,n),smclu(i,n))
        smclu(i,n)  = smclu(i,n) - dsmclf(i,n)
        smclf(i,n)  = smclf(i,n) + dsmclf(i,n)
      END IF

      !-----------------------------------------------------------------------
      ! Diagnose unfrozen and frozen water contents
      !-----------------------------------------------------------------------
      IF (tsl(i,n) >= tmax(i,n)) THEN
        smclu(i,n) = smcl(i,n)
        smclf(i,n) = 0.0
      ELSE
        smclu(i,n) = smclsat(i,n)                                              &
                     * (-dpsidt * tsl(i,n) / sathh(i,n))**(-1.0 / bexp(i,n))
        smclf(i,n) = smcl(i,n) - smclu(i,n)
      END IF

      !-----------------------------------------------------------------------
      ! Calculate the error in heat conservation
      !-----------------------------------------------------------------------
      dsmclf(i,n) = smclf(i,n) - smclf0(i,n)
      dhsl(i,n)   = dhsl(i,n) - (hcap(i,n) * dz(n) + hcapw * smclu0(i,n)       &
                    + hcapi * smclf0(i,n)) * dtsl(i,n)                         &
                    - dsmclf(i,n) * ((hcapi - hcapw) * tsl0(i,n) - lf)

      !-----------------------------------------------------------------------
      ! Calculate the error in flux conservation
      !-----------------------------------------------------------------------
      ceacur(i) = ABS(dhsl(i,n)) / timestep

      IF (ceacur(i)  >   facur) THEN
        iter(i) = .TRUE.
      END IF

    END DO
  END DO
!$OMP END DO

!$OMP END PARALLEL
  !---------------------------------------------------------------------------
  ! End of iteration loop
  !---------------------------------------------------------------------------
END DO

!-----------------------------------------------------------------------------
! Diagnose soil temperatures (K) and fractional values of unfrozen and
! frozen water.
!-----------------------------------------------------------------------------

!$OMP  PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                              &
!$OMP  PRIVATE(jj, n, j, i)                                                    &
!$OMP  SHARED(tsoil, soil_index, sthu, sthf, soil_pts, nshyd,                  &
!$OMP  tsl, smclu, smclsat, smclf, sthu_b, sthu_irr, l_irrig_dmd)
DO jj = 1, soil_pts, j_block
  DO n = 1,nshyd
    DO j = jj,MIN(jj + j_block - 1,soil_pts)
      i = soil_index(j)
      tsoil(i,n) = tsl(i,n) + zerodegc
      sthu(i,n)  = smclu(i,n) / smclsat(i,n)
      sthf(i,n)  = smclf(i,n) / smclsat(i,n)
      IF (l_irrig_dmd) THEN
        ! Diagnose sthu in irrigated fraction.
        ! Add increment in sthu to sthu_irr.
        sthu_irr(i,n) = sthu_irr(i,n) + sthu(i,n) - sthu_b(i,n)
        ! Total soil moisture fraction should not exceed 100% or become
        ! negative.
        IF ( sthu_irr(i,n) + sthf(i,n) > 1.0 ) THEN
          sthu_irr(i,n) = 1.0 - sthf(i,n)
        END IF
        sthu_irr(i,n) = MAX( sthu_irr(i,n), 0.0 )
      END IF
    END DO
  END DO
END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE soil_htc
END MODULE soil_htc_mod
