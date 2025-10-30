! ****************************rCOPYRIGHT*******************************

! (c) [University of Edinburgh]. All rights reserved.
! This routine has been licensed to the Met Office for use and
! distribution under the JULES collaboration agreement, subject
! to the terms and conditions set out therein.
! [Met Office Ref SC237]

! *****************************COPYRIGHT*******************************
!  SUBROUTINE RELAYERSNOW-----------------------------------------------

! Description:
!     Redivide snowpack after changes in depth, conserving mass and energy

! Subroutine Interface:
MODULE relayersnow_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='RELAYERSNOW_MOD'

CONTAINS

SUBROUTINE relayersnow( land_pts, surft_pts, n_wtrac_jls, surft_index,         &
                        rgrain0, rho0, sice0, snowfall, snowmass,              &
                        tsnow0, sice0_wtrac, nsnow, ds, rgrain, rgrainl, sice, &
                        rho_snow_grnd, sliq, tsnow, sice_wtrac, sliq_wtrac,    &
                        rho_snow, snowdepth )

USE layersnow_mod, ONLY: layersnow

USE ereport_mod, ONLY: ereport

USE water_constants_mod, ONLY:                                                 &
  ! imported scalar parameters
  hcapi,                                                                       &
    ! Specific heat capacity of ice (J/kg/K).
  hcapw,                                                                       &
    ! Specific heat capacity of water (J/kg/K).
  tm
    ! Temperature at which fresh water freezes and ice melts (K).

USE jules_snow_mod, ONLY:                                                      &
  ! imported scalars (IN)
  nsmax,                                                                       &
    ! Maximum possible number of snow layers.
  rho_snow_fresh,                                                              &
    ! Density of fresh snow (kg per m**3).
  r0,                                                                          &
    ! Grain size for fresh snow (microns).
  l_rho_snow_corr,                                                             &
    ! Switch for snowpack density correction.
  i_relayer_opt, ip_relayer_linear, ip_relayer_rgrain_inv
    ! Option for relayering the snow pack and permitted values

USE jules_radiation_mod, ONLY: l_snow_albedo, l_embedded_snow

USE jules_water_tracers_mod, ONLY: l_wtrac_jls

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Total number of land points.
  surft_pts,                                                                   &
    ! Number of tile points.
  n_wtrac_jls
    ! Number of water tracers

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  surft_index(land_pts)   ! Index of tile points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  rgrain0(land_pts),                                                           &
    ! Fresh snow grain size (microns).
  rho0(land_pts),                                                              &
    ! Density of fresh snow (kg/m3).
    ! Where nsnow=0, rho0 is the density of the snowpack.
  sice0(land_pts),                                                             &
     ! Ice content of fresh snow (kg/m2).
     ! Where nsnow=0, sice0 is the mass of the snowpack.
  snowfall(land_pts),                                                          &
     ! Total frozen precip reaching the ground (kg/m2).
  snowmass(land_pts),                                                          &
     ! Snow mass on the ground (kg/m2).
  tsnow0(land_pts),                                                            &
     ! Temperature of fresh snow (K).
  sice0_wtrac(land_pts,n_wtrac_jls)
     ! Water tracer ice content of fresh snow (kg/m2).
     ! Where nsnow=0, sice0 is the mass of the snowpack.

!-----------------------------------------------------------------------------
! Array arguments with intent(inout)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN OUT) ::                                                     &
 nsnow(land_pts)       ! Number of snow layers.

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  ds(land_pts,nsmax),                                                          &
    ! Snow layer thicknesses (m).
  rgrain(land_pts),                                                            &
    ! Snow surface grain size (microns).
  rgrainl(land_pts,nsmax),                                                     &
    ! Snow grain size (microns).
  rho_snow_grnd(land_pts),                                                     &
    ! Snowpack bulk density (kg/m3).
  sice(land_pts,nsmax),                                                        &
    ! Ice content of snow layers (kg/m2).
  sliq(land_pts,nsmax),                                                        &
    ! Liquid content of snow layers (kg/m2).
  tsnow(land_pts,nsmax),                                                       &
    ! Snow layer temperatures (K).
  sice_wtrac(land_pts,nsmax,n_wtrac_jls),                                      &
    ! Water tracer ice content of snow layers (kg/m2).
  sliq_wtrac(land_pts,nsmax,n_wtrac_jls)
    ! Water tracer liquid content of snow layers (kg/m2).

!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 rho_snow(land_pts,nsmax),                                                     &
    ! Snow layer densities (kg/m3).
  snowdepth(land_pts)
    ! Snow depth (m).

!-----------------------------------------------------------------------------
! Local scalar parameters
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), PARAMETER :: thin_snow_limit = 1.0e-6
    ! Maximum snow thickness (m) that is neglected during relayering.
    ! All contributions (mass, energy etc) from that snow are neglected.

!-----------------------------------------------------------------------------
! Local scalars
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i,                                                                           &
    ! Land point index.
  iznew,                                                                       &
    ! layer index.
  izz,                                                                         &
    ! layer index.
  k,                                                                           &
    ! Tile point index.
  n,                                                                           &
    ! Snow layer index.
  new,                                                                         &
    ! Layer index.
  old,                                                                         &
    ! Layer index.
  i_wt
    ! Water tracer index.

REAL(KIND=real_jlslsm) ::                                                      &
  csnow,                                                                       &
    ! Areal heat capacity of layer (J/K/m2).
  oldremains,                                                                  &
    ! Remaining depth in an old layer (m).
  wt
    ! Weight given to a layer value.

!-----------------------------------------------------------------------------
! Local arrays
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  nold(land_pts)        ! Number of layers before adjustment.

REAL(KIND=real_jlslsm) ::                                                      &
  d0(land_pts,0:nsmax),                                                        &
    ! Layer thicknesses before adjustment (m).
    ! d0(:,0) represents new snow if nsnow>0, otherwise it is all snow.
  e(0:nsmax),                                                                  &
    ! Internal energy before adjustment (J/m2).
  newremains(nsmax),                                                           &
    ! Available (unfilled) depth in new layer (m).
  r(0:nsmax),                                                                  &
    ! Grain size before adjustment (kg/m2).
  s(0:nsmax),                                                                  &
    ! Ice content before adjustment (kg/m2).
  w(0:nsmax),                                                                  &
    ! Liquid content before adjustment (kg/m2).
  u(nsmax)
    ! Layer energy contents.

REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
  s_wtrac(:,:),                                                                &
    ! Water tracer ice content before adjustment (kg/m2).
  w_wtrac(:,:)
    ! Water tracer liquid content before adjustment (kg/m2).


INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='RELAYERSNOW'

INTEGER              :: errcode            ! Error code
CHARACTER(LEN=80) :: ERRMSG             ! Error message

!-----------------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Allocate water tracer working arrays
IF (l_wtrac_jls) THEN
  ALLOCATE(s_wtrac(0:nsmax,n_wtrac_jls))
  ALLOCATE(w_wtrac(0:nsmax,n_wtrac_jls))
ELSE
  ALLOCATE(s_wtrac(1,1))
  ALLOCATE(w_wtrac(1,1))
END IF

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(n,i,k)                                                           &
!$OMP SHARED(l_snow_albedo,l_embedded_snow,land_pts,rgrain,nsmax,rgrainl,      &
!$OMP        snowdepth,rho_snow,d0,nold,surft_pts,surft_index,sice0,rho0,ds,   &
!$OMP        r0,nsnow)

!-----------------------------------------------------------------------------
! Initialise grain variables if they aren't used.
!-----------------------------------------------------------------------------
IF ( .NOT. (l_snow_albedo .OR. l_embedded_snow)) THEN
!$OMP DO SCHEDULE(STATIC)
  DO i = 1, land_pts
    rgrain(i)    = r0
  END DO
!$OMP END DO

  DO n = 1,nsmax
!$OMP DO SCHEDULE(STATIC)
    DO i = 1, land_pts
      rgrainl(i,n) = r0
    END DO
!$OMP END DO
  END DO
END IF

!-----------------------------------------------------------------------------
! Initialise snowdepth with value that is retained where tile frac=0.
!-----------------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
DO i = 1, land_pts
  snowdepth(i)  = 0.0
END DO
!$OMP END DO

DO n = 1,nsmax
!$OMP DO SCHEDULE(STATIC)
  DO i = 1, land_pts
    rho_snow(i,n) = 0.0
  END DO
!$OMP END DO
END DO

!-----------------------------------------------------------------------------
! Store previous layer thicknesses.
!-----------------------------------------------------------------------------

!$OMP DO SCHEDULE(STATIC)
DO i = 1, land_pts
  d0(i,0) = 0.0
  nold(i) = nsnow(i)
END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
DO k = 1,surft_pts
  i = surft_index(k)
  IF ( sice0(i) > 0.0 ) THEN
    d0(i,0) = sice0(i) / rho0(i)
  END IF

  DO n = 1,nsnow(i)
    d0(i,n) = ds(i,n)
  END DO
END DO
!$OMP END DO

!-----------------------------------------------------------------------------
! Calculate snowdepth
!-----------------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
DO k = 1,surft_pts
  i = surft_index(k)
  snowdepth(i) = d0(i,0)
  DO n = 1,nsnow(i)
    snowdepth(i) = snowdepth(i) + d0(i,n)
  END DO
END DO
!$OMP END DO

!$OMP END PARALLEL

!-----------------------------------------------------------------------------
! Divide snowpack into new layers
!-----------------------------------------------------------------------------
CALL layersnow( land_pts, surft_pts, surft_index, snowdepth, nsnow, ds )

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(SHARED)                                                          &
!$OMP PRIVATE(i,iznew,izz,k,n,new,old,csnow,oldremains,wt,e,newremains,        &
!$OMP         r,s,w,u,errcode,errmsg,i_wt,s_wtrac,w_wtrac)
DO k = 1,surft_pts
  i = surft_index(k)

  IF ( nsnow(i) > 0 ) THEN
    !-------------------------------------------------------------------------
    ! Store previous snow layer energy and mass contents
    !-------------------------------------------------------------------------
    csnow = sice0(i) * hcapi
    e(0)  = csnow * ( tsnow0(i) - tm )
    r(0)  = rgrain0(i)
    ! If the previous timestep had no layers (but could still have snow),
    ! estimate R(0) using average of previous grain size and that of
    ! fresh snow. In fact fresh snow would be on top and so more important!
    IF ( nold(i) == 0 ) THEN
      SELECT CASE (i_relayer_opt)
      CASE (ip_relayer_linear)
        r(0) = ( 1.0 - snowfall(i) / snowmass(i)) * rgrain(i) +                &
                 snowfall(i) / snowmass(i) * rgrain0(i)
      CASE (ip_relayer_rgrain_inv)
        r(0) = 1.0 / ( ( 1.0 - snowfall(i) / snowmass(i)) / rgrain(i) +        &
                             (snowfall(i) / snowmass(i)) / rgrain0(i) )
      CASE DEFAULT
        errcode = 675
        ERRMSG  = 'Unrecognised option for relayering the snowpack.'
        CALL ereport(routinename, errcode, ERRMSG)
      END SELECT
    END IF
    s(0) = sice0(i)
    w(0) = 0.0
    DO n = 1,nold(i)
      csnow = sice(i,n) * hcapi + sliq(i,n) * hcapw
      e(n)  = csnow * ( tsnow(i,n) - tm )
      r(n)  = rgrainl(i,n)
      s(n)  = sice(i,n)
      w(n)  = sliq(i,n)
    END DO

    IF (l_wtrac_jls) THEN
      ! Repeat for water tracers
      DO i_wt = 1, n_wtrac_jls
        s_wtrac(0,i_wt) = sice0_wtrac(i,i_wt)
        w_wtrac(0,i_wt) = 0.0

        DO n = 1,nold(i)
          s_wtrac(n,i_wt) = sice_wtrac(i,n,i_wt)
          w_wtrac(n,i_wt) = sliq_wtrac(i,n,i_wt)
        END DO
      END DO
    END IF ! l_wtrac_jls

    !-------------------------------------------------------------------------
    ! Initialise accumulations for new layer values.
    !-------------------------------------------------------------------------
    DO n = 1, nsmax
      u(n)         = 0.0
      sice(i,n)    = 0.0
      sliq(i,n)    = 0.0
      rgrainl(i,n) = 0.0
    END DO

    IF (l_wtrac_jls) THEN
      ! Repeat for water tracers
      DO i_wt = 1,n_wtrac_jls
        DO n = 1, nsmax
          sice_wtrac(i,n,i_wt) = 0.0
          sliq_wtrac(i,n,i_wt) = 0.0
        END DO
      END DO
    END IF     ! l_wtrac_jls

    !-------------------------------------------------------------------------
    ! Set the state of the new layers.
    !-------------------------------------------------------------------------
    ! Initialise with all new layers empty.
    DO n = 1, nsnow(i)
      newremains(n) = ds(i,n)
    END DO

    ! Start by filling top new layer.
    iznew = 1

    ! Loop over the old layers.
    DO old = 0,nold(i)

      ! All of this old layer remains to be reassigned to new layer(s).
      oldremains = d0(i,old)

      ! Point to first new layer with remaining space.
      izz = iznew

      ! Loop over new layers with remaining space.
      DO new = izz,nsnow(i)

        IF ( oldremains > newremains(new) ) THEN
          !-------------------------------------------------------------------
          ! The remaining depth in the new layer will be exhausted by some or
          ! all of the remaining depth from the old layer.
          !-------------------------------------------------------------------

          ! Decrement old layer by the remaining space in new layer.
          oldremains = oldremains - newremains(new)

          ! Add properties from old layer to accumulation for new layer.
          ! Note that wt is <= 1 since here we have oldRemains>newRemains,
          ! and oldRemains <= d0.
          IF ( d0(i,old) > thin_snow_limit ) THEN
            wt          = newremains(new) / d0(i,old)
            u(new)      = u(new) + e(old) * wt
            sice(i,new) = sice(i,new) + s(old) * wt
            sliq(i,new) = sliq(i,new) + w(old) * wt

            IF (l_wtrac_jls) THEN
              ! Repeat for water tracers
              DO i_wt = 1,n_wtrac_jls
                sice_wtrac(i,new,i_wt) = sice_wtrac(i,new,i_wt)                &
                                         + s_wtrac(old,i_wt) * wt
                sliq_wtrac(i,new,i_wt) = sliq_wtrac(i,new,i_wt)                &
                                         + w_wtrac(old,i_wt) * wt
              END DO
            END IF     ! l_wtrac_jls

            SELECT CASE (i_relayer_opt)
            CASE (ip_relayer_linear)
              rgrainl(i,new) = rgrainl(i,new) +                                &
                               r(old) * newremains(new)
            CASE (ip_relayer_rgrain_inv)
              rgrainl(i,new) = rgrainl(i,new) +                                &
                               newremains(new) / r(old)
            CASE DEFAULT
              errcode = 675
              ERRMSG  = 'Unrecognised option for relayering the snowpack.'
              CALL ereport(routinename, errcode, ERRMSG)
            END SELECT
          END IF

          ! Update the pointer to the next new layer with space.
          izz = new + 1

        ELSE

          !-------------------------------------------------------------------
          ! The old layer will be exhausted by this increment.
          !-------------------------------------------------------------------
          ! Decrement available space in the new layer.
          newremains(new) = newremains(new) - oldremains
          ! Add properties from old layer to accumulation for new layer.
          IF ( d0(i,old) > thin_snow_limit ) THEN
            wt          = oldremains /  d0(i,old)
            u(new)      = u(new) + e(old) * wt
            sice(i,new) = sice(i,new) + s(old) * wt
            sliq(i,new) = sliq(i,new) + w(old) * wt

            IF (l_wtrac_jls) THEN
              ! Repeat for water tracers
              DO i_wt = 1,n_wtrac_jls
                sice_wtrac(i,new,i_wt) = sice_wtrac(i,new,i_wt)                &
                                         + s_wtrac(old,i_wt) * wt
                sliq_wtrac(i,new,i_wt) = sliq_wtrac(i,new,i_wt)                &
                                         + w_wtrac(old,i_wt) * wt
              END DO
            END IF     ! l_wtrac_jls

            SELECT CASE (i_relayer_opt)
            CASE (ip_relayer_linear)
              rgrainl(i,new) = rgrainl(i,new) + r(old) * oldremains
            CASE (ip_relayer_rgrain_inv)
              rgrainl(i,new) = rgrainl(i,new) + oldremains / r(old)
            CASE DEFAULT
              errcode = 675
              ERRMSG  = 'Unrecognised option for relayering the snowpack.'
              CALL ereport(routinename, errcode, ERRMSG)
            END SELECT
          END IF
          ! Proceed to the next old layer by exiting from the new layer loop.
          EXIT
        END IF
      END DO  !  new layers
      ! Update pointer to the next new layer with space.
      iznew = izz
    END DO  !  old layers

    !-------------------------------------------------------------------------
    ! Diagnose layer temperatures and densities.
    !-------------------------------------------------------------------------
    DO n = 1,nsnow(i)
      csnow         = sice(i,n) * hcapi + sliq(i,n) * hcapw
      tsnow(i,n)    = tm + u(n) / csnow
      rho_snow(i,n) = ( sice(i,n) + sliq(i,n) ) / ds(i,n)
      SELECT CASE (i_relayer_opt)
      CASE (ip_relayer_linear)
        rgrainl(i,n) = rgrainl(i,n) / ds(i,n)
      CASE (ip_relayer_rgrain_inv)
        rgrainl(i,n) = ds(i,n) / rgrainl(i,n)
      CASE DEFAULT
        errcode = 675
        ERRMSG  = 'Unrecognised option for relayering the snowpack.'
        CALL ereport(routinename, errcode, ERRMSG)
      END SELECT
    END DO

    !-------------------------------------------------------------------------
    ! Snow surface grain size for radiative calculations
    !-------------------------------------------------------------------------
    rgrain(i) = rgrainl(i,1)

    !-------------------------------------------------------------------------
    ! Diagnose bulk density of pack.
    !-------------------------------------------------------------------------
    rho_snow_grnd(i) = snowmass(i) / snowdepth(i)

  ELSE

    !-------------------------------------------------------------------------
    ! Set bulk density of pack to a constant value if there is (effectively)
    ! no snow. This density is then used the next time a shallow pack forms.
    ! We could also recalculate snowdepth, to make it consistent
    ! with the revised density, but we're not bothering as depths are tiny!
    ! Note that because the bulk density is not calculated for nsnow=0 and
    ! snowmass>0, it remains constant (until the pack is exhausted or it
    ! grows to nsnow=1).
    !-------------------------------------------------------------------------
    IF ( snowmass(i) < 1.0e-9 ) THEN
      rho_snow_grnd(i) = rho_snow_fresh
    ELSE IF ( l_rho_snow_corr ) THEN
      !-----------------------------------------------------------------------
      ! Diagnose bulk density of pack. On most occasions this density is
      ! unchanged from input value, but the calculation is required for
      ! timesteps when nsnow changes to zero.
      !-----------------------------------------------------------------------
      rho_snow_grnd(i) = snowmass(i) / snowdepth(i)
    END IF

  END IF   !  nsnow

  !---------------------------------------------------------------------------
  ! Set values for unused snow layers.
  ! Note: not needed for algorithm, but clearer to follow.
  !---------------------------------------------------------------------------
  IF (  nsnow(i) < nsmax ) THEN
    DO n = nsnow(i) + 1, nsmax
      rgrainl(i,n)   = r0
      rho_snow(i,n)  = 0.0
      sice(i,n)      = 0.0
      sliq(i,n)      = 0.0
      tsnow(i,n)     = tm
    END DO

    IF (l_wtrac_jls) THEN
      ! Repeat for water tracers
      DO i_wt = 1,n_wtrac_jls
        DO n = nsnow(i) + 1, nsmax
          sice_wtrac(i,n,i_wt)      = 0.0
          sliq_wtrac(i,n,i_wt)      = 0.0
        END DO
      END DO
    END IF     ! l_wtrac_jls
  END IF

END DO  !  K (points)
!$OMP END PARALLEL DO

! Deallocate water tracer working arrays
DEALLOCATE(w_wtrac)
DEALLOCATE(s_wtrac)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE relayersnow
END MODULE relayersnow_mod
