MODULE n_leach_mod

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='N_LEACH_MOD'

CONTAINS

SUBROUTINE n_leach(land_pts, asteps_since_triffid, dim_cslayer, timestep,      &
                   w_flux_soilt, qbase_l_soilt, sub_surf_roff, smcl_soilt,     &
                   smcl_old_soilt, n_inorg_avail_pft, n_inorg_soilt_lyrs,      &
                   n_leach_gb_acc, n_leach_soilt)

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
USE jules_hydrology_mod, ONLY: l_top
USE ancil_info, ONLY: nsoilt
USE jules_soil_mod, ONLY: sm_levels, dzsoil
USE jules_surface_types_mod, ONLY: npft
USE jules_soil_biogeochem_mod, ONLY: l_layeredc, sorp

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Number of gridpoints.
  asteps_since_triffid,                                                        &
    ! Number of timesteps since last TRIFFID call.
  dim_cslayer
    ! Soil carbon

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  timestep
    ! Model timestep (s).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  w_flux_soilt(land_pts,nsoilt,0:sm_levels),                                   &
    ! Fluxes of water between layers  (kg/m2/s)
  qbase_l_soilt(land_pts,nsoilt,sm_levels+1),                                  &
    ! Base flow from each level (kg/m2/s).
  sub_surf_roff(land_pts),                                                     &
    ! Sub-surface runoff (kg/m2/s).
  smcl_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Soil moisture content of each layer (kg/m2).
  smcl_old_soilt(land_pts,nsoilt,sm_levels)
    ! Soil moisture content of each layer at start of hydrol (kg/m2).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  n_inorg_avail_pft(land_pts,npft,dim_cslayer),                                &
    ! Available inorganic N for PFTs (kg N m-2).
  n_inorg_soilt_lyrs(land_pts,nsoilt,dim_cslayer),                             &
    ! Inorganic N pool on soil levels (kg N m-2).
  n_leach_gb_acc(land_pts)
    ! Accumulated leached nitrogen diagnostic on TRIFFID timesteps
    ! (kg m-2 (360days)-1).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  n_leach_soilt(land_pts,nsoilt)
    ! Leached N (kg m-2 s-1).

!-----------------------------------------------------------------------------
! Local variables:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i,                                                                           &
    ! Counter for land points.
  n,                                                                           &
    ! Counter for soil level.
  nn,                                                                          &
    ! Counter for pft.
  m,                                                                           &
    ! Counter for soil tile.
  n_1m
    ! Layer index for top 1m.

REAL(KIND=real_jlslsm) ::                                                      &
  n_inorg_gb_old(land_pts,sm_levels),                                          &
    ! Start value of inorganic nitrogen.
  n_inorg_avail_old(land_pts,npft,sm_levels),                                  &
    ! Start value of available inorganic nitrogen.
  w_flux_leach,                                                                &
    ! Intermediary in calc of leaching flux.
  tstep_flux_leach,                                                            &
    ! Intermediary in calc of leaching flux (w_flux_leach*timestep).
  max_leach_flux,                                                              &
    ! Max limit of leach flux so not to exceed the soluble fraction.
  ztop
    ! Incremental soil depth used for calc no layers in top 1 m.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='N_LEACH'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!----------------------------------------------------------------------

DO m = 1,nsoilt

  IF (m == 1 .AND. asteps_since_triffid == 1) THEN
    n_leach_gb_acc(:) = 0.0
  END IF

  DO i = 1,land_pts

    n_leach_soilt(i,m) = 0.0

    IF (l_layeredc) THEN
      ! Record old n_inorg, so we can calculate leaching.
      ! These do not need soilt dimension as they're only in scope within the
      ! soilt loop (and the land_pts loop so don't need land_pts either).
      DO n = 1,sm_levels
        n_inorg_gb_old(i,n)      = n_inorg_soilt_lyrs(i,m,n)
        n_inorg_avail_old(i,:,n) = n_inorg_avail_pft(i,:,n)
      END DO
      ! Max limit of flux so not to exceed the soluble fraction.
      max_leach_flux = 1.0 / sorp / timestep
      ! Explicitly move n_inorg_soilt through the layers with w_flux_soilt.
      DO n = 2,sm_levels
        IF (w_flux_soilt(i,m,n-1) >= 0.0 .AND.                                 &
            smcl_old_soilt(i,m,n-1) > EPSILON(0.0)) THEN
          w_flux_leach = w_flux_soilt(i,m,n-1) /                               &
                                             (sorp * smcl_old_soilt(i,m,n-1))
          tstep_flux_leach = MIN(w_flux_leach, max_leach_flux) * timestep
          n_inorg_soilt_lyrs(i,m,n-1) = n_inorg_soilt_lyrs(i,m,n-1) -          &
                                        tstep_flux_leach *                     &
                                        n_inorg_gb_old(i,n-1)
          n_inorg_soilt_lyrs(i,m,n) = n_inorg_soilt_lyrs(i,m,n) +              &
                                        tstep_flux_leach *                     &
                                        n_inorg_gb_old(i,n-1)
          IF (n_inorg_soilt_lyrs(i,m,n-1) < 0.0) THEN
            n_inorg_soilt_lyrs(i,m,n)   = n_inorg_soilt_lyrs(i,m,n) +          &
                                          n_inorg_soilt_lyrs(i,m,n-1)
            n_inorg_soilt_lyrs(i,m,n-1) = 0.0
          END IF

          n_inorg_avail_pft(i,:,n-1) = n_inorg_avail_pft(i,:,n-1) -            &
                                       tstep_flux_leach *                      &
                                       n_inorg_avail_old(i,:,n-1)
          n_inorg_avail_pft(i,:,n) = n_inorg_avail_pft(i,:,n) +                &
                                       tstep_flux_leach *                      &
                                       n_inorg_avail_old(i,:,n-1)
          DO nn = 1,npft
            IF (n_inorg_avail_pft(i,nn,n-1) < 0.0) THEN
              n_inorg_avail_pft(i,nn,n) = n_inorg_avail_pft(i,nn,n) +          &
                                          n_inorg_avail_pft(i,nn,n-1)
              n_inorg_avail_pft(i,nn,n-1) = 0.0
            END IF
          END DO
        ELSE IF (w_flux_soilt(i,m,n-1) < 0.0 .AND.                             &
                 smcl_old_soilt(i,m,n) > EPSILON(0.0)) THEN
          w_flux_leach = w_flux_soilt(i,m,n-1) /                               &
                                     (sorp * smcl_old_soilt(i,m,n))
          tstep_flux_leach = MAX(w_flux_leach, max_leach_flux * (-1.0) ) *     &
                             timestep
          n_inorg_soilt_lyrs(i,m,n) = n_inorg_soilt_lyrs(i,m,n) +              &
                                      tstep_flux_leach *                       &
                                      n_inorg_gb_old(i,n)
          n_inorg_soilt_lyrs(i,m,n-1) = n_inorg_soilt_lyrs(i,m,n-1) -          &
                                        tstep_flux_leach *                     &
                                        n_inorg_gb_old(i,n)
          IF (n_inorg_soilt_lyrs(i,m,n) < 0.0) THEN
            n_inorg_soilt_lyrs(i,m,n-1) = n_inorg_soilt_lyrs(i,m,n-1) +        &
                                          n_inorg_soilt_lyrs(i,m,n)
            n_inorg_soilt_lyrs(i,m,n)   = 0.0
          END IF
          n_inorg_avail_pft(i,:,n) = n_inorg_avail_pft(i,:,n) +                &
                                     tstep_flux_leach *                        &
                                     n_inorg_avail_old(i,:,n)
          n_inorg_avail_pft(i,:,n-1) = n_inorg_avail_pft(i,:,n-1) -            &
                                        tstep_flux_leach *                     &
                                     n_inorg_avail_old(i,:,n)
          DO nn = 1,npft
            IF (n_inorg_avail_pft(i,nn,n) < 0.0) THEN
              n_inorg_avail_pft(i,nn,n-1) = n_inorg_avail_pft(i,nn,n-1) +      &
                                            n_inorg_avail_pft(i,nn,n)
              n_inorg_avail_pft(i,nn,n) = 0.0
            END IF
          END DO
        END IF
      END DO ! sm_levels-1

      IF (w_flux_soilt(i,m,sm_levels) > 0.0 .AND.                              &
          smcl_old_soilt(i,m,sm_levels) > EPSILON(0.0)) THEN
        w_flux_leach = w_flux_soilt(i,m,sm_levels) /                           &
                       (sorp * smcl_old_soilt(i,m,sm_levels))
        tstep_flux_leach = MIN(w_flux_leach, max_leach_flux) * timestep
        n_inorg_soilt_lyrs(i,m,sm_levels) =                                    &
                                         n_inorg_soilt_lyrs(i,m,sm_levels) -   &
                                         tstep_flux_leach *                    &
                                         n_inorg_gb_old(i,sm_levels)
        IF (n_inorg_soilt_lyrs(i,m,sm_levels) < 0.0) THEN
          n_inorg_soilt_lyrs(i,m,sm_levels) = 0.0
        END IF
        n_inorg_avail_pft(i,:,sm_levels) = n_inorg_avail_pft(i,:,sm_levels) -  &
                                           tstep_flux_leach *                  &
                                           n_inorg_avail_old(i,:,sm_levels)
        DO nn = 1,npft
          IF (n_inorg_avail_pft(i,nn,sm_levels) < 0.0) THEN
            n_inorg_avail_pft(i,nn,sm_levels) = 0.0
          END IF
        END DO
      END IF

      IF (w_flux_soilt(i,m,0) < 0.0 .AND.                                      &
          smcl_old_soilt(i,m,1) > EPSILON(0.0)) THEN
        w_flux_leach = w_flux_soilt(i,m,0) / (sorp * smcl_old_soilt(i,m,1))
        tstep_flux_leach = MAX(w_flux_leach, max_leach_flux * (-1.0)) * timestep
        n_inorg_soilt_lyrs(i,m,1) = n_inorg_soilt_lyrs(i,m,1) +                &
                                    tstep_flux_leach *                         &
                                    n_inorg_gb_old(i,1)
        IF (n_inorg_soilt_lyrs(i,m,1) < 0.0) THEN
          n_inorg_soilt_lyrs(i,m,1) = 0.0
        END IF
        n_inorg_avail_pft(i,:,1) = n_inorg_avail_pft(i,:,1) +                  &
                                   tstep_flux_leach *                          &
                                   n_inorg_avail_old(i,:,1)
        DO nn = 1,npft
          IF (n_inorg_avail_pft(i,nn,1) < 0.0) THEN
            n_inorg_avail_pft(i,nn,1) = 0.0
          END IF
        END DO
      END IF

      IF (l_top) THEN ! LSH on: include leaching from lateral runoff.
        DO n = 1,sm_levels
          IF (smcl_old_soilt(i,m,n) > EPSILON(0.0) .AND.                       &
              qbase_l_soilt(i,m,n) > EPSILON(0.0)) THEN
            w_flux_leach = qbase_l_soilt(i,m,n) /                              &
                              (sorp * smcl_old_soilt(i,m,n))
            tstep_flux_leach = MIN(w_flux_leach, max_leach_flux) * timestep
            n_inorg_soilt_lyrs(i,m,n) = n_inorg_soilt_lyrs(i,m,n) -            &
                                 tstep_flux_leach * n_inorg_gb_old(i,n)
            IF (n_inorg_soilt_lyrs(i,m,n) < 0.0) THEN
              n_inorg_soilt_lyrs(i,m,n) = 0.0
            END IF
            n_inorg_avail_pft(i,:,n) = n_inorg_avail_pft(i,:,n) -              &
                                       tstep_flux_leach *                      &
                                       n_inorg_avail_old(i,:,n)
            DO nn = 1,npft
              IF (n_inorg_avail_pft(i,nn,1) < 0.0) THEN
                n_inorg_avail_pft(i,nn,1) = 0.0
              END IF
            END DO
          END IF

        END DO  !  layers
      END IF ! LSH on.

      ! Calculate total leaching as difference in total inorganic nitrogen
      n_leach_soilt(i,m) = (SUM(n_inorg_gb_old(i,:)) -                         &
                            SUM(n_inorg_soilt_lyrs(i,m,:))) / timestep
      n_leach_soilt(i,m) = MAX(n_leach_soilt(i,m),0.0)

    ELSE !.NOT. l_layeredc
      ! Find layer index for top 1m.
      ztop = 0.0
      DO n = 1,sm_levels
        ztop = ztop + dzsoil(n)
        IF (ztop > 1.0) EXIT
        n_1m = n
      END DO

      IF (SUM(smcl_soilt(i,m,1:n_1m)) > EPSILON(0.0)) THEN
        n_leach_soilt(i,m) = sub_surf_roff(i)                                  &
                             * (n_inorg_soilt_lyrs(i,m,1) / sorp)              &
                             / SUM(smcl_soilt(i,m,1:n_1m))  ! (top 1m)
      END IF
      n_leach_soilt(i,m) = MIN(n_leach_soilt(i,m),                             &
                           (n_inorg_soilt_lyrs(i,m,1) / sorp) / timestep)
      !H20 flux                ! N concentration Kg [N]/kg [H20]
      n_inorg_soilt_lyrs(i,m,1) = n_inorg_soilt_lyrs(i,m,1)                    &
                                  - n_leach_soilt(i,m) * timestep
    END IF

    ! Accumulate leaching to allow it to be output as STASH 19114 on TRIFFID
    ! timesteps like all other section 19 diagnostics.
    IF ( m == 1 ) THEN
      n_leach_gb_acc(i) =  n_leach_gb_acc(i) + n_leach_soilt(i,m) * timestep
    END IF

  END DO !land_pts
END DO !nsoilt

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE n_leach
END MODULE n_leach_mod
