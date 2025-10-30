!******************************COPYRIGHT****************************************
! (c) British Antarctic Survey and University of Bristol, 2023.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT****************************************

MODULE sf_land_imp_wtrac_mod

!------------------------------------------------------------------------------
! Description:
!    Calculate final water tracer surface fluxes over land.
!
! Method:
!   Update the explicit water tracer surface fluxes over land for changes
!   in the normal water flux due to:
!   a) the implicit calculation (in sf_im_pt2)
!   b) ensuring there is sufficient water for the individual evaporative
!      fluxes (sf_evap)
!   c) any change in temperature caused by surface melt (sf_melt).
!
!   Water tracers are updated here based on the assumption that the surface
!   flux ratio of water tracer to water remains constant between the explicit
!   and implicit schemes for each evaporative source type.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in Surface
!
! Code Description:
!   Language: Fortran 90.
!-------------------------------------------------------------------------------

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE


CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SF_LAND_IMP_WTRAC_MOD'

CONTAINS

SUBROUTINE sf_land_imp_wtrac (                                                 &
 land_pts, nsurft, sm_levels, n_evap_srce,                                     &
 surft_pts, land_index, surft_index, timestep, std_ratio, flandg, tile_frac,   &
 non_lake_frac,                                                                &
 snow_surft_wtrac, smc_soilt_wtrac, canopy_wtrac, wt_ext_surft,                &
 ei_surft, esoil_surft, ecan_surft, elake_surft,                               &
 fqw_evapsrce_wtrac, fqw_norm_wtrac, fqw_surft_wtrac,                          &
 ei_wtrac, ei_surft_wtrac, esoil_soilt_wtrac, esoil_surft_wtrac,               &
 ext_soilt_wtrac, ecan_wtrac, ecan_surft_wtrac, elake_surft_wtrac,             &
 fqw_1_wtrac, dfqw_wtrac )


USE atm_fields_bounds_mod, ONLY: tdims
USE theta_field_sizes,     ONLY: t_i_length

USE ancil_info, ONLY: nsoilt

USE jules_water_tracers_mod, ONLY: evap_srce_snow, evap_srce_cano,             &
                                   evap_srce_soil, evap_srce_lake, min_q_ratio
USE jules_surface_mod,       ONLY: l_flake_model
USE jules_surface_types_mod, ONLY: lake

USE parkind1, ONLY: jprb, jpim
USE yomhook,  ONLY: lhook, dr_hook

IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
 land_pts                                                                      &
                       ! IN Number of land points to be processed
,nsurft                                                                        &
                       ! IN Number of tiles per land point
,sm_levels                                                                     &
                       ! IN Number of soil moisture levels
,n_evap_srce                                                                   &
                       ! IN Number of evaporative sources
,surft_pts(nsurft)                                                             &
                       ! IN Number of tile points
,land_index(land_pts)                                                          &
                       ! IN Index of land points
,surft_index(land_pts,nsurft)
                       ! IN Index of tile points

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 timestep                                                                      &
                       ! IN Timestep in seconds
,std_ratio                                                                     &
                       ! IN Standard water tracer to water ratio
,flandg(tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end)                  &
                       ! IN Fraction of gridbox which is land

,tile_frac(land_pts,nsurft)                                                    &
                       ! IN Tile fractions
,non_lake_frac(land_pts)                                                       &
                       ! IN Sum of fractions of non-FLake surface tiles
,snow_surft_wtrac(land_pts,nsurft)                                             &
                       ! IN Amount of water tracer in lying
                       !           snow on tiles (kg/m2)
,smc_soilt_wtrac(land_pts,nsoilt)                                              &
                       ! IN Available water tracer in soil moisture (kg/m2)
,canopy_wtrac(land_pts,nsurft)                                                 &
                       ! IN Amount of water tracer in surface/canopy
                       !    water on land tiles (kg/m2)
,wt_ext_surft(land_pts,sm_levels,nsurft)
                       ! IN Fraction of transpiration extracted from each soil
                       !    layer by each tile.

! Normal water fluxes (all kg/m2/s) (which have been updated in sf_evap and
!  sf_melt)
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 ei_surft(land_pts,nsurft)                                                     &
                       ! IN Sublimation from snow or land-ice
,esoil_surft(land_pts,nsurft)                                                  &
                       ! IN Evapotranspiration from soil moisture
,ecan_surft(land_pts,nsurft)                                                   &
                       ! IN Evaporation from canopy/surface store
,elake_surft(land_pts,nsurft)
                       ! IN Lake evaporation

! Water tracer fluxes (all kg/m2/s)

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 fqw_evapsrce_wtrac(land_pts,n_evap_srce,nsurft)                               &
                       ! IN Surface water tracer FQW for the seperate
                       !    evaporative sources on land tiles (calculated
                       !    in the explicit scheme)
,fqw_norm_wtrac(land_pts,n_evap_srce,nsurft)
                       ! IN Surface 'normal water' FQW for the separate
                       !    evaporative sources (calculated in the explicit
                       !    scheme)

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
 fqw_surft_wtrac(land_pts,nsurft)
                       ! IN OUT Water tracer local FQW_1 for tiles.

! Final water tracer fluxes from each evaporative source (all kg/m2/s)
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 ei_wtrac(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                 &
                       ! OUT Land mean water tracer sublimation from
                       !     snow of land-ice
,ei_surft_wtrac(land_pts,nsurft)                                               &
                       ! OUT Sublimation of water tracer from snow
                       !    or land-ice
,esoil_soilt_wtrac(tdims%i_start:tdims%i_end,                                  &
                   tdims%j_start:tdims%j_end,nsoilt)                           &
                       ! OUT Land mean water tracer evapotranspiration from
                       !     soil moisture
,esoil_surft_wtrac(land_pts,nsurft)                                            &
                       ! OUT Water tracer ESOIL for land tiles.
,ext_soilt_wtrac(land_pts,nsoilt,sm_levels)                                    &
                       ! OUT Extraction of water tracer from each
                       !     soil layer of land tiles.
,ecan_wtrac(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)               &
                       ! OUT Land mean water tracer evaporation from
                       !     canopy/surface store
,ecan_surft_wtrac(land_pts,nsurft)                                             &
                       ! OUT Water tracer ECAN for land tiles
,elake_surft_wtrac(land_pts,nsurft)                                            &
                       ! OUT Lake water tracer evaporation for land tiles
,fqw_1_wtrac(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)              &
                       ! OUT GBM Surface moisture flux of water
                       !       tracers
,dfqw_wtrac(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                       ! OUT Increment in water tracer GBM moisture flux.


! Local variables

INTEGER ::                                                                     &
 i,j                                                                           &
             ! Loop counter (horizontal field index).
,k                                                                             &
             ! Loop counter (land, snow or land-ice field index).
,m                                                                             &
             ! Loop counter (soil level index).
,l                                                                             &
             ! Loop counter (land point field index).
,n                                                                             &
             ! Loop counter (tile index).
,mm                                                                            &
             ! Index for soil tile.
,n_evap
             ! Loop counter (evaporative source)

REAL(KIND=real_jlslsm) ::                                                      &
 edt                                                                           &
                       ! Temperature field updated for flux
,fqw_surft_wtrac_old                                                           &
                       ! Water tracer surface moisture flux
                       ! before adjustment.
,fqw_ratio(land_pts,n_evap_srce,nsurft)
                       ! Ratio of water tracer flux to normal water flux

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SF_LAND_IMP_WTRAC'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(SHARED)                                                          &
!$OMP PRIVATE(i,j,l,k,n_evap,m,mm,n,edt,fqw_surft_wtrac_old)

! Initialise GBM water tracer surface flux to zero - this is different to
! the treatment of normal water in sf_evap.  This is because the GBM normal
! water flux is updated for the implicit solution in im_sf_pt2 and then a
! correction is applied in sf_evap for limited moisture availability.
!
! Also, initialise land mean fields
!
!$OMP DO SCHEDULE(STATIC)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    fqw_1_wtrac(i,j)         = 0.0
    dfqw_wtrac(i,j)          = 0.0
    DO mm = 1, nsoilt
      esoil_soilt_wtrac(i,j,mm) = 0.0
    END DO
    ecan_wtrac(i,j)          = 0.0
    ei_wtrac(i,j)            = 0.0
  END DO
END DO
!$OMP END DO NOWAIT

! Initialise tile water tracer evaporative fluxes
DO n = 1, nsurft
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    ei_surft_wtrac(l,n)    = 0.0
    esoil_surft_wtrac(l,n) = 0.0
    ecan_surft_wtrac(l,n)  = 0.0
    elake_surft_wtrac(l,n) = 0.0
  END DO
!$OMP END DO NOWAIT
END DO
! Initialise the extraction of water tracers from each layer
DO m = 1,sm_levels
  DO mm = 1, nsoilt
!$OMP DO SCHEDULE(STATIC)
    DO l = 1,land_pts
      ext_soilt_wtrac(l,mm,m) = 0.0
    END DO
!$OMP END DO NOWAIT
  END DO
END DO

! Calculate the ratio of the moisture fluxes for water tracers to
! moisture fluxes for normal water which were calculated in the explicit
! scheme.
DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
  DO k = 1,surft_pts(n)
    l = surft_index(k,n)
    DO n_evap = 1, n_evap_srce
      IF (ABS(fqw_norm_wtrac(l,n_evap,n)) > min_q_ratio) THEN
        fqw_ratio(l,n_evap,n) = fqw_evapsrce_wtrac(l,n_evap,n)                 &
                                  / fqw_norm_wtrac(l,n_evap,n)
      ELSE
        fqw_ratio(l,n_evap,n) = std_ratio
      END IF
    END DO
  END DO
!$OMP END DO
END DO

! Update explicit fluxes to account for the normal water flux changing in
! the implicit scheme.
!
! Assumption here is that the ratio of isotopic evaporative fluxes to normal
! water evaporative fluxes remains UNCHANGED.
!
! This is new_wtrac_flux = old_wtrac_flux
!                           + ratio * (new_water_flux - old_water_flux)
! where ratio = old_wtrac_flux/old_water_flux
!
! which can be reduced to new_wtrac_flux = ratio * new_water_flux

DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
  DO k = 1,surft_pts(n)
    l = surft_index(k,n)

    ! Snow_sublimation
    ei_surft_wtrac(l,n)    = fqw_ratio(l,evap_srce_snow,n) * ei_surft(l,n)

    ! Soil evapotranspiration
    esoil_surft_wtrac(l,n) = fqw_ratio(l,evap_srce_soil,n) * esoil_surft(l,n)

    ! Canopy intercepted water evaporation
    ecan_surft_wtrac(l,n)  = fqw_ratio(l,evap_srce_cano,n) * ecan_surft(l,n)

    ! Lake
    elake_surft_wtrac(l,n) = fqw_ratio(l,evap_srce_lake,n) * elake_surft(l,n)

  END DO
!$OMP END DO
END DO

! Calculate land mean evapotranspiration for water tracers from soil moisture
DO n = 1,nsurft
  !Set the current soil tile
  IF (nsoilt == 1) THEN
    !There is only 1 soil tile
    mm = 1
  ELSE  ! nsoilt == nsurft
    !Soil tiles map directly on to surface tiles
    mm = n
  END IF
!$OMP DO SCHEDULE(STATIC)
  DO k = 1,surft_pts(n)
    l = surft_index(k,n)
    j=(land_index(l) - 1) / t_i_length + 1
    i = land_index(l) - (j-1) * t_i_length

    IF ( .NOT. ((l_flake_model) .AND. (n == lake) .AND. (nsoilt == 1)) ) THEN
      ! So this is done unless l_flake_model .AND. (n == lake) .AND.
      !                        (nsoilt == 1)
      esoil_soilt_wtrac(i,j,mm) = esoil_soilt_wtrac(i,j,mm)                    &
                                  + tile_frac(l,n) * esoil_surft_wtrac(l,n)
    END IF
  END DO
!$OMP END DO
END DO

IF ((nsoilt == 1) .AND. (l_flake_model)) THEN
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    IF (non_lake_frac(l) > EPSILON(0.0)) THEN
      j=(land_index(l) - 1) / t_i_length + 1
      i = land_index(l) - (j-1) * t_i_length
      esoil_soilt_wtrac(i,j,1) = esoil_soilt_wtrac(i,j,1) / non_lake_frac(l)
    END IF
  END DO
!$OMP END DO
END IF

! If necessary, reduce the calculated evaporative fluxes if there is
! insufficient source water tracer.
! (Note, there is no check on lakes, as it is assummed that there is infinite
!  water and water tracer available)

! First check sublimation and canopy evaporation
DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
  DO k = 1,surft_pts(n)
    l = surft_index(k,n)

    ! Ensure the sublimation flux does not remove more water tracer from the
    ! snow than is available
    IF (ei_surft_wtrac(l,n) > 0.0) THEN
      ei_surft_wtrac(l,n) = MIN( ei_surft_wtrac(l,n),                          &
                                 snow_surft_wtrac(l,n)/timestep )
    END IF

    ! Ensure the canopy evap flux does not remove more water tracer from the
    ! canopy than is available
    IF (ecan_surft_wtrac(l,n) > 0.0) THEN
      ecan_surft_wtrac(l,n) = MIN( ecan_surft_wtrac(l,n),                      &
                                   canopy_wtrac(l,n) / timestep )
    END IF

    ! No check on lakes, as assumption is there is infinite water and water
    ! tracer available

  END DO
!$OMP END DO
END DO

! Second, check soil evapotranspiration does not remove more water tracer
! than is present in the available soil moisture

!$OMP DO SCHEDULE(STATIC)
DO l = 1,land_pts
  j=(land_index(l) - 1) / t_i_length + 1
  i = land_index(l) - (j-1) * t_i_length
  IF (nsoilt == 1) THEN
    ! There is only 1 soil tile
    mm = 1
    edt = esoil_soilt_wtrac(i,j,mm) * timestep
    IF ( edt > smc_soilt_wtrac(l,mm) ) THEN
      DO n = 1, nsurft
        esoil_surft_wtrac(l,n) = smc_soilt_wtrac(l,mm)                         &
                                   * esoil_surft_wtrac(l,n) / edt
      END DO
      esoil_soilt_wtrac(i,j,mm) = smc_soilt_wtrac(l,mm) / timestep
    END IF

  ELSE
    DO n = 1, nsurft
      ! Soil tiles map directly on to surface tiles
      mm = n
      edt = esoil_soilt_wtrac(i,j,mm) * timestep
      IF ( edt > smc_soilt_wtrac(l,mm) ) THEN
        esoil_surft_wtrac(l,n) = smc_soilt_wtrac(l,mm)                         &
                                     * esoil_surft_wtrac(l,n) / edt
        esoil_soilt_wtrac(i,j,mm) = smc_soilt_wtrac(l,mm) / timestep
      END IF
    END DO  ! nsurft
  END IF  ! nsoilt ==1
END DO
!$OMP END DO

! Now, calculate the extraction of water tracers from each soil layer
DO m = 1,sm_levels
  DO n = 1,nsurft

    IF (nsoilt == 1) THEN
      !There is only 1 soil tile
      mm = 1
      IF ( .NOT. ((l_flake_model) .AND. (n == lake)) ) THEN
!$OMP DO SCHEDULE(STATIC)
        DO k = 1,surft_pts(n)
          l = surft_index(k,n)
          ext_soilt_wtrac(l,mm,m) = ext_soilt_wtrac(l,mm,m)                    &
                                    + tile_frac(l,n) * wt_ext_surft(l,m,n)     &
                                    * esoil_surft_wtrac(l,n)
        END DO
!$OMP END DO
      END IF

    ELSE ! nsoilt == nsurft
      !Soil tiles map directly on to surface tiles
      mm = n
!$OMP DO SCHEDULE(STATIC)
      DO k = 1,surft_pts(n)
        l = surft_index(k,n)
        ext_soilt_wtrac(l,mm,m) = ext_soilt_wtrac(l,mm,m)                      &
                                + wt_ext_surft(l,m,n) * esoil_surft_wtrac(l,n)
      END DO
!$OMP END DO
    END IF !nsoilt
  END DO !nsurft

  IF ((nsoilt == 1) .AND. (l_flake_model)) THEN
    ! Normalise ext_soilt, excluding the lake tile fraction.
!$OMP DO SCHEDULE(STATIC)
    DO l = 1,land_pts
      IF (non_lake_frac(l) > EPSILON(0.0)) THEN
        ext_soilt_wtrac(l,1,m) = ext_soilt_wtrac(l,1,m) / non_lake_frac(l)
      END IF
    END DO
!$OMP END DO
  END IF

END DO !sm_levels


! Now that all the individual surface fluxes are finalised,
! calculate GBM water tracer surface flux and the increment

DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
  DO k = 1,surft_pts(n)
    l = surft_index(k,n)
    j = (land_index(l) - 1) / t_i_length + 1
    i = land_index(l) - (j-1) * t_i_length

    fqw_surft_wtrac_old  = fqw_surft_wtrac(l,n)
    fqw_surft_wtrac(l,n) = ecan_surft_wtrac(l,n) + esoil_surft_wtrac(l,n)      &
                         + elake_surft_wtrac(l,n) + ei_surft_wtrac(l,n)

    fqw_1_wtrac(i,j) = fqw_1_wtrac(i,j) + flandg(i,j) * tile_frac(l,n)         &
                              * fqw_surft_wtrac(l,n)

    dfqw_wtrac(i,j)  = dfqw_wtrac(i,j) + flandg(i,j) * tile_frac(l,n) *        &
                           ( fqw_surft_wtrac(l,n) - fqw_surft_wtrac_old )

    ei_wtrac(i,j)   = ei_wtrac(i,j)   + tile_frac(l,n) * ei_surft_wtrac(l,n)
    ecan_wtrac(i,j) = ecan_wtrac(i,j) + tile_frac(l,n) * ecan_surft_wtrac(l,n)

  END DO
!$OMP END DO
END DO
!$OMP END PARALLEL

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE sf_land_imp_wtrac

END MODULE sf_land_imp_wtrac_mod
