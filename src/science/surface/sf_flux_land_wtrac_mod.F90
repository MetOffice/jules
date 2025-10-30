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

MODULE sf_flux_land_wtrac_mod

!-----------------------------------------------------------------------------
! Description:
!   Calculate water tracer surface fluxes over land.
!
!   Unlike normal water, these are calculated separately for the four
!   different evaporative sources:
!   snow, canopy intercepted water, bare soil/transpiration from veg and
!   open water (i.e. lakes).
!
!   This is necessary due to evaporative sources having different water
!   tracer concentrations and different kinetic fractionation factors (for
!   isotopes).
!
!   The module also contains a subroutine to check the calculated fluxes
!   against the water fluxes.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in Surface
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!
!-----------------------------------------------------------------------------

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

! Maximum difference between water and water tracer fqw_1 which trigger
! warning or abort
REAL(KIND=real_jlslsm), PARAMETER :: max_diff_warning = 1.0e-16
REAL(KIND=real_jlslsm), PARAMETER :: max_diff_abort   = 1.0e-12

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SF_FLUX_LAND_WTRAC_MOD'

CONTAINS

SUBROUTINE sf_flux_land_wtrac (i_wt,                                           &
 land_pts, surft_pts, surft_index, alpha1, rhokh_1, qstar,                     &
 dtstar, resf_wtrac, evap_srce_frac, q_elev_wtrac,                             &
 snow_surft, snow_surft_wtrac, smc_soilt, smc_soilt_wtrac,                     &
 canopy, canopy_wtrac, fqw_evapsrce_wtrac, fqw_1_wtrac)

USE jules_water_tracers_mod, ONLY: n_evap_srce, wtrac_lake_fixed_ratio,        &
                                    evap_srce_snow, evap_srce_cano,            &
                                    evap_srce_soil, evap_srce_lake,            &
                                    wtrac_calc_ratio_fn_jules

USE parkind1, ONLY: jprb, jpim
USE yomhook,  ONLY: lhook, dr_hook

IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
 i_wt                                                                          &
                       ! IN Water tracer index
,land_pts                                                                      &
                       ! IN Total number of land points.
,surft_pts                                                                     &
                       ! IN Number of tile points.
,surft_index(land_pts)
                       ! IN Index of tile points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 alpha1(land_pts)                                                              &
                       ! IN Mean gradient of saturated specific humidity with
                       !    respect to temperature between the bottom model
                       !    layer and tile surfaces
,rhokh_1(land_pts)                                                             &
                       ! IN Surface exchange coefficients for land tiles
,qstar(land_pts)                                                               &
                       ! IN Surface saturated specific humidity
,dtstar(land_pts)                                                              &
                       ! IN Change in TSTAR over timestep for land tiles
,resf_wtrac(land_pts,n_evap_srce)                                              &
                       ! IN Resitance factor for water tracers for individual
                       !     evaporative source types
,evap_srce_frac(land_pts,n_evap_srce)                                          &
                       ! IN Evaporative source fractional contribution to
                       !    total evaporation from tile
,q_elev_wtrac(land_pts)                                                        &
                       ! IN Water tracer specific humidity at elevated
                       !    height
,snow_surft(land_pts)                                                          &
                       ! IN Lying snow
,snow_surft_wtrac(land_pts)                                                    &
                       ! IN Water tracer content of lying snow
,smc_soilt(land_pts)                                                           &
                       ! IN Available water content in soil layers
,smc_soilt_wtrac(land_pts)                                                     &
                       ! IN Available water tracer content in soil layers
,canopy(land_pts)                                                              &
                       ! IN Surface/canopy water for snow-free land tiles
,canopy_wtrac(land_pts)
                       ! IN Water tracer content in canopy

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
 fqw_evapsrce_wtrac(land_pts,n_evap_srce)                                      &
                       ! IN OUT Water tracer surface flux per evaporative
                       !        source
,fqw_1_wtrac(land_pts)
                       ! IN OUT Water tracer surface flux per tile

! Local variables

INTEGER ::                                                                     &
 k                                                                             &
             ! Loop counter (tile field index).
,l                                                                             &
             ! Loop counter (land point field index).
,n_evap
             ! Loop counter for evaporative sources

REAL(KIND=real_jlslsm) ::                                                      &
 eqm_factor                                                                    &
             ! Equilibrium fractionation factor
,r_wtrac                                                                       &
             ! Ratio of water tracer to water for evaporative source
,alpha1_wtrac                                                                  &
             !  d(qsat for water tracers) / dT
,epot
             ! Potential evaporation for water tracers

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SF_FLUX_LAND_WTRAC'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEL DO IF(surft_pts > 1)                                            &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(l,k,n_evap,eqm_factor,r_wtrac,alpha1_wtrac,epot)                 &
!$OMP SHARED(surft_pts,surft_index,i_wt,snow_surft_wtrac,snow_surft,           &
!$OMP        smc_soilt_wtrac,smc_soilt,canopy_wtrac,canopy,alpha1,             &
!$OMP        rhokh_1,qstar,q_elev_wtrac,resf_wtrac,fqw_evapsrce_wtrac,         &
!$OMP        dtstar,fqw_1_wtrac,evap_srce_frac,wtrac_lake_fixed_ratio)
DO k = 1,surft_pts
  l = surft_index(k)

  DO n_evap = 1, n_evap_srce
    SELECT CASE(n_evap)

    CASE (evap_srce_snow)
      ! Snow sublimation

      ! Set isotopic equilibrium fractionation factor
      ! (Temporarily set to 1 until isotopes are coded)
      eqm_factor = 1.0

      ! Set ratio of water tracer to water for source
      r_wtrac = wtrac_calc_ratio_fn_jules(i_wt,snow_surft_wtrac(l),            &
                                               snow_surft(l))

    CASE (evap_srce_soil)
      ! Soil evapotranspiration

      ! Set isotopic equilibrium fractionation factor
      ! (Temporarily set to 1 until isotopes are coded)
      eqm_factor = 1.0

      ! Set ratio of water tracer to water for source
      r_wtrac = wtrac_calc_ratio_fn_jules(i_wt,smc_soilt_wtrac(l),smc_soilt(l))

    CASE (evap_srce_cano)
      ! Canopy intercepted water evaporation

      ! Set isotopic equilibrium fractionation factor
      ! (Temporarily set to 1 until isotopes are coded)
      eqm_factor = 1.0

      ! Set ratio of water tracer to water for source
      r_wtrac = wtrac_calc_ratio_fn_jules(i_wt, canopy_wtrac(l), canopy(l))


    CASE (evap_srce_lake)
      ! Lake evaporation

      ! Set isotopic equilibrium fractionation factor
      ! (Temporarily set to 1 until isotopes are coded)
      eqm_factor = 1.0

      ! Set ratio of water tracer to water for source
      r_wtrac = wtrac_lake_fixed_ratio(i_wt)

    END SELECT

    ! Calculate water tracer equivalent to alpha1
    alpha1_wtrac = (r_wtrac/eqm_factor) * alpha1(l)

    ! Calculate evaporation potential
    epot = rhokh_1(l) *                                                        &
             ( (r_wtrac/eqm_factor) * qstar(l) - q_elev_wtrac(l) )

    ! Calculate explicit water tracer surface flux
    fqw_evapsrce_wtrac(l,n_evap) = resf_wtrac(l,n_evap) * epot

    ! Correction to surface flux due to change in surface temperature
    fqw_evapsrce_wtrac(l,n_evap) =  fqw_evapsrce_wtrac(l,n_evap)               &
                                    + resf_wtrac(l,n_evap)                     &
                                      * rhokh_1(l) * alpha1_wtrac * dtstar(l)

    ! Scale surface flux by evaporative source fraction to get tile mean value
    fqw_evapsrce_wtrac(l,n_evap) =  fqw_evapsrce_wtrac(l,n_evap)               &
                                        * evap_srce_frac(l,n_evap)

    ! Calculate total tile flux
    fqw_1_wtrac(l) = fqw_1_wtrac(l) + fqw_evapsrce_wtrac(l,n_evap)

  END DO  ! evaporation sources

END DO  ! tile points
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN
END SUBROUTINE sf_flux_land_wtrac

!-----------------------------------------------------------------------------

SUBROUTINE sf_flux_land_check_wtrac (                                          &
 land_pts, nsurft, land_index, tile_frac, flandg,                              &
 snowdep_surft, canopy, flake, dq, fracaero, resfs, resft,                     &
 resf_wtrac, evap_srce_frac,                                                   &
 fqw_surft, fqw_1, fqw_surft_wtrac, fqw_1_wtrac, fqw_evapsrce_wtrac )

!-----------------------------------------------------------------------------
! Description:
!   Compare the 'normal' water tracer explicit surface flux with the
!   calculated water flux.  If the differences are large, then either
!   write a warning or abort model.
!-----------------------------------------------------------------------------

USE atm_fields_bounds_mod, ONLY: tdims
USE theta_field_sizes,     ONLY: t_i_length

USE jules_water_tracers_mod, ONLY: n_evap_srce

USE jules_print_mgr, ONLY: jules_message, jules_print, newline

USE ereport_mod, ONLY: ereport

USE parkind1, ONLY: jprb, jpim
USE yomhook,  ONLY: lhook, dr_hook

IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
 land_pts                                                                      &
                       ! IN Total number of land points.
,nsurft                                                                        &
                       ! IN No. of land-surface tiles
,land_index(land_pts)
                       ! IN Index of land points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 tile_frac(land_pts,nsurft)                                                    &
                       ! IN Tile fractions
,flandg(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                       ! IN Land fraction on all tiles.
,snowdep_surft(land_pts,nsurft)                                                &
                       ! IN Snow depth
,canopy(land_pts, nsurft)                                                      &
                       ! IN Surface/canopy water for snow-free land tiles
,flake(land_pts,nsurft)                                                        &
                       ! IN Lake fraction.
,dq(land_pts,nsurft)                                                           &
                       ! IN Specific humidity difference between
                       !    surface and lowest atmospheric lev
,fracaero(land_pts,nsurft)                                                     &
                       ! IN Fraction of surface moisture flux with only
                       !    aerodynamic resistance for snow-free land tiles.
,resfs(land_pts,nsurft)                                                        &
                       ! IN Combined soil, stomatal and aerodynamic resistance
                       !     factor for fraction (1-fracaero) of snow-free land
                       !     tiles.
,resft(land_pts,nsurft)                                                        &
                       ! IN Total resistance factor.
                       !  fracaero+(1-fracaero)*resfs for snow-free land,
                       !  1 for snow.
,resf_wtrac(land_pts,n_evap_srce,nsurft)                                       &
                       ! IN Resitance factor for water tracers for individual
                       !     evaporative source types
,evap_srce_frac(land_pts,n_evap_srce,nsurft)
                       ! IN Evaporative source fractional contribution to
                       !    total evaporation from tile

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 fqw_surft(land_pts,nsurft)                                                    &
                       ! IN Surface FQW for land tiles
,fqw_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                       ! IN GBM surface FQW
,fqw_surft_wtrac(land_pts,nsurft)                                              &
                       ! IN Water tracer surface flux per tile
,fqw_1_wtrac(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)              &
                       ! IN GBM Water tracer surface FQW
,fqw_evapsrce_wtrac(land_pts,n_evap_srce,nsurft)
                       ! IN Water tracer surface flux per evaporative
                       !        source

! Local variables

INTEGER ::                                                                     &
 i,j                                                                           &
             ! Loop counter (horizontal field index).
,l                                                                             &
             ! Loop counter (land point field index).
,n                                                                             &
             ! Loop counter (tile index)
,n_evap
             ! Loop counter for evaporative sources

INTEGER :: imax, jmax, lmax

INTEGER :: ErrorStatus

REAL(KIND=real_jlslsm) :: diff, largest_error

CHARACTER(LEN=256)     :: message                 ! Error reporting

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SF_FLUX_LAND_CHECK_WTRAC'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Compare water tracer flux to normal water flux and find largest error.

largest_error = 0.0

DO l = 1,land_pts
  j=(land_index(l) - 1) / t_i_length + 1
  i = land_index(l) - (j-1) * t_i_length
  diff = fqw_1(i,j) - fqw_1_wtrac(i,j)
  IF ( ABS(diff) > ABS(largest_error) ) THEN
    largest_error = diff
    imax          = i
    jmax          = j
    lmax          = l
  END IF
END DO

! If error is greater than max_diff_warning then write warning.

IF (ABS(largest_error) > max_diff_warning) THEN

  WRITE(jules_message,'(a,3g20.12,a,2(i0,1x),e14.6)')                          &
  newline// '*********WATER TRACER PROBLEM*********' //newline//               &
  'Large difference between tracer and water for explicit fqw_1 in JULES'      &
  //newline// 'after land calculations' //newline//                            &
  ' fqw_1, fqw_1_wtrac, max diff  = ' //newline,                               &
  fqw_1(imax,jmax),fqw_1_wtrac(imax,jmax), largest_error,                      &
  newline// ' at i,j,flandg = ', imax,jmax,flandg(imax,jmax)

  CALL jules_print(RoutineName, jules_message)

  ! Print out tile information

  DO n = 1, nsurft

    IF (tile_frac(lmax,n) > 0.0) THEN

      WRITE(jules_message,'(A,i3,e20.12)')                                     &
        newline// 'Individual file fluxes for n, tile_frac =',                 &
        n,tile_frac(lmax,n)
      CALL jules_print(RoutineName, jules_message)

      WRITE(jules_message,'(A,3e20.12)')                                       &
        newline// 'fqw_surft: water, wtrac, diff = ', fqw_surft(lmax,n),       &
      fqw_surft_wtrac(lmax,n), fqw_surft(lmax,n)-fqw_surft_wtrac(lmax,n)
      CALL jules_print(RoutineName, jules_message)

      WRITE(jules_message,'(A,7e20.12)')                                       &
       'snow,canopy,flake,fracaero,dq,resft,resfs = ',                         &
        snowdep_surft(lmax,n), canopy(lmax,n),                                 &
        flake(lmax,n),fracaero(lmax,n),dq(lmax,n),resfs(lmax,n),resft(lmax,n)
      CALL jules_print(RoutineName, jules_message)

      WRITE(jules_message,'(A,4e20.12)')                                       &
       'fqw_evapsrce_wtrac=', (fqw_evapsrce_wtrac(lmax,n_evap,n), n_evap=1,4)
      CALL jules_print(RoutineName, jules_message)

      WRITE(jules_message,'(A,4e20.12)')                                       &
        'resf_wtrac=', (resf_wtrac(lmax,n_evap,n), n_evap=1,4)
      CALL jules_print(RoutineName, jules_message)

      WRITE(jules_message,'(A,4e20.12)')                                       &
        'evap_srce_frac=', (evap_srce_frac(lmax,n_evap,n),n_evap=1,4)
      CALL jules_print(RoutineName, jules_message)

    END IF ! tile_frac > 0
  END DO   ! n

  ! Either write warning or abort model
  IF (ABS(largest_error) > max_diff_abort) THEN
    ! Abort model
    ErrorStatus = 100
    message = 'Problem with water tracers.'
  ELSE
    ! Output warning
    ErrorStatus = -100
    message = 'Problem with water tracers.'
  END IF

  CALL ereport(RoutineName, ErrorStatus, message)

END IF  ! Test on largest error size

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE sf_flux_land_check_wtrac

END MODULE sf_flux_land_wtrac_mod
