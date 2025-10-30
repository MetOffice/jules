! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE crop_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

PRIVATE
PUBLIC :: crop

CONTAINS

SUBROUTINE crop(p_field, land_pts, land_index, a_step,                         &
                crop_call, sm_levels, frac, phot, dphotdt,                     &
                t_surft, t_soil_soilt, sthu_soilt, smvccl_soilt,               &
                smvcst_soilt, npp_ft_acc,                                      &
                canht, lai, dvi, rootc, harvc,                                 &
                reservec, croplai, cropcanht,                                  &
                catch_t, z0_t,                                                 &
                !New arguments replacing USE statements
                !crop_vars_mod
                sow_date_cpft, tt_veg_cpft, tt_rep_cpft,                       &
                latestharv_date_cpft,                                          &
                yield_diag_cpft, stemc_diag_cpft,                              &
                leafc_diag_cpft, nonyield_diag_cpft,                           &
                harvest_trigger_cpft, harvest_counter_cpft,                    &
                !ancil_info (IN)
                l_lice_point, l_lice_surft)

!Use in relevant subroutines
USE pft_sparm_mod, ONLY: pft_sparm
USE tilepts_mod,   ONLY: tilepts

!Use in variables
USE cropparm, ONLY: t_mort, yield_frac, initial_c_dvi

USE jules_surface_types_mod, ONLY: ncpft, ntype, nnpft, npft

USE crop_utils_mod, ONLY:                                                      &
  ! variables
  cropharvc_min,                                                               &
  ! functions/subroutines
  reset_crop,                                                                  &
  leafc_from_prognostics, stemc_from_prognostics,                              &
  lai_from_leafc, canht_from_stemc,                                            &
  initialise_crop

USE datetime_utils_mod, ONLY: day_of_year, days_in_year

USE time_info_mod, ONLY: l_360, l_leap, current_model_time

USE jules_vegetation_mod, ONLY: l_prescsow

USE ancil_info, ONLY: nsurft, nsoilt

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Calls crop routines and harvests crop when DVI exceeds 2
!
! Method:
!   See JULES-crop technical documentation (Tom Osborne, Josh Hooker).
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) ::                                                         &
  p_field,                                                                     &
    ! Number of model points
  land_pts,                                                                    &
    ! Number of land points to be processed.
  land_index(land_pts),                                                        &
    ! Index of land points
  a_step,                                                                      &
    ! Atmospheric timestep number
  crop_call,                                                                   &
    ! Indicates when to call sow and partition (daily timestep)
  sm_levels
    ! Number of soil levels

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  frac(land_pts,ntype),                                                        &
    ! Fractional coverage of tiles
  phot(p_field),                                                               &
    ! Photoperiod (hours)
  dphotdt(p_field),                                                            &
    ! Change in photoperiod in hours between this day and the previous day
  t_surft(land_pts,nsurft),                                                    &
    ! Temperature of tile (the 1.5m air temperature) (K).
  t_soil_soilt(land_pts,nsoilt,sm_levels),                                     &
    ! Soil temperatures (K).
  sthu_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Unfrozen soil moisture content of each layer as a fraction of saturation
  smvccl_soilt(land_pts,nsoilt,sm_levels),                                     &
    ! Critical volumetric SMC (cubic m per cubic m of soil)
  smvcst_soilt(land_pts,nsoilt,sm_levels)
    ! Volumetric saturation point (m3/m3 of soil)

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  npp_ft_acc(land_pts,npft),                                                   &
    ! Accumulated NPP (kg m-2).
  canht(land_pts,npft),                                                        &
    ! Canopy height of tiles (m).
  lai(land_pts,npft),                                                          &
    ! LAI of tiles
  dvi(land_pts,ncpft),                                                         &
    ! Development index of crop tiles
  rootc(land_pts,ncpft),                                                       &
    ! Root carbon of crop tiles (kg m-2).
  harvc(land_pts,ncpft),                                                       &
    ! Harvest carbon of crop tiles (kg m-2).
  reservec(land_pts,ncpft)
    ! Carbon in stem reserve pool (kg m-2).

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  croplai(land_pts,ncpft),                                                     &
    ! LAI of crop tiles
  cropcanht(land_pts,ncpft),                                                   &
    ! Canopy height of crop tiles (m).
  catch_t(land_pts,nsurft),                                                    &
    ! Canopy capacity (kg/m2).
  z0_t(land_pts,nsurft)
    ! Roughness length (m)


!New arguments replacing USE statements
!crop_vars_mod
REAL(KIND=real_jlslsm), INTENT(IN) :: sow_date_cpft(land_pts,ncpft)
REAL(KIND=real_jlslsm), INTENT(IN) :: tt_veg_cpft(land_pts,ncpft)
REAL(KIND=real_jlslsm), INTENT(IN) :: tt_rep_cpft(land_pts,ncpft)
REAL(KIND=real_jlslsm), INTENT(IN) :: latestharv_date_cpft(land_pts,ncpft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: yield_diag_cpft(land_pts,ncpft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: stemc_diag_cpft(land_pts,ncpft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: leafc_diag_cpft(land_pts,ncpft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: nonyield_diag_cpft(land_pts,ncpft)
INTEGER, INTENT(IN OUT) :: harvest_trigger_cpft(land_pts,ncpft)
INTEGER, INTENT(IN OUT) :: harvest_counter_cpft(land_pts,ncpft)

!ancil_info (IN)
LOGICAL, INTENT(IN) :: l_lice_point(land_pts)
LOGICAL, INTENT(IN OUT) :: l_lice_surft(ntype)

! Local variables
INTEGER :: n                            ! Crop tile number
INTEGER :: m                            ! Soil tile number
INTEGER :: i,j,l                        ! Counters
INTEGER :: surft_pts(ntype)             ! Number of land points which
                                        ! include the nth surface type.
INTEGER :: surft_index(land_pts,ntype)  ! Indices of land points which
                                        ! include the nth surface type.

INTEGER ::  day        ! Current day
INTEGER ::  month      ! Current month
INTEGER ::  year       ! Current year
INTEGER ::  day_of_yr  ! Current day of year
INTEGER ::  days_in_yr ! Total number of days in current year
INTEGER ::  day_before_sowdate ! Day of year for day just before sowing date

REAL(KIND=real_jlslsm) :: nonyield_diag_dummy = 0.0
                                   ! Dummy variable used when making
     ! sure that crop is consistent at initialisation if it's not sown.

REAL(KIND=real_jlslsm) :: dvi_dummy = 0.0
                         ! Dummy variable used to make sure dvi is not
                         ! overwritten when initialising crop at beginning
                         ! of run.

REAL(KIND=real_jlslsm) :: dvi_prev1 ! variable to store previous value of DVI
REAL(KIND=real_jlslsm) :: dvi_prev2 ! variable to store previous value of DVI

!-----------------------------------------------------------------------------
! Create the surft_index array of land points with each surface type
!-----------------------------------------------------------------------------
! NB: tilepts loops over all land surface types
CALL tilepts(land_pts, frac, surft_pts, surft_index, l_lice_point, l_lice_surft)

!-----------------------------------------------------------------------------
! Initialise two of the crop diagnostics at start of run
!-----------------------------------------------------------------------------
IF ( a_step == 1 ) THEN
  DO  n = 1,ncpft
    DO j = 1,surft_pts(nnpft + n)
      l = surft_index(j,nnpft + n)
      IF ( dvi(l,n) < initial_c_dvi(n) ) THEN
        CALL reset_crop(n, nonyield_diag_dummy, dvi_dummy,                     &
                        lai(l,n + nnpft), canht(l,n + nnpft),                  &
                        rootc(l,n), harvc(l,n), reservec(l,n),                 &
                        stemc_diag_cpft(l,n), leafc_diag_cpft(l,n))
      ELSE
        !------------------------------------------------
        ! Derive stem and leaf pools from prognostics
        !------------------------------------------------
        stemc_diag_cpft(l,n) = stemc_from_prognostics(n, canht(l,n + nnpft))
        leafc_diag_cpft(l,n) = leafc_from_prognostics(n, dvi(l,n),             &
                                                      lai(l,n + nnpft))
      END IF
    END DO
  END DO
END IF

IF ( l_prescsow ) THEN
  ! Get the current model year, month and day
  CALL current_model_time(year, month, day)

  day_of_yr  = day_of_year(year, month, day, l_360, l_leap)
  days_in_yr = days_in_year(year, l_360, l_leap)
END IF

!-----------------------------------------------------------------------------
! Loop over crop tiles and sow/grow/harvest the crop
!-----------------------------------------------------------------------------
DO  n = 1,ncpft

  !===========================================================================
  ! *NOTICE REGARDING SOIL TILING**
  !
  !The following section facilitates the use of soil tiling. As implemented,
  !there are two soil tiling options:
  !
  !nsoilt == 1
  !Operate as with a single soil tile, functionally identical to JULES upto
  ! at least vn4.7 (Oct 2016)
  ! This means that a soilt variable being passed 'up' to the surface is
  ! broadcast to the surft variable (with weighting by frac if requred)
  !
  !nsoilt > 1
  !Operate with nsoilt = nsurft, with a direct mapping between them
  ! This means that a soilt variable being passed 'up' to the surface is
  ! simply copied into the surft variable
  !
  ! This will need to be refactored for other tiling approaches. This note
  ! will be replicated elsewhere in the code as required
  !
  !These comments apply until **END NOTICE REGARDING SOIL TILING**
  !===========================================================================
    !Set the current soil tile (see notice above)
  IF (nsoilt == 1) THEN
    !There is only 1 soil tile
    m = 1
  ELSE ! nsoilt == nsurft
    !Soil tiles map directly on to surface tiles, rembmering we're working on
    !crop tiles
    m = nnpft + n
  END IF !nsoilt
  !===========================================================================
  ! *END NOTICE REGARDING SOIL TILING**
  !===========================================================================

  !---------------------------------------------------------------------------
  ! Loop over crop tile points
  !---------------------------------------------------------------------------
  DO j = 1,surft_pts(nnpft + n)

    l = surft_index(j,nnpft + n)
    i = land_index(l)

    !reset yield and non-yield carbon
    yield_diag_cpft(l,n)      = 0.0
    harvest_trigger_cpft(l,n) = 0
    harvest_counter_cpft(l,n) = 0
    nonyield_diag_cpft(l,n)   = 0.0

    IF ( NINT(dvi(l,n)) == -2 .AND. crop_call == 0 ) THEN
      CALL sow(n, sm_levels, t_soil_soilt(l,m,:), sthu_soilt(l,m,:),           &
               smvcst_soilt(l,m,:),                                            &
               smvccl_soilt(l,m,:), dphotdt(i), sow_date_cpft(l,n),            &
               dvi(l,n))
    END IF

    !-------------------------------------------------------------------------
    ! If crop is sown, increment DVI.
    !-------------------------------------------------------------------------

    dvi_prev1 = dvi(l,n)

    IF ( dvi(l,n) >= -1.0 .AND. dvi(l,n) < 0.0 ) THEN
      CALL emerge(n, t_surft(l,n + nnpft), dvi(l,n))
      ! On the timestep where crop emerges, DVI is incremented twice
      ! This should be changed (kept in for the moment for bit
      ! compatibility in the rose stem tests).
    END IF

    ! This variable is currently needed for bit compatibility, but could
    ! be removed if DVI was not incremented twice on the timestep crop
    ! emerges
    dvi_prev2 = dvi(l,n)

    IF ( dvi(l,n) >= 0.0 ) THEN
      CALL develop(n, t_surft(l,n + nnpft), phot(i), tt_veg_cpft(l,n),         &
                   tt_rep_cpft(l,n), dvi(l,n))
    END IF

    !-------------------------------------------------------------------------
    ! Check whether this is where DVI crosses from below initial_c_dvi
    ! to above. If it is, initialise the crop.
    !-------------------------------------------------------------------------
    IF ( dvi_prev1 <= initial_c_dvi(n) .AND.                                   &
         dvi(l,n) >= initial_c_dvi(n) ) THEN

      CALL initialise_crop(n, dvi_prev2,                                       &
                           rootc(l,n), harvc(l,n),                             &
                           stemc_diag_cpft(l,n), leafc_diag_cpft(l,n))
    END IF

    !-------------------------------------------------------------------------
    ! If crop has been initialised, partition the NPP, update the diagnostics,
    ! check harvest conditions.
    !-------------------------------------------------------------------------
    IF ( dvi(l,n) >= initial_c_dvi(n) ) THEN
      IF (crop_call == 0) THEN
        CALL partition(n, npp_ft_acc(l,n + nnpft),                             &
                       dvi(l,n), rootc(l,n), harvc(l,n),                       &
                       reservec(l,n), nonyield_diag_cpft(l,n),                 &
                       stemc_diag_cpft(l,n), leafc_diag_cpft(l,n),             &
                       harvest_counter_cpft(l,n), harvest_trigger_cpft(l,n))
        npp_ft_acc(l,n + nnpft) = 0.0
      END IF

      !-----------------------------------------------------------------------
      ! Derive new prognostics from carbon pools and DVI
      !-----------------------------------------------------------------------
      lai(l,n + nnpft)   = lai_from_leafc(n, dvi(l,n), leafc_diag_cpft(l,n))
      canht(l,n + nnpft) = canht_from_stemc(n, stemc_diag_cpft(l,n))

      IF ( dvi(l,n) >= 2.0 ) THEN
        harvest_counter_cpft(l,n) = 1
        harvest_trigger_cpft(l,n) = 1
      ELSE IF ( lai(l,n + nnpft) >= 15.0 ) THEN
        harvest_counter_cpft(l,n) = 1
        harvest_trigger_cpft(l,n) = 2
      ELSE IF ( dvi(l,n) > 1.0 .AND. t_soil_soilt(l,m,2) < t_mort(n) ) THEN
        harvest_counter_cpft(l,n) = 1
        harvest_trigger_cpft(l,n) = 3
        ! harvest_trigger_cpft = 4 is set in partition.F90
      ELSE IF ( l_prescsow ) THEN
        day_before_sowdate = NINT(sow_date_cpft(l,n)) - 1
        IF ( day_before_sowdate == 0 ) THEN
          day_before_sowdate = days_in_yr
        END IF
        IF (day_of_yr == day_before_sowdate .OR. day_of_yr ==                  &
                  latestharv_date_cpft(l,n)) THEN
          harvest_counter_cpft(l,n) = 1
          harvest_trigger_cpft(l,n) = 5
        END IF
      END IF

      IF ( harvest_counter_cpft(l,n) == 1 ) THEN
        yield_diag_cpft(l,n) = yield_frac(n) * harvc(l,n) - cropharvc_min
        harvc(l,n)           = harvc(l,n) - yield_diag_cpft(l,n)
        dvi(l,n)             = -2.0
      END IF

      IF ( NINT(dvi(l,n)) == -2 ) THEN
        CALL reset_crop(n, nonyield_diag_cpft(l,n), dvi(l,n),                  &
                        lai(l,n + nnpft), canht(l,n + nnpft),                  &
                        rootc(l,n), harvc(l,n), reservec(l,n),                 &
                        stemc_diag_cpft(l,n), leafc_diag_cpft(l,n))
      END IF

    END IF  !DVI gt 0.0

    croplai(l,n)   = lai(l,n + nnpft)
    cropcanht(l,n) = canht(l,n + nnpft)

    IF (crop_call == 0) THEN
      nonyield_diag_cpft(l,n) = nonyield_diag_cpft(l,n) +                      &
                                npp_ft_acc(l,n + nnpft)
      npp_ft_acc(l,n + nnpft) = 0.0
    END IF

  END DO  ! surft_pts

  !---------------------------------------------------------------------------
  ! Update canopy capacity and roughness length
  !---------------------------------------------------------------------------
  CALL pft_sparm(land_pts, n + nnpft, surft_pts(n + nnpft),                    &
                 surft_index(:,n + nnpft), canht(:,n + nnpft),                 &
                 lai(:,n + nnpft), catch_t(:,n + nnpft), z0_t(:,n + nnpft))

END DO  !  crop PFTs

END SUBROUTINE crop
END MODULE crop_mod
