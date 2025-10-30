!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE soil_inorg_n_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Routines concerning soil inorganic nitrogen.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGEOCHEMISTRY
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Private scope by default.
PRIVATE
PUBLIC soil_n_deposition, soil_n_fertiliser, soil_n_fixation, soil_n_uptake

! This code does not fully support soil tiling (nor does the rest of the
! JULES biogeochemistry code). Here we hardwire a soil tile index.
INTEGER, PARAMETER :: s = 1

CONTAINS

!#############################################################################

SUBROUTINE soil_n_deposition( land_pts, trif_pts, r_gamma, trif_index,         &
                              n_inorg_avail_pft, n_inorg_soilt_lyrs,           &
                              deposition,                                      &
                          ! These arguments replace USE statements
                              !trif_vars_mod
                              deposition_n_gb)

! Description:
!   Add nitrogen deposition to the soil and make it available to plants.

USE ancil_info, ONLY:                                                          &
  ! imported scalar variables
  dim_cslayer, nsoilt

USE jules_soil_biogeochem_mod, ONLY:                                           &
  ! imported scalar parameters
  soil_model_4pool,                                                            &
  ! imported scalar variables
  l_layeredc, soil_bgc_model

USE jules_surface_types_mod, ONLY:                                             &
  ! imported scalar variables
  npft

USE veg_param, ONLY:                                                           &
  ! imported parameters
  secs_per_360days

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Total number of land points.
  trif_pts
    ! Number of points on which TRIFFID may operate.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  r_gamma
    ! Inverse timestep (/360days).

INTEGER, INTENT(IN) ::                                                         &
  trif_index(land_pts)
    ! Indices of land points on which TRIFFID may operate.

!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  n_inorg_avail_pft(land_pts,npft,dim_cslayer),                                &
    ! Availabile inorganic N for PFTs (kg m-2).
  n_inorg_soilt_lyrs(land_pts,nsoilt,dim_cslayer)
    ! Inorganic N pool on soil levels (kg m-2).

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  deposition(land_pts)
    ! N deposition (kg m-2 [360days]-1).

!-----------------------------------------------------------------------------
! These arguments replace USE statements.
!-----------------------------------------------------------------------------
!trif_vars_mod
REAL(KIND=real_jlslsm), INTENT(IN) :: deposition_n_gb(land_pts)

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  l, n, t
    !  Indices.

REAL(KIND=real_jlslsm) ::                                                      &
  scale_to_step
    ! Scaling from 360 days to the TRIFFID timestep.

!-----------------------------------------------------------------------------
!end of header

IF ( soil_bgc_model == soil_model_4pool ) THEN

  ! Calculate time factor.
  scale_to_step = 1.0 / r_gamma

  DO t = 1,trif_pts
    l = trif_index(t)

    ! Convert deposition from kg m-2 s-1 to kg m-2 per 360days.
    ! Note that deposition_N is the instantaneous rate. Any variation on
    ! timescales shorter than triffid_period will be lost.
    deposition(l) = MAX(deposition_n_gb(l) * secs_per_360days, 0.0)

    !-------------------------------------------------------------------------
    ! Add deposition to the soil and make it available for vegetation uptake
    ! (from the top layer only).
    !-------------------------------------------------------------------------
    n_inorg_soilt_lyrs(l,s,1) = n_inorg_soilt_lyrs(l,s,1) + deposition(l)      &
                                * scale_to_step
    IF (l_layeredc) THEN
      DO n = 1,npft
        n_inorg_avail_pft(l,n,1) = n_inorg_avail_pft(l,n,1)                    &
                                   + deposition(l) * scale_to_step
      END DO
    END IF

  END DO  !  points
END IF  !  soil_bgc_model = 4-pool soil C

END SUBROUTINE soil_n_deposition

!#############################################################################

SUBROUTINE soil_n_fixation( land_pts, trif_pts, r_gamma, have_layers,          &
                            trif_index, frac, f_root_pft, isunfrozen, npp,     &
                            n_inorg_avail_pft, n_inorg_soilt_lyrs, n_fix_gb,   &
                            n_fix_add, n_fix_pft)

! Description:
!   Calculate nitrogen fixation by plants and make it available to plants.

USE ancil_info, ONLY:                                                          &
  ! imported scalar variables
  dim_cslayer, nsoilt

USE jules_soil_biogeochem_mod, ONLY:                                           &
  ! imported scalar parameters
  soil_model_ecosse, soil_model_4pool,                                         &
  ! imported scalar variables
  soil_bgc_model

#if !defined(UM_JULES)
USE jules_soil_ecosse_mod, ONLY:                                               &
  ! imported scalar variables
  l_soil_N
#endif

USE jules_surface_types_mod, ONLY:                                             &
  ! imported scalar variables
  npft, ntype

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Total number of land points.
  trif_pts
    ! Number of points on which TRIFFID may operate.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  r_gamma
    ! Inverse timestep (/360days).

LOGICAL, INTENT(IN) ::                                                         &
  have_layers
    !  T if we have multiple soil layers, else F.

INTEGER, INTENT(IN) ::                                                         &
  trif_index(land_pts)
    ! Indices of land points on which TRIFFID may operate.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  frac(land_pts,ntype),                                                        &
    ! Fractional cover of each surface type.
  f_root_pft(npft,dim_cslayer),                                                &
    ! Root fraction in each soil layer per PFT.
 isunfrozen(land_pts,dim_cslayer),                                             &
    ! Matrix to mask out frozen layers (inaccessible to plants).
 npp(land_pts,npft)
    ! Net primary productivity (kg C/m2/360days).

!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  n_inorg_avail_pft(land_pts,npft,dim_cslayer),                                &
    ! Availabile inorganic N for PFTs (kg m-2).
  n_inorg_soilt_lyrs(land_pts,nsoilt,dim_cslayer)
    ! Inorganic N pool on soil levels (kg m-2).

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  n_fix_gb(land_pts),                                                          &
    ! N fixed by plants (kg m-2 [360days]-1).
  n_fix_add(land_pts,nsoilt,dim_cslayer),                                      &
  n_fix_pft(land_pts,npft)

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  npp_to_nfix = 1.6 / 1.0e3
    ! Constant relating N fixation (g of N) to NPP (kg of C) [g N (kg C)-1.].

#if defined(UM_JULES)
LOGICAL, PARAMETER ::                                                          &
  l_soil_N = .TRUE.  ! Flag used with ECOSSE and therefore not currently
                     ! used in the UM.
#endif

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  iz, l, n, t
    !  Indices.

REAL(KIND=real_jlslsm) ::                                                      &
  n_fix_denom,                                                                 &
    ! Denominator for fixation calculation if we have multiple soil layers.
  scale_to_step
    ! Scaling from 360 days to the TRIFFID timestep.

REAL(KIND=real_jlslsm) ::                                                      &
  n_avail_delta(land_pts,npft,dim_cslayer)
    ! Change in available N (kg m-2).

!-----------------------------------------------------------------------------
!end of header

!-----------------------------------------------------------------------------
! Initialisation.
!-----------------------------------------------------------------------------
n_fix_add(:,:,:) = 0.0
n_fix_gb(:)      = 0.0

IF ( soil_bgc_model == soil_model_4pool .OR.                                   &
     ( soil_bgc_model == soil_model_ecosse .AND. l_soil_N ) ) THEN

  ! Initialise increment.
  n_avail_delta(:,:,:) = 0.0

  ! Calculate time factor.
  scale_to_step = 1.0 / r_gamma

  DO t = 1,trif_pts
    l = trif_index(t)

    ! Calculate fixation.
    DO n = 1,npft
      n_fix_pft(l,n) = MAX(npp(l,n) * npp_to_nfix, 0.0)
      n_fix_gb(l)    = n_fix_gb(l) + frac(l,n) * n_fix_pft(l,n)
    END DO

    !-------------------------------------------------------------------------
    ! Calculate nitrogen added to each layer by fixation.
    ! If layered, nitrogen fixation is distributed according to root profile
    ! and whether layer is unfrozen.
    !-------------------------------------------------------------------------
    IF ( have_layers ) THEN

      IF (SUM(isunfrozen(l,:)) > 0.0) THEN
        DO n = 1,npft
          n_fix_denom = 1.0                                                    &
                        / ( r_gamma * SUM(isunfrozen(l,:) * f_root_pft(n,:)) )
          DO iz = 1,dim_cslayer
            n_avail_delta(l,n,iz) = n_fix_pft(l,n)                             &
                                  * f_root_pft(n,iz) * isunfrozen(l,iz)        &
                                  * n_fix_denom
            n_fix_add(l,1,iz)   = n_fix_add(l,1,iz)                            &
                                  + n_avail_delta(l,n,iz) * frac(l,n)
          END DO
        END DO
      ELSE
        ! Whole soil is frozen, just put all N in the top layer.
        n_avail_delta(l,:,1) = n_fix_pft(l,:) * scale_to_step
        n_fix_add(l,1,1)     = n_fix_gb(l) * scale_to_step
      END IF

    ELSE
      ! .NOT. have_layers
      ! Add to the only layer.
      n_fix_add(l,1,1) = n_fix_gb(l) * scale_to_step

    END IF  !  have_layers

    !-------------------------------------------------------------------------
    ! Add fixation to the soil and make it available for vegetation uptake.
    !-------------------------------------------------------------------------
    n_inorg_soilt_lyrs(l,s,:) = n_inorg_soilt_lyrs(l,s,:) + n_fix_add(l,1,:)
    IF ( have_layers) THEN
      n_inorg_avail_pft(l,:,:) = n_inorg_avail_pft(l,:,:)                      &
                                 + n_avail_delta(l,:,:)
    END IF

  END DO  !  points
END IF  !  soil_bgc_model

END SUBROUTINE soil_n_fixation

!#############################################################################

SUBROUTINE soil_n_fertiliser( land_pts, trif_pts, fert_layer, r_gamma,         &
                              have_layers,                                     &
                              trif_index, dz, frac_flux, f_root_pft,           &
                              f_root_pft_dz, n_fertiliser_pft,                 &
                              n_inorg_avail_pft, n_inorg_soilt_lyrs,           &
                              n_fertiliser_gb, n_fertiliser_add )

! Description:
!   Make any nitrogen fertiliser available to plants.

USE ancil_info, ONLY:                                                          &
  ! imported scalar variables
  dim_cslayer, nsoilt

USE jules_soil_biogeochem_mod, ONLY:                                           &
  ! imported scalar parameters
  soil_model_ecosse, soil_model_4pool,                                         &
  ! imported scalar variables
  soil_bgc_model

#if !defined(UM_JULES)
USE jules_soil_ecosse_mod, ONLY:                                               &
  ! imported scalar variables
  l_soil_N
#endif

USE jules_surface_types_mod, ONLY:                                             &
  ! imported scalar variables
  npft

USE jules_vegetation_mod, ONLY:                                                &
  ! imported scalar variables
  l_trif_crop

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Total number of land points.
  trif_pts,                                                                    &
    ! Number of points on which TRIFFID may operate.
  fert_layer
    ! Deepest soil level to which fertiliser is applied.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  r_gamma
    ! Inverse timestep (/360days).

LOGICAL, INTENT(IN) ::                                                         &
  have_layers
    !  T if we have multiple soil layers, else F.

INTEGER, INTENT(IN) ::                                                         &
  trif_index(land_pts)
    ! Indices of land points on which TRIFFID may operate.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  dz(dim_cslayer),                                                             &
    ! Soil carbon layer thicknesses (m).
  frac_flux(land_pts,npft),                                                    &
    ! PFT fraction to be used in the calculation of the gridbox mean fluxes.
  f_root_pft(npft,dim_cslayer),                                                &
    ! Root fraction in each soil layer per PFT.
  f_root_pft_dz(npft,dim_cslayer),                                             &
    ! Normalised roots in each soil layer per PFT.
 n_fertiliser_pft(land_pts,npft)
    ! Nitrogen available to crop PFTs in addition to soil nitrogen
    ! (kg m-2 yr-1).

!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  n_inorg_avail_pft(land_pts,npft,dim_cslayer),                                &
    ! Availabile inorganic N for PFTs (kg m-2).
  n_inorg_soilt_lyrs(land_pts,nsoilt,dim_cslayer)
    ! Inorganic N pool on soil levels (kg m-2).

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  n_fertiliser_gb(land_pts),                                                   &
    ! N available to crop PFTs in addition to soil N: gridbox mean
    ! (kg m-2 yr-1).
  n_fertiliser_add(land_pts, nsoilt, dim_cslayer)

#if defined(UM_JULES)
!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
LOGICAL, PARAMETER ::                                                          &
  l_soil_N = .TRUE.  ! Flag used with ECOSSE and therefore not currently
                     ! used in the UM.
#endif

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  l, n, t
    !  Indices.

REAL(KIND=real_jlslsm) ::                                                      &
  scale_to_step
    ! Scaling from 360 days to the TRIFFID timestep.

REAL(KIND=real_jlslsm) ::                                                      &
  add_weight(dim_cslayer),                                                     &
    ! Layer weights used in calculation of n_fertiliser_add (the N added to
    ! each layer).
  delta_weight(npft,dim_cslayer)
    ! Layer weights used in calculation of increment to n_inorg_avail_pft.

!-----------------------------------------------------------------------------
!end of header

!-----------------------------------------------------------------------------
! Initialisation.
!-----------------------------------------------------------------------------
n_fertiliser_add(:,:,:) = 0.0
n_fertiliser_gb(:)      = 0.0

!-----------------------------------------------------------------------------
! If we are not using l_trif_crop there is nothing more to do here as there is
! no consideration of fertiliser.
!-----------------------------------------------------------------------------
IF ( .NOT. l_trif_crop ) RETURN

! Calculate time factor.
scale_to_step = 1.0 / r_gamma

IF ( soil_bgc_model == soil_model_4pool .OR.                                   &
     ( soil_bgc_model == soil_model_ecosse .AND. l_soil_N ) ) THEN

  !---------------------------------------------------------------------------
  ! Calculate the fertiliser requirement at the gridbox level
  !---------------------------------------------------------------------------
  DO t = 1,trif_pts
    l = trif_index(t)
    DO n = 1,npft
      n_fertiliser_gb(l) = n_fertiliser_gb(l) +                                &
                           frac_flux(l,n) * n_fertiliser_pft(l,n)
    END DO
  END DO

  ! Calculate layer weights.
  ! Only the values for layers 1:fert_layer are used.
  IF ( have_layers ) THEN
    add_weight(1:fert_layer) =  dz(1:fert_layer)                               &
                                / ( SUM(dz(1:fert_layer)) * r_gamma )
    ! Fertiliser is made available to plants depending on layer thickness and
    ! root fraction.
    IF ( soil_bgc_model == soil_model_4pool ) THEN
      ! In general this form means that not all of the fertiliser is made
      ! available to plants (because of the factor f_root_pft_dz).
      DO n = 1,npft
        delta_weight(n,1:fert_layer) = ( dz(1:fert_layer)                      &
                                         * f_root_pft_dz(n,1:fert_layer) )     &
                                       / ( SUM(dz(1:fert_layer)) * r_gamma )
      END DO
    ELSE IF ( soil_bgc_model == soil_model_ecosse .AND. l_soil_N ) THEN
      ! All of the fertiliser is made available.
      DO n = 1,npft
        delta_weight(n,1:fert_layer) = ( dz(1:fert_layer)                      &
                                         * f_root_pft(n,1:fert_layer) )        &
                                       / ( SUM( dz(1:fert_layer)               &
                                           * f_root_pft(n,1:fert_layer) )      &
                                           * r_gamma )
      END DO
    END IF  !  soil_bgc_model

  ELSE

    ! .NOT. have_layers
    ! All fertiliser will be added to the only layer.
    ! This sets weight to 1 * timestep factor.
    add_weight(1) = scale_to_step

  END IF  !  have_layers

  !-------------------------------------------------------------------------
  ! Calculate nitrogen added to each layer by fertiliser.
  !-------------------------------------------------------------------------
  DO t = 1,trif_pts
    l = trif_index(t)

    n_fertiliser_add(l,1,1:fert_layer) = n_fertiliser_gb(l)                    &
                                         * add_weight(1:fert_layer)

    ! Add to the soil.
    n_inorg_soilt_lyrs(l,s,1:fert_layer) =                                     &
         n_inorg_soilt_lyrs(l,s,1:fert_layer)                                  &
         + n_fertiliser_add(l,1,1:fert_layer)

  END DO

  !-------------------------------------------------------------------------
  ! Make fertiliser available to the vegetation.
  !-------------------------------------------------------------------------
  IF ( have_layers ) THEN
    DO t = 1,trif_pts
      l = trif_index(t)
      DO n = 1,npft
        n_inorg_avail_pft(l,n,1:fert_layer) =                                  &
          n_inorg_avail_pft(l,n,1:fert_layer)                                  &
          +  n_fertiliser_pft(l,n) * delta_weight(n,1:fert_layer)
      END DO
    END DO
  END IF  !  have_layers

END IF  !  soil_bgc_model

END SUBROUTINE soil_n_fertiliser

!#############################################################################

SUBROUTINE soil_n_uptake( land_pts, trif_pts, r_gamma, have_layers,            &
                          trif_index, frac_flux, f_root_pft, isunfrozen,       &
                          n_inorg_avail_pft, n_inorg_soilt_lyrs,               &
                          n_uptake_pft, n_uptake_gb, n_uptake_extract,         &
                      ! These arguments replace USE statements
                          !prognostics
                          n_inorg_gb )

! Description:
!   Limit uptake of nitrogen by plants to the available soil pool and
!   calculate the change in the pool.

USE ancil_info, ONLY:                                                          &
  ! imported scalar variables
  dim_cslayer, nsoilt

USE ereport_mod, ONLY:                                                         &
  ! imported procedures
  ereport

USE jules_soil_biogeochem_mod, ONLY:                                           &
  ! imported scalar parameters
  soil_model_ecosse, soil_model_4pool,                                         &
  ! imported scalar variables
  soil_bgc_model

#if !defined(UM_JULES)
USE jules_soil_ecosse_mod, ONLY:                                               &
  ! imported scalar variables
  l_soil_N
#endif

USE jules_surface_types_mod, ONLY:                                             &
  ! imported scalar variables
  npft

USE jules_vegetation_mod, ONLY:                                                &
  ! imported scalar variables
  l_nitrogen

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Total number of land points.
  trif_pts
    ! Number of points on which TRIFFID may operate.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  r_gamma
    ! Inverse timestep (/360days).

LOGICAL, INTENT(IN) ::                                                         &
  have_layers
    !  T if we have multiple soil layers, else F.

INTEGER, INTENT(IN) ::                                                         &
  trif_index(land_pts)
    ! Indices of land points on which TRIFFID may operate.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  frac_flux(land_pts,npft),                                                    &
    ! PFT fraction to be used in the calculation of the gridbox mean fluxes.
  f_root_pft(npft,dim_cslayer),                                                &
    ! Root fraction in each soil layer per PFT.
  isunfrozen(land_pts,dim_cslayer)
    ! Matrix to mask out frozen layers (inaccessible to plants).

!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  n_inorg_avail_pft(land_pts,npft,dim_cslayer),                                &
    ! Availabile inorganic N for PFTs (kg m-2).
  n_inorg_soilt_lyrs(land_pts,nsoilt,dim_cslayer),                             &
    ! Inorganic N pool on soil levels (kg m-2).
  n_uptake_pft(land_pts,npft)
    ! N uptake by vegetation, on PFTs (kg m-2 [360day]-1).

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  n_uptake_gb(land_pts),                                                       &
    ! Vegetation N uptake (kg/m2/360 days).
  n_uptake_extract(land_pts,nsoilt,dim_cslayer)

!-----------------------------------------------------------------------------
! New Arguments to replce USE statements
!-----------------------------------------------------------------------------
! prognostics
REAL(KIND=real_jlslsm), INTENT(OUT) ::n_inorg_gb(land_pts)

#if defined(UM_JULES)
!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
LOGICAL, PARAMETER ::                                                          &
  l_soil_N = .TRUE.  ! Flag used with ECOSSE and therefore not currently
                     ! used in the UM.
#endif

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  error_status,                                                                &
    ! Error status.
  l, n, t
    ! Indices.

REAL(KIND=real_jlslsm) ::                                                      &
  scale_to_step
    ! Scaling from 360 days to the TRIFFID timestep.

REAL(KIND=real_jlslsm) ::                                                      &
  n_avail_delta(land_pts,npft,dim_cslayer)
    ! Change in available N (kg m-2).

CHARACTER(LEN=*), PARAMETER :: RoutineName='SOIL_N_UPTAKE'

!-----------------------------------------------------------------------------
!end of header

!-----------------------------------------------------------------------------
! Initialisation.
!-----------------------------------------------------------------------------
n_avail_delta(:,:,:)    = 0.0
n_uptake_extract(:,:,:) = 0.0
n_uptake_gb(:)          = 0.0

! Calculate time factor.
scale_to_step = 1.0 / r_gamma

!-----------------------------------------------------------------------------
! Limit the N uptake to the size of pool. Additional demand is to
! maintain frac_min and LAI_min will be later be taken as negative litter N.
!-----------------------------------------------------------------------------
DO t = 1,trif_pts
  l = trif_index(t)

  DO n = 1,npft

    !-------------------------------------------------------------------------
    ! Calculate the inorganic N.
    !-------------------------------------------------------------------------
    IF ( have_layers ) THEN
      ! We don't have a gridbox variable in this case.
      n_inorg_gb(l) = SUM(n_inorg_avail_pft(l,n,:) * isunfrozen(l,:))
    ELSE
      n_inorg_gb(l) = n_inorg_soilt_lyrs(l,s,1)
    END IF

    !-------------------------------------------------------------------------
    ! If required N uptake to keep frac_min is in excess of n_inorg_soilt on
    ! tile, force the model to meet this through negative N uptake at the PFT
    ! level.
    !-------------------------------------------------------------------------
    IF ( l_nitrogen .AND.                                                      &
         n_uptake_pft(l,n) * scale_to_step > n_inorg_gb(l) ) THEN
      n_uptake_pft(l,n) = n_inorg_gb(l) * r_gamma
    END IF

    !-------------------------------------------------------------------------
    ! Calculate nitrogen removed from each layer by plant uptake.
    !-------------------------------------------------------------------------
    IF ( soil_bgc_model == soil_model_4pool .OR.                               &
         ( soil_bgc_model == soil_model_ecosse .AND. l_soil_N ) ) THEN

      IF (have_layers) THEN

        IF (n_inorg_gb(l) > 0.0) THEN

          n_avail_delta(l,n,:) = ( n_uptake_pft(l,n)                           &
                                 * n_inorg_avail_pft(l,n,:)                    &
                                 * isunfrozen(l,:) )                           &
                               / ( r_gamma * n_inorg_gb(l) )
          n_uptake_extract(l,1,:) = n_uptake_extract(l,1,:)                    &
                                    + n_avail_delta(l,n,:) * frac_flux(l,n)

        ELSE IF (n_inorg_gb(l) == 0.0) THEN

          IF (n_uptake_pft(l,n) * frac_flux(l,n) < 0.0) THEN
            n_uptake_extract(l,1,:) = n_uptake_extract(l,1,:)                  &
                                      + n_uptake_pft(l,n) * frac_flux(l,n)     &
                                        * f_root_pft(n,:) * scale_to_step
            n_avail_delta(l,n,:) = n_uptake_pft(l,n) * f_root_pft(n,:)         &
                                   * scale_to_step
          ELSE IF (n_uptake_pft(l,n) * frac_flux(l,n) > 0.0) THEN
            n_uptake_pft(l,n) = 0.0
            IF (l_nitrogen) THEN
              error_status = -101
              CALL ereport(RoutineName, error_status,                          &
                           "trying to uptake N but none available")
            END IF
          END IF

        ELSE
          ! n_inorg_gb < 0
          error_status = -101
          CALL ereport(RoutineName, error_status,                              &
                       "n_inorg_gb is negative")

        END IF  !  n_inorg_gb

      END IF  !  have_layers

      ! Update gridbox uptake.
      n_uptake_gb(l) = n_uptake_gb(l) + frac_flux(l,n) * n_uptake_pft(l,n)

    END IF  !  soil_bgc_model

  END DO  !  PFTs

  IF ( .NOT. have_layers) THEN
    n_uptake_extract(l,1,1) = n_uptake_gb(l) * scale_to_step
  END IF

  IF ( soil_bgc_model == soil_model_4pool ) THEN
    !-------------------------------------------------------------------------
    ! Remove plant uptake from soil stores.
    ! This needn't be done for ECOSSE as neither variable is used again.
    !-------------------------------------------------------------------------
    n_inorg_soilt_lyrs(l,s,:) = n_inorg_soilt_lyrs(l,s,:)                      &
                                - n_uptake_extract(l,1,:)
    IF ( have_layers) THEN
      n_inorg_avail_pft(l,:,:) = n_inorg_avail_pft(l,:,:)                      &
                                 - n_avail_delta(l,:,:)
    END IF
  END IF  !  soil_bgc_model

END DO  !  trif_pts

END SUBROUTINE soil_n_uptake

END MODULE soil_inorg_n_mod
