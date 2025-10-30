#if !defined(UM_JULES)
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

MODULE ecosse_leaching_mod

!-----------------------------------------------------------------------------
! Description:
!   Calculates leaching from the ECOSSE model.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGEOCHEMISTRY
!
! Code Description:
!   Language: Fortran 90.
!-----------------------------------------------------------------------------

USE ancil_info, ONLY:                                                          &
  ! imported scalars
  nz_soilc=>dim_cslayer

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

PRIVATE
PUBLIC calc_leaching

CONTAINS

!#############################################################################
!#############################################################################

SUBROUTINE calc_leaching( land_pts, vs_pts, vs_index,                          &
                     residual_N, water_flux_down, water_flux_lateral,          &
                     smvc, n_amm, n_nit,                                       &
                     n_leach_amm, n_leach_nit )


USE ecosse_param_mod, ONLY:                                                    &
  ! imported scalars
  amm_leach_min

USE ecosse_utils_mod, ONLY:                                                    &
  ! imported parameters
  nt

USE jules_soil_ecosse_mod, ONLY:                                               &
  ! imported scalars
  l_soil_N,                                                                    &
  ! imported arrays
  dz_soilc

! Description:
!   Top-level code to call leaching routine.

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Number of land points.
  vs_pts
    ! The number of points with veg and/or soil.

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) :: vs_index(land_pts)
    ! Indices of veg/soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  residual_N(land_pts,nz_soilc),                                               &
    ! N in minimum allowed (residual) nitrate or ammonium amount (kg m-2).
  water_flux_down(land_pts,0:nz_soilc),                                        &
    ! Water drained through bottom of each layer (kg m-2 s-1).
  water_flux_lateral(land_pts,nz_soilc),                                       &
    ! Lateral flow of water from layer (kg m-2 s-1).
  smvc(land_pts,nz_soilc)
    ! Volumetric soil moisture content (1).

!-----------------------------------------------------------------------------
! Array arguments with intent(inout)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  n_amm(land_pts,nz_soilc,nt),                                                 &
    ! N in soil ammonium  (kg m-2).
  n_nit(land_pts,nz_soilc,nt)
    ! N in soil nitrate (kg m-2).

!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  n_leach_amm(land_pts),                                                       &
    ! N lost from column through leaching of ammonium (kg m-2 s-1).
  n_leach_nit(land_pts)
    ! N lost from column through leaching of nitrate (kg m-2 s-1).

!----------------------------------------------------------------------------
! Local arrays.
!----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  avail_store(land_pts,nz_soilc),                                              &
    ! Mass available for leaching, excluding any adsorbed fraction
    ! (kg m-2). This is expressed in terms of the mass of C or N.
  min_amm(land_pts,nz_soilc)
    ! Minimum allowed amount of N in soil ammonium (kg m-2).

!-----------------------------------------------------------------------------
!end of header

IF ( l_soil_N ) THEN
  !---------------------------------------------------------------------------
  ! Calculate leaching of ammonium and nitrate.
  !---------------------------------------------------------------------------

  !---------------------------------------------------------------------------
  ! Ammonium.
  !---------------------------------------------------------------------------
  ! Calculate ammonimum available for leaching.
  ! First calculate the minimum-allowed amount at each location.
  min_amm(:,:)     = MAX( amm_leach_min * SPREAD( dz_soilc(:),1,land_pts),     &
                          residual_N(:,:) )
  ! We use time level nt as this is only used to ensure that the final store
  ! is not over depleted.
  avail_store(:,:) = MAX( n_amm(:,:,nt) - min_amm(:,:), 0.0 )

  CALL leach( land_pts, vs_pts, vs_index,                                      &
              water_flux_down, water_flux_lateral,                             &
              smvc, avail_store, n_amm, n_leach_amm )

  !---------------------------------------------------------------------------
  ! Nitrate.
  !---------------------------------------------------------------------------
  ! Calculate nitrate available for leaching.
  ! We use time level nt as this is only used to ensure that the final store
  ! is not over depleted.
  avail_store(:,:) = MAX( n_nit(:,:,nt) - residual_N(:,:), 0.0 )

  CALL leach( land_pts, vs_pts, vs_index,                                      &
              water_flux_down, water_flux_lateral,                             &
              smvc, avail_store, n_nit, n_leach_nit )

END IF  !  l_soil_N

END SUBROUTINE calc_leaching

!#############################################################################
!#############################################################################

SUBROUTINE leach( land_pts, vs_pts, vs_index,                                  &
      water_flux_down, water_flux_lateral, smvc, avail_store, total_store,     &
      leached )

USE ecosse_utils_mod, ONLY:                                                    &
  ! imported parameters
  nt

USE jules_soil_ecosse_mod, ONLY:                                               &
  ! imported scalars
  dt_soilc,                                                                    &
  ! imported arrays
  dz_soilc

USE jules_hydrology_mod, ONLY:                                                 &
  ! imported scalars
  l_top

! Description:
!  Calculate leaching of an arbitrary solute.

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Local scalar parameters.
!-----------------------------------------------------------------------------
LOGICAL, PARAMETER ::                                                          &
  l_constant_grad = .TRUE.
    ! Switch controlling bottom boundary condition for vertical flux.
    ! This controls how concentration is extraplated to the bottom of the
    ! soil column.
    ! .TRUE. means a constant gradient.
    ! .FALSE. means a gradient of zero.
    ! Note this can likely be removed after testing has quantified the
    ! sensitivity of results to this setting.

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) :: land_pts  ! number of land points
INTEGER, INTENT(IN) :: vs_pts    ! the number of points with veg and/or soil

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) :: vs_index(land_pts)           ! indices of veg/soil

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  water_flux_down(land_pts,0:nz_soilc),                                        &
    ! Water draining through the bottom of each layer (kg m-2 s-1).
    ! Note that layer 0 (flux into top soil layer) is not currently used.
  water_flux_lateral(land_pts,nz_soilc),                                       &
    ! Lateral water flow from layer (kg m-2 s-1).
  smvc(land_pts,nz_soilc),                                                     &
    ! Volumetric soil moisture content (1).
  avail_store(land_pts,nz_soilc)
    ! Mass available for leaching, excluding adsorbed fraction (kg m-2).
    ! This is expressed in terms of the mass of C or N.

!-----------------------------------------------------------------------------
! Array arguments with intent(inout)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  total_store(land_pts,nz_soilc,nt)
    ! Storage in a soil layer, including adsorbed fraction (kg m-2).
    ! This is expressed in terms of the mass of C or N.

!-----------------------------------------------------------------------------
! Array arguments with intent(out).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) :: leached(land_pts)
    ! Solute loss through leaching (kg m-2 s-1).

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: iz     ! loop counter

!-----------------------------------------------------------------------------
! Local array variables.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  conc(land_pts,nz_soilc),                                                     &
    ! Concentration of solute in soil water (kg m-3).
  leached_lat(land_pts,nz_soilc),                                              &
    ! Loss through leaching in lateral fluxes (kg m-2 s-1).
  leached_vert(land_pts,0:nz_soilc)
    ! Amount leached through lower boundary of each layer (kg m-2 s-1).
    ! This is restricted to be >= 0, i.e. downwards (or zero).
    ! Includes space for zero flux into the top layer.

!-----------------------------------------------------------------------------
!end of header

!-----------------------------------------------------------------------------
! Initialise.
!-----------------------------------------------------------------------------
leached(:)       = 0.0
leached_lat(:,:) = 0.0

!-----------------------------------------------------------------------------
! Calculate concentration in soil water. Assuming the concentration in the
! liquid fraction does not vary with the amount of freezing, we use the total
! soil moisture amount here.
!-----------------------------------------------------------------------------
conc(:,:) = total_store(:,:,1)                                                 &
            / ( smvc(:,:) * SPREAD( dz_soilc(:),1,land_pts) )

!-----------------------------------------------------------------------------
! Calculate vertical solute fluxes between layers (and out of bottom layer).
!-----------------------------------------------------------------------------
! Apply a zero-flux top boundary condition.
leached_vert(:,0) = 0.0

! Calculate the flux from the bottom of each layer that has a layer below.
DO iz = 1,nz_soilc-1
  leached_vert(:,iz) = solute_flux_v_interp( land_pts, vs_pts,                 &
                                  dz_soilc(iz), dz_soilc(iz+1),                &
                                  vs_index,                                    &
                                  conc(:,iz), conc(:,iz+1),                    &
                                  water_flux_down(:,iz) )
END DO

! Calculate the flux from the bottom layer.
IF ( l_constant_grad .AND. nz_soilc > 1 ) THEN
  ! Constant gradient, with more than one layer.
  ! Extrapolate using the bottom two layers to estimate the
  ! concentrations at the bottom of the lowest (base) layer.
  leached_vert(:,nz_soilc) = solute_flux_v_extrap( land_pts, vs_pts,           &
                                dz_soilc(nz_soilc), dz_soilc(nz_soilc-1),      &
                                vs_index,                                      &
                                conc(:,nz_soilc), conc(:,nz_soilc-1),          &
                                water_flux_down(:,nz_soilc) )
ELSE
  ! No gradient of concentration (assume the concentration is constant through
  ! the layer. We acheive this by interpolating the concentration between
  ! layers, but pass the the concentration in the lowest layer twice to mimic
  ! no gradient.
  leached_vert(:,nz_soilc) = solute_flux_v_interp( land_pts, vs_pts,           &
                                dz_soilc(nz_soilc), dz_soilc(nz_soilc),        &
                                vs_index,                                      &
                                conc(:,nz_soilc), conc(:,nz_soilc),            &
                                water_flux_down(:,nz_soilc) )
END IF

!-----------------------------------------------------------------------------
! Calculate horizontal solute fluxes out of layers. These are zero except with
! l_top=T.
!-----------------------------------------------------------------------------
IF ( l_top ) THEN
  DO iz = 1,nz_soilc
    leached_lat(:,iz) = solute_flux_h( land_pts, vs_pts,                       &
                                       vs_index, conc(:,iz),                   &
                                       water_flux_lateral(:,iz) )
  END DO
END IF

!-----------------------------------------------------------------------------
! Update stores to account for leaching.
!-----------------------------------------------------------------------------
CALL extract_leached( land_pts, vs_pts, dt_soilc, vs_index,                    &
                      avail_store, leached_vert, leached_lat,                  &
                      total_store(:,:,nt), leached )

END SUBROUTINE leach

!#############################################################################
!#############################################################################

SUBROUTINE extract_leached( land_pts, vs_pts, dt, vs_index,                    &
                            avail_store, leached_vert, leached_lat,            &
                            total_store, leached )

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Number of land points.
  vs_pts
    ! Number of points with vegetation and/or soil

REAL(KIND=real_jlslsm), INTENT(IN) :: dt
    ! Timestep over which to integrate fluxes (s).

!-----------------------------------------------------------------------------
! Array arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  vs_index(land_pts)
    ! Indices of veg/soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  avail_store(land_pts,nz_soilc)
    ! Mass available for leaching, excluding adsorbed fraction (kg m-2).

!-----------------------------------------------------------------------------
! Array arguments with intent(inout).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  leached_vert(land_pts,0:nz_soilc),                                           &
    ! Solute flux across lower boundary of layer (kg m-2 s-1).
    ! This is restricted to be >= 0, i.e. downwards (or zero).
  leached_lat(land_pts,nz_soilc),                                              &
    ! Solute leaving in lateral flow (kg m-2 s-1).
  total_store(land_pts,nz_soilc)
    ! Storage in soil layer, including any adsorbed fraction (kg m-2).

!-----------------------------------------------------------------------------
! Array arguments with intent(out).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) :: leached(land_pts)
    ! Total leached flux out of soil column (kg m-2 s-1). This is the sum of
    ! the flux through the base of the column and lateral (TOPMODEL) fluxes
    ! from each layer (both ignoring TOPMODEL's deep layer).

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: i, iz, j  ! Indices.

REAL(KIND=real_jlslsm) ::                                                      &
  leachRed,                                                                    &
    ! Amount by which to reduce leaching.
  reduceRatio,                                                                 &
    ! Ratio for reducing leaching so we do not try to leach more than is
    ! available.
  totOut
    ! The output leaching from a layer (kg m-2 s-1).

!-----------------------------------------------------------------------------
! Local arrays.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) :: leached_lay(land_pts,nz_soilc)
    ! Total leached flux (out-in) from each layer (kg m-2 s-1).

!-----------------------------------------------------------------------------
!end of header

!-----------------------------------------------------------------------------
! Initialise
!-----------------------------------------------------------------------------
leached(:)       = 0.0
leached_lay(:,:) = 0.0

DO j = 1,vs_pts
  i = vs_index(j)

  DO iz = 1,nz_soilc

    ! Net leaching flux (output-input).
    leached_lay(i,iz) = leached_lat(i,iz) + leached_vert(i,iz)                 &
                                          - leached_vert(i,iz-1)

    ! If leaching will over deplete the store, reduce the output fluxes and
    ! leave the input fluxes alone. This works because we have only allowed
    ! water movement and leaching to be downwards, meaning we can move down
    ! the column without having to go back and alter layers above.
    IF ( leached_lay(i,iz) * dt > avail_store(i,iz) ) THEN

      ! Calculate the reduction required.
      ! This will be achieved by reducing the vertical and lateral leaching
      ! proportionally.
      leachRed = leached_lay(i,iz) - avail_store(i,iz) / dt

      ! There are two possibilities of where the reduction can be made:
      ! 1) lateral leaching (which is always positive)
      ! 2) leached_vert(i,iz) (which is also required to be positive, i.e.
      !    out of the layer.
      !    Note that if we also allowed leached_vert<0 any adjustment to this
      !    would also require adjustment of layer iz-1 - which would be more
      !    complicated!
      ! Note we expect totOut to be > 0 given that we are dealing with the
      ! case of when leaching over depletes the store, so division by zero
      ! should not be an issue below.
      totOut = leached_vert(i,iz) + leached_lat(i,iz)

      ! Reduce downward leaching from this layer.
      reduceRatio    = leached_vert(i,iz) / totOut
      leached_vert(i,iz) = leached_vert(i,iz) - reduceRatio * leachRed

      ! Reduce the lateral leaching.
      leached_lat(i,iz) = leached_lat(i,iz) - (1.0 - reduceRatio) * leachRed

      ! Recalculate the total flux
      leached_lay(i,iz) = leached_lat(i,iz) + leached_vert(i,iz)               &
                                            - leached_vert(i,iz-1)

    END IF  !  too much leaching

    ! Update the layer storage.
    total_store(i,iz) = total_store(i,iz) - leached_lay(i,iz) * dt

    ! Add any horizontal flux to the total leaching from the column.
    leached(i) = leached(i) + leached_lat(i,iz)

  END DO  !  layers

  ! Add vertical flux from bottom layer to total leaching.
  leached(i) = leached(i) + leached_vert(i,nz_soilc)

END DO  !  point

END SUBROUTINE extract_leached

!#############################################################################
!#############################################################################

FUNCTION solute_flux_v_interp( land_pts, vs_pts, dz_upper, dz_lower,           &
                               vs_index, conc_upper, conc_lower,               &
                               water_flux_down )                               &
         RESULT(solflux)

! Description:
!  Calculate the vertical flux of a solute from upper to lower soil layer. The
!  solute concentration at the layer boundary is found by interpolating
!  the concentrations between the upper and lower layers.

USE water_constants_mod, ONLY:                                                 &
  ! imported parameters
  rho_water

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Number of land points.
  vs_pts
    ! The number of points with veg and/or soil.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  dz_upper,                                                                    &
    ! Thickness of upper soil layer (m).
  dz_lower
    ! Thickness of lower soil layer (m).

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) :: vs_index(land_pts)
    ! Indices of veg/soil points/

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  conc_upper(land_pts),                                                        &
    ! Solute concentration in soil water in upper soil layer (kg m-3).
  conc_lower(land_pts),                                                        &
    ! Solute concentration in soil water in lower soil layer (kg m-3).
  water_flux_down(land_pts)
    ! Downward water flux from the upper soil layer to the lower (kg m-2 s-1).

!-----------------------------------------------------------------------------
! Function result
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) :: solflux(land_pts)
    ! Downward flux of solute flux at the bottom of upper soil
    ! layer (kg m-2 s-1).
    ! This is restricted to be >= 0, i.e. downwards (or zero).

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: i, j  !  Indices.

REAL(KIND=real_jlslsm) ::                                                      &
  dz_u_over_sum
    ! A precalculated term involving layer thicknesses.

!----------------------------------------------------------------------------
! Local array variables.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) :: clbnd(land_pts)
  ! Solute concentration in soil water at layer boundary (kg m-3).

!-----------------------------------------------------------------------------
!end of header

! Calculate constant terms.
dz_u_over_sum = dz_upper / ( dz_upper + dz_lower )

! Interpolate concentration to layer boundary.

DO j = 1,vs_pts
  i = vs_index(j)

  clbnd(i) = conc_upper(i) * (1.0 - dz_u_over_sum) +                           &
             conc_lower(i) * dz_u_over_sum

END DO

! Calculate the solute flux out of the bottom of upper soil layer, given the
! water flux. For simplicity we only consider downward fluxes - this simplifies
! later code that guards against over depletion of any store.
DO j = 1,vs_pts
  i = vs_index(j)
  solflux(i) = MAX( water_flux_down(i), 0.0 ) * clbnd(i) / rho_water
END DO

END FUNCTION solute_flux_v_interp

!#############################################################################
!#############################################################################

FUNCTION solute_flux_v_extrap( land_pts, vs_pts, dz_base, dz_above, vs_index,  &
                        conc_base, conc_above, water_flux_down )               &
         RESULT(solflux)

! Description:
!  Calculate the vertical flux of a solute out of a soil layer at the bottom
!  of the soil column. The solute concentration at the bottom of the lowest
!  soil layer is found by extrapolating from the soil layer above.

USE water_constants_mod, ONLY:                                                 &
  ! imported parameters
  rho_water

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Number of land points.
  vs_pts
    ! The number of points with veg and/or soil.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  dz_base,                                                                     &
    ! Thickness of layer at base of soil column (m).
  dz_above
    ! Thickness of layer above the layer at the base of the soil column (m).

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) :: vs_index(land_pts)
    ! Indices of veg/soil points/

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  conc_base(land_pts),                                                         &
    ! Solute concentration in soil water in layer at base of soil
    ! column (kg m-3).
  conc_above(land_pts),                                                        &
    ! Solute concentration in soil water in layer above the layer at the base
    ! of the soil column kg m-3).
  water_flux_down(land_pts)
    ! Downward water flux from the bottom of the soil column (kg m-2 s-1).

!-----------------------------------------------------------------------------
! Function result
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) :: solflux(land_pts)
    ! Downward flux of solute flux at the bottom of the soil
    ! column (kg m-2 s-1).
    ! This is restricted to be >= 0, i.e. downwards (or zero).

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: i, j  !  Indices.

REAL(KIND=real_jlslsm) ::                                                      &
  dz_b_over_sum,                                                               &
    ! A precalculated term involving layer thicknesses.
  dz_term_over_sum
    ! A precalculated term involving layer thicknesses.

!----------------------------------------------------------------------------
! Local array variables.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) :: clbnd(land_pts)
  ! Solute concentration in soil water at layer boundary (kg m-3).

!-----------------------------------------------------------------------------
!end of header

! Extrapolate to find the concentration.

! Calculate constant terms.
dz_b_over_sum = dz_base / ( dz_base + dz_above )
dz_term_over_sum  = ( dz_above + 2.0 * dz_base ) / ( dz_base + dz_above )

DO j = 1,vs_pts
  i = vs_index(j)

  ! Interpolate concentration to bottom of base layer, ensuring the value
  ! is positive.
  clbnd(i) = MAX( conc_base(i) * dz_term_over_sum -                            &
                  conc_above(i) * dz_b_over_sum,                               &
                  0.0 )

END DO

! Calculate the solute flux out of the bottom of base layer, given the water
! flux. For simplicity we only consider downward fluxes - this simplifies
! later code that guards against over depletion of any store.
DO j = 1,vs_pts
  i = vs_index(j)
  solflux(i) = MAX( water_flux_down(i), 0.0 ) * clbnd(i) / rho_water
END DO

END FUNCTION solute_flux_v_extrap

!#############################################################################
!#############################################################################

FUNCTION solute_flux_h( land_pts, vs_pts, vs_index, conc,                      &
                        water_flux_lateral)                                    &
         RESULT(solflux)

! Description:
!  Calculate the solute flux laterally out of a soil layer.
!  Uses the solute concentration at the centre of the soil layer.

USE water_constants_mod, ONLY:                                                 &
  ! imported parameters
  rho_water

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Number of land points.
  vs_pts
    ! The number of points with veg and/or soil.

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) :: vs_index(land_pts)
    ! Indices of veg/soil points/

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  conc(land_pts),                                                              &
    ! Solute concentration in soil water (kg m-3).
  water_flux_lateral(land_pts)
    ! Lateral water flux out of layer (kg m-2 s-1).

!-----------------------------------------------------------------------------
! Function result.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) :: solflux(land_pts)
                          ! Solute flux between layers (kg m-2 s-1).

!-----------------------------------------------------------------------------
! Local scalars.
!-----------------------------------------------------------------------------
INTEGER :: i, j  ! Indices.

!-----------------------------------------------------------------------------
!end of header

DO j = 1,vs_pts
  i = vs_index(j)

  ! Flux (kg m-2 s-1).
  solflux(i) = water_flux_lateral(i) * conc(i) / rho_water

END DO

END FUNCTION solute_flux_h

!#############################################################################
!#############################################################################

END MODULE ecosse_leaching_mod
#endif
