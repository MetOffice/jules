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

MODULE deposition_jules_aerod_mod

!-------------------------------------------------------------------------------
! Description:
!   Calculates the aerodynamic and quasi-laminar resistances.
!   Based on UKCA routine
!     ukca/src/science/chemistry/deposition/ukca_aerod.F90
!   Returns Ra, Rb
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGENIC FLUXES.
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName = "DEPOSITION_JULES_AEROD_MOD"

CONTAINS

!-------------------------------------------------------------------------------
SUBROUTINE deposition_jules_aerod(                                             &
  row_length, rows, land_pts, land_index, water,                               &
  tstar_ij, pstar_ij, surf_ht_flux_ij, ustar_ij, gsf_ij, zbl_ij, z0tile_ij,    &
  canht_pft, dep_ra_ij, dep_rb_ij, dep_vd_so4_ij)
!-------------------------------------------------------------------------------

USE jules_deposition_mod,    ONLY: ndry_dep_species, dep_rnull
USE deposition_species_mod,  ONLY: dep_species_name, diffusion_coeff

! The UKCA module ukca_constants has been replaced with a JULES-based module
! Constants now in deposition_initialisation_aerod_mod
USE deposition_initialisation_aerod_mod, ONLY:                                 &
                                   cp, pr, vkman, gg, cp_vkman_g, m_air,       &
                                   zero, one, two, sixteen, minus5, minus30,   &
                                   minus300, half, twothirds, zref, zpd_factor,&
                                   vair296, tkva296, kva_factor, mo_neutral,   &
                                   ustar_min, z0_min, z0_coeff1, z0_coeff2,    &
                                   vd_coeff1, vd_coeff2

USE c_rmol,                  ONLY: rmol
USE jules_surface_types_mod, ONLY: ntype, npft
USE um_types,                ONLY: real_jlslsm
USE parkind1,                ONLY: jprb, jpim
USE yomhook,                 ONLY: lhook, dr_hook
USE jules_print_mgr,         ONLY: jules_message, jules_print
USE ereport_mod,             ONLY: ereport

IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
  row_length,                                                                  &
    ! Number of points on a row
  rows,                                                                        &
    ! Number of rows
  land_pts,                                                                    &
    ! Number of land points
  water
    ! pseudo tile ID for open water surface type

! Input arguments on land points
INTEGER, INTENT(IN) ::                                                         &
  land_index(land_pts)

! Arrays on grid
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  tstar_ij(row_length,rows),                                                   &
    ! Surface temperature (K)
  pstar_ij(row_length,rows),                                                   &
    ! Surface pressure (Pa)
  surf_ht_flux_ij(row_length,rows),                                            &
    ! Surface heat flux (W m-2)
  ustar_ij(row_length,rows),                                                   &
    ! Surface friction velocity (m s-1)
  gsf_ij(row_length,rows,ntype),                                               &
    ! Surface tile fractions
  zbl_ij(row_length,rows)
    ! Boundary layer depth

! Arrays on land points
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  canht_pft(land_pts,npft)
    ! Canopy height of vegetation (m)

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  z0tile_ij(row_length,rows,ntype)
    ! Roughness length on tiles (m)

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  dep_ra_ij(row_length,rows,ntype),                                            &
    ! Aerodynamic resistance, Ra (s m-1)
  dep_rb_ij(row_length,rows,ndry_dep_species),                                 &
    ! Quasi-laminar resistance, Rb (s m-1)
  dep_vd_so4_ij(row_length,rows)
    ! Surface deposition velocity term for
    !  sulphate particles

!-------------------------------------------------------------------------------
! Local variables
INTEGER              :: i, j, k, l, n
                             ! Loop parameters

REAL(KIND=real_jlslsm) ::                                                      &
  b0,                                                                          &
    ! Temporary store
  b1,                                                                          &
    ! Temporary store
  sc_pr
    ! Ratio of Schmidt and Prandtl numbers

REAL(KIND=real_jlslsm) ::                                                      &
  mo_len(row_length,rows),                                                     &
    ! Monin-Obukhov length (m)
  rho_air(row_length,rows),                                                    &
    ! Density of air at surface (kg m-3)
  kva(row_length,rows),                                                        &
    ! Kinematic velocity of air (m2 s-1)
  ustar(row_length,rows),                                                      &
    ! Friction velocity [checked] (m s-1)
  zh(row_length,rows,ntype),                                                   &
    ! Reference height (m)
  psi(row_length,rows,ntype),                                                  &
    ! Businger function (dimensionless)
  bl_by_mo_len
    ! Boundary layer height divided by
    ! Monin-Obukhov length

INTEGER(KIND=jpim), PARAMETER     :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER     :: zhook_out = 1
REAL(KIND=jprb)                   :: zhook_handle

CHARACTER(LEN=*), PARAMETER       ::                                           &
  RoutineName='DEPOSITION_JULES_AEROD'

! Ereport variables
INTEGER :: icode

!-------------------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Calculate air density, rho-air.
! Set ustar to minimum value
DO j = 1, rows
  DO i = 1, row_length
    rho_air(i,j) = m_air * pstar_ij(i,j) / (rmol * tstar_ij(i,j))
    ustar(i,j)   = ustar_ij(i,j)
    IF (ustar(i,j) <= zero) THEN
      ustar(i,j) = ustar_min
    END IF
  END DO        ! End of DO loop over row_length
END DO          ! End of DO loop over rows

! Set z to reference height above surface minus zero plane displacement
! Zero-plane displacement is 0.7 of canopy height for vegetated surfaces,
! otherwise 0.0
! (Smith et al., 2000).

! Initialise on gridded domain
DO n = 1, ntype
  DO j = 1, rows
    DO i = 1, row_length
      zh(i,j,n) = zref
    END DO      ! End of DO loop over row_length
  END DO        ! End of DO loop over rows
END DO          ! End of DO loop over surface types

! Modify for vegetated surfaces
DO n = 1, npft
  DO l = 1, land_pts
    j = (land_index(l)-1)/row_length + 1
    i = land_index(l) - (j-1)*row_length
    IF (canht_pft(l,n) > zero) THEN
      zh(i,j,n) = zref - canht_pft(l,n)*zpd_factor
    END IF
  END DO        ! End of DO loop over land points
END DO          ! End of DO loop over pfts

! Set any undefined values of z0 from the regridding, especially over the ocean
! and cryosphere, to 0.001 (z0_min) to avoid any floating-point exceptions.
DO n = 1, ntype
  DO j = 1, rows
    DO i = 1, row_length
      IF (z0tile_ij(i,j,n) <= zero) THEN
        z0tile_ij(i,j,n) = z0_min
      END IF
    END DO      ! End of DO loop over row_length
  END DO        ! End of DO loop over rows
END DO          ! End of DO loop over surface types

! Calculate kinematic viscosity of air,
! KVA; = dynamic viscosity / density
! Formula from Kaye and Laby, 1952, p.43.
DO j = 1, rows
  DO i = 1, row_length
    kva(i,j) = (vair296 - kva_factor*(tkva296-tstar_ij(i,j))) / rho_air(i,j)

    ! Calculate roughness length over oceans using Charnock
    ! formula in form given by Hummelshoj et al., 1992.
    IF (gsf_ij(i,j,water) > zero) THEN
      z0tile_ij(i,j,water) = (kva(i,j) / (z0_coeff1*ustar(i,j))) +             &
        z0_coeff2*ustar(i,j)*ustar(i,j) / gg
    END IF
  END DO        ! End of DO loop over row_length
END DO          ! End of DO loop over rows

! Calculate Monin-Obukhov length L
DO j = 1, rows
  DO i = 1, row_length
    IF (surf_ht_flux_ij(i,j) /= zero) THEN
      ! Stable or unstable b.l.
      mo_len(i,j) = cp_vkman_g * tstar_ij(i,j) * rho_air(i,j) *                &
        (ustar(i,j)**3) / surf_ht_flux_ij(i,j)
    ELSE
      ! Neutral b.l.
      mo_len(i,j) = mo_neutral
    END IF
  END DO        ! End of DO loop over row_length
END DO          ! End of DO loop over rows

! Calculate Businger functions (PSI(ZETA))
DO n = 1, ntype
  DO j = 1, rows
    DO i = 1, row_length
      IF (mo_len(i,j) > zero) THEN
        ! Stable b.l.
        psi(i,j,n) = minus5 * (zh(i,j,n) - z0tile_ij(i,j,n)) / mo_len(i,j)
      ELSE
        ! Unstable b.l.
        b1 = (one - sixteen * (zh(i,j,n) / mo_len(i,j)))**half
        b0 = (one - sixteen * (z0tile_ij(i,j,n) / mo_len(i,j)))**half
        psi(i,j,n) = two * LOG((one+b1) / (one+b0))
      END IF
    END DO      ! End of DO loop over row_length
  END DO        ! End of DO loop over rows
END DO          ! End of DO loop over surface types

! 1. Ra: Calculate the aerodynamic resistance for all species
DO n = 1, ntype
  DO j = 1, rows
    DO i = 1, row_length
      dep_ra_ij(i,j,n) = (LOG(zh(i,j,n) / z0tile_ij(i,j,n)) - psi(i,j,n)) /    &
        (vkman * ustar(i,j))
    END DO      ! End of DO loop over row_length
  END DO        ! End of DO loop over rows
END DO          ! End of DO loop over surface types

! 2. Rb: Calculate quasi-laminar boundary-layer resistance
!    Calculate Schmidt number divided by Prandtl number for each
!    species. Schmidt number = Kinematic viscosity of air divided
!    by molecular diffusivity (Hicks et al., 1987)
DO n = 1, ndry_dep_species

  ! First set rb for aerosols to one as done in STOCHEM code
  IF ( (dep_species_name(n) == 'ORGNIT    ') .OR.                              &
       (dep_species_name(n) == 'ASOA      ') .OR.                              &
       (dep_species_name(n) == 'BSOA      ') .OR.                              &
       (dep_species_name(n) == 'ISOSOA    ')                                   &
     ) THEN

    DO j = 1, rows
      DO i = 1, row_length
        dep_rb_ij(i,j,n) =  one
      END DO
    END DO

  ELSE IF (diffusion_coeff(n) > zero) THEN

    DO j = 1, rows
      DO i = 1, row_length
        sc_pr = kva(i,j) / (pr * diffusion_coeff(n))
        dep_rb_ij(i,j,n) = (sc_pr**twothirds) / (vkman * ustar(i,j))
      END DO
    END DO
  ELSE
    DO j = 1, rows
      DO i = 1, row_length
        dep_rb_ij(i,j,n) = zero
      END DO
    END DO
  END IF
END DO          ! End of DO loop over deposited atmospheric species

! 3. SO4_VD: Needed for ORGNIT or other species treated as aerosols
!    Calculate surface deposition velocity term for sulphate particles
!    using simple parameterisation of Wesely in the form given by
!    Zhang et al. (2001), Atmos Environ, 35, 549-560.
!    These values can be used for different aerosol types
!    and ORGNIT is considered to be an aerosol.

dep_vd_so4_ij = one / dep_rnull

DO n = 1, ndry_dep_species
  IF ( (dep_species_name(n) == 'ORGNIT    ') .OR.                              &
       (dep_species_name(n) == 'ASOA      ') .OR.                              &
       (dep_species_name(n) == 'BSOA      ') .OR.                              &
       (dep_species_name(n) == 'ISOSOA    ')                                   &
     ) THEN
    DO j = 1, rows
      DO i = 1, row_length
        dep_vd_so4_ij(i,j) = zero
        bl_by_mo_len = zbl_ij(i,j) / mo_len(i,j)
        IF (bl_by_mo_len < minus30) THEN
          dep_vd_so4_ij(i,j) = vd_coeff2 * ustar(i,j)                          &
                               * ((-bl_by_mo_len)**twothirds)
        ELSE IF (mo_len(i,j) >= zero) THEN
          dep_vd_so4_ij(i,j) = vd_coeff1 * ustar(i,j)
        ELSE
          ! bl_by_mo_len >= minus30 and mo_len(i,j) < zero
          dep_vd_so4_ij(i,j) = vd_coeff1 * ustar(i,j)                          &
            * (one + (minus300/mo_len(i,j))**twothirds)
        END IF
      END DO
    END DO

    EXIT

  END IF
END DO          ! End of DO loop over deposited atmospheric species

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN

END SUBROUTINE deposition_jules_aerod
END MODULE deposition_jules_aerod_mod
