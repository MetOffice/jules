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

MODULE deposition_jules_ddcalc_mod

!-------------------------------------------------------------------------------
! Description:
!   Calculates the overall deposition velocity and loss rate
!   Based on UKCA routine
!     ukca/src/science/chemistry/deposition/ukca_ddcalc.F90
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
  ModuleName = "DEPOSITION_JULES_DDCALC_MOD"

CONTAINS

!-------------------------------------------------------------------------------
SUBROUTINE deposition_jules_ddcalc(row_length, rows, bl_levels,                &
  timestep, zh, dzl, gsf_ij, dep_ra_ij, dep_rb_ij, dep_rc_ij, dep_vd_ij,       &
  dep_loss_rate_ij, nlev_with_ddep                                             &
  )
!-------------------------------------------------------------------------------

USE jules_deposition_mod,    ONLY: ndry_dep_species, l_ukca_ddep_lev1,         &
                                   dep_rnull

USE deposition_species_mod,  ONLY: dep_species_name

USE ereport_mod,             ONLY: ereport

USE jules_print_mgr,         ONLY: jules_message

USE jules_surface_types_mod, ONLY: ntype, npft

USE parkind1,                ONLY: jprb, jpim

USE yomhook,                 ONLY: lhook, dr_hook

USE um_types,                ONLY: real_jlslsm

IMPLICIT NONE

INTEGER, INTENT(IN)  :: row_length, rows, bl_levels

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  timestep

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  gsf_ij(row_length,rows,ntype),                                               &
    ! surface type grid square fractions
  zh(row_length,rows),                                                         &
    ! boundary layer depth
  dzl(row_length, rows, bl_levels),                                            &
    ! Separation of atmospheric layers (in m)
  dep_ra_ij(row_length, rows, ntype),                                          &
    ! Aerodynamic resistance, Ra (s m-1)
  dep_rb_ij(row_length, rows, ndry_dep_species),                               &
    ! Quasi-laminar resistance, Rb (s m-1)
  dep_rc_ij(row_length, rows, ntype, ndry_dep_species)
    ! Surface resistance, Rc (s m-1)

! Output arguments
INTEGER, INTENT(OUT) :: nlev_with_ddep(row_length,rows)
    ! Number of layers in boundary layer

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  dep_vd_ij(row_length, rows, ntype, ndry_dep_species),                        &
    ! Surface deposition velocity term (m s-1)
  dep_loss_rate_ij(row_length, rows, ndry_dep_species)
    ! First-order loss rate (s-1) for trace gases

INTEGER                :: i, j, k, m, n
INTEGER                :: errcode           ! Error code for ereport

REAL(KIND=real_jlslsm) :: dd
REAL(KIND=real_jlslsm) :: layer_depth(row_length,rows)

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER   ::                                               &
  RoutineName='DEPOSITION_JULES_DDCALC'

!-------------------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Set all arrays to zero
dep_loss_rate_ij(:,:,:)  = 0.0
dep_vd_ij(:,:,:,:)       = 0.0

! Two options:
IF (l_ukca_ddep_lev1) THEN
  ! (A) dry deposition confined to lowest model layer:
  nlev_with_ddep(:,:) = 1
  layer_depth(:,:)    = dzl(:,:,1)
ELSE
  ! (B) If dry deposition applied everywhere within the boundary
  !  layer then look for the highest model level completely contained in it.

  DO j = 1, rows
    DO i = 1, row_length
      layer_depth(i,j)    = dzl(i,j,1)
      nlev_with_ddep(i,j) = 1
    END DO
  END DO

  ! Loop over atmospheric boundary-layer levels
  DO k = 2, bl_levels
    DO j = 1, rows
      DO i = 1, row_length
        dd = layer_depth(i,j) + dzl(i,j,k)
        IF (dd < zh(i,j)) THEN
          layer_depth(i,j)    = dd
          nlev_with_ddep(i,j) = k
        END IF
      END DO    ! End of DO loop over row_length
    END DO      ! End of DO loop over rows
  END DO        ! End of DO loop over atmospheric boundary layers

END IF

! Calculate overall dry deposition velocity [dep_vd_ij = 1/(ra + rb + rc)]
! Do vegetated tiles first. Quasi-laminar resistance pre-multiplied by
! ln[z0m/z0] = 2.0 for vegetated areas, or 1.0 for smooth surfaces
! See Ganzeveld & Lelieveld, JGR 1995 Vol.100 No. D10 pp.20999-21012.
DO m = 1, ndry_dep_species
  DO n = 1, npft
    DO j = 1, rows
      DO i = 1, row_length
        IF (dep_rc_ij(i,j,n,m) < dep_rnull .AND. gsf_ij(i,j,n) > 0.0) THEN
          dep_vd_ij(i,j,n,m) = 1.0 /                                           &
            (dep_ra_ij(i,j,n)+2.0*dep_rb_ij(i,j,m)+dep_rc_ij(i,j,n,m))
        END IF
      END DO    ! End of DO loop over row_length
    END DO      ! End of DO loop over rows
  END DO        ! End of DO loop over pfts
END DO          ! End of DO loop over deposited atmospheric species

! Now do calculation for non-vegetated tiles
DO m = 1, ndry_dep_species
  DO n = npft+1, ntype
    DO j = 1, rows
      DO i = 1, row_length
        IF (dep_rc_ij(i,j,n,m) < dep_rnull .AND. gsf_ij(i,j,n) > 0.0) THEN
          dep_vd_ij(i,j,n,m) = 1.0 /                                           &
            (dep_ra_ij(i,j,n)+dep_rb_ij(i,j,m)+dep_rc_ij(i,j,n,m))
        END IF
      END DO    ! End of DO loop over row_length
    END DO      ! End of DO loop over rows
  END DO        ! End of DO loop over non-vegetated surfaces
END DO          ! End of DO loop over deposited atmospheric species

! dep-vd_ij now contains dry deposition velocities for each tile
! in each grid sq. Calculate overall first-order loss rate over
! time "timestep" for each tile and sum over all tiles to obtain
! overall first-order loss rate dep_loss_rate_ij().
DO m = 1, ndry_dep_species
  DO n = 1, ntype
    DO j = 1, rows
      DO i = 1, row_length
        IF (dep_vd_ij(i,j,n,m) > 0.0) THEN
          dep_loss_rate_ij(i,j,m) = dep_loss_rate_ij(i,j,m)                    &
            + gsf_ij(i,j,n) *                                                  &
            (1.0-EXP(-dep_vd_ij(i,j,n,m)*timestep/layer_depth(i,j)))
        END IF
      END DO    ! End of DO loop over row_length
    END DO      ! End of DO loop over rows
  END DO        ! End of DO loop over pfts
END DO          ! End of DO loop over deposited atmospheric species

! dep_loss_rate_ij contains loss rate over time "timestep".
! Divide by timestep to get rate in s-1.
DO m = 1, ndry_dep_species
  DO j = 1, rows
    DO i = 1, row_length
      dep_loss_rate_ij(i,j,m) = -LOG(1.0-dep_loss_rate_ij(i,j,m)) / timestep
    END DO      ! End of DO loop over row_length
  END DO        ! End of DO loop over rows
END DO          ! End of DO loop over deposited atmospheric species

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN

END SUBROUTINE deposition_jules_ddcalc
END MODULE deposition_jules_ddcalc_mod
