! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE lotka_eq_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='LOTKA_EQ_MOD'

CONTAINS

!-----------------------------------------------------------------------------
! Subroutine LOTKA -----------------------------------------------------------
!
! Purpose : Updates fractional coverage of each functional type.
!           Based on the Lotka-Volterra equations of interspecies
!           competition.
!
! ----------------------------------------------------------------------------
SUBROUTINE lotka_eq (land_pts, trif_pts, trif_index,                           &
                     c_veg, frac_prev, frac_agric, ht, pc_s,                   &
                     frac_eq, frac_nofire,g_burn_pft)

USE jules_surface_types_mod, ONLY:                                             &
  ! imported scalars
  npft, nnpft, ntype, soil

USE trif, ONLY: crop, g_area
USE jules_vegetation_mod, ONLY: frac_min

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Total number of land points.
  trif_pts
    ! Number of points on which TRIFFID may operate.

INTEGER, INTENT(IN) ::                                                         &
  trif_index(land_pts)
    ! Indices of land points on which TRIFFID may operate.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  c_veg(land_pts,npft),                                                        &
    ! Carbon content of vegetation (kg C/m2).
  frac_prev(land_pts,ntype),                                                   &
    ! Previous time step fractional cover.
  frac_agric(land_pts),                                                        &
    ! Fraction of agriculture.
  ht(land_pts,npft),                                                           &
    ! Canopy height (m).
  pc_s(land_pts,npft)
    ! Net carbon flux available for spreading (kg C/m2/360days).

!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  frac_eq(land_pts,ntype)  ! Fractional cover of each Functional Type.

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  frac_nofire(land_pts,ntype)  ! Fractional cover of each Functional Type.

!New arguments replacing USE statements
REAL(KIND=real_jlslsm), INTENT(IN) :: g_burn_pft(land_pts,npft)

!-----------------------------------------------------------------------------
! Local integer variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  k,l,m,n,t,i,j       ! Loop counters.

INTEGER ::                                                                     &
  dom(land_pts,nnpft) ! Dominance hierarchy.

!-----------------------------------------------------------------------------
! Local real variables.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) :: tallest
                      ! Height of the tallest PFT from temp_ht (m).

REAL(KIND=real_jlslsm) ::                                                      &
  com(land_pts,nnpft,nnpft),                                                   &
    ! Coefficients representing the influence of one type (second argument)
    !  on another (first argument).
  nosoil(land_pts),                                                            &
    ! Fractional area not available to vegetation.
  space(land_pts,nnpft),                                                       &
    ! Space available for invasion.
  spacemax(land_pts),                                                          &
    ! Total space in the grid cell.
  g_dist(land_pts,nnpft),                                                      &
    ! Area disturbance.
  temp_ht(nnpft)
    ! Store height for assigning dominance (m).

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='LOTKA_EQ'

!-----------------------------------------------------------------------------
!end of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Initialisation.
!-----------------------------------------------------------------------------
frac_nofire(:,:) = frac_eq(:,:)

!-----------------------------------------------------------------------------
! Derive area disturbance
!-----------------------------------------------------------------------------
DO n = 1,nnpft
  DO l = 1,land_pts
    g_dist(l,n) = g_area(n) + g_burn_pft(l,n)
  END DO
END DO

!-----------------------------------------------------------------------------
! Define competition coefficients and the dominance hierachy
!-----------------------------------------------------------------------------
DO n = 1,nnpft
  DO m = 1,nnpft
    DO t = 1,trif_pts
      l = trif_index(t)
      com(l,n,m) = 1.0
    END DO
  END DO
END DO

!NEW: COM(I,J) BASED ON HEIGHT OF PFT I & J
DO t = 1,trif_pts
  l = trif_index(t)

  !DOM IS THE DOMINANCE HIERARCHY BASED ON HEIGHTS
  !WARNING:If two or more PFTs
  !have exactly the same height, the first PFT in the list
  !will get the higher dominance.
  temp_ht(1:nnpft) = ht(l,1:nnpft)
  DO i = 1,nnpft
    tallest = MAXVAL(temp_ht)
    DO j = 1,nnpft
      IF (ABS(temp_ht(j) - tallest) < EPSILON(1.0)) THEN
        dom(l,i)   = j
        temp_ht(j) = -999.0
        EXIT
      END IF
    END DO
  END DO

  DO j = 1,nnpft
    n = dom(l,j)
    com(l,n,n) = 0.0
    IF (j < nnpft) THEN
      DO k = j+1,nnpft
        m = dom(l,k)
        com(l,m,n) = 1.0
        com(l,n,m) = 0.0
      END DO
    END IF
  END DO
END DO

!-----------------------------------------------------------------------------
! Calculate the space available for expansion of vegetation.
! Loop over non-veg types, assuming only the soil type can contribute space.
!-----------------------------------------------------------------------------
nosoil(:) = 0.0
DO n = npft+1, ntype
  IF ( n == soil ) CYCLE
  DO t = 1,trif_pts
    l = trif_index(t)
    nosoil(l) = nosoil(l) + frac_prev(l,n)
  END DO
END DO

!-----------------------------------------------------------------------------
! Exclude non-crop types from agricultural regions
!-----------------------------------------------------------------------------
DO j = 1,nnpft
  DO t = 1,trif_pts
    l = trif_index(t)
    n = dom(l,j)
    space(l,n) = 1.0 - nosoil(l) - frac_agric(l) * REAL(1 - crop(n))           &
                     - frac_min * REAL(nnpft - j)
  END DO
END DO

DO n = 1,nnpft
  DO m = 1,nnpft
    DO t = 1,trif_pts
      l = trif_index(t)
      space(l,n) = space(l,n) - com(l,n,m) * frac_eq(l,m)
    END DO
  END DO
END DO

!-----------------------------------------------------------------------------
! We are removing the implicit timestepping approach to equilibrium.
! Divide the update equation by FRAC to eliminate the (unstable)
! bare soil solution.
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
! Update the areal fractions (Compete code now here:)
!-----------------------------------------------------------------------------
spacemax(:) = 0.0
DO t = 1,trif_pts
  l = trif_index(t)
  spacemax(l) = 1.0 - nosoil(l) - frac_min * REAL(nnpft-1)
END DO

DO t = 1,trif_pts
  l = trif_index(t)
  DO j = 1,nnpft

    n = dom(l,j)
    IF (pc_s(l,n) < 0.0) THEN
      frac_eq(l,n) = frac_min
    ELSE
      frac_eq(l,n) = space(l,n) - (g_dist(l,n) * c_veg(l,n) / pc_s(l,n))
    END IF

    IF (frac_eq(l,n) < frac_min) THEN
      frac_eq(l,n) = frac_min
    ELSE IF (frac_eq(l,n) > spacemax(l)) THEN
      frac_eq(l,n) = spacemax(l)
    END IF

    spacemax(l) = spacemax(l) - frac_eq(l,n) + frac_min
  END DO
END DO

!-----------------------------------------------------------------------------
! Diagnose the new bare soil fraction
!-----------------------------------------------------------------------------
DO t = 1,trif_pts
  l = trif_index(t)
  frac_eq(l,soil) = spacemax(l)
  IF (frac_eq(l,soil) < frac_min) THEN !AVOID NEGATIVE SOIL FRACTIONS
    frac_eq(l,soil) = frac_min
  END IF
END DO

!-----------------------------------------------------------------------------
! Calculate litter without fire
!-----------------------------------------------------------------------------
DO j = 1,nnpft
  DO t = 1,trif_pts
    l = trif_index(t)
    n = dom(l,j)
    space(l,n) = 1.0 - nosoil(l) - frac_agric(l) * REAL(1 - crop(n))           &
                     - frac_min * REAL(nnpft - j)
  END DO
END DO

DO n = 1,nnpft
  DO m = 1,nnpft
    DO t = 1,trif_pts
      l = trif_index(t)
      space(l,n) = space(l,n) - com(l,n,m) * frac_nofire(l,m)
    END DO
  END DO
END DO

!-----------------------------------------------------------------------------
! We are removing the implicit timestepping approach to equilibrium.
! Divide the update equation by FRAC to eliminate the (unstable)
! bare soil solution.
!-----------------------------------------------------------------------------
! Update the areal fractions (Compete code now here:)
!-----------------------------------------------------------------------------
spacemax(:) = 0.0
DO t = 1,trif_pts
  l = trif_index(t)
  spacemax(l) = 1.0 - nosoil(l) - frac_min * REAL(nnpft-1)
END DO

DO t = 1,trif_pts
  l = trif_index(t)
  DO j = 1,nnpft

    n = dom(l,j)
    IF (pc_s(l,n) < 0.0) THEN
      frac_nofire(l,n) = frac_min
    ELSE
      frac_nofire(l,n) = space(l,n) - (g_area(n) * c_veg(l,n) / pc_s(l,n))
    END IF

    IF (frac_nofire(l,n) < frac_min) THEN
      frac_nofire(l,n) = frac_min
    ELSE IF (frac_nofire(l,n) > spacemax(l)) THEN
      frac_nofire(l,n) = spacemax(l)
    END IF

    spacemax(l) = spacemax(l) - frac_nofire(l,n) + frac_min
  END DO
END DO

!-----------------------------------------------------------------------------
! Diagnose the new bare soil fraction
!-----------------------------------------------------------------------------
DO t = 1,trif_pts
  l = trif_index(t)
  frac_nofire(l,soil) = spacemax(l)
  IF (frac_nofire(l,soil) < frac_min) THEN !AVOID NEGATIVE SOIL FRACTIONS
    frac_nofire(l,soil) = frac_min
  END IF
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN
END SUBROUTINE lotka_eq

END MODULE lotka_eq_mod

