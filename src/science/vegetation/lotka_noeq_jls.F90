! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE lotka_noeq_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='LOTKA_NOEQ_MOD'

CONTAINS

!----------------------------------------------------------------------------
! Subroutine LOTKA ----------------------------------------------------------
!
! Purpose : Updates fractional coverage of each functional type.
!           Based on the Lotka-Volterra equations of interspecies
!           competition.
!
! ---------------------------------------------------------------------------
SUBROUTINE lotka_noeq (land_pts, trif_pts, trif_index,                         &
                       r_gamma, c_veg, frac_agric, frac_agr_prev,              &
                       ht, pc_s, frac, frac_na,                                &
                       frac_nofire, dfrac, dfrac_na, dfrac_nofire, g_burn_pft)

USE jules_surface_types_mod, ONLY:                                             &
  ! imported scalars
  npft, nnpft, ntype, soil

USE trif, ONLY: crop, g_area
USE jules_vegetation_mod, ONLY: frac_min, frac_seed

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
  r_gamma
    ! Inverse timestep (/360days).

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  c_veg(land_pts,npft),                                                        &
    ! Carbon content of vegetation (kg C/m2).
  frac_agric(land_pts),                                                        &
    ! Fraction of agriculture.
  frac_agr_prev(land_pts),                                                     &
    ! Fraction of agriculture.
  ht(land_pts,npft),                                                           &
    ! Canopy height (m)
  pc_s(land_pts,npft)
    ! Net carbon flux available for spreading (kg C/m2/360days).

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  frac(land_pts,ntype)  ! Fractional cover of each Functional Type.

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  frac_na(land_pts,ntype),                                                     &
    ! Fractional cover of each Functional Type.
  frac_nofire(land_pts,ntype),                                                 &
    ! Fractional cover of each Functional Type.
  dfrac(land_pts,npft),                                                        &
    ! Increment to the areal fraction during the timestep.
  dfrac_na(land_pts,npft),                                                     &
    ! Increment to the areal fraction during the timestep,
    ! prior to the land use change
  dfrac_nofire(land_pts,npft)
    ! Increment to the areal fraction during the timestep,
    ! with LUC, without fire

!New arguments replacing USE statements
REAL(KIND=real_jlslsm), INTENT(IN) :: g_burn_pft(land_pts,npft)

!-----------------------------------------------------------------------------
! Local integer variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  k,l,m,n,t,i,j        ! Loop counters

INTEGER ::                                                                     &
  dom(land_pts,nnpft)  ! Dominance hierarchy.

!-----------------------------------------------------------------------------
! Local real variables.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  fracn,                                                                       &
    ! Fraction used in the spreading calculation.
  tallest
    ! Work Height of the tallest PFT from temp_ht (m).

REAL(KIND=real_jlslsm) ::                                                      &
  com(land_pts,nnpft,nnpft),                                                   &
    ! Coefficients representing the influence of one type (second argument)
    ! on another (first argument).
  nosoil(land_pts),                                                            &
    ! Fractional area not available to vegetation.
  space(land_pts,nnpft),                                                       &
    ! Space available for invasion.
  spacemax(land_pts),                                                          &
    ! Total space in the grid cell.
  g_dist(land_pts,nnpft),                                                      &
    ! Area disturbance.
  temp_ht(npft)
    ! Store height for assigning dominance.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='LOTKA_NOEQ'

!-----------------------------------------------------------------------------
!end of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Derive area disturbance.
!-----------------------------------------------------------------------------
DO n = 1,nnpft
  DO l = 1,land_pts
    g_dist(l,n) = g_area(n) + g_burn_pft(l,n)
  END DO
END DO

!-----------------------------------------------------------------------------
! Initialise frac variables.
!-----------------------------------------------------------------------------
DO n = 1,ntype
  DO l = 1,land_pts
    frac_na(l,n)      = frac(l,n)
    frac_nofire(l,n)  = frac(l,n)
  END DO
END DO

!-----------------------------------------------------------------------------
! Define competition coefficients and the dominance hierachy.
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
  !WARNING: If two or more PFTs
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
    com(l,n,n) = 1.0
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
    nosoil(l) = nosoil(l) + frac(l,n)
  END DO
END DO

!-----------------------------------------------------------------------------
! Exclude non-crop types from agricultural regions
!-----------------------------------------------------------------------------
DO k = 1,nnpft
  DO t = 1,trif_pts
    l = trif_index(t)
    n = dom(l,k)
    space(l,n) = 1.0 - nosoil(l) - frac_agric(l) * REAL(1 - crop(n))           &
                     - frac_min * REAL(nnpft - k)
  END DO
END DO

DO n = 1,nnpft
  DO m = 1,nnpft
    DO t = 1,trif_pts
      l = trif_index(t)
      space(l,n) = space(l,n) - (com(l,n,m) * frac(l,m))
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
  DO n = 1,nnpft
    dfrac(l,n) = 0.0
  END DO
  spacemax(l) = 1.0 - nosoil(l) - frac_min * REAL(nnpft-1)
END DO

DO t = 1,trif_pts
  l = trif_index(t)
  DO j = 1,nnpft
    n = dom(l,j)
    fracn      = frac(l,n)
    fracn      = MAX(fracn,frac_seed)
    dfrac(l,n) = (pc_s(l,n) * space(l,n) / c_veg(l,n) - g_dist(l,n))           &
                 * (fracn / r_gamma)
    frac(l,n)  = frac(l,n) + dfrac(l,n)

    IF (frac(l,n) > (spacemax(l) - REAL(1 - crop(n)) * frac_agric(l))) THEN
      dfrac(l,n) = dfrac(l,n)                                                  &
                   + (spacemax(l) - REAL(1 - crop(n)) * frac_agric(l))         &
                   - frac(l,n)
      frac(l,n)  = spacemax(l) - REAL(1 - crop(n)) * frac_agric(l)
    END IF

    IF (frac(l,n) < frac_min) THEN
      dfrac(l,n) = dfrac(l,n) + (frac_min - frac(l,n))
      frac(l,n)  = frac_min
    END IF

    spacemax(l) = spacemax(l) - frac(l,n) + frac_min

  END DO
END DO

!-----------------------------------------------------------------------------
! Diagnose the new bare soil fraction
!-----------------------------------------------------------------------------
DO t = 1,trif_pts
  l = trif_index(t)
  frac(l,soil) = spacemax(l)
  IF (frac(l,soil) < frac_min) THEN !AVOID NEGATIVE SOIL FRACTIONS
    frac(l,soil) = frac_min
  END IF
END DO


!-----------------------------------------------------------------------------
! For the dynamic land use we require a double call to COMPETE
! in order to be able to clearly diagnose what change is caused by the
! land use change.
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Exclude non-crop types from agricultural regions
!-----------------------------------------------------------------------------
DO k = 1,nnpft
  DO t = 1,trif_pts
    l = trif_index(t)
    n = dom(l,k)
    space(l,n) = 1.0 - nosoil(l) - frac_agr_prev(l) * REAL(1 - crop(n))        &
                     - frac_min *  REAL(nnpft - k)
  END DO
END DO

DO n = 1,nnpft
  DO m = 1,nnpft
    DO t = 1,trif_pts
      l = trif_index(t)
      space(l,n) = space(l,n) - (com(l,n,m) * frac_na(l,m))
    END DO
  END DO
END DO

!SECOND CALL TO COMPETE TO ACCOUNT FOR CHANGES FROM LU
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
  DO n = 1,nnpft
    dfrac_na(l,n) = 0.0
  END DO
  spacemax(l) = 1.0 - nosoil(l) - frac_min *  REAL(nnpft-1)
END DO

DO t = 1,trif_pts
  l = trif_index(t)
  DO j = 1,nnpft
    n = dom(l,j)
    fracn = frac_na(l,n)
    fracn = MAX(fracn,frac_seed)

    dfrac_na(l,n) = (pc_s(l,n) * space(l,n) / c_veg(l,n) - g_dist(l,n))        &
                    * (fracn / r_gamma)
    frac_na(l,n)  = frac_na(l,n) + dfrac_na(l,n)

    IF (frac_na(l,n) < frac_min) THEN
      dfrac_na(l,n) = dfrac_na(l,n) + (frac_min - frac_na(l,n))
      frac_na(l,n)  = frac_min
    ELSE IF (frac_na(l,n)                                                      &
             > (spacemax(l) - REAL(1 - crop(n)) * frac_agr_prev(l))) THEN
      IF ( (spacemax(l) - REAL(1 - crop(n)) * frac_agr_prev(l))                &
           < frac_min ) THEN
        dfrac_na(l,n) = dfrac_na(l,n) + (frac_min - frac_na(l,n))
        frac_na(l,n)  = frac_min
      ELSE
        dfrac_na(l,n) = dfrac_na(l,n) + (spacemax(l)                           &
                        - REAL(1 - crop(n)) * frac_agr_prev(l)) - frac_na(l,n)
        frac_na(l,n)  = spacemax(l) - REAL(1 - crop(n)) * frac_agr_prev(l)
      END IF
    END IF

    IF (frac_na(l,n) < frac_min) THEN
      dfrac_na(l,n) = dfrac_na(l,n) + (frac_min - frac_na(l,n))
      frac_na(l,n)  = frac_min
    END IF

    spacemax(l) = spacemax(l) - frac_na(l,n) + frac_min

  END DO
END DO

!-----------------------------------------------------------------------------
! Diagnose the new bare soil fraction
!-----------------------------------------------------------------------------
DO t = 1,trif_pts
  l = trif_index(t)
  frac_na(l,soil) = spacemax(l)
  IF (frac_na(l,soil) < frac_min) THEN !AVOID NEGATIVE SOIL FRACTIONS
    frac_na(l,soil) = frac_min
  END IF
END DO

!-----------------------------------------------------------------------------
! Calculate litter without fire
!-----------------------------------------------------------------------------
DO k = 1,nnpft
  DO t = 1,trif_pts
    l = trif_index(t)
    n = dom(l,k)
    space(l,n) = 1.0 - nosoil(l) - frac_agric(l) *  REAL(1 - crop(n))          &
                     - frac_min *  REAL(nnpft - k)
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
  DO n = 1,nnpft
    dfrac_nofire(l,n) = 0.0
  END DO
  spacemax(l) = 1.0 - nosoil(l) - frac_min *  REAL(nnpft-1)
END DO


DO t = 1,trif_pts
  l = trif_index(t)
  DO j = 1,nnpft
    n = dom(l,j)
    fracn = frac_nofire(l,n)
    fracn = MAX(fracn,frac_seed)

    dfrac_nofire(l,n) = (pc_s(l,n) * space(l,n) / c_veg(l,n) - g_area(n))      &
                        * (fracn / r_gamma)
    frac_nofire(l,n)  = frac_nofire(l,n) + dfrac_nofire(l,n)

    IF (frac_nofire(l,n)                                                       &
        > (spacemax(l) - REAL(1 - crop(n)) * frac_agric(l)) ) THEN
      dfrac_nofire(l,n) = dfrac_nofire(l,n)                                    &
                          + (spacemax(l) - REAL(1 - crop(n))                   &
                            * frac_agric(l)) - frac_nofire(l,n)
      frac_nofire(l,n)  = spacemax(l) - REAL(1 - crop(n)) * frac_agric(l)
    END IF

    IF (frac_nofire(l,n) < frac_min) THEN
      dfrac_nofire(l,n) = dfrac_nofire(l,n) + (frac_min - frac_nofire(l,n))
      frac_nofire(l,n)  = frac_min
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
END SUBROUTINE lotka_noeq

END MODULE lotka_noeq_mod
