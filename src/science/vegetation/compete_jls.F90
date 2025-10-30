! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Subroutine COMPETE ---------------------------------------------------------
!
! Purpose : Updates fractional coverage of each functional type.
!           Requires a dominance hierachy as input.
!
!-----------------------------------------------------------------------------
MODULE compete_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='COMPETE_MOD'

CONTAINS

SUBROUTINE compete (land_pts, trif_pts, trif_index, dom,                       &
                    forw, r_gamma, b, db_dfrac, nosoil,                        &
                    frac_agric, frac, dfrac)

USE descent, ONLY: denom_min
USE jules_vegetation_mod, ONLY: frac_min, frac_seed
USE trif, ONLY: crop
USE jules_surface_types_mod, ONLY: npft, nnpft, ntype, soil

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
  trif_index(land_pts),                                                        &
    ! Indices of land points on which TRIFFID may operate.
  dom(land_pts,nnpft)
    ! Dominance hierarchy.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  forw,                                                                        &
    ! Forward weighting factor.
 r_gamma
    ! Inverse timestep (/360days).

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  b(land_pts,nnpft),                                                           &
    ! Mean rate of change of vegetation fraction over
    ! the timestep (kg C/m2/360days).
  db_dfrac(land_pts,nnpft,nnpft),                                              &
    ! Rate of change of b with vegetation fraction.
  nosoil(land_pts),                                                            &
    ! Fractional area not available to vegetation.
  frac_agric(land_pts)
    ! Fraction of Agriculture

!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  frac(land_pts,ntype)  ! Updated areal fraction.

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  dfrac(land_pts,npft)  ! Increment to areal fraction.

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  k,l,m,n,t  ! Loop counters.

REAL(KIND=real_jlslsm) ::                                                      &
  denom,                                                                       &
    ! Denominator of update equation.
  fracn,fracm,                                                                 &
    ! Fractions used in the spreading calculation.
  numer,                                                                       &
    ! Numerator of the update equation.
  p1,p2,q1,q2,r1,r2
    ! Coefficients in simultaneous equations.

! Local array variables.
REAL(KIND=real_jlslsm) ::                                                      &
  frac_a_loc(land_pts),                                                        &
    ! Local copy of disturbance mask to allow the fix (including it) to be
    ! turned off.
  space(land_pts)
    ! Available space.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='COMPETE'

!-----------------------------------------------------------------------------
!end of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Initialisations. Set increments to zero and define the space
! available to the dominant type leaving space for the seeds of others.
!-----------------------------------------------------------------------------
DO t = 1,trif_pts
  l = trif_index(t)
  DO n = 1,nnpft
    dfrac(l,n) = 0.0
  END DO
  space(l)      = 1.0 - nosoil(l) - frac_min * REAL(nnpft-1)
  frac_a_loc(l) = frac_agric(l)
END DO

!-----------------------------------------------------------------------------
! Calculate the increments to the tree fractions
!-----------------------------------------------------------------------------
DO t = 1,trif_pts
  l = trif_index(t)
  n = dom(l,1)
  m = dom(l,2)

  fracn = frac(l,n)
  fracn = MAX(fracn,frac_seed)

  fracm = frac(l,m)
  fracm = MAX(fracm,frac_seed)

  p1 = r_gamma / fracn - forw * db_dfrac(l,n,n)
  p2 = r_gamma / fracm - forw * db_dfrac(l,m,m)
  q1 = -forw * db_dfrac(l,n,m)
  q2 = -forw * db_dfrac(l,m,n)
  r1 = b(l,n)
  r2 = b(l,m)
  DO k = 1,nnpft
    r1 = r1 + forw * (db_dfrac(l,n,k) * dfrac(l,k))
    r2 = r2 + forw * (db_dfrac(l,m,k) * dfrac(l,k))
  END DO

  ! Update the dominant tree PFT.
  numer      = r1 - (q1 / p2) * r2
  denom      = p1 - (q1 / p2) * q2
  denom      = MAX(denom,denom_min)
  dfrac(l,n) = numer / denom
  frac(l,n)  = frac(l,n) + dfrac(l,n)

  IF (frac(l,n) <  frac_min) THEN
    dfrac(l,n) = dfrac(l,n) + (frac_min - frac(l,n))
    frac(l,n)  = frac_min

  ELSE IF (frac(l,n) > (space(l) - REAL(1 - crop(n)) * frac_a_loc(l))) THEN
    dfrac(l,n) = dfrac(l,n) +                                                  &
                 ((space(l) - REAL(1 - crop(n)) * frac_a_loc(l)) - frac(l,n))
    frac(l,n)  = space(l) - REAL(1 - crop(n)) * frac_a_loc(l)
  END IF

  ! Update the space remaining.
  space(l)   = space(l) - frac(l,n) + frac_min

  ! Update the less dominant tree PFT.
  numer      = r2 - q2 * dfrac(l,n)
  denom      = p2
  denom      = MAX(denom,denom_min)
  dfrac(l,m) = numer / denom
  frac(l,m)  = frac(l,m) + dfrac(l,m)

  IF (frac(l,m) <  frac_min) THEN
    dfrac(l,m) = dfrac(l,m) + (frac_min - frac(l,m))
    frac(l,m)  = frac_min

  ELSE IF (frac(l,m) > (space(l) - REAL(1 - crop(m)) * frac_a_loc(l))) THEN
    dfrac(l,m) = dfrac(l,m) +                                                  &
                 ((space(l) - REAL(1 - crop(m)) * frac_a_loc(l)) - frac(l,m))
    frac(l,m)  = space(l) - REAL(1 - crop(m)) * frac_a_loc(l)
  END IF

  ! Update the space remaining.
  space(l) = space(l) - frac(l,m) + frac_min

END DO

!-----------------------------------------------------------------------------
! Calculate the increment to the shrub fraction
!-----------------------------------------------------------------------------
DO t = 1,trif_pts
  l = trif_index(t)
  n = dom(l,3)

  fracn = frac(l,n)
  fracn = MAX(fracn,frac_seed)

  denom = r_gamma / fracn - forw * db_dfrac(l,n,n)
  denom = MAX(denom,denom_min)

  numer = b(l,n)
  DO k = 1,nnpft
    numer = numer + forw * (db_dfrac(l,n,k) * dfrac(l,k))
  END DO

  dfrac(l,n) = numer / denom
  frac(l,n)  = frac(l,n) + dfrac(l,n)

  IF (frac(l,n) <  frac_min) THEN
    dfrac(l,n) = dfrac(l,n) + (frac_min - frac(l,n))
    frac(l,n)  = frac_min
  ELSE IF (frac(l,n) > (space(l) - REAL(1 - crop(n)) * frac_a_loc(l))) THEN
    dfrac(l,n) = dfrac(l,n) +                                                  &
                 ((space(l) - REAL(1 - crop(n)) * frac_a_loc(l)) - frac(l,n))
    frac(l,n)  = space(l) - REAL(1 - crop(n)) * frac_a_loc(l)
  END IF

  ! Update the space remaining.
  space(l) = space(l) - frac(l,n) + frac_min
END DO

!-----------------------------------------------------------------------------
! Calculate the increments to the grass fractions
!-----------------------------------------------------------------------------
DO t = 1,trif_pts
  l = trif_index(t)
  n = dom(l,4)
  m = dom(l,5)

  fracn = frac(l,n)
  fracn = MAX(fracn,frac_seed)

  fracm = frac(l,m)
  fracm = MAX(fracm,frac_seed)

  p1 = r_gamma / fracn - forw * db_dfrac(l,n,n)
  p2 = r_gamma / fracm - forw * db_dfrac(l,m,m)
  q1 = -forw * db_dfrac(l,n,m)
  q2 = -forw * db_dfrac(l,m,n)
  r1 = b(l,n)
  r2 = b(l,m)
  DO k = 1,nnpft
    r1 = r1 + forw * (db_dfrac(l,n,k) * dfrac(l,k))
    r2 = r2 + forw * (db_dfrac(l,m,k) * dfrac(l,k))
  END DO

  ! Update the dominant grass PFT.
  numer      = r1 - (q1 / p2) * r2
  denom      = p1 - (q1 / p2) * q2
  denom      = MAX(denom,denom_min)
  dfrac(l,n) = numer / denom
  frac(l,n)  = frac(l,n) + dfrac(l,n)

  IF (frac(l,n) <  frac_min) THEN
    dfrac(l,n) = dfrac(l,n) + (frac_min - frac(l,n))
    frac(l,n)  = frac_min
  ELSE IF (frac(l,n) > (space(l) - REAL(1 - crop(n)) * frac_a_loc(l))) THEN
    dfrac(l,n) = dfrac(l,n) +                                                  &
                 ((space(l) -REAL(1 - crop(n)) * frac_a_loc(l)) - frac(l,n))
    frac(l,n)  = space(l)
  END IF

  ! Update the space remaining.
  space(l)   = space(l) - frac(l,n) + frac_min

  ! Update the less dominant grass PFT.
  numer      = r2 - q2 * dfrac(l,n)
  denom      = p2
  denom      = MAX(denom,denom_min)
  dfrac(l,m) = numer / denom
  frac(l,m) = frac(l,m) + dfrac(l,m)

  IF (frac(l,m) <  frac_min) THEN
    dfrac(l,m) = dfrac(l,m) + (frac_min - frac(l,m))
    frac(l,m)  = frac_min
  ELSE IF (frac(l,m) > (space(l) - REAL(1 - crop(m)) * frac_a_loc(l))) THEN
    dfrac(l,m) = dfrac(l,m) +                                                  &
                 ((space(l) -REAL(1 - crop(m)) * frac_a_loc(l)) - frac(l,m))
    frac(l,m)  = space(l)
  END IF

  ! Update the space remaining.
  space(l) = space(l) - frac(l,m) + frac_min

END DO

!-----------------------------------------------------------------------------
! Diagnose the new bare soil fraction
!-----------------------------------------------------------------------------
DO t = 1,trif_pts
  l = trif_index(t)
  frac(l,soil) = space(l)
  IF (frac(l,soil) < frac_min) THEN
    frac(l,soil) = frac_min
  END IF
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE compete

END MODULE compete_mod
