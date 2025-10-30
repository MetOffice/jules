! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

!  SUBROUTINE SIEVE-----------------------------------------------------------

!  PURPOSE : TO CALCULATE THE THROUGHFALL OF WATER FALLING
!            THROUGH THE SURFACE CANOPY

!  SUITABLE FOR SINGLE COLUMN MODEL USE

!  DOCUMENTATION : UNIFIED MODEL DOCUMENTATION PAPER NO 25
!                  SECTION (3B(II)), EQN(P252.9)

!-----------------------------------------------------------------------------

MODULE sieve_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SIEVE_MOD'

CONTAINS

SUBROUTINE sieve ( npnts, surft_pts, curr_surft, nsurft, n_wtrac_jls,          &
                   timestep, surft_index, area, can_cpy, r, r_wtrac, frac,     &
                   can_wcnt, tot_tfall, tot_tfall_surft,                       &
                   can_wcnt_wtrac, tot_tfall_wtrac, tot_tfall_surft_wtrac )

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

USE jules_surface_mod, ONLY: l_flake_model
USE jules_surface_types_mod, ONLY: lake

USE jules_water_tracers_mod, ONLY: l_wtrac_jls

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  npnts,                                                                       &
    ! Total number of land points.
  surft_pts,                                                                   &
    ! Number of tile points.
  curr_surft,                                                                  &
    ! Current surface tile
  nsurft,                                                                      &
    ! Number of tiles (only used by water tracers)
  n_wtrac_jls
    ! Number of water tracers

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  timestep
    ! Timestep (s).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  surft_index(npnts)
    ! Index of tile points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  area(npnts),                                                                 &
    ! Fractional area of gridbox over which water falls (%).
  can_cpy(npnts),                                                              &
    ! Canopy capacity (kg/m2).
  r(npnts),                                                                    &
    ! Water fall rate (kg/m2/s).
  r_wtrac(npnts,n_wtrac_jls),                                                  &
    ! Water tracer fall rate (kg/m2/s).
  frac(npnts)
    ! Tile fraction.

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  can_wcnt(npnts),                                                             &
    ! Canopy water content (kg/m2).
  tot_tfall(npnts),                                                            &
    ! Cumulative canopy throughfall (kg/m2/s).
  tot_tfall_surft(npnts),                                                      &
    ! Throughfall contributions for this tile (kg/m2/s).
  can_wcnt_wtrac(npnts,nsurft,n_wtrac_jls),                                    &
    ! Canopy water tracer content (kg/m2) for all tiles.
    ! (Note, unlike the water equivalent, this is the full field containing
    !  all tiles)
  tot_tfall_wtrac(npnts,n_wtrac_jls),                                          &
    ! Cummulative canopy water tracer throughfall (kg/m2/s).
  tot_tfall_surft_wtrac(npnts,nsurft,n_wtrac_jls)
    ! Water tracer throughfall contributions for all tiles (kg/m2/s).
    ! (Note, unlike the water equivalent, this is the full field containing
    !  all tiles)

!-----------------------------------------------------------------------------
! Local scalar variables:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, j, i_wt
    ! Loop counters:
    ! i for land point
    ! j for tile point
    ! i_wt for water tracers

REAL(KIND=real_jlslsm) ::                                                      &
  aexp,                                                                        &
    ! Used in calculation of exponential in throughfall formula.
  can_ratio,                                                                   &
    ! CAN_WCNT / CAN_CPY
  smallp,                                                                      &
    ! Small positive number << 1.
  smallestp
    ! Smallest +ve real which can be represented.

!-----------------------------------------------------------------------------
! Local array variables:
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  tfall(npnts)
    ! Local throughfall (kg/m2/s).

REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
  tfall_wtrac(:,:)
    ! Local water tracer throughfall (kg/m2/s).

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SIEVE'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
!-----------------------------------------------------------------------------

smallestp = TINY(1.0)
smallp    = EPSILON( r )

!$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(j, i, aexp, can_ratio)                 &
!$OMP SHARED(surft_pts, surft_index, can_cpy, r, smallp, area, timestep,       &
!$OMP        smallestp, can_wcnt, tfall, tot_tfall, frac, tot_tfall_surft,     &
!$OMP        l_flake_model, lake, curr_surft )                                 &
!$OMP SCHEDULE(STATIC)
DO j = 1,surft_pts
  i = surft_index(j)
  IF ( can_cpy(i)  >  0.0 .AND. r(i) > smallp ) THEN
    aexp = area(i) * can_cpy(i) / (r(i) * timestep)
    ! Only calculate if AEXP is small enough to avoid underflow
    IF (aexp < -LOG(smallestp)) THEN
      aexp = EXP(-aexp)
    ELSE
      aexp = 0.0
    END IF

    can_ratio = can_wcnt(i) / can_cpy(i)

    can_ratio = MIN(can_ratio,1.0)
    tfall(i) = r(i) * ((1.0 - can_ratio) * aexp + can_ratio)
  ELSE
    tfall(i) = r(i)
  END IF

  can_wcnt(i)  = can_wcnt(i) + (r(i) - tfall(i)) * timestep

  IF ( .NOT. ((l_flake_model) .AND. (curr_surft == lake)) ) THEN
    tot_tfall(i) = tot_tfall(i) + frac(i) * tfall(i)
  END IF

  ! Increment the tile throughfall for this precip type
  tot_tfall_surft(i) = tot_tfall_surft(i) + tfall(i)

END DO
!$OMP END PARALLEL DO

! Repeat for water tracers
IF (l_wtrac_jls) THEN

  ALLOCATE(tfall_wtrac(npnts,n_wtrac_jls))

!$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(j, i, i_wt)                            &
!$OMP SHARED(surft_pts, surft_index, can_cpy, r, r_wtrac, smallp, timestep,    &
!$OMP        can_wcnt_wtrac, tfall, tfall_wtrac, tot_tfall_wtrac, frac,        &
!$OMP        tot_tfall_surft_wtrac, l_flake_model, lake, curr_surft,           &
!$OMP        n_wtrac_jls)                                                      &
!$OMP SCHEDULE(STATIC)
  DO j = 1,surft_pts
    i = surft_index(j)
    IF ( can_cpy(i)  >  0.0 .AND. r(i) > smallp ) THEN
      DO i_wt = 1, n_wtrac_jls
        tfall_wtrac(i,i_wt) = (r_wtrac(i,i_wt)/r(i)) * tfall(i)
      END DO
    ELSE
      DO i_wt = 1, n_wtrac_jls
        tfall_wtrac(i,i_wt) = r_wtrac(i,i_wt)
      END DO
    END IF

    DO i_wt = 1, n_wtrac_jls
      can_wcnt_wtrac(i,curr_surft,i_wt)  = can_wcnt_wtrac(i,curr_surft,i_wt)   &
                                + (r_wtrac(i,i_wt) - tfall_wtrac(i,i_wt))      &
                                * timestep

      IF ( .NOT. ((l_flake_model) .AND. (curr_surft == lake)) ) THEN
        tot_tfall_wtrac(i,i_wt) = tot_tfall_wtrac(i,i_wt)                      &
                                  + frac(i) * tfall_wtrac(i,i_wt)
      END IF

      ! Increment the tile throughfall for this precip type
      tot_tfall_surft_wtrac(i,curr_surft,i_wt) =                               &
                                     tot_tfall_surft_wtrac(i,curr_surft,i_wt)  &
                                      + tfall_wtrac(i,i_wt)
    END DO
  END DO
!$OMP END PARALLEL DO

  DEALLOCATE(tfall_wtrac)
END IF ! l_wtrac_jls

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE sieve
END MODULE sieve_mod
