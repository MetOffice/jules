! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE woodprod_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CONTAINS

!-----------------------------------------------------------------------------
! Subroutine WOODPROD --------------------------------------------------------
!
! Purpose : Update wood products pool, driven by forest clearance
!           from land use change
!
! Allocation rules between the pools are:
!
!             QUICK            MED      SLOW
!             1 yr + burn     10 yr     100 yr
!     NL      60%             30%       10%
!     BL      60%             40%       0%
!     SH      80%             20%       0%
!
!  These values are from McGuire et al 2001 "Carbon balance of the
!  terrestrial biosphere in the 20th century:.." Global Biogeochemical
!  Cycles, Vol 15(1): 183-206, table 3, recalculated to make use of
!  TRIFFID simulated ratios of above ground : below ground carbon storage.
!
! ----------------------------------------------------------------------------
SUBROUTINE woodprod (land_pts, trif_pts, trif_index,                           &
                     r_gamma,                                                  &
                     lit_c_ag,                                                 &
                     wood_prod_fast, wood_prod_med,                            &
                     wood_prod_slow,                                           &
                     wp_fast_in, wp_med_in, wp_slow_in,                        &
                     wp_fast_out, wp_med_out, wp_slow_out)

USE jules_surface_types_mod, ONLY: nnpft, npft

USE trif, ONLY: alloc_fast, alloc_med, alloc_slow

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
  lit_c_ag(land_pts,nnpft)
    ! Litter carbon (kg C/m2/360days).

!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  wood_prod_fast(land_pts),                                                    &
    ! Fast-turnover wood product C pool (kg m-2).
  wood_prod_med(land_pts),                                                     &
    ! Medium-turnover wood product C pool (kg m-2).
  wood_prod_slow(land_pts),                                                    &
    ! Slow-turnover wood product C pool (kg m-2).
  wp_fast_in(land_pts),                                                        &
    ! Fast-turnover wood product C pool input (kg m-2 [360days]-1).
  wp_med_in(land_pts),                                                         &
    ! Fast-turnover wood product C pool input (kg m-2 [360days]-1).
  wp_slow_in(land_pts)
    ! Fast-turnover wood product C pool input (kg m-2 [360days]-1).

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  wp_fast_out(land_pts),                                                       &
    ! Fast-turnover wood product C pool output (kg m-2 [360days]-1).
  wp_med_out(land_pts),                                                        &
    ! Fast-turnover wood product C pool output (kg m-2 [360days]-1).
  wp_slow_out(land_pts)
    ! Fast-turnover wood product C pool output (kg m-2 [360days]-1).

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: k,l,t ! Loop counters

!-----------------------------------------------------------------------------
! Local array variables.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) :: wpresp(3)
  ! Wood products psuedo respiration rates.
  ! Loss rate constants of the pools.
DATA wpresp  / 1.0,1.0e-1,1.0e-2 /

!-----------------------------------------------------------------------------
!end of header

DO t = 1,trif_pts
  l = trif_index(t)

  ! Calculate fluxes

  ! Wood products respiration (loss)
  wp_fast_out(l) = wood_prod_fast(l) * wpresp(1)
  wp_med_out(l)  = wood_prod_med(l)  * wpresp(2)
  wp_slow_out(l) = wood_prod_slow(l) * wpresp(3)

  ! Gain
  DO k = 1,npft
    wp_fast_in(l) = wp_fast_in(l) + (lit_c_ag(l,k) * alloc_fast(k))
    wp_med_in(l)  = wp_med_in(l)  + (lit_c_ag(l,k) * alloc_med(k))
    wp_slow_in(l) = wp_slow_in(l) + (lit_c_ag(l,k) * alloc_slow(k))
  END DO


  ! Update Wood product pools
  wood_prod_fast(l) = wood_prod_fast(l) - (wp_fast_out(l)) / r_gamma
  wood_prod_med(l)  = wood_prod_med(l)  - (wp_med_out(l)) / r_gamma
  wood_prod_slow(l) = wood_prod_slow(l) - (wp_slow_out(l)) / r_gamma

  wood_prod_fast(l) = MAX(wood_prod_fast(l),0.0)
  wood_prod_med(l)  = MAX(wood_prod_med(l),0.0)
  wood_prod_slow(l) = MAX(wood_prod_slow(l),0.0)

  wood_prod_fast(l) = wood_prod_fast(l) + wp_fast_in(l) / r_gamma
  wood_prod_med(l)  = wood_prod_med(l)  + wp_med_in(l) / r_gamma
  wood_prod_slow(l) = wood_prod_slow(l) + wp_slow_in(l) / r_gamma

END DO

RETURN
END SUBROUTINE woodprod

END MODULE woodprod_mod
