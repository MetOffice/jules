! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE lotka_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='LOTKA_MOD'

CONTAINS

!-----------------------------------------------------------------------------
! Subroutine LOTKA -----------------------------------------------------------
!
! Purpose : Updates fractional coverage of each functional type.
!           Based on the Lotka-Volterra equations of interspecies
!           competition.
!
! ----------------------------------------------------------------------------
SUBROUTINE lotka (land_pts, trif_pts, trif_index,                              &
                  forw, r_gamma, c_veg, frac_agric, frac_agr_prev,             &
                  lai, pc_s, frac, frac_na,                                    &
                  frac_nofire, dfrac, dfrac_na, dfrac_nofire,g_burn_pft)

USE jules_surface_types_mod, ONLY: npft, nnpft, ntype, soil
USE pftparm, ONLY: a_wl, a_ws, b_wl, eta_sl
USE jules_vegetation_mod, ONLY: frac_min, pow
USE trif, ONLY: crop, g_area
USE compete_mod, ONLY: compete

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
    ! Indices of land points on which TRIFFID may operate

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  forw,                                                                        &
    ! Forward timestep weighting.
  r_gamma
    ! Inverse timestep (/360days).

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  c_veg(land_pts,npft),                                                        &
    ! Carbon content of vegetation (kg C/m2).
  frac_agric(land_pts),                                                        &
    ! Fraction of agriculture.
  frac_agr_prev(land_pts),                                                     &
  lai(land_pts,npft),                                                          &
    ! Leaf area index.
  pc_s(land_pts,npft)
    ! Net carbon flux available for spreading (kg C/m2/360days).

!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT).
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
    ! prior to the land use change, with fire.
  dfrac_nofire(land_pts,npft)
    ! Increment to the areal fraction during the timestep,
    ! with land use change, but without fire.

!New arguments replacing USE statements
REAL(KIND=real_jlslsm), INTENT(IN) :: g_burn_pft(land_pts,npft)

!-----------------------------------------------------------------------------
! Local integer variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
 k,l,m,n,t  ! Loop counters.

INTEGER ::                                                                     &
 dom(land_pts,nnpft) ! Dominance hierarchy.

!-----------------------------------------------------------------------------
! Local real variables.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  diff_sum,                                                                    &
    ! Difference divided by sum for competing canopy heights.
  hc1,hc2,hc3,hc4
    ! Competing canopy heights (m).

REAL(KIND=real_jlslsm) ::                                                      &
  b(land_pts,nnpft),                                                           &
    ! Mean rate of change of vegetation fraction over the timestep
    ! (kg C/m2/360days).
  db_dfrac(land_pts,nnpft,nnpft),                                              &
    ! Rate of change of b with vegetation fraction.
  com(land_pts,nnpft,nnpft),                                                   &
    ! Coefficients representing the influence of one type (second argument)
    ! on another (first argument).
  nosoil(land_pts),                                                            &
    ! Fractional area not available to vegetation.
  space(land_pts,nnpft),                                                       &
    ! Space available for invasion.
  g_dist(land_pts,nnpft)
    ! Area disturbance.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='LOTKA'

!-----------------------------------------------------------------------------
!end of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Initialiase frac variables.
!-----------------------------------------------------------------------------
DO n = 1,ntype
  DO l = 1,land_pts
    frac_na(l,n)      = frac(l,n)
    frac_nofire(l,n)  = frac(l,n)
  END DO
END DO

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

DO t = 1,trif_pts
  l = trif_index(t)

  hc1 = a_wl(1) / (a_ws(1) * eta_sl(1)) * (lai(l,1)**(b_wl(1) - 1.0))
  hc2 = a_wl(2) / (a_ws(2) * eta_sl(2)) * (lai(l,2)**(b_wl(2) - 1.0))
  diff_sum = (hc1 - hc2) / (hc1 + hc2)

  com(l,1,2) = 1.0 / (1.0 + EXP(pow * diff_sum))    ! BT vs NT
  com(l,1,3) = 0.0                          ! BT vs C3G
  com(l,1,4) = 0.0                          ! BT vs C4G
  com(l,1,5) = 0.0                          ! BT vs S

  com(l,2,1) = 1.0 - com(l,1,2)             ! NT vs BT
  com(l,2,3) = 0.0                          ! NT vs C3G
  com(l,2,4) = 0.0                          ! NT vs C4G
  com(l,2,5) = 0.0                          ! NT vs S

  hc3 = a_wl(3) / (a_ws(3) * eta_sl(3)) * (lai(l,3)**(b_wl(3) - 1.0))
  hc4 = a_wl(4) / (a_ws(4) * eta_sl(4)) * (lai(l,4)**(b_wl(4) - 1.0))
  diff_sum = (hc3 - hc4) / (hc3 + hc4)

  com(l,3,4) = 1.0 / (1.0 + EXP(pow * diff_sum)) ! C3G vs C4G
  com(l,4,3) = 1.0 - com(l,3,4)                  ! C4G vs C3G

  com(l,5,3) = 0.0                           ! S vs C3G
  com(l,5,4) = 0.0                           ! S vs C4G

  IF (hc1  >=  hc2) THEN
    dom(l,1) = 1
    dom(l,2) = 2
  ELSE IF (hc1  <   hc2) THEN
    dom(l,1) = 2
    dom(l,2) = 1
  END IF

  dom(l,3) = 5

  IF (hc3  >=  hc4) THEN
    dom(l,4) = 3
    dom(l,5) = 4
  ELSE IF (hc3  <   hc4) THEN
    dom(l,4) = 4
    dom(l,5) = 3
  END IF

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
      space(l,n) = space(l,n) - com(l,n,m) * frac(l,m)
    END DO
  END DO
END DO

!-----------------------------------------------------------------------------
! Calculate the variables required for the implicit calculation.
! Divide the update equation by FRAC to eliminate the (unstable)
! bare soil solution.
!-----------------------------------------------------------------------------
DO n = 1,nnpft
  DO t = 1,trif_pts
    l = trif_index(t)
    b(l,n) = pc_s(l,n) * space(l,n) / c_veg(l,n) - g_dist(l,n)

    DO m = 1,nnpft
      db_dfrac(l,n,m) = -com(l,n,m) * pc_s(l,n) / c_veg(l,n)
    END DO
  END DO
END DO

!-----------------------------------------------------------------------------
! Update the areal fractions
!-----------------------------------------------------------------------------
CALL compete (land_pts, trif_pts, trif_index, dom,                             &
              forw, r_gamma, b, db_dfrac, nosoil,                              &
              frac_agric, frac, dfrac)

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
                                 - frac_min * REAL(nnpft - k)
  END DO
END DO

DO n = 1,nnpft
  DO m = 1,nnpft
    DO t = 1,trif_pts
      l = trif_index(t)
      space(l,n) = space(l,n) - com(l,n,m) * frac_na(l,m)
    END DO
  END DO
END DO

!-----------------------------------------------------------------------------
! Calculate the variables required for the implicit calculation.
! Divide the update equation by FRAC to eliminate the (unstable)
! bare soil solution.
!-----------------------------------------------------------------------------
DO n = 1,nnpft
  DO t = 1,trif_pts
    l = trif_index(t)
    b(l,n) = pc_s(l,n) * space(l,n) / c_veg(l,n) - g_dist(l,n)

    DO m = 1,nnpft
      db_dfrac(l,n,m) = - com(l,n,m) * pc_s(l,n) / c_veg(l,n)
    END DO
  END DO
END DO

!-----------------------------------------------------------------------------
! Update the areal fractions
!-----------------------------------------------------------------------------
CALL compete (land_pts, trif_pts, trif_index, dom,                             &
              forw, r_gamma, b, db_dfrac, nosoil,                              &
              frac_agr_prev, frac_na, dfrac_na)

!-----------------------------------------------------------------------------
! Calculate litter without fire
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
      space(l,n) = space(l,n) - com(l,n,m) * frac_nofire(l,m)
    END DO
  END DO
END DO

!-----------------------------------------------------------------------------
! Calculate the variables required for the implicit calculation.
! Divide the update equation by FRAC to eliminate the (unstable)
! bare soil solution.
!-----------------------------------------------------------------------------
DO n = 1,nnpft
  DO t = 1,trif_pts
    l = trif_index(t)
    b(l,n) = pc_s(l,n) * space(l,n) / c_veg(l,n) - g_area(n)

    DO m = 1,nnpft
      db_dfrac(l,n,m) = - com(l,n,m) * pc_s(l,n) / c_veg(l,n)
    END DO
  END DO
END DO

!-----------------------------------------------------------------------------
! Update the areal fractions
!-----------------------------------------------------------------------------
CALL compete (land_pts, trif_pts, trif_index, dom,                             &
              forw, r_gamma, b, db_dfrac, nosoil,                              &
              frac_agric, frac_nofire, dfrac_nofire)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN
END SUBROUTINE lotka

END MODULE lotka_mod
