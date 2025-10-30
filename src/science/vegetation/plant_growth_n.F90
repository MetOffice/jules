! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE plant_growth_n_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='PLANT_GROWTH_N_MOD'

CONTAINS

! Subroutine plant_growth_n --------------------------------------------------
!
! Purpose : Increments leaf, root and wood carbon.
! Andy Wiltshire March 2014
! ----------------------------------------------------------------------------

SUBROUTINE plant_growth_n (land_pts, trif_pts, n, trif_index,                  &
                           forw, r_gamma, dpcg_dlai, pc_g, lit_c_l, phen,      &
                           n_avail, leaf, root, wood,                          &
                           dleaf, droot, dwood, exudate,                       &
                           n_demand_growth, n_uptake_growth, n_fertiliser,     &
                           dvi_cpft)

USE descent, ONLY: denom_min
USE pftparm, ONLY: a_wl, b_wl, lma, sigl
USE trif, ONLY: crop, lai_max, lai_min
USE jules_vegetation_mod, ONLY: l_trait_phys,l_nitrogen,l_trif_crop
USE jules_surface_mod, ONLY: cmass
USE CN_utils_mod, ONLY: calc_n_comps_triffid

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Total number of land points.
  trif_pts,                                                                    &
    ! Number of points on which TRIFFID may operate.
  n
    ! Vegetation type.

INTEGER, INTENT(IN) ::                                                         &
  trif_index(land_pts)
    ! Indices of land points on which TRIFFID may operate.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  forw,                                                                        &
    ! Forward timestep weighting.
  r_gamma
    ! Inverse timestep (/360days).

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  dpcg_dlai(land_pts),                                                         &
    ! Rate of change of PC_G with leaf area index (kg C/m2/360days/LAI).
  pc_g(land_pts),                                                              &
    ! Net carbon flux available for growth (kg C/m2/360days).
  lit_c_l(land_pts),                                                           &
    ! Local rate of Carbon Litter production (kg C/m2/360days).
  phen(land_pts),                                                              &
    ! Phenological state.
  dvi_cpft(:,:)
    !  Development index for crop tiles

!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  n_avail(land_pts),                                                           &
    ! Plant Available Nitrogen (kg N/m2).
  leaf(land_pts),                                                              &
    ! Leaf biomass (kg C/m2).
  root(land_pts),                                                              &
    ! Root biomass (kg C/m2).
  wood(land_pts)
    ! Woody biomass (kg C/m2).

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  dleaf(land_pts),                                                             &
  droot(land_pts),                                                             &
  dwood(land_pts)
    ! Increments to leaf, root and woody biomass (kg C/m2).

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  exudate(land_pts),                                                           &
    ! Exudate (kgC/m2/360day).
  n_demand_growth(land_pts),                                                   &
    ! (kg N/m2/360day).
  n_uptake_growth(land_pts),                                                   &
    ! (kg N/m2/360day).
  n_fertiliser(land_pts)
    ! (kg N/m2/360day).

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  l,t,                                                                         &
    ! Loop counters
  it
    ! Number of tries towards solution.

REAL(KIND=real_jlslsm) ::                                                      &
  denom,                                                                       &
    ! Denominator of update equation.
  dl_dw,                                                                       &
    ! Rate of change of leaf carbon with wood carbon.
  dlai_dw,                                                                     &
    ! Rate of change of leaf area index with wood carbon (LAI m2/kg C).
  dr_dw,                                                                       &
    ! Rate of change of root carbon with wood carbon.
  numer,                                                                       &
    ! Numerator of the update equation.
  wood_max,                                                                    &
    ! Maximum wood carbon (kg C/m2).
  wood_min,                                                                    &
    ! Minimum wood carbon (kg C/m2).
  lai,                                                                         &
    ! Balanced LAI.
  dleaf_pot,                                                                   &
    ! Unlimited inc leaf C (kgC m-2).
  dwood_pot,                                                                   &
    ! Unlimited inc wood leaf C (kgC m-2).
  droot_pot,                                                                   &
    ! Unlimited inc root leaf C (kgC m-2).
  n_leaf,                                                                      &
    ! Leaf N (kgN m-2).
  n_root,                                                                      &
    ! Root N (kgN m-2).
  n_stem,                                                                      &
    ! Stem N (kgN m-2).
  nit_pot,                                                                     &
    ! Unlimited N demand (kgN m-2).
  x1,                                                                          &
    ! Minimum bracket of the root.
  x2,                                                                          &
    ! Maximum bracket of the root.
  rtbis,                                                                       &
    ! Root of the bisection.
  dx,                                                                          &
    ! Bisection.
  fmid,                                                                        &
    ! Bisection.
  xmid,                                                                        &
    ! Bisection.
  n_plant_pot,                                                                 &
    ! Unlimited plant N (kgN m-2).
  n_root_pot,                                                                  &
    ! Unlimited root N (kgN m-2).
  n_stem_pot,                                                                  &
    ! Unlimited stem N (kgN m-2).
  n_leaf_pot,                                                                  &
    ! Unlimited leaf N (kgN m-2).
  lai_pot
    ! Unlimited Balanced LAI.

REAL(KIND=real_jlslsm) ::                                                      &
  n_veg(land_pts),                                                             &
    ! Vegetation N content (kg n/m2).
  n_veg_old(land_pts)
    ! Vegetation N content (kg n/m2).

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='PLANT_GROWTH_N'

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Initialisation.
!-----------------------------------------------------------------------------
exudate(:)      = 0.0
n_fertiliser(:) = 0.0

DO t = 1,trif_pts
  l = trif_index(t)

  !---------------------------------------------------------------------------
  ! Calculate the unlimited increment to the wood carbon
  !---------------------------------------------------------------------------
  dl_dw = leaf(l) / (b_wl(n) * wood(l))
  dr_dw = dl_dw
  IF (l_trait_phys) THEN
    dlai_dw = dl_dw / (cmass * lma(n))
                      ! gC/gleaf * gleaf/m2
  ELSE
    dlai_dw = dl_dw / sigl(n)
  END IF

  numer = pc_g(l)
  denom = (1.0 + dl_dw + dr_dw) * r_gamma - forw * dlai_dw * dpcg_dlai(l)
  denom = MAX(denom,denom_min)

  dwood_pot = numer / denom
  !---------------------------------------------------------------------------
  ! Ensure that the local leaf area index does not drop below its
  ! minimum value or exceed its maximum value.
  !---------------------------------------------------------------------------
  wood_min = a_wl(n) * lai_min(n)**b_wl(n)
  wood_max = a_wl(n) * lai_max(n)**b_wl(n)

  dwood_pot = MAX((wood_min - wood(l)),dwood_pot)
  dwood_pot = MIN((wood_max - wood(l)),dwood_pot)

  !---------------------------------------------------------------------------
  ! Diagnose the unlimited increments to leaf and root carbon
  !---------------------------------------------------------------------------
  IF ( l_trait_phys ) THEN
    dleaf_pot = (cmass * lma(n))                                               &
                * ((wood(l) + dwood_pot) / a_wl(n))**(1.0 / b_wl(n)) - leaf(l)
    lai       = leaf(l) / (cmass * lma(n))   ! Balanced LAI
  ELSE
    dleaf_pot = sigl(n) * ((wood(l) + dwood_pot) / a_wl(n))**(1.0 / b_wl(n))   &
                - leaf(l)
    lai       = leaf(l) / sigl(n)          ! Balanced LAI
  END IF
  droot_pot = dleaf_pot

  CALL calc_n_comps_triffid(l, n, phen(l), lai, wood(l), root(l),              &
                            n_leaf, n_root, n_stem, dvi_cpft)

  n_veg(l) = n_leaf + n_root + n_stem

  IF ( l_trait_phys ) THEN
    lai_pot = (leaf(l) + dleaf_pot) / (cmass * lma(n))
  ELSE
    lai_pot = (leaf(l) + dleaf_pot) / sigl(n)
  END IF
  CALL calc_n_comps_triffid(l, n,phen(l), lai_pot, wood(l) + dwood_pot,        &
                            root(l) + droot_pot, n_leaf_pot, n_root_pot,       &
                            n_stem_pot, dvi_cpft)

  n_plant_pot        = n_leaf_pot + n_root_pot + n_stem_pot
  nit_pot            = n_plant_pot - n_veg(l)
  n_demand_growth(l) = nit_pot * r_gamma
  n_uptake_growth(l) = n_demand_growth(l)

  IF ( (l_trif_crop .AND. crop(n) == 1) .AND. (nit_pot > n_avail(l)) ) THEN
    n_fertiliser(l) = nit_pot - n_avail(l)
    n_avail(l)      = nit_pot
    n_fertiliser(l) = n_fertiliser(l) * r_gamma
  END IF

  !--------------------------------------------------------------------------
  ! So, if demand is greater than availbility we need to limit growth.
  ! Veg can only lose as much C as local lit_c - all NPP used as exudates
  ! More C can be lost through contration
  ! N Limitation does not force the plant to use C as exudates
  !--------------------------------------------------------------------------
  IF (nit_pot >  n_avail(l) .AND. l_nitrogen) THEN

    x1    = -1.0 * lit_c_l(l)
    x2    = pc_g(l)  !maximum bracket of the root
    rtbis = x1       !root of the bisection
    dx    = x2 - x1

    it   = 0  !number of tries towards solution
    fmid = EPSILON(1.0) + 1.0

    DO WHILE (ABS(fmid) >  EPSILON(1.0) .AND. it <= 20)
      ! Abort search if >20 iterations needed to find solution
      it = it + 1

      dx    = dx * 0.5
      xmid  = rtbis + dx

      numer = xmid
      denom = (1.0 + dl_dw + dr_dw) * r_gamma - forw * dlai_dw * dpcg_dlai(l)
      denom = MAX(denom,denom_min)

      dwood(l) = numer / denom
      !-----------------------------------------------------------------------
      ! Ensure that the local leaf area index does not drop below its
      ! minimum value or exceed its maximum value.
      !-----------------------------------------------------------------------
      wood_min = a_wl(n) * lai_min(n)**b_wl(n)
      wood_max = a_wl(n) * lai_max(n)**b_wl(n)

      dwood(l) = MAX((wood_min - wood(l)),dwood(l))
      dwood(l) = MIN((wood_max - wood(l)),dwood(l))

      !-----------------------------------------------------------------------
      ! Diagnose the increments to leaf and root carbon
      !-----------------------------------------------------------------------
      IF (l_trait_phys) THEN
        dleaf(l)   = cmass * lma(n)                                            &
                     * ((wood(l) + dwood(l)) / a_wl(n))**(1.0 / b_wl(n))       &
                     - leaf(l)
        lai_pot = (leaf(l) + dleaf(l)) / (cmass * lma(n))
      ELSE
        dleaf(l)   = sigl(n) * ((wood(l) + dwood(l)) /                         &
                                 a_wl(n))**(1.0 / b_wl(n))                     &
                     - leaf(l)
        lai_pot = (leaf(l) + dleaf(l)) / sigl(n)
      END IF
      droot(l) = dleaf(l)

      ! Calculate the nitrogen required to satisfy the demand in new growth.
      CALL calc_n_comps_triffid(l, n, phen(l), lai_pot, wood(l) + dwood(l),    &
                                root(l) + droot(l), n_leaf_pot,                &
                                n_root_pot, n_stem_pot, dvi_cpft)

      n_plant_pot = n_leaf_pot + n_root_pot + n_stem_pot
      nit_pot     = n_plant_pot - n_veg(l)
      fmid        = nit_pot - n_avail(l)

      IF (fmid <  0.0) rtbis = xmid

    END DO !bisection

    !-------------------------------------------------------------------------
    ! Update carbon contents
    !-------------------------------------------------------------------------
    leaf(l) = leaf(l) + dleaf(l)
    root(l) = root(l) + droot(l)
    wood(l) = wood(l) + dwood(l)

    ! Put the excess C into exudates. This is added directly to soil
    ! respiration.
    exudate(l)= (dleaf_pot + droot_pot + dwood_pot) -                          &
                (dleaf(l) + droot(l) + dwood(l))

  ELSE
    dleaf(l) = dleaf_pot
    droot(l) = droot_pot
    dwood(l) = dwood_pot

    !-------------------------------------------------------------------------
    ! Update carbon contents
    !-------------------------------------------------------------------------
    leaf(l)    = leaf(l) + dleaf_pot
    root(l)    = root(l) + droot_pot
    wood(l)    = wood(l) + dwood_pot
    exudate(l) = 0.0

  END IF

  IF ( l_trait_phys ) THEN
    lai = leaf(l) / (cmass * lma(n))
  ELSE
    lai = leaf(l) / sigl(n)
  END IF
  CALL calc_n_comps_triffid(l, n, phen(l), lai, wood(l), root(l),              &
                            n_leaf, n_root, n_stem, dvi_cpft)

  n_veg_old(l)       = n_veg(l)
  n_veg(l)           = n_leaf + n_root + n_stem

  n_avail(l)         = n_avail(l) - (n_veg(l) - n_veg_old(l))
  n_uptake_growth(l) = (n_veg(l) - n_veg_old(l)) * r_gamma

  exudate(l)         = exudate(l) * r_gamma
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE plant_growth_n

END MODULE plant_growth_n_mod
