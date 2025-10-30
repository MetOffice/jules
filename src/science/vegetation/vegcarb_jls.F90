! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE vegcarb_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='VEGCARB_MOD'

CONTAINS

!-----------------------------------------------------------------------------
! Subroutine VEGCARB ---------------------------------------------------------
!
! Purpose : Updates carbon contents of the vegetation.
!
! ----------------------------------------------------------------------------
SUBROUTINE vegcarb (land_pts, trif_pts, n, trif_index,                         &
                    forw, r_gamma, phen,                                       &
                    g_leaf, n_avail, npp, resp_w,                              &
                    leaf, root, wood,                                          &
                    dleaf, droot, dwood, dcveg,                                &
                    exudate, n_demand_growth, n_demand_lit, n_demand_spread,   &
                    n_uptake_growth, n_uptake_spread, n_fertiliser, pc_s,      &
                    dvi_cpft,                                                  &
                    !New arguments replacing USE statements
                    ! trif_vars_mod
                    root_litc_pft, leaf_litc_pft, wood_litc_pft,               &
                    root_litn_pft, leaf_litn_pft, wood_litn_pft)


USE pftparm, ONLY: b_wl, g_leaf_0, kpar, lma, r_grow, sigl
USE trif, ONLY: crop, g_root, g_wood, lai_max, lai_min, retran_l, retran_r
USE jules_surface_mod, ONLY: cmass
USE jules_vegetation_mod, ONLY: l_trait_phys, l_Nitrogen, l_trif_crop
USE jules_surface_types_mod, ONLY: npft

USE CN_utils_mod, ONLY: calc_n_comps_triffid
USE plant_growth_n_mod, ONLY: plant_growth_n
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
    ! Plant functional type.

INTEGER, INTENT(IN) ::                                                         &
  trif_index(land_pts)
    ! Indices of land points on which TRIFFID may operate.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  forw,                                                                        &
    ! Forward timestep weighting.
  r_gamma
    ! Inverse timestep (/360days).

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  phen(land_pts),                                                              &
    ! Phenological state.
  dvi_cpft(:,:)
    !  Development index for crop tiles

!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  g_leaf(land_pts),                                                            &
    ! Turnover rate for leaf and fine root biomass (/360days).
  n_avail(land_pts),                                                           &
    ! Available Nitrogen (kg n/m2).
  npp(land_pts),                                                               &
    ! Net primary productivity (kg C/m2/360days).
  resp_w(land_pts),                                                            &
    ! Wood maintenance respiration (kg C/m2/360days).
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
    ! Leaf biomass (kg C/m2).
  droot(land_pts),                                                             &
    ! Root biomass (kg C/m2).
  dwood(land_pts),                                                             &
    ! Woody biomass (kg C/m2).
  dcveg(land_pts),                                                             &
    ! Change in vegetation carbon during the timestep (kg C/m2).
  exudate(land_pts),                                                           &
    !(kg C/m2/360day).
  n_demand_growth(land_pts),                                                   &
    !(kg N/m2/360day).
  n_demand_lit(land_pts),                                                      &
    !(kg N/m2/360day).
  n_demand_spread(land_pts),                                                   &
    !(kg N/m2/360day).
  n_uptake_growth(land_pts),                                                   &
    !(kg N/m2/360day).
  n_uptake_spread(land_pts),                                                   &
    !(kg N/m2/360day).
  n_fertiliser(land_pts),                                                      &
    !(kg N/m2/360day).
  pc_s(land_pts)
    ! Net carbon flux available for spreading (kg C/m2/360days).

!New arguments replacing USE statements
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: root_litc_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: leaf_litc_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: wood_litc_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: root_litn_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: leaf_litn_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: wood_litn_pft(land_pts,npft)

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
 l,t  ! Loop counters

REAL(KIND=real_jlslsm) ::                                                      &
  dfpar_dlai,                                                                  &
    ! Rate of change of FPAR with leaf area index.
  dlai,                                                                        &
    ! Increment to the leaf area index.
  dlamg_dlai,                                                                  &
    ! Required for calculation of the equilibrium increments.
  dlit_dlai,                                                                   &
    ! Required for calculation of the equilibrium increments.
  drespw_dlai,                                                                 &
    ! Rate of change of RESP_W with leaf area index.
  fpar,                                                                        &
    ! PAR interception factor.
  lambda_g,                                                                    &
    ! Fraction of NPP available for spreading.
  n_root, n_leaf, n_stem, n_leaf0, n_leaf1,                                    &
    ! N_pools (kg N).
  root_cn,                                                                     &
    ! Root C:N Ratio.
  stem_cn,                                                                     &
    ! Stem C:N Ratio.
  leaf_cn,                                                                     &
    ! Leaf C:N Ratio.
  plant_cn
    ! Plant C:N Ratio.

REAL(KIND=real_jlslsm) ::                                                      &
  dnpp_dlai(land_pts),                                                         &
    ! Rate of change of NPP with leaf area index (kg C/m2/360days/LAI).
  dpc_dlai(land_pts),                                                          &
    ! Rate of change of PC with leaf area index (kg C/m2/360days/LAI).
  dpcg_dlai(land_pts),                                                         &
    ! Rate of change of PC_G with leaf area index (kg C/m2/360days/LAI).
  lai(land_pts),                                                               &
    ! Balanced leaf area index.
  lit_c_l(land_pts),                                                           &
    ! Local rate of Carbon Litter production (kg C/m2/360days).
  pc(land_pts),                                                                &
    ! Net carbon flux available to vegetation (kg C/m2/360days)
  pc_g(land_pts),                                                              &
    ! Net carbon flux available for growth (kg C/m2/360days).
  n_avail_growth(land_pts),                                                    &
    !(kg N/m2/360day).
  n_demand_phenology(land_pts),                                                &
    !(kg N/m2/360day).
  dphen(land_pts)
    ! Change in Phen due to phenology since last TRIFFID call.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='VEGCARB'
!-----------------------------------------------------------------------------
!end of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DO t = 1,trif_pts
  l = trif_index(t)

  ! lma replaces sigl
  IF ( l_trait_phys ) THEN
    lai(l) = leaf(l) / (cmass * lma(n))
  ELSE
    lai(l) = leaf(l) / sigl(n)
  END IF

  ! Diagnose dphen for leaf turnover
  dphen(l) = g_leaf(l) / r_gamma

  IF ( l_nitrogen .AND. g_leaf(l)< 0.0 ) THEN
    ! Background turnover only
    g_leaf(l) = g_leaf_0(n) * phen(l)
  END IF

  !---------------------------------------------------------------------------
  ! Calculate the local production rate for carbon litter
  !---------------------------------------------------------------------------
  lit_c_l(l) = g_leaf(l) * leaf(l) + g_root(n) * root(l)                       &
                                   + g_wood(n) * wood(l)

  ! leaf N at full leaf
  CALL calc_n_comps_triffid(l, n, 1.0, lai(l), wood(l), root(l),               &
                            n_leaf1, n_root, n_stem, dvi_cpft)
  ! leaf N without phenological change
  CALL calc_n_comps_triffid(l, n, phen(l) + dphen(l), lai(l), wood(l),         &
                            root(l), n_leaf0, n_root, n_stem, dvi_cpft)
  ! leaf N after phenology
  CALL calc_n_comps_triffid(l, n, phen(l), lai(l), wood(l), root(l),           &
                            n_leaf, n_root, n_stem, dvi_cpft)

  root_cn  = root(l) / n_root
  stem_cn  = wood(l) / n_stem
  leaf_cn  = leaf(l) / n_leaf1

  root_litc_pft(l,n) = g_root(n) * root(l)
  leaf_litc_pft(l,n) = g_leaf(l) * leaf(l)
  wood_litc_pft(l,n) = g_wood(n) * wood(l)

  root_litn_pft(l,n) = (1.0 - retran_r(n)) * root_litc_pft(l,n) / root_cn
  leaf_litn_pft(l,n) = (1.0 - retran_l(n)) * leaf_litc_pft(l,n) / leaf_cn
  wood_litn_pft(l,n) = wood_litc_pft(l,n) / stem_cn

  !---------------------------------------------------------------------------
  ! Diagnose the net local carbon flux into the vegetation
  !---------------------------------------------------------------------------
  pc(l) = npp(l) - lit_c_l(l)
  !---------------------------------------------------------------------------
  ! Variables required for the implicit and equilibrium calculations
  !---------------------------------------------------------------------------
  dlit_dlai    = (g_leaf(l) * leaf(l) + g_root(n) * root(l)) / lai(l)          &
                 + b_wl(n) * g_wood(n) * wood(l) / lai(l)

  fpar         = (1.0 - EXP(-kpar(n) * lai(l))) / kpar(n)
  dfpar_dlai   = EXP(-kpar(n) * lai(l))

  dnpp_dlai(l) = npp(l) * dfpar_dlai / fpar                                    &
                 + (1.0 - r_grow(n)) * resp_w(l)                               &
                   *(dfpar_dlai / fpar - b_wl(n) / lai(l))

  lambda_g     = 1.0 - (lai(l) - lai_min(n))  / (lai_max(n) - lai_min(n))

  dlamg_dlai   = -1.0 / (lai_max(n) - lai_min(n))

  pc_g(l)      = lambda_g * npp(l) - lit_c_l(l)
  dpcg_dlai(l) = lambda_g * dnpp_dlai(l)                                       &
                 + dlamg_dlai * npp(l)                                         &
                 - dlit_dlai
  dpc_dlai(l) = dnpp_dlai(l) - dlit_dlai

  !---------------------------------------------------------------------------
  ! Nitrogen required for litter loss
  ! Reduces N available for growth, possibly leading to negative growth
  ! in the absence of enough N
  !---------------------------------------------------------------------------
  n_demand_lit(l)       = root_litn_pft(l,n) + leaf_litn_pft(l,n)              &
                          + wood_litn_pft(l,n)
  n_demand_phenology(l) = (n_leaf - n_leaf0) * r_gamma
  n_demand_lit(l)       = n_demand_lit(l) + n_demand_phenology(l)

  n_avail(l)            = n_avail(l) - n_demand_lit(l) / r_gamma
  !---------------------------------------------------------------------------
  ! Split available N between growing and spreading
  !---------------------------------------------------------------------------
  n_avail_growth(l) = lambda_g * n_avail(l)

END DO

!-----------------------------------------------------------------------------
! Update vegetation carbon contents
! Split available N between growing and spreading
! Excess C is considered exudate
!-----------------------------------------------------------------------------
DO t = 1,trif_pts
  l = trif_index(t)
  dcveg(l) = leaf(l) + root(l) + wood(l)
END DO

CALL plant_growth_n (land_pts, trif_pts, n, trif_index,                        &
                     forw, r_gamma, dpcg_dlai, pc_g, lit_c_l, phen,            &
                     n_avail_growth, leaf, root, wood,                         &
                     dleaf, droot, dwood, exudate,                             &
                     n_demand_growth, n_uptake_growth, n_fertiliser, dvi_cpft)

DO t = 1,trif_pts
  l = trif_index(t)
  dcveg(l)   = leaf(l) + root(l) + wood(l) - dcveg(l)
  n_avail(l) = n_avail(l) - n_uptake_growth(l) / r_gamma
END DO

!-----------------------------------------------------------------------------
! Diagnose the carbon available for spreading and apply implicit
! corrections to the driving fluxes.
!-----------------------------------------------------------------------------
DO t = 1,trif_pts
  l = trif_index(t)

  ! lma replaces sigl
  IF ( l_trait_phys ) THEN
    dlai = leaf(l) / (cmass * lma(n)) - lai(l)
  ELSE
    dlai = leaf(l) / sigl(n) - lai(l)
  END IF
  pc_s(l) = pc(l) + forw * dpc_dlai(l) * dlai - dcveg(l) * r_gamma             &
            - exudate(l)

  CALL calc_n_comps_triffid(l, n, phen (l), lai(l), wood(l), root(l),          &
                            n_leaf, n_root, n_stem, dvi_cpft)

  plant_cn = (leaf(l) + root(l) + wood(l)) / (n_leaf + n_root + n_stem)

  !---------------------------------------------------------------------------
  ! N demand for spreading a function of whole plant C:N ratio
  !---------------------------------------------------------------------------
  n_demand_spread(l) = pc_s(l) / plant_cn
  n_uptake_spread(l) = n_demand_spread(l)

  !---------------------------------------------------------------------------
  ! Limit spreading if N limiting
  !---------------------------------------------------------------------------
  IF ( (l_trif_crop .AND. crop(n) == 1) .AND.                                  &
       (n_demand_spread(l) / r_gamma > n_avail(l)) ) THEN
    n_fertiliser(l) = n_fertiliser(l) +                                        &
                      ((n_demand_spread(l) / r_gamma - n_avail(l)) * r_gamma)
    n_avail(l)      = n_demand_spread(l) / r_gamma
  END IF

  IF (l_nitrogen .AND. n_demand_spread(l) / r_gamma > n_avail(l)) THEN
    n_uptake_spread(l) = n_avail(l) * r_gamma
    exudate(l)         = exudate(l) + pc_s(l)
    pc_s(l)            = n_uptake_spread(l) * plant_cn
    exudate(l)         = exudate(l) - pc_s(l)
  END IF

  drespw_dlai = resp_w(l) * b_wl(n) / lai(l)

  npp(l)      = npp(l) + forw * dnpp_dlai(l) * dlai
  resp_w(l)   = resp_w(l) + forw * drespw_dlai * dlai

END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE vegcarb

END MODULE vegcarb_mod
