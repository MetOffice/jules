! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE bvoc_emissions_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='BVOC_EMISSIONS_MOD'

CONTAINS
SUBROUTINE bvoc_emissions(land_pts,ft,veg_index,                               &
                          open_pts,open_index,clos_pts,clos_index,             &
                          lai,ci,gpp,tstar,                                    &
                          isoprene,terpene,methanol,acetone,                   &
                          !New arguments replacing USE statements
                          !crop_vars_mod (IN)
                          dvi_cpft)

USE c_bvoc, ONLY: atau, btau, f_tmax, t_ref

USE conversions_mod, ONLY: hour2sec => rhour_per_sec, rsec_per_hour

USE jules_vegetation_mod, ONLY: l_trait_phys

USE jules_surface_mod, ONLY: cmass

USE pftparm, ONLY: ci_st, gpp_st, ief, tef, mef, aef, sigl, lma

USE jules_surface_types_mod, ONLY: nnpft, ncpft

USE crop_utils_mod, ONLY: lma_from_prognostics

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Calculates emission of BVOCs (see Pacifico et al. (2011) Atm. Chem Phys.)
!
!   Also calculates emissions of (mono)terpenes, methanol and acetone.
!   Based on model developed by
!   Guenther et al. JGR 1995, V.100(D5) 8873-8892.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
                 ! IN Total number of land points
  veg_index(land_pts),                                                         &
                 ! IN Index of vegetated points
  ft,                                                                          &
                 ! IN Plant functional type.
  open_index(land_pts),                                                        &
                 ! IN Index of land points
                 !      with open stomata.
  open_pts,                                                                    &
                 ! IN Number of land points
                 !      with open stomata.
  clos_index(land_pts),                                                        &
                 ! IN Index of land points
                 !      with closed stomata.
  clos_pts
                 ! IN Number of land points
                 !      with closed stomata.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  ci(land_pts),                                                                &
                 ! IN Internal CO2 pressure (Pa)
  gpp(land_pts),                                                               &
                 ! IN Gross Primary Productivity
                 !     (kg C/m2/s).
  tstar(land_pts),                                                             &
                 ! IN Leaf temperature (K) = Surface temperature (K)
  lai(land_pts)
                 ! IN Leaf area index.

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  isoprene(land_pts),                                                          &
                 ! OUT Isoprene Emission Flux (kgC/m2/s)
  terpene(land_pts),                                                           &
                 ! OUT (Mono-)Terpene Emission Flux (kgC/m2/s)
  methanol(land_pts),                                                          &
                 ! OUT Methanol Emission Flux (kgC/m2/s)
  acetone(land_pts)
                 ! OUT Acetone Emission Flux (kgC/m2/s)

!New arguments replacing USE statements
!crop_vars_mod (IN)
REAL(KIND=real_jlslsm), INTENT(IN) :: dvi_cpft(land_pts,ncpft)

! Work variables
REAL(KIND=real_jlslsm) ::                                                      &
  f_co2(land_pts),                                                             &
                 ! WORK CO2 scaling factor
  f_t_isop(land_pts),                                                          &
                 ! WORK Temperature scaling factor for isoprene
  f_t_terp(land_pts)
                 ! WORK Temperature scaling factor for isoprene

INTEGER         :: first_nonBT_PFT

REAL(KIND=real_jlslsm) :: dw_area
                  ! WORK Dry weight of leaf per unit area (g DW/m2)

INTEGER :: m,l  ! WORK Loop counters

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='BVOC_EMISSIONS'

!-----------------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Initialise arrays
!-----------------------------------------------------------------------------
f_co2(:)    = 0.0
f_t_isop(:) = 0.0
f_t_terp(:) = 0.0

isoprene(:) = 0.0
terpene(:)  = 0.0
methanol(:) = 0.0
acetone(:)  = 0.0

IF (nnpft == 9) THEN
  first_nonBT_PFT = 4
ELSE IF (nnpft == 5) THEN
  first_nonBT_PFT = 2
ELSE
  ! future functionality to be added here.
  first_nonBT_PFT = 2
END IF

!-----------------------------------------------------------------------------
! Calculate BVOC emissions
!-----------------------------------------------------------------------------

DO m = 1,open_pts
  l = veg_index(open_index(m))

  ! Calculate mass of carbon in leaf per unit area (kgC/m2)
  IF ( ft > nnpft ) THEN
    ! the dw_area calculated here is the canopy average in kg per m2 leaf area
    dw_area = lma_from_prognostics(ft - nnpft, dvi_cpft(l,ft - nnpft)) * 1.0e03
  ELSE
    IF ( l_trait_phys ) THEN
      dw_area = lma(ft)  * 1.0e03
              !kg_dw/m2
    ELSE
      dw_area = sigl(ft) * 1.0e03 / cmass
              !kgC/m2 * kg_dw/kgC --> g_dw/m2
    END IF
  END IF

  !-----------------------------------------------------------------------------
  ! CO2 factor
  !-----------------------------------------------------------------------------
  f_co2(l) = ci_st(ft) / ci(l)

  !-----------------------------------------------------------------------------
  ! Temperature factors (eq A4b Arneth et al., 2007)
  !-----------------------------------------------------------------------------
  f_t_isop(l) = MIN(f_tmax, EXP(atau * (tstar(l) - t_ref)))
  f_t_terp(l) =             EXP(btau * (tstar(l) - t_ref))

  !-----------------------------------------------------------------------------
  ! Isoprene emission (Niinemets et al.,1999; Arneth et al.,2007)
  !-----------------------------------------------------------------------------
  isoprene(l) = MAX(0.0, ief(ft) * dw_area *                                   &
                       !           g_dw/m2 leaf
                       ! convert ugC to kgC
                         1.0e-09 *                                             &
                       ! convert hours to secs
                         hour2sec *                                            &
                       ! compute relative productivity factor
                         gpp(l) / gpp_st(ft) *                                 &
                       ! account for temperature sensitivity
                         f_t_isop(l) *                                         &
                       ! account for CO2 inhibition
                         f_co2(l))

  !-----------------------------------------------------------------------------
  ! (Mono-)Terpene emission
  ! (Niinemets et al.,1999; Arneth et al.,2007; Guenther et al., 1995)
  !-----------------------------------------------------------------------------
  IF ( ft < first_nonBT_PFT ) THEN
    terpene(l) =                                                               &
    !                  ... terpene emissions with PAR dependence
                       0.5 * (MAX(0.0, tef(ft) * dw_area *                     &
                              !                  g_dw/m2 leaf
                              ! convert ugC to kgC
                                1.0e-09 *                                      &
                              ! convert hours to secs
                                hour2sec *                                     &
                              ! compute relative productivity factor
                                gpp(l) / gpp_st(ft) *                          &
                              ! account for temperature sensitivity
                                f_t_isop(l))) +                                &
    !                  ... terpene emissions without PAR dependence
                       0.5 * (MAX(0.0, tef(ft) * dw_area * lai(l) *            &
                              !                  g_dw/m2 leaf
                              ! convert ugC to kgc
                                1.0e-09 *                                      &
                              ! convert hours to secs
                                hour2sec *                                     &
                              ! apply temperature sensitivity
                                f_t_terp(l)))
  ELSE
    terpene(l) = MAX(0.0, tef(ft) * dw_area * lai(l) *                         &
                        !           g_dw/m2 leaf
                        ! convert ugC to kgc
                          1.0e-09 *                                            &
                        ! convert hours to secs
                          hour2sec *                                           &
                        ! apply temperature sensitivity
                          f_t_terp(l))
  END IF

  !-----------------------------------------------------------------------------
  ! Methanol emission
  !-----------------------------------------------------------------------------
  methanol(l) = MAX(0.0, mef(ft) * dw_area * lai(l) *                          &
                       !           g_dw/m2 leaf
                       ! convert ugC to kgC
                         1.0e-09 *                                             &
                       ! convert hours to secs
                       ! To retain bit-level comparison this (and below for
                       ! acetone) has not been replaced by hour2sec .
                         1.0 / rsec_per_hour *                                 &
                       ! apply temperature sensitivity
                         f_t_terp(l))

  !-----------------------------------------------------------------------------
  ! Acetone emission
  !-----------------------------------------------------------------------------
  acetone(l) = MAX(0.0, aef(ft) * dw_area * lai(l) *                           &
                      !                  g_dw/m2 leaf
                      ! convert ugC to kgC
                        1.0e-09 *                                              &
                      ! convert hours to secs
                        1.0 / rsec_per_hour *                                  &
                      ! apply temperature sensitivity
                        f_t_terp(l))

END DO

DO m = 1,clos_pts
  l = veg_index(clos_index(m))
  isoprene(l) = 0.0
  terpene(l)  = 0.0
  methanol(l) = 0.0
  acetone(l)  = 0.0
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN

END SUBROUTINE bvoc_emissions
END MODULE bvoc_emissions_mod
