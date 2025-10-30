! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE cancap_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='CANCAP_MOD'

CONTAINS

! Subroutine CANCAP ------------------------------------------------

! Purpose : Calculate the heat capacity of a given PFT from its LAI

! -----------------------------------------------------------------
SUBROUTINE cancap (land_pts,veg_pts,veg_index,can_model,ft,                    &
                   veg_state,ht,lai,canhc,vfrac,                               &
                    !New arguments replacing USE statements
                    !crop_vars_mod (IN)
                    dvi_cpft)

USE pftparm, ONLY: kext, a_ws, eta_sl, a_wl, b_wl, lma, sigl
USE jules_vegetation_mod, ONLY: l_trait_phys, l_limit_canhc, l_red
USE jules_surface_mod, ONLY: hleaf,hwood,cmass
USE jules_surface_types_mod, ONLY: nnpft, ncpft
USE crop_utils_mod, ONLY:                                                      &
   leafc_from_prognostics, stemc_from_prognostics

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE veg3_field_mod, ONLY: veg_state_type

IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
land_pts                                                                       &
                           ! IN Total number of land points.
,veg_pts                                                                       &
                           ! IN Number of vegetated points.
,veg_index(land_pts)                                                           &
                           ! IN Index of vegetated points.
,can_model                  ! IN Swith for thermal vegetation
!                                 !    canopy

INTEGER, INTENT(IN) ::                                                         &
 ft                         ! IN Plant functional type.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 ht(land_pts)                                                                  &
                            ! IN Vegetation height (m).
,lai(land_pts)
                            ! IN Leaf area index.
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 canhc(land_pts)                                                               &
                            ! OUT Areal heat capacity of
!                                 !     vegetation canopy (J/K/m2).
,vfrac(land_pts)            ! OUT Fractional canopy coverage.

TYPE(veg_state_type), INTENT(IN OUT) :: veg_state

!New arguments replacing USE statements
!crop_vars_mod (IN)
REAL(KIND=real_jlslsm), INTENT(IN) :: dvi_cpft(land_pts,ncpft)

REAL(KIND=real_jlslsm) ::                                                      &
 lai_bal(land_pts)                                                             &
                            ! WORK Leaf area index in balanced
!                                 !      growth state.
,leaf                                                                          &
                            ! WORK Leaf biomass (kg C/m2).
,wood
                            ! WORK Woody biomass (kg C/m2).

INTEGER ::                                                                     &
 j,l                        ! WORK Loop counters.

REAL(KIND=real_jlslsm), PARAMETER :: canhc_max = 1.15e5
!                           ! Maximum limit for vegetation canopy heat
!                           ! capacity if l_limit_canhc is .TRUE

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CANCAP'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(j,l,leaf,wood)                                                   &
!$OMP SHARED(veg_pts,veg_index,canhc,vfrac,can_model,kext,lai,dvi_cpft,ht,     &
!$OMP        lai_bal,l_trait_phys,ft,nnpft,a_ws,eta_sl,a_wl,b_wl,lma,sigl,     &
!$OMP        hleaf,hwood,l_limit_canhc,l_red,veg_state)

!$OMP DO SCHEDULE(STATIC)
DO j = 1,veg_pts
  l = veg_index(j)
  canhc(l) = 0.0
  vfrac(l) = 0.0
END DO
!$OMP END DO

IF (can_model  ==  2) THEN
  !     Radiative canopy without heat capacity

!$OMP DO SCHEDULE(STATIC)
  DO j = 1,veg_pts
    l = veg_index(j)
    canhc(l) = 0.0
    vfrac(l) = 1.0 - EXP(-kext(ft) * lai(l))
  END DO
!$OMP END DO

ELSE IF (can_model == 3 .OR. can_model == 4) THEN
  !     Radiative canopy with heat capacity
!$OMP DO SCHEDULE(STATIC)
  DO j = 1,veg_pts
    l = veg_index(j)

    IF ( ft > nnpft ) THEN
      leaf = leafc_from_prognostics(ft - nnpft, dvi_cpft(l,ft - nnpft), lai(l))
      wood = stemc_from_prognostics(ft - nnpft, ht(l) )
    ELSE
      lai_bal(l) = ( a_ws(ft) * eta_sl(ft) * ht(l) /                           &
                     a_wl(ft) )**(1.0 / (b_wl(ft) - 1))
      ! lma replaces sigl
      IF ( l_trait_phys ) THEN
        leaf = cmass * lma(ft) * lai_bal(l)
        !kgC/m2 = kgC/kgleaf * gleaf/m2 * kg/g
      ELSE
        leaf = sigl(ft) * lai_bal(l)      !leaf=kgC/m2
      END IF
      wood = a_wl(ft) * (lai_bal(l)**b_wl(ft))
    END IF

    IF (l_red) THEN
      canhc(l) = hleaf * veg_state%leafC(l,ft) + hwood * veg_state%woodC(l,ft)
    ELSE
      canhc(l) = hleaf * leaf + hwood * wood
    END IF

    ! If using tree heights from alternative sources (such as SIMARD dataset),
    ! then the heat capacicy can become unrealisitcaly large for the tallest trees.
    ! Therefore, if l_limit_canhc is true, then set a limit based on the default
    ! broadleaf tree height of 19.01m
    IF (l_limit_canhc .AND.  canhc(l) > canhc_max ) THEN
      canhc(l) = canhc_max
    END IF
    vfrac(l) = 1.0 - EXP(-kext(ft) * lai(l))
  END DO
!$OMP END DO NOWAIT
END IF
!$OMP END PARALLEL

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE cancap
END MODULE cancap_mod
