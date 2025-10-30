!******************************COPYRIGHT****************************************
! (c) British Antarctic Survey and University of Bristol, 2023.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT****************************************

MODULE sf_resist_wtrac_mod

!-----------------------------------------------------------------------------
! Description:
!   Set resistance factors for water tracers (which will include the kinetic
!   fractionation factors for water isotopes in the future).
!
!   Resistance factors are set for the four different evaporative sources:
!   snow, canopy intercepted water, bare soil/transpiration from vegetation
!   and open water (i.e. lakes).
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in Surface
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!
!-----------------------------------------------------------------------------

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SF_RESIST_WTRAC_MOD'

CONTAINS

SUBROUTINE sf_resist_wtrac (i_wt, n, land_pts, surft_pts, surft_index,         &
 snowdep_surft, resfs, dq, catch, evap_srce_frac, cansnowtile,                 &
 resf_wtrac)

USE jules_water_tracers_mod, ONLY: n_evap_srce, evap_srce_snow,                &
                                   evap_srce_cano, evap_srce_soil,             &
                                   evap_srce_lake
USE jules_vegetation_mod,    ONLY: can_model
USE jules_surface_types_mod, ONLY: lake

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
 i_wt                                                                          &
                       ! IN Water tracer index
,n                                                                             &
                       ! IN Tile index
,land_pts                                                                      &
                       ! IN Total number of land points.
,surft_pts                                                                     &
                       ! IN Number of tile points.
,surft_index(land_pts)
                       ! IN Index of tile points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 snowdep_surft(land_pts)                                                       &
                       ! IN Snow depth (m)
,resfs(land_pts)                                                               &
                       ! IN Combined soil, stomatal and aerodynamic
                       !     resistance factor for fraction 1-fracaero.
,dq(land_pts)                                                                  &
                       ! IN Specific humidity difference between lowest
                       !    atmospheric level and the surface (Q1 - Q*)
,catch(land_pts)                                                               &
                       ! IN Surface/canopy water capacity (kg/m2)
,evap_srce_frac(land_pts,n_evap_srce)
                       ! IN Evaporative source fractional contribution to
                       !     total evaporation from tile

LOGICAL, INTENT (IN) ::                                                        &
 cansnowtile           ! IN Switch for pft canopy snow model

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 resf_wtrac(land_pts,n_evap_srce)
                       ! OUT Resitance factor for water tracers for individual
                       !     evaporative source types

! Local variables

INTEGER ::                                                                     &
 k                                                                             &
             ! Loop counter (tile field index)
,l                                                                             &
             ! Loop counter (land point field index)
,n_evap
             ! Loop counter (evaporative sources index)

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SF_RESIST_WTRAC'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEL DO IF(surft_pts > 1)                                            &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(l,k,n_evap)                                                      &
!$OMP SHARED(surft_pts,surft_index,i_wt,evap_srce_frac,dq,resf_wtrac,          &
!$OMP        snowdep_surft,catch,can_model,cansnowtile,resfs,n,lake)
DO k = 1,surft_pts
  l = surft_index(k)

  ! Set resistance factors for water tracers
  DO n_evap = 1, n_evap_srce
    ! Initialise to 0.0 so that there are only non-zero fluxes from
    ! the evaporative surfaces set below
    resf_wtrac(l,n_evap) = 0.0
  END DO

  ! Snow sublimation
  ! No kinetic fractionation for sublimation
  IF (snowdep_surft(l) > 0.0) THEN
    IF (can_model == 4 .AND. cansnowtile) THEN
      ! Canopy snow model and snow sublimation has resistance factor < 1
      resf_wtrac(l,evap_srce_snow) = resfs(l)
    ELSE
      ! For all non-vegetative tiles + vegetative tiles when no canopy snow
      !  model
      resf_wtrac(l,evap_srce_snow) = 1.0
    END IF
  END IF

  ! Soil evapotranspiration (Bare soil evaporation + Transpiration)
  ! Evaporation is limited by soil moisture availability and stomatal
  ! resistance.
  ! Kinetic fractionation will be added here for isotopes
  IF (snowdep_surft(l) > 0.0 .OR. dq(l) < 0.0) THEN
    ! Evaporation with no snow, or either sublimation or deposition with snow
    resf_wtrac(l,evap_srce_soil) = resfs(l) * 1.0
  ELSE
    ! Condensation with no snow (No additional resistance)
    resf_wtrac(l,evap_srce_soil) = 1.0
  END IF

  ! Canopy intercepted water
  IF (catch(l) > 0.0) THEN
    resf_wtrac(l,evap_srce_cano) = 1.0
  END IF

  ! Lake evaporation
  IF (n == lake) THEN
    ! Only set for lake tile
    resf_wtrac(l,evap_srce_lake) = 1.0
  END IF
END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN
END SUBROUTINE sf_resist_wtrac
END MODULE sf_resist_wtrac_mod
