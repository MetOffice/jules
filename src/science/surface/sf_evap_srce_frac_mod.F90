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

MODULE sf_evap_srce_frac_mod

!-----------------------------------------------------------------------------
! Description:
!   Calculate the fractional contribution of each evaporative source to the
!   total tile evaporative flux.
!
!   The different evaporative sources are: snow, canopy intercepted water,
!   bare soil/transpiration from vegetation and open water (i.e. lakes).
!
!   This code is based on how the total tile resistance factors are
!   set in sf_resist.
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

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SF_EVAP_SRCE_FRAC_MOD'

CONTAINS

SUBROUTINE sf_evap_srce_frac (land_pts, surft_pts, surft_index, flake,         &
 fracaero, snowdep_surft, dq, catch, cansnowtile, evap_srce_frac)

USE jules_water_tracers_mod, ONLY: n_evap_srce, evap_srce_snow,                &
                                   evap_srce_cano, evap_srce_soil,             &
                                   evap_srce_lake
USE jules_vegetation_mod,    ONLY: can_model

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
 land_pts                                                                      &
                       ! IN Total number of land points.
,surft_pts                                                                     &
                       ! IN Number of tile points.
,surft_index(land_pts)
                       ! IN Index of tile points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 flake(land_pts)                                                               &
                       ! IN Lake fraction
,fracaero(land_pts)                                                            &
                       ! IN Fraction of surface moisture flux with only
                       !    aerodynamic resistance
,snowdep_surft(land_pts)                                                       &
                       ! IN Snow depth (m)
,dq(land_pts)                                                                  &
                       ! IN Specific humidity difference between lowest
                       !    atmospheric level and the surface (Q1 - Q*)
,catch(land_pts)
                       ! IN Surface/canopy water capacity (km/m2)

LOGICAL, INTENT (IN) ::                                                        &
 cansnowtile           ! IN Switch for pft canopy snow model

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 evap_srce_frac(land_pts,n_evap_srce)
                       ! OUT Evaporative source fractional contribution to
                       !     total evaporation from tile

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

CHARACTER(LEN=*), PARAMETER :: RoutineName='SF_EVAP_SRCE_FRAC'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEL DO IF(surft_pts > 1)                                            &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(l,k,n_evap)                                                      &
!$OMP SHARED(surft_pts,surft_index,evap_srce_frac,                             &
!$OMP        flake,fracaero,dq,snowdep_surft,can_model,cansnowtile,catch)
DO k = 1,surft_pts
  l = surft_index(k)

  ! Set tile fractions for each evaporative source

  DO n_evap = 1, n_evap_srce
    evap_srce_frac(l,n_evap) = 0.0
  END DO

  IF (snowdep_surft(l) > 0.0) THEN
    IF (can_model == 4 .AND. cansnowtile) THEN
      ! Canopy snow model is used
      ! In this case, fracaero = 0.0
      evap_srce_frac(l,evap_srce_snow) = (1.0 - flake(l))
      evap_srce_frac(l,evap_srce_lake) = flake(l)
    ELSE
      ! Either non-vegatative tile or vegatative tile with no canopy snow
      ! model
      evap_srce_frac(l,evap_srce_snow) = fracaero(l) * (1.0 - flake(l))
      evap_srce_frac(l,evap_srce_soil) = (1.0-fracaero(l)) * (1.0 - flake(l))
      evap_srce_frac(l,evap_srce_lake) = flake(l)
    END IF
  ELSE
    IF (dq(l) < 0.0) THEN
      ! Evaporation is occurring
      evap_srce_frac(l,evap_srce_cano) = fracaero(l) * (1.0 - flake(l))
      evap_srce_frac(l,evap_srce_soil) = (1.0-fracaero(l)) * (1.0 - flake(l))
      evap_srce_frac(l,evap_srce_lake) = flake(l)
    ELSE
      ! Condensation is occurring
      ! Just aerodynamic resistance for all surfaces so no distinction
      ! between canopy and soil (fracaero = 1.0)
      IF (catch(l) > 0.0) THEN
        ! From canopy
        evap_srce_frac(l,evap_srce_cano) = (1.0 - flake(l))
        evap_srce_frac(l,evap_srce_soil) = 0.0
      ELSE
        ! From soil
        evap_srce_frac(l,evap_srce_cano) = 0.0
        evap_srce_frac(l,evap_srce_soil) = (1.0 - flake(l))
      END IF
      evap_srce_frac(l,evap_srce_lake) = flake(l)
    END IF
  END IF
END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN
END SUBROUTINE sf_evap_srce_frac
END MODULE sf_evap_srce_frac_mod
