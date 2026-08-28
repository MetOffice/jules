! Description:
!     Updates deep soil temperatures in deep layers

MODULE bedrock_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='BEDROCK_MOD'

CONTAINS

SUBROUTINE bedrock (npnts,soil_pts,dzsoil,timestep,soil_index,                 &
                    tsoil,hcsoil,tsoil_deep_gb,hflux_in,dtsd_acc_gb)

USE jules_soil_mod,   ONLY: ns_deep, hcapdeep, hcondeep, dzdeep, hflux_geo
USE conversions_mod,  ONLY: zerodegc

USE parkind1,       ONLY: jprb, jpim
USE yomhook,        ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN)  ::                                                        &
  npnts,                                                                       &
    ! Number of land points
  soil_pts
    ! Number of soil points

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  dzsoil,                                                                      &
    ! Thickness of base soil layer (m).
  timestep
    ! Model timestep (s).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN)  ::                                                        &
  soil_index(npnts)
    ! Index of soil points

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  tsoil(npnts),                                                                &
    ! Soil temp at base of column (Celsius)
  hcsoil(npnts)
    ! Heat conductivity of base soil layer (W/m/K)

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  tsoil_deep_gb(npnts,ns_deep),                                                &
    ! Deep soil temperature (K).
  dtsd_acc_gb(npnts,ns_deep)
    ! Accumulated correction in deep soil (bedrock) temperature (K).

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  hflux_in(npnts)
    ! heat flux from base of soil column into bedrock layers (W/m2)

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: i, j, n  ! loop counter

REAL(KIND=real_jlslsm) ::                                                      &
  hctop,                                                                       &
    ! interpolated heat conductivity where bedrock joins soil (W/m/K)
  dztop,                                                                       &
    ! interpolated layer thickness for heat transfer between base of soil
    ! and top of bedrock (m).
  tsoil_k,                                                                     &
    ! temperature of base soil layer in Kelvin
  tsoil_deep_prev,                                                             &
    ! Previous value of deep soil temperature (K).
  dtsh_applied
    ! Change in value of deep soil temperature in this timestep (K).
!-----------------------------------------------------------------------------
! Local array variables.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  dtsd(npnts,ns_deep)
    ! temperature increment

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='BEDROCK'

!-----------------------------------------------------------------------------
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Calculate dztop (doesn't change spatially)
dztop = 0.5 * (dzdeep + dzsoil)

DO j = 1,soil_pts
  i = soil_index(j)

  ! Convert base soil temp to Kelvin
  tsoil_k = tsoil(i) + zerodegc

  !---------------------------------------------------------------------------
  ! Calculate the thermal conductivity at the top of the bedrock.
  !---------------------------------------------------------------------------
  hctop = (dzdeep * hcsoil(i) + dzsoil * hcondeep) / (dzdeep + dzsoil)

  ! Calculate the heat flux from the soil above.
  hflux_in(i) = hctop * (tsoil_k - tsoil_deep_gb(i,1)) / dztop

  !---------------------------------------------------------------------------
  ! Calculate the temperature increments to the bedrock layers.
  !---------------------------------------------------------------------------
  IF (ns_deep > 1) THEN
    ! bottom:
    dtsd(i,ns_deep) = timestep * ( hcondeep * (tsoil_deep_gb(i,ns_deep-1) -    &
            tsoil_deep_gb(i,ns_deep)) / dzdeep + hflux_geo ) /                 &
            (hcapdeep * dzdeep)
    ! top:
    dtsd(i,1) = timestep * ( hcondeep * (tsoil_deep_gb(i,2) -                  &
                tsoil_deep_gb(i,1)) / dzdeep + hflux_in(i) ) /                 &
                (hcapdeep * dzdeep)
  ELSE
    dtsd(i,1) = timestep * hflux_in(i) / (hcapdeep * dzdeep)
  END IF

  IF (ns_deep > 2) THEN
    DO n = 2,ns_deep-1
      dtsd(i,n) = timestep * hcondeep * (tsoil_deep_gb(i,n+1) +                &
                  tsoil_deep_gb(i,n-1) - 2 * tsoil_deep_gb(i,n)) /             &
                  (hcapdeep * dzdeep**2)
    END DO
  END IF

  !---------------------------------------------------------------------------
  ! Update the layer temperatures
  !---------------------------------------------------------------------------
  DO n = 1,ns_deep
    tsoil_deep_prev = tsoil_deep_gb(i,n)
    tsoil_deep_gb(i,n) = MAX(tsoil_deep_gb(i,n) + dtsd(i,n) +       &
               dtsd_acc_gb(i,n), 0.0)
    tsoil_deep_gb(i,n) = MIN(tsoil_deep_gb(i,n), 1000.0)
    ! Calculate cumulative numerical correction (avoids rounding error)
    dtsh_applied = tsoil_deep_gb(i,n) - tsoil_deep_prev
    dtsd_acc_gb(i,n) = dtsd(i,n) + dtsd_acc_gb(i,n) - dtsh_applied
  END DO

END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE bedrock
END MODULE bedrock_mod
