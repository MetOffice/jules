! Description:
!     Updates deep soil temperatures in deep layers

MODULE bedrock_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='BEDROCK_MOD'

CONTAINS

SUBROUTINE bedrock (npnts,soil_pts,dzsoil,timestep,soil_index,                 &
                    tsoil,hcsoil,tsoil_deep_gb,hflux_in)

USE jules_soil_mod,   ONLY: ns_deep, hcapdeep, hcondeep, dzdeep
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
    ! number of land points
  soil_pts
    ! number of soil points

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  dzsoil,                                                                      &
    ! thickness of base soil layer (m).
  timestep
    ! model timestep (s).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN)  ::                                                        &
  soil_index(npnts)
    ! index of soil points

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  tsoil(npnts),                                                                &
    ! soil temp at base of column (Celsius)
  hcsoil(npnts)
    ! heat conductivity of base soil layer

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: tsoil_deep_gb(npnts,ns_deep)
    ! Deep soil temperature (K).

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  hflux_in(npnts)
    ! heat flux from base of soil column into bedrock layers

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: i, j, n  ! loop counter

REAL(KIND=real_jlslsm) ::                                                      &
  hctop,                                                                       &
    ! interpolated heat conductivity where bedrock joins soil
  dztop,                                                                       &
    ! interpolated layer thickness for heat transfer between base of soil
    ! and top of bedrock.
  tsoil_k
    ! temperature of base soil layer in Kelvin

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
    dtsd(i,ns_deep) = hcondeep * timestep * (tsoil_deep_gb(i,ns_deep-1) -      &
                      tsoil_deep_gb(i,ns_deep)) / (hcapdeep * dzdeep**2)
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
    tsoil_deep_gb(i,n) = MAX(tsoil_deep_gb(i,n) + dtsd(i,n),0.0)
    tsoil_deep_gb(i,n) = MIN(tsoil_deep_gb(i,n),10000.0)
  END DO

END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE bedrock
END MODULE bedrock_mod
