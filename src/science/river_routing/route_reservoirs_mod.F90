!******************************COPYRIGHT**************************************
! (c) UK Centre for Ecology & Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE  route_reservoirs_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

PRIVATE  ! Private scope by default
PUBLIC route_reservoirs

CONTAINS

!##############################################################################

SUBROUTINE route_reservoirs( dt, abstracted_res_rp,                            &
                                   res_cap_current,                            &
                                   res_catch,                                  &
                                   res_critical, res_flood,                    &
                                   res_emergency,                              &
                                   res_normal_release,                         &
                                   res_flood_release,                          &
                                   res_storage, reservoir_flow )

! Route runoff through reservoirs.

USE jules_rivers_mod, ONLY: dt_rivers, nstep_rivers

USE timestep_mod, ONLY: timestep

IMPLICIT NONE
!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  dt,                                                                          &
    ! Timestep of routing model (s)
  abstracted_res_rp,                                                           &
    ! Water abstracted from reservoirs (kg).
  res_cap_current,                                                             &
    ! Storage capacity of reservoirs (kg).
  res_catch,                                                                   &
    ! Upstream catchment area of reservoirs (m2).
  res_critical,                                                                &
    ! Critical storage threshold of reservoirs (kg).
  res_flood,                                                                   &
    ! Flood storage threshold of reservoirs (kg).
  res_emergency,                                                               &
    ! Emergency storage threshold of reservoirs (kg).
  res_normal_release,                                                          &
    ! Normal release rate from reservoirs (kg s-1).
  res_flood_release
    ! Flood release rate from reservoirs (kg s-1).

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  res_storage,                                                                 &
    ! Water stored in reservoirs (kg).
  reservoir_flow
    ! Flow in and out of reservoir (kg s-1)

!------------------------------------------------------------------------------
! Local scalar variables
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  k
    ! Release coefficient.

!end of header
!------------------------------------------------------------------------------

! Update reservoir storage with inflow and abstractions
res_storage = res_storage + reservoir_flow * dt                                &
                     - abstracted_res_rp

k = MAX( 0.0, 1.0 - (res_cap_current - res_storage) / 1000.0                   &
                                    / ( res_catch * 0.2 ) )

! Calculate reservoir release based on storage and inflow

! Release calculation if inflow is not extremely high
IF (reservoir_flow < res_flood_release) THEN

  ! Calculate outflow depending on storage level
  IF (res_storage <= res_critical) THEN
    reservoir_flow = res_normal_release *                                      &
                  res_storage / res_flood
  ELSE IF (res_storage <= res_emergency) THEN
    reservoir_flow = res_normal_release / 2.0 +                                &
    ( (res_storage - res_critical) /                                           &
      (res_emergency - res_critical) ) ** 2.0 *                                &
      ( res_flood_release - res_normal_release )
  ELSE
    reservoir_flow = res_flood_release
  END IF

  ! Release calculation if inflow is extremely high
ELSE

  ! Calculate outflow depending on storage level
  IF (res_storage <= res_critical) THEN
    reservoir_flow = res_normal_release *                                      &
                  res_storage / res_flood
  ELSE IF (res_storage <= res_flood) THEN
    reservoir_flow = res_normal_release / 2.0 +                                &
    (res_storage - res_critical) /                                             &
    (res_flood - res_critical) *                                               &
      ( res_flood_release - res_normal_release )
  ELSE IF (res_storage <= res_emergency) THEN
    reservoir_flow = res_flood_release + k *                                   &
    (res_storage - res_flood) /                                                &
    (res_emergency - res_flood) *                                              &
      ( reservoir_flow - res_flood_release )
  ELSE
    reservoir_flow = reservoir_flow
  END IF

END IF

! Ensure that release never exceeds storage
reservoir_flow = MIN( reservoir_flow, res_storage/dt )

! Update reservoir storage with release
res_storage = res_storage - reservoir_flow * dt

RETURN
END SUBROUTINE route_reservoirs

!##############################################################################

END MODULE route_reservoirs_mod
