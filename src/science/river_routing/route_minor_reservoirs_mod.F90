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

MODULE  route_minor_reservoirs_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

PRIVATE  ! Private scope by default
PUBLIC route_minor_reservoirs

CONTAINS

!##############################################################################

SUBROUTINE route_minor_reservoirs( abstracted_minor_res_rp, minor_res_capacity,&
                                   minor_res_frac, rivers_boxareas_rp,         &
                                   minor_res_storage, surf_runoff )

! Route runoff through minor reservoirs.

USE jules_rivers_mod, ONLY: dt_rivers, np_rivers, nstep_rivers

USE timestep_mod, ONLY: timestep

IMPLICIT NONE
!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  abstracted_minor_res_rp(np_rivers),                                          &
    ! Water abstracted from minor reservoirs (kg).
  minor_res_capacity(np_rivers),                                               &
     ! Storage capacity of minor reservoirs (kg).
  minor_res_frac(np_rivers),                                                   &
    ! Catchment area of minor reservoirs as fraction of grid box.
  rivers_boxareas_rp(np_rivers)
    ! Area of each river grid pixel (m2).

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  minor_res_storage(np_rivers),                                                &
    ! Water stored in minor reservoirs (kg).
  surf_runoff(np_rivers)
    ! Average rate of surface runoff since last call (kg m-2 s-1)

!------------------------------------------------------------------------------
! Local scalar variables
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  i  ! Loop counter.

REAL(KIND=real_jlslsm) ::                                                      &
  dt ! Routing timestep length (s).

!end of header
!------------------------------------------------------------------------------

! Calculate timestep length.
dt = REAL(nstep_rivers) * timestep

DO i = 1, np_rivers

  ! Add a fraction of surface runoff to storage in minor reservoirs and reduce
  ! the amount of runoff entering rivers by the same amount.
  minor_res_storage(i) = minor_res_storage(i)                                  &
                         + surf_runoff(i)                                      &
                         * minor_res_frac(i) * rivers_boxareas_rp(i) * dt
  surf_runoff(i) = surf_runoff(i) * ( 1.0 - minor_res_frac(i) )
  ! Remove water abstracted.
  minor_res_storage(i) = minor_res_storage(i) - abstracted_minor_res_rp(i)

  ! If reservoir overflows, reduce storage to capacity and add the overflow to
  ! runoff entering rivers.
  IF ( minor_res_storage(i) > minor_res_capacity(i) ) THEN
    surf_runoff(i) = surf_runoff(i)                                            &
                     + ( minor_res_storage(i) - minor_res_capacity(i) )        &
                       / ( rivers_boxareas_rp(i) * dt )
    minor_res_storage(i) = minor_res_capacity(i)
  END IF

END DO

RETURN
END SUBROUTINE route_minor_reservoirs

!##############################################################################

END MODULE route_minor_reservoirs_mod
