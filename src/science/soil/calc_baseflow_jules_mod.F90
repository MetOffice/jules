! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    SUBROUTINE CALC_BASEFLOW_jules------------------------------------

! Description:
!     Calculates subsurface runoff (aka baseflow).

!Called from hydrol
!Not called directly from the UM repo (but via surf_couple_extra and hydrol)

MODULE calc_baseflow_jules_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='CALC_BASEFLOW_JULES_MOD'

CONTAINS

SUBROUTINE calc_baseflow_jules( npnts, nshyd, soil_pts, soil_index,            &
                                b, fexp, sthf, ti_mean, zdepth, zw, ksz,       &
                                qbase, qbase_l, top_crit )

USE jules_hydrology_mod, ONLY:                                                 &
  ti_max,zw_max,l_baseflow_corr

USE jules_soil_mod, ONLY:                                                      &
  l_vg_soil

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
USE jules_print_mgr, ONLY:                                                     &
  jules_message,                                                               &
  jules_print

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  npnts,                                                                       &
    ! Number of gridpoints.
  nshyd,                                                                       &
    ! Number of soil moisture levels.
  soil_pts
    ! Number of soil points.

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  soil_index(npnts)   ! Array of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  b(npnts,nshyd),                                                              &
    ! Brooks & Corey exponent.
  fexp(npnts),                                                                 &
    ! Decay factor in Sat. Conductivity in deep LSH/TOPMODEL layer.
  sthf(npnts,nshyd),                                                           &
    ! Frozen soil moisture content of each layer
  ti_mean(npnts),                                                              &
    ! Mean topographic index.
  zdepth(0:nshyd),                                                             &
    ! Lower soil layer boundary depth (m).
  zw(npnts),                                                                   &
    ! Water table depth (m).
  ksz(npnts,0:nshyd)
    ! Saturated hydraulic conductivity for each layer (kg/m2/s).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  qbase(npnts),                                                                &
    ! Total base flow (kg/m2/s).
  qbase_l(npnts,nshyd+1),                                                      &
    ! Base flow from each layer (kg/m2/s).
  top_crit(npnts)
    ! Critical topographic index required to calc surface saturation frac.

!-----------------------------------------------------------------------------
! Local scalars:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, j,                                                                        &
    ! Loop counters.
  n
    ! Tile loop counter.

! Variables to limit printout
INTEGER :: fail_count
INTEGER, PARAMETER :: max_print = 10

!-----------------------------------------------------------------------------
! Local arrays:
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  bracket(npnts,nshyd+1),                                                      &
    ! work: 1-(1-S^(b+1))^(1/(b+1))
  ksfz(npnts,nshyd+1),                                                         &
    ! Function of sat. hydraulic conductivity,
    ! frozen soil and mean topographic index.
  qbase_max(npnts),                                                            &
    ! Max possible base flow (kg/m2/s).
  qbase_max_l(npnts,nshyd+1),                                                  &
    ! Max possible base flow from each level (kg/m2/s).
  qbase_min(npnts)
    ! Residual base flow at zw_max (kg/m2/s).

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CALC_BASEFLOW_JULES'

!-----------------------------------------------------------------------------
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)


!-----------------------------------------------------------------------
! Initialise print counter.
!-----------------------------------------------------------------------
fail_count =  0

!-----------------------------------------------------------------------------
! Initialise TOP_CRIT to maximum value.
! Initialise baseflow components to zero.
!-----------------------------------------------------------------------------
!
! In the following OMP region, max_print is a parameter
!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(SHARED)                                                          &
!$OMP PRIVATE(j,i,n)

!$OMP DO SCHEDULE(STATIC)
DO j = 1,soil_pts
  i = soil_index(j)

  top_crit(i)  = ti_max
  qbase(i)     = 0.0
  qbase_max(i) = 0.0
  DO n = 1,nshyd+1
    qbase_l(i,n) = 0.0
  END DO

  !-----------------------------------------------------------------------
  ! Calculate a layer-dependent variable which is dependent on
  ! effective saturated conductivity:
  !-----------------------------------------------------------------------
  IF ( l_vg_soil .AND. l_baseflow_corr ) THEN

    DO n = 1 , nshyd
      IF ( sthf(i,n) <  1.0 ) THEN
        ! bracket = ( 1 - S^(b+1) ) ^ ( 1/(b+1) )
        bracket(i,n) = ( 1.0 - ( 1.0 - sthf(i,n) )**( b(i,n) + 1.0 ) )         &
                       ** ( 1.0 / (b(i,n) + 1.0) )
        ksfz(i,n) = 0.5 * ( ksz(i,n-1) + ksz(i,n) )                            &
                    * ( (1.0 - sthf(i,n))**0.5 )                               &
                    * ( 1.0 - bracket(i,n) ) * ( 1.0 - bracket(i,n) )          &
                    * EXP( -ti_mean(i) )
      ELSE
        ksfz(i,n) = 0.0
      END IF
    END DO

    IF ( sthf(i,nshyd) <  1.0 ) THEN
      ! Using the params from the lowest layer, so bracket has already been
      ! calculated in the last step of the previous loop
      ksfz(i,nshyd+1) = ( ksz(i,nshyd) / fexp(i) )                             &
                        * ( (1.0 - sthf(i,nshyd))**0.5 )                       &
                        * ( 1.0 - bracket(i,nshyd) )                           &
                        * ( 1.0 - bracket(i,nshyd) )                           &
                        * EXP( -ti_mean(i) )
    ELSE
      ksfz(i,nshyd+1) = 0.0
    END IF

  ELSE

    !   l_vg_soil = .FALSE.
    DO n = 1,nshyd
      IF ( sthf(i,n) <  1.0 ) THEN
        ksfz(i,n) = 0.5 * (ksz(i,n-1) + ksz(i,n))                              &
                    * (1.0 - sthf(i,n))** (2.0 * b(i,n) + 3.0)                 &
                    * EXP(-ti_mean(i))
      ELSE
        ksfz(i,n) = 0.0
      END IF
    END DO
    IF ( sthf(i,nshyd) <  1.0 ) THEN
      ksfz(i,nshyd+1) = ksz(i,nshyd) / fexp(i)                                 &
                        * (1.0 - sthf(i,nshyd))** (2.0 * b(i,nshyd) + 3.0)     &
                        * EXP(-ti_mean(i))
    ELSE
      ksfz(i,nshyd+1) = 0.0
    END IF
  END IF  !  l_vg_soil

  IF ( EXP( -fexp(i) * (zw_max - zdepth(nshyd) ) ) >  0.05 ) THEN
!$OMP CRITICAL (failcount)
    fail_count = fail_count + 1
    IF (fail_count <= max_print) THEN
      WRITE(jules_message,*)'CB_J: maximum water table depth is too small!'
      CALL jules_print('calc_baseflow_jules',jules_message)
      WRITE(jules_message,*)'at soil point ',i,fexp(i),zw_max,zdepth(nshyd)
      CALL jules_print('calc_baseflow_jules',jules_message)
      WRITE(jules_message,*)EXP(-fexp(i) * (zw_max - zdepth(nshyd)))
      CALL jules_print('calc_baseflow_jules',jules_message)
      WRITE(jules_message,*)'try zw_max>',-LOG(0.05) / fexp(i) + zdepth(nshyd)
      CALL jules_print('calc_baseflow_jules',jules_message)
    END IF
!$OMP END CRITICAL (failcount)
  END IF

  !-----------------------------------------------------------------------
  ! Calculate base flow between maximum allowed water table depth and
  ! "infinity":
  !-----------------------------------------------------------------------
  qbase_min(i) = ksfz(i,nshyd+1)                                               &
              * EXP( -fexp(i) * (zw_max - zdepth(nshyd)) )

  !-----------------------------------------------------------------------
  ! Calculate maximum possible and actual base flow for each layer:
  !-----------------------------------------------------------------------

  DO n = 1,nshyd

    qbase_max_l(i,n) = ksfz(i,n) * ( zdepth(n) - zdepth(n-1) )

    IF ( zw(i) <= zdepth(n-1) ) qbase_l(i,n) = qbase_max_l(i,n)

    IF ( zw(i) <  zdepth(n) .AND. zw(i) >  zdepth(n-1) ) THEN
      qbase_l(i,n) = ksfz(i,n) * ( zdepth(n) - zw(i) )
    END IF

    IF ( n == 1 .AND. zw(i) <  zdepth(n) ) THEN
      qbase_l(i,n) = ksfz(i,n) * ( zdepth(n) - zw(i) )
    END IF

  END DO

  qbase_max_l(i,nshyd+1) = ksfz(i,nshyd+1) - qbase_min(i)

  IF ( zw(i) <= zdepth(nshyd) ) THEN
    qbase_l(i,nshyd+1) = qbase_max_l(i,nshyd+1)
  ELSE
    qbase_l(i,nshyd+1) = ksfz(i,nshyd+1)                                       &
                         * EXP( -fexp(i) * (zw(i) - zdepth(nshyd)) )           &
                         - qbase_min(i)
  END IF

  !-----------------------------------------------------------------------
  ! Calculate total possible and actual base flow:
  !-----------------------------------------------------------------------
  DO n = 1,nshyd+1
    qbase_l(i,n) = MAX( 0.0, qbase_l(i,n) )
    qbase(i)     = qbase(i) + qbase_l(i,n)
    qbase_max(i) = qbase_max(i) + qbase_max_l(i,n)
  END DO

  !-----------------------------------------------------------------------
  ! Calculate critical topographic index.
  !-----------------------------------------------------------------------
  IF (qbase(i) >  qbase_max(i)) qbase(i) = qbase_max(i)

  ! Check that QBASE_MAX(I)/QBASE(I) will not underflow.
  IF ( qbase_max(i) >  EPSILON(qbase_max(i)) .AND.                             &
       qbase(i) >  qbase_max(i) * (EPSILON(qbase(i))) ) THEN
    top_crit(i) = LOG( qbase_max(i) / qbase(i) )
  END IF

END DO
!$OMP END DO
!$OMP END PARALLEL

IF (fail_count > max_print) THEN
  WRITE(jules_message,*)'CB_J: ZW_MAX point-by-point warnings terminated.'
  CALL jules_print('calc_baseflow_jules',jules_message)
  WRITE(jules_message,*)'CB_J: Total pts with ZW_MAX too small = ',fail_count
  CALL jules_print('calc_baseflow_jules',jules_message)
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE calc_baseflow_jules
END MODULE calc_baseflow_jules_mod
