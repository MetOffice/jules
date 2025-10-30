! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

SUBROUTINE sow(n, sm_levels, t_soil, sthu, smvcst, smvccl, dphotdt,            &
               sowdate, dvi)

USE cropparm, ONLY: t_bse

USE datetime_utils_mod, ONLY: day_of_year, days_in_year

USE time_info_mod, ONLY: l_360, l_leap, current_model_time

USE jules_vegetation_mod, ONLY: l_prescsow

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Sows crop if sowing conditions are fulfilled.
!
! Method:
!   Crop should already have been sown when this subroutine is called.
!   Requires at least 3 soil levels.
!
!   See JULES-crop technical documentation (Tom Osborne, Josh Hooker).
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) :: n               ! crop tile number
INTEGER, INTENT(IN) :: sm_levels       ! number of soil levels

REAL(KIND=real_jlslsm), INTENT(IN) :: t_soil(sm_levels)  ! soil temperatures (K)
REAL(KIND=real_jlslsm), INTENT(IN) :: sthu(sm_levels)
                                       ! soil moisture contents as a fraction of saturation
REAL(KIND=real_jlslsm), INTENT(IN) :: smvcst(sm_levels)
                                       ! Volumetric moisture content at
                                       ! saturation (m3/m3 of soil).
REAL(KIND=real_jlslsm), INTENT(IN) :: smvccl(sm_levels)
                                       ! Volumetric moisture content at
                                       ! critical point (m3/m3 of soil).
REAL(KIND=real_jlslsm), INTENT(IN) :: dphotdt
                                       ! change in photoperiod between this
                                       ! day and the one before, in hours
REAL(KIND=real_jlslsm), INTENT(IN) :: sowdate
                                       ! prescribed sowing date

REAL(KIND=real_jlslsm), INTENT(IN OUT) :: dvi
                                       ! crop development index

! Local variables
INTEGER :: k            ! soil level counter
INTEGER ::  day         ! Current day
INTEGER ::  month       ! Current month
INTEGER ::  year        ! Current year
INTEGER ::  day_of_yr   ! Current day of year
INTEGER ::  days_in_yr  ! Total number of days in current year
REAL(KIND=real_jlslsm) :: smvc(sm_levels)
                        ! Volumetric soil moisture content (m3/m3 of soil).
LOGICAL :: sow_today    ! whether crop should be sown on this day

!-----------------------------------------------------------------------------

! Get the current model year, month and day
CALL current_model_time(year, month, day)

day_of_yr  = day_of_year(year, month, day, l_360, l_leap)
days_in_yr = days_in_year(year, l_360, l_leap)

sow_today = .FALSE.

DO k = 1, sm_levels
  smvc(k) = sthu(k) * smvcst(k)
END DO

IF ( l_prescsow ) THEN
  IF ( day_of_yr == NINT(sowdate) ) THEN
    sow_today = .TRUE.
  END IF
ELSE
  IF ( smvc(2) > smvccl(2) .AND. dphotdt > -0.02 .AND.                         &
       t_soil(3) > ( t_bse(n) + 2.0) ) THEN
    sow_today = .TRUE.
  END IF
END IF

IF ( sow_today ) THEN
  dvi = -1.0
END IF

END SUBROUTINE sow
