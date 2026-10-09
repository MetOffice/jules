! Part of the fire module. Called every timestep and manages the models.
MODULE fire_timestep_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='FIRE_TIMESTEP_MOD'

CONTAINS
SUBROUTINE fire_timestep(metstats_prog, smc, fire_prog, fire_diag,             &
!Things that really ought to come in via USE but won't work with the UM
                         current_time, current_month, timestep, land_pts)

! < Module imports >
USE metstats_mod,   ONLY: metstats_prog_struct
USE fire_mod,       ONLY: fire_prog_struct, fire_diag_struct
USE fire_calc_daily_mod, ONLY: fire_calc_daily

USE parkind1,       ONLY: jprb, jpim
USE yomhook,        ONLY: lhook, dr_hook

!  Statements we'd like to use but can't because of the UM
!  USE datetime_mod,   ONLY : datetime
!  USE model_time_mod, ONLY : current_time, timestep_len

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE
!
! Description:
!   Called every model timestep, this subroutine does the overal management of
!   updating driving variables for the models and when to call them.
!
! Method:
!   Important point- all indices are updated on the first MODEL timestep
!   >= midnight to give the value for the day just completed.
!   Midnight call to calc_daily and reset_daily checked using simple print
!   statement on Loobos data.

! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!
REAL(KIND=real_jlslsm), INTENT(IN)    :: timestep
INTEGER, INTENT(IN) :: current_time, current_month, land_pts

TYPE (metstats_prog_struct), INTENT(IN)    :: metstats_prog(land_pts)
REAL(KIND=real_jlslsm),      INTENT(IN)    :: smc(land_pts)
TYPE (fire_prog_struct),     INTENT(IN OUT) :: fire_prog(land_pts)
TYPE (fire_diag_struct),     INTENT(OUT)   :: fire_diag(land_pts)

!Local variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='FIRE_TIMESTEP'

! End of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!---------------------------------------------------
!Perform tasks that happen every timestep
!None

!---------------------------------------------------
!Perform tasks that happen daily at model midnight
IF ( current_time < NINT(timestep) ) THEN

  CALL fire_calc_daily(metstats_prog,smc,fire_prog,fire_diag,                  &
      !Things that really ought to come in via USE but won't work with the UM
      current_month, land_pts)
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE fire_timestep
END MODULE fire_timestep_mod
