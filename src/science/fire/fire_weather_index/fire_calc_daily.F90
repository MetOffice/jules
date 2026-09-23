MODULE fire_calc_daily_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='FIRE_CALC_DAILY_MOD'

CONTAINS

SUBROUTINE fire_calc_daily(msd, smc, fire_prog, fire_diag,                     &
!Things that really ought to come in via USE but won't work with the UM
current_month, land_pts)

  ! < Module imports >
USE conversions_mod,  ONLY: zerodegc, mps_to_kph

USE mcarthur,         ONLY: mcarthur_calc
USE canadian,         ONLY: canadian_calc
USE nesterov,         ONLY: nesterov_calc

USE metstats_mod,     ONLY: metstats_prog_struct
USE fire_mod,         ONLY: fire_prog_struct, fire_diag_struct, fire_cntl

USE parkind1,         ONLY: jprb, jpim
USE yomhook,          ONLY: lhook, dr_hook

!  USE model_time_mod,   ONLY : current_time

IMPLICIT NONE
!
! Description:
!   Finalises met variable conversions and calls daily fire models
!
! Method:
! Conditionally calls the individual fire models, giving them input variables
! with the units they require.
!
! NOTE regarding logging via dr_hook
! Logging for the individual fire models has been set up with the calls
! to dr_hook straddling the CALLs in this subroutine. This helps preserve
! the portability of these codes as they'll get used outside JULES/UM
! The fire code as a whole is logged in fire_timestep (and metstats_timestep)
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!

! Subroutine arguments
INTEGER,                     INTENT(IN)    :: current_month, land_pts

TYPE (metstats_prog_struct), INTENT(IN)    :: msd(land_pts)
REAL(KIND=real_jlslsm),      INTENT(IN)    :: smc(land_pts)
TYPE (fire_prog_struct),     INTENT(IN OUT) :: fire_prog(land_pts)
TYPE (fire_diag_struct),     INTENT(OUT)   :: fire_diag(land_pts)

! < Array local variables >
!Temp variables to help get data in and out of models in a readable way
REAL(KIND=real_jlslsm)                     :: mcarthur_temp(land_pts),         &
                                              mcarthur_rhum(land_pts),         &
                                              mcarthur_wind(land_pts),         &
                                              mcarthur_prec(land_pts),         &
                                              mcarthur_i_dr(land_pts),         &
                                              canadian_temp(land_pts),         &
                                              canadian_rhum(land_pts),         &
                                              canadian_wind(land_pts),         &
                                              canadian_prec(land_pts),         &
                                              nesterov_temp(land_pts),         &
                                              nesterov_dewp(land_pts),         &
                                              nesterov_prec(land_pts)

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='FIRE_CALC_DAILY'

! End of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
IF ( fire_cntl%mcarthur%flag ) THEN

  mcarthur_temp(:) = msd(:)%temp_max_00h%fin - zerodegc
  mcarthur_rhum(:) = msd(:)%rhum_min_00h%fin
  mcarthur_wind(:) = msd(:)%wind_ave_00h%fin * mps_to_kph
  mcarthur_prec(:) = msd(:)%prec_tot_00h%fin

  !Use the appropriate value for the soil moisture deficit
  IF (fire_cntl%mcarthur%option == 1) THEN
    !Explicitly uses the model value for rain needed to top up to 200 mm
    mcarthur_i_dr(:) = MAX(200.0 - (smc(:) * fire_cntl%mcarthur%smc_coeff),    &
                             0.0)
  ELSE IF (fire_cntl%mcarthur%option == 2) THEN
    !Assumes 120mm is needed to top up to 200 mm
    mcarthur_i_dr(:) = 120.0
  END IF

  CALL mcarthur_calc(                                                          &
    !Array INTENT(IN)                                                        &
    mcarthur_temp(:), mcarthur_rhum(:), mcarthur_wind(:), mcarthur_prec(:),    &
    mcarthur_i_dr(:),                                                          &
    !Array INTENT(INOUT)                                                     &
    fire_prog(:)%mcarthur%r_dr, fire_prog(:)%mcarthur%n_dr,                    &
    !Array INTENT(OUT)                                                       &
    fire_diag(:)%mcarthur%ffdi)

END IF !mcarthur

!-----------------------------------------------------------------------------
IF ( fire_cntl%canadian%flag ) THEN

  canadian_temp(:) = msd(:)%temp_pnt_12h%fin - zerodegc
  canadian_rhum(:) = msd(:)%rhum_pnt_12h%fin
  ! Restrict relative humidity to 100.0 since canadian calculations can't deal
  ! with super-saturation
  WHERE ( canadian_rhum > 100.0 ) canadian_rhum = 100.0
  canadian_wind(:) = msd(:)%wind_pnt_12h%fin * mps_to_kph
  canadian_prec(:) = msd(:)%prec_tot_12h%fin

  CALL canadian_calc(                                                          &
    !Scalar INTENT(IN)                                                       &
    current_month, fire_cntl%canadian%hemi_opt,                                &
    !Array INTENT(IN)                                                        &
    fire_prog(:)%canadian%hemi_NtSf,                                           &
    canadian_temp(:), canadian_rhum(:), canadian_wind(:), canadian_prec(:),    &
    !Array INTENT(INOUT)                                                     &
    fire_prog(:)%canadian%ffmc, fire_prog(:)%canadian%ffmc_mois,               &
    fire_prog(:)%canadian%dmc,  fire_prog(:)%canadian%dc,                      &
    !Array INTENT(OUT)                                                       &
    fire_diag(:)%canadian%isi,  fire_diag(:)%canadian%bui,                     &
    fire_diag(:)%canadian%fwi)

END IF

!-----------------------------------------------------------------------------
IF ( fire_cntl%nesterov%flag ) THEN

  nesterov_temp(:)  = msd(:)%temp_ave_00h%fin - zerodegc
  nesterov_dewp(:)  = msd(:)%dewp_ave_00h%fin - zerodegc
  nesterov_prec(:)  = msd(:)%prec_tot_00h%fin

  CALL nesterov_calc(                                                          &
    !Array INTENT(IN)                                                        &
    nesterov_temp(:), nesterov_dewp(:), nesterov_prec(:),                      &
    !Array INTENT(INOUT)                                                     &
    fire_prog(:)%nesterov%findex)

END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE fire_calc_daily
END MODULE fire_calc_daily_mod
