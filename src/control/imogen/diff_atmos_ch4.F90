#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************
SUBROUTINE diff_atmos_ch4(d_land_atmos_ch4, ch4_ppbv)

USE io_constants, ONLY: imogen_unit
USE conversions_mod, ONLY: conv_gtc_to_ppm, ppm_to_ppb, pg_to_kg
USE imogen_constants, ONLY: nyr_max
USE imogen_run, ONLY: file_ch4_n2o, nyr_ch4_n2o, ch4_ppbv_ref, tau_ch4_ref
USE model_time_mod, ONLY: main_run_start, current_time
USE jules_print_mgr, ONLY: jules_message, jules_print
USE parallel_mod, ONLY: is_master_task
USE logging_mod, ONLY: log_fatal

IMPLICIT NONE
!-----------------------------------------------------------------------------
! Adjusts the atmospheric CH4 concentration based on the increase in
! global CH4 emissions from natural sources
!-----------------------------------------------------------------------------

REAL, INTENT(IN) ::                                                            &
  d_land_atmos_ch4
        ! Additional methane accumulated flux (kgC/year)

REAL, INTENT(IN OUT) :: ch4_ppbv
        ! ch4 concentration in ppbv

! Local parameters
INTEGER ::                                                                     &
  yr_ref(nyr_max),                                                             &
        ! years for which ch4 and n2o conc of reference are prescribed
  years_int(nyr_max)
        ! years for which ch4 and n2o conc. required by imogen

REAL ::                                                                        &
  d_land_atmos_ch4_ppbv,                                                       &
        ! Additional methane accumulated flux (ppbv)
  ch4_ref(nyr_max),                                                            &
        ! Specified ch4 conc (ppbv).
  ch4_interp_ppbv(nyr_max)
        ! Time interpolated ch4_ref. (ppbv)

REAL ::                                                                        &
  ch4_itm,                                                                     &
        ! Model atmospheric methane concentration at previous year.
  n2o_ref,                                                                     &
        ! N2O conc (ppbv) - dummy for reading
  tau_ch4
        ! lifetime of methane for current year


INTEGER ::                                                                     &
  i, iy, itm, iinit

INTEGER :: io_error

!-----------------------------------------------------------------------------
! Read in file of ch4 and n2o atmospheric concentrations if required
!-----------------------------------------------------------------------------
OPEN(imogen_unit, FILE=file_ch4_n2o, STATUS = 'old',                           &
      POSITION = 'rewind', ACTION = 'read', IOSTAT=io_error)
IF (io_error /= 0) THEN
  CALL log_fatal("diff_atmos_ch4", ': file_ch4_n2o not found')
END IF

DO i = 1, nyr_ch4_n2o
  READ(imogen_unit, *, IOSTAT = io_error) yr_ref(i), ch4_ref(i), n2o_ref
  IF (io_error /= 0) THEN
    CALL log_fatal("diff_atmos_ch4", ': error reading file_ch4_n2o')
  END IF
END DO
CLOSE(imogen_unit)

years_int(:)       = yr_ref(:)
ch4_interp_ppbv(:) = ch4_ref(:)

i = -999
iinit = -999
DO iy = 1, nyr_max
  IF (years_int(iy) == current_time%year) THEN
    i = iy
  END IF
  IF (years_int(iy) == main_run_start%year) THEN
    iinit = iy
  END IF
END DO

itm = i - 1
IF (itm <= 0) THEN
  itm = i
END IF

d_land_atmos_ch4_ppbv = d_land_atmos_ch4 *                                     &
                        conv_gtc_to_ppm * ppm_to_ppb / pg_to_kg

! CH4 atmospheric decay factor based on decay factor at ref year, 2000.
! https://www.ipcc.ch/site/assets/uploads/2018/03/WGI_TAR_full_report.pdf p251
! Table 4.3 - TAR value: PT/LT=1.4, 0.28 = 1- LT/PT
tau_ch4 = tau_ch4_ref * EXP(0.28 * LOG(ch4_ppbv / ch4_ppbv_ref) )
IF ( is_master_task() ) THEN
  WRITE(jules_message, *) 'ch4_ppbv,ch4_ppbv_ref,LOG(ch4_ppbv/ch4_ppbv_ref)=', &
                ch4_ppbv, ch4_ppbv_ref, LOG(ch4_ppbv / ch4_ppbv_ref)
  CALL jules_print('diff_atmos_ch4', jules_message)
  WRITE(jules_message, *) 'TAU_CH4,TAU_CH4_REF', tau_ch4, tau_ch4_ref
  CALL jules_print('diff_atmos_ch4', jules_message)
END IF

! Calculate the new ch4 value in ppbv, which is equal to:
!   the previous year ch4 PLUS the wetland feedback component MINUS
!   the decayed component of the addtional ch4 from the previous year
!   PLUS the difference between the current year value and previous year
!   value from the reference file
ch4_itm = ch4_ppbv  !previous year
ch4_ppbv = ch4_itm + d_land_atmos_ch4_ppbv                                     &
          - ((ch4_itm - ch4_interp_ppbv(itm)) / tau_ch4)                       &
          + (ch4_interp_ppbv(i) - ch4_interp_ppbv(itm))

IF ( is_master_task() ) THEN
  WRITE(jules_message, *) 'CH4,CH4_INTERP(I),CH4_ITM,CH4_INTERP(I-1)',         &
           ch4_ppbv, ch4_interp_ppbv(i), ch4_itm, ch4_interp_ppbv(itm)
  CALL jules_print('diff_atmos_ch4', jules_message)
END IF

RETURN

END

#endif
