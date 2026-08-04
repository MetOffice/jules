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

MODULE radf_ch4_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE radf_ch4(ch4_ppbv, q_ch4)

USE io_constants, ONLY: imogen_unit
USE imogen_constants, ONLY: nyr_max
USE imogen_run, ONLY: file_ch4_n2o, nyr_ch4_n2o
USE model_time_mod, ONLY: current_time
USE parallel_mod, ONLY: is_master_task
USE jules_print_mgr, ONLY: jules_message, jules_print
USE logging_mod, ONLY: log_fatal

IMPLICIT NONE
!-----------------------------------------------------------------------------
! Description:
!   Calculates the radiative forcing due to a given CH4 concentration
!
!    Written by Peter Cox (August 1998)
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

REAL, INTENT(IN) ::                                                            &
  ch4_ppbv              ! CH4 concetration (ppbv)

REAL, INTENT(OUT) ::                                                           &
  q_ch4                 ! Forcing due to ch4 emissions (W/m2)

! Local parameters
INTEGER ::                                                                     &
  yr_ref(nyr_max),                                                             &
                        ! years for which ch4 and N2O concentrations
                        !  of reference are prescribed.
  years_interp(nyr_max)    ! years for which ch4 and N2O
                        ! concs. are required by  imogen

REAL ::                                                                        &
  ch4_ref(nyr_max),                                                            &
                        ! Specified ch4 conc (ppbv).
  N2O_ref(nyr_max),                                                            &
                        ! Specified N2O conc (ppbv).
  ch4_interp_ppbv(nyr_max),                                                    &
                        ! Time interpolated ch4_ref.
  n2o_interp_ppbv(nyr_max) ! Time interpolated N2O_ref.

INTEGER :: i, iy
INTEGER :: io_error


!-----------------------------------------------------------------------------
! Read in file of ch4 and n2o atmospheric concentrations if required
!-----------------------------------------------------------------------------
OPEN(imogen_unit, FILE=file_ch4_n2o, STATUS = 'old',                           &
      POSITION = 'rewind', ACTION = 'read', IOSTAT=io_error)
IF (io_error /= 0) THEN
  CALL log_fatal("radf_ch4", ': file_ch4_n2o not found')
END IF

DO i = 1, nyr_ch4_n2o
  READ(imogen_unit, *, IOSTAT = io_error) yr_ref(i), ch4_ref(i), n2o_ref(i)
  IF (io_error /= 0) THEN
    CALL log_fatal("radf_ch4", ': error reading file_ch4_n2o')
  END IF
END DO
CLOSE(imogen_unit)

years_interp(:)      = yr_ref(:)
ch4_interp_ppbv(:)   = ch4_ref(:)
n2o_interp_ppbv(:)   = n2o_ref(:)

i = -999
DO iy = 1, nyr_max
  IF (years_interp(iy) == current_time%year) THEN
    i = iy
  END IF
END DO

CALL calc_q_ch4(ch4_ppbv, ch4_interp_ppbv(i), n2o_interp_ppbv(i), q_ch4)

IF ( is_master_task() ) THEN
  WRITE(jules_message,*) 'i, CH4, CH4_interp, N2O_interp',                     &
                          i, ch4_ppbv, ch4_interp_ppbv(i), n2o_interp_ppbv(i)
  CALL jules_print('radf_ch4', jules_message)
  WRITE(jules_message,*) 'CH4 feedback:', q_ch4
  CALL jules_print('radf_ch4', jules_message)
END IF
RETURN

END SUBROUTINE radf_ch4
!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
SUBROUTINE calc_q_ch4(ch4_ppbv, ch4_interp_ppbv, n2o_interp_ppbv, q_ch4)
!-----------------------------------------------------------------------------
! Subroutine to calculate the change in forcing due to the
! interactive methane flux from wetland/rice emissions.
!-----------------------------------------------------------------------------

IMPLICIT NONE

REAL, INTENT(IN) ::                                                            &
  ch4_ppbv,                                                                    &
                  ! imogen methane concentration.
  ch4_interp_ppbv,                                                             &
                  ! Time interpolated ch4 reference conc.
  n2o_interp_ppbv
                  ! Time interpolated N2O reference conc.
REAL, INTENT(OUT) ::                                                           &
  q_ch4           ! Forcing due to wetland/rice emissions.


! Local parameters
REAL ::                                                                        &
!   overlap,                                                                     &
!                   ! overlap function.
  olap,                                                                        &
                  ! overlap fn output with modelled ch4 conc
  olap_interp     ! overlap fn output with reference ch4 conc.


olap = overlap(ch4_ppbv, n2o_interp_ppbv)
olap_interp = overlap(ch4_interp_ppbv, n2o_interp_ppbv)

! https://www.ipcc.ch/site/assets/uploads/2018/03/TAR-06.pdf
! Table 6.2 page 358
q_ch4 = 0.036 * (SQRT(ch4_ppbv) - SQRT(ch4_interp_ppbv))                       &
            - (olap - olap_interp)

RETURN

END SUBROUTINE calc_q_ch4
!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
FUNCTION overlap(ch4_ppbv, n2o_ppbv) RESULT(overlap_val)
!-----------------------------------------------------------------------------
! Function to return the IPCC FAR WG1 1990 report (equation Table 2.2
! subscript) / Hansen et al 1988 term for radiative overlap of ch4 & N2O
!-----------------------------------------------------------------------------

IMPLICIT NONE

REAL, INTENT(IN) ::                                                            &
  ch4_ppbv,                                                                    &
                   ! Methane conc. (ppbv)
  n2o_ppbv         ! N2O conc. (ppbv)

REAL ::                                                                        &
  overlap_val      ! scalar function result

! https://www.ipcc.ch/site/assets/uploads/2018/03/TAR-06.pdf
! Table 6.2 page 358
overlap_val = 0.47 *                                                           &
              LOG(1.0 + (2.01e-5 * (ch4_ppbv * n2o_ppbv)** 0.75)               &
                + (5.31e-15 * ch4_ppbv * (ch4_ppbv * n2o_ppbv)** 1.52))

END FUNCTION overlap
END MODULE radf_ch4_mod
#endif
