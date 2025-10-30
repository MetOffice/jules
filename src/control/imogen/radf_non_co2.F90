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

SUBROUTINE radf_non_co2(                                                       &
  year,q,nyr_non_co2,file_non_co2_radf)

USE imogen_constants, ONLY: nyr_max
USE io_constants, ONLY: imogen_unit, max_file_name_len

USE logging_mod, ONLY: log_fatal

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Calculates the radiative forcing due to non-CO2 GHGs. This version
!   is consistent with the HadCM3 GHG run (AAXZE).
!
!   Written by Peter Cox (Sept 1998)
!   Adjusted by Chris Huntingford (Dec 1999) to include
!   other scenarios copied from file.
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

INTEGER ::                                                                     &
  year,                                                                        &
           ! IN Julian year.
  i,                                                                           &
           ! WORK Loop counter.
  nyr_non_co2
           ! IN Number of years for which non_co2
           !    forcing is prescribed.

CHARACTER(LEN=max_file_name_len) ::                                            &
  file_non_co2_radf   ! IN File of non-co2 radiative forcings

REAL :: q             ! OUT non-CO2 radiative forcing (W/m2).

INTEGER :: io_error

!-----------------------------------------------------------------------
! Local parameters
!-----------------------------------------------------------------------

INTEGER ::                                                                     &
  years(nyr_max)      ! Years for which radiative is
                      ! prescribed.

REAL ::                                                                        &
  q_non_co2(nyr_max),                                                          &
                      ! Specified radiative forcings (W/m2).
  growth_rate         ! Growth rate in the forcing after the
                      ! last prescribed value (%/pa).
PARAMETER (growth_rate = 0.0)


IF (nyr_non_co2 > nyr_max) THEN
  CALL log_fatal("RADF_NON_CO2", 'nyr_non_co2 too large')
END IF


!-----------------------------------------------------------------------
! File of non-co2 forcings read in if required
!-----------------------------------------------------------------------
OPEN(imogen_unit, FILE=file_non_co2_radf,                                      &
      POSITION = 'rewind', ACTION = 'read', IOSTAT=io_error)
IF (io_error /= 0) THEN
  CALL log_fatal("radf_non_co2", ':file_non_co2_radf not found')
END IF

DO i = 1,nyr_non_co2
  READ(imogen_unit,*, IOSTAT=io_error) years(i), q_non_co2(i)
  IF (io_error /= 0) THEN
    CALL log_fatal("radf_non_co2", ': error reading file_non_co2_radf')
  END IF
END DO
CLOSE(imogen_unit)

!-----------------------------------------------------------------------
! Now calculate the non_co2 forcing
!-----------------------------------------------------------------------
IF (year < years(1)) THEN
  q = 0.0
ELSE IF (year > years(nyr_non_co2)) THEN
  q = q_non_co2(nyr_non_co2)                                                   &
    * ((1.0+0.01 * growth_rate)**(year - years(nyr_non_co2)))
ELSE
  DO i = 1,nyr_non_co2-1
    IF ((year >= years(i)) .AND. (year <= years(i+1))) THEN
      q = q_non_co2(i) + (year - years(i))                                     &
        * (q_non_co2(i+1) - q_non_co2(i)) / (years(i+1) - years(i))
    END IF
  END DO
END IF

RETURN

END SUBROUTINE radf_non_co2
#endif
