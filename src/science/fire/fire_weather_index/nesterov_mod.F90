! *****************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use
! and distribution under the JULES collaboration agreement, subject
! to the terms and conditions set out therein.
!
! [Met Office Ref SC0237]
! *****************************COPYRIGHT****************************************

! Calculates the Nesterov fire risk index

MODULE nesterov

! No module imports

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!
! Description:
!   Contains code to enable calculation of the Nesterov index.
!
!Equation numbers refer to the documentation report
!"Met Office JULES fire module Version 1.0 (JULES V4.1)
! December 2014"
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!

PRIVATE !Default everything to being private. Unlock as needed

! Module constants
REAL(KIND=real_jlslsm), PARAMETER   :: precip_thresh = 3.0
                                         !Precip (mm) to reset the index
! Set in line with common practice rather than a well-defined reference.

! Public module variables
PUBLIC :: nesterov_calc

CONTAINS

!#include "nesterov_calc.inc"
! Part of the nesterov module, performs the actual index calculation

SUBROUTINE nesterov_calc(                                                      &
!Array INTENT(IN)                                                           &
temp,dewpoint,precip,                                                          &
!Array INTENT(INOUT)                                                        &
nest_index)

! No module imports but has access to module variables

IMPLICIT NONE

!
! Description:
!   Performs the calculation of the Nesterov index for the current day.
!
! Method:
!   The index is very simple. If there has been > 3mm precip today, the index
!   resets to zero. Otherwise N = N_yesterday * (temp(temp-dewpoint)) where
!   all amounts are daily means/totals.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!

! Subroutine arguments
REAL(KIND=real_jlslsm),   INTENT(IN)    :: temp(:),     & !Mean daily temp (C)
                         dewpoint(:), & !Mean daily dewpoint (C)
                         precip(:)      !Daily total precip (mm)

!Note this is an INOUT variable and stored in the parent unit
REAL(KIND=real_jlslsm),   INTENT(IN OUT) :: nest_index(:)  !Nesterov index (C**2)

! Local constants- None

! Local variables- None

! End of header

! All operations are array operations. (:) notation ommitted

  !Documentation equation 30
WHERE ( precip >= precip_thresh )
  nest_index = 0.0
ELSE WHERE
  !Protect against -ve index values using MAX
  nest_index = nest_index + MAX((temp * (temp - dewpoint)),0.0)
END WHERE

RETURN
END SUBROUTINE nesterov_calc

END MODULE nesterov
