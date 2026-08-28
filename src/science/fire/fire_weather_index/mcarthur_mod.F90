! *****************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use
! and distribution under the JULES collaboration agreement, subject
! to the terms and conditions set out therein.
!
! [Met Office Ref SC0237]
! *****************************COPYRIGHT****************************************

!Controls the calculation of the McArthur Forest Fire Index (FFDI)

MODULE mcarthur

!No module imports

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!
! Description:
!   Encapsulated code to calculate the Mcarthur FFDI.

    !A good reference to start from is:
    !Global Biogeochemical Cycles, Vol. 22, No. 4. (11 November 2008), GB4007
    !doi:10.1029/2007GB003166

    !Note that this implementation uses a fixed value of i in the drought
    !calculation. Code has been commented out and alternative lines written
    !to take advantage of the simplification afforded.

    !This module contains 1 subroutine:
    !-mcarthur_calc(temp,relhum,wind,precip,ffdi_index) Calculates the FFDI
    ! and should be called on a daily basis

    !Equation numbers refer to the documentation report
    !"Met Office JULES fire module Version 1.0 (JULES V4.1)
    ! December 2014"

    !See comments in mcarthur_calc for expected units

    !Most parameters are hard coded directly instead of declaring parameters

! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!

PRIVATE !Default everything to being private. Unlock as needed

! Module constants
  !Parameters from McArthur that need inverting to remove some division
REAL(KIND=real_jlslsm), PARAMETER   :: inv_t_ref      = 1.0 / 29.5858
REAL(KIND=real_jlslsm), PARAMETER   :: inv_h_ref      = 1.0 / 28.9855
REAL(KIND=real_jlslsm), PARAMETER   :: inv_w_ref      = 1.0 / 42.735

!Threshold for a 'wet' day. Set to a nominal low value.
REAL(KIND=real_jlslsm), PARAMETER   :: precip_thresh  = 0.01

! Public module variables- NONE

! Private module variables- NONE

!Set the subroutines available outside the module
PUBLIC  :: mcarthur_calc

CONTAINS

!#include "mcarthur_calc.inc"
!Part of the mcarthur module. Calculates the FFDI.

SUBROUTINE mcarthur_calc(                                                      &
!Array INTENT(IN)                                                            &
temp, relhum, wind, precip, i_drought,                                         &
!Array INTENT(INOUT)                                                         &
r_drought, n_drought_p1,                                                       &
!Array INTENT(OUT)                                                           &
ffdi)

IMPLICIT NONE
!
! Description:
!   Performs the calculation of the McArthur FFDI.
!
! Method:
!   Note the units of the met variables it expects.
!
!   All arrays are the same size.
!   Local arrays are sized using SIZE of the 1st argument
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!

! Subroutine arguments

! < Array arguments with INTENT(IN) >
REAL(KIND=real_jlslsm) ,   INTENT(IN)     ::                                   &
                            temp(:),        & !Daily max Temperature (C)
                            relhum(:),      & !Daily min Rel Hum (%)
                            wind(:),        & !Daily mean wind speed (km/h)
                            precip(:),      & !24h total precip (mm)
                            i_drought(:)      !Soil moisture deficit (mm)

! < Array arguments with INTENT(INOUT)>
REAL(KIND=real_jlslsm),    INTENT(IN OUT)  ::                                  &
                            r_drought(:),   & !24h accum. on last rain day
                            n_drought_p1(:)   !N days since last rain+1

! < Array arguments with INTENT(OUT)>
REAL(KIND=real_jlslsm),    INTENT(OUT)    ::  ffdi(:)

! < Array local variables >
REAL(KIND=real_jlslsm)                    ::  drought_term(SIZE(temp)),        &
                                              drought_denom(SIZE(temp)),       &
                                              drought(SIZE(temp)),             &
                                              ffdi_term(SIZE(temp))

! End of header

  !Everything is done using array operations but some scalar variables used.

  !Calculate the drought term, equation 29

  !Update drought parameters for the current day
  !In theory there should be an i_drought update too
WHERE (precip > precip_thresh) !we have a wet day
  n_drought_p1 = 1.0
  r_drought    = precip
ELSE WHERE !we have a dry day
  n_drought_p1 = n_drought_p1 + 1.0
END WHERE

!Precalculate some things
drought_term(:)  = n_drought_p1(:)**1.5

!1/x to remove a division later
drought_denom(:) = 1.0 / (3.52 * drought_term(:) + r_drought(:) - 1.0)

drought(:)       = (0.191 * (i_drought(:) + 104.0) * drought_term(:)) *        &
                    drought_denom(:)

!Calculate the FFDI, equation 28

ffdi_term(:)     = 1.275 * EXP((temp(:) * inv_t_ref) -                         &
                   (relhum(:) * inv_h_ref) + (wind(:) * inv_w_ref))

ffdi(:)          = drought(:)**0.987 * ffdi_term(:)

RETURN
END SUBROUTINE mcarthur_calc

END MODULE mcarthur
