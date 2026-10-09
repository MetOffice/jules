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

MODULE drdat_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE drdat(iyear)

USE fill_variables_from_file_mod, ONLY: fill_variables_from_file
USE imogen_anlg_vals, ONLY: file_base_anom
USE io_constants, ONLY: max_file_name_len

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads anomalies of driving data for the IMOGEN simulations
!   For l_change_metdata = .true. and change_metdata_method = 2
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
! Written by: P. Cox (1997) and C. Huntingford (2004)
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) ::                                                         &
  iyear
              ! Year of interest.

INTEGER, PARAMETER :: nvars = 8
INTEGER :: i ! Loop counter

CHARACTER(LEN=4) ::                                                            &
  drive_year
              ! Label for year of driving data.
CHARACTER(LEN=max_file_name_len) ::                                            &
  file_year
              ! File containing anomalies for the year of interest.

CHARACTER(LEN=24), DIMENSION(nvars) ::                                         &
  imgn_name = ['tl1_ij_anom_imgn        ',                                     &
               'ql1_ij_anom_imgn        ',                                     &
               'wind_ij_anom_imgn       ',                                     &
               'lwdown_ij_anom_imgn     ',                                     &
               'swdown_ij_anom_imgn     ',                                     &
               'precip_ij_anom_imgn     ',                                     &
               'pstar_ij_anom_imgn      ',                                     &
               'diurnal_tl1_ij_anom_imgn']

CHARACTER(LEN=14), DIMENSION(nvars) ::                                         &
  nc_name = ['tl1_anom      ',                                                 &
             'ql1_anom      ',                                                 &
             'wind_anom     ',                                                 &
             'lwdown_anom   ',                                                 &
             'swdown_anom   ',                                                 &
             'precip_anom   ',                                                 &
             'pstar_anom    ',                                                 &
             'range_tl1_anom' ]

!-----------------------------------------------------------------------
! Convert year to string.
!-----------------------------------------------------------------------
WRITE(drive_year,'(I4)') iyear

file_year = TRIM(file_base_anom) // "_" // drive_year //".nc"
! curently need to be as a separate netcdf file for each year

! In the following call, is_climatology adds a 12-month time dimension to the
! size of the input variable in order to interpolate to the appropriate start
! time for initial conditions. It is therefore .FALSE. in this call.
CALL fill_variables_from_file(                                                 &
   file_year, imgn_name, nc_name, is_climatology = [ (.FALSE., i=1, nvars) ]   &
   )

RETURN

END SUBROUTINE drdat
END MODULE drdat_mod
#endif
