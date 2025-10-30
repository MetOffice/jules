#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE init_irrigation(nml_dir)

!Use in variables
USE io_constants, ONLY: max_file_name_len, namelist_unit

USE string_utils_mod, ONLY: to_string

USE jules_irrig_mod

USE jules_hydrology_mod, ONLY: l_top

USE jules_vegetation_mod, ONLY: l_crop

USE logging_mod, ONLY: log_info, log_fatal, log_error

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Sets up and reads irrigation namelists
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

INTEGER :: i,l  ! Index variables

INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

CALL read_nml_jules_irrig(nml_dir)
CALL print_nlist_jules_irrig()
CALL check_jules_irrig()
!-----------------------------------------------------------------------------
! Print some human friendly summary information about the selected options
!-----------------------------------------------------------------------------
IF ( l_irrig_dmd ) THEN
  CALL log_info("init_irrigation", "Irrigation demand model is on")

  SELECT CASE ( irr_crop )
  CASE ( irr_crop_all_year )
    CALL log_info("init_irrigation",                                           &
                  "irr_crop = 0: continuous irrigation")
  CASE ( irr_crop_doell )
    CALL log_info("init_irrigation",                                           &
                  "irr_crop = 1: following Doell & Siebert (2002)")
  CASE ( irr_crop_dvimax )
    CALL log_info("init_irrigation",                                           &
                  "irr_crop = 2: irrigation triggered by DVI from crop model")
  CASE DEFAULT
    CALL log_error("init_irrigation",                                          &
                   "irr_crop value not recognised")
  END SELECT

  IF ( l_irrig_limit ) THEN
    CALL log_info("init_irrigation",                                           &
                  "Irrigation is limited by water availability")
  END IF

END IF  !  l_irrig_dmd

RETURN

END SUBROUTINE init_irrigation
#endif
