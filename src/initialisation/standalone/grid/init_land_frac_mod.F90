#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************
MODULE init_land_frac_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_land_frac()

USE io_constants, ONLY: max_sdf_name_len, max_file_name_len, namelist_unit

USE string_utils_mod, ONLY: to_string

USE coastal, ONLY: flandg, l_use_land_fraction

USE input_mod, ONLY: input_grid => grid

USE fill_variables_from_file_mod, ONLY: fill_variables_from_file

USE errormessagelength_mod, ONLY: errormessagelength

USE logging_mod, ONLY: log_info, log_warn, log_fatal

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads in the land fraction on the full grid and checks for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Work variables
INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

!-----------------------------------------------------------------------------
! Definition of the jules_land namelist - this combines variables
! from input_mod with some local variables
!-----------------------------------------------------------------------------
CHARACTER(LEN=max_file_name_len) :: FILE
CHARACTER(LEN=max_sdf_name_len) :: land_frac_name

NAMELIST /jules_land_frac/ FILE, land_frac_name, l_use_land_fraction

!-----------------------------------------------------------------------------
! Initialise
!-----------------------------------------------------------------------------
FILE=''  !  Empty file name.
land_frac_name = ''  !  Empty variable name.

CALL log_info("init_land_frac", "Reading JULES_LAND_FRAC namelist...")

! First, we read the namelist
READ(namelist_unit, NML = jules_land_frac, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_land_frac",                                             &
                 "Error reading namelist JULES_LAND_FRAC " //                  &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

!-----------------------------------------------------------------------------
! Verify for consistency
!-----------------------------------------------------------------------------
CALL log_info("init_land_frac",                                                &
              "Getting land fraction for the full input grid...")

! For now, we allocate flandg on the full input grid
ALLOCATE(flandg(input_grid%nx,input_grid%ny), STAT = ERROR)
IF ( ERROR > 0 )                                                               &
  CALL log_fatal("init_land_frac", "Error allocating flandg")

IF ( input_grid%nx * input_grid%ny <= 1 ) THEN
  ! For single point data, just assume the single point is 100% land
  CALL log_info("init_land_frac",                                              &
                "Data is at a single point - setting land fraction to 1.0")
  flandg(1,1) = 1.0
ELSE
  ! For any other grid, read land fraction from file
  CALL log_info("init_land_frac",                                              &
                "Data is on a grid - reading land fraction from file " //      &
                TRIM(FILE))
  !   Check that a file name was provided.
  IF ( LEN_TRIM(FILE) == 0 )                                                   &
    CALL log_fatal("init_land_frac", "No file name provided")

  CALL fill_variables_from_file(FILE, [ 'land_fraction' ],                     &
                                      [  land_frac_name ],                     &
                                      is_climatology = [ .FALSE. ])
END IF

IF ( .NOT. l_use_land_fraction ) THEN
  ! For now, make sure that land fraction is either 1.0 or 0.0 - until JULES
  ! can deal with coastal tiling
  WHERE ( flandg > EPSILON(1.0) )
    flandg = 1.0
  ELSE WHERE
    flandg = 0.0
  END WHERE
END IF

RETURN

END SUBROUTINE init_land_frac

END MODULE init_land_frac_mod
#endif
