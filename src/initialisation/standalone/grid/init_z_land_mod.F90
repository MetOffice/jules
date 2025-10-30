#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE init_z_land_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_z_land(ainfo_data,jules_vars_data)

USE io_constants, ONLY: max_sdf_name_len, max_file_name_len, namelist_unit

USE string_utils_mod, ONLY: to_string

USE fill_variables_from_file_mod, ONLY: fill_variables_from_file

USE missing_data_mod, ONLY: rmdi

USE c_elevate, ONLY: l_elev_absolute_height, z_land_io, surf_hgt_band

USE ancil_info, ONLY: nsurft, land_pts

USE theta_field_sizes, ONLY: t_i_length

USE errormessagelength_mod, ONLY: errormessagelength

USE logging_mod, ONLY: log_info, log_warn, log_fatal

!TYPE definitions
USE ancil_info,    ONLY: ainfo_data_type
USE jules_vars_mod, ONLY: jules_vars_data_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises the elevation of the forcing data elevation if any tile has
!   an absolute height above sea-level.
!   Tile heights are set to be spatially invarient.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
TYPE(ainfo_data_type), INTENT(IN OUT) :: ainfo_data
TYPE(jules_vars_data_type), INTENT(IN OUT) :: jules_vars_data

! Work variables
INTEGER :: ERROR  ! Error indicator

!-----------------------------------------------------------------------------
! Definition of the jules_z_land namelist
!-----------------------------------------------------------------------------
LOGICAL :: use_file
                      !   T - the variable uses the file
                      !   F - the variable is set using a constant value

CHARACTER(LEN=max_file_name_len) :: FILE  ! If input grid has more than one
                                          ! point, read heights from this
                                          ! file
CHARACTER(LEN=max_sdf_name_len) :: z_land_name    ! Use the variable with
                                                  ! this name for forcing
                                                  ! altitude
CHARACTER(LEN=errormessagelength) :: iomessage

NAMELIST  / jules_z_land/ use_file, FILE, z_land_io, z_land_name,              &
                        surf_hgt_band

INTEGER :: i, j, l, n

!-----------------------------------------------------------------------------
! Initialise
!-----------------------------------------------------------------------------
use_file = .TRUE.      ! Default is for every variable to be read from file
FILE=''       ! Empty file name
z_land_name = 'z_land' ! Default variable name

!-----------------------------------------------------------------------------
! If some tiles have absolute heights then the gridbox mean height must also
! be provided. For a single point, this can come from the namelist, otherwise
! we need to use the file option
!-----------------------------------------------------------------------------
jules_vars_data%z_land_ij(:,:) = 0.0

IF ( ANY(l_elev_absolute_height) ) THEN

  !---------------------------------------------------------------------------
  !   Read namelist
  !---------------------------------------------------------------------------
  CALL log_info("init_z_land", "Reading JULES_Z_LAND namelist...")

  !   First, we read the namelist
  READ(namelist_unit, NML = jules_z_land, IOSTAT = ERROR, IOMSG = iomessage)
  IF ( ERROR /= 0 )                                                            &
    CALL log_fatal("init_z_land",                                              &
                 "Error reading namelist JULES_Z_LAND " //                     &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

  !---------------------------------------------------------------------------
  !   Set values derived from namelist and verify for consistency
  !---------------------------------------------------------------------------
  CALL log_info("init_z_land","Some tiles have  " //                           &
         "absolute heights above sea-level, see l_elev_absolute_height " //    &
         "for which. Where l_elev_absolute_height is false, surf_hgt "   //    &
         "offsets can only be applied as global to that tile "           //    &
         "type (usually these are 0, indicating no offset from "         //    &
         "the gridbox mean).")

  !---------------------------------------------------------------------------
  !   Set the heights (relative or absolute) to be constant across a domain.
  !---------------------------------------------------------------------------
  DO n = 1,nsurft
    jules_vars_data%surf_hgt_surft(:,n) = surf_hgt_band(n)
  END DO

  !---------------------------------------------------------------------------
  !   If we have a grid, set gridbox mean heights from the specified file
  !---------------------------------------------------------------------------
  IF ( use_file ) THEN

    CALL log_info("init_z_land",                                               &
                  "Data is on a grid - reading z_land from file " //           &
                  TRIM(FILE))

    IF ( ERROR > 0 )                                                           &
      CALL log_fatal("init_z_land", "Error allocating z_land")

    !     Check that a file name was provided
    IF ( LEN_TRIM(FILE) == 0 )                                                 &
      CALL log_fatal("init_z_land", "No file name provided for gridbox " //    &
                     "mean heights")

    CALL fill_variables_from_file(FILE,                                        &
                                  [ 'z_land_land' ], [ z_land_name ],          &
                                  is_climatology = [ .FALSE. ] )

    DO l = 1,land_pts
      j = ( ainfo_data%land_index(l) - 1 ) / t_i_length + 1
      i = ainfo_data%land_index(l) - (j-1) * t_i_length
      jules_vars_data%z_land_ij(i,j) = jules_vars_data%z_land_land(l)
    END DO

    !-------------------------------------------------------------------------
    !     If we are reading data at a single point, read height from the
    !     namelist
    !-------------------------------------------------------------------------
  ELSE

    CALL log_info("init_z_land",                                               &
                  "Data is at a single point - reading z_land from " //        &
                  "z_land_io in namelist JULES_Z_LAND")
    jules_vars_data%z_land_ij(1,1) = z_land_io

    !-------------------------------------------------------------------------
    !     Check that a value for the gridbox height has been set
    !-------------------------------------------------------------------------
    IF ( z_land_io == rmdi )                                                   &
      CALL log_fatal("init_z_land", "Some tiles have absolute "   //           &
                     "heights above sea-level but no value for "  //           &
                     "z_land has been provided. Set a value for " //           &
                     "z_land in the JULES_Z_LAND namelist")

    !-------------------------------------------------------------------------
    !     Check that a value for the elevation bands have been set
    !-------------------------------------------------------------------------
    IF ( ANY(surf_hgt_band(1:nsurft)  == rmdi)  )                              &
      CALL log_fatal("init_z_land", "Some tiles have absolute "   //           &
                     "heights above sea-level but no values for " //           &
                     "elevation bands have been provided. Set a " //           &
                     "value for surf_hgt_band in the "//                       &
                     "JULES_Z_LAND namelist")

  END IF

END IF

RETURN

END SUBROUTINE init_z_land

END MODULE init_z_land_mod
#endif
