#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************
MODULE init_soil_ecosse_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_soil_ecosse(nml_dir)
!-----------------------------------------------------------------------------
! Description:
!   Reads in ECOSSE namelist items and checks them for consistency.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGEOCHEMISTRY
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE ancil_info, ONLY:                                                          &
  ! imported scalar variables
  dim_cslayer

USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE jules_soil_biogeochem_mod, ONLY:                                           &
  ! imported scalar parameters
  soil_model_ecosse,                                                           &
  ! imported scalar variables
  soil_bgc_model

USE jules_soil_ecosse_mod, ONLY:                                               &
  ! imported procedures
  check_jules_soil_ecosse,                                                     &
  ! imported scalar parameters
  temp_mod_q10, temp_mod_smith, water_mod_clark, water_mod_smith,              &
  ! imported scalar variables
  dt_soilc, l_decomp_slow, l_driver_ave, l_soil_N,                             &
  plant_input_profile, temp_modifier, water_modifier,                          &
  ! imported array variables
  dz_soilc,                                                                    &
  ! imported namelists
  jules_soil_ecosse

USE logging_mod, ONLY: log_info, log_fatal

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

!-----------------------------------------------------------------------------
! Local scalar parameters.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER ::                                                 &
  RoutineName = 'init_soil_ecosse'   ! Name of this routine.
!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

!-----------------------------------------------------------------------------
! If ECOSSE is not selected, there's nothing to do.
!-----------------------------------------------------------------------------
IF ( soil_bgc_model /= soil_model_ecosse ) RETURN

!-----------------------------------------------------------------------------
! Open the namelist file.
!-----------------------------------------------------------------------------
CALL log_info( RoutineName, "Reading JULES_SOIL_ECOSSE namelist..." )

OPEN(namelist_unit,                                                            &
     FILE=(TRIM(nml_dir) // '/' // 'jules_soil_ecosse.nml'),                   &
     STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR,           &
     IOMSG = iomessage)

IF ( ERROR /= 0 ) THEN
  CALL log_fatal( RoutineName,                                                 &
                  "Error opening namelist file jules_soil_ecosse.nml " //      &
                  "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG="    //      &
                  TRIM(iomessage) // ")" )
END IF

!-----------------------------------------------------------------------------
! Read the namelist.
!-----------------------------------------------------------------------------
READ(namelist_unit, NML = jules_soil_ecosse, IOSTAT = ERROR,                   &
     IOMSG = iomessage)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal( RoutineName,                                                 &
                  "Error reading namelist JULES_SOIL_ECOSSE "       //         &
                  "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //         &
                  TRIM(iomessage) // ")" )
END IF

!-----------------------------------------------------------------------------
! Close namelist file.
!-----------------------------------------------------------------------------
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal( RoutineName,                                                 &
                  "Error closing namelist file jules_soil_ecosse.nml " //      &
                  "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG="   //       &
                  TRIM(iomessage) // ")" )
END IF

!-----------------------------------------------------------------------------
! Check that settings are reasonable.
!-----------------------------------------------------------------------------
CALL check_jules_soil_ecosse

!-----------------------------------------------------------------------------
! Print some human friendly summary information about the selected options.
!-----------------------------------------------------------------------------

! Top-level switches.
IF ( l_soil_n ) THEN
  CALL log_info( RoutineName, "Soil N is modelled using ECOSSE." )
ELSE
  CALL log_info( RoutineName, "Soil N is NOT modelled by ECOSSE." )
END IF

! Layering and timestep.
CALL log_info( RoutineName,                                                    &
               "Number of soil layers for ECOSSE= " //                         &
               TRIM(to_string(dim_cslayer)) )
CALL log_info( RoutineName,                                                    &
               "Thicknesses (m): " // TRIM(to_string(dz_soilc)) )
CALL log_info( RoutineName,                                                    &
               "ECOSSE timestep length (s)= " // TRIM(to_string(dt_soilc)) )

IF ( l_driver_ave ) THEN
  CALL log_info( RoutineName,                                                  &
                 "Physical drivers of ECOSSE will be averaged over time." )
ELSE
  CALL log_info( RoutineName,                                                  &
                 "Physical drivers of ECOSSE will be instantaneous values.")
END IF

IF ( l_soil_N ) THEN
  CALL log_info( RoutineName,                                                  &
                 "l_decomp_slow= " // TRIM(to_string(l_decomp_slow)) )
END IF

SELECT CASE ( temp_modifier )
CASE ( temp_mod_smith )
  CALL log_info( RoutineName,                                                  &
                 "Smith et al. (2010)  form of T modifier equation used." )
CASE ( temp_mod_q10 )
  CALL log_info( RoutineName,                                                  &
                 "Clark et al. (2005) --Q10-- form of T modifier equation used." )
END SELECT

SELECT CASE ( water_modifier )
CASE ( water_mod_smith )
  CALL log_info( RoutineName,                                                  &
                 "4-pool form of moisture modifier equation used." )
CASE ( water_mod_clark )
  CALL log_info( RoutineName,                                                  &
                 "JULES form of moisture modifier equation used." )
END SELECT

CALL log_info( RoutineName,                                                    &
               "plant_input_profile = " //                                     &
               TRIM(to_string(plant_input_profile)) )

RETURN
END SUBROUTINE init_soil_ecosse

END MODULE init_soil_ecosse_mod
#endif
