#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE init_nvegparm_jules(nml_dir)


USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE jules_surface_types_mod, ONLY: nnvg, npft

USE nvegparm_io, ONLY: jules_nvegparm, init_nvegparm_allocated

USE nvegparm, ONLY: check_jules_nvegparm

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises the non-vegetation parameters
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
! Work variables
INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

CHARACTER(LEN=*), PARAMETER :: routinename='INIT_NVEGPARM_JULES'

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
CALL log_info(routinename, "Reading JULES_NVEGPARM namelist...")

! Open the nveg parameters namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'nveg_params.nml'),          &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error opening namelist file nveg_params.nml " //             &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

READ(namelist_unit, NML = jules_nvegparm, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error reading namelist JULES_NVEGPARM " //                   &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

! Close the namelist file
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error closing namelist file nveg_params.nml " //             &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")


!-----------------------------------------------------------------------------
! Process the namelist values
!-----------------------------------------------------------------------------
! Copy values from fixed length arrays used in namelist into allocated arrays
CALL init_nvegparm_allocated()
CALL check_jules_nvegparm(nnvg,npft)

RETURN

END SUBROUTINE init_nvegparm_jules
#endif
