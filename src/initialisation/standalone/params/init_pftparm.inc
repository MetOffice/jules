#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE init_pftparm_jules(nml_dir,progs)

USE missing_data_mod, ONLY:                                                    &
!  imported scalar parameters
     rmdi

USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE jules_surface_types_mod, ONLY: npft, nnpft

USE ancil_info, ONLY: land_pts

USE pftparm_io, ONLY: jules_pftparm, init_pftparm_allocated,                   &
                      canht_ft_io, lai_io

USE pftparm, ONLY: print_nlist_jules_pftparm, check_jules_pftparm

USE errormessagelength_mod, ONLY: errormessagelength

USE jules_print_mgr, ONLY: jules_print

!TYPE definitions
USE prognostics, ONLY: progs_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises the PFT parameters
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

!TYPES containing the data
TYPE(progs_type), INTENT(IN OUT) :: progs

! Work variables
INTEGER :: ERROR  ! Error indicator
INTEGER :: i      ! Loop counter.
CHARACTER(LEN=errormessagelength) :: iomessage

CHARACTER(LEN=*), PARAMETER :: routinename='INIT_PFTPARM_JULES'

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
CALL log_info(routinename, "Reading JULES_PFTPARM namelist...")

! Open the pft parameters namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'pft_params.nml'),           &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error opening namelist file pft_params.nml " //              &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

READ(namelist_unit, NML = jules_pftparm, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error reading namelist JULES_PFTPARM " //                    &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

! Close the namelist file
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error closing namelist file pft_params.nml " //              &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

!-----------------------------------------------------------------------------
! Process the namelist values
!-----------------------------------------------------------------------------
CALL init_pftparm_allocated()
CALL print_nlist_jules_pftparm()

! Print members of pftparm_io not copied to pftparm_mod.
CALL jules_print('pftparm',                                                    &
    'Contents of namelist jules_pftparm (standalone only)')
WRITE(iomessage,*)' canht_ft = ',canht_ft_io(1:npft)
CALL jules_print('pftparm',iomessage)
WRITE(iomessage,*)' lai = ',lai_io(1:npft)
CALL jules_print('pftparm',iomessage)
CALL jules_print('pftparm',                                                    &
    '- - - - - - end of namelist - - - - - -')

CALL check_jules_pftparm(npft,nnpft)
! Check values of pftparm_io not copied to pftparm_mod before spreading
IF ( ANY( ABS( canht_ft_io(1:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for canht_ft")
END IF
IF ( ANY( ABS( lai_io(1:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for lai")
END IF

! Spread over land_pts array
progs%canht_pft(:,:)   = SPREAD(canht_ft_io(1:npft), 1, land_pts)
progs%lai_pft(:,:)     = SPREAD(lai_io(1:npft), 1, land_pts)

RETURN

END SUBROUTINE init_pftparm_jules

#endif
