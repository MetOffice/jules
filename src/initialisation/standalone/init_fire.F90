#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

SUBROUTINE init_fire(nml_dir,land_index)

USE io_constants,     ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE logging_mod,      ONLY: log_info,log_fatal

USE fire_mod,         ONLY: fire_cntl, l_fire

USE fire_allocate_mod,ONLY: fire_allocate

USE fire_init_mod,    ONLY: fire_init

USE metstats_mod,     ONLY: l_metstats

USE missing_data_mod, ONLY: imdi

USE ancil_info,       ONLY: land_pts

IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

INTEGER, INTENT(IN) :: land_index(land_pts)

INTEGER :: ERROR  ! Error indicators

!Temp variables
LOGICAL :: mcarthur_flag = .FALSE.
LOGICAL :: canadian_flag = .FALSE.
LOGICAL :: nesterov_flag = .FALSE.
LOGICAL :: canadian_hemi_opt = .FALSE.
INTEGER :: mcarthur_opt = imdi

!-----------------------------------------------------------------------------
! Definition of the fire_Switches namelist
!-----------------------------------------------------------------------------
NAMELIST  / fire_switches/ l_fire, mcarthur_flag, mcarthur_opt,                &
                         canadian_flag, canadian_hemi_opt,                     &
                         nesterov_flag
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Read the namelist
!-----------------------------------------------------------------------------

  ! Open the fire namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'fire.nml'),                 &
    STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_fire",                                                  &
    "Error opening namelist file fire.nml " //                                 &
    "(IOSTAT=" // TRIM(to_string(ERROR)) // ")")


! There is one namelist to read from this file
CALL log_info("init_fire", "Reading FIRE_SWITCHES namelist...")
READ(namelist_unit, NML = fire_switches, IOSTAT = ERROR)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_fire",                                                  &
    "Error reading namelist fire_SWITCHES " //                                 &
    "(IOSTAT=" // TRIM(to_string(ERROR)) // ")")

! Close the namelist file
CLOSE(namelist_unit, IOSTAT = ERROR)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_fire",                                                  &
    "Error closing namelist file fire.nml " //                                 &
    "(IOSTAT=" // TRIM(to_string(ERROR)) // ")")

! If not running fire, we can bail
IF ( .NOT. l_fire ) RETURN

! Copy across namelist variables to the fire_cntl structure
fire_cntl%canadian%flag     = canadian_flag
fire_cntl%canadian%hemi_opt = canadian_hemi_opt

fire_cntl%mcarthur%flag     = mcarthur_flag
fire_cntl%mcarthur%option   = mcarthur_opt

fire_cntl%nesterov%flag     = nesterov_flag

!Call the module initialisation subroutine
CALL fire_allocate(land_index)
CALL fire_init()

!Activate the metstats module
l_metstats = .TRUE.

RETURN

END SUBROUTINE init_fire
#endif
