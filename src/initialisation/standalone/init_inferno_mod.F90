#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_inferno_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_inferno(nml_dir)

USE jules_inferno_mod, ONLY: read_nml_jules_inferno,                           &
                           print_nlist_jules_inferno,                          &
                           check_jules_inferno,                                &
                           l_inferno, l_trif_fire, ignition_method,            &
                           ignition_constant, ignition_vary_natural,           &
                           ignition_vary_natural_human

USE logging_mod, ONLY: log_info

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises the inferno fire model parameters and properties
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

CALL read_nml_jules_inferno(nml_dir)

CALL print_nlist_jules_inferno()

IF ( .NOT. l_inferno .AND. .NOT. l_trif_fire ) RETURN

CALL check_jules_inferno()

!-----------------------------------------------------------------------------
! Print some human friendly summary information about the selected options.
!-----------------------------------------------------------------------------
IF ( l_inferno ) THEN
  CALL log_info("init_inferno",                                                &
                "Interactive fires and emissions (INFERNO) will be diagnosed")
  IF (ignition_method == ignition_constant ) THEN
    CALL log_info("init_inferno",                                              &
                  "Constant or ubiquitous ignitions (INFERNO)")
  ELSE IF (ignition_method == ignition_vary_natural ) THEN
    CALL log_info("init_inferno",                                              &
                  "Constant human ignitions, varying lightning (INFERNO)")
  ELSE IF (ignition_method == ignition_vary_natural_human ) THEN
    CALL log_info("init_inferno",                                              &
                  "Fully prescribed ignitions (INFERNO)")
  END IF
END IF

IF ( l_trif_fire ) THEN
  CALL log_info("init_inferno",                                                &
                "Fires will interact with the carbon cycle in triffid")
END IF

RETURN

END SUBROUTINE init_inferno
END MODULE init_inferno_mod
#endif
