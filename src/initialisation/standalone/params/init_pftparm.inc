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

USE jules_surface_types_mod, ONLY: npft, nnpft

USE ancil_info, ONLY: land_pts

USE pftparm_io, ONLY: read_nml_jules_pftparm

USE pftparm, ONLY: print_nlist_jules_pftparm, check_jules_pftparm,             &
                   canht_pft, lai_pft

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

CALL read_nml_jules_pftparm (nml_dir)
!-----------------------------------------------------------------------------
! Process the namelist values
!-----------------------------------------------------------------------------
CALL print_nlist_jules_pftparm()
CALL check_jules_pftparm()

! Spread over land_pts array
progs%canht_pft(:,:)   = SPREAD(canht_pft(:), 1, land_pts)
progs%lai_pft(:,:)     = SPREAD(lai_pft(:), 1, land_pts)

RETURN

END SUBROUTINE init_pftparm_jules

#endif
