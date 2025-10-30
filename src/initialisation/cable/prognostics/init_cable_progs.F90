#if !defined(UM_JULES)
!-----------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

MODULE init_cable_progs_mod

!------------------------------------------------------------------------------
! Description:
!   Main driver to dec/alloc/initialize CABLE prognostic variables
!
! This MODULE is USEd:
!      init.F90
!
! This MODULE contains 1 public Subroutine:
!      init_cable_progs
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in CABLE SCIENCE
!------------------------------------------------------------------------------

IMPLICIT NONE

PUBLIC :: init_cable_progs
PRIVATE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='INIT_CABLE_PROGS_MOD'

CONTAINS

SUBROUTINE init_cable_progs( land_pts, nsurft, sm_levels, lsm_id, cable,       &
                         progs_cbl_vars, progs_cbl_vars_data )

! Description:
!   Nothing further to add to the module description

USE read_cable_progs_mod,         ONLY: read_cable_progs
USE progs_cbl_vars_mod,           ONLY: progs_cbl_vars_type,                   &
                                        progs_cbl_vars_data_type,              &
                                        progs_cbl_vars_alloc,                  &
                                        progs_cbl_vars_assoc

IMPLICIT NONE

INTEGER, INTENT(IN) :: land_pts, nsurft,sm_levels
INTEGER, INTENT(IN) :: lsm_id, cable
TYPE(progs_cbl_vars_data_type), INTENT(IN OUT) :: progs_cbl_vars_data
TYPE(progs_cbl_vars_type), INTENT(IN OUT)      :: progs_cbl_vars

CALL progs_cbl_vars_alloc(land_pts, nsurft, sm_levels, lsm_id, cable,          &
                          progs_cbl_vars_data )
CALL progs_cbl_vars_assoc(progs_cbl_vars, progs_cbl_vars_data )
CALL read_cable_progs()

RETURN

END SUBROUTINE init_cable_progs

END MODULE init_cable_progs_mod
#endif
