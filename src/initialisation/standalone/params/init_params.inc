#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE init_params(nml_dir,progs,land_pts,nsurft,nnpft,npft,nmasst,ntype)

!Module imports
USE jules_model_environment_mod, ONLY: lsm_id, jules, cable
USE c_z0h_z0m,                   ONLY: c_z0h_z0m_print, c_z0h_z0m_check

!TYPE definitions
USE prognostics, ONLY: progs_type

!Common modules
USE ereport_mod,              ONLY: ereport
USE jules_print_mgr,          ONLY: jules_message

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises various model parameters
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
INTEGER, INTENT(IN) :: land_pts, nnpft, npft, nsurft, nmasst, ntype

CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists
CHARACTER(LEN=*), PARAMETER :: RoutineName='INIT_PARAMS'

!TYPES containing the data
TYPE(progs_type), INTENT(IN OUT) :: progs

!-----------------------------------------------------------------------------
! Variables

INTEGER :: errorstatus

!-----------------------------------------------------------------------------

! Process the PFT and non-veg parameters

SELECT CASE(lsm_id)

CASE (jules)
  CALL init_pftparm_jules(nml_dir,progs)
  CALL init_nvegparm_jules(nml_dir)
  CALL c_irrigation_print()
  CALL check_irrigation()
  CALL c_z0h_z0m_print()
  CALL c_z0h_z0m_check(ntype)

CASE (cable)
  CALL init_vegin_cbl(nml_dir,progs)
  CALL init_soilin_cbl(nml_dir)

CASE DEFAULT
  errorstatus = 101
  WRITE(jules_message,'(A,I0)') 'Unrecognised surface scheme. lsm_id = ',      &
     lsm_id
  CALL ereport(RoutineName, errorstatus, jules_message)

END SELECT

! Process the crop parameters
CALL init_cropparm(nml_dir)

! Process TRIFFID parameters
CALL init_triffid(nml_dir)

! Process RED parameters
CALL init_red(nml_dir,land_pts,nsurft,nnpft,npft,nmasst)

! Process atmospheric deposition species.
CALL init_deposition_species(nml_dir)

RETURN

END SUBROUTINE init_params
#endif
