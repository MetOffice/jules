! *****************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use
! and distribution under the JULES collaboration agreement, subject
! to the terms and conditions set out therein.
!
! [Met Office Ref SC0237]
! *****************************COPYRIGHT****************************************

MODULE fire_mod

  !No imports

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!
! Description:
!   Module defining a set of TYPE variables to store data for various fire
!   models
!   Each model has several TYPEs associated with it
!   model_progs   -contains all prognostic variables.
!                  Declared as an ALLOCATABLE array over land_pts
!   model_diags   -contains all diagnostic variables
!                  Declared as an ALLOCATABLE array over land_pts
!   model_cntl    -contains flags, constants etc. Generally scalars used to
!                  control model execution options
!
!   fire_inis_struct contains initialisation values for all prognostic variables
!
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.

! Module constants

  !Switch for if we're using the fire module
LOGICAL :: l_fire = .FALSE.

!Canadian model user defined types for prognostics, diagnotics and control
TYPE :: canadian_progs
  REAL(KIND=real_jlslsm)    :: ffmc, ffmc_mois, dmc, dc
  LOGICAL :: hemi_NtSf !T if land point in the N hemisphere, else F
END TYPE canadian_progs

TYPE :: canadian_diags
  REAL(KIND=real_jlslsm)    :: isi, bui, fwi
END TYPE canadian_diags

TYPE :: canadian_cntl
  LOGICAL :: flag     = .FALSE.
  LOGICAL :: hemi_opt = .FALSE.
END TYPE canadian_cntl

!McArthur model user defined types for prognostics, diagnotics and control
TYPE :: mcarthur_progs
  REAL(KIND=real_jlslsm) :: r_dr, n_dr
END TYPE mcarthur_progs

TYPE :: mcarthur_diags
  REAL(KIND=real_jlslsm) :: ffdi
END TYPE mcarthur_diags

TYPE :: mcarthur_cntl
  LOGICAL                   :: flag       = .FALSE.
  INTEGER                   :: option     = 1
  REAL(KIND=real_jlslsm)    :: smc_coeff
  REAL(KIND=real_jlslsm)    :: targ_depth = 0.8
  !Depth of soil column (m) for drought calc
END TYPE mcarthur_cntl

!Nesterov model user defined types for prognostics, diagnotics and control
TYPE :: nesterov_progs
  REAL(KIND=real_jlslsm) :: findex
END TYPE nesterov_progs

TYPE :: nesterov_cntl
  LOGICAL :: flag
END TYPE nesterov_cntl

!Overall user defined type for fire prognostics
TYPE :: fire_prog_struct
  TYPE (canadian_progs) :: canadian
  TYPE (mcarthur_progs) :: mcarthur
  TYPE (nesterov_progs) :: nesterov
END TYPE fire_prog_struct

!Overall user defined type for fire diagnostics
TYPE :: fire_diag_struct
  TYPE (canadian_diags) :: canadian
  TYPE (mcarthur_diags) :: mcarthur
  !TYPE (nesterov_diags) :: nesterov
END TYPE fire_diag_struct

!Overall user defined type for fire control variables
TYPE :: fire_cntl_struct
  TYPE (canadian_cntl) :: canadian
  TYPE (mcarthur_cntl) :: mcarthur
  TYPE (nesterov_cntl) :: nesterov
END TYPE fire_cntl_struct

!User defined type for initial values for fire prognostics
TYPE :: fire_inis_struct
  REAL(KIND=real_jlslsm) ::            canadian_ffmc      = 0.0,               &
                     canadian_ffmc_mois = 0.0,                                 &
                     canadian_dmc       = 0.0,                                 &
                     canadian_dc        = 0.0,                                 &
                     canadian_isi       = 0.0,                                 &
                     canadian_bui       = 0.0,                                 &
                     canadian_fwi       = 0.0,                                 &
                     mcarthur_r_dr      = 0.005,                               &
                     mcarthur_n_dr      = 1.0  ,                               &
                     mcarthur_ffdi      = 0.0  ,                               &
                     nesterov_index     = 0.0
END TYPE fire_inis_struct

!Declarations
TYPE (fire_prog_struct), ALLOCATABLE :: fire_prog(:)
TYPE (fire_diag_struct), ALLOCATABLE :: fire_diag(:)
TYPE (fire_cntl_struct), SAVE        :: fire_cntl
TYPE (fire_inis_struct), SAVE        :: fire_inis

!No subroutines or functions

!#include "canadian_calc.inc"



END MODULE fire_mod
