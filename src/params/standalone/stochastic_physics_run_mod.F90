#if !defined(UM_JULES)
! *****************************COPYRIGHT********************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT********************************

! **********************************************************************
! THIS VERSION OF stochastic_physics_run_mod IS STRICTLY FOR USE WITH
! STANDALONE JULES!
! **********************************************************************

!  Data module for switches/options concerned with the stochastic
!  physics scheme.
! Description:
!   Module containing runtime options/data used by the stochastic
!   physics scheme.

! Method:
!   Switches and associated data values used by the stochastic physics
!   scheme are defined here and assigned default values.

!   Any routine wishing to use these options may do so with the 'USE'
!   statement.

! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
! This file belongs in section: Stochastic Physics

! Code Description:
!   Language: FORTRAN 90

MODULE stochastic_physics_run_mod

USE max_dimensions, ONLY: npft_max
USE missing_data_mod, ONLY: rmdi, imdi

! Declarations:

IMPLICIT NONE

!=======================================================================
!   Permissible settings for STPH options.
!=======================================================================

! Logical to switch on Random Parameter Scheme.  Currently this
! scheme can only be used when JULES is coupled to the UM.  For
! standalone JULES it should always be false.
LOGICAL, PARAMETER :: l_rp2 = .FALSE.

INTEGER, PARAMETER :: i_rp_scheme = imdi ! Switch to specify RP scheme option
INTEGER, PARAMETER :: i_rp2b = 1 ! Parameter corresponding to RP2b scheme
INTEGER, PARAMETER :: n_rp = 3 ! Length of RP entry
INTEGER, PARAMETER :: rp_idx = 2  ! Index of RP value for rp

! Default values for land-surface random parameters
! These values are only used when l_rp2 = .TRUE. and
! i_rp_scheme = i_rp2b
REAL :: lai_mult_rp(npft_max)
DATA lai_mult_rp / npft_max * rmdi /
REAL :: z0hm_pft_rp(npft_max)
DATA z0hm_pft_rp / npft_max * rmdi /
REAL :: z0hm_soil_rp(n_rp)
DATA z0hm_soil_rp / n_rp * rmdi /
REAL :: z0v_rp(npft_max)
DATA z0v_rp / npft_max * rmdi /
REAL :: z0_soil_rp(n_rp)
DATA z0_soil_rp / n_rp * rmdi /
REAL :: z0_urban_mult_rp(n_rp)
DATA z0_urban_mult_rp / n_rp * rmdi /
REAL :: orog_drag_param_rp(n_rp)
DATA orog_drag_param_rp / n_rp * rmdi /
REAL :: alnir_rp(npft_max)
DATA alnir_rp / npft_max * rmdi /
REAL :: alpar_rp(npft_max)
DATA alpar_rp / npft_max * rmdi /
REAL :: omega_rp(npft_max)
DATA omega_rp / npft_max * rmdi /
REAL :: omnir_rp(npft_max)
DATA omnir_rp / npft_max * rmdi /

END MODULE stochastic_physics_run_mod

#endif
