!******************************COPYRIGHT****************************************
! (c) British Antarctic Survey and University of Bristol, 2023.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT****************************************

MODULE jules_water_tracers_mod

!-------------------------------------------------------------------------------
! Description:
!   This module contains water tracer settings and common functions
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in Surface
!
! Code Description:
!   Language: Fortran 90.
!-------------------------------------------------------------------------------

USE missing_data_mod,        ONLY: imdi, rmdi
USE um_types,                ONLY: real_jlslsm
USE free_tracers_inputs_mod, ONLY: max_wtrac

IMPLICIT NONE

! Switch to control water tracers in JULES.
! If UM_JULES, this is set in wtrac_setup_jls based on user input.
! If standalone JULES, water tracers are currently turned off.

LOGICAL :: l_wtrac_jls = .FALSE.

! Number of water tracers used in JULES.
! If UM_JULES, this is set in wtrac_setup_jls based on user input. (Note,
!   the number of water tracers used in JULES can be a subset of those used
!   in the UM.)
! If standalone JULES, water tracers are turned off, but n_wtrac_jls must be
!   set to 1 as it sets the size of arrays.

INTEGER :: n_wtrac_jls = 1

! Logical to control the water tracer calculations on the implicit scheme
! (Water tracers only need to be updated on the final dynamics loop in
!  UMJULES)
! This is passed into JULES in the call to surf_couple_implicit

LOGICAL :: l_wtrac_imp_jls = .FALSE.

! Number of evaporative sources modelled:
!   snow, canopy intercepted water, soil evapotranspiration (bare soil
!   evap and transpiration from vegetative tiles),  and open water
!   (i.e. lakes).

INTEGER, PARAMETER :: n_evap_srce = 4

INTEGER, PARAMETER :: evap_srce_snow = 1
INTEGER, PARAMETER :: evap_srce_cano = 2
INTEGER, PARAMETER :: evap_srce_soil = 3
INTEGER, PARAMETER :: evap_srce_lake = 4

! Standard ratio for water tracers
! If UM_JULES, these are set in wtrac_init_jls
REAL(KIND=real_jlslsm) :: standard_ratio_wtrac(max_wtrac) = 1.0

! Water tracer fixed ratio for lakes
! If UM_JULES, these are set in wtrac_init_jls
REAL(KIND=real_jlslsm) :: wtrac_lake_fixed_ratio(max_wtrac) = 1.0

! Set these here for now but possibly put in namelist when isotope code
! is fully developed.
REAL(KIND=real_jlslsm), PARAMETER :: h218o_lake_ratio = 1.0
REAL(KIND=real_jlslsm), PARAMETER :: hdo_lake_ratio   = 1.0

! Minimum water value used to calculate water tracer ratios
REAL(KIND=real_jlslsm), PARAMETER :: min_q_ratio = 1.0e-18

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
                            ModuleName='JULES_WATER_TRACERS_MOD'

CONTAINS

!----------------------------------------------------------------------------

REAL FUNCTION wtrac_calc_ratio_fn_jules(i_wt, q_wtrac, q)

!-----------------------------------------------------------------------------
! Description:
!
!  Function to calculate the ratio of water tracer to normal water.
!  If there is no water, set ratio to Standard value.
!  This function is for 'real_jlslsm' precision inputs/outputs (i.e. used
!  in JULES).
!
!-----------------------------------------------------------------------------

IMPLICIT NONE

INTEGER, INTENT(IN) :: i_wt                     ! Water tracer number

REAL(KIND=real_jlslsm), INTENT(IN) :: q_wtrac   ! Water tracer
REAL(KIND=real_jlslsm), INTENT(IN) :: q         ! Normal water

REAL(KIND=real_jlslsm) :: q_ratio               ! Output ratio
REAL(KIND=real_jlslsm) :: q_ratio_std           ! Standard ratio

q_ratio_std = standard_ratio_wtrac(i_wt)

IF (ABS(q) > min_q_ratio) THEN
  q_ratio = q_wtrac / q
ELSE
  q_ratio = q_ratio_std
END IF

! Yet to be decided if it's best to block negative ratios or not.
!IF (q_ratio < 0.0) q_ratio = q_ratio_std

wtrac_calc_ratio_fn_jules = q_ratio

END FUNCTION wtrac_calc_ratio_fn_jules

END MODULE jules_water_tracers_mod
