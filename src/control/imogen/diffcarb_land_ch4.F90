#if !defined(UM_JULES)
! *********************************************************************
! Calculates the global change in atmospheric CH4
! *********************************************************************
SUBROUTINE diffcarb_land_ch4(land_pts, d_land_atmos_ch4, darea, dctot_ch4)

USE imogen_run, ONLY: fch4_ref, yr_fch4_ref

USE jules_print_mgr, ONLY: jules_message, jules_print

USE model_time_mod, ONLY: current_time
USE parallel_mod, ONLY: is_master_task

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Calculates the global change in atmospheric CH4 due to sum of the global ch4
!   emissions
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Written by: E. Burke and E. Comyn-Platt (2017/18)
!
! Code Description:
!   Language: Fortran 90.
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) ::                                                         &
  land_pts         ! Number of land points

REAL, INTENT(IN) ::                                                            &
  darea(land_pts),                                                             &
                   ! Gridbox area (m2)
  dctot_ch4(land_pts)
                   ! Total surface CH4 emissions for the period of
                   ! accumulation, this should be 1 year. (kg C yr-1)

REAL, INTENT(OUT) ::                                                           &
  d_land_atmos_ch4 ! Change in global CH4 flux from a fixed reference flux
                   ! specified in the imogen run namelist
                   ! flux is from land to atmosphere (kg C / year)

REAL ::                                                                        &
  global_total_ch4_emis,                                                       &
                   ! Global total CH4 flux from land to atmosphere
                   ! (kg C / year)
  kgC_to_TgCH4     ! Conversion factor to go from kg carbon to Tg of CH4

INTEGER ::                                                                     &
  l                ! Looping parameter


global_total_ch4_emis = 0.0
kgC_to_TgCH4 = 1e-9 * (16.04 / 12.01)

DO l = 1,land_pts
  ! Sum the gridcell methane emission to the global total methane emission
  ! kg C/yr (1e-9TgC/yr)
  global_total_ch4_emis = global_total_ch4_emis + darea(l) * dctot_ch4(l)
END DO

! fch4_ref*1.e9/1.e12*12./16. is in Gt C/yr
! if at or after refence year modify methane emissions
IF (current_time%year >= yr_fch4_ref) THEN
  d_land_atmos_ch4 = ( (global_total_ch4_emis) - (fch4_ref / kgC_to_TgCH4) )
ELSE
  d_land_atmos_ch4 = 0.0
END IF

IF ( is_master_task() ) THEN
  WRITE(jules_message, *) 'global_total_ch4_emis,fch4_ref,d_land_atmos_ch4',   &
         global_total_ch4_emis * kgC_to_TgCH4, fch4_ref,                       &
         d_land_atmos_ch4 * kgC_to_TgCH4
  CALL jules_print('diffcarb_land_ch4', jules_message)
END IF
RETURN

END SUBROUTINE diffcarb_land_ch4
#endif
