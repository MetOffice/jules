#if !defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE imgn_vars_mod

USE missing_data_mod, ONLY: rmdi

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
! Module containing additional variables for IMOGEN.
! this is an alternate way of driving JULES and will never be part of the UM
! Therefore I have tried to keep this routine separate from the UM
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Implementation for field variables:
! Each variable is declared in both the 'data' TYPE and the 'pointer' type.
! Instances of these types are declared at as high a level as required
! This is to facilitate advanced memory management features, which are generally
! not visible in the science code.
! Checklist for adding a new variable:
! -add to data_type
! -add to pointer_type
! -add to the allocate routine, passing in any new dimension sizes required
!  by argument (not via USE statement)
! -add to the deallocate routine
! -add to the assoc and nullify routines
!-------------------------------------------------------------------------------

! imogen variables.
TYPE :: imgn_vars_data_type
  REAL, ALLOCATABLE :: dtemp_g(:)
            ! global mean temperature anomaly (K)
  REAL, ALLOCATABLE :: c_emiss_data(:)
            ! Carbon emissions (GtC/year)
  REAL, ALLOCATABLE :: dctot_co2(:)
            ! Change in total surface gridbox CO2
            ! content during a period "YEAR_CO2" (kg C/m2)
  REAL, ALLOCATABLE :: dctot_ch4(:)
            ! Total surface gridbox CH4 flux to the atmosphere
            ! during a period "YEAR_CO2" (kg C/m2)
  REAL, ALLOCATABLE :: dtemp_o(:)
            ! Ocean temperature anomalies (K)
  REAL, ALLOCATABLE :: fa_ocean(:)
            ! CO2 fluxes from atmos to  ocean (+ up) (ppm/m2/yr)
  REAL, ALLOCATABLE :: d_land_atmos_co2(:)
            ! Change in atmos CO2 conc from land co2 feedbacks (ppm/year)
  REAL, ALLOCATABLE :: d_land_atmos_ch4(:)
            ! Change in global total CH4 flux from the land to the atmosphere
            ! w.r.t a specified reference flux, fch4_ref (kgC/year)
  REAL, ALLOCATABLE :: d_ocean_atmos(:)
            ! Change in atmospheric CO2 concentration due to
            ! ocean feedbacks (ppm/year)
  REAL, ALLOCATABLE :: imogen_radf(:)
            ! Total radiative forcing applied in IMOGEN (W/m2)
  REAL, ALLOCATABLE :: q_non_co2(:)
            ! Non-CO2 radiative forcing
  REAL, ALLOCATABLE :: co2_mmr_prescribe(:)
            ! Precscribed CO2 concentration in kg/kg
  REAL, ALLOCATABLE :: co2_start_ppmv(:)
            ! Atmospheric CO2 concentration at start of year (ppmv)
  REAL, ALLOCATABLE :: co2_ppmv(:)
            ! Atmospheric CO2 concentration (ppmv)
  REAL, ALLOCATABLE :: co2_change_ppmv(:)
            ! Change in CO2 between restarts (ppmv)
  REAL, ALLOCATABLE :: ch4_ppbv(:)
            ! Atmospheric CH4 concentration (ppbv)

END TYPE imgn_vars_data_type


TYPE :: imgn_vars_type
  ! imogen variables.
  REAL, POINTER :: dtemp_g(:)
  REAL, POINTER :: c_emiss_data(:)
  REAL, POINTER :: dctot_co2(:)
  REAL, POINTER :: dctot_ch4(:)
  REAL, POINTER :: dtemp_o(:)
  REAL, POINTER :: fa_ocean(:)
  REAL, POINTER :: d_land_atmos_co2(:)
  REAL, POINTER :: d_land_atmos_ch4(:)
  REAL, POINTER :: d_ocean_atmos(:)
  REAL, POINTER :: imogen_radf(:)
  REAL, POINTER :: q_non_co2(:)
  REAL, POINTER :: co2_mmr_prescribe(:)
  REAL, POINTER :: co2_start_ppmv(:)
  REAL, POINTER :: co2_ppmv(:)
  REAL, POINTER :: co2_change_ppmv(:)
  REAL, POINTER :: ch4_ppbv(:)

END TYPE imgn_vars_type

TYPE(imgn_vars_data_type), TARGET :: imgn_vars_data
TYPE(imgn_vars_type) :: imgn_vars

!-------------------------------------------------------------------------------

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='IMGN_VARS_MOD'

CONTAINS

SUBROUTINE imgn_vars_alloc(land_pts, imgn_vars_data)

USE imogen_constants, ONLY: n_olevs, nfarray

USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: land_pts


TYPE(imgn_vars_data_type), INTENT(IN OUT) :: imgn_vars_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='IMGN_VARS_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

ALLOCATE( imgn_vars_data%dtemp_g(land_pts))
ALLOCATE( imgn_vars_data%c_emiss_data(land_pts))
ALLOCATE( imgn_vars_data%dctot_co2(land_pts))
ALLOCATE( imgn_vars_data%dctot_ch4(land_pts))
ALLOCATE( imgn_vars_data%dtemp_o(n_olevs))
ALLOCATE( imgn_vars_data%fa_ocean(nfarray))
ALLOCATE( imgn_vars_data%d_land_atmos_co2(land_pts))
ALLOCATE( imgn_vars_data%d_land_atmos_ch4(land_pts))
ALLOCATE( imgn_vars_data%d_ocean_atmos(land_pts))
ALLOCATE( imgn_vars_data%imogen_radf(land_pts))
ALLOCATE( imgn_vars_data%q_non_co2(1))
ALLOCATE( imgn_vars_data%co2_mmr_prescribe(land_pts))
ALLOCATE( imgn_vars_data%co2_start_ppmv(1))
ALLOCATE( imgn_vars_data%co2_ppmv(1))
ALLOCATE( imgn_vars_data%co2_change_ppmv(1))
ALLOCATE( imgn_vars_data%ch4_ppbv(1))

imgn_vars_data%dtemp_g(:) = 0.0
imgn_vars_data%c_emiss_data(:) = 0.0
imgn_vars_data%dctot_co2(:) = 0.0
imgn_vars_data%dctot_ch4(:) = 0.0
imgn_vars_data%dtemp_o(:) = 0.0
imgn_vars_data%fa_ocean(:) = 0.0
imgn_vars_data%d_land_atmos_co2(:) = 0.0
imgn_vars_data%d_land_atmos_ch4(:) = 0.0
imgn_vars_data%d_ocean_atmos(:) = 0.0
imgn_vars_data%imogen_radf(:) = 0.0
imgn_vars_data%q_non_co2(:) = 0.0
imgn_vars_data%co2_mmr_prescribe(:) = rmdi
imgn_vars_data%co2_start_ppmv(:) = rmdi
imgn_vars_data%co2_ppmv(:) = rmdi
imgn_vars_data%co2_change_ppmv(:) = 0.0
imgn_vars_data%ch4_ppbv(:) = rmdi



IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE imgn_vars_alloc

!===============================================================================
SUBROUTINE imgn_vars_dealloc(imgn_vars_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

TYPE(imgn_vars_data_type), INTENT(IN OUT) :: imgn_vars_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='IMGN_VARS_DEALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)


DEALLOCATE( imgn_vars_data%dtemp_g)
DEALLOCATE( imgn_vars_data%c_emiss_data)
DEALLOCATE( imgn_vars_data%dctot_co2)
DEALLOCATE( imgn_vars_data%dctot_ch4)
DEALLOCATE( imgn_vars_data%dtemp_o)
DEALLOCATE( imgn_vars_data%fa_ocean)
DEALLOCATE( imgn_vars_data%d_land_atmos_co2)
DEALLOCATE( imgn_vars_data%d_land_atmos_ch4)
DEALLOCATE( imgn_vars_data%d_ocean_atmos)
DEALLOCATE( imgn_vars_data%imogen_radf)
DEALLOCATE( imgn_vars_data%q_non_co2)
DEALLOCATE( imgn_vars_data%co2_mmr_prescribe)
DEALLOCATE( imgn_vars_data%co2_start_ppmv)
DEALLOCATE( imgn_vars_data%co2_ppmv)
DEALLOCATE( imgn_vars_data%co2_change_ppmv)
DEALLOCATE( imgn_vars_data%ch4_ppbv)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE imgn_vars_dealloc

!===============================================================================
SUBROUTINE imgn_vars_assoc(imgn_vars, imgn_vars_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

TYPE(imgn_vars_type), INTENT(IN OUT) :: imgn_vars
TYPE(imgn_vars_data_type), INTENT(IN OUT), TARGET :: imgn_vars_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='IMGN_VARS_ASSOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL imgn_vars_nullify(imgn_vars)

imgn_vars%dtemp_g => imgn_vars_data%dtemp_g
imgn_vars%c_emiss_data => imgn_vars_data%c_emiss_data
imgn_vars%dctot_co2 => imgn_vars_data%dctot_co2
imgn_vars%dctot_ch4 => imgn_vars_data%dctot_ch4
imgn_vars%dtemp_o => imgn_vars_data%dtemp_o
imgn_vars%fa_ocean => imgn_vars_data%fa_ocean
imgn_vars%d_land_atmos_co2 => imgn_vars_data%d_land_atmos_co2
imgn_vars%d_land_atmos_ch4 => imgn_vars_data%d_land_atmos_ch4
imgn_vars%d_ocean_atmos => imgn_vars_data%d_ocean_atmos
imgn_vars%imogen_radf => imgn_vars_data%imogen_radf
imgn_vars%q_non_co2 => imgn_vars_data%q_non_co2
imgn_vars%co2_mmr_prescribe => imgn_vars_data%co2_mmr_prescribe
imgn_vars%co2_start_ppmv => imgn_vars_data%co2_start_ppmv
imgn_vars%co2_ppmv => imgn_vars_data%co2_ppmv
imgn_vars%co2_change_ppmv => imgn_vars_data%co2_change_ppmv
imgn_vars%ch4_ppbv => imgn_vars_data%ch4_ppbv


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE imgn_vars_assoc

!===============================================================================
SUBROUTINE imgn_vars_nullify(imgn_vars)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

TYPE(imgn_vars_type), INTENT(IN OUT) :: imgn_vars

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='IMGN_VARS_NULLIFY'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

NULLIFY( imgn_vars%dtemp_g)
NULLIFY( imgn_vars%c_emiss_data)
NULLIFY( imgn_vars%dctot_co2)
NULLIFY( imgn_vars%dctot_ch4)
NULLIFY( imgn_vars%dtemp_o)
NULLIFY( imgn_vars%fa_ocean)
NULLIFY( imgn_vars%d_land_atmos_co2)
NULLIFY( imgn_vars%d_land_atmos_ch4)
NULLIFY( imgn_vars%d_ocean_atmos)
NULLIFY( imgn_vars%imogen_radf)
NULLIFY( imgn_vars%q_non_co2)
NULLIFY( imgn_vars%co2_mmr_prescribe)
NULLIFY( imgn_vars%co2_start_ppmv)
NULLIFY( imgn_vars%co2_ppmv)
NULLIFY( imgn_vars%co2_change_ppmv)
NULLIFY( imgn_vars%ch4_ppbv)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE imgn_vars_nullify


END MODULE imgn_vars_mod
#endif
