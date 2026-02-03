! *****************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
! *****************************COPYRIGHT****************************************
!
! Module containing surface fluxes.
!
! Code Description:
!   Language: FORTRAN 90
!
! Code Owner: Please refer to ModuleLeaders.txt
!

MODULE fire_vars_mod


USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

! Implementation for field variables:
! Each variable is declared in both the 'data' TYPE and the 'pointer' type.
! Instances of these types are declared at at high level as required
! This is to facilitate advanced memory management features, which are generally
! not visible in the science code.
! Checklist for adding a new variable:
! -add to data_type
! -add to pointer_type
! -add to the allocate routine, passing in any new dimension sizes required
!  by argument (not via USE statement)
! -add to the deallocate routine
! -add to the assoc and nullify routines

TYPE :: fire_vars_data_type
      ! Cloud to ground lightning strikes - only required for standalone
  REAL(KIND=real_jlslsm), ALLOCATABLE :: flash_rate(:)
      ! The lightning flash rate (flashes/km2), Cloud to Ground
  REAL(KIND=real_jlslsm), ALLOCATABLE :: pop_den(:)
      ! The population density (ppl/km2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: wealth_index(:)
      ! The Human Develoment Index (1)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: wham_arable_ba(:)
      ! WHAM! crop residue burning (1)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: wham_pasture_ba(:)
      ! WHAM! pastoral burning (1)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: wham_other_man(:)
      ! WHAM! vegetation management burning (1)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: wham_other_ag(:)
      ! WHAM! other agricultural burning (1)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: wham_ignitions(:)
      ! WHAM! unmanaged fire counts (1)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: wham_escaped(:)
      ! Escaped prescribed fires (1)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: wham_suppression(:)
      ! WHAM! fire suppression intensity (1)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: road_density(:)
      ! Road density (m / km-2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: flammability_ft(:,:)
      ! PFT-specific flammability
  REAL(KIND=real_jlslsm), ALLOCATABLE :: burnt_area(:)
      ! Gridbox mean burnt area fraction (/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: burnt_area_ft(:,:)
      ! PFT burnt area fraction (/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: wham_ba(:)
      ! Gridbox mean BA from wham managed fires (/s) (1)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: wham_ba_ft(:,:)
      ! proportion of burnt area from managed fire (1)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: intensity_ft(:,:)
      ! PFT-specific fireline intensity
  REAL(KIND=real_jlslsm), ALLOCATABLE :: intensity_clim(:,:)
      ! PFT-specific impact of climate on fireline intensity
  REAL(KIND=real_jlslsm), ALLOCATABLE :: intensity_wham(:,:)
      ! PFT-specific impact of management on fireline intensity
  REAL(KIND=real_jlslsm), ALLOCATABLE :: emitted_carbon(:)
      ! Gridbox mean emitted carbon (kgC/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: emitted_carbon_ft(:,:)
      ! PFT emitted carbon (kgC/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: emitted_carbon_DPM(:)
      ! DPM emitted carbon (kgC/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: emitted_carbon_RPM(:)
      ! RPM emitted carbon (kgC/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_CO2(:)
      ! Gridox mean fire CO2 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_CO2_ft(:,:)
      ! PFT fire CO2 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_CO2_DPM(:)
      ! DPM fire CO2 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_CO2_RPM(:)
      ! RPM fire CO2 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_CO(:)
      ! Gridbox mean fire CO emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_CO_ft(:,:)
      ! PFT fire CO emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_CO_DPM(:)
      ! DPM fire CO emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_CO_RPM(:)
      ! RPM fire CO emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_CH4(:)
      ! Gridbox mean fire CH4 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_CH4_ft(:,:)
      ! PFT fire CH4 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_CH4_DPM(:)
      ! DPM fire CH4 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_CH4_RPM(:)
      ! RPM fire CH4 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_NOx(:)
      ! Gridbox mean fire NOx emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_NOx_ft(:,:)
      ! PFT fire NOx emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_NOx_DPM(:)
      ! DPM fire NOx emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_NOx_RPM(:)
      ! RPM fire NOx emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_SO2(:)
      ! Gridbox mean fire SO2 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_SO2_ft(:,:)
      ! PFT fire SO2 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_SO2_DPM(:)
      ! DPM fire SO2 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_SO2_RPM(:)
      ! RPM fire SO2 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_OC(:)
      ! Gridbox mean fire Organic Carbon emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_OC_ft(:,:)
      ! PFT fire OC emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_OC_DPM(:)
      ! DPM fire OC emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_OC_RPM(:)
      ! RPM fire OC emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_BC(:)
      ! Gridbox mean fire Black Carbon emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_BC_ft(:,:)
      ! PFT fire BC emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_BC_DPM(:)
      ! DPM fire BC emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_BC_RPM(:)
      ! RPM fire BC emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_C2H4(:)
      ! Gridbox mean fire C2H4 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_C2H4_ft(:,:)
      ! PFT fire C2H4 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_C2H4_DPM(:)
      ! DPM fire C2H4 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_C2H4_RPM(:)
      ! RPM fire C2H4 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_C2H6(:)
      ! Gridbox mean fire C2H6 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_C2H6_ft(:,:)
      ! PFT fire C2H6 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_C2H6_DPM(:)
      ! DPM fire C2H6 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_C2H6_RPM(:)
      ! RPM fire C2H6 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_C3H8(:)
      ! Gridbox mean fire C3H8 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_C3H8_ft(:,:)
      ! PFT fire C3H8 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_C3H8_DPM(:)
      ! DPM fire C3H8 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_C3H8_RPM(:)
      ! RPM fire C3H8 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_HCHO(:)
      ! Gridbox mean fire HCHO emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_HCHO_ft(:,:)
      ! PFT fire HCHO emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_HCHO_DPM(:)
      ! DPM fire HCHO emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_HCHO_RPM(:)
      ! RPM fire HCHO emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_MeCHO(:)
      ! Gridbox mean fire MeCHO emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_MeCHO_ft(:,:)
      ! PFT fire MeCHO emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_MeCHO_DPM(:)
      ! DPM fire MeCHO emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_MeCHO_RPM(:)
      ! RPM fire MeCHO emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_NH3(:)
      ! Gridbox mean fire NH3 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_NH3_ft(:,:)
      ! PFT fire NH3 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_NH3_DPM(:)
      ! DPM fire NH3 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_NH3_RPM(:)
      ! RPM fire NH3 emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_DMS(:)
      ! Gridbox mean fire DMS emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_DMS_ft(:,:)
      ! PFT fire DMS emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_DMS_DPM(:)
      ! DPM fire DMS emission (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fire_em_DMS_RPM(:)
      ! RPM fire DMS emission (kg/m2/s)
END TYPE fire_vars_data_type

TYPE :: fire_vars_type
  REAL(KIND=real_jlslsm), POINTER :: flash_rate(:)
  REAL(KIND=real_jlslsm), POINTER :: pop_den(:)
  REAL(KIND=real_jlslsm), POINTER :: wealth_index(:)
  REAL(KIND=real_jlslsm), POINTER :: wham_arable_ba(:)
  REAL(KIND=real_jlslsm), POINTER :: wham_pasture_ba(:)
  REAL(KIND=real_jlslsm), POINTER :: wham_other_man(:)
  REAL(KIND=real_jlslsm), POINTER :: wham_other_ag(:)
  REAL(KIND=real_jlslsm), POINTER :: wham_ignitions(:)
  REAL(KIND=real_jlslsm), POINTER :: wham_escaped(:)
  REAL(KIND=real_jlslsm), POINTER :: wham_suppression(:)
  REAL(KIND=real_jlslsm), POINTER :: road_density(:)
  REAL(KIND=real_jlslsm), POINTER :: flammability_ft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: burnt_area(:)
  REAL(KIND=real_jlslsm), POINTER :: burnt_area_ft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: wham_ba(:)
  REAL(KIND=real_jlslsm), POINTER :: wham_ba_ft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: intensity_ft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: intensity_clim(:,:)
  REAL(KIND=real_jlslsm), POINTER :: intensity_wham(:,:)
  REAL(KIND=real_jlslsm), POINTER :: emitted_carbon(:)
  REAL(KIND=real_jlslsm), POINTER :: emitted_carbon_ft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: emitted_carbon_DPM(:)
  REAL(KIND=real_jlslsm), POINTER :: emitted_carbon_RPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_CO2(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_CO2_ft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_CO2_DPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_CO2_RPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_CO(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_CO_ft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_CO_DPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_CO_RPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_CH4(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_CH4_ft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_CH4_DPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_CH4_RPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_NOx(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_NOx_ft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_NOx_DPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_NOx_RPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_SO2(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_SO2_ft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_SO2_DPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_SO2_RPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_OC(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_OC_ft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_OC_DPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_OC_RPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_BC(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_BC_ft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_BC_DPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_BC_RPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_C2H4(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_C2H4_ft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_C2H4_DPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_C2H4_RPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_C2H6(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_C2H6_ft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_C2H6_DPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_C2H6_RPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_C3H8(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_C3H8_ft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_C3H8_DPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_C3H8_RPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_HCHO(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_HCHO_ft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_HCHO_DPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_HCHO_RPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_MeCHO(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_MeCHO_ft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_MeCHO_DPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_MeCHO_RPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_NH3(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_NH3_ft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_NH3_DPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_NH3_RPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_DMS(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_DMS_ft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_DMS_DPM(:)
  REAL(KIND=real_jlslsm), POINTER :: fire_em_DMS_RPM(:)
END TYPE fire_vars_type

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='FIRE_VARS_MOD'

CONTAINS

!===============================================================================
SUBROUTINE fire_vars_alloc(land_pts,npft, fire_vars_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: land_pts,npft
TYPE(fire_vars_data_type), INTENT(IN OUT) :: fire_vars_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='FIRE_VARS_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------
! Allocate space for inferno diagnostic variables
!-----------------------------------------------------------------------
ALLOCATE(fire_vars_data%burnt_area(land_pts))
ALLOCATE(fire_vars_data%burnt_area_ft(land_pts,npft))
ALLOCATE(fire_vars_data%emitted_carbon(land_pts))
ALLOCATE(fire_vars_data%emitted_carbon_ft(land_pts,npft))
ALLOCATE(fire_vars_data%emitted_carbon_DPM(land_pts))
ALLOCATE(fire_vars_data%emitted_carbon_RPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_CO2(land_pts))
ALLOCATE(fire_vars_data%fire_em_CO2_ft(land_pts,npft))
ALLOCATE(fire_vars_data%fire_em_CO2_DPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_CO2_RPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_CO(land_pts))
ALLOCATE(fire_vars_data%fire_em_CO_ft(land_pts,npft))
ALLOCATE(fire_vars_data%fire_em_CO_DPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_CO_RPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_CH4(land_pts))
ALLOCATE(fire_vars_data%fire_em_CH4_ft(land_pts,npft))
ALLOCATE(fire_vars_data%fire_em_CH4_DPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_CH4_RPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_NOx(land_pts))
ALLOCATE(fire_vars_data%fire_em_NOx_ft(land_pts,npft))
ALLOCATE(fire_vars_data%fire_em_NOx_DPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_NOx_RPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_SO2(land_pts))
ALLOCATE(fire_vars_data%fire_em_SO2_ft(land_pts,npft))
ALLOCATE(fire_vars_data%fire_em_SO2_DPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_SO2_RPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_OC(land_pts))
ALLOCATE(fire_vars_data%fire_em_OC_ft(land_pts,npft))
ALLOCATE(fire_vars_data%fire_em_OC_DPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_OC_RPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_BC(land_pts))
ALLOCATE(fire_vars_data%fire_em_BC_ft(land_pts,npft))
ALLOCATE(fire_vars_data%fire_em_BC_DPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_BC_RPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_C2H4(land_pts))
ALLOCATE(fire_vars_data%fire_em_C2H4_ft(land_pts,npft))
ALLOCATE(fire_vars_data%fire_em_C2H4_DPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_C2H4_RPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_C2H6(land_pts))
ALLOCATE(fire_vars_data%fire_em_C2H6_ft(land_pts,npft))
ALLOCATE(fire_vars_data%fire_em_C2H6_DPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_C2H6_RPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_C3H8(land_pts))
ALLOCATE(fire_vars_data%fire_em_C3H8_ft(land_pts,npft))
ALLOCATE(fire_vars_data%fire_em_C3H8_DPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_C3H8_RPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_HCHO(land_pts))
ALLOCATE(fire_vars_data%fire_em_HCHO_ft(land_pts,npft))
ALLOCATE(fire_vars_data%fire_em_HCHO_DPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_HCHO_RPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_MeCHO(land_pts))
ALLOCATE(fire_vars_data%fire_em_MeCHO_ft(land_pts,npft))
ALLOCATE(fire_vars_data%fire_em_MeCHO_DPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_MeCHO_RPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_NH3(land_pts))
ALLOCATE(fire_vars_data%fire_em_NH3_ft(land_pts,npft))
ALLOCATE(fire_vars_data%fire_em_NH3_DPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_NH3_RPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_DMS(land_pts))
ALLOCATE(fire_vars_data%fire_em_DMS_ft(land_pts,npft))
ALLOCATE(fire_vars_data%fire_em_DMS_DPM(land_pts))
ALLOCATE(fire_vars_data%fire_em_DMS_RPM(land_pts))
ALLOCATE(fire_vars_data%pop_den(land_pts))
ALLOCATE(fire_vars_data%wealth_index(land_pts))
ALLOCATE(fire_vars_data%flash_rate(land_pts))
ALLOCATE(fire_vars_data%wham_arable_ba(land_pts))
ALLOCATE(fire_vars_data%wham_pasture_ba(land_pts))
ALLOCATE(fire_vars_data%wham_other_man(land_pts))
ALLOCATE(fire_vars_data%wham_other_ag(land_pts))
ALLOCATE(fire_vars_data%wham_ignitions(land_pts))
ALLOCATE(fire_vars_data%wham_escaped(land_pts))
ALLOCATE(fire_vars_data%wham_suppression(land_pts))
ALLOCATE(fire_vars_data%road_density(land_pts))
ALLOCATE(fire_vars_data%flammability_ft(land_pts,npft))
ALLOCATE(fire_vars_data%wham_ba(land_pts))
ALLOCATE(fire_vars_data%wham_ba_ft(land_pts,npft))
ALLOCATE(fire_vars_data%intensity_ft(land_pts,npft))
ALLOCATE(fire_vars_data%intensity_clim(land_pts,npft))
ALLOCATE(fire_vars_data%intensity_wham(land_pts,npft))

fire_vars_data%burnt_area(:)          = 0.0
fire_vars_data%burnt_area_ft(:,:)     = 0.0
fire_vars_data%emitted_carbon(:)      = 0.0
fire_vars_data%emitted_carbon_ft(:,:) = 0.0
fire_vars_data%emitted_carbon_DPM(:)  = 0.0
fire_vars_data%emitted_carbon_RPM(:)  = 0.0
fire_vars_data%fire_em_CO2(:)         = 0.0
fire_vars_data%fire_em_CO2_ft(:,:)    = 0.0
fire_vars_data%fire_em_CO2_DPM(:)     = 0.0
fire_vars_data%fire_em_CO2_RPM(:)     = 0.0
fire_vars_data%fire_em_CO(:)          = 0.0
fire_vars_data%fire_em_CO_ft(:,:)     = 0.0
fire_vars_data%fire_em_CO_DPM(:)      = 0.0
fire_vars_data%fire_em_CO_RPM(:)      = 0.0
fire_vars_data%fire_em_CH4(:)         = 0.0
fire_vars_data%fire_em_CH4_ft(:,:)    = 0.0
fire_vars_data%fire_em_CH4_DPM(:)     = 0.0
fire_vars_data%fire_em_CH4_RPM(:)     = 0.0
fire_vars_data%fire_em_NOx(:)         = 0.0
fire_vars_data%fire_em_NOx_ft(:,:)    = 0.0
fire_vars_data%fire_em_NOx_DPM(:)     = 0.0
fire_vars_data%fire_em_NOx_RPM(:)     = 0.0
fire_vars_data%fire_em_SO2(:)         = 0.0
fire_vars_data%fire_em_SO2_ft(:,:)    = 0.0
fire_vars_data%fire_em_SO2_DPM(:)     = 0.0
fire_vars_data%fire_em_SO2_RPM(:)     = 0.0
fire_vars_data%fire_em_OC(:)          = 0.0
fire_vars_data%fire_em_OC_ft(:,:)     = 0.0
fire_vars_data%fire_em_OC_DPM(:)      = 0.0
fire_vars_data%fire_em_OC_RPM(:)      = 0.0
fire_vars_data%fire_em_BC(:)          = 0.0
fire_vars_data%fire_em_BC_ft(:,:)     = 0.0
fire_vars_data%fire_em_BC_DPM(:)      = 0.0
fire_vars_data%fire_em_BC_RPM(:)      = 0.0
fire_vars_data%fire_em_C2H4(:)        = 0.0
fire_vars_data%fire_em_C2H4_ft(:,:)   = 0.0
fire_vars_data%fire_em_C2H4_DPM(:)    = 0.0
fire_vars_data%fire_em_C2H4_RPM(:)    = 0.0
fire_vars_data%fire_em_C2H6(:)        = 0.0
fire_vars_data%fire_em_C2H6_ft(:,:)   = 0.0
fire_vars_data%fire_em_C2H6_DPM(:)    = 0.0
fire_vars_data%fire_em_C2H6_RPM(:)    = 0.0
fire_vars_data%fire_em_C3H8(:)        = 0.0
fire_vars_data%fire_em_C3H8_ft(:,:)   = 0.0
fire_vars_data%fire_em_C3H8_DPM(:)    = 0.0
fire_vars_data%fire_em_C3H8_RPM(:)    = 0.0
fire_vars_data%fire_em_HCHO(:)        = 0.0
fire_vars_data%fire_em_HCHO_ft(:,:)   = 0.0
fire_vars_data%fire_em_HCHO_DPM(:)    = 0.0
fire_vars_data%fire_em_HCHO_RPM(:)    = 0.0
fire_vars_data%fire_em_MeCHO(:)       = 0.0
fire_vars_data%fire_em_MeCHO_ft(:,:)  = 0.0
fire_vars_data%fire_em_MeCHO_DPM(:)   = 0.0
fire_vars_data%fire_em_MeCHO_RPM(:)   = 0.0
fire_vars_data%fire_em_NH3(:)         = 0.0
fire_vars_data%fire_em_NH3_ft(:,:)    = 0.0
fire_vars_data%fire_em_NH3_DPM(:)     = 0.0
fire_vars_data%fire_em_NH3_RPM(:)     = 0.0
fire_vars_data%fire_em_DMS(:)         = 0.0
fire_vars_data%fire_em_DMS_ft(:,:)    = 0.0
fire_vars_data%fire_em_DMS_DPM(:)     = 0.0
fire_vars_data%fire_em_DMS_RPM(:)     = 0.0
fire_vars_data%pop_den(:)             = 0.0
fire_vars_data%wealth_index(:)        = 0.0
fire_vars_data%wham_arable_ba(:)      = 0.0
fire_vars_data%wham_pasture_ba(:)     = 0.0
fire_vars_data%wham_other_man(:)      = 0.0
fire_vars_data%wham_other_ag(:)       = 0.0
fire_vars_data%wham_ignitions(:)      = 0.0
fire_vars_data%wham_escaped(:)        = 0.0
fire_vars_data%wham_suppression(:)    = 0.0
fire_vars_data%road_density(:)        = 0.0
fire_vars_data%flash_rate(:)          = 0.0
fire_vars_data%flammability_ft(:,:)   = 0.0
fire_vars_data%wham_ba(:)             = 0.0
fire_vars_data%wham_ba_ft(:,:)        = 0.0
fire_vars_data%intensity_ft(:,:)      = 0.0
fire_vars_data%intensity_clim(:,:)    = 0.0
fire_vars_data%intensity_wham(:,:)    = 0.0

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE fire_vars_alloc


!===============================================================================
SUBROUTINE fire_vars_dealloc(fire_vars_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
TYPE(fire_vars_data_type), INTENT(IN OUT) :: fire_vars_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='FIRE_VARS_DEALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------
! Allocate space for inferno diagnostic variables
!-----------------------------------------------------------------------
DEALLOCATE(fire_vars_data%burnt_area)
DEALLOCATE(fire_vars_data%burnt_area_ft)
DEALLOCATE(fire_vars_data%emitted_carbon)
DEALLOCATE(fire_vars_data%emitted_carbon_ft)
DEALLOCATE(fire_vars_data%emitted_carbon_DPM)
DEALLOCATE(fire_vars_data%emitted_carbon_RPM)
DEALLOCATE(fire_vars_data%fire_em_CO2)
DEALLOCATE(fire_vars_data%fire_em_CO2_ft)
DEALLOCATE(fire_vars_data%fire_em_CO2_DPM)
DEALLOCATE(fire_vars_data%fire_em_CO2_RPM)
DEALLOCATE(fire_vars_data%fire_em_CO)
DEALLOCATE(fire_vars_data%fire_em_CO_ft)
DEALLOCATE(fire_vars_data%fire_em_CO_DPM)
DEALLOCATE(fire_vars_data%fire_em_CO_RPM)
DEALLOCATE(fire_vars_data%fire_em_CH4)
DEALLOCATE(fire_vars_data%fire_em_CH4_ft)
DEALLOCATE(fire_vars_data%fire_em_CH4_DPM)
DEALLOCATE(fire_vars_data%fire_em_CH4_RPM)
DEALLOCATE(fire_vars_data%fire_em_NOx)
DEALLOCATE(fire_vars_data%fire_em_NOx_ft)
DEALLOCATE(fire_vars_data%fire_em_NOx_DPM)
DEALLOCATE(fire_vars_data%fire_em_NOx_RPM)
DEALLOCATE(fire_vars_data%fire_em_SO2)
DEALLOCATE(fire_vars_data%fire_em_SO2_ft)
DEALLOCATE(fire_vars_data%fire_em_SO2_DPM)
DEALLOCATE(fire_vars_data%fire_em_SO2_RPM)
DEALLOCATE(fire_vars_data%fire_em_OC)
DEALLOCATE(fire_vars_data%fire_em_OC_ft)
DEALLOCATE(fire_vars_data%fire_em_OC_DPM)
DEALLOCATE(fire_vars_data%fire_em_OC_RPM)
DEALLOCATE(fire_vars_data%fire_em_BC)
DEALLOCATE(fire_vars_data%fire_em_BC_ft)
DEALLOCATE(fire_vars_data%fire_em_BC_DPM)
DEALLOCATE(fire_vars_data%fire_em_BC_RPM)
DEALLOCATE(fire_vars_data%fire_em_C2H4)
DEALLOCATE(fire_vars_data%fire_em_C2H4_ft)
DEALLOCATE(fire_vars_data%fire_em_C2H4_DPM)
DEALLOCATE(fire_vars_data%fire_em_C2H4_RPM)
DEALLOCATE(fire_vars_data%fire_em_C2H6)
DEALLOCATE(fire_vars_data%fire_em_C2H6_ft)
DEALLOCATE(fire_vars_data%fire_em_C2H6_DPM)
DEALLOCATE(fire_vars_data%fire_em_C2H6_RPM)
DEALLOCATE(fire_vars_data%fire_em_C3H8)
DEALLOCATE(fire_vars_data%fire_em_C3H8_ft)
DEALLOCATE(fire_vars_data%fire_em_C3H8_DPM)
DEALLOCATE(fire_vars_data%fire_em_C3H8_RPM)
DEALLOCATE(fire_vars_data%fire_em_HCHO)
DEALLOCATE(fire_vars_data%fire_em_HCHO_ft)
DEALLOCATE(fire_vars_data%fire_em_HCHO_DPM)
DEALLOCATE(fire_vars_data%fire_em_HCHO_RPM)
DEALLOCATE(fire_vars_data%fire_em_MeCHO)
DEALLOCATE(fire_vars_data%fire_em_MeCHO_ft)
DEALLOCATE(fire_vars_data%fire_em_MeCHO_DPM)
DEALLOCATE(fire_vars_data%fire_em_MeCHO_RPM)
DEALLOCATE(fire_vars_data%fire_em_NH3)
DEALLOCATE(fire_vars_data%fire_em_NH3_ft)
DEALLOCATE(fire_vars_data%fire_em_NH3_DPM)
DEALLOCATE(fire_vars_data%fire_em_NH3_RPM)
DEALLOCATE(fire_vars_data%fire_em_DMS)
DEALLOCATE(fire_vars_data%fire_em_DMS_ft)
DEALLOCATE(fire_vars_data%fire_em_DMS_DPM)
DEALLOCATE(fire_vars_data%fire_em_DMS_RPM)
DEALLOCATE(fire_vars_data%pop_den)
DEALLOCATE(fire_vars_data%wealth_index)
DEALLOCATE(fire_vars_data%wham_arable_ba)
DEALLOCATE(fire_vars_data%wham_pasture_ba)
DEALLOCATE(fire_vars_data%wham_other_man)
DEALLOCATE(fire_vars_data%wham_other_ag)
DEALLOCATE(fire_vars_data%wham_ignitions)
DEALLOCATE(fire_vars_data%wham_escaped)
DEALLOCATE(fire_vars_data%wham_suppression)
DEALLOCATE(fire_vars_data%road_density)
DEALLOCATE(fire_vars_data%flash_rate)
DEALLOCATE(fire_vars_data%flammability_ft)
DEALLOCATE(fire_vars_data%wham_ba)
DEALLOCATE(fire_vars_data%wham_ba_ft)
DEALLOCATE(fire_vars_data%intensity_ft)
DEALLOCATE(fire_vars_data%intensity_clim)
DEALLOCATE(fire_vars_data%intensity_wham)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE fire_vars_dealloc

!===============================================================================
SUBROUTINE fire_vars_assoc(fire_vars, fire_vars_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
TYPE(fire_vars_type), INTENT(IN OUT) :: fire_vars
TYPE(fire_vars_data_type), INTENT(IN OUT), TARGET :: fire_vars_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='FIRE_VARS_ASSOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL fire_vars_nullify(fire_vars)

fire_vars%burnt_area => fire_vars_data%burnt_area
fire_vars%burnt_area_ft => fire_vars_data%burnt_area_ft
fire_vars%emitted_carbon => fire_vars_data%emitted_carbon
fire_vars%emitted_carbon_ft => fire_vars_data%emitted_carbon_ft
fire_vars%emitted_carbon_DPM => fire_vars_data%emitted_carbon_DPM
fire_vars%emitted_carbon_RPM => fire_vars_data%emitted_carbon_RPM
fire_vars%fire_em_CO2 => fire_vars_data%fire_em_CO2
fire_vars%fire_em_CO2_ft => fire_vars_data%fire_em_CO2_ft
fire_vars%fire_em_CO2_DPM => fire_vars_data%fire_em_CO2_DPM
fire_vars%fire_em_CO2_RPM => fire_vars_data%fire_em_CO2_RPM
fire_vars%fire_em_CO => fire_vars_data%fire_em_CO
fire_vars%fire_em_CO_ft => fire_vars_data%fire_em_CO_ft
fire_vars%fire_em_CO_DPM => fire_vars_data%fire_em_CO_DPM
fire_vars%fire_em_CO_RPM => fire_vars_data%fire_em_CO_RPM
fire_vars%fire_em_CH4 => fire_vars_data%fire_em_CH4
fire_vars%fire_em_CH4_ft => fire_vars_data%fire_em_CH4_ft
fire_vars%fire_em_CH4_DPM => fire_vars_data%fire_em_CH4_DPM
fire_vars%fire_em_CH4_RPM => fire_vars_data%fire_em_CH4_RPM
fire_vars%fire_em_NOx => fire_vars_data%fire_em_NOx
fire_vars%fire_em_NOx_ft => fire_vars_data%fire_em_NOx_ft
fire_vars%fire_em_NOx_DPM => fire_vars_data%fire_em_NOx_DPM
fire_vars%fire_em_NOx_RPM => fire_vars_data%fire_em_NOx_RPM
fire_vars%fire_em_SO2 => fire_vars_data%fire_em_SO2
fire_vars%fire_em_SO2_ft => fire_vars_data%fire_em_SO2_ft
fire_vars%fire_em_SO2_DPM => fire_vars_data%fire_em_SO2_DPM
fire_vars%fire_em_SO2_RPM => fire_vars_data%fire_em_SO2_RPM
fire_vars%fire_em_OC => fire_vars_data%fire_em_OC
fire_vars%fire_em_OC_ft => fire_vars_data%fire_em_OC_ft
fire_vars%fire_em_OC_DPM => fire_vars_data%fire_em_OC_DPM
fire_vars%fire_em_OC_RPM => fire_vars_data%fire_em_OC_RPM
fire_vars%fire_em_BC => fire_vars_data%fire_em_BC
fire_vars%fire_em_BC_ft => fire_vars_data%fire_em_BC_ft
fire_vars%fire_em_BC_DPM => fire_vars_data%fire_em_BC_DPM
fire_vars%fire_em_BC_RPM => fire_vars_data%fire_em_BC_RPM
fire_vars%fire_em_C2H4 => fire_vars_data%fire_em_C2H4
fire_vars%fire_em_C2H4_ft => fire_vars_data%fire_em_C2H4_ft
fire_vars%fire_em_C2H4_DPM => fire_vars_data%fire_em_C2H4_DPM
fire_vars%fire_em_C2H4_RPM => fire_vars_data%fire_em_C2H4_RPM
fire_vars%fire_em_C2H6 => fire_vars_data%fire_em_C2H6
fire_vars%fire_em_C2H6_ft => fire_vars_data%fire_em_C2H6_ft
fire_vars%fire_em_C2H6_DPM => fire_vars_data%fire_em_C2H6_DPM
fire_vars%fire_em_C2H6_RPM => fire_vars_data%fire_em_C2H6_RPM
fire_vars%fire_em_C3H8 => fire_vars_data%fire_em_C3H8
fire_vars%fire_em_C3H8_ft => fire_vars_data%fire_em_C3H8_ft
fire_vars%fire_em_C3H8_DPM => fire_vars_data%fire_em_C3H8_DPM
fire_vars%fire_em_C3H8_RPM => fire_vars_data%fire_em_C3H8_RPM
fire_vars%fire_em_HCHO => fire_vars_data%fire_em_HCHO
fire_vars%fire_em_HCHO_ft => fire_vars_data%fire_em_HCHO_ft
fire_vars%fire_em_HCHO_DPM => fire_vars_data%fire_em_HCHO_DPM
fire_vars%fire_em_HCHO_RPM => fire_vars_data%fire_em_HCHO_RPM
fire_vars%fire_em_MeCHO => fire_vars_data%fire_em_MeCHO
fire_vars%fire_em_MeCHO_ft => fire_vars_data%fire_em_MeCHO_ft
fire_vars%fire_em_MeCHO_DPM => fire_vars_data%fire_em_MeCHO_DPM
fire_vars%fire_em_MeCHO_RPM => fire_vars_data%fire_em_MeCHO_RPM
fire_vars%fire_em_NH3 => fire_vars_data%fire_em_NH3
fire_vars%fire_em_NH3_ft => fire_vars_data%fire_em_NH3_ft
fire_vars%fire_em_NH3_DPM => fire_vars_data%fire_em_NH3_DPM
fire_vars%fire_em_NH3_RPM => fire_vars_data%fire_em_NH3_RPM
fire_vars%fire_em_DMS => fire_vars_data%fire_em_DMS
fire_vars%fire_em_DMS_ft => fire_vars_data%fire_em_DMS_ft
fire_vars%fire_em_DMS_DPM => fire_vars_data%fire_em_DMS_DPM
fire_vars%fire_em_DMS_RPM => fire_vars_data%fire_em_DMS_RPM
fire_vars%pop_den => fire_vars_data%pop_den
fire_vars%wealth_index => fire_vars_data%wealth_index
fire_vars%wham_arable_ba => fire_vars_data%wham_arable_ba
fire_vars%wham_pasture_ba => fire_vars_data%wham_pasture_ba
fire_vars%wham_other_man => fire_vars_data%wham_other_man
fire_vars%wham_other_ag => fire_vars_data%wham_other_ag
fire_vars%wham_ignitions => fire_vars_data%wham_ignitions
fire_vars%wham_escaped => fire_vars_data%wham_escaped
fire_vars%wham_suppression => fire_vars_data%wham_suppression
fire_vars%road_density => fire_vars_data%road_density
fire_vars%flash_rate => fire_vars_data%flash_rate
fire_vars%flammability_ft => fire_vars_data%flammability_ft
fire_vars%wham_ba => fire_vars_data%wham_ba
fire_vars%wham_ba_ft => fire_vars_data%wham_ba_ft
fire_vars%intensity_ft => fire_vars_data%intensity_ft
fire_vars%intensity_clim => fire_vars_data%intensity_clim
fire_vars%intensity_wham => fire_vars_data%intensity_wham

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE fire_vars_assoc

!===============================================================================
SUBROUTINE fire_vars_nullify(fire_vars)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
TYPE(fire_vars_type), INTENT(IN OUT) :: fire_vars

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='FIRE_VARS_NULLIFY'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

NULLIFY(fire_vars%burnt_area)
NULLIFY(fire_vars%burnt_area_ft)
NULLIFY(fire_vars%emitted_carbon)
NULLIFY(fire_vars%emitted_carbon_ft)
NULLIFY(fire_vars%emitted_carbon_DPM)
NULLIFY(fire_vars%emitted_carbon_RPM)
NULLIFY(fire_vars%fire_em_CO2)
NULLIFY(fire_vars%fire_em_CO2_ft)
NULLIFY(fire_vars%fire_em_CO2_DPM)
NULLIFY(fire_vars%fire_em_CO2_RPM)
NULLIFY(fire_vars%fire_em_CO)
NULLIFY(fire_vars%fire_em_CO_ft)
NULLIFY(fire_vars%fire_em_CO_DPM)
NULLIFY(fire_vars%fire_em_CO_RPM)
NULLIFY(fire_vars%fire_em_CH4)
NULLIFY(fire_vars%fire_em_CH4_ft)
NULLIFY(fire_vars%fire_em_CH4_DPM)
NULLIFY(fire_vars%fire_em_CH4_RPM)
NULLIFY(fire_vars%fire_em_NOx)
NULLIFY(fire_vars%fire_em_NOx_ft)
NULLIFY(fire_vars%fire_em_NOx_DPM)
NULLIFY(fire_vars%fire_em_NOx_RPM)
NULLIFY(fire_vars%fire_em_SO2)
NULLIFY(fire_vars%fire_em_SO2_ft)
NULLIFY(fire_vars%fire_em_SO2_DPM)
NULLIFY(fire_vars%fire_em_SO2_RPM)
NULLIFY(fire_vars%fire_em_OC)
NULLIFY(fire_vars%fire_em_OC_ft)
NULLIFY(fire_vars%fire_em_OC_DPM)
NULLIFY(fire_vars%fire_em_OC_RPM)
NULLIFY(fire_vars%fire_em_BC)
NULLIFY(fire_vars%fire_em_BC_ft)
NULLIFY(fire_vars%fire_em_BC_DPM)
NULLIFY(fire_vars%fire_em_BC_RPM)
NULLIFY(fire_vars%fire_em_C2H4)
NULLIFY(fire_vars%fire_em_C2H4_ft)
NULLIFY(fire_vars%fire_em_C2H4_DPM)
NULLIFY(fire_vars%fire_em_C2H4_RPM)
NULLIFY(fire_vars%fire_em_C2H6)
NULLIFY(fire_vars%fire_em_C2H6_ft)
NULLIFY(fire_vars%fire_em_C2H6_DPM)
NULLIFY(fire_vars%fire_em_C2H6_RPM)
NULLIFY(fire_vars%fire_em_C3H8)
NULLIFY(fire_vars%fire_em_C3H8_ft)
NULLIFY(fire_vars%fire_em_C3H8_DPM)
NULLIFY(fire_vars%fire_em_C3H8_RPM)
NULLIFY(fire_vars%fire_em_HCHO)
NULLIFY(fire_vars%fire_em_HCHO_ft)
NULLIFY(fire_vars%fire_em_HCHO_DPM)
NULLIFY(fire_vars%fire_em_HCHO_RPM)
NULLIFY(fire_vars%fire_em_MeCHO)
NULLIFY(fire_vars%fire_em_MeCHO_ft)
NULLIFY(fire_vars%fire_em_MeCHO_DPM)
NULLIFY(fire_vars%fire_em_MeCHO_RPM)
NULLIFY(fire_vars%fire_em_NH3)
NULLIFY(fire_vars%fire_em_NH3_ft)
NULLIFY(fire_vars%fire_em_NH3_DPM)
NULLIFY(fire_vars%fire_em_NH3_RPM)
NULLIFY(fire_vars%fire_em_DMS)
NULLIFY(fire_vars%fire_em_DMS_ft)
NULLIFY(fire_vars%fire_em_DMS_DPM)
NULLIFY(fire_vars%fire_em_DMS_RPM)
NULLIFY(fire_vars%pop_den)
NULLIFY(fire_vars%wealth_index)
NULLIFY(fire_vars%wham_arable_ba)
NULLIFY(fire_vars%wham_pasture_ba)
NULLIFY(fire_vars%wham_other_man)
NULLIFY(fire_vars%wham_other_ag)
NULLIFY(fire_vars%wham_ignitions)
NULLIFY(fire_vars%wham_escaped)
NULLIFY(fire_vars%wham_suppression)
NULLIFY(fire_vars%road_density)
NULLIFY(fire_vars%flash_rate)
NULLIFY(fire_vars%flammability_ft)
NULLIFY(fire_vars%wham_ba)
NULLIFY(fire_vars%wham_ba_ft)
NULLIFY(fire_vars%intensity_ft)
NULLIFY(fire_vars%intensity_clim)
NULLIFY(fire_vars%intensity_wham)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE fire_vars_nullify

END MODULE fire_vars_mod
