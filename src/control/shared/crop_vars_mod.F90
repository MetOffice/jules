! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE crop_vars_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Module holding various variables for the crop model
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

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


! Constants
INTEGER, PARAMETER ::                                                          &
  ndpy = 365,                                                                  &
      ! No. of days per year
  nyav = 3,                                                                    &
      ! No. of years averaged for crop plant estimates
  nday_crop = 150
      ! Cropping date

INTEGER :: iyear_old, startyr, startmon, startday, starttime
      ! Variables to store required datetime information

TYPE :: crop_vars_data_type
  !-----------------------------------------------------------------------------
  ! Variables
  !-----------------------------------------------------------------------------
  REAL(KIND=real_jlslsm), ALLOCATABLE ::         sow_date_cpft(:,:)
          ! Sowing date of each crop functional type
  REAL(KIND=real_jlslsm), ALLOCATABLE ::         latestharv_date_cpft(:,:)
          ! Sowing date of each crop functional type
  REAL(KIND=real_jlslsm), ALLOCATABLE ::         tt_veg_cpft(:,:)
          ! Thermal requirement of stage 1 for crop pfts (degree days).
  REAL(KIND=real_jlslsm), ALLOCATABLE ::         tt_rep_cpft(:,:)
          ! Thermal requirement of stage 2 for crop pfts (degree days).
  REAL(KIND=real_jlslsm), ALLOCATABLE ::         phot(:)
          !  Photoperiod (hours) for crop model
  REAL(KIND=real_jlslsm), ALLOCATABLE ::         dphotdt(:)
          !  Rate of change of phot for crops (hours per day).
  !-----------------------------------------------------------------------------
  ! Prognostics
  !-----------------------------------------------------------------------------
  REAL(KIND=real_jlslsm), ALLOCATABLE ::         dvi_cpft(:,:)
          !  Development index for crop tiles
  REAL(KIND=real_jlslsm), ALLOCATABLE ::         rootc_cpft(:,:)
          !  Root carbon pool for crop tiles (kg m-2).
  REAL(KIND=real_jlslsm), ALLOCATABLE ::         harvc_cpft(:,:)
          !  Carbon in 'harvest parts' pool for crop tiles (kg m-2).
  REAL(KIND=real_jlslsm), ALLOCATABLE ::         reservec_cpft(:,:)
          !  Carbon in stem reserves pool for crop tiles (kg m-2).
  REAL(KIND=real_jlslsm), ALLOCATABLE ::         croplai_cpft(:,:)
          !  Leaf area index for crop tiles
  REAL(KIND=real_jlslsm), ALLOCATABLE ::         cropcanht_cpft(:,:)
          !  Canopy height for crop tiles (m).
  REAL(KIND=real_jlslsm), ALLOCATABLE ::         sthu_irr_soilt(:,:,:)
          !  Unfrozen soil wetness over irrigation
  !-----------------------------------------------------------------------------
  ! Diagnostics
  !-----------------------------------------------------------------------------
  REAL(KIND=real_jlslsm), ALLOCATABLE ::         yield_diag_cpft(:,:)
          ! Harvested carbon (kg m-2).
  REAL(KIND=real_jlslsm), ALLOCATABLE ::         stemc_diag_cpft(:,:)
          ! Stem carbon (kg m-2).
  REAL(KIND=real_jlslsm), ALLOCATABLE ::         leafc_diag_cpft(:,:)
          ! Leaf carbon (kg m-2).
  REAL(KIND=real_jlslsm), ALLOCATABLE ::         irrig_water_gb(:)
          ! Addition of irrigation water to soil (kg/m2/s).
  REAL(KIND=real_jlslsm), ALLOCATABLE ::         nonyield_diag_cpft(:,:)
          ! Carbon leaving the crop model which is not yield (kg m-2).

  INTEGER, ALLOCATABLE ::  harvest_trigger_cpft(:,:)
          ! Trigger condition for the harvest in this timestep
  INTEGER, ALLOCATABLE ::  harvest_counter_cpft(:,:)
          ! 1 if timestep contains a harvest, 0 otherwise

  !-----------------------------------------------------------------------------
  ! Irrigation variables
  !-----------------------------------------------------------------------------
  REAL(KIND=real_jlslsm), ALLOCATABLE ::       frac_irr_all(:,:)
  REAL(KIND=real_jlslsm), ALLOCATABLE ::       frac_irr_soilt(:,:)
          ! Irrigation fraction for each soil tile
  REAL(KIND=real_jlslsm), ALLOCATABLE ::       frac_irr_old_soilt(:,:)
          ! Previous irrigated fraction in grid box
  REAL(KIND=real_jlslsm), ALLOCATABLE ::       irrfrac_irrtiles(:,:)
          ! irrigated fraction on irrigated tiles.

  INTEGER ,ALLOCATABLE ::  plant_n_gb(:)
          ! Best planting date for non-rice crops.
  INTEGER ,ALLOCATABLE ::  icntmax_gb(:)
          ! counter for start date for non-rice

          ! Variables for calculating average temperature, precip and radiation
  REAL(KIND=real_jlslsm), ALLOCATABLE ::  tl_1_day_av_gb(:)
          ! Average air temperature for the current day (K).
  REAL(KIND=real_jlslsm), ALLOCATABLE ::  tl_1_day_av_use_gb(:,:,:)
          ! Daily average air temperature (K).
  REAL(KIND=real_jlslsm), ALLOCATABLE ::  prec_1_day_av_gb(:)
          ! Average precipitation rate for the current day (kg m-2 s-1).
  REAL(KIND=real_jlslsm), ALLOCATABLE ::  prec_1_day_av_use_gb(:,:,:)
          ! Daily average precipitation rate (kg m-2 s-1).
  REAL(KIND=real_jlslsm), ALLOCATABLE ::  rn_1_day_av_gb(:)
          ! Average net radiation for the current day (W m-2).
  REAL(KIND=real_jlslsm), ALLOCATABLE ::  rn_1_day_av_use_gb(:,:,:)
          ! Daily average net radiation (W m-2).

  REAL(KIND=real_jlslsm), ALLOCATABLE ::  frac_irr_surft(:,:)
          ! Irrigation fraction in tile
  REAL(KIND=real_jlslsm), ALLOCATABLE ::  smc_irr_soilt(:,:)
          ! Available moisture in the soil profile in irrig frac (mm)
  REAL(KIND=real_jlslsm), ALLOCATABLE ::  wt_ext_irr_surft(:,:,:)
          ! Fraction of transpiration over irrigated fraction which is
          ! extracted from each soil layer by each tile
  REAL(KIND=real_jlslsm), ALLOCATABLE ::  gs_irr_surft(:,:)
          ! Conductance for irrigated surface types (m s-1).
  REAL(KIND=real_jlslsm), ALLOCATABLE ::  gc_irr_surft(:,:)
          ! Interactive canopy conductance (m s-1).
  REAL(KIND=real_jlslsm), ALLOCATABLE ::  resfs_irr_surft(:,:)
          ! Combined soil, stomatal and aerodynamic resistance factor for
          ! fraction 1-fracaero
  REAL(KIND=real_jlslsm), ALLOCATABLE ::  ext_irr_soilt(:,:,:)
          ! Extraction of water from each soil layer over irrigation
          ! (kg m-2 s-1).
  REAL(KIND=real_jlslsm), ALLOCATABLE ::  wt_ext_irr_gb(:,:)
          ! Fraction of transpiration extracted from each soil layer
          ! over irrigated fraction.
  REAL(KIND=real_jlslsm), ALLOCATABLE ::  fsmc_irr_gb(:)
          ! Soil moisture availability factor over irrigated fraction
  REAL(KIND=real_jlslsm), ALLOCATABLE ::  irrDaysDiag_gb(:)
          ! Number of days on which irrigation is applied

END TYPE crop_vars_data_type

TYPE :: crop_vars_type
  !-----------------------------------------------------------------------------
  ! Variables
  !-----------------------------------------------------------------------------
  REAL(KIND=real_jlslsm), POINTER ::         sow_date_cpft(:,:)
  REAL(KIND=real_jlslsm), POINTER ::         latestharv_date_cpft(:,:)
  REAL(KIND=real_jlslsm), POINTER ::         tt_veg_cpft(:,:)
  REAL(KIND=real_jlslsm), POINTER ::         tt_rep_cpft(:,:)
  REAL(KIND=real_jlslsm), POINTER ::         phot(:)
  REAL(KIND=real_jlslsm), POINTER ::         dphotdt(:)
  !-----------------------------------------------------------------------------
  ! Prognostics
  !-----------------------------------------------------------------------------
  REAL(KIND=real_jlslsm), POINTER ::         dvi_cpft(:,:)
  REAL(KIND=real_jlslsm), POINTER ::         rootc_cpft(:,:)
  REAL(KIND=real_jlslsm), POINTER ::         harvc_cpft(:,:)
  REAL(KIND=real_jlslsm), POINTER ::         reservec_cpft(:,:)
  REAL(KIND=real_jlslsm), POINTER ::         croplai_cpft(:,:)
  REAL(KIND=real_jlslsm), POINTER ::         cropcanht_cpft(:,:)
  REAL(KIND=real_jlslsm), POINTER ::         sthu_irr_soilt(:,:,:)
  !-----------------------------------------------------------------------------
  ! Diagnostics
  !-----------------------------------------------------------------------------
  REAL(KIND=real_jlslsm), POINTER ::         yield_diag_cpft(:,:)
  REAL(KIND=real_jlslsm), POINTER ::         stemc_diag_cpft(:,:)
  REAL(KIND=real_jlslsm), POINTER ::         leafc_diag_cpft(:,:)
  REAL(KIND=real_jlslsm), POINTER ::         irrig_water_gb(:)
  REAL(KIND=real_jlslsm), POINTER ::         nonyield_diag_cpft(:,:)

  INTEGER, POINTER ::                        harvest_trigger_cpft(:,:)
  INTEGER, POINTER ::                        harvest_counter_cpft(:,:)

  !-----------------------------------------------------------------------------
  ! Irrigation variables
  !-----------------------------------------------------------------------------
  REAL(KIND=real_jlslsm), POINTER ::         frac_irr_all(:,:)
  REAL(KIND=real_jlslsm), POINTER ::         frac_irr_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER ::         frac_irr_old_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER ::         irrfrac_irrtiles(:,:)

  INTEGER ,POINTER ::                        plant_n_gb(:)
  INTEGER ,POINTER ::                        icntmax_gb(:)

  REAL(KIND=real_jlslsm), POINTER ::         tl_1_day_av_gb(:)
  REAL(KIND=real_jlslsm), POINTER ::         tl_1_day_av_use_gb(:,:,:)
  REAL(KIND=real_jlslsm), POINTER ::         prec_1_day_av_gb(:)
  REAL(KIND=real_jlslsm), POINTER ::         prec_1_day_av_use_gb(:,:,:)
  REAL(KIND=real_jlslsm), POINTER ::         rn_1_day_av_gb(:)
  REAL(KIND=real_jlslsm), POINTER ::         rn_1_day_av_use_gb(:,:,:)

  REAL(KIND=real_jlslsm), POINTER ::         frac_irr_surft(:,:)
  REAL(KIND=real_jlslsm), POINTER ::         smc_irr_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER ::         wt_ext_irr_surft(:,:,:)
  REAL(KIND=real_jlslsm), POINTER ::         gs_irr_surft(:,:)
  REAL(KIND=real_jlslsm), POINTER ::         gc_irr_surft(:,:)
  REAL(KIND=real_jlslsm), POINTER ::         resfs_irr_surft(:,:)
  REAL(KIND=real_jlslsm), POINTER ::         ext_irr_soilt(:,:,:)
  REAL(KIND=real_jlslsm), POINTER ::         wt_ext_irr_gb(:,:)
  REAL(KIND=real_jlslsm), POINTER ::         fsmc_irr_gb(:)
  REAL(KIND=real_jlslsm), POINTER ::         irrDaysDiag_gb(:)

END TYPE crop_vars_type


CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='CROP_VARS_MOD'

CONTAINS

!===============================================================================
SUBROUTINE crop_vars_alloc(land_pts, t_i_length, t_j_length,                   &
                     nsurft,ncpft,nsoilt,sm_levels, l_crop, irr_crop,          &
                     irr_crop_doell, crop_vars_data )

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: land_pts, t_i_length, t_j_length,                       &
                       nsurft,ncpft,nsoilt,sm_levels

LOGICAL, INTENT(IN) :: l_crop
INTEGER, INTENT(IN) :: irr_crop, irr_crop_doell

TYPE(crop_vars_data_type), INTENT(IN OUT) :: crop_vars_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CROP_VARS_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!  ====crop_vars_mod module common====
! Irrigation variables
ALLOCATE(crop_vars_data%sthu_irr_soilt(land_pts,nsoilt,sm_levels))
ALLOCATE(crop_vars_data%frac_irr_all(land_pts,1))
ALLOCATE(crop_vars_data%irrfrac_irrtiles(land_pts,1))
ALLOCATE(crop_vars_data%frac_irr_soilt(land_pts,nsoilt))
ALLOCATE(crop_vars_data%frac_irr_old_soilt(land_pts,nsoilt))
ALLOCATE(crop_vars_data%frac_irr_surft(land_pts,nsurft))
ALLOCATE(crop_vars_data%plant_n_gb(land_pts))
ALLOCATE(crop_vars_data%smc_irr_soilt(land_pts,nsoilt))
ALLOCATE(crop_vars_data%wt_ext_irr_surft(land_pts,sm_levels,nsurft))
ALLOCATE(crop_vars_data%gs_irr_surft(land_pts,nsurft))
ALLOCATE(crop_vars_data%gc_irr_surft(land_pts,nsurft))
ALLOCATE(crop_vars_data%resfs_irr_surft(land_pts,nsurft))
ALLOCATE(crop_vars_data%ext_irr_soilt(land_pts,nsoilt,sm_levels))
ALLOCATE(crop_vars_data%wt_ext_irr_gb(land_pts,sm_levels))
ALLOCATE(crop_vars_data%fsmc_irr_gb(land_pts))
ALLOCATE(crop_vars_data%irrDaysDiag_gb(land_pts))
ALLOCATE(crop_vars_data%irrig_water_gb(land_pts))
crop_vars_data%sthu_irr_soilt(:,:,:)   = 0.0
crop_vars_data%frac_irr_all(:,:)       = 0.0
crop_vars_data%frac_irr_soilt(:,:)     = 0.0
crop_vars_data%frac_irr_old_soilt(:,:) = 0.0
crop_vars_data%frac_irr_surft(:,:)     = 0.0
crop_vars_data%plant_n_gb(:)           = 0
crop_vars_data%smc_irr_soilt(:,:)      = 0.0
crop_vars_data%wt_ext_irr_surft(:,:,:) = 0.0
crop_vars_data%gs_irr_surft(:,:)       = 0.0
crop_vars_data%gc_irr_surft(:,:)       = 0.0
crop_vars_data%resfs_irr_surft(:,:)    = 0.0
crop_vars_data%ext_irr_soilt(:,:,:)    = 0.0
crop_vars_data%wt_ext_irr_gb(:,:)      = 0.0
crop_vars_data%fsmc_irr_gb(:)          = 0.0
crop_vars_data%irrDaysDiag_gb(:)       = 0.0
crop_vars_data%irrig_water_gb(:)       = 0.0
crop_vars_data%irrfrac_irrtiles(:,:)   = 0.0

! vars for Doell and Siebert crop calendar
IF ( irr_crop == irr_crop_doell ) THEN
  ALLOCATE(crop_vars_data%icntmax_gb(land_pts))
  ALLOCATE(crop_vars_data%tl_1_day_av_gb(land_pts))
  ALLOCATE(crop_vars_data%tl_1_day_av_use_gb(land_pts,ndpy,nyav))
  ALLOCATE(crop_vars_data%prec_1_day_av_gb(land_pts))
  ALLOCATE(crop_vars_data%prec_1_day_av_use_gb(land_pts,ndpy,nyav))
  ALLOCATE(crop_vars_data%rn_1_day_av_gb(land_pts))
  ALLOCATE(crop_vars_data%rn_1_day_av_use_gb(land_pts,ndpy,nyav))

  crop_vars_data%icntmax_gb(:)                = 0
  crop_vars_data%tl_1_day_av_gb(:)            = 0.0
  crop_vars_data%tl_1_day_av_use_gb(:,:,:)    = 0.0
  crop_vars_data%prec_1_day_av_gb(:)          = 0.0
  crop_vars_data%prec_1_day_av_use_gb(:,:,:)  = 0.0
  crop_vars_data%rn_1_day_av_gb(:)            = 0.0
  crop_vars_data%rn_1_day_av_use_gb(:,:,:)    = 0.0
ELSE
  ALLOCATE(crop_vars_data%icntmax_gb(1))
  ALLOCATE(crop_vars_data%tl_1_day_av_gb(1))
  ALLOCATE(crop_vars_data%tl_1_day_av_use_gb(1,1,1))
  ALLOCATE(crop_vars_data%prec_1_day_av_gb(1))
  ALLOCATE(crop_vars_data%prec_1_day_av_use_gb(1,1,1))
  ALLOCATE(crop_vars_data%rn_1_day_av_gb(1))
  ALLOCATE(crop_vars_data%rn_1_day_av_use_gb(1,1,1))
END IF

ALLOCATE(crop_vars_data%dvi_cpft(land_pts,ncpft))
ALLOCATE(crop_vars_data%rootc_cpft(land_pts,ncpft))
ALLOCATE(crop_vars_data%phot(t_i_length * t_j_length))
ALLOCATE(crop_vars_data%dphotdt(t_i_length * t_j_length))

crop_vars_data%dvi_cpft(:,:)   = 0.0
crop_vars_data%rootc_cpft(:,:) = 0.0
crop_vars_data%phot(:)         = 0.0
crop_vars_data%dphotdt(:)      = 0.0

!  Done together as they share the l_crop IF
IF ( l_crop ) THEN
  ALLOCATE(crop_vars_data%harvc_cpft(land_pts,ncpft))
  ALLOCATE(crop_vars_data%reservec_cpft(land_pts,ncpft))
  ALLOCATE(crop_vars_data%yield_diag_cpft(land_pts,ncpft))
  ALLOCATE(crop_vars_data%nonyield_diag_cpft(land_pts,ncpft))
  ALLOCATE(crop_vars_data%harvest_trigger_cpft(land_pts,ncpft))
  ALLOCATE(crop_vars_data%harvest_counter_cpft(land_pts,ncpft))
  ALLOCATE(crop_vars_data%leafc_diag_cpft(land_pts,ncpft))
  ALLOCATE(crop_vars_data%stemc_diag_cpft(land_pts,ncpft))
  ALLOCATE(crop_vars_data%croplai_cpft(land_pts,ncpft))
  ALLOCATE(crop_vars_data%cropcanht_cpft(land_pts,ncpft))
  ALLOCATE(crop_vars_data%sow_date_cpft(land_pts,ncpft))
  ALLOCATE(crop_vars_data%tt_veg_cpft(land_pts,ncpft))
  ALLOCATE(crop_vars_data%tt_rep_cpft(land_pts,ncpft))
  ALLOCATE(crop_vars_data%latestharv_date_cpft(land_pts,ncpft))

  crop_vars_data%harvc_cpft(:,:)     = 0.0
  crop_vars_data%reservec_cpft(:,:)  = 0.0
  crop_vars_data%yield_diag_cpft(:,:)      = 0.0
  crop_vars_data%nonyield_diag_cpft(:,:)   = 0.0
  crop_vars_data%harvest_trigger_cpft(:,:) = 0
  crop_vars_data%harvest_counter_cpft(:,:) = 0
  crop_vars_data%stemc_diag_cpft(:,:)      = 0.0
  crop_vars_data%leafc_diag_cpft(:,:)      = 0.0
  crop_vars_data%croplai_cpft(:,:)   = 0.0
  crop_vars_data%cropcanht_cpft(:,:) = 0.0
  crop_vars_data%sow_date_cpft(:,:)        = 0.0
  crop_vars_data%tt_veg_cpft(:,:)          = 0.0
  crop_vars_data%tt_rep_cpft(:,:)          = 0.0
  crop_vars_data%latestharv_date_cpft(:,:) = 0.0
ELSE
  ALLOCATE(crop_vars_data%harvc_cpft(1,1))
  ALLOCATE(crop_vars_data%reservec_cpft(1,1))
  ALLOCATE(crop_vars_data%yield_diag_cpft(1,1))
  ALLOCATE(crop_vars_data%nonyield_diag_cpft(1,1))
  ALLOCATE(crop_vars_data%harvest_trigger_cpft(1,1))
  ALLOCATE(crop_vars_data%harvest_counter_cpft(1,1))
  ALLOCATE(crop_vars_data%leafc_diag_cpft(1,1))
  ALLOCATE(crop_vars_data%stemc_diag_cpft(1,1))
  ALLOCATE(crop_vars_data%croplai_cpft(1,1))
  ALLOCATE(crop_vars_data%cropcanht_cpft(1,1))
  ALLOCATE(crop_vars_data%sow_date_cpft(1,1))
  ALLOCATE(crop_vars_data%tt_veg_cpft(1,1))
  ALLOCATE(crop_vars_data%tt_rep_cpft(1,1))
  ALLOCATE(crop_vars_data%latestharv_date_cpft(1,1))
END IF


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE crop_vars_alloc

!===============================================================================
SUBROUTINE crop_vars_dealloc( crop_vars_data )

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
TYPE(crop_vars_data_type), INTENT(IN OUT) :: crop_vars_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CROP_VARS_DEALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!  ====crop_vars_mod module common====
DEALLOCATE(crop_vars_data%sthu_irr_soilt)
DEALLOCATE(crop_vars_data%frac_irr_all)
DEALLOCATE(crop_vars_data%irrfrac_irrtiles)
DEALLOCATE(crop_vars_data%frac_irr_soilt)
DEALLOCATE(crop_vars_data%frac_irr_old_soilt)
DEALLOCATE(crop_vars_data%frac_irr_surft)
DEALLOCATE(crop_vars_data%plant_n_gb)
DEALLOCATE(crop_vars_data%smc_irr_soilt)
DEALLOCATE(crop_vars_data%wt_ext_irr_surft)
DEALLOCATE(crop_vars_data%gs_irr_surft)
DEALLOCATE(crop_vars_data%gc_irr_surft)
DEALLOCATE(crop_vars_data%resfs_irr_surft)
DEALLOCATE(crop_vars_data%ext_irr_soilt)
DEALLOCATE(crop_vars_data%wt_ext_irr_gb)
DEALLOCATE(crop_vars_data%fsmc_irr_gb)
DEALLOCATE(crop_vars_data%irrDaysDiag_gb)
DEALLOCATE(crop_vars_data%irrig_water_gb)
DEALLOCATE(crop_vars_data%icntmax_gb)
DEALLOCATE(crop_vars_data%tl_1_day_av_gb)
DEALLOCATE(crop_vars_data%tl_1_day_av_use_gb)
DEALLOCATE(crop_vars_data%prec_1_day_av_gb)
DEALLOCATE(crop_vars_data%prec_1_day_av_use_gb)
DEALLOCATE(crop_vars_data%rn_1_day_av_gb)
DEALLOCATE(crop_vars_data%rn_1_day_av_use_gb)
DEALLOCATE(crop_vars_data%dvi_cpft)
DEALLOCATE(crop_vars_data%rootc_cpft)
DEALLOCATE(crop_vars_data%phot)
DEALLOCATE(crop_vars_data%dphotdt)
DEALLOCATE(crop_vars_data%harvc_cpft)
DEALLOCATE(crop_vars_data%reservec_cpft)
DEALLOCATE(crop_vars_data%yield_diag_cpft)
DEALLOCATE(crop_vars_data%nonyield_diag_cpft)
DEALLOCATE(crop_vars_data%harvest_trigger_cpft)
DEALLOCATE(crop_vars_data%harvest_counter_cpft)
DEALLOCATE(crop_vars_data%leafc_diag_cpft)
DEALLOCATE(crop_vars_data%stemc_diag_cpft)
DEALLOCATE(crop_vars_data%croplai_cpft)
DEALLOCATE(crop_vars_data%cropcanht_cpft)
DEALLOCATE(crop_vars_data%sow_date_cpft)
DEALLOCATE(crop_vars_data%tt_veg_cpft)
DEALLOCATE(crop_vars_data%tt_rep_cpft)
DEALLOCATE(crop_vars_data%latestharv_date_cpft)


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE crop_vars_dealloc

!===============================================================================
SUBROUTINE crop_vars_assoc(crop_vars, crop_vars_data )

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
TYPE(crop_vars_type), INTENT(IN OUT) :: crop_vars
TYPE(crop_vars_data_type), INTENT(IN OUT), TARGET :: crop_vars_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CROP_VARS_ASSOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL crop_vars_nullify(crop_vars)

!  ====crop_vars_mod module common====
crop_vars%sthu_irr_soilt => crop_vars_data%sthu_irr_soilt
crop_vars%frac_irr_all => crop_vars_data%frac_irr_all
crop_vars%irrfrac_irrtiles => crop_vars_data%irrfrac_irrtiles
crop_vars%frac_irr_soilt => crop_vars_data%frac_irr_soilt
crop_vars%frac_irr_old_soilt => crop_vars_data%frac_irr_old_soilt
crop_vars%frac_irr_surft => crop_vars_data%frac_irr_surft
crop_vars%plant_n_gb => crop_vars_data%plant_n_gb
crop_vars%smc_irr_soilt => crop_vars_data%smc_irr_soilt
crop_vars%wt_ext_irr_surft => crop_vars_data%wt_ext_irr_surft
crop_vars%gs_irr_surft => crop_vars_data%gs_irr_surft
crop_vars%gc_irr_surft => crop_vars_data%gc_irr_surft
crop_vars%resfs_irr_surft => crop_vars_data%resfs_irr_surft
crop_vars%ext_irr_soilt => crop_vars_data%ext_irr_soilt
crop_vars%wt_ext_irr_gb => crop_vars_data%wt_ext_irr_gb
crop_vars%fsmc_irr_gb => crop_vars_data%fsmc_irr_gb
crop_vars%irrDaysDiag_gb => crop_vars_data%irrDaysDiag_gb
crop_vars%irrig_water_gb => crop_vars_data%irrig_water_gb
crop_vars%icntmax_gb => crop_vars_data%icntmax_gb
crop_vars%tl_1_day_av_gb => crop_vars_data%tl_1_day_av_gb
crop_vars%tl_1_day_av_use_gb => crop_vars_data%tl_1_day_av_use_gb
crop_vars%prec_1_day_av_gb => crop_vars_data%prec_1_day_av_gb
crop_vars%prec_1_day_av_use_gb => crop_vars_data%prec_1_day_av_use_gb
crop_vars%rn_1_day_av_gb => crop_vars_data%rn_1_day_av_gb
crop_vars%rn_1_day_av_use_gb => crop_vars_data%rn_1_day_av_use_gb
crop_vars%dvi_cpft => crop_vars_data%dvi_cpft
crop_vars%rootc_cpft => crop_vars_data%rootc_cpft
crop_vars%phot => crop_vars_data%phot
crop_vars%dphotdt => crop_vars_data%dphotdt
crop_vars%harvc_cpft => crop_vars_data%harvc_cpft
crop_vars%reservec_cpft => crop_vars_data%reservec_cpft
crop_vars%yield_diag_cpft => crop_vars_data%yield_diag_cpft
crop_vars%nonyield_diag_cpft => crop_vars_data%nonyield_diag_cpft
crop_vars%harvest_trigger_cpft => crop_vars_data%harvest_trigger_cpft
crop_vars%harvest_counter_cpft => crop_vars_data%harvest_counter_cpft
crop_vars%leafc_diag_cpft => crop_vars_data%leafc_diag_cpft
crop_vars%stemc_diag_cpft => crop_vars_data%stemc_diag_cpft
crop_vars%croplai_cpft => crop_vars_data%croplai_cpft
crop_vars%cropcanht_cpft => crop_vars_data%cropcanht_cpft
crop_vars%sow_date_cpft => crop_vars_data%sow_date_cpft
crop_vars%tt_veg_cpft => crop_vars_data%tt_veg_cpft
crop_vars%tt_rep_cpft => crop_vars_data%tt_rep_cpft
crop_vars%latestharv_date_cpft => crop_vars_data%latestharv_date_cpft

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE crop_vars_assoc

!===============================================================================
SUBROUTINE crop_vars_nullify(crop_vars)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
TYPE(crop_vars_type), INTENT(IN OUT) :: crop_vars

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CROP_VARS_NULLIFY'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!  ====crop_vars_mod module common====
NULLIFY(crop_vars%sthu_irr_soilt)
NULLIFY(crop_vars%frac_irr_all)
NULLIFY(crop_vars%irrfrac_irrtiles)
NULLIFY(crop_vars%frac_irr_soilt)
NULLIFY(crop_vars%frac_irr_old_soilt)
NULLIFY(crop_vars%frac_irr_surft)
NULLIFY(crop_vars%plant_n_gb)
NULLIFY(crop_vars%smc_irr_soilt)
NULLIFY(crop_vars%wt_ext_irr_surft)
NULLIFY(crop_vars%gs_irr_surft)
NULLIFY(crop_vars%gc_irr_surft)
NULLIFY(crop_vars%resfs_irr_surft)
NULLIFY(crop_vars%ext_irr_soilt)
NULLIFY(crop_vars%wt_ext_irr_gb)
NULLIFY(crop_vars%fsmc_irr_gb)
NULLIFY(crop_vars%irrDaysDiag_gb)
NULLIFY(crop_vars%irrig_water_gb)
NULLIFY(crop_vars%icntmax_gb)
NULLIFY(crop_vars%tl_1_day_av_gb)
NULLIFY(crop_vars%tl_1_day_av_use_gb)
NULLIFY(crop_vars%prec_1_day_av_gb)
NULLIFY(crop_vars%prec_1_day_av_use_gb)
NULLIFY(crop_vars%rn_1_day_av_gb)
NULLIFY(crop_vars%rn_1_day_av_use_gb)
NULLIFY(crop_vars%dvi_cpft)
NULLIFY(crop_vars%rootc_cpft)
NULLIFY(crop_vars%phot)
NULLIFY(crop_vars%dphotdt)
NULLIFY(crop_vars%harvc_cpft)
NULLIFY(crop_vars%reservec_cpft)
NULLIFY(crop_vars%yield_diag_cpft)
NULLIFY(crop_vars%nonyield_diag_cpft)
NULLIFY(crop_vars%harvest_trigger_cpft)
NULLIFY(crop_vars%harvest_counter_cpft)
NULLIFY(crop_vars%leafc_diag_cpft)
NULLIFY(crop_vars%stemc_diag_cpft)
NULLIFY(crop_vars%croplai_cpft)
NULLIFY(crop_vars%cropcanht_cpft)
NULLIFY(crop_vars%sow_date_cpft)
NULLIFY(crop_vars%tt_veg_cpft)
NULLIFY(crop_vars%tt_rep_cpft)
NULLIFY(crop_vars%latestharv_date_cpft)
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE crop_vars_nullify
END MODULE crop_vars_mod
