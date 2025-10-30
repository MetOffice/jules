!******************************COPYRIGHT****************************************
! (c) British Antarctic Survey and University of Bristol, 2024.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT****************************************

MODULE jules_wtrac_type_mod

!-------------------------------------------------------------------------------
! Description:
!   Derived type for storing water tracer fields (prognostics and
!   diagnostics) that are used in JULES.
!
!   Water tracer fields are equal to the equivalent water field multiplied by
!   the ratio of water tracer to water e.g.
!
!             wtrac_jls%snow_surft = ratio * snow_surft
!
!   where ratio is the ratio of water tracer to water in the snow.
!
! Code Description:
!   Language: Fortran 90.
!-------------------------------------------------------------------------------

USE missing_data_mod, ONLY: imdi, rmdi
USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Data
!-----------------------------------------------------------------------------

TYPE :: jls_wtrac_data_type

  !==================================================================
  ! Prognostic fields (i.e. required at the start of a UM_JULES run):
  !==================================================================

  ! Land snow:

  REAL(KIND=real_jlslsm), ALLOCATABLE :: snow_surft(:,:,:)
    ! Water tracer amount in lying snow on tiles (kg/m2)
    ! If can_model=4,
    !   snow_surft is the snow on the canopy
    !   snow_grnd is the snow on the ground beneath canopy
    ! If can_model/=4, snow_surft is the total snow.
    ! Used in: explicit, implicit, extra(snow)
    ! Dimensions: (land_pts,nsurft,n_wtrac_jls)
    ! UM stash code: 33,178
    ! Normal water equivalents: progs%snow_surft (UM stash code: 0,240)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: snow_grnd_surft(:,:,:)
    ! Water tracer in snow on the ground (kg/m2)
    ! This is the snow beneath the canopy and is only used if can_model=4.
    ! Used in: extra(snow)
    ! Dimensions: (land_pts,nsurft,n_wtrac_jls)
    ! UM stash code: 33,179
    ! Normal water equivalents: progs%snow_grnd  (UM stash code: 0,242)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: sice_surft(:,:,:,:)
    ! Water tracer snow layer ice mass on tiles (kg/m2)
    ! Used in: extra(snow)
    ! Dimensions: (land_pts,nsurft,nsmax,n_wtrac_jls)
    ! UM stash code: 33,180
    ! Normal water equivalents: progs%sice_surft (UM stash code: 0,382)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: sliq_surft(:,:,:,:)
    ! Water tracer snow layer liquid mass on tiles (kg/m2)
    ! Used in: extra(snow)
    ! Dimensions: (land_pts,nsurft,nsmax,n_wtrac_jls)
    ! UM stash code: 33,181
    ! Normal water equivalents: progs%sliq_surft (UM stash code: 0,383)

  ! Canopy:

  REAL(KIND=real_jlslsm), ALLOCATABLE :: canopy_surft(:,:,:)
    ! Surface/canopy water tracer for snow-free land tiles (kg/m2)
    ! Used in: explicit, implicit, extra(hydrol)
    ! Dimensions: (land_pts,nsurft,n_wtrac_jls)
    ! UM stash code: 33,182
    ! Normal water equivalents: progs%canopy_surft (UM stash code: 0,229)

  ! Soil:

  REAL(KIND=real_jlslsm), ALLOCATABLE :: sthu_soilt(:,:,:,:)
    ! Water tracer in unfrozen soil moisture of layer on
    ! soil tiles as a fraction of saturation
    ! Used in: explicit and extra (hydrol, rivers)
    ! Dimensions: (land_pts,nsoilt,sm_levels,n_wtrac_jls)
    ! UM stash code: 33,183
    ! Normal water equivalents: psparms%sthu_soilt (UM stash code: 0,214)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: sthf_soilt(:,:,:,:)
    ! Water tracer in frozen soil moisture of layer on
    ! soil tiles as a fraction of saturation
    ! Used in: extra (hydrol)
    ! Dimensions: (land_pts,nsoilt,sm_levels,n_wtrac_jls)
    ! UM stash code: 33,184
    ! Normal water equivalents: psparms%sthf_soilt (UM stash code: 0,215)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: smcl_soilt(:,:,:,:)
    ! Water tracer soil moisture of layer on soil tiles (kg/m2)
    ! Used in: extra (hydrol, rivers)
    ! Dimensions: (land_pts,nsoilt,sm_levels,n_wtrac_jls)
    ! UM stash code: 33,185  (Note, in UM, nsoilt = 1)
    ! Normal water equivalents: progs%smcl_soilt (In UM: soil_layer_moisture)
    !                                           !(UM stash code: 0,009)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: sthzw_soilt(:,:,:)
    ! Water tracer soil moisture fraction in additional deep soil layer
    ! relative to saturation.
    ! used in TOPMODEL scheme.
    ! Used in: extra(hydrol)
    ! Dimensions: (land_pts,nsoilt,n_wtrac_jls)
    ! UM stash code: 33,186
    ! Normal water equivalents: sthzw (UM stash code: 0,281)

  ! Rivers:

  REAL(KIND=real_jlslsm), ALLOCATABLE :: tot_surf_runoff_gb(:,:)
    ! Water tracer surface runoff accumulated over river routing timestep
    ! (kg/m2/s)
    ! Used in: extra (rivers)
    ! Dimensions: (land_pts,n_wtrac_jls)
    ! UM stash code: 33,187
    ! Normal water equivalents: tot_surf_runoff_gb  (UM stash code: 0,155)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: tot_sub_runoff_gb(:,:)
    ! Water tracer subsurface runoff accumulated over river routing timestep
    ! (kg/m2/s)
    ! Used in: extra (rivers)
    ! Dimensions: (land_pts,n_wtrac_jls)
    ! UM stash code: 33,188
    ! Normal water equivalents: tot_sub_runoff_gb  (UM stash code: 0,156)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: acc_lake_evap_gb(:,:,:)
    ! Water tracer lake evaporation accumulated over river routing timestep
    ! (kg/m2)
    ! Used in: extra (rivers)
    ! Dimensions: (t_i_length,t_j_length,n_wtrac_jls)
    ! UM stash code: 33,189
    ! Normal water equivalents: acc_lake_evap_gb  (UM stash code: 0,290)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: twatstor(:,:,:)
    ! Water tracer store in rivers on RIVER grid (kg)
    ! Used in: extra (rivers)
    ! Dimensions: (river_row_length,river_rows,n_wtrac_jls))
    ! UM stash code: 33,190
    ! Normal water equivalents: twatstor    (UM stash code: 0,153)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: inlandout_atm_gb(:,:)
    ! Inland basin water tracer outflow (kg/m2/s)
    ! Used in: extra (hydrol, rivers)
    ! Dimensions: (land_pts,n_wtrac_jls)
    ! UM stash code: 33,191
    ! Normal water equivalents: inlandout_atm_gb (UM stash code: 0,511)

  !=====================================================================
  ! Coupling fields (UM -> JULES) (note, these are at jls_lsm precision)
  !=====================================================================

  REAL(KIND=real_jlslsm), ALLOCATABLE :: qw_1_ij(:,:,:)
    ! Total water tracer content at lowest atmos level (Kg/Kg)
    ! Used in: explicit
    ! Dimensions: (t_i_length,t_j_length,n_wtrac_jls)
    ! Normal water equivalents: forcing%qw_1_ij

  REAL(KIND=real_jlslsm), ALLOCATABLE :: ls_rain_ij(:,:,:)
    ! Water tracer large-scale rain (kg/m2/s)
    ! Used in: extra
    ! Dimensions: (t_i_length,t_j_length,n_wtrac_jls)
    ! UM stash code: 33,170
    ! Normal water equivalents: forcing%ls_rain_ij (UM stash code: 4,203)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: con_rain_ij(:,:,:)
    ! Water tracer convective rain (kg/m2/s)
    ! Used in: extra
    ! Dimensions: (t_i_length,t_j_length,n_wtrac_jls)
    ! UM stash code: 33,172
    ! Normal water equivalents:  forcing%con_rain_ij (UM stash code: 5,205)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: ls_snow_ij(:,:,:)
    ! Water tracer large-scale snowfall (kg/m2/s)
    ! Used in: extra
    ! Dimensions: (t_i_length,t_j_length,n_wtrac_jls)
    ! UM stash code: 33,171
    ! Normal water equivalents: forcing%ls_snow_ij (UM stash code: 4,204)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: con_snow_ij(:,:,:)
    ! Water tracer convective snowfall (kg/m2/s)
    ! Used in: extra
    ! Dimensions: (t_i_length,t_j_length,n_wtrac_jls)
    ! UM stash code: 33,173
    ! Normal water equivalents: forcing%con_snow_ij (UM stash code: 5,206)

  !=====================================================================
  ! Coupling fields (JULES -> UM) (note, these are at jls_lsm precision)
  !=====================================================================

  REAL(KIND=real_jlslsm), ALLOCATABLE :: fqw_1(:,:,:)
   !  Water tracer GBM surface flux (kg/m2/s)
    ! Used in: explicit, implicit
    ! Dimensions: (t_i_length,t_j_length,n_wtrac_jls)
    ! UM stash code: 33,177
    ! Normal water equivalents: fqw_1

  !=====================================================================
  ! Coupling fields (OCEAN/SEAICE -> UM -> JULES)
  !=====================================================================

  REAL(KIND=real_jlslsm), ALLOCATABLE :: r_sea(:,:,:)
    ! Water tracer to water ratio of sea surface
    ! Temporarily set to 1.0 in atmos_physics2 but will come from
    ! coupling to ocean model or an ancillary file in the future.
    ! Used in: explicit
    ! Dimensions: (t_i_length,t_j_length,n_wtrac_jls)
    ! UM stash code: TBD
    ! Normal water equivalents: NONE

  REAL(KIND=real_jlslsm), ALLOCATABLE :: r_sice(:,:,:,:)
    ! Water tracer to water ratio of sea ice categories
    ! Temporarily set to 1.0 in atmos_physics2 but will come from
    ! coupling to ocean model or an ancillary file in the future.
    ! Used in: explicit
    ! Dimensions: (t_i_length,t_j_length,nice,n_wtrac_jls)
    ! UM stash code: TBD
    ! Normal water equivalents: NONE

  !=====================================================================
  ! Coupling fields (JULES -> UM -> OCEAN/SEAICE)
  !=====================================================================

  REAL(KIND=real_jlslsm), ALLOCATABLE :: e_sea_ij(:,:,:)
  !   Water tracer evaporation from open ocean (kg/m2/s)
  !   On output from JULES (implicit), this is a sea mean flux.
  ! Used in: implicit
  ! Dimensions: (t_i_length,t_j_length,n_wtrac_jls)
  ! UM stash code: 33,192
  ! Normal water equivalents: fluxes%e_sea_ij (UM stash code: 3,232)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: ei_sice_ij(:,:,:,:)
  ! Water tracer sea ice sublimation on sea ice categories (kg/m2/s)
  ! On output from JULES (implicit), this is a local flux.
  ! Used in: implicit
  ! Dimensions: (t_i_length,t_j_length,nice,n_wtrac_jls)
  ! UM stash code: 33,193  (weighted by ice fraction on output)
  ! Normal water equivalents: fluxes%ei_sice (UM stash code: 3,509 - weighted
  !                                               by ice fraction on output)

  !================
  ! SURFACE FLUXES
  !================

  ! Passed between JULES surf_couple_* routines

  REAL(KIND=real_jlslsm), ALLOCATABLE :: fqw_surft(:,:,:)
    ! Water tracer surface moisture flux for land tiles (kg/m2/s)
    ! Used in: explicit, implicit, extra(rivers)
    ! Dimensions: (land_pts,nsurft,n_wtrac_jls)
    ! UM stash code: None
    ! Normal water equivalents: fluxes%fqw_surft

  REAL(KIND=real_jlslsm), ALLOCATABLE :: fqw_evapsrce(:,:,:,:)
    ! Water tracer surface EXPLICIT moisture flux for land tiles for
    ! separate evaporative surfaces (kg/m2/s):
    !     (:,1,:,:) sublimation
    !     (:,2,:,:) evaporation from intercepted water
    !     (:,3,:,:) soil evapotranspiration
    !     (:,4,:,:) lake evaporation
    ! Used in: explicit, implicit
    ! Dimensions: (land_pts,n_evap_srce,nsurft,n_wtrac_jls)
    ! UM stash codes: 33,194-197
    ! Normal water equivalents: NONE

  REAL(KIND=real_jlslsm), ALLOCATABLE :: fqw_sea(:,:,:)
    ! Water tracer evaporation from open ocean from the EXPLICIT scheme
    ! (kg/m2/s)
    ! Used in: explicit, implicit
    ! Dimensions: (t_i_length,t_j_length,n_wtrac_jls)
    ! UM stash code: 33,198 - Weighted by leads fraction on output so
    !                comparable with final flux e_sea_ij
    ! Normal water equivalents: NONE

  REAL(KIND=real_jlslsm), ALLOCATABLE :: fqw_sicat(:,:,:,:)
   ! Sea ice sublimation for water tracers on sea ice categories from
   ! the EXPLICIT scheme (kg/m2/s)
   ! Used in: explicit, implicit
   ! Dimensions: (t_i_length,t_j_length,nice,n_wtrac_jls)
   ! UM stash code: 33,199 - Weighted by sea ice fraction on output so
   !                comparable with final flux ei_sice_ij
   ! Normal water equivalents: NONE

  REAL(KIND=real_jlslsm), ALLOCATABLE :: ei_surft(:,:,:)
  ! Water tracer sublimation for land tiles (kg/m2/s)
  ! Used in: implicit, extra(snow)
  ! Dimensions: (land_pts,nsurft,n_wtrac_jls))
  ! UM stash code: 33,200
  ! Normal water equivalents: fluxes%ei_surft (UM stash code: 3,331)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: ecan_surft(:,:,:)
  ! Water tracer canopy evaporation from for snow-free land tiles (kg/m2/s)
  ! Used in: implicit, extra(hydrol)
  ! Dimensions: (land_pts,nsurft,n_wtrac_jls))
  ! UM stash code: 33,201
  ! Normal water equivalents: fluxes%ecan_surft  (UM stash code: 3,287)

  ! Diagnostics

  REAL(KIND=real_jlslsm), ALLOCATABLE :: esoil_surft(:,:,:)
  ! Water tracer evapotranspiration for snow-free land tiles (kg/m2/s)
  ! Used in: implicit
  ! Dimensions: (land_pts,nsurft,n_wtrac_jls)
  ! UM stash code: 33,202
  ! Normal water equivalents: fluxes%esoil_surft (UM stash code: 3,288)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: lake_evap(:,:)
  ! Water tracer evaporation from lakes (kg/m2/s)
  ! This is only set on the 'lake' tile, hence there is no nsurft dimension
  ! Used in: implicit, extra(rivers)
  ! Dimensions: (land_pts,n_wtrac_jls)
  ! UM stash code: 33,203
  ! Normal water equivalents: NONE (not output from JULES)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: ei_ij(:,:,:)
  ! Water tracer sublimation from land (land mean) (kg/m2/s)
  ! Used in: implicit
  ! Dimensions: (t_i_length,t_j_length,n_wtrac_jls)
  ! UM stash code: 33,204
  ! Normal water equivalents: fluxes%ei_ij  (UM stash code: 3,298)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: ecan_ij(:,:,:)
  ! Water tracer evaporation from canopy/surface store (land mean) (kg/m2/s)
  ! Used in: implicit
  ! Dimensions: (t_i_length,t_j_length,n_wtrac_jls)
  ! UM stash code: 33,205
  ! Normal water equivalents: fluxes%ecan_ij (UM stash code: 3,297)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: esoil_ij_soilt(:,:,:,:)
  ! Water tracer evapotranspiration from soil moisture store (land mean)
  ! (kg/m2/s)
  ! (nsoilt=1 in the UM, so this is land mean)
  ! Used in: implicit
  ! Dimensions: (t_i_length,t_j_length,nsoilt,n_wtrac_jls)
  ! UM stash code: 33,206
  ! Normal water equivalents: fluxes%esoil_ij_soilt  (UM stash code: 3,296)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: dfqw_imp(:,:,:)
    ! Implicit update applied to water tracer surface flux (kg/m2/s)
    ! Used in: implicit
    ! Dimensions: (t_i_length,t_j_length,n_wtrac_jls)
    ! UM stash code: 33,207
    ! Normal water equivalents: NONE

  !==================================
  ! Non-prognostic soil fields/fluxes
  !==================================

  ! Passed between JULES surf_couple_* routines

  REAL(KIND=real_jlslsm), ALLOCATABLE :: smc_soilt(:,:,:)
    ! Soil moisture water tracer in a layer at the surface (kg/m2).
    ! Used in: explicit, implicit and extra (hydrol)
    ! Dimensions: (land_pts,nsoilt,sm_levels,n_wtrac_jls)
    ! UM stash code: NONE (store in this module only)
    ! Normal water equivalents: progs%smc_soilt   (UM stash code: NONE - local
    !                                               field in AP2)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: ext_soilt(:,:,:,:)
    ! Extraction of water tracers by plants from each soil layer (kg/m2/s)
    ! Used in: implicit and extra (hydrol)
    ! Dimensions: (land_pts,nsoilt,sm_levels,n_wtrac_jls)
    ! UM stash code: NONE (store in this module only)
    ! Normal water equivalents: fluxes%ext_soilt (UM stash code: NONE)


  !=================================
  ! River routing fields
  !=================================

  ! Passed between hydrology and river routing routines

  REAL(KIND=real_jlslsm), ALLOCATABLE :: surf_roff_gb(:,:)
  ! Water tracer surface runoff (kg/m2/s)
  ! Used in: extra (hydrol, rivers)
  ! Dimensions: (land_pts,n_wtrac_jls)
  ! UM stash code: NONE (store in this module only)
  ! Normal water equivalents: fluxes%surf_roff_gb

  REAL(KIND=real_jlslsm), ALLOCATABLE :: sub_surf_roff_gb(:,:)
  ! Water tracer sub-surface runoff (kg/m2/s)
  ! Used in: extra (hydrol, rivers)
  ! Dimensions: (land_pts,n_wtrac_jls)
  ! UM stash code: NONE (store in this module only)
  ! Normal water equivalents: fluxes%sub_surf_roff_gb

END TYPE jls_wtrac_data_type

!-----------------------------------------------------------------------------
! Pointers
!-----------------------------------------------------------------------------

TYPE :: jls_wtrac_type

  REAL(KIND=real_jlslsm), POINTER :: snow_surft(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: snow_grnd_surft(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: sice_surft(:,:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: sliq_surft(:,:,:,:)

  REAL(KIND=real_jlslsm), POINTER :: canopy_surft(:,:,:)

  REAL(KIND=real_jlslsm), POINTER :: sthu_soilt(:,:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: sthf_soilt(:,:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: smcl_soilt(:,:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: sthzw_soilt(:,:,:)

  REAL(KIND=real_jlslsm), POINTER :: tot_surf_runoff_gb(:,:)
  REAL(KIND=real_jlslsm), POINTER :: tot_sub_runoff_gb(:,:)
  REAL(KIND=real_jlslsm), POINTER :: acc_lake_evap_gb(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: twatstor(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: inlandout_atm_gb(:,:)

  REAL(KIND=real_jlslsm), POINTER :: qw_1_ij(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: ls_rain_ij(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: con_rain_ij(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: ls_snow_ij(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: con_snow_ij(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: fqw_1(:,:,:)

  REAL(KIND=real_jlslsm), POINTER :: r_sea(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: r_sice(:,:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: e_sea_ij(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: ei_sice_ij(:,:,:,:)

  REAL(KIND=real_jlslsm), POINTER :: fqw_surft(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: fqw_evapsrce(:,:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: fqw_sea(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: fqw_sicat(:,:,:,:)

  REAL(KIND=real_jlslsm), POINTER :: ei_surft(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: ecan_surft(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: esoil_surft(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: lake_evap(:,:)

  REAL(KIND=real_jlslsm), POINTER :: ei_ij(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: ecan_ij(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: esoil_ij_soilt(:,:,:,:)

  REAL(KIND=real_jlslsm), POINTER :: dfqw_imp(:,:,:)

  REAL(KIND=real_jlslsm), POINTER :: smc_soilt(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: ext_soilt(:,:,:,:)

  REAL(KIND=real_jlslsm), POINTER :: surf_roff_gb(:,:)
  REAL(KIND=real_jlslsm), POINTER :: sub_surf_roff_gb(:,:)

END TYPE jls_wtrac_type

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_WTRAC_TYPE_MOD'

CONTAINS

! ==========================================================================

SUBROUTINE wtrac_jls_alloc(land_pts, t_i_length, t_j_length,                   &
                       nsurft, nsoilt, sm_levels, nsmax,                       &
                       nice_use, n_wtrac_jls, n_evap_srce,                     &
                       river_row_length, river_rows, l_wtrac_jls,              &
                       wtrac_jls_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

INTEGER, INTENT(IN) :: land_pts, t_i_length, t_j_length,                       &
                       nsurft, nsoilt, sm_levels, nsmax,                       &
                       nice_use, n_wtrac_jls, n_evap_srce,                     &
                       river_row_length, river_rows
LOGICAL, INTENT(IN) :: l_wtrac_jls

!Instance of the data type we need to allocate
TYPE(jls_wtrac_data_type), INTENT(IN OUT) :: wtrac_jls_data

!Local variables
INTEGER :: temp_size, temp_tiles, temp_layers

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='WTRAC_JLS_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!For multilayer snow variables, only allocate to full size if the scheme is
!being used, ie nsmax > 0
IF (nsmax > 0) THEN
  temp_size   = land_pts
  temp_tiles  = nsurft
  temp_layers = nsmax
ELSE
  temp_size   = 1
  temp_tiles  = 1
  temp_layers = 1
END IF

IF (l_wtrac_jls) THEN
  ALLOCATE(wtrac_jls_data%snow_surft(land_pts,nsurft,n_wtrac_jls))
  ALLOCATE(wtrac_jls_data%snow_grnd_surft(land_pts,nsurft,n_wtrac_jls))
  ALLOCATE(wtrac_jls_data%sice_surft(temp_size,temp_tiles,temp_layers,         &
                                     n_wtrac_jls))
  ALLOCATE(wtrac_jls_data%sliq_surft(temp_size,temp_tiles,temp_layers,         &
                                     n_wtrac_jls))

  ALLOCATE(wtrac_jls_data%canopy_surft(land_pts,nsurft,n_wtrac_jls))

  ALLOCATE(wtrac_jls_data%sthu_soilt(land_pts,nsoilt,sm_levels,n_wtrac_jls))
  ALLOCATE(wtrac_jls_data%sthf_soilt(land_pts,nsoilt,sm_levels,n_wtrac_jls))
  ALLOCATE(wtrac_jls_data%smcl_soilt(land_pts,nsoilt,sm_levels,n_wtrac_jls))
  ALLOCATE(wtrac_jls_data%sthzw_soilt(land_pts,nsoilt,n_wtrac_jls))

  ALLOCATE( wtrac_jls_data%tot_surf_runoff_gb(land_pts,n_wtrac_jls))
  ALLOCATE( wtrac_jls_data%tot_sub_runoff_gb(land_pts,n_wtrac_jls))
  ALLOCATE( wtrac_jls_data%acc_lake_evap_gb(t_i_length,t_j_length,n_wtrac_jls))
  ALLOCATE( wtrac_jls_data%twatstor(river_row_length,river_rows,n_wtrac_jls))
  ALLOCATE( wtrac_jls_data%inlandout_atm_gb(land_pts,n_wtrac_jls))

  ALLOCATE(wtrac_jls_data%qw_1_ij(t_i_length,t_j_length,n_wtrac_jls))
  ALLOCATE(wtrac_jls_data%ls_rain_ij(t_i_length,t_j_length,n_wtrac_jls))
  ALLOCATE(wtrac_jls_data%con_rain_ij(t_i_length,t_j_length,n_wtrac_jls))
  ALLOCATE(wtrac_jls_data%ls_snow_ij(t_i_length,t_j_length,n_wtrac_jls))
  ALLOCATE(wtrac_jls_data%con_snow_ij(t_i_length,t_j_length,n_wtrac_jls))
  ALLOCATE(wtrac_jls_data%fqw_1(t_i_length,t_j_length,n_wtrac_jls))

  ALLOCATE(wtrac_jls_data%r_sea(t_i_length,t_j_length,n_wtrac_jls))
  ALLOCATE(wtrac_jls_data%r_sice(t_i_length,t_j_length,nice_use,n_wtrac_jls))
  ALLOCATE(wtrac_jls_data%e_sea_ij(t_i_length,t_j_length,n_wtrac_jls))
  ALLOCATE(wtrac_jls_data%ei_sice_ij(t_i_length,t_j_length,nice_use,           &
                                     n_wtrac_jls))

  ALLOCATE(wtrac_jls_data%fqw_surft(land_pts,nsurft,n_wtrac_jls))
  ALLOCATE(wtrac_jls_data%fqw_evapsrce(land_pts,n_evap_srce,nsurft,            &
                                       n_wtrac_jls))
  ALLOCATE(wtrac_jls_data%fqw_sea(t_i_length,t_j_length,n_wtrac_jls))
  ALLOCATE(wtrac_jls_data%fqw_sicat(t_i_length,t_j_length,nice_use,            &
                                    n_wtrac_jls))

  ALLOCATE(wtrac_jls_data%ei_surft(land_pts,nsurft,n_wtrac_jls))
  ALLOCATE(wtrac_jls_data%ecan_surft(land_pts,nsurft,n_wtrac_jls))
  ALLOCATE(wtrac_jls_data%esoil_surft(land_pts,nsurft,n_wtrac_jls))
  ALLOCATE(wtrac_jls_data%lake_evap(land_pts,n_wtrac_jls))

  ALLOCATE(wtrac_jls_data%ei_ij(t_i_length,t_j_length,n_wtrac_jls))
  ALLOCATE(wtrac_jls_data%ecan_ij(t_i_length,t_j_length,n_wtrac_jls))
  ALLOCATE(wtrac_jls_data%esoil_ij_soilt(t_i_length,t_j_length,nsoilt,         &
                                         n_wtrac_jls))

  ALLOCATE(wtrac_jls_data%dfqw_imp(t_i_length,t_j_length,n_wtrac_jls))

  ALLOCATE(wtrac_jls_data%smc_soilt(land_pts,nsoilt,n_wtrac_jls))
  ALLOCATE(wtrac_jls_data%ext_soilt(land_pts,nsoilt,sm_levels,n_wtrac_jls))

  ALLOCATE(wtrac_jls_data%surf_roff_gb(land_pts,n_wtrac_jls))
  ALLOCATE(wtrac_jls_data%sub_surf_roff_gb(land_pts,n_wtrac_jls))

ELSE
  ALLOCATE(wtrac_jls_data%snow_surft(1,1,1))
  ALLOCATE(wtrac_jls_data%snow_grnd_surft(1,1,1))
  ALLOCATE(wtrac_jls_data%sice_surft(1,1,1,1))
  ALLOCATE(wtrac_jls_data%sliq_surft(1,1,1,1))

  ALLOCATE(wtrac_jls_data%canopy_surft(1,1,1))

  ALLOCATE(wtrac_jls_data%sthu_soilt(1,1,1,1))
  ALLOCATE(wtrac_jls_data%sthf_soilt(1,1,1,1))
  ALLOCATE(wtrac_jls_data%smcl_soilt(1,1,1,1))
  ALLOCATE(wtrac_jls_data%sthzw_soilt(1,1,1))

  ALLOCATE(wtrac_jls_data%tot_surf_runoff_gb(1,1))
  ALLOCATE(wtrac_jls_data%tot_sub_runoff_gb(1,1))
  ALLOCATE(wtrac_jls_data%acc_lake_evap_gb(1,1,1))
  ALLOCATE(wtrac_jls_data%twatstor(1,1,1))
  ALLOCATE(wtrac_jls_data%inlandout_atm_gb(1,1))

  ALLOCATE(wtrac_jls_data%qw_1_ij(1,1,1))
  ALLOCATE(wtrac_jls_data%ls_rain_ij(1,1,1))
  ALLOCATE(wtrac_jls_data%con_rain_ij(1,1,1))
  ALLOCATE(wtrac_jls_data%ls_snow_ij(1,1,1))
  ALLOCATE(wtrac_jls_data%con_snow_ij(1,1,1))
  ALLOCATE(wtrac_jls_data%fqw_1(1,1,1))

  ALLOCATE(wtrac_jls_data%r_sea(1,1,1))
  ALLOCATE(wtrac_jls_data%r_sice(1,1,1,1))
  ALLOCATE(wtrac_jls_data%e_sea_ij(1,1,1))
  ALLOCATE(wtrac_jls_data%ei_sice_ij(1,1,1,1))

  ALLOCATE(wtrac_jls_data%fqw_surft(1,1,1))
  ALLOCATE(wtrac_jls_data%fqw_evapsrce(1,1,1,1))
  ALLOCATE(wtrac_jls_data%fqw_sea(1,1,1))
  ALLOCATE(wtrac_jls_data%fqw_sicat(1,1,1,1))

  ALLOCATE(wtrac_jls_data%ei_surft(1,1,1))
  ALLOCATE(wtrac_jls_data%ecan_surft(1,1,1))
  ALLOCATE(wtrac_jls_data%esoil_surft(1,1,1))
  ALLOCATE(wtrac_jls_data%lake_evap(1,1))

  ALLOCATE(wtrac_jls_data%ei_ij(1,1,1))
  ALLOCATE(wtrac_jls_data%ecan_ij(1,1,1))
  ALLOCATE(wtrac_jls_data%esoil_ij_soilt(1,1,1,1))

  ALLOCATE(wtrac_jls_data%dfqw_imp(1,1,1))

  ALLOCATE(wtrac_jls_data%smc_soilt(1,1,1))
  ALLOCATE(wtrac_jls_data%ext_soilt(1,1,1,1))

  ALLOCATE(wtrac_jls_data%surf_roff_gb(1,1))
  ALLOCATE(wtrac_jls_data%sub_surf_roff_gb(1,1))
END IF

wtrac_jls_data%snow_surft(:,:,:)         = 0.0
wtrac_jls_data%snow_grnd_surft(:,:,:)    = 0.0
wtrac_jls_data%sice_surft(:,:,:,:)       = 0.0
wtrac_jls_data%sliq_surft(:,:,:,:)       = 0.0

wtrac_jls_data%canopy_surft(:,:,:)       = 0.0

wtrac_jls_data%sthu_soilt(:,:,:,:)       = 0.0
wtrac_jls_data%sthf_soilt(:,:,:,:)       = 0.0
wtrac_jls_data%smcl_soilt(:,:,:,:)       = 0.0
wtrac_jls_data%sthzw_soilt(:,:,:)        = 0.0

wtrac_jls_data%tot_surf_runoff_gb(:,:)   = 0.0
wtrac_jls_data%tot_sub_runoff_gb(:,:)    = 0.0
wtrac_jls_data%acc_lake_evap_gb(:,:,:)   = 0.0
wtrac_jls_data%twatstor(:,:,:)           = 0.0
wtrac_jls_data%inlandout_atm_gb(:,:)     = 0.0

wtrac_jls_data%qw_1_ij(:,:,:)            = 0.0
wtrac_jls_data%ls_rain_ij(:,:,:)         = 0.0
wtrac_jls_data%con_rain_ij(:,:,:)        = 0.0
wtrac_jls_data%ls_snow_ij(:,:,:)         = 0.0
wtrac_jls_data%con_snow_ij(:,:,:)        = 0.0
wtrac_jls_data%fqw_1(:,:,:)              = 0.0

wtrac_jls_data%r_sea(:,:,:)              = 1.0
wtrac_jls_data%r_sice(:,:,:,:)           = 1.0
wtrac_jls_data%e_sea_ij(:,:,:)           = 0.0
wtrac_jls_data%ei_sice_ij(:,:,:,:)       = 0.0

wtrac_jls_data%fqw_surft(:,:,:)          = 0.0
wtrac_jls_data%fqw_evapsrce(:,:,:,:)     = 0.0
wtrac_jls_data%fqw_sea(:,:,:)            = 0.0
wtrac_jls_data%fqw_sicat(:,:,:,:)        = 0.0

wtrac_jls_data%ei_surft(:,:,:)           = 0.0
wtrac_jls_data%ecan_surft(:,:,:)         = 0.0
wtrac_jls_data%esoil_surft(:,:,:)        = 0.0
wtrac_jls_data%lake_evap(:,:)            = 0.0

wtrac_jls_data%ei_ij(:,:,:)              = 0.0
wtrac_jls_data%ecan_ij(:,:,:)            = 0.0
wtrac_jls_data%esoil_ij_soilt(:,:,:,:)   = 0.0

wtrac_jls_data%dfqw_imp(:,:,:)           = 0.0

wtrac_jls_data%smc_soilt(:,:,:)          = 0.0
wtrac_jls_data%ext_soilt(:,:,:,:)        = 0.0

wtrac_jls_data%surf_roff_gb(:,:)         = 0.0
wtrac_jls_data%sub_surf_roff_gb(:,:)     = 0.0

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE wtrac_jls_alloc

! =============================================================================
SUBROUTINE wtrac_jls_dealloc(wtrac_jls_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Instance of the data type we need to deallocate
TYPE(jls_wtrac_data_type), INTENT(IN OUT) :: wtrac_jls_data

!Local variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='WTRAC_JLS_DEALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DEALLOCATE(wtrac_jls_data%snow_surft)
DEALLOCATE(wtrac_jls_data%snow_grnd_surft)
DEALLOCATE(wtrac_jls_data%sice_surft)
DEALLOCATE(wtrac_jls_data%sliq_surft)

DEALLOCATE(wtrac_jls_data%canopy_surft)

DEALLOCATE(wtrac_jls_data%sthu_soilt)
DEALLOCATE(wtrac_jls_data%sthf_soilt)
DEALLOCATE(wtrac_jls_data%smcl_soilt)
DEALLOCATE(wtrac_jls_data%sthzw_soilt)

DEALLOCATE(wtrac_jls_data%tot_surf_runoff_gb)
DEALLOCATE(wtrac_jls_data%tot_sub_runoff_gb)
DEALLOCATE(wtrac_jls_data%acc_lake_evap_gb)
DEALLOCATE(wtrac_jls_data%twatstor)
DEALLOCATE(wtrac_jls_data%inlandout_atm_gb)

DEALLOCATE(wtrac_jls_data%qw_1_ij)
DEALLOCATE(wtrac_jls_data%ls_rain_ij)
DEALLOCATE(wtrac_jls_data%con_rain_ij)
DEALLOCATE(wtrac_jls_data%ls_snow_ij)
DEALLOCATE(wtrac_jls_data%con_snow_ij)
DEALLOCATE(wtrac_jls_data%fqw_1)

DEALLOCATE(wtrac_jls_data%r_sea)
DEALLOCATE(wtrac_jls_data%r_sice)
DEALLOCATE(wtrac_jls_data%e_sea_ij)
DEALLOCATE(wtrac_jls_data%ei_sice_ij)

DEALLOCATE(wtrac_jls_data%fqw_surft)
DEALLOCATE(wtrac_jls_data%fqw_evapsrce)
DEALLOCATE(wtrac_jls_data%fqw_sea)
DEALLOCATE(wtrac_jls_data%fqw_sicat)

DEALLOCATE(wtrac_jls_data%ei_surft)
DEALLOCATE(wtrac_jls_data%ecan_surft)
DEALLOCATE(wtrac_jls_data%esoil_surft)
DEALLOCATE(wtrac_jls_data%lake_evap)

DEALLOCATE(wtrac_jls_data%ei_ij)
DEALLOCATE(wtrac_jls_data%ecan_ij)
DEALLOCATE(wtrac_jls_data%esoil_ij_soilt)

DEALLOCATE(wtrac_jls_data%dfqw_imp)

DEALLOCATE(wtrac_jls_data%smc_soilt)
DEALLOCATE(wtrac_jls_data%ext_soilt)

DEALLOCATE(wtrac_jls_data%surf_roff_gb)
DEALLOCATE(wtrac_jls_data%sub_surf_roff_gb)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE wtrac_jls_dealloc

!==============================================================================
SUBROUTINE wtrac_jls_assoc(wtrac_jls,wtrac_jls_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

TYPE(jls_wtrac_type), INTENT(IN OUT) :: wtrac_jls
  !Instance of the pointer type we are associating

TYPE(jls_wtrac_data_type), INTENT(IN OUT), TARGET :: wtrac_jls_data
  !Instance of the data type we are associtating to

!Local variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='WTRAC_JLS_ASSOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL wtrac_jls_nullify(wtrac_jls)

wtrac_jls%snow_surft          => wtrac_jls_data%snow_surft
wtrac_jls%snow_grnd_surft     => wtrac_jls_data%snow_grnd_surft
wtrac_jls%sice_surft          => wtrac_jls_data%sice_surft
wtrac_jls%sliq_surft          => wtrac_jls_data%sliq_surft

wtrac_jls%canopy_surft        => wtrac_jls_data%canopy_surft

wtrac_jls%sthu_soilt          => wtrac_jls_data%sthu_soilt
wtrac_jls%sthf_soilt          => wtrac_jls_data%sthf_soilt
wtrac_jls%smcl_soilt          => wtrac_jls_data%smcl_soilt
wtrac_jls%sthzw_soilt         => wtrac_jls_data%sthzw_soilt

wtrac_jls%tot_surf_runoff_gb  => wtrac_jls_data%tot_surf_runoff_gb
wtrac_jls%tot_sub_runoff_gb   => wtrac_jls_data%tot_sub_runoff_gb
wtrac_jls%acc_lake_evap_gb    => wtrac_jls_data%acc_lake_evap_gb
wtrac_jls%twatstor            => wtrac_jls_data%twatstor
wtrac_jls%inlandout_atm_gb    => wtrac_jls_data%inlandout_atm_gb

wtrac_jls%qw_1_ij             => wtrac_jls_data%qw_1_ij
wtrac_jls%ls_rain_ij          => wtrac_jls_data%ls_rain_ij
wtrac_jls%con_rain_ij         => wtrac_jls_data%con_rain_ij
wtrac_jls%ls_snow_ij          => wtrac_jls_data%ls_snow_ij
wtrac_jls%con_snow_ij         => wtrac_jls_data%con_snow_ij
wtrac_jls%fqw_1               => wtrac_jls_data%fqw_1

wtrac_jls%r_sea               => wtrac_jls_data%r_sea
wtrac_jls%r_sice              => wtrac_jls_data%r_sice
wtrac_jls%e_sea_ij            => wtrac_jls_data%e_sea_ij
wtrac_jls%ei_sice_ij          => wtrac_jls_data%ei_sice_ij

wtrac_jls%fqw_surft           => wtrac_jls_data%fqw_surft
wtrac_jls%fqw_evapsrce        => wtrac_jls_data%fqw_evapsrce
wtrac_jls%fqw_sea             => wtrac_jls_data%fqw_sea
wtrac_jls%fqw_sicat           => wtrac_jls_data%fqw_sicat

wtrac_jls%ei_surft            => wtrac_jls_data%ei_surft
wtrac_jls%ecan_surft          => wtrac_jls_data%ecan_surft
wtrac_jls%esoil_surft         => wtrac_jls_data%esoil_surft
wtrac_jls%lake_evap           => wtrac_jls_data%lake_evap

wtrac_jls%ei_ij               => wtrac_jls_data%ei_ij
wtrac_jls%ecan_ij             => wtrac_jls_data%ecan_ij
wtrac_jls%esoil_ij_soilt      => wtrac_jls_data%esoil_ij_soilt

wtrac_jls%dfqw_imp            => wtrac_jls_data%dfqw_imp

wtrac_jls%smc_soilt           => wtrac_jls_data%smc_soilt
wtrac_jls%ext_soilt           => wtrac_jls_data%ext_soilt

wtrac_jls%surf_roff_gb        => wtrac_jls_data%surf_roff_gb
wtrac_jls%sub_surf_roff_gb    => wtrac_jls_data%sub_surf_roff_gb

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE wtrac_jls_assoc

!==============================================================================

SUBROUTINE wtrac_jls_nullify(wtrac_jls)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

TYPE(jls_wtrac_type), INTENT(IN OUT) :: wtrac_jls
  !Instance of the pointer type we are nullifying

!Local varaibles
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='WTRAC_JLS_NULLIFY'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

NULLIFY(wtrac_jls%snow_surft)
NULLIFY(wtrac_jls%snow_grnd_surft)
NULLIFY(wtrac_jls%sice_surft)
NULLIFY(wtrac_jls%sliq_surft)

NULLIFY(wtrac_jls%canopy_surft)

NULLIFY(wtrac_jls%sthu_soilt)
NULLIFY(wtrac_jls%sthf_soilt)
NULLIFY(wtrac_jls%smcl_soilt)
NULLIFY(wtrac_jls%sthzw_soilt)

NULLIFY(wtrac_jls%tot_surf_runoff_gb)
NULLIFY(wtrac_jls%tot_sub_runoff_gb)
NULLIFY(wtrac_jls%acc_lake_evap_gb)
NULLIFY(wtrac_jls%twatstor)
NULLIFY(wtrac_jls%inlandout_atm_gb)

NULLIFY(wtrac_jls%qw_1_ij)
NULLIFY(wtrac_jls%ls_rain_ij)
NULLIFY(wtrac_jls%con_rain_ij)
NULLIFY(wtrac_jls%ls_snow_ij)
NULLIFY(wtrac_jls%con_snow_ij)
NULLIFY(wtrac_jls%fqw_1)

NULLIFY(wtrac_jls%r_sea)
NULLIFY(wtrac_jls%r_sice)
NULLIFY(wtrac_jls%e_sea_ij)
NULLIFY(wtrac_jls%ei_sice_ij)

NULLIFY(wtrac_jls%fqw_surft)
NULLIFY(wtrac_jls%fqw_evapsrce)
NULLIFY(wtrac_jls%fqw_sea)
NULLIFY(wtrac_jls%fqw_sicat)

NULLIFY(wtrac_jls%ei_surft)
NULLIFY(wtrac_jls%ecan_surft)
NULLIFY(wtrac_jls%esoil_surft)
NULLIFY(wtrac_jls%lake_evap)

NULLIFY(wtrac_jls%ei_ij)
NULLIFY(wtrac_jls%ecan_ij)
NULLIFY(wtrac_jls%esoil_ij_soilt)

NULLIFY(wtrac_jls%dfqw_imp)

NULLIFY(wtrac_jls%smc_soilt)
NULLIFY(wtrac_jls%ext_soilt)

NULLIFY(wtrac_jls%surf_roff_gb)
NULLIFY(wtrac_jls%sub_surf_roff_gb)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE wtrac_jls_nullify
!=============================================================================
END MODULE jules_wtrac_type_mod
