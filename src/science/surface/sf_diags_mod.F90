! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!  ---------------------------------------------------------------------
!  Sructure containing surface exchange diagnostics.
!  This permits easier addition of new surface exchange
!  diagnostics without additional passing of arguments
!  though the boundary layer tree.
!  It also does not require the addition
!  of extra subroutine arguments when adding a new diagnostic.

!  Code Owner: Please refer to ModuleLeaders.txt
!  This file belongs in section: Surface

!- ----------------------------------------------------------------------

MODULE sf_diags_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE
SAVE

TYPE :: strnewsfdiag

  ! Need to create a flag and a pointer.
  ! In general initialise the flag to .FALSE. and use code elsewhere to reset
  ! this if required.
  ! Ordered by UM stash code

  LOGICAL :: l_z0h_eff_gb    = .FALSE. ! 27 effective rough len heat
  LOGICAL :: l_z0m_gb        = .FALSE. ! 28 vegetative rough length mom
  LOGICAL :: l_ra            = .FALSE. ! 54 aerodynamic resistance
  LOGICAL :: l_wt_ext        = .FALSE. ! 55 cumulative soil transp
  LOGICAL :: su10            = .FALSE. ! 209 u10m
  LOGICAL :: sv10            = .FALSE. ! 210 v10m
  LOGICAL :: sfme            = .FALSE. ! 224 wind mixing energy
  LOGICAL :: slh             = .FALSE. ! 234 latent heat
  LOGICAL :: sq_t1p5         = .FALSE. ! lots of things require 236 and 237
  LOGICAL :: st1p5           = .FALSE. ! 236, 328, 545, 547 1.5m temp
  LOGICAL :: sq1p5           = .FALSE. ! 237, 329, 546, 548 1.5m q
  LOGICAL :: simlt           = .FALSE. ! 257 sice mlt heat flux
  LOGICAL :: smlt            = .FALSE. ! 258 snowmelt heat flux
  LOGICAL :: l_gpp           = .FALSE. ! 261 gpp gb mean
  LOGICAL :: l_gpp_pft       = .FALSE. ! 289 gpp on pfts
  LOGICAL :: l_rib_surft     = .FALSE. ! 294 bulk ri on tiles
  LOGICAL :: l_fsmc_pft      = .FALSE. ! 313 soil moisture availability
  LOGICAL :: l_g_leaf_pft    = .FALSE. ! 325 leaf turnover
  LOGICAL :: l_rib_ssi       = .FALSE. ! 339 rib_ssi
  LOGICAL :: l_t10m          = .FALSE. ! 344 t at 10m over sea/sea-ice
  LOGICAL :: l_q10m          = .FALSE. ! 345 q at 10m over sea/sea-ice
  LOGICAL :: suv10m_n        = .FALSE. ! package of neutral winds
  LOGICAL :: l_u10m_n        = .FALSE. ! 368 u10m neutral
  LOGICAL :: l_v10m_n        = .FALSE. ! 369 v10m neutral
  LOGICAL :: l_mu10m_n       = .FALSE. ! 370 x pseudostress
  LOGICAL :: l_mv10m_n       = .FALSE. ! 371 y pseudostress
  LOGICAL :: l_radnet_sea    = .FALSE. ! 380 sea net radiation
  LOGICAL :: l_lw_surft      = .FALSE. ! 383, 384 up and down LW
  LOGICAL :: l_vshr_land     = .FALSE. ! 389 wind shear land
  LOGICAL :: l_vshr_ssi      = .FALSE. ! 390 wind shear sea sea ice
  LOGICAL :: l_ftemp         = .FALSE. ! 485 temp modifier resp
  LOGICAL :: l_fsth          = .FALSE. ! 486 soil moisture modifier resp
  LOGICAL :: l_fprf          = .FALSE. ! 489 veg modifier resp
  LOGICAL :: l_lw_up_sice_weighted_cat = .FALSE. ! 530 sea ice LW up
  LOGICAL :: l_lw_up_sice_weighted     = .FALSE. ! 531 sea ice LW up
  LOGICAL :: l_ftl_ice_sm    = .FALSE. ! 533 sea ice heat flux
  LOGICAL :: l_tstar_sice_weighted_cat = .FALSE. ! 534 sice temp
  LOGICAL :: l_tstar_sice_weighted     = .FALSE. ! 535 sice temp
  LOGICAL :: l_ice_present_cat         = .FALSE. ! 536 ice present
  LOGICAL :: l_ice_present             = .FALSE. ! 537 ice present
  LOGICAL :: l_cd_ssi        = .FALSE. ! 538 cd sea and sea ice
  LOGICAL :: l_et_stom       = .FALSE. ! 539 stom transp
  LOGICAL :: l_et_stom_surft = .FALSE. ! 540 stom transp
  LOGICAL :: l_ch_ssi        = .FALSE. ! 541 ch sea and sea ice
  LOGICAL :: l_lh_land       = .FALSE. ! 543 latent heat land
  LOGICAL :: l_lh_ssi        = .FALSE. ! 544 latent heat sea and sea ice
  LOGICAL :: l_snice         = .FALSE. ! 578-583 (section 8) snow diags
  LOGICAL :: l_blendht       = .FALSE. ! 580 (section 3) blending height
  LOGICAL :: l_resp_p        = .FALSE. ! 663 plant respiration
  LOGICAL :: l_npp_pft       = .FALSE. ! 691 npp on pfts
  LOGICAL :: l_resp_p_pft    = .FALSE. ! 692 plant respiration
  LOGICAL :: l_tau_1         = .FALSE. ! 695 Grid-box mean momentum flux
  LOGICAL :: l_tau_surft     = .FALSE. ! 696 Momentum flux in surface tiles

  REAL(KIND=real_jlslsm), ALLOCATABLE :: z0h_eff_gb(:,:)
  !                    27      effective roughness length for heat
  REAL(KIND=real_jlslsm), ALLOCATABLE :: z0m_gb(:,:)
  !                    28      vegetative roughness length for momentum
  REAL(KIND=real_jlslsm), ALLOCATABLE :: ra(:)
  !                    54      aerodynamic resistance
  REAL(KIND=real_jlslsm), ALLOCATABLE :: wt_ext(:,:)
  !                    55      Culmulative transpiration from soil
  REAL(KIND=real_jlslsm), ALLOCATABLE :: u10m(:,:)
  !                    209      x-cpt of wind at 10m
  REAL(KIND=real_jlslsm), ALLOCATABLE :: v10m(:,:)
  !                    210      y-cpt of wind at 10m
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fme(:,:)
  !                    224      Wind mixing energy
  REAL(KIND=real_jlslsm), ALLOCATABLE :: latent_heat(:,:)
  !                    234      Latent heat flux
  REAL(KIND=real_jlslsm), ALLOCATABLE :: t1p5m(:,:)
  !                    236      Temperature at 1.5m
  REAL(KIND=real_jlslsm), ALLOCATABLE :: q1p5m(:,:)
  !                    237      Specific humidity at 1.5m
  REAL(KIND=real_jlslsm), ALLOCATABLE :: sice_mlt_htf(:,:,:)
  !                    257      Sea ice surface melt heat flux
  REAL(KIND=real_jlslsm), ALLOCATABLE :: snomlt_surf_htf(:,:)
  !                    258      Surface snowmelt heat flux
  REAL(KIND=real_jlslsm), ALLOCATABLE :: gpp(:)
  !                    261      Gross prim prod grid box mean (kg C/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: gpp_pft(:,:)
  !                    289      Gross prim prod on plant func types (kg C/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rib_surft(:,:)
  !                    294      bulk richardson number on land tiles
  REAL(KIND=real_jlslsm), ALLOCATABLE :: t1p5m_surft(:,:)
  !                    328      Temperature at 1.5m over tiles
  REAL(KIND=real_jlslsm), ALLOCATABLE :: q1p5m_surft(:,:)
  !                    329      Specific humidity at 1.5m over tiles
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fsmc_pft(:,:)
  !                    313      Soil moisture availability on pfts
  REAL(KIND=real_jlslsm), ALLOCATABLE :: g_leaf_pft(:,:)
  !                    325      Leaf turnover rate on pfts
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rib_ssi(:,:)
  !                    339      bulk richardson number sea and sea ice
  REAL(KIND=real_jlslsm), ALLOCATABLE :: chr10m(:,:)
  !                    CH at 10m over sea/sea-ice needed for 344/345
  REAL(KIND=real_jlslsm), ALLOCATABLE :: t10m(:,:)
  !                    344      Temperature at 10m over sea/sea-ice
  REAL(KIND=real_jlslsm), ALLOCATABLE :: q10m(:,:)
  !                    345      Specific humidity at 10m over sea/sea-ice
  REAL(KIND=real_jlslsm), ALLOCATABLE :: cdr10m_n(:,:)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: cdr10m_n_u(:,:)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: cdr10m_n_v(:,:)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: cd10m_n(:,:)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: cd10m_n_u(:,:)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: cd10m_n_v(:,:)
  ! coefficients for 10m neutral diagnostics
  REAL(KIND=real_jlslsm), ALLOCATABLE :: u10m_n(:, :)
  !                    368      x-cpt of neutral wind (at 10m)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: v10m_n(:, :)
  !                    369      y-cpt of neutral wind (at 10m)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: mu10m_n(:, :)
  !                    370      x-cpt of pseudostress
  REAL(KIND=real_jlslsm), ALLOCATABLE :: mv10m_n(:, :)
  !                    371      y-cpt of pseudostress
  REAL(KIND=real_jlslsm), ALLOCATABLE :: radnet_sea(:,:)
  !                    380      sea surface net radiation (W/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: lw_up_surft(:,:)
  !                    383      Upwelling LW radiation on tiles
  REAL(KIND=real_jlslsm), ALLOCATABLE :: lw_down_surft(:,:)
  !                    384      Downwelling LW radiation on tiles
  REAL(KIND=real_jlslsm), ALLOCATABLE :: vshr_land(:,:) ! 389 wind shear land
  REAL(KIND=real_jlslsm), ALLOCATABLE :: vshr_ssi(:,:)
                                      ! 390 wind shear sea and sea ice
  REAL(KIND=real_jlslsm), ALLOCATABLE :: ftemp(:,:)
  !                    485      Temperature rate modifier of soil respiration
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fsth(:,:)
  !                    486      Soil moisture rate modifier of soil respiration
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fprf(:)
  !                    489      Soil respiration rate modifier due to vegetation cover
  REAL(KIND=real_jlslsm), ALLOCATABLE :: lw_up_sice_weighted_cat(:,:,:)
  !                    530      category ice area weighted upward LW flux
  !                             over sea ice after boundary layer
  !                             calculation
  REAL(KIND=real_jlslsm), ALLOCATABLE :: lw_up_sice_weighted(:,:)
  !                    531      category ice area weighted upward LW flux
  !                             over sea ice after boundary layer
  !                             calculation
  REAL(KIND=real_jlslsm), ALLOCATABLE :: ftl_ice_sm(:,:)
  !                    533      aggregate ice area weighted sensible
  !                             heat flux over sea ice
  REAL(KIND=real_jlslsm), ALLOCATABLE :: tstar_sice_weighted_cat(:,:,:)
  !                    534      category ice area weighted sea ice surface
  !                             skin temperature
  REAL(KIND=real_jlslsm), ALLOCATABLE :: tstar_sice_weighted(:,:)
  !                    535      category ice area weighted sea ice surface
  !                             skin temperature
  REAL(KIND=real_jlslsm), ALLOCATABLE :: ice_present_cat(:,:,:)
  !                    536      category sea ice time fraction
  REAL(KIND=real_jlslsm), ALLOCATABLE :: ice_present(:,:)
  !                    537      category sea ice time fraction
  REAL(KIND=real_jlslsm), ALLOCATABLE :: cd_ssi(:,:)
  !                    538       Sea and sea ice drag coefficient (momentum)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: et_stom_ij(:,:)
  !                    539      Transpiration through stom (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: et_stom_surft(:,:)
  !                    540      Transpiration through stom (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: resfs_stom(:,:)
  !                    Combined stomatal and aerodynamic
  !                    resistance factor for fraction 1-fracaero.
  REAL(KIND=real_jlslsm), ALLOCATABLE :: ch_ssi(:,:)
  !                    541       Sea and sea ice drag coefficient (heat)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: lh_land(:)
  !                    543       Land latent heat flux
  REAL(KIND=real_jlslsm), ALLOCATABLE :: lh_ssi(:,:)
  !                    544       Sea and sea ice mean latent heat
  REAL(KIND=real_jlslsm), ALLOCATABLE :: t1p5m_ssi(:,:)
  !                    545      Temperature at 1.5m over sea/sea-ice
  REAL(KIND=real_jlslsm), ALLOCATABLE :: q1p5m_ssi(:,:)
  !                    546      Specific humidity at 1.5m over sea/sea-ice
  REAL(KIND=real_jlslsm), ALLOCATABLE :: snice_smb_surft(:,:)
  !                    578      Tiled snow mass rate of change
  !                             (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: snice_m_surft(:,:)
  !                    579      Total internal melt rate of snowpack
  !                             (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: snice_freez_surft(:,:)
  !                    580 (section 8) Total internal refreezing rate in
  !                             snowpack (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: snice_runoff_surft(:,:)
  !                    581 (section 8) Net rate of liquid leaving snowpack
  !                             (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: snice_sicerate_surft(:,:)
  !                    582 (section 8) Rate of change of solid mass in
  !                             snowpack (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: snice_sliqrate_surft(:,:)
  !                    583 (section 8) Rate of change of liquid mass
  !                             snowpack (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: blendht(:,:)
  !                    580 (section 3) blending height
  REAL(KIND=real_jlslsm), ALLOCATABLE :: resp_p(:)
  !                    663      Plant respiration (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: npp_pft(:,:)
  !                    691      Net primary productivity on PFTs (kg C/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: resp_p_pft(:,:)
  !                    692      Plant respiration on PFTs (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: tau_1(:,:)
  !                    695      Gridbox mean surface momentum flux on P-grid
  !                             (N/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: tau_surft(:,:)
  !                    695      Tiled surface momentum flux on P-grid
  !                             (N/m2)

END TYPE strnewsfdiag

TYPE (Strnewsfdiag) :: sf_diag

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName = 'SF_DIAGS_MOD'
! ----------------------------------------------------------------------
#if defined(UM_JULES)
CONTAINS

! allocation of variables for the explicit part of surface code
! ordered by UM stash code
SUBROUTINE alloc_sf_expl(sf_diag, l_apply_diag, land_points)

USE atm_fields_bounds_mod, ONLY: udims, vdims, pdims, tdims, pdims_s
USE jules_soil_mod, ONLY: sm_levels
USE jules_surface_types_mod, ONLY: npft
USE model_domain_mod, ONLY: model_type, mt_single_column, mt_lfric
USE nlsizes_namelist_mod, ONLY: ntiles
USE stash_array_mod, ONLY: sf
USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

IMPLICIT NONE

TYPE (strnewsfdiag), INTENT(IN OUT) :: sf_diag
LOGICAL, INTENT(IN) :: l_apply_diag
INTEGER, INTENT(IN) :: land_points

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='ALLOC_SF_EXPL'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! set the logical switches for whether diagnostic is requested
SELECT CASE (model_type)
CASE DEFAULT

  ! N.B. some of these are duplicated in alloc_sf_imp, because l_apply_diag
  ! will have changed by then
  sf_diag%l_z0h_eff_gb = (l_apply_diag .AND. sf(027,3))
  sf_diag%l_z0m_gb     = (l_apply_diag .AND. sf(028,3))
  sf_diag%l_ra         = (l_apply_diag .AND. sf(054,3))
  sf_diag%l_wt_ext     = (l_apply_diag .AND. sf(055,3))
  sf_diag%su10         = (l_apply_diag .AND.                                   &
                         ( sf(209,3) .OR. sf(225,3) .OR. sf(227,3) .OR.        &
                           sf(230,3) .OR. sf(463,3) .OR. sf(515,3) ))
  sf_diag%sv10         = (l_apply_diag .AND.                                   &
                         ( sf(210,3) .OR. sf(226,3) .OR. sf(227,3) .OR.        &
                           sf(230,3) .OR. sf(463,3) .OR. sf(515,3) ))
  sf_diag%sfme         = (l_apply_diag .AND. sf(224,3))
  sf_diag%l_gpp        = l_apply_diag .AND. sf(261,3)
  sf_diag%l_gpp_pft    = l_apply_diag .AND. sf(289,3)
  sf_diag%sq_t1p5      = (l_apply_diag .AND.                                   &
                         ( sf(236,3) .OR. sf(237,3) .OR. sf(245,3) .OR.        &
                           sf(247,3) .OR. sf(248,3) .OR. sf(250,3) .OR.        &
                           sf(341,3) .OR. sf(342,3) .OR. sf(253,3) .OR.        &
                           sf(328,3) .OR. sf(329,3) .OR.                       &
                           sf(281,3) .OR. sf(282,3) .OR. sf(283,3) .OR.        &
                           sf(545,3) .OR. sf(546,3) .OR.                       &
                           sf(547,3) .OR. sf(548,3) .OR.                       &
                           sf(570,3) .OR. sf(571,3) .OR. sf(572,3) .OR.        &
                           sf(573,3) .OR. sf(574,3) .OR. sf(575,3) .OR.        &
                           sf(576,3) .OR. sf(577,3)))
  sf_diag%l_blendht     = (l_apply_diag .AND. sf(580,3))
  sf_diag%st1p5         = (l_apply_diag .AND. sf_diag%sq_t1p5)
  sf_diag%sq1p5         = (l_apply_diag .AND. sf_diag%sq_t1p5)
  sf_diag%l_rib_surft   = l_apply_diag .AND. (sf(294,3) .OR. sf(339,3))
  sf_diag%l_fsmc_pft    = l_apply_diag .AND. sf(313,3)
  sf_diag%l_g_leaf_pft  = l_apply_diag .AND. sf(325,3)
  sf_diag%l_rib_ssi     = l_apply_diag .AND. sf(339,3)
  sf_diag%l_t10m        = (l_apply_diag .AND. sf(344,3))
  sf_diag%l_q10m        = (l_apply_diag .AND. sf(345,3))
  sf_diag%suv10m_n = ( sf(368,3) .OR. sf(369,3) .OR.                           &
                       sf(370,3) .OR. sf(371,3) .OR.                           &
                       sf(365,3) .OR. sf(366,3) .OR. sf(367,3) ) .AND.         &
                       l_apply_diag
  ! x-cpt of 10 m neutral wind
  sf_diag%l_u10m_n      = (l_apply_diag .AND. sf_diag%suv10m_n)
  ! y-cpt of 10 m neutral wind
  sf_diag%l_v10m_n      = (l_apply_diag .AND. sf_diag%suv10m_n)
  ! x-cpt of 10 m pseudostress
  sf_diag%l_mu10m_n     = (l_apply_diag .AND. sf_diag%suv10m_n)
  ! y-cpt of 10 m pseudostress
  sf_diag%l_mv10m_n     = (l_apply_diag .AND. sf_diag%suv10m_n)
  sf_diag%l_radnet_sea  = l_apply_diag .AND. sf(380,3)
  sf_diag%l_vshr_land   = (l_apply_diag .AND. sf(389,3))
  sf_diag%l_vshr_ssi    = (l_apply_diag .AND. sf(390,3))
  sf_diag%l_ftemp       = (l_apply_diag .AND. sf(485,3))
  sf_diag%l_fsth        = (l_apply_diag .AND. sf(486,3))
  sf_diag%l_fprf        = (l_apply_diag .AND. sf(489,3))
  sf_diag%l_cd_ssi      = (l_apply_diag .AND. sf(538,3))
  sf_diag%l_et_stom     = (l_apply_diag .AND. sf(539,3))
  sf_diag%l_et_stom_surft = (l_apply_diag .AND. sf(540,3))
  sf_diag%l_ch_ssi      = (l_apply_diag .AND. sf(541,3))
  sf_diag%l_resp_p      = l_apply_diag .AND. sf(663,3)
  sf_diag%l_npp_pft     = l_apply_diag .AND. sf(691,3)
  sf_diag%l_resp_p_pft  = l_apply_diag .AND. sf(692,3)
  sf_diag%l_tau_1       = l_apply_diag .AND. sf(695,3)
  sf_diag%l_tau_surft   = l_apply_diag .AND. sf(696,3)

CASE (mt_single_column)
  sf_diag%l_z0h_eff_gb = .TRUE.
  sf_diag%l_z0m_gb  = .TRUE.
  sf_diag%su10    = .TRUE.
  sf_diag%sv10    = .TRUE.
  sf_diag%l_gpp   = .TRUE.
  sf_diag%st1p5   = .TRUE.
  sf_diag%sq1p5   = .TRUE.
  sf_diag%l_resp_p = .TRUE.

CASE (mt_lfric)

  ! Requests are configured in LFRic

END SELECT

! allocate space for the diagnostic if requested
IF (sf_diag%l_z0h_eff_gb) THEN
  ALLOCATE(sf_diag%z0h_eff_gb(                                                 &
       pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
ELSE
  ALLOCATE(sf_diag%z0h_eff_gb(1,1))
END IF
IF (sf_diag%l_z0m_gb) THEN
  ALLOCATE(sf_diag%z0m_gb(                                                     &
       pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
ELSE
  ALLOCATE(sf_diag%z0m_gb(1,1))
END IF
IF (sf_diag%l_blendht) THEN
  ALLOCATE(sf_diag%blendht(                                                    &
       pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
  sf_diag%blendht(:,:) = 0.0
ELSE
  ALLOCATE(sf_diag%blendht(1,1))
END IF
IF (sf_diag%l_ra) THEN
  ALLOCATE(sf_diag%ra(land_points))
ELSE
  ALLOCATE(sf_diag%ra(1))
END IF
IF (sf_diag%l_wt_ext) THEN
  ALLOCATE(sf_diag%wt_ext(land_points,sm_levels))
ELSE
  ALLOCATE(sf_diag%wt_ext(1,1))
END IF
IF (sf_diag%sfme) THEN
  ALLOCATE(sf_diag%fme(                                                        &
       pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
  sf_diag%fme(:,:) = 0.0
ELSE
  ALLOCATE(sf_diag%fme(1,1))
END IF
IF (sf_diag%l_gpp) THEN
  ALLOCATE( sf_diag%gpp(land_points))
ELSE
  ALLOCATE( sf_diag%gpp(1))
END IF
IF (sf_diag%l_gpp_pft) THEN
  ALLOCATE( sf_diag%gpp_pft(land_points,npft))
ELSE
  ALLOCATE( sf_diag%gpp_pft(1,1))
END IF
IF (sf_diag%l_rib_surft) THEN
  ALLOCATE( sf_diag%rib_surft(land_points,ntiles))
ELSE
  ALLOCATE( sf_diag%rib_surft(1,1))
END IF
IF (sf_diag%l_fsmc_pft) THEN
  ALLOCATE( sf_diag%fsmc_pft(land_points,npft))
ELSE
  ALLOCATE( sf_diag%fsmc_pft(1,1))
END IF
IF (sf_diag%l_g_leaf_pft) THEN
  ALLOCATE( sf_diag%g_leaf_pft(land_points,npft))
ELSE
  ALLOCATE( sf_diag%g_leaf_pft(1,1))
END IF
IF (sf_diag%l_rib_ssi) THEN
  ALLOCATE( sf_diag%rib_ssi(tdims%i_start:tdims%i_end,                         &
                            tdims%j_start:tdims%j_end))
ELSE
  ALLOCATE( sf_diag%rib_ssi(1,1))
END IF
! 10m t and q diagnostics over sea/sea-ice
IF (sf_diag%l_t10m .OR. sf_diag%l_q10m) THEN
  ALLOCATE( sf_diag%chr10m(tdims%i_start:tdims%i_end,                          &
                           tdims%j_start:tdims%j_end))
ELSE
  ALLOCATE( sf_diag%chr10m(1,1))
END IF
IF (sf_diag%suv10m_n) THEN
  ALLOCATE(sf_diag%cdr10m_n(pdims_s%i_start:pdims_s%i_end,                     &
                            pdims_s%j_start:pdims_s%j_end))
  ALLOCATE(sf_diag%cdr10m_n_u(udims%i_start:udims%i_end,                       &
                              udims%j_start:udims%j_end))
  ALLOCATE(sf_diag%cdr10m_n_v(vdims%i_start:vdims%i_end,                       &
                              vdims%j_start:vdims%j_end))
  ALLOCATE(sf_diag%cd10m_n(pdims_s%i_start:pdims_s%i_end,                      &
                           pdims_s%j_start:pdims_s%j_end))
  ALLOCATE(sf_diag%cd10m_n_u(udims%i_start:udims%i_end,                        &
                             udims%j_start:udims%j_end))
  ALLOCATE(sf_diag%cd10m_n_v(vdims%i_start:vdims%i_end,                        &
                             vdims%j_start:vdims%j_end))
ELSE
  ALLOCATE(sf_diag%cdr10m_n(1,1))
  ALLOCATE(sf_diag%cdr10m_n_u(1,1))
  ALLOCATE(sf_diag%cdr10m_n_v(1,1))
  ALLOCATE(sf_diag%cd10m_n(1,1))
  ALLOCATE(sf_diag%cd10m_n_u(1,1))
  ALLOCATE(sf_diag%cd10m_n_v(1,1))
END IF
IF (sf_diag%l_u10m_n) THEN
  ALLOCATE(sf_diag%u10m_n(udims%i_start:udims%i_end,                           &
       udims%j_start:udims%j_end))
ELSE
  ALLOCATE(sf_diag%u10m_n(1,1))
END IF
IF (sf_diag%l_v10m_n) THEN
  ALLOCATE(sf_diag%v10m_n(vdims%i_start:vdims%i_end,                           &
       vdims%j_start:vdims%j_end))
ELSE
  ALLOCATE(sf_diag%v10m_n(1,1))
END IF
IF (sf_diag%l_mu10m_n) THEN
  ALLOCATE(sf_diag%mu10m_n(udims%i_start:udims%i_end,                          &
       udims%j_start:udims%j_end))
ELSE
  ALLOCATE(sf_diag%mu10m_n(1,1))
END IF
IF (sf_diag%l_mv10m_n) THEN
  ALLOCATE(sf_diag%mv10m_n(vdims%i_start:vdims%i_end,                          &
       vdims%j_start:vdims%j_end))
ELSE
  ALLOCATE(sf_diag%mv10m_n(1,1))
END IF
IF (sf_diag%l_radnet_sea) THEN
  ALLOCATE(sf_diag%radnet_sea(                                                 &
       pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
ELSE
  ALLOCATE(sf_diag%radnet_sea(1,1))
END IF
IF (sf_diag%l_vshr_land) THEN
  ALLOCATE(sf_diag%vshr_land(                                                  &
       pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
ELSE
  ALLOCATE(sf_diag%vshr_land(1,1))
END IF
IF (sf_diag%l_vshr_ssi) THEN
  ALLOCATE(sf_diag%vshr_ssi(                                                   &
       pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
ELSE
  ALLOCATE(sf_diag%vshr_ssi(1,1))
END IF
! Soil respiration rate modifiers due to temperature, moisture and
! vegetation cover: ftemp, fsth and fprf
IF (sf_diag%l_ftemp) THEN
  ALLOCATE(sf_diag%ftemp(land_points,sm_levels))
ELSE
  ALLOCATE(sf_diag%ftemp(1,1))
END IF
IF (sf_diag%l_fsth) THEN
  ALLOCATE(sf_diag%fsth(land_points,sm_levels))
ELSE
  ALLOCATE(sf_diag%fsth(1,1))
END IF
IF (sf_diag%l_fprf) THEN
  ALLOCATE(sf_diag%fprf(land_points))
ELSE
  ALLOCATE(sf_diag%fprf(1))
END IF
! Sea and sea ice drag coefficient (momentum)
IF (sf_diag%l_cd_ssi) THEN
  ALLOCATE( sf_diag%cd_ssi(tdims%i_start:tdims%i_end,                          &
                           tdims%j_start:tdims%j_end))
ELSE
  ALLOCATE( sf_diag%cd_ssi(1,1))
END IF
IF (sf_diag%l_et_stom .OR. sf_diag%l_et_stom_surft) THEN
  ALLOCATE(sf_diag%et_stom_ij(                                                 &
       pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
  ALLOCATE(sf_diag%et_stom_surft(land_points,ntiles))
  ALLOCATE(sf_diag%resfs_stom(land_points,ntiles))
ELSE
  ALLOCATE(sf_diag%et_stom_ij(1,1))
  ALLOCATE(sf_diag%et_stom_surft(1,1))
  ALLOCATE(sf_diag%resfs_stom(1,1))
END IF
! Sea and sea ice drag coefficient (heat)
IF (sf_diag%l_ch_ssi) THEN
  ALLOCATE( sf_diag%ch_ssi(tdims%i_start:tdims%i_end,                          &
                           tdims%j_start:tdims%j_end))
ELSE
  ALLOCATE( sf_diag%ch_ssi(1,1))
END IF
IF (sf_diag%l_resp_p) THEN
  ALLOCATE(sf_diag%resp_p(land_points))
ELSE
  ALLOCATE(sf_diag%resp_p(1))
END IF
IF (sf_diag%l_npp_pft) THEN
  ALLOCATE( sf_diag%npp_pft(land_points,npft))
ELSE
  ALLOCATE( sf_diag%npp_pft(1,1))
END IF
IF (sf_diag%l_resp_p_pft) THEN
  ALLOCATE(sf_diag%resp_p_pft(land_points,npft))
ELSE
  ALLOCATE(sf_diag%resp_p_pft(1,1))
END IF
IF (sf_diag%l_tau_1) THEN
  ALLOCATE(sf_diag%tau_1(tdims%i_start:tdims%i_end,                            &
                           tdims%j_start:tdims%j_end))
ELSE
  ALLOCATE(sf_diag%tau_1(1,1))
END IF
IF (sf_diag%l_tau_surft) THEN
  ALLOCATE(sf_diag%tau_surft(land_points,ntiles))
ELSE
  ALLOCATE(sf_diag%tau_surft(1,1))
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE alloc_sf_expl

! deallocation of variables from explicit part of surface code
! in reverse order from which they were allocated
SUBROUTINE dealloc_sf_expl(sf_diag)

IMPLICIT NONE

TYPE (strnewsfdiag), INTENT(IN OUT) :: sf_diag

DEALLOCATE(sf_diag%tau_surft)
DEALLOCATE(sf_diag%tau_1)
DEALLOCATE(sf_diag%resp_p_pft)
DEALLOCATE(sf_diag%npp_pft)
DEALLOCATE(sf_diag%resp_p)
DEALLOCATE(sf_diag%ch_ssi)
DEALLOCATE(sf_diag%resfs_stom)
DEALLOCATE(sf_diag%et_stom_surft)
DEALLOCATE(sf_diag%et_stom_ij)
DEALLOCATE(sf_diag%cd_ssi)
DEALLOCATE(sf_diag%fprf)
DEALLOCATE(sf_diag%fsth)
DEALLOCATE(sf_diag%ftemp)
DEALLOCATE(sf_diag%vshr_ssi)
DEALLOCATE(sf_diag%vshr_land)
DEALLOCATE(sf_diag%radnet_sea)
DEALLOCATE(sf_diag%mv10m_n)
DEALLOCATE(sf_diag%mu10m_n)
DEALLOCATE(sf_diag%v10m_n)
DEALLOCATE(sf_diag%u10m_n)
DEALLOCATE(sf_diag%cd10m_n_v)
DEALLOCATE(sf_diag%cd10m_n_u)
DEALLOCATE(sf_diag%cd10m_n)
DEALLOCATE(sf_diag%cdr10m_n_v)
DEALLOCATE(sf_diag%cdr10m_n_u)
DEALLOCATE(sf_diag%cdr10m_n)
DEALLOCATE(sf_diag%chr10m)
DEALLOCATE(sf_diag%rib_ssi)
DEALLOCATE(sf_diag%g_leaf_pft)
DEALLOCATE(sf_diag%fsmc_pft)
DEALLOCATE(sf_diag%rib_surft)
DEALLOCATE(sf_diag%gpp_pft)
DEALLOCATE(sf_diag%gpp)
DEALLOCATE(sf_diag%fme)
DEALLOCATE(sf_diag%wt_ext)
DEALLOCATE(sf_diag%ra)
DEALLOCATE(sf_diag%blendht)
DEALLOCATE(sf_diag%z0m_gb)
DEALLOCATE(sf_diag%z0h_eff_gb)

RETURN

END SUBROUTINE dealloc_sf_expl

! allocation of variables for the implicit part of surface code
! ordered by UM stash code
SUBROUTINE alloc_sf_imp(sf_diag, l_apply_diag, land_points)

USE atm_fields_bounds_mod, ONLY: udims, vdims, pdims, tdims
USE jules_sea_seaice_mod, ONLY: nice, nice_use
USE jules_surface_mod, ONLY: IScrnTDiag, IP_ScrnDecpl2, IP_ScrnDecpl3
USE model_domain_mod, ONLY: model_type, mt_single_column, mt_lfric
USE nlsizes_namelist_mod, ONLY: ntiles
USE stash_array_mod, ONLY: sf
USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

IMPLICIT NONE

TYPE (strnewsfdiag), INTENT(IN OUT) :: sf_diag
LOGICAL, INTENT(IN) :: l_apply_diag
INTEGER, INTENT(IN) :: land_points

INTEGER :: i,j

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='ALLOC_SF_IMP'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! set the logical switches for whether diagnostic is requested
SELECT CASE (model_type)
CASE DEFAULT

  ! N.B. some of these are duplicated in alloc_sf_expl, because l_apply_diag
  ! will have changed from then
  sf_diag%su10    = ((sf(209,3) .OR. sf(225,3) .OR. sf(227,3) .OR.             &
       sf(230,3) .OR. sf(463,3) .OR. sf(515,3) ) .AND. l_apply_diag)
  sf_diag%sv10    = ((sf(210,3) .OR. sf(226,3) .OR. sf(227,3) .OR.             &
       sf(230,3) .OR. sf(463,3) .OR. sf(515,3) ) .AND. l_apply_diag)
  sf_diag%slh     =  sf(234,3) .AND. l_apply_diag
  sf_diag%simlt   = (sf(235,3) .OR. sf(257,3) ) .AND. l_apply_diag
  sf_diag%sq_t1p5 = (sf(236,3) .OR. sf(237,3) .OR. sf(245,3) .OR.              &
       sf(247,3) .OR. sf(248,3) .OR. sf(250,3) .OR.                            &
       sf(341,3) .OR. sf(342,3) .OR.                                           &
       sf(253,3) .OR. sf(328,3) .OR. sf(329,3) .OR.                            &
       sf(281,3) .OR. sf(282,3) .OR. sf(283,3) .OR. sf(545,3) .OR.             &
       sf(546,3) .OR. sf(547,3) .OR. sf(548,3) .OR. sf(570,3) .OR.             &
       sf(571,3) .OR. sf(572,3) .OR. sf(573,3) .OR. sf(574,3) .OR.             &
       sf(575,3) .OR. sf(576,3) .OR. sf(577,3)) .AND. l_apply_diag
  sf_diag%sq1p5   = sf_diag%sq_t1p5 .AND. l_apply_diag
  sf_diag%st1p5   = ((sf_diag%sq_t1p5 .OR. sf(355,3) .OR. sf(463,3)            &
       .OR. sf(515,3) ) .AND. l_apply_diag)
  sf_diag%smlt    =  sf(258,3) .AND. l_apply_diag
  sf_diag%l_t10m  = sf(344,3) .AND. l_apply_diag
  sf_diag%l_q10m  = sf(345,3) .AND. l_apply_diag
  sf_diag%suv10m_n = (sf(368,3) .OR. sf(369,3) .OR. sf(370,3) .OR.             &
       sf(371,3) .OR. sf(365,3) .OR. sf(366,3) .OR.                            &
       sf(367,3) ) .AND. l_apply_diag
  sf_diag%l_lw_surft = (sf(383,3) .OR. sf(384,3)) .AND. l_apply_diag

  ! New sea ice diagnostics for CMIP6
  sf_diag%l_lw_up_sice_weighted_cat = (l_apply_diag .AND. sf(530,3))
  sf_diag%l_lw_up_sice_weighted     = (l_apply_diag .AND. sf(531,3))
  sf_diag%l_ftl_ice_sm      = (l_apply_diag .AND. sf(533,3))
  sf_diag%l_tstar_sice_weighted_cat = (l_apply_diag .AND. sf(534,3))
  sf_diag%l_tstar_sice_weighted     = (l_apply_diag .AND. sf(535,3))
  sf_diag%l_ice_present_cat = (l_apply_diag .AND. sf(536,3))
  sf_diag%l_ice_present     = (l_apply_diag .AND. sf(537,3))

  sf_diag%l_lh_land     = (l_apply_diag .AND. sf(543,3))
  sf_diag%l_lh_ssi      = (l_apply_diag .AND. sf(544,3))

CASE (mt_single_column)

  sf_diag%su10    = .TRUE.        ! Calculate u10m
  sf_diag%sv10    = .TRUE.        !    "      v10m
  sf_diag%slh     = .TRUE.        !    "      latent_heat
  sf_diag%sq1p5   = .TRUE.        !    "      q1p5m
  sf_diag%st1p5   = .TRUE.        !    "      t1p5m
  sf_diag%simlt   = .TRUE.        !    "      sice_mlt_htf
  sf_diag%smlt    = .TRUE.        !    "      snomlt_surf_htf
  sf_diag%l_lw_surft = .TRUE.     !    "      lw_up_surft & lw_down_surft
  sf_diag%l_lw_up_sice_weighted_cat = .TRUE.  ! " lw_up_sice_weighted_cat

CASE (mt_lfric)

  ! Requests are configured in LFRic

END SELECT

IF (sf_diag%su10) THEN
  ALLOCATE(sf_diag%u10m(udims%i_start:udims%i_end,                             &
                        udims%j_start:udims%j_end))
ELSE
  ALLOCATE(sf_diag%u10m(1,1))
END IF

IF (sf_diag%sv10) THEN
  ALLOCATE(sf_diag%v10m(vdims%i_start:vdims%i_end,                             &
                        vdims%j_start:vdims%j_end))
ELSE
  ALLOCATE(sf_diag%v10m(1,1))
END IF

IF (sf_diag%slh) THEN
  ALLOCATE(sf_diag%latent_heat(pdims%i_start:pdims%i_end,                      &
                               pdims%j_start:pdims%j_end))
ELSE
  ALLOCATE(sf_diag%latent_heat(1,1))
END IF

IF (sf_diag%simlt) THEN
  ALLOCATE(sf_diag%sice_mlt_htf(pdims%i_start:pdims%i_end,                     &
                                pdims%j_start:pdims%j_end, nice))
ELSE
  ALLOCATE(sf_diag%sice_mlt_htf(1,1,1))
END IF

IF (sf_diag%st1p5 .OR. (IScrnTDiag == IP_ScrnDecpl2)                           &
     .OR. (IScrnTDiag == IP_ScrnDecpl3) ) THEN
  ALLOCATE(sf_diag%t1p5m(pdims%i_start:pdims%i_end,                            &
                         pdims%j_start:pdims%j_end))
  ALLOCATE(sf_diag%t1p5m_ssi(pdims%i_start:pdims%i_end,                        &
                         pdims%j_start:pdims%j_end))
  ALLOCATE(sf_diag%t1p5m_surft(land_points,ntiles))
ELSE
  ALLOCATE(sf_diag%t1p5m(1,1))
  ALLOCATE(sf_diag%t1p5m_ssi(1,1))
  ALLOCATE(sf_diag%t1p5m_surft(1,1))
END IF

IF (sf_diag%sq1p5 .OR. (IScrnTDiag == IP_ScrnDecpl2)                           &
     .OR. (IScrnTDiag == IP_ScrnDecpl3) ) THEN
  ALLOCATE(sf_diag%q1p5m(pdims%i_start:pdims%i_end,                            &
                         pdims%j_start:pdims%j_end))
  ALLOCATE(sf_diag%q1p5m_ssi(pdims%i_start:pdims%i_end,                        &
                         pdims%j_start:pdims%j_end))
  ALLOCATE(sf_diag%q1p5m_surft(land_points,ntiles))
ELSE
  ALLOCATE(sf_diag%q1p5m(1,1))
  ALLOCATE(sf_diag%q1p5m_ssi(1,1))
  ALLOCATE(sf_diag%q1p5m_surft(1,1))
END IF

IF (sf_diag%smlt) THEN
  ALLOCATE(sf_diag%snomlt_surf_htf(pdims%i_start:pdims%i_end,                  &
                                   pdims%j_start:pdims%j_end))
ELSE
  ALLOCATE(sf_diag%snomlt_surf_htf(1,1))
END IF
! 10m t and q diagnostics over sea/sea-ice
! both are required if one is requested due to ls_cld call in diag_bl
IF (sf_diag%l_t10m .OR. sf_diag%l_q10m) THEN
  ALLOCATE( sf_diag%t10m(tdims%i_start:tdims%i_end,                            &
                         tdims%j_start:tdims%j_end))
  ALLOCATE( sf_diag%q10m(tdims%i_start:tdims%i_end,                            &
                         tdims%j_start:tdims%j_end))
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(j,i)                  &
!$OMP SHARED(tdims,sf_diag)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      sf_diag%t10m(i,j) = 0.0
      sf_diag%q10m(i,j) = 0.0
    END DO
  END DO
!$OMP END PARALLEL DO
ELSE
  ALLOCATE( sf_diag%t10m(1,1))
  ALLOCATE( sf_diag%q10m(1,1))
END IF

IF (sf_diag%l_lw_surft ) THEN
  ALLOCATE(sf_diag%lw_up_surft(land_points,ntiles))
  ALLOCATE(sf_diag%lw_down_surft(land_points,ntiles))
ELSE
  ALLOCATE(sf_diag%lw_up_surft(1,1))
  ALLOCATE(sf_diag%lw_down_surft(1,1))
END IF

IF (sf_diag%l_lw_up_sice_weighted_cat) THEN
  ALLOCATE(sf_diag%lw_up_sice_weighted_cat(pdims%i_start:pdims%i_end,          &
                                           pdims%j_start:pdims%j_end,nice_use))
ELSE
  ALLOCATE(sf_diag%lw_up_sice_weighted_cat(1,1,1))
END IF

IF (sf_diag%l_lw_up_sice_weighted) THEN
  ALLOCATE(sf_diag%lw_up_sice_weighted(pdims%i_start:pdims%i_end,              &
                                       pdims%j_start:pdims%j_end))
ELSE
  ALLOCATE(sf_diag%lw_up_sice_weighted(1,1))
END IF
IF (sf_diag%l_ftl_ice_sm) THEN
  ALLOCATE(sf_diag%ftl_ice_sm(pdims%i_start:pdims%i_end,                       &
                              pdims%j_start:pdims%j_end))
ELSE
  ALLOCATE(sf_diag%ftl_ice_sm(1,1))
END IF
IF (sf_diag%l_tstar_sice_weighted_cat) THEN
  ALLOCATE(sf_diag%tstar_sice_weighted_cat(pdims%i_start:pdims%i_end,          &
                                           pdims%j_start:pdims%j_end,nice_use))
ELSE
  ALLOCATE(sf_diag%tstar_sice_weighted_cat(1,1,1))
END IF

IF (sf_diag%l_tstar_sice_weighted) THEN
  ALLOCATE(sf_diag%tstar_sice_weighted(pdims%i_start:pdims%i_end,              &
                                       pdims%j_start:pdims%j_end))
ELSE
  ALLOCATE(sf_diag%tstar_sice_weighted(1,1))
END IF

IF (sf_diag%l_ice_present_cat) THEN
  ALLOCATE(sf_diag%ice_present_cat(pdims%i_start:pdims%i_end,                  &
                                   pdims%j_start:pdims%j_end,nice_use))
ELSE
  ALLOCATE(sf_diag%ice_present_cat(1,1,1))
END IF

IF (sf_diag%l_ice_present) THEN
  ALLOCATE(sf_diag%ice_present(pdims%i_start:pdims%i_end,                      &
                               pdims%j_start:pdims%j_end))
ELSE
  ALLOCATE(sf_diag%ice_present(1,1))
END IF

IF (sf_diag%l_lh_land) THEN
  ALLOCATE( sf_diag%lh_land(land_points))
ELSE
  ALLOCATE( sf_diag%lh_land(1))
END IF
IF (sf_diag%l_lh_ssi) THEN
  ALLOCATE( sf_diag%lh_ssi(tdims%i_start:tdims%i_end,                          &
                           tdims%j_start:tdims%j_end))
ELSE
  ALLOCATE( sf_diag%lh_ssi(1,1))
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE alloc_sf_imp

! deallocation of variables from implicit part of surface code
! in reverse order from which they were allocated
SUBROUTINE dealloc_sf_imp(sf_diag)

IMPLICIT NONE

TYPE (strnewsfdiag), INTENT(IN OUT) :: sf_diag

DEALLOCATE(sf_diag%lh_ssi)
DEALLOCATE(sf_diag%lh_land)
DEALLOCATE(sf_diag%ice_present)
DEALLOCATE(sf_diag%ice_present_cat)
DEALLOCATE(sf_diag%tstar_sice_weighted)
DEALLOCATE(sf_diag%tstar_sice_weighted_cat)
DEALLOCATE(sf_diag%ftl_ice_sm)
DEALLOCATE(sf_diag%lw_up_sice_weighted)
DEALLOCATE(sf_diag%lw_up_sice_weighted_cat)
DEALLOCATE(sf_diag%lw_down_surft)
DEALLOCATE(sf_diag%lw_up_surft)
DEALLOCATE(sf_diag%q10m)
DEALLOCATE(sf_diag%t10m)
DEALLOCATE(sf_diag%snomlt_surf_htf)
DEALLOCATE(sf_diag%q1p5m_surft)
DEALLOCATE(sf_diag%q1p5m_ssi)
DEALLOCATE(sf_diag%q1p5m)
DEALLOCATE(sf_diag%t1p5m_surft)
DEALLOCATE(sf_diag%t1p5m_ssi)
DEALLOCATE(sf_diag%t1p5m)
DEALLOCATE(sf_diag%sice_mlt_htf)
DEALLOCATE(sf_diag%latent_heat)
DEALLOCATE(sf_diag%v10m)
DEALLOCATE(sf_diag%u10m)

RETURN

END SUBROUTINE dealloc_sf_imp
#endif

END MODULE sf_diags_mod
