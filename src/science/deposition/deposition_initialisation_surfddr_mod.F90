!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE deposition_initialisation_surfddr_mod

! ------------------------------------------------------------------------------
! Description:
!   Initialisation of the standard surface resistance terms.
!   Based on UKCA routine
!     ukca/src/science/chemistry/deposition/ukca_surfddr.F90
!   Returns rsurf
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGENIC FLUXES.
!
! Code Description:
!   Language: Fortran 90.
!-----------------------------------------------------------------------------

! The following UKCA modules have been replaced:
! (1) asad_mod for ndepd, speci, nldepd, jpdd
!     Use ndry_dep_species and dep_species_name
! (2) ukca_config_specification_mod for ukca_config
!     Use jules_deposition_mod for UKCA deposition switches

USE deposition_species_mod,  ONLY: dep_species_name

USE jules_deposition_mod,    ONLY: ndry_dep_species,                           &
                                   l_ukca_dry_dep_so2wet,                      &
                                   l_ukca_emsdrvn_ch4,                         &
                                   dep_rnull

USE jules_science_fixes_mod, ONLY: l_fix_improve_drydep,                       &
                                   l_fix_ukca_h2dd_x,                          &
                                   l_fix_drydep_so2_water

USE jules_surface_types_mod, ONLY: ntype

#if defined(UM_JULES)
USE UM_ParCore, ONLY: mype
#endif

USE jules_print_mgr,         ONLY: jules_print, jules_message

USE ereport_mod,             ONLY:                                             &
    ereport

USE c_rmol,                  ONLY: rmol       ! Universal gas constant
USE um_types,                ONLY: real_jlslsm
USE parkind1,                ONLY: jprb, jpim
USE yomhook,                 ONLY: lhook, dr_hook


IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName = 'DEPOSITION_INITIALISATION_SURFDDR_MOD'

! INTEGER and REAL parameters
! Indices in r_stom for species with stomatal resistance
INTEGER, PARAMETER :: r_stom_spec = 5 ! NO2 , O3 , PAN , SO2, NH3
INTEGER, PARAMETER :: r_stom_no2  = 1
INTEGER, PARAMETER :: r_stom_o3   = 2
INTEGER, PARAMETER :: r_stom_pan  = 3
INTEGER, PARAMETER :: r_stom_so2  = 4
INTEGER, PARAMETER :: r_stom_nh3  = 5

! Resistances for Tundra if different to standard value in rsurf().
INTEGER, PARAMETER :: r_tundra_spec = 5 ! NO2 , CO , O3 , H2, PAN
INTEGER, PARAMETER :: r_tundra_no2  = 1
INTEGER, PARAMETER :: r_tundra_co   = 2
INTEGER, PARAMETER :: r_tundra_o3   = 3
INTEGER, PARAMETER :: r_tundra_h2   = 4
INTEGER, PARAMETER :: r_tundra_pan  = 5

! Integer zero for surface type descriptor
INTEGER, PARAMETER :: zero_int      = 0

! REAL parameters common to deposition_jules_surfddr & deposition_ukca_surfddr
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  tol               = 1.0e-10,                                                 &
    ! Tolerance (below which the number is assumed to be zero)
  min_tile_frac = 0.0,                                                         &
    ! minimum of tile fraction to include tile in CH4 soil uptake calculations
    ! (filter on todo-list)
  soil_moist_thresh = 0.3,                                                     &
    ! Thresholds for determining if soil is wet or not
    ! (0.3 is the nominal value used in the UKCA)
  microb_tmin       = 278.0,                                                   &
    ! Minimum temperature (K) for microbail activity
  microb_rhmin      = 0.4,                                                     &
    ! Minimum relative humidity (-) for microbial activity
  tmin_hno3         = 252.0,                                                   &
    ! Minimum temperature for HNO3 deposition parameterisation
  h2dd_sf           = 1.0e-04,                                                 &
    ! Scale factor for hydrogen deposition parameters
  h2dd_dep_rmax     = 0.00131,                                                 &
    ! Maximum value for H2 deposition resistance from Conrad & Seiler
  h2dd_sm_min       = 0.1,                                                     &
    ! Minimum value for soil moisture for H2 deposition parameterisation
  secs_in_hour      = 3600.0,                                                  &
    ! # of seconds in one hour
  ten               = 10.0
    ! Number ten

! Parameters for improved SO2 deposition, only used in UM-coupled JULES
! Common to deposition_jules_surfddr & deposition_ukca_surfddr
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  rh_thresh         = 0.813,                                                   &
    ! Relative humidity threshold
  minus1degc        = 272.15,                                                  &
    ! -1 degrees C in Kelvin
  minus5degc        = 268.15,                                                  &
    ! -5 degrees C in Kelvin
  r_wet_so2         = 1.0,                                                     &
    ! Wet soil and cuticular surface resistance for SO2 (s m-1)
  r_dry_so2         = 2000.0,                                                  &
    ! Dry cuticular surface resistance for SO2 (s m-1)
  r_cut_so2_5degc   = 500.0,                                                   &
    ! Wet cuticular surface resistance for SO2 when < -5 degrees C
  r_cut_so2_5to_1   = 200.0,                                                   &
    ! Wet cuticular surface resistance for SO2 when > -5 and < -1 degrees C
  so2con1           = 2.5e4,                                                   &
  so2con2           = 0.58e12,                                                 &
  so2con3           = -6.93,                                                   &
  so2con4           = -27.8
    ! The constants used in Equation 9 of Erisman, Pul and Wyers (1994).

! The constants used in Reay et al. (2001) for CH4 soil uptake efficiency.
! Common to deposition_jules_surfddr & deposition_ukca_surfddr
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  smfrac1           = 0.16,                                                    &
  smfrac2           = 0.30,                                                    &
  smfrac3           = 0.50,                                                    &
  smfrac4           = 0.20

! REAL parameter arrays common to deposition_jules_deposition &
! deposition_ukca_surfddr
REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
  ch4_up_flux(:),                                                              &
    ! CH4 uptake fluxes, in ug m-2 hr-1
  h2dd_c(:),                                                                   &
  h2dd_m(:),                                                                   &
  h2dd_q(:)
   ! Hydrogen - linear dependence on soil moisture (except savannah)
   ! Must take the same dimension as npft
   ! Quadratic term ( savannah only )

! REAL parameters only used by deposition_ukca_surfddr
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  mml = 1.008e5,                                                               &
    ! Factor to convert methane flux to dry dep vel.
    ! MML - Used to convert methane flux in ug m-2 h-1 to dry dep vel in
    ! m s-1; MML=3600*0.016*1.0E9*1.75E-6, where 0.016=RMM methane (kg),
    ! 1.0E9 converts ug -> kg, 1.75E-6 = assumed CH4 vmr
  r_wet_o3 = 500.0,                                                            &
    ! Wet soil surface resistance for ozone (s m-1)
  cuticle_o3 = 5000.0,                                                         &
    ! Constant for caln of O3 cuticular resistance
  tundra_s_limit = 0.866,                                                      &
    ! Southern limit for tundra (SIN(60))
  TAR_scaling = 15.0,                                                          &
    ! Scaling of CH4 soil uptake to match present-day TAR value of 30 Tg/year
  glmin       = 1.0e-6
    ! Minimum leaf conductance (m/s)

! REAL parameter arrays used by deposition_ukca_surfddr
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  r_tundra(r_tundra_spec) =  [ 1200.0, 25000.0, 800.0, 3850.0, 1100.0]
    !                          NO2     CO       O3     H2      PAN

REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  ch4dd_tun(4) =  [ -4.757e-6, 4.0288e-3, -1.13592, 106.636 ]
    ! CH4 loss to tundra - Cubic polynomial fit to data.
    ! N.B. Loss flux is in units of ug(CH4) m-2 s-1

REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  hno3dd_ice(3) =  [ -13.57, 6841.9, -857410.6 ]
    ! HNO3 dry dep to ice; quadratic dependence

REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  dif(5) =  [ 1.6, 1.6, 2.6, 1.9, 0.97 ]
    !         NO2   O3  PAN  SO2  NH3
    ! Diffusion correction for stomatal conductance
    ! Wesley (1989), Atmos. Env. 23, 1293.

! Standard surface resistances
REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
  rsurf(:,:),                                                                  &
    ! Following arrays used to set up surface resistance array rsurf.
    ! Must take the same dimension as ntype
  dep_rnull_tile(:),                                                           &
  rooh(:),                                                                     &
  aerosol(:),                                                                  &
  tenpointzero(:)

CONTAINS

SUBROUTINE deposition_initialisation_surfddr_ukca()

USE jules_surface_types_mod, ONLY:                                             &
    brd_leaf,                                                                  &
    brd_leaf_dec,                                                              &
    brd_leaf_eg_trop,                                                          &
    brd_leaf_eg_temp,                                                          &
    ndl_leaf,                                                                  &
    ndl_leaf_dec,                                                              &
    ndl_leaf_eg,                                                               &
    c3_grass,                                                                  &
    c3_crop,                                                                   &
    c3_pasture,                                                                &
    c4_grass,                                                                  &
    c4_crop,                                                                   &
    c4_pasture,                                                                &
    shrub,                                                                     &
    shrub_dec,                                                                 &
    shrub_eg,                                                                  &
    urban,                                                                     &
    lake,                                                                      &
    soil,                                                                      &
    ice,                                                                       &
    elev_ice

IMPLICIT NONE

! Local variables
INTEGER :: errcode
  ! error code

INTEGER :: i         ! Loop count over longitudes
INTEGER :: k         ! Loop count over latitudes
INTEGER :: j         ! Loop count over species that deposit
INTEGER :: n         ! Loop count over tiles

INTEGER  :: n_nosurf
  ! Number of species for which surface resistance values are not set

! Temporary logicals (local copies)
! The block defining the following deposition logicals has been deleted as
! they are defined in the JULES-based module jules_deposition above:
! l_fix_improve_drydep, l_fix_ukca_h2dd_x, l_fix_drydep_so2_water,
! l_ukca_dry_dep_so2wet, l_ukca_ddepo3_ocean, l_ukca_emsdrvn_ch4
! This is to avoid the use of UKCA-based modules in JULES.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER ::                                                 &
  RoutineName='DEPOSITION_INITIALISATION_SURFDDR_UKCA'


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Set up standard resistance array rsurf

! The block assigning surface type descriptors and the deposition logicals
! from the ukca_config structure to local variables has been deleted as
! they are defined in the JULES-based modules: jules_surface_types and
! jules_deposition. This is to avoid the use of UKCA-based modules in JULES,
! in advance of the UKCA moving to its own repository.

ALLOCATE(rsurf(ntype,ndry_dep_species))
ALLOCATE(dep_rnull_tile(ntype))
ALLOCATE(rooh(ntype))
ALLOCATE(aerosol(ntype))
ALLOCATE(tenpointzero(ntype))
ALLOCATE(ch4_up_flux(ntype))
ALLOCATE(h2dd_c(ntype))
ALLOCATE(h2dd_m(ntype))
ALLOCATE(h2dd_q(ntype))

rsurf(:,:) = dep_rnull
dep_rnull_tile(:) = dep_rnull
tenpointzero(:) = ten

! Check if we have standard JULES tile configuration and fail if not as
! Dry deposition in UKCA has not been set up for this.
! Use standard tile set up or extend UKCA dry depostion
WRITE(jules_message,'(A)')                                                     &
     'UKCA does not handle flexible tiles yet: '//                             &
     'Dry deposition needs extending. '         //                             &
     'Please use standard tile configuration'
SELECT CASE (ntype)
CASE (9)
  IF ( brd_leaf /= 1  .OR.                                                     &
       ndl_leaf /= 2  .OR.                                                     &
       c3_grass /= 3  .OR.                                                     &
       c4_grass /= 4  .OR.                                                     &
       shrub    /= 5  .OR.                                                     &
       urban    /= 6  .OR.                                                     &
       lake     /= 7  .OR.                                                     &
       soil     /= 8  .OR.                                                     &
       ice      /= 9 ) THEN

    ! Tile order changed from standard setup i.e. MOSES-II.
    WRITE(jules_message,'(A)') 'Nine tile order changed from standard setup'
    CALL jules_print(RoutineName,jules_message)
    errcode=1009
    CALL ereport(RoutineName,errcode,jules_message)
  END IF
CASE (13,17,27)
  IF ( brd_leaf_dec     /= 1   .OR.                                            &
       brd_leaf_eg_trop /= 2   .OR.                                            &
       brd_leaf_eg_temp /= 3   .OR.                                            &
       ndl_leaf_dec     /= 4   .OR.                                            &
       ndl_leaf_eg      /= 5   .OR.                                            &
       c3_grass         /= 6 ) THEN

    ! Tile order does not match that expected
    WRITE(jules_message,'(A)') 'Tile order changed from standard setup'
    CALL jules_print(RoutineName,jules_message)
    errcode=1001
    CALL ereport(RoutineName,errcode,jules_message)
  END IF
CASE DEFAULT
  ! ntype must equal 9, 13, 17, 27
  WRITE(jules_message,'(A)') 'ntype must be set to 9, 13, 17, 27.' //          &
                             'ntype = ', ntype
  CALL jules_print(RoutineName,jules_message)
  errcode=1002
  CALL ereport(RoutineName,errcode,jules_message)
END SELECT

SELECT CASE (ntype)
CASE (13)
  IF ( c4_grass         /= 7    .OR.                                           &
       shrub_dec        /= 8    .OR.                                           &
       shrub_eg         /= 9    .OR.                                           &
       urban            /= 10   .OR.                                           &
       lake             /= 11   .OR.                                           &
       soil             /= 12   .OR.                                           &
       ice              /= 13 ) THEN

    ! Tile order does not match that expected
    WRITE(jules_message,'(A)') 'Thirteen tile order changed from ' //          &
                               'standard setup'
    CALL jules_print(RoutineName,jules_message)
    errcode=1013
    CALL ereport(RoutineName,errcode,jules_message)
  END IF

CASE (17, 27)
  IF ( c3_crop          /= 7    .OR.                                           &
       c3_pasture       /= 8    .OR.                                           &
       c4_grass         /= 9    .OR.                                           &
       c4_crop          /= 10   .OR.                                           &
       c4_pasture       /= 11   .OR.                                           &
       shrub_dec        /= 12   .OR.                                           &
       shrub_eg         /= 13   .OR.                                           &
       urban            /= 14   .OR.                                           &
       lake             /= 15   .OR.                                           &
       soil             /= 16   .OR.                                           &
       ice              /= 17 ) THEN

    ! Tile order does not match that expected
    WRITE(jules_message,'(A)') 'Tile order changed from standard setup'
    CALL jules_print(RoutineName,jules_message)
    errcode=1727
    CALL ereport(RoutineName,errcode,jules_message)
  END IF
END SELECT

IF ( ntype == 27) THEN
  IF ( elev_ice(1)      /= 18   .OR.                                           &
       elev_ice(2)      /= 19   .OR.                                           &
       elev_ice(3)      /= 20   .OR.                                           &
       elev_ice(4)      /= 21   .OR.                                           &
       elev_ice(5)      /= 22   .OR.                                           &
       elev_ice(6)      /= 23   .OR.                                           &
       elev_ice(7)      /= 24   .OR.                                           &
       elev_ice(8)      /= 25   .OR.                                           &
       elev_ice(9)      /= 26   .OR.                                           &
       elev_ice(10)     /= 27 ) THEN

    ! Tile order does not match that expected
    WRITE(jules_message,'(A)') 'Tile order changed from standard setup'
    CALL jules_print(RoutineName,jules_message)
    errcode=1027
    CALL ereport(RoutineName,errcode,jules_message)
  END IF
END IF

SELECT CASE (ntype)
CASE (9)
  IF (l_fix_improve_drydep) THEN
    rooh =         [279.2,238.2,366.3,322.9,362.5,424.9,933.1,585.4,1156.1]
  ELSE
    rooh =         [30.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0]
  END IF

  aerosol =      [dep_rnull,dep_rnull,dep_rnull,dep_rnull,dep_rnull,dep_rnull, &
                  1000.0,dep_rnull,20000.0]

  IF (l_fix_improve_drydep) THEN
    ! soil should behave as c3_grass not as shrub
    ch4_up_flux =  [39.5,50.0,30.0,37.0,27.5,0.0,0.0,30.0,0.0]
  ELSE
    ch4_up_flux =  [39.5,50.0,30.0,37.0,27.5,0.0,0.0,27.5,0.0]
  END IF

  IF ( l_fix_ukca_h2dd_x ) THEN
    ! from Table 1 Sanderson, J. Atmos. Chem., v46, 15-28, 2003
    ! note : conversion factor 1.0e-4 (from paper) will only be applied
    !        in equation. Prior to the temporary logical switch, this was
    !        being applied twice in a few places.

    ! urban, lake and ice not used
    ! shrub/soil same as c3_grass below tundra limit else same as shrub
    h2dd_c =       [  19.70,  19.70,  17.70,  1.235,  1.000,                   &
                      0.000,  0.000,  0.000,  0.000 ]

    ! urban, lake and ice not used
    ! shrub/soil same as c3_grass below tundra limit else same as shrub
    h2dd_m =       [ -41.90, -41.90, -41.39, -0.472,  0.000,                   &
                      0.000,  0.000, -0.000,  0.000 ]

    ! Quadratic term for H2 loss to savannah only
    h2dd_q =       [   0.00,   0.00,   0.00,   0.27,   0.00,                   &
                       0.00,   0.00,   0.00,   0.00 ]
  ELSE
    ! from Table 1 Sanderson, J. Atmos. Chem., v46, 15-28, 2003
    !  with some errors

    ! urban, lake and ice not used
    ! shrub/soil same as c3_grass below tundra limit else same as shrub
    ! incorrect value for c4_grass
    h2dd_c =       [  0.00197,  0.00197,  0.00177,  1.23460, 0.00010,          &
                      0.00000,  0.00000,  0.00000,  0.00000 ]

    ! urban, lake and ice not used
    ! shrub/soil same as c3_grass below tundra limit else same as shrub
    h2dd_m =       [ -0.00419, -0.00419, -0.00414, -0.47200, 0.00000,          &
                      0.00000,  0.00000,  0.00000,  0.00000 ]

    ! Quadratic term for H2 loss (incorrect as should be savannah only)
    h2dd_q =       [     0.27,     0.27,     0.27,     0.27,    0.27,          &
                         0.27,     0.27,     0.27,     0.27 ]
  END IF

CASE (13,17,27)
  rooh(1:6)       =  [  300.3,  270.3,  266.9,  238.0,  238.5,  366.3 ]
  aerosol(1:6)    =  [ dep_rnull, dep_rnull, dep_rnull, dep_rnull, dep_rnull,  &
                       dep_rnull ]
  ch4_up_flux(1:6)=  [   39.5,   39.5,   39.5,   50.0,   50.0,   30.0 ]
  IF ( l_fix_ukca_h2dd_x ) THEN
    h2dd_c(1:6)   =  [  19.70,  19.70,  19.70,  19.70,  19.70,  17.70 ]
    h2dd_m(1:6)   =  [ -41.90, -41.90, -41.90, -41.90, -41.90, -41.39 ]
    h2dd_q(1:6)   =  [   0.00,   0.00,   0.00,   0.00,   0.00,   0.00 ]
  ELSE
    h2dd_c(1:6)   =  [  0.00197,  0.00197,  0.00197,  0.00197, 0.00197,        &
                        0.00177 ]
    h2dd_m(1:6)   =  [ -0.00419, -0.00419, -0.00419, -0.00419, -0.00419,       &
                       -0.00414 ]
    h2dd_q(1:6)   =  [   0.27,   0.27,   0.27,   0.27,   0.27,   0.27 ]
  END IF
CASE DEFAULT
  WRITE(jules_message,'(A,I0)') 'ntype = ', ntype
  CALL jules_print(RoutineName,jules_message)
  errcode=1
  WRITE(jules_message,'(A)')                                                   &
       'UKCA does not handle flexible tiles yet: ' //                          &
       'Dry deposition needs extending. '          //                          &
       'Please use standard tile configuration'
  CALL ereport(RoutineName,errcode,jules_message)
END SELECT

SELECT CASE (ntype)
CASE (13)
  rooh(7:13)       =  [   322.9, 332.8, 392.2, 424.9, 933.1, 585.4, 1156.1 ]
  aerosol(7:13)    =  [  dep_rnull,dep_rnull,dep_rnull,dep_rnull,1000.0,       &
                         dep_rnull,20000.0 ]

  IF ( l_fix_ukca_h2dd_x ) THEN
    !                   c4_grass,shrub_dec,shrub_eg,urban, lake, soil, ice
    ch4_up_flux(7:13)=  [   37.0,     27.5,    27.5,  0.0,  0.0, 30.0, 0.0 ]
    h2dd_c(7:13)     =  [  1.235,      1.0,     1.0,  0.0,  0.0,  0.0, 0.0 ]
    ! shrub/soil same as c3_grass below tundra limit else same as shrub
    h2dd_m(7:13)     =  [ -0.472,      0.0,     0.0,  0.0,  0.0,  0.0, 0.0 ]
    h2dd_q(7:13)     =  [   0.27,      0.0,     0.0,  0.0,  0.0,  0.0, 0.0 ]
  ELSE
    !                   c4_grass,shrub_dec,shrub_eg,urban, lake, soil, ice
    ch4_up_flux(7:13)=  [   37.0,     27.5,    27.5,  0.0,  0.0, 27.5, 0.0 ]
    h2dd_c(7:13)     =  [ 1.2346,   0.0001,  0.0001,  0.0,  0.0,  0.0, 0.0 ]
    ! shrub/soil same as c3_grass below tundra limit else same as shrub
    h2dd_m(7:13)     =  [ -0.472,      0.0,     0.0,  0.0,  0.0,  0.0, 0.0 ]
    h2dd_q(7:13)     =  [   0.27,     0.27,    0.27, 0.27, 0.27, 0.27,0.27 ]
  END IF
CASE (17, 27)
  rooh(7:17)       =  [  366.3,  366.3,  322.9,  322.9,  322.9,                &
                         332.8,  392.2,  424.9,  933.1,  585.4,  1156.1 ]
  aerosol(7:17)    =  [ dep_rnull, dep_rnull, dep_rnull, dep_rnull, dep_rnull, &
                        dep_rnull, dep_rnull, dep_rnull, 1000.0, dep_rnull,    &
                        20000.0]

  IF ( l_fix_ukca_h2dd_x ) THEN
    ch4_up_flux(7:17)=  [30.0,     30.0,     37.0,     37.0,     37.0,         &
                         27.5,     27.5,      0.0,      0.0,     30.0, 0.0 ]
    ! from Table 1 Sanderson, J. Atmos. Chem., v46, 15-28, 2003
    ! urban, lake and ice not used    & soil same as c3_grass
    h2dd_c(7:17) =  [   17.70,    17.70,    1.235,    1.235,    1.235,         &
                        1.000,    1.000,    0.000,    0.000,    0.000, 0.0 ]

    ! urban, lake and ice not used
    ! shrub/soil same as c3_grass below tundra limit else same as shrub
    h2dd_m(7:17) =  [  -41.39,   -41.39,   -0.472,   -0.472,   -0.472,         &
                        0.000,    0.000,    0.000,    0.000,   -41.39, 0.0 ]

    ! c4_grass, c4_crop, c4_pasture contain quadratic terms for H2 loss
    h2dd_q(7:17) =  [    0.00,     0.00,     0.27,     0.27,     0.27,         &
                         0.00,     0.00,     0.00,     0.00,     0.00, 0.0 ]
  ELSE
    ch4_up_flux(7:17)=  [30.0,     30.0,     37.0,     37.0,     37.0,         &
                         27.5,     27.5,      0.0,      0.0,     27.5, 0.0 ]

    h2dd_c(7:17) =  [ 0.00177,  0.00177,  1.23460,  1.23460,  1.23460,         &
                      0.00010,  0.00010,  0.00000,  0.00000,  0.00000, 0.0 ]

    h2dd_m(7:17) =  [-0.00414, -0.00414, -0.47200, -0.47200, -0.47200,         &
                      0.00000,  0.00000,  0.00000,  0.00000,  0.00000, 0.0 ]

    h2dd_q(7:17) =  [    0.27,     0.27,     0.27,     0.27,     0.27,         &
                         0.27,     0.27,     0.27,     0.27,     0.27, 0.27 ]
  END IF
END SELECT

IF (ntype == 27) THEN
  rooh(18:27)        =  [  1156.1, 1156.1, 1156.1, 1156.1, 1156.1,             &
                           1156.1, 1156.1, 1156.1, 1156.1, 1156.1 ]
  aerosol(18:27)     =  [ 20000.0,20000.0,20000.0,20000.0,20000.0,             &
                          20000.0,20000.0,20000.0,20000.0,20000.0 ]
  ch4_up_flux(18:27) =  [     0.0,    0.0,    0.0,    0.0,    0.0,             &
                              0.0,    0.0,    0.0,    0.0,    0.0 ]
  h2dd_c(18:27)      =  [     0.0,    0.0,    0.0,    0.0,    0.0,             &
                              0.0,    0.0,    0.0,    0.0,    0.0 ]
  h2dd_m(18:27)      =  [     0.0,    0.0,    0.0,    0.0,    0.0,             &
                              0.0,    0.0,    0.0,    0.0,    0.0 ]
  IF ( l_fix_ukca_h2dd_x ) THEN
    h2dd_q(18:27)      =  [   0.0,    0.0,    0.0,    0.0,    0.0,             &
                              0.0,    0.0,    0.0,    0.0,    0.0 ]
  ELSE
    h2dd_q(18:27)      =  [  0.27,   0.27,   0.27,   0.27,   0.27,             &
                             0.27,   0.27,   0.27,   0.27,   0.27 ]
  END IF
END IF

SELECT CASE (ntype)
CASE (9)
  ! Standard surface resistances (s m-1). Values are for 9 tiles in
  ! order: Broadleaved trees, Needleleaf trees, C3 Grass, C4 Grass,
  ! Shrub, Urban, Water, Bare Soil, Ice.
  !
  ! Nine tile dry deposition surface resistance have been updated April 2018
  ! (behind l_fix_improve_drydep logical) to reflect 13/17/27 tiles
  n_nosurf = 0
  DO n = 1, ndry_dep_species
    SELECT CASE (dep_species_name(n))
    CASE ('O3        ','O3S       ')
      IF (l_fix_improve_drydep) THEN
        rsurf(:,n)= [ 219.3, 233.0, 355.0, 309.3, 358.2,                       &
                      444.4,2000.0, 645.2,2000.0 ]
      ELSE
        rsurf(:,n)= [ 200.0, 200.0, 200.0, 200.0, 400.0,                       &
                      800.0,2200.0, 800.0,2500.0 ]
      END IF

    CASE ('NO2       ','NO3       ')
      IF (l_fix_improve_drydep) THEN
        rsurf(:,n)= [ 364.1, 291.3, 443.8, 386.6, 447.8,                       &
                      555.6,2500.0, 806.5,2500.0 ]
      ELSE
        rsurf(:,n)= [ 225.0, 225.0, 400.0, 400.0, 600.0,                       &
                     1200.0,2600.0,1200.0,3500.0 ]
      END IF
    CASE ('NO        ')
      IF (l_fix_improve_drydep) THEN
        rsurf(:,n)= [ 2184.5, 1747.6, 3662.7, 2319.6, 2686.8,                  &
                      3333.3,15000.0, 4838.7,15000.0 ]
      ELSE
        rsurf(:,n)= [ 1350.0, 1350.0, 2400.0, 2400.0, 3600.0,                  &
                     72000.0, dep_rnull,72000.0,21000.0 ]
      END IF
    CASE ('HNO3      ','HONO2     ','B2ndry    ','A2ndry    ','N2O5      ')
      IF (l_fix_improve_drydep) THEN
        rsurf(:,n)= [ 8.5, 8.4, 13.2, 12.0, 62.3, 12.8, 13.9, 16.0, 19.4 ]
      ELSE
        rsurf(:,n)=tenpointzero
      END IF
    CASE ('HNO4      ','HO2NO2    ')
      IF (l_fix_improve_drydep) THEN
        rsurf(:,n)= [ 17.0, 16.8, 26.4, 24.0, 24.9, 25.7, 27.7, 32.1, 38.8 ]
      ELSE
        rsurf(:,n)=tenpointzero
      END IF
      ! CRI organic nitrates deposit as ISON
    CASE ('ISON      ',                                                        &
          'HOC2H4NO3 ','RTX24NO3  ','RN9NO3    ',                              &
          'RN12NO3   ','RN15NO3   ','RN18NO3   ','RU14NO3   ',                 &
          'RTN28NO3  ','RTN25NO3  ','RTX28NO3  ','RTX22NO3  ',                 &
          'RA22NO3   ','RA25NO3   ','RTN23NO3  ','RU12NO3   ',                 &
          'RU10NO3   ','RA13NO3   ','RA16NO3   ','RA19NO3   ')
      IF (l_fix_improve_drydep) THEN
        rsurf(:,n)= [ 582.5, 466.0, 710.1, 618.6, 716.5,                       &
                      888.9,4000.0,1290.3,4000.0 ]
      ELSE
        rsurf(:,n)=tenpointzero
      END IF
    CASE ('HCl       ','HOCl      ','HBr       ','HOBr      ')
      IF (l_fix_improve_drydep) THEN
        rsurf(:,n)= [ 8.5, 8.4, 13.2, 12.0, 62.3, 12.8, 13.9, 16.0, 19.4 ]
      ELSE
        rsurf(:,n)=dep_rnull_tile
      END IF
    CASE ('H2SO4     ')
      IF (l_fix_improve_drydep) THEN
        rsurf(:,n)=  [  84.9,  83.8, 131.9, 120.0, 124.4,                      &
                       128.5, 138.6, 160.4, 194.2 ]
      ELSE
        rsurf(:,n)=dep_rnull_tile
      END IF
    CASE ('H2O2      ','HOOH      ')
      IF (l_fix_improve_drydep) THEN
        rsurf(:,n)= [ 84.9,83.8,131.9,120.0,124.4,128.5,138.6,160.4,194.2 ]
      ELSE
        rsurf(:,n)=tenpointzero
      END IF
      ! CRI R-OOH species deposit as MeOOH
      ! Also including hydroxy-ketones (CARB7-16) which deposit as HACET
    CASE ('CH3OOH    ','MeOOH     ','C2H5OOH   ','EtOOH     ',                 &
          'n_C3H7OOH ','i_C3H7OOH ','n-PrOOH   ','i-PrOOH   ',                 &
          'MeCOCH2OOH','ISOOH     ','MACROOH   ','MeCO3H    ',                 &
          'MeCO2H    ','HCOOH     ','PropeOOH  ','MEKOOH    ',                 &
          'ALKAOOH   ','AROMOOH   ','BSVOC1    ','BSVOC2    ',                 &
          'ASVOC1    ','ASVOC2    ','ISOSVOC1  ','ISOSVOC2  ',                 &
          's-BuOOH   ','MVKOOH    ','HACET     ',                              &
          'EtCO3H    ','HOCH2CO3H ','RCOOH25   ',                              &
          'HOC2H4OOH ','RN10OOH   ','RN13OOH   ','RN16OOH   ',                 &
          'RN19OOH   ','RN8OOH    ','RN11OOH   ','RN14OOH   ',                 &
          'RN17OOH   ','RU14OOH   ','RU12OOH   ','RU10OOH   ',                 &
          'NRU14OOH  ','NRU12OOH  ','RN9OOH    ','RN12OOH   ',                 &
          'RN15OOH   ','RN18OOH   ','NRN6OOH   ','NRN9OOH   ',                 &
          'NRN12OOH  ','RA13OOH   ','RA16OOH   ','RA19OOH   ',                 &
          'RTN28OOH  ','NRTN28OOH ','RTN26OOH  ','RTN25OOH  ',                 &
          'RTN24OOH  ','RTN23OOH  ','RTN14OOH  ','RTN10OOH  ',                 &
          'RTX28OOH  ','RTX24OOH  ','RTX22OOH  ','NRTX28OOH ',                 &
          'CARB7     ','CARB10    ','CARB13    ','CARB16    ',                 &
          'RA22OOH   ','RA25OOH   ','HPUCARB12 ','DHPCARB9  ',                 &
          'DHPR12OOH')
      rsurf(:,n)=rooh
      ! CRI PAN-type species
    CASE ('PAN       ','PPAN      ',                                           &
          'PHAN      ','RU12PAN   ','RTN26PAN  ')
      IF (l_fix_improve_drydep) THEN
        rsurf(:,n)= [ 485.4, 388.4, 591.7, 515.5, 597.1,                       &
                      740.7,3333.3,1075.3,3333.3 ]
      ELSE
        rsurf(:,n)= [ 500.0,  500.0, 500.0,  500.0, 500.0,                     &
                     dep_rnull,12500.0, 500.0,12500.0 ]
      END IF
    CASE ('MPAN      ')
      IF (l_fix_improve_drydep) THEN
        rsurf(:,n)= [  970.9,  776.7, 1183.4, 1030.9, 1194.2,                  &
                      1481.5, 6666.7, 2150.5, 6666.7 ]
      ELSE
        rsurf(:,n)= [ 500.0,  500.0, 500.0,  500.0, 500.0,                     &
                     dep_rnull,12500.0, 500.0,12500.0 ]
      END IF
    CASE ('OnitU     ')
      IF (l_fix_improve_drydep) THEN
        rsurf(:,n)= [ 582.5,  466.0, 710.1,  618.6, 716.5,                     &
                      888.9, 4000.0,1290.3, 4000.0 ]
      ELSE
        rsurf(:,n)= [ 500.0,  500.0, 500.0,  500.0, 500.0,                     &
                     dep_rnull,12500.0, 500.0,12500.0 ]
      END IF
    CASE ('NH3       ')
      IF (l_fix_improve_drydep) THEN
        rsurf(:,n)= [ 120.0, 130.9, 209.8, 196.1, 191.0,                       &
                      180.7, 148.9, 213.5, 215.1 ]
      ELSE
        rsurf(:,n)=tenpointzero
      END IF
    CASE ('CO        ')
      IF (l_fix_improve_drydep) THEN
        rsurf(:,n)= [ 3700.0, 7300.0, 4550.0, 1960.0, 4550.0,                  &
                      dep_rnull, dep_rnull, 4550.0, dep_rnull ]
      ELSE
        rsurf(:,n)= [ 3700.0, 7300.0, 4550.0, 1960.0, 4550.0,                  &
                      dep_rnull, dep_rnull, 4550.0, dep_rnull ]
      END IF
      ! Shrub+bare soil set to C3 grass (guess)
    CASE ('CH4       ')
      IF (l_ukca_emsdrvn_ch4) THEN
        rsurf(:,n)=1.0/dep_rnull_tile(:)
      ELSE
        rsurf(:,n)=dep_rnull_tile(:)
      END IF
    CASE ('HONO      ')
      rsurf(:,n)=dep_rnull_tile
    CASE ('H2        ')
      rsurf(:,n)=dep_rnull_tile
    CASE ('SO2       ')
      IF (l_ukca_dry_dep_so2wet) THEN
        ! With the implementation of the Erisman, Pul and Wyers (1994)
        ! parameterization we additionally reduce the surface resistance
        ! value of water to 1.0 s m-1.
        rsurf(:,n)= [ 120.0, 130.9, 209.8, 196.1, 191.0,                       &
                      180.7,   1.0, 213.5, 215.1 ]
      ELSE
        IF (l_fix_improve_drydep) THEN
          rsurf(:,n)= [ 120.0, 130.9, 209.8, 196.1, 191.0,                     &
                        180.7,  10.0, 213.5, 215.1 ]
        ELSE
          rsurf(:,n)= [ 100.0, 100.0, 150.0, 350.0, 400.0,                     &
                        400.0,  10.0, 700.0, dep_rnull ]
        END IF
      END IF
    CASE ('BSOA      ','ASOA      ','ISOSOA    ')
      rsurf(:,n)=aerosol
    CASE ('ORGNIT    ')
      IF (l_fix_improve_drydep) THEN
        rsurf(:,n)= [ 582.5, 466.0, 710.1, 618.6, 716.5,                       &
                      888.9,4000.0,1290.3,4000.0 ]
      ELSE
        rsurf(:,n)=aerosol
      END IF
    CASE ('Sec_Org   ','SEC_ORG_I ')
      IF (l_fix_improve_drydep) THEN
        rsurf(:,n)=aerosol
      ELSE
        rsurf(:,n)=dep_rnull_tile
      END IF
    CASE ('HCHO      ')
      IF (l_fix_improve_drydep) THEN
        rsurf(:,n)= [ 136.0, 143.5, 228.5, 211.6, 210.5,                       &
                      205.1, 182.7, 246.5, 261.8 ]
      ELSE
        rsurf(:,n)= [ 100.0, 100.0, 150.0, 350.0, 600.0,                       &
                      400.0, 200.0, 700.0, 700.0 ]
      END IF
      ! CRI alcohols deposit as MeOH
    CASE ('MeOH      ','EtOH      ','i-PrOH    ','n-PrOH    ',                 &
          'AROH14    ','ARNOH14   ','AROH17    ','ARNOH17   ',                 &
          'IEPOX     ','HMML      ','HUCARB9   ','DHCARB9   ')
      IF (l_fix_improve_drydep) THEN
        rsurf(:,n)= [ 187.1, 199.4, 318.3, 295.6, 292.2,                       &
                      282.1, 245.1, 337.2, 352.2 ]
      ELSE
        rsurf(:,n)=dep_rnull_tile
      END IF
      ! CRI Carbonyls, copying MeCHO rates
      ! Second generation nitrates deposit as NALD
    CASE ('MeCHO     ','EtCHO     ','MACR      ','NALD      ',                 &
          'HOCH2CHO  ',                                                        &
          'CARB14    ','CARB17    ','CARB11A   ','UCARB10   ',                 &
          'CARB15    ','UCARB12   ','UDCARB8   ','UDCARB11  ',                 &
          'UDCARB14  ','TNCARB26  ','TNCARB10  ','TNCARB12  ',                 &
          'TNCARB11  ','CCARB12   ','TNCARB15  ','TXCARB24  ',                 &
          'TXCARB22  ','UDCARB17  ','NOA       ','NUCARB12  ',                 &
          'ANHY      '                                                         &
          )
      IF (l_fix_improve_drydep) THEN
        rsurf(:,n)= [ 5825.2, 4660.3, 7100.6, 6185.6, 7164.8,                  &
                      8888.9,40000.0,12903.2,40000.0 ]
      ELSE
        rsurf(:,n)= [ 1200.0, 1200.0, 1200.0, 1200.0, 1000.0,                  &
                      2400.0, dep_rnull, dep_rnull, dep_rnull ]
      END IF
      ! CRI: CARB3 = GLY, CARB6 = MGLY etc.
    CASE ('MGLY      ','CARB3     ','CARB6     ','CARB9     ','CARB12    ')
      IF (l_fix_improve_drydep) THEN
        rsurf(:,n)= [ 12001.2, 13086.3, 20979.0, 19607.8, 19091.9,             &
                      18072.3, 14888.3, 21352.3, 21505.4 ]
      ELSE
        rsurf(:,n)= [ 1200.0, 1200.0, 1200.0, 1200.0, 1000.0,                  &
                      2400.0, dep_rnull, dep_rnull, dep_rnull ]
      END IF
    CASE ('DMSO      ','Monoterp  ','APINENE   ','BPINENE   ')
      rsurf(:,n)=dep_rnull_tile
    CASE DEFAULT
      n_nosurf = n_nosurf + 1
      jules_message = 'Surface resistance values not set for '                 &
                    // dep_species_name(n)
      CALL jules_print(RoutineName,jules_message)
    END SELECT
  END DO
  ! Fail if surface resistance values unavailable for some species
  IF (n_nosurf > 0) THEN
    WRITE(jules_message,'(A,I0,A)')                                            &
      ' Surface resistance values not found for ',n_nosurf,' species.'
    errcode = ABS(n_nosurf)
    CALL ereport(RoutineName,errcode,jules_message)
  END IF
CASE (13, 17, 27)
  ! Standard surface resistances (s m-1). Values are for 13/17/27 tiles in
  ! order: Broadleaf deciduous trees, Broadleaf evergreen tropical trees,
  !        Broadleaf evergreem temperate trees, Needleleaf deciduous trees,
  !        Needleleaf evergreen trees, C3 Grass,
  !
  n_nosurf = 0
  DO n = 1, ndry_dep_species
    SELECT CASE (dep_species_name(n))
    CASE ('O3        ','O3S       ')
      rsurf(1:6,n)= [ 307.7,285.7,280.4,232.6,233.5,355.0 ]
    CASE ('SO2       ')
      rsurf(1:6,n)= [ 137.0,111.1,111.9,131.3,130.4,209.8 ]
    CASE ('NO2       ','NO3       ')
      rsurf(1:6,n)= [ 384.6,357.1,350.5,290.7,291.8,443.8 ]
    CASE ('NO        ')
      rsurf(1:6,n)= [ 2307.7,2142.9,2102.8,1744.2,1751.0,3662.7 ]
    CASE ('HNO3      ','HONO2     ','B2ndry    ','A2ndry    ','N2O5      ')
      rsurf(1:6,n)= [ 9.5,8.0,8.0,8.4,8.4,13.2 ]
    CASE ('HCl       ','HOCl      ','HBr       ','HOBr      ')
      IF (l_fix_improve_drydep) THEN
        rsurf(1:6,n)= [ 9.5,8.0,8.0,8.4,8.4,13.2 ]
      ELSE
        rsurf(:,n)=dep_rnull_tile
      END IF
    CASE ('H2SO4     ')
      IF (l_fix_improve_drydep) THEN
        rsurf(1:6,n)= [ 94.8, 80.0, 80.0, 83.9, 83.7, 131.9 ]
      ELSE
        rsurf(:,n)=dep_rnull_tile
      END IF
    CASE ('HNO4      ','HO2NO2    ')
      rsurf(1:6,n)= [ 19.0,16.0,16.0,16.8,16.7,26.4 ]
    CASE ('HONO      ')
      rsurf(1:6,n)= [ 47.4,40.0,40.0,42.0,41.8,65.9 ]
    CASE ('H2O2      ','HOOH      ')
      rsurf(1:6,n)= [ 94.8,80.0,80.0,83.9,83.7,131.9 ]
      ! Extra CRI PAN species
    CASE ('PAN       ','PPAN      ',                                           &
          'PHAN      ','RU12PAN   ','RTN26PAN  ')
      rsurf(1:6,n)= [ 512.8,476.2,467.3,387.6,389.1,591.7 ]
    CASE ('MPAN      ')
      rsurf(1:6,n)= [ 1025.6,952.4,934.6,775.2,778.2,1183.4 ]
    CASE ('OnitU     ','ISON      ','ORGNIT    ',                              &
           'HOC2H4NO3 ','RN9NO3    ','RN12NO3   ','RTN25NO3  ',                &
           'RN15NO3   ','RN18NO3   ','RU14NO3   ','RTN28NO3  ',                &
           'RTX28NO3  ','RA22NO3   ','RA25NO3   ','RU12NO3   ',                &
           'RU10NO3   ','RTX22NO3  ','RTN23NO3  ','RTX24NO3  ',                &
           'RA13NO3   ','RA16NO3   ','RA19NO3   ')
      rsurf(1:6,n)= [ 615.4,571.4,560.7,465.1,466.9,710.1 ]
      ! CRI R-OOH species.
      ! Also including hydroxy-ketones (CARB7-16) which deposit as HACET
    CASE ('CH3OOH    ','MeOOH     ','C2H5OOH   ','EtOOH     ',                 &
          'n_C3H7OOH ','i_C3H7OOH ','n-PrOOH   ','i-PrOOH   ',                 &
          'MeCOCH2OOH','ISOOH     ','MACROOH   ','MeCO3H    ',                 &
          'MeCO2H    ','HCOOH     ','PropeOOH  ','MEKOOH    ',                 &
          'ALKAOOH   ','AROMOOH   ','BSVOC1    ','BSVOC2    ',                 &
          'ASVOC1    ','ASVOC2    ','ISOSVOC1  ','ISOSVOC2  ',                 &
          's-BuOOH   ','MVKOOH    ','HACET     ',                              &
          'EtCO3H    ','HOCH2CO3H ','RCOOH25   ',                              &
          'HOC2H4OOH ','RN10OOH   ','RN13OOH   ','RN16OOH   ',                 &
          'RN19OOH   ','RN8OOH    ','RN11OOH   ','RN14OOH   ',                 &
          'RN17OOH   ','RU14OOH   ','RU12OOH   ','RU10OOH   ',                 &
          'NRU14OOH  ','NRU12OOH  ','RN9OOH    ','RN12OOH   ',                 &
          'RN15OOH   ','RN18OOH   ','NRN6OOH   ','NRN9OOH   ',                 &
          'NRN12OOH  ','RA13OOH   ','RA16OOH   ','RA19OOH   ',                 &
          'RTN28OOH  ','NRTN28OOH ','RTN26OOH  ','RTN25OOH  ',                 &
          'RTN24OOH  ','RTN23OOH  ','RTN14OOH  ','RTN10OOH  ',                 &
          'RTX28OOH  ','RTX24OOH  ','RTX22OOH  ','NRTX28OOH ',                 &
          'CARB7     ','CARB10    ','CARB13    ','CARB16    ',                 &
          'RA22OOH   ','RA25OOH   ','HPUCARB12 ','DHPCARB9  ',                 &
          'DHPR12OOH ')
      rsurf(:,n)=rooh
    CASE ('NH3       ')
      rsurf(1:6,n)= [ 137.0,111.1,111.9,131.3,130.4,209.8 ]
    CASE ('CO        ')
      rsurf(1:6,n)= [ 3700.0,3700.0,3700.0,7300.0,7300.0,4550.0 ]
      ! Shrub+bare soil set to C3 grass (guess)
    CASE ('CH4       ')
      IF (l_ukca_emsdrvn_ch4) THEN
        rsurf(:,n)=1.0/dep_rnull_tile(:) ! removal rate, NOT a resistance!
      ELSE
        rsurf(:,n)=dep_rnull_tile(:)
      END IF
    CASE ('H2        ')
      rsurf(:,n)=dep_rnull_tile
    CASE ('HCHO      ')
      rsurf(1:6,n)= [ 154.1,126.6,127.2,143.8,143.1,228.5 ]
    CASE ('MeOH      ','EtOH      ','i-PrOH    ','n-PrOH    ',                 &
          'AROH14    ','ARNOH14   ','AROH17    ','ARNOH17   ',                 &
          'IEPOX     ','HMML      ','HUCARB9   ','DHCARB9   ')
      IF (l_fix_improve_drydep) THEN
        rsurf(1:6,n)= [ 212.6, 173.9, 174.9, 200.0, 198.0, 318.3 ]
      ELSE
        rsurf(:,n)=dep_rnull_tile
      END IF
      ! CRI Carbonyls, copying MeCHO rates
      ! Second generation nitrates deposit as NALD
    CASE ('MeCHO     ','EtCHO     ','MACR      ','NALD      ',                 &
          'HOCH2CHO  ',                                                        &
          'CARB14    ','CARB17    ','CARB11A   ','UCARB10   ',                 &
          'UCARB12   ','UDCARB8   ','UDCARB11  ',                              &
          'UDCARB14  ','TNCARB26  ','TNCARB10  ','TNCARB12  ',                 &
          'TNCARB11  ','CCARB12   ','TNCARB15  ','TXCARB24  ',                 &
          'TXCARB22  ','UDCARB17  ','NOA       ','NUCARB12  ',                 &
          'ANHY      '                                                         &
          )
      rsurf(1:6,n)= [ 6153.8,5714.3,5607.5,4651.2,4669.3,7100.6 ]
      ! Glyoxal-type CRI species, deposit as MGLY
      !   n.b. CARB6 ~ MGLOX == MGLY
    CASE ('MGLY      ',                                                        &
          'CARB3     ','CARB6     ','CARB9     ','CARB12    ',                 &
          'CARB15    '                                                         &
          )

      rsurf(1:6,n)= [ 13698.6,11111.1,11194.0,13129.1,13043.5,20979.0 ]
    CASE ('BSOA      ','ASOA      ','ISOSOA    ')
      rsurf(:,n)=aerosol
    CASE ('Sec_Org   ','SEC_ORG_I ')
      IF (l_fix_improve_drydep) THEN
        rsurf(:,n)=aerosol
      ELSE
        rsurf(:,n)=dep_rnull_tile
      END IF
    CASE ('DMSO      ','Monoterp  ','APINENE   ','BPINENE   ')
      rsurf(:,n)=dep_rnull_tile
    CASE DEFAULT
      n_nosurf = n_nosurf + 1
      jules_message = 'Surface resistance values not set for '                 &
                    // dep_species_name(n)
      CALL jules_print(RoutineName,jules_message)
    END SELECT
  END DO
  ! Fail if surface resistance values unavailable for some species
  IF (n_nosurf > 0) THEN
    WRITE(jules_message,'(A,I0,A)')                                            &
    ' Surface resistance values not found for ',n_nosurf,' species.'
    errcode = ABS(n_nosurf)
    CALL ereport(RoutineName,errcode,jules_message)
  END IF
CASE DEFAULT
  errcode=319
  WRITE(jules_message,'(A)')                                                   &
       'UKCA does not handle flexible tiles yet: '//                           &
       'Dry deposition needs extending. '         //                           &
       'Please use standard tile configuration'
  CALL ereport(RoutineName,errcode,jules_message)
END SELECT

SELECT CASE (ntype)
CASE (13)
  ! Standard surface resistances (s m-1). Values are for 13 tiles in
  ! order: C4 Grass, Shrub deciduous, Shrub evergreen,
  !        Urban, Water, Bare Soil, Ice.
  DO n = 1, ndry_dep_species
    SELECT CASE (dep_species_name(n))
    CASE ('O3        ','O3S       ')
      rsurf(7:13,n)= [ 309.3,324.3,392.2,444.4,2000.0,645.2,2000.0 ]
    CASE ('SO2       ')
      IF (l_ukca_dry_dep_so2wet) THEN
        ! With the implementation of the Erisman, Pul and Wyers (1994)
        ! parameterization we additionally reduce the surface resistance
        ! value of water to 1.0 s m-1.
        rsurf(7:13,n)= [ 196.1,185.8,196.1,180.7,  1.0,213.5,215.1 ]
      ELSE
        IF (l_fix_drydep_so2_water) THEN
          ! Setting resistance of water to 148.9 s m-1 was an error, so
          ! this change reduces it to its previous value (10.0 s m-1).
          rsurf(7:13,n)= [ 196.1,185.8,196.1,180.7, 10.0,213.5,215.1 ]
        ELSE
          ! Retains previous suspect value of 148.9 s m-1 for water
          ! resistance.
          rsurf(7:13,n)= [ 196.1,185.8,196.1,180.7,148.9,213.5,215.1 ]
        END IF
      END IF
    CASE ('NO2       ','NO3       ')
      rsurf(7:13,n)= [ 386.6,405.4,490.2,555.6,2500.0,806.5,2500.0 ]
    CASE ('NO        ')
      rsurf(7:13,n)= [ 2319.6,2432.4,2941.2,3333.3,15000.0,4838.7,15000.0 ]
    CASE ('HNO3      ','HONO2     ','B2ndry    ','A2ndry    ','N2O5      ')
      rsurf(7:13,n)= [ 12.0,59.1,65.4,12.8,13.9,16.0,19.4 ]
    CASE ('HCl       ','HOCl      ','HBr       ','HOBr      ')
      IF (l_fix_improve_drydep) THEN
        rsurf(7:13,n)= [ 12.0,59.1,65.4,12.8,13.9,16.0,19.4 ]
      END IF
    CASE ('H2SO4     ')
      IF (l_fix_improve_drydep) THEN
        rsurf(7:13,n)= [ 120.0, 118.1, 130.7, 128.5, 138.6, 160.4, 194.2 ]
      END IF
    CASE ('HNO4      ','HO2NO2    ')
      rsurf(7:13,n)= [ 24.0,23.6,26.1,25.7,27.7,32.1,38.8 ]
    CASE ('HONO      ')
      rsurf(7:13,n)= [ 60.0,59.1,65.4,64.2,69.3,80.2,97.1 ]
    CASE ('H2O2      ','HOOH      ')
      rsurf(7:13,n)= [ 120.0,118.1,130.7,128.5,138.6,160.4,194.2 ]
      ! CRI PAN-type species
    CASE ('PAN       ','PPAN      ',                                           &
          'PHAN      ','RU12PAN   ','RTN26PAN  ')
      rsurf(7:13,n)= [ 515.5,540.5,653.6,740.7,3333.3,1075.3,3333.3 ]
    CASE ('MPAN      ')
      rsurf(7:13,n)= [ 1030.9,1081.1,1307.2,1481.5,6666.7,2150.5,6666.7 ]
      ! CRI organic-nitrates, copying ORGANIT rates
    CASE ('OnitU     ','ISON      ','ORGNIT    ',                              &
          'HOC2H4NO3 ','RTX24NO3  ','RN9NO3    ',                              &
          'RN12NO3   ','RN15NO3   ','RN18NO3   ','RU14NO3   ',                 &
          'RTN28NO3  ','RTN25NO3  ','RTX28NO3  ','RTX22NO3  ',                 &
          'RA22NO3   ','RA25NO3   ','RTN23NO3  ','RU12NO3   ',                 &
          'RU10NO3   ','RA13NO3   ','RA16NO3   ','RA19NO3   ')
      rsurf(7:13,n)= [ 618.6,648.6,784.3,888.9,4000.0,1290.3,4000.0 ]
    CASE ('NH3       ')
      rsurf(7:13,n)= [ 196.1,185.8,196.1,180.7,148.9,213.5,215.1 ]
    CASE ('CO        ')
      rsurf(7:13,n)= [ 1960.0,4550.0,4550.0,dep_rnull,dep_rnull,4550.0,        &
                       dep_rnull ]
        ! Shrub+bare soil set to C3 grass (guess)
    CASE ('HCHO      ')
      rsurf(7:13,n)= [ 211.6,203.1,217.9,205.1,182.7,246.5,261.8 ]
      ! CRI v2.2 adds IEPOX, HMML, HUCARB9 and DHCARB9 to this list
    CASE ('MeOH      ','EtOH      ','i-PrOH    ','n-PrOH    ',                 &
          'AROH14    ','ARNOH14   ','AROH17    ','ARNOH17   ',                 &
          'IEPOX     ','HMML      ','HUCARB9   ','DHCARB9   ')
      IF (l_fix_improve_drydep) THEN
        rsurf(7:13,n)= [ 295.6, 282.7, 301.7, 282.1, 245.1, 337.2, 352.2 ]
      END IF
      ! CRI Carbonyls, copying MeCHO rates
      !   Second generation nitrates deposit as NALD
    CASE ('MeCHO     ','EtCHO     ','MACR      ','NALD      ',                 &
          'HOCH2CHO  ',                                                        &
          'CARB14    ','CARB17    ','CARB11A   ','UCARB10   ',                 &
          'UCARB12   ','UDCARB8   ','UDCARB11  ',                              &
          'UDCARB14  ','TNCARB26  ','TNCARB10  ','TNCARB12  ',                 &
          'TNCARB11  ','CCARB12   ','TNCARB15  ','TXCARB24  ',                 &
          'TXCARB22  ','UDCARB17  ','NOA       ','NUCARB12  ',                 &
          'ANHY      '                                                         &
          )
      rsurf(7:13,n)= [ 6185.6,6486.5,7843.1,8888.9,40000.0,12903.2,40000.0 ]
      ! CRI Glyoxal-type species, deposit as MGLY
      !   n.b. CARB6 ~ MGLOX == MGLY
    CASE ('MGLY      ',                                                        &
          'CARB3     ','CARB6     ','CARB9     ','CARB12    ',                 &
          'CARB15    '                                                         &
          )
      rsurf(7:13,n)= [ 19607.8,18575.9,19607.8,18072.3,14888.3,21352.3,        &
                       21505.4 ]
    END SELECT
  END DO
CASE (17, 27)
  ! Standard surface resistances (s m-1). Values are for 17/27 tiles in
  ! order: C3 Crop, C3 Pasture, C4 Grass, C4 Crop, C4 Pasture,
  !        Shrub deciduous, Shrub evergreen, Urban, Water, Bare Soil, Ice.
  DO n = 1, ndry_dep_species
    SELECT CASE (dep_species_name(n))
    CASE ('O3        ','O3S       ')
      rsurf(7:17,n)= [ 355.0,355.0,309.3, 309.3,309.3,                         &
                       324.3,392.2,444.4,2000.0,645.2,2000.0 ]
    CASE ('SO2       ')
      IF (l_ukca_dry_dep_so2wet) THEN
        ! For the Erisman, Pul and Wyers (1994) parameterization, C3
        ! and C4 crops are assumed to be irrigated/watered, and so their
        ! resistances are greatly reduced (although the choice of 30.0
        ! is an estimate). Additionally we reduce the surface resistance
        ! value of water to 1.0 s m-1.
        rsurf(7:17,n)= [  30.0,209.8,196.1, 30.0,196.1,                        &
                         185.8,196.1,180.7,  1.0,213.5,215.1 ]
      ELSE
        IF (l_fix_drydep_so2_water) THEN
          ! Setting resistance of water to 148.9 s m-1 was an error, so
          ! this change reduces it its previous value (10.0 s m-1)
          rsurf(7:17,n)= [ 209.8,209.8,196.1,196.1,196.1,                      &
                           185.8,196.1,180.7, 10.0,213.5,215.1 ]
        ELSE
          ! Retains previous suspect value of 148.9 s m-1 for water
          ! resistance (used in UKESM1.0)
          rsurf(7:17,n)= [ 209.8,209.8,196.1,196.1,196.1,                      &
                           185.8,196.1,180.7,148.9,213.5,215.1 ]
        END IF
      END IF
    CASE ('NO2       ','NO3       ')
      rsurf(7:17,n)= [ 443.8,443.8,386.6, 386.6,386.6,                         &
                       405.4,490.2,555.6,2500.0,806.5,2500.0 ]
    CASE ('NO        ')
      rsurf(7:17,n)= [ 3662.7,3662.7,2319.6, 2319.6,2319.6,                    &
                       2432.4,2941.2,3333.3,15000.0,4838.7,15000.0 ]
    CASE ('HNO3      ','HONO2     ','B2ndry    ','A2ndry    ','N2O5      ')
      rsurf(7:17,n)= [ 13.2,13.2,12.0,12.0,12.0,                               &
                       59.1,65.4,12.8,13.9,16.0,19.4 ]
    CASE ('HCl       ','HOCl      ','HBr       ','HOBr      ')
      IF (l_fix_improve_drydep) THEN
        rsurf(7:17,n)= [ 13.2,13.2,12.0,12.0,12.0,                             &
                         59.1,65.4,12.8,13.9,16.0,19.4 ]
      END IF
    CASE ('H2SO4     ')
      IF (l_fix_improve_drydep) THEN
        rsurf(7:17,n)= [ 131.9, 131.9, 120.0, 120.0, 120.0,                    &
                         118.1, 130.7, 128.5, 138.6, 160.4, 194.2 ]
      END IF
    CASE ('HNO4      ','HO2NO2    ')
      rsurf(7:17,n)= [ 26.4,26.4,24.0,24.0,24.0,                               &
                       23.6,26.1,25.7,27.7,32.1,38.8 ]
    CASE ('HONO      ')
      rsurf(7:17,n)= [ 65.9,65.9,60.0,60.0,60.0,                               &
                       59.1,65.4,64.2,69.3,80.2,97.1 ]
    CASE ('H2O2      ','HOOH      ')
      rsurf(7:17,n)= [ 131.9,131.9,120.0,120.0,120.0,                          &
                       118.1,130.7,128.5,138.6,160.4,194.2 ]
      ! CRI PAN-type species
    CASE ('PAN       ','PPAN      ',                                           &
          'PHAN      ','RU12PAN   ','RTN26PAN  ')
      rsurf(7:17,n)= [ 591.7,591.7,515.5, 515.5, 515.5,                        &
                       540.5,653.6,740.7,3333.3,1075.3,3333.3 ]
    CASE ('MPAN      ')
      rsurf(7:17,n)= [ 1183.4,1183.4,1030.9,1030.9,1030.9,                     &
                       1081.1,1307.2,1481.5,6666.7,2150.5,6666.7 ]
      ! CRI organic-nitrates, copying ORGANIT rates
    CASE ('OnitU     ','ISON      ','ORGNIT    ',                              &
          'HOC2H4NO3 ','RTX24NO3  ','RN9NO3    ',                              &
          'RN12NO3   ','RN15NO3   ','RN18NO3   ','RU14NO3   ',                 &
          'RTN28NO3  ','RTN25NO3  ','RTX28NO3  ','RTX22NO3  ',                 &
          'RA22NO3   ','RA25NO3   ','RTN23NO3  ','RU12NO3   ',                 &
          'RU10NO3   ','RA13NO3   ','RA16NO3   ','RA19NO3   ')
      rsurf(7:17,n)= [ 710.1,710.1,618.6, 618.6, 618.6,                        &
                       648.6,784.3,888.9,4000.0,1290.3,4000.0 ]
    CASE ('NH3       ')
      rsurf(7:17,n)= [ 209.8,209.8,196.1,196.1,196.1,                          &
                       185.8,196.1,180.7,148.9,213.5,215.1 ]
    CASE ('CO        ')
      rsurf(7:17,n)= [ 4550.0,4550.0,1960.0,1960.0,1960.0,                     &
                       4550.0,4550.0,dep_rnull,dep_rnull,4550.0,dep_rnull ]
        ! Shrub+bare soil set to C3 grass (guess)
    CASE ('HCHO      ')
      rsurf(7:17,n)= [ 228.5,228.5,211.6,211.6,211.6,                          &
                       203.1,217.9,205.1,182.7,246.5,261.8 ]
      ! CRI alcohols deposit as MeOH
    CASE ('MeOH      ','EtOH      ','i-PrOH    ','n-PrOH    ',                 &
          'AROH14    ','ARNOH14   ','AROH17    ','ARNOH17   ',                 &
          'IEPOX     ','HMML      ','HUCARB9   ','DHCARB9   ')
      IF (l_fix_improve_drydep) THEN
        rsurf(7:17,n)= [ 318.3, 318.3, 295.6, 295.6, 295.6,                    &
                         282.7, 301.7, 282.1, 245.1, 337.2, 352.2 ]
      END IF
      ! CRI Carbonyls, copying MeCHO rates
      !   Second generation nitrates deposit as NALD
    CASE ('MeCHO     ','EtCHO     ','MACR      ','NALD      ',                 &
          'HOCH2CHO  ',                                                        &
          'CARB14    ','CARB17    ','CARB11A   ','UCARB10   ',                 &
          'UCARB12   ','UDCARB8   ','UDCARB11  ',                              &
          'UDCARB14  ','TNCARB26  ','TNCARB10  ','TNCARB12  ',                 &
          'TNCARB11  ','CCARB12   ','TNCARB15  ','TXCARB24  ',                 &
          'TXCARB22  ','UDCARB17  ','NOA       ','NUCARB12  ',                 &
          'ANHY      '                                                         &
          )
      rsurf(7:17,n)= [ 7100.6,7100.6,6185.6, 6185.6, 6185.6,                   &
                       6486.5,7843.1,8888.9,40000.0,12903.2,40000.0 ]
      ! CRI Glyoxal-type species, deposit as MGLY
      !   n.b. CARB6 ~ MGLOX == MGLY
    CASE ('MGLY      ',                                                        &
          'CARB3     ','CARB6     ','CARB9     ','CARB12    ',                 &
          'CARB15    '                                                         &
          )
      rsurf(7:17,n)= [ 20979.0,20979.0,19607.8,19607.8,19607.8,                &
                       18575.9,19607.8,18072.3,14888.3,21352.3,21505.4 ]
    END SELECT
  END DO
END SELECT

IF (ntype ==27) THEN
  ! Standard surface resistances (s m-1). Values are for 27 tiles in
  ! order: Elev_Ice(1-10).
  DO n = 1, ndry_dep_species
    SELECT CASE (dep_species_name(n))
    CASE ('O3        ','O3S       ')
      rsurf(18:27,n)= [ 2000.0,2000.0,2000.0,2000.0,2000.0,                    &
                        2000.0,2000.0,2000.0,2000.0,2000.0 ]
    CASE ('SO2       ')
      rsurf(18:27,n)= [ 215.1,215.1,215.1,215.1,215.1,                         &
                        215.1,215.1,215.1,215.1,215.1 ]
    CASE ('NO2       ','NO3       ')
      rsurf(18:27,n)= [ 2500.0,2500.0,2500.0,2500.0,2500.0,                    &
                        2500.0,2500.0,2500.0,2500.0,2500.0 ]
    CASE ('NO        ')
      rsurf(18:27,n)= [ 15000.0,15000.0,15000.0,15000.0,15000.0,               &
                        15000.0,15000.0,15000.0,15000.0,15000.0 ]
    CASE ('HNO3      ','HONO2     ','B2ndry    ','A2ndry    ','N2O5      ')
      rsurf(18:27,n)= [ 19.4,19.4,19.4,19.4,19.4,                              &
                        19.4,19.4,19.4,19.4,19.4 ]
    CASE ('HCl       ','HOCl      ','HBr       ','HOBr      ')
      IF (l_fix_improve_drydep) THEN
        rsurf(18:27,n)= [ 19.4,19.4,19.4,19.4,19.4,                            &
                          19.4,19.4,19.4,19.4,19.4 ]
      END IF
    CASE ('H2SO4     ')
      IF (l_fix_improve_drydep) THEN
        rsurf(18:27,n)= [ 194.2, 194.2, 194.2, 194.2, 194.2,                   &
                          194.2, 194.2, 194.2, 194.2, 194.2 ]
      END IF
    CASE ('HNO4      ','HO2NO2    ')
      rsurf(18:27,n)= [ 38.8,38.8,38.8,38.8,38.8,                              &
                        38.8,38.8,38.8,38.8,38.8 ]
    CASE ('HONO      ')
      rsurf(18:27,n)= [ 97.1,97.1,97.1,97.1,97.1,                              &
                        97.1,97.1,97.1,97.1,97.1 ]
    CASE ('H2O2      ','HOOH      ')
      rsurf(18:27,n)= [ 194.2,194.2,194.2,194.2,194.2,                         &
                        194.2,194.2,194.2,194.2,194.2 ]
      ! CRI PAN-type species
    CASE ('PAN       ','PPAN      ',                                           &
          'PHAN      ','RU12PAN   ','RTN26PAN  ')
      rsurf(18:27,n)= [ 3333.3,3333.3,3333.3,3333.3,3333.3,                    &
                        3333.3,3333.3,3333.3,3333.3,3333.3 ]
    CASE ('MPAN      ')
      rsurf(18:27,n)= [ 6666.7,6666.7,6666.7,6666.7,6666.7,                    &
                        6666.7,6666.7,6666.7,6666.7,6666.7 ]
      ! CRI organic-nitrates, copying ORGANIT rates
    CASE ('OnitU     ','ISON      ','ORGNIT    ',                              &
          'HOC2H4NO3 ','RTX24NO3  ','RN9NO3    ',                              &
          'RN12NO3   ','RN15NO3   ','RN18NO3   ','RU14NO3   ',                 &
          'RTN28NO3  ','RTN25NO3  ','RTX28NO3  ','RTX22NO3  ',                 &
          'RA22NO3   ','RA25NO3   ','RTN23NO3  ','RU12NO3   ',                 &
          'RU10NO3   ','RA13NO3   ','RA16NO3   ','RA19NO3   ')
      rsurf(18:27,n)= [ 4000.0,4000.0,4000.0,4000.0,4000.0,                    &
                        4000.0,4000.0,4000.0,4000.0,4000.0 ]
    CASE ('NH3       ')
      rsurf(18:27,n)= [ 215.1,215.1,215.1,215.1,215.1,                         &
                        215.1,215.1,215.1,215.1,215.1 ]
    CASE ('CO        ')
      rsurf(18:27,n)= [ dep_rnull,dep_rnull,dep_rnull,dep_rnull,dep_rnull,     &
                        dep_rnull,dep_rnull,dep_rnull,dep_rnull,dep_rnull ]
        ! Shrub+bare soil set to C3 grass (guess)
    CASE ('HCHO      ')
      rsurf(18:27,n)= [ 261.8,261.8,261.8,261.8,261.8,                         &
                        261.8,261.8,261.8,261.8,261.8 ]
      ! CRI alcohols deposit as MeOH
    CASE ('MeOH      ','EtOH      ','i-PrOH    ','n-PrOH    ',                 &
          'AROH14    ','ARNOH14   ','AROH17    ','ARNOH17   ',                 &
          'IEPOX     ','HMML      ','HUCARB9   ','DHCARB9   ')
      IF (l_fix_improve_drydep) THEN
        rsurf(18:27,n)= [ 352.2, 352.2, 352.2, 352.2, 352.2,                   &
                          352.2, 352.2, 352.2, 352.2, 352.2 ]
      END IF
      ! CRI Carbonyls, copying MeCHO rates
      !   Second generation nitrates deposit as NALD
    CASE ('MeCHO     ','EtCHO     ','MACR      ','NALD      ',                 &
          'HOCH2CHO  ',                                                        &
          'CARB14    ','CARB17    ','CARB11A   ','UCARB10   ',                 &
          'UCARB12   ','UDCARB8   ','UDCARB11  ',                              &
          'UDCARB14  ','TNCARB26  ','TNCARB10  ','TNCARB12  ',                 &
          'TNCARB11  ','CCARB12   ','TNCARB15  ','TXCARB24  ',                 &
          'TXCARB22  ','UDCARB17  ','NOA       ','NUCARB12  ',                 &
          'ANHY      '                                                         &
          )
      rsurf(18:27,n)= [ 40000.0,40000.0,40000.0,40000.0,40000.0,               &
                        40000.0,40000.0,40000.0,40000.0,40000.0 ]
      ! CRI Glyoxal-type species, deposit as MGLY
      !   n.b. CARB6 ~ MGLOX == MGLY
    CASE ('MGLY      ',                                                        &
          'CARB3     ','CARB6     ','CARB9     ','CARB12    ',                 &
          'CARB15    '                                                         &
          )
      rsurf(18:27,n)= [ 21505.4,21505.4,21505.4,21505.4,21505.4,               &
                        21505.4,21505.4,21505.4,21505.4,21505.4 ]
    END SELECT
  END DO
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE deposition_initialisation_surfddr_ukca

END MODULE deposition_initialisation_surfddr_mod
