! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE jules_soil_mod

USE max_dimensions, ONLY: sm_levels_max

USE missing_data_mod, ONLY: rmdi, imdi

!-----------------------------------------------------------------------------
! Description:
!   Contains soil options and a namelist for setting them
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Switches
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  sm_levels = imdi

LOGICAL ::                                                                     &
  l_vg_soil      = .FALSE.,                                                    &
      ! Switch for using Van Genuchten soil scheme
  l_dpsids_dsdz = .FALSE.,                                                     &
      ! Switch to calculate vertical gradient of soil suction with the
      ! assumption of linearity only for fractional saturation
      ! (consistent with the calculation of hydraulic conductivity)
  l_soil_sat_down = .FALSE.,                                                   &
      ! Switch for direction of super_saturated soil moisture
      !   TRUE excess water is pushed down
      !   FALSE excess water is pushed up (as in JULES2.0)
  l_bedrock = .FALSE.,                                                         &
      ! Switch to control the presence of bedrock
      ! This is an additional layer below the hydrologically active
      ! soil where only thermal diffusion occurs
  l_tile_soil = .FALSE.,                                                       &
      ! Switch to control soil tiling.
      ! When F nsoilt = 1, when T nsoilt = nsurft
  soil_props_const_z = .FALSE.,                                                &
      ! Switch for whether soil ancils has the same values on each layer.
      ! Set in the JULES_SOIL_PROPS namelist.
  l_holdwater = .FALSE.
      ! Switch to control how supersaturated and negative soil moisture is
      ! handled in the implicit calculation. FALSE: excess/required moisture
      ! is pushed out/in from the base of the soil. TRUE: water is added/
      ! taken from an adjacent layer.

#if !defined(UM_JULES)
LOGICAL ::                                                                     &
! Switches that are only present in standalone JULES.
  l_broadcast_ancils = .FALSE.
      ! Switch to allow soil ancillaries to be read in as gridbox values and
      ! broadcast to all soil tiles
#endif

INTEGER ::                                                                     &
  soilhc_method = 1
      ! Switch for the calculation method of soil thermal conductivity
      !   SOILHC_METHOD=1: Method of Cox et al (1999)
      !   SOILHC_METHOD=2: Simplified Johansen (1975)
      !   SOILHC_METHOD=3: Chadburn et al. (2015) best for organic soils


!-----------------------------------------------------------------------------
! Fixed parameters
!-----------------------------------------------------------------------------
! Thermal conductivities (Source: "The Frozen Earth" p.90)
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  hcair = 0.025,                                                               &
      ! Thermal conductivity of air (W/m/K)
  hcice = 2.24,                                                                &
      ! Thermal conductivity of ice (W/m/K)
  hcwat = 0.56
      ! Thermal conductivity of liquid water (W/m/K)

! Parameters for soil moisture update
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  gamma_w = 1.0
      ! Forward timestep weighting

! Parameters for soil temperature update
INTEGER, PARAMETER ::                                                          &
  mmax = 3
      ! Maximum number of iterations on temperature

REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  facur = 0.01,                                                                &
      ! Required flux conservation accuracy (W/m2)
  gamma_t = 1.0,                                                               &
      ! Forward timestep weighting
  tacur = 0.00000
      ! Required accuracy of temperature calculation (celsius)


!-----------------------------------------------------------------------------
! Parameters that can be set by the namelist
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  cs_min = rmdi,                                                               &
      ! Minimum soil carbon (kg C/m2)
  zsmc = rmdi,                                                                 &
      ! Depth of layer over which soil moisture diagnostic is averaged (m)
  zst = rmdi,                                                                  &
      ! Depth of layer over which soil temperature diagnostic is averaged (m)
  confrac = rmdi
      ! Fraction of the gridbox over which convective precipitation is
      ! assumed to fall

!-----------------------------------------------------------------------------
! Bedrock parameters
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  ns_deep = imdi
      ! Number of bedrock layers

REAL(KIND=real_jlslsm) ::                                                      &
  hcapdeep = rmdi,                                                             &
      ! Heat capacity of bedrock
  hcondeep = rmdi,                                                             &
      ! Thermal conductivity of bedrock
  dzdeep = rmdi
      ! Thickness of bedrock layers


!-----------------------------------------------------------------------------
! Variable length arrays that can be set using the namelist
!-----------------------------------------------------------------------------
! We provide a fixed-length version that is read by the namelist, then point
! the actual version to the portion of it we will use
REAL(KIND=real_jlslsm), POINTER ::                                             &
  dzsoil(:)
      ! Thicknesses of the soil layers (m)

REAL(KIND=real_jlslsm), TARGET ::                                              &
  dzsoil_io(sm_levels_max)
      ! Fixed length equivalent of dzsoil for namelist IO

! Initialise dzsoil_io to negative values
! In check_jules_soil, we verify that all values of dzsoil we will use are >= 0
DATA dzsoil_io / sm_levels_max * -1.0 /

REAL(KIND=real_jlslsm)  ::                                                     &
  dzsoil_elev = -1.0
      ! Depth of tiled bedrock subsurfaces under elevated tiles (m)

!-----------------------------------------------------------------------------
! Namelist definition for UM and standalone
!-----------------------------------------------------------------------------
NAMELIST  / jules_soil/                                                        &
#if !defined(UM_JULES)
! Additional parameters for standalone JULES only (for soil tiling).
    l_broadcast_ancils,                                                        &
#endif
! Soil levels
    sm_levels,                                                                 &
! Switches
    l_vg_soil, l_dpsids_dsdz, l_soil_sat_down, soilhc_method, l_bedrock,       &
    l_holdwater, l_tile_soil,                                                  &
! Parameters
    cs_min, zsmc, zst, confrac, ns_deep, hcapdeep, hcondeep,                   &
    dzdeep, dzsoil_io, dzsoil_elev



CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_SOIL_MOD'

CONTAINS

#if !defined(RIVERS_ONLY)
SUBROUTINE check_jules_soil()

USE jules_irrig_mod, ONLY: l_irrig_dmd
USE jules_surface_mod, ONLY: l_elev_land_ice
USE ereport_mod, ONLY: ereport

!-----------------------------------------------------------------------------
! Description:
!   Checks JULES_SOIL namelist for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER ::                                                 &
   RoutineName = 'CHECK_JULES_SOIL'   ! Name of this procedure.

INTEGER :: errorstatus

! Set error status to show a fatal error for all checks.
errorstatus = 101

! Check that sm_levels has been set
IF ( sm_levels < 1 ) THEN
  errorstatus = 101
  CALL ereport("check_jules_soil", errorstatus,                                &
               "sm_levels must be between 1 and sm_levels_max")
END IF

! Check that there are not too many levels
IF ( sm_levels > sm_levels_max ) THEN
  CALL ereport(RoutineName, errorstatus,                                       &
    "Too many soil layers specified - increase sm_levels_max and recompile")
END IF

! Check a suitable soilhc_method was given
IF ( soilhc_method < 1 .OR. soilhc_method > 3 ) THEN
  CALL ereport(RoutineName, errorstatus,                                       &
    "soilhc_method must be 1, 2 or 3")
END IF

! Associate the dzsoil pointer with the appropriate section of dzsoil_io
dzsoil => dzsoil_io(1:sm_levels)

! Check we have sensible values for dzsoil
IF ( ANY(dzsoil(:) < 0.0) ) THEN
  CALL ereport(RoutineName, errorstatus,                                       &
               "dzsoil < 0 - check namelist jules_soil")
END IF

IF (l_elev_land_ice .AND. dzsoil_elev < 0.01) THEN
  CALL ereport(RoutineName, errorstatus,                                       &
               "dzsoil_elev < 0.01 - check namelist jules_soil")
END IF

! check that confrac is set and is between 0 and 1
IF ( ABS( confrac - rmdi ) < EPSILON(1.0) ) THEN
  CALL ereport(RoutineName, errorstatus, 'confrac not found')
ELSE IF ( confrac < 0.0 .OR. confrac > 1.0 ) THEN
  CALL ereport(RoutineName, errorstatus,                                       &
                 'confrac must lie in the range 0 to 1')
END IF

! check that cs_min is set and > 1.0e-6
IF ( ABS( cs_min - rmdi ) < EPSILON(1.0) ) THEN
  CALL ereport(RoutineName, errorstatus, 'cs_min not found')
ELSE IF ( cs_min < 1.0e-6 ) THEN
  CALL ereport(RoutineName, errorstatus,                                       &
                 'cs_min must be greater than 1.0e-6')
END IF

! Check that zsmc and zst are set and within soil depth.
IF ( ABS( zsmc - rmdi ) < EPSILON(1.0) ) THEN
  CALL ereport(RoutineName, errorstatus, 'zsmc not found')
ELSE IF ( SUM(dzsoil(:)) < zsmc ) THEN
  CALL ereport(RoutineName, errorstatus,                                       &
               "zsmc is below bottom of soil column")
END IF

IF ( ABS( zst - rmdi ) < EPSILON(1.0) ) THEN
  CALL ereport(RoutineName, errorstatus, 'zst not found')
ELSE IF ( SUM(dzsoil(:)) < zst ) THEN
  CALL ereport(RoutineName, errorstatus,                                       &
               "zst is below bottom of soil column")
END IF

IF ( l_irrig_dmd .AND. sm_levels < 2 ) THEN
  CALL ereport(RoutineName, errorstatus,                                       &
               'Irrigation demand (l_irrig_dmd=T) can only be used with'//     &
               '2 or more soil levels (sm_levels >= 2)')
END IF

IF ( l_bedrock ) THEN
  ! Check ns_deep, dzdeep, hcondeep. hcapdeep are set and valid
  IF ( ABS( ns_deep - rmdi ) < EPSILON(1.0) ) THEN
    CALL ereport(RoutineName, errorstatus, 'ns_deep not found')
  ELSE IF ( ns_deep < 1 ) THEN
    CALL ereport(RoutineName, errorstatus,                                     &
                 'Bedrock must have at least one layer')
  END IF

  IF ( ABS( dzdeep - rmdi ) < EPSILON(1.0) ) THEN
    CALL ereport(RoutineName, errorstatus, 'dzdeep not found')
  ELSE IF ( dzdeep < 0.01 ) THEN
    CALL ereport(RoutineName, errorstatus,                                     &
                 'layer thickness for bedrock (dzdeep) must be > 0.01')
  END IF

  IF ( ABS( hcondeep - rmdi ) < EPSILON(1.0) ) THEN
    CALL ereport(RoutineName, errorstatus, 'hcondeep not found')
  ELSE IF ( hcondeep < 0.4 .OR. hcondeep > 12.0 ) THEN
    CALL ereport(RoutineName, errorstatus,                                     &
                   'hcondeep must lie in the range 0.4 to 12.0')
  END IF

  IF ( ABS( hcapdeep - rmdi ) < EPSILON(1.0) ) THEN
    CALL ereport(RoutineName, errorstatus, 'hcapdeep not found')
  ELSE IF (hcapdeep < 100000.0 .OR. hcapdeep > 8000000.0 ) THEN
    CALL ereport(RoutineName, errorstatus,                                     &
                   'hcapdeep must lie in the range 100000 to 8000000')
  END IF
END IF  ! end if l_bedrock

END SUBROUTINE check_jules_soil
#endif

SUBROUTINE print_nlist_jules_soil()

USE jules_print_mgr, ONLY: jules_print

IMPLICIT NONE

CHARACTER(LEN=50000) :: lineBuffer


!-----------------------------------------------------------------------------


CALL jules_print('jules_soil', 'Contents of namelist jules_soil')

WRITE(lineBuffer, *) '  l_vg_soil = ', l_vg_soil
CALL jules_print('jules_soil', lineBuffer)

WRITE(lineBuffer, *) '  l_dpsids_dsdz = ', l_dpsids_dsdz
CALL jules_print('jules_soil', lineBuffer)

WRITE(lineBuffer, *) '  l_soil_sat_down = ', l_soil_sat_down
CALL jules_print('jules_soil', lineBuffer)

WRITE(lineBuffer, *) '  l_holdwater = ', l_holdwater
CALL jules_print('jules_soil', lineBuffer)

WRITE(lineBuffer, *) '  l_bedrock = ', l_bedrock
CALL jules_print('jules_soil', lineBuffer)

WRITE(lineBuffer, *) '  l_tile_soil = ', l_tile_soil
CALL jules_print('jules_soil', lineBuffer)

WRITE(lineBuffer, *) '  soilhc_method = ', soilhc_method
CALL jules_print('jules_soil', lineBuffer)

WRITE(lineBuffer, *) '  cs_min = ', cs_min
CALL jules_print('jules_soil', lineBuffer)

WRITE(lineBuffer, *) '  zsmc = ', zsmc
CALL jules_print('jules_soil', lineBuffer)

WRITE(lineBuffer, *) '  zst = ', zst
CALL jules_print('jules_soil', lineBuffer)

WRITE(lineBuffer, *) '  confrac = ', confrac
CALL jules_print('jules_soil', lineBuffer)

WRITE(lineBuffer, *) '  ns_deep = ', ns_deep
CALL jules_print('jules_soil', lineBuffer)

WRITE(lineBuffer, *) '  hcapdeep = ', hcapdeep
CALL jules_print('jules_soil', lineBuffer)

WRITE(lineBuffer, *) '  hcondeep = ', hcondeep
CALL jules_print('jules_soil', lineBuffer)

WRITE(lineBuffer, *) '  dzdeep = ', dzdeep
CALL jules_print('jules_soil', lineBuffer)

WRITE(lineBuffer, *) '  dzsoil_io = ', dzsoil_io
CALL jules_print('jules_soil', lineBuffer)

WRITE(lineBuffer, *) '  dzsoil_elev = ', dzsoil_elev
CALL jules_print('jules_soil', lineBuffer)

CALL jules_print('jules_soil',                                                 &
    '- - - - - - end of namelist - - - - - -')

END SUBROUTINE print_nlist_jules_soil


#if defined(UM_JULES) && !defined(LFRIC)
SUBROUTINE read_nml_jules_soil (unitnumber)

! Description:
!  Read the JULES_SOIL namelist

USE setup_namelist, ONLY: setup_nml_type
USE check_iostat_mod, ONLY: check_iostat
USE UM_parcore,       ONLY: mype

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

INTEGER :: my_comm
INTEGER :: mpl_nml_type
INTEGER :: ErrorStatus
INTEGER :: icode
CHARACTER(LEN=errormessagelength) :: iomessage
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_SOIL'
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 3
INTEGER, PARAMETER :: n_int = 3
INTEGER, PARAMETER :: n_real = 8 + sm_levels_max
INTEGER, PARAMETER :: n_log = 6

TYPE :: my_namelist
  SEQUENCE
  INTEGER :: sm_levels
  INTEGER :: soilhc_method
  INTEGER :: ns_deep
  REAL(KIND=real_jlslsm) ::cs_min
  REAL(KIND=real_jlslsm) ::zsmc
  REAL(KIND=real_jlslsm) ::zst
  REAL(KIND=real_jlslsm) ::confrac
  REAL(KIND=real_jlslsm) :: hcapdeep
  REAL(KIND=real_jlslsm) :: hcondeep
  REAL(KIND=real_jlslsm) :: dzdeep
  REAL(KIND=real_jlslsm) ::dzsoil_io(sm_levels_max)
  REAL(KIND=real_jlslsm) ::dzsoil_elev
  LOGICAL :: l_vg_soil
  LOGICAL :: l_dpsids_dsdz
  LOGICAL :: l_soil_sat_down
  LOGICAL :: l_holdwater
  LOGICAL :: l_bedrock
  LOGICAL :: l_tile_soil
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type, n_int_in = n_int,               &
                    n_real_in = n_real, n_log_in = n_log)

IF (mype == 0) THEN

  READ (UNIT = unitnumber, NML = jules_soil, IOSTAT = errorstatus,             &
        IOMSG = iomessage)
  CALL check_iostat(errorstatus, "namelist jules_soil", iomessage)

  my_nml % sm_levels       = sm_levels
  my_nml % soilhc_method   = soilhc_method
  my_nml % ns_deep         = ns_deep
  my_nml % cs_min          = cs_min
  my_nml % zsmc            = zsmc
  my_nml % zst             = zst
  my_nml % confrac         = confrac
  my_nml % hcapdeep        = hcapdeep
  my_nml % hcondeep        = hcondeep
  my_nml % dzdeep          = dzdeep
  my_nml % dzsoil_io       = dzsoil_io
  my_nml % dzsoil_elev     = dzsoil_elev
  my_nml % l_vg_soil       = l_vg_soil
  my_nml % l_dpsids_dsdz   = l_dpsids_dsdz
  my_nml % l_soil_sat_down = l_soil_sat_down
  my_nml % l_holdwater     = l_holdwater
  my_nml % l_bedrock       = l_bedrock
  my_nml % l_tile_soil     = l_tile_soil

END IF

CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN

  sm_levels       = my_nml % sm_levels
  soilhc_method   = my_nml % soilhc_method
  ns_deep         = my_nml % ns_deep
  cs_min          = my_nml % cs_min
  zsmc            = my_nml % zsmc
  zst             = my_nml % zst
  confrac         = my_nml % confrac
  hcapdeep        = my_nml % hcapdeep
  hcondeep        = my_nml % hcondeep
  dzdeep          = my_nml % dzdeep
  dzsoil_io       = my_nml % dzsoil_io
  dzsoil_elev     = my_nml % dzsoil_elev
  l_vg_soil       = my_nml % l_vg_soil
  l_dpsids_dsdz   = my_nml % l_dpsids_dsdz
  l_soil_sat_down = my_nml % l_soil_sat_down
  l_holdwater     = my_nml % l_holdwater
  l_bedrock       = my_nml % l_bedrock
  l_tile_soil     = my_nml % l_tile_soil
END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_nml_jules_soil
#endif

END MODULE jules_soil_mod
