#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Wrapper module containing subroutines for reading JULES namelists
!
MODULE read_jules_namelists_mod

! Description:
!  Contains read_jules_<namelist> and read_<urban_namelist> subroutines
!  for reading namelists into JULES during a UM-JULES job.
!
! Method:
!  The unit number holding the namelist is passed as the sole argument
!  to each file.
!
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
! This file belongs in section: top_level
!
! Code Description:
!  Language: FORTRAN 95.
!  This code is written to UMDP3 v8.5 programming standards.

USE check_iostat_mod, ONLY:                                                    &
  check_iostat

USE ereport_mod,      ONLY: ereport

USE umPrintMgr     ,  ONLY:                                                    &
  PrintStatus, PrStatus_Oper

USE UM_ParCore,       ONLY:                                                    &
  mype

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

INTEGER(KIND=jpim), PARAMETER, PRIVATE :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER, PRIVATE :: zhook_out = 1

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='READ_JULES_NAMELISTS_MOD'

CONTAINS

SUBROUTINE read_jules_elevate (unitnumber)

! Description:
!  Read the JULES_ELEVATE namelist

USE c_elevate,        ONLY:                                                    &
  print_nlist_jules_elevate, read_nml_jules_elevate

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_ELEVATE'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_elevate(unitnumber)

IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_elevate()
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_elevate

! *********************************************************************

SUBROUTINE read_jules_hydrology (unitnumber)

! Description:
!  Read the JULES_HYDROLOGY namelist

USE jules_hydrology_mod,  ONLY:                                                &
  print_nlist_jules_hydrology, check_jules_hydrology,                          &
  read_nml_jules_hydrology

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_HYDROLOGY'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_hydrology(unitnumber)
IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_hydrology()
END IF
CALL check_jules_hydrology()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_hydrology

! *********************************************************************

SUBROUTINE read_jules_deposition (unitnumber)

! Description:
!  Read the JULES_DEPOSITION control namelist

USE jules_deposition_mod,  ONLY:                                               &
  print_nlist_jules_deposition, check_jules_deposition,                        &
  read_nml_jules_deposition

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_DEPOSITION'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_deposition(unitnumber)
IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_deposition()
END IF
CALL check_jules_deposition()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_deposition

! *********************************************************************

SUBROUTINE read_jules_deposition_species (unitnumber)

! Description:
!  Read the JULES_DEPOSITION_OTHER and JULES_DEPOSITION_SPECIES namelists

USE jules_deposition_mod,       ONLY:                                          &
  dry_dep_model, dry_dep_model_ukca, dry_dep_model_jules

USE deposition_species_io_mod,  ONLY:                                          &
  print_nlist_jules_deposition_species,                                        &
  read_nml_jules_deposition_species

USE deposition_species_specific_io_mod,  ONLY:                                 &
  print_nlist_jules_deposition_species_specific,                               &
  read_nml_jules_deposition_species_specific

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber
INTEGER             :: errorstatus

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_DEPOSITION_SPECIES'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

errorstatus = dry_dep_model
SELECT CASE ( dry_dep_model )

CASE ( dry_dep_model_ukca, dry_dep_model_jules )
  ! Species deposition parameter values from namelists
  CALL read_nml_jules_deposition_species(unitnumber)
  IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
    CALL print_nlist_jules_deposition_species()
  END IF

  CALL read_nml_jules_deposition_species_specific(unitnumber)
  IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
    CALL print_nlist_jules_deposition_species_specific()
  END IF
  ! Check is on deposition science variables

CASE DEFAULT
  CALL ereport (RoutineName, errorstatus,                                      &
               "Invalid value for dry_dep_model. Should be 1 or 2." )

END SELECT

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_deposition_species

! *********************************************************************

SUBROUTINE read_jules_nvegparm (unitnumber)

! Description:
!  read the JULES_NVEGPARM namelist

USE nvegparm_io,      ONLY:                                                    &
  read_nml_jules_nvegparm

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_NVEGPARM'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_nvegparm(unitnumber)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_nvegparm

! *********************************************************************

SUBROUTINE read_jules_overbank (unitnumber)

! Description:
!  Read the JULES_OVERBANK namelist

USE overbank_inundation_mod,  ONLY:                                            &
  print_nlist_jules_overbank, check_jules_overbank,                            &
  read_nml_jules_overbank

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_OVERBANK'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_overbank(unitnumber)
IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_overbank()
END IF
CALL check_jules_overbank()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_overbank

! *********************************************************************

SUBROUTINE read_jules_pftparm (unitnumber)

! Description:
!  Read the JULES_PFTPARM namelist

USE pftparm_io,       ONLY:                                                    &
  read_nml_jules_pftparm

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_PFTPARM'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_pftparm(unitnumber)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_pftparm

! *********************************************************************

SUBROUTINE read_jules_red (unitnumber)

! Description:
!  Read the JULES_RED namelist

USE red_io, ONLY:                                                              &
  print_nlist_jules_red, read_nml_jules_red

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_RED'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_red(unitnumber)

IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_red()
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_red

! *********************************************************************

SUBROUTINE read_jules_radiation (unitnumber)

! Description:
!  Read the JULES_RADIATION namelist

USE jules_radiation_mod,  ONLY:                                                &
  print_nlist_jules_radiation, check_jules_radiation,                          &
  read_nml_jules_radiation

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_RADIATION'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_radiation(unitnumber)
IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_radiation()
END IF
CALL check_jules_radiation()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_radiation

! *********************************************************************

SUBROUTINE read_jules_rivers (unitnumber)

! Description:
!  Read the JULES_RIVERS namelist

USE jules_rivers_mod,  ONLY:                                                   &
  print_nlist_jules_rivers, check_jules_rivers,                                &
  read_nml_jules_rivers

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_RIVERS'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_rivers(unitnumber)
IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_rivers()
END IF
CALL check_jules_rivers()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_rivers

! *********************************************************************

SUBROUTINE read_jules_sea_seaice (unitnumber)

! Description:
!  Read the jules_sea_seaice namelist

USE jules_sea_seaice_mod,  ONLY:                                               &
  print_nlist_jules_sea_seaice, check_jules_sea_seaice,                        &
  read_nml_jules_sea_seaice

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_SEA_SEAICE'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_sea_seaice(unitnumber)
IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_sea_seaice()
END IF
CALL check_jules_sea_seaice()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_sea_seaice

! *********************************************************************

SUBROUTINE read_jules_snow (unitnumber)

! Description:
!  Read the JULES_SNOW namelist

USE jules_snow_mod,  ONLY:                                                     &
  print_nlist_jules_snow, check_jules_snow,                                    &
  read_nml_jules_snow

USE land_tile_ids_mod, ONLY: set_ml_snow_type_ids

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_SNOW'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_snow(unitnumber)
IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_snow()
END IF
CALL check_jules_snow()
! Sets multilayer snow indices for headers
CALL set_ml_snow_type_ids()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_snow

! *********************************************************************

SUBROUTINE read_jules_soil (unitnumber)

! Description:
!  Read the JULES_SOIL namelist

USE jules_soil_mod,  ONLY:                                                     &
  print_nlist_jules_soil,                                                      &
  read_nml_jules_soil

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_SOIL'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_soil(unitnumber)
IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_soil()
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_soil

! *********************************************************************

SUBROUTINE read_jules_soil_biogeochem (unitnumber)

! Description:
!  Read the JULES_SOIL_BIOGEOCHEM namelist

USE jules_soil_biogeochem_mod,  ONLY:                                          &
  print_nlist_jules_soil_biogeochem,                                           &
  check_jules_soil_biogeochem, read_nml_jules_soil_biogeochem

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_SOIL_BIOGEOCHEM'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_soil_biogeochem(unitnumber)
IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_soil_biogeochem()
END IF
CALL check_jules_soil_biogeochem()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_soil_biogeochem

! *********************************************************************

SUBROUTINE read_jules_surface (unitnumber)

! Description:
!  Read the jules_surface namelist

USE jules_surface_mod,  ONLY:                                                  &
  print_nlist_jules_surface, check_jules_surface,                              &
  read_nml_jules_surface

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_SURFACE'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_surface(unitnumber)
IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_surface()
END IF
CALL check_jules_surface()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_surface

! *********************************************************************

SUBROUTINE read_jules_surface_types (unitnumber)

! Description:
!  Read the JULES_SURFACE_TYPES namelist

USE jules_surface_types_mod,  ONLY:                                            &
  print_nlist_jules_surface_types,                                             &
  check_jules_surface_types, read_nml_jules_surface_types,                     &
  set_derived_variables_jules_surface_types

USE land_tile_ids_mod, ONLY: set_surface_type_ids

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_SURFACE_TYPES'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_surface_types(unitnumber)
CALL set_derived_variables_jules_surface_types()
IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_surface_types()
END IF
! Set the surface_type_ids array and carry out additional checks
CALL set_surface_type_ids()
CALL check_jules_surface_types()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_surface_types

! *********************************************************************

SUBROUTINE read_jules_triffid (unitnumber)

! Description:
!  Read the JULES_TRIFFID namelist

USE trif_io,          ONLY:                                                    &
  print_nlist_jules_triffid, read_nml_jules_triffid

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_TRIFFID'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_triffid(unitnumber)

IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_triffid()
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_triffid

! *********************************************************************

SUBROUTINE read_jules_vegetation (unitnumber)

! Description:
!  Read the JULES_VEGETATION namelist

USE jules_vegetation_mod,  ONLY:                                               &
  print_nlist_jules_vegetation,                                                &
  check_jules_vegetation, read_nml_jules_vegetation

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_VEGETATION'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_vegetation(unitnumber)
IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_vegetation()
END IF
CALL check_jules_vegetation()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_vegetation

! *********************************************************************

SUBROUTINE read_jules_urban (unitnumber)

! Description:
!  Read the JULES_URBAN namelist

USE jules_urban_mod, ONLY: jules_urban, print_nlist_jules_urban,               &
    read_nml_jules_urban, check_jules_urban

USE jules_surface_mod, ONLY: l_urban2t

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_URBAN'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF ( l_urban2t ) CALL read_nml_jules_urban(unitnumber)

IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_urban()
END IF
CALL check_jules_urban()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_urban

! *********************************************************************

SUBROUTINE read_jules_irrigation (unitnumber)

! Description:
!  Read the JULES_IRRIG namelist

USE jules_irrig_mod,   ONLY:                                                   &
  print_nlist_jules_irrig,                                                     &
  read_nml_jules_irrig, check_jules_irrig

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_IRRIGATION'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_irrig(unitnumber)
IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_irrig()
END IF
CALL check_jules_irrig()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_irrigation


! *********************************************************************

SUBROUTINE read_jules_model_environment (unitnumber)

! Description:
!  Read the jules_model_environment namelist

USE jules_model_environment_mod,  ONLY:                                        &
    print_nlist_jules_model_environment, check_jules_model_environment,        &
    read_nml_jules_model_environment

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_MODEL_ENVIRONMENT'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_model_environment(unitnumber)
IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_model_environment()
END IF
CALL check_jules_model_environment()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_model_environment

! *********************************************************************

SUBROUTINE read_jules_water_resources (unitnumber)

! Description:
!  Read the jules_water_resources namelist

USE jules_water_resources_mod,  ONLY:                                          &
    print_nlist_jules_water_resources, check_jules_water_resources,            &
    read_nml_jules_water_resources

USE jules_hydrology_mod, ONLY: l_top

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_WATER_RESOURCES'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_water_resources(unitnumber)
IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_water_resources()
END IF
CALL check_jules_water_resources( l_top )

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_water_resources

END MODULE read_jules_namelists_mod
#endif
