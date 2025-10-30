!-----------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

MODULE cable_surface_types_mod

!-----------------------------------------------------------------------------
! Description:
!   Contains CABLE surface type information and a namelist for setting them
!   The CABLE equivalent of jules_surface_types_mod
!
! This MODULE is USEd by:
!      surf_couple_radiation_mod.F90,
!      init_surface_types.F90
!
! This MODULE contains public Subroutines:
!      check_cable_surface_types,
!      print_nlist_cable_surface_types,
!      read_nml_cable_surface_types,
!      set_derived_variables_cable_surface_types
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in CABLE SCIENCE
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE missing_data_mod, ONLY: imdi

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Module variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  nnpft_cable,                                                                 &
                ! Number of natural pfts
                !   Derived from namelist inputs as npft_cable - ncpft_cable
  ncpft_cable = 0,                                                             &
                ! Number of crop pfts
  npft_cable  = imdi,                                                          &
                ! Number of plant functional types
  nnvg_cable  = imdi,                                                          &
                ! Number of non-vegetation surface types
  ntype_cable,                                                                 &
                ! Total number of surface types
  lakes_cable  = imdi,                                                         &
                ! Index of the CABLE lakes surface type
  ice_cable  = imdi,                                                           &
                ! Index of the CABLE ice surface type
  urban_cable  = imdi,                                                         &
                ! Index of the CABLE urban surface type
  barren_cable = imdi
                ! Index of the barren surface type

!-----------------------------------------------------------------------------
! UM only: The following are only required for UM flexible tiles.
!-----------------------------------------------------------------------------
! The UM uses the following to identify the original PFTs in the UM output.

INTEGER ::                                                                     &
   evergreen_needleleaf = imdi,                                                &
                  ! Index of surface type 'evergreen needleleaf'
   evergreen_broadleaf  = imdi,                                                &
                  ! Index of surface type 'evergreen broadleaf'
   deciduous_needleleaf = imdi,                                                &
                  ! Index of surface type 'deciduous needleleaf'
   deciduous_broadleaf  = imdi,                                                &
                  ! Index of surface type 'deciduous broadleaf'
   shrub_cable          = imdi,                                                &
                  ! Index of surface type 'shrub'
   c3_grassland = imdi,                                                        &
                  ! Index of surface type 'C3 grassland'
   c4_grassland = imdi,                                                        &
                  ! Index of surface type 'C4 grassland'
   tundra = imdi,                                                              &
                  ! Index of surface type 'tundra'
   c3_cropland = imdi,                                                         &
                  ! Index of surface type 'C3 cropland'
   c4_cropland = imdi,                                                         &
                  ! Index of surface type 'C4 cropland'
   wetland = imdi,                                                             &
                  ! Index of surface type 'wetland'
   empty1 = imdi,                                                              &
                  ! Index of surface type 'empty 1'
   empty2 = imdi
                  ! Index of surface type 'empty 2'


!-----------------------------------------------------------------------------
! Single namelist definition for UM and standalone
!-----------------------------------------------------------------------------
NAMELIST  / cable_surface_types/                                               &
    npft_cable, ncpft_cable, nnvg_cable,                                       &
    evergreen_needleleaf, evergreen_broadleaf, deciduous_needleleaf,           &
    deciduous_broadleaf, shrub_cable, c3_grassland, c4_grassland,              &
    tundra, c3_cropland, c4_cropland, wetland, empty1, empty2,                 &
    barren_cable, urban_cable, lakes_cable, ice_cable

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='CABLE_SURFACE_TYPES_MOD'

CONTAINS

SUBROUTINE check_cable_surface_types()

! Description:
!   Checks CABLE_SURFACE_TYPES namelist for consistency

USE max_dimensions, ONLY: npft_max, ncpft_max, nnvg_max

USE ereport_mod, ONLY: ereport

IMPLICIT NONE

INTEGER :: i ! Loop counter

INTEGER :: errorstatus

!-----------------------------------------------------------------------------
! Check that the given values are less than the fixed values for IO
!-----------------------------------------------------------------------------
IF ( npft_cable > npft_max ) THEN
  errorstatus = 101
  CALL ereport("check_jules_surface_types", errorstatus,                       &
               "npft_cable > npft_max - increase npft_max and recompile")
END IF
IF ( ncpft_cable > ncpft_max ) THEN
  errorstatus = 101
  CALL ereport("check_jules_surface_types", errorstatus,                       &
               "ncpft_cable > ncpft_max - increase ncpft_max and recompile")
END IF
IF ( nnvg_cable > nnvg_max ) THEN
  errorstatus = 101
  CALL ereport("check_jules_surface_types", errorstatus,                       &
               "nnvg_cable > nnvg_max - increase nnvg_max and recompile")
END IF
IF ( ncpft_cable > npft_cable ) THEN
  errorstatus = 101
  CALL ereport("check_jules_surface_types", errorstatus,                       &
               "ncpft_cable > npft_cable - total number of PFTs must be >= "   &
               // "number of crop PFTs")
END IF

! If these are true, then we automatically have:
!   ntype_cable <= ntype_max (since ntype_cable(_max) = npft_cable(_max) +
!   nnvg_cable(_max))
!   nnpft_cable <= nnpft_max (since nnpft_cable(_max) = npft_cable(_max) -
!   ncpft_cable(_max))

!-----------------------------------------------------------------------------
! Check values for the specific surface types are sensible
!-----------------------------------------------------------------------------
! PFT surface types must come before non-veg types, so if urban_cable,
! lakes_cable or ice_cable are given (i.e. > 0) then they must be > npft_cable
! A soil type is required
IF ( urban_cable > 0 .AND.                                                     &
     ( urban_cable <= npft_cable .OR. urban_cable > ntype_cable ) ) THEN
  errorstatus = 101
  CALL ereport("check_cable_surface_types", errorstatus,                       &
               "urban_cable tile is given but is out of range")
END IF

IF ( lakes_cable > 0 .AND.                                                     &
     ( lakes_cable <= npft_cable .OR. lakes_cable > ntype_cable ) ) THEN
  errorstatus = 101
  CALL ereport("check_cable_surface_types", errorstatus,                       &
               "lakes_cable tile is given but is out of range")
END IF

IF ( ice_cable > 0 .AND. ( ice_cable <= npft_cable .OR. ice_cable > ntype_cable ) ) THEN
  errorstatus = 101
  CALL ereport("check_cable_surface_types", errorstatus,                       &
               "ice_cable tile is given but is out of range")
END IF

END SUBROUTINE check_cable_surface_types


SUBROUTINE print_nlist_cable_surface_types()

USE jules_print_mgr, ONLY: jules_print

! Description:
!   Nicely prints the namelist content to log file

IMPLICIT NONE

INTEGER :: i ! Loop counter

CHARACTER(LEN=50000) :: lineBuffer


!-----------------------------------------------------------------------------

CALL jules_print('cable_surface_types',                                        &
                 'Contents of namelist cable_surface_types')

WRITE(lineBuffer, *) '  npft_cable = ', npft_cable
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  nnvg_cable = ', nnvg_cable
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  evergreen_needleleaf = ', evergreen_needleleaf
CALL jules_print('cable_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  evergreen_broadleaf = ', evergreen_broadleaf
CALL jules_print('cable_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  deciduous_needleleaf = ', deciduous_broadleaf
CALL jules_print('cable_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  shrub_cable = ', shrub_cable
CALL jules_print('cable_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  c3_grassland = ', c3_grassland
CALL jules_print('cable_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  c4_grassland = ', c4_grassland
CALL jules_print('cable_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  tundra = ', tundra
CALL jules_print('cable_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  c3_cropland = ', c3_cropland
CALL jules_print('cable_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  c4_cropland = ', c4_cropland
CALL jules_print('cable_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  wetland = ', wetland
CALL jules_print('cable_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  empty1 = ', empty1
CALL jules_print('cable_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  empty2 = ', empty2
CALL jules_print('cable_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  barren_cable = ', barren_cable
CALL jules_print('cable_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  urban_cable = ', urban_cable
CALL jules_print('cable_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  lakes_cable = ', lakes_cable
CALL jules_print('cable_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  ice_cable = ', ice_cable
CALL jules_print('cable_surface_types', lineBuffer)

CALL jules_print('cable_surface_types',                                        &
    '- - - - - - end of namelist - - - - - -')

END SUBROUTINE print_nlist_cable_surface_types

#if defined(UM_JULES) && !defined(LFRIC)
SUBROUTINE read_nml_cable_surface_types (unitnumber)

USE setup_namelist,   ONLY: setup_nml_type
USE check_iostat_mod, ONLY: check_iostat
USE UM_parcore,       ONLY: mype
USE parkind1,         ONLY: jprb, jpim
USE yomhook,          ONLY: lhook, dr_hook
USE errormessagelength_mod, ONLY: errormessagelength

! Description:
!  Read the CABLE_SURFACE_TYPES namelist

IMPLICIT NONE

INTEGER, INTENT(IN) :: unitnumber
INTEGER :: my_comm
INTEGER :: mpl_nml_type
INTEGER :: ErrorStatus
INTEGER :: icode
CHARACTER(LEN=errormessagelength) :: iomessage
REAL(KIND=jprb) :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_CABLE_SURFACE_TYPES'
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 1
INTEGER, PARAMETER :: n_int = 20

TYPE :: my_namelist
  SEQUENCE
  INTEGER :: npft_cable
  INTEGER :: ncpft_cable
  INTEGER :: nnvg_cable
  INTEGER :: evergreen_needleleaf
  INTEGER :: evergreen_broadleaf
  INTEGER :: deciduous_needleleaf
  INTEGER :: deciduous_broadleaf
  INTEGER :: shrub_cable
  INTEGER :: c3_grassland
  INTEGER :: c4_grassland
  INTEGER :: tundra
  INTEGER :: c3_cropland
  INTEGER :: c4_cropland
  INTEGER :: wetland
  INTEGER :: empty1
  INTEGER :: empty2
  INTEGER :: barren_cable
  INTEGER :: urban_cable
  INTEGER :: lakes_cable
  INTEGER :: ice_cable
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type, n_int_in = n_int)

IF (mype == 0) THEN

  READ (UNIT = unitnumber, NML = cable_surface_types, IOSTAT = errorstatus,    &
        IOMSG = iomessage)
  CALL check_iostat(errorstatus, "namelist cable_surface_types",iomessage)

  my_nml % npft_cable           = npft_cable
  my_nml % ncpft_cable          = ncpft_cable
  my_nml % nnvg_cable           = nnvg_cable
  my_nml % evergreen_needleleaf = evergreen_needleleaf
  my_nml % evergreen_broadleaf  = evergreen_broadleaf
  my_nml % deciduous_needleleaf = deciduous_needleleaf
  my_nml % deciduous_broadleaf  = deciduous_broadleaf
  my_nml % shrub_cable          = shrub_cable
  my_nml % c3_grassland         = c3_grassland
  my_nml % c4_grassland         = c4_grassland
  my_nml % tundra               = tundra
  my_nml % c3_cropland          = c3_cropland
  my_nml % c4_cropland          = c4_cropland
  my_nml % wetland              = wetland
  my_nml % empty1               = empty1
  my_nml % empty2               = empty2
  my_nml % barren_cable         = barren_cable
  my_nml % urban_cable          = urban_cable
  my_nml % lakes_cable          = lakes_cable
  my_nml % ice_cable            = ice_cable
END IF

CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN

  npft_cable           = my_nml % npft_cable
  ncpft_cable          = my_nml % ncpft_cable
  nnvg_cable           = my_nml % nnvg_cable
  evergreen_needleleaf = my_nml % evergreen_needleleaf
  evergreen_broadleaf  = my_nml % evergreen_broadleaf
  deciduous_needleleaf = my_nml % deciduous_needleleaf
  deciduous_broadleaf  = my_nml % deciduous_broadleaf
  shrub_cable          = my_nml % shrub_cable
  c3_grassland         = my_nml % c3_grassland
  c4_grassland         = my_nml % c4_grassland
  tundra               = my_nml % tundra
  c3_cropland          = my_nml % c3_cropland
  c4_cropland          = my_nml % c4_cropland
  wetland              = my_nml % wetland
  empty1               = my_nml % empty1
  empty2               = my_nml % empty2
  barren_cable         = my_nml % barren_cable
  urban_cable          = my_nml % urban_cable
  lakes_cable          = my_nml % lakes_cable
  ice_cable            = my_nml % ice_cable

END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_nml_cable_surface_types
#endif

SUBROUTINE set_derived_variables_cable_surface_types()

! Description:
!   Derive ntype_cable and nnpft_cable from the namelist values

IMPLICIT NONE

ntype_cable = npft_cable + nnvg_cable
nnpft_cable = npft_cable - ncpft_cable

RETURN

END SUBROUTINE set_derived_variables_cable_surface_types

END MODULE cable_surface_types_mod
