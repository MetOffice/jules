! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE jules_surface_types_mod

!-----------------------------------------------------------------------------
! Description:
!   Contains JULES surface type information and a namelist for setting them
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE max_dimensions,   ONLY:                                                    &
    elev_tile_max,                                                             &
    ntype_max

USE missing_data_mod, ONLY: imdi


IMPLICIT NONE

!-----------------------------------------------------------------------------
! Module variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  nnpft,                                                                       &
                ! Number of natural pfts
                !   Derived from namelist inputs as npft - ncpft
  ncpft = 0,                                                                   &
                ! Number of crop pfts
  npft  = imdi,                                                                &
                ! Number of plant functional types
  nnvg  = imdi,                                                                &
                ! Number of non-vegetation surface types
  ntype,                                                                       &
                ! Total number of surface types
  urban = imdi,                                                                &
                ! Index of the urban surface type
  lake  = imdi,                                                                &
                ! Index of the lake surface type
  soil  = imdi,                                                                &
                ! Index of the soil surface type
  ice   = imdi,                                                                &
                ! Index of the ice surface type

! For use when l_urban2t == .TRUE.
    urban_canyon = imdi,                                                       &
                  ! Index of the urban_canyon surface type
    urban_roof   = imdi,                                                       &
                  ! Index of the urban_roof surface type

! For use when l_elev_land_ice == .TRUE.
    elev_ice(elev_tile_max) = imdi,                                            &
                  ! Index of the elevated ice surface types
    elev_rock(elev_tile_max) = imdi,                                           &
                  ! Index of the elevated rock surface types

    brd_leaf         = imdi,                                                   &
                  ! Index of surface type 'broad-leaf tree'
    brd_leaf_dec     = imdi,                                                   &
                  ! Index of surface type 'broad-leaf dec tree'
    brd_leaf_eg_trop = imdi,                                                   &
                  ! Index of surface type 'broad-leaf eg trop tree'
    brd_leaf_eg_temp = imdi,                                                   &
                  ! Index of surface type 'broad-leaf eg temp tree'
    ndl_leaf         = imdi,                                                   &
                  ! Index of surface type 'needle-leaf tree'
    ndl_leaf_dec     = imdi,                                                   &
                  ! Index of surface type 'needle-leaf dec tree'
    ndl_leaf_eg      = imdi,                                                   &
                  ! Index of surface type 'needle-leaf eg tree'
    c3_grass         = imdi,                                                   &
                  ! Index of surface type 'C3 grass'
    c3_crop          = imdi,                                                   &
                  ! Index of surface type 'C3 crop'
    c3_pasture       = imdi,                                                   &
                  ! Index of surface type 'C3 pasture'
    c4_grass         = imdi,                                                   &
                  ! Index of surface type 'C4 grass'
    c4_crop          = imdi,                                                   &
                  ! Index of surface type 'C4 crop'
    c4_pasture       = imdi,                                                   &
                  ! Index of surface type 'C4 pasture'
    shrub            = imdi,                                                   &
                  ! Index of surface type 'shrub'
    shrub_dec        = imdi,                                                   &
                  ! Index of surface type 'shrub dec'
    shrub_eg         = imdi,                                                   &
                  ! Index of surface type 'shrub eg'

! Allows new tiles to be added to the UM without a code change.
    usr_type(ntype_max)  = imdi,                                               &
                  ! Index of user defined surface types
    tile_map_ids(ntype_max)    = imdi,                                         &
                  ! User specified tile map
    tile_map_pslevs(ntype_max) = imdi

!-----------------------------------------------------------------------------
! Single namelist definition for UM and standalone
!-----------------------------------------------------------------------------
NAMELIST  / jules_surface_types/                                               &
  npft, ncpft, nnvg, brd_leaf, brd_leaf_dec, brd_leaf_eg_trop,                 &
  brd_leaf_eg_temp, ndl_leaf, ndl_leaf_dec, ndl_leaf_eg, c3_grass,             &
  c3_crop, c3_pasture, c4_grass, c4_crop, c4_pasture, shrub, shrub_dec,        &
  shrub_eg, urban, urban_canyon, urban_roof, lake, soil, ice,                  &
  elev_ice, elev_rock, usr_type, tile_map_ids

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_SURFACE_TYPES_MOD'

CONTAINS

SUBROUTINE check_jules_surface_types()

USE max_dimensions, ONLY: npft_max, ncpft_max, nnvg_max

USE ereport_mod, ONLY: ereport
USE jules_print_mgr, ONLY: jules_print, jules_message

!-----------------------------------------------------------------------------
! Description:
!   Checks JULES_SURFACE_TYPES namelist for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

INTEGER :: i, nchecks ! Loop counter

INTEGER :: errorstatus

CHARACTER(LEN=*), PARAMETER ::                                                 &
   RoutineName = 'check_jules_surface_types'
!-----------------------------------------------------------------------------
! Check that the given values are less than the fixed values for IO
!-----------------------------------------------------------------------------
IF ( npft > npft_max ) THEN
  errorstatus = 101
  CALL ereport(RoutineName, errorstatus,                                       &
               "npft > npft_max - increase npft_max and recompile")
END IF
IF ( ncpft > ncpft_max ) THEN
  errorstatus = 101
  CALL ereport(RoutineName, errorstatus,                                       &
               "ncpft > ncpft_max - increase ncpft_max and recompile")
END IF
IF ( nnvg > nnvg_max ) THEN
  errorstatus = 101
  CALL ereport(RoutineName, errorstatus,                                       &
               "nnvg > nnvg_max - increase nnvg_max and recompile")
END IF
IF ( ncpft > npft ) THEN
  errorstatus = 101
  CALL ereport(RoutineName, errorstatus,                                       &
               "ncpft > npft - total number of PFTs must be >= " //            &
               "number of crop PFTs")
END IF
! If these are true, then we automatically have:
!   ntype <= ntype_max (since ntype(_max) = npft(_max) + nnvg(_max))
!   nnpft <= nnpft_max (since nnpft(_max) = npft(_max) - ncpft(_max))

!-----------------------------------------------------------------------------
! Check values for the specific surface types are sensible
!-----------------------------------------------------------------------------
! PFTs
errorstatus = 0
nchecks = 0

CALL check_surface_type_value ( brd_leaf, "brd_leaf", 1, npft,                 &
   RoutineName, errorstatus, nchecks )
CALL check_surface_type_value ( brd_leaf_dec, "brd_leaf_dec", 1, npft,         &
   RoutineName, errorstatus, nchecks )
CALL check_surface_type_value ( brd_leaf_eg_trop, "brd_leaf_eg_trop", 1, npft, &
   RoutineName, errorstatus, nchecks )
CALL check_surface_type_value ( brd_leaf_eg_temp, "brd_leaf_eg_temp", 1, npft, &
   RoutineName, errorstatus, nchecks )
CALL check_surface_type_value ( ndl_leaf, "ndl_leaf", 1, npft,                 &
   RoutineName, errorstatus, nchecks )
CALL check_surface_type_value ( ndl_leaf_dec, "ndl_leaf_dec", 1, npft,         &
   RoutineName, errorstatus, nchecks )
CALL check_surface_type_value ( ndl_leaf_eg, "ndl_leaf_eg", 1, npft,           &
   RoutineName, errorstatus, nchecks )
CALL check_surface_type_value ( c3_grass, "c3_grass", 1, npft,                 &
   RoutineName, errorstatus, nchecks )
CALL check_surface_type_value ( c3_crop, "c3_crop", 1, npft,                   &
   RoutineName, errorstatus, nchecks )
CALL check_surface_type_value ( c3_pasture, "c3_pasture", 1, npft,             &
   RoutineName, errorstatus, nchecks )
CALL check_surface_type_value ( c4_grass, "c4_grass", 1, npft,                 &
   RoutineName, errorstatus, nchecks )
CALL check_surface_type_value ( c4_crop, "c4_crop", 1, npft,                   &
   RoutineName, errorstatus, nchecks )
CALL check_surface_type_value ( c4_pasture, "c4_pasture", 1, npft,             &
   RoutineName, errorstatus, nchecks )
CALL check_surface_type_value ( shrub, "shrub", 1, npft,                       &
   RoutineName, errorstatus, nchecks )
CALL check_surface_type_value ( shrub_dec, "shrub_dec", 1, npft,               &
   RoutineName, errorstatus, nchecks )
CALL check_surface_type_value ( shrub_eg, "shrub_eg", 1, npft,                 &
   RoutineName, errorstatus, nchecks )

! PFT surface types must come before non-veg types, so if urban, lake, soil,
! ice, urban_canyon or urban_roof are given (i.e. > 0) then they must be > npft
! A soil type is required
CALL check_surface_type_value ( urban, "urban", npft+1, ntype,                 &
   RoutineName, errorstatus, nchecks )
CALL check_surface_type_value ( urban_canyon, "urban_canyon", npft+1, ntype,   &
   RoutineName, errorstatus, nchecks )
CALL check_surface_type_value ( urban_roof, "urban_roof", npft+1, ntype,       &
   RoutineName, errorstatus, nchecks )
CALL check_surface_type_value ( lake, "lake", npft+1, ntype,                   &
   RoutineName, errorstatus, nchecks )
CALL check_surface_type_value ( soil, "soil", npft+1, ntype,                   &
   RoutineName, errorstatus, nchecks )
CALL check_surface_type_value ( ice, "ice", npft+1, ntype,                     &
   RoutineName, errorstatus, nchecks )

DO i = 1, elev_tile_max
  CALL check_surface_type_value ( elev_ice(i), "elev_ice(:)", npft+1, ntype,   &
     RoutineName, errorstatus, nchecks )
  CALL check_surface_type_value ( elev_rock(i), "elev_rock(:)", npft+1, ntype, &
     RoutineName, errorstatus, nchecks )
END DO

! usr_type can be either PFT or non-veg
DO i = 1, ntype
  CALL check_surface_type_value ( usr_type(i), "usr_type(:)", 1, ntype,        &
     RoutineName, errorstatus, nchecks )
END DO

! Check that all present surface types have been checked for range compliance
! This check should also ensure that a check is added for each new surface type
IF ( nchecks /= ntype ) THEN
  errorstatus = 101
  CALL jules_print(RoutineName,                                                &
     "At least one surface type in namelist does not have a range check.")
  WRITE(jules_message,'(A,I0,A,I0)')                                           &
     "These should be the same; ntype = ", ntype, ", nchecks = ", nchecks
  CALL jules_print(RoutineName, jules_message)
END IF

! Now that all surface types have been checked issue abort if required
IF ( errorstatus > 0 ) THEN
  CALL ereport(RoutineName, errorstatus,                                       &
     "Error(s) found. Please see job.out for information ")
END IF

END SUBROUTINE check_jules_surface_types

SUBROUTINE check_surface_type_value ( surface_type, surface_type_name,         &
   min_value, max_value, RoutineName, errorstatus, nchecks )

USE jules_print_mgr, ONLY: jules_print

IMPLICIT NONE

INTEGER          :: surface_type, min_value, max_value, errorstatus, nchecks
CHARACTER(LEN=*) :: surface_type_name, RoutineName

IF ( surface_type > 0 ) THEN
  nchecks = nchecks + 1
  IF ( surface_type < min_value .OR. surface_type > max_value ) THEN
    errorstatus = 101
    CALL jules_print(RoutineName,                                              &
       TRIM(surface_type_name) // " tile is given but is out of range")
  END IF
END IF

END SUBROUTINE  check_surface_type_value

SUBROUTINE print_nlist_jules_surface_types()

USE jules_print_mgr, ONLY: jules_print

IMPLICIT NONE

INTEGER :: i, n ! Loop counter

CHARACTER(LEN=50000) :: lineBuffer


!-----------------------------------------------------------------------------

CALL jules_print('jules_surface_types',                                        &
                 'Contents of namelist jules_surface_types')

WRITE(lineBuffer, *) '  npft = ', npft
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  nnvg = ', nnvg
CALL jules_print('jules_surface_types', lineBuffer)

IF ( brd_leaf > 0 ) THEN
  WRITE(lineBuffer, *) '  brd_leaf = ', brd_leaf
  CALL jules_print('jules_surface_types', lineBuffer)
END IF

IF ( brd_leaf_dec > 0 ) THEN
  WRITE(lineBuffer, *) '  brd_leaf_dec = ', brd_leaf_dec
  CALL jules_print('jules_surface_types', lineBuffer)
END IF

IF ( brd_leaf_eg_trop > 0 ) THEN
  WRITE(lineBuffer, *) '  brd_leaf_eg_trop = ', brd_leaf_eg_trop
  CALL jules_print('jules_surface_types', lineBuffer)
END IF

IF ( brd_leaf_eg_temp > 0 ) THEN
  WRITE(lineBuffer, *) '  brd_leaf_eg_temp = ', brd_leaf_eg_temp
  CALL jules_print('jules_surface_types', lineBuffer)
END IF

IF ( ndl_leaf > 0 ) THEN
  WRITE(lineBuffer, *) '  ndl_leaf = ', ndl_leaf
  CALL jules_print('jules_surface_types', lineBuffer)
END IF

IF ( ndl_leaf_dec > 0 ) THEN
  WRITE(lineBuffer, *) '  ndl_leaf_dec = ', ndl_leaf_dec
  CALL jules_print('jules_surface_types', lineBuffer)
END IF

IF ( ndl_leaf_eg > 0 ) THEN
  WRITE(lineBuffer, *) '  ndl_leaf_eg = ', ndl_leaf_eg
  CALL jules_print('jules_surface_types', lineBuffer)
END IF

IF ( c3_grass > 0 ) THEN
  WRITE(lineBuffer, *) '  c3_grass = ', c3_grass
  CALL jules_print('jules_surface_types', lineBuffer)
END IF

IF ( c3_crop > 0 ) THEN
  WRITE(lineBuffer, *) '  c3_crop = ', c3_crop
  CALL jules_print('jules_surface_types', lineBuffer)
END IF

IF ( c3_pasture > 0 ) THEN
  WRITE(lineBuffer, *) '  c3_pasture = ', c3_pasture
  CALL jules_print('jules_surface_types', lineBuffer)
END IF

IF ( c4_grass > 0 ) THEN
  WRITE(lineBuffer, *) '  c4_grass = ', c4_grass
  CALL jules_print('jules_surface_types', lineBuffer)
END IF

IF ( c4_crop > 0 ) THEN
  WRITE(lineBuffer, *) '  c4_crop = ', c4_crop
  CALL jules_print('jules_surface_types', lineBuffer)
END IF

IF ( c4_pasture > 0 ) THEN
  WRITE(lineBuffer, *) '  c4_pasture = ', c4_pasture
  CALL jules_print('jules_surface_types', lineBuffer)
END IF

IF ( shrub > 0 ) THEN
  WRITE(lineBuffer, *) '  shrub = ', shrub
  CALL jules_print('jules_surface_types', lineBuffer)
END IF

IF ( shrub_dec > 0 ) THEN
  WRITE(lineBuffer, *) '  shrub_dec = ', shrub_dec
  CALL jules_print('jules_surface_types', lineBuffer)
END IF

IF ( shrub_eg > 0 ) THEN
  WRITE(lineBuffer, *) '  shrub_eg = ', shrub_eg
  CALL jules_print('jules_surface_types', lineBuffer)
END IF

IF ( urban > 0 ) THEN
  WRITE(lineBuffer, *) '  urban = ', urban
  CALL jules_print('jules_surface_types', lineBuffer)
END IF

IF ( lake > 0 ) THEN
  WRITE(lineBuffer, *) '  lake = ', lake
  CALL jules_print('jules_surface_types', lineBuffer)
END IF

IF ( soil > 0 ) THEN
  WRITE(lineBuffer, *) '  soil = ', soil
  CALL jules_print('jules_surface_types', lineBuffer)
END IF

IF ( ice > 0 ) THEN
  WRITE(lineBuffer, *) '  ice = ', ice
  CALL jules_print('jules_surface_types', lineBuffer)
END IF

IF ( urban_canyon > 0 ) THEN
  WRITE(lineBuffer, *) '  urban_canyon = ', urban_canyon
  CALL jules_print('jules_surface_types', lineBuffer)
END IF

IF ( urban_roof > 0 ) THEN
  WRITE(lineBuffer, *) '  urban_roof = ', urban_roof
  CALL jules_print('jules_surface_types', lineBuffer)
END IF

n = COUNT( elev_ice(1:nnvg) > 0 )
IF ( n > 0 ) THEN
  DO i = 1, n
    WRITE(lineBuffer, *) '  elev_ice(', i, ') = ', elev_ice(i)
    CALL jules_print('jules_surface_types', lineBuffer)
  END DO
END IF

n = COUNT( elev_rock(1:nnvg) > 0 )
IF ( n > 0 ) THEN
  DO i = 1, n
    WRITE(lineBuffer, *) '  elev_rock(', i, ') = ', elev_rock(i)
    CALL jules_print('jules_surface_types', lineBuffer)
  END DO
END IF

n = COUNT( usr_type(1:ntype) > 0 )
IF ( n > 0 ) THEN
  WRITE(lineBuffer, *) '  usr_type = ', usr_type(1:n)
  CALL jules_print('jules_surface_types', lineBuffer)
END IF

IF ( ANY( tile_map_ids(1:ntype) > 0 ) ) THEN
  WRITE(lineBuffer, *) '  tile_map_ids = ', tile_map_ids(1:ntype)
  CALL jules_print('jules_surface_types', lineBuffer)
END IF

CALL jules_print('jules_surface_types',                                        &
    '- - - - - - end of namelist - - - - - -')

END SUBROUTINE print_nlist_jules_surface_types

#if defined(UM_JULES) && !defined(LFRIC)
SUBROUTINE read_nml_jules_surface_types (unitnumber)

! Description:
!  Read the JULES_SURFACE_TYPES namelist

USE setup_namelist,   ONLY: setup_nml_type
USE check_iostat_mod, ONLY: check_iostat
USE UM_parcore,       ONLY: mype
USE parkind1,         ONLY: jprb, jpim
USE yomhook,          ONLY: lhook, dr_hook
USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

INTEGER, INTENT(IN) :: unitnumber
INTEGER :: my_comm
INTEGER :: mpl_nml_type
INTEGER :: ErrorStatus
INTEGER :: icode
CHARACTER(LEN=errormessagelength) :: iomessage
REAL(KIND=jprb) :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_SURFACE_TYPES'
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 1
INTEGER, PARAMETER :: n_int = 25 + (2 * ntype_max) + (2 * elev_tile_max)

TYPE :: my_namelist
  SEQUENCE
  INTEGER :: npft
  INTEGER :: ncpft
  INTEGER :: nnvg
  INTEGER :: brd_leaf
  INTEGER :: brd_leaf_dec
  INTEGER :: brd_leaf_eg_trop
  INTEGER :: brd_leaf_eg_temp
  INTEGER :: ndl_leaf
  INTEGER :: ndl_leaf_dec
  INTEGER :: ndl_leaf_eg
  INTEGER :: c3_grass
  INTEGER :: c3_crop
  INTEGER :: c3_pasture
  INTEGER :: c4_grass
  INTEGER :: c4_crop
  INTEGER :: c4_pasture
  INTEGER :: shrub
  INTEGER :: shrub_dec
  INTEGER :: shrub_eg
  INTEGER :: urban
  INTEGER :: lake
  INTEGER :: soil
  INTEGER :: ice
  INTEGER :: urban_canyon
  INTEGER :: urban_roof
  INTEGER :: elev_ice(elev_tile_max)
  INTEGER :: elev_rock(elev_tile_max)
  INTEGER :: usr_type(ntype_max)
  INTEGER :: tile_map_ids(ntype_max)
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type, n_int_in = n_int)

IF (mype == 0) THEN

  READ (UNIT = unitnumber, NML = jules_surface_types, IOSTAT = errorstatus,    &
        IOMSG = iomessage)
  CALL check_iostat(errorstatus, "namelist jules_surface_types",iomessage)

  my_nml % npft             = npft
  my_nml % ncpft            = ncpft
  my_nml % nnvg             = nnvg
  my_nml % brd_leaf         = brd_leaf
  my_nml % brd_leaf_dec     = brd_leaf_dec
  my_nml % brd_leaf_eg_trop = brd_leaf_eg_trop
  my_nml % brd_leaf_eg_temp = brd_leaf_eg_temp
  my_nml % ndl_leaf         = ndl_leaf
  my_nml % ndl_leaf_dec     = ndl_leaf_dec
  my_nml % ndl_leaf_eg      = ndl_leaf_eg
  my_nml % c3_grass         = c3_grass
  my_nml % c3_crop          = c3_crop
  my_nml % c3_pasture       = c3_pasture
  my_nml % c4_grass         = c4_grass
  my_nml % c4_crop          = c4_crop
  my_nml % c4_pasture       = c4_pasture
  my_nml % shrub            = shrub
  my_nml % shrub_dec        = shrub_dec
  my_nml % shrub_eg         = shrub_eg
  my_nml % urban            = urban
  my_nml % lake             = lake
  my_nml % soil             = soil
  my_nml % ice              = ice
  my_nml % urban_canyon     = urban_canyon
  my_nml % urban_roof       = urban_roof
  my_nml % elev_ice(:)      = elev_ice(:)
  my_nml % elev_rock(:)     = elev_rock(:)
  my_nml % usr_type(:)      = usr_type(:)
  my_nml % tile_map_ids(:)  = tile_map_ids(:)
END IF

CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN

  npft             = my_nml % npft
  ncpft            = my_nml % ncpft
  nnvg             = my_nml % nnvg
  brd_leaf         = my_nml % brd_leaf
  brd_leaf_dec     = my_nml % brd_leaf_dec
  brd_leaf_eg_trop = my_nml % brd_leaf_eg_trop
  brd_leaf_eg_temp = my_nml % brd_leaf_eg_temp
  ndl_leaf         = my_nml % ndl_leaf
  ndl_leaf_dec     = my_nml % ndl_leaf_dec
  ndl_leaf_eg      = my_nml % ndl_leaf_eg
  c3_grass         = my_nml % c3_grass
  c3_crop          = my_nml % c3_crop
  c3_pasture       = my_nml % c3_pasture
  c4_grass         = my_nml % c4_grass
  c4_crop          = my_nml % c4_crop
  c4_pasture       = my_nml % c4_pasture
  shrub            = my_nml % shrub
  shrub_dec        = my_nml % shrub_dec
  shrub_eg         = my_nml % shrub_eg
  urban            = my_nml % urban
  lake             = my_nml % lake
  soil             = my_nml % soil
  ice              = my_nml % ice
  urban_canyon     = my_nml % urban_canyon
  urban_roof       = my_nml % urban_roof
  elev_ice(:)      = my_nml % elev_ice(:)
  elev_rock(:)     = my_nml % elev_rock(:)
  usr_type(:)      = my_nml % usr_type(:)
  tile_map_ids(:)  = my_nml % tile_map_ids(:)

END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_nml_jules_surface_types
#endif

SUBROUTINE set_derived_variables_jules_surface_types()

USE jules_print_mgr, ONLY: jules_print, jules_message

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Derive ntype and nnpft from the namelist values
!-----------------------------------------------------------------------------
ntype = npft + nnvg
nnpft = npft - ncpft

CALL jules_print('set_derived_variables_jules_surface_types',                  &
                 'Derived variables from jules_surface_types')

WRITE(jules_message, *) '  ntype = ', ntype
CALL jules_print('set_derived_variables_jules_surface_types', jules_message)

WRITE(jules_message, *) '  nnpft = ', nnpft
CALL jules_print('set_derived_variables_jules_surface_types', jules_message)

CALL jules_print('set_derived_variables_jules_surface_types',                  &
    '- - - - - - end of derived variables - - - - - -')

RETURN

END SUBROUTINE set_derived_variables_jules_surface_types

#if !defined(UM_JULES)
SUBROUTINE read_nml_jules_surface_types(nml_dir)

! Description:
!  Read the JULES_SURFACE_TYPES namelist (standalone)

USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE logging_mod, ONLY: log_info, log_fatal

USE errormessagelength_mod, ONLY: errormessagelength


IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the

INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_SURFACE_TYPES'

!-----------------------------------------------------------------------------
! First, read the surface types namelist
!-----------------------------------------------------------------------------
CALL log_info(RoutineName, "Reading JULES_SURFACE_TYPES namelist...")

OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'jules_surface_types.nml'),  &
     STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR,           &
     IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
                 "Error opening namelist file jules_surface_types.nml " //     &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

READ(namelist_unit, NML = jules_surface_types, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
                 "Error reading namelist JULES_SURFACE_TYPES " //              &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
                 "Error closing namelist file jules_surface_types.nml " //     &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

END SUBROUTINE read_nml_jules_surface_types
#endif

END MODULE jules_surface_types_mod
