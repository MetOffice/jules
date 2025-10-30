!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE overbank_inundation_mod

!------------------------------------------------------------------------------
! Description:
!   Contains river overbank inundation variables and switches
!
!  Code Owner: Please refer to ModuleLeaders.txt
!  This file belongs in section: Hydrology
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!------------------------------------------------------------------------------

USE missing_data_mod, ONLY: imdi, rmdi

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!------------------------------------------------------------------------------
! Constants identifying overbank inundation models.
!------------------------------------------------------------------------------
INTEGER, PARAMETER ::                                                          &
  overbank_simple = 1,                                                         &
    ! Constant indicating that the inundated area is calculated using an
    ! allometric relationship to estimate river width, without use of
    ! topographic data.
  overbank_simple_rosgen = 2,                                                  &
    ! Constant indicating that the inundated area is calculated using
    ! allometric relationships to estimate river width and depth, and the
    ! Rosgen entrenchment ratio, without use of topographic data,
  overbank_hypsometric = 3,                                                    &
    ! Constant indicating that the inundated area is calculated using a
    ! hypsometric integral and an allometric relationship to estimate river
    ! depth.
  overbank_quantiles = 4
    ! Constant indicating that the inundated area is calculated using quantiles
    ! of elevation.

!------------------------------------------------------------------------------
! Declare variables that can be read from the jules_overbank namelist.
!------------------------------------------------------------------------------
! Overbank inundation parameters
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  nquantile_hypso = imdi,                                                      &
    ! Number of quantiles used to describe the elevation profile (the
    ! hypsometric curve).
  overbank_model = imdi
    ! Switch used to select the model of overbank inundation.

REAL(KIND=real_jlslsm) ::                                                      &
   riv_a = rmdi,                                                               &
        ! Parameter in Leopold & Maddock (1953: eqn1)
   riv_b = rmdi,                                                               &
        ! Parameter in Leopold & Maddock (1953: eqn1)
   riv_c = rmdi,                                                               &
        ! Parameter in Leopold & Maddock (1953: eqn2)
   riv_f = rmdi,                                                               &
        ! Parameter in Leopold & Maddock (1953: eqn2)
   coef_b = rmdi,                                                              &
        ! Parameter to calculate the bankflow discharge power-law
        ! relationship from "Flood Modeling, Prediction and Mitigation"
        ! by Sen 2018.
   exp_c = rmdi,                                                               &
        ! Parameter to calculate the bankflow discharge power-law
        ! relationship from "Flood Modeling, Prediction and Mitigation"
        ! by Sen 2018.
   ent_ratio = rmdi
        ! Entrenchment ratio (Rosgen 1994). Ratio of the width of
        ! flood-prone area to surface width of bankfull channel.
        ! The flood-prone area width is measured at the elevation that
        ! corresponds to twice the maximum depth of the bankfull channel.

!------------------------------------------------------------------------------
! Declare variables that are not in the namelist.
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Array variables defined on land points updated in overbank inundation
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
  frac_fplain_lp(:)
           ! Overbank inundation area as a fraction of gridcell area
           ! (on land grid)

!------------------------------------------------------------------------------
! Array variables defined on full 2D rivers grid (as read in from ancillary)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
  !----------------------------------------------------------------------------
  ! Variables for overbank_model == overbank_hypsometric.
  !----------------------------------------------------------------------------
  logn_mean(:,:),                                                              &
           ! ln(mean(elevation - elev_min)) for each gridcell
  logn_stdev(:,:),                                                             &
           ! ln(SD(elevation - elev_min)) for each gridcell
  !----------------------------------------------------------------------------
  ! Variables for overbank_model == overbank_quantiles.
  !----------------------------------------------------------------------------
  hypsometric_quantiles_grid(:,:,:)
    ! Quantiles of floodplain elevation (m).
    ! This is the elevation above that of the top of the river channel.
    ! These define the elevation for equal increments of area.

!------------------------------------------------------------------------------
! Ancillary variables on river points.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
  !----------------------------------------------------------------------------
  ! Variables for overbank_model == overbank_hypsometric.
  !----------------------------------------------------------------------------
  logn_mean_rp(:),                                                             &
           ! ln(mean(elevation - elev_min)) for each gridcell (on rivers grid)
  logn_stdev_rp(:),                                                            &
           ! ln(SD(elevation - elev_min)) for each gridcell (on rivers grid)
  !----------------------------------------------------------------------------
  ! Variables for overbank_model == overbank_quantiles.
  !----------------------------------------------------------------------------
  hypsometric_quantiles(:,:)
    ! Quantiles of floodplain elevation (m).
    ! This is the elevation above that of the top of the river channel.
    ! These define the elevation for equal increments of area.

!------------------------------------------------------------------------------
! Other array variables on river points.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
  qbf(:),                                                                      &
           ! Bankfull discharge rate (m3/s)
  dbf(:),                                                                      &
           ! Channel depth when at bankfull discharge rate (m)
  wbf(:),                                                                      &
           ! Channel width when at bankfull discharge rate (m)
  frac_fplain_rp(:)
           ! Overbank inundation area as a fraction of gridcell area
           ! (on rivers grid)

!------------------------------------------------------------------------------
! Single namelist definition for UM and standalone
!------------------------------------------------------------------------------
NAMELIST  /jules_overbank/                                                     &
  coef_b, exp_c, ent_ratio, nquantile_hypso, overbank_model, riv_a, riv_b,     &
  riv_c, riv_f

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='OVERBANK_INUNDATION_MOD'

CONTAINS

!##############################################################################

SUBROUTINE check_jules_overbank()

USE ereport_mod, ONLY: ereport

USE jules_rivers_mod, ONLY: l_riv_overbank

!------------------------------------------------------------------------------
! Description:
!   Checks JULES_OVERBANK switches for consistency
!
! Code Owner:
!   Please refer to ModuleLeaders.txt
!   This file belongs in section: Hydrology
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!------------------------------------------------------------------------------

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER :: RoutineName='CHECK_JULES_OVERBANK'

INTEGER :: errcode

!End of header
!------------------------------------------------------------------------------

IF ( .NOT. l_riv_overbank ) THEN
  ! For clarity, raise an error if a model is specified but l_riv_overbank=F.
  IF ( overbank_model /= imdi ) THEN
    errcode = 101
    CALL ereport(RoutineName, errcode,                                         &
                 'overbank_model should not be specified if '               // &
                 'l_riv_overbank = .FALSE')
  ELSE
    ! No model specified, all is well, nothing more to do here.
    RETURN
  END IF
END IF

! Check that overbank model is provided.
IF ( overbank_model == imdi ) THEN
  errcode = 101
  CALL ereport(RoutineName, errcode,                                           &
               'No value found for overbank_model.')
END IF

! Check that a valid overbank model is specified.
SELECT CASE ( overbank_model )
CASE ( overbank_simple, overbank_simple_rosgen, overbank_hypsometric,          &
       overbank_quantiles )
  ! Valid options, nothing to do.
CASE DEFAULT
  errcode = 101
  CALL ereport(RoutineName, errcode,                                           &
               'Invalid value for overbank_model')
END SELECT

!------------------------------------------------------------------------------
! Check that the required parameters were found.
! In many cases we just check that a value is given, not that it is reasonable!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Check values that are only required with overbank_model = simple or
! simple_rosgen.
!------------------------------------------------------------------------------
SELECT CASE ( overbank_model )

CASE ( overbank_simple, overbank_simple_rosgen )

  ! Parameters for river width allometry are required.

  IF ( ABS( riv_a - rmdi ) < EPSILON(1.0) ) THEN
    errcode = 101
    CALL ereport(RoutineName, errcode,'No value found for riv_a.')
  END IF

  IF ( ABS( riv_b - rmdi ) < EPSILON(1.0) ) THEN
    errcode = 101
    CALL ereport(RoutineName, errcode,'No value found for riv_b.')
  END IF

  !----------------------------------------------------------------------------
  ! Check values that are only required with overbank_model = rosgen.
  !----------------------------------------------------------------------------
  IF ( overbank_model == overbank_simple_rosgen ) THEN

    ! Parameters for bankfull discharge and entrenchment ratio are required.

    IF ( ABS( coef_b - rmdi ) < EPSILON(1.0) ) THEN
      errcode = 101
      CALL ereport(RoutineName, errcode,'No value found for coef_b.')
    END IF

    IF ( ABS( ent_ratio - rmdi ) < EPSILON(1.0) ) THEN
      errcode = 101
      CALL ereport(RoutineName, errcode,'No value found for ent_ratio.')
    END IF

    IF ( ABS( exp_c - rmdi ) < EPSILON(1.0) ) THEN
      errcode = 101
      CALL ereport(RoutineName, errcode,'No value found for exp_c.')
    END IF

  END IF  !  overbank_simple_rosgen

END SELECT

!------------------------------------------------------------------------------
! Check parameters that are required with overbank_model = simple_rosgen or
! hypso.
!------------------------------------------------------------------------------
SELECT CASE ( overbank_model )
CASE ( overbank_simple_rosgen, overbank_hypsometric )

  ! Parameters for river depth allometry are required.

  IF ( ABS( riv_c - rmdi ) < EPSILON(1.0) ) THEN
    errcode = 101
    CALL ereport(RoutineName, errcode,'No value found for riv_c.')
  END IF

  IF ( ABS( riv_f - rmdi ) < EPSILON(1.0) ) THEN
    errcode = 101
    CALL ereport(RoutineName, errcode,'No value found for riv_f.')
  END IF

END SELECT

!------------------------------------------------------------------------------
! Check parameters that are required with overbank_model = quantiles.
!------------------------------------------------------------------------------
IF ( overbank_model == overbank_quantiles ) THEN
  ! Number of quantiles must be provided and be >0.
  IF ( nquantile_hypso == imdi ) THEN
    errcode = 101
    CALL ereport(RoutineName, errcode,'No value found for nquantile_hypso.')
  ELSE IF ( nquantile_hypso < 1 ) THEN
    errcode = 101
    CALL ereport(RoutineName, errcode,'nquantile_hypso must be > 0.')
  END IF
END IF

END SUBROUTINE check_jules_overbank

!##############################################################################

SUBROUTINE init_rosgen_vars( rivers )

USE jules_rivers_mod, ONLY: np_rivers, rivers_type

IMPLICIT NONE

!------------------------------------------------------------------------------
! Arguments with INTENT(IN)
!------------------------------------------------------------------------------
TYPE(rivers_type), INTENT(IN) :: rivers

!------------------------------------------------------------------------------
! Local parameters.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), PARAMETER :: m2tokm2 = 1.0e-6
  ! Conversion from m2 to km2

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER :: ip

REAL(KIND=real_jlslsm) :: contribarea
  ! Upstream contributing drainage area (km2).

!------------------------------------------------------------------------------
! Define overbank inundation initial variables for bankfull on river points
! if using Rosgen entrenchment option.
! Bankfull discharge (qbf) from power-law relationship from
! "Flood Modeling, Prediction and Mitigation" by Sen 2018.
! Bankfull width and depth (wbf, dbf) from Leopold   & Maddock (1953).
!------------------------------------------------------------------------------
DO ip = 1,np_rivers
  contribarea = (rivers%rivers_boxareas_rp(ip) * m2tokm2) *                    &
                ( 1.0 + MAX(0.0, REAL(rivers%rfm_iarea_rp(ip))) )
  qbf(ip) = coef_b * ( contribarea**exp_c )
  wbf(ip) = riv_a * ( qbf(ip)**riv_b )
  dbf(ip) = riv_c * ( qbf(ip)**riv_f )
END DO

RETURN
END SUBROUTINE init_rosgen_vars

!##############################################################################

SUBROUTINE print_nlist_jules_overbank()

USE jules_print_mgr, ONLY: jules_print

IMPLICIT NONE
CHARACTER(LEN=50000) :: lineBuffer

CHARACTER(LEN=*), PARAMETER :: RoutineName='PRINT_NLIST_JULES_OVERBANK'

CALL jules_print('overbank_inundation_mod',                                    &
   'Contents of namelist jules_overbank')

WRITE(lineBuffer,*)' nquantile_hypso = ',nquantile_hypso
CALL jules_print('jules_overbank',lineBuffer)
WRITE(lineBuffer,*)' overbank_model = ',overbank_model
CALL jules_print('jules_overbank',lineBuffer)
WRITE(lineBuffer,*)' riv_a = ',riv_a
CALL jules_print('jules_overbank',lineBuffer)
WRITE(lineBuffer,*)' riv_b = ',riv_b
CALL jules_print('jules_overbank',lineBuffer)
WRITE(lineBuffer,*)' riv_c = ',riv_c
CALL jules_print('jules_overbank',lineBuffer)
WRITE(lineBuffer,*)' riv_f = ',riv_f
CALL jules_print('jules_overbank',lineBuffer)
WRITE(lineBuffer,*)' coef_b = ',coef_b
CALL jules_print('jules_overbank',lineBuffer)
WRITE(lineBuffer,*)' exp_c = ',exp_c
CALL jules_print('jules_overbank',lineBuffer)
WRITE(lineBuffer,*)' ent_ratio = ',ent_ratio
CALL jules_print('jules_overbank',lineBuffer)

CALL jules_print('overbank_inundation_mod',                                    &
   '- - - - - - end of namelist - - - - - -')

END SUBROUTINE print_nlist_jules_overbank

!##############################################################################

#if !defined(UM_JULES) && !defined(LFRIC)
SUBROUTINE read_nml_jules_overbank(nml_dir)

USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE logging_mod, ONLY: log_info, log_fatal

IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists
! Work variables
INTEGER :: ERROR  ! Error indicator

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_OVERBANK'

!------------------------------------------------------------------------------
! Read river routing namelist
!------------------------------------------------------------------------------
CALL log_info(RoutineName, "Reading JULES_OVERBANK namelist...")

! Open the river routing parameters namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'jules_rivers.nml'),         &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error opening namelist file jules_rivers.nml " //            &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // ")")
END IF

READ(namelist_unit, NML = jules_overbank, IOSTAT = ERROR)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error reading namelist JULES_OVERBANK " //                   &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // ")")
END IF

CLOSE(namelist_unit, IOSTAT = ERROR)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error closing namelist file jules_rivers.nml " //            &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // ")")
END IF

END SUBROUTINE read_nml_jules_overbank
#endif

!##############################################################################

#if defined(UM_JULES) && !defined(LFRIC)
SUBROUTINE read_nml_jules_overbank(unit_in)

USE setup_namelist,   ONLY: setup_nml_type
USE check_iostat_mod, ONLY: check_iostat
USE UM_parcore,       ONLY: mype
USE parkind1,         ONLY: jprb, jpim
USE yomhook,          ONLY: lhook, dr_hook
USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

INTEGER,INTENT(IN) :: unit_in
INTEGER :: my_comm
INTEGER :: mpl_nml_type
INTEGER :: ErrorStatus
INTEGER :: icode
CHARACTER(LEN=errormessagelength) :: iomessage
REAL(KIND=jprb) :: zhook_handle
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_OVERBANK'

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 2
INTEGER, PARAMETER :: n_int = 2
INTEGER, PARAMETER :: n_real = 7

TYPE :: my_namelist
  SEQUENCE
  INTEGER :: nquantile_hypso
  INTEGER :: overbank_model
  REAL(KIND=real_jlslsm) :: riv_a
  REAL(KIND=real_jlslsm) :: riv_b
  REAL(KIND=real_jlslsm) :: riv_c
  REAL(KIND=real_jlslsm) :: riv_f
  REAL(KIND=real_jlslsm) :: coef_b
  REAL(KIND=real_jlslsm) :: exp_c
  REAL(KIND=real_jlslsm) :: ent_ratio
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type,                                 &
                    n_int_in = n_int, n_real_in = n_real)

IF (mype == 0) THEN

  READ(UNIT = unit_in, NML = jules_overbank, IOSTAT = ErrorStatus,             &
       IOMSG = iomessage)
  CALL check_iostat(errorstatus, "namelist JULES_OVERBANK", iomessage)

  my_nml % nquantile_hypso  = nquantile_hypso
  my_nml % overbank_model   = overbank_model
  my_nml % riv_a            = riv_a
  my_nml % riv_b            = riv_b
  my_nml % riv_c            = riv_c
  my_nml % riv_f            = riv_f
  my_nml % coef_b           = coef_b
  my_nml % exp_c            = exp_c
  my_nml % ent_ratio        = ent_ratio

END IF

CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN

  nquantile_hypso = my_nml % nquantile_hypso
  overbank_model  = my_nml % overbank_model
  riv_a = my_nml % riv_a
  riv_b = my_nml % riv_b
  riv_c = my_nml % riv_c
  riv_f = my_nml % riv_f
  coef_b = my_nml % coef_b
  exp_c = my_nml % exp_c
  ent_ratio = my_nml % ent_ratio

END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

END SUBROUTINE read_nml_jules_overbank
#endif

END MODULE overbank_inundation_mod

