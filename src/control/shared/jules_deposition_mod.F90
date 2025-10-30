!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
! ******************************** COPYRIGHT *********************************

MODULE jules_deposition_mod

!-----------------------------------------------------------------------------
! Description:
!   Contains dry deposition options and a namelist for setting them, and
!   variables for the schemes.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGENIC FLUXES.
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE max_dimensions,          ONLY: ndep_species_max

USE missing_data_mod,        ONLY: imdi, rmdi

USE ereport_mod,             ONLY: ereport

USE jules_print_mgr,         ONLY: jules_message, jules_print

USE um_types,                ONLY: real_jlslsm

IMPLICIT NONE

! Public scope by default.

!-----------------------------------------------------------------------------
! Module constants
!-----------------------------------------------------------------------------

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_DEPOSITION_MOD'

! Parameters identifying alternative dry deposition schemes.
! These should be unique.
INTEGER, PARAMETER ::                                                          &
  dry_dep_model_ukca  = 1,                                                     &
    ! Deposition is calculated in JULES using UKCA routines.
  dry_dep_model_jules = 2
    ! Deposition is calculated in JULES using code modelled on UKCA and
    ! with restrictions on pft configuations and ordering removed.

! Parameters identifying alternative H2 soil uptake schemes.
! These should be unique.
INTEGER, PARAMETER ::                                                          &
  dep_h2_scheme_conrad = 1,                                                    &
    ! Uses the scheme based on Conrad & Seiler (used in the UKCA).
  dep_h2_scheme_paulot = 2
    ! Uses the scheme based on Paulot et al., (2021).

!-----------------------------------------------------------------------------
! Module variables - these are included in the namelist.
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Variables that can apply to more than one deposition model.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  dry_dep_model = imdi,                                                        &
    ! Indicates the chosen model for dry deposition.
    ! Valid values are given by the dry_dep_* parameters.
  ndry_dep_species = imdi,                                                     &
    ! Number of species for which dry deposition is calculated.
  dep_h2_soil_scheme = imdi
    ! Indicates the chosen scheme for H2 deposition
    ! = 1: Conrad & Seiler parametererisation
    ! = 2: Paulot et al. (2021)

REAL(KIND=real_jlslsm) :: dzl_const = rmdi
  ! Constant value for separation of boundary layer levels (m).
  ! All layer thicknesses are set to this value. This is used as a simple way
  ! to prescribe the layer thicknesses in standalone mode.
  ! When/if this module is added to the UM, this variable should only be
  ! present in standalone JULES.

LOGICAL ::                                                                     &
  l_deposition = .FALSE. ,                                                     &
    ! Switch for calculation of atmospheric dry deposition within JULES.
  l_deposition_flux = .FALSE. ,                                                &
    ! Switch for calculation of deposition fluxes.
    ! Only used if deposition is requested.
    ! T means the deposition flux is calculated.
    ! F means only the deposition velocity is calculated.
  l_deposition_from_ukca = .FALSE. ,                                           &
    ! Switch to call JULES deposition routines from ukca_chemistry_ctl
  l_deposition_gc_corr = .FALSE.
    ! Switch to use stomatal conductance corrected for bare soil evaporation
    ! Only used if deposition is requested.
    ! T means stomatal conductance corrected for bare soil evaporation
    ! F means uses current incorrect treatment of stomatal conductance

!-----------------------------------------------------------------------------
! Variables for the UKCA scheme.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  tundra_s_limit = rmdi
    ! Latitude of southern limit of tundra (degrees).

REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  dep_rnull = 1.0e+30,                                                         &
    ! Infinite resistance to deposition (1/dep_rnull~0)
  dep_rzero =  0.0,                                                            &
    ! Zero resistance to deposition
  dep_rten  = 10.0,                                                            &
    ! Resistance of 10 s m-1
  glmin     =  1.0e-6
    ! Minimum leaf conductance (m s-1)

LOGICAL ::                                                                     &
  l_ukca_ddep_lev1  = .FALSE. ,                                                &
    ! Switch controlling which atmospheric levels are used for dry deposition.
    ! T means only the lowest layer is used.
    ! F means all layers in the boundary layer are used.
    ! **** NOTE ****
    ! In future (when the code is added) if this is FALSE we will be looping
    ! through boundary layer levels (held in dzl) until we reach the top of
    ! the boundary layer (at height zh). At present I am not 100% sure that
    ! the UM always ensures that zh<=SUM(dzl), though I think it does. This
    ! should be checked with UM code owners and if necessary the standalone
    ! code should check that zh<=SUM(dzl) on every timestep. Until then do
    ! not assume that the boundary layer top will be found!
    ! **** END OF NOTE ****

  l_ukca_dry_dep_so2wet = .FALSE. ,                                            &
    ! True if considering the impact of surface wetness on dry deposition
  l_ukca_ddepo3_ocean =.FALSE. ,                                               &
    ! when true using oceanic O3 dry-deposition scheme
    ! of Luhar et al. (2018)
  l_ukca_emsdrvn_ch4 =.FALSE.
    ! when true using emission-driven CH4 scheme

!-----------------------------------------------------------------------------
! Namelist definitions.
!-----------------------------------------------------------------------------
NAMELIST  / jules_deposition /                                                 &
  dep_h2_soil_scheme, dry_dep_model, dzl_const,                                &
  l_deposition, l_deposition_flux,                                             &
  l_deposition_from_ukca, l_deposition_gc_corr, l_ukca_ddep_lev1,              &
  l_ukca_ddepo3_ocean, l_ukca_dry_dep_so2wet, l_ukca_emsdrvn_ch4,              &
  ndry_dep_species, tundra_s_limit

CONTAINS

!#############################################################################
SUBROUTINE check_jules_deposition()

!-----------------------------------------------------------------------------
! Description:
!   Checks values from the JULES_DEPOSITION namelist.
!-----------------------------------------------------------------------------

IMPLICIT NONE

INTEGER :: errorstatus

CHARACTER(LEN=*), PARAMETER ::                                                 &
  RoutineName = 'CHECK_JULES_DEPOSITION'   ! Name of this procedure.

!-----------------------------------------------------------------------------
!end of header

! Checks on l_deposition = FALSE
IF ( .NOT. l_deposition ) THEN
  ! l_deposition_flux cannot be TRUE
  IF ( l_deposition_flux ) THEN
    errorstatus = 100 !  a hard error
    WRITE(jules_message,'(A,L1)') "Deposition switch l_deposition_flux is " // &
       "not available if l_deposition = FALSE. l_deposition_flux = ",          &
       l_deposition_flux
    CALL ereport( TRIM(routineName), errorstatus, jules_message)
  END IF

  ! Nothing more to do here.
  RETURN
END IF

!-----------------------------------------------------------------------------
! Check that a valid dry deposition model is selected.
!-----------------------------------------------------------------------------
SELECT CASE ( dry_dep_model )
CASE ( dry_dep_model_ukca, dry_dep_model_jules )
  ! Acceptable values.
CASE DEFAULT
  errorstatus = 100 !  a hard error
  WRITE(jules_message,'(A,I2)') "Valid values for dry_dep_model: 1-2. Used ",  &
        dry_dep_model
  CALL ereport( TRIM(routineName), errorstatus, jules_message)
END SELECT

!-----------------------------------------------------------------------------
! Check that a valid H2 soil uptake scheme is selected.
!-----------------------------------------------------------------------------
SELECT CASE ( dep_h2_soil_scheme )
CASE ( dep_h2_scheme_conrad, dep_h2_scheme_paulot )
  ! Acceptable values.
CASE DEFAULT
  errorstatus = 100 !  a hard error
  WRITE(jules_message,'(A,I2)') "Valid values for dep_h2_soil_scheme: 1-2." // &
        "Used ", dep_h2_soil_scheme
  CALL ereport( TRIM(routineName), errorstatus, jules_message)
END SELECT

!-----------------------------------------------------------------------------
! Ensure number of species is reasonable.
!-----------------------------------------------------------------------------
IF ( ndry_dep_species < 1 .OR. ndry_dep_species > ndep_species_max ) THEN
  errorstatus = 100 !  a hard error
  WRITE(jules_message,'(A,I3,A,I3)')                                           &
        "Number of species must be in range 1 to ", ndep_species_max,          &
        ". Used ", ndry_dep_species
  CALL ereport( TRIM(routineName), errorstatus, jules_message)
END IF

!-----------------------------------------------------------------------------
! Ensure layer thickness is reasonable.
!-----------------------------------------------------------------------------
IF ( dzl_const <= 0.0 ) THEN
  errorstatus = 100  !  a hard error
  CALL ereport( TRIM(routineName), errorstatus,                                &
                "dzl_const must be greater than zero." )
END IF

!-----------------------------------------------------------------------------
! Ensure tundra_s_limit was provided - any non-missing value is allowed.
!-----------------------------------------------------------------------------
IF ( tundra_s_limit < -1.0 .OR. tundra_s_limit > 1.0 ) THEN
  errorstatus = 100  !  a hard error
  CALL ereport( TRIM(routineName), errorstatus,                                &
                "tundra_s_limit (as sine of latitude) must lie between -1 " // &
                "and +1." )
END IF

END SUBROUTINE check_jules_deposition

!-----------------------------------------------------------------------------

SUBROUTINE print_nlist_jules_deposition()

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER ::                                                 &
  RoutineName = 'PRINT_NLIST_JULES_DEPOSITION'   ! Name of this procedure.

CHARACTER(LEN=50000) :: lineBuffer

CALL jules_print(RoutineName,                                                  &
                 'Contents of namelist jules_deposition')

WRITE(lineBuffer,"(A,L1)") '  l_deposition          = ', l_deposition
CALL jules_print(RoutineName, lineBuffer)

WRITE(lineBuffer,"(A,L1)") '  l_deposition_flux     = ', l_deposition_flux
CALL jules_print(RoutineName, lineBuffer)

WRITE(lineBuffer,"(A,I0)") '  dry_dep_model         = ', dry_dep_model
CALL jules_print(RoutineName, lineBuffer)

WRITE(lineBuffer,"(A,L1)") '  l_deposition_gc_corr  = ', l_deposition_gc_corr
CALL jules_print(RoutineName, lineBuffer)

WRITE(lineBuffer,"(A,L1)") '  l_deposition_from_ukca= ', l_deposition_from_ukca
CALL jules_print(RoutineName, lineBuffer)

WRITE(lineBuffer,"(A,L1)") '  l_ukca_ddep_lev1      = ', l_ukca_ddep_lev1
CALL jules_print(RoutineName, lineBuffer)

WRITE(lineBuffer,"(A,L1)") '  l_ukca_dry_dep_so2wet = ', l_ukca_dry_dep_so2wet
CALL jules_print(RoutineName, lineBuffer)

WRITE(lineBuffer,"(A,L1)") '  l_ukca_ddepo3_ocean   = ', l_ukca_ddepo3_ocean
CALL jules_print(RoutineName, lineBuffer)

WRITE(lineBuffer,"(A,L1)") '  l_ukca_emsdrvn_ch4    = ', l_ukca_emsdrvn_ch4
CALL jules_print(RoutineName, lineBuffer)

WRITE(lineBuffer,"(A,I0)") '  dep_h2_soil_scheme    = ', dep_h2_soil_scheme
CALL jules_print(RoutineName, lineBuffer)

WRITE(lineBuffer,"(A,I0)") '  ndry_dep_species      = ', ndry_dep_species
CALL jules_print(RoutineName, lineBuffer)

WRITE(lineBuffer,"(A,G11.4E2)") '  tundra_s_limit        = ', tundra_s_limit
CALL jules_print(RoutineName, lineBuffer)

CALL jules_print(RoutineName, '- - - - - - end of namelist - - - - - -')

END SUBROUTINE print_nlist_jules_deposition

!-----------------------------------------------------------------------------

#if defined(UM_JULES) && !defined(LFRIC)
SUBROUTINE read_nml_jules_deposition (unitnumber)

! Description:
!  Read the JULES_DEPOSITION namelist

USE setup_namelist,   ONLY: setup_nml_type
USE check_iostat_mod, ONLY: check_iostat
USE UM_parcore,       ONLY: mype

USE parkind1,         ONLY: jprb, jpim
USE yomhook,          ONLY: lhook, dr_hook

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

INTEGER :: my_comm
INTEGER :: mpl_nml_type
INTEGER :: ErrorStatus
INTEGER :: icode
REAL(KIND=jprb) :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_DEPOSITION'

CHARACTER(LEN=errormessagelength) :: iomessage

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 3
INTEGER, PARAMETER :: n_int  = 3
INTEGER, PARAMETER :: n_real = 2
INTEGER, PARAMETER :: n_log  = 8

TYPE :: my_namelist
  SEQUENCE
  INTEGER :: dep_h2_soil_scheme
  INTEGER :: dry_dep_model
  INTEGER :: ndry_dep_species
  REAL(KIND=real_jlslsm) :: dzl_const
  REAL(KIND=real_jlslsm) :: tundra_s_limit
  LOGICAL :: l_deposition
  LOGICAL :: l_deposition_flux
  LOGICAL :: l_deposition_from_ukca
  LOGICAL :: l_deposition_gc_corr
  LOGICAL :: l_ukca_ddep_lev1
  LOGICAL :: l_ukca_ddepo3_ocean
  LOGICAL :: l_ukca_dry_dep_so2wet
  LOGICAL :: l_ukca_emsdrvn_ch4
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type, n_int_in = n_int,               &
                    n_real_in = n_real, n_log_in = n_log)

IF (mype == 0) THEN

  READ (UNIT = unitnumber, NML = jules_deposition, IOSTAT = errorstatus,       &
        IOMSG = iomessage)
  CALL check_iostat(errorstatus, "namelist jules_deposition", iomessage)

  my_nml % l_deposition          = l_deposition
  my_nml % l_deposition_flux     = l_deposition_flux
  my_nml % l_deposition_gc_corr  = l_deposition_gc_corr
  my_nml % l_deposition_from_ukca= l_deposition_from_ukca
  my_nml % l_ukca_ddep_lev1      = l_ukca_ddep_lev1
  my_nml % l_ukca_dry_dep_so2wet = l_ukca_dry_dep_so2wet
  my_nml % l_ukca_ddepo3_ocean   = l_ukca_ddepo3_ocean
  my_nml % l_ukca_emsdrvn_ch4    = l_ukca_emsdrvn_ch4
    ! end of logicals
  my_nml % dep_h2_soil_scheme    = dep_h2_soil_scheme
  my_nml % dry_dep_model         = dry_dep_model
  my_nml % ndry_dep_species      = ndry_dep_species
    ! end of integers
  my_nml % dzl_const             = dzl_const
  my_nml % tundra_s_limit        = tundra_s_limit

END IF

CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN

  l_deposition                   = my_nml % l_deposition
  l_deposition_flux              = my_nml % l_deposition_flux
  l_deposition_gc_corr           = my_nml % l_deposition_gc_corr
  l_deposition_from_ukca         = my_nml % l_deposition_from_ukca
  l_ukca_ddep_lev1               = my_nml % l_ukca_ddep_lev1
  l_ukca_dry_dep_so2wet          = my_nml % l_ukca_dry_dep_so2wet
  l_ukca_ddepo3_ocean            = my_nml % l_ukca_ddepo3_ocean
  l_ukca_emsdrvn_ch4             = my_nml % l_ukca_emsdrvn_ch4
    ! end of logicals
  dep_h2_soil_scheme             = my_nml % dep_h2_soil_scheme
  dry_dep_model                  = my_nml % dry_dep_model
  ndry_dep_species               = my_nml % ndry_dep_species
    ! end of integers
  dzl_const                      = my_nml % dzl_const
  tundra_s_limit                 = my_nml % tundra_s_limit
    ! end of reals

END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN
END SUBROUTINE read_nml_jules_deposition

#endif

! ------------------------------------------------------------------------------

END MODULE jules_deposition_mod
