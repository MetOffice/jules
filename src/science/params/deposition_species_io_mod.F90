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

MODULE deposition_species_io_mod

!-----------------------------------------------------------------------------
! Description:
!   Module containing input species and parameters for the deposition model,
!     and code for checking their values.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGENIC FLUXES.
!
! Code Description:
!   Language: Fortran 90.
!-----------------------------------------------------------------------------

USE deposition_species_mod, ONLY: species_name_len

USE deposition_output_arrays_mod, ONLY: deposition_output_array_real

USE jules_deposition_mod,   ONLY: ndry_dep_species

USE jules_print_mgr,  ONLY: jules_print, jules_message, newline

USE errormessagelength_mod, ONLY: errormessagelength

USE max_dimensions,         ONLY: ndep_species_max, ntype_max

USE um_types,               ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Module containing variables used for reading deposition species
!   parameters, such as fixed-size versions of parameters, and an associated
!   namelist.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGENIC FLUXES.
!
! Code Description:
!   Language: Fortran 90.
!-----------------------------------------------------------------------------

! Public module variables

! Scalar variables.
REAL(KIND=real_jlslsm) ::                                                      &
  diffusion_coeff_io,                                                          &
    ! Diffusion coefficient (m2 s-1).
  dep_species_rmm_io,                                                          &
    ! Relative molecular mass (g mol-1).
  diffusion_corr_io,                                                           &
    ! Diffusion correction for stomatal resistance, accounting for the
    ! different diffusivities of water and other species (dimensionless).
  r_tundra_io
    ! Surface resistance used in tundra region (s m-1).

CHARACTER(LEN=species_name_len) ::                                             &
  dep_species_name_io

! Array variables.
REAL(KIND=real_jlslsm) ::                                                      &
  dd_ice_coeff_io(3),                                                          &
    ! Coefficients in quadratic function relating dry deposition over ice to
    ! temperature.
  rsurf_std_io(ntype_max)
    ! Standard surface resistance used for each surface type (s m-1).

#if defined(UM_JULES)
! TYPE structure to hold all deposition i/o parameters/variables
! For use in coupled/UM-JULES applications
TYPE :: deposition_io_data_type
    ! Species-specific parameters/variables
  REAL(KIND=real_jlslsm) :: ch4_scaling_io
  REAL(KIND=real_jlslsm) :: ch4_mml_io
  REAL(KIND=real_jlslsm) :: ch4dd_tundra_io(4)
  REAL(KIND=real_jlslsm) :: ch4_up_flux_io(ntype_max)
  REAL(KIND=real_jlslsm) :: cuticle_o3_io
  REAL(KIND=real_jlslsm) :: h2dd_c_io(ntype_max)
  REAL(KIND=real_jlslsm) :: h2dd_m_io(ntype_max)
  REAL(KIND=real_jlslsm) :: h2dd_q_io(ntype_max)
  REAL(KIND=real_jlslsm) :: r_wet_soil_o3_io
    ! Parameters/variables with species dimension
  REAL(KIND=real_jlslsm) :: dd_ice_coeff_io(ndep_species_max, 3)
  REAL(KIND=real_jlslsm) :: diffusion_coeff_io(ndep_species_max)
  REAL(KIND=real_jlslsm) :: dep_species_rmm_io(ndep_species_max)
  REAL(KIND=real_jlslsm) :: diffusion_corr_io(ndep_species_max)
  REAL(KIND=real_jlslsm) :: r_tundra_io(ndep_species_max)
  REAL(KIND=real_jlslsm) :: rsurf_std_io(ntype_max, ndep_species_max)
  CHARACTER(LEN=10)      :: dep_species_name_io(ndep_species_max)
END TYPE deposition_io_data_type

TYPE (deposition_io_data_type) :: deposition_io_data_all
#endif

!-----------------------------------------------------------------------------
! Namelist
!-----------------------------------------------------------------------------
NAMELIST / jules_deposition_species /                                          &
  dd_ice_coeff_io, dep_species_name_io, diffusion_coeff_io,                    &
  diffusion_corr_io, r_tundra_io, rsurf_std_io, dep_species_rmm_io

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='DEPOSITION_SPECIES_IO_MOD'

CONTAINS

!-----------------------------------------------------------------------------
SUBROUTINE print_nlist_jules_deposition_species()
!-----------------------------------------------------------------------------

IMPLICIT NONE

!Arguments
CHARACTER(LEN=*), PARAMETER ::                                                 &
  RoutineName = 'PRINT_NLIST_JULES_DEPOSITION_SPECIES'
    ! Name of this procedure.

CALL jules_print(RoutineName,                                                  &
                 'Contents of namelist jules_deposition_species')

! Common to all species
WRITE(jules_message,'(2A)') ' dep_species_name_io = ',dep_species_name_io
CALL jules_print(RoutineName,jules_message)

WRITE(jules_message,'(A,G11.4E2)') ' diffusion_coeff_io = ',diffusion_coeff_io
CALL jules_print(RoutineName,jules_message)

WRITE(jules_message,'(A,G11.4E2)') ' dep_species_rmm_io = ',dep_species_rmm_io
CALL jules_print(RoutineName,jules_message)

WRITE(jules_message,'(A,G11.4E2)') ' diffusion_corr_io = ',diffusion_corr_io
CALL jules_print(RoutineName,jules_message)

WRITE(jules_message,'(A)') ' rsurf_std_io = '
CALL jules_print(RoutineName,jules_message)
CALL deposition_output_array_real(ntype_max, rsurf_std_io(:))

WRITE(jules_message,'(A,G11.4E2)') ' r_tundra_io    = ',r_tundra_io
CALL jules_print(RoutineName,jules_message)

WRITE(jules_message,'(A,3(G11.4E2,1X))') ' dd_ice_coeff_io  = ',               &
  dd_ice_coeff_io(:)
CALL jules_print(RoutineName,jules_message)

CALL jules_print(RoutineName,                                                  &
                 '- - - - - - end of namelist - - - - - -')

END SUBROUTINE print_nlist_jules_deposition_species

#if defined(UM_JULES)
!-----------------------------------------------------------------------------
SUBROUTINE read_nml_jules_deposition_species (unitnumber)
!-----------------------------------------------------------------------------

! Description:
!  Read the DEPOSITION_SPECIES namelist

USE setup_namelist,   ONLY: setup_nml_type
USE missing_data_mod, ONLY: rmdi
USE check_iostat_mod, ONLY: check_iostat
USE UM_parcore,       ONLY: mype

USE parkind1,         ONLY: jprb, jpim
USE yomhook,          ONLY: lhook, dr_hook

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

INTEGER :: my_comm
INTEGER :: mpl_nml_type
INTEGER :: errorstatus
INTEGER :: icode
INTEGER :: ispecies
REAL(KIND=jprb) :: zhook_handle

CHARACTER(LEN=*), PARAMETER ::                                                 &
  RoutineName='READ_NML_JULES_DEPOSITION_SPECIES'

CHARACTER(LEN=errormessagelength) :: iomessage

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

! set number of each type of variable in my_namelist_species type
INTEGER, PARAMETER :: no_of_types = 2
INTEGER, PARAMETER :: n_int       = 0
INTEGER, PARAMETER :: n_real      = 3+4+ntype_max
INTEGER, PARAMETER :: n_log       = 0
INTEGER, PARAMETER :: n_chars     = 10

! SPICE NAG compiler gave error with 2 'my_namelist' TYPES in module
TYPE :: my_namelist_species
  SEQUENCE
    ! Parameters/variables with species dimension
  REAL(KIND=real_jlslsm) :: dd_ice_coeff_io(3)
  REAL(KIND=real_jlslsm) :: diffusion_coeff_io
  REAL(KIND=real_jlslsm) :: dep_species_rmm_io
  REAL(KIND=real_jlslsm) :: diffusion_corr_io
  REAL(KIND=real_jlslsm) :: r_tundra_io
  REAL(KIND=real_jlslsm) :: rsurf_std_io(ntype_max)
  CHARACTER(LEN=10)      :: dep_species_name_io
END TYPE my_namelist_species

TYPE (my_namelist_species) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! All PEs - set IO status as needed for tests

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type,                                 &
                    n_real_in = n_real, n_chars_in = n_chars)

! Based on UM routine rcf_readnl_ideal_free_tracer
IF (mype == 0) THEN
  ! Continue to read the jules_deposition_species namelists
  ! until end of file or other error code
  REWIND(UNIT=unitnumber)
  CALL jules_print(RoutineName,                                                &
               'Check assignment to deposition_io_data_all')
END IF

DO ispecies = 1,ndry_dep_species

  ! Only reads namelist on PE 0
  IF (mype == 0) THEN

    ! Initialise namelist to missing data
    ! - need to do this each time before reading
    dep_species_name_io              = 'unset'
    dd_ice_coeff_io(:)               = rmdi
    diffusion_coeff_io               = rmdi
    dep_species_rmm_io               = rmdi
    diffusion_corr_io                = rmdi
    r_tundra_io                      = rmdi
    rsurf_std_io(:)                  = rmdi

    ! Read namelist
    READ (UNIT = unitnumber, NML = jules_deposition_species,                   &
          IOSTAT = errorstatus, IOMSG = iomessage)
    CALL check_iostat(errorstatus, 'namelist jules_deposition_species',        &
          iomessage)
  END IF

  IF (mype == 0) THEN
    ! Print namelist just read in
    WRITE(jules_message,'(A,2I6)')    ' species index    = ',                  &
          ispecies, mype
    CALL jules_print(RoutineName,jules_message)

    CALL print_nlist_jules_deposition_species()

    ! Copy to my_nml
    my_nml % dep_species_name_io     = dep_species_name_io
    my_nml % dd_ice_coeff_io         = dd_ice_coeff_io
    my_nml % diffusion_coeff_io      = diffusion_coeff_io
    my_nml % dep_species_rmm_io      = dep_species_rmm_io
    my_nml % diffusion_corr_io       = diffusion_corr_io
    my_nml % r_tundra_io             = r_tundra_io
    my_nml % rsurf_std_io            = rsurf_std_io

  END IF

  ! copy to other PEs
  CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

  IF (mype /= 0) THEN

    ! Copy namelist back
    dd_ice_coeff_io                  = my_nml % dd_ice_coeff_io
    diffusion_coeff_io               = my_nml % diffusion_coeff_io
    dep_species_rmm_io               = my_nml % dep_species_rmm_io
    diffusion_corr_io                = my_nml % diffusion_corr_io
    r_tundra_io                      = my_nml % r_tundra_io
    rsurf_std_io                     = my_nml % rsurf_std_io
    dep_species_name_io              = my_nml % dep_species_name_io

  END IF

  ! All PEs copy namelist info into structure deposition_io_data_all

  ! Variables that have a value for every species
  ! (even if not necessarily used for all species).
  deposition_io_data_all % dep_species_name_io(ispecies) =                     &
      dep_species_name_io
  deposition_io_data_all % dd_ice_coeff_io(ispecies,:)   =                     &
      dd_ice_coeff_io(:)
  deposition_io_data_all % diffusion_coeff_io(ispecies)  =                     &
      diffusion_coeff_io
  deposition_io_data_all % dep_species_rmm_io(ispecies)  =                     &
      dep_species_rmm_io
  deposition_io_data_all % diffusion_corr_io(ispecies)   =                     &
      diffusion_corr_io
  deposition_io_data_all % r_tundra_io(ispecies)         =                     &
      r_tundra_io
  deposition_io_data_all % rsurf_std_io(:,ispecies)      =                     &
      rsurf_std_io(:)

END DO ! Loop over ispecies

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN
END SUBROUTINE read_nml_jules_deposition_species
#endif

! ------------------------------------------------------------------------------

END MODULE deposition_species_io_mod
