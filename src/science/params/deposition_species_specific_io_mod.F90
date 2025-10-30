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

MODULE deposition_species_specific_io_mod

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

#if defined (UM_JULES)
USE deposition_species_io_mod, ONLY: deposition_io_data_all
#endif

USE deposition_output_arrays_mod, ONLY: deposition_output_array_real

USE jules_print_mgr,  ONLY: jules_print, jules_message

USE errormessagelength_mod, ONLY: errormessagelength

USE max_dimensions,         ONLY: ntype_max

USE um_types,               ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Module containing variables used for reading the namelist
!   jules_deposition_species_specific, which contains deposition parameters
!   for a specific deposited chemical species.
!
!   The subroutines below have been moved from deposition_species_io_mod
!   as the SPICE NAG compiler gave a compilation error if 2 'my_namelist' TYPES
!   are present in the same module
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
  ch4_scaling_io,                                                              &
    ! Scaling applied to CH4 soil uptake (dimensionless).
    ! Originally this was used to match the value from the IPCC TAR.
  ch4_mml_io,                                                                  &
    ! Factor used to convert methane flux in ug m-2 h-1 to dry dep vel in
    ! m s-1; MML=3600*0.016*1.0E9*1.75E-6, where 0.016=RMM methane (kg),
    ! 1.0E9 converts ug -> kg, 1.75E-6 = assumed CH4 vmr
  cuticle_o3_io,                                                               &
    ! Constant for calculation of cuticular resistance for ozone (s m-1).
  r_wet_soil_o3_io
    ! Wet soil surface resistance for ozone (s m-1).

! Array variables.
REAL(KIND=real_jlslsm) ::                                                      &
  ch4dd_tundra_io(4),                                                          &
    ! Coefficients of cubic polynomial relating CH4 loss for tundra to
    ! temperature.
  ch4_up_flux_io(ntype_max),                                                   &
    ! CH4 uptake fluxes, in ug m-2 hr-1
  h2dd_c_io(ntype_max),                                                        &
    ! Constant in quadratic function relating hydrogen deposition to soil
    ! moisture, for each surface type (s m-1).
  h2dd_m_io(ntype_max),                                                        &
    ! Coefficient of first order term in quadratic function relating hydrogen
    ! deposition to soil moisture, for each surface type (s m-1).
  h2dd_q_io(ntype_max)
    ! Coefficient of second order term in quadratic function relating
    ! hydrogen deposition to soil moisture, for each surface type (s m-1).

!-----------------------------------------------------------------------------
! Namelist
!-----------------------------------------------------------------------------
NAMELIST / jules_deposition_species_specific /                                 &
  ch4_scaling_io, ch4_mml_io, ch4dd_tundra_io, ch4_up_flux_io, cuticle_o3_io,  &
  h2dd_c_io, h2dd_m_io, h2dd_q_io, r_wet_soil_o3_io

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName='DEPOSITION_SPECIES_SPECIFIC_IO_MOD'

CONTAINS

!-----------------------------------------------------------------------------
SUBROUTINE print_nlist_jules_deposition_species_specific()
!-----------------------------------------------------------------------------

IMPLICIT NONE

!Arguments
CHARACTER(LEN=*), PARAMETER ::                                                 &
  RoutineName = 'PRINT_NLIST_JULES_DEPOSITION_SPECIES_SPECIFIC'
    ! Name of this procedure.

CALL jules_print(RoutineName,                                                  &
                 'Contents of namelist jules_deposition_species_specific')

CALL jules_print(RoutineName,                                                  &
                 'Parameters specific to CH4 deposition')

WRITE(jules_message,'(A,G11.4E2)') ' ch4_scaling     = ',ch4_scaling_io
CALL jules_print(RoutineName,jules_message)

WRITE(jules_message,'(A,G11.4E2)') ' ch4_mml_io      = ',ch4_mml_io
CALL jules_print(RoutineName,jules_message)

WRITE(jules_message,'(A,4(G11.4E2,1X))') ' ch4dd_tundra_io = ',                &
  ch4dd_tundra_io(:)
CALL jules_print(RoutineName,jules_message)

WRITE(jules_message,'(A)') ' ch4_up_flux_io = '
CALL jules_print(RoutineName,jules_message)
CALL deposition_output_array_real(ntype_max, ch4_up_flux_io(:))

CALL jules_print(RoutineName,                                                  &
                 'Parameters specific to O3 deposition')

WRITE(jules_message,'(A,G11.4E2)') ' cuticle_o3_io = ',cuticle_o3_io
CALL jules_print(RoutineName,jules_message)

WRITE(jules_message,'(A,G11.4E2)') ' r_wet_soil_o3_io = ',r_wet_soil_o3_io
CALL jules_print(RoutineName,jules_message)

CALL jules_print(RoutineName,                                                  &
                 'Parameters specific to H2 deposition')

WRITE(jules_message,'(A)') ' h2dd_c_io = '
CALL jules_print(RoutineName,jules_message)
CALL deposition_output_array_real(ntype_max, h2dd_c_io(:))

WRITE(jules_message,'(A)') ' h2dd_m_io = '
CALL jules_print(RoutineName,jules_message)
CALL deposition_output_array_real(ntype_max, h2dd_m_io(:))

WRITE(jules_message,'(A)') ' h2dd_q_io = '
CALL jules_print(RoutineName,jules_message)
CALL deposition_output_array_real(ntype_max, h2dd_q_io(:))

CALL jules_print(RoutineName,                                                  &
                 '- - - - - - end of namelist - - - - - -')

END SUBROUTINE print_nlist_jules_deposition_species_specific

#if defined(UM_JULES)
!-----------------------------------------------------------------------------
SUBROUTINE read_nml_jules_deposition_species_specific (unitnumber)
!-----------------------------------------------------------------------------

! Description:
!  Read the JULES_DEPOSITION_SPECIES_SPECIFIC namelist

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
REAL(KIND=jprb) :: zhook_handle

CHARACTER(LEN=*), PARAMETER ::                                                 &
  RoutineName='READ_NML_JULES_DEPOSITION_SPECIES_SPECIFIC'

CHARACTER(LEN=errormessagelength) :: iomessage

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

! set number of each type of variable in my_namelist_specific type
INTEGER, PARAMETER :: no_of_types = 1
INTEGER, PARAMETER :: n_int       = 0
INTEGER, PARAMETER :: n_real      = 4+4+(4*ntype_max)
INTEGER, PARAMETER :: n_log       = 0
INTEGER, PARAMETER :: n_chars     = 0

TYPE :: my_namelist
  SEQUENCE
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
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! All PEs - set IO status as needed for tests

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type, n_real_in = n_real)

! Based on UM routine rcf_readnl_ideal_free_tracer
IF (mype == 0) THEN
  ! Continue to read the jules_deposition_species_specific namelist
  ! until end of file or other error code
  REWIND(UNIT=unitnumber)
  CALL jules_print(RoutineName,                                                &
               'Check assignment to deposition_io_data_all')
END IF

! Only reads namelist on PE 0
IF (mype == 0) THEN

  ! Initialise namelist to missing data
  ! - need to do this each time before reading
  ch4_scaling_io                   = rmdi
  ch4_mml_io                       = rmdi
  ch4dd_tundra_io(:)               = rmdi
  ch4_up_flux_io(:)                = rmdi
  cuticle_o3_io                    = rmdi
  h2dd_c_io(:)                     = rmdi
  h2dd_m_io(:)                     = rmdi
  h2dd_q_io(:)                     = rmdi
  r_wet_soil_o3_io                 = rmdi

  ! Read namelist
  READ (UNIT = unitnumber, NML = jules_deposition_species_specific,            &
        IOSTAT = errorstatus, IOMSG = iomessage)
  CALL check_iostat(errorstatus, 'namelist jules_deposition_species_specific', &
        iomessage)
END IF

IF (mype == 0) THEN
  ! Print namelist just read in
  CALL print_nlist_jules_deposition_species_specific()

  ! Copy to my_nml
  my_nml % ch4_scaling_io          = ch4_scaling_io
  my_nml % ch4_mml_io              = ch4_mml_io
  my_nml % ch4dd_tundra_io         = ch4dd_tundra_io
  my_nml % ch4_up_flux_io          = ch4_up_flux_io
  my_nml % cuticle_o3_io           = cuticle_o3_io
  my_nml % h2dd_c_io               = h2dd_c_io
  my_nml % h2dd_m_io               = h2dd_m_io
  my_nml % h2dd_q_io               = h2dd_q_io
  my_nml % r_wet_soil_o3_io        = r_wet_soil_o3_io

END IF

! copy to other PEs
CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN

  ! Copy namelist back
  ch4_scaling_io                   = my_nml % ch4_scaling_io
  ch4_mml_io                       = my_nml % ch4_mml_io
  ch4dd_tundra_io                  = my_nml % ch4dd_tundra_io
  ch4_up_flux_io                   = my_nml % ch4_up_flux_io
  cuticle_o3_io                    = my_nml % cuticle_o3_io
  h2dd_c_io                        = my_nml % h2dd_c_io
  h2dd_m_io                        = my_nml % h2dd_m_io
  h2dd_q_io                        = my_nml % h2dd_q_io
  r_wet_soil_o3_io                 = my_nml % r_wet_soil_o3_io

END IF

! All PEs copy namelist info into structure deposition_io_data_all

deposition_io_data_all % ch4_scaling_io          = ch4_scaling_io
deposition_io_data_all % ch4_mml_io              = ch4_mml_io
deposition_io_data_all % ch4dd_tundra_io         = ch4dd_tundra_io
deposition_io_data_all % ch4_up_flux_io          = ch4_up_flux_io

deposition_io_data_all % h2dd_c_io               = h2dd_c_io
deposition_io_data_all % h2dd_m_io               = h2dd_m_io
deposition_io_data_all % h2dd_q_io               = h2dd_q_io

deposition_io_data_all % cuticle_o3_io           = cuticle_o3_io
deposition_io_data_all % r_wet_soil_o3_io        = r_wet_soil_o3_io

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN
END SUBROUTINE read_nml_jules_deposition_species_specific
#endif

! ------------------------------------------------------------------------------

END MODULE deposition_species_specific_io_mod
