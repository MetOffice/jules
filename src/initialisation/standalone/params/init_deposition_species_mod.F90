#if !defined(UM_JULES)
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

MODULE init_deposition_species_mod

!-----------------------------------------------------------------------------
! Description:
!   Module containing routine to read deposition_species namelists
!   in standalone model
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGENIC FLUXES.
!
! Code Description:
!   Language: Fortran 90.
!-----------------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------------
SUBROUTINE init_deposition_species(nml_dir)
!-----------------------------------------------------------------------------

USE deposition_initialisation_aerod_mod, ONLY:                                 &
  deposition_initialisation_aerod_jules, deposition_initialisation_aerod_ukca

USE deposition_initialisation_surfddr_mod,                                     &
                             ONLY: deposition_initialisation_surfddr_ukca

USE deposition_species_io_mod, ONLY:                                           &
! imported scalars and arrays
  dd_ice_coeff_io, dep_species_name_io, dep_species_rmm_io,                    &
  diffusion_coeff_io, diffusion_corr_io, rsurf_std_io, r_tundra_io,            &
! imported namelists & routines
  jules_deposition_species,                                                    &
  print_nlist_jules_deposition_species

USE deposition_species_specific_io_mod, ONLY:                                  &
! imported scalars and arrays
  ch4_scaling_io, ch4_mml_io, ch4_up_flux_io, ch4dd_tundra_io,                 &
  cuticle_o3_io, h2dd_c_io, h2dd_m_io, h2dd_q_io, r_wet_soil_o3_io,            &
! imported namelists & routines
  jules_deposition_species_specific,                                           &
  print_nlist_jules_deposition_species_specific

USE deposition_species_mod, ONLY:                                              &
! imported procedures
  check_jules_deposition_species,                                              &
! imported scalars
  ch4_scaling, ch4_mml, cuticle_o3, r_wet_soil_o3,                             &
! imported arrays
  ch4dd_tundra, ch4_up_flux, dd_ice_coeff, dep_species_name, diffusion_coeff,  &
  diffusion_corr, h2dd_c, h2dd_m, h2dd_q, r_tundra, rsurf_std,                 &
  dep_species_rmm

USE jules_deposition_mod, ONLY:                                                &
! imported scalars
  l_deposition, ndry_dep_species, dry_dep_model, dry_dep_model_ukca,           &
  dry_dep_model_jules

USE jules_surface_types_mod, ONLY:                                             &
! imported scalars
  ntype

USE io_constants, ONLY: namelist_unit
USE missing_data_mod, ONLY:                                                    &
! imported scalar parameters
  rmdi
USE string_utils_mod, ONLY: to_string
USE errormessagelength_mod, ONLY: errormessagelength
USE logging_mod, ONLY: log_info, log_warn, log_error, log_fatal

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN).
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

!-----------------------------------------------------------------------------
! Parameters.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName='INIT_DEPOSITION_SPECIES'

!-----------------------------------------------------------------------------
! Scalar variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  ERROR,                                                                       &
    ! Error indicator
  i,                                                                           &
    ! Loop counter.
  j
    ! Loop counter.

CHARACTER(LEN=errormessagelength) :: iomessage

!-----------------------------------------------------------------------------
! There's nothing to do if deposition is not selected.
!-----------------------------------------------------------------------------
IF ( .NOT. l_deposition ) THEN
  RETURN
END IF

!-----------------------------------------------------------------------------
! Read deposition species namelists, one per species.
!-----------------------------------------------------------------------------
CALL log_info(RoutineName, "Reading JULES_DEPOSITION namelist...")

! Open the namelist file.
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'jules_deposition.nml'),     &
     STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR,           &
     IOMSG = iomessage)

IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error opening namelist file jules_deposition.nml " //        &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

!-----------------------------------------------------------------------------
! Loop over the species.
!-----------------------------------------------------------------------------
DO i = 1, ndry_dep_species

  ! Initialise namelist values before reading the next species.
  dep_species_name_io = 'unset'
  dd_ice_coeff_io(:)  = rmdi
  diffusion_coeff_io  = rmdi
  dep_species_rmm_io  = rmdi
  diffusion_corr_io   = rmdi
  r_tundra_io         = rmdi
  rsurf_std_io(:)     = rmdi

  ! Read namelist for this species.
  READ( namelist_unit, NML = jules_deposition_species, IOSTAT = ERROR,         &
       IOMSG = iomessage)

  IF ( ERROR /= 0 ) THEN
    CALL log_fatal(RoutineName,                                                &
                   "Error reading namelist JULES_DEPOSITION_SPECIES " //       &
                   "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //        &
                   TRIM(iomessage) // ")")
  END IF

  CALL print_nlist_jules_deposition_species()

  !---------------------------------------------------------------------------
  ! Load values into the final variables.
  !---------------------------------------------------------------------------
  ! Variables that have a value for every species (even if not necessarily
  ! used for all species).
  dep_species_name(i) = dep_species_name_io
  dd_ice_coeff(i,:)   = dd_ice_coeff_io(:)
  diffusion_coeff(i)  = diffusion_coeff_io
  dep_species_rmm(i)  = dep_species_rmm_io
  diffusion_corr(i)   = diffusion_corr_io
  r_tundra(i)         = r_tundra_io
  rsurf_std(:,i)      = rsurf_std_io(1:ntype)

END DO  !  species

!-----------------------------------------------------------------------------
! Close the namelist file
!-----------------------------------------------------------------------------
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error closing namelist file jules_deposition.nml " //        &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

!-----------------------------------------------------------------------------
! Now read the jules_deposition_species_specific namelist
!-----------------------------------------------------------------------------
CALL log_info(RoutineName, "Reading JULES_DEPOSITION namelist...")

! Open the namelist file.
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'jules_deposition.nml'),     &
     STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR,           &
     IOMSG = iomessage)

IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error opening namelist file jules_deposition.nml " //        &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

! Initialise namelist values
ch4_scaling_io      = rmdi
ch4_mml_io          = rmdi
ch4dd_tundra_io(:)  = rmdi
ch4_up_flux_io(:)   = rmdi
cuticle_o3_io       = rmdi
h2dd_c_io(:)        = rmdi
h2dd_m_io(:)        = rmdi
h2dd_q_io(:)        = rmdi
r_wet_soil_o3_io    = rmdi

! Read namelist for this species.
READ( namelist_unit, NML = jules_deposition_species_specific, IOSTAT = ERROR,  &
     IOMSG = iomessage)

IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error reading namelist JULES_DEPOSITION_SPECIES_SPECIFIC " //&
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

CALL print_nlist_jules_deposition_species_specific()

!---------------------------------------------------------------------------
! Load values into the final variables.
!---------------------------------------------------------------------------
DO i = 1, ndry_dep_species

  ! Deal with variables that only apply to one species.
  SELECT CASE ( dep_species_name(i) )

  CASE ( 'CH4' )
    ch4_scaling     = ch4_scaling_io
    ch4_mml         = ch4_mml_io
    ch4dd_tundra(:) = ch4dd_tundra_io(:)
    ch4_up_flux(:)  = ch4_up_flux_io(1:ntype)

  CASE ( 'H2' )
    h2dd_c(:) = h2dd_c_io(1:ntype)
    h2dd_m(:) = h2dd_m_io(1:ntype)
    h2dd_q(:) = h2dd_q_io(1:ntype)

  CASE ( 'O3' )
    cuticle_o3    = cuticle_o3_io
    r_wet_soil_o3 = r_wet_soil_o3_io

  END SELECT

END DO  !  species

!-----------------------------------------------------------------------------
! Close the namelist file
!-----------------------------------------------------------------------------
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error closing namelist file jules_deposition.nml " //        &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

!-----------------------------------------------------------------------------
! Check the values and print information about the parameters.
!-----------------------------------------------------------------------------
CALL check_jules_deposition_species(ntype)

!-----------------------------------------------------------------------------
! Initialise the parameters for the calculation of
! (1) the aerodynamic (Ra) & quasi-laminar (Rb) resistance terms
! (2) surface resistance (Rc) terms (for dry_dep_model=dry_dep_model_ukca)
!-----------------------------------------------------------------------------
SELECT CASE ( dry_dep_model )
CASE ( dry_dep_model_ukca )
  CALL deposition_initialisation_aerod_ukca()
  CALL deposition_initialisation_surfddr_ukca()
CASE ( dry_dep_model_jules )
  CALL deposition_initialisation_aerod_jules()
END SELECT

RETURN

END SUBROUTINE init_deposition_species

END MODULE init_deposition_species_mod

#endif
