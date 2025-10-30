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

MODULE deposition_check_species_mod

!-----------------------------------------------------------------------------
! Description:
!   Checks that the deposited species read in from the jules_deposition_species
!   namelists match names and order of the deposited species in the UKCA
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGENIC FLUXES.
!
! Code Description:
!   Language: Fortran 90.
!-----------------------------------------------------------------------------

USE ereport_mod,             ONLY: ereport

USE jules_print_mgr,         ONLY: jules_print, jules_message

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName = "DEPOSITION_CHECK_SPECIES_MOD"

CONTAINS

SUBROUTINE deposition_check_species(nspecies_ukca, ndepd_ukca,                 &
  nldepd_ukca, species_ukca)

USE jules_deposition_mod,    ONLY: ndry_dep_species, l_deposition_from_ukca

USE deposition_output_arrays_mod, ONLY: deposition_output_array_char,          &
                                        deposition_output_array_int

USE deposition_species_mod,  ONLY: dep_species_name

USE parkind1,                ONLY: jprb, jpim

USE jules_print_mgr,         ONLY: jules_print, jules_message

USE yomhook,                 ONLY: lhook, dr_hook

IMPLICIT NONE

INTEGER, INTENT(IN)      :: nspecies_ukca
                            ! Number of chemical species in the
                            ! UKCA chemical mechanism
INTEGER, INTENT(IN)      :: ndepd_ukca
                            ! Number of chemical species deposited in the
                            ! UKCA chemical mechanism
INTEGER, INTENT(IN)      :: nldepd_ukca(nspecies_ukca)
                            ! Index in the array species_ukca,
                            ! identifying a species in the chemical mechanism
                            ! that is deposited

CHARACTER(LEN=10), INTENT(IN) :: species_ukca(nspecies_ukca)
                            ! Name of species in the chemical mechanism

!-------------------------------------------------------------------------------

! Local variables

INTEGER              :: i, j, ntimes, nremain
INTEGER              :: errorstatus

INTEGER              :: dep_species_idx(ndry_dep_species)
                            ! Index of chemical species with dry deposition
                            ! for matching with UKCA chemical species list

CHARACTER(LEN=10)    :: species_deposited_ukca(ndepd_ukca)
                            ! Name of species in the chemical mechanism
                            ! that are deposited

CHARACTER(LEN=*), PARAMETER       :: RoutineName='DEPOSITION_CHECK_SPECIES'

INTEGER(KIND=jpim), PARAMETER     :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER     :: zhook_out = 1
REAL(KIND=jprb)                   :: zhook_handle

!-------------------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DO i = 1, ndepd_ukca
  species_deposited_ukca(i) = species_ukca(nldepd_ukca(i))
END DO

WRITE(jules_message,'(A)') 'In deposition_check_species:'
CALL jules_print(RoutineName,jules_message)

WRITE(jules_message,'(A,3I6)') 'Deposition & species parameters     = ',       &
  nspecies_ukca, ndepd_ukca, ndry_dep_species
CALL jules_print(RoutineName,jules_message)

WRITE(jules_message,'(A)') 'UKCA chemical species names        = '
CALL jules_print(RoutineName,jules_message)
CALL deposition_output_array_char(nspecies_ukca, species_ukca)

WRITE(jules_message,'(A)') 'UKCA tracer indices for deposition = '
CALL jules_print(RoutineName,jules_message)
CALL deposition_output_array_int(nspecies_ukca, nldepd_ukca)

WRITE(jules_message,'(A)') 'UKCA deposited species names       = '
CALL jules_print(RoutineName,jules_message)
CALL deposition_output_array_char(ndepd_ukca, species_deposited_ukca)

WRITE(jules_message,'(A)') 'JULES deposition species names     = '
CALL jules_print(RoutineName,jules_message)
CALL deposition_output_array_char(ndry_dep_species, dep_species_name)

! Check that the order of species in the deposition_species namelists
! matches the order of deposited species in the UKCA

IF ( ndry_dep_species /= ndepd_ukca ) THEN
  errorstatus = 1
  CALL ereport(ModuleName//':'//RoutineName, errorstatus,                      &
    'Mismatch in # of JULES deposition species and UKCA deposited species')
END IF

IF ( .NOT. l_deposition_from_ukca ) THEN

  ! Check that order of species in the deposition_species namelist
  ! and UKCA deposited species is the same. Loop over ndry_dep_species
  dep_species_idx(:) = -1
  DO i = 1, ndry_dep_species
    IF (TRIM(ADJUSTL(dep_species_name(i))) ==                                  &
        TRIM(ADJUSTL(species_deposited_ukca(i)))) THEN
      dep_species_idx(i) = i
    END IF
  END DO

  WRITE(jules_message,'(A)') 'JULES deposition tracer index = '
  CALL jules_print(RoutineName,jules_message)
  CALL deposition_output_array_int(ndry_dep_species, dep_species_idx)

  ! Check that all are set
  IF ( ANY( dep_species_idx == -1) ) THEN
    errorstatus = 2
    CALL ereport(ModuleName//':'//RoutineName, errorstatus,                    &
                  'Check order of species in deposition_species ' //           &
                  'namelists as these do not match the order of ' //           &
                  'UKCA deposited species' )

  END IF

END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN

END SUBROUTINE deposition_check_species

END MODULE deposition_check_species_mod
