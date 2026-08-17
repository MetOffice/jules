! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module with setting of
! irrigation switch

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.

MODULE c_irrigation_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

INTEGER, ALLOCATABLE ::                                                        &
  irrig_tile(:)         ! Switch to indicate irrigation for each tile
!                         0 - Not irrigated
!                         1 - Irrigated


CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='C_IRRIGATION_MOD'

CONTAINS

SUBROUTINE c_irrigation_alloc(ntype)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook
USE missing_data_mod, ONLY: imdi
USE jules_irrig_mod, ONLY: irrig_option, tile_based_irrigation

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: ntype

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='C_IRRIGATION_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

ALLOCATE( irrig_tile(ntype) )

! Initialise irrig_tile to zero if irrig_option = tile_based_irrigation (2),
! else set to missing data.
! This clause can be removed when irrigation on non-veg tiles is added.
IF (irrig_option == tile_based_irrigation) THEN
  irrig_tile(:) = 0
ELSE
  irrig_tile(:) = imdi
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE c_irrigation_alloc

#if !defined(UM_JULES) && !defined(LFRIC)
SUBROUTINE check_irrigation()

USE jules_surface_types_mod, ONLY: c3_irrig, c4_irrig, ntype, npft
USE logging_mod, ONLY: log_info, log_warn, log_error, log_fatal
USE missing_data_mod, ONLY: imdi
USE string_utils_mod, ONLY: to_string
USE jules_irrig_mod, ONLY: irrig_option

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Checks to irrig_tile variable
!-----------------------------------------------------------------------------

! Work variables
INTEGER :: ERROR  ! Error indicator
INTEGER :: i      ! Loop counter.

CHARACTER(LEN=*), PARAMETER :: routinename='CHECK_IRRIGATION'

! ----------------------------------------------------------------------------
! Check to ensure that the values are the allowed values i.e. either 1 or 0
! and check to ensure that c3_irrig and c4_irrig tiles are the only ones to
! have irrig_tile == 1
! ----------------------------------------------------------------------------
ERROR = 0
IF (ANY(irrig_tile(1:npft) /= imdi)) THEN
  IF (ALL((irrig_tile(1:npft) == 0) .OR. (irrig_tile(1:npft) == 1))) THEN
    CALL log_info(routinename, "irrig_pft has allowed values i.e. 1 and 0" //  &
                 " Using irrig_tile values = "                             //  &
                  TRIM(to_string(irrig_tile(:))) )
  ELSE
    ERROR = 1
    CALL log_fatal(routinename, "Incorrect values entered for "            //  &
                  "irrig_pft, allowed values either 1 or 0 - "             //  &
                  " irrig_tile values = "                                  //  &
                  TRIM(to_string(irrig_tile(:))) )
  END IF

  DO i = 1, ntype
    IF (irrig_tile(i) == 1 .AND. (i /= c3_irrig .AND. i /= c4_irrig)) THEN
        ! Can only irrigate c3_irrig and c4_irrig tiles
        ! Generate error if any other tile is selected
      ERROR = 1
      CALL log_fatal(routinename, "Selected tiles cannot be irrigated, "   //  &
                    "can only select c3_irrig and c4_irrig")
    ELSE IF (irrig_tile(i) == 1) THEN
      CALL log_info(routinename, "Using tiles c3_irrig and/or c4_irrig")
    END IF
  END DO
ELSE
  CALL log_info(routinename, "Irrigation is not being applied to surface " //  &
                "types, irrig_tile values set to imdi "                    //  &
                 TRIM(to_string(irrig_tile(:))) )
  CALL log_info(routinename, "irrig_option set to "                        //  &
                 TRIM(to_string(irrig_option)) )
END IF

RETURN
END SUBROUTINE check_irrigation

SUBROUTINE c_irrigation_print()

USE jules_print_mgr, ONLY: jules_print

IMPLICIT NONE
CHARACTER(LEN=50000) :: lineBuffer
CHARACTER(LEN=*), PARAMETER :: RoutineName='C_IRRIGATION_PRINT'

CALL jules_print(RoutineName, 'Contents of c_irrigation_mod')

WRITE(lineBuffer,*)' irrig_tile = ', irrig_tile(:)
CALL jules_print(RoutineName,lineBuffer)

CALL jules_print(RoutineName,                                                  &
    '- - - - - - end of c_irrigation_mod - - - - - -')

END SUBROUTINE c_irrigation_print
#endif

#if defined(UM_JULES) && !defined(LFRIC)
SUBROUTINE read_nml_c_irrigation_bcast(ntype)

USE setup_namelist, ONLY: setup_nml_type
USE UM_parcore,     ONLY: mype
USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
USE errormessagelength_mod, ONLY: errormessagelength
USE max_dimensions, ONLY: ntype_max

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: ntype

! Local variables
INTEGER :: my_comm
INTEGER :: mpl_nml_type
INTEGER :: icode

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_C_IRRIGATION_BCAST'
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle
CHARACTER(LEN=errormessagelength) :: iomessage

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 1
INTEGER, PARAMETER :: n_int = ntype_max

TYPE :: my_namelist
  SEQUENCE
  INTEGER :: irrig_tile(ntype_max)
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type, n_int_in = n_int)

IF ( mype == 0 ) THEN
  my_nml % irrig_tile(1:ntype) = irrig_tile(:)
END IF

CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN
  irrig_tile(:) = my_nml % irrig_tile(1:ntype)
END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE read_nml_c_irrigation_bcast
#endif

END MODULE c_irrigation_mod
