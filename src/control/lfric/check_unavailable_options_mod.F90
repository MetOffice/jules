#if defined(LFRIC)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt

MODULE check_unavailable_options_mod

! This contains options unavailable to LFRic apps which have had the namelist
! items plumbed through jules_physics_init. It does not contain those with
! hardwired values.

IMPLICIT NONE

CONTAINS

SUBROUTINE check_unavailable_options()

USE ereport_mod, ONLY: ereport
USE jules_print_mgr, ONLY:                                                     &
    jules_message,                                                             &
    jules_print,                                                               &
    jules_format,                                                              &
    PrNorm

USE jules_surface_mod, ONLY: l_anthrop_heat_src, anthrop_heat_option, dukes

IMPLICIT NONE

!Local variables
INTEGER :: errcode, error_sum
CHARACTER(LEN=*), PARAMETER :: RoutineName='CHECK_UNAVAILABLE_OPTIONS'

error_sum = 0

! jules_surface
IF ( l_anthrop_heat_src .AND. anthrop_heat_option /= dukes ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,I0,A)') error_sum,                                &
     ": Only the DUKES (0) anthopogenic heat option is available. " //         &
     "anthrop_heat_option = ", anthrop_heat_option,                            &
     ". Please see LFRic apps ticket #1009 for details."
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

! Defining errors ends here. Now issue FATAL ereport.
IF ( error_sum > 0 ) THEN
  errcode = 10
  WRITE(jules_message,'(A,I0,A)') ": One or more JULES options (", error_sum,  &
     ") have been incorrectly set for use in LFRic apps." //                   &
     NEW_LINE('A') // "Please see job output for details."
  CALL ereport(RoutineName, errcode, jules_message)
END IF


RETURN
END SUBROUTINE check_unavailable_options
END MODULE check_unavailable_options_mod
#endif
