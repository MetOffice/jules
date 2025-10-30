! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE jules_print_mgr

#if defined(UM_JULES)
USE umPrintMgr, ONLY:                                                          &
    umPrint,                                                                   &
    PrMin,                                                                     &
    PrNorm,                                                                    &
    PrOper,                                                                    &
    PrDiag,                                                                    &
    PrintStatus,                                                               &
    PrStatus_Diag
#else
USE logging_mod, ONLY: log_info
#endif

IMPLICIT NONE

INTEGER, PARAMETER, PRIVATE  :: max_line_len = 1024
CHARACTER(LEN=max_line_len)  :: jules_message
CHARACTER(LEN=20)            :: jules_format
!$OMP THREADPRIVATE(jules_message)

#if !defined(UM_JULES)
INTEGER, PARAMETER :: PrMin   = 1
INTEGER, PARAMETER :: PrNorm  = 2
INTEGER, PARAMETER :: PrOper  = 3
INTEGER, PARAMETER :: PrDiag  = 4
INTEGER, PARAMETER :: PrintStatus = PrDiag
#endif

! Declare newline character
CHARACTER(LEN=1)   :: newline = NEW_LINE('a')

CONTAINS

SUBROUTINE jules_print(src,line,level)
  ! Either send output to the umPrintMgr subsystem for coupled jobs
  ! or send it to stdout otherwise.

IMPLICIT NONE
CHARACTER(LEN=*), INTENT(IN) :: line
CHARACTER(LEN=*), INTENT(IN) :: src
INTEGER, OPTIONAL            :: level

#if defined(UM_JULES)
CALL umPrint(TRIM(line),src = src,model='jules',level = level)
#else
CALL log_info(TRIM(src),TRIM(line))
#endif

END SUBROUTINE jules_print

END MODULE jules_print_mgr
