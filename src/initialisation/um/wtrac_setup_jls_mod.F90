#if defined(UM_JULES)
!******************************COPYRIGHT****************************************
! (c) British Antarctic Survey, 2024.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT****************************************

MODULE wtrac_setup_jls_mod

!-------------------------------------------------------------------------------
! Description:
!   Set up the control logical (l_wtrac_jls) which determines whether water
!   tracers are passed through JULES or not. Also, set the number of water
!   tracers (n_wtrac_jls) which are to be used in JULES.
!   (Note, some types of water tracers do not need to be passed through
!    JULES.)
!
!   Which type of water tracer passes through JULES?:
!      Isotopes     - Always
!      Non-isotopic - Either they all do or they all don't (set by user)
!      Normal       - Yes if there are other types of water tracers passing
!                     through JULES, otherwise no.
!
! This routine is called from the reconfiguration and UM
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in Initialisation
!
! Code Description:
!   Language: Fortran 90.
!-------------------------------------------------------------------------------

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='WTRAC_SETUP_JLS_MOD'

CONTAINS

SUBROUTINE wtrac_setup_jls()

USE free_tracers_inputs_mod, ONLY: l_h218o_wtrac, l_hdo_wtrac,n_norm_wtrac,    &
                                   n_noniso_wtrac_jls, n_wtrac,                &
                                   i_wtrac_start_non_jls
USE jules_water_tracers_mod, ONLY: l_wtrac_jls, n_wtrac_jls

USE UM_ParCore,              ONLY: mype

USE umPrintMgr,              ONLY: PrintStatus, PrStatus_Normal,               &
                                   umPrint, umMessage

USE yomhook,  ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER  :: routinename='WTRAC_SETUP_JLS'

!DrHook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

INTEGER :: n_wtrac_jls_sum

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Starting point: There are no water tracers going through JULES
l_wtrac_jls     = .FALSE.
n_wtrac_jls_sum = 0

! Does normal water water go through JULES?
IF (l_h218o_wtrac .OR. l_hdo_wtrac .OR. n_noniso_wtrac_jls > 0) THEN
  ! Yes
  l_wtrac_jls = .TRUE.
  n_wtrac_jls_sum = n_norm_wtrac
END IF

! Are there isotopes?
IF (l_h218o_wtrac .OR. l_hdo_wtrac) THEN

  IF (l_h218o_wtrac) n_wtrac_jls_sum = n_wtrac_jls_sum + 1
  IF (l_hdo_wtrac)   n_wtrac_jls_sum = n_wtrac_jls_sum + 1

END IF

! Are there non-iso water tracers going through JULES?
IF (n_noniso_wtrac_jls > 0) THEN

  n_wtrac_jls_sum = n_wtrac_jls_sum + n_noniso_wtrac_jls

END IF

IF (l_wtrac_jls) THEN
  n_wtrac_jls = n_wtrac_jls_sum
  ! ELSE
    ! n_wtrac_jls stays set to default value of 1 which is
    ! required for indexing as arrays have size n_wtrac_jls
END IF

! Set up UM parameter which is the index of the first water tracer which does
! not go through JULES
IF (.NOT. l_wtrac_jls) THEN
  ! Set to the first normal water tracer
  i_wtrac_start_non_jls = 1
ELSE IF (n_wtrac_jls /= n_wtrac) THEN
  ! Set to first non-isotopic water tracer which doesn't go through JULES
  i_wtrac_start_non_jls = n_wtrac_jls + 1
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
END SUBROUTINE wtrac_setup_jls

END MODULE wtrac_setup_jls_mod
#endif
