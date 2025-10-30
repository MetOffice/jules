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

MODULE wtrac_init_jls_mod

!-------------------------------------------------------------------------------
! Description:
!   Set up water tracer settings for use in JULES.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in Initialisation
!
! Code Description:
!   Language: Fortran 90.
!-------------------------------------------------------------------------------

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='WTRAC_INIT_JLS_MOD'

CONTAINS

SUBROUTINE wtrac_init_jls(wtrac)

USE water_tracers_mod,       ONLY: wtrac_type
USE free_tracers_inputs_mod, ONLY: n_wtrac, h218o_class_wtrac,                 &
                                   hdo_class_wtrac, noniso_class_wtrac,        &
                                   class_wtrac

USE jules_water_tracers_mod, ONLY: l_wtrac_jls, n_wtrac_jls,                   &
                                   standard_ratio_wtrac,                       &
                                   wtrac_lake_fixed_ratio,                     &
                                   h218o_lake_ratio, hdo_lake_ratio

USE land_tile_ids_mod,       ONLY: set_surface_type_wtrac_ids,                 &
                                   set_ml_snow_type_wtrac_ids,                 &
                                   set_seaice_wtrac_ids

USE UM_ParCore,              ONLY: mype

USE umPrintMgr,              ONLY: PrintStatus, PrStatus_Normal,               &
                                   umPrint, umMessage

USE yomhook,  ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER  :: routinename='WTRAC_INIT_JLS'

TYPE(wtrac_type), INTENT(IN) :: wtrac(n_wtrac)

!DrHook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

INTEGER :: i_wt

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Set up water tracer info needed by JULES from UM information
IF (l_wtrac_jls) THEN
  DO i_wt = 1, n_wtrac_jls
    standard_ratio_wtrac(i_wt) = wtrac(i_wt)%standard_ratio

    ! Set lake ratios
    SELECT CASE(class_wtrac(i_wt))
    CASE (h218o_class_wtrac)
      wtrac_lake_fixed_ratio(i_wt) = h218o_lake_ratio
    CASE (hdo_class_wtrac)
      wtrac_lake_fixed_ratio(i_wt) = hdo_lake_ratio
    CASE (noniso_class_wtrac)
      wtrac_lake_fixed_ratio(i_wt) = 1.0
    CASE DEFAULT
      wtrac_lake_fixed_ratio(i_wt) = 1.0
    END SELECT
  END DO
END IF

! Write out settings for PE0
IF (PrintStatus >= PrStatus_Normal .AND. mype == 0) THEN
  IF (l_wtrac_jls) THEN
    WRITE(umMESSAGE,'(A,i0)')                                                  &
       'Water tracers are used in JULES, with n_wtrac_jls =', n_wtrac_jls
    CALL umPrint(umMessage,src=routinename)
    DO i_wt = 1, n_wtrac_jls
      WRITE(umMessage,'(A,I0,A,A7,A,E13.5)')                                   &
       'Water tracer in JULES #',i_wt,                                         &
       ' wt_class:',class_wtrac(i_wt),                                         &
       ' standard_ratio:',standard_ratio_wtrac(i_wt)
      CALL umPrint(umMessage,src=routinename)
    END DO
  ELSE
    WRITE(umMESSAGE,'(A,i0)')                                                  &
       'Water tracers are not being used in JULES'
    CALL umPrint(umMessage,src=routinename)
  END IF
END IF

! Set up pseudo level IDs for output
CALL set_surface_type_wtrac_ids()
CALL set_ml_snow_type_wtrac_ids()
CALL set_seaice_wtrac_ids()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
END SUBROUTINE wtrac_init_jls

END MODULE wtrac_init_jls_mod
#endif
