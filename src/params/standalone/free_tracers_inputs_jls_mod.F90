#if !defined(UM_JULES)
! *****************************COPYRIGHT********************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT********************************
!
!  Data module for water tracer settings used in standalone JULES ONLY!
!  In UMJULES, these parameters are set in the UM module
!  free_tracers_inputs_mod.F90.
!
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
! This file belongs in section: Water_Tracers
!

MODULE free_tracers_inputs_mod

IMPLICIT NONE

! Maximum number of water tracers
INTEGER, PARAMETER :: max_wtrac = 20

! Water tracer classes
CHARACTER(LEN=7), PARAMETER :: norm_class_wtrac   = 'norm   '
CHARACTER(LEN=7), PARAMETER :: noniso_class_wtrac = 'non_iso'
CHARACTER(LEN=7), PARAMETER :: h218o_class_wtrac  = 'h218o  '
CHARACTER(LEN=7), PARAMETER :: hdo_class_wtrac    = 'hdo    '

CHARACTER(LEN=7)            :: class_wtrac(max_wtrac) = 'not_set'

END MODULE free_tracers_inputs_mod
#endif
