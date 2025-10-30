! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module holds parameters for each crop plant functional type

MODULE cropparm

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Module containing parameters for each crop PFT
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Public module variables
REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
  t_bse(:),                                                                    &
      ! Base temp for development (K).
  t_opt(:),                                                                    &
      ! Optimum temp for development (K).
  t_max(:),                                                                    &
      ! Maximum temp for development (K).
  tt_emr(:),                                                                   &
      ! Thermal time for emergence (degree days).
  crit_pp(:),                                                                  &
      ! Critical daylength for photoperiod sensitivity (hours).
  pp_sens(:),                                                                  &
      ! Sensitivity to daylength (hours-1).
  rt_dir(:),                                                                   &
      ! Alpha for root growth direction
  alpha1(:),                                                                   &
  alpha2(:),                                                                   &
  alpha3(:),                                                                   &
  beta1(:),                                                                    &
  beta2(:),                                                                    &
  beta3(:),                                                                    &
      ! Coefficients to calculate partition coefficients
  r_gamma(:),                                                                  &
  delta(:),                                                                    &
      ! Coefficients for sla calculation (m2 kg-1).
  remob(:),                                                                    &
      ! Remobilisation factor
  cfrac_s(:),                                                                  &
      ! Carbon fraction of stems
  cfrac_r(:),                                                                  &
      ! Carbon fraction of roots
  cfrac_l(:),                                                                  &
      ! Carbon fraction of leaves
  allo1(:),                                                                    &
  allo2(:),                                                                    &
      ! Allometric coefficients for stemc <-> canht
  mu(:),                                                                       &
      ! Coefficient for senescence calculation
  nu(:),                                                                       &
      ! Coefficient for senescence calculation
  yield_frac(:),                                                               &
      ! Fraction of the harv carbon pool converted to yield carbon
  initial_carbon(:),                                                           &
      ! Carbon in crops at DVI=initial_c_dvi (kgC/m2).
  initial_c_dvi(:),                                                            &
      ! DVI at which total crop carbon is set to initial_carbon. Should be
      ! emergence (0.0) or shortly after.
  sen_dvi(:),                                                                  &
      ! DVI at which leaf senescence begins.
  t_mort(:)
      ! Soil temperature (second level) at which to kill crop if dvi>1 (K).

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='CROPPARM'

CONTAINS

SUBROUTINE cropparm_alloc(ncpft,l_crop)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: ncpft

LOGICAL, INTENT(IN) :: l_crop

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CROPPARM_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

#if !defined(UM_JULES)

IF ( l_crop ) THEN
  ALLOCATE( t_bse(ncpft))
  ALLOCATE( t_opt(ncpft))
  ALLOCATE( t_max(ncpft))
  ALLOCATE( tt_emr(ncpft))
  ALLOCATE( crit_pp(ncpft))
  ALLOCATE( pp_sens(ncpft))
  ALLOCATE( rt_dir(ncpft))
  ALLOCATE( alpha1(ncpft))
  ALLOCATE( alpha2(ncpft))
  ALLOCATE( alpha3(ncpft))
  ALLOCATE( beta1(ncpft))
  ALLOCATE( beta2(ncpft))
  ALLOCATE( beta3(ncpft))
  ALLOCATE( r_gamma(ncpft))
  ALLOCATE( delta(ncpft))
  ALLOCATE( remob(ncpft))
  ALLOCATE( cfrac_s(ncpft))
  ALLOCATE( cfrac_r(ncpft))
  ALLOCATE( cfrac_l(ncpft))
  ALLOCATE( allo1(ncpft))
  ALLOCATE( allo2(ncpft))
  ALLOCATE( mu(ncpft))
  ALLOCATE( nu(ncpft))
  ALLOCATE( yield_frac(ncpft))
  ALLOCATE( initial_carbon(ncpft))
  ALLOCATE( initial_c_dvi(ncpft))
  ALLOCATE( sen_dvi(ncpft))
  ALLOCATE( t_mort(ncpft))

  t_bse(:)   = 0.0
  t_opt(:)   = 0.0
  t_max(:)   = 0.0
  tt_emr(:)  = 0.0
  crit_pp(:) = 0.0
  pp_sens(:) = 0.0
  rt_dir(:)  = 0.0
  alpha1(:)  = 0.0
  alpha2(:) = 0.0
  alpha3(:) = 0.0
  beta1(:)  = 0.0
  beta2(:)  = 0.0
  beta3(:)  = 0.0
  r_gamma(:) = 0.0
  delta(:)   = 0.0
  remob(:)   = 0.0
  cfrac_s(:) = 0.0
  cfrac_r(:) = 0.0
  cfrac_l(:) = 0.0
  allo1(:)   = 0.0
  allo2(:)   = 0.0
  mu(:) = 0.0
  nu(:) = 0.0
  yield_frac(:)     = 0.0
  initial_carbon(:) = 0.0
  initial_c_dvi(:)  = 0.0
  sen_dvi(:)        = 0.0
  t_mort(:)         = 0.0
END IF

#endif

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE cropparm_alloc

END MODULE cropparm
