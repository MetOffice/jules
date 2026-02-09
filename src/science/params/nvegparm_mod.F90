! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module holding parameter arrays for non-vegetation surface types.


! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.


MODULE nvegparm

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
 albsnc_nvg(:)                                                                 &
                  ! Snow-covered albedo.
,albsnf_nvgu(:)                                                                &
                  ! Max Snow-free albedo, when scaled to obs
,albsnf_nvg(:)                                                                 &
                  ! Snow-free albedo.
,albsnf_nvgl(:)                                                                &
                  ! Min Snow-free albedo, when scaled to obs
,catch_nvg(:)                                                                  &
                  ! Canopy capacity for water (kg/m2).
,gs_nvg(:)                                                                     &
                  ! Surface conductance (m/s).
,infil_nvg(:)                                                                  &
                  ! Infiltration enhancement factor.
,z0_nvg(:)                                                                     &
                  ! Roughness length (m).
,ch_nvg(:)                                                                     &
                  ! "Canopy" heat capacity (J/K/m2)
,vf_nvg(:)                                                                     &
                  ! Fractional "canopy" coverage
,emis_nvg(:)

LOGICAL, ALLOCATABLE :: l_z0_nvg(:)
                  ! Flag to initialise roughness length from namelist

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='NVEGPARM'

CONTAINS

SUBROUTINE nvegparm_alloc(nnvg)

USE missing_data_mod, ONLY: rmdi

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: nnvg

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='NVEGPARM_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

ALLOCATE( albsnc_nvg(nnvg) )
ALLOCATE( albsnf_nvgu(nnvg) )
ALLOCATE( albsnf_nvg(nnvg) )
ALLOCATE( albsnf_nvgl(nnvg) )
ALLOCATE( catch_nvg(nnvg) )
ALLOCATE( emis_nvg(nnvg) )
ALLOCATE( gs_nvg(nnvg) )
ALLOCATE( infil_nvg(nnvg) )
ALLOCATE( z0_nvg(nnvg) )
ALLOCATE( ch_nvg(nnvg) )
ALLOCATE( vf_nvg(nnvg) )
ALLOCATE( l_z0_nvg(nnvg) )

albsnc_nvg(:)  = rmdi
albsnf_nvgu(:) = rmdi
albsnf_nvg(:)  = rmdi
albsnf_nvgl(:) = rmdi
catch_nvg(:)   = rmdi
emis_nvg(:)    = rmdi
gs_nvg(:)      = rmdi
infil_nvg(:)   = rmdi
z0_nvg(:)      = rmdi
ch_nvg(:)      = rmdi
vf_nvg(:)      = rmdi
! Initialise to .true. i.e. z0_surft will be set to the namelist value z0_nvg
! When .false. z0_surft will be set according to other science options.
l_z0_nvg(:)    = .TRUE.

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE nvegparm_alloc


SUBROUTINE check_jules_nvegparm(nnvg,npft)

USE ereport_mod,      ONLY: ereport
USE check_jules_nml_values_mod, ONLY: check_jules_nml_values

! May want to move this to remove dependency
USE c_z0h_z0m, ONLY: z0h_z0m,  z0h_z0m_classic

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: nnvg, npft

! Work variables
INTEGER :: errorstatus = 0
CHARACTER(LEN=*), PARAMETER :: RoutineName='CHECK_JULES_NVEGPARM'

! In LFRic if options are not required they have a zero size.
IF ( SIZE( albsnc_nvg(:) ) > 0 )                                               &
   CALL check_jules_nml_values ( albsnc_nvg(:), 'albsnc_nvg', nnvg,            &
   0.0, 1.0, RoutineName, errorstatus )
IF ( SIZE( albsnf_nvgu(:) ) > 0 )                                              &
   CALL check_jules_nml_values ( albsnf_nvgu(:), 'albsnf_nvgu', nnvg,          &
   0.0, 1.0, RoutineName, errorstatus )
IF ( SIZE( albsnf_nvg(:) ) > 0 )                                               &
   CALL check_jules_nml_values ( albsnf_nvg(:), 'albsnf_nvg', nnvg,            &
   0.0, 1.0, RoutineName, errorstatus )
IF ( SIZE( albsnf_nvgl(:) ) > 0 )                                              &
   CALL check_jules_nml_values ( albsnf_nvgl(:), 'albsnf_nvgl', nnvg,          &
   0.0, 1.0, RoutineName, errorstatus )
IF ( SIZE( catch_nvg(:) ) > 0 )                                                &
   CALL check_jules_nml_values ( catch_nvg(:), 'catch_nvg', nnvg,              &
   0.0, HUGE(1.0), RoutineName, errorstatus )
IF ( SIZE( emis_nvg(:) ) > 0 )                                                 &
   CALL check_jules_nml_values ( emis_nvg(:), 'emis_nvg', nnvg,                &
   0.0, 1.0, RoutineName, errorstatus )
IF ( SIZE( gs_nvg(:) ) > 0 )                                                   &
   CALL check_jules_nml_values ( gs_nvg(:), 'gs_nvg', nnvg,                    &
   0.0, HUGE(1.0), RoutineName, errorstatus )
IF ( SIZE( infil_nvg(:) ) > 0 )                                                &
   CALL check_jules_nml_values ( infil_nvg(:), 'infil_nvg', nnvg,              &
   0.0, HUGE(1.0), RoutineName, errorstatus )
IF ( SIZE( z0_nvg(:) ) > 0 )                                                   &
   CALL check_jules_nml_values ( z0_nvg(:), 'z0_nvg', nnvg,                    &
   0.0, HUGE(1.0), RoutineName, errorstatus )
IF ( SIZE( ch_nvg(:) ) > 0 )                                                   &
   CALL check_jules_nml_values ( ch_nvg(:), 'ch_nvg', nnvg,                    &
   0.0, HUGE(1.0), RoutineName, errorstatus )
IF ( SIZE( vf_nvg(:) ) > 0 )                                                   &
   CALL check_jules_nml_values ( vf_nvg(:), 'vf_nvg', nnvg,                    &
   0.0, 1.0, RoutineName, errorstatus )
IF ( SIZE( z0h_z0m(npft+1:) ) > 0 )                                            &
   CALL check_jules_nml_values ( z0h_z0m(npft+1:), 'z0hm_nvg', nnvg,           &
   0.0, HUGE(1.0), RoutineName, errorstatus )
IF ( SIZE( z0h_z0m_classic(npft+1:) ) > 0 )                                    &
   CALL check_jules_nml_values ( z0h_z0m_classic(npft+1:),                     &
   'z0hm_classic_nvg', nnvg, 0.0, HUGE(1.0), RoutineName, errorstatus )

IF ( errorstatus > 0 )                                                         &
   CALL ereport(RoutineName, errorstatus,                                      &
   ' Error(s) were found in jules_nvegparm - see job.out for details')

END SUBROUTINE check_jules_nvegparm

END MODULE nvegparm
