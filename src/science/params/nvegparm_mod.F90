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
   CALL check_jules_nml_values_real ( albsnc_nvg(:), 'albsnc_nvg', nnvg,       &
   0.0, 1.0, RoutineName, errorstatus )
IF ( SIZE( albsnf_nvgu(:) ) > 0 )                                              &
   CALL check_jules_nml_values_real ( albsnf_nvgu(:), 'albsnf_nvgu', nnvg,     &
   0.0, 1.0, RoutineName, errorstatus )
IF ( SIZE( albsnf_nvg(:) ) > 0 )                                               &
   CALL check_jules_nml_values_real ( albsnf_nvg(:), 'albsnf_nvg', nnvg,       &
   0.0, 1.0, RoutineName, errorstatus )
IF ( SIZE( albsnf_nvgl(:) ) > 0 )                                              &
   CALL check_jules_nml_values_real ( albsnf_nvgl(:), 'albsnf_nvgl', nnvg,     &
   0.0, 1.0, RoutineName, errorstatus )
IF ( SIZE( catch_nvg(:) ) > 0 )                                                &
   CALL check_jules_nml_values_real ( catch_nvg(:), 'catch_nvg', nnvg,         &
   0.0, HUGE(1.0), RoutineName, errorstatus )
IF ( SIZE( emis_nvg(:) ) > 0 )                                                 &
   CALL check_jules_nml_values_real ( emis_nvg(:), 'emis_nvg', nnvg,           &
   0.0, 1.0, RoutineName, errorstatus )
IF ( SIZE( gs_nvg(:) ) > 0 )                                                   &
   CALL check_jules_nml_values_real ( gs_nvg(:), 'gs_nvg', nnvg,               &
   0.0, HUGE(1.0), RoutineName, errorstatus )
IF ( SIZE( infil_nvg(:) ) > 0 )                                                &
   CALL check_jules_nml_values_real ( infil_nvg(:), 'infil_nvg', nnvg,         &
   0.0, HUGE(1.0), RoutineName, errorstatus )
IF ( SIZE( z0_nvg(:) ) > 0 )                                                   &
   CALL check_jules_nml_values_real ( z0_nvg(:), 'z0_nvg', nnvg,               &
   0.0, HUGE(1.0), RoutineName, errorstatus )
IF ( SIZE( ch_nvg(:) ) > 0 )                                                   &
   CALL check_jules_nml_values_real ( ch_nvg(:), 'ch_nvg', nnvg,               &
   0.0, HUGE(1.0), RoutineName, errorstatus )
IF ( SIZE( vf_nvg(:) ) > 0 )                                                   &
   CALL check_jules_nml_values_real ( vf_nvg(:), 'vf_nvg', nnvg,               &
   0.0, 1.0, RoutineName, errorstatus )
IF ( SIZE( z0h_z0m(npft+1:) ) > 0 )                                            &
   CALL check_jules_nml_values_real ( z0h_z0m(npft+1:), 'z0hm_nvg', nnvg,      &
   0.0, HUGE(1.0), RoutineName, errorstatus )
IF ( SIZE( z0h_z0m_classic(npft+1:) ) > 0 )                                    &
   CALL check_jules_nml_values_real ( z0h_z0m_classic(npft+1:),                &
   'z0hm_classic_nvg', nnvg, 0.0, HUGE(1.0), RoutineName, errorstatus )

IF ( errorstatus > 0 )                                                         &
   CALL ereport(RoutineName, errorstatus,                                      &
   ' Error(s) were found in jules_nvegparm - see job.out for details')

END SUBROUTINE check_jules_nvegparm


SUBROUTINE check_jules_nml_values_real ( var, var_name, var_size,  min_value,  &
   max_value, RoutineName, errorstatus )

USE jules_print_mgr, ONLY: jules_print, jules_message
USE jules_surface_types_mod, ONLY: soil, npft
USE missing_data_mod, ONLY: rmdi

IMPLICIT NONE

INTEGER                :: var_size, errorstatus
REAL(KIND=real_jlslsm) :: var(var_size), min_value, max_value
CHARACTER(LEN=*)       :: var_name, RoutineName
LOGICAL                :: soil_chck(var_size)

!-----------------------------------------------------------------------------
! The namelist variables should be initialised to rmdi and will still be
! rmdi if they are attached to science options when not required (UM/JULES).
! Specific checks to ensure that these required variables are not rmdi can be
! found in check_compatible_options.
!-----------------------------------------------------------------------------

IF ( ANY( ABS( var(:) - rmdi ) > EPSILON(1.0) ) ) THEN
  IF ( ANY( var(:) > max_value ) ) errorstatus = 2
  IF ( ANY( var(:) < min_value ) ) THEN
    ! Need to account for albsnf_nvg(soil) = -1 for ancil
    IF ( var_name == 'albsnf_nvg' ) THEN
      soil_chck(:) = ( var(:) < min_value )
      SELECT CASE ( COUNT( soil_chck(:) ) )
      CASE ( 1 )
        IF ( .NOT. ABS ( var(soil-npft) + 1.0 ) < EPSILON(1.0) ) THEN
          errorstatus = 3
        END IF
      CASE DEFAULT
        errorstatus = 3
      END SELECT
    ELSE
      errorstatus = 3
    END IF
  END IF
  IF ( errorstatus > 1 ) THEN
    WRITE(jules_message,*) TRIM(var_name) // ' is out of range: ', var(:)
    CALL jules_print(RoutineName, jules_message)
    ! Reset to fail value after printing message
    errorstatus = 1
  END IF
END IF

END SUBROUTINE check_jules_nml_values_real

END MODULE nvegparm
