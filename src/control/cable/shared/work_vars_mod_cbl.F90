!-----------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
MODULE work_vars_mod_cbl

!------------------------------------------------------------------------------
! Description:
!   Declares/(de)allocates/assigns CABLE "working" variables
!   These are vars reqested to be kept across the surf_couple* pathways
!   and/or timesteps. Some will be elevated to prognostics, others will be
!   removed via rewriting of the algorithm where requested
!
! This MODULE is USEd by:
!      cable_fields_mod.F90,
!      surf_couple_explicit_mod.F90,
!      surf_couple_extra_mod.F90,
!      surf_couple_implicit_mod.F90,
!      control.F90,
!      init_cable_working_vars.F90,
!      init.F90
!
! This MODULE contains 4 public Subroutines:
!      alloc_work_vars_cbl,
!      dealloc_work_vars_cbl,
!      assoc_work_vars_cbl,
!      nullify_assoc_work_vars_cbl
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in CABLE SCIENCE
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!------------------------------------------------------------------------------

IMPLICIT NONE

PUBLIC :: alloc_work_vars_cbl
PUBLIC :: assoc_work_vars_cbl
PUBLIC :: work_vars_data_type
PUBLIC :: work_vars_type
PRIVATE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='WORK_VARS_MOD_CBL'

TYPE :: work_vars_data_type

  !fields returned @ hydrology (surf_couple_extra) level of JULES
  !computed in CABLE @ implicit (surf_couple_implicit) level
  REAL, ALLOCATABLE, PUBLIC :: snow_tile(:,:)
  REAL, ALLOCATABLE, PUBLIC :: lying_snow(:)
  REAL, ALLOCATABLE, PUBLIC :: surf_roff(:)
  REAL, ALLOCATABLE, PUBLIC :: sub_surf_roff(:)
  REAL, ALLOCATABLE, PUBLIC :: tot_tfall(:)

END TYPE work_vars_data_type

TYPE :: work_vars_type

  !fields returned @ hydrology (surf_couple_extra) level of JULES
  !computed in CABLE @ implicit (surf_couple_implicit) level
  REAL, POINTER, PUBLIC :: snow_tile(:,:)
  REAL, POINTER, PUBLIC :: lying_snow(:)
  REAL, POINTER, PUBLIC :: surf_roff(:)
  REAL, POINTER, PUBLIC :: sub_surf_roff(:)
  REAL, POINTER, PUBLIC :: tot_tfall(:)

END TYPE work_vars_type

CONTAINS

!===============================================================================
SUBROUTINE alloc_work_vars_cbl(land_pts, nsurft, sm_levels, lsm_id, cable,     &
                                work_data_cbl )

! Description:
!   Allocate the CABLE work data variables in the derived type structure

!Common Non-science modules
USE parkind1,                 ONLY: jprb, jpim
USE yomhook,                  ONLY: lhook, dr_hook
USE jules_print_mgr,          ONLY: jules_message, jules_print, PrNorm
USE ereport_mod,              ONLY: ereport

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: land_pts, nsurft,sm_levels
INTEGER, INTENT(IN) :: lsm_id, cable

TYPE(work_vars_data_type), INTENT(IN OUT) :: work_data_cbl

!-----------------------------------------------------------------------
! Local variables for error trapping
!-----------------------------------------------------------------------
! CABLE constant, to be replaced once compilation with CABLE
! library works. ccc25
INTEGER, PARAMETER :: nsnl = 3 !# of snow layers

INTEGER ::                                                                     &
  ERROR        = 0,                                                            &
                       ! Variable for trapping the error from each
                       ! individual call to allocate
  error_sum    = 0,                                                            &

                       ! Variable to track the sum of all errors
                       ! resulting from calls to allocate. Hence we
                       ! know that everything was successful if and
                       ! only if this is zero at the end
  errcode
                       ! Variable to use in error report

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='ALLOC_WORK_VARS_CBL'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! CABLE vars to be initialized
IF ( lsm_id == cable ) THEN

  ALLOCATE( work_data_cbl%snow_tile(land_pts, nsurft), STAT = ERROR )
  ALLOCATE( work_data_cbl%lying_snow(land_pts),        STAT = ERROR )
  ALLOCATE( work_data_cbl%surf_roff(land_pts),         STAT = ERROR )
  ALLOCATE( work_data_cbl%tot_tfall(land_pts),         STAT = ERROR )
  ALLOCATE( work_data_cbl%sub_surf_roff(land_pts),     STAT = ERROR )
  error_sum = error_sum + ERROR

  !-----------------------------------------------------------------------
  ! Write out an error if there was one
  !-----------------------------------------------------------------------
  IF ( error_sum /= 0 )                                                        &
  CALL ereport(RoutineName, errcode,                                           &
              "Error allocating CABLE work type")

ELSE

  ALLOCATE( work_data_cbl%snow_tile(1,1), STAT = ERROR )
  ALLOCATE( work_data_cbl%lying_snow(1),  STAT = ERROR )
  ALLOCATE( work_data_cbl%surf_roff(1),   STAT = ERROR )
  ALLOCATE( work_data_cbl%tot_tfall(1),   STAT = ERROR )
  ALLOCATE( work_data_cbl%sub_surf_roff(1), STAT = ERROR )
  error_sum = error_sum + ERROR

  !-----------------------------------------------------------------------
  ! Write out an error if there was one
  !-----------------------------------------------------------------------
  IF ( error_sum /= 0 )                                                        &
  CALL ereport(RoutineName, errcode,                                           &
              "Error allocating CABLE work type")

END IF

work_data_cbl% snow_tile      (:,:)       = 0.0
work_data_cbl% lying_snow     (:)         = 0.0
work_data_cbl% surf_roff      (:)         = 0.0
work_data_cbl% sub_surf_roff  (:)         = 0.0
work_data_cbl% tot_tfall      (:)         = 0.0

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE alloc_work_vars_cbl

!===============================================================================
SUBROUTINE dealloc_work_vars_cbl(work_data_cbl )

! Description:
!   Deallocate the CABLE work data variables in the derived type structure

!Common Non-science modules
USE parkind1,                 ONLY: jprb, jpim
USE yomhook,                  ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
TYPE(work_vars_data_type), INTENT(IN OUT) :: work_data_cbl

!-----------------------------------------------------------------------
! Local variables
!-----------------------------------------------------------------------
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='DEALLOC_WORK_VARS_CBL'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! CABLE vars to be initialized via JULES i/o
DEALLOCATE( work_data_cbl %snow_tile     )
DEALLOCATE( work_data_cbl %lying_snow    )
DEALLOCATE( work_data_cbl %surf_roff     )
DEALLOCATE( work_data_cbl %sub_surf_roff )
DEALLOCATE( work_data_cbl %tot_tfall     )

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE dealloc_work_vars_cbl

!===============================================================================
SUBROUTINE assoc_work_vars_cbl(work_cbl, work_data_cbl )

! Description:
!   Associate the CABLE work pointers in the derived type structure

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
TYPE(work_vars_type),      INTENT(IN OUT)         :: work_cbl
TYPE(work_vars_data_type), INTENT(IN OUT), TARGET :: work_data_cbl

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='ASSOC_WORK_VARS_CBL'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL nullify_assoc_work_vars_cbl(work_cbl)

work_cbl% snow_tile     => work_data_cbl% snow_tile
work_cbl% lying_snow    => work_data_cbl% lying_snow
work_cbl% surf_roff     => work_data_cbl% surf_roff
work_cbl% sub_surf_roff => work_data_cbl% sub_surf_roff
work_cbl% tot_tfall     => work_data_cbl% tot_tfall

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE assoc_work_vars_cbl

!===============================================================================
SUBROUTINE nullify_assoc_work_vars_cbl(work_cbl)

! Description:
!   Nullify the CABLE work pointers in the derived type structure

  !No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
TYPE(work_vars_type), INTENT(IN OUT) :: work_cbl

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='NULLIFY_ASSOC_WORK_VARS_CBL'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

NULLIFY( work_cbl% snow_tile      )
NULLIFY( work_cbl% lying_snow     )
NULLIFY( work_cbl% surf_roff      )
NULLIFY( work_cbl% sub_surf_roff  )
NULLIFY( work_cbl% tot_tfall      )

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE nullify_assoc_work_vars_cbl

END MODULE work_vars_mod_cbl

