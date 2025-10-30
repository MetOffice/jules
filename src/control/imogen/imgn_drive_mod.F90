#if !defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE imgn_drive_mod

USE missing_data_mod, ONLY: rmdi

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
! Module containing all of the atmospheric forcing variables for IMOGEN.
! this is an alternate way of driving JULES and will never be part of the UM
! Therefore I have tried to keep this routine separate from the UM
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Implementation for field variables:
! Each variable is declared in both the 'data' TYPE and the 'pointer' type.
! Instances of these types are declared at at high level as required
! This is to facilitate advanced memory management features, which are generally
! not visible in the science code.
! Checklist for adding a new variable:
! -add to data_type
! -add to pointer_type
! -add to the allocate routine, passing in any new dimension sizes required
!  by argument (not via USE statement)
! -add to the deallocate routine
! -add to the assoc and nullify routines
!-------------------------------------------------------------------------------

! imogen forcing variables.
TYPE :: imgn_drive_data_type
  ! Driving "control" climatology on grid
  REAL, ALLOCATABLE :: tl1_ij_clim(:,:,:)
  REAL, ALLOCATABLE :: ql1_ij_clim(:,:,:)
  REAL, ALLOCATABLE :: wind_ij_clim(:,:,:)
  REAL, ALLOCATABLE :: lwdown_ij_clim(:,:,:)
  REAL, ALLOCATABLE :: swdown_ij_clim(:,:,:)
  REAL, ALLOCATABLE :: diurnal_tl1_ij_clim(:,:,:)
  REAL, ALLOCATABLE :: precip_ij_clim(:,:,:)
  REAL, ALLOCATABLE :: pstar_ij_clim(:,:,:)
  ! anomalies read in directly
  REAL, ALLOCATABLE :: tl1_ij_anom(:,:,:)
  REAL, ALLOCATABLE :: ql1_ij_anom(:,:,:)
  REAL, ALLOCATABLE :: wind_ij_anom(:,:,:)
  REAL, ALLOCATABLE :: lwdown_ij_anom(:,:,:)
  REAL, ALLOCATABLE :: swdown_ij_anom(:,:,:)
  REAL, ALLOCATABLE :: diurnal_tl1_ij_anom(:,:,:)
  REAL, ALLOCATABLE :: precip_ij_anom(:,:,:)
  REAL, ALLOCATABLE :: pstar_ij_anom(:,:,:)
  ! pattern scaling read in and used to make anomalies
  REAL, ALLOCATABLE :: tl1_ij_patt(:,:,:)
  REAL, ALLOCATABLE :: ql1_ij_patt(:,:,:)
  REAL, ALLOCATABLE :: wind_ij_patt(:,:,:)
  REAL, ALLOCATABLE :: lwdown_ij_patt(:,:,:)
  REAL, ALLOCATABLE :: swdown_ij_patt(:,:,:)
  REAL, ALLOCATABLE :: diurnal_tl1_ij_patt(:,:,:)
  REAL, ALLOCATABLE :: precip_ij_patt(:,:,:)
  REAL, ALLOCATABLE :: pstar_ij_patt(:,:,:)

  ! Create fine temperal resolution year of climatology
  ! to be used by impact studies or DGVMs
  REAL, ALLOCATABLE :: tl1_ij_drive(:,:,:,:,:)
  REAL, ALLOCATABLE :: conv_rain_ij_drive(:,:,:,:,:) !mm/day
  REAL, ALLOCATABLE :: conv_snow_ij_drive(:,:,:,:,:) !mm/day
  REAL, ALLOCATABLE :: ls_rain_ij_drive(:,:,:,:,:) !mm/day
  REAL, ALLOCATABLE :: ls_snow_ij_drive(:,:,:,:,:) !mm/day
  REAL, ALLOCATABLE :: ql1_ij_drive(:,:,:,:,:)
  REAL, ALLOCATABLE :: wind_ij_drive(:,:,:,:,:)
  REAL, ALLOCATABLE :: pstar_ij_drive(:,:,:,:,:)
  REAL, ALLOCATABLE :: swdown_ij_drive(:,:,:,:,:)
  REAL, ALLOCATABLE :: lwdown_ij_drive(:,:,:,:,:)

END TYPE imgn_drive_data_type


TYPE :: imgn_drive_type
  ! The forcing variables.
  REAL, POINTER :: tl1_ij_clim(:,:,:)
  REAL, POINTER :: ql1_ij_clim(:,:,:)
  REAL, POINTER :: wind_ij_clim(:,:,:)
  REAL, POINTER :: lwdown_ij_clim(:,:,:)
  REAL, POINTER :: swdown_ij_clim(:,:,:)
  REAL, POINTER :: diurnal_tl1_ij_clim(:,:,:)
  REAL, POINTER :: precip_ij_clim(:,:,:)
  REAL, POINTER :: pstar_ij_clim(:,:,:)
  REAL, POINTER :: tl1_ij_anom(:,:,:)
  REAL, POINTER :: ql1_ij_anom(:,:,:)
  REAL, POINTER :: wind_ij_anom(:,:,:)
  REAL, POINTER :: lwdown_ij_anom(:,:,:)
  REAL, POINTER :: swdown_ij_anom(:,:,:)
  REAL, POINTER :: diurnal_tl1_ij_anom(:,:,:)
  REAL, POINTER :: precip_ij_anom(:,:,:)
  REAL, POINTER :: pstar_ij_anom(:,:,:)

  REAL, POINTER :: tl1_ij_patt(:,:,:)
  REAL, POINTER :: ql1_ij_patt(:,:,:)
  REAL, POINTER :: wind_ij_patt(:,:,:)
  REAL, POINTER :: lwdown_ij_patt(:,:,:)
  REAL, POINTER :: swdown_ij_patt(:,:,:)
  REAL, POINTER :: diurnal_tl1_ij_patt(:,:,:)
  REAL, POINTER :: precip_ij_patt(:,:,:)
  REAL, POINTER :: pstar_ij_patt(:,:,:)

  REAL, POINTER :: tl1_ij_drive(:,:,:,:,:)
  REAL, POINTER :: conv_rain_ij_drive(:,:,:,:,:)
  REAL, POINTER :: conv_snow_ij_drive(:,:,:,:,:)
  REAL, POINTER :: ls_rain_ij_drive(:,:,:,:,:)
  REAL, POINTER :: ls_snow_ij_drive(:,:,:,:,:)
  REAL, POINTER :: ql1_ij_drive(:,:,:,:,:)
  REAL, POINTER :: wind_ij_drive(:,:,:,:,:)
  REAL, POINTER :: pstar_ij_drive(:,:,:,:,:)
  REAL, POINTER :: swdown_ij_drive(:,:,:,:,:)
  REAL, POINTER :: lwdown_ij_drive(:,:,:,:,:)

END TYPE imgn_drive_type

TYPE(imgn_drive_data_type), TARGET :: imgn_drive_data
TYPE(imgn_drive_type) :: imgn_drive

!-------------------------------------------------------------------------------

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='IMGN_DRIVE_MOD'

#if !defined(RIVERS_ONLY)
CONTAINS

SUBROUTINE imgn_drive_alloc(land_pts, imgn_drive_data)

USE imogen_time,       ONLY: nsdmax, mm, md
USE imogen_run, ONLY: l_daily_metdata_climatol
USE theta_field_sizes, ONLY: t_i_length, t_j_length

USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: land_pts

INTEGER :: mdused

TYPE(imgn_drive_data_type), INTENT(IN OUT) :: imgn_drive_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='IMGN_DRIVE_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF (l_daily_metdata_climatol) THEN
  mdused = md
ELSE
  mdused = 1
END IF

ALLOCATE( imgn_drive_data%tl1_ij_clim(t_i_length,t_j_length,mm*mdused))
ALLOCATE( imgn_drive_data%ql1_ij_clim(t_i_length,t_j_length,mm*mdused))
ALLOCATE( imgn_drive_data%wind_ij_clim(t_i_length,t_j_length,mm*mdused))
ALLOCATE( imgn_drive_data%lwdown_ij_clim(t_i_length,t_j_length,mm*mdused))
ALLOCATE( imgn_drive_data%swdown_ij_clim(t_i_length,t_j_length,mm*mdused))
ALLOCATE( imgn_drive_data%diurnal_tl1_ij_clim(t_i_length,t_j_length,mm*mdused))
ALLOCATE( imgn_drive_data%precip_ij_clim(t_i_length,t_j_length,mm*mdused))
ALLOCATE( imgn_drive_data%pstar_ij_clim(t_i_length,t_j_length,mm*mdused))
ALLOCATE( imgn_drive_data%tl1_ij_anom(t_i_length,t_j_length,mm))
ALLOCATE( imgn_drive_data%ql1_ij_anom(t_i_length,t_j_length,mm))
ALLOCATE( imgn_drive_data%wind_ij_anom(t_i_length,t_j_length,mm))
ALLOCATE( imgn_drive_data%lwdown_ij_anom(t_i_length,t_j_length,mm))
ALLOCATE( imgn_drive_data%swdown_ij_anom(t_i_length,t_j_length,mm))
ALLOCATE( imgn_drive_data%diurnal_tl1_ij_anom(t_i_length,t_j_length,mm))
ALLOCATE( imgn_drive_data%precip_ij_anom(t_i_length,t_j_length,mm))
ALLOCATE( imgn_drive_data%pstar_ij_anom(t_i_length,t_j_length,mm))
ALLOCATE( imgn_drive_data%tl1_ij_patt(t_i_length,t_j_length,mm))
ALLOCATE( imgn_drive_data%ql1_ij_patt(t_i_length,t_j_length,mm))
ALLOCATE( imgn_drive_data%wind_ij_patt(t_i_length,t_j_length,mm))
ALLOCATE( imgn_drive_data%lwdown_ij_patt(t_i_length,t_j_length,mm))
ALLOCATE( imgn_drive_data%swdown_ij_patt(t_i_length,t_j_length,mm))
ALLOCATE( imgn_drive_data%diurnal_tl1_ij_patt(t_i_length,t_j_length,mm))
ALLOCATE( imgn_drive_data%precip_ij_patt(t_i_length,t_j_length,mm))
ALLOCATE( imgn_drive_data%pstar_ij_patt(t_i_length,t_j_length,mm))
ALLOCATE( imgn_drive_data%tl1_ij_drive(t_i_length,t_j_length,mm,md,nsdmax))
ALLOCATE( imgn_drive_data%conv_rain_ij_drive(t_i_length,t_j_length,mm,md,nsdmax))
ALLOCATE( imgn_drive_data%conv_snow_ij_drive(t_i_length,t_j_length,mm,md,nsdmax))
ALLOCATE( imgn_drive_data%ls_rain_ij_drive(t_i_length,t_j_length,mm,md,nsdmax))
ALLOCATE( imgn_drive_data%ls_snow_ij_drive(t_i_length,t_j_length,mm,md,nsdmax))
ALLOCATE( imgn_drive_data%ql1_ij_drive(t_i_length,t_j_length,mm,md,nsdmax))
ALLOCATE( imgn_drive_data%wind_ij_drive(t_i_length,t_j_length,mm,md,nsdmax))
ALLOCATE( imgn_drive_data%pstar_ij_drive(t_i_length,t_j_length,mm,md,nsdmax))
ALLOCATE( imgn_drive_data%swdown_ij_drive(t_i_length,t_j_length,mm,md,nsdmax))
ALLOCATE( imgn_drive_data%lwdown_ij_drive(t_i_length,t_j_length,mm,md,nsdmax))

imgn_drive_data%tl1_ij_clim(:,:,:) = rmdi
imgn_drive_data%ql1_ij_clim(:,:,:) = rmdi
imgn_drive_data%wind_ij_clim(:,:,:) = rmdi
imgn_drive_data%lwdown_ij_clim(:,:,:) = rmdi
imgn_drive_data%swdown_ij_clim(:,:,:) = rmdi
imgn_drive_data%diurnal_tl1_ij_clim(:,:,:) = rmdi
imgn_drive_data%precip_ij_clim(:,:,:) = rmdi
imgn_drive_data%pstar_ij_clim(:,:,:) = rmdi
imgn_drive_data%tl1_ij_anom(:,:,:) = 0.0
imgn_drive_data%ql1_ij_anom(:,:,:) = 0.0
imgn_drive_data%wind_ij_anom(:,:,:) = 0.0
imgn_drive_data%lwdown_ij_anom(:,:,:) =0.0
imgn_drive_data%swdown_ij_anom(:,:,:) = 0.0
imgn_drive_data%diurnal_tl1_ij_anom(:,:,:) = 0.0
imgn_drive_data%precip_ij_anom(:,:,:) = 0.0
imgn_drive_data%pstar_ij_anom(:,:,:) = 0.0
imgn_drive_data%tl1_ij_patt(:,:,:) = 0.0
imgn_drive_data%ql1_ij_patt(:,:,:) = 0.0
imgn_drive_data%wind_ij_patt(:,:,:) = 0.0
imgn_drive_data%lwdown_ij_patt(:,:,:) =0.0
imgn_drive_data%swdown_ij_patt(:,:,:) = 0.0
imgn_drive_data%diurnal_tl1_ij_patt(:,:,:) = 0.0
imgn_drive_data%precip_ij_patt(:,:,:) = 0.0
imgn_drive_data%pstar_ij_patt(:,:,:) = 0.0
imgn_drive_data%tl1_ij_drive(:,:,:,:,:) = rmdi
imgn_drive_data%conv_rain_ij_drive(:,:,:,:,:) = rmdi
imgn_drive_data%conv_snow_ij_drive(:,:,:,:,:) = rmdi
imgn_drive_data%ls_rain_ij_drive(:,:,:,:,:) = rmdi
imgn_drive_data%ls_snow_ij_drive(:,:,:,:,:) = rmdi
imgn_drive_data%ql1_ij_drive(:,:,:,:,:) = rmdi
imgn_drive_data%wind_ij_drive(:,:,:,:,:) = rmdi
imgn_drive_data%pstar_ij_drive(:,:,:,:,:) = rmdi
imgn_drive_data%swdown_ij_drive(:,:,:,:,:) = rmdi
imgn_drive_data%lwdown_ij_drive(:,:,:,:,:) = rmdi

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE imgn_drive_alloc

!===============================================================================
SUBROUTINE imgn_drive_dealloc(imgn_drive_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

TYPE(imgn_drive_data_type), INTENT(IN OUT) :: imgn_drive_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='IMGN_DRIVE_DEALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DEALLOCATE( imgn_drive_data%tl1_ij_clim)
DEALLOCATE( imgn_drive_data%ql1_ij_clim)
DEALLOCATE( imgn_drive_data%wind_ij_clim)
DEALLOCATE( imgn_drive_data%lwdown_ij_clim)
DEALLOCATE( imgn_drive_data%swdown_ij_clim)
DEALLOCATE( imgn_drive_data%diurnal_tl1_ij_clim)
DEALLOCATE( imgn_drive_data%precip_ij_clim)
DEALLOCATE( imgn_drive_data%pstar_ij_clim)
DEALLOCATE( imgn_drive_data%tl1_ij_anom)
DEALLOCATE( imgn_drive_data%ql1_ij_anom)
DEALLOCATE( imgn_drive_data%wind_ij_anom)
DEALLOCATE( imgn_drive_data%lwdown_ij_anom)
DEALLOCATE( imgn_drive_data%swdown_ij_anom)
DEALLOCATE( imgn_drive_data%diurnal_tl1_ij_anom)
DEALLOCATE( imgn_drive_data%precip_ij_anom)
DEALLOCATE( imgn_drive_data%pstar_ij_anom)
DEALLOCATE( imgn_drive_data%tl1_ij_patt)
DEALLOCATE( imgn_drive_data%ql1_ij_patt)
DEALLOCATE( imgn_drive_data%wind_ij_patt)
DEALLOCATE( imgn_drive_data%lwdown_ij_patt)
DEALLOCATE( imgn_drive_data%swdown_ij_patt)
DEALLOCATE( imgn_drive_data%diurnal_tl1_ij_patt)
DEALLOCATE( imgn_drive_data%precip_ij_patt)
DEALLOCATE( imgn_drive_data%pstar_ij_patt)
DEALLOCATE( imgn_drive_data%tl1_ij_drive)
DEALLOCATE( imgn_drive_data%conv_rain_ij_drive)
DEALLOCATE( imgn_drive_data%conv_snow_ij_drive)
DEALLOCATE( imgn_drive_data%ls_rain_ij_drive)
DEALLOCATE( imgn_drive_data%ls_snow_ij_drive)
DEALLOCATE( imgn_drive_data%ql1_ij_drive)
DEALLOCATE( imgn_drive_data%wind_ij_drive)
DEALLOCATE( imgn_drive_data%pstar_ij_drive)
DEALLOCATE( imgn_drive_data%swdown_ij_drive)
DEALLOCATE( imgn_drive_data%lwdown_ij_drive)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE imgn_drive_dealloc

!===============================================================================
SUBROUTINE imgn_drive_assoc(imgn_drive, imgn_drive_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

TYPE(imgn_drive_type), INTENT(IN OUT) :: imgn_drive
TYPE(imgn_drive_data_type), INTENT(IN OUT), TARGET :: imgn_drive_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='IMGN_DRIVE_ASSOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL imgn_drive_nullify(imgn_drive)

imgn_drive%tl1_ij_clim => imgn_drive_data%tl1_ij_clim
imgn_drive%ql1_ij_clim => imgn_drive_data%ql1_ij_clim
imgn_drive%wind_ij_clim => imgn_drive_data%wind_ij_clim
imgn_drive%lwdown_ij_clim => imgn_drive_data%lwdown_ij_clim
imgn_drive%swdown_ij_clim => imgn_drive_data%swdown_ij_clim
imgn_drive%diurnal_tl1_ij_clim => imgn_drive_data%diurnal_tl1_ij_clim
imgn_drive%precip_ij_clim => imgn_drive_data%precip_ij_clim
imgn_drive%pstar_ij_clim => imgn_drive_data%pstar_ij_clim
imgn_drive%tl1_ij_anom => imgn_drive_data%tl1_ij_anom
imgn_drive%ql1_ij_anom => imgn_drive_data%ql1_ij_anom
imgn_drive%wind_ij_anom => imgn_drive_data%wind_ij_anom
imgn_drive%lwdown_ij_anom => imgn_drive_data%lwdown_ij_anom
imgn_drive%swdown_ij_anom => imgn_drive_data%swdown_ij_anom
imgn_drive%diurnal_tl1_ij_anom =>imgn_drive_data%diurnal_tl1_ij_anom
imgn_drive%precip_ij_anom => imgn_drive_data%precip_ij_anom
imgn_drive%pstar_ij_anom => imgn_drive_data%pstar_ij_anom
imgn_drive%tl1_ij_patt => imgn_drive_data%tl1_ij_patt
imgn_drive%ql1_ij_patt => imgn_drive_data%ql1_ij_patt
imgn_drive%wind_ij_patt => imgn_drive_data%wind_ij_patt
imgn_drive%lwdown_ij_patt => imgn_drive_data%lwdown_ij_patt
imgn_drive%swdown_ij_patt => imgn_drive_data%swdown_ij_patt
imgn_drive%diurnal_tl1_ij_patt =>imgn_drive_data%diurnal_tl1_ij_patt
imgn_drive%precip_ij_patt => imgn_drive_data%precip_ij_patt
imgn_drive%pstar_ij_patt => imgn_drive_data%pstar_ij_patt
imgn_drive%tl1_ij_drive => imgn_drive_data%tl1_ij_drive
imgn_drive%conv_rain_ij_drive => imgn_drive_data%conv_rain_ij_drive
imgn_drive%conv_snow_ij_drive => imgn_drive_data%conv_snow_ij_drive
imgn_drive%ls_rain_ij_drive => imgn_drive_data%ls_rain_ij_drive
imgn_drive%ls_snow_ij_drive => imgn_drive_data%ls_snow_ij_drive
imgn_drive%ql1_ij_drive => imgn_drive_data%ql1_ij_drive
imgn_drive%wind_ij_drive => imgn_drive_data%wind_ij_drive
imgn_drive%pstar_ij_drive => imgn_drive_data%pstar_ij_drive
imgn_drive%swdown_ij_drive => imgn_drive_data%swdown_ij_drive
imgn_drive%lwdown_ij_drive => imgn_drive_data%lwdown_ij_drive

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE imgn_drive_assoc

!===============================================================================
SUBROUTINE imgn_drive_nullify(imgn_drive)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

TYPE(imgn_drive_type), INTENT(IN OUT) :: imgn_drive

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='IMGN_DRIVE_NULLIFY'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

NULLIFY( imgn_drive%tl1_ij_clim)
NULLIFY( imgn_drive%ql1_ij_clim)
NULLIFY( imgn_drive%wind_ij_clim)
NULLIFY( imgn_drive%lwdown_ij_clim)
NULLIFY( imgn_drive%swdown_ij_clim)
NULLIFY( imgn_drive%diurnal_tl1_ij_clim)
NULLIFY( imgn_drive%precip_ij_clim)
NULLIFY( imgn_drive%pstar_ij_clim)
NULLIFY( imgn_drive%tl1_ij_anom)
NULLIFY( imgn_drive%ql1_ij_anom)
NULLIFY( imgn_drive%wind_ij_anom)
NULLIFY( imgn_drive%lwdown_ij_anom)
NULLIFY( imgn_drive%swdown_ij_anom)
NULLIFY( imgn_drive%diurnal_tl1_ij_anom)
NULLIFY( imgn_drive%precip_ij_anom)
NULLIFY( imgn_drive%pstar_ij_anom)
NULLIFY( imgn_drive%tl1_ij_patt)
NULLIFY( imgn_drive%ql1_ij_patt)
NULLIFY( imgn_drive%wind_ij_patt)
NULLIFY( imgn_drive%lwdown_ij_patt)
NULLIFY( imgn_drive%swdown_ij_patt)
NULLIFY( imgn_drive%diurnal_tl1_ij_patt)
NULLIFY( imgn_drive%precip_ij_patt)
NULLIFY( imgn_drive%pstar_ij_patt)
NULLIFY( imgn_drive%tl1_ij_drive)
NULLIFY( imgn_drive%conv_rain_ij_drive)
NULLIFY( imgn_drive%conv_snow_ij_drive)
NULLIFY( imgn_drive%ls_rain_ij_drive)
NULLIFY( imgn_drive%ls_snow_ij_drive)
NULLIFY( imgn_drive%ql1_ij_drive)
NULLIFY( imgn_drive%wind_ij_drive)
NULLIFY( imgn_drive%pstar_ij_drive)
NULLIFY( imgn_drive%swdown_ij_drive)
NULLIFY( imgn_drive%lwdown_ij_drive)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE imgn_drive_nullify

#endif

END MODULE imgn_drive_mod
#endif
