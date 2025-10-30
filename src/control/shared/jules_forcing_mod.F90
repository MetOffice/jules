! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE jules_forcing_mod

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
! Module containing all of the driving (atmospheric forcing) variables.
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

TYPE :: forcing_data_type
  ! The forcing variables.
  REAL, ALLOCATABLE :: qw_1_ij(:,:)
                      !  Total water content (Kg/Kg)
  REAL, ALLOCATABLE :: tl_1_ij(:,:)
                      ! Ice/liquid water temperature (k)
  REAL, ALLOCATABLE :: u_0_ij(:,:)
                      ! W'ly component of surface current (m/s)
  REAL, ALLOCATABLE :: v_0_ij(:,:)
                      ! S'ly component of surface current (m/s)
  REAL, ALLOCATABLE :: pstar_ij(:,:)
                      ! Surface pressure (Pascals)
  REAL, ALLOCATABLE :: ls_rain_ij(:,:)
                      ! Large-scale rain (kg/m2/s)
  REAL, ALLOCATABLE :: con_rain_ij(:,:)
                      ! Convective rain (kg/m2/s)
  REAL, ALLOCATABLE :: ls_snow_ij(:,:)
                      ! Large-scale snowfall (kg/m2/s)
  REAL, ALLOCATABLE :: con_snow_ij(:,:)
                      ! Convective snowfall (kg/m2/s)
  REAL, ALLOCATABLE :: sw_down_ij(:,:)
                      ! Surface downward SW radiation (W/m2)
  REAL, ALLOCATABLE :: lw_down_ij(:,:)
                      ! Surface downward LW radiation (W/m2)
  REAL, ALLOCATABLE :: diurnal_temperature_range_ij(:,:)
                      ! diurnal temperature range (K), used when
                      !l_dailydisagg=T

  ! Variables that aid in the calculation of the actual forcing variables
  REAL, ALLOCATABLE :: diff_rad_ij(:,:)       ! Input diffuse radiation (W/m2)
END TYPE forcing_data_type


TYPE :: forcing_type
  ! The forcing variables.
  REAL, POINTER :: qw_1_ij(:,:)
  REAL, POINTER :: tl_1_ij(:,:)
  REAL, POINTER :: u_0_ij(:,:)
  REAL, POINTER :: v_0_ij(:,:)
  !  REAL, POINTER :: u_1_ij(:,:)
  !  REAL, POINTER :: v_1_ij(:,:)
  REAL, POINTER :: pstar_ij(:,:)
  REAL, POINTER :: ls_rain_ij(:,:)
  REAL, POINTER :: con_rain_ij(:,:)
  REAL, POINTER :: ls_snow_ij(:,:)
  REAL, POINTER :: con_snow_ij(:,:)
  REAL, POINTER :: sw_down_ij(:,:)
  REAL, POINTER :: lw_down_ij(:,:)
  REAL, POINTER :: diurnal_temperature_range_ij(:,:)
  ! Variables that aid in the calculation of the actual forcing variables
  REAL, POINTER :: diff_rad_ij(:,:)

END TYPE forcing_type

  ! Leave these two out of the type for now due to conflicts with the UM.
REAL, ALLOCATABLE :: u_1_ij(:,:)
                    ! W'ly wind component (m/s)
REAL, ALLOCATABLE :: v_1_ij(:,:)
                    ! S'ly wind component (m/s)
!-------------------------------------------------------------------------------

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_FORCING_MOD'

CONTAINS

SUBROUTINE forcing_alloc(t_i_length, t_j_length,                               &
                         u_i_length, u_j_length,                               &
                         v_i_length, v_j_length, forcing_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: t_i_length, t_j_length,                                 &
                       u_i_length, u_j_length,                                 &
                       v_i_length, v_j_length

TYPE(forcing_data_type), INTENT(IN OUT) :: forcing_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='FORCING_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

ALLOCATE( forcing_data%qw_1_ij(t_i_length,t_j_length))
ALLOCATE( forcing_data%tl_1_ij(t_i_length,t_j_length))
ALLOCATE( forcing_data%u_0_ij(u_i_length,u_j_length))
ALLOCATE( forcing_data%v_0_ij(v_i_length,v_j_length))
ALLOCATE( u_1_ij(u_i_length,u_j_length))
ALLOCATE( v_1_ij(v_i_length,v_j_length))
ALLOCATE( forcing_data%pstar_ij(t_i_length,t_j_length))
ALLOCATE( forcing_data%ls_rain_ij(t_i_length,t_j_length))
ALLOCATE( forcing_data%con_rain_ij(t_i_length,t_j_length))
ALLOCATE( forcing_data%ls_snow_ij(t_i_length,t_j_length))
ALLOCATE( forcing_data%con_snow_ij(t_i_length,t_j_length))
ALLOCATE( forcing_data%sw_down_ij(t_i_length,t_j_length))
ALLOCATE( forcing_data%lw_down_ij(t_i_length,t_j_length))
ALLOCATE( forcing_data%diff_rad_ij(t_i_length,t_j_length))
ALLOCATE( forcing_data%diurnal_temperature_range_ij(t_i_length,t_j_length))

forcing_data%qw_1_ij(:,:)                       = 0.0
forcing_data%tl_1_ij(:,:)                       = 0.0
forcing_data%u_0_ij(:,:)                        = 0.0
forcing_data%v_0_ij(:,:)                        = 0.0
u_1_ij(:,:)                        = 0.0
v_1_ij(:,:)                        = 0.0
forcing_data%pstar_ij(:,:)                      = 0.0
forcing_data%ls_rain_ij(:,:)                    = 0.0
forcing_data%con_rain_ij(:,:)                   = 0.0
forcing_data%ls_snow_ij(:,:)                    = 0.0
forcing_data%con_snow_ij(:,:)                   = 0.0
forcing_data%sw_down_ij(:,:)                    = 0.0
forcing_data%lw_down_ij(:,:)                    = 0.0
forcing_data%diff_rad_ij(:,:)                   = 0.0
forcing_data%diurnal_temperature_range_ij(:,:)  = 0.0

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE forcing_alloc

!===============================================================================
SUBROUTINE forcing_dealloc(forcing_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

TYPE(forcing_data_type), INTENT(IN OUT) :: forcing_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='FORCING_DEALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DEALLOCATE( forcing_data%qw_1_ij)
DEALLOCATE( forcing_data%tl_1_ij)
DEALLOCATE( forcing_data%u_0_ij)
DEALLOCATE( forcing_data%v_0_ij)
DEALLOCATE( u_1_ij)
DEALLOCATE( v_1_ij)
DEALLOCATE( forcing_data%pstar_ij)
DEALLOCATE( forcing_data%ls_rain_ij)
DEALLOCATE( forcing_data%con_rain_ij)
DEALLOCATE( forcing_data%ls_snow_ij)
DEALLOCATE( forcing_data%con_snow_ij)
DEALLOCATE( forcing_data%sw_down_ij)
DEALLOCATE( forcing_data%lw_down_ij)
DEALLOCATE( forcing_data%diff_rad_ij)
DEALLOCATE( forcing_data%diurnal_temperature_range_ij)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE forcing_dealloc

!===============================================================================
SUBROUTINE forcing_assoc(forcing, forcing_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

TYPE(forcing_type), INTENT(IN OUT) :: forcing
TYPE(forcing_data_type), INTENT(IN OUT), TARGET :: forcing_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='FORCING_ASSOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL forcing_nullify(forcing)

forcing%qw_1_ij => forcing_data%qw_1_ij
forcing%tl_1_ij => forcing_data%tl_1_ij
forcing%u_0_ij => forcing_data%u_0_ij
forcing%v_0_ij => forcing_data%v_0_ij
!forcing%u_1_ij => forcing_data%u_1_ij
!forcing%v_1_ij => forcing_data%v_1_ij
forcing%pstar_ij => forcing_data%pstar_ij
forcing%ls_rain_ij => forcing_data%ls_rain_ij
forcing%con_rain_ij => forcing_data%con_rain_ij
forcing%ls_snow_ij => forcing_data%ls_snow_ij
forcing%con_snow_ij => forcing_data%con_snow_ij
forcing%sw_down_ij => forcing_data%sw_down_ij
forcing%lw_down_ij => forcing_data%lw_down_ij
forcing%diff_rad_ij => forcing_data%diff_rad_ij
forcing%diurnal_temperature_range_ij=>forcing_data%diurnal_temperature_range_ij

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE forcing_assoc

!===============================================================================
SUBROUTINE forcing_nullify(forcing)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

TYPE(forcing_type), INTENT(IN OUT) :: forcing

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='FORCING_NULLIFY'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

NULLIFY( forcing%qw_1_ij)
NULLIFY( forcing%tl_1_ij)
NULLIFY( forcing%u_0_ij)
NULLIFY( forcing%v_0_ij)
!NULLIFY( forcing%u_1_ij)
!NULLIFY( forcing%v_1_ij)
NULLIFY( forcing%pstar_ij)
NULLIFY( forcing%ls_rain_ij)
NULLIFY( forcing%con_rain_ij)
NULLIFY( forcing%ls_snow_ij)
NULLIFY( forcing%con_snow_ij)
NULLIFY( forcing%sw_down_ij)
NULLIFY( forcing%lw_down_ij)
NULLIFY( forcing%diff_rad_ij)
NULLIFY( forcing%diurnal_temperature_range_ij)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE forcing_nullify

END MODULE jules_forcing_mod
