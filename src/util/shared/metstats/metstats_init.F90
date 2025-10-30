MODULE metstats_init_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='METSTATS_INIT_MOD'

CONTAINS
SUBROUTINE metstats_init(ainfo)

! < Module imports >
USE metstats_mod,       ONLY: metstats_flag,                                   &
                               metstats_inis,                                  &
                               metstats_prog,                                  &
                               l_metstats

USE theta_field_sizes,  ONLY: t_i_length

USE ancil_info,         ONLY: land_pts
USE model_grid_mod,     ONLY: longitude
USE conversions_mod,    ONLY: rsec_per_day
USE ancil_info,         ONLY: ainfo_type

USE parkind1,           ONLY: jprb, jpim
USE yomhook,            ONLY: lhook, dr_hook

IMPLICIT NONE
!
! Description:
!   Deals with setting up the module variables.
!
!   Only called by JULES-stand alone
!
! Method:
!   Must be called during model initialisation
!   Must be called after fire_init etc to ensure any flag dependencies are set
!   therein
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!

! Arguments
TYPE(ainfo_type), INTENT(IN OUT) :: ainfo

! Local variables
INTEGER                     :: i,j,l !Counters

!Local variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='METSTATS_INIT'

! End of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
! ------------------------------------------
! Set reset values.

IF (l_metstats) THEN
  IF (metstats_flag%temp_max_00h) THEN
    metstats_prog(:)%temp_max_00h%fin = metstats_inis%temp_max_00h
    metstats_prog(:)%temp_max_00h%run = metstats_inis%temp_max_00h
  END IF

  IF (metstats_flag%temp_ave_00h) THEN
    metstats_prog(:)%temp_ave_00h%fin = metstats_inis%temp_ave_00h
    metstats_prog(:)%temp_ave_00h%run = metstats_inis%temp_ave_00h
  END IF

  IF (metstats_flag%temp_ave_nday) THEN
    metstats_prog(:)%temp_ave_nday%fin = metstats_inis%temp_ave_nday
  END IF

  IF (metstats_flag%temp_pnt_12h) THEN
    metstats_prog(:)%temp_pnt_12h%fin = metstats_inis%temp_pnt_12h
  END IF

  IF (metstats_flag%prec_tot_00h) THEN
    metstats_prog(:)%prec_tot_00h%fin = metstats_inis%prec_tot_00h
    metstats_prog(:)%prec_tot_00h%run = metstats_inis%prec_tot_00h
  END IF

  IF (metstats_flag%prec_tot_12h) THEN
    metstats_prog(:)%prec_tot_12h%fin = metstats_inis%prec_tot_12h
    metstats_prog(:)%prec_tot_12h%run = metstats_inis%prec_tot_12h
  END IF

  IF (metstats_flag%rhum_min_00h) THEN
    metstats_prog(:)%rhum_min_00h%fin = metstats_inis%rhum_min_00h
    metstats_prog(:)%rhum_min_00h%run = metstats_inis%rhum_min_00h
  END IF

  IF (metstats_flag%rhum_pnt_12h) THEN
    metstats_prog(:)%rhum_pnt_12h%fin = metstats_inis%rhum_pnt_12h
  END IF

  IF (metstats_flag%dewp_ave_00h) THEN
    metstats_prog(:)%dewp_ave_00h%fin = metstats_inis%dewp_ave_00h
    metstats_prog(:)%dewp_ave_00h%run = metstats_inis%dewp_ave_00h
  END IF

  IF (metstats_flag%wind_ave_00h) THEN
    metstats_prog(:)%wind_ave_00h%fin = metstats_inis%wind_ave_00h
    metstats_prog(:)%wind_ave_00h%run = metstats_inis%wind_ave_00h
  END IF

  IF (metstats_flag%wind_pnt_12h) THEN
    metstats_prog(:)%wind_pnt_12h%fin = metstats_inis%wind_pnt_12h
  END IF

  ! Calculate difference between model time and local time
    !Get local time for land points
  DO l = 1, land_pts
    j = (ainfo%land_index(l) - 1) / t_i_length + 1
    i = ainfo%land_index(l) - (j-1) * t_i_length
    metstats_prog(l)%lon_time_diff = (longitude(i,j) / 360.0) * rsec_per_day
  END DO !Land points
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE metstats_init
END MODULE metstats_init_mod
