#if !defined(UM_JULES)
! Module containing variables for a day's worth of disaggregated precip.

MODULE disaggregated_precip

IMPLICIT NONE

!-------------------------------------------------------------------------------

REAL, ALLOCATABLE ::                                                           &
  ls_rain_disagg(:,:,:),                                                       &
                      ! Large-scale rain (kg/m2/s)
  con_rain_disagg(:,:,:),                                                      &
                      ! Convective rain (kg/m2/s)
  ls_snow_disagg(:,:,:),                                                       &
                      ! Large-scale snowfall (kg/m2/s)
  con_snow_disagg(:,:,:)
                      ! Convective snowfall (kg/m2/s)

!-------------------------------------------------------------------------------

END MODULE disaggregated_precip
#endif
