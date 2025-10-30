#if defined(UM_JULES)
!
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE lake_evap_global_avg_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!---------------------------------------------------------------------------
! Description:
!    Calculate the global average lake evaporation to be taken from soil
!    moisture to ensure moisture conservation.
!    The same routine is used for both normal water and water tracers.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: River Routing
!
! Code Description:
!   Language: FORTRAN 90
!   This code is written to JULES coding standards v1.
!----------------------------------------------------------------------------

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='LAKE_EVAP_GLOBAL_AVG_MOD'

CONTAINS

SUBROUTINE lake_evap_global_avg                                                &
                         (row_length, rows, global_row_length, global_rows,    &
                          n_wt, mask_for_lake_evap_corr,                       &
                          a_boxareas, flandg, acc_lake_evap,                   &
                          water_type, total_soil_area, acc_lake_evap_avg)

USE global_2d_sums_mod,  ONLY: global_2d_sums

USE yomhook,  ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

IMPLICIT NONE

INTEGER, INTENT(IN) :: row_length
                                  ! No. of columns in atmosphere
INTEGER, INTENT(IN) :: rows
                                  ! No. of rows in atmosphere
INTEGER, INTENT(IN) :: global_row_length
                                  ! No. of points on a row in global grid
INTEGER, INTENT(IN) :: global_rows
                                  ! No. of rows in global grid
INTEGER, INTENT(IN) :: n_wt
                                  ! No. of water fields
                                  ! (either 1 or n_wtrac_jls)

INTEGER, INTENT(IN) :: mask_for_lake_evap_corr(row_length, rows)
                                  ! Mask to indicate where to apply lake
                                  ! evaporation correction

REAL(KIND=real_jlslsm), INTENT(IN) :: a_boxareas(row_length, rows)
                                  ! Atmosphere gridbox areas
REAL(KIND=real_jlslsm), INTENT(IN) :: flandg(row_length, rows)
                                  ! Land fraction

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
                                  acc_lake_evap(row_length,rows,n_wt)
                                  ! Accumulated water or water tracer lake
                                  ! evap over river routing timestep (Kg/m2)

CHARACTER(LEN=5), INTENT(IN) :: water_type
                                  ! Type of water
                                  ! = 'water' for normal water
                                  ! = 'wtrac' for water tracers

REAL(KIND=real_jlslsm), INTENT(IN OUT) :: total_soil_area
                                  ! Global sum of soil area over which lake
                                  ! evaporation is applied (m2).
                                  ! For normal water, this is INTENT(OUT)
                                  ! For water tracers, this is INTENT(IN)

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
                                  acc_lake_evap_avg(n_wt)
                                  ! Global average lake evaporation amount to
                                  ! be removed from soil moisture (kg/m2)
                                  ! Either normal water or water tracers


! Local variables

INTEGER :: i,j,i_wt              ! Counters

INTEGER :: n_sums                ! No. of fields to be summed globally
                                 ! = 2 for normal water (soil area and
                                 !                       lake evap)
                                 ! = n_wtrac_jls for water tracers (water
                                 !                   tracer lake evap only)

REAL(KIND=real_jlslsm), ALLOCATABLE :: sum_r(:,:,:) ! Input to global sum
REAL(KIND=real_jlslsm), ALLOCATABLE :: sum_all(:)   ! Output from global sum

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='LAKE_EVAP_GLOBAL_AVG'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)


! Set up number of global sums
IF (water_type == 'water') THEN
  n_sums = 2
ELSE
  ! Water tracer
  n_sums = n_wt
END IF

ALLOCATE(sum_r(row_length,rows,n_sums))
ALLOCATE(sum_all(n_sums))
sum_r   = 0.0
sum_all = 0.0

! Set up fields to be globally summed
IF (water_type == 'water') THEN

  ! Available soil area and normal water lake evaporation field
  DO j = 1, rows
    DO i = 1, row_length

      IF (mask_for_lake_evap_corr(i,j) == 1) THEN
        sum_r(i,j,2) = a_boxareas(i,j) * flandg(i,j)
      END IF
      sum_r(i,j,1) = acc_lake_evap(i,j,1) * a_boxareas(i,j) * flandg(i,j)

    END DO
  END DO

ELSE
  ! Water tracers
  ! Only water tracer lake evaporation needs to be summed as total land area
  ! has already been calculated in the normal water call to this routine.

  DO i_wt = 1, n_wt
    DO j = 1, rows
      DO i = 1, row_length
        sum_r(i,j,i_wt)= acc_lake_evap(i,j,i_wt)*a_boxareas(i,j)*flandg(i,j)
      END DO
    END DO
  END DO

END IF

! Calculate global sums

CALL global_2d_sums(sum_r, row_length, rows, 0, 0, n_sums, sum_all)

! Outputs:
! If water_type = 'water'
!   sum_all(1)  = global sum of normal water lake evaporation
!   sum_all(2)  = global sum of available soil area
! If water_type ='wtrac'
!   sum_all(1->n_wt) = global sum of water tracer lake evaporation

IF (water_type == 'water') THEN
  total_soil_area = sum_all(2)
END IF

! Calculate global lake evaporation average
DO i_wt = 1, n_wt
  acc_lake_evap_avg(i_wt) = sum_all(i_wt) / total_soil_area
END DO

DEALLOCATE(sum_all)
DEALLOCATE(sum_r)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,                &
                        zhook_handle)
RETURN
END SUBROUTINE lake_evap_global_avg
END MODULE lake_evap_global_avg_mod
#endif
