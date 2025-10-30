#if defined(UM_JULES)
!
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE lake_evap_apply_soil_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!---------------------------------------------------------------------------
! Description:
!    Remove lake evaporation from soil moisture to conserve water.
!    This routine is used for both normal water and water tracers.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: River Routing
!
! Code Description:
!   Language: FORTRAN 90
!   This code is written to JULES coding standards v1.
!----------------------------------------------------------------------------

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='LAKE_EVAP_APPLY_SOIL_MOD'

CONTAINS

SUBROUTINE lake_evap_apply_soil                                                &
                         (row_length, rows,                                    &
                          land_points, land_index,                             &
                          mask_for_lake_evap_corr, soil_sat_ij,                &
                          acc_lake_evap_avg, water_type,                       &
                          sthu_dsm_ij, smcl_dsm_ij, sthu, smcl)

USE umPrintMgr, ONLY: umPrint, umMessage

USE yomhook,  ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

IMPLICIT NONE

INTEGER, INTENT(IN) :: row_length
                                  ! No. of columns in atmosphere
INTEGER, INTENT(IN) :: rows
                                  ! No. of rows in atmosphere
INTEGER, INTENT(IN) :: land_points
                                  ! No. of landpoints

INTEGER, INTENT(IN) :: land_index (land_points)
                                  ! index of land to global points

INTEGER, INTENT(IN) :: mask_for_lake_evap_corr(row_length,rows)
                                  ! Mask used to apply the lake evaporation
                                  ! to soil points
                                  ! = 1 to apply, = 0 to not apply

REAL(KIND=real_jlslsm), INTENT(IN) :: soil_sat_ij(row_length, rows)
                                  ! Soil saturation point (kg/m2) on ij grid

REAL(KIND=real_jlslsm), INTENT(IN) :: acc_lake_evap_avg
                                  ! Global average accumulated water or water
                                  ! tracer lake evap over
                                  ! river routing timestep (Kg/m2)

CHARACTER(LEN=5), INTENT(IN) :: water_type
                                  !  Type of water
                                  ! 'water' for normal water
                                  ! 'wtrac' for water tracers

REAL(KIND=real_jlslsm), INTENT(IN OUT) :: sthu_dsm_ij(row_length, rows)
                                  ! Water or water tracer STHU on dsm_levels
                                  !  layer on (i,j) grid

REAL(KIND=real_jlslsm), INTENT(IN OUT) :: smcl_dsm_ij(row_length, rows)
                                  ! Water or water tracer SMCL on dsm_levels
                                  !  layer on (i,j) grid

REAL(KIND=real_jlslsm), INTENT(IN OUT) :: sthu(land_points)
                                  ! Water or water tracer unfrozen soil moisture
                                  ! content as a frac of saturation
                                  ! (of deepest soil layer)

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::  smcl(land_points)
                                  ! Water or water tracer total soil moisture
                                  ! contents (of deepest soil layer) (kg/m2)

! Local variables

INTEGER ::  i,j,l                 ! Counters

CHARACTER(LEN=22) :: message_error

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='LAKE_EVAP_APPLY_SOIL'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Set up soil fields on full field arrays.

IF (water_type == 'water') THEN
  message_error= '*WARNING:'
ELSE IF (water_type =='wtrac') THEN
  message_error= '*WATER TRACER WARNING:'
END IF

! Remove global average lake evaporation amount from soil

DO j = 1, rows
  DO i = 1, row_length

    ! Always apply to same grid boxes regardless of whether it's normal water
    ! or water tracers being updated.
    IF (mask_for_lake_evap_corr(i,j) == 1) THEN

      smcl_dsm_ij(i,j) = smcl_dsm_ij(i,j) - acc_lake_evap_avg
      sthu_dsm_ij(i,j) = sthu_dsm_ij(i,j) - acc_lake_evap_avg                  &
                                                 / soil_sat_ij(i,j)

      IF (sthu_dsm_ij(i,j) <  0.0) THEN
        WRITE(umMessage,'(a,a)') message_error,                                &
          ' Unfrozen soil moisture of DSM_LEVELS layer < 0'
        CALL umPrint(umMessage, src=RoutineName)
        WRITE(umMessage,'(2(F8.2))') sthu_dsm_ij(i,j),                         &
             sthu_dsm_ij(i,j) + acc_lake_evap_avg/soil_sat_ij(i,j)
        CALL umPrint(umMessage,src=RoutineName)
      END IF

      IF (smcl_dsm_ij(i,j) <  0.0) THEN
        WRITE(umMessage,'(a,a)') message_error,                                &
          ' Total soil moisture of DSM_LEVELS layer < 0'
        CALL umPrint(umMessage, src=RoutineName)
        WRITE(umMessage,'(2(F8.2))') smcl_dsm_ij(i,j),                         &
             smcl_dsm_ij(i,j) + acc_lake_evap_avg
        CALL umPrint(umMessage,src=RoutineName)
      END IF

    END IF
  END DO
END DO

! Copy full soil fields array back to land points
DO l = 1, land_points
  j=(land_index(l) - 1) / row_length + 1
  i = land_index(l) - (j-1) * row_length
  smcl(l) = smcl_dsm_ij(i,j)
  sthu(l) = sthu_dsm_ij(i,j)
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,                &
                        zhook_handle)
RETURN
END SUBROUTINE lake_evap_apply_soil
END MODULE lake_evap_apply_soil_mod
#endif
