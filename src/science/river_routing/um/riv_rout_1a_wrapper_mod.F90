#if defined(UM_JULES)
!
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE riv_rout_1a_wrapper_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!---------------------------------------------------------------------------
! Description:
!    Wrapper routine to call the river routing routine riv_rout_1a.
!    This routine is used for both normal water and water tracers
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: River Routing
!
! Code Description:
!   Language: FORTRAN 90
!   This code is written to JULES coding standards v1.
!----------------------------------------------------------------------------

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='RIV_ROUT_1A_WRAPPER_MOD'

CONTAINS

SUBROUTINE riv_rout_1a_wrapper                                                 &
      (global_row_length, global_rows, land_points, land_index,                &
      tot_surf_runoff_gb, tot_sub_runoff_gb,                                   &
      row_length, rows, invert_atmos, xua, yva,                                &
      river_row_length, river_rows, flandg, riv_step, a_boxareas,              &
      twatstor, riverout_rgrid, trivdir, trivseq,                              &
      box_outflow, box_inflow, riverout_atmos, inlandout_riv,                  &
      inlandout_atmos, global_river_row_length, global_river_rows,             &
      land_frac, first, cmessage)


USE errormessagelength_mod, ONLY: errormessagelength
USE riv_rout_mod_1A, ONLY: riv_rout_1a

USE yomhook,  ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
  global_row_length                                                            &
                           ! no. of columns in global atmos grid
, global_rows                                                                  &
                           ! no. of rows in global atmos grid
, row_length                                                                   &
                           ! no. of columns in local atmos grid
, rows                                                                         &
                           ! no. of rows in local atmos grid
, river_row_length                                                             &
                           ! no. of columns in river grid
, river_rows                                                                   &
                           ! no. of rows in river grid
, global_river_row_length                                                      &
                           ! no. of columns in global river grid
, global_river_rows                                                            &
                           ! no. of rows in global river grid
, land_points
                           ! no. of land points

INTEGER, INTENT(IN) ::                                                         &
  land_index (land_points) ! Index of land points

LOGICAL, INTENT(IN) ::                                                         &
  invert_atmos                                                                 &
                           ! TRUE if ATMOS fields are S->N
                           ! for regridding runoff from ATMOS.
, first
                           ! TRUE if first entry into this routine

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  xua(0:global_row_length)                                                     &
                           ! Atmosphere UV longitude coords
, yva(0:global_rows)                                                           &
                           ! Atmosphere latitude coords
, a_boxareas(row_length, rows)                                                 &
                           ! Atmosphere gridbox areas
, flandg(row_length, rows)                                                     &
                           ! Land fraction on local grid
, land_frac(global_row_length, global_rows)
                           ! Land fraction on global grid

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  tot_surf_runoff_gb(land_points)                                              &
                           ! Surf runoff on land pts (kg/m2/s)
, tot_sub_runoff_gb(land_points)
                           ! Subsurf runoff (kg/m2/s)


REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  trivdir(river_row_length, river_rows)                                        &
                           ! River direction
, trivseq(river_row_length, river_rows)                                        &
                           ! River sequence
, riv_step
                           ! River timestep (secs)

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  twatstor(river_row_length, river_rows)
                           ! Water store (Kg) - normal water or water tracer

! The following fields are either for normal water or water tracers:
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  riverout_atmos(row_length,rows)                                              &
                           ! River flow out from each  gridbox (KG/m2/S)
, riverout_rgrid(river_row_length, river_rows)                                 &
                           ! River flow out from TRIP grid to ocean (Kg/s)
, box_outflow(river_row_length, river_rows)                                    &
                           ! Gridbox outflow river grid (Kg/s)
, box_inflow(river_row_length, river_rows)                                     &
                           ! Gridbox runoff river grid(Kg/s)
, inlandout_riv(river_row_length,river_rows)                                   &
                           ! TRIP outflow from inland basins on TRIP grid Kg/s
, inlandout_atmos(row_length,rows)
                           ! TRIP outflow from inland basins on ATMOS grid
                           ! Kg/m2/s

CHARACTER(LEN=errormessagelength), INTENT(OUT) ::                              &
  cmessage                 ! Error message if return code >0


! Local variables

INTEGER ::  i,j,l          ! Counters

REAL(KIND=real_jlslsm) ::                                                      &
  surf_runoff_ij(row_length,rows),                                             &
                           ! Total rate of surface runoff on ij grid (kg/m2/s)
  sub_runoff_ij(row_length,rows)
                           ! Total rate of subsurface runoff on ij grid
                           !  (kg/m2/s)

LOGICAL, PARAMETER ::                                                          &
  regrid = .TRUE.
                           ! TRUE if TRIP grid different to atmos grid

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='RIV_ROUT_1A_WRAPPER'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DO j = 1,rows
  DO i = 1,row_length
    surf_runoff_ij(i,j) = 0.0
    sub_runoff_ij(i,j)  = 0.0
  END DO
END DO

! Copy land points output back to full fields array.
DO l = 1, land_points
  j=(land_index(l) - 1) / row_length + 1
  i = land_index(l) - (j-1) * row_length
  surf_runoff_ij(i,j) = tot_surf_runoff_gb(l)
  sub_runoff_ij(i,j)  = tot_sub_runoff_gb(l)

END DO

! Call river routing routine
CALL riv_rout_1a(global_row_length, global_rows, surf_runoff_ij,               &
      sub_runoff_ij, row_length, rows, invert_atmos, xua, yva,                 &
      river_row_length, river_rows, flandg, regrid,                            &
      riv_step, a_boxareas,                                                    &
      twatstor, riverout_rgrid, trivdir, trivseq,                              &
      box_outflow, box_inflow, riverout_atmos, inlandout_riv,                  &
      inlandout_atmos, global_river_row_length, global_river_rows,             &
      land_frac, first, cmessage)

WHERE (flandg == 1.0) riverout_atmos = 0.0


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,                &
                        zhook_handle)
RETURN
END SUBROUTINE riv_rout_1a_wrapper
END MODULE riv_rout_1a_wrapper_mod
#endif
