! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Purpose:
! Subroutine to calculate leaf-level values of net photosynthesis,
! leaf conductance and ozone flux.
! *********************************************************************
MODULE leaf_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='LEAF_MOD'

CONTAINS
SUBROUTINE leaf (clos_pts, ft, land_field, open_pts, pft_photo_model, veg_pts  &
,                clos_index ,open_index, veg_index                             &
,                ca, ci, fsmc, o3mol, ra, tl, wcarb, wexpt, wlite, rd          &
,                al, flux_o3, fo3, gl)

USE pftparm, ONLY: dfp_dcuo, fl_o3_ct, glmin
USE c_rmol, ONLY: rmol
USE jules_surface_mod, ONLY: beta1, beta2, ratio, ratio_o3

USE jules_vegetation_mod, ONLY:                                                &
! imported parameters
    photo_collatz, photo_farquhar,                                             &
! imported scalars that are not changed
     l_o3_damage

USE ereport_mod, ONLY: ereport
USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
 clos_pts                                                                      &
                            ! Number of land points with closed stomata.
,ft                                                                            &
                            ! Plant functional type.
,land_field                                                                    &
                            ! Total number of land points.
,open_pts                                                                      &
                            ! Number of land points with open stomata.
,pft_photo_model                                                               &
                            ! Indicates which photosynthesis model to use for
                            ! the current PFT.
,veg_pts
                            ! Number of vegetated points.

!-----------------------------------------------------------------------------
! Array arguments with INTENT(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
 clos_index(land_field)                                                        &
                            ! Index of land points with closed stomata.
,open_index(land_field)                                                        &
                            ! Index of land points with open stomata.
,veg_index(land_field)
                            ! Index of vegetated points on the land grid.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 ca(land_field)                                                                &
                            ! Canopy CO2 pressure (Pa).
,ci(land_field)                                                                &
                            ! Internal CO2 pressure (Pa).
,fsmc(land_field)                                                              &
                            ! Soil water factor.
,o3mol(land_field)                                                             &
                            ! Molar concentration of ozone
                            ! at reference level (nmol/m3).
,ra(land_field)                                                                &
                            ! Total aerodynamic+boundary layer resistance
                            ! between leaf surface and reference level (s/m).
,tl(land_field)                                                                &
                            ! Leaf temperature (K).
,wcarb(land_field)                                                             &
                            ! Carboxylation-limited gross photosynthetic
                            ! rate (mol CO2/m2/s).
,wexpt(land_field)                                                             &
                            ! Export-limited gross photosynthetic rate
                            ! (mol CO2/m2/s).
,wlite(land_field)
                            ! Light-limited gross photosynthetic rate
                            ! (mol CO2/m2/s).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(inout).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
 rd(land_field)              ! Dark respiration (mol CO2/m2/s).
                             ! This is modified only if l_o3_damage=.TRUE..

!-----------------------------------------------------------------------------
! Array arguments with INTENT(out).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 al(land_field)                                                                &
                            ! Net Leaf photosynthesis (mol CO2/m2/s).
,flux_o3(land_field)                                                           &
                            ! Flux of O3 to stomata (nmol O3/m2/s).
,fo3(land_field)                                                               &
                            ! Ozone exposure factor.
,gl(land_field)
                            ! Leaf conductance for H2O (m/s).

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
 errcode                                                                       &
                            ! Error code to pass to ereport.
,j,l                        ! Loop counters.

REAL(KIND=real_jlslsm) ::                                                      &
  b,c,                                                                         &
                            ! Work variables for ozone calculations.
  beta1p2m4, beta2p2m4,                                                        &
                            ! beta[12] ** 2 * 4.
  glmin_ft
                            ! glmin(ft)

REAL(KIND=real_jlslsm) ::                                                      &
 b1(land_field)                                                                &
,b2(land_field)                                                                &
,b3(land_field)                                                                &
                            ! Coefficients of the quadratic.
,conv(land_field)                                                              &
                            ! Factor for converting mol/m3 into Pa (J/mol).
,glco2(land_field)                                                             &
                            ! Leaf conductance for CO2 (m/s).
,wl(land_field)                                                                &
                            ! Gross leaf phtosynthesis (mol CO2/m2/s).
,wp(land_field)
                            ! Smoothed minimum of carboxylation- and light-
                            ! limited gross photosynthesis (mol CO2/m2/s).

INTEGER ::                                                                     &
 land_grid_index(open_pts)
                            ! Land grid index

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='LEAF'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DO j = 1,open_pts
  land_grid_index(j) = veg_index(open_index(j))
END DO
glmin_ft = glmin(ft)

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(j,l,b,c,beta1p2m4,beta2p2m4)              &
!$OMP IF(open_pts > 1)                                                         &
!$OMP SHARED(open_pts, pft_photo_model, land_grid_index, beta1,                &
!$OMP        errcode, wcarb, wlite,                                            &
!$OMP        beta2, wp, wexpt, b1, b2, b3, wl, al, rd, fsmc,                   &
!$OMP        conv, tl, glco2, ca, ci, gl, glmin_ft, ft, l_o3_damage,           &
!$OMP        o3mol, ra, fo3, dfp_dcuo, fl_o3_ct, flux_o3)

SELECT CASE ( pft_photo_model )

CASE ( photo_collatz )
  !---------------------------------------------------------------------------
  ! Use the Collatz model.
  ! Calculate the co-limited rate of gross photosynthesis.
  !---------------------------------------------------------------------------
  beta1p2m4 = 4 * beta1 * beta1
  beta2p2m4 = 4 * beta2 * beta2

!DIR$ IVDEP
!$OMP DO SCHEDULE(STATIC)
  DO j = 1,open_pts
    l = land_grid_index(j)

    b1(l) = beta1
    b2(l) = - (wcarb(l) + wlite(l))
    b3(l) = wcarb(l) * wlite(l)

    wp(l) = -b2(l) / (2.0 * b1(l))                                             &
            - SQRT(b2(l) * b2(l) / beta1p2m4      - b3(l) / b1(l))

    b1(l) = beta2
    b2(l) = - (wp(l) + wexpt(l))
    b3(l) = wp(l) * wexpt(l)

    wl(l) = -b2(l) / (2.0 * b1(l))                                             &
            - SQRT(b2(l) * b2(l) / beta2p2m4       - b3(l) / b1(l))

  END DO
!$OMP END DO

CASE ( photo_farquhar )

  !---------------------------------------------------------------------------
  ! Use the Farquhar model.
  !---------------------------------------------------------------------------
!DIR$ IVDEP
!$OMP DO SCHEDULE(STATIC)
  DO j = 1,open_pts
    l = land_grid_index(j)
    wl(l) = MIN( wcarb(l), wlite(l) )
  END DO
!$OMP END DO

CASE DEFAULT
!$OMP MASTER
  errcode = 101  !  a hard error
  CALL ereport(RoutineName, errcode,                                           &
               'pft_photo_model should be photo_collatz or photo_farquhar')
!$OMP END MASTER

END SELECT  !  pft_photo_model

!-----------------------------------------------------------------------------
! Carry out calculations for points with open stomata
!-----------------------------------------------------------------------------
!DIR$ IVDEP
!$OMP DO  SCHEDULE(STATIC)
DO j = 1,open_pts
  l = land_grid_index(j)

  !---------------------------------------------------------------------------
  ! Calculate the net rate of photosynthesis
  !---------------------------------------------------------------------------
  al(l) = (wl(l) - rd(l)) * fsmc(l)

  !---------------------------------------------------------------------------
  ! Calculate the factor for converting mol/m3 into Pa (J/m3).
  !---------------------------------------------------------------------------
  conv(l) = rmol * tl(l)

  !---------------------------------------------------------------------------
  ! Diagnose the leaf conductance
  !---------------------------------------------------------------------------
  glco2(l) = (al(l) * conv(l)) / (ca(l) - ci(l))
  gl(l)    = ratio * glco2(l)

END DO
!$OMP END DO

!-----------------------------------------------------------------------------
! Close stomata at points with negative or zero net photosynthesis
! or where the leaf resistance exceeds its maximum value.
!-----------------------------------------------------------------------------
!DIR$ IVDEP
!$OMP DO SCHEDULE(STATIC)
DO j = 1,open_pts
  l = land_grid_index(j)

  IF (gl(l) <= glmin_ft .OR. al(l) <= 0.0) THEN
    gl(l)    = glmin_ft
    al(l)    = -rd(l) * fsmc(l)
  END IF

END DO
!$OMP END DO

IF ( l_o3_damage ) THEN
  !---------------------------------------------------------------------------
  ! Modify the stomatal conductance and photosynthesis for ozone effects
  ! (Peter Cox, 12/11/04)
  !---------------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
  DO j = 1,open_pts
    l = land_grid_index(j)

    !-------------------------------------------------------------------------
    ! Flux of O3 without ozone effects (for use in analytical eqn)
    !-------------------------------------------------------------------------
    flux_o3(l) = o3mol(l) / (ra(l) + (ratio_o3 / gl(l)))

    !-------------------------------------------------------------------------
    ! Analytic solution for the ozone exposure factor
    !-------------------------------------------------------------------------
    ! Use EPSILON to avoid overflow on division
    IF (ABS(ra(l)) < EPSILON(1.0)) THEN
      fo3(l) = (1.0 + dfp_dcuo(ft) * fl_o3_ct(ft))                             &
             / (1.0 + dfp_dcuo(ft) * flux_o3(l))
    ELSE
      b      = ratio_o3 / (gl(l) * ra(l))                                      &
               + dfp_dcuo(ft) * o3mol(l) / ra(l)                               &
               - (1.0 + dfp_dcuo(ft) * fl_o3_ct(ft))
      c      = -ratio_o3 / (gl(l) * ra(l))                                     &
               * (1.0 + dfp_dcuo(ft) * fl_o3_ct(ft))
      fo3(l) = -0.5 * b + 0.5 * SQRT(b * b - 4.0 * c)
    END IF

    fo3(l) = MIN(MAX(fo3(l),0.0),1.0)

    !-------------------------------------------------------------------------
    ! Update the leaf conductance and photosynthesis
    !-------------------------------------------------------------------------
    gl(l) = gl(l) * fo3(l)
    al(l) = al(l) * fo3(l)
  END DO
!$OMP END DO

  !---------------------------------------------------------------------------
  ! Close stomata at points with negative or zero net photosynthesis
  ! or where the leaf resistance exceeds its maximum value.
  !---------------------------------------------------------------------------
!DIR$ IVDEP
!$OMP DO SCHEDULE(STATIC)
  DO j = 1,open_pts
    l = land_grid_index(j)

    IF (gl(l) <= glmin_ft .OR. al(l) <= 0.0) THEN
      gl(l) = glmin_ft
      al(l) = -rd(l) * fsmc(l) * fo3(l)
    END IF
  END DO
!$OMP END DO

END IF ! o3 damage

!$OMP END PARALLEL

!-----------------------------------------------------------------------------
! Define fluxes and conductances for points with closed stomata
!-----------------------------------------------------------------------------
!$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(j,l) IF(clos_pts > 1) SCHEDULE(STATIC) &
!$OMP  SHARED(clos_pts,veg_index,clos_index,gl,glmin_ft,al,rd,fsmc,l_o3_damage,fo3)
!DIR$ IVDEP
DO j = 1,clos_pts
  l = veg_index(clos_index(j))

  gl(l)    = glmin_ft
  al(l)    = -rd(l) * fsmc(l)

  ! Indicate no ozone damage at closed points.
  IF (l_o3_damage) fo3(l) = 1.0

END DO
!$OMP END PARALLEL DO

IF ( l_o3_damage ) THEN
  !---------------------------------------------------------------------------
  ! Diagnose the ozone deposition flux on all points
  !---------------------------------------------------------------------------
!$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(j,l) IF(veg_pts > 1) SCHEDULE(STATIC)  &
!$OMP  SHARED(veg_pts,veg_index,flux_o3,o3mol,ra,gl,rd,fo3)
  DO j = 1,veg_pts
    l = veg_index(j)
    flux_o3(l) = o3mol(l) / (ra(l) + (ratio_o3 / gl(l)))
    rd(l)      = rd(l) * fo3(l)
  END DO
!$OMP END PARALLEL DO
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE leaf
END MODULE leaf_mod
