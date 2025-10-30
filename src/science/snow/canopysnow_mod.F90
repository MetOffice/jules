! *****************************COPYRIGHT*******************************

! (c) [University of Edinburgh]. All rights reserved.
! This routine has been licensed to the Met Office for use and
! distribution under the JULES collaboration agreement, subject
! to the terms and conditions set out therein.
! [Met Office Ref SC237]

! *****************************COPYRIGHT*******************************
!  SUBROUTINE CANOPYSNOW------------------------------------------------------

! Description:
!     Partition snowfall into canopy interception, throughfall and unloading.

! Subroutine Interface:
MODULE canopysnow_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='CANOPYSNOW_MOD'

CONTAINS

SUBROUTINE canopysnow ( land_pts, surft_pts, n_wtrac_jls,                      &
                        timestep, cansnowtile,                                 &
                        surft_index, catch_snow, con_snow, ls_snow, ls_graup,  &
                        unload_backgrnd_surft, melt_surft,                     &
                        con_snow_wtrac, ls_snow_wtrac, ls_graup_wtrac,         &
                        snow_can, snow_can_wtrac,                              &
                        snowfall, graupfall, snowfall_wtrac, graupfall_wtrac )

USE jules_snow_mod, ONLY:                                                      &
  ! imported scalars
  snowinterceptfact,                                                           &
  ! Constant in relationship between mass of intercepted snow and
  ! snowfall rate.
  snowunloadfact
  ! Constant in relationship between canopy snow unloading and canopy
  ! snow melt rate.

USE jules_science_fixes_mod, ONLY: l_fix_neg_snow
USE jules_water_tracers_mod, ONLY: l_wtrac_jls, wtrac_calc_ratio_fn_jules

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER,INTENT(IN) ::                                                          &
  land_pts,                                                                    &
    !  Number of land points.
  surft_pts,                                                                   &
    !  Number of tile points.
  n_wtrac_jls
    !  Number of water tracers.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  timestep                 ! Timestep (s).

LOGICAL, INTENT(IN) ::                                                         &
  cansnowtile              ! Switch for canopy snow model.

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  surft_index(land_pts)    ! Index of tile points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  catch_snow(land_pts),                                                        &
    ! Canopy snow capacity (kg/m2).
  con_snow(land_pts),                                                          &
    ! Convective snowfall rate (kg/m2/s).
  ls_snow(land_pts),                                                           &
    ! Large-scale frozen precip fall rate (kg/m2/s).
  ls_graup(land_pts),                                                          &
    ! Large-scale graupel fall rate (kg/m2/s).
  melt_surft(land_pts),                                                        &
    ! Canopy snow melt rate (kg/m2/s).
  unload_backgrnd_surft(land_pts),                                             &
    ! Background canopy unloading rate.
  con_snow_wtrac(land_pts,n_wtrac_jls),                                        &
    ! Water tracer convective snowfall rate (kg/m2/s).
  ls_snow_wtrac(land_pts,n_wtrac_jls),                                         &
    ! Water tracer large-scale frozen precip fall rate (kg/m2/s).
  ls_graup_wtrac(land_pts,n_wtrac_jls)
    ! Water tracer large-scale graupel fall rate (kg/m2/s).

!-----------------------------------------------------------------------------
! Array arguments with intent(inout)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  snow_can(land_pts),                                                          &
    ! Canopy snow load, if the canopy snow model is selected (kg/m2).
  snow_can_wtrac(land_pts,n_wtrac_jls)
    ! Water tracer canopy snow load (kg/m2).

!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  snowfall(land_pts),                                                          &
    ! Frozen precip reaching the ground in timestep (kg/m2).
  graupfall(land_pts),                                                         &
    ! Graupel reaching the ground in timestep (kg/m2).
  snowfall_wtrac(land_pts,n_wtrac_jls),                                        &
    ! Water tracer frozen precip reaching the ground in timestep (kg/m2).
  graupfall_wtrac(land_pts,n_wtrac_jls)
    ! Water tracer graupel reaching the ground in timestep (kg/m2).

!-----------------------------------------------------------------------------
! Local scalars
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i,                                                                           &
    ! Land point index
  k,                                                                           &
    ! Tile point index
  i_wt
    ! Water tracer index

REAL(KIND=real_jlslsm) ::                                                      &
  intercept(surft_pts),                                                        &
    ! Snow intercepted by canopy in timestep (kg/m2).
  unload(surft_pts)
    ! Canopy snow unloaded in timestep (kg/m2).

REAL(KIND=real_jlslsm) ::                                                      &
  intercept_wtrac,                                                             &
    ! Water tracer snow intercepted by canopy in timestep (kg/m2).
  unload_wtrac,                                                                &
    ! Water tracer canopy snow unloaded in timestep (kg/m2).
  ratio_snowfall,                                                              &
    ! Ratio of water tracer to water for snow fall
  ratio_snow_can
    ! Ratio of water tracer to water for canopy snow

REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
  snowfall_old(:),                                                             &
    ! Initial snowfall
  snow_can_old(:)
    ! Initial canopy snow load

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CANOPYSNOW'

!-----------------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF ( cansnowtile ) THEN

  ! Allocate temporary fields for water tracers use
  IF (l_wtrac_jls) THEN
    ALLOCATE(snowfall_old(surft_pts))
    ALLOCATE(snow_can_old(surft_pts))
  ELSE
    ALLOCATE(snowfall_old(1))
    ALLOCATE(snow_can_old(1))
  END IF

!$OMP PARALLEL DEFAULT(NONE)                                                   &
!$OMP PRIVATE(k,i,i_wt,ratio_snowfall,ratio_snow_can,intercept_wtrac,          &
!$OMP         unload_wtrac)                                                    &
!$OMP SHARED(surft_pts,surft_index,snowfall,ls_snow,ls_graup,con_snow,         &
!$OMP        timestep,graupfall,catch_snow,snow_can,melt_surft,                &
!$OMP        unload_backgrnd_surft,snowinterceptfact,snowunloadfact,           &
!$OMP        intercept,unload,snowfall_old,snow_can_old,l_wtrac_jls,           &
!$OMP        snowfall_wtrac,ls_snow_wtrac,ls_graup_wtrac, con_snow_wtrac,      &
!$OMP        graupfall_wtrac,snow_can_wtrac,n_wtrac_jls, l_fix_neg_snow)
  IF (l_wtrac_jls) THEN
    ! If water tracers, save initial values of some fields for use later on.
    ! (Note, these 'old' fields have size surft_pts)
!$OMP DO SCHEDULE(STATIC)
    DO k = 1,surft_pts
      i = surft_index(k)
      snowfall_old(k) = ( ls_snow(i) - ls_graup(i) + con_snow(i) ) * timestep
      snow_can_old(k) = snow_can(i)
    END DO
!$OMP END DO
  END IF


!$OMP DO SCHEDULE(STATIC)
  DO k = 1,surft_pts
    i = surft_index(k)
    snowfall(i)  = ( ls_snow(i) - ls_graup(i) + con_snow(i) ) * timestep
    graupfall(i) = ls_graup(i) * timestep
    intercept(k) = snowinterceptfact * (catch_snow(i) - snow_can(i))           &
                   * (1.0 - EXP(-snowfall(i) / catch_snow(i)))
    unload(k)    = snowunloadfact * melt_surft(i) * timestep                   &
                   + unload_backgrnd_surft(i) * snow_can(i) * timestep
    !-----------------------------------------------------------------------
    ! At this point, the value of unload can be larger than the
    ! amount of snow on the canopy (which has already had melt
    ! and sublimation removed), so we need to limit to the amount
    ! of snow available. However, snow_can can be <0 (with small
    ! absolute value) because of "issues" in the surface flux code,
    ! so we also restrict unload to be >=0.
    !-----------------------------------------------------------------------
    IF (l_fix_neg_snow) THEN
      ! Ensure the interception and the unloading are non-negative
      ! and ensure that the final canopy snow amount does not exceed
      ! the canopy capacity.
      intercept(k) = MAX(0.0, intercept(k))
      unload(k)    = MAX(0.0, unload(k))
      snow_can(i) = snow_can(i) + intercept(k) - unload(k)
      IF (snow_can(i) > catch_snow(i)) THEN
        unload(k) = unload(k) + (snow_can(i) - catch_snow(i))
        snow_can(i) = catch_snow(i)
      END IF
    ELSE
      ! Do not limit the interception and limit the unloading based
      ! solely on the existing canopy amounts.
      unload(k)      = MAX( MIN( unload(k), snow_can(i) ), 0.0 )
      snow_can(i) = snow_can(i) + intercept(k) - unload(k)
    END IF
    snowfall(i) = snowfall(i) + graupfall(i) - intercept(k) + unload(k)
  END DO
!$OMP END DO

  IF (l_wtrac_jls) THEN
    ! Repeat for water tracers
    DO i_wt = 1, n_wtrac_jls
!$OMP DO SCHEDULE(STATIC)
      DO k = 1,surft_pts
        i = surft_index(k)
        snowfall_wtrac(i,i_wt)  = ( ls_snow_wtrac(i,i_wt) -                    &
                                  ls_graup_wtrac(i,i_wt) +                     &
                                  con_snow_wtrac(i,i_wt) ) * timestep
        graupfall_wtrac(i,i_wt) = ls_graup_wtrac(i,i_wt) * timestep

        ! Calculate water tracer to water ratio for snowfall to scale
        ! intercept for water tracers. (Use snowfall value on input to routine.)
        ratio_snowfall          = wtrac_calc_ratio_fn_jules(i_wt,              &
                                  snowfall_wtrac(i,i_wt), snowfall_old(k))
        intercept_wtrac         = intercept(k) * ratio_snowfall

        ! Calculate water tracer to water ratio for canopy snow to scale
        ! unload for water tracers. (Use snow_can value on input to routine.)
        ratio_snow_can          = wtrac_calc_ratio_fn_jules(i_wt,              &
                                  snow_can_wtrac(i,i_wt), snow_can_old(k))
        unload_wtrac            = unload(k) * ratio_snow_can

        unload_wtrac            = MAX( MIN( unload_wtrac,                      &
                                       snow_can_wtrac(i,i_wt) ), 0.0 )
        snow_can_wtrac(i,i_wt) = snow_can_wtrac(i,i_wt) +                      &
                                 intercept_wtrac - unload_wtrac
        snowfall_wtrac(i,i_wt) = snowfall_wtrac(i,i_wt) +                      &
                                 graupfall_wtrac(i,i_wt) -                     &
                                 intercept_wtrac + unload_wtrac

      END DO
!$OMP END DO
    END DO
  END IF ! l_wtrac_jls

!$OMP END PARALLEL

  DEALLOCATE(snow_can_old)
  DEALLOCATE(snowfall_old)

ELSE

!$OMP PARALLEL DEFAULT(NONE)                                                   &
!$OMP PRIVATE(k,i,i_wt)                                                        &
!$OMP SHARED(surft_pts,surft_index,snowfall,ls_snow,con_snow,timestep,         &
!$OMP        graupfall,ls_graup,snowfall_wtrac,ls_snow_wtrac,con_snow_wtrac,   &
!$OMP        graupfall_wtrac,ls_graup_wtrac,n_wtrac_jls,l_wtrac_jls)
!$OMP DO SCHEDULE(STATIC)
  DO k = 1,surft_pts
    i = surft_index(k)
    snowfall(i)  = ( ls_snow(i) + con_snow(i) ) * timestep
    graupfall(i) = ls_graup(i) * timestep
  END DO
!$OMP END DO

  IF (l_wtrac_jls) THEN
    DO i_wt = 1, n_wtrac_jls
!$OMP DO SCHEDULE(STATIC)
      DO k = 1,surft_pts
        i = surft_index(k)
        snowfall_wtrac(i,i_wt)  = ( ls_snow_wtrac(i,i_wt) +                    &
                                  con_snow_wtrac(i,i_wt) ) * timestep
        graupfall_wtrac(i,i_wt) = ls_graup_wtrac(i,i_wt) * timestep
      END DO
!$OMP END DO
    END DO
  END IF

!$OMP END PARALLEL

END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE canopysnow
END MODULE canopysnow_mod
