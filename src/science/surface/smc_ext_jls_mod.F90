! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE smc_ext_mod

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SMC_EXT_MOD'

CONTAINS

SUBROUTINE smc_ext (npnts,nshyd,surft_pts,surft_index,ft                       &
,                   f_root,sthu,v_open,v_sat,v_close                           &
,                   bexp, sathh                                                &
,                   wt_ext,fsmc,psi_root_zone_sox)
!---------------------------------------------------------------------

! Description:
!     Calculates the soil moisture availability factor and
!     the fraction of the transpiration which is extracted from each
!     soil layer.

! Documentation : UM Documentation Paper 25
!---------------------------------------------------------------------
USE pftparm, ONLY: fsmc_mod
USE hyd_psi_mod, ONLY: psi_from_sthu
USE jules_vegetation_mod, ONLY: fsmc_shape,stomata_model,stomata_sox

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) ::                                                         &
 npnts                                                                         &
                      ! Number of gridpoints.
,nshyd                                                                         &
                      ! Number of soil moisture layers.
,surft_pts                                                                     &
                      ! Number of points containing the
!                     !    given surface type.
,surft_index(npnts)                                                            &
                      ! Indices on the land grid of the
!                     !    points containing the given
!                     !    surface type
,ft                   ! Plant functional type.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 f_root(npnts,nshyd)                                                           &
                      ! Fraction of roots in each soil
!                     !    layer.
,sthu(npnts,nshyd)                                                             &
                      ! Unfrozen soil moisture content of
!                     !    each layer as a fraction of
!                     !    saturation.
,v_sat(npnts,nshyd)                                                            &
                      ! Volumetric soil moisture
!                     !    concentration at saturation
!                     !    (m3 H2O/m3 soil) from JULES_SOIL_PROPS namelist
,v_open(npnts,nshyd)                                                           &
!                     ! Volumetric soil moisture
!                     !    concentration above which stomatal aperture
!                     !    is not limited by soil water (m3 H2O/m3 soil)
!                     ! When l_use_pft_psi=F, v_open
!                     !    is set from smvccl_soilt - fsmc_p0 *
!                     !    (smvccl_soilt - smvcwt_soilt) (in physiol).
!                     ! When l_use_pft_psi=T, v_open
!                     !    is calculated from psi_open.
,v_close(npnts,nshyd)                                                          &
                      ! Volumetric soil moisture
!                     !    concentration below which
!                     !    stomata close (m3 H2O/m3 soil).
!                     ! When l_use_pft_psi=F, v_close
!                     !    is set from smvcwt_soilt (in physiol).
!                     ! When l_use_pft_psi=T, v_close
!                     !    is calculated from psi_close.
,bexp(npnts,nshyd)                                                             &
                      ! Exponent in soil hydraulic characteristics.
,sathh(npnts,nshyd)
                      ! If l_vg=False, absolute value of the soil matric
                      ! suction at saturation in m
                      ! If l_vg=True, sathh = 1 / alpha, where alpha
                      ! (in m-1) is a parameter in the van Genuchten model

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 wt_ext(npnts,nshyd)  ! Cumulative fraction of transpiration
!                     !    extracted from each soil layer
!                     !    (kg/m2/s).

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 fsmc(npnts)                                                                   &
                      ! Soil moisture availability
!                     ! factor.
,psi_root_zone_sox(npnts)
                      ! (negative) Soil water potential in root zone (Pa)
                      ! To be passed up for use by SOX in sf_stom.

! work
INTEGER ::                                                                     &
 i,j,n                ! Loop counters

REAL(KIND=real_jlslsm) ::                                                      &
 fsmc_l(npnts,nshyd)                                                           &
!                     ! Soil moisture availability
!                     !    factor for each soil layer.
,v_layer(npnts,nshyd)                                                          &
!                     ! Volumetric soil moisture
!                     !    concentration of layer
!                     !    (m3 H2O/m3 soil).
,psi(npnts,nshyd)                                                              &
!                     ! (negative) soil water potential (in Pa) on layers
,psi_root_zone(npnts)
!                     ! (negative) soil water potential (in Pa) in root zone

REAL(KIND=real_jlslsm) ::                                                      &
 v_root_zone(npnts)                                                            &
!                     ! Volumetric soil moisture
!                     !    concentration of root zone
!                     !    (m3 H2O/m3 soil)
,v_close_root_zone(npnts)                                                      &
!                     ! Volumetric soil moisture
!                     !    concentration of root zone below which
!                     !    stomata close (m3 H2O/m3 soil)
,v_open_root_zone(npnts)
!                     ! Volumetric soil moisture
!                     !    concentration above which stomatal aperture is
!                     !    not limited by soil water.
!                     !    (m3 H2O/m3 soil).

REAL(KIND=real_jlslsm), PARAMETER :: sthu_min = 0.01 ! required by psi_from_sthu

REAL(KIND=real_jlslsm) :: ones(npnts)

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SMC_EXT'

!----------------------------------------------------------------------
! Initialisations
!----------------------------------------------------------------------
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(n,i)                                                             &
!$OMP SHARED(nshyd,npnts,psi,v_layer,sthu,v_sat,wt_ext,psi_root_zone,fsmc,ones,&
!$OMP        psi_root_zone_sox)
DO n = 1,nshyd
!$OMP DO SCHEDULE(STATIC)
  DO i = 1,npnts
    psi(i,n)     = 0.0
    v_layer(i,n) = sthu(i,n) * v_sat(i,n)
    wt_ext(i,n)  = 0.0
  END DO
!$OMP END DO NOWAIT
END DO

!$OMP DO SCHEDULE(STATIC)
DO i = 1,npnts
  psi_root_zone(i)  = 0.0
  psi_root_zone_sox(i) = 0.0
  fsmc(i)           = 0.0
  ones(i)           = 1.0
END DO
!$OMP END DO NOWAIT
!$OMP END PARALLEL

IF ( fsmc_mod(ft) == 1 ) THEN

  v_root_zone       = calc_weighted_mean(npnts, nshyd, surft_pts,              &
                                         surft_index, v_layer, f_root)
  v_close_root_zone = calc_weighted_mean(npnts, nshyd, surft_pts,              &
                                         surft_index, v_close, f_root)
  v_open_root_zone  = calc_weighted_mean(npnts, nshyd, surft_pts,              &
                                         surft_index, v_open, f_root)

  IF ( fsmc_shape == 1 ) THEN
!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(j,i)                                                             &
!$OMP SHARED(surft_pts,surft_index,psi_root_zone,v_root_zone,v_sat,sathh,bexp)
    DO j = 1,surft_pts
      i = surft_index(j)
      ! Have already checked for soil_props_const_z in init_pftparm in
      ! standalone jules, so just using soil props of top layer. UM doesn't
      ! allow this option (fsmc_shape=1) yet anyway
      psi_root_zone(i) = psi_from_sthu(v_root_zone(i) / v_sat(i,1),            &
                                       sathh(i,1), bexp(i,1), sthu_min)
    END DO
!$OMP END PARALLEL DO
  END IF

  IF ( stomata_model == stomata_sox ) THEN
!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(j,i)                                                             &
!$OMP SHARED(surft_pts,surft_index,psi_root_zone_sox,v_root_zone,v_sat,sathh,  &
!$OMP        bexp)
    DO j = 1,surft_pts
      i = surft_index(j)
      ! If SOX is being used psi_root_zone is needed. fsmc_shape is not
      ! be used in sox - fsmc will be set to 1.0 always
      psi_root_zone_sox(i) = psi_from_sthu(v_root_zone(i) / v_sat(i,1),        &
                                       sathh(i,1), bexp(i,1), sthu_min)

    END DO
!$OMP END PARALLEL DO

    fsmc(:) = 1.0

  ELSE
    fsmc = fsmc_layer(npnts, surft_pts, surft_index, ft, v_root_zone,          &
                      v_close_root_zone, v_open_root_zone, ones, psi_root_zone)
  END IF !stomata_model

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(j,i,n)                                                           &
!$OMP SHARED(surft_pts,surft_index,wt_ext,f_root,v_layer,v_close,nshyd)
  DO n = 1,nshyd
!$OMP DO SCHEDULE(STATIC)
    DO j = 1,surft_pts
      i = surft_index(j)
      ! Calculate the fraction of transpiration extracted from each soil layer
      ! based on the water in that layer above the point where stomata close.
      wt_ext(i,n) = MAX(f_root(i,n) * (v_layer(i,n) - v_close(i,n)), 0.0)
    END DO
!$OMP END DO NOWAIT
  END DO
!$OMP END PARALLEL

  wt_ext = calc_norm_weights(npnts, nshyd, surft_pts, surft_index,             &
                             wt_ext)

ELSE
  IF ( fsmc_shape == 1 ) THEN
!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(j,i,n)                                                           &
!$OMP SHARED(surft_pts,surft_index,psi,sthu,sathh,bexp,nshyd)
    DO n = 1,nshyd
!$OMP DO SCHEDULE(STATIC)
      DO j = 1,surft_pts
        i = surft_index(j)
        psi(i,n) = psi_from_sthu(sthu(i,n), sathh(i,n), bexp(i,n), sthu_min)
      END DO
!$OMP END DO NOWAIT
    END DO
!$OMP END PARALLEL
  END IF
  !----------------------------------------------------------------------
  ! Calculate the soil moisture availability factor for each layer and
  ! weight with the root fraction to calculate the total availability
  ! factor.
  !----------------------------------------------------------------------
  DO n = 1,nshyd
    fsmc_l(:,n) = fsmc_layer(npnts,surft_pts,surft_index,                      &
                             ft, sthu(:,n), v_close(:,n),                      &
                             v_open(:,n), v_sat(:,n), psi(:,n))
  END DO

  fsmc = calc_weighted_mean(npnts, nshyd, surft_pts, surft_index,              &
                            fsmc_l, f_root)

  !----------------------------------------------------------------------
  ! Calculate the fraction of the transpiration which is extracted from
  ! each soil layer.
  !----------------------------------------------------------------------

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(j,i,n)                                                           &
!$OMP SHARED(surft_pts,surft_index,fsmc,wt_ext,f_root,fsmc_l,nshyd)
  DO n = 1,nshyd
!$OMP DO SCHEDULE(STATIC)
    DO j = 1,surft_pts
      i = surft_index(j)
      IF (fsmc(i) > 0.0)                                                       &
        wt_ext(i,n) = f_root(i,n) * fsmc_l(i,n) / fsmc(i)
    END DO
!$OMP END DO NOWAIT
  END DO
!$OMP END PARALLEL
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE smc_ext

FUNCTION calc_norm_weights(npnts, nshyd, surft_pts, surft_index,               &
                           weights) RESULT (norm_weights)
!-----------------------------------------------------------------------------
! Description:
!   Normalises an array of weights over the soil levels dimension.
!   The weights should be >= 0.
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

! function arguments
INTEGER, INTENT(IN) :: npnts              ! Number of gridpoints.
INTEGER, INTENT(IN) :: nshyd              ! Number of soil moisture layers.
INTEGER, INTENT(IN) :: surft_pts          ! Number of points containing the
!                                           !    given surface type.
INTEGER, INTENT(IN) :: surft_index(npnts) ! Indices on the land grid of the
!                                           !    points containing the given
!                                           !    surface type.

REAL(KIND=real_jlslsm), INTENT(IN) :: weights(npnts,nshyd)
                                          ! Unnormalised weights.
!                                           ! Should all be >= 0.

! work
REAL(KIND=real_jlslsm) :: norm_factor     ! Factor used in the normalisation
INTEGER :: i,j,n                          ! Loop counters

! returns
REAL(KIND=real_jlslsm) :: norm_weights(npnts,nshyd)         ! Normalised weights

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CALC_NORM_WEIGHTS'
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

norm_weights(:, :) = 0.0
norm_factor = 0.0

DO n = 1,nshyd
  DO j = 1,surft_pts
    i = surft_index(j)

    norm_factor = SUM(weights(i,:))

    IF (norm_factor > 0.0) THEN
      norm_weights(i,n) = weights(i,n) / norm_factor
    END IF
  END DO
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END FUNCTION calc_norm_weights

FUNCTION calc_weighted_mean(npnts, nshyd, surft_pts, surft_index,              &
                            var, norm_weights) RESULT (weighted_mean)
!-----------------------------------------------------------------------------
! Description:
!   Calculates the weighted mean of a variable over soil levels
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

! function arguments
INTEGER, INTENT(IN) :: npnts              ! Number of gridpoints.
INTEGER, INTENT(IN) :: nshyd              ! Number of soil moisture layers.
INTEGER, INTENT(IN) :: surft_pts          ! Number of points containing the
!                                           !    given surface type.
INTEGER, INTENT(IN) :: surft_index(npnts) ! Indices on the land grid of the
!                                           !    points containing the given
!                                           !    surface type.

REAL(KIND=real_jlslsm), INTENT(IN) :: var(npnts,nshyd)
                                          ! Variable to take the weighted mean of
REAL(KIND=real_jlslsm), INTENT(IN) :: norm_weights(npnts,nshyd)
!                                           ! Normalised weights to use in the mean

! work
INTEGER :: i,j,n                          ! Loop counters

! returns
REAL(KIND=real_jlslsm) :: weighted_mean(npnts)

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CALC_WEIGHTED_MEAN'
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

weighted_mean(:)=0.0

DO n = 1,nshyd
  !CDIR NODEP
  DO j = 1,surft_pts
    i = surft_index(j)

    weighted_mean(i) = weighted_mean(i) + norm_weights(i,n) * var(i,n)

  END DO
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END FUNCTION calc_weighted_mean

FUNCTION fsmc_layer(npnts,surft_pts,surft_index,                               &
                    pft, v, v_close,                                           &
                    v_open, extra_factor, psi) RESULT (fsmc_l)
!-----------------------------------------------------------------------------
! Description:
!   Calculates the soil water availability factor of a soil layer
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE jules_vegetation_mod, ONLY: fsmc_shape
USE pftparm, ONLY: psi_open, psi_close

IMPLICIT NONE

INTEGER, INTENT(IN) :: npnts
INTEGER, INTENT(IN) :: surft_pts
INTEGER, INTENT(IN) :: surft_index(npnts)
INTEGER, INTENT(IN) :: pft  ! Plant functional type.

REAL(KIND=real_jlslsm), INTENT(IN) :: v(npnts), extra_factor(npnts)
                            ! v*extra_factor is the volumetric soil moisture
!                             !    concentration in layer
!                             !    (m3 H2O/m3 soil).
REAL(KIND=real_jlslsm), INTENT(IN) :: v_close(npnts)  ! Volumetric soil moisture
!                             !    concentration below which
!                             !    stomata close (m3 H2O/m3 soil).
REAL(KIND=real_jlslsm), INTENT(IN) :: v_open(npnts)  ! Volumetric soil moisture
!                             !    concentration above which
!                             !    stomatal aperture is not limited by soil water.
!                             !    (m3 H2O/m3 soil).
REAL(KIND=real_jlslsm), INTENT(IN) :: psi(npnts)
!                             ! (negative) soil water potential (in Pa)

! internal
REAL(KIND=real_jlslsm) :: x, x_open, x_close

! returns
REAL(KIND=real_jlslsm) :: fsmc_l(npnts)
                       ! soil water availability factor for this soil layer

INTEGER :: i,j

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='FSMC_LAYER'
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(j,i,x_open,x_close,x)                                            &
!$OMP SHARED(surft_pts,surft_index,fsmc_shape,fsmc_l,extra_factor,psi_open,    &
!$OMP        psi_close,psi,v_open,v_close,v,pft)
DO j = 1,surft_pts
  i = surft_index(j)

  IF ( fsmc_shape == 0 ) THEN
    x_open  = v_open(i)
    x_close = v_close(i)
    x       = v(i)

    IF ( ABS(x_open - x_close) > 0.0 ) THEN
      fsmc_l(i) = (x * extra_factor(i) - x_close) / (x_open - x_close)
    ELSE
      fsmc_l(i) = 0.0
    END IF
  ELSE
    x_open  = psi_open(pft)
    x_close = psi_close(pft)
    x       = psi(i)

    IF ( ABS(x_open - x_close) > 0.0 ) THEN
      fsmc_l(i) = (x - x_close) / (x_open - x_close)
    ELSE
      fsmc_l(i) = 0.0
    END IF
  END IF

  fsmc_l(i) = MAX(fsmc_l(i),0.0)
  fsmc_l(i) = MIN(fsmc_l(i),1.0)
END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END FUNCTION fsmc_layer

END MODULE smc_ext_mod
