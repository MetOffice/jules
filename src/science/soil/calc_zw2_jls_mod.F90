#if !defined(RECON)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!   Subroutine CALC_ZW2------------------------------------------------

!   Purpose: To calculate the mean water table depth from the soil
!            moisture deficit. This is based on Koster et al., 2000.,
!            using the Newton-Raphson method. However it estimates a
!            water table for each soil layer by considering the soil
!            moisture deficit up to that soil layer. These separate
!            estimates are merged. This results in a generally higher
!            water table and limits the impact of root zone abstraction
!            limiting the assumption in Koster et al., 2000,
!            i.e. the assumption that soil moisture profile above
!            the water table is determined from the balance between
!            pressure head gradient and gravity.

! Documentation: UNIFIED MODEL DOCUMENTATION PAPER NO 25

MODULE calc_zw2_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='CALC_ZW2_MOD'

CONTAINS

SUBROUTINE calc_zw2(npnts, nshyd, soil_pts, soil_index,                        &
                   bexp, sathh, smcl, smclzw, smclsat, smclsatzw, v_sat, zw)

USE water_constants_mod,  ONLY: rho_water
USE jules_hydrology_mod,  ONLY: zw_max
USE jules_soil_mod,       ONLY: dzsoil

USE parkind1,             ONLY: jprb, jpim
USE yomhook,              ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!Subroutine arguments
!-----------------------------------------------------------------------------
! Scalar arguments with intent(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  npnts,                                                                       &
    ! Number of gridpoints.
  nshyd,                                                                       &
    ! Number of soil moisture levels.
  soil_pts
    ! Number of soil points.

!-----------------------------------------------------------------------------
! Array arguments with intent(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  soil_index(npnts)
    ! Array of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  bexp(npnts,nshyd),                                                           &
    ! Clapp-Hornberger or Brooks & Corey exponent.
  sathh(npnts,nshyd),                                                          &
    ! Saturated soil water pressure (m).
  smcl(npnts,nshyd),                                                           &
    ! Total soil moisture contents of each layer (kg/m2).
  smclsat(npnts,nshyd),                                                        &
    ! Soil moisture contents of each layer at saturation (kg/m2).
  smclzw(npnts),                                                               &
    ! Moisture content in deep layer (kg/m2).
  smclsatzw(npnts),                                                            &
    ! Moisture content in deep layer at saturation (kg/m2).
  v_sat(npnts,nshyd)
    ! Volumetric soil moisture concentration at saturation (m3 H2O/m3 soil).

!-----------------------------------------------------------------------------
! Array arguments with intent(INOUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  zw(npnts)
    ! Water table depth (m).

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
INTEGER, PARAMETER :: niter = 10
    ! Number of iterations.

!-----------------------------------------------------------------------------
! Local scalar variables:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, j, n, it, nn, nnd, nu,                                                    &
    !Loop counters
  nzw ! soil layer with water table in

REAL(KIND=real_jlslsm) ::                                                      &
  zw_l_min,                                                                    &
    !zw from last timestep/iteration.
  wgt_tot,                                                                     &
    ! weighting used to calculate overall water table depth.
  dwgt_tot,                                                                    &
    ! weighting used to calculate overall water table depth.
  fn,                                                                          &
    !fn calc in Newton-Raphson iteration.
  dfn,                                                                         &
    !derivative of fn.
  fnnew,                                                                       &
    !used when Newton-Raphson iteration cannot be applied as dfn is too small
  zwest,                                                                       &
    ! estimated water table depth in iteration
  zwestnn,                                                                     &
    ! estimated water table depth in iteration upto a specific soil layer
  smd,                                                                         &
    !soil moisture deficit to top of soil layer
  sm
    !soil moisture column to top of soil layer

!-----------------------------------------------------------------------------
! Local array variables:
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  wgt(nshyd+1),                                                                &
    ! weighting to estimate water table from individual layer estimates
  zw_l(npnts,nshyd+1),                                                         &
    ! water table estimate for each layer
  zdepth(0:nshyd+1),                                                           &
    ! Lower soil layer boundary depth (m).
  psisat(nshyd)
    !Saturated soil water pressure (m) (negative).

!-----------------------------------------------------------------------------
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CALC_ZW2'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

zdepth(:) = 0.0
DO n=1,nshyd
  zdepth(n) = zdepth(n-1)+dzsoil(n)
END DO
zdepth(nshyd+1) = zw_max

!-----------------------------------------------------------------------------
!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(j,i,smd,sm,psisat,zwest,it,fn,dfn,fnnew,nu,nn,zwestnn,           &
!$OMP         zw_l,zw_l_min,wgt_tot,wgt,dwgt_tot,nzw,nnd,n)                    &
!$OMP SHARED(soil_pts,soil_index,zw,smclsat,smcl,smclsatzw,smclzw,sathh,bexp,  &
!$OMP        v_sat,nshyd,zw_max,zdepth)

DO j = 1,soil_pts
  i = soil_index(j)

  !---------------------------------------------------------------------------
  ! For each layer (n) estimate water table depth assuming an equilibrium
  ! profile from above water table to the top of that layer
  ! (equilibrium profile is when pressure head gradient and gravity balance):
  !---------------------------------------------------------------------------

  DO n = nshyd+1,1,-1
    !-------------------------------------------------------------------------
    ! Calculate total soil moisture deficit to the top of the current layer:
    !-------------------------------------------------------------------------
    smd = ((smclsatzw(i) - smclzw(i)) / rho_water)
    sm = (smclsatzw(i) / rho_water)
    DO nn = n,nshyd
      smd = smd + ((smclsat(i,nn) - smcl(i,nn)) / rho_water)
      sm = sm + smclsat(i,nn) / rho_water
    END DO
    smd = MAX(smd,0.0)

    psisat(:) = -sathh(i,:)
    zwest = zw(i)

    DO it = 1,niter

      !-----------------------------------------------------------------------
      ! Find layer with current water table estimate in it:
      !-----------------------------------------------------------------------
      nzw = nshyd+1
      DO nn = nshyd+1,1,-1
        IF (zwest <= zdepth(nn) .AND. nn < nzw) THEN
          nzw = nn
        END IF
      END DO

      nzw = MAX(n,nzw)
      zwest = MAX(zwest,zdepth(n-1))

      !-----------------------------------------------------------------------
      ! Newton-Raphson. zw(next)=zw-f(zw)/f'(zw).
      !-----------------------------------------------------------------------
      fn =  - smd
      DO nn = n,nzw
        nnd = MIN(nn,nshyd) ! The deep LSH layer has same soil params as nshyd
        IF (nn < nzw) THEN
          zwestnn = zdepth(nn)
        ELSE
          zwestnn = zwest
          dfn =  v_sat(i,nnd) - v_sat(i,nnd) *                                 &
         ( 1.0 - zwestnn / psisat(nnd) )** (-1.0 / bexp(i,nnd))
        END IF
        fn = fn + (zwestnn - zdepth(nn-1)) * v_sat(i,nnd)                      &
             - v_sat(i,nnd) * bexp(i,nnd) / (bexp(i,nnd) - 1.0) * psisat(nnd)  &
             * ( (1.0 - zdepth(nn-1) / psisat(nnd) )**                         &
             ( 1.0 - 1.0 / bexp(i,nnd) )                                       &
             - (1.0 - zwestnn / psisat(nnd) )**                                &
             ( 1.0 - 1.0 / bexp(i,nnd) ) )       !   f(zw)
      END DO

      IF (ABS(dfn) > EPSILON(dfn)) THEN
        zwest = zwest - fn / dfn
      ELSE
        ! Else assume estimate correct from equation. So no need to iterate:
        nu = MIN(nshyd,nzw)
        fnnew = fn + smd - zwest* v_sat(i,nu)
        zwest = (smd-fnnew)/v_sat(i,nu)
      END IF

      IF (zwest < 0.0) THEN
        zwest = 0.0
      END IF
      IF (zwest >  zw_max) THEN
        zwest = zw_max
      END IF
    END DO  !  iterations

    zw_l(i,n) = MAX(zwest,zdepth(n-1))

  END DO ! layers

  ! find layer with water table depth for smallest zw_l estimate:
  zw_l_min = zw_max
  DO n=1,nshyd+1
    zw_l_min = MIN(zw_l_min,zw_l(i,n))
  END DO

  nzw = nshyd+1
  DO n = nshyd+1,1,-1
    IF (zw_l(i,n) == zw_l_min) THEN
      nzw = n
    END IF
  END DO
  nzw = MAX(1,nzw)

  zw(i)=0.0
  wgt_tot=0.0
  wgt(:)=0.0

  !---------------------------------------------------------------------------
  ! Assume weighting based on how close middle of that layer is nearest the
  ! highest water table estimate and apply to the current nzw layer and layer
  ! above:
  !---------------------------------------------------------------------------
  DO n=1,nshyd+1
    IF (n >= nzw-2 .AND. n <= nzw+1) THEN
      wgt(n) = ABS(0.5*(zdepth(nzw-1)+zdepth(nzw))-zw_l_min)
      wgt(n) = MAX(wgt(n),0.01) ! limit so that does not get zw stuck
      wgt_tot=wgt_tot+wgt(n)
    END IF
  END DO

  dwgt_tot=0.0
  DO n=1,nshyd+1
    IF (n >= nzw-2 .AND. n <= nzw+1) THEN
      dwgt_tot=dwgt_tot+(wgt_tot-wgt(n))
      zw(i) = zw(i) + (wgt_tot-wgt(n))*zw_l(i,n)
    END IF
  END DO

  IF (nzw == 1) THEN
    zw(i) = zw_l(i,1)
  ELSE
    zw(i)=zw(i)/dwgt_tot
  END IF

  ! scheme is designed to stop the over-estimate of water table depth
  ! therefore ensure new scheme does not set a deeper water table:
  IF (zw(i) > zw_l(i,1)) THEN
    zw(i) = zw_l(i,1)
  END IF

  IF (zw(i) >  zw_max) THEN
    zw(i) = zw_max
  END IF

END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE calc_zw2
END MODULE calc_zw2_mod
#endif

