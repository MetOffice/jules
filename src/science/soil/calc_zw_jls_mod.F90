#if !defined(RECON)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!   Subroutine CALC_ZW-------------------------------------------------

!   Purpose: To calculate the mean water table depth from the soil
!            moisture deficit as described in Koster et al., 2000.,
!            using the Newton-Raphson method.

! Documentation: UNIFIED MODEL DOCUMENTATION PAPER NO 25

MODULE calc_zw_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='CALC_ZW_MOD'

CONTAINS

SUBROUTINE calc_zw(npnts, nshyd, soil_pts, soil_index,                         &
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
    ! Clapp-Hornberger exponent.
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
! Array arguments with intent(IN OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  zw(npnts)
    ! Water table depth (m).

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
INTEGER, PARAMETER :: niter = 3
    ! Number of iterations.

!-----------------------------------------------------------------------------
! Local scalar variables:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, j, n, it, nn, nzw, nu, niteruse
    !Loop counters

REAL(KIND=real_jlslsm) ::                                                      &
  zw_old,                                                                      &
    !zw from last timestep/iteration.
  fn,                                                                          &
    !fn calc in Newton-Raphson iteration.
  dfn,                                                                         &
    !derivative of fn.
  fnnew,                                                                       &
    !used when Newton-Raphson iteration cannot be applied as dfn is too small
  zwest,                                                                       &
  zdeep,                                                                       &
  smd,                                                                         &
    !soil moisture deficit
  zdepth(0:nshyd+1),                                                           &
    ! Lower soil layer boundary depth (m).
  psisat(nshyd)
    !Saturated soil water pressure (m) (negative).

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CALC_ZW'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

zdepth(:) = 0.0
DO n=1,nshyd
  zdepth(n) = zdepth(n-1) + dzsoil(n)
END DO
zdepth(nshyd+1) = zw_max

!-----------------------------------------------------------------------------
!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(j,i,zw_old,smd,psisat,zwest,it,fn,dfn,n,nn,nzw,nu,niteruse,fnnew)&
!$OMP SHARED(soil_pts,soil_index,zw,smclsat,smcl,smclsatzw,smclzw,sathh,bexp,  &
!$OMP        v_sat,nshyd,zw_max,zdepth,zdeep)
DO j = 1,soil_pts
  i = soil_index(j)
  zw_old = zw(i)

  !---------------------------------------------------------------------------
  ! Calculate total soil moisture deficit:
  !---------------------------------------------------------------------------
  smd = 0.0
  DO n = 1,nshyd
    smd = smd + ((smclsat(i,n) - smcl(i,n)) / rho_water)
  END DO

  smd = smd + ((smclsatzw(i) - smclzw(i)) / rho_water)

  !---------------------------------------------------------------------------
  ! Estimate water table depth assuming an equilibrium profile
  ! above water table (i.e. pressure head gradient and gravity balance):
  !---------------------------------------------------------------------------
  psisat(:) = -sathh(i,:)
  zwest  = zw_old

  ! Allow for extra iterations if previous water table is at the surface as
  ! dfn is likely to be too small on the first step for Newton-Raphson
  ! iteration to be accurately carried out:
  ! on first time step:
  niteruse = niter
  IF (zwest < EPSILON(zwest)) THEN
    niteruse = niter + 5
  END IF

  DO it = 1,niteruse
    IF (zwest <  0.0) THEN
      zwest = 0.0
    END IF

    IF (zwest >  zw_max) THEN
      zwest = zw_max
    END IF

    zw_old = zwest
    zwest  = zw_old

    nzw = nshyd+1
    DO nn = nshyd+1,1,-1
      IF (zwest <= zdepth(nn) .AND. nn < nzw) THEN
        nzw = nn
      END IF
    END DO

    ! Newton-Raphson. zw(next)=zw-f(zw)/f'(zw).
    ! Intend for vertically varying soil properties:
    fn = - smd
    DO nn = nzw,1,-1
      ! soil properties have values defined in layers 1-nshyd
      nu = MIN(nshyd,nn)
      IF (nn == nzw) THEN
        zdeep = zwest
      ELSE
        zdeep = zdepth(nn)
      END IF

      fn = fn + ( zdeep - zdepth(nn-1) ) * v_sat(i,nu)                         &
         - v_sat(i,nu) * bexp(i,nu) / (bexp(i,nu) - 1.0) * psisat(nu)          &
         * ( ( 1.0 - zdepth(nn-1) / psisat(nu) )**( 1.0 - 1.0 / bexp(i,nu) )   &
         - ( 1.0 - zdeep / psisat(nu) )**( 1.0 - 1.0 / bexp(i,nu) )            &
         )         !   f(zw)
    END DO
    nu = MIN(nshyd,nzw)
    fnnew = fn + smd - zwest * v_sat(i,nu)

    dfn =  v_sat(i,nu) - v_sat(i,nu) * ( 1.0 - zwest / psisat(nu) )**          &
           ( -1.0 / bexp(i,nu ) )

    IF (ABS(dfn) >  EPSILON(dfn)) THEN
      zwest = zwest - fn / dfn
    ELSE
      ! Assume estimate is correct from equations and so do not need to iterate:
      zwest = ( smd - fnnew ) / v_sat(i,nu)
    END IF

    IF (zwest < 0) THEN
      zwest = 0.0
    END IF

    IF (zwest > zw_max) THEN
      zwest = zw_max
    END IF
  END DO  !  iterations

  zw(i) = zwest

  IF (zw(i) <  0) THEN
    zw(i) = 0.0
  END IF

  IF (zw(i) >  zw_max) THEN
    zw(i) = zw_max
  END IF

END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE calc_zw
END MODULE calc_zw_mod
#endif
