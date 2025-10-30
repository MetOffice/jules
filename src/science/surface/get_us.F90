!******************************COPYRIGHT***************************************
! (c) Crown copyright, Met Office All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
!******************************COPYRIGHT***************************************
MODULE get_us_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='GET_US_MOD'

CONTAINS

SUBROUTINE get_us( u1, v1, bht, hwr, z0m, z0t, z1_u, disp , u_us, rbulk)

! Description:
!   Calculates the bulk resistance (rbulk) to heat transport for the canyon
!   for three different regimes; isolated flow, wake interference             &
!   skimming flow depending on the geometry of the canyon
!
! -----------------------------------------------------------------------------
! Call with: u1=1.0, v1=0.0
! Note that this suggests that we work here with normalized wind speeds
! z0t   = z0e_h*bht = z0e = ztm = bulk roughness length for momentum
! rbulk = urest_can = total canyon resistance
! z1_u  = z1_uv
! z0m   = material roughness length
! z0t   = effective roughness length for momentum

USE urban_param_mod, ONLY: kappa2

IMPLICIT NONE

! Arguments:
REAL(KIND=real_jlslsm), INTENT (IN) :: u1, v1,                                 &
   bht,           & ! Height of buildings
   hwr,           & ! Canyon aspect ratio
   z0m,           & ! Material roughness length for momentum
   z0t,           & ! Bulk roughness length for momentum
   z1_u,          & ! Height of the level 1 winds
   disp,          & ! Displacement height
   u_us             ! Scaling for wind speed
                    ! (normalised reciprcal of friction velocity)

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
   rbulk            ! Bulk resistance to transport into boundary layer

! Local declarations:
REAL(KIND=real_jlslsm), PARAMETER :: alpha = 0.15
                                ! Exponent of decay of recirculation jet

REAL(KIND=real_jlslsm) ::           & ! Speed of jets along facets
   uu,            & ! Street: upstream
   ud,            & ! Street: downstream
   uuw,           & ! Wall: upstream
   udw,           & ! Wall: downstream
   ut, vt,        & !
   nu,            & ! Alpha increased by fractor nu for skimming flow regime
   rbulk1,        & ! Bulk resistance into recirculation region
   rbulk2           ! Bulk resistance into ventilated region

REAL(KIND=real_jlslsm) :: aloncan1, aloncan2, uct, vct, pi

REAL(KIND=real_jlslsm) ::                                                      &
   wd,                    & ! Width of the canyon
   lt,                    & ! Length of the sloping edge of the recirc region
   hs,                    & ! Length of downstream wall in ventilated region
   lr,                    & ! Length of recirculation region
   var1, var2 ,log_inf

REAL(KIND=real_jlslsm) ::                                                      &
   lrw, ltw, hrh, lttw,  & ! Factors for rescaling resistance over sloping
                           ! edge of recirculation region
   z0h,                  & ! Material roughness length for heat
   scaler6, scaler7,     & ! Scaling factors for resistances
   scaler8, scaler6hat,                                                        &
   r6hat,                & ! Resistance to transport
   zref                    ! Thickness of IBL formed along facets

REAL(KIND=real_jlslsm) ::                                                      &
   r(8)                    ! Resistances to transport

CHARACTER(LEN=*), PARAMETER :: RoutineName='GET_US'

zref = 0.1 * bht

! calculation of LttW factor to rescale the resistance out of the circulation
! region
pi   = 4.0 * ATAN( 1.0 )
lrw  = 3.0 * ( 2.0 * hwr / pi )
ltw  = 1.5 * ( 2.0 * hwr / pi )
hrh  = ( lrw - 1.0 ) / ( lrw - ltw )

lrw  = MIN( lrw, 1.0 )
ltw  = MIN( ltw, 1.0 )
hrh  = MAX( hrh, 0.0 )
hrh  = MIN( hrh, 1.0 )
lttw = lrw + (1.0 - hrh) * SQRT( ( lrw - ltw )**2.0 + hwr**2.0 )

!----------------------------------------------------------------------

! Calculation of wind components assuming orientational averaging
ut  = SQRT( u1**2.0 + v1**2.0 ) * 2.0 / pi
vt  = SQRT( u1**2.0 + v1**2.0 ) * 2.0 / pi

! Interpolation of wind speed to canyon top
log_inf = MAX( bht - disp, z0t )
uct     = ut * LOG( log_inf / z0t ) / LOG( z1_u / z0t + 1.0 )
vct     = vt * LOG( log_inf / z0t ) / LOG( z1_u / z0t + 1.0 )

! Suppress stability correction in comparison with Ian's scheme (UEBBL)
! Add changes for coordinates into UM : +z0t/z0t = +1.
! Do not add changes for LOG ( bht - disp ) because:
! In the new coordinate system, the surface is 0 at disp + z0t.
! When log( (bht - disp) /z0t ) appears in the wind speed interpolation, the
! interpolation is made down to a height X that is equal to bht. We define the
! height X in our new system (above disp + z0t) such that disp + z0t + X should
! also be equal to bht. In these conditions, if we try: bht - d = X + z0t,
! where X is the height in the new coordinate, such that X = bht - disp - z0t.
! The location of X above ground level is then:
!        disp + z0t + X = disp + z0t + ( bht - disp - z0t ) = bht -> correct
! Similarly, no '+1. correction' is applied to LOG( terms not including z1_u )

! Calculation of along-canyon wind componenents
! Note that the material r.l. is used now instead of the effective r.l. z0t,
! because we are near the facets

! Along canyon
aloncan1 = vct *                                                               &
   LOG( zref / z0m ) / LOG( bht / z0m )                        ! Street bit
aloncan2 = vct *                                                               &
   ( 1.0 / (1.0 - ( z0m / bht ) ) - 1.0 / LOG( bht / z0m ) )   ! Wall bit

! No correction of integral in aloncan2 because the integral is already
! defined between H and z0m, and not between H and 0.

!------------------------------------------------------------------------------
! Geometrical factors used in the three circulation regimes
!------------------------------------------------------------------------------
wd = bht / hwr       ! Width of canyon
lr = 3.0 * bht       ! Length of recirculation region

! Length of sloping edge (lt)
! Length of downstream wall in ventilated region (hs)

IF ( hwr <= 1.0 / 3.0 ) THEN
  ! Isolated flow
  lt = SQRT(3.25) * bht
  hs = bht
ELSE IF ( 1.0 / 3.0 < hwr .AND. hwr <= 2.0 / 3.0 ) THEN
  ! Wake interference
  lt = ( ( wd - lr / 2.0 )**2.0 ) * ( ( 2.0 * bht / lr )**2.0 + 1.0 )
  lt = SQRT(lt)
  hs = 2.0 * ( wd - lr / 2.0 ) * bht / lr
ELSE
  ! Skimming flow
  lt = 0.0
  hs = 0.0
END IF
lt = 1.25 * lt

!--------------------------------------------------------------------------

! Across canyon
IF ( hwr <= 1.0 / 3.0 ) THEN
  ! Isolated flow regime

  IF ( lr == wd ) THEN
    lr = lr - 1.0e-5
  END IF

  ! Jet formulation: Ventilated region
  ! Downstream fraction of street
  ud = bht * EXP( -alpha * lt / bht ) *                                        &
     ( 1.0 - EXP( -alpha * ( wd - lr ) / bht ) )
  ud = ud * uct / ( alpha * ( wd - lr ) )
  ! Lower bound of jet speed (min uds)
  var1 = uct * LOG( zref / z0m ) / LOG( bht / z0m )
  ud = MAX( ud, var1 )

  ! Downstream wall
  udw = uct * EXP( -alpha * (lt + wd - lr) / bht ) *                           &
     (1.0 - EXP( -alpha ) ) / alpha
  ! Lower bound of jet speed (min udw)
  var2 = uct *                                                                 &
     ( ( 1.0 / ( 1.0 - ( z0m / bht ) ) ) - ( 1.0 / LOG( bht / z0m ) ) )
  ! No correction again in the integral (see previous comment)
  udw = MAX( udw, var2 )

  ! Jet formulation: Recirculation region
  ! Upstream fraction of street
  uu = uct * bht * EXP( -alpha * lt / bht ) *                                  &
     ( 1.0 - EXP ( -alpha * lr / bht ) )
  uu = uu / ( alpha * lr )

  ! Upstream wall
  uuw = uct * EXP( -alpha * ( lt + lr ) / bht ) *                              &
     ( 1.0 - EXP( -alpha ) )
  uuw = uuw / alpha

ELSE IF ( 1.0 / 3.0 < hwr .AND. hwr < 2.0 / 3.0) THEN
  ! Wake interference

  IF ( hs == bht ) THEN
    hs = hs - 1.0e-5
  END IF

  ! Jet formulation: Ventilated region
  ! Downstream fraction of wall
  udw = uct * bht * EXP( -alpha * lt / bht ) *                                 &
     ( 1.0 - EXP( -alpha * hs / bht ) )
  udw = udw / ( alpha * hs )
  ! Lower bound of jet speed (min udw)
  log_inf = MAX( ( bht - hs ), z0m )
  var2 = uct * ( bht / hs - 1.0 / LOG( bht / z0m ) -                           &
     ( bht - hs ) * LOG( log_inf / z0m ) / ( hs * LOG( bht / z0m ) ) )
  ! No correction again in the integral (see previous comment)
  udw = MAX( udw, var2 )

  ! Jet formulation: Recirculation region
  ! Downstream fraction of wall
  ! ud itself not important for this regime but is used in calculating udw
  ud = bht * EXP( -alpha * lt / bht ) *                                        &
     ( 1.0 - EXP( -alpha * ( bht - hs ) / bht ) )
  ud = ud * uct / ( alpha * ( bht - hs ) )

  ! Total downstream wall
  ! (weighted average of ventilated & recirculation regions)
  udw = ( hs * udw + ( bht - hs ) * ud ) / bht

  ! Upstream street
  uu = uct * bht * EXP( -alpha * ( lt + bht - hs ) / bht ) *                   &
     ( 1.0 - EXP( -alpha * wd / bht ) )
  uu = uu / ( alpha * wd )

  ! Upstream wall
  uuw = uct * EXP( -alpha * ( lt + bht - hs + wd ) / bht ) *                   &
     ( 1.0 - EXP( -alpha ) )
  uuw = uuw / alpha

ELSE
  ! Skimming flow

  nu = lr / ( 2.0 * wd ) ! alpha is increased by factor nu: jet slows more

  ! Downstream wall
  udw = uct * ( 1.0 - EXP( -alpha ) ) / alpha
  ud = udw ! ud not important for this regime: done for numerical reasons

  ! Upstream steet
  uu = uct * bht * EXP( -nu * alpha ) *                                        &
     ( 1.0 - EXP( -nu * alpha * wd / bht ) )
  uu = uu / ( nu * alpha * wd )

  ! Upstream wall
  uuw = uct * EXP( -nu * alpha  * ( bht + wd ) / bht ) *                       &
     ( 1.0 - EXP( -nu * alpha ) )
  uuw = uuw / ( nu * alpha )

END IF

!------------------------------------------------------------------------------
! Add along-canyon components
!------------------------------------------------------------------------------

uu  = SQRT( uu**2.0  + aloncan1**2.0 ) ! Street
ud  = SQRT( ud**2.0  + aloncan1**2.0 )
uuw = SQRT( uuw**2.0 + aloncan2**2.0 ) ! Wall
udw = SQRT( udw**2.0 + aloncan2**2.0 )

!------------------------------------------------------------------------------
! Calculate resistances
!------------------------------------------------------------------------------

! OUT of recirculation region: tranport across a shear layer
r(5) =  ( 1.0 - uu ) * u_us**2.0
! uu is chosen instead of uuw because the street level represents the shear
! better than the wall level
! Scaling factor introduced from get_BETA from UEBBL in order to represent
! the heat transfer across the sloping edge of the recirculation region
r(5) = r(5) / ( 1.0 + lttw - ltw )


! Transport over an internal boundary layer

z0h = 0.1 * z0m

! Numerator of resistance same for the transport across an internal BL.
r(8) = LOG( zref / z0m ) * LOG( zref / z0h )

! Off upstream wall
r(3) = r(8) / ( uuw * kappa2 )

! Off upstream street
r(4) = r(8) / ( uu  * kappa2 )

! Off downstream wall
r(6) = r(8) / ( udw * kappa2 )

! Off downstream street
r(8) = r(8) / ( ud  * kappa2 )


! OUT of ventilated region: tranport across a shear layer
r(7) = ( 1.0 - udw ) * u_us**2.0
!udw is chosen instead of ud because
!ud is not significant in the 2nd regime

!------------------------------------------------------------------------------
! Scaling of resistances
!------------------------------------------------------------------------------

r(3) = r(3) / hwr
r(4) = r(4) / ( MIN( lr,     wd ) / wd )
r(5) = r(5) / ( MIN( lr / 2.0, wd ) / wd )

! The 1e-3 is included for numerical reasons

scaler6hat = MAX( 1.0e-3, ( bht - hs ) / wd ) ! Recirculation pathway (rbulk1)
scaler6    = MAX( 1.0e-3,   hs         / wd ) ! Ventilation pathway   (rbulk2)
scaler7    = MAX( 1.0e-3, ( wd - MIN( lr / 2.0, wd ) ) / wd )
scaler8    = MAX( 1.0e-3, ( wd - MIN( lr    , wd ) ) / wd )

r6hat = r(6) / scaler6hat
r(6)  = r(6) / scaler6
r(7)  = r(7) / scaler7
r(8)  = r(8) / scaler8

! Explanation
!
! For H/W > 1/3, skimming flow, all of the transfer from the street goes into
! the recirculation region. r(8) represents the resistance from the downstream
! street to the ventilated region. So, for H/W > 1/3, scaler8 == 0, so r(8)
! ( = r(8) / scaler8 ) should be infinite.
!
! For 1/3 <H/W < 2/3, wake interference, some of the transfer from the
! downstream wall goes out through the recirculation region and some out
! through the ventilated region. r(6) represents the resistance from the
! downstream wall into either the recirculation region (rbulk1) or the
! ventilated region (rbulk2). Hence, we define two different scaling factors:
! scaler6 and scaler6hat
!
! In the isolated flow regime, hs == bht, so scaler6 != 0., so contribution
! from downstream wall is into ventilated area (rbulk2). scaler6hat == 0, so
! r6hat is infinite, so no contribution downstream wall in recirculation region
! (rbulk1)
!
! In the skimming flow regime, Hs == 0, so scaler6hat != 0, so contribution
! from downstream wall into recirculation region (rbulk1).
! But scaler6 == 0, so r(6) is infinite, so no contribution downstream wall in
! ventilated region (rbulk2)
!
! In the wake interference regime, both r(6) and r6hat contribute to the
! resistance network.


! calculation of total resistance for recirculation region
rbulk1 = ( r(3) * r(4) * r6hat ) /                                             &
   ( ( r(3) * r(4) ) + ( r(3) * r6hat ) + ( r(4) * r6hat ) )
rbulk1 = rbulk1 + r(5)

! calculation of total resistance for ventilated region
rbulk2 = ( r(6) * r(8) ) / ( r(6) + r(8) )
rbulk2 = rbulk2 + r(7)

! calculation to total resistance for canyon tile
rbulk = ( 1.0 / rbulk1 ) + ( 1.0 / rbulk2 )
rbulk = 1.0 / rbulk
RETURN

END SUBROUTINE get_us
END MODULE get_us_mod
