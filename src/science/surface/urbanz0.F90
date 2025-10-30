!******************************COPYRIGHT***************************************
! (c) Crown copyright, Met Office All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
!******************************COPYRIGHT***************************************

MODULE urbanz0_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='URBANZ0_MOD'

CONTAINS

SUBROUTINE urbanz0( n, z1_uv, z1_tq, hgt, hwr, disp, z0m, ztm, zth )

! Description:
!   Calculates the effective roughness lengths for the urban tiles.
!   Note that the meaning of the input z0m for urban areas has changed
!   * z0m in the argument list is the material roughness length
!   * z0m_surft passed into this routine should be ztm, previously set in sparm

USE urban_param_mod, ONLY: kappa2
USE jules_surface_types_mod, ONLY: urban_canyon, urban_roof
USE get_us_mod, ONLY: get_us

USE um_types, ONLY: real_64

USE jules_print_mgr, ONLY:                                                     &
  jules_message,                                                               &
  jules_print,                                                                 &
  PrNorm
IMPLICIT NONE

! This routine considers that wind speeds tend to 0 when z is equal to d+z0
! this suggests that the interpolation log functions are
! written as log((z+z0)/z0) or log((z+ztm)/ztm)
! for respectively transfer from surface to internal
! boundary-layer and from internal to 1st atm. layer

  ! Subroutine arguments with intent(in)
INTEGER, INTENT(IN) ::                                                         &
   n                   !index of tile

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
   z1_uv,            & ! Height of the level 1 winds
   z1_tq,            & ! Height of the level 1 t-q
   hgt,              & ! Height of buildings at each land point
   hwr,              & ! Canyon aspect ratio at each land point
   disp,             & ! Displacement height
   z0m,              & ! Material r.l. for momentum
                       ! This z0m is unaffected by snow and used
                       ! only on walls
   ztm                 ! Bulk r.l. for momentum

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
   zth                 ! Bulk r.l. for heat

!Work variables - scalars
INTEGER, SAVE ::     & ! Looping variables
   callnum = 0         ! Call number

REAL(KIND=real_jlslsm) ::                                                      &
!     ures1_can,       & ! Resistances to transport in canyon
!     ures2_can,                                                              &
!     ures3_can,                                                              &
     ures1_roof,      & ! Resistances to transport in roof
     ures3_roof,                                                               &
     urest_can,       & ! Bulk resistance to transport into bl
     urest_roof,                                                               &
     u_us,            & ! Scaling for wind speed
                        ! (normalised reciprcal of friction velocity)
!    fzct,                                                                    &
     fz1_roof,                                                                 &
!    fz1_road,                                                                &
!    fz2_wall,                                                                &
     zref,            & ! Thickness of IBL formed along facets (in get_us also)
     roof_log           ! Precision for log infinity
!    canyon_log         ! Precision for log infinity

REAL ( KIND=real_64 ) ::  & ! Do need the double precision (Aurore Porson)
   zth_h                     ! Normalised effective r.l. for total heat

CHARACTER(LEN=*), PARAMETER :: RoutineName='URBANZ0'

! May need z0h_z0m = 0.1 as well

!------------------------------------------------------------------------------

IF ( z0m - (1.0e-6) <= ztm ) THEN ! i.e. change if increases

  zref = 0.1 * hgt

  u_us = LOG( z1_uv / ztm + 1.0 ) / SQRT( kappa2 )

  IF ( n == urban_roof ) THEN

    roof_log = MAX( ( 1.1 * hgt - disp ), ztm )
    fz1_roof = LOG( roof_log / ztm ) / LOG( z1_uv / ztm + 1.0)

    ures1_roof = LOG( zref / z0m + 1.0 ) *                                     &
       LOG( ( zref + z0m ) / ( 0.1 * z0m ) )
    ures1_roof = ures1_roof / ( kappa2 * fz1_roof )
    ures3_roof = ( 1.0 - fz1_roof ) * ( u_us**2.0 )

    urest_roof = ures1_roof + ures3_roof

    zth_h = kappa2 * urest_roof / LOG( z1_uv / ztm + 1.0 )
    zth_h = ( z1_tq + ztm ) / ( EXP( zth_h ) * hgt )

    zth = zth_h * hgt
    zth = MAX( zth, 1.0e-30 )
    IF ( hwr < (1.0 / 3.0) ) THEN
      zth = MIN( zth, 0.1 * z0m )
    END IF

  ELSE IF ( n == urban_canyon ) THEN

    CALL get_us( 1.0, 0.0, hgt, hwr, z0m, ztm, z1_uv, disp,                    &
       u_us, urest_can)

    zth_h = kappa2 * urest_can / LOG( z1_uv / ztm + 1.0)
    zth_h = ( z1_tq + ztm ) / (hgt * EXP(zth_h) )

    zth = zth_h * hgt
    zth = MAX( zth, 1.0e-30 )
    IF ( hwr < (1.0 / 3.0) ) THEN
      zth = MIN( zth, 0.1 * z0m )
    END IF

  ELSE
    CALL jules_print('urbanz0',                                                &
        'WARNING: Call to urbanz0 occuring not just for urban tiles',          &
        level = PrNorm)
  END IF

  !------------------------------------------------------------------------------
  ! Print warning to screen on first calling
  !------------------------------------------------------------------------------
  ! This section should only be executed by one thread.
!$OMP MASTER
  IF ( callnum == 0 ) THEN

    CALL jules_print('urbanz0',                                                &
        'MORUSES : Altered roughness lengths for heat for urban tiles',        &
        level = PrNorm)

    callnum = 1
  END IF
!$OMP END MASTER

ELSE

  CALL jules_print('urbanz0',                                                  &
      "ERROR - wrong calling of resistance network",level = PrNorm)
  WRITE(jules_message,*) 'z0m', z0m, 'ztm', ztm
  CALL jules_print('urbanz0',jules_message,level = PrNorm)
  RETURN
END IF

RETURN

END SUBROUTINE urbanz0
END MODULE urbanz0_mod
