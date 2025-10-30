! *****************************COPYRIGHT*******************************

! (c) [University of Edinburgh]. All rights reserved.
! This routine has been licensed to the Met Office for use and
! distribution under the JULES collaboration agreement, subject
! to the terms and conditions set out therein.
! [Met Office Ref SC237]

! *****************************COPYRIGHT*******************************
!  SUBROUTINE SNOWGRAIN------------------------------------------------

! Description:
!     Calculate growth of snow grains.

! Subroutine Interface:
MODULE snowgrain_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SNOWGRAIN_MOD'

CONTAINS

SUBROUTINE snowgrain ( land_pts, surft_pts, timestep, nsnow,                   &
                       surft_index, sice, snowfall, snowmass, tsnow,           &
                       tstar_surft, rgrain, rgrainl, rgrain0 )

USE conversions_mod, ONLY: rsec_per_hour, pi

USE c_rmol, ONLY:                                                              &
  rmol ! universal gas constant

USE water_constants_mod, ONLY:                                                 &
  rho_ice,                                                                     &
  tm
    ! Temperature at which fresh water freezes and ice melts (K).

USE jules_snow_mod, ONLY:                                                      &
  nsmax,                                                                       &
    ! Maximum possible number of snow layers.
  r0,                                                                          &
    ! Grain size for fresh snow (microns).
  rmax,                                                                        &
    ! Maximum snow grain size (microns).
  snow_ggr,                                                                    &
    ! Snow grain area growth rates (microns**2 s-1).
  i_grain_growth_opt,                                                          &
  ip_grain_growth_marshall89, ip_grain_growth_taillandier_et
    ! Option for rate of growth of snow grains and permitted values.

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Total number of land points.
  surft_pts
    ! Number of tile points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  timestep         ! Timestep (s).

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  nsnow(land_pts),                                                             &
    ! Number of snow layers.
  surft_index(land_pts)
    ! Index of tile points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  sice(land_pts,nsmax),                                                        &
    ! Ice content of snow layers (kg/m2).
  snowfall(land_pts),                                                          &
    ! Total frozen precip fall rate (kg m-2 s-1).
  snowmass(land_pts),                                                          &
    ! Snow mass on ground on tile (kg m-2).
  tsnow(land_pts,nsmax),                                                       &
    ! Snow layer temperatures (K).
  tstar_surft(land_pts)
    ! Tile surface temperature (K).

!-----------------------------------------------------------------------------
! Array arguments with intent(inout)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  rgrain(land_pts),                                                            &
    ! Snow grain size (microns).
  rgrainl(land_pts,nsmax)
    ! Snow grain size in snow layers (microns).

!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  rgrain0(land_pts)
    ! Fresh snow grain size (microns).

!-----------------------------------------------------------------------------
! Local scalars.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i,                                                                           &
    ! Land point index.
  k,                                                                           &
    ! Tile point index.
  n
    ! Snow layer index.

REAL(KIND=real_jlslsm) :: rate      ! Grain area growth rate (microns2/s).

REAL(KIND=real_jlslsm) :: ssa       ! Specific surface area of snow.
REAL(KIND=real_jlslsm) :: ssa_0     ! Specific surface area of snow.
REAL(KIND=real_jlslsm) :: agrain_et ! Coefficient in fit to rate of growth.
REAL(KIND=real_jlslsm) :: bgrain_et ! Coefficient in fit to rate of growth.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SNOWGRAIN'

!-----------------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(SHARED)                                                          &
!$OMP PRIVATE(n,k,i,rate,ssa,ssa_0,agrain_et,bgrain_et)
DO k = 1,surft_pts
  i = surft_index(k)
  !-----------------------------------------------------------------------
  ! Set grain size for fresh snow.
  !-----------------------------------------------------------------------
  rgrain0(i) = r0

  IF ( nsnow(i) == 0 ) THEN
    !-----------------------------------------------------------------------
    ! No snow, or zero-layer model selected.
    !-----------------------------------------------------------------------
    IF ( snowmass(i) > 0.0 ) THEN
      rate = snow_ggr(1)
      IF ( tstar_surft(i) < tm ) THEN
        SELECT CASE (i_grain_growth_opt)
        CASE (ip_grain_growth_marshall89)
          ! Rate of growth following Marshall (1989)
          IF ( rgrain(i) < 150.0 ) THEN
            rate = snow_ggr(2)
          ELSE
            rate = snow_ggr(3) * EXP( -3.7e4 / (rmol * tstar_surft(i)) )
          END IF
        CASE (ip_grain_growth_taillandier_et)
          ! Rate following Taillandier et al. (2007) for ET, noting that
          ! that scheme is in cm^2g^-1 and that the time therein is
          ! measured in hours.
          ssa       = 3.0 / (rho_ice * 1.0e-6 * rgrain(i))
          ssa_0     = 3.0 / (rho_ice * 1.0e-6 * r0)
          agrain_et = 0.00760 * ssa_0 - 0.176 * ( tstar_surft(i) - tm - 2.96)
          bgrain_et = 0.0629 * ssa_0 - 1.50 * ( tstar_surft(i) - tm - 11.2)
          rate      = (1.0e-6 / rsec_per_hour) * rho_ice *                     &
                      (2.0 * pi * agrain_et / 3.0) * rgrain(i)**3 *            &
                      EXP( (ssa - bgrain_et) / agrain_et )
        END SELECT
      END IF
      rgrain(i) = SQRT( rgrain(i)**2 + (rate / pi) * timestep )                &
                    - (rgrain(i) - r0) * snowfall(i) / 2.5
      rgrain(i) = MIN( rmax, rgrain(i) )
      rgrain(i) = MAX( r0, rgrain(i) )
    ELSE
      ! No snow. Set grain size to that for fresh snow (ready for next
      ! occurence).
      rgrain(i) = r0
    END IF  !  snowmass

  ELSE

    !-------------------------------------------------------------------------
    ! nsnow>0: one or more snow layers.
    !-------------------------------------------------------------------------
    DO n = 1,nsnow(i)
      IF ( sice(i,n) > 0 ) THEN
        rate = snow_ggr(1)
        IF ( tsnow(i,n) < tm ) THEN
          SELECT CASE (i_grain_growth_opt)
          CASE (ip_grain_growth_marshall89)
            ! Original scheme following Marshall (1989)
            IF ( rgrainl(i,n) < 150.0 ) THEN
              rate = snow_ggr(2)
            ELSE
              rate = snow_ggr(3) *                                             &
                     EXP( -3.7e4 / (rmol * tsnow(i,n)) )
            END IF
          CASE (ip_grain_growth_taillandier_et)
            ! Rate following Taillandier et al. (2007) for ET, noting that
            !  that scheme is in cm^2g^-1.
            ssa       = 3.0 / (rho_ice * 1.0e-6 * rgrainl(i,n))
            ssa_0     = 3.0 / (rho_ice * 1.0e-6 * r0)
            agrain_et = 0.00760 * ssa_0 - 0.176 * ( tsnow(i,n) - tm - 2.96)
            bgrain_et = 0.0629 * ssa_0 - 1.50 * ( tsnow(i,n) - tm - 11.2)
            rate      = (1.0e-6 / rsec_per_hour) * rho_ice *                   &
                        (2.0 * pi * agrain_et / 3.0) * rgrainl(i,n)**3 *       &
                        EXP( (ssa - bgrain_et) / agrain_et )
          END SELECT
        END IF
        rgrainl(i,n) = SQRT(rgrainl(i,n)**2 + (rate / pi) * timestep)
        rgrainl(i,n) = MIN( rmax, rgrainl(i,n) )
        rgrainl(i,n) = MAX( r0, rgrainl(i,n) )
      END IF
    END DO

    !-----------------------------------------------------------------------
    ! For all currently empty layers, set grain size to that for fresh snow.
    !-----------------------------------------------------------------------
    IF ( nsnow(i) < nsmax ) THEN
      DO n = nsnow(i) + 1,nsmax
        rgrainl(i,n) = rgrain0(i)
      END DO
    END IF

  END IF  !  nsnow

END DO  !  k (tile points)
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE snowgrain
END MODULE snowgrain_mod
