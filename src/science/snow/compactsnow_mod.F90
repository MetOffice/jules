! *****************************COPYRIGHT*******************************

! (c) [University of Edinburgh]. All rights reserved.
! This routine has been licensed to the Met Office for use and
! distribution under the JULES collaboration agreement, subject
! to the terms and conditions set out therein.
! [Met Office Ref SC237]

! *****************************COPYRIGHT*******************************
!  SUBROUTINE COMPACTSNOW-----------------------------------------------

! Description:
!     Mechanical compaction of snow

! Subroutine Interface:
MODULE compactsnow_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='COMPACTSNOW_MOD'

CONTAINS

SUBROUTINE compactsnow ( land_pts, surft_pts, timestep, nsnow,                 &
                         surft_index, sice,sliq, tsnow, rho_snow, ds )

USE water_constants_mod, ONLY:                                                 &
  rho_ice,                                                                     &
  tm
    ! Melting temperature of ice (K).

USE jules_snow_mod, ONLY:                                                      &
  ! imported scalars
  nsmax,                                                                       &
    !  Maximum possible number of snow layers
  l_et_metamorph,                                                              &
    !  Switch to include equitemperature metamorphism
  a_snow_et,                                                                   &
  b_snow_et,                                                                   &
  c_snow_et,                                                                   &
    !  Parameters for rate of ET metamorphism
  rho_snow_et_crit
    !  Critical density for ET metamorphism

USE planet_constants_mod, ONLY:                                                &
  ! imported scalar parameters
  g
    !  Mean acceleration due to gravity at earth's surface (m s-2).

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
  timestep
    ! Timestep (s).

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  nsnow(land_pts),                                                             &
    ! Number of snow layers.
  surft_index(land_pts)
    ! Index of tile points

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  sice(land_pts,nsmax),                                                        &
    ! Ice content of snow layers (kg/m2).
  sliq(land_pts,nsmax),                                                        &
    ! Liquid content of snow layers (kg/m2).
  tsnow(land_pts,nsmax)
    ! Snow layer temperatures (K).

!-----------------------------------------------------------------------------
! Array arguments with intent(inout)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  rho_snow(land_pts,nsmax)  ! Snow layer densities (kg/m3).

!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  ds(land_pts,nsmax)        ! Snow layer thicknesses (m).

!-----------------------------------------------------------------------------
! Local scalars
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  k,                                                                           &
    ! Tile point index.
  l,                                                                           &
    ! Land point index.
  n
    ! Snow layer index.

REAL(KIND=real_jlslsm) ::                                                      &
  mass,                                                                        &
    ! Overlying mass of snow (kg/m2).
  rho
    ! Snow density (kg/m3).

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='COMPACTSNOW'

!-----------------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(k,l,mass,rho,n)                                                  &
!$OMP SHARED(surft_pts,surft_index,ds,nsnow,rho_snow,timestep,tsnow,           &
!$OMP        l_et_metamorph,sice,sliq,g,rho_snow_et_crit,a_snow_et,b_snow_et,  &
!$OMP        c_snow_et,nsmax)
DO k = 1,surft_pts
  l = surft_index(k)

  mass    = 0.0

  !Reset for all possible layers
  DO n = 1,nsmax
    ds(l,n) = 0.0
  END DO

  DO n = 1,nsnow(l)
    mass = mass + 0.5 * (sice(l,n) + sliq(l,n))
    rho  = rho_snow(l,n)
    rho  = rho + 0.5e-7 * rho * g * mass * timestep *                          &
                EXP(14.643-4.0e3 / tsnow(l,n) - 0.02 * rho)
    IF (l_et_metamorph)                                                        &
      rho = rho + rho * timestep * a_snow_et *                                 &
                  EXP(-b_snow_et * (tm - tsnow(l,n)) -                         &
                      c_snow_et * MAX(0.0, rho - rho_snow_et_crit))
    ! Do not allow the density to rise above that of solid ice.
    rho = MIN(rho, rho_ice)
    ! Note: mass (and hence rho) can be zero but nsnow>0 (likely 1!) if a very
    ! shallow snowpack has been exhausted in this timestep.
    IF ( rho > EPSILON(rho) )                                                  &
      ds(l,n)     = (sice(l,n) + sliq(l,n)) / rho
    rho_snow(l,n) = rho
    mass          = mass + 0.5 * (sice(l,n) + sliq(l,n))
  END DO
END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE compactsnow
END MODULE compactsnow_mod
