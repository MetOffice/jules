! *****************************COPYRIGHT*******************************

! (c) [University of Edinburgh]. All rights reserved.
! This routine has been licensed to the Met Office for use and
! distribution under the JULES collaboration agreement, subject
! to the terms and conditions set out therein.
! [Met Office Ref SC237]

! *****************************COPYRIGHT*******************************
!  SUBROUTINE SNOWTHERM------------------------------------------------

! Description:
!     Thermal properties of snow layers

! Subroutine Interface:
MODULE snowtherm_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SNOWTHERM_MOD'

CONTAINS

SUBROUTINE snowtherm ( land_pts, surft_pts, nsnow, surft_index, ds,            &
                       sice, sliq, csnow, ksnow )

USE jules_snow_mod, ONLY:                                                      &
  nsmax,                                                                       &
    ! Maximum number of snow layers.
  ip_snow_cond_yen81,                                                          &
    ! Conductivity following Yen (1981).
  ip_snow_cond_calonne11,                                                      &
    ! Conductivity following Calonne et al. (2011).
  i_snow_cond_parm
    ! Scheme selected.

USE water_constants_mod, ONLY:                                                 &
  ! imported scalar parameters
  hcapi,                                                                       &
    ! Specific heat capacity of ice (J/kg/K).
  hcapw,                                                                       &
    ! Specific heat capacity of water (J/kg/K).
  rho_water
    ! Density of water (kg/m3).

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER,INTENT(IN) ::                                                          &
  land_pts,                                                                    &
    ! Number of land points.
  surft_pts
    ! Number of tile points.

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  nsnow(land_pts),                                                             &
    ! Number of snow layers.
 surft_index(land_pts)
    ! Index of tile points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  ds(land_pts,nsmax),                                                          &
    ! Snow layer thicknesses (m).
  sice(land_pts,nsmax),                                                        &
    ! Ice content of snow layers (kg/m2).
  sliq(land_pts,nsmax)
    ! Liquid content of snow layers (kg/m2).

!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  csnow(land_pts,nsmax),                                                       &
    ! Areal heat capacity of layers (J/K/m2).
  ksnow(land_pts,nsmax)
    ! Thermal conductivity of layers (W/m/K).

!-----------------------------------------------------------------------------
! Local scalars
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  k,                                                                           &
    ! Tile point index
  l,                                                                           &
    ! Land point index.
  n
    ! Snow layer index.

REAL(KIND=real_jlslsm) :: rho_snow   ! Snow layer density (kg/m3).

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SNOWTHERM'

!-----------------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Initialise output arrays
!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(l,n)                                                             &
!$OMP SHARED(land_pts,nsmax,csnow,ksnow)
DO n = 1,nsmax
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    csnow(l,n) = 0.0
    ksnow(l,n) = 0.0
  END DO
!$OMP END DO NOWAIT
END DO
!$OMP END PARALLEL

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(k,l,n,rho_snow)                                                  &
!$OMP SHARED(surft_pts,surft_index,nsnow,csnow,sice,sliq,i_snow_cond_parm,     &
!$OMP        ds,ksnow)
DO k = 1,surft_pts
  l = surft_index(k)
  DO n = 1,nsnow(l)
    csnow(l,n) = sice(l,n) * hcapi + sliq(l,n) * hcapw
  END DO
  SELECT CASE (i_snow_cond_parm)
  CASE (ip_snow_cond_yen81)
    DO n = 1,nsnow(l)
      rho_snow   = (sice(l,n) + sliq(l,n)) / ds(l,n)
      ksnow(l,n) = 2.22 * (rho_snow / rho_water)**1.88
    END DO
  CASE (ip_snow_cond_calonne11)
    DO n = 1,nsnow(l)
      rho_snow   = (sice(l,n) + sliq(l,n)) / ds(l,n)
      ksnow(l,n) = 0.024 - 1.23e-4 * rho_snow + 2.5e-6 * rho_snow**2
    END DO
  END SELECT
END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE snowtherm
END MODULE snowtherm_mod
