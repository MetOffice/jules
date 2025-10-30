! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    SUBROUTINE ICE_HTC------------------------------------------------

! Description:
!     Updates deep soil temperatures for ice. No external subroutines
!     are called.

! Documentation : UM Documentation Paper 25

MODULE ice_htc_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='ICE_HTC_MOD'

CONTAINS

SUBROUTINE ice_htc ( lice_pts, npnts, nshyd, timestep, lice_index, dz,         &
                     surf_ht_flux, tsoil )

!Use in relevant subroutines
USE gauss_mod,          ONLY: gauss

!Use in relevant variables
USE jules_snow_mod,     ONLY: snow_hcap,snow_hcon
USE jules_surface_mod,  ONLY: l_land_ice_imp
USE jules_soil_mod,     ONLY: gamma_t

USE parkind1,           ONLY: jprb, jpim
USE yomhook,            ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  lice_pts,                                                                    &
    ! Number of land ice points.
  npnts,                                                                       &
    ! Number of gridpoints.
  nshyd
    ! Number of soil moisture levels.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  timestep
    ! Model timestep (s).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  lice_index(npnts)
    ! Array of ice points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  dz(nshyd),                                                                   &
    ! Thicknesses of the soil layers (m).
  surf_ht_flux(npnts)
    ! Net downward surface heat flux (W/m2).

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  tsoil(npnts,nshyd)
    ! Sub-surface temperatures (K).

!-----------------------------------------------------------------------------
! Local parameters
!-----------------------------------------------------------------------------
LOGICAL, PARAMETER :: use_lims_gauss = .TRUE.
! Whether to apply the dsthumin and dsthumax limits in the Gauss solver.

!-----------------------------------------------------------------------------
! Local scalar variables
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, j, n, l
    ! Loop counters.

REAL(KIND=real_jlslsm) ::                                                      &
  gamcon
    ! Forward timestep weighting constant.

!-----------------------------------------------------------------------------
! Local array variables
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  h_flux(npnts,0:nshyd)
    ! The fluxes of heat between layers (W/m2).

! Variables required for the implicit calculation.
REAL(KIND=real_jlslsm) ::                                                      &
  dhflux_dtsl1(npnts,0:nshyd),                                                 &
    ! Rate of change of the explicit downward flux at the base of the layer
    ! with the temperature of the layer (W/m2/K).
  dhflux_dtsl2(npnts,0:nshyd),                                                 &
    ! Rate of change of the explicit downward flux at the base of the layer
    ! with the temperature of the lower layer (W/m2/K).
  a(npnts,nshyd),b(npnts,nshyd),c(npnts,nshyd),d(npnts,nshyd),                 &
    ! Matrix elements.
  dtsoil(npnts,nshyd),                                                         &
    ! The increment to the ice temperature (K/timestep).
  dtsoilmax(npnts,nshyd),                                                      &
    ! Maximum allowed increment to soil temperature (K/timestep).
  dtsoilmin(npnts,nshyd)
    ! Minimum allowed increment to soil temperature (K/timestep).


INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='ICE_HTC'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------

IF (l_land_ice_imp) THEN

  ! Implicit scheme modelled on soil_htc
  !---------------------------------------------------------------------------
  ! Calculate heat fluxes across layer boundaries and the derivatives
  ! of the fluxes with respect to the temperatures.
  !---------------------------------------------------------------------------

!$OMP PARALLEL IF(lice_pts > 1) DEFAULT(NONE) PRIVATE(n,j,i,l,gamcon)          &
!$OMP SHARED(nshyd,lice_pts,lice_index,h_flux,snow_hcon,tsoil,dz,              &
!$OMP        dhflux_dtsl1,dhflux_dtsl2,surf_ht_flux,timestep,snow_hcap,a,b,c,  &
!$OMP        d,npnts,dtsoilmin,dtsoilmax)
  DO n = 1,nshyd-1
!$OMP DO SCHEDULE(STATIC)
    DO j = 1,lice_pts
      i = lice_index(j)
      h_flux(i,n)       = -snow_hcon * 2.0 * (tsoil(i,n+1) - tsoil(i,n))       &
                                             / (dz(n+1) + dz(n))
      dhflux_dtsl1(i,n) =  snow_hcon * 2.0 / (dz(n+1) + dz(n))
      dhflux_dtsl2(i,n) = -snow_hcon * 2.0 / (dz(n+1) + dz(n))
    END DO
!$OMP END DO NOWAIT
  END DO

!$OMP DO SCHEDULE(STATIC)
  DO j = 1,lice_pts
    i = lice_index(j)
    h_flux(i,0)           = surf_ht_flux(i)
    dhflux_dtsl1(i,0)     = 0.0
    dhflux_dtsl2(i,0)     = 0.0
    h_flux(i,nshyd)       = 0.0
    dhflux_dtsl1(i,nshyd) = 0.0
    dhflux_dtsl2(i,nshyd) = 0.0
  END DO
!$OMP END DO NOWAIT

  !---------------------------------------------------------------------------
  ! Calculate the matrix elements required for the implicit update.
  !---------------------------------------------------------------------------
  DO n = 1, nshyd
!$OMP DO SCHEDULE(STATIC)
    DO j = 1, lice_pts
      i = lice_index(j)
      gamcon = gamma_t * timestep / (snow_hcap * dz(n))
      a(i,n) = -gamcon * dhflux_dtsl1(i,n-1)
      b(i,n) = 1.0 - gamcon * (dhflux_dtsl2(i,n-1) - dhflux_dtsl1(i,n))
      c(i,n) = gamcon * dhflux_dtsl2(i,n)
      d(i,n) = (-h_flux(i,n) + h_flux(i,n-1)) * timestep / (snow_hcap * dz(n))
    END DO
!$OMP END DO NOWAIT
  END DO

  !---------------------------------------------------------------------------
  ! Solve the triadiagonal matrix equation.
  !---------------------------------------------------------------------------
  ! Allow wide limits as we should not need them.
!$OMP DO SCHEDULE(STATIC) COLLAPSE(2)
  DO n = 1,nshyd
    DO l = 1, npnts
      dtsoilmin(l,n) = -1.0e4
      dtsoilmax(l,n) = 1.0e4
    END DO
  END DO
!$OMP END DO NOWAIT
!$OMP END PARALLEL

  CALL gauss(nshyd, npnts, lice_pts, use_lims_gauss, lice_index, a, b, c, d,   &
             dtsoilmin, dtsoilmax, dtsoil)

  !---------------------------------------------------------------------------
  ! Update the layer temperatures
  !---------------------------------------------------------------------------
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) IF(lice_pts > 1)              &
!$OMP PRIVATE(n,j,i)                                                           &
!$OMP SHARED(nshyd,lice_pts,lice_index,tsoil,dtsoil)                           &
!$OMP COLLAPSE(2)
  DO n = 1,nshyd
    !CDIR NODEP
    DO j = 1,lice_pts
      i = lice_index(j)
      tsoil(i,n) = tsoil(i,n) + dtsoil(i,n)
    END DO
  END DO
!$OMP END PARALLEL DO

ELSE

  ! .NOT. l_land_ice_imp

!$OMP PARALLEL DEFAULT(NONE) IF(lice_pts > 1) PRIVATE(n,j,i)                   &
!$OMP SHARED(nshyd,lice_pts,lice_index,h_flux,snow_hcon,tsoil,dz,              &
!$OMP        surf_ht_flux,snow_hcap,timestep)

  ! Original explicit scheme
  !---------------------------------------------------------------------------
  ! Calculate heat fluxes across layer boundaries
  !---------------------------------------------------------------------------

  DO n = 1,nshyd-1
!$OMP DO SCHEDULE(STATIC)
    DO j = 1,lice_pts
      i = lice_index(j)
      h_flux(i,n) = -snow_hcon * 2.0 * (tsoil(i,n+1) - tsoil(i,n))             &
                                       / (dz(n+1) + dz(n))
    END DO
!$OMP END DO NOWAIT
  END DO

!DIR$ IVDEP
  ! Fujitsu vectorization directive
  !OCL NOVREC
!$OMP DO SCHEDULE(STATIC)
  DO j = 1,lice_pts
    i = lice_index(j)
    h_flux(i,nshyd) = 0.0
    h_flux(i,0) = surf_ht_flux(i)
  END DO
!$OMP END DO

  !---------------------------------------------------------------------------
  ! Update the sub-surface temperatures
  !---------------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
  DO n = 1,nshyd
    !CDIR NOVECTOR
    !   CDIR$ IVDEP here would force vectorization but changes GA6 results!
    DO j = 1,lice_pts
      i = lice_index(j)

      tsoil(i,n) = tsoil(i,n) + 1.0 / (snow_hcap * dz(n))                      &
                   * (h_flux(i,n-1) - h_flux(i,n)) * timestep

    END DO
  END DO
!$OMP END DO NOWAIT
!$OMP END PARALLEL

END IF  !  l_land_ice_imp

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE ice_htc
END MODULE ice_htc_mod
