! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    SUBROUTINE CH4_TDEP-----------------------------------------------

! Description:
!     Calculates methane emissions from wetland area.

MODULE ch4_tdep_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='CH4_TDEP_MOD'

CONTAINS

SUBROUTINE ch4_tdep(npnts, soil_pts, const_tdep_cs, const_tdep_npp,            &
                    const_tdep_resps, t0_ch4, soil_index, tsoil_d, cs_eff,     &
                    resp_s_tot, npp,                                           &
                    decomp_wetlfrac_cs, decomp_wetlfrac_npp,                   &
                    decomp_wetlfrac_resps)

USE water_constants_mod,  ONLY:                                                &
  tm

USE jules_soil_biogeochem_mod, ONLY:                                           &
   const_ch4_cs, const_ch4_npp, const_ch4_resps

USE parkind1, ONLY: jprb, jpim
USE yomhook,  ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  npnts,                                                                       &
    ! Number of gridpoints.
  soil_pts
    ! Number of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  const_tdep_cs,                                                               &
    ! T and Q10(0) dependent function
  const_tdep_npp,                                                              &
    ! T and Q10(0) dependent function
  const_tdep_resps,                                                            &
    ! T and Q10(0) dependent function
  t0_ch4
    ! T0 value (zero celsius in kelvin)

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  soil_index(npnts)
    ! Array of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  tsoil_d(npnts),                                                              &
    ! Diagnosed soil temp to 1 metre (K).
  cs_eff(npnts),                                                               &
    ! Effective soil carbon (kg C/m2).
  resp_s_tot(npnts),                                                           &
    ! Soil respiration total (kg C/m2/s).
  npp(npnts)
    ! Gridbox mean net primary productivity (kg C/m2/s).

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  decomp_wetlfrac_cs(npnts),                                                   &
    ! Anaerobic decomposition using soil carbon as substrate, over the wetland
    ! fraction of the grid box (i.e. this is not scaled by wetland fraction)
    ! (kg C/m2/s).
  decomp_wetlfrac_npp(npnts),                                                  &
    ! Anaerobic decomposition using NPP as substrate, over the wetland fraction
    ! of the grid box (i.e. this is not scaled by wetland fraction)
    ! (kg C/m2/s).
  decomp_wetlfrac_resps(npnts)
    ! Anaerobic decomposition using soil respiration as substrate, over the
    ! wetland fraction of the grid box (i.e. this is not scaled by wetland
    ! fraction) (kg C/m2/s).

!-----------------------------------------------------------------------------
! Local scalar variables:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, j

REAL(KIND=real_jlslsm) ::                                                      &
  q10t_ch4_cs,                                                                 &
    ! Q10 value at T
  q10t_ch4_npp,                                                                &
    ! Q10 value at T
  q10t_ch4_resps
    ! Q10 value at Tcs

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CH4_TDEP'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Calculate an effective soil carbon for wetland methane emission.
!-----------------------------------------------------------------------------

!$OMP PARALLEL DO IF(soil_pts > 1) DEFAULT(NONE)                               &
!$OMP PRIVATE(i,j,q10t_ch4_cs,q10t_ch4_npp,q10t_ch4_resps)                     &
!$OMP SHARED(soil_pts,soil_index,tsoil_d,cs_eff,npp,decomp_wetlfrac_npp,       &
!$OMP        resp_s_tot,decomp_wetlfrac_resps, const_tdep_cs,                  &
!$OMP        const_tdep_npp,const_tdep_resps,const_ch4_cs,const_ch4_npp,       &
!$OMP        const_ch4_resps,t0_ch4,decomp_wetlfrac_cs)
DO j = 1,soil_pts
  i = soil_index(j)
  IF ( tsoil_d(i) > tm ) THEN

    q10t_ch4_cs    = EXP( const_tdep_cs / tsoil_d(i) )
    q10t_ch4_npp   = EXP( const_tdep_npp / tsoil_d(i) )
    q10t_ch4_resps = EXP( const_tdep_resps / tsoil_d(i) )

    decomp_wetlfrac_cs(i) = const_ch4_cs * cs_eff(i) *                         &
                      q10t_ch4_cs ** ( 0.1 * (tsoil_d(i) - t0_ch4) )
    IF ( npp(i) > 0.0 ) THEN
      decomp_wetlfrac_npp(i) = const_ch4_npp * npp(i) *                        &
                         q10t_ch4_npp ** ( 0.1 * (tsoil_d(i) - t0_ch4) )
    END IF
    IF ( resp_s_tot(i) > 0.0 ) THEN
      decomp_wetlfrac_resps(i) = const_ch4_resps * resp_s_tot(i) *             &
                           q10t_ch4_resps ** ( 0.1 * (tsoil_d(i) - t0_ch4) )
    END IF

  END IF  !  tsoil_d
END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE ch4_tdep
END MODULE ch4_tdep_mod
