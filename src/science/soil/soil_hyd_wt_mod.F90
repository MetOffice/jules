! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    SUBROUTINE SOIL_HYD_WT---------------------------------------------------

! Description: Updates water table depth, and calculates drainage and surface
!              runoff.

! Documentation : UM Documentation Paper 25


MODULE soil_hyd_wt_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SOIL_HYD_WT_MOD'

CONTAINS

SUBROUTINE soil_hyd_wt (npnts, nshyd, soil_pts, curr_soilt, nsoilt,            &
                        n_wtrac_jls, timestep,                                 &
                        stf_slow_runoff, l_top, soil_index,                    &
                        bexp, fw, sathh, smclsat, smclsatzw, v_sat, w_flux,    &
                        fw_wtrac, w_flux_wtrac, smcl, surf_roff, qbase_l,      &
                        smclzw, zw,                                            &
                        surf_roff_wtrac, qbase_l_wtrac, sthzw_wtrac,           &
                        slow_runoff, drain, qbase, sthzw,                      &
                        surf_roff_inc, slow_runoff_wtrac, qbase_wtrac,         &
                        surf_roff_inc_wtrac )

! Use relevant subroutines
USE calc_zw_mod,  ONLY: calc_zw

USE jules_water_tracers_mod, ONLY: l_wtrac_jls

USE parkind1,     ONLY: jprb, jpim
USE yomhook,      ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  npnts,                                                                       &
    ! Number of gridpoints.
  nshyd,                                                                       &
    ! Number of soil moisture levels.
  soil_pts,                                                                    &
    ! Number of soil points.
  curr_soilt,                                                                  &
    ! Current soil tile.
  nsoilt,                                                                      &
    ! Number of soil tiles
  n_wtrac_jls
    ! Number of water tracers

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  timestep
    ! Model timestep (s).

LOGICAL, INTENT(IN) ::                                                         &
  stf_slow_runoff,                                                             &
    ! Stash flag for sub-surface runoff.
  l_top
    ! Flag for TOPMODEL-based hydrology.

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  soil_index(npnts)
    ! Array of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  bexp(npnts,nshyd),                                                           &
    ! Brooks & Corey exponent.
  fw(npnts),                                                                   &
    ! Throughfall from canopy plus snowmelt minus surface runoff (kg/m2/s).
  sathh(npnts,nshyd),                                                          &
    ! Saturated soil water pressure (m).
  smclsat(npnts,nshyd),                                                        &
    ! The saturation moisture content of each layer (kg/m2).
  smclsatzw(npnts),                                                            &
    ! Moisture content in deep layer at saturation (kg/m2).
  v_sat(npnts,nshyd),                                                          &
    ! Volumetric soil moisture concentration at saturation (m3 H2O/m3 soil).
  w_flux(npnts,0:nshyd),                                                       &
    ! The fluxes of water between layers (kg/m2/s).
  fw_wtrac(npnts,n_wtrac_jls),                                                 &
    ! Water tracer throughfall from canopy plus snowmelt minus surface
    ! runoff (kg/m2/s).
  w_flux_wtrac(npnts,0:nshyd,n_wtrac_jls)
    ! The fluxes of water tracer between layers (kg/m2/s).


!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  smcl(npnts,nshyd),                                                           &
    ! Total soil moisture contents of each layer (kg/m2).
  surf_roff(npnts),                                                            &
    ! Surface runoff (kg/m2/s).
  qbase_l(npnts,nshyd+1),                                                      &
    ! Base flow from each level (kg/m2/s).
  smclzw(npnts),                                                               &
    ! Moisture content in deep layer (kg/m2).
  zw(npnts),                                                                   &
    ! Mean water table depth (m).
  surf_roff_wtrac(npnts,n_wtrac_jls),                                          &
    ! Surface water tracer runoff (kg/m2/s).
  qbase_l_wtrac(npnts,nshyd+1,n_wtrac_jls),                                    &
    ! Water tracer base flow from each level (kg/m2/s).
  sthzw_wtrac(npnts,nsoilt,n_wtrac_jls)
    ! Soil water tracer fraction in deep layer.
    ! (Note, unlike the water equivalent, this is the full field for all soil
    !  tiles)


!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  slow_runoff(npnts),                                                          &
    ! Drainage from the base of the soil profile (kg/m2/s).
  drain(npnts),                                                                &
    ! Drainage out of nshyd'th level (kg/m2/s).
  qbase(npnts),                                                                &
    ! Base flow (kg/m2/s).
  sthzw(npnts),                                                                &
    ! Soil moisture fraction in deep layer.
  surf_roff_inc(npnts),                                                        &
    ! Increment to surface runoff (kg m-2 s-1).
  slow_runoff_wtrac(npnts,n_wtrac_jls),                                        &
    ! Water tracer drainage from the base of the soil profile (kg/m2/s).
  qbase_wtrac(npnts,n_wtrac_jls),                                              &
    ! Water tracer base flow (kg/m2/s).
  surf_roff_inc_wtrac(npnts,n_wtrac_jls)
    ! Increment to surface water tracer runoff (kg m-2 s-1).

!-----------------------------------------------------------------------------
! Local scalars:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, j, n, i_wt              ! WORK Loop counters.

REAL(KIND=real_jlslsm) ::                                                      &
  smclzw_wtrac
    ! Water tracer content in deep layer (kg/m2)

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SOIL_HYD_WT'

!End of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
!-----------------------------------------------------------------------------

IF (l_top) THEN

  !---------------------------------------------------------------------------
  ! Diagnose the new water table depth.
  ! Assume local equilibrium psi profile.
  !---------------------------------------------------------------------------

  DO j = 1,soil_pts
    i = soil_index(j)
    smclzw(i) = smclzw(i) - (qbase_l(i,nshyd+1) - w_flux(i,nshyd)) * timestep
    ! Update prognostic deep layer soil moisture fraction:
    sthzw(i) = smclzw(i) / smclsatzw(i)
  END DO

  CALL calc_zw(npnts, nshyd, soil_pts, soil_index,                             &
               bexp, sathh, smcl, smclzw, smclsat, smclsatzw, v_sat, zw)

  IF (l_wtrac_jls) THEN
    ! Update water tracer deep layer soil moisture fraction
    DO i_wt = 1, n_wtrac_jls
      DO j = 1,soil_pts
        i = soil_index(j)
        smclzw_wtrac = sthzw_wtrac(i,curr_soilt,i_wt) * smclsatzw(i)
        smclzw_wtrac = smclzw_wtrac - (qbase_l_wtrac(i,nshyd+1,i_wt)           &
                                      - w_flux_wtrac(i,nshyd,i_wt)) * timestep
        ! Update prognostic deep layer water tracer soil moisture fraction
        sthzw_wtrac(i,curr_soilt,i_wt) = smclzw_wtrac / smclsatzw(i)
      END DO
    END DO
  END IF ! l_wtrac_jls

  !---------------------------------------------------------------------------
  ! Dont allow negative base flows:
  !---------------------------------------------------------------------------
  DO j = 1,soil_pts
    i = soil_index(j)
    qbase(i) = 0.0
    DO n = 1,nshyd+1
      qbase_l(i,n) = MAX(qbase_l(i,n),0.0)
      qbase(i)     = qbase(i) + qbase_l(i,n)
    END DO
  END DO

  IF (l_wtrac_jls) THEN
    ! Repeat for water tracers
    DO i_wt = 1, n_wtrac_jls
      DO j = 1,soil_pts
        i = soil_index(j)
        qbase_wtrac(i,i_wt) = 0.0
        DO n = 1,nshyd+1
          qbase_l_wtrac(i,n,i_wt) = MAX(qbase_l_wtrac(i,n,i_wt),0.0)
          qbase_wtrac(i,i_wt)     = qbase_wtrac(i,i_wt)                        &
                                    + qbase_l_wtrac(i,n,i_wt)
        END DO
      END DO
    END DO
  END IF

END IF
!-----------------------------------------------------------------------------
! Output slow runoff (drainage) diagnostic.
!-----------------------------------------------------------------------------
IF (stf_slow_runoff) THEN
  DO i = 1,npnts
    slow_runoff(i) = 0.0
  END DO
  DO j = 1,soil_pts
    i = soil_index(j)
    ! Ensure correct field is output as deep runoff: dependent on L_TOP
    IF (l_top) THEN
      slow_runoff(i) = qbase(i)
    ELSE
      slow_runoff(i) = w_flux(i,nshyd)
    END IF
    drain(i) = w_flux(i,nshyd)
  END DO
END IF

!-----------------------------------------------------------------------------
! Update surface runoff diagnostic.
!-----------------------------------------------------------------------------
DO j = 1,soil_pts
  i = soil_index(j)
  surf_roff(i)     = surf_roff(i) + (fw(i) - w_flux(i,0))
  surf_roff_inc(i) = fw(i) - w_flux(i,0)
END DO

!-----------------------------------------------------------------------------
! Output water tracer slow runoff and update surface runoff
!-----------------------------------------------------------------------------

IF (l_wtrac_jls) THEN

  ! Initialise water tracer slow_runoff
  DO i_wt = 1, n_wtrac_jls
    DO i = 1, npnts
      slow_runoff_wtrac(i,i_wt) = 0.0
    END DO
  END DO

  DO i_wt = 1, n_wtrac_jls
    DO j = 1, soil_pts
      i = soil_index(j)

      ! Ensure correct field is output as deep runoff: dependant on L_TOP
      IF (l_top) THEN
        slow_runoff_wtrac(i,i_wt) = qbase_wtrac(i,i_wt)
      ELSE
        slow_runoff_wtrac(i,i_wt) = w_flux_wtrac(i,nshyd,i_wt)
      END IF

      surf_roff_wtrac(i,i_wt)     = surf_roff_wtrac(i,i_wt)                    &
                                  + (fw_wtrac(i,i_wt) - w_flux_wtrac(i,0,i_wt))
      surf_roff_inc_wtrac(i,i_wt) = fw_wtrac(i,i_wt) - w_flux_wtrac(i,0,i_wt)

    END DO
  END DO
END IF  ! l_wtrac_jls

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE soil_hyd_wt
END MODULE soil_hyd_wt_mod
