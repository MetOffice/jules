!******************************COPYRIGHT****************************************
! (c) British Antarctic Survey, 2023.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT****************************************

MODULE soil_hyd_wtrac_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!----------------------------------------------------------------------------
! Description:
!    Calculate the gravitational drainage and baseflow of water tracers
!    and update the water tracer layer soil moisture contents.
!    Use an explicit calculation following the approach for nutrient leaching
!    in n_leach.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in Hydrology
!
! Code Description:
!   Language: FORTRAN 90
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SOIL_HYD_WTRAC_MOD'

CONTAINS

SUBROUTINE soil_hyd_wtrac (npnts, nshyd, soil_pts, i_wt, timestep, l_top,      &
                           soil_index, sthzw, smclsat, qbase_l, w_flux,        &
                           sthzw_wtrac, ext_wtrac, fw, fw_wtrac,               &
                           sthu_old, qbase_l_wtrac, qbase_wtrac, smcl_wtrac,   &
                           sthu_wtrac, w_flux_wtrac)

USE jules_water_tracers_mod, ONLY: wtrac_calc_ratio_fn_jules

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

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
  i_wt
    ! Water tracer number

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  timestep
    ! Model timestep (s).

LOGICAL, INTENT(IN) ::                                                         &
  l_top
    ! Flag for TOPMELT-based hydrology

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) ::                                                         &
  soil_index(npnts)
    ! Array of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  sthzw(npnts),                                                                &
    ! Soil moisture fraction in the deep-zw layer
  smclsat(npnts,nshyd),                                                        &
    ! The saturation moisture content of each layer (kg/m2).
  qbase_l(npnts,nshyd+1),                                                      &
    ! Base flow from each level (kg/m2/s)
  w_flux(npnts,0:nshyd),                                                       &
    ! The fluxes of water between layers (kg/m2/s).
  sthzw_wtrac(npnts),                                                          &
    ! Soil water tracer fraction in the deep-zw layer
  ext_wtrac(npnts,nshyd),                                                      &
    ! Extraction of water tracer from each soil layer (kg/m2/s).
  fw(npnts),                                                                   &
    ! Throughfall from canopy plus snowmelt minus surface runoff (kg/m2/s).
  fw_wtrac(npnts)
    ! Water tracer throughfall from canopy plus snowmelt minus surface
    ! runoff (kg/m2/s).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  sthu_old(npnts,nshyd),                                                       &
    ! Unfrozen soil moisture content of each layer as a fraction of
    ! saturation after call to soil_hyd
    ! (IN: values before soil_hyd; OUT: values after soil_hyd)
  qbase_wtrac(npnts),                                                          &
    ! Water tracer total base flow (kg/m2/s)
  qbase_l_wtrac(npnts,nshyd+1),                                                &
    ! Water tracer base flow from each level (kg/m2/s)
  smcl_wtrac(npnts,nshyd),                                                     &
    ! Total soil water tracer contents of each layer (kg/m2).
  sthu_wtrac(npnts,nshyd)
    ! Unfrozen soil water tracer content of each layer as a fraction of
    ! saturation.

!-----------------------------------------------------------------------------
! Array arguments with INTENT(OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  w_flux_wtrac(npnts,0:nshyd)
    ! The fluxes of water tracer between layers (kg/m2/s).

!-----------------------------------------------------------------------------
! Local scalars:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, j, n
    ! Loop counters.

REAL(KIND=real_jlslsm) ::                                                      &
  ratio_wt_fw,                                                                 &
    ! Ratio of the surface flux (fw) water tracer to water
  ratio_wt_zw
    ! Ratio of soil water tracer to water in deep zw layer

!-----------------------------------------------------------------------------
! Local arrays:
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  ratio_wt(npnts,nshyd),                                                       &
    ! Ratio of unfrozen soil water tracer to water content
  smcl_old_wtrac(npnts,nshyd),                                                 &
    ! Initial water tracer soil moisture content of each layer (kg/m2).
  smclu_wtrac(npnts,nshyd)
    ! Unfrozen soil moisture contents of each layer (kg/m2).

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SOIL_HYD_WTRAC'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!----------------------------------------------------------------------------
! Calculate the water tracer to water ratio of the unfrozen soil moisture
! which is used to scale water fluxes in this routine
!----------------------------------------------------------------------------
!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,n,ratio_wt_zw,ratio_wt_fw)            &
!$OMP SHARED(soil_pts,nshyd,soil_index,ratio_wt,sthu_wtrac,sthu_old,           &
!$OMP        qbase_wtrac,qbase_l_wtrac,qbase_l,sthzw_wtrac,sthzw,i_wt,         &
!$OMP        smclu_wtrac,smclsat,smcl_wtrac,smcl_old_wtrac,w_flux,fw,l_top,    &
!$OMP        w_flux_wtrac,fw_wtrac,timestep,ext_wtrac)

DO n = 1,nshyd
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, soil_pts
    i = soil_index(j)
    ratio_wt(i,n) = wtrac_calc_ratio_fn_jules                                  &
                                  (i_wt,sthu_wtrac(i,n),sthu_old(i,n))
  END DO
!$OMP END DO
END DO

!----------------------------------------------------------------------------
! Calculate water tracer base flow
! (Note, if l_top=F, qbase_wtrac has been initialised to zero.)
!----------------------------------------------------------------------------

IF (l_top) THEN
!$OMP DO SCHEDULE(STATIC)
  DO j = 1,soil_pts
    i = soil_index(j)
    qbase_wtrac(i) = 0.0
    DO n = 1,nshyd
      qbase_l_wtrac(i,n) = qbase_l(i,n) * ratio_wt(i,n)
    END DO
    qbase_wtrac(i) = qbase_wtrac(i) + qbase_l_wtrac(i,n)
  END DO
!$OMP END DO NOWAIT

  ! Deep zw layer
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, soil_pts
    i = soil_index(j)
    ratio_wt_zw  = wtrac_calc_ratio_fn_jules(i_wt,sthzw_wtrac(i),sthzw(i))
    qbase_l_wtrac(i,nshyd+1) = qbase_l(i,nshyd+1) * ratio_wt_zw
    qbase_wtrac(i) = qbase_wtrac(i) + qbase_l_wtrac(i,nshyd+1)
  END DO
!$OMP END DO NOWAIT
END IF

!----------------------------------------------------------------------------
! Calculate the unfrozen water tracer content
!----------------------------------------------------------------------------

DO n = 1,nshyd
!$OMP DO SCHEDULE(STATIC)
  DO j = 1,soil_pts
    i = soil_index(j)
    smclu_wtrac(i,n) = sthu_wtrac(i,n) * smclsat(i,n)
  END DO
!$OMP END DO NOWAIT
END DO

!----------------------------------------------------------------------------
! Calculate the water tracer vertical flux between soil layers and update the
! water tracer soil moisture content
!----------------------------------------------------------------------------

! Store initial smcl_wtrac
DO n = 1, nshyd
!$OMP DO SCHEDULE(STATIC)
  DO j = 1,soil_pts
    i = soil_index(j)
    smcl_old_wtrac(i,n) = smcl_wtrac(i,n)
  END DO
!$OMP END DO NOWAIT
END DO

! Set top boundary flux
!$OMP DO SCHEDULE(STATIC)
DO j = 1,soil_pts
  i = soil_index(j)
  IF (ABS(w_flux(i,0)-fw(i)) < TINY(fw)) THEN
    ! w_flux(i,0) = fw(i)
    w_flux_wtrac(i,0) = fw_wtrac(i)
  ELSE
    ! In this case, w_flux(i,0) has been limited in soil_hyd to avoid
    ! supersaturation in the top layer.  Reduce water tracer flux whilst
    ! maintaining the water tracer to water fw ratio.
    ratio_wt_fw       = wtrac_calc_ratio_fn_jules(i_wt,fw_wtrac(i),fw(i))
    w_flux_wtrac(i,0) = ratio_wt_fw * w_flux(i,0)
  END IF
END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
DO j = 1,soil_pts
  i = soil_index(j)

  ! Add surface flux to top layer
  smcl_wtrac(i,1) = smcl_wtrac(i,1) + w_flux_wtrac(i,0) * timestep

  ! Work down soil column
  DO n = 1, nshyd-1

    IF (w_flux(i,n) >= 0.0) THEN
      ! If water flux is downwards

      ! Calculate water tracer vertical flux between layers n and n+1
      ! (Use ratio of water tracer at n as flux is downwards)
      w_flux_wtrac(i,n) = w_flux(i,n) * ratio_wt(i,n)

      ! Remove vertical water flux plus water tracer extracted by vegetation
      ! and baseflow
      smcl_wtrac(i,n) = smcl_wtrac(i,n) - ( w_flux_wtrac(i,n)                  &
                         + ext_wtrac(i,n) + qbase_l_wtrac(i,n) ) * timestep

      ! Add vertical water flux to layer below
      smcl_wtrac(i,n+1)   = smcl_wtrac(i,n+1) + w_flux_wtrac(i,n) * timestep

      ! Check that the vertical flux isn't removing more water tracer than is
      ! available
      IF (smcl_wtrac(i,n) < 0.0 ) THEN
        smcl_wtrac(i,n+1) = smcl_wtrac(i,n+1) + smcl_wtrac(i,n)
        smcl_wtrac(i,n)   = 0.0
        w_flux_wtrac(i,n) = w_flux_wtrac(i,n-1)                                &
                            - ext_wtrac(i,n) - qbase_l_wtrac(i,n)              &
                            - (smcl_wtrac(i,n)-smcl_old_wtrac(i,n)) / timestep
      END IF

    ELSE
      ! Water flux is upwards

      ! Calculate water tracer vertical flux between layers n and n+1
      ! (Use ratio of water tracer at n+1 as flux is upwards)
      w_flux_wtrac(i,n) = w_flux(i,n) * ratio_wt(i,n+1)

      ! Remove vertical water flux plus water tracer extracted by vegetation
      ! and baseflow
      smcl_wtrac(i,n) = smcl_wtrac(i,n) - ( w_flux_wtrac(i,n)                  &
                         + ext_wtrac(i,n) + qbase_l_wtrac(i,n) ) * timestep

      ! Add vertical water flux to layer below
      smcl_wtrac(i,n+1)   = smcl_wtrac(i,n+1) + w_flux_wtrac(i,n) * timestep

      ! Check that the vertical flux isn't removing more water tracer than is
      ! available
      IF (smcl_wtrac(i,n+1) < 0.0 ) THEN
        smcl_wtrac(i,n)   = smcl_wtrac(i,n) + smcl_wtrac(i,n+1)
        smcl_wtrac(i,n+1) = 0.0
        w_flux_wtrac(i,n) = w_flux_wtrac(i,n-1)                                &
                            - ext_wtrac(i,n) - qbase_l_wtrac(i,n)              &
                            - (smcl_wtrac(i,n)-smcl_old_wtrac(i,n)) / timestep
      END IF
    END IF

  END DO

  ! Bottom soil layer
  w_flux_wtrac(i,nshyd) = w_flux(i,nshyd) * ratio_wt(i,nshyd)
  smcl_wtrac(i,nshyd)   = smcl_wtrac(i,nshyd) - ( w_flux_wtrac(i,nshyd)        &
                         + ext_wtrac(i,nshyd) + qbase_l_wtrac(i,nshyd) )       &
                             * timestep
  IF (smcl_wtrac(i,nshyd) < 0.0) THEN
    ! Note, this w_flux_wtrac is used to update the deep soil layer in
    ! soil_hyd_wt
    w_flux_wtrac(i,nshyd) = w_flux_wtrac(i,nshyd-1)                            &
                            - ext_wtrac(i,nshyd) - qbase_l_wtrac(i,nshyd)      &
                            + smcl_old_wtrac(i,nshyd) / timestep
    smcl_wtrac(i,nshyd) = 0.0
  END IF

  DO n = 1, nshyd
    ! Update unfrozen soil moisture fields
    smclu_wtrac(i,n) = smclu_wtrac(i,n)                                        &
                       + (smcl_wtrac(i,n) - smcl_old_wtrac(i,n))
    sthu_wtrac(i,n)  = smclu_wtrac(i,n) / smclsat(i,n)
  END DO

END DO
!$OMP END DO
!$OMP END PARALLEL

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE soil_hyd_wtrac
END MODULE soil_hyd_wtrac_mod
