! *****************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
! *****************************COPYRIGHT****************************************
MODULE surf_couple_rivers_mod

USE um_types, ONLY: real_jlslsm

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SURF_COUPLE_RIVERS_MOD'

CONTAINS
SUBROUTINE surf_couple_rivers(                                                 &
   !INTEGER, INTENT(IN)
   land_pts, n_wtrac_jls,                                                      &
   !REAL, INTENT(IN)
   net_abstracted_river,                                                       &
   sub_surf_roff, surf_roff, sub_surf_roff_wtrac, surf_roff_wtrac,             &
   !INTEGER, INTENT(INOUT)
   a_steps_since_riv,                                                          &
   !REAL, INTENT (INOUT)
   tot_surf_runoff_gb, tot_sub_runoff_gb, acc_lake_evap_gb,                    &
   tot_surf_runoff_gb_wtrac, tot_sub_runoff_gb_wtrac,                          &
   acc_lake_evap_gb_wtrac,                                                     &
   !REAL, INTENT (OUT)
   rivers_sto_per_m2_on_landpts, rflow, rrun,                                  &
   !Arguments for the UM-----------------------------------------
   !INTEGER, INTENT(IN)
   n_proc, row_length, rows, river_row_length, river_rows,                     &
   land_index, aocpl_row_length, aocpl_p_rows, g_p_field,                      &
   g_r_field, global_row_length, global_rows, global_river_row_length,         &
   global_river_rows,                                                          &
   !REAL, INTENT(IN)
   lake_evap, delta_lambda, delta_phi, xx_cos_theta_latitude,                  &
   xpa, xua, xva, ypa, yua, yva, flandg, trivdir,                              &
   trivseq, r_area, slope, flowobs1, r_inext, r_jnext, r_land,                 &
   smvcst_soilt, smvcwt_soilt, frac_surft, lake_evap_wtrac,                    &
   !REAL, INTENT(INOUT)
   substore, surfstore, flowin, bflowin, twatstor, smcl_soilt, sthu_soilt,     &
   twatstor_wtrac, smcl_soilt_wtrac, sthu_soilt_wtrac,                         &
   !REAL, INTENT(OUT)
   inlandout_atm_gb, inlandout_riv, riverout, box_outflow, box_inflow,         &
   riverout_rgrid, inlandout_atm_gb_wtrac,                                     &
   !  imported rivers arrays
   rivers)

!Module imports

!Common modules
USE rivers_route_mod,         ONLY: scatter_land_from_riv_field,               &
                                    rivers_route_rp
USE jules_rivers_mod,         ONLY: i_river_vn, nstep_rivers,                  &
                                    rivers_camaflood, rivers_rfm,              &
                                    rivers_trip, rivers_call, np_rivers,       &
                                    rivers_type, l_riv_overbank,               &
                                    l_vary_sea_level,                          &
                                    ! UM only
                                    l_inland_outflow, rivers_um_trip,          &
                                    rivers_first
USE jules_model_environment_mod, ONLY:  l_oasis_rivers
USE timestep_mod,             ONLY: timestep

USE ereport_mod,              ONLY: ereport

USE atm_fields_bounds_mod,    ONLY: tdims_s, pdims_s, pdims

USE ancil_info,               ONLY: nsoilt

USE jules_soil_mod,           ONLY: sm_levels

USE overbank_update_mod,      ONLY: overbank_update

USE jules_surface_types_mod,  ONLY: lake, ntype

USE jules_irrig_mod,          ONLY: l_irrig_limit !Used in ifdef

USE rivers_regrid_mod,        ONLY: landpts_to_rivpts, rivpts_to_landpts

USE jules_water_tracers_mod,  ONLY: l_wtrac_jls

!Module imports - Variables required only in UM-mode
#if defined(UM_JULES)
USE riv_intctl_mod_1A,        ONLY: riv_intctl_1a

USE timestep_mod,             ONLY: timestep_number

USE level_heights_mod,        ONLY: r_theta_levels

USE atm_land_sea_mask,        ONLY: global_land_pts => atmos_number_of_landpts
USE um_parallel_mod,          ONLY: is_master_task, gather_land_field
USE um_riv_to_jules_mod,      ONLY: um_riv_to_jules, jules_riv_to_um
#else
!Variables required only in JULES standalone-mode
!Imported module routines
USE model_grid_mod,           ONLY: global_land_pts
USE parallel_mod,             ONLY: is_master_task, gather_land_field,         &
                                    scatter_land_field
USE diag_swchs,               ONLY: srflow, srrun
USE overbank_inundation_mod,  ONLY: frac_fplain_lp, frac_fplain_rp
#endif

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Control routine for rivers. If OASIS rivers is used, regridding between
!   the land and river grids is done automatically by the coupler.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Subroutine arguments

INTEGER, INTENT(IN) ::                                                         &
  n_proc,                                                                      &
  land_pts,                                                                    &
    ! Number of land points
  row_length,                                                                  &
  rows,                                                                        &
  river_row_length,                                                            &
  river_rows,                                                                  &
  land_index(land_pts),                                                        &
  aocpl_row_length,                                                            &
  aocpl_p_rows,                                                                &
  g_p_field,                                                                   &
  g_r_field,                                                                   &
  global_row_length,                                                           &
  global_rows,                                                                 &
  global_river_row_length,                                                     &
  global_river_rows,                                                           &
  n_wtrac_jls

INTEGER, INTENT(IN OUT)  ::                                                    &
  a_steps_since_riv

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  net_abstracted_river(land_pts),                                              &
    ! Net abstraction from rivers (kg m-2).
  surf_roff(land_pts),                                                         &
  ! Surface runoff (kg m-2 s-1)
  sub_surf_roff(land_pts),                                                     &
  ! Sub-surface runoff (kg m-2 s-1)
  lake_evap(land_pts),                                                         &
  surf_roff_wtrac(land_pts,n_wtrac_jls),                                       &
  ! Water tracer surface runoff (kg m-2 s-1)
  sub_surf_roff_wtrac(land_pts,n_wtrac_jls),                                   &
  ! Water tracer sub-surface runoff (kg m-2 s-1)
  lake_evap_wtrac(land_pts,n_wtrac_jls),                                       &
  delta_lambda,                                                                &
  delta_phi,                                                                   &
  xx_cos_theta_latitude(tdims_s%i_start:tdims_s%i_end,                         &
                        tdims_s%j_start:tdims_s%j_end),                        &
  xpa(aocpl_row_length+1),                                                     &
  xua(0:aocpl_row_length),                                                     &
  xva(aocpl_row_length+1),                                                     &
  ypa(aocpl_p_rows),                                                           &
  yua(aocpl_p_rows),                                                           &
  yva(0:aocpl_p_rows),                                                         &
  flandg(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end),         &
  trivdir(river_row_length, river_rows),                                       &
  trivseq(river_row_length, river_rows),                                       &
  r_area(row_length, rows),                                                    &
  slope(row_length, rows),                                                     &
  flowobs1(row_length, rows),                                                  &
  r_inext(row_length, rows),                                                   &
  r_jnext(row_length, rows),                                                   &
  r_land(row_length, rows),                                                    &
  smvcst_soilt(land_pts,nsoilt),                                               &
  smvcwt_soilt(land_pts,nsoilt),                                               &
  frac_surft(land_pts,ntype)

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  tot_surf_runoff_gb(land_pts),                                                &
    ! Average rate of surface runoff over river timestep (kg m-2 s-1).
  tot_sub_runoff_gb(land_pts),                                                 &
    ! Average rate of subsurface runoff over river timestep (kg m-2 s-1).
  acc_lake_evap_gb(row_length,rows),                                           &
  tot_surf_runoff_gb_wtrac(land_pts,n_wtrac_jls),                              &
  tot_sub_runoff_gb_wtrac(land_pts,n_wtrac_jls),                               &
  acc_lake_evap_gb_wtrac(row_length,rows,n_wtrac_jls),                         &
  twatstor(river_row_length, river_rows),                                      &
  smcl_soilt(land_pts,nsoilt,sm_levels),                                       &
  sthu_soilt(land_pts,nsoilt,sm_levels),                                       &
  twatstor_wtrac(river_row_length, river_rows,n_wtrac_jls),                    &
  smcl_soilt_wtrac(land_pts,nsoilt,sm_levels,n_wtrac_jls),                     &
  sthu_soilt_wtrac(land_pts,nsoilt,sm_levels,n_wtrac_jls),                     &
  substore(row_length, rows),                                                  &
  surfstore(row_length, rows),                                                 &
  flowin(row_length, rows),                                                    &
  bflowin(row_length, rows)

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
   rivers_sto_per_m2_on_landpts(land_pts),                                     &
   ! Water storage (kg m-2) on land points
   rflow(land_pts),                                                            &
   ! River flow diagnostic on land points
   rrun(land_pts),                                                             &
   ! Runoff diagnostic on land points
   inlandout_atm_gb(land_pts),                                                 &
   inlandout_riv(river_row_length,river_rows),                                 &
   riverout(row_length, rows),                                                 &
   box_outflow(river_row_length, river_rows),                                  &
   box_inflow(river_row_length, river_rows),                                   &
   riverout_rgrid(river_row_length, river_rows)

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
   inlandout_atm_gb_wtrac(land_pts,n_wtrac_jls)

! Rivers Arrays
TYPE(rivers_type), INTENT(IN OUT) :: rivers

! Local array variables.
!-----------------------------------------------------------------------------
! Jules-standalone
#if !defined(UM_JULES)
REAL(KIND=real_jlslsm) :: rivers_sto_per_m2_rgrid(np_rivers)
                          ! Water storage on river points in kg m-2
#endif

REAL(KIND=real_jlslsm), ALLOCATABLE :: global_tot_sub_runoff(:)
  ! Average rate of subsurface runoff over river timestep (kg m-2 s-1).
REAL(KIND=real_jlslsm), ALLOCATABLE :: global_tot_surf_runoff(:)
  ! Average rate of surface runoff over river timestep (kg m-2 s-1).
REAL(KIND=real_jlslsm), ALLOCATABLE :: global_rrun(:)
REAL(KIND=real_jlslsm), ALLOCATABLE :: global_rflow(:)
REAL(KIND=real_jlslsm), ALLOCATABLE :: global_sea_level(:)

#if defined(UM_JULES)
REAL(KIND=real_jlslsm) ::                                                      &
  a_boxareas(row_length,rows),                                                 &
  inlandout_atmos(row_length,rows)
#endif

REAL(KIND=real_jlslsm), ALLOCATABLE :: inlandout_atmos_wtrac(:,:,:)
                           ! inlandout_atmos_gb_wtrac on (i,j) grid
REAL(KIND=real_jlslsm), ALLOCATABLE :: net_abstracted_river_wtrac(:,:)
                           ! Net abstraction of water tracers from rivers
                           ! (kg m-2).

!Local variables
INTEGER ::                                                                     &
  ERROR,                                                                       &
  ! Error status from each call to ALLOCATE.
  error_sum,                                                                   &
  ! Accumulated error status.
  l,i,j,ip,i_wt

#if defined(UM_JULES)
REAL(KIND=real_jlslsm) ::                                                      &
   riv_step
   ! River timestep (secs)

LOGICAL ::                                                                     &
   invert_atmos,                                                               &
   invert_ocean = .FALSE.
#endif

!Error reporting
CHARACTER(LEN=256)       :: message
INTEGER                  :: errorstatus
CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'SURF_COUPLE_RIVERS'

!Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Allocate water tracer fields
IF (l_wtrac_jls) THEN
  ALLOCATE(inlandout_atmos_wtrac(row_length,rows,n_wtrac_jls))
  ALLOCATE(net_abstracted_river_wtrac(land_pts,n_wtrac_jls))
ELSE
  ALLOCATE(inlandout_atmos_wtrac(1,1,1))
  ALLOCATE(net_abstracted_river_wtrac(1,1))
END IF

#if defined(UM_JULES)

!Pass inland flow to soil moisture every timestep
IF ( .NOT. l_inland_outflow) THEN
  DO j = 1, rows
    DO i = 1, row_length
      inlandout_atmos(i,j) = 0.0
    END DO
  END DO
  DO j = 1, river_rows
    DO i = 1, river_row_length
      inlandout_riv(i,j)   = 0.0
    END DO
  END DO

  IF (l_wtrac_jls) THEN
    DO i_wt = 1, n_wtrac_jls
      DO j = 1, rows
        DO i = 1, row_length
          inlandout_atmos_wtrac(i,j,i_wt) = 0.0
        END DO
      END DO
    END DO
  END IF
END IF

!Set rrun and rflow to zero
DO l = 1, land_pts
  rrun(l)  = 0.0
  rflow(l) = 0.0
END DO

#endif

!Initialise the accumulated surface and subsurface runoff to zero
!at the beginning of river routing timestep
IF ( a_steps_since_riv == 0 ) THEN
  tot_surf_runoff_gb(:) = 0.0
  tot_sub_runoff_gb(:)  = 0.0
  acc_lake_evap_gb(:,:) = 0.0

  IF (l_wtrac_jls) THEN
    DO i_wt = 1, n_wtrac_jls
      DO l = 1, land_pts
        tot_surf_runoff_gb_wtrac(l,i_wt) = 0.0
        tot_sub_runoff_gb_wtrac(l,i_wt)  = 0.0
      END DO
      DO j = 1, rows
        DO i = 1, row_length
          acc_lake_evap_gb_wtrac(i,j,i_wt)   = 0.0
        END DO
      END DO
    END DO
  END IF

  IF (l_oasis_rivers) THEN
    rivers%rrun_sub_surf_rp(:) = 0.0
    rivers%rrun_surf_rp(:)     = 0.0
  END IF
END IF

! Increment counters.
a_steps_since_riv = a_steps_since_riv + 1

IF (a_steps_since_riv == nstep_rivers) THEN
  rivers_call = .TRUE.
ELSE
  rivers_call = .FALSE.
END IF

!Accumulate the runoff as Kg/m2/s over the River Routing period (that is,
! between calls to river routing).
IF (l_oasis_rivers) THEN

  CALL accumulate_runoff(np_rivers, net_abstracted_river,                      &
                         rivers%surf_roff_rp, rivers%sub_surf_roff_rp,         &
                         rivers%rrun_surf_rp, rivers%rrun_sub_surf_rp)

ELSE

  CALL accumulate_runoff(land_pts, net_abstracted_river,                       &
                         surf_roff, sub_surf_roff,                             &
                         tot_surf_runoff_gb, tot_sub_runoff_gb)

  ! Repeat for water tracers
  IF (l_wtrac_jls) THEN
    ! Water tracers are not yet represented in the water resources code, hence
    ! set net abstraction of tracers to zero.
    net_abstracted_river_wtrac(:,:) = 0.0
    DO i_wt = 1, n_wtrac_jls
      CALL accumulate_runoff(land_pts, net_abstracted_river_wtrac(:,i_wt),     &
                       surf_roff_wtrac(:,i_wt), sub_surf_roff_wtrac(:,i_wt),   &
                       tot_surf_runoff_gb_wtrac(:,i_wt),                       &
                       tot_sub_runoff_gb_wtrac(:,i_wt))
    END DO
  END IF  ! l_wtrac_jls

END IF  !  l_oasis_rivers)

! Could this UM ifdef be replaced with the i_rivers_vn switch?
#if defined(UM_JULES)
! Accumulate the GBM lake evaporation over river routing period
DO l = 1, land_pts
  j = (land_index(l) - 1) / row_length +1
  i = land_index(l) - (j-1) * row_length
  acc_lake_evap_gb(i,j) = acc_lake_evap_gb(i,j) +                              &
     frac_surft(l,lake) * lake_evap(l) * timestep
END DO

! Repeat for water tracers
IF (l_wtrac_jls) THEN
  DO i_wt = 1, n_wtrac_jls
    DO l = 1, land_pts
      j = (land_index(l) - 1) / row_length +1
      i = land_index(l) - (j-1) * row_length
      acc_lake_evap_gb_wtrac(i,j,i_wt) = acc_lake_evap_gb_wtrac(i,j,i_wt) +    &
              frac_surft(l,lake) * lake_evap_wtrac(l,i_wt) * timestep
    END DO
  END DO
END IF

! initialise diagnostics
IF (timestep_number == nstep_rivers) THEN
  riverout = 0.0
  box_outflow = 0.0
  box_inflow  = 0.0
END IF
#endif

!-------------------------------------------------------------------------
! Main call to river routing
!-------------------------------------------------------------------------

IF ( rivers_call ) THEN

#if defined(UM_JULES)
  !If ATMOS fields are as Ocean (i.e. inverted NS) set invert_atmos
  invert_atmos = .FALSE.

  IF ( .NOT. invert_ocean) THEN
    invert_atmos = .TRUE.
  END IF

  !Calculate the Atmosphere gridbox areas
  DO j = 1, rows
    DO i = 1, row_length
      a_boxareas(i,j) = r_theta_levels(i,j,0)                                  &
                    * r_theta_levels(i,j,0)                                    &
                    * delta_lambda * delta_phi                                 &
                    * xx_cos_theta_latitude(i,j)
    END DO
  END DO
#endif

  SELECT CASE ( i_river_vn )

#if defined(UM_JULES)
  CASE ( rivers_um_trip )
    IF (nsoilt == 1) THEN

      riv_step = REAL(nstep_rivers) * timestep
      !This subroutine is deprecated and has not been adapted to work with
      !soil tiling.
      CALL riv_intctl_1a(                                                      &
        xpa, xua, xva, ypa, yua, yva,                                          &
        g_p_field, g_r_field,                                                  &
        land_pts,n_wtrac_jls, land_index,                                      &
        invert_atmos, row_length, rows,                                        &
        global_row_length, global_rows,                                        &
        river_row_length, river_rows,                                          &
        global_river_row_length, global_river_rows,                            &
        flandg(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end),           &
        riv_step,                                                              &
        trivdir, trivseq, twatstor, riverout_rgrid, a_boxareas,                &
        twatstor_wtrac,                                                        &
        ! Fields used for UM-RFM implementation
        delta_phi,                                                             &
        r_area, slope, flowobs1,r_inext,r_jnext,r_land,                        &
        substore,surfstore,flowin,bflowin,                                     &
        !in accumulated runoff
        tot_surf_runoff_gb, tot_sub_runoff_gb,                                 &
        tot_surf_runoff_gb_wtrac, tot_sub_runoff_gb_wtrac,                     &
        !out
        box_outflow, box_inflow, riverout,                                     &
        inlandout_atmos,inlandout_riv,                                         &
        inlandout_atmos_wtrac,                                                 &
        !required for soil moisture correction for water conservation
        sm_levels,acc_lake_evap_gb,smvcst_soilt,smvcwt_soilt,                  &
        smcl_soilt(1:,1,sm_levels),sthu_soilt(1:,1,sm_levels),                 &
        acc_lake_evap_gb_wtrac, smcl_soilt_wtrac, sthu_soilt_wtrac             &
        )
    ELSE
      errorstatus = 10
      WRITE (message,*) 'riv_intctl_1a cannot be used when nsoilt > 1'
      CALL Ereport ( RoutineName, errorstatus, message)
    END IF
#endif

  CASE ( rivers_camaflood, rivers_rfm, rivers_trip )

#if defined(UM_JULES)
    ! Translate UM variables to jules_rivers_mod variables
    IF (rivers_first) THEN
      CALL um_riv_to_jules(g_p_field, global_river_row_length,                 &
                           global_river_rows, land_pts, land_index,            &
                           delta_lambda, delta_phi, a_boxareas,                &
                           r_area, flowobs1, r_inext, r_jnext,                 &
                           substore, surfstore, flowin, bflowin,               &
                           rivers)
    END IF
#endif
    !--------------------------------------------------------------------------
    !   Gather runoff information from all processors
    !--------------------------------------------------------------------------

    IF (.NOT. l_oasis_rivers .AND. is_master_task()) THEN
      ALLOCATE(global_tot_sub_runoff(global_land_pts), STAT = ERROR)
      error_sum = ERROR
      ALLOCATE(global_tot_surf_runoff(global_land_pts), STAT = ERROR)
      error_sum = error_sum + ERROR
      ALLOCATE(global_rrun(global_land_pts), STAT = ERROR)
      error_sum = error_sum + ERROR
      ALLOCATE(global_rflow(global_land_pts), STAT = ERROR)
      error_sum = error_sum + ERROR
    ELSE
      ALLOCATE(global_tot_sub_runoff(1), STAT = ERROR)
      error_sum = ERROR
      ALLOCATE(global_tot_surf_runoff(1), STAT = ERROR)
      error_sum = error_sum + ERROR
      ALLOCATE(global_rrun(1), STAT = ERROR)
      error_sum = error_sum + ERROR
      ALLOCATE(global_rflow(1), STAT = ERROR)
      error_sum = error_sum + ERROR
    END IF

    IF ( .NOT. l_oasis_rivers .AND. l_vary_sea_level .AND. is_master_task() )  &
      THEN
      ALLOCATE(global_sea_level(global_land_pts), STAT = ERROR)
      error_sum = error_sum + ERROR
    ELSE
      ALLOCATE(global_sea_level(1), STAT = ERROR)
      error_sum = error_sum + ERROR
    END IF

    IF ( error_sum /= 0 ) THEN
      errorstatus = 10
      CALL ereport( RoutineName, errorstatus,                                  &
                     "Error related to allocation of runoff variables." )
    END IF

    IF (.NOT. l_oasis_rivers) THEN
      CALL gather_land_field(tot_sub_runoff_gb, global_tot_sub_runoff,         &
                             rivers%global_land_index)
      CALL gather_land_field(tot_surf_runoff_gb, global_tot_surf_runoff,       &
                             rivers%global_land_index)
      IF ( l_vary_sea_level ) THEN
        CALL gather_land_field(rivers%sea_level_lp, global_sea_level,          &
                               rivers%global_land_index )
      END IF
    END IF

    !-------------------------------------------------------------------------
    ! Call routing driver on single processor
    !-------------------------------------------------------------------------
    IF ( is_master_task() ) THEN

      IF (l_oasis_rivers) THEN
        ! No regridding is required between the land and river grids as this
        ! is done by the OASIS coupler. Only the science routine needs calling.
        CALL rivers_route_rp( rivers )
      ELSE
        ! Initialisation
        DO l = 1, global_land_pts
          global_rflow(l)= 0.0
          global_rrun(l) = 0.0
        END DO

        ! Regrid surface and subsurface runoff from land points to rivers
        ! points.
        CALL landpts_to_rivpts( global_land_pts, np_rivers,                    &
                                rivers%map_river_to_land_points,               &
                                rivers%global_land_index,                      &
                                rivers%rivers_index_rp,                        &
                                global_tot_sub_runoff,                         &
                                rivers%rrun_sub_surf_rp )

        CALL landpts_to_rivpts( global_land_pts, np_rivers,                    &
                                rivers%map_river_to_land_points,               &
                                rivers%global_land_index,                      &
                                rivers%rivers_index_rp,                        &
                                global_tot_surf_runoff, rivers%rrun_surf_rp )

        IF ( l_vary_sea_level ) THEN
          ! Regrid sea level from land points to river points.
          CALL landpts_to_rivpts( global_land_pts, np_rivers,                  &
                                  rivers%map_river_to_land_points,             &
                                  rivers%global_land_index,                    &
                                  rivers%rivers_index_rp,                      &
                                  global_sea_level, rivers%sea_level )
        END IF

        ! Call the routing science routine.
        CALL rivers_route_rp( rivers )

        ! Regrid outputs from rivers to land grid
        CALL rivpts_to_landpts( global_land_pts, np_rivers,                    &
                                rivers%map_river_to_land_points,               &
                                rivers%global_land_index,                      &
                                rivers%rivers_index_rp,                        &
                                rivers%rflow_rp, global_rflow )

        CALL rivpts_to_landpts( global_land_pts, np_rivers,                    &
                                rivers%map_river_to_land_points ,              &
                                rivers%global_land_index,                      &
                                rivers%rivers_index_rp,                        &
                                rivers%rrun_rp, global_rrun )
      END IF

      !-----------------------------------------------------------------------
      ! Compute overbank inundation
      !-----------------------------------------------------------------------
      IF ( l_riv_overbank ) THEN
        CALL overbank_update(rivers%rfm_rivflow_rp)
      END IF
    END IF    ! end is_master

    !-------------------------------------------------------------------------
    ! Update output diagnostics
    !-------------------------------------------------------------------------
#if defined(UM_JULES)

    CALL jules_riv_to_um(global_rflow, riverout,                               &
                         substore, surfstore, flowin, bflowin,                 &
                         rivers)

#else
    IF (.NOT. l_oasis_rivers) THEN
      IF (srflow .OR. srrun) THEN
        CALL scatter_land_field(global_rrun, rrun)
        CALL scatter_land_field(global_rflow, rflow)

        !----------------------------------------------------------------------
        ! Update riv storage on land points used to calculate water for
        ! irrigation.
        !----------------------------------------------------------------------
        IF ( l_irrig_limit ) THEN
          DO ip = 1, np_rivers
            rivers_sto_per_m2_rgrid(ip) = rivers%rivers_sto_rp(ip) /           &
                                          rivers%rivers_boxareas_rp(ip)
          END DO
          CALL scatter_land_from_riv_field(rivers_sto_per_m2_rgrid,            &
                                           rivers_sto_per_m2_on_landpts,       &
                                           rivers)
        END IF
      END IF

      !------------------------------------------------------------------------
      ! Update fraction of inundated floodplain from overbank inundation.
      !------------------------------------------------------------------------
      IF ( l_riv_overbank ) THEN
        CALL scatter_land_from_riv_field( frac_fplain_rp, frac_fplain_lp,      &
                                          rivers )
      END IF
    END IF  !  .NOT. l_oasis_rivers
#endif

    DEALLOCATE(global_sea_level)
    DEALLOCATE(global_rflow)
    DEALLOCATE(global_rrun)
    DEALLOCATE(global_tot_surf_runoff)
    DEALLOCATE(global_tot_sub_runoff)

    IF (rivers_first) THEN
      rivers_first = .FALSE.
    END IF

  CASE DEFAULT

    errorstatus = 10
    WRITE (message,'(A,I6,A)') 'River model type option ',                     &
                       i_river_vn,' not recognised.'
    CALL Ereport ( RoutineName, errorstatus, message)

  END SELECT

#if defined(UM_JULES)
  !compress inland basin outputs to land points only
  IF (l_inland_outflow) THEN
    DO l = 1,land_pts
      j = (land_index(l) - 1) / row_length +1
      i = land_index(l) - (j-1) * row_length
      inlandout_atm_gb(l) = inlandout_atmos(i,j)
    END DO

    IF (l_wtrac_jls) THEN
      DO i_wt = 1, n_wtrac_jls
        DO l = 1,land_pts
          j = (land_index(l) - 1) / row_length +1
          i = land_index(l) - (j-1) * row_length
          inlandout_atm_gb_wtrac(l,i_wt) = inlandout_atmos_wtrac(i,j,i_wt)
        END DO
      END DO
    END IF
  END IF
#endif

  !-------------------------------------------------------------------------
  !   Reset counters after a call to routing.
  !-------------------------------------------------------------------------
  !Mult RIVEROUT by the number of physics timesteps per River routing
  !timestep as DAGHYD stores RIVEROUT every timestep. Non-routing
  !timestep vals are passed in as 0.0
  a_steps_since_riv = 0

END IF ! rivers_call

! Deallocate water tracer fields
DEALLOCATE(net_abstracted_river_wtrac)
DEALLOCATE(inlandout_atmos_wtrac)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE surf_couple_rivers

!---------------------------------------------------------------------------

SUBROUTINE accumulate_runoff(npoints, net_abstracted_river, surf_runoff,       &
                             sub_runoff, surf_runoff_accum, sub_runoff_accum)

! Accumulate the surface and subsurface runoff over the river routing period.
! This is a generic routine used for normal water and water tracers

USE jules_rivers_mod,          ONLY: nstep_rivers
USE jules_water_resources_mod, ONLY: sw_river_source
USE timestep_mod,              ONLY: timestep

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

INTEGER, INTENT(IN) :: npoints   ! No. of points

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  net_abstracted_river(npoints),                                               &
                                 ! Net abstraction from rivers (kg m-2).
  surf_runoff(npoints),                                                        &
                                 ! Surface runoff (kg m-2 s-1)
  sub_runoff(npoints)
                                 ! Sub-surface runoff (kg m-2 s-1)

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  surf_runoff_accum(npoints),                                                  &
                                 ! Average rate of surface runoff over river
                                 ! timestep (kg m-2 s-1).
  sub_runoff_accum(npoints)
                                 ! Average rate of subsurface runoff over river
                                 ! timestep (kg m-2 s-1).

! Local variables

INTEGER :: ip                    ! Loop counter

CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'ACCUMULATE_RUNOFF'

!Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle
!end of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DO ip = 1, npoints

  IF (surf_runoff(ip) >=  0.0) THEN
    surf_runoff_accum(ip) = surf_runoff_accum(ip) +                            &
                            (surf_runoff(ip) / REAL(nstep_rivers))
  END IF

  IF (sub_runoff(ip) >=  0.0) THEN
    sub_runoff_accum(ip) = sub_runoff_accum(ip) +                              &
                            (sub_runoff(ip) / REAL(nstep_rivers))
  END IF

  ! Consider abstraction of river water by water resources.
  IF ( sw_river_source > 0 ) THEN
    ! Remove net abstraction from the accumulated surface runoff.
    ! Convert units of abstraction from kg m-2 to kg m-2 s-1.
    surf_runoff_accum(ip) = surf_runoff_accum(ip) -                            &
                              ( net_abstracted_river(ip) /                     &
                                (REAL(nstep_rivers) * timestep) )
  END IF

END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE accumulate_runoff

END MODULE surf_couple_rivers_mod
