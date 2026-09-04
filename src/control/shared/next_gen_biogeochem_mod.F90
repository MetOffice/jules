! *****************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in Veg3 Ecosystem Demography
! *****************************COPYRIGHT****************************************

MODULE next_gen_biogeochem_mod

IMPLICIT NONE

PRIVATE
!Make routines available
PUBLIC :: next_gen_biogeochem

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='NEXT_GEN_BIOGEOCHEM_MOD'

CONTAINS

SUBROUTINE next_gen_biogeochem(                                                &
        !IN control vars
          asteps_since_triffid,a_step,land_pts,nnpft,nmasst,veg3_ctrl,         &
          ainfo,                                                               &
        !IN parms
          litter_parms,red_parms,                                              &
        !INOUT data structures
          veg_state,red_state                                                  &
        !OUT diagnostics
        )

!Only get the data structures - the data comes through the calling tree

USE veg3_parm_mod,           ONLY:                                             &
  veg3_ctrl_type,litter_parm_type,red_parm_type

USE veg3_field_mod,          ONLY:                                             &
  veg_state_type,red_state_type

USE ancil_info,              ONLY: ainfo_type

IMPLICIT NONE

!----------------------------------------------------------------------------
! Objects with INTENT in
!----------------------------------------------------------------------------
TYPE(veg3_ctrl_type), INTENT(IN)     :: veg3_ctrl
TYPE(litter_parm_type), INTENT(IN)   :: litter_parms
TYPE(red_parm_type), INTENT(IN)      :: red_parms

!----------------------------------------------------------------------------
! Objects with INTENT inout
!----------------------------------------------------------------------------
TYPE(veg_state_type), INTENT(IN OUT)  :: veg_state
TYPE(red_state_type), INTENT(IN OUT)  :: red_state
TYPE(ainfo_type), INTENT(IN OUT)      :: ainfo

!----------------------------------------------------------------------------
! INTEGERS with INTENT in
!----------------------------------------------------------------------------
INTEGER, INTENT(IN)    :: land_pts, nnpft, nmasst
INTEGER, INTENT(IN)    :: a_step
                      ! Current atmospheric timestep number, used to
                      ! determine when the phenology timestep falls due.

!----------------------------------------------------------------------------
! Variables with INTENT inout
!----------------------------------------------------------------------------

INTEGER, INTENT(IN OUT) ::                                                     &
asteps_since_triffid
                      ! IN Number of atmospheric timesteps since last call
                      !    to TRIFFID.

!-----------------------------------------------------------------------------
! Local Objects
!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Local Variables
!-----------------------------------------------------------------------------

INTEGER ::                                                                     &
  veg_index(land_pts),                                                         &
  veg_index_pts,                                                               &
  l,n
        ! Counters

REAL ::                                                                        &
  frac_vs(land_pts)
        ! Veg/Soil Fractional coverage

! End of header

!---------------------------------------------------------------------
! Find total fraction of gridbox covered by vegetation and soil, and
! use this to set indices of land points on which veg3 may operate.
!---------------------------------------------------------------------
veg_index_pts = 0
DO l = 1,land_pts
  frac_vs(l) = 0.0
  DO n = 1,nnpft
    frac_vs(l) = frac_vs(l) + veg_state%frac(l,n)
  END DO
  frac_vs(l) = frac_vs(l) + veg_state%frac(l,veg3_ctrl%soil)
  IF ( frac_vs(l) >= REAL(nnpft) *  veg3_ctrl%frac_min ) THEN
    veg_index_pts = veg_index_pts + 1
    veg_index(veg_index_pts) = l
  END IF
END DO

! Call the Vegetation Biogeochemistry model
IF (veg_index_pts > 0) CALL veg3_run_ctrl(                                     &
              !IN Control vars
              asteps_since_triffid,a_step,land_pts,nnpft,nmasst,veg_index_pts, &
              veg_index,veg3_ctrl,ainfo,                                       &
              !IN parms
              litter_parms,red_parms,                                          &
              !IN state
              veg_state,red_state                                              &
              !OUT Diagnostics
              )

! Call the soil Biogeochemistry model
! This is where the soil biogechemistry ctrl call will be made

END SUBROUTINE next_gen_biogeochem

!------------------------------------------------------------------------------
SUBROUTINE veg3_run_ctrl(                                                      &
                !IN Control vars
                asteps_since_triffid,a_step,land_pts,nnpft,nmasst,             &
                veg_index_pts,veg_index,veg3_ctrl,ainfo,                       &
                !IN parms
                litter_parms,red_parms,                                        &
                !IN state
                veg_state,red_state                                            &
                !OUT Diagnostics
                )

!Only get the data structures - the data comes through the calling tree
USE veg3_parm_mod,   ONLY:  veg3_ctrl_type,litter_parm_type,red_parm_type
USE veg3_field_mod,  ONLY:  veg_state_type,red_state_type,red_veg3_couple
USE veg3_litter_mod, ONLY:  veg3_litter
USE ancil_info,      ONLY: ainfo_type

!Access subroutines
USE veg3_red_dynamic_mod, ONLY:  veg3_red_dynamic

!Access some parameters direct from module
USE conversions_mod, ONLY: rsec_per_day

IMPLICIT NONE

!----------------------------------------------------------------------------
! Integers with INTENT IN
!----------------------------------------------------------------------------
INTEGER, INTENT(IN OUT) ::                                                     &
asteps_since_triffid
                      ! IN Number of atmospheric timesteps since last call
                      !    to TRIFFID.

INTEGER, INTENT(IN)    :: a_step
                      ! IN Current atmospheric timestep number, used to
                      !    determine when the phenology timestep falls due.

!-----------------------------------------------------------------------------
! Objects with INTENT IN
!-----------------------------------------------------------------------------
TYPE(veg3_ctrl_type),INTENT(IN)   :: veg3_ctrl
TYPE(litter_parm_type),INTENT(IN) :: litter_parms
TYPE(red_parm_type),INTENT(IN)    :: red_parms

!-----------------------------------------------------------------------------
! Objects with INTENT INOUT
!-----------------------------------------------------------------------------
TYPE(veg_state_type),INTENT(IN OUT)   :: veg_state
TYPE(red_state_type),INTENT(IN OUT)   :: red_state
TYPE(ainfo_type), INTENT(IN OUT)      :: ainfo

!----------------------------------------------------------------------------
! INTEGERS with INTENT in
!----------------------------------------------------------------------------
INTEGER, INTENT(IN)    :: land_pts,nnpft,nmasst,veg_index_pts,veg_index(land_pts)

!-----------------------------------------------------------------------------
!Local Vars
!-----------------------------------------------------------------------------

REAL::                                                                         &
npp_dr(land_pts,nnpft),                                                        &
    ! Mean NPP for driving vegetation (kg C/m2/s).
local_litter(land_pts,nnpft),                                                  &
    ! Litter production (kg C/m2/s).
growth(land_pts,nnpft),                                                        &
    ! growth (kg C/m2/s).
mort_add(land_pts,nnpft,nmasst),                                               &
    ! mortality above baseline (/m2)
g_leaf_phen_dr(land_pts,nnpft)
    ! Mean phenology-driven leaf turnover rate for driving vegetation
    ! dynamics (/s)

!End of headers

!Initialise Arrays
npp_dr(:,:)          = 0.0
mort_add(:,:,:)      = 0.0

!-----------------------------------------------------------------------------
! Work out the phenology at its own timestep, appending to the accumulated
! leaf turnover rates, and (on the vegetation dynamics timestep) diagnose
! the mean phenology-driven leaf turnover rate for driving litterfall and
! vegetation dynamics. This mirrors the phenology/TRIFFID timestep split in
! veg-veg2a_jls_mod.
!-----------------------------------------------------------------------------
CALL veg3_phenol_couple(veg_index_pts,veg_index,veg3_ctrl,land_pts,nnpft,      &
                        a_step,asteps_since_triffid,veg_state,g_leaf_phen_dr)

! Now call vegetation model
IF (asteps_since_triffid == veg3_ctrl%nstep_trif) THEN

  !Call Litter
  CALL veg3_Litter(                                                            &
                !IN Control vars
                veg_index_pts,veg_index,veg3_ctrl,land_pts,nnpft,              &
                !IN parms
                litter_parms,                                                  &
                !IN fields
                g_leaf_phen_dr,                                                &
                !IN state
                veg_state,                                                     &
                ! OUT Fields
                local_litter                                                   &
                !OUT Diagnostics
                )

  !CALL Allocation/Nitrogen/NSC


  !Work out growth

  ! Use the accumulated npp from sf_expl. Copy to new variable for driving
  ! veg dynamics

  npp_dr = veg_state%npp_acc / veg3_ctrl%dt_red

  ! Record driving npp_dr and convert to s-1 -> (360 days)-1
  veg_state%npp_dr_out(:,:) = npp_dr * rsec_per_day * 360

  ! Reset accumulation to zero - note this can be used to pass a negative flux
  ! back to JULES.
  veg_state%npp_acc(:,:)=0.0
  growth = npp_dr - local_litter

  !Call Allocation/Nitrogen/NSC

  !Now on Mass classes
  !Veg_dynamics_mass

  !Call Mortality+Disturbance - total disturbance term on mass classes
  ! mort_add should be calculated here

  !This could get complicated - what is the disturbance term to maintain a
  !managed gridbox fraction? Maybe need multiple calls but then the order matters.

  !Call Veg Dynamics
  !Call Veg Dynamics - in this case RED
  CALL veg3_red_dynamic(                                                       &
                  !IN control vars
                  veg3_ctrl%dt_red,veg_index_pts,veg_index,veg3_ctrl,land_pts, &
                  nnpft,nmasst,                                                &
                  !IN RED_parms
                  red_parms,                                                   &
                  !IN fields
                  growth,mort_add,                                             &
                  !IN state
                  veg_state,red_state                                          &
                  !OUT Diagnostics
                  )

  !Update Vegetation State
  CALL red_veg3_couple(ainfo)

  !Partition Density Dependent Litter

  !Call Harvest

  !Call Wood Products

  !Pass fVegLitterC and N out to soil bgc


END IF

IF ( asteps_since_triffid == veg3_ctrl%nstep_trif ) asteps_since_triffid = 0

END SUBROUTINE veg3_run_ctrl

!------------------------------------------------------------------------------
SUBROUTINE veg3_phenol_couple(                                                 &
                !IN Control vars
                veg_index_pts,veg_index,veg3_ctrl,land_pts,nnpft,              &
                a_step,asteps_since_triffid,                                   &
                !IN state
                veg_state,                                                     &
                !OUT fields
                g_leaf_phen_dr                                                 &
                )

! Diagnoses leaf phenology and the mean phenology-driven leaf turnover rate
! that drives litterfall/vegetation dynamics, in veg1/veg2.

!Only get the data structures - the data comes through the calling tree
USE jules_vegetation_mod, ONLY: l_phenol
USE veg3_parm_mod,        ONLY: veg3_ctrl_type
USE veg3_field_mod,       ONLY: veg_state_type
USE phenol_mod,           ONLY: phenol

!Access some parameters direct from module
USE conversions_mod, ONLY: rsec_per_day

IMPLICIT NONE

!----------------------------------------------------------------------------
! Integers with INTENT IN
!----------------------------------------------------------------------------
INTEGER, INTENT(IN)    :: land_pts,nnpft,veg_index_pts,veg_index(land_pts)

INTEGER, INTENT(IN)    :: a_step
                      ! Current atmospheric timestep number, used to
                      ! determine when the phenology timestep falls due.

INTEGER, INTENT(IN)    :: asteps_since_triffid
                      ! Number of atmospheric timesteps since last call to
                      ! vegetation dynamics.

!-----------------------------------------------------------------------------
! Objects with INTENT IN
!-----------------------------------------------------------------------------
TYPE(veg3_ctrl_type),INTENT(IN)   :: veg3_ctrl

!-----------------------------------------------------------------------------
! Objects with INTENT INOUT
!-----------------------------------------------------------------------------
TYPE(veg_state_type),INTENT(IN OUT)   :: veg_state

!-----------------------------------------------------------------------------
! Reals with INTENT OUT
!-----------------------------------------------------------------------------
REAL, INTENT(OUT)      ::                                                      &
g_leaf_phen_dr(land_pts,nnpft)
    ! Mean phenology-driven leaf turnover rate for driving vegetation
    ! dynamics (s-1).

!-----------------------------------------------------------------------------
!Local Vars
!-----------------------------------------------------------------------------
REAL ::                                                                        &
gam_trif,                                                                      &
              ! Inverse vegetation dynamics coupling timestep ((360d)-1).
lai_bal_dummy(land_pts),                                                       &
              ! Dummy lai to pass into phenol routine, gets around bug where
              ! FORTRAN does not accept veg_state%lai_bal as optional
              ! argument.
g_leaf_day(land_pts)
              ! Mean leaf turnover rate driving phenology, diagnosed from
              ! the physiological leaf turnover accumulated since the
              ! previous phenology call. ((360d)-1)

INTEGER :: l,n,k
    ! Loop counters.

!End of headers

g_leaf_phen_dr(:,:) = 0.0

!-----------------------------------------------------------------------------
! Work out the phenology at its own timestep, appending to the accumulated
! leaf turnover rates. This is called independently of the vegetation
! dynamics timestep, mirroring the phenology/TRIFFID timestep split in
! veg-veg2a_jls_mod.
!-----------------------------------------------------------------------------
IF (l_phenol .AND. MOD(a_step,veg3_ctrl%nstep_phen) == 0) THEN

  veg_state%phen(:,:) = 1.0
  lai_bal_dummy(:) = 0.0
  g_leaf_day(:) = 0.0

  DO n = 1,nnpft
    lai_bal_dummy(:) = veg_state%lai_bal(:,n)

    ! Diagnose the mean leaf turnover rate driving phenology over the
    ! elapsed phenology period, mirroring g_leaf_day in veg-veg2a_jls_mod.
    DO k = 1,veg_index_pts
      l = veg_index(k)
      g_leaf_day(l) = veg_state%g_leaf_acc(l,n) / veg3_ctrl%dt_phen_360d
    END DO

    CALL phenol(land_pts,veg_index_pts,n,veg_index,veg3_ctrl%dt_phen_360d,     &
                g_leaf_day,veg_state%canht(:,n),                               &
                veg_state%lai(:,n),veg_state%g_leaf_phen(:,n),lai_bal_dummy)

    DO k = 1,veg_index_pts
      l = veg_index(k)

      ! Save the diagnosed LAI immediately following the phenology update,
      veg_state%lai_phen(l,n) = veg_state%lai(l,n)

      ! Accumulate the mean phenological leaf turnover rate for driving
      ! vegetation dynamics.
      veg_state%g_leaf_phen_acc(l,n) = veg_state%g_leaf_phen_acc(l,n)          &
                                       + veg_state%g_leaf_phen(l,n) *          &
                                         veg3_ctrl%dt_phen_360d

      ! Reset the accumulated physiological leaf turnover ready for the
      ! next phenology period.
      veg_state%g_leaf_acc(l,n) = 0.0

      IF (veg_state%lai_bal(l,n) > 0) veg_state%phen(l,n) =                    &
      veg_state%lai(l,n)/veg_state%lai_bal(l,n)

    END DO

  END DO

END IF

!-----------------------------------------------------------------------------
! On the vegetation dynamics timestep, diagnose the mean phenology-driven
! leaf turnover rate that will drive litterfall and vegetation dynamics.
!-----------------------------------------------------------------------------
IF (asteps_since_triffid == veg3_ctrl%nstep_trif) THEN

  ! Calculate the inverse vegetation dynamics coupling timestep.
  gam_trif = 360.0 / REAL(veg3_ctrl%triffid_period)

  DO n = 1,nnpft
    DO k = 1,veg_index_pts
      l = veg_index(k)

      IF (l_phenol) THEN
        ! Diagnose the mean phenological leaf turnover rate over the
        ! coupling period and convert to JULES-standard per-second units.
        g_leaf_phen_dr(l,n) = veg_state%g_leaf_phen_acc(l,n) * gam_trif /      &
                              (rsec_per_day * 360.0)

        ! Reset the accumulated phenological turnover ready for the next
        ! coupling period.
        veg_state%g_leaf_phen_acc(l,n) = 0.0
      ELSE
        ! No phenology - fall back to the raw accumulated physiological
        ! leaf turnover rate, as in veg-veg2a_jls_mod.
        g_leaf_phen_dr(l,n) = veg_state%g_leaf_acc(l,n) * gam_trif /           &
                              (rsec_per_day * 360.0)

        veg_state%g_leaf_acc(l,n) = 0.0
      END IF

      ! Ensure the turnover will not remove more leaf than is present over
      ! the vegetation dynamics timestep. If it does, reduce the rate so
      ! that the turnover does not exceed the current LAI.
      IF (veg_state%lai(l,n) > 0.0) THEN
        IF (g_leaf_phen_dr(l,n) * veg3_ctrl%dt_red > 1.0) THEN
          g_leaf_phen_dr(l,n) = 1.0 / veg3_ctrl%dt_red

        END IF
      ELSE
        g_leaf_phen_dr(l,n) = 0.0

      END IF

    END DO
  END DO

END IF

END SUBROUTINE veg3_phenol_couple

END MODULE next_gen_biogeochem_mod
