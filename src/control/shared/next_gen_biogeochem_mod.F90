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
          asteps_since_triffid,land_pts,nnpft,nmasst,veg3_ctrl,                &
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
              asteps_since_triffid,land_pts,nnpft,nmasst,veg_index_pts,        &
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
                asteps_since_triffid,land_pts,nnpft,nmasst,veg_index_pts,      &
                veg_index,veg3_ctrl,ainfo,                                     &
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
demographic_lit(land_pts,nnpft)
    ! demographic litter aggregated to PFT

!End of headers

!Initialise Arrays
npp_dr(:,:)      = 0.0
mort_add(:,:,:)  = 0.0


! Now call vegetation model
IF (asteps_since_triffid == veg3_ctrl%nstep_trif) THEN

  !Call veg3_phenol()

  CALL veg3_Litter(                                                            &
                !IN Control vars
                veg_index_pts,veg_index,veg3_ctrl,land_pts,nnpft,              &
                !IN parms
                litter_parms,                                                  &
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

  npp_dr = veg_state%npp_acc / (rsec_per_day * veg3_ctrl%triffid_period)
  ! Reset accumulation to zero - note this can be used to pass a negative flux back to JULES.
  veg_state%npp_acc(:,:)=0.0
  growth = npp_dr - local_litter

  !Call Litter

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
                  rsec_per_day,veg_index_pts,veg_index,veg3_ctrl,land_pts,     &
                  nnpft,nmasst,                                                &
                  !IN RED_parms
                  red_parms,                                                   &
                  !IN fields
                  growth,mort_add,                                             &
                  !IN state
                  veg_state,red_state,                                         &
                  ! OUT Fields
                  demographic_lit                                              &
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

END MODULE next_gen_biogeochem_mod
