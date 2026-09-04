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
!
! Some of the content of this file has been produced with the assistance of
! Met Office Github Copilot Enterprise.

MODULE veg3_field_mod

USE veg3_parm_mod, ONLY: red_parms
USE um_types, ONLY: real_jlslsm
USE jules_vegetation_mod, ONLY: l_red

!Use at module level
USE ancil_info,    ONLY: ainfo_type
USE prognostics,   ONLY: progs_type

IMPLICIT NONE

! Structure to keep the vegetation state variables and fields
TYPE :: veg_state_type
  REAL, POINTER ::                                                             &
      leafC(:,:),                                                              &
              ! PFT leaf carbon per PFT area fraction. (kg C m-2)
      rootC(:,:),                                                              &
              ! PFT root carbon per PFT area fraction. (kg C m-2)
      woodC(:,:),                                                              &
              ! PFT woody carbon per PFT area fraction. (kg C m-2)
      lai_bal(:,:),                                                            &
              ! Balanced LAI. (m2 m-2)
      vegCpft(:,:),                                                            &
              ! Total PFT carbon density per PFT area fraction. (kg C m-2)
      vegC(:),                                                                 &
              ! Gridbox mean vegetation carbon. (kg C m-2)
      npp_acc(:,:),                                                            &
              ! Accumulated NPP. (kg C m-2 s-1)
      npp_dr_out(:,:),                                                         &
              ! A diagnostic NPP variable driving RED. (kg C m-2 (360d)-1)
      frac(:,:),                                                               &
              ! Gridbox area fraction of each tile type including PFTs.
      phen(:,:),                                                               &
              ! PFT phenology state variable, diagnosed from lai and lai_bal.
      lai(:,:),                                                                &
              ! Leaf area index. Associated with trif_vars%lai_pft (m2 m-2).
      canht(:,:),                                                              &
              ! Canopy height. Associated with trif_vars%canht_pft. (m)
      g_leaf_phen(:,:),                                                        &
              ! PFT leaf carbon mass density turnover. ((360d)-1)
      g_leaf(:,:),                                                             &
              ! Pointer necessary to get the diagnosed g_leaf rate from the
              ! rest of JULES. ((360d)-1)
      lai_phen(:,:),                                                           &
              ! Diagnostic LAI immediately following the phenology update.
              ! Associated with trifctl%lai_phen_pft. (m2 m-2)
      g_leaf_acc(:,:),                                                         &
              ! Accumulated leaf turnover rate since the last phenology call.
              ! ((360d)-1)
      g_leaf_phen_acc(:,:),                                                    &
              ! Accumulated mean phenological leaf turnover rate since the
              ! last vegetation dynamics call. ((360d)-1)
      leaf_litC(:,:),                                                          &
              ! Leaf litter carbon flux per PFT fraction. (kg C m-2 (360d)-1)
      root_litC(:,:),                                                          &
              ! Root litter carbon flux per PFT fraction. (kg C m-2 (360d)-1)
      wood_litC(:,:),                                                          &
              ! Wood litter carbon flux per PFT fraction. (kg C m-2 (360d)-1)
      litCpft(:,:),                                                            &
              ! Total litter carbon flux per PFT fraction, also includes any
              ! litter from dynamics (e.g., mortality). (kg C m-2 (360d)-1)
      litC(:)
              ! Total litter carbon flux per gridbox. (kg C m-2 (360d)-1)

  REAL, ALLOCATABLE ::                                                         &
      mort_litC(:,:)
              ! Mortality/demographic litter carbon flux from vegetation
              ! dynamics, normalised per unit PFT canopy area (kg C m-2 s-1).

END TYPE veg_state_type

! Structure to keep the RED state variables and fields
TYPE :: red_state_type
  REAL, ALLOCATABLE ::                                                         &
    mass_mass(:,:),                                                            &
              !  PFT plant mass tiles/classes. (kg C)
    ht_mass(:,:),                                                              &
              !  PFT height across plant mass. (m)
    lai_bal_mass(:,:),                                                         &
              !  PFT balanced leaf area index across plant mass. (m2/m2)
    crwn_area_mass(:,:),                                                       &
              !  PFT crown area across plant mass. (m2)
    g_mass_scale(:,:),                                                         &
              !  PFT plant growth scaling wrt metabolic scaling
              !  theory across mass. (kg C /kg C)
    mclass_geom_mult(:),                                                       &
              !  PFT geometric scaling coefficent for binning mass classes (-)
    mort(:,:,:)
              !  PFT mortality rate across plant mass. (/s)
  REAL, POINTER ::                                                             &
    plantNumDensity(:,:,:)
              !  PFT number density across plant mass. (/m2)
END TYPE red_state_type

TYPE(veg_state_type)   :: veg_state
TYPE(red_state_type)   :: red_state

!Private by default
PRIVATE

!Expose routines
PUBLIC :: veg3_field_init, veg3_field_allocate, veg3_field_assoc,              &
          red_veg3_couple

!Expose data
PUBLIC :: veg_state, red_state

!Expose data structures
PUBLIC :: veg_state_type, red_state_type

!Allow external code to read but not write
!PROTECTED ::

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='VEG3_FIELD_MOD'

CONTAINS
!-------------------------------------------------------------------------------

SUBROUTINE veg3_field_allocate(land_pts,nsurft,nnpft,nmasst)

IMPLICIT NONE
INTEGER, INTENT(IN) :: land_pts, nsurft, nnpft, nmasst

!End of Header

ALLOCATE(veg_state%leafC        ( land_pts, nnpft) )
ALLOCATE(veg_state%rootC        ( land_pts, nnpft) )
ALLOCATE(veg_state%woodC        ( land_pts, nnpft) )
ALLOCATE(veg_state%vegCpft      ( land_pts, nnpft) )
ALLOCATE(veg_state%lai_bal      ( land_pts, nnpft) )
ALLOCATE(veg_state%canht        ( land_pts, nnpft) )
ALLOCATE(veg_state%lai          ( land_pts, nnpft) )
ALLOCATE(veg_state%phen         ( land_pts, nnpft) )
ALLOCATE(veg_state%npp_acc      ( land_pts, nnpft) )
ALLOCATE(veg_state%npp_dr_out   ( land_pts, nnpft) )
ALLOCATE(veg_state%g_leaf_phen  ( land_pts, nnpft) )
ALLOCATE(veg_state%g_leaf       ( land_pts, nnpft) )
ALLOCATE(veg_state%lai_phen     ( land_pts, nnpft) )
ALLOCATE(veg_state%g_leaf_acc   ( land_pts, nnpft) )
ALLOCATE(veg_state%g_leaf_phen_acc ( land_pts, nnpft) )
ALLOCATE(veg_state%frac         ( land_pts, nsurft) )
ALLOCATE(veg_state%vegC         ( land_pts) )
ALLOCATE(veg_state%leaf_litC    ( land_pts, nnpft) )
ALLOCATE(veg_state%root_litC    ( land_pts, nnpft) )
ALLOCATE(veg_state%wood_litC    ( land_pts, nnpft) )
ALLOCATE(veg_state%litCpft     ( land_pts, nnpft) )
ALLOCATE(veg_state%litC         ( land_pts) )
ALLOCATE(veg_state%mort_litC    ( land_pts, nnpft) )

!Initialise
veg_state%leafC(:,:)           = 0.0
veg_state%rootC(:,:)           = 0.0
veg_state%woodC(:,:)           = 0.0
veg_state%vegCpft(:,:)         = 0.0
veg_state%lai_bal(:,:)         = 0.0
veg_state%canht(:,:)           = 0.0
veg_state%lai(:,:)             = 0.0
veg_state%phen(:,:)            = 0.0
veg_state%npp_acc(:,:)         = 0.0
veg_state%npp_dr_out(:,:)      = 0.0
veg_state%g_leaf_phen(:,:)     = 0.0
veg_state%g_leaf(:,:)          = 0.0
veg_state%lai_phen(:,:)        = 0.0
veg_state%g_leaf_acc(:,:)      = 0.0
veg_state%g_leaf_phen_acc(:,:) = 0.0
veg_state%frac(:,:)            = 0.0
veg_state%vegC(:)              = 0.0
veg_state%leaf_litC(:,:)       = 0.0
veg_state%root_litC(:,:)       = 0.0
veg_state%wood_litC(:,:)       = 0.0
veg_state%litCpft(:,:)         = 0.0
veg_state%litC(:)              = 0.0
veg_state%mort_litC(:,:)       = 0.0

! RED

! Allocate red_data_type
ALLOCATE(red_state%mass_mass          (nnpft, nmasst ))
ALLOCATE(red_state%ht_mass            (nnpft, nmasst ))
ALLOCATE(red_state%lai_bal_mass       (nnpft, nmasst ))
ALLOCATE(red_state%crwn_area_mass     (nnpft, nmasst ))
ALLOCATE(red_state%g_mass_scale       (nnpft, nmasst ))
ALLOCATE(red_state%plantNumDensity    (land_pts, nnpft, nmasst ))
ALLOCATE(red_state%mort               (land_pts, nnpft, nmasst ))

! Initialise red_data_type
red_state%mass_mass(:,:)          = 0.0
red_state%ht_mass(:,:)            = 0.0
red_state%lai_bal_mass(:,:)       = 0.0
red_state%crwn_area_mass(:,:)     = 0.0
red_state%g_mass_scale(:,:)       = 0.0
red_state%plantNumDensity(:,:,:)  = 0.0
red_state%mort(:,:,:)             = 0.0

RETURN
END SUBROUTINE veg3_field_allocate

!-------------------------------------------------------------------------------
SUBROUTINE veg3_field_assoc(progs, ainfo)

! Initial code to associate the veg3 and red fields to the rest of JULES
! This new routine moves out the pointers from veg3_set_fields to here
! to keep clean and more in line with in init.F90.

! We still need to run with l_triffid, but this will need to be addressed in a
! future revision to allow for veg3 to run fully independently of the switch.

USE jules_vegetation_mod,     ONLY: l_triffid
USE jules_fields_mod,         ONLY: trifctl_data, trif_vars_data

IMPLICIT NONE

TYPE(progs_type), INTENT(IN) :: progs
TYPE(ainfo_type), INTENT(IN) :: ainfo
! End of header
!-------------------------------------------------------------------------------
IF (l_red .AND. l_triffid) THEN

  ! Set pointers to the prognostic fields
  ! Note: veg_state%phen is not associated to an external target here - it is
  ! not an independent prognostic. It is diagnosed each phenology call as
  ! lai / lai_bal (see veg3_phenol_couple in next_gen_biogeochem_mod), so it
  ! remains the array allocated locally in veg3_field_allocate.
  red_state%plantNumDensity => progs%plantNumDensity
  veg_state%frac        => ainfo%frac_surft
  veg_state%canht       => progs%canht_pft
  veg_state%lai         => progs%lai_pft

  ! Additional TRIFFID fields mimicking the pointer associations set up in
  ! trif_vars_assoc and trifctl_assoc, so that veg3/RED diagnostics feed
  ! straight back into the standard JULES carbon diagnostics and outputs.

  ! Firstly trif_vars_data
  veg_state%lai_bal => trif_vars_data%lai_bal_pft
  veg_state%leafC   => trif_vars_data%leafc_pft
  veg_state%rootC   => trif_vars_data%rootc_pft
  veg_state%woodC   => trif_vars_data%woodc_pft
  veg_state%leaf_litC => trif_vars_data%leaf_litc_pft
  veg_state%root_litC => trif_vars_data%root_litc_pft
  veg_state%wood_litC => trif_vars_data%wood_litc_pft

  ! Next trifctl_data
  veg_state%npp_dr_out => trifctl_data%npp_dr_out_pft
  veg_state%g_leaf => trifctl_data%g_leaf_pft
  veg_state%g_leaf_phen => trifctl_data%g_leaf_phen_pft
  veg_state%lai_phen => trifctl_data%lai_phen_pft
  veg_state%g_leaf_acc => trifctl_data%g_leaf_acc_pft
  veg_state%g_leaf_phen_acc => trifctl_data%g_leaf_phen_acc_pft
  veg_state%vegCpft => trifctl_data%c_veg_pft
  veg_state%litCpft => trifctl_data%lit_c_pft
  veg_state%vegC => trifctl_data%cv_gb
  veg_state%litC => trifctl_data%lit_c_mn_gb

END IF

RETURN

END SUBROUTINE veg3_field_assoc


!-------------------------------------------------------------------------------
SUBROUTINE veg3_set_fields(land_pts,nsurft,nnpft,nmasst,ainfo,progs)

!Source parms, etc from io modules - these should be available on and offline

! Above only allocated if triffid on - needs to be addressed
USE jules_vegetation_mod,     ONLY: l_triffid, triffid_period
USE conversions_mod,          ONLY: rsec_per_day

USE jules_surface_types_mod,  ONLY: soil

IMPLICIT NONE

INTEGER, INTENT(IN) :: land_pts, nsurft, nnpft, nmasst

TYPE(ainfo_type), INTENT(IN OUT) :: ainfo
TYPE(progs_type), INTENT(IN) :: progs

INTEGER :: l,n

!End of header
!-----------------------------------------------------------------------------

IF (l_red .AND. l_triffid) THEN
  veg_state%phen(:,:) = 1.0

  DO l = 1, land_pts
    DO n = 1, nnpft

      ! We estimate the PFT physical properties from the prognostic field
      ! plantNumDensity for the following reasons:
      !
      ! i.) Check to see if plantNumDensity derived frac is less than the
      !     minimum fraction, this should introduce a small error in carbon
      !     flux but probably only once.

      ! ii.) red_veg3_couple needs to know the phen state variable, which
      !      is diagnosed from lai_bal. Both are not prognostic variables,
      !      but lai is, so we can infer then phen state from this and
      !      plantNumDensity for lai_bal.

      CALL pft_mean_from_mass_class(                                           &
        !IN sizing
        red_parms%mclass(n),                                                   &
        !IN mass-cohort properties
        red_state%plantNumDensity(l,n,1:red_parms%mclass(n)),                  &
        red_state%mass_mass(n,1:red_parms%mclass(n)),                          &
        red_state%lai_bal_mass(n,1:red_parms%mclass(n)),                       &
        red_state%ht_mass(n,1:red_parms%mclass(n)),                            &
        red_state%crwn_area_mass(n,1:red_parms%mclass(n)),                     &
        !OUT fields
        veg_state%frac(l,n),veg_state%vegCpft(l,n),veg_state%lai_bal(l,n),     &
        veg_state%canht(l,n)                                                   &
        )

      ! Check to see if frac is less than the min fraction, if so, add
      ! the necessary amount to the plant number density to balance in the
      ! lowest mass class, and set the fraction to the minimum directly.
      IF (veg_state%frac(l,n) < red_parms%frac_min(n)) THEN
        red_state%plantNumDensity(l,n,1) = red_state%plantNumDensity(l,n,1)    &
          + (red_parms%frac_min(n) - veg_state%frac(l,n)) /                    &
          red_state%crwn_area_mass(n,1)
        veg_state%frac(l,n) = red_parms%frac_min(n)

      END IF

      ! Here we estimate the phenology diagnosed from the lai and lai_bal from
      ! the dump/intialisation.
      IF (veg_state%lai_bal(l,n) > 0.0 .AND.  veg_state%lai_bal(l,n) >=        &
          veg_state%lai(l,n)) THEN
        veg_state%phen(l,n) = veg_state%lai(l,n) / veg_state%lai_bal(l,n)

      ELSE
        veg_state%phen(l,n) = TINY(0.0)

      END IF

    END DO
  END DO

  ! Initialise veg state from red prognostic
  CALL red_veg3_couple(ainfo)

  !-----------------------------------------------------------------------------
  ! AJW <<END
  !-----------------------------------------------------------------------------

END IF

RETURN
END SUBROUTINE veg3_set_fields

!-------------------------------------------------------------------------------
SUBROUTINE veg3_red_set_fields(nnpft,nmasst)

!Source parms, etc from io modules - these should be available on and offline

IMPLICIT NONE

INTEGER, INTENT(IN) :: nnpft, nmasst

INTEGER :: n,k

!End of header

! Setup Allometry
DO k = 1,nmasst
  DO n = 1,nnpft
    IF (k == 1) THEN
      red_state%mass_mass(n,k) = red_parms%mass0(n)
      red_state%ht_mass(n,k) = red_parms%height0(n)
      red_state%crwn_area_mass(n,k) = red_parms%crwn_area0(n)
      red_state%g_mass_scale(n,k) = 1.0
      red_state%lai_bal_mass(n,k) = red_parms%lai_bal0(n)
    ELSE IF (k > 1 .AND. k <= red_parms%mclass(n)) THEN
      red_state%mass_mass(n,k) = red_state%mass_mass(n,k-1)                    &
        * red_parms%mclass_geom_mult(n)
      red_state%ht_mass(n,k) = red_parms%height0(n)                            &
        * (red_state%mass_mass(n,k) / red_parms%mass0(n))** red_parms%phi_h(n)
      red_state%crwn_area_mass(n,k) = red_parms%crwn_area0(n)                  &
        * (red_state%mass_mass(n,k) / red_parms%mass0(n))** red_parms%phi_a(n)
      red_state%g_mass_scale(n,k) = (red_state%mass_mass(n,k)                  &
        / red_parms%mass0(n))** red_parms%phi_g(n)
      red_state%lai_bal_mass(n,k) = red_parms%lai_bal0(n)                      &
        * (red_state%mass_mass(n,k) / red_parms%mass0(n))** red_parms%phi_l(n)
    END IF
  END DO
END DO

RETURN
END SUBROUTINE veg3_red_set_fields

!-------------------------------------------------------------------------------

SUBROUTINE veg3_field_init(land_pts,nsurft,nnpft,npft,nmasst,ainfo,progs)

IMPLICIT NONE

INTEGER, INTENT(IN) :: land_pts, nnpft, npft, nsurft,nmasst

TYPE(ainfo_type), INTENT(IN OUT) :: ainfo
TYPE(progs_type), INTENT(IN) :: progs

!End of header

IF (l_red) THEN
  ! Set up all the necessary fields in RED
  CALL veg3_red_set_fields(nnpft,nmasst)
  ! Set up the rest of the fields for VEG3
  CALL veg3_set_fields(land_pts,nsurft,nnpft,nmasst,ainfo,progs)
END IF

RETURN
END SUBROUTINE veg3_field_init
!-----------------------------------------------------------------------------

SUBROUTINE red_veg3_couple(ainfo)
!-----------------------------------------------------------------------------
! Main coupling routine to aggregate between mass tiles to pft properties
! provides diagnostics and the main coupling fields back to JULES
! Same routine called during initialisation and runtime
!-----------------------------------------------------------------------------

USE ancil_info,                   ONLY: land_pts, nmasst, ainfo_type
USE jules_surface_types_mod,      ONLY: nnpft, soil
USE jules_surface_mod,            ONLY: cmass
USE pftparm,                      ONLY: lma
USE gridbox_mean_mod,             ONLY: pfttiles_to_gbm,                       &
                                        masstiles_to_pfttiles
USE conversions_mod, ONLY: rsec_per_day

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
TYPE(ainfo_type),INTENT(IN OUT) :: ainfo

!Local
INTEGER :: l,n  ! Index variables.

REAL :: frac_old(land_pts,nnpft)
    ! PFT fraction prior to updating veg_state%frac below, i.e. before the
    ! change in canopy area resulting from vegetation dynamics.

!-----------------------------------------------------------------------------
!end of header

! Record the vegetation fraction prior to updating it below, for use in
! weighting litter fluxes that occurred over the previous fraction.
frac_old(:,:) = veg_state%frac(:,1:nnpft)

veg_state%vegCpft(:,:)  = 0.0
veg_state%lai_bal(:,:)  = 0.0
veg_state%canht(:,:)    = 0.0
veg_state%frac(:,1:nnpft) = 0.0
veg_state%lai_bal(:,:) = 0.0
veg_state%canht(:,:)   = 0.0

DO n = 1,nnpft
  DO l = 1,land_pts

    ! Estimate the PFT mean physical properties by aggregating across the
    ! mass class structure.
    CALL pft_mean_from_mass_class(                                             &
      !IN sizing
      red_parms%mclass(n),                                                     &
      !IN mass-cohort properties
      red_state%plantNumDensity(l,n,1:red_parms%mclass(n)),                    &
      red_state%mass_mass(n,1:red_parms%mclass(n)),                            &
      red_state%lai_bal_mass(n,1:red_parms%mclass(n)),                         &
      red_state%ht_mass(n,1:red_parms%mclass(n)),                              &
      red_state%crwn_area_mass(n,1:red_parms%mclass(n)),                       &
      !OUT fields
      veg_state%frac(l,n),veg_state%vegCpft(l,n),veg_state%lai_bal(l,n),       &
      veg_state%canht(l,n)                                                     &
      )

    ! Update the phenology
    veg_state%lai(l,n)   = veg_state%phen(l,n) * veg_state%lai_bal(l,n)

    ! Update the leaf the root pools
    veg_state%leafC(l,n) = cmass * lma(n) * veg_state%lai_bal(l,n)
    veg_state%rootC(l,n) = cmass * lma(n) * veg_state%lai_bal(l,n)
    ! Wood carbon balance of total minus leaf and root
    veg_state%woodC(l,n) = veg_state%vegCpft(l,n) - veg_state%leafC(l,n)       &
                            - veg_state%rootC(l,n)

    ! NPP and its derived litter fluxes are normalised per unit PFT canopy
    ! Aggregate the leaf/root/wood turnover litter and the mortality litter
    ! demographic litter for the total litter flux per PFT fraction.
    veg_state%litCpft(l,n) = (veg_state%leaf_litC(l,n) +                       &
                              veg_state%root_litC(l,n) +                       &
                              veg_state%wood_litC(l,n) +                       &
                              veg_state%mort_litC(l,n) * rsec_per_day *        &
                              360.0)
    ! Update bare soil
    veg_state%frac(l,soil) = MAX(0.0, 1.0 - SUM(veg_state%frac(l,1:nnpft)))

  END DO

END DO

!Final aggregation to gridbox for vegetation carbon for diagnostic purposes
veg_state%vegC = pfttiles_to_gbm(veg_state%vegCpft,ainfo,frac_surft_in         &
               = veg_state%frac)

! Aggregate the per-PFT litter contributions for the gridbox total
veg_state%litC(:) = pfttiles_to_gbm(veg_state%litCpft,ainfo,frac_surft_in      &
                  = veg_state%frac)

RETURN
END SUBROUTINE red_veg3_couple

!-----------------------------------------------------------------------------
SUBROUTINE pft_mean_from_mass_class(                                           &
                !IN sizing
                mclass,                                                        &
                !IN mass-cohort properties
                plantNumDensity,mass_mass,lai_bal_mass,ht_mass,crwn_area_mass, &
                !OUT fields
                frac,vegCpft,lai_bal,canht                                     &
                )
!-----------------------------------------------------------------------------
! Aggregates the plant number density across the mass class structure of a
! single PFT at a single point into the PFT mean physical properties
! (fraction, carbon density, balanced LAI and canopy height).
!-----------------------------------------------------------------------------

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Integers with INTENT IN
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) :: mclass
              !  Number of mass classes for this PFT.

!-----------------------------------------------------------------------------
! Reals with INTENT IN
!-----------------------------------------------------------------------------
REAL, INTENT(IN)     ::                                                        &
plantNumDensity(mclass),                                                       &
              !  Population density within each mass cohort. (m-2)
mass_mass(mclass),                                                             &
              !  Mass of an individual member of each mass cohort. (kgC)
lai_bal_mass(mclass),                                                          &
              !  Balanced leaf area index of an individual member of each mass
              !  cohort. (m2 m-2)
ht_mass(mclass),                                                               &
              !  Height of an individual member of each mass cohort. (m)
crwn_area_mass(mclass)
              !  Crown area of an individual member of each mass cohort. (m2)

!-----------------------------------------------------------------------------
! Reals with INTENT OUT
!-----------------------------------------------------------------------------
REAL, INTENT(OUT)    ::                                                        &
frac,                                                                          &
              !  PFT fraction across the gridbox. (-)
vegCpft,                                                                       &
              !  Total PFT carbon density per PFT area fraction. (kg C m-2)
lai_bal,                                                                       &
              !  Balanced LAI per PFT area fraction. (m2 m-2)
canht
              !  Canopy height per PFT area fraction. (m)

!-----------------------------------------------------------------------------
!Local Vars
!-----------------------------------------------------------------------------
INTEGER              :: k

!End of headers

! Initialise vars
frac    = 0.0
vegCpft = 0.0
lai_bal = 0.0
canht   = 0.0

! Convert from plant number on mass classes to area on PFTs
! and biomass to carbon
DO k = 1, mclass
  frac    = frac    + plantNumDensity(k) * crwn_area_mass(k)
  vegCpft = vegCpft + plantNumDensity(k) * mass_mass(k)
  lai_bal = lai_bal + plantNumDensity(k) * lai_bal_mass(k) * crwn_area_mass(k)
  canht   = canht   + plantNumDensity(k) * ht_mass(k) * crwn_area_mass(k)
END DO

! Convert to per m2 plant
IF (frac > 0.0) THEN
  vegCpft = vegCpft / frac
  lai_bal = lai_bal / frac
  canht   = canht   / frac
ELSE

  ! If frac is zero, set mean to lowest mass class value
  vegCpft = mass_mass(1)
  lai_bal = lai_bal_mass(1)
  canht   = ht_mass(1)
END IF

END SUBROUTINE pft_mean_from_mass_class
!-----------------------------------------------------------------------------

END MODULE veg3_field_mod
