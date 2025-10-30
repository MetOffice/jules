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
  REAL, ALLOCATABLE ::                                                         &
      leafC(:,:),                                                              &
      rootC(:,:),                                                              &
      woodC(:,:),                                                              &
      lai_bal(:,:),                                                            &
      vegCpft(:,:),                                                            &
      vegC(:),                                                                 &
      npp_acc(:,:)
  REAL, POINTER ::                                                             &
      frac(:,:),                                                               &
      phen(:,:),                                                               &
      lai(:,:),                                                                &
      canht(:,:)
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
PUBLIC :: veg3_field_init, veg3_field_allocate, red_veg3_couple

!Expose data
PUBLIC :: veg_state, red_state

!Expose data structures
PUBLIC :: veg_state_type,red_state_type

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

ALLOCATE(veg_state%frac         ( land_pts, nsurft) )
ALLOCATE(veg_state%vegC         ( land_pts) )

!Initialise
veg_state%leafC(:,:)          = 0.0
veg_state%rootC(:,:)          = 0.0
veg_state%woodC(:,:)          = 0.0
veg_state%vegCpft(:,:)        = 0.0
veg_state%lai_bal(:,:)        = 0.0
veg_state%vegC(:)             = 0.0
veg_state%npp_acc(:,:)        = 0.0


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
red_state%lai_bal_mass(:,:  )     = 0.0
red_state%crwn_area_mass(:,:)     = 0.0
red_state%g_mass_scale(:,:)       = 0.0
red_state%mort(:,:,:)             = 0.0


RETURN
END SUBROUTINE veg3_field_allocate

!-------------------------------------------------------------------------------
SUBROUTINE veg3_set_fields(land_pts,nsurft,nnpft,nmasst,ainfo,progs)

!Source parms, etc from io modules - these should be available on and offline

! Above only allocated if triffid on - needs to be addressed
USE jules_vegetation_mod,     ONLY: l_triffid, triffid_period,frac_min

USE conversions_mod,          ONLY: rsec_per_day

USE jules_surface_types_mod,  ONLY: soil

!Functions
USE calc_c_comps_triffid_mod, ONLY: calc_c_comps_triffid

IMPLICIT NONE

INTEGER, INTENT(IN) :: land_pts, nsurft, nnpft, nmasst

TYPE(ainfo_type), INTENT(IN OUT) :: ainfo
TYPE(progs_type), INTENT(IN) :: progs

INTEGER :: l,n,k

!End of header
!-----------------------------------------------------------------------------

IF (l_red .AND. l_triffid) THEN

  ! Set pointers to the prognostic fields
  red_state%plantNumDensity => progs%plantNumDensity
  veg_state%frac => ainfo%frac_surft
  veg_state%canht => progs%canht_pft
  veg_state%lai => progs%lai_pft

  !-----------------------------------------------------------------------------
  ! AJW The following code section  temporarily sets up the veg3 model
  ! In further revisions these fields will imported from the dump
  !-----------------------------------------------------------------------------

  veg_state%frac = 0.0
  DO n = 1, nnpft
    red_state%plantNumDensity(:,n,1:red_parms%mclass(n)) =                     &
      0.1 / SUM(red_state%crwn_area_mass(n,:))
  END DO

  veg_state%phen(:,:) = 1.0
  ! phen is a prognostic to be added to model - hardwired here to 1 for now

  ! Initialise veg state from red prognostic
  CALL red_veg3_couple(ainfo)

  ! Temp set rest to bareground
  veg_state%frac(:,soil) = 1 - SUM(veg_state%frac(:,:))

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
USE jules_surface_types_mod,      ONLY: nnpft
USE jules_surface_mod,            ONLY: cmass
USE pftparm,                      ONLY: lma
USE gridbox_mean_mod,             ONLY: pfttiles_to_gbm,                       &
                                        masstiles_to_pfttiles

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
! None at the moment

!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
TYPE(ainfo_type),INTENT(IN OUT) :: ainfo

!Local
INTEGER :: l,n,k  ! Index variables.

!-----------------------------------------------------------------------------
!end of header

veg_state%vegCpft(:,:)  = 0.0
veg_state%lai_bal(:,:)  = 0.0
veg_state%canht(:,:)    = 0.0
veg_state%frac(:,:)     = 0.0

veg_state%lai_bal = masstiles_to_pfttiles(red_state%lai_bal_mass,ainfo,        &
                        red_state%plantNumDensity, red_state%crwn_area_mass)
veg_state%canht   = masstiles_to_pfttiles(red_state%ht_mass,ainfo,             &
                        red_state%plantNumDensity, red_state%crwn_area_mass)

DO n = 1,nnpft
  DO l = 1,land_pts
    ! Convert from plant number on mass classes to area on PFTS
    ! and biomass to carbon
    DO k = 1,nmasst
      veg_state%frac(l,n) = veg_state%frac(l,n)                                &
        + red_state%plantNumDensity(l,n,k) * red_state%crwn_area_mass(n,k)
      veg_state%vegCpft(l,n) = veg_state%vegCpft(l,n)                          &
        + red_state%plantNumDensity(l,n,k) * red_state%mass_mass(n,k)
    END DO

    ! Convert to per m2 plant
    veg_state%vegCpft(l,n) = veg_state%vegCpft(l,n) / veg_state%frac(l,n)
    veg_state%lai_bal(l,n) = veg_state%lai_bal(l,n) / veg_state%frac(l,n)
    veg_state%canht(l,n)   = veg_state%canht(l,n) / veg_state%frac(l,n)

    veg_state%lai(l,n)     = veg_state%phen(l,n) * veg_state%lai_bal(l,n)

    veg_state%leafC(l,n) = cmass * lma(n) * veg_state%lai_bal(l,n)
    veg_state%rootC(l,n) = cmass * lma(n) * veg_state%lai_bal(l,n)
    ! Wood carbon balance of total minus leaf and root
    veg_state%woodC(l,n) = veg_state%vegCpft(l,n) - veg_state%leafC(l,n)       &
                            - veg_state%rootC(l,n)
  END DO
END DO

!Final aggregation to gridbox for vegetation carbon for diagnostic purposes
veg_state%vegC = pfttiles_to_gbm(veg_state%vegCpft,ainfo,frac_surft_in         &
               = veg_state%frac)

RETURN
END SUBROUTINE red_veg3_couple
!-----------------------------------------------------------------------------
END MODULE veg3_field_mod
