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

MODULE veg3_parm_mod

USE jules_vegetation_mod, ONLY: l_red

IMPLICIT NONE

!Set up object for veg3 control

TYPE :: veg3_ctrl_type
  INTEGER :: land_pts,nsurft,npft,nnpft,soil,triffid_period,nstep_trif,nmasst, &
             phenol_period,nstep_phen
  REAL    :: timestep,frac_min,dt_red,dt_phen_360d
END TYPE veg3_ctrl_type

!Set up objects containing everything we need for litter calculation

TYPE :: litter_parm_type
  REAL, ALLOCATABLE ::                                                         &
    g_wood(:),              & ! Turnover rate for woody biomass (/s).
    g_root(:),              & ! Turnover rate for root biomass (/s).
    g_leaf(:)                 ! Turnover rate for leaf biomass (/s).
END TYPE litter_parm_type

TYPE :: red_parm_type
  INTEGER, ALLOCATABLE ::                                                      &
    mclass(:),                                                                 &
    dom_order(:)
  REAL, ALLOCATABLE ::                                                         &
    alpha_recrt(:),                                                            &
    crwn_area0(:),                                                             &
    height0(:),                                                                &
    lai_bal0(:),                                                               &
    mass0(:),                                                                  &
    massi(:),                                                                  &
    mort_base(:),                                                              &
    phi_a(:),                                                                  &
    phi_g(:),                                                                  &
    phi_h(:),                                                                  &
    phi_l(:),                                                                  &
    comp_coef(:,:),                                                            &
    frac_min(:),                                                               &
    mclass_geom_mult(:)
END TYPE red_parm_type

TYPE(veg3_ctrl_type)   :: veg3_ctrl
TYPE(litter_parm_type) :: litter_parms
TYPE(red_parm_type)    :: red_parms

!Private by default
PRIVATE

!Expose routines
PUBLIC :: veg3_parm_init, veg3_parm_allocate, check_jules_red_parms

!Expose data
PUBLIC :: veg3_ctrl, litter_parms, red_parms, l_red

!Expose data structures
PUBLIC :: veg3_ctrl_type, litter_parm_type, red_parm_type

!Allow external code to read but not write
PROTECTED :: litter_parms, veg3_ctrl, red_parms

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='VEG3_PARM_MOD'

CONTAINS
!-------------------------------------------------------------------------------

SUBROUTINE veg3_parm_allocate(land_pts,nsurft,nnpft,npft)

USE missing_data_mod, ONLY: rmdi, imdi

IMPLICIT NONE
INTEGER, INTENT(IN) :: land_pts, nsurft, nnpft, npft

!End of Header

! Set up litter_parms object

ALLOCATE(litter_parms%g_wood(nnpft))
ALLOCATE(litter_parms%g_leaf(nnpft))
ALLOCATE(litter_parms%g_root(nnpft))

! Allocate red_data_type
ALLOCATE(red_parms%alpha_recrt        (nnpft))
ALLOCATE(red_parms%crwn_area0         (nnpft))
ALLOCATE(red_parms%dom_order          (nnpft))
ALLOCATE(red_parms%height0            (nnpft))
ALLOCATE(red_parms%lai_bal0           (nnpft))
ALLOCATE(red_parms%mass0              (nnpft))
ALLOCATE(red_parms%massi              (nnpft))
ALLOCATE(red_parms%mclass             (nnpft))
ALLOCATE(red_parms%mort_base          (nnpft))
ALLOCATE(red_parms%phi_a              (nnpft))
ALLOCATE(red_parms%phi_g              (nnpft))
ALLOCATE(red_parms%phi_h              (nnpft))
ALLOCATE(red_parms%phi_l              (nnpft))
ALLOCATE(red_parms%frac_min           (nnpft))
ALLOCATE(red_parms%comp_coef          (nnpft,nnpft))
ALLOCATE(red_parms%mclass_geom_mult   (nnpft))

litter_parms%g_wood          = rmdi
litter_parms%g_leaf          = rmdi
litter_parms%g_root          = rmdi

red_parms%alpha_recrt        = rmdi
red_parms%crwn_area0         = rmdi
red_parms%dom_order          = rmdi
red_parms%height0            = rmdi
red_parms%lai_bal0           = rmdi
red_parms%mass0              = rmdi
red_parms%massi              = rmdi
red_parms%mclass             = imdi
red_parms%mort_base          = rmdi
red_parms%phi_a              = rmdi
red_parms%phi_g              = rmdi
red_parms%phi_h              = rmdi
red_parms%phi_l              = rmdi
red_parms%frac_min           = rmdi
red_parms%comp_coef          = rmdi
red_parms%mclass_geom_mult   = rmdi

RETURN
END SUBROUTINE veg3_parm_allocate

!-------------------------------------------------------------------------------
SUBROUTINE veg3_set_parms(land_pts,nsurft,nnpft,npft,nmasst)

!Source parms, etc from io modules - these should be available on and offline

USE pftparm,                  ONLY: g_leaf_0
USE trif,                     ONLY: g_root, g_wood
! Above only allocated if triffid on - needs to be addressed
USE jules_vegetation_mod,     ONLY: l_triffid, triffid_period,frac_min,        &
                                    phenol_period

USE red_io,                   ONLY: alpha_recrt, crwn_area0, dom_order,        &
                                    height0, lai_bal0, mass0, massi, mclass,   &
                                    mort_base, phi_a, phi_g, phi_h, phi_l
!Get the timestep length
USE timestep_mod,             ONLY: timestep
USE conversions_mod,          ONLY: rsec_per_day

USE jules_surface_types_mod,  ONLY: soil

IMPLICIT NONE

INTEGER, INTENT(IN) :: land_pts, nsurft, nnpft, npft, nmasst

INTEGER :: n,k

!End of header

IF (l_red .AND. l_triffid) THEN

  ! Allocate memory for veg3 objects
  !  CALL veg3_parm_allocate(land_pts,nsurft,nnpft,npft)

  ! Setup parameters/control

  !Get the timestep length
  !Convert JULES to real number - important for call later
  !This way call occurs on same timestep on or offline - addresses issue in
  !current setup such that REAL is always used

  veg3_ctrl%timestep = REAL(timestep)
  veg3_ctrl%triffid_period = triffid_period
  veg3_ctrl%dt_red = rsec_per_day * REAL(veg3_ctrl%triffid_period)
  veg3_ctrl%nstep_trif = INT(rsec_per_day * veg3_ctrl%triffid_period           &
    / veg3_ctrl%timestep)

  veg3_ctrl%phenol_period = phenol_period
  veg3_ctrl%dt_phen_360d = REAL(veg3_ctrl%phenol_period) / 360.0
  veg3_ctrl%nstep_phen = INT(rsec_per_day * veg3_ctrl%phenol_period            &
    / veg3_ctrl%timestep)

  veg3_ctrl%land_pts = land_pts
  veg3_ctrl%nsurft   = nsurft
  veg3_ctrl%nnpft    = nnpft
  veg3_ctrl%npft     = npft
  veg3_ctrl%nmasst   = nmasst
  veg3_ctrl%soil     = soil
  veg3_ctrl%frac_min = frac_min

  ! Convert to s-1 from 360d-1
  litter_parms%g_wood(:) = g_wood(1:nnpft)   / rsec_per_day / 360.0
  litter_parms%g_leaf(:) = g_leaf_0(1:nnpft) / rsec_per_day / 360.0
  litter_parms%g_root(:) = g_root(1:nnpft)   / rsec_per_day / 360.0

  !RED
  red_parms%alpha_recrt(:)      = alpha_recrt(1:nnpft)
  red_parms%crwn_area0(:)       = crwn_area0(1:nnpft)
  red_parms%dom_order(:)        = dom_order(1:nnpft)
  red_parms%height0(:)          = height0(1:nnpft)
  red_parms%lai_bal0(:)         = lai_bal0(1:nnpft)
  red_parms%mass0(:)            = mass0(1:nnpft)
  red_parms%massi(:)            = massi(1:nnpft)
  red_parms%mclass(:)           = mclass(1:nnpft)
  red_parms%mort_base(:)        = mort_base(1:nnpft) / rsec_per_day / 360.0
  red_parms%phi_a(:)            = phi_a(1:nnpft)
  red_parms%phi_g(:)            = phi_g(1:nnpft)
  red_parms%phi_h(:)            = phi_h(1:nnpft)
  red_parms%phi_l(:)            = phi_l(1:nnpft)
  red_parms%mclass_geom_mult(:) = 1.0 ! Default assumes 1 mass class
  red_parms%frac_min(:)         = frac_min

  red_parms%comp_coef(:,:)      = 0.0

  DO n = 1,nnpft
    ! Cycle through the PFTs
    ! Update mclass_geom_mult for each PFT
    IF (red_parms%mclass(n) > 1) THEN
      red_parms%mclass_geom_mult(n) =                                          &
        (red_parms%massi(n) / red_parms%mass0(n))**                            &
        (1.0 / REAL(red_parms%mclass(n)-1))
    END IF

    DO k=1,nnpft
      ! If the n'th PFT is less dominant than k'th PFT, then k shades n.
      IF (dom_order(n)  <=  dom_order(k)) THEN
        red_parms%comp_coef(n,k) = 1.0
      END IF

    END DO
  END DO

END IF

RETURN
END SUBROUTINE veg3_set_parms

!-------------------------------------------------------------------------------

SUBROUTINE veg3_parm_init(land_pts,nsurft,nnpft,npft,nmasst)

IMPLICIT NONE

INTEGER, INTENT(IN) :: land_pts, nnpft, npft, nsurft, nmasst

!End of header

CALL veg3_set_parms(land_pts,nsurft,nnpft,npft,nmasst)

RETURN
END SUBROUTINE veg3_parm_init

!-------------------------------------------------------------------------------

SUBROUTINE check_jules_red_parms()

USE ereport_mod,     ONLY: ereport
USE jules_print_mgr, ONLY: jules_print, jules_message

IMPLICIT NONE

! Work variables
INTEGER :: error_sum
CHARACTER(LEN=*), PARAMETER :: RoutineName='CHECK_JULES_RED_PARMS'

!-----------------------------------------------------------------------------
! Check that all required variables were present in the namelist.
! The namelist variables were initialised to rmdi.
!-----------------------------------------------------------------------------
error_sum = 0
IF ( l_red ) THEN
  IF ( ANY( red_parms%alpha_recrt(:) < 0 ) ) THEN
    error_sum = error_sum + 1
    CALL jules_print(RoutineName, "No value for alpha_recrt")
  END IF
  IF ( ANY( red_parms%crwn_area0(:) < 0 ) ) THEN
    error_sum = error_sum + 1
    CALL jules_print(RoutineName, "No value for crwn_area0")
  END IF
  IF ( ANY( red_parms%dom_order(:) < 0 ) ) THEN
    error_sum = error_sum + 1
    CALL jules_print(RoutineName, "No value for dom_order")
  END IF
  IF ( ANY( red_parms%height0(:) < 0 ) ) THEN
    error_sum = error_sum + 1
    CALL jules_print(RoutineName, "No value for height0")
  END IF
  IF ( ANY( red_parms%lai_bal0(:) < 0 ) ) THEN
    error_sum = error_sum + 1
    CALL jules_print(RoutineName, "No value for lai_bal0")
  END IF
  IF ( ANY( red_parms%mass0(:) < 0 ) ) THEN
    error_sum = error_sum + 1
    CALL jules_print(RoutineName, "No value for mass0")
  END IF
  IF ( ANY( red_parms%massi(:) < 0 ) ) THEN
    error_sum = error_sum + 1
    CALL jules_print(RoutineName, "No value for massi")
  END IF
  IF ( ANY( red_parms%mclass(:) < 0 ) ) THEN
    error_sum = error_sum + 1
    CALL jules_print(RoutineName, "No value for mclass")
  END IF
  IF ( ANY( red_parms%mort_base(:) < 0 ) ) THEN
    error_sum = error_sum + 1
    CALL jules_print(RoutineName, "No value for mort_base")
  END IF
  IF ( ANY( red_parms%phi_a(:) < 0 ) ) THEN
    error_sum = error_sum + 1
    CALL jules_print(RoutineName, "No value for phi_a")
  END IF
  IF ( ANY( red_parms%phi_g(:) < 0 ) ) THEN
    error_sum = error_sum + 1
    CALL jules_print(RoutineName, "No value for phi_g")
  END IF
  IF ( ANY( red_parms%phi_h(:) < 0 ) ) THEN
    error_sum = error_sum + 1
    CALL jules_print(RoutineName, "No value for phi_h")
  END IF
  IF ( ANY( red_parms%phi_l(:) < 0 ) ) THEN
    error_sum = error_sum + 1
    CALL jules_print(RoutineName, "No value for phi_l")
  END IF
END IF

! Defining errors ends here. Now issue FATAL ereport.
IF ( error_sum > 0 ) THEN
  CALL ereport(RoutineName, error_sum,                                         &
     "Variable(s) missing from namelist - see earlier " //                     &
     "error message(s)")
END IF

END SUBROUTINE check_jules_red_parms

END MODULE veg3_parm_mod
