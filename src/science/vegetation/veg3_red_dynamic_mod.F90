
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

MODULE veg3_red_dynamic_mod

IMPLICIT NONE

PRIVATE
PUBLIC :: veg3_red_dynamic

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='VEG3_RED_DYNAMIC_MOD'

CONTAINS

!-----------------------------------------------------------------------------
SUBROUTINE veg3_red_dynamic(                                                   &
                !IN Control vars
                dt,veg_index_pts,veg_index,veg3_ctrl,land_pts,                 &
                nnpft,nmasst,                                                  &
                !IN red_parms
                red_parms,                                                     &
                !IN fields
                growth,mort_add,                                               &
                !IN state
                veg_state,red_state                                            &
                !OUT Diagnostics
                )

!Only get the data structures - the data comes through the calling tree
USE veg3_parm_mod,ONLY:  red_parm_type, veg3_ctrl_type
USE veg3_field_mod,ONLY:  veg_state_type, red_state_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Objects with INTENT IN
!-----------------------------------------------------------------------------
TYPE(red_state_type)  :: red_state
TYPE(red_parm_type)   :: red_parms
TYPE(veg_state_type)  :: veg_state
TYPE(veg3_ctrl_type)  :: veg3_ctrl

!----------------------------------------------------------------------------
! Integers with INTENT IN
!----------------------------------------------------------------------------
INTEGER, INTENT(IN) :: land_pts,nnpft,veg_index(land_pts),veg_index_pts,nmasst

!----------------------------------------------------------------------------
! Reals with INTENT IN
!----------------------------------------------------------------------------
REAL, INTENT(IN)   ::                                                          &
growth(land_pts,nnpft),                                                        &
              !  The total carbon assimilate across the PFT area. (kgC/m2/s)
mort_add(land_pts,nnpft,nmasst),                                               &
              !  Additional plant mortality across plant mass (/s)
dt
              !  Dynamic vegetation time-step (s)

!-----------------------------------------------------------------------------
!Local Vars
!-----------------------------------------------------------------------------
INTEGER                ::l,n,k,j

REAL                   ::                                                      &
P_s(land_pts,nnpft),                                                           &
              !  Total gridbox carbon assimilate devoted to recruitment. 
              ! (kgC/m2/s)
g0(land_pts,nnpft),                                                            &
              !  Boundary growth for an individual member of the smallest mass
              !  cohort. (kgC/year)
neg_growth(land_pts,nnpft),                                                    &
              !  Uniform PFT-carbon-proportional loss rate for when growth is
              !  negative (e.g., local litterfall exceeds NPP). Drives a
              !  downward mass-class shrinkage flux, and is only applied as a
              !  mortality rate for the lowest mass class. (/s)
frac_shade(land_pts,nnpft)
              !  Competitive shading of seedlings in each PFT.


!End of headers

! Initialise vars
veg_state%mort_litC(:,:)    = 0.0
P_s(:,:)                    = 0.0
g0(:,:)                     = 0.0
neg_growth(:,:)             = 0.0
frac_shade(:,:)             = 0.0

! Dynamic demographic loop to update the number density of each PFT across the
! PFT mass classes.
DO l = 1,land_pts
  DO n = 1,nnpft
    ! Call to partition the PFT growth onto the mass class structure.
    CALL growth_onto_mass_class(                                               &
      !IN sizing
      red_parms%mclass(n),                                                     &
      !IN PFT parameters
      red_parms%alpha_recrt(n),                                                &
      !IN fields
      veg_state%frac(l,n),growth(l,n),veg_state%vegCpft(l,n),                  &
      !IN mass-cohort properties
      red_state%plantNumDensity(l,n,1:red_parms%mclass(n)),                    &
      red_state%g_mass_scale(n,1:red_parms%mclass(n)),                         &
      !OUT fields
      P_s(l,n),g0(l,n),neg_growth(l,n)                                         &
      )

    !Estimate the inter-PFT competition.
    DO j = 1, nnpft
      frac_shade(l,n) = MIN(1.0,frac_shade(l,n) + red_parms%comp_coef(n,j)    &
          * veg_state%frac(l,j))
    END DO

    ! Call to update the PFT number density.
    CALL update_pft_size_structure(                                           &
      !IN sizing
      red_parms%mclass(n),                                                    &
      !IN Control vars
      dt,                                                                     &
      !IN PFT parameters
      red_parms%mort_base(n),red_parms%frac_min(n),                           &
      !IN fields
      mort_add(l,n,1:red_parms%mclass(n)),growth(l,n),neg_growth(l,n),        &
      P_s(l,n),g0(l,n),frac_shade(l,n),                                       &
      !IN mass-cohort properties
      red_state%g_mass_scale(n,1:red_parms%mclass(n)),                        &
      red_state%mass_mass(n,1:red_parms%mclass(n)),                           &
      red_state%crwn_area_mass(n,1:red_parms%mclass(n)),                      &
      !INOUT state
      red_state%plantNumDensity(l,n,1:red_parms%mclass(n)),                   &
      red_state%mort(l,n,1:red_parms%mclass(n)),                              &
      !OUT diagnostics
      veg_state%mort_litC(l,n)                                                &
      )

      ! Divide mort_litC by the PFT fraction (not restimated yet)
      IF (veg_state%frac(l,n) > 0.0) THEN
        veg_state%mort_litC(l,n) = veg_state%mort_litC(l,n) /                 &
          veg_state%frac(l,n)
      ELSE
        veg_state%mort_litC(l,n) = 0.0
      END IF

  END DO
END DO

END SUBROUTINE veg3_red_dynamic
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
SUBROUTINE growth_onto_mass_class(                                             &
                !IN sizing
                mclass,                                                        &
                !IN PFT parameters
                alpha_recrt,                                                   &
                !IN fields
                frac,growth,vegCpft,                                           &
                !IN mass-cohort properties
                plantNumDensity,g_mass_scale,                                  &
                !OUT fields
                P_s,g0,neg_growth                                              &
                )

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
alpha_recrt,                                                                   &
              !  Fraction of PFT growth devoted to recruitment. (-)
frac,                                                                          &
              !  PFT fraction across the gridbox. (-)
growth,                                                                        &
              !  The total carbon assimilate across the PFT area. (kgC/m2/s)
vegCpft,                                                                       &
              !  The total PFT vegetation carbon. (kgC/m2)
plantNumDensity(mclass),                                                       &
              !  Population density within each mass cohort. (/m2)
g_mass_scale(mclass)
              !  Allometric scaling of growth across the mass cohorts.

!-----------------------------------------------------------------------------
! Reals with INTENT OUT
!-----------------------------------------------------------------------------
REAL, INTENT(OUT)    ::                                                        &
P_s,                                                                           &
              !  Gridbox carbon assimilate devoted to recruitment. (kgC/m2/s)
g0,                                                                            &
              !  Boundary growth for an individual member of the smallest mass
              !  cohort. (kgC/year)
neg_growth
              !  Uniform PFT-carbon-proportional loss rate for when growth is
              !  negative (e.g., local litterfall exceeds NPP). Drives a
              !  downward mass-class shrinkage flux, and is only applied as a
              !  mortality rate for the lowest mass class. (/s)

!-----------------------------------------------------------------------------
!Local Vars
!-----------------------------------------------------------------------------
INTEGER              :: k

REAL                 ::                                                        &
p,                                                                             &
              !  The total PFT carbon assimilate across the gridbox. (kgC/m2/s)
g,                                                                             &
              !  Total gridbox carbon assimilate devoted to vegetation
              ! structural growth. (kgC/m2/s)
plantNumDensity_g_sum
              !  Summation of the relative cohort contribution towards the
              !  total PFT assimilate (/m2)

!End of headers

! Initialise vars
P_s                     = 0.0
g0                      = 0.0
neg_growth              = 0.0
plantNumDensity_g_sum   = 0.0

! Sum product of the number density and the allometric scaling
DO k = 1, mclass
  plantNumDensity_g_sum = plantNumDensity_g_sum                                &
    + plantNumDensity(k) * g_mass_scale(k)
END DO

! Partition the growth into recruitment and structural growth
p = frac * growth
P_s = alpha_recrt * p
g = (1.0 - alpha_recrt) * p

IF (plantNumDensity_g_sum > 0) THEN
  g0 = g / plantNumDensity_g_sum
END IF

IF (growth < 0.0) THEN
  P_s = 0.0

  ! Estimate the uniform PFT-carbon-proportional loss rate required to
  ! account for the negative growth (local litterfall exceeding NPP).
  IF (vegCpft > 0) THEN
    neg_growth = - growth / vegCpft

  END IF

END IF

END SUBROUTINE growth_onto_mass_class
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
SUBROUTINE update_pft_size_structure(                                          &
                !IN sizing
                mclass,                                                        &
                !IN Control vars
                dt,                                                            &
                !IN PFT parameters
                mort_base,frac_min,                                            &
                !IN fields
                mort_add,growth,neg_growth,P_s,g0,frac_shade,                  &
                !IN mass-cohort properties
                g_mass_scale,mass_mass,crwn_area_mass,                         &
                !INOUT state
                plantNumDensity,mort,                                          &
                !OUT diagnostics
                mort_litC                                                      &
                )

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Integers with INTENT IN
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) :: mclass
              !  Number of mass classes for this PFT.

!-----------------------------------------------------------------------------
! Reals with INTENT IN
!-----------------------------------------------------------------------------
REAL, INTENT(IN)     ::                                                         &
dt,                                                                             &
              !  Dynamic vegetation time-step (s)
mort_base,                                                                      &
              !  Background mortality rate for this PFT. (/s)
frac_min,                                                                       &
              !  Minimum vegetation fraction for this PFT. (-)
mort_add(mclass),                                                               &
              !  Additional plant mortality across plant mass (/s)
growth,                                                                         &
              !  The total carbon assimilate across the PFT area. (kgC/m2/s)
neg_growth,                                                                     &
              !  Uniform PFT-carbon-proportional loss rate for when growth is
              !  negative (e.g., local litterfall exceeds NPP). Drives a
              !  downward mass-class shrinkage flux, and is only applied as a
              !  mortality rate for the lowest mass class. (/s)
P_s,                                                                            &
              !  Gridbox carbon assimilate devoted to recruitment. (kgC/m2/s)
g0,                                                                             &
              !  Boundary growth for an individual member of the smallest mass
              !  cohort. (kgC/year)
frac_shade,                                                                     &
              !  Competitive shading of seedlings in this PFT.
g_mass_scale(mclass),                                                           &
              !  Allometric scaling of growth across the mass cohorts.
mass_mass(mclass),                                                              &
              !  Mass of an individual member of each mass cohort. (kgC)
crwn_area_mass(mclass)
              !  Crown area of an individual member of each mass cohort. (m2)

!-----------------------------------------------------------------------------
! Reals with INTENT INOUT
!-----------------------------------------------------------------------------
REAL, INTENT(INOUT)  ::                                                        &
plantNumDensity(mclass),                                                       &
              !  Population density within each mass cohort. (/m2)
mort(mclass)
              !  Mortality rate within each mass cohort. (/s)

!-----------------------------------------------------------------------------
! Reals with INTENT OUT
!-----------------------------------------------------------------------------
REAL, INTENT(OUT)    :: mort_litC
              !  Mortality/demographic litter for this PFT, normalised per
              !  unit PFT canopy area. (kgC/m2/s)

!-----------------------------------------------------------------------------
!Local Vars
!-----------------------------------------------------------------------------
INTEGER              :: k

REAL                 ::                                                        &
g_mass(mclass),                                                                &
              !  Individual growth across the mass cohorts. (kgC/s)
dplantNumDensity_dt(mclass),                                                   &
              !  Net rate of change of population density within each mass
              ! cohort. (m2/s)
flux_in(mclass),                                                               &
              ! Rate of change of population growing into a mass cohort. (m2/s)
flux_out(mclass),                                                              &
              ! Rate of change of population growing out of a mass cohort. 
              ! (m2/s)
frac_check
              !  The difference between the minimum vegetation fraction and the 
              !  updated fraction. (-)

!End of headers

! Initialise vars
mort_litC               = 0.0
frac_check              = 0.0
g_mass(:)               = 0.0
dplantNumDensity_dt(:)  = 0.0
flux_in(:)              = 0.0
flux_out(:)             = 0.0

DO k = 1, mclass

  IF (growth < 0.0) THEN
    ! Negative growth is represented as a downward shrinkage flux of
    ! individuals through the mass classes, the lowest mass class has 
    ! nowhere lower to shrink into, so its share of the loss is instead matched
    ! by an equivalent mortality rate.
    g_mass(k) = 0.0

    IF (k == 1) THEN
      mort(k) = mort_base + mort_add(k) + neg_growth
    ELSE
      mort(k) = mort_base + mort_add(k)
    END IF

    IF (k < mclass) THEN
      ! Flux shrinking down into this class from the class above
      flux_in(k) = plantNumDensity(k+1) * neg_growth * mass_mass(k+1)          &
        / (mass_mass(k+1) - mass_mass(k))
    ELSE
      flux_in(k) = 0.0
    END IF

    IF (k > 1) THEN
      ! Flux shrinking out of this class into the class below
      flux_out(k) = plantNumDensity(k) * neg_growth * mass_mass(k)             &
        / (mass_mass(k) - mass_mass(k-1))
    ELSE
      flux_out(k) = 0.0
    END IF

  ELSE
    g_mass(k) = g0 * g_mass_scale(k)
    mort(k) = mort_base + mort_add(k)

    IF (k == 1) THEN
      ! Seedling flux
      flux_in(k) = P_s / mass_mass(k) * (1.0 - frac_shade)
      mort_litC = mort_litC + frac_shade * P_s

    ELSE
      ! Flux into mass class
      flux_in(k) = flux_out(k-1)

    END IF

    IF (k == mclass) THEN
      ! Truncate growth at the top mass class
      flux_out(k) = 0.0
      mort_litC = mort_litC + plantNumDensity(k) * g_mass(k)

    ELSE
      flux_out(k) = plantNumDensity(k) * g_mass(k) / (mass_mass(k+1)           &
        - mass_mass(k))

    END IF

  END IF

  ! Update the number density
  dplantNumDensity_dt(k) = flux_in(k) - flux_out(k)                            &
    - mort(k) * plantNumDensity(k)

  !Prevent the mass class from being exhausted over a timestep
  IF (plantNumDensity(k)                                                       &
      + (dplantNumDensity_dt(k) * dt) < 0.0 ) THEN
    dplantNumDensity_dt(k) = -plantNumDensity(k) / dt
    mort_litC =  mort_litC                                                      &
      + (dplantNumDensity_dt(k) - flux_out(k))                                 &
      * mass_mass(k)

  ELSE
    mort_litC = mort_litC + mort(k)                                            &
    * plantNumDensity(k) * mass_mass(k)

  END IF

  plantNumDensity(k) = plantNumDensity(k)                                      &
      + dplantNumDensity_dt(k) * dt
  frac_check = frac_check + plantNumDensity(k)                                 &
    * crwn_area_mass(k)

END DO

! If the resultant vegetation fraction is less than the minimum
! fraction, add trees to the lowest mass class to make up the difference
IF (frac_check < frac_min) THEN
  plantNumDensity(1) = plantNumDensity(1)                                      &
    +(frac_min - frac_check) / crwn_area_mass(1)
END IF

END SUBROUTINE update_pft_size_structure
!-----------------------------------------------------------------------------

END MODULE veg3_red_dynamic_mod
