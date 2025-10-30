
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
                veg_state,red_state,                                           &
                ! OUT Fields
                demographic_lit                                                &
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
! Reals with INTENT OUT
!-----------------------------------------------------------------------------
REAL, INTENT(OUT)      ::                                                      &
demographic_lit(land_pts,nnpft)
              ! Demographic litter across PFT area. (kgC/m2/s)

!-----------------------------------------------------------------------------
!Local Vars
!-----------------------------------------------------------------------------
INTEGER                ::l,n,k,j

REAL                   ::                                                      &
p(land_pts,nnpft),                                                             &
              !  The total PFT carbon assimilate across the gridbox. (kgC/m2/s)
P_s(land_pts,nnpft),                                                           &
              !  Total gridbox carbon assimilate devoted to recruitment. (kgC/m2/s)
g(land_pts,nnpft),                                                             &
              !  Total gridbox carbon assimilate devoted to vegetation structural growth. (kgC/m2/s)
g0(land_pts,nnpft),                                                            &
              !  Boundary growth for an individual member of the smallest mass cohort. (kgC/year)
g_mass(land_pts,nnpft,nmasst),                                                 &
              !  Individual growth across the mass cohorts. (kgC/s)
plantNumDensity_g_sum(land_pts,nnpft),                                         &
              !  Summation of the relative cohort contribution towards the total PFT assimilate (/m2)
frac_shade(land_pts,nnpft),                                                    &
              !  Competitive shading of seedlings in each PFT
dplantNumDensity_dt(land_pts,nnpft,nmasst),                                    &
              !  Net rate of change of population density within each mass cohort. (m2/s)
flux_in(land_pts,nnpft,nmasst),                                                &
              ! Rate of change of population growing into a mass cohort. (m2/s)
flux_out(land_pts,nnpft,nmasst),                                               &
              ! Rate of change of population growing out of a mass cohort. (m2/s)
frac_check(land_pts,nnpft)
              !  The difference between the minimum vegetation fraction and the updated fraction. (-)


!End of headers

! Initialise vars
demographic_lit(:,:)        = 0.0
p(:,:)                      = 0.0
P_s(:,:)                    = 0.0
g(:,:)                      = 0.0
g0(:,:)                     = 0.0
g_mass(:,:,:)               = 0.0
plantNumDensity_g_sum(:,:)  = 0.0
frac_shade(:,:)             = 0.0
frac_check(:,:)             = 0.0
dplantNumDensity_dt(:,:,:)  = 0.0
flux_in(:,:,:)              = 0.0
flux_out(:,:,:)             = 0.0

! Loop finds the growth boundary condition, the fraction of shade for seedlings

DO k = 1,nmasst
  DO n = 1,nnpft
    DO l = 1,land_pts
      IF (k < red_parms%mclass(n)) THEN
        ! Sum product of the number density and the allometric scaling
        plantNumDensity_g_sum(l,n) = plantNumDensity_g_sum(l,n)                &
          + red_state%plantNumDensity(l,n,k) * red_state%g_mass_scale(n,k)

      ELSE IF (k == red_parms%mclass(n)) THEN
        plantNumDensity_g_sum(l,n) = plantNumDensity_g_sum(l,n)                &
          + red_state%plantNumDensity(l,n,k) * red_state%g_mass_scale(n,k)

        ! Partition the growth
        p(l,n) = veg_state%frac(l,n) * growth(l,n)
        P_s(l,n) = red_parms%alpha_recrt(n) * p(l,n)
        g(l,n) = (1.0 - red_parms%alpha_recrt(n)) * p(l,n)

        IF (plantNumDensity_g_sum(l,n) > 0) THEN
          g0(l,n) = g(l,n) / plantNumDensity_g_sum(l,n)
        END IF

        IF (growth(l,n)<0) THEN
          p(l,n) = 0.0
          P_s(l,n) = 0.0
          g(l,n) = 0.0
        END IF

        !Extra loop through to calculate shading
        DO j = 1, nnpft
          frac_shade(l,n) = MIN(1.0,frac_shade(l,n) + red_parms%comp_coef(n,j) &
              * veg_state%frac(l,j))
        END DO

      END IF
    END DO
  END DO
END DO

! Loop updates the demographic distribution.
DO k = 1, nmasst
  DO n = 1, nnpft
    DO l = 1, land_pts

      g_mass(l,n,k) = g0(l,n) * red_state%g_mass_scale(n,k)

      IF (growth(l,n)<0) THEN
        ! For negative growth we add to the mortality rate
        red_state%mort(l,n,k) = red_parms%mort_base(n) + mort_add(l,n,k)       &
          - g_mass(l,n,k) / red_state%mass_mass(n,k)
        g_mass(l,n,k) = 0.0
      ELSE
        red_state%mort(l,n,k) = red_parms%mort_base(n) + mort_add(l,n,k)
      END IF

      IF (k == 1) THEN
        ! Seedling flux
        flux_in(l,n,k) = P_s(l,n) / red_state%mass_mass(n,k)                   &
          * (1.0 - frac_shade(l,n))
        demographic_lit(l,n) = demographic_lit(l,n) + frac_shade(l,n) * P_s(l,n)
      ELSE
        ! Flux into mass class
        flux_in(l,n,k) = flux_out(l,n,k-1)
      END IF

      IF (k == red_parms%mclass(n)) THEN
        ! Truncate growth at the top mass class
        flux_out(l,n,k) = 0.0
        demographic_lit(l,n) = demographic_lit(l,n)                            &
          + red_state%plantNumDensity(l,n,k) * g_mass(l,n,k)
      ELSE IF (k < red_parms%mclass(n)) THEN
        flux_out(l,n,k) = red_state%plantNumDensity(l,n,k)                     &
          * g_mass(l,n,k) / (red_state%mass_mass(n,k+1)                        &
          - red_state%mass_mass(n,k))
      END IF
        ! Update the number density

      !Only update over the given mass classes
      IF (k <= red_parms%mclass(n)) THEN
        dplantNumDensity_dt(l,n,k) = flux_in(l,n,k) - flux_out(l,n,k)          &
          - red_state%mort(l,n,k) * red_state%plantNumDensity(l,n,k)

        !Prevent the mass class from being exhausted over a timestep
        IF (red_state%plantNumDensity(l,n,k)                                   &
            + (dplantNumDensity_dt(l,n,k) * dt) < 0.0 ) THEN
          dplantNumDensity_dt(l,n,k) = -red_state%plantNumDensity(l,n,k) / dt
          demographic_lit(l,n) =  demographic_lit(l,n)                         &
            + (dplantNumDensity_dt(l,n,k) - flux_out(l,n,k))                   &
            * red_state%mass_mass(n,k)
        ELSE
          demographic_lit(l,n) = demographic_lit(l,n) + red_state%mort(l,n,k)  &
          * red_state%plantNumDensity(l,n,k) * red_state%mass_mass(n,k)

        END IF
        red_state%plantNumDensity(l,n,k) = red_state%plantNumDensity(l,n,k)    &
            + dplantNumDensity_dt(l,n,k) * dt
        frac_check(l,n) = frac_check(l,n) + red_state%plantNumDensity(l,n,k)   &
          * red_state%crwn_area_mass(n,k)

        ! If the resultant vegetation fraction is less than the minimum
        ! fraction, add trees to the lowest mass class to make up the difference
        IF (k == red_parms%mclass(n) .AND. frac_check(l,n)                     &
          < red_parms%frac_min(n)) THEN
          red_state%plantNumDensity(l,n,1) = red_state%plantNumDensity(l,n,1)  &
            +(red_parms%frac_min(n) - frac_check(l,n))                         &
            / red_state%crwn_area_mass(n,1)
          demographic_lit(l,n) = demographic_lit(l,n)                          &
            - (red_parms%frac_min(n) - frac_check(l,n))                        &
            * red_state%mass_mass(n,1) / red_state%crwn_area_mass(n,1) / dt
          frac_check(l,n) = red_parms%frac_min(n)

        END IF

        IF (k == red_parms%mclass(n)) THEN
          !  Convert demographic litter into correct dimensions (per PFT area
          ! rather than per grid-box area)
          demographic_lit(l,n) = demographic_lit(l,n) / frac_check(l,n)
        END IF
      END IF
    END DO
  END DO
END DO

END SUBROUTINE veg3_red_dynamic
!-----------------------------------------------------------------------------

END MODULE veg3_red_dynamic_mod
