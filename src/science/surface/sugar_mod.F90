!******************************COPYRIGHT********************************************
! (c) University of Exeter 2022
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT********************************************

MODULE sugar_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SUGAR_MOD'

PRIVATE
PUBLIC sugar

CONTAINS
! *********************************************************************
! Routines to calculate the utilisation of non-structural carbohydrate
! by respiration and growth for each PFT from the SUGAR model
!
! References:
!   Jones et al., 2020, Biogeosciences, 17, 3589-3612,
!        https://doi.org/10.5194/bg-17-3589-2020
! *********************************************************************

SUBROUTINE sugar(ft, leafc, leafc_bal, woodc, rootc, f_nsc, resp_l,            &
                 resp_w, resp_r, resp_p_m, resp_p_g,                           &
                 resp_p, growth, tstar, gpp)

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) ::                                                         &
 ft
    ! IN Plant functional type

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 tstar                                                                         &
    ! IN Surface temperature (K)
,gpp                                                                           &
    ! IN Gross Primary Productivity (kg C/m2)
,leafc                                                                         &
    ! IN (Phenological) Leaf carbon (kg C/m2)
,leafc_bal                                                                     &
    ! IN (Balanced) Leaf carbon (kg C/m2)
,woodc                                                                         &
    ! IN Wood carbon (kg C/m2)
,rootc
    ! IN Root carbon (kg C/m2).

!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
f_nsc
    ! INOUT NSC mass fraction (kg kg-1)

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 resp_w                                                                        &
    ! OUT Wood maintenance respiration rate (kg C/m2/s).
,resp_r                                                                        &
    ! OUT Root maintenance respiration rate (kg C/m2/s)
,resp_l                                                                        &
    ! OUT Leaf maintenance respiration rate (kg C/m2/s)
,resp_p_m                                                                      &
    ! OUT Plant maintenance respiration (kg C/m2/s)
,resp_p_g                                                                      &
    ! OUT Plant growth respiration (kg C/m2/s)
,resp_p                                                                        &
    ! OUT Plant respiration (kg C/m2/s)
,growth
    ! OUT Plant growth rate (kg C/m2/s)

!-----------------------------------------------------------------------------
! Local variables
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
 df_nsc
    ! WORK Increment in the NSC mass fraction

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SUGAR'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!------------------------------------------------------------------------------
! Calculate explicit fluxes using previous timestep f_nsc value
!------------------------------------------------------------------------------
CALL calculate_sug_fluxes(ft, leafc, woodc, rootc, f_nsc, resp_l,              &
                          resp_w, resp_r, resp_p_m, resp_p_g,                  &
                          resp_p, growth, tstar)

!------------------------------------------------------------------------------
! Update NSC mass fraction (f_nsc)
!------------------------------------------------------------------------------
CALL update_nsc_pool(ft, gpp, resp_p, growth, f_nsc, leafc, leafc_bal,         &
                     woodc, rootc, df_nsc, tstar)

!------------------------------------------------------------------------------
! Perform flux corrections to ensure they match the f_nsc increment
! This is only needed if a forward timestep weighting is used
! (which it is by default) but will still be performed if the explicit
! scheme (forw_gamma=0) is used (the corrections will just be zero).
!------------------------------------------------------------------------------
CALL sugar_flux_correction(ft, leafc_bal, leafc, woodc, rootc, df_nsc, resp_l, &
                           resp_w, resp_r, resp_p_m, resp_p_g,                 &
                           resp_p, growth, tstar, gpp)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

END SUBROUTINE sugar

SUBROUTINE calculate_sug_fluxes(ft, leafc, woodc, rootc, f_nsc, resp_l,        &
                                resp_w, resp_r, resp_p_m, resp_p_g,            &
                                resp_p, growth, tstar)

USE pftparm, ONLY:                                                             &
! imported parameters
    sug_grec, sug_g0, sug_yg, q10_leaf

USE conversions_mod, ONLY: zerodegc

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE
!------------------------------------------------------------------------------
! Arguments with INTENT(IN).
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
 ft
    ! IN Plant functional type

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 tstar                                                                         &
    ! IN Surface temperature (K)
,f_nsc                                                                         &
    ! INOUT NSC mass fraction (kg kg-1)
,leafc                                                                         &
    ! IN (Phenological) Leaf carbon (kg C/m2)
,woodc                                                                         &
    ! IN Wood carbon (kg C/m2)
,rootc
    ! IN Root carbon (kg C/m2).

!------------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 resp_w                                                                        &
    ! OUT Wood maintenance respiration rate (kg C/m2/s).
,resp_r                                                                        &
    ! OUT Root maintenance respiration rate (kg C/m2/s)
,resp_l                                                                        &
    ! OUT Leaf maintenance respiration rate (kg C/m2/s)
,resp_p_m                                                                      &
    ! OUT Plant maintenance respiration (kg C/m2/s)
,resp_p_g                                                                      &
    ! OUT Plant growth respiration (kg C/m2/s)
,resp_p                                                                        &
    ! OUT Plant respiration (kg C/m2/s)
,growth
    ! OUT Plant growth rate (kg C/m2/s)

!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
 r0                                                                            &
    ! WORK Specific rate of NSC utilisation by respiration
,g0                                                                            &
    ! WORK Specific rate of NSC utilisation by structural growth
,grec0                                                                         &
    ! WORK Specific rate of structural C decomposition
,f_temp                                                                        &
    ! WORK Temperature function used for respiration and growth rates
,growth_l                                                                      &
    ! WORK Leaf growth rate (kg C/m2/s)
,growth_w                                                                      &
    ! WORK Wood growth rate (kg C/m2/s)
,growth_r                                                                      &
    ! WORK Root growth rate (kg C/m2/s)
,grecl                                                                         &
    ! WORK Decay rate of structural leaf carbon into NSC (leaf maintenance)
,grecw                                                                         &
    ! WORK Decay rate of structural stem carbon into NSC (stem maintenance)
,grecr
    ! WORK Decay rate of structural root carbon into NSC (root maintenance)

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CALCULATE_SUG_FLUXES'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!------------------------------------------------------------------------------
! Calculate temperature function
!------------------------------------------------------------------------------
f_temp = q10_leaf(ft)**( 0.1 * (tstar - zerodegc - 25.0) )

!------------------------------------------------------------------------------
! Calculate temperature dependent coefficients
!------------------------------------------------------------------------------
r0    = sug_g0(ft)   * f_temp * ( 1.0 - sug_yg(ft) ) / sug_yg(ft)
g0    = sug_g0(ft)   * f_temp
grec0 = sug_grec(ft) * f_temp

!------------------------------------------------------------------------------
! Calculate fluxes
!------------------------------------------------------------------------------
resp_l   = r0 * f_nsc * leafc
resp_w   = r0 * f_nsc * woodc
resp_r   = r0 * f_nsc * rootc

grecl    = grec0 * leafc * (1.0 - f_nsc)
grecw    = grec0 * woodc * (1.0 - f_nsc)
grecr    = grec0 * rootc * (1.0 - f_nsc)

growth_l = g0 * f_nsc * leafc - grecl
growth_w = g0 * f_nsc * woodc - grecw
growth_r = g0 * f_nsc * rootc - grecr

growth   = growth_l + growth_w + growth_r
resp_p   = resp_l + resp_w + resp_r
resp_p_m = (1.0 - sug_yg(ft)) * (grecl + grecw + grecr) / sug_yg(ft)
resp_p_g = resp_p - resp_p_m

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

END SUBROUTINE calculate_sug_fluxes

SUBROUTINE update_NSC_pool(ft, gpp, resp_p, growth, f_nsc, leafc,              &
                           leafc_bal, woodc, rootc, df_nsc_corr, tstar)

USE timestep_mod, ONLY: timestep

USE pftparm, ONLY:                                                             &
! imported parameters
    sug_grec, sug_g0, sug_yg, q10_leaf

USE conversions_mod, ONLY: zerodegc

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
 ft
    ! IN Plant functional type

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 gpp                                                                           &
    ! IN Gross primary productivity (kg C/m2/s)
,tstar                                                                         &
    ! IN Surface temperature (K)
,leafc                                                                         &
    ! IN Phenological leaf carbon (kg/m2)
,leafc_bal                                                                     &
    ! IN Balanced leaf carbon (kg/m2)
,woodc                                                                         &
    ! IN Wood carbon (kg/m2)
,rootc                                                                         &
    ! IN Root carbon (kg/m2)
,resp_p                                                                        &
    ! IN Total plant respiration rate (kg C/m2/s)
,growth
    ! IN Total plant growth rate (kg C/m2/s)

!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
f_nsc
    ! INOUT NSC mass fraction (kg kg-1)

!------------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 df_nsc_corr
    ! OUT Corrected increment to NSC mass fraction

!-----------------------------------------------------------------------------
! Local variables
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
 r0                                                                            &
    ! WORK Specific rate of NSC utilisation by respiration
,g0                                                                            &
    ! WORK Specific rate of NSC utilisation by structural growth
,grec0                                                                         &
    ! WORK Specific rate of structural C decomposition
,f_temp                                                                        &
    ! WORK Temperature function used for respiration and growth rates
,c_veg                                                                         &
    ! WORK Total phenological vegetation carbon (kg C/m2)
    !      (leafC+woodC+rootC)
,c_veg_bal                                                                     &
    ! WORK Total balanced vegetation carbon (kg C/m2)
    !      (leafC_bal+woodC+rootC)
,df_nsc_expl                                                                   &
    ! WORK Explicit increment to NSC mass fractioon
,j_fnsc
    ! WORK Jacobian of f_nsc rate equation evaluated using previous time-step
    !      f_nsc value.

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  forw_gamma = 1.0
    ! Forward time-step weighting. (0 = explicit), (1 = approx implicit)

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='UPDATE_NSC_POOL'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!------------------------------------------------------------------------------
! Calculate temperature function
!------------------------------------------------------------------------------
f_temp = q10_leaf(ft)**( 0.1 * (tstar - zerodegc - 25.0) )

!------------------------------------------------------------------------------
! Calculate temperature dependent coefficients
!------------------------------------------------------------------------------
r0    = sug_g0(ft)   * f_temp * ( 1.0 - sug_yg(ft) ) / sug_yg(ft)
g0    = sug_g0(ft)   * f_temp
grec0 = sug_grec(ft) * f_temp

!-----------------------------------------------------------------------------
! Calculate balanced and phenological biomass
!-----------------------------------------------------------------------------
c_veg      = leafc     + woodc + rootc
c_veg_bal  = leafc_bal + woodc + rootc

!-----------------------------------------------------------------------------
! Calculate increment to NSC mass fraction
!-----------------------------------------------------------------------------
j_fnsc      = (2.0* r0 * f_nsc - (r0 + g0 + grec0))* c_veg / c_veg_bal         &
              - gpp / c_veg_bal

df_nsc_expl = timestep * (1.0 / c_veg_bal)                                     &
              * (( 1.0 - f_nsc ) * (gpp - resp_p) - growth)
df_nsc_corr = df_nsc_expl / (1.0 - forw_gamma * j_fnsc)
df_nsc_corr = MIN(1.0 - f_nsc, df_nsc_corr)
df_nsc_corr = MAX(-f_nsc,df_nsc_corr)

!-----------------------------------------------------------------------------
! Increment f_nsc to next timestep value
!-----------------------------------------------------------------------------
f_nsc = f_nsc + df_nsc_corr

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

END SUBROUTINE update_NSC_pool

SUBROUTINE sugar_flux_correction(ft, leafc_bal, leafc, woodc, rootc, df_nsc,   &
                                 resp_l, resp_w, resp_r, resp_p_m, resp_p_g,   &
                                 resp_p, growth, tstar, gpp)

USE pftparm, ONLY:                                                             &
! imported parameters
    sug_grec, sug_g0, sug_yg, q10_leaf

USE conversions_mod, ONLY: zerodegc

USE timestep_mod, ONLY: timestep

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) ::                                                         &
 ft
    ! IN Plant functional type

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 tstar                                                                         &
    ! IN Surface temperature (K)
,gpp                                                                           &
    ! IN Gross primary productivity (kg C/m2/s)
,leafc                                                                         &
    ! IN Phenological leaf carbon (kg/m2)
,leafc_bal                                                                     &
    ! IN Balanced leaf carbon (kg/m2)
,woodc                                                                         &
    ! IN Wood carbon (kg/m2)
,rootc                                                                         &
    ! IN Root carbon (kg/m2)
,df_nsc
    ! IN Increment change in NSC mass fraction

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 resp_w                                                                        &
    ! OUT Wood maintenance respiration rate (kg C/m2/s).
,resp_r                                                                        &
    ! OUT Root maintenance respiration rate (kg C/m2/s)
,resp_l                                                                        &
    ! OUT Leaf maintenance respiration rate (kg C/m2/s)
,resp_p_m                                                                      &
    ! OUT Plant maintenance respiration (kg C/m2/s)
,resp_p_g                                                                      &
    ! OUT Plant growth respiration (kg C/m2/s)
,resp_p                                                                        &
    ! OUT Plant respiration (kg C/m2/s)
,growth
    ! OUT Plant growth rate (kg C/m2/s)

!-----------------------------------------------------------------------------
! Local variables
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
 r0                                                                            &
    ! WORK Specific rate of NSC utilisation by respiration
,g0                                                                            &
    ! WORK Specific rate of NSC utilisation by structural growth
,grec0                                                                         &
    ! WORK Specific rate of structural C decomposition
,c_veg                                                                         &
    ! WORK Total phenological vegetation carbon (kg C/m2)
    !      (leafC+woodC+rootC)
,c_veg_bal                                                                     &
    ! WORK Total balanced vegetation carbon (kg C/m2)
    !      (leafC_bal+woodC+rootC)
,f_temp                                                                        &
    ! WORK Temperature function used for respiration and growth rates
,f_nsc_av                                                                      &
    ! WORK Average NSC mass fraction over time-step that balances rate of
    !      change equation
,a                                                                             &
    ! WORK Coefficient of second order term in quadratic function for
    !      average f_nsc over time-step
,b                                                                             &
    ! WORK Coefficient of first order term in quadratic function for
    !      average f_nsc over time-step
,c
    ! WORK Constant in quadratic function for average f_nsc over time-step

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SUGAR_FLUX_CORRECTION'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!------------------------------------------------------------------------------
! Calculate temperature function
!------------------------------------------------------------------------------
f_temp = q10_leaf(ft)**( 0.1 * (tstar - zerodegc - 25.0) )

!------------------------------------------------------------------------------
! Calculate temperature dependent coefficients
!------------------------------------------------------------------------------
r0    = sug_g0(ft)   * f_temp * ( 1.0 - sug_yg(ft) ) / sug_yg(ft)
g0    = sug_g0(ft)   * f_temp
grec0 = sug_grec(ft) * f_temp

!-----------------------------------------------------------------------------
! Calculate balanced and phenological biomass
!-----------------------------------------------------------------------------
c_veg      = leafc     + woodc + rootc
c_veg_bal  = leafc_bal + woodc + rootc

!-----------------------------------------------------------------------------
! Flux correction
! - need to recalculate fluxes to match corrected f_nsc increment
!-----------------------------------------------------------------------------
! Calculate average f_nsc value over timestep that corresponds to increment
a = r0 * c_veg / c_veg_bal
b = -(r0 + g0 + grec0) * c_veg / c_veg_bal - gpp / c_veg
c = gpp / c_veg + grec0 * c_veg / c_veg_bal - df_nsc / timestep
f_nsc_av = ( -b - ( b**2.0 - 4.0 * a * c )**0.5 ) / ( 2.0 * a )

! Recalculate fluxes with average f_nsc
CALL calculate_sug_fluxes(ft, leafc, woodc, rootc, f_nsc_av, resp_l,           &
                          resp_w, resp_r, resp_p_m, resp_p_g,                  &
                          resp_p, growth, tstar)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

END SUBROUTINE sugar_flux_correction

END MODULE sugar_mod
