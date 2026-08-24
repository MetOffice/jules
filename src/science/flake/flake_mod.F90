!-----------------------------------------------------------
! FLake is freely available under the terms of the MIT license.
!
! Copyright (c)
!
! Permission is hereby granted, free of charge, to any person obtaining
! a copy of this software and associated documentation files (the
! "Software"), to deal in the Software without restriction, including
! without limitation the rights to use, copy, modify, merge, publish,
! distribute, sublicense, and/or sell copies of the Software, and to
! permit persons to whom the Software is furnished to do so, subject to
! the following conditions:
!
! The above copyright notice and this permission notice shall be
! included in all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
! EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
! MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
! NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
! LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
! OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
! WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!-----------------------------------------------------------
! File %M% from Library %Q%
! Version %I% from %G% extracted: %H%
!------------------------------------------------------------------------------

MODULE flake

!------------------------------------------------------------------------------
!
! Description:
!
!  The main program unit of the lake model FLake,
!  containing most of the FLake procedures.
!  Most FLake variables and local parameters are declared.
!
!  FLake (Fresh-water Lake) is a lake model capable of predicting the surface temperature
!  in lakes of various depth on the time scales from a few hours to a year.
!  The model is based on a two-layer parametric representation of
!  the evolving temperature profile, where the structure of the stratified layer between the
!  upper mixed layer and the basin bottom, the lake thermocline,
!  is described using the concept of self-similarity of the temperature-depth curve.
!  The concept was put forward by Kitaigorodskii and Miropolsky (1970)
!  to describe the vertical temperature structure of the oceanic seasonal thermocline.
!  It has since been successfully used in geophysical applications.
!  The concept of self-similarity of the evolving temperature profile
!  is also used to describe the vertical structure of the thermally active upper layer
!  of bottom sediments and of the ice and snow cover.
!
!  The lake model incorporates the heat budget equations
!  for the four layers in question, viz., snow, ice, water and bottom sediments,
!  developed with due regard for the vertically distributed character
!  of solar radiation heating.
!  The entrainment equation that incorporates the Zilitinkevich (1975) spin-up term
!  is used to compute the depth of a convectively-mixed layer.
!  A relaxation-type equation is used
!  to compute the wind-mixed layer depth in stable and neutral stratification,
!  where a multi-limit formulation for the equilibrium mixed-layer depth
!  proposed by Zilitinkevich and Mironov (1996)
!  accounts for the effects of the earth's rotation, of the surface buoyancy flux
!  and of the static stability in the thermocline.
!  The equations for the mixed-layer depth are developed with due regard for
!  the volumetric character of the radiation heating.
!  Simple thermodynamic arguments are invoked to develop
!  the evolution equations for the ice thickness and for the snow thickness.
!  The heat flux through the water-bottom sediment interface is computed,
!  using a parameterization proposed by Golosov et al. (1998).
!  The heat flux trough the air-water interface
!  (or through the air-ice or air-snow interface)
!  is provided by the driving atmospheric model.
!
!  Empirical constants and parameters of the lake model
!  are estimated, using independent empirical and numerical data.
!  They should not be re-evaluated when the model is applied to a particular lake.
!  The only lake-specific parameters are the lake depth,
!  the optical characteristics of lake water,
!  the temperature at the bottom of the thermally active layer
!  of bottom sediments and the depth of that layer.
!
!  A detailed description of the lake model is given in
!  Mironov, D. V., 2005:
!  Parameterization of Lakes in Numerical Weather Prediction.
!  Part 1: Description of a Lake Model.
!  Manuscript is available from the author.
!  Dmitrii Mironov
!  German Weather Service, Kaiserleistr. 29/35, D-63067 Offenbach am Main, Germany.
!  dmitrii.mironov@dwd.de
!
!  Lines embraced with "!_tmp" contain temporary parts of the code.
!  Lines embraced/marked with "!_dev" may be replaced
!  as improved parameterizations are developed and tested.
!  Lines embraced/marked with "!_dm" are DM's comments
!  that may be helpful to a user.
!  Lines embraced/marked with "!_dbg" are used
!  for debugging purposes only.
!
!
! Current Code Owner: DWD, Dmitrii Mironov
!  Phone:  +49-69-8062 2705
!  Fax:    +49-69-8062 3721
!  e-mail: dmitrii.mironov@dwd.de
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 1.00       2005/11/17 Dmitrii Mironov
!  Initial release
! !VERSION!  !DATE!     <Your name>
!  <Modification comments>
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================
!
! Declarations:
!
! Modules used:

USE data_parameters , ONLY:                                                    &
  ireals                   , & ! KIND-type parameter for real variables
  iintegers                    ! KIND-type parameter for "normal" integer variables

!==============================================================================

IMPLICIT NONE

!==============================================================================
!
! Declarations
!
!  The variables declared below
!  are accessible to all program units of the MODULE flake.
!  Some of them should be USEd by the driving routines that call flake routines.
!  These are basically the quantities computed by FLake.
!  All variables declared below have a suffix "flk".

!  FLake variables of type REAL

!  Temperatures at the previous time step ("p") and the updated temperatures ("n")
REAL (KIND=ireals) ::                                                          &
  T_mnw_p_flk, T_mnw_n_flk      , & ! Mean temperature of the water column [K]
  T_snow_p_flk, T_snow_n_flk    , & ! Temperature at the air-snow interface [K]
  T_ice_p_flk, T_ice_n_flk      , & ! Temperature at the snow-ice or air-ice interface [K]
  T_wML_p_flk, T_wML_n_flk      , & ! Mixed-layer temperature [K]
  T_bot_p_flk, T_bot_n_flk      , & ! Temperature at the water-bottom sediment interface [K]
  T_B1_p_flk, T_B1_n_flk            ! Temperature at the bottom of the upper layer of the sediments [K]

!  Thickness of various layers at the previous time step ("p") and the updated values ("n")
REAL (KIND=ireals) ::                                                          &
  h_snow_p_flk, h_snow_n_flk    , & ! Snow thickness [m]
  h_ice_p_flk, h_ice_n_flk      , & ! Ice thickness [m]
  h_ML_p_flk, h_ML_n_flk        , & ! Thickness of the mixed-layer [m]
  H_B1_p_flk, H_B1_n_flk            ! Thickness of the upper layer of bottom sediments [m]

!  The shape factor(s) at the previous time step ("p") and the updated value(s) ("n")
REAL (KIND=ireals) ::                                                          &
  C_T_p_flk, C_T_n_flk          , & ! Shape factor (thermocline)
  C_TT_flk                      , & ! Dimensionless parameter (thermocline)
  C_Q_flk                       , & ! Shape factor with respect to the heat flux (thermocline)
  C_I_flk                       , & ! Shape factor (ice)
  C_S_flk                           ! Shape factor (snow)

!  Derivatives of the shape functions
REAL (KIND=ireals) ::                                                          &
  Phi_T_pr0_flk                 , & ! d\Phi_T(0)/d\zeta   (thermocline)
  Phi_I_pr0_flk                 , & ! d\Phi_I(0)/d\zeta_I (ice)
  Phi_I_pr1_flk                 , & ! d\Phi_I(1)/d\zeta_I (ice)
  Phi_S_pr0_flk                     ! d\Phi_S(0)/d\zeta_S (snow)

!  Heat and radiation fluxes
REAL (KIND=ireals) ::                                                          &
  Q_snow_flk                    , & ! Heat flux through the air-snow interface [W m^{-2}]
  Q_ice_flk                     , & ! Heat flux through the snow-ice or air-ice interface [W m^{-2}]
  Q_w_flk                       , & ! Heat flux through the ice-water or air-water interface [W m^{-2}]
  Q_bot_flk                     , & ! Heat flux through the water-bottom sediment interface [W m^{-2}]
  I_atm_flk                     , & ! Radiation flux at the lower boundary of the atmosphere [W m^{-2}],
                                    ! i.e. the incident radiation flux with no regard for the surface albedo.
  I_snow_flk                    , & ! Radiation flux through the air-snow interface [W m^{-2}]
  I_ice_flk                     , & ! Radiation flux through the snow-ice or air-ice interface [W m^{-2}]
  I_w_flk                       , & ! Radiation flux through the ice-water or air-water interface [W m^{-2}]
  I_h_flk                       , & ! Radiation flux through the mixed-layer-thermocline interface [W m^{-2}]
  I_bot_flk                     , & ! Radiation flux through the water-bottom sediment interface [W m^{-2}]
  I_intm_0_h_flk                , & ! Mean radiation flux over the mixed layer [W m^{-1}]
  I_intm_h_D_flk                , & ! Mean radiation flux over the thermocline [W m^{-1}]
  Q_star_flk                        ! A generalized heat flux scale [W m^{-2}]

!  Velocity scales
REAL (KIND=ireals) ::                                                          &
  u_star_w_flk                  , & ! Friction velocity in the surface layer of lake water [m s^{-1}]
  w_star_sfc_flk                    ! Convective velocity scale,
                                    ! using a generalized heat flux scale [m s^{-1}]

!  The rate of snow accumulation
REAL (KIND=ireals) ::                                                          &
  dMsnowdt_flk                      ! The rate of snow accumulation [kg m^{-2} s^{-1}]

!==============================================================================
! Procedures
!==============================================================================

CONTAINS

! File %M% from Library %Q%
! Version %I% from %G% extracted: %H%
!------------------------------------------------------------------------------

REAL (KIND=ireals) FUNCTION flake_buoypar (T_water)

!------------------------------------------------------------------------------
!
! Description:
!
!  Computes the buoyancy parameter,
!  using a quadratic equation of state for the fresh-water.
!
!
! Current Code Owner: DWD, Dmitrii Mironov
!  Phone:  +49-69-8062 2705
!  Fax:    +49-69-8062 3721
!  e-mail: dmitrii.mironov@dwd.de
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 1.00       2005/11/17 Dmitrii Mironov
!  Initial release
! !VERSION!  !DATE!     <Your name>
!  <Modification comments>
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================
!
! Declarations:
!
! Modules used:

!_dm Parameters are USEd in module "flake".
!_nu USE data_parameters , ONLY : &
!_nu     ireals,                  & ! KIND-type parameter for real variables
!_nu     iintegers                  ! KIND-type parameter for "normal" integer variables

USE flake_parameters , ONLY:                                                   &
  tpl_grav                  , & ! Acceleration due to gravity [m s^{-2}]
  tpl_T_r                   , & ! Temperature of maximum density of fresh water [K]
  tpl_a_T                       ! Constant in the fresh-water equation of state [K^{-2}]

!==============================================================================

IMPLICIT NONE

!==============================================================================
!
! Declarations

!  Input (function argument)
REAL (KIND=ireals), INTENT(IN) ::                                              &
  T_water                             ! Water temperature [K]

!==============================================================================
!  Start calculations
!------------------------------------------------------------------------------

! Buoyancy parameter [m s^{-2} K^{-1}]

flake_buoypar = tpl_grav * tpl_a_T * (T_water - tpl_T_r)

!------------------------------------------------------------------------------
!  End calculations
!==============================================================================

END FUNCTION flake_buoypar

!==============================================================================

! File %M% from Library %Q%
! Version %I% from %G% extracted: %H%
!------------------------------------------------------------------------------

REAL (KIND=ireals) FUNCTION flake_snowdensity (h_snow)

!------------------------------------------------------------------------------
!
! Description:
!
!  Computes the snow density,
!  using an empirical approximation from Heise et al. (2003).
!
!
! Current Code Owner: DWD, Dmitrii Mironov
!  Phone:  +49-69-8062 2705
!  Fax:    +49-69-8062 3721
!  e-mail: dmitrii.mironov@dwd.de
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 1.00       2005/11/17 Dmitrii Mironov
!  Initial release
! !VERSION!  !DATE!     <Your name>
!  <Modification comments>
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================
!
! Declarations:
!
! Modules used:

!_dm Parameters are USEd in module "flake".
!_nu USE data_parameters , ONLY : &
!_nu     ireals,                  & ! KIND-type parameter for real variables
!_nu     iintegers                  ! KIND-type parameter for "normal" integer variables

USE flake_parameters , ONLY:                                                   &
  tpl_rho_w_r               , & ! Maximum density of fresh water [kg m^{-3}]
  tpl_rho_S_min             , & ! Minimum snow density [kg m^{-3}]
  tpl_rho_S_max             , & ! Maximum snow density [kg m^{-3}]
  tpl_Gamma_rho_S           , & ! Empirical parameter [kg m^{-4}] in the expression for the snow density
  c_small_flk                   ! A small number

!==============================================================================

IMPLICIT NONE

!==============================================================================
!
! Declarations

!  Input (function argument)
REAL (KIND=ireals), INTENT(IN) ::                                              &
  h_snow                              ! Snow thickness [m]

!==============================================================================
!  Start calculations
!------------------------------------------------------------------------------

! Snow density [kg m^{-3}]

!  Security. Ensure that the expression in () does not become negative at a very large h_snow.
flake_snowdensity = MAX( c_small_flk, (1.0_ireals - h_snow *                   &
                         tpl_Gamma_rho_S / tpl_rho_w_r) )
flake_snowdensity = MIN( tpl_rho_S_max, tpl_rho_S_min / flake_snowdensity )

!------------------------------------------------------------------------------
!  End calculations
!==============================================================================

END FUNCTION flake_snowdensity

!==============================================================================

! File %M% from Library %Q%
! Version %I% from %G% extracted: %H%
!------------------------------------------------------------------------------

REAL (KIND=ireals) FUNCTION flake_snowheatconduct (h_snow)

!------------------------------------------------------------------------------
!
! Description:
!
!  Computes the snow heat conductivity,
!  using an empirical approximation from Heise et al. (2003).
!
!
! Current Code Owner: DWD, Dmitrii Mironov
!  Phone:  +49-69-8062 2705
!  Fax:    +49-69-8062 3721
!  e-mail: dmitrii.mironov@dwd.de
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 1.00       2005/11/17 Dmitrii Mironov
!  Initial release
! !VERSION!  !DATE!     <Your name>
!  <Modification comments>
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================
!
! Declarations:
!
! Modules used:

!_dm Parameters are USEd in module "flake".
!_nu USE data_parameters , ONLY : &
!_nu     ireals,                  & ! KIND-type parameter for real variables
!_nu     iintegers                  ! KIND-type parameter for "normal" integer variables

USE flake_parameters , ONLY:                                                   &
  tpl_rho_w_r               , & ! Maximum density of fresh water [kg m^{-3}]
  tpl_kappa_S_min           , & ! Minimum molecular heat conductivity of snow [J m^{-1} s^{-1} K^{-1}]
  tpl_kappa_S_max           , & ! Maximum molecular heat conductivity of snow [J m^{-1} s^{-1} K^{-1}]
  tpl_Gamma_kappa_S             ! Empirical parameter [J m^{-2} s^{-1} K^{-1}] in the expression for kappa_S

!==============================================================================

IMPLICIT NONE

!==============================================================================
!
! Declarations

!  Input (function argument)
REAL (KIND=ireals), INTENT(IN) ::                                              &
  h_snow                              ! Snow thickness [m]

!==============================================================================
!  Start calculations
!------------------------------------------------------------------------------

! Snow heat conductivity [J m^{-1} s^{-1} K^{-1} = kg m s^{-3} K^{-1}]

flake_snowheatconduct = flake_snowdensity( h_snow )   ! Compute snow density
flake_snowheatconduct = MIN( tpl_kappa_S_max, tpl_kappa_S_min + h_snow *       &
                             tpl_Gamma_kappa_S * flake_snowheatconduct /       &
                             tpl_rho_w_r )

!------------------------------------------------------------------------------
!  End calculations
!==============================================================================

END FUNCTION flake_snowheatconduct

!==============================================================================

END MODULE flake

