! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Routine to calculate melt pond albedos for visible and near-infrared
! for direct (including zenith angle) and diffuse fluxes.

! *********************************************************************
MODULE albpond_mod

USE jules_sea_seaice_mod, ONLY: albpondv_cice

USE parkind1,             ONLY: jprb, jpim
USE yomhook,              ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='ALBPOND_MOD'

PRIVATE  !  private scope by default
PUBLIC albpond_mal

CONTAINS

! -------------------------------

SUBROUTINE albpond_mal(cos_zenith_angle, pond_depth, bottom_albedo, pond_albedo)

! Use funtions originally derived from Malinka et al. (2018) to calculate the melt pond albedo.
! Malinka et al. (2018). Reflective properties of melt ponds on sea ice. The Cryosphere. Volume 12. Issue 6. 1921–1937
! https://doi.org/10.5194/tc-12-1921-2018

IMPLICIT NONE

! Inputs
REAL(KIND=real_jlslsm), INTENT(IN) :: cos_zenith_angle  ! Cos of the zenith angle of incident light
REAL(KIND=real_jlslsm), INTENT(IN) :: pond_depth        ! The depth of the melt pond (m)
REAL(KIND=real_jlslsm), INTENT(IN) :: bottom_albedo(4)  ! The albedo of the sea ice at the base of the melt pond in radiation bands

! Outputs
REAL(KIND=real_jlslsm), INTENT(OUT) :: pond_albedo(4)   ! The melt pond albedos on each band
                                      ! 1 = Direct visible
                                      ! 2 = Diffuse visible
                                      ! 3 = Direct near infrared
                                      ! 4 = Diffuse near infrared

! Locals
! Angle information
REAL(KIND=real_jlslsm) :: angle_air       ! The angle of the light on the air side of the pond
REAL(KIND=real_jlslsm) :: angle_water     ! The angle of the light on the water side of the pond
REAL(KIND=real_jlslsm) :: sin_angle_water ! The SIN of the angle of the light on the water side
REAL(KIND=real_jlslsm) :: cos_angle_water ! The COS of the angle of the light on the water side
REAL(KIND=real_jlslsm) :: cos_angle_air   ! The COS of the angle of the light on the air side

! Other variables
REAL(KIND=real_jlslsm) :: reflected_light ! The fraction of light reflected off the surface of the melt pond
REAL(KIND=real_jlslsm) :: transmitted_light ! The fraction of light transmitted into the melt pond
REAL(KIND=real_jlslsm) :: x               ! Variable x in the Malinka equations 4 and 5. Equal to the
                                          ! extinction coefficient multipled by the pond depth.
REAL(KIND=real_jlslsm) :: f_out           ! The result of the f_out equation (equation 5 of Malinka)
                                          ! Calculated using a best fit approximation instead of an integral
REAL(KIND=real_jlslsm) :: f_in            ! The result of the f_in equation (equation 4 of Malinka)
                                          ! Calculated using a best fit approximation instead of an integral

! Constants
REAL(KIND=real_jlslsm), PARAMETER :: n_air = 1             ! Refractive index for air
REAL(KIND=real_jlslsm), PARAMETER :: n_water = 1.33        ! Refractive index of water
REAL(KIND=real_jlslsm), PARAMETER :: ext_coeff_visible = 0.2152    ! Extintion coeffient of water
                                         ! from NEMO trc_oce.F90 rkrgb lookup table for 1.0 mg m-3 chlorophyll
                                         ! (averaged over blue, green and red)
REAL(KIND=real_jlslsm), PARAMETER :: ext_coeff_nir = 2.857 ! Extintion coeffient of near infrared light in water  
                                         ! 2.857 = 1.0 / rn_si0 = value used by NEMO
REAL(KIND=real_jlslsm), PARAMETER :: RFD = 0.0659          ! Diffuse Fresnel reflection
                                                           ! This is the integral of 2*fresnel_reflection*cos_angle_air*delta_cos
                                                           ! For air over water (with refractive indexes of 1 and 1.33)
                                                           ! this is a constant number.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='ALBPOND'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
 
! Calculate the sin of transmitted angle (Snells law)
angle_air = ACOS(cos_zenith_angle)
sin_angle_water = n_air/n_water * SIN(angle_air)

! Calculate the other angle information
angle_water = ASIN(sin_angle_water)
cos_angle_water = COS(angle_water)
cos_angle_air = cos_zenith_angle

! Calculate inputs into Malinka equations
reflected_light = reflected_fresnel(cos_angle_air, cos_angle_water, n_air, n_water)
transmitted_light = 1.0 - reflected_light

! ----------- Section on visible light ------------------

! For visible light calculate integrals in equations 4 and 5 by using pre-calculated
! best fit parameters (calculated using malinka_find_best_fits.py).
! Use a small extinction coefficient valid for visible light.
x = ext_coeff_visible * pond_depth
f_out = EXP(-1.19335 * x) * 0.93404
f_in = EXP(-4.87944 * x) * 0.46628

! For direct visible light use equation 1 of Malinka
pond_albedo(1) = reflected_light + transmitted_light * EXP(-1.0*x/cos_angle_water) * &
                 f_out * bottom_albedo(2) / ( n_water**2 * (1 - bottom_albedo(2) * f_in) )

! Make sure that this direct visible pond albedo is within acceptable limits
IF (pond_albedo(1) > 1.0) pond_albedo(1) = 1.0
IF (pond_albedo(1) < albpondv_cice) pond_albedo(1) = albpondv_cice

! For diffuse visible light use equation 9 of Malinka
pond_albedo(2) = RFD + f_out**2 * bottom_albedo(2) / ( n_water**2 * (1 - bottom_albedo(2) * f_in) )

! Apply limits to the diffuse_albedo
IF (pond_albedo(2) > bottom_albedo(2)) pond_albedo(2) = bottom_albedo(2)
IF (pond_albedo(2) < albpondv_cice) pond_albedo(2) = albpondv_cice

! ----------- Section on near infrared (NIR) light ------------------

! For NIR light calculate integrals in equations 4 and 5 by using pre-calculated
! best fit parameters (calculated using malinka_find_best_fits.py).
! Use a large extinction coefficient valid for NIR light.
x = ext_coeff_nir * pond_depth
f_out = EXP(-1.18120 * x) * 0.93116
f_in = EXP(-3.59441 * x) * 0.37321

! For direct NIR light use equation 1 of Malinka
pond_albedo(3) = reflected_light + transmitted_light * EXP(-1.0*x/cos_angle_water) * &
                 f_out * bottom_albedo(4) / ( n_water**2 * (1 - bottom_albedo(4) * f_in) )

! Make sure that this direct NIR pond albedo is within acceptable limits
IF (pond_albedo(3) > 1.0) pond_albedo(3) = 1.0
IF (pond_albedo(3) < 0.01) pond_albedo(3) = 0.01

! For diffuse NIR light use equation 9 of Malinka
pond_albedo(4) = RFD + f_out**2 * bottom_albedo(4) / ( n_water**2 * (1 - bottom_albedo(4) * f_in) )

! Apply limits to the diffuse_albedo
IF (pond_albedo(4) > bottom_albedo(4)) pond_albedo(4) = bottom_albedo(4)

END SUBROUTINE albpond_mal

! -------------------------------------------------------
! --- Extra functions that the subroutine above calls

FUNCTION reflected_fresnel(cos_angle_in, cos_angle_out, n_in, n_out) RESULT(R_F)

IMPLICIT NONE

! Inputs
REAL(KIND=real_jlslsm), INTENT(IN) :: cos_angle_in
REAL(KIND=real_jlslsm), INTENT(IN) :: cos_angle_out
REAL(KIND=real_jlslsm), INTENT(IN) :: n_in
REAL(KIND=real_jlslsm), INTENT(IN) :: n_out

! Returns
REAL(KIND=real_jlslsm)  :: R_F    ! Total light reflected

! Local
REAL(KIND=real_jlslsm)  :: top    ! Top part of fresnel equations
REAL(KIND=real_jlslsm)  :: bottom ! Bottom part of fresnel equations
REAL(KIND=real_jlslsm)  :: R_s    ! S polarised light reflected
REAL(KIND=real_jlslsm)  :: R_p    ! P polarised light reflected

! Calculate the amount of reflected light off of a interface between two fluids
! using the Fresnel equations

! Do the S polarised light component of what is reflected
top = n_in * cos_angle_in - n_out * cos_angle_out
bottom = n_in * cos_angle_in + n_out * cos_angle_out
R_s = (top/bottom)**2.0

! Do the P polarised light component of what is reflected
top = n_in * cos_angle_out - n_out * cos_angle_in
bottom = n_in * cos_angle_out + n_out * cos_angle_in
R_p = (top/bottom)**2.0

! Combine them by taking the average
R_F = 0.5*(R_s+R_p)

END FUNCTION reflected_fresnel

! -------------------------------------------------------------

END MODULE albpond_mod
