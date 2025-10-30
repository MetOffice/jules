! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE calc_direct_albsoil_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CONTAINS

FUNCTION calc_direct_albsoil(albdif, cosz) RESULT (albdir)

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  albdif,                                                                      &
    ! Diffuse albedo of soil
  cosz
    ! Cosine of the solar zenith angle
REAL(KIND=real_jlslsm) ::                                                      &
  albdir
    ! Direct albedo, RESULT of function


REAL(KIND=real_jlslsm) ::                                                      &
  gamma_sim,                                                                   &
    ! Isotropic similarity parameter
  albdif_app
    ! Approximate diffuse albedo

!End of header

!           Pade approximant to the isotropic similarity parameter
!           using the diffuse albedo.
gamma_sim = (1.0 - albdif) /                                                   &
            (1.0 + (4.0 / 3.0) * albdif)
!           Calculate approximate diffuse albedo to scale direct
!           to a more accurate value
albdif_app = (1.0 - gamma_sim) *                                               &
             (1.0 - LOG(1.0+2.0 * gamma_sim) /                                 &
             (2.0 * gamma_sim) ) / gamma_sim
!           Equations 24 and 40 from Hapke (1981) with scaling correction.
albdir = (albdif / albdif_app) *                                               &
                 (1.0 - gamma_sim) /                                           &
                 (1.0+2.0 * gamma_sim * cosz)

END FUNCTION calc_direct_albsoil

END MODULE calc_direct_albsoil_mod
