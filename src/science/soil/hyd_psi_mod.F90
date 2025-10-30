! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE hyd_psi_mod

USE water_constants_mod, ONLY: rho_water  !  Density of pure water (kg m-3).
USE planet_constants_mod, ONLY: g
  !  Mean acceleration due to gravity at earth's surface (m s-2)   .

USE jules_soil_mod, ONLY: l_vg_soil
! If l_vg_soil=False, uses Brooks and Corey relation.
! If l_vg_soil=True, uses van Genuchten relation.

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='HYD_PSI_MOD'

CONTAINS

FUNCTION psi_from_sthu(sthu, sathh, b, sthu_min) RESULT (psi)
!-----------------------------------------------------------------------------
! Description:
!   Calculates the (negative) soil water potential (in Pa) from the volumetric
!   soil water content as a fraction of saturation.
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  sthu,                                                                        &
    ! Unfrozen soil moisture content of each layer as a fraction of
    ! saturation.
  sathh,                                                                       &
    ! If l_vg_soil=False, absolute value of the soil matric suction at
    ! saturation (m).
    ! If l_vg_soil=True, sathh = 1 / alpha, where alpha (in m-1) is a
    ! parameter in the van Genuchten model.
  b,                                                                           &
    ! Exponent in soil hydraulic characteristics.
  sthu_min
    ! Minimum value of sthu for use in the calculation.

!-----------------------------------------------------------------------------
! Function result:
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  psi  ! (Negative) soil water potential (Pa).

!-----------------------------------------------------------------------------
! Local variables:
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  hh  ! Absolute soil water suction (m).

hh = hh_from_sthu(sthu, sathh, b, sthu_min)
psi = - hh * rho_water * g

END FUNCTION psi_from_sthu

!#############################################################################

FUNCTION sthu_from_psi(psi, sathh, b, sthu_min) RESULT (sthu)
!-----------------------------------------------------------------------------
! Description:
!   Calculates the volumetric soil water content as
!   a fraction of saturation from the (negative) soil water potential (in Pa).
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  psi,                                                                         &
    ! (Negative) soil water potential (Pa).
  sathh,                                                                       &
    ! If l_vg_soil=False, absolute value of the soil matric suction at
    ! saturation (m).
    ! If l_vg_soil=True, sathh = 1 / alpha, where alpha (m-1) is a parameter
    ! in the van Genuchten model.
  b,                                                                           &
    ! Exponent in soil hydraulic characteristics.
  sthu_min
    ! Minimum value of sthu for use in the calculation.

!-----------------------------------------------------------------------------
! Function result:
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
 sthu
   ! Unfrozen soil moisture content of each layer as a fraction of
   ! saturation.

!-----------------------------------------------------------------------------
! Local variables:
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  hh ! Absolute soil water suction (m).

hh = - psi / ( rho_water * g )
sthu = sthu_from_hh(hh, sathh, b, sthu_min)

END FUNCTION sthu_from_psi

!#############################################################################

FUNCTION hh_from_sthu(sthu, sathh, b, sthu_min) RESULT (hh)
!-----------------------------------------------------------------------------
! Description:
!   Calculates the absolute soil water suction (in m) from the volumetric
!   soil water content as a fraction of saturation.
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  sthu,                                                                        &
    ! Unfrozen soil moisture content of each layer as a fraction of
    ! saturation.
  sathh,                                                                       &
    ! If l_vg_soil=False, absolute value of the soil matric
    ! suction at saturation (m).
    ! If l_vg_soil=True, sathh = 1 / alpha, where alpha
    ! (m-1) is a parameter in the van Genuchten model.
  b,                                                                           &
    ! Exponent in soil hydraulic characteristics.
  sthu_min
    ! Minimum value of sthu for use in the calculation.

!-----------------------------------------------------------------------------
! Function result:
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  hh  ! Absolute soil water suction (m).

!-----------------------------------------------------------------------------
! Local variables:
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  bracket,                                                                     &
    ! Intermediate step in calculation when l_vg=True.
  sthu_local
    ! Local value of sthu (after making sure it is not below sthu_min).

IF (sthu < sthu_min) THEN
  sthu_local = sthu_min
ELSE
  sthu_local = sthu
END IF

IF ( l_vg_soil ) THEN
  bracket = -1.0 + sthu_local ** ( -b - 1.0 )
  hh = sathh * bracket ** ( b / ( b + 1.0 ) )
ELSE
  hh  =  sathh / (sthu_local ** b)
END IF

END FUNCTION hh_from_sthu

!#############################################################################

FUNCTION sthu_from_hh(hh, sathh, b, sthu_min) RESULT (sthu)
!-----------------------------------------------------------------------------
! Description:
!   Calculates the volumetric soil water content as
!   a fraction of saturation from the absolute soil water suction (in m).
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  hh,                                                                          &
    ! Absolute soil water suction (m).
  sathh,                                                                       &
    ! If l_vg_soil=False, absolute value of the soil matric
    ! suction at saturation (m).
    ! If l_vg_soil=True, sathh = 1 / alpha, where alpha
    ! (m-1) is a parameter in the van Genuchten model.
  b,                                                                           &
    ! Exponent in soil hydraulic characteristics.
  sthu_min
    ! Minimum value of sthu for use in the calculation .

!-----------------------------------------------------------------------------
! Function result:
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  sthu
    ! Unfrozen soil moisture content of each layer as a fraction of
    ! saturation.

!-----------------------------------------------------------------------------
! Local variables:
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  bracket
    ! Intermediate step in calculation when l_vg=True.

IF ( l_vg_soil ) THEN
  bracket = 1.0 + ( hh / sathh )** ( ( b + 1.0 ) / b )
  sthu    = ( 1.0 / bracket )** ( 1.0 / (1.0 + b ) )
ELSE
  sthu    = ( hh / sathh )** ( -1.0 / b )
END IF

IF (sthu < sthu_min) THEN
  sthu = sthu_min
END IF

END FUNCTION sthu_from_hh


END MODULE hyd_psi_mod
