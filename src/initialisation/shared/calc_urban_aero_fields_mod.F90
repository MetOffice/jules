! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

!-----------------------------------------------------------------------------
! Description:
!   Calculates urban aerodynamic parameters (roughness length and displacement
!   height), currently using MacDonald 1998, although other parameterisations
!   exist
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

MODULE calc_urban_aero_fields_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE calc_urban_aero_fields(land_pts, wrr_gb, hwr_gb, hgt_gb,            &
                                  ztm_gb, disp_gb)

USE conversions_mod,      ONLY: pi
USE um_types,             ONLY: real_jlslsm
USE urban_param_mod,      ONLY: kappa2, cdz, a, z0m_mat

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN)                    :: land_pts
REAL(KIND=real_jlslsm), INTENT(IN)     :: wrr_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN)     :: hwr_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN)     :: hgt_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: ztm_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: disp_gb(land_pts)

! Local variables
INTEGER :: l  ! Index variables
REAL(KIND=real_jlslsm) :: sc_hwr, d_h

CHARACTER(LEN=*), PARAMETER :: RoutineName='CALC_URBAN_AERO_FIELDS'

DO l = 1, land_pts
  sc_hwr = hwr_gb(l) / pi
  d_h    = 1.0 - wrr_gb(l) * ( a**( wrr_gb(l) - 1.0 ) )
  disp_gb(l) = d_h * hgt_gb(l)
  ztm_gb(l)  = ( cdz * ( 1.0 - d_h ) * sc_hwr * wrr_gb(l) / kappa2 )**(-0.5)
  ztm_gb(l)  = ( 1.0 - d_h ) * EXP( -1.0 * ztm_gb(l) )
  ztm_gb(l)  = ztm_gb(l) * hgt_gb(l)
  ztm_gb(l)  = MAX( ztm_gb(l), z0m_mat )
END DO

END SUBROUTINE calc_urban_aero_fields
END MODULE calc_urban_aero_fields_mod
