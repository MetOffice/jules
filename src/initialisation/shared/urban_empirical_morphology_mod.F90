! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

!-----------------------------------------------------------------------------
! Description:
!   Calculates urban morphology from empirical relationships. Where:
!     wrr   Repeating width ratio (or canyon fraction, W/R)
!     hwr   Height-to-width ratio (H/W)
!     hgt   Building height (H)
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Empirical relationships derived from correlating CEH urban fraction and
! LUCID urban geometry data for London. Obtained from collaboration with the
! University of Reading. See:
!     Bohnenstengel, S.I., Evans, S., Clark, P.A. and Belcher, S. (2011),
!     Simulations of the London urban heat island. Q.J.R. Meteorol. Soc.,
!     137: 1625-1640. https://doi.org/10.1002/qj.855
! for more information
!-----------------------------------------------------------------------------

MODULE urban_empirical_morphology_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
                  ModuleName='URBAN_EMPIRICAL_MORPHOLOGY_MOD'

CONTAINS

SUBROUTINE urban_empirical_morphology(frac_surft, surft_index, surft_pts,      &
                                      urban_param)

USE ancil_info, ONLY: land_pts

USE jules_surface_types_mod, ONLY:  ntype, urban_canyon, urban_roof

USE jules_urban_mod, ONLY: l_moruses

USE jules_print_mgr, ONLY: jules_message, jules_print

!TYPE definitions
USE ancil_info, ONLY: ainfo_type
USE urban_param_mod, ONLY: urban_param_type

IMPLICIT NONE

!Arguments
INTEGER :: surft_index(land_pts,ntype)        ! Indices of land points
INTEGER, INTENT(IN) :: surft_pts(ntype)
REAL(KIND=real_jlslsm), INTENT(IN)     :: frac_surft(land_pts,ntype)
                                              ! Fractions of surface types

TYPE(urban_param_type), INTENT(IN OUT) :: urban_param

! Local variables
REAL(KIND=real_jlslsm) :: furb

INTEGER :: j, l  ! Index variables

CHARACTER(LEN=*), PARAMETER :: RoutineName='urban_empirical_morphology'

WRITE(jules_message,'(A)')                                                     &
   "Using empirical relationships for urban geometry (W/R): wrr"
CALL jules_print(RoutineName, jules_message)

IF ( l_moruses ) THEN
  WRITE(jules_message,'(A)')                                                   &
     "Using empirical relationships for urban geometry (H/W): hwr"
  CALL jules_print(RoutineName, jules_message)
  WRITE(jules_message,'(A)')                                                   &
     "Using empirical relationships for urban geometry (H): hgt"
  CALL jules_print(RoutineName, jules_message)
END IF

DO j = 1,surft_pts(urban_canyon)
  l = surft_index(j,urban_canyon)
  !----------------------------------------------------------------------------
  ! The two-tile urban schemes may either have a combined urban in urban_canyon
  ! surface type or a fraction split between the canyon and roof. The combined
  ! urban fraction must be used with the empirical relationships.
  !----------------------------------------------------------------------------
  furb = frac_surft(l,urban_canyon) + frac_surft(l,urban_roof)

  urban_param%wrr_gb(l)   = wrr(furb)

  IF ( l_moruses ) THEN
    urban_param%hwr_gb(l) = hwr(furb, urban_param%wrr_gb(l))
    urban_param%hgt_gb(l) = hgt(furb)
  END IF
END DO

RETURN

END SUBROUTINE urban_empirical_morphology

!******************************************************************************

FUNCTION wrr(furb)

IMPLICIT NONE

! Arguments
REAL(KIND=real_jlslsm), INTENT(IN) :: furb

! Return
REAL(KIND=real_jlslsm) :: wrr

! Local variable
REAL(KIND=real_jlslsm) :: lambdap

!-------------------------------------------------------------------------------

lambdap = 22.878 * furb**6 - 59.473 * furb**5 + 57.749 * furb**4 -             &
          25.108 * furb**3 + 4.3337 * furb**2 + 0.1926 * furb    +             &
          0.036
wrr     = 1.0 - lambdap

RETURN

END FUNCTION wrr

!******************************************************************************

FUNCTION hwr(furb, wrr)

USE conversions_mod, ONLY: pi

IMPLICIT NONE

! Arguments
REAL(KIND=real_jlslsm), INTENT(IN) :: furb, wrr

! Return
REAL(KIND=real_jlslsm) :: hwr

! Local variable
REAL(KIND=real_jlslsm) :: lambdaf

!-------------------------------------------------------------------------------

lambdaf = 16.412 * furb**6 - 41.855 * furb**5 + 40.387 * furb**4 -             &
          17.759 * furb**3 + 3.2399 * furb**2 + 0.0626 * furb    +             &
          0.0271
hwr     = ( 0.5 * pi * lambdaf ) / wrr

RETURN

END FUNCTION hwr

!******************************************************************************

FUNCTION hgt(furb)

IMPLICIT NONE

! Arguments
REAL(KIND=real_jlslsm), INTENT(IN) :: furb

! Return
REAL(KIND=real_jlslsm) :: hgt

!-------------------------------------------------------------------------------

hgt = 167.409 * furb**5 - 337.853 * furb**4 + 247.813 * furb**3 -              &
      76.3678 * furb**2 + 11.4832 * furb    +                                  &
      4.48226

RETURN

END FUNCTION hgt

!******************************************************************************

END MODULE urban_empirical_morphology_mod
