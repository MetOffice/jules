!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE veg_soil_index_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

! Description:
!   Create an index of points with veg and/or soil, i.e. points on which
!   TRIFFID and soil biogeochemical models operate.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.

PRIVATE
PUBLIC get_veg_soil_index

CONTAINS

!#############################################################################

SUBROUTINE get_veg_soil_index( land_pts, frac_surft, vs_pts,                   &
                               vs_index, frac_vs )

USE jules_surface_types_mod, ONLY:                                             &
  ! imported scalars
  nnpft, ntype, soil

USE jules_vegetation_mod, ONLY:                                                &
  ! imported scalars
  frac_min

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) :: land_pts
    ! Number of land points.

REAL(KIND=real_jlslsm), INTENT(IN) :: frac_surft(land_pts,ntype)
    ! Fractional coverage of each land type.

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT)
!-----------------------------------------------------------------------------
INTEGER, INTENT(OUT) :: vs_pts
    ! Number of points with vegetation and/or soil.

INTEGER, INTENT(OUT) :: vs_index(land_pts)
    ! Index of land points with vegetation and/or soil.

REAL(KIND=real_jlslsm), INTENT(OUT) :: frac_vs(land_pts)
    ! Fraction of gridbox covered by veg or soil.

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER :: l,n  ! Indices.

!-----------------------------------------------------------------------------
!end of header

!-----------------------------------------------------------------------------
! Get index for points with soil and/or vegetation.
!-----------------------------------------------------------------------------
vs_pts = 0

DO l = 1,land_pts

  frac_vs(l) = 0.0

  ! Accumulate area under vegetation.
  DO n = 1,nnpft
    frac_vs(l) = frac_vs(l) + frac_surft(l,n)
  END DO

  ! Add area of bare soil.
  frac_vs(l) = frac_vs(l) + frac_surft(l,soil)

  IF ( frac_vs(l) >= REAL(nnpft) * frac_min ) THEN
    vs_pts           = vs_pts + 1
    vs_index(vs_pts) = l
  END IF

END DO

END SUBROUTINE get_veg_soil_index

!#############################################################################
!#############################################################################

END MODULE veg_soil_index_mod

