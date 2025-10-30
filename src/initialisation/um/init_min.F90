#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Ensures that PFT fractions are greater than non-zero minimum fraction
!
! Subroutine Interface:
MODULE init_min_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='INIT_MIN_MOD'

CONTAINS
SUBROUTINE init_min(land_pts,frac,cs)


USE jules_surface_types_mod, ONLY: ntype, npft, soil
USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
USE jules_soil_mod, ONLY: cs_min
USE jules_vegetation_mod, ONLY: frac_min
USE um_types, ONLY: real_jlslsm

IMPLICIT NONE
!
! Description:
!   If fractions of any PFTs are less than a non-zero minimum fraction
!   on land points that are not entirely (or mostly) covered by ice,
!   water or urban, initialise the PFT fractions to the minimum fraction
!   and take the excess proportionally from other PFTs and soil to
!   ensure that the total fractional cover of all PFTs + soil remains
!   unchanged.
!
! Method:
!   For PFTs with fraction < minimum fraction, reset fraction to minimum
!   fraction and find the total increase for all PFTs.  For all PFTS,
!   define "available fraction" as the difference between fraction
!   and minimum fraction, and find "available fraction" from sum
!   of all PFT available fractions plus fraction of soil (this is the
!   "available fraction" for soil; the minimum fraction for soil is
!   zero).  Reduce fractions of all PFTs and soil by amounts weighted
!   by "available fraction" / "total available fraction" such that the
!   sum of the reductions equals the total increase made earlier.  On
!   points with insufficent veg or soil to do this, take no action as
!   vegetation will not be modelled on these points.
!
!
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
! This file belongs in section: Vegetation
!
! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 programming standards.
!

! Subroutine arguments
!   Scalar arguments with intent(in):
INTEGER ::                                                                     &
 land_pts            ! IN Number of land point to be processed.

REAL(KIND=real_jlslsm) ::                                                      &
 cs(land_pts,4)                                                                &
                     ! INOUT Soil carbon content (kg C/m2).
,frac(land_pts,ntype) ! INOUT Fractions of surface types.

INTEGER ::                                                                     &
 l                                                                             &
                     ! Loop counter for land points
,n                   ! Loop counter for surface types

REAL(KIND=real_jlslsm) ::                                                      &
 frac_avail(land_pts,ntype)                                                    &
                             ! LOCAL The part of FRAC that is
!                                  !       available for "donation"
      ,tot_frac_need(land_pts)                                                 &
                                   ! LOCAL Total fraction needed to make
!                                  !       PFT fractions up to minimum
      ,tot_frac_avail(land_pts)    ! LOCAL Total fractional area
!                                  !       available to give to PFTs
!                                  !       with less than minimum frac.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='INIT_MIN'


!----------------------------------------------------------------------
! Local parameters
!----------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
DO l = 1,land_pts
  tot_frac_need(l) = 0.0
  tot_frac_avail(l) = 0.0

  !-----------------------------------------------------------------------
  ! Find total fraction available for donation to PFTs with less than
  ! the minimum coverage
  !-----------------------------------------------------------------------
  DO n = 1,npft
    IF (frac(l,n) <  frac_min) THEN
      tot_frac_need(l) = tot_frac_need(l) +                                    &
      (frac_min - frac(l,n))
    ELSE IF (frac(l,n) >= frac_min) THEN
      frac_avail(l,n) = frac(l,n) - frac_min
      tot_frac_avail(l) = tot_frac_avail(l) + frac_avail(l,n)
    END IF
  END DO
  n = soil
  frac_avail(l,n) = frac(l,n)
  tot_frac_avail(l) = tot_frac_avail(l) + frac(l,n)

  !-----------------------------------------------------------------------
  ! If sufficient total fraction is available, modify fractions of veg and
  ! soil and also modify soil carbon.  If insufficient fraction available,
  ! do neither of these as TRIFFID will not operate on such points.
  !-----------------------------------------------------------------------
  IF (tot_frac_avail(l) >= tot_frac_need(l)) THEN

    !-----------------------------------------------------------------------
    ! i)  If PFT fraction is less than the minimum fraction, increase it
    !     to the minimum fraction.
    !-----------------------------------------------------------------------
    DO n = 1,npft
      IF (frac(l,n) <  frac_min) THEN
        frac(l,n) = frac_min
        frac_avail(l,n) = 0.0
      ELSE IF (frac(l,n) == frac_min) THEN
        frac_avail(l,n) = 0.0
      END IF
    END DO

    !-----------------------------------------------------------------------
    ! ii) Scale other PFTs and soil to keep total coverage of veg+soil
    !     unchanged.  The relative proportions of the soil fraction and the
    !     PFT fractions greater than the minimum fraction remain constant.
    !-----------------------------------------------------------------------
    DO n = 1,npft
      frac(l,n) = frac(l,n) -                                                  &
      ( (frac_avail(l,n) / tot_frac_avail(l)) * tot_frac_need(l) )
    END DO

    n = soil
    frac(l,n) = frac(l,n) -                                                    &
    ( (frac_avail(l,n) / tot_frac_avail(l)) * tot_frac_need(l) )

    !-----------------------------------------------------------------------
    ! iii) If soil carbon content is less than minimum allowed, increase
    !      it to the minimum.
    !-----------------------------------------------------------------------
    IF ((cs(l,1) + cs(l,2) + cs(l,3) + cs(l,4)) <  cs_min) THEN
      cs(l,1) = cs_min * 0.1
      cs(l,2) = cs_min * 0.2
      cs(l,3) = cs_min * 0.3
      cs(l,4) = cs_min * 0.4
    END IF

  END IF

END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE init_min
END MODULE init_min_mod
#endif
