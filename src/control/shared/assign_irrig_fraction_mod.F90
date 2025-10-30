MODULE assign_irrig_fraction_mod

! Description:
!   Module to update irrigation fractions over tiles and set mimimum threshold
!
! *****************************COPYRIGHT***************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use
! and distribution under the JULES collaboration agreement, subject
! to the terms and conditions set out therein.
!
! [Met Office Ref SC0237]
! *****************************COPYRIGHT***************************************

IMPLICIT NONE

CONTAINS

SUBROUTINE assign_irrig_fraction (crop_vars,ainfo)

USE ereport_mod, ONLY: ereport
USE errormessagelength_mod, ONLY: errormessagelength

USE ancil_info, ONLY: nsoilt

USE jules_surface_types_mod, ONLY: ntype

USE jules_irrig_mod, ONLY:                                                     &
!  imported logicals with intent(in)
   set_irrfrac_on_irrtiles,                                                    &
   frac_irrig_all_tiles, irrtiles, nirrtile

!TYPE definitions
USE crop_vars_mod, ONLY: crop_vars_type
USE ancil_info,    ONLY: ainfo_type


!Module imports - Variables required in UM-mode
#if defined(UM_JULES)
USE nlsizes_namelist_mod,     ONLY: nsurft            => ntiles,               &
                                    land_pts          => land_field
#else
USE ancil_info, ONLY:         land_pts, nsurft
#endif


IMPLICIT NONE

!Arguments
!TYPES containing field data (IN OUT)
TYPE(crop_vars_type), INTENT(IN OUT) :: crop_vars
TYPE(ainfo_type), INTENT(IN OUT) :: ainfo

INTEGER ::                                                                     &
 m,l,k,n
                       ! Loop indices

INTEGER ::   alltiles(nsurft)

! Local arrays
REAL ::                                                                        &
 frac_irr_rest_soilt(land_pts,nsoilt)
                      ! Remaining irrigation fraction to assign to tiles

CHARACTER(LEN=errormessagelength) :: cmessage
INTEGER                           :: errorstatus

!-------------------------------------------------------------------------------
!   Update irrigation ancillary
!-------------------------------------------------------------------------------
! create index of all tiles (veg+non-veg)
DO m = 1,nsurft
  alltiles(m) = m
END DO

! Loop over land points and assign irrigation fraction
! note frac_irr_all has shape (land_points, year) or (land_points, 1)

IF ( set_irrfrac_on_irrtiles ) THEN

  crop_vars%frac_irr_old_soilt(:,:) = crop_vars%frac_irr_soilt(:,:)
  crop_vars%frac_irr_soilt(:,:) = 0.0

  DO k = 1,nirrtile ! loop over irrigated pfts
    n = irrtiles(k)
    !Set the current soil tile (see notice above)
    IF (nsoilt == 1) THEN
      !There is only 1 soil tile
      m = 1
    ELSE ! nsoilt == nsurft
      !Soil tiles map directly on to surface tiles
      m = k
    END IF

    DO l = 1,land_pts
      crop_vars%frac_irr_soilt(l,m) = crop_vars%frac_irr_soilt(l,m) +          &
                             crop_vars%irrfrac_irrtiles(l,1) *                 &
                             ainfo%frac_surft(l, n)
    END DO
  END DO
ELSE
  DO m = 1,nsoilt
    DO l = 1,land_pts
      crop_vars%frac_irr_old_soilt(l,m) = crop_vars%frac_irr_soilt(l,m)
      crop_vars%frac_irr_soilt(l,m) = crop_vars%frac_irr_all(l,1)
    END DO
  END DO
END IF

WHERE (crop_vars%frac_irr_soilt < 1.0e-3) crop_vars%frac_irr_soilt = 0.0

!-------------------------------------------------------------------------------
!   Assign irrigation fraction to each tile
!   hadrd - this was originally done in physiol.f90
!-------------------------------------------------------------------------------
IF ( .NOT. frac_irrig_all_tiles .AND. .NOT. set_irrfrac_on_irrtiles ) THEN
  ! assign irrigated fraction to pre-defined tiles ONLY
  ! in decreasing order of importance
  crop_vars%frac_irr_surft(:,:) = 0.0
  frac_irr_rest_soilt(:,:) = crop_vars%frac_irr_soilt(:,:)

  DO l = 1,land_pts
    DO k = 1,nirrtile ! loop over irrigated pfts

      !==============================================================================
      !**NOTICE REGARDING SOIL TILING**
      !
      !The following section facilitates the use of soil tiling. As implemented,
      !there are two soil tiling options:
      !
      !nsoilt == 1
      !Operate as with a single soil tile, functionally identical to JULES upto
      ! at least vn4.7 (Oct 2016)
      ! This means that a soilt variable being passed 'up' to the surface is
      ! broadcast to the surft variable (with weighting by frac if requred)
      !
      !nsoilt > 1
      !Operate with nsoilt = nsurft, with a direct mapping between them
      ! This means that a soilt variable being passed 'up' to the surface is simply
      ! copied into the surft variable
      !
      ! This will need to be refactored for other tiling approaches. This note
      ! will be replicated elsewhere in the code as required
      !
      !These comments apply until **END NOTICE REGARDING SOIL TILING**
      !==============================================================================

              !Set the current soil tile (see notice above)
      IF (nsoilt == 1) THEN
        !There is only 1 soil tile
        m = 1
      ELSE ! nsoilt == nsurft
        !Soil tiles map directly on to surface tiles
        m = k
      END IF !nsoilt

      !==============================================================================
      !**END NOTICE REGARDING SOIL TILING**
      !==============================================================================

      ! for each tile, check if the index corresponds with index in irrtiles
      DO n = 1,ntype
        IF ( alltiles(n) == irrtiles(k) ) THEN
          ! assign (remaining) irrigated fraction to this tile
          crop_vars%frac_irr_surft(l,n)      = MIN(frac_irr_rest_soilt(l,m),   &
                                         ainfo%frac_surft(l,n) )
          frac_irr_rest_soilt(l,m) = frac_irr_rest_soilt(l,m)                  &
                                     - crop_vars%frac_irr_surft(l,n)
          ! check for negative remaining frac_irr_soilt
          IF (frac_irr_rest_soilt(l,m) < 0.0) THEN
            errorstatus = 1
            WRITE (cmessage,"(A,A,I1,A,I6,A,I6,A,I6)") "Error assigning ",     &
              "irrigated fraction at point - " //                              &
              " l = ",l,                                                       &
              ", irrigated fraction: ", crop_vars%frac_irr_soilt(l,m),         &
              ", tile fraction: ", ainfo%frac_surft(l,n),                      &
              ", irrigated fraction assigned: ", crop_vars%frac_irr_surft(l,n)
            CALL ereport ("assign_irrig_fraction", errorstatus, cmessage)
          END IF
        END IF
      END DO ! ntype
    END DO ! irrtilenames
  END DO ! land_pts

  !to prevent negative fractions (this can happen due to numerical inaccuracy)
  crop_vars%frac_irr_surft(:,:) = MAX(crop_vars%frac_irr_surft(:,:),0.0)

  ! in sf_evap, frac_irr_surft is used as a multiplier to frac
  WHERE ( ainfo%frac_surft > 0.0 )
    crop_vars%frac_irr_surft = crop_vars%frac_irr_surft / ainfo%frac_surft
  END WHERE

ELSE ! if frac_irrig_all_tiles = TRUE
  ! assign irrigation fraction to all tiles
  ! to reproduce the original results
  crop_vars%frac_irr_surft(:,:) = 0.0
  DO n = 1,nsurft

    !==============================================================================
    !**NOTICE REGARDING SOIL TILING**
    !
    !The following section facilitates the use of soil tiling. As implemented,
    !there are two soil tiling options:
    !
    !nsoilt == 1
    !Operate as with a single soil tile, functionally identical to JULES upto
    ! at least vn4.7 (Oct 2016)
    ! This means that a soilt variable being passed 'up' to the surface is
    ! broadcast to the surft variable (with weighting by frac if requred)
    !
    !nsoilt > 1
    !Operate with nsoilt = nsurft, with a direct mapping between them
    ! This means that a soilt variable being passed 'up' to the surface is simply
    ! copied into the surft variable
    !
    ! This will need to be refactored for other tiling approaches. This note
    ! will be replicated elsewhere in the code as required
    !
    !These comments apply until **END NOTICE REGARDING SOIL TILING**
    !==============================================================================

          !Set the current soil tile (see notice above)
    IF (nsoilt == 1) THEN
      !There is only 1 soil tile
      m = 1
    ELSE ! nsoilt == nsurft
      !Soil tiles map directly on to surface tiles
      m = n
    END IF !nsoilt

    !==============================================================================
    !**END NOTICE REGARDING SOIL TILING**
    !==============================================================================

    WHERE ( crop_vars%frac_irr_soilt(:,m) > EPSILON(1.0) )
      crop_vars%frac_irr_surft(:,n) = crop_vars%frac_irr_soilt(:,m)
    END WHERE
  END DO

END IF ! .not. frac_irrig_all_tiles

END SUBROUTINE assign_irrig_fraction

END MODULE assign_irrig_fraction_mod