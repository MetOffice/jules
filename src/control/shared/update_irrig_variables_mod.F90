MODULE update_irrig_variables_mod

! Description:
!   Module to update and adjust soil moistures where irrigation fraction > 0
!
! *****************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use
! and distribution under the JULES collaboration agreement, subject
! to the terms and conditions set out therein.
!
! [Met Office Ref SC0237]
! *****************************COPYRIGHT****************************************

IMPLICIT NONE

CONTAINS

SUBROUTINE update_irrig_variables (crop_vars,psparms,ainfo)

USE ancil_info, ONLY: land_pts, nsoilt

USE jules_surface_types_mod, ONLY: ncpft, nnpft

USE jules_irrig_mod, ONLY: irr_crop, irr_crop_dvimax

!TYPE definitions
USE crop_vars_mod, ONLY: crop_vars_type
USE p_s_parms, ONLY: psparms_type
USE ancil_info,    ONLY: ainfo_type

USE jules_soil_mod, ONLY: sm_levels

IMPLICIT NONE

!Arguments
!TYPES containing field data (IN OUT)
TYPE(crop_vars_type), INTENT(IN OUT) :: crop_vars
TYPE(psparms_type), INTENT(IN OUT) :: psparms
TYPE(ainfo_type), INTENT(IN OUT) :: ainfo

INTEGER ::                                                                     &
 l,n,m

! Local arrays
REAL ::                                                                        &
 sthu_nir(land_pts,sm_levels)
                      ! soil moisture content in non-irrigated fraction as
                      ! a fraction of saturation

!-------------------------------------------------------------------------------
!   When using JULES-crop model, adjust irrigation fraction
!   to those tiles that have suitable development index
!-------------------------------------------------------------------------------
!   When irr_crop=2, JULES-crop is used to determine irrigation period.
!   However, within a grid box the development index can be different for each
!   crop tile while irrigation is applied to the (single) irrigated fraction of
!   that grid box.
!   Here frac_irr_surft is set to zero in those tiles where the dvi_cpft is below
!   the threshold and irrigation is only applied to those tiles with suitable
!   dvi_cpft, i.e. the actual irrigation fraction grows or shrinks according to
!   which crop tiles have the right dvi_cpft. Note that frac_irr_soilt can also be
!   allocated to non-crop tiles, these will (currently) only be irrigated when
!   any crop has a dvi_cpft above the threshold - see subroutine irrig_dmd.

IF ( irr_crop == irr_crop_dvimax ) THEN
  DO l = 1,land_pts
    DO n = 1,ncpft

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
        !Soil tiles map directly on to surface tiles, remembering we're
        !working on crop tiles
        m = nnpft + n
      END IF !nsoilt
      !==============================================================================
      !**END NOTICE REGARDING SOIL TILING**
      !==============================================================================

      IF ( crop_vars%dvi_cpft(l,n) <= -1 ) THEN
        !         no suitable dvi_cpft - set frac_irr_soilt for this crop tile to zero
        !             and subtract from overall irrigation fraction
        crop_vars%frac_irr_soilt(l,m) = crop_vars%frac_irr_soilt(l,m)          &
                    - ainfo%frac_surft(l,n + nnpft)                            &
                    * crop_vars%frac_irr_surft(l,n + nnpft)
        crop_vars%frac_irr_soilt(l,m) =                                        &
                    MAX( crop_vars%frac_irr_soilt(l,m), 0.0 )
        crop_vars%frac_irr_surft(l,n + nnpft) = 0.0
      END IF
    END DO
  END DO
END IF

!-------------------------------------------------------------------------------
!   Update soil moisture content in irrigated fraction (sthu_irr_soilt)
!-------------------------------------------------------------------------------
!   If frac_irr expands, the 'added fraction' does not have the moisture content
!   of the irrigated fraction, but of the non-irrigated fraction (which is not
!   a prognostic). Total gridbox moisture content should remain the same
!   Conversely, when frac_irr shrinks, the moisture content in the non-irrig
!   fraction should become higher, but in the (remaining) irrigated fraction
!   (and total gridbox) it remains the same, so no need to cater for that here

DO m = 1, nsoilt
  DO l = 1,land_pts
    IF ( crop_vars%frac_irr_soilt(l,m) > crop_vars%frac_irr_old_soilt(l,m) ) THEN
      DO n = 1,sm_levels

        !Note the hard-coded index of 1 for soil tiling.
        sthu_nir(l,n) = (psparms%sthu_soilt(l,m,n)                             &
                         - crop_vars%frac_irr_old_soilt(l,m)                   &
                         * crop_vars%sthu_irr_soilt(l,m,n))                    &
                        / (1.0 - crop_vars%frac_irr_old_soilt(l,m))

        crop_vars%sthu_irr_soilt(l,m,n) =                                      &
          (crop_vars%frac_irr_old_soilt(l,m) *                                 &
           crop_vars%sthu_irr_soilt(l,m,n) +                                   &
          (crop_vars%frac_irr_soilt(l,m) - crop_vars%frac_irr_old_soilt(l,m))  &
          * sthu_nir(l,n)) / crop_vars%frac_irr_soilt(l,m)
      END DO
    END IF
  END DO
END DO

END SUBROUTINE update_irrig_variables

END MODULE update_irrig_variables_mod
