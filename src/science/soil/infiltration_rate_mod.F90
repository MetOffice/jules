! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Calculates the maximum surface infiltration rate for each tile
! Only needs to be called when frac is populated or changed
!
! If this parametrisation is updated to depend on more variables, then please
! review where this routine is called. Current sparm calling points to check
! are:
!
! Where this routine is NOT called:
! -control/um/update_veg.F90
! -control/standalone/update/update_derived_variables.inc
!
! Where this routine is called:
! -initialisation/um/init_veg.F90
! -initialisation/standalone/init_parms.F90
! -science/vegetation/veg-veg1a_jls.F90 (Under review!)
! -science/vegetation/veg-veg2a_jls.F90
!
! These calculations were formerly in sparm and pft_sparm
!

MODULE infiltration_rate_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='INFILTRATION_RATE_MOD'

CONTAINS

SUBROUTINE infiltration_rate(land_pts, nsurft, surft_pts, surft_index,         &
                             satcon_soilt_sfc, frac, infil_surft)

! USE in relevant variables
USE jules_surface_types_mod,  ONLY: npft, ntype
USE pftparm,                  ONLY: infil_f
USE nvegparm,                 ONLY: infil_nvg
USE jules_surface_mod,        ONLY: l_aggregate
USE ancil_info,               ONLY: nsoilt

USE parkind1,                 ONLY: jprb, jpim
USE yomhook,                  ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Number of land points.
  nsurft
    ! Number of surface tiles.

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  surft_pts(ntype),                                                            &
    ! Number of tile points.
  surft_index(land_pts,ntype)
    ! Index of tile points.

REAL(KIND=real_jlslsm), INTENT(IN)    ::                                       &
  satcon_soilt_sfc(land_pts,nsoilt),                                           &
    ! Saturated hydraulic conductivity of the soil surface (kg/m2/s).
  frac(land_pts,ntype)
    ! Fractional cover of each surface type.

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT)   ::                                       &
  infil_surft(land_pts,nsurft)
    ! Max infiltration rate for each tile (kg/m2/s).

!-----------------------------------------------------------------------------
! Local variables:
!-----------------------------------------------------------------------------
INTEGER :: j, l, n, m

! Internal values of infilration rate to facilitate aggregation.
REAL(KIND=real_jlslsm) ::                                                      &
  infil(land_pts),                                                             &
    ! Max infiltration rate (kg m-2 s-1).
  infil_t(land_pts,ntype)
    ! Max infiltration rate on surface types (kg m-2 s-1).

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='INFILTRATION_RATE'

!End of header
!-----------------------------------------------------------------------------
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!=============================================================================
! *NOTICE REGARDING SOIL TILING**
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
!=============================================================================

! Vegetated tiles
DO n = 1,npft

  ! Set the current soil tile (see notice above)
  IF (nsoilt == 1) THEN
    ! There is only 1 soil tile
    m = 1
  ELSE ! nsoilt == nsurft
    ! Soil tiles map directly on to surface tiles
    m = n
  END IF ! nsoilt

  DO j = 1,surft_pts(n)
    l = surft_index(j,n)
    infil_t(l,n) = infil_f(n) * satcon_soilt_sfc(l,m)
  END DO
END DO

! Non-vegetated tiles
DO n = npft+1,ntype

  ! Set the current soil tile (see notice above)
  IF (nsoilt == 1) THEN
    ! There is only 1 soil tile
    m = 1
  ELSE ! nsoilt == nsurft
    ! Soil tiles map directly on to surface tiles
    m = n
  END IF ! nsoilt

  DO j = 1,surft_pts(n)
    l = surft_index(j,n)
    infil_t(l,n) = infil_nvg(n - npft) * satcon_soilt_sfc(l,m)
  END DO
END DO

!=============================================================================
! *END NOTICE REGARDING SOIL TILING**
!=============================================================================

! Post the results into infil, depending on whether aggregation is being used
IF ( l_aggregate ) THEN

  infil(:) = 0.0
  DO n = 1,ntype
    DO j = 1,surft_pts(n)
      l = surft_index(j,n)
      infil(l) = infil(l) + frac(l,n) * infil_t(l,n)
    END DO
  END DO

  DO l = 1,land_pts
    infil_surft(l,1) = infil(l)
  END DO

ELSE

  DO n = 1,ntype
    DO j = 1,surft_pts(n)
      l = surft_index(j,n)
      infil_surft(l,n) = infil_t(l,n)
    END DO
  END DO

END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE infiltration_rate
END MODULE infiltration_rate_mod
