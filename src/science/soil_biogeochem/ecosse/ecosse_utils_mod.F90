#if !defined(UM_JULES)
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

MODULE ecosse_utils_mod

!-----------------------------------------------------------------------------
! Description:
!   Utilities (i.e. helpful bits) for use with the ECOSSE model.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGEOCHEMISTRY
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

PRIVATE
PUBLIC                                                                         &
  ! shared parameters
  nt,                                                                          &
  ! shared procedures
  adjust_soil, get_residual_n, transfer_layers

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName = 'ECOSSE_UTILS_MOD'

!-----------------------------------------------------------------------------
! Module parameters.
!-----------------------------------------------------------------------------
INTEGER, PARAMETER :: nt = 2
  ! Number of time levels stored for prognostic variables (the local copies
  ! of the prognostic variables used in the science code).
  ! This is expected to be 2, in which case explicit fluxes are used, and
  ! values at the start and end of the timestep are held in positions 1 and 2
  ! respectively.
  ! A value of 1 would mean that each process subroutine uses the updated
  ! (end of timestep) value from the previous subroutine, e.g. the inputs
  ! values to subroutine #2 are the end of timestep values from subroutine
  ! #1, i.e. generally not explicit fluxes.

CONTAINS

!#############################################################################
!#############################################################################

FUNCTION transfer_layers( land_pts,vs_pts,nzIn,nzOut,l_midpoint,l_interp,      &
                          vs_index,dzIn,dzOut,inval )                          &
         RESULT(outval)

! Given a land field on one set of layers, map onto another set of layers.
! In general the mean value is not conserved.
! Originally introduced to map from soil moisture to ECOSSE layers.
! The method used depends on the value of the switch l_interp.
!   l_interp = .TRUE.  :  linear interpolation/extrapolation is used.
!   l_interp = .FALSE. :  depth-weighted average of the input.

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Number of land points.
  vs_pts,                                                                      &
    ! Number of soil/veg points.
  nzIn,                                                                        &
    ! Number of levels in input variables.
  nzout
    ! Number of levels in output variables.

LOGICAL, INTENT(IN) ::                                                         &
  l_midpoint,                                                                  &
    ! Flag indicating whether variables apply at mid-point or bottom of each
    ! layer, used for interpolation (l_interp=.TRUE.).
    ! .TRUE. means variables apply at layer mid-points.
    ! .FALSE. means variables apply at bottom of each layer.
  l_interp
    ! Switch to select linear interpolation.

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN)  :: vs_index(land_pts)
    ! Indices of veg/soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  dzIn(nzIn),                                                                  &
    ! Thicknesses of input layers (m).
  dzOut(nzOut),                                                                &
    ! Thicknesses of output layers (m).
  inval(land_pts,nzIn)
    ! Input field, on dzIn layers.

!-----------------------------------------------------------------------------
! Function result - an array on the output layers.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) :: outval(land_pts,nzOut)

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'TRANSFER_LAYERS'

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER :: iz
  ! Loop counter.

REAL(KIND=real_jlslsm) ::                                                      &
  zIn(nzIn),                                                                   &
    ! Depths at which input values apply (m).
  zOut(nzOut)
    ! Depths at which output values apply (m).

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle
!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF ( l_interp ) THEN
  ! Interpolate.
  ! Calculate depths at which input variables are valid.
  IF ( l_midpoint ) THEN
    ! Calculate depth to mid-point of each layer.
    zIn(1) = 0.5 * dzIn(1)
    DO iz = 2,nzIn
      zIn(iz) =  zIn(iz-1) + 0.5 * ( dzIn(iz-1) + dzIn(iz) )
    END DO
    zOut(1) = 0.5 * dzOut(1)
    DO iz = 2,nzOut
      zOut(iz) =  zOut(iz-1) + 0.5 * ( dzOut(iz-1) + dzOut(iz) )
    END DO
  ELSE
    ! Calculate depth to bottom of each layer.
    zIn(1) = dzIn(1)
    DO iz = 2,nzIn
      zIn(iz) = zIn(iz-1) + dzIn(iz)
    END DO
    zOut(1) = dzOut(1)
    DO iz = 2,nzOut
      zOut(iz) = zOut(iz-1) + dzOut(iz)
    END DO
  END IF
  CALL linear_interp( land_pts, vs_pts, nzIn, nzOut, vs_index, zIn, zOut,      &
                      inval, outval )
ELSE

  ! Calculate average. This is only coded for nzOut=1.
  CALL depth_average( land_pts, vs_pts, nzIn, dzOut(1), vs_index, dzIn,        &
                      inval, outval(:,1) )

END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END FUNCTION transfer_layers

!#############################################################################
!#############################################################################

SUBROUTINE depth_average( np, np_index, nzIn, dzOut, index_val, dzIn, inval,   &
                          outval )

! Calculate the weighted average of input values in a layer at the surface.

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  np,                                                                          &
    ! Size of arrays.
  np_index,                                                                    &
    ! Number of indexed points.
  nzIn
    ! Number of layers of input field.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  dzOut
    ! Thickness of output layer (m).

!-----------------------------------------------------------------------------
! Array arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) :: index_val(np)
    ! Indices of points to process.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  dzIn(nzIn),                                                                  &
    ! Thicknesses of input layers (m).
  inval(np,nzIn)
    ! Input field, on dzIn layers.

!-----------------------------------------------------------------------------
! Array arguments with intent(out).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::  outval(np)
   ! Output field, on dzOut layers.

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'DEPTH_AVERAGE'

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: i, iz, j
   ! Loop counters/indices.

REAL(KIND=real_jlslsm) :: z1, z2
   ! Depths to top and bottom of layer (m).

!-----------------------------------------------------------------------------
! Local array variables.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) :: sumVal(np)
   ! Accumulating sum of weighted values.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Loop over input layers, adding to accumulations if any of this layer
! overlaps with the output layer.
!-----------------------------------------------------------------------------
z2        = 0.0
sumVal(:) = 0.0
DO iz = 1,nzIn
  z1 = z2
  z2 = z2 + dzIn(iz)
  IF ( z2 < dzOut ) THEN
    ! Input layer is entirely within output layer.
    DO j = 1,np_index
      i = index_val(j)
      sumVal(i) = sumVal(i) + inval(i,iz) * dzIn(iz)
    END DO
  ELSE IF ( z2 >= dzOut .AND. z1 <  dzOut ) THEN
    ! Input layer partially overlaps with output layer.
    DO j = 1,np_index
      i = index_val(j)
      sumVal(i) = sumVal(i) + inval(i,iz) * ( dzOut - z1 )
    END DO
  END IF
END DO

! Normalise the result.
DO j = 1,np_index
  i = index_val(j)
  outval(i) = sumVal(i) / dzOut
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE depth_average

!#############################################################################
!#############################################################################

SUBROUTINE linear_interp( np,np_index,nzIn,nzOut,index_val,zIn,zOut,inval,     &
                          outval )

! The value in each output layer is linearly interpolated from input.
! There is no extrapolation, rather end values are used (zero gradient).

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  np,                                                                          &
    ! Size of arrays.
  np_index,                                                                    &
    ! Number of indexed points.
  nzIn,                                                                        &
    ! Number of layers of input field.
  nzOut
    ! Number of layers of output field.

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) :: index_val(np)
   ! Indices of points to process.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  zIn(nzIn),                                                                   &
    ! Depths at which input data are valid, in ascending order (m).
  zOut(nzOut),                                                                 &
    ! Depths at which output data are valid, in ascending order (m).
  inval(np,nzIn)
    ! Input field.

!-----------------------------------------------------------------------------
! Arguments with intent(out).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::  outval(np,nzOut)
   ! Output field.

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'LINEAR_INTERP'

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER :: i, izIn, izOut, j, jz
   ! Loop counters/indices.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DO izOut = 1,nzOut
  !-------------------------------------------------------------------------
  ! Locate output abscissa within the input axis.
  ! We anticipate small arrays and infrequent calls, so a simple approach
  ! suffices. Larger arrays could use a binary search, or similar.
  !-------------------------------------------------------------------------
  IF ( zOut(izOut) <= zIn(1) ) THEN
    ! We are at or below the bottom of the input range. Use lowest value.
    DO j = 1,np_index
      i = index_val(j)
      outval(i,izOut) = inval(i,1)
    END DO
  ELSE IF ( zOut(izOut) >= zIn(nzIn) ) THEN
    ! We are at or beyond the top of the input range. Use highest value.
    DO j = 1,np_index
      i = index_val(j)
      outval(i,izOut) = inval(i,nzIn)
    END DO
  ELSE
    ! Locate value of the abscissa at or below the desired value.
    DO izIn = 1,nzIn-1
      IF ( zOut(izOut) <= zIn(izIn+1) ) THEN
        jz = izIn
        EXIT
      END IF
    END DO
    ! Interpolate between values jz and jz+1.
    DO j = 1,np_index
      i = index_val(j)
      outval(i,izOut) = inval(i,jz) +                                          &
                        ( zOut(izout) - zIn(jz) ) / ( zIn(jz+1) - zIn(jz) )    &
                        * ( inval(i,jz+1) - inval(i,jz) )
    END DO

  END IF

END DO  !  izOut (output layers)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE linear_interp

!#############################################################################
!#############################################################################

SUBROUTINE get_residual_n( land_pts, vs_pts, vs_index, residual_n )

! Description:
!   Calculates the minimum-allowed (residual) N content of each layer given
!   the minimum concentration.
!
!   At present this is trivial but in future this could be implemented as a
!   function of clay content.

USE ancil_info, ONLY:                                                          &
  ! imported scalars
  nz_soilc=>dim_cslayer

USE jules_soil_ecosse_mod, ONLY:                                               &
  ! imported arrays
  dz_soilc

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Number of land points.
  vs_pts
    ! Number of points with veg and/or soil.

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) :: vs_index(land_pts)
    ! Indices of veg/soil points.

!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) :: residual_n(land_pts,nz_soilc)
    ! Minimum-allowed (residual) inorganic N amount (kg m-2).

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  min_n_conc = 0.001
    ! Minimum-allowed (residual) inorganic N concentration (kg m-3).

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'GET_RESIDUAL_N'

!-----------------------------------------------------------------------------
! Local variables
!-----------------------------------------------------------------------------
INTEGER :: i, j    ! Loop counters/indices.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Initialise.
!-----------------------------------------------------------------------------
residual_n(:,:) = 0.0

!-----------------------------------------------------------------------------
! Caculate the minimum-allowed N.
!-----------------------------------------------------------------------------
DO j = 1,vs_pts
  i = vs_index(j)
  residual_n(i,:) = min_n_conc * dz_soilc(:)
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE get_residual_n

!#############################################################################
!#############################################################################

SUBROUTINE adjust_soil( land_pts, vs_pts, initial_call, vs_index, biohum_nc,   &
                        c_dpm, c_rpm, c_bio, c_hum,                            &
                        n_dpm, n_rpm, n_bio, n_hum, co2_soil_gb,               &
                    ! These arguments replace USE statements
                        soil_c_add, soil_n_add )

! Description:
!   Adjusts soil stores to impose minimum C store and max C:N ratio.

USE ancil_info, ONLY:                                                          &
  ! imported scalars
  nz_soilc => dim_cslayer

USE ecosse_param_mod, ONLY:                                                    &
  ! imported scalar parameters
  cn_max_litter_pools

USE jules_soil_ecosse_mod, ONLY:                                               &
  ! imported scalars
  dt_soilc, l_soil_N

USE jules_soil_mod, ONLY:                                                      &
  ! imported scalars
  cs_min

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! The number of land points.
  vs_pts
    ! The number of points with veg and/or soil.

LOGICAL, INTENT(IN) ::                                                         &
  initial_call
    ! TRUE when called during initialisation, else FALSE.

!-----------------------------------------------------------------------------
! Array arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) :: vs_index(land_pts)
    ! Indices of veg/soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  biohum_nc(land_pts,nz_soilc)
    ! Stable N:C ratio of the BIO and HUM pools (kgN/kgC).

!-----------------------------------------------------------------------------
! Array arguments with intent(inout).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  c_dpm(land_pts,nz_soilc),                                                    &
    ! C in DPM in a soil layer (kg m-2).
  c_rpm(land_pts,nz_soilc),                                                    &
    ! C in RPM in a soil layer (kg m-2).
  c_bio(land_pts,nz_soilc),                                                    &
    ! C in BIO in a soil layer (kg m-2).
  c_hum(land_pts,nz_soilc),                                                    &
    ! C in HUM in a soil layer (kg m-2).
  n_dpm(land_pts,nz_soilc),                                                    &
    ! N in DPM in a soil layer (kg m-2).
  n_rpm(land_pts,nz_soilc),                                                    &
    ! N in RPM in a soil layer (kg m-2).
  n_bio(land_pts,nz_soilc),                                                    &
    ! N in BIO in a soil layer (kg m-2).
  n_hum(land_pts,nz_soilc),                                                    &
    ! N in HUM in a soil layer (kg m-2).
  co2_soil_gb(:)
    ! C in CO2 flux from soil to atmosphere (kg m-2 s-1).

!-----------------------------------------------------------------------------
! These arguments replace USE statements
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::soil_c_add(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) ::soil_n_add(land_pts)

!-----------------------------------------------------------------------------
! Local scalar parameters.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), PARAMETER :: n_toler = 1.0e-9
    ! A small amount of N (kg m-2). If a pool differs from the amount expected
    ! (from a given C:N) by more than this, it is adjusted.

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: i, iz, l  ! Indices.

REAL(KIND=real_jlslsm) :: dpool  ! Amount added to a pool (kg m-2).

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'ADJUST_SOIL'

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DO i = 1,vs_pts
  l = vs_index(i)

  !---------------------------------------------------------------------------
  ! Carbon stores.
  ! Each pool in each layer is adjusted independently to ensure it is >= the
  ! minimum-allowed value. No consideration is taken here of the C:N ratio -
  ! that is dealt with separately below.
  ! A more complicated alternative would be to consider if other pools can
  ! donate C.
  ! Any C added is later removed from the CO2 flux.
  !---------------------------------------------------------------------------
  ! C DPM.
  !---------------------------------------------------------------------------
  DO iz = 1,nz_soilc
    IF ( c_dpm(l,iz) < cs_min ) THEN
      dpool         = cs_min - c_dpm(l,iz)
      c_dpm(l,iz)   = cs_min
      soil_c_add(l) = soil_c_add(l) + dpool
    END IF
  END DO
  !---------------------------------------------------------------------------
  ! C RPM.
  !---------------------------------------------------------------------------
  DO iz = 1,nz_soilc
    IF ( c_rpm(l,iz) < cs_min ) THEN
      dpool         = cs_min - c_rpm(l,iz)
      c_rpm(l,iz)   = cs_min
      soil_c_add(l) = soil_c_add(l) + dpool
    END IF
  END DO
  !---------------------------------------------------------------------------
  ! C BIO.
  !---------------------------------------------------------------------------
  DO iz = 1,nz_soilc
    IF ( c_bio(l,iz) < cs_min ) THEN
      dpool         = cs_min - c_bio(l,iz)
      c_bio(l,iz)   = cs_min
      soil_c_add(l) = soil_c_add(l) + dpool
    END IF
  END DO
  !---------------------------------------------------------------------------
  ! C HUM.
  !---------------------------------------------------------------------------
  DO iz = 1,nz_soilc
    IF ( c_hum(l,iz) < cs_min ) THEN
      dpool         = cs_min - c_hum(l,iz)
      c_hum(l,iz)   = cs_min
      soil_c_add(l) = soil_c_add(l) + dpool
    END IF
  END DO

  IF ( l_soil_n ) THEN
    !-------------------------------------------------------------------------
    ! Nitrogen stores stores.
    ! Each pool in each layer is adjusted independently to ensure that the C:N
    ! ratio is <= the maximum-allowed value for that pool.
    !-------------------------------------------------------------------------
    ! N DPM.
    ! Ensure max C:N by adding N.
    !-------------------------------------------------------------------------
    DO iz = 1,nz_soilc
      IF ( n_dpm(l,iz) < c_dpm(l,iz) / cn_max_litter_pools ) THEN
        dpool         = c_dpm(l,iz) / cn_max_litter_pools - n_dpm(l,iz)
        n_dpm(l,iz)   = c_dpm(l,iz) / cn_max_litter_pools
        soil_n_add(l) = soil_n_add(l) + dpool
      END IF
    END DO
    !-------------------------------------------------------------------------
    ! N RPM.
    ! Ensure max C:N by adding N.
    !-------------------------------------------------------------------------
    DO iz = 1,nz_soilc
      IF ( n_rpm(l,iz) < c_rpm(l,iz) / cn_max_litter_pools ) THEN
        dpool         = c_rpm(l,iz) / cn_max_litter_pools - n_rpm(l,iz)
        n_rpm(l,iz)   = c_rpm(l,iz) / cn_max_litter_pools
        soil_n_add(l) = soil_n_add(l) + dpool
      END IF
    END DO
    !-----------------------------------------------------------------------
    ! N_bio.
    ! Impose the target C:N. This could mean adding or removing N.
    !-----------------------------------------------------------------------
    DO iz = 1,nz_soilc
      IF ( ABS(n_bio(l,iz) - c_bio(l,iz) * biohum_nc(l,iz)) > n_toler ) THEN
        dpool         = c_bio(l,iz) * biohum_nc(l,iz) - n_bio(l,iz)
        n_bio(l,iz)   = c_bio(l,iz) * biohum_nc(l,iz)
        soil_n_add(l) = soil_n_add(l) + dpool
      END IF
    END DO
    !-----------------------------------------------------------------------
    ! N_hum.
    ! Impose the target C:N. This could mean adding or removing N.
    !-----------------------------------------------------------------------
    DO iz = 1,nz_soilc
      IF ( ABS(n_hum(l,iz) - c_hum(l,iz) * biohum_nc(l,iz)) > n_toler ) THEN
        dpool         = c_hum(l,iz) * biohum_nc(l,iz) - n_hum(l,iz)
        n_hum(l,iz)   = c_hum(l,iz) * biohum_nc(l,iz)
        soil_n_add(l) = soil_n_add(l) + dpool
      END IF
    END DO

  END IF  !  l_soil_n

  !---------------------------------------------------------------------------
  ! Reduce the CO2 flux to account for any C added.
  ! At present any N added is not conserved.
  !---------------------------------------------------------------------------
  co2_soil_gb(l) = co2_soil_gb(l) - soil_c_add(l) / dt_soilc

END DO  !  i (points)

!-----------------------------------------------------------------------------
! Reset variables if this was a call during initiallisation.
! We don't want these increments to be recorded with any later increments.
!-----------------------------------------------------------------------------
IF ( initial_call ) THEN

  soil_c_add(:) = 0.0
  IF ( l_soil_n ) THEN
    soil_n_add(:) = 0.0
  END IF

END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

END SUBROUTINE adjust_soil

!#############################################################################
!#############################################################################

END MODULE ecosse_utils_mod
#endif
