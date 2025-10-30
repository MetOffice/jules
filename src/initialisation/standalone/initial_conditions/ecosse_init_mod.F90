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

MODULE ecosse_init_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

! Description:
!   This module contains code that is used to initialise the ECOSSE model, in
!   particular to ensure consistency between variables.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGEOCHEMISTRY
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.

PRIVATE
PUBLIC ecosse_init

CONTAINS

!#############################################################################
!#############################################################################

SUBROUTINE ecosse_init(soil_pH_soilt,frac_surft,cs_pool_soilt)

USE ancil_info, ONLY:                                                          &
  ! imported scalars
  land_pts, nsoilt, nz_soilc=>dim_cslayer, dim_cslayer, dim_cs1

USE jules_surface_types_mod,  ONLY: ntype

USE ecosse_prepare_mod, ONLY:                                                  &
  ! imported procedures
  stable_n_c

USE ecosse_utils_mod, ONLY:                                                    &
  ! imported procedures
  adjust_soil, get_residual_n

USE jules_soil_ecosse_mod, ONLY:                                               &
  ! imported scalars
  l_soil_N

USE soil_ecosse_vars_mod, ONLY:                                                &
  ! imported parameters
  i_amm, i_nit

USE veg_soil_index_mod, ONLY:                                                  &
  ! imported procedures
  get_veg_soil_index

USE jules_fields_mod, ONLY: soilecosse

IMPLICIT NONE

!arguments

REAL(KIND=real_jlslsm), INTENT(IN) :: soil_pH_soilt(land_pts,nsoilt,dim_cslayer)
REAL(KIND=real_jlslsm), INTENT(IN) :: frac_surft(land_pts,ntype)

REAL(KIND=real_jlslsm), INTENT(IN OUT) :: cs_pool_soilt(land_pts,nsoilt,       &
                                                    dim_cslayer,dim_cs1)

!-----------------------------------------------------------------------------
! Local scalar parameters.
!-----------------------------------------------------------------------------
LOGICAL, PARAMETER ::                                                          &
  l_init_true = .TRUE.
    ! For use as an argument to subroutine adjust_soil, to indicate to that
    ! routine that it is being called during initialisation.

!-----------------------------------------------------------------------------
! Local scalar variables
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  vs_pts,                                                                      &
    ! The number of points with veg and/or soil.
  i, l,                                                                        &
    ! Indices.
  s ! Soil tile number.

!-----------------------------------------------------------------------------
! Local array variables
!-----------------------------------------------------------------------------
INTEGER :: vs_index(land_pts)      !  indices of veg/soil points

REAL(KIND=real_jlslsm) ::                                                      &
  biohum_nc(land_pts,nz_soilc),                                                &
    ! Stable N:C ratio of biomass and humus pools.
  co2_dummy(land_pts),                                                         &
    ! Local or "dummy" variable so we can provide an argument. Notionally this
    ! is the C in the CO2 flux from soil to atmosphere (kg m-2 s-1).
  frac_vs(land_pts),                                                           &
    ! Fraction of gridbox covered by veg or soil.
  residual_n(land_pts,nz_soilc)
    ! Minimum-allowed (residual) inorganic N amount (kg m-2).

!-----------------------------------------------------------------------------
!end of header

!-----------------------------------------------------------------------------
! Get index for points with soil and/or vegetation.
!-----------------------------------------------------------------------------
CALL get_veg_soil_index( land_pts, frac_surft, vs_pts,                         &
                         vs_index, frac_vs )

!-----------------------------------------------------------------------------
! Loop over soil tiles.
! Note that many of these variables don't yet support soil tiling, so this
! loop is rather academic!
!-----------------------------------------------------------------------------
DO s = 1,nsoilt

  IF ( l_soil_N ) THEN

    !-------------------------------------------------------------------------
    ! Calculate stable N:C of biomass and humus pools.
    !-------------------------------------------------------------------------
    CALL stable_n_c( land_pts, vs_pts, vs_index, soil_pH_soilt(:,s,:),         &
                     biohum_nc )

    !------------------------------------------------------------------------
    ! Ensure that inorganic N pools are not smaller than the minimum allowed.
    ! This should only really be done during a "hard" (i.e. comprehensive)
    ! reconfiguration, but for now we will always do it.
    !------------------------------------------------------------------------
    ! Calculate the minimum-allowed amount of inorganic N.
    CALL get_residual_n( land_pts, vs_pts, vs_index, residual_n )

    DO i = 1,vs_pts
      l = vs_index(i)
      soilecosse%n_soil_pool_soilt(l,s,:,i_amm) =                              &
              MAX( soilecosse%n_soil_pool_soilt(l,s,:,i_amm), residual_n(l,:) )
      soilecosse%n_soil_pool_soilt(l,s,:,i_nit) =                              &
              MAX( soilecosse%n_soil_pool_soilt(l,s,:,i_nit), residual_n(l,:) )
    END DO

  END IF  !  l_soil_N

  ! Enforce minimum-allowed pool sizes (which also ensures C:N of biomass and
  ! humus pools). Again this should only really be done during a "hard"
  ! (i.e. comprehensive) reconfiguration but we will do it now to avoid
  ! issues such as zero-sized pools. The indices 1-4 here refer to the 4 pools
  ! of organic matter.
  co2_dummy(:) = 0.0
  CALL adjust_soil( land_pts, vs_pts, l_init_true, vs_index, biohum_nc,        &
                    cs_pool_soilt(:,s,:,1), cs_pool_soilt(:,s,:,2),            &
                    cs_pool_soilt(:,s,:,3), cs_pool_soilt(:,s,:,4),            &
                    soilecosse%n_soil_pool_soilt(:,s,:,1),                     &
                    soilecosse%n_soil_pool_soilt(:,s,:,2),                     &
                    soilecosse%n_soil_pool_soilt(:,s,:,3),                     &
                    soilecosse%n_soil_pool_soilt(:,s,:,4),                     &
                    co2_dummy,                                                 &
                ! These arguments replace USE statements
                    soilecosse%soil_c_add, soilecosse%soil_n_add )

END DO  !  soil tiles

END SUBROUTINE ecosse_init

!#############################################################################
!#############################################################################

END MODULE ecosse_init_mod
#endif
