#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE total_snow_init_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE total_snow_init(ainfo,progs)

USE layersnow_mod,   ONLY: layersnow

USE ancil_info, ONLY: land_pts, nsurft, lice_pts, nsoilt

USE jules_snow_mod, ONLY: nsmax, rho_snow_const, rho_snow_fresh, canSnowTile

USE jules_surface_mod, ONLY: l_elev_land_ice

USE jules_radiation_mod, ONLY: l_snow_albedo, l_embedded_snow

USE um_types, ONLY: real_jlslsm

!JULES TYPEs
USE ancil_info,    ONLY: ainfo_type
USE prognostics,   ONLY: progs_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Sets up the initial snow conditions from snow_surft only
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Arguments

!JULES TYPEs
TYPE(ainfo_type), INTENT(IN OUT) :: ainfo
TYPE(progs_type), INTENT(IN OUT) :: progs

! Work variables
INTEGER :: i, j, k, n, m  ! Index variables

REAL(KIND=real_jlslsm) :: snow_on_ground(land_pts,nsurft)
              ! Snow considered to be on the ground (not in canopy) (kg m-2)
              ! This is all the snow

!-------------------------------------------------------------------------------
! Put all snow onto the ground and zero canopy snow.
! Currently all snow is held in snow_surft.
! For can_model=4 tiles, put snow into snow_grnd_surft and zero snow_surft.
!-------------------------------------------------------------------------------

! Save input value.
snow_on_ground(:,:) = progs%snow_surft(:,:)

! Initialise stores to zero.
progs%snow_grnd_surft(:,:) = 0.0
progs%snow_surft(:,:) = 0.0

! Initialise other variables with values that will be retained where there is
! no tile - using "sensible" values for when these are printed.
progs%snowdepth_surft(:,:) = 0.0
IF ( nsmax < 1 ) THEN
  progs%rho_snow_grnd_surft(:,:) = rho_snow_const
ELSE
  progs%rho_snow_grnd_surft(:,:) = rho_snow_fresh
  progs%tsnow_surft(:,:,:) = 273.15
  progs%ds_surft(:,:,:) = 0.0
  IF ( l_snow_albedo .OR. l_embedded_snow ) progs%rgrainl_surft(:,:,:) = 0.0
END IF

DO n = 1,nsurft
  IF ( canSnowTile(n) ) THEN
    DO j = 1,ainfo%surft_pts(n)
      i = ainfo%surft_index(j,n)
      progs%snow_grnd_surft(i,n) = snow_on_ground(i,n)
    END DO
  ELSE
    DO j = 1,ainfo%surft_pts(n)
      i = ainfo%surft_index(j,n)
      progs%snow_surft(i,n) = snow_on_ground(i,n)
    END DO
  END IF
END DO

!-------------------------------------------------------------------------------
! Set snow density, calculate snow depth and set temperature of snow to equal
! that of soil.
!-------------------------------------------------------------------------------

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

DO n = 1,nsurft

  !Set the current soil tile (see notice above)
  IF (nsoilt == 1) THEN
    !There is only 1 soil tile
    m = 1
  ELSE ! nsoilt == nsurft
    !Soil tiles map directly on to surface tiles
    m = n
  END IF !nsoilt

  DO j = 1,ainfo%surft_pts(n)
    i = ainfo%surft_index(j,n)
    !     Use the constant (snowpack) density for nsmax=0 and if there is an
    !     existing pack. If nsmax>0 and there is no pack, initialise the density
    !     to the fresh snow value so that this value is used when/if a snowpack
    !     next develops.
    IF ( nsmax == 0 .OR.                                                       &
         ( snow_on_ground(i,n) > EPSILON(snow_on_ground) ) ) THEN
      progs%rho_snow_grnd_surft(i,n) = rho_snow_const
    ELSE
      progs%rho_snow_grnd_surft(i,n) = rho_snow_fresh
    END IF
    progs%snowdepth_surft(i,n) = snow_on_ground(i,n) /                         &
                                progs%rho_snow_grnd_surft(i,n)
    IF ( nsmax > 0 ) THEN

      progs%tsnow_surft(i,n,:) = progs%t_soil_soilt(i,m,1)
      IF ( l_snow_albedo .OR. l_embedded_snow )                                &
        progs%rgrainl_surft(i,n,:) = progs%rgrain_surft(i,n)
    END IF
  END DO
END DO

!==============================================================================
!**END NOTICE REGARDING SOIL TILING**
!==============================================================================

IF (l_elev_land_ice) THEN
  DO j = 1,lice_pts
    i = ainfo%lice_index(j)
    IF ( nsmax > 0 ) THEN
      DO n = 1,nsurft
        progs%tsnow_surft(i,n,:) = progs%tsurf_elev_surft(i,n)
      END DO
    END IF
  END DO
END IF

progs%nsnow_surft(:,:) = 0
IF ( nsmax > 0 ) THEN
  !-------------------------------------------------------------------------------
  ! Calculate snow layer thicknesses.
  !-------------------------------------------------------------------------------
  DO n = 1,nsurft
    CALL layersnow(land_pts, ainfo%surft_pts(n), ainfo%surft_index(:,n),       &
                   progs%snowdepth_surft(:,n), progs%nsnow_surft(:,n),         &
                   progs%ds_surft(:,n,:))
  END DO

  !-------------------------------------------------------------------------------
  ! Set layer frozen and liquid contents.
  !-------------------------------------------------------------------------------
  progs%sice_surft(:,:,:) = 0.0
  progs%sliq_surft(:,:,:) = 0.0
  DO n = 1,nsurft
    DO j = 1,ainfo%surft_pts(n)
      i = ainfo%surft_index(j,n)
      DO k = 1,progs%nsnow_surft(i,n)
        progs%sice_surft(i,n,k) = snow_on_ground(i,n) * progs%ds_surft(i,n,k)  &
                              / progs%snowdepth_surft(i,n)
      END DO
    END DO
  END DO
END IF

RETURN

END SUBROUTINE total_snow_init
END MODULE total_snow_init_mod
#endif
