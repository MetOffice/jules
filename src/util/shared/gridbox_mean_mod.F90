MODULE gridbox_mean_mod

! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

PRIVATE
PUBLIC surftiles_to_gbm, soiltiles_to_gbm, pfttiles_to_gbm,                    &
       masstiles_to_pfttiles

CONTAINS

FUNCTION surftiles_to_gbm(tile_data, ainfo, tile_mask) RESULT(gbm_data)

USE ancil_info, ONLY: land_pts, nsurft, ainfo_type

USE jules_surface_mod, ONLY: l_aggregate

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes data on land points * nsurft and calculates a gridbox mean value
!   for each land point. If mask is given, only the tiles for which mask
!   is true are included
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
REAL, INTENT(IN) :: tile_data(land_pts,nsurft)  ! Per tile data

TYPE(ainfo_type), INTENT(IN) :: ainfo

LOGICAL, INTENT(IN), OPTIONAL :: tile_mask(nsurft)
                                  ! T - include tile in calculation of gbm
                                  ! F - do not include tile in calculation

! Return type
REAL :: gbm_data(land_pts)

! Work variables
INTEGER :: i, p, t  ! Index variables

LOGICAL :: tile_mask_local(nsurft)  ! Local version of tile_mask that is
                                    ! always present

!-------------------------------------------------------------------------------
! Set mask.
IF (PRESENT( tile_mask )) THEN
  tile_mask_local(:) = tile_mask(:)
ELSE
  tile_mask_local(:) = .TRUE.
END IF

! Initialise the average.
gbm_data(:) = 0.0

IF ( l_aggregate ) THEN
  ! If l_aggregate is .TRUE., then all tile variables are essentially gridbox
  ! means already
  gbm_data(:) = tile_data(:,1)
ELSE
  ! Otherwise, we can just use ainfo%frac_surft, since nsurft=ntype
  DO t = 1,nsurft
    IF ( tile_mask_local(t) ) THEN
      DO i = 1,ainfo%surft_pts(t)
        p = ainfo%surft_index(i,t)
        gbm_data(p) = gbm_data(p) + ainfo%frac_surft(p,t) * tile_data(p,t)
      END DO
    END IF
  END DO
END IF

RETURN

END FUNCTION surftiles_to_gbm

!-------------------------------------------------------------------------------

FUNCTION soiltiles_to_gbm(tile_data, ainfo, tile_mask) RESULT(gbm_data)

USE ancil_info, ONLY: land_pts, nsoilt, ainfo_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes data on land points * nsoilt and calculates a gridbox mean value
!   for each land point. If mask is given, only the tiles for which mask
!   is true are included
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
REAL, INTENT(IN) :: tile_data(land_pts,nsoilt)  ! Per tile data

TYPE(ainfo_type), INTENT(IN) :: ainfo

LOGICAL, INTENT(IN), OPTIONAL :: tile_mask(nsoilt)
                                  ! T - include tile in calculation of gbm
                                  ! F - do not include tile in calculation

! Return type
REAL :: gbm_data(land_pts)

! Work variables
INTEGER :: i, p, t  ! Index variables

LOGICAL :: tile_mask_local(nsoilt)  ! Local version of tile_mask that is
                                    ! always present

!-------------------------------------------------------------------------------
! Set mask.
IF (PRESENT( tile_mask )) THEN
  tile_mask_local(:) = tile_mask(:)
ELSE
  tile_mask_local(:) = .TRUE.
END IF

! Initialise the average
gbm_data(:) = 0.0

! 1 soil tile is a common use case, so allow for a straight copy
IF ( nsoilt == 1) THEN
  gbm_data(:) = tile_data(:,1)
ELSE
  ! Otherwise add up the contributions
  DO t = 1,nsoilt
    IF ( tile_mask_local(t) ) THEN
      DO i = 1,ainfo%soilt_pts(t)
        p = ainfo%soilt_index(i,t)
        gbm_data(p) = gbm_data(p) + (ainfo%frac_soilt(p,t) * tile_data(p,t))
      END DO
    END IF
  END DO
END IF

RETURN
END FUNCTION soiltiles_to_gbm

!-----------------------------------------------------------------------------

FUNCTION pfttiles_to_gbm(tile_data,ainfo,tile_mask,frac_surft_in) RESULT(gbm_data)

USE ancil_info, ONLY: land_pts, ainfo_type

USE jules_surface_mod, ONLY: l_aggregate
USE jules_surface_types_mod,      ONLY: npft

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes data on land points * npft and calculates a gridbox mean value
!   for each land point. If mask is given, only the tiles for which mask
!   is true are included
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
REAL, INTENT(IN) :: tile_data(land_pts,npft)  ! Per tile data
REAL, INTENT(IN), OPTIONAL :: frac_surft_in(land_pts,npft)

TYPE(ainfo_type), INTENT(IN) :: ainfo

LOGICAL, INTENT(IN), OPTIONAL :: tile_mask(npft)
                                  ! T - include tile in calculation of gbm
                                  ! F - do not include tile in calculation

! Return type
REAL :: gbm_data(land_pts)

! Work variables
INTEGER :: i, p, t  ! Index variables

REAL :: frac_surft_local(land_pts,npft)
LOGICAL :: tile_mask_local(npft)  ! Local version of tile_mask that is
                                    ! always present

!-------------------------------------------------------------------------------
! Set mask.
IF (PRESENT( tile_mask )) THEN
  tile_mask_local(:) = tile_mask(:)
ELSE
  tile_mask_local(:) = .TRUE.
END IF

IF (PRESENT( frac_surft_in )) THEN
  frac_surft_local(:,:) = frac_surft_in(:,:)
ELSE
  frac_surft_local(:,:) = ainfo%frac_surft(:,:)
END IF

! Initialise the average.
gbm_data(:) = 0.0

IF ( l_aggregate ) THEN
  ! If l_aggregate is .TRUE., then all tile variables are essentially gridbox
  ! means already
  gbm_data(:) = tile_data(:,1)
ELSE
  ! Otherwise, we can just use ainfo%frac_surft, since npft is the first part of nsurft
  DO t = 1, npft
    IF ( tile_mask_local(t) ) THEN
      DO i = 1,ainfo%surft_pts(t)
        p = ainfo%surft_index(i,t)
        gbm_data(p) = gbm_data(p) + frac_surft_local(p,t) * tile_data(p,t)
      END DO
    END IF
  END DO
END IF

RETURN

END FUNCTION pfttiles_to_gbm

!-----------------------------------------------------------------------------

FUNCTION masstiles_to_pfttiles(mass_data, ainfo, plantNumDensity, crwn_area_mass, &
                                mass_mask) RESULT(pft_data)

USE ancil_info,              ONLY: land_pts, nmasst, ainfo_type

USE jules_surface_types_mod, ONLY: npft

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes data on land points * npft * nmasst and calculates a pft mean value
!   for each land point. If mask is given, only the tiles for which mask
!   is true are included
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(ainfo_type), INTENT(IN) :: ainfo

REAL, INTENT(IN)   ::                                                          &
mass_data(land_pts,npft,nmasst),                                               &
              !  Per mass tile data
plantNumDensity(land_pts,npft,nmasst),                                         &
              !  PFT number density across plant mass. (/m2)
crwn_area_mass(npft,nmasst)
              !  PFT crown area across plant mass. (m2)

LOGICAL, INTENT(IN), OPTIONAL :: mass_mask(nmasst)
                                  ! T - include tile in calculation of pft
                                  ! F - do not include tile in calculation

! Return type
REAL :: pft_data(land_pts,npft)

! Work variables
INTEGER :: l,p,n,k  ! Index variables

LOGICAL :: tile_mask_local(nmasst)  ! Local version of tile_mask that is
                                    ! always present

!-------------------------------------------------------------------------------
! Set mask.
IF (PRESENT( mass_mask )) THEN
  tile_mask_local(:) = mass_mask(:)
ELSE
  tile_mask_local(:) = .TRUE.
END IF

! Initialise the average.
pft_data(:,:) = 0.0

DO k = 1, nmasst
  IF ( tile_mask_local(k) ) THEN
    DO n = 1, npft
      DO l = 1, ainfo%surft_pts(n)
        p = ainfo%surft_index(l,n)
        pft_data(p,n) = pft_data(p,n) + plantNumDensity(p,n,k)                 &
        * crwn_area_mass(n,k) * mass_data(p,n,k)
      END DO
    END DO
  END IF
END DO

RETURN

END FUNCTION masstiles_to_pfttiles

END MODULE gridbox_mean_mod
