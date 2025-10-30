! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module containing size and dimension parameters, indexing variables
! ...and more.
!
! Most of these are not required in the UM implementation
!

MODULE ancil_info

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.

INTEGER ::                                                                     &
  land_pts = 0                                                                 &
                    !  No. of land points
  ,ssi_pts                                                                     &
                    !  Number of sea or sea-ice points
                    !  NOTE: Currently, this is set to the total number
                    !        of grid boxes
                    !        This probably needs to be looked at in
                    !        the future...
 ,sea_pts                                                                      &
                    !  Number of sea points
 ,sice_pts                                                                     &
                    !  Number of sea-ice points
 ,nsoilt = 1                                                                   &
                    !  Number of soil tiles, defaulting to 1
 ,nmasst = 10                                                                  &
                    ! Number of vegetation mass classes, defaulting to 10
 ,rad_nband = 1
                    !Number of radiation bands

! Stencil sizes of i and j index of an array. These are set in "coastal"
INTEGER :: stencil_i
INTEGER :: stencil_j

!-----------------------------------------------------------------------
! If we are not in UM, define everything else that is needed
!-----------------------------------------------------------------------

! The following block declares variables that are being removed
! from the UM. They have not been removed from here because this
! code is never used in the UM. When the standalone code is
! supplied with an equivalent to atm_fields_bounds_mod, they
! may be mapped to the variables in that equivalent module.

INTEGER ::                                                                     &
  halo_i = 0                                                                   &
                    !  Size of halo in i direction
 ,halo_j = 0                                                                   &
                    !  Size of halo in j direction
 ,n_rows                                                                       &
                    !  Number of rows in a v field
 ,off_x = 0                                                                    &
                    !  Size of small halo in i
 ,off_y = 0                                                                    &
                    !  Size of small halo in j
 ,co2_dim_len                                                                  &
                    !  Length of a CO2 field row
 ,co2_dim_row                                                                  &
                    !  Number of CO2 field rows
 ,lice_pts                                                                     &
                    !  Number of land ice points
 ,nsurft                                                                       &
                    !  Number of surface tiles
 ,soil_pts                                                                     &
                    !  Number of soil points
 ,dim_cs1                                                                      &
                    !  size of pool dimension in soil carbon (cs)
                    !  and related respiration variables
 ,dim_cslayer = 1                                                              &
                    !  size of depth dimension in soil carbon (cs),
                    !  nitrogen and related respiration variables
                    !  Initialised to 1 to ensure that configuations that
                    !  do not use science connected with it are not broken
                    !  by zero-sized array dimensions
 ,dim_soil_n_pool                                                              &
                    !  Number of soil N pools. (Only used with ECOSSE.)
                    !  Pools 1 to dim_cs1 are organic pools, matching the soil
                    !  C (organic) pools.
 ,row_length                                                                   &
                    !  Number of points on a row
 ,rows              !  Number of rows in a theta field

! Implementation:
! Each variable is declared in both the 'data' TYPE and the 'pointer' type.
! Instances of these types are declared at at high level as required
! This is to facilitate advanced memory management features, which are generally
! not visible in the science code.
! Checklist for adding a new variable:
! -add to data_type
! -add to pointer_type
! -add to the allocate routine, passing in any new dimension sizes required
!  by argument (not via USE statement)
! -add to the deallocate routine
! -add to the assoc and nullify routines

TYPE :: ainfo_data_type
  LOGICAL, ALLOCATABLE :: l_lice_surft(:)
    !  TRUE if a land ice (surface) tile, FALSE otherwise
  LOGICAL, ALLOCATABLE :: land_mask(:,:)
   !  T if land, F elsewhere
  LOGICAL, ALLOCATABLE :: l_soil_point(:)
   !  TRUE if a soil point, FALSE otherwise
  LOGICAL, ALLOCATABLE ::  l_lice_point(:)
   !  TRUE if a land ice point, FALSE otherwise.
   !  Used as a test for ice points, replacing testing sm_sat <= 0.0, which
   !  will not work (or be rather inflexible) with tiled soils.
  LOGICAL, ALLOCATABLE :: ocn_cpl_point(:,:)
   !  T if coupled to ocean, F elsewhere
  INTEGER, ALLOCATABLE :: soilt_pts(:)
    !  Number of points for each soil tile
  INTEGER, ALLOCATABLE :: surft_pts(:)
    !  Number of land points which include the nth surface type
  INTEGER, ALLOCATABLE :: sice_pts_ncat(:)
    !  Number of points for each sea-ice category
  INTEGER, ALLOCATABLE ::land_index(:)
    !  index of land points
  INTEGER, ALLOCATABLE :: ssi_index(:)
    !  index of sea and sea-ice points
  INTEGER, ALLOCATABLE :: sea_index(:)
    !  index of sea points
  INTEGER, ALLOCATABLE :: sice_index(:)
    !  index of sea-ice points
  INTEGER, ALLOCATABLE :: sice_index_ncat(:,:)
    !  index of points for each sea-ice category
  INTEGER, ALLOCATABLE :: soilt_index(:,:)
    !  Number of points that include the nth soil tile
  INTEGER, ALLOCATABLE :: surft_index(:,:)
    !  indices of land points which include the nth surface type
  INTEGER, ALLOCATABLE :: soil_index(:)
    !  index of soil points (i.e. land point number for each soil point)
  INTEGER, ALLOCATABLE :: lice_index(:)
    !  index of land ice points (i.e. land point number for each land ice point)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: fssi_ij(:,:)
    !  Fraction of gridbox covered by sea or sea-ice
  REAL(KIND=real_jlslsm), ALLOCATABLE :: sea_frac(:)
    !  Fraction of gridbox covered by sea (converted to single vector array)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: sice_frac(:)
    !  Fraction of gridbox covered by sea-ice (converted to single vector array)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: sice_frac_ncat(:,:)
    !  Fraction of gridbox covered by each sea-ice category
    !  (converted to single vector array)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: frac_soilt(:,:)
    !  Fraction of gridbox for each soil tile
  REAL(KIND=real_jlslsm), ALLOCATABLE :: frac_surft(:,:)
    !  fractional cover of each surface type
  REAL(KIND=real_jlslsm), ALLOCATABLE :: z1_tq_ij(:,:)
    !  height of temperature data
  REAL(KIND=real_jlslsm), ALLOCATABLE :: z1_uv_ij(:,:)
    !  height of wind data
  REAL(KIND=real_jlslsm), ALLOCATABLE :: ice_fract_ij(:,:)
    !  fraction of gridbox covered by sea-ice (decimal fraction)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: ice_fract_ncat_sicat(:,:,:)
    !  fraction of gridbox covered by sea-ice on catagories
  REAL(KIND=real_jlslsm), ALLOCATABLE :: ti_cat_sicat(:,:,:)
    ! sea ice surface temperature on categories
  REAL(KIND=real_jlslsm), ALLOCATABLE :: pond_frac_cat_sicat(:,:,:)
    ! Meltpond fraction on sea ice categories
  REAL(KIND=real_jlslsm), ALLOCATABLE :: pond_depth_cat_sicat(:,:,:)
    ! Meltpond depth on sea ice categories (m)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: sstfrz_ij(:,:)
    ! Salinity-dependent sea surface freezing temperature (K)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: latitude(:,:)
    ! latitude (degree)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: longitude(:,:)
    ! longitude (degree)
    ! hard-coded to 0 if l_local_solar_time=T to get local diagnostics
END TYPE

!================================

TYPE :: ainfo_type
  LOGICAL, POINTER :: l_lice_surft(:)
  LOGICAL, POINTER :: land_mask(:,:)
  LOGICAL, POINTER :: l_soil_point(:)
  LOGICAL, POINTER :: l_lice_point(:)
  LOGICAL, POINTER :: ocn_cpl_point(:,:)
  INTEGER, POINTER :: soilt_pts(:)
  INTEGER, POINTER :: surft_pts(:)
  INTEGER, POINTER :: sice_pts_ncat(:)
  INTEGER, POINTER :: land_index(:)
  INTEGER, POINTER :: ssi_index(:)
  INTEGER, POINTER :: sea_index(:)
  INTEGER, POINTER :: sice_index(:)
  INTEGER, POINTER :: sice_index_ncat(:,:)
  INTEGER, POINTER :: soilt_index(:,:)
  INTEGER, POINTER :: surft_index(:,:)
  INTEGER, POINTER :: soil_index(:)
  INTEGER, POINTER :: lice_index(:)

  REAL(KIND=real_jlslsm), POINTER :: fssi_ij(:,:)
  REAL(KIND=real_jlslsm), POINTER :: sea_frac(:)
  REAL(KIND=real_jlslsm), POINTER :: sice_frac(:)
  REAL(KIND=real_jlslsm), POINTER :: sice_frac_ncat(:,:)
  REAL(KIND=real_jlslsm), POINTER :: frac_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: frac_surft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: z1_tq_ij(:,:)
  REAL(KIND=real_jlslsm), POINTER :: z1_uv_ij(:,:)
  REAL(KIND=real_jlslsm), POINTER :: ice_fract_ij(:,:)
  REAL(KIND=real_jlslsm), POINTER :: ice_fract_ncat_sicat(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: ti_cat_sicat(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: pond_frac_cat_sicat(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: pond_depth_cat_sicat(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: sstfrz_ij(:,:)
  REAL(KIND=real_jlslsm), POINTER :: latitude(:,:)
  REAL(KIND=real_jlslsm), POINTER :: longitude(:,:)
END TYPE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='ANCIL_INFO'

CONTAINS

SUBROUTINE ancil_info_alloc(land_pts,t_i_length,t_j_length,                    &
                      nice,nsoilt,ntype,                                       &
                      ainfo_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: land_pts,t_i_length,t_j_length,                         &
                       nice,nsoilt,ntype

TYPE(ainfo_data_type), INTENT(IN OUT) :: ainfo_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='ANCIL_INFO_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

ALLOCATE(ainfo_data%l_lice_surft(ntype))
ALLOCATE(ainfo_data%soilt_pts(nsoilt))
ALLOCATE(ainfo_data%surft_pts(ntype))
ALLOCATE(ainfo_data%sice_pts_ncat(nice))
ALLOCATE(ainfo_data%ssi_index(t_i_length * t_j_length))
ALLOCATE(ainfo_data%sea_index(t_i_length * t_j_length))
ALLOCATE(ainfo_data%sice_index(t_i_length * t_j_length))
ALLOCATE(ainfo_data%fssi_ij(t_i_length,t_j_length))
ALLOCATE(ainfo_data%sea_frac(t_i_length * t_j_length))
ALLOCATE(ainfo_data%sice_frac(t_i_length * t_j_length))
ALLOCATE(ainfo_data%sice_frac_ncat(t_i_length * t_j_length,nice))
ALLOCATE(ainfo_data%sice_index_ncat(t_i_length * t_j_length,nice))
ALLOCATE(ainfo_data%l_lice_point(land_pts))
ALLOCATE(ainfo_data%l_soil_point(land_pts))
ALLOCATE(ainfo_data%soilt_index(land_pts,nsoilt))
ALLOCATE(ainfo_data%frac_soilt(land_pts,nsoilt))
ALLOCATE(ainfo_data%land_mask(t_i_length,t_j_length))
ALLOCATE(ainfo_data%ocn_cpl_point(t_i_length,t_j_length))
ALLOCATE(ainfo_data%land_index(land_pts))
ALLOCATE(ainfo_data%surft_index(land_pts,ntype))
ALLOCATE(ainfo_data%soil_index(land_pts))
ALLOCATE(ainfo_data%lice_index(land_pts))
ALLOCATE(ainfo_data%ice_fract_ij(t_i_length,t_j_length))
ALLOCATE(ainfo_data%ice_fract_ncat_sicat(t_i_length,t_j_length,nice))
ALLOCATE(ainfo_data%ti_cat_sicat(t_i_length,t_j_length,nice))
ALLOCATE(ainfo_data%pond_frac_cat_sicat(t_i_length,t_j_length,nice))
ALLOCATE(ainfo_data%pond_depth_cat_sicat(t_i_length,t_j_length,nice))
ALLOCATE(ainfo_data%sstfrz_ij(t_i_length,t_j_length))
ALLOCATE(ainfo_data%z1_uv_ij(t_i_length,t_j_length))
ALLOCATE(ainfo_data%z1_tq_ij(t_i_length,t_j_length))
ALLOCATE(ainfo_data%frac_surft(land_pts,ntype))
ALLOCATE(ainfo_data%latitude(t_i_length,t_j_length))
ALLOCATE(ainfo_data%longitude(t_i_length,t_j_length))

ainfo_data%l_lice_surft(:)             = .FALSE.
ainfo_data%soilt_pts(:)                = 0
ainfo_data%surft_pts(:)                = 0
ainfo_data%sice_pts_ncat(:)            = 0
ainfo_data%ssi_index(:)                = 0
ainfo_data%sea_index(:)                = 0
ainfo_data%sice_index(:)               = 0
ainfo_data%fssi_ij(:,:)                = 0.0
ainfo_data%sea_frac(:)                 = 0.0
ainfo_data%sice_frac(:)                = 0.0
ainfo_data%sice_frac_ncat(:,:)         = 0.0
ainfo_data%sice_index_ncat(:,:)        = 0
ainfo_data%l_lice_point(:)             = .FALSE.
ainfo_data%l_soil_point(:)             = .FALSE.
ainfo_data%soilt_index(:,:)            = 0
ainfo_data%frac_soilt(:,:)             = 0.0
ainfo_data%surft_index(:,:)            = 0
ainfo_data%soil_index(:)               = 0
ainfo_data%lice_index(:)               = 0
ainfo_data%ice_fract_ij(:,:)           = 0.0
ainfo_data%ice_fract_ncat_sicat(:,:,:) = 0.0
ainfo_data%ti_cat_sicat(:,:,:)         = 0.0
ainfo_data%pond_frac_cat_sicat(:,:,:)  = 0.0
ainfo_data%pond_depth_cat_sicat(:,:,:) = 0.0
ainfo_data%sstfrz_ij(:,:)              = 0.0
ainfo_data%z1_uv_ij(:,:)               = 0.0
ainfo_data%z1_tq_ij(:,:)               = 0.0
ainfo_data%frac_surft(:,:)             = 0.0
ainfo_data%latitude(:,:)               = 0.0
ainfo_data%longitude(:,:)              = 0.0

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE ancil_info_alloc

!===============================================================================
SUBROUTINE ancil_info_dealloc(ainfo_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
TYPE(ainfo_data_type), INTENT(IN OUT) :: ainfo_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='ANCIL_INFO_DEALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!Allocatables held in the TYPE
DEALLOCATE(ainfo_data%l_lice_surft)
DEALLOCATE(ainfo_data%soilt_pts)
DEALLOCATE(ainfo_data%surft_pts)
DEALLOCATE(ainfo_data%sice_pts_ncat)
DEALLOCATE(ainfo_data%ssi_index)
DEALLOCATE(ainfo_data%sea_index)
DEALLOCATE(ainfo_data%sice_index)
DEALLOCATE(ainfo_data%fssi_ij)
DEALLOCATE(ainfo_data%sea_frac)
DEALLOCATE(ainfo_data%sice_frac)
DEALLOCATE(ainfo_data%sice_frac_ncat)
DEALLOCATE(ainfo_data%sice_index_ncat)
DEALLOCATE(ainfo_data%l_lice_point)
DEALLOCATE(ainfo_data%l_soil_point)
DEALLOCATE(ainfo_data%soilt_index)
DEALLOCATE(ainfo_data%frac_soilt)
DEALLOCATE(ainfo_data%land_mask)
DEALLOCATE(ainfo_data%ocn_cpl_point)
DEALLOCATE(ainfo_data%land_index)
DEALLOCATE(ainfo_data%surft_index)
DEALLOCATE(ainfo_data%soil_index)
DEALLOCATE(ainfo_data%lice_index)
DEALLOCATE(ainfo_data%ice_fract_ij)
DEALLOCATE(ainfo_data%ice_fract_ncat_sicat)
DEALLOCATE(ainfo_data%ti_cat_sicat)
DEALLOCATE(ainfo_data%pond_frac_cat_sicat)
DEALLOCATE(ainfo_data%pond_depth_cat_sicat)
DEALLOCATE(ainfo_data%sstfrz_ij)
DEALLOCATE(ainfo_data%z1_uv_ij)
DEALLOCATE(ainfo_data%z1_tq_ij)
DEALLOCATE(ainfo_data%frac_surft)
DEALLOCATE(ainfo_data%latitude)
DEALLOCATE(ainfo_data%longitude)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
END SUBROUTINE ancil_info_dealloc

!==============================================================================
SUBROUTINE ancil_info_assoc(ainfo,ainfo_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

TYPE(ainfo_type), INTENT(IN OUT) :: ainfo
  !Instance of the pointer type we are associating

TYPE(ainfo_data_type), INTENT(IN OUT), TARGET :: ainfo_data
  !Instance of the data type we are associtating to

!Local variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='ANCIL_INFO_ASSOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL ancil_info_nullify(ainfo)

ainfo%l_lice_surft => ainfo_data%l_lice_surft
ainfo%soilt_pts => ainfo_data%soilt_pts
ainfo%surft_pts => ainfo_data%surft_pts
ainfo%sice_pts_ncat => ainfo_data%sice_pts_ncat
ainfo%ssi_index => ainfo_data%ssi_index
ainfo%sea_index => ainfo_data%sea_index
ainfo%sice_index => ainfo_data%sice_index
ainfo%fssi_ij => ainfo_data%fssi_ij
ainfo%sea_frac => ainfo_data%sea_frac
ainfo%sice_frac => ainfo_data%sice_frac
ainfo%sice_frac_ncat => ainfo_data%sice_frac_ncat
ainfo%sice_index_ncat => ainfo_data%sice_index_ncat
ainfo%l_lice_point => ainfo_data%l_lice_point
ainfo%l_soil_point => ainfo_data%l_soil_point
ainfo%soilt_index => ainfo_data%soilt_index
ainfo%frac_soilt => ainfo_data%frac_soilt
ainfo%land_mask => ainfo_data%land_mask
ainfo%ocn_cpl_point => ainfo_data%ocn_cpl_point
ainfo%land_index => ainfo_data%land_index
ainfo%surft_index => ainfo_data%surft_index
ainfo%soil_index => ainfo_data%soil_index
ainfo%lice_index => ainfo_data%lice_index
ainfo%ice_fract_ij => ainfo_data%ice_fract_ij
ainfo%ice_fract_ncat_sicat => ainfo_data%ice_fract_ncat_sicat
ainfo%ti_cat_sicat => ainfo_data%ti_cat_sicat
ainfo%pond_frac_cat_sicat => ainfo_data%pond_frac_cat_sicat
ainfo%pond_depth_cat_sicat => ainfo_data%pond_depth_cat_sicat
ainfo%sstfrz_ij => ainfo_data%sstfrz_ij
ainfo%z1_uv_ij => ainfo_data%z1_uv_ij
ainfo%z1_tq_ij => ainfo_data%z1_tq_ij
ainfo%frac_surft => ainfo_data%frac_surft
ainfo%latitude => ainfo_data%latitude
ainfo%longitude => ainfo_data%longitude

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE ancil_info_assoc

!==============================================================================

SUBROUTINE ancil_info_nullify(ainfo)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

TYPE(ainfo_type), INTENT(IN OUT) :: ainfo
  !Instance of the pointer type we are nullifying

!Local variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='ANCIL_INFO_NULLIFY'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

NULLIFY(ainfo%l_lice_surft)
NULLIFY(ainfo%soilt_pts)
NULLIFY(ainfo%surft_pts)
NULLIFY(ainfo%sice_pts_ncat)
NULLIFY(ainfo%ssi_index)
NULLIFY(ainfo%sea_index)
NULLIFY(ainfo%sice_index)
NULLIFY(ainfo%fssi_ij)
NULLIFY(ainfo%sea_frac)
NULLIFY(ainfo%sice_frac)
NULLIFY(ainfo%sice_frac_ncat)
NULLIFY(ainfo%sice_index_ncat)
NULLIFY(ainfo%l_lice_point)
NULLIFY(ainfo%l_soil_point)
NULLIFY(ainfo%soilt_index)
NULLIFY(ainfo%frac_soilt)
NULLIFY(ainfo%land_mask)
NULLIFY(ainfo%ocn_cpl_point)
NULLIFY(ainfo%land_index)
NULLIFY(ainfo%surft_index)
NULLIFY(ainfo%soil_index)
NULLIFY(ainfo%lice_index)
NULLIFY(ainfo%ice_fract_ij)
NULLIFY(ainfo%ice_fract_ncat_sicat)
NULLIFY(ainfo%ti_cat_sicat)
NULLIFY(ainfo%pond_frac_cat_sicat)
NULLIFY(ainfo%pond_depth_cat_sicat)
NULLIFY(ainfo%sstfrz_ij)
NULLIFY(ainfo%z1_uv_ij)
NULLIFY(ainfo%z1_tq_ij)
NULLIFY(ainfo%frac_surft)
NULLIFY(ainfo%latitude)
NULLIFY(ainfo%longitude)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE ancil_info_nullify

END MODULE ancil_info
