! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************
!
!  module of declarations and initialisation routines for JULES/UM

MODULE jules_vars_mod

  ! Description:
  !   Module containing declarations of JULES-related variables
  !   and initialisation subroutines.
  !
  ! Method:
  !   Module contains declarations of variables needed for
  !   implementation of JULES routines in the UM, and also
  !   subroutines required to initialise these variables.
  !
  ! Code Owner: Please refer to ModuleLeaders.txt
  !
  ! Code Description:
  !   Language: FORTRAN 90
  !
  ! Declarations:

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Implementation for field variables
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

!===============================================================================
TYPE :: jules_vars_data_type
  ! Contains LOGICALs, INTEGERs and REALs

  REAL(KIND=real_jlslsm), ALLOCATABLE :: snowdep_surft(:,:)
    ! Depth of snow (=snowdepth) for all surfaces except those using the
    ! snow canopy, for which it is the depth of snow in the canopy (m)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: albobs_scaling_surft(:,:,:)
    ! scaling factor applied to match observed albedo, within limits,
    ! for each tile, for SW radn or VIS and NIR
    ! Allocated size of last dimension depends on l_spec_albedo
    ! rad_nband is set in check_jules_radiation
    ! in jules_radiation_mod and stored in ancil_info

  REAL(KIND=real_jlslsm), ALLOCATABLE :: sthuf_soilt(:, :, :)
    ! stores sthu_soilt + sthf_soilt for standalone input/output
    ! Only used during initialisation for standalone JULES
    ! and under limited other circumstances
    ! Could potentially be replaced by local-scope variables

  REAL(KIND=real_jlslsm), ALLOCATABLE :: z_land_ij(:,:)
    ! Elevation of forcing data

  REAL(KIND=real_jlslsm), ALLOCATABLE :: z_land_land(:)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: surf_hgt_surft(:,:)
    ! Height of tile above mean gridbox surface (m)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: lw_down_elevcorr_surft(:,:)
    ! In JULES this is currently initialised as the same at every land point
    ! with one value for each tile
    ! So we create an array to use for IO which can hold a value for each
    ! tile

  REAL(KIND=real_jlslsm), ALLOCATABLE :: unload_backgrnd_pft(:,:)
    ! Background unloading rate for canopy (s-1)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: diff_frac(:)
    ! The fraction of radiation that is diffuse

  REAL(KIND=real_jlslsm), ALLOCATABLE :: dzl(:,:,:)
    ! Separation of boundary layer levels (m).
    ! The levels are listed starting at the surface and working up.
    ! Used in standalone JULES only

  REAL(KIND=real_jlslsm), ALLOCATABLE :: zh(:,:)
    ! Height above surface of top of boundary layer (m).
    ! Used in standalone JULES only

  REAL(KIND=real_jlslsm), ALLOCATABLE :: sil_orog_land_gb(:)
    ! Silhouette area of unresolved orography
    !   per unit horizontal area on land points only

  REAL(KIND=real_jlslsm), ALLOCATABLE :: ho2r2_orog_gb(:)
    ! Standard Deviation of orography
    ! equivalent to peak to trough height of unresolved orography

  REAL(KIND=real_jlslsm), ALLOCATABLE :: h_blend_orog_ij(:,:)
    ! Blending height used as part of effective roughness scheme

  REAL(KIND=real_jlslsm), ALLOCATABLE :: z0m_eff_ij(:,:)
    ! Effective grid-box roughness length for momentum

  REAL(KIND=real_jlslsm), ALLOCATABLE :: u_0_p_ij(:,:)
    ! W'ly component of surface current (m/s). P grid

  REAL(KIND=real_jlslsm), ALLOCATABLE :: v_0_p_ij(:,:)
    ! S'ly component of surface current (m/s). P grid

  REAL(KIND=real_jlslsm), ALLOCATABLE :: u_1_p_ij(:,:)
    ! U_1 on P-grid

  REAL(KIND=real_jlslsm), ALLOCATABLE :: v_1_p_ij(:,:)
    ! V_1 on P-grid

  REAL(KIND=real_jlslsm), ALLOCATABLE :: blendht(:,:)
    ! Blending height (m)

  REAL(KIND=real_jlslsm), ALLOCATABLE :: dtrdz_charney_grid_1_ij(:,:)
    ! -g.dt/dp for model layers

END TYPE

!===============================================================================
TYPE :: jules_vars_type
  ! Contains LOGICALs, INTEGERs and REALs

  REAL(KIND=real_jlslsm), POINTER :: snowdep_surft(:,:)
    ! Depth of snow (=snowdepth) for all surfaces except those using the
    ! snow canopy, for which it is the depth of snow in the canopy (m)

  REAL(KIND=real_jlslsm), POINTER :: albobs_scaling_surft(:,:,:)
    ! scaling factor applied to match observed albedo, within limits,
    ! for each tile, for SW radn or VIS and NIR
    ! Allocated size of last dimension depends on l_spec_albedo
    ! rad_nband is set in check_jules_radiation
    ! in jules_radiation_mod and stored in ancil_info

  REAL(KIND=real_jlslsm), POINTER :: sthuf_soilt(:, :, :)
    ! stores sthu_soilt + sthf_soilt for standalone input/output
    ! Only used during initialisation for standalone JULES
    ! and under limited other circumstances

  REAL(KIND=real_jlslsm), POINTER :: z_land_ij(:,:)
  ! Elevation of forcing data

  REAL(KIND=real_jlslsm), POINTER :: z_land_land(:)

  REAL(KIND=real_jlslsm), POINTER :: surf_hgt_surft(:,:)
    ! Height of tile above mean gridbox surface (m)

  REAL(KIND=real_jlslsm), POINTER :: lw_down_elevcorr_surft(:,:)
    ! In JULES this is currently initialised as the same at every land point
    ! with one value for each tile
    ! So we create an array to use for IO which can hold a value for each
    ! tile

  REAL(KIND=real_jlslsm), POINTER :: unload_backgrnd_pft(:,:)
    ! Background unloading rate for canopy (s-1)

  REAL(KIND=real_jlslsm), POINTER :: diff_frac(:)
    ! The fraction of radiation that is diffuse

  REAL(KIND=real_jlslsm), POINTER :: dzl(:,:,:)
    ! Separation of boundary layer levels (m).
    ! The levels are listed starting at the surface and working up.
    ! Used in standalone JULES only

  REAL(KIND=real_jlslsm), POINTER :: zh(:,:)
    ! Height above surface of top of boundary layer (m).
    ! Used in standalone JULES only

  REAL(KIND=real_jlslsm), POINTER :: sil_orog_land_gb(:)
    ! Silhouette area of unresolved orography
    !   per unit horizontal area on land points only

  REAL(KIND=real_jlslsm), POINTER :: ho2r2_orog_gb(:)
    ! Standard Deviation of orography
    ! equivalent to peak to trough height of unresolved orography

  REAL(KIND=real_jlslsm), POINTER :: h_blend_orog_ij(:,:)
    ! Blending height used as part of effective roughness scheme

  REAL(KIND=real_jlslsm), POINTER :: z0m_eff_ij(:,:)
    ! Effective grid-box roughness length for momentum

  REAL(KIND=real_jlslsm), POINTER :: u_0_p_ij(:,:)
    ! W'ly component of surface current (m/s). P grid

  REAL(KIND=real_jlslsm), POINTER :: v_0_p_ij(:,:)
    ! S'ly component of surface current (m/s). P grid

  REAL(KIND=real_jlslsm), POINTER :: u_1_p_ij(:,:)
    ! U_1 on P-grid

  REAL(KIND=real_jlslsm), POINTER :: v_1_p_ij(:,:)
    ! V_1 on P-grid

  REAL(KIND=real_jlslsm), POINTER :: blendht(:,:)
    ! Blending height (m)

  REAL(KIND=real_jlslsm), POINTER :: dtrdz_charney_grid_1_ij(:,:)
    ! -g.dt/dp for model layers

END TYPE

! MPI communicator for river coupled jobs
INTEGER :: mpi_local_comm

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_VARS_MOD'

CONTAINS

SUBROUTINE jules_vars_alloc(land_pts,ntype,nsurft,rad_nband,nsoilt,sm_levels,  &
                t_i_length, t_j_length, npft, bl_levels, pdims_s, pdims,       &
                l_albedo_obs, cansnowtile, l_deposition,                       &
                jules_vars_data)

USE atm_fields_bounds_mod, ONLY: array_dims

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: land_pts,ntype,nsurft,rad_nband,nsoilt,sm_levels,       &
                       t_i_length, t_j_length, npft, bl_levels

TYPE(array_dims), INTENT(IN) :: pdims_s, pdims

LOGICAL, INTENT(IN) :: l_albedo_obs, cansnowtile(npft), l_deposition

TYPE(jules_vars_data_type), INTENT(IN OUT) :: jules_vars_data

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='JULES_VARS_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)


ALLOCATE(jules_vars_data%snowdep_surft(land_pts,nsurft))
jules_vars_data%snowdep_surft(:,:)          = 0.0

IF ( l_albedo_obs ) THEN
  ALLOCATE(jules_vars_data%albobs_scaling_surft(land_pts,ntype,rad_nband))
  jules_vars_data%albobs_scaling_surft(:,:,:) = 0.0
ELSE
  ALLOCATE(jules_vars_data%albobs_scaling_surft(1,1,1))
END IF

ALLOCATE(jules_vars_data%sthuf_soilt(land_pts,nsoilt,sm_levels))
jules_vars_data%sthuf_soilt(:,:,:)  = 0.0

ALLOCATE(jules_vars_data%surf_hgt_surft(land_pts,nsurft))
ALLOCATE(jules_vars_data%lw_down_elevcorr_surft(land_pts,nsurft))
ALLOCATE(jules_vars_data%z_land_ij(t_i_length,t_j_length))
ALLOCATE(jules_vars_data%z_land_land(land_pts))
jules_vars_data%surf_hgt_surft(:,:)         = 0.0
jules_vars_data%lw_down_elevcorr_surft(:,:) = 0.0
jules_vars_data%z_land_ij(:,:)              = 0.0
jules_vars_data%z_land_land(:)              = 0.0

IF (ANY(cansnowtile(1:npft)) .EQV. .TRUE.) THEN
  ALLOCATE(jules_vars_data%unload_backgrnd_pft(land_pts,npft))
ELSE
  ALLOCATE(jules_vars_data%unload_backgrnd_pft(1,1))
END IF
jules_vars_data%unload_backgrnd_pft(:,:) = 0.0

ALLOCATE(jules_vars_data%diff_frac(t_i_length * t_j_length))
jules_vars_data%diff_frac(:) = 0.0

ALLOCATE(jules_vars_data%zh(t_i_length,t_j_length))
jules_vars_data%zh(:,:) = 0.0

IF ( l_deposition ) THEN
  ALLOCATE(jules_vars_data%dzl(t_i_length,t_j_length,bl_levels))
  jules_vars_data%dzl(:,:,:) = 0.0
ELSE
  ALLOCATE(jules_vars_data%dzl(1,1,1))
END IF

ALLOCATE(jules_vars_data%sil_orog_land_gb(land_pts))
ALLOCATE(jules_vars_data%ho2r2_orog_gb(land_pts))
ALLOCATE(jules_vars_data%h_blend_orog_ij(t_i_length,t_j_length))
ALLOCATE(jules_vars_data%z0m_eff_ij(t_i_length,t_j_length))
jules_vars_data%sil_orog_land_gb(:)  = 0.0
jules_vars_data%ho2r2_orog_gb(:)  = 0.0
jules_vars_data%h_blend_orog_ij(:,:)  = 0.0
jules_vars_data%z0m_eff_ij(:,:)  = 0.0

ALLOCATE(jules_vars_data%u_0_p_ij(pdims_s%i_start:pdims_s%i_end,               &
                                  pdims_s%j_start:pdims_s%j_end))
ALLOCATE(jules_vars_data%v_0_p_ij(pdims_s%i_start:pdims_s%i_end,               &
                                  pdims_s%j_start:pdims_s%j_end))
ALLOCATE(jules_vars_data%u_1_p_ij(pdims_s%i_start:pdims_s%i_end,               &
                                  pdims_s%j_start:pdims_s%j_end))
ALLOCATE(jules_vars_data%v_1_p_ij(pdims_s%i_start:pdims_s%i_end,               &
                                  pdims_s%j_start:pdims_s%j_end))
ALLOCATE(jules_vars_data%dtrdz_charney_grid_1_ij(pdims%i_start:pdims%i_end,    &
                                  pdims%j_start:pdims%j_end))
ALLOCATE(jules_vars_data%blendht(pdims%i_start:pdims%i_end,                    &
                                 pdims%j_start:pdims%j_end))

jules_vars_data%u_0_p_ij(:,:)    = 0.0
jules_vars_data%v_0_p_ij(:,:)    = 0.0
jules_vars_data%u_1_p_ij(:,:)    = 0.0
jules_vars_data%v_1_p_ij(:,:)    = 0.0
jules_vars_data%blendht(:,:)     = 0.0
jules_vars_data%dtrdz_charney_grid_1_ij(:,:) = 0.0

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE jules_vars_alloc


!===============================================================================
SUBROUTINE jules_vars_dealloc(jules_vars_data)

  !No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(jules_vars_data_type), INTENT(IN OUT) :: jules_vars_data

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='JULES_VARS_DEALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DEALLOCATE(jules_vars_data%snowdep_surft)
DEALLOCATE(jules_vars_data%albobs_scaling_surft)
DEALLOCATE(jules_vars_data%sthuf_soilt)
DEALLOCATE(jules_vars_data%surf_hgt_surft)
DEALLOCATE(jules_vars_data%lw_down_elevcorr_surft)
DEALLOCATE(jules_vars_data%z_land_ij)
DEALLOCATE(jules_vars_data%z_land_land)
DEALLOCATE(jules_vars_data%unload_backgrnd_pft)
DEALLOCATE(jules_vars_data%diff_frac)
DEALLOCATE(jules_vars_data%zh)
DEALLOCATE(jules_vars_data%dzl)
DEALLOCATE(jules_vars_data%sil_orog_land_gb)
DEALLOCATE(jules_vars_data%ho2r2_orog_gb)
DEALLOCATE(jules_vars_data%h_blend_orog_ij)
DEALLOCATE(jules_vars_data%z0m_eff_ij)
DEALLOCATE(jules_vars_data%u_0_p_ij)
DEALLOCATE(jules_vars_data%v_0_p_ij)
DEALLOCATE(jules_vars_data%u_1_p_ij)
DEALLOCATE(jules_vars_data%v_1_p_ij)
DEALLOCATE(jules_vars_data%blendht)
DEALLOCATE(jules_vars_data%dtrdz_charney_grid_1_ij)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE jules_vars_dealloc

!===============================================================================
SUBROUTINE jules_vars_assoc(jules_vars,jules_vars_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(jules_vars_data_type), TARGET, INTENT(IN OUT) :: jules_vars_data
! Instance of the data type we are associtating to
TYPE(jules_vars_type), INTENT(IN OUT) :: jules_vars
  ! Instance of the pointer type we are associating

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='JULES_VARS_ASSOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL jules_vars_nullify(jules_vars)

jules_vars%snowdep_surft => jules_vars_data%snowdep_surft
jules_vars%albobs_scaling_surft => jules_vars_data%albobs_scaling_surft
jules_vars%sthuf_soilt => jules_vars_data%sthuf_soilt
jules_vars%surf_hgt_surft => jules_vars_data%surf_hgt_surft
jules_vars%lw_down_elevcorr_surft => jules_vars_data%lw_down_elevcorr_surft
jules_vars%z_land_ij => jules_vars_data%z_land_ij
jules_vars%z_land_land => jules_vars_data%z_land_land
jules_vars%unload_backgrnd_pft => jules_vars_data%unload_backgrnd_pft
jules_vars%diff_frac => jules_vars_data%diff_frac
jules_vars%zh => jules_vars_data%zh
jules_vars%dzl => jules_vars_data%dzl
jules_vars%sil_orog_land_gb => jules_vars_data%sil_orog_land_gb
jules_vars%ho2r2_orog_gb => jules_vars_data%ho2r2_orog_gb
jules_vars%h_blend_orog_ij => jules_vars_data%h_blend_orog_ij
jules_vars%z0m_eff_ij => jules_vars_data%z0m_eff_ij
jules_vars%u_0_p_ij => jules_vars_data%u_0_p_ij
jules_vars%v_0_p_ij => jules_vars_data%v_0_p_ij
jules_vars%u_1_p_ij => jules_vars_data%u_1_p_ij
jules_vars%v_1_p_ij => jules_vars_data%v_1_p_ij
jules_vars%blendht  => jules_vars_data%blendht
jules_vars%dtrdz_charney_grid_1_ij => jules_vars_data%dtrdz_charney_grid_1_ij

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE jules_vars_assoc

!===============================================================================
SUBROUTINE jules_vars_nullify(jules_vars)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(jules_vars_type), INTENT(IN OUT) :: jules_vars
  ! Instance of the pointer type we are associating

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='JULES_VARS_NULLIFY'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

NULLIFY(jules_vars%snowdep_surft)
NULLIFY(jules_vars%albobs_scaling_surft)
NULLIFY(jules_vars%sthuf_soilt)
NULLIFY(jules_vars%surf_hgt_surft)
NULLIFY(jules_vars%lw_down_elevcorr_surft)
NULLIFY(jules_vars%z_land_ij)
NULLIFY(jules_vars%z_land_land)
NULLIFY(jules_vars%unload_backgrnd_pft)
NULLIFY(jules_vars%diff_frac)
NULLIFY(jules_vars%zh)
NULLIFY(jules_vars%dzl)
NULLIFY(jules_vars%sil_orog_land_gb)
NULLIFY(jules_vars%ho2r2_orog_gb)
NULLIFY(jules_vars%h_blend_orog_ij)
NULLIFY(jules_vars%z0m_eff_ij)
NULLIFY(jules_vars%u_0_p_ij)
NULLIFY(jules_vars%v_0_p_ij)
NULLIFY(jules_vars%u_1_p_ij)
NULLIFY(jules_vars%v_1_p_ij)
NULLIFY(jules_vars%blendht)
NULLIFY(jules_vars%dtrdz_charney_grid_1_ij)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE jules_vars_nullify


END MODULE jules_vars_mod

