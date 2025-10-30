! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!

! Module containing jules_chem diagnostics

MODULE jules_chemvars_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

TYPE :: chemvars_data_type
  ! Contains LOGICALs, INTEGERs and REALs
  ! BVOC diagnostics
  REAL(KIND=real_jlslsm), ALLOCATABLE :: isoprene_gb(:)
        ! Gridbox mean isoprene emission flux (kgC/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: isoprene_pft(:,:)
        ! Isoprene emission flux on PFTs (kgC/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: terpene_gb(:)
        ! Gridbox mean (mono-)terpene emission flux (kgC/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: terpene_pft(:,:)
        ! (Mono-)Terpene emission flux on PFTs (kgC/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: methanol_gb(:)
        ! Gridbox mean methanol emission flux (kgC/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: methanol_pft(:,:)
        ! Methanol emission flux on PFTs (kgC/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: acetone_gb(:)
        ! Gridbox mean acetone emission flux (kgC/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: acetone_pft(:,:)
        ! Acetone emission flux on PFTs (kgC/m2/s)
  ! Ozone forcing
  REAL(KIND=real_jlslsm), ALLOCATABLE :: o3_gb(:)
        ! Surface ozone concentration (ppb).
  ! Ozone diagnostics
  REAL(KIND=real_jlslsm), ALLOCATABLE :: flux_o3_pft(:,:)
        ! Flux of O3 to stomata (nmol O3/m2/s).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fo3_pft(:,:)
        ! Ozone exposure factor.
  ! Atmospheric deposition variables
  REAL(KIND=real_jlslsm), ALLOCATABLE :: gc_corr(:,:)
      ! Stomatal conductance without bare soil evaporation
  REAL(KIND=real_jlslsm), ALLOCATABLE :: dep_ftl_1_ij(:,:)
      ! Sensible heat flux (W m-2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: dep_sinlat_ij(:,:)
      ! Sine of latitude
  REAL(KIND=real_jlslsm), ALLOCATABLE :: dep_surfconc_ij(:,:,:)
      ! Concentration of chemical tracers in the atmosphere, for the
      ! calculation of deposition, as mass mixing ratio (kg kg-1).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: dep_ustar_ij(:,:)
      ! Friction velocity (m s-1)
  ! Atmospheric deposition diagnostics
  REAL(KIND=real_jlslsm), ALLOCATABLE :: dep_ra_ij(:,:,:)
      ! Aerodynamic resistance, Ra (s m-1)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: dep_rb_ij(:,:,:)
      ! Quasi-laminar resistance, Rb (s m-1)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: dep_rc_ij(:,:,:,:)
      ! Surface resistance, Rc (s m-1)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: dep_rc_stom_ij(:,:,:,:)
      ! Surface resistance, Rc (s m-1)
      ! stomatal component
  REAL(KIND=real_jlslsm), ALLOCATABLE :: dep_rc_nonstom_ij(:,:,:,:)
      ! Surface resistance, Rc (s m-1)
      ! non-stomatal component
  REAL(KIND=real_jlslsm), ALLOCATABLE :: dep_vd_ij(:,:,:,:)
      ! Surface deposition velocity term for trace gases (m s-1)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: dep_vd_gb(:,:,:)
      ! Surface deposition velocity term for trace gases (m s-1) on gridbox
  REAL(KIND=real_jlslsm), ALLOCATABLE :: dep_loss_rate_ij(:,:,:)
      ! First-order loss rate for trace gases (s-1)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: dep_flux_ij(:,:,:)
      ! Depostion flux for trace gases (kg m-3 s-1)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: dep_vd_so4_ij(:,:)
      ! Surface deposition velocity term for sulphate particles (m s-1)
  INTEGER,                ALLOCATABLE :: nlev_with_ddep(:,:)
      ! From UKCA, no of levels over which dry deposition acts
END TYPE

!===============================================================================
TYPE :: chemvars_type
  ! Contains LOGICALs, INTEGERs and REALs
  REAL(KIND=real_jlslsm), POINTER :: isoprene_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: isoprene_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: terpene_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: terpene_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: methanol_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: methanol_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: acetone_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: acetone_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: o3_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: flux_o3_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: fo3_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: gc_corr(:,:)
  REAL(KIND=real_jlslsm), POINTER :: dep_ftl_1_ij(:,:)
  REAL(KIND=real_jlslsm), POINTER :: dep_sinlat_ij(:,:)
  REAL(KIND=real_jlslsm), POINTER :: dep_surfconc_ij(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: dep_ustar_ij(:,:)
  REAL(KIND=real_jlslsm), POINTER :: dep_ra_ij(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: dep_rb_ij(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: dep_rc_ij(:,:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: dep_rc_stom_ij(:,:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: dep_rc_nonstom_ij(:,:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: dep_vd_ij(:,:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: dep_vd_gb(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: dep_loss_rate_ij(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: dep_flux_ij(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: dep_vd_so4_ij(:,:)
  INTEGER               , POINTER :: nlev_with_ddep(:,:)
END TYPE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_CHEMVARS_MOD'

CONTAINS

SUBROUTINE chemvars_alloc(land_pts, t_i_length, t_j_length, npft, ntype,       &
                          l_deposition, ndry_dep_species, chemvars_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: land_pts, t_j_length, t_i_length, npft, ntype,          &
                       ndry_dep_species
LOGICAL, INTENT(IN) :: l_deposition
TYPE(chemvars_data_type), INTENT(IN OUT) :: chemvars_data

!Local variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CHEMVARS_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)


! ==== from bvoc_vars module ====
ALLOCATE(chemvars_data%isoprene_gb(land_pts))
ALLOCATE(chemvars_data%isoprene_pft(land_pts,npft))
ALLOCATE(chemvars_data%terpene_gb(land_pts))
ALLOCATE(chemvars_data%terpene_pft(land_pts,npft))
ALLOCATE(chemvars_data%methanol_gb(land_pts))
ALLOCATE(chemvars_data%methanol_pft(land_pts,npft))
ALLOCATE(chemvars_data%acetone_gb(land_pts))
ALLOCATE(chemvars_data%acetone_pft(land_pts,npft))
! ==== from ozone_vars module ====
ALLOCATE(chemvars_data%o3_gb(land_pts))
ALLOCATE(chemvars_data%flux_o3_pft(land_pts,npft))
ALLOCATE(chemvars_data%fo3_pft(land_pts,npft))
!  ==== from bvoc_vars module ====
chemvars_data%isoprene_gb(:)             = 0.0
chemvars_data%isoprene_pft(:,:)          = 0.0
chemvars_data%terpene_gb(:)              = 0.0
chemvars_data%terpene_pft(:,:)           = 0.0
chemvars_data%methanol_gb(:)             = 0.0
chemvars_data%methanol_pft(:,:)          = 0.0
chemvars_data%acetone_gb(:)              = 0.0
chemvars_data%acetone_pft(:,:)           = 0.0
!           ==== from ozone_vars module ====
chemvars_data%o3_gb(:)                   = 0.0
chemvars_data%flux_o3_pft(:,:)           = 0.0
chemvars_data%fo3_pft(:,:)               = 0.0

! ==== for atmospheric deposition ====
ALLOCATE(chemvars_data%gc_corr(land_pts,npft))
ALLOCATE(chemvars_data%dep_ftl_1_ij(t_i_length,t_j_length))
ALLOCATE(chemvars_data%dep_sinlat_ij(t_i_length,t_j_length))
ALLOCATE(chemvars_data%dep_ustar_ij(t_i_length,t_j_length))

chemvars_data%gc_corr(:,:)               = 0.0
chemvars_data%dep_ftl_1_ij(:,:)          = 0.0
chemvars_data%dep_sinlat_ij(:,:)         = 0.0
chemvars_data%dep_ustar_ij(:,:)          = 0.0

IF (l_deposition) THEN
  ALLOCATE(chemvars_data%dep_surfconc_ij(t_i_length,t_j_length,                &
    ndry_dep_species))
  ALLOCATE(chemvars_data%nlev_with_ddep(t_i_length,t_j_length))
  ALLOCATE(chemvars_data%dep_ra_ij(t_i_length,t_j_length,ntype))
  ALLOCATE(chemvars_data%dep_rb_ij(t_i_length,t_j_length,ndry_dep_species))
  ALLOCATE(chemvars_data%dep_rc_ij(t_i_length,t_j_length,ntype,                &
    ndry_dep_species))
  ALLOCATE(chemvars_data%dep_rc_stom_ij(t_i_length,t_j_length,ntype,           &
    ndry_dep_species))
  ALLOCATE(chemvars_data%dep_rc_nonstom_ij(t_i_length,t_j_length,ntype,        &
    ndry_dep_species))
  ALLOCATE(chemvars_data%dep_vd_ij(t_i_length,t_j_length,ntype,                &
    ndry_dep_species))
  ALLOCATE(chemvars_data%dep_vd_gb(t_i_length,t_j_length,                      &
    ndry_dep_species))
  ALLOCATE(chemvars_data%dep_loss_rate_ij(t_i_length,t_j_length,               &
    ndry_dep_species))
  ALLOCATE(chemvars_data%dep_vd_so4_ij(t_i_length,t_j_length))
  ALLOCATE(chemvars_data%dep_flux_ij(t_i_length,t_j_length,ndry_dep_species))

  chemvars_data%nlev_with_ddep(:,:)        = 0
  chemvars_data%dep_ra_ij(:,:,:)           = 0.0
  chemvars_data%dep_rb_ij(:,:,:)           = 0.0
  chemvars_data%dep_rc_ij(:,:,:,:)         = 0.0
  chemvars_data%dep_rc_stom_ij(:,:,:,:)    = 0.0
  chemvars_data%dep_rc_nonstom_ij(:,:,:,:) = 0.0
  chemvars_data%dep_vd_ij(:,:,:,:)         = 0.0
  chemvars_data%dep_vd_gb(:,:,:)           = 0.0
  chemvars_data%dep_loss_rate_ij(:,:,:)    = 0.0
  chemvars_data%dep_vd_so4_ij(:,:)         = 0.0
  chemvars_data%dep_surfconc_ij(:,:,:)     = 0.0
  chemvars_data%dep_flux_ij(:,:,:)         = 0.0
END IF


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE chemvars_alloc


!===============================================================================
SUBROUTINE chemvars_dealloc(chemvars_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(chemvars_data_type), INTENT(IN OUT) :: chemvars_data

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CHEMVARS_DEALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DEALLOCATE(chemvars_data%isoprene_gb)
DEALLOCATE(chemvars_data%isoprene_pft)
DEALLOCATE(chemvars_data%terpene_gb)
DEALLOCATE(chemvars_data%terpene_pft)
DEALLOCATE(chemvars_data%methanol_gb)
DEALLOCATE(chemvars_data%methanol_pft)
DEALLOCATE(chemvars_data%acetone_gb)
DEALLOCATE(chemvars_data%acetone_pft)
DEALLOCATE(chemvars_data%o3_gb)
DEALLOCATE(chemvars_data%flux_o3_pft)
DEALLOCATE(chemvars_data%fo3_pft)

! ==== for atmospheric deposition ====
DEALLOCATE(chemvars_data%gc_corr)
DEALLOCATE(chemvars_data%dep_ftl_1_ij)
DEALLOCATE(chemvars_data%dep_sinlat_ij)
DEALLOCATE(chemvars_data%dep_ustar_ij)

! Following prognostics TYPE, use chemvars_data%dep_ra_ij
! to DEALLOCATE deposition diagnostics
IF ( ALLOCATED(chemvars_data%dep_ra_ij) ) THEN
  DEALLOCATE(chemvars_data%nlev_with_ddep)
  DEALLOCATE(chemvars_data%dep_ra_ij)
  DEALLOCATE(chemvars_data%dep_rb_ij)
  DEALLOCATE(chemvars_data%dep_rc_ij)
  DEALLOCATE(chemvars_data%dep_rc_stom_ij)
  DEALLOCATE(chemvars_data%dep_rc_nonstom_ij)
  DEALLOCATE(chemvars_data%dep_vd_ij)
  DEALLOCATE(chemvars_data%dep_vd_gb)
  DEALLOCATE(chemvars_data%dep_loss_rate_ij)
  DEALLOCATE(chemvars_data%dep_vd_so4_ij)
  DEALLOCATE(chemvars_data%dep_surfconc_ij)
  DEALLOCATE(chemvars_data%dep_flux_ij)
END IF


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE chemvars_dealloc

!===============================================================================
SUBROUTINE chemvars_assoc(chemvars,chemvars_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(chemvars_data_type), TARGET, INTENT(IN OUT) :: chemvars_data
! Instance of the data type we are associtating to
TYPE(chemvars_type), INTENT(IN OUT) :: chemvars
  ! Instance of the pointer type we are associating

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CHEMVARS_ASSOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL chemvars_nullify(chemvars)

chemvars%isoprene_gb       => chemvars_data%isoprene_gb
chemvars%isoprene_pft      => chemvars_data%isoprene_pft
chemvars%terpene_gb        => chemvars_data%terpene_gb
chemvars%terpene_pft       => chemvars_data%terpene_pft
chemvars%methanol_gb       => chemvars_data%methanol_gb
chemvars%methanol_pft      => chemvars_data%methanol_pft
chemvars%acetone_gb        => chemvars_data%acetone_gb
chemvars%acetone_pft       => chemvars_data%acetone_pft
chemvars%o3_gb             => chemvars_data%o3_gb
chemvars%flux_o3_pft       => chemvars_data%flux_o3_pft
chemvars%fo3_pft           => chemvars_data%fo3_pft

!      ==== for atmospheric deposition ====
chemvars%gc_corr           => chemvars_data%gc_corr
chemvars%dep_ftl_1_ij      => chemvars_data%dep_ftl_1_ij
chemvars%dep_sinlat_ij     => chemvars_data%dep_sinlat_ij
chemvars%dep_ustar_ij      => chemvars_data%dep_ustar_ij

! Following prognostics TYPE, use chemvars_data%dep_ra_ij
! to associate pointers to deposition diagnostics
IF ( ALLOCATED(chemvars_data%dep_ra_ij) ) THEN
  chemvars%nlev_with_ddep    => chemvars_data%nlev_with_ddep
  chemvars%dep_ra_ij         => chemvars_data%dep_ra_ij
  chemvars%dep_rb_ij         => chemvars_data%dep_rb_ij
  chemvars%dep_rc_ij         => chemvars_data%dep_rc_ij
  chemvars%dep_rc_stom_ij    => chemvars_data%dep_rc_stom_ij
  chemvars%dep_rc_nonstom_ij => chemvars_data%dep_rc_nonstom_ij
  chemvars%dep_vd_ij         => chemvars_data%dep_vd_ij
  chemvars%dep_vd_gb         => chemvars_data%dep_vd_gb
  chemvars%dep_loss_rate_ij  => chemvars_data%dep_loss_rate_ij
  chemvars%dep_vd_so4_ij     => chemvars_data%dep_vd_so4_ij
  chemvars%dep_surfconc_ij   => chemvars_data%dep_surfconc_ij
  chemvars%dep_flux_ij       => chemvars_data%dep_flux_ij
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE chemvars_assoc

!===============================================================================
SUBROUTINE chemvars_nullify(chemvars)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(chemvars_type), INTENT(IN OUT) :: chemvars
  ! Instance of the pointer type we are associating

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CHEMVARS_NULLIFY'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

NULLIFY(chemvars%isoprene_gb)
NULLIFY(chemvars%isoprene_pft)
NULLIFY(chemvars%terpene_gb)
NULLIFY(chemvars%terpene_pft)
NULLIFY(chemvars%methanol_gb)
NULLIFY(chemvars%methanol_pft)
NULLIFY(chemvars%acetone_gb)
NULLIFY(chemvars%acetone_pft)
NULLIFY(chemvars%o3_gb)
NULLIFY(chemvars%flux_o3_pft)
NULLIFY(chemvars%fo3_pft)

! ==== for atmospheric deposition ====
! Deposition diagnostics
NULLIFY(chemvars%nlev_with_ddep)
NULLIFY(chemvars%dep_flux_ij)
NULLIFY(chemvars%dep_ra_ij)
NULLIFY(chemvars%dep_rb_ij)
NULLIFY(chemvars%dep_rc_ij)
NULLIFY(chemvars%dep_rc_stom_ij)
NULLIFY(chemvars%dep_rc_nonstom_ij)
NULLIFY(chemvars%dep_vd_ij)
NULLIFY(chemvars%dep_vd_gb)
NULLIFY(chemvars%dep_loss_rate_ij)
NULLIFY(chemvars%dep_vd_so4_ij)
! Deposition variables
NULLIFY(chemvars%dep_surfconc_ij)

! Deposition variables
NULLIFY(chemvars%gc_corr)
NULLIFY(chemvars%dep_ftl_1_ij)
NULLIFY(chemvars%dep_sinlat_ij)
NULLIFY(chemvars%dep_ustar_ij)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN
END SUBROUTINE chemvars_nullify

END MODULE jules_chemvars_mod
