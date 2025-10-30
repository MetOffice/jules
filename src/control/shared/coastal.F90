! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Module containing variables for coastal tiling

MODULE coastal

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='COASTAL'

! flandg is allocated elsewhere for multiple purposes and so is not included in
! the type
REAL, DIMENSION(:,:), ALLOCATABLE :: flandg
! Land fraction on all tiles.

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
TYPE :: coastal_data_type
  ! Contains LOGICALs, INTEGERs and REALs

  REAL, ALLOCATABLE :: fland(:)
    ! Land fraction on land tiles
    ! For offline JULES, this might be better considered as a "0/1 flag to
    ! indicate which gridboxes are to be modelled", since we can use it to
    ! select land points from a larger set of land points.
    ! As long as JULES is not simulating sea points, FLAND and FLANDG
    ! (which is what's input) should be 0 or 1 (not intermediate values).
    ! Divided by 2SQRT(2) on land points only (m)
  REAL, ALLOCATABLE :: tstar_land_ij(:,:)
    ! Land mean surface temperature (K)
  REAL, ALLOCATABLE :: tstar_sea_ij(:,:)
    ! Open sea surface temperature (K)
  REAL, ALLOCATABLE :: tstar_sice_ij(:,:)
    ! Sea-ice surface temperature (K)
  REAL, ALLOCATABLE :: tstar_sice_sicat(:,:,:)
    ! Category sea-ice surface temperature (K)
  REAL, ALLOCATABLE :: tstar_ssi_ij(:,:)
    ! mean sea surface temperature (K)
  REAL, ALLOCATABLE :: taux_land_ij(:,:)
    ! W'ly component of sfc wind stress (N/sq m).
    !   (On U-grid with first and last rows undefined or,
    !    at present, set to missing data)
  REAL, ALLOCATABLE :: taux_ssi_ij(:,:)
    ! W'ly component of sfc wind stress (N/sq m).
    !   (On U-grid with first and last rows undefined or,
    !    at present, set to missing data)
  REAL, ALLOCATABLE :: tauy_land_ij(:,:)
    ! S'ly component of sfc wind stress (N/sq m).
    !   On V-grid; comments as per TAUX
  REAL, ALLOCATABLE :: tauy_ssi_ij(:,:)
    ! S'ly component of sfc wind stress (N/sq m).
    !   On V-grid; comments as per TAUX
  REAL, ALLOCATABLE :: vshr_land_ij(:,:)
    ! Magnitude of surface-to-lowest atm level wind shear (m per s)
  REAL, ALLOCATABLE :: vshr_ssi_ij(:,:)
    ! Magnitude of surface-to-lowest atm level wind shear (m per s)
  REAL, ALLOCATABLE :: surf_ht_flux_land_ij(:,:)
    ! Net downward heat flux at surface over land fraction of gridbox (W/m2)

  REAL, ALLOCATABLE ::                                                         &
    taux_land_star(:,:),                                                       &
    tauy_land_star(:,:),                                                       &
    taux_ssi_star(:,:),                                                        &
    tauy_ssi_star(:,:)
  ! Maps to the stencil points of each i and j index of an array
  INTEGER, ALLOCATABLE :: mapi(:,:)
  INTEGER, ALLOCATABLE :: mapj(:,:)
END TYPE

!===============================================================================
TYPE :: coastal_type
  ! Contains LOGICALs, INTEGERs and REALs

  REAL, POINTER :: fland(:)
  REAL, POINTER :: tstar_land_ij(:,:)
  REAL, POINTER :: tstar_sea_ij(:,:)
  REAL, POINTER :: tstar_sice_ij(:,:)
  REAL, POINTER :: tstar_sice_sicat(:,:,:)
  REAL, POINTER :: tstar_ssi_ij(:,:)
  REAL, POINTER :: taux_land_ij(:,:)
  REAL, POINTER :: taux_ssi_ij(:,:)
  REAL, POINTER :: tauy_land_ij(:,:)
  REAL, POINTER :: tauy_ssi_ij(:,:)
  REAL, POINTER :: vshr_land_ij(:,:)
  REAL, POINTER :: vshr_ssi_ij(:,:)
  REAL, POINTER :: surf_ht_flux_land_ij(:,:)

  REAL, POINTER ::                                                             &
    taux_land_star(:,:),                                                       &
    tauy_land_star(:,:),                                                       &
    taux_ssi_star(:,:),                                                        &
    tauy_ssi_star(:,:)

  INTEGER, POINTER :: mapi(:,:)
  INTEGER, POINTER :: mapj(:,:)
END TYPE

CONTAINS

SUBROUTINE coastal_alloc(land_pts,t_i_length,t_j_length,                       &
                         u_i_length,u_j_length,                                &
                         v_i_length,v_j_length,                                &
                         nice_use,nice, coastal_data,                          &
                         ! Optional arguments
                         stencil_i_in, stencil_j_in)

USE ancil_info, ONLY: stencil_i, stencil_j

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: land_pts,t_i_length,t_j_length,                         &
                       u_i_length,u_j_length,                                  &
                       v_i_length,v_j_length,                                  &
                       nice_use,nice
! Stencil size required for the mapi and mapj variables
INTEGER, INTENT(IN), OPTIONAL:: stencil_i_in, stencil_j_in
TYPE(coastal_data_type), INTENT(IN OUT) :: coastal_data

INTEGER :: i, j

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='COASTAL_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!flandg is allocated elsewhere and used for multiple purposes so cannot easily
!be relocated here

! Set the stencil size from input data if present, otherwise default to
! structured i,j mesh
IF (PRESENT(stencil_i_in)) THEN
  stencil_i = stencil_i_in
ELSE
  stencil_i = 3
END IF
IF (PRESENT(stencil_j_in)) THEN
  stencil_j = stencil_j_in
ELSE
  stencil_j = 3
END IF

ALLOCATE(coastal_data%fland(land_pts))
ALLOCATE(coastal_data%tstar_land_ij(t_i_length,t_j_length))
ALLOCATE(coastal_data%tstar_sea_ij(t_i_length,t_j_length))
ALLOCATE(coastal_data%tstar_sice_ij(t_i_length,t_j_length))
ALLOCATE(coastal_data%tstar_sice_sicat(t_i_length,t_j_length,nice_use))
ALLOCATE(coastal_data%tstar_ssi_ij(t_i_length,t_j_length))
ALLOCATE(coastal_data%taux_land_ij(u_i_length,u_j_length))
ALLOCATE(coastal_data%taux_ssi_ij(u_i_length,u_j_length))
ALLOCATE(coastal_data%tauy_land_ij(v_i_length,v_j_length))
ALLOCATE(coastal_data%tauy_ssi_ij(v_i_length,v_j_length))
ALLOCATE(coastal_data%vshr_land_ij(t_i_length,t_j_length))
ALLOCATE(coastal_data%vshr_ssi_ij(t_i_length,t_j_length))
ALLOCATE(coastal_data%surf_ht_flux_land_ij(t_i_length,t_j_length))
ALLOCATE(coastal_data%taux_land_star(u_i_length,u_j_length))
ALLOCATE(coastal_data%tauy_land_star(v_i_length,v_j_length))
ALLOCATE(coastal_data%taux_ssi_star(u_i_length,u_j_length))
ALLOCATE(coastal_data%tauy_ssi_star(v_i_length,v_j_length))
ALLOCATE(coastal_data%mapi(t_i_length,stencil_i))
ALLOCATE(coastal_data%mapj(t_j_length,stencil_j))
coastal_data%tstar_land_ij(:,:)             = 0.0
coastal_data%tstar_sea_ij(:,:)              = 0.0
coastal_data%tstar_sice_ij(:,:)             = 0.0
coastal_data%tstar_sice_sicat(:,:,:)        = 0.0
coastal_data%tstar_ssi_ij(:,:)              = 0.0
coastal_data%taux_land_ij(:,:)              = 0.0
coastal_data%taux_ssi_ij(:,:)               = 0.0
coastal_data%tauy_land_ij(:,:)              = 0.0
coastal_data%tauy_ssi_ij(:,:)               = 0.0
coastal_data%vshr_land_ij(:,:)              = 0.0
coastal_data%vshr_ssi_ij(:,:)               = 0.0
coastal_data%surf_ht_flux_land_ij(:,:)      = 0.0
coastal_data%taux_land_star(:,:)            = 0.0
coastal_data%tauy_land_star(:,:)            = 0.0
coastal_data%taux_ssi_star(:,:)             = 0.0
coastal_data%tauy_ssi_star(:,:)             = 0.0
! Set the stencil map for a structured i,j mesh if this is what has been
! requested
! Otherwise, the stencil map will need setting elsewhere in the driving
! model
IF (.NOT. PRESENT(stencil_i_in) .AND. .NOT. PRESENT(stencil_j_in)) THEN
  DO i = 1, t_i_length
    coastal_data%mapi(i,1) = i-1
    coastal_data%mapi(i,2) = i
    coastal_data%mapi(i,3) = i+1
  END DO
  DO j = 1, t_j_length
    coastal_data%mapj(j,1) = j-1
    coastal_data%mapj(j,2) = j
    coastal_data%mapj(j,3) = j+1
  END DO
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE coastal_alloc

!===============================================================================
SUBROUTINE coastal_dealloc(coastal_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(coastal_data_type), INTENT(IN OUT) :: coastal_data

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='COASTAL_DEALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DEALLOCATE(coastal_data%fland)
DEALLOCATE(coastal_data%tstar_land_ij)
DEALLOCATE(coastal_data%tstar_sea_ij)
DEALLOCATE(coastal_data%tstar_sice_ij)
DEALLOCATE(coastal_data%tstar_sice_sicat)
DEALLOCATE(coastal_data%tstar_ssi_ij)
DEALLOCATE(coastal_data%taux_land_ij)
DEALLOCATE(coastal_data%taux_ssi_ij)
DEALLOCATE(coastal_data%tauy_land_ij)
DEALLOCATE(coastal_data%tauy_ssi_ij)
DEALLOCATE(coastal_data%vshr_land_ij)
DEALLOCATE(coastal_data%vshr_ssi_ij)
DEALLOCATE(coastal_data%surf_ht_flux_land_ij)
DEALLOCATE(coastal_data%taux_land_star)
DEALLOCATE(coastal_data%tauy_land_star)
DEALLOCATE(coastal_data%taux_ssi_star)
DEALLOCATE(coastal_data%tauy_ssi_star)
DEALLOCATE(coastal_data%mapi)
DEALLOCATE(coastal_data%mapj)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE coastal_dealloc

!===============================================================================
SUBROUTINE coastal_assoc(coastal,coastal_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(coastal_data_type), TARGET, INTENT(IN OUT) :: coastal_data
  ! Instance of the data type we are associtating to
TYPE(coastal_type), INTENT(IN OUT) :: coastal
  ! Instance of the pointer type we are associating

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='COASTAL_ASSOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL coastal_nullify(coastal)

coastal%fland => coastal_data%fland
coastal%tstar_land_ij => coastal_data%tstar_land_ij
coastal%tstar_sea_ij => coastal_data%tstar_sea_ij
coastal%tstar_sice_ij => coastal_data%tstar_sice_ij
coastal%tstar_sice_sicat => coastal_data%tstar_sice_sicat
coastal%tstar_ssi_ij => coastal_data%tstar_ssi_ij
coastal%taux_land_ij => coastal_data%taux_land_ij
coastal%taux_ssi_ij => coastal_data%taux_ssi_ij
coastal%tauy_land_ij => coastal_data%tauy_land_ij
coastal%tauy_ssi_ij => coastal_data%tauy_ssi_ij
coastal%vshr_land_ij => coastal_data%vshr_land_ij
coastal%vshr_ssi_ij => coastal_data%vshr_ssi_ij
coastal%surf_ht_flux_land_ij => coastal_data%surf_ht_flux_land_ij
coastal%taux_land_star => coastal_data%taux_land_star
coastal%tauy_land_star => coastal_data%tauy_land_star
coastal%taux_ssi_star => coastal_data%taux_ssi_star
coastal%tauy_ssi_star => coastal_data%tauy_ssi_star
coastal%mapi => coastal_data%mapi
coastal%mapj => coastal_data%mapj
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE coastal_assoc

!===============================================================================
SUBROUTINE coastal_nullify(coastal)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(coastal_type), INTENT(IN OUT) :: coastal
  ! Instance of the pointer type we are associating

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='COASTAL_NULLIFY'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

NULLIFY(coastal%fland)
NULLIFY(coastal%tstar_land_ij)
NULLIFY(coastal%tstar_sea_ij)
NULLIFY(coastal%tstar_sice_ij)
NULLIFY(coastal%tstar_sice_sicat)
NULLIFY(coastal%tstar_ssi_ij)
NULLIFY(coastal%taux_land_ij)
NULLIFY(coastal%taux_ssi_ij)
NULLIFY(coastal%tauy_land_ij)
NULLIFY(coastal%tauy_ssi_ij)
NULLIFY(coastal%vshr_land_ij)
NULLIFY(coastal%vshr_ssi_ij)
NULLIFY(coastal%surf_ht_flux_land_ij)
NULLIFY(coastal%taux_land_star)
NULLIFY(coastal%tauy_land_star)
NULLIFY(coastal%taux_ssi_star)
NULLIFY(coastal%tauy_ssi_star)
NULLIFY(coastal%mapi)
NULLIFY(coastal%mapj)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE coastal_nullify

!-------------------------------------------------------------------------------

END MODULE coastal
