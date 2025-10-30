#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (c) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE init_urban_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='INIT_URBAN_MOD'

CONTAINS
SUBROUTINE init_urban (urban_param)
! Description:
!   Routine to copy D1 array prognostics to JULES module.
!
!  6.1   10/01/07   First written. Peter Clark and Aurore Porson
!
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
! This file belongs in section: Land

USE atm_fields_mod, ONLY:                                                      &
   hgt, hwr, wrr, disp, ztm, albwl, albrd, emisw, emisr

USE nlsizes_namelist_mod, ONLY: land_pts => land_field

USE jules_surface_mod, ONLY: l_urban2t, l_anthrop_heat_use_wrr

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE urban_param_mod, ONLY: urban_param_type

IMPLICIT NONE

!Arguments
TYPE(urban_param_type), INTENT(IN OUT) :: urban_param

INTEGER :: l

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='INIT_URBAN'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! .NOT. l_moruses
! Currently only really need wrr for anthrop heat scaling, but they are all in
! the dump and set currently for l_urban2t. Should probably be removed from the
! dump unless l_moruses, but for now pass to avoid dump confusion i.e. if
! MORUSES is run from a seemingly MORUSES dump, which turns out to be urban-2t
! dump with unset fields.
! Prognostics are not currently updated, but some may be in the future to
! account for snow and are thus copied back again the the D1 array in
! atmos_physics2. Any changes made here should be duplicated there too.
IF ( l_urban2t ) THEN
  DO l = 1, land_pts
    urban_param%wrr_gb(l)   = wrr(l)
    urban_param%hwr_gb(l)   = hwr(l)
    urban_param%hgt_gb(l)   = hgt(l)
    urban_param%disp_gb(l)  = disp(l)
    urban_param%ztm_gb(l)   = ztm(l)
    urban_param%albwl_gb(l) = albwl(l)
    urban_param%albrd_gb(l) = albrd(l)
    urban_param%emisw_gb(l) = emisw(l)
    urban_param%emisr_gb(l) = emisr(l)
  END DO
  ! To maintain bit comparability in generate_anthropogenic_heat
  IF ( ANY( urban_param%wrr_gb(:) > 0.0 ) ) l_anthrop_heat_use_wrr = .TRUE.
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE init_urban
END MODULE init_urban_mod
#endif
