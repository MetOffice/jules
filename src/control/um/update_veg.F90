! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Subroutine UPDATE_VEG
!
!  Purpose: The routine is entered when any of the ancillary
!           fields have to be updated. It then checks to see if
!           leaf area index and/or canopy height have been updated.
!           If this is the case, then the subroutine SPARM is called
!           to ensure that all other vegetation parameters are
!           consistent.
!
!           Code Owner: Please refer to ModuleLeaders.txt

MODULE update_veg_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName='UPDATE_VEG_MOD'

CONTAINS

SUBROUTINE update_veg ()

!Use in relevant subroutines
USE sparm_mod,                ONLY: sparm
USE infiltration_rate_mod,    ONLY: infiltration_rate
USE tilepts_mod,              ONLY: tilepts

!USE in JULES modules
USE jules_surface_types_mod, ONLY: ntype

!USE in UM modules (check!)
USE atm_fields_mod,           ONLY: canht_pft, lai_pft,                        &
                                    frac_surft        => frac_typ,             &
                                    z0m_soil_gb       => z0m_soil,             &
                                    ztm_gb            => ztm,                  &
                                    catch_snow_surft  => catch_snow,           &
                                    catch_surft       => catch_tile,           &
                                    z0_surft          => z0_tile,              &
                                    z0h_bare_surft    => z0h_tile,             &
                                    satcon_gb         => sat_soil_cond,        &
                                    infil_surft       => infil_tile,           &
                                    !JULES TYPEs
                                    ainfo

USE nlsizes_namelist_mod,     ONLY: nsurft            => ntiles,               &
                                    land_pts          => land_field

USE um_stashcode_mod,         ONLY: stashcode_lai, stashcode_canopy_height,    &
                                    stashcode_z0m_soil
USE ancil_mod, ONLY: num_ancil_requests, ancil_requests
USE cancila_mod, ONLY: update

USE yomhook,                  ONLY: lhook, dr_hook
USE parkind1,                 ONLY: jprb, jpim

IMPLICIT NONE

!     Local variables
INTEGER :: surft_pts(ntype)              ! No of land pts which include surface
                                        !  type ntype
INTEGER :: surft_index(land_pts,ntype) ! Indices of land points which
                                        !  include the surface type ntype

INTEGER :: field
INTEGER :: field_lai, field_canht, field_z0msoil
LOGICAL :: update_lai, update_canht, update_z0msoil

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='UPDATE_VEG'

! Update vegetation parameters if required

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

field_lai       = 0
update_lai      = .FALSE.
field_canht     = 0
update_canht    = .FALSE.
field_z0msoil   = 0
update_z0msoil  = .FALSE.

! check to see which fields are present as ancillary files to do updates,
! and whether or not they are updated at this time.
DO field =  1,num_ancil_requests
  IF (ancil_requests(field)%stashcode == stashcode_lai) THEN
    ! field is present:
    field_lai = field
    ! is it updated?:
    update_lai = update(field_lai)
  END IF
  IF (ancil_requests(field)%stashcode == stashcode_canopy_height) THEN
    field_canht = field
    update_canht = update(field_canht)
  END IF
  IF (ancil_requests(field)%stashcode == stashcode_z0m_soil) THEN
    field_z0msoil = field
    update_z0msoil = update(field_z0msoil)
  END IF
END DO

IF (update_lai .OR. update_canht .OR. update_z0msoil ) THEN
  !-----------------------------------------------------------------------
  ! Call TILEPTS to initialise TILE_PTS and TILE_INDEX
  !-----------------------------------------------------------------------
  CALL tilepts(land_pts,frac_surft,surft_pts,surft_index,ainfo%l_lice_point,   &
               ainfo%l_lice_surft)

  !-----------------------------------------------------------------------
  ! Initialise tiled and gridbox mean vegetation parameters
  !-----------------------------------------------------------------------
  CALL sparm (land_pts, nsurft, surft_pts, surft_index,                        &
              frac_surft, canht_pft, lai_pft, z0m_soil_gb,                     &
              catch_snow_surft, catch_surft, z0_surft, z0h_bare_surft,         &
              ztm_gb)

  !Not sure we need to call this here, but rose stem breaks if we don't
  CALL infiltration_rate(land_pts, nsurft, surft_pts, surft_index,             &
                        satcon_gb, frac_surft, infil_surft)

END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE update_veg
END MODULE update_veg_mod
