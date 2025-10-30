! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Routine to set the surface parameters for non-vegetated surface types
!
! *********************************************************************
MODULE nvg_sparm_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='NVG_SPARM_MOD'

CONTAINS

SUBROUTINE nvg_sparm (land_pts, surft_pts, surft_index, z0m_soil_gb, ztm_gb,   &
                      catch_t, z0_t)

!Use in relevant variables
USE jules_surface_types_mod,  ONLY: npft, ntype, urban_canyon,                 &
                                    urban_roof, soil
USE nvegparm,                 ONLY: catch_nvg, z0_nvg, l_z0_nvg
USE jules_surface_mod,        ONLY: l_vary_z0m_soil
USE jules_urban_mod,          ONLY: l_moruses
USE dust_param,               ONLY: z0_soil

USE parkind1,                 ONLY: jprb, jpim
USE yomhook,                  ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Number of land points to be processed.
  surft_pts(ntype),                                                            &
    ! Number of land points which include the nth surface type.
  surft_index(land_pts,ntype)
    ! Indices of land points which include the nth surface type.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  z0m_soil_gb(land_pts),                                                       &
    ! z0m of bare soil (m)
  ztm_gb(land_pts)
    ! Urban roughness length

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  catch_t(land_pts,ntype),                                                     &
    ! Canopy capacities for types (kg/m2).
  z0_t(land_pts,ntype)
    ! Roughness lengths for types (m).

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  j,l,n  ! Loop counters

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='NVG_SPARM'

!-----------------------------------------------------------------------------
!end of header
!-----------------------------------------------------------------------------
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! It would be nice to not have to set the catch_surft and z0_surft to the
! namelist values whenever sparm is called, however something initialises them
! to zero before now. l_z0_nvg could be used, by setting it to .false. after it
! has been done once for z0.
DO n = npft+1,ntype
  DO j = 1,surft_pts(n)
    l = surft_index(j,n)
    catch_t(l,n) = catch_nvg(n - npft)
  END DO
END DO

! z0m is set from the from the namelist value when l_z0_nvg
! Otherwise:
!  l_vary_z0m_soil sets z0_t(:,soil)
!  l_moruses sets z0_t(:,{urban_canyon,urban_roof})
DO n = npft+1,ntype
  IF ( l_z0_nvg(n - npft) ) THEN
    DO j = 1,surft_pts(n)
      l = surft_index(j,n)
      z0_t(l,n) = z0_nvg(n - npft)
    END DO
  END IF
END DO

IF (l_vary_z0m_soil) THEN
  ! For soil, get the z0m from the input data:
  n = soil
  DO j = 1,surft_pts(n)
    l = surft_index(j,n)
    z0_t(l,n) = z0m_soil_gb(l)
  END DO
END IF

! MORUSES Set canyon & roof roughness length
IF ( l_moruses ) THEN
  n = urban_canyon
  DO j = 1,surft_pts(n)
    l = surft_index(j,n)
    z0_t(l,n)          = ztm_gb(l)
    z0_t(l,urban_roof) = ztm_gb(l)
  END DO
END IF

!-----------------------------------------------------------------------------
! Set bare soil roughness for use in 1 tile dust scheme
!-----------------------------------------------------------------------------
! The following line has been added to readlsta so to fix CRUNs
! and is now a duplication.
! When l_vary_z0m_soil is true, the variable is also passed down to the
! dust emission, so this isn't used.

z0_soil = z0_nvg(soil - npft)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE nvg_sparm
END MODULE nvg_sparm_mod
