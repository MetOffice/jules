! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Routine to set the components of the albedo of the soil surface
! (direct / diffuse and VIS / NIR) from the broad-band albedo.
!
MODULE set_soil_alb_components_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName='SET_SOIL_ALB_COMPONENTS_MOD'

CONTAINS

SUBROUTINE set_soil_alb_components(                                            &
  ! INTENT(IN)
  land_pts, albsoil, cosz_gb,                                                  &
  ! INTENT(OUT)
  albudir, albudif,                                                            &
  ! INTENT(IN), OPTIONAL
  albobs_scaling                                                               &
)


USE um_types, ONLY: real_jlslsm

USE jules_radiation_mod,      ONLY:                                            &
  l_partition_albsoil, ratio_albsoil, swdn_frac_albsoil, l_hapke_soil

USE parkind1,                 ONLY: jprb, jpim
USE yomhook,                  ONLY: lhook, dr_hook

USE calc_direct_albsoil_mod, ONLY: calc_direct_albsoil

IMPLICIT NONE

!Subroutine arguments
!Scalar arguments with intent(in):

INTEGER, INTENT(IN) ::                                                         &
  land_pts
                                      !Number of land points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  albsoil(land_pts),                                                           &
                                      !Soil albedo.
  cosz_gb(land_pts)
                                      !Cosine of the zenith angle.
REAL(KIND=real_jlslsm), INTENT(IN), OPTIONAL ::                                &
  albobs_scaling(land_pts,2)

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  albudir(land_pts,2),                                                         &
                                      !Direct albedo of underlying surface
  albudif(land_pts,2)
                                      !Diffuse albedo of underlying surface

!Local variables:
INTEGER  ::                                                                    &
  l
                                      !Loop index

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SET_SOIL_ALB_COMPONENTS'


!End of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEl DEFAULT(NONE)                                                   &
!$OMP PRIVATE(l)                                                               &
!$OMP SHARED(l_partition_albsoil,land_pts,albsoil,albudif,                     &
!$OMP        swdn_frac_albsoil,ratio_albsoil,albobs_scaling,cosz_gb,           &
!$OMP        l_hapke_soil,albudir)

! Set the diffuse components of the albedo.
IF (l_partition_albsoil) THEN
  !
  !  Use a threshold of 0.6 to separate true soils (broad-band albedo
  !  only barely exceeding 0.5 in isolated spots) from values
  !  actually representing land ice (broad-band albedo about 0.75).
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    IF (albsoil(l) < 0.6) THEN
      albudif(l,1) = albsoil(l) /                                              &
        (1.0 + swdn_frac_albsoil * (ratio_albsoil-1.0))
      albudif(l,2) = albsoil(l) * ratio_albsoil /                              &
        (1.0 + swdn_frac_albsoil * (ratio_albsoil-1.0))
    ELSE
      albudif(l,1) = albsoil(l)
      albudif(l,2) = albsoil(l)
    END IF
  END DO
!$OMP END DO NOWAIT
ELSE
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    albudif(l,1) = albsoil(l)
    albudif(l,2) = albsoil(l)
  END DO
!$OMP END DO NOWAIT
END IF

! Apply scaling factors if present
IF (PRESENT(albobs_scaling)) THEN
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    albudif(l,1) = albudif(l,1) * albobs_scaling(l,1)
    albudif(l,2) = albudif(l,2) * albobs_scaling(l,2)
  END DO
!$OMP END DO NOWAIT
END IF

! Set the direct components.
IF (l_hapke_soil) THEN
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    IF (cosz_gb(l) > EPSILON(cosz_gb)) THEN
      albudir(l,1) = calc_direct_albsoil(albudif(l,1), cosz_gb(l))
      albudir(l,2) = calc_direct_albsoil(albudif(l,2), cosz_gb(l))
    ELSE
      albudir(l,1) = albudif(l,1)
      albudir(l,2) = albudif(l,2)
    END IF
  END DO
!$OMP END DO NOWAIT
ELSE
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    albudir(l,1) = albudif(l,1)
    albudir(l,2) = albudif(l,2)
  END DO
!$OMP END DO NOWAIT
END IF

!$OMP END PARALLEl
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE set_soil_alb_components
END MODULE set_soil_alb_components_mod
