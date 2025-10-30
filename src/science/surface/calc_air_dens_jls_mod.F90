! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    SUBROUTINE CALC_AIR_DENS--------------------------------------------

MODULE calc_air_dens_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='CALC_AIR_DENS_MOD'

CONTAINS
! Subroutine Interface:
SUBROUTINE calc_air_dens(l_mr_physics,qv_star,rhostar,rhostar_mom)

USE atm_fields_bounds_mod, ONLY: tdims
USE planet_constants_mod, ONLY: repsilon
USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

! Description:
!     Calculates the surface air density

! Documentation : UM Documentation Paper 24

! Subroutine arguments:
LOGICAL, INTENT(IN) ::                                                         &
l_mr_physics
                      ! IN Switch for when mixing ratios are used
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
qv_star(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                      ! IN surface humidity

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
 rhostar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                      ! INOUT Surface air density for scalar exchange
,rhostar_mom(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                      ! INOUT Surface air density for momentum exchange

! Local scalars:
INTEGER ::                                                                     &
 i,j                  ! WORK Loop counters

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CALC_AIR_DENS'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j)                                      &
!$OMP SHARED(tdims,l_mr_physics,rhostar,rhostar_mom,repsilon,qv_star)

IF (l_mr_physics) THEN

!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start, tdims%j_end
    DO i = tdims%i_start, tdims%i_end
      ! use dry rho for scalars and wet rho for momentum
      rhostar(i,j) = rhostar_mom(i,j) /                                        &
                     (1.0 + (1.0 / repsilon) * qv_star(i,j))
      rhostar_mom(i,j) = rhostar(i,j) * (1.0 + qv_star(i,j))
    END DO
  END DO
!$OMP END DO NOWAIT

ELSE

!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start, tdims%j_end
    DO i = tdims%i_start, tdims%i_end
      ! use wet density for scalars and momentum
      rhostar_mom(i,j) = rhostar_mom(i,j) /                                    &
                         (1.0 + ( (1.0 / repsilon) - 1.0 ) * qv_star(i,j))
      rhostar(i,j) = rhostar_mom(i,j)
    END DO
  END DO
!$OMP END DO NOWAIT

END IF
!$OMP END PARALLEL

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE calc_air_dens

END MODULE calc_air_dens_mod
