! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE soil_evap_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SOIL_EVAP_MOD'

CONTAINS

! *********************************************************************
! Description:
! Subroutine to adjust canopy conductance and soil moisture extraction
! for soil evaporation beneath vegetation.
! *********************************************************************

SUBROUTINE soil_evap (npnts,nshyd,surft_pts,surft_index,                       &
                      gsoil,lai,gs,wt_ext,fsoil                                &
                      ,gsoil_irr,gs_irr,wt_ext_irr                             &
                      )

USE jules_irrig_mod, ONLY: l_irrig_dmd
USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
 npnts                                                                         &
                      ! IN Number of gridpoints.
,nshyd                                                                         &
                      ! IN Number of soil moisture layers.
,surft_pts                                                                     &
                      ! IN Number of points containing the
!                           !    given surface type.
,surft_index(npnts)    ! IN Indices on the land grid of the
!                           !    points containing the given
!                           !    surface type.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 gsoil(npnts)                                                                  &
                      ! IN Soil surface conductance (m/s).
,lai(npnts)           ! IN Leaf area index.


REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
gsoil_irr(npnts)     ! IN  Soil surface conductance (m/s) over
!                                 irrigated fraction.

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
 gs(npnts)                                                                     &
                      ! INOUT Surface conductance (m/s).
,wt_ext(npnts,nshyd)  ! INOUT Fraction of evapotranspiration
!                           !       extracted from each soil layer.

! required for irrigation
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
 wt_ext_irr(npnts,nshyd)                                                       &
!                     ! INOUT Fraction of evapotranspiration
!                           !       extracted from each soil layer
!                           !       over irrigated area.
,gs_irr(npnts)
!                     ! INOUT Conductance for irrigated
!                           !     surface fraction.

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 fsoil(npnts)         ! Fraction of ground below canopy
!                           ! contributing to evaporation.

INTEGER ::                                                                     &
 j,k,l                ! Loop indices

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SOIL_EVAP'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!CDIR NODEP - order independent so can vectorise

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(l,j,k)                                                           &
!$OMP SHARED(npnts,fsoil,surft_pts,surft_index,lai,nshyd,wt_ext,gs,gsoil,      &
!$OMP        wt_ext_irr,gs_irr,gsoil_irr,l_irrig_dmd)

! Initialisations

!$OMP DO SCHEDULE(STATIC)
DO l = 1,npnts
  fsoil(l) = 0.0
END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
DO j = 1,surft_pts
  l = surft_index(j)
  fsoil(l) = EXP(-0.5 * lai(l))
END DO
!$OMP END DO NOWAIT

DO k = 2,nshyd
!$OMP DO SCHEDULE(STATIC)
  DO j = 1,surft_pts
    l = surft_index(j)
    wt_ext(l,k) = gs(l) * wt_ext(l,k) / (gs(l) + fsoil(l) * gsoil(l))
    IF (l_irrig_dmd) THEN
      wt_ext_irr(l,k) = gs_irr(l) * wt_ext_irr(l,k)                            &
             / (gs_irr(l) + fsoil(l) * gsoil_irr(l))
    END IF
  END DO
!$OMP END DO NOWAIT
END DO

!$OMP DO SCHEDULE(STATIC)
!CDIR NODEP
DO j = 1,surft_pts
  l = surft_index(j)
  wt_ext(l,1) = (gs(l) * wt_ext(l,1) + fsoil(l) * gsoil(l))                    &
                 / (gs(l) + fsoil(l) * gsoil(l))
  gs(l) = gs(l) + fsoil(l) * gsoil(l)

  IF (l_irrig_dmd) THEN
    wt_ext_irr(l,1) = (gs_irr(l) * wt_ext_irr(l,1)                             &
                        + fsoil(l) * gsoil_irr(l))                             &
                        / (gs_irr(l) + fsoil(l) * gsoil_irr(l))

    ! Transpiration and soil conductances over irrigated fraction of tile
    ! relative to tile mean conductance (scaled by relative area later).
    ! Assume soil evaporation uses grid box mean soil moisture:
    gs_irr(l) = gs_irr(l) + fsoil(l) * gsoil_irr(l)
  END IF
END DO
!$OMP END DO NOWAIT

!$OMP END PARALLEL
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE soil_evap
END MODULE soil_evap_mod
