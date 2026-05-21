! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    SUBROUTINE ALT---------------------------------------------------------

! Description:
!     Calculates the active layer thickness for the current year
!     and the year before. Last year ALT is used to update 
!     the maximum rooting depth for some PFTs.

MODULE alt_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='ALT_MOD'

CONTAINS

SUBROUTINE alt ( npnts, nshyd, soil_pts, soil_index, dz, tsoil,                &
                 alt_currentyear, alt_lastyear )

!Use in relevant variables
USE conversions_mod,        ONLY: zerodegc
USE model_time_mod, ONLY: start_of_year

USE parkind1,       ONLY: jprb, jpim
USE yomhook,                ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  npnts,                                                                       &
    ! Number of gridpoints.
  nshyd,                                                                       &
    ! Number of soil moisture levels.
  soil_pts
    ! Number of soil points.

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  soil_index(npnts)
    ! Array of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  dz(nshyd),                                                                   &
    ! Thicknesses of the soil layers (m).
  tsoil(npnts,nshyd)
    ! Sub-surface temperatures (K).

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  alt_currentyear(npnts),                                                      &
    ! Current year active layer thickness
  alt_lastyear(npnts)
    ! Last year active layer thickness

!-----------------------------------------------------------------------------
! Local scalar variables:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, j, n
    ! Loop counters.

REAL(KIND=real_jlslsm) ::                                                      &
  alpha = 0.5
    ! Smoothing factor for updating alt_lastyear

!-----------------------------------------------------------------------------
! Local array variables:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  alt_instant_ind(npnts)
    ! Indice of the first frozen layer

REAL(KIND=real_jlslsm) ::                                                      &
  alt_instant(npnts)
    ! Depth to the first frozen soil layer (instantaneous ALT)

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='ALT'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Calculate instantaneous ALT and update current and last year ALT 
!-----------------------------------------------------------------------------

DO j = 1,soil_pts
  i = soil_index(j)

  !------------------------------
  ! Calculate instantaneous ALT
  !------------------------------
  alt_instant_ind(i) = nshyd + 1 ! Intialise with unfrozen profile

  DO n = 1,nshyd
    IF (tsoil(i,n) <= zerodegc) THEN
        alt_instant_ind(i) = n
        EXIT
    END IF
  END DO ! n=1,nshyd
  
  IF (alt_instant_ind(i) == 1) THEN
    ! All the column frozen
    alt_instant(i) = 0.0
  ELSE
    alt_instant(i) = SUM(dz(1:alt_instant_ind(i)-1))
  END IF

  !-----------------------------------
  ! Update current and last year ALT
  !-----------------------------------
  IF (alt_instant(i) > alt_currentyear(i)) THEN
    ! Update current ALT
    alt_currentyear(i) = alt_instant(i)
  END IF

  IF ( start_of_year ) THEN
    ! Update last year ALT on the first timestep of the year
    alt_lastyear(i) = alt_lastyear(i) +                                        &
                      alpha * ( alt_currentyear(i) - alt_lastyear(i) )
    alt_currentyear(i) = alt_instant(i)
  END IF

END DO ! j=1,soil_pts

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE alt
END MODULE alt_mod