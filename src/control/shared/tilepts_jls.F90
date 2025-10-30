! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Purpose:
! Counts the number of points containing each surface type and creates
! a surft_index array specifying the location of these points on the land
! grid.

! Subroutine Interface:

MODULE tilepts_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='TILEPTS_MOD'
CONTAINS
SUBROUTINE tilepts(land_pts,frac,surft_pts,surft_index,l_lice_point,           &
                   l_lice_surft)

USE jules_surface_mod, ONLY: all_tiles, l_elev_land_ice

USE jules_surface_types_mod, ONLY: ntype, ice, elev_ice, elev_rock

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

INTEGER, INTENT(IN) :: land_pts  !num land points to process

REAL(KIND=real_jlslsm), INTENT(IN) :: frac(land_pts,ntype)
                                          !fractions of surface types

INTEGER, INTENT(OUT) ::  surft_pts(ntype)                                      &
                                          ! Number of land points which
                                          ! include the nth surface type
,                        surft_index(land_pts,ntype)
                                          ! Indices of land points which
                                          ! include the nth surface type

LOGICAL, INTENT(IN) :: l_lice_point(land_pts)
LOGICAL, INTENT(OUT) :: l_lice_surft(ntype)

LOGICAL :: use_tile  ! Indicates if we will model the tile for the current
                     ! land point

INTEGER :: n,l,c   !local counters: type, land pts, count

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='TILEPTS'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------
! Create the surft_index array of land points with each surface type
!-----------------------------------------------------------------------
!$OMP PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                               &
!$OMP SHARED( ntype, land_pts, surft_index, all_tiles, frac, ice,              &
!$OMP         elev_ice, elev_rock, l_lice_point, l_lice_surft,                 &
!$OMP         l_elev_land_ice, surft_pts )                                     &
!$OMP PRIVATE( l, n, c, use_tile )
DO n = 1,ntype
  c = 0
  l_lice_surft(n) = ( n == ice .OR. ANY(elev_ice == n) )
  !CDIR NODEP
  DO l = 1,land_pts
    surft_index(l,n) = 0
    IF ( all_tiles == 0 ) THEN
      ! If all_tiles is off, we model tiles with frac > 0 only
      use_tile = ( frac(l,n) > 0.0 )
      ! If there are elevated ice tiles, model them all on land-ice points
      ! as this may be needed for on- or off-line icesheet coupling
      IF (l_elev_land_ice .AND. l_lice_point(l) .AND. l_lice_surft(n))         &
        use_tile = .TRUE.
    ELSE
      !  with all_tiles we do
      !   * All tiles except the ice/elevated tiles on non-land-ice points
      !   * All ice and elevated rock tiles on land-ice points (l_lice_point .TRUE.)
      !  (this is more complicated where land ice can have multiple tiles, some rock,
      !  some ice)

      ! Default to TRUE
      use_tile = .TRUE.

      ! Correct state for whether this tile is on an land ice gridbox
      IF ( .NOT. l_lice_point(l)) THEN
        ! l _is not_ a land-ice gridbox, so FALSE if n
        ! is an ice or elevated rock tile
        IF (l_lice_surft(n) .OR. ANY(elev_rock == n)) use_tile = .FALSE.
      ELSE
        ! l _is_ an ice/elevated gridbox, so FALSE if n
        ! is not an ice or elevated rock tile
        IF ( .NOT. (l_lice_surft(n) .OR. ANY(elev_rock == n) )) use_tile = .FALSE.
      END IF

    END IF

    IF ( use_tile ) THEN
      c = c + 1
      surft_index(c,n) = l
    END IF
  END DO
  surft_pts(n) = c
END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE tilepts
END MODULE tilepts_mod
