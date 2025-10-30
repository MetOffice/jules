! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE sf_rib_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SF_RIB_MOD'

CONTAINS
!  SUBROUTINE SF_RIB -----------------------------------------------
!
!  Purpose: Calculate bulk Richardson number for surface layer
!
! ------------------------------------------------------------------

!   Subroutine interface
SUBROUTINE sf_rib (                                                            &
 points,surft_pts,pts_index,surft_index,                                       &
 bq_1,bt_1,qstar,q_elev,resft,t_elev,tstar,vshr,z0h,z0m,zdt,                   &
 z1_tq,z1_uv,l_vegdrag,rib,db                                                  &
 )

USE atm_fields_bounds_mod, ONLY: tdims
USE theta_field_sizes, ONLY: t_i_length

USE planet_constants_mod, ONLY: cp, g

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

INTEGER ::                                                                     &
 points                                                                        &
                      ! IN Total number of points.
,surft_pts                                                                     &
                      ! IN Number of tile points.
,pts_index(points)                                                             &
                      ! IN Index of points.
,surft_index(points) ! IN Index of tile points.

LOGICAL, INTENT(IN) ::                                                         &
 l_vegdrag
                       ! IN Option for vegetation canopy drag scheme.

REAL(KIND=real_jlslsm) ::                                                      &
 bq_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                       ! IN A buoyancy parameter for lowest atm
!                            !    level. ("beta-q twiddle").
,bt_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                       ! IN A buoyancy parameter for lowest atm
!                            !    level. ("beta-T twiddle").
,qstar(points)                                                                 &
                       ! IN Surface saturated sp humidity.
,q_elev(points)                                                                &
                       ! IN Total water content of lowest
!                            !    atmospheric layer (kg per kg air).
,resft(points)                                                                 &
                       ! IN Total resistance factor.
,t_elev(points)                                                                &
                       ! IN Liquid/frozen water temperature for
!                            !    lowest atmospheric layer (K).
,tstar(points)                                                                 &
                       ! IN Surface temperature (K).
,vshr(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                       ! IN Magnitude of surface-to-lowest-level
!                            !    wind shear.
,z0h(points)                                                                   &
                       ! IN Roughness length for heat and
!                            !    moisture m
,z0m(points)                                                                   &
                       ! IN Effective roughness length for
!                            !    momentum
,zdt(points)                                                                   &
                       ! IN Difference between the canopy height and
                       !    displacement height (m)
,z1_tq(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                       ! IN Height of lowest TQ level (m).
,z1_uv(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                       ! IN Height of lowest UV level (m).

REAL(KIND=real_jlslsm) ::                                                      &
 rib(points)                                                                   &
                     ! OUT Bulk Richardson number for lowest layer
,db(points)          ! OUT Buoyancy difference between surface
!                          !     and lowest atmospheric level.

!  Workspace --------------------------------------------------------
INTEGER ::                                                                     &
 i,j                                                                           &
                     ! Horizontal field index.
,k                                                                             &
                     ! Tile field index.
,l                   ! Points field index.

REAL(KIND=real_jlslsm) ::                                                      &
 dq                                                                            &
                     ! Sp humidity difference between surface
!                          ! and lowest atmospheric level (Q1 - Q*).
,dtemp               ! Modified temperature difference between
!                            surface and lowest atmospheric level.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SF_RIB'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(l,k,j,i,dtemp,dq)                                                &
!$OMP SHARED(surft_pts,surft_index,pts_index,t_i_length,t_elev,tstar,g,cp,     &
!$OMP        z1_tq,z0m,z0h,q_elev,qstar,db,bt_1,bq_1,resft,rib,z1_uv,vshr,     &
!$OMP        zdt,l_vegdrag)
DO k = 1,surft_pts
  l = surft_index(k)
  j=(pts_index(l) - 1) / t_i_length + 1
  i = pts_index(l) - (j-1) * t_i_length

  !-----------------------------------------------------------------------
  !!  1 Calculate temperature (strictly, liquid/ice static energy) and
  !!    humidity jumps across the surface layer.
  !-----------------------------------------------------------------------
  IF (l_vegdrag) THEN
    dtemp = t_elev(l) - tstar(l) + (g / cp) * (z1_tq(i,j) + zdt(l) - z0h(l))
  ELSE
    dtemp = t_elev(l) - tstar(l) + (g / cp) * (z1_tq(i,j) + z0m(l) - z0h(l))
                                                                   !P243.118
  END IF
  dq = q_elev(l) - qstar(l)                                        !P243.119

  !-----------------------------------------------------------------------
  !!  2 Calculate bulk Richardson number for the surface layer.
  !-----------------------------------------------------------------------
  db(l) = g * (bt_1(i,j) * dtemp + bq_1(i,j) * resft(l) * dq)
  rib(l) = z1_uv(i,j) * db(l) / ( vshr(i,j) * vshr(i,j) )
END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE sf_rib
END MODULE sf_rib_mod
