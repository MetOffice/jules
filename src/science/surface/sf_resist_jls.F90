! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE sf_resist_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SF_RESIST_MOD'

CONTAINS
!    L  SUBROUTINE SF_RESIST----------------------------------------------
!
!    Purpose: Calculate surface moisture flux resistance factors.
!
!    Arguments --------------------------------------------------------
SUBROUTINE sf_resist (                                                         &
 land_pts,surft_pts,land_index,surft_index,cansnowtile,                        &
 canopy,catch,ch,dq,epdt,flake,gc,gc_stom_surft,snowdep_surft,snow_surft,      &
 vshr,tstar,fracaero_t, fracaero_s,resfs,resft,                                &
 resfs_stom,l_et_stom,l_et_stom_surft)

USE atm_fields_bounds_mod, ONLY: tdims
USE theta_field_sizes, ONLY: t_i_length

USE jules_snow_mod, ONLY: maskd, frac_snow_subl_melt
USE jules_science_fixes_mod, ONLY: l_fix_snow_frac, l_fix_neg_snow
USE water_constants_mod, ONLY: tm, rho_ice
USE jules_surface_mod, ONLY: l_aggregate
USE jules_vegetation_mod, ONLY: can_model
USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
 land_pts                                                                      &
                       ! IN Total number of land points.
,surft_pts                                                                     &
                       ! IN Number of tile points.
,land_index(land_pts)                                                          &
                       ! IN Index of land points.
,surft_index(land_pts) ! IN Index of tile points.


LOGICAL, INTENT(IN) ::                                                         &
cansnowtile            ! IN Switch for pft canopy snow model

LOGICAL, INTENT(IN) ::                                                         &
 l_et_stom                                                                     &
                        !IN flag for calculating transpiration for gridbox
,l_et_stom_surft
                        !IN flag for calculating transpiration on tiles

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 canopy(land_pts)                                                              &
                     ! IN Surface water (kg per sq metre).  F642.
,catch(land_pts)                                                               &
                     ! IN Surface capacity (max. surface water)
!                          !    (kg per sq metre).  F6416.
,ch(land_pts)                                                                  &
                     ! IN Transport coefficient for heat and
!                          !    moisture transport
,dq(land_pts)                                                                  &
                     ! IN Sp humidity difference between surface
!                          !    and lowest atmospheric level (Q1 - Q*).
,epdt(land_pts)                                                                &
                     ! IN "Potential" Evaporation * Timestep.
!                          !    Dummy variable for first call to routine
,flake(land_pts)                                                               &
                     ! IN Lake fraction.
,gc(land_pts)                                                                  &
                     ! IN Interactive canopy conductance
!                          !    to evaporation (m/s)
,gc_stom_surft(land_pts)                                                       &
                     ! IN canopy conductance (excluding soil) (m/s)
,snowdep_surft(land_pts)                                                       &
                     ! IN Snow depth (m)
,snow_surft(land_pts)                                                          &
                     ! IN Lying snow on tiles (kg/m2)
,tstar(land_pts)                                                               &
                     ! IN Surface temperature (K)
,vshr(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                     ! IN Magnitude of surface-to-lowest-level
!                          !     windshear

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 fracaero_t(land_pts)                                                          &
                     ! OUT Total fraction of surface moisture flux with
!                          !     only aerodynamic resistance
!                          !     frozen fractions (:,2).
,fracaero_s(land_pts)                                                          &
                     ! OUT Fractions of surface moisture flux with
!                          !     only aerodynamic resistance over a frozen
!                          !     surface.
!                          !     i.e. the fraction of the tile with
!                          !     sublimation or deposition is fracaero_s
!                          !     and the fraction with evaporation or
!                          !     condensation is fracaero_t-fracaero_s
,resfs(land_pts)                                                               &
                     ! OUT Combined soil, stomatal and aerodynamic
!                          !     resistance factor for fraction 1-fracaero_t.
,resft(land_pts)                                                               &
                     ! OUT Total resistance factor
!                          !     fracaero_t+(1-fracaero_t)*RESFS.
,resfs_stom(land_pts)
                     ! OUT Combined stomatal and aerodynamic
!                          !     resistance factor for fraction 1-fracaero_t.

! Workspace -----------------------------------------------------------
INTEGER ::                                                                     &
 i,j                                                                           &
             ! Horizontal field index.
,k                                                                             &
             ! Tile field index.
,l           ! Land field index.
REAL(KIND=real_jlslsm) ::                                                      &
 msk_sndpth
             ! Temporary scalar

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SF_RESIST'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------
!     Evaporation over land surfaces without snow is limited by
!     soil moisture availability and stomatal resistance.
!-----------------------------------------------------------------------

!$OMP PARALLEL DO IF(surft_pts > 1)                                            &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(l,k,j,i,msk_sndpth)                                              &
!$OMP SHARED(surft_pts,surft_index,land_index,t_i_length,fracaero_t,fracaero_s,&
!$OMP        dq,snowdep_surft,tstar,snow_surft,                                &
!$OMP        catch,frac_snow_subl_melt,maskd,resfs,gc,ch,vshr,l_et_stom,       &
!$OMP        l_et_stom_surft,resfs_stom,gc_stom_surft,flake,                   &
!$OMP        canopy,epdt,resft,l_fix_snow_frac, l_fix_neg_snow)
DO k = 1,surft_pts
  l = surft_index(k)
  j=(land_index(l) - 1) / t_i_length + 1
  i = land_index(l) - (j-1) * t_i_length

  !-----------------------------------------------------------------------
  ! Calculate the fraction of the flux with only aerodynamic resistance
  ! (canopy evaporation).
  ! Set to 1 for negative moisture flux or snow-covered land
  ! (no surface/stomatal resistance to condensation).
  ! Calculate for a snow-free canopy then adjust for snow. With the fix,
  ! the potential fraction depends on the canopy water content.
  !-----------------------------------------------------------------------
  IF (l_fix_neg_snow) THEN
    ! Make the logic more transparent.
    IF (dq(l) >= 0.0) THEN
      ! Only aerodynamic resistance for downward fluxes.
      fracaero_t(l) = 1.0
      IF (tstar(l) > tm) THEN
        ! All downward moisture flux is added to canopy water.
        fracaero_s(l) = 0.0
      ELSE
        ! All downward moisture flux is added to snow.
        fracaero_s(l) = 1.0
      END IF
    ELSE
      ! For upward fluxes, calculate the fraction for canopy water,
      ! then apply that fraction on the snow-free part of the grid box.
      IF (catch(l) > 0.0) THEN
        fracaero_t(l) = canopy(l) / ( epdt(l) + catch(l) )
      ELSE
        fracaero_t(l) = 0.0
      END IF
      msk_sndpth = MAX(snowdep_surft(l), snow_surft(l) / rho_ice)
      IF (msk_sndpth > 0.0) THEN
        IF (frac_snow_subl_melt == 1) THEN
          msk_sndpth = maskd * msk_sndpth
          IF (msk_sndpth > SQRT(2.0 * EPSILON(msk_sndpth))) THEN
            fracaero_s(l) = 1.0 - EXP(-msk_sndpth)
          ELSE
            fracaero_s(l) = msk_sndpth
          END IF
        ELSE
          ! Snow is assumed to cover the entire canopy.
          fracaero_s(l) = 1.0
        END IF
        fracaero_t(l) = fracaero_s(l) + (1.0 - fracaero_s(l)) * fracaero_t(l)
      ELSE
        ! Set the sublimation fraction to 0.
        fracaero_s(l) = 0.0
      END IF
    END IF
  ELSE
    !   Original code
    fracaero_t(l) = 1.0
    IF (dq(l) <  0.0 .AND. snowdep_surft(l) <= 0.0) fracaero_t(l) = 0.0
    IF (dq(l) <  0.0 .AND. snowdep_surft(l) <= 0.0 .AND. catch(l) >  0.0)      &
      fracaero_t(l) = canopy(l) / ( epdt(l) + catch(l) )
    IF (snowdep_surft(l) > 0.0) THEN
      IF (frac_snow_subl_melt == 1) THEN
        IF (l_fix_snow_frac) THEN
          ! Use linear expansion of exponential if non-linear term
          ! is of order EPSILON (i.e., x^2.0/2.0 ~ EPSILON)
          IF (snowdep_surft(l) > SQRT(2.0*EPSILON(snowdep_surft))/maskd) THEN
            fracaero_t(l) = 1.0 - EXP(-maskd * snowdep_surft(l)) *             &
                         (1.0 - fracaero_t(l))
          ELSE
            fracaero_t(l) = fracaero_t(l) +                                    &
                         (1.0 - fracaero_t(l)) * maskd * snowdep_surft(l)
          END IF
        ELSE
          fracaero_t(l) = 1.0 - EXP(-maskd * snowdep_surft(l))
        END IF
      END IF
    END IF
    fracaero_t(l) = MIN(fracaero_t(l),1.0)
  END IF

  !-----------------------------------------------------------------------
  ! Calculate resistance factors for transpiration from vegetation tiles
  ! and bare soil evaporation from soil tiles.
  !-----------------------------------------------------------------------
  resfs(l) = gc(l) / ( gc(l) + ch(l) * vshr(i,j) )
  IF (l_et_stom .OR. l_et_stom_surft) THEN
    resfs_stom(l) = gc_stom_surft(l) / ( gc_stom_surft(l) + ch(l) * vshr(i,j) )
  END IF
  resft(l) = flake(l) + (1.0 - flake(l)) *                                     &
                        ( fracaero_t(l) + (1.0 - fracaero_t(l)) * resfs(l) )

END DO
!$OMP END PARALLEL DO


! RESFT < 1 for snow on canopy if canopy snow model used, so re-calculate.
! This works only if the first npft tiles are the vegetated ones.
IF ( .NOT. l_aggregate .AND. can_model == 4) THEN
  IF ( cansnowtile ) THEN
!$OMP PARALLEL DO IF(surft_pts > 1) DEFAULT(NONE) PRIVATE(i, j, k, l)          &
!$OMP          SHARED(surft_pts, surft_index, land_index, t_i_length,          &
!$OMP                 snow_surft, gc, vshr, fracaero_t,                        &
!$OMP                 resfs, ch, resft) SCHEDULE(STATIC)
    DO k = 1,surft_pts
      l = surft_index(k)
      IF (snow_surft(l) >  0.0) THEN
        j = (land_index(l) - 1) / t_i_length + 1
        i = land_index(l) - (j-1) * t_i_length
        fracaero_t(l) = 0.0
        resfs(l) = gc(l) /                                                     &
                (gc(l) + ch(l) * vshr(i,j))
        resft(l) = resfs(l)
      END IF
    END DO
!$OMP END PARALLEL DO
  END IF
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE sf_resist
END MODULE sf_resist_mod
