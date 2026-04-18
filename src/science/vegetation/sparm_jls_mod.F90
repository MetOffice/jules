! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Routine to calculate the gridbox mean land surface parameters from
! the areal fractions of the surface types and the structural
! properties of the plant functional types.
!
! This routine no longer calculates the max infiltration rate. This
! is now done in science/soil/infiltration_rate.F90 and called seperately
! as required.
!
! Future developments make calling infiltration_rate from within sparm
! undesirable (eg smcl-dependence)
!
! *********************************************************************
MODULE sparm_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SPARM_MOD'

CONTAINS

SUBROUTINE sparm (land_pts, nsurft, surft_pts, surft_index,                    &
                  frac_surft, canht_pft, lai_pft, z0m_soil_gb,                 &
                  catch_snow_surft, catch_surft, z0_surft, z0h_bare_surft,     &
                  ztm_gb)

!Use in relevant subroutines
USE pft_sparm_mod,            ONLY: pft_sparm
USE nvg_sparm_mod,            ONLY: nvg_sparm

!Use in relevant variables
USE jules_surface_types_mod,  ONLY: ice, lake, npft, ntype, soil,              & 
                                    urban_canyon, urban_roof
USE jules_vegetation_mod,     ONLY: can_model, l_spec_veg_z0
USE blend_h,                  ONLY: lb
USE jules_surface_mod,        ONLY: i_aggregate_opt, l_aggregate
USE jules_snow_mod,           ONLY: cansnowtile, i_snow_tile, snowloadlai
USE c_z0h_z0m,                ONLY: z0h_z0m
USE jules_urban_mod,          ONLY: l_moruses

USE stochastic_physics_run_mod, ONLY: l_rp2, i_rp_scheme, i_rp2b, rp_idx,      &
                                      z0hm_pft_rp, z0hm_soil_rp, z0v_rp,       &
                                      z0_soil_rp, z0_urban_mult_rp


USE parkind1,                 ONLY: jprb, jpim
USE yomhook,                  ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Number of land points to be processed.
  nsurft,                                                                      &
    ! Number of surface tiles.
  surft_pts(ntype),                                                            &
    ! Number of land points which include the nth surface type.
  surft_index(land_pts,ntype)
    ! Indices of land points which include the nth surface type.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  frac_surft(land_pts,ntype),                                                  &
    ! Fractional cover of each surface type.
  canht_pft(land_pts,npft),                                                    &
    ! Vegetation height (m).
  lai_pft(land_pts,npft),                                                      &
    ! Leaf area index.
  z0m_soil_gb(land_pts),                                                       &
    ! z0m of bare soil (m)
  ztm_gb(land_pts)
    ! Roughness length

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  catch_snow_surft(land_pts,nsurft),                                           &
    ! Snow capacity for tile (kg/m2)
  catch_surft(land_pts,nsurft),                                                &
    ! Canopy capacity for each tile (kg/m2).
  z0_surft(land_pts,nsurft),                                                   &
    ! Roughness length for each tile (m).
  z0h_bare_surft(land_pts,nsurft)
    ! Snow-free thermal roughness length for each tile (m).


!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i,j,l,n  ! Loop counters

INTEGER ::                                                                     &
  tile_pts(ntype),                                                             &
!   Number of points contining tiles of the current category
  tile_index(land_pts,ntype)
!   Index over tiles

REAL(KIND=real_jlslsm) ::                                                      &
  catch(land_pts),                                                             &
    ! GBM canopy capacity (kg/m2).
  catch_t(land_pts,ntype),                                                     &
    ! Canopy capacities for types not tiles (kg/m2); required for l_aggregate.
  fst(land_pts),                                                               & 
    ! Total fraction of tiles using a separate energy balance for snow.
  fz0(land_pts),                                                               &
    ! Aggregation function of Z0.
  fz0h(land_pts),                                                              &
    ! Aggregation function of Z0H.
  z0(land_pts),                                                                &
    ! GBM roughness length (m).
  z0h(land_pts),                                                               &
    ! GBM thermal roughness length (m).
  z0_t(land_pts,ntype)
    ! Roughness lengths for types not tiles (m); required for l_aggregate.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SPARM'

!-----------------------------------------------------------------------------
!end of header
!-----------------------------------------------------------------------------
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Set parameters for vegetated surface types
!-----------------------------------------------------------------------------
IF ( l_rp2 .AND. i_rp_scheme == i_rp2b) THEN
  DO n = 1,npft
    z0h_z0m(n) = z0hm_pft_rp(n)
  END DO
  z0h_z0m(soil) = z0hm_soil_rp(rp_idx)
END IF

IF ( .NOT. l_aggregate ) THEN
  !---------------------------------------------------------------------------
  ! Set parameters for vegetated surface types
  !---------------------------------------------------------------------------
  DO n = 1,npft
    CALL pft_sparm (land_pts, n, surft_pts(n), surft_index(:,n),               &
                    canht_pft(:,n), lai_pft(:,n), catch_surft(:,n),            &
                    z0_surft(:,n))
  END DO

  ! cansnowtile is only used when l_aggregate is .FALSE. - needs to be
  ! consistent with logic where cansnowtile is set.
  IF (can_model  ==  4 ) THEN
    DO n = 1,npft
      IF ( cansnowtile(n) ) THEN
        DO j = 1,surft_pts(n)
          l = surft_index(j,n)
          catch_snow_surft(l,n) = snowloadlai * lai_pft(l,n)
        END DO
      END IF
    END DO
  END IF

  !-----------------------------------------------------------------------------
  ! Set parameters for non-vegetated surface types
  !-----------------------------------------------------------------------------
  ! It would be nice not to have to call this every time, however, catch_surft,
  ! z0_surft gets set to zero somewhere so needs to be called.
  CALL nvg_sparm (land_pts, surft_pts, surft_index, z0m_soil_gb, ztm_gb,       &
                  catch_surft, z0_surft)
                  
  !-----------------------------------------------------------------------------
  ! Set parameters for the snow tile, number ice = ntype
  !-----------------------------------------------------------------------------
  ! A separate snow tile can only be used if this is not a land ice point and
  ! surface types are not aggregated
  IF (ANY(i_snow_tile == 1)) THEN
    fst(:) = 0
    fz0(:) = 0
    DO n = 1,ntype-1
      IF (i_snow_tile(n) == 1) THEN
        DO j = 1,surft_pts(n)
          l = surft_index(j,n)
          IF (frac_surft(l,ice) .eq. 0.0) THEN
            fst(l) = fst(l) + frac_surft(l,n)
            fz0(l) = fz0(l) + frac_surft(l,n) / (LOG(lb / z0_surft(l,n)))**2
          END IF
        END DO
      END IF
    END DO
    DO l = 1,land_pts
      IF (frac_surft(l,ice) .eq. 0.0) THEN
        catch_surft(l,ice) = 0.0
        IF (fz0(l) > EPSILON(0.0))                                             &
          z0_surft(l,ice) = lb * EXP(-SQRT(fst(l)/fz0(l)))
      END IF
    END DO
  END IF

ELSE ! l_aggregate
  !---------------------------------------------------------------------------
  ! Set parameters for vegetated surface types
  !---------------------------------------------------------------------------
  DO n = 1,npft
    CALL pft_sparm (land_pts, n, surft_pts(n), surft_index(:,n),               &
                    canht_pft(:,n), lai_pft(:,n), catch_t(:,n), z0_t(:,n))
  END DO

  !-----------------------------------------------------------------------------
  ! Set parameters for non-vegetated surface types
  !-----------------------------------------------------------------------------
  CALL nvg_sparm (land_pts, surft_pts, surft_index, z0m_soil_gb, ztm_gb,       &
                  catch_t, z0_t)

  !---------------------------------------------------------------------------
  ! Form means and copy to tile arrays if required for aggregate tiles
  !---------------------------------------------------------------------------
  DO l = 1,land_pts
    catch(l)  = 0.0
    fz0(l)    = 0.0
    fz0h(l)   = 0.0
    z0(l)     = 0.0
  END DO

  DO n = 1,ntype
    DO j = 1,surft_pts(n)
      l = surft_index(j,n)
      fz0(l) = fz0(l) + frac_surft(l,n) / (LOG(lb / z0_t(l,n)))**2
      ! Explicit aggregation of z0h if required.
      IF (i_aggregate_opt == 1) THEN
        fz0h(l) = fz0h(l) + frac_surft(l,n) /                                  &
           ( LOG(lb / z0_t(l,n)) * LOG(lb / (z0h_z0m(n) * z0_t(l,n))) )
      END IF
    END DO
  END DO

  DO l = 1,land_pts
    z0(l) = lb * EXP(-SQRT(1.0 / fz0(l)))
    ! Explicit aggregation of z0h if required.
    IF (i_aggregate_opt == 1) THEN
      z0h(l) = lb * EXP(-1.0 / (fz0h(l) * LOG(lb / z0(l))) )
    END IF
  END DO

  DO n = 1,ntype
    DO j = 1,surft_pts(n)
      l = surft_index(j,n)
      catch(l) = catch(l) + frac_surft(l,n) * catch_t(l,n)
    END DO
  END DO

  DO l = 1,land_pts
    IF ( lake > 0 ) THEN
      ! Canopy capacity is average over non-lake surface types
      IF ( frac_surft(l,lake) < 1.0 ) THEN
        catch_surft(l,1) = catch(l) / (1.0 - frac_surft(l,lake))
      ELSE
        catch_surft(l,1) = 0.0
      END IF
    ELSE
      catch_surft(l,1) = catch(l)
    END IF
    z0_surft(l,1) = z0(l)
    IF (i_aggregate_opt == 1) z0h_bare_surft(l,1) = z0h(l)
  END DO
END IF  !  l_aggregate

! Apply perturbations

IF ( l_rp2 .AND. i_rp_scheme == i_rp2b) THEN
  IF ( l_spec_veg_z0) THEN
    ! Apply perturbations to z0_surft for plant functional types
    DO i = 1, npft
      z0_surft(:,i) = z0v_rp(i)
    END DO
  END IF
  z0_surft(:,soil) = z0_soil_rp(rp_idx)

  IF ( l_moruses ) THEN
    ! Apply perturbations to z0_surft for urban roof and canyon tiles

    IF (l_aggregate) THEN
      tile_pts(1) = land_pts
      DO l = 1, land_pts
        tile_index(l,1) = l
      END DO
    ELSE
      DO n = 1, ntype
        tile_pts(n)=surft_pts(n)
        DO j = 1, surft_pts(n)
          ! Here tiles match types.
          tile_index(j,n)=surft_index(j,n)
        END DO
      END DO
    END IF

    n = urban_canyon
    DO j = 1,tile_pts(n)
      l = tile_index(j,n)
      z0_surft(l,n)          = z0_urban_mult_rp(rp_idx)*z0_surft(l,n)
      z0_surft(l,urban_roof) = z0_urban_mult_rp(rp_idx)*z0_surft(l,urban_roof)
    END DO
  END IF
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE sparm
END MODULE sparm_mod
