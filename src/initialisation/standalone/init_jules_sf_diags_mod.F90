#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_jules_sf_diags_mod

!-----------------------------------------------------------------------------
! Description:
!   Set switches and allocate memory for surface diagnostics (sf_diag).
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

PRIVATE  ! Private scope by default
PUBLIC set_sf_diag_switches, allocate_sf_diags

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName = 'INIT_JULES_SF_DIAGS_MOD'

CONTAINS

!#############################################################################
SUBROUTINE set_sf_diag_switches

!-----------------------------------------------------------------------------
! Description:
!   Set switches for surface diagnostics (sf_diag) where the science code
!   requires this (because some of the science code uses components of
!   sf_diag to pass information).
!-----------------------------------------------------------------------------

USE jules_radiation_mod, ONLY: i_sea_alb_method

USE jules_vegetation_mod, ONLY: l_crop, l_fao_ref_evapotranspiration,          &
                                l_inferno

USE sf_diags_mod, ONLY: sf_diag

! Non-science modules.
USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SET_SF_DIAG_SWITCHES'

!End of header
!-----------------------------------------------------------------------------
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF ( l_crop ) THEN
  ! Set flags needed to provide variables to the crop code.
  sf_diag % st1p5 = .TRUE.
END IF

IF ( l_inferno ) THEN
  ! Set flags needed to provide variables to INFERNO.
  sf_diag % sq1p5 = .TRUE.
  sf_diag % st1p5 = .TRUE.
END IF

IF ( l_fao_ref_evapotranspiration ) THEN
  ! Set flags needed to provide variables.
  sf_diag % sq1p5 = .TRUE.
  sf_diag % st1p5 = .TRUE.
  sf_diag % su10  = .TRUE.
  sf_diag % sv10  = .TRUE.
END IF

IF ( i_sea_alb_method == 3 ) THEN
  ! Set flags needed in the calculation of 10m windspeed.
  sf_diag % su10 = .TRUE.
  sf_diag % sv10 = .TRUE.
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN
END SUBROUTINE set_sf_diag_switches

!#############################################################################

SUBROUTINE allocate_sf_diags

!-----------------------------------------------------------------------------
! Description:
!   Allocate components of the surface diagnostics (sf_diag) as required for
!   the current configuration.
!-----------------------------------------------------------------------------

USE ancil_info, ONLY: land_pts, nsurft

USE jules_sea_seaice_mod, ONLY: nice, nice_use

USE jules_soil_mod, ONLY: sm_levels

USE sf_diags_mod, ONLY: sf_diag

USE theta_field_sizes, ONLY: t_i_length, t_j_length

! Non-science modules.
USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
USE ereport_mod, ONLY: ereport

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  ERROR,                                                                       &
    ! Variable for trapping the error from each individual call to allocate.
  error_sum,                                                                   &
    ! Variable to track the sum of all errors resulting from calls to
    ! allocate.
  errcode
    ! Variable to use in error report.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='ALLOCATE_SF_DIAGS'

!End of header
!-----------------------------------------------------------------------------
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Set the value reported for the error code and initialise sum of error
! values.
errcode   = 101
error_sum = 0

!-----------------------------------------------------------------------------
! Set flags that depend on others.
!-----------------------------------------------------------------------------
sf_diag % sq_t1p5  = ( sf_diag%sq1p5 .OR. sf_diag%st1p5 )
sf_diag % suv10m_n = ( sf_diag%l_u10m_n .OR. sf_diag%l_v10m_n )

!-----------------------------------------------------------------------------
! Arrays are only allocated at full size if required;
! otherwise allocated a minimal size to ensure a valid address is
! assigned in subroutine calls. In some cases this might only be precautionary
! in that the arrays are not currently used as arguments.
!
! Arrays are presented here in the order in which the related flags are
! declared in sf_diags_mod.
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! x-cpt of wind at 10m.
!-----------------------------------------------------------------------------
IF ( sf_diag % su10 ) THEN
  ALLOCATE( sf_diag%u10m(t_i_length,t_j_length), STAT = ERROR )
  error_sum = error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%u10m(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%u10m(:,:)       = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % u10m.",                       &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! y-cpt of wind at 10m.
!-----------------------------------------------------------------------------
IF ( sf_diag % sv10 ) THEN
  ALLOCATE( sf_diag%v10m(t_i_length,t_j_length), STAT = ERROR )
  error_sum = error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%v10m(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%v10m(:,:)       = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % v10m.",                       &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Specific humidity at 1.5m.
!-----------------------------------------------------------------------------
IF ( sf_diag % sq1p5 ) THEN
  ALLOCATE( sf_diag%q1p5m(t_i_length,t_j_length), STAT = ERROR )
  error_sum = error_sum + ERROR
  ALLOCATE( sf_diag%q1p5m_ssi(t_i_length,t_j_length), STAT = ERROR )
  error_sum = error_sum + ERROR
  ALLOCATE( sf_diag%q1p5m_surft(land_pts,nsurft), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%q1p5m(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
  ALLOCATE( sf_diag%q1p5m_ssi(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
  ALLOCATE( sf_diag%q1p5m_surft(1,1), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%q1p5m(:,:)       = 0.0
  sf_diag%q1p5m_ssi(:,:)   = 0.0
  sf_diag%q1p5m_surft(:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag specific humidity at 1.5m.",    &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Temperature at 1.5m.
!-----------------------------------------------------------------------------
IF ( sf_diag % st1p5 ) THEN
  ALLOCATE( sf_diag%t1p5m(t_i_length,t_j_length), STAT = ERROR )
  error_sum = error_sum + ERROR
  ALLOCATE( sf_diag%t1p5m_ssi(t_i_length,t_j_length), STAT = ERROR )
  error_sum = error_sum + ERROR
  ALLOCATE( sf_diag%t1p5m_surft(land_pts,nsurft), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%t1p5m(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
  ALLOCATE( sf_diag%t1p5m_ssi(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
  ALLOCATE( sf_diag%t1p5m_surft(1,1), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%t1p5m(:,:)       = 0.0
  sf_diag%t1p5m_ssi(:,:)   = 0.0
  sf_diag%t1p5m_surft(:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag temperature at 1.5m.",          &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Wind mixing energy.
!-----------------------------------------------------------------------------
IF ( sf_diag % sfme ) THEN
  ALLOCATE (sf_diag%fme(t_i_length,t_j_length), STAT = ERROR)
  error_sum = error_sum + ERROR
ELSE
  ALLOCATE (sf_diag%fme(1,1), STAT = ERROR)
  error_sum = error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%fme(:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % fme. ",                       &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Gridbox Mean  Momentum flux.
!-----------------------------------------------------------------------------
IF ( sf_diag % l_tau_1 ) THEN
  ALLOCATE( sf_diag%tau_1(t_i_length,t_j_length), STAT = ERROR )
  error_sum = error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%tau_1(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%tau_1(:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % tau_1.",                      &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Tiled Momentum flux.
!-----------------------------------------------------------------------------
IF ( sf_diag % l_tau_surft ) THEN
  ALLOCATE( sf_diag%tau_surft(land_pts,nsurft), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%tau_surft(1,1), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%tau_surft(:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % tau_surft.",                  &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Latent heat flux.
!-----------------------------------------------------------------------------
IF ( sf_diag % slh ) THEN
  ALLOCATE( sf_diag%latent_heat(t_i_length,t_j_length), STAT = ERROR )
  error_sum = error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%latent_heat(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%latent_heat(:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % latent_heat. ",               &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Sea ice surface melt heat flux.
!-----------------------------------------------------------------------------
IF ( sf_diag % simlt ) THEN
  ALLOCATE( sf_diag%sice_mlt_htf(t_i_length,t_j_length,nice), STAT = ERROR )
  error_sum = error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%sice_mlt_htf(1,1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%sice_mlt_htf(:,:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % sice_mlt_htf. ",              &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Snowmelt surface heat flux.
!-----------------------------------------------------------------------------
IF ( sf_diag % smlt ) THEN
  ALLOCATE( sf_diag%snomlt_surf_htf(t_i_length,t_j_length), STAT = ERROR )
  error_sum = error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%snomlt_surf_htf(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%snomlt_surf_htf(:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % snomlt_surf_htf. ",           &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Components of neutral wind at 10m and pseudostresses.
!-----------------------------------------------------------------------------
IF ( sf_diag % suv10m_n ) THEN
  ALLOCATE( sf_diag%u10m_n(t_i_length,t_j_length), STAT = ERROR )
  error_sum = error_sum + ERROR
  ALLOCATE( sf_diag%v10m_n(t_i_length,t_j_length), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
  ALLOCATE( sf_diag%mu10m_n(t_i_length,t_j_length), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
  ALLOCATE( sf_diag%mv10m_n(t_i_length,t_j_length), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%u10m_n(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
  ALLOCATE( sf_diag%v10m_n(1,1), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
  ALLOCATE( sf_diag%mu10m_n(1,1), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
  ALLOCATE( sf_diag%mv10m_n(1,1), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%u10m_n(:,:)  = 0.0
  sf_diag%v10m_n(:,:)  = 0.0
  sf_diag%mu10m_n(:,:) = 0.0
  sf_diag%mv10m_n(:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating neutral winds at 10m.",                 &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! LW radiation on tiles.
!-----------------------------------------------------------------------------
IF ( sf_diag % l_lw_surft ) THEN
  ALLOCATE( sf_diag%lw_up_surft(land_pts,nsurft), STAT = ERROR )
  error_sum = error_sum + ERROR
  ALLOCATE( sf_diag%lw_down_surft(land_pts,nsurft), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%lw_down_surft(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
  ALLOCATE( sf_diag%lw_up_surft(1,1), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%lw_down_surft(:,:) = 0.0
  sf_diag%lw_up_surft(:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating LW fluxes. ",                           &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Temperature at 10m over sea/sea-ice.
!-----------------------------------------------------------------------------
IF ( sf_diag % l_t10m ) THEN
  ALLOCATE( sf_diag%t10m(t_i_length,t_j_length), STAT = ERROR )
  error_sum = error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%t10m(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%t10m(:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % t10m.",                       &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Specific humidity at 10m over sea/sea-ice.
!-----------------------------------------------------------------------------
IF ( sf_diag % l_q10m ) THEN
  ALLOCATE( sf_diag%q10m(t_i_length,t_j_length), STAT = ERROR )
  error_sum = error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%q10m(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%q10m(:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % q10m.",                       &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! CH at 10m over sea/sea-ice.
!-----------------------------------------------------------------------------
IF ( sf_diag % l_t10m .OR. sf_diag % l_q10m ) THEN
  ALLOCATE( sf_diag%chr10m(t_i_length,t_j_length), STAT = ERROR )
  error_sum = error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%chr10m(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%chr10m(:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % chr10m.",                     &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Category ice area weighted sea ice surface skin temperature.
!-----------------------------------------------------------------------------
IF ( sf_diag % l_tstar_sice_weighted_cat ) THEN
  ALLOCATE( sf_diag%tstar_sice_weighted_cat(t_i_length,t_j_length,nice_use),   &
           STAT = ERROR )
  error_sum = error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%tstar_sice_weighted_cat(1,1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%tstar_sice_weighted_cat(:,:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % tstar_sice_weighted_cat.",    &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Category ice area weighted upward LW flux over sea ice after boundary
! layer calculation.
!-----------------------------------------------------------------------------
IF ( sf_diag % l_lw_up_sice_weighted_cat ) THEN
  ALLOCATE( sf_diag%lw_up_sice_weighted_cat(t_i_length,t_j_length,nice_use),   &
            STAT = ERROR )
  error_sum = error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%lw_up_sice_weighted_cat(1,1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%lw_up_sice_weighted_cat(:,:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % l_lw_up_sice_weighted_cat.",  &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Category sea ice time fraction.
!-----------------------------------------------------------------------------
IF ( sf_diag % l_ice_present_cat ) THEN
  ALLOCATE( sf_diag%ice_present_cat(t_i_length,t_j_length,nice_use),           &
           STAT = ERROR )
  error_sum = error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%ice_present_cat(1,1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%ice_present_cat(:,:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % ice_present_cat.",            &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Ice area weighted sea ice surface skin temperature.
!-----------------------------------------------------------------------------
IF ( sf_diag % l_tstar_sice_weighted ) THEN
  ALLOCATE( sf_diag%tstar_sice_weighted(t_i_length,t_j_length),                &
           STAT = ERROR )
  error_sum = error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%tstar_sice_weighted(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%tstar_sice_weighted(:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % tstar_sice_weighted.",        &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Ice area weighted upward LW flux over sea ice after boundary
! layer calculation.
!-----------------------------------------------------------------------------
IF ( sf_diag % l_lw_up_sice_weighted ) THEN
  ALLOCATE( sf_diag%lw_up_sice_weighted(t_i_length,t_j_length),                &
           STAT = ERROR )
  error_sum = error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%lw_up_sice_weighted(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%lw_up_sice_weighted(:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % l_lw_up_sice_weighted.",      &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Sea ice time fraction.
!-----------------------------------------------------------------------------
IF ( sf_diag % l_ice_present ) THEN
  ALLOCATE( sf_diag%ice_present(t_i_length,t_j_length),                        &
           STAT = ERROR )
  error_sum = error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%ice_present(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%ice_present(:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % ice_present.",                &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Aggregate ice area weighted sensible heat flux over sea ice.
!-----------------------------------------------------------------------------
IF ( sf_diag % l_ftl_ice_sm ) THEN
  ALLOCATE( sf_diag%ftl_ice_sm(t_i_length,t_j_length),                         &
           STAT = ERROR )
  error_sum = error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%ftl_ice_sm(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%ftl_ice_sm(:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % ftl_ice_sm.",                 &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Sea and sea ice drag coefficient (momentum).
!-----------------------------------------------------------------------------
IF ( sf_diag % l_cd_ssi ) THEN
  ALLOCATE( sf_diag%cd_ssi(t_i_length,t_j_length), STAT = ERROR )
  error_sum = error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%cd_ssi(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%cd_ssi(:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % cd_ssi.",                     &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Sea and sea ice drag coefficient (heat).
!-----------------------------------------------------------------------------
IF ( sf_diag % l_ch_ssi ) THEN
  ALLOCATE( sf_diag%ch_ssi(t_i_length,t_j_length), STAT = ERROR )
  error_sum = error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%ch_ssi(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%ch_ssi(:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % ch_ssi.",                     &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Latent heat land.
!-----------------------------------------------------------------------------
IF ( sf_diag % l_lh_land ) THEN
  ALLOCATE( sf_diag%lh_land(land_pts), STAT = ERROR )
  error_sum = error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%lh_land(1), STAT = ERROR )
  error_sum = error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%lh_land(:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % lh_land.",                    &
                errcode, " Check " // TRIM(RoutineName) )
END IF


!-----------------------------------------------------------------------------
! Latent heat sea and sea ice.
!-----------------------------------------------------------------------------
IF ( sf_diag % l_lh_ssi ) THEN
  ALLOCATE( sf_diag%lh_ssi(t_i_length,t_j_length), STAT = ERROR )
  error_sum = error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%lh_ssi(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%lh_ssi(:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % lh_ssi.",                     &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Diagnostics for JULES snow.
!-----------------------------------------------------------------------------
IF ( sf_diag % l_snice ) THEN
  ALLOCATE( sf_diag%snice_smb_surft(land_pts,nsurft), STAT = ERROR )
  error_sum = error_sum + ERROR
  ALLOCATE( sf_diag%snice_m_surft(land_pts,nsurft), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
  ALLOCATE( sf_diag%snice_freez_surft(land_pts,nsurft), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
  ALLOCATE( sf_diag%snice_sicerate_surft(land_pts,nsurft), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
  ALLOCATE( sf_diag%snice_sliqrate_surft(land_pts,nsurft), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
  ALLOCATE( sf_diag%snice_runoff_surft(land_pts,nsurft), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%snice_smb_surft(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
  ALLOCATE( sf_diag%snice_m_surft(1,1), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
  ALLOCATE( sf_diag%snice_freez_surft(1,1), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
  ALLOCATE( sf_diag%snice_sicerate_surft(1,1), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
  ALLOCATE( sf_diag%snice_sliqrate_surft(1,1), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
  ALLOCATE( sf_diag%snice_runoff_surft(1,1), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%snice_smb_surft(:,:)      = 0.0
  sf_diag%snice_m_surft(:,:)        = 0.0
  sf_diag%snice_freez_surft(:,:)    = 0.0
  sf_diag%snice_sicerate_surft(:,:) = 0.0
  sf_diag%snice_sliqrate_surft(:,:) = 0.0
  sf_diag%snice_runoff_surft(:,:)   = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag for snow. ",                    &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Transpiration fluxes.
! et_stom_surft is also used with et_stom_ij.
!-----------------------------------------------------------------------------
IF ( sf_diag % l_et_stom .OR. sf_diag % l_et_stom_surft ) THEN
  ALLOCATE( sf_diag%et_stom_surft(land_pts,nsurft), STAT = ERROR )
  error_sum = error_sum + ERROR
  ALLOCATE( sf_diag%et_stom_ij(t_i_length,t_j_length), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
  ALLOCATE( sf_diag%resfs_stom(land_pts,nsurft), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%et_stom_surft(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
  ALLOCATE( sf_diag%et_stom_ij(1,1), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
  ALLOCATE( sf_diag%resfs_stom(1,1), STAT = ERROR )
  error_sum = error_sum + error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%et_stom_surft(:,:) = 0.0
  sf_diag%et_stom_ij(:,:)    = 0.0
  sf_diag%resfs_stom(:,:)    = 0.0
ELSE
  CALL ereport( "Error when allocating transpiration fluxes.",                 &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Soil moisture rate modifier of soil respiration.
!-----------------------------------------------------------------------------
IF ( sf_diag % l_fsth ) THEN
  ALLOCATE( sf_diag%fsth(land_pts,sm_levels), STAT = ERROR )
  error_sum = error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%fsth(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%fsth(:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % fsth.",                       &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Temperature rate modifier of soil respiration.
!-----------------------------------------------------------------------------
IF ( sf_diag % l_ftemp ) THEN
  ALLOCATE( sf_diag%ftemp(land_pts,sm_levels), STAT = ERROR )
  error_sum = error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%ftemp(1,1), STAT = ERROR )
  error_sum = error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%ftemp(:,:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % ftemp.",                      &
                errcode, " Check " // TRIM(RoutineName) )
END IF

!-----------------------------------------------------------------------------
! Soil respiration rate modifier due to vegetation cover.
!-----------------------------------------------------------------------------
IF ( sf_diag % l_fprf ) THEN
  ALLOCATE( sf_diag%fprf(land_pts), STAT = ERROR )
  error_sum = error_sum + ERROR
ELSE
  ALLOCATE( sf_diag%fprf(1), STAT = ERROR )
  error_sum = error_sum + ERROR
END IF
IF ( error_sum == 0 ) THEN
  sf_diag%fprf(:) = 0.0
ELSE
  CALL ereport( "Error when allocating sf_diag % fpft.",                       &
                errcode, " Check " // TRIM(RoutineName) )
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN
END SUBROUTINE allocate_sf_diags

END MODULE init_jules_sf_diags_mod
#endif
