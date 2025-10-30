#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_parms_mod
CONTAINS
SUBROUTINE init_parms(psparms,ainfo,urban_param,progs,coast,jules_vars,fluxes)

  !Use in relevant subroutines
USE sparm_mod, ONLY: sparm
USE infiltration_rate_mod, ONLY: infiltration_rate

USE theta_field_sizes, ONLY: t_i_length, t_j_length

USE jules_surface_mod, ONLY: l_aggregate

USE jules_sea_seaice_mod, ONLY: nice

USE ancil_info, ONLY: land_pts, nsurft, sea_pts, sice_pts, ssi_pts

USE coastal, ONLY: flandg

!TYPE definitions
USE prognostics, ONLY: progs_type
USE p_s_parms, ONLY: psparms_type
USE ancil_info, ONLY: ainfo_type
USE urban_param_mod, ONLY: urban_param_type
USE coastal, ONLY: coastal_type
USE jules_vars_mod, ONLY: jules_vars_type
USE fluxes_mod, ONLY: fluxes_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises various variables that may change their initialisation in
!   future versions
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
!arguments
!TYPES containing the data
TYPE(progs_type), INTENT(IN OUT) :: progs
TYPE(psparms_type), INTENT(IN OUT) :: psparms
TYPE(ainfo_type), INTENT(IN OUT) :: ainfo
TYPE(urban_param_type), INTENT(IN OUT) :: urban_param
TYPE(coastal_type), INTENT(IN OUT) :: coast
TYPE(jules_vars_type), INTENT(IN OUT) :: jules_vars
TYPE(fluxes_type), INTENT(IN OUT) :: fluxes

! Work variables
INTEGER :: i,j,l,n  ! Loop counters


!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Calculate surface parameters.
!-----------------------------------------------------------------------------
CALL sparm(land_pts, nsurft, ainfo%surft_pts,                                  &
           ainfo%surft_index, ainfo%frac_surft,                                &
           progs%canht_pft, progs%lai_pft, psparms%z0m_soil_gb,                &
           psparms%catch_snow_surft, psparms%catch_surft, psparms%z0_surft,    &
           psparms%z0h_bare_surft, urban_param%ztm_gb)

CALL infiltration_rate(land_pts, nsurft, ainfo%surft_pts, ainfo%surft_index,   &
                       psparms%satcon_soilt, ainfo%frac_surft,                 &
                       psparms%infil_surft)

!-----------------------------------------------------------------------
! Set up index for sea and sea-ice
!-----------------------------------------------------------------------
ssi_pts = 0
ainfo%ssi_index(:) = 0
DO j = 1,t_j_length
  DO i = 1,t_i_length
    ssi_pts = ssi_pts + 1
    IF ( flandg(i,j) < 1.0 ) THEN
      ainfo%ssi_index(ssi_pts) = (j-1) * t_i_length + i
    END IF
    ainfo%fssi_ij(i,j) = 1.0 - flandg(i,j)
  END DO
END DO

!-----------------------------------------------------------------------
! Set sea ice fraction.
!-----------------------------------------------------------------------
DO j = 1,t_j_length
  DO i = 1,t_i_length
    ainfo%ice_fract_ij(i,j) = 0.0
    coast%tstar_sice_ij(i,j) = 0.0
    DO n = 1,nice
      ainfo%ice_fract_ij(i,j) = ainfo%ice_fract_ij(i,j) +                      &
                                    ainfo%ice_fract_ncat_sicat(i,j,n)
    END DO
    IF (ainfo%ice_fract_ij(i,j) > 0.0) THEN
      DO n = 1,nice  !assuming nice=nice_use here
        coast%tstar_sice_ij(i,j) = coast%tstar_sice_ij(i,j)                    &
                  + ainfo%ice_fract_ncat_sicat(i,j,n)                          &
                  * coast%tstar_sice_sicat(i,j,n) / ainfo%ice_fract_ij(i,j)
      END DO
    END IF
  END DO
END DO

!-----------------------------------------------------------------------
! Initialise sea and sea-ice indices
!-----------------------------------------------------------------------
sea_pts  = 0
sice_pts = 0
ainfo%sea_index(:)  = 0
ainfo%sice_index(:) = 0
ainfo%sice_frac(:) = 0.0
ainfo%sea_frac(:)  = 0.0
DO l = 1,ssi_pts
  j = (ainfo%ssi_index(l) - 1) / t_i_length + 1
  i = ainfo%ssi_index(l) - (j-1) * t_i_length
  IF ( ainfo%ssi_index(l) > 0 ) THEN
    IF ( ainfo%ice_fract_ij(i,j) > 0.0 ) THEN
      sice_pts = sice_pts + 1
      ainfo%sice_index(sice_pts) = l
      ainfo%sice_frac(l) = ainfo%ice_fract_ij(i,j)
    END IF
    IF ( ainfo%ice_fract_ij(i,j) < 1.0 ) THEN
      sea_pts = sea_pts + 1
      ainfo%sea_index(sea_pts) = l
      ainfo%sea_frac(l) = 1.0 - ainfo%sice_frac(l)
    END IF
  END IF
END DO

ainfo%sice_pts_ncat(:) = 0
ainfo%sice_index_ncat(:,:) = 0
ainfo%sice_frac_ncat(:,:) = 0.0
DO n = 1,nice
  DO l = 1,ssi_pts
    j = (ainfo%ssi_index(l) - 1) / t_i_length + 1
    i = ainfo%ssi_index(l) - (j-1) * t_i_length
    IF ( ainfo%ssi_index(l) > 0 ) THEN
      IF ( ainfo%ice_fract_ncat_sicat(i,j,n) > 0.0 ) THEN
        ainfo%sice_pts_ncat(n) = ainfo%sice_pts_ncat(n) + 1
        ainfo%sice_index_ncat(ainfo%sice_pts_ncat(n),n) = l
        ainfo%sice_frac_ncat(l,n) =                                            &
          ainfo%ice_fract_ncat_sicat(i,j,n)
      END IF
    END IF
  END DO
END DO

!-----------------------------------------------------------------------
! Set up gridbox "prognostics".
!-----------------------------------------------------------------------

fluxes%tstar_ij(:,:)     = 0.0
coast%tstar_land_ij(:,:) = 0.0
coast%tstar_ssi_ij(:,:)  = 0.0

DO l = 1,land_pts
  j = (ainfo%land_index(l) - 1) / t_i_length + 1
  i = ainfo%land_index(l) - (j-1) * t_i_length
  IF ( l_aggregate ) THEN
    coast%tstar_land_ij(i,j) = progs%tstar_surft(l,1)
  ELSE
    DO n = 1,nsurft
      coast%tstar_land_ij(i,j) = coast%tstar_land_ij(i,j) +                    &
                           ainfo%frac_surft(l,n) * progs%tstar_surft(l,n)
    END DO
  END IF
END DO

coast%tstar_ssi_ij(:,:) = (1.0 - ainfo%ice_fract_ij(:,:))                      &
               * coast%tstar_sea_ij(:,:) + ainfo%ice_fract_ij(:,:)             &
               * coast%tstar_sice_ij(:,:)
fluxes%tstar_ij(:,:) = flandg(:,:) * coast%tstar_land_ij(:,:)                  &
           + (1.0 - flandg(:,:)) * coast%tstar_ssi_ij(:,:)

!-----------------------------------------------------------------------------
! Set up information on U, V and T grids (assume that att grids are the same)
!-----------------------------------------------------------------------------
jules_vars%dtrdz_charney_grid_1_ij(:,:) = 0.0

RETURN

END SUBROUTINE init_parms
END MODULE init_parms_mod
#endif
