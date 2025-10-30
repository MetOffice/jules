!******************************COPYRIGHT****************************************
! (c) British Antarctic Survey and University of Bristol, 2023.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT****************************************

MODULE sf_ssi_imp_wtrac_mod

!-----------------------------------------------------------------------------
! Description:
!   Calculate final water tracer surface fluxes over sea and sea ice.
!
! Method:
!   Update the explicit water tracer surface fluxes to account for
!   changes in the normal water flux due to:
!   a) the implicit calculation (in sf_im_pt2)
!   b) any change in temperature caused by surface melt (sf_melt) (only
!      applicable for sea ice).
!
!   Water tracers are updated here based on the assumption that the surface
!   flux ratio of water tracer to water remains constant between the explicit
!   and implicit schemes for each evaporative source type.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in Surface
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!
!-----------------------------------------------------------------------------

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SF_SSI_IMP_WTRAC_MOD'

CONTAINS

SUBROUTINE sf_ssi_imp_wtrac (                                                  &
 ssi_pts, sea_pts, ssi_index, sea_index, std_ratio, tile_frac, fssi, fqw,      &
 fqw_wtrac, fqw_norm_wtrac, fqw_1_wtrac, dfqw_1_wtrac, e_wtrac)

USE atm_fields_bounds_mod,    ONLY: tdims
USE theta_field_sizes,        ONLY: t_i_length
USE jules_water_tracers_mod,  ONLY: min_q_ratio

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
 ssi_pts                                                                       &
                       ! IN Total number of sea or sea-ice points
,sea_pts                                                                       &
                       ! IN Number of sea points.
,ssi_index(ssi_pts)                                                            &
                       ! IN Index of sea and sea-ice points.
,sea_index(ssi_pts)
                       ! IN Index of sea points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 std_ratio             ! IN Standard water tracer to water ratio

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 tile_frac(ssi_pts)                                                            &
                       ! IN 'Tile' fraction so leads fraction if ocean call
                       !    or sea ice category fraction if sea ice call
,fssi(ssi_pts)                                                                 &
                       ! IN Non-land fraction of grid box

,fqw(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                      &
                       ! IN Final normal water surface flux at end of implicit
                       !    scheme
,fqw_wtrac(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                &
                       ! IN Explicit water tracer surface flux for 'tile' (i.e.
                       !        open ocean or sea ice category)
,fqw_norm_wtrac(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                       ! IN Explicit NORMAL water tracer surface flux for
                       !   'tile' (i.e. open ocean or sea ice category)

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
 fqw_1_wtrac(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)              &
                       ! IN OUT GBM water tracer surface flux
,dfqw_1_wtrac(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                       ! IN OUT GBM increment to water tracer surface flux

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 e_wtrac(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                       ! OUT    Water tracer evaporation for 'tile' (i.e.
                       !        open ocean or sea ice category)


! Local variables

INTEGER ::                                                                     &
 i,j                                                                           &
             ! Horizontal field index
,k                                                                             &
             ! Point counter
,l
             ! Sea or sea ice field index

REAL(KIND=real_jlslsm) ::                                                      &
 fqw_ratio
             ! Ratio of water tracer flux to normal water flux from
             ! explicit calculation

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SF_SSI_IMP_WTRAC'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Initialise fields
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE)                               &
!$OMP PRIVATE(i,j)                                                             &
!$OMP SHARED(tdims, e_wtrac)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    e_wtrac(i,j) = 0.0
  END DO
END DO
!$OMP END PARALLEL DO

!$OMP PARALLEL DO IF(sea_pts > 1)                                              &
!$OMP SCHEDULE(STATIC) DEFAULT(NONE)                                           &
!$OMP PRIVATE(l,k,i,j,fqw_ratio)                                               &
!$OMP SHARED(sea_pts,sea_index,ssi_index,t_i_length,fqw_wtrac,fqw_norm_wtrac,  &
!$OMP        fqw,dfqw_1_wtrac,fqw_1_wtrac,e_wtrac,tile_frac,fssi,std_ratio)
DO k = 1,sea_pts
  l = sea_index(k)
  j=(ssi_index(l) - 1) / t_i_length + 1
  i = ssi_index(l) - (j-1) * t_i_length

  ! Calculate ratio of the moisture fluxes for water tracers to
  ! moisture fluxes for normal water which were calculated in the explicit
  ! scheme.

  IF (ABS(fqw_norm_wtrac(i,j)) > min_q_ratio) THEN
    fqw_ratio = fqw_wtrac(i,j) / fqw_norm_wtrac(i,j)
  ELSE
    fqw_ratio = std_ratio
  END IF

  ! Update explicit fluxes to account for the normal water flux changing
  ! in the implicit scheme.
  !
  ! Assumption here is that the ratio of isotopic evaporative fluxes to normal
  ! water evaporative fluxes remains UNCHANGED.
  !
  ! This is new_wtrac_flux = old_wtrac_flux
  !                           + ratio * (new_water_flux - old_water_flux)
  ! where ratio = old_wtrac_flux/old_water_flux
  !
  ! which can be simplified to new_wtrac_flux = ratio * new_water_flux

  e_wtrac(i,j) = fqw_ratio * fqw(i,j)

  ! Add new flux to GBM fields
  ! (where tile_frac = leads fraction for ocean call
  !                  = sea ice fraction for sea ice call)
  dfqw_1_wtrac(i,j) = dfqw_1_wtrac(i,j)                                        &
                           + (e_wtrac(i,j) - fqw_wtrac(i,j))                   &
                              * tile_frac(l) * fssi(l)
  fqw_1_wtrac(i,j)  = fqw_1_wtrac(i,j)                                         &
                           +  e_wtrac(i,j) * tile_frac(l) * fssi(l)

END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN
END SUBROUTINE sf_ssi_imp_wtrac
END MODULE sf_ssi_imp_wtrac_mod
