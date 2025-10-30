! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE jules_griddiag_sf_explicit_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
                  ModuleName='JULES_GRIDDIAG_SF_EXPLICIT_MOD'

CONTAINS
!  SUBROUTINE JULES_GRIDDIAG_SF_EXPLICIT -----------------------------
!
!  Purpose: Calculate explicit grid diagnostics and other parameters
!           required elsewhere in the code
!
!
!  Documentation: UMDP 24.
!
!---------------------------------------------------------------------
!    Arguments :-
SUBROUTINE jules_griddiag_sf_explicit (                                        &
! IN values defining field dimensions and subset to be processed :
 land_pts, nice_use,                                                           &
! IN parameters required from boundary-layer scheme :
 bq_1,bt_1,z1_uv,                                                              &
! IN soil/vegetation/land surface data :
 land_index,nsurft,ho2r2_orog, flandg,sil_orog_land,                           &
! IN everything not covered so far :
 pstar,rhostar_land,rhostar_ssi,zh,tstar,l_aero_classic,                       &
! IN data :
 z0msea,cdr10m,                                                                &
! IN Diagnostic not requiring STASH flags :
 fqw_1,ftl_1,                                                                  &
! IN variables for message passing
 rhokm_land, rhokm_ssi,                                                        &
! IN data required elsewhere in boundary layer or surface code
 rhokh_surft,rhokh_sice_ncats,rhokh_sea,z0h_surft, z0m_surft,                  &
 vshr_land,vshr_ssi,surft_index,surft_pts,tile_frac,                           &
! IN required for classic aerosols
 cd_ssi,ch_ssi,rhokh_1_sice,rib_sea,z0h_sea,                                   &
 cd_land,rib_ice,rib_surft,z0m_ice,z0h_ice,ice_fract,                          &
 ch_surft_classic,cd_std_classic,                                              &
! INOUT data
 rhostar,                                                                      &
! INOUT diagnostics
 sf_diag,                                                                      &
! OUT Diagnostic not requiring STASH flags :
 rhokm_1,rib,                                                                  &
! OUT data required for tracer mixing :
 rho_aresist,aresist,resist_b,rho_aresist_surft,aresist_surft,resist_b_surft,  &
! OUT data required for mineral dust scheme
 r_b_dust,cd_std_dust,                                                         &
! OUT data required elsewhere in UM system :
 fb_surf,u_s,blendht,                                                          &
! OUT data required elsewhere in boundary layer or surface code
 rhokh,h_blend_orog,z0hssi,z0mssi,z0m_eff,                                     &
!ancil_info (IN)
 ssi_index, sice_index_ncat, sice_frac_ncat, sice_pts_ncat,                    &
!jules_internal
 unload_backgrnd_pft                                                           &
 )

USE sf_aero_mod, ONLY: sf_aero
USE theta_field_sizes, ONLY: t_i_length
USE dust_param, ONLY: ndiv
USE planet_constants_mod, ONLY: g
USE jules_science_fixes_mod, ONLY: l_accurate_rho
USE jules_surface_types_mod, ONLY: ntype, npft

USE sf_diags_mod, ONLY: strnewsfdiag

#if defined(UM_JULES)
USE dust_parameters_mod, ONLY: l_dust, l_dust_diag
#else
USE switches, ONLY: l_dust
#endif

USE bl_option_mod, ONLY: max_stress_grad

USE atm_fields_bounds_mod, ONLY:                                               &
   pdims_s, pdims, tdims

USE blend_h, ONLY: lb

USE jules_snow_mod, ONLY: cansnowtile                                          &
                          ,unload_rate_cnst                                    &
                          ,unload_rate_u

USE jules_surface_mod, ONLY: formdrag, effective_z0, h_blend_min

USE sf_orog_gb_mod, ONLY: sf_orog_gb

USE jules_sea_seaice_mod,     ONLY: nice
USE theta_field_sizes,        ONLY: t_i_length, t_j_length

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE
!-----------------------------------------------------------------------
!  Inputs :-
!-----------------------------------------------------------------------
! (a) Defining horizontal grid and subset thereof to be processed.
!    Checked for consistency.
INTEGER, INTENT(IN) ::                                                         &
 land_pts,                                                                     &
            ! IN No of land points being processed.
 nice_use   ! No. of sea ice categories used fully in surface calculations

! Defining vertical grid of model atmosphere.
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 bq_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                             ! IN A buoyancy parameter
                             !    (beta q tilde).
,bt_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                             ! IN A buoyancy parameter
                             !    (beta T tilde).
,z1_uv(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! IN Height of lowest uv level (m).

INTEGER, INTENT(IN) ::                                                         &
 land_index(land_pts)        ! IN LAND_INDEX(I)=J => the Jth
                             !    point in ROW_LENGTH,ROWS is the
                             !    land point.

INTEGER, INTENT(IN) ::                                                         &
 nsurft

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 sil_orog_land(land_pts)                                                       &
                             ! IN Silhouette area of unresolved
                             !    orography per unit horizontal
                             !    area on land points only.
,ho2r2_orog(land_pts)                                                          &
                             ! IN Standard Deviation of orography.
                             !    equivilent to peak to trough
                             !    height of unresolved orography
,flandg(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! IN Land fraction on all tiles.
                             !    divided by 2SQRT(2) on land
                             !    points only (m)

! (f) Atmospheric + any other data not covered so far, incl control.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 pstar(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end)                    &
                             ! IN Surface pressure (Pascals).
,rhostar_land(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)             &
                             ! IN Surface air density over land
,rhostar_ssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)              &
                             ! IN Surface air density over sea/sea-ice
,zh(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                       &
                             ! IN Height above surface of top of
                             !    boundary layer (metres).
,tstar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! IN GBM surface temperature (K).

LOGICAL, INTENT(IN) ::                                                         &
 l_aero_classic
                             ! IN switch for using CLASSIC aerosol
                             !    scheme

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 z0msea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                             ! IN Sea-surface roughness
                             !    length for momentum (m).
,cdr10m(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end)

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 fqw_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! IN Moisture flux between layers
                             !    (kg per square metre per sec).
                             !    FQW(,1) is total water flux
                             !    from surface, 'E'.
,ftl_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! IN FTL(,K) contains net turbulent
                             !    sensible heat flux into layer K
                             !    from below; so FTL(,1) is the
                             !    surface sensible heat, H.(W/m2)

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 rhokm_land(pdims_s%i_start:pdims_s%i_end,                                     &
            pdims_s%j_start:pdims_s%j_end),                                    &
 rhokm_ssi(pdims_s%i_start:pdims_s%i_end,                                      &
           pdims_s%j_start:pdims_s%j_end)

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 rhokh_surft(land_pts,nsurft)                                                  &
                             ! IN Surface exchange coefficients
                             !    for land tiles
,rhokh_sice_ncats(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,         &
                                                        nice_use)              &
                             ! IN Surface exchange coefficients
                             !    for sea-ice
,rhokh_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                &
                             ! IN Surface exchange coefficients
                             !    for sea
,z0h_surft(land_pts,nsurft)                                                    &
                             ! IN Tile roughness lengths for heat
                             !    and moisture (m).
,z0m_surft(land_pts,nsurft)                                                    &
                             ! IN Tile roughness lengths for
                             !    momentum.
,vshr_land(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                &
                             ! IN Magnitude of surface-to-lowest
                             !    atm level wind shear (m per s).
,vshr_ssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                 &
                             ! IN Magnitude of surface-to-lowest
                             !    atm level wind shear (m per s).
,tile_frac(land_pts,nsurft)
                             ! IN Tile fractions including
                             !    snow cover in the ice tile.

!  sea and sea-ice leads
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 cd_ssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                             ! IN Bulk transfer coefficient for
                             !    momentum over sea mean.
,ch_ssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                             ! IN Bulk transfer coefficient for heat
                             !    and/or moisture over sea mean.
,rhokh_1_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)             &
                             ! IN Surface exchange coefficient for
                             !    sea-ice.
,rib_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                             ! IN Bulk Richardson number
,z0h_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! IN Roughness length for heat and
                             !    moisture transport


!  sea-ice and marginal ice zone
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 cd_land(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                             ! IN Bulk transfer coefficient for
                             !    momentum over land.
,rib_ice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,                  &
                                                        nice_use)              &
                             ! IN Bulk Richardson number
,rib_surft(land_pts,nsurft)                                                    &
                             ! IN RIB for land tiles.
,z0m_ice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,                  &
                                                        nice_use)              &
                             ! IN Momentum Roughness length.
,z0h_ice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,                  &
                                                        nice_use)              &
                             ! IN Thermal Roughness length.
,ice_fract(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! IN Sea ice fraction summed over all cats

!  land tiles
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 ch_surft_classic(land_pts,nsurft)                                             &
                             ! IN Bulk transfer coefficient for
                             !    heat for aerosol deposition.
,cd_std_classic(land_pts,nsurft)
                             ! IN Bulk transfer coefficient for
                             !    momentum for aerosol deposition.

INTEGER, INTENT(IN) ::                                                         &
 surft_index(land_pts,ntype)                                                   &
                             ! IN Index of tile points
,surft_pts(ntype)            ! IN Number of tile points



!-----------------------------------------------------------------------
!  In/outs :-
!-----------------------------------------------------------------------
! data
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
 rhostar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                              ! INOUT Surface air density
,blendht(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! INOUT blending height

!Diagnostics
TYPE (strnewsfdiag), INTENT(IN OUT) :: sf_diag

!-----------------------------------------------------------------------
!  Outputs :-
!-----------------------------------------------------------------------
!-1 Diagnostic (or effectively so - includes coupled model requisites):-
!  (a) Calculated anyway (use STASH space from higher level) :-

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 rhokm_1(pdims_s%i_start:pdims_s%i_end,                                        &
         pdims_s%j_start:pdims_s%j_end)                                        &
                             ! OUT Exchange coefficients for
                             !     momentum on P-grid
,rib(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                      &
                             ! OUT Mean bulk Richardson number for
                             !     lowest layer.
,rho_aresist(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)              &
                             ! OUT RHOSTAR*CD_STD*VSHR
                             !     for CLASSIC aerosol scheme
,aresist(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                             ! OUT 1/(CD_STD*VSHR)
                             !     for CLASSIC aerosol scheme
,resist_b(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                 &
                             ! OUT (1/CH-1/(CD_STD)/VSHR
                             !     for CLASSIC aerosol scheme
,rho_aresist_surft(land_pts,nsurft)                                            &
                             ! OUT RHOSTAR*CD_STD*VSHR on land
                             !     tiles for CLASSIC aerosol scheme
,aresist_surft(land_pts,nsurft)                                                &
                             ! OUT 1/(CD_STD*VSHR) on land tiles
                             !     for CLASSIC aerosol scheme
,resist_b_surft(land_pts,nsurft)                                               &
                             ! OUT (1/CH-1/CD_STD)/VSHR on land
                             !     tiles for CLASSIC aerosol scheme
,r_b_dust(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,ndiv)            &
                             ! OUT surf layer res for dust
,cd_std_dust(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! OUT Bulk transfer coef. for
!                            !     momentum, excluding orographic
!                            !     effects for mineral dust

!  (b) Not passed between lower-level routines (not in workspace at this
!      level) :-

!-2 Genuinely output, needed by other atmospheric routines :-
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 fb_surf(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                             ! OUT Surface flux buoyancy over
                             !     density (m^2/s^3)
,u_s(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! OUT Surface friction velocity (m/s)

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 rhokh(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! OUT Grid-box surface exchange
                             !     coefficients
,h_blend_orog(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)             &
                             ! OUT Blending height used as part of
                             !     effective roughness scheme
,z0hssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                             ! OUT Roughness length for heat and
                             !     moisture over sea (m).
,z0mssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                             ! OUT Roughness length for momentum
                             !     over sea (m).
,z0m_eff(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! OUT Effective grid-box roughness
                             !     length for momentum

!ancil_info (IN)
INTEGER, INTENT(IN) ::                                                         &
  ssi_index(t_i_length * t_j_length),                                          &
  sice_index_ncat(t_i_length * t_j_length,nice),                               &
  sice_pts_ncat(nice)

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  sice_frac_ncat(t_i_length * t_j_length,nice)

!jules_internal
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: unload_backgrnd_pft(land_pts,npft)

!-----------------------------------------------------------------------
! LOCAL variables
!-----------------------------------------------------------------------
!  Workspace :-

!  Workspace for land tiles
REAL(KIND=real_jlslsm) ::                                                      &
 fz0(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                      &
                             ! Aggregation function for Z0.
,fz0h(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                             ! Aggregation function for Z0H.
,z0h_eff(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                             ! Effective roughness length for heat
,z0m_gb_land(land_pts)                                                         &
                             ! GBM momentum land roughness length
,z0h_gb_land(land_pts)                                                         &
                             ! GBM land roughness length for heat
,cd_gb                                                                         &
                             ! GBM transfer coefficient
,blendht_now                                                                   &
                             ! blending height at the current time step
,blendht_next
                             ! blending height at the next time step

#if !defined(UM_JULES)
LOGICAL :: l_dust_diag
  !In standalone, this switch essentially does the same job as l_dust
#endif

!  Local scalars :-

INTEGER ::                                                                     &
 i,j                                                                           &
             ! Loop counter (horizontal field index).
,k                                                                             &
             ! Loop counter (tile field index).
,l                                                                             &
             ! Loop counter (land point field index).
,n
             ! Loop counter (tile index).

REAL(KIND=real_jlslsm) ::                                                      &
 bl_stress_grad                                                                &
             ! Stress gradient across boundary layer
,ws10
             ! 10-m wind speed, used in calculation of unloading of snow from
             ! vegetation (m s-1)

REAL(KIND=real_jlslsm) ::tmp1, tmp2

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='JULES_GRIDDIAG_SF_EXPLICIT'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)


!
!-----------------------------------------------------------------------
! GBM diagnstic calculations
!-----------------------------------------------------------------------

#if !defined(UM_JULES)
! In standalone, this switch does the same job as l_dust, so set accordingly
l_dust_diag = l_dust
#endif


!-----------------------------------------------------------------------
! Calculate effective roughness lengths, orographic blending heights
! and gridbox-average Richardson numbers.
!-----------------------------------------------------------------------

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i, j, tmp1, tmp2)                         &
!$OMP SHARED(tdims, fz0, fz0h, rib, h_blend_orog, flandg,                      &
!$OMP        ice_fract, rib_sea, z0msea, z0h_sea)

!$OMP DO SCHEDULE(STATIC)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    h_blend_orog(i,j) = h_blend_min
    tmp1 = (1.0 - flandg(i,j)) * (1.0 - ice_fract(i,j))
    tmp2 = LOG(lb / z0msea(i,j))
    rib(i,j) = tmp1 * rib_sea(i,j)
    fz0(i,j) = tmp1 / (tmp2**2)
    fz0h(i,j) = tmp1 / (tmp2 * LOG(lb / z0h_sea(i,j)))
  END DO
END DO
!$OMP END DO
!$OMP END PARALLEL
!
IF (nice_use >  1) THEN
  DO n = 1,nice_use
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(i, j,k,l,tmp1,tmp2)   &
!$OMP SHARED(n, sice_pts_ncat, sice_index_ncat, ssi_index,t_i_length,          &
!$OMP fz0, fz0h, rib, sice_frac_ncat, flandg, rib_ice, z0m_ice, z0h_ice)
    DO k = 1,sice_pts_ncat(n)
      l = sice_index_ncat(k,n)
      j=(ssi_index(l) - 1) / t_i_length + 1
      i = ssi_index(l) - (j-1) * t_i_length
      tmp1 = (1.0 - flandg(i,j)) * sice_frac_ncat(l,n)
      tmp2 = LOG(lb / z0m_ice(i,j,n))
      rib(i,j) = rib(i,j) + tmp1 * rib_ice(i,j,n)
      fz0(i,j) = fz0(i,j) + tmp1 / (tmp2**2)
      fz0h(i,j) = fz0h(i,j) +  tmp1 / (tmp2 * LOG(lb / z0h_ice(i,j,n)))
    END DO
!$OMP END PARALLEL DO
  END DO
ELSE
!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i, j, tmp1, tmp2)                         &
!$OMP SHARED(tdims, fz0, fz0h, rib, h_blend_orog, flandg,                      &
!$OMP        ice_fract, rib_ice, z0m_ice, z0h_ice)
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      tmp1 = (1.0 - flandg(i,j)) * ice_fract(i,j)
      tmp2 = LOG(lb / z0m_ice(i,j,1))
      rib(i,j) = rib(i,j) + tmp1 * rib_ice(i,j,1)
      fz0(i,j) = fz0(i,j) + tmp1 / (tmp2**2)
      fz0h(i,j) = fz0h(i,j) + tmp1 / (tmp2 * LOG(lb / z0h_ice(i,j,1)))
    END DO
  END DO
!$OMP END DO
!$OMP END PARALLEL
END IF
!
DO n = 1,nsurft
!$OMP PARALLEL DO IF(surft_pts(n) > 1) SCHEDULE(STATIC) DEFAULT(NONE)          &
!$OMP SHARED(n,surft_pts,surft_index,land_index,t_i_length,rib,flandg,         &
!$OMP        tile_frac,rib_surft,fz0,z0m_surft,fz0h,z0h_surft)                 &
!$OMP PRIVATE(i,j,k,l, tmp1,tmp2)
  DO k = 1,surft_pts(n)
    l = surft_index(k,n)
    j=(land_index(l) - 1) / t_i_length + 1
    i = land_index(l) - (j-1) * t_i_length
    tmp1 = flandg(i,j) * tile_frac(l,n)
    tmp2 = LOG(lb / z0m_surft(l,n))
    rib(i,j) = rib(i,j) + tmp1 * rib_surft(l,n)
    fz0(i,j) = fz0(i,j) + tmp1 / (tmp2**2)
    fz0h(i,j) = fz0h(i,j) + tmp1 / (tmp2 * LOG(lb / z0h_surft(l,n)))
  END DO
!$OMP END PARALLEL DO
END DO

IF (sf_diag%l_rib_surft) THEN
  sf_diag%rib_surft = rib_surft
END IF

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(j,i,l)                                    &
!$OMP SHARED(tdims,fz0,fz0h,z0m_eff,z0h_eff,sf_diag,land_pts,land_index,       &
!$OMP        t_i_length,z0h_gb_land,z0m_gb_land)
!$OMP DO SCHEDULE(STATIC)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    z0m_eff(i,j) = lb * EXP( - SQRT(1.0 / fz0(i,j)) )
    z0h_eff(i,j) = lb * EXP( - SQRT(fz0(i,j)) / fz0h(i,j))
  END DO
END DO
!$OMP END DO

IF (sf_diag%l_z0m_gb) THEN
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      sf_diag%z0m_gb(i,j) = z0m_eff(i,j)
    END DO
  END DO
!$OMP END DO
END IF

!$OMP DO SCHEDULE(STATIC)
DO l = 1,land_pts
  j=(land_index(l) - 1) / t_i_length + 1
  i = land_index(l) - (j-1) * t_i_length
  z0m_gb_land(l) = z0m_eff(i,j)
  z0h_gb_land(l) = z0h_eff(i,j)
END DO
!$OMP END DO
!$OMP END PARALLEL


IF (formdrag ==  effective_z0) THEN
  CALL sf_orog_gb(                                                             &
   land_pts,land_index,                                                        &
   ho2r2_orog,rib,sil_orog_land,z0m_gb_land,z1_uv,                             &
   h_blend_orog,z0m_eff,sf_diag,z0h_gb_land,z0h_eff                            &
   )
END IF

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(j,i)                                      &
!$OMP SHARED(sf_diag,tdims,z0h_eff,rhokm_1,flandg,rhokm_land,rhokm_ssi,        &
!$OMP        rhostar,rhostar_land,rhostar_ssi,l_accurate_rho)
IF (sf_diag%l_z0h_eff_gb) THEN
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      sf_diag%z0h_eff_gb(i,j) = z0h_eff(i,j)
    END DO
  END DO
!$OMP END DO
END IF

!-----------------------------------------------------------------------
! Set grid-box surface air density and exchange coefficients
!-----------------------------------------------------------------------
IF (l_accurate_rho) THEN
  ! No need to recalculate, and breaks kgo if we do
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      rhostar(i,j) = flandg(i,j) * rhostar_land(i,j) +                         &
                         (1.0 - flandg(i,j)) * rhostar_ssi(i,j)
    END DO
  END DO
!$OMP END DO
END IF

!$OMP DO SCHEDULE(STATIC)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    rhokm_1(i,j) = flandg(i,j) * rhokm_land(i,j) +                             &
                       (1.0 - flandg(i,j)) * rhokm_ssi(i,j)
  END DO
END DO
!$OMP END DO
!$OMP END PARALLEL

IF (nice_use > 1) THEN
  ! Take account of leads, and use multiple thickness categories,
  ! if these are present in CICE.
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(i,j)                  &
!$OMP SHARED(tdims,flandg,rhokh,ice_fract,rhokh_sea)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      IF ( flandg(i,j) <  1.0) THEN
        rhokh(i,j) = (1.0 - flandg(i,j)) * (1.0 - ice_fract(i,j))              &
                         * rhokh_sea(i,j)
      ELSE
        rhokh(i,j) = 0.0
      END IF
    END DO
  END DO
!$OMP  END PARALLEL DO
  DO n = 1,nice_use
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(i,j,k,l)              &
!$OMP SHARED(sice_pts_ncat,n,sice_index_ncat,ssi_index,t_i_length,             &
!$OMP rhokh,sice_frac_ncat,rhokh_sice_ncats)
    DO k = 1,sice_pts_ncat(n)
      l = sice_index_ncat(k,n)
      j=(ssi_index(l) - 1) / t_i_length + 1
      i = ssi_index(l) - (j-1) * t_i_length
      rhokh(i,j) = rhokh(i,j) +                                                &
              (sice_frac_ncat(l,n) * rhokh_sice_ncats(i,j,n))
    END DO
!$OMP  END PARALLEL DO
  END DO
ELSE
  ! Do not include leads or thickness categories.
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(i,j)                  &
!$OMP SHARED(tdims,flandg,rhokh,rhokh_1_sice)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      IF ( flandg(i,j) <  1.0) THEN
        rhokh(i,j) = (1.0 - flandg(i,j)) * rhokh_1_sice(i,j)
      ELSE
        rhokh(i,j) = 0.0
      END IF
    END DO
  END DO
!$OMP  END PARALLEL DO
END IF


DO n = 1,nsurft
!$OMP PARALLEL DO IF(surft_pts(n) > 1) SCHEDULE(STATIC)                        &
!$OMP SHARED(n,surft_pts,surft_index,land_index,t_i_length,rhokh,flandg,       &
!$OMP        tile_frac,rhokh_surft)                                            &
!$OMP PRIVATE(i,j,k,l)
  DO k = 1,surft_pts(n)
    l = surft_index(k,n)
    j=(land_index(l) - 1) / t_i_length + 1
    i = land_index(l) - (j-1) * t_i_length
    rhokh(i,j) = rhokh(i,j)                                                    &
      + flandg(i,j) * tile_frac(l,n) * rhokh_surft(l,n)
  END DO
!$OMP  END PARALLEL DO
END DO


!-----------------------------------------------------------------------
! Calculate scaling parameters required for non-local BL scheme
!-----------------------------------------------------------------------

!$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(bl_stress_grad, i, j)                  &
!$OMP SHARED(tdims, u_s, flandg, cd_land, vshr_land, cd_ssi, vshr_ssi, zh,     &
!$OMP        fb_surf, g, bt_1, ftl_1, bq_1, fqw_1, rhostar)                    &
!$OMP SCHEDULE(STATIC)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    u_s(i,j) = SQRT( flandg(i,j) * cd_land(i,j) * vshr_land(i,j) *             &
                     vshr_land(i,j) + (1.0 - flandg(i,j)) * cd_ssi(i,j) *      &
                     vshr_ssi(i,j) * vshr_ssi(i,j) )
    !   !------------------------------------------------------
    !   ! Limit the explicitly calculated surface stress,
    !   ! used to scale the non-local parametrizations,
    !   ! such that the implied stress gradient across the BL
    !   ! is less than Max_Stress_Grad.
    !   !------------------------------------------------------
    bl_stress_grad = u_s(i,j) * u_s(i,j) / zh(i,j)
    IF (bl_stress_grad > max_stress_grad) THEN
      u_s(i,j) = SQRT(zh(i,j) * max_stress_grad)
    END IF

    fb_surf(i,j) = g * ( bt_1(i,j) * ftl_1(i,j) +                              &
                     bq_1(i,j) * fqw_1(i,j) ) / rhostar(i,j)
  END DO
END DO
!$OMP END PARALLEL DO

!---------------------------------------------------------------------
! Calculate blending height for the next time step. To do this, we
! need to calculate gridbox mean transfer coefficent, cd_gb.
!----------------------------------------------------------------------

!$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(cd_gb, blendht_now, blendht_next, i, j)&
!$OMP SHARED(tdims, flandg, cd_land, cd_ssi, blendht)                          &
!$OMP SCHEDULE(STATIC)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    cd_gb = flandg(i,j) * cd_land(i,j) + (1.0 - flandg(i,j)) * cd_ssi(i,j)
    blendht_now = blendht(i,j)
    blendht_next = 2.0 * 220.0 * cd_gb
    blendht(i,j) = 0.5 * (blendht_now + blendht_next)
  END DO
END DO
!$OMP END PARALLEL DO

! Feed blending height to the surface diagnostic data structure.
IF (sf_diag%l_blendht) THEN
  sf_diag%blendht = blendht
END IF


! Calculate parameters required for CLASSIC aerosol
! Note that the values for cd_std and ch_surft are now those
! set using the "aerosol roughness length"
CALL sf_aero (                                                                 &
 land_pts,nsurft,land_index,surft_index,surft_pts,                             &
 l_aero_classic,l_dust,l_dust_diag,flandg,tile_frac,                           &
 pstar,rhostar,rhostar_land,rhostar_ssi,tstar,vshr_land,vshr_ssi,              &
 cd_ssi,ch_ssi,cd_std_classic,ch_surft_classic,                                &
 rho_aresist,aresist,resist_b,rho_aresist_surft,aresist_surft,                 &
 resist_b_surft,r_b_dust,cd_std_dust                                           &
 )

! set these roughness lengths which otherwise are unspecified
!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(j,i)                                                             &
!$OMP SHARED(tdims,z0mssi,z0msea,z0hssi,z0h_sea)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    z0mssi(i,j) = z0msea(i,j)
    z0hssi(i,j) = z0h_sea(i,j)
  END DO
END DO
!$OMP END PARALLEL DO


!-----------------------------------------------------------------------
! Atmospherically determined variables for the snow scheme.
! (These calculations are placed here to use the full GBM wind speed
! with coastal tiling and for possible later convenience when integrated
! with sea-ice.)
!-----------------------------------------------------------------------
!
DO n = 1, nsurft
  IF (cansnowtile(n)) THEN
    unload_backgrnd_pft(:,n) = unload_rate_cnst(n)
    IF ( unload_rate_u(n) /= 0.0 ) THEN
!$OMP PARALLEL DO IF(surft_pts(n) > 1) SCHEDULE(STATIC)                        &
!$OMP SHARED(n,surft_pts,surft_index,land_index,t_i_length,cdr10m,vshr_land,   &
!$OMP        unload_rate_u,unload_backgrnd_pft)                                &
!$OMP PRIVATE(i,j,k,l,ws10)
      DO k = 1, surft_pts(n)
        l = surft_index(k,n)
        j=(land_index(l) - 1) / t_i_length + 1
        i = land_index(l) - (j-1) * t_i_length
        !       Use GBM value of CD for simplicity. The complexity of
        !       distinguishing coastal points does not seem justified.
        ws10 = cdr10m(i,j) * vshr_land(i,j)
        unload_backgrnd_pft(l,n) = unload_backgrnd_pft(l,n) +                  &
          unload_rate_u(n) * ws10
      END DO
!$OMP END PARALLEL DO
    END IF
  END IF
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN
END SUBROUTINE jules_griddiag_sf_explicit
END MODULE jules_griddiag_sf_explicit_mod
