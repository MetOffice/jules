! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!     SUBROUTINE SF_FLUX -----------------------------------------------
! Description:
! Subroutines SF_FLUX to calculate explicit surface fluxes of
! heat and moisture
!-----------------------------------------------------------------------
MODULE sf_flux_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SF_FLUX_MOD'

CONTAINS
SUBROUTINE sf_flux (                                                           &
 points,surft_pts,pts_index,surft_index,                                       &
 nsnow,n,canhc,dzsurf,hcons,ashtf,qstar,q_elev,radnet,resft,fracs,             &
 rhokh_1,l_soil_point,snowdepth,timestep,                                      &
 t_elev,ts1_elev,tstar,vfrac,rhokh_can,                                        &
 z0h,z0m_eff,zdt,z1_tq,lh0,emis_surft,emis_soil,                               &
 salinityfactor,anthrop_heat,scaling_urban,l_vegdrag,                          &
 alpha1,ashtf_prime,fqw_1,epot,ftl_1,dtstar,sea_point                          &
 )

USE atm_fields_bounds_mod, ONLY: tdims
USE theta_field_sizes, ONLY: t_i_length

USE csigma, ONLY: sbcon
USE planet_constants_mod, ONLY: grcp, cp
USE jules_snow_mod, ONLY: snow_hcon
USE jules_surface_mod, ONLY: ls, lc, lf
USE jules_surface_types_mod, ONLY: urban_roof
USE jules_vegetation_mod, ONLY: l_vegcan_soilfx
USE jules_urban_mod, ONLY: l_moruses_storage
USE jules_surface_mod, ONLY: l_aggregate, l_epot_corr
USE jules_science_fixes_mod, ONLY: l_fix_moruses_roof_rad_coupling,            &
                                   l_fix_neg_snow

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
 points                                                                        &
                           ! IN Total number of points.
,surft_pts                                                                     &
                           ! IN Number of tile points.
,pts_index(points)                                                             &
                           ! IN Index of points.
,surft_index(points)                                                           &
                           ! IN Index of tile points.
,nsnow(points)                                                                 &
                           ! IN Number of snow layers
,n                         ! IN Tile number.
                           ! For sea and sea-ice this = 0

LOGICAL, INTENT(IN) ::                                                         &
 l_soil_point(points)                                                          &
                      ! IN Boolean to test for soil points
,l_vegdrag
                      ! IN Option for vegetation canopy drag scheme.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 canhc(points)                                                                 &
                           ! IN Areal heat capacity of canopy (J/K/m2).
,dzsurf(points)                                                                &
                           ! IN Surface layer thickness (m).
,ashtf(points)                                                                 &
                           ! IN Coefficient to calculate surface
                           !    heat flux into soil (W/m2/K).
,qstar(points)                                                                 &
                           ! IN Surface qsat.
,q_elev(points)                                                                &
                           ! IN Total water content of lowest
                           !    atmospheric layer (kg per kg air).
,radnet(points)                                                                &
                           ! IN Net surface radiation (W/m2) positive
                           !    downwards
,fracs(points)                                                                 &
                           ! IN fractional coverage of snow
,resft(points)                                                                 &
                           ! IN Total resistance factor.
,rhokh_1(points)                                                               &
                           ! IN Surface exchange coefficient.
,snowdepth(points)                                                             &
                           ! IN Snow depth (on ground) (m)
,timestep                                                                      &
                           ! IN Timestep (s).
,t_elev(points)                                                                &
                           ! IN Liquid/frozen water temperature for
                           !     lowest atmospheric layer (K).
,ts1_elev(points)                                                              &
                           ! IN Temperature of surface layer (K).
,tstar(points)                                                                 &
                           ! IN Surface temperature (K).
,vfrac(points)                                                                 &
                           ! IN Fractional canopy coverage.
,rhokh_can(points)                                                             &
                           ! IN Exchange coefficient for canopy air
                           !     to surface
,z0h(points)                                                                   &
                           ! IN Roughness length for heat and moisture
,z0m_eff(points)                                                               &
                           ! IN Effective roughness length for momentum
,zdt(points)                                                                   &
                           ! IN Difference between the canopy height and
                           !    displacement height (m)
,z1_tq(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                           ! IN Height of lowest atmospheric level (m).
,emis_surft(points)                                                            &
                           ! IN Emissivity for land tiles
,emis_soil(points)                                                             &
                           ! IN Emissivity of underlying soil
,lh0                                                                           &
                           ! IN Latent heat for snow free surface
                           !    =LS for sea-ice, =LC otherwise
,salinityfactor                                                                &
                           ! IN Factor allowing for the effect of the
                           !    salinity of sea water on the
                           !    evaporative flux.
,anthrop_heat(points)                                                          &
                           ! IN Anthropogenic contribution to surface
                           !    heat flux (W/m2). Zero except for
                           !    urban and L_ANTHROP_HEAT=.true.
                           !    or for urban_canyon & urban_roof when
                           !    l_urban2t=.true.
,scaling_urban(points)                                                         &
                           ! IN MORUSES: ground heat flux scaling;
                           ! canyon tile only coupled to soil.
                           ! This equals 1.0 except for urban tiles when
                           ! MORUSES is used.
,alpha1(points)
                           ! IN Gradient of saturated specific humidity
                           !    with respect to temperature between the
                           !    bottom model layer and the surface.

REAL(KIND=real_jlslsm) ::                                                      &
 hcons(points)
                           ! IN Soil thermal conductivity (W/m/K).

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
 ashtf_prime(points)
                          ! INOUT Adjusted SEB coefficient

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 fqw_1(points)                                                                 &
                           ! OUT Local surface flux of QW (kg/m2/s).
,epot(points)                                                                  &
                           ! OUT
,ftl_1(points)                                                                 &
                           ! OUT Local surface flux of TL.
,dtstar(points)            ! OUT Change in TSTAR over timestep

REAL(KIND=real_jlslsm) ::                                                      &
 sea_point                 ! =1.0 IF SEA POINT, =0.0 OTHERWISE


! Workspace
REAL(KIND=real_jlslsm) ::                                                      &
dtstar_pot(points)                                                             &
                           ! Change in TSTAR over timestep that is
                           ! appropriate for the potential evaporation
,surf_ht_flux              ! Flux of heat from surface to sub-surface

! Scalars
INTEGER ::                                                                     &
 i,j                                                                           &
                           ! Horizontal field index.
,k                                                                             &
                           ! Tile field index.
,l                         ! Points field index.

REAL(KIND=real_jlslsm) ::                                                      &
 lh                        ! Latent heat (J/K/kg).

REAL(KIND=real_jlslsm) :: lambda
                           ! Attenuation factor for influence of soil
                           ! temperature

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SF_FLUX'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(SHARED)                                                          &
!$OMP PRIVATE(l,k,lambda,lh,surf_ht_flux,i,j)

!-----------------------------------------------------------------------
!!  0 initialise
!-----------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
DO l = 1,points
  ftl_1(l)  = 0.0
  epot(l)   = 0.0
END DO
!$OMP END DO

! If conduction in the soil beneath the vegetative canopy is not
! explicitly considered, the attentuation facor may be set equal
! to 1 everywhere.
lambda = 1.0

!$OMP DO SCHEDULE(STATIC)
DO k = 1,surft_pts
  l = surft_index(k)
  j=(pts_index(l) - 1) / t_i_length + 1
  i = pts_index(l) - (j-1) * t_i_length

  ! Calculate the attenuation factor if different from 1.
  IF (l_vegcan_soilfx)                                                         &
    lambda = 2.0 * hcons(l) / dzsurf(l) /                                      &
             ( 2.0 * hcons(l) / dzsurf(l) + rhokh_can(l) +                     &
             4.0 * emis_soil(l) * emis_surft(l) * sbcon *                      &
             tstar(l)**3 )

  lh = lh0
  IF (l_fix_neg_snow) THEN
    ! Effective latent. resft should not be 0 if there is any snow.
    IF (resft(l) > 0.0) lh = lc + lf * fracs(l) / resft(l)
  ELSE
    IF (snowdepth(l) > 0.0) lh = ls
  END IF

  IF (l_vegdrag) THEN
    ftl_1(l) = rhokh_1(l) * (tstar(l) - t_elev(l) -                            &
                    grcp * (z1_tq(i,j) + zdt(l) - z0h(l)))
  ELSE
    ftl_1(l) = rhokh_1(l) * (tstar(l) - t_elev(l) -                            &
                    grcp * (z1_tq(i,j) + z0m_eff(l) - z0h(l)))
  END IF
  epot(l) = rhokh_1(l) * (salinityfactor * qstar(l) - q_elev(l))
  fqw_1(l) = resft(l) * epot(l)

  surf_ht_flux = ((1.0 - vfrac(l)) * ashtf(l) +                                &
                    vfrac(l) * rhokh_can(l) * lambda ) *                       &
                                   (tstar(l) - ts1_elev(l)) +                  &
                 vfrac(l) * emis_soil(l) * emis_surft(l) * sbcon *             &
                  lambda * (tstar(l)**4.0 - ts1_elev(l)**4.0)


  ashtf_prime(l) = 4.0 * (1.0 + lambda * emis_soil(l) * vfrac(l)) *            &
                          emis_surft(l) * sbcon * tstar(l)**3.0 +              &
                          lambda * vfrac(l) * rhokh_can(l) +                   &
                          (1.0 - vfrac(l)) * ashtf(l) + canhc(l) / timestep

  dtstar(l) = (radnet(l) + anthrop_heat(l) - cp * ftl_1(l) -                   &
                         lh * fqw_1(l) - surf_ht_flux)  /                      &
               ( rhokh_1(l) * (cp + lh * alpha1(l) * resft(l)) +               &
                    ashtf_prime(l) )

  ! Correction to surface fluxes due to change in surface temperature
  ftl_1(l) = ftl_1(l) + rhokh_1(l) * dtstar(l) * (1.0 - sea_point)
  fqw_1(l) = fqw_1(l) + resft(l) * rhokh_1(l) * alpha1(l) *                    &
                        dtstar(l) * (1.0 - sea_point)

  IF (l_epot_corr) THEN
    dtstar_pot(l) = (radnet(l) + anthrop_heat(l) - cp * ftl_1(l) -             &
                         lh * epot(l) - surf_ht_flux)  /                       &
               ( rhokh_1(l) * (cp + lh * alpha1(l)) + ashtf_prime(l) )
    epot(l) = epot(l) + rhokh_1(l) * alpha1(l) * dtstar_pot(l)
  ELSE
    epot(l) = epot(l) + rhokh_1(l) * alpha1(l) * dtstar(l)
  END IF

END DO
!$OMP END DO
!$OMP END PARALLEL

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE sf_flux
END MODULE sf_flux_mod
