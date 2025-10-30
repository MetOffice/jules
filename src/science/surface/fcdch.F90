! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE fcdch_mod

USE phi_m_h_vol_mod, ONLY: phi_m_h_vol
USE phi_m_h_mod, ONLY: phi_m_h
USE sea_rough_int_mod, ONLY: sea_rough_int

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='FCDCH_MOD'

CONTAINS

!   SUBROUTINE FCDCH-------------------------------------------------

!  Purpose: Calculate surface transfer coefficients at one or more
!           gridpoints.


!  Documentation: UM Documentation Paper No 24, section 8.
!  Code Owner: Please refer to ModuleLeaders.txt
!--------------------------------------------------------------------

SUBROUTINE fcdch (                                                             &
 cor_mo_iter,points,surft_pts,surft_index,pts_index,                           &
 db,vshr,z0m,z0h,zdt,zh,z1_uv,z1_uv_top,z1_tq,z1_tq_top,                       &
 wind_profile_factor,ddmfx,i_surfalg,charnock,                                 &
 charnock_w,l_vegdrag,canht,lai,                                               &
 nsnow,n,l_mo_buoyancy_calc,cansnowtile,l_soil_point,                          &
 canopy,catch,flake,gc,snowdep,snow,canhc,                                     &
 dzsurf,qstar,q_elev,radnet,snowdepth,timestep,                                &
 t_elev,tsurf,tstar,vfrac,emis,emis_soil,                                      &
 anthrop_heat,scaling_urban,alpha1,hcons,ashtf,                                &
 rhostar,bq_1,bt_1,                                                            &
 cdv,chv,cdv_std,v_s,v_s_std,recip_l_mo,u_s_std                                &
)

USE atm_fields_bounds_mod, ONLY: tdims
USE planet_constants_mod,  ONLY: g, cp, vkman
USE sf_flux_mod,           ONLY: sf_flux
USE sf_resist_mod,         ONLY: sf_resist
USE theta_field_sizes,     ONLY: t_i_length
USE water_constants_mod,   ONLY: lc

USE jules_surface_mod, ONLY:                                                   &
             i_modiscopt, Limit_ObukhovL, srf_ex_cnv_gust,IP_SrfExWithCnv,     &
             Improve_Initial_Guess, third, beta, beta_cndd, beta_cnv_bl,       &
             cnst_cndd_0, cnst_cndd_1, cnst_cndd_2,                            &
             min_wind, min_ustar, Ri_m, l_aggregate, off, on
USE jules_vegetation_mod, ONLY: can_model
USE jules_sea_seaice_mod, ONLY:                                                &
             ip_ss_surf_div_int,ip_ss_coare_mq,ip_ss_surf_div_int_coupled

USE can_drag_mod, ONLY:                                                        &
  can_drag_z0, can_drag_phi_m_h, can_drag_phi_m_h_vol

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

INTEGER ::                                                                     &
 cor_mo_iter          ! IN Switch for MO iteration correction

INTEGER ::                                                                     &
 points                                                                        &
                      ! IN Number of points.
,surft_pts                                                                     &
                      ! IN Number of tile points.
,surft_index(points)                                                           &
                      ! IN Index of tile points.
,pts_index(points) ! IN Index of land points.

INTEGER, INTENT(IN) ::                                                         &
 i_surfalg
                      ! Option for sea surface transfer.
                      ! Set to ip_ss_solid to mark solid surfaces

LOGICAL, INTENT(IN) ::                                                         &
 l_vegdrag
                      ! Option for vegetation canopy drag scheme.

REAL(KIND=real_jlslsm) ::                                                      &
 db(points)                                                                    &
               ! IN Buoyancy difference between surface and lowest
!                    !    temperature and humidity level in the
!                    !    atmosphere (m/s^2).
,vshr(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                       ! IN Wind speed difference between the surf
!                    !    the lowest wind level in the atmosphere (m/s).
,z0m(points)                                                                   &
               ! INOUT Roughness length for momentum transport (m).
,z0h(points)                                                                   &
               ! INOUT Roughness length for heat and moisture (m).
,zdt(points)                                                                   &
               ! INOUT Difference between the canopy height and
!                    ! displacement height (m)
,zh(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                       &
                       ! IN Depth of boundary layer (m).
,z1_uv(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                       ! IN Height of lowest wind level (m).
,z1_tq(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                       ! IN Height of lowest temperature and
!                    !    humidity level (m).
,wind_profile_factor(points)
!                    ! IN for adjusting the surface transfer
!                    !    coefficients to remove form drag effects.
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  z1_uv_top(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                     ! Height of top of lowest uv-layer
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  z1_tq_top(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                     !  Height of top of lowest Tq-layer
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
                    ddmfx(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
!                    !    Convective downdraught mass-flux
!                    !    at cloud-base.
REAL(KIND=real_jlslsm), INTENT(IN) :: charnock
!                    ! Prescribed value of Charnock's coefficient
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  charnock_w(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
!                    ! Charnock's coefficient from the wave model
REAL(KIND=real_jlslsm), INTENT(IN) :: canht(points)
                     ! Canopy height (m)
REAL(KIND=real_jlslsm), INTENT(IN) :: lai(points)
                     ! Leaf area index

INTEGER, INTENT(IN) ::                                                         &
 nsnow(points)                                                                 &
                           ! IN Number of snow layers
,n                         ! IN Tile number.
                           ! For sea and sea-ice this = 0
LOGICAL, INTENT(IN) ::                                                         &
 l_mo_buoyancy_calc                                                            &
                           ! IN Switch for interactive buoyancy
,cansnowtile                                                                   &
                           ! IN Switch for pft canopy snow model
,l_soil_point(points)
                           ! IN Boolean to test for soil points

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 canopy(points)                                                                &
                           ! IN Surface water (kg per sq metre).  F642.
,catch(points)                                                                 &
                           ! IN Surface capacity (max. surface water)
!                          !    (kg per sq metre).  F6416.
,flake(points)                                                                 &
                           ! IN Lake fraction.
,gc(points)                                                                    &
                           ! IN Interactive canopy conductance
!                          !    to evaporation (m/s)
,snowdep(points)                                                               &
                           ! IN Snow depth (m)
,snow(points)                                                                  &
                           ! IN Lying snow on tiles (kg/m2)
,canhc(points)                                                                 &
                           ! IN Areal heat capacity of canopy (J/K/m2).
,dzsurf(points)                                                                &
                           ! IN Surface layer thickness (m).
,qstar(points)                                                                 &
                           ! IN Surface qsat.at start of timestep
,q_elev(points)                                                                &
                           ! IN Total water content of lowest
                           !    atmospheric layer (kg per kg air).
,radnet(points)                                                                &
                           ! IN Net surface radiation (W/m2) positive
                           !    downwards
,snowdepth(points)                                                             &
                           ! IN Snow depth (on ground) (m)
,timestep                                                                      &
                           ! IN Timestep (s).
,t_elev(points)                                                                &
                           ! IN Liquid/frozen water temperature for
                           !     lowest atmospheric layer (K).
,tsurf(points)                                                                 &
                           ! IN Temperature of surface layer (K).
,tstar(points)                                                                 &
                           ! IN Surface temperature (K).
,vfrac(points)                                                                 &
                           ! IN Fractional canopy coverage.
,emis(points)                                                                  &
                           ! IN Emissivity for land tiles
,emis_soil(points)                                                             &
                           ! IN Emissivity of underlying soil
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
,alpha1(points)                                                                &
                           ! IN Gradient of saturated specific humidity
                           !    with respect to temperature between the
                           !    bottom model layer and the surface.
,hcons(points)                                                                 &
                           ! IN Soil thermal conductivity (W/m/K).
,ashtf(points)                                                                 &
                           ! IN Adjusted SEB coefficient
,rhostar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                           ! IN Surface air density
,bq_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                           ! IN A buoyancy parameter for lowest atm
                           !    level. ("beta-q twiddle").
,bt_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                           ! IN A buoyancy parameter for lowest atm
                           !    level. ("beta-T twiddle").

REAL(KIND=real_jlslsm) ::                                                      &
 cdv(points)                                                                   &
               ! OUT Surface transfer coefficient for momentum
!                    !     including orographic form drag (m/s).
,chv(points)                                                                   &
               ! OUT Surface transfer coefficient for
!                    !     heat, moisture & other scalars (m/s).
,cdv_std(points)                                                               &
!                    ! OUT Surface transfer coefficient for momentum
!                    !     excluding orographic form drag (m/s).
,v_s(points)                                                                   &
               ! OUT Surface layer scaling velocity
!                    !     including orographic form drag (m/s).
,v_s_std(points)                                                               &
!                    ! OUT Surface layer scaling velocity
!                    !     excluding orographic form drag (m/s).
,u_s_std(points)                                                               &
               ! OUT Scaling velocity from middle of MO iteration
!                    !     - picked up in error by dust code!
,recip_l_mo(points)
!                    ! OUT Reciprocal of the Monin-Obukhov length
!                    !     (m^-1).

!    Workspace usage----------------------------------------------------

!     Local work arrays.

REAL(KIND=real_jlslsm), PARAMETER ::                                           &
 sea_point = 0.0     ! =1.0 IF SEA POINT, =0.0 OTHERWISE
                     ! Here this is set to zero becuase the iterative
                     ! buoyancy should only be used for land points


REAL(KIND=real_jlslsm) ::                                                      &
 phi_m(points)                                                                 &
!                    ! Monin-Obukhov stability function for momentum
!                    ! integrated to the model's lowest wind level.
,phi_h(points)                                                                 &
!                    ! Monin-Obukhov stability function for scalars
!                    ! integrated to the model's lowest temperature
!                    ! and humidity level.
,chv_dim(points)                                                               &
                     ! Dimensionless transport coefficient
,chv_old(points)                                                               &
                     ! Previous value of chv
,cdv_old(points)                                                               &
                     ! Previous value of cdv
,cdv_std_old(points)                                                           &
                     ! Previous value of cdv_std
,v_s_old(points)                                                               &
                     ! Previous value of v_s
,v_s_std_old(points)                                                           &
                     ! Previous value of v_s_std
,u_s_std_old(points)                                                           &
                     ! Previous value of u_s_std
,recip_l_mo_old(points)                                                        &
                     ! Previous value of recip_l_mo
,db_old(points)                                                                &
                     ! Previous value of db
,db_older(points)                                                              &
                     ! Previous value of db_old
,db_diff(points)                                                               &
                     ! Difference in db between two iterations
,db_diff_old(points)                                                           &
                     ! Previous value of db_diff
,dq(points)                                                                    &
                     ! Sp humidity difference between surface
!                          !    and lowest atmospheric level (Q1 - Q*).
,epdt(points)                                                                  &
                     ! "Potential" Evaporation * Timestep.
!                          !    Dummy variable for first call to routine
,fracaero_t(points)                                                            &
                     ! Total fraction of surface moisture flux with
!                          !     only aerodynamic resistance
,fracaero_s(points)                                                            &
                     ! Fraction of surface moisture flux with
!                          !     only aerodynamic resistance
!                          !     over the frozen part of the surface
,resfs(points)                                                                 &
                     ! Combined soil, stomatal and aerodynamic
!                          !     resistance factor for fraction 1-fracaero_t.
,resft(points)                                                                 &
                     ! Total resistance factor
!                          !     fracaero_t+(1-fracaero_t)*resfs.
,qstemp(points)                                                                &
                     ! Surface qsat at (tstar+dtstar)
,rhokh(points)                                                                 &
                     ! Surface exchange coefficient.
,rhokh_can(points)                                                             &
                     ! Exchange coefficient for canopy air
                     !     to surface
,ashtf_prime(points)                                                           &
                     ! Adjusted SEB coefficient
,fqw(points)                                                                   &
                     ! Local surface flux of QW (kg/m2/s).
,epot(points)                                                                  &
                     ! Local potential flux of QW (Kg/m2/s)
,ftl(points)                                                                   &
                     ! Local surface flux of TL.
,dtstar(points)                                                                &
                     ! Change in TSTAR over timestep
,dt
                     ! Modified temperature difference between
                     ! surface and lowest atmospheric level.

! Dummy arrays required for call to sf_resist
REAL(KIND=real_jlslsm) ::                                                      &
 gc_stom_surft(points)                                                         &
,resfs_stom(points)

REAL(KIND=real_jlslsm) ::                                                      &
        wstrcnvgust(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
!                   ! Turbulent velocity scale for convective gustiness
REAL(KIND=real_jlslsm) :: wgst_tmp
REAL(KIND=real_jlslsm) :: tolerance
                    ! Tolerance for convergence criteria on buoyancy db

! ----------------------------------------------------------------------

!  Define local variables

INTEGER :: i,j,k,l ! Loop counter; horizontal field index.
INTEGER :: it    ! Iteration loop counter.
INTEGER :: n_its ! Number of iterations for Monin-Obukhov length
!                   ! and stability functions.

INTEGER :: indexi(surft_pts), indexj(surft_pts)
              ! Arrays to store horizontal field indices i and j

REAL(KIND=real_jlslsm) ::                                                      &
 b_flux                                                                        &
              ! Surface buoyancy flux over air density.
,u_s2                                                                          &
              ! Iteration surface friction velocity squared
              ! (effective value)
,u_s_std2                                                                      &
              ! Non-effective version of U_S2
,w_s
              ! Surface turbulent convective scaling velocity.

! Derived constants for the convective gustiness calculation
REAL(KIND=real_jlslsm) :: cnst_cndd_1a, cnst_cndd_2a

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='FCDCH'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------
! 0. Initialise values
!-----------------------------------------------------------------------
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(l)                    &
!$OMP SHARED(points,cdv,chv,cdv_std,v_s,v_s_std,u_s_std,recip_l_mo,            &
!$OMP        phi_m,phi_h,rhokh_can,dtstar,db_diff,db_diff_old,                 &
!$OMP        qstemp,qstar,dq,q_elev,gc_stom_surft,gc)
DO l = 1, points
  cdv(l)           = 0.0
  chv(l)           = 0.0
  cdv_std(l)       = 0.0
  v_s(l)           = 0.0
  v_s_std(l)       = 0.0
  u_s_std(l)       = 0.0
  recip_l_mo(l)    = 0.0
  phi_m(l)         = 0.0
  phi_h(l)         = 0.0
  rhokh_can(l)     = 0.0
  dtstar(l)        = 0.0
  db_diff(l)       = 0.0
  db_diff_old(l)   = 0.0
  qstemp(l)        = qstar(l)
  dq(l)            = q_elev(l) - qstemp(l)
  gc_stom_surft(l) = gc(l)
END DO
!$OMP END PARALLEL DO

DO k = 1,surft_pts
  l = surft_index(k)
  indexj(k) = (pts_index(l) - 1) / t_i_length + 1
  indexi(k) = pts_index(l) - (indexj(k) - 1) * t_i_length
END DO

n_its         = 8     ! Found typically 0.2% from converged value
tolerance     = 0.25  ! Set tolerance value for convergence of db to 25%
!-----------------------------------------------------------------------
! 1. Set initial values for the iteration.
!-----------------------------------------------------------------------
IF (cor_mo_iter == Improve_Initial_Guess) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(k,l,j,i,wgst_tmp,cnst_cndd_1a,cnst_cndd_2a) &
!$OMP  SHARED(surft_pts,surft_index,pts_index,t_i_length,ddmfx,wstrcnvgust,    &
!$OMP  beta_cnv_bl,db,vshr,recip_l_mo,zh,srf_ex_cnv_gust,g,indexi,indexj) IF(surft_pts>1)
  IF (srf_ex_cnv_gust == ip_srfexwithcnv) THEN
    cnst_cndd_1a = cnst_cndd_1 / g
    cnst_cndd_2a = cnst_cndd_2 / g**2
!$OMP DO SCHEDULE(STATIC)
    DO k = 1,surft_pts
      j = indexj(k)
      i = indexi(k)

      ! Compute convective gustiness, which is unchanged during the iteration.
      ! Redelsperger's adjustment is to U_10; here it is scaled to apply to
      ! the friction velocity.
      wgst_tmp = MIN( 0.6, MAX(0.0, ddmfx(i, j) ) )
      wstrcnvgust(i,j) = 0.067 *  LOG ( cnst_cndd_0 +                          &
                                      cnst_cndd_1a * wgst_tmp +                &
                                      cnst_cndd_2a * wgst_tmp ** 2 )
    END DO
!$OMP END DO
  END IF

!$OMP DO SCHEDULE(STATIC)
  DO k = 1,surft_pts
    l = surft_index(k)
    j = indexj(k)
    i = indexi(k)
    IF (db(l)  <   0.0 .AND. vshr(i,j)  <   2.0) THEN
      !-----------------------------------------------------------------------
      !         Start the iteration from the convective limit.
      !-----------------------------------------------------------------------
      recip_l_mo(l) = -vkman /                                                 &
                         (beta_cnv_bl * beta_cnv_bl * beta_cnv_bl * zh(i,j))
    ELSE
      !-----------------------------------------------------------------------
      !         Start the iteration from neutral values.
      !-----------------------------------------------------------------------
      recip_l_mo(l) = 0.0
    END IF
  END DO
!$OMP END DO
!$OMP END PARALLEL

  IF (l_vegdrag) THEN
    CALL can_drag_z0(points, surft_pts, surft_index,                           &
                     recip_l_mo, canht, lai,                                   &
                     z0m, z0h, zdt)
    SELECT CASE (i_modiscopt)
    CASE (off)
      CALL can_drag_phi_m_h(points, surft_pts, surft_index, pts_index,         &
                            recip_l_mo, z1_uv, z1_tq, canht, lai, z0m, z0h,    &
                            phi_m, phi_h)
    CASE (on)
      CALL can_drag_phi_m_h_vol(points, surft_pts, surft_index, pts_index,     &
                       recip_l_mo, z1_uv_top, z1_tq_top, canht, lai, z0m, z0h, &
                       phi_m, phi_h)
    END SELECT
  ELSE
    SELECT CASE (i_modiscopt)
    CASE (off)
      CALL phi_m_h ( points,surft_pts,surft_index,pts_index,                   &
                     recip_l_mo,z1_uv,z1_tq,z0m,z0h,                           &
                     phi_m,phi_h)
    CASE (on)
      CALL phi_m_h_vol ( points,surft_pts,surft_index,pts_index,               &
                     recip_l_mo,z1_uv_top,z1_tq_top,z0m,z0h,                   &
                     phi_m,phi_h)
    END SELECT
  END IF

  IF (l_vegdrag) THEN
!$OMP PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC) PRIVATE(k,l,j,i)              &
!$OMP  SHARED(surft_pts,surft_index,pts_index,t_i_length,db,vshr,              &
!$OMP  v_s_std,phi_h,zh,v_s,wind_profile_factor,chv,beta_cnv_bl,               &
!$OMP  cdv,cdv_std,phi_m,indexi,indexj) IF(surft_pts>1)
    DO k = 1,surft_pts
      l = surft_index(k)
      j = indexj(k)
      i = indexi(k)
      IF (db(l)  <   0.0 .AND. vshr(i,j)  <   2.0) THEN
        !---------------------------------------------------------------------
        !         Start the iteration from the convective limit
        !---------------------------------------------------------------------
        v_s_std(l) = beta_cnv_bl *                                             &
            SQRT( beta_cnv_bl * ( vkman / phi_h(l) ) * zh(i,j) * (-db(l)) )
        !---------------------------------------------------------------------
        !         Start from the neutral limit if only slightly unstable
        !---------------------------------------------------------------------
        v_s_std(l) = MAX(v_s_std(l), ( vkman / phi_m(l) ) * vshr(i,j))
        v_s(l) = v_s_std(l)
      ELSE
        !---------------------------------------------------------------------
        !         Start the iteration from neutral values.
        !---------------------------------------------------------------------
        v_s_std(l) = ( vkman / phi_m(l) ) * vshr(i,j)
        v_s(l) = v_s_std(l) / wind_profile_factor(l)
      END IF
      chv(l) = ( vkman / phi_h(l) ) * v_s_std(l)
      cdv_std(l) = ( vkman / phi_m(l) ) * v_s_std(l)
      cdv(l) = cdv_std(l) * ( v_s(l) / v_s_std(l) ) /                          &
                            wind_profile_factor(l)
    END DO
!$OMP END PARALLEL DO
  ELSE
!$OMP PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC) PRIVATE(k,l,j,i)              &
!$OMP  SHARED(surft_pts,surft_index,pts_index,t_i_length,db,vshr,              &
!$OMP  v_s_std,phi_h,zh,v_s,wind_profile_factor,chv,beta_cnv_bl,               &
!$OMP  cdv,cdv_std,phi_m,indexi,indexj) IF(surft_pts>1)
    DO k = 1,surft_pts
      l = surft_index(k)
      j = indexj(k)
      i = indexi(k)
      IF (db(l)  <   0.0 .AND. vshr(i,j)  <   2.0) THEN
        !---------------------------------------------------------------------
        !         Start the iteration from the convective limit.
        !---------------------------------------------------------------------
        v_s_std(l) = beta_cnv_bl *                                             &
            SQRT( beta_cnv_bl * ( vkman / phi_h(l) ) * zh(i,j) * (-db(l)) )
        !---------------------------------------------------------------------
        !         Start from the neutral limit if only slightly unstable
        !---------------------------------------------------------------------
        v_s_std(l) = MAX(v_s_std(l), ( vkman / phi_m(l) ) * vshr(i,j))
        v_s(l) = v_s_std(l)
      ELSE
        !---------------------------------------------------------------------
        !         Start the iteration from neutral values.
        !---------------------------------------------------------------------
        v_s(l) = ( vkman / phi_m(l) ) * vshr(i,j)
        v_s_std(l) = v_s(l) * wind_profile_factor(l)
      END IF
      chv(l) = ( vkman / phi_h(l) ) * v_s_std(l)
      cdv(l) = ( vkman / phi_m(l) ) * v_s(l)
      cdv_std(l) = cdv(l) * ( v_s_std(l) / v_s(l) ) *                          &
                          wind_profile_factor(l)
    END DO
!$OMP END PARALLEL DO
  END IF

  IF (l_mo_buoyancy_calc) THEN

    ! If using interactive buoyancy, then calculate updated moisture resistance
    ! terms. This is only calculated once and not in the iteration to improve
    ! the speed of convergence, without impacting too much on accuracy.

!$OMP PARALLEL IF(surft_pts > 1) DEFAULT(NONE) PRIVATE(i, j, k, l)             &
!$OMP SHARED(surft_pts, surft_index, pts_index, t_i_length, db_old, db,        &
!$OMP        db_older, chv_dim, chv, vshr, rhostar, epdt, dq, timestep,        &
!$OMP        rhokh_can, cp, cdv, l_aggregate, can_model,indexi,indexj)
!$OMP DO SCHEDULE(STATIC)
    DO k = 1,surft_pts
      l = surft_index(k)
      j = indexj(k)
      i = indexi(k)

      ! Save initial values ready for iterations and calculate the
      ! rate of change of potential evaporation with time
      db_old(l)   = db(l)
      db_older(l) = db(l)
      chv_dim(l)  = chv(l) / vshr(i,j)
      epdt(l)     = - rhostar(i,j) * chv(l) *dq(l) * timestep
    END DO
!$OMP END DO NOWAIT

    IF ( .NOT. l_aggregate .AND. can_model == 4) THEN
!$OMP DO SCHEDULE(STATIC)
      DO k = 1,surft_pts
        l = surft_index(k)
        j = indexj(k)
        i = indexi(k)

        ! Set aerodynamic resistance between canopy and soil surface
        rhokh_can(l) = rhostar(i,j) * cp / (43.0 / SQRT(cdv(l)))
      END DO
!$OMP END DO
    END IF
!$OMP END PARALLEL

    ! Calculate surface resistance to moisture terms
    CALL sf_resist (                                                           &
     points,surft_pts,pts_index,surft_index,cansnowtile,                       &
     canopy,catch,chv_dim,dq,epdt,flake,gc,gc_stom_surft,                      &
     snowdep,snow,vshr,tstar,fracaero_t,fracaero_s,                            &
     resfs,resft,resfs_stom,.FALSE.,.FALSE.)

  END IF  ! l_mo_buoyancy_calc

  !-----------------------------------------------------------------------
  ! 2. Iterate to obtain sucessively better approximations for CD & CH.
  !-----------------------------------------------------------------------
  DO it = 1,n_its

    ! Option to include interactive bouyancy flux in the MO iteration
    IF (l_mo_buoyancy_calc) THEN

!$OMP PARALLEL DO IF(surft_pts > 1) DEFAULT(NONE) PRIVATE(i, j, k, l)          &
!$OMP SHARED(surft_pts, surft_index, pts_index, t_i_length,                    &
!$OMP        rhokh, rhostar, chv, indexi, indexj) SCHEDULE(STATIC)
      DO k = 1,surft_pts
        l = surft_index(k)
        j = indexj(k)
        i = indexi(k)

        ! Calculate aerodynamic resistance term
        rhokh(l) = rhostar(i,j) * chv(l)
      END DO
!$OMP END PARALLEL DO

      ! Calculate new values for the sensible and latent heat fluxes
      ! based on the latest values for the exchange coefficients
      CALL sf_flux (                                                           &
       points,surft_pts,                                                       &
       pts_index,surft_index,                                                  &
       nsnow,n,canhc,dzsurf,hcons,ashtf,                                       &
       qstemp,q_elev,                                                          &
       radnet,resft,fracaero_s(:),rhokh,l_soil_point,                          &
       snowdepth,timestep,t_elev,tsurf,                                        &
       tstar,vfrac,rhokh_can,z0h,                                              &
       z0m,zdt,z1_tq,lc,emis,emis_soil,                                        &
       1.0,anthrop_heat,scaling_urban,l_vegdrag,                               &
       alpha1,ashtf_prime,fqw,                                                 &
       epot,ftl,dtstar,sea_point                                               &
       )

!$OMP PARALLEL DO IF(surft_pts > 1) DEFAULT(NONE) PRIVATE(i, j, k, l, dt)      &
!$OMP SHARED(surft_pts, surft_index, pts_index, t_i_length, l_vegdrag,         &
!$OMP        t_elev, tstar, g, cp, z1_tq, zdt, z0m, z0h, dtstar,               &
!$OMP        qstemp, qstar, alpha1, dq, q_elev, db, bt_1, bq_1, resft,         &
!$OMP        chv, chv_old, cdv, cdv_old, cdv_std, cdv_std_old,                 &
!$OMP        v_s, v_s_old, v_s_std, v_s_std_old,                               &
!$OMP        u_s_std, u_s_std_old, recip_l_mo, recip_l_mo_old,                 &
!$OMP        db_diff_old, db_diff, db_old, db_older, indexi, indexj) SCHEDULE(STATIC)
      DO k = 1,surft_pts
        l = surft_index(k)
        j = indexj(k)
        i = indexi(k)

        ! Calculate temperature gradient
        IF (l_vegdrag) THEN
          dt = t_elev(l) - tstar(l) +                                          &
                    (g / cp) * (z1_tq(i,j) + zdt(l) - z0h(l))
        ELSE
          dt = t_elev(l) - tstar(l) +                                          &
                    (g / cp) * (z1_tq(i,j) + z0m(l) - z0h(l))
                                                                   !P243.118
        END IF

        ! Update temperature gradient with surface temperature increment
        ! from surface energy balance calculation and calculate humidity
        ! gradient
        dt        = dt - dtstar(l)
        qstemp(l) = qstar(l) + alpha1(l) * dtstar(l)
        dq(l)     = q_elev(l) - qstemp(l)

        ! Calculate updated value of the buoyancy
        db(l)     = g * (bt_1(i,j) * dt + bq_1(i,j) * resft(l) * dq(l))

        ! Store old value of buoyancy differnece between iterations and
        ! calculate new value
        db_diff_old(l) = db_diff(l)
        db_diff(l)     = db(l)- db_old(l)

        ! If sequential differences in the subsequent buoyancy values have
        ! the opposite sign (i.e., product is negative), then the iteration
        ! is oscillating and not monotonic. Hence use bisection method to
        ! update new buoyancy value to speed up convergence
        IF (db_diff(l) * db_diff_old(l)  <   0.0)                              &
            db(l) = 0.5*(db_old(l) + db(l))

        ! Store the current values of the variables in case of non-convergence
        ! within maximum number of iterations
        db_older(l)       = db_old(l)
        db_old(l)         = db(l)
        chv_old(l)        = chv(l)
        cdv_old(l)        = cdv(l)
        cdv_std_old(l)    = cdv_std(l)
        v_s_old(l)        = v_s(l)
        v_s_std_old(l)    = v_s_std(l)
        u_s_std_old(l)    = u_s_std(l)
        recip_l_mo_old(l) = recip_l_mo(l)

      END DO
!$OMP END PARALLEL DO

    END IF  ! l_mo_buoyancy_calc

    !
    ! --------------------------------------------------------------
    ! Sea Surface
    ! --------------------------------------------------------------
    ! Modify roughness lengths if required by the surface algorithm.
    SELECT CASE (i_surfalg)
      !
    CASE (ip_ss_surf_div_int, ip_ss_coare_mq, ip_ss_surf_div_int_coupled)
      CALL sea_rough_int (                                                     &
        points,surft_pts,surft_index,pts_index,                                &
        charnock,charnock_w,v_s,recip_l_mo,                                    &
        z0m,z0h                                                                &
        )
      !
    CASE DEFAULT
      !     Do not alter the roughness lengths at this point.
      !
    END SELECT
    !
    ! --------------------------------------------------------------
    !
!$OMP PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                               &
!$OMP  PRIVATE(k,l,j,i,b_flux,u_s2,w_s,u_s_std2)                               &
!$OMP  SHARED(surft_pts,surft_index,pts_index,t_i_length,chv,db,cdv,           &
!$OMP  vshr,cdv_std,u_s_std,zh,v_s,v_s_std,recip_l_mo,srf_ex_cnv_gust,         &
!$OMP  beta_cnv_bl,wstrcnvgust,z1_tq,z1_uv,indexi,indexj) IF(surft_pts>1)
    DO k = 1,surft_pts
      l = surft_index(k)
      j = indexj(k)
      i = indexi(k)
      b_flux = -chv(l) * db(l)

      IF ( vshr(i,j) > min_wind) THEN
        u_s2 = cdv(l) * vshr(i,j)
      ELSE
        u_s2 = min_ustar
      END IF
      u_s_std2 = cdv_std(l) * vshr(i,j)
      u_s_std(l) = SQRT( u_s_std2 )

      IF (db(l)  <   -EPSILON(0.0)) THEN
        w_s = MAX( (zh(i,j) * b_flux)**third, EPSILON(0.0) )
        ! Note that, during this iteration, CDV already includes
        ! this gust enhancement and so U_S2 is not simply the
        ! friction velocity arising from the mean wind, hence:
        SELECT CASE(srf_ex_cnv_gust)
        CASE (off)
          v_s(l) = SQRT( 0.5 * ( beta_cnv_bl * beta_cnv_bl * w_s * w_s         &
                 + SQRT( (beta_cnv_bl * w_s)**4 + 4.0 * u_s2 * u_s2 )) )
          v_s_std(l) = SQRT( 0.5 * ( beta_cnv_bl * beta_cnv_bl * w_s * w_s     &
                     + SQRT( (beta_cnv_bl * w_s)**4                            &
                     + 4.0 * u_s_std2 * u_s_std2 )) )
        CASE (ip_srfexwithcnv)
          v_s(l) = SQRT( 0.5 * ( beta_cnv_bl * beta_cnv_bl * w_s * w_s         &
                 + wstrcnvgust(i,j)**2                                         &
                 + SQRT( ((beta_cnv_bl * w_s)**2                               &
                 + wstrcnvgust(i,j)**2)**2                                     &
                     + 4.0 * u_s2 * u_s2 )) )
          v_s_std(l) = SQRT( 0.5 * ( beta_cnv_bl * beta_cnv_bl * w_s * w_s     &
                 + wstrcnvgust(i,j)**2                                         &
                 + SQRT( ((beta_cnv_bl * w_s)**2                               &
                 + wstrcnvgust(i,j)**2)**2                                     &
                 + 4.0 * u_s_std2 * u_s_std2 )) )
        END SELECT
      ELSE
        v_s(l) = SQRT( u_s2 )
        v_s_std(l) = SQRT( u_s_std2 )
      END IF

      IF ( v_s(l) > min_wind) THEN
        recip_l_mo(l) = -vkman * b_flux                                        &
                      / (v_s(l) * v_s(l) * v_s(l))
      ELSE
        recip_l_mo(l) = 1.0 / SQRT(EPSILON(0.0))
      END IF

      IF ( db(l) > Ri_m * z1_tq(i,j) * (vshr(i,j) / z1_uv(i,j))**2.0 ) THEN
        recip_l_mo(l)= 1.5 * Ri_m**2.0 / z1_tq(i,j)
      END IF
    END DO
!$OMP END PARALLEL DO

    IF (l_vegdrag) THEN
      CALL can_drag_z0(points, surft_pts, surft_index,                         &
                       recip_l_mo, canht, lai,                                 &
                       z0m, z0h, zdt)

      SELECT CASE (i_modiscopt)
      CASE (off)
        CALL can_drag_phi_m_h(points, surft_pts, surft_index, pts_index,       &
                              recip_l_mo, z1_uv, z1_tq, canht, lai, z0m, z0h,  &
                              phi_m, phi_h)
      CASE (on)
        CALL can_drag_phi_m_h_vol(points, surft_pts, surft_index, pts_index,   &
                       recip_l_mo, z1_uv_top, z1_tq_top, canht, lai, z0m, z0h, &
                       phi_m, phi_h)
      END SELECT

!$OMP PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC) PRIVATE(k,l)                  &
!$OMP  SHARED(surft_pts,surft_index,chv,cdv,cdv_std,phi_h,                     &
!$OMP  phi_m,v_s_std,v_s,wind_profile_factor) IF(surft_pts>1)
      DO k = 1,surft_pts
        l = surft_index(k)
        chv(l) = ( vkman / phi_h(l) ) * v_s_std(l)
        cdv_std(l) = ( vkman / phi_m(l) ) * v_s_std(l)
        cdv(l) = cdv_std(l) * ( v_s(l) / v_s_std(l) ) /                        &
                              wind_profile_factor(l)
      END DO
!$OMP END PARALLEL DO
    ELSE
      SELECT CASE (i_modiscopt)
      CASE (off)
        CALL phi_m_h ( points,surft_pts,surft_index,pts_index,                 &
                       recip_l_mo,z1_uv,z1_tq,z0m,z0h,                         &
                       phi_m,phi_h)
      CASE (on)
        CALL phi_m_h_vol ( points,surft_pts,surft_index,pts_index,             &
                       recip_l_mo,z1_uv_top,z1_tq_top,z0m,z0h,                 &
                       phi_m,phi_h)
      END SELECT


!$OMP PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC) PRIVATE(k,l)                  &
!$OMP  SHARED(surft_pts,surft_index,chv,cdv,cdv_std,phi_h,                     &
!$OMP  phi_m,v_s_std,v_s,wind_profile_factor) IF(surft_pts>1)
      DO k = 1,surft_pts
        l = surft_index(k)
        chv(l) = ( vkman / phi_h(l) ) * v_s_std(l)
        cdv(l) = ( vkman / phi_m(l) ) * v_s(l)
        cdv_std(l) = cdv(l) * ( v_s_std(l) / v_s(l) ) *                        &
                              wind_profile_factor(l)
      END DO
!$OMP END PARALLEL DO
    END IF
  END DO ! Iteration loop

ELSE     ! cor_mo_iter earlier option than Improve_Initial_Guess

!$OMP PARALLEL DEFAULT(NONE)                                                   &
!$OMP  PRIVATE(k,l,j,i,wgst_tmp,cnst_cndd_1a,cnst_cndd_2a)                     &
!$OMP  SHARED(surft_pts,surft_index,pts_index,t_i_length,ddmfx,wstrcnvgust,    &
!$OMP  db,vshr,recip_l_mo,zh,srf_ex_cnv_gust,g,indexi,indexj) IF(surft_pts>1)
  IF (srf_ex_cnv_gust == ip_srfexwithcnv) THEN
    cnst_cndd_1a = cnst_cndd_1 / g
    cnst_cndd_2a = cnst_cndd_2 / g**2
!$OMP DO SCHEDULE(STATIC)
    DO k = 1,surft_pts
      j = indexj(k)
      i = indexi(k)

      !         Compute convective gustiness, which is unchanged during
      !         the iteration. Redelsperger's adjustment is to U_10; here it
      !         is scaled to apply to the friction velocity.
      wgst_tmp = MIN( 0.6, MAX(0.0, ddmfx(i, j) ) )
      wstrcnvgust(i,j) = 0.067 *  LOG ( cnst_cndd_0 +                          &
                                     cnst_cndd_1a * wgst_tmp +                 &
                                     cnst_cndd_2a * wgst_tmp ** 2 )
    END DO
!$OMP END DO
  END IF

!$OMP DO SCHEDULE(STATIC)
  DO k = 1,surft_pts
    l = surft_index(k)
    j = indexj(k)
    i = indexi(k)
    IF (db(l)  <   0.0 .AND. vshr(i,j)  <   2.0) THEN
      !-----------------------------------------------------------------------
      !         Start the iteration from the convective limit.
      !-----------------------------------------------------------------------
      recip_l_mo(l) = -vkman / (beta * beta * beta * zh(i,j))
    ELSE
      !-----------------------------------------------------------------------
      !         Start the iteration from neutral values.
      !-----------------------------------------------------------------------
      recip_l_mo(l) = 0.0
    END IF
  END DO
!$OMP END DO
!$OMP END PARALLEL

  IF (l_vegdrag) THEN
    CALL can_drag_z0(points, surft_pts, surft_index,                           &
                     recip_l_mo, canht, lai,                                   &
                     z0m, z0h, zdt)
    SELECT CASE (i_modiscopt)
    CASE (off)
      CALL can_drag_phi_m_h(points, surft_pts, surft_index, pts_index,         &
                            recip_l_mo, z1_uv, z1_tq, canht, lai, z0m, z0h,    &
                            phi_m, phi_h)
    CASE (on)
      CALL can_drag_phi_m_h_vol(points, surft_pts, surft_index, pts_index,     &
                       recip_l_mo, z1_uv_top, z1_tq_top, canht, lai, z0m, z0h, &
                       phi_m, phi_h)
    END SELECT
  ELSE
    SELECT CASE (i_modiscopt)
    CASE (off)
      CALL phi_m_h ( points,surft_pts,surft_index,pts_index,                   &
                     recip_l_mo,z1_uv,z1_tq,z0m,z0h,                           &
                     phi_m,phi_h)
    CASE (on)
      CALL phi_m_h_vol ( points,surft_pts,surft_index,pts_index,               &
                     recip_l_mo,z1_uv_top,z1_tq_top,z0m,z0h,                   &
                     phi_m,phi_h)
    END SELECT
  END IF

  IF (l_vegdrag) THEN
!$OMP PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC) PRIVATE(k,l,j,i)              &
!$OMP  SHARED(surft_pts,surft_index,pts_index,t_i_length,db,vshr,              &
!$OMP  v_s_std,phi_h,zh,v_s,wind_profile_factor,chv,                           &
!$OMP  cdv,cdv_std,phi_m,indexi,indexj) IF(surft_pts>1)
    DO k = 1,surft_pts
      l = surft_index(k)
      j = indexj(k)
      i = indexi(k)
      IF (db(l)  <   0.0 .AND. vshr(i,j)  <   2.0) THEN
        !---------------------------------------------------------------------
        !         Start the iteration from the convective limit
        !---------------------------------------------------------------------
        v_s_std(l) = beta *                                                    &
            SQRT( beta * ( vkman / phi_h(l) ) * zh(i,j) * (-db(l)) )
        !---------------------------------------------------------------------
        !         Start from the neutral limit if only slightly unstable
        !---------------------------------------------------------------------
        v_s(l) = v_s_std(l)
      ELSE
        !---------------------------------------------------------------------
        !         Start the iteration from neutral values.
        !---------------------------------------------------------------------
        v_s_std(l) = ( vkman / phi_m(l) ) * vshr(i,j)
        v_s(l) = v_s_std(l) / wind_profile_factor(l)
      END IF
      chv(l) = ( vkman / phi_h(l) ) * v_s_std(l)
      cdv_std(l) = ( vkman / phi_m(l) ) * v_s_std(l)
      cdv(l) = cdv_std(l) * ( v_s(l) / v_s_std(l) ) /                          &
                            wind_profile_factor(l)
    END DO
!$OMP END PARALLEL DO
  ELSE
!$OMP PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC) PRIVATE(k,l,j,i)              &
!$OMP  SHARED(surft_pts,surft_index,pts_index,t_i_length,db,vshr,              &
!$OMP  v_s_std,phi_h,zh,v_s,wind_profile_factor,chv,                           &
!$OMP  cdv,cdv_std,phi_m,indexi,indexj) IF(surft_pts>1)
    DO k = 1,surft_pts
      l = surft_index(k)
      j = indexj(k)
      i = indexi(k)
      IF (db(l)  <   0.0 .AND. vshr(i,j)  <   2.0) THEN
        !---------------------------------------------------------------------
        !         Start the iteration from the convective limit.
        !---------------------------------------------------------------------
        v_s_std(l) = beta *                                                    &
            SQRT( beta * ( vkman / phi_h(l) ) * zh(i,j) * (-db(l)) )
        !---------------------------------------------------------------------
        !         Start from the neutral limit if only slightly unstable
        !---------------------------------------------------------------------
        v_s(l) = v_s_std(l)
      ELSE
        !---------------------------------------------------------------------
        !         Start the iteration from neutral values.
        !---------------------------------------------------------------------
        v_s(l) = ( vkman / phi_m(l) ) * vshr(i,j)
        v_s_std(l) = v_s(l) * wind_profile_factor(l)
      END IF
      chv(l) = ( vkman / phi_h(l) ) * v_s_std(l)
      cdv(l) = ( vkman / phi_m(l) ) * v_s(l)
      cdv_std(l) = cdv(l) * ( v_s_std(l) / v_s(l) ) *                          &
                          wind_profile_factor(l)
    END DO
!$OMP END PARALLEL DO
  END IF

  IF (l_mo_buoyancy_calc) THEN

    ! If using interactive buoyancy, then calculate updated moisture resistance
    ! terms. This is only calculated once and not in the iteration to improve
    ! the speed of convergence, without impacting too much on accuracy.

!$OMP PARALLEL IF(surft_pts > 1) DEFAULT(NONE) PRIVATE(i, j, k, l)             &
!$OMP SHARED(surft_pts, surft_index, pts_index, t_i_length, db_old, db,        &
!$OMP        db_older, chv_dim, chv, vshr, rhostar, epdt, dq, timestep,        &
!$OMP        rhokh_can, cp, cdv, l_aggregate, can_model, indexi, indexj)
!$OMP DO SCHEDULE(STATIC)
    DO k = 1,surft_pts
      l = surft_index(k)
      j = indexj(k)
      i = indexi(k)

      ! Save initial values ready for iterations and calculate the
      ! rate of change of potential evaporation with time
      db_old(l)   = db(l)
      db_older(l) = db(l)
      chv_dim(l)  = chv(l) / vshr(i,j)
      epdt(l)     = - rhostar(i,j) * chv(l) *dq(l) * timestep
    END DO
!$OMP END DO NOWAIT

    IF ( .NOT. l_aggregate .AND. can_model == 4) THEN
!$OMP DO SCHEDULE(STATIC)
      DO k = 1,surft_pts
        l = surft_index(k)
        j = indexj(k)
        i = indexi(k)

        ! Set aerodynamic resistance between canopy and soil surface
        rhokh_can(l) = rhostar(i,j) * cp / (43.0 / SQRT(cdv(l)))
      END DO
!$OMP END DO
    END IF
!$OMP END PARALLEL

    ! Calculate surface resistance to moisture terms
    CALL sf_resist (                                                           &
     points,surft_pts,pts_index,surft_index,cansnowtile,                       &
     canopy,catch,chv_dim,dq,epdt,flake,gc,gc_stom_surft,                      &
     snowdep,snow,vshr,tstar,fracaero_t,fracaero_s,                            &
     resfs,resft,resfs_stom,.FALSE.,.FALSE.)

  END IF  ! l_mo_buoyancy_calc

  !-----------------------------------------------------------------------
  ! 2. Iterate to obtain sucessively better approximations for CD & CH.
  !-----------------------------------------------------------------------
  DO it = 1,n_its

    ! Option to include interactive bouyancy flux in the MO iteration
    IF (l_mo_buoyancy_calc) THEN

!$OMP PARALLEL DO IF(surft_pts > 1) DEFAULT(NONE) PRIVATE(i, j, k, l)          &
!$OMP SHARED(surft_pts, surft_index, pts_index, t_i_length,                    &
!$OMP        rhokh, rhostar, chv, indexi, indexj) SCHEDULE(STATIC)
      DO k = 1,surft_pts
        l = surft_index(k)
        j = indexj(k)
        i = indexi(k)

        ! Calculate aerodynamic resistance term
        rhokh(l) = rhostar(i,j) * chv(l)
      END DO
!$OMP END PARALLEL DO

      ! Calculate new values for the sensible and latent heat fluxes
      ! based on the latest values for the exchange coefficients
      CALL sf_flux (                                                           &
       points,surft_pts,                                                       &
       pts_index,surft_index,                                                  &
       nsnow,n,canhc,dzsurf,hcons,ashtf,                                       &
       qstemp,q_elev,                                                          &
       radnet,resft,fracaero_s(:),rhokh,l_soil_point,                          &
       snowdepth,timestep,t_elev,tsurf,                                        &
       tstar,vfrac,rhokh_can,z0h,                                              &
       z0m,zdt,z1_tq,lc,emis,emis_soil,                                        &
       1.0,anthrop_heat,scaling_urban,l_vegdrag,                               &
       alpha1,ashtf_prime,fqw,                                                 &
       epot,ftl,dtstar,sea_point                                               &
       )

!$OMP PARALLEL DO IF(surft_pts > 1) DEFAULT(NONE) PRIVATE(i, j, k, l, dt)      &
!$OMP SHARED(surft_pts, surft_index, pts_index, t_i_length, l_vegdrag,         &
!$OMP        t_elev, tstar, g, cp, z1_tq, zdt, z0m, z0h, dtstar,               &
!$OMP        qstemp, qstar, alpha1, dq, q_elev, db, bt_1, bq_1, resft,         &
!$OMP        chv, chv_old, cdv, cdv_old, cdv_std, cdv_std_old,                 &
!$OMP        v_s, v_s_old, v_s_std, v_s_std_old,                               &
!$OMP        u_s_std, u_s_std_old, recip_l_mo, recip_l_mo_old,                 &
!$OMP        db_diff_old, db_diff, db_old, db_older, indexi, indexj) SCHEDULE(STATIC)
      DO k = 1,surft_pts
        l = surft_index(k)
        j = indexj(k)
        i = indexi(k)

        ! Calculate temperature gradient
        IF (l_vegdrag) THEN
          dt = t_elev(l) - tstar(l) +                                          &
                    (g / cp) * (z1_tq(i,j) + zdt(l) - z0h(l))
        ELSE
          dt = t_elev(l) - tstar(l) +                                          &
                    (g / cp) * (z1_tq(i,j) + z0m(l) - z0h(l))
                                                                   !P243.118
        END IF

        ! Update temperature gradient with surface temperature increment
        ! from surface energy balance calculation and calculate humidity
        ! gradient
        dt        = dt - dtstar(l)
        qstemp(l) = qstar(l) + alpha1(l) * dtstar(l)
        dq(l)     = q_elev(l) - qstemp(l)

        ! Calculate updated value of the buoyancy
        db(l)     = g * (bt_1(i,j) * dt + bq_1(i,j) * resft(l) * dq(l))

        ! Store old value of buoyancy differnece between iterations and
        ! calculate new value
        db_diff_old(l) = db_diff(l)
        db_diff(l)     = db(l)- db_old(l)

        ! If sequential differences in the subsequent buoyancy values have
        ! the opposite sign (i.e., product is negative), then the iteration
        ! is oscillating and not monotonic. Hence use bisection method to
        ! update new buoyancy value to speed up convergence
        IF (db_diff(l) * db_diff_old(l)  <   0.0)                              &
            db(l) = 0.5*(db_old(l) + db(l))

        ! Store the current values of the variables in case of non-convergence
        ! within maximum number of iterations
        db_older(l)       = db_old(l)
        db_old(l)         = db(l)
        chv_old(l)        = chv(l)
        cdv_old(l)        = cdv(l)
        cdv_std_old(l)    = cdv_std(l)
        v_s_old(l)        = v_s(l)
        v_s_std_old(l)    = v_s_std(l)
        u_s_std_old(l)    = u_s_std(l)
        recip_l_mo_old(l) = recip_l_mo(l)

      END DO
!$OMP END PARALLEL DO

    END IF  ! l_mo_buoyancy_calc

    !
    ! --------------------------------------------------------------
    ! Sea Surface
    ! --------------------------------------------------------------
    ! Modify roughness lengths if required by the surface algorithm.
    SELECT CASE (i_surfalg)
      !
    CASE (ip_ss_surf_div_int, ip_ss_coare_mq, ip_ss_surf_div_int_coupled)
      CALL sea_rough_int (                                                     &
        points,surft_pts,surft_index,pts_index,                                &
        charnock,charnock_w,v_s,recip_l_mo,                                    &
        z0m,z0h                                                                &
        )
      !
    CASE DEFAULT
      !     Do not alter the roughness lengths at this point.
      !
    END SELECT
    !
    !-----------------------------------------------------------------------
    !  Corrected version
    !-----------------------------------------------------------------------
!$OMP PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                               &
!$OMP  PRIVATE(k, l, j, i, b_flux, u_s2, w_s, u_s_std2)                        &
!$OMP  SHARED(surft_pts, surft_index, pts_index, t_i_length, chv, db, cdv,     &
!$OMP  vshr, cdv_std, u_s_std, zh, v_s, v_s_std, recip_l_mo, srf_ex_cnv_gust,  &
!$OMP  wstrcnvgust, cor_mo_iter, z1_uv, z1_tq, indexi, indexj) IF(surft_pts>1)
    DO k = 1,surft_pts
      l = surft_index(k)
      j = indexj(k)
      i = indexi(k)
      b_flux = -chv(l) * db(l)

      IF ( vshr(i,j) > min_wind) THEN
        u_s2 = cdv(l) * vshr(i,j)
      ELSE
        u_s2 = min_ustar
      END IF
      u_s_std2 = cdv_std(l) * vshr(i,j)
      u_s_std(l) = SQRT( u_s_std2 )

      IF (db(l)  <   -EPSILON(0.0)) THEN
        w_s = MAX( (zh(i,j) * b_flux)**third, EPSILON(0.0) )
        !           ! Note that, during this iteration, CDV already includes
        !           ! this gust enhancement and so U_S2 is not simply the
        !           ! friction velocity arising from the mean wind, hence:
        SELECT CASE(srf_ex_cnv_gust)
        CASE (off)
          v_s(l) = SQRT( 0.5 * ( beta * beta * w_s * w_s                       &
                 + SQRT( (beta * w_s)**4 + 4.0 * u_s2 * u_s2 )) )
          v_s_std(l) = SQRT( 0.5 * ( beta * beta * w_s * w_s                   &
                     + SQRT( (beta * w_s)**4                                   &
                     + 4.0 * u_s_std2 * u_s_std2 )) )
        CASE (ip_srfexwithcnv)
          v_s(l) = SQRT( 0.5 * ( beta_cndd * beta_cndd * w_s * w_s             &
                 + wstrcnvgust(i,j)**2                                         &
                 + SQRT( ((beta_cndd * w_s)**2                                 &
                 + wstrcnvgust(i,j)**2)**2                                     &
                     + 4.0 * u_s2 * u_s2 )) )
          v_s_std(l) = SQRT( 0.5 * ( beta_cndd * beta_cndd * w_s * w_s         &
                 + wstrcnvgust(i,j)**2                                         &
                 + SQRT( ((beta_cndd * w_s)**2                                 &
                 + wstrcnvgust(i,j)**2)**2                                     &
                 + 4.0 * u_s_std2 * u_s_std2 )) )
        END SELECT
      ELSE
        v_s(l) = SQRT( u_s2 )
        v_s_std(l) = SQRT( u_s_std2 )
      END IF

      IF ( v_s(l) > min_wind) THEN
        recip_l_mo(l) = -vkman * b_flux                                        &
                      / (v_s(l) * v_s(l) * v_s(l))
      ELSE
        IF (cor_mo_iter >= Limit_ObukhovL) THEN
          recip_l_mo(l) = 1.0 / SQRT(EPSILON(0.0))
        ELSE
          recip_l_mo(l) = SQRT(EPSILON(0.0))
        END IF
      END IF

      IF (cor_mo_iter >= Limit_ObukhovL) THEN
        IF ( db(l) > Ri_m  * z1_tq(i,j)                                        &
                       *(vshr(i,j) / z1_uv(i,j))**2.0 ) THEN
          recip_l_mo(l)= 1.5 * Ri_m**2.0 / z1_tq(i,j)
        END IF
      END IF

    END DO
!$OMP END PARALLEL DO

    IF (l_vegdrag) THEN
      CALL can_drag_z0(points, surft_pts, surft_index,                         &
                       recip_l_mo, canht, lai,                                 &
                       z0m, z0h, zdt)

      SELECT CASE (i_modiscopt)
      CASE (off)
        CALL can_drag_phi_m_h(points, surft_pts, surft_index, pts_index,       &
                              recip_l_mo, z1_uv, z1_tq, canht, lai, z0m, z0h,  &
                              phi_m, phi_h)
      CASE (on)
        CALL can_drag_phi_m_h_vol(points, surft_pts, surft_index, pts_index,   &
                       recip_l_mo, z1_uv_top, z1_tq_top, canht, lai, z0m, z0h, &
                       phi_m, phi_h)
      END SELECT

!$OMP PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC) PRIVATE(k,l)                  &
!$OMP  SHARED(surft_pts,surft_index,chv,cdv,cdv_std,phi_h,                     &
!$OMP  phi_m,v_s_std,v_s,wind_profile_factor) IF(surft_pts>1)
      DO k = 1,surft_pts
        l = surft_index(k)
        chv(l) = ( vkman / phi_h(l) ) * v_s_std(l)
        cdv_std(l) = ( vkman / phi_m(l) ) * v_s_std(l)
        cdv(l) = cdv_std(l) * ( v_s(l) / v_s_std(l) ) /                        &
                              wind_profile_factor(l)
      END DO
!$OMP END PARALLEL DO
    ELSE
      SELECT CASE (i_modiscopt)
      CASE (off)
        CALL phi_m_h ( points,surft_pts,surft_index,pts_index,                 &
                       recip_l_mo,z1_uv,z1_tq,z0m,z0h,                         &
                       phi_m,phi_h)
      CASE (on)
        CALL phi_m_h_vol ( points,surft_pts,surft_index,pts_index,             &
                       recip_l_mo,z1_uv_top,z1_tq_top,z0m,z0h,                 &
                       phi_m,phi_h)
      END SELECT


!$OMP PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC) PRIVATE(k,l)                  &
!$OMP  SHARED(surft_pts,surft_index,chv,cdv,cdv_std,phi_h,                     &
!$OMP  phi_m,v_s_std,v_s,wind_profile_factor) IF(surft_pts>1)
      DO k = 1,surft_pts
        l = surft_index(k)
        chv(l) = ( vkman / phi_h(l) ) * v_s_std(l)
        cdv(l) = ( vkman / phi_m(l) ) * v_s(l)
        cdv_std(l) = cdv(l) * ( v_s_std(l) / v_s(l) ) *                        &
                              wind_profile_factor(l)
      END DO
!$OMP END PARALLEL DO
    END IF
  END DO ! Iteration loop

END IF ! test on cor_mo_iter eq Improve_Initial_Guess

IF (l_mo_buoyancy_calc) THEN

  ! If buoyancy has not converged to within the parameter tolerance,
  ! then set the transfer coefficients to be the maximum of the previous
  ! two iterations to ensure that high surface temperatures are avoided

!$OMP PARALLEL DO IF(surft_pts > 1) DEFAULT(NONE) PRIVATE(k, l)                &
!$OMP SHARED(surft_pts, surft_index, pts_index, t_i_length, db, db_older,      &
!$OMP        chv, chv_old, cdv, cdv_old, cdv_std, cdv_std_old, v_s, v_s_old,   &
!$OMP        v_s_std, v_s_std_old, u_s_std, u_s_std_old, recip_l_mo,           &
!$OMP        recip_l_mo_old, tolerance) SCHEDULE(STATIC)
  DO k = 1,surft_pts
    l = surft_index(k)

    ! Test if difference in last two buoyancy values are greater than
    ! the tolerance value and if the latest transfer coefficient is
    ! smaller and the previous value. If so, set variables to be consistent
    ! with the larger tansfer coefficient
    IF (ABS(db(l) - db_older(l))  >   tolerance * db(l) .AND.                  &
                         chv(l)  <   chv_old(l)) THEN
      chv(l)        = chv_old(l)
      cdv(l)        = cdv_old(l)
      cdv_std(l)    = cdv_std_old(l)
      v_s(l)        = v_s_old(l)
      v_s_std(l)    = v_s_std_old(l)
      u_s_std(l)    = u_s_std_old(l)
      recip_l_mo(l) = recip_l_mo_old(l)
    END IF
  END DO
!$OMP END PARALLEL DO
END IF

!-----------------------------------------------------------------------
! Set CD's and CH's to be dimensionless paremters
!-----------------------------------------------------------------------
!$OMP PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC) PRIVATE(k,l,j,i)              &
!$OMP  SHARED(surft_pts,surft_index,pts_index,t_i_length,cdv,                  &
!$OMP  vshr,cdv_std,chv, indexi, indexj) IF(surft_pts>1)
DO k = 1,surft_pts
  l = surft_index(k)
  j = indexj(k)
  i = indexi(k)
  cdv(l) = cdv(l) / vshr(i,j)
  cdv_std(l) = cdv_std(l) / vshr(i,j)
  chv(l) = chv(l) / vshr(i,j)
END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE fcdch
END MODULE fcdch_mod
