!******************************COPYRIGHT**************************************
!
!  Part of the UKCA model, a community model supported by the
!  Met Office and NCAS, with components provided initially
!  by The University of Cambridge, University of Leeds and
!  The Met. Office.  See www.ukca.ac.uk
!  This module was provided by CSIRO.
!
!******************************COPYRIGHT**************************************

MODULE deposition_ukca_h2dd_soil_mod

!-----------------------------------------------------------------------------
! Code description:
! Language: FORTRAN 90
!
! H2 Deposition from Paulot et al. (2021) and Ehhalt (2013)
! Dependent on soil properties
!
! Called from deposition_jules_surfddr or deposition_ukca_surfddr
!-----------------------------------------------------------------------------

USE um_types,                ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName ='DEPOSITION_UKCA_H2DD_SOIL_MOD'

CONTAINS

SUBROUTINE deposition_ukca_h2dd_soil(row_length, rows, land_pts, ntype,        &
  nsurft, dim_cs1, land_index_ij, gsf, p_surf, smr_land, vol_smc_sat_dep,      &
  snow_depth, soil_carbon_dep, deep_soil_temp_dep, soil_sand, rc_h2 )

! The following UKCA modules have been replaced:
! (1) asad_mod for ndepd, speci, nldepd, jpdd
!     Replace asad_mod with deposition_ukca_var_mod
! (2) ukca_config_specification_mod for ukca_config
!     Use jules_deposition_mod for UKCA deposition switches

USE jules_surface_types_mod, ONLY: urban, lake
  ! Indices for surface types: urban and lake

USE jules_soil_mod, ONLY: dzsoil
USE jules_deposition_mod, ONLY: dep_rnull

USE missing_data_mod, ONLY: imdi
USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!-------------------------------------------------------------------------------
! Input arguments

! Input INTEGERS
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Number of land points
  ntype,                                                                       &
    ! Number of surface tiles
  nsurft,                                                                      &
    ! Number of surface tiles
  dim_cs1,                                                                     &
    ! Number of pools for soil carbon (cs)
  row_length,                                                                  &
    ! Number of points on a row
  rows
    ! Number of rows

! Input arguments on lat, long grid
INTEGER, INTENT(IN) ::                                                         &
  land_index_ij(row_length, rows)
    ! index of land points

! Input REAL arguments on land vector
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  smr_land(land_pts),                                                          &
    ! Soil moisture content (Fraction by volume)
  vol_smc_sat_dep(land_pts),                                                   &
    ! volumetric saturation point (m3 water / m3 of soil)
    ! for Paulot H2 deposition scheme
  snow_depth(land_pts,nsurft),                                                 &
    ! snow depth on ground on tiles (m)
    ! for Paulot H2 deposition scheme
  soil_carbon_dep(land_pts,dim_cs1),                                           &
    ! soil carbon
    ! for Paulot H2 deposition scheme
  deep_soil_temp_dep(land_pts),                                                &
    ! sub-surface temperature for surface layer (K)
    ! for Paulot H2 deposition scheme
  soil_sand(land_pts)
   ! fraction of soil that is sand

! Input REAL arguments on lat, lon grid
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  p_surf(row_length,rows),                                                     &
    ! Surface pressure (Pa)
  gsf(row_length,rows,ntype)
    ! Global surface fractions

!-------------------------------------------------------------------------------
! Output arguments

REAL, INTENT(IN OUT) ::                                                        &
  rc_h2(row_length, rows, ntype)
    ! surface resistance of H2 (s m-1)

!-------------------------------------------------------------------------------
! Local variables

INTEGER       :: i, j, k, l, n
  ! loop variables

REAL ::                                                                        &
  alpha = 30.0,                                                                &
    ! variable for A parameter (scalable)
  beta  = 7e-6,                                                                &
    ! variable for A parameter (kg C cm-3)
  depth,                                                                       &
    ! soil depth (m)
  fOw_S_min,                                                                   &
    ! Minimum value for fOw_S
  fOw_L_min,                                                                   &
    ! Minimum value for fOw_L
  smc_ratio
    ! Ratio of SMC / total porosity

! REAL variable on land vector
REAL(KIND=real_jlslsm) ::                                                      &
  a(land_pts),                                                                 &
    ! scaling factor describing microbe activity
  smr_sat(land_pts),                                                           &
    ! saturation soil moisture as volume fraction (m3 pore space/m3 tot vol)
    ! soil porosity
  smr_loc(land_pts),                                                           &
    ! volumetric soil moisture content (m3 water/m3 tot vol)
  air_frac(land_pts),                                                          &
    ! soil air fraction (m3 air / m3 tot vol)
  D_a(land_pts),                                                               &
    ! molecular diffusivity (cm2 s-1)
  D_s(land_pts),                                                               &
    ! soil diffusivity (cm2 s-1)
  D_snow(land_pts),                                                            &
    ! snow diffusivity (cm2 s-1)
  g_T(land_pts),                                                               &
    ! dependence on soil temperature, averaged over top 10cm soil
  delta_S(land_pts),                                                           &
    ! soil moisture parameter (sand) (cm)
  delta_L(land_pts),                                                           &
    ! soil moisture parameter (loam) (cm)
  delta(land_pts),                                                             &
    ! soil moisture parameter        (cm)
  fOw_L(land_pts),                                                             &
    ! derived function for kO_a (loam)
  fOw_S(land_pts),                                                             &
    ! derived function for kO_a (sand)
  fOw(land_pts),                                                               &
    ! derived function for kO_a
  kO_a(land_pts),                                                              &
    ! moisture dependence (s-1)
    ! for Paulot H2 deposition scheme
  deep_soil_temp_loc(land_pts),                                                &
    ! deep soil temperature
  snow_depth_gb(land_pts),                                                     &
    ! grid-box snow depth (cm)
  soil_loam(land_pts),                                                         &
    ! volume fraction of loam (clay+silt)
  soil_carbon_loc(land_pts),                                                   &
    ! soil carbon content (kg C cm-3)

    ! Three main parameters for calculating deposition velocity
  difus(land_pts),                                                             &
    ! delta/D_s
  snow_difus(land_pts),                                                        &
    ! snow_depth_gb/D_snow
  uptake(land_pts)
    ! 1/sqrt(D_s * kO_a)

! REAL variable on lat, long grid
REAL(KIND=real_jlslsm) ::                                                      &
  vd_h2(row_length, rows)
    ! deposition velocity (m s-1)

CHARACTER(LEN=*), PARAMETER :: RoutineName='DEPOSITION_UKCA_H2DD_SOIL'

INTEGER(KIND=jpim), PARAMETER  :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER  :: zhook_out = 1
REAL(KIND=jprb)                :: zhook_handle

!-------------------------------------------------------------------------------
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-------------------------------------------------------------------------------
! write(*,*) 'Entered into hd22_soil'

! Setting up variables
! On land points
a(:)             = 0.0
air_frac(:)      = 0.0
delta_S(:)       = 0.0
delta_L(:)       = 0.0
delta(:)         = 0.0
D_a(:)           = 0.0
D_s(:)           = 0.0
D_snow(:)        = 0.0
g_T(:)           = 0.0
fOw_L(:)         = 0.0
fOw_S(:)         = 0.0
fOw(:)           = 0.0
kO_a(:)          = 0.0
smr_sat(:)       = 0.0
smr_loc(:)       = 0.0
difus(:)         = 0.0
uptake(:)        = 0.0
snow_difus(:)    = 0.0
snow_depth_gb(:) = 0.0
! On lat, long
rc_h2(:,:,:)     = 0.0
vd_h2(:,:)       = 0.0

!-------------------------------------------------------------------------------
! convert surface soil depth in m
depth = dzsoil(1)

! Setting local variables on land points
DO l = 1, land_pts

  ! Converting variables
  smr_sat(l) = vol_smc_sat_dep(l)
  smr_loc(l) = smr_land(l)

  ! soil_carbon is on a (land_field, dim_cs1) grid. dim_cs1 is either 1 or 4.
  ! We only want the first layer of total soil carbon or the humus layer (4).
  ! This is set in deposition_jules_surfddr_mod
  ! kg C m-2 -> kg cm-3
  soil_carbon_loc(l) = soil_carbon_dep(l, dim_cs1) / (depth * 1.0e6)

  ! convert temp K into C
  deep_soil_temp_loc(l) = deep_soil_temp_dep(l)-273.15

  ! SMC adjustment at high latitudes
  IF (smr_sat(l) > 0.0) THEN
    smc_ratio = smr_loc(l) / smr_sat(l)
    IF (smc_ratio > 0.7) THEN ! limit for saturation
      smr_loc(l) = 0.7 * smr_sat(l)
    END IF
  END IF

  ! Calculate
  ! * air fraction
  air_frac(l) = smr_sat(l) - smr_loc(l)

  ! * A parameter
  a(l) = alpha * (soil_carbon_loc(l) / (soil_carbon_loc(l) + beta))

  ! * loam fraction (silt + clay)
  soil_loam(l) = 1.0 - soil_sand(l)

END DO  ! loop over land points

! Calculate H2 uptake on lat, long
DO j = 1, rows
  DO i = 1, row_length

    l = land_index_ij(i,j)
    IF (l /= imdi) THEN
      ! Land point

      ! Create grid-box mean snow depth, converting m to cm
      DO n = 1, ntype
        snow_depth_gb(l) = snow_depth_gb(l) + (100.0*snow_depth(l,n)*gsf(i,j,n))
      END DO

      IF (smr_sat(l) <= 0.0) THEN
        ! Total porosity saturation is 0

        rc_h2(i,j,:) = dep_rnull
        vd_h2(i,j) = 1.0 / dep_rnull

      ELSE
        ! Total porosity saturation is > 0
        ! There is deposition

        ! D_a: molecular diffusivity of H2 in air (cm2 s-1)
        D_a(l) = 0.611 * 101312.0/p_surf(i,j) *                                &
              (((deep_soil_temp_loc(l)+273.15)/273.15)**1.75)

        ! D_s: Soil diffusivity (cm2 s-1)
        D_s(l) = D_a(l) * (air_frac(l)**3.1) / (smr_sat(l)**2.0)

        ! D_s: capped, as equations are valid between 0.2-0.8 air porosity
        ! and this is a problem IF air porosity tends to 0 (Yonemmura 2000)
        IF (D_s(l) < 1.0e-4) D_s(l) = 1.0e-4

        ! D_snow: snow diffusivity (cm2 s-1)
        D_snow(l) = 0.64 * D_a(l)

        ! g(T): dependence on soil temperature
        g_T(l) = (1.0/(1.0+EXP(-(deep_soil_temp_loc(l)-3.8)/6.7))) +           &
                  (1.0/(1.0+EXP((deep_soil_temp_loc(l)-62.2)/7.1))) - 1.0

        ! delta: Parameter for soil moisture, delta (cm),
        ! using average soil moisture over top 10cm
        delta_S(l) = 0.0057 * (air_frac(l)/smr_loc(l))**2.5 ! sandy loam
        delta_L(l) = 0.109 * (air_frac(l)/smr_loc(l))**1.8  ! loam

        ! From Downy-Smith 2008
        ! Nearly all H2 was taken up by 20cm down even in desert envs
        IF (delta_S(l) > 20.0) delta_S(l) = 20.0
        IF (delta_L(l) > 20.0) delta_L(l) = 20.0

        ! f(O_w) derived function from lab experiments used for kO_a
        ! (dependent on conditions)
        ! For Sand
        IF ((smr_loc(l) >= 0.0264) .AND. (smr_sat(l) <= 1.0)) THEN
          fOw_S(l) = 0.00936 * ( ( ((smr_loc(l)/smr_sat(l))-0.02640)*          &
                      (1.0-(smr_loc(l)/smr_sat(l))) )  /                       &
                      ( (smr_loc(l)/smr_sat(l))**2.0 -                         &
                       (0.1715*(smr_loc(l)/smr_sat(l))) + 0.03144 ) )
        ELSE
          ! Calculate min threshold and set values below to *0.3 of lowest
          ! number as can get minimal uptake in desert environments
          fOw_S_min  = 0.00936 * ( ( ((0.0264/smr_sat(l))-0.02640)*            &
                      (1.0-(0.0264/smr_sat(l))) )  /                           &
                      ( (0.0264/smr_sat(l))**2.0 -                             &
                       (0.1715*(0.0264/smr_sat(l))) + 0.03144 ) )

          fOw_S(l) = fOw_S_min * 0.3
        END IF

        ! For Loam
        IF ((smr_loc(l) >= 0.0537) .AND. (smr_sat(l) <= 0.851)) THEN
          fOw_L(l) = 0.01997 * ( (((smr_loc(l)/smr_sat(l))-0.05369)*           &
                    (0.8508-(smr_loc(l)/smr_sat(l))) ) /                       &
                    ( (smr_loc(l)/smr_sat(l))**2.0 -                           &
                     (0.7541*(smr_loc(l)/smr_sat(l))) + 0.2806 ) )
        ELSE ! same as fOw_S
          fOw_L_min = 0.01997 * ( (((0.0537/smr_sat(l))-0.05369)*              &
                      (0.8508-(0.0537/smr_sat(l))) ) /                         &
                      ( (0.0537/smr_sat(l))**2.0 -                             &
                       (0.7541*(0.0537/smr_sat(l))) + 0.2806 ) )
          fOw_L(l)   = fOw_L_min * 0.3
        END IF

        ! Set parameters according to soil type
        fOw(l)   = (soil_sand(l) * fOw_S(l)) + (soil_loam(l) * fOw_L(l))
        delta(l) = ((soil_sand(l) * delta_S(l)) + (soil_loam(l) * delta_L(l)))

        ! kO_a : depedence on soil moisture (s-1)
        kO_a(l) = a(l) * fOw(l) * g_T(l)

        ! Calculate surface resistance rc (s cm-1)
        ! This can be inverted to produce velocity depositon (cm s-1) according
        ! to soil uptake only (no other resistances)

        ! Diffusivity
        difus(l) = delta(l) / D_s(l)

        ! Snow diffusivity
        snow_difus(l) = snow_depth_gb(l) / D_snow(l)

        ! Soil uptake
        IF (kO_a(l) > 0.0 ) THEN
          uptake(l) = 1.0 / SQRT(D_s(l) * kO_a(l))
        ELSE
          uptake(l) = dep_rnull
        END IF

        IF (uptake(l) < dep_rnull) THEN
          ! Need to make sure there is uptake occurring
          vd_h2(i,j) = 1.0 / (difus(l) + snow_difus(l) + uptake(l))

          DO n = 1, ntype
            IF ((n == urban) .OR. (n == lake)) THEN
              ! Urban and water surfaces
              rc_h2(i,j,n)     = dep_rnull
            ELSE
              ! All other surfaces
              rc_h2(i,j,n) = difus(l) + snow_difus(l) + uptake(l)

              ! convert s cm-1 to s m-1
              rc_h2(i,j,n) = rc_h2(i,j,n) * 100.0
            END IF      ! urban + lake
          END DO        ! ntype

        ELSE
          ! No uptake/difusion
          rc_h2(i,j,:) = dep_rnull
          vd_h2(i,j) = 1.0 / dep_rnull

        END IF          ! Uptake + difusion
      END IF            ! Total porosity
    END IF              ! Land points

  END DO
END DO

! write(*,*) 'Average h2 vd deposition:'
! write(*,*) sum(vd_h2) / size(vd_h2)
! write(*,*) 'Minimum', minval(vd_h2)
! write(*,*) '------------------'

!-------------------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN

END SUBROUTINE ! depositon_ukca_h2dd_soil
END MODULE deposition_ukca_h2dd_soil_mod
