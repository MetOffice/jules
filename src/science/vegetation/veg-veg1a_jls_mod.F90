! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Version 1A of vegetation section: models leaf phenology

! Subroutine Interface:
MODULE veg1_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='VEG1_MOD'

CONTAINS

SUBROUTINE veg1(                                                               &
               land_pts, nsurft,                                               &
               a_step, phenol_period, atimestep,                               &
               satcon_soilt_sfc,  z0m_soil, l_phenol,                          &
               g_leaf_ac, g_leaf_phen_ac, frac, lai, ht,                       &
               catch_s, catch_t, infil_t,                                      &
               g_leaf_day, g_leaf_phen, lai_phen,                              &
               z0_t, z0h_t, l_lice_point, l_lice_surft, ztm_gb                 &
               )

!Use in relevant subroutines
USE sparm_mod,                ONLY: sparm
USE infiltration_rate_mod,    ONLY: infiltration_rate
USE tilepts_mod,              ONLY: tilepts

!Use in variables
USE jules_surface_types_mod,  ONLY: npft, ntype, nnpft

USE jules_vegetation_mod,     ONLY: l_gleaf_fix

USE ancil_info,               ONLY: nsoilt

USE conversions_mod,          ONLY: rsec_per_day

USE phenol_mod,               ONLY: phenol

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

! Description:
!   Updates Leaf Area Index for Plant Functional Types (PFTs) and uses
!   this to derive new vegetation parameters for PFTs along with gridbox
!   mean values where appropriate.

! Method:
!   Calls PHENOL which models phenology and updates Leaf Area Index
!   (LAI), then passes new LAI into SPARM along with canopy height
!   and fractional cover of Plant Functional Types.  SPARM uses this to
!   derive the vegetation parameters for each PFT, and also derives
!   gridbox means where this is required.

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Number of land points to be processed.
  nsurft,                                                                      &
    ! Number of land-surface tiles.
  a_step,                                                                      &
    ! Atmospheric timestep number.
  phenol_period
    ! Phenology period (days).

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  atimestep
    ! Atmospheric timestep (s).

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  satcon_soilt_sfc(land_pts,nsoilt),                                           &
    ! Saturated hydraulic conductivity of the soil surface (kg/m2/s).
  z0m_soil(land_pts)
    ! z0m of bare soil (m).

LOGICAL, INTENT(IN) ::                                                         &
  l_phenol
    ! .T. for interactive leaf phenology.

!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  g_leaf_ac(land_pts,npft),                                                    &
    ! Accumulated leaf turnover rate.
  g_leaf_phen_ac(land_pts,npft),                                               &
    ! Accumulated mean leaf turnover rate
    ! over phenology period (/360days)
  frac(land_pts,ntype),                                                        &
    ! Fractions of surface types.
  lai(land_pts,npft),                                                          &
    ! LAI of plant functional types.
  ht(land_pts,npft)
    ! Height of plant functional types (m).

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  catch_s(land_pts,nsurft),                                                    &
    ! Snow capacity for tiles (kg/m2).
  catch_t(land_pts,nsurft),                                                    &
    ! Canopy capacity for tiles (kg/m2).
  infil_t(land_pts,nsurft),                                                    &
    ! Maximum surface infiltration rate for tiles
    ! (kg/m2/s).
  g_leaf_day(land_pts,npft),                                                   &
    ! Mean leaf turnover rate for input to PHENOL
    ! (/360days).
  g_leaf_phen(land_pts,npft),                                                  &
    ! Mean leaf turnover rate over phenology period (/360days).
  lai_phen(land_pts,npft),                                                     &
    ! LAI of PFTs after phenology. Required as separate variable for top-level
    ! argument list matching with VEG_IC2A.
  z0_t(land_pts,nsurft),                                                       &
    ! Roughness length for tiles (m).
  z0h_t(land_pts,nsurft),                                                      &
    ! Thermal roughness length for tiles (m).
  ztm_gb(land_pts)
    ! Roughness length

LOGICAL, INTENT(IN) :: l_lice_point(land_pts)
LOGICAL, INTENT(IN OUT) :: l_lice_surft(ntype)

!-----------------------------------------------------------------------------
! Local integer variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  j,l,n,                                                                       &
    ! Loop counters.
  nstep_phen
    ! Number of atmospheric timesteps between calls to PHENOL.

INTEGER ::                                                                     &
  surft_pts(ntype),                                                            &
    ! Number of land points which include the nth surface type.
  surft_index(land_pts,ntype)
    ! Indices of land points which include the nth surface type.

!-----------------------------------------------------------------------------
! Local real variables.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  dtime_phen
    ! The phenology timestep (yr).

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='VEG1'

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Initialisations
!-----------------------------------------------------------------------------
DO n = 1,nsurft
  DO l = 1,land_pts
    catch_s(l,n) = 0.0
    catch_t(l,n) = 0.0
    infil_t(l,n) = 0.0
    z0_t(l,n)    = 0.0
  END DO
END DO

DO n = 1,npft
  DO l = 1,land_pts
    g_leaf_phen(l,n) = 0.0
    g_leaf_day(l,n)  = 0.0
  END DO
END DO

!-----------------------------------------------------------------------------
! Calculate the number of atmospheric timesteps between calls to PHENOL
! and TRIFFID.
!-----------------------------------------------------------------------------
nstep_phen = INT(rsec_per_day * phenol_period / atimestep)

!-----------------------------------------------------------------------------
! Create the surft_index array of land points with each surface type
!-----------------------------------------------------------------------------
CALL tilepts(land_pts, frac, surft_pts, surft_index, l_lice_point, l_lice_surft)

IF (l_phenol .AND. MOD(a_step,nstep_phen) == 0) THEN

  !---------------------------------------------------------------------------
  ! Calculate the phenology timestep in years.
  !---------------------------------------------------------------------------
  dtime_phen = REAL(phenol_period) / 360.0


  DO n = 1,nnpft

    !-------------------------------------------------------------------------
    ! Calculate the mean turnover rate and update the leaf phenological
    ! state.
    !-------------------------------------------------------------------------
    DO j = 1,surft_pts(n)
      l = surft_index(j,n)
      g_leaf_day(l,n) = g_leaf_ac(l,n) / dtime_phen
    END DO

    CALL phenol (land_pts, surft_pts(n), n, surft_index(:,n), dtime_phen,      &
                 g_leaf_day(:,n), ht(:,n),                                     &
                 lai(:,n), g_leaf_phen(:,n))

    DO l = 1,land_pts
      lai_phen(l,n) = lai(l,n)
    END DO

    IF ( l_gleaf_fix ) THEN
      !gleaf fix copied from veg-veg2a
      !-----------------------------------------------------------------------
      ! Increment the leaf turnover rate for driving TRIFFID and reset
      ! the accumulation over atmospheric model timesteps to zero.
      !-----------------------------------------------------------------------
      DO j = 1,surft_pts(n)
        l = surft_index(j,n)
        g_leaf_phen_ac(l,n) = g_leaf_phen_ac(l,n)                              &
                              + (g_leaf_phen(l,n) * dtime_phen)
      END DO
    END IF

    !-------------------------------------------------------------------------
    ! Reset the accumulation over atmospheric model timesteps to zero.
    !-------------------------------------------------------------------------
    DO l = 1,land_pts
      g_leaf_ac(l,n) = 0.0
    END DO
  END DO
END IF

!-----------------------------------------------------------------------------
! Calculate gridbox mean vegetation parameters from fractions of
! surface functional types
!-----------------------------------------------------------------------------
CALL sparm (land_pts, nsurft, surft_pts, surft_index,                          &
            frac, ht, lai, z0m_soil,                                           &
            catch_s, catch_t, z0_t, z0h_t, ztm_gb)

CALL infiltration_rate(land_pts, nsurft, surft_pts, surft_index,               &
                       satcon_soilt_sfc, frac, infil_t)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE veg1
END MODULE veg1_mod
