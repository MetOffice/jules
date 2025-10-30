! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Subroutine SOILCARB_TRACER_AGE
!
! Purpose : Tracks soil carbon age or labels a fraction of carbon.
!
! Note that triffid is not compatible with soil tiling at this time, so
! all soilt variables have their soil tile index hard-coded to 1
!
! -------------------------------------------------------------------
MODULE soilcarb_tracer_age_mod

USE um_types, ONLY: real_jlslsm

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SOILCARB_TRACER_AGE_MOD'

CONTAINS

SUBROUTINE soilcarb_tracer_age(land_pts, trif_pts, trif_index, mix_s, resp_s,  &
                    resp_frac_cspool, cs_orig, lit_c_t, lit_frac,              &
                    dpm_ratio_gb, frac_label_age_pool, r_gamma,                &
                    l_soilage)

USE ancil_info, ONLY: dim_cslayer, dim_cs1
USE jules_soil_mod, ONLY: dzsoil
USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Total number of land points.
  trif_pts,                                                                    &
    ! Number of points on which TRIFFID may operate
  trif_index(land_pts)
    ! Indices of land points where TRIFFID may operate

LOGICAL, INTENT(IN) ::                                                         &
  l_soilage
    ! if this is true then use this code to calculate soil age
    ! otherwise a fraction of the soil carbon is labelled and traced
    ! this is not currently used but will be once soil C accumulation is added

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  dpm_ratio_gb(land_pts),                                                      &
   ! Gridbox mean DPM ratio of litter input (.)
  lit_c_t(land_pts),                                                           &
    ! Total carbon litter (kg C/m2/360days)
  lit_frac(dim_cslayer),                                                       &
    ! Litter fraction into each layer (.)
  mix_s(land_pts,dim_cslayer-1,dim_cs1),                                       &
    ! Diffusion coefficient for soil C between soil layers (m^2/360days)
  resp_s(land_pts,dim_cslayer,dim_cs1+1),                                      &
    ! Soil respiration (kg C/m2/360days)
  resp_frac_cspool(land_pts,dim_cslayer,dim_cs1),                              &
    ! The fraction of resp_s (soil respiration) that forms new soil C.
    ! This is the fraction that is NOT released to the atmosphere.
  cs_orig(land_pts,dim_cslayer,dim_cs1),                                       &
    ! The value of the soil carbon at the beginning of the timestep (kg C/m2)
  r_gamma
    ! Inverse timestep (/360days)

REAL(KIND=real_jlslsm), INTENT(IN OUT)::                                       &
  frac_label_age_pool(land_pts,dim_cslayer,dim_cs1)
    ! Fraction of carbon that is labelled or given an age
    ! in each pool and layer (.)

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
 l, t, n, ipool                ! Loop counters

REAL(KIND=real_jlslsm) ::                                                      &
  cs_b4_input(dim_cslayer,dim_cs1),                                            &
    ! Amount of original soil C before any other carbon is input (kg C/m2)
  label_cs_added(dim_cslayer,dim_cs1),                                         &
    ! Amount of carbon input into pool multiplied by
    ! fraction of labelled C (kg)
  cs_added(dim_cslayer,dim_cs1),                                               &
    ! Total carbon input into pool (kg)
  litterin(dim_cslayer,2),                                                     &
    ! Litter inputs (kg C/m2/360days) only into dpm & rpm pools (1&2)
  invdz2_nplus1_term(dim_cslayer),                                             &
    ! 2.0 / dz**2 for n+1 layer (m-2)
  invdz2_nminus1_term(dim_cslayer),                                            &
    ! 2.0 / dz**2 for n-1 layer (m-2)
  inv_r_gamma,                                                                 &
    ! 1/r_gamma for optimal calculations
  mix_term1(dim_cslayer,dim_cs1),                                              &
    ! temporary term in soil carbon mixing calculation
  mix_term2(dim_cslayer,dim_cs1),                                              &
    ! temporary term in soil carbon mixing calculation
  mix_term1_label(dim_cslayer,dim_cs1),                                        &
    ! temporary term for labelled soil carbon mixing calculation
  mix_term2_label(dim_cslayer,dim_cs1),                                        &
    ! temporary term for labelled soil carbon mixing calculation
  resp_cs_retain(dim_cslayer,dim_cs1),                                         &
    ! Amount of respiration from all carbon that is retained in the
    ! soil carbon pools (kg C/m2/360days)
  resp_label_cs_retain(dim_cslayer,dim_cs1),                                   &
    ! Amount of respiration from labelled carbon that is retained in the
    ! soil carbon pools (kg C/m2/360days)
  resp_cs_retain_multi(dim_cs1),                                               &
    ! fraction of respiration retained for each pool defined below
    ! zero for pools 1 and 2 and pools 3 and 4 need input from pools 1 and 2
  dz(dim_cslayer)
    ! layer thicknesses for this calculation
    ! will change when l_accumulate_soilc is added later

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SOILCARB_TRACER_AGE'

!-----------------------------------------------------------------------------
! End of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)


! See eq. 72 & 73 in Clark et al. (2011); doi:10.5194/gmd-4-701-2011
! for definition of 0.46 and 0.54
resp_cs_retain_multi = [0.0, 0.0, 0.46, 0.54 ]

inv_r_gamma = 1.0 / r_gamma
dz(:) = dzsoil(:)

DO t = 1,trif_pts
  l = trif_index(t)

  !-------------------------------------------------------------------
  ! Define the litter inputs to pools 1 (dpm) and 2 (rpm)
  !-------------------------------------------------------------------
  DO n = 1,dim_cslayer
    litterin(n,1) = dpm_ratio_gb(l) * lit_c_t(l) * lit_frac(n)
    litterin(n,2) = (1.0 - dpm_ratio_gb(l)) * lit_c_t(l) * lit_frac(n)
  END DO

  !-------------------------------------------------------------------
  ! amount of respiration from all C & labelled C retained in C pools
  ! zero for pools 1 and 2 and pools 3 and 4 need input from pools 1 and 2
  !-------------------------------------------------------------------
  DO ipool = 1,dim_cs1 ! loop over pools
    DO n = 1,dim_cslayer
      resp_cs_retain(n,ipool) = resp_cs_retain_multi(ipool) *                  &
                          SUM( resp_frac_cspool(l,n,1:4) * resp_s(l,n,1:4) )
      resp_label_cs_retain(n,ipool) = resp_cs_retain_multi(ipool) *            &
                         SUM( resp_frac_cspool(l,n,1:4) *                      &
                         frac_label_age_pool(l,n,1:4) * resp_s(l,n,1:4) )
    END DO
  END DO

  !-------------------------------------------------------------------
  ! Calculate mixing term used to calculate "original" carbon before any changes
  !-------------------------------------------------------------------
  mix_term1(:,:) = 0.0
  mix_term2(:,:) = 0.0
  DO ipool = 1,dim_cs1
    DO n = 1, dim_cslayer - 1
      mix_term1(n,ipool) = mix_s(l,n,ipool) / (dzsoil(n) + dzsoil(n+1))
    END DO
    DO n = 2, dim_cslayer
      mix_term2(n,ipool) = mix_s(l,n-1,ipool) / (dzsoil(n-1) + dzsoil(n))
    END DO

    !-------------------------------------------------------------------
    ! How much of the "original" carbon is left in the pool after the timestep
    ! before any other carbon is input
    !-------------------------------------------------------------------
    DO n = 1, dim_cslayer
      cs_b4_input(n,ipool) = cs_orig(l,n,ipool) -                              &
                       ( resp_s(l,n,ipool) + 2.0 * cs_orig(l,n,ipool)          &
                         * ( mix_term1(n,ipool) + mix_term2(n,ipool) )         &
                       / dzsoil(n) ) * inv_r_gamma
    END DO
  END DO

  !-------------------------------------------------------------------
  ! calculate 2.0 / dz**2 for n+1 and n-1 layers (m-2)
  !-------------------------------------------------------------------
  invdz2_nplus1_term(:) = 0.0
  invdz2_nminus1_term(:) = 0.0
  DO n = 1, dim_cslayer - 1
    invdz2_nplus1_term(n) = 2.0 / ( dzsoil(n+1) * (dzsoil(n) + dzsoil(n+1)) )
  END DO
  DO n = 2, dim_cslayer
    invdz2_nminus1_term(n) = 2.0 / ( dzsoil(n-1) * (dzsoil(n) + dzsoil(n-1)) )
  END DO

  !-------------------------------------------------------------------
  ! calculate mixing term for added soil C and labelled and added soil C
  !-------------------------------------------------------------------
  mix_term1(:,:) = 0.0
  mix_term2(:,:) = 0.0
  mix_term1_label(:,:) = 0.0
  mix_term2_label(:,:) = 0.0
  DO ipool = 1,dim_cs1
    DO n = 1,dim_cslayer-1
      mix_term1(n,ipool) = mix_s(l,n,ipool) * cs_orig(l,n+1,ipool)
      mix_term1_label(n,ipool) = mix_term1(n,ipool) *                          &
                                           frac_label_age_pool(l,n+1,ipool)
    END DO
    DO n = 2, dim_cslayer
      mix_term2(n,ipool) = mix_s(l,n-1,ipool) * cs_orig(l,n-1,ipool)
      mix_term2_label(n,ipool) = mix_term2(n,ipool) *                          &
                                           frac_label_age_pool(l,n-1,ipool)
    END DO

    !-------------------------------------------------------------------
    ! how much soil c and labelled soilc is added?
    !-------------------------------------------------------------------
    DO n = 1, dim_cslayer
      cs_added(n,ipool) = ( mix_term1(n,ipool) * invdz2_nplus1_term(n) +       &
                            mix_term2(n,ipool) * invdz2_nminus1_term(n) +      &
                            resp_cs_retain(n,ipool) ) * inv_r_gamma
      label_cs_added(n,ipool) = ( mix_term1_label(n,ipool) *                   &
                                  invdz2_nplus1_term(n) +                      &
                                  mix_term2_label(n,ipool) *                   &
                                  invdz2_nminus1_term(n) +                     &
                                  resp_label_cs_retain(n,ipool) ) * inv_r_gamma
    END DO
  END DO

  !-------------------------------------------------------------------
  ! Add litter to the carbon going in OR out (latter deals with -ve litter)
  !-------------------------------------------------------------------
  DO n = 1,dim_cslayer
    DO ipool = 1,2
      IF (litterin(n,ipool) < 0.0) THEN
        cs_b4_input(n,ipool) = cs_b4_input(n,ipool) + litterin(n,ipool) *      &
                                                                  inv_r_gamma
      ELSE
        cs_added(n,ipool) = cs_added(n,ipool) + litterin(n,ipool) * inv_r_gamma
      END IF
    END DO
  END DO

  !-------------------------------------------------------------------
  ! Calculate labelled carbon fractions/age of carbon
  !-------------------------------------------------------------------
  DO n = 1,dim_cslayer
    DO ipool = 1,dim_cs1
      IF (cs_b4_input(n,ipool) > 0.0) THEN
        IF ( (cs_added(n,ipool) + cs_b4_input(n,ipool)) > 1.0e-8 ) THEN
          frac_label_age_pool(l,n,ipool) =                                     &
            ( frac_label_age_pool(l,n,ipool) * cs_b4_input(n,ipool) +          &
              label_cs_added(n,ipool) ) / ( cs_added(n,ipool) +                &
                                            cs_b4_input(n,ipool) )
        END IF
      ELSE IF (cs_added(n,ipool) > 1.0e-8) THEN
        frac_label_age_pool(l,n,ipool) = label_cs_added(n,ipool) /             &
                                                          cs_added(n,ipool)
      ELSE IF (cs_b4_input(n,ipool) <= 0.0 .AND. cs_added(n,ipool) <= 0.0) THEN
        frac_label_age_pool(l,n,ipool) = 0.0
      END IF
      IF (frac_label_age_pool(l,n,ipool) < 0.0) THEN
        ! Only ever goes a tiny bit below zero - numerical imprecision
        frac_label_age_pool(l,n,ipool) = 0.0
      END IF
    END DO
  END DO

END DO ! land points.

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE soilcarb_tracer_age
END MODULE soilcarb_tracer_age_mod
