! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Subroutine SOILCARB_MIX --------------------------------------------------------
!
! Purpose : Defines the mixing terms for l_layeredC.
!
! ----------------------------------------------------------------------------
MODULE soilcarb_mix_mod

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SOILCARB_MIX_MOD'

CONTAINS

SUBROUTINE soilcarb_mix(land_pts, trif_pts, trif_index, soil_bgc,              &
                        t_soil_soilt_acc, mix_term, mix_s)

USE ancil_info, ONLY: dim_cslayer, nsoilt
USE conversions_mod, ONLY: zerodegc
USE jules_soil_mod, ONLY: dzsoil, sm_levels

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN)::                                                          &
  land_pts,                                                                    &
    ! Total number of land points.
  trif_pts,                                                                    &
    ! Number of points on which TRIFFID may operate.
  trif_index(land_pts)
    ! Indices of land points on which TRIFFID may operate.

REAL(KIND=real_jlslsm), INTENT(IN)::                                           &
  soil_bgc(land_pts,dim_cslayer,4)
    ! currently either soil C or soil N in kg /m2

REAL(KIND=real_jlslsm), INTENT(IN)::                                           &
  t_soil_soilt_acc(land_pts,nsoilt,sm_levels)

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT)::                                          &
  mix_term(land_pts,dim_cslayer,4),                                            &
    ! Diffusion term for transfer between soil layers (kg/m^2/360days)
    ! this can be applied to the soil C or N
    ! depending on context
  mix_s(land_pts,dim_cslayer-1,4)
    ! Diffusion coefficient for soil C between soil layers (m^2/360days)
    ! Equation 15 of Burke et al. (2017)
    ! https://www.geosci-model-dev.net/10/959/2017/gmd-10-959-2017.pdf
!-----------------------------------------------------------------------------
! Local scalar parameters.
!-----------------------------------------------------------------------------
! Parameters for mixing
REAL(KIND=real_jlslsm), PARAMETER :: botuniform   = 1.0
    ! Bottom of the uniform part of mixing in the soil (m).
REAL(KIND=real_jlslsm), PARAMETER :: botlinear    = 3.0
    ! Bottom of the linearly reducing part of mixing in the soil (m).
REAL(KIND=real_jlslsm), PARAMETER :: bioturb_mix  = 0.0001
    ! BIOTURBATION mixing in (m^2/360days).
REAL(KIND=real_jlslsm), PARAMETER :: cryoturb_mix  = 0.0005
    ! CRYOTURBATION mixing (m^2/360days).

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, l, t, n  ! Loop counters

REAL(KIND=real_jlslsm) ::                                                      &
  zsoil,                                                                       &
    ! soil depth (m)
  mixparam,                                                                    &
    ! mixing rate (m^2/360days)
  acc_rates(4)
    ! Acceleration rates for quick pool spinup not here yet.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SOILCARB_MIX'

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)


! Initialisation.
mix_s(:,:,:)      = 0.0
mix_term(:,:,:)   = 0.0
acc_rates         = [1.0,1.0,1.0,1.0 ]

!Calculate the mixing parameters
DO t = 1,trif_pts
  l = trif_index(t)
  IF (t_soil_soilt_acc(l,1,dim_cslayer) < zerodegc) THEN
    mixparam = cryoturb_mix
  ELSE
    mixparam = bioturb_mix
  END IF
  zsoil = 0.0
  DO n = 1,dim_cslayer-1
    zsoil = zsoil + dzsoil(n)
    IF (zsoil < botuniform) THEN
      mix_s(l,n,1) = mixparam * acc_rates(1)
      mix_s(l,n,2) = mixparam * acc_rates(2)
      mix_s(l,n,3) = mixparam * acc_rates(3)
      mix_s(l,n,4) = mixparam * acc_rates(4)
    ELSE IF (zsoil < botlinear) THEN
      mix_s(l,n,1) = mixparam  * acc_rates(1) *                                &
                     (1.0 - (zsoil - botuniform) / (botlinear - botuniform))
      mix_s(l,n,2) = mixparam * acc_rates(2) *                                 &
                     (1.0 - (zsoil - botuniform) / (botlinear - botuniform))
      mix_s(l,n,3) = mixparam * acc_rates(3) *                                 &
                     (1.0 - (zsoil - botuniform) / (botlinear - botuniform))
      mix_s(l,n,4) = mixparam * acc_rates(4) *                                 &
                     (1.0 - (zsoil - botuniform) / (botlinear - botuniform))
    ELSE
      mix_s(l,n,1) = 0.00000000001
      mix_s(l,n,2) = 0.00000000001
      mix_s(l,n,3) = 0.00000000001
      mix_s(l,n,4) = 0.00000000001
    END IF
  END DO
END DO

! Calculate the mixing terms applied to either the soil carbon, or the
! soil c:n ratios depending on the call (generic name used here is soil_bgc).
DO t = 1,trif_pts
  l = trif_index(t)
  DO i = 1,4  !soil carbon pools
    mix_term(l,1,i) = mix_s(l,1,i) * ( (soil_bgc(l,2,i) / dzsoil(2))           &
                      - (soil_bgc(l,1,i) / dzsoil(1)) )                        &
                      / ( 0.5 * (dzsoil(2) + dzsoil(1)) )
    DO n = 2,dim_cslayer-1
      mix_term(l,n,i) = mix_s(l,n,i) * ( (soil_bgc(l,n+1,i)                    &
                        / dzsoil(n+1)) - (soil_bgc(l,n,i) / dzsoil(n)) )       &
                        / ( 0.5 * (dzsoil(n+1) + dzsoil(n)) )                  &
                        - mix_s(l,n-1,i) *( (soil_bgc(l,n,i)                   &
                        /  dzsoil(n)) - (soil_bgc(l,n-1,i) / dzsoil(n-1)) )    &
                        / ( 0.5 * (dzsoil(n) + dzsoil(n-1)) )
    END DO
    mix_term(l,dim_cslayer,i) = mix_s(l,dim_cslayer-1,i) *                     &
                                ( (soil_bgc(l,dim_cslayer-1,i)                 &
                                / dzsoil(dim_cslayer-1))                       &
                                - (soil_bgc(l,dim_cslayer,i)                   &
                                / dzsoil(dim_cslayer)) )                       &
                                / ( 0.5 * (dzsoil(dim_cslayer)                 &
                                + dzsoil(dim_cslayer-1)) )
    ! The mixing has to be done in terms of density (kg/m3) but
    ! converted back to kg/m2 for the increment, hence no overall factor
    ! of 1/dzsoil(n) as it cancels out.
  END DO
END DO


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE soilcarb_mix
END MODULE soilcarb_mix_mod