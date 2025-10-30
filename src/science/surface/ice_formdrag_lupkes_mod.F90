! *****************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
! *****************************COPYRIGHT****************************************

MODULE ice_formdrag_lupkes_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='ICE_FORMDRAG_LUPKES_MOD'

CONTAINS
SUBROUTINE ice_formdrag_lupkes (                                               &
 flandg, ice_fract,                                                            &
 z0m_ice, z0m_sea, cd_int_stb, cd_sea_stb,                                     &
 z1_tq_sea,z1_tq_top_sea,                                                      &
 cd_frm                                                                        &
 )

USE atm_fields_bounds_mod, ONLY: tdims

USE jules_sea_seaice_mod, ONLY:                                                &
  l_stability_lupkes,                                                          &
  h_freeboard_min, h_freeboard_max, beta_floe,                                 &
  d_floe_min, d_floe_max, ss_floe, ce_floe

USE planet_constants_mod, ONLY: vkman
USE jules_surface_mod, ONLY: i_modiscopt, on

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE


! Arguments
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
                    flandg(tdims%i_start:tdims%i_end,                          &
                           tdims%j_start:tdims%j_end)
! Land fractions in each grid box
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
                    ice_fract(tdims%i_start:tdims%i_end,                       &
                              tdims%j_start:tdims%j_end)
! Ice fractions (this scheme will not be called with multiple categories)
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
                    z1_tq_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
! Height of lowest model level relative to sea
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
                    z1_tq_top_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
! Height of top of lowest model level relative to sea
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
                    z0m_ice(tdims%i_start:tdims%i_end,                         &
                            tdims%j_start:tdims%j_end)
! Momemtum roughness length over ice
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
                    z0m_sea(tdims%i_start:tdims%i_end,                         &
                            tdims%j_start:tdims%j_end)
! Momemtum roughness length over open water
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
                    cd_int_stb(tdims%i_start:tdims%i_end,                      &
                               tdims%j_start:tdims%j_end)
! Interfacial drag coefficient over pack ice (including stability)
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
                    cd_sea_stb(tdims%i_start:tdims%i_end,                      &
                               tdims%j_start:tdims%j_end)
! Drag coefficient over open ocean (including stability)

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
                     cd_frm(tdims%i_start:tdims%i_end,                         &
                            tdims%j_start:tdims%j_end)
! Drag coefficient for form drag

! Local variables

INTEGER :: i, j
!         Loop variables

REAL(KIND=real_jlslsm) :: a_star
!         Local scalar in calculation of floe width
REAL(KIND=real_jlslsm) :: hf
!         Height of freeboard
REAL(KIND=real_jlslsm) :: di
!         Cross-wind length of floe
REAL(KIND=real_jlslsm) :: dw
!         Separation of floes
REAL(KIND=real_jlslsm) :: shlt
!         Sheltering factor
REAL(KIND=real_jlslsm) :: int_u2_ice
!         Integral of u^2 up to height of free-board using profile
!         over ice
REAL(KIND=real_jlslsm) :: int_u2_sea
!         Integral of u^2 up to height of free-board using profile
!         over sea
REAL(KIND=real_jlslsm) :: cd_sea
!         Drag coefficient over sea for calculation of form drag
REAL(KIND=real_jlslsm) :: cd_int
!         Drag coefficient over sea ice for calculation of form drag

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='ICE_FORMDRAG_LUPKES'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)


! Precalculated terms
a_star = 1.0 / ( 1.0 - (d_floe_min / d_floe_max)** (1.0 / beta_floe) )

!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE)                               &
!$OMP PRIVATE(i,j,hf,di,dw,shlt,int_u2_sea,int_u2_ice,cd_sea,cd_int)           &
!$OMP SHARED(tdims,flandg,ice_fract,h_freeboard_max,h_freeboard_min,a_star,    &
!$OMP        beta_floe,d_floe_min,ss_floe,z0m_sea,z0m_ice,l_stability_lupkes,  &
!$OMP        cd_sea_stb,cd_int_stb,i_modiscopt,z1_tq_top_sea,z1_tq_sea,        &
!$OMP        cd_frm,ce_floe)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end

    IF (flandg(i,j) < 1.0) THEN
      IF (ice_fract(i,j) > SQRT(EPSILON(ice_fract))) THEN

        !       Free-board height
        hf = h_freeboard_max * ice_fract(i,j) +                                &
             h_freeboard_min * (1.0 - ice_fract(i,j))

        !       Cross-wind flow length and sheltering. The sheltering is
        !       assumed to be the same for flow over water or ice.
        di = d_floe_min * ( a_star / (a_star - ice_fract(i,j)) )** beta_floe
        dw = di * ( 1.0 / SQRT(ice_fract(i,j)) - 1.0 )
        shlt = 1.0 - EXP( - ss_floe * dw / hf )

        !       Integral of U^2 for a logarithmic profile up to the freeboard.
        !       NB. The use of the full form of the integral follows more
        !       closely Lupkes and Gryanik (2015) than Lupkes et a. (2012).
        int_u2_sea = (LOG(1.0 + hf / z0m_sea(i,j)) - 1.0)**2 + 1.0
        int_u2_ice = (LOG(1.0 + hf / z0m_ice(i,j)) - 1.0)**2 + 1.0

        !       If the stability dependence is included, the existing drag
        !       coefficients can be used, but otherwise we require the neutral
        !       coefficients.
        IF (l_stability_lupkes) THEN
          cd_sea = cd_sea_stb(i,j)
          cd_int = cd_int_stb(i,j)
        ELSE
          !         Calculate neutral drag coefficients accounting for the
          !         method of discretization of the bottom layer.
          IF (i_modiscopt == on) THEN
            cd_sea = (vkman / LOG(1.0 + z1_tq_top_sea(i,j) /                   &
                                   z0m_sea(i,j)))**2
            cd_int = (vkman / LOG(1.0 + z1_tq_top_sea(i,j) /                   &
                                   z0m_ice(i,j)))**2
          ELSE
            cd_sea = (vkman / LOG(1.0 + z1_tq_sea(i,j) /                       &
                                   z0m_sea(i,j)))**2
            cd_int = (vkman / LOG(1.0 + z1_tq_sea(i,j) /                       &
                                   z0m_ice(i,j)))**2
          END IF
        END IF

        !       Overall form drag coefficient per unit area of ice
        cd_frm(i,j)  = (ce_floe / 2.0) * (1.0 / (vkman**2)) *                  &
                         ( hf / di ) * shlt**2 * (                             &
                           (1.0 - ice_fract(i,j)) * cd_sea * int_u2_sea +      &
                           ice_fract(i,j) * cd_int * int_u2_ice)

      ELSE

        !       Set a sensible default.
        cd_frm(i,j) = 0.0

      END IF
    END IF
  END DO
END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE ice_formdrag_lupkes
END MODULE ice_formdrag_lupkes_mod

