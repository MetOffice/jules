!-----------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
MODULE params_io_mod_cbl

!-----------------------------------------------------------------------------
! Description:
!   Defines variable types and variables for veg and soil parameters.
!   We only define the pointer associations relevant to TYPES to be parssed
!   around (following JULES params*_io). The allocation is unecessary as these
!   input parameters are read in from namelist (following JULES params *_io),
!   these are initialized in corresponding section
!   Based on cable_def_types_mod.F90 from the CABLE trunk.
!
! This MODULE is USEd by:
!      cable_fields_mod.F90,
!      init_vegin_cbl.inc
!
! This MODULE contains 2 public Subroutines:
!      params_io_assoc_cbl,
!      params_io_nullify_cbl
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in CABLE SCIENCE
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

PUBLIC :: params_io_data_type
PUBLIC :: params_io_type
PUBLIC :: params_io_assoc_cbl

PRIVATE
! CABLE constants. They will be sourced from CABLE modules once the
! compilation is done. ccc25
INTEGER, PARAMETER :: ntype_max = 17
INTEGER, PARAMETER :: nsoil_max = 9
INTEGER, PARAMETER :: nrb = 3                  !# rad bands VIS/NIR + Legacy LW
INTEGER, PARAMETER :: nsl = 6                  !# soil layers
INTEGER, PARAMETER :: nscs = 2                 !# soil carbon stores
INTEGER, PARAMETER :: nvcs = 3                 !# vegetation carbon stores

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='PARAMS_IO_MOD_CBL'

! Vegetation/Soil parameters I/O:
TYPE :: params_io_data_type

  ! Veg parameters I/O:
  REAL ::                                                                      &
    vegin_canst1(ntype_max),                                                   &
    vegin_dleaf(ntype_max),                                                    &
    vegin_length(ntype_max),                                                   &
    vegin_width(ntype_max),                                                    &
    vegin_vcmax(ntype_max),                                                    &
    vegin_ejmax(ntype_max),                                                    &
    vegin_hc(ntype_max),                                                       &
    vegin_xfang(ntype_max),                                                    &
    vegin_rp20(ntype_max),                                                     &
    vegin_rpcoef(ntype_max),                                                   &
    vegin_rs20(ntype_max),                                                     &
    vegin_wai(ntype_max),                                                      &
    vegin_rootbeta(ntype_max),                                                 &
    vegin_shelrb(ntype_max),                                                   &
    vegin_vegcf(ntype_max),                                                    &
    vegin_frac4(ntype_max),                                                    &
    vegin_xalbnir(ntype_max),                                                  &
    vegin_extkn(ntype_max),                                                    &
    vegin_tminvj(ntype_max),                                                   &
    vegin_tmaxvj(ntype_max),                                                   &
    vegin_vbeta(ntype_max),                                                    &
    vegin_a1gs(ntype_max),                                                     &
    vegin_d0gs(ntype_max),                                                     &
    vegin_alpha(ntype_max),                                                    &
    vegin_convex(ntype_max),                                                   &
    vegin_cfrd(ntype_max),                                                     &
    vegin_gswmin(ntype_max),                                                   &
    vegin_conkc0(ntype_max),                                                   &
    vegin_conko0(ntype_max),                                                   &
    vegin_ekc(ntype_max),                                                      &
    vegin_eko(ntype_max),                                                      &
    vegin_g0(ntype_max),                                                       &
    vegin_g1(ntype_max),                                                       &
    vegin_zr(ntype_max),                                                       &
    vegin_clitt(ntype_max),                                                    &
    vegin_froot(nsl,ntype_max),                                                &
    vegin_csoil(nscs,ntype_max),                                               &
    vegin_ratecs(nscs,ntype_max),                                              &
    vegin_cplant(nvcs,ntype_max),                                              &
    vegin_ratecp(nvcs,ntype_max),                                              &
    vegin_refl(nrb,ntype_max),                                                 &
    vegin_taul(nrb,ntype_max)

  ! Soil parameters I/O:
  REAL ::                                                                      &
    soilin_silt(nsoil_max),                                                    &
    soilin_clay(nsoil_max),                                                    &
    soilin_sand(nsoil_max),                                                    &
    soilin_swilt(nsoil_max),                                                   &
    soilin_sfc(nsoil_max),                                                     &
    soilin_ssat(nsoil_max),                                                    &
    soilin_bch(nsoil_max),                                                     &
    soilin_hyds(nsoil_max),                                                    &
    soilin_sucs(nsoil_max),                                                    &
    soilin_rhosoil(nsoil_max),                                                 &
    soilin_css(nsoil_max)

END TYPE params_io_data_type

TYPE :: params_io_type

  ! Veg parameters I/O:
  REAL, POINTER, PUBLIC ::                                                     &
    vegin_canst1(:),                                                           &
    vegin_dleaf(:),                                                            &
    vegin_length(:),                                                           &
    vegin_width(:),                                                            &
    vegin_vcmax(:),                                                            &
    vegin_ejmax(:),                                                            &
    vegin_hc(:),                                                               &
    vegin_xfang(:),                                                            &
    vegin_rp20(:),                                                             &
    vegin_rpcoef(:),                                                           &
    vegin_rs20(:),                                                             &
    vegin_wai(:),                                                              &
    vegin_rootbeta(:),                                                         &
    vegin_shelrb(:),                                                           &
    vegin_vegcf(:),                                                            &
    vegin_frac4(:),                                                            &
    vegin_xalbnir(:),                                                          &
    vegin_extkn(:),                                                            &
    vegin_tminvj(:),                                                           &
    vegin_tmaxvj(:),                                                           &
    vegin_vbeta(:),                                                            &
    vegin_a1gs(:),                                                             &
    vegin_d0gs(:),                                                             &
    vegin_alpha(:),                                                            &
    vegin_convex(:),                                                           &
    vegin_cfrd(:),                                                             &
    vegin_gswmin(:),                                                           &
    vegin_conkc0(:),                                                           &
    vegin_conko0(:),                                                           &
    vegin_ekc(:),                                                              &
    vegin_eko(:),                                                              &
    vegin_g0(:),                                                               &
    vegin_g1(:),                                                               &
    vegin_zr(:),                                                               &
    vegin_clitt(:),                                                            &
    vegin_froot(:,:),                                                          &
    vegin_csoil(:,:),                                                          &
    vegin_ratecs(:,:),                                                         &
    vegin_cplant(:,:),                                                         &
    vegin_ratecp(:,:),                                                         &
    vegin_refl(:,:),                                                           &
    vegin_taul(:,:)

  ! Soil parameters I/O:
  REAL, POINTER, PUBLIC ::                                                     &
    soilin_silt(:),                                                            &
    soilin_clay(:),                                                            &
    soilin_sand(:),                                                            &
    soilin_swilt(:),                                                           &
    soilin_sfc(:),                                                             &
    soilin_ssat(:),                                                            &
    soilin_bch(:),                                                             &
    soilin_hyds(:),                                                            &
    soilin_sucs(:),                                                            &
    soilin_rhosoil(:),                                                         &
    soilin_css(:)

END TYPE params_io_type

CONTAINS
!==============================================================================
SUBROUTINE params_io_assoc_cbl(pars_io, pars_io_data)

! Description:
!   Associate veg. and soil parameters pointer types

  !No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
TYPE(params_io_type), INTENT(IN OUT)              :: pars_io
TYPE(params_io_data_type), INTENT(IN OUT), TARGET :: pars_io_data
!local:needed by dr_hook
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='PARAMS_IO_ASSOC_CBL'
!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL params_io_nullify_cbl(pars_io)

! Veg parameters I/O:
pars_io % vegin_canst1   =>   pars_io_data % vegin_canst1
pars_io % vegin_dleaf    =>   pars_io_data % vegin_dleaf
pars_io % vegin_length   =>   pars_io_data % vegin_length
pars_io % vegin_width    =>   pars_io_data % vegin_width
pars_io % vegin_vcmax    =>   pars_io_data % vegin_vcmax
pars_io % vegin_ejmax    =>   pars_io_data % vegin_ejmax
pars_io % vegin_hc       =>   pars_io_data % vegin_hc
pars_io % vegin_xfang    =>   pars_io_data % vegin_xfang
pars_io % vegin_rp20     =>   pars_io_data % vegin_rp20
pars_io % vegin_rpcoef   =>   pars_io_data % vegin_rpcoef
pars_io % vegin_rs20     =>   pars_io_data % vegin_rs20
pars_io % vegin_wai      =>   pars_io_data % vegin_wai
pars_io % vegin_rootbeta =>   pars_io_data % vegin_rootbeta
pars_io % vegin_shelrb   =>   pars_io_data % vegin_shelrb
pars_io % vegin_vegcf    =>   pars_io_data % vegin_vegcf
pars_io % vegin_frac4    =>   pars_io_data % vegin_frac4
pars_io % vegin_xalbnir  =>   pars_io_data % vegin_xalbnir
pars_io % vegin_extkn    =>   pars_io_data % vegin_extkn
pars_io % vegin_tminvj   =>   pars_io_data % vegin_tminvj
pars_io % vegin_tmaxvj   =>   pars_io_data % vegin_tmaxvj
pars_io % vegin_vbeta    =>   pars_io_data % vegin_vbeta
pars_io % vegin_a1gs     =>   pars_io_data % vegin_a1gs
pars_io % vegin_d0gs     =>   pars_io_data % vegin_d0gs
pars_io % vegin_alpha    =>   pars_io_data % vegin_alpha
pars_io % vegin_convex   =>   pars_io_data % vegin_convex
pars_io % vegin_cfrd     =>   pars_io_data % vegin_cfrd
pars_io % vegin_gswmin   =>   pars_io_data % vegin_gswmin
pars_io % vegin_conkc0   =>   pars_io_data % vegin_conkc0
pars_io % vegin_conko0   =>   pars_io_data % vegin_conko0
pars_io % vegin_ekc      =>   pars_io_data % vegin_ekc
pars_io % vegin_eko      =>   pars_io_data % vegin_eko
pars_io % vegin_g0       =>   pars_io_data % vegin_g0
pars_io % vegin_g1       =>   pars_io_data % vegin_g1
pars_io % vegin_zr       =>   pars_io_data % vegin_zr
pars_io % vegin_clitt    =>   pars_io_data % vegin_clitt
pars_io % vegin_froot    =>   pars_io_data % vegin_froot
pars_io % vegin_csoil    =>   pars_io_data % vegin_csoil
pars_io % vegin_ratecs   =>   pars_io_data % vegin_ratecs
pars_io % vegin_cplant   =>   pars_io_data % vegin_cplant
pars_io % vegin_ratecp   =>   pars_io_data % vegin_ratecp
pars_io % vegin_refl     =>   pars_io_data % vegin_refl
pars_io % vegin_taul     =>   pars_io_data % vegin_taul

! Soil params_io_type % parameters I/O:
pars_io % soilin_silt    =>   pars_io_data % soilin_silt
pars_io % soilin_clay    =>   pars_io_data % soilin_clay
pars_io % soilin_sand    =>   pars_io_data % soilin_sand
pars_io % soilin_swilt   =>   pars_io_data % soilin_swilt
pars_io % soilin_sfc     =>   pars_io_data % soilin_sfc
pars_io % soilin_ssat    =>   pars_io_data % soilin_ssat
pars_io % soilin_bch     =>   pars_io_data % soilin_bch
pars_io % soilin_hyds    =>   pars_io_data % soilin_hyds
pars_io % soilin_sucs    =>   pars_io_data % soilin_sucs
pars_io % soilin_rhosoil =>   pars_io_data % soilin_rhosoil
pars_io % soilin_css     =>   pars_io_data % soilin_css

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN
END SUBROUTINE params_io_assoc_cbl

SUBROUTINE params_io_nullify_cbl(pars_io)

! Description:
!   Nullify veg. and soil parameters pointer types

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
TYPE(params_io_type), INTENT(IN OUT) :: pars_io
!local:needed by dr_hook
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='PARAMS_IO_NULLIFY_CBL'
!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Veg parameters I/O:
NULLIFY( pars_io%vegin_canst1 )
NULLIFY( pars_io%vegin_dleaf )
NULLIFY( pars_io%vegin_length )
NULLIFY( pars_io%vegin_width )
NULLIFY( pars_io%vegin_vcmax )
NULLIFY( pars_io%vegin_ejmax )
NULLIFY( pars_io%vegin_hc )
NULLIFY( pars_io%vegin_xfang )
NULLIFY( pars_io%vegin_rp20 )
NULLIFY( pars_io%vegin_rpcoef )
NULLIFY( pars_io%vegin_rs20 )
NULLIFY( pars_io%vegin_wai )
NULLIFY( pars_io%vegin_rootbeta )
NULLIFY( pars_io%vegin_shelrb )
NULLIFY( pars_io%vegin_vegcf )
NULLIFY( pars_io%vegin_frac4 )
NULLIFY( pars_io%vegin_xalbnir )
NULLIFY( pars_io%vegin_extkn )
NULLIFY( pars_io%vegin_tminvj )
NULLIFY( pars_io%vegin_tmaxvj )
NULLIFY( pars_io%vegin_vbeta )
NULLIFY( pars_io%vegin_a1gs )
NULLIFY( pars_io%vegin_d0gs )
NULLIFY( pars_io%vegin_alpha )
NULLIFY( pars_io%vegin_convex )
NULLIFY( pars_io%vegin_cfrd )
NULLIFY( pars_io%vegin_gswmin )
NULLIFY( pars_io%vegin_conkc0 )
NULLIFY( pars_io%vegin_conko0 )
NULLIFY( pars_io%vegin_ekc )
NULLIFY( pars_io%vegin_eko )
NULLIFY( pars_io%vegin_g0 )
NULLIFY( pars_io%vegin_g1 )
NULLIFY( pars_io%vegin_zr )
NULLIFY( pars_io%vegin_clitt )
NULLIFY( pars_io%vegin_froot )
NULLIFY( pars_io%vegin_csoil )
NULLIFY( pars_io%vegin_ratecs )
NULLIFY( pars_io%vegin_cplant )
NULLIFY( pars_io%vegin_ratecp )
NULLIFY( pars_io%vegin_refl )
NULLIFY( pars_io%vegin_taul )

! Soil parameters I/O:
NULLIFY( pars_io%soilin_silt )
NULLIFY( pars_io%soilin_clay )
NULLIFY( pars_io%soilin_sand )
NULLIFY( pars_io%soilin_swilt )
NULLIFY( pars_io%soilin_sfc )
NULLIFY( pars_io%soilin_ssat )
NULLIFY( pars_io%soilin_bch )
NULLIFY( pars_io%soilin_hyds )
NULLIFY( pars_io%soilin_sucs )
NULLIFY( pars_io%soilin_rhosoil )
NULLIFY( pars_io%soilin_css )

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN
END SUBROUTINE params_io_nullify_cbl

END MODULE params_io_mod_cbl


