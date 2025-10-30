#if !defined(UM_JULES)
!-----------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

SUBROUTINE init_vegin_cbl(nml_dir,progs)

!-----------------------------------------------------------------------------
! Description:
!  Reads the JULES_PFT_PARAMS_CABLE namelist for standalone runs
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE missing_data_mod, ONLY:                                                    &
!  imported scalar parameters
     rmdi
USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE errormessagelength_mod, ONLY: errormessagelength

USE logging_mod, ONLY: log_info, log_fatal

USE jules_surface_types_mod, ONLY: ntype, npft

USE ancil_info, ONLY: land_pts

USE max_dimensions, ONLY: ntype_max

USE prognostics,      ONLY: progs_type
USE cable_fields_mod, ONLY: pars_io_data_cbl, pars_io_cbl

USE params_io_mod_cbl, ONLY: params_io_assoc_cbl

IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists
!TYPES containing the data
TYPE(progs_type), INTENT(IN OUT) :: progs

! CABLE constant, to be replaced once compilation with CABLE
! library works. ccc25
INTEGER, PARAMETER :: nsoil_max = 9 ! # of soil types


! Work variables
INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

CHARACTER(LEN=*), PARAMETER :: routinename='INIT_VEGIN_CABLE'

! With some compilers, namelists cannot contain multidimensional arrays.
! Therefore, an input type without multidimensional arrays is used to read
! in the the values from the namelist, and these values will then be
! transferred to the desired data type which does contain multidimensional
! arrays

! Whereas in JULES PFT parameters are only used for veg types, in CABLE they
! are used for both. Therefore CABLE PFT arrays need to be allocated ntype_max,
! which equals (npft_max + nvg_max).

REAL ::                                                                        &
      canst1_io(ntype_max),                                                    &
      length_io(ntype_max),                                                    &
      width_io(ntype_max),                                                     &
      vcmax_io(ntype_max),                                                     &
      ejmax_io(ntype_max),                                                     &
      hc_io(ntype_max),                                                        &
      xfang_io(ntype_max),                                                     &
      rp20_io(ntype_max),                                                      &
      rpcoef_io(ntype_max),                                                    &
      rs20_io(ntype_max),                                                      &
      wai_io(ntype_max),                                                       &
      rootbeta_io(ntype_max),                                                  &
      shelrb_io(ntype_max),                                                    &
      vegcf_io(ntype_max),                                                     &
      frac4_io(ntype_max),                                                     &
      xalbnir_io(ntype_max),                                                   &
      extkn_io(ntype_max),                                                     &
      tminvj_io(ntype_max),                                                    &
      tmaxvj_io(ntype_max),                                                    &
      vbeta_io(ntype_max),                                                     &
      a1gs_io(ntype_max),                                                      &
      d0gs_io(ntype_max),                                                      &
      alpha_io(ntype_max),                                                     &
      convex_io(ntype_max),                                                    &
      cfrd_io(ntype_max),                                                      &
      gswmin_io(ntype_max),                                                    &
      conkc0_io(ntype_max),                                                    &
      conko0_io(ntype_max),                                                    &
      ekc_io(ntype_max),                                                       &
      eko_io(ntype_max),                                                       &
      g0_io(ntype_max),                                                        &
      g1_io(ntype_max),                                                        &
      zr_io(ntype_max),                                                        &
      clitt_io(ntype_max),                                                     &
      froot1_io(ntype_max),                                                    &
      froot2_io(ntype_max),                                                    &
      froot3_io(ntype_max),                                                    &
      froot4_io(ntype_max),                                                    &
      froot5_io(ntype_max),                                                    &
      froot6_io(ntype_max),                                                    &
      csoil1_io(ntype_max),                                                    &
      csoil2_io(ntype_max),                                                    &
      ratecs1_io(ntype_max),                                                   &
      ratecs2_io(ntype_max),                                                   &
      cplant1_io(ntype_max),                                                   &
      cplant2_io(ntype_max),                                                   &
      cplant3_io(ntype_max),                                                   &
      ratecp1_io(ntype_max),                                                   &
      ratecp2_io(ntype_max),                                                   &
      ratecp3_io(ntype_max),                                                   &
      refl1_io(ntype_max),                                                     &
      refl2_io(ntype_max),                                                     &
      refl3_io(ntype_max),                                                     &
      taul1_io(ntype_max),                                                     &
      taul2_io(ntype_max),                                                     &
      taul3_io(ntype_max),                                                     &
      lai_io(ntype_max)

!-----------------------------------------------------------------------------
! Namelist definition
!-----------------------------------------------------------------------------
NAMELIST / cable_pftparm/ canst1_io, length_io, width_io, vcmax_io,            &
          ejmax_io, hc_io, xfang_io, rp20_io, rpcoef_io, rs20_io, wai_io,      &
          rootbeta_io, shelrb_io, vegcf_io, frac4_io, xalbnir_io, extkn_io,    &
          tminvj_io, tmaxvj_io, vbeta_io, a1gs_io, d0gs_io, alpha_io,          &
          convex_io, cfrd_io, gswmin_io, conkc0_io, conko0_io, ekc_io,         &
          eko_io, g0_io, g1_io, zr_io, clitt_io, froot1_io, froot2_io,         &
          froot3_io, froot4_io, froot5_io, froot6_io, cplant1_io,              &
          cplant2_io, cplant3_io, csoil1_io, csoil2_io, ratecp1_io,            &
          ratecp2_io, ratecp3_io, ratecs1_io, ratecs2_io, refl1_io,            &
          refl2_io, refl3_io, taul1_io, taul2_io, taul3_io, lai_io

!-----------------------------------------------------------------------------
! Initialise namelist values before reading them
!-----------------------------------------------------------------------------
canst1_io(:ntype)     = rmdi
length_io(:ntype)     = rmdi
width_io(:ntype)      = rmdi
vcmax_io(:ntype)      = rmdi
ejmax_io(:ntype)      = rmdi
hc_io(:ntype)         = rmdi
xfang_io(:ntype)      = rmdi
rp20_io(:ntype)       = rmdi
rpcoef_io(:ntype)     = rmdi
rs20_io(:ntype)       = rmdi
wai_io(:ntype)        = rmdi
rootbeta_io(:ntype)   = rmdi
shelrb_io(:ntype)     = rmdi
vegcf_io(:ntype)      = rmdi
frac4_io(:ntype)      = rmdi
xalbnir_io(:ntype)    = rmdi
extkn_io(:ntype)      = rmdi
tminvj_io(:ntype)     = rmdi
tmaxvj_io(:ntype)     = rmdi
vbeta_io(:ntype)      = rmdi
a1gs_io(:ntype)       = rmdi
d0gs_io(:ntype)       = rmdi
alpha_io(:ntype)      = rmdi
convex_io(:ntype)     = rmdi
cfrd_io(:ntype)       = rmdi
gswmin_io(:ntype)     = rmdi
conkc0_io(:ntype)     = rmdi
conko0_io(:ntype)     = rmdi
ekc_io(:ntype)        = rmdi
eko_io(:ntype)        = rmdi
g0_io(:ntype)         = rmdi
g1_io(:ntype)         = rmdi
zr_io(:ntype)         = rmdi
clitt_io(:ntype)      = rmdi
froot1_io(:ntype)     = rmdi
froot2_io(:ntype)     = rmdi
froot3_io(:ntype)     = rmdi
froot4_io(:ntype)     = rmdi
froot5_io(:ntype)     = rmdi
froot6_io(:ntype)     = rmdi
cplant1_io(:ntype)    = rmdi
cplant2_io(:ntype)    = rmdi
cplant3_io(:ntype)    = rmdi
csoil1_io(:ntype)     = rmdi
csoil2_io(:ntype)     = rmdi
ratecp1_io(:ntype)    = rmdi
ratecp2_io(:ntype)    = rmdi
ratecp3_io(:ntype)    = rmdi
ratecs1_io(:ntype)    = rmdi
ratecs2_io(:ntype)    = rmdi
refl1_io(:ntype)      = rmdi
refl2_io(:ntype)      = rmdi
refl3_io(:ntype)      = rmdi
taul1_io(:ntype)      = rmdi
taul2_io(:ntype)      = rmdi
taul3_io(:ntype)      = rmdi
lai_io(:ntype)        = rmdi

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
CALL log_info(routinename, "Reading CABLE_PFTPARM namelist...")

OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'pft_params.nml'),           &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT  = ERROR,&
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error opening namelist file pft_params.nml " //              &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

READ(namelist_unit, NML = cable_pftparm, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error reading namelist CABLE_PFTPARM " //                    &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error closing namelist file pft_params.nml " //              &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

!-----------------------------------------------------------------------------
! Transfer values from io values to vegin
!-----------------------------------------------------------------------------

pars_io_data_cbl%vegin_canst1(:ntype)     = canst1_io(:ntype)
pars_io_data_cbl%vegin_length(:ntype)     = length_io(:ntype)
pars_io_data_cbl%vegin_width(:ntype)      = width_io(:ntype)
pars_io_data_cbl%vegin_vcmax(:ntype)      = vcmax_io(:ntype)
pars_io_data_cbl%vegin_ejmax(:ntype)      = ejmax_io(:ntype)
pars_io_data_cbl%vegin_hc(:ntype)         = hc_io(:ntype)
pars_io_data_cbl%vegin_xfang(:ntype)      = xfang_io(:ntype)
pars_io_data_cbl%vegin_rp20(:ntype)       = rp20_io(:ntype)
pars_io_data_cbl%vegin_rpcoef(:ntype)     = rpcoef_io(:ntype)
pars_io_data_cbl%vegin_rs20(:ntype)       = rs20_io(:ntype)
pars_io_data_cbl%vegin_wai(:ntype)        = wai_io(:ntype)
pars_io_data_cbl%vegin_rootbeta(:ntype)   = rootbeta_io(:ntype)
pars_io_data_cbl%vegin_shelrb(:ntype)     = shelrb_io(:ntype)
pars_io_data_cbl%vegin_vegcf(:ntype)      = vegcf_io(:ntype)
pars_io_data_cbl%vegin_frac4(:ntype)      = frac4_io(:ntype)
pars_io_data_cbl%vegin_xalbnir(:ntype)    = xalbnir_io(:ntype)
pars_io_data_cbl%vegin_extkn(:ntype)      = extkn_io(:ntype)
pars_io_data_cbl%vegin_tminvj(:ntype)     = tminvj_io(:ntype)
pars_io_data_cbl%vegin_tmaxvj(:ntype)     = tmaxvj_io(:ntype)
pars_io_data_cbl%vegin_vbeta(:ntype)      = vbeta_io(:ntype)
pars_io_data_cbl%vegin_a1gs(:ntype)       = a1gs_io(:ntype)
pars_io_data_cbl%vegin_d0gs(:ntype)       = d0gs_io(:ntype)
pars_io_data_cbl%vegin_alpha(:ntype)      = alpha_io(:ntype)
pars_io_data_cbl%vegin_convex(:ntype)     = convex_io(:ntype)
pars_io_data_cbl%vegin_cfrd(:ntype)       = cfrd_io(:ntype)
pars_io_data_cbl%vegin_gswmin(:ntype)     = gswmin_io(:ntype)
pars_io_data_cbl%vegin_conkc0(:ntype)     = conkc0_io(:ntype)
pars_io_data_cbl%vegin_conko0(:ntype)     = conko0_io(:ntype)
pars_io_data_cbl%vegin_ekc(:ntype)        = ekc_io(:ntype)
pars_io_data_cbl%vegin_eko(:ntype)        = eko_io(:ntype)
pars_io_data_cbl%vegin_g0(:ntype)         = g0_io(:ntype)
pars_io_data_cbl%vegin_g1(:ntype)         = g1_io(:ntype)
pars_io_data_cbl%vegin_zr(:ntype)         = zr_io(:ntype)
pars_io_data_cbl%vegin_clitt(:ntype)      = clitt_io(:ntype)
pars_io_data_cbl%vegin_froot(1,:ntype)    = froot1_io(:ntype)
pars_io_data_cbl%vegin_froot(2,:ntype)    = froot2_io(:ntype)
pars_io_data_cbl%vegin_froot(3,:ntype)    = froot3_io(:ntype)
pars_io_data_cbl%vegin_froot(4,:ntype)    = froot4_io(:ntype)
pars_io_data_cbl%vegin_froot(5,:ntype)    = froot5_io(:ntype)
pars_io_data_cbl%vegin_froot(6,:ntype)    = froot6_io(:ntype)
pars_io_data_cbl%vegin_cplant(1,:ntype)   = cplant1_io(:ntype)
pars_io_data_cbl%vegin_cplant(2,:ntype)   = cplant2_io(:ntype)
pars_io_data_cbl%vegin_cplant(3,:ntype)   = cplant3_io(:ntype)
pars_io_data_cbl%vegin_csoil(1,:ntype)    = csoil1_io(:ntype)
pars_io_data_cbl%vegin_csoil(2,:ntype)    = csoil2_io(:ntype)
pars_io_data_cbl%vegin_ratecp(1,:ntype)   = ratecp1_io(:ntype)
pars_io_data_cbl%vegin_ratecp(2,:ntype)   = ratecp2_io(:ntype)
pars_io_data_cbl%vegin_ratecp(3,:ntype)   = ratecp3_io(:ntype)
pars_io_data_cbl%vegin_ratecs(1,:ntype)   = ratecs1_io(:ntype)
pars_io_data_cbl%vegin_ratecs(2,:ntype)   = ratecs2_io(:ntype)
pars_io_data_cbl%vegin_refl(1,:ntype)     = refl1_io(:ntype)
pars_io_data_cbl%vegin_refl(2,:ntype)     = refl2_io(:ntype)
pars_io_data_cbl%vegin_refl(3,:ntype)     = refl3_io(:ntype)
pars_io_data_cbl%vegin_taul(1,:ntype)     = taul1_io(:ntype)
pars_io_data_cbl%vegin_taul(2,:ntype)     = taul2_io(:ntype)
pars_io_data_cbl%vegin_taul(3,:ntype)     = taul3_io(:ntype)
progs%canht_pft(:,:npft)       = SPREAD(hc_io(1:npft), 1, land_pts)
progs%lai_pft(:,:npft)         = SPREAD(lai_io(1:npft), 1, land_pts)

!-----------------------------------------------------------------------------
! Check that all required variables were present in the namelist.
! The namelist variables were initialised to rmdi.
! Some configurations don't need all parameters but for now we insist on
! getting all parameters (and that there are not rmdi!).
!-----------------------------------------------------------------------------
ERROR = 0
IF ( ANY( ABS( pars_io_data_cbl%vegin_canst1(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for canst1")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_length(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for length")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_width(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for width")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_vcmax(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for vcmax")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_ejmax(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for ejmax")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_hc(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for hc")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_xfang(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for xfang")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_rp20(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for rp20")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_rpcoef(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for rpcoef")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_rs20(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for rs20")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_wai(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for wai")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_rootbeta(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for rootbeta")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_shelrb(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for shelrb")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_vegcf(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for vegcf")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_frac4(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for frac4")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_xalbnir(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for xalbnir")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_extkn(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for extkni")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_tminvj(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for tminvj")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_tmaxvj(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for tmaxvj")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_vbeta(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for vbeta")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_a1gs(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for a1hs")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_d0gs(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for d0gs")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_alpha(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for alpha")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_convex(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for convex")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_cfrd(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for cfrd")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_gswmin(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for gswmin")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_conkc0(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for conkc0")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_conko0(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for conko0")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_ekc(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for ekc")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_eko(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for eko")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_g0(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for g0")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_g1(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for g1")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_zr(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for zr")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_clitt(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for clitt")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_froot(:,:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for froot")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_cplant(:,:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for cplant")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_csoil(:,:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for csoil")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_ratecp(:,:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for ratecp")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_ratecs(:,:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for ratecs")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_refl(:,:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for refl")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_taul(:,:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for taul")
END IF
IF ( ANY( ABS( progs%canht_pft(:,:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for hc")
END IF
IF ( ANY( ABS( progs%lai_pft(:,:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for lai")
END IF

IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Variable(s) missing from namelist - see earlier " //         &
                 "error message(s)")

pars_io_data_cbl%vegin_dleaf(:) = SQRT(pars_io_data_cbl%vegin_width(:) * pars_io_data_cbl%vegin_length(:))

CALL params_io_assoc_cbl(pars_io_cbl,pars_io_data_cbl)

END SUBROUTINE init_vegin_cbl

#endif
