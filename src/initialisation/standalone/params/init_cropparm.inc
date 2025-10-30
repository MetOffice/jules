#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

SUBROUTINE init_cropparm(nml_dir)

USE missing_data_mod, ONLY: rmdi

USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE jules_surface_types_mod, ONLY: ncpft

USE cropparm, ONLY: t_bse, t_opt, t_max, tt_emr, crit_pp, pp_sens, rt_dir,     &
  alpha1, alpha2, alpha3, beta1, beta2, beta3, r_gamma, delta, remob,          &
  cfrac_s, cfrac_r, cfrac_l, allo1, allo2, mu, nu, yield_frac, initial_carbon, &
  initial_c_dvi, sen_dvi, t_mort

USE cropparm_io, ONLY: jules_cropparm,                                         &
   t_bse_io, t_opt_io, t_max_io, tt_emr_io, crit_pp_io, pp_sens_io,            &
   rt_dir_io, alpha1_io, alpha2_io, alpha3_io, beta1_io, beta2_io, beta3_io,   &
   gamma_io, delta_io, remob_io, cfrac_s_io, cfrac_r_io, cfrac_l_io,           &
   allo1_io, allo2_io, mu_io, nu_io, yield_frac_io, initial_carbon_io,         &
   initial_c_dvi_io, sen_dvi_io, t_mort_io


USE jules_vegetation_mod, ONLY: l_crop

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises the crop PFT parameters
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!
!-----------------------------------------------------------------------------
! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

! Work variables
INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

!----------------------------------------------------------------------------

! Nothing to do if crop model is not selected
IF ( .NOT. l_crop ) RETURN

!-----------------------------------------------------------------------------
! Initialise namelist values before reading them
!-----------------------------------------------------------------------------

T_BSE_io(:)     = rmdi
T_OPT_io(:)     = rmdi
T_MAX_io(:)     = rmdi
TT_EMR_io(:)    = rmdi

CRIT_PP_io(:)   = rmdi
PP_SENS_io(:)   = rmdi
RT_DIR_io(:)    = rmdi
ALPHA1_io(:)    = rmdi

ALPHA2_io(:)    = rmdi
ALPHA3_io(:)    = rmdi
BETA1_io(:)     = rmdi
BETA2_io(:)     = rmdi
BETA3_io(:)     = rmdi

gamma_io(:)     = rmdi
DELTA_io(:)     = rmdi
REMOB_io(:)     = rmdi
CFRAC_S_io(:)   = rmdi
CFRAC_R_io(:)   = rmdi

CFRAC_L_io(:)   = rmdi
ALLO1_io(:)     = rmdi
ALLO2_io(:)     = rmdi

mu_io(:)             = rmdi
nu_io(:)             = rmdi
yield_frac_io(:)     = rmdi
initial_carbon_io(:) = rmdi
initial_c_dvi_io(:)  = rmdi
sen_dvi_io(:)        = rmdi
t_mort_io(:)         = rmdi

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
CALL log_info("init_cropparm", "Reading JULES_CROPPARM namelist...")

! Open the crop pft parameters namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'crop_params.nml'),          &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_cropparm",                                              &
                 "Error opening namelist file crop_params.nml " //             &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

READ(namelist_unit, NML = jules_cropparm, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_cropparm",                                              &
                 "Error reading namelist JULES_CROPPARM " //                   &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

! Close the namelist file
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_cropparm",                                              &
                 "Error closing namelist file crop_params.nml " //             &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")


!-----------------------------------------------------------------------------
! Process the namelist values
!-----------------------------------------------------------------------------
! Copy values from fixed length arrays used in namelist into allocated arrays

t_bse(:)   = T_BSE_io(1:ncpft)
t_opt(:)   = T_OPT_io(1:ncpft)
t_max(:)   = T_MAX_io(1:ncpft)
tt_emr(:)  = TT_EMR_io(1:ncpft)
crit_pp(:) = CRIT_PP_io(1:ncpft)
pp_sens(:) = PP_SENS_io(1:ncpft)
rt_dir(:)  = RT_DIR_io(1:ncpft)
alpha1(:)  = ALPHA1_io(1:ncpft)
alpha2(:)  = ALPHA2_io(1:ncpft)
alpha3(:)  = ALPHA3_io(1:ncpft)
beta1(:)   = BETA1_io(1:ncpft)
beta2(:)   = BETA2_io(1:ncpft)
beta3(:)   = BETA3_io(1:ncpft)
r_gamma(:) = gamma_io(1:ncpft)
delta(:)   = DELTA_io(1:ncpft)
remob(:)   = REMOB_io(1:ncpft)
cfrac_s(:) = CFRAC_S_io(1:ncpft)
cfrac_r(:) = CFRAC_R_io(1:ncpft)
cfrac_l(:) = CFRAC_L_io(1:ncpft)
allo1(:)   = ALLO1_io(1:ncpft)
allo2(:)   = ALLO2_io(1:ncpft)
mu(:)             = mu_io(1:ncpft)
nu(:)             = nu_io(1:ncpft)
yield_frac(:)     = yield_frac_io(1:ncpft)
initial_carbon(:) = initial_carbon_io(1:ncpft)
initial_c_dvi(:)  = initial_c_dvi_io(1:ncpft)
sen_dvi(:)        = sen_dvi_io(1:ncpft)
t_mort(:)         = t_mort_io(1:ncpft)

!-----------------------------------------------------------------------------
! Check that all required variables were present in the namelist.
! The namelist variables were initialised to rmdi.
!-----------------------------------------------------------------------------
ERROR = 0

IF ( ANY( ABS( t_bse(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for T_BSE")
END IF
IF ( ANY( ABS( t_opt(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for T_OPT")
END IF
IF ( ANY( ABS( t_max(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for T_MAX")
END IF
IF ( ANY( ABS( tt_emr(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for TT_EMR")
END IF


IF ( ANY( ABS( crit_pp(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for CRIT_PP")
END IF
IF ( ANY( ABS( pp_sens(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for PP_SENS")
END IF
IF ( ANY( ABS( rt_dir(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for RT_DIR")
END IF
IF ( ANY( ABS( alpha1(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for ALPHA1")
END IF


IF ( ANY( ABS( alpha2(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for ALPHA2")
END IF
IF ( ANY( ABS( alpha3(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for ALPHA3")
END IF
IF ( ANY( ABS( beta1(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for BETA1")
END IF
IF ( ANY( ABS( beta2(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for BETA2")
END IF
IF ( ANY( ABS( beta3(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for BETA3")
END IF


IF ( ANY( ABS( r_gamma(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for gamma")
END IF
IF ( ANY( ABS( delta(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for DELTA")
END IF
IF ( ANY( ABS( remob(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for REMOB")
END IF
IF ( ANY( ABS( cfrac_s(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for CFRAC_S")
END IF
IF ( ANY( ABS( cfrac_r(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for CFRAC_R")
END IF


IF ( ANY( ABS( cfrac_l(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for CFRAC_L")
END IF
IF ( ANY( ABS( allo1(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for ALLO1")
END IF
IF ( ANY( ABS( allo2(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for ALLO2")
END IF


IF ( ANY( ABS( mu(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for mu")
END IF
IF ( ANY( ABS( nu(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for nu")
END IF

IF ( ANY( ABS( yield_frac(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for yield_frac")
END IF
IF ( ANY( ABS( initial_carbon(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for initial_carbon")
END IF
IF ( ANY( ABS( initial_c_dvi(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for initial_c_dvi")
END IF
IF ( ANY( ABS( sen_dvi(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for sen_dvi")
END IF
IF ( ANY( ABS( t_mort(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for t_mort")
END IF


IF ( ERROR /= 0 ) THEN
  CALL log_fatal("init_cropparm",                                              &
                 "Variable(s) missing from namelist - see earlier " //         &
                 "error message(s)")
END IF

RETURN

END SUBROUTINE init_cropparm
#endif

