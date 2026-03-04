#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE init_params_mod

USE logging_mod, ONLY: log_info, log_warn, log_error, log_fatal

USE init_deposition_species_mod, ONLY: init_deposition_species

USE init_red_mod, ONLY: init_red

USE c_irrigation_mod, ONLY: check_irrigation

IMPLICIT NONE

PRIVATE
PUBLIC init_params

CONTAINS

! Fortran INCLUDE statements would be preferred, but (at least) the pgf90
! compiler objects to their use when the included file contains pre-processor
! directives. At present, such directives are used to exclude files from
! the UM build, so are required. This may change in the future.
#include "init_params.inc"
#include "init_pftparm.inc"
#include "init_vegin_cbl.inc"
#include "init_nvegparm.inc"
#include "init_cropparm.inc"
#include "init_triffid.inc"
#include "init_soilin_cbl.inc"

END MODULE init_params_mod

#endif
