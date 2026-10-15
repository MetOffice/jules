#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE model_interface_mod

USE io_constants, ONLY: max_sdf_name_len, max_attr_val_len

USE logging_mod, ONLY: log_warn, log_fatal

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This module provides the interface between the IO routines and the
!   model variables
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Module constants
!-----------------------------------------------------------------------------
! Length for identifiers of model variables
INTEGER, PARAMETER :: identifier_len = 29

! The name of each levels dimension in output/dump files
CHARACTER(LEN=max_sdf_name_len), PARAMETER ::                                  &
  bl_level_dim_name_out     = 'bllevel',                                       &
  pft_dim_name_out          = 'pft',                                           &
  quantile_dim_name_out     = 'quantile',                                      &
  cpft_dim_name_out         = 'cpft',                                          &
  nvg_dim_name_out          = 'nvg',                                           &
  type_dim_name_out         = 'type',                                          &
  tile_dim_name_out         = 'tile',                                          &
  soilt_dim_name_out        = 'soilt',                                         &
  snow_dim_name_out         = 'snow',                                          &
  soil_dim_name_out         = 'soil',                                          &
  scpool_dim_name_out       = 'scpool',                                        &
  soil_n_pool_dim_name_out  = 'snpool',                                        &
  imogen_drive_dim_name_out = 'imogen_drive',                                  &
  imogen_clim_dim_name_out  = 'imogen_clim',                                   &
  sclayer_dim_name_out      = 'sclayer',                                       &
  dep_species_dim_name_out  = 'dep_species',                                   &
  ch4layer_dim_name_out     = 'ch4layer',                                      &
  bedrock_dim_name_out      = 'bedrock',                                       &
  nmasst_dim_name_out       = 'nmasst',                                        &
  land_dim_name_out         = 'land',                                          &
  nfarray_dim_name_out      = 'nfarray',                                       &
  nolevs_dim_name_out       = 'olevs',                                         &
  p_rivers_dim_name_out     = 'p_rivers',                                      &
  seed_dim_name_out         = 'seed',                                          &
  scalar_dim_name_out       = 'scalar'

!-----------------------------------------------------------------------------
! Module variables
!-----------------------------------------------------------------------------
! The name of each levels dimension in input files
CHARACTER(LEN=max_sdf_name_len) ::                                             &
  bl_level_dim_name = 'bllevel',                                               &
  pft_dim_name      = 'pft',                                                   &
  quantile_dim_name = 'quantile',                                              &
  cpft_dim_name     = 'cpft',                                                  &
  nvg_dim_name      = 'nvg',                                                   &
  type_dim_name     = 'type',                                                  &
  tile_dim_name     = 'tile',                                                  &
  soilt_dim_name    = 'soilt',                                                 &
  snow_dim_name     = 'snow',                                                  &
  soil_dim_name     = 'soil',                                                  &
  scpool_dim_name   = 'scpool',                                                &
  soil_n_pool_dim_name  = 'snpool',                                            &
  imogen_drive_dim_name = 'imogen_drive',                                      &
  imogen_clim_dim_name  = 'imogen_clim',                                       &
  clim_time_dim_name    = 'time',                                              &
  sclayer_dim_name      = 'sclayer',                                           &
  dep_species_dim_name  = 'dep_species',                                       &
  ch4layer_dim_name = 'ch4layer',                                              &
  bedrock_dim_name  = 'bedrock',                                               &
  nmasst_dim_name   = 'nmasst'

! The size of each possible levels dimension.
! Initialising these to something "unphysical" means that in future we can
! recognise if any dimension that is added to the code has not been set
! correctly (i.e. it will retain this obviously-wrong initial value).
INTEGER ::                                                                     &
  bl_level_dim_size    = -1,                                                   &
  pft_dim_size         = -1,                                                   &
  quantile_dim_size    = -1,                                                   &
  cpft_dim_size        = -1,                                                   &
  nvg_dim_size         = -1,                                                   &
  type_dim_size        = -1,                                                   &
  tile_dim_size        = -1,                                                   &
  soilt_dim_size       = -1,                                                   &
  snow_dim_size        = -1,                                                   &
  soil_dim_size        = -1,                                                   &
  scpool_dim_size      = -1,                                                   &
  soil_n_pool_dim_size = -1,                                                   &
  imogen_drive_dim_size= -1,                                                   &
  imogen_clim_dim_size = -1,                                                   &
  sclayer_dim_size     = -1,                                                   &
  dep_species_dim_size = -1,                                                   &
  ch4layer_dim_size    = -1,                                                   &
  bedrock_dim_size     = -1,                                                   &
  nmasst_dim_size      = -1

! Due to how CABLE sets variables differently to JULES, these numbers
! are currently necessary.
INTEGER ::                                                                     &
  cable_tile_dim_size  = 17,                                                   &
  cable_soil_dim_size  = 6,                                                    &
  cable_snow_dim_size  = 3

! Size for 12 monthly values for use as a climatology. This is not set through
! the namelists.
INTEGER, PARAMETER :: monthly_climatology_dim_size = 12

!-----------------------------------------------------------------------------
! Information about the actual variables available for output
!-----------------------------------------------------------------------------
! Constants for the different 'types' of variable
INTEGER, PARAMETER ::                                                          &
  var_type_bl_level       = 0,                                                 &
  var_type_surface        = var_type_bl_level+1,                               &
  var_type_pft            = var_type_surface+1,                                &
  var_type_quantile       = var_type_pft+1,                                    &
  var_type_cpft           = var_type_quantile+1,                               &
  var_type_nvg            = var_type_cpft+1,                                   &
  var_type_type           = var_type_nvg+1,                                    &
  var_type_surft          = var_type_type+1,                                   &
  var_type_soilt          = var_type_surft+1,                                  &
  var_type_soilt_soil     = var_type_soilt+1,                                  &
  var_type_snow           = var_type_soilt_soil+1,                             &
  var_type_soil           = var_type_snow+1,                                   &
  var_type_scpool         = var_type_soil+1,                                   &
  var_type_soil_n_pool    = var_type_scpool+1,                                 &
  var_type_bedrock        = var_type_soil_n_pool+1,                            &
  var_type_rp             = var_type_bedrock+1,                                &
  var_type_sclayer        = var_type_rp+1,                                     &
  var_type_soilt_sclayer  = var_type_sclayer+1,                                &
  var_type_soilt_sclayer_scpool = var_type_soilt_sclayer+1,                    &
  var_type_dep_species    = var_type_soilt_sclayer_scpool+1,                   &
  var_type_surft_dep_species = var_type_dep_species+1,                         &
  var_type_ch4layer       = var_type_surft_dep_species+1,                      &
  var_type_pft_sclayer    = var_type_ch4layer+1,                               &
  var_type_cable_soil     = var_type_pft_sclayer+1,                            &
  var_type_cable_snow     = var_type_cable_soil+1,                             &
  var_type_cable1L_snow   = var_type_cable_snow + 1,                           &
  var_type_imogen_drive   = var_type_cable1L_snow + 1,                         &
  var_type_nmasst         = var_type_imogen_drive + 1,                         &
  var_type_imogen_clim    = var_type_nmasst + 1

! Derived type to contain metadata about model variables
TYPE :: var_metadata

  CHARACTER(LEN=identifier_len) :: identifier
                            ! The string identifier of the variable

  INTEGER :: var_type  ! The type of the variable - must be one of the above

  CHARACTER(LEN=max_attr_val_len) :: long_name
                            ! The value of the long_name attribute
  CHARACTER(LEN=max_attr_val_len) :: units
                            ! The value of the units attribute

END TYPE var_metadata

! Array holding the metadata for all model variables that we can use for input
! or output. The CABLE land surface model adds 10 prognostics for tiled
! soil/snow prognostics.

INTEGER, PARAMETER :: n_vars = 743

TYPE(var_metadata) :: metadata(n_vars)

! Include the metadata DATA statement
#include "variable_metadata.inc"


!-----------------------------------------------------------------------------
! Visibility declarations
!-----------------------------------------------------------------------------
PRIVATE
PUBLIC                                                                         &
! Parameters
    identifier_len, bl_level_dim_name_out,                                     &
    pft_dim_name_out, quantile_dim_name_out, cpft_dim_name_out,                &
    nvg_dim_name_out, type_dim_name_out,                                       &
    tile_dim_name_out, soilt_dim_name_out, snow_dim_name_out,                  &
    soil_dim_name_out, scpool_dim_name_out, soil_n_pool_dim_name_out,          &
    bedrock_dim_name_out, nmasst_dim_name_out, sclayer_dim_name_out,           &
    dep_species_dim_name_out, ch4layer_dim_name_out,                           &
    imogen_drive_dim_name_out, imogen_clim_dim_name_out,                       &
    land_dim_name_out, nfarray_dim_name_out, nolevs_dim_name_out,              &
    p_rivers_dim_name_out, seed_dim_name_out, scalar_dim_name_out,             &
! Variables
    bl_level_dim_name, pft_dim_name, quantile_dim_name,                        &
    cpft_dim_name, nvg_dim_name, type_dim_name, tile_dim_name,                 &
    soilt_dim_name, snow_dim_name, soil_dim_name, scpool_dim_name,             &
    soil_n_pool_dim_name, bedrock_dim_name, nmasst_dim_name,                   &
    sclayer_dim_name, imogen_drive_dim_name, dep_species_dim_name,             &
    imogen_clim_dim_name, clim_time_dim_name,                                  &
    bl_level_dim_size, pft_dim_size, quantile_dim_size, cpft_dim_size,         &
    nvg_dim_size, imogen_drive_dim_size, nmasst_dim_size,                      &
    imogen_clim_dim_size, monthly_climatology_dim_size,                        &
    type_dim_size, tile_dim_size, soilt_dim_size, snow_dim_size,               &
    soil_dim_size, scpool_dim_size, soil_n_pool_dim_size, bedrock_dim_size,    &
    sclayer_dim_size, dep_species_dim_size, ch4layer_dim_size,                 &
    cable_tile_dim_size, cable_snow_dim_size, cable_soil_dim_size,             &
! Routines for changing between string and integer identifiers
    get_var_id, get_string_identifier,                                         &
! Variable inquiry routines
    get_var_levs_dims, get_var_attrs,                                          &
! Routines for getting and setting values
    extract_var, populate_var,                                                 &
! Routine to check metadata
    check_variable_metadata,                                                   &
! Routine to check whether a variable is on the river grid
    is_river_var


CONTAINS


! Fortran INCLUDE statements would be preferred, but (at least) the pgf90
! compiler objects to their use when the included file contains pre-processor
! directives. At present, such directives are used to exclude files from
! the UM build, so are required. This may change in the future.
#include "check_variable_metadata.inc"
#include "get_var_id.inc"
#include "get_string_identifier.inc"
#include "get_var_levs_dims.inc"
#include "get_var_attrs.inc"
#include "extract_var.inc"
#include "populate_var.inc"
#include "is_river_var.inc"

#include "map_to_land.inc"
#include "map_from_land.inc"

END MODULE model_interface_mod
#endif
