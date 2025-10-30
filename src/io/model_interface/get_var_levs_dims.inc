#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

SUBROUTINE get_var_levs_dims(var_id, is_climatology, ndims, dim_names_in,      &
                             dim_names_out, dim_sizes)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Given an identifer for a model variable, returns the levels dimensions
!   that it uses
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
INTEGER, INTENT(IN) :: var_id  ! Identifies the model variable

LOGICAL, INTENT(IN) :: is_climatology
                               ! Indicates if the variable is to be initialised
                               ! from a 12-month climatology

INTEGER, INTENT(OUT), OPTIONAL :: ndims
                               ! The number of levels dimensions that the
                               ! variable has
CHARACTER(LEN=*), INTENT(OUT), OPTIONAL :: dim_names_in(:)
                               ! The dimension names to use for the variable
                               ! in input files
CHARACTER(LEN=*), INTENT(OUT), OPTIONAL :: dim_names_out(:)
                               ! The dimension names to use for the variable
                               ! in output files
INTEGER, INTENT(OUT), OPTIONAL :: dim_sizes(:)
                               ! The dimension sizes to for the variable

LOGICAL :: is_climatology_error ! Issue a fatal error if a climatology is not
                                ! yet allowed for the variable type

!-----------------------------------------------------------------------------


! var_id is an index into the metadata array

! We decide what to return based on the 'variable type' of the requested
! variable

! Climatologies are only coded for var_type_surface so issue a fatal error if
! any other var_types try to use it as more coding will need to be done.
is_climatology_error = is_climatology

SELECT CASE ( metadata(var_id)%var_type )
CASE ( var_type_surface )
  ! No vertical dimensions although climatology is allowed
  IF ( is_climatology ) THEN
    IF ( PRESENT(ndims) ) ndims = 1
    IF ( PRESENT(dim_names_in) ) dim_names_in(1) = clim_time_dim_name
    IF ( PRESENT(dim_names_out) ) dim_names_out(1) = clim_time_dim_name
    IF ( PRESENT(dim_sizes) ) dim_sizes(1) = monthly_climatology_dim_size
    is_climatology_error = .FALSE.
  ELSE
    IF ( PRESENT(ndims) ) ndims = 0
  END IF

CASE ( var_type_nmasst )
  ! Mass variables have 2 levels dimensions
  IF ( PRESENT(ndims) ) ndims = 2
  IF ( PRESENT(dim_names_in) )                                                 &
    dim_names_in(1:2) = [ pft_dim_name, nmasst_dim_name ]
  IF ( PRESENT(dim_names_out) )                                                &
    dim_names_out(1:2) = [ pft_dim_name_out, nmasst_dim_name_out ]
  IF ( PRESENT(dim_sizes) )                                                    &
    dim_sizes(1:2) = [ pft_dim_size, nmasst_dim_size ]

CASE ( var_type_bl_level )
  IF ( PRESENT(ndims) ) ndims = 1
  IF ( PRESENT(dim_names_in) ) dim_names_in(1) = bl_level_dim_name
  IF ( PRESENT(dim_names_out) ) dim_names_out(1) = bl_level_dim_name_out
  IF ( PRESENT(dim_sizes) ) dim_sizes(1) = bl_level_dim_size

CASE ( var_type_pft )
  IF ( PRESENT(ndims) ) ndims = 1
  IF ( PRESENT(dim_names_in) ) dim_names_in(1) = pft_dim_name
  IF ( PRESENT(dim_names_out) ) dim_names_out(1) = pft_dim_name_out
  IF ( PRESENT(dim_sizes) ) dim_sizes(1) = pft_dim_size

CASE ( var_type_quantile )
  IF ( PRESENT(ndims) ) ndims = 1
  IF ( PRESENT(dim_names_in) ) dim_names_in(1) = quantile_dim_name
  IF ( PRESENT(dim_names_out) ) dim_names_out(1) = quantile_dim_name_out
  IF ( PRESENT(dim_sizes) ) dim_sizes(1) = quantile_dim_size

CASE ( var_type_cpft )
  IF ( PRESENT(ndims) ) ndims = 1
  IF ( PRESENT(dim_names_in) ) dim_names_in(1) = cpft_dim_name
  IF ( PRESENT(dim_names_out) ) dim_names_out(1) = cpft_dim_name_out
  IF ( PRESENT(dim_sizes) ) dim_sizes(1) = cpft_dim_size

CASE ( var_type_nvg )
  IF ( PRESENT(ndims) ) ndims = 1
  IF ( PRESENT(dim_names_in) ) dim_names_in(1) = nvg_dim_name
  IF ( PRESENT(dim_names_out) ) dim_names_out(1) = nvg_dim_name_out
  IF ( PRESENT(dim_sizes) ) dim_sizes(1) = nvg_dim_size

CASE ( var_type_type )
  IF ( PRESENT(ndims) ) ndims = 1
  IF ( PRESENT(dim_names_in) ) dim_names_in(1) = type_dim_name
  IF ( PRESENT(dim_names_out) ) dim_names_out(1) = type_dim_name_out
  IF ( PRESENT(dim_sizes) ) dim_sizes(1) = type_dim_size

CASE ( var_type_surft )
  IF ( PRESENT(ndims) ) ndims = 1
  IF ( PRESENT(dim_names_in) ) dim_names_in(1) = tile_dim_name
  IF ( PRESENT(dim_names_out) ) dim_names_out(1) = tile_dim_name_out
  IF ( PRESENT(dim_sizes) ) dim_sizes(1) = tile_dim_size

CASE ( var_type_soilt )
  IF ( PRESENT(ndims) ) ndims = 1
  IF ( PRESENT(dim_names_in) ) dim_names_in(1) = soilt_dim_name
  IF ( PRESENT(dim_names_out) ) dim_names_out(1) = soilt_dim_name_out
  IF ( PRESENT(dim_sizes) ) dim_sizes(1) = soilt_dim_size

CASE ( var_type_soil )
  IF ( PRESENT(ndims) ) ndims = 1
  IF ( PRESENT(dim_names_in) ) dim_names_in(1) = soil_dim_name
  IF ( PRESENT(dim_names_out) ) dim_names_out(1) = soil_dim_name_out
  IF ( PRESENT(dim_sizes) ) dim_sizes(1) = soil_dim_size

CASE ( var_type_scpool )
  IF ( PRESENT(ndims) ) ndims = 2
  IF ( PRESENT(dim_names_in) )                                                 &
    dim_names_in(1:2) = [ sclayer_dim_name, scpool_dim_name ]
  IF ( PRESENT(dim_names_out) )                                                &
    dim_names_out(1:2) = [ sclayer_dim_name_out, scpool_dim_name_out ]
  IF ( PRESENT(dim_sizes) )                                                    &
    dim_sizes(1:2) = [ sclayer_dim_size, scpool_dim_size ]

CASE ( var_type_soil_n_pool )
  ! Variables with soil (C) layers and soil N pools.
  IF ( PRESENT(ndims) ) ndims = 2
  IF ( PRESENT(dim_names_in) )                                                 &
    dim_names_in(1:2) = [ sclayer_dim_name, soil_n_pool_dim_name ]
  IF ( PRESENT(dim_names_out) )                                                &
    dim_names_out(1:2) = [ sclayer_dim_name_out, soil_n_pool_dim_name_out ]
  IF ( PRESENT(dim_sizes) )                                                    &
    dim_sizes(1:2) = [ sclayer_dim_size, soil_n_pool_dim_size ]

CASE ( var_type_imogen_drive )    ! imogen driving variables
  IF ( PRESENT(ndims) ) ndims = 1
  IF ( PRESENT(dim_names_in) ) dim_names_in(1) = imogen_drive_dim_name
  IF ( PRESENT(dim_names_out) ) dim_names_out(1) = imogen_drive_dim_name_out
  IF ( PRESENT(dim_sizes) ) dim_sizes(1) = imogen_drive_dim_size

CASE ( var_type_imogen_clim )    ! imogen climate input variables
  IF ( PRESENT(ndims) ) ndims = 1
  IF ( PRESENT(dim_names_in) ) dim_names_in(1) = imogen_clim_dim_name
  IF ( PRESENT(dim_names_out) ) dim_names_out(1) = imogen_clim_dim_name_out
  IF ( PRESENT(dim_sizes) ) dim_sizes(1) = imogen_clim_dim_size

CASE ( var_type_sclayer )
  IF ( PRESENT(ndims) ) ndims = 1
  IF ( PRESENT(dim_names_in) ) dim_names_in(1) = sclayer_dim_name
  IF ( PRESENT(dim_names_out) ) dim_names_out(1) = sclayer_dim_name_out
  IF ( PRESENT(dim_sizes) ) dim_sizes(1) = sclayer_dim_size

CASE ( var_type_ch4layer )
  IF ( PRESENT(ndims) ) ndims = 1
  IF ( PRESENT(dim_names_in) ) dim_names_in(1) = ch4layer_dim_name
  IF ( PRESENT(dim_names_out) ) dim_names_out(1) = ch4layer_dim_name_out
  IF ( PRESENT(dim_sizes) ) dim_sizes(1) = ch4layer_dim_size

CASE ( var_type_dep_species )
  IF ( PRESENT(ndims) )         ndims            = 1
  IF ( PRESENT(dim_names_in) )  dim_names_in(1)  = dep_species_dim_name
  IF ( PRESENT(dim_names_out) ) dim_names_out(1) = dep_species_dim_name_out
  IF ( PRESENT(dim_sizes) )     dim_sizes(1)     = dep_species_dim_size

CASE ( var_type_surft_dep_species )
  ! Variables with tile and chemical deposited species
  IF ( PRESENT(ndims) ) ndims = 2
  IF ( PRESENT(dim_names_in) )                                                 &
    dim_names_in(1:2) = [ tile_dim_name, dep_species_dim_name ]
  IF ( PRESENT(dim_names_out) )                                                &
    dim_names_out(1:2) = [ tile_dim_name_out, dep_species_dim_name_out ]
  IF ( PRESENT(dim_sizes) )                                                    &
    dim_sizes(1:2) = [ tile_dim_size, dep_species_dim_size ]

CASE ( var_type_bedrock )
  IF ( PRESENT(ndims) ) ndims = 1
  IF ( PRESENT(dim_names_in) ) dim_names_in(1) = bedrock_dim_name
  IF ( PRESENT(dim_names_out) ) dim_names_out(1) = bedrock_dim_name_out
  IF ( PRESENT(dim_sizes) ) dim_sizes(1) = bedrock_dim_size

CASE ( var_type_snow )
  ! Snow variables have 2 levels dimensions
  IF ( PRESENT(ndims) ) ndims = 2
  IF ( PRESENT(dim_names_in) )                                                 &
    dim_names_in(1:2) = [ tile_dim_name, snow_dim_name ]
  IF ( PRESENT(dim_names_out) )                                                &
    dim_names_out(1:2) = [ tile_dim_name_out, snow_dim_name_out ]
  IF ( PRESENT(dim_sizes) )                                                    &
    dim_sizes(1:2) = [ tile_dim_size, snow_dim_size ]

CASE ( var_type_soilt_soil )
  ! Variables with soil tiles and levels
  IF ( PRESENT(ndims) ) ndims = 2
  IF ( PRESENT(dim_names_in) )                                                 &
    dim_names_in(1:2) = [ soilt_dim_name, soil_dim_name ]
  IF ( PRESENT(dim_names_out) )                                                &
    dim_names_out(1:2) = [ soilt_dim_name_out, soil_dim_name_out ]
  IF ( PRESENT(dim_sizes) )                                                    &
    dim_sizes(1:2) = [ soilt_dim_size, soil_dim_size ]

CASE ( var_type_soilt_sclayer )
  ! Variables with soil tiles and soil C levels
  IF ( PRESENT(ndims) ) ndims = 2
  IF ( PRESENT(dim_names_in) )                                                 &
    dim_names_in(1:2) = [ soilt_dim_name, sclayer_dim_name ]
  IF ( PRESENT(dim_names_out) )                                                &
    dim_names_out(1:2) = [ soilt_dim_name_out, sclayer_dim_name_out ]
  IF ( PRESENT(dim_sizes) )                                                    &
    dim_sizes(1:2) = [ soilt_dim_size, sclayer_dim_size ]

CASE ( var_type_soilt_sclayer_scpool )
  ! Variables with soil tiles, soil C levels, and soil C pools.
  IF ( PRESENT(ndims) ) ndims = 3
  IF ( PRESENT(dim_names_in) )                                                 &
    dim_names_in(1:3) = [ soilt_dim_name, sclayer_dim_name,                    &
                           scpool_dim_name ]
  IF ( PRESENT(dim_names_out) )                                                &
    dim_names_out(1:3) = [ soilt_dim_name_out, sclayer_dim_name_out,           &
                            scpool_dim_name_out  ]
  IF ( PRESENT(dim_sizes) )                                                    &
    dim_sizes(1:3) = [ soilt_dim_size, sclayer_dim_size, scpool_dim_size ]

CASE ( var_type_pft_sclayer )
  IF ( PRESENT(ndims) ) ndims = 2
  IF ( PRESENT(dim_names_in) )                                                 &
    dim_names_in(1:2) = [ pft_dim_name, sclayer_dim_name ]
  IF ( PRESENT(dim_names_out) )                                                &
    dim_names_out(1:2) = [ pft_dim_name_out, sclayer_dim_name_out ]
  IF ( PRESENT(dim_sizes) )                                                    &
    dim_sizes(1:2) = [ pft_dim_size, sclayer_dim_size ]

  ! These CABLE var types can piggy back on existing types if so ordained
CASE ( var_type_cable_soil )
  ! CABLE soil variables have 2 levels dimensions
  IF ( PRESENT(ndims) ) ndims = 2
  IF ( PRESENT(dim_names_in) )                                                 &
    dim_names_in(1:2) = [ tile_dim_name, soil_dim_name ]
  IF ( PRESENT(dim_names_out) )                                                &
    dim_names_out(1:2) = [ tile_dim_name_out, soil_dim_name_out ]
  IF ( PRESENT(dim_sizes) )                                                    &
    dim_sizes(1:2) = [ cable_tile_dim_size, cable_soil_dim_size ]

CASE ( var_type_cable_snow )
  ! CABLE snow variables have 2 levels dimensions
  IF ( PRESENT(ndims) ) ndims = 2
  IF ( PRESENT(dim_names_in) )                                                 &
    dim_names_in(1:2) = [ tile_dim_name, snow_dim_name ]
  IF ( PRESENT(dim_names_out) )                                                &
    dim_names_out(1:2) = [ tile_dim_name_out, snow_dim_name_out ]
  IF ( PRESENT(dim_sizes) )                                                    &
    dim_sizes(1:2) = [ cable_tile_dim_size, cable_snow_dim_size ]

CASE ( var_type_cable1l_snow )
  ! CABLE snow variables per tile only have 1 level dimensions
  IF ( PRESENT(ndims) ) ndims = 1
  IF ( PRESENT(dim_names_in) )                                                 &
    dim_names_in = tile_dim_name
  IF ( PRESENT(dim_names_out) )                                                &
    dim_names_out = tile_dim_name_out
  IF ( PRESENT(dim_sizes) )                                                    &
    dim_sizes = cable_tile_dim_size

CASE ( var_type_rp )
  ! Just indicate that there are no vertical dimensions
  IF ( PRESENT(ndims) ) ndims = 0

CASE DEFAULT
  ! Unrecognised variable type
  CALL log_fatal("get_var_levs_dims",                                          &
                 "Unrecognised variable type - check metadata")
END SELECT

IF ( is_climatology_error ) THEN
  CALL log_fatal("get_var_levs_dims",                                          &
                 "This variable type requires more work to be " //             &
                 "initialised from a 12-month climatology.")
END IF

RETURN

END SUBROUTINE get_var_levs_dims
#endif
