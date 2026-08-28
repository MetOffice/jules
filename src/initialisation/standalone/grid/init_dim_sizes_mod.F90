#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_dim_sizes_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_dim_sizes()

USE switches, ONLY: l_co2_interactive

USE jules_soil_biogeochem_mod, ONLY: soil_model_1pool, soil_model_ecosse,      &
                                      soil_model_4pool, l_layeredC,            &
                                      soil_bgc_model, dim_ch4layer

USE jules_surface_mod, ONLY: l_aggregate

USE theta_field_sizes, ONLY: t_i_length, t_j_length

USE ancil_info, ONLY: land_pts, co2_dim_len, co2_dim_row,                      &
                       dim_cs1,                                                &
                       nsurft, dim_cslayer,                                    &
                       dim_soil_n_pool, nsoilt, nmasst

USE jules_deposition_mod, ONLY: ndry_dep_species

USE jules_surface_types_mod, ONLY: npft, ncpft, nnvg, ntype

USE jules_snow_mod, ONLY: nsmax

USE jules_sea_seaice_mod, ONLY: nice, nice_use

USE jules_soil_mod, ONLY: sm_levels, ns_deep, l_tile_soil

USE model_interface_mod, ONLY: bl_level_dim_size, bedrock_dim_size,            &
                               pft_dim_size, quantile_dim_size,                &
                               cpft_dim_size, nvg_dim_size,                    &
                                type_dim_size, tile_dim_size, snow_dim_size,   &
                                soil_dim_size, soilt_dim_size,                 &
                                scpool_dim_size, ch4layer_dim_size,            &
                                sclayer_dim_size, dep_species_dim_size,        &
                                imogen_drive_dim_size, imogen_clim_dim_size,   &
                                nmasst_dim_size

USE imogen_run, ONLY: l_daily_metdata_climatol

USE nlsizes_namelist_mod, ONLY: bl_levels

USE overbank_inundation_mod, ONLY: nquantile_hypso

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Sets dimension sizes depending on science options and sets up the
!   z-dimension sizes for IO
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Calculate number of surface types based on whether we are using the aggregate
! surface type
IF ( l_aggregate ) THEN
  nsurft = 1
ELSE
  nsurft = ntype
END IF

! Set up the number of soil tiles
IF ( l_tile_soil ) THEN
  nsoilt = nsurft
ELSE
  nsoilt = 1
  !This is also the default value, but reaffirming it here for clarity
END IF

! Set up the ice categories - there is only one
nice = 1
! nice_use MUST EQUAL NICE FOR STANDALONE JULES
nice_use = nice

! Set dimensions for soil carbon and nitrogen model.
SELECT CASE ( soil_bgc_model )
CASE ( soil_model_1pool )
  dim_cs1         = 1  !  1 pool
  dim_soil_n_pool = 0  !  Not used, set to zero so no space used.
  IF (l_layeredC) THEN
    dim_cslayer = sm_levels
  ELSE
    dim_cslayer = 1
  END IF
CASE ( soil_model_4pool )
  dim_cs1         = 4   !  4 soil pools
  dim_soil_n_pool = 0   !  Not used, set to zero so no space used.
  IF (l_layeredC) THEN
    dim_cslayer = sm_levels
  ELSE
    dim_cslayer = 1
  END IF
CASE ( soil_model_ecosse )
  dim_cs1         = 4    !  4 soil organic C pools: DPM, RPM, BIO, HUM
  dim_soil_n_pool = 6    !  4 soil organic N pools, NH4, NO3
  ! dim_cslayer was set in jules_soil_biogeochem_mod.
END SELECT

IF ( l_co2_interactive ) THEN
  co2_dim_len = t_i_length
  co2_dim_row = t_j_length
ELSE
  co2_dim_len = 1
  co2_dim_row = 1
END IF

!-----------------------------------------------------------------------------
! Now we know all the required info, set up the z-dimension sizes for IO
!-----------------------------------------------------------------------------
bl_level_dim_size = bl_levels
pft_dim_size      = npft
quantile_dim_size = nquantile_hypso
cpft_dim_size     = ncpft
nvg_dim_size      = nnvg
type_dim_size     = ntype
tile_dim_size     = nsurft
soilt_dim_size    = nsoilt
snow_dim_size     = nsmax
soil_dim_size     = sm_levels
scpool_dim_size   = dim_cs1
sclayer_dim_size  = dim_cslayer
imogen_drive_dim_size = 12   ! number of months in year
IF ( l_daily_metdata_climatol ) THEN
  imogen_clim_dim_size = 360    ! number of days in year
ELSE
  imogen_clim_dim_size = imogen_drive_dim_size
END IF
ch4layer_dim_size = dim_ch4layer
dep_species_dim_size = ndry_dep_species
bedrock_dim_size  = ns_deep
nmasst_dim_size    = nmasst

RETURN

END SUBROUTINE init_dim_sizes

END MODULE init_dim_sizes_mod
#endif
