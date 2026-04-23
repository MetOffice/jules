#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

!-----------------------------------------------------------------------------
! DATA statements for variable metadata
!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Metadata for latitude
!-----------------------------------------------------------------------------
DATA metadata(1) / var_metadata(                                               &
! String identifier
    'latitude',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox latitude",                                                        &
! Units
    "degrees"                                                                  &
  ) /
!-----------------------------------------------------------------------------
! Metadata for longitude
!-----------------------------------------------------------------------------
DATA metadata(2) / var_metadata(                                               &
! String identifier
    'longitude',                                                               &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox longitude",                                                       &
! Units
    "degrees"                                                                  &
  ) /
!-----------------------------------------------------------------------------
! Metadata for land_fraction (flandg)
!-----------------------------------------------------------------------------
DATA metadata(3) / var_metadata(                                               &
! String identifier
    'land_fraction',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox land fraction",                                                   &
! Units
    ""                                                                         &
  ) /
!-----------------------------------------------------------------------------
! Metadata for surf_hgt
!-----------------------------------------------------------------------------
DATA metadata(4) / var_metadata(                                               &
! String identifier
    'surf_hgt',                                                                &
! Variable type
    var_type_surft,                                                            &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for z_land_land
!-----------------------------------------------------------------------------
DATA metadata(5) / var_metadata(                                               &
! String identifier
    'z_land_land',                                                             &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for frac
!-----------------------------------------------------------------------------
DATA metadata(6) / var_metadata(                                               &
! String identifier
    'frac',                                                                    &
! Variable type
    var_type_type,                                                             &
! Long name
    "Fractional cover of each surface type",                                   &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for albsoil
!-----------------------------------------------------------------------------
DATA metadata(7) / var_metadata(                                               &
! String identifier
    'albsoil',                                                                 &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for clay
!-----------------------------------------------------------------------------
DATA metadata(8) / var_metadata(                                               &
! String identifier
    'clay',                                                                    &
! Variable type
    var_type_sclayer,                                                          &
! Long name
    "Soil clay content",                                                       &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for albobs_sw
!-----------------------------------------------------------------------------
DATA metadata(9) / var_metadata(                                               &
! String identifier
    'albobs_sw',                                                               &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for albobs_vis
!-----------------------------------------------------------------------------
DATA metadata(10) / var_metadata(                                              &
! String identifier
    'albobs_vis',                                                              &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for albobs_nir
!-----------------------------------------------------------------------------
DATA metadata(11) / var_metadata(                                              &
! String identifier
    'albobs_nir',                                                              &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for b
!-----------------------------------------------------------------------------
DATA metadata(12) / var_metadata(                                              &
! String identifier
    'b',                                                                       &
! Variable type
    var_type_soil,                                                             &
! Long name
    "Gridbox Brooks-Corey exponent for each soil layer",                       &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for b_const_z
!-----------------------------------------------------------------------------
DATA metadata(13) / var_metadata(                                              &
! String identifier
    'b_const_z',                                                               &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sathh
!-----------------------------------------------------------------------------
DATA metadata(14) / var_metadata(                                              &
! String identifier
    'sathh',                                                                   &
! Variable type
    var_type_soil,                                                             &
! Long name
    "Gridbox saturated soil water pressure for each soil layer",               &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sathh_const_z
!-----------------------------------------------------------------------------
DATA metadata(15) / var_metadata(                                              &
! String identifier
    'sathh_const_z',                                                           &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for satcon
!-----------------------------------------------------------------------------
DATA metadata(16) / var_metadata(                                              &
! String identifier
    'satcon',                                                                  &
! Variable type
    var_type_soil,                                                             &
! Long name
    "Gridbox saturated hydraulic conductivity for each soil layer",            &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for satcon_const_z
!-----------------------------------------------------------------------------
DATA metadata(17) / var_metadata(                                              &
! String identifier
    'satcon_const_z',                                                          &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sm_sat
!-----------------------------------------------------------------------------
DATA metadata(18) / var_metadata(                                              &
! String identifier
    'sm_sat',                                                                  &
! Variable type
    var_type_soil,                                                             &
! Long name
    "Gridbox volumetric moisture content at saturation for each soil layer",   &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sm_sat_const_z
!-----------------------------------------------------------------------------
DATA metadata(19) / var_metadata(                                              &
! String identifier
    'sm_sat_const_z',                                                          &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sm_crit
!-----------------------------------------------------------------------------
DATA metadata(20) / var_metadata(                                              &
! String identifier
    'sm_crit',                                                                 &
! Variable type
    var_type_soil,                                                             &
! Long name
    "Gridbox volumetric moisture content at critical point for each soil layer", &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sm_crit_const_z
!-----------------------------------------------------------------------------
DATA metadata(21) / var_metadata(                                              &
! String identifier
    'sm_crit_const_z',                                                         &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sm_wilt
!-----------------------------------------------------------------------------
DATA metadata(22) / var_metadata(                                              &
! String identifier
    'sm_wilt',                                                                 &
! Variable type
    var_type_soil,                                                             &
! Long name
    "Gridbox  volumetric moisture content at wilting point for each " //       &
    "soil layer",                                                              &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sm_wilt_const_z
!-----------------------------------------------------------------------------
DATA metadata(23) / var_metadata(                                              &
! String identifier
    'sm_wilt_const_z',                                                         &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for hcap
!-----------------------------------------------------------------------------
DATA metadata(24) / var_metadata(                                              &
! String identifier
    'hcap',                                                                    &
! Variable type
    var_type_soil,                                                             &
! Long name
    "Gridbox dry soil heat capacity for each soil layer",                      &
! Units
    "J K-1 m-3"                                                                &
  ) /
!-----------------------------------------------------------------------------
! Metadata for hcap_const_z
!-----------------------------------------------------------------------------
DATA metadata(25) / var_metadata(                                              &
! String identifier
    'hcap_const_z',                                                            &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for hcon
!-----------------------------------------------------------------------------
DATA metadata(26) / var_metadata(                                              &
! String identifier
    'hcon',                                                                    &
! Variable type
    var_type_soil,                                                             &
! Long name
    "Gridbox dry soil thermal conductivity for each soil layer",               &
! Units
    "W m-1 K-1"                                                                &
  ) /
!-----------------------------------------------------------------------------
! Metadata for hcon_const_z
!-----------------------------------------------------------------------------
DATA metadata(27) / var_metadata(                                              &
! String identifier
    'hcon_const_z',                                                            &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fexp
!-----------------------------------------------------------------------------
DATA metadata(28) / var_metadata(                                              &
! String identifier
    'fexp',                                                                    &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for ti_mean
!-----------------------------------------------------------------------------
DATA metadata(29) / var_metadata(                                              &
! String identifier
    'ti_mean',                                                                 &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for ti_sig
!-----------------------------------------------------------------------------
DATA metadata(30) / var_metadata(                                              &
! String identifier
    'ti_sig',                                                                  &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for wrr
!-----------------------------------------------------------------------------
DATA metadata(31) / var_metadata(                                              &
! String identifier
    'wrr',                                                                     &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for hwr
!-----------------------------------------------------------------------------
DATA metadata(32) / var_metadata(                                              &
! String identifier
    'hwr',                                                                     &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for hgt
!-----------------------------------------------------------------------------
DATA metadata(33) / var_metadata(                                              &
! String identifier
    'hgt',                                                                     &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for ztm
!-----------------------------------------------------------------------------
DATA metadata(34) / var_metadata(                                              &
! String identifier
    'ztm',                                                                     &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for disp
!-----------------------------------------------------------------------------
DATA metadata(35) / var_metadata(                                              &
! String identifier
    'disp',                                                                    &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for albwl
!-----------------------------------------------------------------------------
DATA metadata(36) / var_metadata(                                              &
! String identifier
    'albwl',                                                                   &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for albrd
!-----------------------------------------------------------------------------
DATA metadata(37) / var_metadata(                                              &
! String identifier
    'albrd',                                                                   &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for emisw
!-----------------------------------------------------------------------------
DATA metadata(38) / var_metadata(                                              &
! String identifier
    'emisw',                                                                   &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for emisr
!-----------------------------------------------------------------------------
DATA metadata(39) / var_metadata(                                              &
! String identifier
    'emisr',                                                                   &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for canopy
!-----------------------------------------------------------------------------
DATA metadata(40) / var_metadata(                                              &
! String identifier
    'canopy',                                                                  &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile surface/canopy water for snow-free land tiles",                      &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cs
!-----------------------------------------------------------------------------
DATA metadata(41) / var_metadata(                                              &
! String identifier
    'cs',                                                                      &
! Variable type
    var_type_scpool,                                                           &
! Long name is dynamic - set in get_var_attrs
    "",                                                                        &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for ns
!-----------------------------------------------------------------------------
DATA metadata(42) / var_metadata(                                              &
! String identifier
    'ns',                                                                      &
! Variable type
    var_type_scpool,                                                           &
! Long name
    "Gridbox soil organic Nitrogen in each pool (DPM,RPM,bio,hum)",            &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for immob_n
!-----------------------------------------------------------------------------
DATA metadata(43) / var_metadata(                                              &
! String identifier
    'immob_n',                                                                 &
! Variable type
    var_type_scpool,                                                           &
! Long name
    "Gridbox soil Nitrogen immobilisation in each pool (DPM,RPM,bio,hum)",     &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for immob_n_pot
!-----------------------------------------------------------------------------
DATA metadata(44) / var_metadata(                                              &
! String identifier
    'immob_n_pot',                                                             &
! Variable type
    var_type_scpool,                                                           &
! Long name
    "Gridbox soil potential Nitrogen immobilisation in each pool (DPM,RPM,bio,hum)", &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for minl_n
!-----------------------------------------------------------------------------
DATA metadata(45) / var_metadata(                                              &
! String identifier
    'minl_n',                                                                  &
! Variable type
    var_type_scpool,                                                           &
! Long name
    "Gridbox soil Nitrogen mineralisation in each pool (DPM,RPM,bio,hum)",     &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for minl_n_pot
!-----------------------------------------------------------------------------
DATA metadata(46) / var_metadata(                                              &
! String identifier
    'minl_n_pot',                                                              &
! Variable type
    var_type_scpool,                                                           &
! Long name
    "Gridbox soil potential Nitrogen mineralisation in each pool (DPM,RPM,bio,hum)", &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for resp_s_diag
!-----------------------------------------------------------------------------
DATA metadata(47) / var_metadata(                                              &
! String identifier
    'resp_s_diag',                                                             &
! Variable type
    var_type_scpool,                                                           &
! Long name
    "Gridbox soil respiration carbon flux in each pool (DPM,RPM,bio,hum)",     &
! Units
    "kg m-2 per 360days"                                                       &
  ) /

!-----------------------------------------------------------------------------
! Metadata for resp_s_pot_diag
!-----------------------------------------------------------------------------
DATA metadata(48) / var_metadata(                                              &
! String identifier
    'resp_s_pot_diag',                                                         &
! Variable type
    var_type_scpool,                                                           &
! Long name
    "Gridbox potential soil respiration carbon flux in each pool (DPM,RPM,bio,hum)", &
! Units
    "kg m-2 per 360days"                                                       &
  ) /

!-----------------------------------------------------------------------------
! Metadata for soil_cn
!-----------------------------------------------------------------------------
DATA metadata(49) / var_metadata(                                              &
! String identifier
    'soil_CN',                                                                 &
! Variable type
    var_type_scpool,                                                           &
! Long name
    "Gridbox Soil C:N in each pool (DPM,RPM,bio,hum)",                         &
! Units
    ":"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for soil_cn_gb
!-----------------------------------------------------------------------------
DATA metadata(50) / var_metadata(                                              &
! String identifier
    'soil_CN_gb',                                                              &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox Soil C:N",                                                        &
! Units
    ":"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for resp_s_pot_diag_gb
!-----------------------------------------------------------------------------
DATA metadata(51) / var_metadata(                                              &
! String identifier
    'resp_s_pot_diag_gb',                                                      &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean potential soil respiration",                                 &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for resp_s_diag_gb
!-----------------------------------------------------------------------------
DATA metadata(52) / var_metadata(                                              &
! String identifier
    'resp_s_diag_gb',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean soil respiration",                                           &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for minl_n_pot_gb
!-----------------------------------------------------------------------------
DATA metadata(53) / var_metadata(                                              &
! String identifier
    'minl_n_pot_gb',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean potential soil N mineralisation",                            &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for minl_n_gb
!-----------------------------------------------------------------------------
DATA metadata(54) / var_metadata(                                              &
! String identifier
    'minl_n_gb',                                                               &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean potential soil N mineralisation",                            &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for immob_pot_gb
!-----------------------------------------------------------------------------
DATA metadata(55) / var_metadata(                                              &
! String identifier
    'immob_n_pot_gb',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean potential soil N immobilisation",                            &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for immob_n_gb
!-----------------------------------------------------------------------------
DATA metadata(56) / var_metadata(                                              &
! String identifier
    'immob_n_gb',                                                              &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean potential soil N immobilisation",                            &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for ns_gb
!-----------------------------------------------------------------------------
DATA metadata(57) / var_metadata(                                              &
! String identifier
    'ns_gb',                                                                   &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox soil organic Nitrogen (total)",                                   &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fN
!-----------------------------------------------------------------------------
DATA metadata(58) / var_metadata(                                              &
! String identifier
    'fN',                                                                      &
! Variable type
    var_type_sclayer,                                                          &
! Long name
    "Gridbox Nitrogen availability rate modifier",                             &
! Units
    ":"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for gs
!-----------------------------------------------------------------------------
DATA metadata(59) / var_metadata(                                              &
! String identifier
    'gs',                                                                      &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox surface conductance to evaporation",                              &
! Units
    "m s-1"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snow_tile
!-----------------------------------------------------------------------------
DATA metadata(60) / var_metadata(                                              &
! String identifier
    'snow_tile',                                                               &
! Variable type
    var_type_surft,                                                            &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sthuf
!-----------------------------------------------------------------------------
DATA metadata(61) / var_metadata(                                              &
! String identifier
    'sthuf',                                                                   &
! Variable type
    var_type_soil,                                                             &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for t_soil
!-----------------------------------------------------------------------------
DATA metadata(62) / var_metadata(                                              &
! String identifier
    't_soil',                                                                  &
! Variable type
    var_type_soil,                                                             &
! Long name
    "Gridbox sub-surface temperature of each layer",                           &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for tsoil_deep
!-----------------------------------------------------------------------------
DATA metadata(63) / var_metadata(                                              &
! String identifier
    'tsoil_deep',                                                              &
! Variable type
    var_type_bedrock,                                                          &
! Long name
    "Temperature in bedrock layers",                                           &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for tstar_tile
!-----------------------------------------------------------------------------
DATA metadata(64) / var_metadata(                                              &
! String identifier
    'tstar_tile',                                                              &
! Variable type
    var_type_surft,                                                            &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lai
!-----------------------------------------------------------------------------
DATA metadata(65) / var_metadata(                                              &
! String identifier
    'lai',                                                                     &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT leaf area index",                                                     &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for canht
!-----------------------------------------------------------------------------
DATA metadata(66) / var_metadata(                                              &
! String identifier
    'canht',                                                                   &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT canopy height",                                                       &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sthzw
!-----------------------------------------------------------------------------
DATA metadata(67) / var_metadata(                                              &
! String identifier
    'sthzw',                                                                   &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Soil wetness in deep LSH/TOPMODEL layer",                                 &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for zw
!-----------------------------------------------------------------------------
DATA metadata(68) / var_metadata(                                              &
! String identifier
    'zw',                                                                      &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean depth to water table",                                       &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for rgrain
!-----------------------------------------------------------------------------
DATA metadata(69) / var_metadata(                                              &
! String identifier
    'rgrain',                                                                  &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile snow surface grain size",                                            &
! Units
    "microns"                                                                  &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cv
!-----------------------------------------------------------------------------
DATA metadata(70) / var_metadata(                                              &
! String identifier
    'cv',                                                                      &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean vegetation carbon at end of model timestep.",                &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for rho_snow
!-----------------------------------------------------------------------------
DATA metadata(71) / var_metadata(                                              &
! String identifier
    'rho_snow',                                                                &
! Variable type
    var_type_surft,                                                            &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snow_depth
!-----------------------------------------------------------------------------
DATA metadata(72) / var_metadata(                                              &
! String identifier
    'snow_depth',                                                              &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile snow depth (on ground)",                                             &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snow_grnd
!-----------------------------------------------------------------------------
DATA metadata(73) / var_metadata(                                              &
! String identifier
    'snow_grnd',                                                               &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile snow on ground below canopy (snow_grnd)",                            &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for nsnow
!-----------------------------------------------------------------------------
DATA metadata(74) / var_metadata(                                              &
! String identifier
    'nsnow',                                                                   &
! Variable type
    var_type_surft,                                                            &
! Long name
   "Number of snow layers",                                                    &
! Units
   "1"                                                                         &
 ) /
!-----------------------------------------------------------------------------
! Metadata for snow_ds
!-----------------------------------------------------------------------------
DATA metadata(75) / var_metadata(                                              &
! String identifier
    'snow_ds',                                                                 &
! Variable type
    var_type_snow,                                                             &
! Long name
    "Depth of snow layers for each tile",                                      &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snow_ice
!-----------------------------------------------------------------------------
DATA metadata(76) / var_metadata(                                              &
! String identifier
    'snow_ice',                                                                &
! Variable type
    var_type_snow,                                                             &
! Long name
    "Ice mass in snow layers for each tile",                                   &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snow_liq
!-----------------------------------------------------------------------------
DATA metadata(77) / var_metadata(                                              &
! String identifier
    'snow_liq',                                                                &
! Variable type
    var_type_snow,                                                             &
! Long name
    "Liquid mass in snow layers for each tile",                                &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for tsnow
!-----------------------------------------------------------------------------
DATA metadata(78) / var_metadata(                                              &
! String identifier
    'tsnow',                                                                   &
! Variable type
    var_type_snow,                                                             &
! Long name
    "Temperature of snow layers for each tile",                                &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for rgrainl
!-----------------------------------------------------------------------------
DATA metadata(79) / var_metadata(                                              &
! String identifier
    'rgrainl',                                                                 &
! Variable type
    var_type_snow,                                                             &
! Long name
    "Grain size in snow layers for each tile",                                 &
! Units
    "microns"                                                                  &
  ) /
!-----------------------------------------------------------------------------
! Metadata for pstar
!-----------------------------------------------------------------------------
DATA metadata(80) / var_metadata(                                              &
! String identifier
    'pstar',                                                                   &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox surface pressure",                                                &
! Units
    "Pa"                                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for q
!-----------------------------------------------------------------------------
DATA metadata(81) / var_metadata(                                              &
! String identifier
    'q',                                                                       &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for t
!-----------------------------------------------------------------------------
DATA metadata(82) / var_metadata(                                              &
! String identifier
    't',                                                                       &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for rad_net
!-----------------------------------------------------------------------------
DATA metadata(83) / var_metadata(                                              &
! String identifier
    'rad_net',                                                                 &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Surface net radiation of land points",                                    &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lw_net
!-----------------------------------------------------------------------------
DATA metadata(84) / var_metadata(                                              &
! String identifier
    'lw_net',                                                                  &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox net downward longwave radiation at surface",                      &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sw_net
!-----------------------------------------------------------------------------
DATA metadata(85) / var_metadata(                                              &
! String identifier
    'sw_net',                                                                  &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox net downward shortwave radiation at surface",                     &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lw_down
!-----------------------------------------------------------------------------
DATA metadata(86) / var_metadata(                                              &
! String identifier
    'lw_down',                                                                 &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox surface downward LW radiation",                                   &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sw_down
!-----------------------------------------------------------------------------
DATA metadata(87) / var_metadata(                                              &
! String identifier
    'sw_down',                                                                 &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox surface downward SW radiation",                                   &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for diff_rad
!-----------------------------------------------------------------------------
DATA metadata(88) / var_metadata(                                              &
! String identifier
    'diff_rad',                                                                &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for precip
!-----------------------------------------------------------------------------
DATA metadata(89) / var_metadata(                                              &
! String identifier
    'precip',                                                                  &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox precipitation rate",                                              &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for tot_rain
!-----------------------------------------------------------------------------
DATA metadata(90) / var_metadata(                                              &
! String identifier
    'tot_rain',                                                                &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for tot_snow
!-----------------------------------------------------------------------------
DATA metadata(91) / var_metadata(                                              &
! String identifier
    'tot_snow',                                                                &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for con_rain
!-----------------------------------------------------------------------------
DATA metadata(92) / var_metadata(                                              &
! String identifier
    'con_rain',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox convective rainfall",                                             &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for ls_rain
!-----------------------------------------------------------------------------
DATA metadata(93) / var_metadata(                                              &
! String identifier
    'ls_rain',                                                                 &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox large-scale rainfall",                                            &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for con_snow
!-----------------------------------------------------------------------------
DATA metadata(94) / var_metadata(                                              &
! String identifier
    'con_snow',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox convective snowfall",                                             &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for ls_snow
!-----------------------------------------------------------------------------
DATA metadata(95) / var_metadata(                                              &
! String identifier
    'ls_snow',                                                                 &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox large-scale snowfall",                                            &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for wind
!-----------------------------------------------------------------------------
DATA metadata(96) / var_metadata(                                              &
! String identifier
    'wind',                                                                    &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox wind speed",                                                      &
! Units
    "m s-1"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for u
!-----------------------------------------------------------------------------
DATA metadata(97) / var_metadata(                                              &
! String identifier
    'u',                                                                       &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for v
!-----------------------------------------------------------------------------
DATA metadata(98) / var_metadata(                                              &
! String identifier
    'v',                                                                       &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for z1_tq_in
!-----------------------------------------------------------------------------
DATA metadata(99) / var_metadata(                                              &
! String identifier
    'z1_tq_in',                                                                &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for diurnal temperature range
!-----------------------------------------------------------------------------
DATA metadata(100) / var_metadata(                                             &
! String identifier
    'dt_range',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Diurnal temperature range",                                               &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for albedo_land
!-----------------------------------------------------------------------------
DATA metadata(101) / var_metadata(                                             &
! String identifier
    'albedo_land',                                                             &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox albedo (as used for net shortwave calculation)",                  &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for NDVI_land
!-----------------------------------------------------------------------------
DATA metadata(102) / var_metadata(                                             &
! String identifier
    'NDVI_land',                                                               &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox NDVI (using sum of direct and diffuse for (NIR-VIS)/(NIR+VIS))",  &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for canopy_gb
!-----------------------------------------------------------------------------
DATA metadata(103) / var_metadata(                                             &
! String identifier
    'canopy_gb',                                                               &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox canopy water content",                                            &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cs_gb
!-----------------------------------------------------------------------------
DATA metadata(104) / var_metadata(                                             &
! String identifier
    'cs_gb',                                                                   &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox soil carbon (total)",                                             &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for depth_frozen
!-----------------------------------------------------------------------------
DATA metadata(105) / var_metadata(                                             &
! String identifier
    'depth_frozen',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox depth of frozen ground at surface (temperature definition)",      &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for depth_frozen_sthf
!-----------------------------------------------------------------------------
DATA metadata(106) / var_metadata(                                             &
! String identifier
    'depth_frozen_sthf',                                                       &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox depth of frozen ground at surface (soil moisture definition)",    &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for depth_unfrozen
!-----------------------------------------------------------------------------
DATA metadata(107) / var_metadata(                                             &
! String identifier
    'depth_unfrozen',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox depth of unfrozen ground at surface (temeprature definition)",    &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for depth_unfrozen_sthf
!-----------------------------------------------------------------------------
DATA metadata(108) / var_metadata(                                             &
! String identifier
    'depth_unfrozen_sthf',                                                     &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox depth of unfrozen ground at surface (soil moisture definition)",  &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for drain
!-----------------------------------------------------------------------------
DATA metadata(109) / var_metadata(                                             &
! String identifier
    'drain',                                                                   &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Drainage from bottom (nshyd) soil layer",                                 &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for elake
!-----------------------------------------------------------------------------
DATA metadata(110) / var_metadata(                                             &
! String identifier
    'elake',                                                                   &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean evaporation from lakes",                                     &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for emis_gb
!-----------------------------------------------------------------------------
DATA metadata(111) / var_metadata(                                             &
! String identifier
    'emis_gb',                                                                 &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox emissivity",                                                      &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fch4_wetl
!-----------------------------------------------------------------------------
DATA metadata(112) / var_metadata(                                             &
! String identifier
    'fch4_wetl',                                                               &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Scaled methane flux from wetland fraction (for use in atmos chem)",       &
! Units
    "10^-9 kg m-2 s-1"                                                         &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fch4_wetl_cs
!-----------------------------------------------------------------------------
DATA metadata(113) / var_metadata(                                             &
! String identifier
    'fch4_wetl_cs',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Scaled methane flux from wetland fraction (cs substrate)",                &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fch4_wetl_npp
!-----------------------------------------------------------------------------
DATA metadata(114) / var_metadata(                                             &
! String identifier
    'fch4_wetl_npp',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Scaled methane flux from wetland fraction (npp substrate)",               &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fch4_wetl_resps
!-----------------------------------------------------------------------------
DATA metadata(115) / var_metadata(                                             &
! String identifier
    'fch4_wetl_resps',                                                         &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Scaled methane flux from wetland fraction (soil resp substrate)",         &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fsat
!-----------------------------------------------------------------------------
DATA metadata(116) / var_metadata(                                             &
! String identifier
    'fsat',                                                                    &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Surface saturated fraction",                                              &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fsmc_gb
!-----------------------------------------------------------------------------
DATA metadata(117) / var_metadata(                                             &
! String identifier
    'fsmc_gb',                                                                 &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox soil moisture availability factor (beta)",                        &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fwetl
!-----------------------------------------------------------------------------
DATA metadata(118) / var_metadata(                                             &
! String identifier
    'fwetl',                                                                   &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Wetland fraction at end of model timestep.",                              &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for gpp_gb
!-----------------------------------------------------------------------------
DATA metadata(119) / var_metadata(                                             &
! String identifier
    'gpp_gb',                                                                  &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox gross primary productivity",                                      &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for hf_snow_melt
!-----------------------------------------------------------------------------
DATA metadata(120) / var_metadata(                                             &
! String identifier
    'hf_snow_melt',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox snowmelt heat flux",                                              &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for land_index
!-----------------------------------------------------------------------------
DATA metadata(121) / var_metadata(                                             &
! String identifier
    'land_index',                                                              &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Index (gridbox number) of land points",                                   &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lice_index
!-----------------------------------------------------------------------------
DATA metadata(122) / var_metadata(                                             &
! String identifier
    'lice_index',                                                              &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Index (gridbox number) of land ice points",                               &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lit_c_mean
!-----------------------------------------------------------------------------
DATA metadata(123) / var_metadata(                                             &
! String identifier
    'lit_c_mean',                                                              &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean carbon litter",                                              &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lw_up
!-----------------------------------------------------------------------------
DATA metadata(124) / var_metadata(                                             &
! String identifier
    'lw_up',                                                                   &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox surface upward LW radiation of land points",                      &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for npp_gb
!-----------------------------------------------------------------------------
DATA metadata(125) / var_metadata(                                             &
! String identifier
    'npp_gb',                                                                  &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox net primary productivity prior to N limitation",                  &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for qbase
!-----------------------------------------------------------------------------
DATA metadata(126) / var_metadata(                                             &
! String identifier
    'qbase',                                                                   &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Baseflow (lateral subsurface runoff)",                                    &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for qbase_zw
!-----------------------------------------------------------------------------
DATA metadata(127) / var_metadata(                                             &
! String identifier
    'qbase_zw',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Baseflow from deep LSH/TOPMODEL layer",                                   &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for resp_p_gb
!-----------------------------------------------------------------------------
DATA metadata(128) / var_metadata(                                             &
! String identifier
    'resp_p_gb',                                                               &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox plant respiration",                                               &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for resp_s_gb
!-----------------------------------------------------------------------------
DATA metadata(129) / var_metadata(                                             &
! String identifier
    'resp_s_gb',                                                               &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox soil respiration (total)",                                        &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for resp_s_dr_out
!-----------------------------------------------------------------------------
DATA metadata(130) / var_metadata(                                             &
! String identifier
    'resp_s_dr_out',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean soil respiration for driving TRIFFID",                       &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_root
!-----------------------------------------------------------------------------
DATA metadata(131) / var_metadata(                                             &
! String identifier
    'n_root',                                                                  &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT Root N content scaled by LAI_BAL in sf_stom",                         &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_stem
!-----------------------------------------------------------------------------
DATA metadata(132) / var_metadata(                                             &
! String identifier
    'n_stem',                                                                  &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT Stem N content scaled by LAI_BAL in sf_stom",                         &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_leaf
!-----------------------------------------------------------------------------
DATA metadata(133) / var_metadata(                                             &
! String identifier
    'n_leaf',                                                                  &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT Leaf N content scaled by LAI in sf_stom",                             &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for pc_s
!-----------------------------------------------------------------------------
DATA metadata(134) / var_metadata(                                             &
! String identifier
    'pc_s',                                                                    &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT net carbon available for spreading",                                  &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for runoff
!-----------------------------------------------------------------------------
DATA metadata(135) / var_metadata(                                             &
! String identifier
    'runoff',                                                                  &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox runoff rate",                                                     &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sat_excess_roff
!-----------------------------------------------------------------------------
DATA metadata(136) / var_metadata(                                             &
! String identifier
    'sat_excess_roff',                                                         &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Saturation excess surface ('Dunne') runoff",                              &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for smc_avail_top
!-----------------------------------------------------------------------------
DATA metadata(137) / var_metadata(                                             &
! String identifier
    'smc_avail_top',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name is dynamic - set in get_var_attrs
    "",                                                                        &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for smc_avail_tot
!-----------------------------------------------------------------------------
DATA metadata(138) / var_metadata(                                             &
! String identifier
    'smc_avail_tot',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox available moisture in soil column",                               &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for smc_tot
!-----------------------------------------------------------------------------
DATA metadata(139) / var_metadata(                                             &
! String identifier
    'smc_tot',                                                                 &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox total soil moisture in column",                                   &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snomlt_sub_htf
!-----------------------------------------------------------------------------
DATA metadata(140) / var_metadata(                                             &
! String identifier
    'snomlt_sub_htf',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox sub-canopy snowmelt heat flux",                                   &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snow_can_gb
!-----------------------------------------------------------------------------
DATA metadata(141) / var_metadata(                                             &
! String identifier
    'snow_can_gb',                                                             &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox snow on canopy",                                                  &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snow_depth_gb
!-----------------------------------------------------------------------------
DATA metadata(142) / var_metadata(                                             &
! String identifier
    'snow_depth_gb',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Snow depth (on ground)",                                                  &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snow_frac
!-----------------------------------------------------------------------------
DATA metadata(143) / var_metadata(                                             &
! String identifier
    'snow_frac',                                                               &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox snow-covered fraction of land points",                            &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snow_grnd_gb
!-----------------------------------------------------------------------------
DATA metadata(144) / var_metadata(                                             &
! String identifier
    'snow_grnd_gb',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox snow below canopy (snow_grnd)",                                   &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snow_ice_gb
!-----------------------------------------------------------------------------
DATA metadata(145) / var_metadata(                                             &
! String identifier
    'snow_ice_gb',                                                             &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox ice content of snow on ground",                                   &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snow_liq_gb
!-----------------------------------------------------------------------------
DATA metadata(146) / var_metadata(                                             &
! String identifier
    'snow_liq_gb',                                                             &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox liquid content of snow on ground",                                &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snow_melt_gb
!-----------------------------------------------------------------------------
DATA metadata(147) / var_metadata(                                             &
! String identifier
    'snow_melt_gb',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox rate of snowmelt",                                                &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for soil_index
!-----------------------------------------------------------------------------
DATA metadata(148) / var_metadata(                                             &
! String identifier
    'soil_index',                                                              &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Index (gridbox number) of soil points",                                   &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sub_surf_roff
!-----------------------------------------------------------------------------
DATA metadata(149) / var_metadata(                                             &
! String identifier
    'sub_surf_roff',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox sub-surface runoff",                                              &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for surf_roff
!-----------------------------------------------------------------------------
DATA metadata(150) / var_metadata(                                             &
! String identifier
    'surf_roff',                                                               &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox surface runoff",                                                  &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for swet_liq_tot
!-----------------------------------------------------------------------------
DATA metadata(151) / var_metadata(                                             &
! String identifier
    'swet_liq_tot',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox unfrozen soil moisture as fraction of saturation",                &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for swet_tot
!-----------------------------------------------------------------------------
DATA metadata(152) / var_metadata(                                             &
! String identifier
    'swet_tot',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox soil moisture as fraction of saturation",                         &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for tfall
!-----------------------------------------------------------------------------
DATA metadata(153) / var_metadata(                                             &
! String identifier
    'tfall',                                                                   &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox throughfall",                                                     &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for trad
!-----------------------------------------------------------------------------
DATA metadata(154) / var_metadata(                                             &
! String identifier
    'trad',                                                                    &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox effective radiative temperature (assuming emissivity=1)",         &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for c_veg
!-----------------------------------------------------------------------------
DATA metadata(155) / var_metadata(                                             &
! String identifier
    'c_veg',                                                                   &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT total carbon content of the vegetation at the end of model timestep.",&
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fapar
!-----------------------------------------------------------------------------
DATA metadata(156) / var_metadata(                                             &
! String identifier
    'fapar',                                                                   &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT Fraction of Absorbed Photosynthetically Active Radiation",            &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fao_et0
!-----------------------------------------------------------------------------
DATA metadata(157) / var_metadata(                                             &
! String identifier
    'fao_et0',                                                                 &
! Variable type
    var_type_surface,                                                          &
! Long name
    "FAO Penman-Monteith evapotranspiration for reference crop",               &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for flux_o3_stom
!-----------------------------------------------------------------------------
DATA metadata(158) / var_metadata(                                             &
! String identifier
    'flux_o3_stom',                                                            &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Flux of O3 to stomata",                                                   &
! Units
    "mol m-2 s-1"                                                              &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fsmc
!-----------------------------------------------------------------------------
DATA metadata(159) / var_metadata(                                             &
! String identifier
    'fsmc',                                                                    &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT soil moisture availability factor (beta)",                            &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for g_leaf
!-----------------------------------------------------------------------------
DATA metadata(160) / var_metadata(                                             &
! String identifier
    'g_leaf',                                                                  &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT leaf turnover rate",                                                  &
! Units
    "per 360days"                                                              &
  ) /
!-----------------------------------------------------------------------------
! Metadata for g_leaf_day
!-----------------------------------------------------------------------------
DATA metadata(161) / var_metadata(                                             &
! String identifier
    'g_leaf_day',                                                              &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT mean leaf turnover rate for input to PHENOL",                         &
! Units
    "per 360days"                                                              &
  ) /
!-----------------------------------------------------------------------------
! Metadata for g_leaf_dr_out
!-----------------------------------------------------------------------------
DATA metadata(162) / var_metadata(                                             &
! String identifier
    'g_leaf_dr_out',                                                           &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT mean leaf turnover rate for driving TRIFFID",                         &
! Units
    "per 360days"                                                              &
  ) /
!-----------------------------------------------------------------------------
! Metadata for g_leaf_phen
!-----------------------------------------------------------------------------
DATA metadata(163) / var_metadata(                                             &
! String identifier
    'g_leaf_phen',                                                             &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT mean leaf turnover rate over phenology period",                       &
! Units
    "per 360days"                                                              &
  ) /
!-----------------------------------------------------------------------------
! Metadata for gpp
!-----------------------------------------------------------------------------
DATA metadata(164) / var_metadata(                                             &
! String identifier
    'gpp',                                                                     &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT gross primary productivity",                                          &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lai_phen
!-----------------------------------------------------------------------------
DATA metadata(165) / var_metadata(                                             &
! String identifier
    'lai_phen',                                                                &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT leaf area index after phenology",                                     &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lit_c
!-----------------------------------------------------------------------------
DATA metadata(166) / var_metadata(                                             &
! String identifier
    'lit_c',                                                                   &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT carbon Litter",                                                       &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for npp_dr_out
!-----------------------------------------------------------------------------
DATA metadata(167) / var_metadata(                                             &
! String identifier
    'npp_dr_out',                                                              &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT mean NPP for driving TRIFFID/N model ",                               &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for npp
!-----------------------------------------------------------------------------
DATA metadata(168) / var_metadata(                                             &
! String identifier
    'npp',                                                                     &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT net primary productivity prior to N limitation",                      &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for o3_exp_fac
!-----------------------------------------------------------------------------
DATA metadata(169) / var_metadata(                                             &
! String identifier
    'o3_exp_fac',                                                              &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Ozone exposure factor",                                                   &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for resp_p
!-----------------------------------------------------------------------------
DATA metadata(170) / var_metadata(                                             &
! String identifier
    'resp_p',                                                                  &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT plant respiration",                                                   &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for resp_w_dr_out
!-----------------------------------------------------------------------------
DATA metadata(171) / var_metadata(                                             &
! String identifier
    'resp_w_dr_out',                                                           &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT mean wood respiration carbon flux for driving TRIFFID",               &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for resp_l
!-----------------------------------------------------------------------------
DATA metadata(172) / var_metadata(                                             &
! String identifier
    'resp_l',                                                                  &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT leaf maintenance respiration carbon flux (in sf_stom)",               &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for resp_r
!-----------------------------------------------------------------------------
DATA metadata(173) / var_metadata(                                             &
! String identifier
    'resp_r',                                                                  &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT root maintenance respiration carbon flux (in sf_stom)",               &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for resp_w
!-----------------------------------------------------------------------------
DATA metadata(174) / var_metadata(                                             &
! String identifier
    'resp_w',                                                                  &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT wood maintenance respiration carbon flux (in sf_stom)",               &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for resp_s
!-----------------------------------------------------------------------------
DATA metadata(175) / var_metadata(                                             &
! String identifier
    'resp_s',                                                                  &
! Variable type
    var_type_scpool,                                                           &
! Long name is dynamic - set in get_var_attrs
    "",                                                                        &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cosz
!-----------------------------------------------------------------------------
DATA metadata(176) / var_metadata(                                             &
! String identifier
    'cosz',                                                                    &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Cosine of the zenith angle",                                              &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for diff_frac
!-----------------------------------------------------------------------------
DATA metadata(177) / var_metadata(                                             &
! String identifier
    'diff_frac',                                                               &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox fraction of radiation that is diffuse",                           &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for ecan_gb
!-----------------------------------------------------------------------------
DATA metadata(178) / var_metadata(                                             &
! String identifier
    'ecan_gb',                                                                 &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean evaporation from canopy/surface store",                      &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for ei_gb
!-----------------------------------------------------------------------------
DATA metadata(179) / var_metadata(                                             &
! String identifier
    'ei_gb',                                                                   &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox sublimation from lying snow or sea-ice",                          &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for et_stom
!-----------------------------------------------------------------------------
DATA metadata(180) / var_metadata(                                             &
! String identifier
    'et_stom',                                                                 &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile stomatal transpiration",                                             &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for et_stom_gb
!-----------------------------------------------------------------------------
DATA metadata(181) / var_metadata(                                             &
! String identifier
    'et_stom_gb',                                                              &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox stomatal transpiration",                                          &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for esoil_gb
!-----------------------------------------------------------------------------
DATA metadata(182) / var_metadata(                                             &
! String identifier
    'esoil_gb',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox surface evapotranspiration from soil moisture store",             &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fqw_gb
!-----------------------------------------------------------------------------
DATA metadata(183) / var_metadata(                                             &
! String identifier
    'fqw_gb',                                                                  &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox moisture flux from surface",                                      &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for ftl_gb
!-----------------------------------------------------------------------------
DATA metadata(184) / var_metadata(                                             &
! String identifier
    'ftl_gb',                                                                  &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox surface sensible heat flux",                                      &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for land_albedo_1
!-----------------------------------------------------------------------------
DATA metadata(185) / var_metadata(                                             &
! String identifier
    'land_albedo_1',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox band 1 albedo (direct beam visible)",                             &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for land_albedo_2
!-----------------------------------------------------------------------------
DATA metadata(186) / var_metadata(                                             &
! String identifier
    'land_albedo_2',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox band 2 albedo (diffuse visible)",                                 &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for land_albedo_3
!-----------------------------------------------------------------------------
DATA metadata(187) / var_metadata(                                             &
! String identifier
    'land_albedo_3',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox band 3 albedo (direct beam NIR)",                                 &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for land_albedo_4
!-----------------------------------------------------------------------------
DATA metadata(188) / var_metadata(                                             &
! String identifier
    'land_albedo_4',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox band 4 albedo (diffuse NIR)",                                     &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for latent_heat
!-----------------------------------------------------------------------------
DATA metadata(189) / var_metadata(                                             &
! String identifier
    'latent_heat',                                                             &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox surface latent heat flux",                                        &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for q1p5m_gb
!-----------------------------------------------------------------------------
DATA metadata(190) / var_metadata(                                             &
! String identifier
    'q1p5m_gb',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox specific humidity at 1.5m height",                                &
! Units
    "kg kg-1"                                                                  &
  ) /
!-----------------------------------------------------------------------------
! Metadata for qw1
!-----------------------------------------------------------------------------
DATA metadata(191) / var_metadata(                                             &
! String identifier
    'qw1',                                                                     &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox specific humidity (total water content)",                         &
! Units
    "kg kg-1"                                                                  &
  ) /
!-----------------------------------------------------------------------------
! Metadata for rainfall
!-----------------------------------------------------------------------------
DATA metadata(192) / var_metadata(                                             &
! String identifier
    'rainfall',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox rainfall rate",                                                   &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snomlt_surf_htf
!-----------------------------------------------------------------------------
DATA metadata(193) / var_metadata(                                             &
! String identifier
    'snomlt_surf_htf',                                                         &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox heat flux used for surface melting of snow",                      &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snowfall
!-----------------------------------------------------------------------------
DATA metadata(194) / var_metadata(                                             &
! String identifier
    'snowfall',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox snowfall rate",                                                   &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snow_mass_gb
!-----------------------------------------------------------------------------
DATA metadata(195) / var_metadata(                                             &
! String identifier
    'snow_mass_gb',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox snowmass (on land this is lying_snow=snow_tile+snow_grnd)",       &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for surf_ht_flux_gb
!-----------------------------------------------------------------------------
DATA metadata(196) / var_metadata(                                             &
! String identifier
    'surf_ht_flux_gb',                                                         &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox net downward heat flux at surface over land and sea-ice fraction",&
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for t1p5m_gb
!-----------------------------------------------------------------------------
DATA metadata(197) / var_metadata(                                             &
! String identifier
    't1p5m_gb',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox temperature at 1.5m height",                                      &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for taux1
!-----------------------------------------------------------------------------
DATA metadata(198) / var_metadata(                                             &
! String identifier
    'taux1',                                                                   &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox westerly component of surface wind stress",                       &
! Units
    "N m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for tauy1
!-----------------------------------------------------------------------------
DATA metadata(199) / var_metadata(                                             &
! String identifier
    'tauy1',                                                                   &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox southerly component of surface wind stress",                      &
! Units
    "N m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for tl1
!-----------------------------------------------------------------------------
DATA metadata(200) / var_metadata(                                             &
! String identifier
    'tl1',                                                                     &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox ice/liquid water temperature",                                    &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for tstar_gb
!-----------------------------------------------------------------------------
DATA metadata(201) / var_metadata(                                             &
! String identifier
    'tstar_gb',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox surface temperature",                                             &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for u1
!-----------------------------------------------------------------------------
DATA metadata(202) / var_metadata(                                             &
! String identifier
    'u1',                                                                      &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox westerly wind component",                                         &
! Units
    "m s-1"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for u10m
!-----------------------------------------------------------------------------
DATA metadata(203) / var_metadata(                                             &
! String identifier
    'u10m',                                                                    &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox westerly wind component at 10 m height",                          &
! Units
    "m s-1"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for v1
!-----------------------------------------------------------------------------
DATA metadata(204) / var_metadata(                                             &
! String identifier
    'v1',                                                                      &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox southerly wind component",                                        &
! Units
    "m s-1"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for v10m
!-----------------------------------------------------------------------------
DATA metadata(205) / var_metadata(                                             &
! String identifier
    'v10m',                                                                    &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox southerly wind component at 10 m height",                         &
! Units
    "m s-1"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for ext
!-----------------------------------------------------------------------------
DATA metadata(206) / var_metadata(                                             &
! String identifier
    'ext',                                                                     &
! Variable type
    var_type_soil,                                                             &
! Long name
    "Gridbox extraction of water from each soil layer",                        &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for smcl
!-----------------------------------------------------------------------------
DATA metadata(207) / var_metadata(                                             &
! String identifier
    'smcl',                                                                    &
! Variable type
    var_type_soil,                                                             &
! Long name
    "Gridbox moisture content of each soil layer",                             &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for soil_wet
!-----------------------------------------------------------------------------
DATA metadata(208) / var_metadata(                                             &
! String identifier
    'soil_wet',                                                                &
! Variable type
    var_type_soil,                                                             &
! Long name
    "Gridbox total moisture content of each soil layer, as fraction of saturation", &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sthf
!-----------------------------------------------------------------------------
DATA metadata(209) / var_metadata(                                             &
! String identifier
    'sthf',                                                                    &
! Variable type
    var_type_soil,                                                             &
! Long name
    "Gridbox frozen moisture content of each soil layer as a fraction of saturation", &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sthu
!-----------------------------------------------------------------------------
DATA metadata(210) / var_metadata(                                             &
! String identifier
    'sthu',                                                                    &
! Variable type
    var_type_soil,                                                             &
! Long name
    "Gridbox unfrozen moisture content of each soil layer as a fraction of saturation", &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for alb_tile_1
!-----------------------------------------------------------------------------
DATA metadata(211) / var_metadata(                                             &
! String identifier
    'alb_tile_1',                                                              &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile land albedo, waveband 1",                                            &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for alb_tile_2
!-----------------------------------------------------------------------------
DATA metadata(212) / var_metadata(                                             &
! String identifier
    'alb_tile_2',                                                              &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile land albedo, waveband 2",                                            &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for alb_tile_3
!-----------------------------------------------------------------------------
DATA metadata(213) / var_metadata(                                             &
! String identifier
    'alb_tile_3',                                                              &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile land albedo, waveband 3",                                            &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for alb_tile_4
!-----------------------------------------------------------------------------
DATA metadata(214) / var_metadata(                                             &
! String identifier
    'alb_tile_4',                                                              &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile land albedo, waveband 4",                                            &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for anthrop_heat
!-----------------------------------------------------------------------------
DATA metadata(215) / var_metadata(                                             &
! String identifier
    'anthrop_heat',                                                            &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Anthropogenic heat flux for each tile",                                   &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for catch
!-----------------------------------------------------------------------------
DATA metadata(216) / var_metadata(                                             &
! String identifier
    'catch',                                                                   &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile surface/canopy water capacity of snow-free land tiles",              &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for ecan
!-----------------------------------------------------------------------------
DATA metadata(217) / var_metadata(                                             &
! String identifier
    'ecan',                                                                    &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile evaporation from canopy/surface store for snow-free land tiles",     &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for ei
!-----------------------------------------------------------------------------
DATA metadata(218) / var_metadata(                                             &
! String identifier
    'ei',                                                                      &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile sublimation from lying snow for land tiles",                         &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for emis
!-----------------------------------------------------------------------------
DATA metadata(219) / var_metadata(                                             &
! String identifier
    'emis',                                                                    &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile emissivity",                                                         &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for esoil
!-----------------------------------------------------------------------------
DATA metadata(220) / var_metadata(                                             &
! String identifier
    'esoil',                                                                   &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile surface evapotranspiration from soil moisture store for snow-free land tiles", &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fqw
!-----------------------------------------------------------------------------
DATA metadata(221) / var_metadata(                                             &
! String identifier
    'fqw',                                                                     &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile surface moisture flux for land tiles",                               &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for ftl
!-----------------------------------------------------------------------------
DATA metadata(222) / var_metadata(                                             &
! String identifier
    'ftl',                                                                     &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile surface sensible heat flux for land tiles",                          &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for gc
!-----------------------------------------------------------------------------
DATA metadata(223) / var_metadata(                                             &
! String identifier
    'gc',                                                                      &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile surface conductance to evaporation for land tiles",                  &
! Units
    "m s-1"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for le
!-----------------------------------------------------------------------------
DATA metadata(224) / var_metadata(                                             &
! String identifier
    'le',                                                                      &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile surface latent heat flux for land tiles",                            &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for q1p5m
!-----------------------------------------------------------------------------
DATA metadata(225) / var_metadata(                                             &
! String identifier
    'q1p5m',                                                                   &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile specific humidity at 1.5m over land tiles",                          &
! Units
    "kg kg-1"                                                                  &
  ) /
!-----------------------------------------------------------------------------
! Metadata for rad_net_tile
!-----------------------------------------------------------------------------
DATA metadata(226) / var_metadata(                                             &
! String identifier
    'rad_net_tile',                                                            &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile surface net radiation",                                              &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sw_surft
!-----------------------------------------------------------------------------
DATA metadata(227) / var_metadata(                                             &
! String identifier
    'sw_surft',                                                                &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile surface shortwave net radiation",                                    &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lw_down_surft
!-----------------------------------------------------------------------------
DATA metadata(228) / var_metadata(                                             &
! String identifier
    'lw_down_surft',                                                           &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile surface longwave downwelling radiation",                             &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lw_up_surft
!-----------------------------------------------------------------------------
DATA metadata(229) / var_metadata(                                             &
! String identifier
    'lw_up_surft',                                                             &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile surface longwave upwelling radiation",                               &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snow_can_melt
!-----------------------------------------------------------------------------
DATA metadata(230) / var_metadata(                                             &
! String identifier
    'snow_can_melt',                                                           &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile melt of canopy snow",                                                &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snow_can
!-----------------------------------------------------------------------------
DATA metadata(231) / var_metadata(                                             &
! String identifier
    'snow_can',                                                                &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile canopy snow",                                                        &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snow_grnd_rho
!-----------------------------------------------------------------------------
DATA metadata(232) / var_metadata(                                             &
! String identifier
    'snow_grnd_rho',                                                           &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile density of snow on ground",                                          &
! Units
    "kg m-3"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snow_ground
!-----------------------------------------------------------------------------
DATA metadata(233) / var_metadata(                                             &
! String identifier
    'snow_ground',                                                             &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile snow on ground (snow_tile or snow_grnd)",                            &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snow_ice_tile
!-----------------------------------------------------------------------------
DATA metadata(234) / var_metadata(                                             &
! String identifier
    'snow_ice_tile',                                                           &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile total ice content in snow on ground",                                &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snow_liq_tile
!-----------------------------------------------------------------------------
DATA metadata(235) / var_metadata(                                             &
! String identifier
    'snow_liq_tile',                                                           &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile total liquid content in snow on ground",                             &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snow_mass
!-----------------------------------------------------------------------------
DATA metadata(236) / var_metadata(                                             &
! String identifier
    'snow_mass',                                                               &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile lying snow (snow_tile+snow_grnd)",                                   &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snow_melt
!-----------------------------------------------------------------------------
DATA metadata(237) / var_metadata(                                             &
! String identifier
    'snow_melt',                                                               &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile snow melt (melt_tile)",                                              &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for surf_ht_flux
!-----------------------------------------------------------------------------
DATA metadata(238) / var_metadata(                                             &
! String identifier
    'surf_ht_flux',                                                            &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Downward heat flux for each tile",                                        &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snow_soil_htf
!-----------------------------------------------------------------------------
DATA metadata(239) / var_metadata(                                             &
! String identifier
    'snow_soil_htf',                                                           &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Downward heat flux after snowpack to subsurface",                         &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snice_smb_surft
!-----------------------------------------------------------------------------
DATA metadata(240) / var_metadata(                                             &
! String identifier
    'snice_smb_surft',                                                         &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Rate of change of snowpack mass on tiles",                                &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snice_m_surft
!-----------------------------------------------------------------------------
DATA metadata(241) / var_metadata(                                             &
! String identifier
    'snice_m_surft',                                                           &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Total internal melt rate of snowpack on tiles",                           &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snice_freez_surft
!-----------------------------------------------------------------------------
DATA metadata(242) / var_metadata(                                             &
! String identifier
    'snice_freez_surft',                                                       &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Total internal refreezing rate in snowpack on tiles",                     &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snice_runoff_surft
!-----------------------------------------------------------------------------
DATA metadata(243) / var_metadata(                                             &
! String identifier
    'snice_runoff_surft',                                                      &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Net rate of liquid leaving snowpack on tiles",                            &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snice_sicerate_surft
!-----------------------------------------------------------------------------
DATA metadata(244) / var_metadata(                                             &
! String identifier
    'snice_sicerate_surft',                                                    &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Rate of change of solid mass in snowpack on tiles",                       &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for snice_sliqrate_surft
!-----------------------------------------------------------------------------
DATA metadata(245) / var_metadata(                                             &
! String identifier
    'snice_sliqrate_surft',                                                    &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Rate of change of liquid mass in snowpack on tiles",                      &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for surf_ht_store
!-----------------------------------------------------------------------------
DATA metadata(246) / var_metadata(                                             &
! String identifier
    'surf_ht_store',                                                           &
! Variable type
    var_type_surft,                                                            &
! Long name
    "C*(dT/dt) for each tile",                                                 &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for t1p5m
!-----------------------------------------------------------------------------
DATA metadata(247) / var_metadata(                                             &
! String identifier
    't1p5m',                                                                   &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile temperature at 1.5m over land tiles",                                &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for tstar
!-----------------------------------------------------------------------------
DATA metadata(248) / var_metadata(                                             &
! String identifier
    'tstar',                                                                   &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile surface temperature",                                                &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for tsurf_elev_surft
!-----------------------------------------------------------------------------
DATA metadata(249) / var_metadata(                                             &
! String identifier
    'tsurf_elev_surft',                                                        &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Temperature of elevated subsurface tiles",                                &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for z0
!-----------------------------------------------------------------------------
DATA metadata(250) / var_metadata(                                             &
! String identifier
    'z0',                                                                      &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile surface roughness",                                                  &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for z0h
!-----------------------------------------------------------------------------
DATA metadata(251) / var_metadata(                                             &
! String identifier
    'z0h',                                                                     &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile surface thermal roughness",                                          &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for tile_index
!-----------------------------------------------------------------------------
DATA metadata(252) / var_metadata(                                             &
! String identifier
    'tile_index',                                                              &
! Variable type
    var_type_type,                                                             &
! Long name
    "Index (gridbox number) of land points with each surface type",            &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for co2_mmr
!-----------------------------------------------------------------------------
DATA metadata(253) / var_metadata(                                             &
! String identifier
    'co2_mmr',                                                                 &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Concentration of atmospheric CO2, expressed as a mass mixing ratio.",     &
! Units
    ""                                                                         &
  ) /
!-----------------------------------------------------------------------------
! Metadata for ozone
!-----------------------------------------------------------------------------
DATA metadata(254) / var_metadata(                                             &
! String identifier
    'ozone',                                                                   &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for isoprene_gb
!-----------------------------------------------------------------------------
DATA metadata(255) / var_metadata(                                             &
! String identifier
    'isoprene_gb',                                                             &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean isoprene emission flux",                                     &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for isoprene
!-----------------------------------------------------------------------------
DATA metadata(256) / var_metadata(                                             &
! String identifier
    'isoprene',                                                                &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT isoprene emission flux",                                              &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for terpene_gb
!-----------------------------------------------------------------------------
DATA metadata(257) / var_metadata(                                             &
! String identifier
    'terpene_gb',                                                              &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean (mono-)terpene emission flux",                               &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for terpene
!-----------------------------------------------------------------------------
DATA metadata(258) / var_metadata(                                             &
! String identifier
    'terpene',                                                                 &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT (mono-)terpene emission flux",                                        &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for methanol_gb
!-----------------------------------------------------------------------------
DATA metadata(259) / var_metadata(                                             &
! String identifier
    'methanol_gb',                                                             &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean methanol emission flux",                                     &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for methanol
!-----------------------------------------------------------------------------
DATA metadata(260) / var_metadata(                                             &
! String identifier
    'methanol',                                                                &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT methanol emission flux",                                              &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for acetone_gb
!-----------------------------------------------------------------------------
DATA metadata(261) / var_metadata(                                             &
! String identifier
    'acetone_gb',                                                              &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean acetone emission flux",                                      &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for acetone
!-----------------------------------------------------------------------------
DATA metadata(262) / var_metadata(                                             &
! String identifier
    'acetone',                                                                 &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT acetone emission flux",                                               &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for croprootc
!-----------------------------------------------------------------------------
DATA metadata(263) / var_metadata(                                             &
! String identifier
    'croprootc',                                                               &
! Variable type
    var_type_cpft,                                                             &
! Long name
    "Root carbon of crop at the end of model timestep",                        &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cropharvc
!-----------------------------------------------------------------------------
DATA metadata(264) / var_metadata(                                             &
! String identifier
    'cropharvc',                                                               &
! Variable type
    var_type_cpft,                                                             &
! Long name
    "Carbon in harvested parts of crop at the end of model timestep",          &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cropreservec
!-----------------------------------------------------------------------------
DATA metadata(265) / var_metadata(                                             &
! String identifier
    'cropreservec',                                                            &
! Variable type
    var_type_cpft,                                                             &
! Long name
    "Carbon in crop stem reserve pool at the end of model timestep",           &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cropdvi
!-----------------------------------------------------------------------------
DATA metadata(266) / var_metadata(                                             &
! String identifier
    'cropdvi',                                                                 &
! Variable type
    var_type_cpft,                                                             &
! Long name
    "Developmental index of crops at the end of model timestep",               &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cropyield
!-----------------------------------------------------------------------------
DATA metadata(267) / var_metadata(                                             &
! String identifier
    'cropyield',                                                               &
! Variable type
    var_type_cpft,                                                             &
! Long name
    "Yield carbon at the end of model timestep",                               &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for harvest_trigger
!-----------------------------------------------------------------------------
DATA metadata(268) / var_metadata(                                             &
! String identifier
    'harvest_trigger',                                                         &
! Variable type
    var_type_cpft,                                                             &
! Long name
    "Trigger condition for crop harvest",                                      &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for harvest_counter
!-----------------------------------------------------------------------------
DATA metadata(269) / var_metadata(                                             &
! String identifier
    'harvest_counter',                                                         &
! Variable type
    var_type_cpft,                                                             &
! Long name
    "Counter for crop harvest",                                                &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cropstemc
!-----------------------------------------------------------------------------
DATA metadata(270) / var_metadata(                                             &
! String identifier
    'cropstemc',                                                               &
! Variable type
    var_type_cpft,                                                             &
! Long name
    "Carbon in stem parts of crop at the end of model timestep",               &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cropleafc
!-----------------------------------------------------------------------------
DATA metadata(271) / var_metadata(                                             &
! String identifier
    'cropleafc',                                                               &
! Variable type
    var_type_cpft,                                                             &
! Long name
    "Carbon in leaf parts of crop at the end of model timestep",               &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for croplai
!-----------------------------------------------------------------------------
DATA metadata(272) / var_metadata(                                             &
! String identifier
    'croplai',                                                                 &
! Variable type
    var_type_cpft,                                                             &
! Long name
    "Leaf area index of crop at the end of model timestep",                    &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cropcanht
!-----------------------------------------------------------------------------
DATA metadata(273) / var_metadata(                                             &
! String identifier
    'cropcanht',                                                               &
! Variable type
    var_type_cpft,                                                             &
! Long name
    "Canopy height of crop at the end of model timestep",                      &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cropsowdate
!-----------------------------------------------------------------------------
DATA metadata(274) / var_metadata(                                             &
! String identifier
    'cropsowdate',                                                             &
! Variable type
    var_type_cpft,                                                             &
! Long name
    "Day of year when crop is sown",                                           &
! Units
    "day of year"                                                              &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cropttveg
!-----------------------------------------------------------------------------
DATA metadata(275) / var_metadata(                                             &
! String identifier
    'cropttveg',                                                               &
! Variable type
    var_type_cpft,                                                             &
! Long name
    "Thermal time between emergence and flowering",                            &
! Units
    "degree days"                                                              &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cropttrep
!-----------------------------------------------------------------------------
DATA metadata(276) / var_metadata(                                             &
! String identifier
    'cropttrep',                                                               &
! Variable type
    var_type_cpft,                                                             &
! Long name
    "Thermal time between flowering and maturity/harvest",                     &
! Units
    "degree days"                                                              &
  ) /
!-----------------------------------------------------------------------------
! Neutral winds
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
! Metadata for u10m_n
!-----------------------------------------------------------------------------
DATA metadata(277) / var_metadata(                                             &
! String identifier
    'u10m_n',                                                                  &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox equivalent neutral westerly wind component at 10 m height",       &
! Units
    "m s-1"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for v10m_n
!-----------------------------------------------------------------------------
DATA metadata(278) / var_metadata(                                             &
! String identifier
    'v10m_n',                                                                  &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox equivalent neutral southerly wind component at 10 m height",      &
! Units
    "m s-1"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for mu10m_n
!-----------------------------------------------------------------------------
DATA metadata(279) / var_metadata(                                             &
! String identifier
    'mu10m_n',                                                                 &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox westerly pseudostress referenced to 10 m height",                 &
! Units
    "m2 s-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for mv10m_n
!-----------------------------------------------------------------------------
DATA metadata(280) / var_metadata(                                             &
! String identifier
    'mv10m_n',                                                                 &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox southerly pseudostress referenced to 10 m height",                &
! Units
    "m2 s-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sthu_irr
!-----------------------------------------------------------------------------
DATA metadata(281) / var_metadata(                                             &
! String identifier
    'sthu_irr',                                                                &
! Variable type
    var_type_soil,                                                             &
! Long name
    "Gridbox wetness of each soil layer over irrigation",                      &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for frac_irrig
!-----------------------------------------------------------------------------
DATA metadata(282) / var_metadata(                                             &
! String identifier
    'frac_irrig',                                                              &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Irrigated fraction",                                                      &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for irrDaysDiag
!-----------------------------------------------------------------------------
DATA metadata(283) / var_metadata(                                             &
! String identifier
    'irrDaysDiag',                                                             &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Number of days on which irrigation is applied",                           &
! Units
    "days"                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for irrig_water
!-----------------------------------------------------------------------------
DATA metadata(284) / var_metadata(                                             &
! String identifier
    'irrig_water',                                                             &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Water applied as irrigation",                                             &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for frac_agr
!-----------------------------------------------------------------------------
DATA metadata(285) / var_metadata(                                             &
! String identifier
    'frac_agr',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox agricultural/crop fraction",                                      &
! Units
    "-"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for frac_agr_prev
!-----------------------------------------------------------------------------
DATA metadata(286) / var_metadata(                                             &
! String identifier
    'frac_agr_prev',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox agricultural/crop fraction from previous land use update",        &
! Units
    "-"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for frac_past
!-----------------------------------------------------------------------------
DATA metadata(287) / var_metadata(                                             &
! String identifier
    'frac_past',                                                               &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox pasture fraction",                                                &
! Units
    "-"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for frac_past_prev
!-----------------------------------------------------------------------------
DATA metadata(288) / var_metadata(                                             &
! String identifier
    'frac_past_prev',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox pasture fraction from previous land use update",                  &
! Units
    "-"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for Wood_prod_fast
!-----------------------------------------------------------------------------
DATA metadata(289) / var_metadata(                                             &
! String identifier
    'wood_prod_fast',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Wood Products Fast C Pool",                                               &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for Wood_prod_med
!-----------------------------------------------------------------------------
DATA metadata(290) / var_metadata(                                             &
! String identifier
    'wood_prod_med',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Wood Products Medium C Pool",                                             &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for Wood_prod_slow
!-----------------------------------------------------------------------------
DATA metadata(291) / var_metadata(                                             &
! String identifier
    'wood_prod_slow',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Wood Products Slow C Pool",                                               &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for WP_fast_in
!-----------------------------------------------------------------------------
DATA metadata(292) / var_metadata(                                             &
! String identifier
    'WP_fast_in',                                                              &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Wood Products Fast C Pool Input",                                         &
! Units
    "kg m-2 per 360 days"                                                      &
  ) /
!-----------------------------------------------------------------------------
! Metadata for WP_med_in
!-----------------------------------------------------------------------------
DATA metadata(293) / var_metadata(                                             &
! String identifier
    'WP_med_in',                                                               &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Wood Products Medium C Pool Input",                                       &
! Units
    "kg m-2 per 360 days"                                                      &
  ) /

!-----------------------------------------------------------------------------
! Metadata for WP_slow_in
!-----------------------------------------------------------------------------
DATA metadata(294) / var_metadata(                                             &
! String identifier
    'WP_slow_in',                                                              &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Wood Products Slow C Pool Input",                                         &
! Units
    "kg m-2 per 360 days"                                                      &
  ) /

!-----------------------------------------------------------------------------
! Metadata for WP_fast_out
!-----------------------------------------------------------------------------
DATA metadata(295) / var_metadata(                                             &
! String identifier
    'WP_fast_out',                                                             &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Wood Products Fast C Pool Output",                                        &
! Units
    "kg m-2 per 360 days"                                                      &
  ) /
!-----------------------------------------------------------------------------
! Metadata for WP_med_out
!-----------------------------------------------------------------------------
DATA metadata(296) / var_metadata(                                             &
! String identifier
    'WP_med_out',                                                              &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Wood Products Medium C Pool Output",                                      &
! Units
    "kg m-2 per 360 days"                                                      &
  ) /
!-----------------------------------------------------------------------------
! Metadata for WP_slow_out
!-----------------------------------------------------------------------------
DATA metadata(297) / var_metadata(                                             &
! String identifier
    'WP_slow_out',                                                             &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Wood Products Slow C Pool Output",                                        &
! Units
    "kg m-2 per 360 days"                                                      &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cnsrv_carbon_veg2
!-----------------------------------------------------------------------------
DATA metadata(298) / var_metadata(                                             &
! String identifier
    'cnsrv_carbon_veg2',                                                       &
! Variable type
    var_type_surface,                                                          &
! Long name
    "error in land carbon conservation in veg2 routine",                       &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cnsrv_carbon_triffid
!-----------------------------------------------------------------------------
DATA metadata(299) / var_metadata(                                             &
! String identifier
    'cnsrv_carbon_triffid',                                                    &
! Variable type
    var_type_surface,                                                          &
! Long name
    "error in land carbon conservation in triffid routine",                    &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cnsrv_veg_triffid
!-----------------------------------------------------------------------------
DATA metadata(300) / var_metadata(                                             &
! String identifier
    'cnsrv_veg_triffid',                                                       &
! Variable type
    var_type_surface,                                                          &
! Long name
    "error in vegetation carbon conservation in triffid routine",              &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cnsrv_soil_triffid
!-----------------------------------------------------------------------------
DATA metadata(301) / var_metadata(                                             &
! String identifier
    'cnsrv_soil_triffid',                                                      &
! Variable type
    var_type_surface,                                                          &
! Long name
    "error in soil carbon conservation in triffid routine",                    &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cnsrv_prod_triffid
!-----------------------------------------------------------------------------
DATA metadata(302) / var_metadata(                                             &
! String identifier
    'cnsrv_prod_triffid',                                                      &
! Variable type
    var_type_surface,                                                          &
! Long name
    "error in wood product carbon conservation in triffid routine",            &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lit_c_orig
!-----------------------------------------------------------------------------
DATA metadata(303) / var_metadata(                                             &
! String identifier
    'lit_c_orig',                                                              &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT carbon Litter including LU and fire",                                 &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lit_n_orig
!-----------------------------------------------------------------------------
DATA metadata(304) / var_metadata(                                             &
! String identifier
    'lit_n_orig',                                                              &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT nitrogen Litter including LU and fire",                               &
! Units
    "kg N  m-2 per 360days"                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_inorg
!-----------------------------------------------------------------------------
DATA metadata(305) / var_metadata(                                             &
! String identifier
    'n_inorg',                                                                 &
! Variable type
    var_type_sclayer,                                                          &
! Long name
    "Inorganic N Pool",                                                        &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_loss
!-----------------------------------------------------------------------------
DATA metadata(306) / var_metadata(                                             &
! String identifier
    'n_loss',                                                                  &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Nitrogen loss term (fixed fraction of N_INORG)",                          &
! Units
    "kg m-2 360 days"                                                          &
  ) /
!-----------------------------------------------------------------------------
! Metadata for dpm_ratio
!-----------------------------------------------------------------------------
DATA metadata(307) / var_metadata(                                             &
! String identifier
    'dpm_ratio',                                                               &
! Variable type
    var_type_surface,                                                          &
! Long name
    "DPM:RPM ratio of overall litter input (one value per GB)",                &
! Units
    "fraction (limited to between 0 and 1)"                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lit_c_ag
!-----------------------------------------------------------------------------
DATA metadata(308) / var_metadata(                                             &
! String identifier
    'lit_c_ag',                                                                &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT carbon Litter from LU/Agriculture",                                   &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for root_abandon
!-----------------------------------------------------------------------------
DATA metadata(309) / var_metadata(                                             &
! String identifier
    'root_abandon',                                                            &
! Variable type
    var_type_pft,                                                              &
! Long name
    "carbon flux from roots abandoned during landuse change to soil",          &
! Units
    "kg C  m-2 per 360days"                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for root_abandon_n
!-----------------------------------------------------------------------------
DATA metadata(310) / var_metadata(                                             &
! String identifier
    'root_abandon_n',                                                          &
! Variable type
    var_type_pft,                                                              &
! Long name
    "nitrogen flux from roots abandoned during landuse change to soil",        &
! Units
    "kg N  m-2 per 360days"                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for harvest
!-----------------------------------------------------------------------------
DATA metadata(311) / var_metadata(                                             &
! String identifier
    'harvest',                                                                 &
! Variable type
    var_type_pft,                                                              &
! Long name
    "flux of carbon to product pools due to harvest",                          &
! Units
    "kg C  m-2 per 360days"                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for harvest_gb
!-----------------------------------------------------------------------------
DATA metadata(312) / var_metadata(                                             &
! String identifier
    'harvest_gb',                                                              &
! Variable type
    var_type_surface,                                                          &
! Long name
    "gridbox mean flux of carbon to product pools due to harvest",             &
! Units
    "kg C  m-2 per 360days"                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for harvest_n
!-----------------------------------------------------------------------------
DATA metadata(313) / var_metadata(                                             &
! String identifier
    'harvest_n',                                                               &
! Variable type
    var_type_pft,                                                              &
! Long name
    "flux of nitrogen to atmosphere due to harvest",                           &
! Units
    "kg N  m-2 per 360days"                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for harvest_n_gb
!-----------------------------------------------------------------------------
DATA metadata(314) / var_metadata(                                             &
! String identifier
    'harvest_n_gb',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "gridbox mean flux of nitrogen to product pools due to harvest",           &
! Units
    "kg N  m-2 per 360days"                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_fertiliser
!-----------------------------------------------------------------------------
DATA metadata(315) / var_metadata(                                             &
! String identifier
    'n_fertiliser',                                                            &
! Variable type
    var_type_pft,                                                              &
! Long name
    "N addition from fertiliser",                                              &
! Units
    "kg N  m-2 per 360days"                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_fertiliser_gb
!-----------------------------------------------------------------------------
DATA metadata(316) / var_metadata(                                             &
! String identifier
    'n_fertiliser_gb',                                                         &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox N addition from fertiliser",                                      &
! Units
    "kg N  m-2 per 360days"                                                    &
  ) /
 !-----------------------------------------------------------------------------
! Metadata for lit_n_ag
!-----------------------------------------------------------------------------
DATA metadata(317) / var_metadata(                                             &
! String identifier
    'lit_n_ag',                                                                &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT nitrogen loss due to LU/Agriculture",                                 &
! Units
    "kg N  m-2 per 360days"                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river routing latitude (2d)
!-----------------------------------------------------------------------------
DATA metadata(318) / var_metadata(                                             &
! String identifier
    'latitude_2d',                                                             &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Routing gridbox latitude",                                                &
! Units
    "degrees"                                                                  &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river routing longitude (2d)
!-----------------------------------------------------------------------------
DATA metadata(319) / var_metadata(                                             &
! String identifier
    'longitude_2d',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Routing gridbox latitude",                                                &
! Units
    "degrees"                                                                  &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river direction
!-----------------------------------------------------------------------------
DATA metadata(320) / var_metadata(                                             &
! String identifier
    'direction',                                                               &
! Variable type
    var_type_surface,                                                          &
! Long name
    "River routing direction flag",                                            &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river sequence
!-----------------------------------------------------------------------------
DATA metadata(321) / var_metadata(                                             &
! String identifier
    'sequence',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "River routing sequence flag",                                             &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river drainage area
!-----------------------------------------------------------------------------
DATA metadata(322) / var_metadata(                                             &
! String identifier
    'area',                                                                    &
! Variable type
    var_type_surface,                                                          &
! Long name
    "River routing drainage area",                                             &
! Units
    "number of grid cells"                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river routing latitude (1d)
!-----------------------------------------------------------------------------
DATA metadata(323) / var_metadata(                                             &
! String identifier
    'rivers_ygrid',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Routing gridbox latitude",                                                &
! Units
    "degrees"                                                                  &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river routing longitude (1d)
!-----------------------------------------------------------------------------
DATA metadata(324) / var_metadata(                                             &
! String identifier
    'rivers_xgrid',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Routing gridbox longitude",                                               &
! Units
    "degrees"                                                                  &
  ) /
!-----------------------------------------------------------------------------
! Metadata for TRIP river storage on river routing grid
!-----------------------------------------------------------------------------
DATA metadata(325) / var_metadata(                                             &
! String identifier
    'rivers_sto_rp',                                                           &
! Variable type
    var_type_rp,                                                               &
! Long name
    "River routing gridbox river storage on river points",                     &
! Units
    "kg"                                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river flow
!-----------------------------------------------------------------------------
DATA metadata(326) / var_metadata(                                             &
! String identifier
    'rflow',                                                                   &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox river flow rate",                                                 &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river routing runoff on model grid
!-----------------------------------------------------------------------------
DATA metadata(327) / var_metadata(                                             &
! String identifier
    'rrun',                                                                    &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox runoff rate received by river routing routine",                   &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for Metstat temp_max_00h_r
!-----------------------------------------------------------------------------
DATA metadata(328) / var_metadata(                                             &
! String identifier
    'temp_max_00h_r',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Maximum temperature to 00hrs local- running total",                       &
! Units
    "Kelvin"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for Metstat temp_ave_00h_r
!-----------------------------------------------------------------------------
DATA metadata(329) / var_metadata(                                             &
! String identifier
    'temp_ave_00h_r',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Average temperature to 00hrs local- running total",                       &
! Units
    "Kelvin"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for Metstat prec_tot_00h_r
!-----------------------------------------------------------------------------
DATA metadata(330) / var_metadata(                                             &
! String identifier
    'prec_tot_00h_r',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Total precipitation to 00hrs local- running total",                       &
! Units
    "kg/ms2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for Metstat prec_tot_12h_r
!-----------------------------------------------------------------------------
DATA metadata(331) / var_metadata(                                             &
! String identifier
    'prec_tot_12h_r',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Total precipitation to 12hrs local- running total",                       &
! Units
    "kg/m2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for Metstat rhum_min_00h_r
!-----------------------------------------------------------------------------
DATA metadata(332) / var_metadata(                                             &
! String identifier
    'rhum_min_00h_r',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Minimum relative humidity to 00hrs local- running total",                 &
! Units
    "%"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for Metstat dewp_ave_00h_r
!-----------------------------------------------------------------------------
DATA metadata(333) / var_metadata(                                             &
! String identifier
    'dewp_ave_00h_r',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Average dewpoint to 00 hrs local- running total",                         &
! Units
    "Kelvin"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for Metstat wind_ave_00h_r
!-----------------------------------------------------------------------------
DATA metadata(334) / var_metadata(                                             &
! String identifier
    'wind_ave_00h_r',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Average wind speed to 00hrs local- running total",                        &
! Units
    "m/s"                                                                      &
  ) /
!-----------------------------------------------------------------------------
! Metadata for Metstat temp_max_00h
!-----------------------------------------------------------------------------
DATA metadata(335) / var_metadata(                                             &
! String identifier
    'temp_max_00h',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Maximum temperature to 00hrs local",                                      &
! Units
    "Kelvin"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for Metstat temp_ave_00h
!-----------------------------------------------------------------------------
DATA metadata(336) / var_metadata(                                             &
! String identifier
    'temp_ave_00h',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Average temperature to 00hrs local",                                      &
! Units
    "Kelvin"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for Metstat temp_pnt_12h
!-----------------------------------------------------------------------------
DATA metadata(337) / var_metadata(                                             &
! String identifier
    'temp_pnt_12h',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Timestep temperature at last 12hrs local",                                &
! Units
    "Kelvin"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for Metstat prec_tot_00h
!-----------------------------------------------------------------------------
DATA metadata(338) / var_metadata(                                             &
! String identifier
    'prec_tot_00h',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Total precipitation to 00hrs local",                                      &
! Units
    "kg/ms2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for Metstat prec_tot_12h
!-----------------------------------------------------------------------------
DATA metadata(339) / var_metadata(                                             &
! String identifier
    'prec_tot_12h',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Total precipitation to 12hrs local",                                      &
! Units
    "kg/m2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for Metstat rhum_min_00h
!-----------------------------------------------------------------------------
DATA metadata(340) / var_metadata(                                             &
! String identifier
    'rhum_min_00h',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Minimum relative humidity to 00hrs local",                                &
! Units
    "%"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for Metstat rhum_pnt_12h
!-----------------------------------------------------------------------------
DATA metadata(341) / var_metadata(                                             &
! String identifier
    'rhum_pnt_12h',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Timestep relative humidity at last 12hrs local",                          &
! Units
    "%"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for Metstat dewp_ave_00h
!-----------------------------------------------------------------------------
DATA metadata(342) / var_metadata(                                             &
! String identifier
    'dewp_ave_00h',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Average dewpoint to 00 hrs local",                                        &
! Units
    "Kelvin"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for Metstat wind_ave_00h
!-----------------------------------------------------------------------------
DATA metadata(343) / var_metadata(                                             &
! String identifier
    'wind_ave_00h',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Average wind speed to 00hrs local",                                       &
! Units
    "m/s"                                                                      &
  ) /
!-----------------------------------------------------------------------------
! Metadata for Metstat wind_pnt_12h
!-----------------------------------------------------------------------------
DATA metadata(344) / var_metadata(                                             &
! String identifier
    'wind_pnt_12h',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Timestep wind speed at last 12hrs local",                                 &
! Units
    "m/s"                                                                      &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_mcarthur
!-----------------------------------------------------------------------------
DATA metadata(345) / var_metadata(                                             &
! String identifier
    'fire_mcarthur',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "McArthur FFDI",                                                           &
! Units
    "None"                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_mcarthur_r_dr
!-----------------------------------------------------------------------------
DATA metadata(346) / var_metadata(                                             &
! String identifier
    'fire_mcarthur_r_dr',                                                      &
! Variable type
    var_type_surface,                                                          &
! Long name
    "McArthur FFDI- Drought R term",                                           &
! Units
    "None"                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_mcarthur_n_dr
!-----------------------------------------------------------------------------
DATA metadata(347) / var_metadata(                                             &
! String identifier
    'fire_mcarthur_n_dr',                                                      &
! Variable type
    var_type_surface,                                                          &
! Long name
    "McArthur FFDI- Drought N term",                                           &
! Units
     "None"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_canadian
!-----------------------------------------------------------------------------
DATA metadata(348) / var_metadata(                                             &
! String identifier
    'fire_canadian',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Canadian FWI",                                                            &
! Units
      "None"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_canadian_ffmc
!-----------------------------------------------------------------------------
DATA metadata(349) / var_metadata(                                             &
! String identifier
    'fire_canadian_ffmc',                                                      &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Canadian FWI- FFMC",                                                      &
! Units
    "None"                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_canadian_ffmc_mois
!-----------------------------------------------------------------------------
DATA metadata(350) / var_metadata(                                             &
! String identifier
    'fire_canadian_ffmc_mois',                                                 &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Canadian FWI- FFMC Moisture",                                             &
! Units
    "None"                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_canadian_dmc
!-----------------------------------------------------------------------------
DATA metadata(351) / var_metadata(                                             &
! String identifier
    'fire_canadian_dmc',                                                       &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Canadian FWI- DMC",                                                       &
! Units
    "None"                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_canadian_dc
!-----------------------------------------------------------------------------
DATA metadata(352) / var_metadata(                                             &
! String identifier
    'fire_canadian_dc',                                                        &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Canadian FWI- DC",                                                        &
! Units
    "None"                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_canadian_isi
!-----------------------------------------------------------------------------
DATA metadata(353) / var_metadata(                                             &
! String identifier
    'fire_canadian_isi',                                                       &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Canadian FWI- ISI",                                                       &
! Units
    "None"                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_canadian_bui
!-----------------------------------------------------------------------------
DATA metadata(354) / var_metadata(                                             &
! String identifier
    'fire_canadian_bui',                                                       &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Canadian FWI- BUI",                                                       &
! Units
    "None"                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_nesterov
!-----------------------------------------------------------------------------
DATA metadata(355) / var_metadata(                                             &
! String identifier
    'fire_nesterov',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Nesterov Index",                                                          &
! Units
    "None"                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for leafC
!-----------------------------------------------------------------------------
DATA metadata(356) / var_metadata(                                             &
! String identifier
    'leafC',                                                                   &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Carbon in LEAF biomass on PFTs",                                          &
! Units
    "kg/m2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for rootC
!-----------------------------------------------------------------------------
DATA metadata(357) / var_metadata(                                             &
! String identifier
    'rootC',                                                                   &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Carbon in ROOT biomass on PFTs",                                          &
! Units
    "kg/m2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for stemC
!-----------------------------------------------------------------------------
DATA metadata(358) / var_metadata(                                             &
! String identifier
    'stemC',                                                                   &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Carbon in STEM biomass on PFTs",                                          &
! Units
    "kg/m2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for woodC
!-----------------------------------------------------------------------------
DATA metadata(359) / var_metadata(                                             &
! String identifier
    'woodC',                                                                   &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Carbon in WOODY biomass on PFTs",                                         &
! Units
    "kg/m2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for dleaf
!-----------------------------------------------------------------------------
DATA metadata(360) / var_metadata(                                             &
! String identifier
    'dleaf',                                                                   &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Increment to carbon in LEAF biomass on PFTs",                             &
! Units
    "kg/m2/TRIFFID PERIOD"                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for droot
!-----------------------------------------------------------------------------
DATA metadata(361) / var_metadata(                                             &
! String identifier
    'droot',                                                                   &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Increment to carbon in ROOT biomass on PFTs",                             &
! Units
    "kg/m2/TRIFFID PERIOD"                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for dwood
!-----------------------------------------------------------------------------
DATA metadata(362) / var_metadata(                                             &
! String identifier
    'dwood',                                                                   &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Increment to carbon in WOODY biomass on PFTs",                            &
! Units
    "kg/m2/TRIFFID PERIOD"                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for leaf_litC
!-----------------------------------------------------------------------------
DATA metadata(363) / var_metadata(                                             &
! String identifier
    'leaf_litC',                                                               &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Litter CARBON due to leaf turnover",                                      &
! Units
    "kg/m2/360 days"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for root_litC
!-----------------------------------------------------------------------------
DATA metadata(364) / var_metadata(                                             &
! String identifier
    'root_litC',                                                               &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Litter CARBON due to root turnover",                                      &
! Units
    "kg/m2/360 days"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for wood_litC
!-----------------------------------------------------------------------------
DATA metadata(365) / var_metadata(                                             &
! String identifier
    'wood_litC',                                                               &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Litter CARBON due to wood turnover",                                      &
! Units
    "kg/m2/360 days"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for leaf_litN
!-----------------------------------------------------------------------------
DATA metadata(366) / var_metadata(                                             &
! String identifier
    'leaf_litN',                                                               &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Litter NITROGEN due to leaf turnover",                                    &
! Units
    "kg/m2/360 days"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for exudates
!-----------------------------------------------------------------------------
DATA metadata(367) / var_metadata(                                             &
! String identifier
    'exudates',                                                                &
! Variable type
    var_type_pft,                                                              &
! Long name
    "EXUDATES on PFTs: excess C not assimilable into plant due lack of N availability", &
! Units
    "kg/m2/360 days"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for exudates_gb
!-----------------------------------------------------------------------------
DATA metadata(368) / var_metadata(                                             &
! String identifier
    'exudates_gb',                                                             &
! Variable type
    var_type_surface,                                                          &
! Long name
    "EXUDATES (GBM): excess C not assimilable into plant due lack of N availability", &
! Units
    "kg/m2/360 days"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for npp after N limitation
!-----------------------------------------------------------------------------
DATA metadata(369) / var_metadata(                                             &
! String identifier
    'npp_n',                                                                   &
! Variable type
    var_type_pft,                                                              &
! Long name
    "NPP on PFTs post N-limitation",                                           &
! Units
    "kg/m2/360 days"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for npp_n_gb
!-----------------------------------------------------------------------------
DATA metadata(370) / var_metadata(                                             &
! String identifier
    'npp_n_gb',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "NPP (GBM) post N-limitation",                                             &
! Units
    "kg/m2/360 days"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lai_bal
!-----------------------------------------------------------------------------
DATA metadata(371) / var_metadata(                                             &
! String identifier
    'lai_bal',                                                                 &
! Variable type
    var_type_pft,                                                              &
! Long name
    "LAI in balanced growth state (seasonal maximum LAI)",                     &
! Units
    "m2/m2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for litterN
!-----------------------------------------------------------------------------
DATA metadata(372) / var_metadata(                                             &
! String identifier
    'litterN',                                                                 &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Local Littter N production",                                              &
! Units
    "kg/m2/360 days"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_uptake_growth
!-----------------------------------------------------------------------------
DATA metadata(373) / var_metadata(                                             &
! String identifier
    'n_uptake_growth',                                                         &
! Variable type
    var_type_pft,                                                              &
! Long name
    "N TAKEN UP FOR GROWTH of existing plant biomass",                         &
! Units
    "kg/m2/360 days"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_demand_growth
!-----------------------------------------------------------------------------
DATA metadata(374) / var_metadata(                                             &
! String identifier
    'n_demand_growth',                                                         &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Demand for N for GROWTH of existing plant biomass",                       &
! Units
    "kg/m2/360 days"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_gas
!-----------------------------------------------------------------------------
DATA metadata(375) / var_metadata(                                             &
! String identifier
    'n_gas',                                                                   &
! Variable type
    var_type_sclayer,                                                          &
! Long name
    "Mineralised N Gas Emissions",                                             &
! Units
    "kg/m2/360 days"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_leach
!-----------------------------------------------------------------------------
DATA metadata(376) / var_metadata(                                             &
! String identifier
    'n_leach',                                                                 &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Leached N Losses",                                                        &
! Units
    "kg/m2/s"                                                                  &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_fix_gb
!-----------------------------------------------------------------------------
DATA metadata(377) / var_metadata(                                             &
! String identifier
    'n_fix_gb',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Fixed N",                                                                 &
! Units
    "kg/m2/360 days"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_fix
!-----------------------------------------------------------------------------
DATA metadata(378) / var_metadata(                                             &
! String identifier
    'n_fix',                                                                   &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Fixed N on PFTs",                                                         &
! Units
    "kg/m2/360 days"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_demand_spread
!-----------------------------------------------------------------------------
DATA metadata(379) / var_metadata(                                             &
! String identifier
    'n_demand_spread',                                                         &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Demand for N for SPREADING plant across gridbox",                         &
! Units
    "kg/m2/360 days"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_demand_lit
!-----------------------------------------------------------------------------
DATA metadata(380) / var_metadata(                                             &
! String identifier
    'n_demand_lit',                                                            &
! Variable type
    var_type_pft,                                                              &
! Long name
    "N demand of litter: N lost in leaf, wood, root biomass",                  &
! Units
    "kg/m2/360 days"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for deposition_n
!-----------------------------------------------------------------------------
DATA metadata(381) / var_metadata(                                             &
! String identifier
    'deposition_n',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Nitrogen deposition",                                                     &
! Units
    "kg/m2/s"                                                                  &
  ) /
!-----------------------------------------------------------------------------
! Metadata for root_litN
!-----------------------------------------------------------------------------
DATA metadata(382) / var_metadata(                                             &
! String identifier
    'root_litN',                                                               &
! Variable type
    var_type_pft,                                                              &
! Long name
    "N lost as LITTER due to ROOT turnover",                                   &
! Units
    "kg/m2/360 days"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_veg
!-----------------------------------------------------------------------------
DATA metadata(383) / var_metadata(                                             &
! String identifier
    'n_veg',                                                                   &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Plant NITROGEN content (PFTs): N_LEAF+N_ROOT+N_WOOD from C equivalents",  &
! Units
    "kg/m2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_veg_gb
!-----------------------------------------------------------------------------
DATA metadata(384) / var_metadata(                                             &
! String identifier
    'n_veg_gb',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Plant NITROGEN content (GBM): N_LEAF+N_ROOT+N_WOOD from C equivalents",   &
! Units
    "kg/m2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for dcveg
!-----------------------------------------------------------------------------
DATA metadata(385) / var_metadata(                                             &
! String identifier
    'dcveg',                                                                   &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Increment in vegetation C content (PFTs)",                                &
! Units
    "kg/m2/TRIFFID TIMESTEP"                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for dcveg_gb
!-----------------------------------------------------------------------------
DATA metadata(386) / var_metadata(                                             &
! String identifier
    'dcveg_gb',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Increment in vegetation C content (GBM)",                                 &
! Units
    "kg/m2/TRIFFID TIMESTEP"                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for dnveg
!-----------------------------------------------------------------------------
DATA metadata(387) / var_metadata(                                             &
! String identifier
    'dnveg',                                                                   &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Increment in vegetation N content (PFTs)",                                &
! Units
    "kg/m2/TRIFFID TIMESTEP"                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for dnveg_gb
!-----------------------------------------------------------------------------
DATA metadata(388) / var_metadata(                                             &
! String identifier
    'dnveg_gb',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Increment in vegetation N content (GBM)",                                 &
! Units
    "kg/m2/TRIFFID TIMESTEP"                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_demand_gb
!-----------------------------------------------------------------------------
DATA metadata(389) / var_metadata(                                             &
! String identifier
    'n_demand_gb',                                                             &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Demand for N in total (GBM)",                                             &
! Units
    "kg/m2/360 DAYS"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_demand
!-----------------------------------------------------------------------------
DATA metadata(390) / var_metadata(                                             &
! String identifier
    'n_demand',                                                                &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Demand for N in total (PFTs)",                                            &
! Units
    "kg/m2/360 DAYS"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for wood_litN
!-----------------------------------------------------------------------------
DATA metadata(391) / var_metadata(                                             &
! String identifierN
    'wood_litN',                                                               &
! Variable type
    var_type_pft,                                                              &
! Long name
    "N lost as LITTER due to WOOD turnover",                                   &
! Units
    "kg/m2/360 days"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for litterC
!-----------------------------------------------------------------------------
DATA metadata(392) / var_metadata(                                             &
! String identifier
    'litterC',                                                                 &
! Variable type
    var_type_pft,                                                              &
! Long name
    "LITTER CARBON as the sum of biomass lost through LEAF, ROOT and WOOD turnover", &
! Units
    "kg/m2/360 days"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lit_n
!-----------------------------------------------------------------------------
DATA metadata(393) / var_metadata(                                             &
! String identifier
    'lit_n',                                                                   &
! Variable type
    var_type_pft,                                                              &
! Long name
    "NITROGEN LITTER (PFTs)",                                                  &
! Units
    "kg/m2/360 days"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lit_n_t
!-----------------------------------------------------------------------------
DATA metadata(394) / var_metadata(                                             &
! String identifier
    'lit_n_t',                                                                 &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Total NITROGEN LITTER",                                                   &
! Units
    "kg/m2/360 days"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_uptake
!-----------------------------------------------------------------------------
DATA metadata(395) / var_metadata(                                             &
! String identifier
    'n_uptake',                                                                &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Total NITROGEN UPTAKE by plants (PFTs)",                                  &
! Units
    "kg/m2/360 days"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_uptake_gb
!-----------------------------------------------------------------------------
DATA metadata(396) / var_metadata(                                             &
! String identifier
    'n_uptake_gb',                                                             &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Total NITROGEN UPTAKE by plants (GBM)",                                   &
! Units
    "kg/m2/360 days"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for burnt_area_gb (INFERNO)
!-----------------------------------------------------------------------------
DATA metadata(397) / var_metadata(                                             &
! String identifier
    'burnt_area_gb',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean burnt area fraction",                                        &
! Units
    "s-1"                                                                      &
  ) /
!-----------------------------------------------------------------------------
! Metadata for burnt_area
!-----------------------------------------------------------------------------
DATA metadata(398) / var_metadata(                                             &
! String identifier
    'burnt_area',                                                              &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT burnt area fraction",                                                 &
! Units
    "s-1"                                                                      &
  ) /
!-----------------------------------------------------------------------------
! Metadata for emitted_carbon_gb
!-----------------------------------------------------------------------------
DATA metadata(399) / var_metadata(                                             &
! String identifier
    'emitted_carbon_gb',                                                       &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean emitted carbon",                                             &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for emitted_carbon
!-----------------------------------------------------------------------------
DATA metadata(400) / var_metadata(                                             &
! String identifier
    'emitted_carbon',                                                          &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT emitted carbon",                                                      &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for emitted_carbon_DPM
!-----------------------------------------------------------------------------
DATA metadata(401) / var_metadata(                                             &
! String identifier
    'emitted_carbon_DPM',                                                      &
! Variable type
    var_type_surface,                                                          &
! Long name
    "DPM emitted carbon",                                                      &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for emitted_carbon_RPM
!-----------------------------------------------------------------------------
DATA metadata(402) / var_metadata(                                             &
! String identifier
    'emitted_carbon_RPM',                                                      &
! Variable type
    var_type_surface,                                                          &
! Long name
    "RPM emitted carbon",                                                      &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_CO2_gb
!-----------------------------------------------------------------------------
DATA metadata(403) / var_metadata(                                             &
! String identifier
    'fire_em_CO2_gb',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean CO2 emission from fires",                                    &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_CO2
!-----------------------------------------------------------------------------
DATA metadata(404) / var_metadata(                                             &
! String identifier
    'fire_em_CO2',                                                             &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT CO2 emission from fires",                                             &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_CO2_DPM
!-----------------------------------------------------------------------------
DATA metadata(405) / var_metadata(                                             &
! String identifier
    'fire_em_CO2_DPM',                                                         &
! Variable type
    var_type_surface,                                                          &
! Long name
    "DPM CO2 emission from fires",                                             &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_CO2_RPM
!-----------------------------------------------------------------------------
DATA metadata(406) / var_metadata(                                             &
! String identifier
    'fire_em_CO2_RPM',                                                         &
! Variable type
    var_type_surface,                                                          &
! Long name
    "RPM CO2 emission from fires",                                             &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_CO_gb
!-----------------------------------------------------------------------------
DATA metadata(407) / var_metadata(                                             &
! String identifier
    'fire_em_CO_gb',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean CO emission from fires",                                     &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_CO
!-----------------------------------------------------------------------------
DATA metadata(408) / var_metadata(                                             &
! String identifier
    'fire_em_CO',                                                              &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT CO emission from fires",                                              &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_CO_DPM
!-----------------------------------------------------------------------------
DATA metadata(409) / var_metadata(                                             &
! String identifier
    'fire_em_CO_DPM',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "DPM CO emission from fires",                                              &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_CO_RPM
!-----------------------------------------------------------------------------
DATA metadata(410) / var_metadata(                                             &
! String identifier
    'fire_em_CO_RPM',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "RPM CO emission from fires",                                              &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_CH4_gb
!-----------------------------------------------------------------------------
DATA metadata(411) / var_metadata(                                             &
! String identifier
    'fire_em_CH4_gb',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean CH4 emission from fires",                                    &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_CH4
!-----------------------------------------------------------------------------
DATA metadata(412) / var_metadata(                                             &
! String identifier
    'fire_em_CH4',                                                             &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Gridbox mean CH4 emission from fires",                                    &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_CH4_DPM
!-----------------------------------------------------------------------------
DATA metadata(413) / var_metadata(                                             &
! String identifier
    'fire_em_CH4_DPM',                                                         &
! Variable type
    var_type_surface,                                                          &
! Long name
    "DPM CH4 emission from fires",                                             &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_CH4_RPM
!-----------------------------------------------------------------------------
DATA metadata(414) / var_metadata(                                             &
! String identifier
    'fire_em_CH4_RPM',                                                         &
! Variable type
    var_type_surface,                                                          &
! Long name
    "RPM CH4 emission from fires",                                             &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_NOx_gb
!-----------------------------------------------------------------------------
DATA metadata(415) / var_metadata(                                             &
! String identifier
    'fire_em_NOx_gb',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean NOx emission from fires",                                    &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_NOx
!-----------------------------------------------------------------------------
DATA metadata(416) / var_metadata(                                             &
! String identifier
    'fire_em_NOx',                                                             &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Gridbox mean NOx emission from fires",                                    &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_NOx_DPM
!-----------------------------------------------------------------------------
DATA metadata(417) / var_metadata(                                             &
! String identifier
    'fire_em_NOx_DPM',                                                         &
! Variable type
    var_type_surface,                                                          &
! Long name
    "DPM NOx emission from fires",                                             &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_NOx_RPM
!-----------------------------------------------------------------------------
DATA metadata(418) / var_metadata(                                             &
! String identifier
    'fire_em_NOx_RPM',                                                         &
! Variable type
    var_type_surface,                                                          &
! Long name
    "RPM NOx emission from fires",                                             &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_SO2_gb
!-----------------------------------------------------------------------------
DATA metadata(419) / var_metadata(                                             &
! String identifier
    'fire_em_SO2_gb',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean SO2 emission from fires",                                    &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_SO2
!-----------------------------------------------------------------------------
DATA metadata(420) / var_metadata(                                             &
! String identifier
    'fire_em_SO2',                                                             &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Gridbox mean SO2 emission from fires",                                    &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_SO2_DPM
!-----------------------------------------------------------------------------
DATA metadata(421) / var_metadata(                                             &
! String identifier
    'fire_em_SO2_DPM',                                                         &
! Variable type
    var_type_surface,                                                          &
! Long name
    "DPM SO2 emission from fires",                                             &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_SO2_RPM
!-----------------------------------------------------------------------------
DATA metadata(422) / var_metadata(                                             &
! String identifier
    'fire_em_SO2_RPM',                                                         &
! Variable type
    var_type_surface,                                                          &
! Long name
    "RPM SO2 emission from fires",                                             &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_OC_gb
!-----------------------------------------------------------------------------
DATA metadata(423) / var_metadata(                                             &
! String identifier
    'fire_em_OC_gb',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean OC emission from fires",                                     &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_OC
!-----------------------------------------------------------------------------
DATA metadata(424) / var_metadata(                                             &
! String identifier
    'fire_em_OC',                                                              &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT OC emission from fires",                                              &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_OC_DPM
!-----------------------------------------------------------------------------
DATA metadata(425) / var_metadata(                                             &
! String identifier
    'fire_em_OC_DPM',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "DPM OC emission from fires",                                              &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_OC_RPM
!-----------------------------------------------------------------------------
DATA metadata(426) / var_metadata(                                             &
! String identifier
    'fire_em_OC_RPM',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "RPM OC emission from fires",                                              &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_BC_gb
!-----------------------------------------------------------------------------
DATA metadata(427) / var_metadata(                                             &
! String identifier
    'fire_em_BC_gb',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean BC emission from fires",                                     &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_BC
!-----------------------------------------------------------------------------
DATA metadata(428) / var_metadata(                                             &
! String identifier
    'fire_em_BC',                                                              &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT BC emission from fires",                                              &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_BC_DPM
!-----------------------------------------------------------------------------
DATA metadata(429) / var_metadata(                                             &
! String identifier
    'fire_em_BC_DPM',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "DPM BC emission from fires",                                              &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_BC_RPM
!-----------------------------------------------------------------------------
DATA metadata(430) / var_metadata(                                             &
! String identifier
    'fire_em_BC_RPM',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "RPM BC emission from fires",                                              &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for pop_den
!-----------------------------------------------------------------------------
DATA metadata(431) / var_metadata(                                             &
! String identifier
    'pop_den',                                                                 &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Population Density",                                                      &
! Units
    "ppl/km2"                                                                  &
  ) /
!-----------------------------------------------------------------------------
! Metadata for flash_rate
!-----------------------------------------------------------------------------
DATA metadata(432) / var_metadata(                                             &
! String identifier
    'flash_rate',                                                              &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Cloud to Ground lightning flash rate",                                    &
! Units
    "flashes/km2"                                                              &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lit_c_fire
!-----------------------------------------------------------------------------
DATA metadata(433) / var_metadata(                                             &
! String identifier
    'lit_c_fire',                                                              &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT carbon litter due to fire",                                           &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for burnt_carbon_dpm
!-----------------------------------------------------------------------------
DATA metadata(434) / var_metadata(                                             &
! String identifier
    'burnt_carbon_dpm',                                                        &
! Variable type
    var_type_surface,                                                          &
! Long name
    "DPM burnt carbon due to fire",                                            &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lit_n_fire
!-----------------------------------------------------------------------------
DATA metadata(435) / var_metadata(                                             &
! String identifier
    'lit_n_fire',                                                              &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT nitrogen litter due to fire",                                         &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for burnt_carbon_rpm
!-----------------------------------------------------------------------------
DATA metadata(436) / var_metadata(                                             &
! String identifier
    'burnt_carbon_rpm',                                                        &
! Variable type
    var_type_surface,                                                          &
! Long name
    "RPM burnt carbon due to fire",                                            &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for veg_c_fire_emission_gb
!-----------------------------------------------------------------------------
DATA metadata(437) / var_metadata(                                             &
! String identifier
    'veg_c_fire_emission_gb',                                                  &
! Variable type
    var_type_surface,                                                          &
! Long name
    "GBM carbon emissions from fire",                                          &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for veg_c_fire_emission_pft
!-----------------------------------------------------------------------------
DATA metadata(438) / var_metadata(                                             &
! String identifier
    'veg_c_fire_emission_pft',                                                 &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT carbon emissions from fire",                                          &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for g_burn_pft
!-----------------------------------------------------------------------------
DATA metadata(439) / var_metadata(                                             &
! String identifier
    'g_burn_pft',                                                              &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Fire disturbance rate",                                                   &
! Units
    "m2/m2/360days"                                                            &
  ) /
!-----------------------------------------------------------------------------
! Metadata for RFM surface storage on river routing grid
!-----------------------------------------------------------------------------
DATA metadata(440) / var_metadata(                                             &
! String identifier
    'rfm_surfstore_rp',                                                        &
! Variable type
    var_type_rp,                                                               &
! Long name
    "River routing gridbox surface storage",                                   &
! Units
    "m3"                                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for RFM sub-surface storage on river routing grid
!-----------------------------------------------------------------------------
DATA metadata(441) / var_metadata(                                             &
! String identifier
    'rfm_substore_rp',                                                         &
! Variable type
    var_type_rp,                                                               &
! Long name
    "River routing gridbox sub-surface storage",                               &
! Units
    "m3"                                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for RFM surface inflow on river routing grid
!-----------------------------------------------------------------------------
DATA metadata(442) / var_metadata(                                             &
! String identifier
    'rfm_flowin_rp',                                                           &
! Variable type
    var_type_rp,                                                               &
! Long name
    "River routing gridbox surface inflow",                                    &
! Units
    "m3 s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for RFM sub-surface inflow on river routing grid
!-----------------------------------------------------------------------------
DATA metadata(443) / var_metadata(                                             &
! String identifier
    'rfm_bflowin_rp',                                                          &
! Variable type
    var_type_rp,                                                               &
! Long name
    "River routing gridbox sub-surface inflow",                                &
! Units
    "m3 s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cnsrv_nitrogen_triffid
!-----------------------------------------------------------------------------
DATA metadata(444) / var_metadata(                                             &
! String identifier
    'cnsrv_nitrogen_triffid',                                                  &
! Variable type
    var_type_surface,                                                          &
! Long name
    "error in land nitrogen conservation in triffid routine",                  &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cnsrv_vegN_triffid
!-----------------------------------------------------------------------------
DATA metadata(445) / var_metadata(                                             &
! String identifier
    'cnsrv_vegN_triffid',                                                      &
! Variable type
    var_type_surface,                                                          &
! Long name
    "error in vegetation nitrogen conservation in triffid routine",            &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cnsrv_soilN_triffid
!-----------------------------------------------------------------------------
DATA metadata(446) / var_metadata(                                             &
! String identifier
    'cnsrv_soilN_triffid',                                                     &
! Variable type
    var_type_surface,                                                          &
! Long name
    "error in soil nitrogen conservation in triffid routine",                  &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cnsrv_n_inorg_triffid
!-----------------------------------------------------------------------------
DATA metadata(447) / var_metadata(                                             &
! String identifier
    'cnsrv_n_inorg_triffid',                                                   &
! Variable type
    var_type_surface,                                                          &
! Long name
    "error in inorganic nitrogen conservation in triffid routine",             &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for root_abandon_gb
!-----------------------------------------------------------------------------
DATA metadata(448) / var_metadata(                                             &
! String identifier
    'root_abandon_gb',                                                         &
! Variable type
    var_type_surface,                                                          &
! Long name
    "gridbox mean C flux from roots to soil due to landuse change",            &
! Units
    "kg C  m-2 per 360days"                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for root_abandon_n_gb
!-----------------------------------------------------------------------------
DATA metadata(449) / var_metadata(                                             &
! String identifier
    'root_abandon_n_gb',                                                       &
! Variable type
    var_type_surface,                                                          &
! Long name
    "gridbox mean N flux from roots to soil due to landuse change",            &
! Units
    "kg N  m-2 per 360days"                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for resp_s_to_atmos_gb
!-----------------------------------------------------------------------------
DATA metadata(450) / var_metadata(                                             &
! String identifier
    'resp_s_to_atmos_gb',                                                      &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean soil respiration carbon flux to atmosphere",                 &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_gas_gb
!-----------------------------------------------------------------------------
DATA metadata(451) / var_metadata(                                             &
! String identifier
    'n_gas_gb',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "gridbox mean mineralised N Gas Emissions",                                &
! Units
    "kg/m2/360 days"                                                           &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_inorg_gb
!-----------------------------------------------------------------------------
DATA metadata(452) / var_metadata(                                             &
! String identifier
    'n_inorg_gb',                                                              &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox total N in soil inorganic species",                               &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Variables available with any multi-pool soil C model (e.g. 4-pool, ECOSSE).
!-----------------------------------------------------------------------------
! Soil prognostic variables and single pools from multi-pool prognostic
! variables.
!-----------------------------------------------------------------------------
! Metadata for c_bio
!-----------------------------------------------------------------------------
DATA metadata(453) / var_metadata(                                             &
! String identifier
    'c_bio',                                                                   &
! Variable type
    var_type_sclayer,                                                          &
! Long name
    "C in soil biomass",                                                       &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for c_dpm
!-----------------------------------------------------------------------------
DATA metadata(454) / var_metadata(                                             &
! String identifier
    'c_dpm',                                                                   &
! Variable type
    var_type_sclayer,                                                          &
! Long name
    "C in decomposable plant material",                                        &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for c_hum
!-----------------------------------------------------------------------------
DATA metadata(455) / var_metadata(                                             &
! String identifier
    'c_hum',                                                                   &
! Variable type
    var_type_sclayer,                                                          &
! Long name
    "C in humus",                                                              &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for c_rpm
!-----------------------------------------------------------------------------
DATA metadata(456) / var_metadata(                                             &
! String identifier
    'c_rpm',                                                                   &
! Variable type
    var_type_sclayer,                                                          &
! Long name
    "C in resistant plant material",                                           &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_bio
!-----------------------------------------------------------------------------
DATA metadata(457) / var_metadata(                                             &
! String identifier
    'n_bio',                                                                   &
! Variable type
    var_type_sclayer,                                                          &
! Long name
    "N in soil biomass",                                                       &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_dpm
!-----------------------------------------------------------------------------
DATA metadata(458) / var_metadata(                                             &
! String identifier
    'n_dpm',                                                                   &
! Variable type
    var_type_sclayer,                                                          &
! Long name
    "N in decomposable plant material",                                        &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_hum
!-----------------------------------------------------------------------------
DATA metadata(459) / var_metadata(                                             &
! String identifier
    'n_hum',                                                                   &
! Variable type
    var_type_sclayer,                                                          &
! Long name
    "N in humus",                                                              &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_rpm
!-----------------------------------------------------------------------------
DATA metadata(460) / var_metadata(                                             &
! String identifier
    'n_rpm',                                                                   &
! Variable type
    var_type_sclayer,                                                          &
! Long name
    "N in resistant plant material",                                           &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Gridbox totals of multi-pool soil prognostic variables.
!-----------------------------------------------------------------------------
! Metadata for c_bio_gb
!-----------------------------------------------------------------------------
DATA metadata(461) / var_metadata(                                             &
! String identifier
    'c_bio_gb',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "C in soil biomass, gridbox total",                                        &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for c_dpm_gb
!-----------------------------------------------------------------------------
DATA metadata(462) / var_metadata(                                             &
! String identifier
    'c_dpm_gb',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "C in decomposable plant material, gridbox total",                         &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for c_hum_gb
!-----------------------------------------------------------------------------
DATA metadata(463) / var_metadata(                                             &
! String identifier
    'c_hum_gb',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "C in soil humus, gridbox total",                                          &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for c_rpm_gb
!-----------------------------------------------------------------------------
DATA metadata(464) / var_metadata(                                             &
! String identifier
    'c_rpm_gb',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "C in resistant plant material, gridbox total",                            &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_bio_gb
!-----------------------------------------------------------------------------
DATA metadata(465) / var_metadata(                                             &
! String identifier
    'n_bio_gb',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "N in soil biomass, gridbox total",                                        &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_dpm_gb
!-----------------------------------------------------------------------------
DATA metadata(466) / var_metadata(                                             &
! String identifier
    'n_dpm_gb',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "N in decomposable plant material, gridbox total",                         &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_hum_gb
!-----------------------------------------------------------------------------
DATA metadata(467) / var_metadata(                                             &
! String identifier
    'n_hum_gb',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "N in soil humus, gridbox total",                                          &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_rpm_gb
!-----------------------------------------------------------------------------
DATA metadata(468) / var_metadata(                                             &
! String identifier
    'n_rpm_gb',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "N in resistant plant material, gridbox total",                            &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Gridbox totals of derived variables for multi-pool soil models.
!-----------------------------------------------------------------------------
! Metadata for n_soil_gb
!-----------------------------------------------------------------------------
DATA metadata(469) / var_metadata(                                             &
! String identifier
    'n_soil_gb',                                                               &
! Variable type
    var_type_surface,                                                          &
! Long name
    "N in soil, all forms, gridbox total",                                     &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! ECOSSE: components of prognostic variables.
!-----------------------------------------------------------------------------
! Metadata for n_amm.
!-----------------------------------------------------------------------------
DATA metadata(470) / var_metadata(                                             &
! String identifier
    'n_amm',                                                                   &
! Variable type
    var_type_sclayer,                                                          &
! Long name
    "N in soil ammonium",                                                      &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_nit
!-----------------------------------------------------------------------------
DATA metadata(471) / var_metadata(                                             &
! String identifier
    'n_nit',                                                                   &
! Variable type
    var_type_sclayer,                                                          &
! Long name
    "N in soil nitrate",                                                       &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Gridbox totals of components of ECOSSE prognostic variables.
!-----------------------------------------------------------------------------
! Metadata for n_amm_gb
!-----------------------------------------------------------------------------
DATA metadata(472) / var_metadata(                                             &
! String identifier
    'n_amm_gb',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "N in soil ammonium, gridbox total",                                       &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_nit_gb
!-----------------------------------------------------------------------------
DATA metadata(473) / var_metadata(                                             &
! String identifier
    'n_nit_gb',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "N in soil nitrate, gridbox total",                                        &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Soil ancillaries on soil carbon layers.
!-----------------------------------------------------------------------------
! Metadata for soil_ph
!-----------------------------------------------------------------------------
DATA metadata(474) / var_metadata(                                             &
! String identifier
    'soil_ph',                                                                 &
! Variable type
    var_type_sclayer,                                                          &
! Long name
    "Soil pH",                                                                 &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for soil_ph_const_z
!-----------------------------------------------------------------------------
DATA metadata(475) / var_metadata(                                             &
! String identifier
    'soil_ph_const_z',                                                         &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name
! and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for slope
!-----------------------------------------------------------------------------
DATA metadata(476) / var_metadata(                                             &
! String identifier
    'slope',                                                                   &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Terrain slope",                                                           &
! Units
    "degrees"                                                                  &
  ) /
!-----------------------------------------------------------------------------
! Metadata for b_soilt
!-----------------------------------------------------------------------------
DATA metadata(477) / var_metadata(                                             &
! String identifier
    'b_soilt',                                                                 &
! Variable type
    var_type_soilt_soil,                                                       &
! Long name
    "Soil tiled Brooks-Corey exponent for each soil layer",                    &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for b_const_z_soilt
!-----------------------------------------------------------------------------
DATA metadata(478) / var_metadata(                                             &
! String identifier
    'b_const_z_soilt',                                                         &
! Variable type
    var_type_soilt,                                                            &
! Variable is not available for output, so give dummy values for
! long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sathh_soilt
!-----------------------------------------------------------------------------
DATA metadata(479) / var_metadata(                                             &
! String identifier
    'sathh_soilt',                                                             &
! Variable type
    var_type_soilt_soil,                                                       &
! Long name
    "Soil tiled saturated soil water pressure for each soil layer",            &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sathh_const_z_soilt
!-----------------------------------------------------------------------------
DATA metadata(480) / var_metadata(                                             &
! String identifier
    'sathh_const_z_soilt',                                                     &
! Variable type
    var_type_soilt,                                                            &
! Variable is not available for output, so give dummy values for
! long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for satcon_soilt
!-----------------------------------------------------------------------------
DATA metadata(481) / var_metadata(                                             &
! String identifier
    'satcon_soilt',                                                            &
! Variable type
    var_type_soilt_soil,                                                       &
! Long name
    "Soil tiled saturated hydraulic conductivity for each soil layer",         &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for satcon_const_z_soilt
!-----------------------------------------------------------------------------
DATA metadata(482) / var_metadata(                                             &
! String identifier
    'satcon_const_z_soilt',                                                    &
! Variable type
    var_type_soilt,                                                            &
! Variable is not available for output, so give dummy values for
! long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sm_sat_soilt
!-----------------------------------------------------------------------------
DATA metadata(483) / var_metadata(                                             &
! String identifier
    'sm_sat_soilt',                                                            &
! Variable type
    var_type_soilt_soil,                                                       &
! Long name
    "Soil tiled volumetric moisture content at saturation" //                  &
    "for each soil layer",                                                     &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sm_sat_const_z_soilt
!-----------------------------------------------------------------------------
DATA metadata(484) / var_metadata(                                             &
! String identifier
    'sm_sat_const_z_soilt',                                                    &
! Variable type
    var_type_soilt,                                                            &
! Variable is not available for output, so give dummy values for
! long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sm_crit_soilt
!-----------------------------------------------------------------------------
DATA metadata(485) / var_metadata(                                             &
! String identifier
    'sm_crit_soilt',                                                           &
! Variable type
    var_type_soilt_soil,                                                       &
! Long name
    "Soil tiled volumetric moisture content at critical point" //              &
    "for each soil layer",                                                     &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sm_crit_const_z_soilt
!-----------------------------------------------------------------------------
DATA metadata(486) / var_metadata(                                             &
! String identifier
    'sm_crit_const_z_soilt',                                                   &
! Variable type
    var_type_soilt,                                                            &
! Variable is not available for output, so give dummy values for
! long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sm_wilt_soilt
!-----------------------------------------------------------------------------
DATA metadata(487) / var_metadata(                                             &
! String identifier
    'sm_wilt_soilt',                                                           &
! Variable type
    var_type_soilt_soil,                                                       &
! Long name
    "Soil tiled volumetric moisture content at wilting point" //               &
    "for each soil layer",                                                     &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sm_wilt_const_z_soilt
!-----------------------------------------------------------------------------
DATA metadata(488) / var_metadata(                                             &
! String identifier
    'sm_wilt_const_z_soilt',                                                   &
! Variable type
    var_type_soilt,                                                            &
! Variable is not available for output, so give dummy values for
! long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for hcap_soilt
!-----------------------------------------------------------------------------
DATA metadata(489) / var_metadata(                                             &
! String identifier
    'hcap_soilt',                                                              &
! Variable type
    var_type_soilt_soil,                                                       &
! Long name
    "Soil tiled dry soil heat capacity for each soil layer",                   &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for hcap_const_z_soilt
!-----------------------------------------------------------------------------
DATA metadata(490) / var_metadata(                                             &
! String identifier
    'hcap_const_z_soilt',                                                      &
! Variable type
    var_type_soilt,                                                            &
! Variable is not available for output, so give dummy values for
! long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for hcon_soilt
!-----------------------------------------------------------------------------
DATA metadata(491) / var_metadata(                                             &
! String identifier
    'hcon_soilt',                                                              &
! Variable type
    var_type_soilt_soil,                                                       &
! Long name
    "Soil tiled dry soil thermal conductivity for each soil layer",            &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for hcon_const_z_soilt
!-----------------------------------------------------------------------------
DATA metadata(492) / var_metadata(                                             &
! String identifier
    'hcon_const_z_soilt',                                                      &
! Variable type
    var_type_soilt,                                                            &
! Variable is not available for output, so give dummy values for
! long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fexp_soilt
!-----------------------------------------------------------------------------
DATA metadata(493) / var_metadata(                                             &
! String identifier
    'fexp_soilt',                                                              &
! Variable type
    var_type_soilt,                                                            &
! Variable is not available for output, so give dummy values for
! long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for ti_mean_soilt
!-----------------------------------------------------------------------------
DATA metadata(494) / var_metadata(                                             &
! String identifier
    'ti_mean_soilt',                                                           &
! Variable type
    var_type_soilt,                                                            &
! Variable is not available for output, so give dummy values for
! long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for ti_sig_soilt
!-----------------------------------------------------------------------------
DATA metadata(495) / var_metadata(                                             &
! String identifier
    'ti_sig_soilt',                                                            &
! Variable type
    var_type_soilt,                                                            &
! Variable is not available for output, so give dummy values for
! long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for flammability
!-----------------------------------------------------------------------------
DATA metadata(496) / var_metadata(                                             &
! String identifier
    'flammability',                                                            &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT flammability",                                                        &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for g_burn_gb
!-----------------------------------------------------------------------------
DATA metadata(497) / var_metadata(                                             &
! String identifier
    'g_burn_gb',                                                               &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Fire disturbance rate by gridbox",                                        &
! Units
    "m2/m2/360days"                                                            &
  ) /
!-----------------------------------------------------------------------------
! Metadata for soil_ph_soilt
!-----------------------------------------------------------------------------
DATA metadata(498) / var_metadata(                                             &
! String identifier
    'soil_ph_soilt',                                                           &
! Variable type
    var_type_soilt_sclayer,                                                    &
! Long name
    "Soil pH on soil tiles",                                                   &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for drain_soilt
!-----------------------------------------------------------------------------
DATA metadata(499) / var_metadata(                                             &
! String identifier
    'drain_soilt',                                                             &
! Variable type
    var_type_soilt,                                                            &
! Long name
    "Drainage from bottom (nshyd) soil layer on soil tiles",                   &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fch4_wetl_cs_soilt
!-----------------------------------------------------------------------------
DATA metadata(500) / var_metadata(                                             &
! String identifier
    'fch4_wetl_cs_soilt',                                                      &
! Variable type
    var_type_soilt,                                                            &
! Long name
    "Scaled methane flux from wetland fraction (cs substrate) on soil tiles",  &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fch4_wetl_npp_soilt
!-----------------------------------------------------------------------------
DATA metadata(501) / var_metadata(                                             &
! String identifier
    'fch4_wetl_npp_soilt',                                                     &
! Variable type
    var_type_soilt,                                                            &
! Long name
    "Scaled methane flux from wetland fraction (npp substrate) on soil tiles", &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fch4_wetl_resps_soilt
!-----------------------------------------------------------------------------
DATA metadata(502) / var_metadata(                                             &
! String identifier
    'fch4_wetl_resps_soilt',                                                   &
! Variable type
    var_type_soilt,                                                            &
! Long name
    "Scaled methane flux from wetland fraction (soil resp substrate) " //      &
    "on soil tiles",                                                           &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fsat_soilt
!-----------------------------------------------------------------------------
DATA metadata(503) / var_metadata(                                             &
! String identifier
    'fsat_soilt',                                                              &
! Variable type
    var_type_soilt,                                                            &
! Long name
    "Surface saturated fraction on soil tiles",                                &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fwetl_soilt
!-----------------------------------------------------------------------------
DATA metadata(504) / var_metadata(                                             &
! String identifier
    'fwetl_soilt',                                                             &
! Variable type
    var_type_soilt,                                                            &
! Long name
    "Wetland fraction at end of model timestep on soil tiles.",                &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for qbase_soilt
!-----------------------------------------------------------------------------
DATA metadata(505) / var_metadata(                                             &
! String identifier
    'qbase_soilt',                                                             &
! Variable type
    var_type_soilt,                                                            &
! Long name
    "Baseflow (lateral subsurface runoff) on soil tiles",                      &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for qbase_zw_soilt
!-----------------------------------------------------------------------------
DATA metadata(506) / var_metadata(                                             &
! String identifier
    'qbase_zw_soilt',                                                          &
! Variable type
    var_type_soilt,                                                            &
! Long name
    "Baseflow from deep LSH/TOPMODEL layer on soil tiles",                     &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sat_excess_roff_soilt
!-----------------------------------------------------------------------------
DATA metadata(507) / var_metadata(                                             &
! String identifier
    'sat_excess_roff_soilt',                                                   &
! Variable type
    var_type_soilt,                                                            &
! Long name
    "Saturation excess surface ('Dunne') runoff on soil tiles",                &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for zw_soilt
!-----------------------------------------------------------------------------
DATA metadata(508) / var_metadata(                                             &
! String identifier
    'zw_soilt',                                                                &
! Variable type
    var_type_soilt,                                                            &
! Long name
    "Mean depth to water table on soil tiles",                                 &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for ext_soilt
!-----------------------------------------------------------------------------
DATA metadata(509) / var_metadata(                                             &
! String identifier
    'ext_soilt',                                                               &
! Variable type
    var_type_soilt_soil,                                                       &
! Long name
    "Extraction of water from each soil layer on soil tiles",                  &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for smcl_soilt
!-----------------------------------------------------------------------------
DATA metadata(510) / var_metadata(                                             &
! String identifier
    'smcl_soilt',                                                              &
! Variable type
    var_type_soilt_soil,                                                       &
! Long name
    "Moisture content of each soil layer on soil tiles",                       &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sthf_soilt
!-----------------------------------------------------------------------------
DATA metadata(511) / var_metadata(                                             &
! String identifier
    'sthf_soilt',                                                              &
! Variable type
    var_type_soilt_soil,                                                       &
! Long name
    "Frozen moisture content of each soil layer as a fraction of " //          &
    "saturation on soil tiles",                                                &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sthu_soilt
!-----------------------------------------------------------------------------
DATA metadata(512) / var_metadata(                                             &
! String identifier
    'sthu_soilt',                                                              &
! Variable type
    var_type_soilt_soil,                                                       &
! Long name
    "Unfrozen moisture content of each soil layer as a fraction of "  //       &
    "saturation on soil tiles",                                                &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for t_soil_soilt
!-----------------------------------------------------------------------------
DATA metadata(513) / var_metadata(                                             &
! String identifier
    't_soil_soilt',                                                            &
! Variable type
    var_type_soilt_soil,                                                       &
! Long name
    "Sub-surface temperature of each layer on soil tiles",                     &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for frac_irrig_soilt
!-----------------------------------------------------------------------------
DATA metadata(514) / var_metadata(                                             &
! String identifier
    'frac_irrig_soilt',                                                        &
! Variable type
    var_type_soilt,                                                            &
! Long name
    "Irrigated fraction on soil tiles",                                        &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sthu_irr_soilt
!-----------------------------------------------------------------------------
DATA metadata(515) / var_metadata(                                             &
! String identifier
    'sthu_irr_soilt',                                                          &
! Variable type
    var_type_soilt_soil,                                                       &
! Long name
    "Wetness of each soil layer over irrigation on soil tiles",                &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for croplatestharvdate
!-----------------------------------------------------------------------------
DATA metadata(516) / var_metadata(                                             &
! String identifier
    'croplatestharvdate',                                                      &
! Variable type
    var_type_cpft,                                                             &
! Long name
    "Latest day of year when crop can be harvested",                           &
! Units
    "day of year"                                                              &
  ) /
!-----------------------------------------------------------------------------
! Metadata for ftemp
!-----------------------------------------------------------------------------
DATA metadata(517) / var_metadata(                                             &
! String identifier
    'ftemp',                                                                   &
! Variable type
    var_type_soil,                                                             &
! Long name
    "Soil respiration rate modifier due to soil temperature",                  &
! Units
    "-"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fsth
!-----------------------------------------------------------------------------
DATA metadata(518) / var_metadata(                                             &
! String identifier
    'fsth',                                                                    &
! Variable type
    var_type_soil,                                                             &
! Long name
    "Soil respiration rate modifier due to soil moisture",                     &
! Units
    "-"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fprf
!-----------------------------------------------------------------------------
DATA metadata(519) / var_metadata(                                             &
! String identifier
    'fprf',                                                                    &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Soil respiration rate modifier due to vegetation cover",                  &
! Units
    "-"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for overbank inundation fraction on model grid
!-----------------------------------------------------------------------------
DATA metadata(520) / var_metadata(                                             &
! String identifier
    'frac_fplain_lp',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Overbank inundation area as a fraction of gridcell area",                 &
! Units
    "(fraction of gridcell)"                                                   &
  )                                                                            &
/
!-----------------------------------------------------------------------------
! Metadata for logn_mean
!-----------------------------------------------------------------------------
DATA metadata(521) / var_metadata(                                             &
! String identifier
    'logn_mean',                                                               &
! Variable type
    var_type_surface,                                                          &
! Long name
    "River routing logn_mean",                                                 &
! Units
    "ln(metres)"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for logn_stdev
!-----------------------------------------------------------------------------
DATA metadata(522) / var_metadata(                                             &
! String identifier
    'logn_stdev',                                                              &
! Variable type
    var_type_surface,                                                          &
! Long name
    "River routing logn_stdev",                                                &
! Units
    "ln(metres)"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for clay_const_z
!-----------------------------------------------------------------------------
DATA metadata(523) / var_metadata(                                             &
! String identifier
    'clay_const_z',                                                            &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for clay_soilt
!-----------------------------------------------------------------------------
DATA metadata(524) / var_metadata(                                             &
! String identifier
    'clay_soilt',                                                              &
! Variable type
    var_type_soilt_sclayer,                                                    &
! Long name
    "Soil tiled soil clay content",                                            &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for clay_const_z_soilt
!-----------------------------------------------------------------------------
DATA metadata(525) / var_metadata(                                             &
! String identifier
    'clay_const_z_soilt',                                                      &
! Variable type
    var_type_soilt,                                                            &
! Variable is not available for output, so give dummy values for
! long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for plant_input_c_gb.
!-----------------------------------------------------------------------------
DATA metadata(526) / var_metadata(                                             &
! String identifier
    'plant_input_c_gb',                                                        &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Plant input of carbon to soil by litterfall",                             &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for plant_input_n_gb.
!-----------------------------------------------------------------------------
DATA metadata(527) / var_metadata(                                             &
! String identifier
    'plant_input_n_gb',                                                        &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Plant input of nitrogen to soil by litterfall",                           &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for imogen d_land_atmos_co2
!-----------------------------------------------------------------------------
DATA metadata(528) / var_metadata(                                             &
! String identifier
    'd_land_atmos_co2',                                                        &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Change in atmos CO2 conc from land-atmos feedbacks in IMOGEN" //          &
    "(repeated global value)",                                                 &
! Units
    "ppm/year"                                                                 &
  ) /
!-----------------------------------------------------------------------------
! Metadata for imogen d_ocean_atmos
!-----------------------------------------------------------------------------
DATA metadata(529) / var_metadata(                                             &
! String identifier
    'd_ocean_atmos',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Change in atmos CO2 conc from ocean feedbacks in IMOGEN" //               &
     "(repeated global value)",                                                &
! Units
    "ppm/year"                                                                 &
  ) /
!-----------------------------------------------------------------------------
! Metadata for c_emiss_data
!-----------------------------------------------------------------------------
DATA metadata(530) / var_metadata(                                             &
! String identifier
    'c_emiss_data',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "prescribed carbon emissions in IMOGEN" //                                 &
    "(repeated global value)",                                                 &
! Units
    "Gt/year"                                                                  &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_denitrif_gb.
!-----------------------------------------------------------------------------
DATA metadata(531) / var_metadata(                                             &
! String identifier
    'n_denitrif_gb',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox rate of denitrification, expressed as N",                         &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_nitrif_gb.
!-----------------------------------------------------------------------------
DATA metadata(532) / var_metadata(                                             &
! String identifier
    'n_nitrif_gb',                                                             &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox rate of nitrification, expressed as N",                           &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for no_soil_gb.
!-----------------------------------------------------------------------------
DATA metadata(533) / var_metadata(                                             &
! String identifier
    'no_soil_gb',                                                              &
! Variable type
    var_type_surface,                                                          &
! Long name
    "N in NO flux from soil to atmosphere",                                    &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n2_denitrif_gb.
!-----------------------------------------------------------------------------
DATA metadata(534) / var_metadata(                                             &
! String identifier
    'n2_denitrif_gb',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "N in N2 lost from soil during denitrification",                           &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n2o_denitrif_gb.
!-----------------------------------------------------------------------------
DATA metadata(535) / var_metadata(                                             &
! String identifier
    'n2o_denitrif_gb',                                                         &
! Variable type
    var_type_surface,                                                          &
! Long name
    "N in N2O lost during denitrification",                                    &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n2o_nitrif_gb.
!-----------------------------------------------------------------------------
DATA metadata(536) / var_metadata(                                             &
! String identifier
    'n2o_nitrif_gb',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "N in N2O lost during nitrification, including partial nitrification",     &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n2o_part_nitrif_gb.
!-----------------------------------------------------------------------------
DATA metadata(537) / var_metadata(                                             &
! String identifier
    'n2o_part_nitrif_gb',                                                      &
! Variable type
    var_type_surface,                                                          &
! Long name
    "N in N2O lost by partial nitrification",                                  &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n2o_soil_gb.
!-----------------------------------------------------------------------------
DATA metadata(538) / var_metadata(                                             &
! String identifier
    'n2o_soil_gb',                                                             &
! Variable type
    var_type_surface,                                                          &
! Long name
    "N in N2O flux from soil to atmosphere",                                   &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_leach_amm_gb.
!-----------------------------------------------------------------------------
DATA metadata(539) / var_metadata(                                             &
! String identifier
    'n_leach_amm_gb',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox N lost through leaching of soil ammonium",                        &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_leach_nit_gb.
!-----------------------------------------------------------------------------
DATA metadata(540) / var_metadata(                                             &
! String identifier
    'n_leach_nit_gb',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox N lost through leaching of soil nitrate",                         &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lai_gb.
!-----------------------------------------------------------------------------
DATA metadata(541) / var_metadata(                                             &
! String identifier
    'lai_gb',                                                                  &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox leaf area index",                                                 &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for apar.
!-----------------------------------------------------------------------------
DATA metadata(542) / var_metadata(                                             &
! String identifier
    'apar',                                                                    &
! Variable type
    var_type_surface,                                                          &
! Long name
    "PFT Absorbed Photosynthetically Active Radiation",                        &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for apar_gb.
!-----------------------------------------------------------------------------
DATA metadata(543) / var_metadata(                                             &
! String identifier
    'apar_gb',                                                                 &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox Absorbed Photosynthetically Active Radiation",                    &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for bl_height.
!-----------------------------------------------------------------------------
DATA metadata(544) / var_metadata(                                             &
! String identifier
    'bl_height',                                                               &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, but giving long name and units for
! clarity.
! Long name
    "Height above surface of top of boundary layer",                           &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for level_separation.
!-----------------------------------------------------------------------------
DATA metadata(545) / var_metadata(                                             &
! String identifier
    'level_separation',                                                        &
! Variable type
    var_type_bl_level,                                                         &
! Variable is not available for output, but giving long name and units for
! clarity.
! Long name
    "Vertical separation between atmospheric boundary layer levels",           &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for deposition_conc
!-----------------------------------------------------------------------------
DATA metadata(546) / var_metadata(                                             &
! String identifier
    'deposition_conc',                                                         &
! Variable type
    var_type_dep_species,                                                      &
! Long name
    "Surface concentration of deposited species as mass mixing ratios",        &
! Units
    "kg kg-1"                                                                  &
  ) /
!-----------------------------------------------------------------------------
! Metadata for tau_gb
!-----------------------------------------------------------------------------
DATA metadata(547) / var_metadata(                                             &
! String identifier
    'tau_gb',                                                                  &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean surface wind stress",                                        &
! Units
    "N m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for tau
!-----------------------------------------------------------------------------
DATA metadata(548) / var_metadata(                                             &
! String identifier
    'tau',                                                                     &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Tile surface stress for land tiles",                                      &
! Units
    "N m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_inorg_avail_pft
!-----------------------------------------------------------------------------
DATA metadata(549) / var_metadata(                                             &
! String identifier
    'n_inorg_avail_pft',                                                       &
! Variable type
    var_type_pft_sclayer,                                                      &
! Long name
    "Inorganic N pool that is available for plant uptake",                     &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for metstat temp_ave_nday
!-----------------------------------------------------------------------------
DATA metadata(550) / var_metadata(                                             &
! String identifier
    'temp_ave_nday',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name - this is dynamic and is reset in get_var_attrs.
! This variable is not available as a diagnostic.
    "Average temperature (exp filter) over previous n_day_photo_acclim days",  &
! Units
    "Kelvin"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for t_growth_gb
!-----------------------------------------------------------------------------
DATA metadata(551) / var_metadata(                                             &
! String identifier
    't_growth_gb',                                                             &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Growth (average) temperature for C3 photosynthesis",                      &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for n_soil
!-----------------------------------------------------------------------------
DATA metadata(552) / var_metadata(                                             &
! String identifier
    'n_soil',                                                                  &
! Variable type
    var_type_soil_n_pool,                                                      &
! Long name
    "Nitrogen in soil pools",                                                  &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cropfrac
!-----------------------------------------------------------------------------
DATA metadata(553) / var_metadata(                                             &
! String identifier
    'cropfrac',                                                                &
! Variable type
    var_type_cpft,                                                             &
! Long name
    "Fractional cover of each crop type",                                      &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for irrigation fraction on irrigated tiles
!-----------------------------------------------------------------------------
DATA metadata(554) / var_metadata(                                             &
! String identifier
    'irrfrac_irrtiles',                                                        &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Irrigated fraction on irrigated tiles",                                   &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for SoilTemp_CABLE
!-----------------------------------------------------------------------------
DATA metadata(555) / var_metadata(                                             &
! String identifier
    'SoilTemp_CABLE',                                                          &
! Variable type
    var_type_cable_soil,                                                       &
! Long name
    "PFT soil temperature in layers",                                          &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for FrozenSoilFrac_CABLE
!-----------------------------------------------------------------------------
DATA metadata(556) / var_metadata(                                             &
! String identifier
    'FrozenSoilFrac_CABLE',                                                    &
! Variable type
    var_type_cable_soil,                                                       &
! Long name
    "PFT frozen soil in layers",                                               &
! Units
    "fraction"                                                                 &
  ) /
!-----------------------------------------------------------------------------
! Metadata for SoilMoisture_CABLE
!-----------------------------------------------------------------------------
DATA metadata(557) / var_metadata(                                             &
! String identifier
    'SoilMoisture_CABLE',                                                      &
! Variable type
    var_type_cable_soil,                                                       &
! Long name
    "PFT soil moisture in layers",                                             &
! Units
    ""                                                                         &
  ) /
!-----------------------------------------------------------------------------
! Metadata for SnowDepth_CABLE
!-----------------------------------------------------------------------------
DATA metadata(558) /  var_metadata(                                            &
! String identifier
    'SnowDepth_CABLE',                                                         &
! Variable type
    var_type_cable_snow,                                                       &
! Long name
    "PFT snow depth in layers",                                                &
! Units
    ""                                                                         &
  ) /
!-----------------------------------------------------------------------------
! Metadata for SnowMass_CABLE
!-----------------------------------------------------------------------------
DATA metadata(559) /  var_metadata(                                            &
! String identifier
    'SnowMass_CABLE',                                                          &
! Variable type
    var_type_cable_snow,                                                       &
! Long name
    "PFT snow mass in layers",                                                 &
! Units
    ""                                                                         &
  ) /
!-----------------------------------------------------------------------------
! Metadata for SnowDensity_CABLE
!-----------------------------------------------------------------------------
DATA metadata(560) /  var_metadata(                                            &
! String identifier
    'SnowDensity_CABLE',                                                       &
! Variable type
    var_type_cable_snow,                                                       &
! Long name
    "PFT snow density in layers",                                              &
! Units
    ""                                                                         &
  ) /
!-----------------------------------------------------------------------------
! Metadata for SnowTemp_CABLE
!-----------------------------------------------------------------------------
DATA metadata(561) /  var_metadata(                                            &
! String identifier
    'SnowTemp_CABLE',                                                          &
! Variable type
    var_type_cable_snow,                                                       &
! Long name
    "PFT snow temperature in layers",                                          &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for ThreeLayerSnowFlag_CABLE
!-----------------------------------------------------------------------------
DATA metadata(562) /  var_metadata(                                            &
! String identifier
    'ThreeLayerSnowFlag_CABLE',                                                &
! Variable type
    var_type_cable1l_snow,                                                     &
! Long name
    "3 layer snow flag for tile",                                              &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for SnowAge_CABLE
!-----------------------------------------------------------------------------
DATA metadata(563) /  var_metadata(                                            &
! String identifier
    'SnowAge_CABLE',                                                           &
! Variable type
    var_type_cable1l_snow,                                                     &
! Long name
    "Snow Age for tile",                                                       &
! Units
    ""                                                                         &
  ) /
!-----------------------------------------------------------------------------
! Metadata for OneLyrSnowDensity_CABLE
!-----------------------------------------------------------------------------
DATA metadata(564) /  var_metadata(                                            &
! String identifier
    'OneLyrSnowDensity_CABLE',                                                 &
! Variable type
    VAR_TYPE_CABLE1l_SNOW,                                                     &
! Long name
    "PFT snow density in aggregate 1 layer",                                   &
! Units
    ""                                                                         &
  ) /
!-----------------------------------------------------------------------------
! Metadata for albsoil_soilt
!-----------------------------------------------------------------------------
DATA metadata(565) / var_metadata(                                             &
! String identifier
    'albsoil_soilt',                                                           &
! Variable type
    var_type_soilt,                                                            &
! Variable is not available for output, so give dummy values for long name
! and units.
! Long name
    "",                                                                        &
! Units
    ""                                                                         &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cs_soilt
!-----------------------------------------------------------------------------
DATA metadata(566) / var_metadata(                                             &
! String identifier
    'cs_soilt',                                                                &
! Variable type
    var_type_soilt_sclayer_scpool,                                             &
! Long name
    "Soil carbon, on soil tiles",                                              &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sthuf_soilt
!-----------------------------------------------------------------------------
DATA metadata(567) / var_metadata(                                             &
! String identifier
    'sthuf_soilt',                                                             &
! Variable type
    var_type_soilt_soil,                                                       &
! Variable is not available for output, so give dummy values for long name
! and units.
! Long name
    "",                                                                        &
! Units
    ""                                                                         &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sthzw_soilt
!-----------------------------------------------------------------------------
DATA metadata(568) / var_metadata(                                             &
! String identifier
    'sthzw_soilt',                                                             &
! Variable type
    var_type_soilt,                                                            &
! Long name
    "Soil wetness in deep LSH/TOPMODEL layer, on soil tiles",                  &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for upward_sw_1
! N.B. Diagnostics upward_sw_1, ..., upward_sw_4 are meaningful only if
! appropriate values of wght_alb are set.
!-----------------------------------------------------------------------------
DATA metadata(569) / var_metadata(                                             &
! String identifier
    'upward_sw_1',                                                             &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Upward SW flux in band 1 (direct beam visible)",                          &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for upward_sw_2
!-----------------------------------------------------------------------------
DATA metadata(570) / var_metadata(                                             &
! String identifier
    'upward_sw_2',                                                             &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Upward SW flux in band 2 (diffuse visible)",                              &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for upward_sw_3
!-----------------------------------------------------------------------------
DATA metadata(571) / var_metadata(                                             &
! String identifier
    'upward_sw_3',                                                             &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Upward SW flux in band 3 (direct beam NIR)",                              &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for upward_sw_4
!-----------------------------------------------------------------------------
DATA metadata(572) / var_metadata(                                             &
! String identifier
    'upward_sw_4',                                                             &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Upward SW flux in band 4 (diffuse NIR)",                                  &
! Units
    "W m-2"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for demand_domestic
!-----------------------------------------------------------------------------
DATA metadata(573) / var_metadata(                                             &
! String identifier
    'demand_domestic',                                                         &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Demand for water for domestic use",                                       &
! Units
    "kg s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for demand_environment
!-----------------------------------------------------------------------------
DATA metadata(574) / var_metadata(                                             &
! String identifier
    'demand_environment',                                                      &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Demand for water for environmental use",                                  &
! Units
    "kg s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for demand_industry
!-----------------------------------------------------------------------------
DATA metadata(575) / var_metadata(                                             &
! String identifier
    'demand_industry',                                                         &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Demand for water for industrial use",                                     &
! Units
    "kg s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for demand_irrigation
!-----------------------------------------------------------------------------
DATA metadata(576) / var_metadata(                                             &
! String identifier
    'demand_irrigation',                                                       &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Demand for water for irrigation use",                                     &
! Units
    "kg s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for demand_livestock
!-----------------------------------------------------------------------------
DATA metadata(577) / var_metadata(                                             &
! String identifier
    'demand_livestock',                                                        &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Demand for water for livestock use",                                      &
! Units
    "kg s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for demand_transfers
!-----------------------------------------------------------------------------
DATA metadata(578) / var_metadata(                                             &
! String identifier
    'demand_transfers',                                                        &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Demand for water for transfers",                                          &
! Units
    "kg s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for demand_rate_domestic
!-----------------------------------------------------------------------------
DATA metadata(579) / var_metadata(                                             &
! String identifier
    'demand_rate_domestic',                                                    &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Demand rate for water for domestic use (excluding any allowance "      // &
    "for losses)",                                                             &
! Units
    "kg s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for demand_rate_industry
!-----------------------------------------------------------------------------
DATA metadata(580) / var_metadata(                                             &
! String identifier
    'demand_rate_industry',                                                    &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Demand rate for water for industrial use  (excluding any allowance "   // &
    "for losses)",                                                             &
! Units
    "kg s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for demand_rate_livestock
!-----------------------------------------------------------------------------
DATA metadata(581) / var_metadata(                                             &
! String identifier
    'demand_rate_livestock',                                                   &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Demand rate for water for livestock use (excluding any allowance "     // &
    "for losses)",                                                             &
! Units
    "kg s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for demand_rate_transfers
!-----------------------------------------------------------------------------
DATA metadata(582) / var_metadata(                                             &
! String identifier
    'demand_rate_transfers',                                                   &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Demand rate for water for transfers (excluding any allowance "         // &
    "for losses)",                                                             &
! Units
    "kg s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for water_demand
!-----------------------------------------------------------------------------
DATA metadata(583) / var_metadata(                                             &
! String identifier
    'water_demand',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Demand for water across all sectors",                                     &
! Units
    "kg s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for conv_loss_frac
!-----------------------------------------------------------------------------
DATA metadata(584) / var_metadata(                                             &
! String identifier
    'conv_loss_frac',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Conveyance loss: fraction of abstracted water lost between source " //    &
    "and user",                                                                &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for unmet_domestic
!-----------------------------------------------------------------------------
DATA metadata(585) / var_metadata(                                             &
! String identifier
    'unmet_domestic',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "The part of the demand for water for domestic use that is not satisfied", &
! Units
    "kg s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sfc_water_frac
!-----------------------------------------------------------------------------
DATA metadata(586) / var_metadata(                                             &
! String identifier
    'sfc_water_frac',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Target for the fraction of demand to be met from surface water",          &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lake_depth
!-----------------------------------------------------------------------------
DATA metadata(587) / var_metadata(                                             &
! String identifier
    'lake_depth',                                                              &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Lake depth",                                                              &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lake_fetch_gb
!-----------------------------------------------------------------------------
DATA metadata(588) / var_metadata(                                             &
! String identifier
    'lake_fetch_gb',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Typical wind fetch",                                                      &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lake_t_mean_gb
!-----------------------------------------------------------------------------
DATA metadata(589) / var_metadata(                                             &
! String identifier
    'lake_t_mean_gb',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Lake mean temperature",                                                   &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lake_t_mxl_gb
!-----------------------------------------------------------------------------
DATA metadata(590) / var_metadata(                                             &
! String identifier
    'lake_t_mxl_gb',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Lake mixed-layer temperature",                                            &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lake_t_ice_gb
!-----------------------------------------------------------------------------
DATA metadata(591) / var_metadata(                                             &
! String identifier
    'lake_t_ice_gb',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Temperature at upper boundary of lake ice",                               &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lake_h_mxl_gb
!-----------------------------------------------------------------------------
DATA metadata(592) / var_metadata(                                             &
! String identifier
    'lake_h_mxl_gb',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "lake mixed-layer thickness",                                              &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lake_h_ice_gb
!-----------------------------------------------------------------------------
DATA metadata(593) / var_metadata(                                             &
! String identifier
    'lake_h_ice_gb',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Lake ice thickness",                                                      &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lake_shape_factor_gb
!-----------------------------------------------------------------------------
DATA metadata(594) / var_metadata(                                             &
! String identifier
    'lake_shape_factor_gb'   ,                                                 &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Thermocline shape factor",                                                &
! Units
    ""                                                                         &
  ) /
!-----------------------------------------------------------------------------
! Metadata for g_dt_gb
!-----------------------------------------------------------------------------
DATA metadata(595) / var_metadata(                                             &
! String identifier
    'g_dt_gb',                                                                 &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Ground heat flux over delta T for lakes",                                 &
! Units
    "W m-2 K-1"                                                                &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lake_t_sfc_gb
!-----------------------------------------------------------------------------
DATA metadata(596) / var_metadata(                                             &
! String identifier
    'lake_t_sfc_gb',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Surface temperature of the lake",                                         &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lake_t_snow_gb
!-----------------------------------------------------------------------------
DATA metadata(597) / var_metadata(                                             &
! String identifier
    'lake_t_snow_gb',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Temperature of the air-snow interface",                                   &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lake_h_snow_gb
!-----------------------------------------------------------------------------
DATA metadata(598) / var_metadata(                                             &
! String identifier
    'lake_h_snow_gb',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Snow thickness used in FLake calculations",                               &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lake_albedo_gb
!-----------------------------------------------------------------------------
DATA metadata(599) / var_metadata(                                             &
! String identifier
    'lake_albedo_gb',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Lake albedo",                                                             &
! Units
    ""                                                                         &
  ) /
!-----------------------------------------------------------------------------
! Metadata for substr_ch4.
!-----------------------------------------------------------------------------
DATA metadata(600) / var_metadata(                                             &
! String identifier
    'substr_ch4',                                                              &
! Variable type
    var_type_ch4layer,                                                         &
! Long name
    "Dissolved substrate in wetland fraction",                                 &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for mic_ch4.
!-----------------------------------------------------------------------------
DATA metadata(601) / var_metadata(                                             &
! String identifier
    'mic_ch4',                                                                 &
! Variable type
    var_type_ch4layer,                                                         &
! Long name
    "Methanogenic biomass in wetland fraction",                                &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for mic_act_ch4.
!-----------------------------------------------------------------------------
DATA metadata(602) / var_metadata(                                             &
! String identifier
    'mic_act_ch4',                                                             &
! Variable type
    var_type_ch4layer,                                                         &
! Long name
    "Methanogenic biomass activity level in wetland fraction",                 &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for acclim_ch4.
!-----------------------------------------------------------------------------
DATA metadata(603) / var_metadata(                                             &
! String identifier
    'acclim_ch4',                                                              &
! Variable type
    var_type_ch4layer,                                                         &
! Long name
    "Acclimation modifier for methanogen calculations",                        &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for grid_area
!-----------------------------------------------------------------------------
DATA metadata(604) / var_metadata(                                             &
! String identifier
    'grid_area',                                                               &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Area of each gridbox",                                                    &
! Units
    "m2"                                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cs_label_gb
!-----------------------------------------------------------------------------
DATA metadata(605) / var_metadata(                                             &
! String identifier
    'cs_label_gb',                                                             &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox total labelled soil carbon",                                      &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for cs_label
!-----------------------------------------------------------------------------
DATA metadata(606) / var_metadata(                                             &
! String identifier
    'cs_label',                                                                &
! Variable type
    var_type_scpool,                                                           &
! Long name
    "Amount of labelled soil carbon",                                          &
! Units
    "kg m-2"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for resp_label_cs_gb
!-----------------------------------------------------------------------------
DATA metadata(607) / var_metadata(                                             &
! String identifier
    'resp_label_cs_gb',                                                        &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox total labelled soil respiration",                                 &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for resp_label_cs
!-----------------------------------------------------------------------------
DATA metadata(608) / var_metadata(                                             &
! String identifier
    'resp_label_cs',                                                           &
! Variable type
    var_type_scpool,                                                           &
! Long name
    "Amount of labelled soil carbon respired",                                 &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river routing latitude (river routing grid)
!-----------------------------------------------------------------------------
DATA metadata(609) / var_metadata(                                             &
! String identifier
    'rivers_lat_rp',                                                           &
! Variable type
    var_type_rp,                                                               &
! Long name
    "River routing gridbox latitude",                                          &
! Units
    "degrees"                                                                  &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river routing longitude (river routing grid)
!-----------------------------------------------------------------------------
DATA metadata(610) / var_metadata(                                             &
! String identifier
    'rivers_lon_rp',                                                           &
! Variable type
    var_type_rp,                                                               &
! Long name
    "River routing gridbox longitude",                                         &
! Units
    "degrees"                                                                  &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river flow on river routing grid
!-----------------------------------------------------------------------------
DATA metadata(611) / var_metadata(                                             &
! String identifier
    'rflow_rp',                                                                &
! Variable type
    var_type_rp,                                                               &
! Long name
    "River routing gridbox river flow rate",                                   &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river routing grid runoff
!-----------------------------------------------------------------------------
DATA metadata(612) / var_metadata(                                             &
! String identifier
    'rrun_rp',                                                                 &
! Variable type
    var_type_rp,                                                               &
! Long name
    "River routing gridbox runoff rate received by river routing routine",     &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river routing grid surface runoff
!-----------------------------------------------------------------------------
DATA metadata(613) / var_metadata(                                             &
! String identifier
    'rrun_surf_rp',                                                            &
! Variable type
    var_type_rp,                                                               &
! Long name
    "River routing gridbox surface runoff rate received by river " //          &
    "routing routine",                                                         &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river routing grid sub-surface runoff
!-----------------------------------------------------------------------------
DATA metadata(614) / var_metadata(                                             &
! String identifier
    'rrun_sub_surf_rp',                                                        &
! Variable type
    var_type_rp,                                                               &
! Long name
    "River routing gridbox sub-surface runoff rate received by river " //      &
    "routing routine",                                                         &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for grid_area
!-----------------------------------------------------------------------------
DATA metadata(615) / var_metadata(                                             &
! String identifier
    'grid_area_rp',                                                            &
! Variable type
    var_type_rp,                                                               &
! Long name
    "Area of each river routing gridbox",                                      &
! Units
    "m2"                                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for overbank inundation fraction on river routing grid
!-----------------------------------------------------------------------------
DATA metadata(616) / var_metadata(                                             &
! String identifier
    'frac_fplain_rp',                                                          &
! Variable type
    var_type_rp,                                                               &
! Long name
    "Overbank inundation area as a fraction of river routing gridcell area",   &
! Units
    "(fraction of river routing gridcell)"                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for t_home_gb
!-----------------------------------------------------------------------------
DATA metadata(617) / var_metadata(                                             &
! String identifier
    't_home_gb',                                                               &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Home temperature for C3 photosynthesis",                                  &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for tl1_ij_clim_imgn
!-----------------------------------------------------------------------------
DATA metadata(618) / var_metadata(                                             &
! String identifier
    'tl1_ij_clim_imgn',                                                        &
! Variable type
    var_type_imogen_clim,                                                      &
! Long name
    "imogen air temperature climatology",                                      &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for ql1_ij_clim_imgn
!-----------------------------------------------------------------------------
DATA metadata(619) / var_metadata(                                             &
! String identifier
    'ql1_ij_clim_imgn',                                                        &
! Variable type
    var_type_imogen_clim,                                                      &
! Long name
    "imogen relative humidity climatology",                                    &
! Units
    "%/K"                                                                      &
  ) /
!-----------------------------------------------------------------------------
! Metadata for wind_ij_clim_imgn
!-----------------------------------------------------------------------------
DATA metadata(620) / var_metadata(                                             &
! String identifier
    'wind_ij_clim_imgn',                                                       &
! Variable type
    var_type_imogen_clim,                                                      &
! Long name
    "imogen wind climatology",                                                 &
! Units
    "m/s"                                                                      &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lwdown_ij_clim_imgn
!-----------------------------------------------------------------------------
DATA metadata(621) / var_metadata(                                             &
! String identifier
    'lwdown_ij_clim_imgn',                                                     &
! Variable type
    var_type_imogen_clim,                                                      &
! Long name
    "imogen longwave down climatology",                                        &
! Units
    "W/m2"                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for swdown_ij_clim_imgn
!-----------------------------------------------------------------------------
DATA metadata(622) / var_metadata(                                             &
! String identifier
    'swdown_ij_clim_imgn',                                                     &
! Variable type
    var_type_imogen_clim,                                                      &
! Long name
    "imogen shortwave down climatology",                                       &
! Units
    "W/m2"                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for diurnal_tl1_ij_clim_imgn
!-----------------------------------------------------------------------------
DATA metadata(623) / var_metadata(                                             &
! String identifier
    'diurnal_tl1_ij_clim_imgn',                                                &
! Variable type
    var_type_imogen_clim,                                                      &
! Long name
    "imogen diurnal temeprature range climatology",                            &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for precip_ij_clim_imgn
!-----------------------------------------------------------------------------
DATA metadata(624) / var_metadata(                                             &
! String identifier
    'precip_ij_clim_imgn',                                                     &
! Variable type
    var_type_imogen_clim,                                                      &
! Long name
    "imogen precipitation climatology",                                        &
! Units
    "kg/m2/s"                                                                  &
  ) /
!-----------------------------------------------------------------------------
! Metadata for pstar_ij_clim_imgn
!-----------------------------------------------------------------------------
DATA metadata(625) / var_metadata(                                             &
! String identifier
    'pstar_ij_clim_imgn',                                                      &
! Variable type
    var_type_imogen_clim,                                                      &
! Long name
    "imogen surface pressure climatology",                                     &
! Units
    "Pa"                                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for tl1_ij_anom_imgn
!-----------------------------------------------------------------------------
DATA metadata(626) / var_metadata(                                             &
! String identifier
    'tl1_ij_anom_imgn',                                                        &
! Variable type
    var_type_imogen_drive,                                                     &
! Long name
    "imogen air temperature anomaly",                                          &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for ql1_ij_anom_imgn
!-----------------------------------------------------------------------------
DATA metadata(627) / var_metadata(                                             &
! String identifier
    'ql1_ij_anom_imgn',                                                        &
! Variable type
    var_type_imogen_drive,                                                     &
! Long name
    "imogen relative humidity anomaly",                                        &
! Units
    "%"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for wind_ij_anom_imgn
!-----------------------------------------------------------------------------
DATA metadata(628) / var_metadata(                                             &
! String identifier
    'wind_ij_anom_imgn',                                                       &
! Variable type
    var_type_imogen_drive,                                                     &
! Long name
    "imogen wind anomaly",                                                     &
! Units
    "m/s"                                                                      &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lwdown_ij_anom_imgn
!-----------------------------------------------------------------------------
DATA metadata(629) / var_metadata(                                             &
! String identifier
    'lwdown_ij_anom_imgn',                                                     &
! Variable type
    var_type_imogen_drive,                                                     &
! Long name
    "imogen longwave down anomaly",                                            &
! Units
    "W/m2"                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for swdown_ij_anom_imgn
!-----------------------------------------------------------------------------
DATA metadata(630) / var_metadata(                                             &
! String identifier
    'swdown_ij_anom_imgn',                                                     &
! Variable type
    var_type_imogen_drive,                                                     &
! Long name
    "imogen shortwave down anomaly",                                           &
! Units
    "W/m2"                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for diurnal_tl1_ij_anom_imgn
!-----------------------------------------------------------------------------
DATA metadata(631) / var_metadata(                                             &
! String identifier
    'diurnal_tl1_ij_anom_imgn',                                                &
! Variable type
    var_type_imogen_drive,                                                     &
! Long name
    "imogen diurnal air temperature anomaly",                                  &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for precip_ij_anom_imgn
!-----------------------------------------------------------------------------
DATA metadata(632) / var_metadata(                                             &
! String identifier
    'precip_ij_anom_imgn',                                                     &
! Variable type
    var_type_imogen_drive,                                                     &
! Long name
    "precip_ij_anom_imgn",                                                     &
! Units
    "kg/m2/s"                                                                  &
  ) /
!-----------------------------------------------------------------------------
! Metadata for pstar_ij_anom_imgn
!-----------------------------------------------------------------------------
DATA metadata(633) / var_metadata(                                             &
! String identifier
    'pstar_ij_anom_imgn',                                                      &
! Variable type
    var_type_imogen_drive,                                                     &
! Long name
    "imogen surface pressure anomaly",                                         &
! Units
    "Pa"                                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for tl1_ij_patt_imgn
!-----------------------------------------------------------------------------
DATA metadata(634) / var_metadata(                                             &
! String identifier
    'tl1_ij_patt_imgn',                                                        &
! Variable type
    var_type_imogen_drive,                                                     &
! Long name
    "imogen air temperature pattern",                                          &
! Units
    "K/K"                                                                      &
  ) /
!-----------------------------------------------------------------------------
! Metadata for ql1_ij_patt_imgn
!-----------------------------------------------------------------------------
DATA metadata(635) / var_metadata(                                             &
! String identifier
    'ql1_ij_patt_imgn',                                                        &
! Variable type
    var_type_imogen_drive,                                                     &
! Long name
    "imogen relative humidity pattern",                                        &
! Units
    "%/K"                                                                      &
  ) /
!-----------------------------------------------------------------------------
! Metadata for wind_ij_patt_imgn
!-----------------------------------------------------------------------------
DATA metadata(636) / var_metadata(                                             &
! String identifier
    'wind_ij_patt_imgn',                                                       &
! Variable type
    var_type_imogen_drive,                                                     &
! Long name
    "imogen wind pattern",                                                     &
! Units
    "m/s/K"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for lwdown_ij_patt_imgn
!-----------------------------------------------------------------------------
DATA metadata(637) / var_metadata(                                             &
! String identifier
    'lwdown_ij_patt_imgn',                                                     &
! Variable type
    var_type_imogen_drive,                                                     &
! Long name
    "imogen longwave down pattern",                                            &
! Units
    "W/m2/K"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for swdown_ij_patt_imgn
!-----------------------------------------------------------------------------
DATA metadata(638) / var_metadata(                                             &
! String identifier
    'swdown_ij_patt_imgn',                                                     &
! Variable type
    var_type_imogen_drive,                                                     &
! Long name
    "imogen shortwwave down pattern",                                          &
! Units
    "W/m2/K"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for diurnal_tl1_ij_patt_imgn
!-----------------------------------------------------------------------------
DATA metadata(639) / var_metadata(                                             &
! String identifier
    'diurnal_tl1_ij_patt_imgn',                                                &
! Variable type
    var_type_imogen_drive,                                                     &
! Long name
    "imogen diurnal temeprature range pattern",                                &
! Units
    "K/K"                                                                      &
  ) /
!-----------------------------------------------------------------------------
! Metadata for precip_ij_patt_imgn
!-----------------------------------------------------------------------------
DATA metadata(640) / var_metadata(                                             &
! String identifier
    'precip_ij_patt_imgn',                                                     &
! Variable type
    var_type_imogen_drive,                                                     &
! Long name
    "imogen precipitation pattern",                                            &
! Units
    "kg/m2/s/K"                                                                &
  ) /
!-----------------------------------------------------------------------------
! Metadata for pstar_ij_patt_imgn
!-----------------------------------------------------------------------------
DATA metadata(641) / var_metadata(                                             &
! String identifier
    'pstar_ij_patt_imgn',                                                      &
! Variable type
    var_type_imogen_drive,                                                     &
! Long name
    "imogen surface pressure pattern",                                         &
! Units
    "Pa/K"                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for wealth_index
!-----------------------------------------------------------------------------
DATA metadata(642) / var_metadata(                                             &
! String identifier
    'wealth_index',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Wealth Index",                                                            &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for frac_biocrop
!-----------------------------------------------------------------------------
DATA metadata(643) / var_metadata(                                             &
! String identifier
    'frac_biocrop',                                                            &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Fraction of gridcell occupied by biocrops",                               &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for frac_biocrop_prev
!-----------------------------------------------------------------------------
DATA metadata(644) / var_metadata(                                             &
! String identifier
    'frac_biocrop_prev',                                                       &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Fraction of gridcell occupied by biocrops (at previous timestep)",        &
! Units
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for harvest_biocrop
!-----------------------------------------------------------------------------
DATA metadata(645) / var_metadata(                                             &
! String identifier
    'harvest_biocrop',                                                         &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Carbon harvested from biocrops",                                          &
! Units
    "kg C m-2 per 360 days"                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for harvest_biocrop_gb
!-----------------------------------------------------------------------------
DATA metadata(646) / var_metadata(                                             &
! String identifier
    'harvest_biocrop_gb',                                                      &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Carbon harvested from biocrops (gridbox mean)",                           &
! Units
    "kg C m-2 per 360 days"                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for harvest_biocrop_n
!-----------------------------------------------------------------------------
DATA metadata(647) / var_metadata(                                             &
! String identifier
    'harvest_biocrop_n',                                                       &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Nitrogen harvested from biocrops",                                        &
! Units
    "kg N m-2 per 360 days"                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for harvest_biocrop_n_gb
!-----------------------------------------------------------------------------
DATA metadata(648) / var_metadata(                                             &
! String identifier
    'harvest_biocrop_n_gb',                                                    &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Nitrogen harvested from biocrops (gridbox mean)",                         &
! Units
    "kg N m-2 per 360 days"                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for years_since_harvest
!-----------------------------------------------------------------------------
DATA metadata(649) / var_metadata(                                             &
! String identifier
    'years_since_harvest',                                                     &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Years elapsed since most recent (periodic) harvest",                      &
! Units
    "Years"                                                                    &
  ) /
!-----------------------------------------------------------------------------
! Metadata for harvest_doy
!-----------------------------------------------------------------------------
DATA metadata(650) / var_metadata(                                             &
! String identifier
    'harvest_doy',                                                             &
! Variable type
    var_type_pft,                                                              &
! Long name
    "Day of year on which periodic harvest occurs",                            &
! Units
    "Days"                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for plantNumDensity
!-----------------------------------------------------------------------------
DATA metadata(651) / var_metadata(                                             &
! String identifier
    'plantNumDensity',                                                         &
! Variable type
    var_type_nmasst,                                                           &
! Long name
    "Number Density of plants",                                                &
! Units
    "m-2"                                                                      &
  ) /
!-----------------------------------------------------------------------------
! Metadata for atmospheric deposition parameters: Aerodynamic resistance, Ra
!-----------------------------------------------------------------------------
DATA metadata(652)/var_metadata(                                               &
! String identifier
    'deposition_ra',                                                           &
! Variable type
    var_type_surft,                                                            &
! Long name
    "Aerodynamic resistance (Ra) by surface type",                             &
! Units
    "s m-1"                                                                    &
  )/
!-----------------------------------------------------------------------------
! Metadata for atmospheric deposition parameters: Quasi-laminar resistance, Rb
!-----------------------------------------------------------------------------
DATA metadata(653)/var_metadata(                                               &
! String identifier
    'deposition_rb',                                                           &
! Variable type
    var_type_dep_species,                                                      &
! Long name
    "Quasi-laminar resistance (Rb) by deposited chemical species",             &
! Units
    "s m-1"                                                                    &
  )/
!-----------------------------------------------------------------------------
! Metadata for atmospheric deposition parameters: Surface resistance, Rc
!-----------------------------------------------------------------------------
DATA metadata(654)/var_metadata(                                               &
! String identifier
    'deposition_rc',                                                           &
! Variable type
    var_type_surft_dep_species,                                                &
! Long name
    "Surface resistance (Rc) by surface type and deposited chemical species",  &
! Units
    "s m-1"                                                                    &
  )/
!-----------------------------------------------------------------------------
! Metadata for atmospheric deposition parameters:  Surface resistance, Rc
! stomatal component
!-----------------------------------------------------------------------------
DATA metadata(655)/var_metadata(                                               &
! String identifier
    'deposition_rc_stom',                                                      &
! Variable type
    var_type_surft_dep_species,                                                &
! Long name
    "Surface resistance (Rc) - stomatal component, " //                        &
    "by surface type and deposited chemical species",                          &
! Units
    "s m-1"                                                                    &
  )/
!-----------------------------------------------------------------------------
! Metadata for atmospheric deposition parameters: Surface resistance, Rc
! non-stomatal component
!-----------------------------------------------------------------------------
DATA metadata(656)/var_metadata(                                               &
! String identifier
    'deposition_rc_nonstom',                                                   &
! Variable type
    var_type_surft_dep_species,                                                &
! Long name
    "Surface resistance (Rc) - non-stomatal component, " //                    &
    "by surface type and deposited chemical species",                          &
! Units
    "s m-1"                                                                    &
  )/
!-----------------------------------------------------------------------------
! Metadata for atmospheric deposition parameters: Deposition velocity, vd
!-----------------------------------------------------------------------------
DATA metadata(657)/var_metadata(                                               &
! String identifier
    'deposition_vd',                                                           &
! Variable type
    var_type_surft_dep_species,                                                &
! Long name
    "Deposition velocity by surface type and deposited chemical species",      &
! Units
    "m s-1"                                                                    &
  )/
!-----------------------------------------------------------------------------
! Metadata for atmospheric deposition parameters: Deposition velocity, vd
!-----------------------------------------------------------------------------
DATA metadata(658)/var_metadata(                                               &
! String identifier
    'deposition_vd_gb',                                                        &
! Variable type
    var_type_dep_species,                                                      &
! Long name
    "Gridbox deposition velocity by deposited chemical species",               &
! Units
    "m s-1"                                                                    &
  )/
!-----------------------------------------------------------------------------
! Metadata for atmospheric deposition parameters: Deposition loss rate
!-----------------------------------------------------------------------------
DATA metadata(659)/var_metadata(                                               &
! String identifier
    'deposition_lossrate',                                                     &
! Variable type
    var_type_dep_species,                                                      &
! Long name
    "Deposition loss rate by deposited chemical species",                      &
! Units
    "s-1"                                                                      &
  )/
!-----------------------------------------------------------------------------
! Metadata for atmospheric deposition parameters: Deposition flux
!-----------------------------------------------------------------------------
DATA metadata(660)/var_metadata(                                               &
! String identifier
    'deposition_flux',                                                         &
! Variable type
    var_type_dep_species,                                                      &
! Long name
    "Deposition flux by deposited chemical species",                           &
! Units
    "kg m-3 s-1"                                                               &
  )/
!-----------------------------------------------------------------------------
! Metadata for imogen imogen_radf
!-----------------------------------------------------------------------------
DATA metadata(661) / var_metadata(                                             &
! String identifier
    'imogen_radf',                                                             &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Radiative forcing in IMOGEN " //                                          &
    "(repeated global value)",                                                 &
! Units
    "W/m2"                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for imogen dtemp_g
!-----------------------------------------------------------------------------
DATA metadata(662) / var_metadata(                                             &
! String identifier
    'dtemp_g',                                                                 &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Global temperature change in IMOGEN " //                                  &
    "(repeated global value)",                                                 &
! Units
    "K"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for imogen dctot_co2
!-----------------------------------------------------------------------------
DATA metadata(663) / var_metadata(                                             &
! String identifier
    'dctot_co2',                                                               &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Change in C as CO2 in IMOGEN " //                                         &
    "(repeated global value)",                                                 &
! Units
    "kg m-2 per 360days"                                                       &
 )/
!-----------------------------------------------------------------------------
! Metadata for imogen dctot_ch4
!-----------------------------------------------------------------------------
DATA metadata(664) / var_metadata(                                             &
! String identifier
    'dctot_ch4',                                                               &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Change in C as CH4 in IMOGEN " //                                         &
    "(repeated global value)",                                                 &
! Units
    "kg m-2 per 360days"                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for rivers_outflow_number
!-----------------------------------------------------------------------------
DATA metadata(665) / var_metadata(                                             &
! String identifier
    'rivers_outflow_number',                                                   &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for outflow_per_river
!-----------------------------------------------------------------------------
DATA metadata(666) / var_metadata(                                             &
! String identifier
    'outflow_per_river',                                                       &
! Variable type
    var_type_rp,                                                               &
! Long name
    "River outflow into the ocean for each river",                             &
! Units
    "kg s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for rivers_outflow_rp
!-----------------------------------------------------------------------------
DATA metadata(667) / var_metadata(                                             &
! String identifier
    'rivers_outflow_rp',                                                       &
! Variable type
    var_type_rp,                                                               &
! Long name
    "River outflow on river routing grid",                                     &
! Units
    "kg s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for projection_x_coord
!-----------------------------------------------------------------------------
DATA metadata(668) / var_metadata(                                             &
! String identifier
    'projection_x_coord',                                                      &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Projection coordinate in the x direction",                                &
! Units are unknown - they can vary between grids. Make dimensionless.
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for projection_y_coord
!-----------------------------------------------------------------------------
DATA metadata(669) / var_metadata(                                             &
! String identifier
    'projection_y_coord',                                                      &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Projection coordinate in the y direction",                                &
! Units are unknown - they can vary between grids. Make dimensionless.
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for rivers_x_coord_rp
! Not available for input.
!-----------------------------------------------------------------------------
DATA metadata(670) / var_metadata(                                             &
! String identifier
    'rivers_x_coord_rp',                                                       &
! Variable type
    var_type_rp,                                                               &
! Long name
    "Projection coordinate in the x direction for river points",               &
! Units are unknown - they can vary between grids. Make dimensionless.
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for rivers_y_coord_rp
! Not available for input.
!-----------------------------------------------------------------------------
DATA metadata(671) / var_metadata(                                             &
! String identifier
    'rivers_y_coord_rp',                                                       &
! Variable type
    var_type_rp,                                                               &
! Long name
    "Projection coordinate in the y direction for river points",               &
! Units are unknown - they can vary between grids. Make dimensionless.
    "1"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for f_nsc
!-----------------------------------------------------------------------------
DATA metadata(672) / var_metadata(                                             &
! String identifier
    'f_nsc',                                                                   &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT Non-structural carbohydrate mass fraction",                           &
! Units
    "kg kg-1"                                                                  &
  ) /
!-----------------------------------------------------------------------------
! Metadata for growth_sug
!-----------------------------------------------------------------------------
DATA metadata(673) / var_metadata(                                             &
! String identifier
    'growth_sug',                                                              &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT Net growth of structural carbon pool",                                &
! Units
    "kg m-2 s-1"                                                               &
  ) /
!-----------------------------------------------------------------------------
! Metadata for canopy water potential
!-----------------------------------------------------------------------------
DATA metadata(674) / var_metadata(                                             &
! String identifier
    'lwp_c',                                                                   &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT Canopy water potential",                                              &
! Units
    "MPa"                                                                      &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river_nextx_grid
!-----------------------------------------------------------------------------
DATA metadata(675) / var_metadata(                                             &
! String identifier
    'river_nextx_grid',                                                        &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and
! units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river_nexty_grid
!-----------------------------------------------------------------------------
DATA metadata(676) / var_metadata(                                             &
! String identifier
    'river_nexty_grid',                                                        &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and
! units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for channel_depth_grid
!-----------------------------------------------------------------------------
DATA metadata(677) / var_metadata(                                             &
! String identifier
    'channel_depth_grid',                                                      &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and
! units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for channel_width_grid
!-----------------------------------------------------------------------------
DATA metadata(678) / var_metadata(                                             &
! String identifier
    'channel_width_grid',                                                      &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and
! units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for mean_sea_level_grid
!-----------------------------------------------------------------------------
DATA metadata(679) / var_metadata(                                             &
! String identifier
    'mean_sea_level_grid',                                                     &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and
! units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river_distance_grid
!-----------------------------------------------------------------------------
DATA metadata(680) / var_metadata(                                             &
! String identifier
    'river_distance_grid',                                                     &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and
! units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river_elevation_grid
!-----------------------------------------------------------------------------
DATA metadata(681) / var_metadata(                                             &
! String identifier
    'river_elevation_grid',                                                    &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and
! units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river_length_grid
!-----------------------------------------------------------------------------
DATA metadata(682) / var_metadata(                                             &
! String identifier
    'river_length_grid',                                                       &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and
! units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river_manning_grid
!-----------------------------------------------------------------------------
DATA metadata(683) / var_metadata(                                             &
! String identifier
    'river_manning_grid',                                                      &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and
! units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for hypso_quantiles_grid
!-----------------------------------------------------------------------------
DATA metadata(684) / var_metadata(                                             &
! String identifier
    'hypso_quantiles_grid',                                                    &
! Variable type
    var_type_quantile,                                                         &
! Variable is not available for output, so give dummy values for long name and
! units
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river_elevation
!-----------------------------------------------------------------------------
DATA metadata(685) / var_metadata(                                             &
! String identifier
    'river_elevation',                                                         &
! Variable type
    var_type_rp,                                                               &
! Long name
    "Elevation of top of river channel",                                       &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for channel_bed_elevation
!-----------------------------------------------------------------------------
DATA metadata(686) / var_metadata(                                             &
! String identifier
    'channel_bed_elevation',                                                   &
! Variable type
    var_type_rp,                                                               &
! Long name
    "Elevation of bed of river channel",                                       &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for channel_depth
!-----------------------------------------------------------------------------
DATA metadata(687) / var_metadata(                                             &
! String identifier
    'channel_depth',                                                           &
! Variable type
    var_type_rp,                                                               &
! Long name
    "River channel depth (to bankfull level)",                                 &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for channel_width
!-----------------------------------------------------------------------------
DATA metadata(688) / var_metadata(                                             &
! String identifier
    'channel_width',                                                           &
! Variable type
    var_type_rp,                                                               &
! Long name
    "River channel width (at bankfull level)",                                 &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for flood_flow
!-----------------------------------------------------------------------------
DATA metadata(689) / var_metadata(                                             &
! String identifier
    'flood_flow',                                                              &
! Variable type
    var_type_rp,                                                               &
! Long name
    "Flow on floodplain",                                                      &
! Units
    "m3 s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for flood_storage
!-----------------------------------------------------------------------------
DATA metadata(690) / var_metadata(                                             &
! String identifier
    'flood_storage',                                                           &
! Variable type
    var_type_rp,                                                               &
! Long name
    "Water stored on floodplain",                                              &
! Units
    "m3"                                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river_channel_flow
!-----------------------------------------------------------------------------
DATA metadata(691) / var_metadata(                                             &
! String identifier
    'river_channel_flow',                                                      &
! Variable type
    var_type_rp,                                                               &
! Long name
    "Flow in river channel",                                                   &
! Units
    "m3 s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river_channel_storage
!-----------------------------------------------------------------------------
DATA metadata(692) / var_metadata(                                             &
! String identifier
    'river_channel_storage',                                                   &
! Variable type
    var_type_rp,                                                               &
! Long name
    "River channel storage",                                                   &
! Units
    "m3"                                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river_flow
!-----------------------------------------------------------------------------
DATA metadata(693) / var_metadata(                                             &
! String identifier
    'river_flow',                                                              &
! Variable type
    var_type_rp,                                                               &
! Long name
    "River flow (in channel and on floodplain)",                               &
! Units
    "m3 s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sea_level_lp
!-----------------------------------------------------------------------------
DATA metadata(694) / var_metadata(                                             &
! String identifier
    'sea_level_lp',                                                            &
! Variable type
    var_type_surface,                                                          &
! Variable is not available for output, so give dummy values for long name and
! units.
    "", ""                                                                     &
  ) /
!-----------------------------------------------------------------------------
! Metadata for river_depth
!-----------------------------------------------------------------------------
DATA metadata(695) / var_metadata(                                             &
! String identifier
    'river_depth',                                                             &
! Variable type
    var_type_rp,                                                               &
! Long name
    "Depth of water in river channel)",                                        &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for rivers_storage
!-----------------------------------------------------------------------------
DATA metadata(696) / var_metadata(                                             &
! String identifier
    'rivers_storage',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "River routing gridbox river storage",                                     &
! Units
    "kg"                                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fire_em_C2H4_gb
!-----------------------------------------------------------------------------
DATA metadata(697)/var_metadata(                                               &
! String identifier
    'fire_em_C2H4_gb',                                                         &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean C2H4 emission from fires",                                   &
! Units
    "kg m-2 s-1"                                                               &
  )/
!-----------------------------------------------------------------------------
! Metadata for fire_em_C2H4
!-----------------------------------------------------------------------------
DATA metadata(698)/var_metadata(                                               &
! String identifier
    'fire_em_C2H4',                                                            &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT C2H4 emission from fires",                                            &
! Units
    "kg m-2 s-1"                                                               &
  )/
!-----------------------------------------------------------------------------
! Metadata for fire_em_C2H6_gb
!-----------------------------------------------------------------------------
DATA metadata(699)/var_metadata(                                               &
! String identifier
    'fire_em_C2H6_gb',                                                         &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean C2H6 emission from fires",                                   &
! Units
    "kg m-2 s-1"                                                               &
  )/
!-----------------------------------------------------------------------------
! Metadata for fire_em_C2H6
!-----------------------------------------------------------------------------
DATA metadata(700)/var_metadata(                                               &
! String identifier
    'fire_em_C2H6',                                                            &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT C2H6 emission from fires",                                            &
! Units
    "kg m-2 s-1"                                                               &
  )/
!-----------------------------------------------------------------------------
! Metadata for fire_em_C3H8_gb
!-----------------------------------------------------------------------------
DATA metadata(701)/var_metadata(                                               &
! String identifier
    'fire_em_C3H8_gb',                                                         &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean C3H8 emission from fires",                                   &
! Units
    "kg m-2 s-1"                                                               &
  )/
!-----------------------------------------------------------------------------
! Metadata for fire_em_C3H8
!-----------------------------------------------------------------------------
DATA metadata(702)/var_metadata(                                               &
! String identifier
   'fire_em_C3H8',                                                             &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT C3H8 emission from fires",                                            &
! Units
    "kg m-2 s-1"                                                               &
  )/
!-----------------------------------------------------------------------------
! Metadata for fire_em_HCHO_gb
!-----------------------------------------------------------------------------
DATA metadata(703)/var_metadata(                                               &
! String identifier
    'fire_em_HCHO_gb',                                                         &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean HCHO emission from fires",                                   &
! Units
    "kg m-2 s-1"                                                               &
  )/
!-----------------------------------------------------------------------------
! Metadata for fire_em_HCHO
!-----------------------------------------------------------------------------
DATA metadata(704)/var_metadata(                                               &
! String identifier
    'fire_em_HCHO',                                                            &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT HCHO emission from fires",                                            &
! Units
    "kg m-2 s-1"                                                               &
  )/
!-----------------------------------------------------------------------------
! Metadata for fire_em_MeCHO_gb
!-----------------------------------------------------------------------------
DATA metadata(705)/var_metadata(                                               &
! String identifier
    'fire_em_MeCHO_gb',                                                        &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean MeCHO emission from fires",                                  &
! Units
    "kg m-2 s-1"                                                               &
  )/
!-----------------------------------------------------------------------------
! Metadata for fire_em_MeCHO
!-----------------------------------------------------------------------------
DATA metadata(706)/var_metadata(                                               &
! String identifier
    'fire_em_MeCHO',                                                           &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT MeCHO emission from fires",                                           &
! Units
    "kg m-2 s-1"                                                               &
  )/
!-----------------------------------------------------------------------------
! Metadata for fire_em_NH3_gb
!-----------------------------------------------------------------------------
DATA metadata(707)/var_metadata(                                               &
! String identifier
    'fire_em_NH3_gb',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean NH3 emission from fires",                                    &
! Units
    "kg m-2 s-1"                                                               &
  )/
!-----------------------------------------------------------------------------
! Metadata for fire_em_NH3
!-----------------------------------------------------------------------------
DATA metadata(708)/var_metadata(                                               &
! String identifier
    'fire_em_NH3',                                                             &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT NH3 emission from fires",                                             &
! Units
    "kg m-2 s-1"                                                               &
  )/
!-----------------------------------------------------------------------------
! Metadata for fire_em_DMS_gb
!-----------------------------------------------------------------------------
DATA metadata(709)/var_metadata(                                               &
! String identifier
    'fire_em_DMS_gb',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Gridbox mean DMS emission from fires",                                    &
! Units
    "kg m-2 s-1"                                                               &
  )/
!-----------------------------------------------------------------------------
! Metadata for fire_em_DMS
!-----------------------------------------------------------------------------
DATA metadata(710)/var_metadata(                                               &
! String identifier
    'fire_em_DMS',                                                             &
! Variable type
    var_type_pft,                                                              &
! Long name
    "PFT DMS emission from fires",                                             &
! Units
    "kg m-2 s-1"                                                               &
  )/
!-----------------------------------------------------------------------------
! Metadata for unmet_environment
!-----------------------------------------------------------------------------
DATA metadata(711) / var_metadata(                                             &
! String identifier
    'unmet_environment',                                                       &
! Variable type
    var_type_surface,                                                          &
! Long name
    "The part of the demand for water for environmental use that is not " //   &
    "satisfied",                                                               &
! Units
    "kg s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for gw_abstracted
!-----------------------------------------------------------------------------
DATA metadata(712) / var_metadata(                                             &
! String identifier
    'gw_abstracted',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Water abstracted from renewable groundwater sources",                     &
! Units
    "kg s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for gw_avail
!-----------------------------------------------------------------------------
DATA metadata(713) / var_metadata(                                             &
! String identifier
    'gw_avail',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Groundwater that is available for abstraction at start of timestep",      &
! Units
    "kg"                                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for gw_nr_abstracted
!-----------------------------------------------------------------------------
DATA metadata(714) / var_metadata(                                             &
! String identifier
    'gw_nr_abstracted',                                                        &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Water abstracted from non-renewable groundwater sources",                 &
! Units
    "kg s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for unmet_industry
!-----------------------------------------------------------------------------
DATA metadata(715) / var_metadata(                                             &
! String identifier
    'unmet_industry',                                                          &
! Variable type
    var_type_surface,                                                          &
! Long name
    "The part of the demand for water for industrial use that is not " //      &
    "satisfied",                                                               &
! Units
    "kg s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for unmet_irrigation
!-----------------------------------------------------------------------------
DATA metadata(716) / var_metadata(                                             &
! String identifier
    'unmet_irrigation',                                                        &
! Variable type
    var_type_surface,                                                          &
! Long name
    "The part of the demand for water for irrigation that is not satisfied",   &
! Units
    "kg s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for unmet_livestock
!-----------------------------------------------------------------------------
DATA metadata(717) / var_metadata(                                             &
! String identifier
    'unmet_livestock',                                                         &
! Variable type
    var_type_surface,                                                          &
! Long name
    "The part of the demand for water for livestock use that is not "       // &
    "satisfied",                                                               &
! Units
    "kg s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sw_abstracted
!-----------------------------------------------------------------------------
DATA metadata(718) / var_metadata(                                             &
! String identifier
    'sw_abstracted',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Water abstracted from surface water sources",                             &
! Units
    "kg s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for sw_avail
!-----------------------------------------------------------------------------
DATA metadata(719) / var_metadata(                                             &
! String identifier
    'sw_avail',                                                                &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Surface water that is available for abstraction at start of timestep",    &
! Units
    "kg"                                                                       &
  ) /
!-----------------------------------------------------------------------------
! Metadata for unmet_transfers
!-----------------------------------------------------------------------------
DATA metadata(720) / var_metadata(                                             &
! String identifier
    'unmet_transfers',                                                         &
! Variable type
    var_type_surface,                                                          &
! Long name
    "The part of the demand for water for transfers that is not satisfied",    &
! Units
    "kg s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for water_demand_unmet
!-----------------------------------------------------------------------------
DATA metadata(721) / var_metadata(                                             &
! String identifier
    'water_demand_unmet',                                                      &
! Variable type
    var_type_surface,                                                          &
! Long name
    "The part of the total demand for water that is not satisfied",            &
! Units
    "kg s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for water_removed
!-----------------------------------------------------------------------------
DATA metadata(722) / var_metadata(                                             &
! String identifier
    'water_removed',                                                           &
! Variable type
    var_type_surface,                                                          &
! Long name
    "Water that is removed from the system during use",                        &
! Units
    "kg s-1"                                                                   &
  ) /
!-----------------------------------------------------------------------------
! Metadata for timewet_lyr.
!-----------------------------------------------------------------------------
DATA metadata(723) / var_metadata(                                             &
! String identifier
    'timewet_lyr',                                                             &
! Variable type
    var_type_ch4lyrgrid,                                                       &
! Long name
    "Time that a layer has been below the water table",                        &
! Units
    "s"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for ti_local.
!-----------------------------------------------------------------------------
DATA metadata(724) / var_metadata(                                             &
! String identifier
    'ti_local',                                                                &
! Variable type
    var_type_ch4subgrid,                                                       &
! Long name
    "Local topographic index",                                                 &
! Units
    "-"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for zw_local.
!-----------------------------------------------------------------------------
DATA metadata(725) / var_metadata(                                             &
! String identifier
    'zw_local',                                                                &
! Variable type
    var_type_ch4subgrid,                                                       &
! Long name
    "Local water table depth",                                                 &
! Units
    "m"                                                                        &
  ) /
!-----------------------------------------------------------------------------
! Metadata for fch4_local.
!-----------------------------------------------------------------------------
DATA metadata(726) / var_metadata(                                             &
! String identifier
    'fch4_local',                                                              &
! Variable type
    var_type_ch4subgrid,                                                       &
! Long name
    "Local methane emissions",                                                 &
! Units
    "kgC/m2/s"                                                                 &
  ) /
#endif
