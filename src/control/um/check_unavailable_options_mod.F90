#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt

MODULE check_unavailable_options_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE check_unavailable_options()

USE ereport_mod, ONLY: ereport
USE jules_print_mgr, ONLY:                                                     &
    jules_message, jules_print, PrNorm, newline
USE jules_vegetation_mod, ONLY: l_gleaf_fix, l_o3_damage, l_use_pft_psi,       &
    fsmc_shape, l_prescsow, l_croprotate, l_trif_biocrop, l_ag_expand,         &
    l_sugar, stomata_model, l_red
USE jules_irrig_mod, ONLY: l_irrig_limit
USE jules_urban_mod, ONLY: l_urban_empirical
USE jules_rivers_mod, ONLY: l_riv_overbank, l_rivers, i_river_vn,              &
    rivers_um_trip, rivers_rfm
USE jules_soil_biogeochem_mod, ONLY: l_ch4_microbe, l_label_frac_cs
USE jules_soil_mod, ONLY: l_tile_soil, l_bedrock
USE jules_surface_types_mod, ONLY: ncpft
USE jules_water_resources_mod, ONLY: l_water_resources
USE jules_deposition_mod, ONLY: l_deposition_from_ukca, l_deposition_gc_corr,  &
    dep_h2_soil_scheme, dep_h2_scheme_conrad
USE jules_model_environment_mod, ONLY: lsm_id, cable
USE missing_data_mod, ONLY: imdi

IMPLICIT NONE

!Local variables
INTEGER :: errcode, error_sum
CHARACTER(LEN=*), PARAMETER :: RoutineName='CHECK_UNAVAILABLE_OPTIONS'


error_sum = 0
! jules_model_environment_mod
IF ( lsm_id == cable ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,I0)') error_sum,                                  &
     ": CABLE can only be used in standalone mode. lsm_id = ", lsm_id
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

! jules_urban
! An ereport has not been added for l_cosz as it is .true. by default. If the
! default is changed to .false. it can be added here.
IF ( l_urban_empirical ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A)') error_sum,                                     &
     ": l_urban_empirical should be .false."
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

! jules_rivers
IF ( l_rivers ) THEN
  IF ( i_river_vn /= rivers_um_trip .AND. i_river_vn /= rivers_rfm ) THEN
    error_sum = error_sum + 1
    WRITE(jules_message,'(I0,A,I0)') error_sum,                                &
       ": Only Rivers UM trip (1) or RFM (2) can be run in UM-JULES. " //      &
       "i_river_vn = ", i_river_vn
    CALL jules_print(RoutineName, jules_message, level = PrNorm)
  END IF
END IF

! jules_soil_biogeochem_mod
IF ( l_ch4_microbe ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": Microbial methane scheme is only available to standalone JULES. " //   &
     "l_ch4_microbe = ", l_ch4_microbe
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

! jules_soil_biogeochem_mod
IF ( l_label_frac_cs ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": tracing of soil carbon is only available to standalone JULES. " //     &
     "l_label_frac_cs = ", l_label_frac_cs
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

! jules_soil_mod
IF ( l_tile_soil ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": Soil tiling is only available to standalone JULES. " //                &
     "l_tile_soil = ", l_tile_soil
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

! jules_soil_mod
IF ( l_bedrock ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": Bedrock is only available to standalone JULES. " //                    &
     "l_bedrock = ", l_bedrock
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

! jules_surface_types
IF ( ncpft > 0 ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,I0)') error_sum,                                  &
     ": JULES crop can only be run in standalone. ncpft = ", ncpft
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

! jules_vegetation
IF ( fsmc_shape == 1 ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,I0)') error_sum,                                  &
     ": Piece-wise linear in soil potential (fscm_shape = 1) is only " //      &
     "available to standalone. fscm_shape= ", fsmc_shape
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

! jules_vegetation
IF ( l_sugar ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,I0)') error_sum,                                  &
     ": Sugar only available standalone " //                                   &
     "l_sugar= ", l_sugar
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

! jules_vegetation
IF ( l_red ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": RED only available standalone " //                                     &
     "l_red= ", l_red
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

IF ( l_irrig_limit ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": Limiting irrigation supply is only available to standalone. " //       &
     "l_irrig_limit = ", l_irrig_limit
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

IF ( l_gleaf_fix ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": Bug fix for accumuating g_leaf_phen_acc between calls to " //          &
     "TRIFFID is for standalone only. l_gleaf_fix = ", l_gleaf_fix
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

IF ( l_o3_damage ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": Ozone damage for vegetation is only available to standalone. " //      &
     "l_o3_damage = ", l_o3_damage
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

IF ( l_prescsow ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": Prescribed sowing dates for crops is only available to " //            &
     "standalone. l_prescsow = ", l_prescsow
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

IF ( l_use_pft_psi ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": Use psi_open_io and psi_close_io to calculate the soil moisture " //   &
     "stress function on vegetation is only available to standalone. " //      &
     "l_use_pft_psi = ", l_use_pft_psi
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

IF ( l_croprotate ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": Double cropping is only available to standalone. " //                  &
     "l_croprotate = ", l_croprotate
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

IF ( l_riv_overbank ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": Overbank inundation is only available to standalone. " //              &
     "l_riv_overbank = ", l_riv_overbank
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

IF ( stomata_model == 3 ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,I0)') error_sum,                                  &
     ": SOX stomata model is not currently available in the um " //            &
     "stomata_model= ", stomata_model
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

! jules_water_resources
IF ( l_water_resources ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": Water resource modelling is only available to standalone. " //         &
     "l_water_resources = ", l_water_resources
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

! settings with jules biocrops
IF ( l_trif_biocrop ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": Bioenergy crops are not yet available in the UM. " //                  &
     "l_trif_biocrop = ", l_trif_biocrop
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF
IF ( l_ag_expand ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": Assisted expansion of agricultural crop area is not " //               &
     "available in the UM" //                                                  &
     "l_ag_expand = ", l_ag_expand
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

! Deposition switch settings not available for use in UM-coupled JULES
IF ( .NOT. l_deposition_from_ukca ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": JULES-based deposition routines can only be called from the UKCA. " // &
     "l_deposition_from_ukca must be true but = ", l_deposition_from_ukca
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

IF ( l_deposition_gc_corr ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": Stomatal conductance corrected for bare soil evaporation is not " //   &
     "available in the UKCA. l_deposition_gc_corr = ", l_deposition_gc_corr
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

IF ( dep_h2_soil_scheme /= imdi ) THEN
  IF ( dep_h2_soil_scheme /= dep_h2_scheme_conrad ) THEN
    error_sum = error_sum + 1
    WRITE(jules_message,'(I0,A,I0)') error_sum,                                &
       ": Only the Conrad & Seiler (1, original) H2 soil deposition " //       &
       "scheme is available to the UM. dep_h2_soil_scheme = ",                 &
       dep_h2_soil_scheme
    CALL jules_print(RoutineName, jules_message, level = PrNorm)
  END IF
END IF

IF ( error_sum > 0 ) THEN
  errcode = 30
  WRITE(jules_message,'(A,I0,A)') "One or more JULES options (", error_sum,    &
     ") have been incorrectly set for use in UM-JULES. " //                    &
     "Please see job output for details."
  CALL ereport(RoutineName, errcode, jules_message)
END IF

RETURN
END SUBROUTINE check_unavailable_options
END MODULE check_unavailable_options_mod
#endif
