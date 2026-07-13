#if !defined(UM_JULES)
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
USE jules_print_mgr, ONLY: jules_message, jules_print, jules_format,           &
                           PrNorm, newline
USE missing_data_mod, ONLY: imdi
USE jules_hydrology_mod, ONLY: l_var_rainfrac, l_inland
USE jules_surface_mod, ONLY: formdrag, no_drag, i_modiscopt, iscrntdiag,       &
                             srf_ex_cnv_gust, l_vary_z0m_soil
USE jules_surface_types_mod, ONLY: tile_map_ids, ntype
USE jules_rivers_mod,  ONLY: i_river_vn, rivers_um_trip,                       &
                             trip_globe_shape
USE jules_radiation_mod, ONLY: l_sea_alb_var_chl, l_dolr_land_black
USE jules_vegetation_mod, ONLY: l_nrun_mid_trif, l_trif_init_accum
USE jules_deposition_mod, ONLY: l_ukca_dry_dep_so2wet, l_ukca_ddepo3_ocean,    &
                                l_deposition_from_ukca
USE jules_water_tracers_mod, ONLY: l_wtrac_jls

IMPLICIT NONE

!Local variables
INTEGER :: errcode, error_sum
CHARACTER(LEN=*), PARAMETER :: RoutineName='CHECK_UNAVAILABLE_OPTIONS'

error_sum = 0

! jules_hydrology
IF ( l_inland ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": Re-routing inland basin water back to soil moisture is not " //        &
     " required (must not be selected) by standalone. l_inland = ", l_inland
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

! jules_surface
IF ( formdrag /= no_drag ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,I0)') error_sum,                                  &
     ": formdrag should be 0 (i.e. no drag) in standalone. formdrag = ",       &
     formdrag
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF
IF ( i_modiscopt /= 0 ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,I0)') error_sum,                                  &
     ": i_modiscopt should be 0 if forcing with data at a specific " //        &
     "level, rather than a vertical average. The former is" //                 &
     NEW_LINE('A') //                                                          &
     "most likely in standalone JULES. Check that this setting was " //        &
     "intended. If incorrectly set it may cause failures. i_modiscopt = ",     &
     i_modiscopt
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF
IF ( iscrntdiag /= 0 .AND. iscrntdiag /= 1 ) THEN
  ! iscrntdiag = 1 has been allowed as there are Rose stem tests that already
  ! include this, but it not recommended.
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,I0)') error_sum,                                  &
     ": It is recommended that iscrntdiag = 0 in standalone until " //         &
     "driving JULES with a decoupled variable is fully tested." //             &
     NEW_LINE('A') // "iscrntdiag = ", iscrntdiag
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF
IF ( srf_ex_cnv_gust /= 0 ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,I0)') error_sum,                                  &
     ": srf_ex_cnv_gust should be 0 in standlone i.e. the effects of " //      &
     "convective downdraughts on surface exchange cannot be" //                &
     NEW_LINE('A') // "included. srf_ex_cnv_gust = ", srf_ex_cnv_gust
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF
IF ( l_vary_z0m_soil ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": Variable roughness length of bare soil currently not available" //     &
     " to standalone. l_vary_z0m_soil = ", l_vary_z0m_soil
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

! jules_surface_types
IF ( ANY( tile_map_ids(:) > 0 ) ) THEN
  error_sum = error_sum + 1
  WRITE(jules_format,'(A,I0,A)') '(I0,A,',ntype,'(1X,I0))'
  WRITE(jules_message,jules_format) error_sum,                                 &
     ": Tile mapping is only used in the UM recon, so should be unset." //     &
     " tile_map_ids = ", tile_map_ids(1:ntype)
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

! Checks for jules_rivers
IF ( i_river_vn == rivers_um_trip ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,I0)') error_sum,                                  &
     ": Rivers UM trip (1) can only be run in UM-JULES. i_river_vn = ",        &
     i_river_vn
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

IF ( trip_globe_shape /= imdi ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": The shape of the TRIP globe can not be altered in standalone mode." // &
     " trip_globe_shape must not be input. trip_globe_shape = ",               &
     trip_globe_shape
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

! Checks for jules_radiation
IF ( l_sea_alb_var_chl ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": The ancillary plumbling for spatially varying chlorophyll content " // &
     "is not yet available for standalone. l_sea_alb_var_chl = ",              &
     l_sea_alb_var_chl
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

! Checks for jules_vegetation
IF ( l_nrun_mid_trif ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": Start an NRUN mid way through a TRIFFID calling period is not " //     &
     "applicable to standalone. l_nrun_mid_trif =", l_nrun_mid_trif
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

IF ( l_trif_init_accum ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": Start an NRUN resetting accumulated Carbon fluxes to zero " //         &
     "is not applicable to standalone. l_trif_init_accum =", l_trif_init_accum
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

! Deposition switches which are not available for use in JULES standalone
IF ( l_deposition_from_ukca ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": Deposition switch l_deposition_from_ukca_has to be false for " //      &
     "standalone, as the calling UKCA routines are not available. " //         &
     "l_deposition_from_ukca =", l_deposition_from_ukca
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

IF ( l_ukca_ddepo3_ocean ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": Deposition switch l_ukca_ddepo3_ocean is not applicable to " //        &
     "standalone. l_ukca_ddepo3_ocean =", l_ukca_ddepo3_ocean
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

IF ( l_ukca_dry_dep_so2wet ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": Deposition switch l_ukca_dry_dep_so2wet is not applicable to " //      &
     "standalone. l_ukca_dry_dep_so2wet =", l_ukca_dry_dep_so2wet
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

IF ( l_wtrac_jls ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                  &
     ": Water tracers cannot be run in standalone mode. " //                   &
     "l_wtrac_jls =", l_wtrac_jls
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

! Defining errors ends here. Now issue FATAL ereport.
IF ( error_sum > 0 ) THEN
  errcode = 10
  WRITE(jules_message,'(A,I0,A)') "One or more JULES options (", error_sum,    &
     ") have been incorrectly set for use in JULES standalone." //             &
     NEW_LINE('A') // "Please see job output for details."
  CALL ereport(RoutineName, errcode, jules_message)
END IF

! Warnings for information
IF ( iscrntdiag /= 0 ) THEN
  ! iscrntdiag = 1 has been allowed as there are Rose stem tests that already
  ! include this, but it not recommended.
  errcode = -10
  WRITE(jules_message,'(A,I0)')                                                &
     "It is recommended that iscrntdiag = 0 in standalone until " //           &
     "driving JULES with a decoupled variable is fully tested." //             &
     NEW_LINE('A') // "iscrntdiag = ", iscrntdiag
  CALL ereport(RoutineName, errcode, jules_message)
END IF

IF ( l_dolr_land_black ) THEN
  errcode = -10
  WRITE(jules_message,'(A,L1)')                                                &
     " This has no effect in JULES standalone, but it may indicate that " //   &
     "something else is wrong. Was this intended? l_dolr_land_black = ",       &
     l_dolr_land_black
  CALL ereport(RoutineName, errcode, jules_message)
END IF

! jules_hydrology
IF ( l_var_rainfrac ) THEN
  ! This should be a fatal ereport but configurations with l_var_rainfrac=T
  ! exist in the Rose stem tests.
  errcode = -10
  WRITE(jules_message,'(A,L1)')                                                &
     " The use of variable large scale and convective rain fractions" //       &
     " should not be available to standalone as the" //                        &
     newline //                                                                &
     "                       functionality to supply the large-scale and" //   &
     " convective rain fractions via the driving data does not exist." //      &
     newline //                                                                &
     "                       l_var_rainfrac = T is equivalent to" //           &
     " l_var_rainfrac = F with 0.5 for large-scale and" //                     &
     " 'jules_soil::confrac' for convective rain."
  CALL ereport(RoutineName, errcode, jules_message)
END IF

RETURN
END SUBROUTINE check_unavailable_options
END MODULE check_unavailable_options_mod
#endif
