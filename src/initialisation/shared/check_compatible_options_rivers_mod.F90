! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************
MODULE check_compatible_options_rivers_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE check_compatible_options_rivers( ERROR )
!-----------------------------------------------------------------------------
! Description:
!   Checks that the enabled Rivers schemes are compatible. Refer to the JULES
!   user manual for more information.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE jules_rivers_mod,            ONLY: i_river_vn, l_rivers, l_riv_overbank,   &
                                       rivers_camaflood, rivers_um_trip,       &
                                       rivers_rfm, rivers_trip,                &
                                       l_inland_outflow
USE jules_model_environment_mod, ONLY: lsm_id, rivers
USE overbank_inundation_mod,     ONLY: overbank_hypsometric, overbank_model,   &
                                       overbank_quantiles, overbank_simple,    &
                                       overbank_simple_rosgen
USE coastal,                     ONLY: l_use_land_fraction

USE jules_print_mgr,             ONLY: jules_print, jules_message

IMPLICIT NONE

! Arguments
INTEGER, INTENT(IN OUT) :: ERROR  ! Error indicator

! Local
CHARACTER(LEN=*), PARAMETER :: routinename='CHECK_COMPATIBLE_OPTIONS_RIVERS'

! Overbank inundation can only be used if rivers are modelled.
IF ( l_riv_overbank .AND. .NOT. l_rivers ) THEN
  ERROR = 1
  CALL jules_print(routinename,                                                &
    "Overbank inundation can only be used if river routing is modelled.")
END IF

! Check that a suitable combination of river routing and overbank models is
! selected.
SELECT CASE ( overbank_model )
CASE ( overbank_simple, overbank_simple_rosgen, overbank_hypsometric )
  ! Diagnostic overbank inundation can currently only be used with RFM river
  ! routing.
  IF ( i_river_vn /= rivers_rfm ) THEN
    ERROR = 1
    CALL jules_print(routinename,                                              &
                     "Diagnostic overbank inundation can only be used "     // &
                     "with RFM river routing.")
  END IF
CASE ( overbank_quantiles )
  ! Only CaMa-Flood can use elevation quantiles.
  IF ( i_river_vn /= rivers_camaflood ) THEN
    ERROR = 1
    CALL jules_print(routinename,                                              &
         "Elevation quantiles can only be used with CaMa-Flood routing.")
  END IF
END SELECT

! While CaMa-Flood routing is in development it can only be used with overbank
! inundation using quantiles.
IF ( l_rivers .AND. i_river_vn == rivers_camaflood .AND.                       &
     ( .NOT. l_riv_overbank .OR. .NOT. overbank_model == overbank_quantiles )  &
   ) THEN
  ERROR = 1
  CALL jules_print(routinename,                                                &
                   "CaMa-Flood routing can only be used with overbank "     // &
                   "inundation using quantiles.")
END IF

! Check land fractions are only used in rivers-standalone when inland outflow
! calculation is requested.
IF ( l_use_land_fraction ) THEN
  IF ( lsm_id /= rivers ) THEN
    ERROR = 1
  ELSE
    SELECT CASE ( i_river_vn )
    CASE ( rivers_trip )
      IF ( .NOT. l_inland_outflow ) ERROR = 1
    CASE DEFAULT
      ERROR = 1
    END SELECT
  END IF
  WRITE(jules_message,'(A,1x,L1,2(1x,A,I0))')                                  &
     "Land fractions (l_use_land_fraction) only used to calculate inland " //  &
     "outflow in Rivers-standalone; l_inland_outflow = ", l_inland_outflow,    &
     "lsm_id = ", lsm_id, "i_river_vn = ", i_river_vn
  CALL jules_print(routinename, jules_message)
END IF

IF ( l_inland_outflow ) THEN
  SELECT CASE ( i_river_vn )
  CASE ( rivers_trip, rivers_um_trip )
    ! Inland outflow calculation allowed for these
  CASE DEFAULT
    ERROR = 1
    WRITE(jules_message,'(A,1x,I0)')                                           &
     "Inland outflow can only be calculated by TRIP; i_river_vn = ", i_river_vn
    CALL jules_print(routinename, jules_message)
  END SELECT
END IF

END SUBROUTINE check_compatible_options_rivers
END MODULE check_compatible_options_rivers_mod

