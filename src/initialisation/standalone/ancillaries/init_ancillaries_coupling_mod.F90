! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_ancillaries_coupling_mod

!-----------------------------------------------------------------------------
! Description:
!  Compares the additional River-related ancillary fields that are used to
!  initialise or provide coupling to other models e.g. ocean model or
!  atmosphere model via OASIS, with the main River routing ancillary to ensure
!  that they are consistent. This is required for QA purposes for LFRic-GC.
!-----------------------------------------------------------------------------

USE logging_mod, ONLY: log_fatal, log_error, log_warn, log_info

IMPLICIT NONE

CONTAINS

!##############################################################################

SUBROUTINE check_ancil_rivers( dir_mouth, dir_inland_drainage,                 &
                               direction_grid,                                 &
                               rivers_outflow_number,                          &
                               rivers_storage, land_fraction_2d )

USE jules_rivers_mod, ONLY: nx_rivers, ny_rivers, n_rivers, i_river_vn,        &
                            rivers_trip, l_inland_outflow,                     &
                            l_outflow_per_river, l_init_storage,               &
                            river_mouth
USE jules_rivers_props_mod, ONLY: l_ignore_ancil_rivers_check
USE coastal, ONLY: l_use_land_fraction
USE missing_data_mod, ONLY: imdi
USE string_utils_mod, ONLY: to_string
USE um_types, ONLY: real_jlslsm
USE jules_print_mgr,  ONLY: jules_message

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  dir_mouth,                                                                   &
    ! The value of the flow direction field that indicates a river mouth.
  dir_inland_drainage
    ! The value of the flow direction field for an inland basin flow point.

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  direction_grid(nx_rivers,ny_rivers)
    ! Flow direction.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  rivers_storage(nx_rivers,ny_rivers)
    ! River routing gridbox river storage (kg).

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  land_fraction_2d(nx_rivers,ny_rivers)
    ! Fraction of land in each grid box

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  rivers_outflow_number(nx_rivers,ny_rivers)
    ! Maps ocean outflow gridboxes to the river they belong to (on rivers
    ! grid).

!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------
INTEGER :: i, ERROR

INTEGER, ALLOCATABLE :: check_river_ancil_test(:,:),                           &
                        ! Field to be checked for consistency
                        check_river_ancil_ctrl(:,:),                           &
                        ! Field used as a control to check for consistency
                        check_land_fractions(:,:),                             &
                        ! Field used to check land fractions for conststency
                        match(:,:)
                        ! Difference between test and control

CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'CHECK_ANCIL_RIVERS'

!end of header
!------------------------------------------------------------------------------

ALLOCATE ( check_river_ancil_test(nx_rivers,ny_rivers) )
ALLOCATE ( check_river_ancil_ctrl(nx_rivers,ny_rivers) )
ALLOCATE ( check_land_fractions(nx_rivers,ny_rivers) )
ALLOCATE ( match(nx_rivers,ny_rivers) )

IF ( l_outflow_per_river ) THEN
  n_rivers = NINT( MAXVAL( rivers_outflow_number(:,:) ) )

  IF ( n_rivers == imdi ) THEN
    jules_message = "Error in River number ancillary. " //                     &
       "Maximum River number is missing data."
    CALL log_fatal( RoutineName, jules_message )
  END IF

  DO i = 1, n_rivers
    IF ( COUNT ( rivers_outflow_number(:,:) == i ) == 0 ) THEN
      jules_message = "River numbers are non-consecutive. " //                 &
         "Missing at least River " // TRIM( to_string(i) )
      CALL log_fatal( RoutineName, jules_message )
    END IF
  END DO

  ! Each grid point with a river outflow number > 0 should be defined in the
  ! river routing ancillary as an outflow (either river mouth or an inland
  ! basin)
  check_river_ancil_test(:,:) = 0
  WHERE ( rivers_outflow_number(:,:) > 0 )
    check_river_ancil_test(:,:) = 1
  END WHERE

  ! The River routing ancillary includes inland basin flow points. The River
  ! number ancillary supports inland basin flow for water conservation and hence
  ! each inland basin has an accompanying River number, although inland basin
  ! water conservation is not currently implemented. When l_inland_outflow=F,
  ! outflow is not calculated for inland basins hence we don't include it in the
  ! outflow per river calculation.
  check_river_ancil_ctrl(:,:) = 0
  WHERE ( direction_grid(:,:) == dir_inland_drainage )
    check_river_ancil_ctrl(:,:) = 1
  END WHERE
  ! Setting the river number to zero for inland basins, excludes them from the
  ! outflow per river calculation.
  IF ( .NOT. l_inland_outflow ) THEN
    CALL log_info(RoutineName,                                                 &
       'Inland basins will not be included in outflow per river ' //           &
       'calculation. l_inland_outflow = F.')
    WHERE ( check_river_ancil_ctrl(:,:) == 1 )
      rivers_outflow_number(:,:) = 0
    END WHERE
  END IF
  WHERE ( direction_grid(:,:) == dir_mouth )
    check_river_ancil_ctrl(:,:) = 1
  END WHERE

  match(:,:) = check_river_ancil_test(:,:) - check_river_ancil_ctrl(:,:)
  ERROR      = COUNT ( match(:,:) /= 0 )

  IF ( ERROR /= 0 ) THEN
    WRITE(jules_message, *)                                                    &
       'Gridpoints with a river outflow number,' //                            &
       ' but not classified as a river outflow      = ',                       &
       COUNT ( match(:,:) > 0 )
    CALL log_error(RoutineName, jules_message)

    WRITE(jules_message, *)                                                    &
       'Gridpoints classified as a river outflow,' //                          &
       ' but does not have a river outflow number = ',                         &
       ABS ( COUNT ( match(:,:) < 0 ) )
    CALL log_error(RoutineName, jules_message)

    WRITE(jules_message,*)                                                     &
       'River routing & number ancillaries are not consistent'

    IF ( l_ignore_ancil_rivers_check ) THEN
      CALL log_warn(RoutineName, jules_message)
      CALL log_warn(RoutineName,                                               &
         'Setting river number to zero where river outflows are not classified.')
      ! Ensures missing data numbers in gridboxes, which are not classified as
      ! river outflows, are not used in outflow_per_river calculation.
      WHERE ( match(:,:) > 0 )
        rivers_outflow_number(:,:) = 0.0
      END WHERE
    ELSE
      CALL log_fatal(RoutineName, jules_message)
    END IF
  ELSE
    CALL log_info(RoutineName,                                                 &
       'River routing & number ancillaries are consistent')
  END IF
END IF

IF ( l_init_storage ) THEN
  ! Where river direction > 0 there should be a river storage value >= 0
  check_river_ancil_ctrl(:,:) = 0
  WHERE ( direction_grid(:,:) > 0 )
    check_river_ancil_ctrl(:,:) = 1
  END WHERE
  check_river_ancil_test(:,:) = 0
  WHERE ( rivers_storage(:,:) >= 0 )
    check_river_ancil_test(:,:) = 1
  END WHERE
  match(:,:) = check_river_ancil_test(:,:) - check_river_ancil_ctrl(:,:)
  ERROR      = COUNT ( match(:,:) /= 0 )

  IF ( ERROR /= 0 ) THEN
    WRITE(jules_message, *)                                                    &
       'River routing & storage ancillary are not consistent. ' //             &
       'Number of inconsistent points  = ', ERROR
    CALL log_error(RoutineName, jules_message)
  ELSE
    CALL log_info(RoutineName,                                                 &
       'River routing & storage ancillaries are consistent')
  END IF
END IF

IF ( l_use_land_fraction) THEN
  ! Make sure there are no land fractions at non-river points.
  check_land_fractions(:,:) = 0
  WHERE ( land_fraction_2d(:,:) > 0 )
    check_land_fractions(:,:) = 1   ! Set points with land fractions to 1
  END WHERE
  WHERE ( direction_grid(:,:) > 0 )
    check_land_fractions(:,:) = 0   ! Set points with rivers back to 0
  END WHERE

  ERROR      = COUNT ( check_land_fractions(:,:) /= 0 )

  IF ( ERROR /= 0 ) THEN
    WRITE(jules_message, *)                                                    &
       'There are some land fractions at non-river points. ' //                &
       'Number of inconsistent points  = ', ERROR
    CALL log_error(RoutineName, jules_message)
  ELSE
    CALL log_info(RoutineName,                                                 &
       'All non-zero land fraction points are modelled by rivers')
  END IF

  ! Make sure there are no river direction points (not outflow)
  ! at zero land fraction points.
  check_land_fractions(:,:) = 0
  WHERE ( direction_grid(:,:) > 0 .AND. direction_grid(:,:) < -1*river_mouth )
    check_land_fractions(:,:) = 1   ! Set points with river directions to 1
  END WHERE
  WHERE ( land_fraction_2d(:,:) > 0 )
    check_land_fractions(:,:) = 0   ! Set points with land fractions back to 0
  END WHERE

  ERROR      = COUNT ( check_land_fractions(:,:) /= 0 )

  IF ( ERROR /= 0 ) THEN
    WRITE(jules_message, *)                                                    &
       'There are some river direction points at zero land fraction points. '//&
       'Number of inconsistent points  = ', ERROR
    CALL log_error(RoutineName, jules_message)
  ELSE
    CALL log_info(RoutineName,                                                 &
       'All river direction points have a non-zero land fraction.')
  END IF
END IF

DEALLOCATE( check_river_ancil_test, check_river_ancil_ctrl, match,             &
            check_land_fractions )

END SUBROUTINE check_ancil_rivers

END MODULE init_ancillaries_coupling_mod
