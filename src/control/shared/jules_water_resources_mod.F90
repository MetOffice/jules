!******************************COPYRIGHT**************************************
! (c) UK Centre for Ecology & Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE jules_water_resources_mod

USE um_types, ONLY: real_jlslsm

USE missing_data_mod, ONLY:                                                    &
  ! imported scalar parameters
  imdi, rmdi

IMPLICIT NONE

!------------------------------------------------------------------------------
! Description:
!   Contains water resource management options, parameters and variables,
!   and a namelist for setting some of them.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!------------------------------------------------------------------------------

! Public scope by default.

!------------------------------------------------------------------------------
! Module constants
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName = 'JULES_WATER_RESOURCES_MOD'

INTEGER, PARAMETER, PRIVATE ::                                                 &
  name_len = 3,                                                                &
    ! Length of sector names.
  nwater_use_max = 6
    ! Maximum possible number of water sectors.

INTEGER, PARAMETER ::                                                          &
  no_model = 0
    ! Value used to indicate that a sub-model is not used.

! Parameters identifying alternative models for non-renewable groundwater.
! These should have unique values. A value of no_model is used to indicate
! that non-renewable groundwater is not used.
INTEGER, PARAMETER ::                                                          &
  nr_gwater_last = 1,                                                          &
    ! Indicates that non-renewable groundwater is used as a last resort.
  nr_gwater_use = 2
    ! Indicates that non-renewable groundwater is used as part of the mix of
    ! water sources.

! Parameters identifying alternative approaches to calculating the target for
! the fraction of demand that will be met from surface water. These are used
! with the variable partition_method and should have unique values.
INTEGER, PARAMETER ::                                                          &
  partition_ancil = 1,                                                         &
    ! Indicates that a field will be read from an ancilary file.
  partition_calc_from_stores = 2
    ! Indicates that the target will be calculated based on the current
    ! availability of water in surface and groundwater stores.

CHARACTER(LEN=name_len), PARAMETER ::                                          &
  name_domestic = 'dom',                                                       &
    ! Name used to identify domestic use.
  name_environment = 'env',                                                    &
    ! Name used to identify environmental use.
  name_industry = 'ind',                                                       &
    ! Name used to identify industrial use.
  name_irrigation = 'irr',                                                     &
    ! Name used to identify irrigation use.
  name_livestock = 'liv',                                                      &
    ! Name used to identify livestock use.
  name_transfers = 'tra'
    ! Name used to identify water transfers use.

!------------------------------------------------------------------------------
! Module variables
!------------------------------------------------------------------------------

! Items set in namelist jules_water_resources.

! Top-level switch for the parameterisation.
LOGICAL ::                                                                     &
  l_water_resources = .FALSE.
    ! Switch to select water resource management modelling.
    ! .TRUE.  = represent water resources
    ! .FALSE. = no water resources

! Switches that control which sectors are considered.
LOGICAL ::                                                                     &
  l_water_domestic = .FALSE.,                                                  &
    ! .TRUE. =  consider demand for water for domestic use
    ! .FALSE. = do not consider domestic demand
  l_water_environment = .FALSE.,                                               &
    ! .TRUE.  = consider demand for water for environmental flow requirements
    ! .FALSE. = do not consider environmental demand
  l_water_industry = .FALSE.,                                                  &
    ! .TRUE.  = consider demand for water for industrial use
    ! .FALSE. = do not consider industrial demand
  l_water_irrigation = .FALSE.,                                                &
    ! .TRUE.  = consider demand for water for irrigation
    ! .FALSE. = do not consider irrigation demand
    ! In future this will be made consistent with existing irrigation switches
    ! such as l_irrig_dmd; at present there is no conflicting functionality.
  l_water_livestock = .FALSE.,                                                 &
    ! .TRUE.  = consider demand for water for livestock
    ! .FALSE. = do not consider demand from livestock
  l_water_transfers = .FALSE.
    ! .TRUE.  = consider (explicit) water transfers
    ! .FALSE. = do not consider transfers

LOGICAL ::                                                                     &
  l_prioritise = .FALSE.
    ! Switch controlling prioritisation beween demands.
    ! .TRUE.  = rank demands in priority order
    ! .FALSE. = no prioritisation

! Other parameters of the model.
INTEGER ::                                                                     &
  nr_gwater_model = imdi,                                                      &
    ! Chosen model for non-renewable groundwater. Non-renewable groundwater as
    ! defined here is water that is not otherwise explicitly included in the
    ! model. It is an idealised, infinite source of water which is typically
    ! intended to allow consideration of pumping of groundwater from deep
    ! reserves that are difficult to quantify. Any such store might in fact be
    ! renewable (in the sense that it can be recharged) or non-renewable under
    ! particular conditions (e.g. climate).
  nstep_water_res = imdi,                                                      &
    ! Timestep length for water resource model (number of "main model"
    ! timesteps).
  partition_method = imdi
    ! Chosen method for the target for the fraction of demand that will be
    ! met from surface water.

REAL(KIND=real_jlslsm) ::                                                      &
  rf_domestic = rmdi,                                                          &
    ! Fraction of water for domestic use that is returned (via sewage systems
    ! etc.) after abstraction and use.
  rf_industry = rmdi,                                                          &
    ! Fraction of water for industrial use that is returned after abstraction
    ! and use.
  rf_livestock = rmdi,                                                         &
    ! Fraction of water for livestock use that is returned after abstraction
    ! and use.
  sfc_water_factor = rmdi
    ! The weight (a factor) applied to surface water when calculating the
    ! target for the fraction of demand that will be met from surface water.

CHARACTER(LEN=name_len) ::                                                     &
  priority(nwater_use_max) = 'xxx'
    ! Water sector names, in order of decreasing priority.
    ! This fixed-length variable allows for all possible sectors.

!------------------------------------------------------------------------------
! Declare the namelist.
!------------------------------------------------------------------------------
NAMELIST  / jules_water_resources /                                            &
! Shared
    l_prioritise, l_water_domestic, l_water_environment, l_water_industry,     &
    l_water_irrigation, l_water_livestock, l_water_resources,                  &
    l_water_transfers, nr_gwater_model, nstep_water_res, priority,             &
    rf_domestic, rf_industry, rf_livestock, sfc_water_factor,                  &
    partition_method

!------------------------------------------------------------------------------
! Variables below here are not in the namelist.
!------------------------------------------------------------------------------
! Scalar variables.
INTEGER ::                                                                     &
  n_sw_source = imdi,                                                          &
    ! Number of surface water sources.
  nwater_use = imdi,                                                           &
    ! Number of water resource sectors that are considered.
  !----------------------------------------------------------------------------
  ! The following indices should be initialised to zero, to indicate that a
  ! water use is not considered.
  !----------------------------------------------------------------------------
  sw_river_source = 0,                                                         &
    ! Index of river water in surface water source arrays.
  sw_minor_res_source = 0,                                                     &
    ! Index of minor reservoirs in surface water source arrays.
  use_domestic = 0,                                                            &
    ! Index of domestic use in multi-use arrays.
  use_environment = 0,                                                         &
    ! Index of environmental use in multi-use arrays.
  use_industry = 0,                                                            &
    ! Index of industrial use in multi-use arrays.
  use_irrigation = 0,                                                          &
    ! Index of irrigation use in multi-use arrays.
  use_livestock = 0,                                                           &
    ! Index of livestock use in multi-use arrays.
  use_transfers = 0,                                                           &
    ! Index of transfers in multi-use arrays.
  water_res_count = 0
    ! Counter of timesteps done in current water resource timestep.

LOGICAL ::                                                                     &
  l_have_groundwater = .FALSE.,                                                &
    ! Flag indicating if we have a model of groundwater (renewable or
    ! non-renewable).
  l_have_renew_gwater = .FALSE.,                                               &
    ! Flag indicating if we have a model of renewable groundwater.
  l_have_surface_water = .FALSE.
    ! Flag indicating if we have surface water represented (e.g. rivers).
    ! TRUE means n_sw_source > 0.

CONTAINS

!##############################################################################
!##############################################################################

SUBROUTINE check_jules_water_resources( l_top )

USE ereport_mod, ONLY: ereport

USE jules_print_mgr, ONLY: jules_message

! This dependency on jules_rivers_mod is not ideal.
USE jules_rivers_mod, ONLY: l_rivers

!------------------------------------------------------------------------------
! Description:
!   Checks that values in JULES_WATER_RESOURCES namelist were provided and
!   are acceptable.
!------------------------------------------------------------------------------

IMPLICIT NONE

LOGICAL, INTENT(IN) ::                                                         &
  l_top
    ! Switch for TOPMODEL-based hydrology.

!------------------------------------------------------------------------------
! Local scalar parameters.
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER ::                                                 &
   RoutineName = 'CHECK_JULES_WATER_RESOURCES'   ! Name of this procedure.

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  error_status,                                                                &
    ! Error status.
  i
    ! Loop counter.

LOGICAL ::                                                                     &
  in_list
    ! T when a string appears in a list.

!------------------------------------------------------------------------------
! Set error status to show a fatal error.
error_status = 101

! Most actions need be performed only if water resources are selected.
IF ( l_water_resources ) THEN

  !---------------------------------------------------------------------------
  ! Prevent the use of options that are not yet fully coded.
  !---------------------------------------------------------------------------
  IF ( l_water_environment ) THEN
    ! If prescribed these would be better done on river grid, but that requires
    ! new i.o capability. If not prescribed these need to be calculated.
    CALL ereport ( RoutineName, error_status,                                  &
                   "l_water_environment: Environmental flow requirements " //  &
                   "are not available yet.")
  END IF

  IF ( l_water_transfers ) THEN
    ! These require extra i.o that is not yet coded.
    CALL ereport ( RoutineName, error_status,                                  &
                   "l_water_transfers: Water transfers are not available " //  &
                   "yet.")
  END IF

  !----------------------------------------------------------------------------
  ! Check that a timestep was provided and is reasonable.
  !----------------------------------------------------------------------------
  IF ( nstep_water_res == imdi ) THEN
    CALL ereport ( RoutineName, error_status,                                  &
                   "nstep_water_res not found." )
  END IF
  IF ( nstep_water_res < 1 ) THEN
    CALL ereport ( RoutineName, error_status,                                  &
                   "nstep_water_res must be at least 1." )
  END IF

  !----------------------------------------------------------------------------
  ! Count the number of sectors to be considered, and check that at least one
  ! is selected.
  !----------------------------------------------------------------------------
  nwater_use = COUNT( [ l_water_domestic, l_water_environment,                 &
                         l_water_industry, l_water_irrigation,                 &
                         l_water_livestock, l_water_transfers ] )
  IF ( nwater_use == 0 ) THEN
    CALL ereport ( RoutineName, error_status,                                  &
                   "At least one resource sector must be selected." )
  END IF

  IF ( nwater_use > nwater_use_max ) THEN
    CALL ereport ( RoutineName, error_status,                                  &
                   "Increase size of nwater_use_max." )
  END IF

  !----------------------------------------------------------------------------
  ! Check that a return flow value is given for all relevant and active
  ! sectors, and that it is reasonable.
  !----------------------------------------------------------------------------
  IF ( l_water_domestic ) THEN
    IF ( ABS( rf_domestic - rmdi ) < EPSILON(1.0) ) THEN
      CALL ereport ( RoutineName, error_status,                                &
                     "rf_domestic not found." )
    ELSE IF ( rf_domestic < 0.0 .OR. rf_domestic > 1.0 ) THEN
      CALL ereport ( RoutineName, error_status,                                &
                     "rf_domestic must lie in the range 0 to 1." )
    END IF
  END IF

  IF ( l_water_livestock ) THEN
    IF ( ABS( rf_livestock - rmdi ) < EPSILON(1.0) ) THEN
      CALL ereport ( RoutineName, error_status,                                &
                     "rf_livestock not found." )
    ELSE IF ( rf_livestock < 0.0 .OR. rf_livestock > 1.0 ) THEN
      CALL ereport ( RoutineName, error_status,                                &
                     "rf_livestock must lie in the range 0 to 1." )
    END IF
  END IF

  IF ( l_water_industry ) THEN
    IF ( ABS( rf_industry - rmdi ) < EPSILON(1.0) ) THEN
      CALL ereport ( RoutineName, error_status,                                &
                     "rf_industry not found." )
    ELSE IF ( rf_industry < 0.0 .OR. rf_industry > 1.0 ) THEN
      CALL ereport ( RoutineName, error_status,                                &
                     "rf_industry must lie in the range 0 to 1." )
    END IF
  END IF

  !----------------------------------------------------------------------------
  ! Check that the names provided for prioritisation are valid.
  !----------------------------------------------------------------------------
  IF ( l_prioritise ) THEN
    ! Only consider the first nwater_use values.

    ! Check that names are valid.
    DO i = 1,nwater_use
      SELECT CASE ( priority(i) )
      CASE ( name_domestic, name_environment, name_industry, name_irrigation,  &
             name_livestock, name_transfers )
        ! These are valid names, nothing more to do.
      CASE ( 'xxx' )
        CALL ereport ( RoutineName, error_status,                              &
                       "Insufficient values provided for priority - " //       &
                       "each active sector must be listed." )
      CASE ( '' )
        CALL ereport ( RoutineName, error_status,                              &
                       "Priority name empty." )
      CASE DEFAULT
        CALL ereport ( RoutineName, error_status,                              &
                       "Priority name not valid: " // TRIM(priority(i)) )
      END SELECT
    END DO

    ! Check there are no duplicate names.
    DO i = 1,nwater_use-1
      IF ( ANY( priority(i+1:nwater_use) == priority(i) ) ) THEN
        CALL ereport ( RoutineName, error_status,                              &
                       "Duplicate priority: " // TRIM(priority(i)) )
      END IF
    END DO

    !--------------------------------------------------------------------------
    ! Check that all active sectors are given a priority, and inactive sectors
    ! are not given a priority.
    !--------------------------------------------------------------------------
    ! Domestic
    in_list = ANY( priority(1:nwater_use) == name_domestic )
    IF ( l_water_domestic ) THEN
      IF ( .NOT. in_list ) THEN
        CALL ereport ( RoutineName, error_status,                              &
                       "Domestic use is not given a priority." )
      END IF
    ELSE IF ( in_list ) THEN
      CALL ereport ( RoutineName, error_status,                                &
                     "Domestic use is not modelled and so should not " //      &
                     "given a priority." )
    END IF

    ! Environmental
    in_list = ANY( priority(1:nwater_use) == name_environment )
    IF ( l_water_environment ) THEN
      IF ( .NOT. in_list ) THEN
        CALL ereport ( RoutineName, error_status,                              &
                       "Environmental use is not given a priority." )
      END IF
    ELSE IF ( in_list ) THEN
      CALL ereport ( RoutineName, error_status,                                &
                     "Environmental use is not modelled and so should " //     &
                     "not be given a priority." )
    END IF

    ! Industrial
    in_list = ANY( priority(1:nwater_use) == name_industry )
    IF ( l_water_industry ) THEN
      IF ( .NOT. in_list ) THEN
        CALL ereport ( RoutineName, error_status,                              &
                       "Industrial use is not given a priority." )
      END IF
    ELSE IF ( in_list ) THEN
      CALL ereport ( RoutineName, error_status,                                &
                     "Industrial use is not modelled and so should not " //    &
                     "be given a priority." )
    END IF

    ! Irrigation
    in_list = ANY( priority(1:nwater_use) == name_irrigation )
    IF ( l_water_irrigation ) THEN
      IF ( .NOT. in_list ) THEN
        CALL ereport ( RoutineName, error_status,                              &
                       "Irrigation use is not given a priority." )
      END IF
    ELSE IF ( in_list ) THEN
      CALL ereport ( RoutineName, error_status,                                &
                     "Irrigation use is not modelled and so should not " //    &
                     "be given a priority." )
    END IF

    ! Livestock
    in_list = ANY( priority(1:nwater_use) == name_livestock )
    IF ( l_water_livestock ) THEN
      IF ( .NOT. in_list ) THEN
        CALL ereport ( RoutineName, error_status,                              &
                       "Livestock use is not given a priority." )
      END IF
    ELSE IF ( in_list ) THEN
      CALL ereport ( RoutineName, error_status,                                &
                     "Livestock use is not modelled and so should not " //     &
                     "be given a priority." )
    END IF

    ! Transfers
    in_list = ANY( priority(1:nwater_use) == name_transfers )
    IF ( l_water_transfers ) THEN
      IF ( .NOT. in_list ) THEN
        CALL ereport ( RoutineName, error_status,                              &
                       "Water transfers are not given a priority." )
      END IF
    ELSE IF ( in_list ) THEN
      CALL ereport ( RoutineName, error_status,                                &
                     "Water transfers are not modelled and so should not " //  &
                     "be given a priority." )
    END IF

  END IF  !  l_prioritise

  !----------------------------------------------------------------------------
  ! Check option for non-renewable groundwater.
  !----------------------------------------------------------------------------
  SELECT CASE ( nr_gwater_model )
  CASE ( no_model, nr_gwater_last, nr_gwater_use )
    ! Valid options, nothing more to do here.
  CASE ( imdi )
    CALL ereport ( RoutineName, error_status,                                  &
                   "No value given for nr_gwater_model." )
  CASE DEFAULT
    CALL ereport ( RoutineName, error_status,                                  &
                   "Invalid value for nr_gwater_model (non-renewable " //      &
                   "groundwater option)." )
  END SELECT

END IF  !  l_water_resources

!------------------------------------------------------------------------------
! Set further values that follow from settings in the namelist.
! If l_water_resources=F this will ensure that certain switches are set to F.
! This should be called after all namelists have been read to avoid
! assumptions about the calling order - and certainly after the
! jules_hydrology, jules_rivers and jules_water_resources namelists have been
! read. The current location is not ideal in this respect.
!------------------------------------------------------------------------------
CALL set_jules_water_resources( l_top )

!------------------------------------------------------------------------------
! Further checks that use flags set in set_jules_water_resources.
!------------------------------------------------------------------------------
IF ( l_water_resources ) THEN

  ! Water resource modelling requires that either or both of groundwater and
  ! surface water are modelled.
  IF ( .NOT. ( l_have_groundwater .OR. l_have_surface_water ) ) THEN
    error_status = 101  !  a fatal error
    CALL ereport ( RoutineName, error_status,                                  &
                   "Water resources require that either or both of "        // &
                   "groundwater or surface water are modelled.")
  END IF

  ! If we have sources of surface water and groundwater, we need a way to get
  ! the target partitioning.
  IF ( l_have_surface_water .AND. l_have_groundwater ) THEN

    SELECT CASE ( partition_method )
    CASE ( partition_ancil )
      ! A valid option, nothing more to do here.
    CASE ( partition_calc_from_stores )
      ! A further parameter is required.
      IF ( ABS( sfc_water_factor - rmdi ) < EPSILON(1.0) ) THEN
        CALL ereport ( RoutineName, error_status,                              &
                       " sfc_water_factor not found." )
      ELSE IF ( sfc_water_factor < 0.0 ) THEN
        CALL ereport ( RoutineName, error_status,                              &
                       " sfc_water_factor should be >= 0." )
      END IF
    CASE ( imdi )
      CALL ereport ( RoutineName, error_status,                                &
                     "No value given for partition_method." )
    CASE DEFAULT
      CALL ereport ( RoutineName, error_status,                                &
                     "Invalid value for partition_method (approach used "  //  &
                     "to target fraction for surface water)." )
    END SELECT

  ELSE
    ! .NOT. (l_have_surface_water .AND. l_have_groundwater)
    ! i.e. at most one of surface water and groundwater is modelled.

    ! A target partition for surface:groundwater abstraction is only required
    ! if both surface water and groundwater are represented - otherwise it
    ! should have value no_model. (This situation is slightly unusual in that
    ! the user is essentially having to confirm that they realise that this
    ! capability is not required by the current configuration.)
    IF ( partition_method /= no_model .AND.                                    &
         ( .NOT. l_have_surface_water .OR. .NOT. l_have_groundwater ) ) THEN
      WRITE(jules_message,"(A,I0)")                                            &
        "Surface and/or groundwater is not present. partition_method should "//&
        "have value ", no_model
      error_status = 101  !  a fatal error
      CALL ereport ( RoutineName, error_status, jules_message )
    END IF

  END IF  !  l_have_surface_water .AND. l_have_groundwater

  ! If any water use would potentially include return flow to rivers (as coded
  ! in SUBROUTINE calc_return_flow), check that rivers are modelled.
  ! The domestic and industrial uses return water to rivers in preference to
  ! groundwater, while the livestock use returns water to groundwater in
  ! preference to rivers; in all cases we need either rivers or groundwater.
  ! Note that by testing on l_rivers rather than l_have_surface_water we cater
  ! for a potential future extension in which we might have rivers (l_rivers=T)
  ! that are not used as a water source (likely as part of a counter-factual
  ! experiment).
  IF ( .NOT. l_have_groundwater .AND. .NOT. l_rivers ) THEN
    ! We have neither groundwater nor rivers.
    ! Check if a return flow would potentially go to rivers.
    IF ( use_domestic > 0 .OR. use_industry > 0 .OR. use_livestock > 0 ) THEN
      ! This would potentially return to rivers.
      error_status = 101  !  a fatal error
      CALL ereport ( RoutineName, error_status,                                &
                     "Rivers must be included to allow return flow." )
    END IF
  END IF

END IF  !  l_water_resources

END SUBROUTINE check_jules_water_resources

!##############################################################################
!##############################################################################

SUBROUTINE set_jules_water_resources( l_top )

!------------------------------------------------------------------------------
! Description:
!   Sets values related to water resource code.
!------------------------------------------------------------------------------

! This dependency on jules_rivers_mod is not ideal.
USE jules_rivers_mod, ONLY: l_minor_reservoirs, l_rivers

IMPLICIT NONE

LOGICAL, INTENT(IN) ::                                                         &
  l_top
    ! Switch for TOPMODEL-based hydrology.

!------------------------------------------------------------------------------
! Local scalar parameters.
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER ::                                                 &
   RoutineName = 'SET_JULES_WATER_RESOURCES'   ! Name of this procedure.

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  error_status,                                                                &
    ! An error value.
  n
    ! Counters.

!------------------------------------------------------------------------------

IF ( l_water_resources ) THEN

  !----------------------------------------------------------------------------
  ! Set indices for each use to show position in multi-use arrays.
  !----------------------------------------------------------------------------
  n = 0
  IF ( l_water_domestic ) THEN
    n = n + 1
    use_domestic = n
  END IF

  IF ( l_water_environment ) THEN
    n = n + 1
    use_environment = n
  END IF

  IF ( l_water_industry ) THEN
    n = n + 1
    use_industry = n
  END IF

  IF ( l_water_irrigation ) THEN
    n = n + 1
    use_irrigation = n
  END IF

  IF ( l_water_livestock ) THEN
    n = n + 1
    use_livestock = n
  END IF

  IF ( l_water_transfers ) THEN
    n = n + 1
    use_transfers = n
  END IF

  !----------------------------------------------------------------------------
  ! Set index for each available surface water source to show position in
  ! surface water arrays.
  !----------------------------------------------------------------------------
  ! Initialise as no surface water sources.
  n_sw_source = 0
  IF ( l_rivers ) THEN
    n_sw_source     = n_sw_source + 1
    sw_river_source = n_sw_source
  END IF
  IF ( l_minor_reservoirs ) THEN
    n_sw_source         = n_sw_source + 1
    sw_minor_res_source = n_sw_source
  END IF

  ! Set surface water flag if we have any surface water sources.
  IF ( n_sw_source > 0 ) THEN
    l_have_surface_water = .TRUE.
  END IF

  !----------------------------------------------------------------------------
  ! Set flags if groundwater is modelled.
  !----------------------------------------------------------------------------
  ! Renewable groundwater.
  IF ( l_top ) THEN
    l_have_renew_gwater = .TRUE.
  END IF
  ! Any groundwater.
  IF ( l_have_renew_gwater .OR. nr_gwater_model > 0 ) THEN
    l_have_groundwater = .TRUE.
  END IF

ELSE
  ! l_water_resources = .FALSE.

  ! Ensure the sector-specific switches are FALSE (namelists might have set
  ! them to TRUE), to simplify later checking (which doesn't then have to also
  ! check l_water_resources).
  l_water_domestic    = .FALSE.
  l_water_environment = .FALSE.
  l_water_industry    = .FALSE.
  l_water_irrigation  = .FALSE.
  l_water_livestock   = .FALSE.
  l_water_transfers   = .FALSE.

END IF  !  l_water_resources

END SUBROUTINE set_jules_water_resources

!##############################################################################
!##############################################################################

#if defined(UM_JULES) && !defined(LFRIC)

SUBROUTINE print_nlist_jules_water_resources()

USE jules_print_mgr, ONLY: jules_print

IMPLICIT NONE

INTEGER :: i ! loop counter
CHARACTER(LEN=50000) :: lineBuffer

CALL jules_print('jules_water_resources_mod',                                  &
                 'Contents of namelist jules_water_resources')

WRITE(lineBuffer,"(A,L1)") ' l_water_resources = ', l_water_resources
CALL jules_print('jules_water_resources_mod',lineBuffer)

WRITE(lineBuffer,"(A,L1)") ' l_water_domestic = ', l_water_domestic
CALL jules_print('jules_water_resources_mod',lineBuffer)

WRITE(lineBuffer,"(A,L1)") ' l_water_environment = ', l_water_environment
CALL jules_print('jules_water_resources_mod',lineBuffer)

WRITE(lineBuffer,"(A,L1)") ' l_water_industry = ', l_water_industry
CALL jules_print('jules_water_resources_mod',lineBuffer)

WRITE(lineBuffer,"(A,L1)") ' l_water_irrigation = ', l_water_irrigation
CALL jules_print('jules_water_resources_mod',lineBuffer)

WRITE(lineBuffer,"(A,L1)") ' l_water_livestock = ', l_water_livestock
CALL jules_print('jules_water_resources_mod',lineBuffer)

WRITE(lineBuffer,"(A,L1)") ' l_water_transfers = ', l_water_transfers
CALL jules_print('jules_water_resources_mod',lineBuffer)

WRITE(lineBuffer,"(A,L1)") ' l_prioritise = ', l_prioritise
CALL jules_print('jules_water_resources_mod',lineBuffer)

DO i = 1, nwater_use_max
  WRITE(lineBuffer,"(A,I0,A,A)") ' priority(',i,') = ', TRIM( priority(i) )
  CALL jules_print('jules_water_resources_mod',lineBuffer)
END DO

WRITE(lineBuffer,"(A,I0)") ' nr_gwater_model = ', nr_gwater_model
CALL jules_print('jules_water_resources_mod',lineBuffer)

WRITE(lineBuffer,"(A,I0)") ' nstep_water_res = ', nstep_water_res
CALL jules_print('jules_water_resources_mod',lineBuffer)

WRITE(lineBuffer,"(A,G11.4E2)") ' rf_domestic = ', rf_domestic
CALL jules_print('jules_water_resources_mod',lineBuffer)

WRITE(lineBuffer,"(A,G11.4E2)") ' rf_industry = ', rf_industry
CALL jules_print('jules_water_resources_mod',lineBuffer)

WRITE(lineBuffer,"(A,G11.4E2)") ' rf_livestock = ', rf_livestock
CALL jules_print('jules_water_resources_mod',lineBuffer)

WRITE(lineBuffer,"(A,G11.4E2)") ' sfc_water_factor = ', sfc_water_factor
CALL jules_print('jules_water_resources_mod',lineBuffer)

WRITE(lineBuffer,"(A,I0)") ' partition_method = ', partition_method
CALL jules_print('jules_water_resources_mod',lineBuffer)

CALL jules_print('jules_water_resources_mod',                                  &
    '- - - - - - end of namelist - - - - - -')

END SUBROUTINE print_nlist_jules_water_resources

!##############################################################################
!##############################################################################

SUBROUTINE read_nml_jules_water_resources (unitnumber)

! Description:
!  Read the JULES_WATER_RESOURCES namelist

USE setup_namelist,   ONLY: setup_nml_type
USE check_iostat_mod, ONLY: check_iostat
USE UM_parcore,       ONLY: mype


USE parkind1,         ONLY: jprb, jpim
USE yomhook,          ONLY: lhook, dr_hook

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber
INTEGER :: my_comm
INTEGER :: mpl_nml_type
INTEGER :: ErrorStatus
INTEGER :: icode
REAL(KIND=jprb) :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName=                                    &
                                'READ_NML_JULES_WATER_RESOURCES'
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

CHARACTER(LEN=errormessagelength) :: iomessage

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 4
INTEGER, PARAMETER :: n_int = 3
INTEGER, PARAMETER :: n_real = 4
INTEGER, PARAMETER :: n_log = 8
INTEGER, PARAMETER :: n_chars = nwater_use_max * name_len

TYPE :: my_namelist
  SEQUENCE
  INTEGER :: nr_gwater_model
  INTEGER :: nstep_water_res
  INTEGER :: partition_method
  REAL(KIND=real_jlslsm) :: rf_domestic
  REAL(KIND=real_jlslsm) :: rf_livestock
  REAL(KIND=real_jlslsm) :: rf_industry
  REAL(KIND=real_jlslsm) :: sfc_water_factor
  LOGICAL :: l_prioritise
  LOGICAL :: l_water_domestic
  LOGICAL :: l_water_environment
  LOGICAL :: l_water_industry
  LOGICAL :: l_water_irrigation
  LOGICAL :: l_water_livestock
  LOGICAL :: l_water_resources
  LOGICAL :: l_water_transfers
  CHARACTER(LEN=name_len) :: priority(nwater_use_max)
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,                          &
                        zhook_in,zhook_handle)

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type, n_int_in = n_int,               &
                    n_real_in = n_real, n_log_in = n_log,                      &
                    n_chars_in = n_chars)

IF (mype == 0) THEN

  READ (UNIT = unitnumber, NML = jules_water_resources,                        &
        IOSTAT = errorstatus, IOMSG = iomessage)
  CALL check_iostat(errorstatus, "namelist jules_water_resources",             &
                    iomessage)

  my_nml % nr_gwater_model     = nr_gwater_model
  my_nml % nstep_water_res     = nstep_water_res
  my_nml % partition_method    = partition_method
  my_nml % l_prioritise        = l_prioritise
  my_nml % l_water_domestic    = l_water_domestic
  my_nml % l_water_environment = l_water_environment
  my_nml % l_water_industry    = l_water_industry
  my_nml % l_water_irrigation  = l_water_irrigation
  my_nml % l_water_livestock   = l_water_livestock
  my_nml % l_water_resources   = l_water_resources
  my_nml % l_water_transfers   = l_water_transfers
  my_nml % rf_domestic         = rf_domestic
  my_nml % rf_livestock        = rf_livestock
  my_nml % rf_industry         = rf_industry
  my_nml % sfc_water_factor    = sfc_water_factor
  my_nml % priority            = priority

END IF

CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN
  nr_gwater_model     = my_nml % nr_gwater_model
  nstep_water_res     = my_nml % nstep_water_res
  partition_method    = my_nml % partition_method
  l_prioritise        = my_nml % l_prioritise
  l_water_domestic    = my_nml % l_water_domestic
  l_water_environment = my_nml % l_water_environment
  l_water_industry    = my_nml % l_water_industry
  l_water_irrigation  = my_nml % l_water_irrigation
  l_water_livestock   = my_nml % l_water_livestock
  l_water_resources   = my_nml % l_water_resources
  l_water_transfers   = my_nml % l_water_transfers
  rf_domestic         = my_nml % rf_domestic
  rf_livestock        = my_nml % rf_livestock
  rf_industry         = my_nml % rf_industry
  sfc_water_factor    = my_nml % sfc_water_factor
  priority            = my_nml % priority
END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,                          &
                        zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_nml_jules_water_resources

#endif

!##############################################################################

END MODULE jules_water_resources_mod
