#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE spinup_init_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE spinup_init(progs,trifctltype)

USE ancil_info, ONLY:                                                          &
  ! imported scalars (IN)
  dim_cslayer, land_pts, nsoilt

USE jules_soil_biogeochem_mod, ONLY:                                           &
  ! imported scalar parameters
  soil_model_ecosse, soil_model_4pool,                                         &
  ! imported scalars (IN)
  soil_bgc_model


USE jules_soil_mod, ONLY:                                                      &
  ! imported scalars (IN)
  sm_levels

USE jules_vegetation_mod, ONLY:                                                &
  ! imported scalars (IN)
  l_triffid

! TYPE Definitions
USE trifctl, ONLY: trifctl_type

!TYPE definitions
USE prognostics, ONLY: progs_type

USE spinup_mod, ONLY: nvars, spinup_vars

USE logging_mod, ONLY: log_info, log_fatal

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Allocates variables required for spinup and stores initial state
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
! TYPE Definitions
TYPE(trifctl_type), INTENT(IN OUT) :: trifctltype

! Arguments

!TYPES containing the data
TYPE(progs_type), INTENT(IN OUT) :: progs

! Local parameters
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'spinup_init'

! Work variables
INTEGER :: i  ! Loop counter

!-----------------------------------------------------------------------------
!end of header

!-----------------------------------------------------------------------------
! Allocate space to store the state of the spinup variables for comparison
!-----------------------------------------------------------------------------
DO i = 1,nvars

  !-------------------------------------------------------------------------
  ! Variables that are available in all configurations.
  !-------------------------------------------------------------------------
  SELECT CASE ( spinup_vars(i)%identifier )
  CASE ( 'smcl' )
    ALLOCATE(spinup_vars(i)%DATA(land_pts,nsoilt,sm_levels))
    spinup_vars(i)%DATA(:,:,:) = progs%smcl_soilt(:,:,:)

  CASE ( 't_soil' )
    ALLOCATE(spinup_vars(i)%DATA(land_pts,nsoilt,sm_levels))
    spinup_vars(i)%DATA(:,:,:) = progs%t_soil_soilt(:,:,:)

    !-------------------------------------------------------------------------
    ! Variables that are only available with TRIFFID.
    !-------------------------------------------------------------------------
  CASE ( 'c_veg' )
    IF ( .NOT. l_triffid ) THEN
      CALL log_fatal( RoutineName,                                             &
                      TRIM(spinup_vars(i)%identifier)               //         &
                      " can only be used to determine spin up if"   //         &
                      " l_triffid=.TRUE." )
    END IF
    ALLOCATE( spinup_vars(i)%DATA(land_pts,1,1) )
    spinup_vars(i)%DATA(:,1,1) = trifctltype%cv_gb(:)

    !------------------------------------------------------------------------
    ! Variables that are only available with 4-pool C model or ECOSSE.
    !------------------------------------------------------------------------
  CASE ( 'c_soil' )
    ! Total C in each layer.
    SELECT CASE ( soil_bgc_model )
    CASE ( soil_model_ecosse, soil_model_4pool )
      ! OK. Nothing to do.
    CASE DEFAULT
      CALL log_fatal( RoutineName,                                             &
                      TRIM(spinup_vars(i)%identifier)             //           &
                      " can only be used to determine spin up if" //           &
                      " the 4-pool or ECOSSE model is used." )
    END SELECT
    ALLOCATE(spinup_vars(i)%DATA(land_pts,nsoilt,dim_cslayer))
    ! Sum the C pools in each layer.
    spinup_vars(i)%DATA(:,:,:) = SUM( progs%cs_pool_soilt(:,:,:,:), 4 )

  CASE DEFAULT
    CALL log_fatal(RoutineName,                                                &
                   "Unrecognised variable for spinup - " //                    &
                   TRIM(spinup_vars(i)%identifier))
  END SELECT
END DO

RETURN

END SUBROUTINE spinup_init
END MODULE spinup_init_mod
#endif
