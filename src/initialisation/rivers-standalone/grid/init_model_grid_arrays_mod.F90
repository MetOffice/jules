#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_model_grid_arrays_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_model_grid_arrays( psparms_data, ainfo_data,                   &
                                   progs_data, coastal_data,                   &
                                   fluxes_data, rivers_data,                   &
                                   wtrac_jls_data                              &
                                 )

USE allocate_river_arrays_mod, ONLY: allocate_river_arrays

USE fill_model_grid_arrays_mod, ONLY: fill_model_grid_arrays

!TYPE definitions
USE p_s_parms,            ONLY: psparms_data_type
USE ancil_info,           ONLY: ainfo_data_type
USE prognostics,          ONLY: progs_data_type
USE coastal,              ONLY: coastal_data_type
USE fluxes_mod,           ONLY: fluxes_data_type
USE jules_rivers_mod,     ONLY: rivers_data_type
USE jules_wtrac_type_mod, ONLY: jls_wtrac_data_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Allocates Standalone Rivers arrays and fills model grid arrays
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
!Arguments
!TYPES containing field data (IN OUT)
TYPE(psparms_data_type),   INTENT(IN OUT) :: psparms_data
TYPE(ainfo_data_type),     INTENT(IN OUT) :: ainfo_data
TYPE(progs_data_type),     INTENT(IN OUT) :: progs_data
TYPE(coastal_data_type),   INTENT(IN OUT) :: coastal_data
TYPE(fluxes_data_type),    INTENT(IN OUT) :: fluxes_data
TYPE(rivers_data_type),    INTENT(IN OUT) :: rivers_data
TYPE(jls_wtrac_data_type), INTENT(IN OUT) :: wtrac_jls_data

!-----------------------------------------------------------------------------
! Allocate model arrays and fill model arrays with data
!-----------------------------------------------------------------------------

CALL allocate_river_arrays(psparms_data,ainfo_data, progs_data,                &
                           coastal_data,                                       &
                           fluxes_data,                                        &
                           rivers_data,                                        &
                           wtrac_jls_data)

CALL fill_model_grid_arrays( ainfo_data, coastal_data )

RETURN

END SUBROUTINE init_model_grid_arrays

END MODULE init_model_grid_arrays_mod
#endif
