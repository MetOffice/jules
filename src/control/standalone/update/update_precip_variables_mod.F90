#if !defined(UM_JULES)
!******************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT****************************************

MODULE update_precip_variables_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE update_precip_variables(forcing)

USE update_mod, ONLY: l_daily_disagg, l_disagg_const_rh, use_diff_rad,         &
   io_precip_type, t_for_snow, t_for_con_rain

USE jules_surface_mod, ONLY: l_point_data

USE jules_forcing_mod, ONLY: forcing_type

IMPLICIT NONE

!Arguments
TYPE(forcing_type), INTENT(IN OUT) :: forcing

!-----------------------------------------------------------------------------
! Description:
!   Update precipitation variables
!   What we need to do depends on io_precip_type
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

SELECT CASE ( io_precip_type )
CASE ( 1 )
  ! Total precipitation is given in file and stored in ls_rain_ij

  ! First partition into rain and snow - all snow is assumed to be large-scale
  forcing%ls_snow_ij(:,:) = 0.0
  WHERE ( forcing%tl_1_ij(:,:) <= t_for_snow )
    forcing%ls_snow_ij = forcing%ls_rain_ij
    forcing%ls_rain_ij = 0.0
  END WHERE

  ! Now ls_rain_ij contains total rainfall
  ! If using point data assume all rain is large-scale
  forcing%con_rain_ij(:,:) = 0.0
  IF ( .NOT. l_point_data ) THEN
    ! Otherwise partition into convective and large scale based on t_for_con_rain
    WHERE ( forcing%tl_1_ij(:,:) >= t_for_con_rain )
      forcing%con_rain_ij = forcing%ls_rain_ij
      forcing%ls_rain_ij  = 0.0
    END WHERE
  END IF

  ! Convective snow is assumed to be 0 unless explicitly provided
  forcing%con_snow_ij(:,:) = 0.0

CASE ( 2 )
  ! Total rainfall given in file and currently stored in ls_rain_ij

  ! If using point data assume all rain is large-scale
  forcing%con_rain_ij(:,:) = 0.0
  IF ( .NOT. l_point_data ) THEN
    ! Otherwise partition into convective and large scale based on t_for_con_rain
    WHERE ( forcing%tl_1_ij(:,:) >= t_for_con_rain )
      forcing%con_rain_ij = forcing%ls_rain_ij
      forcing%ls_rain_ij  = 0.0
    END WHERE
  END IF

  ! Total snow was given in file, and all assumed to be large scale

  ! Convective snow is assumed to be 0 unless explicitly provided
  forcing%con_snow_ij(:,:) = 0.0

CASE ( 3 )
  ! Convective and large-scale rain given in file

  ! Total snow was given in file, and all assumed to be large-scale

  ! Convective snow is assumed to be 0 unless explicitly provided
  forcing%con_snow_ij(:,:) = 0.0

CASE ( 4 )
  ! There is nothing to be done for case 4, since all components are input from
  ! file
END SELECT

END SUBROUTINE update_precip_variables
END MODULE update_precip_variables_mod
#endif
