!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology, 2017.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE vmc_from_head_mod

! Description:
!   Calculate soil moisture content given hydraulic head.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

PRIVATE  ! Private scope by default
PUBLIC vmc_from_head

CONTAINS

!#############################################################################
!#############################################################################

SUBROUTINE vmc_from_head( land_pts, soil_pts, sm_levels, psi,                  &
                          soil_index, b, sathh, smvcst,                        &
                          smvc )

USE jules_soil_mod, ONLY:                                                      &
  ! imported scalars
  l_vg_soil

! Description:
!   Control routine for calculating moisture content from hydraulic head.

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! The number of land points.
  soil_pts,                                                                    &
    ! The number of soil points.
  sm_levels
    ! The number of soil moisture layers.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  psi
    ! Suction pressure (Pa) for which moisture content is required.

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  soil_index(land_pts)
    ! Indices of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  b(land_pts,sm_levels),                                                       &
    ! Exponent for soil moisture characteristic functions.
    ! For the van Genuchten model: b=1/(n-1)
  sathh(land_pts,sm_levels),                                                   &
    ! Parameter for soil moisture characteristic functions,
    ! For Brooks and Corey model, sathh is the saturated soil water
    ! pressure (m).
    ! For the van Genuchten model: sathh=1/alpha (m)
  smvcst(land_pts,sm_levels)
    ! Volumetric soil moisture content at saturation (m3 water/m3 of soil).

!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  smvc(land_pts,sm_levels)
    ! Volumetric soil moisture content corresponding to the given suction
    ! pressure.

!-----------------------------------------------------------------------------
!end of header

IF ( l_vg_soil ) THEN
  CALL vmc_from_head_vg( land_pts, soil_pts, sm_levels, psi,                   &
                         soil_index, b, sathh, smvcst,                         &
                         smvc )
ELSE
  CALL vmc_from_head_bc( land_pts, soil_pts, sm_levels, psi,                   &
                         soil_index, b, sathh, smvcst,                         &
                         smvc)
END IF

RETURN

END SUBROUTINE vmc_from_head

!#############################################################################
!#############################################################################

SUBROUTINE vmc_from_head_vg( land_pts, soil_pts, sm_levels, psi,               &
                             soil_index, b, sathh, smvcst,                     &
                             smvc )

! Description:
!   Calculate soil moisture for a given suction pressue using the van
!   Genuchten model.

USE planet_constants_mod, ONLY:                                                &
  ! imported scalar parameters
  g

USE water_constants_mod, ONLY:                                                 &
  ! imported scalar parameters
  rho_water

!-----------------------------------------------------------------------------
! Description:
!   Calculate soil moisture given suction pressure using the van Genuchten
!   hydraulic model.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! The number of land points.
  soil_pts,                                                                    &
    ! The number of soil points.
 sm_levels
    ! The number of soil moisture layers.

REAL(KIND=real_jlslsm), INTENT(IN) :: psi
    ! Suction pressure (Pa).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN)  :: soil_index(land_pts)
    ! Indices of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  b(land_pts,sm_levels),                                                       &
    ! Exponent for soil moisture characteristic functions.
    ! For the van Genuchten model, b=1/(n-1)
  sathh(land_pts,sm_levels),                                                   &
    ! Parameter for soil moisture characteristic functions.
    ! For the van Genuchten model, sathh=1/alpha (m)
  smvcst(land_pts,sm_levels)
    ! Volumetric soil moisture content at saturation (m3 water/m3 of soil).

!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) :: smvc(land_pts,sm_levels)
    ! Volumetric soil moisture content corresponding to the given suction
    ! pressure.

!-----------------------------------------------------------------------------
! Local scalar parameters.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), PARAMETER :: small_head = EPSILON(psi)
     ! A small suction head, at or below which soil is assumed saturated.

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: i, iz, j  ! Indices and loop counters.
REAL(KIND=real_jlslsm) ::                                                      &
  alpha_vg,                                                                    &
    ! van Genuchten alpha parameter.
  n_vg,                                                                        &
    ! van Genuchten n parameter.
  psi_metres
    ! Suction head (m).

!-----------------------------------------------------------------------------
!end of header

!-----------------------------------------------------------------------------
! Convert suction from Pa to m.
!-----------------------------------------------------------------------------
psi_metres = psi / ( rho_water * g )

!-----------------------------------------------------------------------------
! Calculate moisture content following van Genuchten.
!-----------------------------------------------------------------------------
DO j = 1,soil_pts
  i = soil_index(j)
  DO iz = 1,sm_levels
    IF ( psi_metres > small_head ) THEN
      ! Get the van Genuchten parameters explicitly from the input sathh
      ! and b.
      alpha_vg = 1.0 / sathh(i,iz)
      n_vg     = ( 1.0 / b(i,iz) ) - 1.0
      ! theta - theta_r = (theta_s - theta_r) / (1+(alpha*Psi)^n)^m
      smvc(i,iz) = smvcst(i,iz) /                                              &
                   ( ( 1.0 + ( (alpha_vg * psi_metres)** n_vg ) )**            &
                   ( 1.0 - 1.0 / n_vg ) )
    ELSE
      smvc(i,iz) = smvcst(i,iz)
    END IF
  END DO
END DO

RETURN

END SUBROUTINE vmc_from_head_vg

!#############################################################################
!#############################################################################

SUBROUTINE vmc_from_head_bc( land_pts, soil_pts, sm_levels, psi,               &
                             soil_index, b, sathh, smvcst,                     &
                             smvc )

! Description:
!   Calculate soil moisture for a given suction pressure using the Brooks and
!   Corey model.

USE planet_constants_mod, ONLY:                                                &
  ! imported scalar parameters
  g

USE water_constants_mod, ONLY:                                                 &
  ! imported scalar parameters
  rho_water

!-----------------------------------------------------------------------------
! Description:
!   Calculate soil moisture given suction pressure using the Brooks and Corey
!   hydraulic model.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! The number of land points.
  soil_pts,                                                                    &
    ! The number of soil points.
 sm_levels
    ! The number of soil moisture layers.

REAL(KIND=real_jlslsm), INTENT(IN) :: psi
    ! Suction pressure (Pa).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN)  :: soil_index(land_pts)
    ! Indices of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  b(land_pts,sm_levels),                                                       &
    ! Exponent for soil moisture characteristic functions.
  sathh(land_pts,sm_levels),                                                   &
    ! Saturated soil water suction head (m).
  smvcst(land_pts,sm_levels)
    ! Volumetric soil moisture content at saturation (m3 water/m3 of soil).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) :: smvc(land_pts,sm_levels)
    ! Volumetric soil moisture content corresponding to the given hydraulic
    ! head.

!-----------------------------------------------------------------------------
! Local scalar parameters.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), PARAMETER :: small_head = EPSILON(psi)
     ! A small suction head, at or below which soil is assumed saturated.

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: i   ! index
INTEGER :: iz  ! loop counter
INTEGER :: j   ! loop counter
REAL(KIND=real_jlslsm) :: psi_metres   ! Suction head (m).

!-----------------------------------------------------------------------------
!end of header

!-----------------------------------------------------------------------------
! Convert suction from Pa to m.
!-----------------------------------------------------------------------------
psi_metres = psi / ( rho_water * g )

!-----------------------------------------------------------------------------
! Calculate moisture content following Brooks and Corey.
!-----------------------------------------------------------------------------
DO j = 1,soil_pts
  i = soil_index(j)
  DO iz = 1,sm_levels
    IF ( psi_metres > small_head ) THEN
      smvc(i,iz) =  smvcst(i,iz) *                                             &
                    ( sathh(i,iz) / psi_metres )** ( 1.0 / b(i,iz) )
    ELSE
      smvc(i,iz) = smvcst(i,iz)
    END IF
  END DO
END DO

RETURN

END SUBROUTINE vmc_from_head_bc

!#############################################################################
!#############################################################################

END MODULE vmc_from_head_mod

