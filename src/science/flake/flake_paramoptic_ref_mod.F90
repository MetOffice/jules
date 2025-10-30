!-----------------------------------------------------------
! FLake is freely available under the terms of the MIT license.
!
! Copyright (c)
!
! Permission is hereby granted, free of charge, to any person obtaining
! a copy of this software and associated documentation files (the
! "Software"), to deal in the Software without restriction, including
! without limitation the rights to use, copy, modify, merge, publish,
! distribute, sublicense, and/or sell copies of the Software, and to
! permit persons to whom the Software is furnished to do so, subject to
! the following conditions:
!
! The above copyright notice and this permission notice shall be
! included in all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
! EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
! MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
! NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
! LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
! OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
! WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!-----------------------------------------------------------
! File %M% from Library %Q%
! Version %I% from %G% extracted: %H%
!------------------------------------------------------------------------------

MODULE flake_paramoptic_ref

!------------------------------------------------------------------------------
!
! Description:
!
!  This module contains "reference" values of the optical characteristics
!  of the lake water, lake ice and snow. These reference values may be used
!  if no information about the optical characteristics of the lake in question
!  is available. An exponential decay law for the solar radiation flux is assumed.
!  In the simplest one-band approximation,
!  the extinction coefficient for water is set to a large value,
!  leading to the absorption of 95% of the incoming radiation
!  within the uppermost 1 m of the lake water.
!  The extinction coefficients for ice and snow are taken from
!  Launiainen and Cheng (1998). The estimates for the ice correspond
!  to the uppermost 0.1 m of the ice layer and to the clear sky conditions
!  (see Table 2 in op. cit.).
!  Very large values of the extinction coefficients for ice and snow ("opaque")
!  can be used to prevent penetration of the solar radiation
!  through the snow-ice cover.
!
!
! Current Code Owner: DWD, Dmitrii Mironov
!  Phone:  +49-69-8062 2705
!  Fax:    +49-69-8062 3721
!  e-mail: dmitrii.mironov@dwd.de
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 1.00       2005/11/17 Dmitrii Mironov
!  Initial release
! !VERSION!  !DATE!     <Your name>
!  <Modification comments>
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================
!
! Declarations:
!
! Modules used:

USE data_parameters, ONLY:                                                     &
  ireals                       , & ! KIND-type parameter for real variables
  iintegers                        ! KIND-type parameter for "normal" integer variables

USE flake_derivedtypes, ONLY:                                                  &
  nband_optic_max              , & ! Maximum value of the wave-length bands
  opticpar_medium                  ! Derived TYPE

!==============================================================================

IMPLICIT NONE

!==============================================================================
!
! Declarations

INTEGER (KIND=iintegers), PRIVATE :: & ! Help variable(s)
  i                                      ! DO loop index

!  Optical characteristics for water, ice and snow.
!  The simplest one-band approximation is used as a reference.
TYPE (opticpar_medium), PARAMETER ::                                           &
  opticpar_water_ref = opticpar_medium(1,                                      &
                       ! Water (reference)
    [1.0_ireals, (0.0_ireals,i = 2,nband_optic_max) ],                         &
    [3.0_ireals, (1.0e10_ireals,i = 2,nband_optic_max) ]),                     &
  opticpar_water_trans = opticpar_medium(2,                                    &
                         ! Transparent Water (two-band)
    [0.10_ireals, 0.90_ireals, (0.0_ireals,i = 3,nband_optic_max) ],           &
    [2.0_ireals, 0.20_ireals, (1.0e10_ireals,i = 3,nband_optic_max) ]) ,       &
!_nu  opticpar_water_trans = opticpar_medium(1,                    & ! Transparent Water (one-band)
!_nu    (/1.0_ireals, (0.0_ireals,i=2,nband_optic_max)/),            &
!_nu    (/0.30_ireals, (1.0e10_ireals,i=2,nband_optic_max)/))    , &
  opticpar_whiteice_ref = opticpar_medium(1,                                   &
                         ! White ice
    [1.0_ireals, (0.0_ireals,i = 2,nband_optic_max) ],                         &
    [17.1_ireals, (1.0e10_ireals,i = 2,nband_optic_max) ]),                    &
  opticpar_blueice_ref = opticpar_medium(1,                                    &
                         ! Blue ice
    [1.0_ireals, (0.0_ireals,i = 2,nband_optic_max) ],                         &
    [8.4_ireals, (1.0e10_ireals,i = 2,nband_optic_max) ]),                     &
  opticpar_drysnow_ref = opticpar_medium(1,                                    &
                         ! Dry snow
    [1.0_ireals, (0.0_ireals,i = 2,nband_optic_max) ],                         &
    [25.0_ireals, (1.0e10_ireals,i = 2,nband_optic_max) ]),                    &
  opticpar_meltingsnow_ref = opticpar_medium(1,                                &
                             ! Melting snow
    [1.0_ireals, (0.0_ireals,i = 2,nband_optic_max) ],                         &
    [15.0_ireals, (1.0e10_ireals,i = 2,nband_optic_max) ]),                    &
  opticpar_ice_opaque = opticpar_medium(1,                                     &
                        ! Opaque ice
    [1.0_ireals, (0.0_ireals,i = 2,nband_optic_max) ],                         &
    [1.0e07_ireals, (1.0e10_ireals,i = 2,nband_optic_max) ]),                  &
  opticpar_snow_opaque = opticpar_medium(1,                                    &
                         ! Opaque snow
    [1.0_ireals, (0.0_ireals,i = 2,nband_optic_max) ],                         &
    [1.0e07_ireals, (1.0e10_ireals,i = 2,nband_optic_max) ])

!==============================================================================

END MODULE flake_paramoptic_ref

