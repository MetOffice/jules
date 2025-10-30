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

MODULE flake_albedo_ref

!------------------------------------------------------------------------------
!
! Description:
!
!  This module contains "reference" values of albedo
!  for the lake water, lake ice and snow.
!  As in "flake_paramoptic_ref", two ice categories, viz. white ice and blue ice,
!  and two snow categories, viz. dry snow and melting snow, are used.
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

!==============================================================================

IMPLICIT NONE

!==============================================================================
!
! Declarations

!  Albedo for water, ice and snow.
REAL (KIND=ireals), PARAMETER ::                                               &
  albedo_water_ref       = 0.07_ireals  , & ! Water
  albedo_whiteice_ref    = 0.60_ireals  , & ! White ice
  albedo_blueice_ref     = 0.10_ireals  , & ! Blue ice
  albedo_drysnow_ref     = 0.60_ireals  , & ! Dry snow
  albedo_meltingsnow_ref = 0.10_ireals      ! Melting snow

!  Empirical parameters.
REAL (KIND=ireals), PARAMETER ::                                               &
  c_albice_MR = 95.6_ireals          ! Constant in the interpolation formula for
                                     ! the ice albedo (Mironov and Ritter 2004)

!==============================================================================

END MODULE flake_albedo_ref

