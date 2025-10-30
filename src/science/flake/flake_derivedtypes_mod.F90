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

MODULE flake_derivedtypes

!------------------------------------------------------------------------------
!
! Description:
!
!  Derived type(s) is(are) defined.
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

USE data_parameters , ONLY:                                                    &
  ireals                   , & ! KIND-type parameter for real variables
  iintegers                    ! KIND-type parameter for "normal"
                               ! integer variables

!==============================================================================

IMPLICIT NONE

!==============================================================================
!
! Declarations

!  Maximum value of the wave-length bands
!  in the exponential decay law for the radiation flux.
!  A storage for a ten-band approximation is allocated,
!  although a smaller number of bands is actually used.
INTEGER (KIND=iintegers), PARAMETER ::                                         &
  nband_optic_max = 10_iintegers

!  Define TYPE "opticpar_medium"
TYPE :: opticpar_medium
  INTEGER (KIND=iintegers)                        ::                           &
    nband_optic
      ! Number of wave-length bands
  REAL (KIND=ireals), DIMENSION (nband_optic_max) ::                           &
    frac_optic,                                                                &
      ! Fractions of total radiation flux
    extincoef_optic
      ! Extinction coefficients
END TYPE opticpar_medium

!==============================================================================

END MODULE flake_derivedtypes

