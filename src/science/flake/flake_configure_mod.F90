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

MODULE flake_configure

!------------------------------------------------------------------------------
!
! Description:
!
!  Switches and reference values of parameters
!  that configure the lake model FLake are set.
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

! GGR 24/8/2007
!----------------
! Have set the following switch to false.
! For info see http://www.flake.igb-berlin.de/usefulhints.shtml


LOGICAL, PARAMETER ::                                                          &
  lflk_botsed_use   = .FALSE.
    ! .TRUE. indicates that the bottom-sediment scheme is used
    ! to compute the depth penetrated by the thermal wave,
    ! the temperature at this depth and the bottom heat flux.
    ! Otherwise, the heat flux at the water-bottom sediment interface
    ! is set to zero, the depth penetrated by the thermal wave
    ! is set to a reference value defined below,
    ! and the temperature at this depth is set to
    ! the temperature of maximum density of the fresh water.

REAL (KIND=ireals), PARAMETER ::                                               &
  rflk_depth_bs_ref = 10.0_ireals
    ! Reference value of the depth of the thermally active
    ! layer of bottom sediments [m].
    ! This value is used to (formally) define
    ! the depth penetrated by the thermal wave
    ! in case the bottom-sediment scheme is not used.

!==============================================================================

END MODULE flake_configure

