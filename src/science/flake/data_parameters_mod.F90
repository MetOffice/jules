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
!  Data module for global parameters
!-------------------------------------------------------------------------------

MODULE data_parameters

!-------------------------------------------------------------------------------
!
! Description:
!  Global parameters for the program are defined.
!
! Current Code Owner: DWD, Ulrich Schaettler
!  phone:  +49  69  8062 2739
!  fax:    +49  69  8236 1493
!  email:  uschaettler@dwd.d400.de
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 1.1        1998/03/11 Ulrich Schaettler
!  Initial release
! 1.8        1998/08/03 Ulrich Schaettler
!  Eliminated intgribf, intgribc, irealgrib, iwlength and put it to data_io.
! 1.10       1998/09/29 Ulrich Schaettler
!  Eliminated parameters for grid point and diagnostic calculations.
! !VERSION!  !DATE!     <Your name>
!  <Modification comments>
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!=======================================================================
!
! Declarations:
!
! Modules used:
!=======================================================================

IMPLICIT NONE

!=======================================================================
! Global (i.e. public) Declarations:
! Parameters for the Program:

INTEGER, PARAMETER       ::                                                    &
     ireals    = SELECTED_REAL_KIND (12,200),                                  &
                   ! number of desired significant digits for
                   ! real variables
                   ! corresponds to 8 byte real variables

     iintegers = KIND  (1)
                   ! kind-type parameter of the integer values
                   ! corresponds to the default integers

!=======================================================================

END MODULE data_parameters

