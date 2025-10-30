#if defined (UM_JULES)
! *****************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
! *****************************COPYRIGHT****************************************

MODULE riv_concerns

USE regrid_types, ONLY: concern, concern_max, contribution_info

IMPLICIT NONE

!-----------------------------------------------------------------------------
!
! Module: RIV_CONCERNS
!
! Description:
!  Location of persistent data structures needed by
!  the river routing routines to coordinate parallel regridding
!  between atmosphere and river grids.
!
!
! Method:
!   As mentioned data structures below are persistent until the end
!   of a UM run (i.e. allocated but not deallocated, any future
!   modifications should take this into consideration)
!
! Current Code Owner: Please refer to the JULES science module leaders
!   This file belongs in module: Hydrology
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------


TYPE (concern), POINTER :: runoff_send_concern(:) => NULL()
! information for other process's need to regrid
! runoff to trips grid

TYPE (concern), POINTER :: runoff_recv_concern(:) => NULL()
! information this process needs to regrid
! runoff to trips grid

TYPE (concern_max), POINTER :: riverout_send_concern(:) => NULL()
! information for other process's need to regrid
! river outflow to atmos (pressure) grid

TYPE (concern_max), POINTER :: riverout_recv_concern(:) => NULL()
! information this process needs to regrid
! river outflow to atmos (pressure grid)

TYPE (contribution_info), POINTER :: riverout_contribution(:) => NULL()
! convenience data structure for storing regridding information for
! a single target grid point for river outflow regridding


TYPE (concern_max), POINTER :: inlandout_send_concern(:) => NULL()
! information for other process's need to regrid
! inland outflow to atmos (pressure) grid

TYPE (concern_max), POINTER :: inlandout_recv_concern(:) => NULL()
! information this process needs to regrid
! inland outflow to atmos (pressure) grid

TYPE (contribution_info), POINTER :: inlandout_contribution(:) => NULL()
! convenience data structure for storing regridding information for
! a single target grid point for inland outflow regridding

END MODULE riv_concerns
#endif
