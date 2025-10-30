#if !defined(UM_JULES) && defined(MPI_DUMMY)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE mpi

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This module contains just the variable definitions from a "real" mpi
!   module that JULES needs to compile against
!
! Current Code Owner: Kerry Smout-Day
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

  INTEGER, PARAMETER :: mpi_comm_world = 1
  INTEGER, PARAMETER :: mpi_real = 1
  INTEGER, PARAMETER :: mpi_integer = 1
  INTEGER, PARAMETER :: mpi_logical = 1
  INTEGER, PARAMETER :: mpi_info_null = 1
  INTEGER, PARAMETER :: mpi_land = 1

! Setting this to 1 means use the default INTEGER size
  INTEGER, PARAMETER :: mpi_address_kind = 1

END MODULE mpi
#endif
