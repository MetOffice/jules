#if !defined(UM_JULES) && defined(MPI_DUMMY)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


!-----------------------------------------------------------------------------
! Description:
!   This file provides dummy MPI procedures that just do any work required
!   to run in serial
!
!   NOTE: These routines are separate from the MPI module on purpose, since
!         this is how a "real" MPI library works
!         Constants and variables are either MODULE or #include, routines
!         are in a library that is linked at link-time
!
! Current Code Owner: Kerry Smout-Day
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

SUBROUTINE MPI_INIT(error)

  IMPLICIT NONE

  INTEGER, INTENT(OUT) :: error

! This procedure has nothing to do

  error = 0

  RETURN

END SUBROUTINE MPI_INIT



SUBROUTINE MPI_FINALIZE(error)

  IMPLICIT NONE

  INTEGER, INTENT(OUT) :: error

! This procedure has nothing to do

  error = 0

  RETURN

END SUBROUTINE MPI_FINALIZE



SUBROUTINE MPI_ABORT(comm, errorcode, error)

  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: errorcode
  INTEGER, INTENT(OUT) :: error

  ! All this procedure needs to do is terminate the current process
  ! with a non-zero exit code. We use the C intrinsic in order to
  ! ensure portability.
  INTERFACE
    SUBROUTINE c_exit(status) BIND(c, NAME="exit")
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(KIND=C_INT), VALUE, INTENT(IN) :: status
    END SUBROUTINE c_exit
  END INTERFACE
  error = 1
  CALL c_exit(1_C_INT)

  RETURN

END SUBROUTINE MPI_ABORT



SUBROUTINE mpi_comm_size(comm, size, error)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: comm
  INTEGER, INTENT(OUT) :: size
  INTEGER, INTENT(OUT) :: error

! In serial mode, the size is always 1
  size  = 1
  error = 0

  RETURN

END SUBROUTINE mpi_comm_size



SUBROUTINE mpi_comm_rank(comm, rank, error)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: comm
  INTEGER, INTENT(OUT) :: rank
  INTEGER, INTENT(OUT) :: error

! In serial mode, the rank is always 0
  rank  = 0
  error = 0

  RETURN

END SUBROUTINE mpi_comm_rank



SUBROUTINE mpi_scatterv(sendbuf, sendcnts, displs, sendtype,                  &
                        recvbuf, recvcnt, recvtype,                           &
                        root, comm, error)

  IMPLICIT NONE

  REAL, INTENT(IN) :: sendbuf(:,:)
  INTEGER, INTENT(IN) :: sendcnts(:)
  INTEGER, INTENT(IN) :: displs(:)
  INTEGER, INTENT(IN) :: sendtype

  REAL, INTENT(OUT) :: recvbuf(:,:)
  INTEGER, INTENT(IN) :: recvcnt
  INTEGER, INTENT(IN) :: recvtype

  INTEGER, INTENT(IN) :: root
  INTEGER, INTENT(IN) :: comm
  INTEGER, INTENT(OUT) :: error

! This routine ignores all count and offset information and just copies all
! the data from sendbuf into recvbuf
! At the moment, this routine only exists for 2D REAL arrays, which is all JULES uses
! it for

! The input and output buffers must be the same size
  IF ( SIZE(sendbuf) /= SIZE(recvbuf) ) THEN
    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy MPI procedure for non-serial work'
    WRITE(*,*) 'To use MPI routines for parallel execution, recompile using an MPI compiler'

    CALL MPI_ABORT(comm, 1, error)
  END IF

! Just copy the data from the input buffer to the output buffer
  recvbuf(:,:) = sendbuf(:,:)

  error = 0

  RETURN

END SUBROUTINE mpi_scatterv



SUBROUTINE mpi_gatherv(sendbuf, sendcnt, sendtype,                             &
                       recvbuf, recvcnts, displs, recvtype,                    &
                       root, comm, error)

  IMPLICIT NONE

  REAL, INTENT(IN) :: sendbuf(:,:)
  INTEGER, INTENT(IN) :: sendcnt
  INTEGER, INTENT(IN) :: sendtype

  REAL, INTENT(OUT) :: recvbuf(:,:)
  INTEGER, INTENT(IN) :: recvcnts(:)
  INTEGER, INTENT(IN) :: displs(:)
  INTEGER, INTENT(IN) :: recvtype

  INTEGER, INTENT(IN) :: root
  INTEGER, INTENT(IN) :: comm
  INTEGER, INTENT(OUT) :: error

! This routine ignores all count and offset information and just copies all
! the data from sendbuf into recvbuf
! At the moment, this routine only exists for 2D REAL arrays, which is all JULES uses
! it for

! The input and output buffers must be the same size
  IF ( SIZE(sendbuf) /= SIZE(recvbuf) ) THEN
    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy MPI procedure for non-serial work'
    WRITE(*,*) 'To use MPI routines for parallel execution, recompile using an MPI compiler'

    CALL MPI_ABORT(comm, 1, error)
  END IF

! Just copy the data from the input buffer to the output buffer
  recvbuf(:,:) = sendbuf(:,:)

  error = 0

  RETURN

END SUBROUTINE mpi_gatherv



SUBROUTINE mpi_bcast(buffer, count, datatype, root, comm, error)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: count
  REAL, INTENT(INOUT) :: buffer(count)
  INTEGER, INTENT(IN) :: datatype

  INTEGER, INTENT(IN) :: root
  INTEGER, INTENT(IN) :: comm
  INTEGER, INTENT(OUT) :: error

! Since there is only one task, we don't need to do anything to broadcast a value

  error = 0

  RETURN

END SUBROUTINE mpi_bcast



SUBROUTINE mpi_allreduce(sendbuf, recvbuf, sendcnt, datatype, op, comm, error)

  IMPLICIT NONE

  LOGICAL, INTENT(IN) :: sendbuf
  LOGICAL, INTENT(OUT) :: recvbuf
  INTEGER, INTENT(IN) :: sendcnt
  INTEGER, INTENT(IN) :: datatype
  INTEGER, INTENT(IN) :: op
  INTEGER, INTENT(IN) :: comm
  INTEGER, INTENT(OUT) :: error

! This routine only exists in the dummy library for logical variables
! The op is ignored, and this is just treated like a bcast
  recvbuf = sendbuf

  error = 0

  RETURN

END SUBROUTINE mpi_allreduce



SUBROUTINE MPI_BARRIER(comm, error)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: comm
  INTEGER, INTENT(OUT) :: error

! Since there is only one task, we don't need to do anything to form a barrier

  error = 0

  RETURN

END SUBROUTINE MPI_BARRIER



SUBROUTINE mpi_type_get_extent(type, lb, extent, error)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: type
  INTEGER, INTENT(OUT) :: lb
  INTEGER, INTENT(OUT) :: extent
  INTEGER, INTENT(OUT) :: error

! All information about MPI types is ignored by the dummy mpi routines, so
! it doesn't matter what we return
  lb     = 0
  extent = 4
  error  = 0

  RETURN

END SUBROUTINE mpi_type_get_extent



SUBROUTINE mpi_type_vector(count, blocklength, stride, old_type, new_type, error)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: count
  INTEGER, INTENT(IN) :: blocklength
  INTEGER, INTENT(IN) :: stride
  INTEGER, INTENT(IN) :: old_type
  INTEGER, INTENT(OUT) :: new_type
  INTEGER, INTENT(OUT) :: error

! All information about MPI types is ignored by the dummy mpi routines, so
! it doesn't matter what we return

  new_type = 1
  error    = 0

  RETURN

END SUBROUTINE mpi_type_vector



SUBROUTINE mpi_type_create_resized(old_type, lb, extent, new_type, error)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: old_type
  INTEGER, INTENT(IN) :: lb
  INTEGER, INTENT(IN) :: extent
  INTEGER, INTENT(OUT) :: new_type
  INTEGER, INTENT(OUT) :: error

! All information about MPI types is ignored by the dummy mpi routines, so
! it doesn't matter what we return

  new_type = 1
  error    = 0

  RETURN

END SUBROUTINE mpi_type_create_resized



SUBROUTINE mpi_type_commit(datatype, error)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: datatype
  INTEGER, INTENT(OUT) :: error

! All information about MPI types is ignored by the dummy mpi routines, so
! there is nothing to do

  error = 0

  RETURN

END SUBROUTINE mpi_type_commit
#endif
