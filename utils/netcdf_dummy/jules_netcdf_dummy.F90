#if !defined(UM_JULES) && defined(NCDF_DUMMY)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE netcdf

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This module provides a dummy interface to NetCDF constants and procedures
!   that can be used to link against. If any attempt is made to use the
!   procedures, an error is given
!
! Current Code Owner: Kerry Smout-Day
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
  INTEGER, PARAMETER ::                                                       &
    nf90_int    = 4,                                                          &
    nf90_float  = 5,                                                          &
    nf90_double = 6

  INTEGER, PARAMETER ::                                                       &
    nf90_nowrite   = 0,                                                       &
    nf90_write     = 1,                                                       &
    nf90_clobber   = 0,                                                       &
    nf90_noclobber = 4,                                                       &
    nf90_fill      = 0,                                                       &
    nf90_nofill    = 256

  INTEGER, PARAMETER :: nf90_netcdf4 = 0

  INTEGER, PARAMETER :: nf90_mpiio = 0

  INTEGER, PARAMETER :: nf90_unlimited = 0

  INTEGER, PARAMETER :: nf90_global = 0

  INTEGER, PARAMETER :: nf90_noerr = 0

! Overloaded variable FUNCTIONs
  INTERFACE nf90_def_var
    MODULE PROCEDURE nf90_def_var_Scalar, nf90_def_var_oneDim,                &
                     nf90_def_var_ManyDims
  END INTERFACE ! nf90_def_var

! Overloaded attribute FUNCTIONs
  INTERFACE nf90_put_att
    MODULE PROCEDURE nf90_put_att_text, nf90_put_att_int, nf90_put_att_real
  END INTERFACE

  INTERFACE nf90_get_att
    MODULE PROCEDURE nf90_get_att_text, nf90_get_att_int, nf90_get_att_real
  END INTERFACE

! Overloaded variable FUNCTIONs
  INTERFACE nf90_put_var
    MODULE PROCEDURE nf90_put_var_text, nf90_put_var_int, nf90_put_var_real
    MODULE PROCEDURE nf90_put_var_1D_text, nf90_put_var_1D_int,               &
                     nf90_put_var_1D_real
    MODULE PROCEDURE nf90_put_var_2D_text, nf90_put_var_2D_int,               &
                     nf90_put_var_2D_real
    MODULE PROCEDURE nf90_put_var_3D_text, nf90_put_var_3D_int,               &
                     nf90_put_var_3D_real
    MODULE PROCEDURE nf90_put_var_4D_text, nf90_put_var_4D_int,               &
                     nf90_put_var_4D_real
    MODULE PROCEDURE nf90_put_var_5D_text, nf90_put_var_5D_int,               &
                     nf90_put_var_5D_real
    MODULE PROCEDURE nf90_put_var_6D_text, nf90_put_var_6D_int,               &
                     nf90_put_var_6D_real
    MODULE PROCEDURE nf90_put_var_7D_text, nf90_put_var_7D_int,               &
                     nf90_put_var_7D_real
  END INTERFACE ! nf90_put_var

  INTERFACE nf90_get_var
    MODULE PROCEDURE nf90_get_var_text, nf90_get_var_int, nf90_get_var_real
    MODULE PROCEDURE nf90_get_var_1D_text, nf90_get_var_1D_int,               &
                     nf90_get_var_1D_real
    MODULE PROCEDURE nf90_get_var_2D_text, nf90_get_var_2D_int,               &
                     nf90_get_var_2D_real
    MODULE PROCEDURE nf90_get_var_3D_text, nf90_get_var_3D_int,               &
                     nf90_get_var_3D_real
    MODULE PROCEDURE nf90_get_var_4D_text, nf90_get_var_4D_int,               &
                     nf90_get_var_4D_real
    MODULE PROCEDURE nf90_get_var_5D_text, nf90_get_var_5D_int,               &
                     nf90_get_var_5D_real
    MODULE PROCEDURE nf90_get_var_6D_text, nf90_get_var_6D_int,               &
                     nf90_get_var_6D_real
    MODULE PROCEDURE nf90_get_var_7D_text, nf90_get_var_7D_int,               &
                     nf90_get_var_7D_real
  END INTERFACE ! nf90_get_var

CONTAINS

  FUNCTION nf90_strerror(ncerr)
    INTEGER, INTENT(IN) :: ncerr
    CHARACTER(LEN = 80)  :: nf90_strerror

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_strerror

  FUNCTION nf90_enddef(ncid, h_minfree, v_align, v_minfree, r_align)
    INTEGER,           INTENT(IN) :: ncid
    INTEGER, OPTIONAL, INTENT(IN) :: h_minfree, v_align, v_minfree, r_align
    INTEGER                        :: nf90_enddef

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_enddef

  FUNCTION nf90_close(ncid)
    INTEGER, INTENT(IN) :: ncid
    INTEGER              :: nf90_close

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_close

  FUNCTION nf90_inquire(ncid, nDimensions, nVariables, nAttributes,           &
                        unlimitedDimId, formatNum)
    INTEGER,           INTENT(IN) :: ncid
    INTEGER, OPTIONAL, INTENT(OUT) :: nDimensions, nVariables, nAttributes,   &
                                      unlimitedDimId, formatNum
    INTEGER                        :: nf90_inquire

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_inquire

  FUNCTION nf90_open(path, mode, ncid, chunksize, cache_size, cache_nelems,   &
                                                 cache_preemption, comm, info)
    IMPLICIT NONE
    CHARACTER (LEN = *), INTENT(IN) :: path
    INTEGER, INTENT(IN) :: mode
    INTEGER, INTENT(OUT) :: ncid
    INTEGER, OPTIONAL, INTENT(INOUT) :: chunksize
    INTEGER, OPTIONAL, INTENT(IN) :: cache_size, cache_nelems
    REAL, OPTIONAL, INTENT(IN) :: cache_preemption
    INTEGER, OPTIONAL, INTENT(IN) :: comm, info
    INTEGER :: nf90_open

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_open

  FUNCTION nf90_create(path, cmode, ncid, initialsize, chunksize, cache_size, &
                                   cache_nelems, cache_preemption, comm, info)
    IMPLICIT NONE
    CHARACTER (LEN = *), INTENT(IN) :: path
    INTEGER, INTENT(IN) :: cmode
    INTEGER, INTENT(OUT) :: ncid
    INTEGER, OPTIONAL, INTENT(IN) :: initialsize
    INTEGER, OPTIONAL, INTENT(INOUT) :: chunksize
    INTEGER, OPTIONAL, INTENT(IN) :: cache_size, cache_nelems
    INTEGER, OPTIONAL, INTENT(IN) :: cache_preemption
    INTEGER, OPTIONAL, INTENT(IN) :: comm, info
    INTEGER :: nf90_create

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_create

  FUNCTION nf90_def_dim(ncid, name, len, dimid)
    INTEGER,             INTENT(IN) :: ncid
    CHARACTER (LEN = *), INTENT(IN) :: name
    INTEGER,             INTENT(IN) :: len
    INTEGER,             INTENT(OUT) :: dimid
    INTEGER                          :: nf90_def_dim

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_def_dim

  FUNCTION nf90_inq_dimid(ncid, name, dimid)
    INTEGER,             INTENT(IN) :: ncid
    CHARACTER (LEN = *), INTENT(IN) :: name
    INTEGER,             INTENT(OUT) :: dimid
    INTEGER                          :: nf90_inq_dimid

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_inq_dimid

  FUNCTION nf90_inquire_dimension(ncid, dimid, name, len)
    INTEGER,                       INTENT(IN) :: ncid, dimid
    CHARACTER (LEN = *), OPTIONAL, INTENT(OUT) :: name
    INTEGER,             OPTIONAL, INTENT(OUT) :: len
    INTEGER                                    :: nf90_inquire_dimension

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_inquire_dimension

  FUNCTION nf90_put_att_text(ncid, varid, name, values)
    INTEGER,                          INTENT(IN) :: ncid, varid
    CHARACTER(LEN = *),               INTENT(IN) :: name
    CHARACTER(LEN = *),               INTENT(IN) :: values
    INTEGER                                       :: nf90_put_att_text

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_att_text

  FUNCTION nf90_get_att_text(ncid, varid, name, values)
    INTEGER,                          INTENT(IN) :: ncid, varid
    CHARACTER(LEN = *),               INTENT(IN) :: name
    CHARACTER(LEN = *),               INTENT(OUT) :: values
    INTEGER                                       :: nf90_get_att_text

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_att_text

  FUNCTION nf90_put_att_int(ncid, varid, name, values)
    INTEGER,            INTENT(IN) :: ncid, varid
    CHARACTER(LEN = *), INTENT(IN) :: name
    INTEGER,            INTENT(IN) :: values
    INTEGER :: nf90_put_att_int

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_att_int

  FUNCTION nf90_get_att_int(ncid, varid, name, values)
    INTEGER,            INTENT(IN) :: ncid, varid
    CHARACTER(LEN = *), INTENT(IN) :: name
    INTEGER,            INTENT(OUT) :: values
    INTEGER :: nf90_get_att_int

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_att_int

  FUNCTION nf90_put_att_real(ncid, varid, name, values)
    INTEGER,            INTENT(IN) :: ncid, varid
    CHARACTER(LEN = *), INTENT(IN) :: name
    REAL,               INTENT(IN) :: values
    INTEGER :: nf90_put_att_real

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_att_real

  FUNCTION nf90_get_att_real(ncid, varid, name, values)
    INTEGER,            INTENT(IN) :: ncid, varid
    CHARACTER(LEN = *), INTENT(IN) :: name
    REAL,               INTENT(OUT) :: values
    INTEGER :: nf90_get_att_real

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_att_real

  FUNCTION nf90_def_var_Scalar(ncid, name, xtype, varid)
    INTEGER, INTENT(IN) :: ncid
    CHARACTER (LEN = *), INTENT(IN) :: name
    INTEGER, INTENT(IN) :: xtype
    INTEGER, INTENT(OUT) :: varid
    INTEGER :: nf90_def_var_Scalar

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_def_var_Scalar

  FUNCTION nf90_def_var_oneDim(ncid, name, xtype, dimids, varid, contiguous,  &
                  chunksizes, deflate_level, shuffle, fletcher32, endianness, &
                  cache_size, cache_nelems, cache_preemption)
    INTEGER, INTENT(IN) :: ncid
    CHARACTER (LEN = *), INTENT(IN) :: name
    INTEGER, INTENT(IN) :: xtype
    INTEGER, INTENT(IN) :: dimids
    INTEGER, INTENT(OUT) :: varid
    LOGICAL, OPTIONAL, INTENT(IN) :: contiguous
    INTEGER, OPTIONAL, INTENT(IN) :: chunksizes
    INTEGER, OPTIONAL, INTENT(IN) :: deflate_level
    LOGICAL, OPTIONAL, INTENT(IN) :: shuffle, fletcher32
    INTEGER, OPTIONAL, INTENT(IN) :: endianness
    INTEGER, OPTIONAL, INTENT(IN) :: cache_size, cache_nelems,                &
                                     cache_preemption
    INTEGER :: nf90_def_var_oneDim

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_def_var_oneDim

  FUNCTION nf90_def_var_ManyDims(ncid, name, xtype, dimids, varid, contiguous,&
       chunksizes, deflate_level, shuffle, fletcher32, endianness, cache_size,&
       cache_nelems, cache_preemption)
    INTEGER, INTENT(IN) :: ncid
    CHARACTER (LEN = *), INTENT(IN) :: name
    INTEGER, INTENT(IN) :: xtype
    INTEGER, DIMENSION(:), INTENT(IN) :: dimids
    INTEGER, INTENT(OUT) :: varid
    LOGICAL, OPTIONAL, INTENT(IN) :: contiguous
    INTEGER, OPTIONAL, DIMENSION(:), INTENT(IN) :: chunksizes
    INTEGER, OPTIONAL, INTENT(IN) :: deflate_level
    LOGICAL, OPTIONAL, INTENT(IN) :: shuffle, fletcher32
    INTEGER, OPTIONAL, INTENT(IN) :: endianness
    INTEGER, OPTIONAL, INTENT(IN) :: cache_size, cache_nelems,                &
                                     cache_preemption
    INTEGER :: nf90_def_var_ManyDims

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_def_var_ManyDims

  FUNCTION nf90_inq_varid(ncid, name, varid)
    INTEGER, INTENT(IN) :: ncid
    CHARACTER (LEN = *), INTENT(IN) :: name
    INTEGER, INTENT(OUT) :: varid
    INTEGER :: nf90_inq_varid

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_inq_varid

  FUNCTION nf90_inquire_variable(ncid, varid, name, xtype, ndims, dimids,     &
                                 nAtts, contiguous, chunksizes, deflate_level,&
                                 shuffle, fletcher32, endianness, cache_size, &
                                 cache_nelems, cache_preemption)
    INTEGER, INTENT(IN) :: ncid, varid
    CHARACTER (LEN = *), OPTIONAL, INTENT(OUT) :: name
    INTEGER, OPTIONAL, INTENT(OUT) :: xtype, ndims
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(OUT) :: dimids
    INTEGER, OPTIONAL, INTENT(OUT) :: nAtts
    LOGICAL, OPTIONAL, INTENT(OUT) :: contiguous
    INTEGER, OPTIONAL, DIMENSION(:), INTENT(OUT) :: chunksizes
    INTEGER, OPTIONAL, INTENT(OUT) :: deflate_level
    LOGICAL, OPTIONAL, INTENT(OUT) :: shuffle, fletcher32
    INTEGER, OPTIONAL, INTENT(OUT) :: endianness
    INTEGER, OPTIONAL, INTENT(OUT) :: cache_size, cache_nelems,               &
                                      cache_preemption
    INTEGER :: nf90_inquire_variable

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_inquire_variable

  FUNCTION nf90_put_var_text(ncid, varid, values, start, count, stride, map)
    INTEGER, INTENT(IN) :: ncid, varid
    CHARACTER (LEN = *),             INTENT(IN) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER  :: nf90_put_var_text

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_var_text

  FUNCTION nf90_get_var_text(ncid, varid, values, start, count, stride, map)
    INTEGER, INTENT(IN) :: ncid, varid
    CHARACTER (LEN = *),             INTENT(OUT) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_get_var_text

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_var_text

  FUNCTION nf90_put_var_1D_text(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    CHARACTER (LEN = *), DIMENSION(:), INTENT(IN) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER  :: nf90_put_var_1D_text

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_var_1D_text

  FUNCTION nf90_put_var_2D_text(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    CHARACTER (LEN = *), DIMENSION(:, :), INTENT(IN) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER  :: nf90_put_var_2D_text

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_var_2D_text

  FUNCTION nf90_put_var_3D_text(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    CHARACTER (LEN = *), DIMENSION(:, :, :), INTENT(IN) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER  :: nf90_put_var_3D_text

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_var_3D_text

  FUNCTION nf90_put_var_4D_text(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    CHARACTER (LEN = *), DIMENSION(:, :, :, :), INTENT(IN) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER  :: nf90_put_var_4D_text

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_var_4D_text

  FUNCTION nf90_put_var_5D_text(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    CHARACTER (LEN = *), DIMENSION(:, :, :, :, :), INTENT(IN) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER  :: nf90_put_var_5D_text

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_var_5D_text

  FUNCTION nf90_put_var_6D_text(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    CHARACTER (LEN = *), DIMENSION(:, :, :, :, :, :), INTENT(IN) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER  :: nf90_put_var_6D_text

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_var_6D_text

  FUNCTION nf90_put_var_7D_text(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    CHARACTER (LEN = *), DIMENSION(:, :, :, :, :, :, :), INTENT(IN) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER  :: nf90_put_var_7D_text

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_var_7D_text

  FUNCTION nf90_get_var_1D_text(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    CHARACTER (LEN = *), DIMENSION(:), INTENT(OUT) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_get_var_1D_text

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_var_1D_text

  FUNCTION nf90_get_var_2D_text(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    CHARACTER (LEN = *), DIMENSION(:, :), INTENT(OUT) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_get_var_2D_text

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_var_2D_text

  FUNCTION nf90_get_var_3D_text(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    CHARACTER (LEN = *), DIMENSION(:, :, :), INTENT(OUT) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_get_var_3D_text

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_var_3D_text

  FUNCTION nf90_get_var_4D_text(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    CHARACTER (LEN = *), DIMENSION(:, :, :, :), INTENT(OUT) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_get_var_4D_text

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_var_4D_text

  FUNCTION nf90_get_var_5D_text(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    CHARACTER (LEN = *), DIMENSION(:, :, :, :, :), INTENT(OUT) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_get_var_5D_text

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_var_5D_text

  FUNCTION nf90_get_var_6D_text(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    CHARACTER (LEN = *), DIMENSION(:, :, :, :, :, :), INTENT(OUT) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_get_var_6D_text

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_var_6D_text

  FUNCTION nf90_get_var_7D_text(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    CHARACTER (LEN = *), DIMENSION(:, :, :, :, :, :, :), INTENT(OUT) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_get_var_7D_text

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_var_7D_text


  FUNCTION nf90_put_var_int(ncid, varid, values, start)
    INTEGER, INTENT(IN) :: ncid, varid
    INTEGER, INTENT(IN) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start
    INTEGER :: nf90_put_var_int

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_var_int

  FUNCTION nf90_put_var_real(ncid, varid, values, start)
    INTEGER, INTENT(IN) :: ncid, varid
    REAL, INTENT(IN) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start
    INTEGER :: nf90_put_var_real

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_var_real

  FUNCTION nf90_get_var_int(ncid, varid, values, start)
    INTEGER, INTENT(IN) :: ncid, varid
    INTEGER, INTENT(OUT) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start
    INTEGER :: nf90_get_var_int

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_var_int

  FUNCTION nf90_get_var_real(ncid, varid, values, start)
    INTEGER, INTENT(IN) :: ncid, varid
    REAL, INTENT(OUT) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start
    INTEGER :: nf90_get_var_real

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_var_real

  FUNCTION nf90_put_var_1D_int(ncid, varid, values, start, count, stride, map)
    INTEGER, INTENT(IN) :: ncid, varid
    INTEGER, DIMENSION(:), INTENT(IN) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_put_var_1D_int

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_var_1D_int

  FUNCTION nf90_put_var_2D_int(ncid, varid, values, start, count, stride, map)
    INTEGER, INTENT(IN) :: ncid, varid
    INTEGER, DIMENSION(:, :), INTENT(IN) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_put_var_2D_int

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_var_2D_int

  FUNCTION nf90_put_var_3D_int(ncid, varid, values, start, count, stride, map)
    INTEGER, INTENT(IN) :: ncid, varid
    INTEGER, DIMENSION(:, :, :), INTENT(IN) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_put_var_3D_int

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_var_3D_int

  FUNCTION nf90_put_var_4D_int(ncid, varid, values, start, count, stride, map)
    INTEGER, INTENT(IN) :: ncid, varid
    INTEGER, DIMENSION(:, :, :, :), INTENT(IN) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_put_var_4D_int

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_var_4D_int

  FUNCTION nf90_put_var_5D_int(ncid, varid, values, start, count, stride, map)
    INTEGER, INTENT(IN) :: ncid, varid
    INTEGER, DIMENSION(:, :, :, :, :), INTENT(IN) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_put_var_5D_int

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_var_5D_int

  FUNCTION nf90_put_var_6D_int(ncid, varid, values, start, count, stride, map)
    INTEGER, INTENT(IN) :: ncid, varid
    INTEGER, DIMENSION(:, :, :, :, :, :), INTENT(IN) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_put_var_6D_int

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_var_6D_int

  FUNCTION nf90_put_var_7D_int(ncid, varid, values, start, count, stride, map)
    INTEGER, INTENT(IN) :: ncid, varid
    INTEGER, DIMENSION(:, :, :, :, :, :, :), INTENT(IN) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_put_var_7D_int

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_var_7D_int

  FUNCTION nf90_put_var_1D_real(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    REAL, DIMENSION(:), INTENT(IN) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER  :: nf90_put_var_1D_real

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_var_1D_real

  FUNCTION nf90_put_var_2D_real(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    REAL, DIMENSION(:, :), INTENT(IN) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER  :: nf90_put_var_2D_real

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_var_2D_real

  FUNCTION nf90_put_var_3D_real(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    REAL, DIMENSION(:, :, :), INTENT(IN) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER  :: nf90_put_var_3D_real

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_var_3D_real

  FUNCTION nf90_put_var_4D_real(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    REAL, DIMENSION(:, :, :, :), INTENT(IN) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER  :: nf90_put_var_4D_real

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_var_4D_real

  FUNCTION nf90_put_var_5D_real(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    REAL, DIMENSION(:, :, :, :, :), INTENT(IN) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER  :: nf90_put_var_5D_real

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_var_5D_real

  FUNCTION nf90_put_var_6D_real(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    REAL, DIMENSION(:, :, :, :, :, :), INTENT(IN) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER  :: nf90_put_var_6D_real

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_var_6D_real

  FUNCTION nf90_put_var_7D_real(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    REAL, DIMENSION(:, :, :, :, :, :, :), INTENT(IN) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER  :: nf90_put_var_7D_real

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_put_var_7D_real

  FUNCTION nf90_get_var_1D_int(ncid, varid, values, start, count, stride, map)
    INTEGER, INTENT(IN) :: ncid, varid
    INTEGER, DIMENSION(:), INTENT(OUT) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_get_var_1D_int

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_var_1D_int

  FUNCTION nf90_get_var_2D_int(ncid, varid, values, start, count, stride, map)
    INTEGER, INTENT(IN) :: ncid, varid
    INTEGER, DIMENSION(:, :), INTENT(OUT) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_get_var_2D_int

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_var_2D_int

  FUNCTION nf90_get_var_3D_int(ncid, varid, values, start, count, stride, map)
    INTEGER, INTENT(IN) :: ncid, varid
    INTEGER, DIMENSION(:, :, :), INTENT(OUT) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_get_var_3D_int

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_var_3D_int

  FUNCTION nf90_get_var_4D_int(ncid, varid, values, start, count, stride, map)
    INTEGER, INTENT(IN) :: ncid, varid
    INTEGER, DIMENSION(:, :, :, :), INTENT(OUT) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_get_var_4D_int

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_var_4D_int

  FUNCTION nf90_get_var_5D_int(ncid, varid, values, start, count, stride, map)
    INTEGER, INTENT(IN) :: ncid, varid
    INTEGER, DIMENSION(:, :, :, :, :), INTENT(OUT) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_get_var_5D_int

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_var_5D_int

  FUNCTION nf90_get_var_6D_int(ncid, varid, values, start, count, stride, map)
    INTEGER, INTENT(IN) :: ncid, varid
    INTEGER, DIMENSION(:, :, :, :, :, :), INTENT(OUT) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_get_var_6D_int

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_var_6D_int

  FUNCTION nf90_get_var_7D_int(ncid, varid, values, start, count, stride, map)
    INTEGER, INTENT(IN) :: ncid, varid
    INTEGER, DIMENSION(:, :, :, :, :, :, :), INTENT(OUT) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_get_var_7D_int

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_var_7D_int

  FUNCTION nf90_get_var_1D_real(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    REAL, DIMENSION(:), INTENT(OUT) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_get_var_1D_real

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_var_1D_real

  FUNCTION nf90_get_var_2D_real(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    REAL, DIMENSION(:, :), INTENT(OUT) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_get_var_2D_real

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_var_2D_real

  FUNCTION nf90_get_var_3D_real(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    REAL, DIMENSION(:, :, :), INTENT(OUT) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_get_var_3D_real

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_var_3D_real

  FUNCTION nf90_get_var_4D_real(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    REAL, DIMENSION(:, :, :, :), INTENT(OUT) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_get_var_4D_real

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_var_4D_real

  FUNCTION nf90_get_var_5D_real(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    REAL, DIMENSION(:, :, :, :, :), INTENT(OUT) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_get_var_5D_real

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_var_5D_real

  FUNCTION nf90_get_var_6D_real(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    REAL, DIMENSION(:, :, :, :, :, :), INTENT(OUT) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_get_var_6D_real

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_var_6D_real

  FUNCTION nf90_get_var_7D_real(ncid, varid, values, start, count, stride,    &
                                map)
    INTEGER, INTENT(IN) :: ncid, varid
    REAL, DIMENSION(:, :, :, :, :, :, :), INTENT(OUT) :: values
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: start, count, stride, map
    INTEGER :: nf90_get_var_7D_real

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_get_var_7D_real

  FUNCTION nf90_sync(ncid)
    INTEGER, INTENT(IN) :: ncid
    INTEGER :: nf90_sync

    WRITE(*,*) 'FATAL ERROR: Attempt to use dummy NetCDF procedure'
    WRITE(*,*) 'To use NetCDF, recompile linking the NetCDF library'
#if defined(GNU_FORTRAN)
    CALL EXIT(1)  ! Exit with non-zero exit status
#else
    STOP
#endif
  END FUNCTION nf90_sync

END MODULE netcdf
#endif
