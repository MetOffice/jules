#if !defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! JULES version of UM module um_types

MODULE um_types

USE precision_mod, ONLY: real_32, real_64,                                     &
                          integer_32 => int_32, integer_64 => int_64

IMPLICIT NONE

! Kinds for 64 and 32 bit logicals. Note that there is no
! "selected_logical_kind", but using the equivalent integer kind is a
! workaround that works on every platform we have tested.
INTEGER, PARAMETER :: logical_64 = integer_64
INTEGER, PARAMETER :: logical_32 = integer_32

! Explicit kind type for reals used in routines imported by the UM and LFRic
INTEGER, PARAMETER :: real_jlslsm = real_32

END MODULE um_types

#endif
