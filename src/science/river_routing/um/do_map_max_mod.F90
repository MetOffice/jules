#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!Code Owner: Please refer to the UM file CodeOwners.txt
!This file belongs in section: River Routing
MODULE do_map_max_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CONTAINS


SUBROUTINE do_map_max(src_field, src_rows, src_row_length, targ_field,         &
     targ_row_length, targ_rows, targ_grid, mask_targ, want,                   &
     recv_concern_max, recv_size, contribution, contrib_size)


!   Subroutine DO_MAP_MAX  --------------------------------------------
!
! Purpose:
!
!   Uses the count and weights obtained by prearav but puts the
!   variable into the box mostly mapped onto.
!   Perform area-averaging to transform data from the source grid to
!   the target grid, or adjust the values on the source grid to have
!   the area-averages supplied on the target grid. The latter mode
!   is intended for adjusting values obtained by interpolating from
!   "target" to "source" in order to conserve the area-averages.
!   This mode should be used ONLY if each source box belongs in
!   exactly one target box. ADJUST=0 selects normal area-averaging,
!   ADJUST=1 selects adjustment by addition (use this mode for fields
!   which may have either sign), ADJUST=2 selects adjustment by
!   multiplication (for fields which are positive-definite or
!   negative-definite).
!
!   The shape of the source and target grids are specified by their
!   dimensions GAPS_aa_bb, which give the number of gaps in the
!   aa=LAMBDA,PHI coordinate in the bb=SRCE,TARG grid. (The product
!   of GAPS_LAMBDA_bb and GAPS_PHI_bb is the number of boxes in the
!   bb grid.)
!
!   The input and output data are supplied as 2D arrays DATA_SRCE and
!   DATA_TARG, whose first dimensions should also be supplied. Speci-
!   fying these sizes separately from the actual dimensions of the
!   grids allows for columns and rows in the arrays to be ignored.
!   A target land/sea mask should be supplied in MASK_TARG, with the
!   value indicating wanted points specified in WANT. Points which
!   are unwanted or which lie outside the source grid are not altered
!   in DATA_TARG. DATA_SRCE can optionally be supplied with its rows
!   in reverse order (i.e. with the first row corresponding to
!   minimum LAMBDA).
!
!   The arrays COUNT_TARG, BASE_TARG, INDEX_SRCE and WEIGHT should be
!   supplied as returned by PRE_AREAVER q.v.
!
!
!END -----------------------------------------------------------------

USE UM_ParCore, ONLY: mype
USE regrid_types, ONLY: concern_max, contribution_info
USE regrid_utils, ONLY: global_to_local_gridpt,                                &
     get_val_from_concern

IMPLICIT NONE

INTEGER, INTENT(IN) :: recv_size, contrib_size
! length of receive concern max array and contribution array
! respectively
TYPE(concern_max), INTENT(IN) :: recv_concern_max(recv_size)
! contains information on src points and targets to which the
! contribute
TYPE(contribution_info), INTENT(IN) :: contribution(contrib_size)
! this contains the value of source data which contribute to a given
! target point and is arranged in row major order in compressed 1-d
! index
INTEGER, INTENT(IN) :: src_rows, src_row_length, targ_row_length,              &
     targ_rows
! extents of 2d target and source fields on this process

REAL(KIND=real_jlslsm), INTENT(IN) :: src_field(src_row_length, src_rows)
! the subdomain source field on this process
LOGICAL, INTENT(IN) :: mask_targ(src_row_length, src_rows), want
! masks out contribution to a target grid point if mask_targ .eqv. want
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: targ_field(targ_row_length, targ_rows)
! the target subdomain field to regrid source field to
INTEGER, INTENT(IN) :: targ_grid
! the target grid being regridded to (river, atmos p, etc)

! local variables
INTEGER :: i, j, k, c_index, dat_size, ERROR
INTEGER :: xt, yt ! target grid point
REAL(KIND=real_jlslsm) :: targ_val

! loop over source field and apply the contribution arrays to src field

DO j = 1, targ_rows
  DO i = 1, targ_row_length

    ! convert to compressed 1d index
    c_index = (j-1) * targ_row_length + i
    dat_size = contribution(c_index)%SIZE

    DO k = 1, dat_size

      xt = contribution(c_index)%x(k)
      yt = contribution(c_index)%y(k)

      targ_val = 0

      ! if I am contributing to myself then target index
      ! must be in my subdomain
      IF (contribution(c_index)%contrib_proc(k) == mype) THEN

        CALL global_to_local_gridpt(xt, yt, targ_grid)

        IF (mask_targ(xt, yt) .EQV. want) targ_val = src_field(xt,             &
             yt)

      ELSE

        targ_val = get_val_from_concern(recv_concern_max,                      &
             recv_size, xt, yt, contribution(c_index                           &
             )%contrib_proc(k), ERROR)

      END IF

      targ_field(i,j) = targ_field(i,j) + targ_val

    END DO
  END DO
END DO

END SUBROUTINE do_map_max

END MODULE do_map_max_mod
#endif
