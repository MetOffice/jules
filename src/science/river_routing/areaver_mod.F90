!###############################################################################
!###############################################################################

MODULE areaver_mod

!-----------------------------------------------------------------------------
! Description:
!   Contains regular lat/lon regridding functions for standalone running
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='AREAVER_MOD'

CONTAINS

!###############################################################################
!###############################################################################
SUBROUTINE pre_areaver(gaps_lambda_srce,lambda_srce                            &
    ,gaps_phi_srce,phi_srce,cyclic_srce,lrow_srce,want,mask_srce               &
    ,gaps_lambda_targ,lambda_targ,gaps_phi_targ,phi_targ                       &
    ,cyclic_targ,spherical                                                     &
    ,maxl,count_targ,base_targ,index_srce,weight)

!     Subroutine PRE_AREAVER
!
!     Calculate weights for area-averaging data on the source grid to
!     data on the target grid.
!
!    J.Gregory  <- programmer of some or all of previous code or changes
!
!    Model            Modification history from model version 3.0:
!   version  Date
!     3.2    13/07/93 Changed CHARACTER*(*) to CHARACTER*(80) for
!                     portability.  Author Tracey Smith.
!     3.3   3.9.93    Correct out-of-bounds errors for IXL and IYT
!     4.1   22.5.96   J.M.Gregory  Add argument SPHERICAL to produce
!                     correct weights for a spherical surface.
!     5.3   22.10.01  Change for south -> north ordering. C.F.Durman.
!     5.5  28.02.03  Add defined RIVERS. C. Bunton
!     6.0  12.09.03  Change DEF from A20 to A26. D. Robinson.
!
!     Standard, paper 4 version 4 (14.12.90)
!
!
!     The source grid and target grid are each specified by a lambda set
!     and a phi set of coordinates delimiting the boxes. These sets are
!     supplied in 1D arrays aa_bb for coordinate aa=LAMBDA,PHI on grid
!     bb=SRCE,TARG.  The number of gaps is specified by GAPS_aa_bb,
!     which is equal to the number of lines IF (CY! IC_bb) and aa is
!     LAMBDA, otherwise one less. (By "gap" we mean the interval
!     spanning a box in one coordinate only. The total number of boxes,
!     or grid points, is the product of the numbers of gaps in the two
!     coordinates.) Whether the axes are cyclic is not known until
!     run-time, so the dimensions of the arrays LAMBDA_bb are not known
!     at compile-time, and they are dimensioned assumed-size. There are
!     no restrictions on the base meridian of lambda, and it does not
!     have to be the same for source and target grids. The lambda
!     coordinates should be in increasing order (running from left to
!     right), the phi increasing (top to bottom). The coordinates must
!     be given in degrees for cyclic axes, because a range of 360 is
!     assumed, or IF (SPHERICAL), when trigonometric functions are
!     used. IF (SPHERICAL), the weights computed are for a spherical
!     surface, assuming that LAMBDA is longitude and PHI latitude.
!     Otherwise, LAMBDA and PHI are treated as Cartesian axes on a
!     plane.
!
!     The array MASK_SRCE is the land/sea mask for the source grid. The
!     logical value indicating points to be used should be supplied in
!     WANT. The first dimension of MASK_SRCE should be supplied in
!     LROW_SRCE. Specifying this separately allows for the possibility
!     of extra rows and columns in MASK_SRCE which are to be ignored.
!
!     The arrays COUNT_TARG and BASE_TARG should be dimensioned in the
!     calling program to the number of boxes in the target array.
!
!     The arrays INDEX_SRCE and WEIGHT are returned in the form which
!     the area-averaging routine DO_AREAVER expects. They are continuous
!     lists comprising consecutive groups of entries. There is a group
!     for each target point, for which the number of entries is spec-
!     ified by COUNT_TARG, and the groups appear in the normal order of
!     grid points. The size required for INDEX_SRCE and WEIGHT depends
!     on how many source boxes go into each target box, on average, and
!     is not known at compile-time. The maximum that could be needed is
!     (GAPS_LAMBDA_SRCE+GAPS_LAMBDA_TARG)*(GAPS_PHI_SRCE+GAPS_PHI_TARG)
!     and the size to which the arrays are actually dimensioned should
!     be supplied in MAXL. The size used is returned in MAXL. It is the
!     responsibility of the calling routine to provide enough space.

!-----------------------------------------------------------------------
USE conversions_mod, ONLY:                                                     &
!      imported scalar parameters
       pi_over_180

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
!-----------------------------------------------------------------------
!
IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
   gaps_lambda_srce                                                            &
                            ! number of lambda gaps in source grid
   ,gaps_phi_srce                                                              &
                            ! number of phi gaps in source grid
   ,gaps_lambda_targ                                                           &
                            ! number of lambda gaps in target grid
   ,gaps_phi_targ                                                              &
                            ! number of phi gaps in target grid
   ,lrow_srce
                            ! first dimension of MASK_SRCE
INTEGER, INTENT(IN OUT) ::                                                     &
   maxl
                            ! maximum entries in output lists
INTEGER, INTENT(OUT) ::                                                        &
   count_targ(gaps_lambda_targ,gaps_phi_targ)                                  &
                            ! no. of source boxes in target box
    ,base_targ(gaps_lambda_targ,gaps_phi_targ)                                 &
                            ! first index in list for target box
    ,index_srce(maxl)
                            ! list of source box indices

LOGICAL, INTENT(IN) ::                                                         &
   cyclic_srce                                                                 &
                            ! source grid is cyclic
   ,cyclic_targ                                                                &
                            ! target grid is cyclic
   ,want                                                                       &
                            ! indicator of wanted points in mask
   ,mask_srce(lrow_srce,*)                                                     &
                            ! land/sea mask for source grid
   ,spherical
                            ! calculate weights for a sphere

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
   lambda_srce( * )                                                            &
                            ! source lambda line coordinates
   ,phi_srce( * )                                                              &
                            ! source phi line coordinates
   ,lambda_targ( * )                                                           &
                            ! target lambda line coordinates
   ,phi_targ( * )
                            ! target phi line coordinates
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
   weight(maxl)
                            ! list of weights for source boxes

! Work variables
INTEGER ::                                                                     &
   lines_lambda_srce                                                           &
                            ! number of source lambda lines
   ,lines_phi_srce                                                             &
                            ! number of source phi lines
   ,lines_lambda_targ                                                          &
                            ! number of target lambda lines
   ,lines_phi_targ                                                             &
                            ! number of target phi lines
   ,count_lambda(gaps_lambda_targ)                                             &
                            ! number of source lambda gaps per target
   ,count_phi(gaps_phi_targ)                                                   &
                            ! number of source phi gaps per target
   ,index_lambda(gaps_lambda_srce + gaps_lambda_targ)                          &
                            ! source lambda gap indices
   ,index_phi(gaps_phi_srce + gaps_phi_targ)                                   &
                            ! source phi gap indices
   ,ix1,iy1,ix2,iy2                                                            &
                            ! working SRCE/TARG LAMBDA/PHI indices
   ,ix1n,ix1w                                                                  &
                            ! working indices
   ,ixl(gaps_lambda_targ+1)                                                    &
                            ! source line on the left of target line
   ,ix2n                                                                       &
                            ! target gap to the right of IX2
   ,iyt(gaps_phi_targ+1)                                                       &
                            ! source line above target line
   ,ixp,iyp,ip                                                                 &
                            ! pointers into lists
   ,ix,iy,i
                            ! loop indices

REAL(KIND=real_jlslsm)  ::                                                     &
   baslam                                                                      &
                            ! minimum lambda for TEMP coordinates
   ,btarg                                                                      &
                            ! width of target gap
   ,blo,bhi                                                                    &
                            ! limits of gap
   ,temp_srce(gaps_lambda_srce+1)                                              &
                            ! adjusted version of LAMBDA_SRCE
   ,temp_targ(gaps_lambda_targ+1)                                              &
                            ! adjusted version of LAMBDA_TARG
   ,frac_lambda(gaps_lambda_srce + gaps_lambda_targ)                           &
                            ! fractions of target lambda gaps
   ,frac_phi(gaps_phi_srce + gaps_phi_targ)                                    &
                            ! fractions of target phi gaps
   ,SUM
                            ! sum of weights

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='PRE_AREAVER'

!     1  Set up the lambda coordinates to make them easier to handle.
!
!     1.1  Produce in TEMP_SRCE a monotonically increasing set of angles
!     equivalent to LAMBDA_SRCE i.e. equal under modulo 360.
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,                 &
                        zhook_handle)

IF (cyclic_srce) THEN
  lines_lambda_srce = gaps_lambda_srce
ELSE
  lines_lambda_srce = gaps_lambda_srce + 1
END IF
baslam = lambda_srce(1)
DO ix1 = 1,lines_lambda_srce
  IF (lambda_srce(ix1) <  baslam) THEN
    temp_srce(ix1) = lambda_srce(ix1) + 360.0
  ELSE
    temp_srce(ix1) = lambda_srce(ix1)
  END IF
END DO

!     1.2  Produce in TEMP_TARG a set of angles equivalent to
!     LAMBDA_TARG i.e. equal under modulo 360, but all in the range
!     BASLAM to BASLAM+360, where BASLAM=min(TEMP_LAMBDA).
IF (cyclic_targ) THEN
  lines_lambda_targ = gaps_lambda_targ
ELSE
  lines_lambda_targ = gaps_lambda_targ + 1
END IF
DO ix2 = 1,lines_lambda_targ
  temp_targ(ix2) = MOD(lambda_targ(ix2) - baslam,360.0)
  IF (temp_targ(ix2) <  0.0) temp_targ(ix2) = temp_targ(ix2) + 360.0
  temp_targ(ix2) = temp_targ(ix2) + baslam
END DO

!     2  For each target lambda line, find the index of the next source
!     lambda line to the left.
DO ix2 = 1,lines_lambda_targ
  DO ix1 = 1,lines_lambda_srce
    IF (temp_targ(ix2) >= temp_srce(ix1)) ixl(ix2) = ix1
  END DO
END DO

!     3  Find which source lambda gaps cover each target gap and the
!     fractions they contribute.
!
!     At this point IXL(target_line) gives the index of the next source
!     lambda line to the left of the target lambda line, wrapping round
!     if the source grid is cyclic. This is also the index of the source
!     gap in which the target line falls. Similarly, the index of the
!     target line is also that of the target gap of which it is the
!     left-hand limit. Therefore also IXL(target_gap+1, wrapping round
!     if reqd.), is the index of the source gap which contains the
!     right-hand limit of the target gap. For each target gap, we loop
!     over all source gaps and find the fraction covered by each. Record
!     the fraction and the source index in cumulative lists. If the
!     source grid is not cyclic, parts of the target gap lying outside
!     the source grid are neglected.
ixp = 0
DO ix2 = 1,gaps_lambda_targ
  ix = 0
  ix2n = MOD(ix2,lines_lambda_targ) + 1
  btarg = temp_targ(ix2n) - temp_targ(ix2)
  IF (btarg <  0.0) THEN
    btarg = btarg + 360.0
    ix1n = ixl(ix2n) + lines_lambda_srce
  ELSE
    ix1n = ixl(ix2n)
  END IF
  DO ix1w = ixl(ix2),ix1n
    ix1 = MOD(ix1w-1,lines_lambda_srce) + 1
    IF (cyclic_srce .OR. ix1 /= lines_lambda_srce) THEN
      IF (ix1w == ixl(ix2)) THEN
        blo = temp_targ(ix2)
      ELSE
        blo = temp_srce(ix1)
      END IF
      IF (ix1w == ix1n) THEN
        bhi = temp_targ(ix2n)
      ELSE
        bhi = temp_srce(MOD(ix1,lines_lambda_srce) + 1)
      END IF
      IF (bhi <  blo) bhi = bhi + 360.0
      IF (ABS(bhi - blo) >  1.0e-7 * ABS(btarg)) THEN
        ix = ix + 1
        index_lambda(ixp + ix) = ix1
        frac_lambda(ixp + ix)=(bhi - blo) / btarg
      END IF
    END IF
  END DO
  count_lambda(ix2) = ix
  ixp = ixp + count_lambda(ix2)
END DO

!     4  For each target phi line, find the index of the next source phi
!     line above. Comments as for section 2, without wrap-round. Note
!     that this assumes that the atmosphere gridbox ordering is from
!     south to north; a north to south atmosphere would require LE
!     instead of GE in the following IF test.
lines_phi_srce = gaps_phi_srce + 1
lines_phi_targ = gaps_phi_targ + 1
DO iy2 = 1,lines_phi_targ
  iyt(iy2) = 0
  DO iy1 = 1,lines_phi_srce
    IF (phi_targ(iy2) >= phi_srce(iy1)) iyt(iy2) = iy1
  END DO
END DO

!     5  Find which source phi gaps cover each target gap and the
!     fractions they contribute. Comments as for section 3, without
!     wrap-round.
iyp = 0
DO iy2 = 1,gaps_phi_targ
  iy = 0
  IF (spherical) THEN
    !     Contain angle between +-90. There is no real area outside
    !     these limits on a sphere.
    btarg = SIN(MAX(MIN(phi_targ(iy2+1),90.0),-90.0) * pi_over_180)            &
       -SIN(MAX(MIN(phi_targ(iy2),90.0),-90.0) * pi_over_180)
  ELSE
    btarg = phi_targ(iy2+1) - phi_targ(iy2)
  END IF
  DO iy1 = iyt(iy2),iyt(iy2+1)
    IF ( .NOT. (iy1 == 0 .OR. iy1 == lines_phi_srce)) THEN
      IF (iy1 == iyt(iy2)) THEN
        blo = phi_targ(iy2)
      ELSE
        blo = phi_srce(iy1)
      END IF
      IF (iy1 == iyt(iy2+1)) THEN
        bhi = phi_targ(iy2+1)
      ELSE
        bhi = phi_srce(iy1+1)
      END IF
      IF (spherical) THEN
        blo = MAX(MIN(blo,90.0),-90.0)
        bhi = MAX(MIN(bhi,90.0),-90.0)
      END IF
      IF (ABS(bhi - blo) >  1.0e-7 * ABS(btarg)) THEN
        iy = iy + 1
        index_phi(iyp + iy) = iy1
        !             Both numerator and denominator in the following are -ve.
        IF (spherical) THEN
          frac_phi(iyp + iy) = (SIN(bhi * pi_over_180) - SIN(blo * pi_over_180)) &
                              /btarg
        ELSE
          frac_phi(iyp + iy)=(bhi - blo) / btarg
        END IF
      END IF
    END IF
  END DO
  count_phi(iy2) = iy
  iyp = iyp + count_phi(iy2)
END DO

!     6  For each target box, loop over contributing source boxes and
!     calculate the weights for each one, ignoring land boxes.
!
!     After the first pass for each target box, go back and normalise
!     the weights to compensate for land source boxes and any outside
!     the source grid. Record the source box index and the weight in
!     cumulative lists.
ip = 0
iyp = 0
DO iy2 = 1,gaps_phi_targ
  ixp = 0
  DO ix2 = 1,gaps_lambda_targ
    i = 0
    SUM = 0
    DO iy = iyp+1,iyp + count_phi(iy2)
      DO ix = ixp+1,ixp + count_lambda(ix2)
        IF ( mask_srce(index_lambda(ix),index_phi(iy)) .EQV. want) THEN
          i = i + 1
          index_srce(ip + i) = index_lambda(ix)                                &
             + (index_phi(iy) - 1) * gaps_lambda_srce
          weight(ip + i) = frac_lambda(ix) * frac_phi(iy)
          SUM = SUM + weight(ip + i)
        END IF
      END DO
    END DO
    count_targ(ix2,iy2) = i
    base_targ(ix2,iy2) = ip
    DO i = 1,count_targ(ix2,iy2)
      weight(ip + i) = weight(ip + i) / SUM
    END DO
    ip = ip + count_targ(ix2,iy2)
    ixp = ixp + count_lambda(ix2)
  END DO
  iyp = iyp + count_phi(iy2)
END DO
maxl = ip

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,                &
                        zhook_handle)
RETURN

END SUBROUTINE pre_areaver
!###############################################################################
!###############################################################################
SUBROUTINE do_areaver( gaps_lambda_srce,gaps_phi_srce,lrow_srce                &
    ,invert_srce,data_srce,gaps_lambda_targ,gaps_phi_targ,count_targ           &
    ,base_targ,lrow_targ,want,mask_targ,index_srce,weight,adjust               &
    ,data_targ,global_src_lambda_gaps,grid,adjust_targ )

!     Subroutine DO_AREAVER -------------------------------------------
!
!   Purpose:
!
!     Perform area-averaging to transform data from the source grid to
!     the target grid, or adjust the values on the source grid to have
!     the area-averages supplied on the target grid. The latter mode
!     is intended for adjusting values obtained by interpolating from
!     "target" to "source" in order to conserve the area-averages.
!     This mode should be used ONLY if each source box belongs in
!     exactly one target box. ADJUST=0 selects normal area-averaging,
!     ADJUST=1 selects adjustment by addition (use this mode for fields
!     which may have either sign), ADJUST=2 selects adjustment by
!     multiplication (for fields which are positive-definite or
!     negative-definite).
!
!     For two-way conservative coupling, ADJUST=3 makes an adjustment
!     field for fields which may have either sign, ADJUST=4 makes an
!     adjustment field for fields which are positive-definite or
!     negative-definite, ADJUST=5 performs conservative adjustment
!     by addition (use this mode for fields which may have either sign)
!     and ADJUST=6 selects conservative adjustment by multiplication
!     (for fields which are positive-definite or negative-definite).
!
!     The shape of the source and target grids are specified by their
!     dimensions GAPS_aa_bb, which give the number of gaps in the
!     aa=LAMBDA,PHI coordinate in the bb=SRCE,TARG grid. (The product
!     of GAPS_LAMBDA_bb and GAPS_PHI_bb is the number of boxes in the
!     bb grid.)
!
!     The input and output data are supplied as 2D arrays DATA_SRCE and
!     DATA_TARG, whose first dimensions should also be supplied. Speci-
!     fying these sizes separately from the actual dimensions of the
!     grids allows for columns and rows in the arrays to be ignored.
!     A target land/sea mask should be supplied in MASK_TARG, with the
!     value indicating wanted points specified in WANT. Points which
!     are unwanted or which lie outside the source grid are not altered
!     in DATA_TARG. DATA_SRCE can optionally be supplied with its rows
!     in reverse order (i.e. with the first row corresponding to
!     minimum LAMBDA).
!
!     The arrays COUNT_TARG, BASE_TARG, INDEX_SRCE and WEIGHT should be
!     supplied as returned by PRE_AREAVER q.v.
!
!     Programming Standard, paper 4 version 4 (14.12.90)
!
!    Model            Modification history from model version 5.2:
!   Version  date
!    5.3  07.11.01  Extended for use with 2-way conservative coupling
!                   scheme by introducing new output argument
!                   ADJUST_TARG and 4 new operation selectors.
!    5.5  11.04.03  Corrected bug in calculation of adjustment field
!                   for case = 3.
!  5.5  28.02.03  Add defined A20_1A (river routing). C. Bunton
!  6.0  12.09.03  Change DEF from A20 to A26. D. Robinson
!
!   Logical components covered :
!
!   Project task :
!
!   External documentation: Unified Model documentation paper No:
!                           Version:
!
#if defined(UM_JULES)
USE regrid_utils, ONLY: global_to_local_gridpt, find_value,                    &
                        gridpt_outside_proc_domain
USE riv_concerns, ONLY: runoff_recv_concern

USE jules_print_mgr, ONLY: jules_print
#endif

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
!
IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
   gaps_lambda_srce                                                            &
                            ! number lambda gaps in source grid
   ,gaps_phi_srce                                                              &
                            ! number phi gaps in source grid
   ,lrow_srce                                                                  &
                            ! first dimension of source arrays
   ,gaps_lambda_targ                                                           &
                            ! number lambda gaps in target grid
   ,gaps_phi_targ                                                              &
                            ! number phi gaps in target grid
   ,lrow_targ                                                                  &
                            ! first dimension of target arrays
   ,count_targ(gaps_lambda_targ,gaps_phi_targ)                                 &
                            ! no. of source boxes in target box
   ,base_targ(gaps_lambda_targ,gaps_phi_targ)                                  &
                            ! first index in list for target box
   ,index_srce( * )                                                            &
                            ! list of source box indices
   ,global_src_lambda_gaps                                                     &
                            ! lambda gaps in global src grid
   ,grid                                                                       &
                            ! grid type (e.g. atmos, river ...)
   ,adjust
                            ! selects normal or adjust mode

LOGICAL, INTENT(IN) ::                                                         &
   invert_srce                                                                 &
                            ! DATA_SRC rows in reverse order
   ,want                                                                       &
                            ! indicator of wanted points in mask
   ,mask_targ(lrow_targ,*)
                            ! land/sea mask for target grid

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
   weight( * )              ! list of weights for source boxes

REAL(KIND=real_jlslsm) ::                                                      &
   data_srce(lrow_srce,*)
                            ! data on source grid (IN or INOUT depending
                            ! on the value of variable adjust)
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
   data_targ(lrow_targ,*)
                            ! data on target grid

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
   adjust_targ(lrow_targ,*) ! factors by which DATA_SRCE
                            ! must be adjusted to give DATA_TARG

! Work variables
INTEGER ::                                                                     &
   ip                                                                          &
                            ! pointer into lists
   ,i                                                                          &
                            ! loop index
   ,ix1(gaps_lambda_srce * gaps_phi_srce)                                      &
                            ! working SRCE LAMBDA indices
   ,iy1(gaps_lambda_srce * gaps_phi_srce)                                      &
                            ! working SRCE PHI indices
   ,ix2,iy2
                            ! working TARG LAMBDA/PHI indices

REAL(KIND=real_jlslsm) ::                                                      &
   temp_targ                                                                   &
                            ! workspace for area-average
   ,delta                                                                      &
                            ! additive adjustment
   ,ratio                                                                      &
                            ! multiplicative adjustment
   ,data_src
                            ! temporal storage of data_srce

#if defined(UM_JULES)
LOGICAL :: found, not_within
#endif

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='DO_AREAVER'

!-------------------------------------------------------------------------------
!     Loop over all target boxes and calculate values as required.
!
!     The weights and source box indices are recorded in continuous
!     lists. COUNT_TARG indicates how many consecutive entries in these
!     lists apply to each target box.
!
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,                 &
                        zhook_handle)

DO iy2 = 1,gaps_phi_targ
  DO ix2 = 1,gaps_lambda_targ
    IF (mask_targ(ix2,iy2) .EQV. want) THEN
      IF (count_targ(ix2,iy2) /= 0) THEN
        temp_targ = 0.0
        DO i = 1,count_targ(ix2,iy2)
          ip = base_targ(ix2,iy2) + i
          ix1(i) = MOD(index_srce(ip) - 1,global_src_lambda_gaps) + 1
          iy1(i)=(index_srce(ip) - 1) / global_src_lambda_gaps + 1
          IF (invert_srce) iy1(i) = gaps_phi_srce - iy1(i) + 1
#if defined(UM_JULES)
          ! Call only if atmosphere grid is used (=>grid ==1; grid==2 means
          ! river grid). In this case, runoff_recv_concern must be properly
          ! allocated and initialized
          IF (grid == 1) THEN
            ! check if src point is in proc domain
            not_within = gridpt_outside_proc_domain( ix1(i), iy1(i), grid )
            data_src = 0.0

            ! if not check in your recv Concerns
            IF (not_within) THEN
              found = .FALSE.
              CALL find_value( ix1(i), iy1(i), SIZE(runoff_recv_concern, 1),   &
                               runoff_recv_concern, data_src, found )
              IF ( .NOT. found ) CALL jules_print('do_areaver',                &
                                                  'Error, Src Not Found!')
            ELSE
              CALL global_to_local_gridpt( ix1(i), iy1(i), grid )
              data_src = data_srce(ix1(i),iy1(i))
            END IF
          ELSE
            data_src = data_srce(ix1(i),iy1(i))
          END IF
#else
          data_src = data_srce(ix1(i),iy1(i))
#endif
          temp_targ = temp_targ + weight(ip) * data_src
        END DO
      ELSE
        IF (adjust == 5) THEN
          temp_targ = 0.0
        ELSE IF (adjust == 6) THEN
          temp_targ = 1.0
        ELSE
          temp_targ = data_targ(ix2,iy2)
        END IF
      END IF
      IF (adjust == 0) THEN
        data_targ(ix2,iy2) = temp_targ
      ELSE IF (adjust == 1) THEN
        delta = data_targ(ix2,iy2) - temp_targ
        DO i = 1,count_targ(ix2,iy2)
          data_srce(ix1(i),iy1(i))=data_srce(ix1(i),iy1(i)) + delta
        END DO
      ELSE IF (adjust == 2 .AND. temp_targ /= 0.0) THEN
        ratio = data_targ(ix2,iy2) / temp_targ
        DO i = 1,count_targ(ix2,iy2)
          data_srce(ix1(i),iy1(i))=data_srce(ix1(i),iy1(i)) * ratio
        END DO
      ELSE IF (adjust == 3) THEN
        adjust_targ(ix2,iy2) = temp_targ - data_targ(ix2,iy2)
      ELSE IF (adjust == 4) THEN
        IF (temp_targ == 0) THEN
          adjust_targ(ix2,iy2) = 1.0
        ELSE
          adjust_targ(ix2,iy2) = data_targ(ix2,iy2) / temp_targ
        END IF
      ELSE IF (adjust == 5) THEN
        data_targ(ix2,iy2) = data_targ(ix2,iy2) + temp_targ
      ELSE IF (adjust == 6) THEN
        data_targ(ix2,iy2) = data_targ(ix2,iy2) * temp_targ
      END IF
    END IF
  END DO
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,                &
                        zhook_handle)
RETURN

END SUBROUTINE do_areaver
!###############################################################################
!###############################################################################
END MODULE areaver_mod
