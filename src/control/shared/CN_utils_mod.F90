! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE CN_utils_mod

USE ereport_mod, ONLY: ereport

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

PRIVATE
PUBLIC calc_n_comps_triffid, get_can_ave_fac, nleaf_from_lai

!-----------------------------------------------------------------------------
! Description:
!   Some functions to calculate the N contents for leaf, root, and wood
!   in TRIFFID.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in VEGETATION
!
! Code Description:
!   Language: Fortran 90.
!   https://code.metoffice.gov.uk/trac/jules/wiki/AddingNewSubroutines
!   3 Sept. 2015
!-----------------------------------------------------------------------------
CONTAINS

SUBROUTINE calc_n_comps_triffid(l, n, phen, lai_bal, wood, root, n_leaf,       &
                                n_root, n_stem, dvi_cpft)

USE jules_vegetation_mod, ONLY:                                                &
!  imported scalars that are not altered
    can_rad_mod, l_leaf_n_resp_fix, l_trait_phys

USE pftparm, ONLY:                                                             &
!  imported arrays that are not altered
    a_ws, kn, lma, nl0, nmass, nr, nsw, hw_sw, nr_nl, ns_nl, sigl

USE trif, ONLY:                                                                &
!  imported arrays that are not altered
    retran_l

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  l,  &!  Land point number.
  n    !  PFT number.

REAL(KIND=real_jlslsm), INTENT(IN)    ::                                       &
  lai_bal                                                                      &
             ! Balanced LAI
, wood                                                                         &
             ! Wood carbon (kg C m-2)
, root                                                                         &
             ! Root carbon (kg C m-2)
, phen                                                                         &
             ! Phenological state (=LAI/lai_bal)
, dvi_cpft(:,:)
             !  Development index for crop tiles

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT)   ::                                       &
  n_leaf                                                                       &
             ! Leaf N content (kg N m-2)
, n_root                                                                       &
             ! Root N content (kg N m-2)
, n_stem
             ! Wood N content (kg N m-2)

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER             ::                                                         &
  errcode
             ! Error code to pass to ereport.

REAL(KIND=real_jlslsm)                ::                                       &
  fstem                                                                        &
             ! Ratio of respiring stem wood to total wood
, nl_ave                                                                       &
             ! Leaf nitrogen per unit LAI, averaged over the canopy (kg m-2).
, x_tmp
             ! Temporary factor used in calculation of canopy average.

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'CALC_N_COMPS_TRIFFID'

!-----------------------------------------------------------------------------
!end of header

IF ( l_leaf_n_resp_fix ) THEN

  ! Calculate leaf N, first for full leaf, then adjust for phenology/labile
  ! pool.
  n_leaf = nleaf_from_lai( l, n, lai_bal, dvi_cpft )
  n_leaf = n_leaf * phen + n_leaf * (1.0 - phen) * (1.0 + retran_l(n)) / 2.0

ELSE

  ! Calculate the canopy-average leaf nitrogen.
  IF ( l_trait_phys ) THEN

    SELECT CASE ( can_rad_mod )
    CASE ( 1, 6 )
      x_tmp = kn(n) * lai_bal
      IF ( x_tmp > EPSILON(0.0) ) THEN
        nl_ave = nmass(n) * lma(n) * (1 - EXP(-x_tmp))                         &
                 / x_tmp
      ELSE
        nl_ave = nmass(n) * lma(n)
      END IF
    CASE ( 4, 5 )
      nl_ave = nmass(n) * lma(n) * (1.0 - EXP(-kn(n))) / kn(n)
    CASE DEFAULT
      errcode = 101  !  a hard error
      CALL ereport(RoutineName, errcode,                                       &
                   'can_rad_mod should be 1, 4, 5 or 6')
    END SELECT

  ELSE

    SELECT CASE ( can_rad_mod )
    CASE ( 1, 6 )
      x_tmp = kn(n) * lai_bal
      IF ( x_tmp > EPSILON(0.0) ) THEN
        nl_ave = nl0(n) * sigl(n) * (1 - EXP(-x_tmp))                          &
                 / x_tmp
      END IF
    CASE ( 4,5 )
      nl_ave = nl0(n) * sigl(n) * (1.0 - EXP(-kn(n))) / kn(n)
    CASE DEFAULT
      errcode = 101  !  a hard error
      CALL ereport(RoutineName, errcode,                                       &
                   'can_rad_mod should be 1, 4, 5 or 6')
    END SELECT

  END IF  !  l_trait_phys

  ! Calculate leaf N, adjusting for phenology/labile pool.
  n_leaf = ( nl_ave * lai_bal * phen ) +                                       &
            ((1.0 - phen) * nl_ave * lai_bal * (1.0 + retran_l(n)) / 2.0)

END IF

!------------------------------------------------------------------------------
! Calculate root and stem N.
!------------------------------------------------------------------------------
IF ( l_trait_phys ) THEN
  n_root = nr(n) * root
  fstem  = 1.0 / a_ws(n)
  n_stem = wood * ( (nsw(n) * fstem) + (1.0 - fstem) * nsw(n) * hw_sw(n))
ELSE
  n_root = nr_nl(n) * nl0(n) * root
  n_stem = ns_nl(n) * nl0(n) * wood
END IF

END SUBROUTINE calc_n_comps_triffid

!#############################################################################
FUNCTION get_can_ave_fac( ft, land_pts, veg_pts, veg_index, lai )              &
         RESULT( can_averaging_fac)

! Calculate factor converting N at top of canopy to canopy average.

USE pftparm, ONLY:                                                             &
!  imported arrays that are not altered
    kn, knl, kpar

USE jules_vegetation_mod, ONLY:                                                &
!  imported scalars that are not altered
    l_leaf_n_resp_fix, can_rad_mod

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  ft,                                                                          &
    !  Index of PFT.
  land_pts,                                                                    &
    !  Number of land points.
  veg_pts
    !  Number of vegetation points.

INTEGER, INTENT(IN) ::                                                         &
  veg_index(land_pts)
    !  Indices of vegetated points on the land grid.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  lai(land_pts)
    !  Leaf area index.

!-----------------------------------------------------------------------------
! Function result.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) :: can_averaging_fac(land_pts)
    !  Factor converting N at top of canopy to canopy average.

!-----------------------------------------------------------------------------
! Local scalars.
!-----------------------------------------------------------------------------
INTEGER             ::                                                         &
  errcode,                                                                     &
             ! Error code to pass to ereport.
  l,m        !  Indices.

REAL(KIND=real_jlslsm) ::                                                      &
  kval,                                                                        &
    !  Decay parameter for chosen model.
  x_tmp
    !  Work value.

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'GET_CAN_AVE_FAC'

!-----------------------------------------------------------------------------
!end of header

! Initialise result with value indicating no variation of N through the canopy.
can_averaging_fac(:) = 1.0

IF ( l_leaf_n_resp_fix ) THEN
  SELECT CASE ( can_rad_mod )

  CASE ( 1, 6 )
    !       Exponential decay with LAI.
    IF ( can_rad_mod == 1 ) THEN
      kval = kpar(ft)
    ELSE
      kval = knl(ft)
    END IF
    IF ( kval > EPSILON(0.0) ) THEN
      DO m = 1,veg_pts
        l = veg_index(m)
        x_tmp = kval * lai(l)
        IF ( x_tmp > EPSILON(0.0) ) THEN
          can_averaging_fac(l) = (1.0 - EXP(-x_tmp)) / x_tmp
        END IF
      END DO
    END IF

  CASE ( 4, 5 )
    !       Exponential decay with layers.
    IF ( kn(ft) > EPSILON(0.0) ) THEN
      can_averaging_fac(:) = (1.0 - EXP(-kn(ft))) / kn(ft)
    END IF

  CASE DEFAULT
    errcode = 101  !  a hard error
    CALL ereport(RoutineName, errcode,                                         &
                 'can_rad_mod should be 1, 4, 5 or 6')

  END SELECT

ELSE
  !   l_leaf_n_resp_fix = F

  IF ( can_rad_mod == 6 .AND. knl(ft) > EPSILON(0.0) ) THEN
    DO m = 1,veg_pts
      l = veg_index(m)
      x_tmp = knl(ft) * lai(l)
      IF ( x_tmp > EPSILON(0.0) ) THEN
        can_averaging_fac(l) = (1.0 - EXP(-x_tmp)) / x_tmp
      END IF
    END DO
  ELSE
    can_averaging_fac(:) = 1.0
  END IF

END IF  !  l_leaf_n_resp_fix

END FUNCTION get_can_ave_fac

!#############################################################################
FUNCTION nleaf_from_lai( l, ft, lai, dvi_cpft, can_averaging_fac_in)           &
  RESULT( n_leaf )

! Calculate leaf N content given LAI.
!
! Note: The argument can_averaging_fac_in is optional so the code can be used
! (a) when an array of can_averaging_fac has been precalculated (efficient)
! (b) on a point-by-point basis when can_averaging_frac has not been
!     precalculated.

USE crop_utils_mod, ONLY:                                                      &
  leafc_from_prognostics, lma_from_prognostics

USE jules_surface_types_mod, ONLY: nnpft

USE pftparm, ONLY:                                                             &
!  imported arrays that are not altered
    lma, nl0, nmass, sigl

USE jules_vegetation_mod, ONLY:                                                &
!  imported scalars that are not altered
    l_trait_phys

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  ft,                                                                          &
    !  Index of functional type.
  l
    !  Land point index. Used with crops to get DVI.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  lai,                                                                         &
    !  Leaf area index.
  dvi_cpft(:,:)
    !  Development index for crop tiles

!-----------------------------------------------------------------------------
! Optional arguments with INTENT(IN).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN), OPTIONAL ::                                &
  can_averaging_fac_in
    !  Factor converting N at top of canopy to canopy average.

!-----------------------------------------------------------------------------
! Function result.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) :: n_leaf
    !  Nitrogen content of leaves (kg m-2).

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
INTEGER, PARAMETER :: array_one(1) = 1
    !  A single value of 1, in an array, for passing as an argument.

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  can_averaging_fac,                                                           &
    !  Factor converting N at top of canopy to canopy average.
  c_leaf,                                                                      &
    !  Carbon in leaves (kg m-2).
  lma_tmp
    ! Leaf mass per area (kg leaf per m2 leaf area).

!-----------------------------------------------------------------------------
!end of header

!-----------------------------------------------------------------------------
! If canopy-averaging factor was not provided, calculate it.
!-----------------------------------------------------------------------------
IF ( PRESENT(can_averaging_fac_in) ) THEN
  can_averaging_fac = can_averaging_fac_in
ELSE
  can_averaging_fac = TRANSFER(                                                &
                        get_can_ave_fac( ft, 1, 1, array_one, [lai] ),         &
                        can_averaging_fac )
END IF

IF ( ft > nnpft ) THEN
  ! Crop PFTs.

  IF ( l_trait_phys ) THEN
    lma_tmp = lma_from_prognostics(ft - nnpft, dvi_cpft(l,ft - nnpft))
    n_leaf  = nmass(ft) * lma_tmp * lai * can_averaging_fac
  ELSE
    c_leaf  = leafc_from_prognostics(ft - nnpft, dvi_cpft(l,ft - nnpft), lai )
    n_leaf  = nl0(ft) * can_averaging_fac * c_leaf
  END IF

ELSE
  ! Non-crop PFTs.

  IF ( l_trait_phys ) THEN
    n_leaf = nmass(ft) * lma(ft) * lai * can_averaging_fac
  ELSE
    n_leaf = nl0(ft) * can_averaging_fac * sigl(ft) * lai
  END IF

END IF  !  PFTs


END FUNCTION nleaf_from_lai

!#############################################################################

END MODULE CN_utils_mod
