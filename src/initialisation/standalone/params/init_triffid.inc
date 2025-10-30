#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE init_triffid(nml_dir)

USE missing_data_mod, ONLY:                                                    &
!  imported scalar parameters
     rmdi

USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE jules_surface_types_mod, ONLY: npft, nnpft

USE jules_vegetation_mod, ONLY: l_triffid, l_phenol, l_trif_crop, l_ag_expand, &
                                l_trif_biocrop

USE trif, ONLY: crop, harvest_type, harvest_freq, ag_expand,                   &
                g_area, g_grow, g_root, g_wood, lai_max, lai_min,              &
                alloc_fast, alloc_med, alloc_slow, dpm_rpm_ratio,              &
                retran_r, retran_l, harvest_ht

USE trif_io, ONLY: jules_triffid,                                              &
                         crop_io, harvest_type_io, harvest_freq_io,            &
                         ag_expand_io, g_area_io,g_grow_io,g_root_io,          &
                         g_wood_io,lai_max_io,lai_min_io,                      &
                         alloc_fast_io,alloc_med_io,alloc_slow_io,             &
                         dpm_rpm_ratio_io,retran_l_io,retran_r_io,             &
                         harvest_ht_io

USE errormessagelength_mod, ONLY: errormessagelength

USE pftparm, ONLY: a_wl, a_ws, eta_sl, b_wl

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises the TRIFFID parameters
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

! Work variables
INTEGER :: ERROR  ! Error indicator
INTEGER :: n      ! Loop counter
CHARACTER(LEN=errormessagelength) :: iomessage
REAL    :: tmp_minlai          ! Used for trif_biocrop check
INTEGER :: days_in_yr          ! Used for trif_biocrop check

!-----------------------------------------------------------------------------


! Nothing to do if neither triffid or phenology are selected
IF ( .NOT. l_triffid .AND. .NOT. l_phenol ) RETURN

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
CALL log_info("init_triffid", "Reading JULES_TRIFFID namelist...")

! Open the pft parameters namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'triffid_params.nml'),       &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_triffid",                                               &
                 "Error opening namelist file triffid_params.nml " //          &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

READ(namelist_unit, NML = jules_triffid, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_triffid",                                               &
                 "Error reading namelist JULES_TRIFFID " //                    &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

! Close the namelist file
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_triffid",                                               &
                 "Error closing namelist file triffid_params.nml " //          &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")


!-----------------------------------------------------------------------------
! Copy values from fixed length arrays used in namelist into allocated arrays
!-----------------------------------------------------------------------------
crop(:)    = crop_io(1:npft)
harvest_freq(:)    = harvest_freq_io(1:npft)
harvest_type(:)    = harvest_type_io(1:npft)
ag_expand(:)       = ag_expand_io(1:npft)
g_area(:)  = g_area_io(1:npft)
g_grow(:)  = g_grow_io(1:npft)
g_root(:)  = g_root_io(1:npft)
g_wood(:)  = g_wood_io(1:npft)
lai_max(:) = lai_max_io(1:npft)
lai_min(:) = lai_min_io(1:npft)
alloc_fast(:) = alloc_fast_io(1:npft)
alloc_med(:)  = alloc_med_io(1:npft)
alloc_slow(:) = alloc_slow_io(1:npft)
dpm_rpm_ratio(:) = dpm_rpm_ratio_io(1:npft)
retran_r(:)   = retran_r_io(1:npft)
retran_l(:)   = retran_l_io(1:npft)
harvest_ht(:) = harvest_ht_io(1:npft)

!-----------------------------------------------------------------------------
! Check that all required variables were present in the namelist.
! The namelist variables were initialised to rmdi.
!-----------------------------------------------------------------------------
ERROR = 0
IF ( ANY( crop(1:nnpft) < 0 ) ) THEN  ! crop was initialised to < 0
  ERROR = 1
  CALL log_error("init_triffid", "No value for crop")
END IF
IF ( ANY( ABS( g_area(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_triffid", "No value for g_area")
END IF
IF ( ANY( ABS( g_grow(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_triffid", "No value for g_grow")
END IF
IF ( ANY( ABS( g_root(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_triffid", "No value for g_root")
END IF
IF ( ANY( ABS( g_wood(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_triffid", "No value for g_wood")
END IF
IF ( ANY( ABS( lai_max(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_triffid", "No value for lai_max")
END IF
IF ( ANY( ABS( lai_min(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_triffid", "No value for lai_min")
END IF
IF ( ANY( ABS( alloc_fast(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_triffid", "No value for alloc_fast")
END IF
IF ( ANY( ABS( alloc_med(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_triffid", "No value for alloc_med")
END IF
IF ( ANY( ABS( alloc_slow(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_triffid", "No value for alloc_slow")
END IF
IF ( ANY( ABS( dpm_rpm_ratio(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_triffid", "No value for dpm_rpm_ratio")
END IF
IF ( ANY( ABS( retran_r(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_triffid", "No value for retran_r")
END IF
IF ( ANY( ABS( retran_l(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_triffid", "No value for retran_l")
END IF

! Harvest variables are only required if l_trif_biocrop is true
IF ( l_trif_biocrop ) THEN

  IF ( ANY( harvest_type(1:nnpft) < 0 ) ) THEN  ! harvest_type was initialised to < 0
    ERROR = 1
    CALL log_error("init_triffid", "No value for harvest_type")
  END IF

  ! harvest_ht, harvest_freq only required if ANY(harvest_type(1:nnpft) = 2
  IF ( ANY( harvest_type(1:nnpft) == 2 ) ) THEN ! require harvest_ht and harvest_freq

    IF ( ANY( harvest_freq(1:nnpft) < 0 ) ) THEN  ! harvest_freq was initialised to < 0
      ERROR = 1
      CALL log_error("init_triffid", "No value for harvest_freq")
    END IF

    IF ( ANY( ABS( harvest_ht(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
      ERROR = 1
      CALL log_error("init_triffid", "No value for harvest_ht")
    END IF
  END IF ! any (harvest_type ==2)

  ! only warn about ag_expand if l_ag_expand is true
  IF ( l_ag_expand ) THEN
    IF ( ANY( ag_expand(1:nnpft) < 0 ) ) THEN  ! ag_expand was initialised to < 0
      ERROR = 1
      CALL log_error("init_triffid", "No value for ag_expand")
    END IF
    IF ( SUM(ag_expand) < 1 ) THEN
      CALL log_warn("init_triffid", "ag_expand = 0 for all PFTs: Assisted" //  &
                    " expansion will never occur")
    END IF
  END IF ! l_ag_expand

  ! If using periodic harvest (harvest_type = 2),
  ! Check lai_min is appropriate to harvest_ht
  DO n = 1,nnpft
    IF (harvest_type(n) == 2) THEN

      ! Calculate value of lai_bal at harvest_ht
      tmp_minlai = ( harvest_ht(n) * a_ws(n) * eta_sl(n) / a_wl(n) )           &
                   ** ( 1 / ( b_wl(n) - 1 ) )

      IF (lai_min(n) > tmp_minlai) THEN
        ERROR = 1
        CALL log_error("init_triffid",                                         &
                       "lai_min is too large for harvest height for PFT " //   &
                       to_string(n) // ": Reset to < " // to_string(tmp_minlai) )

      END IF ! lai_min > tmp_minlai

    END IF ! harvest_type =2

    ! Check natural PFTs (crop=0) aren't harvested or expanded
    IF (crop(n) == 0) THEN
      ! warn if harvest is enabled (harvest_type>0)
      IF (harvest_type(n) > 0) THEN
        CALL log_warn("init_triffid", "harvest_type > 0 specified for PFT" //  &
                      to_string(n) // ": Harvesting is disabled for " //       &
                      " natural PFTs" )
      END IF

      ! warn if assisted expansion is enabled (ag_expand=1)
      IF (ag_expand(n) > 0) THEN
        CALL log_warn("init_triffid", "ag_expand = 1 specified for PFT" //     &
                      to_string(n) // ": Assisted expansion is disabled" //    &
                      " for natural PFTs" )
      END IF

    END IF ! crop(n) = 0
  END DO ! n=1,nnpft


END IF ! l_trif_biocrop



IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_triffid",                                               &
                 "Variable(s) missing from namelist - see earlier " //         &
                 "error message(s)")

RETURN

END SUBROUTINE init_triffid
#endif

