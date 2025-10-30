! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! This module contains variables used for reading in red data
! and initializations

MODULE red_io

USE max_dimensions, ONLY: npft_max
USE missing_data_mod, ONLY: imdi, rmdi
USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
! Set up variables to use in IO (a fixed size version of each array
! in red that we want to initialise).
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  mclass(npft_max) = imdi

REAL(KIND=real_jlslsm) ::                                                      &
  alpha_recrt(npft_max) = rmdi,                                                &
  crwn_area0(npft_max) = rmdi,                                                 &
  dom_order(npft_max) = rmdi,                                                  &
  height0(npft_max) = rmdi,                                                    &
  lai_bal0(npft_max) = rmdi,                                                   &
  mass0(npft_max) = rmdi,                                                      &
  massi(npft_max) = rmdi,                                                      &
  mort_base(npft_max) = rmdi,                                                  &
  phi_a(npft_max) = rmdi,                                                      &
  phi_g(npft_max) = rmdi,                                                      &
  phi_h(npft_max) = rmdi,                                                      &
  phi_l(npft_max) = rmdi

!-----------------------------------------------------------------------
! Set up a namelist for reading and writing these arrays
!-----------------------------------------------------------------------
NAMELIST  / jules_red /                                                        &
                      alpha_recrt, crwn_area0, dom_order,  height0, lai_bal0,  &
                      mass0, massi, mclass, mort_base, phi_a, phi_g, phi_h,    &
                      phi_l

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='RED_IO'

CONTAINS
SUBROUTINE print_nlist_jules_red()
USE jules_print_mgr, ONLY: jules_print
IMPLICIT NONE
CHARACTER(LEN=50000) :: lineBuffer

CALL jules_print('red_io',                                                     &
    'Contents of namelist jules_red')

WRITE(lineBuffer,*)' alpha_recrt = ',alpha_recrt
CALL jules_print('red_io',lineBuffer)
WRITE(lineBuffer,*)' crwn_area0 = ',crwn_area0
CALL jules_print('red_io',lineBuffer)
WRITE(lineBuffer,*)' dom_order = ',dom_order
CALL jules_print('red_io',lineBuffer)
WRITE(lineBuffer,*)' height0 = ',height0
CALL jules_print('red_io',lineBuffer)
WRITE(lineBuffer,*)' lai_bal0 = ',lai_bal0
CALL jules_print('red_io',lineBuffer)
WRITE(lineBuffer,*)' mass0 = ',mass0
CALL jules_print('red_io',lineBuffer)
WRITE(lineBuffer,*)' massi = ',massi
CALL jules_print('red_io',lineBuffer)
WRITE(lineBuffer,*)' mclass = ',mclass
CALL jules_print('red_io',lineBuffer)
WRITE(lineBuffer,*)' mort_base = ',mort_base
CALL jules_print('red_io',lineBuffer)
WRITE(lineBuffer,*)' phi_a = ',phi_a
CALL jules_print('red_io',lineBuffer)
WRITE(lineBuffer,*)' phi_g = ',phi_g
CALL jules_print('red_io',lineBuffer)
WRITE(lineBuffer,*)' phi_h = ',phi_h
CALL jules_print('red_io',lineBuffer)
WRITE(lineBuffer,*)' phi_l = ',phi_l
CALL jules_print('red_io',lineBuffer)
CALL jules_print('red_io',                                                     &
    '- - - - - - end of namelist - - - - - -')

END SUBROUTINE print_nlist_jules_red

#if defined(UM_JULES) && !defined(LFRIC)

SUBROUTINE read_nml_jules_red (unitnumber)

! Description:
!  Read the JULES_RED namelist

USE setup_namelist,   ONLY: setup_nml_type
USE check_iostat_mod, ONLY: check_iostat
USE UM_parcore,       ONLY: mype
USE errormessagelength_mod, ONLY: errormessagelength

USE parkind1,         ONLY: jprb, jpim
USE yomhook,          ONLY: lhook, dr_hook

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber
INTEGER :: my_comm
INTEGER :: mpl_nml_type
INTEGER :: ErrorStatus
INTEGER :: icode
REAL(KIND=jprb) :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_RED'
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

CHARACTER(LEN=errormessagelength) :: iomessage

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 2
INTEGER, PARAMETER :: n_int = 2 * npft_max
INTEGER, PARAMETER :: n_real = 11 * npft_max

TYPE :: my_namelist
  SEQUENCE
  INTEGER :: mclass(npft_max)
  INTEGER :: dom_order(npft_max)
  REAL(KIND=real_jlslsm) :: alpha_recrt(npft_max)
  REAL(KIND=real_jlslsm) :: crwn_area0(npft_max)
  REAL(KIND=real_jlslsm) :: height0(npft_max)
  REAL(KIND=real_jlslsm) :: lai_bal0(npft_max)
  REAL(KIND=real_jlslsm) :: mass0(npft_max)
  REAL(KIND=real_jlslsm) :: massi(npft_max)
  REAL(KIND=real_jlslsm) :: mort_base(npft_max)
  REAL(KIND=real_jlslsm) :: phi_a(npft_max)
  REAL(KIND=real_jlslsm) :: phi_g(npft_max)
  REAL(KIND=real_jlslsm) :: phi_h(npft_max)
  REAL(KIND=real_jlslsm) :: phi_l(npft_max)
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type, n_int_in = n_int,               &
                    n_real_in = n_real)

IF (mype == 0) THEN

  READ (UNIT = unitnumber, NML = jules_red, IOSTAT = errorstatus,              &
        IOMSG = iomessage)
  CALL check_iostat(errorstatus, "namelist jules_red", iomessage)

  my_nml % mclass      = mclass
  my_nml % alpha_recrt = alpha_recrt
  my_nml % crwn_area0  = crwn_area0
  my_nml % dom_order   = dom_order
  my_nml % height0     = height0
  my_nml % lai_bal0    = lai_bal0
  my_nml % mass0       = mass0
  my_nml % massi       = massi
  my_nml % mort_base   = mort_base
  my_nml % phi_a       = phi_a
  my_nml % phi_g       = phi_g
  my_nml % phi_h       = phi_h
  my_nml % phi_l       = phi_l

END IF

CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN

  mclass      = my_nml % mclass
  alpha_recrt = my_nml % alpha_recrt
  crwn_area0  = my_nml % crwn_area0
  dom_order   = my_nml % dom_order
  height0     = my_nml % height0
  lai_bal0    = my_nml % lai_bal0
  mass0       = my_nml % mass0
  massi       = my_nml % massi
  mort_base   = my_nml % mort_base
  phi_a       = my_nml % phi_a
  phi_g       = my_nml % phi_g
  phi_h       = my_nml % phi_h
  phi_l       = my_nml % phi_l

END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_nml_jules_red
#endif


#if !defined(UM_JULES)
SUBROUTINE read_nml_jules_red(nml_dir)

! Description:
!  Read the JULES_RED namelist (standalone)

USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE logging_mod, ONLY: log_info, log_fatal

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the

INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
CALL log_info("init_red", "Reading JULES_RED namelist...")

! Open the pft parameters namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'red_params.nml'),           &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_red",                                                   &
                 "Error opening namelist file jules_red.nml " //               &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

READ(namelist_unit, NML = jules_red, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_red",                                                   &
                 "Error reading namelist JULES_RED " //                        &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

! Close the namelist file
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_red",                                                   &
                 "Error closing namelist file jules_red.nml " //               &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

END SUBROUTINE read_nml_jules_red
#endif

END MODULE red_io
