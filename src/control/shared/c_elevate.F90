! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module for elevation values.

MODULE c_elevate

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.

! Declarations:

USE max_dimensions, ONLY: nsurft_max
USE missing_data_mod, ONLY: rmdi

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

#if !defined(UM_JULES)
REAL(KIND=real_jlslsm) :: z_land_io = rmdi
                                   ! Single point gridbox mean elevation
REAL(KIND=real_jlslsm) :: surf_hgt_band(nsurft_max) = rmdi
                                        ! Spatially invariant elevation bands
#endif

LOGICAL :: l_elev_absolute_height(nsurft_max)
                                   ! F - tile surf_hgt are
                                   ! anomalies from forcing data
                                   ! altitude (default JULES behaviour)
                                   ! T - tile surf_hgt are absolute
                                   ! elevations above sea level

REAL(KIND=real_jlslsm) ::                                                      &
 surf_hgt_io(nsurft_max)

! In example JULES control files, these are all 0, so initialise at that
DATA surf_hgt_io / nsurft_max * 0.0 /
DATA l_elev_absolute_height / nsurft_max * .FALSE. /

! Namelist used in the UM only
NAMELIST  / jules_elevate/ surf_hgt_io, l_elev_absolute_height

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='C_ELEVATE'

CONTAINS

SUBROUTINE print_nlist_jules_elevate()
USE jules_print_mgr, ONLY: jules_print
IMPLICIT NONE
CHARACTER(LEN=50000) :: lineBuffer

CALL jules_print('c_elevate',                                                  &
    'Contents of namelist jules_elevate')

WRITE(lineBuffer,*)' surf_hgt_io = ',surf_hgt_io
CALL jules_print('c_elevate',lineBuffer)

WRITE(lineBuffer,*)' l_elev_absolute_height = ',l_elev_absolute_height
CALL jules_print('c_elevate',lineBuffer)

CALL jules_print('c_elevate',                                                  &
    '- - - - - - end of namelist - - - - - -')

END SUBROUTINE print_nlist_jules_elevate

#if defined(UM_JULES) && !defined(LFRIC)
SUBROUTINE read_nml_jules_elevate (unitnumber)

! Description:
!  Read the JULES_ELEVATE namelist

USE setup_namelist, ONLY: setup_nml_type
USE check_iostat_mod, ONLY: check_iostat
USE UM_parcore,       ONLY: mype

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

INTEGER :: my_comm
INTEGER :: mpl_nml_type
INTEGER :: ErrorStatus
INTEGER :: icode
CHARACTER(LEN=errormessagelength) :: iomessage
REAL(KIND=jprb) :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_ELEVATE'

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 2
INTEGER, PARAMETER :: n_real = nsurft_max
INTEGER, PARAMETER :: n_log  = nsurft_max

TYPE :: my_namelist
  SEQUENCE
  REAL(KIND=real_jlslsm)    :: surf_hgt_io(nsurft_max)
  LOGICAL :: l_elev_absolute_height(nsurft_max)
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type, n_real_in = n_real,             &
                    n_log_in = n_log )

IF (mype == 0) THEN

  READ (UNIT = unitnumber, NML = jules_elevate, IOSTAT = errorstatus,          &
        IOMSG = iomessage)
  CALL check_iostat(errorstatus, "namelist jules_elevate", iomessage)

  my_nml % surf_hgt_io = surf_hgt_io
  my_nml % l_elev_absolute_height = l_elev_absolute_height

END IF

CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN

  surf_hgt_io = my_nml % surf_hgt_io
  l_elev_absolute_height = my_nml % l_elev_absolute_height

END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_nml_jules_elevate
#endif

END MODULE c_elevate
