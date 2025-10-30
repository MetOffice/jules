#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt

!Intermediate routine to provide a single coupling point when initialising
!via the UM

!Due to a variable typing issue caused by variables usually passed round by
!include files, this routine will not compile as a module
MODULE surf_couple_initialise_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName='SURF_COUPLE_INITIALISE_MOD'

CONTAINS

SUBROUTINE surf_couple_initialise(a_step,                                      &
                                  a_inthd_arg,                                 &
                                  land_index,                                  &
                                  !Arguments for river routing
                                  a_realhd,                                    &
                                  xpa, xua, xva, ypa, yua, yva)

!USE in relevant subroutines
USE init_veg_mod,           ONLY: init_veg
USE init_a2t_4a_mod,        ONLY: init_a2t_4a
USE jules_init_mod,         ONLY: jules_init
USE init_urban_mod,         ONLY: init_urban
USE soil_biogeochem_utils_mod, ONLY: calc_litc_norm

!USE in relevant variables from UM modules
USE nlsizes_namelist_mod,   ONLY: ntiles, land_field,                          &
                                  a_len_inthd, a_len_realhd,                   &
                                  aocpl_row_length, aocpl_p_rows

USE model_time_mod,         ONLY: stepim
USE submodel_mod,           ONLY: atmos_im
USE umPrintMgr,             ONLY: umprint
USE ereport_mod,            ONLY: ereport

!USE in relevant variables from JULES modules
USE jules_surface_mod,      ONLY: l_aggregate
USE jules_rivers_mod,       ONLY: l_rivers
USE jules_irrig_mod,        ONLY: l_irrig_dmd
USE assign_irrig_fraction_mod, ONLY: assign_irrig_fraction
USE update_irrig_variables_mod, ONLY: update_irrig_variables
USE jules_soil_biogeochem_mod, ONLY: l_layeredC
USE veg_param,                 ONLY: litc_norm


USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

USE errormessagelength_mod, ONLY: errormessagelength

!USE in instances of the JULES TYPES
USE atm_fields_mod, ONLY: psparms, trif_vars, urban_param, progs, crop_vars,   &
                          ainfo, lake_vars

IMPLICIT NONE

!Arguments and includes
INTEGER, INTENT(IN)     :: a_step
INTEGER, INTENT(IN OUT)  :: a_inthd_arg(a_len_inthd)
INTEGER, INTENT(IN)     :: land_index(MAX(1,land_field))

!For river routing
REAL(KIND=real_jlslsm),    INTENT(IN)     :: a_realhd(a_len_realhd)
REAL(KIND=real_jlslsm),    INTENT(OUT)    :: xpa(aocpl_row_length+1)
                                                    !tp longitude coordinate
REAL(KIND=real_jlslsm),    INTENT(OUT)    :: xua(0:aocpl_row_length)
                                                    !u  longitude coordinate
REAL(KIND=real_jlslsm),    INTENT(OUT)    :: xva(aocpl_row_length+1)
                                                    !v  longitude coordinate
REAL(KIND=real_jlslsm),    INTENT(OUT)    :: ypa(aocpl_p_rows)
                                                    !tp latitude  coordinate
REAL(KIND=real_jlslsm),    INTENT(OUT)    :: yua(aocpl_p_rows)
                                                    !u  latitude  coordinate
REAL(KIND=real_jlslsm),    INTENT(OUT)    :: yva(0:aocpl_p_rows)
                                                    !v  latitude  coordinate

!Local variables
INTEGER :: triffid_period_arg
INTEGER :: nstep_since_triffid
INTEGER :: nstep_since_riv

! Error reporting
INTEGER                           :: icode  ! =0 normal exit; >0 error exit
CHARACTER(LEN=errormessagelength) :: cmessage
CHARACTER(LEN=*)                  :: RoutineName

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle
PARAMETER(RoutineName='SURF_COUPLE_INITIALISE')

!End of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!Grab the relevant items from inthd and give them a more memorable name
triffid_period_arg  = a_inthd_arg(22)
  !triffid_period in days
nstep_since_triffid = a_inthd_arg(23)
  !Number of atmosphere timesteps since the last call to TRIFFID
nstep_since_riv     = a_inthd_arg(16)
  !Number of atmosphere timesteps since the last call to River routing

! Set l_aggregate appropriately. REVIEW FOR FLEXIBLE TILES?
IF (ntiles == 1) THEN
  l_aggregate = .TRUE.
ELSE
  l_aggregate = .FALSE.
END IF

! Copying urban prognosics from D1 array to JULES module. #231 moved the call
! to init_urban here from init_veg, which is not called on a CRUN, however,
! sparm is called before here from up_ancil -> update_veg. D1 ztm is now passed
! in the call to sparm from the UM module in init_veg and update_veg, following
! the example of z0m_soil, as it was still causing problems on the CRUN from
! update_veg.
CALL init_urban (urban_param)

IF (l_layeredc) THEN
  IF (land_field  >   0) THEN
    ! Calculate litc_norm
    CALL calc_litc_norm(litc_norm)
  END IF
END IF

IF ( stepim(atmos_im) == 0 ) THEN
  IF (land_field  >   0) THEN
    CALL init_veg(a_step, triffid_period_arg, nstep_since_triffid,             &
                  trif_vars, progs)
  ELSE
    !Skip init_veg if land_field=0 for this PE.
    CALL umPrint(                                                              &
      'surf_couple_initialise; skip init_veg, land_field=0 on this PE' ,       &
       src='surf_couple_initialise')
  END IF
END IF

! River routing
IF (l_rivers) THEN
  ! Initialise the step counter for river routing.
  nstep_since_riv = 0

  ! Set up the ATMOS grid lat./long.coords for regridding to river routing grid
  CALL init_a2t_4a(a_realhd, xpa, xua, xva, ypa, yua, yva)
END IF !l_rivers

! 7.5 Initialise JULES variables
CALL jules_init(land_index,psparms,progs,lake_vars,ainfo)

!Copy back the relevant items from inthd and give them a more memorable name
a_inthd_arg(22) = triffid_period_arg
  !triffid_period in days
a_inthd_arg(23) = nstep_since_triffid
  !Number of atmosphere timesteps since the last call to TRIFFID
a_inthd_arg(16) = nstep_since_riv
  !Number of atmosphere timesteps since the last call to River routing

! Update irrigated fraction and soil moisture in the irrigated fraction
IF ( l_irrig_dmd ) THEN
  CALL assign_irrig_fraction(crop_vars,ainfo)
  CALL update_irrig_variables(crop_vars,psparms,ainfo)
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE surf_couple_initialise
END MODULE surf_couple_initialise_mod
#endif
