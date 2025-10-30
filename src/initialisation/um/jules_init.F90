! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Subroutine JULES_INIT ----------------------------------
!
! Description: Initialisation of JULES.
!
! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.
!
!   Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
!   This file belongs in section: Land

MODULE jules_init_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_INIT_MOD'

CONTAINS

! For the scm the call takes a few extra arguments due to differences in
! module from the main UM
SUBROUTINE jules_init(land_index,psparms,progs,lake_vars,ainfo                 &
#if defined(SCMA)
                      ,land_field, ntiles, sm_levels                           &
#endif
                      )

!Sort out scm-dependent USE statements
#if defined(SCMA)
USE s_main_force,             ONLY: rho_snow_grnd => i_rho_snow_grnd,          &
                                    snowdepth     => i_snowdepth,              &
                                    nsnow,                                     &
                                    ds            => i_ds,                     &
                                    sice          => i_sice,                   &
                                    sliq          => i_sliq
#else
USE jules_soil_mod,           ONLY: sm_levels
USE nlsizes_namelist_mod,     ONLY: ntiles, land_field

USE atm_fields_mod,           ONLY: rho_snow_grnd,                             &
                                    snowdepth,                                 &
                                    nsnow,                                     &
                                    ds,                                        &
                                    sice,                                      &
                                    sliq
#endif

!USE in relevant variables from UM modules
USE atm_fields_mod,           ONLY: clapp_horn, sat_soilw_suction,             &
                                    sat_soil_cond, therm_cap, therm_cond,      &
                                    vol_smc_crit, vol_smc_wilt, vol_smc_sat
USE trignometric_mod,         ONLY: true_longitude, true_latitude

USE conversions_mod,          ONLY: recip_pi_over_180
USE dyn_coriolis_mod,         ONLY: f3_at_u
USE theta_field_sizes,        ONLY: t_i_length, t_j_length

!USE in relevant variables from JULES modules

USE jules_surface_mod,        ONLY: l_aggregate, l_flake_model
USE jules_snow_mod,           ONLY: nsmax
USE lake_mod,                 ONLY: nusselt_0
USE ancil_info,               ONLY: nsoilt, ainfo_type

USE parkind1,                 ONLY: jprb, jpim
USE yomhook,                  ONLY: lhook, dr_hook

!TYPE definitions
USE p_s_parms, ONLY: psparms_type
USE prognostics, ONLY: progs_type
USE lake_mod,    ONLY: lake_type
IMPLICIT NONE


! Arguments for scm
#if defined(SCMA)
INTEGER, INTENT(IN) :: land_field
INTEGER, INTENT(IN) :: ntiles
INTEGER, INTENT(IN) :: sm_levels
#endif
! Subroutine argument (have to order this way to ensure land_field is defined)
INTEGER, INTENT(IN) :: land_index    (MAX(1,land_field))

!TYPES containing field data (IN OUT)
TYPE(psparms_type), INTENT(IN OUT) :: psparms
TYPE(progs_type), INTENT(IN OUT) :: progs
TYPE(lake_type), INTENT(IN OUT) :: lake_vars
TYPE(ainfo_type), INTENT(IN OUT) :: ainfo

! WORK variables:
INTEGER :: i,j,l,m,n

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*),   PARAMETER :: RoutineName='JULES_INIT'

! START OF EXECUTABLE CODE

! Dimension the JULES fields.
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

#if ! defined(SCMA)
! Initialise JULES arrays.
! Soil properties on soil moisture levels.
DO m = 1, nsoilt
  DO l = 1, land_field
    DO n = 1, sm_levels
      psparms%bexp_soilt(l,m,n)   = clapp_horn(l)
      psparms%sathh_soilt(l,m,n)  = sat_soilw_suction(l)
      psparms%hcap_soilt(l,m,n)   = therm_cap(l)
      psparms%smvccl_soilt(l,m,n) = vol_smc_crit(l)
      psparms%smvcwt_soilt(l,m,n) = vol_smc_wilt(l)
      psparms%smvcst_soilt(l,m,n) = vol_smc_sat(l)
    END DO
    DO n = 0, sm_levels
      psparms%hcon_soilt(l,m,n)   = therm_cond(l)
      psparms%satcon_soilt(l,m,n) = sat_soil_cond(l)
    END DO
  END DO
END DO
#endif

! snowdepth needed in AP1 for JULES radiation
!---------------------------------------------
DO n = 1, ntiles
  DO l = 1, land_field
    progs%snowdepth_surft(l,n) = snowdepth(l,n)
    progs%rho_snow_grnd_surft(l,n) = rho_snow_grnd(l,n)
    progs%nsnow_surft(l,n)         = nsnow(l,n)
  END DO
END DO
IF (nsmax > 0) THEN
  progs%ds_surft(:,:,:)            = ds(:,:,:)
  progs%sice_surft(:,:,:)          = sice(:,:,:)
  progs%sliq_surft(:,:,:)          = sliq(:,:,:)
END IF

! FLake model
!--------------
IF (     l_flake_model                                                         &
    .AND. ( .NOT. l_aggregate)) THEN

  ! initialise the Nusselt number
  lake_vars%nusselt_gb(:) = nusselt_0

  DO l = 1,land_field

    j=(land_index(l) - 1) / t_i_length + 1
    i = land_index(l) - (j-1) * t_i_length

    ! set the Coriolis parameter : ABSOLUTE VALUE
    !
    ! To get the value at theta points,
    ! average the adjacent values at u points.
    !
    lake_vars%coriolis_param_gb(l) = ABS( (f3_at_u(i,j) + f3_at_u(i-1,j)) / 2.0 )

  END DO

END IF

! Fill latitude and longitude in ainfo with UM module variables
!--------------------------------------------------------------------
DO j = 1,t_j_length
  DO i = 1,t_i_length
    ainfo%latitude(i,j)  =  recip_pi_over_180 * true_latitude(i,j)
    ainfo%longitude(i,j) =  recip_pi_over_180 * true_longitude(i,j)
  END DO
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE jules_init
END MODULE jules_init_mod

