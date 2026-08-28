#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE imogen_update_clim_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE imogen_update_clim(progs, imgn_drive, imgn_vars, ainfo)

USE model_time_mod, ONLY: current_time

USE ancil_info, ONLY: dim_cs1, dim_cslayer, land_pts, nsoilt

USE conversions_mod, ONLY: ppm_to_mmr

USE io_constants, ONLY: imogen_unit

USE aero, ONLY: co2_mmr

USE imogen_run, ONLY:                                                          &
  include_co2, c_emissions, file_scen_co2_ppmv,                                &
  co2_init_ppmv, include_non_co2_radf,                                         &
  land_feed_co2, land_feed_ch4,                                                &
  nyr_non_co2, file_non_co2_radf, l_change_metdata, change_metdata_method

USE imogen_time, ONLY: mm, md, nsdmax

USE imogen_anlg_vals, ONLY:                                                    &
  q2co2, f_ocean, kappa_o, lambda_l, lambda_o, mu

USE imogen_constants, ONLY: n_olevs

USE radf_co2_mod, ONLY: radf_co2

USE radf_non_co2_mod, ONLY: radf_non_co2

USE radf_ch4_mod, ONLY: radf_ch4

USE delta_temp_mod, ONLY: delta_temp

USE pattern_scaling_mod, ONLY: pattern_scaling

USE drdat_mod, ONLY: drdat

USE clim_calc_mod, ONLY: clim_calc

USE logging_mod, ONLY: log_fatal

USE jules_print_mgr, ONLY:                                                     &
  jules_message,                                                               &
  jules_print

!TYPE definitions
USE prognostics, ONLY: progs_type
USE jules_fields_mod, ONLY: trifctltype
USE imgn_drive_mod, ONLY: imgn_drive_type
USE imgn_vars_mod, ONLY: imgn_vars_type
USE ancil_info, ONLY: ainfo_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Updates the climatology and meteorology based on the global temperature
!   anomaly. The Global temperature anomaly is calculated within as the a
!   function of the atmospheric composition, i.e. the CO2 concentration plus the
!   non-CO2 components.
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

! Arguments

!TYPES containing the data
TYPE(progs_type), INTENT(IN OUT) :: progs
TYPE(imgn_drive_type), INTENT(IN OUT) :: imgn_drive
TYPE(imgn_vars_type), INTENT(IN OUT) :: imgn_vars
TYPE(ainfo_type), INTENT(IN) :: ainfo

INTEGER :: l, n, nn, m, i, j ! Loop counter

INTEGER ::                                                                     &
  tally_co2_file,                                                              &
                 !WORK If used, checks CO2 value available in
                 !     file "FILE_SCEN_CO2_PPMV"
  yr_co2_file,                                                                 &
                 !WORK If used, reads in years available in
                 !     file "FILE_SCEN_CO2_PPMV"
  kode           ! Used to hold IOSTAT while reading file

REAL ::                                                                        &
  co2_file_ppmv  ! If used, is prescribed CO2 value for read
                 ! in years in "FILE_SCEN_CO2_PPMV"

REAL ::                                                                        &
  q_co2,                                                                       &
               ! Radiative forcing due to CO2 (W/m2)
  dtemp_l ,                                                                    &
  q_ch4,                                                                       &
               ! Radiative forcing due to changes in CH4 concentration
               !     from reference projection (W/m2)
  q_total      ! Total radiative forcing

!-----------------------------------------------------------------
! Variables to hold calculated anomalies
!-----------------------------------------------------------------
REAL ::                                                                        &
  tl1_anom(land_pts,mm),                                                       &
                ! Temperature anomalies (K)
  precip_anom(land_pts,mm),                                                    &
                ! Precip anomalies (mm/day)
  ql1_anom(land_pts,mm),                                                       &
                ! Specific humidity anomalies
  wind_anom(land_pts,mm),                                                      &
                ! wind anomalies (m/s)
  diurnal_tl1_anom(land_pts,mm),                                               &
                ! Diurnal Temperature (K)
  pstar_ha_anom(land_pts,mm),                                                  &
                ! Pressure anomalies (hPa)
  swdown_anom(land_pts,mm),                                                    &
                ! Shortwave radiation anomalie
  lwdown_anom(land_pts,mm)
                ! Longwave radiation anomalies

!------------------------------------------------------------------------
! Initialisation
q_co2     = 0.0
q_ch4     = 0.0
q_total   = 0.0

tl1_anom(:,:)         = 0.0
precip_anom(:,:)      = 0.0
ql1_anom(:,:)         = 0.0
wind_anom(:,:)        = 0.0
diurnal_tl1_anom(:,:) = 0.0
pstar_ha_anom(:,:)    = 0.0
swdown_anom(:,:)      = 0.0
lwdown_anom(:,:)      = 0.0

WRITE(jules_message,*) 'Updating IMOGEN climate'
CALL jules_print('imogen_update_clim',jules_message)


! Capture CO2 concentration at beginning of year
IF (include_co2) THEN
  imgn_vars%co2_start_ppmv(1) = imgn_vars%co2_ppmv(1)
END IF


! Hydrology 20th Century simulations (note also check for this run in
! subroutine imogen_confirmed_run which includes more stringent checks)
! OR analogue model simulations with CO2 prescribed.
IF ( ( ( .NOT. c_emissions) .AND. include_co2 ) .OR.                           &
             (l_change_metdata .AND. change_metdata_method == 3) ) THEN

  ! This works by reading in a file of CO2 concentrations, and checks that
  ! year is represented
  kode = 0
  OPEN(imogen_unit, FILE=file_scen_co2_ppmv,                                   &
            STATUS='old', POSITION='rewind', ACTION='read', IOSTAT=kode)
  IF (kode /= 0) THEN
    CALL log_fatal("imogen_update_clim", ': file_scen_co2_ppmv not found')
  END IF

  tally_co2_file = 0

  DO WHILE ( .TRUE.)
    READ(imogen_unit,FMT=*,IOSTAT = kode) yr_co2_file,co2_file_ppmv
    ! Check for end of file (or just any error) and exit loop if found
    ! iostat > 0 means illegal data
    ! iostat < 0 means end of record or end of file
    IF (kode /= 0) THEN
      CALL log_fatal("imogen_update_clim", ': error reading file_scen_co2_ppmv')
    END IF

    IF (yr_co2_file == current_time%year) THEN
      imgn_vars%co2_ppmv(1) = co2_file_ppmv
      tally_co2_file = tally_co2_file + 1
      ! We have found the correct year so exit loop
      EXIT
    END IF
  END DO

  CLOSE(imogen_unit)

  ! Check that value has been found.
  IF (tally_co2_file /= 1)                                                     &
    CALL log_fatal("imogen_update_clim", 'CO2 value not found in file')
END IF

! Now calculate the added monthly anomalies, either from analogue model
! or prescribed directly.
! These anomalies are set to zero when initialised (case l_change_metdata=false)
IF (l_change_metdata) THEN
  IF (change_metdata_method == 1) THEN  ! anlg_model
    ! create anomalies here
    ! This call is to the GCM analogue model. It is prescribed CO2
    ! concentration, and calculates non-CO2, and returns total change in
    ! radiative forcing, Q. Recall that the anlg model has a "memory" through
    ! dtemp_o - ie the ocean temperatures. Note that in this version of the
    ! code, the anlg model is updated yearly.

    ! Calculate the CO2 forcing
    IF (include_co2) THEN
      CALL radf_co2(imgn_vars%co2_ppmv(1), co2_init_ppmv, q2co2, q_co2)
    END IF

    ! Calculate the non CO2 forcing
    IF (include_non_co2_radf) THEN
      CALL radf_non_co2(                                                       &
        current_time%year,imgn_vars%q_non_co2(1),nyr_non_co2,file_non_co2_radf)
    END IF

    IF (land_feed_ch4) THEN
      CALL radf_ch4(imgn_vars%ch4_ppbv(1), q_ch4)
    END IF
    ! Calculate the total forcing
    q_total = q_co2 + imgn_vars%q_non_co2(1) + q_ch4

    ! calculate the global and land temperature change using analogue model
    CALL delta_temp(n_olevs, f_ocean, kappa_o, lambda_l, lambda_o, mu, q_total,&
        dtemp_l, imgn_vars%dtemp_o, imgn_vars%dtemp_g(1))
    imgn_vars%dtemp_g(1) = dtemp_l  !ejb sort this
  END IF  ! end anlg_model

  IF (change_metdata_method == 1 .OR. change_metdata_method == 3 ) THEN
    ! calculate the new met data based on global temperature change
    CALL pattern_scaling(land_pts, mm, imgn_vars%dtemp_g(1), imgn_drive, ainfo)
  END IF

  IF (change_metdata_method == 2) THEN
    ! Option where anomalies are prescribed directly not using the analogue model
    CALL drdat(current_time%year)
  END IF

ELSE  ! l_change_metdata=False
  ! Set anomalies to zero.
  imgn_drive%tl1_ij_anom(:,:,:)         = 0.0
  imgn_drive%swdown_ij_anom(:,:,:)      = 0.0
  imgn_drive%lwdown_ij_anom(:,:,:)      = 0.0
  imgn_drive%pstar_ij_anom(:,:,:)       = 0.0
  imgn_drive%ql1_ij_anom(:,:,:)         = 0.0
  imgn_drive%precip_ij_anom(:,:,:)      = 0.0
  imgn_drive%wind_ij_anom(:,:,:)        = 0.0
  imgn_drive%diurnal_tl1_ij_anom(:,:,:) = 0.0
END IF         ! End of where anomalies are calculated

! Now calculate the subdaily values of the driving data.
CALL clim_calc(land_pts, mm, md, nsdmax, progs%seed_rain, imgn_drive, ainfo)

! Compute current land carbon as dctot_co2 for use in imogen_update_carb.
IF (land_feed_co2) THEN
  DO l = 1,land_pts
    imgn_vars%dctot_co2(l) = trifctltype%cv_gb(l)
  END DO

  DO l = 1,land_pts
    DO m = 1,nsoilt
      DO n = 1,dim_cs1
        DO nn = 1,dim_cslayer
          imgn_vars%dctot_co2(l) = imgn_vars%dctot_co2(l) +                    &
                                              progs%cs_pool_soilt(l,m,nn,n)
        END DO
      END DO
    END DO
  END DO
END IF

! Unit conversion ppm_to_mmr (g/g): mol mass co2 / mol mass dry air * 1e-6
!ejb deleted co2_mmr = imgn_vars%co2_ppmv(1) * ppm_to_mmr
co2_mmr = imgn_vars%co2_ppmv(1) * 44.0 / 28.97 * 1.0e-6
imgn_vars%imogen_radf(:) = q_total

RETURN

END SUBROUTINE imogen_update_clim
END MODULE imogen_update_clim_mod
#endif
