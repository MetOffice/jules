#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************
MODULE topmodel_init_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE topmodel_init(psparms,toppdm,ainfo)

  !Use in relevant subroutines
USE calc_fsat_mod,           ONLY: calc_fsat
USE calc_zw_inund_mod,       ONLY: calc_zw_inund
USE calc_baseflow_jules_mod, ONLY: calc_baseflow_jules

!Use in relevant varaibles
USE ancil_info, ONLY:                                                          &
  land_pts, soil_pts, nsoilt

USE jules_soil_mod,       ONLY:                                                &
  dzsoil, sm_levels

USE jules_hydrology_mod,  ONLY:                                                &
  l_wetland_unfrozen

USE water_constants_mod,  ONLY:                                                &
  rho_water  !  density of pure water (kg/m3)

USE um_types, ONLY: real_jlslsm

!TYPE definitions
USE p_s_parms, ONLY: psparms_type
USE top_pdm, ONLY: top_pdm_type
USE ancil_info, ONLY: ainfo_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Finish initialising TOPMODEL by calculating surface saturated fraction
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Arguments

!TYPES containing the data
TYPE(psparms_type), INTENT(IN OUT) :: psparms
TYPE(top_pdm_type), INTENT(IN OUT) :: toppdm
TYPE(ainfo_type), INTENT(IN OUT) :: ainfo

! Work variables
LOGICAL, PARAMETER :: l_gamtot = .FALSE.  !  Switch for calculation of gamtot.
                                          !  .FALSE. so that calc_fsat
                                          !  calculates toppdm%fsat_soilt.

INTEGER :: i, j, n, m                    ! Index variables

REAL(KIND=real_jlslsm) :: qbase_l_soilt(land_pts,nsoilt,sm_levels+1)
  !Base flow from each layer (kg/m2/s).
REAL(KIND=real_jlslsm) :: top_crit_soilt(land_pts,nsoilt)
  !Critical topographic index required to calculate the surface saturation
  !fraction.
REAL(KIND=real_jlslsm) :: zdepth(0:sm_levels)
  !Lower soil layer boundary depth (m).
REAL(KIND=real_jlslsm) :: wutot_soilt(land_pts,nsoilt)
  !UNFROZEN to TOTAL fraction at ZW.
REAL(KIND=real_jlslsm) :: dumwutot_soilt(land_pts,nsoilt)
  !Dummy UNFROZEN to TOTAL fraction at ZW (always set to 1).
REAL(KIND=real_jlslsm) :: ksz_soilt(land_pts,nsoilt,0:sm_levels)
  !Saturated hydraulic conductivity for each layer (kg/m2/s).
REAL(KIND=real_jlslsm) :: dumsthf_soilt(land_pts,nsoilt,sm_levels)
  !Dummy Frozen soil moisture content of each layer as a fraction of
  !saturation (always set to 0).
REAL(KIND=real_jlslsm) :: smcl_soilt(land_pts,nsoilt,sm_levels)
  !Soil moisture content of each layer (kg/m2).
REAL(KIND=real_jlslsm) :: smclsat_soilt(land_pts,nsoilt,sm_levels)
  !Saturated soil moisture content of each layer (kg/m2).
REAL(KIND=real_jlslsm) :: zw_inund_soilt(land_pts,nsoilt)
  ! Adjusted Water table depth (m).

!-------------------------------------------------------------------------------
zdepth(0) = 0.0
DO n = 1,sm_levels
  zdepth(n) = zdepth(n-1) + dzsoil(n)
END DO

! Set values that are retained at non-soil points.
toppdm%fsat_soilt(:,:)     = 0.0
toppdm%fwetl_soilt(:,:)    = 0.0
dumwutot_soilt(:,:) = 1.0

IF ( soil_pts /= 0 ) THEN
  DO j = 1,soil_pts
    i = ainfo%soil_index(j)
    DO n = 0,sm_levels
      ksz_soilt(i,:,n) = psparms%satcon_soilt(i,:,n)
    END DO
    DO n = 1,sm_levels
      smclsat_soilt(i,:,n) = rho_water * dzsoil(n) * psparms%smvcst_soilt(i,:,n)
      smcl_soilt(i,:,n)    =                                                   &
        (psparms%sthu_soilt(i,:,n) + psparms%sthf_soilt(i,:,n))                &
        * smclsat_soilt(i,:,n)
    END DO
  END DO

  IF (L_wetland_unfrozen) THEN
    DO m = 1,nsoilt
      CALL calc_zw_inund(                                                      &
        land_pts,sm_levels,soil_pts,ainfo%soil_index,zdepth,                   &
        psparms%bexp_soilt(:,m,:),psparms%sathh_soilt(:,m,:),                  &
        smclsat_soilt(:,m,:),                                                  &
        smcl_soilt(:,m,:),psparms%sthu_soilt(:,m,:),toppdm%sthzw_soilt(:,m),   &
        toppdm%zw_soilt(:,m),                                                  &
        zw_inund_soilt(:,m),wutot_soilt(:,m))

    END DO
    dumsthf_soilt(:,:,:) = 0.0
  ELSE
    zw_inund_soilt(:,:)  = toppdm%zw_soilt(:,:)
    dumsthf_soilt(:,:,:) = psparms%sthf_soilt(:,:,:)
  END IF

  !   We need top_crit_soilt - get this from calc_baseflow.
  DO m = 1,nsoilt
    CALL calc_baseflow_jules(                                                  &
      land_pts, sm_levels, soil_pts, ainfo%soil_index,                         &
      psparms%bexp_soilt(:,m,:), toppdm%fexp_soilt(:,m), dumsthf_soilt(:,m,:), &
      toppdm%ti_mean_soilt(:,m), zdepth, zw_inund_soilt(:,m), ksz_soilt(:,m,:),&
      toppdm%qbase_soilt(:,m), qbase_l_soilt(:,m,:), top_crit_soilt(:,m) )

    !   Call calc_fsat with l_gamtot=.FALSE. so as to get toppdm%fsat_soilt.
    CALL calc_fsat(                                                            &
      land_pts, soil_pts, l_gamtot, ainfo%soil_index,                          &
      toppdm%ti_mean_soilt(:,m), toppdm%ti_sig_soilt(:,m),                     &
      top_crit_soilt(:,m), dumwutot_soilt(:,m),                                &
      toppdm%gamtot_soilt(:,m), toppdm%fsat_soilt(:,m),                        &
      toppdm%fwetl_soilt(:,m))
  END DO

  IF (L_wetland_unfrozen) THEN
    DO j = 1,soil_pts
      i = ainfo%soil_index(j)
      toppdm%fsat_soilt(i,:)  = wutot_soilt(i,:) * toppdm%fsat_soilt(i,:)
      toppdm%fwetl_soilt(i,:) = wutot_soilt(i,:) * toppdm%fwetl_soilt(i,:)
    END DO
  END IF

END IF  !  soil_pts

RETURN

END SUBROUTINE topmodel_init
END MODULE topmodel_init_mod
#endif
