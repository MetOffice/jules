! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    SUBROUTINE HEAT_CON------------------------------------------------------

! Description:
!    Calculates the soil thermal conductivity including the
!    effects of water and ice.

! Method == 1
!      Described in Cox et al (1999), Appendix B.
!      http://www.springerlink.com/content/9b459pyfhyjwk1ln/

! Method == 2
!      Simplified Johansen (1975).
!      See http://www-nwp/~frid/thermal_conductivity.pdf
!                                             (Dharssi, Jan 2008)

! Documentation : UM Documentation Paper 25

! Method == 3
!      Chadburn et al. (2015)
!      Modified so correctly parameterised for organic soils
!      Geoscientific Model Development


MODULE heat_con_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='HEAT_CON_MOD'

CONTAINS

SUBROUTINE heat_con(npnts, hcon, sthu, sthf, v_sat, hcons )

USE jules_snow_mod, ONLY:                                                      &
  snow_hcon
USE jules_soil_mod, ONLY:                                                      &
  soilhc_method, hcair, hcice, hcwat

USE parkind1,       ONLY: jprb, jpim
USE yomhook,        ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  npnts
    ! Number of gridpoints

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  hcon(npnts),                                                                 &
    ! Dry soil thermal conductivity (W/m/K).
  sthu(npnts),                                                                 &
    ! Fractional saturation of unfrozen water at layer boundaries.
  sthf(npnts),                                                                 &
    ! Fractional saturation of frozen water at layer boundaries.
  v_sat(npnts)
    ! Volumetric soil moisture concentration at saturation (m3/m3 soil).

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  hcons(npnts)
    ! The thermal conductivity between adjacen layers including effects of
    ! water and ic (W/m/K).

!-----------------------------------------------------------------------------
! Local parameters:
!-----------------------------------------------------------------------------
! Following local parameter values determined by linear regression,
! see Dharssi(2008).
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  hcsat_max_allowed = 2.20,                                                    &
  hcsat_min_allowed = 1.58,                                                    &
  hcsat_gradient    = 12.4,                                                    &
  hcsat_intercept   = 0.25

!-----------------------------------------------------------------------------
! Local variables:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i
    ! Loop counter.

REAL(KIND=real_jlslsm) ::                                                      &
  hcsat(npnts),                                                                &
    ! The thermal conductivity of the saturated  soil at current ratio of ice
    ! to liquid water (W/m/K).
  sth(npnts),                                                                  &
    ! Fractional saturation of water (liquid+ice) at layer boundaries.
  thice(npnts),                                                                &
    ! The concentration of ice at saturation for the current mass fraction
    ! of liquidcwater (m3 H2O/m3 soil).
  thwat(npnts),                                                                &
    ! The concentration of liquid water at saturation for the current mass
    ! fraction of liquid (m3 H2O/m3 soil).
  ke(npnts)
    ! Kersten number

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='HEAT_CON'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i)                                        &
!$OMP SHARED(npnts, v_sat, hcons, hcon, soilhc_method, sthu, thwat, sthf,      &
!$OMP        thice, sth, hcsat, ke, snow_hcon)

!----------------------------------------------------------------------
! Initialise all points
!----------------------------------------------------------------------

!$OMP DO SCHEDULE(STATIC)
DO i = 1,npnts
  IF (v_sat(i) >  0.0) THEN ! Soil points
    hcons(i) = hcon(i)
  ELSE ! Ice points
    hcons(i) = snow_hcon
  END IF
END DO
!$OMP END DO

IF (soilhc_method == 1) THEN
!$OMP DO SCHEDULE(STATIC)
  DO i = 1,npnts
    !---------------------------------------------------------------
    ! Only do calculation for non land-ice pts
    ! V_SAT is set to zero for land-ice points
    !---------------------------------------------------------------
    IF (v_sat(i) >  0.0) THEN

      IF (sthu(i) >  0.0) THEN
        thwat(i) = v_sat(i) * sthu(i) / (sthu(i) + sthf(i))
      ELSE
        thwat(i) = 0.0
      END IF

      IF (sthf(i) >  0.0) THEN
        thice(i) = v_sat(i) * sthf(i) / (sthu(i) + sthf(i))
      ELSE
        thice(i) = 0.0
      END IF

      sth(i) = sthu(i) + sthf(i)
      hcsat(i) = hcon(i) * (hcwat**thwat(i)) * (hcice**thice(i))               &
                 / (hcair**v_sat(i))
      hcons(i) = (hcsat(i) - hcon(i)) * sth(i) + hcon(i)
    END IF

  END DO
!$OMP END DO
END IF ! (SOILHC_METHOD==1)

IF (soilhc_method == 2) THEN
!$OMP DO SCHEDULE(STATIC)
  DO i = 1,npnts
    IF (v_sat(i) >  0.0) THEN

      IF (sthf(i) >  0.0) THEN
        thice(i) = v_sat(i) * sthf(i) / (sthu(i) + sthf(i))
      ELSE
        thice(i) = 0.0
      END IF

      thwat(i) = v_sat(i) - thice(i)

      sth(i) = sthu(i) + sthf(i)

      hcsat(i) = hcsat_min_allowed                                             &
                 + hcsat_gradient * (hcon(i) - hcsat_intercept)

      IF (hcsat(i) > hcsat_max_allowed) hcsat(i) = hcsat_max_allowed
      IF (hcsat(i) < hcsat_min_allowed) hcsat(i) = hcsat_min_allowed

      ! Adjust HCSAT for frozen soil water
      hcsat(i) = hcsat(i) * (hcwat**thwat(i)) * (hcice**thice(i))              &
                / (hcwat**v_sat(i))

      IF (sth(i) <= 0.1) THEN
        ke(i) = 0.0
      ELSE
        ke(i) = 1.0 + LOG10(sth(i))
      END IF

      hcons(i) = (hcsat(i) - hcon(i)) * ke(i) + hcon(i)
    END IF

  END DO
!$OMP END DO
END IF ! (SOILHC_METHOD==2)

IF (soilhc_method == 3) THEN
!$OMP DO SCHEDULE(STATIC)
  DO i = 1,npnts

    IF (v_sat(i) >  0.0) THEN

      IF (sthf(i) >  0.0) THEN
        thice(i) = v_sat(i) * sthf(i) / (sthu(i) + sthf(i))
      ELSE
        thice(i) = 0.0
      END IF

      thwat(i) = v_sat(i) - thice(i)

      sth(i) = sthu(i) + sthf(i)

      hcsat(i) = ( 1.0 - 0.0134 * LOG(hcon(i)) ) / ( -0.745 - LOG(hcon(i)) )

      IF (hcsat(i) > hcsat_max_allowed) hcsat(i) = hcsat_max_allowed
      IF (hcsat(i) < 0.5)               hcsat(i) = 0.5

      ! Adjust HCSAT for frozen soil water
      hcsat(i) = hcsat(i) * (hcwat**thwat(i)) * (hcice**thice(i))              &
                 / (hcwat**v_sat(i))

      IF (sth(i) <= 0.1) THEN
        ke(i) = 0.0
      ELSE
        ke(i) = 1.0 + LOG10(sth(i))
      END IF

      hcons(i) = (hcsat(i) - hcon(i)) * ke(i) + hcon(i)
    END IF

  END DO
!$OMP END DO
END IF ! (SOILHC_METHOD==3)

!$OMP END PARALLEL

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE heat_con
END MODULE heat_con_mod
