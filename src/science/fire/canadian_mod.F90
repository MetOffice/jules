! ****************************COPYRIGHT*****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use
! and distribution under the JULES collaboration agreement, subject
! to the terms and conditions set out therein.
!
! [Met Office Ref SC0237]
! ****************************COPYRIGHT*****************************************

!Encapsulated calculation of the Canadian Fire Weather Index (FWI)

MODULE canadian

!No imports

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!
! Description:
! Contains subroutines for calculating the Canadian FWI.
!
! Access is via a call to canadian_calc. This calls each of 6 subroutines
! that calculates a heirarchy of quanities, eventually yielding the fire
! weather index. See references for block diagrams and detailed explanation.
!
! Code rewritten from previous MO F77 code to integrate with JULES.
! All inline equation numbers refer to the original documentation:
!
! Original description of FWI can be found here:
! Van Wagner, C.E. and Pickett, T.L., 1985,
!"Equations and FORTRAN Program for the Canadian Forest Fire Weather Index
! System",
! Canadian Forestry Service, Ottawa
!
! Alternatively, for those referring to the Met Office document entitled
! "Met Office JULES fire module Version 1.0 (JULES V4.1)
! December 2014"
! Additional cross references to equation numbers therein are given in
! {curly brackets}
!
! Common abbreviations:
! ffmc    = fine fuel moisture code
! dmc     = duff moisture code
! dc      = drought code
! bui     = build up index
! isi     = initial spread index
! fwi     = fire weather index
!
! Some of the above are prefixed mois_ to represent the moisture associated
! with the calculation of these quantities.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!

PRIVATE !Default everything to being private. Unlock as needed

! Module constants

  !Constants used in the various subroutines
REAL(KIND=real_jlslsm), DIMENSION(12), PARAMETER  :: day_length  =             &
  [ 6.5, 7.5, 9.0, 12.8, 13.9, 13.9, 12.4, 10.9, 9.4, 8.0, 7.0, 6.0 ]
REAL(KIND=real_jlslsm), DIMENSION(12), PARAMETER  :: day_len_adj =             &
  [ -1.6, -1.6, -1.6, 0.9, 3.8, 5.8, 6.4, 5.0, 2.4, 0.4, -1.6, -1.6 ]

REAL(KIND=real_jlslsm), PARAMETER                 :: canopy_rain_loss = 0.5,   &
! (mm) The amount of rain captured by the canopy
                                   pseudo_zero      = 0.0000001
! A number very close to zero to prevent nasty underflows. Defined here to be
! consistent throughout the module

! Public module variables- NONE

! Private module variables- NONE

!Set the subroutines available outside the module
PUBLIC  :: canadian_calc

CONTAINS

!#include "canadian_calc.inc"
! Part of the canadian module. Controls the FWI calculation.

SUBROUTINE canadian_calc(                                                      &
  !Scalar INTENT(IN)                                                        &
  iMonth, hemi_opt,                                                            &
  !Array INTENT(IN)                                                         &
  hemisphere_NtSf, temperature, rel_hum, wind, rain,                           &
  !Array INTENT(INOUT)                                                      &
  ffmc, mois_ffmc, dmc, dc,                                                    &
  !Array INTENT(OUT)                                                        &
  isi, bui, fwi)

!No Imports, but has access to module variables.

IMPLICIT NONE

!
! Description:
!   Controls the calculation of the FWI.
!
! Method:
!   See duaghter subroutines for details of the calculations
!   Note that the results are not stored inside the module.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!

! ! Subroutine arguments
! < Scalar arguments with INTENT(IN) >
INTEGER,  INTENT(IN)    :: iMonth

!Option to change month-dep. param.s to allow for N/S hemisphere summer/winter
LOGICAL,  INTENT(IN)    :: hemi_opt

! < Array arguments with INTENT(IN) >
REAL(KIND=real_jlslsm),     INTENT(IN)    :: temperature(:), & ! (C)
                           wind(:),        & ! (km/h)
                           rel_hum(:),     & ! (%)
                           rain(:)           ! (mm)

!Mask for points in each hemisphere. N=True, S=False
LOGICAL,  INTENT(IN)    :: hemisphere_NtSf(:)

! < Array arguments with INTENT(INOUT)>
REAL(KIND=real_jlslsm),     INTENT(IN OUT) :: ffmc(:),                         &
                                             !fine fuel moisture Code
                           mois_ffmc(:),   & !moisture part of ffmc
                           dmc(:),         & !Duff Moisture Code
                           dc(:)             !Drought Code

! < Array arguments with INTENT(OUT)>
REAL(KIND=real_jlslsm),     INTENT(OUT)   :: isi(:),                           &
                                             !Initial spread Index
                           bui(:),         & !Buildup Index
                           fwi(:)            !Fire Weather Index

! End of header

CALL canadian_calc_ffmc(                                                       &
  !Array INTENT(IN)                                                         &
  rel_hum(:), temperature(:), wind(:), rain(:),                                &
  !Array INTENT(INOUT)                                                      &
  mois_ffmc(:),                                                                &
  !Array INTENT(OUT)                                                        &
  ffmc(:))

CALL canadian_calc_dmc(                                                        &
  !Scalar INTENT(IN)                                                        &
  iMonth, hemi_opt,                                                            &
  !Array INTENT(IN)                                                         &
  hemisphere_NtSf(:), rel_hum(:), temperature(:), rain(:),                     &
  !ARRAY INTENT(INOUT)                                                      &
  dmc(:))

CALL canadian_calc_dc(                                                         &
  !Scalar INTENT(IN)                                                        &
  iMonth, hemi_opt,                                                            &
  !Array INTENT(IN)                                                         &
  hemisphere_NtSf(:), temperature(:), rain(:),                                 &
  !Array INTENT(INOUT)                                                      &
  dc(:))

CALL canadian_calc_isi(                                                        &
  !Array INTENT(IN)                                                         &
  wind(:), mois_ffmc(:),                                                       &
  !Array INTENT(OUT)                                                        &
  isi(:))

CALL canadian_calc_bui(                                                        &
  !Array INTENT(IN)                                                         &
  dmc(:), dc(:),                                                               &
  !Array INTENT(OUT)                                                        &
  bui(:))

CALL canadian_calc_fwi(                                                        &
  !Array INTENT(IN)                                                         &
  isi(:), bui(:),                                                              &
  !Array INTENT(OUT)                                                        &
  fwi(:))

! Equation 41
!dsr = 0.0272 * (fwi ** 1.77)

RETURN
END SUBROUTINE canadian_calc


!#include "canadian_calc_ffmc.inc"
! Part of the canadian module. Performs the fine fuel moisture code calculation

SUBROUTINE canadian_calc_ffmc(                                                 &
!Array INTENT(IN)                                                           &
rel_hum, temperature, wind, rain,                                              &
!Array INTENT(INOUT)                                                        &
mois_ffmc,                                                                     &
!Array INTENT(OUT)                                                          &
ffmc)

!No Imports, but has access to own module variables.

IMPLICIT NONE

!
! Description:
!   Calculated the fine fuel moisture code part of the FWI.
!   Only called from within the module
!
! Method:
!   See documentation for further details.
!   Equation numbers refer to the reference given at the top of the module
!   Equation numbers in {curly brackets} refer to the Met Office documentation
!   referred to at the top of the module.
!
!   All variables are arrays of the same length
!   Local arrays have been initialised to SIZE of an argument with a short name
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!

! ! Subroutine arguments
! < Array arguments with INTENT(IN) >
REAL(KIND=real_jlslsm), INTENT(IN)      :: rel_hum(:),                         &
                         temperature(:),                                       &
                         wind(:),                                              &
                         rain(:)

! < Array arguments with INTENT(INOUT)>
REAL(KIND=real_jlslsm), INTENT(IN OUT)   :: mois_ffmc(:)


! < Array arguments with INTENT(OUT)>
REAL(KIND=real_jlslsm), INTENT(OUT)     :: ffmc(:)

! < Array local variables >
REAL(KIND=real_jlslsm)  :: common_term(SIZE(rain)),                            &
         Ed(SIZE(rain)),          & !Estimated Drying factor
         Ew(SIZE(rain)),          & !Estimated Wetting factor
         ko(SIZE(rain)),          & !wetting/drying rate
         k(SIZE(rain)),           & !ko after temperature adjustment
         prev_mois(SIZE(rain)),   & !Calculated fine fuel moisture
         eff_rain(SIZE(rain))       !effective rainfall

! End of header

  !All variables used below here are 1-D arrays. (:) notation is omitted.

  !===================Grab a copy of the previous moisture value
prev_mois = mois_ffmc

!===================Perform rainfall phase calculations
!{Documentation eq1}

!Some of the rainfall is captured by the canopy, meaning a threshold amount
!is needed to perform the calculations
WHERE ( rain >  canopy_rain_loss .AND. prev_mois <= 150.0)

  ! Equation 14
  eff_rain = rain - canopy_rain_loss

  ! Equation 12
  mois_ffmc = prev_mois                                                        &
    + (eff_rain * 42.5 * EXP(-100.0 / (251.0 - prev_mois))                     &
    * (1.0 - EXP(-6.93 / eff_rain)))

END WHERE

! Apply a correction if the previous moisture is already quite high
WHERE ( rain >  canopy_rain_loss .AND. prev_mois >  150.0 )

  ! Equation 14
  eff_rain = rain - canopy_rain_loss

  ! Equation 12
  mois_ffmc = prev_mois                                                        &
    + (eff_rain * 42.5 * EXP(-100.0 / (251.0 - prev_mois))                     &
    * (1.0 - EXP(-6.93 / eff_rain)))

  ! Equation 13
  mois_ffmc = mois_ffmc +                                                      &
    (0.0015 * ((prev_mois-150.0)**2.0) * SQRT(eff_rain))

END WHERE

!===================Perform drying phase calculations
!{Documentation eq2 and eq 3}

!Calculate the equilibrium moisture content (E) for the drying and wetting
!isotherms
common_term = (11.0 * EXP((rel_hum-100.0) / 10.0)) +                           &
  (0.18 * (21.1 - temperature) * (1.0 - EXP(-0.115 * rel_hum)))
! Equation 8a
Ed = (0.942 * (rel_hum**0.679)) + common_term
! Equation 8b
Ew = (0.618 * (rel_hum**0.753)) + common_term

!Determine if we're in a wetting or drying phase, and therefore use
!equation 9 or 10. !{Documentation eq4-6}

!This assumess that Ed > Ew.
WHERE ( mois_ffmc > Ed ) !Drying phase

  ! Equation 4
  ko = (0.424 * (1.0 - (rel_hum / 100.0)** 1.7) +                              &
    (0.0694 * wind**0.5) * (1.0 - ((rel_hum / 100.0)**8.0)))

  ! Equation 6
  k = ko * 0.581 * EXP(0.0365 * temperature)

  ! Equation 9
  mois_ffmc = Ed + ((mois_ffmc - Ed) * (10.0**(-k)))
END WHERE

WHERE ( mois_ffmc < Ew ) !Wetting phase

  ! Equation 5
  ko = (0.424 * (1.0 - ((100.0 - rel_hum) / 100.0)**1.7) +                     &
    (0.0694 * wind**0.5) * (1.0 - (((100.0 - rel_hum) / 100.0)**8.0)))

  ! Equation 6
  k = ko * 0.581 * EXP(0.0365 * temperature)

  ! Equation 10
  mois_ffmc = Ew - ((Ew - mois_ffmc) *  (10.0**(-k)))

END WHERE

!ELSE In between and nothing happens

! Ensure that the moisture doesn't exceed the maximum allowed value
mois_ffmc = MIN( mois_ffmc, 250.0 )

!===================Finally calculate the ffmc
!{Documentation eq7}

! Equation 2a
ffmc = (59.5 * (250.0 - mois_ffmc)) / (147.2 + mois_ffmc)

RETURN
END SUBROUTINE canadian_calc_ffmc



!#include "canadian_calc_dmc.inc"
! Part of the canadian module. Performs the duff moisture code calculation

SUBROUTINE canadian_calc_dmc(                                                  &
  !Scalar INTENT(IN)                                                        &
  iMonth, hemi_opt,                                                            &
  !Array INTENT(IN)                                                         &
  hemisphere_NtSf, rel_hum, temperature, rain,                                 &
  !ARRAY INTENT(INOUT)                                                      &
  dmc)

!No Imports, but has access to module variables.

IMPLICIT NONE

!
! Description:
!   Calculated the duff moisture code part of the FWI.
!   Only called from within the module
!
! Method:
!   See documentation for further details.
!   Equation numbers refer to the reference given at the top of the module
!   Equation numbers in {curly brackets} refer to the Met Office documentation
!   referred to at the top of the module.
!
!   All variables are arrays of the same length
!   Local arrays have been initialised to SIZE of an argument with a short name
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!

! ! Subroutine arguments
! < Scalar arguments with INTENT(IN) >
INTEGER,  INTENT(IN)    :: iMonth
!Option to change month-dependent parameters to allow for N/S hemisphere
!summer/winter
LOGICAL,  INTENT(IN)    :: hemi_opt

! < Array arguments with INTENT(IN) >
REAL(KIND=real_jlslsm),     INTENT(IN)    :: rel_hum(:),                       &
                           temperature(:),                                     &
                           rain(:)

!Mask for points in each hemisphere. N=True, S=False
LOGICAL,  INTENT(IN)    :: hemisphere_NtSf(:)

! < Array arguments with INTENT(INOUT)>
REAL(KIND=real_jlslsm),     INTENT(IN OUT) :: dmc(:)

! < Scalar local variables >
INTEGER :: alt_iMonth

! < Array local variables >
REAL(KIND=real_jlslsm)    :: k(SIZE(rain)),                                    &
                                        !log drying rate,log10M/day
           b(SIZE(rain)),             & !slope variable rain effect
           re(SIZE(rain)),            & !effective rainfall
           moisture_dmc(SIZE(rain)),  & !moisture part of the dmc
           hemidep_day_len(SIZE(rain))  !Hemisphere-dep. day len. adjustment

! ! End of header

  !All variables used below here except day_len(nmonth) are 1-D arrays.
  !(:) notation is omitted.

  !===================Calculate the rainfall phase
  !{Documentation eq8}

  !Calculate slope variable in dmc rain effect
WHERE (rain > 1.5 .AND. dmc <= 33.0)
  ! Equation 19a
  b = 100.0 / (0.5 + (0.3 * dmc))
END WHERE
WHERE (rain > 1.5 .AND. dmc > 33.0 .AND. dmc <= 65.0)
  ! Equation 19b
  b = 14.0 - 1.3 * LOG(dmc)
END WHERE
WHERE (rain > 1.5 .AND. dmc > 65.0)
  ! Equation 19c
  b = 6.2 * LOG(dmc) - 17.2
END WHERE

WHERE (rain > 1.5)

  ! Calculate the moisture to take account of the the drying phase in
  ! the last timestep. This saves 2 prognostic variables.
  moisture_dmc = 20.0 + EXP((244.72 - dmc) / 43.43)

  ! Equation 17        Calculate effective rainfall
  !{Documentation eq9}
  re = (0.92 * rain) - 1.27

  ! Equation 18   Calculate duff moisture content after rain
  !{Documentation eq10}
  moisture_dmc = MIN(MAX(moisture_dmc + (1000.0 * re) / (48.77 + b * re),      &
                         20.0 + pseudo_zero)                                   &
                     ,300.0)

  ! Equation 16       Calculate dmc after rain
  !{Documentation eq11}
  dmc = MAX(244.72 - (43.43 * LOG(moisture_dmc-20.0)), pseudo_zero)
  !ELSE WHERE
  !dmc stays the same
END WHERE

!===================Calculate the drying phase
!{Documentation eq12}

!Sort out the hemispheric-dependent value of day_len_adj if needed
IF ( hemi_opt ) THEN

  !Get iMonth moved by 6 months
  IF (iMonth > 6) THEN
    alt_iMonth = iMonth - 6
  ELSE
    alt_iMonth = iMonth + 6
  END IF

  !For all S hemisphere points, point at the alternative month
  WHERE ( hemisphere_NtSf )
    hemidep_day_len = day_length(iMonth)
  ELSE WHERE
    hemidep_day_len = day_length(alt_iMonth)
  END WHERE
ELSE
  hemidep_day_len   = day_length(iMonth)
END IF

WHERE (temperature <= -1.1)
  k = 0.0
ELSE WHERE
  !Equation 20
  k = 1.894 * (temperature+1.1) * (100.0 - rel_hum)                            &
      * hemidep_day_len * 0.000001
END WHERE

!===================Finally calculate the dmc
!{Documentation eq13
! Equation 21
dmc = dmc + (100.0 * k)

RETURN
END SUBROUTINE canadian_calc_dmc



!#include "canadian_calc_dc.inc"
! Part of the canadian module. Performs the drought code calculation

SUBROUTINE canadian_calc_dc(                                                   &
!Scalar INTENT(IN)                                                          &
iMonth, hemi_opt,                                                              &
!Array INTENT(IN)                                                           &
hemisphere_NtSf, temp, rain,                                                   &
!Array INTENT(INOUT)                                                        &
dc)

!No Imports, but has access to module variables.

IMPLICIT NONE

!
! Description:
!   Calculated the drought code part of the FWI.
!   Only called from within the module
!
! Method:
!   See documentation for further details.
!   Equation numbers refer to the reference given at the top of the module
!   Equation numbers in {curly brackets} refer to the Met Office documentation
!   referred to at the top of the module.
!
!   All variables are arrays of the same length
!   Local arrays have been initialised to SIZE of an argument with a short name
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!

! ! Subroutine arguments
! < Scalar arguments with INTENT(IN) >
INTEGER,  INTENT(IN)    :: iMonth
!Option to change month-dep. params to allow for N/S hemisphere summer/winter
LOGICAL,  INTENT(IN)    :: hemi_opt

! < Array arguments with INTENT(IN) >
REAL(KIND=real_jlslsm),     INTENT(IN)    :: temp(:),                          &
                           rain(:)

!Mask for points in each hemisphere. N=True, S=False
LOGICAL,  INTENT(IN)    :: hemisphere_NtSf(:)

! < Array arguments with INTENT(INOUT)>
REAL(KIND=real_jlslsm),     INTENT(IN OUT) :: dc(:)

! < Scalar local variables >
INTEGER :: alt_iMonth

! < Array local variables >
REAL(KIND=real_jlslsm)    :: v(SIZE(rain)),                                    &
                                             !pot evapotranspiration,
                                             !(0.254 mm water/day)
           rd(SIZE(rain)),                 & !effective rainfall
           moisture_dc(SIZE(rain)),                                            &
           hemidep_day_len_adj(SIZE(rain))   !Hemisphere-dependent day
                                             !length adjustment

! End of header

  !All variables used below here except day_len_adj(nmonth) are 1-D arrays.
  !(:) notation is omitted.

  !Calculate the moisture to take account of the drying phase in the last
  !iteration (doing this saves 2 prognostic variables)
  !{Documentation eq20}
moisture_dc = 800.0 * EXP(-dc / 400.0)

!===================Rainfall phase
!{Documentation eq15-17}

!Only calculate rain if above the threshold
WHERE ( rain > 2.8 )
  ! Equation 23   Calculate effective rainfall
  rd = 0.83 * rain - 1.27
  ! Equation 24   Calculate moisture equivalent after rain
  moisture_dc = moisture_dc + 3.937 * rd
  ! Equation 22   Calculate drought_code after rain
  dc = MAX(400.0 * LOG(800.0 / moisture_dc), 0.0)
  !  ELSE
  !   dc reamins dc
END WHERE

!===================Drying phase
!Sort out the hemispheric-dependent value of day_len_adj if needed
IF ( hemi_opt ) THEN

  !Get iMonth moved by 6 months
  IF (iMonth > 6) THEN
    alt_iMonth = iMonth - 6
  ELSE
    alt_iMonth = iMonth + 6
  END IF

  !For all S hemisphere points, point at the alternative month
  WHERE ( hemisphere_NtSf )
    hemidep_day_len_adj = day_len_adj(iMonth)
  ELSE WHERE
    hemidep_day_len_adj = day_len_adj(alt_iMonth)
  END WHERE
ELSE
  hemidep_day_len_adj = day_len_adj(iMonth)
END IF

!Calculate potential evapotranspiration
!{Documentation eq18}
! Equation 25
!   V = MAX(0.36 * (MAX(Temp,-2.8) + 2.8) + day_len_adj(nmonth), pseudo_zero)
v  = MAX(0.36 * (temp+2.8) + hemidep_day_len_adj, 0.0)
! Equation 26 Calculate drought_code {Documentation eq19}
dc = MAX(dc + (0.5 * v), pseudo_zero)

RETURN
END SUBROUTINE canadian_calc_dc


!#include "canadian_calc_isi.inc"
! Part of the canadian module. Performs the initial spread index calculation

SUBROUTINE canadian_calc_isi(                                                  &
  !Array INTENT(IN)                                                         &
  wind, mois_ffmc,                                                             &
  !Array INTENT(OUT)                                                        &
  isi)

!No Imports, but has access to module variables.

IMPLICIT NONE

!
! Description:
!   Calculates the initial spread index part of the FWI.
!   Only called from within the module
!
!   All variables are arrays of the same length
!   Local arrays have been initialised to SIZE of an argument with a short name
!
! Method:
!   See documentation for further details.
!   Equation numbers refer to the reference given at the top of the module
!   Equation numbers in {curly brackets} refer to the Met Office documentation
!   referred to at the top of the module.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!

! ! Subroutine arguments
! < Array arguments with INTENT(IN) >
REAL(KIND=real_jlslsm),     INTENT(IN)  :: wind(:),                            &
                         mois_ffmc(:)

! < Array arguments with INTENT(OUT)>
REAL(KIND=real_jlslsm),     INTENT(OUT) :: isi(:)

! < Array local variables >
REAL(KIND=real_jlslsm)                  :: wf(SIZE(wind)),                     &
                         ffmf(SIZE(wind))

! End of header

  !All variables used below here are 1-D arrays. (:) notation is omitted.
  !{Documentation eq21-23}

  !Equation 33 for wind effect
wf = EXP(0.05039 * wind)

! Equation 34
ffmf = 91.9 * EXP(-0.1386 * mois_ffmc) *                                       &
  (1.0 + (mois_ffmc ** 5.31) / (4.93 * 10.0 ** 7.0))

! Equation 35
isi = 0.208 * wf * ffmf

RETURN
END SUBROUTINE canadian_calc_isi


!#include "canadian_calc_bui.inc"
! Part of the canadian module. Performs the BUI calculation

SUBROUTINE canadian_calc_bui(                                                  &
!Array INTENT(IN)                                                           &
dmc, dc,                                                                       &
!Array INTENT(OUT)                                                          &
bui)

!No Imports, but has access to module variables.

IMPLICIT NONE

!
! Description:
!   Calculated the Build Up Index part of the FWI.
!   Only called from within the module
!
! Method:
!   See documentation for further details.
!   Equation number refer to the reference given at the top of the module
!   Equation numbers in {curly brackets} refer to the Met Office documentation
!   referred to at the top of the module.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!
! < Array arguments with INTENT(IN) >
REAL(KIND=real_jlslsm),   INTENT(IN), DIMENSION(:)          :: dmc
REAL(KIND=real_jlslsm),   INTENT(IN), DIMENSION(:)          :: dc

! < Array arguments with INTENT(OUT)>
REAL(KIND=real_jlslsm),   INTENT(OUT), DIMENSION(:)         :: bui

! End of header

  !All variables are 1-D arrays. (:) notation is omitted

  !{Documentation eq24}

WHERE (dc <= pseudo_zero)
  bui = 0.0
END WHERE

WHERE (dmc <= (0.4 * dc)  .AND. dc > pseudo_zero)
  ! Equation 36
  bui = (0.8 * dmc * dc) / (dmc+0.4 * dc)
END WHERE

WHERE (dmc > (0.4 * dc) .AND. dc > pseudo_zero)
  bui = dmc -                                                                  &
            (1.0 - ((0.8 * dc) / (dmc + (0.4 * dc))))                          &
          * (0.92 + ((0.0114 * dmc)**1.7))
END WHERE

bui = MAX(bui, pseudo_zero)

RETURN
END SUBROUTINE canadian_calc_bui



!#include "canadian_calc_fwi.inc"
! Part of the canadian module. Performs the fire weather index calculation

SUBROUTINE canadian_calc_fwi(                                                  &
!Array INTENT(IN)                                                           &
isi, bui,                                                                      &
!Array INTENT(OUT)                                                          &
fwi)

!No Imports, but has access to module variables.

IMPLICIT NONE

!
! Description:
!   Calculates the the FWI.
!   Only called from within the module
!
! Method:
!   See documentation for further details.
!   Equation numbers refer to the reference given at the top of the module
!   Equation numbers in {curly brackets} refer to the Met Office documentation
!   referred to at the top of the module.
!
!   All variables are arrays of the same length
!   Local arrays have been initialised to SIZE of an argument with a short name
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!

! ! Subroutine arguments
! < Array arguments with INTENT(IN) >
REAL(KIND=real_jlslsm),     INTENT(IN)  ::  isi(:),                            &
                                            bui(:)

! < Array arguments with INTENT(OUT)>
REAL(KIND=real_jlslsm),     INTENT(OUT) ::  fwi(:)

! < Array local variables >
REAL(KIND=real_jlslsm)                  ::  b(SIZE(isi)),                      &
                                            fd(SIZE(isi))

! End of header
  !All variables used below here are 1-D arrays. (:) notation is omitted.

  !{Documentation eq25-27}

WHERE (bui <= 80)
  ! Equation 38a
  fd = 0.626 * (bui ** 0.809) + 2.0
ELSE WHERE
  ! Equation 38b
  fd = 1000.0 / (25+108.64 * EXP(-0.023 * bui))
END WHERE

! Equation 39
b = 0.1 * isi * fd

WHERE (b > 1.0)
  ! Equation 40
  fwi = EXP(2.72 * ((0.434 * LOG(b))**0.647))
ELSE WHERE
  fwi = b
END WHERE

RETURN
END SUBROUTINE canadian_calc_fwi

END MODULE canadian
