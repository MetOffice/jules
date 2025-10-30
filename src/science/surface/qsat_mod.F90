#if !defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Atmosphere Service

! Overarching module for calculating saturation vapour pressure.
! Calculates:
! Saturation Specific Humidity (Qsat): Vapour to Liquid/Ice.
! Saturation Specific Humidity (Qsat_Wat): Vapour to Liquid.

! All return a saturation mixing ratio given a temperature and pressure
! using saturation vapour pressures calculated using the Goff-Gratch
! formulae, adopted by the WMO as taken from Landolt-Bornstein, 1987
! Numerical Data and Functional Relationships in Science and Technology.
! Group V/vol 4B meteorology. Physical and Chemical properties of air, P35

! Note regarding values stored in the lookup tables (es):
! _wat versions     : over water above and below 0 deg c.
! non-_wat versions : over water above 0 deg c
!                     over ice below 0 deg c

! Method:
! Uses lookup tables to find eSAT, calculates qSAT directly from that.

! Documentation: UMDP No.29

MODULE qsat_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

! Set everything as private, and only expose the interfaces we want to
PRIVATE
PUBLIC :: qsat, qsat_wat, qsat_mix, qsat_wat_mix

INTERFACE qsat
  MODULE PROCEDURE qsat_1D,                                                    &
                   qsat_2D,                                                    &
                   qsat_scalar
END INTERFACE

INTERFACE qsat_wat
  MODULE PROCEDURE qsat_wat_1D,                                                &
                   qsat_wat_2D,                                                &
                   qsat_wat_scalar
END INTERFACE

INTERFACE qsat_mix
  MODULE PROCEDURE qsat_mix_1D,                                                &
                   qsat_mix_2D,                                                &
                   qsat_mix_scalar
END INTERFACE

INTERFACE qsat_wat_mix
  MODULE PROCEDURE qsat_wat_mix_1D,                                            &
                   qsat_wat_mix_2D,                                            &
                   qsat_wat_mix_scalar
END INTERFACE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='QSAT_NEW_MOD'

CONTAINS

SUBROUTINE qsat_1D(qs, t, p, npnts)
! Remember to use the correct flavour of es
USE qsat_data_mod, ONLY:  t_low, t_high, delta_t, es
! Use in required info. Make sure it's the right KIND
USE planet_constants_mod, ONLY: repsilon, one_minus_epsilon
USE water_constants_mod,  ONLY: zerodegc => tm
IMPLICIT NONE
! Subroutine Arguments:
INTEGER, INTENT(IN)  :: npnts
REAL(KIND=real_jlslsm),    INTENT(IN)  :: t(npnts), p(npnts)
REAL(KIND=real_jlslsm),    INTENT(OUT) :: qs(npnts)

! Local scalars
INTEGER              :: itable, i
REAL(KIND=real_jlslsm)                 :: atable, fsubw, tt

! Local parameters allows simple templating of KINDs
REAL(KIND=real_jlslsm), PARAMETER      :: one   = 1.0,                         &
                        pconv = 1.0e-8,                                        &
                        term1 = 4.5,                                           &
                        term2 = 6.0e-4

DO i = 1, npnts
  ! Compute the factor that converts from sat vapour pressure in a
  ! pure water system to sat vapour pressure in air, fsubw.
  ! This formula is taken from equation A4.7 of Adrian Gill's book:
  ! atmosphere-ocean dynamics. Note that his formula works in terms
  ! of pressure in mb and temperature in celsius, so conversion of
  ! units leads to the slightly different equation used here.
  fsubw = one + pconv * p(i) *                                                 &
  (term1 + term2 * (t(i) - zerodegc) * (t(i) - zerodegc))

  ! Use the lookup table to find saturated vapour pressure. Store it in qs.
  tt     = MAX(t_low,t(i))
  tt     = MIN(t_high,tt)
  atable = (tt - t_low + delta_t) / delta_t
  itable = atable
  atable = atable - itable
  qs(i)  = (one - atable) * es(itable) + atable * es(itable+1)

  ! Multiply by fsubw to convert to saturated vapour pressure in air
  ! (equation A4.6 OF Adrian Gill's book).
  qs(i)  = qs(i) * fsubw

  ! Now form the accurate expression for qs, which is a rearranged
  ! version of equation A4.3 of Gill's book.
  ! Note that at very low pressures we apply a fix, to prevent a
  ! singularity (qsat tends to 1. kg/kg).
  qs(i)  = (repsilon * qs(i)) / (MAX(p(i), qs(i)) - one_minus_epsilon * qs(i))
END DO
RETURN
END SUBROUTINE qsat_1D

!==============================================================================

SUBROUTINE qsat_wat_1D(qs, t, p, npnts)
! Remember to use the correct flavour of es
USE qsat_data_mod, ONLY:  t_low, t_high, delta_t, es => es_wat
! Use in required info. Make sure it's the right KIND
USE planet_constants_mod, ONLY: repsilon, one_minus_epsilon
USE water_constants_mod,  ONLY: zerodegc => tm
IMPLICIT NONE
! Subroutine Arguments:
INTEGER, INTENT(IN)  :: npnts
REAL(KIND=real_jlslsm),    INTENT(IN)  :: t(npnts), p(npnts)
REAL(KIND=real_jlslsm),    INTENT(OUT) :: qs(npnts)

! Local scalars
INTEGER              :: itable, i
REAL(KIND=real_jlslsm)                 :: atable, fsubw, tt

! Local parameters allows simple templating of KINDs
REAL(KIND=real_jlslsm), PARAMETER      :: one   = 1.0,                         &
                        pconv = 1.0e-8,                                        &
                        term1 = 4.5,                                           &
                        term2 = 6.0e-4

DO i = 1, npnts
  ! Compute the factor that converts from sat vapour pressure in a
  ! pure water system to sat vapour pressure in air, fsubw.
  ! This formula is taken from equation A4.7 of Adrian Gill's book:
  ! atmosphere-ocean dynamics. Note that his formula works in terms
  ! of pressure in mb and temperature in celsius, so conversion of
  ! units leads to the slightly different equation used here.
  fsubw = one + pconv * p(i) *                                                 &
  (term1 + term2 * (t(i) - zerodegc) * (t(i) - zerodegc))

  ! Use the lookup table to find saturated vapour pressure. Store it in qs.
  tt     = MAX(t_low,t(i))
  tt     = MIN(t_high,tt)
  atable = (tt - t_low + delta_t) / delta_t
  itable = atable
  atable = atable - itable
  qs(i)  = (one - atable) * es(itable) + atable * es(itable+1)

  ! Multiply by fsubw to convert to saturated vapour pressure in air
  ! (equation A4.6 OF Adrian Gill's book).
  qs(i)  = qs(i) * fsubw

  ! Now form the accurate expression for qs, which is a rearranged
  ! version of equation A4.3 of Gill's book.
  ! Note that at very low pressures we apply a fix, to prevent a
  ! singularity (qsat tends to 1. kg/kg).
  qs(i)  = (repsilon * qs(i)) / (MAX(p(i), qs(i)) - one_minus_epsilon * qs(i))
END DO
RETURN
END SUBROUTINE qsat_wat_1D

!==============================================================================

SUBROUTINE qsat_mix_1D(qs, t, p, npnts)
! Remember to use the correct flavour of es
USE qsat_data_mod, ONLY:  t_low, t_high, delta_t, es
! Use in required info. Make sure it's the right KIND
USE planet_constants_mod, ONLY: repsilon
USE water_constants_mod,  ONLY: zerodegc => tm
IMPLICIT NONE
! Subroutine Arguments:
INTEGER, INTENT(IN)  :: npnts
REAL(KIND=real_jlslsm),    INTENT(IN)  :: t(npnts), p(npnts)
REAL(KIND=real_jlslsm),    INTENT(OUT) :: qs(npnts)

! Local scalars
INTEGER              :: itable, i
REAL(KIND=real_jlslsm)                 :: atable, fsubw, tt

! Local parameters allows simple templating of KINDs
REAL(KIND=real_jlslsm), PARAMETER      :: one   = 1.0,                         &
                        pconv = 1.0e-8,                                        &
                        term1 = 4.5,                                           &
                        term2 = 6.0e-4,                                        &
                        term3 = 1.1

DO i = 1, npnts
  ! Compute the factor that converts from sat vapour pressure in a
  ! pure water system to sat vapour pressure in air, fsubw.
  ! This formula is taken from equation A4.7 of Adrian Gill's book:
  ! atmosphere-ocean dynamics. Note that his formula works in terms
  ! of pressure in mb and temperature in celsius, so conversion of
  ! units leads to the slightly different equation used here.
  fsubw = one + pconv * p(i) *                                                 &
  (term1 + term2 * (t(i) - zerodegc) * (t(i) - zerodegc))

  ! Use the lookup table to find saturated vapour pressure. Store it in qs.
  tt     = MAX(t_low,t(i))
  tt     = MIN(t_high,tt)
  atable = (tt - t_low + delta_t) / delta_t
  itable = atable
  atable = atable - itable
  qs(i)  = (one - atable) * es(itable) + atable * es(itable+1)

  ! Multiply by fsubw to convert to saturated vapour pressure in air
  ! (equation A4.6 OF Adrian Gill's book).
  qs(i)  = qs(i) * fsubw

  ! Now form the accurate expression for qs, which is a rearranged
  ! version of equation A4.3 of Gill's book.
  ! Note that at very low pressures we apply a fix, to prevent a
  ! singularity (qsat tends to 1. kg/kg).
  qs(i)  = (repsilon * qs(i)) / (MAX(p(i), term3 * qs(i)) - qs(i))
END DO
RETURN
END SUBROUTINE qsat_mix_1D

!==============================================================================

SUBROUTINE qsat_wat_mix_1D(qs, t, p, npnts)
! Remember to use the correct flavour of es
USE qsat_data_mod, ONLY:  t_low, t_high, delta_t, es => es_wat
! Use in required info. Make sure it's the right KIND
USE planet_constants_mod, ONLY: repsilon
USE water_constants_mod,  ONLY: zerodegc => tm
IMPLICIT NONE
! Subroutine Arguments:
INTEGER, INTENT(IN)  :: npnts
REAL(KIND=real_jlslsm),    INTENT(IN)  :: t(npnts), p(npnts)
REAL(KIND=real_jlslsm),    INTENT(OUT) :: qs(npnts)

! Local scalars
INTEGER              :: itable, i
REAL(KIND=real_jlslsm)                 :: atable, fsubw, tt

! Local parameters allows simple templating of KINDs
REAL(KIND=real_jlslsm), PARAMETER      :: one   = 1.0,                         &
                        pconv = 1.0e-8,                                        &
                        term1 = 4.5,                                           &
                        term2 = 6.0e-4,                                        &
                        term3 = 1.1

DO i = 1, npnts
  ! Compute the factor that converts from sat vapour pressure in a
  ! pure water system to sat vapour pressure in air, fsubw.
  ! This formula is taken from equation A4.7 of Adrian Gill's book:
  ! atmosphere-ocean dynamics. Note that his formula works in terms
  ! of pressure in mb and temperature in celsius, so conversion of
  ! units leads to the slightly different equation used here.
  fsubw = one + pconv * p(i) *                                                 &
  (term1 + term2 * (t(i) - zerodegc) * (t(i) - zerodegc))

  ! Use the lookup table to find saturated vapour pressure. Store it in qs.
  tt     = MAX(t_low,t(i))
  tt     = MIN(t_high,tt)
  atable = (tt - t_low + delta_t) / delta_t
  itable = atable
  atable = atable - itable
  qs(i)  = (one - atable) * es(itable) + atable * es(itable+1)

  ! Multiply by fsubw to convert to saturated vapour pressure in air
  ! (equation A4.6 OF Adrian Gill's book).
  qs(i)  = qs(i) * fsubw

  ! Now form the accurate expression for qs, which is a rearranged
  ! version of equation A4.3 of Gill's book.
  ! Note that at very low pressures we apply a fix, to prevent a
  ! singularity (qsat tends to 1. kg/kg).
  qs(i)  = (repsilon * qs(i)) / (MAX(p(i), term3 * qs(i)) - qs(i))
END DO
RETURN
END SUBROUTINE qsat_wat_mix_1D

! ------------------------------------------------------------------------------
! 2D subroutines

SUBROUTINE qsat_2D(qs, t, p, npntsi, npntsj)
IMPLICIT NONE
INTEGER,               INTENT(IN)  :: npntsi, npntsj
REAL(KIND=real_jlslsm),    INTENT(IN)  :: t(npntsi,npntsj), p(npntsi,npntsj)
REAL(KIND=real_jlslsm),    INTENT(OUT) :: qs(npntsi,npntsj)
INTEGER                            :: j
DO j = 1, npntsj
  CALL qsat_1D(qs(1,j),t(1,j),p(1,j),npntsi)
END DO
RETURN
END SUBROUTINE qsat_2D

SUBROUTINE qsat_wat_2D(qs, t, p, npntsi, npntsj)
IMPLICIT NONE
INTEGER,               INTENT(IN)  :: npntsi, npntsj
REAL(KIND=real_jlslsm),    INTENT(IN)  :: t(npntsi,npntsj), p(npntsi,npntsj)
REAL(KIND=real_jlslsm),    INTENT(OUT) :: qs(npntsi,npntsj)
INTEGER                            :: j
DO j = 1, npntsj
  CALL qsat_wat_1D(qs(1,j),t(1,j),p(1,j),npntsi)
END DO
RETURN
END SUBROUTINE qsat_wat_2D

SUBROUTINE qsat_mix_2D(qs, t, p, npntsi, npntsj)
IMPLICIT NONE
INTEGER,               INTENT(IN)  :: npntsi, npntsj
REAL(KIND=real_jlslsm),    INTENT(IN)  :: t(npntsi,npntsj), p(npntsi,npntsj)
REAL(KIND=real_jlslsm),    INTENT(OUT) :: qs(npntsi,npntsj)
INTEGER                            :: j
DO j = 1, npntsj
  CALL qsat_mix_1D(qs(1,j),t(1,j),p(1,j),npntsi)
END DO
RETURN
END SUBROUTINE qsat_mix_2D

SUBROUTINE qsat_wat_mix_2D(qs, t, p, npntsi, npntsj)
IMPLICIT NONE
INTEGER,               INTENT(IN)  :: npntsi, npntsj
REAL(KIND=real_jlslsm),    INTENT(IN)  :: t(npntsi,npntsj), p(npntsi,npntsj)
REAL(KIND=real_jlslsm),    INTENT(OUT) :: qs(npntsi,npntsj)
INTEGER                            :: j
DO j = 1, npntsj
  CALL qsat_wat_mix_1D(qs(1,j),t(1,j),p(1,j),npntsi)
END DO
RETURN
END SUBROUTINE qsat_wat_mix_2D

! ------------------------------------------------------------------------------
! scalar subroutines

SUBROUTINE qsat_scalar(qs, t, p)
IMPLICIT NONE
REAL(KIND=real_jlslsm),    INTENT(IN)  :: t, p
REAL(KIND=real_jlslsm),    INTENT(OUT) :: qs
REAL(KIND=real_jlslsm)                 :: t_arr(1), p_arr(1), qs_arr(1)
t_arr(1) = t
p_arr(1) = p
CALL qsat_1D(qs_arr,t_arr,p_arr,1)
qs = qs_arr(1)
RETURN
END SUBROUTINE qsat_scalar

SUBROUTINE qsat_wat_scalar(qs, t, p)
IMPLICIT NONE
REAL(KIND=real_jlslsm),    INTENT(IN)  :: t, p
REAL(KIND=real_jlslsm),    INTENT(OUT) :: qs
REAL(KIND=real_jlslsm)                 :: t_arr(1), p_arr(1), qs_arr(1)
t_arr(1) = t
p_arr(1) = p
CALL qsat_wat_1D(qs_arr,t_arr,p_arr,1)
qs = qs_arr(1)
RETURN
END SUBROUTINE qsat_wat_scalar

SUBROUTINE qsat_mix_scalar(qs, t, p)
IMPLICIT NONE
REAL(KIND=real_jlslsm),    INTENT(IN)  :: t, p
REAL(KIND=real_jlslsm),    INTENT(OUT) :: qs
REAL(KIND=real_jlslsm)                 :: t_arr(1), p_arr(1), qs_arr(1)
t_arr(1) = t
p_arr(1) = p
CALL qsat_mix_1D(qs_arr,t_arr,p_arr,1)
qs = qs_arr(1)
RETURN
END SUBROUTINE qsat_mix_scalar

SUBROUTINE qsat_wat_mix_scalar(qs, t, p)
IMPLICIT NONE
REAL(KIND=real_jlslsm),    INTENT(IN)  :: t, p
REAL(KIND=real_jlslsm),    INTENT(OUT) :: qs
REAL(KIND=real_jlslsm)                 :: t_arr(1), p_arr(1), qs_arr(1)
t_arr(1) = t
p_arr(1) = p
CALL qsat_wat_mix_1D(qs_arr,t_arr,p_arr,1)
qs = qs_arr(1)
RETURN
END SUBROUTINE qsat_wat_mix_scalar

END MODULE qsat_mod
!==============================================================================
#endif
