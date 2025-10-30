! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Description:
!
!  Part of the UKCA model, a community model supported by the
!  Met Office and NCAS, with components provided initially
!  by The University of Cambridge, University of Leeds and
!  The Met. Office.  See www.ukca.ac.uk
!  This module was provided by CSIRO.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: UKCA
!
!
! Code description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 programming standards.
!
! ---------------------------------------------------------------------
!
MODULE deposition_ukca_ddepo3_ocean_mod

IMPLICIT NONE

! Description
! Routine and functions to calculate the surface resistance term rc for ozone
!  dry deposition to the ocean

PRIVATE

PUBLIC :: deposition_ukca_ddepo3_ocean

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName = 'DEPOSITION_UKCA_DDEPO3_OCEAN_MOD'

CONTAINS

! ---------------------------------------------------------------------
! Subroutine Interface
SUBROUTINE deposition_ukca_ddepo3_ocean(p0,t0,sst,usa,rc)

! Description
! A new two-layer model to calculate the surface resistance (rc) of ocean
! surface to ozone dry deposition (based on Luhar et al. (2018, 18: 4329, ACP)

USE planet_constants_mod,    ONLY: vkman, rgas => r
USE water_constants_mod,     ONLY: rhosea
USE conversions_mod,         ONLY: pi
USE um_types,                ONLY: real_jlslsm
USE parkind1,                ONLY: jpim, jprb
USE yomhook,                 ONLY: lhook, dr_hook

IMPLICIT NONE

! Subroutine arguments
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  p0,                                                                          &
    ! surface pressure (Pa)
  t0,                                                                          &
    ! surface temperature (K)
  sst,                                                                         &
    ! sea surface temperature (K)
  usa
    ! air friction velocity (m/s)

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  rc
    ! surface resistance term (s/m)

REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  del = 3.0e-6
    ! assumed depth of ocean surface micro layer (m)

! Local variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in = 0 ! DrHook tracing entry
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1 ! DrHook tracing exit
REAL(KIND=jprb) :: zhook_handle

REAL(KIND=real_jlslsm) ::                                                      &
  rhoa,                                                                        &
    ! air density (kg/m3)
  alpha,                                                                       &
    ! ozone dimensionless solubility
  usw,                                                                         &
    ! waterside friction velocity (m/s)
  m_diff,                                                                      &
    ! molecular diffusivity (m2/s)
  iodide,                                                                      &
    ! sea surface iodide concentration (M)
  k_rate,                                                                      &
    ! iodide-ozone second-order rate constant (M^-1 s^-1)
  vdw
    ! waterside deposition velocity (m/s)

! intermediate variables
REAL(KIND=real_jlslsm) ::                                                      &
  a1, b0, delzt1, argm, psi, trm2

CHARACTER(LEN=*), PARAMETER :: RoutineName='DEPOSITION_UKCA_DDEPO3_OCEAN'

!------------------------------------------------------------------------
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)


rhoa = p0 / ( rgas * t0 ) ! air density (kg/m3)

usw = SQRT( rhoa / rhosea ) * usa  ! waterside friction velocity

b0=2.0/vkman/usw

! alpha= Ozone dimensionless solubility (1/alpha = H = Henry's constant)
alpha=10.0**(-0.25-0.013*(sst-273.15)) !empirical relationship

! molecular diffusivity of ozone in water (m2/s)
m_diff=1.1e-6*EXP(-1896.0/sst) !Johnson and Davis (1996)

! ocean iodide concentration - based on MacDonald at al. 2014 (M)
iodide=1.46*1.0e6*EXP(-9134.0/sst)

! iodide-ozone second-order rate constant (M^-1 s^-1)
k_rate=EXP((-8772.2/sst)+51.5)

a1 = k_rate*iodide

delzt1=SQRT(2*a1*b0*(del+m_diff*b0/2.0))
argm=SQRT(a1/m_diff)*del
trm2 = SQRT(a1*m_diff)
psi = delzt1/b0/trm2
vdw=SQRT(a1*m_diff)*(psi*b_k1(delzt1)*COSH(argm)+b_k0(delzt1)*SINH(argm))/     &
      (psi*b_k1(delzt1)*SINH(argm)+b_k0(delzt1)*COSH(argm))

rc=1.0/(alpha*vdw) ! surface resistance (s/m)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

END SUBROUTINE deposition_ukca_ddepo3_ocean


!-------------------------------------------
!
! Bessel function routines used by the new, mechanistic overwater ozone dry
!    depostion scheme
! Functions are based on polynomial equations given in Abramowitz and Stegun
!    (1964, Handbook of Mathematical Functions, Dover Publications, New York)
!
REAL FUNCTION b_k0(x)
IMPLICIT NONE

REAL, INTENT(IN) :: x
REAL :: y

REAL, PARAMETER :: f1 = -0.57721566
REAL, PARAMETER :: f2 =  0.42278420
REAL, PARAMETER :: f3 =  0.23069756
REAL, PARAMETER :: f4 =  0.3488590e-1
REAL, PARAMETER :: f5 =  0.262698e-2
REAL, PARAMETER :: f6 =  0.10750e-3
REAL, PARAMETER :: f7 =  0.74e-5
REAL, PARAMETER :: g1 =  1.25331414
REAL, PARAMETER :: g2 = -0.7832358e-1
REAL, PARAMETER :: g3 =  0.2189568e-1
REAL, PARAMETER :: g4 = -0.1062446e-1
REAL, PARAMETER :: g5 =  0.587872e-2
REAL, PARAMETER :: g6 = -0.251540e-2
REAL, PARAMETER :: g7 =  0.53208e-3

IF (x <= 2.0) THEN
  y   = x*x/4.0
  b_k0 = (-LOG(x/2.0)*b_i0(x))+(f1+y*(f2+y*(f3+y*(f4+y*(f5+y*                  &
           (f6+y*f7))))))
ELSE
  y   = 2.0/x
  b_k0 = (EXP(-x)/SQRT(x))*(g1+y*(g2+y*(g3+y*(g4+y*(g5+y*(g6+y*                &
           g7))))))
END IF

RETURN
END FUNCTION b_k0

!---------------------------------------------------------------

REAL FUNCTION b_k1(x)
IMPLICIT NONE

REAL, INTENT(IN) :: x
REAL :: y

REAL, PARAMETER :: f1 =  1.0000
REAL, PARAMETER :: f2 =  0.15443144
REAL, PARAMETER :: f3 = -0.67278579
REAL, PARAMETER :: f4 = -0.18156897
REAL, PARAMETER :: f5 = -0.1919402e-1
REAL, PARAMETER :: f6 = -0.110404e-2
REAL, PARAMETER :: f7 = -0.4686e-4
REAL, PARAMETER :: g1 =  1.25331414
REAL, PARAMETER :: g2 =  0.23498619
REAL, PARAMETER :: g3 = -0.3655620e-1
REAL, PARAMETER :: g4 =  0.1504268e-1
REAL, PARAMETER :: g5 = -0.780353e-2
REAL, PARAMETER :: g6 =  0.325614e-2
REAL, PARAMETER :: g7 = -0.68245e-3

IF (x <= 2.0) THEN
  y   = x*x/4.0
  b_k1 = (LOG(x/2.0)*b_i1(x))+(1.0/x)*(f1+y*(f2+y*(f3+y*(f4+y*                 &
           (f5+y*(f6+y*f7))))))
ELSE
  y   = 2.0/x
  b_k1 = (EXP(-x)/SQRT(x))*(g1+y*(g2+y*(g3+y*(g4+y*(g5+y*(g6+y*                &
           g7))))))
END IF

RETURN
END FUNCTION b_k1

!----------------------------------------------------------------

REAL FUNCTION b_i0(x)
IMPLICIT NONE

REAL, INTENT(IN) :: x
REAL :: xm
REAL :: y

REAL, PARAMETER :: f1 =  1.0000
REAL, PARAMETER :: f2 =  3.5156229
REAL, PARAMETER :: f3 =  3.0899424
REAL, PARAMETER :: f4 =  1.2067492
REAL, PARAMETER :: f5 =  0.2659732
REAL, PARAMETER :: f6 =  0.360768e-1
REAL, PARAMETER :: f7 =  0.45813e-2
REAL, PARAMETER :: g1 =  0.39894228
REAL, PARAMETER :: g2 =  0.1328592e-1
REAL, PARAMETER :: g3 =  0.225319e-2
REAL, PARAMETER :: g4 = -0.157565e-2
REAL, PARAMETER :: g5 =  0.916281e-2
REAL, PARAMETER :: g6 = -0.2057706e-1
REAL, PARAMETER :: g7 =  0.2635537e-1
REAL, PARAMETER :: g8 = -0.1647633e-1
REAL, PARAMETER :: g9 =  0.392377e-2

IF (ABS(x) < 3.75) THEN
  y   = (x/3.75)**2
  b_i0 = f1+y*(f2+y*(f3+y*(f4+y*(f5+y*(f6+y*f7)))))
ELSE
  xm  = ABS(x)
  y   = 3.75/xm
  b_i0 = (EXP(xm)/SQRT(xm))*(g1+y*(g2+y*(g3+y*(g4+y*(g5+y*(g6+y*               &
           (g7+y*(g8+y*g9))))))))
END IF

RETURN
END FUNCTION b_i0

!-----------------------------------------------------------------

REAL FUNCTION b_i1(x)
IMPLICIT NONE

REAL, INTENT(IN) :: x
REAL :: xm
REAL :: y

REAL, PARAMETER :: f1 =  0.5000
REAL, PARAMETER :: f2 =  0.87890594
REAL, PARAMETER :: f3 =  0.51498869
REAL, PARAMETER :: f4 =  0.15084934
REAL, PARAMETER :: f5 =  0.2658733e-1
REAL, PARAMETER :: f6 =  0.301532e-2
REAL, PARAMETER :: f7 =  0.32411e-3
REAL, PARAMETER :: g1 =  0.39894228
REAL, PARAMETER :: g2 = -0.3988024e-1
REAL, PARAMETER :: g3 = -0.362018e-2
REAL, PARAMETER :: g4 =  0.163801e-2
REAL, PARAMETER :: g5 = -0.1031555e-1
REAL, PARAMETER :: g6 =  0.2282967e-1
REAL, PARAMETER :: g7 = -0.2895312e-1
REAL, PARAMETER :: g8 =  0.1787654e-1
REAL, PARAMETER :: g9 = -0.420059e-2

IF (ABS(x) < 3.75) THEN
  y   = (x/3.75)**2
  b_i1 = x*(f1+y*(f2+y*(f3+y*(f4+y*(f5+y*(f6+y*f7))))))
ELSE
  xm = ABS(x)
  y  = 3.75/xm
  b_i1 = (EXP(xm)/SQRT(xm))*(g1+y*(g2+y*(g3+y*(g4+y*(g5+y*(g6+y*               &
           (g7+y*(g8+y*g9))))))))
  IF (x < 0.0) THEN
    b_i1 = -b_i1
  END IF
END IF

RETURN
END FUNCTION b_i1

!-----------------------------------------------------------------


END MODULE deposition_ukca_ddepo3_ocean_mod
