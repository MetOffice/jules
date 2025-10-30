! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    SUBROUTINE FREEZE_SOIL -----------------------------------------

!
! Subroutine Interface:
MODULE freeze_soil_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='FREEZE_SOIL_MOD'

CONTAINS

SUBROUTINE freeze_soil (npnts,nshyd,b,dz,                                      &
                        sathh,smcl,tsoil,v_sat,sthu,sthf)

USE water_constants_mod, ONLY: rho_water, dpsidt
USE conversions_mod, ONLY: zerodegc

USE ereport_mod, ONLY: Ereport

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

IMPLICIT NONE

!
! Description:
!     Calculates the unfrozen and frozen water within a soil layer
!     as a fraction of saturation.                          (Cox, 6/95)
!
! Documentation : UM Documentation Paper 25
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: FORTRAN 90
!
! Subroutine arguments
!   Scalar arguments with intent(IN) :
INTEGER ::                                                                     &
 npnts                                                                         &
                      ! IN Number of gridpoints.
,nshyd                ! IN Number of soil layers.


!   Array arguments with intent(IN) :

REAL(KIND=real_jlslsm) ::                                                      &
 b(npnts)                                                                      &
                      ! IN Brooks & Corey exponent.
,dz(nshyd)                                                                     &
                      ! IN Thicknesses of the soil layers (m).
,sathh(npnts)                                                                  &
                      ! IN Saturated soil water pressure (m).
,smcl(npnts,nshyd)                                                             &
                      ! IN Soil moisture content of
                      !    layers (kg/m2).
,tsoil(npnts,nshyd)                                                            &
                      ! IN Sub-surface temperatures (K).
,v_sat(npnts)         ! IN Volumetric soil moisture
                      !    concentration at saturation
                      !    (m3 H2O/m3 soil).

!   Array arguments with intent(OUT) :
REAL(KIND=real_jlslsm) ::                                                      &
 sthf(npnts,nshyd)                                                             &
                      ! OUT Frozen soil moisture content of
                      !     the layers as a fraction of
                      !     saturation.
,sthu(npnts,nshyd)    ! OUT Unfrozen soil moisture content of
                      !     the layers as a fraction of
                      !     saturation.

! Local scalars:
INTEGER ::                                                                     &
 i,n                ! WORK Loop counters.

! Local arrays:
REAL(KIND=real_jlslsm) ::                                                      &
 smclf(npnts,nshyd)                                                            &
                      ! WORK Frozen moisture content of the
                      !      soil layers (kg/m2).
,smclu(npnts,nshyd)                                                            &
                      ! WORK Unfrozen moisture content of the
                      !      soil layers (kg/m2).
,smclsat(npnts,nshyd)                                                          &
                      ! WORK The saturation moisture content of
                      !      the layers (kg/m2).
,tmax(npnts)                                                                   &
                      ! WORK Temperature above which all water is
                      !      unfrozen (Celsius)
,tsl(npnts,nshyd)     ! WORK Soil layer temperatures (Celsius).

REAL(KIND=real_jlslsm) :: work1(npnts,nshyd)
                            ! Stores (SMCL(I,N)/SMCLSAT(I,N))**(B(I))

LOGICAL :: l_dz_zero     ! Warn if DZ contains zero values

! Error reporting
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'FREEZE_SOIL'
INTEGER                  :: errorstatus

REAL(KIND=real_jlslsm) :: tiny_0, small_value

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
tiny_0 = TINY(0.0)
small_value = EPSILON(0.0)

l_dz_zero = .FALSE.

! Warn if soil layer thickness less than tiny_0 .
DO n = 1,nshyd
  IF ( dz(n)<tiny_0 ) THEN
    l_dz_zero = .TRUE.
  END IF
END DO !NSHYD
IF (l_dz_zero) THEN
  errorstatus = 42
  CALL ereport(RoutineName, errorstatus, "Soil layer thickness is too small.")
END IF

DO n = 1,nshyd

  DO i = 1,npnts
    !-----------------------------------------------------------------------
    ! Calculate TMAX, the temperature above which all soil water is
    ! unfrozen
    !-----------------------------------------------------------------------
    smclsat(i,n) = rho_water * dz(n) * v_sat(i)
    tsl(i,n) = tsoil(i,n) - zerodegc
    IF ((v_sat(i) > tiny_0) .AND. (smcl(i,n) > small_value)) THEN
      work1(i,n)=(smcl(i,n) / smclsat(i,n))**(b(i))
      IF ( work1(i,n) > small_value ) THEN
        tmax(i)=-sathh(i) / (dpsidt * work1(i,n))
      ELSE
        tmax(i)=-273.15
      END IF
    ELSE
      tmax(i)=-273.15
    END IF

    !--------------------------------------------------------------------
    ! Diagnose unfrozen and frozen water contents
    !--------------------------------------------------------------------
    IF (tsl(i,n) >= tmax(i)) THEN
      smclu(i,n) = smcl(i,n)
      smclf(i,n) = 0.0
    ELSE
      !-----------------------------------------------------------------
      ! For ice points (V_SAT=0) set SMCLU=0.0 and SMCLF=0.0
      !-----------------------------------------------------------------
      IF (v_sat(i) == 0.0) THEN
        smclu(i,n) = 0.0
        smclf(i,n) = 0.0
      ELSE
        smclu(i,n) = smclsat(i,n)                                              &
                    *(-dpsidt * tsl(i,n) / sathh(i))**(-1.0 / b(i))
        smclf(i,n) = smcl(i,n) - smclu(i,n)
      END IF
    END IF
    IF (smclsat(i,n) >  0.0) THEN
      sthf(i,n) = smclf(i,n) / smclsat(i,n)
      sthu(i,n) = smclu(i,n) / smclsat(i,n)
    ELSE
      sthf(i,n) = 0.0
      sthu(i,n) = 0.0
    END IF
  END DO

END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE freeze_soil
END MODULE freeze_soil_mod
