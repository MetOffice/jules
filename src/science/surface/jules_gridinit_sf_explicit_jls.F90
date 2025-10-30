! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE jules_gridinit_sf_explicit_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
                  ModuleName='JULES_GRIDINIT_SF_EXPLICIT_MOD'

CONTAINS
!  SUBROUTINE JULES_GRIDINIT_SF_EXPLICIT -----------------------------
!
!  Purpose: Initialise arrays for explicit calculation of surface fluxes
!           of heat, moisture and momentum
!
!
!  Documentation: UMDP 24.
!
!---------------------------------------------------------------------
!    Arguments :-
SUBROUTINE jules_gridinit_sf_explicit (                                        &
! IN soil/vegetation/land surface data :
flandg,                                                                        &
! IN everything not covered so far :
 pstar,tstar,                                                                  &
! IN variables for message passing
 u_1_px, v_1_px, u_0_px, v_0_px,                                               &
! INOUT
 sf_diag,                                                                      &
! OUT Diagnostic not requiring STASH flags :
 fqw_1,ftl_1,                                                                  &
! OUT variables for message passing
 flandfac, fseafac, cdr10m,                                                    &
! OUT data required elsewhere in UM system :
 t1_sd,q1_sd,                                                                  &
! OUT data required elsewhere in boundary layer or surface code
 rhostar,vshr,vshr_land,vshr_ssi,mapi,mapj                                     &
 )

USE planet_constants_mod, ONLY: r

USE atm_fields_bounds_mod, ONLY: pdims_s, pdims, tdims
USE ancil_info, ONLY: stencil_i, stencil_j
USE jules_sea_seaice_mod, ONLY: l_ctile, buddy_sea
USE jules_surface_mod, ONLY: on

USE sf_diags_mod, ONLY: strnewsfdiag

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE
!-----------------------------------------------------------------------
!  Inputs :-
!-----------------------------------------------------------------------
! (a) Defining horizontal grid and subset thereof to be processed.
!    Checked for consistency.
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 u_1_px(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end),          &
 v_1_px(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end),          &
 u_0_px(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end),          &
 v_0_px(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end)

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 flandg(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end)
                             ! IN Land fraction on all tiles.
                             !    divided by 2SQRT(2) on land
                             !    points only (m)


! (f) Atmospheric + any other data not covered so far, incl control.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 pstar(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end)                    &
                             ! IN Surface pressure (Pascals).
,tstar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! IN GBM surface temperature (K).

! Maps to the stencil points of each i and j index of an array
INTEGER :: mapi(tdims%i_start:tdims%i_end,stencil_i)
INTEGER :: mapj(tdims%j_start:tdims%j_end,stencil_j)

!-----------------------------------------------------------------------
!  In/outs :-
!-----------------------------------------------------------------------
!Diagnostics
TYPE (strnewsfdiag), INTENT(IN OUT) :: sf_diag
!-----------------------------------------------------------------------
!  Outputs :-
!-----------------------------------------------------------------------
!-1 Diagnostic (or effectively so - includes coupled model requisites):-
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 fqw_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! OUT Moisture flux between layers
                             !     (kg per square metre per sec).
                             !     FQW(,1) is total water flux
                             !     from surface, 'E'.
,ftl_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! OUT FTL(,K) contains net turbulent
                             !     sensible heat flux into layer K
                             !     from below; so FTL(,1) is the
                             !     surface sensible heat, H.(W/m2)

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 flandfac(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end),        &
 fseafac(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end),         &
 cdr10m(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end)

!  (b) Not passed between lower-level routines (not in workspace at this
!      level) :-

!-2 Genuinely output, needed by other atmospheric routines :-
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 t1_sd(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! OUT Standard deviation of turbulent
                             !     fluctuations of layer 1 temp;
                             !     used in initiating convection.
,q1_sd(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! OUT Standard deviation of turbulent
                             !     flucs of layer 1 humidity;
                             !     used in initiating convection.

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 rhostar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                             ! OUT Surface air density
,vshr(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                             ! OUT Magnitude of surface-to-lowest
                             !     atm level wind shear (m per s).
,vshr_land(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                &
                             ! OUT Magnitude of surface-to-lowest
                             !     atm level wind shear (m per s).
,vshr_ssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! OUT Magnitude of surface-to-lowest
                             !     atm level wind shear (m per s).


!-----------------------------------------------------------------------
! LOCAL variables
!-----------------------------------------------------------------------
!  Workspace :-


!  Local scalars :-

INTEGER ::                                                                     &
 i,j                                                                           &
              ! Loop counter (horizontal field index).
,i_s,j_s                                                                       &
              ! Loop counter for coastal point stencil
,N_av_ws
              ! Counter for average wind speed

REAL(KIND=real_jlslsm) ::                                                      &
 ushear                                                                        &
              ! U-component of surface-to-lowest-level wind shear.
,vshear                                                                        &
              ! V-component of surface-to-lowest-level wind shear.
,vshr2        ! Square of magnitude of surface-to-lowest-level
              ! wind shear.

REAL(KIND=real_jlslsm) :: seawind  ! average wind speed adjacent to coast
REAL(KIND=real_jlslsm) :: fseamax  ! Maximum factor to apply to coast wind speed

! Minimum factor allowed to convert coastal wind speed to land part
REAL(KIND=real_jlslsm) :: flandmin
PARAMETER(flandmin = 0.2)

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='JULES_GRIDINIT_SF_EXPLICIT'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)


!-----------------------------------------------------------------------
!  0. Initialisations
!-----------------------------------------------------------------------

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j)                                      &
!$OMP SHARED(tdims,rhostar,pstar,tstar,r,ftl_1,fqw_1,q1_sd,t1_sd,cdr10m)
!$OMP DO SCHEDULE(STATIC)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    rhostar(i,j) = pstar(i,j) / ( r * tstar(i,j) )
    ! ... first approximation to surface air density from ideal gas equation
    ftl_1(i,j) = 0.0
    fqw_1(i,j) = 0.0
    q1_sd(i,j) = 0.0
    t1_sd(i,j) = 0.0
    cdr10m(i,j) = 0.0
  END DO
END DO
!$OMP END DO NOWAIT
!$OMP END PARALLEL

IF (sf_diag%l_tau_1) THEN
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      sf_diag%tau_1(i,j) = 0.0
    END DO
  END DO
END IF


!-----------------------------------------------------------------------
! Calculate wind shear between level 1 and the surface
!-----------------------------------------------------------------------

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(j,i,ushear,vshear,vshr2)                                         &
!$OMP SHARED(tdims,u_1_px,u_0_px,v_1_px,v_0_px,vshr_ssi,flandg,vshr_land,vshr)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    IF (flandg(i,j) <  1.0) THEN
      ushear = u_1_px(i,j) - u_0_px(i,j)
      vshear = v_1_px(i,j) - v_0_px(i,j)
      vshr2 = MAX (1.0e-6 , ushear * ushear + vshear * vshear)
      vshr_ssi(i,j) = SQRT(vshr2)
    ELSE
      vshr_ssi(i,j) = 0.0
    END IF

    IF (flandg(i,j) >  0.0) THEN
      vshr2 = MAX (1.0e-6 , u_1_px(i,j) * u_1_px(i,j)                          &
        + v_1_px(i,j) * v_1_px(i,j))
      vshr_land(i,j) = SQRT(vshr2)
    ELSE
      vshr_land(i,j) = 0.0
    END IF

    vshr(i,j)= flandg(i,j) * vshr_land(i,j)                                    &
      + (1.0 - flandg(i,j)) * vshr_ssi(i,j)
  END DO
END DO
!$OMP END PARALLEL DO

#if !defined(SCMA)

IF (l_ctile .AND. buddy_sea == on) THEN
!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(j,i,N_av_ws,i_s,j_s,ushear,vshear,vshr2,seawind,fseamax)         &
!$OMP SHARED(tdims,fseafac,flandfac,flandg,u_1_px,u_0_px,v_1_px,v_0_px,        &
!$OMP        vshr,vshr_ssi,vshr_land,mapi,mapj,stencil_i,stencil_j)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      fseafac(i,j)  = 1.0
      flandfac(i,j) = 1.0

      IF ( flandg(i,j) > 0.01 .AND. flandg(i,j) < 0.99 ) THEN
        !           !-----------------------------------------------------
        !           ! Calculate average windspeed over adjacent sea points
        !           !-----------------------------------------------------
        seawind = 0.0
        N_av_ws = 0
        DO i_s = 1,stencil_i
          DO j_s = 1,stencil_j
            IF ( flandg(mapi(i,i_s),mapj(j,j_s)) < 0.001 ) THEN
              ! ie. this is basically a sea point
              ushear = u_1_px(mapi(i,i_s),mapj(j,j_s))                         &
                     - u_0_px(mapi(i,i_s),mapj(j,j_s))
              vshear = v_1_px(mapi(i,i_s),mapj(j,j_s))                         &
                     - v_0_px(mapi(i,i_s),mapj(j,j_s))
              vshr2 = MAX (1.0e-10 , ushear * ushear + vshear * vshear)
              seawind = seawind + SQRT( vshr2 )
              N_av_ws = N_av_ws + 1
            END IF
          END DO
        END DO
        !           !-----------------------------------------------------
        !           ! Calculate multiplicative factor, FSEAFAC, to convert
        !           ! from the GBM VSHR to an appropriate marine VSHR
        !           !-----------------------------------------------------
        IF (N_av_ws > 0) THEN
          seawind = seawind / REAL(N_av_ws)
          !             ! Restrict FSEAFAC so FLANDFAC>FLANDMIN
          fseamax = MIN( 1.0 / flandmin,                                       &
                        (1.0 - flandmin * flandg(i,j)) / (1.0 - flandg(i,j)) )
          !             ! First limit is to keep fseamax sensible as FLANDG -> 1
          !             ! Second limit is to keep fland > flandmin, remembering
          !             !   that the we want FLANDG-weighted sum of factors =1
          !             !   to preserve the gridbox mean VSHR
          fseafac(i,j) = MAX(1.0,                                              &
                         MIN( fseamax, seawind / vshr(i,j) ))
        END IF

        vshr_ssi(i,j) = vshr(i,j) * fseafac(i,j)

        flandfac(i,j) = ( 1.0 - fseafac(i,j) * (1.0 - flandg(i,j)) )           &
                      / flandg(i,j)
        vshr_land(i,j) = vshr(i,j) * flandfac(i,j)


        vshr(i,j)= flandg(i,j) * vshr_land(i,j)                                &
          + (1.0 - flandg(i,j)) * vshr_ssi(i,j)
      END IF
    END DO
  END DO
!$OMP END PARALLEL DO
END IF  ! test on buddy_sea switch
#endif


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN
END SUBROUTINE jules_gridinit_sf_explicit
END MODULE jules_gridinit_sf_explicit_mod





