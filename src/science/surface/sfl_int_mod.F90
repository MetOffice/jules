! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!  SUBROUTINES SFL_INT-----------------------------------------------
!
!  Purpose: To calculate interpolation coefficients for 10m winds
!           and 1.5m temperature/specific humidity diagnostics.
!
!  External Documentation: UMDP No.24
!
!---------------------------------------------------------------------
MODULE sfl_int_mod

USE phi_m_h_mod, ONLY: phi_m_h

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SFL_INT_MOD'

CONTAINS

!    Arguments :-
SUBROUTINE sfl_int (                                                           &
 points,surft_pts,l_cdr10m_snow,surft_index,pts_index,fld_sea                  &
,vshr,cd_std,cd,ch,surft_frac                                                  &
,z0m,z0m_std,z0h                                                               &
,recip_l_mo,v_s,v_s_std                                                        &
,z1_uv,z1_tq,db                                                                &
,sf_diag                                                                       &
,cdr10m,cdr10m_n,cd10m_n,chr1p5m,chr10m                                        &
)

USE atm_fields_bounds_mod, ONLY: tdims, pdims_s
USE theta_field_sizes, ONLY: t_i_length

USE planet_constants_mod, ONLY: vkman

USE jules_surface_mod, ONLY:                                                   &
                eff_int,z_obs_tq, z_obs_wind, ip_scrndecpl1,                   &
                ip_scrndecpl2, ip_scrndecpl3
USE jules_surface_mod, ONLY:                                                   &
                iscrntdiag
USE sf_diags_mod, ONLY: strnewsfdiag
USE ereport_mod, ONLY: ereport

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
 points                                                                        &
                      ! IN Number of points.
,surft_pts
                      ! IN Number of tile points.

LOGICAL, INTENT(IN) ::                                                         &
 l_cdr10m_snow
                      ! Flag indicating if cdr10m is
                      ! to be calculated for use with snow unloading.

INTEGER, INTENT(IN) ::                                                         &
 surft_index(points)                                                           &
                      ! IN Index of tile points.
,pts_index(points)    ! IN Index of points.


REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 fld_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
!                           ! IN Fraction of land or sea
,z0m(points)                                                                   &
                      ! IN Roughness length for momentum (m).
,z0h(points)                                                                   &
                      ! IN Roughness length for heat and
!                         !    moisture (m).
,z0m_std(points)                                                               &
                      ! IN Roughness length for momentum without
!                         !    orographic component (m).
,vshr(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                      ! IN Wind speed difference between the
!                           !    surface and the lowest wind level in
!                           !    the atmosphere (m/s).
,cd(points)                                                                    &
                    ! IN Surface drag coefficient.
,ch(points)                                                                    &
                    ! IN Surface transfer coefficient for heat and
!                         !    moisture.
,cd_std(points)                                                                &
                    ! IN Surface drag coefficient excluding
!                         !    orographic from drag.
,surft_frac(points)                                                            &
!                         ! IN Tile fraction.
,recip_l_mo(points)                                                            &
!                        ! IN Reciprocal of the Monin-Obukhov length (m)
,v_s(points)                                                                   &
                    ! IN Surface layer scaling velocity including
!                         !    orographic form drag (m/s).
,v_s_std(points)                                                               &
!                         ! IN Surface layer scaling velocity excluding
!                         !    orographic form drag (m/s).
,z1_tq(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
!                         ! IN Height of lowest TQ level (m).
,z1_uv(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
!                         ! IN Height of lowest UV level (m).
,db(points)         ! IN Buoyancy difference between
!                         !    surface and lowest atmospheric
!                         !    level

!Diagnostics
TYPE (strnewsfdiag), INTENT(IN OUT) :: sf_diag

! Output variables

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
 cdr10m(pdims_s%i_start:pdims_s%i_end,                                         &
        pdims_s%j_start:pdims_s%j_end)                                         &
!                        ! INOUT interpolation coefficient for 10m wind
,cdr10m_n(pdims_s%i_start:pdims_s%i_end,                                       &
        pdims_s%j_start:pdims_s%j_end)                                         &
!                        ! INOUT interpolation coefficient for 10m
!                        !     neutral wind
,cd10m_n(pdims_s%i_start:pdims_s%i_end,                                        &
        pdims_s%j_start:pdims_s%j_end)                                         &
!                        ! INOUT neutral 10 m drag coefficient
,chr1p5m(points)   ! OUT Interpolation coefficient for 1.5m
!                        !     temperature
REAL(KIND=real_jlslsm), INTENT(OUT), OPTIONAL :: chr10m(points)
         ! OUT interpolation coefficient for 10m temperature

!  ---------------------------------------------------------------------
!  Define local storage.

!  (a) Local work arrays.

REAL(KIND=real_jlslsm) ::                                                      &
 z_wind(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                         ! Height of wind observations.
,z_temp(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                         ! Height of temperature and humidity
!                              ! observations.
,phi_m_obs(points)                                                             &
                      ! Monin-Obukhov stability function for
!                           ! momentum integrated to the wind observatio
!                           ! height.
,phi_h_obs(points)    ! Monin-Obukhov stability function for
!                           ! scalars integrated to their observation
!                           ! height.

!  (b) Scalars.

INTEGER ::                                                                     &
 i,j,k,l       ! Loop counter (horizontal field index).
REAL(KIND=real_jlslsm) ::                                                      &
   rib             ! Bulk Richardson number of lowest layer

! Error reporting

CHARACTER(LEN=256)       :: message
INTEGER                  :: errorstatus


INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SFL_INT'

!
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------
! 0.1 If diagnostics required and array is present,
!    calculate the M-O stability functions and interpolation
!    coefficients for 10m screen temperature and specific humidity.
!-----------------------------------------------------------------------
IF (PRESENT(chr10m) .AND. (sf_diag%l_t10m .OR. sf_diag%l_q10m) ) THEN

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i, j, k, l)                               &
!$OMP SHARED(tdims, z_temp, z_wind,points,phi_m_obs,phi_h_obs,chr10m,          &
!$OMP        surft_pts, surft_index, pts_index, t_i_length, z0h, z0m)

!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      z_wind(i,j)  = 0.0
      z_temp(i,j)  = 0.0
    END DO
  END DO
!$OMP END DO NOWAIT

!$OMP DO SCHEDULE(STATIC)
  DO i = 1, points
    phi_m_obs(i) = 0.0
    phi_h_obs(i) = 0.0
    chr10m(i)    = 0.0
  END DO
!$OMP END DO NOWAIT
  ! Need an OMP barrier here to ensure z_wind and z_temp are
  ! initialised before use.
!$OMP BARRIER

!$OMP DO SCHEDULE(STATIC)
  DO k = 1,surft_pts
    l = surft_index(k)
    j=(pts_index(l) - 1) / t_i_length + 1
    i = pts_index(l) - (j-1) * t_i_length
    z_wind(i,j) = z_obs_wind
    ! only difference from code below - using z_obs_wind (10m) instead of
    ! z_obs_tq (1.5m)
    z_temp(i,j) = z_obs_wind + z0h(l) - z0m(l)
  END DO
!$OMP END DO NOWAIT

!$OMP END PARALLEL

  CALL phi_m_h (points,surft_pts,surft_index,pts_index,                        &
                recip_l_mo,z_wind,z_temp,z0m,z0h,                              &
                phi_m_obs,phi_h_obs)

  ! use standard version, no diagnosis of decoupling
  ! these diagnostics are only produced over the sea, where we would
  ! expect no rapid cooling, hence decoupling is unlikely

!$OMP PARALLEL DO IF(surft_pts > 1) SCHEDULE(STATIC) DEFAULT(NONE)             &
!$OMP PRIVATE(k, l, j, i) SHARED(surft_pts, surft_index,pts_index,             &
!$OMP         t_i_length, chr10m, ch, vshr, phi_h_obs, v_s_std)
  DO k = 1,surft_pts
    l = surft_index(k)
    j=(pts_index(l) - 1) / t_i_length + 1
    i = pts_index(l) - (j-1) * t_i_length
    chr10m(l) = ch(l) * vshr(i,j) * phi_h_obs(l) / (vkman * v_s_std(l))
  END DO
!$OMP END PARALLEL DO
END IF

!-----------------------------------------------------------------------
! 1. If diagnostics required calculate M-O stability functions at
!    observation heights.
!-----------------------------------------------------------------------

! initialise work arrays
!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i, j)                                     &
!$OMP SHARED(tdims, z_temp, z_wind, points, phi_m_obs, phi_h_obs)
!$OMP DO SCHEDULE(STATIC)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    z_wind(i,j)  = 0.0
    z_temp(i,j)  = 0.0
  END DO
END DO
!$OMP END DO NOWAIT

!$OMP DO SCHEDULE(STATIC)
DO i = 1, points
  phi_m_obs(i) = 0.0
  phi_h_obs(i) = 0.0
END DO
!$OMP END DO NOWAIT
!$OMP END PARALLEL

! If using transitional decoupling (IP_ScrnDecpl2), a calculation
! must be performed every timestep, even if the diagnostic is not
! required on that particular timestep.
IF (sf_diag%su10 .OR. sf_diag%sv10 .OR. sf_diag%st1p5 .OR.                     &
    sf_diag%sq1p5 .OR. l_cdr10m_snow .OR.                                      &
    (IScrnTDiag == IP_ScrnDecpl2) .OR.                                         &
    (IScrnTDiag == IP_ScrnDecpl3)) THEN
!$OMP PARALLEL DO IF(surft_pts > 1) SCHEDULE(STATIC) DEFAULT(NONE)             &
!$OMP PRIVATE(k, l, j, i) SHARED(surft_pts, surft_index, pts_index,            &
!$OMP         t_i_length, z_wind, z_temp, z0h, z0m)
  DO k = 1,surft_pts
    l = surft_index(k)
    j=(pts_index(l) - 1) / t_i_length + 1
    i = pts_index(l) - (j-1) * t_i_length
    z_wind(i,j) = z_obs_wind
    z_temp(i,j) = z_obs_tq + z0h(l) - z0m(l)
  END DO
!$OMP END PARALLEL DO
  CALL phi_m_h (points,surft_pts,surft_index,pts_index,                        &
                recip_l_mo,z_wind,z_temp,z0m,z0h,                              &
                phi_m_obs,phi_h_obs)
END IF

!-----------------------------------------------------------------------
! 2. If diagnostics required calculate interpolation coefficient
!    for 1.5m screen temperature and specific humidity.
!-----------------------------------------------------------------------

IF (sf_diag%st1p5 .OR. sf_diag%sq1p5                                           &
    .OR. (IScrnTDiag == IP_ScrnDecpl2)                                         &
    .OR. (IScrnTDiag == IP_ScrnDecpl3) ) THEN

  !       Calculate the screen temperature allowing for decoupling or
  !       using pure surface similarity theory as the default. Seperate
  !       blocks of code are used for efficiency.

  IF (iscrntdiag == ip_scrndecpl1) THEN

!$OMP PARALLEL DO IF(surft_pts > 1) SCHEDULE(STATIC) DEFAULT(NONE)             &
!$OMP PRIVATE(k, l, j, i, rib) SHARED(surft_pts, surft_index, pts_index,       &
!$OMP         t_i_length, z1_uv, db, z1_tq, vshr, chr1p5m, ch, phi_h_obs,      &
!$OMP         v_s_std)
    DO k = 1,surft_pts
      l = surft_index(k)
      j=(pts_index(l) - 1) / t_i_length + 1
      i = pts_index(l) - (j-1) * t_i_length
      rib = ( z1_uv(i,j) * z1_uv(i,j) * db(l) ) /                              &
            ( z1_tq(i,j) * vshr(i,j) * vshr(i,j) )
      IF (rib> 0.25) THEN
        !             Allow for decoupling in very stable conditions
        !             based on the quasi-equilibrium radiative solution.
        !             Note: This value is set for a screen level of 1.5m
        !             and has been fitted for the bottomlevel lying between
        !             1.5 and 20m. It should be recalibrated for coarser
        !             resolutions.
        chr1p5m(l) = 0.335 + 1.78 / z1_tq(i,j) - 1.19 / z1_tq(i,j)**2
      ELSE
        !             Use pure surface similarity theory
        chr1p5m(l) = ch(l) * vshr(i,j) *                                       &
                      phi_h_obs(l) / (vkman * v_s_std(l))
      END IF
    END DO
!$OMP END PARALLEL DO
  ELSE

!$OMP PARALLEL DO IF(surft_pts > 1) SCHEDULE(STATIC) DEFAULT(NONE)             &
!$OMP PRIVATE(k, l, j, i) SHARED(surft_pts, surft_index, pts_index,            &
!$OMP         t_i_length, chr1p5m, ch, vshr, phi_h_obs, v_s_std)
    DO k = 1,surft_pts
      l = surft_index(k)
      j=(pts_index(l) - 1) / t_i_length + 1
      i = pts_index(l) - (j-1) * t_i_length
      chr1p5m(l) = ch(l) * vshr(i,j) *                                         &
                    phi_h_obs(l) / (vkman * v_s_std(l))

    END DO
!$OMP END PARALLEL DO
  END IF

END IF

!-----------------------------------------------------------------------
! 3. If diagnostics required calculate interpolation coefficient
!    for 10m winds.
!-----------------------------------------------------------------------

IF ( sf_diag%su10 .OR. sf_diag%sv10 .OR. l_cdr10m_snow ) THEN
  IF ( eff_int ) THEN
    DO k = 1,surft_pts
      l = surft_index(k)
      j=(pts_index(l) - 1) / t_i_length + 1
      i = pts_index(l) - (j-1) * t_i_length
      cdr10m(i,j) = cdr10m(i,j) + fld_sea(i,j) * surft_frac(l) *               &
                cd(l) * vshr(i,j) * phi_m_obs(l) / (vkman * v_s(l))
    END DO
  ELSE

!$OMP PARALLEL DO IF(surft_pts > 1) SCHEDULE(STATIC) DEFAULT(NONE)             &
!$OMP PRIVATE(k, l, j, i) SHARED(surft_pts, surft_index, pts_index,            &
!$OMP         t_i_length, z_temp, z0h, z0m_std)
    DO k = 1,surft_pts
      l = surft_index(k)
      j=(pts_index(l) - 1) / t_i_length + 1
      i = pts_index(l) - (j-1) * t_i_length
      z_temp(i,j) = z_obs_tq + z0h(l) - z0m_std(l)
    END DO
!$OMP END PARALLEL DO

    CALL phi_m_h (points,surft_pts,surft_index,pts_index,                      &
                  recip_l_mo,z_wind,z_temp,z0m_std,z0h,                        &
                  phi_m_obs,phi_h_obs)

!$OMP PARALLEL DO IF(surft_pts > 1) SCHEDULE(STATIC) DEFAULT(NONE)             &
!$OMP PRIVATE(k, l, j, i) SHARED(surft_pts, surft_index, pts_index,            &
!$OMP         t_i_length, cdr10m, fld_sea, surft_frac, cd_std, vshr,           &
!$OMP         phi_m_obs, v_s_std)
    DO k = 1,surft_pts
      l = surft_index(k)
      j=(pts_index(l) - 1) / t_i_length + 1
      i = pts_index(l) - (j-1) * t_i_length
      cdr10m(i,j) = cdr10m(i,j) + fld_sea(i,j) * surft_frac(l) *               &
                    cd_std(l) * vshr(i,j) * phi_m_obs(l) /                     &
                       (vkman * v_s_std(l))
    END DO
!$OMP END PARALLEL DO
  END IF  !  eff_int
END IF


IF ( sf_diag%suv10m_n ) THEN
  IF (eff_int) THEN
    errorstatus = 201
    message = "Neutral wind diagnostics cannot be produced " //                &
              "when eff_int is TRUE."
    CALL ereport("sfl_int", errorstatus, message)
  END IF
!$OMP PARALLEL DO IF(surft_pts > 1) SCHEDULE(STATIC) DEFAULT(NONE)             &
!$OMP PRIVATE(k, l, j, i) SHARED(surft_pts, surft_index, pts_index,            &
!$OMP         t_i_length, cdr10m_n, fld_sea, surft_frac, cd_std, vshr,         &
!$OMP         z_wind, z0m_std, v_s_std, cd10m_n)
  DO k = 1,surft_pts
    l = surft_index(k)
    j=(pts_index(l) - 1) / t_i_length + 1
    i = pts_index(l) - (j-1) * t_i_length
    cdr10m_n(i,j) = cdr10m_n(i,j) + fld_sea(i,j) * surft_frac(l) *             &
                cd_std(l) * vshr(i,j) *                                        &
                  LOG( 1.0 + z_wind(i,j) / z0m_std(l) ) /                      &
                     (vkman * v_s_std(l))
    cd10m_n(i,j)  = cd10m_n(i,j) + fld_sea(i,j) * surft_frac(l) *              &
                (vkman / LOG( 1.0 + z_wind(i,j) / z0m_std(l) ) )**2
  END DO
!$OMP END PARALLEL DO
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE sfl_int

END MODULE sfl_int_mod
