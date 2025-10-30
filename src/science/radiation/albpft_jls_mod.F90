! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Routine to calculate the spectral albedo of the land surface and the profile
! of PAR through the canopy used by some canopy radiation models using
! the two stream approach of Sellers, 1995. It is called both to calculate the
! albedo if l_spec_albedo is true and to calculate the absorption of sunlight
! for photosynthesis.
! *********************************************************************
MODULE albpft_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='ALBPFT_MOD'
CONTAINS
SUBROUTINE albpft(                                                             &
  !INTENT(IN)
    l_getprofile, land_pts, ilayers, albpft_call,                              &
    surft_pts, surft_index,                                                    &
    cosz_gb, lai_physical, albudir, albudif,                                   &
  !INTENT(INOUT)
    alb_type,                                                                  &
  !INTENT(OUT)
    fapar_dir, fapar_dif, fapar_dir2dif,                                       &
    fapar_dif2dif, fapar_dir2dir, fsun,                                        &
    !New arguments replacing USE statements
    !jules_mod (IN OUT)
    albobs_scaling_surft)

USE jules_surface_types_mod, ONLY: npft, ntype
USE pftparm, ONLY: omega,  omnir,  alpar,  alnir,                              &
                   omegal, omnirl, alparl, alnirl,                             &
                   omegau, omniru, alparu, alniru,                             &
                   can_struct_a, orient

USE stochastic_physics_run_mod, ONLY: l_rp2, i_rp_scheme, i_rp2b,              &
                                      alnir_rp, alpar_rp, omega_rp, omnir_rp

USE jules_radiation_mod, ONLY: l_spec_albedo, l_albedo_obs, l_niso_direct

USE jules_vegetation_mod, ONLY: can_rad_mod

USE ancil_info, ONLY: rad_nband

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE ereport_mod, ONLY: ereport

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

! Subroutine arguments
!   Scalar arguments with intent(in):
INTEGER, INTENT(IN) ::                                                         &
 land_pts                 &! Number of land points.
,ilayers                    &! Number of layers over which
                             ! the PAR absorption profile is to
                             ! be calculated.
,albpft_call                 ! 0 - No scaling to obs
                             ! 1 - receive an input scaling to obs
                             ! and apply a correction to it for the
                             ! non-linearity (and store that scaling)
                             ! 2 - use the stored scaling to obs

LOGICAL, INTENT(IN) :: l_getprofile
                             ! TRUE means profiles through the
!                                    canopy are calculated
!                                  ! FALSE means no profiles calculated

!   Array arguments with intent(in):
INTEGER, INTENT(IN) ::                                                         &
 surft_pts(ntype)                                                              &
                             ! Number of land points which
!                                  ! include the surface type.
,surft_index(land_pts,ntype)
                             ! Indices of land points which
!                                  ! include the surface type.
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 albudir(land_pts,2,npft)                                                      &
                             ! Direct albedo of the underlying
!                            ! surface.
,albudif(land_pts,2,npft)                                                      &
                             ! Diffuse albedo of the underlying
!                            ! surface.
,cosz_gb(land_pts)                                                             &
                             ! Cosine of the zenith angle.
,lai_physical(land_pts,npft) ! Leaf area index.

!   Array arguments with intent(out):
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
fapar_dir(land_pts,npft,ilayers)                                               &
!                                  ! Profile of absorbed PAR
!                                  !     - Direct (fraction/LAI)
,fapar_dif(land_pts,npft,ilayers)                                              &
!                                  ! Profile of absorbed PAR
!                                  !     - Diffuse (fraction/LAI)
,fapar_dir2dif(land_pts,npft,ilayers)                                          &
!                                  ! Profile of scattered PAR
!                                  !   -direct to diffuse
!                                  !      (fraction/LAI)
,fapar_dif2dif(land_pts,npft,ilayers)                                          &
!                                  ! Profile of absorbed PAR
!                                  !     - Diffuse (fraction/LAI)
,fapar_dir2dir(land_pts,npft,ilayers)                                          &
!                                  ! Profile of absorbed only direct PAR
!                                  !     -  (fraction/LAI)
,fsun(land_pts,npft,ilayers)
                             ! fraction of sunlit leaves

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
 alb_type(land_pts,ntype,4)
!                                  ! Albedos for surface types.
!                                  !   (*,*,1) - Direct beam visible
!                                  !   (*,*,2) - Diffuse visible
!                                  !   (*,*,3) - Direct beam near-IR
!                                  !   (*,*,4) - Diffuse near-IR

!New arguments replacing USE statements
!jules_mod (IN OUT)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: albobs_scaling_surft(land_pts,ntype,rad_nband)

! Local arrays:
REAL(KIND=real_jlslsm) ::                                                      &
 alpl                                                                          &
                            ! Leaf reflection coeff
,om                                                                            &
                            ! Leaf scattering coeff
,rnet_dir(0:ilayers)                                                           &
!                                 ! Net downward PAR at layer
!                                 ! boundaries - incident direct
,rnet_dif(0:ilayers)                                                           &
!                                 ! Net downward PAR at layer
!                                 ! boundaries - incident diffuse
,taul                                                                          &
                                  ! Leaf transmission coefficient.
,lai(land_pts,npft)
                                  ! effective leaf area index
! Local scalars:
REAL(KIND=real_jlslsm) ::                                                      &
 betadir                                                                       &
                            ! Upscatter parameter for direct beam.
,betadif                                                                       &
                            ! Upscatter parameter for diffuse beam
,coszm                                                                         &
                            ! Mean value of COSZ.
,k                                                                             &
                            ! Optical depth per unit leaf area.
,g                                                                             &
                            ! Relative projected leaf area in
                            ! direction cosz.
,salb                                                                          &
                            ! Single scattering albedo.
,sqcost                                                                        &
                            ! Cosine squared of the mean leaf
                            ! angle to the horizontal.
,albdif_tmp                                                                    &
                            ! Temporary diffuse albedo
,albscl_tmp                                                                    &
                            ! Temporary scaling value
,albscl_new                                                                    &
                            ! Temporary scaling value
,b,c,ca,d,f,h,u1                                                               &
                            ! Miscellaneous variables from
,p1,p2,p3,p4,d1                                                                &
                            ! Sellers (1985).
,h1,h2,h3,h7,h8                                                                &
                            !
,s1,s2,sig                  !

!-----------------------------------------------------------------------
! Additional work variables to calculate profiles of absorbed PAR
!-----------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
 dlai                                                                          &
                                ! LAI increment.
,la                                                                            &
                                ! Cumulative LAI through canopy.
,drdird_dlai,drdiru_dlai                                                       &
,drdifd_dlai,drdifu_dlai                                                       &
,u2,u3,d2,h4,h5,h6,h9,h10                                                      &
                                ! Rate of change of fluxes with LAI
                                ! (W/m2/LAI).
,rdiru,rdird,rdifu,rdifd                                                       &
                                ! work variable
,dabeer_dla(0:ilayers)          ! Attenuation of direct solar
                                ! beam per unit LAI

!Optimisation variables to reduce the number of EXP evaluations
REAL(KIND=real_jlslsm) :: exp_hla, exp_minus_hla, exp_minus_kla,               &
        exp_k_dlai_min_la

!Optimisation variables to reduce the number of IF evaluations
REAL(KIND=real_jlslsm) :: omega_arr(2),  alpa_arr(2),                          &
        omegal_arr(2), alpal_arr(2),                                           &
        omegau_arr(2), alpau_arr(2)

INTEGER ::                                                                     &
 band,i,j,l,n               ! Loop counters.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='ALBPFT'

INTEGER ::              errcode            ! Error code
CHARACTER(LEN=80) :: ERRMSG             ! Error message text

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Check the consistency of the options.
IF ( albpft_call < 0 .OR. albpft_call > 2) THEN
  errcode = 1
  WRITE(ERRMSG,'(A,I0)') 'albpft_call must be 0, 1 or 2. '                     &
      // 'Called with albpft_call = ', albpft_call
  CALL ereport (routinename,errcode,ERRMSG)
END IF
IF ( albpft_call > 0 ) THEN
  IF ( .NOT. (l_albedo_obs .AND. l_spec_albedo) ) THEN
    errcode = 1
    ERRMSG  = 'invalid call to albpft_jls, needs l_albedo_obs and l_spec_albedo'
    CALL ereport ('albfpt',errcode,ERRMSG)
  END IF
END IF

IF ( l_rp2 .AND. i_rp_scheme == i_rp2b ) THEN
  DO n = 1,npft
    alnir(n) = alnir_rp(n)
    alpar(n) = alpar_rp(n)
    omega(n) = omega_rp(n)
    omnir(n) = omnir_rp(n)
  END DO
END IF

!$OMP PARALLEL DEFAULT(NONE)                                                   &
!$OMP PRIVATE(n, l, i, j, albdif_tmp, albscl_new, albscl_tmp, b, betadif,      &
!$OMP         betadir, c, ca, coszm, d, d1, d2, dabeer_dla, dlai,              &
!$OMP         drdird_dlai, drdiru_dlai, drdifu_dlai, drdifd_dlai, f, g, h,     &
!$OMP         h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, k, la, p1, p2,          &
!$OMP         p3, p4, rdifd, rdifu, rdird, rdiru, rnet_dif, rnet_dir, s1,      &
!$OMP         s2, salb, sig, sqcost, taul, u1, u2, u3, exp_hla,                &
!$OMP         exp_minus_hla, exp_minus_kla, om, alpl, band, omega_arr,         &
!$OMP         alpa_arr, omegal_arr, alpal_arr, omegau_arr,alpau_arr,           &
!$OMP         exp_k_dlai_min_la)                                               &
!$OMP SHARED(npft,omega,omnir,alpar,alnir,omegal,omnirl,alparl,alnirl,         &
!$OMP        omegau,omniru,alparu,alniru,lai_physical,surft_pts,               &
!$OMP        surft_index, albpft_call, land_pts,                               &
!$OMP        albobs_scaling_surft,  orient, l_getprofile,                      &
!$OMP        alb_type, cosz_gb, lai, albudif, ilayers, fapar_dir,              &
!$OMP        fapar_dif, fapar_dir2dir, fapar_dir2dif, fapar_dif2dif,           &
!$OMP        albudir, can_rad_mod, can_struct_a, fsun, l_niso_direct,          &
!$OMP        l_albedo_obs)

! Initialisations
!$OMP DO SCHEDULE(STATIC) COLLAPSE(2)
DO n = 1,npft
  DO l = 1,land_pts
    lai(l,n) = 0.0
  END DO
END DO
!$OMP END DO NOWAIT

! Initialise to zero at top.
!$OMP DO SCHEDULE(STATIC) COLLAPSE(3)
DO i = 1,ilayers
  DO n = 1,npft
    DO l = 1, land_pts
      fapar_dir2dir(l,n,i) = 0.0
      fapar_dir2dif(l,n,i) = 0.0
      fapar_dif2dif(l,n,i) = 0.0
      fsun(l,n,i)          = 0.0
    END DO
  END DO
END DO
!$OMP END DO NOWAIT

! We need an explicit OMP barrier to ensure that the arrays are fully
! initialised before use.
!$OMP BARRIER

DO n = 1,npft

  !Package up variables to fit in with looping over bands. They come as pairs:
  !omega, omnir:   Leaf scattering coefficient for PAR.
  omega_arr(1)  = omega(n)
  omega_arr(2)  = omnir(n)
  !alpar, alnir Leaf reflection coefficient for PAR.
  alpa_arr(1)   = alpar(n)
  alpa_arr(2)   = alnir(n)
  ! IF l_albedo_obs = F then the following will be set to missing data
  IF ( l_albedo_obs ) THEN
    !omegal, omnirl: lower limit on omega, when scaled to albedo obs
    omegal_arr(1) = omegal(n)
    omegal_arr(2) = omnirl(n)
    !alparl, alnirl: lower limit on alpar, when scaled to albedo obs
    alpal_arr(1)  = alparl(n)
    alpal_arr(2)  = alnirl(n)
    !omegau, omniru: upper limit on omega, when scaled to albedo obs
    omegau_arr(1) = omegau(n)
    omegau_arr(2) = omniru(n)
    !alparu, alniru: upper limit on alpar, when scaled to albedo obs
    alpau_arr(1)  = alparu(n)
    alpau_arr(2)  = alniru(n)
  END IF

!$OMP DO SCHEDULE(STATIC)
  DO j = 1,surft_pts(n)
    l = surft_index(j,n)
    lai(l,n) = lai_physical(l,n) * can_struct_a(n)
  END DO
!$OMP END DO NOWAIT

  DO band = 1,2  ! Visible and near-IR bands

    ! If albpft_call is 0, then either there's no scaling to albedo obs,
    ! or this is the first call in that process to make a first guess:
    IF (albpft_call == 0 ) THEN
      om   = omega_arr(band)
      alpl = alpa_arr(band)
    END IF

!$OMP DO SCHEDULE(STATIC)
    DO j = 1,surft_pts(n)
      l = surft_index(j,n)

      IF ( albpft_call == 1 ) THEN

        !Firstly apply the scaling for this point to the inputs:
        om   = omega_arr(band) * albobs_scaling_surft(l,n,band)
        alpl = alpa_arr(band)  * albobs_scaling_surft(l,n,band)

        ! Test to see if the scaling puts us in totally unphysical part of
        !phase-space and set to the upper/lower values if it is:
        IF (om <= 0.0 .OR. alpl <= 0.0 ) THEN
          om   = omegal_arr(band)
          alpl = alpal_arr(band)

          ! Don't do anything with the scaling, leave it to make unphysical values
          ! so the later call will just use the upper/lower range values
        ELSE IF (om >= 1.0 .OR. alpl >= 1.0 ) THEN
          om   = omegau_arr(band)
          alpl = alpau_arr(band)
        ELSE
          ! Now work out a temporary diffuse albedo for the band
          !(a predictor):
          taul = om - alpl
          IF (orient(n) == 0) THEN
            sqcost = 1.0 / 3.0
            coszm  = 1.0
          ELSE IF (orient(n) == 1) THEN
            sqcost = 1.0
            coszm  = 1.0
          END IF

          c           = 0.5 * ( alpl + taul + (alpl - taul) * sqcost )
          betadif     = c / om
          b           = 1.0 - (1.0 - betadif) * om
          h           = SQRT(b * b - c * c) / coszm
          u1          = b - c / albudif(l,band,n)
          s1          = EXP(-h * lai(l,n))
          p1          = b + coszm * h
          p2          = b - coszm * h
          d1          = p1 * (u1 - coszm * h) / s1 - p2 * (u1 + coszm * h) * s1
          h7          =  (c / d1) * (u1 - coszm * h) / s1
          h8          = -(c / d1) * (u1 + coszm * h) * s1
          albdif_tmp  = h7 + h8

          ! compare it to the albedo from the previous first guess call:
          albscl_tmp = albdif_tmp / alb_type(l,n,2 * band)
          ! and apply a corrector to the scaling factor to account for
          !non-linearity:
          albscl_new = albobs_scaling_surft(l,n,band)                          &
                       * (albobs_scaling_surft(l,n,band) / albscl_tmp)
          albobs_scaling_surft(l,n,band) = albscl_new

          om   = omega_arr(band) * albscl_new
          alpl = alpa_arr(band)  * albscl_new

        END IF ! ends test of totally unphysical 'predictor' scaled constants.

      ELSE IF ( albpft_call == 2 ) THEN
        ! Using pre-corrected scalings, so just apply scaling:
        om   = omega_arr(band) * albobs_scaling_surft(l,n,band)
        alpl = alpa_arr(band)  * albobs_scaling_surft(l,n,band)
      END IF

      ! If some scaling has been applied, check to see if any of the values are
      ! unphyical, then check the values are in the lower to upper value range:
      ! (consistent with the unphsyical values checks above)
      IF (albpft_call > 0) THEN
        IF (om <= 0.0 .OR. alpl <= 0.0 ) THEN
          om   = omegal_arr(band)
          alpl = alpal_arr(band)
        ELSE IF (om >= 1.0 .OR. alpl >= 1.0 ) THEN
          om   = omegau_arr(band)
          alpl = alpau_arr(band)
        ELSE
          om   = MIN( MAX(om, omegal_arr(band)), omegau_arr(band) )
          alpl = MIN( MAX(alpl, alpal_arr(band)), alpau_arr(band) )
        END IF
      END IF

      ! Now continue with rest of the subroutine with the om and alpl:
      taul = om - alpl

      IF (orient(n) == 0) THEN
        sqcost = 1.0 / 3.0
        g      = 0.5
        coszm  = 1.0
        k      = g / 0.01
        salb   = 0.5 * om

        IF (cosz_gb(l) > 0.01) THEN
          k = g / cosz_gb(l)

          IF (l_niso_direct) THEN
            !Use the full non-isotropic expression for the albedo Asymmetry
            salb = (4.0 / 9.0) * (om-2.0 * alpl) / om
            !Albedo
            salb = 0.5 * om                                                    &
                   * ((1.0+3.0 * salb *  cosz_gb(l)**2)                        &
                      * (1.0 - cosz_gb(l)                                      &
                         * LOG((cosz_gb(l) + 1.0) / cosz_gb(l)))               &
                      - 1.5 * salb * cosz_gb(l) )
          ELSE
            !Original form directly from Table 2 of Sellers (1985);
            !this is valid only for isotropic scattering.
            salb = 0.5 * om *                                                  &
                   (1.0 - cosz_gb(l) * LOG((cosz_gb(l) + 1.0) / cosz_gb(l)))
          END IF
        END IF

      ELSE IF (orient(n) == 1) THEN
        sqcost = 1.0
        coszm  = 1.0
        !K = G / COSZ(I) = 1.0 because G = COSZ(I) for horizontal leaves
        k = 1.0
        IF (l_niso_direct) THEN
          !Full non-isotropic form.
          salb = alpl * 0.5
        ELSE
          !Original form directly from Table 2 of Sellers (1985);
          !this is valid only for isotropic scattering.
          salb = om * 0.25
        END IF
      END IF

      betadir = (1.0 + coszm * k) / (om * coszm * k) * salb
      c       = 0.5 * ( alpl + taul + (alpl - taul) * sqcost )
      betadif = c / om
      b       = 1.0 - (1.0 - betadif) * om
      d       = om * coszm * k * betadir
      f       = om * coszm * k * (1.0 - betadir)
      h       = SQRT(b * b - c * c) / coszm
      sig     = (coszm * k)**2 + c * c - b * b

      IF ( ABS(sig) < 1.0e-4 ) THEN
        sig = SIGN( 1.0e-4,sig )
      END IF

      u1      = b - c / albudif(l,band,n)
      ca      = c * albudir(l,band,n) / albudif(l,band,n)
      s1      = EXP(-h * lai(l,n))
      s2      = EXP(-k * lai(l,n))
      p1      = b + coszm * h
      p2      = b - coszm * h
      p3      = b + coszm * k
      p4      = b - coszm * k
      d1      = p1 * (u1 - coszm * h) / s1 - p2 * (u1 + coszm * h) * s1
      h1      = -d * p4 - c * f
      h2      =  ((d - p3 * h1 / sig) * (u1 - coszm *  h ) / s1                &
                  - (d - ca - (u1 + coszm * k) * h1 / sig) * p2 * s2) / d1
      h3      = -((d - p3 * h1 / sig) * (u1 + coszm * h) * s1                  &
                  - (d - ca - (u1 + coszm * k) * h1 / sig) * p1 * s2) / d1
      h7      =  (c / d1) * (u1 - coszm * h) / s1
      h8      = -(c / d1) * (u1 + coszm * h) * s1

      alb_type(l,n,2 * band-1) = h1 / sig + h2 + h3   ! Direct beam
      alb_type(l,n,2 * band)   = h7 + h8              ! Diffuse

      !-----------------------------------------------------------------------
      ! If required calculate the profile of absorbed PAR through the canopy
      ! of direct and diffuse beams (BEWARE: assumes PAR is band 1)
      !-----------------------------------------------------------------------
      IF ( l_getprofile .AND. band == 1 ) THEN

        u2  =  b - c * albudif(l,band,n)
        u3  =  f + c * albudif(l,band,n)
        d2  =  (u2 + coszm * h) / s1 - (u2 - coszm * h) * s1
        h4  = -f * p3 - c * d
        h5  = -1.0 / d2 * (h4 / sig * (u2 + coszm * h)                         &
               / s1 + (u3 - h4 / sig * (u2 - coszm * k)) * s2)
        h6  =  1.0 / d2 * (h4 / sig * (u2 - coszm * h)                         &
               * s1 + (u3 - h4 / sig * (u2 - coszm * k)) * s2)
        h9  =  1.0 / d2 * (u2 + coszm * h) / s1
        h10 = -1.0 / d2 * (u2 - coszm * h) * s1
        !-----------------------------------------------------------------------
        ! Two-stream equations for direct and diffuse upward and downward beams:
        !             RDIRU(I)=H1/SIG*EXP(-K*LA)+H2*EXP(-H*LA)+H3*EXP(H*LA)
        !             RDIRD(I)=(H4/SIG+1)*EXP(-K*LA)+H5*EXP(-H*LA)+H6*EXP(H*LA)
        !             RDIFU(I)=H7*EXP(-H*LA)+H8*EXP(H*LA)
        !             RDIFD(I)=H9*EXP(-H*LA)+H10*EXP(H*LA)
        ! Note: The original equations in Sellers (1985) include just the
        ! diffuse radiation scattered from the direct beam. The total downward
        ! flux is represented by RDIRD, with the extra 1 added to h4/sig to
        ! represent direct solar radiation.
        !-----------------------------------------------------------------------
        dlai = lai(l,n) / REAL(ilayers)

        IF ( can_rad_mod /= 5 .AND. can_rad_mod /= 6 ) THEN
          !-----------------------------------------------------------------------
          ! Differentiate these equations to calculate PAR absorption per unit
          ! LAI down through the canopy. Centre derivatives in the centre of each
          ! LAI layer.
          !
          ! With these options fapar_dir represents the total PAR at each level
          ! normalized by the incident PAR in the case when the incident
          ! flux is entirely direct. fapar_dif is the normalized PAR at
          ! each level when the incident flux is entirely diffuse.
          !-----------------------------------------------------------------------
          la = 0.5 * dlai
          DO i = 1,ilayers

            !This is a more efficient version but will cause a change in KGO
            exp_hla       = EXP(h * la)
            exp_minus_hla = EXP(-h * la)  !We could 1/exp_hla too
            exp_minus_kla = EXP(-k * la)

            drdiru_dlai   = -k *  h1 / sig    * exp_minus_kla                  &
                             - h * h2 * exp_minus_hla + h * h3 * exp_hla
            drdird_dlai   = -k * (h4 / sig+1) * exp_minus_kla                  &
                             - h * h5 * exp_minus_hla + h * h6 * exp_hla
            drdifu_dlai   = -h * h7 * exp_minus_hla + h * h8  * exp_hla
            drdifd_dlai   = -h * h9 * exp_minus_hla + h * h10 * exp_hla

            fapar_dir(l,n,i) = (-drdird_dlai + drdiru_dlai) * can_struct_a(n)
            fapar_dif(l,n,i) = (-drdifd_dlai + drdifu_dlai) * can_struct_a(n)

            la = la + dlai
          END DO  !layers

        ELSE

          !-----------------------------------------------------------------------
          !               can_rad_mod = 5 or 6.
          !               Don't use derivatives.
          !-----------------------------------------------------------------------

          la = dlai
          rnet_dir(0) = (h4 - h1) / sig + (h5 - h2) + (h6 - h3)
          rnet_dif(0) = (h9 - h7) + (h10 - h8)

          DO i = 1,ilayers

            exp_hla       = EXP(h * la)
            exp_minus_hla = EXP(-h * la)  !We could 1/exp_hla too
            exp_minus_kla = EXP(-k * la)
            exp_k_dlai_min_la = EXP(k * (dlai - la))

            rdiru = h1 / sig * exp_minus_kla + h2 * exp_minus_hla + h3 * exp_hla
            rdird = h4 / sig * exp_minus_kla + h5 * exp_minus_hla + h6 * exp_hla
            rdifu = h7 * exp_minus_hla + h8 * exp_hla
            rdifd = h9 * exp_minus_hla + h10 * exp_hla

            rnet_dir(i) = rdird - rdiru
            rnet_dif(i) = rdifd - rdifu

            fapar_dir(l,n,i) = (rnet_dir(i-1) - rnet_dir(i)) / dlai
            fapar_dif(l,n,i) = (rnet_dif(i-1) - rnet_dif(i)) / dlai

            dabeer_dla(i) = (exp_k_dlai_min_la - exp_minus_kla) / dlai

            fapar_dir2dir(l,n,i) = (1.0 - om) * dabeer_dla(i)
            fapar_dir2dif(l,n,i) =  om * dabeer_dla(i) + fapar_dir(l,n,i)
            fapar_dif2dif(l,n,i) = fapar_dif(l,n,i)

            fsun(l,n,i) = (exp_k_dlai_min_la - exp_minus_kla) / (k * dlai)

            fapar_dir(l,n,i)     = fapar_dir(l,n,i) * can_struct_a(n)
            fapar_dif(l,n,i)     = fapar_dif(l,n,i) * can_struct_a(n)
            fapar_dir2dir(l,n,i) = fapar_dir2dir(l,n,i) * can_struct_a(n)
            fapar_dir2dif(l,n,i) = fapar_dir2dif(l,n,i) * can_struct_a(n)
            fapar_dif2dif(l,n,i) = fapar_dif2dif(l,n,i) * can_struct_a(n)

            la = la + dlai
          END DO  !layers

        END IF  !  can_rad_mod

      END IF  !  abs. par (band=1+getProfile)

    END DO  !  points
!$OMP END DO NOWAIT

  END DO  !  bands

END DO  !  pfts

!$OMP END PARALLEL

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE albpft
END MODULE albpft_mod
