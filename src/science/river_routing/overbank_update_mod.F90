!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE overbank_update_mod

CONTAINS

SUBROUTINE overbank_update(rfm_rivflow_rp)

!-----------------------------------------------------------------------------
!
! Description:
!
!   Calculate river inundation for diagnostic models of inundation.
!
!  Code Owner: Please refer to ModuleLeaders.txt
!  This file belongs in section: Hydrology
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE timestep_mod, ONLY: timestep

USE jules_rivers_mod, ONLY:                                                    &
   np_rivers, nstep_rivers, rivers_length

USE overbank_inundation_mod, ONLY:                                             &
   !  imported scalar parameters
   overbank_hypsometric, overbank_quantiles, overbank_simple,                  &
   overbank_simple_rosgen,                                                     &
   !  imported scalars with intent(in)
   overbank_model, riv_a, riv_b, riv_c, riv_f, ent_ratio,                      &
   !  imported arrays with intent(in)
   logn_mean_rp, logn_stdev_rp, qbf, dbf, wbf,                                 &
   !  imported arrays with intent(out)
   frac_fplain_rp

!-----------------------------------------------------------------------------

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

! internal variables
REAL(KIND=real_jlslsm) ::                                                      &
   riv_width(np_rivers),                                                       &
       ! river width (m)
   riv_depth(np_rivers)
       ! river depth (m)

REAL(KIND=real_jlslsm) :: dt
       ! River timestep length (s).

!Values used in the ERF() call
REAL(KIND=real_jlslsm), PARAMETER :: erf_int_half = 0.5
REAL(KIND=real_jlslsm), PARAMETER :: erf_int_sqrt2 = 1.41421356

INTEGER :: ip

! Rivers arrays
REAL, INTENT(IN OUT) :: rfm_rivflow_rp(:)

!------------------------------------------------------------------------------
! This routine only applies to diagnostic models of overbank inundation.
!------------------------------------------------------------------------------
SELECT CASE ( overbank_model )
CASE ( overbank_quantiles )
  ! This is not a diagnostic model. Nothing more to do here.
  RETURN
END SELECT

!------------------------------------------------------------------------------
! Initialisation
!------------------------------------------------------------------------------

! Calculate rivers model timestep length.
dt = REAL(nstep_rivers) * timestep

!-----------------------------------------------------------------------------
! Calculate river width and depth, based on flow speed
!-----------------------------------------------------------------------------

DO ip = 1,np_rivers

  ! Initialisation.
  riv_depth(ip) = 0.0
  frac_fplain_rp(ip) = 0.0

  ! Remove any negative flow value.
  IF ( rfm_rivflow_rp(ip) < 0.0 ) rfm_rivflow_rp(ip) = 0.0

  ! Calculate river flow depth, if required.
  IF ( overbank_model == overbank_simple_rosgen .OR.                           &
       overbank_model == overbank_hypsometric ) THEN
    ! Leopold & Maddock (1953: eqn2)
    riv_depth(ip) = riv_c * (rfm_rivflow_rp(ip)** riv_f)
  END IF

  !---------------------------------------------------------------------------
  ! Calculate new overbank inundation
  !---------------------------------------------------------------------------

  SELECT CASE ( overbank_model )
  CASE ( overbank_hypsometric )
    ! Calculate inundated area using a hypsometric integral
    !!! N.B. numerical methods forms for ERF() to be put in at a later point

    IF ( riv_depth(ip) > 0.0 .AND. logn_mean_rp(ip) > -999.0 .AND.             &
         logn_stdev_rp(ip) > -999.0 ) THEN
      frac_fplain_rp(ip) = erf_int_half + erf_int_half *                       &
                               ERF((LOG(riv_depth(ip)) - logn_mean_rp(ip)) /   &
                               (logn_stdev_rp(ip) * erf_int_sqrt2) )
    END IF

  CASE ( overbank_simple, overbank_simple_rosgen )

    ! Calculate inundated fraction as estimated river width divided by
    ! north-south cell extent (i.e. assuming that river channel runs
    !straight W-E).

    ! Leopold & Maddock (1953: eqn1)
    riv_width(ip) = riv_a * (rfm_rivflow_rp(ip)** riv_b)

    IF ( overbank_model == overbank_simple_rosgen ) THEN
      IF ( rfm_rivflow_rp(ip) > qbf(ip) ) THEN
        ! River is above bankfull and using Rosgen entrenchment ratio option.
        ! This assumes linear interpolation between (width=wbf, depth=dbf) and
        ! (width=ent_ratio*wbf, depth=2*dbf) (from definition of Rosgen
        ! entrenchment ratio)
        riv_width(ip) = wbf(ip) + ( ( wbf(ip) * (ent_ratio-1.0)                &
                                      * (riv_depth(ip) - dbf(ip)) ) / dbf(ip) )
      END IF
    END IF  !  overbank_simple_rosgen

    IF ( riv_width(ip) > 0.0 ) THEN
      frac_fplain_rp(ip) = MAX( MIN(riv_width(ip) / rivers_length , 1.0 ) ,    &
                                0.0 )
    END IF

  END SELECT  !  overbank_model

END DO

END SUBROUTINE overbank_update

END MODULE overbank_update_mod
