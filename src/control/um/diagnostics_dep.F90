#if defined (UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE diagnostics_dep_mod

!-------------------------------------------------------------------------------
! Description:
!   Calculates diagnostics for JULES with atmospheric deposition
!   Overall deposition velocity and loss rate
!   Based on ukca_ddclac.F90
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGENIC FLUXES.
!
! Code Description:
!   Language: Fortran 90.
!-----------------------------------------------------------------------------

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName = 'DIAGNOSTICS_DEP_MOD'

CONTAINS

!-------------------------------------------------------------------------------
SUBROUTINE diagnostics_dep(                                                    &
  row_length, rows, bl_levels,                                                 &
  dep_vd_ij, dep_vd_gb, dep_loss_rate_ij,                                      &
  stashwork                                                                    &
  )
!-------------------------------------------------------------------------------

USE jules_deposition_mod,    ONLY: ndry_dep_species, dep_h2_soil_scheme

USE deposition_species_mod,  ONLY: dep_species_name

USE jules_surface_types_mod, ONLY: ntype

USE parkind1,                ONLY: jprb, jpim

USE copydiag_mod,            ONLY: copydiag

USE set_pseudo_list_mod,     ONLY: set_pseudo_list

USE stash_array_mod, ONLY:                                                     &
    len_stlist, stash_pseudo_levels, num_stash_pseudo, stindex, stlist,        &
    num_stash_levels, stash_levels, si, si_last, sf

USE atm_fields_bounds_mod,   ONLY: pdims

USE yomhook,                 ONLY: lhook, dr_hook

IMPLICIT NONE

INTEGER, INTENT(IN)  :: row_length, rows, bl_levels

REAL, INTENT(IN)     :: dep_vd_ij(row_length, rows, ntype, ndry_dep_species)
                             ! Surface deposition velocity term (m s-1)
REAL, INTENT(IN)     :: dep_vd_gb(row_length, rows, ndry_dep_species)
                             ! Surface deposition velocity term (m s-1)
REAL, INTENT(IN)     :: dep_loss_rate_ij(row_length, rows, ndry_dep_species)
                             ! First-order loss rate (s-1) for trace gases
! Diagnostic variables
REAL ::                                                                        &
 stashwork( * )    ! STASH workspace

! Local variables

INTEGER              :: n
INTEGER              :: pslevel           ! n counter for pseudolevels
INTEGER              :: pslevel_out       ! index for pseudolevels sent to STASH
INTEGER              :: si_start, si_stop ! Stashwork bounds for calling copydiag

LOGICAL              :: plltype(ntype)    ! pseudolevel list for surface types

INTEGER(KIND=jpim), PARAMETER      :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER      :: zhook_out = 1
REAL(KIND=jprb)                    :: zhook_handle

CHARACTER(LEN=*), PARAMETER        ::                                          &
  RoutineName='DIAGNOSTICS_DEP'

!-------------------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Write to diagnostic stream

! (1) dry deposition velocities for each tile
! for CH4 (m1s50i432), O3 (m1s50i433) and HONO2 (m1s50i434).

! (2) dry deposition rate (1/s) for entire gridbox
! for CH4 (m1s50i435), O3 (m1s50i436) and HONO2 (m1s50i437)

! (3) grid=box mean dry deposition velocities
! for H2 (m1s50i500) - NOT SET-UP YET.

! These diagnostics are mainly for the purpose of model debugging.

DO n = 1, ndry_dep_species

  IF (dep_species_name(n) == 'CH4       ') THEN
    ! dep_vd_ij on tiles for CH4
    IF (sf(432,50)) THEN
      CALL set_pseudo_list(ntype,len_stlist,                                   &
        stlist(:,stindex(1,432,50,1)),                                         &
        plltype,stash_pseudo_levels,num_stash_pseudo)

      pslevel_out=0
      DO pslevel = 1, ntype
        IF (plltype(pslevel)) THEN
          pslevel_out=pslevel_out+1
          si_start = si(432,50,1)+(pslevel_out-1)*pdims%i_end*pdims%j_end
          si_stop  = si(432,50,1)+(pslevel_out)*pdims%i_end*pdims%j_end-1
          CALL copydiag(stashwork(si_start:si_stop),                           &
            dep_vd_ij(:,:,pslevel,n), pdims%i_end,pdims%j_end)
        END IF
      END DO
    END IF

    ! Item 435: deposition loss rate (s-1) on gridbox for CH4
    IF (sf(435,50)) THEN
      CALL copydiag (                                                          &
        stashwork(si(435,50,1):si_last(435,50,1)),                             &
        dep_loss_rate_ij(:,:,n),                                               &
        row_length,rows)
    END IF

  ELSE IF (dep_species_name(n) == 'O3        ') THEN
    ! dep_vd_ij on tiles for O3
    IF (sf(433,50)) THEN
      CALL set_pseudo_list(ntype,len_stlist,                                   &
        stlist(:,stindex(1,433,50,1)),                                         &
        plltype,stash_pseudo_levels,num_stash_pseudo)

      pslevel_out = 0
      DO pslevel = 1, ntype
        IF (plltype(pslevel)) THEN
          pslevel_out=pslevel_out+1
          si_start = si(433,50,1)+(pslevel_out-1)*pdims%i_end*pdims%j_end
          si_stop  = si(433,50,1)+(pslevel_out)*pdims%i_end*pdims%j_end-1
          CALL copydiag(stashwork(si_start:si_stop),                           &
            dep_vd_ij(:,:,pslevel,n), pdims%i_end,pdims%j_end)
        END IF
      END DO
    END IF

    ! Item 436: deposition loss rate (s-1) on gridbox for O3
    IF (sf(436,50)) THEN
      CALL copydiag (                                                          &
        stashwork(si(436,50,1):si_last(436,50,1)),                             &
        dep_loss_rate_ij(:,:,n),                                               &
        row_length,rows)
    END IF

  ELSE IF (dep_species_name(n) == 'HONO2     ') THEN
    ! dep_vd_ij on tiles for HONO2
    IF (sf(434,50)) THEN
      CALL set_pseudo_list(ntype,len_stlist,                                   &
        stlist(:,stindex(1,434,50,1)),                                         &
        plltype,stash_pseudo_levels,num_stash_pseudo)

      pslevel_out = 0
      DO pslevel = 1, ntype
        IF (plltype(pslevel)) THEN
          pslevel_out=pslevel_out+1
          si_start = si(434,50,1)+(pslevel_out-1)*pdims%i_end*pdims%j_end
          si_stop  = si(434,50,1)+(pslevel_out)*pdims%i_end*pdims%j_end-1
          CALL copydiag(stashwork(si_start:si_stop),                           &
            dep_vd_ij(:,:,pslevel,n), pdims%i_end,pdims%j_end)
        END IF
      END DO
    END IF

    ! Item 437: deposition loss rate (s-1) on gridbox for HONO2
    IF (sf(437,50)) THEN
      CALL copydiag (                                                          &
        stashwork(si(437,50,1):si_last(437,50,1)),                             &
        dep_loss_rate_ij(:,:,n),                                               &
        row_length,rows)
    END IF

  ELSE IF (dep_species_name(n) == 'H2        '                                 &
    .AND.  dep_h2_soil_scheme == 2) THEN
    ! Item 500: deposition velocity (m s-1) on gridbox for H2
    ! For use Paulot et al. scheme for soil H2 uptake
    ! dep_h2_soil_scheme = 2
    ! NOT YET AVAILABLE
    IF (sf(500,50)) THEN
      CALL copydiag (                                                          &
        stashwork(si(500,50,1):si_last(500,50,1)),                             &
        dep_vd_gb(:,:,n),                                                      &
        row_length,rows)
    END IF

  END IF

END DO            ! End of loop over deposited species

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN

END SUBROUTINE diagnostics_dep
END MODULE diagnostics_dep_mod
#endif
