#if !defined(UM_JULES)
MODULE allocate_river_arrays_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
   ModuleName='ALLOCATE_RIVER_ARRAYS_MOD'

CONTAINS
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Subroutine ALLOCATE_RIVER_ARRAYS
!
! Description: Routine that allocates memory used by standalone Rivers
!
! This routine may be better written using completely separate routines as it
! is allocating a small amount of memory that is not required and increasing
! dependencies. Please see individual comments.
!
! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.
!
!   Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
!   This file belongs in section: Technical

SUBROUTINE allocate_river_arrays(psparms_data,ainfo_data, progs_data,          &
                                 coastal_data,                                 &
                                 fluxes_data,                                  &
                                 rivers_data,                                  &
                                 wtrac_jls_data)

USE jules_vegetation_mod,      ONLY: l_triffid, l_phenol, l_use_pft_psi,       &
                                     l_acclim, l_sugar, l_red
USE jules_soil_mod,            ONLY: l_bedrock, ns_deep
USE jules_soil_biogeochem_mod, ONLY: soil_model_ecosse, soil_bgc_model,        &
                                     l_layeredc, dim_ch4layer
USE jules_water_tracers_mod,   ONLY: l_wtrac_jls

!Variables- dimensions
USE jules_snow_mod,            ONLY: nsmax
USE jules_surface_types_mod,   ONLY: npft, ntype, nnpft
USE theta_field_sizes,         ONLY: t_i_length, t_j_length,                   &
                                     u_i_length, u_j_length,                   &
                                     v_i_length, v_j_length
USE ancil_info,                ONLY: nsoilt, dim_cs1, dim_cslayer, land_pts,   &
                                     nsurft, nmasst
USE jules_sea_seaice_mod,      ONLY: nice, nice_use
USE jules_soil_mod,            ONLY: sm_levels
USE jules_water_tracers_mod,   ONLY: n_wtrac_jls, n_evap_srce

!Subroutines
USE fluxes_mod,                ONLY: fluxes_alloc
USE prognostics,               ONLY: prognostics_alloc
USE p_s_parms,                 ONLY: psparms_alloc
USE ancil_info,                ONLY: ancil_info_alloc
USE jules_rivers_mod,          ONLY: jules_rivers_alloc
USE coastal,                   ONLY: coastal_alloc
USE jules_wtrac_type_mod,      ONLY: wtrac_jls_alloc

!TYPE definitions
USE p_s_parms,            ONLY: psparms_data_type
USE ancil_info,           ONLY: ainfo_data_type
USE prognostics,          ONLY: progs_data_type
USE coastal,              ONLY: coastal_data_type
USE fluxes_mod,           ONLY: fluxes_data_type
USE jules_rivers_mod,     ONLY: rivers_data_type
USE jules_wtrac_type_mod, ONLY: jls_wtrac_data_type

USE parkind1,      ONLY: jprb, jpim
USE yomhook,       ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

!TYPES containing field data (IN OUT)
TYPE(psparms_data_type),   INTENT(IN OUT) :: psparms_data
TYPE(ainfo_data_type),     INTENT(IN OUT) :: ainfo_data
TYPE(progs_data_type),     INTENT(IN OUT) :: progs_data
TYPE(coastal_data_type),   INTENT(IN OUT) :: coastal_data
TYPE(fluxes_data_type),    INTENT(IN OUT) :: fluxes_data
TYPE(rivers_data_type),    INTENT(IN OUT) :: rivers_data
TYPE(jls_wtrac_data_type), INTENT(IN OUT) :: wtrac_jls_data

!Local variables
INTEGER :: temp_size

! Dummy fields for the local river grid size which is used to set water tracer
! fields in UM_JULES.  Therefore, a dummy field is needed here to pass into
! wtrac_jls_assoc.
INTEGER, PARAMETER :: river_row_length_dum = 1
INTEGER, PARAMETER :: river_rows_dum = 1

! Water resources is not available with rivers-standalone. Set values so that
! water resource variables are not allocated.
INTEGER, PARAMETER :: tmp_sw_river_source = 0
LOGICAL, PARAMETER :: tmp_l_minor_reservoirs = .FALSE.
LOGICAL, PARAMETER :: tmp_l_water_resources = .FALSE.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='ALLOCATE_RIVER_ARRAYS'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!Any dimension sizes should be set before we get here. Some special cases for
!UM mode can be found in surf_couple_allocate.

! For now set unset dimensions to 1, which are not on switches and thus
! allocated with full size when not required
nsurft     = 1
npft       = 1
sm_levels  = 1

! OASIS-Rivers will have zero land points, so allocate nominal space instead.
IF ( land_pts > 0 ) THEN
  temp_size  = land_pts
ELSE
  temp_size  = 1
  t_i_length = 1
  t_j_length = 1
END IF

! prognostics_alloc is only needed for progs%smcl_soilt as far as currently
! aware. This is not actually needed as only used by rivers_um_trip, but as it
! is part of the soil moisture correction for water conservation it may be
! needed in future. Could possibly allocate this separately though?
CALL prognostics_alloc(temp_size, t_i_length, t_j_length,                      &
                      nsurft, npft, nsoilt, sm_levels, ns_deep, nsmax,         &
                      dim_cslayer, dim_cs1, dim_ch4layer,                      &
                      nice, nice_use, soil_bgc_model, soil_model_ecosse,       &
                      l_layeredc, l_triffid, l_phenol, l_bedrock, l_red,       &
                      nmasst, nnpft, l_acclim, l_sugar, progs_data)

! This is where sub_surf_roff_gb and surf_roff_gb are allocated, but could
! allocate this separately too.
CALL fluxes_alloc(temp_size, t_i_length, t_j_length,                           &
                  nsurft, npft, nsoilt, sm_levels,                             &
                  nice, nice_use,                                              &
                  fluxes_data)

! Similarly psparms%smvcst_soilt, psparms%smvcwt_soilt, psparms%sthu_soilt
! used for soil moisture correction for water conservation
CALL psparms_alloc(temp_size, t_i_length, t_j_length,                          &
                   nsoilt, sm_levels, dim_cslayer, nsurft, npft,               &
                   soil_bgc_model, soil_model_ecosse,  l_use_pft_psi,          &
                   psparms_data)

CALL ancil_info_alloc(temp_size, t_i_length, t_j_length,                       &
                      nice, nsoilt, ntype,                                     &
                      ainfo_data)

CALL jules_rivers_alloc(temp_size, t_i_length, t_j_length,                     &
                        tmp_sw_river_source, tmp_l_water_resources,            &
                        rivers_data)

CALL coastal_alloc(temp_size, t_i_length, t_j_length,                          &
                   u_i_length, u_j_length,                                     &
                   v_i_length, v_j_length,                                     &
                   nice_use, nice, coastal_data)

! Allocate water tracer arrays.  As water tracers cannot currently be used
! in standalone rivers, these arrays will be set to dimension=1.
CALL wtrac_jls_alloc(land_pts, t_i_length, t_j_length, nsurft, nsoilt,         &
                     sm_levels, nsmax, nice_use, n_wtrac_jls,                  &
                     n_evap_srce, river_row_length_dum, river_rows_dum,        &
                     l_wtrac_jls, wtrac_jls_data)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName, zhook_out, zhook_handle)
RETURN
END SUBROUTINE allocate_river_arrays

END MODULE allocate_river_arrays_mod
#endif
