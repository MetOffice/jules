!-----------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

MODULE cable_land_albedo_mod

!-----------------------------------------------------------------------------
! Description:
!  Stub for entry point to CABLE's albedo scheme and radiation.
!  This will be replaced once the CABLE compilation is sorted out.
!-----------------------------------------------------------------------------

IMPLICIT NONE
PUBLIC :: cable_land_albedo
PRIVATE

! Constants to source from CABLE modules once the compilation
! is done. ccc25
INTEGER, PARAMETER :: nrb = 3                  !# rad bands VIS/NIR + Legacy LW
INTEGER, PARAMETER :: nrs = 4                  !# rad streams
                                     !(:,:,1) direct beam VIS
                                     !(:,:,2) diffuse visible
                                     !(:,:,3) direct beam NIR
                                     !(:,:,4) diffuse NIR


CONTAINS

SUBROUTINE cable_land_albedo (                                                 &
  !OUT: JULES (per rad band) albedos [GridBoxMean & per tile albedo]
  land_albedo , alb_surft,                                                     &
  !IN: JULES dimensions and associated
  row_length, rows, land_pts, nsurft, npft,                                    &
  surft_pts, surft_index, land_index,                                          &
  !IN: JULES Surface descriptions generally parametrized
  tile_frac, LAI_pft_um, HGT_pft_um, soil_alb,                                 &
  !IN: JULES  timestep varying fields
  cosine_zenith_angle, snow_tile,                                              &
  !IN: CABLE specific surface_type indexes
  ICE_Surfacetype, lakes_SurfaceType,                                          &
  !IN: CABLE Vegetation/Soil parameters. decl in params_io_cbl.F90
  VeginXfang, VeginTaul, VeginRefl,                                            &
  !IN: CABLE prognostics. decl in progs_cbl_vars_mod.F90
  SoilTemp_CABLE, OneLyrSnowDensity_CABLE, SnowAge_CABLE                       &
)
!-------------------------------------------------------------------------------
! Description:
!   Provide (return) albedo(s) to JULES [land_albedo , alb_surft]
!   per rad stream (VIS/NIR, Direct&Diffuse) [GridBoxMean & per tile albedo]
! Three main sections:
!   1. Pack CABLE variables from those passed from surf_couple_radiation()
!   2. Call CABLE's radiation/albedo scheme
!   3. Unpack albedos to send back to JULES
!-------------------------------------------------------------------------------

IMPLICIT NONE
! re-decl dims necessary to declare OUT fields
!-- IN: JULES model dimensions
INTEGER, INTENT(IN) :: row_length, rows        !# grid cell x, y
INTEGER, INTENT(IN) :: nsurft                  !# surface types, PFTS
INTEGER, INTENT(IN) :: land_pts                !# land points on x,y grid
!--- IN: CABLE  declared in grid_cell_constants_cbl
!--- OUT: JULES (per rad band) albedos [GridBoxMean & per tile albedo]
REAL,    INTENT(OUT) :: land_albedo(row_length,rows,nrs)    ! [land_albedo_ij]
REAL,    INTENT(OUT) :: alb_surft(Land_pts,nsurft,nrs)      ! [alb_tile]

!-- IN: JULES model dimensions
INTEGER, INTENT(IN) :: npft                    !# surface types, PFTS

!---IN: JULES model associated
INTEGER, INTENT(IN) :: surft_pts(nsurft)            ! # land points per PFT
INTEGER, INTENT(IN) :: surft_index(land_pts,nsurft) ! Index in land_pts array
INTEGER, INTENT(IN) :: land_index(land_pts)         ! Index in (x,y) array

!-- IN: JULES Surface descriptions generally parametrized
REAL, INTENT(IN) :: tile_frac(land_pts,nsurft)      ! fraction of each surf type
REAL, INTENT(IN) :: LAI_pft_um(land_pts, npft)      ! Leaf area index.
REAL, INTENT(IN) :: HGT_pft_um(land_pts, npft)      ! Canopy height
REAL, INTENT(IN) :: soil_alb(land_pts)              ! Snow-free, soil albedo

!---IN: JULES  timestep varying fields
REAL, INTENT(IN) :: cosine_zenith_angle(row_length,rows)  ! zenith angle of sun
REAL, INTENT(IN) :: snow_tile(land_pts,nsurft)            ! snow depth (units?)

!--- IN: CABLE  declared in grid_cell_constants_cbl

!--- IN: CABLE specific Surface/Soil type indexes
INTEGER, INTENT(IN) :: ICE_SurfaceType
INTEGER, INTENT(IN) :: lakes_SurfaceType

!---IN: CABLE Vegetation/Soil parameters. decl in params_io_cbl.F90
REAL, INTENT(IN) :: VeginXfang(nsurft)               ! Leaf Angle
REAL, INTENT(IN) :: VeginTaul(nrb, nsurft )          ! Leaf Transmisivity
REAL, INTENT(IN) :: VeginRefl(nrb, nsurft )          ! Leaf Reflectivity

!---IN: CABLE prognostics. decl in progs_cbl_vars_mod.F90
REAL, INTENT(IN) :: SoilTemp_CABLE(land_pts, nsurft )
REAL, INTENT(IN) :: OneLyrSnowDensity_CABLE(land_pts, nsurft )
REAL, INTENT(IN) :: SnowAge_CABLE(land_pts, nsurft )

! initialise INTENT(OUT) fields
land_albedo = 0.0; alb_surft = 0.0

RETURN

END SUBROUTINE cable_land_albedo

END MODULE cable_land_albedo_mod

