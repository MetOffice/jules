! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module setting Tile ID numbers, which are used to identify which land
!   surface tiles are present.

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.

MODULE land_tile_ids_mod

USE max_dimensions,   ONLY:                                                    &
    ntype_max,                                                                 &
    snow_layers_max,                                                           &
    elev_tile_max

USE free_tracers_inputs_mod, ONLY: max_wtrac

USE jules_sea_seaice_mod, ONLY: nice_max

USE missing_data_mod, ONLY: imdi

IMPLICIT NONE

!-----------------------------------------------------------------------

! Land surface types :
!       1 - Broadleaf Tree
!     101 - Broadleaf Tree deciduous
!     102 - Broadleaf Tree evergreen tropical
!     103 - Broadleaf Tree evergreen temperate
!       2 - Needleleaf Tree
!     201 - Needleleaf Tree deciduous
!     202 - Needleleaf Tree evergreen
!       3 - C3 Grass
!     301 - C3 Crop
!     302 - C3 Pasture
!       4 - C4 Grass
!     401 - C4 Crop
!     402 - C4 Pasture
!       5 - Shrub
!     501 - Shrub deciduous
!     502 - Shrub evergreen
!       6 - Urban
!     601 - Urban roof
!     602 - Urban canyon
!       7 - Water
!       8 - Soil
!       9 - Ice
! 901-925 - Glacier/Icesheet model ice surface on elevation classes(1-25)
! 926-950 - Glacier/Icesheet model rock surface on elevation classes(1-25)


!-----------------------------------------------------------------------

INTEGER, PRIVATE :: i ! Loop counter

! Surface type definitions
INTEGER, PARAMETER ::                                                          &
    id_brd_leaf          =  1                                                  &
                ! ID number of surface type 'Broadleaf Tree'
   ,id_brd_leaf_dec      =  101                                                &
                ! ID number of surface type 'Broadleaf Tree deciduous'
   ,id_brd_leaf_eg_trop  =  102                                                &
                ! ID number of surface type 'Broadleaf Tree eg tropical'
   ,id_brd_leaf_eg_temp  =  103                                                &
                ! ID number of surface type 'Broadleaf Tree eg temperate'
   ,id_ndl_leaf          =  2                                                  &
                ! ID number of surface type 'Needleleaf Tree'
   ,id_ndl_leaf_dec      =  201                                                &
                ! ID number of surface type 'Needleleaf Tree deciduous'
   ,id_ndl_leaf_eg       =  202                                                &
                ! ID number of surface type 'Needleleaf Tree evergreen'
   ,id_C3grass           =  3                                                  &
                ! ID number of surface type 'C3 Grass'
   ,id_C3crop            =  301                                                &
                ! ID number of surface type 'C3 Crop'
   ,id_C3pasture         =  302                                                &
                ! ID number of surface type 'C3 Pasture'
   ,id_C4grass           =  4                                                  &
                ! ID number of surface type 'C4 Grass'
   ,id_C4crop            =  401                                                &
                ! ID number of surface type 'C4 Crop'
   ,id_C4pasture         =  402                                                &
                ! ID number of surface type 'C4 Pasture'
   ,id_shrub             =  5                                                  &
                ! ID number of surface type 'Shrub'
   ,id_shrub_dec         =  501                                                &
                ! ID number of surface type 'Shrub deciduous'
   ,id_shrub_eg          =  502                                                &
                ! ID number of surface type 'Shrub evergreen'
   ,id_urban             =  6                                                  &
                ! ID number of surface type 'Urban' (l_urban2t == .FALSE.)
   ,id_lake              =  7                                                  &
                ! ID number of surface type 'Lake'
   ,id_soil              =  8                                                  &
                ! ID number of surface type 'Soil'
   ,id_ice               =  9                                                  &
                ! ID number of surface type 'Ice'

   ,id_elev_ice(25) = [901, 902, 903, 904, 905, 906, 907, 908, 909, 910,       &
                       911, 912, 913, 914, 915, 916, 917, 918, 919, 920,       &
                       921, 922, 923, 924, 925]                                &
                ! ID numbers of surface type 'Ice surface on elevation
                ! classes'

   ,id_elev_rock(25) = [                        926, 927, 928, 929, 930,       &
                       931, 932, 933, 934, 935, 936, 937, 938, 939, 940,       &
                       941, 942, 943, 944, 945, 946, 947, 948, 949, 950]       &
                ! ID numbers of surface type 'Rock surface on elevation
                ! classes'

! For use when l_urban2t == .TRUE. (Urban 2-tile scheme on)

     ,id_urban_canyon      = 601                                               &
                  ! ID number of surface type 'Urban roof'
     ,id_urban_roof        = 602                                               &
                  ! ID number of surface type 'Urban canyon'
     ,id_usr_type(90)      = [ (i, i = 10, 99) ]
                  ! ID numbers for user defined surface type (10-99)

INTEGER :: surface_type_ids(ntype_max) = imdi
              ! Array which maps pseudo levels to tile types

INTEGER :: ml_snow_type_ids(ntype_max * snow_layers_max) = imdi
              ! Array which maps pseudo levels to tile types

INTEGER :: tile_ids_in(ntype_max * snow_layers_max) = imdi
              ! Tile IDs in the input header

INTEGER :: surface_type_wtrac_ids(ntype_max*max_wtrac) = imdi
              ! Array which maps pseudo levels to tile types and water tracers

INTEGER :: ml_snow_type_wtrac_ids(ntype_max*snow_layers_max*max_wtrac) = imdi
              ! Array which maps pseudo levels to tile types, snow layers and
              !  water tracers

INTEGER :: seaice_wtrac_ids(nice_max*max_wtrac) = imdi
              ! Array which maps pseudo levels to sea ice categories and
              !  water tracers

CONTAINS

SUBROUTINE set_surface_type_ids ( )

USE ereport_mod,             ONLY: ereport

USE jules_surface_types_mod, ONLY:                                             &
    ntype,                                                                     &
    brd_leaf,                                                                  &
    brd_leaf_dec,                                                              &
    brd_leaf_eg_trop,                                                          &
    brd_leaf_eg_temp,                                                          &
    ndl_leaf,                                                                  &
    ndl_leaf_dec,                                                              &
    ndl_leaf_eg,                                                               &
    c3_grass,                                                                  &
    c3_crop,                                                                   &
    c3_pasture,                                                                &
    c4_grass,                                                                  &
    c4_crop,                                                                   &
    c4_pasture,                                                                &
    shrub,                                                                     &
    shrub_dec,                                                                 &
    shrub_eg,                                                                  &
    urban,                                                                     &
    urban_canyon,                                                              &
    urban_roof,                                                                &
    lake,                                                                      &
    soil,                                                                      &
    ice,                                                                       &
    usr_type,                                                                  &
    elev_ice,                                                                  &
    elev_rock

USE jules_print_mgr, ONLY:                                                     &
   jules_message,                                                              &
   jules_print,                                                                &
   jules_format

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

INTEGER      :: i ! Loop counter
INTEGER      :: errorstatus
CHARACTER(LEN=18), PARAMETER      :: routinename='set_tile_id_arrays'

! Set up surface type (pl_code = 9) ids array. These indices are only required
! by the aggregate tile to ensure that the surface configuration is
! [1,2,3,4,5,6,7,8,9]. They will be reset to missing data once the checks have
! been performed to prevent errors going undetected.

IF (brd_leaf         > 0)                                                      &
                surface_type_ids(brd_leaf         ) = id_brd_leaf
IF (brd_leaf_dec     > 0)                                                      &
                surface_type_ids(brd_leaf_dec     ) = id_brd_leaf_dec
IF (brd_leaf_eg_trop > 0)                                                      &
                surface_type_ids(brd_leaf_eg_trop ) = id_brd_leaf_eg_trop
IF (brd_leaf_eg_temp > 0)                                                      &
                surface_type_ids(brd_leaf_eg_temp ) = id_brd_leaf_eg_temp
IF (ndl_leaf         > 0)                                                      &
                surface_type_ids(ndl_leaf         ) = id_ndl_leaf
IF (ndl_leaf_dec     > 0)                                                      &
                surface_type_ids(ndl_leaf_dec     ) = id_ndl_leaf_dec
IF (ndl_leaf_eg      > 0)                                                      &
                surface_type_ids(ndl_leaf_eg      ) = id_ndl_leaf_eg
IF (c3_grass         > 0)                                                      &
                surface_type_ids(c3_grass         ) = id_c3grass
IF (c3_crop          > 0)                                                      &
                surface_type_ids(c3_crop          ) = id_c3crop
IF (c3_pasture       > 0)                                                      &
                surface_type_ids(c3_pasture       ) = id_c3pasture
IF (c4_grass         > 0)                                                      &
                surface_type_ids(c4_grass         ) = id_c4grass
IF (c4_crop          > 0)                                                      &
                surface_type_ids(c4_crop          ) = id_c4crop
IF (c4_pasture       > 0)                                                      &
                surface_type_ids(c4_pasture       ) = id_c4pasture
IF (shrub            > 0)                                                      &
                surface_type_ids(shrub            ) = id_shrub
IF (shrub_dec        > 0)                                                      &
                surface_type_ids(shrub_dec        ) = id_shrub_dec
IF (shrub_eg         > 0)                                                      &
                surface_type_ids(shrub_eg         ) = id_shrub_eg
IF (urban            > 0)                                                      &
                surface_type_ids(urban            ) = id_urban
IF (lake             > 0)                                                      &
                surface_type_ids(lake             ) = id_lake
IF (soil             > 0)                                                      &
                surface_type_ids(soil             ) = id_soil
IF (ice              > 0)                                                      &
                surface_type_ids(ice              ) = id_ice
IF (urban_canyon     > 0)                                                      &
                surface_type_ids(urban_canyon     ) = id_urban_canyon
IF (urban_roof       > 0)                                                      &
                surface_type_ids(urban_roof       ) = id_urban_roof

DO i = 1, ntype
  IF (usr_type(i)    > 0)                                                      &
                surface_type_ids(usr_type(i)      ) = id_usr_type(i)
END DO

IF ( (SIZE(id_elev_ice) /= elev_tile_max) .OR.                                 &
     (SIZE(id_elev_rock) /= elev_tile_max) ) THEN
  errorstatus = 20
  WRITE(jules_message, '(2A,I0,A)')                                            &
     'Icesheet elevation classes currently do not support more than ',         &
     elev_tile_max, ' of each elevated tile type'
  CALL ereport ( routinename, errorstatus, jules_message )
END IF


DO i = 1, elev_tile_max
  IF (elev_ice(i) > 0)  surface_type_ids(elev_ice(i))  = id_elev_ice(i)
  IF (elev_rock(i) > 0) surface_type_ids(elev_rock(i)) = id_elev_rock(i)
END DO

! Print the surface types that are present
WRITE(jules_format,'(a3,i3,a8)') '(a,',ntype,'(1x,i6))'
WRITE(jules_message,jules_format) ' Surface types present =',                  &
   surface_type_ids(1:ntype)
CALL jules_print(routinename, jules_message)

! Check that all surface types have been specified
IF ( ANY( surface_type_ids(1:ntype) == imdi ) ) THEN
  errorstatus = 30
  WRITE(jules_message, '(A,I6)')                                               &
     ' All surface types need to be specified. Please see job.out.'
  CALL ereport ( routinename, errorstatus, jules_message )
END IF

! Check that tile IDs are unique
DO i = 1, ntype
  IF ( COUNT( surface_type_ids(:) == surface_type_ids(i) ) /= 1 ) THEN
    errorstatus = 30
    WRITE(jules_message, '(A,I6)')                                             &
       ' Surface type ID not unique :', surface_type_ids(i)
    CALL ereport ( routinename, errorstatus, jules_message )
  END IF
END DO

END SUBROUTINE set_surface_type_ids

SUBROUTINE set_ml_snow_type_ids ( )

USE jules_surface_types_mod, ONLY: ntype
USE jules_snow_mod, ONLY: nsmax

IMPLICIT NONE

INTEGER      :: i, j ! Loop counter
INTEGER      :: pseudo

! Set up multilayer snow type (pl_code = 11) ids array
IF ( nsmax > 0 ) THEN
  DO i = 1, ntype
    DO j = 1, nsmax
      pseudo = i + (j-1) * ntype
      ml_snow_type_ids(pseudo) = surface_type_ids(i) * 1000 + j
    END DO
  END DO
END IF

END SUBROUTINE set_ml_snow_type_ids

! ============================================================

SUBROUTINE set_surface_type_wtrac_ids ()

USE jules_surface_types_mod, ONLY: ntype
USE jules_water_tracers_mod, ONLY: n_wtrac_jls

IMPLICIT NONE

INTEGER      :: i, i_wt ! Loop counter
INTEGER      :: pseudo

! Set up land tiles * water tracers type (pl_code = 23) ids array
DO i = 1, ntype
  DO i_wt = 1, n_wtrac_jls
    pseudo = i + (i_wt-1) * ntype
    surface_type_wtrac_ids(pseudo) = surface_type_ids(i) * 1000 + i_wt
  END DO
END DO

END SUBROUTINE set_surface_type_wtrac_ids

! ======================================================================

SUBROUTINE set_ml_snow_type_wtrac_ids ( )

USE jules_surface_types_mod, ONLY: ntype
USE jules_snow_mod,          ONLY: nsmax
USE jules_water_tracers_mod, ONLY: n_wtrac_jls

IMPLICIT NONE

INTEGER      :: i, j, i_wt ! Loop counter
INTEGER      :: pseudo

! Set up multilayer snow * water tracer type (pl_code = 24) ids array
DO i = 1, ntype
  DO j = 1, nsmax
    DO i_wt = 1, n_wtrac_jls
      pseudo = i + (j-1) * ntype + (i_wt-1) * ntype*nsmax
      ml_snow_type_wtrac_ids(pseudo) = surface_type_ids(i) * 1000000           &
                                        + j*1000 + i_wt
    END DO
  END DO
END DO

END SUBROUTINE set_ml_snow_type_wtrac_ids

! ======================================================================

SUBROUTINE set_seaice_wtrac_ids ( )

USE jules_sea_seaice_mod,    ONLY: nice
USE jules_water_tracers_mod, ONLY: n_wtrac_jls

IMPLICIT NONE

INTEGER      :: i, i_wt ! Loop counter
INTEGER      :: pseudo

! Set up sea ice category * water tracer type (pl_code = 25) ids array
DO i = 1, nice
  DO i_wt = 1, n_wtrac_jls
    pseudo = i + (i_wt-1) * nice
    seaice_wtrac_ids(pseudo) = i * 1000 + i_wt
  END DO
END DO

END SUBROUTINE set_seaice_wtrac_ids

END MODULE land_tile_ids_mod
