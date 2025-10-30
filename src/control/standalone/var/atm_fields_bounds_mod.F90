#if !defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  *** JULES version of atm_fields_bounds_mod ***
!
!  defines a type to address array bounds for UM arrays
!
!  Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
!  This file belongs in section: Top Level

MODULE atm_fields_bounds_mod

IMPLICIT NONE
!
! Description:
!  switchable variables for loop bounds and array size declarations
!
! Method:
!
!
! Code description:
!   Language: Fortran 90.
!   This code is written to programming standard UMDP3 vn 8.1.


TYPE :: array_dims
  INTEGER :: i_start =-HUGE(INT(1))
  INTEGER :: i_end   = HUGE(INT(1))
  INTEGER :: j_start =-HUGE(INT(1))
  INTEGER :: j_end   = HUGE(INT(1))
  INTEGER :: k_start =-HUGE(INT(1))
  INTEGER :: k_end   = HUGE(INT(1))
END TYPE array_dims


! arrays for u,v,w,t,p fields without halo, small halo and large
! halo

TYPE (array_dims), SAVE :: udims, vdims, wdims, tdims, pdims,                  &
                     udims_s, vdims_s, wdims_s, tdims_s, pdims_s,              &
                     udims_l, vdims_l, wdims_l, tdims_l, pdims_l


! further arrays to cope with different number of vertical levels
TYPE (array_dims), SAVE :: rdims2, trdims_xstl, o3dims2,                       &
                           trdims_ltl, o3dims, oneddims

! arrays used in stochastic physics (atm section=35) to define a
! smoothing mask with halo size set in stph_setup
TYPE (array_dims), SAVE :: stphdims_l

CONTAINS


SUBROUTINE atm_fields_bounds_init(offx,offy,halo_i,halo_j,                     &
                  row_length,rows,n_rows,model_levels,wet_levels_opt,          &
                  tr_levels_opt,bl_levels_opt,ozone_levels_opt)

INTEGER, INTENT(IN) ::                                                         &
       offx,offy,halo_i,halo_j,row_length,rows,n_rows,model_levels

INTEGER, INTENT(IN), OPTIONAL ::                                               &
       wet_levels_opt,bl_levels_opt,tr_levels_opt,ozone_levels_opt
INTEGER ::                                                                     &
       wet_levels,bl_levels,tr_levels,ozone_levels


! Initialise to model levels
wet_levels   = model_levels
bl_levels    = model_levels
tr_levels    = model_levels
ozone_levels = model_levels

! If options are present then set the required levels.
IF (PRESENT(wet_levels_opt   )) wet_levels   = wet_levels_opt
IF (PRESENT(bl_levels_opt    )) bl_levels    = bl_levels_opt
IF (PRESENT(tr_levels_opt    )) tr_levels    = tr_levels_opt
IF (PRESENT(ozone_levels_opt )) ozone_levels = ozone_levels_opt

! NEW DYNAMICS

! dimensions of u-field: ND
udims%i_start           = 1
udims%i_end             = row_length
udims%j_start           = 1
udims%j_end             = rows
udims%k_start           = 1
udims%k_end             = model_levels

! dimensions of v-field: ND
vdims%i_start           = 1
vdims%i_end             = row_length
vdims%j_start           = 1
vdims%j_end             = n_rows
vdims%k_start           = 1
vdims%k_end             = model_levels

! dimensions of w-field: ND
wdims%i_start           = 1
wdims%i_end             = row_length
wdims%j_start           = 1
wdims%j_end             = rows
wdims%k_start           = 1
wdims%k_end             = model_levels

! dimensions of theta-field:  ND
tdims                   = wdims

! dimensions of pressure-field:
pdims                   = tdims

! dimensions of u-field (small halos): ND
udims_s%i_start         = 1 - offx
udims_s%i_end           = row_length + offx
udims_s%j_start         = 1 - offy
udims_s%j_end           = rows + offy
udims_s%k_start         = 1
udims_s%k_end           = model_levels

! dimensions of v-field (small halos): ND
vdims_s%i_start         = 1 - offx
vdims_s%i_end           = row_length + offx
vdims_s%j_start         = 1 - offy
vdims_s%j_end           = n_rows + offy
vdims_s%k_start         = 1
vdims_s%k_end           = model_levels

! dimensions of w-field (small halos): ND
wdims_s%i_start         = 1 - offx
wdims_s%i_end           = row_length + offx
wdims_s%j_start         = 1 - offy
wdims_s%j_end           = rows + offy
wdims_s%k_start         = 0
wdims_s%k_end           = model_levels

! dimensions of theta-field (small halos): ND
tdims_s%i_start         = 1 - offx
tdims_s%i_end           = row_length + offx
tdims_s%j_start         = 1 - offy
tdims_s%j_end           = rows + offy
tdims_s%k_start         = 1
tdims_s%k_end           = model_levels

! dimensions of pressure-field (small halos):
pdims_s                 = tdims_s

! large halos

! dimensions of u-field (large halos): ND
udims_l%i_start         = 1 - halo_i
udims_l%i_end           = row_length + halo_i
udims_l%j_start         = 1 - halo_j
udims_l%j_end           = rows + halo_j
udims_l%k_start         = 1
udims_l%k_end           = model_levels

! dimensions of v-field (large halos): ND
vdims_l%i_start         = 1 - halo_i
vdims_l%i_end           = row_length + halo_i
vdims_l%j_start         = 1 - halo_j
vdims_l%j_end           = n_rows + halo_j
vdims_l%k_start         = 1
vdims_l%k_end           = model_levels

! dimensions of w-field (large halos): ND
wdims_l%i_start         = 1 - halo_i
wdims_l%i_end           = row_length + halo_i
wdims_l%j_start         = 1 - halo_j
wdims_l%j_end           = rows + halo_j
wdims_l%k_start         = 0
wdims_l%k_end           = model_levels

! dimensions of theta-field (large halos): ND
tdims_l%i_start         = 1 - halo_i
tdims_l%i_end           = row_length + halo_i
tdims_l%j_start         = 1 - halo_j
tdims_l%j_end           = rows + halo_j
tdims_l%k_start         = 1
tdims_l%k_end           = model_levels

! dimensions of pressure-field (large halos): ND
pdims_l                 = tdims_l

! other fields

! dimensions of rho-field : ND
rdims2                  = pdims
rdims2%k_start          = 0
rdims2%k_end            = model_levels + 1

! dimensions of tracer array : ND
trdims_xstl             = tdims
trdims_xstl%k_end       = tr_levels

! dimensions of O3-array : ND
o3dims2                 = tdims
o3dims2%k_start         = 1
o3dims2%k_end           = ozone_levels

! tracer array dimensions : ND
trdims_ltl              = tdims_l
trdims_ltl%k_end        = tr_levels

! Ozone dimensions (1d): ND
o3dims%i_start          = 1
o3dims%i_end            = row_length * rows * ozone_levels
o3dims%j_start          = 0
o3dims%j_end            = 0
o3dims%k_start          = 0
o3dims%k_end            = 0

! generic 1d-dims ND
oneddims%i_start          = 1
oneddims%i_end            = row_length * rows * model_levels
oneddims%j_start          = 0
oneddims%j_end            = 0
oneddims%k_start          = 0
oneddims%k_end            = 0

END SUBROUTINE atm_fields_bounds_init
END MODULE atm_fields_bounds_mod
#endif
