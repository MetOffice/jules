
! *****************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in Veg3 Ecosystem Demography
! *****************************COPYRIGHT****************************************

MODULE veg3_litter_mod

IMPLICIT NONE

PRIVATE
PUBLIC :: veg3_litter

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='VEG3_LITTER_MOD'

CONTAINS
SUBROUTINE veg3_Litter(                                                        &
                !IN Control vars
                veg_index_pts,veg_index,veg3_ctrl,land_pts,nnpft,              &
                !IN parms
                litter_parms,                                                  &
                !IN state
                veg_state,                                                     &
                ! OUT Fields
                local_litter                                                   &
                !OUT Diagnostics
                )

!Only get the data structures - the data comes through the calling tree
USE veg3_parm_mod, ONLY:  veg3_ctrl_type,litter_parm_type
USE veg3_field_mod, ONLY:  veg_state_type

IMPLICIT NONE

!----------------------------------------------------------------------------
! Integers with INTENT IN
!----------------------------------------------------------------------------
INTEGER, INTENT(IN) :: land_pts,nnpft,veg_index(land_pts),veg_index_pts


!-----------------------------------------------------------------------------
! Objects with INTENT IN
!-----------------------------------------------------------------------------
TYPE(veg3_ctrl_type),INTENT(IN)   :: veg3_ctrl
TYPE(litter_parm_type),INTENT(IN) :: litter_parms

!-----------------------------------------------------------------------------
! Objects with INTENT INOUT
!-----------------------------------------------------------------------------
TYPE(veg_state_type),INTENT(IN OUT)   :: veg_state

!-----------------------------------------------------------------------------
! Reals with INTENT OUT
!-----------------------------------------------------------------------------
REAL, INTENT(OUT)      ::                                                      &
local_litter(veg3_ctrl%land_pts,veg3_ctrl%npft)

!-----------------------------------------------------------------------------
!Local Vars
!-----------------------------------------------------------------------------
INTEGER                ::l,n,k

REAL                   ::                                                      &
leaf_litter(land_pts,nnpft),                                                   &
wood_litter(land_pts,nnpft),                                                   &
root_litter(land_pts,nnpft)


!End of headers

! Initialise vars

leaf_litter(:,:)  = 0.0
root_litter(:,:)  = 0.0
wood_litter(:,:)  = 0.0
local_litter(:,:) = 0.0


!$OMP PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC) COLLAPSE(2)                   &
!$OMP PRIVATE(l,n,k)                                                           &
!$OMP SHARED(litter_parms,veg_state,leaf_litter,root_litter,wood_litter,       &
!$OMP        local_litter,veg_index,veg_index_pts,nnpft)
DO n = 1, nnpft
  DO k = 1, veg_index_pts
    l = veg_index(k)

    leaf_litter(l,n) = litter_parms%g_leaf(n) * veg_state%leafC(l,n)
    root_litter(l,n) = litter_parms%g_root(n) * veg_state%rootC(l,n)
    wood_litter(l,n) = litter_parms%g_wood(n) * veg_state%woodC(l,n)

    local_litter(l,n) = leaf_litter(l,n) + root_litter(l,n) + wood_litter(l,n)

  END DO
END DO
!$OMP END PARALLEL DO

RETURN
END SUBROUTINE veg3_Litter

!-----------------------------------------------------------------------------

END MODULE veg3_litter_mod
