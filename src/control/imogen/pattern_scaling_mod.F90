#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE pattern_scaling_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE pattern_scaling(land_pts, mm, dtemp_g, imgn_drive, ainfo)

USE imgn_drive_mod, ONLY: imgn_drive_type
USE ancil_info, ONLY: ainfo_type
USE theta_field_sizes, ONLY: t_i_length

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Uses pattern scaling to supply changing meteorological driving data
!       based on the global temperature changes.
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Written by: C.Huntingford & P.Cox, 1998
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

TYPE(imgn_drive_type), INTENT(IN OUT) :: imgn_drive
TYPE(ainfo_type), INTENT(IN) :: ainfo

INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
                 ! Number of land points.
  mm             ! Number of months in a year

REAL, INTENT(IN) ::                                                            &
  dtemp_g
                 ! Mean global temperature change
INTEGER ::                                                                     &
  l, im, i, j
                 ! Loop counters.

!-----------------------------------------------------------------
! Loop over months and calculate climate anomalies from patterns (/K)
! and global or land temperature change
!-----------------------------------------------------------------
DO im = 1,mm
  DO l = 1,land_pts

    j = (ainfo%land_index(l) - 1) / t_i_length + 1
    i = ainfo%land_index(l) - (j-1) * t_i_length

    imgn_drive%tl1_ij_anom(i,j,im) =                                           &
                      imgn_drive%tl1_ij_patt(i,j,im) * dtemp_g
    imgn_drive%ql1_ij_anom(i,j,im) =                                           &
                     imgn_drive%ql1_ij_patt(i,j,im) * dtemp_g
    imgn_drive%wind_ij_anom(i,j,im) =                                          &
                     imgn_drive%wind_ij_patt(i,j,im) * dtemp_g
    imgn_drive%precip_ij_anom(i,j,im) =                                        &
                     imgn_drive%precip_ij_patt(i,j,im) * dtemp_g
    imgn_drive%diurnal_tl1_ij_anom(i,j,im) =                                   &
                     imgn_drive%diurnal_tl1_ij_patt(i,j,im) * dtemp_g
    imgn_drive%lwdown_ij_anom(i,j,im) =                                        &
                     imgn_drive%lwdown_ij_patt(i,j,im) * dtemp_g
    imgn_drive%swdown_ij_anom(i,j,im) =                                        &
                     imgn_drive%swdown_ij_patt(i,j,im) * dtemp_g
    imgn_drive%pstar_ij_anom(i,j,im) =                                         &
                     imgn_drive%pstar_ij_patt(i,j,im) * dtemp_g
  END DO     !End of loop over land points
END DO     !End of loop over months

RETURN
END SUBROUTINE pattern_scaling
END MODULE pattern_scaling_mod
#endif
