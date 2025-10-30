! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module setting Parameters for each plant functional type, for TRIFFID.

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.


MODULE trif

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

INTEGER, ALLOCATABLE ::                                                        &
crop(:),                                                                       &
                 !  Flag for crop types if l_trif_crop=T
                 ! 0 = natural vegetation
                 ! 1 = crops as in the "crop" variable
                 ! 2 = pasture
                 ! 3 = bioenergy or forestry ( if l_trif_biocrop=T)
harvest_freq(:),                                                               &
                 ! Frequency of biocrop harvest (years)
                 ! Used only if l_trif_biocrop=T
harvest_type(:),                                                               &
                 ! Regime for harvesting. 0 = no harvest (default),
                 ! 1 = continuous litter harvest,
                 ! 2 = timed harvest based on height
                 ! Used only if l_trif_biocrop=T
ag_expand(:)
                 ! Switch for including in agricultural expansion
                 ! 0 = no (default) , 1 = yes . Used only if l_ag_expand=T

REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
 g_area(:)                                                                     &
                  !  Disturbance rate (/360days).
,g_grow(:)                                                                     &
                  !  Rate of leaf growth (/360days)
,g_root(:)                                                                     &
                  !  Turnover rate for root biomass (/360days).
,g_wood(:)                                                                     &
                  !  Turnover rate for woody biomass (/360days).
,lai_max(:)                                                                    &
                  !  Maximum projected LAI.
,lai_min(:)                                                                    &
                  !  Minimum projected LAI
,alloc_fast(:)                                                                 &
                  ! Fraction of landuse carbon allocated fast
                  ! product pool
,alloc_med(:)                                                                  &
                  ! Fraction of landuse carbon allocated medium
                  ! product pool
,alloc_slow(:)                                                                 &
                  ! Fraction of landuse carbon allocated slow
                  ! product pool
,dpm_rpm_ratio(:)                                                              &
                  !  Ratio of each PFTs litter allocated to the
                  !  DPM soil carbon pool versus the RPM soil
                  !  carbon pool
,retran_l(:)                                                                   &
                  ! Leaf N retranslocation factor
,retran_r(:)                                                                   &
                  ! Root N retranslocation factor
,harvest_ht(:)
                  ! Height to which harvested biocrops are cut (m)

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='TRIF'

CONTAINS

SUBROUTINE trif_alloc(npft,                                                    &
                l_triffid, l_phenol)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: npft

LOGICAL, INTENT(IN) :: l_triffid, l_phenol

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='TRIF_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!  ====trif module common====
! TRIFFID variables - only needed if TRIFFID and/or phenology is selected.
IF ( l_triffid .OR. l_phenol ) THEN
  ALLOCATE( crop(npft))
  ALLOCATE( harvest_freq(npft))
  ALLOCATE( harvest_type(npft))
  ALLOCATE( ag_expand(npft))
  ALLOCATE( g_area(npft))
  ALLOCATE( g_grow(npft))
  ALLOCATE( g_root(npft))
  ALLOCATE( g_wood(npft))
  ALLOCATE( lai_max(npft))
  ALLOCATE( lai_min(npft))
  ALLOCATE( alloc_fast(npft))
  ALLOCATE( alloc_med(npft))
  ALLOCATE( alloc_slow(npft))
  ALLOCATE( dpm_rpm_ratio(npft))
  ALLOCATE( retran_r(npft))
  ALLOCATE( retran_l(npft))
  ALLOCATE( harvest_ht(npft))
  crop(:)          = 0
  harvest_freq(:)  = 0
  harvest_type(:)  = 0
  ag_expand(:)     = 0
  g_area(:)        = 0.0
  g_grow(:)        = 0.0
  g_root(:)        = 0.0
  g_wood(:)        = 0.0
  lai_max(:)       = 0.0
  lai_min(:)       = 0.0
  alloc_fast(:)    = 0.0
  alloc_med(:)     = 0.0
  alloc_slow(:)    = 0.0
  dpm_rpm_ratio(:) = 0.0
  retran_r(:)      = 0.0
  retran_l(:)      = 0.0
  harvest_ht(:)    = 0.0

END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE trif_alloc

END MODULE trif
