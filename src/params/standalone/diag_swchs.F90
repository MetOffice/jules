#if !defined(UM_JULES)
! Module containing logical switches for diagnostic outputs

MODULE diag_swchs

IMPLICIT NONE
LOGICAL, PARAMETER :: stf_sub_surf_roff = .TRUE. ! Flag for sub-surface runoff
LOGICAL, PARAMETER :: srflow = .TRUE.          ! Flag for river outflow
LOGICAL, PARAMETER :: srrun = .TRUE.           ! Flag for runoff after routing

END MODULE diag_swchs
#endif
