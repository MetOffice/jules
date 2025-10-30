MODULE c_bvoc

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------
! Parameter values for BVOC emission routine
! See Pacifico et al., 2011, Atmos. Chem. Phys.
!-----------------------------------------------------------------------
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  f_tmax = 2.3,                                                                &
             ! Empirical estimate that makes isoprene emission level off
             ! at temperature close to 40 C (Arneth personal communication)
  atau = 0.1,                                                                  &
             ! Scaling parameter (isoprene) (eq A4b Arneth et al., 2007)
  btau = 0.09,                                                                 &
             ! Scaling parameter (mono-terpene, methanol, acetone)
             !  (eq 1 Schurges et al., 2009)
  t_ref = 303.15
             ! Temperature reference value (K) (eq A4b Arneth et al., 2007)

END MODULE c_bvoc
