! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    SUBROUTINE INIT_LAYEREDCN---------------------------------------------

MODULE init_layeredcn_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='INIT_LAYEREDCN_MOD'

CONTAINS
! Subroutine Interface:
SUBROUTINE init_layeredcn(soil_index, progs)

USE root_frac_mod,             ONLY: root_frac
USE jules_soil_mod,            ONLY: dzsoil, sm_levels
USE jules_surface_types_mod,   ONLY: npft
USE jules_vegetation_mod,      ONLY: l_nitrogen
USE ancil_info,                ONLY: soil_pts, dim_cslayer, nsoilt, land_pts
USE jules_soil_biogeochem_mod, ONLY: soil_model_4pool, tau_lit,                &
                                     soil_bgc_model
USE pftparm,                   ONLY: rootd_ft
USE prognostics, ONLY: progs_type

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

IMPLICIT NONE

! Description:
!     Calculates the fraction of the total plant roots within each
!     soil layer.

! Documentation : UM Documentation Paper 25

TYPE(progs_type), INTENT(IN OUT) :: progs

INTEGER, INTENT(IN) :: soil_index(land_pts)

! Local variables:
REAL(KIND=real_jlslsm) :: f_root_pft(sm_levels)
                              ! Root fraction in each soil layer
                              ! used to initialise plant available inorg N

REAL(KIND=real_jlslsm) :: f_root_pft_dz(sm_levels)
                              ! Normalised roots in each soil layer
                              ! used to initialise plant available inorg N

! Local scalars:
INTEGER ::                                                                     &
 i,j,l,n,m                    ! WORK Loop counters

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='INIT_LAYEREDCN'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)


!-----------------------------------------------------------------------------
! Initialise n_inorg_avail_pft so it can be leached before the first call
! to TRIFFID.
!-----------------------------------------------------------------------------
IF (l_nitrogen) THEN
  DO n = 1,npft
    CALL root_frac(n,sm_levels,dzsoil,rootd_ft(n),f_root_pft)
    f_root_pft_dz = f_root_pft / dzsoil / (f_root_pft(1) / dzsoil(1))
    DO i = 1,soil_pts
      l = soil_index(i)
      DO j = 1,dim_cslayer
        progs%n_inorg_avail_pft(l,n,j) = progs%n_inorg_soilt_lyrs(l,1,j) * f_root_pft_dz(j)
      END DO
    END DO
  END DO
END IF


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE init_layeredcn

END MODULE init_layeredcn_mod
