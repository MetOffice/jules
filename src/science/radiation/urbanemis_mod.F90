MODULE urbanemis_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='URBANEMIS_MOD'
CONTAINS
SUBROUTINE urbanemis(hwr, emisr, emiss, emisw, sigmalw)

! Description:
!   Calculates the emissivity of the urban canyon
!
! (c) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! -----------------------------------------------------------------------------

USE matinv_mod,       ONLY: matinv

USE yomhook,          ONLY: lhook, dr_hook
USE parkind1,         ONLY: jprb, jpim

USE jules_print_mgr,  ONLY: jules_message, jules_print, PrNorm

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

! Subroutine arguments
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
   hwr,     & ! Canyon height-to-width ratio
   emisr,   & ! Emissivity road
   emiss,   & ! Emissivity sky
   emisw      ! Emissivity wall

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
   sigmalw    ! Effective emissivity

! Local declarations:

INTEGER, PARAMETER :: arraydim = 3 ! Dimension of arrays RF and RFINV

REAL(KIND=real_jlslsm) ::                                                      &
   sigmat,      &  ! Effective emissivity
   aa,bb,cc,dd, &   ! Elements of view factors array
   emisrratio,                                                                 &
   emiswratio

! Work variables - arrays
REAL(KIND=real_jlslsm) ::                                                      &
   gammau(arraydim,arraydim)    ! Exchange cofficients

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='URBANEMIS'


!-----------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

aa = (1.0 + hwr**2.0)**(0.5) - hwr
bb = (1.0 + (1.0 / hwr)**2.0)**(0.5) - (1.0 / hwr)
cc = (1.0 - aa)
dd = (1.0 - bb) / 2.0

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

gammau(1,1) = 1.0
gammau(1,2) = - cc * (1.0 - emisr)
gammau(1,3) = - aa * (1.0 - emisr)
gammau(2,1) = - dd * (1.0 - emisw)
gammau(2,2) = 1.0  - ( bb * (1.0 - emisw) )
gammau(2,3) = - dd * (1.0 - emisw)
gammau(3,1) = - aa * (1.0 - emiss)
gammau(3,2) = - cc * (1.0 - emiss)
gammau(3,3) = 1.0

!------------------------------------------------------------------------------
CALL matinv(gammau,arraydim)

! gamma is then the view factors
!------------------------------------------------------------------------------

emisrratio = emisr/ (1.0 - emisr)
emiswratio = emisw/ (1.0 - emisw)

sigmalw = (emisrratio * gammau(1,3)) +                                         &
   (2.0 * hwr * emiswratio * gammau(2,3))
sigmat = emisrratio * ((gammau(1,1) * emisr) +                                 &
   (gammau(1,2) * emisw) - 1.0)
sigmat = sigmat + (2.0 * hwr * emiswratio *                                    &
   ((gammau(2,1) * emisr) + (gammau(2,2) * emisw) - 1.0))

IF ( ABS( sigmat + sigmalw ) > 1.0e-03 ) THEN
  WRITE(jules_message,*) 'Problem balancing emissivity',                       &
      ABS( sigmat + sigmalw )
  CALL jules_print('urbanemis',jules_message,level = PrNorm)
  WRITE(jules_message,'(a10,f8.3)') 'sigmat  = ',sigmat
  CALL jules_print('urbanemis',jules_message,level = PrNorm)
  WRITE(jules_message,'(a10,f8.3)') 'sigmalw = ',sigmalw
  CALL jules_print('urbanemis',jules_message,level = PrNorm)
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE urbanemis
END MODULE urbanemis_mod
