MODULE canyonalb_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='CANYONALB_MOD'
CONTAINS
SUBROUTINE canyonalb( coszz, hwr, albwl, albrd, albcan )

! Description:
! Routine to calculate albedo of canyon tile for JULES-MORUSES
!
! (c) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! ------------------------------------------------------------------------------

USE matinv_mod, ONLY: matinv

USE jules_print_mgr, ONLY:                                                     &
  jules_message,                                                               &
  jules_print,                                                                 &
  PrNorm
USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

! Subroutine arguments with intent(in)
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
   coszz,            & !cosine of the solar zenith angle
   hwr,              & !canyon aspect ratio
   albwl,            & !albedo of wall (snow free)
   albrd               !albedo of road

!Subroutine arguments with intent(out)
REAL(KIND=real_jlslsm), INTENT(OUT) ::  albcan
                                          !tile albedo (snow affected)

!Internal array dimensions
INTEGER, PARAMETER :: arraydim = 3          !dimension of arrays RFINV

!Work variables - scalars
INTEGER :: i, j             !looping variables

REAL(KIND=real_jlslsm) :: cosz,             & !copy of COSZZ
   tanz,                  & !tan of the zenith angle
   omega0,                & !angle of canyon orientation for
                              !max shadowing
   chi_r,                 & !conversion factor over shadowed road
   chi_w,                 & !conversion facotr over shadowed wall
   aa,bb,cc,dd,           & !elements of reflections matrix
   pi

!work variables - arrays
REAL(KIND=real_jlslsm) :: sf(arraydim,arraydim),    & !array of shape factors
   rfinv(arraydim,arraydim), & !array for reflections and its inverse
   emis(arraydim),           & !array of emissions
   OUT(arraydim),            & !array of total outgoings
   inn(arraydim)               !array of total incomings

!Parameter - later to be a subroutine input
REAL(KIND=real_jlslsm), PARAMETER :: chi_f = 0.3
                                !rato of diffuse to total solar

LOGICAL, SAVE :: firstcall = .TRUE.

CHARACTER(LEN=*), PARAMETER :: RoutineName='CANYONALB'

!----------------------------------------------------------------------

! Stop if nighttime
IF ( coszz < 0.0 ) THEN ! No (significant) solar
  WRITE(jules_message,*) 'COSZZ <0', coszz
  CALL jules_print('canyonalb',jules_message)
  RETURN
ELSE IF ( coszz == 1.0 ) THEN
  cosz = 0.999
ELSE
  cosz = coszz
END IF

! Deal with obvious cases first - all albedos = 0. or 1.
IF ( albwl == 0.0 .AND. albrd == 0.0 ) THEN
  WRITE(jules_message,*) 'ALB EQ 0', albwl, albrd
  CALL jules_print('canyonalb',jules_message,level = PrNorm)
  RETURN

ELSE IF ( albwl == 1.0 .AND. albrd == 1.0 ) THEN
  WRITE(jules_message,*) 'ALB EQ 1', albwl, albrd
  CALL jules_print('canyonalb',jules_message,level = PrNorm)
  RETURN

  ! Otherwise reflections of radiation occur
ELSE
  pi = 4.0 * ATAN(1.0)
  tanz = TAN(ACOS(cosz))
  omega0 = 1.0 / (tanz * hwr)
  omega0 = MIN(omega0,1.0)
  omega0 = ASIN(omega0)
  chi_r = (2.0 / pi) * (omega0 - hwr * tanz * (1.0 - COS(omega0)))
  chi_w = (1.0 - chi_r) / (2.0 * hwr)     !note HWR = 0. not allowed

  !next setup reflections array - note ALBSK:=0.
  !hence RF(3,1) = RF(3,2) = 0.
  !index 1 for road, 2 for wall, 3 for sky
  aa = (1.0 + hwr**2.0)**(0.5) - hwr
  bb = (1.0 + (1.0 / hwr)**2.0)**(0.5) - (1.0 / hwr)
  cc = (1.0 - aa)
  dd = (1.0 - bb) / 2.0

  !first the shape factors
  sf(1,1) = 0.0
  sf(1,2) = cc
  sf(1,3) = aa
  sf(2,1) = dd
  sf(2,2) = bb
  sf(2,3) = dd
  sf(3,1) = aa
  sf(3,2) = cc
  sf(3,3) = 0.0
  !then the reflections array
  rfinv(1,1) = 1.0
  rfinv(1,2) = -albrd * cc
  rfinv(1,3) = -albrd * aa
  rfinv(2,1) = -albwl * dd
  rfinv(2,2) = 1.0 - albwl * bb
  rfinv(2,3) = -albwl * dd
  rfinv(3,1) = 0.0
  rfinv(3,2) = 0.0
  rfinv(3,3) = 1.0

  !Now calculate inverse of RF - should be a routine
  CALL matinv(rfinv,arraydim)

  !next combine ALBWL, ALBRD, RF and RFINV to get new albedo
  !first specify diffuse emitted fluxes.
  emis(1) = albrd * (1.0 - chi_f) * chi_r
  emis(2) = albwl * (1.0 - chi_f) * chi_w
  emis(3) = chi_f

  !next form total outgoings
  DO i = 1,arraydim
    OUT(i) = 0.0
    DO j = 1,arraydim
      OUT(i) = OUT(i) + rfinv(i,j) * emis(j)
    END DO
  END DO

  !next form total incomings
  DO i = 1,arraydim
    inn(i) = 0.0
    DO j = 1,arraydim
      inn(i) = inn(i) + sf(i,j) * OUT(j)
    END DO
  END DO
  !add the direct component to the incoming
  !note this is the full amount - the reflected direct
  !radiation is already accounted for within OUT
  !and also form net for each surface
  inn(1) = inn(1) + (1.0 - chi_f) * chi_r - OUT(1)
  inn(2) = inn(2) + (1.0 - chi_f) * chi_w - OUT(2)

  !form net for whole canyon and then albedo
  inn(1) = inn(1) + 2.0 * hwr * inn(2)
  IF (inn(1) >= 0 .AND. inn(1) <= 1.0) THEN
    albcan = 1.0 - inn(1)
  END IF
END IF

! This section should only be executed by one thread.
!$OMP MASTER
IF ( firstcall ) THEN
  CALL jules_print('canyonalb',                                                &
      'MORUSES: Altered urban canyon tile albedo for reflections',             &
      level = PrNorm)
  firstcall = .FALSE.
END IF
!$OMP END MASTER
RETURN

END SUBROUTINE canyonalb
END MODULE canyonalb_mod
