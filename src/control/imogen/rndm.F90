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

SUBROUTINE rndm(random_num,seed)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This routine calculates the random behaviour. Following the ITE
!   model, the a random number generator is not called - instead,
!   seed(1),..,seed(4) is updated.
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

REAL :: random_num
          !OUT The random number.

INTEGER ::                                                                     &
  seed(4),                                                                     &
          !IN/OUT The seeding numbers
  i       !WORK Integer



seed(4) = 3 * seed(4) + seed(2)
seed(3) = 3 * seed(3) + seed(1)
seed(2) = 3 * seed(2)
seed(1) = 3 * seed(1)

i = INT(seed(1) / 1000.0)
seed(1) = seed(1) - i * 1000
seed(2) = seed(2) + i

i = INT(seed(2) / 100.0)
seed(2) = seed(2) - 100 * i
seed(3) = seed(3) + i

i = INT(seed(3) / 1000.0)
seed(3) = seed(3) - i * 1000
seed(4) = seed(4) + i

i = INT(seed(4) / 100.0)
seed(4) = seed(4) - 100 * i

random_num = (((REAL(seed(1)) * 0.001 + REAL(seed(2)))                         &
              * 0.01 + REAL(seed(3))) * 0.001                                  &
              + REAL(seed(4))) * 0.01

RETURN

END SUBROUTINE rndm
#endif
