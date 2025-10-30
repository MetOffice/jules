! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Theta field horizontal dimensions.

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.


MODULE theta_field_sizes

IMPLICIT NONE

INTEGER ::                                                                     &
  t_i_length,                                                                  &
                       !  Number of points on a row
  t_j_length,                                                                  &
                       !  Number of rows in a theta field
  u_i_length,                                                                  &
                       !  Number of points on a row
  u_j_length,                                                                  &
                       !  Number of rows in a u field
  v_i_length,                                                                  &
                       !  Number of points on a row
  v_j_length
                       !  Number of rows in a v field

END MODULE theta_field_sizes
