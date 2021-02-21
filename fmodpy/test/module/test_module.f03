! This file contains a few modules, only ADDITION is relevant.

MODULE NOTHING
END MODULE NOTHING


! This file contains a single module ADDITION.
MODULE CONSTANT
  REAL, PARAMETER :: C = 1.0 ! Comment should not mess this up
  INTEGER, PARAMETER :: D = 2, E = 3 ! Comment should not mess this up
END MODULE CONSTANT


! This module tests out private and public attributes and subroutines.
MODULE ADDITION
  USE EXTRA_CONSTANT, ONLY: B

  PRIVATE :: A_PR
  PRIVATE B_PR, NOTHING_PR
  REAL :: A_PR
  INTEGER :: B_PR

  PUBLIC
  REAL :: A_PUB
  INTEGER :: B_PUB
  REAL, DIMENSION(10) :: A_VEC_PUB
  REAL, DIMENSION(:), ALLOCATABLE :: B_VEC_PUB
  REAL, DIMENSION(:,:), ALLOCATABLE :: C_MAT_PUB

CONTAINS

  SUBROUTINE NOTHING_PR()
    ! Private subroutine that does nothing.
  END SUBROUTINE NOTHING_PR

  SUBROUTINE ADD_AB(C)
    ! Compute C = A_PUB + B_PUB.
    REAL, INTENT(OUT) :: C
    C = A_PUB + REAL(B_PUB)
  END SUBROUTINE ADD_AB

  SUBROUTINE SUB_VECS(C_VEC)
    ! Compute C_VEC = B_VEC_PUB - A_VEC_PUB.
    REAL, INTENT(OUT), DIMENSION(SIZE(A_VEC_PUB)) :: C_VEC
    IF (ALLOCATED(B_VEC_PUB)) THEN
       C_VEC(:) = B_VEC_PUB(:) - A_VEC_PUB(:)
    ELSE
       C_VEC(:) = -A_VEC_PUB(:)
    END IF
  END SUBROUTINE SUB_VECS

  SUBROUTINE RETURN_ALLOC(A)
    REAL, INTENT(OUT), DIMENSION(:), ALLOCATABLE :: A
  END SUBROUTINE RETURN_ALLOC

END MODULE ADDITION
