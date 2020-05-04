! This file contains a single test module. It also demonstrates basic
! Fortran syntax and conveys some expectations made by `fmodpy` when
! parsing code.


! Documentation for the module "TEST" can go in a comment immediately
! before the module itself.
MODULE SIMPLE_TEST
  ! Documentation can also go immediately inside the module, this will
  ! be included in addition to preceding comments if both exist.
  USE ISO_FORTRAN_ENV, ONLY: REAL64, INT64

  PUBLIC ! <- This is a public-default module.
  PRIVATE :: USELESS

CONTAINS

  ! Documentation for the function "ADD" can go immediately before the
  ! subroutine. This subroutine takes three arguments that would be
  ! considered "pass by reference" in C. Only arguments with INTENT(OUT)
  ! are allowed to be modified by the body code (enforced by compilers).
  SUBROUTINE ADD(ADD_A, ADD_B, ADD_C)
    ! Sometimes documentation goes on the inside too! When it does,
    ! both the outer and inner documentation will be preserved.
    REAL(KIND=REAL64), INTENT(IN) :: ADD_A, ADD_B
    REAL(KIND=REAL64), INTENT(OUT) :: ADD_C
    ! Comments in the middle of routines will be ignored.
    ADD_C = ADD_A + ADD_B ! Comments after lines of code will be ignored.
  END SUBROUTINE ADD


  ! Comments between routines will be ignored, unless they
  ! contiguously run into a FUNCTION of SUBROUTINE (at which point
  ! they're considered documentation for the proceeding code.


  FUNCTION TAUTOLOGY() RESULT(YES) ! These functions have no documentation...
    LOGICAL :: YES
    YES = .TRUE.
  END FUNCTION TAUTOLOGY
  FUNCTION FALSEHOOD()
    LOGICAL :: FALSEHOOD
    FALSEHOOD = .FALSE.
  END FUNCTION FALSEHOOD

  SUBROUTINE USELESS()
  END SUBROUTINE USELESS

  ! Documentation for the function "SUBTRACT". This routine is using
  ! the default "REAL" precision (usually 32 bits).
  FUNCTION SUBTRACT(SUB_A, SUB_B) RESULT(SUB_C)
    REAL, INTENT(IN) :: SUB_A, SUB_B
    REAL :: SUB_C ! Notice that INTENT is not declared for a RESULT.
    SUB_C = SUB_A - SUB_B
  END FUNCTION SUBTRACT


  FUNCTION MULTIPLY(MULT_A, MULT_B)
    ! Documentation for the function "MULTIPLY". Lots of (mathematical)
    ! software strictly has documentation inside the FUNCTION / SUBROUTINE.
    REAL, INTENT(IN) :: MULT_A
    INTEGER(KIND=INT64), INTENT(IN) :: MULT_B ! Use 64 big integer instead of default.
    REAL :: MULTIPLY ! Notice that "INTENT" cannot be declared for the
                     ! function result (defaults to same name as function).
    ! All declarations must happenn at the top of the FUNCTION / SUBROUTINE.
    INTEGER :: I
    ! Comments in the middle of routines will be ignored.
    MULTIPLY = 0.0
    DO I = 1, MULT_B
       MULTIPLY = MULTIPLY + MULT_A
    END DO
  END FUNCTION MULTIPLY


END MODULE SIMPLE_TEST

