SUBROUTINE TEST_STANDARD(QUADRATIC, STEPS, SOLUTION, MAX_WINDOW)
  IMPLICIT NONE
  ! Minimize a quadratic by taking random 
  INTEGER, INTENT(IN) :: STEPS
  REAL, INTENT(INOUT) :: SOLUTION
  REAL, INTENT(IN), OPTIONAL :: MAX_WINDOW
  INTERFACE
     FUNCTION QUADRATIC(X) RESULT(Y)
       REAL, INTENT(IN) :: X
       REAL :: Y
     END FUNCTION QUADRATIC
  END INTERFACE
  ! Local variables.
  REAL :: DEFAULT_WINDOW
  REAL :: W, MOVEMENT, QVALUE, UVALUE
  INTEGER :: I
  ! Set the default window.
  IF (PRESENT(MAX_WINDOW)) THEN ; DEFAULT_WINDOW = MAX_WINDOW
  ELSE                          ; DEFAULT_WINDOW = 8.0
  END IF
  ! Initialize the value of the function.
  QVALUE = QUADRATIC(SOLUTION)
  W = DEFAULT_WINDOW
  ! Search randomly.
  DO I = 1, STEPS
     ! Generate a random movement.
     CALL RANDOM_NUMBER(MOVEMENT)
     MOVEMENT = W * (MOVEMENT - 0.5)
     UVALUE = QUADRATIC(SOLUTION+MOVEMENT)
     ! If this is better, take the step and reset the search window.
     IF (UVALUE < QVALUE) THEN
        SOLUTION = SOLUTION + MOVEMENT
        QVALUE = UVALUE
        W = DEFAULT_WINDOW
     ! This is not better, shrink the search window (increase likely of success).
     ELSE ;  W = W / 2.0
     END IF
  END DO
END SUBROUTINE TEST_STANDARD
