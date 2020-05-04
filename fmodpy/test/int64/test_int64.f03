! Test Fortran REAL wrapping and usage from Python with fmodpy.

SUBROUTINE TEST_STANDARD(SING_IN, SING_OUT, ARRAY_IN, ARRAY_OUT,&
     KNOWN_ARRAY_OUT, KNOWN_MATRIX_OUT, OPT_SING_IN, OPT_SING_OUT)
  ! Test the basic functionaly of the 'REAL' type and its
  ! interoperability with Python. This includes, inputs, outputs,
  ! array inputs with known and unknown size, optional inputs, and
  ! optional outputs. 
  USE ISO_FORTRAN_ENV, ONLY: INT64
  IMPLICIT NONE
  ! Argument definitions.
  INTEGER(KIND=INT64), INTENT(IN) :: SING_IN
  INTEGER(KIND=INT64), INTENT(OUT) :: SING_OUT
  INTEGER(KIND=INT64), DIMENSION(:), INTENT(IN) :: ARRAY_IN
  INTEGER(KIND=INT64), DIMENSION(:), INTENT(OUT) :: ARRAY_OUT
  INTEGER(KIND=INT64), DIMENSION(SIZE(ARRAY_OUT)), INTENT(OUT) :: KNOWN_ARRAY_OUT
  INTEGER(KIND=INT64), DIMENSION(3,SIZE(ARRAY_OUT)), INTENT(OUT) :: KNOWN_MATRIX_OUT
  INTEGER(KIND=INT64), INTENT(IN), OPTIONAL :: OPT_SING_IN
  INTEGER(KIND=INT64), INTENT(OUT), OPTIONAL :: OPT_SING_OUT
  ! Local variable.
  INTEGER :: I
  ! Copy the single input value to the single output value.
  SING_OUT = SING_IN + 1
  ! Copy as much of the input array as possible to the output array.
  ARRAY_OUT(1:MIN(SIZE(ARRAY_IN),SIZE(ARRAY_OUT))) = &
       &ARRAY_IN(1:MIN(SIZE(ARRAY_IN),SIZE(ARRAY_OUT)))
  DO I = MIN(SIZE(ARRAY_IN),SIZE(ARRAY_OUT))+1, SIZE(ARRAY_OUT)
     ARRAY_OUT(I) = I
  END DO
  DO I = 1, SIZE(KNOWN_MATRIX_OUT, 1)
     KNOWN_MATRIX_OUT(I,:) = I
  END DO
  ! Set the KNOWN_ARRAY and the KNOWN_MATRIX values to be identifiabl.
  DO I = 1,SIZE(ARRAY_OUT)
     KNOWN_ARRAY_OUT(I) = I
     KNOWN_MATRIX_OUT(:,I) = KNOWN_MATRIX_OUT(:,I) + I
  END DO
  ! Do some operations on the optional inputs / outputs.
  IF (PRESENT(OPT_SING_OUT)) THEN
     IF (PRESENT(OPT_SING_IN)) THEN
        OPT_SING_OUT = OPT_SING_IN
     ELSE
        OPT_SING_OUT = SING_IN
     END IF
  ELSE IF (PRESENT(OPT_SING_IN)) THEN
     SING_OUT = OPT_SING_IN
  END IF
  ! End of this subroutine.
END SUBROUTINE TEST_STANDARD


FUNCTION TEST_EXTENDED(OPT_ARRAY_IN, KNOWN_OPT_ARRAY_OUT,&
     & OPT_ALLOC_ARRAY_OUT, N ) RESULT(ALLOC_ARRAY_OUT)
  ! Test the extended functionaly of the 'REAL' type and its
  ! interoperability with Python. This includes, optional array
  ! inputs, optional array outputs, and allocatable array outputs.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG
  IMPLICIT NONE
  INTEGER(C_LONG), INTENT(IN), OPTIONAL, DIMENSION(:) :: OPT_ARRAY_IN
  INTEGER(C_LONG), INTENT(OUT), OPTIONAL :: KNOWN_OPT_ARRAY_OUT(3)
  INTEGER(C_LONG), INTENT(OUT), OPTIONAL, ALLOCATABLE :: OPT_ALLOC_ARRAY_OUT(:)
  INTEGER(C_LONG), DIMENSION(:), ALLOCATABLE :: ALLOC_ARRAY_OUT
  INTEGER, INTENT(IN) :: N
  ! Local variable.
  INTEGER :: I

  ! Assign the optional array output values.
  IF (PRESENT(KNOWN_OPT_ARRAY_OUT)) THEN
     IF (PRESENT(OPT_ARRAY_IN)) THEN
        DO I = 1, MIN(SIZE(OPT_ARRAY_IN), SIZE(KNOWN_OPT_ARRAY_OUT))
           KNOWN_OPT_ARRAY_OUT(I) = REAL(I)
        END DO
     ELSE
        DO I = 1, SIZE(KNOWN_OPT_ARRAY_OUT)
           KNOWN_OPT_ARRAY_OUT(I) = REAL(I)
        END DO
     END IF
  END IF

  ! Allocate the optional array output and assign its values.
  IF (PRESENT(OPT_ALLOC_ARRAY_OUT)) THEN
     ALLOCATE(OPT_ALLOC_ARRAY_OUT(1:N/2))
     DO I = 1, SIZE(OPT_ALLOC_ARRAY_OUT)
        OPT_ALLOC_ARRAY_OUT(I) = SIZE(OPT_ALLOC_ARRAY_OUT) - (I-1)
     END DO
  END IF
  
  ! Allocate the required array output to the specified size.
  ALLOCATE(ALLOC_ARRAY_OUT(1:N))
  DO I = 1, SIZE(ALLOC_ARRAY_OUT)
     ALLOC_ARRAY_OUT(I) = SIZE(ALLOC_ARRAY_OUT) - (I-1)
  END DO
  
  ! End of function.
END FUNCTION TEST_EXTENDED
