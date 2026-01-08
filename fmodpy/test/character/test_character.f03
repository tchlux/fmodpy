
SUBROUTINE TEST_SIMPLE_CHARACTER(A,C,B)
  CHARACTER, INTENT(IN), DIMENSION(:) :: A
  CHARACTER, INTENT(IN), OPTIONAL :: C
  CHARACTER, INTENT(OUT), DIMENSION(SIZE(A)) :: B
  ! Local
  INTEGER :: I
  B(:) = 'A'
  DO I = 1, SIZE(B)
     IF (PRESENT(C) .AND. (C .EQ. '1')) THEN
        B(I) = A(I)
     ELSE
        B(I) = CHAR(MOD(I,3))
     END IF
  END DO
END SUBROUTINE TEST_SIMPLE_CHARACTER


SUBROUTINE TEST_STRING(A,B)
  CHARACTER(LEN=*), INTENT(IN) :: A
  CHARACTER(LEN=LEN(A)), INTENT(OUT) :: B
  INTEGER :: I
  DO I = 1, LEN(A)
     B(I:I) = A(I:I)
  END DO
END SUBROUTINE TEST_STRING


! python3 -c "import fmodpy as f; f.fimport('test_character.f03', build_dir='.', verbose=True)"
! python3 -c "import og_fmodpy as f; f.wrap('test_character.f03', working_directory='og_fmodpy', verbose=True)"
