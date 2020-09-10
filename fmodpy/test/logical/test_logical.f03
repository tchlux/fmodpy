
SUBROUTINE TEST_SIMPLE_LOGICAL(A,C,B)
  LOGICAL, INTENT(IN), DIMENSION(:) :: A
  LOGICAL, INTENT(IN), OPTIONAL :: C
  LOGICAL, INTENT(OUT), DIMENSION(SIZE(A)) :: B
  ! Local
  INTEGER :: I
  B(:) = .TRUE.
  DO I = 1, SIZE(B)
     IF (PRESENT(C) .AND. (C)) THEN
        B(I) = .FALSE.
     ELSE
        B(I) = (MOD(I,3) .EQ. 0)
     END IF
  END DO
END SUBROUTINE TEST_SIMPLE_LOGICAL

! python3 -c "import fmodpy as f; f.fimport('simple_logical.f03', build_dir='.', verbose=True)"
! python3 -c "import og_fmodpy as f; f.wrap('simple_logical.f03', working_directory='og_fmodpy', verbose=True)"
