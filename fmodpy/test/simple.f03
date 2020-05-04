SUBROUTINE TEST(N, OUT)
  ! Simple allocation test, allocate a real array of size N.
  INTEGER, INTENT(IN) :: N
  REAL, INTENT(OUT), ALLOCATABLE :: OUT(:)
  ! Local variables.
  INTEGER :: I
  ALLOCATE(OUT(1:I))
  DO I = 1, N
     OUT(I) = I
  END DO
END SUBROUTINE TEST
