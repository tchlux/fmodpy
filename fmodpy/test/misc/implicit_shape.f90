
PURE FUNCTION TEST_SHAPES(a) RESULT(b)
  REAL, INTENT(IN), DIMENSION(:,:) :: a
  INTEGER :: b(SIZE(a,1), SIZE(a(1,:)))
  ! INTEGER :: b(SIZE(a,1), SIZE(a(1,3:4)))
  b(:,:) = INT(a(:,:))
END FUNCTION TEST_SHAPES
