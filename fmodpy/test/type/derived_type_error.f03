! Define a simple subroutine that copies a derived type from A to B.
SUBROUTINE COPY_TYPE(N, A, B)
  IMPLICIT NONE
  TYPE :: TP(NX)
     INTEGER, LEN :: NX
     REAL, DIMENSION(NX) :: X
  END TYPE TP
  INTEGER, INTENT(IN) :: N
  TYPE(TP(N)), INTENT(IN)  :: A
  TYPE(TP(N)), INTENT(OUT) :: B
  B = A
END SUBROUTINE COPY_TYPE

! Run a program that tests derived type functionality.
PROGRAM TEST_DERIVED_TYPES
  IMPLICIT NONE
  ! Configuration.
  TYPE :: TP(NX)
     INTEGER, LEN :: NX
     REAL, DIMENSION(NX) :: X
  END TYPE TP
  INTEGER, PARAMETER :: N = 10
  ! Locals for testing.
  INTEGER :: I
  TYPE(TP(N)) :: A
  TYPE(TP(N)) :: B

  ! -----------------------------------------------------------------
  ! ! Define an interface to the externally defined subroutine above.
  ! INTERFACE
  !    SUBROUTINE COPY_TYPE(N, A, B)
  !      IMPLICIT NONE
  !      TYPE :: TP(NX)
  !         INTEGER, LEN :: NX
  !         REAL, DIMENSION(NX) :: X
  !      END TYPE TP
  !      INTEGER, INTENT(IN) :: N
  !      TYPE(TP(N)), INTENT(IN)  :: A
  !      TYPE(TP(N)), INTENT(OUT) :: B
  !    END SUBROUTINE COPY_TYPE
  ! END INTERFACE

  EXTERNAL :: COPY_TYPE
  ! -----------------------------------------------------------------

  ! Initialize A.
  DO I = 1, N
     A%X(I) = I
  END DO
  ! Call the test-running procedure.
  CALL COPY_TYPE(N, A, B)

END PROGRAM TEST_DERIVED_TYPES
