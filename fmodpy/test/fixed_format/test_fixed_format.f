      SUBROUTINE TEST_STANDARD(A,B,C,D,AA,AB,AC,AD,BA,BB)
* Comments.
*     Parameters
      INTEGER          PA,PB
      PARAMETER        (PA=0,PB=6)
*     .. Scalar Arguments
      INTEGER          A,B,C,D
*        Array Arguments ..
      INTEGER          AA(2,*),AB(3,*),AC(*),AD(*),BA(C,D),
     +                 BB(PB,*)
* Code.
      IF (A.NE.1) RETURN
      AA(2,3) = 1
      AB(2,3) = 2
      AC(2) = 3
      AD(2) = 4
      BA(2,3) = 5
      BB(2,3) = 6
      RETURN
      END
