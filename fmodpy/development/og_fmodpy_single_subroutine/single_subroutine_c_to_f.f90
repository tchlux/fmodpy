!     Module generated by fmodpy for fortran file named 'SINGLE_SUBROUTINE'     
!=====================================================================
MODULE SINGLE_SUBROUTINE_WRAPPER
  ! Any necessary "USE" statements
  USE ISO_FORTRAN_ENV
  ! Make sure it is clear that no implicit types are used in this wrapper
  IMPLICIT NONE
  ! Any interfaces defined for "PROCEDURE" declarations
  
    
  INTERFACE
     ! Fortran interface for passing assumed shapes to non-moduled functions.
     SUBROUTINE SAY_HELLO( A, B, C, D )
       USE ISO_FORTRAN_ENV
       INTEGER(KIND=INT64), INTENT(IN) :: A
       REAL(KIND=REAL64), INTENT(IN) :: B
       INTEGER, INTENT(OUT) :: C
       REAL, INTENT(OUT) :: D
     END SUBROUTINE SAY_HELLO
  END INTERFACE
    
    

CONTAINS
  !     Fortran wrapper for SAY_HELLO, callable from C     
  !========================================================

   SUBROUTINE c_SAY_HELLO( A, B, C, D ) BIND(c)
    ! This subroutine's only purpose is to call the source fortran
    !  code while also being accessible from C.
    INTEGER(KIND=INT64), INTENT(IN) :: A
    REAL(KIND=REAL64), INTENT(IN) :: B
    INTEGER, INTENT(OUT) :: C
    REAL, INTENT(OUT) :: D
    ! Preparation code before function call (copying character arrays)
    
    ! Calling source fortran function
    CALL SAY_HELLO(A, B, C, D)
    ! Copying out any necessary values for return (character arrays)
    
  END SUBROUTINE c_SAY_HELLO

END MODULE SINGLE_SUBROUTINE_WRAPPER
