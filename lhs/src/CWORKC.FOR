C     Last change:  SLD   5 Dec 96    2:25 pm
C     -- INCLUDE BLOCK FOR THE COMMON BLOCK WORKC
C      COMMON/WORKC/Q((NVAR*(NVAR+1))/2),S((NVAR*(NVAR+1))/2)
C
C==============================================================
C
      MODULE CWORKC
C
C       Here are the elements from the old common block
C
      REAL, ALLOCATABLE :: Q(:), S(:)
C
C       Now here is the initialization routine for this module
      CONTAINS
C
      SUBROUTINE CWORKC_INIT()
C
        USE PARMS
C
        ALLOCATE( Q((NVAR*(NVAR+1))/2) )
        Q = 0.0
C
        ALLOCATE( S((NVAR*(NVAR+1))/2) )
        S = 0.0
C
        RETURN
C
      END SUBROUTINE
C
      SUBROUTINE CWORKC_CLOSE()
C
        DEALLOCATE( Q )
C
        DEALLOCATE( S )
C
        RETURN
C
      END SUBROUTINE
C
      END MODULE
C
