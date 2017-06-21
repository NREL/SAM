C     Last change:  SLD   5 Dec 96    2:27 pm
C     -- INCLUDE FILE FOR THE COMMON BLOCK RANK
C      COMMON /RANK/ XV(NMAX), RXV(NMAX), IWK(NMAX)
C
c===============================================================
C
      MODULE CRANK
C
C       Here are the elements from the old common block
C
        REAL, ALLOCATABLE :: XV(:), RXV(:)
        INTEGER, ALLOCATABLE :: IWK(:)
C
C       Now here is the initialization routine for this module
      CONTAINS
C
      SUBROUTINE CRANK_INIT()
C
        USE PARMS
C
        ALLOCATE( XV(NMAX) )
        XV = 0.0
C
        ALLOCATE( RXV(NMAX) ) 
        RXV = 0.0
C
        ALLOCATE( IWK(NMAX) )
        IWK = 0
C
        RETURN
C
      END SUBROUTINE
C
      SUBROUTINE CRANK_CLOSE()
C
        DEALLOCATE( XV )
C
        DEALLOCATE( RXV )
C
        DEALLOCATE( IWK )
C
        RETURN
C
      END SUBROUTINE
C
      END MODULE
C
