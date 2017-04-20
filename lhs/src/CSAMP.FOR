C     Last change:  SLD   5 Dec 96    2:24 pm
C     -- INCLUDE FILE FOR THE COMMON BLOCK SAMP
C      COMMON /SAMP/ X(MAXNNV), XSAVE(MAXNNV)
C
c===============================================================
C
      MODULE CSAMP
C
C       Here are the elements from the old common block
C
      REAL, ALLOCATABLE :: X(:), XSAVE(:)
C
C       Now here is the initialization routine for this module
      CONTAINS
C
      SUBROUTINE CSAMP_INIT()
C
        USE PARMS
C
        ALLOCATE( X(MAXNNV) )
        X = 0.0
C
        ALLOCATE( XSAVE(MAXNNV) )
        XSAVE = 0.0
C
        RETURN
C
      END SUBROUTINE
C
      SUBROUTINE CSAMP_CLOSE()
C
        DEALLOCATE( X )
C
        DEALLOCATE( XSAVE )
C
        RETURN
C
      END SUBROUTINE
C
      END MODULE
C
