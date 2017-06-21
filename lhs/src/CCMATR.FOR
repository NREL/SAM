C     Last change:  SLD   5 Dec 96    2:28 pm
C     -- INCLUDE FILE FOR THE COMMON BLOCK CMATR
C     COMMON /CMATR/ CORR((NVAR*(NVAR+1))/2), LCM(NVAR), NCM
      MODULE CCMATR
C
C       These are the elements from the old common block
        REAL, ALLOCATABLE :: CORR(:)
        INTEGER, ALLOCATABLE :: LCM(:)
        INTEGER NCM
C
C       Now here is the initialization subroutine for this module
      CONTAINS
C
      SUBROUTINE CCMATR_INIT()
C
        USE PARMS
C
        ALLOCATE( CORR( (NVAR*(NVAR+1))/2 ) )
        CORR = 0.0
C
        ALLOCATE ( LCM( NVAR ) )
        LCM = 0
C
        RETURN
C
      END SUBROUTINE
C
      SUBROUTINE CCMATR_CLOSE()
C
        DEALLOCATE( CORR )
C
        DEALLOCATE ( LCM )
C
        RETURN
C
      END SUBROUTINE
C
      END MODULE
