C     Last change:  SLD   5 Dec 96    2:26 pm
C     -- INCLUDE BLOCK FOR THE COMMON BLOCK WORKX
C      COMMON/WORKX/XX(2*MAXTB)
C
C===============================================================
C
      MODULE CWORKX
C
C       Here are the elements from the old common block
C
        REAL, ALLOCATABLE :: XX(:)
C
C       Since there are equivalence statements in the code, and 
C       equivalence can't be used with allocatable arrays, we will
C       define additional arrays here to eliminate the equivalence
C       statements.  The XVLZ array will be handled as an alias of XX.
C
        REAL, ALLOCATABLE :: XTABLE(:,:)
        REAL, ALLOCATABLE :: PRBZ(:)
C
C       Now here is the initialization routine for this module
      CONTAINS
C
      SUBROUTINE CWORKX_INIT()
C
        USE PARMS
C
        ALLOCATE( XX(2*MAXTB) )
        XX = 0.0
C
        ALLOCATE( XTABLE(MAXTB,2) )
        XTABLE = 0.0
C
        ALLOCATE( PRBZ(MAXTB) )
        PRBZ = 0.0
C
        RETURN
C
      END SUBROUTINE
C
      SUBROUTINE CWORKX_CLOSE()
C
        DEALLOCATE( XX )
C
        DEALLOCATE( XTABLE )
C
        DEALLOCATE( PRBZ )
C
        RETURN
C
      END SUBROUTINE
C
      END MODULE
C
