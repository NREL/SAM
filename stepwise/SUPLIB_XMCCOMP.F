      SUBROUTINE XMCCOMP (MYV, VOID, LVOID,
     &   NVOIDS, DPOINT, LDICT, NNAMES, CHRCOL, LASTER)
C
      IMPLICIT INTEGER (A-Z)
      INCLUDE 'SUPLIB_MEMORY_PARAMS.H'
C
C     THIS ROUTINE PERFORMS THE NUMERIC DATA COMPRESSION OPERATION.
C
C************************************************************************
C
C     MYV      Reference array
C     VOID     Void table
C     LVOID    Dimension of VOID
C     NVOIDS   Number of voids
C     DPOINT   Dictionary pointer table
C     LDICT    Dimension of DPOINT
C     NNAMES   Number of names
C     CHRCOL   Column for character tables
C     LASTER   Error return code
C
      DIMENSION DPOINT(LDICT,CHRCOL,2), VOID(LVOID,CHRCOL,2)
      DIMENSION NNAMES(2), NVOIDS(2)
      CHARACTER*1 MYV(1)
C
C************************************************************************
C
      LASTER = SUCESS
C
C     The basic strategy is to look for an array in the dictionary
C     which is immediately preceeded by a void.  If found, a shift
C     is performed, and the void table is updated.
C
      IVOID = 0
  100 CONTINUE
      IVOID = IVOID + 1
  110 CONTINUE
      IF (IVOID .GT. NVOIDS(2)) GOTO 130
      VADDR = VOID(IVOID,2,1) + VOID(IVOID,2,2)
      DO 120 IDICT = 1, NNAMES(2)
         DADDR = DPOINT(IDICT,2,1)
         IF (VADDR .EQ. DADDR .AND. DPOINT(IDICT,2,2) .GT. 0) THEN
C
C              Perform data shift and update void table.
C
            CALL XMSHFTC (MYV, LDICT,
     &         DADDR, DADDR+DPOINT(IDICT,2,2)-1, VOID(IVOID,2,2))
            DPOINT(IDICT,2,1) = VOID(IVOID,2,1)
            VOID(IVOID,2,1) = DPOINT(IDICT,2,1) + DPOINT(IDICT,2,2)
            CALL XMVTABLE (0, 0, VOID(1,2,1), LVOID, NVOIDS(2),
     &         CHRCOL, LASTER)
            IF (LASTER .NE. SUCESS) RETURN
            GOTO 110
C
         END IF
  120 CONTINUE
      GOTO 100
  130 CONTINUE
      RETURN
      END
