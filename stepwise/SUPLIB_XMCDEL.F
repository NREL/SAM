      SUBROUTINE XMCDEL (NAME1, DICT, DPOINT, LDICT, NNAMES, VOID,
     &   LVOID, NVOIDS, CHRCOL, LASTER)
C
      IMPLICIT INTEGER (A-Z)
      INCLUDE 'SUPLIB_MEMORY_PARAMS.H'
C
C     This routine removes a name from the dictionary and returns the
C     available space to the void table.
C
C***********************************************************************
C
C     NAME1    Name to be deleted
      CHARACTER*8 NAME1
C     DICT     Dictionary name table
C     DPOINT   Dictionary pointer table
C     LDICT    Dimension of dictionary tables
C     NNAMES   Number of names in the dictionary
      CHARACTER*8 DICT(LDICT,CHRCOL)
      DIMENSION DPOINT(LDICT,CHRCOL,3), NNAMES(2)
C     VOID     Void table
C     LVOID    Dimension of void table
C     NVOIDS   Number of voids
      DIMENSION VOID(LVOID,CHRCOL,2), NVOIDS(2)
C     CHRCOL   Number of column for character names.
C     LASTER   Error return
C
C***********************************************************************
C
C     FIND NAME1 IN DICTIONARY.
C
      CALL XMCFIND (NAME1, DICT, DPOINT, LDICT, NNAMES,
     &   CHRCOL, LASTER, ROW)
      IF (LASTER .NE. SUCESS) RETURN
C
      LOC = DPOINT(ROW,CHRCOL,1)
      LEN = DPOINT(ROW,CHRCOL,2)
C
C     DELETE DICTIONARY ENTRY.
C
      CALL XMSHFTC (DICT(1,CHRCOL), CHRCOL*LDICT, ROW+1,
     &   NNAMES(CHRCOL), 1)
      CALL XMSHFTI (DPOINT(1,CHRCOL,1), CHRCOL*LDICT, 3, ROW+1,
     &   NNAMES(CHRCOL), 1)
      NNAMES(CHRCOL) = NNAMES(CHRCOL) - 1
      IF (LEN .LE. 0) RETURN
C
C     MAKE AN ENTRY IN THE VOID TABLE.
C
      CALL XMVTABLE (LOC, LEN, VOID(1,CHRCOL,1), LVOID, NVOIDS(CHRCOL),
     &   CHRCOL, LASTER)
      RETURN
      END
