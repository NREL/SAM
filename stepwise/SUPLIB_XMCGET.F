      SUBROUTINE XMCGET (MYLOC, MNGET, VOID, LVOID, NVOIDS,
     &   CHRCOL, MAXSIZ, LASTER, VROW)
C
      IMPLICIT INTEGER (A-Z)
      INCLUDE 'SUPLIB_MEMORY_PARAMS.H'
C
C     This subroutine returns the location (row number) of a void with
C     sufficient space for the memory request.  If necessary, memory is
C     requested from the system.  The memory is contiguous.
C     This routine is to be used only if CHRCOL = 2.
C
C
C***********************************************************************
C
C     MYLOC    Address of internal reference array
C     MNGET    Memory request in character storage units
C     VOID     Void table
C     LVOID    Dimension of void table
C     NVOIDS   Number of voids
      DIMENSION VOID(LVOID,CHRCOL,2), NVOIDS(2)
C     CHRCOL   Column for character tables (must be 2)
C     MAXSIZ   Dimension of static character array.
C     LASTER   Error return
C     VROW     Row number of void which satisfies the memory request
C
C***********************************************************************
C
C     IS THE MEMORY REQUEST SENSIBLE?
C
      IF (MNGET .LT. 0) THEN
         LASTER = BADLEN
         RETURN
      ELSE IF (MNGET .EQ. 0) THEN
         LASTER = SUCESS
         RETURN
      END IF
C
      CALL XMDLOOK (MNGET, VOID(1,CHRCOL,1), CHRCOL*LVOID,
     &   NVOIDS(CHRCOL), VROW, LASTER)
      IF (LASTER .EQ. SUCESS) RETURN
C
C     CALL EXTENSION LIBRARY ROUTINE TO GET SPACE FROM SYSTEM.
C
      CALL XMCMEMY (MNGET, LOC, MEMRET, MAXSIZ)
      LOC = LOC - MYLOC + 1
C
      IF (MEMRET .LT. 0) THEN
C
C        ILLEGAL MEMORY BLOCK SIZE.
C
         LASTER = ILBLK
         RETURN
C
      END IF
C
C     UPDATE VOID TABLE.
C
      CALL XMVTABLE (LOC, MEMRET, VOID(1,CHRCOL,1), LVOID,
     &   NVOIDS(CHRCOL), CHRCOL, LASTER)
      IF (LASTER .NE. SUCESS) RETURN
C
      CALL XMDLOOK (MNGET, VOID(1,CHRCOL,1), CHRCOL*LVOID,
     &   NVOIDS(CHRCOL), VROW, LASTER)
C
      RETURN
      END
