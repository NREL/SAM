      SUBROUTINE XMDRSRV (MYV, NAME1, NEWLEN, NEWLOC, MYLOC, OFFSET,
     &   VOID, LVOID,
     &   NVOIDS, DICT, DPOINT, LDICT, NNAMES, CHRCOL,
     &   DEFER, FILL, FDATA,
     &   LASTER)
C
      IMPLICIT INTEGER (A-Z)
      INCLUDE 'SUPLIB_MEMORY_PARAMS.H'
C
C     This routine finds space to service a non-negative space request.
C     If zero space is requested, a valid pointer of 1 will be
C     generated.
C
C***********************************************************************
C
C     MYV      Internal reference array.
      DIMENSION MYV(*)
C     NAME1    Name to be inserted in the dictionary
      CHARACTER*8 NAME1
C     NEWLEN   Length of requested storage
C     NEWLOC   Pointer of new storage (returned)
C     MYLOC    Reference address of internal array
C     OFFSET   Offset between internal array and user's array
C     VOID     Void table
C     LVOID    Dimension of void table
C     NVOIDS   Number of voids
      DIMENSION VOID(LVOID,CHRCOL,2), NVOIDS(2)
C     DICT     Dictionary name table
C     DPOINT   Dictionary pointer table
C     LDICT    Dimension of dictionary tables
C     NNAMES   Number of names
C     Redefine character into character*8 to be consistant with 
C     XMCFIND  C.S. 05/25/2010
      CHARACTER*8 DICT(LDICT,CHRCOL)
      DIMENSION DPOINT(LDICT,CHRCOL,3), NNAMES(2)
C     CHRCOL   Number of column for character names.
C     DEFER    Flag for deferred mode.
      LOGICAL DEFER
C     FILL     Flag for data fill.
C     FDATA    Data for fill.
      LOGICAL FILL
C     LASTER   Error return
C
C***********************************************************************
C
      LASTER = SUCESS
      MYLEN = NEWLEN
C
      IF (NEWLEN .EQ. 0) THEN
C
C        Zero length entry.
C
         NEWLOC = 1 - OFFSET
C
      ELSE IF (DEFER) THEN
C
         CALL XMDLOOK (MYLEN, VOID, CHRCOL*LVOID, NVOIDS(1),
     &      VROW, LASTER)
C
         IF (LASTER .EQ. SUCESS) THEN
            NEWLOC = VOID(VROW,1,1)
         ELSE IF (LASTER .EQ. NOGET) THEN
C
C           A good void was not found - defer the space request.
C
            NEWLOC = IXLNUM(NEWLOC)
            MYLEN = - NEWLEN
            LASTER = SUCESS
C
         END IF
C
      ELSE
C
C        Get space.
C
         CALL XMDGET (MYLOC, MYLEN, VOID, LVOID, NVOIDS,
     &      CHRCOL, LASTER, VROW)
         IF (LASTER .NE. SUCESS) RETURN
C
         NEWLOC = VOID(VROW,1,1)
C
      END IF
C
C     Update dictionary.
C
      CALL XMDNSRT (NAME1, NEWLOC, MYLEN, DICT, DPOINT, LDICT, 
     &   NNAMES, CHRCOL, LASTER)
      IF (LASTER .EQ. WRTYPE) LASTER = BDNAME
      IF (LASTER .NE. SUCESS) RETURN
C
      IF (MYLEN .GT. 0) THEN
C
C        Data fill pattern.
C
         IF (FILL) THEN
            DO 100 I = VOID(VROW,1,1), VOID(VROW,1,1)+MYLEN-1
               MYV(I) = FDATA
  100       CONTINUE
         END IF
C
C        Update void table.
C
         VOID(VROW,1,1) = VOID(VROW,1,1) + MYLEN
         VOID(VROW,1,2) = VOID(VROW,1,2) - MYLEN
         CALL XMVTABLE (1, 0, VOID, LVOID, NVOIDS(1), CHRCOL, LASTER)
         NEWLOC = NEWLOC + OFFSET
C
      ELSE IF (MYLEN .LT. 0) THEN
C
         NEWLOC = OFFSET - MYLOC
C
      END IF
C
      RETURN
      END
