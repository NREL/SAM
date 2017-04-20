      SUBROUTINE XMCLONG (NAME1, NEWLEN, NEWLOC, MYV, MYCHAR, MYLOC,
     &   MYCLOC, UCLOC, COFFST, OFFSET,
     &   DICT, DPOINT, LDICT, NNAMES, VOID, LVOID, NVOIDS,
     &   FILL, FDATA, CFILL, CFDATA, CHRNUM, CHRCOL, MAXSIZ, LASTER)
C
      IMPLICIT INTEGER (A-Z)
      INCLUDE 'SUPLIB_MEMORY_PARAMS.H'
C
C***********************************************************************
C
C     NAME1    Name of the vector which changes length
      CHARACTER*8 NAME1
C     NEWLEN   The new length of the vector (character units)
C     NEWLOC   The new location of the vector (returned)
C     MYV      Internal reference array
      DIMENSION MYV(*)
C     MYCHAR   Internal reference array.
      CHARACTER MYCHAR(*)
C     MYLOC    Address of internal array
C     MYCLOC   Address of internal character array.
C     UCLOC    Address of user's character array.
C     COFFST   Offset between internal numeric array and user's
C              character array.
C     OFFSET   Address offset from internal array to user's array
C     DICT     Dictionary name table
C     DPOINT   Dictionary pointer table
C     LDICT    Dimension of dictionary
C     NNAMES   Number of names in the dictionary
C     Redefine character into character*8 to be consistant with 
C     XMCFIND  C.S. 05/25/2010
      CHARACTER*8 DICT(LDICT,CHRCOL)
      DIMENSION DPOINT(LDICT,CHRCOL,3), NNAMES(2)
C     VOID     Void table
C     LVOID    Dimension of void table
C     NVOIDS   Number of voids
      DIMENSION VOID(LVOID,CHRCOL,2), NVOIDS(2)
C     FILL     Flag for data fill.
C     FDATA    Data for fill.
      LOGICAL FILL
C     CFILL    Flag for character data fill.
C     CFDATA   Data for fill.
      LOGICAL CFILL
      CHARACTER*1 CFDATA
C     CHRNUM   Number of characters per numeric storage unit
C     CHRCOL   Number of column for character names.
C     MAXSIZ   Dimension of static character array.
C     LASTER   Error return
C
C***********************************************************************
C
      INTLEN = (NEWLEN + CHRNUM - 1) / CHRNUM
C
C     Get current location and length.
C
      CALL XMCFIND (NAME1, DICT, DPOINT, LDICT, NNAMES,
     &   CHRCOL, LASTER, ROW)
      IF (LASTER .NE. SUCESS) RETURN
C
C     Save the current location of the array.
C
      OLDLOC = DPOINT(ROW,CHRCOL,1)
      OLDLEN = DPOINT(ROW,CHRCOL,2)
      OLDCL = DPOINT(ROW,CHRCOL,3)
      NEWLOC = (OLDLOC - 1) * CHRNUM + 1 + COFFST
      IF (NEWLEN .EQ. 0) THEN
C
C        Shrink to zero length move pointer to beginning of base vector.
C
         CALL XMVTABLE (DPOINT(ROW,CHRCOL,1), OLDLEN,
     &      VOID(1,CHRCOL,1), LVOID, NVOIDS(CHRCOL),
     &      CHRCOL, LASTER)
         IF (LASTER .NE. SUCESS) RETURN
         DPOINT(ROW,CHRCOL,1) = 1 - COFFST / CHRNUM
         DPOINT(ROW,CHRCOL,2) = 0
         DPOINT(ROW,CHRCOL,3) = 0
         NEWLOC = 1
         RETURN
      ELSE IF (INTLEN .LT. OLDLEN) THEN
C
C        Update void table.
C
         CALL XMVTABLE (DPOINT(ROW,CHRCOL,1)+INTLEN, OLDLEN-INTLEN,
     &      VOID(1,CHRCOL,1), LVOID, NVOIDS(CHRCOL), CHRCOL, LASTER)
         IF (LASTER .NE. SUCESS) RETURN
C
C        Shorten the vector.
C
         DPOINT(ROW,CHRCOL,2) = INTLEN
         DPOINT(ROW,CHRCOL,3) = NEWLEN
C
         RETURN
      ELSE IF (INTLEN .EQ. OLDLEN) THEN
C
C        No change in numeric length.
C
         IF (NEWLEN .GT. OLDCL .AND. CFILL) THEN
C
C           The character length is greater and fill is required.
C
            I1 = NEWLOC + UCLOC - MYCLOC + OLDCL
            I2 = I1 + NEWLEN - OLDCL - 1
            DO 100 I = I1, I2
               MYCHAR(I) = CFDATA
  100       CONTINUE
         END IF
         DPOINT(ROW,CHRCOL,3) = NEWLEN
         LASTER = SUCESS
         RETURN
      END IF
C
C     The new length is greater than the old length.
C
      ESPACE = 0
      DELTA = INTLEN - OLDLEN
C
C     Look for a null at the end of this vector.
C
      CALL XMSRCHI (VOID(1,CHRCOL,1), 1, NVOIDS(CHRCOL),
     &   OLDLOC+OLDLEN, ERR, VROW)
      IF (ERR .EQ. 1) THEN
C
C        There is a void at the end of the vector.
C
         ESPACE = VOID(VROW,CHRCOL,2)
         IF (ESPACE .GE. DELTA) THEN
C
C           There is enough room with this void.
C
C           Perform data fill if appropriate.
C
            IF (CFILL) THEN
               I1 = NEWLOC + UCLOC - MYCLOC + OLDCL
               I2 = I1 + NEWLEN - OLDCL - 1
               DO 110 I = I1, I2
                  MYCHAR(I) = CFDATA
  110          CONTINUE
            END IF
C
            VOID(VROW,CHRCOL,1) = VOID(VROW,CHRCOL,1) + DELTA
            VOID(VROW,CHRCOL,2) = ESPACE - DELTA
            DPOINT(ROW,CHRCOL,2) = INTLEN
            DPOINT(ROW,CHRCOL,3) = NEWLEN
            IF (VOID(VROW,CHRCOL,2) .EQ. 0) THEN
C
C              Adjust the void table.
C
               CALL XMVTABLE (0, 0, VOID(1,CHRCOL,1), LVOID,
     &            NVOIDS(CHRCOL), CHRCOL, LASTER)
            END IF
            RETURN
         END IF
      END IF
C
C     The most general way to allow voids to join and find a location
C     for the length request is to add the current vector to the void
C     table and then look for any void to fit the entire vector.  This
C     allows most efficient use of space adjoining the vector.
C
      CALL XMVTABLE (OLDLOC, OLDLEN, VOID(1,CHRCOL,1), LVOID,
     &   NVOIDS(CHRCOL), CHRCOL, LASTER)
      IF (LASTER .NE. SUCESS) RETURN
C
      DEFMEM = 0
      CALL XMDLOOK (INTLEN, VOID(1,CHRCOL,1), CHRCOL*LVOID,
     &   NVOIDS(CHRCOL), VROW, LASTER)
C
      IF (LASTER .NE. SUCESS) THEN
C
C        Since a system call for memory is required, calculate the
C        memory necessary for any deferred requests and get this
C        memory at the same time.
C
         DO 120 IDICT = 1, NNAMES(CHRCOL)
            DEFMEM = DEFMEM + MIN(0, DPOINT(IDICT,CHRCOL,2))
  120    CONTINUE
         DEFMEM = - DEFMEM
      END IF
C
      IF (CHRCOL .EQ. 2) THEN
         CALL XMCGET (MYCLOC, INTLEN+DEFMEM, VOID, LVOID, NVOIDS,
     &      CHRCOL, MAXSIZ, LASTER, VROW)
      ELSE
         CALL XMDGET (MYLOC, INTLEN+DEFMEM, VOID, LVOID, NVOIDS,
     &      CHRCOL, LASTER, VROW)
      END IF
C
      IF (LASTER .NE. SUCESS) THEN
C
C        The strategy failed, so reinstate the void table as before.
C
         SAVERR = LASTER
         CALL XMSRCHI (VOID(1,CHRCOL,1), 1, NVOIDS(CHRCOL),
     &      OLDLOC, ERR, VROW)
         IF (ERR .EQ. 1) THEN
C
C           The original space is now at a void boundary.
C
            VOID(VROW,CHRCOL,2) = VOID(VROW,CHRCOL,2) - OLDLEN
            VLOC = 0
            VLEN = 0
C
         ELSE
C
C           A void must be broken up.
C
            IF (VROW .EQ. 1) THEN
               LASTER = BDVOID
               RETURN
            END IF
C
            VLEN = VOID(VROW-1,CHRCOL,2)
            VOID(VROW-1,CHRCOL,2) = OLDLOC - VOID(VROW-1,CHRCOL,1)
            VLEN = VLEN - VOID(VROW-1,CHRCOL,2) - OLDLEN
            VLOC = OLDLOC + OLDLEN
         END IF
         CALL XMVTABLE (VLOC, VLEN, VOID(1,CHRCOL,1), LVOID,
     &      NVOIDS(CHRCOL), CHRCOL, LASTER)
         LASTER = SAVERR
         RETURN
C
      END IF
C
C     VROW now points to a usable void.
C
      IF (OLDCL .GT. 0) THEN
         I1 = NEWLOC + UCLOC - MYCLOC
         I2 = (VOID(VROW,CHRCOL,1) - 1) * CHRNUM + 1 + COFFST
     &      + UCLOC - MYCLOC
         CALL XMSHFTC(MYCHAR, 1, I1, I1+OLDCL-1, I1-I2)
      END IF
C
      DPOINT(ROW,CHRCOL,1) = VOID(VROW,CHRCOL,1)
      NEWLOC = (DPOINT(ROW,CHRCOL,1) - 1) * CHRNUM + 1 + COFFST
      DPOINT(ROW,CHRCOL,2) = INTLEN
      DPOINT(ROW,CHRCOL,3) = NEWLEN
      VOID(VROW,CHRCOL,1) = VOID(VROW,CHRCOL,1) + INTLEN
      VOID(VROW,CHRCOL,2) = VOID(VROW,CHRCOL,2) - INTLEN
C
C     Perform data fill if appropriate.
C
      IF (CFILL) THEN
         I1 = NEWLOC + UCLOC - MYCLOC + OLDCL
         I2 = I1 + NEWLEN - OLDCL - 1
         DO 130 I = I1, I2
            MYCHAR(I) = CFDATA
  130    CONTINUE
      END IF
C
      CALL XMVTABLE (0, 0, VOID(1,CHRCOL,1), LVOID,
     &   NVOIDS(CHRCOL), CHRCOL, LASTER)
      IF (LASTER .NE. SUCESS) RETURN
C
      IF (CHRCOL .EQ. 2) RETURN
C
      CALL XMDEXEC (MYV, MYCHAR, MYLOC, MYCLOC, UCLOC, COFFST,
     &   OFFSET, DPOINT, LDICT, NNAMES,
     &   VOID, LVOID, NVOIDS, FILL, FDATA, CFILL, CFDATA, CHRNUM,
     &   CHRCOL, LASTER)
C
      RETURN
      END
