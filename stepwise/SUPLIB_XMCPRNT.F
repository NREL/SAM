      SUBROUTINE XMCPRNT (NAME1, UNIT, MYCV, OFFSET, TOFFST,
     &   DICT, DPOINT, LDICT, NNAMES, CHRNUM,
     &   CHRCOL, NCOLP, WRDSIZ, LASTER)
C
      IMPLICIT INTEGER (A-Z)
      INCLUDE 'SUPLIB_MEMORY_PARAMS.H'
C
C***********************************************************************
C
C     NAME1    Name of array to be printed
      CHARACTER*8 NAME1
C     UNIT     Output unit number.
C     MYCV     Internal character array
      CHARACTER*1 MYCV(*)
C     OFFSET   Offset between internal reference and users reference
C              string.
C     TOFFST   Offset between internal reference and internal character
C              array.
C     DICT     Dictionary name table
C     DPOINT   Dictionary pointer table
C     LDICT    Dimension of dictionary
C     NNAMES   Number of names in the dictionary
      CHARACTER*8 DICT(LDICT,CHRCOL)
      DIMENSION DPOINT(LDICT,CHRCOL,3), NNAMES(2)
C     CHRCOL   Number of column for character names.
C     NCOLP    Number of print columns
C     WRDSIZ   Number of characters to group together in printing.
C     LASTER   Error return
C
C***********************************************************************
C
C
C     Check worklength
C
      IF (WRDSIZ .LT. 1 .OR. WRDSIZ+2+11 .GT. NCOLP) THEN
         LASTER = BADLEN
         RETURN
      END IF
C     FIND NAME1 IN DICTIONARY
C
      CALL XMCFIND (NAME1, DICT, DPOINT, LDICT, NNAMES,
     &   CHRCOL, LASTER, ROW)
      IF (LASTER .NE. SUCESS) RETURN
      DELTA = (DPOINT(ROW,CHRCOL,1) - 1) * CHRNUM + 1 + OFFSET
      WRITE (UNIT,10000) DICT(ROW,CHRCOL),
     &   DELTA,
     &   DPOINT(ROW,CHRCOL,3)
      IF (DPOINT(ROW,CHRCOL,2) .LT. 0) THEN
         LASTER = DEFRON
         WRITE (UNIT,10020) 'MCPRNT',
     &      NAME1, ' was reserved in the deferred mode'
     &      // ' and is not yet resolved'
         RETURN
      END IF
C
      DELTA = (DPOINT(ROW,CHRCOL,1) - 1) * CHRNUM + TOFFST
      NCOL = INT ((NCOLP - 8) / (WRDSIZ + 2))
      NCOL = MIN (NCOL, 132)
      NNUM = INT ((DPOINT(ROW,CHRCOL,3) + WRDSIZ-1) / WRDSIZ)
      DO 100 ISTART=1,NNUM,NCOL
         IEND=MIN(NNUM,ISTART+NCOL-1)
         WRITE (UNIT,10010) ISTART, (' ', ' ',
     &      (MYCV(DELTA+K), K=WRDSIZ*(IWRD-1)+1,
     &      MIN(WRDSIZ*IWRD,DPOINT(ROW,CHRCOL,3))),
     &      IWRD=ISTART,IEND)
  100 CONTINUE
C
      LASTER = SUCESS
      RETURN
10000  FORMAT(/1X,'Array Name = ',A,
     &   4X,'Location = ',I8,4X,'Length =',I8)
10010  FORMAT(1X,I6,':',132A1)
10020  FORMAT (/1X, '%%% CAMSUPES_LIB error in ', A, :, ' %%% ', 5A)
      END
