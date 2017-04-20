      SUBROUTINE XMDPRNT (NAME1, UNIT, NAME2, MYV, RMYV, OFFSET,
     &   DICT, DPOINT, LDICT, NNAMES, CHRCOL, NCOLP, LASTER)
C
      IMPLICIT INTEGER (A-Z)
      INCLUDE 'SUPLIB_MEMORY_PARAMS.H'
C
C***********************************************************************
C
C     NAME1    Name of array to be printed
      CHARACTER*8 NAME1
C     UNIT     Output unit number.
C     NAME2    Type of array to be printed
      CHARACTER*(*) NAME2
C     MYV      Internal integer array
      INTEGER MYV(*)
C     RMYV     Internal real array
      REAL RMYV(*)
C     DICT     Dictionary name table
C     DPOINT   Dictionary pointer table
C     LDICT    Dimension of dictionary
C     NNAMES   Number of names in the dictionary
      CHARACTER*8 DICT(LDICT,CHRCOL)
      DIMENSION DPOINT(LDICT,CHRCOL,2), NNAMES(2)
C     CHRCOL   Number of column for character names.
C     NCOLP    Number of print columns
C     LASTER   Error return
C
C***********************************************************************
C
C     FIND NAME1 IN DICTIONARY
C
      CALL XMDFIND (NAME1, DICT, DPOINT, LDICT, NNAMES,
     &   CHRCOL, LASTER, ROW)
      IF (LASTER .NE. SUCESS) RETURN
      DELTA = DPOINT(ROW,1,1) - 1
      WRITE (UNIT,10000) DICT(ROW,1), DPOINT(ROW,1,1)+OFFSET,
     &   DPOINT(ROW,1,2)
      IF (DPOINT(ROW,1,2) .LT. 0) THEN
         LASTER = DEFRON
         WRITE (UNIT,10030) 'MDPRNT',
     &      NAME1, ' was reserved in the deferred mode'
     &      // ' and is not yet resolved'
         RETURN
      END IF
      IF (NAME2(1:1) .EQ. 'R') THEN
C
C        VECTOR IS REAL
C
         NCOL=INT((NCOLP-8)/12)
         NCOL=MIN(NCOL,20)
         NNUM=DPOINT(ROW,1,2)
         DO 100 ISTART=1,NNUM,NCOL
            IEND=MIN(NNUM,ISTART+NCOL-1)
            WRITE (UNIT,10010) ISTART, (RMYV(DELTA+K), K=ISTART,IEND)
  100    CONTINUE
C
      ELSE IF (NAME2(1:1) .EQ. 'I') THEN
C
C        VECTOR IS INTEGER
C
         NCOL=INT((NCOLP-8)/10)
         NCOL=MIN(NCOL,20)
         NNUM=DPOINT(ROW,1,2)
         DO 110 ISTART=1,NNUM,NCOL
            IEND=MIN(NNUM,ISTART+NCOL-1)
            WRITE (UNIT,10020) ISTART, (MYV(DELTA+K), K=ISTART,IEND)
  110    CONTINUE
C
      ELSE
C
C        TYPE IS UNKNOWN
C
         LASTER = BDTYPE
         RETURN
C
      END IF
      LASTER = SUCESS
      RETURN
10000  FORMAT(/1X,'Array Name = ',A,
     &   4X,'Location = ',I8,4X,'Length =',I7)
10010  FORMAT(1X,I6,':',1P20E12.4)
10020  FORMAT(1X,I6,':',20I10)
10030  FORMAT (/1X, '%%% CAMSUPES_LIB error in ', A, :, ' %%% ', 5A)
      END
