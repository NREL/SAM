C=======================================================================
      SUBROUTINE FILWRNAMS (IUCOUT, CDUMMY)
C=======================================================================

C   --*** FILWRNAMS *** (CAMCON_LIB) Write the file names read by FILRDNAMS
C   --   Written by Amy Gilkey - revised 01/06/96
C   --
C   --FILWRNAMS writes the file names defined by FILDFNAM and read by FILRDNAMS
C   --to the terminal or an output file.
C   --
C   --Parameters:
C   --   IUCOUT - IN integer - the output file unit number:
C   --      >0 for output file unit number IUCOUT
C   --      =0 for the standard output device (terminal)
C   --      <0 for the defined diagnostics file
C   --   CDUMMY - unused

      IMPLICIT NONE

C   --Common Variables:
      INCLUDE 'CAMLIB_FILES_COMMON.H'

C   --Parameters Definitions:
      INTEGER IUCOUT
C     add dimension to CDUMMY (*) in order to be consistant with definition in stepwise.for 
C     C.S. 05/25/2010      
      CHARACTER*(*) CDUMMY(*)

C   --Definitions for functions called:
      INTEGER IQAERRUNI
      INTEGER ISTRLEN

      INTEGER IUNOUT
      INTEGER I, L
      INTEGER IDUM
      CHARACTER*10 XDATE10
      CHARACTER*8 XTIME

      IUNOUT = IUCOUT
      IF (IUNOUT .LT. 0) IUNOUT = IQAERRUNI (IUCOUT)

      IF (IUNOUT .LE. 0) THEN
         WRITE (*, 10000, IOSTAT=IDUM)
      ELSE
         WRITE (IUNOUT, 10000, IOSTAT=IDUM)
      END IF
10000  FORMAT (/, 1X, 79('*'), /
     &   /, ' FILE ASSIGNMENTS:',
     &   /, ' -----------------')
      DO 100 I = 1, NUMFIL
         L = ISTRLEN (DSCTBL(I))
         IF (IUNOUT .LE. 0) THEN
            WRITE (*, 10010, IOSTAT=IDUM)
     &         DSCTBL(I)(:ISTRLEN(DSCTBL(I)))
         ELSE
            WRITE (IUNOUT, 10010, IOSTAT=IDUM)
     &         DSCTBL(I)(:ISTRLEN(DSCTBL(I)))
         END IF
         L = ISTRLEN (FILTBL(I))
         IF (IUNOUT .LE. 0) THEN
            WRITE (*, 10020, IOSTAT=IDUM) FILTBL(I)(:L)
         ELSE
            WRITE (IUNOUT, 10020, IOSTAT=IDUM) FILTBL(I)(:L)
         END IF
         IF ((INOTBL(I) .EQ. 'I') .OR. (INOTBL(I) .EQ. 'U')) THEN
            CALL EXFILDAT (FILTBL(I), XDATE10, XTIME)
            IF (XDATE10 .NE. ' ') THEN
               IF (IUNOUT .LE. 0) THEN
                  WRITE (*, 10030, IOSTAT=IDUM) XDATE10, XTIME
               ELSE
                  WRITE (IUNOUT, 10030, IOSTAT=IDUM) XDATE10, XTIME
               END IF
            END IF
         END IF
  100 CONTINUE

      RETURN
10010  FORMAT (1X, A, ':')
10020  FORMAT (4X, A)
10030  FORMAT (7X, 'Written on ', A, ' ', A)
      END
