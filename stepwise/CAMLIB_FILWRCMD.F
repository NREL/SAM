C=======================================================================
      SUBROUTINE FILWRCMD (IUNIT, CMDFIL)
C=======================================================================

C   --*** FILWRCMD *** (CAMCON_LIB) Write the file names to a command file
C   --   Written by Amy Gilkey - revised 01/08/96
C   --
C   --FILWRCMD writes the program name with the file names defined by FILDFNAM
C   --and read by FILRDNAMS to a command file.  The command file is opened in
C   --this routine.
C   --
C   --Parameters:
C   --   IUNIT - IN integer - the command file unit number
C   --   CMDFIL - IN character*(*) - the name of the command file

      IMPLICIT NONE

C   --Common Variables:
      INCLUDE 'CAMLIB_PROGRAM_COMMON.H'
      INCLUDE 'CAMLIB_FILES_COMMON.H'

C   --Parameters Definitions:
      INTEGER IUNIT
      CHARACTER*(*) CMDFIL

C   --Definitions for functions called:
      INTEGER ISTRLEN

      INTEGER IFIL
      INTEGER IERR
      INTEGER IDUM
      CHARACTER*128 FILNAM

C   --Ensure the QA common is initialized
      CALL QA0INIT

      CALL FILOPEN (IUNIT, 'OUT', 'LIST', CMDFIL, IERR)
      IF (IERR .NE. 0) THEN
         CALL QAMESSAG (0, 'ERROR', 'Command file '
     &      // CMDFIL(:ISTRLEN(CMDFIL)) // ' cannot be opened')
      END IF

      IF (NUMFIL .GT. 0) THEN
         IF (PROGRM .NE. ' ') THEN
            WRITE (IUNIT, 10000, IOSTAT=IDUM)
     &         PROGRM(:ISTRLEN(PROGRM)), '-'
         ELSE
            WRITE (IUNIT, 10000, IOSTAT=IDUM) 'program', '-'
         END IF
      ELSE
         IF (PROGRM .NE. ' ') THEN
            WRITE (IUNIT, 10000, IOSTAT=IDUM)
     &         PROGRM(:ISTRLEN(PROGRM))
         ELSE
            WRITE (IUNIT, 10000, IOSTAT=IDUM) 'program'
         END IF
      END IF

      DO 100 IFIL = 1, NUMFIL
         FILNAM = FILTBL(IFIL)
         IF (FILNAM .EQ. ' ') THEN
            IF (INOTBL(IFIL) .NE. 'T') THEN
               FILNAM = 'CANCEL'
            ELSE
               FILNAM = '""'
            END IF
         ELSE
            IF (INOTBL(IFIL) .EQ. 'T') THEN
               FILNAM = '"' // FILNAM(:ISTRLEN(FILNAM)) // '"'
            END IF
         END IF
         IF (IFIL .LT. NUMFIL) THEN
            WRITE (IUNIT, 10010, IOSTAT=IDUM)
     &         FILNAM(:ISTRLEN(FILNAM)), '-'
         ELSE
            WRITE (IUNIT, 10010, IOSTAT=IDUM)
     &         FILNAM(:ISTRLEN(FILNAM))
         END IF
  100 CONTINUE

      RETURN
10000  FORMAT ('$ ', A, :, 1X, A)
10010  FORMAT (5X, A, : 1X, A)
      END
