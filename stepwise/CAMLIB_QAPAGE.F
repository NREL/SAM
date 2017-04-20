C=======================================================================
      SUBROUTINE QAPAGE (IUCOUT, FLAG)
C=======================================================================

C   --*** QAPAGE *** (CAMCON_LIB) Print QA information on output file
C   --   Modified by Amy Gilkey - revised 11/02/98
C   --   Written by Rob Rechard - June 1985
C   --
C   --QAPAGE should be called to start each page of output and to end the
C   --output file.  It starts a new page (except for the first call)
C   --and puts the QA information (program name, run time, etc.) at the
C   --top of each page.  A page number (which is maintained internally)
C   --may be printed in the header.  At the end of the file, it puts the
C   --ending QA information.
C   --
C   --Routine QASETUP must be called to set up the CAMCON_LIB common before
C   --QAPAGE is called.
C   --
C   --Parameters:
C   --   IUCOUT - IN integer - the output file unit number:
C   --      >0 for output file unit number IUCOUT
C   --      =0 for the standard output device (terminal)
C   --      <0 for the defined diagnostics file
C   --   FLAG - IN char*(*) - the type indicator:
C   --      (only the first character is checked; case is unimportant)
C   --      ' '    to print the QA information at the top with no page number
C   --      'PAGE' to print the QA information at the top with page number
C   --      'END'  to print the ending QA information
C   --      If first character is '!', precede lines by '!', other type follows

C   --Common Variables:
C   --   Uses PROGRM, EXEVERS, EXEDAT, EXETYP, EXECLASS, RUNDAT, RUNTIM
C   --      of CAMCON_LIB common

      IMPLICIT NONE

C   --Common Variables:
      INCLUDE 'CAMLIB_PROGRAM_COMMON.H'

C   --Parameter Definitions:
      INTEGER IUCOUT
      CHARACTER*(*) FLAG

C   --Definitions for functions called:
      INTEGER IQAERRUNI
      INTEGER ISTRLEN

      INTEGER IUNOUT
      INTEGER ITBL
      INTEGER L, MAXL
      INTEGER IDUM
      CHARACTER*128 TMPSTR
      CHARACTER*1 TMPFLG
      CHARACTER*1 LEADCH

      INTEGER MAXTBL
      PARAMETER (MAXTBL = 10)
      INTEGER NINTBL, IXTBL
      SAVE NINTBL, IXTBL
      INTEGER IUNITTBL(MAXTBL)
      SAVE IUNITTBL
      INTEGER IPAGETBL(MAXTBL)
      SAVE IPAGETBL

      DATA NINTBL / 0 /, IXTBL / 0 /

C   --Ensure the QA common is initialized
      CALL QA0INIT

C   --Determine unit number
      IUNOUT = IUCOUT
      IF (IUNOUT .LT. 0) IUNOUT = IQAERRUNI (IUCOUT)
      IF (IUNOUT .LE. 0) RETURN

C   --Find unit number in table for current page number
      DO 100 ITBL = 1, NINTBL
         IF (IUNITTBL(ITBL) .EQ. IUNOUT) GOTO 110
  100 CONTINUE
      IXTBL = IXTBL + 1
      IF (IXTBL .GT. MAXTBL) IXTBL = 1
      NINTBL = MAX (IXTBL, NINTBL)
      ITBL = IXTBL
      IUNITTBL(ITBL) = IUNOUT
      IPAGETBL(ITBL) = 0
  110 CONTINUE

C   --Handle special comment character in flag
      IF (FLAG(1:1) .EQ. '!') THEN
         LEADCH = '!'
         TMPFLG = FLAG(2:2)
      ELSE
         LEADCH = ' '
         TMPFLG = FLAG(1:1)
      END IF
      CALL STRUPCASE (TMPFLG)

      IF (TMPFLG .NE. 'E') THEN
C      --Start of new page

C      --Start a new page if not first call
         IF (IPAGETBL(ITBL) .GT. 0) THEN
            IF (LEADCH .EQ. ' ') THEN
               WRITE (IUNOUT, 10000, IOSTAT=IDUM)
10000           FORMAT ('1')
            ELSE
               WRITE (IUNOUT, 10010, IOSTAT=IDUM)
10010           FORMAT ('!1')
            END IF
         END IF

C      --Increment the page counter
         IPAGETBL(ITBL) = IPAGETBL(ITBL) + 1

      ELSE
C      --End of program

C      --Write ending QA information
         WRITE (IUNOUT, '(3(/))', IOSTAT=IDUM)
         IF (PROGRM .NE. ' ') THEN
            WRITE (IUNOUT, 10020, IOSTAT=IDUM) LEADCH,
     &         PROGRM(:ISTRLEN(PROGRM))
         ELSE
            WRITE (IUNOUT, 10020, IOSTAT=IDUM) LEADCH,
     &         'PROGRAM'
         END IF
10020     FORMAT (A, '*** END OF ', A, ' ***')
      END IF

C   --Write the QA information
      IF (PROGRM .NE. ' ') THEN
C      --These lengths correspond to the tabs settings on the write formats
         IF (TMPFLG .NE. 'P') THEN
            MAXL = 62-2
         ELSE
            MAXL = 52-2
         END IF
         TMPSTR = ' '
         L = 0
         TMPSTR(L+1:) = PROGRM
         L = ISTRLEN (TMPSTR)
         IF (EXEVERS .NE. ' ') THEN
            TMPSTR(L+1:) = ' ' // EXEVERS
            L = ISTRLEN (TMPSTR)
         END IF
         IF (EXETYP .NE. ' ') THEN
            TMPSTR(L+1:) = ' ' // EXETYP
            L = ISTRLEN (TMPSTR)
         END IF
         IF (EXECLASS .NE. ' ') THEN
            TMPSTR(L+1:) = ' ' // EXECLASS
            L = ISTRLEN (TMPSTR)
         END IF
         IF (EXEDAT10 .NE. ' ') THEN
            IF (L+1+10 .LT. MAXL) THEN
               TMPSTR(L+1:) = ' ' // EXEDAT10
               L = ISTRLEN (TMPSTR)
            END IF
         END IF
         IF (TMPFLG .NE. 'P') THEN
            WRITE (IUNOUT, 10030, IOSTAT=IDUM) LEADCH,
     &         TMPSTR(:L), RUNDAT10, RUNTIM
         ELSE
            WRITE (IUNOUT, 10040, IOSTAT=IDUM) LEADCH,
     &         TMPSTR(:L), RUNDAT10, RUNTIM, IPAGETBL(ITBL)
         END IF
      ELSE IF (RUNDAT10 .NE. ' ') THEN
         IF (TMPFLG .NE. 'P') THEN
            WRITE (IUNOUT, 10030, IOSTAT=IDUM) LEADCH,
     &         ' ', RUNDAT10, RUNTIM
         ELSE
            WRITE (IUNOUT, 10040, IOSTAT=IDUM) LEADCH,
     &         ' ', RUNDAT10, RUNTIM, IPAGETBL(ITBL)
         END IF
      ELSE
         IF (TMPFLG .NE. 'P') THEN
            CONTINUE
         ELSE
            WRITE (IUNOUT, 10050, IOSTAT=IDUM) LEADCH,
     &         IPAGETBL(ITBL)
         END IF
      END IF
      IF (TMPFLG .NE. 'E') WRITE (IUNOUT, *, IOSTAT=IDUM)

      RETURN
C   --Note that if the first tabs settings changed, MAXL must be changed
10030  FORMAT (A, A, T62, A, ' ', A)
10040  FORMAT (A, A, T52, A, ' ', A, T74, 'Page', I3)
10050  FORMAT (A,                    T74, 'Page', I3)
      END
