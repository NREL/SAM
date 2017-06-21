C=======================================================================
      SUBROUTINE FFRDLINE (KIN, KOUT, PROMPT, LINE, IOSTAT)
C=======================================================================

C   --*** FFRDLINE *** (CAMCON_LIB) Read line for Free Field Input System
C   --   Revised by Amy Gilkey - revised 09/02/94
C   --   Free-field reader taken from the SUPES library
C   --
C   --FFRDLINE performs all I/O for the SUPES Free Field Input system.
C   --Its operation depends on the input and output units specified by
C   --the caller as follows.
C   --
C   --This routine does not restrict the length of the input string, but
C   --no more than 132 characters, including the prompt, will be echoed.
C   --
C   --CHANGES FROM SUPES VERSION:
C   --  o Input unit <0 reads from command line
C   --  o Output unit <0 writes to the standard output device (*)
C   --
C   --Parameters:
C   --   KIN - IN integer - the unit from which to read input;
C   --      if <0, the command line is read
C   --      if =0, the standard input device is read (automatically echoes
C   --         to standard output)
C   --      if >0, the file connected to the unit KIN
C   --   KOUT - IN integer - the unit to which to echo input;
C   --      if <0, echo to standard output
C   --      if =0, do not echo, although it may echo to standard output if
C   --         KIN = 0
C   --      if >0, echo to file connected to unit KOUT
C   --   PROMPT - IN char*(*) - the prompt string.  If the prompt string is
C   --      'AUTO' a prompt of the form '   n: ' is generated, where "n" is
C   --      the current input line number (only lines read under the AUTO
C   --      feature are counted.
C   --   LINE - OUT char*(*) - the input record.
C   --   IOSTAT - OUT integer - the ANSI FORTRAN I/O status

      IMPLICIT NONE

C   --Parameter Definitions:
      INTEGER KIN, KOUT
      CHARACTER*(*) PROMPT
      CHARACTER*(*) LINE
      INTEGER IOSTAT

C   --Definitions for functions called:
      INTEGER ISTRLEN

      INTEGER IRIGHT
      INTEGER LPRE
      INTEGER IDUM
      CHARACTER*132 PREFIX

      INTEGER KOUNT
      SAVE KOUNT

      DATA KOUNT /0/

************************************************************************

C   --Generate a prompt if autoincrement mode is requested; in any case
C   --PREFIX(1:LPRE) will contain the prompt -
      IF (PROMPT .EQ. 'AUTO') THEN
         KOUNT = KOUNT + 1
C      --Wrap-around counter, if it exceeds 4 digits -
         IF (KOUNT .EQ. 10000) KOUNT = 0
         WRITE (PREFIX, 10000, IOSTAT=IDUM) KOUNT
         LPRE = 6
      ELSE
         PREFIX = PROMPT
         LPRE = LEN (PROMPT)
      END IF

C   --Initialize return parameters
      LINE = ' '
      IOSTAT = 0

C   --Read the input line -
      IF (KIN .EQ. 0) THEN
         CALL EXREAD (PREFIX(1:LPRE), LINE, IOSTAT)
      ELSE IF (KIN .GT. 0) THEN
         READ (KIN, 10010, IOSTAT=IOSTAT) LINE
      ELSE IF (KIN .LT. 0) THEN
C      --EXCMDLIN reads the command line -
         CALL EXCMDLIN (LINE)
      END IF

C   --Return if I/O error or EOF detected -
      IF (IOSTAT .NE. 0) RETURN

C   --Truncate the string for echo, if necessary -
      IRIGHT = MIN (ISTRLEN (LINE), 132-LPRE)

C   --Echo the input line, if requested -
      IF (KOUT .GT. 0) THEN
         IF (IRIGHT .EQ. 0) THEN
            WRITE (KOUT, 10020, IOSTAT=IDUM)
     &         PREFIX(1:LPRE)
         ELSE
            WRITE (KOUT, 10020, IOSTAT=IDUM)
     &         PREFIX(1:LPRE), LINE(1:IRIGHT)
         END IF
      ELSE IF (KOUT .LT. 0) THEN
         IF (IRIGHT .EQ. 0) THEN
            WRITE (*, 10020, IOSTAT=IDUM)
     &         PREFIX(1:LPRE)
         ELSE
            WRITE (*, 10020, IOSTAT=IDUM)
     &         PREFIX(1:LPRE), LINE(1:IRIGHT)
         END IF
      END IF

      RETURN
10000  FORMAT (I4, ': ')
10010  FORMAT (A)
10020  FORMAT (1X, A, A)
      END
