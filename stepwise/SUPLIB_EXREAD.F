C=======================================================================
      SUBROUTINE EXREAD (PROMPT, INPUT, IOSTAT)
C=======================================================================
C     FORTRAN Extension Library - UNIX ### INCOMPLETE ###

C   --*** EXREAD *** (CAMSUPES_LIB) Prompts and reads from standard input
C   --   Written by Dennis Morrison and Bill Mill-Curran - revised 06/03/95
C   --
C   --EXREAD prompts, reads, and echos from the standard input
C   --device. For an interactive job, this would prompt for input from
C   --the terminal and read (with echo) from the keyboard. For a batch
C   --job, this would read from the main input file and echo to the
C   --log file with the prompt string as a prefix. This routine should
C   --assume the burden of assuring that the standard input and output
C   --devices are properly opened.
C   --
C   --This VMS version uses SYS$INPUT as the standard input device and
C   --SYS$OUTPUT as the standard output device. If SYS$INPUT is not a
C   --terminal, an echo to SYS$OUTPUT is forced.
C   --
C   --Parameters:
C   --   PROMPT - IN char*(*) - the prompt string
C   --   INPUT - OUT char*(*) - the input string
C   --   IOSTAT - OUT integer - the I/O status (-1 = EOF, 0 = normal)

C   --Machine Dependencies:
C   --   ###

      IMPLICIT NONE

C   --Parameter Definitions:
      CHARACTER*(*) PROMPT
      CHARACTER*(*) INPUT
      INTEGER IOSTAT

      LOGICAL FIRST, ECHO
      SAVE FIRST, ECHO
      DATA FIRST /.TRUE./

C   --Decide if commands need be echoed
      IF (FIRST) THEN
         ECHO = .FALSE.
         FIRST = .FALSE.
      END IF

C   --Prompt, read, and echo
      INPUT = ' '
      WRITE (*, '(1X, A, $)') PROMPT
      READ (*, '(A)', IOSTAT=IOSTAT) INPUT

      RETURN
      END
