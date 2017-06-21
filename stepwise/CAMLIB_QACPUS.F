C=======================================================================
      SUBROUTINE QACPUS (IUCOUT, CPUSEC)
C=======================================================================

C   --*** QACPUS *** (CAMCON_LIB) Write CPU seconds used
C   --   Written by Amy Gilkey - revised 08/17/98
C   --
C   --QACPUS displays the number of CPU seconds used so far.
C   --
C   --Routine QASETUP must be called at the start of the program to initialize
C   --the CPU seconds.
C   --
C   --Parameters:
C   --   IUCOUT - IN integer - the output file unit number:
C   --      >0 for output file unit number IUCOUT
C   --      =0 for the standard output device (terminal)
C   --      <0 for the defined diagnostics file
C   --   CPUSEC - OUT real - the number of CPU seconds used so far

C   --Common Variables:
C   --   Uses PROGRM of CAMCON_LIB common

C   --Routines Called:
C   --   ISTRLEN - (CAMCON_LIB) Find string length

      IMPLICIT NONE

C   --Common Variables:
      INCLUDE 'CAMLIB_PROGRAM_COMMON.H'

C   --Parameter Definitions:
      INTEGER IUCOUT
      REAL CPUSEC

C   --Definitions for functions called:
      INTEGER IQAERRUNI
      INTEGER ISTRLEN

      INTEGER IUNOUT
      INTEGER IDUM
      INTEGER LSTR
      INTEGER ICPUHR, ICPUMIN, ICPUSEC
      REAL CPUHR, CPUMIN
      CHARACTER STR40*40

C   --Ensure the QA common is initialized
      CALL QA0INIT

      IUNOUT = IUCOUT
      IF (IUNOUT .LT. 0) IUNOUT = IQAERRUNI (IUCOUT)

C   --Get and display the CPU time used

      CALL EXCPUS (CPUSEC)

      ICPUHR = 0
      ICPUMIN = 0
      IF (CPUSEC .GE. 60.0) THEN
         CPUMIN = INT (CPUSEC / 60.0)
         ICPUSEC = CPUSEC - CPUMIN * 60.0
         IF (CPUMIN .GE. 60.0) THEN
            CPUHR = INT (CPUMIN / 60.0)
            ICPUMIN = CPUMIN - CPUHR * 60.0
            ICPUHR = CPUHR
            WRITE (STR40, 10000, IOSTAT=IDUM) ICPUHR, ICPUMIN, ICPUSEC
         ELSE
            ICPUMIN = CPUMIN
            WRITE (STR40, 10010, IOSTAT=IDUM) ICPUMIN, ICPUSEC
         END IF
      ELSE
         ICPUSEC = INT (CPUSEC + 0.5)
         WRITE (STR40, 10010, IOSTAT=IDUM) ICPUMIN, ICPUSEC
      END IF
10000  FORMAT (I8, ':', I2.2, ':', I2.2, ' (hour:minute:second)')
10010  FORMAT (I8, ':', I2.2, ' (minute:second)')
      CALL STRCMPRS (STR40, LSTR)

      IF (IUNOUT .LE. 0) THEN
         IF (PROGRM .NE. ' ') THEN
            WRITE (*, 10020, IOSTAT=IDUM)
     &         PROGRM(:ISTRLEN(PROGRM)), STR40(:LSTR)
         ELSE
            WRITE (*, 10020, IOSTAT=IDUM)
     &         'The program', STR40(:LSTR)
         END IF
      ELSE
         IF (PROGRM .NE. ' ') THEN
            WRITE (IUNOUT, 10020, IOSTAT=IDUM)
     &         PROGRM(:ISTRLEN(PROGRM)), STR40(:LSTR)
         ELSE
            WRITE (IUNOUT, 10020, IOSTAT=IDUM)
     &         'The program', STR40(:LSTR)
         END IF
      END IF
10020  FORMAT (/, 1X, A, ' CPU time is ', A)

      RETURN
      END
