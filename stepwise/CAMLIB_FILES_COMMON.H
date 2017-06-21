C *** camlib_FILES_COMMON.INC ***

C   --The following information is set and used by the File Definition
C   --Routines that get the program files from the command line.

      INTEGER MAXFIL
      PARAMETER (MAXFIL = 50)
C      --MAXFIL - the maximum number of command line arguments

      INTEGER NUMFIL
C      --NUMFIL - the current number of defined command line arguments
      INTEGER ICTFIL
C      --ICTFIL - the counter for the command line arguments in FILGTNAMS
      CHARACTER*80 DSCTBL(MAXFIL)
C      --DSCTBL - the description of each command line argument
      CHARACTER*1 INOTBL(MAXFIL)
C      --'I' for an input file
C      --'O' for an output file
C      --'U' for an input file that may be interactive
C      --'T' for a non-file entry (a text string entry)
      LOGICAL OPTTBL(MAXFIL)
C      --OPTTBL - the optional/required flag for each command line argument
      CHARACTER*128 FILTBL(MAXFIL)
C      --FILTBL - the default file name, replaced by the full path name
C      --   of each command line argument
      COMMON /XFILCOMMON/ NUMFIL, ICTFIL, OPTTBL
      COMMON /XFILCOMMONc/ DSCTBL, INOTBL, FILTBL
