C=======================================================================
      SUBROUTINE FFRDFLDS (KIN, KOUT, PROMPT, MAXFLD, IOSTAT, NUMFLD,
     &   INTYP, CFIELD, IFIELD, RFIELD)
C=======================================================================

C   --*** FFRDFLDS *** (CAMCON_LIB) Free-field reader
C   --   Revised by Amy Gilkey - revised 02/27/93
C   --   Free-field reader taken from the SUPES library
C   --
C   --FFRDFLDS reads a record and parses the record into fields.  See SUPES
C   --documentation for more information.  The following changes have
C   --been made:
C   --   o A tab is treated the same as a space.
C   --   o The comment character is '!'.
C   --   o The continuation character is '&'.
C   --   o Fields may be enclosed in either single or double quotes.
C   --     The field is stored exactly as input (preserving case and
C   --     embedded delimiters).  The quotes are stripped from the field.
C   --
C   --Parameters:
C   --   KIN - IN integer - the unit from which to read input
C   --   KOUT - IN integer - the unit to which to echo input
C   --   PROMPT - IN char*(*) - the prompt string
C   --   MAXFLD - IN integer - the maximum number of data fields to be returned
C   --   IOSTAT - OUT integer - the ANSI FORTRAN I/O status;
C   --      1000 = number of fields overruns maximum
C   --   NUMFLD - OUT integer - the number of data fields found
C   --   INTYP(MAXFLD) - OUT integer - the translation states of the data
C   --      fields:
C   --      -1 = a null field
C   --       0 = a non-numeric field
C   --       1 = a REAL numeric field
C   --       2 = an INTEGER numeric field
C   --   CFIELD(MAXFLD) - OUT char*(*) - the character values of the data fields
C   --   IFIELD(MAXFLD) - OUT integer - the integer values of the data fields
C   --   RFIELD(MAXFLD) - OUT real - the floating-point values
C   --      of the data fields

C   --Common Variables:
C   --   Sets SAVLIN of /FREEFIELD_COMMON/

      IMPLICIT NONE

C   --Common Variables:
      INCLUDE 'CAMLIB_FREEFIELD_COMMON.H'

C   --Parameter Definitions:
      INTEGER KIN, KOUT
      CHARACTER*(*) PROMPT
      INTEGER MAXFLD
      INTEGER IOSTAT
      INTEGER NUMFLD
      INTEGER INTYP(MAXFLD)
      CHARACTER*(*) CFIELD(MAXFLD)
      INTEGER IFIELD(MAXFLD)
      REAL RFIELD(MAXFLD)

      INTEGER I
      INTEGER IDCONT
      INTEGER LPRMPT
      CHARACTER*80 PRMPTT
      CHARACTER*256 LINE

************************************************************************

C   --Initialize output arrays to their default values and zero field counter -
      DO 100 I = 1 , MAXFLD
         INTYP(I) = -1
         CFIELD(I) = ' '
         IFIELD(I) = 0
         RFIELD(I) = 0.0
  100 CONTINUE
      NUMFLD = 0

C   --Clear the continuation indicator -
      IDCONT = 0

C--Get the next input record via FFRDLINE and parse the fields.  Re-enter
C--to process a continuation line.

  110 CONTINUE

C   --Set prompt (changed for continuation line) -
      IF (IDCONT .EQ. 0) THEN
         PRMPTT = PROMPT
         LPRMPT = LEN(PROMPT)
      ELSE
         PRMPTT = CONTCH // ' '
         LPRMPT = 2
      END IF

C   --Get the next input line -
      CALL FFRDLINE (KIN, KOUT, PRMPTT(:LPRMPT), LINE, IOSTAT)

C   --Return if I/O error or EOF detected -
      IF (IOSTAT .NE. 0) RETURN

C   --Parse input fields until end of line -
      CALL FFPARSE (LINE, MAXFLD, IDCONT, NUMFLD,
     &   INTYP, CFIELD, IFIELD, RFIELD)

C   --Loop if the continuation flag is set, otherwise return to the caller -
      IF (IDCONT .NE. 0) GOTO 110

C   --If fields overrun, set I/O status
      IF ((NUMFLD .GT. MAXFLD) .AND. (IOSTAT .EQ. 0)) THEN
         IOSTAT = 1000
      END IF

      RETURN
      END
