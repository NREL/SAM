C=======================================================================
      SUBROUTINE FFRDFLDg (KIN, KOUT, PROMPT, MAXFLD, IOSTAT, NUMFLD,
     &   INTYP, CFIELD, IFIELD, DFIELD)
C=======================================================================

C   --*** FFRDFLDg *** (CAMCON_LIB) Free-field reader
C   --   Revised by Amy Gilkey - revised 02/27/93
C   --   Free-field reader taken from the SUPES library
C   --
C   --FFRDFLDg reads a record and parses the record into fields.  See SUPES
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
C   --       1 = a DOUBLE PRECISION numeric field
C   --       2 = an INTEGER numeric field
C   --   CFIELD(MAXFLD) - OUT char*(*) - the character values of the data fields
C   --   IFIELD(MAXFLD) - OUT integer - the integer values of the data fields
C   --   DFIELD(MAXFLD) - OUT real*8 - the G_FLOAT double precision values
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
      DOUBLE PRECISION DFIELD(MAXFLD)

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
         DFIELD(I) = 0.0
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
      CALL FFPARSEg (LINE, MAXFLD, IDCONT, NUMFLD,
     &   INTYP, CFIELD, IFIELD, DFIELD)

C   --Loop if the continuation flag is set, otherwise return to the caller -
      IF (IDCONT .NE. 0) GOTO 110

C   --If fields overrun, set I/O status
      IF ((NUMFLD .GT. MAXFLD) .AND. (IOSTAT .EQ. 0)) THEN
         IOSTAT = 1000
      END IF

      RETURN
      END
C=======================================================================
      SUBROUTINE FFPARSEg (LINE, MAXFLD, IDCONT, NUMFLD,
     &   INTYP, CFIELD, IFIELD, DFIELD)
C=======================================================================

C   --*** FFPARSEg *** (CAMCON_LIB) Parse free-field reader line
C   --   Revised by Amy Gilkey - revised 04/04/95
C   --   Free-field reader taken from the SUPES library
C   --
C   --FFPARSEg parses a record into fields.  See SUPES documentation for
C   --more information.  The following changes have been made:
C   --   o A tab is treated the same as a space.
C   --   o The comment character is '!'.
C   --   o The continuation character is '&'.
C   --   o Fields may be enclosed in either single or double quotes.
C   --     The field is stored exactly as input (preserving case and
C   --     embedded delimiters).  The quotes are stripped from the field.
C   --
C   --Parameters:
C   --   LINE - IN char*(*) - the input string
C   --   MAXFLD - IN integer - the maximum number of data fields to be returned
C   --   IDCONT - IN integer - the continuation flag (0=NO continuation)
C   --   NUMFLD - OUT integer - the number of data fields found
C   --   INTYP(MAXFLD) - OUT integer - the translation states of the data
C   --      fields:
C   --      -1 = a null field
C   --       0 = a non-numeric field
C   --       1 = a DOUBLE PRECISION numeric field
C   --       2 = an INTEGER numeric field
C   --   CFIELD(MAXFLD) - OUT char*(*) - the character values of the data fields
C   --   IFIELD(MAXFLD) - OUT integer - the integer values of the data fields
C   --   DFIELD(MAXFLD) - OUT real*8 - the G_FLOAT double precision values
C   --      of the data fields

      IMPLICIT NONE

C   --Parameter Definitions:
      CHARACTER*(*) LINE
      INTEGER MAXFLD
      INTEGER IDCONT
      INTEGER NUMFLD
      INTEGER INTYP(MAXFLD)
      CHARACTER*(*) CFIELD(MAXFLD)
      INTEGER IFIELD(MAXFLD)
      DOUBLE PRECISION DFIELD(MAXFLD)

      INTEGER I
      INTEGER ILEFT
      INTEGER ISTOP
      CHARACTER*256 SCRLIN

************************************************************************

      IF (IDCONT .EQ. 0) THEN
C      --Initialize output fields to their default values -
         DO 100 I = 1 , MAXFLD
            INTYP(I) = -1
            CFIELD(I) = ' '
            IFIELD(I) = 0
            DFIELD(I) = 0.0
  100    CONTINUE
         NUMFLD = 0
      END IF

C   --Process separators, etc. in input line -
      CALL FF0LIN (IDCONT, LINE, SCRLIN, ILEFT, ISTOP)

C   --Parse input fields until end of line -
  110 CONTINUE
      IF (ILEFT .LE. ISTOP) THEN

C      --Increment the field counter -
         NUMFLD = NUMFLD + 1

C      --Call FF0TKNg to parse the next input field -
         CALL FF0TKNg (LINE, SCRLIN, ILEFT, ISTOP, .FALSE.,
     &      (NUMFLD .LE. MAXFLD), INTYP(NUMFLD),
     &      CFIELD(NUMFLD), IFIELD(NUMFLD), DFIELD(NUMFLD))

         GOTO 110
      END IF

      RETURN
      END
C=======================================================================
      SUBROUTINE FF0TKNg (LINE, SCRLIN, ILEFT, ISTOP, EQUAL, STORIT,
     &   INTYP, CFIELD, IFIELD, DFIELD)
C=======================================================================

C   --*** FF0TKNg *** (CAMCON_LIB internal) Return free-field reader field
C   --   Revised by Amy Gilkey - revised 12/03/94
C   --   Free-field reader taken from the SUPES library
C   --
C   --FF0TKNg returns the next field of a free-field reader line.
C   --
C   --Parameters:
C   --   LINE - IN - the input string
C   --   SCRLIN - IN - the processed input line
C   --   ILEFT - IN/OUT - the index of the first character to process
C   --      in the input line
C   --   ISTOP - IN - the index of the last character in the processed line
C   --   EQUAL - IN - true iff equal sign should be flagged as field
C   --   STORIT - IN - true iff data field should be stored;
C   --      if false INTYP, CFIELD, IFIELD, DFIELD are not set!
C   --   INTYP - OUT - the translation states of the data field:
C   --      -1 = a null field
C   --       0 = a non-numeric field
C   --       1 = a DOUBLE PRECISION numeric field
C   --       2 = an INTEGER numeric field
C   --       3 = an equal sign field (if EQUAL)
C   --   CFIELD - OUT - the character values of the data field
C   --   IFIELD - OUT - the integer values of the data field
C   --   DFIELD - OUT - the G_FLOAT double precision values of the data field

      IMPLICIT NONE

C   --Parameter Definitions:
      CHARACTER*(*) LINE
      CHARACTER*(*) SCRLIN
      INTEGER ILEFT, ISTOP
      LOGICAL EQUAL
      LOGICAL STORIT
      INTEGER INTYP
      CHARACTER*(*) CFIELD
      INTEGER IFIELD
      DOUBLE PRECISION DFIELD

      INTEGER ICOMMA
      INTEGER IRIGHT
      INTEGER IOFF
      INTEGER LFLD
      INTEGER IJUST
      INTEGER ITRANS
      INTEGER INUM
      INTEGER IX
      INTEGER IXQUO
      DOUBLE PRECISION DNUM
      CHARACTER CH
      CHARACTER*32 CNUM
      CHARACTER*88 CFLD

      CHARACTER QUOCH, DQUOCH
      PARAMETER (QUOCH = '''', DQUOCH = '"')

************************************************************************

C--Extract, translate, and catagorize data field.
C--LINE(ILEFT:IRIGHT) represents the current data field.
C--ICOMMA points to the next field separator and ISTOP always
C--points to the last character in the effective input line.

C   --Find the next separator -
      ICOMMA = INDEX (SCRLIN(ILEFT:ISTOP), ',') + ILEFT - 1
      IF (ICOMMA .EQ. ILEFT) THEN
C      --Check if the null field has an equal sign as a separator;
C      --if so save the equal sign -
         IF (EQUAL .AND. (LINE(ICOMMA:ICOMMA) .EQ. '=')) THEN
            IRIGHT = ICOMMA
         ELSE
            IRIGHT = ICOMMA - 1
         END IF
      ELSE IF (ICOMMA .GE. ILEFT) THEN
C      --Set the right edge of this field to the character immediately before
C      --the next separator -
         IRIGHT = ICOMMA - 1
      ELSE
C      --There are no more separators in this line; set the right edge and the
C      --separator pointer to the last data character -
         IRIGHT = ISTOP
         ICOMMA = ISTOP
      END IF

************************************************************************

C   --Don't process this field unless the store flag is set -
      IF (STORIT) THEN

C      --Initialize output field to default values -
         INTYP = -1
         CFIELD = ' '
         IFIELD = 0
         DFIELD = 0.0

C      --Strip blanks from this field -
         IF (ILEFT .LE. IRIGHT) THEN
            IOFF = ILEFT - 1
            CALL STRLIMIT (LINE(ILEFT:IRIGHT), ILEFT, IRIGHT)
            ILEFT  = ILEFT  + IOFF
            IRIGHT = IRIGHT + IOFF
         END IF

C      --Calculate the effective length of this field -
         LFLD = IRIGHT - ILEFT + 1
         IF (LFLD .LE. 0) THEN
C         --This is a null field -
            CFLD = ' '
            INTYP = -1

         ELSE IF (LFLD .GT. 32) THEN
C         --This field exceeds the maximum allowable numeric field size;
C         --define only the character value -
            CFLD = LINE(ILEFT:IRIGHT)
            INTYP = 0

         ELSE

C         --Define the character value for this field, then right-justify and
C         --attempt numeric translations -
            CFLD = LINE(ILEFT:IRIGHT)
            INTYP = 0
            CNUM = ' '
            IJUST = 32 - LFLD + 1
            CNUM(IJUST:32) = LINE(ILEFT:IRIGHT)
            READ (CNUM, '(D32.0)', IOSTAT=ITRANS) DNUM
            IF (ITRANS .EQ. 0) THEN
C            --This field has a valid G_FLOAT double precision value -
               DFIELD = DNUM
               INTYP = 1
            END IF
            READ (CNUM, '(I32)', IOSTAT=ITRANS) INUM
            IF (ITRANS .EQ. 0) THEN
C            --This field has a valid integer value -
               IFIELD = INUM
               INTYP = 2
            END IF
         END IF

         CH = CFLD(1:1)
         IF ((CH .EQ. QUOCH) .OR. (CH .EQ. DQUOCH)) THEN
C         --This field is a quoted string; remove starting quote -
            CFLD = CFLD(2:)
            LFLD = LFLD - 1
            IX = 1
  100       CONTINUE
            IF (IX .LE. LFLD) THEN
C            --Find quote and delete from string; do not check next
C            --character (to allow two quote characters to become one quote) -
               IXQUO = INDEX (CFLD(IX:LFLD), CH) + IX - 1
               IF (IXQUO .GE. IX) THEN
                  CFLD(IXQUO:LFLD) = CFLD(IXQUO+1:LFLD)
                  LFLD = LFLD - 1
                  IX = IXQUO + 1
                  GOTO 100
               END IF
            END IF
         ELSE IF (EQUAL .AND. (CH .EQ. '=')) THEN
C         --Identify equal sign -
            INTYP = 3
         ELSE
C         --Convert non-quoted field to upper case -
            CALL STRUPCASE (CFLD)
         END IF
         CFIELD = CFLD
      END IF

************************************************************************

C   --Reposition left edge pointer beyond the next separator -
      IF (EQUAL .AND. (INTYP .EQ. 3)) THEN
         ILEFT = ICOMMA + 1
      ELSE IF (EQUAL .AND. (LINE(ICOMMA:ICOMMA) .EQ. '=')) THEN
         ILEFT = ICOMMA
      ELSE
         ILEFT = ICOMMA + 1
      END IF

      RETURN
      END
