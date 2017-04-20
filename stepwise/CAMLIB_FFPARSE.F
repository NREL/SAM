C=======================================================================
      SUBROUTINE FFPARSE (LINE, MAXFLD, IDCONT, NUMFLD,
     &   INTYP, CFIELD, IFIELD, RFIELD)
C=======================================================================

C   --*** FFPARSE *** (CAMCON_LIB) Parse free-field reader line
C   --   Revised by Amy Gilkey - revised 04/04/95
C   --   Free-field reader taken from the SUPES library
C   --
C   --FFPARSE parses a record into fields.  See SUPES documentation for
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
C   --       1 = a REAL numeric field
C   --       2 = an INTEGER numeric field
C   --   CFIELD(MAXFLD) - OUT char*(*) - the character values of the data fields
C   --   IFIELD(MAXFLD) - OUT integer - the integer values of the data fields
C   --   RFIELD(MAXFLD) - OUT real - the floating-point values
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
      REAL RFIELD(MAXFLD)

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
            RFIELD(I) = 0.0
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

C      --Call FF0TKN to parse the next input field -
         CALL FF0TKN (LINE, SCRLIN, ILEFT, ISTOP, .FALSE.,
     &      (NUMFLD .LE. MAXFLD), INTYP(NUMFLD),
     &      CFIELD(NUMFLD), IFIELD(NUMFLD), RFIELD(NUMFLD))

         GOTO 110
      END IF

      RETURN
      END
