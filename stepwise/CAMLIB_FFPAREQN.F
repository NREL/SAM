C=======================================================================
      SUBROUTINE FFPAREQN (STVERB, LINE, MAXFLD, IDCONT, NUMFLD,
     &   INTYP, CFIELD, IFIELD, RFIELD)
C=======================================================================

C   --*** FFPAREQN *** (CAMCON_LIB) Parse equation line into fields
C   --   Written by Amy Gilkey - revised 02/27/93
C   --
C   --FFPAREQN converts a line to uppercase and parses the line into individual
C   --fields.  A field is one of the following:
C   --   a number (starts with 0-9 or ., rest = 0-9 or . or E or E+ or E-)
C   --   a name (starts with A-Z, rest = A-Z or 0-9 or _)
C   --   a non-number or non-name character; ** is converted to ^
C   --
C   --Parameters:
C   --   STVERB - IN logical - true iff the input line starts with a command
C   --      verb which must be stripped from the equation line
C   --   LINE - IN/OUT char*(*) - the input string; returned with command verb
C   --      stripped if STVERB is true
C   --   MAXFLD - IN integer - the maximum number of data fields to be returned
C   --   IDCONT - IN integer - the continuation flag (0=NO continuation)
C   --   NUMFLD - OUT integer - the number of data fields found
C   --   INTYP(MAXFLD) - OUT integer - the translation states of the data
C   --      fields:
C   --      -1 = a null field
C   --       0 = a non-numeric NAME field
C   --       1 = a REAL numeric field
C   --       2 = an INTEGER numeric field
C   --       3 = a character string
C   --   CFIELD(MAXFLD) - OUT char*(*) - the character values of the data fields
C   --   IFIELD(MAXFLD) - OUT integer - the integer values of the data fields
C   --   RFIELD(MAXFLD) - OUT real - the floating-point values
C   --      of the data fields

C   --Common Variables:
C   --   Uses CMTCH, CONTCH of /FREEFIELD_COMMON/

      IMPLICIT NONE

C   --Common Variables:
      INCLUDE 'CAMLIB_FREEFIELD_COMMON.H'

C   --Parameter Definitions:
      LOGICAL STVERB
      CHARACTER*(*) LINE
      INTEGER MAXFLD
      INTEGER IDCONT
      INTEGER NUMFLD
      INTEGER       INTYP(*)
      CHARACTER*(*) CFIELD(*)
      INTEGER       IFIELD(*)
      REAL          RFIELD(*)

C   --Definitions for functions called:
      INTEGER ISTRLEN

      INTEGER I
      INTEGER ICOMMA
      INTEGER NCOL
      INTEGER ISTOP
      INTEGER ILEFT, IRIGHT
      INTEGER IJUST
      INTEGER ITRANS
      INTEGER INUM
      REAL RNUM
      CHARACTER UPLINE*256
      CHARACTER STR32*32
      CHARACTER*1 CH

      CHARACTER*1 TAB
      PARAMETER (TAB = CHAR(9))

************************************************************************

      IF (IDCONT .EQ. 0) THEN
C      --Initialize output fields to their default values -
         DO 100 I = 1, MAXFLD
            INTYP(I) = -1
            CFIELD(I) = ' '
            IFIELD(I) = 0
            RFIELD(I) = 0.0
  100    CONTINUE
         NUMFLD = 0
      END IF

      IF (STVERB .AND. (IDCONT .EQ. 0)) THEN
C      --Strip off command verb from the start of the line -
         ICOMMA = 0
         NCOL = 1
         ISTOP = ISTRLEN (LINE)
  110    CONTINUE
         IF (NCOL .LE. ISTOP) THEN
            CH = LINE(NCOL:NCOL)
            IF ((CH .EQ. ' ') .OR. (CH .EQ. TAB)) THEN
               NCOL = NCOL + 1
               GOTO 110
            END IF
         END IF
  120    CONTINUE
         IF (NCOL .LE. ISTOP) THEN
            CH = LINE(NCOL:NCOL)
            IF ((CH .NE. ' ') .AND. (CH .NE. TAB)
     &         .AND. (CH .NE. ',')) THEN
               NCOL = NCOL + 1
               GOTO 120
            END IF
         END IF
  130    CONTINUE
         IF (NCOL .LE. ISTOP) THEN
            CH = LINE(NCOL:NCOL)
            IF ((CH .EQ. ' ') .OR. (CH .EQ. TAB)
     &         .OR. (CH .EQ. ',')) THEN
               IF (CH .EQ. ',') ICOMMA = ICOMMA + 1
               IF (ICOMMA .LE. 1) THEN
                  NCOL = NCOL + 1
                  GOTO 130
               END IF
            END IF
         END IF
         LINE = LINE(NCOL:)
      END IF

C   --Clear the continuation indicator -
      IDCONT = 0

C   --Change line to upper case -
      UPLINE = LINE
      CALL STRUPCASE (UPLINE)

C   --Repeat until done with input characters -
      ISTOP = ISTRLEN (LINE)
      NCOL = 1
  140 CONTINUE
      IF (NCOL .LE. ISTOP) THEN

         CH = UPLINE(NCOL:NCOL)

C      --Skip blank character -
         IF (CH .EQ. ' ') THEN
            NCOL = NCOL + 1
            GOTO 140
         END IF

C      --Skip over tab character (treat like blank) -
         IF (CH .EQ. TAB) THEN
            NCOL = NCOL + 1
            GOTO 140
         END IF

C      --End at comment character -
         IF (CH .EQ. CMTCH) GOTO 170

C      --End at continuation character (set indicator) -
         IF (CH .EQ. CONTCH) THEN
            IDCONT = 1
            GOTO 170
         END IF

C      --Increment field and check for overflow -
         NUMFLD = NUMFLD + 1
         IF (NUMFLD .GT. MAXFLD) THEN
            GOTO 170
         END IF
C      --Initialize output field to default values -
         INTYP(NUMFLD) = -1
         CFIELD(NUMFLD) = ' '
         IFIELD(NUMFLD) = 0
         RFIELD(NUMFLD) = 0.0

         IF (((CH .GE. '0') .AND. (CH .LE. '9'))
     &      .OR. (CH .EQ. '.')) THEN

C         --Get number string.

            ILEFT = NCOL
            IRIGHT = 0
  150       CONTINUE
            IF (IRIGHT .EQ. 0) THEN
               NCOL = NCOL + 1
               CH = UPLINE(NCOL:NCOL)
               IF (CH .EQ. 'E') THEN
                  CH = UPLINE(NCOL+1:NCOL+1)
                  IF ((CH .EQ. '+') .OR. (CH .EQ. '-')) NCOL = NCOL + 1
               ELSE IF (CH .EQ. '.') THEN
                  CONTINUE
               ELSE IF ((CH .GE. '0') .AND. (CH .LE. '9')) THEN
                  CONTINUE
               ELSE
                  IRIGHT = NCOL - 1
               END IF
               GOTO 150
            END IF

C         --Convert number string and store as real -
            STR32 = ' '
            IJUST = 32 - (IRIGHT - ILEFT + 1) + 1
            STR32(IJUST:32) = UPLINE(ILEFT:IRIGHT)
            READ (STR32, '(E32.0)', IOSTAT=ITRANS) RNUM
            CFIELD(NUMFLD) = UPLINE(ILEFT:IRIGHT)
            IF (ITRANS .EQ. 0) THEN
               RFIELD(NUMFLD) = RNUM
               INTYP(NUMFLD) = 1
            END IF
C         --Convert number string as integer, if possible -
            READ (STR32, '(I32)', IOSTAT=ITRANS) INUM
            IF (ITRANS .EQ. 0) THEN
               IFIELD(NUMFLD) = INUM
               INTYP(NUMFLD) = 2
            END IF

         ELSE IF ((CH .GE. 'A') .AND. (CH .LE. 'Z')) THEN

C         --Get word string and store.

            ILEFT = NCOL
            IRIGHT = 0
  160       CONTINUE
            IF (IRIGHT .EQ. 0) THEN
               NCOL = NCOL + 1
               CH = UPLINE(NCOL:NCOL)
               IF (((CH .GE. 'A') .AND. (CH .LE. 'Z'))
     &            .OR. ((CH .GE. '0') .AND. (CH .LE. '9'))
     &            .OR. (CH .EQ. '_')) THEN
                  CONTINUE
               ELSE
                  IRIGHT = NCOL - 1
               END IF
               GOTO 160
            END IF

            CFIELD(NUMFLD) = UPLINE(ILEFT:IRIGHT)
            INTYP(NUMFLD) = 0

         ELSE IF ((CH .EQ. '"') .OR. (CH .EQ. '''')) THEN

C         --Store quoted string

            ILEFT = NCOL
            IRIGHT = INDEX (UPLINE(ILEFT+1:), CH) + ILEFT
            IF (IRIGHT .GT. ILEFT) THEN
               CFIELD(NUMFLD) = LINE(ILEFT+1:IRIGHT-1)
               INTYP(NUMFLD) = 0
               NCOL = IRIGHT+1
            ELSE
               CFIELD(NUMFLD) = LINE(ILEFT:)
               INTYP(NUMFLD) = 0
               NCOL = ISTOP+1
            END IF

         ELSE

C         --Get single character (or **) and store -

            NCOL = NCOL + 1
            IF ((CH .EQ. '*') .AND. (UPLINE(NCOL:NCOL) .EQ. '*')) THEN
               CH = '^'
               NCOL = NCOL + 1
            END IF

            CFIELD(NUMFLD) = CH
            INTYP(NUMFLD) = 3
         END IF

         GOTO 140
      END IF

  170 CONTINUE
      RETURN
      END
