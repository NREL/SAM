C=======================================================================
      SUBROUTINE FFCHAR (IFLD, INTYP, CFIELD, DEFVAL, CVAL)
C=======================================================================

C   --*** FFCHAR *** (ff) Parse free-field character string
C   --   Written by Amy Gilkey - revised 02/24/86
C   --
C   --FFCHAR parses a character field.  A default is supplied if the
C   --field is empty.
C   --
C   --Parameters:
C   --   IFLD - IN/OUT - the index of the current field number, incremented
C   --   INTYP - IN - the input type from the free-field reader
C   --   CFIELD - IN - the character fields
C   --   DEFVAL - IN - the default value if field is empty
C   --   CVAL - OUT - the character value

      IMPLICIT NONE

      INTEGER IFLD
      INTEGER INTYP(*)
      CHARACTER*(*) CFIELD(*)
      CHARACTER*(*) DEFVAL, CVAL

      IF (INTYP(IFLD) .GE. 0) THEN
         CVAL = CFIELD(IFLD)
      ELSE IF (INTYP(IFLD) .LE. -1) THEN
         CVAL = DEFVAL
      END IF

      IF (INTYP(IFLD) .GE. -1) IFLD = IFLD + 1
      RETURN
      END

C=======================================================================
      SUBROUTINE FFINT (IFLD, INTYP, IFIELD, EXPECT, IDEFVL, IVAL, *)
C=======================================================================

C   --*** FFINT *** (ff) Parse free-field integer
C   --   Written by Amy Gilkey - revised 02/24/86
C   --
C   --FFINT parses an integer field.  A default is supplied if the
C   --field is empty.
C   --
C   --Parameters:
C   --   IFLD - IN/OUT - the index of the current field number, incremented
C   --   INTYP - IN - the input types from the free-field reader
C   --   IFIELD - IN - the integer fields
C   --   EXPECT - IN - the value to expect string, for error
C   --   IDEFVL - IN - the default value if field is empty
C   --   IVAL - OUT - the integer value, set only if no error
C   --   * - return statement if the field is invalid; message is printed

      IMPLICIT NONE

      INTEGER IFLD
      INTEGER INTYP(*)
      INTEGER IFIELD(*)
      CHARACTER*(*) EXPECT
      INTEGER IDEFVL, IVAL

      CHARACTER*128 TXTMSG

      IF (INTYP(IFLD) .GE. 2) THEN
         IVAL = IFIELD(IFLD)
      ELSE IF (INTYP(IFLD) .LE. -1) THEN
         IVAL = IDEFVL
      ELSE
         TXTMSG = 'Expected ' // EXPECT
         CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
         GOTO 100
      END IF

      IF (INTYP(IFLD) .GE. -1) IFLD = IFLD + 1
      RETURN

  100 CONTINUE
      IF (INTYP(IFLD) .GE. -1) IFLD = IFLD + 1
      RETURN 1
      END

C=======================================================================
      SUBROUTINE FFREAL (IFLD, INTYP, RFIELD, EXPECT, DEFVAL, RVAL, *)
C=======================================================================

C   --*** FFREAL *** (ff) Parse free-field real
C   --   Written by Amy Gilkey - revised 02/24/86
C   --
C   --FFREAL parses a real field.  A default is supplied if the field
C   --is empty.
C   --
C   --Parameters:
C   --   IFLD - IN/OUT - the index of the current field number, incremented
C   --   INTYP - IN - the input type from the free-field reader
C   --   RFIELD - IN - the real field
C   --   EXPECT - IN - the value to expect string, for error
C   --   DEFVAL - IN - the default value if field is empty
C   --   RVAL - OUT - the real value, set only if no error
C   --   * - return statement if the field is invalid; message is printed

      IMPLICIT NONE

      INTEGER IFLD
      INTEGER INTYP(*)
      REAL RFIELD(*)
      CHARACTER*(*) EXPECT
      REAL DEFVAL, RVAL

      CHARACTER*128 TXTMSG

      IF (INTYP(IFLD) .GE. 1) THEN
         RVAL = RFIELD(IFLD)
      ELSE IF (INTYP(IFLD) .LE. -1) THEN
         RVAL = DEFVAL
      ELSE
         TXTMSG = 'Expected ' // EXPECT
         CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
         GOTO 100
      END IF

      IF (INTYP(IFLD) .GE. -1) IFLD = IFLD + 1
      RETURN

  100 CONTINUE
      IF (INTYP(IFLD) .GE. -1) IFLD = IFLD + 1
      RETURN 1
      END

C=======================================================================
      SUBROUTINE FFONOFF (IFLD, INTYP, CFIELD, ISON, *)
C=======================================================================

C   --*** FFONOFF *** (ff) Parse free-field ON/OFF
C   --   Written by Amy Gilkey - revised 02/24/86
C   --
C   --FFONOFF parses an on/off option from an input field.  No field is
C   --assumed 'ON'.
C   --
C   --Parameters:
C   --   IFLD - IN/OUT - the index of the current field number, incremented
C   --   INTYP - IN - the input type from the free-field reader
C   --   CFIELD - IN - the input option string
C   --   ISON - OUT - true iff the option is ON, set only if no error
C   --   * - return statement if the field is invalid; message is printed

      IMPLICIT NONE

      INTEGER IFLD
      INTEGER INTYP(*)
      CHARACTER*(*) CFIELD(*)
      LOGICAL ISON

      CHARACTER*128 TXTMSG
      CHARACTER*4 OPT

      IF (INTYP(IFLD) .EQ. 0) THEN
         OPT = CFIELD(IFLD)
      ELSE IF (INTYP(IFLD) .LE. -1) THEN
         OPT = 'ON'
      ELSE
         OPT = ' '
      END IF
      IF ((OPT .NE. 'ON') .AND. (OPT .NE. 'OFF')) THEN
         TXTMSG = 'Expected "ON" or "OFF"'
         CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
         GOTO 100
      END IF

      ISON = (OPT .EQ. 'ON')

      IF (INTYP(IFLD) .GE. -1) IFLD = IFLD + 1
      RETURN

  100 CONTINUE
      IF (INTYP(IFLD) .GE. -1) IFLD = IFLD + 1
      RETURN 1
      END

C=======================================================================
      SUBROUTINE FFIRANGE (IFLD, INTYP, CFIELD, IFIELD, EXPECT, MAXVAL,
     &   IRANGE, *)
C=======================================================================

C   --*** FFIRANGE *** (ff) Parse free-field integer range
C   --   Written by Amy Gilkey - revised 02/24/86
C   --
C   --FFIRANGE parses a range of integers.  A range has one of the following
C   --forms:
C   --            n1                  assume n2 = n1, n3 = 1
C   --            n1 TO n2            assume n3 = 1
C   --            n1 TO n2 STEP n3
C   --
C   --Parameters:
C   --   IFLD - IN/OUT - the index of the current field number, incremented
C   --   INTYP - IN - the input type array from the free-field reader
C   --   CFIELD - IN - the input string array from the free-field reader
C   --   IFIELD - IN - the input integer array from the free-field reader
C   --   EXPECT - IN - the type of range being parsed, for error
C   --   MAXVAL - IN - the maximum range value
C   --   IRANGE - OUT - the input range value array:
C   --          (1) = n1, (2) = n2, (3) = n3;
C   --      partially set on error
C   --   * - return statement if the range is invalid; message is printed

      IMPLICIT NONE

      INTEGER IFLD
      INTEGER INTYP(*)
      CHARACTER*(*) CFIELD(*)
      INTEGER IFIELD(*)
      CHARACTER*(*) EXPECT
      INTEGER MAXVAL
      INTEGER IRANGE(3)

      INTEGER ISTRLEN

      INTEGER IDUM
      CHARACTER*128 TXTMSG
      CHARACTER STR80*80

      IRANGE(1) = 0
      IRANGE(2) = 0
      IRANGE(3) = 1

      IF (INTYP(IFLD) .GE. -1) THEN

C      --Get starting number

         IF (INTYP(IFLD) .NE. 2) THEN
            WRITE (TXTMSG, 10000)
     &         EXPECT, CFIELD(IFLD)(:ISTRLEN(CFIELD(IFLD)))
            CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
            GOTO 100
         END IF
         IRANGE(1) = IFIELD(IFLD)
         IRANGE(2) = IRANGE(1)
         IRANGE(3) = 1
         IFLD = IFLD + 1

         IF (INTYP(IFLD) .EQ. 0) THEN

C         --Get TO and ending value

            IF (CFIELD(IFLD) .NE. 'TO') THEN
               WRITE (TXTMSG, 10000)
     &            '"TO"', CFIELD(IFLD)(:ISTRLEN(CFIELD(IFLD)))
               CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
               GOTO 100
            END IF
            IFLD = IFLD + 1

            IF (INTYP(IFLD) .NE. 2) THEN
               STR80 = 'TO ' // EXPECT
               WRITE (TXTMSG, 10000) STR80(:ISTRLEN(STR80)),
     &            CFIELD(IFLD)(:ISTRLEN(CFIELD(IFLD)))
               CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
               GOTO 100
            END IF
            IRANGE(2) = IFIELD(IFLD)
            IFLD = IFLD + 1

            IF (INTYP(IFLD) .EQ. 0) THEN

C            --Get BY and step value

               IF (CFIELD(IFLD) .NE. 'BY') THEN
                  WRITE (TXTMSG, 10000)
     &               '"BY"', CFIELD(IFLD)(:ISTRLEN(CFIELD(IFLD)))
                  CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
                  GOTO 100
               END IF
               IFLD = IFLD + 1

               IF (INTYP(IFLD) .NE. 2) THEN
                  WRITE (TXTMSG, 10000)
     &               'BY value', CFIELD(IFLD)(:ISTRLEN(CFIELD(IFLD)))
                  CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
                  GOTO 100
               END IF
               IRANGE(3) = IFIELD(IFLD)
               IFLD = IFLD + 1
            END IF
         END IF

         IF (IRANGE(3) .EQ. 0) THEN
            WRITE (TXTMSG, 10010, IOSTAT=IDUM)
     &         'Invalid BY value', IRANGE(3)
            CALL STRCMPRS (TXTMSG, IDUM)
            CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
            GOTO 110
         END IF
         IF ((IRANGE(3) .GT. 0) .AND. (IRANGE(1) .GT. IRANGE(2))) THEN
            STR80 = 'Starting ' // EXPECT
            WRITE (TXTMSG, 10010, IOSTAT=IDUM) STR80(:ISTRLEN(STR80)),
     &         IRANGE(1), ' > ending ', IRANGE(2)
            CALL STRCMPRS (TXTMSG, IDUM)
            CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
            GOTO 110
         END IF
         IF ((IRANGE(3) .LT. 0) .AND. (IRANGE(1) .LT. IRANGE(2))) THEN
            STR80 = 'Starting ' // EXPECT
            WRITE (TXTMSG, 10010, IOSTAT=IDUM) STR80(:ISTRLEN(STR80)),
     &         IRANGE(1), ' < ending ', IRANGE(2), ' with negative step'
            CALL STRCMPRS (TXTMSG, IDUM)
            CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
         END IF
         IF (MIN (IRANGE(1), IRANGE(2)) .GT. MAXVAL) THEN
            STR80 = 'Minimum ' // EXPECT
            WRITE (TXTMSG, 10010, IOSTAT=IDUM) STR80(:ISTRLEN(STR80)),
     &         MIN (IRANGE(1), IRANGE(2)), ' > maximum ', MAXVAL
            CALL STRCMPRS (TXTMSG, IDUM)
            CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
            GOTO 110
         END IF
         IF ((IRANGE(1) .LE. 0) .OR. (IRANGE(2) .LE. 0)) THEN
            STR80 = 'Negative or zero ' // EXPECT
            WRITE (TXTMSG, 10010, IOSTAT=IDUM) STR80(:ISTRLEN(STR80)),
     &         MIN (IRANGE(1), IRANGE(2)), ' > maximum ', MAXVAL
            CALL STRCMPRS (TXTMSG, IDUM)
            CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
            GOTO 110
         END IF

         IF (IRANGE(1) .GT. MAXVAL) IRANGE(1) = MAXVAL
         IF (IRANGE(2) .GT. MAXVAL) IRANGE(2) = MAXVAL
      END IF

      RETURN

  100 CONTINUE
      IF (INTYP(IFLD) .GE. -1) IFLD = IFLD + 1
  110 CONTINUE
      RETURN 1
10000  FORMAT ('Expected ', A, ', not "', A, '"')
10010  FORMAT (A, I5, A, I5, A)
      END

C=======================================================================
      LOGICAL FUNCTION FFEXIST (IFLD, INTYP)
C=======================================================================

C   --*** FFEXIST *** (ff) Return end of fields status
C   --   Written by Amy Gilkey - revised 08/26/86
C   --
C   --FFEXIST returns true if and only if it has not passed the end of the
C   --parsed fields (marked by a type less than -1).
C   --
C   --Parameters:
C   --   IFLD - IN - the index of the current field number
C   --   INTYP - IN - the input type from the free-field reader;
C   --      <-1 for end of fields

      IMPLICIT NONE

      INTEGER IFLD
      INTEGER INTYP(*)

      FFEXIST = (INTYP(IFLD) .GE. -1)

      RETURN
      END

C=======================================================================
      LOGICAL FUNCTION FFMATCH (IFLD, INTYP, CFIELD, MATCH, NLET)
C=======================================================================

C   --*** FFMATCH *** (ff) Parse free-field character string if match
C   --   Written by Amy Gilkey - revised 07/01/87
C   --
C   --FFMATCH parses a character field and returns true (and increments
C   --IFLD) iff it is equal to the match string.  Only NLET letters must
C   --be in the input field to match, but if more letters are given, they
C   --must match the match string exactly.
C   --
C   --Parameters:
C   --   IFLD - IN/OUT - the index of the current field number, incremented
C   --      only if field matches
C   --   INTYP - IN - the input type from the free-field reader
C   --   CFIELD - IN - the character fields
C   --   MATCH - IN - the match string
C   --   NLET - IN - number of letters that must match; 0 for exact match

      IMPLICIT NONE

      INTEGER IFLD
      INTEGER INTYP(*)
      CHARACTER*(*) CFIELD(*)
      CHARACTER*(*) MATCH
      INTEGER NLET

      LOGICAL MATCHSTR

      IF (INTYP(IFLD) .GE. 0) THEN
         FFMATCH = MATCHSTR (CFIELD(IFLD), MATCH, NLET)
         IF (FFMATCH) IFLD = IFLD + 1
      ELSE
         FFMATCH = .FALSE.
      END IF

      RETURN
      END

C=======================================================================
      SUBROUTINE FFNEED (IFLD, INTYP, FTYPE, NFLD, EXPECT, *)
C=======================================================================

C   --*** FFNEED *** (ff) Check free-field fields for type
C   --   Written by Amy Gilkey - revised 10/21/86
C   --
C   --FFNEED checks that the next free-format fields exist and are of the
C   --appropriate type.
C   --
C   --Parameters:
C   --   IFLD - IN - the index of the current field number, NOT incremented
C   --   INTYP - IN - the input types from the free-field reader
C   --   FTYPE - IN - the expected field type:
C   --      C for character, R for real, I for integer, other for character
C   --   NFLD - IN - the number of expected fields
C   --   EXPECT - IN - the value to expect string, for error
C   --   * - return statement if the fields do not exist or are not of the
C   --      expected type; message is printed

      IMPLICIT NONE

      INTEGER IFLD
      INTEGER INTYP(*)
      CHARACTER*(*) FTYPE
      INTEGER NFLD
      CHARACTER*(*) EXPECT

      INTEGER ITYPE
      INTEGER ICHK
      CHARACTER*128 TXTMSG

      IF (FTYPE(1:1) .EQ. 'R') THEN
         ITYPE = 1
      ELSE IF (FTYPE(1:1) .EQ. 'I') THEN
         ITYPE = 2
      ELSE
         ITYPE = 0
      END IF

      DO 100 ICHK = IFLD, IFLD + NFLD - 1
         IF (INTYP(ICHK) .NE. ITYPE) THEN
            IF ((ITYPE .NE. 1) .OR. (INTYP(ICHK) .NE. 2)) THEN
               TXTMSG = 'Expected ' // EXPECT
               CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
               GOTO 110
            END IF
         END IF
  100 CONTINUE

      RETURN

  110 CONTINUE
      RETURN 1
      END

C=======================================================================
      SUBROUTINE FFSCENID (IFLD, INTYP, CFIELD, IFIELD,
     &   FINDID, NUMSCN, IDSCN, EXPECT, IDEFVL, ID, ISCN, *)
C=======================================================================

C   --*** FFSCENID *** (ff) Parse free-field scenario ID
C   --   Written by Amy Gilkey - revised 10/07/92
C   --
C   --FFSCENID parses a scenario ID, which may be either an integer or a
C   --character string whose last two digits are the ID.  The value may
C   --be checked for minimum/maximum values.
C   --
C   --Parameters:
C   --   IFLD - IN/OUT - the index of the current field number, incremented
C   --   INTYP - IN - the input types from the free-field reader
C   --   CFIELD - IN - the character fields
C   --   IFIELD - IN - the integer fields
C   --   FINDID - IN - whether new identifier is already defined in IDSCN
C   --      'FIND' if ID must be found
C   --      'NEW' if ID must NOT be found
C   --      ' ' if ID may or may not be found
C   --   NUMSCN - IN - the number of defined identifiers (negative if should not
C   --      be found)
C   --   IDSCN - IN - the defined identifiers
C   --   EXPECT - IN - the value to expect string, for error
C   --   IDEFVL - IN - the default value
C   --   ID - OUT - the integer value
C   --   ISCN - OUT - the IDSCN index of the identifier
C   --   * - return statement if invalid id; message is printed

      IMPLICIT NONE

      INTEGER IFLD
      INTEGER INTYP(*)
      CHARACTER*(*) CFIELD(*)
      INTEGER IFIELD(*)
      CHARACTER*(*) FINDID
      INTEGER NUMSCN
      INTEGER IDSCN(*)
      CHARACTER*(*) EXPECT
      INTEGER IDEFVL
      INTEGER ID
      INTEGER ISCN

      INTEGER ISTRLEN
      INTEGER LOCINT

      INTEGER L
      INTEGER IERR
      CHARACTER*128 TXTMSG

      IF (INTYP(IFLD) .GE. 2) THEN
         ID = IFIELD(IFLD)
      ELSE IF (INTYP(IFLD) .EQ. 0) THEN
         L = ISTRLEN (CFIELD(IFLD))
         READ (CFIELD(IFLD)(L-1:L), '(I2)', IOSTAT=IERR) ID
         IF (IERR .NE. 0) THEN
            TXTMSG = 'Expected ' // EXPECT
            CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
            GOTO 100
         END IF
      ELSE IF (INTYP(IFLD) .LE. -1) THEN
         ID = IDEFVL
      ELSE
         ID = IDEFVL
         TXTMSG = 'Expected ' // EXPECT
         CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
         GOTO 100
      END IF

      ISCN = LOCINT (ID, NUMSCN, IDSCN)
      IF (FINDID .EQ. 'FIND') THEN
         IF (ISCN .LE. 0) THEN
            TXTMSG = EXPECT // ' is not defined'
            CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
            GOTO 100
         END IF
      ELSE IF (FINDID .EQ. 'NEW') THEN
         IF (ISCN .GT. 0) THEN
            TXTMSG = EXPECT // ' is already defined'
            CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
            GOTO 100
         END IF
      END IF

      IF (INTYP(IFLD) .GE. -1) IFLD = IFLD + 1
      RETURN

  100 CONTINUE
      RETURN 1
      END
C CMS REPLACEMENT HISTORY, Element STP___FF_RTNS.FOR
C *1     1-NOV-1995 11:24:23 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP___FF_RTNS.FOR
