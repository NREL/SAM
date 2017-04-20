C=======================================================================
      LOGICAL FUNCTION FFNUMBER (IFLD, INTYP)
C=======================================================================

C   --*** FFNUMBER *** (ff) Return number field status
C   --   Written by Amy Gilkey - revised 08/26/86
C   --
C   --FFNUMBER returns true if and only if the field is a number (an
C   --integer or a real).
C   --
C   --Parameters:
C   --   IFLD - IN - the index of the current field number
C   --   INTYP - IN - the input type from the free-field reader

      IMPLICIT NONE

      INTEGER IFLD
      INTEGER INTYP(*)

      FFNUMBER = (INTYP(IFLD) .EQ. 1) .OR. (INTYP(IFLD) .EQ. 2)

      RETURN
      END

C=======================================================================
      SUBROUTINE CKNONE (NVAL, ISSEL, VALNAM, *)
C=======================================================================

C   --*** CKNONE *** (ff) Check number of values is zero
C   --   Written by Amy Gilkey - revised 12/23/87
C   --
C   --CKNONE prints an error message if the number of values is zero.
C   --
C   --Parameters:
C   --   NVAL - IN - the value being checked
C   --   ISSEL - IN - print none selected error message iff true
C   --   VALNAM - IN - the name of the value being checked (plural)
C   --   * - return statement if error

      IMPLICIT NONE

      INTEGER NVAL
      LOGICAL ISSEL
      CHARACTER*(*) VALNAM

      CHARACTER*128 TXTMSG

      IF (NVAL .LE. 0) THEN
         IF (ISSEL) THEN
            TXTMSG = 'No ' // VALNAM // ' are selected'
            CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
         ELSE
            TXTMSG = 'There are no ' // VALNAM
            CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
         END IF
         RETURN 1
      END IF

      RETURN
      END

C=======================================================================
      SUBROUTINE LIMINT (IMINVL, IMAXVL, EXPECT, IVAL, *)
C=======================================================================

C   --*** LIMINT *** (ff) Limit integer
C   --   Written by Amy Gilkey - revised 01/02/90
C   --
C   --LIMINT checks if an integer is within allowable limits.
C   --
C   --Parameters:
C   --   IMINVL - IN - the minimum value
C   --   IMAXVL - IN - the maximum value
C   --   EXPECT - IN - the value to expect string, for error
C   --   IVAL - OUT - the integer value
C   --   * - return statement if the not within allowable limits;
C   --      message is printed

      IMPLICIT NONE

      INTEGER IMINVL, IMAXVL
      CHARACTER*(*) EXPECT
      INTEGER IVAL

      INTEGER IDUM
      CHARACTER*128 TXTMSG

      IF (IVAL .LT. IMINVL) THEN
         WRITE (TXTMSG, 10000, IOSTAT=IDUM) EXPECT, 'less', IMINVL
10000     FORMAT (A, ' is ', A, ' than ', I8)
         CALL STRCMPRS (TXTMSG, IDUM)
         CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
         IVAL = IMINVL
         GOTO 100
      ELSE IF (IVAL .GT. IMAXVL) THEN
         WRITE (TXTMSG, 10000, IOSTAT=IDUM) EXPECT, 'greater', IMAXVL
         CALL STRCMPRS (TXTMSG, IDUM)
         CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
         IVAL = IMAXVL
         GOTO 100
      END IF

      RETURN

  100 CONTINUE
      RETURN 1
      END

C=======================================================================
      SUBROUTINE LIMREAL (RMINVL, RMAXVL, EXPECT, RVAL, *)
C=======================================================================

C   --*** LIMREAL *** (ff) Limit real number
C   --   Written by Amy Gilkey - revised 01/02/90
C   --
C   --LIMREAL checks if a real number is within allowable limits.
C   --
C   --Parameters:
C   --   RMINVL - IN - the minimum value
C   --   RMAXVL - IN - the maximum value
C   --   EXPECT - IN - the value to expect string, for error
C   --   RVAL - OUT - the real value
C   --   * - return statement if the not within allowable limits;
C   --      message is printed

      IMPLICIT NONE

      REAL RMINVL, RMAXVL
      CHARACTER*(*) EXPECT
      REAL RVAL

      CHARACTER*128 TXTMSG

      IF (RVAL .LT. RMINVL) THEN
         TXTMSG = EXPECT // ' is less than minimum allowed'
         CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
         RVAL = RMINVL
         GOTO 100
      ELSE IF (RVAL .GT. RMAXVL) THEN
         TXTMSG = EXPECT // ' is greater than maximum allowed'
         CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
         RVAL = RMAXVL
         GOTO 100
      END IF

      RETURN

  100 CONTINUE
      RETURN 1
      END

C=======================================================================
      SUBROUTINE ABRSTR (RETWRD, ABBR, STRTBL)
C=======================================================================

C   --*** ABRSTR *** (ff) Find abbreviation for string
C   --   Written by Amy Gilkey - revised 02/14/86
C   --
C   --ABRSTR returns the non-abbreviated form of the given abbreviation
C   --from the list of possible strings.  The abbreviation must either
C   --be a complete string or it must only match one string.
C   --
C   --Parameters:
C   --   RETWRD - OUT - the string for the abbreviation; ' ' if none
C   --   ABBR - IN - the abbreviation
C   --   STRTBL - IN - the table of possible strings; ended by ' '

      IMPLICIT NONE

      CHARACTER*(*) RETWRD
      CHARACTER*(*) ABBR
      CHARACTER*(*) STRTBL(*)

      INTEGER L
      INTEGER I
      INTEGER NFOUND

      RETWRD = ' '

      IF (ABBR .EQ. ' ') RETURN

      L = INDEX (ABBR, ' ') - 1
      IF (L .LT. 0) L = LEN (ABBR)

      NFOUND = 0
      I = 1
  100 CONTINUE
      IF (STRTBL(I) .NE. ' ') THEN
         IF (ABBR .EQ. STRTBL(I)(1:L)) THEN
            RETWRD = STRTBL(I)
            IF (ABBR .EQ. STRTBL(I)) GOTO 110
            NFOUND = NFOUND + 1
         END IF
         I = I + 1
         GOTO 100
      END IF

      IF (NFOUND .GT. 1) RETWRD = ' '

  110 CONTINUE
      RETURN
      END

C=======================================================================
      LOGICAL FUNCTION MATCHSTR (INSTR, MATCH, NLET)
C=======================================================================

C   --*** MATCHSTR *** (ff) Check if string matches
C   --   Written by Amy Gilkey - revised 07/01/87
C   --
C   --MATCHSTR true iff the input string is equal to the match string.
C   --Only NLET letters must be in the input string to match, but if more
C   --letters are given, they must match the match string exactly.
C   --
C   --Parameters:
C   --   INSTR - IN - the input string
C   --   MATCH - IN - the match string
C   --   NLET - IN - number of letters that must match; 0 for exact match

      IMPLICIT NONE

      CHARACTER*(*) INSTR
      CHARACTER*(*) MATCH
      INTEGER NLET

      INTEGER ISTRLEN

      INTEGER LMATCH
      INTEGER LMIN
      INTEGER LINSTR

      IF (NLET .LE. 0) THEN
         MATCHSTR = INSTR .EQ. MATCH
      ELSE
         LMATCH = ISTRLEN (MATCH)
         LMIN = MIN (LMATCH, NLET)
         LINSTR = ISTRLEN (INSTR)
         IF ((LINSTR .LE. LMATCH) .AND. (LINSTR .GE. LMIN)) THEN
            IF (LMIN .LT. LINSTR) LMIN = LINSTR
            MATCHSTR = INSTR(:LMIN) .EQ. MATCH(:LMIN)
         ELSE
            MATCHSTR = .FALSE.
         END IF
      END IF

      RETURN
      END

C=======================================================================
      SUBROUTINE QUOTESYMBOL (INQUOT, STRING, *)
C=======================================================================

C   --*** QUOTESYMBOL *** (ff) Convert symbols in string
C   --   Written by Amy Gilkey - revised 03/14/95
C   --
C   --QUOTESYMBOL searches for a symbol name in a string (enclosed by single
C   --quotes) and replaces the symbol.
C   --
C   --Parameters:
C   --   INQUOT - IN - if true, symbol will be enclosed by ''name';
C   --      if false, symbol will be enclosed by 'name'
C   --   STRING - IN/OUT - the input string; returned with symbols replaced
C   --   * - return statement if command error; message is printed

      IMPLICIT NONE

      LOGICAL INQUOT
      CHARACTER*(*) STRING

      INTEGER IS, IS1, IE
      INTEGER L
      CHARACTER*128 TXTMSG

  100 CONTINUE
      IF (INQUOT) THEN
         IS = INDEX (STRING, '''''')
         IS1 = IS + 1
      ELSE
         IS = INDEX (STRING, '''')
         IS1 = IS
      END IF
      IF (IS .GT. 0) THEN
         IE = INDEX (STRING(IS1+1:), '''') + IS1
         IF (IE .LE. IS1) THEN
            TXTMSG = 'Quotes do not match in filename'
            CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
            GOTO 110
         END IF
         CALL EXSYMBOL (STRING(IS1+1:IE-1), TXTMSG, L)
         IF (L .LE. 0) THEN
            TXTMSG = 'Symbol ' // STRING(IS1+1:IE-1)
     &         // ' is undefined'
            CALL QAMESSAG (-1, 'CMDERR', TXTMSG)
            GOTO 110
         END IF
         STRING = STRING(1:IS-1) // TXTMSG(:L) // STRING(IE+1:)
         GOTO 100
      END IF

      RETURN

  110 CONTINUE
      RETURN 1
      END
C CMS REPLACEMENT HISTORY, Element STP___FFMISC_RTNS.FOR
C *1     1-NOV-1995 11:24:21 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP___FFMISC_RTNS.FOR
