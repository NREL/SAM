C=======================================================================
      SUBROUTINE CHKEQNSYN (NUMFLD, INTYP, CFIELD, IFIELD, RFIELD,
     &   MAXENT, IXENTS, IXENTE, NAMENT, TYPENT, INXENT, VALENT, NERR)
C=======================================================================

C   --*** CHKEQNSYN *** (PCCSRC) Assign equation entries and checks syntax
C   --   Written by Amy Gilkey - revised 01/17/93
C   --
C   --CHKEQNSYN assigns the equation entries from the input fields.
C   --It assigns NAMENT, TYPENT, INXENT, VALENT for each entry.
C   --Entries may be added.  The equation syntax is also checked.
C   --The equation is returned in postfix form.
C   --
C   --Parameters:
C   --   NUMFLD - IN - the number of input entries
C   --   INTYP - IN - the free-field reader entry type
C   --      0 = name string
C   --      1 = real number
C   --      2 = integer number
C   --      3 = character or **
C   --   CFIELD - IN - character string fields
C   --   IFIELD - IN - integer fields
C   --   RFIELD - IN - real fields
C   --   MAXENT - IN - the maximum number of entries + 1
C   --   IXENTS - IN - the index of the starting entry (as in /TRNDEF/)
C   --   IXENTE - OUT - the index of the ending entry (as in /TRNDEF/)
C   --   NAMENT - OUT - the equation entries (as in /TRNDEF/)
C   --   TYPENT - OUT - the type of each equation entry (as in /TRNDEF/)
C   --   INXENT - OUT - based on TYPENT (as in /TRNDEF/)
C   --   VALENT - OUT - based on TYPENT (as in /TRNDEF/)
C   --   NERR - IN/OUT - the number of errors in the equation, may be set

      IMPLICIT NONE

      INTEGER NUMFLD
      INTEGER       INTYP(NUMFLD)
      CHARACTER*(*) CFIELD(NUMFLD)
      INTEGER       IFIELD(NUMFLD)
      REAL          RFIELD(NUMFLD)
      INTEGER MAXENT
      INTEGER IXENTS, IXENTE
      CHARACTER*(*) NAMENT(*)
      CHARACTER*1 TYPENT(*)
      INTEGER INXENT(*)
      REAL VALENT(*)
      INTEGER NERR

      INTEGER ISTRLEN
      INTEGER ISTRFIND

      INTEGER I
      INTEGER IFLD
      INTEGER NENT
      INTEGER ISYM
      INTEGER INXF
      INTEGER NPEXP

      CHARACTER*20 ENT20
C      --ENT20 - long equation entry, before trucation

      CHARACTER*8 ENT, LSTENT
C      --ENT, LSTENT - single equation entries

      CHARACTER*11 OPSYM
      SAVE OPSYM
C      --OPSYM - the ordered operators, one to a character, order is used
C      --   to determine the operator INXENT, space is reserved for
C      --   end of line operator (# is 0) and unary plus/minus (~ is 1,2)

      INTEGER NUMFNC
      PARAMETER (NUMFNC=12)
      CHARACTER*8 FNCNAM(NUMFNC)
      INTEGER NPARM(NUMFNC)
      SAVE FNCNAM, NPARM
C      --FNCNAM(i) - the name for function i
C      --NPARM(i) - the required number of parameters for function i

      DATA OPSYM / '  +-*/^(),=' /
C      --Note first space to reserve index 1/2 for unary plus/minus

      DATA (FNCNAM(I), NPARM(I), I = 1, NUMFNC) /
     1   'ABS     ', 1, 'MAX     ',-1, 'MIN     ',-1,
     2   'SQRT    ', 1, 'EXP     ', 1,
     3   'LOG     ', 1, 'LOG10   ', 1,
     4   'ENVMAX  ', 1, 'ENVMIN  ', 1,
     5   'IFLT0   ', 3, 'IFEQ0   ', 3, 'IFGT0   ', 3 /

C   --Assign TYPENT Constant, Operator, Variable or Function and
C   --set INXENT, VALENT

      IFLD = 0
      NENT = IXENTS-1
  100 CONTINUE
      IFLD = IFLD + 1
      IF (IFLD .LE. NUMFLD) THEN

         ENT20 = CFIELD(IFLD)

         NENT = NENT + 1
         IF (NENT .GT. MAXENT) GOTO 110
         NAMENT(NENT) = ENT20
         TYPENT(NENT) = ' '
         INXENT(NENT) = -999
         VALENT(NENT) = -999.0

         IF (INTYP(IFLD) .EQ. 0) THEN

C         --Distinguish between variables and functions (functions names are
C         --followed by '(')

            IF (CFIELD(IFLD+1) .EQ. '(') THEN

C            --Function - store name

               NAMENT(NENT) = ENT20
               TYPENT(NENT) = 'F'
               INXENT(NENT) = -999
               VALENT(NENT) = 0.0

            ELSE

C            --Variable - store name and node/element specifier and time
C            --specifier

               IF (ENT20(9:) .NE. ' ') THEN
                  CALL QAMESSAG (-1, 'CMD',
     &               'WARNING - ' // ENT20(:ISTRLEN(ENT20))
     &               // ' is truncated to "' // ENT20(1:8) // '"')
                  ENT20(9:) = ' '
               END IF

               NAMENT(NENT) = ENT20(1:8)
               TYPENT(NENT) = 'V'
               INXENT(NENT) = -999
               VALENT(NENT) = -999.0
            END IF

         ELSE IF ((INTYP(IFLD) .EQ. 1) .OR. (INTYP(IFLD) .EQ. 2)) THEN

C         --Constant - store value

            NAMENT(NENT) = ENT20(1:8)
            TYPENT(NENT) = 'C'
            VALENT(NENT) = RFIELD(IFLD)

         ELSE IF (INTYP(IFLD) .EQ. 3) THEN

C         --Operator - store index and number of operands

            NAMENT(NENT) = ENT20(1:1)
            TYPENT(NENT) = 'O'
            ISYM = INDEX (OPSYM, ENT20(1:1))
            INXENT(NENT) = ISYM
            VALENT(NENT) = 2.0
            IF (ISYM .EQ. 0) THEN
               NERR = NERR + 1
               CALL QAMESSAG (-1, 'CMD',
     &            '"' // ENT20(1:1) // '" is not a valid operator')
            END IF
         END IF

         GOTO 100
      END IF
  110 CONTINUE

      IXENTE = NENT
      IF (NENT .GE. MAXENT) THEN
         CALL QAMESSAG (-1, 'CMD', 'Too many equation entries')
         NERR = NERR + 1
         IXENTE = MAXENT
      END IF

C   --Temporarily add a '#' to mark the end of the equation to eliminate
C   --special end checks

      NAMENT(IXENTE+1) = 'line end'
      TYPENT(IXENTE+1) = 'O'

C   --Find unary '+' or '-' and convert to constant or unary operator

      DO 120 NENT = IXENTS, IXENTE

         ENT = NAMENT(NENT)

         IF ((ENT .EQ. '+') .OR. (ENT .EQ. '-')) THEN

            IF (NENT .GT. 1) THEN
               LSTENT = NAMENT(NENT-1)
            ELSE
               LSTENT = '='
            END IF

            IF ((LSTENT .EQ. '=') .OR. (LSTENT .EQ. '(')
     &         .OR. (LSTENT .EQ. ',')) THEN
C            --A unary '+' or '-' is found

               IF ((ENT .EQ. '-') .AND. (TYPENT(NENT+1) .EQ. 'C')) THEN

C               --If it is a negative constant, negate the constant and change
C               --to '+'

                  ENT = '+'
                  NAMENT(NENT) = '-' // NAMENT(NENT)
                  VALENT(NENT+1) = - VALENT(NENT+1)
               END IF

C            --It is a signed expression, change to a unary plus/minus

               NAMENT(NENT) = '~'
               VALENT(NENT) = 1.0
               IF (ENT .EQ. '+') THEN
                  INXENT(NENT) = 1
               ELSE
                  INXENT(NENT) = 2
               END IF
            END IF
         END IF
  120 CONTINUE

C   --Check for valid equation entry

      IF (TYPENT(IXENTS) .NE. 'V') THEN
         CALL QAMESSAG (-1, 'CMD', 'Assigned variable is not given')
      END IF

C   --Get equation in postfix form

      CALL POSTFIXEQN (IXENTS, IXENTE, NAMENT, TYPENT, INXENT, VALENT,
     &   NERR)

C   --Check the number of parameters for all functions

      DO 130 NENT = IXENTS, IXENTE

         IF (TYPENT(NENT) .EQ. 'F') THEN

C         --Locate the function name in the function table

            INXF = ISTRFIND (NAMENT(NENT), NUMFNC, FNCNAM)

C         --Store function index

            IF (INXF .GT. 0) THEN
               INXENT(NENT) = INXF
            ELSE
               NERR = NERR + 1
               ENT = NAMENT(NENT)
               CALL QAMESSAG (-1, 'CMD',
     &            'Function "' // ENT(:ISTRLEN(ENT)) //
     &            '" is undefined or a variable is followed by "("')
            END IF

C         --Get number of parameters

            IF (INXF .GT. 0) THEN
               NPEXP = NPARM(INXF)
            ELSE
               NPEXP = -999
            END IF

C         --Set number of parameters for function if may vary
            IF (NPEXP .LT. 0) NPEXP = INT (VALENT(NENT))

            IF (INT (VALENT(NENT)) .NE. NPEXP) THEN
               NERR = NERR + 1
               ENT = NAMENT(NENT)
               CALL QAMESSAG (-1, 'CMD',
     &            'Wrong number of parameters for function '
     &            // ENT(:ISTRLEN(ENT)))
            END IF
         END IF
  130 CONTINUE

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_CHKEQNSYN.FOR
C *2    14-MAY-1996 13:54:53 APGILKE "Change to functions IFLT0, IFEQ0, IFGT0"
C *1     1-NOV-1995 11:24:00 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_CHKEQNSYN.FOR
