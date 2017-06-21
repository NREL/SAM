C=======================================================================
      SUBROUTINE POSTFIXEQN (IXENTS, IXENTE,
     &   NAMENT, TYPENT, INXENT, VALENT, NERR)
C=======================================================================

C   --*** POSTFIXEQN *** (PCCSRC) Convert the equation to postfix form
C   --   Written by Amy Gilkey - revised 01/17/93
C   --
C   --POSTFIXEQN converts the equation to postfix form.  Parenthesis
C   --are eliminated.  Nothing is done if NERR is non-zero.
C   --
C   --Conversion is done using an operator stack.  Operators are
C   --moved to the operator stack and returned to the equation in
C   --postfix order.  Non-operators are simply moved up in the equation.
C   --Any operator which is moved to the stack has precedence over those
C   --already in the stack.  An operator of lower precedence will move
C   --the preceding stack operator to the equation.  A special case is
C   --made for a function.  It is pushed onto the operator stack, and
C   --popped back into the equation at the end of the parameters.
C   --
C   --Parameters:
C   --   IXENTS, IXENTE - IN/OUT - the indices of the starting and ending
C   --      entries (as in /TRNDEF/)
C   --   NAMENT - IN/OUT - the equation entries (as in /TRNDEF/)
C   --   TYPENT - IN/OUT - the type of each equation entry (as in /TRNDEF/)
C   --   INXENT - IN/OUT - based on TYPENT (as in /TRNDEF/)
C   --   VALENT - IN/OUT - based on TYPENT (as in /TRNDEF/)
C   --   NERR - IN/OUT - the number of errors in the equation, may be set

      IMPLICIT NONE

      INTEGER IXENTS, IXENTE
      CHARACTER*(*) NAMENT(*)
      CHARACTER*1 TYPENT(*)
      INTEGER INXENT(*)
      REAL VALENT(*)
      INTEGER NERR

      INTEGER ISTRLEN

      LOGICAL EXPOP, OK
      INTEGER J
      INTEGER IOPTOS
      INTEGER NENT
      INTEGER IEQTOS
      INTEGER IEQU
      INTEGER IACT
      CHARACTER*8 ENT, LSTENT, TOSENT

      INTEGER MAXEQN, MAXENT
      PARAMETER (MAXEQN=500, MAXENT=81)

      CHARACTER*8 NAMSTK(MAXENT)
      CHARACTER*1 TYPSTK(MAXENT)
      INTEGER     INXSTK(MAXENT)
      REAL        VALSTK(MAXENT)
C      --STK.. - the operand stack; holds the corresponding /ENT../ values
      INTEGER     IEQSTK(MAXENT)
C      --IEQSTK - the IOPTBL index for this operand

      INTEGER IEQUIV(0:11)
      SAVE IEQUIV
C      --IEQUIV - the IOPTBL index for the given operator

      INTEGER IOPTBL(-1:8,-1:8)
      SAVE IOPTBL
C      --IOPTBL - chart of stack actions to be performed
C      --IOPTBL(i,j) - i = top stack operator, j = current operator
C      --   = 1   Pop operator from stack into the equation, redo current
C      --   = 2   Push operator onto the stack
C      --   = 3   Pop matching '(' off the stack
C      --   = 4   Delete from equation
C      --   = 5   Process function parameters
C      --   = 6   Pop function from stack into the equation
C      --   = 7   Stop processing at end of equation
C      --   = 0   Error

C                   #  ~+ ~- +  -  *  /  ^  (  )  ,  =
      DATA IEQUIV / 0, 1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 8 /
C      --Index -1 is reserved for the function name

C                                  F  #  ~  +  *  ^  (  )  ,  =
      DATA (IOPTBL(-1,J),J=-1,8) / 2, 0, 2, 2, 2, 2, 5, 6, 5, 0 /
      DATA (IOPTBL(0,J), J=-1,8) / 2,-7, 2, 2, 2, 2, 2, 0, 0, 2 /
      DATA (IOPTBL(1,J), J=-1,8) / 2, 1, 0, 1, 1, 2, 2, 1, 1, 0 /
      DATA (IOPTBL(2,J), J=-1,8) / 2, 1, 2, 1, 2, 2, 2, 1, 1, 0 /
      DATA (IOPTBL(3,J), J=-1,8) / 2, 1, 2, 1, 1, 2, 2, 1, 1, 0 /
      DATA (IOPTBL(4,J), J=-1,8) / 2, 1, 2, 1, 1, 2, 2, 1, 1, 0 /
      DATA (IOPTBL(5,J), J=-1,8) / 2, 0, 2, 2, 2, 2, 2, 3, 0, 0 /
      DATA (IOPTBL(6,J), J=-1,8) / 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /
      DATA (IOPTBL(7,J), J=-1,8) / 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /
      DATA (IOPTBL(8,J), J=-1,8) / 2, 7, 2, 2, 2, 2, 2, 0, 0, 0 /

C   --Initialize the operator stack by putting '#' on it

      NAMSTK(1) = '#'
      TYPSTK(1) = 'O'
      INXSTK(1) = 0
      VALSTK(1) = -999.0
      IEQSTK(1) = 0
      IOPTOS = 1
C      --IOPTOS - the top of the operator stack

C   --Put '#' at end of the equation

      NENT = IXENTE+1
      NAMENT(NENT) = NAMSTK(1)
      TYPENT(NENT) = TYPSTK(1)
      INXENT(NENT) = INXSTK(1)
      VALENT(NENT) = VALSTK(1)

      EXPOP = .FALSE.
C      --EXPOP - true iff next entry is expected to be an operator

      IEQTOS = IXENTS - 1
C      --IEQTOS - the top of new postfix equation stack

      DO 120 NENT = IXENTS, IXENTE+1

         IF (TYPENT(NENT) .EQ. 'O') THEN
            ENT = NAMENT(NENT)
            IF ((ENT .EQ. '~') .OR. (ENT .EQ. '(')) THEN
               OK = (.NOT. EXPOP)
            ELSE
               OK = (EXPOP)
            END IF
            EXPOP = (ENT .EQ. ')')
         ELSE IF (TYPENT(NENT) .EQ. 'F') THEN
            OK = (.NOT. EXPOP)
            EXPOP = .FALSE.
         ELSE
            OK = (.NOT. EXPOP)
            EXPOP = .TRUE.
         END IF

         IF (.NOT. OK) THEN
            ENT = NAMENT(NENT)
            LSTENT = NAMENT(NENT-1)
            NERR = NERR + 1
            IF (ENT .EQ. '#') THEN
               CALL QAMESSAG (-1, 'CMD',
     &            'Ending "' // LSTENT(:ISTRLEN(LSTENT)) // '" found')
            ELSE IF (NENT .LE. 1) THEN
               CALL QAMESSAG (-1, 'CMD',
     &            'Starting "' // ENT(:ISTRLEN(ENT)) // '" found')
            ELSE
               CALL QAMESSAG (-1, 'CMD',
     &            'Consecutive "' // LSTENT(:ISTRLEN(LSTENT))
     &            // '" and "' // ENT(:ISTRLEN(ENT)) // '" found')
            END IF
         END IF

         IF ((TYPENT(NENT) .NE. 'O')
     &      .AND. (TYPENT(NENT) .NE. 'F')) THEN

C         --Move entry up in the equation

            IEQTOS = IEQTOS + 1
            IF (IEQTOS .NE. NENT) THEN
               NAMENT(IEQTOS) = NAMENT(NENT)
               TYPENT(IEQTOS) = TYPENT(NENT)
               INXENT(IEQTOS) = INXENT(NENT)
               VALENT(IEQTOS) = VALENT(NENT)
            END IF

         ELSE

  100       CONTINUE
            IF (TYPENT(NENT) .EQ. 'O') THEN
               IEQU = IEQUIV(INXENT(NENT))
            ELSE
               IEQU = -1
            END IF
            J = IEQSTK(IOPTOS)
            IACT = IOPTBL(J,IEQU)

C         --Check error

            IF (IACT .LE. 0) THEN
               NERR = NERR + 1
               ENT = NAMENT(NENT)
               TOSENT = NAMSTK(IOPTOS)
               LSTENT = NAMENT(NENT-1)
               IF (ENT .EQ. '=') THEN
                  CALL QAMESSAG (-1, 'CMD',
     &               'Misplaced "=" after "'
     &               // LSTENT(:ISTRLEN(LSTENT)) // '"')
               ELSE IF (ENT .EQ. ',') THEN
                  CALL QAMESSAG (-1, 'CMD',
     &               'Misplaced comma after "'
     &               // LSTENT(:ISTRLEN(LSTENT)) // '"')
               ELSE IF (ENT .EQ. ')') THEN
                  CALL QAMESSAG (-1, 'CMD',
     &               'Extra ")" after "'
     &               // LSTENT(:ISTRLEN(LSTENT)) // '"')
               ELSE IF (TOSENT .EQ. '(') THEN
                  CALL QAMESSAG (-1, 'CMD',
     &               'Mismatched "("')
               ELSE IF ((TYPSTK(IOPTOS) .EQ. 'F')
     &            .AND. (ENT .EQ. '#')) THEN
                  CALL QAMESSAG (-1, 'CMD',
     &               'Missing ")" for function '
     &               // TOSENT(:ISTRLEN(TOSENT)))
               ELSE
                  CALL QAMESSAG (-1, 'CMD',
     &               'PROBLEM: tos=' // TOSENT // ', entry=' // ENT)
               END IF

               IACT = IABS (IACT)
            END IF

  110       CONTINUE
            IF ((IACT .EQ. 1) .OR. (IACT .EQ. 6)) THEN

C            --Pop operator from stack onto the equation, and redo

               IEQTOS = IEQTOS + 1
               NAMENT(IEQTOS) = NAMSTK(IOPTOS)
               TYPENT(IEQTOS) = TYPSTK(IOPTOS)
               INXENT(IEQTOS) = INXSTK(IOPTOS)
               VALENT(IEQTOS) = VALSTK(IOPTOS)
               IOPTOS = IOPTOS - 1

               IF (IACT .EQ. 1) GOTO 100

            ELSE IF (IACT .EQ. 2) THEN

C            --Push operator onto the stack

               IOPTOS = IOPTOS + 1
               NAMSTK(IOPTOS) = NAMENT(NENT)
               TYPSTK(IOPTOS) = TYPENT(NENT)
               INXSTK(IOPTOS) = INXENT(NENT)
               IF (TYPENT(NENT) .EQ. 'O') THEN
                  VALSTK(IOPTOS) = VALENT(NENT)
               ELSE
                  VALSTK(IOPTOS) = 0.0
               END IF
               IEQSTK(IOPTOS) = IEQU

            ELSE IF (IACT .EQ. 3) THEN

C            --Pop matching '(' off the stack

               IOPTOS = IOPTOS - 1

            ELSE IF (IACT .EQ. 4) THEN

C            --Delete from equation
               CONTINUE

            ELSE IF (IACT .EQ. 5) THEN

               IF (ENT .EQ. '(') THEN
                  IF (INT (VALSTK(IOPTOS)) .LE. 0) THEN
C                  --Handle beginning '(' for function
                     VALSTK(IOPTOS) = VALSTK(IOPTOS) + 1.0
                  ELSE
C                  --Handle '(' within function
                     IACT = 2
                     GOTO 110
                  END IF
               ELSE
C               --Handle ',' for function
                  VALSTK(IOPTOS) = VALSTK(IOPTOS) + 1.0
               END IF

            ELSE IF (IACT .EQ. 7) THEN

C            --End processing equation
               CONTINUE

            END IF
         END IF

  120 CONTINUE

      IXENTE = IEQTOS

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_POSTFIXEQN.FOR
C *1     1-NOV-1995 11:24:06 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_POSTFIXEQN.FOR
