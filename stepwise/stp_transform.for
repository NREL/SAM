C=======================================================================
      SUBROUTINE TRANSFORM (NUMOBS, XYRAW, MAXSTK, STACK)
C=======================================================================

C   --*** TRANSFORM *** (STEPWISE) Evaluate the transformation variables
C   --   Written by Amy Gilkey - revised 05/24/95
C   --
C   --TRANSFORM evaluates the transformation equations and assigns the
C   --transformation variables.
C   --
C   --Parameters:
C   --   NUMOBS - IN - the XYRAW and STACK dimension
C   --   XYRAW - IN/OUT - the input variables and returned transformation
C   --      variables
C   --   MAXSTK - IN - the maximum STACK size
C   --   STACK - SCRATCH - the evaluation stack
C   --   * - return statement if fatal error occurs; message is printed

      IMPLICIT NONE

      INCLUDE 'stp_transform_common.inc'

      INTEGER NUMOBS
      REAL XYRAW(NUMOBS,*)
      INTEGER MAXSTK
      REAL STACK(NUMOBS,*)

      INTEGER ISTRLEN

      INTEGER ITRN
      INTEGER IXENTS
      INTEGER ITOS
      INTEGER NENT
      INTEGER IVAR
      INTEGER NOP
      INTEGER IOP1, IOP2
      INTEGER L
      INTEGER J
      INTEGER NPARM
      INTEGER IPARM
      INTEGER IXLAST
      INTEGER IXPARM
      INTEGER IANS
      REAL X

      DO 510 ITRN = 1, NUMTRN

         IXENTS = IXTDEF(ITRN-1)+1

C      --Evaluate the equation

         ITOS = 0
C         --ITOS - the current top of the stack

         DO 500 NENT = IXENTS+1, IXTDEF(ITRN)

            IF (TYPENT(NENT) .EQ. 'C') THEN

C            --Constant - store in stack(1,itos)

               ITOS = ITOS + 1
               IF (ITOS .GT. MAXSTK) GOTO 520
               CALL INIREAL (NUMOBS, VALENT(NENT), STACK(1,ITOS))

            ELSE IF (TYPENT(NENT) .EQ. 'V') THEN

C            --Variable - store all its values on the stack

               ITOS = ITOS + 1
               IF (ITOS .GT. MAXSTK) GOTO 520
               IVAR = INXENT(NENT)
               CALL CPYREAL (NUMOBS, XYRAW(1,IVAR), STACK(1,ITOS))

            ELSE IF (TYPENT(NENT) .EQ. 'O') THEN

C            --Operator - pop operands and push result

               IF (INT (VALENT(NENT)) .LE. 1) THEN
C               --Handle unary operator

                  NOP = 1
                  IOP1 = ITOS
C                  --IOP1 - the operand index

               ELSE
C               --Handle binary operator

                  NOP = 2
                  IOP1 = ITOS - 1
                  IOP2 = ITOS
C                  --IOP1, IOP2 - the operand indices

                  ITOS = ITOS - 2 + 1
                  IF (ITOS .LE. 0) GOTO 520
               END IF

               L = INXENT(NENT)
               GOTO (220, 100, 120, 140, 160, 180, 200), L

               CALL QAMESSAG (-1, '+PROGRAM', 'Invalid operator "'
     &            // NAMENT(NENT)(1:1) // '" on stack')
               GOTO 530

C            --Unary minus
  100          CONTINUE
c#????               call chkfnc ('~', nament(nent))
               DO 110 J = 1, NUMOBS
                  STACK(J,ITOS) = - STACK(J,IOP1)
  110          CONTINUE
               GOTO 220

C            --Addition
  120          CONTINUE
c#????               call chkfnc ('+', nament(nent))
               DO 130 J = 1, NUMOBS
                  STACK(J,ITOS) = STACK(J,IOP1) + STACK(J,IOP2)
  130          CONTINUE
               GOTO 220

C            --Subtraction
  140          CONTINUE
c#????               call chkfnc ('-', nament(nent))
               DO 150 J = 1, NUMOBS
                  STACK(J,ITOS) = STACK(J,IOP1) - STACK(J,IOP2)
  150          CONTINUE
               GOTO 220

C            --Multiplication
  160          CONTINUE
c#????               call chkfnc ('*', nament(nent))
               DO 170 J = 1, NUMOBS
                  STACK(J,ITOS) = STACK(J,IOP1) * STACK(J,IOP2)
  170          CONTINUE
               GOTO 220

C            --Division
  180          CONTINUE
c#????               call chkfnc ('/', nament(nent))
               DO 190 J = 1, NUMOBS
                  STACK(J,ITOS) = STACK(J,IOP1) / STACK(J,IOP2)
  190          CONTINUE
               GOTO 220

C            --Exponentiation
  200          CONTINUE
c#????               call chkfnc ('^', nament(nent))
               DO 210 J = 1, NUMOBS
                  STACK(J,ITOS) = STACK(J,IOP1) ** STACK(J,IOP2)
  210          CONTINUE
               GOTO 220

  220          CONTINUE

            ELSE IF (TYPENT(NENT) .EQ. 'F') THEN

C            --Function - pop parameters and push result

               NPARM = INT (VALENT(NENT))
               IPARM = ITOS - NPARM + 1
C               --IPARM - the index of the first function parameter

C            --The equality ITOS = IPARM is assumed in many functions
               ITOS = IPARM
               IF (ITOS .LE. 0) GOTO 520

               L = INXENT(NENT)
               GOTO (230, 250, 280, 310, 330, 350, 370, 390, 410,
     &            430, 450, 470), L

               CALL QAMESSAG (-1, '+PROGRAM', 'Function "'
     &            // NAMENT(NENT)(:ISTRLEN(NAMENT(NENT)))
     &            // '" is undefined')
               GOTO 530

C            --ABS
  230          CONTINUE
c#????               call chkfnc ('ABS', nament(nent))
               DO 240 J = 1, NUMOBS
                  STACK(J,ITOS) = ABS (STACK(J,IPARM))
  240          CONTINUE
               GOTO 490

C            --MAX
  250          CONTINUE
c#????               call chkfnc ('MAX', nament(nent))
               IXLAST = IPARM
               DO 270 IXPARM = IPARM+2-1, IPARM+NPARM-1
                  DO 260 J = 1, NUMOBS
                     STACK(J,ITOS) =
     &                  MAX (STACK(J,IXLAST), STACK(J,IXPARM))
                     IXLAST = ITOS
  260             CONTINUE
  270          CONTINUE
               GOTO 490

C            --MIN
  280          CONTINUE
c#????               call chkfnc ('MIN', nament(nent))
               IXLAST = IPARM
               DO 300 IXPARM = IPARM+2-1, IPARM+NPARM-1
                  DO 290 J = 1, NUMOBS
                     STACK(J,ITOS) =
     &                  MIN (STACK(J,IXLAST), STACK(J,IXPARM))
  290             CONTINUE
                  IXLAST = ITOS
  300          CONTINUE
               GOTO 490

C            --SQRT
  310          CONTINUE
c#????               call chkfnc ('SQRT', nament(nent))
               DO 320 J = 1, NUMOBS
                  STACK(J,ITOS) = SQRT (STACK(J,IPARM))
  320          CONTINUE
               GOTO 490

C            --EXP
  330          CONTINUE
c#????               call chkfnc ('EXP', nament(nent))
               DO 340 J = 1, NUMOBS
                  STACK(J,ITOS) = EXP (STACK(J,IPARM))
  340          CONTINUE
               GOTO 490

C            --LOG
  350          CONTINUE
c#????               call chkfnc ('LOG', nament(nent))
               DO 360 J = 1, NUMOBS
                  STACK(J,ITOS) = LOG (STACK(J,IPARM))
  360          CONTINUE
               GOTO 490

C            --LOG10
  370          CONTINUE
c#????               call chkfnc ('LOG10', nament(nent))
               DO 380 J = 1, NUMOBS
                  STACK(J,ITOS) = LOG10 (STACK(J,IPARM))
  380          CONTINUE
               GOTO 490

C            --ENVMAX
  390          CONTINUE
c#????               call chkfnc ('ENVMAX', nament(nent))
               X = STACK(1,IPARM)
               DO 400 J = 2, NUMOBS
                  X = MAX (X, STACK(J,IPARM))
  400          CONTINUE
               CALL INIREAL (NUMOBS, X, STACK(1,ITOS))
               GOTO 490

C            --ENVMIN
  410          CONTINUE
c#????               call chkfnc ('ENVMIN', nament(nent))
               X = STACK(1,IPARM)
               DO 420 J = 2, NUMOBS
                  X = MIN (X, STACK(J,IPARM))
  420          CONTINUE
               CALL INIREAL (NUMOBS, X, STACK(1,ITOS))
               GOTO 490

C            --IFLT0
  430          CONTINUE
               DO 440 J = 1, NUMOBS
                  IF (STACK(J,IPARM+0) .LT. 0.0) THEN
                     STACK(J,ITOS) = STACK(J,IPARM+1)
                  ELSE
                     STACK(J,ITOS) = STACK(J,IPARM+2)
                  END IF
  440          CONTINUE
               GOTO 490

C            --IFEQ0
  450          CONTINUE
               DO 460 J = 1, NUMOBS
                  IF (STACK(J,IPARM+0) .EQ. 0.0) THEN
                     STACK(J,ITOS) = STACK(J,IPARM+1)
                  ELSE
                     STACK(J,ITOS) = STACK(J,IPARM+2)
                  END IF
  460          CONTINUE
               GOTO 490

C            --IFGT0
  470          CONTINUE
               DO 480 J = 1, NUMOBS
                  IF (STACK(J,IPARM+0) .GT. 0.0) THEN
                     STACK(J,ITOS) = STACK(J,IPARM+1)
                  ELSE
                     STACK(J,ITOS) = STACK(J,IPARM+2)
                  END IF
  480          CONTINUE
               GOTO 490
            END IF

  490       CONTINUE
  500    CONTINUE

C      --Store the equation result into XYRAW

         IANS = INXENT(IXENTS)
         CALL CPYREAL (NUMOBS, STACK(1,ITOS), XYRAW(1,IANS))

  510 CONTINUE

      RETURN

  520 CONTINUE
      CALL QAMESSAG (-1, '+PROGRAM',
     &   'Program stack problem, type ' // TYPENT(NENT))

  530 CONTINUE
      CALL QAABORT ('Transformation problem')
      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_TRANSFORM.FOR
C *2    14-MAY-1996 13:55:01 APGILKE "Change to functions IFLT0, IFEQ0, IFGT0"
C *1     1-NOV-1995 11:24:20 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_TRANSFORM.FOR
