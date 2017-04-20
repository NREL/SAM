C=======================================================================
      SUBROUTINE CPYINT (LEN, IFROM, ITO)
C=======================================================================

C   --*** CPYINT *** (DEFAULT_LIB) Copy all integers in list
C   --   Written by Amy Gilkey - revised 11/03/87
C   --
C   --CPYINT copies all the integers in a list to another list.
C   --
C   --Parameters:
C   --   LEN - IN - the number of integers in the list
C   --   IFROM - IN - the input list
C   --   ITO - OUT - the copied list

      IMPLICIT NONE

      INTEGER LEN
      INTEGER IFROM(LEN), ITO(LEN)

      INTEGER I

      DO 100 I = 1, LEN
         ITO(I) = IFROM(I)
  100 CONTINUE

      RETURN
      END

C=======================================================================
      SUBROUTINE CPYLOG (LEN, LFROM, LTO)
C=======================================================================

C   --*** CPYLOG *** (DEFAULT_LIB) Copy all logicals in list
C   --   Written by Amy Gilkey - revised 11/03/87
C   --
C   --CPYLOG copies all the logicals in a list to another list.
C   --
C   --Parameters:
C   --   LEN - IN - the number of logicals in the list
C   --   LFROM - IN - the input list
C   --   LTO - OUT - the copied list

      IMPLICIT NONE

      INTEGER LEN
      LOGICAL LFROM(LEN), LTO(LEN)

      INTEGER I

      DO 100 I = 1, LEN
         LTO(I) = LFROM(I)
  100 CONTINUE

      RETURN
      END

C=======================================================================
      SUBROUTINE CPYREAL (LEN, RFROM, RTO)
C=======================================================================

C   --*** CPYREAL *** (DEFAULT_LIB) Copy all real numbers in list
C   --   Written by Amy Gilkey - revised 03/15/88
C   --
C   --CPYREAL copies all the real numbers in a list to another list.
C   --
C   --Parameters:
C   --   LEN - IN - the number of real numbers in the list
C   --   RFROM - IN - the input list
C   --   RTO - OUT - the copied list

      IMPLICIT NONE

      INTEGER LEN
      REAL RFROM(LEN), RTO(LEN)

      INTEGER I

      DO 100 I = 1, LEN
         RTO(I) = RFROM(I)
  100 CONTINUE

      RETURN
      END

C=======================================================================
      SUBROUTINE INICOUNT (LEN, ITO)
C=======================================================================

C   --*** INICOUNT *** (DEFAULT_LIB) Initialize all integers in list to count
C   --   Written by Amy Gilkey - revised 05/14/90
C   --
C   --INICOUNT initializes all the integers in a list to the list index.
C   --
C   --Parameters:
C   --   LEN - IN - the number of integers in the list
C   --   ITO - OUT - the initialized list

      IMPLICIT NONE

      INTEGER LEN
      INTEGER ITO(LEN)

      INTEGER I

      DO 100 I = 1, LEN
         ITO(I) = I
  100 CONTINUE

      RETURN
      END

C=======================================================================
      SUBROUTINE INIINT (LEN, IFROM, ITO)
C=======================================================================

C   --*** INIINT *** (DEFAULT_LIB) Initialize all integers in list
C   --   Written by Amy Gilkey - revised 03/15/88
C   --
C   --INIINT initializes all the integers in a list to a specified value.
C   --
C   --Parameters:
C   --   LEN - IN - the number of integers in the list
C   --   IFROM - IN - the initial value
C   --   ITO - OUT - the initialized list

      IMPLICIT NONE

      INTEGER LEN
      INTEGER IFROM
      INTEGER ITO(LEN)

      INTEGER I

      DO 100 I = 1, LEN
         ITO(I) = IFROM
  100 CONTINUE

      RETURN
      END

C=======================================================================
      SUBROUTINE INILOG (LEN, LFROM, LTO)
C=======================================================================

C   --*** INILOG *** (DEFAULT_LIB) Initialize all logicals in list
C   --   Written by Amy Gilkey - revised 03/15/88
C   --
C   --INILOG initializes all the logicals in a list to a specified value.
C   --
C   --Parameters:
C   --   LEN - IN - the number of logicals in the list
C   --   LFROM - IN - the initial value
C   --   LTO - OUT - the initialized list

      IMPLICIT NONE

      INTEGER LEN
      LOGICAL LFROM
      LOGICAL LTO(LEN)

      INTEGER I

      DO 100 I = 1, LEN
         LTO(I) = LFROM
  100 CONTINUE

      RETURN
      END

C=======================================================================
      SUBROUTINE INIREAL (LEN, RFROM, RTO)
C=======================================================================

C   --*** INIREAL *** (DEFAULT_LIB) Initialize all real numbers in list
C   --   Written by Amy Gilkey - revised 03/15/88
C   --
C   --INIREAL initializes all the real numbers in a list to a specified value.
C   --
C   --Parameters:
C   --   LEN - IN - the number of real numbers in the list
C   --   RFROM - IN - the initial value
C   --   RTO - OUT - the initialized list

      IMPLICIT NONE

      INTEGER LEN
      REAL RFROM
      REAL RTO(LEN)

      INTEGER I

      DO 100 I = 1, LEN
         RTO(I) = RFROM
  100 CONTINUE

      RETURN
      END

C=======================================================================
      SUBROUTINE INISTR (LEN, IFROM, ITO)
C=======================================================================

C   --*** INISTR *** (DEFAULT_LIB) Initialize all strings in list
C   --   Written by Amy Gilkey - revised 03/15/88
C   --
C   --INISTR initializes all the strings in a list to a specified value.
C   --
C   --Parameters:
C   --   LEN - IN - the number of strings in the list
C   --   IFROM - IN - the initial value
C   --   ITO - OUT - the initialized list

      IMPLICIT NONE

      INTEGER LEN
      CHARACTER*(*) IFROM
      CHARACTER*(*) ITO(LEN)

      INTEGER I

      DO 100 I = 1, LEN
         ITO(I) = IFROM
  100 CONTINUE

      RETURN
      END

C=======================================================================
      INTEGER FUNCTION INTADD (LENLST, INTLST)
C=======================================================================

C   --*** INTADD *** (DEFAULT_LIB) Add all integers in list
C   --   Written by Amy Gilkey - revised 11/10/87
C   --
C   --INTADD returns the sum of all the integers in a list.
C   --
C   --Parameters:
C   --   LENLST - IN - the number of integers in the list
C   --   INTLST - IN - the list of integers to be added

      IMPLICIT NONE

      INTEGER LENLST
      INTEGER INTLST(LENLST)

      INTEGER I

      INTADD = 0
      DO 100 I = 1, LENLST
         INTADD = INTADD + INTLST(I)
  100 CONTINUE

      RETURN
      END

C=======================================================================
      SUBROUTINE INTSTR (LNGSTR, INUM, ISTR, LSTR)
C=======================================================================

C   --*** INTSTR *** (DEFAULT_LIB) Convert integer to string
C   --   Written by Amy Gilkey - revised 05/21/95
C   --
C   --INTSTR converts an integer number into string of a given length
C   --(right justified).
C   --
C   --Parameters:
C   --   LNGSTR - IN - the number of digits in the string; <= 0 if smallest
C   --      length
C   --   INUM - IN - the integer to be converted
C   --   ISTR - OUT - the integer string
C   --   LSTR - OUT - the length of the number string

      IMPLICIT NONE

      INTEGER LNGSTR
      INTEGER INUM
      CHARACTER*(*) ISTR
      INTEGER LSTR

      INTEGER IDUM
      CHARACTER*5 FFMT

      IF (LNGSTR .LE. 0) THEN

C      --Convert number to string of smallest length

         WRITE (FFMT, 10000, IOSTAT=IDUM) MIN (32, LEN (ISTR))

         WRITE (ISTR, FFMT, IOSTAT=IDUM) INUM
         CALL STRCMPRS (ISTR, LSTR)

      ELSE

C      --Convert number to string of fixed length

         LSTR = MIN (LNGSTR, LEN (ISTR))

         WRITE (FFMT, 10000, IOSTAT=IDUM) LSTR
10000     FORMAT ('(I', I2.2, ')')

         WRITE (ISTR, FFMT, IOSTAT=IDUM) INUM
      END IF

      RETURN
      END

C=======================================================================
      INTEGER FUNCTION LOCINT (INT, LENLST, INTLST)
C=======================================================================

C   --*** LOCINT *** (DEFAULT_LIB) Find integer in list
C   --   Written by Amy Gilkey - revised 11/10/87
C   --
C   --LOCINT returns the index of the given integer in a list of integers.
C   --If the integer is not in the list, returns 0.
C   --
C   --Parameters:
C   --   INT - IN - the integer to be searched for
C   --   LENLST - IN - the number of integers in the list
C   --   INTLST - IN - the list of integers to be searched

      IMPLICIT NONE

      INTEGER INT
      INTEGER LENLST
      INTEGER INTLST(LENLST)

      DO 100 LOCINT = 1, LENLST
         IF (INT .EQ. INTLST(LOCINT)) GOTO 110
  100 CONTINUE
      LOCINT = 0

  110 CONTINUE
      RETURN
      END

C=======================================================================
      INTEGER FUNCTION NUMEQLOG (TORF, LENLST, LOGLST)
C=======================================================================

C   --*** NUMEQLOG *** (DEFAULT_LIB) Count number occurances of logical in list
C   --   Written by Amy Gilkey - revised 12/21/87
C   --
C   --NUMEQLOG returns the number of times the given logical occurs in a list
C   --of logicals.
C   --
C   --Parameters:
C   --   TORF - IN - the logical to be counted
C   --   LENLST - IN - the number of logicals in the list
C   --   LOGLST - IN - the list of logicals to be searched

      IMPLICIT NONE

      LOGICAL TORF
      INTEGER LENLST
      LOGICAL LOGLST(LENLST)

      INTEGER I

      NUMEQLOG = 0
      DO 100 I = 1, LENLST
         IF (TORF .EQV. LOGLST(I)) NUMEQLOG = NUMEQLOG + 1
  100 CONTINUE

      RETURN
      END

C=======================================================================
      SUBROUTINE REALSTR (NNUM, NSIG, RNUM, RSTR, LSTR)
C=======================================================================

C   --*** REALSTR *** (DEFAULT_LIB) Convert real numbers to strings
C   --   Written by Amy Gilkey - revised 12/11/87
C   --
C   --REALSTR converts a set of real numbers into a consistent set of
C   --strings.  It will convert to engineering notation with all
C   --exponents the same, if possible.
C   --
C   --Parameters:
C   --   NNUM - IN - the number of real numbers in the set
C   --   NSIG - IN - the maximum number of significant digits, max of 8
C   --   RNUM - IN - the array of real numbers to be converted
C   --   RSTR - OUT - the set of real number strings
C   --   LSTR - OUT - the maximum length of the number strings

      IMPLICIT NONE

      INTEGER NNUM
      INTEGER NSIG
      REAL RNUM(NNUM)
      CHARACTER*(*) RSTR(NNUM)
      INTEGER LSTR

      INTEGER IENGRX

      INTEGER ISIGN
      INTEGER MINE, MAXE
      INTEGER MAXES
      INTEGER MINE2
      INTEGER I
      INTEGER IE
      INTEGER NEWEXP
      INTEGER NWHOLE
      INTEGER NFRAC
      INTEGER NTOTAL
      INTEGER MINEXP, MAXEXP
      INTEGER IB
      INTEGER NB
      INTEGER IDUM
      REAL EXPDIV
      CHARACTER*20 BLANKS
      CHARACTER*10 SCRFMT
      CHARACTER STR20*20
      CHARACTER*15 FFMT

C   --Convert all to E notation and find the minimum and maximum exponent
C   --   MINE and MAXE are the minimum and maximum exponents
C   --   ISIGN is the number of digits for the sign
C   --      (0 if all positive, 1 if any number negative)

      BLANKS = ' '

      WRITE (SCRFMT, 10000, IOSTAT=IDUM) NSIG+7, NSIG
10000  FORMAT ('(0PE', I2.2, '.', I2.2, ')')

      ISIGN = 0
      MINE = 9999
      MAXE = -9999
      MAXES = MAXE
      DO 100 I = 1, NNUM
         IF (RNUM(I) .NE. 0.0) THEN
            WRITE (STR20(1:NSIG+7), SCRFMT, IOSTAT=IDUM) RNUM(I)
            READ (STR20(NSIG+5:NSIG+7), '(I3)', IOSTAT=IDUM) IE
            IF (MINE .GT. IE) MINE2 = MINE
            MINE = MIN (MINE, IE)
            MAXE = MAX (MAXE, IE)
            IF (RNUM(I) .LT. 0.0) THEN
               ISIGN = 1
               MAXES = MAX (MAXES, IE)
            END IF
         END IF
  100 CONTINUE

C   --Correct for one very small number (should be zero)

      IF ((MINE2 .LT. 1000) .AND. ((MINE2 - MINE) .GE. 6)) MINE = MINE2

C   --Handle all zero case

      IF (MINE .GT. MAXE) THEN
         MINE = 0
         MAXE = 0
         MAXES = 0
      END IF

C   --Determine the new exponent NEWEXP (use engineering notation)

      NEWEXP = IENGRX (MAXE, MINE)
      IF (ISIGN .EQ. 1) THEN
         IF (MAX (1, MAXE - NEWEXP) .GT. MAX (1, MAXES - NEWEXP))
     &      ISIGN = 0
      END IF

C   --Check if the numbers can all be sensibly converted to a common exponent

      IF (((MAXE - NEWEXP) .LE. 4)
     &   .AND. ((NEWEXP - MINE) .LE. 2)
     &   .AND. (-MINE .LT. (NSIG - MAXE))) THEN

C      --Determine the new F format
C      --   EXPDIV is the number to divide by to get the number
C      --      without an exponent
C      --   NWHOLE is the number of digits before the decimal
C      --   NFRAC is the number of digits after the decimal
C      --   NTOTAL is the total number of digits
C      --The new exponent is tagged on the end of the F-format number

         EXPDIV = 10.0 ** NEWEXP

         NWHOLE = MAX (1, MAXE - NEWEXP)
         NFRAC = MAX (0, MIN (NEWEXP - MINE + NSIG,
     &      NSIG - (MAXE - NEWEXP)))
         NTOTAL = ISIGN + NWHOLE + 1 + NFRAC
         IF (EXPDIV .NE. 0.0) THEN
            WRITE (FFMT, 10010, IOSTAT=IDUM) NTOTAL, NFRAC
10010        FORMAT ('(F', I2.2, '.', I2.2, ')')
         ELSE
            WRITE (FFMT, 10020, IOSTAT=IDUM) NTOTAL
10020        FORMAT ('(A', I2.2, 3X, ')')
         END IF

         IF (NEWEXP .EQ. 0) THEN
            LSTR = NTOTAL
         ELSE IF ((NEWEXP .LE. -10) .OR. (NEWEXP .GE. 10)) THEN
            WRITE (FFMT(8:15), 10030, IOSTAT=IDUM) NEWEXP
10030        FORMAT (',''E', SP, I3.2, ''')')
            LSTR = NTOTAL + 4
         ELSE
            WRITE (FFMT(8:15), 10040, IOSTAT=IDUM) NEWEXP
10040        FORMAT (',''E', SP, I2.1, ''')')
            LSTR = NTOTAL + 3
         END IF

C      --Convert all numbers to the new exponent by using the F format

         IF (EXPDIV .NE. 0.0) THEN
            DO 110 I = 1, NNUM
               WRITE (RSTR(I), FFMT, IOSTAT=IDUM) RNUM(I)/EXPDIV
  110       CONTINUE
         ELSE
            DO 120 I = 1, NNUM
               WRITE (RSTR(I), FFMT, IOSTAT=IDUM) '********************'
  120       CONTINUE
         END IF

      ELSE

C      --Do not try to use a common exponent, but use engineering notation;
C      --Algorithm as above

         LSTR = 0
         MINEXP = IENGRX (MINE, MINE)
         MAXEXP = IENGRX (MAXE, MAXE)

         DO 130 I = 1, NNUM
            WRITE (STR20(1:NSIG+7), SCRFMT, IOSTAT=IDUM) RNUM(I)
            READ (STR20(NSIG+5:NSIG+7), '(I3)', IOSTAT=IDUM) IE
            ISIGN = 0
            IF (RNUM(I) .LT. 0.0) ISIGN = 1

            NEWEXP = IENGRX (IE, IE)

            EXPDIV = 10.0 ** NEWEXP

            NWHOLE = MAX (1, IE - NEWEXP)
            NFRAC = MAX (0, MIN (NEWEXP - IE + NSIG,
     &         NSIG - (IE - NEWEXP)))
            IF ((RNUM(I) .EQ. 0.0) .AND. (MINE .GE. 0))
     &         NFRAC = NFRAC - 1
            NTOTAL = ISIGN + NWHOLE + 1 + NFRAC
            IF (EXPDIV .NE. 0.0) THEN
               WRITE (FFMT, 10010, IOSTAT=IDUM) NTOTAL, NFRAC
            ELSE
               WRITE (FFMT, 10020, IOSTAT=IDUM) NTOTAL
            END IF

            IF ((MINEXP .LE. -10) .OR. (MAXEXP .GE. 10)) THEN
               WRITE (FFMT(8:15), 10030, IOSTAT=IDUM) NEWEXP
               LSTR = MAX (LSTR, NTOTAL + 4)
            ELSE
               WRITE (FFMT(8:15), 10040, IOSTAT=IDUM) NEWEXP
               LSTR = MAX (LSTR, NTOTAL + 3)
            END IF

            IF (EXPDIV .NE. 0.0) THEN
               WRITE (RSTR(I), FFMT, IOSTAT=IDUM) RNUM(I)/EXPDIV
            ELSE
               WRITE (RSTR(I), FFMT, IOSTAT=IDUM) '********************'
            END IF
  130    CONTINUE

C      --Adjust the strings so that they are right-justified at
C      --a common length

         DO 140 I = 1, NNUM
            IB = INDEX (RSTR(I)(:LSTR), ' ')
            IF (IB .GT. 0) THEN
               NB = LSTR - IB + 1
               STR20 = RSTR(I)(:IB-1)
               RSTR(I) = BLANKS(:NB) // STR20
            END IF
  140    CONTINUE

      END IF

      RETURN
      END

C=======================================================================
      INTEGER FUNCTION IENGRX (IMAXE, IMINE)
C=======================================================================

C   --*** IENGX *** (DEFAULT_LIB) Internal to REALSTR
C   --   Written by Amy Gilkey - revised 02/14/86
C   --
C   --IENGRX returns the "best" engineering notation exponent for a
C   --minimum and maximum exponent.
C   --
C   --Parameters:
C   --   IMAXE - IN - the maximum exponent
C   --   IMINE - IN - the minimum exponent

      IMPLICIT NONE

      INTEGER IMAXE, IMINE

      IF (IMAXE .GT. 0) THEN
         IENGRX = INT ((IMAXE - 1) / 3) * 3
      ELSE
         IENGRX = INT ((IMAXE - 2) / 3) * 3
      END IF

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP___DEFAULT_LIB.FOR
C *1     1-NOV-1995 11:24:20 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP___DEFAULT_LIB.FOR
