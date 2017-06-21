C=======================================================================
      SUBROUTINE FFRDKEYS (KIN, KOUT, PROMPT, IOSTAT,
     &   KEYWRD, MAXPRM, NUMPRM, CPARAM,
     &   MAXVAL, NUMVAL, IXVAL, ITYVAL, CVALUE, IVALUE, RVALUE)
C=======================================================================

C   --*** FFRDKEYS *** (CAMCON_LIB) Free-field keyword-parameter reader
C   --   Revised by Amy Gilkey - revised 09/20/94
C   --   Free-field reader taken from the SUPES library
C   --
C   --FFRDKEYS reads a record and parses the record into fields.  The line
C   --should be in the form:
C   --   {keyword} {param_1}={value},...,{value} ... &
C   --      {param_n}={value},...,{value}
C   --
C   --See routine FFRDFLDS for more information on separators, etc.  FFRDKEYS
C   --differs from FFRDFLDS in that "=" is a separator.
C   --
C   --Parameters:
C   --   KIN - IN integer - the unit from which to read input
C   --   KOUT - IN integer - the unit to which to echo input
C   --   PROMPT - IN char*(*) - the prompt string
C   --   IOSTAT - OUT integer - the ANSI FORTRAN I/O status;
C   --      1000 = number of parameters or values overruns maximum
C   --   KEYWRD - OUT char*(*) - the input keyword
C   --   MAXPRM - IN integer - the maximum number of parameters allowed
C   --   NUMPRM - OUT integer - the number of input parameters; <0 if no
C   --      keyword or parameter (blank line)
C   --   CPARAM(MAXPRM) - OUT char*(*) - the input parameters
C   --   MAXVAL - IN integer - the maximum number of values
C   --   NUMVAL(MAXPRM) - OUT integer - the number of input values for each
C   --      parameter
C   --   IXVAL(MAXPRM) - OUT integer - the starting index of the input values
C   --      for each parameter
C   --   ITYVAL(MAXVAL) - OUT integer - the translation states of the
C   --      parameter values:
C   --      -1 = a null field
C   --       0 = a non-numeric field
C   --       1 = a REAL numeric field
C   --       2 = an INTEGER numeric field
C   --   CVALUE(MAXVAL) - OUT char*(*) - the character values of the
C   --      parameter values
C   --   IVALUE(MAXVAL) - OUT integer - the integer values of the
C   --      parameter values
C   --   RVALUE(MAXVAL) - OUT real - the floating-point values of the
C   --      parameter values

C   --Common Variables:
C   --   Sets SAVLIN of /FREEFIELD_COMMON/

      IMPLICIT NONE

C   --Common Variables:
      INCLUDE 'CAMLIB_FREEFIELD_COMMON.H'

C   --Parameter Definitions:
      INTEGER KIN, KOUT
      CHARACTER*(*) PROMPT
      INTEGER IOSTAT
      CHARACTER*(*) KEYWRD
      INTEGER MAXPRM, NUMPRM
      CHARACTER*(*) CPARAM(MAXPRM)
      INTEGER MAXVAL, NUMVAL(MAXPRM), IXVAL(MAXPRM)
      INTEGER ITYVAL(MAXVAL)
      CHARACTER*(*) CVALUE(MAXVAL)
      INTEGER IVALUE(MAXVAL)
      REAL RVALUE(MAXVAL)

      INTEGER IEQUAL
      PARAMETER (IEQUAL = 3)
      LOGICAL ANYEQ

      INTEGER I
      INTEGER IDCONT
      INTEGER IPRM
      INTEGER IXVALN
      INTEGER ILEFT
      INTEGER ISTOP
      INTEGER INTYPC, INTYPN
      INTEGER IFIELDC, IFIELDN
      REAL RFIELDC, RFIELDN
      CHARACTER*256 LINE, SCRLIN
      CHARACTER*256 CFIELDC, CFIELDN, TPARAM

************************************************************************

C   --Initialize output fields to their default values -
      KEYWRD = ' '
      NUMPRM = -999
      DO 100 IPRM = 1 , MAXPRM
         CPARAM(IPRM) = ' '
         NUMVAL(IPRM) = 0
         IXVAL(IPRM) = 0
  100 CONTINUE
      IXVALN = 0
      DO 110 I = 1, MAXVAL
         ITYVAL(I) = -1
         CVALUE(I) = ' '
         IVALUE(I) = 0
         RVALUE(I) = 0.0
  110 CONTINUE

C   --Initialize for the first call to FF0TKNK -
      IOSTAT = 0
      IDCONT = -1
      ILEFT = 1
      ISTOP = -999
      ANYEQ = .FALSE.

C--Process the keyword.

C   --Get the next two tokens (C = keyword, N = parameter) -
      CALL FF0TKNK (KIN, KOUT, PROMPT,
     &   IOSTAT, IDCONT, LINE, SCRLIN, ILEFT, ISTOP,
     &   INTYPC, CFIELDC, IFIELDC, RFIELDC,
     &   INTYPN, CFIELDN, IFIELDN, RFIELDN)
      CALL FF0TKNK (KIN, KOUT, PROMPT,
     &   IOSTAT, IDCONT, LINE, SCRLIN, ILEFT, ISTOP,
     &   INTYPC, CFIELDC, IFIELDC, RFIELDC,
     &   INTYPN, CFIELDN, IFIELDN, RFIELDN)

      IF (INTYPC .GE. -1) THEN
C      --The first token is the keyword -
         KEYWRD = CFIELDC
         NUMPRM = 0

         IF (INTYPN .EQ. IEQUAL) THEN
C         --Save the keyword for parameter (strip "*" from *keyword) -
            TPARAM = CFIELDC
            IF (TPARAM(1:1) .EQ. '*') TPARAM = TPARAM(2:)
C         --Ignore this equal -
            CALL FF0TKNK (KIN, KOUT, PROMPT,
     &         IOSTAT, IDCONT, LINE, SCRLIN, ILEFT, ISTOP,
     &         INTYPC, CFIELDC, IFIELDC, RFIELDC,
     &         INTYPN, CFIELDN, IFIELDN, RFIELDN)
         ELSE
            TPARAM = ' '
         END IF

C      --Get the next token (C = keyword, N = "=") -
         CALL FF0TKNK (KIN, KOUT, PROMPT,
     &      IOSTAT, IDCONT, LINE, SCRLIN, ILEFT, ISTOP,
     &      INTYPC, CFIELDC, IFIELDC, RFIELDC,
     &      INTYPN, CFIELDN, IFIELDN, RFIELDN)
      END IF

C--Process the parameters.

  120 CONTINUE
      IF (INTYPC .GE. -1) THEN
         NUMPRM = NUMPRM + 1
         IF (INTYPN .EQ. IEQUAL) ANYEQ = .TRUE.

         IF (NUMPRM .LE. MAXPRM) THEN
C         --Check if this is just a positional parameter value -
            IF (INTYPN .EQ. IEQUAL) THEN
C            --Store parameter -
               CPARAM(NUMPRM) = CFIELDC
C            --Get the next token (C = "=", N = first value) -
               CALL FF0TKNK (KIN, KOUT, PROMPT,
     &            IOSTAT, IDCONT, LINE, SCRLIN, ILEFT, ISTOP,
     &            INTYPC, CFIELDC, IFIELDC, RFIELDC,
     &            INTYPN, CFIELDN, IFIELDN, RFIELDN)
C            --Get the next token (C = first value, N = value or next parameter
C            --or "=") -
               CALL FF0TKNK (KIN, KOUT, PROMPT,
     &            IOSTAT, IDCONT, LINE, SCRLIN, ILEFT, ISTOP,
     &            INTYPC, CFIELDC, IFIELDC, RFIELDC,
     &            INTYPN, CFIELDN, IFIELDN, RFIELDN)
            ELSE
C            --Store parameter (may be keyword) and wipe out -
               CPARAM(NUMPRM) = TPARAM
               TPARAM = ' '
            END IF
C         --Start the parameter value section by storing its number and type -
            NUMVAL(NUMPRM) = 0
            IF (IXVALN + 1 .LE. MAXVAL) IXVAL(NUMPRM) = IXVALN + 1
  130       CONTINUE
            IF ((INTYPC .GE. -1) .AND. (INTYPN .NE. IEQUAL)) THEN
C            --Store each parameter value; stop when the lookahead token
C            --is an "=" (or at end of line) -
               IXVALN = IXVALN + 1
               IF (IXVALN .LE. MAXVAL) THEN
                  NUMVAL(NUMPRM) = NUMVAL(NUMPRM) + 1
                  ITYVAL(IXVALN) = INTYPC
                  CVALUE(IXVALN) = CFIELDC
                  IVALUE(IXVALN) = IFIELDC
                  RVALUE(IXVALN) = RFIELDC
               END IF
C            --Get the next token (C = value or next parameter, N = value or
C            --next parameter or "=") -
               CALL FF0TKNK (KIN, KOUT, PROMPT,
     &            IOSTAT, IDCONT, LINE, SCRLIN, ILEFT, ISTOP,
     &            INTYPC, CFIELDC, IFIELDC, RFIELDC,
     &            INTYPN, CFIELDN, IFIELDN, RFIELDN)
C            --If positional parameter value, stop at one value
               IF (ANYEQ) GOTO 130
            END IF
            GOTO 120
         END IF
      END IF

C   --If parameters or values overrun, set I/O status and adjust numbers
      IF (NUMPRM .GT. MAXPRM) THEN
         IOSTAT = 1000
         NUMPRM = MAXPRM
      END IF
      IF (IXVALN .GT. MAXVAL) THEN
         IOSTAT = 1000
      END IF

      RETURN
      END
