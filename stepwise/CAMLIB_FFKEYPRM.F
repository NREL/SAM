C=======================================================================
      SUBROUTINE FFKEYPRM (FPARAM, INPOS, NUMPRM, CPARAM, IXVAL,
     &   NPARAM, IXVALN)
C=======================================================================

C   --*** FFKEYPRM *** (CAMCON_LIB) Find parameter in keyword line
C   --   Revised by Amy Gilkey - revised 01/06/96
C   --
C   --FFKEYPRM finds a parameter in a line read by FFRDKEYS.  The input
C   --parameter must match the search parameter by more letters than any other
C   --input parameter, and all letters input must be correct.  If
C   --the parameter is a positional parameter (i.e., it is not named), it
C   --may be selected with INPOS.  If the parameter is abbreviated or is a
C   --positional parameter, the search parameter name is substituted.
C   --
C   --Parameters:
C   --   FPARAM - IN char*(*) - the parameter to be found
C   --   INPOS - IN integer - the position of the parameter; only needed if
C   --      the input parameters are blank
C   --   NUMPRM - IN integer - the number of input parameters
C   --   CPARAM(NUMPRM) - IN/OUT char*(*) - the input parameters; set if
C   --      blank and INPOS>0
C   --   IXVAL(NUMPRM) - IN integer - the starting index of the input values
C   --      for each parameter; if IXVAL(1) < 0, IXVAL is not used
C   --   NPARAM - OUT integer - the returned parameter number; 0 if not found
C   --   IXVALN - OUT integer - the starting index of the returned parameter;
C   --      only set if IXVAL(1) >= 0

      IMPLICIT NONE

C   --Parameter Definitions:
      CHARACTER*(*) FPARAM
      INTEGER INPOS
      INTEGER NUMPRM
      CHARACTER*(*) CPARAM(NUMPRM)
      INTEGER IXVAL(NUMPRM)
      INTEGER NPARAM
      INTEGER IXVALN

      INTEGER NUMINP
      INTEGER IPRM
      INTEGER LFND
      INTEGER LETMAT
      INTEGER LPAR

************************************************************************

      NPARAM = 0
      IF (IXVAL(1) .GE. 0) IXVALN = 0

C   --Find out if any input parameters are non-blank
      NUMINP = 0
      DO 100 IPRM = 1, NUMPRM
         IF (CPARAM(IPRM) .NE. ' ') NUMINP = NUMINP + 1
  100 CONTINUE

      IF ((NUMINP .GT. 0) .AND. (FPARAM .NE. ' ')) THEN

C      --Find the parameter that matches the most letters

C      --LFND is the length of FPARAM
         LFND = INDEX (FPARAM, ' ') - 1
         IF (LFND .LT. 0) LFND = LEN (FPARAM)
         LETMAT = 0
         NPARAM = 0
         DO 110 IPRM = 1, NUMPRM
            IF (CPARAM(IPRM) .NE. ' ') THEN
C            --LPAR is the length of CPARAM
               LPAR = INDEX (CPARAM(IPRM), ' ') - 1
               IF (LPAR .LT. 0) LPAR = LEN (CPARAM(IPRM))
C            --Check that input parameter is long enough, but not longer than
C            --the search parameter
               IF (LPAR .LE. LFND) THEN
C               --Parameters must match all letters in input parameter
                  IF (CPARAM(IPRM)(:LPAR) .EQ. FPARAM(:LPAR)) THEN
                     IF (LETMAT .LT. LPAR) THEN
C                     --Parameter matches by more letters than any previous;
C                     --save it
                        LETMAT = LPAR
                        NPARAM = IPRM
                     END IF
                  END IF
               END IF
            END IF
  110    CONTINUE

      ELSE IF ((INPOS .GT. 0) .AND. (INPOS .LE. NUMPRM)) THEN

C      --Find the positional parameter

         NPARAM = INPOS
      END IF

C   --Fill in the parameter index
      IF (NPARAM .GT. 0) THEN
         IF (IXVAL(1) .GE. 0) IXVALN = IXVAL(NPARAM)
      END IF

      RETURN
      END
