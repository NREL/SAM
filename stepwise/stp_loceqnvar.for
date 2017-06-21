C=======================================================================
      SUBROUTINE LOCEQNVAR (NUMVAR, LABEL,
     &   NUMENT, NAMENT, TYPENT, INXENT, VALENT, NERR)
C=======================================================================

C   --*** LOCEQNVAR *** (STEPWISE) Locate equation variables and add to list
C   --   Written by Amy Gilkey - revised 01/17/93
C   --
C   --LOCEQNVAR determines the type and location of an equation's variables.
C   --
C   --Parameters:
C   --   NUMVAR - IN - the number of defined variables
C   --   LABEL - IN - the defined variable names
C   --   NUMENT - IN - the number of entries
C   --   NAMENT - IN/OUT - the equation entries (as in /TRNDEF/)
C   --   TYPENT - IN/OUT - the type of each equation entry (as in /TRNDEF/)
C   --   INXENT - IN/OUT - based on TYPENT (as in /TRNDEF/)
C   --   VALENT - IN/OUT - based on TYPENT (as in /TRNDEF/)
C   --   NERR - IN/OUT - the number of errors in the equation, may be set

      IMPLICIT NONE

      INTEGER NUMVAR
      CHARACTER*(*) LABEL(*)
      INTEGER NUMENT
      CHARACTER*(*) NAMENT(*)
      CHARACTER*1 TYPENT(*)
      INTEGER INXENT(*)
      REAL VALENT(*)
      INTEGER NERR

      INTEGER ISTRFIND
      INTEGER ISTRLEN

      INTEGER NENT
      INTEGER INXF
      CHARACTER*8 ENT

      DO 100 NENT = 2, NUMENT

         IF (TYPENT(NENT) .EQ. 'V') THEN

C         --Locate the variable name

            INXF = ISTRFIND (NAMENT(NENT), NUMVAR, LABEL)

            IF (INXF .GT. 0) THEN

C            --Variable - store index

               INXENT(NENT) = INXF

            ELSE
               NERR = NERR + 1
               ENT = NAMENT(NENT)
               CALL QAMESSAG (-1, 'CMD',
     &            'Variable "' // ENT(:ISTRLEN(ENT)) //
     &            '" is undefined')
            END IF
         END IF
  100 CONTINUE

C   --Save the transformation variable name

      IF (TYPENT(1) .EQ. 'V') THEN
         IF (ISTRFIND (NAMENT(1), NUMVAR, LABEL) .GT. 0) THEN
            NERR = NERR + 1
            CALL QAMESSAG (-1, 'CMD',
     &         'The transformation variable is already defined')
         END IF

         IF (NERR .EQ. 0) THEN
            NUMVAR = NUMVAR + 1
            LABEL(NUMVAR) = NAMENT(1)
            INXENT(1) = NUMVAR
         END IF
      END IF

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_LOCEQNVAR.FOR
C *1     1-NOV-1995 11:24:05 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_LOCEQNVAR.FOR
