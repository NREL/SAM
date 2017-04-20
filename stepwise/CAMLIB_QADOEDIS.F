C=======================================================================
      SUBROUTINE QADOEDIS (IUCOUT, CONTR)
C=======================================================================

C   --*** QADOEDIS *** (CAMCON_LIB) Print standard DOE disclaimer
C   --   Written by Amy Gilkey - revised 08/17/98
C   --
C   --QADOEDIS prints the Sandia DOE contract statement and the DOE disclaimer
C   --to the terminal or an output file.
C   --
C   --Parameters:
C   --   IUCOUT - IN integer - the output file unit number:
C   --      >0 for output file unit number IUCOUT
C   --      =0 for the standard output device (terminal)
C   --      <0 for the defined diagnostics file
C   --   CONTR - IN char*(*) - the DOE contract number; if CONTR is '*',
C   --      contract number DE-AC04-76DP00789 is printed; if CONTR is ' ',
C   --      the contract statement is not printed

      IMPLICIT NONE

C   --Parameter Definitions:
      INTEGER IUCOUT
      CHARACTER*(*) CONTR

C   --Definitions for functions called:
      INTEGER IQAERRUNI
      INTEGER ISTRLEN

      INTEGER IUNOUT
      INTEGER IDUM
      CHARACTER*40 TCONTR

      IUNOUT = IUCOUT
      IF (IUNOUT .LT. 0) THEN
          IUNOUT = IQAERRUNI (IUCOUT)
      ENDIF

      IF (CONTR .NE. ' ') THEN
         IF (CONTR .EQ. '*') THEN
            TCONTR = 'DE-AC04-76DP00789'
         ELSE
            TCONTR = CONTR
         END IF

         IF (IUNOUT .LE. 0) THEN
            WRITE (*, 10000, IOSTAT=IDUM)
            WRITE (*, 10010, IOSTAT=IDUM) TCONTR(:ISTRLEN(TCONTR))
         ELSE
            WRITE (IUNOUT, 10000, IOSTAT=IDUM)
            WRITE (IUNOUT, 10010, IOSTAT=IDUM) TCONTR(:ISTRLEN(TCONTR))
         END IF
      END IF

      IF (IUNOUT .LE. 0) THEN
         WRITE (*, 10000, IOSTAT=IDUM)
         WRITE (*, 10020, IOSTAT=IDUM)
         WRITE (*, 10030, IOSTAT=IDUM)
         WRITE (*, 10000, IOSTAT=IDUM)
      ELSE
         WRITE (IUNOUT, 10000, IOSTAT=IDUM)
         WRITE (IUNOUT, 10020, IOSTAT=IDUM)
         WRITE (IUNOUT, 10030, IOSTAT=IDUM)
         WRITE (IUNOUT, 10000, IOSTAT=IDUM)
      END IF

      RETURN
10000  FORMAT (/, 11X, 60('*'), /)
C$$$OFF  !!! Turn off formatter
10010  FORMAT (
     1 11X'Prepared for'
     2/11X'Sandia National Laboratories'
     3/11X'Albuquerque, New Mexico 87185-5800'
     4/11X'for the United States Department of Energy'
     5/11X'under Contract ', A)
10020  FORMAT (
     1 11X'                         Disclaimer'
     2/11X'                         ----------', /)
10030  FORMAT (
     1 11X'This computer program was prepared as  an  account  of  work'
     2/11X'sponsored  by  an  agency  of  the United States Government.'
     3/11X'Neither the United States Government nor any agency thereof,'
     4/11X'nor  any  of  their employees, nor any of their contractors,'
     5/11X'subcontractors, or  their  employees,  makes  any  warranty,'
     6/11X'express  or  implied,  or  assumes  any  legal  liability or'
     7/11X'responsibility for the accuracy, completeness, or usefulness'
     8/11X'of any information, apparatus, product, or process disclosed'
     9/11X'or represents that its  use  would  not  infringe  privately'
     #/11X'owned  rights.   Reference herein to any specific commercial'
     1/11X'product, process,  or  service  by  trade  name,  trademark,'
     2/11X'manufacturer,  or otherwise, does not necessarily constitute'
     3/11X'or imply its endorsement, recommendation, or favoring by the'
     4/11X'United States Government, any agency thereof or any of their'
     5/11X'contractors  or  subcontractors.   The  views  and  opinions'
     6/11X'expressed  herein  do not necessarily state or reflect those'
     7/11X'of the United States Government, any agency thereof  or  any'
     8/11X'of their contractors or subcontractors.')
C$$$ON  !!! Turn on formatter
      END
