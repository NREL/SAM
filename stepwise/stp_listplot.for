C=======================================================================
      SUBROUTINE LISTPLOTs (
     &   HEADER, LABEL, NUMPTS, SNGPTS)
C=======================================================================

C   --*** LISTPLOTs *** (STEPWISE) Add column of data to plot file (single prec)
C   --   Modified by Amy Gilkey - revised 04/04/96
C   --
C   --LISTPLOTs adds a column of data to the X-Y plot file.  The data will
C   --not be written to the plot file until WRITEPLOT is called.
C   --
C   --There is a limit of 1,260 columns that can be written to the plot file.
C   --This limit reflects the record size of the plot file.
C   --
C   --Parameters:
C   --   HEADER - IN - the plot type
C   --   LABEL - IN - the label for the points
C   --   NUMPTS - IN - the number of data points
C   --   SNGPTS - IN - the data points

      IMPLICIT NONE

      CHARACTER*(*) HEADER
      CHARACTER*(*) LABEL
      INTEGER NUMPTS
      REAL SNGPTS(NUMPTS)
      DOUBLE PRECISION DBLPTS(NUMPTS)

      RETURN
      END

C=======================================================================
      SUBROUTINE LISTPLOTd (
     &   HEADER, LABEL, NUMPTS, DBLPTS)
C=======================================================================

C   --*** LISTPLOTd *** (STEPWISE) Add column of data to plot file (double prec)
C   --   Modified by Amy Gilkey - revised 04/04/96
C   --
C   --LISTPLOTd adds a column of data to the X-Y plot file.  The data will
C   --not be written to the plot file until WRITEPLOT is called.
C   --
C   --Parameters:
C   --   HEADER - IN - the plot type
C   --   LABEL - IN - the label for the points
C   --   NUMPTS - IN - the number of data points
C   --   DBLPTS - IN - the data points

      IMPLICIT NONE

      CHARACTER*(*) HEADER
      CHARACTER*(*) LABEL
      INTEGER NUMPTS
      REAL SNGPTS(NUMPTS)
      DOUBLE PRECISION DBLPTS(NUMPTS)

      RETURN
      END

C=======================================================================
      SUBROUTINE WRITEPLOT 
C=======================================================================

C   --*** WRITEPLOT *** (STEPWISE) Write data to plot file
C   --   Modified by Amy Gilkey - revised 04/04/96
C   --
C   --WRITEPLOT writes the columns of data previously defined with routine
C   --LISTPLOTx to the plot file.  WRITEPLOT may be called more than once.
C   --
C   --Parameters:

      RETURN
      END

C=======================================================================
      SUBROUTINE RLISTPLOT (LABEL, NUMPTS,
     &   NUMPLT, NPLTVAL, PLTLAB, IXPLT)
C=======================================================================

C   --*** RLISTPLOT *** (STEPWISE) Add column of data to plot file (double prec)
C   --   Modified by Amy Gilkey - revised 04/04/96
C   --
C   --RLISTPLOT adds a column of data to the X-Y plot file.  This routine is
C   --internal to LISTPLOT.
C   --
C   --Parameters:
C   --   LABEL - IN - the label for the column to be added
C   --   NUMPTS - IN - the number of data points in the column
C   --   NUMPLT - IN/OUT - the number of columns defined
C   --   NPLTVAL - IN/OUT - the total number of data points in all columns
C   --   PLTLAB - IN/OUT - the label for each column
C   --   IXPLT - IN/OUT - the ending index of the data for each column

      IMPLICIT NONE

      CHARACTER*(*) LABEL
      INTEGER NUMPTS
      INTEGER NUMPLT
      INTEGER NPLTVAL
      CHARACTER*12 PLTLAB(*)
      INTEGER IXPLT(0:*)

      NUMPLT = NUMPLT + 1

      PLTLAB(NUMPLT) = LABEL

      IF (NUMPLT .EQ. 1) IXPLT(0) = 0
      NPLTVAL = NPLTVAL + NUMPTS
      IXPLT(NUMPLT) = NPLTVAL

      RETURN
      END

C=======================================================================
      SUBROUTINE RWRITEPLOT (IUNPLT, HEADER, NUMPLT,
     &   PLTLAB, IXPLT, PLTPTS, PLTLIN)
C=======================================================================

C   --*** RWRITEPLOT *** (STEPWISE) Write data to plot file
C   --   Modified by Amy Gilkey - revised 04/04/96
C   --
C   --RWRITEPLOT writes the columns of data previously defined with routine
C   --LISTPLOTx to the plot file.  RWRITEPLOT may be called more than once.
C   --This routine is internal to WRITEPLOT.
C   --
C   --Parameters:
C   --   IUNPLT - IN - the unit number of the plot text file
C   --   HEADER - IN - the plot type
C   --   NUMPLT - IN - the number of columns defined
C   --   PLTLAB - IN - the label for each column
C   --   IXPLT - IN - the ending index of the data for each column
C   --   PLTPTS - IN - the data for each column
C   --   PLTLIN - SCRATCH - size = 12chars * NUMPLT

      IMPLICIT NONE

      INTEGER IUNPLT
      CHARACTER*(*) HEADER
      INTEGER NUMPLT
      CHARACTER*12 PLTLAB(NUMPLT)
      INTEGER IXPLT(0:NUMPLT)
      REAL PLTPTS(*)
      CHARACTER*12 PLTLIN(NUMPLT)

C#????      INTEGER ISTRLEN
C#????
      INTEGER MAXPTS
      INTEGER IPTS
      INTEGER IX
      INTEGER I
      INTEGER IDUM

      REWIND (IUNPLT)

C#????      WRITE (IUNPLT, 10000, IOSTAT=IDUM) HEADER(:ISTRLEN(HEADER))
C#????10000  FORMAT (A)
C#????
      WRITE (IUNPLT, 10010, IOSTAT=IDUM)
     &   (PLTLAB(I), I=1,NUMPLT)
10010  FORMAT (1260 (A, :, ' '))

      MAXPTS = 0
      DO 100 I = 1, NUMPLT
         MAXPTS = MAX (MAXPTS, IXPLT(I) - IXPLT(I-1))
  100 CONTINUE

      DO 120 IPTS = 1, MAXPTS
         DO 110 I = 1, NUMPLT
            IX = IXPLT(I-1) + IPTS
            IF (IX .LE. IXPLT(I)) THEN
               WRITE (PLTLIN(I), 10020, IOSTAT=IDUM) PLTPTS(IX)
10020           FORMAT (1P1E12.5)
            ELSE
               PLTLIN(I) = ' '
            END IF
  110    CONTINUE
         WRITE (IUNPLT, 10010, IOSTAT=IDUM)
     &      (PLTLIN(I), I=1,NUMPLT)
  120 CONTINUE

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_LISTPLOT.FOR
C *1    10-APR-1996 10:01:10 APGILKE "Add plot text file capability"
C CMS REPLACEMENT HISTORY, Element STP_LISTPLOT.FOR
