C=======================================================================
      SUBROUTINE SCATTER (SCATTR,
     &   NUMOBS, NIV, NDV, IXMODL, LABEL, XYRNK)
C=======================================================================

C   --*** SCATTER *** (STEPWISE) Generate scatter plots for all variables
C   --   Modified by Amy Gilkey - revised 04/28/96
C   --
C   --SCATTER generates the scatter plots for each selected dependent variable
C   --with all selected independent variables.  It may optionally generate
C   --scatter plots for selected dependent variable pairs and selected
C   --independent variable pairs.
C   --
C   --Parameters:
C   --   CM - IN/OUT - the character dynamic memory array
C   --   SCATTR - IN - non-blank iff scatter plots only are requested;
C   --      'PARTIAL' for independent-dependent variable pairs only,
C   --      'ALL' for all variable pairs
C   --   NUMOBS - IN - the number of observations
C   --   NIV - IN - the number of selected independent variables
C   --   NDV - IN - the number of selected dependent variables
C   --   IXMODL - IN - the indices of the selected independent variables
C   --   LABEL - IN - the variable labels
C   --   XYRNK - IN - the variable data specified by DATPLT (rank if 'RANK',
C   --      standard 0-1 if 'STD01')

      IMPLICIT NONE

      INCLUDE 'stp_print_options_common.inc'
      INCLUDE 'stp_plot_options_common.inc'

      CHARACTER*(*) SCATTR
      INTEGER NUMOBS
      INTEGER NIV, NDV
      INTEGER IXMODL(*)
      CHARACTER*8 LABEL(*)
      REAL XYRNK(NUMOBS,*)

      INTEGER IVAR1, IVAR2
      INTEGER IXVAR1, IXVAR2

      IF (PLTTXT) THEN
         DO 100 IVAR1 = 1, NDV
            IXVAR1 = IXMODL(NIV+IVAR1)
            CALL LISTPLOTs (
     &         DATPLT, LABEL(IXVAR1), NUMOBS, XYRNK(1,IXVAR1))
  100    CONTINUE
         DO 110 IVAR1 = 1, NIV
            IXVAR1 = IXMODL(IVAR1)
            CALL LISTPLOTs (
     &         DATPLT, LABEL(IXVAR1), NUMOBS, XYRNK(1,IXVAR1))
  110    CONTINUE

         CALL WRITEPLOT 
      END IF

      IF (PLTDEV) THEN
         CALL QAMESSAG (0, ' ', ' ')

C      --Plot dependent variable vs each independent variable
         DO 140 IVAR1 = 1, NDV
            IXVAR1 = IXMODL(NIV+IVAR1)
            CALL QAMESSAG (0, ' ', 'Plotting scatter plots for '
     &         // 'dependent variable ' // LABEL(IXVAR1))
            DO 120 IVAR2 = 1, NIV
               IXVAR2 = IXMODL(IVAR2)
               CALL PLOT (DATPLT, 'VARIABLE', 'VARIABLE',
     &            LABEL(IXVAR2), LABEL(IXVAR1),
     &            NUMOBS, XYRNK(1,IXVAR2), XYRNK(1,IXVAR1), *170)
  120       CONTINUE
            IF (SCATTR .EQ. 'ALL') THEN
C            --Plot dependent variable vs each dependent variable
               DO 130 IVAR2 = IVAR1+1, NDV
                  IXVAR2 = IXMODL(NIV+IVAR2)
                  CALL PLOT (DATPLT, 'VARIABLE', 'VARIABLE',
     &               LABEL(IXVAR2), LABEL(IXVAR1),
     &               NUMOBS, XYRNK(1,IXVAR2), XYRNK(1,IXVAR1), *170)
  130          CONTINUE
            END IF
  140    CONTINUE

         IF (SCATTR .EQ. 'ALL') THEN
            CALL QAMESSAG (0, ' ', 'Plotting scatter plots for '
     &         // 'independent variable pairs')
C         --Plot independent variable vs each independent variable
            DO 160 IVAR1 = 1, NIV
               IXVAR1 = IXMODL(IVAR1)
               DO 150 IVAR2 = IVAR1+1, NIV
                  IXVAR2 = IXMODL(IVAR2)
                  CALL PLOT (DATPLT, 'VARIABLE', 'VARIABLE',
     &               LABEL(IXVAR2), LABEL(IXVAR1),
     &               NUMOBS, XYRNK(1,IXVAR2), XYRNK(1,IXVAR1), *170)
  150          CONTINUE
  160       CONTINUE
         END IF
  170    CONTINUE
      END IF

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_SCATTER.FOR
C *3    30-APR-1996 12:29:03 APGILKE "Added SCATTER ALL"
C *2    10-APR-1996 10:00:36 APGILKE "Add plot text file capability"
C *1     1-NOV-1995 11:24:11 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_SCATTER.FOR
