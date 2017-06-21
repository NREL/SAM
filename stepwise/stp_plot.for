C=======================================================================
      SUBROUTINE PLOT (DATPLT, XTYP, YTYP, XLAB, YLAB,
     &   NPTS, XDAT, YDAT, *)
C=======================================================================

C   --*** PLOT *** (STEPWISE) Plot the data
C   --   Modified by Amy Gilkey - revised 03/25/96
C   --   Written by Mickey J. Shortencarrier - 1985-6?
C   --
C   --PLOT plots the passed data as scatter plots in the standard format.
C   --
C   --PLOT uses the PLT plot package.
C   --
C   --Parameters:
C   --   DATPLT - IN - the data type: RAW, RANK or STD01
C   --   XTYP, YTYP - IN - the data type:
C   --      'COUNT' = use count for XDAT
C   --      'PRESS', 'RESIDUAL', 'VARIABLE'
C   --   XLAB, YLAB - IN - the label for the X and Y axis
C   --   NPTS - IN - the number of data points
C   --   XDAT, YDAT - IN/OUT - the X and Y data (may be changed if <=0)
C   --   * - return statement if QUIT requested

      IMPLICIT NONE

      INTEGER KLFT, KRGT, KBOT, KTOP
      PARAMETER (KLFT=1, KRGT=2, KBOT=3, KTOP=4)

      INCLUDE 'stp_title_common.inc'
      INCLUDE 'stp_plot_options_common.inc'

      CHARACTER*8 DATPLT
      CHARACTER*(*) XTYP, YTYP
      CHARACTER*(*) XLAB, YLAB
      INTEGER NPTS
      REAL XDAT(*), YDAT(*)

      LOGICAL GRCANCEL
      LOGICAL LDUM, PLTSYMBOL, PLTGETSYM, PLTSETSYM
      INTEGER ISTRLEN

      INTEGER IXLINRT, IYLINRT
      INTEGER IPT
      INTEGER I
      INTEGER IDUM
      REAL XTIC, YTIC
      REAL XMIN, XMAX
      REAL YMIN, YMAX
      REAL CHLSIZ
      REAL DLEGND(KTOP)
      REAL SZSYM
      REAL XT, YT
      REAL XL, YL
      REAL XE, YE
      CHARACTER*8 SYM
      CHARACTER CDUM

      REAL DBORD(KTOP)
      REAL DVIEW(KTOP), WVIEW(KTOP)
      SAVE DBORD, DVIEW

      LOGICAL FIRST
      DATA FIRST / .TRUE. /

      RETURN

C=======================================================================
      ENTRY PLOTEX
C=======================================================================

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_PLOT.FOR
C *4    15-NOV-1996 12:12:08 APGILKE "Correct problem with limited log axis"
C *3    30-APR-1996 12:28:52 APGILKE "Removed bug fix for log axis"
C *2    10-APR-1996 10:00:18 APGILKE "Patch bug with log axis"
C *1     1-NOV-1995 11:24:05 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_PLOT.FOR
