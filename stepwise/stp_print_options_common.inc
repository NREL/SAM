C *** stp_print_options_common.inc ***

      CHARACTER*8 DATTYP
C      --DATTYP - the data to perform the regression analysis on:
C      --   'RAW'   = do regression analysis on the raw data
C      --   'RANK'  = do regression analysis on the ranks of the data
C      --   'STD01' = do regression analysis on the standard 0-1 data
      CHARACTER*8 DATPLT
C      --DATPLT - the data to be plotted, either RAW or the same as DATTYP
      CHARACTER*8 REGTYP
C      --REGTYP - the type of regression analysis to be performed:
C      --   ' '        = no regression analysis
C      --   'STEPWISE' = do stepwise regression
C      --   'BACKWARD' = do backward elimination
      LOGICAL PRPRES
C      --PRPRES - print the PRedicted Error Sum of Squares
      LOGICAL PRMEAN
      LOGICAL PRCORR
C      --PRCORR - print the correlation matrix
      LOGICAL PRSSCP
C      --PRSSCP - print the sum of squares and cross-products matrix
      LOGICAL PRINV
C      --PRINV - print the inverse correlation matrix for each step
      LOGICAL PRSTEP
C      --PRSTEP - print the results from each step of the regression analysis
      LOGICAL PRRESI
C      --PRRESI - print the y, yhat, and residual for each observation of final
C      --   model
      LOGICAL PRMISC
      LOGICAL PLPRES
C      --PLPRES - plot the PRedicted Error Sum of Squares
      LOGICAL PLSCAT
C      --PLSCAT - plot the dependent variable, independent variable scatter
C      --   plots
      LOGICAL PLRESI
C      --PLRESI - plot the residual plots
      LOGICAL DUMPVR
C      --DUMPVR - print the raw and rank/stand01 data
      COMMON /PARDEF/
     &   PRPRES, PRMEAN, PRCORR, PRSSCP, PRINV, PRSTEP, PRRESI, PRMISC,
     &   PLPRES, PLSCAT, PLRESI, DUMPVR
      COMMON /PARDEFc/ DATTYP, DATPLT, REGTYP
C CMS REPLACEMENT HISTORY, Element STP_PRINT_OPTIONS_COMMON.INC
C *2     4-DEC-1995 19:11:36 APGILKE "Add DUMP flag"
C *1     1-NOV-1995 11:24:42 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_PRINT_OPTIONS_COMMON.INC
