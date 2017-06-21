C=======================================================================
      SUBROUTINE QA0INIT
C=======================================================================

C   --*** QA0INIT *** (CAMCON_LIB) Make sure QA common initialized
C   --   Written by Amy Gilkey - revised 05/03/01
C   --
C   --QA0INIT is an internal routine called by every routine that uses
C   --camlib_program_common.h.  The first time it is called, it initializes
C   --the common elements to all blank.  On subsequent calls, it does nothing.
C   --
C   --Parameters:

C   --Common Variables:
C   --   Sets PROGRM, EXEVERS, EXEDAT,
C   --      EXEFULL, EXENAM, EXEDAT, EXETYP, EXECLASS, EXEVERS,
C   --      RUNDAT, RUNTIM, RUNMAC, RUNSYS, AUTHOR, SPONSR of CAMCON_LIB common

      IMPLICIT NONE

C   --Common Variables:
      INCLUDE 'CAMLIB_PROGRAM_COMMON.H'

      LOGICAL INIT
      SAVE INIT
      DATA INIT /.TRUE./

      IF (INIT) THEN
         PROGRM = 'Stepwise'
         EXEFULL = 'Stepwise_v2.21a.exe'
         EXENAM = 'Stepwise_v2.21a.exe'
         EXEDAT8 = '08/05/10'
         EXEDAT10 = '08/05/2010'
         EXETYP = 'SAM'
         EXECLASS = ' '
         EXEVERS = '2.21a'
         RUNDAT8 = ' '
         RUNDAT10 = ' '
         RUNTIM = ' '
         RUNMAC = ' '
         RUNSYS = ' '
         AUTHOR = 'A. Gilkey'
         SPONSR = 'C. Sallaberry'
         INIT = .FALSE.
      END IF

      RETURN
      END
