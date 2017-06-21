C=======================================================================
      SUBROUTINE EXPARM (HARD, SOFT, MODE, KCSU, KNSU, IDAU)
C=======================================================================
C     FORTRAN Extension Library - UNIX

C   --*** EXPARM *** (CAMSUPES_LIB) Get system operating parameters
C   --   Written by Dennis Morrison and Bill Mill-Curran
C   --   Revised by Amy Gilkey - revised 06/01/95
C   --
C   --EXPARM defines various operating environment parameters.  A
C   --character ID is supplied for both the processor hardware type and
C   --the operating system.  The job processing mode, batch or
C   --interactive, is identified; for this purpose an interactive job
C   --is defined as one where the standard input device is attended by
C   --the user who can respond to unforseen events.  The number of
C   --character storage units and the number of numeric storage units
C   --in the smallest block of storage which contains an integral
C   --number of each are defined here.  This routine further defines
C   --whether the record length of a direct access unformatted file is
C   --counted in character or numeric storage units.
C   --
C   --Parameters:
C   --   HARD - OUT char*(*) - the system hardware ID
C   --   SOFT - OUT char*(*) - the system software ID
C   --   MODE - OUT integer - the job mode (0=batch, 1=interactive, 2=spawned)
C   --   KCSU - OUT integer - the number of character storage units
C   --   KNSU - OUT integer - the number of numeric storage units
C   --   IDAU - OUT integer - the unformatted direct access units:
C   --      0 = character storage units
C   --      1 = numeric storage units

C   --Machine Dependencies: 
C   --   Calls HOSTNM to return host name.

C      USE DFPORT
      IMPLICIT NONE

C   --Parameter Definitions:
      CHARACTER*(*) HARD, SOFT
      INTEGER MODE
      INTEGER KCSU, KNSU, IDAU

C      put line below as a comment - no need to redefine HOSTNM
C      INTEGER HOSTNM

      INTEGER I1

C   --Get the system hardware and software ID
cdkr  changed from HOSTNM
C      I1 = HOSTNAM (HARD)
      HARD = 'LOCAL'
      SOFT = 'UNIX'

C   --Get the batch/interactive mode
cdkr
      MODE = 1

C   --Set the storage unit parameters
      KCSU = 4
      KNSU = 1
      IDAU = 0

      END
