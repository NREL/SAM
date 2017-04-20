C     Last change:  SLD  28 Feb 97    2:23 pm
C
C=======================================================================
C
C  This file contains several miscellaneous modules for the LHS code.
C  They are derived from common blocks that were not separated out
C  as their own include files.  The module names are the same as the
C  names of the common blocks that they replace, and are seen in the
C  following USE statements, which should be substituted for the COMMON
C  statements within the program units.
C
C      USE STAR
C      USE UICORR
C      USE CHRCRD
C      USE OBSTR
C      USE PDMAT
C      USE FIRSTS
C
C  In addition, there is a module to declare any local arrays that
C  were previously dimensioned static.
C
C      USE LOCALVARS
C
C  Each module has an initialization subroutine to allocate and initialize
C  arrays.  The name of the initialization subroutine is always the name
C  of the module plus the characters "_INIT", so the initialization subroutine
C  for STAR is STAR_INIT.  There are never any arguments to these routines.
C
C=======================================================================
C
      MODULE STAR
C
C      This module replaces the common block:
C      COMMON/STAR/NSUBOB(NINTMX),SUBINT(NINTMX+1),NINT
C
      INTEGER, ALLOCATABLE :: NSUBOB(:)
      REAL, ALLOCATABLE :: SUBINT(:)
      INTEGER :: NINT = 0
C
C      Here is the initialization subroutine for this module
C
      CONTAINS
C
      SUBROUTINE STAR_INIT()
C
        USE PARMS
C
        ALLOCATE( NSUBOB(NINTMX), SUBINT(NINTMX+1) )
        NSUBOB = 0
        SUBINT = 0.0
C
        RETURN
C
      END SUBROUTINE
C
      SUBROUTINE STAR_CLOSE()
C
        DEALLOCATE( NSUBOB, SUBINT )
C
        RETURN
C
      END SUBROUTINE
C
      END MODULE
C
C=======================================================================
C
      MODULE UICORR
C
C      This module replaces the common block:
C      COMMON/UICORR/ICVAR(NCVAR),JCVAR(NCVAR),CVAR(NCVAR),NCV
C
      INTEGER, ALLOCATABLE :: ICVAR(:), JCVAR(:)
      REAL, ALLOCATABLE :: CVAR(:)
      INTEGER :: NCV = 0
C
C      Here is the initialization subroutine for this module
C
      CONTAINS
C
      SUBROUTINE UICORR_INIT()
C
        USE PARMS
C
        ALLOCATE( ICVAR(NCVAR), JCVAR(NCVAR), CVAR(NCVAR) )
        ICVAR = 0
        JCVAR = 0
        CVAR = 0.0
C
        RETURN
C
      END SUBROUTINE
C
      SUBROUTINE UICORR_CLOSE()
C
        DEALLOCATE( ICVAR, JCVAR, CVAR )
C
        RETURN
C
      END SUBROUTINE
C
      END MODULE
C
C=======================================================================
C
      MODULE CHRCRD
C
C      This module replaces the common block:
C      CHARACTER*(LENC) CRDSTR(NVAR)
C      COMMON/CHRCRD/CRDSTR
C
C      Must use parameters defined in the PARMS module
        USE PARMS
C
        CHARACTER (LEN=LENC), ALLOCATABLE :: CRDSTR(:)
C
C      Here is the initialization subroutine for this module
C
      CONTAINS
C
      SUBROUTINE CHRCRD_INIT()
C
        USE PARMS
C
        ALLOCATE( CRDSTR(NVAR) )
        CRDSTR = ' '
C
        RETURN
C
      END SUBROUTINE
C
      SUBROUTINE CHRCRD_CLOSE()
C
        DEALLOCATE( CRDSTR )
C
        RETURN
C
      END SUBROUTINE
C
      END MODULE
C
C=======================================================================
C
      MODULE OBSTR
C
C      This module replaces the common block:
C      COMMON/OBSTR/NSTR,NOBSTR(NVAR)
C
        INTEGER :: NSTR = 0
        INTEGER, ALLOCATABLE :: NOBSTR(:)
C
C      Here is the initialization subroutine for this module
C
      CONTAINS
C
      SUBROUTINE OBSTR_INIT()
C
        USE PARMS
C
        ALLOCATE( NOBSTR(NVAR) )
        NOBSTR = 0
C
        RETURN
C
      END SUBROUTINE
C
      SUBROUTINE OBSTR_CLOSE()
C
        DEALLOCATE( NOBSTR )
C
        RETURN
C
      END SUBROUTINE
C
      END MODULE
C
C=======================================================================
C
      MODULE PDMAT
C
C      This module replaces the common block:
C      COMMON/PDMAT/Z(NVAR,NVAR),D(NVAR)
C
        REAL, ALLOCATABLE :: Z(:,:),D(:)
C
C      Here is the initialization subroutine for this module
C
      CONTAINS
C
      SUBROUTINE PDMAT_INIT()
C
        USE PARMS
C
        ALLOCATE( Z(NVAR,NVAR), D(NVAR) )
        Z = 0.0
        D = 0.0
C
        RETURN
C
      END SUBROUTINE
C
      SUBROUTINE PDMAT_CLOSE()
C
        DEALLOCATE( Z, D )
C
        RETURN
C
      END SUBROUTINE
C
      END MODULE
C
C=======================================================================
C
      MODULE FIRSTS
C
C     These are all of the variables that have to be reinitialized each
C     time a new execution of the code is started.
C
C       For routine RMCNPI
C         DATA ISARG / 0 /
        INTEGER :: ISARG
C
C       For Routine ERSTGT
C         DATA LNF/-1/,LNT/0/
        INTEGER :: LNF, LNT
C
      CONTAINS 
C
      SUBROUTINE FIRSTS_INIT()
C
        ISARG = 0
        LNF = -1
        LNT = 0
C
        RETURN
C
      END SUBROUTINE
C
      SUBROUTINE FIRSTS_CLOSE()
C
C       Nothing to deallocate
C
        Return
C
      END SUBROUTINE
C
      END MODULE
C
C=======================================================================
C
      MODULE LOCALVARS
c
c       This module contains arrays that were previously dimensioned
c       locally in various subroutines.  They were moved here to make sure
c       that the allocation and deallocation of these arrays during
c       the program does not wipe out data tat the original programmer
c       assumed would be saved from one execution to another.
c
        USE PARMS
c
c       From CORCAL:
        REAL, ALLOCATABLE :: XM(:), SSQ(:)
c
c       From CMCRD:
        REAL, ALLOCATABLE :: RIJ(:)
        INTEGER, ALLOCATABLE :: IJCVAR(:)
c
c       From POSDEF:
        REAL, ALLOCATABLE :: WK(:)
c
c
      CONTAINS
C
      SUBROUTINE LOCALVARS_INIT()
C
        USE PARMS
c
c       From CORCAL:
        ALLOCATE( XM(NVAR), SSQ(NVAR) )
c
c       From CMCRD:
        ALLOCATE( RIJ(NCVAR*2),IJCVAR(2*NCVAR) )
        RIJ = 0.0
        IJCVAR = 0
c
c       From POSDEF:
        ALLOCATE( WK((NVAR*(NVAR+1))/2) )
        WK = 0.0
C
        RETURN
C
      END SUBROUTINE
C
      SUBROUTINE LOCALVARS_CLOSE()
c
c       From CORCAL:
        DEALLOCATE( XM, SSQ )
c
c       From CMCRD:
        DEALLOCATE( RIJ, IJCVAR )
c
c       From POSDEF:
        DEALLOCATE( WK )
C
        Return
C
      END SUBROUTINE
C
c
c
      END MODULE
