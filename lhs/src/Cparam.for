C     Last change:  S    29 Sep 97    1:33 pm
C
C      COMMON /CHAR1/ TITLE
Cc      CHARACTER TITLE*LENT
C      CHARACTER (len = LENT) TITLE
C      Character*(NamLen) List
C      Character*60 TreeFl, SFile, MFile, CmdLin
C      COMMON /PARAM/ ISEED, N, NV, IRS, ICM, NREP, IDATA, IHIST,
C     1               ICORR, IDIST(NVAR), IRP, IV1, IRSET,
C     2               List(NVar), IVarNm(NVar), PValue(NVar),
C     3               NamOut, IPtVal, TreeFl, SFile, MFile, CmdLin
C
c===============================================================
C
      MODULE CPARAM
C
C       Here are the elements from the old common block
C
        USE PARMS
C
        CHARACTER (LEN = LENT) TITLE
        CHARACTER (LEN = 256) :: TreeFl, SFile, MFile, CmdLin
        INTEGER :: ISEED, N, NV, IRS, ICM, NREP, IDATA, IHIST
        INTEGER :: ICORR
        INTEGER, ALLOCATABLE :: IDIST(:)
        INTEGER :: IRP, IV1, IRSET
        Character*(NamLen), ALLOCATABLE :: List (:)
        INTEGER, ALLOCATABLE :: IVarNm(:)
        REAL, ALLOCATABLE :: PValue(:)
        INTEGER NamOut, I1Col, IPtVal
C
C       Now here is the initialization routine for this module
      CONTAINS
C
      SUBROUTINE CPARAM_INIT()
C
        USE PARMS
C
        ALLOCATE( IDIST(NVAR) )
        IDIST = 0 
C
        ALLOCATE( IVarNm(NVAR) )
        IVarNm = 0
C
        ALLOCATE( PValue(NVar) )
        PValue = 0.0
C
        ALLOCATE( List(NVar) ) 
        List = " "
C
        RETURN
C
      END SUBROUTINE
C
      SUBROUTINE CPARAM_CLOSE()
C
        DEALLOCATE( IDIST )
C
        DEALLOCATE( IVarNm )
C
        DEALLOCATE( PValue )
C
        DEALLOCATE( List )
C
        RETURN
C
      END SUBROUTINE
C
      END MODULE
