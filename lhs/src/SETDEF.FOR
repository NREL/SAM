C     Last change:  SLD   5 Dec 96    8:17 am
C****************************************************************
C SUBROUTINE SETDEF SETS THE DEFAULT VALUES OF THE PARAMETERS
C
      SUBROUTINE SETDEF
C
c      INCLUDE 'KILLFILE.INC'  -- include not needed 10-96
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CPARAM.INC'                                              GDW-96  
      USE CPARAM                        
C
C     These statements removed to make modules work - GDW-96
C     COMMON/OBSTR/NSTR,NOBSTR(NVAR)
      USE OBSTR
C
      DO 10 I=1, LENT
         TITLE(I:I)=' '
   10 CONTINUE
      N=0
      NV=0
      IRS=0
      ICM=0
      NREP=1
      IRP=0
      IV1=0
      IDATA=0
      IHIST=0
      ICORR=0
      NSTR=0
      DO I=1,NVAR
         IDIST(I)=0
         NOBSTR(I)=0
c        -- List is the array that contains all of the names encountered
         List(i) = ' '
c        -- IVarNm(i) holds the distribution number that goes with List(i)
c        -- (the order in which the dist was defined, not the dist type)
         IVarNm(i) = 0
c        -- PValue(i) holds the optional point values read from the input
c        -- for each distribution name
         PValue(i) = 0.0
      End Do
C
      RETURN
      END
