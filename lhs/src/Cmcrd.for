C     Last change:  S    17 Feb 98    1:13 pm
C****************************************************************
C SUBROUTINE CMCRD PROCESSES THE CORRELATION MATRIX PARAMETER CARD
C
      SUBROUTINE CMCRD
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CPARAM.INC'                                              GDW-96  
      USE CPARAM                        
C     INCLUDE 'CCMATR.INC'                                              GDW-96  
      USE CCMATR                        
C
C     These statements removed to make modules work - GDW-96
C     COMMON/UICORR/ICVAR(NCVAR),JCVAR(NCVAR),CVAR(NCVAR),NCV
      USE UICORR
c
      USE LOCALVARS, ONLY: RIJ, IJCVAR
c
C     DIMENSION RIJ(NCVAR*2),IJCVAR(2*NCVAR)
c     Moved to the LOCALVARS module
C      REAL, ALLOCATABLE :: RIJ(:)
C      INTEGER, ALLOCATABLE :: IJCVAR(:)
C
C     Statement Function
      LOC1(I,J)=J+(I*I-I)/2
C
C     First executable statement
C     Allocate and initialize local arrays
c     Moved to the LOCALVARS module
C      IF ( .NOT. ALLOCATED( RIJ) ) THEN
C      ALLOCATE( RIJ(NCVAR*2),IJCVAR(2*NCVAR) )
C         RIJ = 0.0
C         IJCVAR = 0
C      END IF
C
      NCV2=NCV*2
      Do ICV=1,NCV
         I=ICVAR(ICV)
         J=JCVAR(ICV)
         If( I == J  .AND.  CVAR(ICV) /= 1.0 ) Then
            WRITE(6,9002)I,J,CVAR(ICV)
            WRITE(99,9002)I,J,CVAR(ICV)
            KLLERR = .TRUE.
            RETURN
         Else If( ABS(CVAR(ICV)) >= 1.0 ) Then
            WRITE(6,9001)I,J,CVAR(ICV)
            WRITE(99,9001)I,J,CVAR(ICV)
            KLLERR = .TRUE.
            RETURN
         Else If( I > NV  .OR.  J > NV ) Then
            WRITE(6,9003)I,J,CVAR(ICV),NV
            WRITE(99,9003)I,J,CVAR(ICV),NV
            KLLERR = .TRUE.
            RETURN
         Else
            IJCVAR(ICV)=I
            IJCVAR(NCV+ICV)=J
         End If
      End Do
c
      DO 110 I=1, NCV2
  110 RIJ(I) = IJCVAR(I)
c
      Call Sift(RIJ,NCV2)
      If(KLLERR) Then
         Return
      END If
c
      DO 120 I=1, NCV2
  120 IJCVAR(I) = RIJ(I)
c
      NCM = 1
      LCM(NCM) = IJCVAR(1)
      Do I=2,NCV2
        IF ( IJCVAR(I) /= LCM(NCM) ) Then
          NCM = NCM+1
          LCM(NCM) = IJCVAR(I)
        End If
      End Do
c
      NSIZE = (NCM*(NCM+1))/2
      DO 300 I=1, NSIZE
  300 CORR(I)=0.0
c
      DO 400 I=1, NCM
  400 CORR(LOC1(I,I)) = 1.0
c
      DO ICV=1, NCV
         I = ICVAR(ICV)
         J = JCVAR(ICV)
         DO KCM=1,NCM
            IF ( I == LCM(KCM) ) IM = KCM
            IF ( J == LCM(KCM) ) JM = KCM
         End Do
c
         If ( IM > JM) Then
            CORR(LOC1(IM,JM)) = CVAR(ICV)
         Else
            CORR(LOC1(JM,IM)) = CVAR(ICV)
         End If
      End Do
c
CCCCCC      DEALLOCATE( RIJ, IJCVAR )
C
      Return
c
 9001 Format('1',3X,'The correlation between variable ',I3,' and ',
     1       'variable ',I3,' is greater than one in absolute ',
     2       'value: ',F5.2)
 9002 Format('1',3X,'The correlation between variable ',I3,' and ',
     1       'variable ',I3,' is not equal to one: ',F5.2)
 9003 Format('1',3X,'The correlation between variable ',I3,' and ',
     2       'variable ',I3,' is ',F5.2,/,4x,'However, only ',I3,
     3       ' variables have been defined.')
c
      End
