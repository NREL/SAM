C     Last change:  S    17 Feb 98    1:03 pm
C****************************************************************
C SUBROUTINE RDPAR PROCESSES THE PARAMETER STATEMENTS, DEFINES
C VARIABLES IN COMMON /PARAM/ AND STORES DISTRIBUTION INFORMATION
C ON UNITS 7, 8 AND 9
C
      SUBROUTINE RDPAR
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CPARAM.INC'                                              GDW-96  
      USE CPARAM                        
C     INCLUDE 'DISTNM.INC'                                              GDW-96  
      USE DISTNM                        
C     INCLUDE 'CWORKX.INC'                                              GDW-96  
      USE CWORKX, XVLZ => XX
C
C     These statements removed to make modules work - GDW-96
C     COMMON/STAR/NSUBOB(NINTMX),SUBINT(NINTMX+1),NINT
      USE STAR
C     COMMON/UICORR/ICVAR(NCVAR),JCVAR(NCVAR),CVAR(NCVAR),NCV
      USE UICORR
C     DIMENSION XVLZ(MAXTB), PRBZ(MAXTB)
C     EQUIVALENCE (XX(1),XVLZ(1))
C     EQUIVALENCE (XX(MAXTB+1), PRBZ(1))
C
      Character*60 MesFil, SamFil
      DIMENSION A(MAXPAR)
      PARAMETER (LENTC=11)
c rgk-4/90 Lahey Fortran 95 was having trouble with the character assignment for dummy
c variables CDUM1 and CDUM2, so changed it
c      CHARACTER CARD*(LENC),TMPCRD*(LENTC),CDUM1,CDUM2
      CHARACTER CARD*(LENC),TMPCRD*(LENTC)
      CHARACTER CDUM1,CDUM2
      CHARACTER PTITLE*6,PRSAMP*14,PRSEED*12,PNOBS*5,LOBS*12,PNREP*6,
     1          PCMINP*18,LCMINP*29,POUT*7,BLANK,
     3          PERIOD,MINUS,PIRP*15,PVERS1*15
      PARAMETER (PTITLE='TITLE ',PRSAMP='RANDOM SAMPLE ',
     1           PRSEED='RANDOM SEED ',PNOBS='NOBS ',
     2           PNREP='NREPS ',PCMINP='CORRELATION MATRIX',
     3           LCMINP='PAIRS OF CORRELATED VARIABLES',
     4           BLANK=' ',PERIOD='.',MINUS='-',
     5           LOBS='OBSERVATIONS',DUM1=9999.0,DUM2=9999.0,
     6           POUT='OUTPUT ',PIRP='RANDOM PAIRING ',
     7           PVERS1='VERSION 1 INPUT')
      CHARACTER PUSDST*18
      PARAMETER (PUSDST='USER DISTRIBUTION ')
      CHARACTER*19 IRSAMP
      DATA IRSAMP/'***RANDOM SAMPLE***'/
C
      IZERO=ICHAR('0')
      ININE=ICHAR('9')
      IRSET=0
C
C     -- SET DEFAULT VALUES FOR PARAMETERS
C
      CALL SETDEF
      If(KLLERR) Return
      IFIRST=0
      MesFil = 'LHS-P.OUT'
      SamFil = 'LHS-S.OUT'
C
C     -- READ A NEW PARAMETER CARD
C
   10 READ(5,9001,END=8000)CARD
C
C     -- PROCESS THE VERSION 1 INPUT CARD
      IF (CARD(1:15) .EQ. PVERS1) THEN
         IF (IFIRST .GT. 0) THEN
            WRITE (6,9012)
            WRITE (99,9012)
            KLLERR = .TRUE.
            RETURN
         ELSE
            IV1=1
            GOTO 10
         END IF
      ELSE
         IFIRST=1
      END IF
C
C     -- TITLE CARD
      IF(CARD(1:6).EQ.PTITLE)THEN
         READ(CARD(7:LENC),9002)(TITLE(I:I),I=1,(LENC-6))
         GO TO 10
C
C     -- RANDOM SAMPLE CARD
      ELSE IF(CARD(1:14).EQ.PRSAMP)THEN
         READ(IRSAMP,9002)(TITLE(I:I),I=98,116)
         IRS=1
         GO TO 10
C
C     -- RANDOM SEED CARD
      ELSE IF(CARD(1:12).EQ.PRSEED)THEN
         CALL DATSQZ(CARD(13:80),PRSEED,TMPCRD)
         If(KLLERR) Return
         READ(TMPCRD,9003)ISEED
         If (ISeed <= 0 ) Then
            Write (*,9022) ISeed
            Write (99,9022) ISeed
            Write (6,9022) ISeed
            KLLERR = .TRUE.
            RETURN
         End If
C
C     -- SET RANDOM NUMBER GENERATOR HERE IF NECESSARY
         IRSET=1
         GO TO 10
C
C     -- RANDOM PAIRING CARD
      ELSE IF(CARD(1:15).EQ.PIRP)THEN
         IRP=1
         GO TO 10
C
C     -- New Output Format Card
      Else If (Card(1:12) == 'NAMED OUTPUT') Then
         NamOut = 1
         Go To 10
C
C     -- Selecting the calculation method for the point value
      Else If (Card(1:17) == 'POINT VALUE TYPE ') Then
         Read(Card(18:),*) IPtVal
C        -- a value of zero is not allowed because there is no way to get
C        -- the optional point values in under this input format.
         If ( IPtVal == 0 ) Then
            Print *, 'Error: POINT VALUE TYPE <= 0 is illegal.'
            Print *, 'Use a value greater than zero.'
            Write (99,*) 'Error: POINT VALUE TYPE <= 0 is illegal.'
            Write (99,*) 'Use a value greater than zero.'
            Write (6,*) 'Error: POINT VALUE TYPE <= 0 is illegal.'
            Write (6,*) 'Use a value greater than zero.'
            KLLERR = .TRUE.
            RETURN
         End If
         Go To 10
C
C     -- Set the sample output file name
      Else If (Card(1:12) == 'SAMPLE FILE ') Then
         SamFil = ' '
         Read(Card(13:),'(A128)') SamFil
         Go To 10
C
C     -- Set the program message output file name
      Else If (Card(1:13) == 'MESSAGE FILE ') Then
         MesFil = ' '
         Read(Card(14:),'(A128)') MesFil
         Go To 10
C
C     -- NUMBER OF OBSERVATIONS CARD
      ELSE IF(CARD(1:5).EQ.PNOBS)THEN
         CALL DATSQZ(CARD(6:80),PNOBS,TMPCRD)
         If(KLLERR) Return
         READ(TMPCRD,9003)N
         IF(N.LT.1)THEN
            WRITE(6,9006)N
            WRITE(99,9006)N
            KLLERR = .TRUE.
            RETURN
         ENDIF
         CALL CHKDIM(1,N,NMAX,PNOBS,LOBS)
         If(KLLERR) Return
         GO TO 10
C
C     -- NUMBER OF REPETITIONS CARD
      ELSE IF(CARD(1:6).EQ.PNREP)THEN
         CALL DATSQZ(CARD(7:80),PNREP,TMPCRD)
         If(KLLERR) Return
         READ(TMPCRD,9003)NREP
         IF(NREP.LT.1)THEN
            WRITE(6,9007)NREP
            WRITE(99,9007)NREP
            KLLERR = .TRUE.
            RETURN
         ENDIF
         GO TO 10
C
C     -- CORRELATION MATRIX CARD
      ELSE IF(CARD(1:18).EQ.PCMINP)THEN
         READ (5,*,ERR=9000) NCV, (ICVAR(I),JCVAR(I),CVAR(I),I=1,NCV)
         CALL CHKDIM(1,NCV,NCVAR,PCMINP,LCMINP)
         If(KLLERR) Return
         ICM=1
         GO TO 10
C
C     -- OUTPUT OPTIONS CARD
      ELSE IF (CARD(1:7).EQ.POUT)THEN
         CALL OUTCRD(CARD)
         If(KLLERR) Return
         GO TO 10
C
      END IF
C
C     --- READ AND CHECK ALL DIATRIBUTIONS WITH A KNOWN NUMBER OF PARAMETERS
C
      DO 100 ID=1,LEND
C
         IF (IDSPAR(ID) .LT. 1) GO TO 100
C
         IDL=IDSEND(ID)-IDSST(ID)+1
         IDL1=IDL+1
         IF ( CARD(1:IDL) .EQ. DIST(IDSST(ID):IDSEND(ID))) THEN
            READ (5,*,ERR=9000) (A(I),I=1,IDSPAR(ID))
            CALL CHKDAT ( DIST(IDSST(ID):IDSEND(ID)), A, MAXPAR)
            If(KLLERR) Return
            CALL WRTCRD ( ID, CARD(IDL1:LENC))
            If(KLLERR) Return
            GO TO 10
         END IF
C
  100 CONTINUE
C
C     --- READ AND CHECK THE * DISTRIBUTIONS
C
      DO 200 ID=1, LEND
C
         IF (IDSPAR(ID) .NE. -1) GO TO 200
C
         IDL=IDSEND(ID)-IDSST(ID)+1
         IDL1=IDL+1
         IF (CARD(1:IDL) .EQ. DIST(IDSST(ID):IDSEND(ID))) THEN
            READ (5,*,ERR=9000) NINT
            BACKSPACE 5
            READ (5,*,ERR=9000) NINT,(NSUBOB(I),I=1,NINT),
     1                               (SUBINT(I),I=1,NINT+1)
            CALL CHKSTR ( DIST(IDSST(ID):IDSEND(ID)), CARD)
            If(KLLERR) Return
            CALL WRTCRD ( ID, CARD(IDL1:LENC))
            If(KLLERR) Return
            GO TO 10
         END IF
C
  200 CONTINUE
C
C     --- READ AND CHECK EMPIRICAL CUMULATIVE AND DENSITY FUNCTIONS
C
      DO 300 ID=1, LEND
C
         IF (IDSPAR(ID) .NE. -2) GO TO 300
C
         IDL=IDSEND(ID)-IDSST(ID)+1
         IDL1=IDL+1
         IF (CARD(1:IDL) .EQ. DIST(IDSST(ID):IDSEND(ID))) THEN
            READ (5,*,ERR=9000) NP
            IF (NP .GT. MAXTB) THEN
               WRITE (6, 9010) MAXTB, NP, CARD
               WRITE (99, 9010) MAXTB, NP, CARD
               KLLERR = .TRUE.
               RETURN
            ELSE IF (NP .LT. 2) THEN
               WRITE (6,9011) NP, CARD
               WRITE (99,9011) NP, CARD
               KLLERR = .TRUE.
               RETURN
            END IF
            BACKSPACE 5
            READ (5,*,ERR=9000) NP,(XVLZ(I), PRBZ(I), I=1, NP)
            CALL CHKEMP ( DIST(IDSST(ID):IDSEND(ID)), PRBZ, XVLZ,
     1                    NP, MAXTB)
            If(KLLERR) Return
            CALL WRTCRD ( ID, CARD(IDL1:LENC))
            If(KLLERR) Return
            GO TO 10
         END IF
C
  300 CONTINUE
C
C     -- IF THE PROGRAM GETS HERE, THEN AN
C     -- UNDEFINED PARAMETER/DATA CARD WAS FOUND
C
      WRITE(6,9004)CARD
      WRITE(99,9004)CARD
      KLLERR = .TRUE.
      RETURN
C
C     -- DONE READING INPUT.  NOW CHECK FOR INPUT ERRORS.
C
 8000 Continue
c
c     -- Open the output files
      OPEN(UNIT=1, FILE=SamFil)
      OPEN(UNIT=6, FILE=MesFil)
c      OPEN(UNIT=6, FILE=MesFil, FORM='FORMATTED',
c     1     CARRIAGE CONTROL='FORTRAN')
c
      CALL CHKZRO(N,NV,IRSET)
      If(KLLERR) Return
      CALL CHKDIM(2,NV,NVAR,CDUM1,CDUM2)
      If(KLLERR) Return
      IF(ICM.EQ.1) Then
         CALL CMCRD
         If(KLLERR) Return
      Endif
c
c     -- Assign dummy names to all distributions
      Do i=1, NV
         Write (List(i),9090) i
         Do j=1, 10
            If ( List(i)(j:j) == ' ' ) List(i)(j:j) = '-'
         End Do
         IVarNm(i) = i
      End Do
c
      RETURN
C
C     -- PROCESS ALL ERRORS THAT OCCUR READING PARAMETERS FROM THE INPUT FILE
C
 9000 WRITE(6,9005)CARD
      WRITE(99,9005)CARD
      KLLERR = .TRUE.
      RETURN
C
 9001 FORMAT(A)
 9002 FORMAT(80A1)
 9003 FORMAT(I11)
 9004 FORMAT('1',5X,'THE FOLLOWING CARD (POSSIBLY BLANK) IS NOT A ',
     1       'VALID PARAMETER/DATA CARD',/,6X,'PLEASE CONSULT THE ',
     2       'USER MANUAL FOR THE CORRECT PARAMETER/DATA CARD SYNTAX',
     3        //,3X,'***',A,'***')
 9005 FORMAT('1',5X,'THE FOLLOWING PARAMETER CARD DID NOT HAVE THE ',
     1       'CORRECT DATA CARD ASSOCIATED WITH IT',/,6X,'PLEASE ',
     2       'CONSULT THE USER MANUAL FOR THE CORRECT DATA ',
     3       'CARD SYNTAX',/,3X,'***',A,'***')
 9006 FORMAT('1',5X,'THE NUMBER OF OBSERVATIONS REQUESTED IS LESS ',
     1       'THAN ONE',I5)
 9007 FORMAT('1',5X,'THE NUMBER OF REPETITIONS REQUESTED IS LESS ',
     1       'THAN ONE',I5)
 9010 FORMAT('1',5X,'A MAXIMUM OF ',I5,' PAIRS OF VALUES ARE ALLOWED ',
     1       'FOR A CUMULATIVE CONTINUOUS DISTRIBUTION FUNCTION.',/,5X,
     2       I5,' VALUES WERE SPECIFIED FOR THE DISTRIBUTION WITH ',
     3       'THE FOLOWING CARD SYNTAX:',/,3X,'***',A,'***')
 9011 FORMAT('1',5X,'A MINIMUM OF 2 PAIRS OF VALUES MUST BE INPUT ',
     1       'FOR A CUMULATIVE CONTINUOUS DISTRIBUTION FUNCTION.',/,5X,
     2       I5,' VALUES WERE SPECIFIED FOR THE DISTRIBUTION WITH ',
     3       'THE FOLOWING CARD SYNTAX:',/,3X,'***',A,'***')
 9012 FORMAT('1',5X,'THE VERSION 1 INPUT RECORD MUST BE THE FIRST ',
     1       'RECORD IN THE INPUT FILE IF IT IS TO BE USED')
 9022 Format('1',5X,'The random number generator seed value must ',
     1       'be positive.',/,5X,'The following value was found: ',I12)
c
 9090 Format('DIST-',I5)
C
      END
