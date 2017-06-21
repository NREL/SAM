C****************************************************************
C SUBROUTINE TO SET THE CONSTANTS REQUIRED BY FUNCTION IGAUSF
C
C     THIS ROUTINE NAMED GILCONS IN RON'S PROGRAM
C
      SUBROUTINE IGAUS1(FL,CHI,PSI)
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
      DOUBLE PRECISION FLM1,PV,SV,QV,R,V,CON1,CON2,CON3,HC1,HC2,HC3,
     1       FLGS1,FLGS2,FLGS3,SIMULT1
      COMMON/IGAUSC/FLM1,PV,SV,QV,R,V,CON1,CON2,CON3,HC1,HC2,HC3,
     1              FLGS1,FLGS2,FLGS3,SIMULT1
C
C     FIND APPROXIMATE S, P & W FOR GENERALIZED INVERSE GAUSSIAN
C     GENERATION GY ALGORITHM IGAUSF
C
C     -- STATEMENT FUNCTIONS
      H1(X) = (X**FLM1)*EXP(-0.5*((CHI/X) + (PSI+2.*S)*X))
      H2(X) = (X**FLM1)*EXP(-0.5*((CHI/X) + (PSI-2.*P)*X))
C
      FLM1 = FL-1.
      T = (FLM1 + SQRT((FLM1)**2 + CHI*PSI))/PSI
      W = T
      EFFMAX = 0.
C
C     BELOW MODE
C
      FMIN1 = 1.E35
      PART = 0.9
    2 XL = T*PART
      S = FLM1/XL + CHI/(2.*XL*XL) - PSI/2.
      S1 = H1(XL)
      DEL1 = (EXP(S*T) - 1.)/S
      FAC1 = S1*DEL1
      IF (FAC1 .LT. FMIN1) THEN
         FMIN1 = FAC1
         SSTAR = S
         DELST1 = DEL1
         BIGS1 = S1
         PART = PART - 0.1
         GO TO 2
      END IF
C
C     ABOVE THE MODE
C
    6 FMIN2 = 1.E35
      W = 2.*W
      PART = 1.0
      P = PSI*0.5
      XH = -CHI/(2.*FLM1)
      XH = MIN(XH, W)
    4 S2 = H2(XH)
      DEL2 = (EXP(-P*T) - EXP(-P*W))/P
      FAC2 = S2*DEL2
      IF (FAC2 .LT. FMIN2) THEN
         FMIN2 = FAC2
         PSTAR = P
         BIGS2 = S2
         PART = PART - 0.1
         IF (PART .GE. 0.1) THEN
            P = PART*PSI*0.5
            AA = PSI - 2.*P
            XH = (FLM1 + SQRT(FLM1*FLM1+CHI*AA))/AA
            XH = MIN(XH, W)
            GO TO 4
         END IF
      END IF
C
C     TAIL
C
      P = PSI*0.5
      XH = -CHI/(2.*FLM1)
      XH = MAX(XH, W)
      S3 = H2(XH)
      DEL3 = (EXP(-P*W))/P
      FAC3 = S3*DEL3
      EFFIC = 1./(FMIN1 + FMIN2 + FAC3)
      IF (EFFIC .GT. EFFMAX) THEN
         EFFMAX = EFFIC
         WSTAR = W
         DELST3 = DEL3
         BIGS3 = S3
         GO TO 6
      END IF
C
C     CALCULATE CONSTANTS
C
      SV = 1./SSTAR
      PV = 1./PSTAR
      QV = 1./P
      FKV = 1./(EFFMAX*BIGS1*BIGS2*BIGS3)
      FK1 = 1./(FKV*BIGS2*BIGS3)
      FK2 = 1./(FKV*BIGS1*BIGS3)
      FK3 = 1./(FKV*BIGS1*BIGS2)
      R = FK1*DELST1
      V = 1. - FK3*DELST3
      CON1 = SSTAR/FK1
      SIMULT1 = PSTAR/FK2
      CON2 = EXP(-PSTAR*WSTAR) + V*SIMULT1
      CON3 = QV*LOG(FK3*QV)
      HC1 = CHI/2.
      HC2 = (PSI + 2.*SSTAR)/2.
      HC3 = (PSI - 2.*PSTAR)/2.
      FLGS1 = LOG(BIGS1)
      FLGS2 = LOG(BIGS2)
      FLGS3 = LOG(BIGS3)
C
      RETURN
      END
