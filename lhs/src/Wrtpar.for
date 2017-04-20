C     Last change:  S    29 Sep 97    5:17 pm
C****************************************************************
C SUBROUTINE WRTPAR PRINTS OUT THE DISTRIBUTION PARAMETERS
C
      SUBROUTINE WRTPAR
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
C      These statements removed to make modules work - GDW-96
C      DIMENSION XVLZ(MAXTB), PRBZ(MAXTB)
C      EQUIVALENCE (XX(1),XVLZ(1))
C      EQUIVALENCE (XX(MAXTB+1), PRBZ(1))
      CHARACTER LABEL*(LENC)
C
      REWIND 8
      REWIND 9
      WRITE(6,9001)TITLE
C
      DO 100 I=1,NV
         ID=IDIST(I)
         READ(9)LABEL
C
C        -- BETA DISTRIBUTION
         IF (ID .EQ. 1) THEN
            READ (8) A, B, P, Q
            XMN=(A*Q+B*P)/(P+Q)
            VAR=P*Q*(B-A)**2/((P+Q+1)*(P+Q)**2)
            WRITE (6,9002) I, DIST(IDSST(ID):IDSEND(ID)), A, B, LABEL
            WRITE (6,9006) P, Q, XMN, VAR
C
C        -- TRIANGLE DISTRIBUTION
         ELSE IF (ID .EQ. 8) THEN
            READ (8) A, B, C
            WRITE (6,9007) I, DIST(IDSST(ID):IDSEND(ID)),
     1                     LABEL, A, B, C
C
C        -- CUMULATIVE CONTINUOUS OR DISCRETE DISTRIBUTION
         ELSE IF (ID .EQ. 9  .OR.  ID .EQ. 10  .OR.  ID .EQ. 12) THEN
            READ (8) NP
            WRITE (6,9010) I, DIST(IDSST(ID):IDSEND(ID)), NP, LABEL
            READ (8) (XVLZ(J), PRBZ(J), J=1,NP)
            WRITE (6,9110) ( J, XVLZ(J), J, PRBZ(J), J=1,NP)
C
C        -- CONTINUOUS OR DISCRETE FREQUENCY DISTRIBUTION
         ELSE IF (ID .EQ. 11  .OR.  ID .EQ. 13) THEN
            READ (8) NP
            WRITE (6,9011) I, DIST(IDSST(ID):IDSEND(ID)), NP, LABEL
            READ (8) (XVLZ(J), PRBZ(J), J=1,NP)
            WRITE (6,9110) ( J, XVLZ(J), J, PRBZ(J), J=1,NP)
C
C        -- UNIFORM* OR LOGUNIFORM* DISTRIBUTION
         ELSE IF (ID .EQ. 6  .OR.  ID .EQ. 7) THEN
            READ (8) NINT
            WRITE (6,9004) I, DIST(IDSST(ID):IDSEND(ID)), NINT, LABEL
            DO 50 J=1, NINT
               READ (8) INT, A, B
               WRITE (6,9005) INT, A, B
   50       CONTINUE
C
C        -- POISSON, GEOMETRIC, OR EXPONENTIAL DISTRIBUTION
         ELSE IF (ID .EQ. 14  .OR.  ID .EQ. 15  .OR.  ID .EQ. 19) THEN
            READ (8) A
            WRITE (6,9008) I, DIST(IDSST(ID):IDSEND(ID)), LABEL, A
C
C        -- BINOMIAL OR NEGATIVE BINOMIAL DISTRIBUTION
         ELSE IF (ID .EQ. 16  .OR.  ID .EQ. 17) THEN
            READ (8) A,NINT
            WRITE (6,9009) I, DIST(IDSST(ID):IDSEND(ID)), LABEL, A
C
C        -- HYPERGEOMETRIC DISTRIBUTION
         ELSE IF (ID .EQ. 18) THEN
            READ (8) NN,N1,NR
            WRITE (6,9012) I,LABEL,NN,N1,NR
C
C        -- WEIBULL, PARETO OR GAMMA DISTRIBUTION
         ELSE IF (ID .EQ. 20  .OR.  ID .EQ. 21  .OR.
     1            ID .EQ. 22) THEN
            READ (8) A,B
            WRITE (6,9013) I, DIST(IDSST(ID):IDSEND(ID)), LABEL, A,B
C
C        -- INVERSE GAUSSIAN DISTRIBUTION
         ELSE IF (ID .EQ. 23) THEN
            READ (8) A,B
            WRITE (6,9014) I, DIST(IDSST(ID):IDSEND(ID)), LABEL, A,B
C
C        -- MAXIMUM ENTROPY DISTRIBUTION
         ELSE IF (ID .EQ. 24) THEN
            READ (8) A, AMU, B
            WRITE (6,9015) I, DIST(IDSST(ID):IDSEND(ID)),
     1                     LABEL, A, AMU, B
C
C        -- TRUNCATED OR BOUNDED EXPONENTIAL DISTRIBUTION
         ELSE IF (ID .EQ. 25  .OR.  ID .EQ. 26) THEN
            READ (8) A1, A, B
            WRITE (6,9016) I, DIST(IDSST(ID):IDSEND(ID)),
     1                     LABEL, A1, A, B
C
C        -- NORMAL-B AND LOGNORMAL-B DISTRIBUTIONS
         ELSE IF ( (IV1 .GT. 0 .AND. ( ID.EQ.2 .OR. ID.EQ.3 ))  .OR.
     1             ID .EQ. 27  .OR.  ID .EQ. 28 ) THEN
            READ (8) A, B
            WRITE (6,9002) I, DIST(IDSST(ID):IDSEND(ID)), A, B, LABEL
C
C        -- NORMAL AND LOGNORMAL-N DISTRIBUTIONS
         ELSE IF (ID .EQ. 2  .OR.  ID .EQ. 29) THEN
            READ (8) AMU,SIG
            WRITE (6,9017) I, DIST(IDSST(ID):IDSEND(ID)), LABEL,
     1                     AMU, SIG
C
C        -- LOGNORMAL DISTRIBUTIONS
         ELSE IF (ID .EQ. 3) THEN
            READ (8) AMU,ERF
            WRITE (6,9018) I, DIST(IDSST(ID):IDSEND(ID)), LABEL,
     1                     AMU, ERF
C
C        -- BOUNDED NORMAL AND BOUNDED LOGNORMAL (-N) DISTRIBUTIONS
         ELSE IF (ID.EQ.30 .OR. ID.EQ.32 .OR. ID.EQ.34) THEN
            READ (8) AMU,SIG,A,B
            WRITE (6,9002) I, DIST(IDSST(ID):IDSEND(ID)), A, B, LABEL
            IF (ID .EQ. 32) THEN
               WRITE (6,9111) AMU,SIG
            ELSE
               WRITE (6,9112) AMU,SIG
            END IF
C
C        -- TRUNCATED NORMAL AND LOGNORMAL-N DISTRIBUTIONS
         ELSE IF (ID.EQ.31 .OR. ID.EQ.35) THEN
            READ (8) AMU,SIG,A,B
            WRITE (6,9017) I, DIST(IDSST(ID):IDSEND(ID)), LABEL,
     1                     AMU, SIG
            WRITE (6,9113) A,B
C
C        -- TRUNCATED LOGNORMAL DISTRIBUTION
         ELSE IF (ID .EQ. 33) THEN
            READ (8) AMU,SIG,A,B
            WRITE (6,9018) I, DIST(IDSST(ID):IDSEND(ID)), LABEL,
     1                     AMU, SIG
            WRITE (6,9113) A,B
C
C        -- ALL OTHER DISTRIBUTIONS
         ELSE
            READ (8) A, B
            WRITE (6,9002) I, DIST(IDSST(ID):IDSEND(ID)), A, B, LABEL
         ENDIF
  100 CONTINUE
C
      RETURN
C
 9001 FORMAT('1',//,4X,A,//,4X,'VARIABLE  DISTRIBUTION',10X,
     1       'RANGE',12X,'LABEL')
 9002 FORMAT('0',5X,I3,5X,A11,2X,1PG10.3,' TO ',1PG10.3,2X,A50)
 9004 FORMAT('0',5X,I3,5X,A,'DISTRIBUTION WITH ',I2,' SUBINTERVALS',
     1       6X,A50)
 9005 FORMAT(' ',13X,I4,' OBS',5X,1PG10.3,' TO ',1PG10.3)
 9006 FORMAT(' ',13X,'WITH PARAMETERS  P = ',1PG10.3,2X,
     1       'Q = ',1PG10.3,/,
     1       14X,'THIS CHOICE OF PARAMETERS GIVES A ',/,14X,
     2       'POPULATION MEAN OF ',1PG10.3,'  AND A',/,14X,
     3       'POPULATION VARIANCE OF ',1PG10.3)
 9007 FORMAT('0',5X,I3,5X,A11,2X,'WITH PARAMETERS BELOW',5X,A50,/,27X,
     1       'A= ',1PG10.3,/,27X,'B= ',1PG10.3,/,27X,'C= ',1PG10.3)
 9008 FORMAT('0',5X,I3,5X,A,'WITH THE PARAMETER BELOW',5X,A50,/,
     1       27X,'P= ',1PG10.3)
 9009 FORMAT('0',5X,I3,5X,A,'WITH THE PARAMETERS BELOW',5X,A50,/,
     1       27X,'REAL PARAMETER= ',1PG10.3,15X,
     2       'INTEGER PARAMETER= ',I10)
 9010 FORMAT('0',5X,I3,5X,A,'DISTRIBUTION WITH ',
     1       I3,' POINTS',/,14X,A50)
 9011 FORMAT('0',5X,I3,5X,A,'DISTRIBUTION WAS CONVERTED TO A ',
     1       'CUMULATIVE DISTRIBUTION WITH ',I3,' POINTS',/,14X,A50)
 9012 FORMAT('0',5X,I3,5X,'HYPERGEOMETRIC DISTRIBUTION WITH THE ',
     1       'PARAMETERS BELOW',6X,A50,/,27X,'N = ',I10,15X,
     2       'N1 = ',I10,15X,'R = ',I10)
 9013 FORMAT('0',5X,I3,5X,A,'WITH THE PARAMETERS BELOW',5X,A50,/,
     1       27X,'ALPHA = ',1PG10.3,15X,'BETA = ',1PG10.3)
 9014 FORMAT('0',5X,I3,5X,A,'WITH THE PARAMETERS BELOW',5X,A50,/,
     1       27X,'MU = ',1PG10.3,15X,'LAMBDA = ',1PG10.3)
 9015 FORMAT('0',5X,I3,5X,A,'WITH PARAMETERS BELOW',5X,A50,/,27X,
     1       'A= ',1PG10.3,/,26X,'MU= ',1PG10.3,/,27X,'B= ',1PG10.3)
 9016 FORMAT('0',5X,I3,5X,A,'WITH PARAMETERS BELOW',5X,A50,/,27X,
     1       'LAMBDA = ',1PG10.3,/,26X,'A = ',1PG10.3,/,27X,'B = ',
     2       1PG10.3)
 9017 FORMAT('0',5X,I3,5X,A,'WITH THE PARAMETERS BELOW',5X,A50,/,
     1       27X,'MU = ',1PG10.3,15X,'SIGMA = ',1PG10.3)
 9018 FORMAT('0',5X,I3,5X,A,'WITH THE PARAMETERS BELOW',5X,A50,/,
     1       27X,'MEAN = ',1PG10.3,15X,'ERROR FACTOR = ',1PG10.3)
 9110 FORMAT(' ',13X,'X(',I2,') = ',1PG10.3,7X,'CUM PROB(',I2,') = ',
     1       1PG10.3)
 9111 FORMAT(27X,'MEAN = ',1PG10.3,15X,'ERROR FACTOR = ',1PG10.3)
 9112 FORMAT(27X,'MU = ',1PG10.3,15X,'SIGMA = ',1PG10.3)
 9113 FORMAT(27X,'BETWEEN QUANTILES ',1PG10.3,' AND ',1PG10.3)
C
      END
