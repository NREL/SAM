C     Last change:  S    17 Feb 98    1:11 pm
C****************************************************************
C SUBROUTINE CHKDAT CHECKS DISTRIBUTION PARAMETERS FOR CONSISTENCY
C
      SUBROUTINE CHKDAT(PAR1,A,MAXA)
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CPARAM.INC'                                              GDW-96  
      USE CPARAM                        
C
      DIMENSION A(MAXA)
      CHARACTER PAR1*(*),PLOG*3,PBETA*4,PTRI*3,PPOISN*4,PGEOM*4,
     1          PBINOM*5,PHYPER*6,PEXPON*5,PWEIBL*6,PPARET*6,
     2          PGAMMA*5,PIGAUS*6,PMAXEN*5,PTRUNC*5,PBOUND*5,
     3          PNORM*6,PMB*2,PMN*2
      CHARACTER*32 PAR
      PARAMETER (PLOG='LOG',PBETA='BETA',PTRI='TRI',PPOISN='POIS',
     1           PGEOM='GEOM',PBINOM='BINOM',PHYPER='HYPERG',
     2           PEXPON='EXPON',PWEIBL='WEIBUL',PPARET='PARETO',
     3           PGAMMA='GAMMA',PIGAUS='INVERS',PMAXEN='MAXIM',
     4           PTRUNC='TRUNC',PBOUND='BOUND',PNORM='NORMAL',
     5           PMB='-B',PMN='-N')
C
C     --- 3/5/96 CHANGED THE VARIABLE PASSED INTO THIS ROUTINE FROM PAR
C     --- TO PAR1, AND DIMENSIONED AND USED A NEW VARIABLE OF FIXED LENGTH
C     --- LENGTH NAMED PAR FOR USE IN THIS ROUTINE TO AVOID LOOKING BEYOND
C     --- THE END OF THE PASSED IN ARRAY.  CLEAR IT OUT FIRST, THOUGH.
C
      PAR = ' '
      PAR = PAR1
C
C     --- CHECK A TRIANGULAR DISTRIBUTION
C
      IF (PAR(1:3) .EQ. PTRI) THEN
         IF (A(1) .GT. A(2)  .OR.  A(2) .GT. A(3)  .OR.
C     1       A(1) .EQ. A(3)) THEN     ----- changed 1-12-96
     1        A(1) .GE. A(3)) THEN
            WRITE (6,9000) A(1),A(2),A(3)
            WRITE (99,9000) A(1),A(2),A(3)
            KLLERR = .TRUE.
            RETURN
         END IF
         WRITE (8) A(1),A(2),A(3)
         RETURN
      END IF
C
C     --- CHECK A MAXIMUM ENTROPY DISTRIBUTION
C
      IF (PAR(1:5) .EQ. PMAXEN) THEN
         IF (A(1) .GE. A(2)  .OR.  A(2) .GE. A(3)) THEN
            WRITE (6,9013) A(1),A(2),A(3)
            WRITE (99,9013) A(1),A(2),A(3)
            KLLERR = .TRUE.
            RETURN
         END IF
         WRITE (8) A(1),A(2),A(3)
         RETURN
      END IF
C
C     --- CHECK A POISSON DISTRIBUTION
C
      IF (PAR(1:4) .EQ. PPOISN) THEN
         IF (A(1) .LE. 0.0  .OR.  A(1) .GT. 1.0E6) THEN
            WRITE (6,9004) A(1)
            WRITE (99,9004) A(1)
            KLLERR = .TRUE.
            RETURN
         END IF
         WRITE (8) A(1)
         RETURN
      END IF
C
C     --- CHECK A GEOMETRIC DISTRIBUTION
C
      IF (PAR(1:4) .EQ. PGEOM) THEN
         IF (A(1) .LE. 0.0  .OR.  A(1) .GE. 1.0) THEN
            WRITE (6,9005) A(1)
            WRITE (99,9005) A(1)
            KLLERR = .TRUE.
            RETURN
         END IF
         WRITE (8) A(1)
         RETURN
      END IF
C
C     --- CHECK A BINOMIAL OR NEGATIVE BINOMIAL DISTRIBUTION
C
      IF (INDEX(PAR,PBINOM) .NE. 0) THEN
         IF (A(1) .LE. 0.0  .OR.  A(1) .GE. 1.0) THEN
            WRITE (6,9006) PAR,A(1)
            WRITE (99,9006) PAR,A(1)
            KLLERR = .TRUE.
            RETURN
         END IF
         NN=A(2)+0.01
         IF (NN .LE. 0) THEN
            WRITE (6,9007) PAR,NN
            WRITE (99,9007) PAR,NN
            KLLERR = .TRUE.
            RETURN
         END IF
         WRITE (8) A(1),NN
         RETURN
      END IF
C
C     --- CHECK A HYPERGEOMETRIC DISTRIBUTION
C
      IF (PAR(1:6) .EQ. PHYPER) THEN
         NN=A(1)+0.01
         N1=A(2)+0.01
         NR=A(3)+0.01
         IF (NN .LE.  0  .OR.  N1 .LE.  0  .OR.  NR .LE.  0  .OR.
     1       N1 .GE. NN  .OR.  NR .GE. NN  .OR.  N1 .GE. NR ) THEN
            WRITE (6,9008) NN,N1,NR
            WRITE (99,9008) NN,N1,NR
            KLLERR = .TRUE.
            RETURN
         END IF
         WRITE (8) NN,N1,NR
         RETURN
      END IF
C
C     --- CHECK AN EXPONENTIAL DISTRIBUTION
C
      IF (INDEX(PAR,PEXPON) .GT. 0) THEN
         IF (A(1) .LE. 0.0) THEN
            WRITE (6,9009) A(1)
            WRITE (99,9009) A(1)
            KLLERR = .TRUE.
            RETURN
         END IF
         IF (INDEX(PAR,PTRUNC) .GT. 0) THEN
            IF (A(2) .LE. 0.0  .OR.  A(2) .GE. 1.0  .OR.
     1          A(3) .LE. 0.0  .OR.  A(3) .GE. 1.0  .OR.
     2          A(2) .GE. A(3) ) THEN
               WRITE (6,9014) PAR,A(2),A(3)
               WRITE (99,9014) PAR,A(2),A(3)
               KLLERR = .TRUE.
               RETURN
            END IF
            WRITE (8) A(1),A(2),A(3)
            RETURN
         ELSE IF (INDEX(PAR,PBOUND) .GT. 0) THEN
            IF (A(2) .LE. 0.0  .OR.  A(3) .LE. 0.0  .OR.
     1          A(2) .GE. A(3) ) THEN
               WRITE (6,9015) PAR,A(2),A(3)
               WRITE (99,9015) PAR,A(2),A(3)
               KLLERR = .TRUE.
               RETURN
            END IF
            WRITE (8) A(1),A(2),A(3)
            RETURN
         ELSE
            WRITE (8) A(1)
            RETURN
         END IF
      END IF
C
C     --- CHECK A WEIBULL DISTRIBUTION
C
      IF (PAR(1:6) .EQ. PWEIBL) THEN
         IF (A(1) .LE. 0.0  .OR.  A(2) .LE. 0.0) THEN
            WRITE (6,9010) A(1),A(2)
            WRITE (99,9010) A(1),A(2)
            KLLERR = .TRUE.
            RETURN
         END IF
         WRITE (8) A(1),A(2)
         RETURN
      END IF
C
C     --- CHECK A PARETO DISTRIBUTION
C
      IF (PAR(1:6) .EQ. PPARET) THEN
         IF (A(1) .LE. 2.0  .OR.  A(2) .LE. 0.0) THEN
            WRITE (6,9011) A(1),A(2)
            WRITE (99,9011) A(1),A(2)
            KLLERR = .TRUE.
            RETURN
         END IF
         WRITE (8) A(1),A(2)
         RETURN
      END IF
C
C     --- CHECK A GAMMA DISTRIBUTION
C
      IF (PAR(1:5) .EQ. PGAMMA) THEN
         WRITE (8) A(1),A(2)
         RETURN
      END IF
C
C     --- CHECK AN INVERSE GAUSSIAN DISTRIBUTION
C
      IF (PAR(1:6) .EQ. PIGAUS) THEN
         IF (A(1) .LE. 0.0  .OR.  A(2) .LE. 0.0) THEN
            WRITE (6,9012) A(1),A(2)
            WRITE (99,9012) A(1),A(2)
            KLLERR = .TRUE.
            RETURN
         END IF
         WRITE (8) A(1),A(2)
         RETURN
      END IF
C
C     --- CHECK NORMAL AND LOGNORMAL DISTRIBUTIONS IF VERSION 1 INPUT
C     --- COMPATIBILITY IS NOT SPECIFIED
C
      IF (IV1 .EQ. 0  .AND.  INDEX(PAR,PNORM) .NE. 0) THEN
         IF (INDEX(PAR,PLOG) .LE. 0) THEN
C
C           -- NORMAL DISTRIBUTION
C
            IF (INDEX(PAR,PMB) .GT. 0) THEN
C              -- NORMAL-B DISTRIBUTION
               IF (A(1) .GE. A(2)) THEN
                  WRITE (6,9001) PAR,A(1),A(2)
                  WRITE (99,9001) PAR,A(1),A(2)
                  KLLERR = .TRUE.
                  RETURN
               END IF
               WRITE (8) A(1),A(2)
               RETURN
C
            ELSE IF (INDEX(PAR,PBOUND) .GT. 0) THEN
C              -- BOUNDED NORMAL DISTRIBUTION
               IF (A(3) .GE. A(4)) THEN
                  WRITE (6,9015) PAR,A(3),A(4)
                  WRITE (99,9015) PAR,A(3),A(4)
                  KLLERR = .TRUE.
                  RETURN
               END IF
               WRITE (8) A(1),A(2),A(3),A(4)
               RETURN
C
            ELSE IF (INDEX(PAR,PTRUNC) .GT. 0) THEN
C              -- TRUNCATED NORMAL DISTRIBUTION
               IF (A(3) .LE. 0.0  .OR.  A(4) .LE. 0.0  .OR.
     1             A(3) .GE. 1.0  .OR.  A(4) .GE. 1.0  .OR.
     2             A(3) .GE. A(4) ) THEN
                  WRITE (6,9014) PAR, A(3), A(4)
                  WRITE (99,9014) PAR, A(3), A(4)
                  KLLERR = .TRUE.
                  RETURN
               END IF
               WRITE (8) A(1),A(2),A(3),A(4)
               RETURN
C
            ELSE
C              -- NORMAL DISTRIBUTION
               IF (A(2) .LE. 0.0) THEN
                  WRITE (6,9017) PAR, A(2)
                  WRITE (99,9017) PAR, A(2)
                  KLLERR = .TRUE.
                  RETURN
               END IF
               WRITE (8) A(1), A(2)
               RETURN
            END IF
C
         ELSE
C
C        --- LOGNORMAL DISTRIBUTION
C
            IF (INDEX(PAR,PMB) .GT. 0) THEN
C              -- LOGNORMAL-B DISTRIBUTION
               IF (A(1) .LE. 0.0  .OR.  A(2) .LE. 0.0) THEN
                  WRITE(6,9002)PAR,A(1),A(2)
                  WRITE(99,9002)PAR,A(1),A(2)
                  KLLERR = .TRUE.
                  RETURN
               ELSE IF (A(1) .GE. A(2)) THEN
                  WRITE(6,9001)PAR,A(1),A(2)
                  WRITE(99,9001)PAR,A(1),A(2)
                  KLLERR = .TRUE.
                  RETURN
               END IF
               WRITE (8) A(1), A(2)
               RETURN
            END IF
C
            IF (INDEX(PAR,PMN) .GT. 0) THEN
C              -- LOGNORMAL-N TYPE DISTRIBUTION SPECIFIED
               IF (A(2) .LE. 0.0) THEN
                  WRITE (6,9017) PAR, A(2)
                  WRITE (99,9017) PAR, A(2)
                  KLLERR = .TRUE.
                  RETURN
               END IF
C
            ELSE
C              -- AN ERROR FACTOR-TYPE DISTRIBUTION WAS SELECTED
               IF (A(1) .LE. 0.0  .OR.  A(2) .LE. 1.0) THEN
                  WRITE (6,9018) PAR, A(1),A(2)
                  WRITE (99,9018) PAR, A(1),A(2)
                  KLLERR = .TRUE.
                  RETURN
               END IF
            END IF
C
            IF (INDEX(PAR,PTRUNC) .GT. 0) THEN
C              -- TRUNCATED LOGNORMAL OR LOGNORMAL-N DISTRIBUTION
               IF (A(3) .LE. 0.0  .OR.  A(4) .LE. 0.0  .OR.
     1             A(3) .GE. 1.0  .OR.  A(4) .GE. 1.0  .OR.
     2             A(3) .GE. A(4) ) THEN
                  WRITE (6,9014) PAR, A(3), A(4)
                  WRITE (99,9014) PAR, A(3), A(4)
                  KLLERR = .TRUE.
                  RETURN
               END IF
               WRITE (8) A(1),A(2),A(3),A(4)
               RETURN
C
            ELSE IF (INDEX(PAR,PBOUND) .GT. 0) THEN
C              -- BOUNDED LOGNORMAL OR LOGNORMAL-N DISTRIBUTION
               IF (A(3) .GE. A(4)) THEN
                  WRITE (6,9015) PAR,A(3),A(4)
                  WRITE (99,9015) PAR,A(3),A(4)
                  KLLERR = .TRUE.
                  RETURN
               ELSE IF (A(3) .LE. 0.0  .OR.  A(4) .LE. 0.0) THEN
                  WRITE (6,9016) PAR,A(3),A(4)
                  WRITE (99,9016) PAR,A(3),A(4)
                  KLLERR = .TRUE.
                  RETURN
               END IF
               WRITE (8) A(1),A(2),A(3),A(4)
               RETURN
C
            ELSE
C              -- DISTRIBUTION NOT BOUNDED OR TRUNCATED
               WRITE (8) A(1),A(2)
               RETURN
            END IF
C
         END IF
C
      ELSE
C
C        --- CHECK FOR INVALID VERSION 1 INPUT TYPES
C
         IF (INDEX(PAR,PBOUND) .GT. 0  .OR.  INDEX(PAR,PTRUNC) .GT. 0
     1      .OR.  INDEX(PAR,PMB) .GT. 0  .OR.  INDEX(PAR,PMN) .GT. 0 )
     2      THEN
            WRITE (6,9019) PAR
            WRITE (99,9019) PAR
            KLLERR = .TRUE.
            RETURN
         END IF
C
      END IF
C
C     --- CHECK RANGE FOR ALL OTHER TYPES OF DISTRIBUTIONS
C
      IF (A(1) .GE. A(2)) THEN
         WRITE(6,9001)PAR,A(1),A(2)
         WRITE(99,9001)PAR,A(1),A(2)
         KLLERR = .TRUE.
         RETURN
      END IF
C
C     --- PERFORM SPECIAL CHECK FOR BETA DISTRIBUTION
C
      IF (PAR .EQ. PBETA) THEN
         IF (A(3) .LT. 0.001  .OR.  A(4) .LT. 0.001  .OR.
     1       A(3) .GT. 1.0E+07  .OR.  A(4) .GT. 1.0E+07 ) THEN
            WRITE(6,9003)A(3),A(4)
            WRITE(99,9003)A(3),A(4)
            KLLERR = .TRUE.
            RETURN
         END IF
         WRITE(8)A(1),A(2),A(3),A(4)
         RETURN
      END IF
C
C     --- PERFORM SPECIAL CHECK FOR LOGUNIFORM AND LOGNORMAL DISTRIBUTIONS
C
      IF (PAR(1:3) .EQ. PLOG) THEN
         IF (A(1) .LE. 0.0  .OR.  A(2) .LE. 0.0) THEN
            WRITE(6,9002)PAR,A(1),A(2)
            WRITE(99,9002)PAR,A(1),A(2)
            KLLERR = .TRUE.
            RETURN
         END IF
         WRITE(8)A(1),A(2)
         RETURN
      END IF
C
C     --- FOR THOSE DISTRIBUTIONS THAT DO NOT HAVE SPECIAL CHECKS ABOVE,
C     --- WRITE OUT THE PARAMETERS AND RETURN.
C
      WRITE(8)A(1),A(2)
      RETURN
C
 9000 FORMAT('1',5X,'FOR THE TRIANGULAR DISTRIBUTION THE PARAMETERS ',
     1       'A,B,C ',3G20.10,/,6X,'HAVE BEEN INPUT IN THE INCORRECT ',
     2       'ORDER',/,6X,'PLEASE CONSULT THE USER MANUAL FOR ',
     3       'INSTRUCTIONS ON THE ORDER OF THE PARAMETERS')
 9001 FORMAT('1',5X,'FOR THE ',A,'DISTRIBUTION THE LOWER LIMIT A ',
     1       G20.10,/,6X,'IS GREATER THAN OR EQUAL TO THE UPPER ',
     2       'LIMIT B ',G20.10)
 9002 FORMAT('1',5X,'FOR THE ',A,'DISTRIBUTION THE LOWER LIMIT A ',
     1       G20.10,/,6X,'OR THE UPPER LIMIT B ',G20.10,/,6X,
     2       'SHOULD NOT BE LESS THAN OR EQUAL TO ZERO')
 9003 FORMAT('1',5X,'FOR THE BETA DISTRIBUTION THE PARAMETERS P ',
     1       'AND Q SHOULD BE BETWEEN 0.001 AND 1.0E+07.',/,6X,
     2       'THE FOLLOWING VALUES WERE FOUND: P = ',G20.10,17X,
     3       ' Q = ',G20.10)
 9004 FORMAT('1',5X,'FOR A POISSON DISTRIBUTION, THE PARAMETER MUST ',
     1       'BE GREATER THAN ZERO AND LESS THAN 1.0E6.',/,5X,
     2       'A VALUE OF ',G20.10,' WAS FOUND.')
 9005 FORMAT('1',5X,'FOR A GEOMETRIC DISTRIBUTION, THE PARAMETER ',
     1       'MUST BE GREATER THAN ZERO AND LESS THAN ONE.',/,5X,
     2       'A VALUE OF ',G20.10,' WAS FOUND.')
 9006 FORMAT('1',5X,'FOR A ',A,'DISTRIBUTION, THE REAL PARAMETER ',
     1       'MUST BE GREATER THAN ZERO AND LESS THAN ONE.',/,5X,
     2       'A VALUE OF ',G20.10,' WAS FOUND.')
 9007 FORMAT('1',5X,'FOR A ',A,'DISTRIBUTION, THE INTEGER PARAMETER ',
     1       'MUST BE GREATER THAN ZERO.',/,5X,
     2       'A VALUE OF ',I10,' WAS FOUND.')
 9008 FORMAT('1',5X,'FOR THE HYPERGEOMETRIC DISTRIBUTION THE INTEGER',
     1       'PARAMETERS N, N1 AND R ',/6X,
     2       'HAVE BEEN INPUT INCORRECTLY.  N = ',I10,'  N1 = ',I10,
     3       '  R = ',I10,/,6X,'PLEASE CONSULT THE USER MANUAL FOR ',
     4       'INSTRUCTIONS ON THE PARAMETERS')
 9009 FORMAT('1',5X,'FOR AN EXPONENTIAL DISTRIBUTION, THE PARAMETER ',
     1       'MUST BE GREATER THAN ZERO.',/,5X,
     2       'A VALUE OF ',G20.10,' WAS FOUND.')
 9010 FORMAT('1',5X,'FOR A WEIBULL DISTRIBUTION, BOTH PARAMETERS ',
     1       'MUST BE GREATER THAN ZERO.',/,6X,
     2       'VALUES OF ',G20.10,' AND ',G20.10,' WERE FOUND.')
 9011 FORMAT('1',5X,'FOR A PARETO DISTRIBUTION, ALPHA MUST BE ',
     1       'GREATER THAN TWO AND',/,6X,'BETA MUST BE GREATER ',
     2       'THAN ZERO.  THE FOLLOWING VALUES WERE FOUND:',/,6X,
     3       'ALPHA = ',G20.10,17X,'BETA = ',G20.10)
 9012 FORMAT('1',5X,'FOR AN INVERSE GAUSSIAN DISTRIBUTION, BOTH ',
     1       'PARAMETERS MUST BE GREATER THAN ZERO.',/,6X,
     2       'THE FOLLOWING VALUES WERE FOUND:  MU = ',
     3       G20.10,17X,'LAMBDA = ',G20.10)
 9013 FORMAT('1',5X,'FOR THE MAXIMUM ENTROPY DISTRIBUTION THE ',
     1       'PARAMETERS A, MU, AND C ',3G20.10,/,6X,
     2       'HAVE BEEN INPUT IN THE INCORRECT ORDER',/,6X,
     3       'PLEASE CONSULT THE USER MANUAL FOR INSTRUCTIONS ON ',
     4       'THE ORDER OF THE PARAMETERS')
 9014 FORMAT('1',5X,'FOR A ',A,'DISTRIBUTION, THE ',
     1       'TRUNCATION PARAMETERS MUST BE AS FOLLOWS: ',/,30X,
     2       '0.0 < A < B < 1.0',//,6X,
     3       'THE FOLLOWING PARAMETERS WERE FOUND:    A = ',G20.10,
     4       10X,'B = ',G20.10)
 9015 FORMAT('1',5X,'FOR A ',A,'DISTRIBUTION, THE ',
     1       'TRUNCATION PARAMETERS MUST BE AS FOLLOWS: ',/,30X,
     2       '0.0 < A < B ',//,6X,
     3       'THE FOLLOWING PARAMETERS WERE FOUND:    A = ',G20.10,
     4       10X,'B = ',G20.10)
 9016 FORMAT('1',5X,'FOR THE ',A,'DISTRIBUTION THE LOWER BOUND ',
     1       G20.10,/,6X,'OR THE UPPER BOUND ',G20.10,/,6X,
     2       'SHOULD NOT BE LESS THAN OR EQUAL TO ZERO')
 9017 FORMAT('1',5X,'THE STANDARD DEVIATION OF A ',A,
     1       'DISTRIBURION MUST BE POSITIVE.',/,6X,
     2       'THE FOLLOWING VALUE WAS FOUND:  ',G20.10)
 9018 FORMAT('1',5X,'FOR A ',A,'DISTRIBURION, THE MEAN MUST BE ',
     1       'POSITIVE AND THE ERROR FACTOR MUST BE GREATER THAN ONE.',
     2       /,6X,'THE FOLLOWING VALUES WERE FOUND:  MEAN = ',G20.10,
     3       15X,'ERROR FACTOR = ',G20.10)
 9019 FORMAT('1',5X,'THE ',A,'DISTRIBUTION IS NOT ALLOWED WHEN THE ',
     1       'VERSION 1 INPUT COMPATIBILITY RECORD IS SPECIFIED.')
C
      END
