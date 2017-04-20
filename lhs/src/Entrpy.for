C     Last change:  S    17 Feb 98    1:37 pm
C****************************************************************
C SUBROUTINE ENTRPY GENERATES THE MAXIMUM ENTROPY DISTRIBUTION
C
      SUBROUTINE ENTRPY(J)
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CPARAM.INC'                                              GDW-96  
      USE CPARAM                        
C     INCLUDE 'CSAMP.INC'                                               GDW-96  
      USE CSAMP                         
C
      DOUBLE PRECISION BETA,RBETA,EA,EB,TERM
c     2-14-96 changes:
c     change made to replace call to function D1MACH with call to LF90
c     routines:
c     call for D1MACH(2) replaced with call to LF90 routine HUGE
c     Assume double precision numbers are represented in the T-digit,
c     base-B form:
c        sign (B**E)*( (X(1)/B + ... + (X(T)/B**T) )
c        where 0 .LE. X(I) .LT. B for I=1,...,T, 0 .LT. X(1), and
c        EMIN .LE. E .LE. EMAX.
c     D1MACH(2) = B**EMAX*(1-B**(-T)), the largest magnitude
c     value of D1MACH(2) was set = 0.176173927216507D+309 for IBM-compatible
c     PCs in double precision function D1MACH
c      DOUBLE PRECISION D1MACH     ! changed 2-14-96
      Double Precision D1MACH2, DARG
      data DARG / 0.0D+0 /
C
      LOC(I,J)=(J-1)*N+I
C
      PROBINC=1/FLOAT(N)
      IF (IRS .NE. 0) PROBINC=1.0
      STRTPT=0.0
      READ (8) A, AMU, B
C
C     --- IF AMU IS VERY CLOSE TO (B-A)/2, THEN USE A UNIFORM DISTRIBUTION
C     --- BECAUSE THE BETA PARAMETER IS ESSENTIALLY ZERO.
C
      DELTA = ( (2.0*AMU) / (B+A) ) - 1.0
      IF (ABS(DELTA) .LT. 1.0E-4) THEN
         DO 30 I=1,N
            R=PROBINC*RNUM1() + STRTPT
            X(LOC(I,J))=A+R*(B-A)
            IF (IRS .EQ. 0) STRTPT=STRTPT+PROBINC
   30    CONTINUE
         RETURN
      END IF
C
C     --- CALCULATE THE PARAMETER BETA FOR THE MAXIMUM ENTROPY DISTRIBUTION
C     --- THIS PARAMETER MUST BE FOUND BY SOLVING THE FOLLOWING NONLINEAR
C     --- FUNCTION FOR BETA (CLODED AS REAL FUNCTION ENTRPF):
C
C                B*EXP(B*BETA) - A*EXP(A*BETA)              1
C        AMU = ---------------------------------    -    ------
C                  EXP(B*BETA) - EXP(A*BETA)              BETA
C
C     --- THE SOLUTION IS DONE USING A BISECTION METHOD CODED BELOW
C
C     --- FIRST, SELECT THE BOUNDS FOR THE BISECTION ALGORITHM AND
C     --- CALCULATE THE CORRESPONDING FUNCTION VALUES (..HI  AND ..LO)
C
      IF (AMU .GT. (B+A)/2.0) THEN
         ICASE=1
      ELSE
         ICASE=-1
      END IF
      BETALO=ICASE*1.0E-4
c     **** revised 8/9/95 gdw for out of bounds condition
c      BETAHI = ICASE * LOG( D1MACH(2)/(2.0*MAX(A,1.0)) ) / (B-A)
c     changed 2-14-96 to use LF90 function
      D1MACH2 = HUGE(DARG)
      BETAHI = ICASE * LOG( D1MACH2 /(2.0*MAX(A,1.0)) ) / (B-A)
c
      RESLO=ENTRPF(BETALO,A,AMU,B)
      RESHI=ENTRPF(BETAHI,A,AMU,B)
C
C     --- IF BOTH FUNCTION VALUES ARE ON THE SAME SIDE OF ZERO THEN
C     --- BISECTION CAN NOT WORK, SO DECLARE AN ERROR AND STOP
C
      IF (RESLO*RESHI .GT. 0.0) THEN
C        -- ERROR IN PARAMETERS IN BISECTION
         WRITE (6,9999)
         WRITE (99,9999)
 9999    FORMAT ('1',10X,'THE BISECTION METHOD USED TO DETERMINE ',
     1           'THE DISTRIBUTION PARAMETER IN THE MAXIMUM ENTROPY',
     2           /,11X,'DISTRIBUTION FAILED BECAUSE THE DISTRIBUTION ',
     3           'RANGE REQUESTED BY THE USER IS TOO LARGE.',/,11X,
     4           'PLEASE CONSULT THE USERS MANUAL FOR MORE ',
     5           'INFORMATION.')
         KLLERR = .TRUE.
         RETURN
      END IF
C
C     --- NOW ENTER THE ITERATION PORTION OF THE BISECTION ROUTINE
C
      NITER=0
 100  NITER=NITER+1
      IF (NITER .GT. 1000) THEN
C        - PROBABLY AN INFINITE LOOP
         WRITE (6,*) 'ENTRPY: Bisection did not converge!'
         WRITE (99,*) 'ENTRPY: Bisection did not converge!'
         KLLERR = .TRUE.
         RETURN
      END IF
      BETAMD=0.50*(BETALO+BETAHI)
      RESMD=ENTRPF(BETAMD,A,AMU,B)
      IF (RESLO*RESMD .GT. 0.0) THEN
         BETALO=BETAMD
      ELSE
         BETAHI=BETAMD
      END IF
      IF (BETAHI/BETALO .GT. 1.00001) GO TO 100
C
C     --- SELECT THE PARAMETER BETA TO BE THE MIDPOINT BETWEEN THE
C     --- HI AND LO BOUNDS FOUND ON THE LAST ITERATION, AND COMPUTE
C     --- CONSTANTS FOR THE INVERSE FUNCTION FOR THE MAXIMUM ENTROPY
C     --- DISTRIBUTION USING THE NEWLY FOUND PARAMETER BETA.
C
      BETA=0.50*(BETAHI+BETALO)
      RBETA=1.0/BETA
      EA=EXP(BETA*A)
      EB=EXP(BETA*B)
      TERM=EB-EA
C
C     --- CALCULATE THE MAXIMUM ENTROPY DISTRIBUTION FROM THE INVERSE FUNCTION
C
      DO I = 1, N
         R=PROBINC*RNUM1()+STRTPT
         X(LOC(I,J)) = RBETA * LOG(TERM * R + EA)
         IF (IRS == 0) STRTPT = REAL(I) / REAL(N)
      END DO
C
      RETURN
      END
