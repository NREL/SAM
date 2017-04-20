C****************************************************************
C FUNCTION ENTRPF IS USED IN THE GENERATION OF THE MAXIMUM ENTROPY
C DISTRIBUTION.  IT CODIFIES THE NONLINEAR FUNCTION THAT IS SOLVED
C USING BISECTION IN THE SUBROUTINE ENTRPY.  THIS FUNCTION IS:
C
C                B*EXP(B*BETA) - A*EXP(A*BETA)              1
C        AMU = ---------------------------------    -    ------
C                  EXP(B*BETA) - EXP(A*BETA)              BETA
C
C AND IS REWRITTEN FOR THIS ROUTINE AS:
C
C                B - A*EXP((A-B)*BETA)             1
C        AMU = -------------------------    -    ------
C                  1 - EXP((A-B)*BETA)            BETA
C
C
      REAL FUNCTION ENTRPF(BETA,A,AMU,B)
C
      DOUBLE PRECISION EAB,TERM,BETA1
C
      BETA1=BETA
      EAB=EXP(BETA1*(A-B))
      TERM=(B-A*EAB)/(1.0-EAB)
      ENTRPF=TERM-AMU-1.0/BETA1
C
      RETURN
      END
