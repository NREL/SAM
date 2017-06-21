C     Last change:  S    26 Jan 98   10:20 am
C****************************************************************
C SUBROUTINE TRIANG GENERATES THE TRIANGULAR DISTRIBUTION
C
      SUBROUTINE TRIANG(J)
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
      LOC(I,J)=(J-1)*N+I
C
      PROBINC=1./FLOAT(N)
      IF(IRS.EQ.1)PROBINC=1.0
      READ(8)A,B,C
      C1=C-A
      C2=(B-A)/C1
      STRTPT=0.
      DO I = 1, N
        R=PROBINC*RNUM1()+STRTPT
        IF(R.LE.C2)THEN
          X(LOC(I,J))=A+SQRT(R*C1*(B-A))
        ELSE
          X(LOC(I,J))=C-SQRT((1.-R)*C1*(C-B))
        ENDIF
        IF (IRS == 0) STRTPT = REAL(I) / REAL(N)
      END DO
C
      RETURN
      END
