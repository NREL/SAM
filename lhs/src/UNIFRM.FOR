C****************************************************************
C SUBROUTINE UNIFRM GENERATES THE UNIFORM, LOGUNIFORM, UNIFORM*,
C OR THE LOGUNIFORM* DISTRIBUTIONS
C
      SUBROUTINE UNIFRM(J,IDT)
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
      I=0
      IF(IDT.EQ.6.OR.IDT.EQ.7)THEN
        READ(8)NINT
      ELSE
        NINT=1
        NPT=N
      ENDIF
      DO 40 K=1,NINT
        IF(IDT.EQ.4.OR.IDT.EQ.5)THEN
           READ(8)A,B
        ELSE
           READ(8)NPT,A,B
           IF(NPT.EQ.0)GO TO 40
        ENDIF
        IF(IDT.EQ.5.OR.IDT.EQ.7)THEN
          A=LOG10(A)
          B=LOG10(B)
        ENDIF
        PROBINC=(B-A)/FLOAT(NPT)
        IF(IRS.NE.0)PROBINC=B-A
        DO 30 ID=1,NPT
          I=I+1
          IF(IRS.EQ.0)THEN
            STRTPT=(ID-1)*PROBINC
          ELSE
            STRTPT=0.0
          ENDIF
          X(LOC(I,J))=A+STRTPT+PROBINC*RNUM1()
          IF(IDT.EQ.5.OR.IDT.EQ.7)X(LOC(I,J))=10.**(X(LOC(I,J)))
   30   CONTINUE
   40 CONTINUE
      RETURN
      END
