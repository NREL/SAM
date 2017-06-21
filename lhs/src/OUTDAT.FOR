C****************************************************************
C SUBROUTINE OUTDAT OUTPUTS THE SAMPLE AND ITS RANKS
C
      SUBROUTINE OUTDAT(IRK)
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
      CHARACTER LAB1*2,LAB2
      PARAMETER (LAB1='X(',LAB2=')')
C
      LOC(I,J)=(J-1)*N+I
C
      K=1
      I1=1
      I2=12
      ITEMP=NV/12
      IF (ITEMP*12.NE.NV) ITEMP=ITEMP+1
  300 IF (K.EQ.ITEMP) GO TO 350
      IF (K.GT.1) GO TO 320
      WRITE(6,820)TITLE
      IF(IRS.EQ.0)THEN
        IF(IRK.EQ.0)THEN
          WRITE(6,810)(LAB1,I,LAB2,I=I1,I2)
        ELSE
          WRITE(6,850)(LAB1,I,LAB2,I=I1,I2)
        ENDIF
      ELSE
        IF(IRK.EQ.0)THEN
          WRITE(6,815)(LAB1,I,LAB2,I=I1,I2)
        ELSE
          WRITE(6,855)(LAB1,I,LAB2,I=I1,I2)
        ENDIF
      ENDIF
      DO 310 J=1,N
        IF(IRK.EQ.0)THEN
          WRITE(6,840)J,(X(LOC(J,I)),I=I1,I2)
        ELSE
          WRITE(6,860)J,(X(LOC(J,I)),I=I1,I2)
        ENDIF
  310 CONTINUE
      GO TO 340
  320 WRITE(6,820)TITLE
      IF(IRS.EQ.0)THEN
        IF(IRK.EQ.0)THEN
          WRITE(6,830)(LAB1,I,LAB2,I=I1,I2)
        ELSE
          WRITE(6,870)(LAB1,I,LAB2,I=I1,I2)
        ENDIF
      ELSE
        IF(IRK.EQ.0)THEN
          WRITE(6,835)(LAB1,I,LAB2,I=I1,I2)
        ELSE
          WRITE(6,875)(LAB1,I,LAB2,I=I1,I2)
        ENDIF
      ENDIF
      DO 330 J=1,N
        IF(IRK.EQ.0)THEN
          WRITE(6,840)J,(X(LOC(J,I)),I=I1,I2)
        ELSE
          WRITE(6,860)J,(X(LOC(J,I)),I=I1,I2)
        ENDIF
  330 CONTINUE
  340 K=K+1
      I1=I1+12
      I2=I2+12
      GO TO 300
  350 IF (K.GT.1) GO TO 370
      WRITE(6,820)TITLE
      IF(IRS.EQ.0)THEN
        IF(IRK.EQ.0)THEN
          WRITE(6,810)(LAB1,I,LAB2,I=I1,NV)
        ELSE
          WRITE(6,850)(LAB1,I,LAB2,I=I1,NV)
        ENDIF
      ELSE
        IF(IRK.EQ.0)THEN
          WRITE(6,815)(LAB1,I,LAB2,I=I1,NV)
        ELSE
          WRITE(6,855)(LAB1,I,LAB2,I=I1,NV)
        ENDIF
      ENDIF
      DO 360 J=1,N
        IF(IRK.EQ.0)THEN
          WRITE(6,840)J,(X(LOC(J,I)),I=I1,NV)
        ELSE
          WRITE(6,860)J,(X(LOC(J,I)),I=I1,NV)
        ENDIF
  360 CONTINUE
      GO TO 390
  370 WRITE(6,820)TITLE
      IF(IRS.EQ.0)THEN
        IF(IRK.EQ.0)THEN
          WRITE(6,830)(LAB1,I,LAB2,I=I1,NV)
        ELSE
          WRITE(6,870)(LAB1,I,LAB2,I=I1,NV)
        ENDIF
      ELSE
        IF(IRK.EQ.0)THEN
          WRITE(6,835)(LAB1,I,LAB2,I=I1,NV)
        ELSE
          WRITE(6,875)(LAB1,I,LAB2,I=I1,NV)
        ENDIF
      ENDIF
      DO 380 J=1,N
        IF(IRK.EQ.0)THEN
          WRITE(6,840)J,(X(LOC(J,I)),I=I1,NV)
        ELSE
          WRITE(6,860)J,(X(LOC(J,I)),I=I1,NV)
        ENDIF
  380 CONTINUE
  390 CONTINUE
      RETURN
  810 FORMAT('0','LATIN HYPERCUBE SAMPLE INPUT VECTORS',//,' RUN NO.',
     1       1X,A,I1,A,8(6X,A,I1,A),3(5X,A,I2,A))
  815 FORMAT('0','RANDOM SAMPLE INPUT VECTORS',//,' RUN NO.',
     1       1X,A,I1,A,8(6X,A,I1,A),3(5X,A,I2,A))
  820 FORMAT('1',A)
  830 FORMAT('0','LATIN HYPERCUBE SAMPLE INPUT VECTORS',//,' RUN NO.',
     1       2X,A,I2,A,11(5X,A,I2,A))
  835 FORMAT('0','RANDOM SAMPLE INPUT VECTORS',//,' RUN NO.',
     1       2X,A,I2,A,11(5X,A,I2,A))
  840 FORMAT('0',I5,12(1PG10.3))
  850 FORMAT('0','RANKS OF LATIN HYPERCUBE SAMPLE INPUT VECTORS',//,
     1       ' RUN NO.',4X,A,I1,A,8(6X,A,I1,A),3(5X,A,I2,A))
  855 FORMAT('0','RANKS OF RANDOM SAMPLE INPUT VECTORS',//,' RUN NO.',
     1        4X,A,I1,A,8(6X,A,I1,A),3(5X,A,I2,A))
  860 FORMAT('0',I5,12F10.0)
  870 FORMAT('0','RANKS OF LATIN HYPERCUBE SAMPLE INPUT VECTORS',//,
     1       ' RUN NO.',4X,A,I2,A,11(5X,A,I2,A))
  875 FORMAT('0','RANKS OF RANDOM SAMPLE INPUT VECTORS',//,' RUN NO.',
     1        4X,A,I2,A,11(5X,A,I2,A))
      END
