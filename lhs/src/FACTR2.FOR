C**********************************************************************
      DOUBLE PRECISION FUNCTION FACTR2(I,N)
C
C     -- THIS FUNCT RETURNS THE LOG OF N FACTORIAL (N!) IF I=1
C     -- RETURNS LOG OF N!/I! OTHERWISE
C     -- IT RETURNS 0 FOR (N .LE. 0)
C
      DOUBLE PRECISION R,S
      SAVE R,IOLD,NOLD
      DATA R,IOLD,NOLD /0.0D0, 0, 0/
C
      IF (N .LE. 1) THEN
         R = 0.0D0
         IOLD = 0
         NOLD = 0
C
      ELSE IF (I .EQ. IOLD  .AND.  N .EQ. NOLD) THEN
         CONTINUE
C
      ELSE IF (I .EQ. IOLD  .AND.  N .GT. NOLD) THEN
         DO 100 J=MAX(2,NOLD+1), N
            S = J
            R = R + LOG(S)
 100     CONTINUE
         NOLD = N
C
      ELSE IF (I .EQ. IOLD  .AND.  N .LT. NOLD  .AND.
     1         NOLD-N .LE. 3) THEN
         DO 150 J=N+1, NOLD
            S = J
            R = R - LOG(S)
 150     CONTINUE
         NOLD=N
C
      ELSE IF (N .EQ. NOLD  .AND.  I .GT. IOLD) THEN
         DO 200 J=IOLD+1, I
            S = J
            R = R - LOG(S)
 200     CONTINUE
         IOLD = I
C
      ELSE IF (N .EQ. NOLD  .AND.  I .LT. IOLD  .AND.
     1         IOLD-I .LE. 3) THEN
         DO 300 J=I+1, IOLD
            S = J
            R = R + LOG(S)
 300     CONTINUE
         IOLD = I
C
      ELSE
         R = 0
         IF (N .GT. 1) THEN
            DO 400 J= MAX(2,I+1), N
               S = J
               R = R + LOG(S)
 400        CONTINUE
         END IF
         NOLD = N
         IOLD = I
C
      END IF
C
      FACTR2 = R
C
      RETURN
      END
