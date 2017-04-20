C     Last change:  GDW  13 Oct 98   11:37 am
C****************************************************************
C
      SUBROUTINE TABLE(FUNCT, XTABLE, ISMAX, ISIZE)
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
      DIMENSION XTABLE(ISMAX,2)
      EXTERNAL FUNCT
      SAVE
C
      DELTA1 = 1.0/(625.0*ISIZE)
      Delta2 = Delta1
      DELTAMN = 1.0 / ISIZE
c
c     -- If the table gets too large, return to this point and
c     -- start the table generation process over.
c
 10   Continue
      I = 1
      XTABLE(I,1) = 0.0
      XTABLE(I,2) = 0.0
      X = 0.0
      Q = 0.0
      FOFX=0.0
      DELTAX = DELTA2
      DELTAQ = DELTA2
c
      Do While (Q < 1.0  .AND.  X < 1.0)
c
c        -- Select a step size DeltaQ so that the maximum step size is
c        -- taken near the center of the distribution (DeltaQ = 1/ISIZE)
c        -- and progressively smaller steps are taken in the tails of the
c        -- distribution.  In no case, let the step size become less than
c        -- the value implied by ISIZE, called DELTAMN.
c
         If (Q > 1.0-8.0*DELTA1) Then
            DELTAQ = MIN(DELTA2, DELTAMN)
         Else If (Q > 1.0-38.0*DELTA1) Then
            DELTAQ = MIN(5.0*DELTA2, DELTAMN)
         Else If (Q > 1.0-185.0*DELTA1) Then
            DELTAQ = MIN(25.0*DELTA2, DELTAMN)
         Else If (Q > 1.0-800.0*DELTA1) Then
            DELTAQ = MIN(125.0*DELTA2, DELTAMN)
         Else If (Q > 625.0*DELTA1) Then
            DELTAQ = MIN(625.0*DELTA2, DELTAMN)
         Else If (Q > 125.0*DELTA1) Then
            DELTAQ = MIN(125.0*DELTA2, DELTAMN)
         Else If (Q > 25.0*DELTA1) Then
            DELTAQ = MIN(25.0*DELTA2, DELTAMN)
         Else If (Q > 5.0*DELTA1) Then
            DELTAQ = MIN(5.0*DELTA2, DELTAMN)
         Else
            DeltaQ = MIN(Delta2, DELTAMN)
         End If
c
c        -- Q is the maximum value of FOFX that will be accepted
c        -- XNEW is the estimate for a value of X that will produce
c        -- an acceptable Q
c
         Q = FOFX + DELTAQ
         Q = MIN(Q,1.0)
         XNEW = X + DELTAX
         XNEW = MIN(XNEW,1.0)
         IChStp = 0
C
         CALL FUNCT(XNEW,FOFX)
         If(KLLERR) Return
c
c        -- If the step was too large, try again with a smaller step size.
c        -- If the step size gets too small then simply accept the best we
c        -- can do and truck on.
         Do While (FOFX > Q)
            IChStp = -1
            DELTAX = 0.6666667 * DELTAX
            IF ( X+DeltaX == X  .OR.  DeltaX < 1.0E-12 ) Then
               DeltaX = 4.0 * DeltaX
               Exit
            End If
            XNEW = X + DELTAX
            CALL FUNCT(XNEW,FOFX)
            If(KLLERR) Return
         End Do
c
c        -- If the step could have been much larger and still produced an
c        -- acceptable Q, then select a larger DeltaX for the next iteration
         If (FOFX < (Q-0.5*DELTAQ)  .AND.  IChStp >= 0) Then
            DELTAX = 1.5 * DELTAX
            DELTAX = MIN(DELTAX, (1.0-X) )
         END IF
c
c        -- add the value to the table and prepare for another step
         I=I+1
         IF ( I >= ISMAX-2 ) THEN
c           -- Table is too big, so adjust the minimum step size
c           -- to be larger (to make a smaller table) and go back
c           -- to the top and start over.
            Delta2 = Delta2 * 2.0
            GOTO 10
         END IF
         XTABLE(I,1)=XNEW
         XTABLE(I,2)=FOFX
         X = XNEW
c
      End Do
c
c     -- Adjust the last value to assure that the table will not have
c     -- duplicate values in the second column
      XTABLE(I,1) = 0.05*XTABLE(I-1,1) + 0.95*XTABLE(I,1)
      XTABLE(I,2) = 0.05*XTABLE(I-1,2) + 0.95*XTABLE(I,2)
c
c     -- Add the last value to the table and exit
      I=I+1
      XTABLE(I,1)=1.0
      XTABLE(I,2)=1.0
      ISIZE=I
C
      RETURN
      END
