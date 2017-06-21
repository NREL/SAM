C     Last change:  S    17 Feb 98    1:39 pm
c
c  = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
c
      Subroutine LRead(Card, LCard, LCLen)
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
c
c     This routine takes what is in Card and packs it plus all continuation
c     lines into the LCard variable.  LCLen is returned as the position of
c     the last non-blank character in LCard
c
      Character*(*) Card
      Character*(*) LCard
c
c     -- First, set the initial conditions with LCard equal to Card as it
c     -- is passed in.
c
c      CLen = Len(Card)
      KCLen = Len(Card)
      LCard = Card
      IStart = 1
c      LCLen = CLen
      LCLen = KCLen
c
      Do
c
c        -- Pack the new addition to LCard (between IStart and LCLen) to
c        -- remove all multiple spaces
         ISpace = 0
         Do i=IStart, LCLen
            If ( LCard(i:i) == ' ' ) Then
               If ( ISpace == 0 ) Then
                  ISpace = 1
               Else
                  Cycle
               End If
            Else
               ISpace = 0
            End If
            If ( IStart /= i ) LCard(IStart:IStart) = LCard(i:i)
            IStart = IStart + 1
         End Do
         IS1 = IStart - 1
         If (LCard(IS1:IS1) == ' ') IStart = IStart - 1
c
c        -- IStart is now one greater than the last point in LCard.
c        -- Fill in the rest of LCard with blanks out to LCLen
         Do i=IStart, LCLen
            LCard(i:i) = ' '
         End Do
c
c        -- Check for a continuation character at the end of the line
         IS1 = IStart - 1
         IS2 = IStart - 2
         IF ( IS2 == 0 ) IS2 = 1         ! for strings of length 1
         If (LCard(IS2:IS1) == ' #'  .OR.  LCard(IS2:IS1) == ' %') Then
c
c           -- continuation found, so remove the continuation character,
c           -- read a new line into Card, concatenate it on the end of
c           -- LCard, reset IStart and LCLen, and re-do the packing loop
c
            LCard(IS1:IS1) = ' '
            Call NewCrd(Card, 5, IEnd)
            If(KLLERR) Return
            If ( IEnd /= 0 ) Then
               Print *, 'Unexpected end of file in the tree input file.'
               Print *, 'Continuation line not completed.'
               Write(99,*) 'Unexpected end of file in the tree ',
     1                     'input file.'
               Write(99,*) 'Continuation line not completed.'
               Write(6,*) 'Unexpected end of file in the tree ',
     1                     'input file.'
               Write(6,*) 'Continuation line not completed.'
               KLLERR = .TRUE.
               RETURN
            End If
            IStart = IS1
c            LCLen = IStart + CLen - 1
c            LCard(IStart:LCLen) = Card(1:CLen)
            LCLen = IStart + KCLen - 1
            LCard(IStart:LCLen) = Card(1:KCLen)


c
         Else
c
c           -- no continuation character found - clean up and exit the routine
c
            Exit
c
         End If
c
      End Do
c
      LCLen = IStart - 1
c
      Return
      End
c
