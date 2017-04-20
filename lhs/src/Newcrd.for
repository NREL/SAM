C     Last change:  S    29 Sep 97    1:00 pm
c
c  = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
c
      Subroutine NewCrd(Card,IUnit,IEnd)
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
c
c     Read a new card, left justify it, strip off trailing comments,
c     and convert it to upper case and convert tabs and commas to blanks
c
      Character*(*) Card
c
c     -- Read a card and make sure it's not blank
c
      IEnd = 0
 100  Read (IUnit,101,End=999) Card
 101  Format (A256)
      If (Card == ' ') Go To 100
c
c     -- Now strip off trailing comments
c
      IComt = Index(Card,'$')
      If (IComt == 1) Then
c        -- record contains only a comment - reject it and read another
         Go To 100
      Else If (IComt > 1) Then
c        -- trailing comment found - strip it off
         Card = Card(1:IComt-1)
      End If
c
c     -- Now convert the card to upper case and convert tabs (ASCII 9)
c     -- and commas to blank spaces
c
C rgk-4/09  Lahey Fortran 95 says Maxi not defined, so borrow from 10 lines
c down and define Maxi=Len(Card)
      Maxi = Len(Card)
      Do i=1, Maxi
         If ( IChar(Card(i:i)) > 96  .AND.  IChar(Card(i:i)) < 123 )
     1         Card(i:i) = Char( IChar(Card(i:i)) - 32 )
         If ( IChar(Card(i:i)) == 9 )  Card(i:i) = ' '
         If ( Card(i:i) == ',' )       Card(i:i) = ' '
      End Do
c
c     -- Left justify the card
c
      Maxi = Len(Card)
      Do i=1, Maxi
         If ( Card(i:i) /= ' ' ) Exit
      End Do
c
      Card = Card(i:Maxi)
c
      Return
c
c     -- Transfer to here and set a flag if the end of the file was reached
c
 999  IEnd = 1
      Return
c
      End
