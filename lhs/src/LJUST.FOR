c
c  = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
c
      Subroutine LJust(Card)
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
c
c     Left justify the character variable
c
      Character*(*) Card
c
c     -- Left justify the card
c
      Maxi = Len(Card)
      Do i=1, Maxi
         If ( Card(i:i) /= ' ' ) Exit
      End Do
c
      If (i > Maxi) i = maxi
      Card = Card(i:Maxi)
c
      Return
      End
