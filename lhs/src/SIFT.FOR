C****************************************************************
C SUBROUTINE SIFT WILL SORT A VECTOR OF DATA IN INCREASING ORDER
C
      SUBROUTINE SIFT (XV,N)
c      INCLUDE 'KILLFILE.INC'  --  include not needed 10-96
      DIMENSION XV(N)
      M=N
   10 M=M/2
      IF (M) 30,20,30
   20 RETURN
   30 K=N-M
      J=1
   40 I=J
   50 L=I+M
      IF (XV(I)-XV(L)) 70,70,60
   60 A=XV(I)
      XV(I)=XV(L)
      XV(L)=A
      I=I-M
      IF (I) 70,70,50
   70 J=J+1
      IF (J-K) 40,40,10
      END
