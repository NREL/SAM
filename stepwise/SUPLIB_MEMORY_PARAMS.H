C *** suplib_MEMORY_PARAMS.INC ***
C
C     Dynamic memory error vector and flags.
c     Changes in the error flags must be accompanied by changes
c     in the error format statement.
C
      INTEGER NERVEC
      INTEGER SUCESS, NOGET, NOINIT, REINIT, NONAME,
     &   BDNAME, BADLEN, BDTYPE, DFULL, VFULL, BFULL,
     &   BDVOID, BDBLK, ILBLK,
     &   BADCOD, BADNAM, DEFRON, WRTYPE
      PARAMETER (NERVEC=18)
      PARAMETER (SUCESS=1, NOGET=2, NOINIT=3, REINIT=4, NONAME=5,
     &   BDNAME=6, BADLEN=7, BDTYPE=8, DFULL=9, VFULL=10, BFULL=11,
     &   BDVOID=12, BDBLK=13, ILBLK=14,
     &   BADCOD=15, BADNAM=16, DEFRON=17, WRTYPE=18)
