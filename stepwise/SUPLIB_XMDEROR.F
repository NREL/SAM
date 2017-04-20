      SUBROUTINE XMDEROR (UNIT, LASTER, ERRVEC, ALLCOD)
C
      IMPLICIT INTEGER (A-Z)
      INCLUDE 'SUPLIB_MEMORY_PARAMS.H'
      DIMENSION ERRVEC(NERVEC)
      LOGICAL ALLCOD
C
      CHARACTER*60 ERRMSG(NERVEC)
      DATA (ERRMSG(I), I=1,NERVEC) /
     1   'SUCCESSFUL COMPLETION$',
     2   'Unable to get requested space from system$',
     3   'Data manager not initialized$',
     4   'Data manager was previously initialized$',
     5   'Name not found in dictionary$',
     6   'Name already exists in dictionary$',
     7   'Illegal length request$',
     8   'Unknown data type$',
     9   'Dictionary is full$',
     #   'Void table is full$',
     1   'Memory block table is full$',
     2   'Overlapping voids - internal error$',
     3   'Overlapping memory blocks - internal error$',
     4   'Invalid memory block - extension library error$',
     5   'Invalid error code$',
     6   'Invalid input name$',
     7   'Illegal call while in deferred mode$',
     8   'Name is of wrong type for operation$' /
C
      IF ((LASTER .GE. 1) .AND. (LASTER .LE. NERVEC)) THEN
         L = INDEX (ERRMSG(LASTER), '$') - 1
         WRITE (UNIT, 10000) LASTER, ERRMSG(LASTER)(1:L)
      ELSE
         WRITE (UNIT, 10000) LASTER
      END IF
      WRITE (UNIT, 10010)
      DO 100 I = 1, NERVEC
         IF (ALLCOD .OR. (ERRVEC(I) .NE. 0)) THEN
            L = INDEX (ERRMSG(I), '$') - 1
            WRITE (UNIT, 10020) ERRVEC(I), I, ERRMSG(I)(1:L)
         END IF
  100 CONTINUE
      RETURN
10000  FORMAT(/' Last CAMSUPES_LIB error: code =',I3,:,', ',A)
10010  FORMAT(
     &   /' * * * * * * * * * * * E R R O R   T A B L E'
     &   ,' * * * * * * * * * * *'
     &   /' Occurances  Code   Error Type'
     &   /' ----------  ----   ',
     &   '----------------------------------------------')
10020  FORMAT (1X,I6,5X,I4,4X,A)
      END
