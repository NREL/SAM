      SUBROUTINE XXUPPER( STRING,ILEFT,IRIGHT )
      IMPLICIT NONE
      CHARACTER*(*) STRING
      INTEGER ILEFT, IRIGHT
      INTEGER LS
      INTEGER I
      INTEGER K
      INTEGER N
      CHARACTER*69 ASCII
      CHARACTER*26 LOWER
      DATA ASCII /   'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 $''()*+,-./:=
     &   !"#%&;<>?@[\]^_`{|}~' /
      DATA LOWER/ 'abcdefghijklmnopqrstuvwxyz' /
C
************************************************************************
C
C     FORTRAN Extension Library - ASCII - USER INTERFACE ROUTINE
C
C     DESCRIPTION:
C     This routine converts a character string to the ASCII character
C     set.
C
C     This skeleton version converts lowercase letters to uppercase, if
C     the processor is capable of representing such characters. All
C     other characters which are not part of the ASCII character set are
C     converted to spaces.
C
C     This routine also returns the location of the first and last
C     non-blank characters.  If the string is completely blank,
C     ILEFT=LEN(STRING)+1 and IRIGHT=0 will be returned.
C
C     FORMAL PARAMETERS:
C     STRING  CHARACTER  String
C     ILEFT   INTEGER    Position of first non-blank character.
C     IRIGHT  INTEGER    Position of last non-blank character.
C
************************************************************************
C
C Get length of the string -
      LS = LEN( STRING )
C
      DO 100 I = 1 , LS
C
C If this character is not a member of the ASCII set, check to see if it
C is a lowercase letter -
         IF( STRING(I:I) .EQ. ' ' ) THEN
            CONTINUE
         ELSE IF( INDEX( ASCII , STRING(I:I) ) .EQ. 0 ) THEN
            K = INDEX( LOWER , STRING(I:I) )
            IF( K .EQ. 0 ) THEN
C Set this unrecognized character to a blank -
               STRING(I:I) = ' '
            ELSE
C Convert this lowercase letter to uppercase -
               STRING(I:I) = ASCII(K:K)
            END IF
         END IF
  100 CONTINUE
C
C Find the first non-blank character -
      DO 110 N = 1 , LS
         IF ( STRING(N:N) .NE. ' ' ) GOTO 120
  110 CONTINUE
  120 CONTINUE
      ILEFT = N
C
C Find the last non-blank character -
      DO 130 N = LS , 1 , -1
         IF ( STRING(N:N) .NE. ' ' ) GOTO 140
  130 CONTINUE
  140 CONTINUE
      IRIGHT = N
C
      END
