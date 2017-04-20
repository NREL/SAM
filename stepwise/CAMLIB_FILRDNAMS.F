C=======================================================================
      SUBROUTINE FILRDNAMS (OUTFIL, IERROR)
C=======================================================================

C   --*** FILRDNAMS *** (CAMCON_LIB) Read the files defined by FILDFNAM
C   --   Written by Amy Gilkey - revised 08/17/98
C   --
C   --FILRDNAMS requests the names of all files previously defined by FILDFNAM.
C   --The file names are read from the command line.  Any file names not
C   --found on the command line are requested from the user (if running
C   --interactively).  If the file is an input file, it must exist.
C   --
C   --Some special inputs for the file names are allowed:
C   --   CANCEL specifies that a file (which must be optional) is not needed.
C   --   INTERACTIVE specifies that the input will be entered interactively
C   --      (if allowed).
C   --   DEFAULT requests that the specified file (and all following files
C   --      if this is the last entry on the command line) should use the
C   --      default.
C   --
C   --Parameters:
C   --   OUTFIL - OUT char*(*) array - the returned file names;
C   --      ' ' for an optional file that is not needed;
C   --      'INTERACTIVE' for interactive input
C   --   IERROR - OUT integer - returned >0 if and only if the returned
C   --      file names are not correct; if running interactively, this
C   --      flag should always be 0

      IMPLICIT NONE

C   --Common Variables:
      INCLUDE 'CAMLIB_PROGRAM_COMMON.H'
      INCLUDE 'CAMLIB_FILES_COMMON.H'

C   --Parameters Definitions:
      CHARACTER*(*) OUTFIL(*)
      INTEGER IERROR

C   --Definitions for functions called:
      LOGICAL QABATCH
      INTEGER ISTRFIND
      INTEGER ISTRLEN

      LOGICAL OK, EX, HEAD
      LOGICAL XBATCH
      INTEGER I, L
      INTEGER NIN
      INTEGER IDEF
      INTEGER IFIL
      INTEGER IERR
      INTEGER IX
      INTEGER IDUM
      CHARACTER*128 FILNAM, FILUP, TXTUP, STR128
      CHARACTER*80 TMPSTR

C   --Ensure the QA common is initialized
      CALL QA0INIT

C   --Read in the command line parameters

      CALL FILCMDLIN (NUMFIL, NIN, OUTFIL)
      ICTFIL = 0

      XBATCH = QABATCH ()

C   --Set command line parameters to default if the last parameter is default
!!!
      IDEF = ISTRFIND ('DEFAULT', NIN, OUTFIL)
      IF ((IDEF .GT. 0) .AND. (IDEF .GE. NIN)) THEN
         DO 100 I = IDEF, NUMFIL
            OUTFIL(I) = 'DEFAULT'
  100    CONTINUE
         NIN = NUMFIL
      END IF

C   --Display the names of the file expected (if not all given)

      IF ((IDEF .NE. 1) .AND. (NIN .LT. NUMFIL)) THEN
         IF (PROGRM .NE. ' ') THEN
            !WRITE (*, 10000, IOSTAT=IDUM) PROGRM(:ISTRLEN(PROGRM))
         ELSE
            !WRITE (*, 10000, IOSTAT=IDUM) 'The program'
         END IF
10000     FORMAT(/, 1X, 80('*')
     &      //, ' ', A, ' expects the following files:')
         !DO 110 I = 1, NUMFIL
         !   IF (OPTTBL(I)) THEN
         !      WRITE (*, 10010, IOSTAT=IDUM)
     &   !         I, DSCTBL(I)(:ISTRLEN(DSCTBL(I)))
!10010    !       FORMAT (I5,') ', A, ' (optional)')
         !   ELSE
         !      WRITE (*, 10020, IOSTAT=IDUM)
     &   !         I, DSCTBL(I)(:ISTRLEN(DSCTBL(I)))
!10020    !       FORMAT (I5,') ', A)
         !   END IF
!  110    !CONTINUE
         WRITE (*, 10030, IOSTAT=IDUM)
10030     FORMAT (/, 1X, 80('*'))
      END IF

      IERROR = 0

      DO 130 IFIL = 1, NUMFIL

         HEAD = .TRUE.
         OK = .TRUE.

C      --Request the name of the file

  120    CONTINUE
         IF (.NOT. XBATCH) THEN
            IF ((OUTFIL(IFIL) .EQ. ' ') .OR. (.NOT. OK)) THEN
               IF (HEAD) THEN
                  HEAD = .FALSE.
                  WRITE (*, *)
                  IF (INOTBL(IFIL) .NE. 'T') THEN
                     STR128 = 'Enter the name of the ' // DSCTBL(IFIL)
                  ELSE
                     STR128 = 'Enter the ' // DSCTBL(IFIL)
                  END IF
                  TMPSTR = ' '
                  IF (INOTBL(IFIL) .NE. 'T') THEN
                     IF (OPTTBL(IFIL) .AND. (INOTBL(IFIL) .EQ. 'U'))
     &                  THEN
                        TMPSTR = '(CANCEL for no file,'
     &                     // ' INTERACTIVE for interactive input)'
                     ELSE IF (OPTTBL(IFIL)) THEN
                        TMPSTR = '(CANCEL for no file)'
                     ELSE IF (INOTBL(IFIL) .EQ. 'U') THEN
                        TMPSTR =
     &                     '(INTERACTIVE for interactive input)'
                     END IF
                  ELSE
                     IF (OPTTBL(IFIL)) THEN
                        TMPSTR = '(optional)'
                     END IF
                  END IF
                  IF (TMPSTR .NE. ' ') THEN
                     IF (ISTRLEN (STR128) + ISTRLEN (TMPSTR) .GE. 80)
     &                  THEN
                        WRITE (*, 10040, IOSTAT=IDUM)
     &                     STR128(:ISTRLEN(STR128))
                        STR128 = '   ' // TMPSTR
                     ELSE
                        STR128(ISTRLEN(STR128)+1:) = ' ' // TMPSTR
                     END IF
                  END IF
                  WRITE (*, 10040, IOSTAT=IDUM) STR128(:ISTRLEN(STR128))
               END IF
10040           FORMAT (1X, 5A)
               IF (FILTBL(IFIL) .NE. ' ') THEN
                  WRITE (STR128, 10050, IOSTAT=IDUM)
     &               FILTBL(IFIL)(:ISTRLEN(FILTBL(IFIL)))
               ELSE IF (OPTTBL(IFIL)) THEN
                  IF (INOTBL(IFIL) .NE. 'T') THEN
                     WRITE (STR128, 10050, IOSTAT=IDUM) 'no file'
                  ELSE
                     WRITE (STR128, 10050, IOSTAT=IDUM) 'no parameter'
                  END IF
               ELSE
                  WRITE (STR128, 10050, IOSTAT=IDUM) 'no default'
               END IF
10050           FORMAT (3X, '<', A, '> ')
               CALL EXREAD (STR128(:ISTRLEN(STR128)+1), FILNAM, IDUM)
               IF (FILNAM .EQ. ' ') THEN
                  FILNAM = FILTBL(IFIL)
               END IF
            ELSE
               FILNAM = OUTFIL(IFIL)
            END IF
         ELSE
            FILNAM = OUTFIL(IFIL)
         END IF

C      --Convert any logical name, and convert to uppercase

         FILUP = FILNAM
c!!!         CALL STRUPCASE (FILUP)  !#VAX only, lowercase on UNIX
         TXTUP = FILUP
         CALL STRUPCASE (TXTUP)  
         IF ((FILUP .NE. ' ') .AND. (INOTBL(IFIL) .NE. 'T')) THEN
            CALL EXLOGICAL (FILUP, STR128, L)
            IF (L .GE. 0) THEN
               FILNAM = STR128
               FILUP = STR128
c!!!               CALL STRUPCASE (FILUP)  !#VAX only, lowercase on UNIX
               TXTUP = FILUP
               CALL STRUPCASE (TXTUP)  
            END IF
            FILNAM = FILUP
         END IF

C      --Fill in DEFAULT, if requested

         IF (TXTUP .EQ. 'DEFAULT') THEN
            FILNAM = FILTBL(IFIL)
            FILUP = FILTBL(IFIL)
            TXTUP = FILUP
         END IF

         OK = .TRUE.
         IF (INOTBL(IFIL) .NE. 'T') THEN

            IF (FILUP .EQ. ' ') THEN

C            --Handle no file given

               FILUP = ' '
               TXTUP = FILUP
               FILNAM = ' '
               CALL QAMESSAG (0, 'CMDERR',
     &            'The ' // DSCTBL(IFIL)(:ISTRLEN(DSCTBL(IFIL)))
     &            // ' must be specified')
               OK = .FALSE.

            ELSE IF ((TXTUP .EQ. 'NO') .OR. (TXTUP .EQ. 'CANCEL')) THEN

C            --Handle no file given, must be optional

               FILUP = ' '
               TXTUP = FILUP
               FILNAM = ' '
               IF (.NOT. OPTTBL(IFIL)) THEN
                  CALL QAMESSAG (0, 'CMDERR',
     &               'The ' // DSCTBL(IFIL)(:ISTRLEN(DSCTBL(IFIL)))
     &               // ' is not optional')
                  OK = .FALSE.
               END IF

            ELSE IF (TXTUP .EQ. 'INTERACTIVE') THEN

C            --Handle INTERACTIVE file

               FILUP = 'INTERACTIVE'
               TXTUP = 'INTERACTIVE'
               FILNAM = 'INTERACTIVE'
               IF (INOTBL(IFIL) .NE. 'U') THEN
                  CALL QAMESSAG (0, 'CMDERR',
     &               'The ' // DSCTBL(IFIL)(:ISTRLEN(DSCTBL(IFIL)))
     &               // ' may not be interactive')
                  OK = .FALSE.
               END IF

            ELSE

C            --Handle file name - parse into default, and check if good file
C            --name and if exists (if input)

               IF (FILUP .NE. FILTBL(IFIL)) THEN
c!!!                  CALL STRUPCASE (FILNAM)  !#VAX only, lowercase on UNIX
                  CALL FILPARSE (' ', FILNAM, FILTBL(IFIL), FILNAM)
               END IF
               INQUIRE (FILE=FILNAM(:ISTRLEN(FILNAM)), NAME=STR128,
     &            IOSTAT=IERR, EXIST=EX)
               IF (IERR .NE. 0) THEN
                  CALL QAMESSAG (0, 'CMDERR', 'The '
     &               // DSCTBL(IFIL)(:ISTRLEN(DSCTBL(IFIL))) // ' "'
     &               // FILNAM(:ISTRLEN(FILNAM)) // '" is invalid')
                  OK = .FALSE.
               ELSE IF ((.NOT. EX) .AND.
     &            ((INOTBL(IFIL) .EQ. 'I') .OR.
     &            (INOTBL(IFIL) .EQ. 'U'))) THEN
                  CALL QAMESSAG (0, 'CMDERR', 'The '
     &               // DSCTBL(IFIL)(:ISTRLEN(DSCTBL(IFIL))) // ' "'
     &               // STR128(:ISTRLEN(STR128)) // '" does not exist')
                  OK = .FALSE.
               END IF
c!!!               IF (INOTBL(IFIL) .EQ. 'O') THEN      !#VAX only
c!!!                  IX = INDEX (STR128, ';')          !#VAX only
c!!!                  IF (IX .GT. 0) STR128(IX:) = ' '  !#VAX only
c!!!               END IF                               !#VAX only
               FILNAM = STR128
            END IF
         END IF

         IF ((.NOT. OK) .AND. (.NOT. XBATCH)) GOTO 120
         IF (.NOT. OK) IERROR = IERROR + 1

         OUTFIL(IFIL) = FILNAM
         FILTBL(IFIL) = FILNAM
  130 CONTINUE

      RETURN
      END
