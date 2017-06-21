C=======================================================================
      SUBROUTINE TRNICLOSE (IUNITI, IERRTRN)
C=======================================================================

C   --*** TRNICLOSE *** (TRN_LIB) Close input transfer file
C   --   Written by Amy Gilkey - revised 03/21/92
C   --
C   --TRNICLOSE closes an input transfer file.
C   --
C   --Parameters:
C   --   IUNITI - IN integer - the input transfer file unit number
C   --   IERRTRN - OUT integer - error flag:
C   --      #-10 - TRN_LIB bug - {message} (call code sponsor)
C   --      #-20 - Undefined input transfer file unit number

      IMPLICIT NONE

C!!!!!INCLUDE 'TRN_COMMON.INC'
      INTEGER MAXTRN
      PARAMETER (MAXTRN = 10)
      INTEGER JIUNIT(MAXTRN)
      INTEGER JIVERSTRN(MAXTRN)
      INTEGER JNUMVAR(MAXTRN), JNUMVEC(MAXTRN), JNUMSTEP(MAXTRN)
      INTEGER JNRECCMT(MAXTRN), JNRECNAM(MAXTRN)
      INTEGER JICURLOC(MAXTRN), JICURVEC(MAXTRN), JICURSTEP(MAXTRN)
      COMMON /TRNCOM/ JIUNIT, JIVERSTRN,
     &   JNUMVAR, JNUMVEC, JNUMSTEP,
     &   JNRECCMT, JNRECNAM,
     &   JICURLOC, JICURVEC, JICURSTEP
      INTEGER IERRBUG, IERRUOVR, IERRUNIT, IERRPAR, IERRUNK
      PARAMETER (IERRBUG=-10, IERRUOVR=-12, IERRUNIT=-20, IERRPAR=-21,
     &   IERRUNK=-999)
      INTEGER IUNITERR
      INTEGER IDEBUG
      INTEGER IXTRNCALL
      LOGICAL SETCALL
      CHARACTER*12 RTNCALL
      COMMON /ERRXTRNCOM/ IUNITERR, IDEBUG, IXTRNCALL, SETCALL
      COMMON /ERRXTRNCOMc/ RTNCALL
C!!!!!End of TRN_COMMON.INC

      INTEGER IUNITI
      INTEGER IERRTRN

      INTEGER IXTRNI
      INTEGER IDUM

      IERRTRN = 0

C   --Look up the input transfer file in the TRN_LIB common
      CALL XTRNIXTRN ('TRNICLOSE', IUNITI, IXTRNI, IERRTRN)
      IF (IERRTRN .NE. 0) RETURN

C   --Close the transfer file
      CLOSE (IUNITI, IOSTAT=IDUM)

C   --Delete memory reserved for this transfer file

C   --Clear out common entry for this transfer file
      CALL XTRNIXCLR (IXTRNI)

      RETURN
      END

C=======================================================================
      SUBROUTINE TRNICMT (IUNITI, MAXCMT, NUMCMT, CMTLIN, IERRTRN)
C=======================================================================

C   --*** TRNICMT *** (TRN_LIB) Read transfer file comment lines
C   --   Modified by Amy Gilkey - revised 09/27/95
C   --
C   --TRNICMT reads the transfer file comment lines
C   --
C   --Parameters:
C   --   IUNITI - IN integer - the transfer file unit number
C   --   MAXCMT - IN integer - the maximum number of comment lines to be read
C   --   NUMCMT - OUT integer - the number of comment lines
C   --   CMTLIN(MAXCMT) - OUT char*(*) - the comment lines
C   --   IERRTRN - OUT integer - error flag

      IMPLICIT NONE

C!!!!!INCLUDE 'TRN_COMMON.INC'
      INTEGER MAXTRN
      PARAMETER (MAXTRN = 10)
      INTEGER JIUNIT(MAXTRN)
      INTEGER JIVERSTRN(MAXTRN)
      INTEGER JNUMVAR(MAXTRN), JNUMVEC(MAXTRN), JNUMSTEP(MAXTRN)
      INTEGER JNRECCMT(MAXTRN), JNRECNAM(MAXTRN)
      INTEGER JICURLOC(MAXTRN), JICURVEC(MAXTRN), JICURSTEP(MAXTRN)
      COMMON /TRNCOM/ JIUNIT, JIVERSTRN,
     &   JNUMVAR, JNUMVEC, JNUMSTEP,
     &   JNRECCMT, JNRECNAM,
     &   JICURLOC, JICURVEC, JICURSTEP
      INTEGER IERRBUG, IERRUOVR, IERRUNIT, IERRPAR, IERRUNK
      PARAMETER (IERRBUG=-10, IERRUOVR=-12, IERRUNIT=-20, IERRPAR=-21,
     &   IERRUNK=-999)
      INTEGER IUNITERR
      INTEGER IDEBUG
      INTEGER IXTRNCALL
      LOGICAL SETCALL
      CHARACTER*12 RTNCALL
      COMMON /ERRXTRNCOM/ IUNITERR, IDEBUG, IXTRNCALL, SETCALL
      COMMON /ERRXTRNCOMc/ RTNCALL
C!!!!!End of TRN_COMMON.INC

      INTEGER IUNITI
      INTEGER MAXCMT
      INTEGER NUMCMT
      CHARACTER*(*) CMTLIN(MAXCMT)
      INTEGER IERRTRN

      CHARACTER*80 LINE

      INTEGER IXTRNI
      INTEGER ICMT

      IERRTRN = 0

C   --Look up the input transfer file in the TRN_LIB common
      CALL XTRNIXTRN ('TRNICMT', IUNITI, IXTRNI, IERRTRN)
      IF (IERRTRN .NE. 0) RETURN

C   --Fill in values from common
      NUMCMT = JNRECCMT(IXTRNI)
C   --NUMCMT will be less than zero if it has not been set
      IF (NUMCMT .EQ. 0) RETURN

C   --Move the transfer file to the proper location
      CALL XTRNMOVHD (IUNITI, IXTRNI, 'COMMENT', IERRTRN)
      IF (IERRTRN .NE. 0) RETURN

C   --Read the comment lines (count the records)

C   --Comment records start with "!"; before version 4, assume no comment
      IF (JNRECCMT(IXTRNI) .LT. 0) THEN
         JNRECCMT(IXTRNI) = 0
         NUMCMT = JNRECCMT(IXTRNI)
  100    CONTINUE
         READ (IUNITI, '(A)', ERR=120, END=120, IOSTAT=IERRTRN)
     &      LINE
         IF (LINE(1:1) .EQ. '!') THEN
            JNRECCMT(IXTRNI) = JNRECCMT(IXTRNI) + 1
            NUMCMT = JNRECCMT(IXTRNI)
            IF (NUMCMT .LE. MAXCMT) THEN
               CMTLIN(NUMCMT) = LINE(2:)
            END IF
            GOTO 100
         END IF
         if ((jiverstrn(ixtrni) .lt. 5)
     &      .and. (jnreccmt(ixtrni) .eq. 0)) then
            JNRECCMT(IXTRNI) = JNRECCMT(IXTRNI) + 1
            NUMCMT = JNRECCMT(IXTRNI)
            IF (NUMCMT .LE. MAXCMT) THEN
               CMTLIN(NUMCMT) = LINE
            END IF
            goto 100
         end if
         BACKSPACE (IUNITI, ERR=120, IOSTAT=IERRTRN)
      ELSE
         DO 110 ICMT = 1, JNRECCMT(IXTRNI)
            READ (IUNITI, '(A)', ERR=120, END=120, IOSTAT=IERRTRN)
     &         LINE
            IF (ICMT .LE. MAXCMT) THEN
               IF (LINE(1:1) .EQ. '!') THEN
                  CMTLIN(ICMT) = LINE(2:)
               ELSE
                  CMTLIN(ICMT) = LINE(1:)
               END IF
            END IF
  110    CONTINUE
      END IF
      JICURLOC(IXTRNI) = JICURLOC(IXTRNI) + 1

      RETURN

  120 CONTINUE
C   --Error encountered in reading TRN file
      CALL XTRNERR ('Reading transfer file comment lines', IERRTRN)
      RETURN
      END

C=======================================================================
      SUBROUTINE TRNINAMES (IUNITI, NUMVAR, NAMES, IERRTRN)
C=======================================================================

C   --*** TRNINAMES *** (TRN_LIB) Read transfer file names
C   --   Modified by Amy Gilkey - revised 06/05/95
C   --
C   --TRNINAMES reads the transfer file variable names.
C   --
C   --Parameters:
C   --   IUNITI - IN - the transfer file unit number
C   --   NUMVAR - OUT - the number of variables
C   --   NAMES - OUT - the variable names
C   --   IERRTRN - OUT integer - error flag

      IMPLICIT NONE

C!!!!!INCLUDE 'TRN_COMMON.INC'
      INTEGER MAXTRN
      PARAMETER (MAXTRN = 10)
      INTEGER JIUNIT(MAXTRN)
      INTEGER JIVERSTRN(MAXTRN)
      INTEGER JNUMVAR(MAXTRN), JNUMVEC(MAXTRN), JNUMSTEP(MAXTRN)
      INTEGER JNRECCMT(MAXTRN), JNRECNAM(MAXTRN)
      INTEGER JICURLOC(MAXTRN), JICURVEC(MAXTRN), JICURSTEP(MAXTRN)
      COMMON /TRNCOM/ JIUNIT, JIVERSTRN,
     &   JNUMVAR, JNUMVEC, JNUMSTEP,
     &   JNRECCMT, JNRECNAM,
     &   JICURLOC, JICURVEC, JICURSTEP
      INTEGER IERRBUG, IERRUOVR, IERRUNIT, IERRPAR, IERRUNK
      PARAMETER (IERRBUG=-10, IERRUOVR=-12, IERRUNIT=-20, IERRPAR=-21,
     &   IERRUNK=-999)
      INTEGER IUNITERR
      INTEGER IDEBUG
      INTEGER IXTRNCALL
      LOGICAL SETCALL
      CHARACTER*12 RTNCALL
      COMMON /ERRXTRNCOM/ IUNITERR, IDEBUG, IXTRNCALL, SETCALL
      COMMON /ERRXTRNCOMc/ RTNCALL
C!!!!!End of TRN_COMMON.INC

      INTEGER IUNITI
      INTEGER NUMVAR
      CHARACTER*8 NAMES(NUMVAR)
      INTEGER IERRTRN

      INTEGER MAXFLD
      PARAMETER (MAXFLD = 20)
      INTEGER INTYP(MAXFLD)
      CHARACTER*8 CFIELD(MAXFLD)
      INTEGER IFIELD(MAXFLD)
      REAL RFIELD(MAXFLD)

      INTEGER IXTRNI
      INTEGER IVAR
      INTEGER NUMFLD
      INTEGER I

      IERRTRN = 0

C   --Look up the input transfer file in the TRN_LIB common
      CALL XTRNIXTRN ('TRNINAMES', IUNITI, IXTRNI, IERRTRN)
      IF (IERRTRN .NE. 0) RETURN

C   --Fill in values from common
      NUMVAR = JNUMVAR(IXTRNI)
      IF (NUMVAR .LE. 0) RETURN

C   --Move the transfer file to the proper location
      CALL XTRNMOVHD (IUNITI, IXTRNI, 'NAMES', IERRTRN)
      IF (IERRTRN .NE. 0) RETURN

C   --Read the variable names (count the records)

      JNRECNAM(IXTRNI) = 0
      IVAR = 0
  100 CONTINUE
      IF (IVAR .LT. NUMVAR) THEN
         JNRECNAM(IXTRNI) = JNRECNAM(IXTRNI) + 1
         CALL FFRDFLDS (IUNITI, 0, '>', MAXFLD, IERRTRN, NUMFLD,
     &      INTYP, CFIELD, IFIELD, RFIELD)
         IF (IERRTRN .NE. 0) GOTO 120
         DO 110 I = 1, MIN (NUMFLD, NUMVAR-IVAR)
            IVAR = IVAR + 1
            NAMES(IVAR) = CFIELD(I)
  110    CONTINUE
         GOTO 100
      END IF
      JICURLOC(IXTRNI) = JICURLOC(IXTRNI) + 1

      RETURN

  120 CONTINUE
C   --Error encountered in reading transfer file
      CALL XTRNERR (
     &   'Reading transfer file variable names', IERRTRN)
      RETURN
      END

C=======================================================================
      SUBROUTINE TRNIOPEN (IUNITI, FILE, IERRTRN)
C=======================================================================

C   --*** TRNIOPEN *** (TRN_LIB) Open input transfer file
C   --   Written by Amy Gilkey - revised 09/27/95
C   --
C   --TRNIOPEN opens an input transfer file and scans for sizing information.
C   --This routine MUST be called before any other TRNI routine.
C   --
C   --Parameters:
C   --   IUNITI - IN integer - the input transfer file unit number
C   --   FILE - IN char*(*) - the input transfer file name
C   --   IERRTRN - OUT integer - error flag:
C   --      #-10 - TRN_LIB bug - {message} (call code sponsor)
C   --      #-11 - Too much dynamic memory requested
C   --      #-12 - Too many transfer files defined
C   --      #-20 - Unit already opened
C   --      #i/o - Cannot open file {file}
C   --      #i/o - Invalid transfer file

      IMPLICIT NONE

C!!!!!INCLUDE 'TRN_COMMON.INC'
      INTEGER MAXTRN
      PARAMETER (MAXTRN = 10)
      INTEGER JIUNIT(MAXTRN)
      INTEGER JIVERSTRN(MAXTRN)
      INTEGER JNUMVAR(MAXTRN), JNUMVEC(MAXTRN), JNUMSTEP(MAXTRN)
      INTEGER JNRECCMT(MAXTRN), JNRECNAM(MAXTRN)
      INTEGER JICURLOC(MAXTRN), JICURVEC(MAXTRN), JICURSTEP(MAXTRN)
      COMMON /TRNCOM/ JIUNIT, JIVERSTRN,
     &   JNUMVAR, JNUMVEC, JNUMSTEP,
     &   JNRECCMT, JNRECNAM,
     &   JICURLOC, JICURVEC, JICURSTEP
      INTEGER IERRBUG, IERRUOVR, IERRUNIT, IERRPAR, IERRUNK
      PARAMETER (IERRBUG=-10, IERRUOVR=-12, IERRUNIT=-20, IERRPAR=-21,
     &   IERRUNK=-999)
      INTEGER IUNITERR
      INTEGER IDEBUG
      INTEGER IXTRNCALL
      LOGICAL SETCALL
      CHARACTER*12 RTNCALL
      COMMON /ERRXTRNCOM/ IUNITERR, IDEBUG, IXTRNCALL, SETCALL
      COMMON /ERRXTRNCOMc/ RTNCALL
C!!!!!End of TRN_COMMON.INC

      INTEGER IUNITI
      CHARACTER*(*) FILE
      INTEGER IERRTRN

      INTEGER ISTRLEN

      INTEGER IXTRNI
      INTEGER IDUM

      IERRTRN = 0

C   --Initialize the TRN_LIB common for the transfer file
      CALL XTRNIXNEW ('TRNIOPEN', IUNITI, IXTRNI, IERRTRN)
      IF (IERRTRN .NE. 0) RETURN
      JIUNIT(IXTRNI) = IUNITI

C   --Open the transfer file
      CALL FILOPEN (IUNITI,
     &   'IN', 'FORM', FILE(:ISTRLEN(FILE)), IERRTRN)
      IF (IERRTRN .NE. 0) GOTO 100

C   --Scan the transfer file for sizing information (stored in common)

      CALL XTRNMOVHD (IUNITI, IXTRNI, 'VERSION', IERRTRN)
      IF (IERRTRN .NE. 0) GOTO 130
      READ (IUNITI, *, ERR=110, END=110, IOSTAT=IERRTRN)
     &   JIVERSTRN(IXTRNI)
      JICURLOC(IXTRNI) = JICURLOC(IXTRNI) + 1

      CALL XTRNMOVHD (IUNITI, IXTRNI, 'SIZES', IERRTRN)
      IF (IERRTRN .NE. 0) GOTO 130
      READ (IUNITI, *, ERR=120, END=120, IOSTAT=IERRTRN)
     &   JNUMVAR(IXTRNI), JNUMVEC(IXTRNI), JNUMSTEP(IXTRNI)
      JICURLOC(IXTRNI) = JICURLOC(IXTRNI) + 1

      IF (JNUMVAR(IXTRNI) .LE. 0) THEN
         CALL XTRNERR ('There are no transfer file variables', 0)
         IERRTRN = IERRPAR
      END IF
      IF (JNUMSTEP(IXTRNI) .LE. 0) THEN
         CALL XTRNERR ('There are no transfer file step intervals', 0)
         IERRTRN = IERRPAR
      END IF
      IF (JNUMVEC(IXTRNI) .LE. 0) THEN
         CALL XTRNERR ('There are no transfer file vectors', 0)
         IERRTRN = IERRPAR
      END IF
      IF (IERRTRN .NE. 0) GOTO 130

      RETURN

C   --Transfer file open error
  100 CONTINUE
      CALL XTRNERR ('Cannot open file ' // FILE, IERRTRN)
      GOTO 130
C   --Error encountered in reading transfer file
  110 CONTINUE
      CALL XTRNERR ('Reading transfer file version number', IERRTRN)
      GOTO 130
  120 CONTINUE
      CALL XTRNERR ('Reading transfer file size information', IERRTRN)
      GOTO 130
  130 CONTINUE
C   --Error - clear entry
      IF (IXTRNI .GT. 0) THEN
         CALL XTRNIXCLR (IXTRNI)
         CLOSE (IUNITI, IOSTAT=IDUM)
      END IF
      RETURN
      END

C=======================================================================
      SUBROUTINE TRNISIZES (IUNITI, NUMVAR, NUMSTEP, NUMVEC, IERRTRN)
C=======================================================================

C   --*** TRNISIZES *** (TRN_LIB) Read transfer file header
C   --   Modified by Amy Gilkey - revised 06/27/93
C   --
C   --TRNISIZES reads the transfer file header, including the number of
C   --variables, steps, and vectors.
C   --
C   --Parameters:
C   --   IUNITI - IN - the transfer file unit number
C   --   NUMVAR - OUT - the number of variables
C   --   NUMSTEP - OUT - the number of steps
C   --   NUMVEC - OUT - the number of vectors
C   --   IERRTRN - OUT integer - error flag

      IMPLICIT NONE

C!!!!!INCLUDE 'TRN_COMMON.INC'
      INTEGER MAXTRN
      PARAMETER (MAXTRN = 10)
      INTEGER JIUNIT(MAXTRN)
      INTEGER JIVERSTRN(MAXTRN)
      INTEGER JNUMVAR(MAXTRN), JNUMVEC(MAXTRN), JNUMSTEP(MAXTRN)
      INTEGER JNRECCMT(MAXTRN), JNRECNAM(MAXTRN)
      INTEGER JICURLOC(MAXTRN), JICURVEC(MAXTRN), JICURSTEP(MAXTRN)
      COMMON /TRNCOM/ JIUNIT, JIVERSTRN,
     &   JNUMVAR, JNUMVEC, JNUMSTEP,
     &   JNRECCMT, JNRECNAM,
     &   JICURLOC, JICURVEC, JICURSTEP
      INTEGER IERRBUG, IERRUOVR, IERRUNIT, IERRPAR, IERRUNK
      PARAMETER (IERRBUG=-10, IERRUOVR=-12, IERRUNIT=-20, IERRPAR=-21,
     &   IERRUNK=-999)
      INTEGER IUNITERR
      INTEGER IDEBUG
      INTEGER IXTRNCALL
      LOGICAL SETCALL
      CHARACTER*12 RTNCALL
      COMMON /ERRXTRNCOM/ IUNITERR, IDEBUG, IXTRNCALL, SETCALL
      COMMON /ERRXTRNCOMc/ RTNCALL
C!!!!!End of TRN_COMMON.INC

      INTEGER IUNITI
      INTEGER NUMVAR
      INTEGER NUMSTEP
      INTEGER NUMVEC
      INTEGER IERRTRN

      INTEGER IXTRNI

      IERRTRN = 0

C   --Look up the input transfer file in the TRN_LIB common
      CALL XTRNIXTRN ('TRNISIZES', IUNITI, IXTRNI, IERRTRN)
      IF (IERRTRN .NE. 0) RETURN

C   --Fill in values from common
      NUMVAR = JNUMVAR(IXTRNI)
      NUMSTEP = JNUMSTEP(IXTRNI)
      NUMVEC = JNUMVEC(IXTRNI)

      RETURN
      END

C=======================================================================
      SUBROUTINE TRNITIMES (IUNITI, NUMSTEP, TIMES, IERRTRN)
C=======================================================================

C   --*** TRNITIMES *** (TRN_LIB) Read transfer file step intervals
C   --   Modified by Amy Gilkey - revised 07/06/93
C   --
C   --TRNITIMES reads the transfer file step intervals.
C   --
C   --Parameters:
C   --   IUNITI - IN - the transfer file unit number
C   --   NUMSTEP - OUT - the number of steps
C   --   TIMES - OUT - the step intervals
C   --   IERRTRN - OUT integer - error flag

      IMPLICIT NONE

C!!!!!INCLUDE 'TRN_COMMON.INC'
      INTEGER MAXTRN
      PARAMETER (MAXTRN = 10)
      INTEGER JIUNIT(MAXTRN)
      INTEGER JIVERSTRN(MAXTRN)
      INTEGER JNUMVAR(MAXTRN), JNUMVEC(MAXTRN), JNUMSTEP(MAXTRN)
      INTEGER JNRECCMT(MAXTRN), JNRECNAM(MAXTRN)
      INTEGER JICURLOC(MAXTRN), JICURVEC(MAXTRN), JICURSTEP(MAXTRN)
      COMMON /TRNCOM/ JIUNIT, JIVERSTRN,
     &   JNUMVAR, JNUMVEC, JNUMSTEP,
     &   JNRECCMT, JNRECNAM,
     &   JICURLOC, JICURVEC, JICURSTEP
      INTEGER IERRBUG, IERRUOVR, IERRUNIT, IERRPAR, IERRUNK
      PARAMETER (IERRBUG=-10, IERRUOVR=-12, IERRUNIT=-20, IERRPAR=-21,
     &   IERRUNK=-999)
      INTEGER IUNITERR
      INTEGER IDEBUG
      INTEGER IXTRNCALL
      LOGICAL SETCALL
      CHARACTER*12 RTNCALL
      COMMON /ERRXTRNCOM/ IUNITERR, IDEBUG, IXTRNCALL, SETCALL
      COMMON /ERRXTRNCOMc/ RTNCALL
C!!!!!End of TRN_COMMON.INC

      INTEGER IUNITI
      INTEGER NUMSTEP
      REAL TIMES(NUMSTEP)
      INTEGER IERRTRN

      INTEGER IXTRNI
      INTEGER ISTEP

      IERRTRN = 0

C   --Look up the input transfer file in the TRN_LIB common
      CALL XTRNIXTRN ('TRNITIMES', IUNITI, IXTRNI, IERRTRN)
      IF (IERRTRN .NE. 0) RETURN

C   --Fill in values from common
      NUMSTEP = JNUMSTEP(IXTRNI)
      IF (NUMSTEP .LE. 0) RETURN

C   --Move the transfer file to the proper location
      CALL XTRNMOVHD (IUNITI, IXTRNI, 'TIMES', IERRTRN)
      IF (IERRTRN .NE. 0) RETURN

C   --Read in the step intervals

      READ (IUNITI, *, ERR=100, END=100, IOSTAT=IERRTRN)
     &   (TIMES(ISTEP), ISTEP=1,NUMSTEP)
      JICURLOC(IXTRNI) = JICURLOC(IXTRNI) + 1

      RETURN

C   --Error encountered in reading transfer file
  100 CONTINUE
      CALL XTRNERR ('Reading transfer file step intervals', IERRTRN)
      RETURN
      END

C=======================================================================
      SUBROUTINE XTRNERR (ERRMSG, IOSTAT)
C=======================================================================

C   --*** XTRNERR *** (TRN_LIB) Display an error message
C   --   Written by Amy Gilkey - revised 09/27/95
C   --
C   --XTRNERR displays an error message.
C   --
C   --Parameters:
C   --   ERRMSG - IN - the error message
C   --   IOSTAT - IN - the I/O error status; 0 if not I/O error

      IMPLICIT NONE

C!!!!!INCLUDE 'TRN_COMMON.INC'
      INTEGER MAXTRN
      PARAMETER (MAXTRN = 10)
      INTEGER JIUNIT(MAXTRN)
      INTEGER JIVERSTRN(MAXTRN)
      INTEGER JNUMVAR(MAXTRN), JNUMVEC(MAXTRN), JNUMSTEP(MAXTRN)
      INTEGER JNRECCMT(MAXTRN), JNRECNAM(MAXTRN)
      INTEGER JICURLOC(MAXTRN), JICURVEC(MAXTRN), JICURSTEP(MAXTRN)
      COMMON /TRNCOM/ JIUNIT, JIVERSTRN,
     &   JNUMVAR, JNUMVEC, JNUMSTEP,
     &   JNRECCMT, JNRECNAM,
     &   JICURLOC, JICURVEC, JICURSTEP
      INTEGER IERRBUG, IERRUOVR, IERRUNIT, IERRPAR, IERRUNK
      PARAMETER (IERRBUG=-10, IERRUOVR=-12, IERRUNIT=-20, IERRPAR=-21,
     &   IERRUNK=-999)
      INTEGER IUNITERR
      INTEGER IDEBUG
      INTEGER IXTRNCALL
      LOGICAL SETCALL
      CHARACTER*12 RTNCALL
      COMMON /ERRXTRNCOM/ IUNITERR, IDEBUG, IXTRNCALL, SETCALL
      COMMON /ERRXTRNCOMc/ RTNCALL
C!!!!!End of TRN_COMMON.INC

      CHARACTER*(*) ERRMSG
      INTEGER IOSTAT

      INTEGER IQAERRUNI
      INTEGER ISTRLEN

      INTEGER IDUM
      CHARACTER*80 IOMSG

C   --Initialize the calling routine
      DATA RTNCALL  / '        ' /
      DATA IXTRNCALL / -999 /
      DATA SETCALL  / .TRUE. /

C   --Initialize the debug flag
      data idebug / 0 /

C   --Define the error unit from the QA error unit
      IUNITERR = IQAERRUNI (-1)

C   --Do not display message if unit number is zero
      IF (IUNITERR .EQ. 0) RETURN

C   --Write the error message including the calling routine name
      IF (IUNITERR .GT. 0) THEN
         WRITE (IUNITERR, 10000, IOSTAT=IDUM)
     &      RTNCALL(:ISTRLEN(RTNCALL)), ERRMSG(:ISTRLEN(ERRMSG))
      ELSE
         WRITE (*, 10000, IOSTAT=IDUM)
     &      RTNCALL(:ISTRLEN(RTNCALL)), ERRMSG(:ISTRLEN(ERRMSG))
      END IF

      IF (IOSTAT .NE. 0) THEN

C      --Write the I/O error
         IF (IOSTAT .LT. 0) THEN
            WRITE (IOMSG, 10010, IOSTAT=IDUM) IOSTAT,
     &         'Unexpected end of file'
         ELSE IF (IOSTAT .EQ. 67) THEN
            WRITE (IOMSG, 10010, IOSTAT=IDUM) IOSTAT,
     &         'Input record is too short'
         ELSE
            WRITE (IOMSG, 10010, IOSTAT=IDUM) IOSTAT
         END IF
         IF (IUNITERR .GT. 0) THEN
            WRITE (IUNITERR, 10020, IOSTAT=IDUM)
     &         IOMSG(:ISTRLEN(IOMSG))
         ELSE
            WRITE (*, 10020, IOSTAT=IDUM)
     &         IOMSG(:ISTRLEN(IOMSG))
         END IF
      END IF

      RETURN
10000  FORMAT (/, ' %%% TRN_LIB ERROR calling ', A, ' - ', A)
10010  FORMAT ('FORTRAN I/O Error #', I3, :, ' - ', A)
10020  FORMAT (5X, A)
      END

C=======================================================================
      SUBROUTINE XTRNIXCLR (IXTRN)
C=======================================================================

C   --*** XTRNIXCLR *** (TRN_LIB) Clear TRN_LIB common entry
C   --   Written by Amy Gilkey - revised 06/25/92
C   --
C   --XTRNIXCLR initializes the given transfer file entry in TRN_LIB
C   --common.  All values are initialized to -999
C   --
C   --Parameters:
C   --   IXTRN - IN - the transfer file TRN_LIB common index

      IMPLICIT NONE

C!!!!!INCLUDE 'TRN_COMMON.INC'
      INTEGER MAXTRN
      PARAMETER (MAXTRN = 10)
      INTEGER JIUNIT(MAXTRN)
      INTEGER JIVERSTRN(MAXTRN)
      INTEGER JNUMVAR(MAXTRN), JNUMVEC(MAXTRN), JNUMSTEP(MAXTRN)
      INTEGER JNRECCMT(MAXTRN), JNRECNAM(MAXTRN)
      INTEGER JICURLOC(MAXTRN), JICURVEC(MAXTRN), JICURSTEP(MAXTRN)
      COMMON /TRNCOM/ JIUNIT, JIVERSTRN,
     &   JNUMVAR, JNUMVEC, JNUMSTEP,
     &   JNRECCMT, JNRECNAM,
     &   JICURLOC, JICURVEC, JICURSTEP
      INTEGER IERRBUG, IERRUOVR, IERRUNIT, IERRPAR, IERRUNK
      PARAMETER (IERRBUG=-10, IERRUOVR=-12, IERRUNIT=-20, IERRPAR=-21,
     &   IERRUNK=-999)
      INTEGER IUNITERR
      INTEGER IDEBUG
      INTEGER IXTRNCALL
      LOGICAL SETCALL
      CHARACTER*12 RTNCALL
      COMMON /ERRXTRNCOM/ IUNITERR, IDEBUG, IXTRNCALL, SETCALL
      COMMON /ERRXTRNCOMc/ RTNCALL
C!!!!!End of TRN_COMMON.INC

      INTEGER IXTRN

      JIUNIT(IXTRN) = -999
      JIVERSTRN(IXTRN) = -999
      JNUMVAR(IXTRN) = -999
      JNUMSTEP(IXTRN) = -999
      JNUMVEC(IXTRN) = -999
      JNRECCMT(IXTRN) = -999
      JNRECNAM(IXTRN) = -999
      JICURLOC(IXTRN) = -999
      JICURVEC(IXTRN) = -999
      JICURSTEP(IXTRN) = -999

      RETURN
      END

C=======================================================================
      SUBROUTINE XTRNIXNEW (CALLER, IUNIT, IXTRN, IERRTRN)
C=======================================================================

C   --*** XTRNIXNEW *** (TRN_LIB) Get new TRN_LIB common entry
C   --   Written by Amy Gilkey - revised 02/21/93
C   --
C   --XTRNIXNEW returns the index of the a new TRN_LIB common entry.
C   --
C   --Parameters:
C   --   CALLER - IN - the calling user routine
C   --   IUNIT - IN - the transfer file unit number
C   --   IXTRN - OUT - the transfer file TRN_LIB common index
C   --   IERRTRN - OUT - error flag:
C   --      #-12 - Too many transfer files defined
C   --      #-20 - Unit already opened

      IMPLICIT NONE

C!!!!!INCLUDE 'TRN_COMMON.INC'
      INTEGER MAXTRN
      PARAMETER (MAXTRN = 10)
      INTEGER JIUNIT(MAXTRN)
      INTEGER JIVERSTRN(MAXTRN)
      INTEGER JNUMVAR(MAXTRN), JNUMVEC(MAXTRN), JNUMSTEP(MAXTRN)
      INTEGER JNRECCMT(MAXTRN), JNRECNAM(MAXTRN)
      INTEGER JICURLOC(MAXTRN), JICURVEC(MAXTRN), JICURSTEP(MAXTRN)
      COMMON /TRNCOM/ JIUNIT, JIVERSTRN,
     &   JNUMVAR, JNUMVEC, JNUMSTEP,
     &   JNRECCMT, JNRECNAM,
     &   JICURLOC, JICURVEC, JICURSTEP
      INTEGER IERRBUG, IERRUOVR, IERRUNIT, IERRPAR, IERRUNK
      PARAMETER (IERRBUG=-10, IERRUOVR=-12, IERRUNIT=-20, IERRPAR=-21,
     &   IERRUNK=-999)
      INTEGER IUNITERR
      INTEGER IDEBUG
      INTEGER IXTRNCALL
      LOGICAL SETCALL
      CHARACTER*12 RTNCALL
      COMMON /ERRXTRNCOM/ IUNITERR, IDEBUG, IXTRNCALL, SETCALL
      COMMON /ERRXTRNCOMc/ RTNCALL
C!!!!!End of TRN_COMMON.INC

      CHARACTER*(*) CALLER
      INTEGER IUNIT, IXTRN
      INTEGER IERRTRN

      INTEGER I
      integer idum

C   --Initialize the transfer file unit numbers
      DATA JIUNIT / MAXTRN * -999 /

      IERRTRN = 0

C   --Set the routine name
      IF (SETCALL) THEN
         RTNCALL = CALLER
         IXTRNCALL = -999
      END IF

C   --Check that unit is not already defined
      DO 100 I = 1, MAXTRN
         IF (IUNIT .EQ. JIUNIT(I)) GOTO 130
  100 CONTINUE

C   --Find empty entry
      DO 110 I = 1, MAXTRN
         IF (JIUNIT(I) .LT. 0) THEN
            IXTRN = I
            GOTO 120
         END IF
  110 CONTINUE

      IXTRN = -999
      GOTO 140

  120 CONTINUE

C   --Initialize new entry
      CALL XTRNIXCLR (IXTRN)

C   --Set the routine index
      IF (SETCALL) THEN
         IXTRNCALL = IXTRN
         if (idebug .ne. 0)
     &      write (*, '(1x, a, a, 3x, a, i2)', iostat=idum)
     &      'Call ', RTNCALL, 'index =', IXTRNCALL
      END IF

      RETURN

  130 CONTINUE
C   --Unit already defined
      IERRTRN = IERRUNIT
      CALL XTRNERR ('Unit already opened', 0)
      RETURN
  140 CONTINUE
C   --Too many transfer files defined
      IERRTRN = IERRUOVR
      CALL XTRNERR ('Too many transfer files defined', 0)
      RETURN
      END

C=======================================================================
      SUBROUTINE XTRNIXTRN (CALLER, IUNIT, IXTRN, IERRTRN)
C=======================================================================

C   --*** XTRNIXTRN *** (TRN_LIB) Get TRN_LIB common entry
C   --   Written by Amy Gilkey - revised 05/08/95
C   --
C   --XTRNIXTRN returns the index of the TRN_LIB common transfer
C   --file entry whose unit number matches the given unit number.
C   --
C   --Parameters:
C   --   CALLER - IN - the calling user routine
C   --   IUNIT - IN - the transfer file unit number
C   --   IXTRN - OUT - the transfer file TRN_LIB common index
C   --   IERRTRN - OUT - error flag:
C   --      #-20 - Undefined transfer file unit number

      IMPLICIT NONE

C!!!!!INCLUDE 'TRN_COMMON.INC'
      INTEGER MAXTRN
      PARAMETER (MAXTRN = 10)
      INTEGER JIUNIT(MAXTRN)
      INTEGER JIVERSTRN(MAXTRN)
      INTEGER JNUMVAR(MAXTRN), JNUMVEC(MAXTRN), JNUMSTEP(MAXTRN)
      INTEGER JNRECCMT(MAXTRN), JNRECNAM(MAXTRN)
      INTEGER JICURLOC(MAXTRN), JICURVEC(MAXTRN), JICURSTEP(MAXTRN)
      COMMON /TRNCOM/ JIUNIT, JIVERSTRN,
     &   JNUMVAR, JNUMVEC, JNUMSTEP,
     &   JNRECCMT, JNRECNAM,
     &   JICURLOC, JICURVEC, JICURSTEP
      INTEGER IERRBUG, IERRUOVR, IERRUNIT, IERRPAR, IERRUNK
      PARAMETER (IERRBUG=-10, IERRUOVR=-12, IERRUNIT=-20, IERRPAR=-21,
     &   IERRUNK=-999)
      INTEGER IUNITERR
      INTEGER IDEBUG
      INTEGER IXTRNCALL
      LOGICAL SETCALL
      CHARACTER*12 RTNCALL
      COMMON /ERRXTRNCOM/ IUNITERR, IDEBUG, IXTRNCALL, SETCALL
      COMMON /ERRXTRNCOMc/ RTNCALL
C!!!!!End of TRN_COMMON.INC

      CHARACTER*(*) CALLER
      INTEGER IUNIT, IXTRN
      INTEGER IERRTRN

      INTEGER I
      integer idum

C   --Initialize the transfer file unit numbers
      DATA JIUNIT / MAXTRN * -999 /

      IERRTRN = 0

C   --Set the routine name
      IF (SETCALL) THEN
         RTNCALL = CALLER
         IXTRNCALL = -999
      END IF

C   --Find transfer file entry whose unit matches given unit
      IF (IUNIT .GE. 0) THEN
         DO 100 I = 1, MAXTRN
            IF (IUNIT .EQ. JIUNIT(I)) THEN
               IXTRN = I
               GOTO 110
            END IF
  100    CONTINUE
      END IF

      IXTRN = -999
      GOTO 120

  110 CONTINUE
C   --Set the routine index
      IF (SETCALL) THEN
         IXTRNCALL = IXTRN
         if (idebug .ne. 0)
     &      write (*, '(1x, a, a, 3x, a, i2)', iostat=idum)
     &      'Call ', RTNCALL, 'index =', IXTRNCALL
      END IF

      RETURN

  120 CONTINUE
C   --Transfer file entry not found
      IERRTRN = IERRUNIT
      CALL XTRNERR ('Undefined transfer file unit number', 0)
      RETURN
      END

C=======================================================================
      SUBROUTINE XTRNMOVHD (IUNITI, IXTRNI, TYPE, IERRTRN)
C=======================================================================

C   --*** XTRNMOVHD *** (TRN_LIB) Move input transfer file to header item
C   --   Written by Amy Gilkey - revised 07/07/93
C   --
C   --XTRNMOVHD moves an input transfer file to the specified header item.
C   --
C   --Parameters:
C   --   IUNITI - IN - the input transfer file file unit number
C   --   IXTRNI - IN - the input transfer file TRN_LIB common index
C   --   TYPE - IN - the type of header item
C   --   IERRTRN - OUT - error flag:
C   --      #i/o - Scanning forward/backward on input transfer file

      IMPLICIT NONE

C!!!!!INCLUDE 'TRN_COMMON.INC'
      INTEGER MAXTRN
      PARAMETER (MAXTRN = 10)
      INTEGER JIUNIT(MAXTRN)
      INTEGER JIVERSTRN(MAXTRN)
      INTEGER JNUMVAR(MAXTRN), JNUMVEC(MAXTRN), JNUMSTEP(MAXTRN)
      INTEGER JNRECCMT(MAXTRN), JNRECNAM(MAXTRN)
      INTEGER JICURLOC(MAXTRN), JICURVEC(MAXTRN), JICURSTEP(MAXTRN)
      COMMON /TRNCOM/ JIUNIT, JIVERSTRN,
     &   JNUMVAR, JNUMVEC, JNUMSTEP,
     &   JNRECCMT, JNRECNAM,
     &   JICURLOC, JICURVEC, JICURSTEP
      INTEGER IERRBUG, IERRUOVR, IERRUNIT, IERRPAR, IERRUNK
      PARAMETER (IERRBUG=-10, IERRUOVR=-12, IERRUNIT=-20, IERRPAR=-21,
     &   IERRUNK=-999)
      INTEGER IUNITERR
      INTEGER IDEBUG
      INTEGER IXTRNCALL
      LOGICAL SETCALL
      CHARACTER*12 RTNCALL
      COMMON /ERRXTRNCOM/ IUNITERR, IDEBUG, IXTRNCALL, SETCALL
      COMMON /ERRXTRNCOMc/ RTNCALL
C!!!!!End of TRN_COMMON.INC

      INTEGER IUNITI, IXTRNI
      CHARACTER*(*) TYPE
      INTEGER IERRTRN

      INTEGER MAXFLD
      PARAMETER (MAXFLD = 20)
      INTEGER INTYP(MAXFLD)
      CHARACTER*8 CFIELD(MAXFLD)
      INTEGER IFIELD(MAXFLD)
      REAL RFIELD(MAXFLD)

      INTEGER NUMVAR
      INTEGER NUMSTEP
      INTEGER NUMVEC
      INTEGER INEWLOC
      INTEGER IREC
      INTEGER NUMFLD
      INTEGER IDUM1, IDUM2, IDUM3
      INTEGER IVAR
      INTEGER ISTEP
      REAL RDUM

      IERRTRN = 0

C   --Fill in values from common
      NUMVAR = JNUMVAR(IXTRNI)
      NUMSTEP = JNUMSTEP(IXTRNI)
      NUMVEC = JNUMVEC(IXTRNI)

      IF (TYPE .EQ. 'REWIND') THEN
         INEWLOC = 0
      ELSE IF (TYPE .EQ. 'VERSION') THEN
         INEWLOC = 1
      ELSE IF (TYPE .EQ. 'COMMENT') THEN
         INEWLOC = 2
      ELSE IF (TYPE .EQ. 'SIZES') THEN
         INEWLOC = 3
      ELSE IF (TYPE .EQ. 'NAMES') THEN
         INEWLOC = 4
      ELSE IF (TYPE .EQ. 'TIMES') THEN
         INEWLOC = 5
      ELSE IF (TYPE .EQ. 'VALUES') THEN
         INEWLOC = 999
      END IF

      IF ((JICURVEC(IXTRNI) .GE. 0)
     &   .AND. (JICURSTEP(IXTRNI) .GE. 0)) THEN
         JICURLOC(IXTRNI) = 999
      END IF

      IF (JICURLOC(IXTRNI) .GT. INEWLOC) THEN
         REWIND (IUNITI)
         JICURLOC(IXTRNI) = 0
         JICURVEC(IXTRNI) = -999
         JICURSTEP(IXTRNI) = -999
      END IF

      IF (JICURLOC(IXTRNI) .LT. 1) JICURLOC(IXTRNI) = 1

      IF (JICURLOC(IXTRNI) .LE. MIN (1, INEWLOC-1)) THEN
         READ (IUNITI, *, ERR=140, END=140, IOSTAT=IERRTRN) IDUM1
         JICURLOC(IXTRNI) = JICURLOC(IXTRNI) + 1
      END IF

      IF (JICURLOC(IXTRNI) .LE. MIN (2, INEWLOC-1)) THEN
         IF (JNRECCMT(IXTRNI) .LT. 0) THEN
            JNRECCMT(IXTRNI) = 0
  100       CONTINUE
            READ (IUNITI, '(A)', ERR=140, END=140, IOSTAT=IERRTRN)
     &         CFIELD(1)
            IF (CFIELD(1)(1:1) .EQ. '!') THEN
               JNRECCMT(IXTRNI) = JNRECCMT(IXTRNI) + 1
               GOTO 100
            END IF
            if ((jiverstrn(ixtrni) .lt. 5)
     &         .and. (jnreccmt(ixtrni) .eq. 0)) then
               jnreccmt(ixtrni) = jnreccmt(ixtrni) + 1
               goto 100
            end if
            BACKSPACE (IUNITI, ERR=140, IOSTAT=IERRTRN)
         ELSE
            DO 110 IREC = 1, JNRECCMT(IXTRNI)
               READ (IUNITI, *, ERR=140, END=140, IOSTAT=IERRTRN)
  110       CONTINUE
         END IF
         JICURLOC(IXTRNI) = JICURLOC(IXTRNI) + 1
      END IF

      IF (JICURLOC(IXTRNI) .LE. MIN (3, INEWLOC-1)) THEN
         READ (IUNITI, *, ERR=140, END=140, IOSTAT=IERRTRN)
     &      IDUM1, IDUM2, IDUM3
         JICURLOC(IXTRNI) = JICURLOC(IXTRNI) + 1
      END IF

      IF (JICURLOC(IXTRNI) .LE. MIN (4, INEWLOC-1)) THEN
         IF (JNRECNAM(IXTRNI) .LT. 0) THEN
            JNRECNAM(IXTRNI) = 0
            IVAR = 0
  120       CONTINUE
            IF (IVAR .LT. NUMVAR) THEN
               JNRECNAM(IXTRNI) = JNRECNAM(IXTRNI) + 1
               CALL FFRDFLDS (IUNITI, 0, '>', MAXFLD, IERRTRN, NUMFLD,
     &            INTYP, CFIELD, IFIELD, RFIELD)
               IF (IERRTRN .NE. 0) GOTO 140
               IVAR = IVAR + NUMFLD
               GOTO 120
            END IF
         ELSE
            DO 130 IREC = 1, JNRECNAM(IXTRNI)
               READ (IUNITI, *, ERR=140, END=140, IOSTAT=IERRTRN)
  130       CONTINUE
         END IF
         JICURLOC(IXTRNI) = JICURLOC(IXTRNI) + 1
      END IF

      IF (JICURLOC(IXTRNI) .LE. MIN (5, INEWLOC-1)) THEN
         READ (IUNITI, *, ERR=140, END=140, IOSTAT=IERRTRN)
     &      (RDUM, ISTEP=1,NUMSTEP)
         JICURLOC(IXTRNI) = JICURLOC(IXTRNI) + 1
      END IF

      RETURN

  140 CONTINUE
C   --Transfer file scanning header error
      CALL XTRNERR ('Error scanning transfer file header', IERRTRN)
      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP___TRN_LIB.FOR
C *2    10-APR-1996 10:00:38 APGILKE "Delete unused routines"
C *1     1-NOV-1995 11:24:27 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP___TRN_LIB.FOR
