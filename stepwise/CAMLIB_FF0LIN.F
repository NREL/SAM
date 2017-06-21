C=======================================================================
      SUBROUTINE FF0LIN (IDCONT, LINE, SCRLIN, ILEFT, ISTOP)
C=======================================================================

C   --*** FF0LIN *** (CAMCON_LIB internal) Process free-field reader record
C   --   Revised by Amy Gilkey - revised 01/06/96
C   --   Free-field reader taken from the SUPES library
C   --
C   --FF0LIN processes an input record (in a scratch string) to make
C   --parsing easier as follows:
C   --   o Tabs are converted to spaces.
C   --   o Continuation lines (end with '&') are identified.
C   --   o Comments (start with '!' or '&') are deleted.
C   --   o Fields enclosed in either single or double quotes are changed to '*'.
C   --   o Equal signs are changed to commas.
C   --   o Separator spaces are changed to commas.
C   --
C   --Parameters:
C   --   IDCONT - IN/OUT - the continuation flag (0=NO continuation)
C   --   LINE - OUT - the input line
C   --   SCRLIN - OUT - the processed input line
C   --   ILEFT - OUT - the index of the first character in the input line
C   --   ISTOP - OUT - the index of the last character in the processed line

C   --Common Variables:
C   --   Sets SAVLIN of /FREEFIELD_COMMON/
C   --   Uses CMTCH, CONTCH of /FREEFIELD_COMMON/

      IMPLICIT NONE

C   --Common Variables:
      INCLUDE 'CAMLIB_FREEFIELD_COMMON.H'

C   --Parameter Definitions:
      INTEGER IDCONT
      CHARACTER*(*) LINE
      CHARACTER*(*) SCRLIN
      INTEGER ILEFT, ISTOP

      LOGICAL NONSEP, SPCSEP
      LOGICAL DELIM
      INTEGER I
      INTEGER IX
      INTEGER IXEND
      INTEGER IXSAV
      INTEGER ITAB
      CHARACTER CH

      CHARACTER TAB, QUOCH, DQUOCH
      PARAMETER (TAB = CHAR (9))
      PARAMETER (QUOCH = '''', DQUOCH = '"')

************************************************************************

C   --Set saved line index -
      IF (IDCONT .LE. 0) THEN
         IXSAV = 1
         SAVLIN = ' '
      ELSE
         IXSAV = MAX (1, INDEX (SAVLIN, CONTCH))
      END IF

C   --Clear the continuation indicator -
      IDCONT = 0

C   --Strip leading and trailing spaces from input line; return to caller
C   --if the remaining line is null -
      CALL STRLIMIT (LINE, ILEFT, ISTOP)
      IF (ILEFT .GT. ISTOP) RETURN

C   --Set saved line -
      SAVLIN(IXSAV:) = LINE(ILEFT:)

C   --Convert any tab to a single space -
  100 CONTINUE
      ITAB = INDEX (LINE, TAB)
      IF (ITAB .GT. 0) THEN
         LINE(ITAB:ITAB) = ' '
         GOTO 100
      END IF

C   --Strip leading and trailing spaces from input line; return to caller
C   --if the remaining line is null -
      CALL STRLIMIT (LINE, ILEFT, ISTOP)
      IF (ILEFT .GT. ISTOP) RETURN

C--Create a scratch line to handle quoted strings.  Find any characters
C--in quotes and fill these characters with non-delimiter characters
C--in the scratch string to make finding delimiters easier.

C   --Copy the line to the scratch line -
      SCRLIN = LINE

C--Make all field separators a comma.  An equal sign is synomous
C--with a comma.  A series of spaces acts as a single separator
C--if is not bordered by another separator.  Two flags are used
C--in this phase to determine if a series of spaces are acting
C--as a separator:
C--   NONSEP is set if the last non-blank character was NOT a
C--      field separator.
C--   SPCSEP is set if the last character was part of a series
C--      of spaces which may act as a field separator. NONSEP must be
C--      set before SPCSEP can be set.

C   --Clear the space-separator flag (leading spaces have been stripped) -
      SPCSEP = .FALSE.
      NONSEP = .FALSE.
      DELIM = .TRUE.

      DO 130 IX = ILEFT, ISTOP
         CH = SCRLIN(IX:IX)

         IF ((CH .EQ. CMTCH) .OR. (CH .EQ. CONTCH)) THEN
C         --Set the continuation flag, if continuation -
            IF (CH .EQ. CONTCH) IDCONT = 1
C         --Blank the input line from the comment delimiter onward
            SCRLIN(IX:) = ' '
            ISTOP = IX - 1
            GOTO 140
         END IF

         IF ((CH .EQ. QUOCH) .OR. (CH .EQ. DQUOCH)) THEN
            IF (DELIM) THEN
  110          CONTINUE
C            --Find end of quoted characters -
               IXEND = INDEX (SCRLIN(IX+1:ISTOP), CH) + IX+1 - 1
               IF (IXEND .LT. IX+1) IXEND = ISTOP
C            --Fill characters with non-delimiter character -
               DO 120 I = IX, IXEND
                  SCRLIN(I:I) = '*'
  120          CONTINUE
C            --Compress two quotes (of starting type) into one quote and
C            --continue looking for ending quote -
               IF (IXEND .LT. ISTOP) THEN
                  IF (SCRLIN(IXEND+1:IXEND+1) .EQ. CH) THEN
                     SCRLIN(IXEND+1:IXEND+1) = '*'
                     GOTO 110
                  END IF
               END IF
            END IF
         END IF

         IF (CH .EQ. ',') THEN
C         --This is already a comma; just disable space-separators -
            DELIM = .TRUE.
            NONSEP = .FALSE.
         ELSE IF (CH .EQ. '=') THEN
C         --Change this equal sign to a comma, then disable space-separators -
            DELIM = .TRUE.
            SCRLIN(IX:IX) = ','
            NONSEP = .FALSE.
         ELSE IF (CH .EQ. ' ') THEN
C         --This space may be part of a space-separator if the last non-blank
C         --character was NOT a field separator -
            DELIM = .TRUE.
            SPCSEP = NONSEP
         ELSE
            DELIM = .FALSE.
            IF (SPCSEP) THEN
C            --This is the first character after a space-separator, change the
C            --last space to a comma and clear the space-separator flag -
               SCRLIN(IX-1:IX-1) = ','
               SPCSEP = .FALSE.
            END IF
C         --Since this character is not a separator, enable space-separators -
            NONSEP = .TRUE.
         END IF
  130 CONTINUE
  140 CONTINUE

C   --Strip leading and trailing spaces from effective line (the remaining
C   --line cannot be null at this point) -
      CALL STRLIMIT (SCRLIN(1:ISTOP), ILEFT, ISTOP)
C   --Set saved line index for continued line -
      IXSAV = IXSAV + (ISTOP - ILEFT + 1) + 1

      RETURN
      END
