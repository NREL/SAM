C=======================================================================
      SUBROUTINE READUSERINPUT (IUNUSR, IUNOUT,
     &   TITLDV, NOBSIN, NUMOBS, NDROP, IXDROP,
     &   NIVIN, NDVIN, NUMVAR, LABEL, NIV, NDV, IXMODL,
     &   NDUMPVAR, IXDUMP, IWTVAR, SCATTR, ANYREG, IXDV, *, *)
C=======================================================================

C   --*** READUSERINPUT *** (STEPWISE) Read user input parameters
C   --   Modified by Amy Gilkey - revised 04/28/96
C   --
C   --READUSERINPUT reads the user input file and sets the input parameters from
C   --the input values or sets them to default values if not input.
C   --
C   --Parameters:
C   --   IUNUSR - IN - the user input file unit number
C   --   IUNOUT - IN - the listing file unit number
C   --   TITLDV - IN - the title of the dependent variables file
C   --   NOBSIN - IN - the number of input observations
C   --   NUMOBS - OUT - the number of observations to be processed (once the
C   --      dropped observations are removed)
C   --   NDROP - OUT - the number of observations to drop
C   --   IXDROP - OUT - the observations to drop
C   --   NIVIN - IN - the number of input independent variables
C   --   NDVIN - IN - the number of input dependent variables
C   --   NUMVAR - OUT - the number of input variables and transformed variables
C   --   LABEL - IN - the variable labels
C   --   NIV - IN/OUT - the number of selected independent variables
C   --   NDV - IN/OUT - the number of selected dependent variables
C   --   IXMODL - IN/OUT - the indices of the selected independent and dependent
C   --      variables
C   --   NDUMPVAR - OUT - the number of the independent and dependent variables
C   --      to be dumped
C   --   IXDUMP - OUT - the indices of the independent and dependent variables
C   --      to be dumped
C   --   IWTVAR - OUT - the variable number of the variable containing weights
C   --   SCATTR - OUT - non-blank iff scatter plots are requested;
C   --      'PARTIAL' for independent-dependent variable pairs only,
C   --      'ALL' for all variable pairs
C   --   ANYREG - OUT - true iff any regression analysis is requested
C   --   IXDV - SCRATCH - size = maximum number of dependent variables
C   --   * - return statement if user input is erroneous
C   --   * - return statement if end of file before any input

      IMPLICIT NONE

      INCLUDE 'stp_title_common.inc'
      INCLUDE 'stp_force_common.inc'
      INCLUDE 'stp_transform_common.inc'
      INCLUDE 'stp_print_options_common.inc'
      INCLUDE 'stp_plot_options_common.inc'

      INTEGER IUNUSR, IUNOUT
      CHARACTER*80 TITLDV
      INTEGER NOBSIN
      INTEGER NUMOBS
      INTEGER NDROP
      INTEGER IXDROP(*)
      INTEGER NIVIN, NDVIN
      INTEGER NUMVAR
      CHARACTER*8 LABEL(*)
      INTEGER NIV, NDV
      INTEGER IXMODL(*)
      INTEGER NDUMPVAR
      INTEGER IXDUMP(*)
      INTEGER IWTVAR
      CHARACTER*8 SCATTR
      LOGICAL ANYREG
      INTEGER IXDV(*)

      CHARACTER*(*) INDFIL, DEPFIL

      LOGICAL FFEXIST, FFNUMBER, FFMATCH, MATCHSTR
      INTEGER LOCINT
      INTEGER ISTRLEN, ISTRFIND

      INTEGER MAXFLD
      PARAMETER (MAXFLD = 100)
      INTEGER INTYP(MAXFLD+1)
      CHARACTER*80 CFIELD(MAXFLD)
      INTEGER IFIELD(MAXFLD)
      REAL RFIELD(MAXFLD)

      LOGICAL ANYINP
      LOGICAL WARN
      LOGICAL ISON
      LOGICAL CMDIV, CMDDV, CMDFRC, CMDDRP, CMDSTP, CMDOUT, CMDPLT
      INTEGER IERRCT
      INTEGER IOSTAT
      INTEGER NUMFLD
      INTEGER IFLD
      INTEGER L
      INTEGER ICONT
      INTEGER IXENTS, IXENTE
      INTEGER NENT
      INTEGER NUM, NUM2
      INTEGER IRAW
      INTEGER NERR
      INTEGER IDEV
      INTEGER IV
      INTEGER I
      INTEGER IFRC
      INTEGER IX
      INTEGER ITRN
      INTEGER IDUM
      REAL RNUM(2)
      CHARACTER*128 LINE
      CHARACTER*8 VERB, WORD
      CHARACTER*80 TRNLIN(MAXTRN)
      CHARACTER STR20(2)*20
      CHARACTER CDUM
      CHARACTER*128 FILNAM, FULNAM
      CHARACTER*128 TXTMSG

      LOGICAL FIRST
      SAVE FIRST

      CHARACTER*12 CMDTBL(26)
      SAVE CMDTBL
C      --CMDTBL - the commands table

C   --Command table follows.  Remember to change the dimensioned size when
C   --changing the table.
      DATA CMDTBL /  'SCATTER ',
     1   'TITLE   ', 'IND_FILE', 'DEP_FILE', 'TRANSFOR',
     2   'RANK    ', 'STAND01 ', 'STEPWISE', 'BACKWARD', 'PRESS   ',
     3   'WEIGHT  ', 'DROP_OBS',
     4   'OUTPUT  ', 'PLOT    ', 'XLOG    ', 'YLOG    ',
     5   'XSCALE  ', 'YSCALE  ',
     6   'QAAID   ', 'SOFTCHAR',
     7   'IND_VARS', 'DEP_VARS', 'FORCE   ',
     8   'DUMP    ', 'END     ', '        ' /

      DATA FIRST / .TRUE. /

      CALL QAPAGE (IUNOUT, 'PAGE')

C   --Set flag to check for end of file
      ANYINP = .FALSE.

      CALL QAMESSAG (0, '+', 'Reading user input parameters and data')

      IF (FIRST) THEN
         TITLE = 'DEFAULT'
         NUMTRN = 0
         CALL INIINT (1+MAXTRN, 0, IXTDEF)
         NUMVAR = NIVIN + NDVIN + NUMTRN
         DATTYP = 'RAW'
         REGTYP = ' '
         IWTVAR = 0
         NDROP = 0
         CALL INIINT (NOBSIN, 0, IXDROP)
         PRPRES = .FALSE.
         PRMEAN = .FALSE.
         PRCORR = .FALSE.
         PRSSCP = .FALSE.
         PRINV = .FALSE.
         PRSTEP = .FALSE.
         PRRESI = .FALSE.
         PRMISC = .FALSE.
         PLPRES = .FALSE.
         PLSCAT = .FALSE.
         PLRESI = .FALSE.
         IXLINR = 0
         IYLINR = 0
         XAXMIN = -999.0
         XAXMAX = -999.0
         XAXTIC = 0.0
         YAXMIN = -999.0
         YAXMAX = -999.0
         YAXTIC = 0.0
         DOAID = .TRUE.
C         CALL GRGETPAR ('SOFTCHAR', 1, SOFTCH(1), IDUM, CDUM)
C         CALL GRGETPAR ('SOFTCHAR', 2, SOFTCH(2), IDUM, CDUM)
         NIV = 0
         NDV = 0
         NUMFRC = 0
         CALL INIINT (MAXFRC, 0, IXFRC)

         FIRST = .FALSE.
      END IF

C   --Copy dependent variables into scratch array so independent and dependent
C   --variables can be treated separately
      CALL CPYINT (NDV, IXMODL(NIV+1), IXDV)

C   --Set options that must be set every data set
      DATPLT = ' '
      SCATTR = ' '
      DUMPVR = .FALSE.
      NDUMPVAR = .FALSE.

      CMDIV = .FALSE.
      CMDDV = .FALSE.
      CMDFRC = .FALSE.
      CMDDRP = .FALSE.
      CMDSTP = .FALSE.
      CMDOUT = .FALSE.
      CMDPLT = .FALSE.

      IERRCT = 0

  100 CONTINUE

C   --Read and parse one line from the input file
      CALL FFRDFLDS (IUNUSR, IUNOUT, '>', MAXFLD, IOSTAT, NUMFLD,
     &   INTYP, CFIELD, IFIELD, RFIELD)
      IF (IOSTAT .NE. 0) GOTO 230
      IF (NUMFLD .EQ. 0) GOTO 100
      INTYP(MIN(NUMFLD,MAXFLD)+1) = -999

      WARN = .FALSE.
      IFLD = 1
      CALL FFCHAR (IFLD, INTYP, CFIELD, ' ', WORD)
      CALL ABRSTR (VERB, WORD, CMDTBL)
      IF (VERB .EQ. ' ') VERB = WORD
      ANYINP = .TRUE.

      IF (VERB .EQ. 'SCATTER') THEN
         IF (FFMATCH (IFLD, INTYP, CFIELD, 'OFF', 3)) THEN
            SCATTR = ' '
         ELSE IF (FFMATCH (IFLD, INTYP, CFIELD, 'PARTIAL', 4)) THEN
            SCATTR = 'PARTIAL'
         ELSE IF (FFMATCH (IFLD, INTYP, CFIELD, 'ALL', 3)) THEN
            SCATTR = 'ALL'
         ELSE IF (FFMATCH (IFLD, INTYP, CFIELD, 'ON', 2)) THEN
            SCATTR = 'PARTIAL'
         ELSE
            SCATTR = 'PARTIAL'
         END IF
         IF (ISON) THEN
            IF (.NOT. (PLTDEV .OR. PLTTXT)) THEN
               CALL QAMESSAG (-1, 'CMDERR',
     &            'There is no plotting device or file defined')
               SCATTR = ' '
               GOTO 210
            END IF
         END IF

      ELSE IF (VERB .EQ. 'TITLE') THEN
         IF (FFMATCH (IFLD, INTYP, CFIELD, 'OFF', 3)) THEN
            TITLE = ' '
         ELSE
            ISON = FFMATCH (IFLD, INTYP, CFIELD, 'ON', 2)
            IF (FFEXIST (IFLD, INTYP)) THEN
               CALL FFCHAR (IFLD, INTYP, CFIELD, ' ', TITLE)
            END IF
         END IF

      ELSE IF ((VERB .EQ. 'IND_FILE')
     &   .OR. (VERB .EQ. 'DEP_FILE')) THEN
         CONTINUE

      ELSE IF (VERB .EQ. 'TRANSFOR') THEN
         IF (NUMTRN .GE. MAXTRN) THEN
            CALL QAMESSAG (-1, 'CMDERR',
     &         'Too many transformations')
            GOTO 210
         END IF

C      --Get the transformation and parse as an equation
         CALL FFGETLIN (LINE)
         ICONT = 0
         CALL FFPAREQN (.TRUE., LINE, MAXFLD, ICONT, NUMFLD,
     &      INTYP, CFIELD, IFIELD, RFIELD)

C      --Parse equation
         IXENTS = IXTDEF(NUMTRN) + 1
         NERR = 0
         CALL CHKEQNSYN (NUMFLD, INTYP, CFIELD, IFIELD, RFIELD,
     &      MXTDEF, IXENTS, IXENTE, NAMENT, TYPENT, INXENT, VALENT,
     &      NERR)

C      --Locate equation variables and add transformation variable
         NENT = IXENTE - IXENTS + 1
         CALL LOCEQNVAR (NUMVAR, LABEL,
     &      NENT, NAMENT(IXENTS), TYPENT(IXENTS),
     &      INXENT(IXENTS), VALENT(IXENTS), NERR)

         IF (NERR .NE. 0) GOTO 210

C      --Save transformation
         NUMTRN = NUMTRN + 1
         IXTDEF(NUMTRN) = IXENTE
         TRNLIN(NUMTRN) = LINE

      ELSE IF (VERB .EQ. 'RANK') THEN
         if (matchstr (cfield(ifld), 'REGRESSION', 4))
     &      cfield(ifld) = 'ON'
         CALL FFONOFF (IFLD, INTYP, CFIELD, ISON, *210)
         IF (ISON) THEN
            DATTYP = 'RANK'
         ELSE
            DATTYP = 'RAW'
         END IF

      ELSE IF (VERB .EQ. 'STAND01') THEN
         CALL FFONOFF (IFLD, INTYP, CFIELD, ISON, *210)
         IF (ISON) THEN
            DATTYP = 'STD01'
         ELSE
            DATTYP = 'RAW'
         END IF

      ELSE IF (VERB .EQ. 'STEPWISE') THEN
         IF (.NOT. CMDSTP) THEN
            CMDSTP = .TRUE.
            REGTYP = ' '
         END IF
         IF (REGTYP .EQ. 'BACKWARD') THEN
            CALL QAMESSAG (-1, 'CMDWARN',
     &         'STEPWISE supersedes previous BACKWARD')
            WARN = .TRUE.
         END IF
         REGTYP = 'STEPWISE'
         CALL FFREAL (IFLD, INTYP, RFIELD,
     &      'STEPWISE parameter', 0.05, RNUM(1), *210)
         CALL FFREAL (IFLD, INTYP, RFIELD,
     &      'STEPWISE parameter', RNUM(1), RNUM(2), *210)
         IF (RNUM(1) .GT. RNUM(2)) THEN
            CALL QAMESSAG (-1, 'CMDERR',
     &         'SIGIN is greater than SIGOUT')
            GOTO 210
         END IF
         SIGF(1) = RNUM(1)
         SIGF(2) = RNUM(2)

      ELSE IF (VERB .EQ. 'BACKWARD') THEN
         IF (.NOT. CMDSTP) THEN
            CMDSTP = .TRUE.
            REGTYP = ' '
         END IF
         IF (REGTYP .EQ. 'STEPWISE') THEN
            CALL QAMESSAG (-1, 'CMDWARN',
     &         'BACKWARD supersedes previous STEPWISE')
            WARN = .TRUE.
         END IF
         REGTYP = 'BACKWARD'
         CALL FFREAL (IFLD, INTYP, RFIELD,
     &      'SIG parameter', 0.05, RNUM(1), *210)
         SIGF(1) = RNUM(1)
         SIGF(2) = SIGF(1)

      ELSE IF (VERB .EQ. 'PRESS') THEN
         CALL FFONOFF (IFLD, INTYP, CFIELD, ISON, *210)
         PRPRES = ISON

      ELSE IF (VERB .EQ. 'WEIGHT') THEN
         IF (FFMATCH (IFLD, INTYP, CFIELD, 'OFF', 3)) THEN
            IWTVAR = 0
         ELSE
            CALL FFXVAR (IFLD, INTYP, CFIELD, IFIELD,
     &         NUMVAR, LABEL, IWTVAR, *210)
         END IF

      ELSE IF (VERB .EQ. 'DROP_OBS') THEN
         IF (.NOT. CMDDRP) THEN
            CMDDRP = .TRUE.
            NDROP = 0
         END IF
  110    CONTINUE
         IF (FFEXIST (IFLD, INTYP)) THEN
            CALL FFINT (IFLD, INTYP, IFIELD,
     &         'observation', 0, NUM, *210)
            IF (FFEXIST (IFLD, INTYP) .AND.
     &         (.NOT. FFNUMBER (IFLD, INTYP))) THEN
               IF (.NOT. FFMATCH (IFLD, INTYP, CFIELD, 'TO', 2)) THEN
                  CALL QAMESSAG (-1, 'CMDERR',
     &               'Expected "TO" after observation')
                  GOTO 210
               END IF
               CALL FFINT (IFLD, INTYP, IFIELD,
     &            'ending observation', 0, NUM2, *210)
               IF (NUM .GT. NUM2) THEN
                  CALL QAMESSAG (-1, 'CMDERR',
     &               'Starting observation is greater than'
     &               // ' ending observation')
                  GOTO 210
               END IF
            ELSE
               NUM2 = NUM
            END IF
            IF (NUM .LT. 1) THEN
               WRITE (TXTMSG, 10000, IOSTAT=IDUM) 'Invalid', NUM
10000           FORMAT (A, ' drop value', I5, ', ignored')
               CALL STRCMPRS (TXTMSG, IDUM)
               CALL QAMESSAG (-1, 'CMDWARN', TXTMSG)
               NUM = 1
               WARN = .TRUE.
            END IF
            IF (NUM2 .GT. NOBSIN) THEN
               WRITE (TXTMSG, 10000, IOSTAT=IDUM) 'Invalid', NUM2
               CALL STRCMPRS (TXTMSG, IDUM)
               CALL QAMESSAG (-1, 'CMDWARN', TXTMSG)
               NUM2 = NOBSIN
               WARN = .TRUE.
            END IF
            DO 120 IRAW = NUM, NUM2
               IF (LOCINT (IRAW, NDROP, IXDROP) .GT. 0) THEN
                  WRITE (TXTMSG, 10000, IOSTAT=IDUM) 'Repeated', IRAW
                  CALL STRCMPRS (TXTMSG, IDUM)
                  CALL QAMESSAG (-1, 'CMDWARN', TXTMSG)
                  WARN = .TRUE.
                  GOTO 120
               END IF
               NDROP = NDROP + 1
               IXDROP(NDROP) = IRAW
  120       CONTINUE
            GOTO 110
         END IF

      ELSE IF (VERB .EQ. 'OUTPUT') THEN
         IF (.NOT. CMDOUT) THEN
            CMDOUT = .TRUE.
            PRMEAN = .FALSE.
            PRCORR = .FALSE.
            PRSSCP = .FALSE.
            PRINV = .FALSE.
            PRSTEP = .FALSE.
            PRRESI = .FALSE.
            PRMISC = .FALSE.
         END IF
  130    CONTINUE
         IF (FFEXIST (IFLD, INTYP)) THEN
            CALL FFCHAR (IFLD, INTYP, CFIELD, ' ', WORD)
            IF (MATCHSTR (WORD, 'MEANS', 4)) THEN
               PRMEAN = .TRUE.
            ELSE IF (MATCHSTR (WORD, 'CORRELATION', 4)) THEN
               PRCORR = .TRUE.
            ELSE IF ((MATCHSTR (WORD, 'SSCP', 4))
     &         .OR. (MATCHSTR (WORD, 'SSXP', 4))) THEN
               PRSSCP = .TRUE.
            ELSE IF (MATCHSTR (WORD, 'INVERSE', 3)) THEN
               PRINV = .TRUE.
            ELSE IF (MATCHSTR (WORD, 'STEPS', 4)) THEN
               PRSTEP = .TRUE.
            ELSE IF (MATCHSTR (WORD, 'RESIDUALS', 3)) THEN
               PRRESI = .TRUE.
            ELSE IF (MATCHSTR (WORD, 'OTHER', 3)) THEN
               PRMISC = .TRUE.
            ELSE IF (MATCHSTR (WORD, 'ALL', 3)) THEN
               PRMEAN = .TRUE.
               PRCORR = .TRUE.
               PRSSCP = .TRUE.
               PRINV = .TRUE.
               PRSTEP = .TRUE.
               PRRESI = .TRUE.
               PRMISC = .TRUE.
            ELSE IF (MATCHSTR (WORD, 'OFF', 3)) THEN
               PRMEAN = .FALSE.
               PRCORR = .FALSE.
               PRSSCP = .FALSE.
               PRINV = .FALSE.
               PRSTEP = .FALSE.
               PRRESI = .FALSE.
               PRMISC = .FALSE.
            ELSE
               CALL QAMESSAG (-1, 'CMDWARN',
     &            'Invalid option "' // WORD(:ISTRLEN(WORD)) // '"')
               WARN = .TRUE.
            END IF
            GOTO 130
         END IF

      ELSE IF (VERB .EQ. 'PLOT') THEN
         IF (.NOT. CMDPLT) THEN
            CMDPLT = .TRUE.
            PLPRES = .FALSE.
            PLSCAT = .FALSE.
            PLRESI = .FALSE.
         END IF
  140    CONTINUE
         IF (FFEXIST (IFLD, INTYP)) THEN
            CALL FFCHAR (IFLD, INTYP, CFIELD, ' ', WORD)
            IF (MATCHSTR (WORD, 'RAW', 3)) THEN
               DATPLT = 'RAW'
            ELSE IF (MATCHSTR (WORD, 'PRESS', 4)) THEN
               PLPRES = .TRUE.
            ELSE IF (MATCHSTR (WORD, 'SCATTER', 4)) THEN
               PLSCAT = .TRUE.
            ELSE IF (MATCHSTR (WORD, 'RESIDUALS', 3)) THEN
               PLRESI = .TRUE.
            ELSE IF (MATCHSTR (WORD, 'ALL', 3)) THEN
               PLPRES = .TRUE.
               PLSCAT = .TRUE.
               PLRESI = .TRUE.
            ELSE IF (MATCHSTR (WORD, 'OFF', 3)) THEN
               PLPRES = .FALSE.
               PLSCAT = .FALSE.
               PLRESI = .FALSE.
            ELSE
               CALL QAMESSAG (-1, 'CMDWARN',
     &            'Invalid option "' // WORD(:ISTRLEN(WORD)) // '"')
               WARN = .TRUE.
            END IF
            GOTO 140
         END IF

         IF (PLPRES .OR. PLSCAT .OR. PLRESI) THEN
            IF (.NOT. (PLTDEV .OR. PLTTXT)) THEN
               PLPRES = .FALSE.
               PLSCAT = .FALSE.
               PLRESI = .FALSE.
               CALL QAMESSAG (-1, 'CMDERR',
     &            'There is no plotting device or file defined')
               GOTO 210
            END IF
         END IF

      ELSE IF (VERB .EQ. 'XLOG') THEN
         CALL FFCHAR (IFLD, INTYP, CFIELD, 'ON', WORD)
         IF (MATCHSTR (WORD, 'ON', 2)
     &      .OR. MATCHSTR (WORD, 'LOGARITHM', 3)) THEN
            IXLINR = 1
         ELSE IF (MATCHSTR (WORD, 'OFF', 3)
     &      .OR. MATCHSTR (WORD, 'LINEAR', 3)) THEN
            IXLINR = 0
         ELSE IF (MATCHSTR (WORD, 'ARCHYPERBOLIC', 3)) THEN
            IXLINR = 2
         END IF

      ELSE IF (VERB .EQ. 'YLOG') THEN
         CALL FFCHAR (IFLD, INTYP, CFIELD, 'ON', WORD)
         IF (MATCHSTR (WORD, 'ON', 2)
     &      .OR. MATCHSTR (WORD, 'LOGARITHM', 3)) THEN
            IYLINR = 1
         ELSE IF (MATCHSTR (WORD, 'OFF', 3)
     &      .OR. MATCHSTR (WORD, 'LINEAR', 3)) THEN
            IYLINR = 0
         ELSE IF (MATCHSTR (WORD, 'ARCHYPERBOLIC', 3)) THEN
            IYLINR = 2
         END IF

      ELSE IF (VERB .EQ. 'XSCALE') THEN
         CALL FFREAL (IFLD, INTYP, RFIELD,
     &      'X minimum', -999.0, XAXMIN, *210)
         CALL FFREAL (IFLD, INTYP, RFIELD,
     &      'X maximum', -999.0, XAXMAX, *210)
         CALL FFREAL (IFLD, INTYP, RFIELD,
     &      'X tick interval', 0.0, XAXTIC, *210)

      ELSE IF (VERB .EQ. 'YSCALE') THEN
         CALL FFREAL (IFLD, INTYP, RFIELD,
     &      'Y minimum', -999.0, YAXMIN, *210)
         CALL FFREAL (IFLD, INTYP, RFIELD,
     &      'Y maximum', -999.0, YAXMAX, *210)
         CALL FFREAL (IFLD, INTYP, RFIELD,
     &      'Y tick interval', 0.0, YAXTIC, *210)

      ELSE IF (VERB .EQ. 'QAAID') THEN
         IF (FFMATCH (IFLD, INTYP, CFIELD, 'OFF', 3)) THEN
            DOAID = .FALSE.
         ELSE
            DOAID = .TRUE.
            ISON = FFMATCH (IFLD, INTYP, CFIELD, 'ON', 2)
            IF (FFEXIST (IFLD, INTYP)) THEN
               CALL FFCHAR (IFLD, INTYP, CFIELD, ' ', QAAID)
            END IF
         END IF

      ELSE IF (VERB .EQ. 'SOFTCHAR') THEN
         CALL FFONOFF (IFLD, INTYP, CFIELD, ISON, *210)
         CALL FFCHAR (IFLD, INTYP, CFIELD, 'ALL', WORD)
         IF (MATCHSTR (WORD, 'TERMINAL', 1)) THEN
            IDEV = 1
         ELSE IF (MATCHSTR (WORD, 'HARDCOPY', 1)) THEN
            IDEV = 2
         ELSE IF (MATCHSTR (WORD, 'ALL', 1)) THEN
            IDEV = -1
         ELSE
            CALL QAMESSAG (-1, 'CMDERR',
     &         'Expected "TERMINAL", "HARDCOPY" or "ALL"')
            GOTO 210
         END IF
         IF (IDEV .NE. 2) SOFTCH(1) = ISON
         IF (IDEV .NE. 1) SOFTCH(2) = ISON

      ELSE IF (VERB .EQ. 'IND_VARS') THEN
         IF (.NOT. CMDIV) THEN
            CMDIV = .TRUE.
            NIV = 0
         END IF
  150    CONTINUE
         IF (FFEXIST (IFLD, INTYP)) THEN
            CALL FFXVAR (IFLD, INTYP, CFIELD, IFIELD,
     &         NUMVAR, LABEL, IV, *210)
            IF (LOCINT (IV, NDV, IXDV) .GT. 0) THEN
               CALL QAMESSAG (-1, 'CMDWARN',
     &            'Independent variable ' // LABEL(IV)
     &            // ' is already a dependent variable')
               WARN = .TRUE.
               GOTO 160
            END IF
            IF (LOCINT (IV, NIV, IXMODL) .GT. 0) THEN
               CALL QAMESSAG (-1, 'CMDWARN',
     &            'Independent variable ' // LABEL(IV)
     &            // ' is already defined')
               WARN = .TRUE.
               GOTO 160
            END IF
            NIV = NIV + 1
            IXMODL(NIV) = IV
  160       CONTINUE
            GOTO 150
         END IF

      ELSE IF (VERB .EQ. 'DEP_VARS') THEN
         IF (.NOT. CMDDV) THEN
            CMDDV = .TRUE.
            NDV = 0
         END IF
  170    CONTINUE
         IF (FFEXIST (IFLD, INTYP)) THEN
            CALL FFXVAR (IFLD, INTYP, CFIELD, IFIELD,
     &         NUMVAR, LABEL, IV, *210)
            IF (LOCINT (IV, NIV, IXMODL) .GT. 0) THEN
               CALL QAMESSAG (-1, 'CMDWARN',
     &            'Dependent variable ' // LABEL(IV)
     &            // ' is already an independent variable')
               WARN = .TRUE.
               GOTO 180
            END IF
            IF (LOCINT (IV, NDV, IXDV) .GT. 0) THEN
               CALL QAMESSAG (-1, 'CMDWARN',
     &            'Dependent variable ' // LABEL(IV)
     &            // ' is already defined')
               WARN = .TRUE.
               GOTO 180
            END IF
            NDV = NDV + 1
            IXDV(NDV) = IV
  180       CONTINUE
            GOTO 170
         END IF

      ELSE IF (VERB .EQ. 'FORCE') THEN
         IF (.NOT. CMDFRC) THEN
            CMDFRC = .TRUE.
            NUMFRC = 0
         END IF
  190    CONTINUE
         IF (FFEXIST (IFLD, INTYP)) THEN
            IF (NUMFRC .GE. MAXFRC) THEN
               CALL QAMESSAG (-1, 'CMDERR',
     &            'Too many force variables')
               GOTO 210
            END IF
            CALL FFXVAR (IFLD, INTYP, CFIELD, IFIELD,
     &         NUMVAR, LABEL, IV, *210)
            NUMFRC = NUMFRC + 1
            IXFRC(NUMFRC) = IV
            GOTO 190
         END IF

      ELSE IF (VERB .EQ. 'DUMP') THEN
         DUMPVR = .TRUE.
  200    CONTINUE
         IF (FFEXIST (IFLD, INTYP)) THEN
            CALL FFCHAR (IFLD, INTYP, CFIELD, ' ', WORD)
            IV = ISTRFIND (WORD, NUMVAR, LABEL)
            IF (IV .GT. 0) THEN
               NDUMPVAR = NDUMPVAR + 1
               IXDUMP(NDUMPVAR) = IV
            ELSE
               CALL QAMESSAG (-1, 'CMDERR',
     &            'Variable "' // WORD(:ISTRLEN(WORD))
     &            // '" is not defined')
            END IF
            GOTO 200
         END IF

      ELSE IF (VERB .EQ. 'END') THEN
         GOTO 230

      ELSE
         CALL QAMESSAG (-1, 'CMDERR',
     &      'Invalid keyword - ignored')
         GOTO 210
      END IF

      IF (WARN) GOTO 220

      GOTO 100

  210 CONTINUE
      IERRCT = IERRCT + 1
  220 CONTINUE
      CALL FFGETLIN (LINE)
      CALL QAMESSAG (-1, ' ', LINE(:ISTRLEN(LINE)))
      GOTO 100

  230 CONTINUE
C   --Check for end of file (with no input)
      IF (.NOT. ANYINP) GOTO 320

C   --Set title
      IF (TITLE .EQ. 'DEFAULT') TITLE = TITLDV

      NUMOBS = NOBSIN - NDROP

      IF (DATPLT .EQ. ' ') DATPLT = DATTYP

      IF (.NOT. PRPRES) PLPRES = .FALSE.

C   --Default selected independent and dependent variables
      IF (NIV .LE. 0) THEN
         NIV = NIVIN
         DO 240 I = 1, NIV
            IXMODL(I) = I
  240    CONTINUE
      END IF
      IF (NDV .LE. 0) THEN
         NDV = NDVIN
         DO 250 I = 1, NDV
            IXDV(I) = NIVIN + I
  250    CONTINUE
      END IF

C   --Make model variables = independent variables and dependent variables
      CALL CPYINT (NDV, IXDV, IXMODL(NIV+1))

C   --Dump selected variables if none chosen
      IF (DUMPVR) THEN
         IF (NDUMPVAR .LE. 0) THEN
            DO 260 IV = 1, NIV + NDV
               IXDUMP(IV) = IXMODL(IV)
  260       CONTINUE
         END IF
      END IF

C   --Process forced variables
      DO 270 IFRC = 1, NUMFRC
         IX = LOCINT (IXFRC(IFRC), NIV, IXMODL)
         IF (IX .LE. 0) THEN
            CALL QAMESSAG (-1, '+CMDERR',
     &         'Force variable ' // LABEL(IXFRC(IFRC))
     &         // ' is not an independent variable'
     &         // ' - all force variables ignored')
            CALL INIINT (NUMFRC, 0, IXFRC)
            NUMFRC = 0
            GOTO 280
         END IF
         IF (REGTYP .EQ. 'STEPWISE') IXFRC(IFRC) = IX
  270 CONTINUE
  280 CONTINUE

C   --Straighten out scatter and regression analysis flags

      IF ((SCATTR .EQ. ' ') .OR. (SCATTR .EQ. '*')) THEN
         ANYREG = .TRUE.
      ELSE
         ANYREG = CMDSTP
      END IF

C   --Echo input

      IF (TITLE .NE. ' ') THEN
         WRITE (IUNOUT, *)
         WRITE (IUNOUT, 10070, IOSTAT=IDUM)
     &      TITLE(:ISTRLEN(TITLE))
      END IF

      WRITE (IUNOUT, *)
      WRITE (IUNOUT, 10010, IOSTAT=IDUM)
     &   'Number of input Independent Variables', NIVIN
      WRITE (IUNOUT, 10010, IOSTAT=IDUM)
     &   'Number of input Dependent Variables', NDVIN
      IF (NUMTRN .GT. 0) THEN
         WRITE (IUNOUT, 10010, IOSTAT=IDUM)
     &      'Number of Transformed Variables =', NUMTRN
         DO 290 ITRN = 1, NUMTRN
            WRITE (IUNOUT, 10070, IOSTAT=IDUM)
     &         '   ' // TRNLIN(ITRN)(:ISTRLEN(TRNLIN(ITRN)))
  290    CONTINUE
      END IF

      WRITE (IUNOUT, *)
      WRITE (IUNOUT, 10010, IOSTAT=IDUM)
     &   'Number of observations on data files =', NOBSIN
10010  FORMAT (1X, A, I5)
      WRITE (IUNOUT, 10010, IOSTAT=IDUM)
     &   'Number of observations to DROP =', NDROP
      IF (NDROP .GT. 0) THEN
         WRITE (IUNOUT, 10020, IOSTAT=IDUM) (IXDROP(I), I=1,NDROP)
10020     FORMAT (4X, 15I5)
      END IF
      WRITE (IUNOUT, 10010, IOSTAT=IDUM)
     &   'Number of observations to process =', NUMOBS

      IF (SCATTR .NE. ' ') THEN
         WRITE (IUNOUT, *)
         IF (SCATTR .EQ. 'ALL') THEN
            WRITE (IUNOUT, 10070, IOSTAT=IDUM)
     &         'Plot SCATTER plots of all variable combinations'
         ELSE
            WRITE (IUNOUT, 10070, IOSTAT=IDUM)
     &         'Plot SCATTER plots of dependent variable,'
     &         // ' independent variable pairs'
         END IF
         IF (DATPLT .EQ. 'RAW') THEN
            WRITE (IUNOUT, 10070, IOSTAT=IDUM)
     &         '   using RAW data'
         ELSE IF (DATPLT .EQ. 'RANK') THEN
            WRITE (IUNOUT, 10070, IOSTAT=IDUM)
     &         '   using RANKS of data'
         ELSE IF (DATPLT .EQ. 'STD01') THEN
            WRITE (IUNOUT, 10070, IOSTAT=IDUM)
     &         '   using STANDARD 0-1 data'
         END IF
      END IF

      IF (ANYREG) THEN
         WRITE (IUNOUT, *)
         IF (DATTYP .EQ. 'RAW') THEN
            WRITE (IUNOUT, 10070, IOSTAT=IDUM)
     &         'Do regression analysis on RAW data'
         ELSE IF (DATTYP .EQ. 'RANK') THEN
            WRITE (IUNOUT, 10070, IOSTAT=IDUM)
     &         'Do regression analysis on RANKS of data'
         ELSE IF (DATTYP .EQ. 'STD01') THEN
            WRITE (IUNOUT, 10070, IOSTAT=IDUM)
     &         'Do regression analysis on STANDARD 0-1 data'
         END IF

         IF (REGTYP .EQ. 'STEPWISE') THEN
            WRITE (IUNOUT, 10070, IOSTAT=IDUM)
     &         'Use STEPWISE procedure'
     &         // ' for "best" subset selection'
            RNUM(1) = SIGF(1)
            RNUM(2) = SIGF(2)
            CALL REALSTR (2, 4, RNUM, STR20, L)
            WRITE (IUNOUT, 10030, IOSTAT=IDUM)
     &         STR20(1)(:L), STR20(2)(:L)
10030        FORMAT (4X, 'Significance level: ', A, :, ' and ', A)
         END IF
         IF (REGTYP .EQ. 'BACKWARD') THEN
            WRITE (IUNOUT, 10070, IOSTAT=IDUM)
     &         'Use BACKWARD ELIMINATION solution'
     &         // ' for "best" subset selection'
            RNUM(1) = SIGF(1)
            CALL REALSTR (1, 4, RNUM, STR20, L)
            WRITE (IUNOUT, 10030, IOSTAT=IDUM) STR20(1)(:L)
         END IF

         IF (PRPRES) THEN
            WRITE (IUNOUT, 10070, IOSTAT=IDUM)
     &         'Use Predicted Error Sum of Squares (PRESS)'
     &         // ' to protect against overfit'
         END IF

         IF (IWTVAR .GT. 0) THEN
            WRITE (IUNOUT, 10070, IOSTAT=IDUM)
     &         'Perform a weighted regression analysis'
            WRITE (IUNOUT, 10070, IOSTAT=IDUM)
     &         '   Weights are in variable '
     &         // LABEL(IWTVAR)(:ISTRLEN(LABEL(IWTVAR)))
         END IF

         WRITE (IUNOUT, *)
         WRITE (IUNOUT, 10070, IOSTAT=IDUM)
     &      'WRITE the following information:'
         IF (PRMEAN) THEN
            WRITE (IUNOUT, 10080, IOSTAT=IDUM)
     &         'MEAN, standard deviations for all variables in model'
         END IF
         IF (PRCORR) THEN
            WRITE (IUNOUT, 10080, IOSTAT=IDUM)
     &         'Simple CORRELATIONS among all variables in model'
         END IF
         IF (PRSSCP) THEN
            WRITE (IUNOUT, 10080, IOSTAT=IDUM)
     &         'Corrected sum of squares and cross products matrix',
     &         '(SSCP) for all variables in model'
         END IF
         IF (PRINV) THEN
            IF (PRSTEP) THEN
               WRITE (IUNOUT, 10080, IOSTAT=IDUM)
     &            'INVERSE correlation matrix at each analysis step',
     &            'for all independent variables'
            ELSE
               WRITE (IUNOUT, 10080, IOSTAT=IDUM)
     &            'INVERSE correlation matrix for final model',
     &            'for all independent variables'
            END IF
         END IF
         IF (PRSTEP) THEN
            WRITE (IUNOUT, 10080, IOSTAT=IDUM)
     &         'Analysis of variance table and regression coefficient'
     &         // ' estimates', 'for each STEP of variable selection'
         ELSE
            WRITE (IUNOUT, 10080, IOSTAT=IDUM)
     &         'Analysis of variance table and regression coefficient'
     &         // ' estimates',
     &         'for final model of each dependent variable'
         END IF
         IF (PRMISC) THEN
            WRITE (IUNOUT, 10080, IOSTAT=IDUM)
     &         'OTHER information for regression analysis'
         END IF
         IF (PRRESI) THEN
            WRITE (IUNOUT, 10080, IOSTAT=IDUM)
     &         'Y, Yhat, and RESIDUAL for each observation'
     &         // ' using the final regression model'
         END IF

         IF (PLPRES .OR. PLSCAT .OR. PLRESI) THEN
            WRITE (IUNOUT, 10070, IOSTAT=IDUM)
     &         'PLOT the following information:'
            IF (PLPRES) THEN
               WRITE (IUNOUT, 10080, IOSTAT=IDUM)
     &            'PRESS'
            END IF
            IF (PLSCAT) THEN
               WRITE (IUNOUT, 10080, IOSTAT=IDUM)
     &            'Dependent variable,'
     &            // ' independent variable SCATTER plots'
            END IF
            IF (PLRESI) THEN
               WRITE (IUNOUT, 10080, IOSTAT=IDUM)
     &            'RESIDUALS'
            END IF
            IF ((DATPLT .NE. DATTYP) .AND. (DATPLT .EQ. 'RAW')) THEN
               WRITE (IUNOUT, 10070, IOSTAT=IDUM)
     &            'Plot RAW data'
            END IF
         ELSE
            WRITE (IUNOUT, 10070, IOSTAT=IDUM)
     &         'NO analysis plots are requested'
         END IF
      END IF

C   --Echo selected variable data
      WRITE (IUNOUT, *)
      WRITE (IUNOUT, 10040)
10040  FORMAT (
     &   1X, 'INDEPENDENT VARIABLES', T29, ' DEPENDENT VARIABLES', /
     &   1X, 'selected for analysis', T29, 'selected for analysis', /
     &   1X, '---------------------', T29, '---------------------')
10050  FORMAT (1X, I6, 2X, A, :, T30, I6, 2X, A)
10060  FORMAT (1X, T30, I6, 2X, A)
      DO 300 IV = 1, MAX (NIV, NDV)
         IF ((IV .LE. NIV) .AND. (IV .LE. NDV)) THEN
            WRITE (IUNOUT, 10050, IOSTAT=IDUM)
     &         IXMODL(IV), LABEL(IXMODL(IV)),
     &         IXMODL(NIV+IV), LABEL(IXMODL(NIV+IV))
         ELSE IF (IV .LE. NIV) THEN
            WRITE (IUNOUT, 10050, IOSTAT=IDUM)
     &         IXMODL(IV), LABEL(IXMODL(IV))
         ELSE
            WRITE (IUNOUT, 10060, IOSTAT=IDUM)
     &         IXMODL(NIV+IV), LABEL(IXMODL(NIV+IV))
         END IF
  300 CONTINUE

      IF (ANYREG .AND. (NUMFRC .GT. 0)) THEN
         WRITE (IUNOUT, *)
         WRITE (IUNOUT, 10070, IOSTAT=IDUM)
     &      'FORCE the following variables to be included in model:'
         DO 310 IFRC = 1, NUMFRC
            IX = IXFRC(IFRC)
            IF (REGTYP .EQ. 'STEPWISE') IX = IXMODL(IX)
            WRITE (IUNOUT, 10050, IOSTAT=IDUM)
     &         IX, LABEL(IX)
  310    CONTINUE
      END IF

C   --Check input data
      IF ((NIV .EQ. 0) .OR. (NDV .EQ. 0)) THEN
         CALL QAMESSAG (-1, '+ERROR',
     &      'Independent and/or dependent variables'
     &      // ' have not been defined')
         IERRCT = IERRCT + 1
      END IF

      IF (IERRCT .GT. 0) RETURN 1

      RETURN

  320 CONTINUE
      CALL QAMESSAG (0, '+',
     &   'Normal termination on end of input file')
      CALL QAMESSAG (-1, '+',
     &   'Normal termination on end of input file')
      RETURN 2

10070  FORMAT (1X, A)
10080  FORMAT (1X, ' o ', A, (:, /, 1X, '   ', A))

C=======================================================================
      ENTRY SCANUSERINPUT (IUNUSR, INDFIL, DEPFIL)
C=======================================================================

C   --*** SCANUSERINPUT *** (STEPWISE) Scan user input file for file names
C   --   Modified by Amy Gilkey - revised 03/05/95
C   --
C   --SCANUSERINPUT scans the user input file for the file names of the
C   --independent and dependent variables data file.
C   --
C   --Parameters:
C   --   IUNUSR - IN - the user input file unit number
C   --   INDFIL - IN - the name of the independent variables data file
C   --   DEPFIL - IN - the name of the dependent variables data file

  330 CONTINUE

C   --Read and parse one line from the input file
      CALL FFRDFLDS (IUNUSR, 0, '>', MAXFLD, IOSTAT, NUMFLD,
     &   INTYP, CFIELD, IFIELD, RFIELD)
      IF (IOSTAT .NE. 0) GOTO 340
      IF (NUMFLD .EQ. 0) GOTO 330
      INTYP(MIN(NUMFLD,MAXFLD)+1) = -999

      IFLD = 1
      CALL FFCHAR (IFLD, INTYP, CFIELD, ' ', WORD)

      IF (MATCHSTR (WORD, 'IND_FILE', 5)
     &   .OR. MATCHSTR (WORD, 'DEP_FILE', 5)) THEN
         CALL FFCHAR (IFLD, INTYP, CFIELD, ' ', FILNAM)
         IF (FILNAM .NE. ' ') THEN
            INQUIRE (FILE=FILNAM, NAME=FULNAM, IOSTAT=IDUM)
            IF (FULNAM .NE. ' ') FILNAM = FULNAM
         END IF
         IF (MATCHSTR (WORD, 'IND_FILE', 5)) THEN
            IF (INDFIL .NE. ' ') THEN
               CALL QAMESSAG (0, 'WARNING',
     &            'Independent variable data file has been redefined')
            END IF
            INDFIL = FILNAM
         ELSE
            IF (DEPFIL .NE. ' ') THEN
               CALL QAMESSAG (0, 'WARNING',
     &            'Dependent variable data file has been redefined')
            END IF
            DEPFIL = FILNAM
         END IF
      END IF

      GOTO 330

  340 CONTINUE
      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_READUSERINPUT.FOR
C *5    30-APR-1996 12:28:55 APGILKE "Minor changes to SCATTER, OUTPUT, PLOT, etc"
C *4    10-APR-1996 10:00:25 APGILKE "Add plot text file capability"
C *3     3-MAR-1996 11:36:18 APGILKE "Convert to double precision"
C *2     4-DEC-1995 19:13:36 APGILKE "Added DUMP command"
C *1     1-NOV-1995 11:24:09 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_READUSERINPUT.FOR
