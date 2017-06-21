C=======================================================================
      SUBROUTINE QABIGLET (IUCOUT, LINCOL, INLINE)
C=======================================================================

C   --*** QABIGLET *** (CAMCON_LIB) Print line in big letters
C   --   Modified by Amy Gilkey - revised 01/06/96
C   --   Written by Bill Mills-Curran
C   --
C   --QABIGLET prints the input line in big letters to the terminal or an
C   --output file.  The letters are centered.
C   --
C   --Parameters:
C   --   IUCOUT - IN integer - the output file unit number:
C   --      >0 for output file unit number IUCOUT
C   --      =0 for the standard output device (terminal)
C   --      <0 for the defined diagnostics file
C   --   LINCOL - IN integer - the maximum number of columns (usually 80)
C   --   INLINE - IN char*(*) - the input line; a maximum of LINCOL/8
C   --      characters are printed

      IMPLICIT NONE

      INTEGER MAXLET
      PARAMETER (MAXLET = 14)
      INTEGER MAXTAB
      PARAMETER (MAXTAB = 69)
      INTEGER LETROW, MINCOL
      PARAMETER (LETROW = 7, MINCOL = 8)

C   --Parameter Definitions:
      INTEGER IUCOUT
      INTEGER LINCOL
      CHARACTER*(*) INLINE

C   --Definitions for functions called:
      INTEGER IQAERRUNI

      INTEGER I
      INTEGER IUNOUT
      INTEGER NUMLET
      INTEGER ILEFT, IRIGHT
      INTEGER LENLIN
      INTEGER IDUM
      INTEGER ICOL, IROW
      INTEGER IPT
      INTEGER LETCOL
      INTEGER NCOL
      INTEGER NBLANK
      CHARACTER*(MAXLET) UPLINE
      CHARACTER*(MINCOL) LINBIG(LETROW,MAXLET)
      CHARACTER*20 FMT

      CHARACTER*(MAXTAB) LETTAB
      CHARACTER*(MINCOL) LETBIG(LETROW,MAXTAB)
      CHARACTER*66 BLANKS
      SAVE LETTAB, LETBIG, BLANKS

      DATA BLANKS /' '/

      DATA LETTAB(1:36)  /'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'/
      DATA LETTAB(37:68) /'!"#$%&()*+,/.:;<>?@[\\]^_`{|}~ -='/
C#????      DATA LETTAB(37:48) /'$()*+,/.: -='/
      DATA LETTAB(MAXTAB:MAXTAB) /''''/

C   --The following character set is not ansi standard


C###START OF CHARACTER DEFINITION
      DATA (LETBIG(I, 1),I=1,LETROW) /
     &   '  AAAAA ',
     &   ' AA   AA',
     &   ' AA   AA',
     &   ' AAAAAAA',
     &   ' AA   AA',
     &   ' AA   AA',
     &   ' AA   AA'/
      DATA (LETBIG(I, 2),I=1,LETROW) /
     &   ' BBBBBB ',
     &   ' BB   BB',
     &   ' BB   BB',
     &   ' BBBBBB ',
     &   ' BB   BB',
     &   ' BB   BB',
     &   ' BBBBBB '/
      DATA (LETBIG(I, 3),I=1,LETROW) /
     &   '  CCCCC ',
     &   ' CC   CC',
     &   ' CC     ',
     &   ' CC     ',
     &   ' CC     ',
     &   ' CC   CC',
     &   '  CCCCC '/
      DATA (LETBIG(I, 4),I=1,LETROW) /
     &   ' DDDDDD ',
     &   ' DD   DD',
     &   ' DD   DD',
     &   ' DD   DD',
     &   ' DD   DD',
     &   ' DD   DD',
     &   ' DDDDDD '/
      DATA (LETBIG(I, 5),I=1,LETROW) /
     &   ' EEEEEEE',
     &   ' EE     ',
     &   ' EE     ',
     &   ' EEEEE  ',
     &   ' EE     ',
     &   ' EE     ',
     &   ' EEEEEEE'/
      DATA (LETBIG(I, 6),I=1,LETROW) /
     &   ' FFFFFFF',
     &   ' FF     ',
     &   ' FF     ',
     &   ' FFFFF  ',
     &   ' FF     ',
     &   ' FF     ',
     &   ' FF     '/
      DATA (LETBIG(I, 7),I=1,LETROW) /
     &   '  GGGGG ',
     &   ' GG   GG',
     &   ' GG     ',
     &   ' GG     ',
     &   ' GG  GGG',
     &   ' GG   GG',
     &   '  GGGGG '/
      DATA (LETBIG(I, 8),I=1,LETROW) /
     &   ' HH   HH',
     &   ' HH   HH',
     &   ' HH   HH',
     &   ' HHHHHHH',
     &   ' HH   HH',
     &   ' HH   HH',
     &   ' HH   HH'/
      DATA (LETBIG(I, 9),I=1,LETROW) /
     &   '   IIII ',
     &   '    II  ',
     &   '    II  ',
     &   '    II  ',
     &   '    II  ',
     &   '    II  ',
     &   '   IIII '/
      DATA (LETBIG(I,10),I=1,LETROW) /
     &   '      JJ',
     &   '      JJ',
     &   '      JJ',
     &   '      JJ',
     &   '      JJ',
     &   ' JJ   JJ',
     &   '  JJJJJ '/
      DATA (LETBIG(I,11),I=1,LETROW) /
     &   ' KK   KK',
     &   ' KK  KK ',
     &   ' KK KK  ',
     &   ' KKKK   ',
     &   ' KKKKK  ',
     &   ' KK  KK ',
     &   ' KK   KK'/
      DATA (LETBIG(I,12),I=1,LETROW) /
     &   ' LL     ',
     &   ' LL     ',
     &   ' LL     ',
     &   ' LL     ',
     &   ' LL     ',
     &   ' LL     ',
     &   ' LLLLLLL'/
      DATA (LETBIG(I,13),I=1,LETROW) /
     &   ' M     M',
     &   ' MM   MM',
     &   ' MMM MMM',
     &   ' MM M MM',
     &   ' MM   MM',
     &   ' MM   MM',
     &   ' MM   MM'/
      DATA (LETBIG(I,14),I=1,LETROW) /
     &   ' N    NN',
     &   ' NN   NN',
     &   ' NNN  NN',
     &   ' NN N NN',
     &   ' NN  NNN',
     &   ' NN   NN',
     &   ' NN    N'/
      DATA (LETBIG(I,15),I=1,LETROW) /
     &   '  OOOOO ',
     &   ' OO   OO',
     &   ' OO   OO',
     &   ' OO   OO',
     &   ' OO   OO',
     &   ' OO   OO',
     &   '  OOOOO '/
      DATA (LETBIG(I,16),I=1,LETROW) /
     &   ' PPPPPP ',
     &   ' PP   PP',
     &   ' PP   PP',
     &   ' PPPPPP ',
     &   ' PP     ',
     &   ' PP     ',
     &   ' PP     '/
      DATA (LETBIG(I,17),I=1,LETROW) /
     &   '  QQQQQ ',
     &   ' QQ   QQ',
     &   ' QQ   QQ',
     &   ' QQ   QQ',
     &   ' QQ Q QQ',
     &   ' QQ  QQQ',
     &   '  QQQQQQ'/
      DATA (LETBIG(I,18),I=1,LETROW) /
     &   ' RRRRRR ',
     &   ' RR   RR',
     &   ' RR   RR',
     &   ' RRRRRR ',
     &   ' RRRRR  ',
     &   ' RR  RR ',
     &   ' RR   RR'/
      DATA (LETBIG(I,19),I=1,LETROW) /
     &   '  SSSSSS',
     &   ' SS     ',
     &   ' SS     ',
     &   '  SSSSS ',
     &   '      SS',
     &   '      SS',
     &   ' SSSSSS '/
      DATA (LETBIG(I,20),I=1,LETROW) /
     &   ' TTTTTT ',
     &   '   TT   ',
     &   '   TT   ',
     &   '   TT   ',
     &   '   TT   ',
     &   '   TT   ',
     &   '   TT   '/
      DATA (LETBIG(I,21),I=1,LETROW) /
     &   ' UU   UU',
     &   ' UU   UU',
     &   ' UU   UU',
     &   ' UU   UU',
     &   ' UU   UU',
     &   ' UU   UU',
     &   '  UUUUU '/
      DATA (LETBIG(I,22),I=1,LETROW) /
     &   ' VV   VV',
     &   ' VV   VV',
     &   '  V   V ',
     &   '  VV VV ',
     &   '   V V  ',
     &   '   VVV  ',
     &   '    V   '/
      DATA (LETBIG(I,23),I=1,LETROW) /
     &   ' WW   WW',
     &   ' WW   WW',
     &   ' WW   WW',
     &   ' WW   WW',
     &   ' WW W WW',
     &   ' WWWWWWW',
     &   '  WW WW '/
      DATA (LETBIG(I,24),I=1,LETROW) /
     &   ' XX   XX',
     &   '  XX XX ',
     &   '   XXX  ',
     &   '   XXX  ',
     &   '   XXX  ',
     &   '  XX XX ',
     &   ' XX   XX'/
      DATA (LETBIG(I,25),I=1,LETROW) /
     &   ' YY   YY',
     &   '  YY YY ',
     &   '   YYY  ',
     &   '    YY  ',
     &   '    YY  ',
     &   '    YY  ',
     &   '    YY  '/
      DATA (LETBIG(I,26),I=1,LETROW) /
     &   ' ZZZZZZZ',
     &   '      Z ',
     &   '     Z  ',
     &   '    Z   ',
     &   '   Z    ',
     &   '  Z     ',
     &   ' ZZZZZZZ'/
      DATA (LETBIG(I,27),I=1,LETROW) /
     &   '  000000',
     &   ' 0    00',
     &   ' 0   0 0',
     &   ' 0  0  0',
     &   ' 0 0   0',
     &   ' 00    0',
     &   ' 000000 '/
      DATA (LETBIG(I,28),I=1,LETROW) /
     &   '    1   ',
     &   '   11   ',
     &   '  1 1   ',
     &   '    1   ',
     &   '    1   ',
     &   '    1   ',
     &   '  11111 '/
      DATA (LETBIG(I,29),I=1,LETROW) /
     &   '   2222 ',
     &   '  2    2',
     &   '      2 ',
     &   '     2  ',
     &   '    2   ',
     &   '   2    ',
     &   '  222222'/
      DATA (LETBIG(I,30),I=1,LETROW) /
     &   '  33333 ',
     &   ' 3     3',
     &   '       3',
     &   '     33 ',
     &   '       3',
     &   ' 3     3',
     &   '  33333 '/
      DATA (LETBIG(I,31),I=1,LETROW) /
     &   '     44 ',
     &   '   4  4 ',
     &   '  4   4 ',
     &   ' 444444 ',
     &   '      4 ',
     &   '      4 ',
     &   '      4 '/
      DATA (LETBIG(I,32),I=1,LETROW) /
     &   ' 555555 ',
     &   ' 5      ',
     &   ' 5      ',
     &   ' 55555  ',
     &   '      5 ',
     &   '      5 ',
     &   ' 55555  '/
      DATA (LETBIG(I,33),I=1,LETROW) /
     &   '  6666  ',
     &   ' 6      ',
     &   ' 6      ',
     &   ' 66666  ',
     &   ' 6    6 ',
     &   ' 6    6 ',
     &   '  6666  '/
      DATA (LETBIG(I,34),I=1,LETROW) /
     &   ' 7777777',
     &   '      7 ',
     &   '     7  ',
     &   '    7   ',
     &   '   7    ',
     &   '  7     ',
     &   ' 7      '/
      DATA (LETBIG(I,35),I=1,LETROW) /
     &   '   8888 ',
     &   '  8    8',
     &   '  8    8',
     &   '   8888 ',
     &   '  8    8',
     &   '  8    8',
     &   '   8888 '/
      DATA (LETBIG(I,36),I=1,LETROW) /
     &   '   9999 ',
     &   '  9    9',
     &   '  9    9',
     &   '   99999',
     &   '       9',
     &   '       9',
     &   '   9999 '/
      DATA (LETBIG(I,37),I=1,LETROW) /
     &   '    !   ',
     &   '    !   ',
     &   '    !   ',
     &   '    !   ',
     &   '    !   ',
     &   '        ',
     &   '    !   '/
      DATA (LETBIG(I,38),I=1,LETROW) /
     &   '   " "  ',
     &   '   " "  ',
     &   '   " "  ',
     &   '        ',
     &   '        ',
     &   '        ',
     &   '        '/
      DATA (LETBIG(I,39),I=1,LETROW) /
     &   '   # #  ',
     &   '   # #  ',
     &   ' #######',
     &   '   # #  ',
     &   ' #######',
     &   '   # #  ',
     &   '   # #  '/
      DATA (LETBIG(I,40),I=1,LETROW) /
     &   '    $   ',
     &   '  $$$$$$',
     &   ' $  $   ',
     &   '  $$$$$ ',
     &   '    $  $',
     &   ' $$$$$$ ',
     &   '    $   '/
      DATA (LETBIG(I,41),I=1,LETROW) /
     &   ' %%    %',
     &   ' %%   % ',
     &   '     %  ',
     &   '    %   ',
     &   '   %    ',
     &   '  %   %%',
     &   ' %    %%'/
      DATA (LETBIG(I,42),I=1,LETROW) /
     &   '   &    ',
     &   '  & &   ',
     &   '   &    ',
     &   '  & &   ',
     &   ' &   & &',
     &   ' &    & ',
     &   '  &&&& &'/
      DATA (LETBIG(I,43),I=1,LETROW) /
     &   '      ( ',
     &   '     (  ',
     &   '    (   ',
     &   '    (   ',
     &   '    (   ',
     &   '     (  ',
     &   '      ( '/
      DATA (LETBIG(I,44),I=1,LETROW) /
     &   '  )     ',
     &   '   )    ',
     &   '    )   ',
     &   '    )   ',
     &   '    )   ',
     &   '   )    ',
     &   '  )     '/
      DATA (LETBIG(I,45),I=1,LETROW) /
     &   ' *     *',
     &   '  *   * ',
     &   '   * *  ',
     &   ' *******',
     &   '   * *  ',
     &   '  *   * ',
     &   ' *     *'/
      DATA (LETBIG(I,46),I=1,LETROW) /
     &   '    +   ',
     &   '    +   ',
     &   '    +   ',
     &   ' +++++++',
     &   '    +   ',
     &   '    +   ',
     &   '    +   '/
      DATA (LETBIG(I,47),I=1,LETROW) /
     &   '        ',
     &   '        ',
     &   '        ',
     &   '        ',
     &   '   ,,   ',
     &   '    ,   ',
     &   '   ,    '/
      DATA (LETBIG(I,48),I=1,LETROW) /
     &   '       /',
     &   '      / ',
     &   '     /  ',
     &   '    /   ',
     &   '   /    ',
     &   '  /     ',
     &   ' /      '/
      DATA (LETBIG(I,49),I=1,LETROW) /
     &   '        ',
     &   '        ',
     &   '        ',
     &   '        ',
     &   '        ',
     &   '   ..   ',
     &   '   ..   '/
      DATA (LETBIG(I,50),I=1,LETROW) /
     &   '        ',
     &   '   ::   ',
     &   '   ::   ',
     &   '        ',
     &   '   ::   ',
     &   '   ::   ',
     &   '        '/
      DATA (LETBIG(I,51),I=1,LETROW) /
     &   '        ',
     &   '   ;;   ',
     &   '   ;;   ',
     &   '        ',
     &   '   ;;   ',
     &   '    ;   ',
     &   '   ;    '/
      DATA (LETBIG(I,52),I=1,LETROW) /
     &   '     <  ',
     &   '    <   ',
     &   '   <    ',
     &   '  <     ',
     &   '   <    ',
     &   '    <   ',
     &   '     <  '/
      DATA (LETBIG(I,53),I=1,LETROW) /
     &   '  >     ',
     &   '   >    ',
     &   '    >   ',
     &   '     >  ',
     &   '    >   ',
     &   '   >    ',
     &   '  >     '/
      DATA (LETBIG(I,54),I=1,LETROW) /
     &   '   ???  ',
     &   '  ?   ? ',
     &   '      ? ',
     &   '     ?  ',
     &   '    ?   ',
     &   '        ',
     &   '    ?   '/
      DATA (LETBIG(I,55),I=1,LETROW) /
     &   '  @@@@@ ',
     &   ' @     @',
     &   '       @',
     &   '  @@@  @',
     &   ' @  @  @',
     &   ' @  @ @ ',
     &   '  @@@@  '/
      DATA (LETBIG(I,56),I=1,LETROW) /
     &   '   [[[  ',
     &   '   [    ',
     &   '   [    ',
     &   '   [    ',
     &   '   [    ',
     &   '   [    ',
     &   '   [[[  '/
      DATA (LETBIG(I,57),I=1,LETROW) /
     &   ' \\      ',
     &   '  \\     ',
     &   '   \\    ',
     &   '    \\   ',
     &   '     \\  ',
     &   '      \\ ',
     &   '       \\'/
      DATA (LETBIG(I,58),I=1,LETROW) /
     &   '  ]]]   ',
     &   '    ]   ',
     &   '    ]   ',
     &   '    ]   ',
     &   '    ]   ',
     &   '    ]   ',
     &   '  ]]]   '/
      DATA (LETBIG(I,59),I=1,LETROW) /
     &   '    ^   ',
     &   '   ^ ^  ',
     &   '  ^   ^ ',
     &   ' ^     ^',
     &   '        ',
     &   '        ',
     &   '        '/
      DATA (LETBIG(I,60),I=1,LETROW) /
     &   '        ',
     &   '        ',
     &   '        ',
     &   '        ',
     &   '        ',
     &   '        ',
     &   ' _______'/
      DATA (LETBIG(I,61),I=1,LETROW) /
     &   '    ``  ',
     &   '    `   ',
     &   '     `  ',
     &   '        ',
     &   '        ',
     &   '        ',
     &   '        '/
      DATA (LETBIG(I,62),I=1,LETROW) /
     &   '     {  ',
     &   '    {   ',
     &   '    {   ',
     &   '   {    ',
     &   '    {   ',
     &   '    {   ',
     &   '     {  '/
      DATA (LETBIG(I,63),I=1,LETROW) /
     &   '    |   ',
     &   '    |   ',
     &   '    |   ',
     &   '    |   ',
     &   '    |   ',
     &   '    |   ',
     &   '    |   '/
      DATA (LETBIG(I,64),I=1,LETROW) /
     &   '   }    ',
     &   '    }   ',
     &   '    }   ',
     &   '     }  ',
     &   '    }   ',
     &   '    }   ',
     &   '   }    '/
      DATA (LETBIG(I,65),I=1,LETROW) /
     &   '  ~~    ',
     &   ' ~  ~  ~',
     &   '     ~~ ',
     &   '        ',
     &   '        ',
     &   '        ',
     &   '        '/
      DATA (LETBIG(I,66),I=1,LETROW) /
     &   '        ',
     &   '        ',
     &   '        ',
     &   '        ',
     &   '        ',
     &   '        ',
     &   '        '/
      DATA (LETBIG(I,67),I=1,LETROW) /
     &   '        ',
     &   '        ',
     &   '        ',
     &   ' -------',
     &   '        ',
     &   '        ',
     &   '        '/
      DATA (LETBIG(I,68),I=1,LETROW) /
     &   '        ',
     &   '        ',
     &   ' =======',
     &   '        ',
     &   ' =======',
     &   '        ',
     &   '        '/
      DATA (LETBIG(I,69),I=1,LETROW) /
     &   '  ''''    ',
     &   '   ''    ',
     &   '  ''     ',
     &   '        ',
     &   '        ',
     &   '        ',
     &   '        '/
C###END OF CHARACTER DEFINITION

      IF (INLINE .EQ. ' ') RETURN

      IUNOUT = IUCOUT
      IF (IUNOUT .LT. 0) THEN
	 IUNOUT = IQAERRUNI (IUCOUT)
      ENDIF

      NUMLET = MAXLET
      NUMLET = MIN (LINCOL/MINCOL, MAXLET)

C   --Delimit nonblank string

      CALL STRLIMIT (INLINE, ILEFT, IRIGHT)
      IF (ILEFT .LE. IRIGHT) THEN
         UPLINE = INLINE (ILEFT:IRIGHT)
         LENLIN = IRIGHT - ILEFT + 1
         IF (LENLIN .GT. NUMLET) THEN
            LENLIN = NUMLET
            CALL STRLIMIT (UPLINE(:LENLIN), IDUM, LENLIN)
C         --Print entire string if string is truncated
            CALL QAMESSAG (IUNOUT, 'CENTER', INLINE(ILEFT:IRIGHT))
            CALL QAMESSAG (IUNOUT, ' ', ' ')
         END IF
      ELSE
         UPLINE = ' '
         LENLIN = 0
      END IF

C   --Convert line to upper case

      CALL STRUPCASE (UPLINE)

C   --Load up characters

      DO 110 ICOL = 1, LENLIN
         IPT = INDEX (LETTAB, UPLINE(ICOL:ICOL))

C      --Character not found - replace with a blank
         IF (IPT .EQ. 0) IPT = INDEX (LETTAB, ' ')

C      --Insert banner letter

         DO 100 IROW = 1, LETROW
            LINBIG(IROW,ICOL) = LETBIG(IROW,IPT)
  100    CONTINUE
  110 CONTINUE

C   --Calculate the number of columns between letters and the total columns

      LETCOL = MINCOL + 1
      NCOL = LENLIN * LETCOL - 2
      IF (NCOL .GT. LINCOL) THEN
         LETCOL = MINCOL
         NCOL = LENLIN * LETCOL - 1
      END IF

C   --Calculate blank fill

      NBLANK = (LINCOL - NCOL) / 2
      NBLANK = MAX (0, MIN (NBLANK, 66))

C   --Print the banner

      DO 120 IROW = 1, LETROW
         IF (NBLANK .GT. 0) THEN
            IF (LETCOL .GT. MINCOL) THEN
               FMT = '(A, 25 (A, :, 1X))'
            ELSE
               FMT = '(A, 25 (A, :))'
            END IF
            IF (IUNOUT .LE. 0) THEN
               WRITE (*, FMT, IOSTAT=IDUM)
     &            BLANKS(:NBLANK), (LINBIG(IROW,I), I=1,LENLIN)
            ELSE
               WRITE (IUNOUT, FMT, IOSTAT=IDUM)
     &            BLANKS(:NBLANK), (LINBIG(IROW,I), I=1,LENLIN)
            END IF
         ELSE
            IF (LETCOL .GT. MINCOL) THEN
               FMT = '(25 (A, :, 1X))'
            ELSE
               FMT = '(25 (A, :))'
            END IF
            IF (IUNOUT .LE. 0) THEN
               WRITE (*, FMT, IOSTAT=IDUM)
     &            (LINBIG(IROW,I), I=1,LENLIN)
            ELSE
               WRITE (IUNOUT, FMT, IOSTAT=IDUM)
     &            (LINBIG(IROW,I), I=1,LENLIN)
            END IF
         END IF
  120 CONTINUE

      RETURN
      END
