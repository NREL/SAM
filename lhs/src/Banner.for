C     Last change:  GDW  13 Oct 98   11:46 am
C****************************************************************
C SUBROUTINE BANNER ECHOES USER INPUT CONCERNING THE SAMPLE
C SETTING. IT IS CALLED AT THE START OF EACH REPETITION OF THE
C SAMPLE AND IS USED TO DELIMIT REPETITIONS IN THE PRINTOUT
C
      SUBROUTINE BANNER(IREP)
C
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CPARAM.INC'                                              GDW-96  
      USE CPARAM                        
C
      CHARACTER DATRES*8, TIMRES*11, VERSN*45
      SAVE DATRES, TIMRES, VERSN
C     DATA IFIRST /  1  /
c     next lines added 1-17-96 to convert date/time routine to LF90
      integer :: dt(8)
      character (len=5) :: zone
      Character * 2 Chr2
C
C as part of DLL conversion, make this based on IREP, not just
C the first time through the code.
C     IF (IFIRST .EQ. 1) THEN
      IF (IREP .EQ. 1) THEN
C
C        -- Set the version number and date-time information the first
C        -- time through only.
C
C        -- LENGTH OF VERSN:
C                        1         2         3         4
C               123456789012345678901234567890123456789012345
         VERSN='2.20 Release 2, Compiled Oct. 13, 2009 IVF11 '
c         CALL DATE(DATRES)     changed 1-17-96
c         CALL TIME(TIMRES)     changed 1-17-96
         Call date_and_time (DATRES,TIMRES,zone,dt)

         Chr2 = DATRES(3:4)
         Write (DATRES,1005) dt(2),dt(3),Chr2
 1005    format(I2,'/',I2,'/',A2)
         Chr2 = TIMRES(8:9)
         Write (TIMRES,1010) dt(5),dt(6),dt(7),Chr2
 1010    format(I2,':',I2,':',I2,'.',A2)
C
         ILenT = LEN_TRIM(Title)
         ILenM = LEN_TRIM(MFile)
         ILenC = LEN_TRIM(CmdLin)
         ILenS = LEN_TRIM(SFile)
         ILenTr = LEN_TRIM(TreeFl)
C
C        -- Write banner information to the sample file (Unit 1) only
C        -- if the new output format is specified.
C
         If (NamOut == 1) Then
            Write (1,*) '$ LHS File Format Version 1.00 '
            Write (1,9100) DATRES, TIMRES, VERSN
            Write (1,9101) TITLE(1:ILenT), MFile(1:ILenM),
     1                     CmdLin(1:ILenC)
            If (TreeFl /= ' ') Write (1,9109) TreeFl(1:ILenTr)
            If (IRP == 1) Write (1,9107)
            If (ICM == 1) Write (1,9102)
            Write (1,9110)
         End If
C
C         IFIRST=0
C
      END IF
C
C     -- IF IREP GT 1 THEN RETRIEVE THE CURRENT VALUE OF THE RANDOM
C     -- SEED (ISEED) HERE (IF NECESSARY)
C
C     -- Write the banner to the output mesage file (Unit 6)
C
      WRITE (6,9000) DATRES, TIMRES, VERSN
      WRITE (6,9001) TITLE(1:ILenT), ISEED, NV, N
      Write (6,9008) SFile(1:ILenS), CmdLin(1:ILenC)
      If (TreeFl /= ' ') Write (6,9009) TreeFl(1:ILenTr)
      IF (NREP > 1) WRITE(6,9006) IREP, NREP
      IF (IRP == 1) WRITE(6,9007)
      IF (ICM == 1) WRITE(6,9002)
      IF (IDATA == 1) WRITE(6,9003)
      IF (IHIST == 1) WRITE(6,9004)
      IF (ICORR == 1) WRITE(6,9005)
      IF (IV1 == 1) WRITE (6,9013)
C
      RETURN
C
 9000 FORMAT('1',//,45X,'* * * * * * * * *  L H S  * * * * * * * * *',
     1           //,45X,'LATIN HYPERCUBE AND RANDOM SAMPLING PROGRAM',
     2           //,45X,'* * * * * * * * * * * * * * * * * * * * * *',
     3   ///,4X,'Run on ',A,' at ',A,' with LHS Version: ',A,//)
 9001 FORMAT('0',4X,A,//,4X,'Random Seed = ',I11,//,4X,
     1       'Number of Variables = ',I3,//,4X,'Number ',
     2       'of Observations = ',I4)
 9002 FORMAT('0',3X,'An input correlation matrix has been specified')
 9003 FORMAT('0',3X,'The sample input vectors will be printed ',
     1       'along with their corresponding ranks.')
 9004 FORMAT('0',3X,'Histograms of the actual sample will be plotted ',
     1       'for each input variable.')
 9005 FORMAT('0',3X,'The correlation matrices (raw data and rank ',
     1       'correlations) will be printed.')
 9006 FORMAT('0',3X,'Replication Number ',I3,' of ',I3,
     1       ' Replications.')
 9007 FORMAT('0',3X,'Random Pairing will be used.')
 9008 Format('0',3x,'The sample was written to the file: ', A, /,
     1       '0',3x,'Input was read from the file: ', A)
 9009 Format('0',3x,'           and from the file: ', A)
 9013 FORMAT(///,6X,'****** Version 1 Input Compatibility will ',
     1       'be used *****',//)
C
 9100 FORMAT('$', /, '$ This LHS run was executed on ', A,' at ', A,/
     1   '$ with LHS Version: ', A)
 9101 FORMAT('$ The run title was:', /,'$ ', A, /, '$', /,
     1   '$ Message output file for this run: ', A, /, '$', /,
     1   '$ Input file(s) for this run: ', A)
 9102 FORMAT('$ An input correlation matrix was specified.')
 9107 FORMAT('$ Random Pairing was used in this evaluation. ')
 9109 Format('$ and ', A, /, '$')
 9110 Format('$')
C
      END
