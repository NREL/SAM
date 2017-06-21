C=======================================================================
      SUBROUTINE REGRVAR (
     &   FIRSTDV, IUNOUT, NUMOBS, NUMVAR,
     &   YMEAN, TSSQ, YRAW, YRNK, YLABEL,
     &   NIVO, IXIVO, LABEL, XRAW, XRNK, WTVAR, WTS,
     &   VMEANO, STDEVO, DIAGO, CORRO, CORDVO,
     &   TIME, YHAT, RESI, YHARAW, RESRAW, PSTP,
     &   RANK, YSORT, BETAS, BETA, PRESS,
     &   IVAR, IXIV, VMEAN, STDEV, DIAG, CORR, CORDV, XINVER,
     &   MAXRSQ, IXRSQ, RSQV,
     &   INPUT_NAMES, SRC_VALUES, COEFF_DETERM, NBSTEPS)
C=======================================================================

C   --*** REGRVAR *** (STEPWISE) Perform regression analysis on all variables
C   --
C   --   Modified by Cédric Sallaberry - 07/27/2010
C   --
C   --   add NUMVAR - IN - number of variables
C   --   add arrays INPUT_NAMES, SRC_VALUE and COEFF_DETERM in order to 
C   --   capture desired information for SAM
C   --
C   --   Modified by Amy Gilkey - revised 11/25/96
C   --
C   --REGRVAR performs the regression analysis on each dependent variable.
C   --Routine REGRANAL is called to do the actual analysis.
C   --
C   --Parameters:
C   --   FIRSTDV - IN - true iff first time thru routine for this command set
C   --   IUNOUT - IN - the listing file unit number
C   --   NUMOBS - IN - the number of observations
C   --   YMEAN - IN - the mean for the dependent variable
C   --   TSSQ - IN - the cross product ? for the dependent variables
C   --   YRAW - IN - the raw dependent variable data
C   --   YRNK - IN - the dependent variable data (rank if 'RANK',
C   --      standard 0-1 if 'STD01')
C   --   YLABEL - IN - the dependent variable label
C   --   NIVO - IN - the number of selected independent variables
C   --   IXIVO - IN - the indices of the selected independent variables
C   --   LABEL - IN - the independent variable labels
C   --   XRAW - IN - the raw variable data
C   --   XRNK - IN - the variable data (rank if 'RANK', standard 0-1 if 'STD01')
C   --   WTVAR - IN - true iff a weighted regression analyis is needed
C   --   WTS - IN - the weights for the regression (if WTVAR)
C   --   VMEANO - IN - the mean for each selected independent variable
C   --   STDEVO - IN - the standard deviation for each selected independent
C   --      variable
C   --   DIAGO - IN - the cross product ? for all selected independent
C   --      variables
C   --   CORRO - IN - the correlation matrix for all selected independent
C   --      variables
C   --   CORDVO - IN - the correlation for all selected independent variables
C   --      with the dependent variable
C   --   TIME - SCRATCH - size = NUMOBS
C   --   YHAT - SCRATCH - size = NUMOBS
C   --   RESI - SCRATCH - size = NUMOBS
C   --   YHARAW - SCRATCH - size = NUMOBS (only if 'RANK')
C   --   RESRAW - SCRATCH - size = NUMOBS (only if 'RANK')
C   --   PSTP - SCRATCH - size = NUMOBS (only if not 'RANK')
C   --   RANK - SCRATCH - size = NUMOBS (only if 'RANK')
C   --   YSORT - SCRATCH - size = NUMOBS (only if 'RANK')
C   --   BETAS - SCRATCH - size = NIVO
C   --   BETA - SCRATCH - size = NIVO
C   --   PRESS - SCRATCH - size = NIVO (only if PLPRES)
C   --   IVAR - SCRATCH - size = NIVO
C   --   IXIV - SCRATCH - size = NIVO
C   --   VMEAN - SCRATCH - size = NIVO
C   --   STDEV - SCRATCH - size = NIVO
C   --   DIAG - SCRATCH - size = NIVO
C   --   CORR - SCRATCH - size = IXSYM(NIVO,NIVO)
C   --   CORDV - SCRATCH - size = NIVO
C   --   XINVER - SCRATCH - size = IXSYM(NIVO,NIVO)
C   --   MAXRSQ - IN - size of IXRSQ and RSQV
C   --   IXRSQ - SCRATCH - size = NIVO * 2 or so
C   --   RSQV - SCRATCH - size = 1 + NIVO * 2 or so

      IMPLICIT NONE

      INCLUDE 'stp_title_common.inc'
      INCLUDE 'stp_force_common.inc'
      INCLUDE 'stp_print_options_common.inc'
      INCLUDE 'stp_plot_options_common.inc'

      LOGICAL FIRSTDV
      INTEGER IUNOUT
      INTEGER NUMOBS
!     add type for NUMVAR      
      INTEGER NUMVAR
      DOUBLE PRECISION YMEAN
      DOUBLE PRECISION TSSQ
      DOUBLE PRECISION YRAW(NUMOBS)
      DOUBLE PRECISION YRNK(NUMOBS)
      CHARACTER*8 YLABEL
      INTEGER NIVO
      INTEGER IXIVO(NIVO)
      CHARACTER*8 LABEL(NIVO)
      DOUBLE PRECISION XRAW(NUMOBS,NIVO)
!     change XRNK size from (NUMOBS,NIVO) to (NUMOBS,NUMVAR)      
      DOUBLE PRECISION XRNK(NUMOBS,NUMVAR)
      LOGICAL WTVAR
      DOUBLE PRECISION WTS(NUMOBS)
      DOUBLE PRECISION VMEANO(NIVO)
      DOUBLE PRECISION STDEVO(NIVO)
      DOUBLE PRECISION DIAGO(NIVO)
      DOUBLE PRECISION CORRO(*)
      DOUBLE PRECISION CORDVO(NIVO)
      DOUBLE PRECISION TIME(NUMOBS)
      DOUBLE PRECISION YHAT(NUMOBS)
      DOUBLE PRECISION RESI(NUMOBS)
      DOUBLE PRECISION YHARAW(NUMOBS)
      DOUBLE PRECISION RESRAW(NUMOBS)
      DOUBLE PRECISION PSTP(NUMOBS)
      DOUBLE PRECISION RANK(NUMOBS)
      DOUBLE PRECISION YSORT(NUMOBS)
      DOUBLE PRECISION BETAS(NIVO)
      DOUBLE PRECISION BETA(NIVO)
      DOUBLE PRECISION PRESS(*)
      INTEGER IVAR(NIVO)
      INTEGER IXIV(NIVO)
      DOUBLE PRECISION VMEAN(NIVO)
      DOUBLE PRECISION STDEV(NIVO)
      DOUBLE PRECISION DIAG(NIVO)
      DOUBLE PRECISION CORR(*)
      DOUBLE PRECISION CORDV(NIVO)
      DOUBLE PRECISION XINVER(*)
      INTEGER MAXRSQ
      INTEGER IXRSQ(*)
      DOUBLE PRECISION RSQV(0:*)

      INTEGER ISTRLEN
      INTEGER LOCINT
      DOUBLE PRECISION FSUB
      DOUBLE PRECISION DCDFT
      DOUBLE PRECISION TINV

      LOGICAL ATEND, DORESI, CALSTP, PRTSTP
      INTEGER IOBS
      INTEGER NSTEPS
      INTEGER NIV
      INTEGER IADIM
      INTEGER IVDEL
      INTEGER NVAR
      INTEGER NVARST
      INTEGER INVERR
      INTEGER NRDF
      INTEGER NRSQ
      INTEGER IV
      INTEGER IV1, IV2
      INTEGER IK, IK1
      INTEGER IDUM
      DOUBLE PRECISION YVALUE
      DOUBLE PRECISION SY
      DOUBLE PRECISION SSY
      DOUBLE PRECISION PROBD
      DOUBLE PRECISION B0
      DOUBLE PRECISION A
      DOUBLE PRECISION CORR12
      DOUBLE PRECISION REGSS
      DOUBLE PRECISION RSQ
      DOUBLE PRECISION REGMS
      DOUBLE PRECISION RESSQ
      DOUBLE PRECISION RESMS
      DOUBLE PRECISION F
      DOUBLE PRECISION FPROB
      DOUBLE PRECISION XG6
      DOUBLE PRECISION RDMAX
      DOUBLE PRECISION B
      DOUBLE PRECISION CCC
      DOUBLE PRECISION BETA2
      DOUBLE PRECISION SS
      DOUBLE PRECISION TI
      DOUBLE PRECISION RSQD
      DOUBLE PRECISION T
      DOUBLE PRECISION PROB
      DOUBLE PRECISION TPROB
      DOUBLE PRECISION YH
      DOUBLE PRECISION SYHAT
      DOUBLE PRECISION SSYHAT
      DOUBLE PRECISION RESSS
      DOUBLE PRECISION RGRSS
      DOUBLE PRECISION SSTY
      DOUBLE PRECISION RPLUSE
      DOUBLE PRECISION COFINT
      DOUBLE PRECISION PRESSX
      DOUBLE PRECISION CONINT
      DOUBLE PRECISION ST
      DOUBLE PRECISION PSTL95, PSTP95
      DOUBLE PRECISION YHLL95, YHLU95
      DOUBLE PRECISION YHPL95, YHPU95
      CHARACTER STR80*80

      INTEGER, ALLOCATABLE :: ISTOK(:)
      REAL, ALLOCATABLE :: XPTS(:)
      REAL, ALLOCATABLE :: YPTS(:)
      DOUBLE PRECISION, ALLOCATABLE :: STA(:,:) 
      DOUBLE PRECISION, ALLOCATABLE :: STF1(:) 
      DOUBLE PRECISION, ALLOCATABLE :: YDUM(:) 
      DOUBLE PRECISION, ALLOCATABLE :: TMPINV(:) 


      CHARACTER*8      :: INPUT_NAMES(NUMVAR)
      DOUBLE PRECISION :: COEFF_DETERM(NUMVAR)
      DOUBLE PRECISION :: SRC_VALUES(NUMVAR)
      INTEGER          :: NBSTEPS   

      INTEGER IXSYM
      INTEGER I, J

      IXSYM(I,J) = (I*I-I)/2 + J
C      --IXSYM statement function calculates (i,j) index for half-stored
C      --   lower-row-wise matrix stored in 1D


      CALL QAPAGE (IUNOUT, 'PAGE')
      CALL QAMESSAG (IUNOUT, 'CENTER', TITLE)
      WRITE (STR80, 10000, IOSTAT=IDUM) YLABEL(:ISTRLEN(YLABEL))
      CALL QAMESSAG (IUNOUT, '+CENTER', STR80(:ISTRLEN(STR80)))

      IF (DATTYP .EQ. 'RANK') THEN

C      --Sort raw value of dependent variable (YSORT) and get corresponding
C      --ranks (RANK)

         DO 100 I = 1, NUMOBS
            YSORT(I) = YRAW(I)
  100    CONTINUE
         CALL RNKSRT (NUMOBS, YSORT, RANK)

C      --Get the sum (SY) and the sum of the squares (SSY) of the raw values
C      --of the dependent variable

         SY = 0.0
         SSY = 0.0
         DO 110 IOBS = 1, NUMOBS
            YVALUE = YRAW(IOBS)
            IF (WTVAR) THEN
               SY = SY + WTS(IOBS)*YVALUE
               SSY = SSY + WTS(IOBS)*YVALUE*YVALUE
            ELSE
               SY = SY + YVALUE
               SSY = SSY + YVALUE*YVALUE
            END IF
  110    CONTINUE
      END IF

      CALSTP = .TRUE.
      PRTSTP = PRSTEP

      DORESI = PLRESI .OR. PRRESI .OR. PRPRES .OR. PLPRES
     &   .OR. (DATTYP .EQ. 'RANK')

      ATEND = .FALSE.

      NSTEPS = 0
      NRSQ = 0
      RSQV(0) = 0.0
C   --   RSQV(0) is only used if IXRSQ overflows

      IF (REGTYP .EQ. 'STEPWISE') THEN

C      --Initialize stepwise regression process

         IADIM = NIVO+1 + NIVO
         ALLOCATE (STA(IADIM,IADIM))
         ALLOCATE (ISTOK(NIVO))
         ALLOCATE (STF1(NIVO))

         CALL STEPINIT (NUMOBS, NIVO, CORRO, CORDVO,
     &      IADIM, STA, ISTOK)
      END IF

      IF (REGTYP .EQ. 'BACKWARD') THEN
C      --Set no variable to delete in first step of backward elimination
         IVDEL = -999
         PROBD = 0.0
         NIV = NIVO
      END IF

      IF (REGTYP .NE. 'STEPWISE') THEN
C      --Select all variables (unless stepwise regression)
         NVAR = NIVO
         CALL INICOUNT (NVAR, IVAR)
      ELSE
         NVAR = 0
      END IF

  120 CONTINUE
      NVARST = NVAR
      IF (REGTYP .EQ. 'STEPWISE') THEN

C      --Perform step of stepwise regression

         CALL STEPSTEP (NVARST, NVAR, IVAR, ATEND,
     &      IADIM, STA, ISTOK, STF1)
         IF (ATEND .AND. (NVAR .EQ. 0)) THEN
            CALL QAMESSAG (-1, '+ERROR',
     &         'No selected independent variables'
     &         // ' qualify for entry into the model')
            GOTO 410
         END IF
      END IF

      IF (REGTYP .EQ. 'BACKWARD') THEN

C      --Delete previously selected variable in backward elimination process

         IF (IVDEL .GE. 0) THEN
C         --Determine if previously selected variable should be deleted
            IF (IVDEL .LE. 0) ATEND = .TRUE.
            IF (PROBD .LE. SIGF(1)) ATEND = .TRUE.
            IF (NIV .LE. 1) ATEND = .TRUE.

            IF (.NOT. ATEND) THEN
               NVAR = NIV - 1
               DO 130 IV = IVDEL, NVAR
                  IVAR(IV) = IVAR(IV+1)
  130          CONTINUE
            END IF
         END IF
      END IF

      IF (ATEND) THEN

C      --If at the end of the regression analysis, the results may need to
C      --be printed (if not PRSTEP).  This is done by setting the flags to
C      --print-only and repeating the calculation section for this step.

         IF (.NOT. PRTSTP) THEN
            CALSTP = .FALSE.
            PRTSTP = .TRUE.
         ELSE
            GOTO 300
         END IF
      END IF

      IF (CALSTP) THEN

C      --Increment step number and adjust arrays for new selected variables
C      --and calculate inverse of correlation matrix

         NSTEPS = NSTEPS + 1

         IF (PRSTEP .AND. (NSTEPS .GT. 1)) THEN
            WRITE (IUNOUT, 10200, IOSTAT=IDUM)
            WRITE (STR80, 10000, IOSTAT=IDUM) YLABEL(:ISTRLEN(YLABEL))
            CALL QAMESSAG (IUNOUT, '+CENTER', STR80(:ISTRLEN(STR80)))
         END IF

C      --Set up means, etc for new selected variables
         CALL DELVAR (NVAR, IVAR,
     &      IXIVO, VMEANO, STDEVO, DIAGO, CORRO, CORDVO,
     &      NIV, IXIV, VMEAN, STDEV, DIAG, CORR, CORDV)

C      --Calculate inverse correlation matrix
         DO 140 I = 1, IXSYM(NIV,NIV)
            XINVER(I) = CORR(I)
  140    CONTINUE
         CALL INVERTSYM (NIV, XINVER, INVERR)
         IF (INVERR .GT. 0) GOTO 420
      END IF

      IF (PRINV .AND. PRTSTP) THEN
         CALL PRTSYMMAT (IUNOUT, 'E',
     &      'INVERSE OF CORRELATION MATRIX for Variable -'
     &      // YLABEL(:ISTRLEN(YLABEL)) // '-',
     &      NIV, IXIV, LABEL, XINVER)
      END IF

      IF (CALSTP) THEN
         NRDF = NUMOBS - NIV - 1

C      --Calculate beta star and beta arrays

         RSQ = 0.0
         B0 = YMEAN
         DO 160 IV1 = 1, NIV
            A = 0.0
            DO 150 IV2 = 1, NIV
               IF (IV2 .LE. IV1) THEN
                  CORR12 = XINVER(IXSYM(IV1,IV2))
               ELSE
                  CORR12 = XINVER(IXSYM(IV2,IV1))
               END IF
               A = A + CORR12*CORDV(IV2)
  150       CONTINUE
            BETAS(IV1) = A
            BETA(IV1) = A * SQRT (TSSQ/dble(DIAG(IV1)))
C         --Calculate b0 (the intercept)
            B0 = B0 - VMEAN(IV1)*BETA(IV1)
C         --Calculate r-square
            RSQ = RSQ + A*CORDV(IV1)
  160    CONTINUE
C      --Calculate regression ssq
         REGSS = RSQ * TSSQ
         REGMS = REGSS / DFLOAT(NIV)

C      --Calculate the analysis of regression table for dependent variable

         RESSQ = TSSQ - REGSS
C      --patch -- negative residual sum of squares
         IF ((RESSQ .LE. 0.0) .OR. (NRDF .LE. 0)) THEN
            ATEND = .TRUE.
            RESSQ = 0.0
            RESMS = 0.0
            F = 9999.
            IF (NRDF .EQ. 1) THEN
               FPROB = 1.0E35
            ELSE
               FPROB = 0.0
            END IF
            XG6 = 0.0
         ELSE
            RESMS = RESSQ / DFLOAT(NRDF)
            F = REGMS / RESMS
            FPROB = FSUB (F, NIV, NRDF)
            IF (PRMISC) THEN
               XG6 = 0.0
               DO 180 IV1 = 1, NIV
                  DO 170 IV2 = 1, NIV
                     IF (IV2 .LE. IV1) THEN
                        CORR12 = XINVER(IXSYM(IV1,IV2))
                     ELSE
                        CORR12 = XINVER(IXSYM(IV2,IV1))
                     END IF
                     IF (IV1 .NE. IV2) THEN
                        XG6 = XG6 + VMEAN(IV1)*VMEAN(IV2)*CORR12
     &                     / SQRT (DIAG(IV1)*DIAG(IV2))
                     ELSE
                        XG6 = XG6 + VMEAN(IV1)*VMEAN(IV2)*CORR12
     &                     / DIAG(IV1)
                     END IF
  170             CONTINUE
  180          CONTINUE
               XG6 = SQRT (RESMS * ((1.0/DFLOAT(NUMOBS)) + XG6))
            END IF
         END IF

         IF (REGTYP .EQ. 'STEPWISE') THEN
C         --Figure out which variables were dropped by this step
            DO 190 IV1 = 1, NRSQ
               IF (IXRSQ(IV1) .GT. 0) THEN
                  IF (LOCINT (IXRSQ(IV1), NVAR, IVAR) .LE. 0) THEN
                     IXRSQ(IV1) = -IXRSQ(IV1)
                  END IF
               END IF
  190       CONTINUE
            DO 200 IV1 = 1, NVAR
               IF (LOCINT (IVAR(IV1), NRSQ, IXRSQ) .LE. 0) THEN
                  IF (NRSQ .LT. MAXRSQ) THEN
                     NRSQ = NRSQ + 1
                     IXRSQ(NRSQ) = IVAR(IV1)
                  END IF
               END IF
  200       CONTINUE
C         --Assign r-square for variables selected this step
            DO 210 IV1 = NVARST+1, NVAR
               IV = LOCINT (IVAR(IV1), NRSQ, IXRSQ)
               IF (IV .GT. 0) RSQV(IV) = RSQ
  210       CONTINUE
         END IF

      END IF

      IF (PRTSTP) THEN
         WRITE (STR80, 10010, IOSTAT=IDUM) YLABEL(:ISTRLEN(YLABEL))
         CALL QAMESSAG (IUNOUT, '+CENTER', STR80(:ISTRLEN(STR80)))
         WRITE (IUNOUT, 10020, IOSTAT=IDUM)
         WRITE (IUNOUT, 10030, IOSTAT=IDUM)
     &      NIV, REGSS, REGMS, F, FPROB,
     &      NRDF, RESSQ, RESMS,
     &      NUMOBS-1, TSSQ
         WRITE (IUNOUT, 10040, IOSTAT=IDUM) RSQ, B0
         IF (PRMISC) THEN
            WRITE (IUNOUT, 10050, IOSTAT=IDUM) XG6
         END IF
      END IF

C   --Calculate beta, beta star, ss, t*s, and r-square deletes and
C   --significance for each independent variable in this analysis

      IF (CALSTP .OR. PRTSTP) THEN
         IF (PRTSTP) THEN
            IF (REGTYP .EQ. 'STEPWISE') THEN
               WRITE (IUNOUT, 10060, IOSTAT=IDUM)
            ELSE
               WRITE (IUNOUT, 10070, IOSTAT=IDUM)
            END IF
         END IF
         RDMAX = 0.0
         IVDEL = 0
         
         DO 230 IV1 = 1, NIV
            B = BETAS(IV1)
            CCC = XINVER(IXSYM(IV1,IV1))
            BETA2 = (B * B) / CCC
            SS = BETA2 * TSSQ
            IF (RESMS .NE. 0.0) THEN
               TI = B * SQRT (TSSQ/(RESMS*CCC))
            ELSE
               TI = 9999.
            END IF
            RSQD = RSQ - BETA2
            T = ABS(TI)
            IF (NRDF .EQ. 0) THEN
               PROB = 0.0
            ELSE IF (T .LE. 1.0E-10) THEN
               PROB = 1.0
            ELSE
               TPROB = DCDFT (T, DFLOAT(NRDF))
               PROB = 2.0 * (1.0-TPROB)
            END IF
            IF (REGTYP .EQ. 'BACKWARD') THEN
               IF (LOCINT (IXIV(IV1), NUMFRC, IXFRC) .LE. 0) THEN
                  IF (RSQD .GT. RDMAX) THEN
                     RDMAX = RSQD
                     PROBD = PROB
                     IVDEL = IV1
                  END IF
               END IF
            END IF
            IF (PRTSTP) THEN
               IV2 = LOCINT (IVAR(IV1), NRSQ, IXRSQ)
               IF (REGTYP .EQ. 'STEPWISE') THEN
                  IF (IV1 .GT. 1) THEN
                     IV = LOCINT (IVAR(IV1-1), NRSQ, IXRSQ)
                  ELSE
                     IV = 0
                  END IF
                  DO 220 IV = IV+1, IV2-1
                     IF (IXRSQ(IV) .LE. 0) THEN
                        WRITE (IUNOUT, 10090, IOSTAT=IDUM)
     &                     LABEL(IXIVO(-IXRSQ(IV))), RSQV(IV)
                     END IF
  220             CONTINUE
                  WRITE (IUNOUT, 10080, IOSTAT=IDUM)
     &               LABEL(IXIV(IV1)), RSQV(IV2),
     &               BETA(IV1), B, SS, TI, RSQD, PROB
               ELSE
                  WRITE (IUNOUT, 10100, IOSTAT=IDUM)
     &               LABEL(IXIV(IV1)),
     &               BETA(IV1), B, SS, TI, RSQD, PROB
               END IF
            END IF
!        save variables of interest in new arrays            
         INPUT_NAMES(IV1)  = LABEL(IXIV(IV1))
         SRC_VALUES(IV1)   = BETA(IV1)
         IF (REGTYP .EQ. 'STEPWISE') THEN
             COEFF_DETERM(IV1) = RSQV(IV2)  
         ELSE
             COEFF_DETERM(IV1) = RSQD
         END IF
  230    CONTINUE
         NBSTEPS = NIV
      END IF

      IF (DORESI) THEN
         IF (CALSTP) THEN

C         --Calculate time, yhats, residuals

            DO 250 IOBS = 1, NUMOBS
               TIME(IOBS) = IOBS
               YH = B0
               DO 240 IV = 1, NIV
                  YH = YH + XRNK(IOBS,IXIV(IV)) * BETA(IV)
  240          CONTINUE
               YHAT(IOBS) = YH
               RESI(IOBS) = YRNK(IOBS) - YHAT(IOBS)
  250       CONTINUE
         END IF
      END IF

      IF (DATTYP .EQ. 'RANK') THEN
         IF (CALSTP) THEN

C         --Convert rank predictions to raw predictions
C         --as outlined in Iman and Conover *technometrics* - 1979

            DO 280 IOBS = 1, NUMOBS
               IF (YHAT(IOBS) .LE. RANK(1)) THEN
                  YHARAW(IOBS) = YSORT(1)
               ELSE IF (YHAT(IOBS) .GE. RANK(NUMOBS)) THEN
                  YHARAW(IOBS) = YSORT(NUMOBS)
               ELSE
                  IK = YHAT(IOBS) + 0.000005
  260             CONTINUE
                  IF (RANK(IK) .GT. YHAT(IOBS)) THEN
                     IK = IK - 1
                     GOTO 260
                  END IF
                  IK1 = IK + 1
  270             CONTINUE
                  IF (RANK(IK1) .LE. RANK(IK)) THEN
                     IK1 = IK1 + 1
                     GOTO 270
                  END IF
                  YHARAW(IOBS) = YSORT(IK) + (YSORT(IK1)-YSORT(IK))
     &               * (YHAT(IOBS)-RANK(IK)) / (RANK(IK1)-RANK(IK))
               END IF
               RESRAW(IOBS) = YRAW(IOBS) - YHARAW(IOBS)
  280       CONTINUE
         END IF

         IF (PRTSTP .AND. PRMISC) THEN
            SYHAT = 0.0
            SSYHAT = 0.0
            RESSS = 0.0
            DO 290 IOBS = 1, NUMOBS
               YVALUE = YRAW(IOBS)
               IF (WTVAR) THEN
                  SYHAT = SYHAT + WTS(IOBS)*YHARAW(IOBS)
                  SSYHAT = SSYHAT + WTS(IOBS)*YHARAW(IOBS)*YHARAW(IOBS)
                  RESSS = RESSS + WTS(IOBS)*RESRAW(IOBS)*RESRAW(IOBS)
               ELSE
                  SYHAT = SYHAT + YHARAW(IOBS)
                  SSYHAT = SSYHAT + YHARAW(IOBS)*YHARAW(IOBS)
                  RESSS = RESSS + RESRAW(IOBS)*RESRAW(IOBS)
               END IF
  290       CONTINUE

            RGRSS = SSYHAT - (2.0*SYHAT - SY) * (SY/DFLOAT(NUMOBS))
            RPLUSE = RGRSS + RESSS
            SSTY = SSY - SY*SY/DFLOAT(NUMOBS)
            IF (RPLUSE .NE. 0.0) THEN
               WRITE (IUNOUT, 10170, IOSTAT=IDUM) RGRSS / RPLUSE
            END IF
            IF ((RPLUSE+SSTY) .NE. 0.0) THEN
               COFINT = ABS (2 * (RPLUSE/(RPLUSE+SSTY)) - 1.0)
               WRITE (IUNOUT, 10180, IOSTAT=IDUM) COFINT
            END IF
         END IF
      END IF

      IF (PRPRES .OR. PLPRES) THEN
         IF (CALSTP) THEN

C         --Calculate the PRedicted Error Sum of Squares (PRESS)

            ALLOCATE (YDUM(NIV))
!           add NUMVAR in CALCPRESS call - C.S. 07/27/2010            
            CALL CALCPRESS (NUMOBS, NUMVAR, NIV, RESI,
     &         VMEAN, STDEV, XINVER,
     &         IXIV, XRNK, WTVAR, WTS, PRESSX, YDUM)
            DEALLOCATE (YDUM)
            IF (PRPRES) WRITE (IUNOUT, 10110, IOSTAT=IDUM) PRESSX
            IF (PLPRES) PRESS(NSTEPS) = PRESSX
         END IF
      END IF

      IF (REGTYP .EQ. ' ') ATEND = .TRUE.

C   --Go do regression analysis for remaining variables
      IF (CALSTP) GOTO 120

  300 CONTINUE

      IF (PRRESI) THEN
         IF (DATTYP .EQ. 'RANK') THEN

C         --Print rank residuals and corresponding raw residuals

            WRITE (STR80, 10120, IOSTAT=IDUM) YLABEL(:ISTRLEN(YLABEL))
            CALL QAMESSAG (IUNOUT, '+CENTER', STR80(:ISTRLEN(STR80)))
            WRITE (IUNOUT, 10130, IOSTAT=IDUM)
            DO 310 IOBS = 1, NUMOBS
               WRITE (IUNOUT, 10140, IOSTAT=IDUM)
     &            TIME(IOBS), YRNK(IOBS), YHAT(IOBS), RESI(IOBS),
     &            YRAW(IOBS), YHARAW(IOBS), RESRAW(IOBS)
  310       CONTINUE

            IF (PRRESI .AND. PRMISC) THEN
               RESSS = 0.0
               DO 320 IOBS = 1, NUMOBS
                  YVALUE = YRAW(IOBS)
                  IF (WTVAR) THEN
                     RESSS = RESSS + WTS(IOBS)*RESRAW(IOBS)*RESRAW(IOBS)
                  ELSE
                     RESSS = RESSS + RESRAW(IOBS)*RESRAW(IOBS)
                  END IF
  320          CONTINUE
               WRITE (IUNOUT, 10190, IOSTAT=IDUM) RESSS
            END IF

         ELSE
C         --Compute 95% mean and individual confidence intervals

            IF (NRDF .NE. 0) THEN
               ALLOCATE (TMPINV(IXSYM(NIV+1,NIV+1)))

               CALL CALCCONFINTV (NUMOBS, NUMVAR, NIV, IXIV,
     &            XRNK, VMEAN, DIAG, CORR, PSTP, TMPINV)

               DEALLOCATE (TMPINV)

               CONINT = 0.975
               ST = SQRT (RESMS) * TINV (CONINT, 1, NRDF)
            ELSE
               ST = 0.0
            END IF

C         --Print raw residuals and confidence intervals

            WRITE (STR80, 10120, IOSTAT=IDUM) YLABEL(:ISTRLEN(YLABEL))
            CALL QAMESSAG (IUNOUT, '+CENTER', STR80(:ISTRLEN(STR80)))
            WRITE (IUNOUT, 10150, IOSTAT=IDUM)
            DO 330 IOBS = 1, NUMOBS
               IF (ST .NE. 0.0) THEN
                  PSTL95 = ST * SQRT (PSTP(IOBS))
                  PSTP95 = ST * SQRT (1.0+PSTP(IOBS))
               ELSE
                  PSTL95 = 0.0
                  PSTP95 = 0.0
               END IF
               YHLL95 = YHAT(IOBS) - PSTL95
               YHLU95 = YHAT(IOBS) + PSTL95
               YHPL95 = YHAT(IOBS) - PSTP95
               YHPU95 = YHAT(IOBS) + PSTP95
               WRITE (IUNOUT, 10160, IOSTAT=IDUM)
     &            TIME(IOBS), YRNK(IOBS), YHAT(IOBS), RESI(IOBS),
     &            YHLL95, YHLU95, YHPL95, YHPU95
  330       CONTINUE
         END IF
      END IF

      IF (PLPRES .AND. (NSTEPS .GT. 1)) THEN
         IF (PLTTXT) THEN
C         --Output PRESS values
            STR80 = 'PR_' // YLABEL
            CALL LISTPLOTd ('PRESS',
     &         STR80, NSTEPS, PRESS)
            CALL WRITEPLOT 
         END IF
         IF (PLTDEV) THEN
C         --Plot PRESS plot
            ALLOCATE (XPTS(NSTEPS))
            ALLOCATE (YPTS(NSTEPS))
            STR80 = 'PRESS of ' // YLABEL
            CALL DBL2SNG (NSTEPS, PRESS, YPTS)
            CALL PLOT (DATTYP, 'COUNT', 'PRESS',
     &         'STEP NUMBER', STR80, NSTEPS, XPTS, YPTS, *340)
  340       CONTINUE
            DEALLOCATE (XPTS)
            DEALLOCATE (YPTS)
         END IF
      END IF

      IF (PLSCAT) THEN
         IF (PLTTXT) THEN
            IF (FIRSTDV) THEN
               DO 350 IV = 1, NIVO
                  IF ((DATTYP .NE. DATPLT)
     &               .AND. (DATPLT .EQ. 'RAW')) THEN
                     CALL LISTPLOTd ('SCATTER',
     &                  LABEL(IXIVO(IV)), NUMOBS, XRAW(1,IXIVO(IV)))
                  ELSE
                     CALL LISTPLOTd ('SCATTER',
     &                  LABEL(IXIVO(IV)), NUMOBS, XRNK(1,IXIVO(IV)))
                  END IF
  350          CONTINUE
            END IF
            IF ((DATTYP .NE. DATPLT)
     &         .AND. (DATPLT .EQ. 'RAW')) THEN
               CALL LISTPLOTd ('SCATTER',
     &            YLABEL, NUMOBS, YRAW)
            ELSE
               CALL LISTPLOTd ('SCATTER',
     &            YLABEL, NUMOBS, YRNK)
            END IF
            CALL WRITEPLOT 
         END IF
C      --Plot dependent variable vs each independent variable
         IF (PLTDEV) THEN
            ALLOCATE (XPTS(NUMOBS))
            ALLOCATE (YPTS(NUMOBS))
            IF ((DATTYP .NE. DATPLT)
     &         .AND. (DATPLT .EQ. 'RAW')) THEN
               CALL DBL2SNG (NUMOBS, YRAW, YPTS)
            ELSE
               CALL DBL2SNG (NUMOBS, YRNK, YPTS)
            END IF
            DO 360 IV = 1, NIV
               IF ((DATTYP .NE. DATPLT)
     &            .AND. (DATPLT .EQ. 'RAW')) THEN
                  CALL DBL2SNG (NUMOBS, XRAW(1,IXIV(IV)), XPTS)
               ELSE
                  CALL DBL2SNG (NUMOBS, XRNK(1,IXIV(IV)), XPTS)
               END IF
               CALL PLOT (DATPLT, 'VARIABLE', 'VARIABLE',
     &            LABEL(IXIV(IV)), YLABEL,
     &            NUMOBS, XPTS, YPTS, *370)
  360       CONTINUE
  370       CONTINUE
            DEALLOCATE (YPTS)
            DEALLOCATE (XPTS)
         END IF
      END IF

      IF (PLRESI) THEN
         IF (PLTTXT) THEN
C         --Output each independent variable
            IF (FIRSTDV .AND. (.NOT. PLSCAT)) THEN
               DO 380 IV = 1, NIVO
                  IF ((DATTYP .NE. DATPLT)
     &               .AND. (DATPLT .EQ. 'RAW')) THEN
                     CALL LISTPLOTd ('RESIDUALS',
     &                  LABEL(IXIVO(IV)), NUMOBS, XRAW(1,IXIVO(IV)))
                  ELSE
                     CALL LISTPLOTd ('RESIDUALS',
     &                  LABEL(IXIVO(IV)), NUMOBS, XRNK(1,IXIVO(IV)))
                  END IF
  380          CONTINUE
            END IF
C         --Output residuals
            STR80 = 'RS_' // YLABEL
            CALL LISTPLOTd ('RESIDUALS',
     &         STR80, NUMOBS, RESI)
C         --Output time
            STR80 = 'TM_' // YLABEL
            CALL LISTPLOTd ('RESIDUALS',
     &         STR80, NUMOBS, TIME)
C         --Output yhats
            STR80 = 'YH_' // YLABEL
            CALL LISTPLOTd ('RESIDUALS',
     &         STR80, NUMOBS, YHAT)
            CALL WRITEPLOT 
         END IF
         IF (PLTDEV) THEN
            ALLOCATE (XPTS(NUMOBS))
            ALLOCATE (YPTS(NUMOBS))
            STR80 = 'RESIDUALS of ' // YLABEL
            CALL DBL2SNG (NUMOBS, RESI, YPTS)
C         --Plot residuals vs time
            CALL DBL2SNG (NUMOBS, TIME, XPTS)
            CALL PLOT (DATTYP, 'TIME', 'RESIDUAL',
     &         'TIME', STR80, NUMOBS, XPTS, YPTS, *400)
C         --Plot residuals vs yhats
            CALL DBL2SNG (NUMOBS, YHAT, XPTS)
            CALL PLOT (DATTYP, 'YHAT', 'RESIDUAL',
     &         'YHAT', STR80, NUMOBS, XPTS, YPTS, *400)
C         --Plot residuals vs each independent variable
            DO 390 IV = 1, NIV
               IF ((DATTYP .NE. DATPLT)
     &            .AND. (DATPLT .EQ. 'RAW')) THEN
                  CALL DBL2SNG (NUMOBS, XRAW(1,IXIV(IV)), XPTS)
               ELSE
                  CALL DBL2SNG (NUMOBS, XRNK(1,IXIV(IV)), XPTS)
               END IF
               CALL PLOT (DATPLT, 'VARIABLE', 'RESIDUAL',
     &            LABEL(IXIV(IV)), STR80,
     &            NUMOBS, XPTS, YPTS, *400)
  390       CONTINUE
  400       CONTINUE
            DEALLOCATE (XPTS)
            DEALLOCATE (YPTS)
         END IF
      END IF

  410 CONTINUE
      GOTO 430

  420 CONTINUE
C   --Error processing data (message already printed)
      GOTO 410

  430 CONTINUE
      IF (REGTYP .EQ. 'STEPWISE') THEN
         DEALLOCATE (STA)
         DEALLOCATE (ISTOK)
         DEALLOCATE (STF1)
      END IF

      RETURN

10000  FORMAT ('REGRESSION ANALYSIS for Dependent Variable -', A, '-')
10010  FORMAT ('ANALYSIS OF VARIANCE TABLE for Variable -', A, '-')
10020  FORMAT (/,
     &   1X, 'SOURCE    ', 3X, 'DofF', 2X, '     SS    ',
     &   3X, '     MS    ', 3X, '      F    ', 3X, ' SIGNIF', /,
     &   1X, '----------', 3X, '----', 2X, ' ----------',
     &   3X, ' ----------', 3X, ' ----------', 3X, ' ------')
10030  FORMAT (
     &   1X, 'REGRESSION', I6, 1P3E14.4, 0PF10.4, /,
     &   1X, 'RESIDUAL  ', I6, 1P2E14.4, /,
     &   1X, 'TOTAL     ', I6, 1PE14.4)
10040  FORMAT (/,
     &   1X, 'R-SQUARE =', F9.5, 5X,
     &   1X, 'INTERCEPT =', 1PE12.4)
10050  FORMAT (
     &   1X, 'Standard error =', 1PE12.4)
10060  FORMAT (/,
     &   1X, 'VARIABLE', 3X, ' R-SQUARE',
     &   3X, ' REGRESSION', 3X, 'STANDARDIZED',
     &   2X, '   PARTIAL ', 3X, '   T-TEST  ', 3X, '  R-SQUARE ',
     &   3X, '   ALPHA   ', /,
     &   1X, '        ', 3X, 'WHEN INCL',
     &   3X, 'COEFFICIENT', 3X, ' REGR COEFF',
     &   3X, '     SSQ   ', 3X, '   VALUES  ', 3X, '   DELETES ',
     &   3X, '    HATS   ', /,
     &   1X, '--------', 3X, ' --------',
     &   6 (3X, ' ----------')) !!! 3X, ' ------')
10070  FORMAT (/,
     &   1X, 'VARIABLE',
     &   3X, ' REGRESSION', 3X, 'STANDARDIZED',
     &   2X, '   PARTIAL ', 3X, '   T-TEST  ', 3X, '  R-SQUARE ',
     &   3X, '   ALPHA   ', /,
     &   1X, '        ',
     &   3X, 'COEFFICIENT', 3X, ' REGR COEFF',
     &   3X, '     SSQ   ', 3X, '   VALUES  ', 3X, '   DELETES ',
     &   3X, '    HATS   ', /,
     &   1X, '--------',
     &   6 (3X, ' ----------')) !!! 3X, ' ------')
10080  FORMAT (1X, A8, 3X, F9.5, 1P6E14.4) !!! 0PF10.4)
10090  FORMAT (1X, A8, 3X, F9.5,
     &   6 (3X, ' **********')) !!! 3X, ' ------')
10100  FORMAT (1X, A8, 1P6E14.4) !!! 0PF10.4)
10110  FORMAT (/, 1X, 'PRESS =', 1PE12.4)
10120  FORMAT ('TABLE OF RESIDUALS for Variable -', A, '-')
10130  FORMAT (/,
     &   1X, 'TIME',
     &   3X, '    RANK   ', 3X, ' PREDICTED ', 3X, '    RANK   ',
     &   3X, '    RAW    ', 3X, '    RAW    ', 3X, '    RAW    ', /,
     &   1X, '    ',
     &   3X, '    OF Y   ', 3X, ' RANK OF Y ', 3X, '  RESIDUAL ',
     &   3X, '     Y     ', 3X, '    YHAT   ', 3X, '  RESIDUAL ', /,
     &   1X, '----',
     &   3X, ' ----------', 3X, ' ----------', 3X, ' ----------',
     &   3X, ' ----------', 3X, ' ----------', 3X, ' ----------')
10140  FORMAT (1X, 0PF4.0, 1P6E14.4)
10150  FORMAT (/,
     &   1X, 'TIME', 3X, ' OBSERVED Y', 3X, 'PREDICTED Y',
     &   3X, '  RESIDUAL ', 3X, '   95% CONF INTV (MEAN)  ',
     &   3X, '  95% CONF INTV (INDIV)  ', /,
     &   1X, '----', 3X, ' ----------', 3X, ' -----------',
     &   3X, ' ----------', 3X, ' ------------------------',
     &   3X, ' ------------------------')
10160  FORMAT (1X, 0PF4.0, 1P7E14.4)
10170  FORMAT (/, 1X, 'Rank fit gives a raw data normalized R-SQUARE =',
     &   1PE12.4)
10180  FORMAT (/, 1X, 'Coefficient of interpolation =', 1PE12.4)
10190  FORMAT (/, 1X, 'Residual sum of squares on raw data =', 1PE12.4)
10200  FORMAT (/, 1X, 80('*'))
      END
C CMS REPLACEMENT HISTORY, Element STP_REGRVAR.FOR
C *6    25-NOV-1996 09:40:37 APGILKE "Fix R-square table"
C *5    15-NOV-1996 12:14:38 APGILKE "Fix R-square table"
C *4    30-APR-1996 12:28:59 APGILKE "Fixed bug in PLOT RAW"
C *3    10-APR-1996 10:00:32 APGILKE "Add plot text file capability"
C *2     3-MAR-1996 11:36:25 APGILKE "Convert to double precision"
C *1     1-NOV-1995 11:24:10 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_REGRVAR.FOR
