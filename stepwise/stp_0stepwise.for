!     Last change:  CS    6 Apr 2010   11:07 am
!   /************************************************************************
!    *                                                                      *
!    *   SALLABERRY Cedric - SANDIA NATIONAL LABORATORIES                   *
!    *   -------------------------------------------------                  *
!    *                                                                      *
!    *                                                                      *
!    *                         STEPWISE INPUT                               *
!    *        (READ PARAMETERS SELECTED BY USER TO GENERATE AN INPUT        *
!    *         FILE FOR THE STEPWISE PROGRAM)                               *
!    *                                                                      *
!    ************************************************************************
!    *                                                                      *
!    *                   file : STEPWISE_INPUT.f90                          *
!    *                                                                      *
!    *                                                                      *
!    *                                                                      *
!    *                                                                      *
!    *    Author(s)    | Version |   Date     | Comments                    *
!    *----------------------------------------------------------------------*
!    * C. SALLABERRY   |  1.0    | 06/17/2010 |                             *
!    *                 |         |            |                             *
!    *                 |         |            |                             *
!    *                 |         |            |                             *
!    *                 |         |            |                             *
!    ************************************************************************/
!



      PROGRAM STEPWISE_INPUT

      IMPLICIT NONE


!  ++++++++++++++++
!  +   VARIABLES  +
!  ++++++++++++++++


!     parameter files (data input and output)

      CHARACTER(LEN=22)                           :: CONTROL_FILENAME, &
                                                     DATA_IN_FILENAME, &
                                                    DATA_OUT_FILENAME
      CHARACTER(LEN=22)                           ::  RESULT_FILENAME

      INTEGER                                     :: U_DATA_IN,   &
                                                     U_DATA_OUT,  &
                                                     U_CONTROL,   &
                                                     U_RESULT


!     File used for stepwise subroutine

      CHARACTER(LEN=22)                           :: STEP_INPUT_FILENAME


      INTEGER                                     :: U_STEP_INPUT


!     arrays for input and output

      REAL, DIMENSION(:,:), ALLOCATABLE           :: INPUT_SET

      REAL, DIMENSION(:,:), ALLOCATABLE           :: OUTPUT_SET

      CHARACTER(LEN=8), DIMENSION(:), ALLOCATABLE :: INPUT_NAMES
      CHARACTER(LEN=9), DIMENSION(:), ALLOCATABLE :: EXTENDED_NAMES


!     Options

      LOGICAL                                     :: IS_TITLE, IS_LABEL,&
                                                     IS_FORCE, IS_PLOT, &
                                                      IS_STEP, IS_BACK, &
                                                     IS_PRESS, IS_RANK, &
                                                    IS_WEIGHT, IS_DROP, &
                                                   IS_TRANSFO

      INTEGER                                     :: OPT             ! integer reading the options
      CHARACTER(LEN=30)                           :: TITLE_TEXT

!     Other parameters
      INTEGER                                     :: I, J            ! Loop counters
      INTEGER                                     :: SAMPLE_SIZE     ! sample size
      INTEGER                                     :: NB_INPUTS       ! Number of input variables
      INTEGER                                     :: NB_TIMESTEPS    ! Number of time steps in output
      REAL                                        :: SIGIN,SIGOUT    ! sigin and sigout for stepwise
      CHARACTER(LEN=10)                           :: DUMMY           ! dummy argument to estimate sample size
      INTEGER                                     :: NB_FORCED       ! Number of forced variables
      INTEGER, DIMENSION(:), ALLOCATABLE          :: FORCED          ! list of variables forced
      INTEGER                                     :: NB_DROPPED      ! Number of dropped variables
      INTEGER, DIMENSION(:), ALLOCATABLE          :: DROPPED         ! list of variables dropped
      INTEGER                                     :: WEIGHT_VAR      ! variable including the weight
      INTEGER                                     :: NB_DIGITS       ! counter Number of digits for DATA option

!     Formats
      CHARACTER(LEN=9)                            :: LABEL_FORMAT    ! format used to copy label
      CHARACTER(LEN=9)                            :: DATA_FORMAT     ! format used to copy data
      CHARACTER(LEN=17)                           :: FORCE_FORMAT    ! format used to list forced variables
      CHARACTER(LEN=17)                           :: DROP_FORMAT     ! format used to list dropped variables
      CHARACTER(LEN=30)                           :: MODEL_FORMAT    ! format used for the model
      CHARACTER(LEN=10)                           :: DATASET_FMT     ! format for the DATA option

!     For reading output data
      INTEGER                                          :: MAXSTEP
      PARAMETER (MAXSTEP=50)
      INTEGER                                          :: NB_STEPS
      REAL(8), DIMENSION(1:MAXSTEP)                    :: COEFF_DETERM
      REAL(8), DIMENSION(1:MAXSTEP)                    :: R_SQUARE_GAIN
      CHARACTER(LEN=8), DIMENSION(1:MAXSTEP,1:MAXSTEP) :: IN_NAMES
      REAL(8), DIMENSION(1:MAXSTEP,1:MAXSTEP)          :: SRC_VALUE
      INTEGER, DIMENSION(1:MAXSTEP,1:MAXSTEP)          :: INPUT_POSITION

!     reordering the data
      INTEGER, DIMENSION(:), ALLOCATABLE               :: RANK_VALUE
      REAL(8), DIMENSION(:), ALLOCATABLE               :: SRC_RANKED
      CHARACTER(LEN=8), DIMENSION(:), ALLOCATABLE      :: NAMES_RANK
      INTEGER                                          :: T, K
      LOGICAL                                          :: IS_TAKEN




!     =====
!     Begin
!     =====

!     ++++++++++++++++++
!     + Initialization +
!     ++++++++++++++++++


!     +++++++++++++++++++++++++++
!     + Assocation of the units +
!     +++++++++++++++++++++++++++

!     * parameter unit - begin at 20 *
      U_CONTROL   = 20

!     * input unit - begin at 30 *
      U_DATA_IN   = 30
      U_DATA_OUT  = 31

!     * output unit - begin at 40 *
      U_STEP_INPUT  = 40
      U_RESULT      = 41

!     +++++++++++++++++++++++++
!     + Naming the info file  +
!     +++++++++++++++++++++++++

      CONTROL_FILENAME    = 'stepin.txt'
      STEP_INPUT_FILENAME = 'STEPPC.INP'


!     *******************
!    *  Read input Data  *
!     *******************

      OPEN (U_CONTROL, FILE=CONTROL_FILENAME, STATUS='OLD')

      REWIND(U_CONTROL)

      ! read if there is title or not
      READ (U_CONTROL,*) OPT

      IS_TITLE = (OPT==1)

      ! read title if exists
      IF  (IS_TITLE) THEN
          READ (U_CONTROL,*) TITLE_TEXT
      END IF

      ! number of input parameters
      READ (U_CONTROL,*) NB_INPUTS


      ! array allocation consequently
      ALLOCATE(    INPUT_NAMES(1:NB_INPUTS+1))
      ALLOCATE( EXTENDED_NAMES(1:NB_INPUTS+1))

     ! number of timesteps
      READ (U_CONTROL,*) NB_TIMESTEPS


      ! read if there is label or not
      READ (U_CONTROL,*) OPT

      IS_LABEL = (OPT==1)


      ! read output label if exists
      IF  (IS_LABEL) THEN
          READ (U_CONTROL,*) INPUT_NAMES(NB_INPUTS+1)
          EXTENDED_NAMES(NB_INPUTS+1) = TRIM(INPUT_NAMES(NB_INPUTS+1))
      END IF

      ! read if there is backward regression or not
      READ (U_CONTROL,*) OPT

      IS_BACK = (OPT==1)

      ! read if there is stepwise regression or not
      READ (U_CONTROL,*) OPT

      IS_STEP = (OPT==1)

      ! read sigin and sigout if stepwise regression
      IF  (IS_STEP) THEN
          READ (U_CONTROL,*) SIGIN
          READ (U_CONTROL,*) SIGOUT
      END IF


      ! read if there are forced variables or not
      READ (U_CONTROL,*) OPT

      IS_FORCE = (OPT==1)

      ! read number of forced variables and # if IS_FORCE is true
      IF  (IS_FORCE) THEN
          READ (U_CONTROL,*) NB_FORCED
          ALLOCATE(FORCED(1:NB_FORCED))
          READ (U_CONTROL,*) (FORCED(J),J=1,NB_FORCED)
      END IF

      ! read if there are dropped  variables or not
      READ (U_CONTROL,*) OPT

      IS_DROP = (OPT==1)

      ! read number of dropped variables and # if IS_DROP is true
      IF  (IS_DROP) THEN
          READ (U_CONTROL,*) NB_DROPPED
          ALLOCATE(DROPPED(1:NB_DROPPED))
          READ (U_CONTROL,*) (DROPPED(J),J=1,NB_DROPPED)
      END IF

      ! read if PRESS calculation is done or not
      READ (U_CONTROL,*) OPT

      IS_PRESS = (OPT==1)

      ! read if variables are RANK transformed or not
      READ (U_CONTROL,*) OPT

      IS_RANK = (OPT==1)

      ! read if one of the variables is used as WEIGHTor not
      READ (U_CONTROL,*) OPT

      IS_WEIGHT = (OPT==1)

      ! read number of dropped variables and # if IS_DROP is true
      IF  (IS_WEIGHT) THEN
          READ (U_CONTROL,*) WEIGHT_VAR
      END IF


!
!
!
!

      DATA_IN_FILENAME    = 'input_data.txt'
      DATA_OUT_FILENAME   = 'output.txt'
      RESULT_FILENAME     = 'result.txt'


      CLOSE(U_CONTROL)

!     ******************************************
!     *  OPTIONS SET UP AS FALSE (NOT USED)    *
!     ******************************************

      IS_PLOT     = .FALSE.
      IS_TRANSFO  = .FALSE.

      NB_TIMESTEPS =  1


!     *******************
!    *  Read input Data  *
!     *******************

      OPEN (U_DATA_IN, FILE=DATA_IN_FILENAME, STATUS='OLD')

      REWIND(U_DATA_IN)

!     1. Determine sample size
!     note that first line is variables names if there are label, so SAMPLE_SIZE is initialized to -1

      IF  (IS_LABEL) THEN
          SAMPLE_SIZE=-1
      ELSE
          SAMPLE_SIZE=0
      END IF

      DO WHILE (.TRUE.)

          READ(U_DATA_IN,*,END=12) DUMMY
          SAMPLE_SIZE = SAMPLE_SIZE+1

      END DO

12    REWIND(U_DATA_IN)

      ALLOCATE(INPUT_SET(1:SAMPLE_SIZE,1:NB_INPUTS))


      IF  (IS_LABEL) THEN
          READ (U_DATA_IN,*) (INPUT_NAMES(J),J=1,NB_INPUTS)
          DO  J=1,NB_INPUTS
              WRITE(EXTENDED_NAMES(J),*) TRIM(INPUT_NAMES(J)),','
          END DO
       END IF
      DO  I=1,SAMPLE_SIZE
          READ(U_DATA_IN,*) (INPUT_SET(I,J),J=1,NB_INPUTS)
      END DO

      CLOSE(U_DATA_IN)

!     *******************
!    *  Read output Data  *
!     *******************

      OPEN (U_DATA_OUT, FILE=DATA_OUT_FILENAME, STATUS='OLD')

      REWIND(U_DATA_OUT)

      ALLOCATE(OUTPUT_SET(1:SAMPLE_SIZE,1:NB_TIMESTEPS))


      DO  I=1,SAMPLE_SIZE
          READ(U_DATA_OUT,*) (OUTPUT_SET(I,J),J=1,NB_TIMESTEPS)
      END DO

      CLOSE(U_DATA_OUT)


!   ==============================
!   CREATE INPUT FILE FOR STEPWISE
!   ==============================

      OPEN (U_STEP_INPUT, FILE=STEP_INPUT_FILENAME, STATUS='REPLACE')



! TITLE, alphanumeric data (title)
! OPTIONAL

      IF  (IS_TITLE) THEN
          WRITE(U_STEP_INPUT,FMT='(A7,A30)') 'TITLE, ',TITLE_TEXT
      END IF

! DATA, nb variables (including output), nb additional variables (as transformations), data disposition - end this with a period.
! REQUIRED

      NB_DIGITS=1
      J=10
      DO  WHILE( (NB_INPUTS+1)/J >= 1)
          NB_DIGITS = NB_DIGITS + 1
          J = J * 10
      END DO

      WRITE(DATASET_FMT,FMT='(A5,I1,A4)') '(A5,I',NB_DIGITS,',A5)'
      WRITE(U_STEP_INPUT,FMT=DATASET_FMT) 'DATA,',NB_INPUTS+1,',0,1.'

! LABEL(x), variable name (up to 8 characters) the number x in front of the label indicates which data is first labeled (subsequent numbers follows automatically).
! OPTIONAL

      IF  (IS_LABEL) THEN
          WRITE(LABEL_FORMAT,FMT='(A1,I3.1,A3)')  '(',NB_INPUTS+2,'A) '
          WRITE(U_STEP_INPUT,LABEL_FORMAT) 'LABEL(1)=',&
                (TRIM(EXTENDED_NAMES(J)),J=1,NB_INPUTS+1)
      END IF


!BACKWARD, SIG=alpha  backward elimination with a significance level alpha
! OPTIONAL BUT REQUIRED IF NO STEPWISE

      IF  (IS_BACK) THEN
          WRITE(U_STEP_INPUT,FMT='(A8)') 'BACKWARD'
      END IF

!STEPWISE, SINGIN=alpha1, SIGOUT=alpha2 - performs stepwise regression
! OPTIONAL BUT REQUIRED IF NO BACKWARD

      IF  (IS_STEP) THEN
          WRITE(U_STEP_INPUT,FMT='(A15,F3.2,A7,F3.2)') 'STEPWISE,SIGIN=' &
                ,SIGIN,',SIGOUT=',SIGOUT
      END IF

! FORCE,  force some variables. end this with a period.
! OPTIONAL

      IF  (IS_FORCE) THEN
          WRITE(FORCE_FORMAT,FMT='(A4,I2.1,A11)')  '(A5,',NB_FORCED, &
                '(A1,I3),A1)'
          WRITE(U_STEP_INPUT,FORCE_FORMAT) 'FORCE', &
                (',',FORCED(J),J=1,NB_FORCED),'.'
      END IF

! DROP  a,b,c.   drops some of the variables from the analysis. end this with a period.
! OPTIONAL
      IF  (IS_DROP) THEN
          WRITE(DROP_FORMAT,FMT='(A4,I2.1,A11)')  &
                '(A4,',NB_DROPPED,'(A1,I3),A1)'
          WRITE(U_STEP_INPUT,DROP_FORMAT) 'DROP', &
                (',',DROPPED(J),J=1,NB_DROPPED),'.'
      END IF

! OUTPUT, - output requested. May be CORR, SSXP, INVERSE, STEPS, RESIDUALS, ALL (residual not recommended)
! REQUIRED

      WRITE(U_STEP_INPUT,FMT='(A10)') 'OUTPUT,ALL'

! PLOT RESIDUALS - may not be appropriate in our case
! OPTIONAL - NOT IMPLEMENTED
      IF  (IS_PLOT) THEN
          WRITE(U_STEP_INPUT,FMT='(A14)')  'PLOT RESIDUALS'
      END IF


! MODEL, description of the model as a sum n=1+2+3+ …  NO BLANK SPACE TO THE LEFT OF EQUAL SIGN)
! REQUIRED
      WRITE(MODEL_FORMAT,FMT='(A12,I2.1,A15)')  &
            '(A6,I3.1,A1,',NB_INPUTS-1,'(I3.1,A1),I3.1)'
      WRITE(U_STEP_INPUT,MODEL_FORMAT) 'MODEL,'  &
            ,NB_INPUTS+1,'=',(J,'+',J=1,NB_INPUTS-1),NB_INPUTS

! PRESS  - use predicted error sum of squares to select between 2 competing models
! OPTIONAL

      IF  (IS_PRESS) THEN
          WRITE(U_STEP_INPUT,FMT='(A5)')  'PRESS'
      END IF

! RANK REGRESSION -  perform rank regression
! OPTIONAL
      IF  (IS_RANK) THEN
          WRITE(U_STEP_INPUT,FMT='(A15)')  'RANK REGRESSION'
      END IF

! WEIGHT=x   weighted regression analysis of the data with weight in variable x
! OPTIONAL
      IF  (IS_WEIGHT) THEN
          WRITE(U_STEP_INPUT,FMT='(A7,I3)')  'WEIGHT=', WEIGHT_VAR
      END IF



!TRANSFORMATION allows to transform some of the variables (may not be appropriate for us). The "-" sign defines reciprocal and not opposite. Use "=" to link variables and comma to separate the equations

      IF  (IS_TRANSFO) THEN
          WRITE(U_STEP_INPUT,FMT='(A14)')   'TRANSFORMATION'
      END IF


!END OF PARAMETERS - end of the analysis parameters - it is followed by the data


      WRITE(U_STEP_INPUT,FMT='(A17)')  'END OF PARAMETERS'

!( …) indicates the format of the data as fortran format

      WRITE(DATA_FORMAT,FMT='(A1,I3.1,A5)')  '(',NB_INPUTS+1,'E9.2) '

      WRITE(U_STEP_INPUT,FMT='(1X,A9)') DATA_FORMAT



!Data (one realization per column) according to the specified format

      DO  I=1,SAMPLE_SIZE
          WRITE(U_STEP_INPUT,FMT=DATA_FORMAT)  &
               (INPUT_SET(I,J),J=1,NB_INPUTS),OUTPUT_SET(I,1)
      END DO
!END OF DATA indicates the end of the data list

      WRITE(U_STEP_INPUT,FMT='(A11)') 'END OF DATA'


      CLOSE(U_STEP_INPUT)

      CALL STPDELT(NB_STEPS, COEFF_DETERM,IN_NAMES,SRC_VALUE,INPUT_POSITION)

! find values rank
      ALLOCATE (RANK_VALUE(1:NB_STEPS))
      ALLOCATE (SRC_RANKED(1:NB_STEPS))
      ALLOCATE (NAMES_RANK(1:NB_STEPS))

      RANK_VALUE(1) = INPUT_POSITION(1,1)

      DO  J=2,MIN(NB_STEPS,MAXSTEP)
          IS_TAKEN = .TRUE.
          T=1
          K=1
          RANK_VALUE(J) = INPUT_POSITION(J,T)
          DO  WHILE (IS_TAKEN)
              IF ( (RANK_VALUE(K).NE.INPUT_POSITION(J,T)).AND.(K==J-1)) THEN
                 IS_TAKEN = .FALSE.
              ELSE
                   IF  (RANK_VALUE(K)==INPUT_POSITION(J,T)) THEN
                       K = 1
                       T = T + 1
                       RANK_VALUE(J) = INPUT_POSITION(J,T)
                   ELSE
                       K = K + 1
                   END IF
              END IF
          END DO

      END DO

      J = NB_STEPS

      DO  T=1,MIN(NB_STEPS,MAXSTEP)
          DO  K=1,MIN(NB_STEPS,MAXSTEP)
              IF  (RANK_VALUE(K)==INPUT_POSITION(J,T)) THEN
                  SRC_RANKED(K) = SRC_VALUE(J,T)
                  NAMES_RANK(K) = IN_NAMES(J,T)
              END IF
          END DO
      END DO

      OPEN (U_RESULT, FILE=RESULT_FILENAME, STATUS='REPLACE')
      WRITE (U_RESULT,*) ' #   VAR. NAME      R^2     Inc     SRC'
      WRITE (U_RESULT,*) '=========================================='
      R_SQUARE_GAIN(1) = COEFF_DETERM(1)
      DO  J=1,NB_STEPS
          IF (J>1) THEN 
             R_SQUARE_GAIN(J) = COEFF_DETERM(J) - COEFF_DETERM(J-1)
          END IF
          WRITE (U_RESULT,FMT='(I3,2X,A8,A2,F9.3,F9.3,F9.4)') &
                 RANK_VALUE(J), NAMES_RANK(J), ': ',COEFF_DETERM(J), &
                 R_SQUARE_GAIN(J), SRC_RANKED(J)
      END DO




      CLOSE(U_RESULT)

      END PROGRAM





!    ****************************
!    *  ORIGINAL STEPWISE CODE  *
!    ****************************

C=======================================================================
      PROGRAM STEPWISE
C=======================================================================

C                         *** STEPWISE ***

C   --*** STEPWISE *** (STEPWISE) Stepwise Regression Program
C   --
C   --The statistical code, STEPWISE, evaluates variable
C   --importance by developing regression models between the
C   --observed response and input variables using either a forward,
C   --backward, or stepwise regression procedure on the raw or
C   --ranked data.  Provided useful regression models can be
C   --developed, the absolute values of the standardized regression
C   --coefficients (or mathematically related partial correlation
C   --coefficients) can be used to rank variable importance.
C   --
C   --
C   --Expected Input:
C   --   The user input file.
C   --   The independent variables data file.
C   --   The dependent variables data file.
C   --
C   --Output:
C   --   The requested plots on the specified graphics device.
C   --   The text file of STEPWISE output in tabular form.
C   --
C   --
C   --Documentation:
C   --   WIPP PA User's Manual for STEPWISE.
C   --      Albuquerque, NM: Sandia National Laboratories.
C   --   R.L. Iman, J.M. Davenport, E.L. Frost, M.J. Shortencarier, 1980.
C   --      Stepwise regression with PRESS and rank regression (program
C   --      user's guide).  SAND79-1472.
C   --      Albuquerque, NM: Sandia National Laboratories.
C   --   Rechard, R.P., ed., 1992.
C   --      User's Manual for CAMCON: Compliance Assessment Methodology
C   --      Controller Version 3.0.  SAND90-1983.
C   --      Albuquerque, NM: Sandia National Laboratories.
C   --
C   --
C   --Code author:           V. J. McDonough (Kansas State University)
C   --                       K. E. Kemp
C   --                       Javier Rojo (Stanford University) added PRESS
C   --                       Amy Gilkey
C   --
C   --Code origin:           Kansas State University (V. J. McDonough)
C   --
C   --Code sponsor:          Amy Gilkey
C   --                       New Mexico Engineering Research Institute
C   --                       (505) 766-9629
C   --
C   --Code consultant:       Amy Gilkey
C   --
C   --Revision History:
C   --   04/09/96  2.20      Still testing - reviewer wants numbers in
C   --                       double precision.  Converted all calculations
C   --                       to double precision.  Added plot text file.
C   --                       DUMP command can choose variables.  Added
C   --                       SCATTER ALL.
C   --   11/28/95  2.20      Added prefix to source file names.  Changed
C   --                       default title.  Added DUMP print.  Fixed
C   --                       bug in STD01.
C   --   03/11/95  2.19ZO    Added archyperbolic axis.  Echo input file to
C   --                       output file.  Put input scan in ENTRY.
C   --   09/27/93  C-2.18VV  Put in TRANSFER_LIB and new graphics library.
C   --   01/18/93  C-2.17VV  Minor printout change.  Changed the TRANSFORM
C   --                       command.
C   --   12/06/92  C-2.16VV  Added RAW plot option.
C   --   11/04/92  C-2.15VV  Added STAND01 option.  Added tick interval to
C   --                       XSCALE/YSCALE.
C   --   10/18/92  C-2.14VV  Changed output format.  Added MEAN, OTHER
C   --                       print options.  Reorder variables during
C   --                       regression analysis.
C   --   08/20/92  C-2.13VV  Minor changes to commands.  Added SOFTCHAR
C   --                       command.
C   --   02/19/92  C-2.12VV  Added scatter-plot only option.  Added
C   --                       XSCALE and YSCALE command.  Added functions
C   --                       IFLZ, IFEZ, IFGZ.
C   --   03/07/91  C-2.11VV  Added warnings for non-unique labels.
C   --   02/12/91            Implemented QUIT option on plot so that it
C   --                       goes to next plot type.
C   --   02/05/91            Added IND_FILE, DEP_FILE command.
C   --   01/31/91            Implemented multiple data set capability.
C   --   01/22/91  C-2.10VV  Added QAAID (the filename) at bottom of plot.
C   --   01/14/91            Implemented the TRANSFORM command.
C   --   01/10/91            Removed limits on number of variables and
C   --                       observations.  Minor changes to fix numeric
C   --                       overflow (DIAG) and divide by zero.  Cleaned
C   --                       up output.
C   --   01/08/91            Cleaned up code by breaking into subroutines.
C   --                       Changed input data files to include number of
C   --                       variables, labels, steps, etc. causing changes
C   --                       in user input file.
C   --   12/04/90  C-2.02VV  Added separate data file.
C   --   09/06/90  C-2.01VV  Fixed accuracy problem in TINV.
C   --   05/31/90  C-2.00VV  Cleaned up code.
C   --   04/11/90            Converted input to use free-field reader.
C   --                       Cleaned up output file (use QAPAGE, etc).
C   --                       Changed from double precision to single
C   --                       precision.
C   --
C   --External software used:
C   --   PLT_LIB graphics packages
C   --   CAMCON_LIB package (QA routines, etc.)
C   --   CAMSUPES_LIB package (dynamic memory, FORTRAN extensions)
C   --
C   --Hardware platform:     DEC Alpha
C   --Software platform:     OpenVMS AXP V6.1
C   --Language(s):           FORTRAN 77


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                ISSUED BY SANDIA NATIONAL LABORATORIES,              *
*                      A PRIME CONTRACTOR TO THE                      *
*                  UNITED STATES DEPARTMENT OF ENERGY                 *
* * * * * * * * * * * * * *   N O T I C E   * * * * * * * * * * * * * *
* This program was prepared as an account of work  sponsored  by  the *
* United States Government.  Neither the United States nor the United *
* States Department of Energy nor any of their employees, nor any  of *
* their  contractors,  subcontractors,  or their employees, makes any *
* warranty, express or implied, or assumes  any  legal  liability  or *
* responsibility  for the accuracy, completeness or usefulness of any *
* information, apparatus, product or process disclosed, or represents *
* that its use would not infringe privately owned rights.             *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


      IMPLICIT NONE

      INCLUDE 'stp_title_common.inc'

      INCLUDE 'stp_force_common.inc'

      INCLUDE 'stp_transform_common.inc'

      INCLUDE 'stp_print_options_common.inc'

      INCLUDE 'stp_plot_options_common.inc'

      INTEGER ISTRLEN
      INTEGER IQAERRUNI

      LOGICAL ANYREG
      LOGICAL WTVAR
      INTEGER IERR
      INTEGER NIVIN
      INTEGER NS
      INTEGER NOBSIN
      INTEGER NDVIN
      INTEGER NSx
      INTEGER NOBSINx
      INTEGER MAXVAR
      INTEGER IX
      INTEGER NIV, NDV
      INTEGER NCMT
      INTEGER NUMOBS
      INTEGER NDROP
      INTEGER NUMVAR
      INTEGER IWTVAR
      INTEGER NMODVR
      INTEGER IXVAR0
      INTEGER NDUMPVAR
      INTEGER MAXSTK
      INTEGER ISZSYM
      INTEGER IDUM
      REAL RDUM
      CHARACTER*8 SCATTR
      CHARACTER*80 NEWCMT(2)
      CHARACTER*80 TITLDV
      CHARACTER*8 DEVTMP
      INTEGER IUNUSR, IUNOUT, IUNIND, IUNDEP
      CHARACTER*128 FILESP(5), USRFIL, OUTFIL, INDFIL, DEPFIL, PLTFIL

      INTEGER, ALLOCATABLE :: IXMODL(:)
      INTEGER, ALLOCATABLE :: IXDUMP(:)
      INTEGER, ALLOCATABLE :: IXDROP(:)
      INTEGER, ALLOCATABLE :: IXDV(:)
      INTEGER, ALLOCATABLE :: IWORK(:)
      REAL, ALLOCATABLE :: XYRAW(:,:)
      REAL, ALLOCATABLE :: STACK(:,:)
      REAL, ALLOCATABLE :: XYRNK(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: XY2RAW(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: XY2RNK(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: WTS(:) 
      DOUBLE PRECISION, ALLOCATABLE :: VMEAN(:) 
      DOUBLE PRECISION, ALLOCATABLE :: STDEV(:) 
      DOUBLE PRECISION, ALLOCATABLE :: SSCP(:) 
      DOUBLE PRECISION, ALLOCATABLE :: DIAG(:) 
      DOUBLE PRECISION, ALLOCATABLE :: CORR(:) 
      CHARACTER*8, ALLOCATABLE :: LABEL(:)

      INTEGER IXSYM
      INTEGER I, J

      DATA IUNUSR / 5 /, IUNOUT / 6 /,
     &   IUNIND / 10 /, IUNDEP / 11 /, IUNPLT / 20 /

      IXSYM(I,J) = (I*I-I)/2 + J
C      --IXSYM statement function calculates (i,j) index for half-stored
C      --   lower-row-wise matrix stored in 1D


C   --Set up the program information

      CALL QASETUP ('STEPWISE', ' ', ' ',
     &   'Kansas State University, Amy Gilkey', 'Amy Gilkey')

      CALL QABANNER (0,
     &   'STEPWISE REGRESSION PROGRAM',
     &   ' ',
     &   'Courtesy of Dept. of Statistics - Kansas State University')

C   --Read the names of the files from the command line

      CALL FILDFNAM ('Input control file',
     &   'IN', 'REQ', '.INP')
      CALL FILDFNAM ('Input independent data file',
     &   'IN', 'OPT', '.TRN')
      CALL FILDFNAM ('Input dependent data file',
     &   'IN', 'OPT', '.TRN')
      CALL FILDFNAM ('Output diagnostics / text file',
     &   'OUT', 'REQ', '.OUT')
      CALL FILDFNAM ('Output plot text file',
     &   'OUT', 'OPT', '.PLT')

      CALL FILRDNAMS (FILESP, IERR)
      IF (IERR .NE. 0) THEN
         CALL FILWRNAMS (0, FILESP)
         CALL QAABORT ('Cannot assign files')
      END IF

      USRFIL = FILESP(1)
      INDFIL = FILESP(2)
      DEPFIL = FILESP(3)
      OUTFIL = FILESP(4)
      PLTFIL = FILESP(5)

C   --Set the QA aid to be the input file
      QAAID = USRFIL

C   --Scan the user input file for the names of the variable data files

      CALL FILOPEN (IUNUSR, 'IN', 'FORM', USRFIL, IERR)
      IF (IERR .EQ. 0) THEN
         CALL SCANUSERINPUT (IUNUSR, INDFIL, DEPFIL)
         FILESP(2) = INDFIL
         FILESP(3) = DEPFIL
      END IF
      CLOSE (IUNUSR, IOSTAT=IDUM)

C   --Open the output file

      CALL FILOPEN (IUNOUT, 'OUT', 'FORM', OUTFIL, IERR)
      IF (IERR .NE. 0) THEN
         CALL QAABORT ('Cannot open file ' // OUTFIL(:ISTRLEN(OUTFIL)))
         IUNOUT = 0
      END IF
      IDUM = IQAERRUNI (IUNOUT)
      IF (IUNOUT .GT. 0) THEN
         CALL QAPAGE (IUNOUT, 'PAGE')
         CALL QABANNER (IUNOUT, ' ', ' ', ' ')
         CALL QADOEDIS (IUNOUT, ' ')
         CALL FILWRNAMS (IUNOUT, FILESP)
      END IF

C   --Determine the plotting status

      PLTTXT = (PLTFIL .NE. ' ')
      PLTDEV = .FALSE.
      CALL EXSYMBOL ('VDI_TERM', DEVTMP, IDUM)
      IF (DEVTMP .NE. ' ') PLTDEV = .TRUE.
      CALL EXSYMBOL ('VDI_FILE', DEVTMP, IDUM)
      IF (DEVTMP .NE. ' ') PLTDEV = .TRUE.

C   --Open the plot text file (may be deleted later if empty)

      IF (PLTTXT) THEN
         CALL FILOPEN (IUNPLT, 'OUT', 'LIST', PLTFIL, IERR)
         IF (IERR .NE. 0) THEN
            CALL QAABORT ('Cannot open file '
     &         // PLTFIL(:ISTRLEN(PLTFIL)))
         END IF
         NUMPLT = 0
      END IF

C   --Read the number of variables and observation and variable labels

      IF (INDFIL .EQ. ' ') THEN
         CALL QAABORT ('Independent data file is not defined')
      END IF
      IF (DEPFIL .EQ. ' ') THEN
         CALL QAABORT ('Dependent data file is not defined')
      END IF

      CALL TRNIOPEN (IUNIND, INDFIL, IERR)
      IF (IERR .NE. 0) THEN
         CALL QAABORT ('Cannot open file ' // INDFIL(:ISTRLEN(INDFIL)))
      END IF
      CALL TRNIOPEN (IUNDEP, DEPFIL, IERR)
      IF (IERR .NE. 0) THEN
         CALL QAABORT ('Cannot open file ' // DEPFIL(:ISTRLEN(DEPFIL)))
      END IF

      TITLDV = ' '
      CALL TRNICMT (IUNDEP, 2, NCMT, NEWCMT, IERR)
      IF (IERR .EQ. 0) THEN
         IF (NCMT .GE. 2) THEN
            TITLDV = NEWCMT(2)
         END IF
      END IF

      CALL TRNISIZES (IUNIND, NIVIN, NS, NOBSIN, IERR)
      IF (IERR .NE. 0) GOTO 120
      CALL TRNISIZES (IUNDEP, NDVIN, NSx, NOBSINx, IERR)
      IF (IERR .NE. 0) GOTO 120
      IF ((NS .NE. 1) .OR. (NSx .NE. 1)) THEN
         CALL QAMESSAG (-1, 'CMDERR',
     &      'Number of steps on data files must be one')
         GOTO 120
      END IF
      IF (NOBSIN .NE. NOBSINx) THEN
         CALL QAMESSAG (-1, 'CMDERR',
     &      'Number of observations on data files must be equal')
         GOTO 120
      END IF

      MAXVAR = NIVIN + NDVIN + MAXTRN

      ALLOCATE (LABEL(MAXVAR))
      CALL TRNINAMES (IUNIND, NIVIN, LABEL, IERR)
      IF (IERR .NE. 0) GOTO 120
      CALL TRNINAMES (IUNDEP, NDVIN, LABEL(NIVIN+1), IERR)
      IF (IERR .NE. 0) GOTO 120

      CALL TRNICLOSE (IUNIND, IDUM)
      CALL TRNICLOSE (IUNDEP, IDUM)

C   --Check the variables for unique labels

      CALL CHKUNILAB (IUNOUT, NIVIN + NDVIN, LABEL, IDUM)

C   --Open user input file

      CALL FILOPEN (IUNUSR, 'IN', 'FORM', USRFIL, IERR)
      IF (IERR .NE. 0) THEN
         CALL QAABORT ('Cannot open file ' // USRFIL(:ISTRLEN(USRFIL)))
      END IF

C   --Set up to input the parameters for the first data set

      NIV = 0
      NDV = 0
      ALLOCATE (IXMODL(MAXVAR))
      NDUMPVAR = 0
      ALLOCATE (IXDUMP(MAXVAR))
      NDROP = 0
      ALLOCATE (IXDROP(NOBSIN))

C   --Input the parameters for the next data set

  100 CONTINUE

      ALLOCATE (IXDV(MAXVAR))

      CALL READUSERINPUT (IUNUSR, IUNOUT,
     &   TITLDV, NOBSIN, NUMOBS, NDROP, IXDROP,
     &   NIVIN, NDVIN, NUMVAR, LABEL, NIV, NDV, IXMODL,
     &   NDUMPVAR, IXDUMP,
     &   IWTVAR, SCATTR, ANYREG, IXDV, *110, *140)

      DEALLOCATE (IXDV)

C   --Read the data from disk

      ALLOCATE (XYRAW(NUMOBS,NUMVAR))

      IXVAR0 = 0
      CALL RDDATVAL (IUNIND, INDFIL, IXVAR0, NUMOBS,
     &   NDROP, IXDROP, XYRAW, *120)
      CALL RDDATVAL (IUNDEP, DEPFIL, IXVAR0, NUMOBS,
     &   NDROP, IXDROP, XYRAW, *120)

C   --Do the transformations on raw data

      IF (NUMTRN .GT. 0) THEN
         MAXSTK = 10
         ALLOCATE (STACK(MAXSTK,NUMOBS))

         CALL TRANSFORM (NUMOBS, XYRAW, MAXSTK, STACK)

         DEALLOCATE (STACK)
      END IF

C   --Dump the raw data, if requested

      if (dumpvr) then
         call dumpvar (iunout, title, 'RAW',
     &      numobs, ndumpvar, ixdump, label, xyraw)
      end if

      ALLOCATE (XYRNK(NUMOBS,NUMVAR))

      IF (DATTYP .EQ. 'RANK') THEN

C      --Rank the data

         ALLOCATE (IWORK(NUMOBS))

         CALL RANKALLDATA (NUMOBS, NUMVAR,
     &      XYRAW, XYRNK, IWORK)

         DEALLOCATE (IWORK)

      ELSE IF (DATTYP .EQ. 'STD01') THEN

C      --Calculate the standard 0-1 data

         CALL STD01 (NUMOBS, NUMVAR, XYRAW, XYRNK)

      ELSE
C      --Overlay raw and ranked data
         DO J = 1, NUMVAR
            DO I = 1, NUMOBS
               XYRNK(I,J) = XYRAW(I,J)
            END DO
         END DO
      END IF

C   --Dump the rank or standard 0-1 data, if requested

      if (dumpvr .and. (dattyp .ne. 'RAW')) then
         call dumpvar (iunout, title, dattyp,
     &      numobs, ndumpvar, ixdump, label, xyrnk)
      end if

      IF (SCATTR .NE. ' ') THEN

C      --Generate scatter plots

         IF ((DATTYP .NE. DATPLT) .AND. (DATPLT .EQ. 'RAW')) THEN
            CALL SCATTER (SCATTR,
     &         NUMOBS, NIV, NDV, IXMODL,
     &         LABEL, XYRAW)
         ELSE
            CALL SCATTER (SCATTR,
     &         NUMOBS, NIV, NDV, IXMODL,
     &         LABEL, XYRNK)
         END IF
      END IF

      IF (ANYREG) THEN

C      --Convert data to double precision for calculations

         ALLOCATE (XY2RAW(NUMOBS,NUMVAR))
         CALL SNG2DBL (NUMOBS * NUMVAR, XYRAW, XY2RAW)
         ALLOCATE (XY2RNK(NUMOBS,NUMVAR))
         CALL SNG2DBL (NUMOBS * NUMVAR, XYRNK, XY2RNK)

C      --Get the weight variable, if any

         WTVAR = (IWTVAR .NE. 0)

         IF (WTVAR) THEN
            ALLOCATE (WTS(NUMOBS))
            CALL CALCWTVAR (NUMOBS, XY2RAW, IWTVAR, WTS)
         ELSE
            ALLOCATE (WTS(1))
         END IF

C      --Calculate the mean, standard deviation, correlation matrix, etc.

         CALL QAMESSAG (0, '+',
     &      'Calculating means, correlation matrix, etc')

         NMODVR = NIV + NDV
         ISZSYM = IXSYM(NMODVR,NMODVR)

         ALLOCATE (VMEAN(NMODVR))
         ALLOCATE (STDEV(NMODVR))
         ALLOCATE (SSCP(ISZSYM))
         ALLOCATE (DIAG(NMODVR))
         ALLOCATE (CORR(ISZSYM))

         CALL CALCOR (IUNOUT, NUMOBS, NMODVR, IXMODL, LABEL,
     &      XY2RNK, WTVAR, WTS,
     &      VMEAN, STDEV, SSCP, DIAG, CORR)

         DEALLOCATE (SSCP)

C      --Perform regression analysis

         CALL REGRANAL (
     &      IUNOUT, NUMOBS, NIV, NDV, IXMODL,
     &      LABEL, XY2RAW, XY2RNK, WTVAR, WTS,
     &      VMEAN, STDEV, DIAG, CORR)

         DEALLOCATE (VMEAN)
         DEALLOCATE (STDEV)
         DEALLOCATE (DIAG)
         DEALLOCATE (CORR)
         DEALLOCATE (WTS)
         DEALLOCATE (XY2RAW)
         DEALLOCATE (XY2RNK)
      END IF
      DEALLOCATE (XYRAW)
      DEALLOCATE (XYRNK)

C   --Do next data set
      GOTO 100

  110 CONTINUE
      CALL QAABORT ('Invalid command input')
      GOTO 140

  120 CONTINUE
      CALL QAABORT ('Invalid data set')
      GOTO 140

  140 CONTINUE
C   --Terminate graphics
      CALL PLOTEX
      IF (PLTTXT) THEN
         IF (NUMPLT .GT. 0) THEN
            CLOSE (IUNPLT, IOSTAT=IDUM)
         ELSE
            CLOSE (IUNPLT, STATUS='DELETE', IOSTAT=IDUM)
         END IF
      END IF

      CLOSE (IUNUSR, IOSTAT=IDUM)
      IF (IUNOUT .GT. 0) THEN
         CALL QACPUS (IUNOUT, RDUM)
         CALL QAPAGE (IUNOUT, 'END')
         CLOSE (IUNOUT, IOSTAT=IDUM)
      END IF
      CALL QACPUS (0, RDUM)

      DEALLOCATE (LABEL)
      DEALLOCATE (IXMODL)
      DEALLOCATE (IXDUMP)
      DEALLOCATE (IXDROP)

      END
C CMS REPLACEMENT HISTORY, Element STP_0STEPWISE.FOR
C *5    30-APR-1996 12:28:49 APGILKE "Changed scatter"
C *4    10-APR-1996 09:59:49 APGILKE "Add plot text file capability"
C *3     3-MAR-1996 11:35:35 APGILKE "Convert to double precision"
C *2     4-DEC-1995 19:15:54 APGILKE "Fixed memory bugs, added dump"
C *1     1-NOV-1995 11:23:57 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_0STEPWISE.FOR
