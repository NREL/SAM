C     Last change:  S    28 Jan 98    2:45 pm
       Program LHSDRV
c      New driver for LHS Program developed to accomodate conversion
c      to DLLs for Visual Basic Interface with stand-alone version of code
c
C      INCLUDE 'KILLFILE.INC'                                           GDW-96  
      USE KILLFILE                      
C      INCLUDE 'PARMS.INC'                                              GDW-96  
      USE PARMS                         
C      INCLUDE 'CPARAM.INC'                                             GDW-96  
      USE CPARAM                        
C
      USE DISTNM
      USE CSAMP
      USE CWORKC
      USE CWORKX
      USE CRANK
      USE CCMATR
      USE STAR
      USE UICORR
      USE CHRCRD
      USE OBSTR
      USE PDMAT
      USE FIRSTS
      USE LOCALVARS
c
C       Here are the calls that need to be made to initialize the modules.
C       Note that PARMS must be initialized first since it contains the
C       constants that are to be used to initialize the other modules.
C       Note that it must also be compiled first!
C
        CALL PRAMS_INIT()
C
        CALL DISTNM_INIT()
        CALL CPARAM_INIT()
        CALL CSAMP_INIT()
        CALL CWORKC_INIT()
        CALL CWORKX_INIT()
        CALL CRANK_INIT()
        CALL CCMATR_INIT()
        CALL STAR_INIT()
        CALL UICORR_INIT()
        CALL CHRCRD_INIT()
        CALL OBSTR_INIT()
        CALL PDMAT_INIT()
        CALL FIRSTS_INIT()
        CALL LOCALVARS_INIT()
C
       KLLERR = .False.
c
c      moved setup for LHS error file 99 to here -- Error file is
c      now opened/closed in driver routine rather than subroutine FILEOC
c
c
c        -- Processing to open the LHS Error File
c
c        -- We want to be sure that the error file is created anew for this
c        -- run in order to assure that it has the proper date and time stamp
c        -- information.  Thus, we open it with Status=Unknown and immediately
c        -- close it with Status=Delete.  Then we re-open the file and write
c        -- an error message to it.  We then close it with Status=Keep in
c        -- order to assure that the program buffers are flushed and the file
c        -- will really exist on the disk.  Finally, we re-open the file and
c        -- leave it open until the program executes a normal termination.
c        -- If the program crashes, the error file will remain in existance.
c        -- However, on normal termination, the error file will be deleted.
c        -- Thus, an external program can check for normal termination by
c        -- checking for the existance of this file.
         Open (99, File='LHS.ERR', Status='UNKNOWN', Form='FORMATTED')
         Write (99,*) 'One line into the file just to be sure...'
         Close  (99, Status='DELETE')
         Open (99, File='LHS.ERR', Status='NEW', Form='FORMATTED')
         Write (99,*) 'An error occurred during LHS processing.'
         Write (99,*) 'Consult the message file for additional ',
     1      'information.'
         Close (99, Status='KEEP')
         Open (99, File='LHS.ERR', Status='OLD', Form='FORMATTED')
c

C
c
c  NOTE:  Keyword file was previously obtained in the READ subroutine.
c  The location was moved to accomodate LHS conversion to DLL for use
c  in the Visual Basic program; putting control of keyword input in driver
c  routine allows all the other routines to remain the same for both the
c  Visual Basic and stand-alone versions.
c
c
c     -- Get the keyword file name from the command line.
c     -- If none specified, offer the user the opportunity to input
c     -- one from the keyboard or accept the default KEYWORD.DAT
c
      Print *, 'Welcome to LHS - The Latin Hypercube Sampling Program'
      Print *, ' '
c
c     Call GetCL(CmdLin)

!dec$ if defined (__CVF66__)
c	USE DFLIB
!dec$ endif

      call GETARG(1,CmdLin)

      If (CmdLin /= ' ') Then
          LenFil = LEN_TRIM(CmdLin)
         Open (5, File=CmdLin(1:LenFil), Status="OLD", Err=10)
         Go To 100
 10      Print *, 'Error opening file specified on command line.'
         Print *
      End If
 15   Print *, 'Enter the name of the LHS input file to be read,'
      Print *, 'enter / to exit LHS, or enter . to accept the '
      Print *, 'default input file name (KEYWORD.DAT):  '
      Print *
      Read (*,9001) CmdLin
 9001 FORMAT (A128)
      If (CmdLin == '/') Then
         Print *, 'Program Terminated'
         Stop
      Else If (CmdLin == '.') Then
         Open (5, File='KEYWORD.DAT', Status="OLD", Err=20)
         CmdLin = 'KEYWORD.DAT'
         Go To 100
      Else
         LenFil = LEN_TRIM(CmdLin)
         Open (5, File=CmdLin(1:LenFil), Status="OLD", Err=20)
         Go To 100
      End If
c
 20   Print *, 'Error opening the file ', CmdLin
      Print *
      Go To 15
c
c     -- Now ready to start reading the keyword file.
 100  Continue
      Call LHS
C
      If (KLLERR) then
c     An Error has occurred
         Write (99,*) 'Error was detected during LHS run'
         Close (99, Status = 'Keep')
       Else
c      No error has occurred, close and delete error file
C        -- Since this is a normal program termination, close the LHS
C        -- Error file with Status=Delete so that other processors can
C        -- know that the program terminated successfully.
         Close (99, Status='DELETE')
       Endif
C
       Close (6)
C
        CALL LOCALVARS_CLOSE()
        CALL DISTNM_CLOSE()
        CALL CPARAM_CLOSE()
        CALL CSAMP_CLOSE()
        CALL CWORKC_CLOSE()
        CALL CWORKX_CLOSE()
        CALL CRANK_CLOSE()
        CALL CCMATR_CLOSE()
        CALL STAR_CLOSE()
        CALL UICORR_CLOSE()
        CALL CHRCRD_CLOSE()
        CALL OBSTR_CLOSE()
        CALL PDMAT_CLOSE()
        CALL FIRSTS_CLOSE()
C
        CALL PRAMS_CLOSE()
C
        PRINT *
        PRINT *, "***** LHS COMPLETE *****"
        PRINT *
C
       Stop
C
       end
