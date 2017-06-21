C *** camlib_PROGRAM_COMMON.INC ***

      CHARACTER*32 PROGRM
C   --   PROGRM - the program name
      CHARACTER*128 EXEFULL
C   --   EXEFULL - the full path file name of the executable
      CHARACTER*32 EXENAM
C   --   EXENAM - the build name of the executable
      CHARACTER*8 EXEDAT8
      CHARACTER*10 EXEDAT10
C   --   EXEDAT8, EXEDAT10 - the build link date (mm/dd/yy, mm/dd/yyyy)
      CHARACTER*8 EXETYP
C   --   EXETYP - the build type (PROD, etc.)
      CHARACTER*16 EXECLASS
C   --   EXECLASS - the build class
      CHARACTER*8 EXEVERS
C   --   EXEVERS - the build version
      CHARACTER*8 RUNDAT8
      CHARACTER*10 RUNDAT10
      CHARACTER*8 RUNTIM
C   --   RUNDAT8, RUNDAT10 - the run date (mm/dd/yy, mm/dd/yyyy)
C   --   RUNTIM - the run time
      CHARACTER*16 RUNMAC, RUNSYS
C   --   RUNMAC, RUNSYS - the execution machine and operation system
      CHARACTER*80 AUTHOR
C   --   AUTHOR - the program author
      CHARACTER*80 SPONSR
C   --   SPONSR - the program sponsor
      COMMON /XQACOMMON/ PROGRM, 
     &   EXEFULL, EXENAM, EXEDAT8, EXEDAT10, EXETYP, EXECLASS, EXEVERS,
     &   RUNDAT8, RUNDAT10, RUNTIM, RUNMAC, RUNSYS,
     &   AUTHOR, SPONSR
