C *** camlib_FREEFIELD_COMMON.INC ***

C   --These items pertain to the free-field reader routines:
      CHARACTER*256 SAVLIN
C   --   SAVLIN - the input line
      COMMON /XFFCOMMON/ SAVLIN

      CHARACTER CMTCH, CONTCH
C   --   CMTCH - the comment character
C   --   CONTCH - the continuation character
      PARAMETER (CMTCH = '!', CONTCH = '&')
