C *** stp_transform_common.inc ***

      INTEGER MAXTRN, MXTDEF
      PARAMETER (MAXTRN = 100, MXTDEF = 500)

      INTEGER NUMTRN
C      --NUMTRN - the number of transformation variables
      INTEGER IXTDEF(0:MAXTRN)
C      --IXTDEF - the cumulative index of the transformation entries
      CHARACTER*8 NAMENT(MXTDEF)
C      --NAMENT - the transformation entry name
      CHARACTER*1 TYPENT(MXTDEF)
C      --TYPENT - the transformation entry type
C      --   (Constant, Variable, Operator, Function)
      INTEGER INXENT(MXTDEF)
C      --INXENT - the transformation entry index:
C      --   for Variable - the variable index
C      --   for Operator - the operator index
C      --   for Function - the function index
      REAL VALENT(MXTDEF)
C      --VALENT - the transformation entry value:
C      --   for Constant - the constant value
C      --   for Function - the number or parameters
      COMMON /TRNDEF/ NUMTRN, IXTDEF, INXENT, VALENT
      COMMON /TRNDEFc/ NAMENT, TYPENT
C CMS REPLACEMENT HISTORY, Element STP_TRANSFORM_COMMON.INC
C *1     1-NOV-1995 11:24:44 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_TRANSFORM_COMMON.INC
