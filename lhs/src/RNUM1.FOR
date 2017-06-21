C****************************************************************
C FUNCTION RNUM1 IS USED TO CREATE A RANDOM NUMBER.
C
      REAL FUNCTION RNUM1()
C
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CPARAM.INC'                                              GDW-96  
      USE CPARAM                        
C
      DOUBLE PRECISION RMCNP
      EXTERNAL RMCNP
C
      CALL RMCNPI(ISEED)
      If(KLLERR) Return
      RNUM1 = RMCNP(ISEED)
C
      RETURN
      END
