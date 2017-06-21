C=======================================================================
      INTEGER FUNCTION IQAERRUNI (IUCOUT)
C=======================================================================

C   --*** IQAERRUNI *** (CAMCON_LIB) Set or return diagnostics file unit number
C   --   Written by Amy Gilkey - revised 02/27/93
C   --
C   --IQAERRUNI defines the unit number for the diagnostics file.  IQAERRUNI
C   --returns the new unit number.  If the given unit number is negative,
C   --IQAERRUNI returns the current unit number without resetting it.  If
C   --this routine is not called, the standard output device (terminal) is
C   --the default diagnostics file.
C   --
C   --Parameters:
C   --   IUCOUT - IN integer - the unit number for the diagnostics file:
C   --      >0 for unit number for the diagnostics file
C   --      =0 for the standard output device (terminal)
C   --      <0 to return the defined diagnostics file (unit is not changed)

      IMPLICIT NONE

C   --Parameter Definitions:
      INTEGER IUCOUT

      INTEGER NDIAG
      SAVE NDIAG

      DATA NDIAG / 0 /

      IF (IUCOUT .GE. 0) THEN
         NDIAG = IUCOUT
      END IF

      IQAERRUNI = NDIAG

      RETURN
      END
