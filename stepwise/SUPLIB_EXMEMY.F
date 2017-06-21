      SUBROUTINE EXMEMY( MEMREQ,LOCBLK,MEMRTN )
      PARAMETER (MAXSIZ=1024000)
      COMMON /EXTLIB/ BLOCK(MAXSIZ)
C     changed the type for block as integer as expected by ixlnum C.S. 05/25/2010
      INTEGER BLOCK
      SAVE NUSED
      DATA NUSED /0/
C
************************************************************************
C
C     FORTRAN EXTENSION LIBRARY - ANSI FORTRAN - USER INTERFACE ROUTINE
C
C     DESCRIPTION:
C     This routine requests the operating system to allocate or release
C     numeric storage. A postive MEMREQ indicates a request for memory,
C     while a negative MEMREQ indicates a release. All locations and
C     sizes are measured in numeric storage units.
C
C     In memory request mode, MEMRTN .LT. MEMREQ indicates an error.
C
C     In memory release mode, MEMRTN .LE. -MEMREQ. Furthermore, memory
C     must be released from the top down, i.e., LOCBLK must not change.
C
C     This version actually allocates storage from a static pool, whose
C     size is defined by the parameter MAXSIZ. If system dependent
C     support for the function IXLNUM is not implemented, the PARAMETER
C     and COMMON statements above must be duplicated in the caller.
C
C     FORMAL PARAMETERS:
C     MEMREQ    INTEGER         Number of numeric units
C     LOCBLK    INTEGER         Location of memory block
C     MEMRTN    INTEGER         Size of memory block at routine completion
C
C     COMMON VARIABLES:
C     BLOCK                     Memory pool.
C
C     SAVED VARIABLES:
C     NUSED     INTEGER         Number of units dynamically allocated
C
C     ROUTINES CALLED:
C     IXLNUM    INTEGER         Return location of numeric variable.
C
************************************************************************
C
      IF ( MEMREQ .GE. 0 ) THEN
C
C Allocate storage - 
C change BLOCK into BLOCK(1) - C.S. 5/25/2010
         LOCBLK = IXLNUM( BLOCK(1) ) + NUSED
         MEMRTN = MIN( MAXSIZ-NUSED , MEMREQ )
         NUSED = NUSED + MEMRTN
      ELSE
C
C Only accept free at the top of currently allocated pool -
C change BLOCK into BLOCK(1) - C.S. 5/25/2010
         IF ( LOCBLK - MEMREQ .EQ. IXLNUM( BLOCK(1) ) + NUSED ) THEN
            NUSED = NUSED + MEMREQ
            MEMRTN = 0
         ELSE
            MEMRTN = -MEMREQ
         END IF
      END IF
C
      END
