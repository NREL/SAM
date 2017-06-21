C=======================================================================

C                         *** CAMCON_LIB 2.18 ***

C   --*** CAMCON_LIB *** (CAMCON_LIB) CAMCON general-purpose routines
C   --
C   --Library CAMCON_LIB contains general routines for all CAMCON
C   --programs.  These include routines to give all programs a
C   --common user interface.  CAMCON_LIB must be linked with the
C   --CAMSUPES_LIB library.
C   --
C   --
C   --Documentation:
C   --   WIPP PA User's Manual for CAMCON_LIB.
C   --   Rechard, R.P., ed., 1992.
C   --      Programmer's Reference Manual for CAMCON: Compliance
C   --      Assessment Methodology Controller.  SAND90-1984.
C   --      Albuquerque, NM: Sandia National Laboratories.
C   --
C   --Code authors:          Amy Gilkey, Jonathan S. Rath, David K. Rudeen,
C   --                       Dave W. Osborn, Harold J. Iuzzolino,
C   --                       Bill Mills-Curran
C   --
C   --Code origin:           WIPP Performance Assessment Department 6749
C   --                       Sandia National Laboratories
C   --                       Albuquerque, NM  87185-5800
C   --
C   --Code sponsor:          Amy Gilkey
C   --                       New Mexico Engineering Research Institute
C   --                       (505) 299-1282
C   --
C   --Code consultant:       Amy Gilkey
C   --
C   --Revision History:
C   --   10/28/98  2.18      Handle 10-digit date.  Limit write string length.
C   --                       Fix FILRDNAMS so it handles input file versions
C   --                       and passes text logicals (FILDFNAM too).  QASETUP
C   --                       sets version for non-QA programs.  QAPAGE handles
C   --                       up to 10 units.
C   --   03/xx/98  2.17      Recompiled for OpenVMS 7.1.
C   --   01/30/96  2.16      Program name, version, date taken from 
C   --                       executable, not QASETUP arguments.
C   --   01/08/96  2.15      Deleted FFRDKEY, QAABORTP, and EXNAME ref.
C   --                       Changed date in page header.  Minor changes in
C   --                       several routines.  FILRDNAMS, etc are no longer
C   --                       entry points.
C   --   10/07/95  2.14      Version no longer includes QA level and ZO.
C   --                       Executable information is fetched and printed.
C   --   08/11/95  2.13ZO    Changed EXLOGICAL for input file name in
C   --                       FILDFNAM.
C   --   08/09/95  2.12ZO    Changed QAABORT and FILOPEN to use CAMSUPES_LIB
C   --                       routines.  Call EXLOGICAL for default file name
C   --                       in FILDFNAM.
C   --   06/08/95  2.11ZO    Deleted obsolete entry points.
C   --                       Added prefix to source file names.
C   --   06/01/95  2.10ZO    Added QAHELP and QAOKCANCEL and QACANCEL.
C   --                       Expanded program name and hardware to 16
C   --                       characters.
C   --   04/12/95  2.09ZO    Fixed minor problems with FFPARSE, FFKEYPRM,
C   --                       etc.
C   --   03/30/95  2.08ZO    Fixed minor problem with QAPAGE.
C   --   12/01/94  2.07ZO    Added option to QAPAGE.
C   --   09/24/94  2.06ZO    Separated VAX-specific code.  Created EXCMDLIN
C   --                       in CAMSUPES_LIB.  Deleted unneeded entry points.
C   --                       Moved FILFIELD to CAMSUPES_LIB.  Added FFRDKEYS
C   --                       (to replace FFRDKEY) and FFKEYPRM (to replace
C   --                       FFFNDPRM).  Changed format of QAMESSAG continued
C   --                       line if error.
C   --   06/20/94  2.05ZO    Deleted D_FLOAT double precision.  Changed the
C   --                       machine version code to ZO.
C   --   06/15/94  2.04VV    Changed FILPARSE so that it defaults to current
C   --                       disk if a directory is given.  QA level is now
C   --                       read from a file.
C   --   02/27/94  C-2.03VV  Changed format of error messages.
C   --   11/04/93  C-2.02VV  FILOPEN opens file with large RECL.
C   --   06/28/93  C-2.01VV  FILPARSE handles node name.
C   --   05/28/93  nochange  Added FILECHO2.
C   --   05/02/93  nochange  Spawned processes treated like batch.
C   --   03/15/93  C-2.00VV  Renamed all routines.  Moved FILDAT to
C   --                       CAMSUPES_LIB.  Cleaned up QABANNER if QASETUP
C   --                       not called.  Added execution machine-system.
C   --
C   --   01/10/93  nochange  Change to FFIEQN (undocumented routine).
C   --   12/17/92  C-1.05VV  Added FFIEQN.  Deleted ECHOF and ECHOU.  Added
C   --                       parameter to ECHO.
C   --   10/18/92  C-1.04VV  Added free-field reader routines from SUPES.
C   --   08/19/92  C-1.03VV  Corrected NENUM and IJK for 2D.
C   --   01/30/92  C-1.02VV  Added FILDAT routine.
C   --   10/13/91  C-1.01VV  MEMERR keeps track of number of errors, prints
C   --                       message first time called only.  PRTMSG
C   --                       initializes SPONSR with data.
C   --   05/13/91            Lengthen directory field in FPARSE routine.
C   --   04/02/91            Added UNDFIL routine.  Set default file name
C   --                       to input file name in RDFILN.
C   --   03/21/91            Added UNKNOWN option to OPNFIL.
C   --
C   --                       CAMCON_LIB was derived from GENERIC.
C   --
C   --External software used:
C   --   CAMSUPES_LIB package (dynamic memory, FORTRAN extensions)
C   --
C   --Hardware platform:     DEC Alpha
C   --Software platform:     OpenVMS AXP V7.1
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


