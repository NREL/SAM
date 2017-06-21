C=======================================================================
      program testcam
C=======================================================================

      implicit none

      integer numfld
      integer i
      integer idum
      character*80 line
      character*40 prog

C   --Read the command line to determine which test to run:
      call filcmdlin (1, numfld, line)
      if (numfld .lt. 1)
     &   call qaabort ('File name(s) must be given on command line')
      call filparse ('name', line, ' ', prog)
      i = index (prog, 'test')
      if (i .gt. 1) prog = prog(i:)
      i = index (prog, '_')
      if (i .gt. 1) prog(i:) = ' '
      write (*, 10000, iostat=idum) prog
10000  format (1x, 'Running test case ', a)

      if (prog .eq. 'testfe') then
         call testfe
      else if (prog .eq. 'testff') then
         call testff
      else if (prog .eq. 'testffkey') then
         call testffkey
      else if (prog .eq. 'testfilcmd') then
         call testfilcmd
      else if (prog .eq. 'testfilecho') then
         call testfilecho
      else if (prog .eq. 'testqa') then
         call testqa
      else if (prog .eq. 'teststr') then
         call teststr
      end if

      end

C=======================================================================
      subroutine testfe
C=======================================================================
C
C     CAMCON_LIB test - FE routines
C
C=======================================================================

      implicit none

      integer iunit
      integer ierr
      integer nx, ny, nz
      integer node
      integer npts
      integer i
      integer idum
      real pts(5)
      real vmin, vmax

      integer numfld
      character*80 filesp(1)
      character*80 outfil

      
      

C   --Read the output file name from the command line

      call filcmdlin (1, numfld, filesp)
      if (numfld .lt. 1)
     &   call qaabort ('File name(s) must be given on command line')
      outfil = filesp(1)

C   --Open the output file

      iunit = 10
      call filopen (iunit, 'out', 'form', outfil, ierr)
      if (ierr .ne. 0) call qaabort ('Output file cannot be opened')
      call qamessag (iunit, 'center',
     &   '*** Test for CAMCON_LIB FE Routines ***')
      call qamessag (iunit, 'center', outfil)

C   --Test the IJK routines

      nx = 40
      ny = 25
      nz = 3

C [1]
      node = 10.5 * nx
      call tfe (1, iunit, nx, ny, nz, node)

C [2]
      node = 10.5 * nx
      call tfe (2, iunit, nx, ny, 0, node)

C [3]
      node = nx * ny + 1
      call tfe (3, iunit, nx, ny, nz, node)

C [4]
      node = nx * ny + 3.5 * nx
      call tfe (4, iunit, nx, ny, nz, node)

C   --Test node on right border (not first in any element)
C [5]
      node = 4 * nx
      call tfe (5, iunit, nx, ny, nz, node)

C   --Test node too large
C [6]
      node = nx * ny * nz + 1
      call tfe (6, iunit, nx, ny, nz, node)

C   --Test node too small
C [7]
      node = 0
      call tfe (7, iunit, nx, ny, nz, node)

C   --Test bad NX, NY, NZ
C [8]
      nx = 40
      ny = -25
      nz = 0
      node = 10.5 * nx
      call tfe (8, iunit, nx, ny, nz, node)

C   --Test FEMINMAX

      write (iunit, *)
C [9]
      npts = 5
      pts(1) = 15.0
      pts(2) = -2.0
      pts(3) = 105.0
      pts(4) = 15.0
      pts(5) = -30.0
      write (iunit, 10000, iostat=idum) 9, 'FEMINMAX', npts
      if (npts .gt. 0)
     &   write (iunit, 10010, iostat=idum) (pts(i), i=1,npts)
      call FEMINMAX (npts, pts, VMIN, VMAX)
      write (iunit, 10020, iostat=idum) vmin, vmax

C [10]
      npts = 0
      write (iunit, 10000, iostat=idum) 10, 'FEMINMAX', npts
      if (npts .gt. 0)
     &   write (iunit, 10030, iostat=idum) (pts(i), i=1,npts)
      call FEMINMAX (0, pts, VMIN, VMAX)
      write (iunit, 10040, iostat=idum) vmin, vmax

      call qamessag (iunit, '+center',
     &   '*** End of Test ***')

10000  format (1x, '[', i2, '] Call ', a, '  with', 4i6)
10010  format (5x, 5f10.3)
10020  format (1x, 5x, 'It returned', 5f10.3)
10030  format (5x, 5e14.3)
10040  format (1x, 5x, 'It returned', 5e14.3)
      end

C=======================================================================
      subroutine tfe (icall, iunit, nx, ny, nz, node)

C   --Subroutine tfe is internal to testfe.

      implicit none

      integer icall
      integer iunit
      integer nx, ny, nz
      integer node

      integer ielem
      integer i, j, k
      integer n
      integer idum

      write (iunit, *)
      write (iunit, 10000, iostat=idum) icall,
     &   'FEELEM', node, nx, ny, nz
      call FEELEM (node, nx, ny, nz, IELEM)
      write (iunit, 10010, iostat=idum) ielem

      write (iunit, 10000, iostat=idum) icall,
     &   'FEIJK', node, nx, ny, nz
      call FEIJK (node, nx, ny, nz, I, J, K)
      write (iunit, 10010, iostat=idum) i, j, k

      if ((i+j+k .le. 0) .and. (node .gt. 0)) then
         i = nx+1
         j = ny+1
         k = nz
      end if
      write (iunit, 10000, iostat=idum) icall,
     &   'FENENUM', i, j, k, nx, ny, nz
      call FENENUM (i, j, k, nx, ny, nz, N)
      write (iunit, 10010, iostat=idum) n

      return
10000  format (1x, '[', i2, '] Call ', a, '  with', 10i6)
10010  format (1x, 5x, 'It returned', 10i6)
      end

C=======================================================================
      subroutine testff
C=======================================================================
C
C     CAMCON_LIB test - FF routines except keyword routines
C
C=======================================================================

      implicit none
      integer icall

      integer istrlen

      integer maxfld
      parameter (maxfld = 10)

      integer intyp(maxfld)
      character*20 cfield(maxfld)
      integer ifield(maxfld)
      real rfield(maxfld)
      double precision dfield(maxfld)
      integer iunit
      integer inunit
      integer ierr
      integer idum
      integer iostat
      integer numfld
      integer i
      integer iloop
      integer idcont
      character cmtch, contch
      character*80 retlin

      character*80 filesp(2)
      character*80 outfil, infil

C   --Read the input file name and the output file name from the command line

      call filcmdlin (2, numfld, filesp)
      if (numfld .lt. 2)
     &   call qaabort ('File name(s) must be given on command line')
      outfil = filesp(1)
      infil = filesp(2)

C   --Open the output file

      iunit = 10
      call filopen (iunit, 'out', 'form', outfil, ierr)
      if (ierr .ne. 0) call qaabort ('Output file cannot be opened')
      call qamessag (iunit, 'center',
     &   '*** Test for CAMCON_LIB FF Routines ***')
      call qamessag (iunit, 'center', outfil)

C   --Open the input file

      inunit = 20
      call filopen (inunit, 'in', 'form', infil, ierr)
      if (ierr .ne. 0) call qaabort ('Input file does not exist')

C   --Read and parse the command line

      write (iunit, *)
C [1]
      write (iunit, 10020, iostat=idum) 1, 'FFRDFLDS',
     &   -1, iunit, maxfld
      call FFRDFLDS (-1, iunit, 'command line> ', maxfld,
     &   IOSTAT, NUMFLD, INTYP, CFIELD, IFIELD, RFIELD)
      write (iunit, 10040, iostat=idum) iostat, numfld
      do 100 i = 1, min (numfld, maxfld)
         write (iunit, 10070, iostat=idum)
     &      i, intyp(i), cfield(i), ifield(i), rfield(i)
  100 continue

C   --Check comment and continuation character

      write (iunit, *)
C [2]
      write (iunit, 10010, iostat=idum) 2, 'FFGETCMT'
      call FFGETCMT (CMTCH, CONTCH)
      write (iunit, 10050, iostat=idum) cmtch, contch

C   --Read and parse lines read from the input file

      write (iunit, *)
      call qamessag (iunit, 'center',
     &   '*** Read and parse lines ***')
      icall = 10-1  !just start at a number not used

      do 120 iloop = 1, 100
         write (iunit, *)
         icall = icall + 1
         write (iunit, 10020, iostat=idum) icall, 'FFRDFLDS',
     &      inunit, iunit, maxfld
         call FFRDFLDS (inunit, iunit, 'prompt>', maxfld,
     &      IOSTAT, NUMFLD, INTYP, CFIELD, IFIELD, RFIELD)
         write (iunit, 10040, iostat=idum) iostat, numfld
         if (iostat .lt. 0) goto 130
         do 110 i = 1, min (numfld, maxfld)
            write (iunit, 10070, iostat=idum)
     &         i, intyp(i), cfield(i), ifield(i), rfield(i)
  110    continue

         write (iunit, 10010, iostat=idum) icall, 'FFGETLIN'
         call FFGETLIN (RETLIN)
         write (iunit, 10060, iostat=idum) retlin(:istrlen(retlin))
  120 continue

C   --Read and parse lines read from the input file (double precision)

  130 continue
      rewind inunit
      write (iunit, 10000, iostat=idum)
      call qamessag (iunit, 'center',
     &   '*** Read and parse lines (double precision) ***')
      icall = 10-1  !start at the same number as above

      do 150 iloop = 1, 100
         write (iunit, *)
         icall = icall + 1
         write (iunit, 10020, iostat=idum) icall, 'FFRDFLDg',
     &      inunit, iunit, maxfld
         call FFRDFLDg (inunit, iunit, 'prompt>', maxfld,
     &      IOSTAT, NUMFLD, INTYP, CFIELD, IFIELD, DFIELD)
         write (iunit, 10040, iostat=idum) iostat, numfld
         if (iostat .lt. 0) goto 160
         do 140 i = 1, min (numfld, maxfld)
            write (iunit, 10070, iostat=idum)
     &         i, intyp(i), cfield(i), ifield(i), dfield(i)
  140    continue
  150 continue

C   --Read lines read from the input file and apply various parsing routines

  160 continue
      rewind inunit
      write (iunit, 10000, iostat=idum)
      call qamessag (iunit, 'center',
     &   '*** Read lines, then parse in various ways ***')

      idcont = 0
      do 200 iloop = 1, 100
         write (iunit, *)
         icall = icall + 1
         write (iunit, 10020, iostat=idum) icall, 'FFRDLINE',
     &      inunit, iunit
         call FFRDLINE (inunit, iunit, 'prompt>',
     &      RETLIN, IOSTAT)
         write (iunit, 10040, iostat=idum) iostat
         write (iunit, 10060, iostat=idum) retlin(:istrlen(retlin))
         if (iostat .lt. 0) goto 210

         idcont = 0
         write (iunit, 10020, iostat=idum) icall, 'FFPARSE',
     &      maxfld, idcont
         write (iunit, 10030, iostat=idum) retlin(:istrlen(retlin))
         call FFPARSE (retlin, maxfld, idcont,
     &      NUMFLD, INTYP, CFIELD, IFIELD, RFIELD)
         write (iunit, 10040, iostat=idum) idcont, numfld
         do 170 i = 1, min (numfld, maxfld)
            write (iunit, 10070, iostat=idum)
     &         i, intyp(i), cfield(i), ifield(i), rfield(i)
  170    continue

         idcont = 0
         write (iunit, 10020, iostat=idum) icall, 'FFPARSEg',
     &      maxfld, idcont
         write (iunit, 10030, iostat=idum) retlin(:istrlen(retlin))
         call FFPARSEg (retlin, maxfld, IDCONT,
     &      NUMFLD, INTYP, CFIELD, IFIELD, DFIELD)
         write (iunit, 10040, iostat=idum) idcont, numfld
         do 180 i = 1, min (numfld, maxfld)
            write (iunit, 10070, iostat=idum)
     &         i, intyp(i), cfield(i), ifield(i), dfield(i)
  180    continue

         idcont = 0
         write (iunit, 10020, iostat=idum) icall, 'FFPAREQN',
     &      maxfld, idcont
         write (iunit, 10030, iostat=idum) retlin(:istrlen(retlin))
         call FFPAREQN (.false., retlin, maxfld, IDCONT,
     &      NUMFLD, INTYP, CFIELD, IFIELD, RFIELD)
         write (iunit, 10040, iostat=idum) idcont, numfld
         do 190 i = 1, min (numfld, maxfld)
            write (iunit, 10070, iostat=idum)
     &         i, intyp(i), cfield(i), ifield(i), rfield(i)
  190    continue
  200 continue

  210 continue

      call qamessag (iunit, '+center',
     &   '*** End of Test ***')

10000  format ('1')
10010  format (1x, '[', i2, '] Call ', a)
10020  format (1x, '[', i2, '] Call ', a, '  with', 4i6)
10030  format (1x, 5x, '|', a)
10040  format (1x, 5x, 'It returned', 4i6)
10050  format (1x, 5x, 'It returned',
     &   '  comment = ', a, '  continuation = ', a)
10060  format (1x, 5x, 'It returned', 2x, '|', a)
10070  format (1x, 5x, i4, i5, 3x, a, i8,1pe18.8)
      end

C=======================================================================
      subroutine testffkey
C=======================================================================
C
C     CAMCON_LIB test - FF keyword routines
C
C=======================================================================

      implicit none
      integer icall

      integer maxprm
      parameter (maxprm = 5)
      integer maxval
      parameter (maxval = 10)

      character*8 keywrd
      integer numprm
      character*8 cparam(maxprm)
      integer numval(maxprm)
      integer ixval(maxprm)
      integer ityval(maxval)
      character*8 cvalue(maxval)
      integer ivalue(maxval)
      real rvalue(maxval)

      integer iunit
      integer inunit
      integer iloop
      integer iostat
      integer idum
      integer iprm
      integer i
      integer inpos
      integer nparam
      integer ix
      integer ierr
      character*8 inpar

      integer numfld
      character*80 filesp(2)
      character*80 outfil, infil
      integer, dimension(:), allocatable :: IXTEST

      icall = 0

      allocate(ixtest(numfld))
      ixtest(1)=-999

C   --Read the input file name and the output file name from the command line

      call filcmdlin (2, numfld, filesp)
      if (numfld .lt. 2)
     &   call qaabort ('File name(s) must be given on command line')
      outfil = filesp(1)
      infil = filesp(2)

C   --Open the output file

      iunit = 10
      call filopen (iunit, 'out', 'form', outfil, ierr)
      if (ierr .ne. 0) call qaabort ('Output file cannot be opened')
      call qamessag (iunit, 'center',
     &   '*** Test for CAMCON_LIB FF Keyword Routines ***')
      call qamessag (iunit, 'center', outfil)

C   --Open the input file

      inunit = 20
      call filopen (inunit, 'in', 'form', infil, ierr)
      if (ierr .ne. 0) call qaabort ('Input file cannot be opened')

C   --Read lines read from the input file and parse in keyword format (FFRDKEYS)

      do 120 iloop = 1, 100
         write (iunit, *)
         icall = icall + 1
         write (iunit, 10010, iostat=idum) icall, 'FFRDKEYS',
     &      inunit, iunit, maxprm, maxval
         call FFRDKEYS (inunit, iunit, 'keyword> ',
     &      IOSTAT, KEYWRD, maxprm, NUMPRM, CPARAM,
     &      maxval, NUMVAL, IXVAL, ITYVAL, CVALUE, IVALUE, RVALUE)
         write (iunit, 10020, iostat=idum) iostat
         if (iostat .lt. 0) goto 130

         write (iunit, 10030, iostat=idum) keywrd, numprm
         do 110 iprm = 1, max (1, numprm)
            write (iunit, 10040, iostat=idum) iprm,
     &         cparam(iprm), numval(iprm), ixval(iprm)
            do 100 i = 1, numval(iprm)
               ix = ixval(iprm) + i - 1
               write (iunit, 10050, iostat=idum) ix, ityval(ix),
     &            cvalue(ix), ivalue(ix), rvalue(ix)
  100       continue
  110    continue
  120 continue

  130 continue

C   --Retrieve parameters from the parsed line

      write (iunit, *)
      do 140 i = 1, maxprm
         ixval(i) = (i-1) * 3 + 1
  140 continue
      cparam(1) = 'PAR_ABC'
      cparam(2) = 'PAR_DEF'
      cparam(3) = 'PAR_A'
      cparam(4) = 'PAR_B'
      cparam(5) = 'PAR_C'
      numprm = 5

C   --FFKEYPRM with exact match
C [80]
      ix = -99
      inpar = 'PAR_A'
      inpos = 0
      write (iunit, 10000, iostat=idum) 80, 'FFKEYPRM',
     &   inpar, inpos, numprm, ixtest(1)
      call FFKEYPRM (inpar, inpos, numprm, cparam, ixtest,
     &   NPARAM, IX)
      write (iunit, 10020, iostat=idum) nparam, ix
      if (nparam .ge. 1)
     &   write (iunit, 10060, iostat=idum) nparam, cparam(nparam)

C   --FFKEYPRM with abbreviated match
C [81]
      ix = -99
      inpar = 'PAR_CABR'
      inpos = 2
      write (iunit, 10000, iostat=idum) 81, 'FFKEYPRM',
     &   inpar, inpos, numprm, ixtest(1)
      call FFKEYPRM (inpar, inpos, numprm, cparam, ixtest,
     &   NPARAM, IX)
      write (iunit, 10020, iostat=idum) nparam, ix
      if (nparam .ge. 1)
     &   write (iunit, 10060, iostat=idum) nparam, cparam(nparam)

C   --FFKEYPRM with more than one match
C [82]
      ix = -99
      inpar = 'PAR'
      inpos = 2
      write (iunit, 10000, iostat=idum) 82, 'FFKEYPRM',
     &   inpar, inpos, numprm, ixtest(1)
      call FFKEYPRM (inpar, inpos, numprm, cparam, ixtest,
     &   NPARAM, IX)
      write (iunit, 10020, iostat=idum) nparam, ix
      if (nparam .ge. 1)
     &   write (iunit, 10060, iostat=idum) nparam, cparam(nparam)

C   --FFKEYPRM with no match (lower case)
C [83]
      ix = -99
      inpar = 'par_a'
      inpos = 0
      write (iunit, 10000, iostat=idum) 83, 'FFKEYPRM',
     &   inpar, inpos, numprm, ixtest(1)
      call FFKEYPRM (inpar, inpos, numprm, cparam, ixtest,
     &   NPARAM, IX)
      write (iunit, 10020, iostat=idum) nparam, ix
      if (nparam .ge. 1)
     &   write (iunit, 10060, iostat=idum) nparam, cparam(nparam)

C   --FFKEYPRM with positional parameter
C [84]
      ix = -99
      inpar = ' '
      inpos = 2
      write (iunit, 10000, iostat=idum) 84, 'FFKEYPRM',
     &   inpar, inpos, numprm, ixval(1)
      call FFKEYPRM (inpar, inpos, numprm, cparam, ixval,
     &   NPARAM, IX)
      write (iunit, 10020, iostat=idum) nparam, ix
      if (nparam .ge. 1)
     &   write (iunit, 10060, iostat=idum) nparam, cparam(nparam)

C   --FFKEYPRM with positional parameter (parameters all blank)
      do 150 i = 1, numprm
         cparam(i) = ' '
  150 continue
C [85]
      ix = -99
      inpar = 'THISONE'
      inpos = 2
      write (iunit, 10000, iostat=idum) 85, 'FFKEYPRM',
     &   inpar, inpos, numprm, ixval(1)
      call FFKEYPRM (inpar, inpos, numprm, cparam, ixval,
     &   NPARAM, IX)
      write (iunit, 10020, iostat=idum) nparam, ix
      if (nparam .ge. 1)
     &   write (iunit, 10060, iostat=idum) nparam, cparam(nparam)

C   --FFKEYPRM with positional parameter too big
C [86]
      ix = -99
      inpar = ' '
      inpos = numprm + 1
      write (iunit, 10000, iostat=idum) 86, 'FFKEYPRM',
     &   inpar, inpos, numprm, ixval(1)
      call FFKEYPRM (inpar, inpos, numprm, cparam, ixval,
     &   NPARAM, IX)
      write (iunit, 10020, iostat=idum) nparam, ix
      if (nparam .ge. 1)
     &   write (iunit, 10060, iostat=idum) nparam, cparam(nparam)

      call qamessag (iunit, '+center',
     &   '*** End of Test ***')

10000  format (1x, '[', i2, '] Call ', a, '  with  ', a, 4i6)
10010  format (1x, '[', i2, '] Call ', a, '  with', 4i6)
10020  format (1x, 5x, 'It returned', 4i6)
10030  format (1x, 5x, 'Keyword = ', a, i5, ' parameters')
10040  format (1x, 5x, 3x, 'Parameter', i3, ':',
     &   3x, a, i5, ' values', :, i5, ' index')
10050  format (1x, 5x, 3x, 3x, 'Field', i3, ':',
     &   i7, 3x, a, i8, f16.6)
10060  format (1x, 5x, 'Parameter', i3, ' output as ', a)
      end

C=======================================================================
      subroutine testfilcmd
C=======================================================================
C
C     CAMCON_LIB test - FIL Command Line routines
C
C=======================================================================

      implicit none

      integer istrlen
      logical QABATCH

      integer iunit
      integer ierr
      integer idum
      integer numfld
      integer i
      integer iuncmd
      logical lqa
      character*80 filesp(10)
      character*80 fildsc, inout, optreq, defspc
      character*80 outfil, cmdfil
      character cdum

      call qasetup ('DRIVER', '0.00', '04/25/01',
     &   'Amy Author', 'Sally Sponsor')

C   --Read the input file name and the output file name from the command line

      call filcmdlin (2, numfld, filesp)
      if (numfld .lt. 2)
     &   call qaabort ('File name(s) must be given on command line')
      outfil = filesp(1)
      cmdfil = filesp(2)

C   --Open the output file

      iunit = 10
      call filopen (iunit, 'out', 'form', outfil, ierr)
      if (ierr .ne. 0) call qaabort ('Output file cannot be opened')
      call qamessag (iunit, 'center',
     &   '*** Test for CAMCON_LIB FIL Command Line Routines ***')
      call qamessag (iunit, 'center', outfil)

C   --Check the QABATCH flag

      write (iunit, *)
C [1]
      write (iunit, 10000, iostat=idum) 1, 'QABATCH'
      LQA = QABATCH ()
      write (iunit, 10040, iostat=idum) lqa

C   --Read the command line

      write (iunit, *)
C [2]
      write (iunit, 10010, iostat=idum) 2, 'FILCMDLIN', 6
      call FILCMDLIN (10, NUMFLD, FILESP)
      write (iunit, 10030, iostat=idum) numfld
      do 100 i = 1, numfld
         write (iunit, 10050, iostat=idum)
     &      filesp(i)(:istrlen(filesp(i)))
  100 continue

C   --Define the files to be read from the command lines

      numfld = 0
      write (iunit, *)
C [3]
      numfld = numfld + 1
      write (iunit, 10020, iostat=idum) 3, 'FILDFNAM',
     &   'output file', 'out', 'req', ' '
      call FILDFNAM ('output file', 'out', 'req', ' ')
C [4]
      numfld = numfld + 1
      write (iunit, 10020, iostat=idum) 4, 'FILDFNAM',
     &   'command file', 'out', 'req', 'CMDFILE.DAT'
      call FILDFNAM ('command file', 'out', 'req', 'CMDFILE.DAT')
C [5]
      numfld = numfld + 1
      write (iunit, 10020, iostat=idum) 5, 'FILDFNAM',
     &   'input file', 'in', 'req', 'LOGICAL$DEFAULT'
      call FILDFNAM ('input file', 'in', 'req', 'LOGICAL$DEFAULT')
C [6]
      numfld = numfld + 1
      write (iunit, 10020, iostat=idum) 6, 'FILDFNAM',
     &   'text input', 'text', 'req', 'TEXT'
      call FILDFNAM ('text input', 'text', 'req', 'TEXT')
C [7]
      numfld = numfld + 1
      write (iunit, 10020, iostat=idum) 7, 'FILDFNAM',
     &   'optional input file', 'in', 'opt', 'OPTFILE.DAT'
      call FILDFNAM ('optional input file', 'in', 'opt', 'OPTFILE.DAT')
C [8]
      numfld = numfld + 1
      write (iunit, 10020, iostat=idum) 8, 'FILDFNAM',
     &   'diagnostics file', 'out', 'opt', '.DBG'
      call FILDFNAM ('diagnostics file', 'out', 'opt', '.DBG')

C   --Get the file name definitions before the files are read

      write (iunit, *)
C [9] [10] [11]
      do 110 i = 1, 3
         write (iunit, 10000, iostat=idum) 9+i-1, 'FILGTNAMS'
         call FILGTNAMS (FILDSC, INOUT, OPTREQ, DEFSPC)
         write (iunit, 10060, iostat=idum)
     &      'FileDesc', fildsc(:istrlen(fildsc))
         write (iunit, 10060, iostat=idum)
     &      'InOut   ', inout(:istrlen(inout))
         write (iunit, 10060, iostat=idum)
     &      'Optional', optreq(:istrlen(optreq))
         write (iunit, 10060, iostat=idum)
     &      'FileName', defspc(:istrlen(defspc))
  110 continue

C   --Read the file names

      write (iunit, *)
C [12]
      write (iunit, 10000, iostat=idum) 12, 'FILRDNAMS'
      call FILRDNAMS (FILESP, IERR)
      write (iunit, 10030, iostat=idum) ierr
      do 120 i = 1, numfld
         write (iunit, 10050, iostat=idum)
     &      filesp(i)(:istrlen(filesp(i)))
  120 continue

C   --Write the file names to the output file

      write (iunit, *)
C [13]
      write (iunit, 10010, iostat=idum) 13, 'FILWRNAMS', iunit
      call FILWRNAMS (iunit, cdum)

C   --Get the file name definitions after the files are read

      write (iunit, *)
C [14] [15] [16] [17] [18] [19] [20]
      do 130 i = 1, numfld+1
         write (iunit, 10000, iostat=idum) 14+i-1, 'FILGTNAMS'
         call FILGTNAMS (FILDSC, INOUT, OPTREQ, DEFSPC)
         write (iunit, 10060, iostat=idum)
     &      'FileDesc', fildsc(:istrlen(fildsc))
         write (iunit, 10060, iostat=idum)
     &      'InOut   ', inout(:istrlen(inout))
         write (iunit, 10060, iostat=idum)
     &      'Optional', optreq(:istrlen(optreq))
         write (iunit, 10060, iostat=idum)
     &      'FileName', defspc(:istrlen(defspc))
  130 continue

C   --Write a command file

      write (iunit, *)
      iuncmd = 20
C [21]
      write (iunit, 10020, iostat=idum) 21, 'FILWRCMD', cmdfil
      call FILWRCMD (iuncmd, cmdfil)
      close (iuncmd)

C   --Echo the command file to the output file

C [22]
      write (iunit, 10020, iostat=idum) 22, 'FILOPEN',
     &   'IN', 'FORM', cmdfil(:istrlen(cmdfil))
      call FILOPEN (iuncmd, 'IN', 'FORM', cmdfil, IERR)
      write (iunit, 10030, iostat=idum) ierr

C [23]
      write (iunit, 10020, iostat=idum) 23, 'FILECHO', 'page'
      call FILECHO (iuncmd, iunit, 'page')
      close (iuncmd)

      call qamessag (iunit, '+center',
     &   '*** End of Test ***')

10000  format (1x, '[', i2, '] Call ', a)
10010  format (1x, '[', i2, '] Call ', a, '  with', 4i6)
10020  format (1x, '[', i2, '] Call ', a, '  with', 4 (2x, a))
10030  format (1x, 5x, 'It returned', 4i6)
10040  format (1x, 5x, 'It returned  ', l1)
10050  format (1x, 5x, 3x, '|', a)
10060  format (1x, 5x, 3x, a, 2x, '|', a)
      END

C=======================================================================
      subroutine testfilecho
C=======================================================================
C
C     CAMCON_LIB test - FIL Echo and Open and Parse routines
C
C=======================================================================

      implicit none

      integer istrlen

      integer iunit
      integer iunecho
      integer iunout
      integer iuntmp
      integer ierr
      integer idum
      character*80 line

      integer numfld
      character*80 filesp(4)
      character*80 outfil, infil, ech1fil, ech2fil, fname

C   --Read the input file name and the output file names from the command line

      call filcmdlin (4, numfld, filesp)
      if (numfld .lt. 4)
     &   call qaabort ('File name(s) must be given on command line')
      outfil = filesp(1)
      infil = filesp(2)
      ech1fil = filesp(3)
      ech2fil = filesp(4)

C   --Open the output file

      iunit = 10
      call filopen (iunit, 'out', 'form', outfil, ierr)
      if (ierr .ne. 0) call qaabort ('Output file cannot be opened')
      call qamessag (iunit, 'center',
     &   '*** Test for CAMCON_LIB FIL Echo and Parse Routines ***')
      call qamessag (iunit, 'center', outfil)
      write (iunit, *)

C   --Open the input file (for echo)

      iunecho = 20
C [1]
      write (iunit, 10010, iostat=idum) 1, 'FILOPEN',
     &   'IN', 'FORM', infil(:istrlen(infil))
      call FILOPEN (iunecho, 'IN', 'FORM', infil, IERR)
      if (ierr .ne. 0) call qaabort ('Input file cannot be opened')
      write (iunit, 10030, iostat=idum) ierr

C   --Open the first echo file and echo to it

      iunout = 30
C [2]
      write (iunit, 10010, iostat=idum) 2, 'FILOPEN',
     &   'OUT', 'FORM', ech1fil(:istrlen(ech1fil))
      call FILOPEN (iunout, 'OUT', 'FORM', ech1fil, IERR)
      write (iunit, 10030, iostat=idum) ierr

C [3]
      write (iunout, 10040, iostat=idum) 3, 'FILECHO'
      write (iunit, 10000, iostat=idum) 3, 'FILECHO'
      call FILECHO (iunecho, iunout, 'page')
      close (iunout)

C   --Open the second echo file and echo to it

C [4]
      write (iunit, 10010, iostat=idum) 4, 'FILOPEN',
     &   'OUT', 'LIST', ech2fil(:istrlen(ech2fil))
      call FILOPEN (iunout, 'OUT', 'LIST', ech2fil, IERR)
      write (iunit, 10030, iostat=idum) ierr

C [5]
      write (iunout, 10040, iostat=idum) 5, 'FILECHO2'
      write (iunit, 10000, iostat=idum) 5, 'FILECHO2'
      call FILECHO2 (iunecho, iunout, ' ')
      rewind (iunecho)

C [6]
      write (iunout, 10040, iostat=idum) 6, 'FILECHO'
      write (iunit, 10000, iostat=idum) 6, 'FILECHO'
      call FILECHO (iunecho, iunout, ' ')
      close (iunecho)

C [7]
C   --Note that IUNECHO was closed above
      write (iunout, 10040, iostat=idum) 7, 'FILECHO'
      write (iunit, 10000, iostat=idum) 7, 'FILECHO'
      call FILECHO (iunecho, iunout, ' ')
      close (iunout)

C   --Check file opening; unformatted

      iuntmp = 31

      write (iunit, *)
      fname = 'camlib_testfilecho.unform'
C [8]
      write (iunit, 10010, iostat=idum) 8, 'FILOPEN',
     &   'out', 'unform', fname
      call FILOPEN (iuntmp, 'out', 'unform', fname, IERR)
      write (iunit, 10030, iostat=idum) ierr
      write (iuntmp, iostat=idum)
     &   'Test of FILOPEN UNformatted'
      close (iuntmp)

C [9]
      write (iunit, 10010, iostat=idum) 9, 'FILOPEN',
     &   'in', 'unform', fname
      call FILOPEN (iuntmp, 'in', 'unform', fname, IERR)
      write (iunit, 10030, iostat=idum) ierr
      read (iuntmp, iostat=idum) line
      write (iunit, 10020, iostat=idum) line(:istrlen(line))
      close (iuntmp)

C   --Check file opening; scratch

      write (iunit, *)
      fname = 'camlib_testfilecho.scratch'
C [10]
      write (iunit, 10010, iostat=idum) 10, 'FILOPEN',
     &   'scr', 'form', fname
      call FILOPEN (iuntmp, 'scr', 'form', fname, IERR)
      write (iunit, 10030, iostat=idum) ierr
      write (iuntmp, '(1x, a)', iostat=idum)
     &   'Test of FILOPEN scratch'
      rewind (iuntmp)
      read (iuntmp, '(a)', iostat=idum) line
      write (iunit, 10020, iostat=idum) line(:istrlen(line))
      close (iuntmp)

C   --Scratch file should have been deleted; expect error
C [11]
      write (iunit, 10010, iostat=idum) 11, 'FILOPEN',
     &   'in', 'form', fname
      call FILOPEN (iuntmp, 'in', 'form', fname, IERR)
      write (iunit, 10030, iostat=idum) ierr
      close (iuntmp, iostat=idum)

C   --Check FILPARSE with various file names and defaults

C [12]
      call tparse (12, iunit, 'testfil.out',
     &   'node::disk:[main][dir.sub]name.ext;1')
C [13]
      call tparse (13, iunit, 'x$node::disk:[main][dir.sub]name.ext;3',
     &   'testfil.out')
C [14]
      call tparse (14, iunit, '[]testfil.out', 'logdir:name.ext')
C [15]
      call tparse (15, iunit, 'logdir:name.ext', '[]testfil.out')
C [16]
      call tparse (16, iunit, 'ldir:testfil.out', 'logdir:name.ext')
C [17]
      call tparse (17, iunit, 'testfil.out', 'logdir:name.ext;3')
C [18]
      call tparse (18, iunit, 'logdir:name.ext;3', 'testfil.out')
C [19]
      call tparse (19, iunit, 'testfil.out', ' ')
C [20]
      call tparse (20, iunit, ' ', 'testfil.out;3')

      call qamessag (iunit, '+center',
     &   '*** End of Test ***')

10000  format (1x, '[', i2, '] Call ', a)
10010  format (1x, '[', i2, '] Call ', a, '  with', 4 (2x, a))
10020  format (1x, 5x, 'Line:', 2x, '|', a)
10030  format (1x, 5x, 'It returned', 4i6)
10040  format (1x, '[', i2, '] The output below was written by ', a)
      END

      subroutine tparse (icall, iunit, innam, defnam)

C   --Subroutine tparse is internal to testfilecho.

      implicit none

      integer istrlen

      integer icall
      integer iunit
      character*(*) innam, defnam

      character*80 name
      character*10 type
      integer idum

      write (iunit, *)
      write (iunit, 10000, iostat=idum) icall, 'FILPARSE'
      type = 'INPUT'
      write (iunit, 10020, iostat=idum)
     &   type, innam(:istrlen(innam))
      type = 'DEFAULT'
      write (iunit, 10020, iostat=idum)
     &   type, defnam(:istrlen(defnam))
      write (iunit, 10010, iostat=idum)

      type = ' '
      call FILPARSE (type, innam, defnam, NAME)
      write (iunit, 10020, iostat=idum)
     &   type, name(:istrlen(NAME))
      type = 'NODE'
      call FILPARSE (type, innam, defnam, NAME)
      write (iunit, 10020, iostat=idum)
     &   type, name(:istrlen(name))
      type = 'Disk'
      call FILPARSE (type, innam, defnam, NAME)
      write (iunit, 10020, iostat=idum)
     &   type, name(:istrlen(name))
      type = 'Directory'
      call FILPARSE (type, innam, defnam, NAME)
      write (iunit, 10020, iostat=idum)
     &   type, name(:istrlen(name))
      type = 'Name'
      call FILPARSE (type, innam, defnam, NAME)
      write (iunit, 10020, iostat=idum)
     &   type, name(:istrlen(name))
      type = 'Ext'
      call FILPARSE (type, innam, defnam, NAME)
      write (iunit, 10020, iostat=idum)
     &   type, name(:istrlen(name))
      type = 'Version'
      call FILPARSE (type, innam, defnam, NAME)
      write (iunit, 10020, iostat=idum)
     &   type, name(:istrlen(name))
      type = 'BAD'
      call FILPARSE (type, innam, defnam, NAME)
      write (iunit, 10020, iostat=idum)
     &   type, name(:istrlen(name))

10000  format (1x, '[', i2, '] Call ', a)
10010  format (1x, 5x, 'Successive calls returned:')
10020  format (1x, 5x, 3x, a, '=  |', a)
      END

C=======================================================================
      subroutine testqa
C=======================================================================
C
C     CAMCON_LIB test - QA routines
C
C=======================================================================

      implicit none

      logical QABATCH, QAMAXERR, QAMEMERR
      logical QAOKCANCEL, QACANCEL, QAHELP
      integer IQAERRUNI

      logical lqa
      integer iunit
      integer idum
      integer i
      integer nqa
      integer ierr
      integer kabc
      integer rmem(1)
      real cpusec
      real rdum1, rdum2
      character*8 tvers, tdate, trdat, trtim
      character*32 tprog, tauth, tspon
      character*8 qarec(4)

      integer numfld
      character*80 filesp(1)
      character*80 outfil

C   --Read the output file name from the command line

      call filcmdlin (1, numfld, filesp)
      if (numfld .lt. 1)
     &   call qaabort ('File name(s) must be given on command line')
      outfil = filesp(1)

C   --Open the output file

      iunit = 10
      call filopen (iunit, 'out', 'form', outfil, ierr)
      if (ierr .ne. 0) call qaabort ('Output file cannot be opened')
C [1]
      write (iunit, 10030, iostat=idum) 1, 'QAMESSAG', 'CENTER'
      call QAMESSAG (iunit, 'center',
     &   '*** Test for CAMCON_LIB QA Routines ***')
      call qamessag (iunit, 'center', outfil)

C   --Check the QA setup routines

      write (iunit, *)
C [2]
      write (iunit, 10030, iostat=idum) 2, 'QAPAGE', 'PAGE'
      call QAPAGE (iunit, 'PAGE')

      write (iunit, *)
C [3]
      write (iunit, 10030, iostat=idum) 3, 'QASETUP',
     &   'Amy Author', 'Sally Sponsor'
      call QASETUP (' ', ' ', ' ',
     &   'Amy Author', 'Sally Sponsor')

      write (iunit, *)
C [4]
      write (iunit, 10010, iostat=idum) 4, 'QAFETCH'
      call QAFETCH (TPROG, TVERS, TDATE, TRDAT, TRTIM, TAUTH, TSPON)
      write (iunit, 10080, iostat=idum)
     &   tprog, tvers, tdate
      write (iunit, 10080, iostat=idum)
     &   trdat, trtim
      write (iunit, 10080, iostat=idum) tauth, tspon

      write (iunit, *)
      nqa = 0
C [5]
      write (iunit, 10020, iostat=idum) 5, 'QAMAKREC', nqa
      call QAMAKREC (NQA, QAREC)
      write (iunit, 10060, iostat=idum) nqa
      write (iunit, 10080, iostat=idum) (qarec(i), i=1,4)

      write (iunit, *)
C [6]
      write (iunit, 10010, iostat=idum) 6, 'QABATCH'
      LQA = qabatch ()
      write (iunit, 10050, iostat=idum) lqa

C   --Set the error unit; Most output routines accept unit -1 as the error unit
C   --and unit 0 as the terminal

      write (iunit, *)
C [7]
      write (iunit, 10020, iostat=idum) 7, 'IQAERRUNI', iunit
      IDUM = IQAERRUNI (iunit)
      write (iunit, 10060, iostat=idum) idum

C [8]
      write (iunit, 10020, iostat=idum) 8, 'IQAERRUNI', -1
      i = IQAERRUNI (-1)
      write (iunit, 10060, iostat=idum) i

C   --Check the QA standard output routines

C   --Test that unit 0 outputs to the terminal
      write (iunit, *)
      write (*, 10000, iostat=idum) 'QABIGLET'
C [9]
      write (iunit, 10030, iostat=idum) 9, 'QABIGLET to 0', 'TESTQA'
      call QABIGLET (0, 80, 'TESTQA')

      write (iunit, *)
C [10]
      write (iunit, 10010, iostat=idum) 10, 'QABANNER'
      call QABANNER (iunit,
     &   'This tests all the CAMCON_LIB QA routines.',
     &   'It produces a file called TESTQA.OUT.', ' ')

      write (iunit, *)
C   --Test that unit -1 outputs to the output file set by IQAERRUNI
C [11]
      write (iunit, 10030, iostat=idum) 11, 'QABIGLET to -1',
     &   'QABIGLET'
      call QABIGLET (-1, 80, 'QABIGLET')

      write (iunit, *)
C [12]
      write (iunit, 10030, iostat=idum) 12, 'QADOEDIS', ' '
      call QADOEDIS (iunit, ' ')

      write (iunit, *)
C [13]
      write (iunit, 10030, iostat=idum) 13, 'QAPAGE', ' '
      call QAPAGE (iunit, ' ')

      write (iunit, *)
C [14]
      write (iunit, 10030, iostat=idum) 14, 'QABIGLET',
     &   'LineThatIsTooBig'
      call QABIGLET (iunit, 80, 'LineThatIsTooBig')

      write (iunit, *)
C [15]
      write (iunit, 10030, iostat=idum) 15, 'QADOEDIS to -1', '*'
      call QADOEDIS (-1, '*')

      write (iunit, *)
C [16]
      write (iunit, 10030, iostat=idum) 16, 'QAPAGE to -1', 'page'
      call QAPAGE (-1, 'page')

C   --Check message routines

      write (iunit, *)
C [17]
      write (iunit, 10010, iostat=idum) 17, 'QAPRTSTR'
      call QAPRTSTR (iunit, 'Message from QAPRTSTR')

      write (iunit, *)
C [18]
      write (iunit, 10030, iostat=idum) 18, 'QAMESSAG', '+error'
      call QAMESSAG (iunit, '+error', 'Error message from QAMESSAG')
C [19]
      write (iunit, 10030, iostat=idum) 19, 'QAMESSAG', 'CONT'
      call QAMESSAG (iunit, 'CONT', 'Continue previous message')

      write (iunit, *)
C [20]
      write (iunit, 10020, iostat=idum) 20, 'QAMAXERR', 99, 100
      LQA = QAMAXERR (.false., 'OK', 'MAX', 99, 100)
      write (iunit, 10050, iostat=idum) lqa

      write (iunit, *)
C [21]
      write (iunit, 10020, iostat=idum) 21, 'QAMAXERR', 101, 100
      LQA = QAMAXERR (.false., 'BAD', 'MAX', 101, 100)
      write (iunit, 10050, iostat=idum) lqa

C   --Check memory error routines

      call mdinit (rmem)

C   --Valid memory request
      write (iunit, *)
      call mdrsrv ('abc', kabc, 100)
C [22]
      write (iunit, 10010, iostat=idum) 22, 'QAMEMERR'
      LQA = QAMEMERR (.false.)
      write (iunit, 10050, iostat=idum) lqa

C   --Invalid memory request (array does not exist)
      write (iunit, *)
      call mddel ('def')
C [23]
      write (iunit, 10010, iostat=idum) 23, 'QAMEMERR'
      LQA = QAMEMERR (.false.)
      write (iunit, 10050, iostat=idum) lqa

C   --Valid memory request
      write (iunit, *)
      call mddel ('abc')
C [24]
      write (iunit, 10010, iostat=idum) 24, 'QAMEMERR'
      LQA = QAMEMERR (.false.)
      write (iunit, 10050, iostat=idum) lqa

C   --Check the error unit by writing to unit -1, then changing error unit to 0
C   --and writing again

      write (iunit, *)
C [25]
      write (iunit, 10010, iostat=idum) 25, 'QAPRTSTR to -1'
      call QAPRTSTR (-1, 'Message from QAPRTSTR to -1')

C [26]
      write (iunit, 10020, iostat=idum) 26, 'IQAERRUNI', 0
      idum = IQAERRUNI (0)
      write (iunit, 10060, iostat=idum) idum

C [27]
      write (iunit, 10020, iostat=idum) 27, 'IQAERRUNI', -1
      i = IQAERRUNI (-1)
      write (iunit, 10060, iostat=idum) i

      write (*, 10000, iostat=idum) 'QAPRTSTR'
C [28]
      write (iunit, 10010, iostat=idum) 28, 'QAPRTSTR to -1'
      call QAPRTSTR (-1, 'Message from QAPRTSTR to -1 (defined as 0)')

C [29]
      write (iunit, 10020, iostat=idum) 29, 'IQAERRUNI', iunit
      idum = IQAERRUNI (iunit)
      write (iunit, 10060, iostat=idum) idum

C   --QAOKCANCEL and QACANCEL are uninteresting unless interactive

      write (iunit, *)
C [30]
      write (iunit, 10040, iostat=idum) 30, 'QAOKCANCEL', .true.
      LQA = QAOKCANCEL (.TRUE.)
      write (iunit, 10050, iostat=idum) lqa
C [31]
      write (iunit, 10010, iostat=idum) 31, 'QACANCEL'
      LQA = QACANCEL ()
      write (iunit, 10050, iostat=idum) lqa
C [32]
      write (iunit, 10040, iostat=idum) 32, 'QAOKCANCEL', .false.
      LQA = QAOKCANCEL (.FALSE.)
      write (iunit, 10050, iostat=idum) lqa
C [33]
      write (iunit, 10010, iostat=idum) 33, 'QACANCEL'
      LQA = QACANCEL ()
      write (iunit, 10050, iostat=idum) lqa

C   --QAHELP is uninteresting unless interactive

      write (*, 10000, iostat=idum) 'QAHELP'
C [34]
      write (iunit, 10030, iostat=idum) 34, 'QAHELP',
     &   'BADPROG', 'BADTYPE', 'BADOPTION'
      LQA = QAHELP ('BADPROG', 'BADTYPE', 'BADOPTION')
      write (iunit, 10050, iostat=idum) lqa

      write (*, 10000, iostat=idum) 'QAHELP'
C [35]
      write (iunit, 10030, iostat=idum) 35, 'QAHELP',
     &   'TESTQAHELP', 'SECOND_LEVEL_2', 'FIRST_SUBLEVEL_3'
      LQA = QAHELP ('TESTQAHELP', 'SECOND_LEVEL_2', 'FIRST_SUBLEVEL_3')
      write (iunit, 10050, iostat=idum) lqa

C   --Check QACPUS

      write (iunit, *)
C   --Do a dummy calculation to run up CPU times
      rdum1 = 0
      rdum2 = 0
      do i = 1, 1000000
         rdum1 = rdum1 + sin (real (i))
         rdum2 = rdum2 + sqrt (real (i))
      end do
C [36]
      write (iunit, 10010, iostat=idum) 36, 'QACPUS'
      call QACPUS (iunit, CPUSEC)
      write (iunit, 10070, iostat=idum) cpusec

      write (iunit, *)
C [37]
      write (iunit, 10030, iostat=idum) 37, 'QAPAGE', 'end'
      call QAPAGE (iunit, 'end')

C   --Check QAABORT

      write (iunit, *)
      write (*, 10000, iostat=idum) 'QAABORT'
C [38]
      write (iunit, 10010, iostat=idum) 38, 'QAABORT'
      call QAABORT ('QAABORT this test')

C   --This message should not be printed
      write (iunit, *)
      write (*, 10000, iostat=idum) 'QAMESSAG'
C [39]
      write (iunit, 10030, iostat=idum) 39, 'QAMESSAG', '+CENTER'
      call QAMESSAG (0, '+center',
     &   '*** End of Test ***')
      call QAMESSAG (iunit, '+center',
     &   '*** End of Test ***')

10000  format (/, 1x, '* Calling ', a, ' *')
10010  format (1x, '[', i2, '] Call ', a)
10020  format (1x, '[', i2, '] Call ', a, '  with', 4i6)
10030  format (1x, '[', i2, '] Call ', a, '  with', 6 (2x, a))
10040  format (1x, '[', i2, '] Call ', a, '  with  ', l1)
10050  format (1x, 5x, 'It returned  ', l1)
10060  format (1x, 5x, 'It returned', 4i6)
10070  format (1x, 5x, 'It returned ', 1pe15.8)
10080  format (1x, 5x, 'It returned', 5 (2x, :, '|', a))
      end

C=======================================================================
      subroutine teststr
C=======================================================================
C
C     CAMCON_LIB test - STR routines
C
C=======================================================================

      implicit none

      integer ISTRFIND

      integer iunit
      integer ierr
      integer idum
      integer i
      character*32 str
      character*3 strlist(10)

      integer numfld
      character*80 filesp(1)
      character*80 outfil

      data strlist /
     &   'abc', 'def', 'ghi', 'jkl', 'mno',
     &   'pqr', 'stu', 'vwx', 'yz ', '***' /

C   --Read the output file name from the command line

      call filcmdlin (1, numfld, filesp)
      if (numfld .lt. 1)
     &   call qaabort ('File name(s) must be given on command line')
      outfil = filesp(1)

C   --Open the output file

      iunit = 10
      call filopen (iunit, 'out', 'form', outfil, ierr)
      if (ierr .ne. 0) call qaabort ('Output file cannot be opened')
      call qamessag (iunit, 'center',
     &   '*** Test for CAMCON_LIB STR Routines ***')
      call qamessag (iunit, 'center', outfil)

C   --Test ISTRFIND

      write (iunit, *)
      write (iunit, 10000, iostat=idum) (strlist(i), i=1,10)

C [1]
      str = 'jkl'
      write (iunit, 10010, iostat=idum) 1, 'ISTRFIND', str(:5)
      I = ISTRFIND (str, 10, strlist)
      write (iunit, 10020, iostat=idum) i

C [2]
      str = 'JKL'
      write (iunit, 10010, iostat=idum) 2, 'ISTRFIND', str(:5)
      I = ISTRFIND (str, 10, strlist)
      write (iunit, 10020, iostat=idum) i

C [3]
      str = 'jklm'
      write (iunit, 10010, iostat=idum) 3, 'ISTRFIND', str(:5)
      I = ISTRFIND (str, 10, strlist)
      write (iunit, 10020, iostat=idum) i

C [4]
      str = 'jk'
      write (iunit, 10010, iostat=idum) 4, 'ISTRFIND', str(:5)
      I = ISTRFIND (str, 10, strlist)
      write (iunit, 10020, iostat=idum) i

C [5]
      str = '***'
      write (iunit, 10010, iostat=idum) 5, 'ISTRFIND', str(:5)
      I = ISTRFIND (str, 10, strlist)
      write (iunit, 10020, iostat=idum) i

C   --Test the other string routines

c            12345678901234567890123456789012
C [6]
      str = '   This  is  a  test  string !  '
      call tstr (6, iunit, str)
C [7]
      str = 'This string is too long  and  will be truncated'
      call tstr (7, iunit, str)
C [8]
      str = ' Special %# characters !@$ okay '
      call tstr (8, iunit, str)
C [9]
      str = ' '
      call tstr (9, iunit, str)

      call qamessag (iunit, '+center',
     &   '*** End of Test ***')

10000  format (1x, 'List strings:', 10 (2x, a))
10010  format (1x, '[', i2, '] Call ', a, '  with  ', a)
10020  format (1x, 5x, 'It returned', 3i5)
      end

      subroutine tstr (icall, iunit, str)

C   --Subroutine tstr is internal to teststr.
C   --Test ISTRLEN, STRLIMIT, STRUPCASE, STRCMPRS, STRPACK on a single string.

      implicit none

      integer ISTRLEN

      integer icall
      integer iunit
      character*32 str

      integer ileft, iright
      integer lstr
      integer idum
      character*32 strout

      write (iunit, *)
      write (iunit, 10000, iostat=idum) str

      strout = str
      write (iunit, 10010, iostat=idum) icall, 'ISTRLEN'
      lstr = ISTRLEN (strout)
      write (iunit, 10030, iostat=idum) lstr, strout(1:lstr)
      write (iunit, 10020, iostat=idum) strout

      strout = str
      write (iunit, 10010, iostat=idum) icall, 'STRLIMIT'
      call STRLIMIT (strout, ileft, iright)
      write (iunit, 10040, iostat=idum)
     &   ileft, iright, strout(ileft:iright)
      write (iunit, 10020, iostat=idum) strout

      strout = str
      write (iunit, 10010, iostat=idum) icall, 'STRUPCASE'
      call STRUPCASE (strout)
      write (iunit, 10020, iostat=idum) strout
      write (iunit, 10020, iostat=idum) strout

      strout = str
      write (iunit, 10010, iostat=idum) icall, 'STRCMPRS'
      call STRCMPRS (strout, lstr)
      write (iunit, 10030, iostat=idum) lstr, strout(1:lstr)
      write (iunit, 10020, iostat=idum) strout

      strout = str
      write (iunit, 10010, iostat=idum) icall, 'STRPACK'
      call STRPACK (strout, lstr)
      write (iunit, 10030, iostat=idum) lstr, strout(1:lstr)
      write (iunit, 10020, iostat=idum) strout

10000  format (1x, 'Input string is ', t30, '[', a, ']')
10010  format (1x, '[', i2, '] Call ', a)
10020  format (1x, 5x, 'It returned', t30, '[', a, ']')
10030  format (1x, 5x, 'It returned', i5, t30, '[', a, ']')
10040  format (1x, 5x, 'It returned', i5, i5, t30, '[', a, ']')
      end
