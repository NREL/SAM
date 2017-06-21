C=======================================================================
      program testsup
C=======================================================================

      implicit none
      character*80 line

C   --Read the command line to determine which test to run:
c      call excmdlin (line)

c      if (index (line, 'testex') .NE. 0) then
c         call testex
c      else if (index (line, 'testmem') .NE. 0) then
         call testmem
c      end if

      end

C=======================================================================
      subroutine testex
C=======================================================================
C
C     CAMSUPES_LIB test program - EX routines
C
C=======================================================================

C   --In order to test the system-dependent routines, the following global
C   --symbols and logicals should be set before the program is run:
C   --   Symbols TESTSYM, EMPTYSYM (to "")
C   --   Logicals TESTLOG, USERLOG
C   --The following symbols and logical should be undefined:
C   --   Symbol BADSYM, logical BADLOG
C   --The following file must exist as a CMS BUILD executable:
C   --   TESTDIR:SUPLIB_TESTINFO.EXE

      implicit none

      logical EXCANCEL, EXHELP

      logical ok
      integer iunit
      integer iuntmp
      integer ierr
      integer l
      integer mode, kcsu, knsu, idau
      integer idum
      real cpusec
      character*48 line
      character*8 dat8, tim
      character*10 dat10
      character*16 hard, soft
      character*32 fname
      character*60 fullname
      character*40 lognam, logval
      character*40 symnam, symval
      character*16 node, disk, dir, base, ext, vers
      character*32 exenam
      character*8 exedat8, exetyp, execlass, exevers
      character*10 exedat10

C   --Open the output file

C   --Get the name of the output file from the command line
      call excmdlin (line)
      l = index (line, ' ') - 1

      iunit = 10
      call EXFOPEN (iunit, 'out', 'form', line(:l), ierr)
      write (iunit, 10000, iostat=idum)
10000  format (21x, '*** Test of CAMSUPES_LIB EX Routines ***')

      write (iunit, *)
C [1]
      write (iunit, 10040, iostat=idum) 1, 'EXFOPEN',
     &   'out', 'form', line(:l)
      write (iunit, 10060, iostat=idum) ierr

      write (iunit, *)
C [2]
      write (iunit, 10030, iostat=idum) 2, 'EXCPUS'
C   --Initialize CPU seconds (normally would be done as first statement)
      call EXCPUS (cpusec)
      write (iunit, 10100, iostat=idum) cpusec

C   --Check the command line

      write (iunit, *)
C [3]
      write (iunit, 10030, iostat=idum) 3, 'EXCMDLIN'
      call EXCMDLIN (line)
      write (iunit, 10070, iostat=idum) line

C   --Check the current date and time

      write (iunit, *)
C [4]
      write (iunit, 10030, iostat=idum) 4, 'EXDATE'
      call EXDATE (dat8)
      write (iunit, 10070, iostat=idum) dat8
      write (iunit, 10030, iostat=idum) 4, 'EXDATE-10'
      call EXDATE (dat10)
      write (iunit, 10070, iostat=idum) dat10
C [5]
      write (iunit, 10030, iostat=idum) 5, 'EXTIME'
      call EXTIME (tim)
      write (iunit, 10070, iostat=idum) tim

C   --Check file opening; open file for output, then for input

      write (iunit, *)
      iuntmp = 20
      fname = 'test.xxx'
C [6]
      write (iunit, 10040, iostat=idum) 6, 'EXFOPEN',
     &   'OUT', 'FORM', fname
      call EXFOPEN (iuntmp, 'OUT', 'FORM', fname, ierr)
      write (iunit, 10060, iostat=idum) ierr
      write (iuntmp, '(1x, a)', iostat=idum)
     &   'Test of EXFOPEN formatted'
      close (iuntmp)

C [7]
      write (iunit, 10040, iostat=idum) 7, 'EXFOPEN',
     &   'in', 'form', fname
      call EXFOPEN (iuntmp, 'in', 'form', fname, ierr)
      write (iunit, 10060, iostat=idum) ierr
      read (iuntmp, '(a)', iostat=idum) line
      write (iunit, 10110, iostat=idum) line
      close (iuntmp)

C   --Check file opening; unformatted

      write (iunit, *)
C [8]
      write (iunit, 10040, iostat=idum) 8, 'EXFOPEN',
     &   'out', 'unform', fname
      call EXFOPEN (iuntmp, 'out', 'unform', fname, ierr)
      write (iunit, 10060, iostat=idum) ierr
      write (iuntmp, iostat=idum)
     &   'Test of EXFOPEN UNformatted'
      close (iuntmp)

C [9]
      write (iunit, 10040, iostat=idum) 9, 'EXFOPEN',
     &   'in', 'unform', fname
      call EXFOPEN (iuntmp, 'in', 'unform', fname, ierr)
      write (iunit, 10060, iostat=idum) ierr
      read (iuntmp, iostat=idum) line
      write (iunit, 10110, iostat=idum) line
      close (iuntmp)

C   --Check file opening; listing

      write (iunit, *)
C [10]
      write (iunit, 10040, iostat=idum) 10, 'EXFOPEN',
     &   'out', 'list', fname
      call EXFOPEN (iuntmp, 'out', 'list', fname, ierr)
      write (iunit, 10060, iostat=idum) ierr
      write (iuntmp, '(a)', iostat=idum)
     &   'Test of EXFOPEN listing'
      close (iuntmp)

C [11]
      write (iunit, 10040, iostat=idum) 11, 'EXFOPEN',
     &   'in', 'form', fname
      call EXFOPEN (iuntmp, 'in', 'form', fname, ierr)
      write (iunit, 10060, iostat=idum) ierr
      read (iuntmp, '(a)', iostat=idum) line
      write (iunit, 10110, iostat=idum) line
      close (iuntmp)

C   --Check file date of file just created
C [12]
      write (iunit, 10040, iostat=idum) 12, 'EXFILDAT', fname
      call EXFILDAT (fname, dat8, tim)
      write (iunit, 10070, iostat=idum) dat8, tim
      write (iunit, 10040, iostat=idum) 12, 'EXFILDAT-10', fname
      call EXFILDAT (fname, dat10, tim)
      write (iunit, 10070, iostat=idum) dat10, tim

C   --Check file opening; scratch

      write (iunit, *)
      fname = 'scratch.xxx'
C [13]
      write (iunit, 10040, iostat=idum) 13, 'EXFOPEN',
     &   'scr', 'form', fname
      call EXFOPEN (iuntmp, 'scr', 'form', fname, ierr)
      write (iunit, 10060, iostat=idum) ierr
      write (iuntmp, '(1x, a)', iostat=idum)
     &   'Test of EXFOPEN scratch'
      rewind (iuntmp)
      read (iuntmp, '(a)', iostat=idum) line
      write (iunit, 10110, iostat=idum) line
      close (iuntmp)

C   --Scratch file should have been deleted; expect error
C [14]
      write (iunit, 10040, iostat=idum) 14, 'EXFOPEN',
     &   'in', 'form', fname
      call EXFOPEN (iuntmp, 'in', 'form', fname, ierr)
      write (iunit, 10060, iostat=idum) ierr
      close (iuntmp, iostat=idum)

C   --Check file date of scratch file (does not exist)
C [15]
      write (iunit, 10040, iostat=idum) 15, 'EXFILDAT', fname
      call EXFILDAT (fname, dat8, tim)
      write (iunit, 10070, iostat=idum) dat8, tim

C   --Check file date

      write (iunit, *)
      fname = 'a.out'
C [16]
      write (iunit, 10040, iostat=idum) 16, 'EXFILDAT', fname
      call EXFILDAT (fname, dat8, tim)
      write (iunit, 10070, iostat=idum) dat8, tim
      write (iunit, 10040, iostat=idum) 16, 'EXFILDAT-10', fname
      call EXFILDAT (fname, dat10, tim)
      write (iunit, 10070, iostat=idum) dat10, tim

C   --Check file date of file that does not exist
      fname = 'NOTHERE.EXT'
C [17]
      write (iunit, 10040, iostat=idum) 17, 'EXFILDAT', fname
      call EXFILDAT (fname, dat8, tim)
      write (iunit, 10070, iostat=idum) dat8, tim

C   --Check system parameters

      write (iunit, *)
C [18]
      write (iunit, 10030, iostat=idum) 18, 'EXPARM'
      call EXPARM (hard, soft, mode, kcsu, knsu, idau)
      write (iunit, 10070, iostat=idum) hard, soft
      write (iunit, 10060, iostat=idum) mode, kcsu, knsu, idau

C   --Check values of system symbols

      write (iunit, *)
      symnam = 'TESTSYM'
C [19]
      write (iunit, 10040, iostat=idum) 19, 'EXSYMBOL', symnam
      call EXSYMBOL (symnam, symval, l)
      write (iunit, 10080, iostat=idum) l, symval
      symnam = 'testsym'
C [20]
      write (iunit, 10040, iostat=idum) 20, 'EXSYMBOL', symnam
      call EXSYMBOL (symnam, symval, l)
      write (iunit, 10080, iostat=idum) l, symval
      symnam = 'BADSYM'
C [21]
      write (iunit, 10040, iostat=idum) 21, 'EXSYMBOL', symnam
      call EXSYMBOL (symnam, symval, l)
      write (iunit, 10080, iostat=idum) l, symval
      symnam = 'EMPTYSYM'
C [22]
      write (iunit, 10040, iostat=idum) 22, 'EXSYMBOL', symnam
      call EXSYMBOL (symnam, symval, l)
      write (iunit, 10080, iostat=idum) l, symval

C   --Check values of system logicals

      write (iunit, *)
      lognam = 'TESTLOG'
C [23]
      write (iunit, 10040, iostat=idum) 23, 'EXLOGICAL', lognam
      call EXLOGICAL (lognam, logval, l)
      write (iunit, 10080, iostat=idum) l, logval

      lognam = 'testlog'
C [24]
      write (iunit, 10040, iostat=idum) 24, 'EXLOGICAL', lognam
      call EXLOGICAL (lognam, logval, l)
      write (iunit, 10080, iostat=idum) l, logval

      lognam = 'BADLOG'
C [25]
      write (iunit, 10040, iostat=idum) 25, 'EXLOGICAL', lognam
      call EXLOGICAL (lognam, logval, l)
      write (iunit, 10080, iostat=idum) l, logval

      lognam = 'USERLOG'
C [26]
      write (iunit, 10040, iostat=idum) 26, 'EXLOGICAL', lognam
      call EXLOGICAL (lognam, logval, l)
      write (iunit, 10080, iostat=idum) l, logval

C   --Check file name parsing routine

      write (iunit, *)
      fullname = '/root/user/dir/file_name.ext'
C [27]
      write (iunit, 10040, iostat=idum) 27, 'EXFILFLD', fullname
      call EXFILFLD (fullname, node, disk, dir, base, ext, vers)
      write (iunit, 10070, iostat=idum) node, disk, dir
      write (iunit, 10070, iostat=idum) base, ext, vers

      fullname = 'filename.ext'
C [28]
      write (iunit, 10040, iostat=idum) 28, 'EXFILFLD', fullname
      call EXFILFLD (fullname, node, disk, dir, base, ext, vers)
      write (iunit, 10070, iostat=idum) node, disk, dir
      write (iunit, 10070, iostat=idum) base, ext, vers

      fullname = '/root/user/dir/'
C [29]
      write (iunit, 10040, iostat=idum) 29, 'EXFILFLD', fullname
      call EXFILFLD (fullname, node, disk, dir, base, ext, vers)
      write (iunit, 10070, iostat=idum) node, disk, dir
      write (iunit, 10070, iostat=idum) base, ext, vers

      fullname = '/root/user/dir/file_name.ext'
C [30]
      write (iunit, 10040, iostat=idum) 30, 'EXFILFLD', fullname
      call EXFILFLD (fullname, node, disk, dir, base, ext, vers)
      write (iunit, 10070, iostat=idum) node, disk, dir
      write (iunit, 10070, iostat=idum) base, ext, vers

C   --Check EXEXEINFO

      write (iunit, *)
      write (*, 10020, iostat=idum) 'EXEXEINFO'
      fullname = ' '
C [31]
!      write (iunit, 10040, iostat=idum) 31, 'EXEXEINFO', fullname
!      call EXEXEINFO (fullname,
!     &   exenam, exedat8, exetyp, execlass, exevers, ierr)
!      write (iunit, 10080, iostat=ierr) ierr, fullname
!      write (iunit, 10070, iostat=ierr) exenam
!      write (iunit, 10070, iostat=ierr)
!     &   exedat8, exetyp, execlass, exevers

      fullname = 'testdir:suplib_testinfo.exe'
C [32]
!      write (iunit, 10040, iostat=idum) 32, 'EXEXEINFO', fullname
!      call EXEXEINFO (fullname,
!     &   exenam, exedat8, exetyp, execlass, exevers, ierr)
!      write (iunit, 10080, iostat=ierr) ierr, fullname
!      write (iunit, 10070, iostat=ierr) exenam
!      write (iunit, 10070, iostat=ierr)
!     &   exedat8, exetyp, execlass, exevers
!      write (iunit, 10040, iostat=idum) 32, 'EXEXEINFO-10', fullname
!      call EXEXEINFO (fullname,
!     &   exenam, exedat10, exetyp, execlass, exevers, ierr)
!      write (iunit, 10080, iostat=ierr) ierr, fullname
!      write (iunit, 10070, iostat=ierr) exenam
!      write (iunit, 10070, iostat=ierr)
!     &   exedat10, exetyp, execlass, exevers

      fullname = 'badexe.exe'
C [33]
!      write (iunit, 10040, iostat=idum) 33, 'EXEXEINFO', fullname
!      call EXEXEINFO (fullname,
!     &   exenam, exedat8, exetyp, execlass, exevers, ierr)
!      write (iunit, 10080, iostat=ierr) ierr, fullname
!      write (iunit, 10070, iostat=ierr) exenam
!      write (iunit, 10070, iostat=ierr)
!     &   exedat8, exetyp, execlass, exevers

C   --Check prompt and input routine

      write (iunit, *)
      write (*, 10020, iostat=idum) 'EXREAD'
C [34]
      write (iunit, 10030, iostat=idum) 34, 'EXREAD'
      call EXREAD ('prompt> ', line, ierr)
      write (iunit, 10080, iostat=ierr) ierr, line

C   --Check CPU routine

      write (iunit, *)
C [35]
      write (iunit, 10030, iostat=idum) 35, 'EXCPUS'
      call EXCPUS (cpusec)
      write (iunit, 10100, iostat=idum) cpusec

C   --Check EXOVERWR (quickly)

      write (iunit, *)
      write (*, 10020, iostat=idum) 'EXOVERWR'
C [36]
      write (iunit, 10040, iostat=idum) 36, 'EXOVERWR', 'i'
      call EXOVERWR ('i', 'THIS IS A SHORT LINE')
C [37]
      write (iunit, 10040, iostat=idum) 37, 'EXOVERWR', ' '
      call EXOVERWR (' ', 'this is a longer line')
C [38]
      write (iunit, 10040, iostat=idum) 38, 'EXOVERWR', 'B'
      call EXOVERWR ('B', 'Blank line first')

C   --Check EXCANCEL (quickly)

      write (iunit, *)
C [39]
      write (iunit, 10050, iostat=idum) 39, 'EXCANCEL', .true.
      ok = EXCANCEL (.TRUE.)
      write (iunit, 10090, iostat=idum) ok

C [40]
      write (iunit, 10050, iostat=idum) 40, 'EXCANCEL', .false.
      ok = EXCANCEL (.FALSE.)
      write (iunit, 10090, iostat=idum) ok

C   --Check EXHELP (quickly)

      write (iunit, *)
      write (*, 10020, iostat=idum) 'EXHELP'
C [41]
      write (iunit, 10040, iostat=idum) 41, 'EXHELP',
     &   'BADPROG BADTYPE BADOPTION'
      ok = EXHELP ('SUPLIB_TEST_HELP',
     &   'BADPROG BADTYPE BADOPTION')
      write (iunit, 10090, iostat=idum) ok

      write (*, 10020, iostat=idum) 'EXHELP'
C [42]
      write (iunit, 10040, iostat=idum) 42, 'EXHELP',
     &   'TESTEXHELP SECOND_LEVEL_2 FIRST_SUBLEVEL_3'
      ok = EXHELP ('SUPLIB_TEST_HELP',
     &   'TESTEXHELP SECOND_LEVEL_2 FIRST_SUBLEVEL_3')
      write (iunit, 10090, iostat=idum) ok

C   --Check EXSPAWN

      write (iunit, *)
      write (*, 10020, iostat=idum) 'EXSPAWN'
      line = 'ls t*'
C [43]
!      write (iunit, 10040, iostat=idum) 43, 'EXSPAWN', line
!      call EXSPAWN (line, ierr)
!      write (iunit, 10060, iostat=ierr) ierr

C   --Check CPU routine

      write (iunit, *)
C [44]
      write (iunit, 10030, iostat=idum) 44, 'EXCPUS'
      call EXCPUS (cpusec)
      write (iunit, 10100, iostat=idum) cpusec

C   --Check abort routine (message should not be printed)

      write (iunit, *)
      write (*, 10020, iostat=idum) 'EXABORT'
C [45]
      write (iunit, 10030, iostat=idum) 45, 'EXABORT'
      call EXABORT

C   --This message should not be written
      write (*, 10010, iostat=idum)
      write (iunit, 10010, iostat=idum)
10010  format (/, 30x,
     &   '*** End of Test ***')

10020  format (/, 1x, 'Calling * ', a, ' *')
10030  format (1x, '[', i2, '] Call ', a)
10040  format (1x, '[', i2, '] Call ', a, '  with', 4 (2x, a))
10050  format (1x, '[', i2, '] Call ', a, '  with  ', l1)
10060  format (1x, 5x, 'It returned', 10i5)
10070  format (1x, 5x, 'It returned', 8 (:, 2x, '!', a))
10080  format (1x, 5x, 'It returned', i5, :, 2x, '!', a)
10090  format (1x, 5x, 'It returned', 2x, l1)
10100  format (1x, 5x, 'It returned', 1pe15.6)
10110  format (1x, 'Line read:', 2x, '!', a)
      return
      end

C========================================================================
      subroutine testmem
C=======================================================================
C
C     CAMSUPES_LIB test program - dynamic memory routines
C
C=======================================================================

      implicit none

      integer IXLNUM, IXLCHR

      real rmem(1)
      character*1 cmem(1)
      integer imem(1)
      logical lmem(1)
      equivalence (rmem, imem, lmem)

      integer iunit
      integer len
      integer mnerrs, mnused
      integer nsua, nsud, nsuv, nsulv
      integer ierrno
      integer icount
      integer i, j
      integer ix
      integer idum
      integer kptr
      integer krary1, kcary1
      integer krealary, kintary, kcharary
      integer krealnew, kcharnew
      integer kinofill, kcnofill

      character*80 line

C   --Define an internal function to convert character index to
C   --internal memory manager table location
      integer ixoff
      integer ix2tbl
      ix2tbl (ix) = (ix - 1 - ixoff) / 4 + 1

C   --Get the name of the output file from the command line
      call excmdlin (line)

C   --Open the output file
      iunit = 10
      call exfopen (iunit, 'out', 'form', line, idum)
      write (iunit, 10000, iostat=idum)
10000  format (21x,
     &   '*** Test of CAMSUPES_LIB Dynamic Memory Routines ***')

C   --Find out where memory base arrays are located, with respect to each other

      write (iunit, *)
      ix = loc (rmem(1))
      write (iunit, 10080, iostat=idum) ix
      ixoff = loc (cmem(1))
      write (iunit, 10080, iostat=idum) ixoff
      ixoff = ix - ixoff

C   --Initialize memory - must be first call of memory routines

      write (iunit, *)
C [1]
      write (iunit, 10020, iostat=idum) 1, 'MDINIT'
      call MDINIT (rmem)
C [2]
      write (iunit, 10020, iostat=idum) 2, 'MCINIT'
      call MCINIT (cmem)

C   --Cause new reserved arrays to be filled

      write (iunit, *)
C [3]
      write (iunit, 10060, iostat=idum) 3, 'MDFILL', -999.0
      call MDFILL (-999.0)
C [4]
      write (iunit, 10030, iostat=idum) 4, 'MCFILL', 'Z'
      call MCFILL ('Z')

C   --Reserve arrays (will be deleted later)

      write (iunit, *)
C [5]
      write (iunit, 10040, iostat=idum) 5, 'MDRSRV', 'RARY1', 1000
      call MDRSRV ('RARY1', krary1, 1000)
      write (iunit, 10070, iostat=idum) krary1
C [6]
      write (iunit, 10040, iostat=idum) 6, 'MCRSRV', 'CARY1', 1000
      call MCRSRV ('CARY1', kcary1, 1000)
      write (iunit, 10070, iostat=idum) kcary1, ix2tbl (kcary1)

C   --Reserve arrays and print out to check fill

      write (iunit, *)
C [7]
      write (iunit, 10040, iostat=idum) 7, 'MDRSRV', 'REALARY', 100
      call MDRSRV ('REALARY', krealary, 100)
      write (iunit, 10070, iostat=idum) krealary
C [8]
      write (iunit, 10040, iostat=idum) 8, 'MDRSRV', 'intary', 100
      call MDRSRV ('intary', kintary, 100)
      write (iunit, 10070, iostat=idum) kintary
C [9]
      write (iunit, 10040, iostat=idum) 9, 'MCRSRV', 'CHARARY', 8*10
      call MCRSRV ('CHARARY', kcharary, 8*10)
      write (iunit, 10070, iostat=idum) kcharary, ix2tbl (kcharary)
C [10]
      write (iunit, 10030, iostat=idum) 10, 'MDPRNT', 'REALARY', 'r'
      call MDPRNT ('REALARY', iunit, 'r')
C [11]
      write (iunit, 10040, iostat=idum) 11, 'MCPRNT', 'CHARARY', 8
      call MCPRNT ('CHARARY', iunit, 8)

C   --Cause new arrays not to be filled

C [12]
      write (iunit, 10020, iostat=idum) 12, 'MDFOFF'
      call MDFOFF
C [13]
      write (iunit, 10020, iostat=idum) 13, 'MCFOFF'
      call MCFOFF

C   --Reserve arrays and print out to check NO fill (may be garbage)

      write (iunit, *)
C [14]
      write (iunit, 10040, iostat=idum) 14, 'MDRSRV', 'inofill', 10
      call MDRSRV ('inofill', kinofill, 10)
      write (iunit, 10070, iostat=idum) kinofill
C [15]
      write (iunit, 10040, iostat=idum) 15, 'MCRSRV', 'cnofill', 10*5
      call MCRSRV ('cnofill', kcnofill, 10*5)
      write (iunit, 10070, iostat=idum) kcnofill, ix2tbl (kcnofill)
C [16]
      write (iunit, 10030, iostat=idum) 16, 'MDPRNT', 'inofill', 'i'
      call MDPRNT ('inofill', iunit, 'i')
C [17]
      write (iunit, 10040, iostat=idum) 17, 'MCPRNT', 'cnofill', 10
      call MCPRNT ('cnofill', iunit, 10)

C   --Check MxRSRV by listing memory allocation table

C [18]
      write (iunit, 10050, iostat=idum) 18, 'MDLIST', iunit
      call MDLIST (iunit)

C   --Check that memory can be read and written and check MxPRNT

      write (iunit, *)
      do 100 i = 1, 100
         imem(kintary+i-1) = i * 10
  100 continue
C [19]
      write (iunit, 10030, iostat=idum) 19, 'MDPRNT', 'intary', 'I'
      call MDPRNT ('intary', iunit, 'I')

      do 110 i = 1, 100
         rmem(krealary+i-1) = - imem(kintary+i-1) * 100
  110 continue
C [20]
      write (iunit, 10030, iostat=idum) 20, 'MDPRNT', 'REALARY', 'r'
      call MDPRNT ('REALARY', iunit, 'r')

      do 120 i = 1, 40
         cmem(kcharary+i-1) = 'A'
  120 continue
      do 130 i = 41, 80
         cmem(kcharary+i-1) = 'B'
  130 continue
C [21]
      write (iunit, 10040, iostat=idum) 21, 'MCPRNT', 'CHARARY', 10
      call MCPRNT ('CHARARY', iunit, 10)
C [22]
      write (iunit, 10040, iostat=idum) 22, 'MCPRNT', 'CHARARY', 9
      call MCPRNT ('CHARARY', iunit, 9)

C   --Rename arrays, then find new pointer

      write (iunit, *)
C [23]
      write (iunit, 10030, iostat=idum) 23, 'MDFIND', 'REALARY'
      call MDFIND ('REALARY', krealnew, len)
      write (iunit, 10070, iostat=idum) krealnew, len
C [24]
      write (iunit, 10030, iostat=idum) 24, 'MCFIND', 'CHARARY'
      call MCFIND ('CHARARY', kcharnew, len)
      write (iunit, 10070, iostat=idum) kcharnew, ix2tbl (kcharnew), len
C [25]
      write (iunit, 10030, iostat=idum) 25, 'MDNAME',
     &   'REALARY', 'realnew'
      call MDNAME ('REALARY', 'realnew')
C [26]
      write (iunit, 10030, iostat=idum) 26, 'MCNAME',
     &   'CHARARY', 'charnew'
      call MCNAME ('CHARARY', 'charnew')
C [27]
      write (iunit, 10030, iostat=idum) 27, 'MDFIND', 'REALNEW'
      call MDFIND ('REALNEW', krealnew, len)
      write (iunit, 10070, iostat=idum) krealnew, len
C [28]
      write (iunit, 10030, iostat=idum) 28, 'MCFIND', 'CHARNEW'
      call MCFIND ('CHARNEW', kcharnew, len)
      write (iunit, 10070, iostat=idum) kcharnew, ix2tbl (kcharnew), len

C   --Check MxNAME and MxFIND by listing memory allocation table

C [29]
      write (iunit, 10050, iostat=idum) 29, 'MDLIST', iunit
      call MDLIST (iunit)

C   --Turn fill back on

      write (iunit, *)
C [30]
      write (iunit, 10060, iostat=idum) 30, 'MDFILL', -888.0
      call MDFILL (-888.0)
C [31]
      write (iunit, 10030, iostat=idum) 31, 'MCFILL', 'Y'
      call MCFILL ('Y')

C   --Lengthen arrays (may move array) and check by finding and printing the
C   --array

      write (iunit, *)
C [32]
      write (iunit, 10040, iostat=idum) 32, 'MDLONG', 'REALNEW', 200
      call MDLONG ('REALNEW', krealnew, 200)
      write (iunit, 10070, iostat=idum) krealnew
C [33]
      write (iunit, 10040, iostat=idum) 33, 'MCLONG', 'CHARNEW', 8*20
      call MCLONG ('CHARNEW', kcharnew, 8*20)
      write (iunit, 10070, iostat=idum) kcharnew, ix2tbl (kcharnew)
C [34]
      write (iunit, 10030, iostat=idum) 34, 'MDFIND', 'REALNEW'
      call MDFIND ('REALNEW', krealnew, len)
      write (iunit, 10070, iostat=idum) krealnew, len
C [35]
      write (iunit, 10030, iostat=idum) 35, 'MCFIND', 'CHARNEW'
      call MCFIND ('CHARNEW', kcharnew, len)
      write (iunit, 10070, iostat=idum) kcharnew, ix2tbl (kcharnew), len
C [36]
      write (iunit, 10030, iostat=idum) 36, 'MDPRNT', 'REALNEW', 'r'
      call MDPRNT ('REALNEW', iunit, 'r')
C [37]
      write (iunit, 10040, iostat=idum) 37, 'MCPRNT', 'CHARNEW', 10
      call MCPRNT ('CHARNEW', iunit, 10)

C   --Shorten arrays and check by finding and printing the array

      write (iunit, *)
C [38]
      write (iunit, 10040, iostat=idum) 38, 'MDLONG', 'RARY1', 500
      call MDLONG ('RARY1', krary1, 500)
      write (iunit, 10070, iostat=idum) krary1
C [39]
      write (iunit, 10040, iostat=idum) 39, 'MCLONG', 'CARY1', 8*40
      call MCLONG ('CARY1', kcary1, 8*40)
      write (iunit, 10070, iostat=idum) kcary1, ix2tbl (kcary1)
C [40]
      write (iunit, 10030, iostat=idum) 40, 'MDFIND', 'RARY1'
      call MDFIND ('RARY1', krary1, len)
      write (iunit, 10070, iostat=idum) krary1, len
C [41]
      write (iunit, 10030, iostat=idum) 41, 'MCFIND', 'CARY1'
      call MCFIND ('CARY1', kcary1, len)
      write (iunit, 10070, iostat=idum) kcary1, ix2tbl (kcary1), len
C [42]
      write (iunit, 10030, iostat=idum) 42, 'MDPRNT', 'RARY1', 'r'
      call MDPRNT ('RARY1', iunit, 'r')
C [43]
      write (iunit, 10040, iostat=idum) 43, 'MCPRNT', 'CARY1', 10
      call MCPRNT ('CARY1', iunit, 10)

C   --Check MxLONG by listing memory allocation table

C [44]
      write (iunit, 10050, iostat=idum) 44, 'MDLIST', iunit
      call MDLIST (iunit)

C   --Delete arrays

      write (iunit, *)
C [45]
      write (iunit, 10030, iostat=idum) 45, 'MDDEL', 'RARY1'
      call MDDEL ('RARY1')
C [46]
      write (iunit, 10030, iostat=idum) 46, 'MCDEL', 'CARY1'
      call MCDEL ('CARY1')

C   --Check MxDEL by listing memory allocation table

C [47]
      write (iunit, 10050, iostat=idum) 47, 'MDLIST', iunit
      call MDLIST (iunit)

C   --Generate error with no MDDEBG.

      write (iunit, *)
C   --Generate error (type 5) because the array was not reserved
C [48]
      write (iunit, 10030, iostat=idum) 48, 'MDDEL', 'bad'
      call MDDEL ('bad')

C   --Check MDDEBG

C [49]
      write (iunit, 10050, iostat=idum) 49, 'MDDEBG', iunit
      call MDDEBG (iunit)
C   --Generate error (type 6) because the array already exists
C [50]
      write (iunit, 10040, iostat=idum) 50, 'MDRSRV', 'REALNEW', 123
      call MDRSRV ('REALNEW', kptr, 123)

C   --Check MCDEBG (exactly equivalent to MDDEBG)

C [51]
      write (iunit, 10050, iostat=idum) 51, 'MCDEBG', iunit
      call MCDEBG (iunit)
C   --Generate error (type 5) because the array was already deleted
C [52]
      write (iunit, 10030, iostat=idum) 52, 'MDDEL', 'RARY1'
      call MDDEL ('RARY1')

C   --Check error status

      write (iunit, *)
C [53]
      write (iunit, 10020, iostat=idum) 53, 'MDSTAT'
      call MDSTAT (mnerrs, mnused)
      write (iunit, 10070, iostat=idum) mnerrs, mnused
C [54]
      write (iunit, 10020, iostat=idum) 54, 'MCSTAT'
      call MCSTAT (mnerrs, mnused)
      write (iunit, 10070, iostat=idum) mnerrs, mnused

C   --Print error table

C [55]
      write (iunit, 10050, iostat=idum) 55, 'MDEROR', iunit
      call MDEROR (iunit)

C   --Check last error

C [56]
      write (iunit, 10020, iostat=idum) 56, 'MDLAST'
      call MDLAST (ierrno)
      write (iunit, 10070, iostat=idum) ierrno
C [57]
      write (iunit, 10020, iostat=idum) 57, 'MCLAST'
      call MCLAST (ierrno)
      write (iunit, 10070, iostat=idum) ierrno

C   --Get error counts by error code

      ierrno = 5
C [58]
      write (iunit, 10050, iostat=idum) 58, 'MDERPT', ierrno
      call MDERPT (ierrno, icount)
      write (iunit, 10070, iostat=idum) icount
      ierrno = 4
C [59]
      write (iunit, 10050, iostat=idum) 59, 'MCERPT', ierrno
      call MCERPT (ierrno, icount)
      write (iunit, 10070, iostat=idum) icount

C   --Reset error type 5 to 0 and set error type 2 to 10

      ierrno = 5
C [60]
      write (iunit, 10050, iostat=idum) 60, 'MDEFIX', ierrno, 0
      call MDEFIX (ierrno, 0)
      ierrno = 2
C [61]
      write (iunit, 10050, iostat=idum) 61, 'MCEFIX', ierrno, 10
      call MCEFIX (ierrno, 10)
C [62]
      write (iunit, 10050, iostat=idum) 62, 'MCEROR', iunit
      call MCEROR (iunit)
C [63]
      write (iunit, 10020, iostat=idum) 63, 'MDSTAT'
      call MDSTAT (mnerrs, mnused)
      write (iunit, 10070, iostat=idum) mnerrs, mnused

C   --Compress memory and check by finding and printing arrays

      write (iunit, *)
C [64]
      write (iunit, 10020, iostat=idum) 64, 'MDCOMP'
      call MDCOMP
C [65]
      write (iunit, 10020, iostat=idum) 65, 'MCCOMP'
      call MCCOMP
C [66]
      write (iunit, 10030, iostat=idum) 66, 'MDFIND', 'REALNEW'
      call MDFIND ('REALNEW', krealnew, len)
      write (iunit, 10070, iostat=idum) krealnew, len
C [67]
      write (iunit, 10030, iostat=idum) 67, 'MCFIND', 'CHARNEW'
      call MCFIND ('CHARNEW', kcharnew, len)
      write (iunit, 10070, iostat=idum) kcharnew, ix2tbl (kcharnew), len
C [68]
      write (iunit, 10030, iostat=idum) 68, 'MDPRNT', 'REALNEW', 'r'
      call MDPRNT ('REALNEW', iunit, 'r')
C [69]
      write (iunit, 10040, iostat=idum) 69, 'MCPRNT', 'CHARNEW', 10
      call MCPRNT ('CHARNEW', iunit, 10)

C   --Check MxCOMP by listing memory allocation table

C [70]
      write (iunit, 10050, iostat=idum) 70, 'MDLIST', iunit
      call MDLIST (iunit)

C   --The following routines have little visible effect.  They may affect the
C   --void table.

      write (iunit, *)

      len = 3*1000
C [71]
      write (iunit, 10050, iostat=idum) 71, 'MDGET', len
      call MDGET (len)
      len = 3*8*10
C [72]
      write (iunit, 10050, iostat=idum) 72, 'MCGET', len
      call MCGET (len)

C [73]
      write (iunit, 10020, iostat=idum) 73, 'MDGIVE'
      call MDGIVE
C [74]
      write (iunit, 10020, iostat=idum) 74, 'MCGIVE'
      call MCGIVE

C   --Check that the routines above did nothing dangerous by listing memory
C   --allocation table and error table

C [75]
      write (iunit, 10050, iostat=idum) 75, 'MDLIST', iunit
      call MDLIST (iunit)
C [76]
      write (iunit, 10050, iostat=idum) 76, 'MCEROR', iunit
      call MCEROR (iunit)

C   --Try to reserve arrays and list memory allocation table again

      write (iunit, *)
C [77]
      write (iunit, 10040, iostat=idum) 77, 'MDRSRV', 'RARY2', 2000
      call MDRSRV ('RARY2', kptr, 2000)
      write (iunit, 10070, iostat=idum) kptr
C [78]
      write (iunit, 10040, iostat=idum) 78, 'MDRSRV', 'RARY1', 1000
      call MDRSRV ('RARY1', kptr, 1000)
      write (iunit, 10070, iostat=idum) kptr
C [79]
      write (iunit, 10040, iostat=idum) 79, 'MCRSRV', 'CARY1', 1000
      call MCRSRV ('CARY1', kptr, 1000)
      write (iunit, 10070, iostat=idum) kptr, ix2tbl (kptr)

C [80]
      write (iunit, 10050, iostat=idum) 80, 'MCLIST', iunit
      call MCLIST (iunit)

C   --Check IXLNUM and IXLCHR against LOC

      write (iunit, *)
C [81]
      write (iunit, 10040, iostat=idum) 81, 'IXLNUM', 'rmem(krealnew)'
      ix = IXLNUM (rmem(krealnew))
      write (iunit, 10070, iostat=idum) ix
      ix = loc (rmem(krealnew))
      write (iunit, 10080, iostat=idum) ix, ix/4
C [82]
      write (iunit, 10040, iostat=idum) 82, 'IXLCHR', 'rmem(kcharnew)'
      ix = IXLCHR (cmem(kcharnew))
      write (iunit, 10070, iostat=idum) ix
      ix = loc (cmem(kcharnew))
      write (iunit, 10080, iostat=idum) ix

      write (iunit, 10010, iostat=idum)
10010  format (/, 30x,
     &   '*** End of Test ***')

10020  format (1x, '[', i2, '] Call ', a)
10030  format (1x, '[', i2, '] Call ', a, '  with  ', 4 (a, :, 2x))
10040  format (1x, '[', i2, '] Call ', a, '  with  ', a, 4i6)
10050  format (1x, '[', i2, '] Call ', a, '  with', 4i6)
10060  format (1x, '[', i2, '] Call ', a, '  with', f9.2)
10070  format (1x, 5x, 'It returned', 4i10)
10080  format (1x, 5x, 'LOC returned', 4i12)
      return
      end
