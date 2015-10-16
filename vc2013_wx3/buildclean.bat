REM   This batch file does a clean build of SAM in Windows so you don't have
REM   to do the builds manually in Visual Studio Express. It builds both the
REM   32-bit and 64-bit versions of SAM.
REM   It assumes the Windows environment variables are set as described in the 
REM   "Getting started with Visual Studio 2013 Express and wxWidgets 3" document.
REM   This file should go in the directory that contains the LK, WEX, SSC, and 
REM   SAMNT folders.

cd\
cd C:\Program Files\TortoiseSVN\bin\

TortoiseProc.exe /command:update /path:%LKDIR% /closeonend:1
TortoiseProc.exe /command:update /path:%WEXDIR% /closeonend:1
TortoiseProc.exe /command:update /path:%SSCDIR% /closeonend:1
TortoiseProc.exe /command:update /path:%SAMNTDIR% /closeonend:1

cd\
cd C:\Program Files (x86)\Microsoft Visual Studio 12.0

msbuild %LKDIR%\vc2013_wx3\lkvc13wx3.sln /p:Configuration=Debug;Platform=win32 /t:Clean;Build
msbuild %LKDIR%\vc2013_wx3\lkvc13wx3.sln /p:Configuration=Release;Platform=win32 /t:Clean;Build
msbuild %LKDIR%\vc2013_wx3\lkvc13wx3.sln /p:Configuration=Debug;Platform=x64 /t:Clean;Build
msbuild %LKDIR%\vc2013_wx3\lkvc13wx3.sln /p:Configuration=Release;Platform=x64 /t:Clean;Build

msbuild %WEXDIR%\vc2013_wx3\wexvc13wx3.sln /p:Configuration=Debug;Platform=win32 /t:Clean;Build
msbuild %WEXDIR%\vc2013_wx3\wexvc13wx3.sln /p:Configuration=Release;Platform=win32 /t:Clean;Build
msbuild %WEXDIR%\vc2013_wx3\wexvc13wx3.sln /p:Configuration=Debug;Platform=x64 /t:Clean;Build
msbuild %WEXDIR%\vc2013_wx3\wexvc13wx3.sln /p:Configuration=Release;Platform=x64 /t:Clean;Build

msbuild %SSCDIR%\build_vc2013\ssc_vc2013.sln /p:Configuration=Debug;Platform=win32 /t:Clean;Build
msbuild %SSCDIR%\build_vc2013\ssc_vc2013.sln /p:Configuration=Release;Platform=win32 /t:Clean;Build
msbuild %SSCDIR%\build_vc2013\ssc_vc2013.sln /p:Configuration=Debug;Platform=x64 /t:Clean;Build
msbuild %SSCDIR%\build_vc2013\ssc_vc2013.sln /p:Configuration=Release;Platform=x64 /t:Clean;Build

copy %SSCDIR%\build_vc2013\Win32\Release\ssc.dll %SAMNTDIR%\deploy\win32
copy %SSCDIR%\build_vc2013\x64\Release\ssc.dll %SAMNTDIR%\SAMnt\deploy\x64

msbuild %SAMNTDIR%\vc2013_wx3\SAMnt_vc2013.sln /p:Configuration=Debug;Platform=win32 /t:Clean;Build
msbuild %SAMNTDIR%\vc2013_wx3\SAMnt_vc2013.sln /p:Configuration=Release;Platform=win32 /t:Clean;Build
msbuild %SAMNTDIR%\vc2013_wx3\SAMnt_vc2013.sln /p:Configuration=Debug;Platform=x64 /t:Clean;Build
msbuild %SAMNTDIR%\vc2013_wx3\SAMnt_vc2013.sln /p:Configuration=Release;Platform=x64 /t:Clean;Build

cd\

