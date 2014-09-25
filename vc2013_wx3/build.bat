cd\
cd C:\Program Files\TortoiseSVN\bin\

TortoiseProc.exe /command:update /path:"F:\sam_dev\lk" /closeonend:1
TortoiseProc.exe /command:update /path:"F:\sam_dev\wex" /closeonend:1
TortoiseProc.exe /command:update /path:"F:\sam_dev\ssc" /closeonend:1
TortoiseProc.exe /command:update /path:"F:\sam_dev\SAMnt" /closeonend:1

cd\
cd C:\Program Files (x86)\Microsoft Visual Studio 12.0

msbuild %LKDIR%\vc2013_wx3\lkvc13wx3.sln /p:Configuration=Debug;Platform=win32
msbuild %LKDIR%\vc2013_wx3\lkvc13wx3.sln /p:Configuration=Release;Platform=win32
msbuild %LKDIR%\vc2013_wx3\lkvc13wx3.sln /p:Configuration=Debug;Platform=x64
msbuild %LKDIR%\vc2013_wx3\lkvc13wx3.sln /p:Configuration=Release;Platform=x64

msbuild %WEXDIR%\vc2013_wx3\wexvc13wx3.sln /p:Configuration=Debug;Platform=win32
msbuild %WEXDIR%\vc2013_wx3\wexvc13wx3.sln /p:Configuration=Release;Platform=win32
msbuild %WEXDIR%\vc2013_wx3\wexvc13wx3.sln /p:Configuration=Debug;Platform=x64
msbuild %WEXDIR%\vc2013_wx3\wexvc13wx3.sln /p:Configuration=Release;Platform=x64

msbuild %SSCDIR%\build_vc2013\ssc_vc2013.sln /p:Configuration=Debug;Platform=win32
msbuild %SSCDIR%\build_vc2013\ssc_vc2013.sln /p:Configuration=Release;Platform=win32
msbuild %SSCDIR%\build_vc2013\ssc_vc2013.sln /p:Configuration=Debug;Platform=x64
msbuild %SSCDIR%\build_vc2013\ssc_vc2013.sln /p:Configuration=Release;Platform=x64

copy %SSCDIR%\build_vc2013\Win32\Release\ssc.dll F:\sam_dev\SAMnt\deploy\win32
copy %SSCDIR%\build_vc2013\x64\Release\ssc.dll F:\sam_dev\SAMnt\deploy\x64

msbuild %SAMNTDIR%\vc2013_wx3\SAMnt_vc2013.sln /p:Configuration=Debug;Platform=win32
msbuild %SAMNTDIR%\vc2013_wx3\SAMnt_vc2013.sln /p:Configuration=Release;Platform=win32
msbuild %SAMNTDIR%\vc2013_wx3\SAMnt_vc2013.sln /p:Configuration=Debug;Platform=x64
msbuild %SAMNTDIR%\vc2013_wx3\SAMnt_vc2013.sln /p:Configuration=Release;Platform=x64

cd\
cd F:\sam_dev
