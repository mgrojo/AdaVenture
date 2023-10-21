
rem using Window 64-bit GNU Ada:

rem One method. Requires installation of
rem MSYS2+mingw64; and some pacman prepatory commands:
rem PATH=c:\msys64\mingw64\bin;%PATH%

rem Second, simpler method. Requires single install to <HOME>\opt\ ...
rem go to...
rem https://github.com/alire-project/GNAT-FSF-builds/releases/tag/gnat-12.1.0-2
rem and download: 	gnat-x86_64-windows64-12.1.0-2.tar.gz
rem Then adjust path...
PATH=%HOMEPATH%\opt\gnat-x86_64-windows64-12.1.0-2\bin;%PATH%

