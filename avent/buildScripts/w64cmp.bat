
rem -- Copyright (C) 2024  <fastrgv@gmail.com>
rem --
rem -- This program is free software: you can redistribute it and/or modify
rem -- it under the terms of the GNU General Public License as published by
rem -- the Free Software Foundation, either version 3 of the License, or
rem -- (at your option) any later version.
rem --
rem -- This program is distributed in the hope that it will be useful,
rem -- but WITHOUT ANY WARRANTY; without even the implied warranty of
rem -- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
rem -- GNU General Public License for more details.
rem --
rem -- You may read the full text of the GNU General Public License
rem -- at <http://www.gnu.org/licenses/>.


rem using 64-bit GNU Ada:
rem First, use setpath64

del obj\*




rem build realWinTime.o:
g++ ^
 ..\src\adautils\realTime\realWinTime.cpp -c -fPIC ^
 -I..\src\adautils\realTime

move realWinTime.o obj\


rem build cls.o
g++ ^
 ..\src\adabindings\win00\cls.cc -c -fPIC

move cls.o obj\





rem build oal.o:
g++ ^
 ..\src\adabindings\adaOpenAL\OalBinding\oal.cpp -c ^
 -D obj ^
 -I..\src\adabindings\adaOpenAL\OalBinding ^
 -I..\src\adabindings\adaOpenAL\OalBinding\incoal

move oal.o obj\


rem next, create terminals.o
gcc ..\src\adautils\terminals.c -c

move terminals.o obj\




rem ----------------------------------------
rem libs\w64ming\ are all static
rem ----------------------------------------

gnatmake.exe adaventure ^
 -o adaventure64 ^
 -O3 -gnat12 -m64 ^
 -D obj ^
 -I..\src ^
 -I..\src\adautils ^
 -I..\src\adautils\realTime ^
 -I..\src\adabindings\win00 ^
 -I..\src\adabindings\gl ^
 -I..\src\adabindings\glfwada ^
 -I..\src\adabindings\AdaPngLib ^
 -I..\src\adabindings\adaOpenAL ^
 -I..\src\adabindings\adaOpenAL\OalBinding ^
 -I..\src\adabindings\Tables ^
 -I..\src\adabindings\FreeTypeAda ^
 -largs ^
 obj\oal.o obj\realWinTime.o obj\cls.o ^
 obj\terminals.o ^
 -lOpenGL32 -lGdi32 -lwinmm ^
 	..\binw64\openal32.dll ^
 	..\binw64\libstdc++-6.dll ^
 	..\binw64\zlib1.dll ^
 	..\binw64\glext.lib ^
 	..\binw64\freetype.dll ^
	-L..\libs\w64ming ^
	-lglfw3dll

move adaventure64.exe ..\binw64\

rem removed: binw64\glfw3.dll ^
