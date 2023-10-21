
# Mainline script for Linux
#
# linux compile script for adaventure
# suitable for gnat (Gnu Ada)
#
# uses rpath to tell runtime linker to look also in
# ./libs/gnu/ for any non-standard *.so files 
# not found elsewhere, eg libglfw.so

#########################################################
#
# use this to ensure a complete recompilation:
if [ -d ./obj/ ]; then
	rm ./obj/*
else
	mkdir obj
fi




# using [old] AdaCore 2016 on mint...
# works for old linux distros:
export PATH=$HOME/opt/GNAT/2016/bin:$PATH


# first, create oal.o:
g++ ../src/adabindings/adaOpenAL/OalBinding/oal.cpp -c \
-D obj \
-I../src/adabindings/adaOpenAL/OalBinding \
-I../src/adabindings/adaOpenAL/OalBinding/incoal



# next, create terminals.o
gcc ../src/adautils/terminals.c -c \







gnatmake adaventure -o adaventure_gnu \
-O3 -gnat12 \
-D $PWD/obj \
-I$PWD/../src \
-I$PWD/../src/adautils \
-I$PWD/../src/adautils/fakeTime \
-I$PWD/../src/adabindings/fake00 \
-I$PWD/../src/adabindings/gl \
-I$PWD/../src/adabindings/glfwada \
-I$PWD/../src/adabindings/AdaPngLib \
-I$PWD/../src/adabindings/adaOpenAL \
-I$PWD/../src/adabindings/adaOpenAL/OalBinding \
-I$PWD/../src/adabindings/Tables \
-I$PWD/../src/adabindings/FreeTypeAda \
-largs \
terminals.o \
oal.o \
-lGL -lpthread -lstdc++ -lm -lz -lX11 \
-lopenal \
-Wl,-rpath,'$ORIGIN/libs/gnu' \
-L$PWD/../libs/gnu \
-lfreetype -lpng15 \
-lglfw

mv adaventure_gnu ..

rm *.o

# Note that 9 standard libs must be found on target
# system in order to build & execute on linux:
#	(explicit:)
# libopenal.so.1
# libpthread.so.0
# libstdc++.so.6
# libz.so.1
# libm.so.6 
#	(implicit:)
# libdl.so.2
# librt.so.1 
# libc.so.6
# libgcc_s.so.1

# the following rpath version is not understood by GNAT:
#-Xlinker -rpath='$ORIGIN/libs/gnu' \

# -- Copyright (C) 2023  <fastrgv@gmail.com>
# --
# -- This program is free software: you can redistribute it and/or modify
# -- it under the terms of the GNU General Public License as published by
# -- the Free Software Foundation, either version 3 of the License, or
# -- (at your option) any later version.
# --
# -- This program is distributed in the hope that it will be useful,
# -- but WITHOUT ANY WARRANTY; without even the implied warranty of
# -- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# -- GNU General Public License for more details.
# --
# -- You may read the full text of the GNU General Public License
# -- at <http://www.gnu.org/licenses/>.

