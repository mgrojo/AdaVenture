
# Mainstream script
#
# Flawless:
# oal.o built with Xcode g++
# remainder built with AdaCore Ada

#!/bin/sh


if [ -d ./obj/ ]; then
	rm ./obj/*
else
	mkdir obj
fi


# use the Xcode C++ compiler...
# first, create oal.o:
g++ src/adabindings/adaOpenAL/OalBinding/oal.cpp -c \
-D obj \
-Isrc/adabindings/adaOpenAL/OalBinding \
-Isrc/adabindings/adaOpenAL/OalBinding/incoal



#--------------------------------------------------------


# set priority path to AdaCore compiler...
export PATH=$HOME/opt/GNAT/2020/bin:$PATH


# note that using Xcode to build oal.o
# requires using -lc++ rather than -lstdc++


gnatmake  adaventure -O3  \
-o adaventure_osx \
-D $PWD/obj \
-I$PWD/src \
-I$PWD/src/adautils \
-I$PWD/src/adabindings/gl \
-I$PWD/src/adabindings/glfwada \
-I$PWD/src/adabindings/AdaPngLib \
-I$PWD/src/adabindings/adaOpenAL \
-I$PWD/src/adabindings/adaOpenAL/OalBinding \
-I$PWD/src/adabindings/Tables \
-I$PWD/src/adabindings/FreeTypeAda \
-largs \
oal.o \
$PWD/libs/osx/libglfw3.a \
$PWD/libs/osx/libfreetype.a \
$PWD/libs/osx/libpng16.a \
$PWD/libs/osx/libbz2.a \
\
-lm -lz -lc++ \
\
-framework OpenGL \
-framework ForceFeedback \
-framework CoreFoundation \
-framework Carbon \
-framework Cocoa \
-framework QuartzCore \
-framework IOKit \
-framework CoreAudio \
-framework AudioUnit \
-framework AudioToolBox \
-framework Metal \
-framework OpenAL \
-pthread

# -- Copyright (C) 2022  <fastrgv@gmail.com>
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

