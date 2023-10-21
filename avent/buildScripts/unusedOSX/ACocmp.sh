
# Example script that avoids using Xcode g++:
#
# 100% AdaCore (no Xcode):
#
# Build script for OSX using AdaCore

# In nov2021 my Xcode was upgraded to 10.1 but
# my MacBookPro TBD files became "out-of-sync"
# with SDK libraries. Here, I give specific path
# to libraries & frameworks.


#!/bin/sh

# this ensures a complete recompilation:
if [ -d ./obj/ ]; then
	rm ./obj/*
else
	mkdir obj
fi

# set priority path to AdaCore compiler...
export PATH=$HOME/opt/GNAT/2020/bin:$PATH


# using the AdaCore g++ compiler...
# first, create oal.o:
g++ \
src/adabindings/adaOpenAL/OalBinding/oal.cpp -c \
-Isrc/adabindings/adaOpenAL/OalBinding \
-Isrc/adabindings/adaOpenAL/OalBinding/incoal


#---------------------------------------------------

# note that the compilation succeeds with
# or without these two specific paths...

export SDKROOT=$(xcrun --show-sdk-path)

export FWROOT=/System/Library/Frameworks

# note that this setup requires
# -lstdc++ rather than -lc++

gnatmake  adaventure -O3  \
-o adaventure_osx_ac \
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
-L$SDKROOT/usr/lib \
-lm -lz  -lstdc++ \
\
-F$FWROOT \
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

