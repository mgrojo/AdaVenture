
# Build script for OSX using gcc v11.2.0 gnat
# Example script that avoids Xcode g++:
#
# 100% GNU (no Xcode compilers):
#

# In nov2021 my Xcode was upgraded to 10.1 but
# my MacBookPro TBD files became "out-of-sync"
# with SDK libraries. Here, I now give specific 
# paths to libraries & frameworks.


#!/bin/sh

# this ensures a complete recompilation:
if [ -d ./obj/ ]; then
	rm ./obj/*
else
	mkdir obj
fi

export GROOT=/opt/gcc-11.2.0/bin
export PATH=$GROOT:$PATH



# first, create oal.o:
$GROOT/g++ \
src/adabindings/adaOpenAL/OalBinding/oal.cpp -c \
-Isrc/adabindings/adaOpenAL/OalBinding \
-Isrc/adabindings/adaOpenAL/OalBinding/incoal


#---------------------------------------------------

export SDKROOT=$(xcrun --show-sdk-path)

export FWROOT=/System/Library/Frameworks


# note that this setup requires
# -lstdc++ rather than -lc++


$GROOT/gnatmake  adaventure -O3  \
-o adaventure_osx_gn \
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

