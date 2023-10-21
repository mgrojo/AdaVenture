
# Build script for OSX using gcc v11.2.0 gnat
# Example script that avoids Xcode g++:
#
# 100% GNU (no Xcode compilers):
#

# Here, I now give specific 
# paths to libraries & frameworks.


#!/bin/sh

# this ensures a complete recompilation:
if [ -d ./obj/ ]; then
	rm ./obj/*
else
	mkdir obj
fi

export GROOT=/Users/rufascube/opt/GNAT/2018/bin
export PATH=$GROOT:$PATH



# first, create oal.o:
$GROOT/g++ \
$PWD/../src/adabindings/adaOpenAL/OalBinding/oal.cpp -c \
-I$PWD/../src/adabindings/adaOpenAL/OalBinding \
-I$PWD/../src/adabindings/adaOpenAL/OalBinding/incoal

# next create terminals.o:
$GROOT/gcc $PWD/../src/adautils/terminals.c -c


#---------------------------------------------------

export FWROOT=/System/Library/Frameworks
export SDKROOT=$(xcrun --show-sdk-path)


# note that this setup requires
# -lstdc++ rather than -lc++


$GROOT/gnatmake  adaventure -O3  \
-o adaventure_osx \
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
oal.o \
terminals.o \
$PWD/../libs/osx/libglfw3.a \
$PWD/../libs/osx/libfreetype.a \
$PWD/../libs/osx/libpng16.a \
$PWD/../libs/osx/libbz2.a \
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

mv adaventure_osx ..

#rm *.o

