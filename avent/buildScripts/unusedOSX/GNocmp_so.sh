
# Build script for OSX using gcc v11.2.0 gnat
# Example script that avoids using Xcode g++
# and uses shared library liboal.so

# 100% GNU using shared OpenAL binding...
# works nicely using liboal.so
# providing its install_name is properly set.
#

# In nov2021 my Xcode was upgraded to 10.1 but
# my MacBookPro TBD files became "out-of-sync"
# with SDK libraries. I must now give specific path
# to libraries & frameworks.


#!/bin/sh

# this ensures a complete recompilation:
if [ -d ./obj/ ]; then
	rm ./obj/*
else
	mkdir obj
fi

export GROOT=/opt/gcc-11.2.0/bin
export PATH=$GROOT:$PATH


export SDKROOT=$(xcrun --show-sdk-path)

export FWROOT=/System/Library/Frameworks


# now using -lstdc++
# I believe using -lc++ is Ok too.

# assumes liboal.so is present...
# make sure it was generated using Xcode g++ !


$GROOT/gnatmake  adaventure -O3  \
-o adaventure_osx_GNU \
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
$PWD/libs/osx/libglfw3.a \
$PWD/libs/osx/libfreetype.a \
$PWD/libs/osx/libpng16.a \
$PWD/libs/osx/libbz2.a \
$PWD/libs/osx/liboal.so \
\
-L$SDKROOT/usr/lib \
-lm -lz -lstdc++ \
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

# next line was NOT necessary:
#-Xlinker -rpath -Xlinker '@executable_path/libs/osx' \
# but the install_path of liboal.so must be set properly
# ...see ./src/adabindings/adaOpenAL/OalBinding/ocmp.sh

