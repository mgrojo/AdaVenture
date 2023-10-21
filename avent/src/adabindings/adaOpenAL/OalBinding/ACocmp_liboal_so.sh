
# Example script to
# create liboal.so using AdaCore on OSX

# set priority path to AdaCore compiler...
export PATH=$HOME/opt/GNAT/2021/bin:$PATH



#create oal.o:
g++ oal.cpp -c \
-fPIC \
-Iincoal \
-I.



export SDKROOT=$(xcrun --show-sdk-path)

export FWROOT=/System/Library/Frameworks


# note that we use -lstdc++
# I believe using -lc++ is Ok too.

# note also that we MUST properly set
# the install_name so that the executable
# can find this library at runtime.

#create liboal.so:
g++ \
-shared -o liboal.so oal.o \
\
-L$SDKROOT/usr/lib \
-lm -lz -lstdc++ \
\
-Xlinker -install_name -Xlinker '@executable_path/libs/osx/liboal.so' \
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

cp liboal.so ../../../../libs/osx/

