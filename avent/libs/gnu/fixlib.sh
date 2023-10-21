
# this library adjustment is not needed using AdaCore,
# Ada, but seems to be necessary for GNU compilers:

patchelf --set-rpath '$ORIGIN' libfreetype.so.6.16.0

# this tells libfreetype* where to find a dependency:
# libpng15* (in the same directory).
#
# patchelf --print-rpath <sharedLibFile>
# gives info only

