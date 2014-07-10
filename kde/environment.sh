# In order to have this file available throughout your whole session place a
# copy or a link to it in $HOME/.kde/env not forgetting it shall have the .sh
# file extension.

export CODE=$HOME/code

# ------------------------------------------------------------------------------
# Java and Android
# ------------------------------------------------------------------------------
export JDK=$CODE/jdk
export JRE=$JDK/jre

export ANDROID_SDK=$CODE/android-sdk-linux

export PATH=$PATH:$HOME/bin

export PATH=$PATH:$JDK/bin
export PATH=$PATH:$JRE/bin

export PATH=$PATH:$ANDROID_SDK/tools
export PATH=$PATH:$ANDROID_SDK/platform-tools

# ------------------------------------------------------------------------------
# C and C++ 
# ------------------------------------------------------------------------------
export CC=/usr/bin/clang
export CXX=/usr/bin/clang++

# ------------------------------------------------------------------------------
# KDE
# ------------------------------------------------------------------------------
export KDE=$CODE/projects/kde
export CMAKE_PREFIX_PATH=$CMAKE_PREFIX_PATH:$KDE/inst
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$KDE/inst/lib
