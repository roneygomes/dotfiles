export CODE=$HOME/code
export PROJECTS=$CODE/projects
export TOOLS=$CODE/tools

# ------------------------------------------------------------------------------
# Java and Android
# ------------------------------------------------------------------------------
export JDK=$TOOLS/jdk
export JRE=$JDK/jre

export ANDROID_SDK=$TOOLS/android-sdk-linux

# ------------------------------------------------------------------------------
# Path 
# ------------------------------------------------------------------------------
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
