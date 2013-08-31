###
## A script to setup some needed variables and functions for KDE 4 development.
###

# Uncomment if building on a 64 bit system
export LIB_SUFFIX=64

# Set where your base KDE development folder is located, usually ~/kde
export BASEDIR=~/repos/kde

# Give the build a name, e.g. master, 4.6, debug, etc
# export BUILDNAME=master

# Set up which Qt to use
# Use the system Qt, adjust path as required
# export QTDIR=/usr

# Uncomment to use your own build of qt-kde
export QTDIR=$HOME/Qt-4.8
export PATH=$QTDIR/bin:$PATH
export LD_LIBRARY_PATH=$QTDIR/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=$QTDIR/lib/pkgconfig:$PKG_CONFIG_PATH

# Set up the KDE paths
export KDE_SRC=$BASEDIR/src
export KDE_BUILD=$BASEDIR/build
export KDEDIR=$BASEDIR/inst
export KDEDIRS=$KDEDIR
export KDEHOME=$BASEDIR/home
export KDETMP=/tmp/$USER
export KDEVARTMP=/var/tmp/$USER

mkdir -p $KDETMP
mkdir -p $KDEVARTMP

# Add the KDE plugins to the Qt plugins path
export QT_PLUGIN_PATH=$KDEDIR/lib/kde4/plugins

# Do we really need these?
export KDE4_DBUS_INTERFACES_DIR=$KDEDIR/share/dbus-1/interfaces
export PYTHON_SITE_PACKAGES_DIR=$KDEDIR/lib/python2.6/site-packages/PyKDE4

# Export the standard paths to include KDE
export PATH=$KDEDIR/bin:$PATH
export LD_LIBRARY_PATH=$KDEDIR/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=$KDEDIR/lib/pkgconfig:$PKG_CONFIG_PATH

# Export the CMake paths so it searches for KDE in the right places
export CMAKE_PREFIX_PATH=$KDEDIR:$CMAKE_PREFIX_PATH
export CMAKE_LIBRARY_PATH=$KDEDIR/lib:$CMAKE_LIBRARY_PATH
export CMAKE_INCLUDE_PATH=$KDEDIR/include:$CMAKE_INCLUDE_PATH

# Unset XDG to avoid seeing KDE files from /usr
# If unset then you must install shared-mime-info
# unset XDG_DATA_DIRS
# unset XDG_CONFIG_DIRS


# Uncomment if you are using Icecream for distributed compiling
#export PATH=/opt/icecream/bin:$PATH

CURRENT_SHELL=$(echo $0)

prepend() { [ -d "$2" ] && eval $1=\"$2\$\{$1:+':'\$$1\}\" && export $1 ; }

# This will make the debug output prettier
export KDE_COLOR_DEBUG=1
export QTEST_COLORED=1

# Make
# Tell many scripts how to switch from source dir to build dir:
export OBJ_REPLACEMENT="s#$KDE_SRC#$KDE_BUILD#"

# Use makeobj instead of make, to automatically switch to the build dir.
# If you don't have makeobj, install the package named kdesdk-scripts or
# kdesdk, or check out kdesdk/scripts from svn, or just don't set the alias
# yet.
alias make=makeobj

##
# A function to easily build the current directory of KDE.
#
# This builds only the sources in the current ~/{src,build}/KDE subdirectory.
# Usage: cs KDE/kdebase && cmakekde
#   will build/rebuild the sources in ~/src/KDE/kdebase
##
function cmakekde {
    if test -n "$1"; then
        # srcFolder is defined via command line argument
        srcFolder="$1"
    else
        # get srcFolder for current dir
        srcFolder=`pwd | sed -e s,$KDE_BUILD,$KDE_SRC,`
    fi
    # we are in the src folder, change to build directory
    # Alternatively, we could just use makeobj in the commands below...
    current=`pwd`
    if [ "$srcFolder" = "$current" ]; then
        cb
    fi
    # To disable tests, remove -DKDE4_BUILD_TESTS=TRUE
    # To save disk space change "debugfull" to "debug"
    cmake "$srcFolder" \
          -DCMAKE_INSTALL_PREFIX=$KDEDIR \
          -DKDE4_AUTH_POLICY_FILES_INSTALL_DIR=$KDEDIR/share/polkit-1/actions \
          -DKDE4_BUILD_TESTS=TRUE \
          -DCMAKE_BUILD_TYPE=debugfull

        # Comment out the following two lines to stop builds waiting after
        # the configuration step, so that the user can check configure output
        # echo "Press <ENTER> to continue..."
        # read userinput

        # Note: To speed up compiling, change 'make -j2' to 'make -jx',
        #   where x is your number of processors +1
        nice make -j5 && make install
        #Use this line instead if using icecream
        #nice make CC=icecc -j6 && make install
        RETURN=$?
        cs
        return ${RETURN}
}

##
# A function to easily build the current directory of KDE.
#
# This builds only the sources in the current ~/{src,build}/KDE subdirectory.
# Usage: cs KDE/kdebase && kdebuild
#   will build/rebuild the sources in ~/src/KDE/kdebase
##
function kdebuild {
    if test -n "$1"; then
        # srcFolder is defined via command line argument
        srcFolder="$1"
    else
        # get srcFolder for current dir
        srcFolder=`pwd | sed -e s,$KDE_BUILD,$KDE_SRC,`
    fi
    # we are in the src folder, change to build directory
    # Alternatively, we could just use makeobj in the commands below...
    current=`pwd`
    if [ "$srcFolder" = "$current" ]; then
        cb
    fi
    # To disable tests, remove -DKDE4_BUILD_TESTS=TRUE
    # To save disk space change "debugfull" to "debug"
    cmake "$srcFolder" \
          -DCMAKE_INSTALL_PREFIX=$KDEDIR \
          -DKDE4_AUTH_POLICY_FILES_INSTALL_DIR=$KDEDIR/share/polkit-1/actions \
          -DKDE4_BUILD_TESTS=TRUE \
          -DCMAKE_BUILD_TYPE=debugfull

    # Comment out the following two lines to stop builds waiting after
    # the configuration step, so that the user can check configure output
    # echo "Press <ENTER> to continue..."
    # read userinput

    # Note: To speed up compiling, change 'make -j2' to 'make -jx',
    #   where x is your number of processors +1
    nice make -j5 && make install
    #Use this line instead if using icecream
    #nice make CC=icecc -j6 && make install
    RETURN=$?
    cs
    return ${RETURN}
}

##
# A function to easily run cmake for KDE configuration
##
function kdecmake {
    if test -n "$1"; then
        # srcFolder is defined via command line argument
        srcFolder="$1"
    else
        # get srcFolder for current dir
        srcFolder=`pwd | sed -e s,$KDE_BUILD,$KDE_SRC,`
    fi
    # we are in the src folder, change to build directory
    # Alternatively, we could just use makeobj in the commands below...
    current=`pwd`
    if [ "$srcFolder" = "$current" ]; then
        cb
    fi
    # To disable tests, remove -DKDE4_BUILD_TESTS=TRUE
    # To save disk space change "debugfull" to "debug"
    cmake "$srcFolder" \
          -DCMAKE_INSTALL_PREFIX=$KDEDIR \
          -DKDE4_AUTH_POLICY_FILES_INSTALL_DIR=$KDEDIR/share/polkit-1/actions \
          -DKDE4_BUILD_TESTS=TRUE \
          -DCMAKE_BUILD_TYPE=debugfull
    RETURN=$?
    cs
    return ${RETURN}
}

##
# A function to easily make and install the current or selected directory of KDE.
##
function kdemake {
    if test -n "$1"; then
        # srcFolder is defined via command line argument
        srcFolder="$1"
    else
        # get srcFolder for current dir
        srcFolder=`pwd | sed -e s,$KDE_BUILD,$KDE_SRC,`
    fi
    # we are in the src folder, change to build directory
    # Alternatively, we could just use makeobj in the commands below...
    current=`pwd`
    if [ "$srcFolder" = "$current" ]; then
        cb
    fi

    # Note: To speed up compiling, change 'make -j2' to 'make -jx',
    #   where x is your number of processors +1
    nice make -j5 && make install
    #Use this line instead if using icecream
    #nice make CC=icecc -j6 && make install
    RETURN=$?
    cs
    return ${RETURN}
}

function cd() {
  if test -z "$1"; then
    builtin cd
  elif test -z "$2"; then
    builtin cd "$1"
  else
    builtin cd "$1" "$2"
  fi
  _f=`findup .build-config`
  if test -n "$_f" -a "$_lastf" != "$_f"; then
    echo "Loading $_f"
    _lastf="$_f"
    source "$_f"
  fi
}

##
# A function to easily change to the build directory.
# Usage: cb KDE/kdebase
#   will change to $KDE_BUILD/KDE/kdebase
# Usage: cb
#   will simply go to the build folder if you are currently in a src folder
#   Example:
#     $ pwd
#     /home/user/src/KDE/kdebase
#     $ cb && pwd
#     /home/user/build/KDE/kdebase
#
function cb {
        local dest

    # Make sure build directory exists.
    mkdir -p "$KDE_BUILD"

    # command line argument
    if test -n "$1"; then
        cd "$KDE_BUILD/$1"
        return
    fi
    # substitute src dir with build dir
    dest=`pwd | sed -e s,$KDE_SRC,$KDE_BUILD,`
    if test ! -d "$dest"; then
        # build directory does not exist, create
        mkdir -p "$dest"
    fi
    cd "$dest"
}

##
# Change to the source directory.  Same as cb, except this
# switches to $KDE_SRC instead of $KDE_BUILD.
# Usage: cs KDE/kdebase
#   will change to $KDE_SRC/KDE/kdebase
# Usage: cs
#   will simply go to the source folder if you are currently in a build folder
#   Example:
#     $ pwd
#     /home/myuser/kde/build/master/KDE/kdebase
#     $ cs && pwd
#     /home/myuser/kde/src/master/KDE/kdebase
#
function cs {
        local dest current

    # Make sure source directory exists.
    mkdir -p "$KDE_SRC"

    # command line argument
    if test -n "$1"; then
        cd "$KDE_SRC/$1"
    else
        # substitute build dir with src dir
        dest=`pwd | sed -e s,$KDE_BUILD,$KDE_SRC,`
        current=`pwd`
        if [ "$dest" = "$current" ]; then
            cd "$KDE_SRC"
        else
            cd "$dest"
        fi
    fi
}

##
# Add autocompletion to cs function
#
function _cs_scandir
{
        local base ext

    base=$1
    ext=$2
    if [ -d $base ]; then
        for d in `ls $base`; do
            if [ -d $base/$d ]; then
                dirs="$dirs $ext$d/"
            fi
        done
    fi
}

function _cs()
{
    local cur dirs
    _cs_scandir "$KDE_SRC"
    _cs_scandir "$KDE_SRC/KDE" "KDE/"
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    COMPREPLY=( $(compgen -W "${dirs}" -- ${cur}) )
}

svndiff ()
{
    svn diff "$*" | colordiff | less;
}

# Setup shell
if [ "$CURRENT_SHELL" = "bash" ]; then
    complete -F _cs cs
fi
