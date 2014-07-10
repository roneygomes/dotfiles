# ------------------------------------------------------------------------------
# KDE Frameworks 5
# ------------------------------------------------------------------------------
export KF5=$CODE/projects/kde/inst/kf5
export KDE_SESSION_VERSION=5
export KDE_FULL_SESSION=true

export QTDIR=$CODE/qt/qt-5.2/5.2.1/gcc_64/include

export QT_PLUGIN_PATH=$QT_PLUGIN_PATH:$QTDIR/plugins
export QT_PLUGIN_PATH=$QT_PLUGIN_PATH:$KF5/lib/plugins
export QT_PLUGIN_PATH=$QT_PLUGIN_PATH:$KF5/lib64/plugins
export QT_PLUGIN_PATH=$QT_PLUGIN_PATH:$KF5/lib/x86_64-linux-gnu/plugins

export QML2_IMPORT_PATH=$QML2_IMPORT_PATH:$KF5/lib/qml
export QML2_IMPORT_PATH=$QML2_IMPORT_PATH:$KF5/lib64/qml
export QML2_IMPORT_PATH=$QML2_IMPORT_PATH:$KF5/lib/x86_64-linux-gnu/qml
export QML2_IMPORT_PATH=$QML2_IMPORT_PATH:$QTDIR/qml
export QML_IMPORT_PATH=$QML2_IMPORT_PATH

export XDG_DATA_DIRS=$XDG_DATA_DIRS:$KF5/share
export XDG_DATA_DIRS=$XDG_DATA_DIRS:/usr/share

export XDG_CONFIG_DIRS=$XDG_CONFIG_DIRS:$KF5/etc/xdg
export XDG_CONFIG_DIRS=$XDG_CONFIG_DIRS:/etc/xdg

export PATH=$KF5/bin:$QTDIR/bin:$PATH
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$KF5/lib
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$KF5/lib/x86_64-linux-gnu
export CMAKE_PREFIX_PATH=$CMAKE_PREFIX_PATH:$KF5

