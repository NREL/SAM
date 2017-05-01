#! /bin/bash
#  Script to download wxWidgets, configure it, and configure some path variables to enable successful building of SAM

#  Pass optional command line arguments
#  1st argument - working directory where wxWidgets should be installed, otherwise assumes current directory
#  2nd argument - version of wxWidgets to install.  Should be 3.1.0 unless version is updated
#  Example usage: ./setup.sh /local/workspace/test 3.1.0

if [ -z "$1" ]; then
	WORKING_DIR=pwd
else
	WORKING_DIR="$1"
fi

if [ -z "$2" ]; then
	WX_VERSION="3.1.0"
else
	WX_VERSION="$2"
fi

INSTALL_DIR=$WORKING_DIR/$WX_VERSION
WX_BIN_DIR=$WORKING_DIR/wxbin
WX_WIDGETS_NAME=wxWidgets-$WX_VERSION

if [ ! -d "$WORKING_DIR" ]; then
	mkdir $WORKING_DIR
fi

if [ ! -d "$INSTALL_DIR" ]; then
	mkdir $INSTALL_DIR
fi

if [ ! -d "$WX_BIN_DIR" ]; then
	mkdir $WX_BIN_DIR
fi 

cd "$WORKING_DIR"

wget https://github.com/wxWidgets/wxWidgets/releases/download/v$WX_VERSION/$WX_WIDGETS_NAME.tar.bz2
tar -xvjf $WX_WIDGETS_NAME.tar.bz2
cd $WX_WIDGETS_NAME

./configure --prefix=$INSTALL_DIR --enable-shared=no --enable-stl=yes --enable-debug=no --with-gtk=2 --with-libjpeg=builtin --with-libpng=builtin --with-regex=builtin --with-libtiff=builtin --with-zlib=builtin --with-expat=builtin --without-libjbig --without-liblzma --without-gtkprint --with-libnotify=no --with-libmspack=no --with-gnomevfs=no --with-opengl=yes --with-sdl=no

make
make install

ln -s "$INSTALL_DIR/bin/wx-config" "$WX_BIN_DIR/wx-config-3"
export PATH=$PATH:$WX_BIN_DIR

