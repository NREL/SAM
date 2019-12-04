#!/bin/bash

function log_message
{
	# Severity is first argument.
	echo ${@:2}
	echo "$(date) - $1 - ${@:2}" >> $LOG_FILE
}

function debug
{
	if [ $VERBOSE != 0 ]; then
		log_message "DEBUG" $@
	fi
}

function info
{
	log_message "INFO" $@
}

function error
{
	log_message "ERROR" $@
}

function run_command
{
	debug "run command [$@]"
	$@
	ret=$?
	if [ $ret != 0 ]; then
		error "command=[$@] failed return_code=$ret"
		exit $ret
	fi
}

function download_python_src
{
	run_command "curl $PYTHON_SRC_URL -o $PYTHON_PACKAGE_PATH"
	info "Finished downloading $PYTHON_PACKAGE_NAME"
}

function extract_package
{
	cd $(dirname $1)
	run_command "tar -xzf $1"
	if [ ! -d $2 ]; then
		error "extracting $1 did not produce $2"
		exit 1
	fi
	cd -
	info "Finished extracting $1 to $2"
}

function build_python
{
	info "Building Python $PYTHON_VERSION_FULL"
	cd $PYTHON_SRC_PATH
	./configure --prefix=$INSTALL_PATH > /dev/null 2>&1
	ret=$?
	if [ $ret != 0 ]; then
		error "configure failed, return_code=$ret. Refer to $(realpath $(pwd)/config.log)"
		exit ret
	fi
	make -j$(nproc) > make.log 2>&1
	ret=$?
	if [ $ret != 0 ]; then
		error "make failed, return_code=$ret. Refer to $(realpath $(pwd)/make.log)"
		exit ret
	fi
	make install > make-install.log 2>&1
	ret=$?
	if [ $ret != 0 ]; then
		error "make install failed, return_code=$ret. Refer to $(realpath $(pwd)/make-install.log)"
		exit ret
	fi
	cd -
	run_command "rm -rf $PYTHON_SRC_PATH"
	info "Finished building Python $PYTHON_VERSION_FULL"
}

function install_packages
{
	run_command "$PIP install --upgrade pip"
	install_land_bosse
	info "Finished installing packages"
}

function install_land_bosse
{
	pip=$(realpath $PIP)
	cd $SITE_PACKAGES
	run_command "wget https://github.com/WISDEM/LandBOSSE/archive/$LAND_BOSSE_VERSION.tar.gz"
	extract_package "$LAND_BOSSE_VERSION.tar.gz" "LandBOSSE-$LAND_BOSSE_VERSION"
	cd LandBOSSE-$LAND_BOSSE_VERSION
	run_command "$pip install --quiet -e ."
	run_command rm -rf LandBOSSE-$LAND_BOSSE_VERSION $LAND_BOSSE_VERSION.tar.gz
	cd -
	info "Finished installing LandBOSSE"
}

### MAIN ###
LOG_FILE="/tmp/python-installation.log"
> $LOG_FILE

if [ -z $2 ]; then
	error "Usage:  $0 PYTHON_INSTALL_DIRECTORY LAND_BOSSE_VERSION"
	exit 1
fi

# TODO: allow caller to pass in the python version
PYTHON_VERSION=3.7
PYTHON_VERSION_FULL=3.7.4
PYTHON_SRC=Python-$PYTHON_VERSION_FULL
PYTHON_SRC_PATH=/tmp/$PYTHON_SRC
PYTHON_PACKAGE_NAME=$PYTHON_SRC.tgz
PYTHON_PACKAGE_PATH=/tmp/$PYTHON_PACKAGE_NAME
PYTHON_SRC_URL=https://www.python.org/ftp/python/$PYTHON_VERSION_FULL/$PYTHON_PACKAGE_NAME
INSTALL_PATH=$1/python-$PYTHON_VERSION_FULL
PIP=$INSTALL_PATH/bin/pip$PYTHON_VERSION
SITE_PACKAGES=$INSTALL_PATH/lib/python$PYTHON_VERSION/site-packages
LAND_BOSSE_VERSION=$2
if [ -z $VERBOSE ]; then
	VERBOSE=0
fi

debug "PYTHON_VERSION=$PYTHON_VERSION"
debug "PYTHON_PACKAGE_NAME=$PYTHON_PACKAGE_NAME"
debug "PYTHON_PACKAGE_PATH=$PYTHON_PACKAGE_PATH"
debug "PYTHON_SRC_URL=$PYTHON_SRC_URL"
debug "INSTALL_PATH=$INSTALL_PATH"
debug "PIP=$PIP"
debug "SITE_PACKAGES=$SITE_PACKAGES"

if [ ! -d $INSTALL_PATH ]; then
	mkdir -p $INSTALL_PATH
fi

if [ ! -f $PYTHON_PACKAGE_PATH ]; then
	download_python_src
fi

if [ ! -d $PYTHON_SRC_PATH ]; then
	extract_package $PYTHON_PACKAGE_PATH $PYTHON_SRC_PATH
fi

build_python
# TODO: delete the Python tarball
install_packages
info "Finished installation"
