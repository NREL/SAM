#!/bin/bash

function log_message
{
	# Severity is first argument.
	echo ${@:2}
	echo "$(date) - $1 - ${@:2}" >> $LOG_FILE
}

function debug
{
	if [ $VERBOSE == 1 ]; then
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

	if [[ "$OSTYPE" == "linux-gnu" ]]; then
		num_procs=$(nproc)
	elif [[ "$OSTYPE" == "darwin"* ]]; then
		num_procs=$(sysctl -n hw.ncpu)
	else
		error "unknown OS type $OSTYPE"
		exit 1
	fi

	make -j$num_procs > make.log 2>&1
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

function install_pip
{
	run_command "$PIP install --upgrade pip"
	info "Finished installing packages"
}

function show_help
{
	echo "Usage:  $0"
}

### MAIN ###
LOG_FILE="/tmp/python-installation.log"
> $LOG_FILE

if [ -z $1 ]; then
	INSTALL_BASE=.
else
	INSTALL_BASE=$1
	if [ ! -d $INSTALL_BASE ]; then
		error "path $INSTALL_BASE does not exist"
		exit 1
	fi
fi


if [ -z $VERBOSE ]; then
	VERBOSE=0
fi

# TODO: allow caller to pass in the python version
PYTHON_VERSION=3.7
PYTHON_VERSION_FULL=3.7.4
PYTHON_SRC=Python-$PYTHON_VERSION_FULL
PYTHON_SRC_PATH=/tmp/$PYTHON_SRC
PYTHON_PACKAGE_NAME=$PYTHON_SRC.tgz
PYTHON_PACKAGE_PATH=/tmp/$PYTHON_PACKAGE_NAME
PYTHON_SRC_URL=https://www.python.org/ftp/python/$PYTHON_VERSION_FULL/$PYTHON_PACKAGE_NAME
INSTALL_BASE=$INSTALL_BASE/python
if [ ! -d $INSTALL_BASE ]; then
	mkdir $INSTALL_BASE
fi
INSTALL_PATH=$(realpath $INSTALL_BASE/python-$PYTHON_VERSION_FULL)
PIP=$INSTALL_PATH/bin/pip$PYTHON_VERSION

debug "PYTHON_VERSION=$PYTHON_VERSION"
debug "PYTHON_PACKAGE_NAME=$PYTHON_PACKAGE_NAME"
debug "PYTHON_PACKAGE_PATH=$PYTHON_PACKAGE_PATH"
debug "PYTHON_SRC_URL=$PYTHON_SRC_URL"
debug "INSTALL_PATH=$INSTALL_PATH"
debug "PIP=$PIP"

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
run_command "rm $PYTHON_PACKAGE_PATH"
install_pip
info "Finished installation"
