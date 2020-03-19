#!/bin/bash

# Installs Python via Miniconda for Linux and MacOS.

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

function show_help
{
	echo "Usage:  $0 PATH"
}

### MAIN ###
PYTHON_VERSION=37
CONDA_VERSION=4.8.2
LANDBOSSE_INSTALLER=install_landbosse.py
LOG_FILE="/tmp/install_python.log"
> $LOG_FILE

if [ -z $VERBOSE ]; then
	VERBOSE=0
fi

if [ -z $1 ]; then
	show_help
	exit 1
fi

INSTALL_BASE=$1

if [ ! -d $INSTALL_BASE ]; then
	error "path $INSTALL_BASE does not exist"
	exit 1
fi


if [[ "$OSTYPE" == "linux-gnu" ]]; then
	PLATFORM=Linux-x86_64
elif [[ "$OSTYPE" == "darwin"* ]]; then
	PLATFORM=MacOSX-x86_64
else
	error "unknown OS type $OSTYPE"
	exit 1
fi

CONDA_PACKAGE_NAME=Miniconda3-py${PYTHON_VERSION}_${CONDA_VERSION}-${PLATFORM}.sh
CONDA_URL=https://repo.anaconda.com/miniconda/$CONDA_PACKAGE_NAME
CONDA_PACKAGE_PATH=/tmp/$CONDA_PACKAGE_NAME

PYTHON_BASE_PATH=$INSTALL_BASE/python
if [ ! -d $PYTHON_BASE_PATH ]; then
	error "python base path does not exist:  $PYTHON_BASE_PATH"
	exit 1
fi
INSTALL_PATH=$(realpath $PYTHON_BASE_PATH/Miniconda-$CONDA_VERSION)
PIP=$INSTALL_PATH/bin/pip
PYTHON=$INSTALL_PATH/bin/python

debug "CONDA_URL=$CONDA_URL"
debug "CONDA_PACKAGE_NAME=$CONDA_PACKAGE_NAME"
debug "CONDA_PACKAGE_PATH=$CONDA_PACKAGE_PATH"
debug "INSTALL_PATH=$INSTALL_PATH"
debug "PIP=$PIP"

if [ ! -f $CONDA_PACKAGE_PATH ]; then
	run_command "curl $CONDA_URL -o $CONDA_PACKAGE_PATH"
	info "Finished downloading $CONDA_PACKAGE_NAME"
fi

# TODO: conda is redistributable. Do we need to publish their license somewhere?
run_command "bash $CONDA_PACKAGE_PATH -b -p $INSTALL_PATH"
run_command "$PYTHON $PYTHON_BASE_PATH/$LANDBOSSE_INSTALLER \
	--python-base-path=$INSTALL_BASE/runtime/python \
	--python-exec=$PYTHON \
	--pip=$PIP"
run_command "rm -rf $CONDA_PACKAGE_PATH"

info "Finished installation of LandBOSSE"
