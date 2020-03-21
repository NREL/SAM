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
	$@ > /dev/null
	ret=$?
	if [ $ret != 0 ]; then
		error "command=[$@] failed return_code=$ret"
		exit $ret
	fi
}

function update_config_file
{
	# Use python because it can handle JSON.
	script=/tmp/fix-config.py
	echo "
import json
import os
import sys

filename = sys.argv[1]
python_exec = sys.argv[2]
pip_exec = sys.argv[3]


with open(filename) as f_in:
    data = json.load(f_in)

data[\"exec_path\"] = python_exec
data[\"pip_path\"] = pip_exec

tmp_file = filename + \".tmp\"
with open(tmp_file, \"w\") as f_in:
    json.dump(data, f_in, indent=4)

os.rename(tmp_file, filename)" > $script

	run_command "$INSTALL_PATH/bin/python $script $CONFIG_FILE $INSTALL_PATH/bin/python $INSTALL_PATH/bin/pip"
	rm $script
}

function show_help
{
	echo "Usage:  $0 MINICONDA_VERSION PYTHON_VERSION PATH"
}

### MAIN ###

LOG_FILE="/tmp/install_python.log"
> $LOG_FILE

if [ -z $VERBOSE ]; then
	VERBOSE=0
fi

if [ -z $3 ]; then
	show_help
	exit 1
fi

CONDA_VERSION=$1
PYTHON_FULL_VERSION=$2
IFS="." read -ra VER_ARRAY <<< "$PYTHON_FULL_VERSION"
if [ ${#VER_ARRAY[@]} -lt 2 ] ; then
	error "invalid python version: format x.y.z"
	exit 1
fi
PYTHON_MAJOR_MINOR="${VER_ARRAY[0]}${VER_ARRAY[1]}"
INSTALL_BASE=$3

if [ ! -d $INSTALL_BASE ]; then
	error "path $INSTALL_BASE does not exist"
	exit 1
fi

cd $INSTALL_BASE
CONFIG_FILE=python_config.json

if [ ! -f $CONFIG_FILE ]; then
	error "config file $CONFIG_FILE does not exist"
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

CONDA_PACKAGE_NAME=Miniconda3-py${PYTHON_MAJOR_MINOR}_${CONDA_VERSION}-${PLATFORM}.sh
CONDA_URL=https://repo.anaconda.com/miniconda/$CONDA_PACKAGE_NAME
CONDA_PACKAGE_PATH=/tmp/$CONDA_PACKAGE_NAME

INSTALL_PATH=./Miniconda-${CONDA_VERSION}
PIP=$INSTALL_PATH/bin/pip
PYTHON=$INSTALL_PATH/bin/python

debug "PYTHON_FULL_VERSION=$PYTHON_FULL_VERSION"
debug "PYTHON_MAJOR_MINOR=$PYTHON_MAJOR_MINOR"
debug "CONDA_VERSION=$CONDA_VERSION"
debug "CONDA_PACKAGE_NAME=$CONDA_PACKAGE_NAME"
debug "CONDA_URL=$CONDA_URL"
debug "CONDA_PACKAGE_PATH=$CONDA_PACKAGE_PATH"
debug "INSTALL_PATH=$INSTALL_PATH"
debug "PIP=$PIP"

if [ ! -f $CONDA_PACKAGE_PATH ]; then
	run_command "curl $CONDA_URL -o $CONDA_PACKAGE_PATH"
	debug "Finished downloading $CONDA_PACKAGE_NAME"
fi

run_command "bash $CONDA_PACKAGE_PATH -b -p $INSTALL_PATH"
run_command "rm -rf $CONDA_PACKAGE_PATH"

update_config_file
debug "Finished installation of Python $PYTHON_FULL_VERSION"
