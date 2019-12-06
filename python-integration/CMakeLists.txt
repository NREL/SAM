#####################################################################################################################
#
# CMake and System Settings
#
#####################################################################################################################

set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 3.11)

set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")

if (UNIX AND NOT CMAKE_C_COMPILER)
	set(CMAKE_C_COMPILER gcc)
	set(CMAKE_CXX_COMPILER g++)
endif()


#####################################################################################################################
#
# Project Settings 
#
#####################################################################################################################

Project(python-integration)

# use PYTHONHOME from cmdline for now, but use SAM's installation of Python in future
#set(PYTHONHOME $ENV{PYTHONHOME})

if (MSVC)
	execute_process(COMMAND CMD /c ${PYTHONDIR}/bin/python3.7-config --cflags OUTPUT_VARIABLE PYTHON_CFLAGS)
	execute_process(COMMAND CMD /c ${PYTHONDIR}/bin/python3.7-config --ldflags OUTPUT_VARIABLE PYTHON_LDFLAGS)
else()
	execute_process(COMMAND ${PYTHONDIR}/bin/python3.7-config --cflags OUTPUT_VARIABLE PYTHON_CFLAGS)
	execute_process(COMMAND ${PYTHONDIR}/bin/python3.7-config --ldflags OUTPUT_VARIABLE PYTHON_LDFLAGS)	
	if (NOT APPLE)
		string(REPLACE "-lpython3.7m" ";-lpython3.7m" PYTHON_LDFLAGS ${PYTHON_LDFLAGS})
		list(GET PYTHON_LDFLAGS 1 PYTHON_LINKLIBS)
		list(GET PYTHON_LDFLAGS 0 PYTHON_LDFLAGS)
		string(STRIP ${PYTHON_LINKLIBS} PYTHON_LINKLIBS)
	endif()
endif()

string(STRIP ${PYTHON_CFLAGS} PYTHON_CFLAGS)
string(STRIP ${PYTHON_LDFLAGS} PYTHON_LDFLAGS)

message("${PYTHON_CFLAGS}")
message("${PYTHON_LDFLAGS}")
message("${PYTHON_LINKLIBS}")

#####################################################################################################################
#
# Compile Options per Platform
#
#####################################################################################################################

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)


#####################################################################################################################
# 
# CMake Targets
#
#####################################################################################################################

add_executable(SAM-Python
			lib_util.cpp
			main.cpp
			vartab.cpp)

set_target_properties(SAM-Python 
	PROPERTIES 
	COMPILE_FLAGS ${PYTHON_CFLAGS}
	LINK_FLAGS ${PYTHON_LDFLAGS}
	)

target_link_libraries(SAM-Python ${PYTHON_LINKLIBS})

set( CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR} )
