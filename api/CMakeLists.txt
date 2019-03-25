set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 2.8)

if(APPLE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.7" CACHE STRING "Minimum OS X deployment version")
elseif(MSVC)
    set(CMAKE_SYSTEM_VERSION 8.1 CACHE TYPE INTERNAL FORCE)
endif()


if (UNIX AND NOT CMAKE_C_COMPILER)
    set(CMAKE_C_COMPILER gcc)
    set(CMAKE_CXX_COMPILER g++)
endif()

Project(SAM_API)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")


#include_directories(../src $ENV{LKDIR}/include $ENV{SSCDIR})
include_directories(. $ENV{SSCDIR} $ENV{SSCDIR}/ssc)

set(SAM_API_SRC
        ErrorHandler.h
        ErrorHandler.cpp
        SAM_api.h
		SAM_api.cpp
        visibility.h
        SAM_GenericSystem.h
        SAM_GenericSystem.cpp )
		

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)


# set build types for archive
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    add_library(SAM_apid SHARED ${SAM_API_SRC})
    set(SAM_API_LIB SAM_apid)

    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
else()
    add_library(SAM_api SHARED
            ${SAM_API_SRC})
    set(SAM_API_LIB SAM_api)
endif()

if(APPLE)
    set(CMAKE_SHARED_LIBRARY_SUFFIX ".so")
endif()

# setup linking dependencies and flags

# if running as combined build, make sure to link to projects
if (${CMAKE_PROJECT_NAME} STREQUAL system_advisor_model)
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        target_link_libraries(${SAM_API_LIB} sscd)
        set(SSC_LIB $ENV{SSCDIR}/ssc/sscd.dylib)
    else()
        target_link_libraries(${SAM_API_LIB} ssc)
        set(SSC_LIB $ENV{SSCDIR}/ssc/ssc.dylib)
    endif()
else()
    unset(SSC_LIB CACHE)
	
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        find_library( SSC_LIB
                NAMES sscd.dylib sscd.so sscd.lib
                PATHS $ENV{SSCDIR}/ssc $ENV{SSCDIR}/build_vs2017/x64/Debug)
    elseif(system_advisor_model_EXPORT)
        find_library( SSC_LIB
                NAMES libsscd.so libsscd.lib
                PATHS $ENV{SSCDIR}/ssc $ENV{SSCDIR}/build_vs2017/x64/Debug)
    else()
        find_library(SSC_LIB
                NAMES ssc.dylib ssc.so sscd.lib
                PATHS $ENV{SSCDIR}/ssc $ENV{SSCDIR}/ssc/build_vs2017/x64/Release )
    endif()
endif()


if(MSVC)
    add_compile_options(/W3 /wd4996)
    if (CMAKE_BUILD_TYPE STREQUAL Debug)
        add_compile_options(/MDd)
        add_compile_definitions(_DEBUG)
    else()
        add_compile_options(/MD /O2)
        add_compile_definitions(NDEBUG )
    endif()
    add_compile_definitions( __DLL__ __64BIT__ __WINDOWS__ _WINDOWS _CRT_NON_CONFORMING_SWPRINTFS=1
        _SCL_SECURE_NO_WARNINGS=1 _UNICODE NOPCH)
else()
	#setup compile options and linking
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -std=c++11" )
    target_link_libraries(${SAM_API_LIB} -lm -lcurl)
endif()

add_definitions(-Dsystem_advisor_model_EXPORT)
target_link_libraries(${SAM_API_LIB} ${SSC_LIB})

add_subdirectory(test)

# copy needed dlls to sam/api/test and PySAM
# windows applications prefer the dlls in same path
if (MSVC)
    add_custom_command(
            TARGET ${SAM_API_LIB}
            POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
            $ENV{SSCDIR}/build_vs2017/x64/Debug/sscd.dll ${CMAKE_CURRENT_SOURCE_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
            $ENV{SAMNTDIR}/api/SAM_apid.dll ${CMAKE_CURRENT_SOURCE_DIR}
    )
endif()

# PySAM setup.py
if (system_advisor_model_EXPORT)
    if (MSVC)
        add_custom_command(
                TARGET ${SAM_API_LIB}
                POST_BUILD
                COMMAND ${CMAKE_COMMAND} -E copy_if_different
                ${SSC_LIB} $ENV{PYSAMDIR}/lib/
                COMMAND ${CMAKE_COMMAND} -E copy_if_different
                 $ENV{SAMNTDIR}/api/SAM_apid.dll $ENV{PYSAMDIR}/lib/
        )
    elseif(UNIX)
        add_custom_command(
                TARGET ${SAM_API_LIB}
                POST_BUILD
                COMMAND ${CMAKE_COMMAND} -E copy_if_different
                ${SSC_LIB} $ENV{PYSAMDIR}/lib/
                COMMAND ${CMAKE_COMMAND} -E copy_if_different
                $ENV{SAMNTDIR}/api/libSAM_apid.so $ENV{PYSAMDIR}/lib/
        )
    endif()
endif()