set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 3.12)


if(APPLE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.13" CACHE STRING "Minimum OS X deployment version")
endif()

Project(SAM_API)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")


#include_directories(../src $ENV{LKDIR}/include $ENV{SSCDIR})
include_directories(. $ENV{SSCDIR})

set(SAM_API_SRC
        ErrorHandler.h
        ErrorHandler.cpp
        SAM_api.h
        visibility.h
        SAM_GenericSystem.h
        SAM_GenericSystem.cpp
        GenericSystem-External.h
        GenericSystem-External.cpp
        SystemLoader.cpp
        SystemLoader.h
        )

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# set build types for archive
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    add_library(SAM_apid SHARED
            ${SAM_API_SRC})
    set(SAM_API_LIB SAM_apid)

    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
else()
    add_library(SAM_api SHARED
            ${SAM_API_SRC})
    set(SAM_API_LIB SAM_apid)
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
                PATHS $ENV{SSCDIR}/ssc)
    else()
        find_library(SSC_LIB
                NAMES  ssc.dylib ssc.so ssc.lib
                PATHS $ENV{SSCDIR}/ssc)
    endif()
endif()
set_property(TARGET ${SAM_API_LIB} PROPERTY PREFIX "")


#setup compile options
target_compile_options(${SAM_API_LIB} PUBLIC -O3 -std=c++11 )


# setup linking dependencies and flags
target_link_libraries(${SAM_API_LIB} -lm -lcurl)
target_link_libraries(${SAM_API_LIB} ${SSC_LIB})


## make test

add_executable(SAM_api_test
        test.cpp)

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_link_libraries(SAM_api_test sscd SAM_apid)


else()
    target_link_libraries(SAM_api_test ssc SAM_api)

endif()

#setup compile options
target_compile_options(SAM_api_test PUBLIC -O3 -std=c++11 )


# setup linking dependencies and flags
target_link_libraries(SAM_api_test -lm -lcurl)
