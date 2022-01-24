#####################################################################################################################
#
# CMake and System Settings
#
#####################################################################################################################

set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 3.19)

# For CMake build option `SAMAPI_EXPORT` and `SAM_SKIP_AUTOGEN`, see ../CMakeLists.txt

if(APPLE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.15" CACHE STRING "Minimum OS X deployment version")
    set(CMAKE_MACOSX_RPATH 1)
endif()

if (UNIX AND NOT CMAKE_C_COMPILER)
    set(CMAKE_C_COMPILER gcc)
    set(CMAKE_CXX_COMPILER g++)
endif()


#####################################################################################################################
#
# Project Settings
#
#####################################################################################################################

Project(SAM_API)

if (NOT SAM_SKIP_AUTOGEN)
    add_subdirectory(api_autogen)
endif()

set(SAM_API_SRC
        src/ErrorHandler.cpp
        src/SAM_api.cpp
        src/visibility.h
        src/SAM_AdjustmentFactors.cpp
        src/SAM_eqns.cpp
        )
file(GLOB SAM_API_MODULES ${PROJECT_SOURCE_DIR}/modules/*.cpp ${PROJECT_SOURCE_DIR}/include/*.h)


#####################################################################################################################
#
# Compile Options per Platform
#
#####################################################################################################################

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if(MSVC)
    add_compile_options(/MP /W3 /wd4996 /bigobj)
    add_compile_definitions( __DLL__ __64BIT__ __WINDOWS__ _WINDOWS _CRT_NON_CONFORMING_SWPRINTFS=1
        _SCL_SECURE_NO_WARNINGS=1 _UNICODE NOPCH)
    foreach (flag_var CMAKE_C_FLAGS_DEBUG CMAKE_CXX_FLAGS_DEBUG)
        set(${flag_var} "${${flag_var}} /D_DEBUG" CACHE STRING "compile flags" FORCE)
    endforeach()
else()
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -Wno-attributes" )
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        add_compile_definitions(_DEBUG)
    else()
        SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -DNDEBUG" )
    endif()
endif()

if(APPLE)
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -mmacosx-version-min=10.9" )
endif()



#####################################################################################################################
#
# CMake Targets
#
#####################################################################################################################

add_library(SAM_api SHARED ${SAM_API_SRC} ${SAM_API_MODULES})

target_include_directories(SAM_api PRIVATE include src $ENV{SSCDIR} $ENV{SSCDIR}/ssc $ENV{SSCDIR}/shared)

if (NOT SAM_SKIP_AUTOGEN)
    add_dependencies(SAM_api export_config)
endif()

# required functions not exported in ssc.dll
if(MSVC)
    target_sources(SAM_api PRIVATE $ENV{SSCDIR}/ssc/vartab.cpp $ENV{SSCDIR}/shared/lib_util.cpp)
endif()

# libSAM_api for PySAM
if(APPLE)
    set(CMAKE_SHARED_LIBRARY_SUFFIX ".so")
endif()


#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

if (${CMAKE_PROJECT_NAME} STREQUAL system_advisor_model)
    target_link_libraries(SAM_api ssc)
else()
    unset(SSC_LIB CACHE)
    find_library( SSC_LIB
            NAMES ssc.lib libssc.so
            PATHS $ENV{SSC_LIB} $ENV{SSCDIR}/build/ssc $ENV{SSCDIR}/build/ssc/Release $ENV{SSCDIR}/../build_linux_ssc/ssc)
    target_link_libraries(SAM_api optimized ${SSC_LIB})

    if (CMAKE_BUILD_TYPE STREQUAL "Debug" OR MSVC)
        unset(SSCD_LIB CACHE)
        find_library( SSCD_LIB
                NAMES sscd.lib libsscd.so
                PATHS $ENV{SSCD_LIB} $ENV{SSCDIR}/build/ssc $ENV{SSCDIR}/build/ssc/Debug)
        target_link_libraries(SAM_api debug ${SSCD_LIB})
    endif()
endif()

if (UNIX)
    target_link_libraries(SAM_api -lm)
endif()


#####################################################################################################################
#
# Target Installation
#
#####################################################################################################################

if (APPLE)
    add_custom_command(
            TARGET SAM_api
            POST_BUILD
            COMMAND install_name_tool -id @rpath/$<TARGET_FILE_NAME:SAM_api> $<TARGET_FILE:SAM_api>
    )
endif()

if(DEFINED ENV{PYSAMDIR})
    if (SAMAPI_EXPORT OR MSVC)
		if (MSVC)
			add_custom_command(
                TARGET SAM_api
                POST_BUILD
				COMMAND ${CMAKE_COMMAND} -E remove
					$ENV{PYSAMDIR}/files/$<TARGET_FILE_BASE_NAME:SAM_api>.lib
					$ENV{PYSAMDIR}/files/$<TARGET_FILE_BASE_NAME:SAM_api>.dll
			)
		    set(SAMAPI_LIB $<TARGET_FILE_DIR:SAM_api>/$<TARGET_FILE_BASE_NAME:SAM_api>.lib)
        endif()
        add_custom_command(
                TARGET SAM_api
                POST_BUILD
                COMMAND ${CMAKE_COMMAND}
                ARGS -E copy $<TARGET_FILE:SAM_api> ${SAMAPI_LIB} $ENV{PYSAMDIR}/files
        )
    endif()
endif()
