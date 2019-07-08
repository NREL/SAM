#####################################################################################################################
#
# CMake and System Settings
#
#####################################################################################################################

set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 2.8)

if(APPLE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")
    set(CMAKE_MACOSX_RPATH 1)
elseif(MSVC)
    set(CMAKE_SYSTEM_VERSION 8.1 CACHE TYPE INTERNAL FORCE)
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

if (NOT skip_autogen)
    add_subdirectory(api_autogen)
endif()

if (MSVC)
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY $<1:${CMAKE_CURRENT_SOURCE_DIR}>)
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY $<1:${CMAKE_CURRENT_SOURCE_DIR}>)
    set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY $<1:${CMAKE_CURRENT_SOURCE_DIR}>)
else()
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endif()

include_directories(. include $ENV{SSCDIR} $ENV{SSCDIR}/ssc $ENV{SSCDIR}/shared)

set(SAM_API_SRC
        src/ErrorHandler.cpp
        src/SAM_api.cpp
        include/visibility.h
        src/SAM_AdjustmentFactors.cpp
        src/SAM_Battery.cpp
        src/SAM_Battwatts.cpp
        src/SAM_Belpe.cpp
        src/SAM_Biomass.cpp
        src/SAM_Cashloan.cpp
        src/SAM_Equpartflip.cpp
        src/SAM_Fuelcell.cpp
        src/SAM_GenericSystem.cpp
        src/SAM_Geothermal.cpp
		src/SAM_Grid.cpp
        src/SAM_Hcpv.cpp
        src/SAM_HostDeveloper.cpp
        src/SAM_IphToLcoefcr.cpp
        src/SAM_Lcoefcr.cpp
        src/SAM_Levpartflip.cpp
        src/SAM_LinearFresnelDsgIph.cpp
        src/SAM_Pvsamv1.cpp
        src/SAM_Pvwattsv5.cpp
        src/SAM_Pvwattsv5Lifetime.cpp
        src/SAM_Saleleaseback.cpp
        src/SAM_Singleowner.cpp
        src/SAM_Swh.cpp
        src/SAM_TcsdirectSteam.cpp
        src/SAM_Tcsdish.cpp
        src/SAM_TcsgenericSolar.cpp
        src/SAM_Tcsiscc.cpp
        src/SAM_TcslinearFresnel.cpp
        src/SAM_TcsmoltenSalt.cpp
        src/SAM_TcsMSLF.cpp
        src/SAM_TcstroughEmpirical.cpp
        src/SAM_TcstroughPhysical.cpp
        src/SAM_Thermalrate.cpp
        src/SAM_Thirdpartyownership.cpp
        src/SAM_TroughPhysicalProcessHeat.cpp
        src/SAM_Utilityrate5.cpp
        src/SAM_Windpower.cpp
        src/SAM_eqns.cpp)
		

#####################################################################################################################
#
# Compile Options per Platform
#
#####################################################################################################################

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if(MSVC)
    add_compile_options(/MP /W3 /wd4996)
    foreach (flag_var CMAKE_C_FLAGS_DEBUG CMAKE_CXX_FLAGS_DEBUG)
        set(${flag_var} "${${flag_var}} /D_DEBUG" CACHE STRING "compile flags" FORCE)
    endforeach()
	if (system_advisor_model_EXPORT)
        add_compile_definitions(system_advisor_model_EXPORT)
	endif()
    add_compile_definitions( __DLL__ __64BIT__ __WINDOWS__ _WINDOWS _CRT_NON_CONFORMING_SWPRINTFS=1
        _SCL_SECURE_NO_WARNINGS=1 _UNICODE NOPCH)
else()
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -Wno-attributes" )
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
        set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
        add_compile_definitions(_DEBUG)
    else()
        SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3" )
    endif()
endif()



#####################################################################################################################
# 
# CMake Targets
#
#####################################################################################################################

add_library(SAM_API SHARED ${SAM_API_SRC})
set_target_properties(SAM_API
    PROPERTIES
    DEBUG_POSTFIX "d"
    RUNTIME_OUTPUT_DIRECTORY $<1:${CMAKE_CURRENT_SOURCE_DIR}>)
if (MSVC)
    set_target_properties(SAM_API
        PROPERTIES
        LINK_FLAGS /SUBSYSTEM:WINDOWS)
endif()

if (NOT skip_autogen)
    add_dependencies(SAM_API export_config)
endif()

# required functions not exported in ssc.dll
if(MSVC)
    target_sources(SAM_API PRIVATE $ENV{SSCDIR}/ssc/vartab.cpp $ENV{SSCDIR}/shared/lib_util.cpp)
endif() 


#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################


unset(SSCD_LIB CACHE)
unset(SSC_LIB CACHE)
if (MSVC)
    SET(CMAKE_FIND_LIBRARY_PREFIXES "")
    SET(CMAKE_FIND_LIBRARY_SUFFIXES ".dll")
endif()

string( CONCAT TMP_PATH $ENV{SSCDIR} "/ssc" )
message(STATUS "SAM_API: TMP_PATH=${TMP_PATH}")

find_library( SSCD_LIB
    NAMES sscd
    PATHS ${TMP_PATH} )
find_library( SSC_LIB
    NAMES ssc
    PATHS ${TMP_PATH}  )
    message(STATUS "SAM_API RELEASE: SSC_LIB=${SSC_LIB}")
    message(STATUS "SAM_API DEBUG: SSCD_LIB=${SSCD_LIB}")

target_link_libraries(SAM_API debug ${SSCD_LIB})
target_link_libraries(SAM_API optimized ${SSC_LIB})

if (UNIX)
    target_link_libraries(${SAM_API_LIB} -lm)
endif()


#####################################################################################################################
#
# Target Installation
#
#####################################################################################################################

if(APPLE)
    # PySAM setup.py uses .so
    set(CMAKE_SHARED_LIBRARY_SUFFIX ".so")
    add_custom_command(
        TARGET ${SAM_API_LIB}
        POST_BUILD
        COMMAND install_name_tool -id @rpath/libSAM_api.so $ENV{SAMNTDIR}/api/libSAM_api.so
    )
elseif(UNIX )
    set(CMAKE_SHARED_LIBRARY_PREFIX "lib")
endif()


