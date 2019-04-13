set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 2.8)

if(APPLE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")
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
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

include_directories(. include $ENV{SSCDIR} $ENV{SSCDIR}/ssc $ENV{SSCDIR}/shared)

set(SAM_API_SRC
        ErrorHandler.cpp
		SAM_api.cpp
        include/visibility.h
        src/SAM_AdjustmentFactors.cpp
        src/SAM_Battwatts.cpp
        src/SAM_Fuelcell.cpp
        src/SAM_IphToLcoefcr.cpp
        src/SAM_Pvwattsv5.cpp
        src/SAM_Swh.cpp
        src/SAM_Tcsiscc.cpp
        src/SAM_Belpe.cpp
        src/SAM_GenericSystem.cpp
        src/SAM_Lcoefcr.cpp
        src/SAM_Pvwattsv5Lifetime.cpp
        src/SAM_TcsMSLF.cpp
        src/SAM_TcslinearFresnel.cpp
        src/SAM_Thirdpartyownership.cpp
        src/SAM_Biomass.cpp
        src/SAM_Geothermal.cpp
        src/SAM_Levpartflip.cpp
        src/SAM_Saleleaseback.cpp
        src/SAM_TcsdirectSteam.cpp
        src/SAM_TcsmoltenSalt.cpp
        src/SAM_TroughPhysicalProcessHeat.cpp
        src/SAM_Cashloan.cpp
        src/SAM_Hcpv.cpp
        src/SAM_LinearFresnelDsgIph.cpp
        src/SAM_Singleowner.cpp
        src/SAM_Tcsdish.cpp
        src/SAM_TcstroughEmpirical.cpp
        src/SAM_Utilityrate5.cpp
        src/SAM_Equpartflip.cpp
        src/SAM_HostDeveloper.cpp
        src/SAM_Pvsamv1.cpp
        src/SAM_Battery.cpp
        src/SAM_TcsgenericSolar.cpp
        src/SAM_TcstroughPhysical.cpp
        src/SAM_Windpower.cpp
        )
		

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


# setup linking dependencies and flags
if(MSVC)
target_sources(${SAM_API_LIB} PRIVATE $ENV{SSCDIR}/ssc/vartab.cpp $ENV{SSCDIR}/shared/lib_util.cpp)
endif() 
# if running as combined build, make sure to link to projects
if (${CMAKE_PROJECT_NAME} STREQUAL system_advisor_model)
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        target_link_libraries(${SAM_API_LIB} sscd)
        set(SSC_LIB sscd)
    else()
        target_link_libraries(${SAM_API_LIB} ssc)
        set(SSC_LIB ssc)
    endif()
else()
    unset(SSC_LIB CACHE)
	
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        find_library( SSC_LIB
                NAMES sscd.so sscd.lib
                PATHS $ENV{SSCDIR}/ssc $ENV{SSCDIR}/build_vs2017/x64/Debug)
    elseif(system_advisor_model_EXPORT)
        find_library( SSC_LIB
                NAMES libssc.so ssc.lib ssc.so
                PATHS $ENV{SSCDIR}/ssc )
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

target_link_libraries(${SAM_API_LIB} ${SSC_LIB})


# PySAM setup.py
add_definitions(-Dsystem_advisor_model_EXPORT)
if(APPLE)
    set(CMAKE_SHARED_LIBRARY_SUFFIX ".so")
    add_custom_command(
        TARGET ${SAM_API_LIB}
        POST_BUILD
        COMMAND install_name_tool -id @rpath/libSAM_api.so $ENV{SAMNTDIR}/api/libSAM_api.so
    )
elseif(UNIX )
    set(CMAKE_SHARED_LIBRARY_PREFIX "lib")
endif()


#export libraries
if (system_advisor_model_EXPORT)
    add_custom_command(
        TARGET ${SAM_API_LIB}
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E make_directory $ENV{PYSAMDIR}/data/
        COMMAND ${CMAKE_COMMAND} -E make_directory $ENV{PYSAMDIR}/include/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            $ENV{SAMNTDIR}/api/include/SAM_api.h $ENV{PYSAMDIR}/include
    )
    if (MSVC)
        add_custom_command(
                TARGET ${SAM_API_LIB}
                POST_BUILD
                COMMAND ${CMAKE_COMMAND} -E copy_if_different
                    $ENV{SSCDIR}/build_vs2017/x64/Release/ssc.lib $ENV{PYSAMDIR}/data/
				COMMAND ${CMAKE_COMMAND} -E copy_if_different
                    $ENV{SSCDIR}/build_vs2017/x64/Release/ssc.dll $ENV{PYSAMDIR}/data/
				COMMAND ${CMAKE_COMMAND} -E copy_if_different
                    $ENV{SAMNTDIR}/api/SAM_api.lib $ENV{PYSAMDIR}/data/
                COMMAND ${CMAKE_COMMAND} -E copy_if_different
                    $ENV{SAMNTDIR}/api/SAM_api.dll $ENV{PYSAMDIR}/data/
        )
    elseif(UNIX)
        add_custom_command(
                TARGET ${SAM_API_LIB}
                POST_BUILD
                COMMAND ${CMAKE_COMMAND} -E copy_if_different
                    $ENV{SSCDIR}/ssc/libssc.so $ENV{PYSAMDIR}/data/
                COMMAND ${CMAKE_COMMAND} -E copy_if_different
                    $ENV{SAMNTDIR}/api/libSAM_api.so $ENV{PYSAMDIR}/data/
        )
    endif()
endif()

#startup project for visual studios
if (MSVC)
	set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY VS_STARTUP_PROJECT ${SAM_API_LIB})
endif()