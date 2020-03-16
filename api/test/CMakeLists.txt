set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 2.8)


if(APPLE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.7" CACHE STRING "Minimum OS X deployment version")
elseif(MSVC)
    set(CMAKE_SYSTEM_VERSION 8.1 CACHE TYPE INTERNAL FORCE)
endif()


Project(SAM_API_TEST)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")


include_directories(. .. $ENV{SSCDIR} $ENV{SSCDIR}/ssc)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

add_executable(SAM_api_test
		SAM_api.h
        test.cpp
        GenericSystem-External.h
        GenericSystem-External.cpp
        SystemLoader.h
        SystemLoader.cpp
        )

set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
add_compile_definitions(_DEBUG)

# setup linking dependencies and flags

# if running as combined build, make sure to link to projects
if (${CMAKE_PROJECT_NAME} STREQUAL system_advisor_model)
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        target_link_libraries(SAM_api_test sscd SAM_apid)
    else()
        target_link_libraries(SAM_api_test ssc SAM_apid)
    endif()
else()
    unset(SSC_LIB CACHE)
	unset(SAM_API_LIB CACHE)
    find_library( SSC_LIB
            NAMES sscd.dylib sscd.so ssc.lib
            PATHS ${CMAKE_CURRENT_SOURCE_DIR})
	find_library( SAM_API_LIB
            NAMES libSAM_apid.dylib libSAM_apid.so SAM_api.lib
            PATHS ${CMAKE_CURRENT_SOURCE_DIR})
    target_link_libraries(SAM_api_test ${SSC_LIB} ${SAM_API_LIB})

endif()




#setup compile options
if(MSVC)
    add_compile_definitions( __64BIT__ __WINDOWS__
        _WINDOWS _CRT_NON_CONFORMING_SWPRINTFS=1
        _SCL_SECURE_NO_WARNINGS=1 _UNICODE NOPCH)
    set_target_properties(SAM_api_test PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY $<1:${CMAKE_CURRENT_SOURCE_DIR}>)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SUBSYSTEM:CONSOLE")
    set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} 
        PROPERTY VS_STARTUP_PROJECT SAM_api_test)
else()
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -std=c++11" )

    target_link_libraries(SAM_api_test -lm -lcurl)
endif()


# copy needed dlls to sam/api/test and PySAM
# windows applications prefer the dlls in same path
if (MSVC)
    add_custom_command(
            TARGET SAM_api_test
            PRE_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
            $ENV{SSCDIR}/build_vs2017/x64/Debug/sscd.dll ${CMAKE_CURRENT_SOURCE_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
            $ENV{SAMNTDIR}/api/SAM_apid.dll ${CMAKE_CURRENT_SOURCE_DIR}
    )
else()
    add_custom_command(
            TARGET SAM_api_test
            PRE_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
            $ENV{SSCDIR}/ssc/libsscd.so ${CMAKE_CURRENT_SOURCE_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
            $ENV{SAMNTDIR}/api/libSAM_apid.so ${CMAKE_CURRENT_SOURCE_DIR}
    )
endif()