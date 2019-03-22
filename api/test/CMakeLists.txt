set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 3.12)


if(APPLE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.13" CACHE STRING "Minimum OS X deployment version")
endif()

Project(SAM_API_TEST)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")


include_directories(.. $ENV{SSCDIR} $ENV{SSCDIR}/ssc)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

add_executable(SAM_api_test
        test.cpp
        GenericSystem-External.h
        GenericSystem-External.cpp
        )

set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")


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
	
    find_library( SSC_LIB
            NAMES sscd.dylib sscd.so sscd.lib
            PATHS $ENV{SSCDIR}/ssc $ENV{SSCDIR}/build_vs2017/x64/Debug)
    target_link_libraries(SAM_api_test ${SSC_LIB} SAM_apid)

endif()




#setup compile options
if(MSVC)
    add_compile_definitions( __64BIT__ __WINDOWS__
        _WINDOWS _CRT_NON_CONFORMING_SWPRINTFS=1
        _SCL_SECURE_NO_WARNINGS=1 _UNICODE NOPCH)
    set_target_properties(${SAM_API_LIB} PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY $<1:${CMAKE_CURRENT_SOURCE_DIR}>)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SUBSYSTEM:CONSOLE")
    set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} 
        PROPERTY VS_STARTUP_PROJECT ${SAM_API_LIB})
else()
    target_compile_options(SAM_api_test PUBLIC -O3 -std=c++11 )

    target_link_libraries(SAM_api_test -lm -lcurl)
endif()

# copy needed dlls
if(MSVC)
add_custom_command(
	TARGET SAM_api_test
		POST_BUILD 
		COMMAND ${CMAKE_COMMAND} -E copy_if_different
		$ENV{SSCDIR}/build_vs2017/x64/Debug/sscd.dll ${CMAKE_CURRENT_SOURCE_DIR}
		COMMAND ${CMAKE_COMMAND} -E copy_if_different
		$ENV{SAMNTDIR}/api/api.dll ${CMAKE_CURRENT_SOURCE_DIR}
	)
endif()