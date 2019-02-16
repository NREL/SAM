set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 3.12)


if(APPLE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")
endif()

Project(sam_api)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

# setup wxWidgets
if (UNIX)
    set(wxWidgets_CONFIG_EXECUTABLE /usr/local/bin/wx-config-3)
    find_package(wxWidgets COMPONENTS xrc stc richtext ribbon propgrid aui gl html qa adv core xml net base REQUIRED)
else()
    set(wxWidgets_CONFIGURATION mswu)
    set(wxWidgets_ROOT_DIR "C:\\System Advisor Model Code\\wxWidgets-3.1.1")
    find_package(wxWidgets COMPONENTS qa webview aui richtext html propgrid adv net stc core base scintilla REQUIRED)
endif()

include(${wxWidgets_USE_FILE})

include_directories(../src $ENV{LKDIR}/include $ENV{SSCDIR})

add_executable(export_config
        export_config.h
        export_config.cpp)

set(SAM_API_SRC
        sam_api.cpp
        ../src/invoke.cpp
        ../src/variables.cpp
        ../src/equations.cpp)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# make lk without wxWidgets


# set build types for archive
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    add_library(SAM_apid
            ${SAM_API_SRC})
    set(SAM_API_LIB SAM_apid)

    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
else()
    add_library(SAM_api
            ${SAM_API_SRC})
    set(SAM_API_LIB SAM_apid)
endif()

target_include_directories(${SAM_API_LIB} PUBLIC
        . ../src $ENV{LKDIR}/include $ENV{SSCDIR} ../tcs)

# setup linking dependencies and flags

# if running as combined build, make sure to link to projects
if (${CMAKE_PROJECT_NAME} STREQUAL system_advisor_model)
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        target_link_libraries(${SAM_API_LIB} lkd sscd)
        set(SSC_LIB $ENV{SSCDIR}/ssc/sscd.dylib)
    else()
        target_link_libraries(${SAM_API_LIB} lk ssc)
        set(SSC_LIB $ENV{SSCDIR}/ssc/ssc.dylib)
    endif()
    target_link_libraries(export_config lkd sscd)
else()
    unset(LK_LIB CACHE)
    unset(SSC_LIB CACHE)
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        find_library( LK_LIB
                NAMES lkd.a lkd.lib
                PATHS $ENV{LKDIR})
        find_library( SSC_LIB
                NAMES sscd.dylib sscd.so sscd.lib
                PATHS $ENV{SSCDIR}/ssc)
    else()
        find_library( LK_LIB
                NAMES lk.a lk.lib
                PATHS $ENV{LKDIR})
        find_library(SSC_LIB
                NAMES  ssc.dylib ssc.so ssc.lib
                PATHS $ENV{SSCDIR}/ssc)
    endif()
    target_link_libraries(${SAM_API_LIB} ${SSC_LIB} ${LK_LIB} )
    target_link_libraries(${export_config} ${SSC_LIB} ${LK_LIB} ${wxWidgets_LIBRARIES})

endif()

# setup linking dependencies and flags
target_link_libraries(${SAM_API_LIB} -lm -lcurl)
target_link_libraries(export_config -lm)

