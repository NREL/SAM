#####################################################################################################################
#
# CMake and System Settings
#
#####################################################################################################################

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


#####################################################################################################################
#
# Project Settings 
#
#####################################################################################################################

Project(EXPORT_SAM_UI)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

include_directories(
        . ../../src $ENV{LKDIR}/include $ENV{WEXDIR}/include $ENV{SSCDIR} ../tcs)

set(SAM_SOURCES
        ../../src/variables.cpp
        ../../src/equations.cpp)

file(TO_CMAKE_PATH $ENV{SSCDIR}/shared/lib_util.cpp SSC_SOURCES)


#####################################################################################################################
#
# Compile Options per Platform
#
#####################################################################################################################

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if(MSVC)
    add_compile_options(/MP /W3)
    if (CMAKE_BUILD_TYPE STREQUAL Debug)
        add_compile_options(/MDd)
        add_compile_definitions(_DEBUG)
    else()
        add_compile_options(/MD /O3)
        add_compile_definitions(NDEBUG )
    endif()
    add_compile_definitions( _CRT_SECURE_NO_DEPRECATE=1 _CRT_NON_CONFORMING_SWPRINTFS=1
            _SCL_SECURE_NO_WARNINGS=1 __WXMSW__ _UNICODE NOPCH)
else()
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
# Packages
#
#####################################################################################################################

if (UNIX)
    set(wxWidgets_CONFIG_EXECUTABLE /usr/local/bin/wx-config-3)
    find_package(wxWidgets COMPONENTS xrc stc richtext ribbon propgrid aui gl html qa adv core xml net base REQUIRED)

else()
    set(wxWidgets_CONFIGURATION mswu)
    set(wxWidgets_ROOT_DIR $ENV{WXMSW3})
    find_package(wxWidgets COMPONENTS qa webview aui richtext html propgrid adv net stc core base scintilla REQUIRED)
endif()

include(${wxWidgets_USE_FILE})


#####################################################################################################################
# 
# CMake Targets
#
#####################################################################################################################

add_executable(export_config
        ${SAM_SOURCES}
		${SSC_SOURCES}
        export_config.cpp export_config.h
        startup_extractor.cpp startup_extractor.h
        ui_form_extractor.cpp ui_form_extractor.h
        equation_extractor.cpp equation_extractor.h
        lk_env.h
        data_structures.cpp data_structures.h
        config_extractor.cpp config_extractor.h
        callback_extractor.cpp callback_extractor.h
        variable_graph.cpp variable_graph.h
        test.cpp test.h
        builder_generator.h builder_generator.cpp
        lk_eval.cpp lk_eval.h
        builder_generator_helper.h builder_generator_helper.cpp 
		builder_C_API.cpp builder_C_API.h 
		builder_PySAM.cpp builder_PySAM.h)


target_compile_definitions(export_config PUBLIC LK_USE_WXWIDGETS )

set_property(TARGET export_config PROPERTY CXX_STANDARD 11)

if(MSVC)
    set_target_properties(export_config PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY $<1:${CMAKE_CURRENT_SOURCE_DIR}>
        LINK_FLAGS /SUBSYSTEM:CONSOLE)
endif()


#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

if (${CMAKE_PROJECT_NAME} STREQUAL system_advisor_model)
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        target_link_libraries(export_config lkd wexd sscd)
    else()
        target_link_libraries(export_config lk wex ssc)
    endif()
else()
    unset(LK_LIB CACHE)
    unset(SSC_LIB CACHE)
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        find_library( LK_LIB
                NAMES lkd.a lkd.lib lkd.dylib
                PATHS $ENV{LKDIR})
        find_library( WEX_LIB
                NAMES wexd.a wexd.lib wexd.dylib
                PATHS $ENV{WEXDIR})
        find_library( SSC_LIB
                NAMES sscd.dylib sscd.so sscd.lib
                PATHS $ENV{SSCDIR}/ssc)
    else()
        find_library( LK_LIB
                NAMES lk.a lk.lib lk.dylib
                PATHS $ENV{LKDIR})
        find_library( WEX_LIB
                NAMES wex.a wex.lib wex.dylib
                PATHS $ENV{WEXDIR})
        find_library(SSC_LIB
                NAMES ssc.dylib ssc.so ssc.lib
                PATHS $ENV{SSCDIR}/ssc)
    endif()
    target_link_libraries(export_config ${SSC_LIB} ${WEX_LIB} ${LK_LIB})
endif()

if (UNIX)
    target_link_libraries(export_config -lm)
endif()

target_link_libraries(export_config ${wxWidgets_LIBRARIES})

# need to tell executable where to find libssc.so
if(APPLE)
    set_target_properties(export_config PROPERTIES LINK_FLAGS  "-Wl,-rpath -Wl,$ENV{SSCDIR}/ssc/")
endif()


#####################################################################################################################
#
# Target Installation
#
#####################################################################################################################

if(system_advisor_model_EXPORT)
	file(TO_NATIVE_PATH $ENV{SAMNTDIR}/api API_DEST)
    if (MSVC)
        if(CMAKE_BUILD_TYPE STREQUAL "Debug")
            set(SSC_NAME sscd)
        else()
            set(SSC_NAME ssc)
        endif()
        file(TO_NATIVE_PATH $ENV{SSCDIR}/ssc/${SSC_NAME}.dll SSC_LIB_PATH)
        add_custom_command(
                TARGET export_config
                POST_BUILD
                COMMAND ${CMAKE_COMMAND}
                    ARGS -E copy ${SSC_LIB} ${SSC_LIB_PATH} ${CMAKE_CURRENT_SOURCE_DIR}
        )
    endif()
    add_custom_command(
        TARGET export_config
        POST_BUILD
        COMMAND "$<TARGET_FILE:export_config>" --api ${API_DEST} --pysam $ENV{PYSAMDIR}
    )
endif()