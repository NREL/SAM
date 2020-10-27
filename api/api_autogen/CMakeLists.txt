#####################################################################################################################
#
# CMake and System Settings
#
#####################################################################################################################

set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 2.8)

if(APPLE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")
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

set(SAM_SOURCES
        ../../src/variables.cpp
        ../../src/equations.cpp)

file(TO_CMAKE_PATH ../../../ssc/shared/lib_util.cpp SSC_SOURCES)


#####################################################################################################################
#
# Compile Options per Platform
#
#####################################################################################################################

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if(MSVC)
    add_compile_options(/MP /W3)
    add_compile_definitions( _CRT_SECURE_NO_DEPRECATE=1 _CRT_NON_CONFORMING_SWPRINTFS=1
            _SCL_SECURE_NO_WARNINGS=1 __WXMSW__ _UNICODE NOPCH)
    foreach (flag_var CMAKE_C_FLAGS_DEBUG CMAKE_CXX_FLAGS_DEBUG)
        set(${flag_var} "${${flag_var}} /D_DEBUG" CACHE STRING "compile flags" FORCE)
    endforeach()
else()
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        add_compile_definitions(_DEBUG)
    else()
        SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -DNDEBUG" )
    endif()
endif()

add_compile_definitions( LK_USE_WXWIDGETS )



#####################################################################################################################
#
# Packages
#
#####################################################################################################################

if (UNIX)
    set(wxWidgets_CONFIG_EXECUTABLE /usr/local/bin/wx-config-3)
    find_package(wxWidgets COMPONENTS xrc stc richtext ribbon propgrid aui gl html qa adv core xml net base REQUIRED)
else()
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
		builder_PySAM.cpp builder_PySAM.h
        library_extractor.cpp library_extractor.h)

target_include_directories(export_config SYSTEM PRIVATE ../../src $ENV{SSCDIR} $ENV{SSCDIR}/tcs $ENV{WEXDIR}/include $ENV{LKDIR}/include )

set_target_properties(export_config
    PROPERTIES
    DEBUG_POSTFIX "d"
    PREFIX ""
    )
if (MSVC)
    set_target_properties(export_config
        PROPERTIES
        LINK_FLAGS /SUBSYSTEM:CONSOLE)
endif()



#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

if (${CMAKE_PROJECT_NAME} STREQUAL system_advisor_model)
    target_link_libraries(export_config lk wex ssc)
else()
    unset(WEX_LIB CACHE)
    unset(LK_LIB CACHE)
    unset(SSC_LIB CACHE)
    find_library( WEX_LIB
            NAMES wex.a wex.lib
            PATHS $ENV{WEX_LIB} $ENV{WEXDIR}/build $ENV{WEXDIR}/build/Release)
    find_library( LK_LIB
            NAMES lk.a lk.lib
            PATHS $ENV{LK_LIB} $ENV{LKDIR}/build $ENV{LKDIR}/build/Release)
    find_library( SSC_LIB
            NAMES ssc.dylib ssc.lib ssc.so
            PATHS $ENV{SSC_LIB} $ENV{SSC_LIB} $ENV{SSCDIR}/build/ssc $ENV{SSCDIR}/build/ssc/Release)
    target_link_libraries(export_config optimized ${WEX_LIB} optimized ${SSC_LIB} optimized ${LK_LIB})

    if (CMAKE_BUILD_TYPE STREQUAL "Debug" OR MSVC)
        unset(LKD_LIB CACHE)
        unset(WEXD_LIB CACHE)
        unset(SSCD_LIB CACHE)
        find_library( WEXD_LIB
                NAMES wexd.a wexd.lib
                PATHS $ENV{WEXD_LIB} $ENV{WEXDIR}/build $ENV{WEXDIR}/build/Debug)
        find_library( LKD_LIB
                NAMES lkd.a lkd.lib
                PATHS $ENV{LKD_LIB} $ENV{LKDIR}/build $ENV{LKDIR}/build/Debug)
        find_library( SSCD_LIB
                NAMES sscd.dylib sscd.lib sscd.so
                PATHS $ENV{SSCD_LIB} $ENV{SSCDIR}/build/ssc $ENV{SSCDIR}/build/ssc/Debug)
        target_link_libraries(export_config debug ${SSCD_LIB} debug ${WEXD_LIB} debug ${LKD_LIB})
    endif()
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

if(DEFINED ENV{PYSAMDIR} )
    set(PYSAM $ENV{PYSAMDIR})
else()
    set(PYSAM ${CMAKE_CURRENT_SOURCE_DIR}/library/PySAM )
endif()


file(TO_NATIVE_PATH $ENV{SAMNTDIR}/api API_DEST)
set(EXPORT_RUN "$<TARGET_FILE:export_config> --api ${API_DEST} --pysam ${PYSAM}")

if (MSVC)
	add_custom_command(
		TARGET export_config
		POST_BUILD
		COMMAND cmd /C $<$<CONFIG:Release>:${EXPORT_RUN}>
	)
else()
	if(NOT CMAKE_BUILD_TYPE STREQUAL "Debug")
		add_custom_command(
			TARGET export_config
			POST_BUILD
			VERBATIM
			COMMAND sh -c ${EXPORT_RUN}
		)
	endif()
endif()
