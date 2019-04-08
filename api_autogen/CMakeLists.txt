set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 3.12)


if(APPLE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")
endif()

Project(EXPORT_SAM_UI)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

# setup wxWidgets
if (UNIX)
    set(wxWidgets_CONFIG_EXECUTABLE /usr/local/bin/wx-config-3)
    find_package(wxWidgets COMPONENTS xrc stc richtext ribbon propgrid aui gl html qa adv core xml net base REQUIRED)
else()
    add_compile_options(/W3 /wd4996)
    if (CMAKE_BUILD_TYPE STREQUAL Debug)
        add_compile_options(/MDd)
        add_compile_definitions(_DEBUG)
    else()
        add_compile_options(/MD /O2)
        add_compile_definitions(NDEBUG )
    endif()
    add_compile_definitions( WIN32 _CRT_SECURE_NO_DEPRECATE=1 _CRT_NON_CONFORMING_SWPRINTFS=1
            _SCL_SECURE_NO_WARNINGS=1 __WXMSW__ _UNICODE NOPCH)

    set(wxWidgets_CONFIGURATION mswu)
    set(wxWidgets_ROOT_DIR $ENV{WXMSW3})
    find_package(wxWidgets COMPONENTS qa webview aui richtext html propgrid adv net stc core base scintilla REQUIRED)
endif()

include(${wxWidgets_USE_FILE})

include_directories(../src $ENV{LKDIR}/include $ENV{SSCDIR})

set(SAM_SOURCES
        ../src/variables.cpp
        ../src/equations.cpp)

add_executable(export_config
        ${SAM_SOURCES}
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
        builder_generator_helper.h builder_generator_helper.cpp builder_C_API.cpp builder_C_API.h builder_PySAM.cpp builder_PySAM.h)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)


include_directories(
        . ../src $ENV{LKDIR}/include $ENV{WEXDIR}/include $ENV{SSCDIR} ../tcs)

# setup linking dependencies and flags

# if running as combined build, make sure to link to projects
if (${CMAKE_PROJECT_NAME} STREQUAL system_advisor_model)
    target_link_libraries(export_config lkd wexd sscd)
else()
    unset(LK_LIB CACHE)
    unset(SSC_LIB CACHE)
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        find_library( LK_LIB
                NAMES lkd.a lkd.lib
                PATHS $ENV{LKDIR})
        find_library( WEX_LIB
                NAMES wexd.a wexd.lib
                PATHS $ENV{WEXDIR})
        find_library( SSC_LIB
                NAMES sscd.dylib sscd.so sscd.lib
                PATHS $ENV{SSCDIR}/ssc)
    else()
        find_library( LK_LIB
                NAMES lk.a lk.lib
                PATHS $ENV{LKDIR})
        find_library( WEX_LIB
                NAMES wex.a wex.lib
                PATHS $ENV{WEXDIR})
        find_library(SSC_LIB
                NAMES  ssc.dylib ssc.so ssc.lib
                PATHS $ENV{SSCDIR}/ssc)
    endif()
    target_link_libraries(export_config ${SSC_LIB} ${WEX_LIB} ${LK_LIB} ${wxWidgets_LIBRARIES})

endif()

#setup compile options
target_compile_definitions(export_config PUBLIC LK_USE_WXWIDGETS )

set_property(TARGET export_config PROPERTY CXX_STANDARD 11)


# setup linking dependencies and flags
target_link_libraries(export_config -lm)

# windows
if(MSVC)
    set_target_properties(export_config PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY $<1:${CMAKE_CURRENT_SOURCE_DIR}>)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SUBSYSTEM:WINDOWS")
#    set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY VS_STARTUP_PROJECT export_config)
endif()

if (system_advisor_model_EXPORT)
    file(GLOB MY_PUBLIC_HEADERS
           "${CMAKE_CURRENT_SOURCE_DIR}/library/C/*.h"
            )

    file(GLOB MY_SRC
            "${CMAKE_CURRENT_SOURCE_DIR}/library/C/*.cpp"
            )

    file(GLOB PYSAM_SRC
            "${CMAKE_CURRENT_SOURCE_DIR}/library/PySAM/*.c"
            )

    add_custom_command(
            TARGET export_config
            POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E make_directory $ENV{SAMNTDIR}/api/include/
            COMMAND ${CMAKE_COMMAND} -E make_directory $ENV{SAMNTDIR}/api/src/
            COMMAND ${CMAKE_COMMAND} -E make_directory $ENV{PYSAMDIR}/src/
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
                ${MY_PUBLIC_HEADERS} $ENV{SAMNTDIR}/api/include/
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
                ${MY_SRC} $ENV{SAMNTDIR}/api/src/
            COMMAND ${CMAKE_COMMAND} -E copy
                ${PYSAM_SRC} $ENV{PYSAMDIR}/src/
    )

endif()