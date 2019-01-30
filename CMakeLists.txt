set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 3.7)

set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")

if(MSVC)
	set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "Debug and Release Builds Configured" FORCE)
endif()

Project(sam)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

set(wxWidgets_CONFIG_EXECUTABLE /usr/local/bin/wx-config-3)
find_package(wxWidgets REQUIRED xrc webview stc richtext ribbon propgrid aui gl html qa adv core xml net base)
include(${wxWidgets_USE_FILE})

include_directories(src $ENV{WEXDIR}/include $ENV{LKDIR}/include $ENV{SSCDIR} ../tcs)

set(SAM_SRC
	src/ide.cpp
	src/lossdiag.cpp
	src/defmgr.cpp
	src/urdb.cpp
	src/macro.cpp
	src/s3engine.cpp
	src/s3objects.cpp
	src/s3tool.cpp
	src/s3view.cpp
	src/stochastic.cpp
	src/urdb.cpp
	src/nsrdb.cpp
	src/windtoolkit.cpp
	src/gridsupport.cpp
	src/macro.cpp
	src/p50p90.cpp
	src/reports.cpp
	src/script.cpp
	src/graph.cpp
	src/excelexch.cpp
	src/simulation.cpp
	src/library.cpp
	src/results.cpp
	src/ipagelist.cpp
	src/casewin.cpp
	src/invoke.cpp
	src/case.cpp
	src/main.cpp
	src/equations.cpp
	src/inputpage.cpp
	src/uiobjects.cpp
	src/lossadj.cpp
	src/widgets.cpp
	src/variables.cpp
	src/project.cpp
	src/object.cpp
	src/variablegrid.cpp
	src/parametric.cpp
	src/welcome.cpp
	src/ptlayoutctrl.cpp
	src/troughloop.cpp
	src/shadingfactors.cpp
	src/materials.cpp
	src/codegenerator.cpp)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# Setup Application
if (APPLE)
	set(MACOSX_BUNDLE_ICON_FILE SAM.icns)
	set(SAM_ICON ${CMAKE_SOURCE_DIR}/build_osx/SAM.icns)
	set_source_files_properties(${SAM_ICON} PROPERTIES
		MACOSX_PACKAGE_LOCATION "Resources")
elseif(WIN32)
	set(SAM_ICON ${CMAKE_SOURCE_DIR}/build_vs2013/SAM.rc)
endif()

# set build types for archive
unset(WEX_LIB CACHE)
unset(LK_LIB CACHE)
unset(SSC_LIB CACHE)
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
	add_executable(SAMd 
		WIN32
		MACOSX_BUNDLE
		${SAM_SRC}
		${SAM_ICON})
	set(SAM_EXE SAMd)
	find_library( WEX_LIB
		NAMES wexosxwx3d.a wexuxwx3d.a wexd.lib
		PATHS $ENV{WEXDIR})
	find_library( LK_LIB
		NAMES lkosxwx3d.a lkuxwx3d.lib
		PATHS $ENV{LKDIR})
	find_library( SSC_LIB
		NAMES sscd.dylib sscd.lib
		PATHS $ENV{SSCDIR}/ssc)
	set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
	set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
else()
	add_executable(SAM
		WIN32
		MACOSX_BUNDLE
		${SAM_SRC}
		${SAM_ICON})
	set(SAM_EXE SAM)
	find_library( WEX_LIB
		NAMES wexosxwx3.a  wexuxwx3.a wex.lib
		PATHS $ENV{WEXDIR})
	find_library( LK_LIB
		NAMES lkosxwx3.a lkuxwx3.a lk.lib
		PATHS $ENV{LKDIR})
	find_library(SSC_LIB
		NAMES ssc.dylib ssc.lib
		PATHS $ENV{SSCDIR}/ssc)
endif()

# setup linking dependencies and flags
target_link_libraries(${SAM_EXE} -lm -lcurl)
target_link_libraries(${SAM_EXE} ${WEX_LIB} ${LK_LIB} ${SSC_LIB}) 
target_link_libraries(${SAM_EXE} ${wxWidgets_LIBRARIES})

message("here ${SSC_LIB}")

if(MSVC)
# need msvc optimization levels & different configs, windows sdk
	add_compile_options(/W4 /WX)
	add_compile_definitions( _CRT_SECURE_NO_WARNINGS _MBCS __WXMSW__)
else(MSVC)
	add_compile_options(-Wall -O3 -arch x86_64 -fno-common ${wxWidgets_CXX_FLAGS})
	add_compile_definitions(LK_USE_WXWIDGETS WX_PRECOMP )
endif(MSVC)

# dynamic linking
SET(CMAKE_SKIP_BUILD_RPATH)

# create application info for executable
if (APPLE)
	set_target_properties(${SAM_EXE} PROPERTIES
		MACOSX_BUNDLE_INFO_PLIST ${CMAKE_SOURCE_DIR}/build_osx/Info-SAM.plist
	)
	set(SAM_APP ${CMAKE_CURRENT_SOURCE_DIR}/${SAM_EXE}.app)
	add_custom_command(
		TARGET ${SAM_EXE}
		POST_BUILD 
		COMMAND rsync -a -v --ignore-existing ${CMAKE_SOURCE_DIR}/deploy/* ${SAM_APP}/Contents
		COMMAND mkdir -p ${SAM_APP}/Contents/runtime
		COMMAND cp ${CMAKE_SOURCE_DIR}/Sandia/osx/lhs.bin ${SAM_APP}/Contents/runtime
		COMMAND cp ${CMAKE_SOURCE_DIR}/Sandia/osx/stepwise.bin ${SAM_APP}/Contents/runtime
		COMMAND mkdir -p ${SAM_APP}/Contents/Frameworks
		COMMAND cp ${SSC_LIB} ${SAM_APP}/Contents/Frameworks
		COMMAND install_name_tool -rpath $ENV{SSCDIR}/ssc ${SAM_APP}/Contents/Frameworks/ ${SAM_APP}/Contents/MacOS/SAM 
	)
endif()
