#####################################################################################################################
#
# CMake and System Settings
#
#####################################################################################################################

set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 3.11)

if(APPLE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")
	set(CMAKE_MACOSX_RPATH 1)
elseif(MSVC)
    set(CMAKE_SYSTEM_VERSION 8.1 CACHE TYPE INTERNAL FORCE)
endif()


#####################################################################################################################
#
# Project Settings 
#
#####################################################################################################################

Project(sam)

add_subdirectory(api)

if (MSVC)
	set(CMAKE_RUNTIME_OUTPUT_DIRECTORY $<1:${CMAKE_CURRENT_SOURCE_DIR}/deploy/x64>)
else()
	set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
endif()

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
	src/uncertainties.cpp
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


#####################################################################################################################
#
# Compile Options per Platform
#
#####################################################################################################################

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if(MSVC)
	add_compile_options(/W4 /MP)
	foreach (flag_var CMAKE_C_FLAGS_DEBUG CMAKE_CXX_FLAGS_DEBUG)
		set(${flag_var} "${${flag_var}} /D_DEBUG" CACHE STRING "compile flags" FORCE)
	endforeach()
	add_compile_definitions( _CRT_SECURE_NO_WARNINGS _MBCS __WXMSW__ __64BIT__)
else(MSVC)
	if (APPLE)
		add_compile_options(-arch x86_64 -fno-common)
		add_definitions(-DWX_PRECOMP)
	else()
		add_compile_options(-Wno-deprecated -Wno-misleading-indentation)
	endif()
	add_compile_options(-Wall -Wno-unknown-pragmas -Wno-overloaded-virtual -std=c++11)
	if(CMAKE_BUILD_TYPE STREQUAL "Debug")
		set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
		set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
		add_compile_definitions(_DEBUG)
	else()
		add_compile_options(-O3)
	endif()
endif(MSVC)
add_compile_options(${wxWidgets_CXX_FLAGS})
add_definitions(-DLK_USE_WXWIDGETS )




#####################################################################################################################
#
# Packages
#
#####################################################################################################################

if (UNIX)
	set(wxWidgets_CONFIG_EXECUTABLE /usr/local/bin/wx-config-3)
	find_package(wxWidgets REQUIRED xrc webview stc richtext ribbon propgrid aui gl html qa adv core xml net base )
else()
	set(wxWidgets_ROOT_DIR $ENV{WXMSW3})
	if (CMAKE_BUILD_TYPE STREQUAL "Debug")
		set(wxWidgets_CONFIGURATION mswud)
	else()
		set(wxWidgets_CONFIGURATION mswu)
	endif()
	find_package(wxWidgets REQUIRED xrc webview stc richtext ribbon propgrid aui gl html qa adv core xml net base scintilla)
endif()

include(${wxWidgets_USE_FILE})


#####################################################################################################################
# 
# CMake Targets
#
#####################################################################################################################

# Setup Application
if (APPLE)
	set(MACOSX_BUNDLE_ICON_FILE SAM.icns)
	set(SAM_ICON ${CMAKE_CURRENT_SOURCE_DIR}/build_osx/SAM.icns)
	set_source_files_properties(${SAM_ICON} PROPERTIES
		MACOSX_PACKAGE_LOCATION "Resources")
elseif(WIN32)
	set(SAM_ICON ${CMAKE_CURRENT_SOURCE_DIR}/build_vs2017/SAM.rc)
endif()

add_executable(SAM ${SAM_SRC} ${SAM_ICON})
set_target_properties(SAM
    PROPERTIES
    DEBUG_POSTFIX "d"
	RUNTIME_OUTPUT_DIRECTORY $<1:${CMAKE_CURRENT_SOURCE_DIR}>)
if (MSVC)
	set_target_properties(SAM
		PROPERTIES
		LINK_FLAGS /SUBSYSTEM:WINDOWS)
endif()


#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

	unset(WEXD_LIB CACHE)
	unset(LKD_LIB CACHE)
	unset(SSCD_LIB CACHE)
	unset(WEXD_LIB CACHE)
	unset(LKD_LIB CACHE)
	unset(SSC_LIB CACHE)
	find_library( WEXD_LIB
			NAMES wexd.a wexd.lib
			PATHS $ENV{WEXDIR})
	find_library( LKD_LIB
			NAMES lkd.a lkd.lib
			PATHS $ENV{LKDIR})
	find_library( SSCD_LIB
			NAMES sscd.dylib sscd.so sscd.lib
			PATHS $ENV{SSCDIR}/ssc)
	find_library( WEX_LIB
			NAMES wex.a wex.lib
			PATHS $ENV{WEXDIR})
	find_library( LK_LIB
			NAMES lk.a lk.lib
			PATHS $ENV{LKDIR})
	find_library(SSC_LIB
			NAMES  ssc.dylib ssc.so ssc.lib
			PATHS $ENV{SSCDIR}/ssc)
	message(STATUS "RELEASE: SSC_LIB=${SSC_LIB}")
	message(STATUS "DEBUG: SSCD_LIB=${SSCD_LIB}")

	target_link_libraries(SAM debug ${SSCD_LIB} debug ${WEXD_LIB} debug ${LKD_LIB} )
	target_link_libraries(SAM optimized ${SSC_LIB} optimized ${WEX_LIB} optimized ${LK_LIB} )

target_link_libraries(SAM ${wxWidgets_LIBRARIES})

if (UNIX)
	target_link_libraries(${SAM_EXE} -lm -lcurl)
elseif (MSVC)
	find_library( CURL_LIB
				NAMES libcurl.lib
				PATHS $ENV{WEXDIR}/build_vs2017/libcurl_ssl_x64/lib)
	target_link_libraries(SAM ${CURL_LIB})
	target_link_libraries(SAM Winhttp)
endif()


#####################################################################################################################
#
# Target Installation
#
#####################################################################################################################

SET(CMAKE_SKIP_BUILD_RPATH)

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
	set(SSC_NAME sscd)
else()
	set(SSC_NAME ssc)
endif()

if (APPLE)
	set(SAM_APP ${CMAKE_CURRENT_SOURCE_DIR}/${SAM_EXE}.app)
	add_custom_command(
		TARGET ${SAM_EXE}
		POST_BUILD
		COMMAND rsync -a -v --ignore-existing ${CMAKE_CURRENT_SOURCE_DIR}/deploy/* ${SAM_APP}/Contents
		COMMAND mkdir -p ${SAM_APP}/Contents/runtime
		COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/Sandia/osx/lhs.bin ${SAM_APP}/Contents/runtime
		COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/Sandia/osx/stepwise.bin ${SAM_APP}/Contents/runtime
		COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/build_osx/Info-SAM.plist ${SAM_APP}/Contents
		COMMAND mkdir -p ${SAM_APP}/Contents/Frameworks
		COMMAND cp $ENV{SSCDIR}/ssc/${SSC_LIB}.dylib ${SAM_APP}/Contents/Frameworks
		COMMAND install_name_tool -id @executable_path/../Frameworks/${SSC_LIB}.dylib ${SAM_APP}/Contents/Frameworks/${SSC_NAME}.dylib
		COMMAND install_name_tool -change ${SSC_LIB} @executable_path/../Frameworks/${SSC_NAME}.dylib ${SAM_APP}/Contents/MacOS/${SAM_EXE}
	)
else()
	if (UNIX)
		set_target_properties(${SAM_EXE}
			PROPERTIES OUTPUT_NAME sam.bin)
		add_custom_command(
			TARGET ${SAM_EXE}
			POST_BUILD
			COMMAND rm -rf ${PROJECT_SOURCE_DIR}/sam.deploy)
		set(CMAKE_INSTALL_PREFIX ${PROJECT_SOURCE_DIR}/sam.deploy)
		install(TARGETS ${SAM_EXE}
			RUNTIME DESTINATION linux_64)
		install(FILES ${SSC_LIB}
			DESTINATION linux_64)
		install(FILES Sandia/linux/lhs.bin Sandia/linux/stepwise.bin
			DESTINATION runtime/bin)
		install(DIRECTORY build_linux/GtkTheme
			DESTINATION linux_64)
		install(DIRECTORY deploy/libraries deploy/runtime deploy/solar_resource deploy/wind_resource
			DESTINATION .)

		set(CPACK_GENERATOR "STGZ;TGZ")
		set(CPACK_PACKAGE_FILE_NAME "sam.deploy")
		set(CPACK)
		include(CPack)
		add_custom_target( installer
			COMMAND "rm" "-f" "install0"
			COMMAND "echo" "#!/bin/sh" ">" "install0"
			COMMAND "echo" "SAMVER=Open.Source" ">>" "install0"
			COMMAND "cat" "install0" "${CMAKE_SOURCE_DIR}/build_linux/install.sh" "sam.deploy.tar.gz" ">" "sam-linux-$(SAMVER).run"
			COMMAND "chmod" "a+x" "sam-linux-$(SAMVER).run"
			COMMAND "./sam-linux-$(SAMVER).run"
			DEPENDS ${SAM_EXE})
	elseif(MSVC)
		set( LIBCURL_FILES
		    dbghelp.dll
		    libcurl.dll
		    libeay32.dll
		    libssh2.dll
		    msvcp140.dll
		    openssl.exe
		    ssleay32.dll
		    vcruntime140.dll
		)

		foreach( file_i ${LIBCURL_FILES})
		    add_custom_command(
		    TARGET SAM
		    POST_BUILD
		    COMMAND ${CMAKE_COMMAND}
		    ARGS -E copy $ENV{WEXDIR}/build_vs2017/libcurl_ssl_x64/bin/${file_i} ${CMAKE_CURRENT_SOURCE_DIR}/deploy/x64
		)
		endforeach( file_i )



		add_custom_command(
		    TARGET SAM
		    POST_BUILD
		    COMMAND ${CMAKE_COMMAND}
		    ARGS -E copy $ENV{SSCDIR}/ssc/${SSC_LIB}.lib $ENV{SSCDIR}/ssc/${SSC_NAME}.dll ${CMAKE_CURRENT_SOURCE_DIR}/deploy/x64
		)
	endif()
endif()

