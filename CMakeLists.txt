#####################################################################################################################
#
# CMake and System Settings
#
#####################################################################################################################

cmake_minimum_required(VERSION 3.19)

set(CMAKE_OSX_DEPLOYMENT_TARGET "10.15" CACHE STRING "Minimum OS X deployment version")

set(CMAKE_XCODE_ATTRIBUTE_OTHER_CODE_SIGN_FLAGS "-o linker-signed")

if (UNIX AND NOT CMAKE_C_COMPILER)
	set(CMAKE_C_COMPILER gcc)
	set(CMAKE_CXX_COMPILER g++)
endif()


#####################################################################################################################
#
# Project Settings
#
#####################################################################################################################

option(SAMAPI_EXPORT "Export of ssc binaries to the SAM_api directory; for Unix, compile ssc libraries for SAM_api" ON)

option(SAM_SKIP_AUTOGEN "Skips the automatic regeneration of SAMAPI files from export_config" ON)

Project(samos)

if (SAMAPI_EXPORT)
	add_subdirectory(api)
endif()

include_directories(src $ENV{WEXDIR}/include $ENV{LKDIR}/include $ENV{SSCDIR} ../tcs )

set(SAM_SRC src/private.h
    src/main_add.h
	src/ide.cpp            src/ide.h
	src/lossdiag.cpp       src/lossdiag.h
	src/defmgr.cpp         src/defmgr.h
	src/urdb.cpp           src/urdb.h
	src/macro.cpp          src/macro.h
	src/s3engine.cpp       src/s3engine.h
	src/s3objects.cpp      src/s3objects.h
	src/s3tool.cpp         src/s3tool.h
	src/s3view.cpp         src/s3view.h
	src/stochastic.cpp     src/stochastic.h
	src/urdb.cpp           src/urdb.h
	src/nsrdb.cpp          src/nsrdb.h
	src/windtoolkit.cpp    src/windtoolkit.h
	src/wavetoolkit.cpp    src/wavetoolkit.h
	src/gridsupport.cpp    src/gridsupport.h
	src/macro.cpp          src/macro.h
	src/p50p90.cpp         src/p50p90.h
	src/reports.cpp        src/reports.h
	src/script.cpp         src/script.h
	src/graph.cpp          src/graph.h
	src/uncertainties.cpp  src/uncertainties.h
	src/excelexch.cpp      src/excelexch.h
	src/simulation.cpp     src/simulation.h
	src/library.cpp        src/library.h
	src/results.cpp        src/results.h
	src/ipagelist.cpp      src/ipagelist.h
	src/casewin.cpp        src/casewin.h
	src/invoke.cpp         src/invoke.h
	src/case.cpp           src/case.h
	src/main.cpp           src/main.h
	src/equations.cpp      src/equations.h
	src/inputpage.cpp      src/inputpage.h
	src/uiobjects.cpp      src/uiobjects.h
	src/lossadj.cpp        src/lossadj.h
	src/widgets.cpp        src/widgets.h
	src/variables.cpp      src/variables.h
	src/project.cpp        src/project.h
	src/object.cpp         src/object.h
	src/variablegrid.cpp   src/variablegrid.h
	src/parametric.cpp     src/parametric.h
	src/welcome.cpp        src/welcome.h
	src/ptlayoutctrl.cpp   src/ptlayoutctrl.h
	src/troughloop.cpp     src/troughloop.h
	src/shadingfactors.cpp src/shadingfactors.h
	src/materials.cpp      src/materials.h
	src/codegenerator.cpp  src/codegenerator.h
	src/pythonhandler.cpp src/pythonhandler.h
	src/combinecases.cpp src/combinecases.h)


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
	foreach (flag_var CMAKE_C_FLAGS_DEBUG CMAKE_CXX_FLAGS_DEBUG)
		set(${flag_var} "${${flag_var}} /D_DEBUG" CACHE STRING "compile flags" FORCE)
	endforeach()
else(MSVC)
	if (APPLE)
		add_definitions(-DWX_PRECOMP)
	else()
		add_compile_options(-Wno-deprecated)
	endif()
	add_compile_options(-Wall -Wno-unknown-pragmas -Wno-overloaded-virtual -std=c++11)
	if(CMAKE_BUILD_TYPE STREQUAL "Debug")
		add_compile_definitions(_DEBUG)
	else()
		add_compile_options(-O3 -DNDEBUG)
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
    if (EXISTS /usr/local/bin/wx-config-3)
        set(wxWidgets_CONFIG_EXECUTABLE /usr/local/bin/wx-config-3)
    else ()
        set(wxWidgets_CONFIG_EXECUTABLE $ENV{WXMSW3}/bin/wx-config)
    endif ()

    if (APPLE)
        find_package(wxWidgets REQUIRED xrc webview stc richtext ribbon propgrid aui gl html qa adv core xml net base )
    else()
	    find_package(wxWidgets REQUIRED xrc stc richtext ribbon propgrid aui gl html qa adv core xml net base )
    endif()
else()
	set(wxWidgets_ROOT_DIR $ENV{WXMSW3})
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
	set(SAM_ICON ${CMAKE_CURRENT_SOURCE_DIR}/build_resources/SAM.icns)
	set_source_files_properties(${SAM_ICON} PROPERTIES
		MACOSX_PACKAGE_LOCATION "Resources")
elseif(WIN32)
	set(SAM_ICON ${CMAKE_CURRENT_SOURCE_DIR}/build_resources/SAM.rc)
endif()

# have to make executables with different names so the .app's have different names
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
	add_executable(SAMOSd
			WIN32
			MACOSX_BUNDLE
			${SAM_SRC}
			${SAM_ICON})
	set(SAM_EXE SAMOSd)
else() # if Release or if MSVC multigenerator
	add_executable(SAMOS
			WIN32
			MACOSX_BUNDLE
			${SAM_SRC}
			${SAM_ICON})
	set(SAM_EXE SAMOS)
endif()

if (MSVC)
	set_target_properties(${SAM_EXE}
		PROPERTIES
		LINK_FLAGS /SUBSYSTEM:WINDOWS
        DEBUG_POSTFIX "d"
		RUNTIME_OUTPUT_DIRECTORY $<1:${CMAKE_CURRENT_SOURCE_DIR}>/deploy/x64)
else()
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        set(SAM_NAME SAMOSd)
    else()
        set(SAM_NAME SAMOS)
    endif()
    if(APPLE)
        if (CMAKE_GENERATOR STREQUAL Xcode)
            if(CMAKE_BUILD_TYPE STREQUAL "Debug")
                set(SAM_APP ${CMAKE_CURRENT_BINARY_DIR}/Debug/${SAM_NAME}.app)
            else()
                set(SAM_APP ${CMAKE_CURRENT_BINARY_DIR}/Release/${SAM_NAME}.app)
            endif()
        else()
            set(SAM_APP ${CMAKE_CURRENT_BINARY_DIR}/${SAM_NAME}.app)
        endif()
    else()
        set(SAM_APP ${CMAKE_CURRENT_BINARY_DIR}/deploy)
        set_target_properties(${SAM_EXE}
                PROPERTIES
                OUTPUT_NAME ${SAM_NAME}.bin
                RUNTIME_OUTPUT_DIRECTORY ${SAM_APP}/linux_64)
    endif()
endif()

#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

if (${CMAKE_PROJECT_NAME} STREQUAL system_advisor_model)
	target_link_libraries(${SAM_EXE} lk wex ssc)
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
			PATHS $ENV{SSC_LIB} $ENV{SSCDIR}/build/ssc $ENV{SSCDIR}/build/ssc/Release)
	target_link_libraries(${SAM_EXE} optimized ${WEX_LIB} optimized ${SSC_LIB} optimized ${LK_LIB})

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
		target_link_libraries(${SAM_EXE} debug ${SSCD_LIB} debug ${WEXD_LIB} debug ${LKD_LIB})
	endif()
endif()

target_link_libraries(${SAM_EXE} ${wxWidgets_LIBRARIES})

if (UNIX)
	target_link_libraries(${SAM_EXE} -lm -lcurl)
elseif (MSVC)
	find_library( CURL_LIB
				NAMES libcurl.lib
				PATHS $ENV{WEXDIR}/build_resources/libcurl_ssl_x64/lib)
	target_link_libraries(${SAM_EXE} ${CURL_LIB} Winhttp)
endif()


#####################################################################################################################
#
# Target Installation
#
#####################################################################################################################

SET(CMAKE_SKIP_BUILD_RPATH)

if (UNIX)
    if (${CMAKE_PROJECT_NAME} STREQUAL system_advisor_model)
        set(SSC_SO $<TARGET_FILE:ssc>)
    else()
        set(SSC_SO $<IF:$<CONFIG:Debug>,${SSCD_LIB},${SSC_LIB}>)
    endif()
    if (APPLE)
        if(CMAKE_BUILD_TYPE STREQUAL "Debug")
            set(SSC_NAME sscd)
        else()
            set(SSC_NAME ssc)
        endif()
        if (SAMAPI_EXPORT)
            set(SSC_NAME "lib${SSC_NAME}.so")
        else()
            set(SSC_NAME "${SSC_NAME}.dylib")
        endif ()
        add_custom_command(
            TARGET ${SAM_EXE}
            POST_BUILD
            COMMAND rsync -a -v ${CMAKE_CURRENT_SOURCE_DIR}/deploy/* ${SAM_APP}/Contents
            COMMAND rm ${SAM_APP}/Contents/test_script_ow.lk
            COMMAND rm ${SAM_APP}/Contents/dummy.sam
            COMMAND mkdir -p ${SAM_APP}/Contents/runtime
            COMMAND mkdir -p ${SAM_APP}/Contents/runtime/bin
            COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/Sandia/osx/lhs.bin ${SAM_APP}/Contents/runtime/bin
            COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/Sandia/osx/stepwise.bin ${SAM_APP}/Contents/runtime/bin
            COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/build_resources/Info-${SAM_NAME}.plist ${SAM_APP}/Contents/Info.plist
            COMMAND mkdir -p ${SAM_APP}/Contents/Frameworks
            COMMAND cp ${SSC_SO} ${SAM_APP}/Contents/Frameworks
            COMMAND install_name_tool -id @executable_path/../Frameworks/${SSC_NAME} ${SAM_APP}/Contents/Frameworks/${SSC_NAME}
            COMMAND install_name_tool -change ${SSC_NAME} @executable_path/../Frameworks/${SSC_NAME} ${SAM_APP}/Contents/MacOS/${SAM_NAME}
        #    COMMAND install_name_tool -add_rpath @executable_path/../Frameworks/ ${SAM_APP}/Contents/MacOS/${SAM_NAME}  
        )
    else()
        add_custom_command(
                TARGET ${SAM_EXE}
                PRE_BUILD
                COMMAND rm -rf ${SAM_APP}/linux_64
                COMMAND mkdir -p ${SAM_APP}/linux_64
        )
        add_custom_command(
                TARGET ${SAM_EXE}
                POST_BUILD
                COMMAND rsync -a -v ${CMAKE_CURRENT_SOURCE_DIR}/deploy/* ${SAM_APP}
                COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/Sandia/linux/lhs.bin ${SAM_APP}/runtime/bin
                COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/Sandia/linux/stepwise.bin ${SAM_APP}/runtime/bin
#                COMMAND cp -r ${CMAKE_CURRENT_SOURCE_DIR}/build_linux/GtkTheme ${SAM_APP}/linux_64
                COMMAND cp ${SSC_SO} ${SAM_APP}/linux_64
        )

        #		install(TARGETS ${SAM_EXE}
        #			RUNTIME DESTINATION linux_64)
        #		install(FILES ${SSC_LIB}
        #			DESTINATION linux_64)
        #		install(FILES Sandia/linux/lhs.bin Sandia/linux/stepwise.bin
        #			DESTINATION runtime/bin)
        #		install(DIRECTORY build_linux/GtkTheme
        #			DESTINATION linux_64)
        #		install(DIRECTORY deploy/libraries deploy/runtime deploy/solar_resource deploy/wind_resource
        #			DESTINATION .)

        #		set(CPACK_GENERATOR "STGZ;TGZ")
        #		set(CPACK_PACKAGE_FILE_NAME "sam.deploy")
        #		set(CPACK)
        #		include(CPack)
        #		add_custom_target( installer
        #			COMMAND "rm" "-f" "install0"
        #			COMMAND "echo" "#!/bin/sh" ">" "install0"
        #			COMMAND "echo" "SAMVER=Open.Source" ">>" "install0"
        #			COMMAND "cat" "install0" "${CMAKE_SOURCE_DIR}/build_linux/install.sh" "sam.deploy.tar.gz" ">" "sam-linux-$(SAMVER).run"
        #			COMMAND "chmod" "a+x" "sam-linux-$(SAMVER).run"
        #			COMMAND "./sam-linux-$(SAMVER).run"
        #			DEPENDS SAM)
    endif()
else()
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

    set(SAMEXE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/deploy/x64)

    foreach( file_i ${LIBCURL_FILES})
        add_custom_command(
        TARGET ${SAM_EXE}
        POST_BUILD
        COMMAND ${CMAKE_COMMAND}
        ARGS -E copy $ENV{WEXDIR}/build_resources/libcurl_ssl_x64/bin/${file_i} ${SAMEXE_DIR}
    )
    endforeach( file_i )

    if (${CMAKE_PROJECT_NAME} STREQUAL system_advisor_model)
        set(SSC_DLL $<TARGET_FILE:ssc>)
        set(SSC_LIB $<TARGET_FILE_DIR:ssc>/$<TARGET_FILE_BASE_NAME:ssc>.lib)
    else()
        set(SSC_DLL $<IF:$<CONFIG:Debug>,${SSCD_LIB},${SSC_LIB}>)
        string(REPLACE ".dll" ".lib" SSC_LIB ${SSC_DLL})
    endif()
    add_custom_command(
        TARGET ${SAM_EXE}
        POST_BUILD
        COMMAND ${CMAKE_COMMAND}
        ARGS -E copy ${SSC_DLL} ${SSC_LIB} ${SAMEXE_DIR}
    )
endif()

