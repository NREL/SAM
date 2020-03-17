#####################################################################################################################
#
# CMake and System Settings
#
#####################################################################################################################

set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 3.8)

if(APPLE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")
endif()


#####################################################################################################################
#
# Project Settings 
#
#####################################################################################################################

Project(sam_test)

if (DEFINED ENV{GTDIR})
	set(GTDIR $ENV{GTDIR})
elseif (DEFINED ENV{GTEST})
	set(GTDIR $ENV{GTEST})
endif()

include_directories(${GTDIR}/include ${GTDIR}/googletest/include . ../src ../ssc ../lk/include ../wex/include)

file(GLOB SAM_TESTS *.cpp)
# files to test
set(SAM_SRC
		../src/variables.cpp)

#####################################################################################################################
#
# Compile Options per Platform
#
#####################################################################################################################

set(CMAKE_CXX_STANDARD 11)

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
		add_compile_options(-arch x86_64 -fno-common)
		add_definitions(-DWX_PRECOMP)
	else()
		add_compile_options(-Wno-deprecated -Wno-misleading-indentation)
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

# setup wxWidgets
if (UNIX)
	set(wxWidgets_CONFIG_EXECUTABLE /usr/local/bin/wx-config-3)
	find_package(wxWidgets REQUIRED xrc stc richtext ribbon propgrid aui gl html qa adv core xml net base)
else()
	set(wxWidgets_ROOT_DIR $ENV{WXMSW3})
	find_package(wxWidgets REQUIRED xrc stc richtext ribbon propgrid aui gl html qa adv core xml net base scintilla)
endif()

include(${wxWidgets_USE_FILE})
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${wxWidgets_CXX_FLAGS}")


#####################################################################################################################
# 
# CMake Targets
#
#####################################################################################################################

add_executable(SAMtest
	${SAM_TESTS}
	${SAM_SRC}
	main.cpp)

set_target_properties(SAMtest
    PROPERTIES
    DEBUG_POSTFIX "d"
	PREFIX ""
	)

if (MSVC)
	set_target_properties(SAMtest PROPERTIES
        LINK_FLAGS /SUBSYSTEM:CONSOLE)
endif()


#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

if (${CMAKE_PROJECT_NAME} STREQUAL system_advisor_model)
	target_link_libraries(SAMtest ssc wex lk)
else()
	unset(WEX_LIB CACHE)
	unset(LK_LIB CACHE)
	unset(SSC_LIB CACHE)
	find_library( WEX_LIB
			NAMES wex.a wex.lib
			PATHS ../wex/build ../wex/build/Release)
	find_library( LK_LIB
			NAMES lk.a lk.lib
			PATHS ../lk/build ../lk/build/Release)
	find_library( SSC_LIB
			NAMES ssc.dylib ssc.lib ssc.so
			PATHS ../ssc/build/ssc ../ssc/build/ssc/Release)
	target_link_libraries(SAMtest optimized ${SSC_LIB} optimized ${WEX_LIB} optimized ${LK_LIB})

	if (CMAKE_BUILD_TYPE STREQUAL "Debug" OR MSVC)
		unset(LKD_LIB CACHE)
		unset(WEXD_LIB CACHE)
		unset(SSCD_LIB CACHE)
		find_library( WEXD_LIB
				NAMES wexd.a wexd.lib
				PATHS ../wex/build ../wex/build/Debug)
		find_library( LKD_LIB
				NAMES lkd.a lkd.lib
				PATHS ../lk/build ../lk/build/Debug)
		find_library( SSCD_LIB
				NAMES sscd.dylib sscd.lib sscd.so
				PATHS ../ssc/build/ssc ../ssc/build/ssc/Debug)
		target_link_libraries(SAMtest debug ${SSCD_LIB} debug ${WEXD_LIB} debug ${LKD_LIB})
	endif()
endif()

target_link_libraries(SAMtest ${wxWidgets_LIBRARIES})


unset(GTEST_LIB CACHE)
find_library( GTEST_LIB
	NAMES libgtest.a gtest.lib
	PATHS ${GTDIR}/build/lib ${GTDIR}/build/lib/Release)
target_link_libraries(SAMtest optimized ${GTEST_LIB})

if (CMAKE_BUILD_TYPE STREQUAL "Debug" OR MSVC)
	unset(GTESTD_LIB CACHE)
	find_library( GTESTD_LIB
		NAMES libgtestd.a gtestd.lib
		PATHS ${GTDIR}/build/lib ${GTDIR}/build/lib/Debug)
	target_link_libraries(SAMtest debug ${GTESTD_LIB})
endif()

if (UNIX)
	target_link_libraries(SAMtest -ldl -lpthread)
endif()


#####################################################################################################################
#
# Target Installation
#
#####################################################################################################################

if(MSVC)
	set(TEST_DIR ${CMAKE_CURRENT_BINARY_DIR}/$<$<CONFIG:Debug>:Debug>$<$<CONFIG:Release>:Release>)

	set(SSC_LIB $<TARGET_FILE_DIR:ssc>/$<TARGET_FILE_BASE_NAME:ssc>.lib)
	add_custom_command(
	    TARGET SAMtest
	    POST_BUILD
		COMMAND ${CMAKE_COMMAND}
		ARGS -E remove -f ${TEST_DIR}/$<TARGET_FILE_BASE_NAME:ssc>.lib 
		COMMAND ${CMAKE_COMMAND}
		ARGS -E remove -f ${TEST_DIR}/$<TARGET_FILE_BASE_NAME:ssc>.dll
	    COMMAND ${CMAKE_COMMAND}
	    ARGS -E copy $<TARGET_FILE:ssc> ${SSC_LIB} ${TEST_DIR}
	)
endif()