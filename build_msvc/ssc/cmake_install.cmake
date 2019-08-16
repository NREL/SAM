# Install script for directory: C:/SAM-Development/sam/ssc

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "C:/Program Files/system_advisor_model")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "Release")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  include("C:/SAM-Development/sam/build_msvc/ssc/splinter/cmake_install.cmake")
  include("C:/SAM-Development/sam/build_msvc/ssc/shared/cmake_install.cmake")
  include("C:/SAM-Development/sam/build_msvc/ssc/nlopt/cmake_install.cmake")
  include("C:/SAM-Development/sam/build_msvc/ssc/lpsolve/cmake_install.cmake")
  include("C:/SAM-Development/sam/build_msvc/ssc/solarpilot/cmake_install.cmake")
  include("C:/SAM-Development/sam/build_msvc/ssc/tcs/cmake_install.cmake")
  include("C:/SAM-Development/sam/build_msvc/ssc/ssc/cmake_install.cmake")
  include("C:/SAM-Development/sam/build_msvc/ssc/sdktool/cmake_install.cmake")
  include("C:/SAM-Development/sam/build_msvc/ssc/tcsconsole/cmake_install.cmake")
  include("C:/SAM-Development/sam/build_msvc/ssc/test/cmake_install.cmake")

endif()

