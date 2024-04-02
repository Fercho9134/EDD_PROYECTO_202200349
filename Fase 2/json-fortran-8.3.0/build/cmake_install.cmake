# Install script for directory: C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "C:/Program Files (x86)")
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

# Set default install directory permissions.
if(NOT DEFINED CMAKE_OBJDUMP)
  set(CMAKE_OBJDUMP "C:/TDM-GCC-64/bin/objdump.exe")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/lib" TYPE STATIC_LIBRARY OPTIONAL FILES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/libjsonfortran.dll.a")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/lib" TYPE STATIC_LIBRARY FILES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/lib/libjsonfortran.a")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(GLOB_RECURSE MODULE_FILES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/include/*.mod")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(GLOB_RECURSE SUBMOD_FILES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/include/*.smod")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL ${MODULE_FILES} DESTINATION "${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/lib")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL ${SUBMOD_FILES} DESTINATION "${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/lib")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/cmake/jsonfortran-gnu-targets.cmake")
    file(DIFFERENT _cmake_export_file_changed FILES
         "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/cmake/jsonfortran-gnu-targets.cmake"
         "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/CMakeFiles/Export/12f567646e65c05937922d4fe92037a8/jsonfortran-gnu-targets.cmake")
    if(_cmake_export_file_changed)
      file(GLOB _cmake_old_config_files "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/cmake/jsonfortran-gnu-targets-*.cmake")
      if(_cmake_old_config_files)
        string(REPLACE ";" ", " _cmake_old_config_files_text "${_cmake_old_config_files}")
        message(STATUS "Old export file \"$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/cmake/jsonfortran-gnu-targets.cmake\" will be replaced.  Removing files [${_cmake_old_config_files_text}].")
        unset(_cmake_old_config_files_text)
        file(REMOVE ${_cmake_old_config_files})
      endif()
      unset(_cmake_old_config_files)
    endif()
    unset(_cmake_export_file_changed)
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/cmake" TYPE FILE FILES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/CMakeFiles/Export/12f567646e65c05937922d4fe92037a8/jsonfortran-gnu-targets.cmake")
  if(CMAKE_INSTALL_CONFIG_NAME MATCHES "^([Rr][Ee][Ll][Ee][Aa][Ss][Ee])$")
    file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/cmake" TYPE FILE FILES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/CMakeFiles/Export/12f567646e65c05937922d4fe92037a8/jsonfortran-gnu-targets-release.cmake")
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/cmake" TYPE FILE FILES
    "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/pkg/jsonfortran-gnu-config.cmake"
    "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/jsonfortran-gnu-config-version.cmake"
    )
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/lib/pkgconfig" TYPE FILE FILES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/json-fortran.pc")
endif()

if(CMAKE_INSTALL_COMPONENT)
  set(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
else()
  set(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
endif()

string(REPLACE ";" "\n" CMAKE_INSTALL_MANIFEST_CONTENT
       "${CMAKE_INSTALL_MANIFEST_FILES}")
file(WRITE "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/${CMAKE_INSTALL_MANIFEST}"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")
