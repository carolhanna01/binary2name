# Copyright (C) 2009-2014 David Sugar, Tycho Softworks.
# Copyright (C) 2015 Cherokees of Idaho.
#
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#

cmake_minimum_required(VERSION 2.6)
PROJECT(ccscript)
set (VERSION 5.1.0)
set (USES_UCOMMON_REQUIRED 7.0.0)

INCLUDE(CheckIncludeFiles)
INCLUDE(CheckLibraryExists)
INCLUDE(GNUInstallDirs)

option(BUILD_RUNTIME "Set to OFF to build static runtime" ON)
option(BUILD_TESTING "Set to ON to build test programs" OFF)
MESSAGE( STATUS "Configuring GNU ccscript ${VERSION}...")

if (USES_UCOMMON_INCLUDE_DIRS)
    message(STATUS "  Using local ucommon dependency")
else()
    find_package(PkgConfig)
    pkg_check_modules(USES_UCOMMON REQUIRED ucommon>=${USES_UCOMMON_REQUIRED})
endif()

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/inc ${WITH_INCLUDES} ${USES_UCOMMON_INCLUDE_DIRS})
add_definitions(${USES_UCOMMON_CFLAGS} ${WITH_CFLAGS})
link_libraries(${WITH_LIBS})
link_directories(${USES_UCOMMON_LIBRARY_DIRS})

if(USES_UCOMMON_CMAKE_MACROS)
	set(CMAKE_MODULE_PATH "${USES_UCOMMON_CMAKE_MACROS}")
elseif(EXISTS "${USES_UCOMMON_PREFIX}/share/ucommon/cmake")
	set(CMAKE_MODULE_PATH "${USES_UCOMMON_PREFIX}/share/ucommon/cmake")
else()
	set(CMAKE_MODULE_PATH "${CMAKE_INSTALL_FULL_DATADIR}/ucommon/cmake")
endif()

include(CapeConfig)
include(CapeMakeTargets)

# by default we build static libs for windows, shared libs for unix.
# we may also set this from a top level cmake or -DWITH_XX_LIBS

if(BUILD_RUNTIME)
    set(BUILD_RUNTIME_TYPE SHARED)
else()
    set(BUILD_RUNTIME_TYPE STATIC)
endif()

check_include_files(endian.h HAVE_ENDIAN_H)
check_library_exists(m sqrt "" MATH_LIB)
if(MATH_LIB)
    set(MATH_LIB m)
endif()

file(GLOB ccscript_src src/*.cpp)
set(ccscript_inc inc/ccscript.h)
pc_libs(PKG_CCSCRIPT_LIBS ${MATH_LIB})

create_headers()
create_specfile()
create_scripts(noinst_scr PKGBUILD)
create_pcfiles(pc_files ccscript)

add_library(ccscript ${BUILD_RUNTIME_TYPE} ${ccscript_src} ${ccscript_inc})
set_library_version(ccscript)
set_source_dependencies(ccscript ucommon)
target_link_libraries(ccscript ${USES_UCOMMON_LIBRARIES} ${WITH_MATH_LIB} ${MATH_LIB})

if(BUILD_TESTING)
    ENABLE_TESTING()
    add_subdirectory(test)
endif()

install(FILES inc/ccscript.h DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
install(TARGETS ccscript DESTINATION ${CMAKE_INSTALL_LIBDIR})
install(FILES ${pc_files} DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig)

# we use automake builds for std targets...
#add_cape_make_targets(ccscript ${VERSION})
add_make_lint_target()
add_make_uninstall_target()
