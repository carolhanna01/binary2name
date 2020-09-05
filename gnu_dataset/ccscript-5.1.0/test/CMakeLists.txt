# Copyright (C) 2009 David Sugar, Tycho Softworks
#
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
include_directories(${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/..inc)

link_directories(${CMAKE_CURRENT_BINARY_DIR}/../src)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/mergescript.scr
    ${CMAKE_CURRENT_BINARY_DIR}/mergescript.scr COPYONLY)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/testscript.scr
    ${CMAKE_CURRENT_BINARY_DIR}/testscript.scr COPYONLY)

add_executable(test-ccScript script.cpp)
target_link_libraries(test-ccScript ccscript)
add_test(NAME ccScript COMMAND test-ccScript)

