/* Copyright (C) 2009-2014 David Sugar, Tycho Softworks.
   Copyright (C) 2015 Cherokees of Idaho.

   This file is free software; as a special exception the author gives
   unlimited permission to copy and/or distribute it, with or without
   modifications, as long as this notice is preserved.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
   implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*/

#cmakedefine PACKAGE "${PROJECT_NAME}"
#define STDC_HEADERS 1
#cmakedefine VERSION "${VERSION}"
#cmakedefine HAVE_ENDIAN_H 1
#cmakedefine BUILD_STATIC 1
#define DEFAULT_LIBPATH "${CMAKE_INSTALL_FULL_LIBDIR}"
