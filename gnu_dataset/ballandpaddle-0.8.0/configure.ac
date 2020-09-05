AC_INIT([GNU Ball and Paddle], [0.8.0], [hutchiep190@potsdam.edu], [ballandpaddle])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

AC_CONFIG_HEADERS([src/config.h])
AC_PROG_CXX

AC_GNU_SOURCE

AM_GNU_GETTEXT([external])

SDL_VERSION=1.2.0
AM_PATH_SDL($SDL_VERSION,
            CXXFLAGS="$CXXFLAGS $SDL_CFLAGS" LIBS="$SDL_LIBS $LIBS",
            AC_MSG_ERROR([*** SDL version $SDL_VERSION not found!]))

AC_CHECK_LIB([SDL_image], [IMG_LoadPNG_RW],
            LIBS="-lSDL_image $LIBS",
            AC_MSG_ERROR([library SDL_image not found]))

AC_CHECK_LIB([SDL_mixer], [Mix_LoadWAV_RW],
            LIBS="-lSDL_mixer $LIBS",
            AC_MSG_ERROR([library SDL_mixer not found]))

AC_CHECK_LIB([SDL_ttf], [TTF_OpenFontRW],
            LIBS="-lSDL_ttf $LIBS",
            AC_MSG_ERROR([library SDL_ttf not found]))

AC_CHECK_LIB(guile, scm_boot_guile,
            LIBS="-lguile $LIBS",
            AC_MSG_ERROR(can't find guile. This software REQUIRES guile.))


AC_CONFIG_FILES([
  Makefile
  src/Makefile
])
AC_OUTPUT([po/Makefile.in])
