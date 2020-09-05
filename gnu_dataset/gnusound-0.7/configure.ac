dnl
dnl GNUsound - configure.ac
dnl Copyright (C) 2002-2004  Pascal Haakmat <a.haakmat@chello.nl>
dnl
dnl This program is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 2 of the License, or
dnl (at your option) any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software
dnl Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
dnl

dnl You are more than welcome to improve this script because
dnl I don't understand autoconf that well and I don't know enough 
dnl about different architectures/OSs/distributions to know what 
dnl to do when something isn't there or not in the expected place. 

dnl In other words this file will become better (or not) as 
dnl people test it on more platforms and contribute their
dnl fixes.

dnl Process this file with autoconf to produce a configure script.
AC_INIT(gnusound, 0.7)
AC_CONFIG_SRCDIR(src/gui.h)
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_HEADER(src/defines.h)
AM_INIT_AUTOMAKE(AC_PACKAGE_NAME, AC_PACKAGE_VERSION)

dnl System type
AC_CANONICAL_HOST
case $host_cpu in
i686|x86|x86pc|k5|k6|k6-2|k6-3|pentium*|athlon*)
AC_DEFINE(HAVE_ARCH_X86,,[Whether this computer has an x86 CPU])
;;
esac

AC_GNU_SOURCE
AC_CPU_COUNT

dnl Checks for programs.
AC_PROG_INSTALL
AC_PROG_GCC_TRADITIONAL

dnl Checks for header files.
AC_CHECK_HEADER(dlfcn.h,,
  AC_MSG_ERROR([*** dlfcn.h not found ***]))
AC_CHECK_HEADER(byteswap.h, [AC_DEFINE(HAVE_BYTESWAP_H, 1)])

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_TYPE_SIZE_T
AC_CHECK_SIZEOF(char)
AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(int8_t)
AC_CHECK_SIZEOF(int16_t)
AC_CHECK_SIZEOF(int32_t)

if test "x${ac_cv_sizeof_char}" != "x${ac_cv_sizeof_int8_t}" -o \
         "x${ac_cv_sizeof_short}" != "x${ac_cv_sizeof_int16_t}" -o \
         "x${ac_cv_sizeof_int}" != "x${ac_cv_sizeof_int32_t}"; then
  messages="
* This system has unsupported data type widths:
  char   expected: ${ac_cv_sizeof_int8_t}, actual: ${ac_cv_sizeof_char}
  short  expected: ${ac_cv_sizeof_int16_t}, actual: ${ac_cv_sizeof_short}
  int    expected: ${ac_cv_sizeof_int32_t}, actual: ${ac_cv_sizeof_int}
  This may yield unexpected behavior, including crashes and data loss.
  Please report this to the author, including your system configuration.
"
fi

dnl Checks for library functions.
AC_TYPE_SIGNAL
AC_CHECK_FUNCS([asprintf vasprintf mlock munlock])
AC_CHECK_FUNC(backtrace_symbols, [AC_DEFINE(HAVE_BACKTRACE, 1, [backtrace_symbols check])])

dnl Some systems have the DocBook to XHTML XSL stylesheet 
dnl file in different locations.
AC_PATH_DB2XHTML_STYLESHEET

module_path=modules
CFLAGS=${CFLAGS="-Wall"}
CFLAGS="-pthread $CFLAGS"

dnl
dnl Probe for file drivers
dnl

file_drivers=
dnl Audiofile
if test -z "$AUDIOFILE_CONFIG"; then
   AC_PATH_PROG(AUDIOFILE_CONFIG, audiofile-config, no,
                [$PATH:/usr/local/bin:/opt/bin])
fi
if test "x$AUDIOFILE_CONFIG" != "xno"; then
   AC_MSG_CHECKING([whether $AUDIOFILE_CONFIG works])
   if ! "$AUDIOFILE_CONFIG" --version > /dev/null 2>&1; then
      AUDIOFILE_CONFIG=no
   else
      AC_MSG_RESULT(yes)
   fi
fi
if test ! "x$AUDIOFILE_CONFIG" = "xno"; then
   echo "`$AUDIOFILE_CONFIG --cflags --libs`" > ${module_path}/.file_af.cflags
   AC_DEFINE(HAVE_AUDIOFILE)
   file_drivers="audiofile"
fi

dnl LAME
AC_CHECK_HEADER([lame/lame.h],
  [AC_CHECK_LIB([mp3lame], [lame_init],
    [AC_DEFINE(HAVE_LAME)
     echo "-lmp3lame -lm" > ${module_path}/.file_lame.cflags
     file_drivers="lame $file_drivers"], 
    [echo > ${module_path}/.file_lame.cflags], [-lm])])

dnl sndfile
PKG_CHECK_MODULES(SNDFILE, sndfile >= 1.0.4,
  AC_DEFINE(HAVE_SNDFILE)
  file_drivers="sndfile $file_drivers"
  echo "$SNDFILE_CFLAGS $SNDFILE_LIBS" > ${module_path}/.file_sndfile.cflags,
  echo > ${module_path}/.file_sndfile.cflags
  echo "no")

dnl OGG
AC_CHECK_LIB([ogg], [ogg_stream_init],
  [AC_DEFINE(HAVE_OGG)
   use="$use -logg"], [], [])

dnl Vorbis
AC_CHECK_LIB([vorbis], [vorbis_info_init],
  [AC_DEFINE(HAVE_VORBIS)
   use="$use -lvorbis"], [], [])

dnl Vorbisenc
AC_CHECK_LIB([vorbisenc], [vorbis_encode_init],
  [AC_DEFINE(HAVE_VORBIS)
   use="$use -lvorbisenc"], [], [])

dnl FFMPEG
AC_CHECK_HEADER([ffmpeg/avformat.h],
  [AC_CHECK_LIB([avformat], [av_open_input_file],
    [AC_DEFINE(HAVE_FFMPEG)
     echo "-lavformat -lavcodec -lm $use" > ${module_path}/.file_ffmpeg.cflags
     file_drivers="ffmpeg $file_drivers"], 
    [echo > ${module_path}/.file_ffmpeg.cflags], [-lavcodec -lm])])

if test "x${file_drivers}" = "x"; then
  AC_MSG_ERROR([*** no suitable file drivers found ***])
fi

dnl FLAC
AC_CHECK_HEADER([FLAC/format.h],
  [AC_CHECK_LIB([FLAC], [FLAC__file_decoder_new],
    [AC_DEFINE(HAVE_FLAC)
     echo "-lFLAC -lm" > ${module_path}/.file_flac.cflags
     file_drivers="flac $file_drivers"], 
    [echo > ${module_path}/.file_flac.cflags], [-lm])])

if test "x${file_drivers}" = "x"; then
  AC_MSG_ERROR([*** no suitable file drivers found ***])
fi

dnl
dnl Probe for audio drivers
dnl

audio_drivers=
dnl OSS
AC_CHECK_HEADER(sys/soundcard.h,
  audio_drivers="oss"
  AC_DEFINE(HAVE_OSS),)

dnl ALSA
LIBS_BACKUP=$LIBS
AM_PATH_ALSA(1.0.2, 
  AC_DEFINE(HAVE_ALSA)
  audio_drivers="$audio_drivers alsa"
  echo "$ALSA_CFLAGS $ALSA_LIBS" > ${module_path}/.player_alsa.cflags,
  [echo > ${module_path}/.player_alsa.cflags
   echo "no"])
LIBS=$LIBS_BACKUP

dnl JACK
PKG_CHECK_MODULES(JACK,jack >= 0.9.4,
  AC_DEFINE(HAVE_JACK)
  audio_drivers="$audio_drivers jack"
  echo "$JACK_CFLAGS $JACK_LIBS" > ${module_path}/.player_jack.cflags,
  [echo > ${module_path}/.player_jack.cflags
   echo "no"])

if test "x${audio_drivers}" = "x"; then
  AC_MSG_ERROR([*** no suitable audio drivers found, install OSS (www.4front-tech.com) or ALSA (www.alsa-project.org) ***])
fi

dnl libsamplerate
use_samplerate=no
AC_ARG_WITH(libsamplerate,
     [  --with-libsamplerate    use libsamplerate (autodetect)],
         if test "x$withval" = "xyes"; then
            use_samplerate=yes
         fi,
         use_samplerate=yes)
if test "x$use_samplerate" = "xyes"; then
  PKG_CHECK_MODULES(SAMPLERATE,samplerate,
        AC_DEFINE(HAVE_SAMPLERATE,,[use libsamplerate])
        CFLAGS="$CFLAGS $SAMPLERATE_CFLAGS"
        LIBS="$LIBS $SAMPLERATE_LIBS",
        use_samplerate=no
        messages="$messages
* Recommend you install libsamplerate (SRC):
  http://www.mega-nerd.com/SRC
")
fi

dnl GNOME2
use_gnome2=no
AC_ARG_WITH(gnome2,
    [  --with-gnome2           use GNOME2 (autodetect)],
         if test "x$withval" = "xyes"; then
            use_gnome2=yes
         fi,
         use_gnome2=yes)
if test "x$use_gnome2" = "xyes"; then
  PKG_CHECK_MODULES(GNOME2,libgnomeui-2.0 >= 2.0.0 libglade-2.0 gthread,
        AC_DEFINE(HAVE_GNOME2,,[use GNOME2])
        CFLAGS="$CFLAGS -DGNOME_DISABLE_DEPRECATED -DGTK_DISABLE_DEPRECATED -rdynamic $GNOME2_CFLAGS"
        LIBS="$LIBS -rdynamic $GNOME2_LIBS",
        use_gnome2=no)
fi

if test "x$use_gnome2" = "xno"; then
dnl GNOME1
dnl Check for gnome-config
    if test -z "$GNOME_CONFIG"; then
       AC_PATH_PROG(GNOME_CONFIG, gnome-config, no,
                                  [$PATH:/usr/local/bin:/opt/bin])
    fi
    if test "x$GNOME_CONFIG" != "xno"; then
       AC_MSG_CHECKING([whether $GNOME_CONFIG works])
       if ! "$GNOME_CONFIG" --version > /dev/null 2>&1; then
          GNOME_CONFIG=no
       else
          AC_MSG_RESULT(yes)
       fi
    fi
    if test "x$GNOME_CONFIG" = "xno"; then
       AC_MSG_ERROR([*** cannot find gnome-config, set GNOME_CONFIG environment variable to path of gnome-config script ***])
    fi
    AM_PATH_GTK(1.2.0,,AC_MSG_ERROR([*** GTK+ >= 1.2.0 not installed - please install first ***]))
    CFLAGS="$CFLAGS `$GNOME_CONFIG --cflags gnomeui libglade` `$GTK_CONFIG --cflags gtk gthread`"
    LIBS="$LIBS `$GNOME_CONFIG --libs gnomeui libglade` `$GTK_CONFIG --libs gtk gthread`"
fi
HAVE_GNOME2=$use_gnome2

dnl Optional features.
AC_ARG_ENABLE(optimization,
     [  --enable-optimization   enable compiler optimizations],
         if test "x$enableval" = "xyes"; then
            CFLAGS="$CFLAGS -O3 -fomit-frame-pointer"
         fi)
AC_ARG_ENABLE(cpudetect,
     [  --disable-cpudetect     do not try to autodetect CPU type],
         if test "x$enableval" = "xno"; then
            AC_DEFINE(DISABLE_CPUDETECT,,[do not try to autodetect CPU type])
         fi)
AC_ARG_ENABLE(fastmemcpy, 
     [  --disable-fastmemcpy    do not use MMX/3DNow!/SSE optimized memory copy code],
         if test "x$enableval" = "xno"; then
            AC_DEFINE(DISABLE_FASTMEMCPY,,[do not use MMX/3DNow!/SSE optimized memory copy code])
         fi)
AC_ARG_ENABLE(fastminmax,
     [  --disable-fastminmax    do not use MMX optimized min/max code],
         if test "x$enableval" = "xno"; then
            AC_DEFINE(DISABLE_FASTMINMAX,,[do not use MMX optimized min/max code])
         fi)
AC_ARG_ENABLE(static-cas,
     [  --enable-static-cas     statically compile CAS function],
         if test "x$enableval" = "xyes"; then
            AC_DEFINE(ENABLE_STATIC_CAS,,[statically compile CAS functions])
         fi)

dnl Miscellaneous compiler/linker flags.
LIBS="$LIBS"

GNUSOUNDDIR=`pwd`
AC_SUBST(GNUSOUNDDIR)
AC_SUBST(PIXMAPDIR)
AC_SUBST(GNOME_CONFIG)
AC_SUBST(HAVE_GNOME2)

dnl Dummy output to define $prefix and friends
AC_OUTPUT
dnl At this point $prefix is defined...

eval eval eval GNUSOUND_LIBDIR=${libdir}/$PACKAGE
AC_SUBST(GNUSOUND_LIBDIR)

if test "x$HAVE_GNOME2" = "xyes"; then
   GNOME_HELP_DIR=${datadir}/gnome/help/${PACKAGE}
   if test "x${prefix}" != "x/usr"; then
     messages="$messages
* GNUsound may not show up in the GNOME application menu since GNUsound
  will be installed in a different location from where the GNOME expects 
  it.

  To solve this issue, either configure GNUsound using 

    './configure --prefix=/usr'

  or create the following symbolic link after performing 'make install':

    'ln -s ${prefix}/share/gnome/apps/Multimedia/gnusound.desktop /usr/share/gnome/apps/Multimedia/gnusound.desktop'
"
  fi
else
   GNOME_HELP_DIR=${prefix}/share/gnome/help/${PACKAGE}
   if test "x${prefix}" != "x/usr"; then
     messages="$messages
* The user manual may not be available from the Help menu since documentation 
  will be installed in a different location from where the GNOME 1 help
  viewer expects it. For the same reason, GNUsound may show up in the 
  application menu.

  To overcome these issues, either configure GNUsound using 

    './configure --prefix=/usr'

  or create the following symbolic links after performing 'make install':

    'ln -s ${GNOME_HELP_DIR} /usr/share/gnome/help/${PACKAGE}'
    'ln -s ${prefix}/share/gnome/apps/Multimedia/gnusound.desktop /usr/share/gnome/apps/Multimedia/gnusound.desktop'
"
   
   fi
fi
AC_SUBST(GNOME_HELP_DIR)

AC_OUTPUT([
Makefile 
gnusound.desktop
doc/Makefile 
${module_path}/Makefile 
])

cat <<EOF

  Installing GNUsound in     : `eval eval echo $bindir`
  Installing data in         : `eval eval echo $libdir/$PACKAGE`
  Installing documentation in: `eval eval echo $GNOME_HELP_DIR`
  Runtime CPU detection      : ${enable_cpudetect:-yes}
  Optimized memcpy           : ${enable_fastmemcpy:-yes}
  Optimized min/max          : ${enable_fastminmax:-yes}
  GNOME2 support             : ${use_gnome2}
  Using libsamplerate        : ${use_samplerate}
  Audio drivers              : ${audio_drivers}
  File drivers               : ${file_drivers}
   $messages
If these settings are OK you can type 'make' to compile.

EOF
