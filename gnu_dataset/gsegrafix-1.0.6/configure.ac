################################################################################
#
# configure.ac file for GSEGrafix plotting program
# Processing configure.ac with autoconf produces a configure script
#
# To create a configure script and Makefiles run the following commands:
#    $ aclocal
#         (gathers all necessary macros from the macro library into the
#          aclocal.m4 file)
#    $ autoheader
#         (creates a config.h.in file, a template for the preprocessor macros
#          necessary for the gsesgrafix package)
#    $ automake --add-missing --gnu
#         (builds a Makefile.in template from each Makefile.am file)
#    $ autoconf
#         (creates the configure script)
#
# To configure, compile, and install the gsegrafix package run the following
# commands:
#    $ ./configure --prefix=dir
#         (dir is the desired installation prefix; the default is /usr/local;
#          applications installed by GNU/Linux distributions usually use /usr;
#          $HOME/usr can be used to keep the installation in the user's home
#          directory)
#    $ make -s
#         (the -s option suppresses printing of the commands being run)
#    $ su
#    $ Password:
#         (if not installing the program in the home directory)
#    $ make install
#         (installs the gsegrafix program files)
#
#   Most useful targets that the GNU Coding Standards specify (from GNU Automake
#   Manual, section 2.2.2 Standard 'Makefile' Targets)
#      make all:
#         Build programs, libraries, documentation, etc. (same as make).
#      make install:
#         Install what needs to be installed, copying the files from the
#         package’s tree to system-wide directories.
#      make install-strip:
#         Same as make install, then strip debugging symbols. Some users like to
#         trade space for useful bug reports. . .
#      make uninstall:
#         The opposite of make install: erase the installed files. (This needs
#         to be run from the same build tree that was installed.)
#      make clean:
#         Erase from the build tree the files built by make all.
#      make distclean:
#         Additionally erase anything ./configure created.
#      make check:
#         Run the test suite, if any.
#      make installcheck:
#         Check the installed programs or libraries, if supported.
#
################################################################################
#
# Copyright © 2008, 2009, 2010, 2011 Spencer A. Buckner
# http://savannah.gnu.org/projects/gsegrafix
#
# This file is part of GSEGrafix, a scientific and engineering plotting program.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
################################################################################

# Initialization
AC_PREREQ([2.63])
AC_INIT([gsegrafix], [1.0.6], [http://savannah.gnu.org/projects/gsegrafix])
AC_CONFIG_SRCDIR([src/EditMenu.c])
AC_CONFIG_HEADERS([config.h])
AM_INIT_AUTOMAKE

# Checks for programs
AC_PROG_CC
AC_PROG_INSTALL

# Checks for libraries

# Checks for header files
AC_CHECK_HEADERS([float.h string.h])

# Checks for typedefs, structures, and compiler characteristics
AC_TYPE_SIZE_T

# Checks for library functions
AC_FUNC_MALLOC
AC_CHECK_FUNCS([atexit floor memset pow snprintf sqrt strchr strrchr])

# Checks for GNOME
GNOME_MODULES="libgnomeui-2.0 >= 2.0.0"
PKG_CHECK_MODULES(GNOME, $GNOME_MODULES)

# Generate files
AC_CONFIG_FILES([Makefile src/Makefile])
AC_OUTPUT
