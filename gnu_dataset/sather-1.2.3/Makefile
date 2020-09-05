#------------------------------->  Makefile  <--------------------------------#
#- Copyright (C) 1994 by International Computer Science Institute            -#
#- This file is part of the GNU Sather package. It is free software; you may -#
#- redistribute  and/or modify it under the terms of the  GNU General Public -#
#- License (GPL)  as  published  by the  Free  Software  Foundation;  either -#
#- version 3 of the license, or (at your option) any later version.          -#
#- This  program  is distributed  in the  hope that it will  be  useful, but -#
#- WITHOUT ANY WARRANTY without even the implied warranty of MERCHANTABILITY -#
#- or FITNESS FOR A PARTICULAR PURPOSE. See Doc/GPL for more details.        -#
#- The license text is also available from:  Free Software Foundation, Inc., -#
#- 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                     -#
#------------->  Please email comments to <bug-sather@gnu.org>  <-------------#

# Be sure to read the file Doc/Installation!
# You may need to set some variables in System/Common/CONFIG.proto
# to be appropriate to your system.
# Also make sure to adjust the paths in System/Platforms/X/Platform.module
# and System/Platforms/tcltk/Platform.module

export SATHER_HOME=$(shell pwd)

############################## VARIABLES TO SET ###############################

# Standard things
# CMP=cmp
RANLIB=ranlib
MV=mv
AR=ar
RM=rm -f
LN=ln -sf
CP=cp

CPP=/lib/cpp -C -P
CC=gcc
EXEC_SUFFIX=
# CC is only used for bootstrapping, check System/Common/CONFIG.proto if
# you want to change it for all Sather compilations

# Changes for Alpha
#CPP=/usr/ucb/cpp -C -P -DALPHA
#CC=gcc -DALPHA
#GC_CC=cc

# Changes for Win32
#CPP=gcc -E -C -P
#EXEC_SUFFIX=.exe

# The name of the produced binaries
SACOMP      =Bin/sacomp$(EXEC_SUFFIX)
SACOMP-BOOT =Boot/sacomp$(EXEC_SUFFIX)    # This has to be changed lateron
SABROWSE    =Bin/sabrowse$(EXEC_SUFFIX)

CS_OPT        =-verbose -O_fast -O_no_move_while -O_no_hoist_const -O_no_cse -only_reachable
CS_OPT_DEBUG  =-verbose -chk -C_flag -O -debug -debug_deterministic -debug_no_backtrace
CS_OPT_PROFILE=-verbose -only_reachable -optimize -C_flag -pg


# The list of platforms to install, in addition to "BOOT".
# "BOOT" intended to be the least common denominator required
# across all present and future platforms.  At the moment it
# uses the Boehm collector.
# This needs to edited to match the system. PLATFORMS is a comma
# separated list.
# If you intend to use threads, specify the matching parallel
# platform.

#PLATFORMS=linux,linux/lwp,linux_at,linux_at/smp
PLATFORMS=linux
#	  solaris,			\
#	  solaris_at/sparc_smp,		\
#	  solaris_at/sparc_myrinet,	\
#	  solaris/sparc_smp,		\
#	  solaris/sparc_myrinet,	\
#	  solaris/sparc_tcpip,		\
#	  solaris_at/i386_smp,		\
#	  hpux_at/hppa_smp,		\
#	  osf_at/axp_smp,		\
#	  unix,				\
#	  linux,			\
#	  linux/lwp,			\
#	  linux_at,			\
#	  linux_at/smp,			\
#	  iris-4,			\
#	  freebsd,			\
#	  win32,			\
#	  win32_at,			\
#	  win32_at/i386_smp,		\
#	  win32_at/i386_tcpip,		\


# The platform to use if not overridden by a -<platform> option
# or the SATHER_PLATFORM environment variable
# Should be edited for a particular system
DEFAULT_PLATFORM=linux

# The platform used to compile the boot compiler. Usually just default, but has to
# be one of: freebsd, hpux_at, iris-4, linux, osf_at, solaris, solaris_at, unix,
#            win32, win32_at
BOOT_PLATFORM=$(DEFAULT_PLATFORM)


################################ DEPENDENCIES #################################

# Make the boot compiler, compile the compiler with the boot compiler
# and install this new compiler. You need at least 60MB virtual memory
# for this. See Doc/Installation for details.
full: compiler


# Compile the boot compiler and use it as the compiler. This should
# work on most systems and does not require you to recompile the
# compiler itself. This is the default installation target.
small: bootcompiler
	${LN} ../$(SACOMP-BOOT) $(SACOMP)
	@echo; echo "...Boot compiler is now installation compiler." ; echo


# The compiler which ends up in Bin/ is the "fast" executable,
# which means that checking is turned off and optimizations are on.

# Some makes can't expand glob syntax with more than one *, so
# only specify one directory to expand instead of listing all of them.
# Watch out for this if you are modifying the compiler.

# This runs the C compiles after the Sather compiler instead of at
# the same time, to ease installation for those with small VM.

compiler: system bootcompiler
	@echo; echo "Creating C for installation compiler..." ; echo
	$(SACOMP-BOOT) $(CS_OPT) Compiler/sacomp.module -o $(SACOMP) -only_C
	@echo; echo "Compiling C for installation compiler..." ; echo
	$(MAKE) -C $(SACOMP).code

optional: bootcompiler
	@echo "*************************************************************"
	@echo "        Trying to compile the optional Browser and Gui"
	@echo "*************************************************************"
	@echo "This step compiles the browser and provides the"
	@echo "        -dualgui and -debug_graphical compiler options:"
	@echo "If this step fails, it may be restarted by calling"
	@echo "      make optional"
	@echo "Requirements: "
	@echo "     Tcl7.4 and Tk4.0 for the Browser and Gui"
	@echo "     Sockets for the -dualgui and the -debug_graphical option"
	@echo "See Doc/Installation.html for more details."
	@echo "For errors, check:"
	@echo "      System/Platforms/tcltk/Platform.module "
	@echo "      System/Platforms/X/Platform.module "
	@echo "Also check System/Platforms/tcltk/startup.tcl"
	@echo "*************************************************************"
	@echo
	$(MAKE) browser
	$(MAKE) dualgui

browser:
	@echo; echo "Creating C for Browser..." 
	$(SACOMP) $(CS_OPT) Browser/browser.module -main BROWSER -o $(SABROWSE) -only_C
	@echo; echo "Compiling C for Browser..."
	$(MAKE) -C $(SABROWSE).code

# The following step creates the server for the dual process gui.
# The standard gui/browser do not need this step. However, it is almost
# certain that any system problems affecting the dual gui server will
# also affect that the standard gui and browser.
dualgui:	
	@echo; echo Creating server end for two-process Gui...; 
	@echo "****************************************************"; 
	@echo "Installing the server for the Dual process Gui, -dualgui"
	@echo "Required for use of the"
	@echo "     -dualgui and -debug_graphical"
	@echo "compiler options"
	@echo "See the notes in Doc/Installation.html for more details."
	@echo "****************************************************";
	$(MAKE) -C System/Platforms/dualgui

debug: bootcompiler
	$(SACOMP-BOOT) $(CS_OPT_DEBUG) Compiler/sacomp.module -o $(SACOMP) -only_C
	$(MAKE) -C $(SACOMP).code

profile: 
	$(SACOMP-BOOT) $(CS_OPT_PROFILE) Compiler/sacomp.module -o $(SACOMP) -only_C
	$(MAKE) -C cd $(SACOMP).code

bootcompiler: system
	@echo; echo "Making boot compiler..." ; echo
	$(MAKE) -C System/Platforms/$(BOOT_PLATFORM) boot CC='$(CC)'


# The rule for System/Common/CONFIG also makes the serial libraries,
# as well as making any platforms.

system: common library platforms

common:
	@ CC="$(CC)" RANLIB="$(RANLIB)" CPP="$(CPP)"			\
	  MV="$(MV)" AR="$(AR)" RM="$(RM)" LN="$(LN)" CP="$(CP)"	\
	  export CC GC_CC RANLIB CPP MV AR RM LN CP;			\
	DEFAULT_PLATFORM='$(DEFAULT_PLATFORM)' PLATFORMS='$(PLATFORMS)' \
	$(MAKE) -C System/Common

library:
	@echo; echo -n "Making serial library: "
	@cd pLibrary; ./PP

platforms:
	@ CC="$(CC)" RANLIB="$(RANLIB)" CPP="$(CPP)"			\
	  MV="$(MV)" AR="$(AR)" RM="$(RM)" LN="$(LN)" CP="$(CP)"	\
	  export CC GC_CC RANLIB CPP MV AR RM LN CP;			\
	echo;								\
	for i in `echo "BOOT",$(PLATFORMS) | tr , \ `; do		\
	  echo "Making platform $$i..." ;				\
	  $(MAKE) -C System/Platforms/$$i ;				\
	done							

info:
	cd Emacs && makeinfo sather.texinfo
	cd Emacs && makeinfo sather-tutorial.texinfo
	cd Emacs && makeinfo sather-mode.texinfo


################################### TESTING ###################################

# There are two tests.  The first is the test classes in the library,
# which is pretty straightforward.  The second is a full 
# double compilation of the compiler, which can take quite a while.

test:
	$(MAKE) -C Test test

# The full test does the following:
#
#    1. Compile using Bin/sacomp, with full checking on.
#    2. Compile using result.

testfull:
	$(MAKE) -C Test test-full


################################### CLEANING ##################################

clean-unneeded:
	$(RM) -r $(SACOMP).code $(SABROWSE).code
	$(RM) $(SACOMP-BOOT) $(SACOMP-BOOT).code/*.o

clean: clean-unneeded
	$(MAKE) -C System/Common clean DEPEND=
	$(MAKE) -C System/pSather clean DEPEND=
	$(MAKE) -C Test clean DEPEND=
	 
	for i in System/Platforms/* ; do				\
	  if [ -f $$i/Makefile ] ; then $(MAKE) -C $$i clean ; fi	\
	done;
	
	$(RM) $(SACOMP) $(SABROWSE)
	
	$(RM) -r Doc/man/{whatis,cat1,windex}
	$(RM) Emacs/*.elc
	$(RM) Emacs/*.info Emacs/*.info??
	$(RM) Library/System/Socket/socket.sa
	$(RM) Library/Containers/stack.sa
	$(RM) Library/Containers/nr_stack.sa
	$(RM) Library/Containers/a_stack.sa
	$(RM) Library/Containers/nr_a_stack.sa
	$(RM) pLibrary/Containers/nr_stack.sa
	$(RM) pLibrary/Containers/nr_a_stack.sa
	
	debian/rules clean-safe
