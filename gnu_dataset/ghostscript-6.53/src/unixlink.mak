#    Copyright (C) 1990, 1995, 1996, 1997, 1998, 1999 Aladdin Enterprises.  All rights reserved.
# 
# This file is part of GNU Ghostscript.
# 
# GNU Ghostscript is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
# to anyone for the consequences of using it or for whether it serves any
# particular purpose or works at all, unless he says so in writing.  Refer
# to the GNU General Public License for full details.
# 
# Everyone is granted permission to copy, modify and redistribute GNU
# Ghostscript, but only under the conditions described in the GNU General
# Public License.  A copy of this license is supposed to have been given
# to you along with GNU Ghostscript so you can know your rights and
# responsibilities.  It should be in a file named COPYING.  Among other
# things, the copyright notice and this notice must be preserved on all
# copies.

# $RCSfile: unixlink.mak,v $ $Revision: 1.2.2.2 $
# Partial makefile common to all Unix configurations.
# This part of the makefile contains the linking steps.

# Define the name of this makefile.
UNIXLINK_MAK=$(GLSRC)unixlink.mak

# The following prevents GNU make from constructing argument lists that
# include all environment variables, which can easily be longer than
# brain-damaged system V allows.

.NOEXPORT:

# ----------------------------- Main program ------------------------------ #

### Library files and archive

LIB_ARCHIVE_ALL=$(LIB_ALL) $(DEVS_ALL)\
 $(GLOBJ)gsnogc.$(OBJ) $(GLOBJ)gconfig.$(OBJ) $(GLOBJ)gscdefs.$(OBJ)

# Build an archive for the library only.
# This is not used in a standard build.
GSLIB_A=$(GS)lib.a
$(GSLIB_A): $(LIB_ARCHIVE_ALL)
	rm -f $(GSLIB_A)
	$(AR) $(ARFLAGS) $(GSLIB_A) $(LIB_ARCHIVE_ALL)
	$(RANLIB) $(GSLIB_A)

### Interpreter main program

INT_ARCHIVE_ALL=$(PSOBJ)imainarg.$(OBJ) $(PSOBJ)imain.$(OBJ) $(INT_ALL) $(DEVS_ALL)\
 $(GLOBJ)gconfig.$(OBJ) $(GLOBJ)gscdefs.$(OBJ)
XE_ALL=$(PSOBJ)gs.$(OBJ) $(INT_ARCHIVE_ALL)

# Build a library archive for the entire interpreter.
# This is not used in a standard build.
GS_A=$(GS).a
$(GS_A): $(INT_ARCHIVE_ALL)
	rm -f $(GS_A)
	$(AR) $(ARFLAGS) $(GS_A) $(INT_ARCHIVE_ALL)
	$(RANLIB) $(GS_A)

# Here is the final link step.  The stuff with LD_RUN_PATH is for SVR4
# systems with dynamic library loading; I believe it's harmless elsewhere.
# The resetting of the environment variables to empty strings is for SCO Unix,
# which has limited environment space.
ldt_tr=$(PSOBJ)ldt.tr
$(GS_XE): $(ld_tr) $(ECHOGS_XE) $(XE_ALL)
	$(ECHOGS_XE) -w $(ldt_tr) -n - $(CCLD) $(LDFLAGS) -o $(GS_XE)
	$(ECHOGS_XE) -a $(ldt_tr) -n -s $(PSOBJ)gs.$(OBJ) -s
	cat $(ld_tr) >>$(ldt_tr)
	$(ECHOGS_XE) -a $(ldt_tr) -s - $(EXTRALIBS) $(STDLIBS)
	if [ x$(XLIBDIR) != x ]; then LD_RUN_PATH=$(XLIBDIR); export LD_RUN_PATH; fi; \
	XCFLAGS= XINCLUDE= XLDFLAGS= XLIBDIRS= XLIBS= \
	FEATURE_DEVS= DEVICE_DEVS= DEVICE_DEVS1= DEVICE_DEVS2= DEVICE_DEVS3= \
	DEVICE_DEVS4= DEVICE_DEVS5= DEVICE_DEVS6= DEVICE_DEVS7= DEVICE_DEVS8= \
	DEVICE_DEVS9= DEVICE_DEVS10= DEVICE_DEVS11= DEVICE_DEVS12= \
	DEVICE_DEVS13= DEVICE_DEVS14= DEVICE_DEVS15= DEVICE_DEVS16= \
	DEVICE_DEVS17= DEVICE_DEVS18= DEVICE_DEVS19= DEVICE_DEVS20= \
	$(SH) <$(ldt_tr)
