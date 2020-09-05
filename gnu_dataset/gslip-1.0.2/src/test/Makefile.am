# Copyright (C) 2014 Arthur I. Schwarz
#
# This file is part of the C++ SLIP library.  This library is free
# software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option)
# any later version.

# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# Under Section 7 of GPL version 3, you are granted additional
# permissions described in the GCC Runtime Library Exception, version
# 3.1, as published by the Free Software Foundation.

# You should have received a copy of the GNU General Public License and
# a copy of the GCC Runtime Library Exception along with this program;
# see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
# <http:#www.gnu.org/licenses/>.

## Process this file with automake to produce Makefile.in


# Distribute the tap-driver into the source directory.
EXTRA_DIST = tap-driver.sh


# Header files for testing SLIP
testHead=TestErrors.h TestGlobal.h TestHeader.h TestIO.h TestMisc.h TestOperators.h TestReader.h TestReplace.h TestSequencer.h TestUtilities.h 

# Source files for testing SLIP
testCPP=SlipTest.cc TestErrors.cc TestGlobal.cc TestHeader.cc TestIO.cc TestMisc.cc  TestOperators.cc TestReader.cc TestReplace.cc TestSequencer.cc  TestUtilities.cc

# Compiler flags
AM_CXXFLAGS=-std=gnu++11 -Wall -Wno-reorder -Wno-unused-value -Wno-address -Wmaybe-uninitialized -g  -MMD -MP -MF


# Allow subdirectories to be used in references
# select testing format
AUTOMAKE_OPTIONS = parallel-tests subdir-objects

# Testing SLIP
TEST_EXTENSIONS     = $(EXEEXT)

#  if ($(EXEEXT) == ".exe")
EXE_LOG_DRIVER      = env AM_TAP_AWK='$(AWK)' $(SHELL) $(srcdir)/tap-driver.sh

#  if ($(EXEEXT) == "")
AM_EXE_LOG_DRIVER_FLAGS = --no-merge
LOG_DRIVER          = env AM_TAP_AWK='$(AWK)' $(SHELL) $(srcdir)/tap-driver.sh
AM_LOG_DRIVER_FLAGS = --no-merge

check_PROGRAMS      = SlipTest$(EXEEXT)
SlipTest_INCLUDE    = -I$(top_srcdir)/src/libgslip
SlipTest_SOURCES    = $(testCPP) $(testHead)
LDADD               = $(top_srcdir)/src/libgslip/libgslip.a
TESTS               = SlipTest
VERBOSE             = yes


CLEANFILES         = $(test_SOURCES) $(test_INCLUDE) $(EXTRA_DIST)

