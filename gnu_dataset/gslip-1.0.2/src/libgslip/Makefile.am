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


# Header and Source file for SLIP
publicheaders=Slip.h SlipAsciiEntry.h SlipBinaryEntry.h SlipCell.h SlipCellBase.h SlipDatum.h SlipDef.h SlipDescription.h SlipErr.h SlipException.h SlipGlobal.h SlipHeader.h SlipOp.h SlipPointer.h SlipPtrOp.h SlipRead.h SlipReader.h SlipReaderCell.h SlipRegister.h SlipSequencer.h SlipSublist.h SlipWrite.h 

privateHeaders=lex.Slip.h location.hh position.hh Slip.Bison.hpp SlipBoolOp.h SlipCharOp.h SlipDoubleOp.h SlipHash.h SlipHashEntry.h  SlipHeaderOp.h SlipInputMethods.h SlipLongOp.h SlipReaderOp.h SlipStringConst.h SlipStringNonConst.h SlipStringOp.h SlipSublistOp.h SlipUndefinedOp.h SlipUnsignedCharOp.h SlipUnsignedLongOp.h stack.hh

sources=lex.Slip.cc Slip.Bison.cc Slip.cc SlipAsciiEntry.cc SlipBinaryEntry.cc SlipBoolOp.cc SlipCell.cc SlipCellBase.cc SlipCharOp.cc SlipDatum.cc SlipDescription.cc SlipDoubleOp.cc SlipErr.cc SlipException.cc SlipGlobal.cc SlipHash.cc SlipHashEntry.cc SlipHeader.cc SlipHeaderOp.cc SlipInputMethods.cc SlipLongOp.cc SlipOp.cc SlipPointer.cc SlipPtrOp.cc SlipRead.cc SlipReader.cc SlipReaderCell.cc SlipReaderOp.cc SlipRegister.cc SlipSequencer.cc SlipStringConst.cc SlipStringNonConst.cc SlipStringOp.cc SlipSublist.cc SlipSublistOp.cc SlipUndefinedOp.cc SlipUnsignedCharOp.cc SlipUnsignedLongOp.cc SlipWrite.cc 


# Header files for testing SLIP
testHead=TestErrors.h TestGlobal.h TestHeader.h TestIO.h TestMisc.h TestOperators.h TestReader.h TestReplace.h TestSequencer.h TestUtilities.h 

# Compiler flags
AM_CXXFLAGS=-std=gnu++11 -Wall -Wno-reorder -Wno-unused-value -Wno-address -Wmaybe-uninitialized -g  -MMD -MP -MF

# Creating Slip Library

lib_LIBRARIES      = libgslip.a
libgslip_a_SOURCES = $(sources) $(privateHeaders)
pkginclude_HEADERS = $(publicheaders)

