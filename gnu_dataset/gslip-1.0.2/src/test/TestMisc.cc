// Copyright (C) 2014 Arthur I. Schwarz
//
// This file is part of the C++ SLIP library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/**
 * @file    TestMisc.cpp
 * @author  A. Schwarz
 * @date November 19, 2012
 *
 * @brief Miscellaneous test implementation file.
 *
 */

# include <iomanip>
# include <iostream>
# include <sstream>

# include "TestMisc.h"
# include "TestGlobal.h"
# include "TestUtilities.h"

# include "../libgslip/SlipCell.h"
# include "../libgslip/SlipCellBase.h"
# include "../libgslip/SlipDatum.h"
# include "../libgslip/SlipHeader.h"
# include "../libgslip/SlipReader.h"
# include "../libgslip/SlipSequencer.h"
# include "../libgslip/SlipSublist.h"

using namespace slip;
using namespace std;

bool testAllocation() {
   high_resolution_clock::time_point start =  high_resolution_clock::now();
   bool flag = true;
   SlipState state = SlipCellBase::getSlipState();
   ULONG avail = state.avail;

   SlipDatum*     datum     = new SlipDatum((LONG)0x15);
   SlipHeader*    header    = new SlipHeader();
   SlipSublist*   sublist   = new SlipSublist(*header);
   SlipReader*    reader    = new SlipReader(*header);
   SlipSequencer* sequencer = new SlipSequencer(*header);

   flag = (checkSlipState("testAllocation: Slip Cell allocation", INITIALALLOCATION, avail - 4))? flag: false;

   delete datum;
   header->deleteList();
   delete sublist;
   delete reader;
   delete sequencer;

   flag = (checkSlipState("testAllocation: Slip Cell deletion", INITIALALLOCATION, avail - 1))? flag: false;
   
   high_resolution_clock::time_point end =  high_resolution_clock::now();
   duration<double> seconds = duration_cast<duration<double>>(end - start);
   testResultOutput("Test Allocation", seconds.count(), flag);
   return flag;
}; // bool testAllocation()

bool testGetSpace() {
   high_resolution_clock::time_point start =  high_resolution_clock::now();
   SlipState state        = SlipCellBase::getSlipState();
   SlipHeader* header     = new SlipHeader();
   short     total        = state.total + INITIALALLOCATION;
   short     avail        = state.avail + INITIALALLOCATION - (INITIALALLOCATION + 2 + 1);
   bool      flag;

   for (int i = 0; i < INITIALALLOCATION + 2; i++) {
      header->enqueue((ULONG)i);
   }
   flag = checkSlipState("Test Get Space", total, avail);

   header->deleteList();
   
   high_resolution_clock::time_point end =  high_resolution_clock::now();
   duration<double> seconds = duration_cast<duration<double>>(end - start);
   testResultOutput("Test Get Space", seconds.count(), flag);
   return flag;
}; // void testGetSpace

bool testIntrinsics() {
   high_resolution_clock::time_point start =  high_resolution_clock::now();
   static string title = "Test Intrinsics";
   bool flag = true;
   SlipHeader* header   = new SlipHeader();
   TestCase data[]      = { {new SlipHeader()               , Intrinsics(title, eHEADER,  2, 4,  "HEADER",  false, true,  false, false)}
                          , {new SlipSublist(*header)       , Intrinsics(title, eSUBLIST, 1, 9,  "SUBLIST", false, false, false, true)}
                          , {new SlipReaderCell(*header)    , Intrinsics(title, eREADER,  4, 7,  "READER",  false, false, false, false)}
                          , {new SlipDatum(true)            , Intrinsics(title, eBOOL,    5, 1,  "bool",    true,  false, true,  false)}
                          , {new SlipDatum((UCHAR)'a')      , Intrinsics(title, eUCHAR,   5, 10, "UCHAR",   true,  false, true,  false)}
                          , {new SlipDatum((CHAR)'B')       , Intrinsics(title, eCHAR,    5, 2,  "CHAR",    true,  false, true,  false)}
                          , {new SlipDatum((ULONG)123)      , Intrinsics(title, eULONG,   5, 11, "ULONG",   true,  false, true,  false)}
                          , {new SlipDatum((LONG)345)       , Intrinsics(title, eLONG,    5, 5,  "LONG",    true,  false, true,  false)}
                          , {new SlipDatum(678.123)         , Intrinsics(title, eDOUBLE,  5, 3,  "DOUBLE",  true,  false, true,  false) }
                          , {new SlipDatum(new Pointer())   , Intrinsics(title, ePTR,     6, 6,  "PTR",     true,  false, false, false)}
                          , {new SlipDatum((string)"String"), Intrinsics(title, eSTRING,  6, 8,  "STRING",  true,  false, false, false)}
                          };
   for (unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++)
      flag = (data[i].test.compare(data[i].X))? flag: false;
   for (unsigned int i = 1; i < sizeof(data)/sizeof(data[0]); i++) delete data[i].X;
   header->deleteList();
   ((SlipHeader*)data[0].X)->deleteList();
   
   high_resolution_clock::time_point end =  high_resolution_clock::now();
   duration<double> seconds = duration_cast<duration<double>>(end - start);
   testResultOutput("Test Intrinsics", seconds.count(), flag);
   return flag;
}; // bool testIntrinsics()

bool testDatum() {
   high_resolution_clock::time_point start =  high_resolution_clock::now();
   SlipState state        = SlipCellBase::getSlipState();
   bool flag              = constructData();
   bool passFailFlag;
   for( int i = 0; i < DATASIZE; i++) {
      string actual   = data[i]->toString();
      string expected = resultStr[i];
      passFailFlag = (actual == expected)? true: false;
      flag   = (passFailFlag)? flag: false;
      if (!passFailFlag) {
         std::stringstream msg; 
              msg << "actual \"" << actual << "\" != \"" << expected << "\"";
      }
   }
   deleteData();
   flag = (checkSlipState("Test Datum Delete", state.total, state.avail))? flag: false;
   
   high_resolution_clock::time_point end =  high_resolution_clock::now();
   duration<double> seconds = duration_cast<duration<double>>(end - start);
   testResultOutput("Test Datum", seconds.count(), flag);
   return flag;
}; // void testDatum()
