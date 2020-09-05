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
 * @file    TestErrors.cpp
 * @author  A. Schwarz
 * @date    January 28, 2015
 *
 * @brief Test SLIP errors.
 *
 */

# include <cstdio>
# include <cstdio>
# include <ctime>
# include <fstream>
# include <exception>
# include <iostream>
# include <stdlib.h>
# include <unistd.h>

# include "../libgslip/Slip.h"
# include "TestGlobal.h"
# include "TestErrors.h"
# include "TestUtilities.h"

class MyException : public exception {
public:
}; // class MyException : public exception

void dump() {
   
   SlipDatum cells[] = { *new SlipDatum((bool)true)
                       , *new SlipDatum((CHAR)  'A')
                       , *new SlipDatum((UCHAR) 'B')
                       , *new SlipDatum((LONG)  0x1234)
                       , *new SlipDatum((ULONG) 0x2468)
                       , *new SlipDatum((DOUBLE) 1.234)
                       };
   for(unsigned i = 0; i < sizeof(cells)/sizeof(cells[0]); i++)
      logFile << cells[i].dump() << endl;
   SlipHeader* head = new SlipHeader();
   SlipSublist* sublist = new SlipSublist(*head);
   logFile << head->dump() << '\n' << sublist->dump() << endl;
}; // void dump()

bool testHeadErrors() {
   bool flag = true;
   try {
      SlipHeader* xhead = new SlipHeader();
      *xhead = (CHAR)'x';
   } catch(SlipException e) {
      logFile << "\n\n" << endl;
      SlipErr::Error base = e.getBase();
      logFile << "base Error[" << base.getErrorNumber() << "<-" << base.getIndex() << "] "
           << base.getMessage() << "in state " << base.getState() << endl;
      logFile << "Exception " << e.getMessage() << endl;
      logFile << "cell1 ";
      if (e.getCell1()) logFile << e.getCell1()->dump() << endl;
      else              logFile << "null" << endl;
      logFile << "cell2 ";
      if (e.getCell2()) logFile << e.getCell2()->dump() << endl;
      else              logFile << "null" << endl;
   }
   return flag;
}; // bool testHeadErrors()

/**
 * @fn testErrors()
 * @brief Driver to exercise SlipErrors.
 * <p>Driver for functions which will generate errors, catch the result
 *    and determine whether the correct error has been generated for the
 *    erroneous condition. The intent is to check all errors in the Slip
 *    system.</p>
 * <p>The routine is not ready for prime time. It is TBS.</p>
 * @return true all errors have been successfully detected
 */
bool testErrors() {
   high_resolution_clock::time_point start =  high_resolution_clock::now();
   bool flag = true;
   
   //dump();
   
   // flag &= testHeadErrors();
   
   high_resolution_clock::time_point end =  high_resolution_clock::now();
   duration<double> seconds = duration_cast<duration<double>>(end - start);
   testResultOutput("Test Errors", seconds.count(), flag);
   return flag;
   
}; // bool testErrors()
