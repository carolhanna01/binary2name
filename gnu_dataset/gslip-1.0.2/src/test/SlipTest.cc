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

/*
 * File:   SlipTest.cpp
 * Author: Arthur Schwarz
 *
 * Created on July 15, 2009, 11:45 AM
 */

# include <stdlib.h>
# include <iostream>
# include <fstream>

# include "TestErrors.h"
# include "TestGlobal.h"
# include "TestHeader.h"
# include "TestIO.h"
# include "TestMisc.h"
# include "TestOperators.h"
# include "TestReader.h"
# include "TestReplace.h"
# include "TestSequencer.h"
# include "TestUtilities.h"
# include "../libgslip/Slip.h"

using namespace slip;
using namespace std;

int totalTests = 0;    //!< mumber of test executed

void openTestFiles() {
   const string  logFileName("test.log");
   logFile.open(logFileName, ofstream::out | ofstream::trunc);
}; // void openTestFiles()

void closeTestFiles() {
   logFile.close();
}; // void closeTestFile()

/**
 * @fn testPreamble()
 * @brief setup for testing and output to TAP.
 * <p>Open files and output the TAP test plan.</p>
 */
void testPreamble() {
   void openTestFiles();
   cout << "1.." << totalTests + 1 << endl;     // TAP test plan
} // void testPreable())

int main(int argc, char** argv) {
   typedef bool (*func)();
   const func tests[] = { testAllocation
                        , testDatum
                        , testEnqueue
                        , testPush
                        , testPop
                        , testDequeue
                        , testHeaderMisc
                        , testGetSpace
                        , testIntrinsics
                        , testSplitLeft
                        , testSplitRight
                        , testMoveLeft
                        , testMoveRight
                        , testReplace
                        , testDList
                        , testReader
                        , testSequencer
                        , testOperators
                        , testIO
                        , testErrors
   };
   totalTests = sizeof(tests)/sizeof(tests[0]);
   
   high_resolution_clock::time_point start =  high_resolution_clock::now();
   bool flag = true;
   slipInit(INITIALALLOCATION, DELTAALLOCATION);
   testPreamble();
   sysInfo(logFile);
   
   for (int i = 0; i < totalTests; i++) 
      flag = (tests[i]())?          flag: false;
//*DEBUG*/SlipCellBase::avslHistory(true);
   
   high_resolution_clock::time_point end =  high_resolution_clock::now();
   duration<double> seconds = duration_cast<duration<double>>(end - start);
   testResultOutput("Testing Complete", seconds.count(), flag, true);
   cout << flush;
   closeTestFiles();
   return !flag;                  // 'false' conforms to automake test harness 'truth'
}

