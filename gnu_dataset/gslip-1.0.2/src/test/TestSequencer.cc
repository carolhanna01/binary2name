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
 * @file    TestSequencer.cpp
 * @author  A. Schwarz
 * @date December 10, 2012
 *
 * @brief Test sequencer implementation file.
 *
 */

# include <iostream>
# include <sstream>

# include "TestGlobal.h"
# include "TestSequencer.h"
# include "TestUtilities.h"

# include "../libgslip/SlipDef.h"
# include "../libgslip/SlipCell.h"
# include "../libgslip/SlipDatum.h"
# include "../libgslip/SlipHeader.h"
# include "../libgslip/SlipSequencer.h"
# include "../libgslip/SlipSublist.h"

using namespace slip;
using namespace std;

struct TestResults;

bool testSequencerAdvanceLinear();
bool testSequencerAdvanceStructural();
bool testSequencerCore();
bool testSequencerMisc();
bool testSequencerCell(string name, SlipSequencer* sequencer, bool flag, TestResults test);

struct TestResults {
   SlipCell*   currentCell;
   bool        flag;
   TestResults(SlipCell* cell, bool flag)
              : currentCell(cell)
              , flag(flag) { }
};

bool testSequencerAdvanceLinear() {
   bool flag = true;
   SlipHeader* header1 = new SlipHeader();
   SlipHeader* header2 = new SlipHeader();
   SlipSequencer* sequencer  = new SlipSequencer(*header1);
   TestResults* testResults[] = { // List: (11 (21 23) 13)
   /* 0*/                         new TestResults( header1,                      true )
   /* 1*/                       , new TestResults( &header1->insLeft((ULONG)11), false)
   /* 2*/                       , new TestResults( &header1->insLeft(*header2),  false)
   /* 3*/                       , new TestResults( &header1->insLeft((ULONG)13), false)
   /* 4*/                       , new TestResults( header2,                      true )
   /* 5*/                       , new TestResults( &header2->insLeft((ULONG)21), false)
   /* 6*/                       , new TestResults( &header2->insLeft((ULONG)23), false)
                                };

   TestResults* testAdvanceLER[] = { testResults[1], testResults[3], testResults[0] };
   TestResults* testAdvanceLEL[] = { testResults[3], testResults[1], testResults[0] };
   TestResults* testAdvanceLWR[] = { testResults[1], testResults[2], testResults[3]
                                   , testResults[0] };
   TestResults* testAdvanceLWL[] = { testResults[3], testResults[2], testResults[1]
                                   , testResults[0]};
   TestResults* testAdvanceLNR[] = { testResults[2], testResults[0] };
   TestResults* testAdvanceLNL[] = { testResults[2], testResults[0] };

   const int testAdvanceLERSize = sizeof(testAdvanceLER)/sizeof(testAdvanceLER[0]);
   const int testAdvanceLELSize = sizeof(testAdvanceLEL)/sizeof(testAdvanceLEL[0]);
   const int testAdvanceLWRSize = sizeof(testAdvanceLWR)/sizeof(testAdvanceLWR[0]);
   const int testAdvanceLWLSize = sizeof(testAdvanceLWL)/sizeof(testAdvanceLWL[0]);
   const int testAdvanceLNRSize = sizeof(testAdvanceLNR)/sizeof(testAdvanceLNR[0]);
   const int testAdvanceLNLSize = sizeof(testAdvanceLNL)/sizeof(testAdvanceLNR[0]);

   /*************************************************************
    * Test reset()
    *************************************************************/
   if (sequencer->currentCell() != *header1) {
      stringstream pretty;
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "Test sequencer should point to the header");
      writeDiagnostics(__FILE__, "     currentCell  " + sequencer->currentCell().dump());
   }

   /*************************************************************
    * Detect: (11 () 13)
    *************************************************************/
   for(int i = 0; i < testAdvanceLERSize; i++)
      flag = (testSequencerCell("Test advanceLER", sequencer, sequencer->advanceLER().isHeader(), *testAdvanceLER[i]))? flag: false;

   for(int i = 0; i < testAdvanceLELSize; i++)
      flag = (testSequencerCell("Test advanceLEL", sequencer, sequencer->advanceLEL().isHeader(), *testAdvanceLEL[i]))? flag: false;

   for(int i = 0; i < testAdvanceLNRSize; i++)
      flag = (testSequencerCell("Test advanceLNR", sequencer, sequencer->advanceLNR().isHeader(), *testAdvanceLNR[i]))? flag: false;

   for(int i = 0; i < testAdvanceLNLSize; i++)
      flag = (testSequencerCell("Test advanceLNL", sequencer, sequencer->advanceLNL().isHeader(), *testAdvanceLNL[i]))? flag: false;

   for(int i = 0; i < testAdvanceLWRSize; i++)
      flag = (testSequencerCell("Test advanceLWR", sequencer, sequencer->advanceLWR().isHeader(), *testAdvanceLWR[i]))? flag: false;

   for(int i = 0; i < testAdvanceLWLSize; i++)
      flag = (testSequencerCell("Test advanceLWL", sequencer, sequencer->advanceLWL().isHeader(), *testAdvanceLWL[i]))? flag: false;

   delete sequencer;
   header2->deleteList();
   header1->deleteList();
   if (!flag)
      testResultOutput("Test Sequencer Advance Linear", 0.0, flag);
   return flag;
}; // bool testSequencerAdvanceLinear()

bool testSequencerAdvanceStructural() {
   bool flag = true;
   SlipHeader* header1 = new SlipHeader();
   SlipHeader* header2 = new SlipHeader();
   SlipHeader* header3 = new SlipHeader();
   TestResults* testResults[] = { // List: ( (22 () 24 25) 12 (22 () 24 25) 14)
   /* 0*/                         new TestResults( header1,                      true )
   /* 1*/                       , new TestResults( &header1->insLeft(*header2),  false)
   /* 2*/                       , new TestResults( &header1->insLeft((ULONG)12), false)
   /* 3*/                       , new TestResults( &header1->insLeft(*header2),  false)
   /* 4*/                       , new TestResults( &header1->insLeft((ULONG)14), false)
   /* 5*/                       , new TestResults( header2,                      true )
   /* 6*/                       , new TestResults( &header2->insLeft((ULONG)22), false)
   /* 7*/                       , new TestResults( &header2->insLeft(*header3),  false)
   /* 8*/                       , new TestResults( &header2->insLeft((ULONG)24), false)
   /* 9*/                       , new TestResults( &header2->insLeft((ULONG)25), false)
   /*10*/                       , new TestResults( header3,                      true )
                                };

   /*************************************************************
    * Test Results
    *************************************************************/
   TestResults* testAdvanceSER[] = { testResults[ 6], testResults[10]};
   TestResults* testAdvanceSEL[] = { testResults[ 4], testResults[ 9], testResults[ 8]
                                   , testResults[10]};
   TestResults* testAdvanceSWR[] = { testResults[ 1], testResults[ 6], testResults[ 7]
                                   , testResults[10]};
   TestResults* testAdvanceSWL[] = { testResults[ 4], testResults[ 3], testResults[ 9]
                                   , testResults[ 8], testResults[ 7], testResults[10]};
   TestResults* testAdvanceSNR[] = { testResults[ 1], testResults[ 7], testResults[10]};
   TestResults* testAdvanceSNL[] = { testResults[ 3], testResults[ 7], testResults[10]};

   /*************************************************************
    * Test sizes
    *************************************************************/
   const int testAdvanceSERSize = sizeof(testAdvanceSER)/sizeof(testAdvanceSER[0]);
   const int testAdvanceSELSize = sizeof(testAdvanceSEL)/sizeof(testAdvanceSEL[0]);
   const int testAdvanceSWRSize = sizeof(testAdvanceSWR)/sizeof(testAdvanceSWR[0]);
   const int testAdvanceSWLSize = sizeof(testAdvanceSWL)/sizeof(testAdvanceSWL[0]);
   const int testAdvanceSNRSize = sizeof(testAdvanceSNR)/sizeof(testAdvanceSNR[0]);
   const int testAdvanceSNLSize = sizeof(testAdvanceSNL)/sizeof(testAdvanceSNR[0]);

   SlipSequencer* sequencer = new SlipSequencer(*header1);

   if ((void*)&sequencer->currentCell() != (void*)header1) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "Test sequencer should point to the header");
      writeDiagnostics(__FILE__, "     currentCell  " + sequencer->currentCell().dump());
   }

   /*************************************************************
    * List: (11 (21 () 23) 13)
    *************************************************************/
   for(int i = 0; i < testAdvanceSERSize; i++) {
      flag = (testSequencerCell("Test advanceSER", sequencer, sequencer->advanceSER().isHeader(), *testAdvanceSER[i]))? flag: false;
   }

   sequencer->reset(*header1);
   for(int i = 0; i < testAdvanceSELSize; i++)
      flag = (testSequencerCell("Test advanceSEL", sequencer, sequencer->advanceSEL().isHeader(), *testAdvanceSEL[i]))? flag: false;

   sequencer->reset(*header1);
   for(int i = 0; i < testAdvanceSNRSize; i++)
      flag = (testSequencerCell("Test advanceSNR", sequencer, sequencer->advanceSNR().isHeader(), *testAdvanceSNR[i]))? flag: false;

   sequencer->reset(*header1);
   for(int i = 0; i < testAdvanceSNLSize; i++)
      flag = (testSequencerCell("Test advanceSNL", sequencer, sequencer->advanceSNL().isHeader(), *testAdvanceSNL[i]))? flag: false;

   sequencer->reset(*header1);
   for(int i = 0; i < testAdvanceSWRSize; i++)
      flag = (testSequencerCell("Test advanceSWR", sequencer, sequencer->advanceSWR().isHeader(), *testAdvanceSWR[i]))? flag: false;

   sequencer->reset(*header1);
   for(int i = 0; i < testAdvanceSWLSize; i++)
      flag = (testSequencerCell("Test advanceSWL", sequencer, sequencer->advanceSWL().isHeader(), *testAdvanceSWL[i]))? flag: false;

   header3->deleteList();
   header2->deleteList();
   header1->deleteList();
   if (!flag)
      testResultOutput("Test Sequencer Advance Structural", 0.0, flag);
   return flag;
}; // bool testSequencerAdvanceStructural()

bool testSequencerCell(string name, SlipSequencer* sequencer, bool flag, TestResults test) {
   stringstream pretty;
   bool retFlag = true;
   if (sequencer->currentCell() != *test.currentCell) {
      if (sequencer->currentCell().isPtr() && (*test.currentCell).isPtr())
         retFlag = (sequencer->currentCell().toString() == sequencer->currentCell().toString());
      if (!retFlag) {
         retFlag = false;
         writeDiagnostics(__FILE__, __LINE__, name + ": current cell is not correct");
         writeDiagnostics(__FILE__, "    current cell " + (sequencer->currentCell()).dump());
         writeDiagnostics(__FILE__, "    test cell    " + test.currentCell->dump());
      }
   }
   if (flag != test.flag) {
      stringstream pretty;
      retFlag = false;
      writeDiagnostics(__FILE__, __LINE__,  name + ": sequencer flag fails");
      pretty << "    sequencer flag  " << flag;
      writeDiagnostics(__FILE__, pretty.str());
      pretty << "    expected  flag  " << test.flag << endl;
      writeDiagnostics(__FILE__, pretty.str());
   }

   return retFlag;
}; // bool testSequencerCell(string name, SlipSequencer* sequencer, SlipHeader* header, USHORT depth)

bool testSequencerCore() {
   bool flag = true;
   SlipHeader* header = new SlipHeader();
   SlipSequencer* sequencer = new SlipSequencer(*header);
   if (sequencer->getClassType() != eHEADER) {
      stringstream pretty;
      flag = false;
      pretty << "sequencer Class Type is " << sequencer->getClassType() << " should be eHEADER";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }
   if (sequencer->getName() != "HEADER") {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "sequencer name is " + sequencer->getName() + " should be \"READER\"");
   }
   if (sequencer->isData() != false) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "sequencer isData() is \"true\" should be \"false\"");
   }
   if (sequencer->isHeader() != true) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "sequencer isHeader () is \"true\" should be \"false\"");
   }
   if (sequencer->isName() != false) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "sequencer isName() is \"true\" should be \"false\"");
   }
   if (sequencer->isNumber() != false) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "sequencer isNumber() is \"true\" should be \"false\"");
   }
   if (sequencer->isPtr() != false) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "sequencer isPtr() is \"true\" should be \"false\"");
   }
   if (sequencer->isSublist() != false) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "sequencer isSublist() is \"true\" should be \"false\"");
   }

   delete sequencer;
   header->deleteList();
   if (!flag)
      testResultOutput("Test Sequencer Core", 0.0, flag);
   return flag;
}; // bool testSequencerCore()

bool testSequencerInsert() {
   bool flag = true;
   SlipHeader* header1 = new SlipHeader();
   SlipHeader* header2 = new SlipHeader();
   SlipSequencer* sequencer  = new SlipSequencer(*header1);
   TestResults* testResults[] = { // List: ("datum" true 2.3 1 2 'a' 'B' PTR "string&" "String")
   /* 0*/                         new TestResults( header2,                                     true )
   /* 1*/                       , new TestResults( &header2->insRight(*new SlipDatum("datum")), false)
   /* 2*/                       , new TestResults( &header2->insRight(true),                    false)
   /* 3*/                       , new TestResults( &header2->insRight(2.3),                     false)
   /* 4*/                       , new TestResults( &header2->insRight((ULONG) 1),               false)
   /* 5*/                       , new TestResults( &header2->insRight((LONG)  2),               false)
   /* 6*/                       , new TestResults( &header2->insRight((UCHAR)'a'),              false)
   /* 7*/                       , new TestResults( &header2->insRight((CHAR) 'B'),              false)
   /* 8*/                       , new TestResults( &header2->insRight(new Pointer(7)),          false)
   /* 9*/                       , new TestResults( &header2->insRight("string&"),               false)
   /*10*/                       , new TestResults( &header2->insRight((string*)str),            false)
                               };
   TestResults* testInsert[] = { testResults[10], testResults[ 9], testResults[ 8]
                               , testResults[ 7], testResults[ 6], testResults[ 5]
                               , testResults[ 4], testResults[ 3], testResults[ 2]
                               , testResults[ 1], testResults[ 0]};
   TestResults* testMove[]   = { testResults[ 8], testResults[ 9], testResults[ 7]
                               , testResults[10], testResults[ 6], testResults[ 5]
                               , testResults[ 4], testResults[ 3], testResults[ 2]
                               , testResults[ 1], testResults[ 0]};

   const int testInsertSize  = sizeof(testInsert)/sizeof(testInsert[0]);

   /*************************************************************
    * Test InsertLeft
    *************************************************************/

   sequencer->reset(*header1);
   sequencer->insLeft((string*)str);
   sequencer->insLeft("string&");
   sequencer->insLeft(new Pointer(7));
   sequencer->insLeft((CHAR)  'B');
   sequencer->insLeft((UCHAR) 'a');
   sequencer->insLeft((LONG)  2);
   sequencer->insLeft((ULONG) 1);
   sequencer->insLeft(2.3);
   sequencer->insLeft(true);
   sequencer->insLeft(*new SlipDatum("datum"));

   sequencer->reset(*header1);
   for(int i = 0; i < testInsertSize - 1; i++)
      flag = (testSequencerCell("Test insertLeft", sequencer, sequencer->advanceLWR().isHeader(), *testInsert[i]))? flag: false;


   /*************************************************************
    * Test InsertRight
    *************************************************************/

   header1->flush();
   sequencer->reset(*header1);
   sequencer->insRight(*new SlipDatum("datum"));
   sequencer->insRight(true);
   sequencer->insRight(2.3);
   sequencer->insRight((ULONG) 1);
   sequencer->insRight((LONG)  2);
   sequencer->insRight((UCHAR) 'a');
   sequencer->insRight((CHAR)  'B');
   sequencer->insRight(new Pointer(7));
   sequencer->insRight("string&");
   sequencer->insRight((string*)str);

   sequencer->reset(*header1);
   for(int i = 0; i < testInsertSize - 1; i++)
      flag = (testSequencerCell("Test insertRight", sequencer, sequencer->advanceLWR().isHeader(), *testInsert[i]))? flag: false;

   /*************************************************************
    * Test Move Left/Right
    *************************************************************/

   header1->flush();
   sequencer->reset(*header1);
   sequencer->insRight(*new SlipDatum("datum"));
   sequencer->insRight(true);
   sequencer->insRight(2.3);
   sequencer->insRight((ULONG) 1);
   sequencer->insRight((LONG)  2);
   sequencer->insRight((UCHAR) 'a');
   sequencer->insRight((CHAR)  'B');
   sequencer->insRight(new Pointer(7));
   sequencer->insRight("string&");
   sequencer->insRight((string*)str);

   sequencer->reset(*header1);
   SlipCell& src1 = sequencer->advanceLWR().advanceLWR().advanceLWR().currentCell();
   sequencer->reset(*header1).advanceLWR().advanceLWR().moveLeft(src1);

   sequencer->reset(*header1);
   SlipCell& src2 = sequencer->advanceLWR().currentCell();
   sequencer->reset(*header1).advanceLWR().advanceLWR().advanceLWR().advanceLWR().moveRight(src2);

   sequencer->reset(*header1);
   for(int i = 0; i < testInsertSize - 1; i++)
      flag = (testSequencerCell("Test insertMove", sequencer, sequencer->advanceLWR().isHeader(), *testMove[i]))? flag: false;

   /*
   SlipSequencer&  moveLeft(SlipCell& X);                                       // Move the current cell to the left of X
   SlipSequencer&  moveRight(SlipCell& X);                                      // Move the current cell to the right of X
*/

   delete sequencer;
   header2->deleteList();
   header1->deleteList();
   if (!flag)
      testResultOutput("Test Sequencer Insert", 0.0, flag);
   return flag;
}; // bool testSequencerInsert()

bool testSequencerMisc() {
   bool flag = true;
   if (!flag)
      testResultOutput("Test Sequencer Miscellaneous", 0.0, flag);
   return flag;
}; // bool testSequencerMisc()


bool testSequencerOperators() {
   bool flag = true;
   SlipHeader*    header1    = new SlipHeader();
   SlipHeader*    header2    = new SlipHeader();
   SlipHeader*    header3    = new SlipHeader();
   SlipSequencer* sequencer1 = new SlipSequencer(*header1);
   SlipSequencer* sequencer2 = new SlipSequencer(*header2);
   SlipSequencer* sequencer3 = new SlipSequencer(*header3);

   header1->push("Too Little. Too Late");
   constructData();
   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
      header1->enqueue(*data[i]);
   }

   sequencer1->advanceLWR();
   *sequencer1 = (UCHAR) 'a';
   if ((SlipDatum&)sequencer1->currentCell() != (UCHAR) 'a') {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: (SlipDatum&)sequencer1->currentCell() != (UCHAR) 'a'");
      writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString());
   }
   if (*sequencer1 != (UCHAR) 'a') {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: *sequencer1 != (UCHAR) 'a'");
      writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString());
   }
   if (!(*sequencer1 == (UCHAR) 'a')) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: !(*sequencer1 == (UCHAR) 'a')");
      writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString());
   }

   *sequencer1 = (CHAR) 'B';
   if ((SlipDatum&)sequencer1->currentCell() != (CHAR) 'B') {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: (SlipDatum&)sequencer1->currentCell() != (CHAR) 'B'");
      writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString());
   }
   if (*sequencer1 != (CHAR) 'B') {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: *sequencer1 != (CHAR) 'B'");
      writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString());
   }
   if (!(*sequencer1 == (CHAR) 'B')) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: !(*sequencer1 == (CHAR) 'B'");
      writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString());
   }

   *sequencer1 = (ULONG) 15;
   if ((SlipDatum&)sequencer1->currentCell() != (ULONG) 15) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: (SlipDatum&)sequencer1->currentCell() != (ULONG) 15");
      writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString());
   }
   if (*sequencer1 != (ULONG) 15) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: *sequencer1 != (ULONG) 15");
      writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString());
   }
   if (!(*sequencer1 == (ULONG) 15)) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: !(*sequencer1 == (ULONG) 15)");
      writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString());
   }

   *sequencer1 = (LONG) 123;
   if ((SlipDatum&)sequencer1->currentCell() != (LONG) 123) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: (SlipDatum&)sequencer1->currentCell() != (LONG) 123");
      writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString());
   }
   if (*sequencer1 != (LONG) 123) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: *sequencer1 != (LONG) 123");
      writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString());
   }
   if (!(*sequencer1 == (LONG) 123)) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: !(*sequencer1 == (LONG) 123)");
      writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString());
   }

   *sequencer1 = (DOUBLE) 2.0;
   if ((SlipDatum&)sequencer1->currentCell() != (DOUBLE) 2.0) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: (SlipDatum&)sequencer1->currentCell() != (DOUBLE) 2.0");
      writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString());
   }
   if (*sequencer1 != (DOUBLE) 2.0) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: *sequencer1 != (DOUBLE) 2.0");
      writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString());
   }
   if (!(*sequencer1 == (DOUBLE) 2.0)) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: !(*sequencer1 == (DOUBLE) 2.0");
      writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString());
   }

   *sequencer1 = "A String";
   if ((SlipDatum&)sequencer1->currentCell() != "A String") {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: (SlipDatum&)sequencer1->currentCell() != \"A String\"");
      writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString());
   }
   if (*sequencer1 != "A String") {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: *sequencer1 != \"A String\"");
      writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString());
   }
   if (!(*sequencer1 == "A String")) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: !(*sequencer1 == \"A String\")");
      writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString());
   }

   *sequencer1 = str;
   if ((SlipDatum&)sequencer1->currentCell() != str) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: (SlipDatum&)sequencer1->currentCell() != str");
      writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString());
   }
   if (*sequencer1 != str) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: *sequencer1 != str");
      writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString());
   }
   if (!(*sequencer1 == str)) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: !(*sequencer1 == str)");
      writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString());
   }

   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
      sequencer1->advanceLWR();
      if ((SlipDatum&)sequencer1->currentCell() != *data[i]) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: (SlipDatum&)sequencer1->currentCell() != *data[i]");
         writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString());
      }
      if (*sequencer1 != *data[i]) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: *sequencer1 != *data[i]");
         writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString());
      }
      if (!(*sequencer1 == *data[i])) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: !(*sequencer1 == *data[i])");
         writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString());
      }
   }

   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
      header2->enqueue(*new SlipDatum(*data[i]));
   }

   sequencer1->reset(*header1);
   sequencer1->advanceLWR();
   sequencer2->reset(*header2);
   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
      sequencer1->advanceLWR();
      sequencer2->advanceLWR();
      if (*sequencer1 != *sequencer2) {
         if (sequencer1->isPtr() != sequencer2->isPtr()) {
            flag = false;
            writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: *sequencer1 != *sequencer2");
            writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString()
                                     + " != "  + sequencer2->currentCell().toString());
         }
      }
      if (!(*sequencer1 == *sequencer2)) {
         if (sequencer1->isPtr() != sequencer2->isPtr()) {
            flag = false;
            writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: !(*sequencer1 == *sequencer2)");
            writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString()
                                     + " != "  + sequencer2->currentCell().toString());
         }
      }
   }

   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
      header3->push(*new SlipDatum(*data[i]));
   }

   sequencer1->reset(*header1);
   sequencer1->advanceLWR();
   sequencer3->reset(*header3);
   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
      sequencer1->advanceLWR();
      sequencer3->advanceLWR();
      *sequencer1 = *sequencer3;
      if (*sequencer1 != *sequencer3) {
         if (sequencer1->isPtr() != sequencer3->isPtr()) {
            flag = false;
            writeDiagnostics(__FILE__, __LINE__, "   Comparison of assignment Failed: *sequencer1 != *sequencer3");
            writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString()
                                     + " != "  + sequencer3->currentCell().toString());
         }
      }
      if (!(*sequencer1 == *sequencer3)) {
         if (sequencer1->isPtr() != sequencer3->isPtr()) {
            flag = false;
            writeDiagnostics(__FILE__, __LINE__, "   Comparison of assignment Failed: !(*sequencer1 == *sequencer3)");
            writeDiagnostics(__FILE__, "     " + sequencer1->currentCell().toString()
                                     + " != " + sequencer3->currentCell().toString());
         }
      }
   }

   delete sequencer1;
   delete sequencer2;
   delete sequencer3;
   header1->deleteList();
   header2->deleteList();
   header3->deleteList();
   if (!flag)
      testResultOutput("Test Sequencer Operators", 0.0, flag);
   return flag;

}; // bool testSequencerOperators()

bool testSequencerReplace() {
   bool flag = true;
   SlipHeader* header1 = new SlipHeader();
   SlipHeader* header2 = new SlipHeader();
   SlipSequencer* sequencer  = new SlipSequencer(*header1);
   TestResults* testResults[] = { // List: ("datum" true 2.3 1 2 'a' 'B' PTR "string&" "String")
   /* 0*/                         new TestResults( header2,                                     true )
   /* 1*/                       , new TestResults( &header2->insRight(*new SlipDatum("datum")), false)
   /* 2*/                       , new TestResults( &header2->insRight(true),                    false)
   /* 3*/                       , new TestResults( &header2->insRight(2.3),                     false)
   /* 4*/                       , new TestResults( &header2->insRight((ULONG) 1),               false)
   /* 5*/                       , new TestResults( &header2->insRight((LONG)  2),               false)
   /* 6*/                       , new TestResults( &header2->insRight((UCHAR)'a'),              false)
   /* 7*/                       , new TestResults( &header2->insRight((CHAR) 'B'),              false)
   /* 8*/                       , new TestResults( &header2->insRight(new Pointer(7)),          false)
   /* 9*/                       , new TestResults( &header2->insRight("string&"),               false)
   /*10*/                       , new TestResults( &header2->insRight((string*)str),            false)
                               };
   TestResults* testReplace[] = { testResults[10], testResults[ 9], testResults[ 8]
                               , testResults[ 7], testResults[ 6], testResults[ 5]
                               , testResults[ 4], testResults[ 3], testResults[ 2]
                               , testResults[ 1], testResults[ 0]};

   const int testReplaceSize = sizeof(testReplace)/sizeof(testReplace[0]);

   /*************************************************************
    * Test Replace
    *************************************************************/

   sequencer->reset(*header1);

   sequencer->insLeft((ULONG) 1);
   sequencer->insLeft((ULONG) 2);
   sequencer->insLeft((ULONG) 3);
   sequencer->insLeft((ULONG) 4);
   sequencer->insLeft((ULONG) 5);
   sequencer->insLeft((ULONG) 6);
   sequencer->insLeft((ULONG) 7);
   sequencer->insLeft((ULONG) 8);
   sequencer->insLeft((ULONG) 9);
   sequencer->insLeft((ULONG)10);

   sequencer->reset(*header1);

   sequencer->advanceLWR().replace((string*)str);
   sequencer->advanceLWR().replace("string&");
   sequencer->advanceLWR().replace(new Pointer(7));
   sequencer->advanceLWR().replace((CHAR)  'B');
   sequencer->advanceLWR().replace((UCHAR) 'a');
   sequencer->advanceLWR().replace((LONG)  2);
   sequencer->advanceLWR().replace((ULONG) 1);
   sequencer->advanceLWR().replace(2.3);
   sequencer->advanceLWR().replace(true);
   sequencer->advanceLWR().replace(*new SlipDatum("datum"));

   sequencer->reset(*header1);
   for(int i = 0; i < testReplaceSize - 1; i++)
      flag = (testSequencerCell("Test insertLeft", sequencer, sequencer->advanceLWR().isHeader(), *testReplace[i]))? flag: false;

   delete sequencer;
   header2->deleteList();
   header1->deleteList();
   if (!flag)
      testResultOutput("Test Sequencer Replace", 0.0, flag);
   return flag;
}; // bool testSequencerReplace()

bool testSequencer() {
   high_resolution_clock::time_point start =  high_resolution_clock::now();
   bool flag = true;
   flag = (testSequencerCore())?              flag: false;
   flag = (testSequencerAdvanceLinear())?     flag: false;
   flag = (testSequencerAdvanceStructural())? flag: false;
   flag = (testSequencerMisc())?              flag: false;
   flag = (testSequencerOperators())?         flag: false;
   flag = (testSequencerInsert())?            flag: false;
   flag = (testSequencerReplace())?           flag: false;
   
   high_resolution_clock::time_point end =  high_resolution_clock::now();
   duration<double> seconds = duration_cast<duration<double>>(end - start);
   testResultOutput("Test Sequencer", seconds.count(), flag);
   return flag;
}; // bool testSequencer()
