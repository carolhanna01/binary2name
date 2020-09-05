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
 * @file    TestReader.cpp
 * @author  A. Schwarz
 * @date November 22, 2012
 *
 * @brief Test  reader implemenation file.
 *
 */

# include <iostream>
# include <sstream>

# include "TestGlobal.h"
# include "TestReader.h"
# include "TestUtilities.h"

# include "../libgslip/SlipDef.h"
# include "../libgslip/SlipCell.h"
# include "../libgslip/SlipDatum.h"
# include "../libgslip/SlipHeader.h"
# include "../libgslip/SlipReader.h"
# include "../libgslip/SlipSublist.h"
# include "../libgslip/SlipUnsignedLongOp.h"

using namespace slip;
using namespace std;

struct TestResults {
   SlipHeader* currentList;
   SlipCell*   currentCell;
   USHORT      depth;
   bool        flag;
   TestResults(SlipHeader* header, SlipCell* cell, USHORT depth, bool flag)
              : currentList(header)
              , currentCell(cell)
              , depth(depth)
              , flag(flag) { }
};

bool testCurrentCell(SlipReader& reader, Intrinsics& test);
bool testReaderAdvanceLinear();
bool testReaderAdvanceStructural();
bool testReaderCore();
bool testReaderMoveSplit();
bool testReaderMisc();
bool testReaderOperators();
bool testReaderCell(string name, SlipReader* reader, bool flag, TestResults test);

bool testCurrentCell(SlipReader& reader, Intrinsics& test) {
   bool flag = true;
   static const string sClassType[eSENTINEL] = { "UNDEFINED", "BOOL", "CHAR",  "DOUBLE",  "HEADER"
                                               , "LONG",      "PTR",  "READER", "STRING", "SUBLIST"
                                               , "UCHAR",     "ULONG"
                                               };
   stringstream message;
   if (reader.getName() != test.name) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "  " + test.title + "getName() actual " + reader.getName() + " expected " + test.name);
   }
   if (reader.isData() != test.isDataFlag) {
      flag = false;
      message << "  " << test.title << "isData() actual " << reader.isData() << " expected " << test.isDataFlag;
      writeDiagnostics(__FILE__, __LINE__, message.str());
   }
   if (reader.isHeader() != test.isHeaderFlag) {
      flag = false;
      message << "  " << test.title << "isHeader() actual " << reader.isHeader() << " expected " << test.isSublistFlag;
      writeDiagnostics(__FILE__, __LINE__, message.str());
   }
   if (reader.isNumber() != test.isNumberFlag) {
      flag = false;
      message << "  " << test.title << "isNumber() actual " << reader.isNumber() << " expected " << test.isNumberFlag;
      writeDiagnostics(__FILE__, __LINE__, message.str());
   }
   if (reader.isSublist() != test.isSublistFlag) {
      flag = false;
      message << "  " << test.title << "isSublist() actual " << reader.isSublist() << " expected " << test.isSublistFlag;
      writeDiagnostics(__FILE__, __LINE__, message.str());
   }
   return flag;
}; // bool testCurrentCell(SlipReader& reader, Intrinsics* test)
bool testReaderEnqueueDequeue() {
   bool flag = true;
   SlipHeader* header = new SlipHeader();
   SlipReader* reader = new SlipReader(*header);

   constructData();
   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++)
      reader->insLeft(*data[i]);

   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
      flag = (reader->advanceLWR() == (SlipDatum&)*data[i])? flag: false;
      if (!flag) {
         writeDiagnostics(__FILE__, __LINE__, "  insLeft " + reader->currentCell().toString() + " != " + data[i]->toString());
      }
   }

   if (!(reader->advanceLWR()).isHeader()) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "    push   reader.currentCell() is not a header: " + reader->toString());
   }

   reader->reset();

   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]) - 1; i++) {
      SlipCell* cell = &reader->dequeue();
      flag = (*cell == *data[DATASIZE - i - 1])? flag: false;
      if (!flag) {
         writeDiagnostics(__FILE__, __LINE__, "    pop    " + cell->toString() + " != " + data[DATASIZE - i - 1]->toString());
      }
      delete cell;
   }

   reader->pop();

   delete reader;
   header->deleteList();
   deleteData();
   if (!flag)
      testResultOutput("Test Reader Enqueue/Dequeue", 0.0, flag);
   return flag;
}; // bool testReaderEnqueueDequeue()

bool testReaderAdvanceLinear() {
   bool flag = true;
   SlipHeader* header1 = new SlipHeader();
   SlipHeader* header2 = new SlipHeader();
   SlipReader* reader  = new SlipReader(*header1);
   TestResults* testResults[] = { // List: (11 (21 23) 13)
   /* 0*/                         new TestResults(header1, header1,                      0, false)
   /* 1*/                       , new TestResults(header1, &header1->insLeft((ULONG)11), 0, true )
   /* 2*/                       , new TestResults(header1, &header1->insLeft(*header2),  0, true )
   /* 3*/                       , new TestResults(header1, &header1->insLeft((ULONG)13), 0, true )
   /* 4*/                       , new TestResults(header2, header2,                      1, false)
   /* 5*/                       , new TestResults(header2, &header2->insLeft((ULONG)21), 1, true )
   /* 6*/                       , new TestResults(header2, &header2->insLeft((ULONG)23), 1, true )
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
   reader->reset();

   if (&reader->currentCell() != &reader->currentList()) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "Test reset reader should point to the header");
      writeDiagnostics(__FILE__, "     currentCell  " + reader->currentCell().dump());
      writeDiagnostics(__FILE__, "     current List " + reader->currentList().dump());
   }

   /*************************************************************
    * Detect: (11 () 13)
    *************************************************************/
   reader->reset();
   for(int i = 0; i < testAdvanceLERSize; i++)
      flag = (testReaderCell("Test advanceLER", reader, reader->advanceLER().isHeader(), *testAdvanceLER[i]))? flag: false;

   reader->reset();
   for(int i = 0; i < testAdvanceLELSize; i++)
      flag = (testReaderCell("Test advanceLEL", reader, reader->advanceLEL().isHeader(), *testAdvanceLEL[i]))? flag: false;

   reader->reset();
   for(int i = 0; i < testAdvanceLNRSize; i++)
      flag = (testReaderCell("Test advanceLNR", reader, reader->advanceLNR().isHeader(), *testAdvanceLNR[i]))? flag: false;

   reader->reset();
   for(int i = 0; i < testAdvanceLNLSize; i++)
      flag = (testReaderCell("Test advanceLNL", reader, reader->advanceLNL().isHeader(), *testAdvanceLNL[i]))? flag: false;

   reader->reset();
   for(int i = 0; i < testAdvanceLWRSize; i++)
      flag = (testReaderCell("Test advanceLWR", reader, reader->advanceLWR().isHeader(), *testAdvanceLWR[i]))? flag: false;

   reader->reset();
   for(int i = 0; i < testAdvanceLWLSize; i++)
      flag = (testReaderCell("Test advanceLWL", reader, reader->advanceLWL().isHeader(), *testAdvanceLWL[i]))? flag: false;

   delete reader;
   header2->deleteList();
   header1->deleteList();
   if (!flag)
      testResultOutput("Test Reader Advance Linear", 0.0, flag);
   return flag;
}; // bool testReaderAdvanceLinear()

bool testReaderAdvanceStructural() {
   bool flag = true;
   SlipHeader* header1 = new SlipHeader();
   SlipHeader* header2 = new SlipHeader();
   SlipHeader* header3 = new SlipHeader();
   TestResults* testResults[] = { // List: ( ( () 22 () 24 25 () ) 12 ( () 22 () 24 25 () ) 14 ( () 22 () 24 25 () ) )
   /* 0*/                         new TestResults(header1, header1,                      0, false)
   /* 1*/                       , new TestResults(header1, &header1->insLeft(*header2),  0, true )
   /* 2*/                       , new TestResults(header1, &header1->insLeft((ULONG)12), 0, true )
   /* 3*/                       , new TestResults(header1, &header1->insLeft(*header2),  0, true )
   /* 4*/                       , new TestResults(header1, &header1->insLeft((ULONG)14), 0, true )
   /* 5*/                       , new TestResults(header1, &header1->insLeft(*header2),  0, true )
   /* 6*/                       , new TestResults(header2, header2,                      1, false)
   /* 7*/                       , new TestResults(header2, &header2->insLeft(*header3),  1, true )
   /* 8*/                       , new TestResults(header2, &header2->insLeft((ULONG)22), 1, true )
   /* 9*/                       , new TestResults(header2, &header2->insLeft(*header3),  1, true )
   /*10*/                       , new TestResults(header2, &header2->insLeft((ULONG)24), 1, true )
   /*11*/                       , new TestResults(header2, &header2->insLeft((ULONG)25), 1, true )
   /*12*/                       , new TestResults(header2, &header2->insLeft(*header3),  1, true )
   /*13*/                       , new TestResults(header3, header3,                      2, false)
                                };

   /*************************************************************
    * Test Results
    *************************************************************/
   TestResults* testAdvanceSER[] = { testResults[ 8], testResults[10], testResults[11]
                                   , testResults[ 2], testResults[ 8], testResults[10]
                                   , testResults[11], testResults[ 4], testResults[ 8]
                                   , testResults[10], testResults[11], testResults[ 0]};
   TestResults* testAdvanceSEL[] = { testResults[11], testResults[10], testResults[ 8]
                                   , testResults[ 4], testResults[11], testResults[10]
                                   , testResults[ 8], testResults[ 2], testResults[11]
                                   , testResults[10], testResults[ 8], testResults[ 0]};
   TestResults* testAdvanceSWR[] = { testResults[ 1], testResults[ 7], testResults[ 8]
                                   , testResults[ 9], testResults[10], testResults[11]
                                   , testResults[12], testResults[ 2], testResults[ 3]
                                   , testResults[ 7], testResults[ 8], testResults[ 9]
                                   , testResults[10], testResults[11], testResults[12]
                                   , testResults[ 4], testResults[ 5], testResults[ 7]
                                   , testResults[ 8], testResults[ 9], testResults[10]
                                   , testResults[11], testResults[12], testResults[ 0]};
   TestResults* testAdvanceSWL[] = { testResults[ 5], testResults[12], testResults[11]
                                   , testResults[10], testResults[ 9], testResults[ 8]
                                   , testResults[ 7], testResults[ 4], testResults[ 3]
                                   , testResults[12], testResults[11], testResults[10]
                                   , testResults[ 9], testResults[ 8], testResults[ 7]
                                   , testResults[ 2], testResults[ 1], testResults[12]
                                   , testResults[11], testResults[10], testResults[ 9]
                                   , testResults[ 8], testResults[ 7], testResults[ 0]};
   TestResults* testAdvanceSNR[] = { testResults[ 1], testResults[ 7], testResults[ 9]
                                   , testResults[12], testResults[ 3], testResults[ 7]
                                   , testResults[ 9], testResults[12], testResults[ 5]
                                   , testResults[ 7], testResults[ 9], testResults[12]
                                   , testResults[ 0]};
   TestResults* testAdvanceSNL[] = { testResults[ 5], testResults[12], testResults[ 9]
                                   , testResults[ 7], testResults[ 3], testResults[12]
                                   , testResults[ 9], testResults[ 7], testResults[ 1]
                                   , testResults[12], testResults[ 9], testResults[ 7]
                                   , testResults[ 0]};

   /*************************************************************
    * Test sizes
    *************************************************************/
   const int testAdvanceSERSize = sizeof(testAdvanceSER)/sizeof(testAdvanceSER[0]);
   const int testAdvanceSELSize = sizeof(testAdvanceSEL)/sizeof(testAdvanceSEL[0]);
   const int testAdvanceSWRSize = sizeof(testAdvanceSWR)/sizeof(testAdvanceSWR[0]);
   const int testAdvanceSWLSize = sizeof(testAdvanceSWL)/sizeof(testAdvanceSWL[0]);
   const int testAdvanceSNRSize = sizeof(testAdvanceSNR)/sizeof(testAdvanceSNR[0]);
   const int testAdvanceSNLSize = sizeof(testAdvanceSNL)/sizeof(testAdvanceSNR[0]);

   SlipReader* reader = new SlipReader(*header1);

   reader->resetTop();
   if (&reader->currentCell() != &reader->currentList()) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "Test resetTop reader should point to the header");
      writeDiagnostics(__FILE__, "     currentCell  " + reader->currentCell().dump());
      writeDiagnostics(__FILE__, "     current List " + reader->currentList().dump());
   }

   /*************************************************************
    * List: (11 (21 () 23) 13)
    *************************************************************/
   reader->resetTop();
   for(int i = 0; i < testAdvanceSERSize; i++) {
      flag = (testReaderCell("Test advanceSER", reader, reader->advanceSER().isHeader(), *testAdvanceSER[i]))? flag: false;
   }

   reader->resetTop();
   for(int i = 0; i < testAdvanceSELSize; i++) {
      flag = (testReaderCell("Test advanceSEL", reader, reader->advanceSEL().isHeader(), *testAdvanceSEL[i]))? flag: false;
   }

   reader->resetTop();
   for(int i = 0; i < testAdvanceSNRSize; i++)
      flag = (testReaderCell("Test advanceSNR", reader, reader->advanceSNR().isHeader(), *testAdvanceSNR[i]))? flag: false;

   reader->resetTop();
   for(int i = 0; i < testAdvanceSNLSize; i++)
      flag = (testReaderCell("Test advanceSNL", reader, reader->advanceSNL().isHeader(), *testAdvanceSNL[i]))? flag: false;

   reader->resetTop();
   for(int i = 0; i < testAdvanceSWRSize; i++)
      flag = (testReaderCell("Test advanceSWR", reader, reader->advanceSWR().isHeader(), *testAdvanceSWR[i]))? flag: false;

   reader->resetTop();
   for(int i = 0; i < testAdvanceSWLSize; i++)
      flag = (testReaderCell("Test advanceSWL", reader, reader->advanceSWL().isHeader(), *testAdvanceSWL[i]))? flag: false;

   header3->deleteList();
   header2->deleteList();
   header1->deleteList();
   delete reader;
   if (!flag)
      testResultOutput("Test Reader Advance Structural", 0.0, flag);
   return flag;
}; // bool testReaderAdvanceStructural()

bool testReaderCell(string name, SlipReader* reader, bool flag, TestResults test) {
   stringstream pretty;
   bool retFlag = true;
   if (reader->currentCell() != *test.currentCell) {
      retFlag = false;
      writeDiagnostics(__FILE__, __LINE__, name + ": current cell is not correct");
      writeDiagnostics(__FILE__, reader->dump());
      writeDiagnostics(__FILE__, "    current cell " + (reader->currentCell()).dump());
      writeDiagnostics(__FILE__, "    test cell    " + test.currentCell->dump());
   }
   if (reader->currentList() != *test.currentList) {
      retFlag = false;
      writeDiagnostics(__FILE__, __LINE__, name + ": current list is not header");
      writeDiagnostics(__FILE__, reader->dump());
      writeDiagnostics(__FILE__, "    current list " + (reader->currentList()).dump());
      writeDiagnostics(__FILE__, "    header       " + test.currentList->dump());
   }
   if (reader->listDepth() != test.depth) {
      retFlag = false;
      pretty << name << ": current depth is "
           << reader->listDepth()
           << " should be " << test.depth<< endl;
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
      writeDiagnostics(__FILE__, __LINE__, "           " + reader->dump());
   }
   return retFlag;
}; // bool testReaderCell(string name, SlipReader* reader, SlipHeader* header, USHORT depth)

/**
 * @brief Test SlipReader core capabilities.
 * @return <b>true</b> test succeeded
 */
bool testReaderCore() {
   static string title = "Test Reader Core";
   bool flag = true;
   SlipHeader* header = new SlipHeader();
   SlipReader* reader = new SlipReader(*header);                                // create and initialize reader
   TestCase data[]      = { {new SlipHeader()               , Intrinsics(title, eHEADER,  2, 4,  "HEADER",  false, true,  false, false)}
                          , {new SlipSublist(*header)       , Intrinsics(title, eSUBLIST, 1, 9,  "SUBLIST", false, false, false, true)}
                          , {new SlipDatum(true)            , Intrinsics(title, eBOOL,    5, 1,  "bool",    true,  false, true,  false)}
                          , {new SlipDatum((UCHAR)'a')      , Intrinsics(title, eUCHAR,   5, 10, "UCHAR",   true,  false, true,  false)}
                          , {new SlipDatum((CHAR)'B')       , Intrinsics(title, eCHAR,    5, 2,  "CHAR",    true,  false, true,  false)}
                          , {new SlipDatum((ULONG)123)      , Intrinsics(title, eULONG,   5, 11, "ULONG",   true,  false, true,  false)}
                          , {new SlipDatum((LONG)345)       , Intrinsics(title, eLONG,    5, 5,  "LONG",    true,  false, true,  false)}
                          , {new SlipDatum(678.123)         , Intrinsics(title, eDOUBLE,  5, 3,  "DOUBLE",  true,  false, true,  false)}
                          , {new SlipDatum(new Pointer())   , Intrinsics(title, ePTR,     6, 6,  "PTR",     true,  false, false, false)}
                          , {new SlipDatum((string)"String"), Intrinsics(title, eSTRING,  6, 8,  "STRING",  true,  false, false, false)}
                          };

   // create list
   header->insLeft(*header);
   header->insLeft(true);
   header->insLeft((UCHAR) 'a');
   header->insLeft((CHAR)  'B');
   header->insLeft((ULONG) 123);
   header->insLeft((LONG)  345);
   header->insLeft(678.123);
   header->insLeft(new Pointer());
   header->insLeft((string)"String");

   for (unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
      flag = (data[i].test.compare(&(reader->currentCell())))? flag: false;
      flag = (testCurrentCell(*reader, data[i].test))? flag: false;
      reader->advanceLWR();
   }

   for (unsigned int i = 1; i < sizeof(data)/sizeof(data[0]); i++) delete data[i].X;
   ((SlipHeader*)data[0].X)->deleteList();

   TestResults results= TestResults(header, header, 0, true);
   flag = (testReaderCell("New Reader Cell", reader, true, results))?   flag: false;
   reader->reset();
   flag = (testReaderCell("Reset Reader Cell", reader, true, results))? flag: false;
   reader->resetTop();
   flag = (testReaderCell("ResetTop Cell", reader, true, results))?     flag: false;
   reader->upLevel();
   flag = (testReaderCell("upLevel", reader, true, results))?           flag: false;

   delete reader;
   header->deleteList();
   if (!flag)
      testResultOutput("Test Reader Core", 0.0, flag);
   return flag;
}; // bool testReaderCore()

bool testReaderMisc() {
   stringstream pretty;
   bool flag = true;
   SlipHeader* header = new SlipHeader();
   SlipReader* reader = new SlipReader(*header);
   reader->push((ULONG)1);
   if (!reader->currentCell().isHeader()) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "  currentCell() should be a header after reader creation.");
      writeDiagnostics(__FILE__, "  " + reader->currentCell().dump());
   }
   reader->advanceLWL();
   if (reader->currentCell().isHeader()) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "  current cell should not be a header after advanceLEL().");
      writeDiagnostics(__FILE__, "  " + reader->currentCell().dump());
   }
   if (!reader->isData()) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "  current cell should be data after advanceLEL().");
      writeDiagnostics(__FILE__, "  " + reader->currentCell().dump());
   }
   if (!reader->isNumber()) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "  current cell should be a number after advanceLEL().");
      writeDiagnostics(__FILE__, "  " + reader->currentCell().dump());
   }
   if (reader->isSublist() || reader->isName()) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "  current cell should not be a sublist after advanceLEL().");
      writeDiagnostics(__FILE__, "  " + reader->currentCell().dump());
   }
   if (reader->getName() != ((SlipUnsignedLongOp*)ulongOP)->getName()) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "  current cell getName() fails. it is "
                                           + reader->getName() +  " it should be "
                                           + ((SlipUnsignedLongOp*)ulongOP)->getName());
   }
   if (reader->getClassType() != ((SlipUnsignedLongOp*)ulongOP)->getClassType()) {
      flag = false;
      pretty << "  current cell getClassType() fails. it is "
              << reader->getClassType() << " it should be "
              << ((SlipUnsignedLongOp*)ulongOP)->getClassType();
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }
   reader->deleteCell();
   if (!reader->currentCell().isHeader()) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__,  " current cell should be a header after the current cell is deleted");
      writeDiagnostics(__FILE__, " " + reader->currentCell().dump());
   }
   if (!reader->currentList().isEmpty()) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__,  " current list should be empty after the current cell is deleted");
      writeDiagnostics(__FILE__, " " + reader->currentCell().dump());
   }
   reader->push((ULONG)2);
   reader->flush();
   if (!reader->currentList().isEmpty()) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, " current list should be empty the list is deleted");
      writeDiagnostics(__FILE__, " " + reader->currentCell().dump());
   }

   delete reader;
   header->deleteList();
   if (!flag)
      testResultOutput("Test Reader Miscellaneous", 0.0, flag);
   return flag;
}; // bool testReaderMisc()

bool testReaderMoveSplit() {
   bool flag = true;
   SlipHeader* header1 = new SlipHeader();
   SlipHeader* header2 = new SlipHeader();
   SlipHeader* header3 = new SlipHeader();
   SlipReader* reader1 = new SlipReader(*header2);
   SlipReader* reader2 = new SlipReader(*header3);
   TestResults* testResults[] = { // List: (1 2 3 4 5 6 7 8 9 0)
   /*  0*/                        new TestResults(header1, header1,                      0, false)
   /*  1*/                      , new TestResults(header1, &header1->insLeft((LONG)1 ),  0, true )
   /*  2*/                      , new TestResults(header1, &header1->insLeft((LONG)2 ),  0, true )
   /*  3*/                      , new TestResults(header1, &header1->insLeft((LONG)3 ),  0, true )
   /*  4*/                      , new TestResults(header1, &header1->insLeft((LONG)4 ),  0, true )
   /*  5*/                      , new TestResults(header1, &header1->insLeft((LONG)5 ),  0, true )
   /*  6*/                      , new TestResults(header1, &header1->insLeft((LONG)6 ),  0, true )
   /*  7*/                      , new TestResults(header1, &header1->insLeft((LONG)7 ),  0, true )
   /*  8*/                      , new TestResults(header1, &header1->insLeft((LONG)8 ),  0, true )
   /*  9*/                      , new TestResults(header1, &header1->insLeft((LONG)9 ),  0, true )
   /* 10*/                      , new TestResults(header1, &header1->insLeft((LONG)10),  0, true )
                                };

   /*************************************************************
    *                       Test Results                        *
    *************************************************************/
   TestResults* testMoveListLeft[] =  { testResults[ 1], testResults[ 2], testResults[ 1]
                                      , testResults[ 2], testResults[ 3], testResults[ 4]
                                      , testResults[ 5], testResults[ 6], testResults[ 7]
                                      , testResults[ 8], testResults[ 9], testResults[10]
                                      , testResults[ 3]};
   TestResults* testMoveListRight[] = { testResults[ 1], testResults[ 2], testResults[ 3]
                                      , testResults[ 1], testResults[ 2], testResults[ 3]
                                      , testResults[ 4], testResults[ 5], testResults[ 6]
                                      , testResults[ 7], testResults[ 8], testResults[ 9]
                                      , testResults[10]};
   TestResults* testSplitLeftDst[]  = { testResults[ 1], testResults[ 2], testResults[ 3]
                                      , testResults[ 4]};
   TestResults* testSplitLeftSrc[]  = { testResults[ 5], testResults[ 6], testResults[ 7]
                                      , testResults[ 8], testResults[ 9], testResults[10]};
   TestResults* testSplitRightDst[] = { testResults[ 4], testResults[ 5], testResults[ 6]
                                      , testResults[ 7], testResults[ 8], testResults[ 9]
                                      , testResults[10]};
    TestResults* testSplitRightSrc[] = { testResults[ 1], testResults[ 2], testResults[ 3]};

   const int testMoveListLeftSize  = sizeof(testMoveListLeft)/sizeof(testMoveListLeft[0]);
   const int testMoveListRightSize = sizeof(testMoveListRight)/sizeof(testMoveListLeft[0]);
   const int testSplitLeftDstSize  = sizeof(testSplitLeftDst)/sizeof(testSplitLeftDst[0]);
   const int testSplitLeftSrcSize  = sizeof(testSplitLeftSrc)/sizeof(testSplitLeftSrc[0]);
   const int testSplitRightDstSize = sizeof(testSplitRightDst)/sizeof(testSplitRightDst[0]);
   const int testSplitRightSrcSize = sizeof(testSplitRightSrc)/sizeof(testSplitRightSrc[0]);

   /*************************************************************
    *                          Tests                            *
    *************************************************************/

      /*******************************************************
       *                    MoveListLeft                     *
       *******************************************************/
   for(long i = 0; i < 10; i++)
      header2->insLeft((LONG) (i + 1));

   header3->insLeft((LONG) 1);
   header3->insLeft((LONG) 2);
   header3->insLeft((LONG) 3);

   reader2->advanceLWR().advanceLWR().advanceLWR();
   reader1->moveListLeft(reader2->currentCell());
   reader2->reset();
   if (!reader1->isEmpty()) {
      flag = false;
       writeDiagnostics(__FILE__, __LINE__,  "Test Move List Left: Move From list is not empty");
       reader1->write(logFile);
   }

   for(int i = 0;i < testMoveListLeftSize; i++) {
      reader2->advanceLWR();
      if (reader2->currentCell() != *(testMoveListLeft[i]->currentCell)) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__,  "Test Move List Left: current cell is not correct");
         writeDiagnostics(__FILE__, reader2->dump());
         writeDiagnostics(__FILE__,  "    current cell " + (reader2->currentCell()).dump());
         writeDiagnostics(__FILE__, "    test cell    " + testMoveListLeft[i]->currentCell->dump());
      }
   }

      /*******************************************************
       *                   MoveListRight                     *
       *******************************************************/
   reader1->flush();
   reader2->flush();
   for(long i = 0; i < 10; i++)
      header2->insLeft((LONG) (i + 1));

   header3->insLeft((LONG) 1);
   header3->insLeft((LONG) 2);
   header3->insLeft((LONG) 3);

   reader2->advanceLWR().advanceLWR().advanceLWR();
   reader1->moveListRight(reader2->currentCell());
   reader2->reset();
   if (!reader1->isEmpty()) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "Test Move List Right: Move From list is not empty");
      reader1->write(logFile);
   }

   for(int i = 0; i < testMoveListRightSize; i++) {
      reader2->advanceLWR();
      if (reader2->currentCell() != *(testMoveListRight[i]->currentCell)) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, "Test Move List Right: current cell is not correct");
         writeDiagnostics(__FILE__, reader2->dump());
         writeDiagnostics(__FILE__, "    current cell " + (reader2->currentCell()).dump());
         writeDiagnostics(__FILE__, "    test cell    " + testMoveListLeft[i]->currentCell->dump());
      }
   }

      /*******************************************************
       *                      SplitLeft                      *
       *******************************************************/
   reader1->flush();
   reader2->flush();
   for(long i = 0; i < 10; i++)
      header2->insLeft((LONG) (i + 1));

   reader1->advanceLWR().advanceLWR().advanceLWR().advanceLWR();
   SlipHeader& header4 = reader1->splitLeft();

   reader1->reset();
   if (reader1->isEmpty()) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "Test Split Left Source: list is empty");
      reader1->write(logFile);
   }

   for(int i = 0; i < testSplitLeftSrcSize; i++) {
      reader1->advanceLWR();
      if (reader1->currentCell() != *(testSplitLeftSrc[i]->currentCell)) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, "Test Split Left Source: current cell is not correct");
         writeDiagnostics(__FILE__, reader1->dump());
         writeDiagnostics(__FILE__, "    current cell " + (reader1->currentCell()).dump());
         writeDiagnostics(__FILE__, "    test cell    " + testSplitLeftSrc[i]->currentCell->dump());
      }
   }

   *reader1 = header4;
   if (reader1->isEmpty()) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__,"Test Split Left Destination: list is empty" );
      reader1->write();
   }

   for(int i = 0; i < testSplitLeftDstSize; i++) {
      reader1->advanceLWR();
      if (reader1->currentCell() != *(testSplitLeftDst[i]->currentCell)) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, "Test Split Left Destination: current cell is not correct");
         writeDiagnostics(__FILE__, reader1->dump());
         writeDiagnostics(__FILE__, "    current cell " + (reader1->currentCell()).dump());
         writeDiagnostics(__FILE__, "    test cell    " + testSplitLeftDst[i]->currentCell->dump());
      }
   }

   header4.deleteList();

      /*******************************************************
       *                      SplitRight                     *
       *******************************************************/
   *reader1 = *header2;
   reader2->flush();
   for(long i = 0; i < 10; i++)
      header2->insLeft((LONG) (i + 1));

   reader1->advanceLWR().advanceLWR().advanceLWR().advanceLWR();
   SlipHeader& header5 = reader1->splitRight();

   reader1->reset();
   if (reader1->isEmpty()) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "Test Split Right Source: list is empty");
      reader1->write();
   }

   for(int i = 0; i < testSplitRightSrcSize; i++) {
      reader1->advanceLWR();
      if (reader1->currentCell() != *(testSplitRightSrc[i]->currentCell)) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, "Test Split Right Source: current cell is not correct");
         writeDiagnostics(__FILE__, reader1->dump());
         writeDiagnostics(__FILE__, "    current cell " + (reader1->currentCell()).dump());
         writeDiagnostics(__FILE__, "    test cell    " + testSplitRightSrc[i]->currentCell->dump());
      }
   }

   *reader1 = header5;
   if (reader1->isEmpty()) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "Test Split Right Destination: list is empty");
      reader1->write();
   }

   for(int i = 0; i < testSplitRightDstSize; i++) {
      reader1->advanceLWR();
      if (reader1->currentCell() != *(testSplitRightDst[i]->currentCell)) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, "Test Split Right Destination: current cell is not correct");
         writeDiagnostics(__FILE__, reader1->dump());
         writeDiagnostics(__FILE__, "    current cell " + (reader1->currentCell()).dump());
         writeDiagnostics(__FILE__, "    test cell    " + testSplitRightDst[i]->currentCell->dump());
      }
   }

   header5.deleteList();

   delete reader2;
   delete reader1;
   header3->deleteList();
   header2->deleteList();
   header1->deleteList();

   if (!flag)
      testResultOutput("Test Reader Move/Split", 0.0, flag);
   return flag;
}; // bool testReaderMoveSplit)()

bool testReaderOperators() {
   bool flag = true;
   SlipHeader* header1 = new SlipHeader();
   SlipHeader* header2 = new SlipHeader();
   SlipHeader* header3 = new SlipHeader();
   SlipReader* reader1 = new SlipReader(*header1);
   SlipReader* reader2 = new SlipReader(*header2);
   SlipReader* reader3 = new SlipReader(*header3);

   header1->push("Too Little. Too Late");
   constructData();
   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
      reader1->insLeft(*data[i]);
   }

   reader1->advanceLWR();
   *reader1 = (UCHAR) 'a';
   if ((SlipDatum&)reader1->currentCell() != (UCHAR) 'a') {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: (SlipDatum&)reader1->currentCell() != (UCHAR) 'a'");
      writeDiagnostics(__FILE__, "     " + reader1->currentCell().toString());
   }
   if (*reader1 != (UCHAR) 'a') {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: *reader1 != (UCHAR) 'a'");
      writeDiagnostics(__FILE__, "     " + reader1->currentCell().toString());
   }
   if (!(*reader1 == (UCHAR) 'a')) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: !(*reader1 == (UCHAR) 'a')");
      writeDiagnostics(__FILE__, "     " + reader1->currentCell().toString());
   }

   *reader1 = (CHAR) 'B';
   if ((SlipDatum&)reader1->currentCell() != (CHAR) 'B') {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: (SlipDatum&)reader1->currentCell() != (CHAR) 'B'");
      writeDiagnostics(__FILE__, "     " + reader1->currentCell().toString());
   }
   if (*reader1 != (CHAR) 'B') {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__,  "   Comparison Failed: *reader1 != (CHAR) 'B'");
      writeDiagnostics(__FILE__, "     " + reader1->currentCell().toString());
   }
   if (!(*reader1 == (CHAR) 'B')) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: !(*reader1 == (CHAR) 'B'");
      writeDiagnostics(__FILE__, "     " + reader1->currentCell().toString());
   }

   *reader1 = (ULONG) 15;
   if ((SlipDatum&)reader1->currentCell() != (ULONG) 15) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: (SlipDatum&)reader1->currentCell() != (ULONG) 15");
      writeDiagnostics(__FILE__, "     " + reader1->currentCell().toString());
   }
   if (*reader1 != (ULONG) 15) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: *reader1 != (ULONG) 15");
      writeDiagnostics(__FILE__, "     " + reader1->currentCell().toString());
   }
   if (!(*reader1 == (ULONG) 15)) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: !(*reader1 == (ULONG) 15)");
      writeDiagnostics(__FILE__, "     " + reader1->currentCell().toString());
   }

   *reader1 = (LONG) 123;
   if ((SlipDatum&)reader1->currentCell() != (LONG) 123) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: (SlipDatum&)reader1->currentCell() != (LONG) 123");
      writeDiagnostics(__FILE__, "     " +reader1->currentCell().toString());
   }
   if (*reader1 != (LONG) 123) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: *reader1 != (LONG) 123");
      writeDiagnostics(__FILE__, "     " +reader1->currentCell().toString());
   }
   if (!(*reader1 == (LONG) 123)) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: !(*reader1 == (LONG) 123)");
      writeDiagnostics(__FILE__, "     " +reader1->currentCell().toString());
   }

   *reader1 = (DOUBLE) 2.0;
   if ((SlipDatum&)reader1->currentCell() != (DOUBLE) 2.0) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: (SlipDatum&)reader1->currentCell() != (DOUBLE) 2.0");
      writeDiagnostics(__FILE__, "     " +reader1->currentCell().toString());
   }
   if (*reader1 != (DOUBLE) 2.0) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: *reader1 != (DOUBLE) 2.0");
      writeDiagnostics(__FILE__, "     " +reader1->currentCell().toString());
   }
   if (!(*reader1 == (DOUBLE) 2.0)) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: !(*reader1 == (DOUBLE) 2.0");
      writeDiagnostics(__FILE__, "     " +reader1->currentCell().toString());
   }

   *reader1 = "A String";
   if ((SlipDatum&)reader1->currentCell() != "A String") {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: (SlipDatum&)reader1->currentCell() != \"A String\"");
      writeDiagnostics(__FILE__, "     " +reader1->currentCell().toString());
   }
   if (*reader1 != "A String") {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: *reader1 != \"A String\"");
      writeDiagnostics(__FILE__, "     " +reader1->currentCell().toString());
   }
   if (!(*reader1 == "A String")) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: !(*reader1 == \"A String\")");
      writeDiagnostics(__FILE__, "     " +reader1->currentCell().toString());
   }

   *reader1 = str;
   if ((SlipDatum&)reader1->currentCell() != str) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: (SlipDatum&)reader1->currentCell() != str");
      writeDiagnostics(__FILE__, "     " +reader1->currentCell().toString());
   }
   if (*reader1 != str) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: *reader1 != str");
      writeDiagnostics(__FILE__, "     " +reader1->currentCell().toString());
   }
   if (!(*reader1 == str)) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: !(*reader1 == str)");
      writeDiagnostics(__FILE__, "     " +reader1->currentCell().toString());
   }

   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
      reader1->advanceLWR();
      if ((SlipDatum&)reader1->currentCell() != *data[i]) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: (SlipDatum&)reader1->currentCell() != *data[i]");
         writeDiagnostics(__FILE__, "     " +reader1->currentCell().toString());
      }
      if (*reader1 != *data[i]) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: *reader1 != *data[i]");
         writeDiagnostics(__FILE__, "     " +reader1->currentCell().toString());
      }
      if (!(*reader1 == *data[i])) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: !(*reader1 == *data[i])");
         writeDiagnostics(__FILE__, "     " +reader1->currentCell().toString());
      }
   }

   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
      reader2->insLeft(*new SlipDatum(*data[i]));
   }

   reader1->reset();
   reader1->advanceLWR();
   reader2->reset();
   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
      reader1->advanceLWR();
      reader2->advanceLWR();
      if (*reader1 != *reader2) {
         if (reader1->isPtr() != reader2->isPtr()) {
            flag = false;
            writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: *reader1 != *reader2");
            writeDiagnostics(__FILE__, "     " +reader1->currentCell().toString()
                                               +" != " +reader2->currentCell().toString());
         }
      }
      if (!(*reader1 == *reader2)) {
         if (reader1->isPtr() != reader2->isPtr()) {
            flag = false;
            writeDiagnostics(__FILE__, __LINE__, "   Comparison Failed: !(*reader1 == *reader2)");
            writeDiagnostics(__FILE__, "     " +reader1->currentCell().toString()
                                               +" != " +reader2->currentCell().toString());
         }
      }
   }

   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
      reader3->push(*new SlipDatum(*data[i]));
   }

   reader1->reset();
   reader1->advanceLWR();
   reader3->reset();
   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
      reader1->advanceLWR();
      reader3->advanceLWR();
      *reader1 = *reader3;
      if (*reader1 != *reader3) {
         if (reader1->isPtr() != reader3->isPtr()) {
            flag = false;
            writeDiagnostics(__FILE__, __LINE__, "   Comparison of assignment Failed: *reader1 != *reader3");
            writeDiagnostics(__FILE__, "     " +reader1->currentCell().toString()
                                               +" != " +reader3->currentCell().toString());
         }
      }
      if (!(*reader1 == *reader3)) {
         if (reader1->isPtr() != reader3->isPtr()) {
            flag = false;
            writeDiagnostics(__FILE__, __LINE__, "   Comparison of assignment Failed: !(*reader1 == *reader3)");
            writeDiagnostics(__FILE__, "     " +reader1->currentCell().toString()
                                               +" != " +reader3->currentCell().toString());
         }
      }
   }

   delete reader1;
   delete reader2;
   delete reader3;
   header1->deleteList();
   header2->deleteList();
   header3->deleteList();
   if (!flag)
      testResultOutput("Test Reader Operators", 0.0, flag);
   return flag;

}; // bool testReaderOperators()

bool testReaderPushPop() {
   bool flag = true;
   SlipHeader* header = new SlipHeader();
   SlipReader* reader = new SlipReader(*header);

   constructData();
   for(int i = sizeof(data)/sizeof(data[0]) - 1; i >= 0; i--)
      reader->push(*data[i]);

   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
      flag = (reader->advanceLWR() == (SlipDatum&)*data[i])? flag: false;
      if (!flag) {
         writeDiagnostics(__FILE__, __LINE__, "  push " + reader->currentCell().toString() + " != " + data[i]->toString());
      }
   }

   if (!(reader->advanceLWR()).isHeader()) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__,  "    push   reader.currentCell() is not a header: " + reader->toString());
   }

   reader->reset();

   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]) - 1; i++) {
      SlipCell* cell = &reader->pop();
      flag = (*cell == *data[i])? flag: false;
      if (!flag) {
         writeDiagnostics(__FILE__, __LINE__, "    pop    " + cell->toString() + " != " + data[i]->toString());
      }
      delete cell;
   }

   reader->pop();

   delete reader;
   header->deleteList();
   deleteData();
   if (!flag)
      testResultOutput("Test Reader Push/Pop", 0.0, flag);
   return flag;
}; // bool testReaderPushPop()

bool testReaderReplaceBot() {
   bool flag = true;
   SlipHeader* header = new SlipHeader();
   SlipReader* reader = new SlipReader(*header);

   constructData();
   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++)
      reader->insLeft(*data[i]);

   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
      bool locFlag = true;
      locFlag = ((reader->replaceBot(*data[i])).getBot() == (SlipDatum&)*data[i]);
      if (!locFlag) {
         if (!(reader->getBot().isPtr() && data[i]->isPtr())) {
            flag    = (locFlag)? flag: false;
            writeDiagnostics(__FILE__, __LINE__, "  replaceBot " + reader->getBot().toString() + " != " + data[i]->toString());
         }
      }
   }

   SlipHeader* newHeader = new SlipHeader();
   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
      bool locFlag = true;
      reader->insLeft(*newHeader);
      locFlag = ((reader->replaceBot(*data[i])).getBot() == (SlipDatum&)*data[i]);
      if (!locFlag) {
         if (!(reader->getBot().isPtr() && data[i]->isPtr())) {
            flag    = (locFlag)? flag: false;
            writeDiagnostics(__FILE__, __LINE__, "  replaceBot "+ reader->getBot().toString()+ " != "+ data[i]->toString());
         }
      }
   }

   reader->insLeft(*newHeader);
   SlipHeader* newerHeader = new SlipHeader();
   if (reader->replaceBot(*newerHeader).getBot() != *newerHeader) {
      flag    = false;
      writeDiagnostics(__FILE__, __LINE__, "  replaceBot "+ reader->getBot().toString()+ " != "+ newerHeader->toString());
   }


   delete reader;
   newerHeader->deleteList();
   newHeader->deleteList();
   header->deleteList();
   deleteData();
   if (!flag)
      testResultOutput("Test Reader Replace Bot", 0.0, flag);
   return flag;
}; // bool testReaderReplaceBot()

bool testReaderReplaceTop() {
   bool flag = true;
   SlipHeader* header = new SlipHeader();
   SlipReader* reader = new SlipReader(*header);

   constructData();
   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++)
      reader->push(*data[i]);

   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
      bool locFlag = true;
      locFlag = ((reader->replaceTop(*data[i])).getTop() == (SlipDatum&)*data[i]);
      if (!locFlag) {
         if (!(reader->getTop().isPtr() && data[i]->isPtr())) {
            flag    = (locFlag)? flag: false;
            cout << "  replaceTop " << reader->getTop().toString() << " != " << data[i]->toString() << endl;
         }
      }
   }

   SlipHeader* newHeader = new SlipHeader();
   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
      bool locFlag = true;
      reader->push(*newHeader);
      locFlag = ((reader->replaceTop(*data[i])).getTop() == (SlipDatum&)*data[i]);
      if (!locFlag) {
         if (!(reader->getTop().isPtr() && data[i]->isPtr())) {
            flag    = (locFlag)? flag: false;
           writeDiagnostics(__FILE__, __LINE__, "  replaceTop "+ reader->getTop().toString()+ " != "+ data[i]->toString());
         }
      }
   }

   reader->push(*newHeader);
   SlipHeader* newerHeader = new SlipHeader();
   if (reader->replaceTop(*newerHeader).getTop() != *newerHeader) {
      flag    = false;
      writeDiagnostics(__FILE__, __LINE__, "  replaceTop "+ reader->getTop().toString()+ " != "+ newerHeader->toString());
   }


   delete reader;
   newerHeader->deleteList();
   newHeader->deleteList();
   header->deleteList();
   deleteData();
   if (!flag)
      testResultOutput("Test Reader ReplaceTop", 0.0, flag);
   return flag;
}; // bool testReaderReplaceTop()

bool testReader() {
   high_resolution_clock::time_point start =  high_resolution_clock::now();
   bool flag = true;
   flag = (testReaderCore())?              flag: false;
   flag = (testReaderAdvanceLinear())?     flag: false;
   flag = (testReaderAdvanceStructural())? flag: false;
   flag = (testReaderMisc())?              flag: false;
   flag = (testReaderPushPop())?           flag: false;
   flag = (testReaderEnqueueDequeue())?    flag: false;
   flag = (testReaderReplaceBot())?        flag: false;
   flag = (testReaderReplaceTop())?        flag: false;
   flag = (testReaderMisc())?              flag: false;
   flag = (testReaderMoveSplit())?         flag: false;
   flag = (testReaderOperators())?         flag: false;
   
   high_resolution_clock::time_point end =  high_resolution_clock::now();
   duration<double> seconds = duration_cast<duration<double>>(end - start);
   testResultOutput("Test Reader", seconds.count(), flag);
   return flag;
}; // bool testReader()
