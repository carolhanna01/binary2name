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
 * @file    TestHeader.cpp
 * @author  A. Schwarz
 * @date    November 16, 2012
 *
 * @brief header test implmentation file.
 *
 */

# include <iostream>
# include <iomanip>
# include <sstream>

# include "TestHeader.h"
# include "TestGlobal.h"
# include "TestUtilities.h"
# include "../libgslip/SlipCell.h"
# include "../libgslip/SlipDatum.h"
# include "../libgslip/SlipHeader.h"
# include "../libgslip/SlipSublist.h"

using namespace slip;
using namespace std;

bool testDListCore();
bool testDListNonCore();
bool checkHeaderInsert(string str, SlipHeader* head, USHORT before);
bool checkHeaderDelete(string str, SlipHeader* head, USHORT before);

bool checkHeaderInsert(string str, SlipHeader* head, USHORT before) {
   bool flag = true;
   USHORT after = head->getRefCount();
   if (after != (before + 1)) {
      stringstream pretty;
      flag = false;
      pretty << " error refCnt" << head->dump();
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
      pretty << "  " << str << " reference count error "
             << after << " should be " << (before + 1);
      writeDiagnostics(__FILE__, pretty.str());
   }
   return flag;
}; // bool checkHeaderInsert(string str, SlipHeader* head, USHORT before)

bool testDequeue() {
   high_resolution_clock::time_point start =  high_resolution_clock::now();
   SlipState state        = SlipCellBase::getSlipState();
   bool passFailFlag;
   bool flag              = constructData();
   SlipHeader* header     = new SlipHeader();
   for (int i = 0; i < DATASIZE; i++) header->push(*data[i]);
   for (int i = 0; i < DATASIZE; i++) {
      SlipCell& cell = header->dequeue();
      string actual   = cell.toString();
      string expected = data[i]->toString();
      passFailFlag    = (actual == expected);
      flag            = (passFailFlag)? flag: false;
      if (!passFailFlag) {
         stringstream pretty;
         pretty << "pop " << passFailStr[passFailFlag] << "  "
                << setw(18) << setfill(' ') << name[i]
                << actual << " should be " << expected;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      delete &cell;
   }
   header->deleteList();
   flag = (checkSlipState("Test Pop ", state.total, state.avail))? flag: false;
   
   high_resolution_clock::time_point end =  high_resolution_clock::now();
   duration<double> seconds = duration_cast<duration<double>>(end - start);
   testResultOutput("Test Enqueue", seconds.count(), flag);
   return flag;
}; // void testDequeue()

/**
 * @brief Test descriptor list functionality.
 * @return <b>true</b> tests were succesful..
 */
bool testDList() {
   high_resolution_clock::time_point start =  high_resolution_clock::now();
   bool flag = true;
   flag = (testDListCore())? flag: false;
   flag = (testDListNonCore())? flag: false;
   
   high_resolution_clock::time_point end =  high_resolution_clock::now();
   duration<double> seconds = duration_cast<duration<double>>(end - start);
   testResultOutput("Test Descriptor List", seconds.count(), flag);
   return flag;
}; // bool testDescriptorList()

bool testDListCore() {
   stringstream pretty;
   bool flag = true;
   SlipHeader* header = new SlipHeader();
   if (header->isDList()) {
      string testStr("Test[ 1]");
      flag = false;
      header->dump();
      pretty << testStr << " DList reported when none exists";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }
   if (flag) {
      string testStr("Test[ 2]");
      header->create_dList();
      if (!header->isDList()) {
         header->dump();
         flag = false;
         pretty << testStr << " DList not created successfully.";
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
   }
   if (flag) {
      string testStr("Test[ 3]");
      unsigned count = header->size_dList();
      if (count != 0) {
         header->dump();
         flag = false;
         pretty << testStr << " DList should be empty.";
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
   }
   if (flag) {
      string testStr("Test[ 4]");
      header->delete_dList();
      if (header->isDList()) {
         header->dump();
         flag = false;
         pretty << testStr << " Deleted DList reported present.";
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
   }
   if (flag) {
      string testStr("Test[ 5]");
      header->create_dList();
      SlipDatum* key   = new SlipDatum((LONG)1);
      SlipDatum* value = new SlipDatum((LONG)2);
      header->put(*key, *value);
      unsigned count = header->size_dList();
      if (count == 0) {
         header->dump();
         flag = false;
         pretty << testStr << " Descriptor list should not be empty.";
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      } else if (count > 1) {
         header->dump();
         flag = false;
         pretty << testStr << " Descriptor list has more than one entry. Should have only one.";
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      } else if (!header->containsKey(*key)) {
         header->dump();
         flag = false;
         pretty << testStr << " Key not found.";
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      } else if (!header->contains(*value)) {
         header->dump();
         flag = false;
         pretty << testStr << " Value not found in Descriptor List.";
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      } else if (header->get(*key) != *value) {
         header->dump();
         flag = false;
         pretty << testStr << " Could not get(key).";
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      } else if (header->containsKey(*value)) {
         header->dump();
         flag = false;
         pretty << testStr << " Non-existent key found.";
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      } else if (header->contains(*key)) {
         header->dump();
         flag = false;
         pretty << testStr << " Non-existent value found..";
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
   }
   if (flag) {
      string testStr("Test[ 6]");
      header->flush_dList();
      if (!header->isDList()) {
         header->dump();
         flag = false;
         pretty << testStr << " Descriptor list not flushed correctly.";
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
   }
   if (flag) {
      header->delete_dList();
      if (header->isDList()) {
         string testStr("Test[ 7]");
         header->dump();
         flag = false;
         pretty << testStr << " Not deleted correctly.";
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
   }
   header->deleteList();
   if (!flag)
      testResultOutput("Test Descriptor List core ", 0.0, flag);
   return flag;
}; // bool testDlistCore()

bool testDListNonCore() {
   bool flag = true;
   struct keyValuePair {
      SlipCell* key;
      SlipCell* value;
   };
   SlipHeader* header    = new SlipHeader();
   SlipHeader* attribute = new SlipHeader();
   keyValuePair keyValue[] = { new SlipDatum((bool)false),  new SlipDatum((string)"true")
                             , new SlipDatum(((UCHAR)1)),   new SlipDatum((CHAR)2)
                             , new SlipDatum((CHAR)3),      new SlipDatum((ULONG)4)
                             , new SlipDatum((ULONG)5),     new SlipDatum((DOUBLE)6.0)
                             , new SlipDatum((DOUBLE)7.0),  new SlipDatum((UCHAR)8)
                             , new SlipSublist(*attribute), new SlipSublist(*attribute)
                             };
   const int keyValueSize = sizeof(keyValue)/sizeof(keyValue[0]);
   header->create_dList();
   for (int i = 0; i < keyValueSize; i ++) {
      header->put(*keyValue[i].key, *keyValue[i].value);
   }
   if (header->size_dList() != keyValueSize) {
      stringstream pretty;
      string testStr("Test[ 1]");
      header->dump();
      flag = false;
      pretty << testStr << " Incorrect number of attributes "
                      << header->size_dList()
                      << " should be "
                      << keyValueSize;
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }
   if (flag) {
      stringstream pretty;
      string testStr("Test[ 2]");
      for ( int i = 0; i < keyValueSize; i ++) {
         if (!header->containsKey(*keyValue[i].key)) {
            flag = false;
            pretty << testStr << " key not found for key[" << i << "]"
                   << keyValue[i].key->toString();
            writeDiagnostics(__FILE__, __LINE__, pretty.str());
            header->dump();
         }
      }
  }
   if (flag) {
      stringstream pretty;
      string testStr("Test[ 3]");
      int i;
      for ( i = 0; i < keyValueSize; i++) {
         if (!header->contains(*keyValue[i].value)) {
            header->dump();
            flag = false;
            pretty << testStr << " value "
                              << keyValue[i].value->toString()
                              << " not found";
            writeDiagnostics(__FILE__, __LINE__, pretty.str());
         }
      }
   }
   if (flag) {
      string testStr("Test[ 4]");
      for (int i = 0; i < keyValueSize; i++) {
         SlipDatum key((SlipDatum&)*(keyValue[i].key));
         SlipDatum value((SlipDatum&)*(keyValue[i].value));
         header->deleteAttribute(key);
         if (header->containsKey(key)) {
            stringstream pretty;
            header->dump();
            flag = false;
            pretty << testStr << " failed to delete key  "
                              << key.toString();
            writeDiagnostics(__FILE__, __LINE__, pretty.str());
         }
         if (header->contains(value)) {
            stringstream pretty;
            header->dump();
            flag = false;
            pretty << testStr << " failed to delete value  " << endl
                              << "     key:  " << key.toString() << endl
                              << "     value:" << value.toString();
            writeDiagnostics(__FILE__, __LINE__, pretty.str());
         }
      }
   }
   attribute->deleteList();
   header->deleteList();
   if (!flag)
      testResultOutput("Test Descriptor List non-core ", 0.0, flag);
   return flag;
}; // bool testDlistNonCore()


bool testEnqueue() {
   high_resolution_clock::time_point start =  high_resolution_clock::now();
   SlipState state        = SlipCellBase::getSlipState();
   bool flag              = constructData();
   SlipHeader* header     = new SlipHeader();

   for (int i = 0; i < DATASIZE; i++) header->enqueue(*data[i]);
   flag = (checkSlipState("Test Enqueue Slip Datum Cell Allocation", state.total, (state.avail - DATASIZE - 1)))? flag: false;
   header->flush();
   flag = (checkSlipState("Test Enqueue Allocation", state.total, state.avail - 1))? flag: false;
   header->enqueue((bool)true);
   header->enqueue((CHAR)'a');
   header->enqueue(2.0);
   header->enqueue((LONG)123);
   header->enqueue(new Pointer());
   header->enqueue(str);
   header->enqueue(str);
   header->enqueue((UCHAR) 'A');
   header->enqueue((ULONG) 15);
   flag = (checkSlipState("Test Enqueue Slip Datum Allocation", state.total, (state.avail - DATASIZE - 1)))? flag: false;
   header->flush();
   header->deleteList();
   deleteData();
   flag = (checkSlipState("Test Enqueue Deallocation", state.total, state.avail))? flag: false;
   
   high_resolution_clock::time_point end =  high_resolution_clock::now();
   duration<double> seconds = duration_cast<duration<double>>(end - start);
   testResultOutput("Test Enqueue", seconds.count(), flag);
   return flag;
}; // void testEnqueue()

/**
 * @brief Test miscellaneous header functionality.
 * <p>Test the following:</p>
 * <ul>
 *    <li><b>copy()</b> Copy one list to another during instantiation.</li>
 *    <li><b>flush()</b> Empty the list contents.</li>
 *    <li><b>getBot()</b> Return the bottom list object.</li>
 *    <li><b>getRefCnt()</b> Return the reference count.</li>
 *    <li><b>getTop()</b> Return the top list object.</li>
 *    <li><b>isEmpty()</b> Is the list empty.</li>
 *    <li><b>putMark()</b> Change the list user mark value.</li>
 * </ul>
 * @return <b>true</b> test was successful
 */
bool testHeaderMisc() {
   high_resolution_clock::time_point start =  high_resolution_clock::now();
   SlipState state        = SlipCellBase::getSlipState();
   bool flag              = constructData();
   SlipHeader* header     = new SlipHeader();
   for (int i = 0; i < DATASIZE; i++) header->enqueue(*data[i]);
   flag = (checkSlipState("Test Header Misc Allocation ", state.total, (state.avail - DATASIZE - 1)))? flag: false;

   SlipCell& x    = header->getBot();    // compiler bugfix
   string actual  = x.toString();        // compiler bugfix
   if (actual != resultStr[DATASIZE - 1]) {
      stringstream pretty;
      flag = false;
      pretty << "TestHeaderMist getBot() FAIL "
             << actual << " != " <<  resultStr[DATASIZE - 1];
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }

   SlipCell& y    = header->getTop();   // compiler bugfix
   actual  = y.toString();              // compiler bugfix
   if (actual != resultStr[0]) {
      stringstream pretty;
      flag = false;
      pretty << "TestHeaderMist getBot() FAIL "
             << actual << " != " <<  resultStr[0];
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }
   state        = SlipCellBase::getSlipState();
   SlipHeader* duplicateList = new SlipHeader(header);
   flag = (checkSlipState("Test Header Misc listCopy() ", state.total, (state.avail - DATASIZE - 1)))? flag: false;
   flag = (compareLists(header, duplicateList))? flag: false;

   header->putMark(201);
   if (201 != header->getMark()) {
      flag         = false;
      string message = "Test Header Misc putMark(201) != " + header->getMark();
      testResultOutput(message, 0.0, false);
   }

   if (header->isEmpty()) {
      flag         = false;
      testResultOutput("Test Header Misc isEmpty() is not empty ",0.0,  false);
   }

   header->flush();
   if (!header->isEmpty()) {
      flag         = false;
      testResultOutput("Test Header Misc isEmpty() ", 0.0, false);
   }

   short count = header->getRefCount();
   duplicateList->push(*header);
   if (count != (header->getRefCount() - 1)) {
      flag         = false;
      testResultOutput("Test Header Misc getRefCount() ", 0.0, false);
   }

   header->deleteList();
   duplicateList->deleteList();
   deleteData();
   
   high_resolution_clock::time_point end =  high_resolution_clock::now();
   duration<double> seconds = duration_cast<duration<double>>(end - start);
   testResultOutput("Test Header Misc", seconds.count(), flag);
   return flag;
}; // void testHeaderMisc()

bool testMoveLeft() {
   stringstream pretty;
   high_resolution_clock::time_point start =  high_resolution_clock::now();
   bool flag              = constructData();
   SlipHeader* testHeader = new SlipHeader();
   SlipHeader* dst        = new SlipHeader();
   SlipHeader* src        = new SlipHeader();
   dst->push((LONG)0xfeedfade);

   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) src->enqueue(*new SlipDatum(*data[i]));
   *testHeader             = *src;
   testHeader->push((LONG)0xfeedfade);

   dst->moveLeft(*src);
   if (!compareLists(dst, testHeader)) {
      flag = false;
      pretty << "-- Test Move Left Full        FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   } else if (!src->isEmpty()) {
      flag = false;
      pretty << "-- Test Move Left not Empty   FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }

   delete &testHeader->pop();
   testHeader->push((LONG)0xdeadbeef);
   src->push((LONG)0xdeadbeef);
   (dst->getTop()).moveLeft(src->getTop());

   if (!compareLists(dst, testHeader, 1)) {
      flag = false;
      pretty << "-- Test Move Left Singleton   FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   } else if (!src->isEmpty()) {
      flag = false;
      pretty << "-- Test Move Left not Empty   FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }

   dst->flush();
   src->push((LONG)0xdeadbeef);
   (dst->getTop()).moveLeft(src->getTop());
   if (!compareLists(dst, testHeader, 1)) {
      flag = false;
      pretty << "-- Test Move Left to Empty    FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   } else if (!src->isEmpty()) {
      flag = false;
      pretty << "-- Test Move Left not Empty   FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }

   dst->flush();
   dst->push((ULONG)0xfaded);
   src->push((LONG)0xdeadbeef);
   (dst->getTop()).moveLeft(src->getTop());
   if (!compareLists(dst, testHeader, 1)) {
      flag = false;
      pretty << "-- Test Move Left to Single    FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   } else if (!src->isEmpty()) {
      flag = false;
      pretty << "-- Test Move Left not Empty    FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }

   delete &testHeader->pop();
   src->flush();
   *src = *testHeader;
   dst->flush();
   dst->moveListLeft(*src);

   if (!compareLists(dst, testHeader)) {
      flag = false;
      pretty << "-- Test Move Left bot()       FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   } else if (!src->isEmpty()) {
      flag = false;
      pretty << "-- Test Move Left not Empty   FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }

   dst->deleteList();
   src->deleteList();
   testHeader->deleteList();
   deleteData();
   
   high_resolution_clock::time_point end =  high_resolution_clock::now();
   duration<double> seconds = duration_cast<duration<double>>(end - start);
   testResultOutput("Test Move List Left", seconds.count(), flag);
   return flag;
}; // testMoveLeft()

bool testMoveRight() {
   stringstream pretty;
   high_resolution_clock::time_point start =  high_resolution_clock::now();
   bool flag              = constructData();
   SlipHeader* testHeader = new SlipHeader();
   SlipHeader* dst        = new SlipHeader();
   SlipHeader* src        = new SlipHeader();

   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) src->enqueue(*new SlipDatum(*data[i]));
   *testHeader            = *src;

   dst->moveRight(*src);
   if (!compareLists(dst, testHeader)) {
      flag = false;
      pretty << "-- Test Move Right Full       FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   } else if (!src->isEmpty()) {
      flag = false;
      pretty << "-- Test Move Right not Empty  FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }

   testHeader->push((LONG)0xdeadbeef);
   src->push((LONG)0xdeadbeef);
   (dst)->moveRight(src->getTop());
   if (!compareLists(dst, testHeader)) {
      flag = false;
      pretty << "-- Test Move Right Singleton  FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   } else if (!src->isEmpty()) {
      flag = false;
      pretty << "-- Test Move Right not Empty  FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }

   dst->flush();
   src->push((LONG)0xdeadbeef);
   (dst->getTop()).moveRight(src->getTop());
   if (!compareLists(dst, testHeader, 1)) {
      flag = false;
      pretty << "-- Test Move Right to Empty    FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   } else if (!src->isEmpty()) {
      flag = false;
      pretty << "-- Test Move Right not Empty   FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }

   dst->flush();
   dst->push((ULONG)0xfaded);
   src->push((LONG)0xdeadbeef);
   testHeader->push((LONG)0xdeadbeef);
   testHeader->push((ULONG)0xfaded);
   (dst->getTop()).moveRight(src->getTop());
   if (!compareLists(dst, testHeader, 2)) {
      flag = false;
      pretty << "-- Test Move Right to Single  FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   } else if (!src->isEmpty()) {
      flag = false;
      pretty << "-- Test Move Right not Empty  FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }

   delete &testHeader->pop();
   delete &testHeader->pop();
   delete &testHeader->pop();
   src->flush();
   *src = *testHeader;
   dst->flush();
   dst->moveListRight(*src);
   if (!compareLists(dst, testHeader)) {
      flag = false;
      pretty << "-- Test Move Right bot()       FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   } else if (!src->isEmpty()) {
      flag = false;
      pretty << "-- Test Move Right not Empty   FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }

   dst->deleteList();;
   src->deleteList();;
   testHeader->deleteList();
   deleteData();
   
   high_resolution_clock::time_point end =  high_resolution_clock::now();
   duration<double> seconds = duration_cast<duration<double>>(end - start);
   testResultOutput("Test Move List Right", seconds.count(), flag);
   return flag;
}; // testMoveRight()

bool testPop() {
   high_resolution_clock::time_point start =  high_resolution_clock::now();
   SlipState state        = SlipCellBase::getSlipState();
   bool passFailFlag;
   bool flag              = constructData();
   SlipHeader* header     = new SlipHeader();
   for (int i = 0; i < DATASIZE; i++) header->enqueue(*data[i]);
   for (int i = 0; i < DATASIZE; i++) {
      SlipCell& cell = header->pop();
      string actual   = cell.toString();
      string expected = data[i]->toString();
      passFailFlag    = (actual == expected);
      flag            = (passFailFlag)? flag: false;
      if (!passFailFlag) {
         stringstream pretty;
         pretty << "pop " << passFailStr[passFailFlag] << "  "
                << setw(18) << setfill(' ') << name[i]
                << actual << " should be " << expected;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      delete &cell;
   }
   header->deleteList();
   flag = (checkSlipState("Test Pop ", state.total, state.avail))? flag: false;
   
   high_resolution_clock::time_point end =  high_resolution_clock::now();
   duration<double> seconds = duration_cast<duration<double>>(end - start);
   testResultOutput("Test Pop", seconds.count(), flag);
   return flag;
}; // bool testPop()

bool testPush() {
   high_resolution_clock::time_point start =  high_resolution_clock::now();
   SlipState state        = SlipCellBase::getSlipState();
   bool flag              = constructData();
   SlipHeader* header     = new SlipHeader();
   for (int i = 0; i < DATASIZE; i++) header->push(*data[i]);
   flag = (checkSlipState("Test Push Slip Datum Cell Allocation", state.total, (state.avail - DATASIZE - 1)))? flag: false;
   header->flush();
   flag = (checkSlipState("Test Push Allocation", state.total, state.avail - 1))? flag: false;
   header->push((bool)true);
   header->push((CHAR)'a');
   header->push(2.0);
   header->push((LONG)123);
   header->push(new Pointer());
   header->push((string*)str);
   header->push((string*)str);
   header->push((UCHAR) 'A');
   header->push((ULONG) 15);
   flag = (checkSlipState("Test Push Slip Datum Allocation", state.total, (state.avail - DATASIZE - 1)))? flag: false;
   header->flush();
   header->deleteList();
   deleteData();
   flag = (checkSlipState("Test Push Deallocation", state.total, state.avail))? flag: false;
   
   high_resolution_clock::time_point end =  high_resolution_clock::now();
   duration<double> seconds = duration_cast<duration<double>>(end - start);
   testResultOutput("Test Push", seconds.count(), flag);
   return flag;
}; // bool testPush()

bool testSplitLeft() {
   high_resolution_clock::time_point start =  high_resolution_clock::now();
   bool flag              = constructData();
   SlipHeader* header     = new SlipHeader();
   SlipCell*   cellPtr[DATASIZE];

   for (int i = 0; i < DATASIZE; i++) cellPtr[i] = &(header->insLeft(*data[i]));
   SlipHeader* duplicate = new SlipHeader(header);
   SlipHeader& splitHeader1 = header->splitLeft(*cellPtr[DATASIZE/2]);
   if (!compareLists(&splitHeader1, duplicate, DATASIZE/2)) {
      stringstream pretty;
      flag = false;
      pretty << "-- Test Split Left            FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }
   (&splitHeader1)->deleteList();

   for(int i = 0; i < DATASIZE/2 + 1; i++) {
      delete &duplicate->pop();
   }
   SlipHeader& splitHeader2 = header->splitLeft(*cellPtr[DATASIZE/2 + 1]);

   if (!compareLists(&splitHeader2, duplicate, 1)) {
      stringstream pretty;
      flag = false;
      pretty << "-- TestSplit Left Top         FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }
   (&splitHeader2)->deleteList();

   header->flush();
   duplicate->flush();
   duplicate->enqueue(*new SlipDatum((ULONG)4321));
   header->enqueue(*new SlipDatum((ULONG)4321));
   SlipHeader& splitHeader3 = header->splitLeft(header->getBot());
   if (!compareLists(&splitHeader3, duplicate, 1)) {
      stringstream pretty;
      flag = false;
      pretty << "-- TestSplit Left Singleton   FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }
   header->deleteList();
   duplicate->deleteList();
   (&splitHeader3)->deleteList();

   deleteData();
   
   high_resolution_clock::time_point end =  high_resolution_clock::now();
   duration<double> seconds = duration_cast<duration<double>>(end - start);
   testResultOutput("Test Split Left", seconds.count(), flag);
   return flag;
}; // void testSplitLeft()

bool testSplitRight() {
   high_resolution_clock::time_point start =  high_resolution_clock::now();
   bool flag              = constructData();
   SlipHeader* header     = new SlipHeader();
   SlipCell*   cellPtr[DATASIZE];

   for (int i = 0; i < DATASIZE; i++) cellPtr[i] = &(header->insLeft(*data[i]));
   SlipHeader* duplicate = new SlipHeader(header);
   for(int i = 0; i < DATASIZE/2; i++) delete &duplicate->pop();
   SlipHeader& splitHeader1 = header->splitRight(*cellPtr[DATASIZE/2]);
   if (!compareLists(&splitHeader1, duplicate, DATASIZE/2)) {
      stringstream pretty;
      flag = false;
      pretty << "-- Test Split Right           FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }
   (&splitHeader1)->deleteList();
   duplicate->flush();

   duplicate->push(*new SlipDatum((SlipDatum&)header->getBot()));
   SlipHeader& splitHeader2 = header->splitRight(header->getBot());

   if (!compareLists(&splitHeader2, duplicate, 1)) {
      stringstream pretty;
      flag = false;
      pretty << "-- TestSplit Right Bot        FAIL";
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }
   (&splitHeader2)->deleteList();

   header->flush();
   duplicate->flush();
   duplicate->enqueue(*new SlipDatum((ULONG)4321));
   header->enqueue(*new SlipDatum((ULONG)4321));
   SlipHeader& splitHeader3 = header->splitRight(header->getTop());
   if (!compareLists(&splitHeader3, duplicate, 1)) {
      stringstream pretty;
      flag = false;
      pretty << "-- TestSplit Right Singleton  FAIL";
   }

   header->deleteList();
   duplicate->deleteList();
   (&splitHeader3)->deleteList();

   deleteData();
   
   high_resolution_clock::time_point end =  high_resolution_clock::now();
   duration<double> seconds = duration_cast<duration<double>>(end - start);
   testResultOutput("Test Split Right", seconds.count(), flag);
   return flag;
}; // void testSplitRight()
