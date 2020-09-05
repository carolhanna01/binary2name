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
 * @file    TestReplace.cpp
 * @author  A. Schwarz
 * @date November 19, 2012
 *
 * @brief Test Replace functionality implementation file.
 *
 */

# include <iostream>
# include <sstream>

# include "TestReplace.h"
# include "TestGlobal.h"
# include "TestUtilities.h"
# include "../libgslip/SlipCell.h"
# include "../libgslip/SlipCellBase.h"
# include "../libgslip/SlipDatum.h"
# include "../libgslip/SlipHeader.h"
# include "../libgslip/SlipSublist.h"

using namespace slip;
using namespace std;

bool checkHeaderDelete(string str, SlipHeader* head, USHORT before);
bool testReplaceBot(string test[]);
bool testReplaceSublist();
bool testReplaceSublistBot(SlipDatum* datum);
bool testReplaceSublistTop(SlipDatum* datum);
bool testReplaceTop(string test[]);

void outputOperatorFailure( SlipDatum& data   // lhs va lue after operation: lhs <op> test
                          , string     test   // test being conducted
                          , string     ans    // answer expected
                          ) {
   stringstream pretty;
   pretty << test << " fail: expected answer is " << ans;
   writeDiagnostics(__FILE__, __LINE__, pretty.str());
   writeDiagnostics(__FILE__, data.dump());
}; // void outputOperatorFailure(SlipDatum& data, string test, string ans)
bool checkHeaderDelete(string str, SlipHeader* head, USHORT before) {
   bool flag = true;
   USHORT after = head->getRefCount();
   USHORT chkBefore = (before == 0)? 0: before - 1;
   if (after != chkBefore) {
      stringstream pretty;
      flag = false;
      pretty << " error refCnt" << head->dump();
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
      pretty << "  " << str << " reference count error "
             << after << " should be " << chkBefore;
    writeDiagnostics(__FILE__, pretty.str());
   }
   return flag;
}; // bool checkHeaderDelete(string str, SlipHeader* head, USHORT before)

bool testReplaceBot(string test[]) {
   enum { MAX_SIZE = 8 };
   enum { RAND_SEED = 16729 };
   stringstream pretty;
   long        startState = SlipCellBase::getSlipState().avail;
   long        state1;
   long        state2;
   USHORT      refCntBefore;
   bool        flag       = constructData();
   SlipHeader* header     = new SlipHeader();
   SlipHeader* sublistHead= new SlipHeader();
   string      str[MAX_SIZE];

   srand (RAND_SEED);
   for (int i = 0; i < 10; i++) sublistHead->push((LONG)rand());

   header->enqueue((LONG)1);

   refCntBefore = sublistHead->getRefCount();
   header->push(*sublistHead);
   flag = (checkSublistInsert("push(*sublistHead)", sublistHead, refCntBefore))? flag: false;

   state1 = SlipCellBase::getSlipState().avail;

   /*************************************************************
    * Test whether replacement of a SlipDatum cell by a
    * SlipDatum cell is successful.
    *
    * At each loop entry the top of the list has a SlipDatum cell.
    *
    * At each loop exit:
    * 1. The datum at the top is replaced by new SlipDatum cell.
    * 2. The original top datum cell is deleted.
    * 3. The AVSL size is >= the loop start AVSL size. It can
    *    be greater if allocation of a new datum cell is at the
    *    expense of a deleted sublist (at the top of the AVSL).
    *
    * At the end of the loop:
    * 1. The AVSL size will be greater than or equal to the size
    *    at loop entry.
    * 2. All cells stored into the global 'data' array, but one,
    *    will be deleted.
    ************************************************************/
   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
      header->getBot().replace(*data[i]);
      string str1   = (header->getBot()).toString();
      string str2   = data[i]->toString();
      state2 = SlipCellBase::getSlipState().avail;
      if ( str1 != str2) {
         flag = false;
         pretty << "  data[" << i << "] '"
                << str1 << "' != '"
                << str2 << "' replacment wrong ";
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ( (state1 - state2) > 1) {
         flag = false;
         pretty << " For data[" << i << "] "
                << " AVSL size wrong " << state2 << " != " << state1;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
   }

   /*************************************************************
    * Test single replacement of a literal value creates the
    * correct datum cell.
    *
    * At entry the list cop contains a data cell.
    * This data cell is replaced by a literal value creating
    *   a new SlipDatum cell.
    * At exit the AVSL size is greater than or equal to the
    * size at the start.
   *************************************************************/

   state1 = SlipCellBase::getSlipState().avail;

   (header->getBot()).replace((bool) true);   str[0] = header->getBot().toString();
   (header->getBot()).replace((CHAR) 'a');    str[1] = header->getBot().toString();
   (header->getBot()).replace(2.0);           str[2] = header->getBot().toString();
   (header->getBot()).replace((LONG) 123);    str[3] = header->getBot().toString();
   (header->getBot()).replace(new Pointer()); str[4] = header->getBot().toString();
   (header->getBot()).replace(::str);         str[5] = header->getBot().toString();
   (header->getBot()).replace((UCHAR) 'A');   str[6] = header->getBot().toString();
   (header->getBot()).replace((ULONG) 15);    str[7] = header->getBot().toString();
   for ( int i = 0; i < MAX_SIZE; i++) {
      if (str[i] != test[i]) {
         flag = false;
         string strng = str[i];
         pretty << "  test[" << i << "] '"
                << strng   << "' != "
                << test[i] << "' replace wrong.";
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
   }
   state2 = SlipCellBase::getSlipState().avail;
   if (state2 != state1) {
      flag = false;
      pretty << " After replacement of the top SlipDatum cell by a literal, the "
             << " AVSL size " << state2 << " != " << state1;
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }

   header->flush();
   sublistHead->flush();

   refCntBefore = header->getRefCount();
   header->deleteList();
   flag = (checkHeaderDelete("delete header", header, refCntBefore))? flag: false;

   refCntBefore = sublistHead->getRefCount();
   sublistHead->deleteList();
   flag = (checkHeaderDelete("delete sublistHead", sublistHead, refCntBefore))? flag: false;

   deleteData();

   /*************************************************************
    * All test cells must be released prior to exit.
   *************************************************************/
   state1 = SlipCellBase::getSlipState().avail;
   if (abs(state1 - startState) > 1) {
      flag = false;
      pretty << " All space was not released. AVSL size "
             << state1 << " >= " << (startState - 1);
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
      slip::SlipCellBase::printAVSL();
   }
   if (!flag)
      testResultOutput("Test Replace Bot ", 0.0, flag);
   return flag;
}; // bool testReplaceBot(string test[])

/*************************************************************
 * testReplaceTop MUST be executed before testReplaceBot
 *************************************************************/
bool testReplaceTop(string test[]) {
   enum { MAX_SIZE = 8 };
   enum { RAND_SEED = 16729 };
   stringstream pretty;
   long        startState = SlipCellBase::getSlipState().avail;
   USHORT      refCntBefore;
   bool        flag       = constructData();
   SlipHeader* header     = new SlipHeader();
   SlipHeader* sublistHead= new SlipHeader();
   long        state1;
   long        state2;
   string      str[MAX_SIZE];

   srand (RAND_SEED);
   for (int i = 0; i < 10; i++) sublistHead->push((LONG)rand());

   header->push((LONG)1);


   refCntBefore = sublistHead->getRefCount();
   header->enqueue(*sublistHead);
   flag = (checkSublistInsert("enqueue(*sublistHead)", sublistHead, refCntBefore))? flag: false;

   state1 = SlipCellBase::getSlipState().avail;

   /*************************************************************
    * Test whether replacement of a SlipDatum cell  by a
    * SlipDatum cell is successful.
    *
    * At each loop entry the top of the list has a SlipDatum cell.
    *
    * At each loop exit:
    * 1. The datum at the top is replaced by new SlipDatum cell.
    * 2. The original top datum cell is deleted.
    * 3. The AVSL size is >= the loop start AVSL size. It can
    *    be greater if allocation of a new datum cell is at the
    *    expense of a deleted sublist (at the top of the AVSL).
    *
    * At the end of the loop:
    * 1. The AVSL size will be greater than or equal to the size
    *    at loop entry.
    * 2. All cells stored into the global 'data' array, but one,
    *    will be deleted.
    ************************************************************/
   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
      (header->getTop()).replace(*data[i]);
      string str1   = (header->getTop()).toString();
      string str2   = data[i]->toString();
      state2 = SlipCellBase::getSlipState().avail;
      if ( str1 != str2) {
         flag = false;
         pretty << "  data[" << i << "] '"
                << str1 << "' != '"
                << str2 << "' replacement wrong ";
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ( (state1 - state2) > 1) {
         flag = false;
         pretty << " for data[" << i << "] "
                << " AVSL size wrong " << state2 << " != " << state1;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
   }

   /*************************************************************
    * Test single replacement of a literal value creates the
    * correct datum cell.
    *
    * At entry the list cop contains a data cell.
    * This data cell is replaced by a literal value creating
    *   a new SlipDatum cell.
    * At exit the AVSL size is greater than or equal to the
    * size at the start.
   *************************************************************/

   state1 = SlipCellBase::getSlipState().avail;

   (header->getTop()).replace((bool) true);   str[0] = header->getTop().toString();
   (header->getTop()).replace((CHAR) 'a');    str[1] = header->getTop().toString();
   (header->getTop()).replace(2.0);           str[2] = header->getTop().toString();
   (header->getTop()).replace((LONG) 123);    str[3] = header->getTop().toString();
   (header->getTop()).replace(new Pointer()); str[4] = header->getTop().toString();
   (header->getTop()).replace(::str);         str[5] = header->getTop().toString();
   (header->getTop()).replace((UCHAR) 'A');   str[6] = header->getTop().toString();
   (header->getTop()).replace((ULONG) 15);    str[7] = header->getTop().toString();
   for ( int i = 0; i < MAX_SIZE; i++) {
      if (str[i] != test[i]) {
         flag = false;
         string strng = str[i];
         pretty << "  test[" << i << "] '"
                << strng   << "' != "
                << test[i] << "' replace wrong.";
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
   }
   state2 = SlipCellBase::getSlipState().avail;
   if (state2 != state1) {
      flag = false;
      pretty << " After replacement of the top SlipDatum cell by a literal, the "
             << " AVSL size " << state2 << " < " << state1;
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }

   header->flush();
   sublistHead->flush();

   refCntBefore = header->getRefCount();
   header->deleteList();
   flag = (checkHeaderDelete("delete header", header, refCntBefore))? flag: false;

   refCntBefore = sublistHead->getRefCount();
   sublistHead->deleteList();
   flag = (checkHeaderDelete("delete sublistHead", sublistHead, refCntBefore))? flag: false;
   deleteData();

   /*************************************************************
    * All test cells must be released prior to exit.
   *************************************************************/
   state2 = SlipCellBase::getSlipState().avail;
   if (state2 != (startState - 1)) {
      flag = false;
      pretty << " All space was not released. AVSL size "
             << state2 << " != " << (startState - 1);
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
      slip::SlipCellBase::printAVSL();
   }
   if (!flag)
      testResultOutput("Test Replace Bot ", 0.0, flag);
   return flag;
}; // bool testReplaceTop(string test[])

bool testReplaceSublist() {
   bool flag = true;

   flag = testReplaceSublistBot(new SlipDatum((bool)true))?   flag: false;
   flag = testReplaceSublistBot(new SlipDatum((CHAR)'a'))?    flag: false;
   flag = testReplaceSublistBot(new SlipDatum((UCHAR)'b'))?   flag: false;
   flag = testReplaceSublistBot(new SlipDatum((LONG)9871))?   flag: false;
   flag = testReplaceSublistBot(new SlipDatum((ULONG)2468))?  flag: false;
   flag = testReplaceSublistBot(new SlipDatum((DOUBLE)3.14))? flag: false;
   flag = testReplaceSublistBot(new SlipDatum((string*)str))? flag: false;
   flag = testReplaceSublistBot(new SlipDatum(new Pointer()))?flag: false;

   flag = testReplaceSublistTop(new SlipDatum((bool)true))?   flag: false;
   flag = testReplaceSublistTop(new SlipDatum((CHAR)'a'))?    flag: false;
   flag = testReplaceSublistTop(new SlipDatum((UCHAR)'b'))?   flag: false;
   flag = testReplaceSublistTop(new SlipDatum((LONG)9871))?   flag: false;
   flag = testReplaceSublistTop(new SlipDatum((ULONG)2468))?  flag: false;
   flag = testReplaceSublistTop(new SlipDatum((DOUBLE)3.14))? flag: false;
   flag = testReplaceSublistTop(new SlipDatum((string*)str))? flag: false;
   flag = testReplaceSublistTop(new SlipDatum(new Pointer()))?flag: false;

   return flag;
}; // bool testReplaceSublist(string test[], SlipHeader* sublistHead)s

bool testReplaceSublistBot(SlipDatum* datum) {
   stringstream pretty;
   bool  flag    = true;
   SHORT refCntBefore;
   static string start = "Start";
   static string sublistText = "Sublist";
   SlipHeader* header = new SlipHeader();
   SlipHeader* sublistHead = new SlipHeader();
   SlipDatum& copy = *new SlipDatum(*datum);
   string name = "  for '" + datum->toString() + "'";

   header->push((string*)&start);
   header->enqueue((SlipDatum&)*datum);
   sublistHead->push((string*)&sublistText);

   /*******************************************************
    * Verify data on list bottom                          *
    ******************************************************/
   if (!header->getBot().isData()) {
      string testStr("test 1" + name);
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, header->getBot().dump());
      pretty << testStr << " bottom should be Data ";
      writeDiagnostics(__FILE__, pretty.str());
   } else if ((header->getBot()).toString() != datum->toString()) {
      string testStr("test 1" + name);
      flag = false;
      pretty << testStr << datum->toString()
             << " should be " << datum->toString()
             << " is " << header->getBot().toString();
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }

   /*******************************************************
    * Verify a sublist can replace a datum cell           *
    ******************************************************/
   if (flag) {
      string testStr("test 2" + name);
      refCntBefore = sublistHead->getRefCount();
      &header->replaceBot((SlipHeader&)*sublistHead);
      flag = (checkSublistInsert(testStr, sublistHead, refCntBefore))? flag: false;

      if (!header->getBot().isSublist()) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, header->getBot().toString());
         pretty << "  " << testStr << " bottom is not a sublist";
         writeDiagnostics(__FILE__, pretty.str());
      }
   }

   /*******************************************************
    * Verify a a datum cell can replace a sublist         *
    ******************************************************/
   if (flag) {
      string testStr("test 3" + name);
      refCntBefore = sublistHead->getRefCount();
      &header->replaceBot(copy);
      flag = (checkSublistDelete(testStr, sublistHead, refCntBefore))? flag: false; // sublist header is not deleted. refCnt unchanged

      if (!header->getBot().isData()) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, header->getBot().dump());
         pretty << testStr  << " List bottom is not data";
         writeDiagnostics(__FILE__, pretty.str());
      } else if ((header->getBot()).toString() != copy.toString()) {
         flag = false;
         pretty << testStr
                << " List bottom value should be " << copy.toString()
                << " is " << header->getTop().toString();
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
   }

   /*******************************************************
    * Verify a header is converted to a sublist and       *
    * replaces a datum cell and no action when a header   *
    * attempts to replace a sublist pointing to the header*
    ******************************************************/
   if (flag) {
      string testStr("test 4" + name);

      refCntBefore = sublistHead->getRefCount();
      &header->replaceBot(*sublistHead);
      flag = (checkSublistInsert(testStr, sublistHead, refCntBefore))? flag: false;

      refCntBefore = sublistHead->getRefCount();
      &header->replaceBot(*sublistHead);
      flag = (checkSublistInsert(testStr, sublistHead, (refCntBefore - 1)))? flag: false;

      if (!header->getBot().isSublist()) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, header->getBot().toString());
         pretty << testStr << " List bottom is not a sublist.";
         writeDiagnostics(__FILE__, pretty.str());
      }
   }

   /*******************************************************
    * Verify no action when two sublists point to the same*
    * header.                                             *
    ******************************************************/
   if (flag) {
      string testStr("test 5" + name);
      SlipSublist* sublist = new SlipSublist(*sublistHead);
      SHORT headerRefCntBefore = header->getRefCount();

      refCntBefore = sublistHead->getRefCount();
      &header->replaceBot(*sublist);
      flag = (checkSublistDelete(testStr, sublistHead, refCntBefore))? flag: false;

      SHORT headerRefCntAfter = header->getRefCount();

      if (!header->getBot().isSublist()) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, header->getBot().toString());
         pretty << testStr << " List bottom is not a sublist.";
         writeDiagnostics(__FILE__, pretty.str());
      } else if (headerRefCntBefore != headerRefCntAfter) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, header->dump());
         pretty << testStr << " Header reference count not updated correctly";
         writeDiagnostics(__FILE__, pretty.str());
      }

      refCntBefore = sublistHead->getRefCount();
      delete sublist;
      flag = (checkSublistDelete(testStr, sublistHead, refCntBefore))? flag: false;
    }

   /*******************************************************
    * Verify a new header replaces a sublist              *
    ******************************************************/
   if (flag) {
      string testStr("test 6" + name);
      SlipHeader* newHeader = new SlipHeader();
      SHORT headerRefCntBefore = newHeader->getRefCount();

      refCntBefore = sublistHead->getRefCount();
      &header->replaceBot(*newHeader);
      flag = (checkSublistDelete(testStr, sublistHead, (refCntBefore - 1)))? flag: false; // Existing sublist header
      flag = (checkSublistInsert(testStr, newHeader, headerRefCntBefore))? flag: false;

      if (!header->getBot().isSublist()) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, header->getBot().toString());
         pretty << testStr << " List bottom is not a sublist.";
         writeDiagnostics(__FILE__, pretty.str());
      }
      newHeader->deleteList();
    }

   header->flush();
   sublistHead->flush();

   refCntBefore = header->getRefCount();
   header->deleteList();
   flag = (checkHeaderDelete("delete header", header, refCntBefore))? flag: false;

   refCntBefore = sublistHead->getRefCount();
   sublistHead->deleteList();
   flag = (checkHeaderDelete("delete sublistHead", sublistHead, refCntBefore))? flag: false;

   delete &copy;
   if (!flag)
      testResultOutput("Test Replace Sublist Bot " + name, 0.0, flag);
   return flag;
}; // bool testReplaceSublistBot(SlipDatum* datum)

bool testReplaceSublistTop(SlipDatum* datum) {
   stringstream pretty;
   bool flag = true;
   SHORT refCntBefore;
   static string start = "Start";
   static string sublistText = "Sublist";
   SlipHeader* header = new SlipHeader();
   SlipHeader* sublistHead = new SlipHeader();
   SlipDatum& copy = *new SlipDatum(*datum);
   string name = "  for '" + datum->toString() + "'";

   header->push((string*)&start);
   header->push((SlipDatum&)*datum);
   sublistHead->push((string*)&sublistText);

   /*******************************************************
    * Verify data on list top                             *
    ******************************************************/
   if (!header->getTop().isData()) {
      string testStr("test[ 1]" + name);
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, header->getTop().dump());
      pretty << testStr << " List top should be Data ";
      writeDiagnostics(__FILE__, pretty.str());
   } else if ((header->getTop()).toString() != datum->toString()) {
      string testStr("test[ 1]" + name);
      flag = false;
      pretty << testStr << "should be "
             << datum->toString()
             << " is " << header->getTop().toString();
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }

   /*******************************************************
    * Verify a sublist can replace a datum cell           *
    ******************************************************/
   if (flag) {
      string testStr("test[ 2]" + name);
      refCntBefore = sublistHead->getRefCount();
      &header->replaceTop((SlipHeader&)*sublistHead);
      flag = (checkSublistInsert(testStr, sublistHead, refCntBefore))? flag: false;

      if (!header->getTop().isSublist()) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, header->getTop().toString());
         pretty << testStr << " List top should be a sublist.";
         writeDiagnostics(__FILE__, pretty.str());
      }
   }

   /*******************************************************
    * Verify a a datum cell can replace a sublist         *
    ******************************************************/
   if (flag) {
      string testStr("test[ 3]" + name);
      refCntBefore = sublistHead->getRefCount();
      &header->replaceTop(copy);
      flag = (checkSublistDelete(testStr, sublistHead, refCntBefore))? flag: false;

      if (!header->getTop().isData()) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, header->getTop().dump());
         pretty << testStr << " List top is not data";
         writeDiagnostics(__FILE__, pretty.str());
      } else if ((header->getTop()).toString() != copy.toString()) {
         flag = false;
         pretty << testStr
                << " List top value should be " << copy.toString()
                << " is " << header->getTop().toString();
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
   }

   /*******************************************************
    * Verify a header is converted to a sublist and       *
    * replaces a datum cell and no action when a header   *
    * attempts to replace a sublist pointing to the header*
    ******************************************************/
   if (flag) {
      string testStr("test[ 4]" + name);

      refCntBefore = sublistHead->getRefCount();
      &header->replaceTop(*sublistHead);
      flag = (checkSublistInsert(testStr, sublistHead, refCntBefore))? flag: false;

      refCntBefore = sublistHead->getRefCount();
      &header->replaceTop(*sublistHead);
      flag = (checkSublistDelete(testStr, sublistHead, refCntBefore))? flag: false;

      if (!header->getTop().isSublist()) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, header->getTop().toString());
         pretty << testStr << " List top is not a sublist";
         writeDiagnostics(__FILE__, pretty.str());
      }
   }

   /*******************************************************
    * Verify no action when two sublists point to the same*
    * header.                                             *
    ******************************************************/
   if (flag) {
      string testStr("test[ 5]" + name);
      SlipSublist* sublist = new SlipSublist(*sublistHead);
      SHORT headerRefCntBefore = header->getRefCount();

      refCntBefore = sublistHead->getRefCount();
      &header->replaceTop(*sublist);
      flag = (checkSublistDelete(testStr, sublistHead, refCntBefore))? flag: false;

      SHORT headerRefCntAfter = header->getRefCount();

      if (!header->getTop().isSublist()) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, header->getTop().toString());
         pretty << testStr << " List top is not a sublist";
         writeDiagnostics(__FILE__, pretty.str());
      } else if (headerRefCntBefore != headerRefCntAfter) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, header->getTop().toString());
         pretty << testStr
                << " Header reference count not updated correctly";
         writeDiagnostics(__FILE__, pretty.str());
      }

      refCntBefore = sublistHead->getRefCount();
      delete sublist;
      flag = (checkSublistDelete(testStr, sublistHead, refCntBefore))? flag: false;
   }

   /*******************************************************
    * Verify a new header replaces a sublist              *
    ******************************************************/
   if (flag) {
      string testStr("test[ 6]" + name);
      SlipHeader* newHeader = new SlipHeader();
      SHORT headerRefCntBefore = newHeader->getRefCount();

      refCntBefore = sublistHead->getRefCount();
      &header->replaceTop(*newHeader);
      flag = (checkSublistDelete(testStr, sublistHead, (refCntBefore - 1)))? flag: false; // existing sublist header deleted
      flag = (checkSublistInsert(testStr, newHeader, headerRefCntBefore))? flag: false;

      if (!header->getTop().isSublist()) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, header->getTop().toString());
         pretty << testStr << " List top is not a sublist";
         writeDiagnostics(__FILE__, pretty.str());
      }
      newHeader->deleteList();
   }

   header->flush();
   sublistHead->flush();

   refCntBefore = header->getRefCount();
   header->deleteList();
   flag = (checkHeaderDelete("delete header", header, refCntBefore))? flag: false;

   refCntBefore = sublistHead->getRefCount();
   sublistHead->deleteList();
   flag = (checkHeaderDelete("delete sublistHead", sublistHead, refCntBefore))? flag: false;

   delete &copy;
   if (!flag)
      testResultOutput("Test Replace Sublist Top " + name, 0.0, flag);
   return flag;
}; // bool testReplaceSublistTop(SlipDatum* datum)

bool testReplace() {
   high_resolution_clock::time_point start =  high_resolution_clock::now();
   enum { MAX_SIZE = 8 };
   bool      flag = true;
   string    test[MAX_SIZE]  =  { "true"
                                , "a"
                                , "2.0000000"
                                , "123"
                                , "Pointer"
                                , "String"
                                , "A"
                                , "15"
                                };

   flag = (testReplaceTop(test))? flag: false;
   flag = (testReplaceBot(test))? flag: false;
   flag = (testReplaceSublist())? flag: false;
   
   high_resolution_clock::time_point end =  high_resolution_clock::now();
   duration<double> seconds = duration_cast<duration<double>>(end - start);
   testResultOutput("Test Replace", seconds.count(), flag);

   return flag;
}; // bool testReplace()
