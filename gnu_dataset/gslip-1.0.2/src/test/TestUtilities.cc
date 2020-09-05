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
 * @file    TestUtilities.cpp
 * @author  A. Schwarz
 * @date November 19, 2012
 *
 * @brief Implementation of general utility functions and classes used in testing.
 *
 */

# include <iomanip>
# include <iostream>
# include <sstream>

# include "TestUtilities.h"
# include "TestGlobal.h"
# include "../libgslip/SlipCell.h"
# include "../libgslip/SlipCellBase.h"
# include "../libgslip/SlipDef.h"
# include "../libgslip/SlipDatum.h"
# include "../libgslip/SlipSequencer.h"

using namespace slip;
using namespace std;

bool checkSublistInsert(string str, SlipHeader* head, USHORT before) {
   bool flag = true;
   USHORT after = head->getRefCount();
   if (after != (before + 1)) {
      stringstream pretty;
      flag = false;
      logFile << "  " << str << " reference count error " 
              << after << " should be " << (before + 1) << endl;
      logFile << " " << head->dump() << endl;
   }
   return flag;
}; // bool checkSublistInsert(string str, SlipHeader* head, USHORT before)

bool checkSublistDelete(string str, SlipHeader* head, USHORT before) {
   bool flag = true;
   USHORT after = head->getRefCount();
   if (after != before) {
      flag = false;
      logFile << "  " << str << " reference count error " 
              << after << " should be " << before << endl;
      logFile << "  " << head->dump() << endl;
   }
   return flag;
}; // bool checkSublistDelete(string str, SlipHeader* head, USHORT before)

PTR constructConstPtr() {
   return new Pointer();
}; // string constructConstPtr()

/*****************************************************
 * Compare two lists for identity.
 * 1. Each list contains data only.
 * 2. The lists are identical if the string output of
 *    all cells are identical.
 * 3. Sublists are not allowed.
 * 4. toString is used (here) to avoid using the 
 *    overloaded '==' operator. '==' is checked
 *    elsewhere.
 ****************************************************/ 
bool compareLists(SlipHeader* list1, SlipHeader* list2) {
   SlipCellBase* link1 = list1->getRightLink();
   SlipCellBase* link2 = list2->getRightLink();
   for(; !(link1->isHeader() || link2->isHeader())
       ; link1 = link1->getRightLink()
       , link2 = link2->getRightLink()) {
      string str1 = link1->toString();
      string str2 = link2->toString();
      if (str1 != str2) {
         logFile << "list1"; printList(list1);
         logFile << "list2"; printList(list2);
         return false;
      }
   }
   /**************************************
    * If the termination cell for both
    * lists is not a header cell then the
    * lists are not of equal size
    **************************************/
   if (!(link1->isHeader() && link2->isHeader())) {
      logFile << "     list1";
      printList(list1);
      logFile << "     list2";
      printList(list2);
      if (link1->isHeader()) logFile << "     second list is shorter than first" << endl;
      else                   logFile << "     first list is shorter than second" << endl;
   }
   return true;
}; //bool compareLlsts(SlipHeader* list1, SlipHeader&*list2)

/*****************************************************
 * Compare two lists for identity.
 * 1. Each list contains data only.
 * 2. The lists are identical if the string output of
 *    all cells are identical.
 * 3. Sublists are not allowed.
 * 4. toString is used (here) to avoid using the 
 *    overloaded '==' operator. '==' is checked
 *    elsewhere.
 * 5. The lists may be of unequal size. The comparison
 *    stops if either or both lists have reached a
 *    header or if the comparison size is exceeded.
 ****************************************************/ 

bool compareLists(SlipHeader* list1, SlipHeader* list2, int size) {
   SlipCellBase* link1 = list1->getRightLink();
   SlipCellBase* link2 = list2->getRightLink();
   int i;
   for(i = 0
       ; !(link1->isHeader() || link2->isHeader()) && (i < size)
       ; link1 = link1->getRightLink()
       , link2 = link2->getRightLink()
       , i++) {
      string str1 = link1->toString();
      string str2 = link2->toString();
      if (str1 != str2) {
         logFile << "     Values don't match [" << i << "} " << str1 <<  " != " << str2 << endl;
         logFile << "list1"; printList(list1);
         logFile << "list2"; printList(list2);
         return false;
      }
   }
   /*******************************************
    * If the number of cells compared before a
    * header cell is seen is less that the
    * required comparison size then the comparison
    * fails.
    *******************************************/
   if (i < size) {
      logFile << "     ";
      printList(list1);
      logFile << "     ";
      printList(list2);
      logFile << "     Lists are of unequal size ";
      if (link1->isHeader()) logFile << "first list is shorter" << endl;
      else                   logFile << "second list is shorter" << endl;
      return false;
   }
   return true;
}; //bool compareLlsts(SlipHeader* list1, SlipHeader&*list2, size)

/*****************************************************
 * The Slip operational state is compared. The 
 * operational state is defined in a Slip Private
 * structure, the Slip State. The comparison is 
 * against user provided criteria.
 * 
 * This test is used to check that allocation / 
 * deallocation is correct.
 ****************************************************/ 
bool checkSlipState(string message, ULONG total, ULONG avail) {
   bool flag = true;
   SlipState state = SlipCellBase::getSlipState();
   flag = (state.total == total)?             flag: false;
   flag = (state.avail >= avail)?             flag: false;
   flag = (state.alloc == INITIALALLOCATION)? flag: false;
   flag = (state.delta == DELTAALLOCATION)?   flag: false;
   if (!flag) 
      logFile << message << " FAIL\n"
              << "   Total Allocation is   " << state.total << " should be " << total << endl
              << "   Available Space is    " << state.avail << " should be " << avail << endl
              << "   Initial Allocation is " << state.alloc << " should be " << INITIALALLOCATION << endl
              << "   Delta Allocation is   " << state.delta << " should be " << INITIALALLOCATION << endl;
   return flag;
}; // bool checkSlipState(SlipState State, ULONG total, ULONG avail)

/*****************************************************
 * Constrict an array of known (fixed) data for usage
 * in testing. The data is created and a pointer to
 * the data provided in a global array.
 ****************************************************/ 

bool constructData() {
   SlipState state = SlipCellBase::getSlipState();
   data[0]     = new SlipDatum(true);
   data[1]     = new SlipDatum((CHAR)'a');
   data[2]     = new SlipDatum(2.0);
   data[3]     = new SlipDatum((LONG)123);
   data[4]     = new SlipDatum(new Pointer());
   data[5]     = new SlipDatum((string)"4456");
   data[6]     = new SlipDatum(str);
   data[7]     = new SlipDatum((UCHAR) 'A');
   data[8]     = new SlipDatum((ULONG) 15);
   return checkSlipState("Data Construction Failed", state.total, state.avail - 9);
}; // void constructData()

/*****************************************************
 * Delete the fixed and known array of data used for
 * testing.
 ****************************************************/ 
void deleteData() {
   for (unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) 
      if (!data[i]->isDeleted()) delete data[i];
}; // void deleteData()

/**
 * @fn printList
 * @brief Output a list to logFile;
 * <p>Formats and outputs a list to logFile. This is for Utility use and does
 *    not output the list descriptor list or integer size indications (UC, C,
 *    UL, L). To be used for small lists.</p>
 * <p>This method uses the most significant bit of the header mark to check
 *    for cyclic lists. This bit can not be set by the user.</p>
 * @param[in] list (SlipHeader*) list pointer
 */
void printList(SlipHeader* list) {
   stringstream pretty;
   enum { CYCLEBIT = 0x8000 };
   if (!list->isHeader()) {
      pretty << "List is not a list. Sorry";
   } else if ((list->getMark() && CYCLEBIT)) {
      pretty << "cyclic graph detedtec";
   } else {
      list->putMark(list->getMark() || CYCLEBIT);
      pretty << "( ";
      if (list->isDList()) {
         pretty << "<>";
      }
      SlipSequencer* seq = new SlipSequencer(*list);
      while(!(seq->advanceLWR()).isHeader()) {
         if (seq->currentCell().isData()) {
            pretty << ((SlipDatum&)seq->currentCell()).toString() << ' ';
         } else printList(&(SlipHeader&)(seq->currentCell()));
      }
      pretty << ")";
      list->putMark(list->getMark() && ~CYCLEBIT);
   }
   writeDiagnostics(__FILE__, __LINE__, pretty.str());
}; // printList(SlipHeader* list)

/**
 * @fn testResultOutput
 * @brief Output test results to stdout using the TAP protocol
 */
void testResultOutput(string test, double seconds, const bool flag, bool nl) {
   const string fp[] = {":test-result: FAIL", ":test-result: PASS" };
   const string passFailTAPStr[] = {"not ok ", "ok " };
   stringstream pretty;
   if (nl) pretty << '\n';
   pretty << left << passFailTAPStr[flag] << left << setw(25) << setfill(' ') << test 
          << "  " << setprecision(5) << seconds*1000. << " ms" << endl;
   cout << pretty.str();    cout.flush();
//   logFile << pretty.str(); logFile.flush();
}; // void testResultOutput(string test, bool flag, bool nl = false)

/**
 * @fn writeDiagnostics(int lineno, string filename, string str)
 * @brief Outputs a TAP Diagnostic statement containing an error
 * @param filename filename where error occurred
 * @param msg diagnostic message
 */
void writeDiagnostics(string filename, string msg) {
   stringstream pretty;
   ios_base::fmtflags flags = logFile.flags();
   int width = 9 + filename.length();
   pretty << "# " << setw(width) << ' ' << msg << endl;
   logFile << pretty;
   logFile.setf(flags);
} // writeDiagnostics(int lineno, string filename, string msg)

/**
 * @fn writeDiagnostics(int lineno, string filename, string str)
 * @brief Outputs a TAP Diagnostic statement containing an error
 * @param lineno line number where error occurred
 * @param filename filename where error occurred
 * @param msg diagnostic message
 */
void writeDiagnostics(string filename, int lineno, string msg) {
   stringstream pretty;
   ios_base::fmtflags flags = logFile.flags();
   pretty << "# " << setw(6) << setfill(' ') << right << lineno << right
          << ":" << filename << ":  " << msg << endl;
   logFile << pretty;
   logFile.setf(flags);
} // writeDiagnostics(int lineno, string filename, string msg)
