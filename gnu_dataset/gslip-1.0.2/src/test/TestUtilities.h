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
 * @file    TestUtilities.h
 * @author  A. Schwarz
 * @date November 19, 2012
 *
 * @brief General utility functions and classes used in testing.
 * 
 */

#ifndef TESTUTILITIES_H
#define	TESTUTILITIES_H

# include <iomanip>
# include <sstream>
# include <string>
# include <ctime>
# include <chrono>

# include "../libgslip/SlipDef.h"
# include "../libgslip/SlipHeader.h"
# include "../libgslip/SlipPointer.h"

using namespace slip;
using namespace std;
using namespace std::chrono;

static int counter = 0;

static SlipDatum* pointerData;

class Pointer : public SlipPointer {
   int ctr;
   const string name;
public:
      Pointer() : SlipPointer(new string("PTR")), ctr(++counter) {  }           // constructor
      Pointer(int val) : SlipPointer(), ctr(val) {  }                           // constructor
      virtual ~Pointer() { }
      virtual Pointer* copy( )        {return new Pointer(); }                  // Copy 
      virtual Parse    getParse() { return &parse; };                           // pointer to parser
      static  SlipDatum& parse(SlipHeader& head) { return *pointerData; };      // input parser
      virtual void     remove() const { delete this; }
      virtual string   dump() const {                                           // uglify contents
         stringstream pretty;
         pretty << "Pointer " << ctr;
         return pretty.str();
      }
      virtual string toString() const {                                         // pretty-print contents
         return "Pointer";
      }
      virtual string write() const {
         stringstream pretty;
         pretty << "( \"" << toString() << "\" )";
         return pretty.str();
      }; 
}; // class Pointer : SlipPointer

extern bool checkSlipState(string message, ULONG total, ULONG avail);
extern bool checkSublistDelete(string str, SlipHeader* head, USHORT before);
extern bool checkSublistInsert(string str, SlipHeader* head, USHORT before);
extern bool compareLists(SlipHeader* list1, SlipHeader* list2);
extern bool compareLists(SlipHeader* list1, SlipHeader* list2, int size);
extern PTR  constructConstPtr();
extern bool constructData();
extern void deleteData();
extern void printList(SlipHeader* list);
extern void testResultOutput(string test, double seconds, const bool flag, bool nl = false);
extern void writeDiagnostics(string filename, string msg);
extern void writeDiagnostics(string filename, int lineno, string msg);

#endif	/* TESTUTILITIES_H */

