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
 * @file    TestGlobal.cpp
 * @author  A. Schwarz
 * @date November 19, 2012
 *
 * @brief Test framework global information implementation file.
 *
 */

# include <iomanip>
# include <iostream>
# include <sstream>

# include "TestGlobal.h"
# include <stdlib.h>
# include "../libgslip/SlipDatum.h"
# include "TestUtilities.h"

ofstream logFile;                           //!< TAP log file
const string  passFailStr[]    = { "FAIL", "PASS" };
const string* str              = new string("String");
const void*   ptr              = (void*)str;
const string  name[]           = { "boolDatum"
                                 , "charDatum"
                                 , "doubleDatum"
                                 , "longDatum"
                                 , "ptrDatum"
                                 , "StringDatum"
                                 , "StringDatum"
                                 , "unsignedCharDatum"
                                 , "unsignedLongDatum"
                                 }; // name[]
const string resultStr[]       = { "true"
                                 , "a"
                                 , "2.0000000"
                                 , "123"
                                 , "Pointer"
                                 , "4456"
                                 , "String"
                                 , "A"
                                 , "15"
                                 };
SlipDatum* data[DATASIZE];

bool Intrinsics::compare(SlipCellBase* X) {
   static const string sClassType[eSENTINEL] = { "UNDEFINED", "BOOL", "CHAR",  "DOUBLE",  "HEADER"
                                               , "LONG",      "PTR",  "READER", "STRING", "SUBLIST"
                                               , "UCHAR",     "ULONG"
                                               };
   stringstream message;
   stringstream pretty;
   bool flag = true;
   if (X->getClassType() != classType) {
      pretty  << title << " getClassType() actual "
              << sClassType[X->getClassType()]
              << " expected " << sClassType[classType]
              << endl;
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
      flag = false;
   }
   if (X->getName() != name) {
      flag = false;
      pretty << title << " getName() actual " << X->getName() << " expected " << name << endl;
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }
   if (X->isData() != isDataFlag) {
      flag = false;
      pretty << title << " isData() actual " << X->isData() << " expected " << isDataFlag << endl;
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }
   if (X->isHeader() != isHeaderFlag) {
      flag = false;
      pretty << title << " isHeader() actual " << X->isHeader() << " expected " << isSublistFlag << endl;
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }
   if (X->isNumber() != isNumberFlag) {
      flag = false;
      pretty << title << " isNumber() actual " << X->isNumber() << " expected " << isNumberFlag << endl;
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }
   if (X->isSublist() != isSublistFlag) {
      flag = false;
      pretty << title << " isSublist() actual " << X->isSublist() << " expected " << isSublistFlag << endl;
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
   }
   if (!flag)
      testResultOutput(title, 0.0, false);
   return flag;
}; // bool Intrinsics::compare(SlipCellBase* X)
