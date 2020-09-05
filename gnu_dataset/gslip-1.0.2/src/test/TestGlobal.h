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
 * @file    TestGlobal.h
 * @author  A. Schwarz
 * @date November 19, 2012
 *
 * @brief Test framework global information.
 */

#ifndef TESTGLOBAL_H
#define	TESTGLOBAL_H

# include <stdlib.h>
# include <ostream>
# include <fstream>
# include "../libgslip/SlipDatum.h"

using namespace std;

enum { INITIALALLOCATION = 25                           //!< Initial heap allocation
     , DELTAALLOCATION   = 25                           //!< Incremental heap allocation
     , DATASIZE          =  9
     };
     
enum { TEST_SUCCESS      =  0                           //!< Test succeeded: for make test harness
     , TEST_SKIPPED      = 77                           //!< Test skipped: for make test harness
     , TEST_ERROR        = 00                           //!< Test error: for make test harness
     };     
     
class Intrinsics {
public:
   const string    title;                               //!< diagnostic message source
   const ClassType classType;                           //!< return the cell subtype
   const ULONG     ID;                                  //!< return Cell ID
   const string    name;                                //!< string representing SlipDatumCell
   const ULONG     cellType;                            //!< class number
   const bool      isDataFlag;                          //!< true if cell is data
   const bool      isHeaderFlag;                        //!< true if cell is a header
   const bool      isNumberFlag;                        //!< true if cell is numeric data
   const bool      isSublistFlag;                       //!< true if cell is sublist

public:
   Intrinsics( string title                             //!< test name
             , ClassType classType                      //!< expected class type
             , ULONG ID                                 //!< expected ID
             , ULONG cellType                           //!< expected cell type
             , string name                              //!< expected type name
             , bool isDataFlag                          //!< expected data flag value
             , bool isHeaderFlag                        //!< expected header flag value
             , bool isNumberFlag                        //!< exptected number flag value
             , bool isSublistFlag)                      //!< expected sublist flag value
             : name(name),             cellType(cellType), ID(ID), classType(classType)
             , isDataFlag(isDataFlag), isHeaderFlag(isHeaderFlag), isNumberFlag(isNumberFlag)
             , isSublistFlag(isSublistFlag), title(title)
             {}; 
   bool compare(SlipCellBase* X);
}; //  class Intrinsics
 
     
struct TestCase {
   SlipCellBase* X;
   Intrinsics    test;
}; // struct TestCase

extern const string  passFailStr[];
extern const string* str;
extern const void*   ptr;
extern const string  name[];
extern const string  resultStr[];
extern SlipDatum*    data[DATASIZE];
extern       ofstream logFile;                           //!< TAP log file

#endif	/* TESTGLOBAL_H */

