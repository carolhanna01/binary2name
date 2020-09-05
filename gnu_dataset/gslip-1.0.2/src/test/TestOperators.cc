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
 * @file    TestOperators.cpp
 * @author  A. Schwarz
 * @date    December 26, 2012
 *
 * @brief Test SlipDatum operation implementation file.
 *
 */
# include <cmath>
# include <iostream>
# include <iomanip>
# include <sstream>
# include <typeinfo>

# include "../libgslip/Slip.h"
# include "../libgslip/SlipSublist.h"
# include "TestOperators.h"
# include "TestGlobal.h"
# include "TestUtilities.h"

using namespace std;
using namespace slip;

const DOUBLE epsilon = 1.0E-13;     // allowable error in a calculation

bool findType(type_info* info) {
   struct type {
      type_info* typeInfo;
      string     name;
   };
   static type types[] = { {const_cast<type_info*>(&typeid(bool)),           " bool"  }
                         , {const_cast<type_info*>(&typeid(CHAR)),           " CHAR"  }
                         , {const_cast<type_info*>(&typeid(UCHAR)),          " UCHAR" }
                         , {const_cast<type_info*>(&typeid(short)),          " SHORT" }
                         , {const_cast<type_info*>(&typeid(unsigned short)), " USHORT"}
                         , {const_cast<type_info*>(&typeid(int)),            " INT"   }
                         , {const_cast<type_info*>(&typeid(unsigned int)),   " UINT"  }
                         , {const_cast<type_info*>(&typeid(LONG)),           " LONG"  }
                         , {const_cast<type_info*>(&typeid(ULONG)),          " ULONG" }
                         , {const_cast<type_info*>(&typeid(DOUBLE)),         " DOUBLE"}
                         };
   for(unsigned int i = 0; i < sizeof(types)/sizeof(types[0]); i++ )
      if (types[i].typeInfo == info) return true;
   return false;
}; // bool findType(type_info* info)

bool outputOperatorFailure( SlipDatum& data   // lhs value after operation: lhs <op> test
                          , SlipDatum& test   // rhs of operation: lhs <op> rhs
                          , string testName   // operation being tested (+=, -=, *=, /=, %=, &=, |=, ^=)
                          , string dataType   // type of lhs (CHAR, UCHAR, LONG, ULONG, BOOL, DOUBLE)
                          , string testType   // type of Rhs (CHAR, UCHAR, LONG, ULONG, BOOL, DOUBLE)
                          , long i            // lhs[i]
                          , long j            // rhs[i][j]
                          , LONG ans) {       // expected answer
   stringstream pretty;
   pretty  << testName << " failure: SlipDatum((" << setw(5) << dataType << ") "
           << (LONG)data << ")[" << i << "] " << testName << " "
           << "SlipDatum((" << setw(5) << testType << ") " << (LONG)test
           << ")[" << j << "][0]  "
           << (LONG)data << " != " << ans << endl;
   writeDiagnostics(__FILE__, __LINE__, pretty.str());
   return false;
}; // bool outputOperatorFailure(SlipDatum&, SlipDatum&, string, string, string, long, long, long)
bool outputOperatorFailure( SlipDatum& data   // lhs value after operation: lhs <op> test
                          , DOUBLE     test   // rhs of operation: lhs <op> rhs
                          , string  testName  // operation being tested (+=, -=, *=, /=, %=, &=, |=, ^=)
                          , string  dataType  // type of lhs (CHAR, UCHAR, LONG, ULONG, BOOL, DOUBLE)
                          , string  testType  // type of Rhs (CHAR, UCHAR, LONG, ULONG, BOOL, DOUBLE)
                          , double  ans) {    // expected answer
   stringstream pretty;
   pretty  << testName << " failure: (" << setw(5) << dataType << ") "
           << data.toString() << " " << testName << " "
           << "(" << testType << ") " << test << " "
           << data.toString() << " != " << ans << endl;
   writeDiagnostics(__FILE__, __LINE__, pretty.str());
   return false;
}; //
bool outputOperatorFailure( SlipDatum& data   // lhs value after operation: lhs <op> test
                          , LONG       test   // rhs of operation: lhs <op> rhs
                          , string testName   // operation being tested (+=, -=, *=, /=, %=, &=, |=, ^=)
                          , string dataType   // type of lhs (CHAR, UCHAR, LONG, ULONG, BOOL, DOUBLE)
                          , string testType   // type of Rhs (CHAR, UCHAR, LONG, ULONG, BOOL, DOUBLE)
                          , long i            // lhs[i]
                          , long j            // lhs[i] <op> rhs[j]
                          , LONG ans) {       // expected answer
   stringstream pretty;
   pretty  << "operator" << testName << " failure: SlipDatum((" << setw(5) << dataType << ") "
           << (LONG)data << ")[" << i << "] " << testName << " "
           << "SlipDatum((" << setw(5) << testType << ") " << (LONG)test
           << ")["  << j << "]  "
           << (LONG)data << " != " << ans << endl;
   writeDiagnostics(__FILE__, __LINE__, pretty.str());
   return false;
}; // bool outputOperatorFailure(SlipDatum&, SlipDatum&, string, string, string, long, long, long)
bool outputOperatorFailure( DOUBLE     data   // lhs value after operation: lhs <op> test
                          , LONG       test   // rhs of operation: lhs <op> rhs
                          , string testName   // operation being tested (+=, -=, *=, /=, %=, &=, |=, ^=)
                          , string dataType   // type of lhs (CHAR, UCHAR, LONG, ULONG, BOOL, DOUBLE)
                          , string testType   // type of Rhs (CHAR, UCHAR, LONG, ULONG, BOOL, DOUBLE)
                          , long j            // lhs[i] <op> rhs[j]
                          , DOUBLE ans) {     // expected answer
   stringstream pretty;
   pretty  << testName << " failure: SlipDatum((" << setw(5) << dataType << ") "
           << data << ") " << testName << " "
           << "SlipDatum((" << setw(5) << testType << ") " << (long)test
           << ")["  << j << "]  "
           << data << " != " << ans << endl;
   writeDiagnostics(__FILE__, __LINE__, pretty.str());
   return false;
}; // bool outputOperatorFailure(SlipDatum&, SlipDatum&, string, string, string, long, long, long)
bool outputOperatorFailure( SlipDatum& data1  // operand1
                          , SlipDatum& data2  // operand2
                          , string testName   // operation being tested
                          , LONG value        // value of data1 <op> data2
                          , LONG ans          // expected answer
                          , long i            //
                          , long j ) {        //
   stringstream pretty;
   pretty  << "operator" << testName << " failure: "
           << "SlipDatum((" << setw(5) << data1.getName() << ")" << data1.toString() << ") "
           << testName << " "
           << "SlipDatum((" << setw(5) << data2.getName() << ")" << data2.toString() << ") "
           << value << " != " << ans << "[" << i << "][" << j << "]" << endl;
   writeDiagnostics(__FILE__, __LINE__, pretty.str());
   return false;
}; // bool outputOperatorFailure(SlipDatum&, SlipDatum&, string, long, long, long, long)
bool outputOperatorFailure( SlipDatum& data1  // operand1
                          , LONG       data2  // operand2
                          , string testName   // operation being tested
                          , string discreteType // string representing discrete type
                          , LONG value        // value of data1 <op> data2
                          , LONG ans          // expected answer
                          , long i            //
                          , long j ) {        //
   stringstream pretty;
   pretty  << "operator" << testName << " failure: "
           << "SlipDatum((" << setw(5) << data1.getName() << ")" << data1.toString() << ") "
           << testName << " (" << discreteType << ") " << data2 << "    "
           << setw(3) << right << value << " != "
           << ans << "[" << i << "][" << j << "]" << endl;
   writeDiagnostics(__FILE__, __LINE__, pretty.str());
   return false;
}; // bool outputOperatorFailure(SlipDatum&, SlipDatum&, string, long, long, long, long)
bool outputOperatorFailure( long       data1  // operand1
                          , SlipDatum& data2  // operand2
                          , string testName   // operation being tested
                          , string discreteType // string representing discrete type
                          , LONG value        // value of data1 <op> data2
                          , LONG ans          // expected answer
                          , long i            //
                          , long j ) {        //
   stringstream pretty;
   pretty  << "operator" << testName << " failure: "
           << "(" << discreteType << ") " << setw(3) << right << data1 << "[" << j << "] "
           << testName << " "
           << "SlipDatum((" << setw(5) << left << data2.getName() << ")"
           << data2.toString() << ")[" << i << "] "
           << setw(3) << right << value << " != "
           << ans << "[" << i << "][" << j << "]" << endl;
   writeDiagnostics(__FILE__, __LINE__, pretty.str());
   return false;
}; // bool outputOperatorFailure(SlipDatum&, SlipDatum&, string, long, long, long, long)
bool outputOperatorFailure( SlipDatum& data1  // operand1
                          , SlipDatum& data2  // operand2
                          , string testName   // operation being tested
                          , SlipDatum& value  // value of SlipDatum(data1 <op> data2)
                          , LONG ans          // expected answer
                          , long i            //
                          , long j ) {        //
   stringstream pretty;
   pretty  << "operator" << testName << " failure: "
           << "SlipDatum("
           << "SlipDatum(("  << setw(5) << data1.getName() << ")" << data1.toString() << ")[" << i << "} "
           << " " << testName << " "
           << "SlipDatum(("  << setw(5) << data2.getName() << ")" << data2.toString() << ")[" << j << "])"
           << "   " << setw(4) << value.toString() << " != "
           << setw(4) << ans << "[" << i << "][" << j << "}" << endl;
   writeDiagnostics(__FILE__, __LINE__, pretty.str());
   return false;
}; // bool outputOperatorFailure(SlipDatum&, SlipDatum&, string, long, long, long, long)

bool testArithmeticDiscreteOperators() {
   bool flag = true;
   static const LONG add[][6] = {
 //         true  false  CHAR UCHAR  LONG ULONG
/*true */  {   2,    1,    6,    8,   18,   20}  //  1
/*false*/, {   1,    0,    5,    7,   17,   19}  //  0
/*CHAR */, {   6,    5,   10,   12,   22,   24}  //  5
/*UCHAR*/, {   8,    7,   12,   14,   24,   26}  //  7
/*LONG */, {  18,   17,   22,   24,   34,   36}  // 17
/*ULONG*/, {  20,   19,   24,   26,   36,   38}  // 19
};
   static const LONG sub[][6] = {
 //         true  false  CHAR UCHAR  LONG ULONG
/*true */  {   0,    1,   -4,   -6,  -16,  -18}  //  1
/*false*/, {  -1,    0,   -5,   -7,  -17,  -19}  //  0
/*CHAR */, {   4,    5,    0,   -2,  -12,  -14}  //  5
/*UCHAR*/, {   6,    7,    2,    0,  -10,  -12}  //  7
/*LONG */, {  16,   17,   12,   10,    0,   -2}  // 17
/*ULONG*/, {  18,   19,   14,   12,    2,    0}  // 19
};
   static const LONG mul[][6] = {
 //         true  false  CHAR UCHAR  LONG ULONG
/*true */  {   1,    0,    5,    7,   17,   19}  //  1
/*false*/, {   0,    0,    0,    0,    0,    0}  //  0
/*CHAR */, {   5,    0,   25,   35,   85,   95}  //  5
/*UCHAR*/, {   7,    0,   35,   49,  119,  133}  //  7
/*LONG */, {  17,    0,   85,  119,  289,  323}  // 17
/*ULONG*/, {  19,    0,   95,  133,  323,  361}  // 19
};
   static const LONG div[][6] = {
 //         true  false  CHAR UCHAR  LONG ULONG
/*true */  {   1,    0,    0,    0,    0,    0}  //  1
/*false*/, {   0,    0,    0,    0,    0,    0}  //  0
/*CHAR */, {   5,    0,    1,    0,    0,    0}  //  5
/*UCHAR*/, {   7,    0,    1,    1,    0,    0}  //  7
/*LONG */, {  17,    0,    3,    2,    1,    0}  // 17
/*ULONG*/, {  19,    0,    3,    2,    1,    1}  // 19
};
   static const LONG mod[][6] = {
 //         true  false  CHAR UCHAR  LONG ULONG
/*true */  {   0,    0,    1,    1,    1,    1}  //  1
/*false*/, {   0,    0,    0,    0,    0,    0}  //  0
/*CHAR */, {   0,    0,    0,    5,    5,    5}  //  5
/*UCHAR*/, {   0,    0,    2,    0,    7,    7}  //  7
/*LONG */, {   0,    0,    2,    3,    0,   17}  // 17
/*ULONG*/, {   0,    0,    4,    5,    2,    0}  // 19
};


    static const LONG badd[][6] = {
 //         true  false  true  true  true true
/*true */  {   2,    1,    2,    2,    2,    2}  //  1
/*false*/, {   1,    0,    1,    1,    1,    1}  //  0
/*CHAR */, {   6,    5,    6,    6,    6,    6}  //  5
/*UCHAR*/, {   8,    7,    8,    8,    8,    8}  //  7
/*LONG */, {  18,   17,   18,   18,   18,   18}  // 17
/*ULONG*/, {  20,   19,   20,   20,   20,   20}  // 19
};
   static const LONG bsub[][6] = {
 //         true  false  true  true  true true
/*true */  {   0,    1,    0,    0,    0,    0}  //  1
/*false*/, {  -1,    0,   -1,   -1,   -1,   -1}  //  0
/*CHAR */, {   4,    5,    4,    4,    4,    4}  //  5
/*UCHAR*/, {   6,    7,    6,    6,    6,    6}  //  7
/*LONG */, {  16,   17,   16,   16,   16,   16}  // 17
/*ULONG*/, {  18,   19,   18,   18,   18,   18}  // 19
};
   static const LONG bmul[][6] = {
 //         true  false  true  true  true true
/*true */  {   1,    0,    1,    1,    1,    1}  //  1
/*false*/, {   0,    0,    0,    0,    0,    0}  //  0
/*CHAR */, {   5,    0,    5,    5,    5,    5}  //  5
/*UCHAR*/, {   7,    0,    7,    7,    7,    7}  //  7
/*LONG */, {  17,    0,   17,   17,   17,   17}  // 17
/*ULONG*/, {  19,    0,   19,   19,   19,   19}  // 19
};
   static const LONG bdiv[][6] = {
 //         true  false  true  true  true true
/*true */  {   1,    0,    1,    1,    1,    1}  //  1
/*false*/, {   0,    0,    0,    0,    0,    0}  //  0
/*CHAR */, {   5,    0,    5,    5,    5,    5}  //  5
/*UCHAR*/, {   7,    0,    7,    7,    7,    7}  //  7
/*LONG */, {  17,    0,   17,   17,   17,   17}  // 17
/*ULONG*/, {  19,    0,   19,   19,   19,   19}  // 19
};
   static const LONG bmod[][6] = {
 //         true  false  true  true  true true
/*true */  {   0,    0,    0,    0,    0,    0}  //  1
/*false*/, {   0,    0,    0,    0,    0,    0}  //  0
/*CHAR */, {   1,    1,    1,    1,    1,    1}  //  5
/*UCHAR*/, {   1,    1,    1,    1,    1,    1}  //  7
/*LONG */, {   1,    1,    1,    1,    1,    1}  // 17
/*ULONG*/, {   1,    1,    1,    1,    1,    1}  // 19
};

   bool  bdiscrete[]   = { true, false, true, true, true, true };
   CHAR  cdiscrete[]   = { 1, 0, 5, 7, 17, 19 };
   UCHAR ucdiscrete[]  = { 1, 0, 5, 7, 17, 19 };
   LONG  ldiscrete[]   = { 1, 0, 5, 7, 17, 19 };
   ULONG uldiscrete[]  = { 1, 0, 5, 7, 17, 19 };
   SlipDatum* datum[]   = { new SlipDatum((bool) true)
                          , new SlipDatum((bool) false)
                          , new SlipDatum((CHAR)    5)
                          , new SlipDatum((UCHAR)   7)
                          , new SlipDatum((LONG)   17)
                          , new SlipDatum((ULONG)  19)
                          };
   for(unsigned int i = 0; i < sizeof(datum)/sizeof(datum[0]); i++) {
      for(unsigned int j = 0; j < sizeof(datum)/sizeof(datum[0]); j++) {
         flag = ((*datum[i] + *datum[j]) == add[i][j])
                 ? flag
                 :  outputOperatorFailure(*datum[i], *datum[j], "+", (LONG)(*datum[i] + *datum[j]), add[i][j], i, j);
         flag = ((*datum[i] - *datum[j]) == sub[i][j])
                 ? flag
                 :  outputOperatorFailure(*datum[i], *datum[j], "-", (LONG)(*datum[i] - *datum[j]), sub[i][j], i, j);
         flag = ((*datum[i] * (*datum[j])) == mul[i][j])
                 ? flag
                 :  outputOperatorFailure(*datum[i], *datum[j], "*", (LONG)(*datum[i] * (*datum[j])), mul[i][j], i, j);

         if (*datum[j] != (LONG)0) {
            flag = ((*datum[i] / (*datum[j])) == div[i][j])
                    ? flag
                    :  outputOperatorFailure(*datum[i], *datum[j], "/", (LONG)(*datum[i] / (*datum[j])), div[i][j], i, j);
            flag = ((*datum[i] % (*datum[j])) == mod[i][j])
                    ? flag
                    :  outputOperatorFailure(*datum[i], *datum[j], "%", (LONG)(*datum[i] % (*datum[j])), mod[i][j], i, j);
         }
//*************************************************************
         flag = ((*datum[i] + ldiscrete[j]) == add[i][j])
                 ? flag
                 :  outputOperatorFailure(*datum[i], ldiscrete[j], "+", "LONG", (LONG)(*datum[i] + ldiscrete[j]), add[i][j], i, j);
         flag = ((*datum[i] - ldiscrete[j]) == sub[i][j])
                 ? flag
                 :  outputOperatorFailure(*datum[i], ldiscrete[j], "-", "LONG", (LONG)(*datum[i] - ldiscrete[j]), sub[i][j], i, j);
         flag = ((*datum[i] * ldiscrete[j]) == mul[i][j])
                 ? flag
                 :  outputOperatorFailure(*datum[i], ldiscrete[j], "*", "LONG", (LONG)(*datum[i] * ldiscrete[j]), mul[i][j], i, j);

         if (ldiscrete[j] != (LONG)0) {
            flag = ((*datum[i] / ldiscrete[j]) == div[i][j])
                    ? flag
                    :  outputOperatorFailure(*datum[i], ldiscrete[j], "LONG", "/", (LONG)(*datum[i] / ldiscrete[j]), div[i][j], i, j);
            flag = ((*datum[i] % ldiscrete[j]) == mod[i][j])
                    ? flag
                    :  outputOperatorFailure(*datum[i], ldiscrete[j], "LONG", "%", (LONG)(*datum[i] % ldiscrete[j]), mod[i][j], i, j);
         }
//*************************************************************
         flag = ((*datum[i] + bdiscrete[j]) == badd[i][j])
                 ? flag
                 :  outputOperatorFailure(*datum[i], bdiscrete[j], "+", "bool", (LONG)(*datum[i] + bdiscrete[j]), badd[i][j], i, j);
         flag = ((*datum[i] - bdiscrete[j]) == bsub[i][j])
                 ? flag
                 :  outputOperatorFailure(*datum[i], bdiscrete[j], "-", "bool", (LONG)(*datum[i] - bdiscrete[j]), bsub[i][j], i, j);
         flag = ((*datum[i] * bdiscrete[j]) == bmul[i][j])
                 ? flag
                 :  outputOperatorFailure(*datum[i], bdiscrete[j], "*", "bool", (LONG)(*datum[i] * bdiscrete[j]), bmul[i][j], i, j);

         if (bdiscrete[j]) {
            flag = ((*datum[i] / bdiscrete[j]) == bdiv[i][j])
                    ? flag
                    :  outputOperatorFailure(*datum[i], bdiscrete[j], "/", "bool", (LONG)(*datum[i] / bdiscrete[j]), bdiv[i][j], i, j);
            flag = ((*datum[i] % bdiscrete[j]) == (LONG)0)
                    ? flag
                    :  outputOperatorFailure(*datum[i], bdiscrete[j], "%", "bool", (LONG)(*datum[i] % bdiscrete[j]), 0, i, j);
         }
//*************************************************************
         flag = ((*datum[i] + cdiscrete[j]) == add[i][j])
                 ? flag
                 :  outputOperatorFailure(*datum[i], cdiscrete[j], "+", "CHAR", (LONG)(*datum[i] + cdiscrete[j]), add[i][j], i, j);
         flag = ((*datum[i] - cdiscrete[j]) == sub[i][j])
                 ? flag
                 :  outputOperatorFailure(*datum[i], cdiscrete[j], "-", "CHAR", (LONG)(*datum[i] - cdiscrete[j]), sub[i][j], i, j);
         flag = ((*datum[i] * cdiscrete[j]) == mul[i][j])
                 ? flag
                 :  outputOperatorFailure(*datum[i], cdiscrete[j], "*", "CHAR", (LONG)(*datum[i] * cdiscrete[j]), mul[i][j], i, j);

         if (cdiscrete[j] != (LONG)0) {
            flag = ((*datum[i] / cdiscrete[j]) == div[i][j])
                    ? flag
                    :  outputOperatorFailure(*datum[i], cdiscrete[j], "CHAR", "/", (LONG)(*datum[i] / cdiscrete[j]), div[i][j], i, j);
            flag = ((*datum[i] % cdiscrete[j]) == mod[i][j])
                    ? flag
                    :  outputOperatorFailure(*datum[i], cdiscrete[j], "CHAR", "%", (LONG)(*datum[i] % cdiscrete[j]), mod[i][j], i, j);
         }
//*************************************************************
         flag = ((*datum[i] + ucdiscrete[j]) == add[i][j])
                 ? flag
                 :  outputOperatorFailure(*datum[i], ucdiscrete[j], "+", "UCHAR", (LONG)(*datum[i] + ucdiscrete[j]), add[i][j], i, j);
         flag = ((*datum[i] - ucdiscrete[j]) == sub[i][j])
                 ? flag
                 :  outputOperatorFailure(*datum[i], ucdiscrete[j], "-", "UCHAR", (LONG)(*datum[i] - ucdiscrete[j]), sub[i][j], i, j);
         flag = ((*datum[i] * ucdiscrete[j]) == mul[i][j])
                 ? flag
                 :  outputOperatorFailure(*datum[i], ucdiscrete[j], "*", "UCHAR", (LONG)(*datum[i] * ucdiscrete[j]), mul[i][j], i, j);

         if (ucdiscrete[j] != (LONG)0) {
            flag = ((*datum[i] / ucdiscrete[j]) == div[i][j])
                    ? flag
                    :  outputOperatorFailure(*datum[i], ucdiscrete[j], "UCHAR", "/", (LONG)(*datum[i] / ucdiscrete[j]), div[i][j], i, j);
            flag = ((*datum[i] % ucdiscrete[j]) == mod[i][j])
                    ? flag
                    :  outputOperatorFailure(*datum[i], ucdiscrete[j], "UCHAR", "%", (LONG)(*datum[i] % ucdiscrete[j]), mod[i][j], i, j);
         }
//*************************************************************
         flag = ((*datum[i] + uldiscrete[j]) == add[i][j])
                 ? flag
                 :  outputOperatorFailure(*datum[i], uldiscrete[j], "+", "ULONG", (LONG)(*datum[i] + uldiscrete[j]), add[i][j], i, j);
         flag = ((*datum[i] - uldiscrete[j]) == sub[i][j])
                 ? flag
                 :  outputOperatorFailure(*datum[i], uldiscrete[j], "-", "ULONG", (LONG)(*datum[i] - uldiscrete[j]), sub[i][j], i, j);
         flag = ((*datum[i] * uldiscrete[j]) == mul[i][j])
                 ? flag
                 :  outputOperatorFailure(*datum[i], uldiscrete[j], "*", "ULONG", (LONG)(*datum[i] * uldiscrete[j]), mul[i][j], i, j);

         if (uldiscrete[j] != (LONG)0) {
            flag = ((*datum[i] / uldiscrete[j]) == div[i][j])
                    ? flag
                    :  outputOperatorFailure(*datum[i], uldiscrete[j], "ULONG", "/", (LONG)(*datum[i] / uldiscrete[j]), div[i][j], i, j);
            flag = ((*datum[i] % uldiscrete[j]) == mod[i][j])
                    ? flag
                    :  outputOperatorFailure(*datum[i], uldiscrete[j], "ULONG", "%", (LONG)(*datum[i] % uldiscrete[j]), mod[i][j], i, j);
         }
//*************************************************************
         flag = ((ldiscrete[i] + *datum[j]) == add[i][j])
                 ? flag
                 :  outputOperatorFailure(ldiscrete[i], *datum[j], "+", "LONG", (LONG)(ldiscrete[i] + *datum[j]), add[i][j], i, j);
         flag = ((ldiscrete[i] - *datum[j]) == sub[i][j])
                 ? flag
                 :  outputOperatorFailure(ldiscrete[i], *datum[j], "-", "LONG", (LONG)(ldiscrete[i] - *datum[j]), sub[i][j], i, j);
         flag = ((ldiscrete[i] * (*datum[j])) == mul[i][j])
                 ? flag
                 :  outputOperatorFailure(ldiscrete[i], *datum[j], "*", "LONG", (LONG)(ldiscrete[i] * (*datum[j])), mul[i][j], i, j);

         if (*datum[j] != (LONG)0) {
            flag = ((ldiscrete[i] / *datum[j]) == div[i][j])
                    ? flag
                    :  outputOperatorFailure(ldiscrete[i], *datum[j], "/", "LONG", (LONG)(ldiscrete[i] / *datum[j]), div[i][j], i, j);
            flag = ((ldiscrete[i] % *datum[j]) == mod[i][j])
                    ? flag
                    :  outputOperatorFailure(ldiscrete[i], *datum[j], "%", "LONG", (LONG)(ldiscrete[i] % *datum[j]), mod[i][j], i, j);
         }
//*************************************************************
         flag = ((bdiscrete[i] + *datum[j]) == badd[j][i])
                 ? flag
                 :  outputOperatorFailure(bdiscrete[i], *datum[j], "+", "bool", (LONG)(bdiscrete[i] + *datum[j]), badd[j][i], j, i);
         flag = ((bdiscrete[i] - *datum[j]) == -bsub[j][i])
                 ? flag
                 :  outputOperatorFailure(bdiscrete[i], *datum[j], "-", "bool", (LONG)(bdiscrete[i] - *datum[j]), -bsub[j][i], j, i);
         flag = ((bdiscrete[i] * (*datum[j])) == bmul[j][i])
                 ? flag
                 :  outputOperatorFailure(bdiscrete[i], *datum[j], "*", "bool", (LONG)(bdiscrete[i] * (*datum[j])), bmul[j][i], j, i);

         if (*datum[j] != (LONG)0) {
            if (bdiscrete[i]) {
               flag = ((bdiscrete[i] / *datum[j]) == 1/bdiv[j][i])
                       ? flag
                       :  outputOperatorFailure(bdiscrete[i], *datum[j], "/", "bool", (LONG)(bdiscrete[i] / *datum[j]), 1/bdiv[j][i], j, i);
               flag = ((bdiscrete[i] % *datum[j]) == bmod[j][i])
                       ? flag
                       :  outputOperatorFailure(bdiscrete[i], *datum[j], "%", "bool", (LONG)(bdiscrete[i] % *datum[j]), bmod[j][i], j, i);
            } else {
               flag = ((bdiscrete[i] / *datum[j]) == (LONG)0)
                       ? flag
                       :  outputOperatorFailure(bdiscrete[i], *datum[j], "/", "bool", (LONG)(bdiscrete[i] / *datum[j]), 0, j, i);
               flag = ((bdiscrete[i] % *datum[j]) == (LONG)0)
                       ? flag
                       :  outputOperatorFailure(bdiscrete[i], *datum[j], "%", "bool", (LONG)(bdiscrete[i] % *datum[j]), bmod[j][i], j, i);
            }
         }
//*************************************************************
         flag = ((cdiscrete[i] + *datum[j]) == add[i][j])
                 ? flag
                 :  outputOperatorFailure(cdiscrete[i], *datum[j], "+", "CHAR", (LONG)(cdiscrete[i] + *datum[j]), add[i][j], i, j);
         flag = ((cdiscrete[i] - *datum[j]) == sub[i][j])
                 ? flag
                 :  outputOperatorFailure(cdiscrete[i], *datum[j], "-", "CHAR", (LONG)(cdiscrete[i] - *datum[j]), sub[i][j], i, j);
         flag = ((cdiscrete[i] * (*datum[j])) == mul[i][j])
                 ? flag
                 :  outputOperatorFailure(cdiscrete[i], *datum[j], "*", "CHAR", (LONG)(cdiscrete[i] * (*datum[j])), mul[i][j], i, j);

         if (*datum[j] != (LONG)0) {
            flag = ((cdiscrete[i] / *datum[j]) == div[i][j])
                    ? flag
                    :  outputOperatorFailure(cdiscrete[i], *datum[j], "/", "CHAR", (LONG)(cdiscrete[i] / *datum[j]), div[i][j], i, j);
            flag = ((cdiscrete[i] % *datum[j]) == mod[i][j])
                    ? flag
                    :  outputOperatorFailure(cdiscrete[i], *datum[j], "%", "CHAR", (LONG)(cdiscrete[i] % *datum[j]), mod[i][j], i, j);
         }
//*************************************************************
         flag = ((ucdiscrete[i] + *datum[j]) == add[i][j])
                 ? flag
                 :  outputOperatorFailure(ucdiscrete[i], *datum[j], "+", "UCHAR", (LONG)(ucdiscrete[i] + *datum[j]), add[i][j], i, j);
         flag = ((ucdiscrete[i] - *datum[j]) == sub[i][j])
                 ? flag
                 :  outputOperatorFailure(ucdiscrete[i], *datum[j], "-", "UCHAR", (LONG)(ucdiscrete[i] - *datum[j]), sub[i][j], i, j);
         flag = ((ucdiscrete[i] * (*datum[j])) == mul[i][j])
                 ? flag
                 :  outputOperatorFailure(ucdiscrete[i], *datum[j], "*", "UCHAR", (LONG)(ucdiscrete[i] * (*datum[j])), mul[i][j], i, j);

         if (*datum[j] != (LONG)0) {
            flag = ((ucdiscrete[i] / *datum[j]) == div[i][j])
                    ? flag
                    :  outputOperatorFailure(ucdiscrete[i], *datum[j], "/", "UCHAR", (LONG)(ucdiscrete[i] / *datum[j]), div[i][j], i, j);
            flag = ((ucdiscrete[i] % *datum[j]) == mod[i][j])
                    ? flag
                    :  outputOperatorFailure(ucdiscrete[i], *datum[j], "%", "UCHAR", (LONG)(ucdiscrete[i] % *datum[j]), mod[i][j], i, j);
         }
//*************************************************************
         flag = ((uldiscrete[i] + *datum[j]) == add[i][j])
                 ? flag
                 :  outputOperatorFailure(uldiscrete[i], *datum[j], "+", "ULONG", (LONG)(uldiscrete[i] + *datum[j]), add[i][j], i, j);
         flag = ((uldiscrete[i] - *datum[j]) == sub[i][j])
                 ? flag
                 :  outputOperatorFailure(uldiscrete[i], *datum[j], "-", "ULONG", (LONG)(uldiscrete[i] - *datum[j]), sub[i][j], i, j);
         flag = ((uldiscrete[i] * (*datum[j])) == mul[i][j])
                 ? flag
                 :  outputOperatorFailure(uldiscrete[i], *datum[j], "*", "ULONG", (LONG)(uldiscrete[i] * (*datum[j])), mul[i][j], i, j);

         if (*datum[j] != (LONG)0) {
            flag = ((uldiscrete[i] / *datum[j]) == div[i][j])
                    ? flag
                    :  outputOperatorFailure(uldiscrete[i], *datum[j], "/", "ULONG", (LONG)(uldiscrete[i] / *datum[j]), div[i][j], i, j);
            flag = ((uldiscrete[i] % *datum[j]) == mod[i][j])
                    ? flag
                    :  outputOperatorFailure(uldiscrete[i], *datum[j], "%", "ULONG", (LONG)(uldiscrete[i] % *datum[j]), mod[i][j], i, j);
         }
//*************************************************************
         SlipDatum addDatum(*datum[i] + *datum[j]);
         SlipDatum subDatum(*datum[i] - *datum[j]);
         SlipDatum mulDatum(*datum[i] * *datum[j]);

         flag = (addDatum == add[i][j])
                 ? flag
                 :  outputOperatorFailure(*datum[i], *datum[j], "+", addDatum, add[i][j], i, j);
         flag = (subDatum == sub[i][j])
                 ? flag
                 :  outputOperatorFailure(*datum[i], *datum[j], "-", subDatum, sub[i][j], i, j);
         flag = (mulDatum == mul[i][j])
                 ? flag
                 :  outputOperatorFailure(*datum[i], *datum[j], "*", mulDatum, mul[i][j], i, j);
         if (*datum[j] != (LONG)0) {
            SlipDatum divDatum(*datum[i] / *datum[j]);
            SlipDatum modDatum(*datum[i] % *datum[j]);
            flag = (divDatum == div[i][j])
                    ? flag
                    :  outputOperatorFailure(*datum[i], *datum[j], "/", divDatum, div[i][j], i, j);
            flag = (modDatum == mod[i][j])
                    ? flag
                    :  outputOperatorFailure(*datum[i], *datum[j], "%", modDatum, mod[i][j], i, j);
         }
      }
   }

//*************************************************************

   for(unsigned int i = 0; i < sizeof(datum)/sizeof(datum[0]); i++) delete datum[i];

   if (!flag) testResultOutput("Test Arithmetic Operators ", 0.0, flag);
   return flag;
}; // bool testArithmeticDiscreteOperators()
bool testArithmeticNonDiscreteOperators() {
   bool flag = true;
   DOUBLE add[]         = {  7.3,  6.3,  11.3, 13.3, 23.3,   25.3 };
   DOUBLE sub[]         = { -5.3, -6.3,  -1.3,  0.7, 10.7,   12.7 };
   DOUBLE mul[]         = {  6.3,  0.0,  31.5, 44.1, 107.1, 119.7 };
   DOUBLE div[][6]      = { { 0.15873015873015873015 // true  / 6.3
                            , 0.0                    // false / 6.3
                            , 0.79365079365079365079 //  5 / 6.3
                            , 1.11111111111111111111 //  7 / 6.3
                            , 2.69841269841269841269 // 17 / 6.3
                            , 3.01587301587301587301}// 19 / 6.3
                          , { 6.3                    // 6.3 / true
                            , 0.0                    // 6.3 / false
                            , 1.26                   // 6.3 /  5
                            , 0.9                    // 6.3 /  7
                            , 0.37058823529411764705 // 6.3 / 17
                            , 0.33157894736842105263}// 6.3 / 19
                          };
   SlipDatum* datum[]   = { new SlipDatum((bool) true )
                          , new SlipDatum((bool) false)
                          , new SlipDatum((CHAR)     5)
                          , new SlipDatum((UCHAR)    7)
                          , new SlipDatum((LONG)    17)
                          , new SlipDatum((ULONG)   19)
                          };
   SlipDatum dbl((DOUBLE) 6.3);

   for(unsigned int i = 0; i < sizeof(datum)/sizeof(datum[0]); i++) {
      SlipDatum addDatum(*datum[i] + dbl);
      SlipDatum subDatum(*datum[i] - dbl);
      SlipDatum mulDatum(*datum[i] * (dbl));
      SlipDatum divDatum((*datum[i]) / dbl);
      flag = (fabs((double)(*datum[i] + dbl) - add[i]) <= epsilon)
              ? flag
              :  outputOperatorFailure(*datum[i], dbl, "+", (*datum[i] + dbl), add[i], i, -1);
      flag = (fabs((double)(dbl + *datum[i]) - add[i]) <= epsilon)
              ? flag
              :  outputOperatorFailure(dbl, *datum[i],  "+", (*datum[i] + dbl), add[i], i, -1);
      flag = (fabs((double)addDatum - add[i]) <= epsilon)
              ? flag
              :  outputOperatorFailure(addDatum, dbl, "-", (dbl + *datum[i]), add[i], i, -1);
      flag = (fabs((double)(*datum[i] - dbl) - sub[i]) <= epsilon)
              ? flag
              :  outputOperatorFailure(*datum[i], dbl, "-", (*datum[i] - dbl), sub[i], i, -1);
      flag = (fabs((double)(dbl - *datum[i]) + sub[i]) <= epsilon)
              ? flag
              :  outputOperatorFailure(dbl, *datum[i],  "-", (dbl - *datum[i]), sub[i], i, -1);
      flag = (fabs((double)subDatum - sub[i]) <= epsilon)
              ? flag
              :  outputOperatorFailure(subDatum, dbl, "-", (*datum[i] - dbl), sub[i], i, -1);
      flag = (fabs((double)(*datum[i] * (dbl)) - mul[i]) <= epsilon)
              ? flag
              :  outputOperatorFailure(*datum[i], dbl, "*", (*datum[i] * dbl), mul[i], i, -1);
      flag = (fabs((double)(dbl * (*datum[i])) - mul[i]) <= epsilon)
              ? flag
              :  outputOperatorFailure(dbl, *datum[i],  "*", (dbl * *datum[i]), mul[i], i, -1);
      flag = (fabs((double)subDatum - sub[i]) <= epsilon)
              ? flag
              :  outputOperatorFailure(mulDatum, dbl, "*", (*datum[i] * dbl), mul[i], i, -1);
      flag = (fabs((double)(*datum[i] / (dbl)) - div[0][i]) <= epsilon)
              ? flag
              :  outputOperatorFailure(*datum[i], dbl, "/", (*datum[i] / dbl), div[0][i], 0, i);
      flag = (fabs((double)(divDatum - div[0][i])) <= epsilon)
              ? flag
              :  outputOperatorFailure(divDatum, dbl, "/", (*datum[i] / dbl), div[0][i], 0, i);


      if (*datum[i] != 0.0) {
         SlipDatum divDatum(dbl / (*datum[i]));
         flag = (fabs((double)(dbl / (*datum[i])) - div[1][i]) <= epsilon)
                 ? flag
                 :  outputOperatorFailure(dbl, *datum[i],  "/", (dbl / *datum[i]), div[1][i], 1, i);
         flag = (fabs((double)(dbl / (*datum[i])) - div[1][i]) <= epsilon)
                 ? flag
                 :  outputOperatorFailure(divDatum, *datum[i],  "/", (dbl / *datum[i]), div[1][i], 1, i);
      }
  }
   SlipDatum  dis(17.0);
//***********************************************************
   flag = ((dbl + dis) == (6.3 + 17.0))
      ? flag
      :  outputOperatorFailure(dbl, dis, "+", (dbl + dis), (6.3 + 17.0), 0, 0);
   flag = ((dbl - dis) == (6.3 - 17.0))
      ? flag
      :  outputOperatorFailure(dbl, dis, "-", (dbl - dis), (6.3 - 17.0), 0, 0);
   flag = ((dbl * dis) == (6.3 * 17.0))
      ? flag
      :  outputOperatorFailure(dbl, dis, "*", (dbl * dis), (6.3 * 17.0), 0, 0);
   flag = ((dbl / dis) == (6.3 / 17.0))
      ? flag
      :  outputOperatorFailure(dbl, dis, "/", (dbl / dis), (6.3 / 17.0), 0, 0);

   flag = ((dbl + 17.0) == (6.3 + 17.0))
      ? flag
      :  outputOperatorFailure(dbl, dis, "+", (dbl + 17.0), (6.3 + 17.0), 0, 0);
   flag = ((dbl - 17.0) == (6.3 - 17.0))
      ? flag
      :  outputOperatorFailure(dbl, dis, "-", (dbl - 17.0), (6.3 - 17.0), 0, 0);
   flag = ((dbl * 17.0) == (6.3 * 17.0))
      ? flag
      :  outputOperatorFailure(dbl, dis, "*", (dbl * 17.0), (6.3 * 17.0), 0, 0);
   flag = ((dbl / 17.0) == (6.3 / 17.0))
      ? flag
      :  outputOperatorFailure(dbl, dis, "/", (dbl / 17.0), (6.3 / 17.0), 0, 0);

   flag = ((17.0 + dbl) == (17.0 + 6.3))
      ? flag
      :  outputOperatorFailure(dis, dbl, "+", (17.0 + dbl), (17.0 + 6.3), 0, 0);
   flag = ((17.0 - dbl) == (17.0 - 6.3))
      ? flag
      :  outputOperatorFailure(dis, dbl, "-", (dbl - 17.0), (17.0 - 6.3), 0, 0);
   flag = ((17.0 * dbl) == (17.0 * 6.3))
      ? flag
      :  outputOperatorFailure(dis, dbl, "*", (17.0 * dbl), (17.0 * 6.3), 0, 0);
   flag = ((17.0 / dbl) == (17.0 / 6.3))
      ? flag
      :  outputOperatorFailure(dis, dbl, "/", (17.0 / dbl), (17.0 / 6.3), 0, 0);

   flag = ((dbl + (CHAR)17) == (6.3 + (CHAR)17))
      ? flag
      :  outputOperatorFailure(dbl, dis, "+", (dbl + (CHAR)17), (6.3 + (CHAR)17), 0, 0);
   flag = ((dbl - (CHAR)17) == (6.3 - (CHAR)17))
      ? flag
      :  outputOperatorFailure(dbl, dis, "-", (dbl - (CHAR)17), (6.3 - (CHAR)17), 0, 0);
   flag = ((dbl * (CHAR)17) == (6.3 * (CHAR)17))
      ? flag
      :  outputOperatorFailure(dbl, dis, "*", (dbl * (CHAR)17), (6.3 * (CHAR)17), 0, 0);
   flag = ((dbl / (CHAR)17) == (6.3 / (CHAR)17))
      ? flag
      :  outputOperatorFailure(dbl, dis, "/", (dbl / (CHAR)17), (6.3 / (CHAR)17), 0, 0);

   flag = (((CHAR)17 + dbl) == ((CHAR)17 + 6.3))
      ? flag
      :  outputOperatorFailure(dis, dbl, "+", ((CHAR)17 + dbl), ((CHAR)17 + 6.3), 0, 0);
   flag = (((CHAR)17 - dbl) == ((CHAR)17 - 6.3))
      ? flag
      :  outputOperatorFailure(dis, dbl, "-", (dbl - (CHAR)17), ((CHAR)17 - 6.3), 0, 0);
   flag = (((CHAR)17 * dbl) == ((CHAR)17 * 6.3))
      ? flag
      :  outputOperatorFailure(dis, dbl, "*", ((CHAR)17 * dbl), ((CHAR)17 * 6.3), 0, 0);
   flag = (((CHAR)17 / dbl) == ((CHAR)17 / 6.3))
      ? flag
      :  outputOperatorFailure(dis, dbl, "/", ((CHAR)17 / dbl), ((CHAR)17 / 6.3), 0, 0);

   flag = ((dbl + (UCHAR)17) == (6.3 + (UCHAR)17))
      ? flag
      :  outputOperatorFailure(dbl, dis, "+", (dbl + (UCHAR)17), (6.3 + (UCHAR)17), 0, 0);
   flag = ((dbl - (UCHAR)17) == (6.3 - (UCHAR)17))
      ? flag
      :  outputOperatorFailure(dbl, dis, "-", (dbl - (UCHAR)17), (6.3 - (UCHAR)17), 0, 0);
   flag = ((dbl * (UCHAR)17) == (6.3 * (UCHAR)17))
      ? flag
      :  outputOperatorFailure(dbl, dis, "*", (dbl * (UCHAR)17), (6.3 * (UCHAR)17), 0, 0);
   flag = ((dbl / (UCHAR)17) == (6.3 / (UCHAR)17))
      ? flag
      :  outputOperatorFailure(dbl, dis, "/", (dbl / (UCHAR)17), (6.3 / (UCHAR)17), 0, 0);

   flag = (((UCHAR)17 + dbl) == ((UCHAR)17 + 6.3))
      ? flag
      :  outputOperatorFailure(dis, dbl, "+", ((UCHAR)17 + dbl), ((UCHAR)17 + 6.3), 0, 0);
   flag = (((UCHAR)17 - dbl) == ((UCHAR)17 - 6.3))
      ? flag
      :  outputOperatorFailure(dis, dbl, "-", (dbl - (UCHAR)17), ((UCHAR)17 - 6.3), 0, 0);
   flag = (((UCHAR)17 * dbl) == ((UCHAR)17 * 6.3))
      ? flag
      :  outputOperatorFailure(dis, dbl, "*", ((UCHAR)17 * dbl), ((UCHAR)17 * 6.3), 0, 0);
   flag = (((UCHAR)17 / dbl) == ((UCHAR)17 / 6.3))
      ? flag
      :  outputOperatorFailure(dis, dbl, "/", ((UCHAR)17 / dbl), ((UCHAR)17 / 6.3), 0, 0);

   flag = ((dbl + (LONG)17) == (6.3 + (LONG)17))
      ? flag
      :  outputOperatorFailure(dbl, dis, "+", (dbl + (LONG)17), (6.3 + (LONG)17), 0, 0);
   flag = ((dbl - (LONG)17) == (6.3 - (LONG)17))
      ? flag
      :  outputOperatorFailure(dbl, dis, "-", (dbl - (LONG)17), (6.3 - (LONG)17), 0, 0);
   flag = ((dbl * (LONG)17) == (6.3 * (LONG)17))
      ? flag
      :  outputOperatorFailure(dbl, dis, "*", (dbl * (LONG)17), (6.3 * (LONG)17), 0, 0);
   flag = ((dbl / (LONG)17) == (6.3 / (LONG)17))
      ? flag
      :  outputOperatorFailure(dbl, dis, "/", (dbl / (LONG)17), (6.3 / (LONG)17), 0, 0);

   flag = (((LONG)17 + dbl) == ((LONG)17 + 6.3))
      ? flag
      :  outputOperatorFailure(dis, dbl, "+", ((LONG)17 + dbl), ((LONG)17 + 6.3), 0, 0);
   flag = (((LONG)17 - dbl) == ((LONG)17 - 6.3))
      ? flag
      :  outputOperatorFailure(dis, dbl, "-", (dbl - (LONG)17), ((LONG)17 - 6.3), 0, 0);
   flag = (((LONG)17 * dbl) == ((LONG)17 * 6.3))
      ? flag
      :  outputOperatorFailure(dis, dbl, "*", ((LONG)17 * dbl), ((LONG)17 * 6.3), 0, 0);
   flag = (((LONG)17 / dbl) == ((LONG)17 / 6.3))
      ? flag
      :  outputOperatorFailure(dis, dbl, "/", ((LONG)17 / dbl), ((LONG)17 / 6.3), 0, 0);

   flag = ((dbl + (ULONG)17) == (6.3 + (ULONG)17))
      ? flag
      :  outputOperatorFailure(dbl, dis, "+", (dbl + (ULONG)17), (6.3 + (ULONG)17), 0, 0);
   flag = ((dbl - (ULONG)17) == (6.3 - (ULONG)17))
      ? flag
      :  outputOperatorFailure(dbl, dis, "-", (dbl - (ULONG)17), (6.3 - (ULONG)17), 0, 0);
   flag = ((dbl * (ULONG)17) == (6.3 * (ULONG)17))
      ? flag
      :  outputOperatorFailure(dbl, dis, "*", (dbl * (ULONG)17), (6.3 * (ULONG)17), 0, 0);
   flag = ((dbl / (ULONG)17) == (6.3 / (ULONG)17))
      ? flag
      :  outputOperatorFailure(dbl, dis, "/", (dbl / (ULONG)17), (6.3 / (ULONG)17), 0, 0);

   flag = (((ULONG)17 + dbl) == ((ULONG)17 + 6.3))
      ? flag
      :  outputOperatorFailure(dis, dbl, "+", ((ULONG)17 + dbl), ((ULONG)17 + 6.3), 0, 0);
   flag = (((ULONG)17 - dbl) == ((ULONG)17 - 6.3))
      ? flag
      :  outputOperatorFailure(dis, dbl, "-", (dbl - (ULONG)17), ((ULONG)17 - 6.3), 0, 0);
   flag = (((ULONG)17 * dbl) == ((ULONG)17 * 6.3))
      ? flag
      :  outputOperatorFailure(dis, dbl, "*", ((ULONG)17 * dbl), ((ULONG)17 * 6.3), 0, 0);
   flag = (((ULONG)17 / dbl) == ((ULONG)17 / 6.3))
      ? flag
      :  outputOperatorFailure(dis, dbl, "/", ((ULONG)17 / dbl), ((ULONG)17 / 6.3), 0, 0);

   flag = ((dbl + (bool)true) == (6.3 + (bool)true))
      ? flag
      :  outputOperatorFailure(dbl, dis, "+", (dbl + (bool)true), (6.3 + (bool)true), 0, 0);
   flag = ((dbl - (bool)true) == (6.3 - (bool)true))
      ? flag
      :  outputOperatorFailure(dbl, dis, "-", (dbl - (bool)true), (6.3 - (bool)true), 0, 0);
   flag = ((dbl * (bool)true) == (6.3 * (bool)true))
      ? flag
      :  outputOperatorFailure(dbl, dis, "*", (dbl * (bool)true), (6.3 * (bool)true), 0, 0);
   flag = ((dbl / (bool)true) == (6.3 / (bool)true))
      ? flag
      :  outputOperatorFailure(dbl, dis, "/", (dbl / (bool)true), (6.3 / (bool)true), 0, 0);

   flag = (((bool)true + dbl) == ((bool)true + 6.3))
      ? flag
      :  outputOperatorFailure(dis, dbl, "+", ((bool)true + dbl), ((bool)true + 6.3), 0, 0);
   flag = (((bool)true - dbl) == ((bool)true - 6.3))
      ? flag
      :  outputOperatorFailure(dis, dbl, "-", (dbl - (bool)true), ((bool)true - 6.3), 0, 0);
   flag = (((bool)true * dbl) == ((bool)true * 6.3))
      ? flag
      :  outputOperatorFailure(dis, dbl, "*", ((bool)true * dbl), ((bool)true * 6.3), 0, 0);
   flag = (((bool)true / dbl) == ((bool)true / 6.3))
      ? flag
      :  outputOperatorFailure(dis, dbl, "/", ((bool)true / dbl), ((bool)true / 6.3), 0, 0);

   for(unsigned int i = 0; i < sizeof(datum)/sizeof(datum[0]); i++) delete datum[i];

   if (!flag) testResultOutput("Test Arithmetic Operators ", 0.0, flag);
   return flag;
}; // bool testArithmeticNonDiscreteOperators()
/*******************************************************************
 * Test operators under the following conditions:
 * SlipDatum lhs <op> SlipDatum rhs
 * SlipDatum lhs <op> <type> rhs
 * <type> rhs <op> SlipDatum rhs
 *
 * where
 * SlipDatum is any of {bool, CHAR, UCHAR, LONG, ULONG, DOUBLE)
 * <type>    is any of (bool, CHAR, UCHAR, LONG, ULONG, DOUBLE)
 * <op>      is any of (+=, -=, *=, /=, %=, <<=, >>=, &=, |=, ^=)
 *           as appropriate
 *******************************************************************/
bool testAssignmentOperators() {
   bool flag = true;
   //                             operator+=          operator-=          operator*=          operator/=          operator%=          operator<<=         operator>>=         operator&=          operator|=          operator^=
   SlipDatum  test[][10]  = { {SlipDatum((CHAR)1), SlipDatum((CHAR)1), SlipDatum((CHAR)3), SlipDatum((CHAR)2), SlipDatum((CHAR)5), SlipDatum((CHAR)1), SlipDatum((CHAR)1), SlipDatum((CHAR)6), SlipDatum((CHAR)3), SlipDatum((CHAR)4)}
                            , {SlipDatum((UCHAR)1),SlipDatum((UCHAR)1),SlipDatum((UCHAR)3),SlipDatum((UCHAR)2),SlipDatum((UCHAR)5),SlipDatum((UCHAR)1),SlipDatum((UCHAR)1),SlipDatum((UCHAR)6),SlipDatum((UCHAR)3),SlipDatum((UCHAR)4)}
                            , {SlipDatum((LONG)1), SlipDatum((LONG)1), SlipDatum((LONG)3), SlipDatum((LONG)2), SlipDatum((LONG)5), SlipDatum((LONG)1), SlipDatum((LONG)1), SlipDatum((LONG)6), SlipDatum((LONG)3), SlipDatum((LONG)4)}
                            , {SlipDatum((ULONG)1),SlipDatum((ULONG)1),SlipDatum((ULONG)3),SlipDatum((ULONG)2),SlipDatum((ULONG)5),SlipDatum((ULONG)1),SlipDatum((ULONG)1),SlipDatum((ULONG)6),SlipDatum((ULONG)3),SlipDatum((ULONG)4)}
                            };
   //                             operator+=            operator-=            operator*=            operator/=         operator%=          operator<<=          operator>>=          operator&=           operator|=           operator^=
   SlipDatum dtest[]     = { SlipDatum((DOUBLE)1),SlipDatum((DOUBLE)1),SlipDatum((DOUBLE)3),SlipDatum((DOUBLE)2),SlipDatum((DOUBLE)5),SlipDatum((DOUBLE)1),SlipDatum((DOUBLE)1),SlipDatum((DOUBLE)6),SlipDatum((DOUBLE)3),SlipDatum((DOUBLE)4) };
   CHAR      charTest[]  =  { 1, 1, 3, 2, 5, 1, 1, 6, 3, 4 };
   UCHAR     ucharTest[] =  { 1, 1, 3, 2, 5, 1, 1, 6, 3, 4 };
   LONG      longTest[]  =  { 1, 1, 3, 2, 5, 1, 1, 6, 3, 4 };
   ULONG     ulongTest[] =  { 1, 1, 3, 2, 5, 1, 1, 6, 3, 4 };
   DOUBLE    doubleTest[]=  { 1, 1, 3, 2 };
   //                 operator<op>   +=  -=   *=  /=  %= <<= >>=  &=  !=  ^=
   static const LONG   ans[]     = { 10,  9,  27, 13,  3,  6,  3,  2,  3,  7 };
   static const double dans[]    = { 10,  9,  27, 13.5 };
   SlipDatum  data[]    = {SlipDatum((UCHAR)9),SlipDatum((CHAR)9),SlipDatum((LONG)9),SlipDatum((ULONG)9)};
   string     types[]   = { "CHAR", "UCHAR", "LONG", "ULONG", "DOUBLE" };
   SlipDatum  testData((LONG)0);
   SlipDatum  dtestData((DOUBLE)0);

   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
      string dataType = types[i];
      for(unsigned int j = 0; j < sizeof(test)/sizeof(test[0]); j++) {
         string testType = types[j];
         testData = data[i];
         flag = ((testData +=  test[j][0]) == ans[0])? flag: outputOperatorFailure(testData, test[j][0], "+=",  dataType, testType, i, j, ans[0]);
         flag = ((testData -=  test[j][1]) == ans[1])? flag: outputOperatorFailure(testData, test[j][1], "-=",  dataType, testType, i, j, ans[1]);
         flag = ((testData *=  test[j][2]) == ans[2])? flag: outputOperatorFailure(testData, test[j][2], "*=",  dataType, testType, i, j, ans[2]);
         flag = ((testData /=  test[j][3]) == ans[3])? flag: outputOperatorFailure(testData, test[j][3], "/=",  dataType, testType, i, j, ans[3]);
         flag = ((testData %=  test[j][4]) == ans[4])? flag: outputOperatorFailure(testData, test[j][4], "%=",  dataType, testType, i, j, ans[4]);
         flag = ((testData <<= test[j][5]) == ans[5])? flag: outputOperatorFailure(testData, test[j][5], "<<=", dataType, testType, i, j, ans[5]);
         flag = ((testData >>= test[j][6]) == ans[6])? flag: outputOperatorFailure(testData, test[j][6], ">>=", dataType, testType, i, j, ans[6]);
         flag = ((testData &=  test[j][7]) == ans[7])? flag: outputOperatorFailure(testData, test[j][7], "&=",  dataType, testType, i, j, ans[7]);
         flag = ((testData |=  test[j][8]) == ans[8])? flag: outputOperatorFailure(testData, test[j][8], "|=",  dataType, testType, i, j, ans[8]);
         flag = ((testData ^=  test[j][9]) == ans[9])? flag: outputOperatorFailure(testData, test[j][9], "^=",  dataType, testType, i, j, ans[9]);
      }
      testData = data[i];
      flag = ((testData +=  charTest[0]) == ans[0])? flag: outputOperatorFailure(testData, (LONG)charTest[0], "+=",  dataType, "CHAR",  i, 0, ans[0]);
      flag = ((testData -=  charTest[1]) == ans[1])? flag: outputOperatorFailure(testData, (LONG)charTest[1], "-=",  dataType, "CHAR",  i, 1, ans[1]);
      flag = ((testData *=  charTest[2]) == ans[2])? flag: outputOperatorFailure(testData, (LONG)charTest[2], "*=",  dataType, "CHAR",  i, 2, ans[2]);
      flag = ((testData /=  charTest[3]) == ans[3])? flag: outputOperatorFailure(testData, (LONG)charTest[3], "/=",  dataType, "CHAR",  i, 3, ans[3]);
      flag = ((testData %=  charTest[4]) == ans[4])? flag: outputOperatorFailure(testData, (LONG)charTest[4], "%=",  dataType, "CHAR",  i, 4, ans[4]);
      flag = ((testData <<= charTest[5]) == ans[5])? flag: outputOperatorFailure(testData, (LONG)charTest[5], "<<=", dataType, "CHAR",  i, 5, ans[5]);
      flag = ((testData >>= charTest[6]) == ans[6])? flag: outputOperatorFailure(testData, (LONG)charTest[6], ">>=", dataType, "CHAR",  i, 6, ans[6]);
      flag = ((testData &=  charTest[7]) == ans[7])? flag: outputOperatorFailure(testData, (LONG)charTest[7], "&=",  dataType, "CHAR",  i, 7, ans[7]);
      flag = ((testData |=  charTest[8]) == ans[8])? flag: outputOperatorFailure(testData, (LONG)charTest[8], "|=",  dataType, "CHAR",  i, 8, ans[8]);
      flag = ((testData ^=  charTest[9]) == ans[9])? flag: outputOperatorFailure(testData, (LONG)charTest[9], "^=",  dataType, "CHAR",  i, 9, ans[9]);
      testData = data[i];
      flag = ((testData +=  ucharTest[0]) == ans[0])? flag: outputOperatorFailure(testData, (long)ucharTest[0], "+=",  dataType, "UCHAR",  i, 0, ans[0]);
      flag = ((testData -=  ucharTest[1]) == ans[1])? flag: outputOperatorFailure(testData, (long)ucharTest[1], "-=",  dataType, "UCHAR",  i, 1, ans[1]);
      flag = ((testData *=  ucharTest[2]) == ans[2])? flag: outputOperatorFailure(testData, (long)ucharTest[2], "*=",  dataType, "UCHAR",  i, 2, ans[2]);
      flag = ((testData /=  ucharTest[3]) == ans[3])? flag: outputOperatorFailure(testData, (long)ucharTest[3], "/=",  dataType, "UCHAR",  i, 3, ans[3]);
      flag = ((testData %=  ucharTest[4]) == ans[4])? flag: outputOperatorFailure(testData, (long)ucharTest[4], "%=",  dataType, "UCHAR",  i, 4, ans[4]);
      flag = ((testData <<= ucharTest[5]) == ans[5])? flag: outputOperatorFailure(testData, (long)ucharTest[5], "<<=", dataType, "UCHAR",  i, 5, ans[5]);
      flag = ((testData >>= ucharTest[6]) == ans[6])? flag: outputOperatorFailure(testData, (long)ucharTest[6], ">>=", dataType, "UCHAR",  i, 6, ans[6]);
      flag = ((testData &=  ucharTest[7]) == ans[7])? flag: outputOperatorFailure(testData, (long)ucharTest[7], "&=",  dataType, "UCHAR",  i, 7, ans[7]);
      flag = ((testData |=  ucharTest[8]) == ans[8])? flag: outputOperatorFailure(testData, (long)ucharTest[8], "|=",  dataType, "UCHAR",  i, 8, ans[8]);
      flag = ((testData ^=  ucharTest[9]) == ans[9])? flag: outputOperatorFailure(testData, (long)ucharTest[9], "^=",  dataType, "UCHAR",  i, 9, ans[9]);
      testData = data[i];
      flag = ((testData +=  longTest[0]) == ans[0])? flag: outputOperatorFailure(testData, (long)longTest[0], "+=",  dataType, "LONG",  i, 0, ans[0]);
      flag = ((testData -=  longTest[1]) == ans[1])? flag: outputOperatorFailure(testData, (long)longTest[1], "-=",  dataType, "LONG",  i, 1, ans[1]);
      flag = ((testData *=  longTest[2]) == ans[2])? flag: outputOperatorFailure(testData, (long)longTest[2], "*=",  dataType, "LONG",  i, 2, ans[2]);
      flag = ((testData /=  longTest[3]) == ans[3])? flag: outputOperatorFailure(testData, (long)longTest[3], "/=",  dataType, "LONG",  i, 3, ans[3]);
      flag = ((testData %=  longTest[4]) == ans[4])? flag: outputOperatorFailure(testData, (long)longTest[4], "%=",  dataType, "LONG",  i, 4, ans[4]);
      flag = ((testData <<= longTest[5]) == ans[5])? flag: outputOperatorFailure(testData, (long)longTest[5], "<<=", dataType, "LONG",  i, 5, ans[5]);
      flag = ((testData >>= longTest[6]) == ans[6])? flag: outputOperatorFailure(testData, (long)longTest[6], ">>=", dataType, "LONG",  i, 6, ans[6]);
      flag = ((testData &=  longTest[7]) == ans[7])? flag: outputOperatorFailure(testData, (long)longTest[7], "&=",  dataType, "LONG",  i, 7, ans[7]);
      flag = ((testData |=  longTest[8]) == ans[8])? flag: outputOperatorFailure(testData, (long)longTest[8], "|=",  dataType, "LONG",  i, 8, ans[8]);
      flag = ((testData ^=  longTest[9]) == ans[9])? flag: outputOperatorFailure(testData, (long)longTest[9], "^=",  dataType, "LONG",  i, 9, ans[9]);
      testData = data[i];
      flag = ((testData +=  ulongTest[0]) == ans[0])? flag: outputOperatorFailure(testData, (long)ulongTest[0], "+=",  dataType, "ULONG",  i, 0, ans[0]);
      flag = ((testData -=  ulongTest[1]) == ans[1])? flag: outputOperatorFailure(testData, (long)ulongTest[1], "-=",  dataType, "ULONG",  i, 1, ans[1]);
      flag = ((testData *=  ulongTest[2]) == ans[2])? flag: outputOperatorFailure(testData, (long)ulongTest[2], "*=",  dataType, "ULONG",  i, 2, ans[2]);
      flag = ((testData /=  ulongTest[3]) == ans[3])? flag: outputOperatorFailure(testData, (long)ulongTest[3], "/=",  dataType, "ULONG",  i, 3, ans[3]);
      flag = ((testData %=  ulongTest[4]) == ans[4])? flag: outputOperatorFailure(testData, (long)ulongTest[4], "%=",  dataType, "ULONG",  i, 4, ans[4]);
      flag = ((testData <<= ulongTest[5]) == ans[5])? flag: outputOperatorFailure(testData, (long)ulongTest[5], "<<=", dataType, "ULONG",  i, 5, ans[5]);
      flag = ((testData >>= ulongTest[6]) == ans[6])? flag: outputOperatorFailure(testData, (long)ulongTest[6], ">>=", dataType, "ULONG",  i, 6, ans[6]);
      flag = ((testData &=  ulongTest[7]) == ans[7])? flag: outputOperatorFailure(testData, (long)ulongTest[7], "&=",  dataType, "ULONG",  i, 7, ans[7]);
      flag = ((testData |=  ulongTest[8]) == ans[8])? flag: outputOperatorFailure(testData, (long)ulongTest[8], "|=",  dataType, "ULONG",  i, 8, ans[8]);
      flag = ((testData ^=  ulongTest[9]) == ans[9])? flag: outputOperatorFailure(testData, (long)ulongTest[9], "^=",  dataType, "ULONG",  i, 9, ans[9]);
      testData = data[i];
      flag = ((testData +=  dtest[0]) == ans[0])? flag: outputOperatorFailure(testData, dtest[0], "+=",  dataType, "DOUBLE",  i, 0, ans[0]);
      flag = ((testData -=  dtest[1]) == ans[1])? flag: outputOperatorFailure(testData, dtest[1], "-=",  dataType, "DOUBLE",  i, 1, ans[1]);
      flag = ((testData *=  dtest[2]) == ans[2])? flag: outputOperatorFailure(testData, dtest[2], "*=",  dataType, "DOUBLE",  i, 2, ans[2]);
      flag = ((testData /=  dtest[3]) == ans[3])? flag: outputOperatorFailure(testData, dtest[3], "/=",  dataType, "DOUBLE",  i, 3, ans[3]);
   }
   {
      dtestData = (DOUBLE)9.0;
      string dataType = types[4];
      flag = ((dtestData +=  doubleTest[0]) == dans[0])? flag: outputOperatorFailure(dtestData, doubleTest[0], "+=",  dataType, types[4], dans[0]);
      flag = ((dtestData -=  doubleTest[1]) == dans[1])? flag: outputOperatorFailure(dtestData, doubleTest[1], "-=",  dataType, types[4], dans[1]);
      flag = ((dtestData *=  doubleTest[2]) == dans[2])? flag: outputOperatorFailure(dtestData, doubleTest[2], "*=",  dataType, types[4], dans[2]);
      flag = ((dtestData /=  doubleTest[3]) == dans[3])? flag: outputOperatorFailure(dtestData, doubleTest[3], "/=",  dataType, types[4], dans[3]);
   }
   for(unsigned int j = 0; j < sizeof(test)/sizeof(test[0]); j++) {
      string testType = types[j];
      dtestData = (double)9.0;
      flag = ((dtestData +=  test[j][0]) == dans[0])? flag: outputOperatorFailure(dtestData, test[j][0], "+=",  "DOUBLE", testType, j, dans[0]);
      flag = ((dtestData -=  test[j][1]) == dans[1])? flag: outputOperatorFailure(dtestData, test[j][1], "-=",  "DOUBLE", testType, j, dans[1]);
      flag = ((dtestData *=  test[j][2]) == dans[2])? flag: outputOperatorFailure(dtestData, test[j][2], "*=",  "DOUBLE", testType, j, dans[2]);
      flag = (abs((double)(dtestData /=  test[j][3]) - dans[3]) < 1.0E-14)
              ? flag: outputOperatorFailure(dtestData, test[j][3], "/=",  "DOUBLE", testType, j, dans[3]);
   }

   string testType = "DOUBLE";
   dtestData = (double)9.0;
   flag = ((dtestData +=  dtest[0]) == dans[0])? flag: outputOperatorFailure(dtestData, dtest[0], "+=",  "DOUBLE", testType, dans[0]);
   flag = ((dtestData -=  dtest[1]) == dans[1])? flag: outputOperatorFailure(dtestData, dtest[1], "-=",  "DOUBLE", testType, dans[1]);
   flag = ((dtestData *=  dtest[2]) == dans[2])? flag: outputOperatorFailure(dtestData, dtest[2], "*=",  "DOUBLE", testType, dans[2]);
   flag = (abs((double)(dtestData /=  dtest[3]) - dans[3]) < 1.0E-14)
           ? flag: outputOperatorFailure(dtestData, dtest[3], "/=",  "DOUBLE", testType, dans[3]);

   for(unsigned int i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
      string dataType = types[i];
      for(unsigned int j = 0; j < sizeof(test)/sizeof(test[0]); j++) {
         string testType = types[j];
         {
            string dataType = "UCHAR";
            UCHAR testData = 9;
            flag = ((testData +=  test[j][0]) == ans[0])? flag: outputOperatorFailure(testData, test[j][0], "+=",  dataType, testType, j, ans[0]);
            flag = ((testData -=  test[j][1]) == ans[1])? flag: outputOperatorFailure(testData, test[j][1], "-=",  dataType, testType, j, ans[1]);
            flag = ((testData *=  test[j][2]) == ans[2])? flag: outputOperatorFailure(testData, test[j][2], "*=",  dataType, testType, j, ans[2]);
            flag = ((testData /=  test[j][3]) == ans[3])? flag: outputOperatorFailure(testData, test[j][3], "/=",  dataType, testType, j, ans[3]);
            flag = ((testData %=  test[j][4]) == ans[4])? flag: outputOperatorFailure(testData, test[j][4], "%=",  dataType, testType, j, ans[4]);
            flag = ((testData <<= test[j][5]) == ans[5])? flag: outputOperatorFailure(testData, test[j][5], "<<=", dataType, testType, j, ans[5]);
            flag = ((testData >>= test[j][6]) == ans[6])? flag: outputOperatorFailure(testData, test[j][6], ">>=", dataType, testType, j, ans[6]);
            flag = ((testData &=  test[j][7]) == ans[7])? flag: outputOperatorFailure(testData, test[j][7], "&=",  dataType, testType, j, ans[7]);
            flag = ((testData |=  test[j][8]) == ans[8])? flag: outputOperatorFailure(testData, test[j][8], "|=",  dataType, testType, j, ans[8]);
            flag = ((testData ^=  test[j][9]) == ans[9])? flag: outputOperatorFailure(testData, test[j][9], "^=",  dataType, testType, j, ans[9]);
         }
         {
            string dataType = "CHAR";
            CHAR testData = 9;
            flag = ((testData +=  test[j][0]) == ans[0])? flag: outputOperatorFailure(testData, test[j][0], "+=",  dataType, testType, j, ans[0]);
            flag = ((testData -=  test[j][1]) == ans[1])? flag: outputOperatorFailure(testData, test[j][1], "-=",  dataType, testType, j, ans[1]);
            flag = ((testData *=  test[j][2]) == ans[2])? flag: outputOperatorFailure(testData, test[j][2], "*=",  dataType, testType, j, ans[2]);
            flag = ((testData /=  test[j][3]) == ans[3])? flag: outputOperatorFailure(testData, test[j][3], "/=",  dataType, testType, j, ans[3]);
            flag = ((testData %=  test[j][4]) == ans[4])? flag: outputOperatorFailure(testData, test[j][4], "%=",  dataType, testType, j, ans[4]);
            flag = ((testData <<= test[j][5]) == ans[5])? flag: outputOperatorFailure(testData, test[j][5], "<<=", dataType, testType, j, ans[5]);
            flag = ((testData >>= test[j][6]) == ans[6])? flag: outputOperatorFailure(testData, test[j][6], ">>=", dataType, testType, j, ans[6]);
            flag = ((testData &=  test[j][7]) == ans[7])? flag: outputOperatorFailure(testData, test[j][7], "&=",  dataType, testType, j, ans[7]);
            flag = ((testData |=  test[j][8]) == ans[8])? flag: outputOperatorFailure(testData, test[j][8], "|=",  dataType, testType, j, ans[8]);
            flag = ((testData ^=  test[j][9]) == ans[9])? flag: outputOperatorFailure(testData, test[j][9], "^=",  dataType, testType, j, ans[9]);
         }
         {
            string dataType = "LONG";
            LONG   testData = 9;
            flag = ((testData +=  test[j][0]) == ans[0])? flag: outputOperatorFailure(testData, test[j][0], "+=",  dataType, testType, j, ans[0]);
            flag = ((testData -=  test[j][1]) == ans[1])? flag: outputOperatorFailure(testData, test[j][1], "-=",  dataType, testType, j, ans[1]);
            flag = ((testData *=  test[j][2]) == ans[2])? flag: outputOperatorFailure(testData, test[j][2], "*=",  dataType, testType, j, ans[2]);
            flag = ((testData /=  test[j][3]) == ans[3])? flag: outputOperatorFailure(testData, test[j][3], "/=",  dataType, testType, j, ans[3]);
            flag = ((testData %=  test[j][4]) == ans[4])? flag: outputOperatorFailure(testData, test[j][4], "%=",  dataType, testType, j, ans[4]);
            flag = ((testData <<= test[j][5]) == ans[5])? flag: outputOperatorFailure(testData, test[j][5], "<<=", dataType, testType, j, ans[5]);
            flag = ((testData >>= test[j][6]) == ans[6])? flag: outputOperatorFailure(testData, test[j][6], ">>=", dataType, testType, j, ans[6]);
            flag = ((testData &=  test[j][7]) == ans[7])? flag: outputOperatorFailure(testData, test[j][7], "&=",  dataType, testType, j, ans[7]);
            flag = ((testData |=  test[j][8]) == ans[8])? flag: outputOperatorFailure(testData, test[j][8], "|=",  dataType, testType, j, ans[8]);
            flag = ((testData ^=  test[j][9]) == ans[9])? flag: outputOperatorFailure(testData, test[j][9], "^=",  dataType, testType, j, ans[9]);
         }
         {
            string dataType = "ULONG";
            ULONG  testData = 9;
            flag = ((testData +=  test[j][0]) == (unsigned)ans[0])? flag: outputOperatorFailure(testData, test[j][0], "+=",  dataType, testType, j, ans[0]);
            flag = ((testData -=  test[j][1]) == (unsigned)ans[1])? flag: outputOperatorFailure(testData, test[j][1], "-=",  dataType, testType, j, ans[1]);
            flag = ((testData *=  test[j][2]) == (unsigned)ans[2])? flag: outputOperatorFailure(testData, test[j][2], "*=",  dataType, testType, j, ans[2]);
            flag = ((testData /=  test[j][3]) == (unsigned)ans[3])? flag: outputOperatorFailure(testData, test[j][3], "/=",  dataType, testType, j, ans[3]);
            flag = ((testData %=  test[j][4]) == (unsigned)ans[4])? flag: outputOperatorFailure(testData, test[j][4], "%=",  dataType, testType, j, ans[4]);
            flag = ((testData <<= test[j][5]) == (unsigned)ans[5])? flag: outputOperatorFailure(testData, test[j][5], "<<=", dataType, testType, j, ans[5]);
            flag = ((testData >>= test[j][6]) == (unsigned)ans[6])? flag: outputOperatorFailure(testData, test[j][6], ">>=", dataType, testType, j, ans[6]);
            flag = ((testData &=  test[j][7]) == (unsigned)ans[7])? flag: outputOperatorFailure(testData, test[j][7], "&=",  dataType, testType, j, ans[7]);
            flag = ((testData |=  test[j][8]) == (unsigned)ans[8])? flag: outputOperatorFailure(testData, test[j][8], "|=",  dataType, testType, j, ans[8]);
            flag = ((testData ^=  test[j][9]) == (unsigned)ans[9])? flag: outputOperatorFailure(testData, test[j][9], "^=",  dataType, testType, j, ans[9]);
         }
         {
            string  dataType = "DOUBLE";
            double  dtestData = 9;
            flag = ((dtestData +=  test[j][0]) == dans[0])? flag: outputOperatorFailure(dtestData, test[j][0], "+=",  dataType, testType, j, dans[0]);
            flag = ((dtestData -=  test[j][1]) == dans[1])? flag: outputOperatorFailure(dtestData, test[j][1], "-=",  dataType, testType, j, dans[1]);
            flag = ((dtestData *=  test[j][2]) == dans[2])? flag: outputOperatorFailure(dtestData, test[j][2], "*=",  dataType, testType, j, dans[2]);
            flag = ((dtestData /=  test[j][3]) == dans[3])? flag: outputOperatorFailure(dtestData, test[j][3], "/=",  dataType, testType, j, dans[3]);
         }
      }
   }
   if (!flag) testResultOutput("Test Assignment Operators ", 0.0, flag);
   return flag;
}; // bool testAssignmentOperators()
bool testCastOperators() {
   stringstream pretty;
   bool flag = true;

   struct Cast {
      string     name;
      SlipDatum* datum;
      bool       bValue;
      CHAR       cValue;
      UCHAR      ucValue;
      LONG       lValue;
      ULONG      ulValue;
      DOUBLE     dValue;
      Cast( string     name
          , SlipDatum* datum
          , bool       bValue
          , CHAR       cValue
          , UCHAR      ucValue
          , LONG       lValue
          , ULONG      ulValue
          , DOUBLE     dValue
          ) : name(name),       datum(datum),   bValue(bValue),   cValue(cValue)
            , ucValue(ucValue), lValue(lValue), ulValue(ulValue), dValue(dValue)
      { }
   }; // struct Cast
   static const Cast cast[] =
   { Cast("true",  new SlipDatum((bool)true),  (bool)true,  (CHAR)1,  (UCHAR)1,  (LONG)1,  (ULONG)1,  (DOUBLE)1.0)
   , Cast("false", new SlipDatum((bool)false), (bool)false, (CHAR)0,  (UCHAR)0,  (LONG)0,  (ULONG)0,  (DOUBLE)0.0)
   , Cast("5",     new SlipDatum((CHAR)    5), (bool)true,  (CHAR)5,  (UCHAR)5,  (LONG)5,  (ULONG)5,  (DOUBLE)5.0)
   , Cast("6",     new SlipDatum((UCHAR)   6), (bool)true,  (CHAR)6,  (UCHAR)6,  (LONG)6,  (ULONG)6,  (DOUBLE)6.0)
   , Cast("7",     new SlipDatum((LONG)    7), (bool)true,  (CHAR)7,  (UCHAR)7,  (LONG)7,  (ULONG)7,  (DOUBLE)7.0)
   , Cast("8",     new SlipDatum((ULONG)   8), (bool)true,  (CHAR)8,  (UCHAR)8,  (LONG)8,  (ULONG)8,  (DOUBLE)8.0)
   , Cast("9.0",   new SlipDatum((DOUBLE)9.0), (bool)true,  (CHAR)9,  (UCHAR)9,  (LONG)9,  (ULONG)9,  (DOUBLE)9.0)
   , Cast("-5",    new SlipDatum((CHAR)   -5), (bool)true,  (CHAR)-5, (UCHAR)-5, (LONG)-5, (ULONG)-5, (DOUBLE)-5.0)
   , Cast("-6",    new SlipDatum((UCHAR)  -6), (bool)true,  (CHAR)-6, (UCHAR)250,(LONG)250,(ULONG)250,(DOUBLE)250.0)
   , Cast("-7",    new SlipDatum((LONG)   -7), (bool)true,  (CHAR)-7, (UCHAR)-7, (LONG)-7, (ULONG)-7, (DOUBLE)-7.0)
   , Cast("9.1)",  new SlipDatum((DOUBLE)9.1), (bool)true,  (CHAR)9,  (UCHAR)9,  (LONG)9,  (ULONG)9,  (DOUBLE)9.1)
   }; // Cast cast[]

   for (unsigned int i = 0; i < sizeof(cast)/sizeof(cast[0]); i ++) {
      const string* name     = &cast[i].name;
      const SlipDatum datum = *cast[i].datum;
      if ((bool)datum != cast[i].bValue) {
         flag = false;
         pretty  << "(bool)SlipDatum(" << *name << ") == " << boolalpha
                 << (bool)datum
                 << " should be " << cast[i].bValue
                 << noboolalpha << endl;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((CHAR)datum != cast[i].cValue) {
         flag = false;
         pretty  << "(char)SlipDatum(" << *name << ") == "
                 << (long)((CHAR)datum)
                 << " should be " << (long)cast[i].cValue << endl;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((UCHAR)datum != cast[i].ucValue) {
         flag = false;
         pretty  << "(unsigned char)SlipDatum(" << *name << ") == "
                 << (unsigned long)((UCHAR)datum)
                 << " should be " << (unsigned long)cast[i].ucValue << endl;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((LONG)datum != cast[i].lValue) {
         flag = false;
         pretty  << "(long)SlipDatum(" << *name << ") == "
                 << (LONG)datum
                 << " should be " << cast[i].lValue << endl;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((ULONG)datum != cast[i].ulValue) {
         flag = false;
         pretty  << "(unsigned long)SlipDatum(" << *name << ") == "
                 << (ULONG)datum
                 << " should be 0x" << hex << setfill(' ') <<cast[i].ulValue
                 << dec << endl;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((DOUBLE)datum != cast[i].dValue) {
         flag = false;
         pretty  << "(double)SlipDatum(" << *name << ") == "
                 << (DOUBLE)datum
                 << " should be " << cast[i].dValue << endl;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
   }

   for (unsigned int i = 0; i < sizeof(cast)/sizeof(cast[0]); i ++) delete cast[i].datum;

   if (!flag) testResultOutput("Test Cast Operators ", 0.0, flag);
   return flag;
}; // bool testCastOperators()
bool testEqualityOperators() {
   bool flag = true;
   SlipHeader*  header1 = new SlipHeader();
   SlipHeader*  header2 = new SlipHeader();
   SlipCell* slipCell[] = { header1
                          , new SlipHeader()
                          , new SlipHeader()
                          , new SlipSublist(*header2)
                          };
   SlipDatum* datum[]   = { new SlipDatum((bool) true)
                          , new SlipDatum((bool) false)
                          , new SlipDatum((CHAR)  127)
                          , new SlipDatum((UCHAR) 126)
                          , new SlipDatum((LONG)  125)
                          , new SlipDatum((ULONG) 124)
                          , new SlipDatum((DOUBLE)123.3)
                          , new SlipDatum(new Pointer())
                          , new SlipDatum((string)"122")
                          };

   for(unsigned int i = 0; i < sizeof(slipCell)/sizeof(slipCell[0]); i++) {
      for(unsigned int j = 0; j < sizeof(slipCell)/sizeof(slipCell[0]); j++) {
         if (i == j) {
            if (slipCell[i] != slipCell[j]) {
               flag = false;
               writeDiagnostics(__FILE__, __LINE__, "false failure "
                                                    + slipCell[i]->toString() + " != "
                                                    + slipCell[j]->toString());
            }
         } else if (*slipCell[i] == *slipCell[j]) {
            flag = false;
            writeDiagnostics(__FILE__, __LINE__, "false success "
                                                 + slipCell[i]->toString() + " == "
                                                 + slipCell[j]->toString());
         }
      }
      for(unsigned int j = 0; j < sizeof(datum)/sizeof(datum[0]); j++) {
         if (*slipCell[i] == *datum[j]) {
            flag = false;
            writeDiagnostics(__FILE__, __LINE__, "false success "
                                                 + slipCell[i]->toString() + " == "
                                                 + datum[j]->toString());
         }
      }
   }

   for(unsigned int i = 0; i < sizeof(datum)/sizeof(datum[0]); i++) {
      for(unsigned int j = 0; j < sizeof(datum)/sizeof(datum[0]); j++) {
         if ( i != j) {
            if (*datum[i] == *datum[j]) {
               flag = false;
               writeDiagnostics(__FILE__, __LINE__, "false success "
                                                    + datum[i]->toString() + " == "
                                                    + datum[j]->toString());
            }
            if (!(*datum[i] != *datum[j])) {
               flag = false;
               writeDiagnostics(__FILE__, __LINE__, "false failure "
                                                    + datum[i]->toString() + " == "
                                                    + datum[j]->toString());
            }
         } else {
            if (!(*datum[i] == *datum[j])) {
               flag = false;
               writeDiagnostics(__FILE__, __LINE__, "false success "
                                                    + datum[i]->toString() + " == "
                                                    + datum[j]->toString());
            }
            if (*datum[i] != *datum[j]) {
               flag = false;
               writeDiagnostics(__FILE__, __LINE__, "false failure "
                                                    + datum[i]->toString() + " == "
                                                    + datum[j]->toString());
             }
         }
      }
   }

   for(unsigned int i = 0; i < sizeof(datum)/sizeof(datum[0]); i++) {
      if (i != 0) {
         if (*datum[i] == (bool)true) {
            flag = false;
            writeDiagnostics(__FILE__, __LINE__, "false success "
                                                 + datum[i]->toString() + " == "
                                                 + "true");
         }
         if (!(*datum[i] != (bool)true)) {
            flag = false;
            writeDiagnostics(__FILE__, __LINE__, "false failure "
                                                 + datum[i]->toString() + " == "
                                                 + "true");
         }
      }
      if (i != 1) {
         if (*datum[i] == (bool)false) {
            if (i != 1) {
               flag = false;
               writeDiagnostics(__FILE__, __LINE__, "false success "
                                                    + datum[i]->toString() + " == "
                                                    + "false");
             }
         }
         if (!(*datum[i] != (bool)false)) {
               flag = false;
               writeDiagnostics(__FILE__, __LINE__, "false failure "
                                                    + datum[i]->toString() + " == "
                                                    + "false");
         }
      }
      if (*datum[i] == (CHAR)81) {
         if (i != 1) {
            flag = false;
            writeDiagnostics(__FILE__, __LINE__, "false success "
                                                 + datum[i]->toString() + " == "
                                                 + "0x51 Q");
          }
      }
      if (!(*datum[i] != (CHAR)81)) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, "false failure "
                                             + datum[i]->toString() + " == "
                                             + "0x51 Q");
      }
      if (*datum[i] == (UCHAR)80) {
         if (i != 1) {
            flag = false;
            writeDiagnostics(__FILE__, __LINE__, "false success "
                                                 + datum[i]->toString() + " == "
                                                 + "0x50 P");
          }
      }
      if (!(*datum[i] != (UCHAR)80)) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, "false failure "
                                              + datum[i]->toString() + " == "
                                              + "0x50 P");
      }
      if (*datum[i] == (LONG)50) {
         if (i != 1) {
            flag = false;
            writeDiagnostics(__FILE__, __LINE__, "false success "
                                                 + datum[i]->toString() + " == "
                                                 + "50");
          }
      }
      if (!(*datum[i] != (LONG)50)) {
            flag = false;
            writeDiagnostics(__FILE__, __LINE__, "false failure "
                                                 + datum[i]->toString() + " == "
                                                 + "50");
      }
      if (*datum[i] == (DOUBLE)6.93) {
         if (i != 1) {
            flag = false;
            writeDiagnostics(__FILE__, __LINE__, "false success "
                                                 + datum[i]->toString() + " == "
                                                 + "6.93");
          }
      }
      if (!(*datum[i] != (DOUBLE)6.93)) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, "false failure "
                                              + datum[i]->toString() + " == "
                                              + "6.93");
      }
      if (i != 0) {
         if ((bool)true == *datum[i]) {
            if (i != 0) {
               flag = false;
               writeDiagnostics(__FILE__, __LINE__, "false success true == "
                                                    + datum[i]->toString());
             }
         }
         if (!((bool)true != *datum[i])) {
            flag = false;
            writeDiagnostics(__FILE__, __LINE__, "false failure true == "
                                                 + datum[i]->toString());
         }
      }
      if (i != 1) {
         if ((bool)false == *datum[i]) {
            if (i != 1) {
               flag = false;
               writeDiagnostics(__FILE__, __LINE__, "false success false == "
                                                    + datum[i]->toString());
             }
         }
         if (!((bool)false != *datum[i])) {
            flag = false;
            writeDiagnostics(__FILE__, __LINE__, "false failure false == "
                                                 + datum[i]->toString());
         }
      }
      if ((CHAR)81 == *datum[i]) {
         if (i != 1) {
            flag = false;
            writeDiagnostics(__FILE__, __LINE__, "false success 0x51 Q == "
                                                 + datum[i]->toString());
          }
      }
      if (!((CHAR)81 != *datum[i])) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, "false failure 0x51 Q == "
                                              + datum[i]->toString());
      }
      if ((UCHAR)80 == *datum[i]) {
         if (i != 1) {
            flag = false;
            writeDiagnostics(__FILE__, __LINE__, "false success 0x50 P == "
                                                 + datum[i]->toString());
          }
      }
      if (!((UCHAR)80 != *datum[i])) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, "false failure 0x50 P == "
                                              + datum[i]->toString());
      }
      if ((LONG)50 == *datum[i]) {
         if (i != 1) {
            flag = false;
            writeDiagnostics(__FILE__, __LINE__, "false success 50 == "
                                                 + datum[i]->toString());
          }
      }
      if (!((LONG)50 != *datum[i])) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, "false failure 50 == "
                                              + datum[i]->toString());
      }
      if ((DOUBLE)6.93 == *datum[i]) {
         if (i != 1) {
            flag = false;
            writeDiagnostics(__FILE__, __LINE__, "false success 6.93 == "
                                                 + datum[i]->toString());
          }
      }
      if (!((DOUBLE)6.93 != *datum[i])) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, "false failure 6.93 == "
                                              + datum[i]->toString());
      }
   }

   for(unsigned int i = 0; i < sizeof(datum)/sizeof(datum[0]); i++) delete datum[i];

   for(unsigned int i = 0; i < sizeof(slipCell)/sizeof(slipCell[0]); i++) {
      if (slipCell[i]->isHeader())
         ((SlipHeader*)slipCell[i])->deleteList();
      else
         delete slipCell[i];
   }

   header2->deleteList();
   header1->deleteList();
   if (!flag) testResultOutput("Test Equality Operators ", 0.0, flag);
   return flag;
}; // bool testEqualityOperators()

bool testLogicalOperators() {
   stringstream pretty;
   bool flag = true;
   const string type[] = { "BOOL", "BOOL", "CHAR", "UCHAR", "LONG", "ULONG", "DOUBLE" };
   SlipDatum* datum[]   = { new SlipDatum((bool)  false)
                          , new SlipDatum((bool)  true )
                          , new SlipDatum((CHAR)   0x31)
                          , new SlipDatum((UCHAR)  0x32)
                          , new SlipDatum((LONG)   0x33)
                          , new SlipDatum((ULONG)  0x34)
                          , new SlipDatum((DOUBLE) 53.0)
                          };
   const bool ans[7][7][6] =
  //                              <      <=     ==     >=     >      !=
  /*(bool)false*/            { {{false, true,  true,  true,  false, false}  // false
                             ,  {true,  true,  false, false, false, true }  // true
                             ,  {true,  true,  false, false, false, true }  // 0x31
                             ,  {true,  true,  false, false, false, true }  // 0x32
                             ,  {true,  true,  false, false, false, true }  // 0x33
                             ,  {true,  true,  false, false, false, true }  // 0x34
                             ,  {true,  true,  false, false, false, true }} // 53.0

 /*(bool)true */             , {{false, false, false,  true, true , true }  // false
                             ,  {false, true,  true,  true,  false, false}  // true
                             ,  {true,  true,  false, false, false, true }  // 0x31
                             ,  {true,  true,  false, false, false, true }  // 0x32
                             ,  {true,  true,  false, false, false, true }  // 0x33
                             ,  {true,  true,  false, false, false, true }  // 0x34
                             ,  {true,  true,  false, false, false, true }} // 53.0

 /*0x31       */             , {{false, false, false, true,  true , true }  // (bool) false
                             ,  {false, false, false, true,  true , true }  // (bool) true
                             ,  {false, true,  true,  true,  false, false}  // 0x31
                             ,  {true,  true,  false, false, false, true }  // 0x32
                             ,  {true,  true,  false, false, false, true }  // 0x33
                             ,  {true,  true,  false, false, false, true }  // 0x34
                             ,  {true,  true,  false, false, false, true }} // 53.0

 /*0x32       */             , {{false, false, false, true,  true , true }  // (bool) false
                             ,  {false, false, false, true,  true , true }  // (bool) true
                             ,  {false, false, false, true,  true , true }  // 0x31
                             ,  {false, true,  true,  true,  false, false}  // 0x32
                             ,  {true,  true,  false, false, false, true }  // 0x33
                             ,  {true,  true,  false, false, false, true }  // 0x34
                             ,  {true,  true,  false, false, false, true }} // 53.0

 /*0x33       */             , {{false, false, false, true,  true , true }  // (bool) false
                             ,  {false, false, false, true,  true , true }  // (bool) true
                             ,  {false, false, false, true,  true , true }  // 0x31
                             ,  {false, false, false, true,  true , true }  // 0x32
                             ,  {false, true,  true,  true,  false, false}  // 0x33
                             ,  {true,  true,  false, false, false, true }  // 0x34
                             ,  {true,  true,  false, false, false, true }} // 53.0

 /*0x34       */             , {{false, false, false, true,  true , true }  // (bool) false
                             ,  {false, false, false, true,  true , true }  // (bool) true
                             ,  {false, false, false, true,  true , true }  // 0x31
                             ,  {false, false, false, true,  true , true }  // 0x32
                             ,  {false, false, false, true,  true , true }  // 0x33
                             ,  {false, true,  true,  true,  false, false}  // 0x34
                             ,  {true,  true,  false, false, false, true }} // 53.0

 /*53.0   */                 , {{false, false, false, true,  true , true }  // (bool) false
                             ,  {false, false, false, true,  true , true }  // (bool) true
                             ,  {false, false, false, true,  true , true }  // 0x31
                             ,  {false, false, false, true,  true , true }  // 0x32
                             ,  {false, false, false, true,  true , true }  // 0x33
                             ,  {false, false, false, true,  true , true }  // 0x34
                             ,  {false, true,  true,  true,  false, false}} // 53.0
                             };

   for(unsigned int i = 0; i < sizeof(datum)/sizeof(datum[0]); i++) {
      for(unsigned int j = 0; j < sizeof(datum)/sizeof(datum[0]); j++) {
         if ((*datum[i] < *datum[j]) != (unsigned)ans[i][j][0]) {
            flag = false;
            pretty  << "LT failure (SlipDatum(" << type[i] << ") < SlipDatum(" << type[j] << "))  "
                    << datum[i]->toString() << "[" << i << "] < "
                    << datum[j]->toString() << "[" << j << "]"
                    << " |= " << boolalpha << ans[i][j][0];
            writeDiagnostics(__FILE__, __LINE__, pretty.str());
         }
         if ((*datum[i] <= *datum[j]) != ans[i][j][1]) {
            flag = false;
            pretty  << "LE failure (SlipDatum(" << type[i] << ") < SlipDatum(" << type[j] << "))  "
                    << datum[i]->toString() << "[" << i << "] <= "
                    << datum[j]->toString() << "[" << j << "]"
                    << " |= " << boolalpha << ans[i][j][1];
            writeDiagnostics(__FILE__, __LINE__, pretty.str());
         }
         if ((*datum[i] == *datum[j]) != ans[i][j][2]) {
            flag = false;
            pretty  << "EQ failure (SlipDatum(" << type[i] << ") < SlipDatum(" << type[j] << "))  "
                    << datum[i]->toString() << "[" << i << "] == "
                    << datum[j]->toString() << "[" << j << "]"
                    << " |= " << boolalpha << ans[i][j][2];
            writeDiagnostics(__FILE__, __LINE__, pretty.str());
         }
         if ((*datum[i] >= *datum[j]) != ans[i][j][3]) {
            flag = false;
            pretty  << "GE failure (SlipDatum(" << type[i] << ") < SlipDatum(" << type[j] << "))  "
                    << datum[i]->toString() << "[" << i << "] >= "
                    << datum[j]->toString() << "[" << j << "]"
                    << " |= " << boolalpha << ans[i][j][3];
            writeDiagnostics(__FILE__, __LINE__, pretty.str());
         }
         if ((*datum[i] > *datum[j]) != ans[i][j][4]) {
            flag = false;
            pretty  << "GT failure (SlipDatum(" << type[i] << ") < SlipDatum(" << type[j] << "))  "
                    << datum[i]->toString() << "[" << i << "] > "
                    << datum[j]->toString() << "[" << j << "]"
                    << " |= " << boolalpha << ans[i][j][4];
            writeDiagnostics(__FILE__, __LINE__, pretty.str());
         }
         if ((*datum[i] != *datum[j]) != ans[i][j][5]) {
            flag = false;
            pretty  << "NE failure (SlipDatum(" << type[i] << ") < SlipDatum(" << type[j] << "))  "
                    << datum[i]->toString() << "[" << i << "] !- "
                    << datum[j]->toString() << "[" << j << "]"
                    << " |= " << boolalpha << ans[i][j][4];
            writeDiagnostics(__FILE__, __LINE__, pretty.str());
         }
      }
   }

   for(unsigned int i = 0; i < sizeof(datum)/sizeof(datum[0]); i++) {
      if ((*datum[i] < (bool)false) != ans[i][0][0]) {
         flag = false;
         pretty  << "LT failure (SlipDatum(" << type[i] << ") < (bool)false)  "
                 << datum[i]->toString() << "[" << i << "] < "
                 << "false"
                 << " |= " << boolalpha << ans[i][0][0]<< endl;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] <= (bool)false) != ans[i][0][1])  {
         flag = false;
         pretty  << "LE failure (SlipDatum(" << type[i] << ") <= (bool)false)  "
                 << datum[i]->toString() << "[" << i << "] <= "
                 << "false"
                 << " |= " << boolalpha << ans[i][0][1];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] == (bool)false) != ans[i][0][2]) {
         flag = false;
         pretty  << "EQ failure (SlipDatum(" << type[i] << ") == (bool)false)  "
                 << datum[i]->toString() << "[" << i << "] == "
                 << "false"
                 << " |= " << boolalpha << ans[i][0][3];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] >= (bool)false) != ans[i][0][3]) {
         flag = false;
         pretty  << "GE failure (SlipDatum(" << type[i] << ") >= (bool)false)  "
                 << datum[i]->toString() << "[" << i << "] >= "
                 << "false"
                 << " |= " << boolalpha << ans[i][0][3]<< endl;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] > (bool)false) != ans[i][0][4]) {
         flag = false;
         pretty  << "GT failure (SlipDatum(" << type[i] << ") > (bool)false)  "
                 << datum[i]->toString() << "[" << i << "] > "
                 << "false"
                 << " |= " << ans[i][0][4];
            writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] != (bool)false) != ans[i][0][5]) {
         flag = false;
         pretty  << "NE failure (SlipDatum(" << type[i] << ") > (bool)false)  "
                 << datum[i]->toString() << "[" << i << "] > "
                 << "false"
                 << " |= " << boolalpha << ans[i][0][5];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((bool)false < *datum[i]) != ans[0][i][0]) {
         flag = false;
         pretty  << "LT failure ((bool)false < SlipDatum(" << type[i] << ")) "
                 << "false < "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[0][i][0];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((bool)false <= *datum[i]) != ans[0][i][1]) {
         flag = false;
         pretty  << "LE failure ((bool)false <= SlipDatum(" << type[i] << ")) "
                 << "false <= "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[0][i][1];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((bool)false == *datum[i]) != ans[0][i][2]) {
         flag = false;
         pretty  << "EQ failure ((bool)false == SlipDatum(" << type[i] << ")) "
                 << "false == "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[0][i][2];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((bool)false >= *datum[i]) != ans[0][i][3]) {
         flag = false;
         pretty  << "LE failure ((bool)false >= SlipDatum(" << type[i] << ")) "
                 << "false >= "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[0][i][3];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((bool)false > *datum[i]) != ans[0][i][4]) {
         flag = false;
         pretty  << "LE failure ((bool)false > SlipDatum(" << type[i] << ")) "
                 << "false > "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[0][i][4];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((bool)false != *datum[i]) != ans[0][i][5]) {
         flag = false;
         pretty  << "LE failure ((bool)false != SlipDatum(" << type[i] << ")) "
                 << "false != "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[0][i][5];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] < (bool)true) != ans[i][1][0]) {
         flag = false;
         pretty  << "LT failure (SlipDatum(" << type[i] << ") < (bool)true)  "
                 << datum[i]->toString() << "[" << i << "] < "
                 << "true"
                 << " |= " << boolalpha << ans[i][1][0]<< endl;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] <= (bool)true) != ans[i][1][1])  {
         flag = false;
         pretty  << "LE failure (SlipDatum(" << type[i] << ") <= (bool)true)  "
                 << datum[i]->toString() << "[" << i << "] <= "
                 << "true"
                 << " |= " << boolalpha << ans[i][1][1];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] == (bool)true) != ans[i][1][2]) {
         flag = false;
         pretty  << "EQ failure (SlipDatum(" << type[i] << ") == (bool)true)  "
                 << datum[i]->toString() << "[" << i << "] == "
                 << "true"
                 << " |= " << boolalpha << ans[i][1][3];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] >= (bool)true) != ans[i][1][3]) {
         flag = false;
         pretty  << "GE failure (SlipDatum(" << type[i] << ") >= (bool)true)  "
                 << datum[i]->toString() << "[" << i << "] >= "
                 << "true"
                 << " |= " << boolalpha << ans[i][1][3]<< endl;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] > (bool)true) != ans[i][1][4]) {
         flag = false;
         pretty  << "GT failure (SlipDatum(" << type[i] << ") > (bool)true)  "
                 << datum[i]->toString() << "[" << i << "] > "
                 << "true"
                 << " |= " << ans[i][1][4];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] != (bool)true) != ans[i][1][5]) {
         flag = false;
         pretty  << "NE failure (SlipDatum(" << type[i] << ") > (bool)true)  "
                 << datum[i]->toString() << "[" << i << "] > "
                 << "true"
                 << " |= " << boolalpha << ans[i][1][5];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((bool)true < *datum[i]) != ans[1][i][0]) {
         flag = false;
         pretty  << "LT failure ((bool)true < SlipDatum(" << type[i] << ")) "
                 << "true < "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[1][i][0];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((bool)true <= *datum[i]) != ans[1][i][1]) {
         flag = false;
         pretty  << "LE failure ((bool)true <= SlipDatum(" << type[i] << ")) "
                 << "true <= "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[1][i][1];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((bool)true == *datum[i]) != ans[1][i][2]) {
         flag = false;
         pretty  << "EQ failure ((bool)true == SlipDatum(" << type[i] << ")) "
                 << "true == "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[1][i][2];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((bool)true >= *datum[i]) != ans[1][i][3]) {
         flag = false;
         pretty  << "LE failure ((bool)true >= SlipDatum(" << type[i] << ")) "
                 << "true >= "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[1][i][3];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((bool)true > *datum[i]) != ans[1][i][4]) {
         flag = false;
         pretty  << "LE failure ((bool)true > SlipDatum(" << type[i] << ")) "
                 << "true > "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[1][i][4];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((bool)true != *datum[i]) != ans[1][i][5]) {
         flag = false;
         pretty  << "LE failure ((bool)true != SlipDatum(" << type[i] << ")) "
                 << "true != "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[1][i][5];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] < (CHAR)0x31) != ans[i][2][0]) {
         flag = false;
         pretty  << "LT failure (SlipDatum(" << type[i] << ") < (CHAR)0x31)  "
                 << datum[i]->toString() << "[" << i << "] < "
                 << "49"
                 << " |= " << boolalpha << ans[i][2][0]<< endl;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] <= (CHAR)0x31) != ans[i][2][1])  {
         flag = false;
         pretty  << "LE failure (SlipDatum(" << type[i] << ") <= (CHAR)0x31)  "
                 << datum[i]->toString() << "[" << i << "] <= "
                 << "49"
                 << " |= " << boolalpha << ans[i][2][1];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] == (CHAR)0x31) != ans[i][2][2]) {
         flag = false;
         pretty  << "EQ failure (SlipDatum(" << type[i] << ") == (CHAR)0x31)  "
                 << datum[i]->toString() << "[" << i << "] == "
                 << "49"
                 << " |= " << boolalpha << ans[i][2][3];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] >= (CHAR)0x31) != ans[i][2][3]) {
         flag = false;
         pretty  << "GE failure (SlipDatum(" << type[i] << ") >= (CHAR)0x31)  "
                 << datum[i]->toString() << "[" << i << "] >= "
                 << "49"
                 << " |= " << boolalpha << ans[i][2][3]<< endl;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] > (CHAR)0x31) != ans[i][2][4]) {
         flag = false;
         pretty  << "GT failure (SlipDatum(" << type[i] << ") > (CHAR)0x31)  "
                 << datum[i]->toString() << "[" << i << "] > "
                 << "49"
                 << " |= " << ans[i][2][4];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] != (CHAR)0x31) != ans[i][2][5]) {
         flag = false;
         pretty  << "NE failure (SlipDatum(" << type[i] << ") > (CHAR)0x31)  "
                 << datum[i]->toString() << "[" << i << "] > "
                 << "49"
                 << " |= " << boolalpha << ans[i][2][5];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((CHAR)0x31 < *datum[i]) != ans[2][i][0]) {
         flag = false;
         pretty  << "LT failure ((CHAR)0x31 < SlipDatum(" << type[i] << ")) "
                 << "49 < "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[2][i][0];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((CHAR)0x31 <= *datum[i]) != ans[2][i][1]) {
         flag = false;
         pretty  << "LE failure ((CHAR)0x31 <= SlipDatum(" << type[i] << ")) "
                 << "49 <= "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[2][i][1];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((CHAR)0x31 == *datum[i]) != ans[2][i][2]) {
         flag = false;
         pretty  << "EQ failure ((CHAR)0x31 == SlipDatum(" << type[i] << ")) "
                 << "49 == "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[2][i][2];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((CHAR)0x31 >= *datum[i]) != ans[2][i][3]) {
         flag = false;
         pretty  << "LE failure ((CHAR)0x31 >= SlipDatum(" << type[i] << ")) "
                 << "49 >= "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[2][i][3];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((CHAR)0x31 > *datum[i]) != ans[2][i][4]) {
         flag = false;
         pretty  << "LE failure ((CHAR)0x31 > SlipDatum(" << type[i] << ")) "
                 << "49 > "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[2][i][4];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((CHAR)0x31 != *datum[i]) != ans[2][i][5]) {
         flag = false;
         pretty  << "LE failure ((CHAR)0x31 != SlipDatum(" << type[i] << ")) "
                 << "49 != "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[2][i][5];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] < (UCHAR)0x32) != ans[i][3][0]) {
         flag = false;
         pretty  << "LT failure (SlipDatum(" << type[i] << ") < (UCHAR)0x32)  "
                 << datum[i]->toString() << "[" << i << "] < "
                 << "50"
                 << " |= " << boolalpha << ans[i][3][0]<< endl;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] <= (UCHAR)0x32) != ans[i][3][1])  {
         flag = false;
         pretty  << "LE failure (SlipDatum(" << type[i] << ") <= (UCHAR)0x32)  "
                 << datum[i]->toString() << "[" << i << "] <= "
                 << "50"
                 << " |= " << boolalpha << ans[i][3][1];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] == (UCHAR)0x32) != ans[i][3][2]) {
         flag = false;
         pretty  << "EQ failure (SlipDatum(" << type[i] << ") == (UCHAR)0x32)  "
                 << datum[i]->toString() << "[" << i << "] == "
                 << "50"
                 << " |= " << boolalpha << ans[i][3][3];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] >= (UCHAR)0x32) != ans[i][3][3]) {
         flag = false;
         pretty  << "GE failure (SlipDatum(" << type[i] << ") >= (UCHAR)0x32)  "
                 << datum[i]->toString() << "[" << i << "] >= "
                 << "50"
                 << " |= " << boolalpha << ans[i][3][3]<< endl;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] > (UCHAR)0x32) != ans[i][3][4]) {
         flag = false;
         pretty  << "GT failure (SlipDatum(" << type[i] << ") > (UCHAR)0x32)  "
                 << datum[i]->toString() << "[" << i << "] > "
                 << "50"
                 << " |= " << ans[i][3][4];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] != (UCHAR)0x32) != ans[i][3][5]) {
         flag = false;
         pretty  << "NE failure (SlipDatum(" << type[i] << ") > (UCHAR)0x32)  "
                 << datum[i]->toString() << "[" << i << "] > "
                 << "50"
                 << " |= " << boolalpha << ans[i][3][5];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((UCHAR)0x32 < *datum[i]) != ans[3][i][0]) {
         flag = false;
         pretty  << "LT failure ((UCHAR)0x32 < SlipDatum(" << type[i] << ")) "
                 << "50 < "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[3][i][0];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((UCHAR)0x32 <= *datum[i]) != ans[3][i][1]) {
         flag = false;
         pretty  << "LE failure ((UCHAR)0x32 <= SlipDatum(" << type[i] << ")) "
                 << "50 <= "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[3][i][1];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((UCHAR)0x32 == *datum[i]) != ans[3][i][2]) {
         flag = false;
         pretty  << "EQ failure ((UCHAR)0x32 == SlipDatum(" << type[i] << ")) "
                 << "50 == "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[3][i][2];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((UCHAR)0x32 >= *datum[i]) != ans[3][i][3]) {
         flag = false;
         pretty  << "LE failure ((UCHAR)0x32 >= SlipDatum(" << type[i] << ")) "
                 << "50 >= "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[3][i][3];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((UCHAR)0x32 > *datum[i]) != ans[3][i][4]) {
         flag = false;
         pretty  << "LE failure ((UCHAR)0x32 > SlipDatum(" << type[i] << ")) "
                 << "50 > "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[3][i][4];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((UCHAR)0x32 != *datum[i]) != ans[3][i][5]) {
         flag = false;
         pretty  << "LE failure ((UCHAR)0x32 != SlipDatum(" << type[i] << ")) "
                 << "50 != "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[3][i][5];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] < (LONG)0x33) != ans[i][4][0]) {
         flag = false;
         pretty  << "LT failure (SlipDatum(" << type[i] << ") < (LONG)0x33)  "
                 << datum[i]->toString() << "[" << i << "] < "
                 << "51"
                 << " |= " << boolalpha << ans[i][4][0]<< endl;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] <= (LONG)0x33) != ans[i][4][1])  {
         flag = false;
         pretty  << "LE failure (SlipDatum(" << type[i] << ") <= (LONG)0x33)  "
                 << datum[i]->toString() << "[" << i << "] <= "
                 << "51"
                 << " |= " << boolalpha << ans[i][4][1];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] == (LONG)0x33) != ans[i][4][2]) {
         flag = false;
         pretty  << "EQ failure (SlipDatum(" << type[i] << ") == (LONG)0x33)  "
                 << datum[i]->toString() << "[" << i << "] == "
                 << "51"
                 << " |= " << boolalpha << ans[i][4][3];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] >= (LONG)0x33) != ans[i][4][3]) {
         flag = false;
         pretty  << "GE failure (SlipDatum(" << type[i] << ") >= (LONG)0x33)  "
                 << datum[i]->toString() << "[" << i << "] >= "
                 << "51"
                 << " |= " << boolalpha << ans[i][4][3]<< endl;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] > (LONG)0x33) != ans[i][4][4]) {
         flag = false;
         pretty  << "GT failure (SlipDatum(" << type[i] << ") > (LONG)0x33)  "
                 << datum[i]->toString() << "[" << i << "] > "
                 << "51"
                 << " |= " << ans[i][4][4];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] != (LONG)0x33) != ans[i][4][5]) {
         flag = false;
         pretty  << "NE failure (SlipDatum(" << type[i] << ") > (LONG)0x33)  "
                 << datum[i]->toString() << "[" << i << "] > "
                 << "51"
                 << " |= " << boolalpha << ans[i][4][5];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((LONG)0x33 < *datum[i]) != ans[4][i][0]) {
         flag = false;
         pretty  << "LT failure ((LONG)0x33 < SlipDatum(" << type[i] << ")) "
                 << "51 < "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[4][i][0];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((LONG)0x33 <= *datum[i]) != ans[4][i][1]) {
         flag = false;
         pretty  << "LE failure ((LONG)0x33 <= SlipDatum(" << type[i] << ")) "
                 << "51 <= "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[4][i][1];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((LONG)0x33 == *datum[i]) != ans[4][i][2]) {
         flag = false;
         pretty  << "EQ failure ((LONG)0x33 == SlipDatum(" << type[i] << ")) "
                 << "51 == "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[4][i][2];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((LONG)0x33 >= *datum[i]) != ans[4][i][3]) {
         flag = false;
         pretty  << "LE failure ((LONG)0x33 >= SlipDatum(" << type[i] << ")) "
                 << "51 >= "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[4][i][3];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((LONG)0x33 > *datum[i]) != ans[4][i][4]) {
         flag = false;
         pretty  << "LE failure ((LONG)0x33 > SlipDatum(" << type[i] << ")) "
                 << "51 > "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[4][i][4];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((LONG)0x33 != *datum[i]) != ans[4][i][5]) {
         flag = false;
         pretty  << "LE failure ((LONG)0x33 != SlipDatum(" << type[i] << ")) "
                 << "51 != "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[4][i][5];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] < (ULONG)0x34) != ans[i][5][0]) {
         flag = false;
         pretty  << "LT failure (SlipDatum(" << type[i] << ") < (ULONG)0x34)  "
                 << datum[i]->toString() << "[" << i << "] < "
                 << "52"
                 << " |= " << boolalpha << ans[i][5][0]<< endl;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] <= (ULONG)0x34) != ans[i][5][1])  {
         flag = false;
         pretty  << "LE failure (SlipDatum(" << type[i] << ") <= (ULONG)0x34)  "
                 << datum[i]->toString() << "[" << i << "] <= "
                 << "52"
                 << " |= " << boolalpha << ans[i][5][1];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] == (ULONG)0x34) != ans[i][5][2]) {
         flag = false;
         pretty  << "EQ failure (SlipDatum(" << type[i] << ") == (ULONG)0x34)  "
                 << datum[i]->toString() << "[" << i << "] == "
                 << "52"
                 << " |= " << boolalpha << ans[i][5][3];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] >= (ULONG)0x34) != ans[i][5][3]) {
         flag = false;
         pretty  << "GE failure (SlipDatum(" << type[i] << ") >= (ULONG)0x34)  "
                 << datum[i]->toString() << "[" << i << "] >= "
                 << "52"
                 << " |= " << boolalpha << ans[i][5][3]<< endl;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] > (ULONG)0x34) != ans[i][5][4]) {
         flag = false;
         pretty  << "GT failure (SlipDatum(" << type[i] << ") > (ULONG)0x34)  "
                 << datum[i]->toString() << "[" << i << "] > "
                 << "52"
                 << " |= " << ans[i][5][4];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] != (ULONG)0x34) != ans[i][5][5]) {
         flag = false;
         pretty  << "NE failure (SlipDatum(" << type[i] << ") > (ULONG)0x34)  "
                 << datum[i]->toString() << "[" << i << "] > "
                 << "52"
                 << " |= " << boolalpha << ans[i][5][5];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((ULONG)0x34 < *datum[i]) != ans[5][i][0]) {
         flag = false;
         pretty  << "LT failure ((ULONG)0x34 < SlipDatum(" << type[i] << ")) "
                 << "52 < "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[5][i][0];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((ULONG)0x34 <= *datum[i]) != ans[5][i][1]) {
         flag = false;
         pretty  << "LE failure ((ULONG)0x34 <= SlipDatum(" << type[i] << ")) "
                 << "52 <= "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[5][i][1];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((ULONG)0x34 == *datum[i]) != ans[5][i][2]) {
         flag = false;
         pretty  << "EQ failure ((ULONG)0x34 == SlipDatum(" << type[i] << ")) "
                 << "52 == "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[5][i][2];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((ULONG)0x34 >= *datum[i]) != ans[5][i][3]) {
         flag = false;
         pretty  << "LE failure ((ULONG)0x34 >= SlipDatum(" << type[i] << ")) "
                 << "52 >= "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[5][i][3];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((ULONG)0x34 > *datum[i]) != ans[5][i][4]) {
         flag = false;
         pretty  << "LE failure ((ULONG)0x34 > SlipDatum(" << type[i] << ")) "
                 << "52 > "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[5][i][4];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((ULONG)0x34 != *datum[i]) != ans[5][i][5]) {
         flag = false;
         pretty  << "LE failure ((ULONG)0x34 != SlipDatum(" << type[i] << ")) "
                 << "52 != "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[5][i][5];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] < (DOUBLE)53.0) != ans[i][6][0]) {
         flag = false;
         pretty  << "LT failure (SlipDatum(" << type[i] << ") < (DOUBLE)53.0)  "
                 << datum[i]->toString() << "[" << i << "] < "
                 << "53.0"
                 << " |= " << boolalpha << ans[i][6][0]<< endl;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] <= (DOUBLE)53.0) != ans[i][6][1])  {
         flag = false;
         pretty  << "LE failure (SlipDatum(" << type[i] << ") <= (DOUBLE)53.0)  "
                 << datum[i]->toString() << "[" << i << "] <= "
                 << "53.0"
                 << " |= " << boolalpha << ans[i][6][1];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] == (DOUBLE)53.0) != ans[i][6][2]) {
         flag = false;
         pretty  << "EQ failure (SlipDatum(" << type[i] << ") == (DOUBLE)53.0)  "
                 << datum[i]->toString() << "[" << i << "] == "
                 << "53.0"
                 << " |= " << boolalpha << ans[i][6][3];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] >= (DOUBLE)53.0) != ans[i][6][3]) {
         flag = false;
         pretty  << "GE failure (SlipDatum(" << type[i] << ") >= (DOUBLE)53.0)  "
                 << datum[i]->toString() << "[" << i << "] >= "
                 << "53.0"
                 << " |= " << boolalpha << ans[i][6][3]<< endl;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] > (DOUBLE)53.0) != ans[i][6][4]) {
         flag = false;
         pretty  << "GT failure (SlipDatum(" << type[i] << ") > (DOUBLE)53.0)  "
                 << datum[i]->toString() << "[" << i << "] > "
                 << "53.0"
                 << " |= " << ans[i][6][4];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((*datum[i] != (DOUBLE)53.0) != ans[i][6][5]) {
         flag = false;
         pretty  << "NE failure (SlipDatum(" << type[i] << ") > (DOUBLE)53.0)  "
                 << datum[i]->toString() << "[" << i << "] > "
                 << "53.0"
                 << " |= " << boolalpha << ans[i][6][5];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((DOUBLE)53.0 < *datum[i]) != ans[6][i][0]) {
         flag = false;
         pretty  << "LT failure ((DOUBLE)53.0 < SlipDatum(" << type[i] << ")) "
                 << "53.0 < "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[6][i][0];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((DOUBLE)53.0 <= *datum[i]) != ans[6][i][1]) {
         flag = false;
         pretty  << "LE failure ((DOUBLE)53.0 <= SlipDatum(" << type[i] << ")) "
                 << "53.0 <= "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[6][i][1];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((DOUBLE)53.0 == *datum[i]) != ans[6][i][2]) {
         flag = false;
         pretty  << "EQ failure ((DOUBLE)53.0 == SlipDatum(" << type[i] << ")) "
                 << "53.0 == "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[6][i][2];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((DOUBLE)53.0 >= *datum[i]) != ans[6][i][3]) {
         flag = false;
         pretty  << "LE failure ((DOUBLE)53.0 >= SlipDatum(" << type[i] << ")) "
                 << "53.0 >= "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[6][i][3];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((DOUBLE)53.0 > *datum[i]) != ans[6][i][4]) {
         flag = false;
         pretty  << "LE failure ((DOUBLE)53.0 > SlipDatum(" << type[i] << ")) "
                 << "53.0 > "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[6][i][4];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (((DOUBLE)53.0 != *datum[i]) != ans[6][i][5]) {
         flag = false;
         pretty  << "LE failure ((DOUBLE)53.0 != SlipDatum(" << type[i] << ")) "
                 << "53.0 != "
                 << datum[i]->toString() << "[" << i << "]"
                 << " |= " << boolalpha << ans[6][i][5];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
   }

   for(unsigned int i = 0; i < sizeof(datum)/sizeof(datum[0]); i++) delete datum[i];

   if (!flag) testResultOutput("Test Logical Operators ", 0.0, flag);
   return flag;
}; // bool testLogicalOperators()
bool testMaskOperators() {
   stringstream pretty;
   bool flag = true;
   SlipDatum* datum[]   = { new SlipDatum((bool) true)
                          , new SlipDatum((bool) false)
                          , new SlipDatum((CHAR)  0x60)
                          , new SlipDatum((UCHAR) 0x60)
                          , new SlipDatum((LONG)  0x0000060)
                          , new SlipDatum((ULONG) 0x0000060)
                          };

   const ULONG ulongMask = 0x00005051;
   const LONG  longMask  = 0x00005051;
   const UCHAR ucharMask = 0x0051;
   const CHAR  charMask  = 0x0051;
   const bool  boolMask  = true;
   const ULONG longValue = 0x00000060;
   const UCHAR charValue = 0x0060;

   const ULONG longAnd[] = { 1 & ulongMask & 1,       0 & ulongMask & 0
                             , longValue & ulongMask, longValue & ulongMask
                             , longValue & ulongMask, longValue & ulongMask
                             };
   const UCHAR charAnd[] = { 1 & ucharMask & 1,       0 & ucharMask & 0
                             , charValue & ucharMask, charValue & ucharMask
                             , charValue & ucharMask, charValue & ucharMask
                             };
   const UCHAR boolAnd[] = { 1 & boolMask & 1,       0 & boolMask & 0
                             , charValue & boolMask, charValue & boolMask
                             , charValue & boolMask, charValue & boolMask
                             };

   const ULONG longOr[] =  { 1 | ulongMask | 1,       0 | ulongMask | 0
                             , longValue | ulongMask, longValue | ulongMask
                             , longValue | ulongMask, longValue | ulongMask
                             };
   const UCHAR charOr[] =  { 1 | ucharMask | 1,       0 | ucharMask | 0
                             , charValue | ucharMask, charValue | ucharMask
                             , charValue | ucharMask, charValue | ucharMask
                             };
   const UCHAR boolOr[] =  { 1 | boolMask,           0 | boolMask
                             , charValue | boolMask, charValue | boolMask
                             , charValue | boolMask, charValue | boolMask
                             };

   const ULONG longXor[]=  { 1 ^ ulongMask,           0 ^ ulongMask
                             , longValue ^ ulongMask, longValue ^ ulongMask
                             , longValue ^ ulongMask, longValue ^ ulongMask
                             };
   const UCHAR charXor[]=  { 1 ^ ucharMask,           0 ^ ucharMask ^ 0
                             , charValue ^ ucharMask, charValue ^ ucharMask
                             , charValue ^ ucharMask, charValue ^ ucharMask
                             };
   const UCHAR boolXor[]=  { 1 ^ boolMask,           0 ^ boolMask
                             , charValue ^ boolMask, charValue ^ boolMask
                             , charValue ^ boolMask, charValue ^ boolMask
                             };

   for(unsigned int i = 0; i < sizeof(datum)/sizeof(datum[0]); i++) {
      SlipDatum andDatum((*datum[i] & ulongMask));
      SlipDatum orDatum((*datum[i] | ulongMask));
      SlipDatum xorDatum((*datum[i] ^ ulongMask));

      if ((*datum[i] & ulongMask) != (LONG)longAnd[i]) {
         flag = false;
         pretty  << "and failure (SlipDatum & ULONG)"
                 << " 0x" << hex << setfill('0') << setw(8)<< (ULONG)*datum[i]
                 << "["      << dec << i << "]"
                 << " & 0x"  << hex << setfill('0') << setw(8) << ulongMask
                 << "  0x"   << hex << setfill('0') << setw(8) << (*datum[i] & ulongMask)
                    << " != 0x" << hex << setfill('0') << setw(8) << longAnd[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((ulongMask & *datum[i]) != (LONG)longAnd[i]) {
         flag = false;
         pretty  << "and failure (ULONG & SlipDatum)"
                 << " 0x"    << hex << setfill('0') << setw(8) << ulongMask
                 << " & 0x"  << hex << setfill('0') << setw(8) << (ULONG)*datum[i]
                 << "["      << dec << i << "] "
                 << " 0x"    << hex << setfill('0') << setw(8) << (ulongMask & *datum[i])
                 << " != 0x" << hex << setfill('0') << setw(8) << longAnd[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }

      if ((*datum[i] & longMask) != (LONG)longAnd[i]) {
         flag = false;
         pretty  << "and failure (SlipDatum & LONG)"
                 << " 0x" << hex << setfill('0') << setw(8)<< (LONG)*datum[i]
                 << "["      << dec << i << "]"
                 << " & 0x"  << hex << setfill('0') << setw(8) << longMask
                 << "  0x"   << hex << setfill('0') << setw(8) << (*datum[i] & longMask)
                    << " != 0x" << hex << setfill('0') << setw(8) << longAnd[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((longMask & *datum[i]) != (LONG)longAnd[i]) {
         flag = false;
         pretty  << "and failure (LONG & SlipDatum)"
                 << " 0x"    << hex << setfill('0') << setw(8) << longMask
                 << " & 0x"  << hex << setfill('0') << setw(8) << (LONG)*datum[i]
                 << "["      << dec << i << "] "
                 << " 0x"    << hex << setfill('0') << setw(8) << (longMask & *datum[i])
                 << " != 0x" << hex << setfill('0') << setw(8) << longAnd[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }

      if ((*datum[i] & ucharMask) != charAnd[i]) {
         flag = false;
         pretty  << "and failure (SlipDatum & UCHAR)"
                 << " 0x" << hex << setfill('0') << setw(8)<< (UCHAR)*datum[i]
                 << "["      << dec << i << "]"
                 << " & 0x"  << hex << setfill('0') << setw(8) << ucharMask
                 << "  0x"   << hex << setfill('0') << setw(8) << (*datum[i] & ucharMask)
                    << " != 0x" << hex << setfill('0') << setw(8) << charAnd[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((ucharMask & *datum[i]) != charAnd[i]) {
         flag = false;
         pretty  << "and failure (UCHAR & SlipDatum)"
                 << " 0x"    << hex << setfill('0') << setw(8) << ucharMask
                 << " & 0x"  << hex << setfill('0') << setw(8) << (UCHAR)*datum[i]
                 << "["      << dec << i << "] "
                 << " 0x"    << hex << setfill('0') << setw(8) << (ucharMask & *datum[i])
                 << " != 0x" << hex << setfill('0') << setw(8) << charAnd[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }

      if ((*datum[i] & charMask) != charAnd[i]) {
         flag = false;
         pretty  << "and failure (SlipDatum & CHAR)"
                 << " 0x" << hex << setfill('0') << setw(8)<< (CHAR)*datum[i]
                 << "["      << dec << i << "]"
                 << " & 0x"  << hex << setfill('0') << setw(8) << charMask
                 << "  0x"   << hex << setfill('0') << setw(8) << (*datum[i] & charMask)
                    << " != 0x" << hex << setfill('0') << setw(8) << charAnd[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((charMask & *datum[i]) != charAnd[i]) {
         flag = false;
         pretty  << "and failure (CHAR & SlipDatum)"
                 << " 0x"    << hex << setfill('0') << setw(8) << charMask
                 << " & 0x"  << hex << setfill('0') << setw(8) << (CHAR)*datum[i]
                 << "["      << dec << i << "] "
                 << " 0x"    << hex << setfill('0') << setw(8) << (charMask & *datum[i])
                 << " != 0x" << hex << setfill('0') << setw(8) << charAnd[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }

      if ((*datum[i] & boolMask) != boolAnd[i]) {
         flag = false;
         pretty  << "and failure (SlipDatum & bool)"
                 << " 0x" << hex << setfill('0') << setw(8)<< (bool)*datum[i]
                 << "["      << dec << i << "]"
                 << " & 0x"  << hex << setfill('0') << setw(8) << boolMask
                 << "  0x"   << hex << setfill('0') << setw(8) << (*datum[i] & boolMask)
                    << " != 0x" << hex << setfill('0') << setw(8) << boolAnd[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((boolMask & *datum[i]) != boolAnd[i]) {
         flag = false;
         pretty  << "and failure (bool & SlipDatum)"
                 << " 0x"    << hex << setfill('0') << setw(8) << boolMask
                 << " & 0x"  << hex << setfill('0') << setw(8) << (bool)*datum[i]
                 << "["      << dec << i << "] "
                 << " 0x"    << hex << setfill('0') << setw(8) << (boolMask & *datum[i])
                 << " != 0x" << hex << setfill('0') << setw(8) << boolAnd[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (andDatum != longAnd[i]) {
         flag = false;
         pretty  << "and failure (SlipDatum)        "
                 << " 0x"    << hex << setfill('0') << setw(8)<< (LONG)*datum[i]
                 << "["      << dec << i << "]  "
                 << " 0x"    << hex << setfill('0') << setw(8) << (LONG)andDatum
                 << " != 0x" << hex << setfill('0') << setw(8) << longAnd[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }


      if ((*datum[i] | ulongMask) != (LONG)longOr[i]) {
         flag = false;
         pretty  << "or failure (SlipDatum | ULONG)"
                 << " 0x" << hex << setfill('0') << setw(8)<< (ULONG)*datum[i]
                 << "["      << dec << i << "]"
                 << " | 0x"  << hex << setfill('0') << setw(8) << ulongMask
                 << "  0x"   << hex << setfill('0') << setw(8) << (*datum[i] | ulongMask)
                 << " != 0x" << hex << setfill('0') << setw(8) << longOr[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((ulongMask | *datum[i]) != (LONG)longOr[i]) {
         flag = false;
         pretty  << "or failure (ULONG | SlipDatum)"
                 << " 0x"    << hex << setfill('0') << setw(8) << ulongMask
                 << " | 0x"  << hex << setfill('0') << setw(8) << (ULONG)*datum[i]
                 << "["      << dec << i << "] "
                 << " 0x"    << hex << setfill('0') << setw(8) << (ulongMask | *datum[i])
                 << " != 0x" << hex << setfill('0') << setw(8) << longOr[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }

      if ((*datum[i] | longMask) != (LONG)longOr[i]) {
         flag = false;
         pretty  << "or failure (SlipDatum | LONG)"
                 << " 0x" << hex << setfill('0') << setw(8)<< (LONG)*datum[i]
                 << "["      << dec << i << "]"
                 << " | 0x"  << hex << setfill('0') << setw(8) << longMask
                 << "  0x"   << hex << setfill('0') << setw(8) << (*datum[i] | longMask)
                    << " != 0x" << hex << setfill('0') << setw(8) << longOr[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((longMask | *datum[i]) != (LONG)longOr[i]) {
         flag = false;
         pretty  << "or failure (LONG | SlipDatum)"
                 << " 0x"    << hex << setfill('0') << setw(8) << longMask
                 << " | 0x"  << hex << setfill('0') << setw(8) << (LONG)*datum[i]
                 << "["      << dec << i << "] "
                 << " 0x"    << hex << setfill('0') << setw(8) << (longMask | *datum[i])
                 << " != 0x" << hex << setfill('0') << setw(8) << longOr[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }

      if ((*datum[i] | ucharMask) != charOr[i]) {
         flag = false;
         pretty  << "or failure (SlipDatum | UCHAR)"
                 << " 0x" << hex << setfill('0') << setw(8)<< (UCHAR)*datum[i]
                 << "["      << dec << i << "]"
                 << " | 0x"  << hex << setfill('0') << setw(8) << ucharMask
                 << "  0x"   << hex << setfill('0') << setw(8) << (*datum[i] | ucharMask)
                    << " != 0x" << hex << setfill('0') << setw(8) << charOr[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((ucharMask | *datum[i]) != charOr[i]) {
         flag = false;
         pretty  << "or failure (UCHAR | SlipDatum)"
                 << " 0x"    << hex << setfill('0') << setw(8) << ucharMask
                 << " | 0x"  << hex << setfill('0') << setw(8) << (UCHAR)*datum[i]
                 << "["      << dec << i << "] "
                 << " 0x"    << hex << setfill('0') << setw(8) << (ucharMask | *datum[i])
                 << " != 0x" << hex << setfill('0') << setw(8) << charOr[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }

      if ((*datum[i] | charMask) != charOr[i]) {
         flag = false;
         pretty  << "or failure (SlipDatum | CHAR)"
                 << " 0x" << hex << setfill('0') << setw(8)<< (CHAR)*datum[i]
                 << "["      << dec << i << "]"
                 << " | 0x"  << hex << setfill('0') << setw(8) << charMask
                 << "  0x"   << hex << setfill('0') << setw(8) << (*datum[i] | charMask)
                    << " != 0x" << hex << setfill('0') << setw(8) << charOr[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((charMask | *datum[i]) != charOr[i]) {
         flag = false;
         pretty  << "or failure (CHAR | SlipDatum)"
                 << " 0x"    << hex << setfill('0') << setw(8) << charMask
                 << " | 0x"  << hex << setfill('0') << setw(8) << (CHAR)*datum[i]
                 << "["      << dec << i << "] "
                 << " 0x"    << hex << setfill('0') << setw(8) << (charMask | *datum[i])
                 << " != 0x" << hex << setfill('0') << setw(8) << charOr[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }

      if ((*datum[i] | boolMask) != boolOr[i]) {
         flag = false;
         pretty  << "or failure (SlipDatum | bool)"
                 << " 0x" << hex << setfill('0') << setw(8)<< (bool)*datum[i]
                 << "["      << dec << i << "]"
                 << " | 0x"  << hex << setfill('0') << setw(8) << boolMask
                 << "  0x"   << hex << setfill('0') << setw(8) << (*datum[i] | boolMask)
                    << " != 0x" << hex << setfill('0') << setw(8) << boolOr[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((boolMask | *datum[i]) != boolOr[i]) {
         flag = false;
         pretty  << "or failure (bool | SlipDatum)"
                 << " 0x"    << hex << setfill('0') << setw(8) << boolMask
                 << " | 0x"  << hex << setfill('0') << setw(8) << (bool)*datum[i]
                 << "["      << dec << i << "] "
                 << " 0x"    << hex << setfill('0') << setw(8) << (boolMask | *datum[i])
                 << " != 0x" << hex << setfill('0') << setw(8) << boolOr[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (orDatum != (LONG)longOr[i]) {
         flag = false;
         pretty  << "or failure (SlipDatum)        "
                 << " 0x"    << hex << setfill('0') << setw(8)<< (ULONG)*datum[i]
                 << "["      << dec << i << "]  "
                 << " 0x"    << hex << setfill('0') << setw(8) << (ULONG)andDatum
                 << " != 0x" << hex << setfill('0') << setw(8) << longOr[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }


      if ((*datum[i] ^ ulongMask) != (LONG)longXor[i]) {
         flag = false;
         pretty  << "xor failure (SlipDatum ^ ULONG)"
                 << " 0x" << hex << setfill('0') << setw(8)<< (ULONG)*datum[i]
                 << "["      << dec << i << "]"
                 << " ^ 0x"  << hex << setfill('0') << setw(8) << ulongMask
                 << "  0x"   << hex << setfill('0') << setw(8) << (*datum[i] ^ ulongMask)
                 << " != 0x" << hex << setfill('0') << setw(8) << longXor[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((ulongMask ^ *datum[i]) != (LONG)longXor[i]) {
         flag = false;
         pretty  << "xor failure (ULONG ^ SlipDatum)"
                 << " 0x"    << hex << setfill('0') << setw(8) << ulongMask
                 << " ^ 0x"  << hex << setfill('0') << setw(8) << (ULONG)*datum[i]
                 << "["      << dec << i << "] "
                 << " 0x"    << hex << setfill('0') << setw(8) << (ulongMask ^ *datum[i])
                 << " != 0x" << hex << setfill('0') << setw(8) << longXor[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }

      if ((*datum[i] ^ longMask) != (LONG)longXor[i]) {
         flag = false;
         pretty  << "xor failure (SlipDatum ^ LONG)"
                 << " 0x" << hex << setfill('0') << setw(8)<< (LONG)*datum[i]
                 << "["      << dec << i << "]"
                 << " ^ 0x"  << hex << setfill('0') << setw(8) << longMask
                 << "  0x"   << hex << setfill('0') << setw(8) << (*datum[i] ^ longMask)
                    << " != 0x" << hex << setfill('0') << setw(8) << longXor[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((longMask ^ *datum[i]) != (LONG)longXor[i]) {
         flag = false;
         pretty  << "xor failure (LONG ^ SlipDatum)"
                 << " 0x"    << hex << setfill('0') << setw(8) << longMask
                 << " ^ 0x"  << hex << setfill('0') << setw(8) << (LONG)*datum[i]
                 << "["      << dec << i << "] "
                 << " 0x"    << hex << setfill('0') << setw(8) << (longMask ^ *datum[i])
                 << " != 0x" << hex << setfill('0') << setw(8) << longXor[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }

      if ((*datum[i] ^ ucharMask) != charXor[i]) {
         flag = false;
         pretty  << "xor failure (SlipDatum ^ UCHAR)"
                 << " 0x" << hex << setfill('0') << setw(8)<< (UCHAR)*datum[i]
                 << "["      << dec << i << "]"
                 << " ^ 0x"  << hex << setfill('0') << setw(8) << ucharMask
                 << "  0x"   << hex << setfill('0') << setw(8) << (*datum[i] ^ ucharMask)
                    << " != 0x" << hex << setfill('0') << setw(8) << charXor[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((ucharMask ^ *datum[i]) != charXor[i]) {
         flag = false;
         pretty  << "xor failure (UCHAR ^ SlipDatum)"
                 << " 0x"    << hex << setfill('0') << setw(8) << ucharMask
                 << " ^ 0x"  << hex << setfill('0') << setw(8) << (UCHAR)*datum[i]
                 << "["      << dec << i << "] "
                 << " 0x"    << hex << setfill('0') << setw(8) << (ucharMask ^ *datum[i])
                 << " != 0x" << hex << setfill('0') << setw(8) << charXor[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }

      if ((*datum[i] ^ charMask) != charXor[i]) {
         flag = false;
         pretty  << "xor failure (SlipDatum ^ CHAR)"
                 << " 0x" << hex << setfill('0') << setw(8)<< (CHAR)*datum[i]
                 << "["      << dec << i << "]"
                 << " ^ 0x"  << hex << setfill('0') << setw(8) << charMask
                 << "  0x"   << hex << setfill('0') << setw(8) << (*datum[i] ^ charMask)
                 << " != 0x" << hex << setfill('0') << setw(8) << charXor[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((charMask ^ *datum[i]) != charXor[i]) {
         flag = false;
         pretty  << "xor failure (CHAR ^ SlipDatum)"
                 << " 0x"    << hex << setfill('0') << setw(8) << charMask
                 << " ^ 0x"  << hex << setfill('0') << setw(8) << (CHAR)*datum[i]
                 << "["      << dec << i << "] "
                 << " 0x"    << hex << setfill('0') << setw(8) << (charMask ^ *datum[i])
                 << " != 0x" << hex << setfill('0') << setw(8) << charXor[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }

      if ((*datum[i] ^ boolMask) != boolXor[i]) {
         flag = false;
         pretty  << "xor failure (SlipDatum ^ bool)"
                 << " 0x" << hex << setfill('0') << setw(8)<< (bool)*datum[i]
                 << "["      << dec << i << "]"
                 << " ^ 0x"  << hex << setfill('0') << setw(8) << boolMask
                 << "  0x"   << hex << setfill('0') << setw(8) << (*datum[i] ^ boolMask)
                 << " != 0x" << hex << setfill('0') << setw(8) << boolXor[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((boolMask ^ *datum[i]) != boolXor[i]) {
         flag = false;
         pretty  << "xor failure (bool ^ SlipDatum)"
                 << " 0x"    << hex << setfill('0') << setw(8) << boolMask
                 << " ^ 0x"  << hex << setfill('0') << setw(8) << (bool)*datum[i]
                 << "["      << dec << i << "] "
                 << " 0x"    << hex << setfill('0') << setw(8) << (boolMask ^ *datum[i])
                 << " != 0x" << hex << setfill('0') << setw(8) << boolXor[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if (xorDatum != longXor[i]) {
         flag = false;
         pretty  << "xor failure (SlipDatum)        "
                 << " 0x"    << hex << setfill('0') << setw(8)<< (ULONG)*datum[i]
                 << "["      << dec << i << "]  "
                 << " 0x"    << hex << setfill('0') << setw(8) << (ULONG)andDatum
                 << " != 0x" << hex << setfill('0') << setw(8) << longXor[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
   }

   for(unsigned int i = 0; i < sizeof(datum)/sizeof(datum[0]); i++) delete datum[i];

   if (!flag) testResultOutput("Test Mask Operators ", 0.0, flag);
   return flag;
}; // bool testMaskOperators()
bool testShiftOperators() {
   stringstream pretty;
   bool flag = true;
   ULONG discrete[] = { 1, 0, 5, 7, 17, 19 };
//#if __SIZEOF_LONG__ == 4
//   //                 true         false       5            7         17          19
//   ULONG shl[] = { 0x00020002, 0x00010001, 0x00200020, 0x00800080, 0x00020000, 0x00080000 };
//   ULONG shr[] = { 0x00008000, 0x00010001, 0x00000800, 0x00000200, 0x00000000, 0x00000000 };
//
//   ULONG chl[] = { 0x00000004, 0x00000000, 0x00000004, 0x00000004, 0x00000004, 0x00000004 };
//   ULONG chr[] = { 0x00000000, 0x00000000, 0x00000001, 0x00000001, 0x00000001, 0x00000001 };
//#elif __SIZEOF_LONG__ == 8
//   //                 true         false       5            7         17           19
//   ULONG shl[] = { 0x00020002, 0x00010001, 0x00200020, 0x00800080, 0x200020000, 0x800080000 };
//   ULONG shr[] = { 0x00008000, 0x00010001, 0x00000800, 0x00000200, 0x000000000, 0x000000000 };
//
//   ULONG chl[] = { 0x00000004, 0x00000000, 0x00000004, 0x00000004, 0x00000004, 0x00000004 };
//   ULONG chr[] = { 0x00000000, 0x00000000, 0x00000001, 0x00000001, 0x00000001, 0x00000001 };
//#endif
   //                 true         false       5            7         17          19
   ULONG shl[] = { 0x00020002, 0x00010001, 0x00200020, 0x00800080, 0x00020000, 0x00080000 };
   ULONG shr[] = { 0x00008000, 0x00010001, 0x00000800, 0x00000200, 0x00000000, 0x00000000 };

   ULONG chl[] = { 0x00000004, 0x00000000, 0x00000004, 0x00000004, 0x00000004, 0x00000004 };
   ULONG chr[] = { 0x00000000, 0x00000000, 0x00000001, 0x00000001, 0x00000001, 0x00000001 };
   SlipDatum* datum[]   = { new SlipDatum((bool) true)
                          , new SlipDatum((bool) false)
                          , new SlipDatum((CHAR)    5)
                          , new SlipDatum((UCHAR)   7)
                          , new SlipDatum((LONG)   17)
                          , new SlipDatum((ULONG)  19)
                          };
   LONG lhsShl[]        = { 0x12345678, 0x12345678, 0x12345678, 0x12345678
                          , 0x12345678, 0x12345678                          };
   LONG lhsShr[]        = { 0x12345678, 0x12345678, 0x12345678, 0x12345678
                          , 0x12345678, 0x12345678                          };
   ULONG baseValue = 0x00010001;

   for(unsigned int i = 0; i < sizeof(datum)/sizeof(datum[0]); i++) {
      SlipDatum cell((ULONG) 0x00010001);
      if ((ULONG)(cell << *datum[i]) != shl[i]) {
         flag = false;
         pretty  << "shl failure for datum[" << i << "] (SlipDatum << SlipDatum) "
                 << hex << setfill('0') << " 0x" << setw(8) << (cell << *datum[i]) << " != "
                 << hex << setfill('0') << " 0x" << setw(8) << shl[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((ULONG)(baseValue << *datum[i]) != shl[i]) {
         flag = false;
         pretty  << "shl failure for datum[" << i << "] (ULONG << SlipDatum) "
                 << hex << setfill('0') << " 0x" << setw(8) << (baseValue << *datum[i]) << " != "
                 << hex << setfill('0') << " 0x" << setw(8) << shl[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((ULONG)(cell << discrete[i]) != shl[i]) {
         flag = false;
         pretty  << "shl failure for datum[" << i << "] (SlipDatum << discrete) "
                 << hex << setfill('0') << " 0x" << setw(8) << (cell << discrete[i]) << " != "
                 << hex << setfill('0') << " 0x" << setw(8) << shl[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      SlipDatum shlCell((cell << *datum[i]));
      if (shlCell != shl[i]) {
         flag = false;
         pretty  << "shl failure for datum[" << i << "] (SlipDatum(SlipDatum << discrete)) "
                 << hex << setfill('0') << " 0x" << setw(8) << (ULONG)shlCell << " != "
                 << hex << setfill('0') << " 0x" << setw(8) << shl[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }

      if ((ULONG)(cell >> *datum[i]) != shr[i]) {
         flag = false;
         pretty  << "shr failure for datum[" << i << "] (SlipDatum >> SlipDatum) "
                 << hex << setfill('0') << " 0x" << setw(8) << (cell >> *datum[i]) << " != "
                 << hex << setfill('0') << " 0x" << setw(8) << shr[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((ULONG)(baseValue >> *datum[i]) != shr[i]) {
         flag = false;
         pretty  << "shr failure for datum[" << i << "] (ULONG << SlipDatum) "
                 << hex << setfill('0') << " 0x" << setw(8) << (baseValue >> *datum[i]) << " != "
                 << hex << setfill('0') << " 0x" << setw(8) << shr[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((ULONG)(cell >> discrete[i]) != shr[i]) {
         flag = false;
         pretty  << "shr failure for datum[" << i << "] (SlipDatum << discrete) "
                 << hex << setfill('0') << " 0x" << setw(8) << (cell >> discrete[i]) << " != "
                 << hex << setfill('0') << " 0x" << setw(8) << shr[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      SlipDatum shrCell((cell >> *datum[i]));
      if (shrCell != shr[i]) {
         flag = false;
         pretty  << "shr failure for datum[" << i << "] (SlipDatum(SlipDatum << discrete)) "
                 << hex << setfill('0') << " 0x" << setw(8) << (ULONG)shrCell << " != "
                 << hex << setfill('0') << " 0x" << setw(8) << shr[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
   }

   SlipDatum cell((ULONG) 0x00010001);
   lhsShl[ 0] = (cell << (bool)true );
   lhsShl[ 1] = (cell << (bool)false);
   lhsShl[ 2] = (cell << (CHAR)    5);
   lhsShl[ 3] = (cell << (UCHAR)   7);
   lhsShl[ 4] = (cell << (LONG)   17);
   lhsShl[ 5] = (cell << (ULONG)  19);
   lhsShr[ 0] = (cell >> (bool) true);
   lhsShr[ 1] = (cell >> (bool)false);
   lhsShr[ 2] = (cell >> (CHAR)    5);
   lhsShr[ 3] = (cell >> (UCHAR)   7);
   lhsShr[ 4] = (cell >> (LONG)   17);
   lhsShr[ 5] = (cell >> (ULONG)  19);

   for(unsigned int i = 0; i < (sizeof(lhsShr)/sizeof(lhsShr[0]) + 1)/2; i++) {
      if ((ULONG)lhsShl[i] != shl[i]) {
         flag = false;
         pretty  << "shl failure for datum[" << i << "] (lhsShl[" << i << "] = cell << value "
                 << hex << setfill('0') << " 0x" << setw(8) << lhsShl[i] << " != "
                 << hex << setfill('0') << " 0x" << setw(8) << shl[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((ULONG)lhsShr[i] != shr[i]) {
         flag = false;
         pretty  << "shr failure for datum[" << i << "] (lhsShr[" << i << "] = cell << value "
                 << hex << setfill('0') << " 0x" << setw(8) << lhsShr[i] << " != "
                 << hex << setfill('0') << " 0x" << setw(8) << shr[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
   }

   cell = (ULONG)2;
   lhsShl[0] = ((bool)true  << cell);
   lhsShl[1] = ((bool)false << cell);
   lhsShl[2] = ((CHAR) 1 << cell);
   lhsShl[3] = ((UCHAR)1 << cell);
   lhsShl[4] = ((LONG) 1 << cell);
   lhsShl[5] = ((ULONG)1 << cell);

   lhsShr[0] = ((bool)true  >> cell);
   lhsShr[1] = ((bool)false >> cell);
   lhsShr[2] = ((CHAR) 4 >> cell);
   lhsShr[3] = ((UCHAR)4 >> cell);
   lhsShr[4] = ((LONG) 4 >> cell);
   lhsShr[5] = ((ULONG)4 >> cell);
   for(unsigned int i = 0; i < (sizeof(lhsShr)/sizeof(lhsShr[0]) + 1)/2; i++) {
      if ((ULONG)lhsShl[i] != chl[i]) {
         flag = false;
         pretty  << "shl failure for datum[" << i << "] (lhsShl[" << i << "] = value << cell "
                 << hex << setfill('0') << " 0x" << setw(8) << lhsShl[i] << " != "
                 << hex << setfill('0') << " 0x" << setw(8) << chl[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
      if ((ULONG)lhsShr[i] != chr[i]) {
         flag = false;
         pretty  << "shr failure for datum[" << i << "] (lhsShr[" << i << "] = value >> cell "
                 << hex << setfill('0') << " 0x" << setw(8) << lhsShr[i] << " != "
                 << hex << setfill('0') << " 0x" << setw(8) << chr[i];
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
   }

   for(unsigned int i = 0; i < sizeof(datum)/sizeof(datum[0]); i++) delete datum[i];

   if (!flag) testResultOutput("Test Shift Operators ", 0.0, flag);
   return flag;
}; // bool testShiftOperators()
bool testSimpleAssignmentOperator() {
   stringstream pretty;
   bool flag = true;
   SlipDatum* datum[]   = { new SlipDatum((bool) true)
                          , new SlipDatum((bool) false)
                          , new SlipDatum((CHAR)    5)
                          , new SlipDatum((UCHAR)   7)
                          , new SlipDatum((LONG)   17)
                          , new SlipDatum((ULONG)  19)
                          , new SlipDatum((DOUBLE)3.1)
                          , new SlipDatum((string)"able")
                          , new SlipDatum(datum)
                          };
   SlipDatum lhs[]      = { SlipDatum((LONG)123456789)
                          , SlipDatum((LONG)123456789)
                          , SlipDatum((LONG)123456789)
                          , SlipDatum((LONG)123456789)
                          , SlipDatum((LONG)123456789)
                          , SlipDatum((LONG)123456789)
                          , SlipDatum((LONG)123456789)
                          , SlipDatum((LONG)123456789)
                          , SlipDatum((LONG)123456789)
                          };
   for(unsigned int i = 0; i < sizeof(datum)/sizeof(datum[0]); i++) {
      SlipDatum lhs((DOUBLE)5.9);
      lhs = *datum[i];
      if (lhs != *datum[i]) {
         flag = false;
         pretty  << "assignment failure for datum[" << i << "] (SlipDatum == SlipDatum) "
                 << lhs.toString() << " != "
                 << datum[i]->toString();
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
   }

   lhs[0] = (bool)true;
   lhs[1] = (bool)false;
   lhs[2] = (CHAR)   5;
   lhs[3] = (UCHAR)  7;
   lhs[4] = (LONG)  17;
   lhs[5] = (ULONG) 19;
   lhs[6] = (DOUBLE)3.1;
   lhs[7] = (string)"able";
   lhs[8] = (void*)datum;
   for(unsigned int i = 0; i < sizeof(datum)/sizeof(datum[0]); i++) {
      if (lhs[i] != *datum[i]) {
         flag = false;
         pretty  << "assignment failure for lhs[" << i << "] (SlipDatum = SlipDatum) "
                 << lhs[i].toString() << " != "
                 << datum[i]->toString() << endl;
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
      }
   }


   for(unsigned int i = 0; i < sizeof(datum)/sizeof(datum[0]); i++) delete datum[i];

   if (!flag) testResultOutput("Test Simple Assignment ", 0.0, flag);
   return flag;

}; // bool testSimpleAssignmentOperator()

bool testOperators() {
   high_resolution_clock::time_point start =  high_resolution_clock::now();
   bool flag = true;
   flag = (testCastOperators())?                  flag: false;
   flag = (testEqualityOperators())?              flag: false;
   flag = (testArithmeticDiscreteOperators())?    flag: false;
   flag = (testArithmeticNonDiscreteOperators())? flag: false;
   flag = (testSimpleAssignmentOperator())?       flag: false;
   flag = (testShiftOperators())?                 flag: false;
   flag = (testMaskOperators())?                  flag: false;
   flag = (testLogicalOperators())?               flag: false;
   flag = (testAssignmentOperators())?            flag: false;
   
   high_resolution_clock::time_point end =  high_resolution_clock::now();
   duration<double> seconds = duration_cast<duration<double>>(end - start);
   testResultOutput("Test Operators", seconds.count(), flag);
   return flag;
}; // bool testOperators()