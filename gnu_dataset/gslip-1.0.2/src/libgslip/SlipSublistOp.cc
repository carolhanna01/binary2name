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
 * @file    SlipSublistOp.cpp
 * @author  A. Schwarz
 * @date    June 04, 2012
 *
 */

 # include <string>
 # include <sstream>
 # include <iomanip>
 # include "SlipDatum.h"
 # include "SlipDef.h"
 # include "SlipErr.h"
 # include "SlipGlobal.h"
 # include "SlipHeader.h"
 # include "SlipHeaderOp.h"
 # include "SlipSublist.h"
 # include "SlipSublistOp.h"

using namespace slip;

namespace slip {

   /*********************************************************
    *                  Casting Operators                    *
   *********************************************************/

   bool SlipSublistOp::cast_bool(const SlipDatum& X) const {                    // (bool) X
      postError(__FILE__, __LINE__, SlipErr::E1035, "SlipSublistOp(bool)", "", "Operation not supported for sublist.", X);
      return false;
   }; // bool SlipSublistOp::cast_bool(const SlipDatum& X) const
   UCHAR SlipSublistOp::cast_UCHAR(const SlipDatum& X) const {                  // (UCHAR) X
      postError(__FILE__, __LINE__, SlipErr::E1036, "SlipSublistOp(UCHAR)", "", "Operation not supported for sublist.", X);
      return 0;
   }; // UCHAR SlipSublistOp::cast_UCHAR(const SlipDatum& X) const
   CHAR SlipSublistOp::cast_CHAR(const SlipDatum& X) const {                    // (CHAR) X
      postError(__FILE__, __LINE__, SlipErr::E1037, "SlipSublistOp(CHAR)", "", "Operation not supported for sublist.", X);
      return 0;
   }; // CHAR SlipSublistOp::cast_CHAR(const SlipDatum& X) const
   ULONG SlipSublistOp::cast_ULONG(const SlipDatum& X) const {                  // (ULONG) X
      postError(__FILE__, __LINE__, SlipErr::E1038, "SlipSublistOp(ULONG)", "", "Operation not supported for sublist.", X);
      return 0;
   }; // ULONG SlipSublistOp::cast_ULONG(const SlipDatum& X) const
   LONG SlipSublistOp::cast_LONG(const SlipDatum& X) const {                    // (LONG) X
      postError(__FILE__, __LINE__, SlipErr::E1039, "SlipSublistOp(LONG)", "", "Operation not supported for sublist.", X);
      return 0;
   }; // LONG SlipSublistOp::cast_LONG(const SlipDatum& X) const
   DOUBLE SlipSublistOp::cast_DOUBLE(const SlipDatum& X) const {                // (DOUBLE) X
      postError(__FILE__, __LINE__, SlipErr::E1040, "SlipSublistOp(DOUBLE)", "", "Operation not supported for sublist.", X);
      return 0.0;
   }; // DOUBLE SlipSublistOp::cast_DOUBLE(const SlipDatum& X) const
   PTR SlipSublistOp::cast_PTR(const SlipDatum& Y) const {                      // (PTR) Y
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipBoolOp++", "", "Operation not supported for SUBLIST.", Y);
      return NULL;
   }; // PTR SlipSublistOp::cast_PTR(const SlipDatum& Y) const

      /*********************************************************
       *                 Operator Overloading                  *
      *********************************************************/

          /*************************************************
           *                Unary operators                *
          *************************************************/
   ULONG SlipSublistOp::bitNOT (const SlipDatum& X) const { // operator~
      postError(__FILE__, __LINE__, SlipErr::E1043, "SlipSublistOp~", "", "Operation not supported for sublist.", X);
      return 0;
   }; // ULONG SlipSublistOp::bitNOT (const SlipDatum& X) const
   bool SlipSublistOp::logNOT (const SlipDatum& X) const { // operator!
      postError(__FILE__, __LINE__, SlipErr::E1025, "SlipSublistOp!", "", "Operation not supported for sublist.", X);
      return false;
   }; // bool SlipSublistOp::logNOT (const SlipDatum& X) const
   SlipDatum SlipSublistOp::plus (const SlipDatum& X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1001, "SlipSublistOp+", "", "Operation not supported for sublist.", X);
      return X;
   }; // SlipDatum SlipSublistOp::plus (const SlipDatum& X) const
   SlipDatum SlipSublistOp::minus (const SlipDatum& X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1002, "SlipSublistOp-", "", "Operation not supported for sublist.", X);
      return X;
   }; // SlipDatum SlipSublistOp::minus (const SlipDatum& X) const
   SlipDatum& SlipSublistOp::postInc (SlipDatum& X) const { // operator++
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipSublistOp++", "", "Operation not supported for sublist.", X);
      return X;
   }; // SlipDatum& SlipSublistOp::postInc (SlipDatum& X) const
   SlipDatum& SlipSublistOp::postDec (SlipDatum& X) const { // operator--
      postError(__FILE__, __LINE__, SlipErr::E1004, "SlipSublistOp--", "", "Operation not supported for sublist.", X);
      return X;
   }; // SlipDatum& SlipSublistOp::postDec (SlipDatum& X) const
   SlipDatum& SlipSublistOp::preInc (SlipDatum& X) const { // ++operator
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipSublistOp++(int)", "", "Operation not supported for sublist.", X);
      return X;
   }; // SlipDatum& SlipSublistOp::preInc (SlipDatum& X) const
   SlipDatum& SlipSublistOp::preDec (SlipDatum& X) const { // --operator
      postError(__FILE__, __LINE__, SlipErr::E1004, "SlipSublistOp--(int)", "", "Operation not supported for sublist.", X);
      return X;
   }; // SlipDatum& SlipSublistOp::preDec (SlipDatum& X) const

          /*************************************************
           *                Binary operators               *
          *************************************************/
   SlipDatum SlipSublistOp::add(const SlipDatum& Y, const SlipDatum& X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1005, "SlipSublistOp+", "", "Operation not supported for sublist.", X);
      return Y;
   }; // SlipDatum SlipSublistOp::add(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipSublistOp::sub(const SlipDatum& Y, const SlipDatum& X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1006, "SlipSublistOp-", "", "Operation not supported for sublist.", X);
      return Y;
   }; // SlipDatum SlipSublistOp::sub(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipSublistOp::mul(const SlipDatum& Y, const SlipDatum& X) const { // operator*
      postError(__FILE__, __LINE__, SlipErr::E1007, "SlipSublistOp*", "", "Operation not supported for sublist.", X);
      return Y;
   }; // SlipDatum SlipSublistOp::mul(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipSublistOp::div(const SlipDatum& Y, const SlipDatum& X) const { // operator/
      postError(__FILE__, __LINE__, SlipErr::E1008, "SlipSublistOp/", "", "Operation not supported for sublist.", X);
      return Y;
   }; // SlipDatum SlipSublistOp::div(const SlipDatum& Y, const SlipDatum& X) const
   LONG       SlipSublistOp::mod(const SlipDatum& Y, const SlipDatum& X) const { // operator%
      postError(__FILE__, __LINE__, SlipErr::E1026, "SlipSublistOp%", "", "Operation not supported for sublist.", Y, X);
      return 0;
   }; // SlipDatum SlipSublistOp::mod(const SlipDatum& Y, const SlipDatum& X) const

   SlipDatum SlipSublistOp::add(const SlipDatum& Y, const DOUBLE X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1005, "SlipSublistOp+", "", "Operation not supported for sublist.", Y);
      return Y;
   }; // SlipDatum SlipSublistOp::add(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipSublistOp::sub(const SlipDatum& Y, const DOUBLE X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1006, "SlipSublistOp-", "", "");
      return Y;
   }; // SlipDatum SlipSublistOp::sub(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipSublistOp::mul(const SlipDatum& Y, const DOUBLE X) const { // operator*
      postError(__FILE__, __LINE__, SlipErr::E1007, "SlipSublistOp*", "", "");
      return Y;
   }; // SlipDatum SlipSublistOp::mul(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipSublistOp::div(const SlipDatum& Y, const DOUBLE X) const { // operator/
      postError(__FILE__, __LINE__, SlipErr::E1008, "SlipSublistOp/", "", "");
      return Y;
   }; // SlipDatum SlipSublistOp::div(const SlipDatum& Y, const DOUBLE X) const

   SlipDatum SlipSublistOp::add(const SlipDatum& Y, const ULONG X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1005, "SlipSublistOp+", "", "Operation not supported for sublist.", Y);
      return Y;
   }; // SlipDatum SlipSublistOp::add(const SlipDatum& Y, const ULONG X) const

   SlipDatum SlipSublistOp::sub(const SlipDatum& Y, const ULONG X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1006, "SlipSublistOp-", "", "");
      return Y;
   }; // SlipDatum SlipSublistOp::sub(const SlipDatum& Y, const ULONG X) const
   SlipDatum SlipSublistOp::mul(const SlipDatum& Y, const ULONG X) const { // operator*
      postError(__FILE__, __LINE__, SlipErr::E1007, "SlipSublistOp*", "", "");
      return Y;
   }; // SlipDatum SlipSublistOp::mul(const SlipDatum& Y, const ULONG X) const
   SlipDatum SlipSublistOp::div(const SlipDatum& Y, const ULONG X) const { // operator/
      postError(__FILE__, __LINE__, SlipErr::E1008, "SlipSublistOp/", "", "");
      return Y;
   }; // SlipDatum SlipSublistOp::div(const SlipDatum& Y, const ULONG X) const
   LONG       SlipSublistOp::mod(const SlipDatum& Y, const ULONG X) const { // operator%
      postError(__FILE__, __LINE__, SlipErr::E1026, "SlipSublistOp%", "", "Operation not supported for sublist.", Y);
      return 0;
   }; // SlipDatum SlipSublistOp::mod(const SlipDatum& Y, const ULONG X) const

   SlipDatum SlipSublistOp::add(const SlipDatum& Y, const LONG X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1005, "SlipSublistOp+", "", "Operation not supported for sublist.", Y);
      return Y;
   }; // SlipDatum SlipSublistOp::add(const SlipDatum& Y, const LONG X) const

   SlipDatum SlipSublistOp::sub(const SlipDatum& Y, const LONG X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1006, "SlipSublistOp-", "", "");
      return Y;
   }; // SlipDatum SlipSublistOp::sub(const SlipDatum& Y, const LONG X) const
   SlipDatum SlipSublistOp::mul(const SlipDatum& Y, const LONG X) const { // operator*
      postError(__FILE__, __LINE__, SlipErr::E1007, "SlipSublistOp*", "", "");
      return Y;
   }; // SlipDatum SlipSublistOp::mul(const SlipDatum& Y, const LONG X) const
   SlipDatum SlipSublistOp::div(const SlipDatum& Y, const LONG X) const { // operator/
      postError(__FILE__, __LINE__, SlipErr::E1008, "SlipSublistOp/", "", "");
      return Y;
   }; // SlipDatum SlipSublistOp::div(const SlipDatum& Y, const LONG X) const
   LONG       SlipSublistOp::mod(const SlipDatum& Y, const LONG X) const { // operator%
      postError(__FILE__, __LINE__, SlipErr::E1026, "SlipSublistOp%", "", "Operation not supported for sublist.", Y);
      return 0;
   }; // SlipDatum SlipSublistOp::mod(const SlipDatum& Y, const LONG X) const

          /*************************************************
           *                Bit operators                  *
          *************************************************/
   LONG  SlipSublistOp::shl (const SlipDatum& Y, const SlipDatum& X) const { // operator<<
      postError(__FILE__, __LINE__, SlipErr::E1020, "SlipSublistOp<<", "", "Operation not supported for sublist.", Y, X);
      return 0;
   }; // LONG  SlipSublistOp::shl (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipSublistOp::shr (const SlipDatum& Y, const SlipDatum& X) const { // operator>>
      postError(__FILE__, __LINE__, SlipErr::E1021, "SlipSublistOp>>", "", "Operation not supported for sublist.", Y, X);
      return 0;
   }; // LONG  SlipSublistOp::shr (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipSublistOp::band(const SlipDatum& Y, const SlipDatum& X) const { // operator&
      postError(__FILE__, __LINE__, SlipErr::E1024, "SlipSublistOp&", "", "Operation not supported for sublist.", Y, X);
      return 0;
   }; // LONG  SlipSublistOp::band(const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipSublistOp::bor (const SlipDatum& Y, const SlipDatum& X) const { // operator|
      postError(__FILE__, __LINE__, SlipErr::E1023, "SlipSublistOp|", "", "Operation not supported for sublist.", Y, X);
      return 0;
   }; // LONG  SlipSublistOp::bor (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipSublistOp::bxor(const SlipDatum& Y, const SlipDatum& X) const { // operator^
      postError(__FILE__, __LINE__, SlipErr::E1022, "SlipSublistOp^", "", "Operation not supported for sublist.", Y, X);
      return 0;
   }; // LONG  SlipSublistOp::bxor(const SlipDatum& Y, const SlipDatum& X) const

   LONG  SlipSublistOp::shl (const SlipDatum& Y, const ULONG X) const { // operator<<
      postError(__FILE__, __LINE__, SlipErr::E1020, "SlipSublistOp<<", "", "Operation not supported for sublist.", Y);
      return 0;
   }; // LONG  SlipSublistOp::shl (const SlipDatum& Y, const ULONG X) const
   LONG  SlipSublistOp::shr (const SlipDatum& Y, const ULONG X) const { // operator>>
      postError(__FILE__, __LINE__, SlipErr::E1021, "SlipSublistOp>>", "", "Operation not supported for sublist.", Y);
      return 0;
   }; // LONG  SlipSublistOp::shr (const SlipDatum& Y, const ULONG X) const
   LONG  SlipSublistOp::band(const SlipDatum& Y, const ULONG X) const { // operator&
      postError(__FILE__, __LINE__, SlipErr::E1024, "SlipSublistOp&", "", "Operation not supported for sublist.", Y);
      return 0;
   }; // LONG  SlipSublistOp::band(const SlipDatum& Y, const ULONG X) const
   LONG  SlipSublistOp::bor (const SlipDatum& Y, const ULONG X) const { // operator|
      postError(__FILE__, __LINE__, SlipErr::E1023, "SlipSublistOp|", "", "Operation not supported for sublist.", Y);
      return 0;
   }; // LONG  SlipSublistOp::bor (const SlipDatum& Y, const ULONG X) const
   LONG  SlipSublistOp::bxor(const SlipDatum& Y, const ULONG X) const { // operator^
      postError(__FILE__, __LINE__, SlipErr::E1022, "SlipSublistOp^", "", "Operation not supported for sublist.", Y);
      return 0;
   }; // LONG  SlipSublistOp::bxor(const SlipDatum& Y, const ULONG X) const

   LONG  SlipSublistOp::shl (const SlipDatum& Y, const LONG X) const { // operator<<
      postError(__FILE__, __LINE__, SlipErr::E1020, "SlipSublistOp<<", "", "Operation not supported for sublist.", Y);
      return 0;
   }; // LONG  SlipSublistOp::shl (const SlipDatum& Y, const LONG X) const
   LONG  SlipSublistOp::shr (const SlipDatum& Y, const LONG X) const { // operator>>
      postError(__FILE__, __LINE__, SlipErr::E1021, "SlipSublistOp>>", "", "Operation not supported for sublist.", Y);
      return 0;
   }; // LONG  SlipSublistOp::shr (const SlipDatum& Y, const LONG X) const
   LONG  SlipSublistOp::band(const SlipDatum& Y, const LONG X) const { // operator&
      postError(__FILE__, __LINE__, SlipErr::E1024, "SlipSublistOp&", "", "Operation not supported for sublist.", Y);
      return 0;
   }; // LONG  SlipSublistOp::band(const SlipDatum& Y, const LONG X) const
   LONG  SlipSublistOp::bor (const SlipDatum& Y, const LONG X) const { // operator|
      postError(__FILE__, __LINE__, SlipErr::E1023, "SlipSublistOp|", "", "Operation not supported for sublist.", Y);
      return 0;
   }; // LONG  SlipSublistOp::bor (const SlipDatum& Y, const LONG X) const
   LONG  SlipSublistOp::bxor(const SlipDatum& Y, const LONG X) const { // operator^
      postError(__FILE__, __LINE__, SlipErr::E1022, "SlipSublistOp^", "", "Operation not supported for sublist.", Y);
      return 0;
   }; // LONG  SlipSublistOp::bxor(const SlipDatum& Y, const LONG X) const

          /*************************************************
           *               Simple Assignment               *
          *************************************************/
   SlipDatum& SlipSublistOp::asgn(SlipDatum& Y, const SlipDatum& X) const {        // operator+=
      SlipHeader* cell = (SlipHeader*)&X;
      if (X.getClassType()== eSUBLIST)
         cell = (SlipHeader*)*getSublistHeader(X);
      else
         cell = NULL;
      *getSublistHeader(Y) = (cell == NULL)? *getSublistHeader(Y): cell;
      if (cell == NULL)
         postError(__FILE__, __LINE__, SlipErr::E1009, "SlipSublistOp+=", "", "Operation not supported for sublist.", Y, X);
      return Y;
   }; // SlipDatum& SlipSublistOp::asgn(SlipDatum& Y, const SlipDatum& X) const

          /*************************************************
           *       Arithmetic Assignment operators         *
          *************************************************/
   SlipDatum& SlipSublistOp::addAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1009, "SlipSublistOp+=", "", "Operation not supported for sublist.", Y, X);
      return Y;
   }; // SlipDatum& SlipSublistOp::addAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipSublistOp::subAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator-=
      postError(__FILE__, __LINE__, SlipErr::E1010, "SlipSublistOp-=", "", "Operation not supported for sublist.", Y, X);
      return Y;
   }; // SlipDatum& SlipSublistOp::subAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipSublistOp::mulAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator*=
      postError(__FILE__, __LINE__, SlipErr::E1011, "SlipSublistOp*=", "", "Operation not supported for sublist.", Y, X);
      return Y;
   }; // SlipDatum& SlipSublistOp::mulAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipSublistOp::divAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator/=
      postError(__FILE__, __LINE__, SlipErr::E1012, "SlipSublistOp/=", "", "Operation not supported for sublist.", Y, X);
      return Y;
   }; // SlipDatum& SlipSublistOp::divAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipSublistOp::modAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator%=
      postError(__FILE__, __LINE__, SlipErr::E1013, "SlipSublistOp%=", "", "Operation not supported for sublist.", Y, X);
      return Y;
   }; // SlipDatum& SlipSublistOp::modAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipSublistOp::shlAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator<<=
      postError(__FILE__, __LINE__, SlipErr::E1014, "SlipSublistOp<<=", "", "Operation not supported for sublist.", Y, X);
      return Y;
   }; // SlipDatum& SlipSublistOp::shlAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipSublistOp::shrAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator>>=
      postError(__FILE__, __LINE__, SlipErr::E1015, "SlipSublistOp>>=", "", "Operation not supported for sublist.", Y, X);
      return Y;
   }; // SlipDatum& SlipSublistOp::shrAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipSublistOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator&=
      postError(__FILE__, __LINE__, SlipErr::E1016, "SlipSublistOp&=", "", "Operation not supported for sublist.", Y, X);
      return Y;
   }; // SlipDatum& SlipSublistOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipSublistOp::borAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator|=
      postError(__FILE__, __LINE__, SlipErr::E1019, "SlipSublistOp|=", "", "Operation not supported for sublist.", Y, X);
      return Y;
   }; // SlipDatum& SlipSublistOp::borAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipSublistOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator^=
      postError(__FILE__, __LINE__, SlipErr::E1017, "SlipSublistOp^=", "", "Operation not supported for sublist.", Y, X);
      return Y;
   }; // SlipDatum& SlipSublistOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const

   SlipDatum& SlipSublistOp::addAsgn (SlipDatum& Y, const DOUBLE X) const { // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1009, "SlipSublistOp", "", "");
      return Y;
   }; // SlipDatum& SlipSublistOp::addAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipSublistOp::subAsgn (SlipDatum& Y, const DOUBLE X) const { // operator-=
      postError(__FILE__, __LINE__, SlipErr::E1010, "SlipSublistOp", "", "");
      return Y;
   }; // SlipDatum& SlipSublistOp::subAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipSublistOp::mulAsgn (SlipDatum& Y, const DOUBLE X) const { // operator*=
      postError(__FILE__, __LINE__, SlipErr::E1011, "SlipSublistOp", "", "");
      return Y;
   }; // SlipDatum& SlipSublistOp::mulAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipSublistOp::divAsgn (SlipDatum& Y, const DOUBLE X) const { // operator/=
      postError(__FILE__, __LINE__, SlipErr::E1012, "SlipSublistOp", "", "");
      return Y;
   }; // SlipDatum& SlipSublistOp::divAsgn (SlipDatum& Y, const DOUBLE X) const

   SlipDatum& SlipSublistOp::addAsgn (SlipDatum& Y, const ULONG X) const { // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1009, "SlipSublistOp+=", "", "Operation not supported for sublist.", Y);
      return Y;
   }; // SlipDatum& SlipSublistOp::addAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipSublistOp::subAsgn (SlipDatum& Y, const ULONG X) const { // operator-=
      postError(__FILE__, __LINE__, SlipErr::E1010, "SlipSublistO-=p", "", "Operation not supported for sublist.", Y);
      return Y;
   }; // SlipDatum& SlipSublistOp::subAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipSublistOp::mulAsgn (SlipDatum& Y, const ULONG X) const { // operator*=
      postError(__FILE__, __LINE__, SlipErr::E1011, "SlipSublistOp*=", "", "Operation not supported for sublist.", Y);
      return Y;
   }; // SlipDatum& SlipSublistOp::mulAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipSublistOp::divAsgn (SlipDatum& Y, const ULONG X) const { // operator/=
      postError(__FILE__, __LINE__, SlipErr::E1012, "SlipSublistOp/=", "", "Operation not supported for sublist.", Y);
      return Y;
   }; // SlipDatum& SlipSublistOp::divAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipSublistOp::modAsgn (SlipDatum& Y, const ULONG X) const { // operator%=
      postError(__FILE__, __LINE__, SlipErr::E1013, "SlipSublistOp%=", "", "Operation not supported for sublist.", Y);
      return Y;
   }; // SlipDatum& SlipSublistOp::modAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipSublistOp::shlAsgn (SlipDatum& Y, const ULONG X) const { // operator<<=
      postError(__FILE__, __LINE__, SlipErr::E1014, "SlipSublistOp<<=", "", "Operation not supported for sublist.", Y);
      return Y;
   }; // SlipDatum& SlipSublistOp::shlAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipSublistOp::shrAsgn (SlipDatum& Y, const ULONG X) const { // operator>>=
      postError(__FILE__, __LINE__, SlipErr::E1015, "SlipSublistOp>>=", "", "Operation not supported for sublist.", Y);
      return Y;
   }; // SlipDatum& SlipSublistOp::shrAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipSublistOp::bandAsgn(SlipDatum& Y, const ULONG X) const { // operator&=
      postError(__FILE__, __LINE__, SlipErr::E1016, "SlipSublistOp&=", "", "Operation not supported for sublist.", Y);
      return Y;
   }; // SlipDatum& SlipSublistOp::bandAsgn(SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipSublistOp::borAsgn (SlipDatum& Y, const ULONG X) const { // operator|=
      postError(__FILE__, __LINE__, SlipErr::E1019, "SlipSublistOp|=", "", "Operation not supported for sublist.", Y);
      return Y;
   }; // SlipDatum& SlipSublistOp::borAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipSublistOp::bxorAsgn(SlipDatum& Y, const ULONG X) const { // operator^=
      postError(__FILE__, __LINE__, SlipErr::E1017, "SlipSublistOp^=", "", "Operation not supported for sublist.", Y);
      return Y;
   }; // SlipDatum& SlipSublistOp::bxorAsgn(SlipDatum& Y, const ULONG X) const

   SlipDatum& SlipSublistOp::addAsgn (SlipDatum& Y, const LONG X) const { // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1009, "SlipSublistOp+=", "", "Operation not supported for sublist.", Y);
      return Y;
   }; // SlipDatum& SlipSublistOp::addAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipSublistOp::subAsgn (SlipDatum& Y, const LONG X) const { // operator-=
      postError(__FILE__, __LINE__, SlipErr::E1010, "SlipSublistOp-=", "", "Operation not supported for sublist.", Y);
      return Y;
   }; // SlipDatum& SlipSublistOp::subAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipSublistOp::mulAsgn (SlipDatum& Y, const LONG X) const { // operator*=
      postError(__FILE__, __LINE__, SlipErr::E1011, "SlipSublistOp*=", "", "Operation not supported for sublist.", Y);
      return Y;
   }; // SlipDatum& SlipSublistOp::mulAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipSublistOp::divAsgn (SlipDatum& Y, const LONG X) const { // operator/=
      postError(__FILE__, __LINE__, SlipErr::E1012, "SlipSublistOp/=", "", "Operation not supported for sublist.", Y);
      return Y;
   }; // SlipDatum& SlipSublistOp::divAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipSublistOp::modAsgn (SlipDatum& Y, const LONG X) const { // operator%=
      postError(__FILE__, __LINE__, SlipErr::E1013, "SlipSublistOp%=", "", "Operation not supported for sublist.", Y);
      return Y;
   }; // SlipDatum& SlipSublistOp::modAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipSublistOp::shlAsgn (SlipDatum& Y, const LONG X) const { // operator<<=
      postError(__FILE__, __LINE__, SlipErr::E1014, "SlipSublistOp<<=", "", "Operation not supported for sublist.", Y);
      return Y;
   }; // SlipDatum& SlipSublistOp::shlAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipSublistOp::shrAsgn (SlipDatum& Y, const LONG X) const { // operator>>=
      postError(__FILE__, __LINE__, SlipErr::E1015, "SlipSublistOp>>=", "", "Operation not supported for sublist.", Y);
      return Y;
   }; // SlipDatum& SlipSublistOp::shrAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipSublistOp::bandAsgn(SlipDatum& Y, const LONG X) const { // operator&=
      postError(__FILE__, __LINE__, SlipErr::E1016, "SlipSublistOp&=", "", "Operation not supported for sublist.", Y);
      return Y;
   }; // SlipDatum& SlipSublistOp::bandAsgn(SlipDatum& Y, const LONG X) const
   SlipDatum& SlipSublistOp::borAsgn (SlipDatum& Y, const LONG X) const { // operator|=
      postError(__FILE__, __LINE__, SlipErr::E1019, "SlipSublistOp|=", "", "Operation not supported for sublist.", Y);
      return Y;
   }; // SlipDatum& SlipSublistOp::borAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipSublistOp::bxorAsgn(SlipDatum& Y, const LONG X) const { // operator^=
      postError(__FILE__, __LINE__, SlipErr::E1017, "SlipSublistOp^=", "", "Operation not supported for sublist.", Y);
      return Y;
   }; // SlipDatum& SlipSublistOp::bxorAsgn(SlipDatum& Y, const LONG X) const

          /*************************************************
           *               Logical operators               *
          *************************************************/
   bool SlipSublistOp::notEqual (const SlipDatum& Y, const SlipDatum& X) const { // operator!=
       SlipHeader* head1 = *(SlipHeader**)getSublistHeader(Y);
       SlipHeader* head2 = NULL;
       if (X.isSublist()) {
           head2 = *(SlipHeader**)getSublistHeader(X);
       } else if (X.isHeader()) {
           head2 = &(SlipHeader&)X;
       } else {
         postError(__FILE__, __LINE__, SlipErr::E1028, "SlipSublistOp!=", "", "Operation not supported for sublist.", Y, X);
       }
       return (head1 != head2);
   }; // bool SlipSublistOp::notEqual (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipSublistOp::lessThan (const SlipDatum& Y, const SlipDatum& X) const { // operator<
      return false;
   }; // bool SlipSublistOp::lessThan (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipSublistOp::lessEqual (const SlipDatum& Y, const SlipDatum& X) const { // operator<=
      return false;
   }; // bool SlipSublistOp::lessEqual (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipSublistOp::equal (const SlipDatum& Y, const SlipDatum& X) const { // operator==
       SlipHeader* head1 = *(SlipHeader**)getSublistHeader(Y);
       SlipHeader* head2 = NULL;
       if (X.isSublist()) {
           head2 = *(SlipHeader**)getSublistHeader(X);
       } else if (X.isHeader()) {
           head2 = &(SlipHeader&)X;
       } else {
         postError(__FILE__, __LINE__, SlipErr::E1031, "SlipSublistOp==", "", "Operation not supported for sublist.", Y, X);
         return false;
       }
       return (head1 == head2);
   }; // bool SlipSublistOp::equal (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipSublistOp::greatEqual(const SlipDatum& Y, const SlipDatum& X) const { // operator>=
      return false;
   }; // bool SlipSublistOp::greatEqual(const SlipDatum& Y, const SlipDatum& X) const
   bool SlipSublistOp::greatThan (const SlipDatum& Y, const SlipDatum& X) const { // operator>
      return false;
   }; // bool SlipSublistOp::greatThan (const SlipDatum& Y, const SlipDatum& X) const

   bool SlipSublistOp::notEqual (const SlipDatum& Y, const DOUBLE X) const { // operator!=
      return false;
   }; // bool SlipSublistOp::notEqual (const SlipDatum& Y, const DOUBLE X) const
   bool SlipSublistOp::lessThan (const SlipDatum& Y, const DOUBLE X) const { // operator<
      return false;
   }; // bool SlipSublistOp::lessThan (const SlipDatum& Y, const DOUBLE X) const
   bool SlipSublistOp::lessEqual (const SlipDatum& Y, const DOUBLE X) const { // operator<=
      return false;
   }; // bool SlipSublistOp::lessEqual (const SlipDatum& Y, const DOUBLE X) const
   bool SlipSublistOp::equal (const SlipDatum& Y, const DOUBLE X) const { // operator==
      return false;
   }; // bool SlipSublistOp::equal (const SlipDatum& Y, const DOUBLE X) const
   bool SlipSublistOp::greatEqual(const SlipDatum& Y, const DOUBLE X) const { // operator>=
      return false;
   }; // bool SlipSublistOp::greatEqual(const SlipDatum& Y, const DOUBLE X) const
   bool SlipSublistOp::greatThan (const SlipDatum& Y, const DOUBLE X) const { // operator>
      return false;
   }; // bool SlipSublistOp::greatThan (const SlipDatum& Y, const DOUBLE X) const

   bool SlipSublistOp::notEqual (const SlipDatum& Y, const ULONG X) const { // operator!=
      return false;
   }; // bool SlipSublistOp::notEqual (const SlipDatum& Y, const ULONG X) const
   bool SlipSublistOp::lessThan (const SlipDatum& Y, const ULONG X) const { // operator<
      return false;
   }; // bool SlipSublistOp::lessThan (const SlipDatum& Y, const ULONG X) const
   bool SlipSublistOp::lessEqual (const SlipDatum& Y, const ULONG X) const { // operator<=
      return false;
   }; // bool SlipSublistOp::lessEqual (const SlipDatum& Y, const ULONG X) const
   bool SlipSublistOp::equal (const SlipDatum& Y, const ULONG X) const { // operator==
      return false;
   }; // bool SlipSublistOp::equal (const SlipDatum& Y, const ULONG X) const
   bool SlipSublistOp::greatEqual(const SlipDatum& Y, const ULONG X) const { // operator>=
      return false;
   }; // bool SlipSublistOp::greatEqual(const SlipDatum& Y, const ULONG X) const
   bool SlipSublistOp::greatThan (const SlipDatum& Y, const ULONG X) const { // operator>
      return false;
   }; // bool SlipSublistOp::greatThan (const SlipDatum& Y, const ULONG X) const

   bool SlipSublistOp::notEqual (const SlipDatum& Y, const LONG X) const { // operator!=
      return false;
   }; // bool SlipSublistOp::notEqual (const SlipDatum& Y, const LONG X) const
   bool SlipSublistOp::lessThan (const SlipDatum& Y, const LONG X) const { // operator<
      return false;
   }; // bool SlipSublistOp::lessThan (const SlipDatum& Y, const LONG X) const
   bool SlipSublistOp::lessEqual (const SlipDatum& Y, const LONG X) const { // operator<=
      return false;
   }; // bool SlipSublistOp::lessEqual (const SlipDatum& Y, const LONG X) const
   bool SlipSublistOp::equal (const SlipDatum& Y, const LONG X) const { // operator==
      return false;
   }; // bool SlipSublistOp::equal (const SlipDatum& Y, const LONG X) const
   bool SlipSublistOp::greatEqual(const SlipDatum& Y, const LONG X) const { // operator>=
      return false;
   }; // bool SlipSublistOp::greatEqual(const SlipDatum& Y, const LONG X) const
   bool SlipSublistOp::greatThan (const SlipDatum& Y, const LONG X) const { // operator>
      return false;
   }; // bool SlipSublistOp::greatThan (const SlipDatum& Y, const LONG X) const

   bool SlipSublistOp::notEqual (const SlipDatum& Y, const PTR X) const { // operator!=
      return false;
   }; // bool SlipSublistOp::notEqual (const SlipDatum& Y, const PTR X) const
   bool SlipSublistOp::equal (const SlipDatum& Y, const PTR X) const { // operator==
      return false;
   }; // bool SlipSublistOp::equal (const SlipDatum& Y, const PTR X) const

   bool SlipSublistOp::notEqual (const SlipDatum& Y, const string& X) const { // operator!=
       return false;
   }; // bool SlipSublistOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipSublistOp::lessThan (const SlipDatum& Y, const string& X) const { // operator<
       return false;
   }; // bool SlipSublistOp::equal (const SlipDatum& Y, const string& X) const
   bool SlipSublistOp::lessEqual (const SlipDatum& Y, const string& X) const { // operator<=
       return false;
   }; // bool SlipSublistOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipSublistOp::equal (const SlipDatum& Y, const string& X) const { // operator==
       return false;
   }; // bool SlipSublistOp::equal (const SlipDatum& Y, const string& X) const
   bool SlipSublistOp::greatEqual (const SlipDatum& Y, const string& X) const { // operator>=
       return false;
   }; // bool SlipSublistOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipSublistOp::greatThan (const SlipDatum& Y, const string& X) const { // operator>
       return false;
   }; // bool SlipSublistOp::equal (const SlipDatum& Y, const string& X) const

   string SlipSublistOp::toString(const SlipCell& Y) const  { // pretty print SLIP cell
      return dump(Y);
   }; // string SlipSublistOp::toString(const SlipCell& Y) const

   string SlipSublistOp::dump(const SlipCell& Y) const {
      stringstream pretty;
      sCell* cell = (sCell*)&Y;
      SlipHeader* header = (SlipHeader*)cell->datum.Sublist.Header;
      pretty << "[sublist  ] " << dumpLink(Y) << "\n    -> ";
      if ((SlipHeader*)cell->datum.Sublist.Header == 0) pretty << "NULL";
      else {
         pretty << ((SlipHeaderOp*)headOP)->toString(*header);
      }
      return pretty.str();
   }; // string SlipSublistOp::dump(const SlipCell& Y) const

   string SlipSublistOp::write(const SlipCell& X) const {
      return "";
   }; // string SlipSublistOp::write(const SlipCell& X) const

}; // namespace slip