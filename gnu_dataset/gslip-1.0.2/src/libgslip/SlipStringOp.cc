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
 * @file    SlipStringOp.cpp
 * @author  A. Schwarz
 * @date    June 04, 2012
 *
 */

 # include <iomanip>
 # include <string>
 # include <sstream>
 # include "SlipDatum.h"
 # include "SlipDef.h"
 # include "SlipErr.h"
 # include "SlipGlobal.h"
 # include "SlipPointer.h"
 # include "SlipStringOp.h"

using namespace slip;

namespace slip {

   /*********************************************************
    *                  Casting Operators                    *
   *********************************************************/

   bool SlipStringOp::cast_bool(const SlipDatum& X) const {                     // (bool) X
      postError(__FILE__, __LINE__, SlipErr::E1035, "SlipStringOp::(bool)", "", "Operation not supported for string.", X);
      return false;
   }; // bool SlipStringOp::cast_bool(const SlipDatum& X) const
   UCHAR SlipStringOp::cast_UCHAR(const SlipDatum& X) const {                   // (UCHAR) X
      postError(__FILE__, __LINE__, SlipErr::E1036, "SlipStringOp::(UCHAR)", "", "Operation not supported for string.", X);
      return 0;
   }; // UCHAR SlipStringOp::cast_UCHAR(const SlipDatum& X) const
   CHAR SlipStringOp::cast_CHAR(const SlipDatum& X) const {                     // (CHAR) X
      postError(__FILE__, __LINE__, SlipErr::E1037, "SlipStringOp(CHAR)", "", "Operation not supported for string.", X);
      return 0;
   }; // CHAR SlipStringOp::cast_CHAR(const SlipDatum& X) const
   ULONG SlipStringOp::cast_ULONG(const SlipDatum& X) const {                   // (ULONG) X
      postError(__FILE__, __LINE__, SlipErr::E1038, "SlipStringOp(ULONG)", "", "Operation not supported for string.", X);
      return 0;
   }; // ULONG SlipStringOp::cast_ULONG(const SlipDatum& X) const
   LONG SlipStringOp::cast_LONG(const SlipDatum& X) const {                     // (LONG) X
      postError(__FILE__, __LINE__, SlipErr::E1039, "SlipStringOp(LONG)", "", "Operation not supported for string.", X);
      return 0;
   }; // LONG SlipStringOp::cast_LONG(const SlipDatum& X) const
   DOUBLE SlipStringOp::cast_DOUBLE(const SlipDatum& X) const {                 // (DOUBLE) X
      postError(__FILE__, __LINE__, SlipErr::E1040, "SlipStringOp(DOUBLE)", "", "Operation not supported for string.", X);
      return 0.0;
   }; // DOUBLE SlipStringOp::cast_DOUBLE(const SlipDatum& X) const
   PTR SlipStringOp::cast_PTR(const SlipDatum& Y) const {                       // (PTR) Y
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipBoolOp++", "", "Operation not supported for string.", Y);
      return NULL;
   }; // PTR SlipStringOp::cast_PTR(const SlipDatum& Y) const

      /*********************************************************
       *                 Operator Overloading                  *
      *********************************************************/

          /*************************************************
           *                Unary operators                *
          *************************************************/
   ULONG SlipStringOp::bitNOT (const SlipDatum& X) const { // operator~
      postError(__FILE__, __LINE__, SlipErr::E1043, "SlipStringOp~", "", "Operation not supported for string.", X);
      return 0;
   }; // ULONG SlipStringOp::bitNOT (const SlipDatum& X) const
   bool SlipStringOp::logNOT (const SlipDatum& X) const { // operator!
      postError(__FILE__, __LINE__, SlipErr::E1025, "SlipStringOp!", "", "Operation not supported for string.", X);
      return false;
   }; // bool SlipStringOp::logNOT (const SlipDatum& X) const
   SlipDatum SlipStringOp::plus (const SlipDatum& X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1001, "SlipStringOp+", "", "Operation not supported for string.", X);
      return X;
   }; // SlipDatum SlipStringOp::plus (const SlipDatum& X) const
   SlipDatum SlipStringOp::minus (const SlipDatum& X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1002, "SlipStringOp-", "", "Operation not supported for string.", X);
      return X;
   }; // SlipDatum SlipStringOp::minus (const SlipDatum& X) const
   SlipDatum& SlipStringOp::postInc (SlipDatum& X) const { // operator++
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipStringOp(int)++", "", "Operation not supported for string.", X);
      return X;
   }; // SlipDatum& SlipStringOp::postInc (SlipDatum& X) const
   SlipDatum& SlipStringOp::postDec (SlipDatum& X) const { // operator--
      postError(__FILE__, __LINE__, SlipErr::E1004, "SlipStringOp(int)--", "", "Operation not supported for string.", X);
      return X;
   }; // SlipDatum& SlipStringOp::postDec (SlipDatum& X) const
   SlipDatum& SlipStringOp::preInc (SlipDatum& X) const { // ++operator
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipStringOp++", "", "Operation not supported for string.", X);
      return X;
   }; // SlipDatum& SlipStringOp::preInc (SlipDatum& X) const
   SlipDatum& SlipStringOp::preDec (SlipDatum& X) const { // --operator
      postError(__FILE__, __LINE__, SlipErr::E1004, "SlipStringOp--", "", "Operation not supported for string.", X);
      return X;
   }; // SlipDatum& SlipStringOp::preDec (SlipDatum& X) const

          /*************************************************
           *                Binary operators               *
          *************************************************/
   SlipDatum SlipStringOp::add(const SlipDatum& Y, const SlipDatum& X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1005, "SlipStringOp+", "", "Operation not supported for string.", Y, X);
      return Y;
   }; // SlipDatum SlipStringOp::add(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipStringOp::sub(const SlipDatum& Y, const SlipDatum& X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1006, "SlipStringOp-", "", "Operation not supported for string.", Y, X);
      return Y;
   }; // SlipDatum SlipStringOp::sub(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipStringOp::mul(const SlipDatum& Y, const SlipDatum& X) const { // operator*
      postError(__FILE__, __LINE__, SlipErr::E1007, "SlipStringOp*", "", "Operation not supported for string.", Y, X);
      return Y;
   }; // SlipDatum SlipStringOp::mul(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipStringOp::div(const SlipDatum& Y, const SlipDatum& X) const { // operator/
      postError(__FILE__, __LINE__, SlipErr::E1008, "SlipStringOp/", "", "Operation not supported for string.", Y, X);
      return Y;
   }; // SlipDatum SlipStringOp::div(const SlipDatum& Y, const SlipDatum& X) const
   LONG       SlipStringOp::mod(const SlipDatum& Y, const SlipDatum& X) const { // operator%
      postError(__FILE__, __LINE__, SlipErr::E1026, "SlipStringOp%", "", "Operation not supported for string.", Y, X);
      return 0;
   }; // SlipDatum SlipStringOp::mod(const SlipDatum& Y, const SlipDatum& X) const

   SlipDatum SlipStringOp::add(const SlipDatum& Y, const DOUBLE X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1005, "SlipStringOp+", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum SlipStringOp::add(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipStringOp::sub(const SlipDatum& Y, const DOUBLE X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1006, "SlipStringOp-", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum SlipStringOp::sub(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipStringOp::mul(const SlipDatum& Y, const DOUBLE X) const { // operator*
      postError(__FILE__, __LINE__, SlipErr::E1007, "SlipStringOp*", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum SlipStringOp::mul(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipStringOp::div(const SlipDatum& Y, const DOUBLE X) const { // operator/
      postError(__FILE__, __LINE__, SlipErr::E1008, "SlipStringOp/", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum SlipStringOp::div(const SlipDatum& Y, const DOUBLE X) const

   SlipDatum SlipStringOp::add(const SlipDatum& Y, const ULONG X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1005, "SlipStringOp+", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum SlipStringOp::add(const SlipDatum& Y, const ULONG X) const

   SlipDatum SlipStringOp::sub(const SlipDatum& Y, const ULONG X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1006, "SlipStringOp-", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum SlipStringOp::sub(const SlipDatum& Y, const ULONG X) const
   SlipDatum SlipStringOp::mul(const SlipDatum& Y, const ULONG X) const { // operator*
      postError(__FILE__, __LINE__, SlipErr::E1007, "SlipStringOp*", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum SlipStringOp::mul(const SlipDatum& Y, const ULONG X) const
   SlipDatum SlipStringOp::div(const SlipDatum& Y, const ULONG X) const { // operator/
      postError(__FILE__, __LINE__, SlipErr::E1008, "SlipStringOp/", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum SlipStringOp::div(const SlipDatum& Y, const ULONG X) const
   LONG       SlipStringOp::mod(const SlipDatum& Y, const ULONG X) const { // operator%
      postError(__FILE__, __LINE__, SlipErr::E1026, "SlipStringOp%", "", "Operation not supported for string.", Y);
      return 0;
   }; // SlipDatum SlipStringOp::mod(const SlipDatum& Y, const ULONG X) const

   SlipDatum SlipStringOp::add(const SlipDatum& Y, const LONG X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1005, "SlipStringOp+", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum SlipStringOp::add(const SlipDatum& Y, const LONG X) const

   SlipDatum SlipStringOp::sub(const SlipDatum& Y, const LONG X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1006, "SlipStringOp-", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum SlipStringOp::sub(const SlipDatum& Y, const LONG X) const
   SlipDatum SlipStringOp::mul(const SlipDatum& Y, const LONG X) const { // operator*
      postError(__FILE__, __LINE__, SlipErr::E1007, "SlipStringOp*", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum SlipStringOp::mul(const SlipDatum& Y, const LONG X) const
   SlipDatum SlipStringOp::div(const SlipDatum& Y, const LONG X) const { // operator/
      postError(__FILE__, __LINE__, SlipErr::E1008, "SlipStringOp/", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum SlipStringOp::div(const SlipDatum& Y, const LONG X) const
   LONG       SlipStringOp::mod(const SlipDatum& Y, const LONG X) const { // operator%
      postError(__FILE__, __LINE__, SlipErr::E1026, "SlipStringOp%", "", "Operation not supported for string.", Y);
      return 0;
   }; // SlipDatum SlipStringOp::mod(const SlipDatum& Y, const LONG X) const

          /*************************************************
           *                Bit operators                  *
          *************************************************/
   LONG  SlipStringOp::shl (const SlipDatum& Y, const SlipDatum& X) const { // operator<<
      postError(__FILE__, __LINE__, SlipErr::E1020, "SlipStringOp<<", "", "Operation not supported for string.", Y, X);
      return 0;
   }; // LONG  SlipStringOp::shl (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipStringOp::shr (const SlipDatum& Y, const SlipDatum& X) const { // operator>>
      postError(__FILE__, __LINE__, SlipErr::E1021, "SlipStringOp>>", "", "Operation not supported for string.", Y, X);
      return 0;
   }; // LONG  SlipStringOp::shr (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipStringOp::band(const SlipDatum& Y, const SlipDatum& X) const { // operator&
      postError(__FILE__, __LINE__, SlipErr::E1024, "SlipStringOp&", "", "Operation not supported for string.", Y, X);
      return 0;
   }; // LONG  SlipStringOp::band(const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipStringOp::bor (const SlipDatum& Y, const SlipDatum& X) const { // operator|
      postError(__FILE__, __LINE__, SlipErr::E1023, "SlipStringOp|", "", "Operation not supported for string.", Y, X);
      return 0;
   }; // LONG  SlipStringOp::bor (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipStringOp::bxor(const SlipDatum& Y, const SlipDatum& X) const { // operator^
      postError(__FILE__, __LINE__, SlipErr::E1022, "SlipStringOp^", "", "Operation not supported for string.", Y, X);
      return 0;
   }; // LONG  SlipStringOp::bxor(const SlipDatum& Y, const SlipDatum& X) const

   LONG  SlipStringOp::shl (const SlipDatum& Y, const ULONG X) const { // operator<<
      postError(__FILE__, __LINE__, SlipErr::E1020, "SlipStringOp<<", "", "Operation not supported for string.", Y);
      return 0;
   }; // LONG  SlipStringOp::shl (const SlipDatum& Y, const ULONG X) const
   LONG  SlipStringOp::shr (const SlipDatum& Y, const ULONG X) const { // operator>>
      postError(__FILE__, __LINE__, SlipErr::E1021, "SlipStringOp>>", "", "Operation not supported for string.", Y);
      return 0;
   }; // LONG  SlipStringOp::shr (const SlipDatum& Y, const ULONG X) const
   LONG  SlipStringOp::band(const SlipDatum& Y, const ULONG X) const { // operator&
      postError(__FILE__, __LINE__, SlipErr::E1024, "SlipStringOp&", "", "Operation not supported for string.", Y);
      return 0;
   }; // LONG  SlipStringOp::band(const SlipDatum& Y, const ULONG X) const
   LONG  SlipStringOp::bor (const SlipDatum& Y, const ULONG X) const { // operator|
      postError(__FILE__, __LINE__, SlipErr::E1023, "SlipStringOp|", "", "Operation not supported for string.", Y);
      return 0;
   }; // LONG  SlipStringOp::bor (const SlipDatum& Y, const ULONG X) const
   LONG  SlipStringOp::bxor(const SlipDatum& Y, const ULONG X) const { // operator^
      postError(__FILE__, __LINE__, SlipErr::E1022, "SlipStringOp^", "", "Operation not supported for string.", Y);
      return 0;
   }; // LONG  SlipStringOp::bxor(const SlipDatum& Y, const ULONG X) const

   LONG  SlipStringOp::shl (const SlipDatum& Y, const LONG X) const { // operator<<
      postError(__FILE__, __LINE__, SlipErr::E1020, "SlipStringOp<<", "", "Operation not supported for string.", Y);
      return 0;
   }; // LONG  SlipStringOp::shl (const SlipDatum& Y, const LONG X) const
   LONG  SlipStringOp::shr (const SlipDatum& Y, const LONG X) const { // operator>>
      postError(__FILE__, __LINE__, SlipErr::E1021, "SlipStringOp>>", "", "Operation not supported for string.", Y);
      return 0;
   }; // LONG  SlipStringOp::shr (const SlipDatum& Y, const LONG X) const
   LONG  SlipStringOp::band(const SlipDatum& Y, const LONG X) const { // operator&
      postError(__FILE__, __LINE__, SlipErr::E1024, "SlipStringOp&", "", "Operation not supported for string.", Y);
      return 0;
   }; // LONG  SlipStringOp::band(const SlipDatum& Y, const LONG X) const
   LONG  SlipStringOp::bor (const SlipDatum& Y, const LONG X) const { // operator|
      postError(__FILE__, __LINE__, SlipErr::E1023, "SlipStringOp|", "", "Operation not supported for string.", Y);
      return 0;
   }; // LONG  SlipStringOp::bor (const SlipDatum& Y, const LONG X) const
   LONG  SlipStringOp::bxor(const SlipDatum& Y, const LONG X) const { // operator^
      postError(__FILE__, __LINE__, SlipErr::E1022, "SlipStringOp^", "", "Operation not supported for string.", Y);
      return 0;
   }; // LONG  SlipStringOp::bxor(const SlipDatum& Y, const LONG X) const

          /*************************************************
           *               Simple Assignment               *
          *************************************************/
   SlipDatum& SlipStringOp::asgn(SlipDatum& Y, const SlipDatum& X) const {        // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1022, "SlipStringOp^", "", "Operation not supported for string.", Y);
//      *getPtr(Y) = (STRING)X;
      return Y;
   }; // SlipDatum& SlipStringOp::asgn(SlipDatum& Y, const SlipDatum& X) const

          /*************************************************
           *       Arithmetic Assignment operators         *
          *************************************************/
   SlipDatum& SlipStringOp::addAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1009, "SlipStringOp+=", "", "Operation not supported for string.", Y, X);
      return Y;
   }; // SlipDatum& SlipStringOp::addAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipStringOp::subAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator-=
      postError(__FILE__, __LINE__, SlipErr::E1010, "SlipStringOp-=", "", "Operation not supported for string.", Y, X);
      return Y;
   }; // SlipDatum& SlipStringOp::subAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipStringOp::mulAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator*=
      postError(__FILE__, __LINE__, SlipErr::E1011, "SlipStringOp*=", "", "Operation not supported for string.", Y, X);
      return Y;
   }; // SlipDatum& SlipStringOp::mulAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipStringOp::divAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator/=
      postError(__FILE__, __LINE__, SlipErr::E1012, "SlipStringOp/=", "", "Operation not supported for string.", Y, X);
      return Y;
   }; // SlipDatum& SlipStringOp::divAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipStringOp::modAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator%=
      postError(__FILE__, __LINE__, SlipErr::E1013, "SlipStringOp%=", "", "Operation not supported for string.", Y, X);
      return Y;
   }; // SlipDatum& SlipStringOp::modAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipStringOp::shlAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator<<=
      postError(__FILE__, __LINE__, SlipErr::E1014, "SlipStringOp<<=", "", "Operation not supported for string.", Y, X);
      return Y;
   }; // SlipDatum& SlipStringOp::shlAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipStringOp::shrAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator>>=
      postError(__FILE__, __LINE__, SlipErr::E1015, "SlipStringOp>>=", "", "Operation not supported for string.", Y, X);
      return Y;
   }; // SlipDatum& SlipStringOp::shrAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipStringOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator&=
      postError(__FILE__, __LINE__, SlipErr::E1016, "SlipStringOp&=", "", "Operation not supported for string.", Y, X);
      return Y;
   }; // SlipDatum& SlipStringOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipStringOp::borAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator|=
      postError(__FILE__, __LINE__, SlipErr::E1019, "SlipStringOp|=", "", "Operation not supported for string.", Y, X);
      return Y;
   }; // SlipDatum& SlipStringOp::borAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipStringOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator^=
      postError(__FILE__, __LINE__, SlipErr::E1017, "SlipStringOp^=", "", "Operation not supported for string.", Y, X);
      return Y;
   }; // SlipDatum& SlipStringOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const

   SlipDatum& SlipStringOp::addAsgn (SlipDatum& Y, const DOUBLE X) const { // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1009, "SlipStringOp", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum& SlipStringOp::addAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipStringOp::subAsgn (SlipDatum& Y, const DOUBLE X) const { // operator-=
      postError(__FILE__, __LINE__, SlipErr::E1010, "SlipStringOp", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum& SlipStringOp::subAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipStringOp::mulAsgn (SlipDatum& Y, const DOUBLE X) const { // operator*=
      postError(__FILE__, __LINE__, SlipErr::E1011, "SlipStringOp", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum& SlipStringOp::mulAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipStringOp::divAsgn (SlipDatum& Y, const DOUBLE X) const { // operator/=
      postError(__FILE__, __LINE__, SlipErr::E1012, "SlipStringOp", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum& SlipStringOp::divAsgn (SlipDatum& Y, const DOUBLE X) const

   SlipDatum& SlipStringOp::addAsgn (SlipDatum& Y, const ULONG X) const { // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1009, "SlipStringOp+=", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum& SlipStringOp::addAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipStringOp::subAsgn (SlipDatum& Y, const ULONG X) const { // operator-=
      postError(__FILE__, __LINE__, SlipErr::E1010, "SlipSublistO-=p", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum& SlipStringOp::subAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipStringOp::mulAsgn (SlipDatum& Y, const ULONG X) const { // operator*=
      postError(__FILE__, __LINE__, SlipErr::E1011, "SlipStringOp*=", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum& SlipStringOp::mulAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipStringOp::divAsgn (SlipDatum& Y, const ULONG X) const { // operator/=
      postError(__FILE__, __LINE__, SlipErr::E1012, "SlipStringOp/=", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum& SlipStringOp::divAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipStringOp::modAsgn (SlipDatum& Y, const ULONG X) const { // operator%=
      postError(__FILE__, __LINE__, SlipErr::E1013, "SlipStringOp%=", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum& SlipStringOp::modAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipStringOp::shlAsgn (SlipDatum& Y, const ULONG X) const { // operator<<=
      postError(__FILE__, __LINE__, SlipErr::E1014, "SlipStringOp<<=", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum& SlipStringOp::shlAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipStringOp::shrAsgn (SlipDatum& Y, const ULONG X) const { // operator>>=
      postError(__FILE__, __LINE__, SlipErr::E1015, "SlipStringOp>>=", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum& SlipStringOp::shrAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipStringOp::bandAsgn(SlipDatum& Y, const ULONG X) const { // operator&=
      postError(__FILE__, __LINE__, SlipErr::E1016, "SlipStringOp&=", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum& SlipStringOp::bandAsgn(SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipStringOp::borAsgn (SlipDatum& Y, const ULONG X) const { // operator|=
      postError(__FILE__, __LINE__, SlipErr::E1019, "SlipStringOp|=", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum& SlipStringOp::borAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipStringOp::bxorAsgn(SlipDatum& Y, const ULONG X) const { // operator^=
      postError(__FILE__, __LINE__, SlipErr::E1017, "SlipStringOp^=", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum& SlipStringOp::bxorAsgn(SlipDatum& Y, const ULONG X) const

   SlipDatum& SlipStringOp::addAsgn (SlipDatum& Y, const LONG X) const { // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1009, "SlipStringOp+=", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum& SlipStringOp::addAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipStringOp::subAsgn (SlipDatum& Y, const LONG X) const { // operator-=
      postError(__FILE__, __LINE__, SlipErr::E1010, "SlipStringOp-=", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum& SlipStringOp::subAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipStringOp::mulAsgn (SlipDatum& Y, const LONG X) const { // operator*=
      postError(__FILE__, __LINE__, SlipErr::E1011, "SlipStringOp*=", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum& SlipStringOp::mulAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipStringOp::divAsgn (SlipDatum& Y, const LONG X) const { // operator/=
      postError(__FILE__, __LINE__, SlipErr::E1012, "SlipStringOp/=", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum& SlipStringOp::divAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipStringOp::modAsgn (SlipDatum& Y, const LONG X) const { // operator%=
      postError(__FILE__, __LINE__, SlipErr::E1013, "SlipStringOp%=", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum& SlipStringOp::modAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipStringOp::shlAsgn (SlipDatum& Y, const LONG X) const { // operator<<=
      postError(__FILE__, __LINE__, SlipErr::E1014, "SlipStringOp<<=", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum& SlipStringOp::shlAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipStringOp::shrAsgn (SlipDatum& Y, const LONG X) const { // operator>>=
      postError(__FILE__, __LINE__, SlipErr::E1015, "SlipStringOp>>=", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum& SlipStringOp::shrAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipStringOp::bandAsgn(SlipDatum& Y, const LONG X) const { // operator&=
      postError(__FILE__, __LINE__, SlipErr::E1016, "SlipStringOp&=", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum& SlipStringOp::bandAsgn(SlipDatum& Y, const LONG X) const
   SlipDatum& SlipStringOp::borAsgn (SlipDatum& Y, const LONG X) const { // operator|=
      postError(__FILE__, __LINE__, SlipErr::E1019, "SlipStringOp|=", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum& SlipStringOp::borAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipStringOp::bxorAsgn(SlipDatum& Y, const LONG X) const { // operator^=
      postError(__FILE__, __LINE__, SlipErr::E1017, "SlipStringOp^=", "", "Operation not supported for string.", Y);
      return Y;
   }; // SlipDatum& SlipStringOp::bxorAsgn(SlipDatum& Y, const LONG X) const

          /*************************************************
           *               Logical operators               *
          *************************************************/
   bool SlipStringOp::notEqual (const SlipDatum& Y, const SlipDatum& X) const { // operator!=
      if (X.getClassType() != eSTRING) return false;
      return Y.toString() != X.toString();
   }; // bool SlipStringOp::notEqual (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipStringOp::lessThan (const SlipDatum& Y, const SlipDatum& X) const { // operator<
      if (X.getClassType() != eSTRING) return false;
      return Y.toString() < X.toString();
   }; // bool SlipStringOp::lessThan (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipStringOp::lessEqual (const SlipDatum& Y, const SlipDatum& X) const { // operator<=
      if (X.getClassType() != eSTRING) return false;
      return Y.toString() <= X.toString();
   }; // bool SlipStringOp::lessEqual (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipStringOp::equal (const SlipDatum& Y, const SlipDatum& X) const { // operator==
      if (X.getClassType() != eSTRING) return false;
      return Y.toString() == X.toString();
   }; // bool SlipStringOp::equal (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipStringOp::greatEqual(const SlipDatum& Y, const SlipDatum& X) const { // operator>=
      if (X.getClassType() != eSTRING) return false;
      return Y.toString() >= X.toString();
  }; // bool SlipStringOp::greatEqual(const SlipDatum& Y, const SlipDatum& X) const
   bool SlipStringOp::greatThan (const SlipDatum& Y, const SlipDatum& X) const { // operator>
      if (X.getClassType() != eSTRING) return false;
      return Y.toString() > X.toString();
   }; // bool SlipStringOp::greatThan (const SlipDatum& Y, const SlipDatum& X) const

   bool SlipStringOp::notEqual (const SlipDatum& Y, const DOUBLE X) const { // operator!=
      stringstream str;
      str << X;
      return (Y.toString() != str.str());
   }; // bool SlipStringOp::notEqual (const SlipDatum& Y, const DOUBLE X) const
   bool SlipStringOp::lessThan (const SlipDatum& Y, const DOUBLE X) const { // operator<
      stringstream str;
      str << X;
      return (Y.toString() < str.str());
   }; // bool SlipStringOp::lessThan (const SlipDatum& Y, const DOUBLE X) const
   bool SlipStringOp::lessEqual (const SlipDatum& Y, const DOUBLE X) const { // operator<=
      stringstream str;
      str << X;
      return (Y.toString() <= str.str());
   }; // bool SlipStringOp::lessEqual (const SlipDatum& Y, const DOUBLE X) const
   bool SlipStringOp::equal (const SlipDatum& Y, const DOUBLE X) const { // operator==
      stringstream str;
      str << X;
      return (Y.toString() == str.str());
   }; // bool SlipStringOp::equal (const SlipDatum& Y, const DOUBLE X) const
   bool SlipStringOp::greatEqual(const SlipDatum& Y, const DOUBLE X) const { // operator>=
      stringstream str;
      str << X;
      return (Y.toString() >= str.str());
   }; // bool SlipStringOp::greatEqual(const SlipDatum& Y, const DOUBLE X) const
   bool SlipStringOp::greatThan (const SlipDatum& Y, const DOUBLE X) const { // operator>
      stringstream str;
      str << X;
      return (Y.toString() > str.str());
   }; // bool SlipStringOp::greatThan (const SlipDatum& Y, const DOUBLE X) const

   bool SlipStringOp::notEqual (const SlipDatum& Y, const ULONG X) const { // operator!=
      stringstream str;
      str << X;
      return (Y.toString() != str.str());
   }; // bool SlipStringOp::notEqual (const SlipDatum& Y, const ULONG X) const
   bool SlipStringOp::lessThan (const SlipDatum& Y, const ULONG X) const { // operator<
      stringstream str;
      str << X;
      return (Y.toString() < str.str());
   }; // bool SlipStringOp::lessThan (const SlipDatum& Y, const ULONG X) const
   bool SlipStringOp::lessEqual (const SlipDatum& Y, const ULONG X) const { // operator<=
      stringstream str;
      str << X;
      return (Y.toString() <= str.str());
   }; // bool SlipStringOp::lessEqual (const SlipDatum& Y, const ULONG X) const
   bool SlipStringOp::equal (const SlipDatum& Y, const ULONG X) const { // operator==
      stringstream str;
      str << X;
      return (Y.toString() == str.str());
   }; // bool SlipStringOp::equal (const SlipDatum& Y, const ULONG X) const
   bool SlipStringOp::greatEqual(const SlipDatum& Y, const ULONG X) const { // operator>=
      stringstream str;
      str << X;
      return (Y.toString() >= str.str());
   }; // bool SlipStringOp::greatEqual(const SlipDatum& Y, const ULONG X) const
   bool SlipStringOp::greatThan (const SlipDatum& Y, const ULONG X) const { // operator>
      stringstream str;
      str << X;
      return (Y.toString() > str.str());
   }; // bool SlipStringOp::greatThan (const SlipDatum& Y, const ULONG X) const

   bool SlipStringOp::notEqual (const SlipDatum& Y, const LONG X) const { // operator!=
      stringstream str;
      str << X;
      return (Y.toString() != str.str());
   }; // bool SlipStringOp::notEqual (const SlipDatum& Y, const LONG X) const
   bool SlipStringOp::lessThan (const SlipDatum& Y, const LONG X) const { // operator<
      stringstream str;
      str << X;
      return (Y.toString() < str.str());
   }; // bool SlipStringOp::lessThan (const SlipDatum& Y, const LONG X) const
   bool SlipStringOp::lessEqual (const SlipDatum& Y, const LONG X) const { // operator<=
      stringstream str;
      str << X;
      return (Y.toString() <= str.str());
   }; // bool SlipStringOp::lessEqual (const SlipDatum& Y, const LONG X) const
   bool SlipStringOp::equal (const SlipDatum& Y, const LONG X) const { // operator==
      stringstream str;
      str << X;
      return (Y.toString() == str.str());
   }; // bool SlipStringOp::equal (const SlipDatum& Y, const LONG X) const
   bool SlipStringOp::greatEqual(const SlipDatum& Y, const LONG X) const { // operator>=
      stringstream str;
      str << X;
      return (Y.toString() >= str.str());
   }; // bool SlipStringOp::greatEqual(const SlipDatum& Y, const LONG X) const
   bool SlipStringOp::greatThan (const SlipDatum& Y, const LONG X) const { // operator>
      stringstream str;
      str << X;
      return (Y.toString() > str.str());
   }; // bool SlipStringOp::greatThan (const SlipDatum& Y, const LONG X) const

   bool SlipStringOp::notEqual (const SlipDatum& Y, const PTR X) const { // operator!=
       postError(__FILE__, __LINE__, SlipErr::E1028, "SlipStringOp!=", "", "Operation not supported for string.", Y);
       return false;
   }; // bool SlipStringOp::notEqual (const SlipDatum& Y, const PTR X) const
   bool SlipStringOp::equal (const SlipDatum& Y, const PTR X) const { // operator==
       postError(__FILE__, __LINE__, SlipErr::E1029, "SlipStringOp==", "", "Operation not supported for string.", Y);
       return false;
   }; // bool SlipStringOp::equal (const SlipDatum& Y, const PTR X) const

   bool SlipStringOp::notEqual (const SlipDatum& Y, const string& X) const { // operator!=
      return Y.toString() != X;
   }; // bool SlipStringOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipStringOp::lessThan (const SlipDatum& Y, const string& X) const { // operator<
      return Y.toString() < X;
   }; // bool SlipStringOp::equal (const SlipDatum& Y, const string& X) const
   bool SlipStringOp::lessEqual (const SlipDatum& Y, const string& X) const { // operator<=
      return Y.toString() <= X;
   }; // bool SlipStringOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipStringOp::equal (const SlipDatum& Y, const string& X) const { // operator==
      return Y.toString() == X;
   }; // bool SlipStringOp::equal (const SlipDatum& Y, const string& X) const
   bool SlipStringOp::greatEqual (const SlipDatum& Y, const string& X) const { // operator>=
      return Y.toString() >= X;
   }; // bool SlipStringOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipStringOp::greatThan (const SlipDatum& Y, const string& X) const {   // operator>
      return Y.toString() > X;
   }; // bool SlipStringOp::equal (const SlipDatum& Y, const string& X) const

   SlipDatum& SlipStringOp::copy(const SlipDatum& X) const {                    // Return a reference to a copy of the current cell
      return (SlipDatum&)X;
   }; // SlipDatum&  SlipStringOp::copy(const SlipDatum& X) const

   void SlipStringOp::remove(const SlipDatum& X) {                              // Delete the datum in this cell
      PTR ptr = *getPtr(X);
      ptr->remove();
   }; // voidvSlipPtrOp::remove(const SlipDatum& X)

   sAllData SlipStringOp::getDatumCopy(const SlipDatum& X) const {              // Return valid datum
      Data data = { 0, };
      data.Ptr = (*getPtr(X))->copy();                                          // Return a SlipPointer defined copy
      return data.allData;
   }; // sAllData SlipPtrOp::getDatumCopy(SlipDatum& X) const

   string SlipStringOp::toString(const SlipCell& X) const {                     // pretty print SLIP cell
      SlipPointer* ptr = *getPtr(X);
      return ptr->toString();
   }; // string SlipStringOp::toString(const SlipCell& X) const

   string SlipStringOp::dump(const SlipCell& X) const {
      stringstream pretty;
      PTR ptr = *getPtr(X);

      pretty << "[string   ] " << dumpLink(X)  << " = "
              << setfill('0') << setw(8) << hex << ptr
              << " -> ";
      pretty << ptr->dump();
      return pretty.str();
   }; // string SlipStringOp::dump(const SlipCell& X) const

   string SlipStringOp::write(const SlipCell& X) const {                        // pretty print SLIP cell
      SlipPointer* ptr = *getPtr(X);
      return ptr->write();
   }; // string SlipStringOp::write(const SlipCell& X) const

}; // namespace slip