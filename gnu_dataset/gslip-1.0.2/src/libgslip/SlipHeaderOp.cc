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
 * @file    SlipHeaderOp.cpp
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
 # include "SlipHeader.h"
 # include "SlipHeaderOp.h"

using namespace slip;

namespace slip {

   /*********************************************************
    *                  Casting Operators                    *
   *********************************************************/

   bool SlipHeaderOp::cast_bool(const SlipDatum& X) const  {                    // (bool) X
      postError(__FILE__, __LINE__, SlipErr::E1035, "SlipHeaderOp(bool)", "", "Operation not supported for header.", X);
      return false;
   }; // bool SlipHeaderOp::cast_bool(const SlipDatum& X) const
   UCHAR SlipHeaderOp::cast_UCHAR(const SlipDatum& X) const  {                  // (UCHAR) X
      postError(__FILE__, __LINE__, SlipErr::E1036, "SlipHeaderOp(UCHAR)", "", "Operation not supported for header.", X);
      return 0;
   }; // UCHAR SlipHeaderOp::cast_UCHAR(const SlipDatum& X) const
   CHAR SlipHeaderOp::cast_CHAR(const SlipDatum& X) const  {                    // (CHAR) X
      postError(__FILE__, __LINE__, SlipErr::E1037, "SlipHeaderOp(CHAR)", "", "Operation not supported for header.", X);
      return 0;
   }; // CHAR SlipHeaderOp::cast_CHAR(const SlipDatum& X) const
   ULONG SlipHeaderOp::cast_ULONG(const SlipDatum& X) const  {                  // (ULONG) X
      postError(__FILE__, __LINE__, SlipErr::E1038, "SlipHeaderOp(ULONG)", "", "Operation not supported for header.", X);
      return 0;
   }; // ULONG SlipHeaderOp::cast_ULONG(const SlipDatum& X) const
   LONG SlipHeaderOp::cast_LONG(const SlipDatum& X) const  {                    // (LONG) X
      postError(__FILE__, __LINE__, SlipErr::E1039, "SlipHeaderOp(LONG)", "", "Operation not supported for header.", X);
      return 0;
   }; // LONG SlipHeaderOp::cast_LONG(const SlipDatum& X) const
   DOUBLE SlipHeaderOp::cast_DOUBLE(const SlipDatum& X) const  {                // (DOUBLE) X
      postError(__FILE__, __LINE__, SlipErr::E1040, "SlipHeaderOp(DOUBLE)", "", "Operation not supported for header.", X);
      return 0.0;
   }; // DOUBLE SlipHeaderOp::cast_DOUBLE(const SlipDatum& X) const
   PTR SlipHeaderOp::cast_PTR(const SlipDatum& Y) const {                        // (PTR) Y
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipBoolOp++", "", "Operation not supported for HEADER.", Y);
      return NULL;
   }; // PTR SlipHeaderOp::cast_PTR(const SlipDatum& Y) const

      /*********************************************************
       *                 Operator Overloading                  *
      *********************************************************/

          /*************************************************
           *                Unary operators                *
          *************************************************/
   ULONG SlipHeaderOp::bitNOT (const SlipDatum& X) const { // operator~
      postError(__FILE__, __LINE__, SlipErr::E1043, "SlipHeaderOpoperato~r", "", "Operation not supported for header.", X);
      return 0;
   }; // ULONG SlipHeaderOp::bitNOT (const SlipDatum& X) const
   bool SlipHeaderOp::logNOT (const SlipDatum& X) const { // operator!
      postError(__FILE__, __LINE__, SlipErr::E1025, "SlipHeaderOpoperator!", "", "Operation not supported for header.", X);
      return false;
   }; // bool SlipHeaderOp::logNOT (const SlipDatum& X) const
   SlipDatum SlipHeaderOp::plus (const SlipDatum& X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1001, "SlipHeaderOp+", "", "Operation not supported for header.", X);
      return X;
   }; // SlipDatum& SlipHeaderOp::plus (const SlipDatum& X) const
   SlipDatum SlipHeaderOp::minus (const SlipDatum& X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1002, "SlipHeaderOp-", "", "Operation not supported for header.", X);
      return X;
   }; // SlipDatum& SlipHeaderOp::minus (const SlipDatum& X) const
   SlipDatum& SlipHeaderOp::postInc (SlipDatum& X) const { // operator++
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipHeaderOp(iny)++", "", "Operation not supported for header.", X);
      return X;
   }; // SlipDatum& SlipHeaderOp::postInc (SlipDatum& X) const
   SlipDatum& SlipHeaderOp::postDec (SlipDatum& X) const { // operator--
      postError(__FILE__, __LINE__, SlipErr::E1004, "SlipHeaderOp(int)--", "", "Operation not supported for header.", X);
      return X;
   }; // SlipDatum& SlipHeaderOp::postDec (SlipDatum& X) const
   SlipDatum& SlipHeaderOp::preInc (SlipDatum& X) const { // ++operator
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipHeaderOp++", "", "Operation not supported for header.", X);
      return X;
   }; // SlipDatum& SlipHeaderOp::preInc (SlipDatum& X) const
   SlipDatum& SlipHeaderOp::preDec (SlipDatum& X) const { // --operator
      postError(__FILE__, __LINE__, SlipErr::E1004, "SlipHeaderOp--", "", "Operation not supported for header.", X);
      return X;
   }; // SlipDatum& SlipHeaderOp::preDec (SlipDatum& X) const

          /*************************************************
           *                Binary operators               *
          *************************************************/
   SlipDatum SlipHeaderOp::add(const SlipDatum& Y, const SlipDatum& X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1005, "SlipHeaderOp+", "", "Operation not supported for header.", Y, X);
      return Y;
   }; // SlipDatum SlipHeaderOp::add(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipHeaderOp::sub(const SlipDatum& Y, const SlipDatum& X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1006, "SlipHeaderOp-", "", "Operation not supported for header.", Y, X);
      return Y;
   }; // SlipDatum SlipHeaderOp::sub(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipHeaderOp::mul(const SlipDatum& Y, const SlipDatum& X) const { // operator*
      postError(__FILE__, __LINE__, SlipErr::E1007, "SlipHeaderOp*", "", "Operation not supported for header.", Y, X);
      return Y;
   }; // SlipDatum SlipHeaderOp::mul(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipHeaderOp::div(const SlipDatum& Y, const SlipDatum& X) const { // operator/
      postError(__FILE__, __LINE__, SlipErr::E1008, "SlipHeaderOp/", "", "Operation not supported for header.", Y, X);
      return Y;
   }; // SlipDatum SlipHeaderOp::div(const SlipDatum& Y, const SlipDatum& X) const
   LONG SlipHeaderOp::mod(const SlipDatum& Y, const SlipDatum& X) const { // operator%
      postError(__FILE__, __LINE__, SlipErr::E1026, "SlipHeaderOp%", "", "Operation not supported for header.", Y, X);
      return 0;
   }; // SlipDatum SlipHeaderOp::mod(const SlipDatum& Y, const SlipDatum& X) const

   SlipDatum SlipHeaderOp::add(const SlipDatum& Y, const DOUBLE X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1005, "SlipHeaderOp+", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum SlipHeaderOp::add(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipHeaderOp::sub(const SlipDatum& Y, const DOUBLE X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1006, "SlipHeaderOp-", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum SlipHeaderOp::sub(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipHeaderOp::mul(const SlipDatum& Y, const DOUBLE X) const { // operator*
      postError(__FILE__, __LINE__, SlipErr::E1007, "SlipHeaderOp*", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum SlipHeaderOp::mul(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipHeaderOp::div(const SlipDatum& Y, const DOUBLE X) const { // operator/
      postError(__FILE__, __LINE__, SlipErr::E1008, "SlipHeaderOp/", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum SlipHeaderOp::div(const SlipDatum& Y, const DOUBLE X) const

   SlipDatum SlipHeaderOp::add(const SlipDatum& Y, const ULONG X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1005, "SlipHeaderOp+", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum SlipHeaderOp::add(const SlipDatum& Y, const ULONG X) const

   SlipDatum SlipHeaderOp::sub(const SlipDatum& Y, const ULONG X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1006, "SlipHeaderOp-", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum SlipHeaderOp::sub(const SlipDatum& Y, const ULONG X) const
   SlipDatum SlipHeaderOp::mul(const SlipDatum& Y, const ULONG X) const { // operator*
      postError(__FILE__, __LINE__, SlipErr::E1007, "SlipHeaderOp*", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum SlipHeaderOp::mul(const SlipDatum& Y, const ULONG X) const
   SlipDatum SlipHeaderOp::div(const SlipDatum& Y, const ULONG X) const { // operator/
      postError(__FILE__, __LINE__, SlipErr::E1008, "SlipHeaderOp/", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum SlipHeaderOp::div(const SlipDatum& Y, const ULONG X) const
   LONG       SlipHeaderOp::mod(const SlipDatum& Y, const ULONG X) const { // operator%
      postError(__FILE__, __LINE__, SlipErr::E1026, "%", "", "Operation not supported for header.", Y);
      return 0;
   }; // SlipDatum SlipHeaderOp::mod(const SlipDatum& Y, const ULONG X) const

   SlipDatum SlipHeaderOp::add(const SlipDatum& Y, const LONG X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1005, "SlipHeaderOp+", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum SlipHeaderOp::add(const SlipDatum& Y, const LONG X) const

   SlipDatum SlipHeaderOp::sub(const SlipDatum& Y, const LONG X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1006, "SlipHeaderOp-", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum SlipHeaderOp::sub(const SlipDatum& Y, const LONG X) const
   SlipDatum SlipHeaderOp::mul(const SlipDatum& Y, const LONG X) const { // operator*
      postError(__FILE__, __LINE__, SlipErr::E1007, "SlipHeaderOp*", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum SlipHeaderOp::mul(const SlipDatum& Y, const LONG X) const
   SlipDatum SlipHeaderOp::div(const SlipDatum& Y, const LONG X) const { // operator/
      postError(__FILE__, __LINE__, SlipErr::E1008, "SlipHeaderOp/", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum SlipHeaderOp::div(const SlipDatum& Y, const LONG X) const
   LONG       SlipHeaderOp::mod(const SlipDatum& Y, const LONG X) const { // operator%
      postError(__FILE__, __LINE__, SlipErr::E1026, "SlipHeaderOp%", "", "Operation not supported for header.", Y);
      return 0;
   }; // SlipDatum SlipHeaderOp::mod(const SlipDatum& Y, const LONG X) const

          /*************************************************
           *                Bit operators                  *
          *************************************************/
   LONG  SlipHeaderOp::shl (const SlipDatum& Y, const SlipDatum& X) const { // operator<<
      postError(__FILE__, __LINE__, SlipErr::E1020, "SlipHeaderOp<<", "", "Operation not supported for header.", Y, X);
      return 0;
   }; // LONG  SlipHeaderOp::shl (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipHeaderOp::shr (const SlipDatum& Y, const SlipDatum& X) const { // operator>>
      postError(__FILE__, __LINE__, SlipErr::E1021, "SlipHeaderOp>>", "", "Operation not supported for header.", Y, X);
      return 0;
   }; // LONG  SlipHeaderOp::shr (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipHeaderOp::band(const SlipDatum& Y, const SlipDatum& X) const { // operator&
      postError(__FILE__, __LINE__, SlipErr::E1024, "SlipHeaderOp&", "", "Operation not supported for header.", Y, X);
      return 0;
   }; // LONG  SlipHeaderOp::band(const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipHeaderOp::bor (const SlipDatum& Y, const SlipDatum& X) const { // operator|
      postError(__FILE__, __LINE__, SlipErr::E1023, "SlipHeaderOp|", "", "Operation not supported for header.", Y, X);
      return 0;
   }; // LONG  SlipHeaderOp::bor (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipHeaderOp::bxor(const SlipDatum& Y, const SlipDatum& X) const { // operator^
      postError(__FILE__, __LINE__, SlipErr::E1022, "SlipHeaderOp^", "", "Operation not supported for header.", Y, X);
      return 0;
   }; // LONG  SlipHeaderOp::bxor(const SlipDatum& Y, const SlipDatum& X) const

   LONG  SlipHeaderOp::shl (const SlipDatum& Y, const ULONG X) const { // operator<<
      postError(__FILE__, __LINE__, SlipErr::E1020, "SlipHeaderOp<<", "Operation not supported for header.", "", Y);
      return 0;
   }; // LONG  SlipHeaderOp::shl (const SlipDatum& Y, const ULONG X) const
   LONG  SlipHeaderOp::shr (const SlipDatum& Y, const ULONG X) const { // operator>>
      postError(__FILE__, __LINE__, SlipErr::E1021, "SlipHeaderOp>>", "Operation not supported for header.", "", Y);
      return 0;
   }; // LONG  SlipHeaderOp::shr (const SlipDatum& Y, const ULONG X) const
   LONG  SlipHeaderOp::band(const SlipDatum& Y, const ULONG X) const { // operator&
      postError(__FILE__, __LINE__, SlipErr::E1024, "SlipHeaderOp&", "Operation not supported for header.", "", Y);
      return 0;
   }; // LONG  SlipHeaderOp::band(const SlipDatum& Y, const ULONG X) const
   LONG  SlipHeaderOp::bor (const SlipDatum& Y, const ULONG X) const { // operator|
      postError(__FILE__, __LINE__, SlipErr::E1023, "SlipHeaderOp|", "Operation not supported for header.", "", Y);
      return 0;
   }; // LONG  SlipHeaderOp::bor (const SlipDatum& Y, const ULONG X) const
   LONG  SlipHeaderOp::bxor(const SlipDatum& Y, const ULONG X) const { // operator^
      postError(__FILE__, __LINE__, SlipErr::E1022, "SlipHeaderOp^", "", "Operation not supported for header.", Y);
      return 0;
   }; // LONG  SlipHeaderOp::bxor(const SlipDatum& Y, const ULONG X) const

   LONG  SlipHeaderOp::shl (const SlipDatum& Y, const LONG X) const { // operator<<
      postError(__FILE__, __LINE__, SlipErr::E1020, "SlipHeaderOp<<", "", "Operation not supported for header.", Y);
      return 0;
   }; // LONG  SlipHeaderOp::shl (const SlipDatum& Y, const LONG X) const
   LONG  SlipHeaderOp::shr (const SlipDatum& Y, const LONG X) const { // operator>>
      postError(__FILE__, __LINE__, SlipErr::E1021, "SlipHeaderOp>>", "", "Operation not supported for header.", Y);
      return 0;
   }; // LONG  SlipHeaderOp::shr (const SlipDatum& Y, const LONG X) const
   LONG  SlipHeaderOp::band(const SlipDatum& Y, const LONG X) const { // operator&
      postError(__FILE__, __LINE__, SlipErr::E1024, "SlipHeaderOp&", "", "Operation not supported for header.", Y);
      return 0;
   }; // LONG  SlipHeaderOp::band(const SlipDatum& Y, const LONG X) const
   LONG  SlipHeaderOp::bor (const SlipDatum& Y, const LONG X) const { // operator|
      postError(__FILE__, __LINE__, SlipErr::E1023, "SlipHeaderOp|", "", "Operation not supported for header.", Y);
      return 0;
   }; // LONG  SlipHeaderOp::bor (const SlipDatum& Y, const LONG X) const
   LONG  SlipHeaderOp::bxor(const SlipDatum& Y, const LONG X) const { // operator^
      postError(__FILE__, __LINE__, SlipErr::E1022, "SlipHeaderOp^", "", "Operation not supported for header.", Y);
      return 0;
   }; // LONG  SlipHeaderOp::bxor(const SlipDatum& Y, const LONG X) const

          /*************************************************
           *               Simple Assignment               *
          *************************************************/
   SlipDatum& SlipHeaderOp::asgn(SlipDatum& Y, const SlipDatum& X) const {        // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1022, "SlipHeaderOp^", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::asgn(SlipDatum& Y, const SlipDatum& X) const

          /*************************************************
           *       Arithmetic Assignment operators         *
          *************************************************/
   SlipDatum& SlipHeaderOp::addAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1009, "SlipHeaderOp+=", "", "Operation not supported for header.", X);
      return Y;
   }; // SlipDatum& SlipHeaderOp::addAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipHeaderOp::subAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator-=
      postError(__FILE__, __LINE__, SlipErr::E1010, "SlipHeaderOp-=", "", "Operation not supported for header.", X);
      return Y;
   }; // SlipDatum& SlipHeaderOp::subAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipHeaderOp::mulAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator*=
      postError(__FILE__, __LINE__, SlipErr::E1011, "SlipHeaderOp*=", "", "Operation not supported for header.", X);
      return Y;
   }; // SlipDatum& SlipHeaderOp::mulAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipHeaderOp::divAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator/=
      postError(__FILE__, __LINE__, SlipErr::E1012, "SlipHeaderOp/=", "", "Operation not supported for header.", X);
      return Y;
   }; // SlipDatum& SlipHeaderOp::divAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipHeaderOp::modAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator%=
      postError(__FILE__, __LINE__, SlipErr::E1013, "SlipHeaderOp%=", "", "Operation not supported for header.", Y, X);
      return Y;
   }; // SlipDatum& SlipHeaderOp::modAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipHeaderOp::shlAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator<<=
      postError(__FILE__, __LINE__, SlipErr::E1014, "SlipHeaderOp<<=", "", "Operation not supported for header.", Y, X);
      return Y;
   }; // SlipDatum& SlipHeaderOp::shlAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipHeaderOp::shrAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator>>=
      postError(__FILE__, __LINE__, SlipErr::E1015, "SlipHeaderOp>>=", "", "Operation not supported for header.", Y, X);
      return Y;
   }; // SlipDatum& SlipHeaderOp::shrAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipHeaderOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator&=
      postError(__FILE__, __LINE__, SlipErr::E1016, "SlipHeaderOp&=", "", "Operation not supported for header.", Y, X);
      return Y;
   }; // SlipDatum& SlipHeaderOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipHeaderOp::borAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator|=
      postError(__FILE__, __LINE__, SlipErr::E1019, "SlipHeaderOp|=", "", "Operation not supported for header.", Y, X);
      return Y;
   }; // SlipDatum& SlipHeaderOp::borAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipHeaderOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator^=
      postError(__FILE__, __LINE__, SlipErr::E1017, "SlipHeaderOp^=", "", "Operation not supported for header.", Y, X);
      return Y;
   }; // SlipDatum& SlipHeaderOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const

   SlipDatum& SlipHeaderOp::addAsgn (SlipDatum& Y, const DOUBLE X) const { // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1009, "SlipHeaderOp+=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::addAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipHeaderOp::subAsgn (SlipDatum& Y, const DOUBLE X) const { // operator-=
      postError(__FILE__, __LINE__, SlipErr::E1010, "SlipHeaderOp-=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::subAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipHeaderOp::mulAsgn (SlipDatum& Y, const DOUBLE X) const { // operator*=
      postError(__FILE__, __LINE__, SlipErr::E1011, "SlipHeaderOp*=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::mulAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipHeaderOp::divAsgn (SlipDatum& Y, const DOUBLE X) const { // operator/=
      postError(__FILE__, __LINE__, SlipErr::E1012, "SlipHeaderOp/=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::divAsgn (SlipDatum& Y, const DOUBLE X) const

   SlipDatum& SlipHeaderOp::addAsgn (SlipDatum& Y, const ULONG X) const { // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1009, "SlipHeaderOp+=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::addAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipHeaderOp::subAsgn (SlipDatum& Y, const ULONG X) const { // operator-=
      postError(__FILE__, __LINE__, SlipErr::E1010, "SlipHeaderOp-=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::subAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipHeaderOp::mulAsgn (SlipDatum& Y, const ULONG X) const { // operator*=
      postError(__FILE__, __LINE__, SlipErr::E1011, "SlipHeaderOp*=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::mulAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipHeaderOp::divAsgn (SlipDatum& Y, const ULONG X) const { // operator/=
      postError(__FILE__, __LINE__, SlipErr::E1012, "SlipHeaderOp/=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::divAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipHeaderOp::modAsgn (SlipDatum& Y, const ULONG X) const { // operator%=
      postError(__FILE__, __LINE__, SlipErr::E1013, "SlipHeaderOp%=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::modAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipHeaderOp::shlAsgn (SlipDatum& Y, const ULONG X) const { // operator<<=
      postError(__FILE__, __LINE__, SlipErr::E1014, "SlipHeaderOp<<=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::shlAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipHeaderOp::shrAsgn (SlipDatum& Y, const ULONG X) const { // operator>>=
      postError(__FILE__, __LINE__, SlipErr::E1015, "SlipHeaderOp>>=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::shrAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipHeaderOp::bandAsgn(SlipDatum& Y, const ULONG X) const { // operator&=
      postError(__FILE__, __LINE__, SlipErr::E1016, "SlipHeaderOp>>=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::bandAsgn(SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipHeaderOp::borAsgn (SlipDatum& Y, const ULONG X) const { // operator|=
      postError(__FILE__, __LINE__, SlipErr::E1019, "SlipHeaderOp|=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::borAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipHeaderOp::bxorAsgn(SlipDatum& Y, const ULONG X) const { // operator^=
      postError(__FILE__, __LINE__, SlipErr::E1017, "SlipHeaderOp^=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::bxorAsgn(SlipDatum& Y, const ULONG X) const

   SlipDatum& SlipHeaderOp::addAsgn (SlipDatum& Y, const LONG X) const { // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1009, "SlipHeaderOp+=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::addAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipHeaderOp::subAsgn (SlipDatum& Y, const LONG X) const { // operator-=
      postError(__FILE__, __LINE__, SlipErr::E1010, "SlipHeaderOp-=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::subAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipHeaderOp::mulAsgn (SlipDatum& Y, const LONG X) const { // operator*=
      postError(__FILE__, __LINE__, SlipErr::E1011, "SlipHeaderOp*=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::mulAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipHeaderOp::divAsgn (SlipDatum& Y, const LONG X) const { // operator/=
      postError(__FILE__, __LINE__, SlipErr::E1012, "SlipHeaderOp/=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::divAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipHeaderOp::modAsgn (SlipDatum& Y, const LONG X) const { // operator%=
      postError(__FILE__, __LINE__, SlipErr::E1013, "SlipHeaderOp%=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::modAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipHeaderOp::shlAsgn (SlipDatum& Y, const LONG X) const { // operator<<=
      postError(__FILE__, __LINE__, SlipErr::E1014, "SlipHeaderOp<<=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::shlAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipHeaderOp::shrAsgn (SlipDatum& Y, const LONG X) const { // operator>>=
      postError(__FILE__, __LINE__, SlipErr::E1015, "SlipHeaderOp>>=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::shrAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipHeaderOp::bandAsgn(SlipDatum& Y, const LONG X) const { // operator&=
      postError(__FILE__, __LINE__, SlipErr::E1016, "SlipHeaderOp&=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::bandAsgn(SlipDatum& Y, const LONG X) const
   SlipDatum& SlipHeaderOp::borAsgn (SlipDatum& Y, const LONG X) const { // operator|=
      postError(__FILE__, __LINE__, SlipErr::E1019, "SlipHeaderOp|=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::borAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipHeaderOp::bxorAsgn(SlipDatum& Y, const LONG X) const { // operator^=
      postError(__FILE__, __LINE__, SlipErr::E1017, "SlipHeaderOp^=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipHeaderOp::bxorAsgn(SlipDatum& Y, const LONG X) const

          /*************************************************
           *               Logical operators               *
          *************************************************/
   bool SlipHeaderOp::notEqual (const SlipDatum& Y, const SlipDatum& X) const { // operator!=
      bool flag = true;
      if (!X.isNumber()) {
         if (X.isHeader()) {
            flag = (&Y != &X);
         } else if (X.isSublist()) {
            flag = ((void*)&Y != getSublistHeader(X));
         }
      }
      return flag;
   }; // bool SlipHeaderOp::notEqual (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipHeaderOp::lessThan (const SlipDatum& Y, const SlipDatum& X) const { // operator<
      postError(__FILE__, __LINE__, SlipErr::E1029, "SlipHeaderOp<", "", "Operation not supported for header.", X);
      return false;
   }; // bool SlipHeaderOp::lessThan (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipHeaderOp::lessEqual (const SlipDatum& Y, const SlipDatum& X) const { // operator<=
      postError(__FILE__, __LINE__, SlipErr::E1030, "SlipHeaderOp<=", "", "Operation not supported for header.", X);
      return false;
   }; // bool SlipHeaderOp::lessEqual (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipHeaderOp::equal (const SlipDatum& Y, const SlipDatum& X) const { // operator==
      bool flag = false;
      if (!X.isNumber()) {
         if (X.isHeader()) {
            flag = (&Y == &X);
         } else if (X.isSublist()) {
            flag = ((void*)&Y == getSublistHeader(X));
         }
      }
      return flag;
   }; // bool SlipHeaderOp::equal (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipHeaderOp::greatEqual(const SlipDatum& Y, const SlipDatum& X) const { // operator>=
      return false;
   }; // bool SlipHeaderOp::greatEqual(const SlipDatum& Y, const SlipDatum& X) const
   bool SlipHeaderOp::greatThan (const SlipDatum& Y, const SlipDatum& X) const { // operator>
      return false;
   }; // bool SlipHeaderOp::greatThan (const SlipDatum& Y, const SlipDatum& X) const

   bool SlipHeaderOp::notEqual (const SlipDatum& Y, const DOUBLE X) const { // operator!=
      return false;
   }; // bool SlipHeaderOp::notEqual (const SlipDatum& Y, const DOUBLE X) const
   bool SlipHeaderOp::lessThan (const SlipDatum& Y, const DOUBLE X) const { // operator<
      return false;
   }; // bool SlipHeaderOp::lessThan (const SlipDatum& Y, const DOUBLE X) const
   bool SlipHeaderOp::lessEqual (const SlipDatum& Y, const DOUBLE X) const { // operator<=
      return false;
   }; // bool SlipHeaderOp::lessEqual (const SlipDatum& Y, const DOUBLE X) const
   bool SlipHeaderOp::equal (const SlipDatum& Y, const DOUBLE X) const { // operator==
      return false;
   }; // bool SlipHeaderOp::equal (const SlipDatum& Y, const DOUBLE X) const
   bool SlipHeaderOp::greatEqual(const SlipDatum& Y, const DOUBLE X) const { // operator>=
      return false;
   }; // bool SlipHeaderOp::greatEqual(const SlipDatum& Y, const DOUBLE X) const
   bool SlipHeaderOp::greatThan (const SlipDatum& Y, const DOUBLE X) const { // operator>
      return false;
   }; // bool SlipHeaderOp::greatThan (const SlipDatum& Y, const DOUBLE X) const

   bool SlipHeaderOp::notEqual (const SlipDatum& Y, const ULONG X) const { // operator!=
      return false;
   }; // bool SlipHeaderOp::notEqual (const SlipDatum& Y, const ULONG X) const
   bool SlipHeaderOp::lessThan (const SlipDatum& Y, const ULONG X) const { // operator<
      return false;
   }; // bool SlipHeaderOp::lessThan (const SlipDatum& Y, const ULONG X) const
   bool SlipHeaderOp::lessEqual (const SlipDatum& Y, const ULONG X) const { // operator<=
      return false;
   }; // bool SlipHeaderOp::lessEqual (const SlipDatum& Y, const ULONG X) const
   bool SlipHeaderOp::equal (const SlipDatum& Y, const ULONG X) const { // operator==
      return false;
   }; // bool SlipHeaderOp::equal (const SlipDatum& Y, const ULONG X) const
   bool SlipHeaderOp::greatEqual(const SlipDatum& Y, const ULONG X) const { // operator>=
      return false;
   }; // bool SlipHeaderOp::greatEqual(const SlipDatum& Y, const ULONG X) const
   bool SlipHeaderOp::greatThan (const SlipDatum& Y, const ULONG X) const { // operator>
      return false;
   }; // bool SlipHeaderOp::greatThan (const SlipDatum& Y, const ULONG X) const

   bool SlipHeaderOp::notEqual (const SlipDatum& Y, const LONG X) const { // operator!=
      return false;
   }; // bool SlipHeaderOp::notEqual (const SlipDatum& Y, const LONG X) const
   bool SlipHeaderOp::lessThan (const SlipDatum& Y, const LONG X) const { // operator<
      return false;
   }; // bool SlipHeaderOp::lessThan (const SlipDatum& Y, const LONG X) const
   bool SlipHeaderOp::lessEqual (const SlipDatum& Y, const LONG X) const { // operator<=
      return false;
   }; // bool SlipHeaderOp::lessEqual (const SlipDatum& Y, const LONG X) const
   bool SlipHeaderOp::equal (const SlipDatum& Y, const LONG X) const { // operator==
      return false;
   }; // bool SlipHeaderOp::equal (const SlipDatum& Y, const LONG X) const
   bool SlipHeaderOp::greatEqual(const SlipDatum& Y, const LONG X) const { // operator>=
      return false;
   }; // bool SlipHeaderOp::greatEqual(const SlipDatum& Y, const LONG X) const
   bool SlipHeaderOp::greatThan (const SlipDatum& Y, const LONG X) const { // operator>
      return false;
   }; // bool SlipHeaderOp::greatThan (const SlipDatum& Y, const LONG X) const

   bool SlipHeaderOp::notEqual (const SlipDatum& Y, const PTR X) const { // operator!=
       return false;
   }; // bool SlipHeaderOp::notEqual (const SlipDatum& Y, const PTR X) const
   bool SlipHeaderOp::equal (const SlipDatum& Y, const PTR X) const { // operator==
       return false;
   }; // bool SlipHeaderOp::equal (const SlipDatum& Y, const PTR X) const

   bool SlipHeaderOp::notEqual (const SlipDatum& Y, const string& X) const { // operator!=
       return false;
   }; // bool SlipHeaderOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipHeaderOp::lessThan (const SlipDatum& Y, const string& X) const { // operator<
       return false;
   }; // bool SlipHeaderOp::equal (const SlipDatum& Y, const string& X) const
   bool SlipHeaderOp::lessEqual (const SlipDatum& Y, const string& X) const { // operator<=
       return false;
   }; // bool SlipHeaderOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipHeaderOp::equal (const SlipDatum& Y, const string& X) const { // operator==
       return false;
   }; // bool SlipHeaderOp::equal (const SlipDatum& Y, const string& X) const
   bool SlipHeaderOp::greatEqual (const SlipDatum& Y, const string& X) const { // operator>=
       return false;
   }; // bool SlipHeaderOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipHeaderOp::greatThan (const SlipDatum& Y, const string& X) const { // operator>
       return false;
   }; // bool SlipHeaderOp::equal (const SlipDatum& Y, const string& X) const


   string SlipHeaderOp::toString(const SlipCell& X) const  { // pretty print SLIP cell
      return dump(X);
   }; // string SlipHeaderOp::toString(const SlipCell& X) const

   string SlipHeaderOp::dump(const SlipCell& X) const  {
      stringstream pretty;
      sCell*      cell  = (sCell*)&X;
      pretty << "[header   ] "
             << dumpLink(X)
             << " RefCnt: "    << cell->datum.Head.refCnt
             << " mrk: "       << cell->datum.Head.mrk
             << " Descriptor ";
      if (cell->datum.Head.descriptionList == 0) pretty << "NULL";
      else pretty << hex << setfill('0') << setw(8)<< cell->datum.Head.descriptionList;
      return pretty.str();
   }; // string SlipHeaderOp::dump(const SlipDatum& X) const

   string SlipHeaderOp::write(const SlipCell& X) const {
      return "";
   }; // string SlipHeaderOp::write(const SlipCell& X) const

}; // namespace slip