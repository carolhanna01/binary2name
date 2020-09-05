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
 * @file    SlipReaderOp.cpp
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
 # include "SlipReaderCell.h"
 # include "SlipReaderOp.h"

using namespace slip;

namespace slip {

   /*********************************************************
    *                  Casting Operators                    *
   *********************************************************/

   bool SlipReaderOp::cast_bool(const SlipDatum& X) const {                     // (bool) X
      postError(__FILE__, __LINE__, SlipErr::E1035, "SlipReaderOp(bool)", "", "Operation not supported for reader.", X);
      return false;
   }; // bool SlipReaderOp::cast_bool(const SlipDatum& X) const
   UCHAR SlipReaderOp::cast_UCHAR(const SlipDatum& X) const {                   // (UCHAR) X
      postError(__FILE__, __LINE__, SlipErr::E1036, "SlipReaderOp(UCHAR)", "", "Operation not supported for reader.", X);
      return 0;
   }; // UCHAR SlipReaderOp::cast_UCHAR(const SlipDatum& X) const
   CHAR SlipReaderOp::cast_CHAR(const SlipDatum& X) const {                     // (CHAR) X
      postError(__FILE__, __LINE__, SlipErr::E1037, "SlipReaderOp(CHAR)", "", "Operation not supported for reader.", X);
      return 0;
   }; // CHAR SlipReaderOp::cast_CHAR(const SlipDatum& X) const
   ULONG SlipReaderOp::cast_ULONG(const SlipDatum& X) const {                   // (ULONG) X
      postError(__FILE__, __LINE__, SlipErr::E1038, "SlipReaderOp(ULONG)", "", "Operation not supported for reader.", X);
      return 0;
   }; // ULONG SlipReaderOp::cast_ULONG(const SlipDatum& X) const
   LONG SlipReaderOp::cast_LONG(const SlipDatum& X) const {                     // (LONG) X
      postError(__FILE__, __LINE__, SlipErr::E1039, "SlipReaderOp(LONG)", "", "Operation not supported for reader.", X);
      return 0;
   }; // LONG SlipReaderOp::cast_LONG(const SlipDatum& X) const
   DOUBLE SlipReaderOp::cast_DOUBLE(const SlipDatum& X) const {                 // (DOUBLE) X
      postError(__FILE__, __LINE__, SlipErr::E1040, "SlipReaderOp(DOUBLE)", "", "Operation not supported for reader.", X);
      return 0.0;
   }; // DOUBLE SlipReaderOp::cast_DOUBLE(const SlipDatum& X) const
   PTR SlipReaderOp::cast_PTR(const SlipDatum& Y) const {                       // (PTR) Y
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipBoolOp++", "", "Operation not supported for READER.", Y);
      return NULL;
   }; // PTR SlipReaderOp::cast_PTR(const SlipDatum& Y) const

      /*********************************************************
       *                 Operator Overloading                  *
      *********************************************************/

          /*************************************************
           *                Unary operators                *
          *************************************************/
   ULONG SlipReaderOp::bitNOT (const SlipDatum& X) const { // operator~
      postError(__FILE__, __LINE__, SlipErr::E1043, "SlipReaderOpoperato~r", "", "Operation not supported for reader.", X);
      return 0;
   }; // ULONG SlipReaderOp::bitNOT (const SlipDatum& X) const
   bool SlipReaderOp::logNOT (const SlipDatum& X) const { // operator!
      postError(__FILE__, __LINE__, SlipErr::E1025, "SlipReaderOpoperator!", "", "Operation not supported for reader.", X);
      return false;
   }; // bool SlipReaderOp::logNOT (const SlipDatum& X) const
   SlipDatum SlipReaderOp::plus (const SlipDatum& X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1001, "SlipReaderOp+", "", "Operation not supported for reader.", X);
      return X;
   }; // SlipDatum SlipReaderOp::plus (const SlipDatum& X) const
   SlipDatum SlipReaderOp::minus (const SlipDatum& X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1002, "SlipReaderOp-", "", "Operation not supported for reader.", X);
      return X;
   }; // SlipDatum SlipReaderOp::minus (const SlipDatum& X) const
   SlipDatum& SlipReaderOp::postInc (SlipDatum& X) const { // operator++
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipReaderOp(int)++", "", "Operation not supported for reader.", X);
      return X;
   }; // SlipDatum& SlipReaderOp::postInc (SlipDatum& X) const
   SlipDatum& SlipReaderOp::postDec (SlipDatum& X) const { // operator--
      postError(__FILE__, __LINE__, SlipErr::E1004, "SlipReaderOp(int)--", "", "Operation not supported for reader.", X);
      return X;
   }; // SlipDatum& SlipReaderOp::postDec (SlipDatum& X) const
   SlipDatum& SlipReaderOp::preInc (SlipDatum& X) const { // ++operator
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipReaderOp++", "", "Operation not supported for reader.", X);
      return X;
   }; // SlipDatum& SlipReaderOp::preInc (SlipDatum& X) const
   SlipDatum& SlipReaderOp::preDec (SlipDatum& X) const { // --operator
      postError(__FILE__, __LINE__, SlipErr::E1004, "SlipReaderOp--", "", "Operation not supported for reader.", X);
      return X;
   }; // SlipDatum& SlipReaderOp::preDec (SlipDatum& X) const

          /*************************************************
           *                Binary operators               *
          *************************************************/
   SlipDatum SlipReaderOp::add(const SlipDatum& Y, const SlipDatum& X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1005, "SlipReaderOp+", "", "Operation not supported for reader.", Y, X);
      return Y;
   }; // SlipDatum SlipReaderOp::add(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipReaderOp::sub(const SlipDatum& Y, const SlipDatum& X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1006, "SlipReaderOp-", "", "Operation not supported for reader.", Y, X);
      return Y;
   }; // SlipDatum SlipReaderOp::sub(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipReaderOp::mul(const SlipDatum& Y, const SlipDatum& X) const { // operator*
      postError(__FILE__, __LINE__, SlipErr::E1007, "SlipReaderOp*", "", "Operation not supported for reader.", Y, X);
      return Y;
   }; // SlipDatum SlipReaderOp::mul(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipReaderOp::div(const SlipDatum& Y, const SlipDatum& X) const { // operator/
      postError(__FILE__, __LINE__, SlipErr::E1008, "SlipReaderOp/", "", "Operation not supported for reader.", Y, X);
      return Y;
   }; // SlipDatum SlipReaderOp::div(const SlipDatum& Y, const SlipDatum& X) const
   LONG SlipReaderOp::mod(const SlipDatum& Y, const SlipDatum& X) const { // operator%
      postError(__FILE__, __LINE__, SlipErr::E1026, "SlipReaderOp%", "", "Operation not supported for reader.", Y, X);
      return 0;
   }; // SlipDatum SlipReaderOp::mod(const SlipDatum& Y, const SlipDatum& X) const

   SlipDatum SlipReaderOp::add(const SlipDatum& Y, const DOUBLE X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1005, "SlipReaderOp+", "", "Operation not supported for reader.");
      return Y;
   }; // SlipDatum SlipReaderOp::add(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipReaderOp::sub(const SlipDatum& Y, const DOUBLE X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1006, "SlipReaderOp-", "", "Operation not supported for reader.");
      return Y;
   }; // SlipDatum SlipReaderOp::sub(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipReaderOp::mul(const SlipDatum& Y, const DOUBLE X) const { // operator*
      postError(__FILE__, __LINE__, SlipErr::E1007, "SlipReaderOp*", "", "Operation not supported for reader.");
      return Y;
   }; // SlipDatum SlipReaderOp::mul(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipReaderOp::div(const SlipDatum& Y, const DOUBLE X) const { // operator/
      postError(__FILE__, __LINE__, SlipErr::E1008, "SlipReaderOp/", "", "Operation not supported for reader.");
      return Y;
   }; // SlipDatum SlipReaderOp::div(const SlipDatum& Y, const DOUBLE X) const

   SlipDatum SlipReaderOp::add(const SlipDatum& Y, const ULONG X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1005, "SlipReaderOp+", "", "Operation not supported for reader.");
      return Y;
   }; // SlipDatum SlipReaderOp::add(const SlipDatum& Y, const ULONG X) const
   SlipDatum SlipReaderOp::sub(const SlipDatum& Y, const ULONG X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1006, "SlipReaderOp-", "", "Operation not supported for reader.");
      return Y;
   }; // SlipDatum SlipReaderOp::sub(const SlipDatum& Y, const ULONG X) const
   SlipDatum SlipReaderOp::mul(const SlipDatum& Y, const ULONG X) const { // operator*
      postError(__FILE__, __LINE__, SlipErr::E1007, "SlipReaderOp*", "", "Operation not supported for reader.");
      return Y;
   }; // SlipDatum SlipReaderOp::mul(const SlipDatum& Y, const ULONG X) const
   SlipDatum SlipReaderOp::div(const SlipDatum& Y, const ULONG X) const { // operator/
      postError(__FILE__, __LINE__, SlipErr::E1008, "SlipReaderOp/", "", "Operation not supported for reader.");
      return Y;
   }; // SlipDatum SlipReaderOp::div(const SlipDatum& Y, const ULONG X) const
   LONG       SlipReaderOp::mod(const SlipDatum& Y, const ULONG X) const { // operator%
      postError(__FILE__, __LINE__, SlipErr::E1026, "%", "", "Operation not supported for reader.", Y);
      return 0;
   }; // SlipDatum SlipReaderOp::mod(const SlipDatum& Y, const ULONG X) const

   SlipDatum SlipReaderOp::add(const SlipDatum& Y, const LONG X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1005, "SlipReaderOp+", "", "Operation not supported for reader.");
      return Y;
   }; // SlipDatum SlipReaderOp::add(const SlipDatum& Y, const LONG X) const

   SlipDatum SlipReaderOp::sub(const SlipDatum& Y, const LONG X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1006, "SlipReaderOp-", "", "Operation not supported for reader.");
      return Y;
   }; // SlipDatum SlipReaderOp::sub(const SlipDatum& Y, const LONG X) const
   SlipDatum SlipReaderOp::mul(const SlipDatum& Y, const LONG X) const { // operator*
      postError(__FILE__, __LINE__, SlipErr::E1007, "SlipReaderOp*", "", "Operation not supported for reader.");
      return Y;
   }; // SlipDatum SlipReaderOp::mul(const SlipDatum& Y, const LONG X) const
   SlipDatum SlipReaderOp::div(const SlipDatum& Y, const LONG X) const { // operator/
      postError(__FILE__, __LINE__, SlipErr::E1008, "SlipReaderOp/", "", "Operation not supported for reader.");
      return Y;
   }; // SlipDatum SlipReaderOp::div(const SlipDatum& Y, const LONG X) const
   LONG       SlipReaderOp::mod(const SlipDatum& Y, const LONG X) const { // operator%
      postError(__FILE__, __LINE__, SlipErr::E1026, "SlipReaderOp%", "", "Operation not supported for reader.", Y);
      return 0;
   }; // SlipDatum SlipReaderOp::mod(const SlipDatum& Y, const LONG X) const

          /*************************************************
           *                Bit operators                  *
          *************************************************/
   LONG  SlipReaderOp::shl (const SlipDatum& Y, const SlipDatum& X) const { // operator<<
      postError(__FILE__, __LINE__, SlipErr::E1020, "SlipReaderOp<<", "", "Operation not supported for reader.", Y, X);
      return 0;
   }; // LONG  SlipReaderOp::shl (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipReaderOp::shr (const SlipDatum& Y, const SlipDatum& X) const { // operator>>
      postError(__FILE__, __LINE__, SlipErr::E1021, "SlipReaderOp>>", "", "Operation not supported for reader.", Y, X);
      return 0;
   }; // LONG  SlipReaderOp::shr (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipReaderOp::band(const SlipDatum& Y, const SlipDatum& X) const { // operator&
      postError(__FILE__, __LINE__, SlipErr::E1024, "SlipReaderOp&", "", "Operation not supported for reader.", Y, X);
      return 0;
   }; // LONG  SlipReaderOp::band(const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipReaderOp::bor (const SlipDatum& Y, const SlipDatum& X) const { // operator|
      postError(__FILE__, __LINE__, SlipErr::E1023, "SlipReaderOp|", "", "Operation not supported for reader.", Y, X);
      return 0;
   }; // LONG  SlipReaderOp::bor (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipReaderOp::bxor(const SlipDatum& Y, const SlipDatum& X) const { // operator^
      postError(__FILE__, __LINE__, SlipErr::E1022, "SlipReaderOp^", "", "Operation not supported for reader.", Y, X);
      return 0;
   }; // LONG  SlipReaderOp::bxor(const SlipDatum& Y, const SlipDatum& X) const

   LONG  SlipReaderOp::shl (const SlipDatum& Y, const ULONG X) const { // operator<<
      postError(__FILE__, __LINE__, SlipErr::E1020, "", "Operation not supported for reader.", "", Y);
      return 0;
   }; // LONG  SlipReaderOp::shl (const SlipDatum& Y, const ULONG X) const
   LONG  SlipReaderOp::shr (const SlipDatum& Y, const ULONG X) const { // operator>>
      postError(__FILE__, __LINE__, SlipErr::E1021, "", "Operation not supported for reader.", "", Y);
      return 0;
   }; // LONG  SlipReaderOp::shr (const SlipDatum& Y, const ULONG X) const
   LONG  SlipReaderOp::band(const SlipDatum& Y, const ULONG X) const { // operator&
      postError(__FILE__, __LINE__, SlipErr::E1024, "", "Operation not supported for reader.", "", Y);
      return 0;
   }; // LONG  SlipReaderOp::band(const SlipDatum& Y, const ULONG X) const
   LONG  SlipReaderOp::bor (const SlipDatum& Y, const ULONG X) const { // operator|
      postError(__FILE__, __LINE__, SlipErr::E1023, "", "Operation not supported for reader.", "", Y);
      return 0;
   }; // LONG  SlipReaderOp::bor (const SlipDatum& Y, const ULONG X) const
   LONG  SlipReaderOp::bxor(const SlipDatum& Y, const ULONG X) const { // operator^
      postError(__FILE__, __LINE__, SlipErr::E1022, "SlipReaderOp^", "", "Operation not supported for reader.", Y);
      return 0;
   }; // LONG  SlipReaderOp::bxor(const SlipDatum& Y, const ULONG X) const

   LONG  SlipReaderOp::shl (const SlipDatum& Y, const LONG X) const { // operator<<
      postError(__FILE__, __LINE__, SlipErr::E1020, "SlipReaderOp<<", "", "Operation not supported for reader.", Y);
      return 0;
   }; // LONG  SlipReaderOp::shl (const SlipDatum& Y, const LONG X) const
   LONG  SlipReaderOp::shr (const SlipDatum& Y, const LONG X) const { // operator>>
      postError(__FILE__, __LINE__, SlipErr::E1021, "SlipReaderOp>>", "", "Operation not supported for reader.", Y);
      return 0;
   }; // LONG  SlipReaderOp::shr (const SlipDatum& Y, const LONG X) const
   LONG  SlipReaderOp::band(const SlipDatum& Y, const LONG X) const { // operator&
      postError(__FILE__, __LINE__, SlipErr::E1024, "SlipReaderOp&", "", "Operation not supported for reader.", Y);
      return 0;
   }; // LONG  SlipReaderOp::band(const SlipDatum& Y, const LONG X) const
   LONG  SlipReaderOp::bor (const SlipDatum& Y, const LONG X) const { // operator|
      postError(__FILE__, __LINE__, SlipErr::E1023, "SlipReaderOp|", "", "Operation not supported for reader.", Y);
      return 0;
   }; // LONG  SlipReaderOp::bor (const SlipDatum& Y, const LONG X) const
   LONG  SlipReaderOp::bxor(const SlipDatum& Y, const LONG X) const { // operator^
      postError(__FILE__, __LINE__, SlipErr::E1022, "SlipReaderOp^", "", "Operation not supported for reader.", Y);
      return 0;
   }; // LONG  SlipReaderOp::bxor(const SlipDatum& Y, const LONG X) const

          /*************************************************
           *               Simple Assignment               *
          *************************************************/
   SlipDatum& SlipReaderOp::asgn(SlipDatum& Y, const SlipDatum& X) const {        // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1023, "SlipReaderOp|", "", "Operation not supported for reader.", Y);
      return Y;
   }; // SlipDatum& SlipReaderOp::asgn(SlipDatum& Y, const SlipDatum& X) const

          /*************************************************
           *       Arithmetic Assignment operators         *
          *************************************************/
   SlipDatum& SlipReaderOp::addAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1009, "SlipReaderOp+=", "", "Operation not supported for reader.", X);
      return Y;
   }; // SlipDatum& SlipReaderOp::addAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipReaderOp::subAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator-=
      postError(__FILE__, __LINE__, SlipErr::E1010, "SlipReaderOp-=", "", "Operation not supported for reader.", X);
      return Y;
   }; // SlipDatum& SlipReaderOp::subAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipReaderOp::mulAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator*=
      postError(__FILE__, __LINE__, SlipErr::E1011, "SlipReaderOp*=", "", "Operation not supported for reader.", X);
      return Y;
   }; // SlipDatum& SlipReaderOp::mulAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipReaderOp::divAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator/=
      postError(__FILE__, __LINE__, SlipErr::E1012, "SlipReaderOp/=", "", "Operation not supported for reader.", X);
      return Y;
   }; // SlipDatum& SlipReaderOp::divAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipReaderOp::modAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator%=
      postError(__FILE__, __LINE__, SlipErr::E1013, "SlipReaderOp%=", "", "Operation not supported for reader.", Y, X);
      return Y;
   }; // SlipDatum& SlipReaderOp::modAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipReaderOp::shlAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator<<=
      postError(__FILE__, __LINE__, SlipErr::E1014, "SlipReaderOp<<=", "", "Operation not supported for reader.", Y, X);
      return Y;
   }; // SlipDatum& SlipReaderOp::shlAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipReaderOp::shrAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator>>=
      postError(__FILE__, __LINE__, SlipErr::E1015, "SlipReaderOp>>=", "", "Operation not supported for reader.", Y, X);
      return Y;
   }; // SlipDatum& SlipReaderOp::shrAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipReaderOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator&=
      postError(__FILE__, __LINE__, SlipErr::E1016, "SlipReaderOp&=", "", "Operation not supported for reader.", Y, X);
      return Y;
   }; // SlipDatum& SlipReaderOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipReaderOp::borAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator|=
      postError(__FILE__, __LINE__, SlipErr::E1019, "SlipReaderOp|=", "", "Operation not supported for reader.", Y, X);
      return Y;
   }; // SlipDatum& SlipReaderOp::borAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipReaderOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator^=
      postError(__FILE__, __LINE__, SlipErr::E1017, "SlipReaderOp^=", "", "Operation not supported for reader.", Y, X);
      return Y;
   }; // SlipDatum& SlipReaderOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const

   SlipDatum& SlipReaderOp::addAsgn (SlipDatum& Y, const DOUBLE X) const { // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1009, "SlipReaderOp+=", "", "Operation not supported for reader.");
      return Y;
   }; // SlipDatum& SlipReaderOp::addAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipReaderOp::subAsgn (SlipDatum& Y, const DOUBLE X) const { // operator-=
      postError(__FILE__, __LINE__, SlipErr::E1010, "SlipReaderOp-=", "", "Operation not supported for reader.");
      return Y;
   }; // SlipDatum& SlipReaderOp::subAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipReaderOp::mulAsgn (SlipDatum& Y, const DOUBLE X) const { // operator*=
      postError(__FILE__, __LINE__, SlipErr::E1011, "SlipReaderOp*=", "", "Operation not supported for reader.");
      return Y;
   }; // SlipDatum& SlipReaderOp::mulAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipReaderOp::divAsgn (SlipDatum& Y, const DOUBLE X) const { // operator/=
      postError(__FILE__, __LINE__, SlipErr::E1012, "SlipReaderOp/=", "", "Operation not supported for reader.");
      return Y;
   }; // SlipDatum& SlipReaderOp::divAsgn (SlipDatum& Y, const DOUBLE X) const

   SlipDatum& SlipReaderOp::addAsgn (SlipDatum& Y, const ULONG X) const { // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1009, "SlipReaderOp+=", "", "Operation not supported for reader.");
      return Y;
   }; // SlipDatum& SlipReaderOp::addAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipReaderOp::subAsgn (SlipDatum& Y, const ULONG X) const { // operator-=
      postError(__FILE__, __LINE__, SlipErr::E1010, "SlipReaderOp-=", "", "Operation not supported for reader.");
      return Y;
   }; // SlipDatum& SlipReaderOp::subAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipReaderOp::mulAsgn (SlipDatum& Y, const ULONG X) const { // operator*=
      postError(__FILE__, __LINE__, SlipErr::E1011, "SlipReaderOp*=", "", "Operation not supported for reader.");
      return Y;
   }; // SlipDatum& SlipReaderOp::mulAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipReaderOp::divAsgn (SlipDatum& Y, const ULONG X) const { // operator/=
      postError(__FILE__, __LINE__, SlipErr::E1012, "SlipReaderOp/=", "", "Operation not supported for reader.");
      return Y;
   }; // SlipDatum& SlipReaderOp::divAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipReaderOp::modAsgn (SlipDatum& Y, const ULONG X) const { // operator%=
      postError(__FILE__, __LINE__, SlipErr::E1013, "SlipReaderOp%=", "", "Operation not supported for reader.", Y);
      return Y;
   }; // SlipDatum& SlipReaderOp::modAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipReaderOp::shlAsgn (SlipDatum& Y, const ULONG X) const { // operator<<=
      postError(__FILE__, __LINE__, SlipErr::E1014, "SlipReaderOp<<=", "", "Operation not supported for reader.", Y);
      return Y;
   }; // SlipDatum& SlipReaderOp::shlAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipReaderOp::shrAsgn (SlipDatum& Y, const ULONG X) const { // operator>>=
      postError(__FILE__, __LINE__, SlipErr::E1015, "SlipReaderOp>>=", "", "Operation not supported for reader.", Y);
      return Y;
   }; // SlipDatum& SlipReaderOp::shrAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipReaderOp::bandAsgn(SlipDatum& Y, const ULONG X) const { // operator&=
      postError(__FILE__, __LINE__, SlipErr::E1016, "SlipReaderOp>>=", "", "Operation not supported for reader.", Y);
      return Y;
   }; // SlipDatum& SlipReaderOp::bandAsgn(SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipReaderOp::borAsgn (SlipDatum& Y, const ULONG X) const { // operator|=
      postError(__FILE__, __LINE__, SlipErr::E1019, "SlipReaderOp|=", "", "Operation not supported for reader.", Y);
      return Y;
   }; // SlipDatum& SlipReaderOp::borAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipReaderOp::bxorAsgn(SlipDatum& Y, const ULONG X) const { // operator^=
      postError(__FILE__, __LINE__, SlipErr::E1017, "SlipReaderOp^=", "", "Operation not supported for reader.", Y);
      return Y;
   }; // SlipDatum& SlipReaderOp::bxorAsgn(SlipDatum& Y, const ULONG X) const

   SlipDatum& SlipReaderOp::addAsgn (SlipDatum& Y, const LONG X) const { // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1009, "SlipReaderOp+=", "", "Operation not supported for reader.");
      return Y;
   }; // SlipDatum& SlipReaderOp::addAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipReaderOp::subAsgn (SlipDatum& Y, const LONG X) const { // operator-=
      postError(__FILE__, __LINE__, SlipErr::E1010, "SlipReaderOp-=", "", "Operation not supported for reader.");
      return Y;
   }; // SlipDatum& SlipReaderOp::subAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipReaderOp::mulAsgn (SlipDatum& Y, const LONG X) const { // operator*=
      postError(__FILE__, __LINE__, SlipErr::E1011, "SlipReaderOp*=", "", "Operation not supported for reader.");
      return Y;
   }; // SlipDatum& SlipReaderOp::mulAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipReaderOp::divAsgn (SlipDatum& Y, const LONG X) const { // operator/=
      postError(__FILE__, __LINE__, SlipErr::E1012, "SlipReaderOp/=", "", "Operation not supported for reader.");
      return Y;
   }; // SlipDatum& SlipReaderOp::divAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipReaderOp::modAsgn (SlipDatum& Y, const LONG X) const { // operator%=
      postError(__FILE__, __LINE__, SlipErr::E1013, "SlipReaderOp%=", "", "Operation not supported for reader.", Y);
      return Y;
   }; // SlipDatum& SlipReaderOp::modAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipReaderOp::shlAsgn (SlipDatum& Y, const LONG X) const { // operator<<=
      postError(__FILE__, __LINE__, SlipErr::E1014, "SlipReaderOp<<=", "", "Operation not supported for reader.", Y);
      return Y;
   }; // SlipDatum& SlipReaderOp::shlAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipReaderOp::shrAsgn (SlipDatum& Y, const LONG X) const { // operator>>=
      postError(__FILE__, __LINE__, SlipErr::E1015, "SlipReaderOp>>=", "", "Operation not supported for reader.", Y);
      return Y;
   }; // SlipDatum& SlipReaderOp::shrAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipReaderOp::bandAsgn(SlipDatum& Y, const LONG X) const { // operator&=
      postError(__FILE__, __LINE__, SlipErr::E1016, "SlipReaderOp&=", "", "Operation not supported for reader.", Y);
      return Y;
   }; // SlipDatum& SlipReaderOp::bandAsgn(SlipDatum& Y, const LONG X) const
   SlipDatum& SlipReaderOp::borAsgn (SlipDatum& Y, const LONG X) const { // operator|=
      postError(__FILE__, __LINE__, SlipErr::E1019, "SlipReaderOp|=", "", "Operation not supported for reader.", Y);
      return Y;
   }; // SlipDatum& SlipReaderOp::borAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipReaderOp::bxorAsgn(SlipDatum& Y, const LONG X) const { // operator^=
      postError(__FILE__, __LINE__, SlipErr::E1017, "SlipReaderOp^=", "", "Operation not supported for reader.", Y);
      return Y;
   }; // SlipDatum& SlipReaderOp::bxorAsgn(SlipDatum& Y, const LONG X) const

          /*************************************************
           *               Logical operators               *
          *************************************************/
   bool SlipReaderOp::notEqual (const SlipDatum& Y, const SlipDatum& X) const { // operator!=
      return false;
   }; // bool SlipReaderOp::notEqual (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipReaderOp::lessThan (const SlipDatum& Y, const SlipDatum& X) const { // operator<
      return false;
   }; // bool SlipReaderOp::lessThan (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipReaderOp::lessEqual (const SlipDatum& Y, const SlipDatum& X) const { // operator<=
      return false;
   }; // bool SlipReaderOp::lessEqual (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipReaderOp::equal (const SlipDatum& Y, const SlipDatum& X) const { // operator==
      return false;
   }; // bool SlipReaderOp::equal (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipReaderOp::greatEqual(const SlipDatum& Y, const SlipDatum& X) const { // operator>=
      return false;
   }; // bool SlipReaderOp::greatEqual(const SlipDatum& Y, const SlipDatum& X) const
   bool SlipReaderOp::greatThan (const SlipDatum& Y, const SlipDatum& X) const { // operator>
      return false;
   }; // bool SlipReaderOp::greatThan (const SlipDatum& Y, const SlipDatum& X) const

   bool SlipReaderOp::notEqual (const SlipDatum& Y, const DOUBLE X) const { // operator!=
      return false;
   }; // bool SlipReaderOp::notEqual (const SlipDatum& Y, const DOUBLE X) const
   bool SlipReaderOp::lessThan (const SlipDatum& Y, const DOUBLE X) const { // operator<
      return false;
   }; // bool SlipReaderOp::lessThan (const SlipDatum& Y, const DOUBLE X) const
   bool SlipReaderOp::lessEqual (const SlipDatum& Y, const DOUBLE X) const { // operator<=
      return false;
   }; // bool SlipReaderOp::lessEqual (const SlipDatum& Y, const DOUBLE X) const
   bool SlipReaderOp::equal (const SlipDatum& Y, const DOUBLE X) const { // operator==
      return false;
   }; // bool SlipReaderOp::equal (const SlipDatum& Y, const DOUBLE X) const
   bool SlipReaderOp::greatEqual(const SlipDatum& Y, const DOUBLE X) const { // operator>=
      return false;
   }; // bool SlipReaderOp::greatEqual(const SlipDatum& Y, const DOUBLE X) const
   bool SlipReaderOp::greatThan (const SlipDatum& Y, const DOUBLE X) const { // operator>
      return false;
   }; // bool SlipReaderOp::greatThan (const SlipDatum& Y, const DOUBLE X) const

   bool SlipReaderOp::notEqual (const SlipDatum& Y, const ULONG X) const { // operator!=
      return false;
   }; // bool SlipReaderOp::notEqual (const SlipDatum& Y, const ULONG X) const
   bool SlipReaderOp::lessThan (const SlipDatum& Y, const ULONG X) const { // operator<
      return false;
   }; // bool SlipReaderOp::lessThan (const SlipDatum& Y, const ULONG X) const
   bool SlipReaderOp::lessEqual (const SlipDatum& Y, const ULONG X) const { // operator<=
      return false;
   }; // bool SlipReaderOp::lessEqual (const SlipDatum& Y, const ULONG X) const
   bool SlipReaderOp::equal (const SlipDatum& Y, const ULONG X) const { // operator==
      return false;
   }; // bool SlipReaderOp::equal (const SlipDatum& Y, const ULONG X) const
   bool SlipReaderOp::greatEqual(const SlipDatum& Y, const ULONG X) const { // operator>=
      return false;
   }; // bool SlipReaderOp::greatEqual(const SlipDatum& Y, const ULONG X) const
   bool SlipReaderOp::greatThan (const SlipDatum& Y, const ULONG X) const { // operator>
      return false;
   }; // bool SlipReaderOp::greatThan (const SlipDatum& Y, const ULONG X) const

   bool SlipReaderOp::notEqual (const SlipDatum& Y, const LONG X) const { // operator!=
      return false;
   }; // bool SlipReaderOp::notEqual (const SlipDatum& Y, const LONG X) const
   bool SlipReaderOp::lessThan (const SlipDatum& Y, const LONG X) const { // operator<
      return false;
   }; // bool SlipReaderOp::lessThan (const SlipDatum& Y, const LONG X) const
   bool SlipReaderOp::lessEqual (const SlipDatum& Y, const LONG X) const { // operator<=
      return false;
   }; // bool SlipReaderOp::lessEqual (const SlipDatum& Y, const LONG X) const
   bool SlipReaderOp::equal (const SlipDatum& Y, const LONG X) const { // operator==
      return false;
   }; // bool SlipReaderOp::equal (const SlipDatum& Y, const LONG X) const
   bool SlipReaderOp::greatEqual(const SlipDatum& Y, const LONG X) const { // operator>=
      return false;
   }; // bool SlipReaderOp::greatEqual(const SlipDatum& Y, const LONG X) const
   bool SlipReaderOp::greatThan (const SlipDatum& Y, const LONG X) const { // operator>
      return false;
   }; // bool SlipReaderOp::greatThan (const SlipDatum& Y, const LONG X) const

   bool SlipReaderOp::notEqual (const SlipDatum& Y, const PTR X) const { // operator!=
       return false;
   }; // bool SlipReaderOp::notEqual (const SlipDatum& Y, const PTR X) const
   bool SlipReaderOp::equal (const SlipDatum& Y, const PTR X) const { // operator==
       return false;
   }; // bool SlipReaderOp::equal (const SlipDatum& Y, const PTR X) const

   bool SlipReaderOp::notEqual (const SlipDatum& Y, const string& X) const { // operator!=
       return false;
   }; // bool SlipReaderOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipReaderOp::lessThan (const SlipDatum& Y, const string& X) const { // operator<
       return false;
   }; // bool SlipReaderOp::equal (const SlipDatum& Y, const string& X) const
   bool SlipReaderOp::lessEqual (const SlipDatum& Y, const string& X) const { // operator<=
       return false;
   }; // bool SlipReaderOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipReaderOp::equal (const SlipDatum& Y, const string& X) const { // operator==
       return false;
   }; // bool SlipReaderOp::equal (const SlipDatum& Y, const string& X) const
   bool SlipReaderOp::greatEqual (const SlipDatum& Y, const string& X) const { // operator>=
       return false;
   }; // bool SlipReaderOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipReaderOp::greatThan (const SlipDatum& Y, const string& X) const { // operator>
       return false;
   }; // bool SlipReaderOp::equal (const SlipDatum& Y, const string& X) const

   string SlipReaderOp::toString(const SlipCell& Y) const { // pretty print SLIP cell
      stringstream pretty;
      return dump(Y);
   }; // string SlipReaderOp::toString(const SlipCell& Y) const

   string SlipReaderOp::dump(const SlipCell& Y) const  {
      stringstream pretty;
      SlipReaderCell* reader = &(SlipReaderCell&)Y;
      pretty << "[reader   ] " << dumpLink(Y)
             << " Depth "     << reader->listDepth()
             << " List "      << hex << setfill('0') << setw(8) << &reader->currentList()
             << " Current "   << hex << setfill('0') << setw(8) << &reader->currentCell();
      return pretty.str();
   }; // string SlipReaderOp::dump(const SlipCell& Y) const

   string SlipReaderOp::write(const SlipCell& X) const {
      return "";
   }; // string SlipReaderOp::write(const SlipCell& X) const

}; // namespace slip