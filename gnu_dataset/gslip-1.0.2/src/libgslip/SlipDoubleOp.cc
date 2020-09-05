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
 * @file    SlipDoubleOp.cpp
 * @author  A. Schwarz
 * @date    June 04, 2012
 *
 */

 # include <cmath>
 # include <iomanip>
 # include <string>
 # include <sstream>
 # include "SlipDatum.h"
 # include "SlipDef.h"
 # include "SlipDoubleOp.h"
 # include "SlipErr.h"

using namespace slip;

namespace slip {

   /*********************************************************
    *                  Casting Operators                    *
   *********************************************************/

   bool SlipDoubleOp::cast_bool(const SlipDatum& X) const {                     // (bool) X
       return (*getDouble(X) == 0.0)? false: true;
   }; // bool SlipDoubleOp::cast_bool(const SlipDatum& X) const
   UCHAR SlipDoubleOp::cast_UCHAR(const SlipDatum& X) const {                   // (UCHAR) X
       return (UCHAR)*getDouble(X);
   }; // UCHAR SlipDoubleOp::cast_UCHAR(const SlipDatum& X) const
   CHAR SlipDoubleOp::cast_CHAR(const SlipDatum& X) const {                     // (CHAR) X
       return (CHAR)*getDouble(X);
   }; // CHAR SlipDoubleOp::cast_CHAR(const SlipDatum& X) const
   ULONG SlipDoubleOp::cast_ULONG(const SlipDatum& X) const {                   // (ULONG) X
       return (ULONG)*getDouble(X);
   }; // ULONG SlipDoubleOp::cast_ULONG(const SlipDatum& X) const
   LONG SlipDoubleOp::cast_LONG(const SlipDatum& X) const {                     // (LONG) X
       return (LONG)*getDouble(X);
   }; // LONG SlipDoubleOp::cast_LONG(const SlipDatum& X) const
   DOUBLE SlipDoubleOp::cast_DOUBLE(const SlipDatum& X) const {                 // (DOUBLE) X
       return (DOUBLE)*getDouble(X);
   }; // DOUBLE SlipDoubleOp::cast_DOUBLE(const SlipDatum& X) const
   PTR SlipDoubleOp::cast_PTR(const SlipDatum& Y) const {                       // (PTR) Y
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipBoolOp++", "", "Operation not supported for double.", Y);
      return NULL;
   }; // PTR SlipDoubleOp::cast_PTR(const SlipDatum& Y) const

      /*********************************************************
       *                 Operator Overloading                  *
      *********************************************************/

          /*************************************************
           *                Unary operators                *
          *************************************************/
   ULONG SlipDoubleOp::bitNOT (const SlipDatum& X) const { // operator~
      postError(__FILE__, __LINE__, SlipErr::E1043, "SlipDoubleOp~", "", "Operation not supported for double.", X);
      return 0;
   }; // ULONG SlipDoubleOp::bitNOT (const SlipDatum& X) const
   bool SlipDoubleOp::logNOT (const SlipDatum& X) const { // operator!
      postError(__FILE__, __LINE__, SlipErr::E1025, "SlipDoubleOp!", "", "Operation not supported for double.", X);
      return SlipDatum((DOUBLE)(*getDouble(X)));
   }; // bool SlipDoubleOp::logNOT (const SlipDatum& X) const
   SlipDatum SlipDoubleOp::plus (const SlipDatum& X) const { // operator+
      return X;
   }; // SlipDatum SlipDoubleOp::plus (const SlipDatum& X) const
   SlipDatum SlipDoubleOp::minus (const SlipDatum& X) const { // operator-
      return SlipDatum(-(DOUBLE)(*getDouble(X)));
   }; // SlipDatum SlipDoubleOp::minus (const SlipDatum& X) const
   SlipDatum& SlipDoubleOp::postInc (SlipDatum& X) const { // operator++
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipDoubleOp(int)++", "", "Operation not supported for double.", X);
      return X;
   }; // SlipDatum& SlipDoubleOp::postInc (SlipDatum& X) const
   SlipDatum& SlipDoubleOp::postDec (SlipDatum& X) const { // operator--
      postError(__FILE__, __LINE__, SlipErr::E1004, "SlipDoubleOp(int)-~", "", "Operation not supported for double.", X);
      return X;
   }; // SlipDatum& SlipDoubleOp::postDec (SlipDatum& X) const
   SlipDatum& SlipDoubleOp::preInc (SlipDatum& X) const { // ++operator
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipDoubleOp++", "", "Operation not supported for double.", X);
      return X;
   }; // SlipDatum& SlipDoubleOp::preInc (SlipDatum& X) const
   SlipDatum& SlipDoubleOp::preDec (SlipDatum& X) const { // --operator
      postError(__FILE__, __LINE__, SlipErr::E1004, "SlipDoubleOp--", "", "Operation not supported for double.", X);
      return X;
   }; // SlipDatum& SlipDoubleOp::preDec (SlipDatum& X) const

          /*************************************************-
           *                Binary operators               *
          *************************************************/
   SlipDatum SlipDoubleOp::add(const SlipDatum& Y, const SlipDatum& X) const { // operator+
      DOUBLE num = *getDouble(Y);
      switch(X.getClassType()) {
         case eBOOL:
            num += *getBool(X);
            break;
         case eCHAR:
            num += *getChar(X);
            break;
         case eUCHAR:
            num += *getUChar(X);
            break;
         case eLONG:
            num += *getLong(X);
            break;
         case eULONG:
            num += *getULong(X);
            break;
         case eDOUBLE:
            num += *getDouble(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1029, "SlipDoubleOp+=", "", "Operation not supported for bool.", Y, X);
            break;
      }
      return SlipDatum(num);
   }; // SlipDatum SlipDoubleOp::add(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipDoubleOp::sub(const SlipDatum& Y, const SlipDatum& X) const { // operator-
      return (*getOperator(Y))->subDOP((DOUBLE)*getDouble(Y), X);
   }; // SlipDatum SlipDoubleOp::sub(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipDoubleOp::mul(const SlipDatum& Y, const SlipDatum& X) const { // operator*
      DOUBLE num = *getDouble(Y);
      switch(X.getClassType()) {
         case eBOOL:
            num *= *getBool(X);
            break;
         case eCHAR:
            num *= *getChar(X);
            break;
         case eUCHAR:
            num *= *getUChar(X);
            break;
         case eLONG:
            num *= *getLong(X);
            break;
         case eULONG:
            num *= *getULong(X);
            break;
         case eDOUBLE:
            num *= *getDouble(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1029, "SlipBoolOp*=", "", "Operation not supported for bool.", Y, X);
            break;
      }
      return SlipDatum(num);
   }; // SlipDatum SlipDoubleOp::mul(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipDoubleOp::div(const SlipDatum& Y, const SlipDatum& X) const { // operator/
      return (*getOperator(Y))->divDOP((DOUBLE)*getDouble(Y), X);
   }; // SlipDatum SlipDoubleOp::div(const SlipDatum& Y, const SlipDatum& X) const
   LONG       SlipDoubleOp::mod(const SlipDatum& Y, const SlipDatum& X) const { // operator%
      postError(__FILE__, __LINE__, SlipErr::E1026, "SlipDoubleOp%", "", "Operation not supported for header.", Y);
      return 0;
   }; // SlipDatum SlipDoubleOp::mod(const SlipDatum& Y, const SlipDatum& X) const

   SlipDatum SlipDoubleOp::add(const SlipDatum& Y, const DOUBLE X) const { // operator+
       return SlipDatum((DOUBLE)(*getDouble(Y) + X));
   }; // SlipDatum SlipDoubleOp::add(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipDoubleOp::sub(const SlipDatum& Y, const DOUBLE X) const { // operator-
       return SlipDatum((DOUBLE)(*getDouble(Y) - X));
   }; // SlipDatum SlipDoubleOp::sub(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipDoubleOp::mul(const SlipDatum& Y, const DOUBLE X) const { // operator*
       return SlipDatum((DOUBLE)(*getDouble(Y) * X));
   }; // SlipDatum SlipDoubleOp::mul(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipDoubleOp::div(const SlipDatum& Y, const DOUBLE X) const { // operator/
       return SlipDatum((DOUBLE)(*getDouble(Y) / X));
   }; // SlipDatum SlipDoubleOp::div(const SlipDatum& Y, const DOUBLE X) const

   SlipDatum SlipDoubleOp::add(const SlipDatum& Y, const ULONG X) const { // operator+
       return SlipDatum((DOUBLE)(*getDouble(Y) + X));
   }; // SlipDatum SlipDoubleOp::add(const SlipDatum& Y, const ULONG X) const

   SlipDatum SlipDoubleOp::sub(const SlipDatum& Y, const ULONG X) const { // operator-
       return SlipDatum((DOUBLE)(*getDouble(Y) - X));
   }; // SlipDatum SlipDoubleOp::sub(const SlipDatum& Y, const ULONG X) const
   SlipDatum SlipDoubleOp::mul(const SlipDatum& Y, const ULONG X) const { // operator*
       return SlipDatum((DOUBLE)(*getDouble(Y) * X));
   }; // SlipDatum SlipDoubleOp::mul(const SlipDatum& Y, const ULONG X) const
   SlipDatum SlipDoubleOp::div(const SlipDatum& Y, const ULONG X) const { // operator/
       return SlipDatum((DOUBLE)(*getDouble(Y) / X));
   }; // SlipDatum SlipDoubleOp::div(const SlipDatum& Y, const ULONG X) const
   LONG       SlipDoubleOp::mod(const SlipDatum& Y, const ULONG X) const { // operator%
       postError(__FILE__, __LINE__, SlipErr::E1026, "SlipDoubleOp%", "", "Operation not supported for header.", Y);
       return 0;
   }; // SlipDatum SlipDoubleOp::mod(const SlipDatum& Y, const ULONG X) const

   SlipDatum SlipDoubleOp::add(const SlipDatum& Y, const LONG X) const { // operator+
       return SlipDatum((DOUBLE)(*getDouble(Y) + X));
   }; // SlipDatum SlipDoubleOp::add(const SlipDatum& Y, const LONG X) const

   SlipDatum SlipDoubleOp::sub(const SlipDatum& Y, const LONG X) const { // operator-
       return SlipDatum((DOUBLE)(*getDouble(Y) - X));
   }; // SlipDatum SlipDoubleOp::sub(const SlipDatum& Y, const LONG X) const
   SlipDatum SlipDoubleOp::mul(const SlipDatum& Y, const LONG X) const { // operator*
       return SlipDatum((DOUBLE)(*getDouble(Y) * X));
   }; // SlipDatum SlipDoubleOp::mul(const SlipDatum& Y, const LONG X) const
   SlipDatum SlipDoubleOp::div(const SlipDatum& Y, const LONG X) const { // operator/
       return SlipDatum((DOUBLE)(*getDouble(Y) / X));
   }; // SlipDatum SlipDoubleOp::div(const SlipDatum& Y, const LONG X) const
   LONG       SlipDoubleOp::mod(const SlipDatum& Y, const LONG X) const { // operator%
       postError(__FILE__, __LINE__, SlipErr::E1026, "", "", "Operation not supported for header.", Y);
       return 0;
   }; // SlipDatum SlipDoubleOp::mod(const SlipDatum& Y, const LONG X) const

          /*************************************************
           *                Bit operators                  *
          *************************************************/
   LONG  SlipDoubleOp::shl (const SlipDatum& Y, const SlipDatum& X) const { // operator<<
      postError(__FILE__, __LINE__, SlipErr::E1020, "SlipDoubleOp<<", "", "Operation not supported for header.", Y, X);
      return 0;
   }; // LONG  SlipDoubleOp::shl (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipDoubleOp::shr (const SlipDatum& Y, const SlipDatum& X) const { // operator>>
      postError(__FILE__, __LINE__, SlipErr::E1021, "SlipDoubleOp>>", "", "Operation not supported for header.", Y, X);
      return 0;
   }; // LONG  SlipDoubleOp::shr (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipDoubleOp::band(const SlipDatum& Y, const SlipDatum& X) const { // operator&
      postError(__FILE__, __LINE__, SlipErr::E1024, "SlipDoubleOp&", "", "Operation not supported for header.", Y, X);
      return 0;
   }; // LONG  SlipDoubleOp::band(const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipDoubleOp::bor (const SlipDatum& Y, const SlipDatum& X) const { // operator|
      postError(__FILE__, __LINE__, SlipErr::E1023, "SlipDoubleOp|", "", "Operation not supported for header.", Y, X);
      return 0;
   }; // LONG  SlipDoubleOp::bor (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipDoubleOp::bxor(const SlipDatum& Y, const SlipDatum& X) const { // operator^
      postError(__FILE__, __LINE__, SlipErr::E1022, "SlipDoubleOp^", "", "Operation not supported for header.", Y, X);
      return 0;
   }; // LONG  SlipDoubleOp::bxor(const SlipDatum& Y, const SlipDatum& X) const

   LONG  SlipDoubleOp::shl (const SlipDatum& Y, const ULONG X) const { // operator<<
       postError(__FILE__, __LINE__, SlipErr::E1020, "SlipDoubleOp<<", "", "Operation not supported for header.", Y);
       return 0;
   }; // LONG  SlipDoubleOp::shl (const SlipDatum& Y, const ULONG X) const
   LONG  SlipDoubleOp::shr (const SlipDatum& Y, const ULONG X) const { // operator>>
       postError(__FILE__, __LINE__, SlipErr::E1021, "SlipDoubleOp>>", "", "Operation not supported for header.", Y);
       return 0;
   }; // LONG  SlipDoubleOp::shr (const SlipDatum& Y, const ULONG X) const
   LONG  SlipDoubleOp::band(const SlipDatum& Y, const ULONG X) const { // operator&
       postError(__FILE__, __LINE__, SlipErr::E1024, "SlipDoubleOp&", "", "Operation not supported for header.", Y);
       return 0;
   }; // LONG  SlipDoubleOp::band(const SlipDatum& Y, const ULONG X) const
   LONG  SlipDoubleOp::bor (const SlipDatum& Y, const ULONG X) const { // operator|
       postError(__FILE__, __LINE__, SlipErr::E1023, "SlipDoubleOp|", "", "Operation not supported for header.", Y);
       return 0;
   }; // LONG  SlipDoubleOp::bor (const SlipDatum& Y, const ULONG X) const
   LONG  SlipDoubleOp::bxor(const SlipDatum& Y, const ULONG X) const { // operator^
       postError(__FILE__, __LINE__, SlipErr::E1022, "SlipDoubleOp^", "", "Operation not supported for header.", Y);
       return 0;
   }; // LONG  SlipDoubleOp::bxor(const SlipDatum& Y, const ULONG X) const

   LONG  SlipDoubleOp::shl (const SlipDatum& Y, const LONG X) const { // operator<<
       postError(__FILE__, __LINE__, SlipErr::E1020, "SlipDoubleOp<<", "", "Operation not supported for header.", Y);
       return 0;
   }; // LONG  SlipDoubleOp::shl (const SlipDatum& Y, const LONG X) const
   LONG  SlipDoubleOp::shr (const SlipDatum& Y, const LONG X) const { // operator>>
       postError(__FILE__, __LINE__, SlipErr::E1021, "SlipDoubleOp>>", "", "Operation not supported for header.", Y);
       return 0;
   }; // LONG  SlipDoubleOp::shr (const SlipDatum& Y, const LONG X) const
   LONG  SlipDoubleOp::band(const SlipDatum& Y, const LONG X) const { // operator&
       postError(__FILE__, __LINE__, SlipErr::E1024, "SlipDoubleOp&", "", "Operation not supported for header.", Y);
       return 0;
   }; // LONG  SlipDoubleOp::band(const SlipDatum& Y, const LONG X) const
   LONG  SlipDoubleOp::bor (const SlipDatum& Y, const LONG X) const { // operator|
       postError(__FILE__, __LINE__, SlipErr::E1023, "SlipDoubleOp|", "", "Operation not supported for header.", Y);
       return 0;
   }; // LONG  SlipDoubleOp::bor (const SlipDatum& Y, const LONG X) const
   LONG  SlipDoubleOp::bxor(const SlipDatum& Y, const LONG X) const { // operator^
       postError(__FILE__, __LINE__, SlipErr::E1022, "SlipDoubleOp^", "", "Operation not supported for header.", Y);
       return 0;
   }; // LONG  SlipDoubleOp::bxor(const SlipDatum& Y, const LONG X) const

          /*************************************************
           *               Simple Assignment               *
          *************************************************/
   SlipDatum& SlipDoubleOp::asgn(SlipDatum& Y, const SlipDatum& X) const {        // operator+=
      *getDouble(Y) = (DOUBLE)X;
      return Y;
   }; // SlipDatum& SlipDoubleOp::asgn(SlipDatum& Y, const SlipDatum& X) const

          /*************************************************
           *       Arithmetic Assignment operators         *
          *************************************************/
   SlipDatum& SlipDoubleOp::addAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator+=
      DOUBLE num = *getDouble(Y);
      switch(X.getClassType()) {
         case eBOOL:
            num += *getBool(X);
            break;
         case eCHAR:
            num += *getChar(X);
            break;
         case eUCHAR:
            num += *getUChar(X);
            break;
         case eLONG:
            num += *getLong(X);
            break;
         case eULONG:
            num += *getULong(X);
            break;
         case eDOUBLE:
            num += *getDouble(X);
            break;
         case ePTR:
            num += (*getOperator(X))->cast_DOUBLE(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1009, "SlipDoubleOp+=", "", "Operation not supported for DOUBLE.", Y, X);
            break;
      }
      *getDouble(Y) = num;
      return Y;
   }; // SlipDatum& SlipDoubleOp::addAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipDoubleOp::subAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator-=
      DOUBLE num = *getDouble(Y);
      switch(X.getClassType()) {
         case eBOOL:
            num -= *getBool(X);
            break;
         case eCHAR:
            num -= *getChar(X);
            break;
         case eUCHAR:
            num -= *getUChar(X);
            break;
         case eLONG:
            num -= *getLong(X);
            break;
         case eULONG:
            num -= *getULong(X);
            break;
         case eDOUBLE:
            num -= *getDouble(X);
            break;
         case ePTR:
            num -= (*getOperator(X))->cast_DOUBLE(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1010, "SlipDoubleOp-=", "", "Operation not supported for DOUBLE.", Y, X);
            break;
      }
      *getDouble(Y) = num;
      return Y;
   }; // SlipDatum& SlipDoubleOp::subAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipDoubleOp::mulAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator*=
      DOUBLE num = *getDouble(Y);
      switch(X.getClassType()) {
         case eBOOL:
            num *= *getBool(X);
            break;
         case eCHAR:
            num *= *getChar(X);
            break;
         case eUCHAR:
            num *= *getUChar(X);
            break;
         case eLONG:
            num *= *getLong(X);
            break;
         case eULONG:
            num *= *getULong(X);
            break;
         case eDOUBLE:
            num *= *getDouble(X);
            break;
         case ePTR:
            num *= (*getOperator(X))->cast_DOUBLE(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1011, "SlipDoubleOp*=", "", "Operation not supported for DOUBLE.", Y, X);
            break;
      }
      *getDouble(Y) = num;
      return Y;
   }; // SlipDatum& SlipDoubleOp::mulAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipDoubleOp::divAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator/=
      DOUBLE num = *getDouble(Y);
      switch(X.getClassType()) {
         case eBOOL:
            num /= *getBool(X);
            break;
         case eCHAR:
            num /= *getChar(X);
            break;
         case eUCHAR:
            num /= *getUChar(X);
            break;
         case eLONG:
            num /= *getLong(X);
            break;
         case eULONG:
            num /= *getULong(X);
            break;
         case eDOUBLE:
            num /= *getDouble(X);
            break;
         case ePTR:
            num /= (*getOperator(X))->cast_DOUBLE(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1012, "SlipDoubleOp/=", "", "Operation not supported for DOUBLE.", Y, X);
            break;
      }
      *getDouble(Y) = num;
      return Y;
   }; // SlipDatum& SlipDoubleOp::divAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipDoubleOp::modAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator%=
      postError(__FILE__, __LINE__, SlipErr::E1013, "SlipDoubleOp%=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipDoubleOp::modAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipDoubleOp::shlAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator<<=
      postError(__FILE__, __LINE__, SlipErr::E1014, "SlipDoubleOp<<=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipDoubleOp::shlAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipDoubleOp::shrAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator>>=
      postError(__FILE__, __LINE__, SlipErr::E1015, "SlipDoubleOp>>=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipDoubleOp::shrAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipDoubleOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator&=
      postError(__FILE__, __LINE__, SlipErr::E1016, "SlipDoubleOp&=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipDoubleOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipDoubleOp::borAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator|=
      postError(__FILE__, __LINE__, SlipErr::E1019, "SlipDoubleOp|=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipDoubleOp::borAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipDoubleOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator^=
      postError(__FILE__, __LINE__, SlipErr::E1017, "SlipDoubleOp^=", "", "Operation not supported for header.", Y);
      return Y;
   }; // SlipDatum& SlipDoubleOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const

   SlipDatum& SlipDoubleOp::addAsgn (SlipDatum& Y, const DOUBLE X) const { // operator+=
       *getDouble(Y) += X;
       return Y;
   }; // SlipDatum& SlipDoubleOp::addAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipDoubleOp::subAsgn (SlipDatum& Y, const DOUBLE X) const { // operator-=
       *getDouble(Y) -= X;
       return Y;
   }; // SlipDatum& SlipDoubleOp::subAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipDoubleOp::mulAsgn (SlipDatum& Y, const DOUBLE X) const { // operator*=
       *getDouble(Y) *= X;
       return Y;
   }; // SlipDatum& SlipDoubleOp::mulAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipDoubleOp::divAsgn (SlipDatum& Y, const DOUBLE X) const { // operator/=
       *getDouble(Y) /= X;
       return Y;
   }; // SlipDatum& SlipDoubleOp::divAsgn (SlipDatum& Y, const DOUBLE X) const

   SlipDatum& SlipDoubleOp::addAsgn (SlipDatum& Y, const ULONG X) const { // operator+=
       *getDouble(Y) += X;
       return Y;
   }; // SlipDatum& SlipDoubleOp::addAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipDoubleOp::subAsgn (SlipDatum& Y, const ULONG X) const { // operator-=
       *getDouble(Y) -= X;
       return Y;
   }; // SlipDatum& SlipDoubleOp::subAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipDoubleOp::mulAsgn (SlipDatum& Y, const ULONG X) const { // operator*=
       *getDouble(Y) *= X;
       return Y;
   }; // SlipDatum& SlipDoubleOp::mulAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipDoubleOp::divAsgn (SlipDatum& Y, const ULONG X) const { // operator/=
       *getDouble(Y) /= X;
       return Y;
   }; // SlipDatum& SlipDoubleOp::divAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipDoubleOp::modAsgn (SlipDatum& Y, const ULONG X) const { // operator%=
       postError(__FILE__, __LINE__, SlipErr::E1013, "SlipDoubleOp%=", "", "Operation not supported for header.", Y);
       return Y;
   }; // SlipDatum& SlipDoubleOp::modAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipDoubleOp::shlAsgn (SlipDatum& Y, const ULONG X) const { // operator<<=
       postError(__FILE__, __LINE__, SlipErr::E1014, "SlipDoubleOp<<=", "", "Operation not supported for header.", Y);
       return Y;
   }; // SlipDatum& SlipDoubleOp::shlAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipDoubleOp::shrAsgn (SlipDatum& Y, const ULONG X) const { // operator>>=
       postError(__FILE__, __LINE__, SlipErr::E1015, "SlipDoubleOp>>=", "", "Operation not supported for header.", Y);
       return Y;
   }; // SlipDatum& SlipDoubleOp::shrAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipDoubleOp::bandAsgn(SlipDatum& Y, const ULONG X) const { // operator&=
       postError(__FILE__, __LINE__, SlipErr::E1016, "SlipDoubleOp&=", "", "Operation not supported for header.", Y);
       return Y;
   }; // SlipDatum& SlipDoubleOp::bandAsgn(SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipDoubleOp::borAsgn (SlipDatum& Y, const ULONG X) const { // operator|=
       postError(__FILE__, __LINE__, SlipErr::E1019, "SlipDoubleOp|=", "", "Operation not supported for header.", Y);
       return Y;
   }; // SlipDatum& SlipDoubleOp::borAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipDoubleOp::bxorAsgn(SlipDatum& Y, const ULONG X) const { // operator^=
       postError(__FILE__, __LINE__, SlipErr::E1017, "SlipDoubleOp^=", "", "Operation not supported for header.", Y);
       return Y;
   }; // SlipDatum& SlipDoubleOp::bxorAsgn(SlipDatum& Y, const ULONG X) const

   SlipDatum& SlipDoubleOp::addAsgn (SlipDatum& Y, const LONG X) const { // operator+=
       *getDouble(Y) += X;
       return Y;
   }; // SlipDatum& SlipDoubleOp::addAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipDoubleOp::subAsgn (SlipDatum& Y, const LONG X) const { // operator-=
       *getDouble(Y) -= X;
       return Y;
   }; // SlipDatum& SlipDoubleOp::subAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipDoubleOp::mulAsgn (SlipDatum& Y, const LONG X) const { // operator*=
       *getDouble(Y) *= X;
       return Y;
   }; // SlipDatum& SlipDoubleOp::mulAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipDoubleOp::divAsgn (SlipDatum& Y, const LONG X) const { // operator/=
       *getDouble(Y) /= X;
       return Y;
   }; // SlipDatum& SlipDoubleOp::divAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipDoubleOp::modAsgn (SlipDatum& Y, const LONG X) const { // operator%=
       postError(__FILE__, __LINE__, SlipErr::E1013, "SlipDoubleOp%=", "", "Operation not supported for header.", Y);
       return Y;
   }; // SlipDatum& SlipDoubleOp::modAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipDoubleOp::shlAsgn (SlipDatum& Y, const LONG X) const { // operator<<=
       postError(__FILE__, __LINE__, SlipErr::E1014, "SlipDoubleOp<<=", "", "Operation not supported for header.", Y);
       return Y;
   }; // SlipDatum& SlipDoubleOp::shlAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipDoubleOp::shrAsgn (SlipDatum& Y, const LONG X) const { // operator>>=
       postError(__FILE__, __LINE__, SlipErr::E1015, "SlipDoubleOp>>=", "", "Operation not supported for header.", Y);
       return Y;
   }; // SlipDatum& SlipDoubleOp::shrAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipDoubleOp::bandAsgn(SlipDatum& Y, const LONG X) const { // operator&=
       postError(__FILE__, __LINE__, SlipErr::E1016, "SlipDoubleOp&=", "", "Operation not supported for header.", Y);
       return Y;
   }; // SlipDatum& SlipDoubleOp::bandAsgn(SlipDatum& Y, const LONG X) const
   SlipDatum& SlipDoubleOp::borAsgn (SlipDatum& Y, const LONG X) const { // operator|=
       postError(__FILE__, __LINE__, SlipErr::E1019, "SlipDoubleOp|=", "", "Operation not supported for header.", Y);
       return Y;
   }; // SlipDatum& SlipDoubleOp::borAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipDoubleOp::bxorAsgn(SlipDatum& Y, const LONG X) const { // operator^=
       postError(__FILE__, __LINE__, SlipErr::E1017, "SlipDoubleOp^=", "", "Operation not supported for header.", Y);
       return Y;
   }; // SlipDatum& SlipDoubleOp::bxorAsgn(SlipDatum& Y, const LONG X) const

          /*************************************************
           *               Logical operators               *
          *************************************************/
   bool SlipDoubleOp::notEqual (const SlipDatum& Y, const SlipDatum& X) const { // operator!=
      bool flag = true;
      DOUBLE num = *getDouble(Y);
      switch(X.getClassType()) {
         case eBOOL:
            flag = num != *getBool(X);
            break;
         case eCHAR:
            flag = num != *getChar(X);
            break;
         case eUCHAR:
            flag = num != *getUChar(X);
            break;
         case eLONG:
            flag = num != *getLong(X);
            break;
         case eULONG:
            flag = num != *getULong(X);
            break;
         case eDOUBLE:
            flag = num != *getDouble(X);
            break;
         case ePTR:
            flag = (*getOperator(X))->notEqual(X, num);
            break;
         default:
            flag = false;
            break;
      }
      return flag;
   }; // bool SlipDoubleOp::notEqual (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipDoubleOp::lessThan (const SlipDatum& Y, const SlipDatum& X) const { // operator<
      bool flag = false;
      DOUBLE num = *getDouble(Y);
      switch(X.getClassType()) {
         case eBOOL:
            flag = num < *getBool(X);
            break;
         case eCHAR:
            flag = num < *getChar(X);
            break;
         case eUCHAR:
            flag = num < *getUChar(X);
            break;
         case eLONG:
            flag = num < *getLong(X);
            break;
         case eULONG:
            flag = num < *getULong(X);
            break;
         case eDOUBLE:
            flag = num < *getDouble(X);
            break;
         case ePTR:
            flag = (*getOperator(X))->greatThan(X, num);
            break;
         default:
            flag = false;
            break;
      }
      return flag;
   }; // bool SlipDoubleOp::lessThan (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipDoubleOp::lessEqual (const SlipDatum& Y, const SlipDatum& X) const { // operator<=
      bool flag = false;
      DOUBLE num = *getDouble(Y);
      switch(X.getClassType()) {
         case eBOOL:
            flag = num <= *getBool(X);
            break;
         case eCHAR:
            flag = num <= *getChar(X);
            break;
         case eUCHAR:
            flag = num <= *getUChar(X);
            break;
         case eLONG:
            flag = num <= *getLong(X);
            break;
         case eULONG:
            flag = num <= *getULong(X);
            break;
         case eDOUBLE:
            flag = num <= *getDouble(X);
            break;
         case ePTR:
            flag = (*getOperator(X))->greatEqual(X, num);
            break;
         default:
            flag = false;
            break;
      }
      return flag;
   }; // bool SlipDoubleOp::lessEqual (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipDoubleOp::equal (const SlipDatum& Y, const SlipDatum& X) const { // operator==
      bool flag = false;
      DOUBLE num = *getDouble(Y);
      switch(X.getClassType()) {
         case eBOOL:
            flag = num == *getBool(X);
            break;
         case eCHAR:
            flag = num == *getChar(X);
            break;
         case eUCHAR:
            flag = num == *getUChar(X);
            break;
         case eLONG:
            flag = num == *getLong(X);
            break;
         case eULONG:
            flag = num == *getULong(X);
            break;
         case eDOUBLE:
            flag = num == *getDouble(X);
            break;
         case ePTR:
            flag = (*getOperator(X))->equal(X, num);
            break;
         default:
            flag = false;
            break;
      }
      return flag;
   }; // bool SlipDoubleOp::equal (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipDoubleOp::greatEqual(const SlipDatum& Y, const SlipDatum& X) const { // operator>=
      bool flag = false;
      DOUBLE num = *getDouble(Y);
      switch(X.getClassType()) {
         case eBOOL:
            flag = num >= *getBool(X);
            break;
         case eCHAR:
            flag = num >= *getChar(X);
            break;
         case eUCHAR:
            flag = num >= *getUChar(X);
            break;
         case eLONG:
            flag = num >= *getLong(X);
            break;
         case eULONG:
            flag = num >= *getULong(X);
            break;
         case eDOUBLE:
            flag = num >= *getDouble(X);
            break;
         case ePTR:
            flag = (*getOperator(X))->lessEqual(X, num);
            break;
         default:
            flag = false;
            break;
      }
      return flag;
   }; // bool SlipDoubleOp::greatEqual(const SlipDatum& Y, const SlipDatum& X) const
   bool SlipDoubleOp::greatThan (const SlipDatum& Y, const SlipDatum& X) const { // operator>
      bool flag = false;
      DOUBLE num = *getDouble(Y);
      switch(X.getClassType()) {
         case eBOOL:
            flag = num > *getBool(X);
            break;
         case eCHAR:
            flag = num > *getChar(X);
            break;
         case eUCHAR:
            flag = num > *getUChar(X);
            break;
         case eLONG:
            flag = num > *getLong(X);
            break;
         case eULONG:
            flag = num > *getULong(X);
            break;
         case eDOUBLE:
            flag = num > *getDouble(X);
            break;
         case ePTR:
            flag = (*getOperator(X))->lessThan(X, num);
            break;
         default:
            flag = false;
            break;
      }
      return flag;
   }; // bool SlipDoubleOp::greaterThan (const SlipDatum& Y, const SlipDatum& X) const

   bool SlipDoubleOp::notEqual (const SlipDatum& Y, const DOUBLE X) const { // operator!=
       return *getDouble(Y) != X;
   }; // bool SlipDoubleOp::notEqual (const SlipDatum& Y, const DOUBLE X) const
   bool SlipDoubleOp::lessThan (const SlipDatum& Y, const DOUBLE X) const { // operator<
       return *getDouble(Y) < X;
   }; // bool SlipDoubleOp::lessThan (const SlipDatum& Y, const DOUBLE X) const
   bool SlipDoubleOp::lessEqual (const SlipDatum& Y, const DOUBLE X) const { // operator<=
       return *getDouble(Y) <= X;
   }; // bool SlipDoubleOp::lessEqual (const SlipDatum& Y, const DOUBLE X) const
   bool SlipDoubleOp::equal (const SlipDatum& Y, const DOUBLE X) const { // operator==
       return *getDouble(Y) == X;
   }; // bool SlipDoubleOp::equal (const SlipDatum& Y, const DOUBLE X) const
   bool SlipDoubleOp::greatEqual(const SlipDatum& Y, const DOUBLE X) const { // operator>=
       return *getDouble(Y) >= X;
   }; // bool SlipDoubleOp::greatEqual(const SlipDatum& Y, const DOUBLE X) const
   bool SlipDoubleOp::greatThan (const SlipDatum& Y, const DOUBLE X) const { // operator>
       return *getDouble(Y) > X;
   }; // bool SlipDoubleOp::greatThan (const SlipDatum& Y, const DOUBLE X) const

   bool SlipDoubleOp::notEqual (const SlipDatum& Y, const ULONG X) const { // operator!=
       return *getDouble(Y) != X;
   }; // bool SlipDoubleOp::notEqual (const SlipDatum& Y, const ULONG X) const
   bool SlipDoubleOp::lessThan (const SlipDatum& Y, const ULONG X) const { // operator<
       return *getDouble(Y) < X;
   }; // bool SlipDoubleOp::lessThan (const SlipDatum& Y, const ULONG X) const
   bool SlipDoubleOp::lessEqual (const SlipDatum& Y, const ULONG X) const { // operator<=
       return *getDouble(Y) <= X;
   }; // bool SlipDoubleOp::lessEqual (const SlipDatum& Y, const ULONG X) const
   bool SlipDoubleOp::equal (const SlipDatum& Y, const ULONG X) const { // operator==
       return *getDouble(Y) == X;
   }; // bool SlipDoubleOp::equal (const SlipDatum& Y, const ULONG X) const
   bool SlipDoubleOp::greatEqual(const SlipDatum& Y, const ULONG X) const { // operator>=
       return *getDouble(Y) >= X;
   }; // bool SlipDoubleOp::greatEqual(const SlipDatum& Y, const ULONG X) const
   bool SlipDoubleOp::greatThan (const SlipDatum& Y, const ULONG X) const { // operator>
       return *getDouble(Y) > X;
   }; // bool SlipDoubleOp::greatThan (const SlipDatum& Y, const ULONG X) const

   bool SlipDoubleOp::notEqual (const SlipDatum& Y, const LONG X) const { // operator!=
       return *getDouble(Y) != X;
   }; // bool SlipDoubleOp::notEqual (const SlipDatum& Y, const LONG X) const
   bool SlipDoubleOp::lessThan (const SlipDatum& Y, const LONG X) const { // operator<
       return *getDouble(Y) < X;
   }; // bool SlipDoubleOp::lessThan (const SlipDatum& Y, const LONG X) const
   bool SlipDoubleOp::lessEqual (const SlipDatum& Y, const LONG X) const { // operator<=
       return *getDouble(Y) <= X;
   }; // bool SlipDoubleOp::lessEqual (const SlipDatum& Y, const LONG X) const
   bool SlipDoubleOp::equal (const SlipDatum& Y, const LONG X) const { // operator==
       return *getDouble(Y) == X;
   }; // bool SlipDoubleOp::equal (const SlipDatum& Y, const LONG X) const
   bool SlipDoubleOp::greatEqual(const SlipDatum& Y, const LONG X) const { // operator>=
       return *getDouble(Y) >= X;
   }; // bool SlipDoubleOp::greatEqual(const SlipDatum& Y, const LONG X) const
   bool SlipDoubleOp::greatThan (const SlipDatum& Y, const LONG X) const { // operator>
       return *getDouble(Y) > X;
   }; // bool SlipDoubleOp::greatThan (const SlipDatum& Y, const LONG X) const

   bool SlipDoubleOp::notEqual (const SlipDatum& Y, const PTR X) const { // operator!=
       return false;
   }; // bool SlipDoubleOp::notEqual (const SlipDatum& Y, const PTR X) const
   bool SlipDoubleOp::equal (const SlipDatum& Y, const PTR X) const { // operator==
       return false;
   }; // bool SlipDoubleOp::equal (const SlipDatum& Y, const PTR X) const

   bool SlipDoubleOp::notEqual (const SlipDatum& Y, const string& X) const { // operator!=
       return false;
   }; // bool SlipDoubleOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipDoubleOp::lessThan (const SlipDatum& Y, const string& X) const { // operator<
       return false;
   }; // bool SlipDoubleOp::equal (const SlipDatum& Y, const string& X) const
   bool SlipDoubleOp::lessEqual (const SlipDatum& Y, const string& X) const { // operator<=
       return false;
   }; // bool SlipDoubleOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipDoubleOp::equal (const SlipDatum& Y, const string& X) const { // operator==
       return false;
   }; // bool SlipDoubleOp::equal (const SlipDatum& Y, const string& X) const
   bool SlipDoubleOp::greatEqual (const SlipDatum& Y, const string& X) const { // operator>=
       return false;
   }; // bool SlipDoubleOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipDoubleOp::greatThan (const SlipDatum& Y, const string& X) const { // operator>
       return false;
   }; // bool SlipDoubleOp::equal (const SlipDatum& Y, const string& X) const

   string SlipDoubleOp::toString(const SlipCell& Y) const { // pretty print SLIP cell
      stringstream pretty;
      double dbl =  *getDouble((SlipDatum&)Y);
      double lg  = log10(dbl);
      if      (lg < -3) pretty << scientific << dbl;
      else if (lg >  7) pretty << scientific << dbl;
      else              pretty << setprecision(7) << fixed << dbl;
      return pretty.str();
   }; // string SlipDoubleOp::toString(const SlipCell& Y) const

   string SlipDoubleOp::dump(const SlipCell& Y) const {
      stringstream pretty;
      pretty << "[double   ] " << dumpLink(Y) << " = " << toString(Y);
      return pretty.str();
   }; // string SlipDoubleOp::dump(const SlipDatum& Y) const

   string SlipDoubleOp::write(const SlipCell& X) const {
      char buffer[25];
      sprintf(buffer, "%la", *getDouble((SlipDatum&)X));
      return string(buffer);
//      return toString(X);
   }; // string SlipDoubleOp::write(const SlipCell& X) const

}; // namespace slip