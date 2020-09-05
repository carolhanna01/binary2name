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
 * @file    SlipLongOp.cpp
 * @author  A. Schwarz
 * @date    June 04, 2012
 *
 */

 # include <string>
 # include <sstream>
 # include "SlipDatum.h"
 # include "SlipDef.h"
 # include "SlipErr.h"
 # include "SlipLongOp.h"

using namespace slip;

namespace slip {

   /*********************************************************
    *                  Casting Operators                    *
   *********************************************************/

   bool SlipLongOp::cast_bool(const SlipDatum& X) const {                       // (bool) X
       return (*getLong(X) == 0)? false: true;
   }; // bool SlipLongOp::cast_bool(const SlipDatum& X) const
   UCHAR SlipLongOp::cast_UCHAR(const SlipDatum& X) const {                     // (UCHAR) X
       return (UCHAR)*getLong(X);
   }; // UCHAR SlipLongOp::cast_UCHAR(const SlipDatum& X) const
   CHAR SlipLongOp::cast_CHAR(const SlipDatum& X) const {                       // (CHAR) X
       return (CHAR)*getLong(X);
   }; // CHAR SlipLongOp::cast_CHAR(const SlipDatum& X) const
   ULONG SlipLongOp::cast_ULONG(const SlipDatum& X) const {                     // (ULONG) X
       return (ULONG)*getLong(X);
   }; // ULONG SlipLongOp::cast_ULONG(const SlipDatum& X) const
   LONG SlipLongOp::cast_LONG(const SlipDatum& X) const {                       // (LONG) X
       return *getLong(X);
   }; // LONG SlipLongOp::cast_LONG(const SlipDatum& X) const
   DOUBLE SlipLongOp::cast_DOUBLE(const SlipDatum& X) const {                   // (DOUBLE) X
       return (DOUBLE)*getLong(X);
   }; // DOUBLE SlipLongOp::cast_DOUBLE(const SlipDatum& X) const
   PTR SlipLongOp::cast_PTR(const SlipDatum& Y) const {                         // (PTR) Y
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipBoolOp++", "", "Operation not supported for long.", Y);
      return NULL;
   }; // PTR SlipLongOp::cast_PTR(const SlipDatum& Y) const

      /*********************************************************
       *                 Operator Overloading                  *
      *********************************************************/

          /*************************************************
           *                Unary operators                *
          *************************************************/
   ULONG SlipLongOp::bitNOT (const SlipDatum& X) const { // operator~
      return ~(ULONG)*getLong(X);
   }; // ULONG SlipLongOp::bitNOT (const SlipDatum& X) const
   bool SlipLongOp::logNOT (const SlipDatum& X) const { // operator!
      return !(bool)*getLong(X);
   }; // bool SlipLongOp::logNOT (const SlipDatum& X) const
   SlipDatum SlipLongOp::plus (const SlipDatum& X) const { // operator+
       return SlipDatum((ULONG)(*getLong(X)));
   }; // SlipDatum SlipLongOp::plus (const SlipDatum& X) const
   SlipDatum SlipLongOp::minus (const SlipDatum& X) const { // operator-
       return SlipDatum(-(ULONG)(*getLong(X)));
   }; // SlipDatum SlipLongOp::minus (const SlipDatum& X) const
   SlipDatum& SlipLongOp::postInc (SlipDatum& X) const { // operator++
       (*getLong(X))++;
       return X;
   }; // SlipDatum& SlipLongOp::postInc (SlipDatum& X) const
   SlipDatum& SlipLongOp::postDec (SlipDatum& X) const { // operator--
       (*getLong(X))--;
       return X;
   }; // SlipDatum& SlipLongOp::postDec (SlipDatum& X) const
   SlipDatum& SlipLongOp::preInc (SlipDatum& X) const { // ++operator
       ++(*getLong(X));
       return X;
   }; // SlipDatum& SlipLongOp::preInc (SlipDatum& X) const
   SlipDatum& SlipLongOp::preDec (SlipDatum& X) const { // --operator
       --(*getLong(X));
       return X;
   }; // SlipDatum& SlipLongOp::preDec (SlipDatum& X) const

          /*************************************************
           *                Binary operators               *
          *************************************************/
   SlipDatum SlipLongOp::add(const SlipDatum& Y, const SlipDatum& X) const { // operator+
      return (*getOperator(X))->add(X, (LONG)*getLong(Y));
   }; // SlipDatum SlipLongOp::add(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipLongOp::sub(const SlipDatum& Y, const SlipDatum& X) const { // operator-
      return (*getOperator(Y))->subOP((LONG)*getLong(Y), X);
   }; // SlipDatum SlipLongOp::sub(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipLongOp::mul(const SlipDatum& Y, const SlipDatum& X) const { // operator*
      return (*getOperator(X))->mul(X, (LONG)*getLong(Y));
   }; // SlipDatum SlipLongOp::mul(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipLongOp::div(const SlipDatum& Y, const SlipDatum& X) const { // operator/
      return (*getOperator(Y))->divOP((LONG)*getLong(Y), X);
   }; // SlipDatum SlipLongOp::div(const SlipDatum& Y, const SlipDatum& X) const
   LONG SlipLongOp::mod(const SlipDatum& Y, const SlipDatum& X) const { // operator%
      return (*getOperator(Y))-> modOP((LONG)*getLong(Y), X);
   }; // SlipDatum SlipLongOp::mod(const SlipDatum& Y, const SlipDatum& X) const

   SlipDatum SlipLongOp::add(const SlipDatum& Y, const DOUBLE X) const { // operator+
       return SlipDatum((*getLong(Y) + X));
   }; // SlipDatum SlipLongOp::add(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipLongOp::sub(const SlipDatum& Y, const DOUBLE X) const { // operator-
       return SlipDatum((*getLong(Y) - X));
   }; // SlipDatum SlipLongOp::sub(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipLongOp::mul(const SlipDatum& Y, const DOUBLE X) const { // operator*
       return SlipDatum((*getLong(Y) * X));
   }; // SlipDatum SlipLongOp::mul(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipLongOp::div(const SlipDatum& Y, const DOUBLE X) const { // operator/
       return SlipDatum((*getLong(Y) / X));
   }; // SlipDatum SlipLongOp::div(const SlipDatum& Y, const DOUBLE X) const

   SlipDatum SlipLongOp::add(const SlipDatum& Y, const ULONG X) const { // operator+
       return SlipDatum((*getLong(Y) + X));
   }; // SlipDatum SlipLongOp::add(const SlipDatum& Y, const ULONG X) const

   SlipDatum SlipLongOp::sub(const SlipDatum& Y, const ULONG X) const { // operator-
       return SlipDatum((*getLong(Y) - X));
   }; // SlipDatum SlipLongOp::sub(const SlipDatum& Y, const ULONG X) const
   SlipDatum SlipLongOp::mul(const SlipDatum& Y, const ULONG X) const { // operator*
       return SlipDatum((*getLong(Y) * X));
   }; // SlipDatum SlipLongOp::mul(const SlipDatum& Y, const ULONG X) const
   SlipDatum SlipLongOp::div(const SlipDatum& Y, const ULONG X) const { // operator/
       return SlipDatum((*getLong(Y) / X));
   }; // SlipDatum SlipLongOp::div(const SlipDatum& Y, const ULONG X) const
   LONG       SlipLongOp::mod(const SlipDatum& Y, const ULONG X) const { // operator%
       return SlipDatum((*getLong(Y) % X));
   }; // SlipDatum SlipLongOp::mod(const SlipDatum& Y, const ULONG X) const

   SlipDatum SlipLongOp::add(const SlipDatum& Y, const LONG X) const { // operator+
       return SlipDatum((*getLong(Y) + X));
   }; // SlipDatum SlipLongOp::add(const SlipDatum& Y, const LONG X) const

   SlipDatum SlipLongOp::sub(const SlipDatum& Y, const LONG X) const { // operator-
       return SlipDatum((*getLong(Y) - X));
   }; // SlipDatum SlipLongOp::sub(const SlipDatum& Y, const LONG X) const
   SlipDatum SlipLongOp::mul(const SlipDatum& Y, const LONG X) const { // operator*
       return SlipDatum((*getLong(Y) * X));
   }; // SlipDatum SlipLongOp::mul(const SlipDatum& Y, const LONG X) const
   SlipDatum SlipLongOp::div(const SlipDatum& Y, const LONG X) const { // operator/
       return SlipDatum((*getLong(Y) / X));
   }; // SlipDatum SlipLongOp::div(const SlipDatum& Y, const LONG X) const
   LONG       SlipLongOp::mod(const SlipDatum& Y, const LONG X) const { // operator%
       return SlipDatum((*getLong(Y) % X));
   }; // SlipDatum SlipLongOp::mod(const SlipDatum& Y, const LONG X) const


          /*************************************************
           *                Bit operators                  *
          *************************************************/
   LONG SlipLongOp::shl (const SlipDatum& Y, const SlipDatum& X) const { // operator<<
      return (*getOperator(Y))->shlOP((LONG)*getLong(Y), X);
   }; // SlipDatum& SlipLongOp::shl (const SlipDatum& Y, const SlipDatum& X) const
   LONG SlipLongOp::shr (const SlipDatum& Y, const SlipDatum& X) const { // operator>>
      return (*getOperator(Y))->shrOP((LONG)*getLong(Y), X);
   }; // LONG SlipLongOp::shr (const SlipDatum& Y, const SlipDatum& X) const
   LONG SlipLongOp::band(const SlipDatum& Y, const SlipDatum& X) const { // operator&
      return (*getOperator(X))->band(X, (LONG)*getLong(Y));
   }; // LONG SlipLongOp::band(const SlipDatum& Y, const SlipDatum& X) const
   LONG SlipLongOp::bor (const SlipDatum& Y, const SlipDatum& X) const { // operator|
      return (*getOperator(X))->bor(X, (LONG)*getLong(Y));
   }; // LONG SlipLongOp::bor (const SlipDatum& Y, const SlipDatum& X) const
   LONG SlipLongOp::bxor(const SlipDatum& Y, const SlipDatum& X) const { // operator^
      return (*getOperator(X))->bxor(X, (LONG)*getLong(Y));
   }; // LONG SlipLongOp::bxor(const SlipDatum& Y, const SlipDatum& X) const

   LONG SlipLongOp::shl (const SlipDatum& Y, const ULONG X) const { // operator<<
       return (LONG)(*getLong(Y) << X);
   }; // LONG SlipLongOp::shl (const SlipDatum& Y, const ULONG X) const
   LONG SlipLongOp::shr (const SlipDatum& Y, const ULONG X) const { // operator>>
       return (LONG)(*getLong(Y) >> X);
   }; // LONG SlipLongOp::shr (const SlipDatum& Y, const ULONG X) const
   LONG SlipLongOp::band(const SlipDatum& Y, const ULONG X) const { // operator&
       return (LONG)(*getLong(Y) & X);
   }; // LONG SlipLongOp::band(const SlipDatum& Y, const ULONG X) const
   LONG SlipLongOp::bor (const SlipDatum& Y, const ULONG X) const { // operator|
       return (LONG)(*getLong(Y) | X);
   }; // LONG SlipLongOp::bor (const SlipDatum& Y, const ULONG X) const
   LONG SlipLongOp::bxor(const SlipDatum& Y, const ULONG X) const { // operator^
       return (LONG)(*getLong(Y) ^ X);
   }; // LONG SlipLongOp::bxor(const SlipDatum& Y, const ULONG X) const

   LONG SlipLongOp::shl (const SlipDatum& Y, const LONG X) const { // operator<<
       return (LONG)(*getLong(Y) << X);
   }; // LONG SlipLongOp::shl (const SlipDatum& Y, const LONG X) const
   LONG SlipLongOp::shr (const SlipDatum& Y, const LONG X) const { // operator>>
       return (LONG)(*getLong(Y) >> X);
   }; // LONG SlipLongOp::shr (const SlipDatum& Y, const LONG X) const
   LONG SlipLongOp::band(const SlipDatum& Y, const LONG X) const { // operator&
       return (LONG)(*getLong(Y) & X);
   }; // LONG SlipLongOp::band(const SlipDatum& Y, const LONG X) const
   LONG SlipLongOp::bor (const SlipDatum& Y, const LONG X) const { // operator|
       return (LONG)(*getLong(Y) | X);
   }; // LONG SlipLongOp::bor (const SlipDatum& Y, const LONG X) const
   LONG SlipLongOp::bxor(const SlipDatum& Y, const LONG X) const { // operator^
       return (LONG)(*getLong(Y) ^ X);
   }; // LONG SlipLongOp::bxor(const SlipDatum& Y, const LONG X) const

          /*************************************************
           *               Simple Assignment               *
          *************************************************/
   SlipDatum& SlipLongOp::asgn(SlipDatum& Y, const SlipDatum& X) const {        // operator+=
      *getLong(Y) = (DOUBLE)X;
      return Y;
   }; // SlipDatum& SlipLongOp::asgn(SlipDatum& Y, const SlipDatum& X) const

          /*************************************************
           *       Arithmetic Assignment operators         *
          *************************************************/
   SlipDatum& SlipLongOp::addAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator+=
      LONG num = *getLong(Y);
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
            num += (*getOperator(X))->cast_LONG(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1009, "SlipLongOp+=", "", "Operation not supported for LONG.", Y, X);
            break;
      }
      *getLong(Y) = num;
      return Y;
   }; // SlipDatum& SlipLongOp::addAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipLongOp::subAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator-=
      LONG num = *getLong(Y);
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
            num -= (*getOperator(X))->cast_LONG(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1010, "SlipLongOp-=", "", "Operation not supported for LONG.", Y, X);
            break;
      }
      *getLong(Y) = num;
      return Y;
   }; // SlipDatum& SlipLongOp::subAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipLongOp::mulAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator*=
      LONG num = *getLong(Y);
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
            num *= (*getOperator(X))->cast_LONG(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1011, "SlipLongOp*=", "", "Operation not supported for LONG.", Y, X);
            break;
      }
      *getLong(Y) = num;
      return Y;
   }; // SlipDatum& SlipLongOp::mulAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipLongOp::divAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator/=
      LONG num = *getLong(Y);
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
            num /= (*getOperator(X))->cast_LONG(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1012, "SlipLongOp/=", "", "Operation not supported for LONG.", Y, X);
            break;
      }
      *getLong(Y) = num;
      return Y;
   }; // SlipDatum& SlipLongOp::divAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipLongOp::modAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator%=
      LONG num = *getLong(Y);
      switch(X.getClassType()) {
         case eBOOL:
            num %= *getBool(X);
            break;
         case eCHAR:
            num %= *getChar(X);
            break;
         case eUCHAR:
            num %= *getUChar(X);
            break;
         case eLONG:
            num %= *getLong(X);
            break;
         case eULONG:
            num %= *getULong(X);
            break;
         case ePTR:
            num %= (*getOperator(X))->cast_LONG(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1013, "SlipLongOp%=", "", "Operation not supported for LONG.", Y, X);
            break;
      }
      *getLong(Y) = num;
      return Y;
   }; // SlipDatum& SlipLongOp::modAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipLongOp::shlAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator<<=
      LONG num = *getLong(Y);
      switch(X.getClassType()) {
         case eBOOL:
            num <<= *getBool(X);
            break;
         case eCHAR:
            num <<= *getChar(X);
            break;
         case eUCHAR:
            num <<= *getUChar(X);
            break;
         case eLONG:
            num <<= *getLong(X);
            break;
         case eULONG:
            num <<= *getULong(X);
            break;
         case ePTR:
            num <<= (*getOperator(X))->cast_LONG(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1014, "SlipLongOp<<=", "", "Operation not supported for LONG.", Y, X);
            break;
      }
      *getLong(Y) = num;
      return Y;
   }; // SlipDatum& SlipLongOp::shlAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipLongOp::shrAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator>>=
      LONG num = *getLong(Y);
      switch(X.getClassType()) {
         case eBOOL:
            num >>= *getBool(X);
            break;
         case eCHAR:
            num >>= *getChar(X);
            break;
         case eUCHAR:
            num >>= *getUChar(X);
            break;
         case eLONG:
            num >>= *getLong(X);
            break;
         case eULONG:
            num >>= *getULong(X);
            break;
         case ePTR:
            num >>= (*getOperator(X))->cast_LONG(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1015, "SlipLongOp>>=", "", "Operation not supported for LONG.", Y, X);
            break;
      }
      *getLong(Y) = num;
      return Y;
   }; // SlipDatum& SlipLongOp::shrAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipLongOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator&=
      LONG num = *getLong(Y);
      switch(X.getClassType()) {
         case eBOOL:
            num &= *getBool(X);
            break;
         case eCHAR:
            num &= *getChar(X);
            break;
         case eUCHAR:
            num &= *getUChar(X);
            break;
         case eLONG:
            num &= *getLong(X);
            break;
         case eULONG:
            num &= *getULong(X);
            break;
         case ePTR:
            num &= (*getOperator(X))->cast_LONG(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1016, "SlipLongOp&=", "", "Operation not supported for LONG.", Y, X);
            break;
      }
      *getLong(Y) = num;
      return Y;
   }; // SlipDatum& SlipLongOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipLongOp::borAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator|=
      LONG num = *getLong(Y);
      switch(X.getClassType()) {
         case eBOOL:
            num |= *getBool(X);
            break;
         case eCHAR:
            num |= *getChar(X);
            break;
         case eUCHAR:
            num |= *getUChar(X);
            break;
         case eLONG:
            num |= *getLong(X);
            break;
         case eULONG:
            num |= *getULong(X);
            break;
         case ePTR:
            num |= (*getOperator(X))->cast_LONG(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1019, "SlipLongOp|=", "", "Operation not supported for LONG.", Y, X);
            break;
      }
      *getLong(Y) = num;
      return Y;
   }; // SlipDatum& SlipLongOp::borAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipLongOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator^=
      LONG num = *getLong(Y);
      switch(X.getClassType()) {
         case eBOOL:
            num ^= *getBool(X);
            break;
         case eCHAR:
            num ^= *getChar(X);
            break;
         case eUCHAR:
            num ^= *getUChar(X);
            break;
         case eLONG:
            num ^= *getLong(X);
            break;
         case eULONG:
            num ^= *getULong(X);
            break;
         case ePTR:
            num ^= (*getOperator(X))->cast_LONG(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1017, "SlipLongOp^=", "", "Operation not supported for LONG.", Y, X);
            break;
      }
      *getLong(Y) = num;
      return Y;
   }; // SlipDatum& SlipLongOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const

   SlipDatum& SlipLongOp::addAsgn (SlipDatum& Y, const DOUBLE X) const { // operator+=
       *getLong(Y) += X;
       return Y;
   }; // SlipDatum& SlipLongOp::addAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipLongOp::subAsgn (SlipDatum& Y, const DOUBLE X) const { // operator-=
       *getLong(Y) -= X;
       return Y;
   }; // SlipDatum& SlipLongOp::subAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipLongOp::mulAsgn (SlipDatum& Y, const DOUBLE X) const { // operator*=
       *getLong(Y) *= X;
       return Y;
   }; // SlipDatum& SlipLongOp::mulAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipLongOp::divAsgn (SlipDatum& Y, const DOUBLE X) const { // operator/=
       *getLong(Y) /= X;
       return Y;
   }; // SlipDatum& SlipLongOp::divAsgn (SlipDatum& Y, const DOUBLE X) const

   SlipDatum& SlipLongOp::addAsgn (SlipDatum& Y, const ULONG X) const { // operator+=
       *getLong(Y) += X;
       return Y;
   }; // SlipDatum& SlipLongOp::addAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipLongOp::subAsgn (SlipDatum& Y, const ULONG X) const { // operator-=
       *getLong(Y) -= X;
       return Y;
   }; // SlipDatum& SlipLongOp::subAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipLongOp::mulAsgn (SlipDatum& Y, const ULONG X) const { // operator*=
       *getLong(Y) *= X;
       return Y;
   }; // SlipDatum& SlipLongOp::mulAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipLongOp::divAsgn (SlipDatum& Y, const ULONG X) const { // operator/=
       *getLong(Y) /= X;
       return Y;
   }; // SlipDatum& SlipLongOp::divAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipLongOp::modAsgn (SlipDatum& Y, const ULONG X) const { // operator%=
       *getLong(Y) %= X;
       return Y;
   }; // SlipDatum& SlipLongOp::modAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipLongOp::shlAsgn (SlipDatum& Y, const ULONG X) const { // operator<<=
       *getLong(Y) <<= X;
       return Y;
   }; // SlipDatum& SlipLongOp::shlAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipLongOp::shrAsgn (SlipDatum& Y, const ULONG X) const { // operator>>=
       *getLong(Y) >>= X;
       return Y;
   }; // SlipDatum& SlipLongOp::shrAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipLongOp::bandAsgn(SlipDatum& Y, const ULONG X) const { // operator&=
       *getLong(Y) &= X;
       return Y;
   }; // SlipDatum& SlipLongOp::bandAsgn(SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipLongOp::borAsgn (SlipDatum& Y, const ULONG X) const { // operator|=
       *getLong(Y) |= X;
       return Y;
   }; // SlipDatum& SlipLongOp::borAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipLongOp::bxorAsgn(SlipDatum& Y, const ULONG X) const { // operator^=
       *getLong(Y) ^= X;
       return Y;
   }; // SlipDatum& SlipLongOp::bxorAsgn(SlipDatum& Y, const ULONG X) const

   SlipDatum& SlipLongOp::addAsgn (SlipDatum& Y, const LONG X) const { // operator+=
       *getLong(Y) += X;
       return Y;
   }; // SlipDatum& SlipLongOp::addAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipLongOp::subAsgn (SlipDatum& Y, const LONG X) const { // operator-=
       *getLong(Y) -= X;
       return Y;
   }; // SlipDatum& SlipLongOp::subAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipLongOp::mulAsgn (SlipDatum& Y, const LONG X) const { // operator*=
       *getLong(Y) *= X;
       return Y;
   }; // SlipDatum& SlipLongOp::mulAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipLongOp::divAsgn (SlipDatum& Y, const LONG X) const { // operator/=
       *getLong(Y) /= X;
       return Y;
   }; // SlipDatum& SlipLongOp::divAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipLongOp::modAsgn (SlipDatum& Y, const LONG X) const { // operator%=
       *getLong(Y) %= X;
       return Y;
   }; // SlipDatum& SlipLongOp::modAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipLongOp::shlAsgn (SlipDatum& Y, const LONG X) const { // operator<<=
       *getLong(Y) <<= X;
       return Y;
   }; // SlipDatum& SlipLongOp::shlAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipLongOp::shrAsgn (SlipDatum& Y, const LONG X) const { // operator>>=
       *getLong(Y) >>= X;
       return Y;
   }; // SlipDatum& SlipLongOp::shrAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipLongOp::bandAsgn(SlipDatum& Y, const LONG X) const { // operator&=
       *getLong(Y) &= X;
       return Y;
   }; // SlipDatum& SlipLongOp::bandAsgn(SlipDatum& Y, const LONG X) const
   SlipDatum& SlipLongOp::borAsgn (SlipDatum& Y, const LONG X) const { // operator|=
       *getLong(Y) |= X;
       return Y;
   }; // SlipDatum& SlipLongOp::borAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipLongOp::bxorAsgn(SlipDatum& Y, const LONG X) const { // operator^=
       *getLong(Y) ^= X;
       return Y;
   }; // SlipDatum& SlipLongOp::bxorAsgn(SlipDatum& Y, const LONG X) const

          /*************************************************
           *               Logical operators               *
          *************************************************/
   bool SlipLongOp::notEqual (const SlipDatum& Y, const SlipDatum& X) const { // operator!=
      bool flag = true;
      LONG num = *getLong(Y);
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
            flag = num != (LONG)*getULong(X);
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
   }; // bool SlipLongOp::notEqual (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipLongOp::lessThan (const SlipDatum& Y, const SlipDatum& X) const { // operator<
      bool flag = true;
      LONG num = *getLong(Y);
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
            flag = num < (LONG)*getULong(X);
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
   }; // bool SlipLongOp::lessThan (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipLongOp::lessEqual (const SlipDatum& Y, const SlipDatum& X) const { // operator<=
      bool flag = true;
      LONG num = *getLong(Y);
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
            flag = num <= (LONG)*getULong(X);
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
   }; // bool SlipLongOp::lessEqual (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipLongOp::equal (const SlipDatum& Y, const SlipDatum& X) const { // operator==
      bool flag = false;
      LONG num = *getLong(Y);
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
            flag = num == (LONG)*getULong(X);
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
   }; // bool SlipLongOp::equal (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipLongOp::greatEqual(const SlipDatum& Y, const SlipDatum& X) const { // operator>=
      bool flag = true;
      LONG num = *getLong(Y);
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
            flag = num >= (LONG)*getULong(X);
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
   }; // bool SlipLongOp::greatEqual(const SlipDatum& Y, const SlipDatum& X) const
   bool SlipLongOp::greatThan (const SlipDatum& Y, const SlipDatum& X) const { // operator>
      bool flag = true;
      LONG num = *getLong(Y);
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
            flag = num > (LONG)*getULong(X);
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
   }; // bool SlipLongOp::greatThan (const SlipDatum& Y, const SlipDatum& X) const

   bool SlipLongOp::notEqual (const SlipDatum& Y, const DOUBLE X) const { // operator!=
       return *getLong(Y) != X;
   }; // bool SlipLongOp::notEqual (const SlipDatum& Y, const DOUBLE X) const
   bool SlipLongOp::lessThan (const SlipDatum& Y, const DOUBLE X) const { // operator<
       return *getLong(Y) < X;
   }; // bool SlipLongOp::lessThan (const SlipDatum& Y, const DOUBLE X) const
   bool SlipLongOp::lessEqual (const SlipDatum& Y, const DOUBLE X) const { // operator<=
       return *getLong(Y) <= X;
   }; // bool SlipLongOp::lessEqual (const SlipDatum& Y, const DOUBLE X) const
   bool SlipLongOp::equal (const SlipDatum& Y, const DOUBLE X) const { // operator==
       return *getLong(Y) == X;
   }; // bool SlipLongOp::equal (const SlipDatum& Y, const DOUBLE X) const
   bool SlipLongOp::greatEqual(const SlipDatum& Y, const DOUBLE X) const { // operator>=
       return *getLong(Y) >= X;
   }; // bool SlipLongOp::greatEqual(const SlipDatum& Y, const DOUBLE X) const
   bool SlipLongOp::greatThan (const SlipDatum& Y, const DOUBLE X) const { // operator>
       return *getLong(Y) > X;
   }; // bool SlipLongOp::greatThan (const SlipDatum& Y, const DOUBLE X) const

   bool SlipLongOp::notEqual (const SlipDatum& Y, const ULONG X) const { // operator!=
       return *getLong(Y) != (LONG)X;
   }; // bool SlipLongOp::notEqual (const SlipDatum& Y, const ULONG X) const
   bool SlipLongOp::lessThan (const SlipDatum& Y, const ULONG X) const { // operator<
       return *getLong(Y) < (LONG)X;
   }; // bool SlipLongOp::lessThan (const SlipDatum& Y, const ULONG X) const
   bool SlipLongOp::lessEqual (const SlipDatum& Y, const ULONG X) const { // operator<=
       return *getLong(Y) <= (LONG)X;
   }; // bool SlipLongOp::lessEqual (const SlipDatum& Y, const ULONG X) const
   bool SlipLongOp::equal (const SlipDatum& Y, const ULONG X) const { // operator==
       return *getLong(Y) == (LONG)X;
   }; // bool SlipLongOp::equal (const SlipDatum& Y, const ULONG X) const
   bool SlipLongOp::greatEqual(const SlipDatum& Y, const ULONG X) const { // operator>=
       return *getLong(Y) >= (LONG)X;
   }; // bool SlipLongOp::greatEqual(const SlipDatum& Y, const ULONG X) const
   bool SlipLongOp::greatThan (const SlipDatum& Y, const ULONG X) const { // operator>
       return *getLong(Y) > (LONG)X;
   }; // bool SlipLongOp::greatThan (const SlipDatum& Y, const ULONG X) const

   bool SlipLongOp::notEqual (const SlipDatum& Y, const LONG X) const { // operator!=
       return *getLong(Y) != X;
   }; // bool SlipLongOp::notEqual (const SlipDatum& Y, const LONG X) const
   bool SlipLongOp::lessThan (const SlipDatum& Y, const LONG X) const { // operator<
       return *getLong(Y) < X;
   }; // bool SlipLongOp::lessThan (const SlipDatum& Y, const LONG X) const
   bool SlipLongOp::lessEqual (const SlipDatum& Y, const LONG X) const { // operator<=
       return *getLong(Y) <= X;
   }; // bool SlipLongOp::lessEqual (const SlipDatum& Y, const LONG X) const
   bool SlipLongOp::equal (const SlipDatum& Y, const LONG X) const { // operator==
       return *getLong(Y) == X;
   }; // bool SlipLongOp::equal (const SlipDatum& Y, const LONG X) const
   bool SlipLongOp::greatEqual(const SlipDatum& Y, const LONG X) const { // operator>=
       return *getLong(Y) >= X;
   }; // bool SlipLongOp::greatEqual(const SlipDatum& Y, const LONG X) const
   bool SlipLongOp::greatThan (const SlipDatum& Y, const LONG X) const { // operator>
       return *getLong(Y) > X;
   }; // bool SlipLongOp::greatThan (const SlipDatum& Y, const LONG X) const

   bool SlipLongOp::notEqual (const SlipDatum& Y, const PTR X) const { // operator!=
       return false;
   }; // bool SlipLongOp::notEqual (const SlipDatum& Y, const PTR X) const
   bool SlipLongOp::equal (const SlipDatum& Y, const PTR X) const { // operator==
       return false;
   }; // bool SlipLongOp::equal (const SlipDatum& Y, const PTR X) const

   bool SlipLongOp::notEqual (const SlipDatum& Y, const string& X) const { // operator!=
       return false;
   }; // bool SlipLongOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipLongOp::lessThan (const SlipDatum& Y, const string& X) const { // operator<
       return false;
   }; // bool SlipLongOp::equal (const SlipDatum& Y, const string& X) const
   bool SlipLongOp::lessEqual (const SlipDatum& Y, const string& X) const { // operator<=
       return false;
   }; // bool SlipLongOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipLongOp::equal (const SlipDatum& Y, const string& X) const { // operator==
       return false;
   }; // bool SlipLongOp::equal (const SlipDatum& Y, const string& X) const
   bool SlipLongOp::greatEqual (const SlipDatum& Y, const string& X) const { // operator>=
       return false;
   }; // bool SlipLongOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipLongOp::greatThan (const SlipDatum& Y, const string& X) const { // operator>
       return false;
   }; // bool SlipLongOp::equal (const SlipDatum& Y, const string& X) const

   string SlipLongOp::toString(const SlipCell& Y) const { // pretty print SLIP cell
      stringstream pretty;
      pretty << *getLong((SlipDatum&)Y);
      return pretty.str();
   }; // string SlipLongOp::toString(const SlipCell& Y) const

   string SlipLongOp::dump(const SlipCell& Y) const {
      stringstream pretty;
      pretty << "[long     ] " << dumpLink(Y) << " = " << toString(Y);
      return pretty.str();
   }; // string SlipLongOp::dump(const SlipCell& Y) const

   string SlipLongOp::write(const SlipCell& X) const {
      return toString(X);
   }; // string SlipLongOp::write(const SlipCell& X) const

}; // namespace slip