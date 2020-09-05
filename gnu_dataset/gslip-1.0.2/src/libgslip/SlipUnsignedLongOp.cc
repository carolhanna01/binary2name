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
 * @file    SlipUnsignedLongOp.cpp
 * @author  A. Schwarz
 * @date    June 04, 2012
 *
 */

 # include <string>
 # include <sstream>
 # include "SlipDatum.h"
 # include "SlipDef.h"
 # include "SlipErr.h"
 # include "SlipUnsignedLongOp.h"

using namespace slip;

namespace slip {

   /*********************************************************
    *                  Casting Operators                    *
   *********************************************************/

   bool SlipUnsignedLongOp::cast_bool(const SlipDatum& X) const {               // (bool) Y
       return (*getULong(X) == 0)? false: true;
   }; // bool SlipUnsignedLongOp::cast_bool(const SlipDatum& X) const
   UCHAR SlipUnsignedLongOp::cast_UCHAR(const SlipDatum& X) const {             // (UCHAR) X
       return (UCHAR)*getULong(X);
   }; // UCHAR SlipUnsignedLongOp::cast_UCHAR(const SlipDatum& X) const
   CHAR SlipUnsignedLongOp::cast_CHAR(const SlipDatum& X) const {               // (CHAR) X
       return (CHAR)*getULong(X);
   }; // CHAR SlipUnsignedLongOp::cast_CHAR(const SlipDatum& X) const
   ULONG SlipUnsignedLongOp::cast_ULONG(const SlipDatum& X) const {             // (ULONG) X
       return (ULONG)*getULong(X);
   }; // ULONG SlipUnsignedLongOp::cast_ULONG(const SlipDatum& X) const
   LONG SlipUnsignedLongOp::cast_LONG(const SlipDatum& X) const {               // (LONG) X
       return (LONG)*getULong(X);
   }; // LONG SlipUnsignedLongOp::cast_LONG(const SlipDatum& X) const
   DOUBLE SlipUnsignedLongOp::cast_DOUBLE(const SlipDatum& X) const {           // (DOUBLE) X
       return (DOUBLE)*getULong(X);
   }; // DOUBLE SlipUnsignedLongOp::cast_DOUBLE(const SlipDatum& X) const
   PTR SlipUnsignedLongOp::cast_PTR(const SlipDatum& Y) const {                 // (PTR) Y
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipBoolOp++", "", "Operation not supported for unsigned char.", Y);
      return NULL;
   }; // PTR SlipUnsignedLongOp::cast_PTR(const SlipDatum& Y) const

      /*********************************************************
       *                 Operator Overloading                  *
      *********************************************************/

          /*************************************************
           *                Unary operators                *
          *************************************************/
   ULONG SlipUnsignedLongOp::bitNOT (const SlipDatum& X) const{ // operator~
      return ~(ULONG)*getULong(X);
   }; // ULONG SlipUnsignedLongOp::bitNOT (const SlipDatum& X) const
   bool SlipUnsignedLongOp::logNOT (const SlipDatum& X) const{ // operator!
      return !(bool)*getULong(X);
   }; // bool SlipUnsignedLongOp::logNOT (const SlipDatum& X) const
   SlipDatum SlipUnsignedLongOp::plus (const SlipDatum& X) const{ // operator+
       return SlipDatum((ULONG)(*getULong(X)));
   }; // SlipDatum& SlipUnsignedLongOp::plus (const SlipDatum& X) const
   SlipDatum SlipUnsignedLongOp::minus (const SlipDatum& X) const{ // operator-
       return SlipDatum(-(ULONG)(*getULong(X)));
   }; // SlipDatum SlipUnsignedLongOp::minus (const SlipDatum& X) const
   SlipDatum& SlipUnsignedLongOp::postInc (SlipDatum& X) const{ // operator++
       (*getULong(X))++;
       return X;
   }; // SlipDatum& SlipUnsignedLongOp::postInc (SlipDatum& X) const
   SlipDatum& SlipUnsignedLongOp::postDec (SlipDatum& X) const{ // operator--
       (*getULong(X))--;
       return X;
   }; // SlipDatum& SlipUnsignedLongOp::postDec (SlipDatum& X) const
   SlipDatum& SlipUnsignedLongOp::preInc (SlipDatum& X) const{ // ++operator
       ++(*getULong(X));
       return X;
   }; // SlipDatum& SlipUnsignedLongOp::preInc (SlipDatum& X) const
   SlipDatum& SlipUnsignedLongOp::preDec (SlipDatum& X) const{ // --operator
       --(*getULong(X));
       return X;
   }; // SlipDatum& SlipUnsignedLongOp::preDec (SlipDatum& X) const

          /*************************************************
           *                Binary operators               *
          *************************************************/
   SlipDatum SlipUnsignedLongOp::add(const SlipDatum& Y, const SlipDatum& X) const{ // operator+
      return (*getOperator(X))->add(X, (ULONG)*getULong(Y));
   }; // SlipDatum SlipUnsignedLongOp::add(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipUnsignedLongOp::sub(const SlipDatum& Y, const SlipDatum& X) const{ // operator-
      return (*getOperator(Y))->subUOP((ULONG)*getULong(Y), X);
   }; // SlipDatum SlipUnsignedLongOp::sub(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipUnsignedLongOp::mul(const SlipDatum& Y, const SlipDatum& X) const{ // operator*
      return (*getOperator(X))->mul(X, (ULONG)*getULong(Y));
   }; // SlipDatum SlipUnsignedLongOp::mul(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipUnsignedLongOp::div(const SlipDatum& Y, const SlipDatum& X) const{ // operator/
      return (*getOperator(Y))-> divUOP((ULONG)*getULong(Y), X);
   }; // SlipDatum SlipUnsignedLongOp::div(const SlipDatum& Y, const SlipDatum& X) const
   LONG SlipUnsignedLongOp::mod(const SlipDatum& Y, const SlipDatum& X) const{ // operator%
      return (*getOperator(Y))->modUOP((ULONG)*getULong(Y), X);
   }; // SlipDatum SlipUnsignedLongOp::mod(const SlipDatum& Y, const SlipDatum& X) const

   SlipDatum SlipUnsignedLongOp::add(const SlipDatum& Y, const DOUBLE X) const{ // operator+
       return SlipDatum((*getULong(Y) + X));
   }; // SlipDatum SlipUnsignedLongOp::add(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipUnsignedLongOp::sub(const SlipDatum& Y, const DOUBLE X) const{ // operator-
       return SlipDatum((*getULong(Y) - X));
   }; // SlipDatum SlipUnsignedLongOp::sub(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipUnsignedLongOp::mul(const SlipDatum& Y, const DOUBLE X) const{ // operator*
       return SlipDatum((*getULong(Y) * X));
   }; // SlipDatum SlipUnsignedLongOp::mul(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipUnsignedLongOp::div(const SlipDatum& Y, const DOUBLE X) const{ // operator/
       return SlipDatum((*getULong(Y) / X));
   }; // SlipDatum SlipUnsignedLongOp::div(const SlipDatum& Y, const DOUBLE X) const

   SlipDatum SlipUnsignedLongOp::add(const SlipDatum& Y, const ULONG X) const{ // operator+
       return SlipDatum((*getULong(Y) + X));
   }; // SlipDatum SlipUnsignedLongOp::add(const SlipDatum& Y, const ULONG X) const

   SlipDatum SlipUnsignedLongOp::sub(const SlipDatum& Y, const ULONG X) const{ // operator-
       return SlipDatum((*getULong(Y) - X));
   }; // SlipDatum SlipUnsignedLongOp::sub(const SlipDatum& Y, const ULONG X) const
   SlipDatum SlipUnsignedLongOp::mul(const SlipDatum& Y, const ULONG X) const{ // operator*
       return SlipDatum((*getULong(Y) * X));
   }; // SlipDatum SlipUnsignedLongOp::mul(const SlipDatum& Y, const ULONG X) const
   SlipDatum SlipUnsignedLongOp::div(const SlipDatum& Y, const ULONG X) const{ // operator/
       return SlipDatum((*getULong(Y) / X));
   }; // SlipDatum SlipUnsignedLongOp::div(const SlipDatum& Y, const ULONG X) const
   LONG       SlipUnsignedLongOp::mod(const SlipDatum& Y, const ULONG X) const{ // operator%
       return SlipDatum((*getULong(Y) % X));
   }; // SlipDatum SlipUnsignedLongOp::mod(const SlipDatum& Y, const ULONG X) const

   SlipDatum SlipUnsignedLongOp::add(const SlipDatum& Y, const LONG X) const{ // operator+
       return SlipDatum((*getULong(Y) + X));
   }; // SlipDatum SlipUnsignedLongOp::add(const SlipDatum& Y, const LONG X) const

   SlipDatum SlipUnsignedLongOp::sub(const SlipDatum& Y, const LONG X) const{ // operator-
       return SlipDatum((*getULong(Y) - X));
   }; // SlipDatum SlipUnsignedLongOp::sub(const SlipDatum& Y, const LONG X) const
   SlipDatum SlipUnsignedLongOp::mul(const SlipDatum& Y, const LONG X) const{ // operator*
       return SlipDatum((*getULong(Y) * X));
   }; // SlipDatum SlipUnsignedLongOp::mul(const SlipDatum& Y, const LONG X) const
   SlipDatum SlipUnsignedLongOp::div(const SlipDatum& Y, const LONG X) const{ // operator/
       return SlipDatum((*getULong(Y) / X));
   }; // SlipDatum SlipUnsignedLongOp::div(const SlipDatum& Y, const LONG X) const
   LONG       SlipUnsignedLongOp::mod(const SlipDatum& Y, const LONG X) const{ // operator%
       return SlipDatum((*getULong(Y) % X));
   }; // SlipDatum SlipUnsignedLongOp::mod(const SlipDatum& Y, const LONG X) const

          /*************************************************
           *                Bit operators                  *
          *************************************************/
   LONG  SlipUnsignedLongOp::shl (const SlipDatum& Y, const SlipDatum& X) const{ // operator<<
      return (*getOperator(Y))->shlOP((LONG)*getULong(Y), X);
   }; // LONG  SlipUnsignedLongOp::shl (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipUnsignedLongOp::shr (const SlipDatum& Y, const SlipDatum& X) const{ // operator>>
      return (*getOperator(Y))->shrOP((LONG)*getULong(Y), X);
   }; // LONG  SlipUnsignedLongOp::shr (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipUnsignedLongOp::band(const SlipDatum& Y, const SlipDatum& X) const{ // operator&
      return (*getOperator(X))->band(X, (LONG)*getULong(Y));
   }; // LONG  SlipUnsignedLongOp::band(const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipUnsignedLongOp::bor (const SlipDatum& Y, const SlipDatum& X) const{ // operator|
      return (*getOperator(X))->bor(X, (LONG)*getULong(Y));
   }; // LONG  SlipUnsignedLongOp::bor (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipUnsignedLongOp::bxor(const SlipDatum& Y, const SlipDatum& X) const{ // operator^
      return (*getOperator(X))->bxor(X, (LONG)*getULong(Y));
   }; // LONG  SlipUnsignedLongOp::bxor(const SlipDatum& Y, const SlipDatum& X) const

   LONG SlipUnsignedLongOp::shl (const SlipDatum& Y, const ULONG X) const{ // operator<<
       return (LONG)(*getULong(Y) << X);
   }; // LONG SlipUnsignedLongOp::shl (const SlipDatum& Y, const ULONG X) const
   LONG SlipUnsignedLongOp::shr (const SlipDatum& Y, const ULONG X) const{ // operator>>
       return (LONG)(*getULong(Y) >> X);
   }; // LONG SlipUnsignedLongOp::shr (const SlipDatum& Y, const ULONG X) const
   LONG SlipUnsignedLongOp::band(const SlipDatum& Y, const ULONG X) const{ // operator&
       return (LONG)(*getULong(Y) & X);
   }; // LONG SlipUnsignedLongOp::band(const SlipDatum& Y, const ULONG X) const
   LONG SlipUnsignedLongOp::bor (const SlipDatum& Y, const ULONG X) const{ // operator|
       return (LONG)(*getULong(Y) | X);
   }; // LONG SlipUnsignedLongOp::bor (const SlipDatum& Y, const ULONG X) const
   LONG SlipUnsignedLongOp::bxor(const SlipDatum& Y, const ULONG X) const{ // operator^
       return (LONG)(*getULong(Y) ^ X);
   }; // LONG SlipUnsignedLongOp::bxor(const SlipDatum& Y, const ULONG X) const

   LONG SlipUnsignedLongOp::shl (const SlipDatum& Y, const LONG X) const{ // operator<<
       return (LONG)(*getULong(Y) << X);
   }; // LONG SlipUnsignedLongOp::shl (const SlipDatum& Y, const LONG X) const
   LONG SlipUnsignedLongOp::shr (const SlipDatum& Y, const LONG X) const{ // operator>>
       return (LONG)(*getULong(Y) >> X);
   }; // LONG SlipUnsignedLongOp::shr (const SlipDatum& Y, const LONG X) const
   LONG SlipUnsignedLongOp::band(const SlipDatum& Y, const LONG X) const{ // operator&
       return (LONG)(*getULong(Y) & X);
   }; // LONG SlipUnsignedLongOp::band(const SlipDatum& Y, const LONG X) const
   LONG SlipUnsignedLongOp::bor (const SlipDatum& Y, const LONG X) const{ // operator|
       return (LONG)(*getULong(Y) | X);
   }; // LONG SlipUnsignedLongOp::bor (const SlipDatum& Y, const LONG X) const
   LONG SlipUnsignedLongOp::bxor(const SlipDatum& Y, const LONG X) const{ // operator^
       return (LONG)(*getULong(Y) ^ X);
   }; // LONG SlipUnsignedLongOp::bxor(const SlipDatum& Y, const LONG X) const

          /*************************************************
           *               Simple Assignment               *
          *************************************************/
   SlipDatum& SlipUnsignedLongOp::asgn(SlipDatum& Y, const SlipDatum& X) const {        // operator+=
      *getULong(Y) = (ULONG)X;
      return Y;
   }; // SlipDatum& SlipUnsignedLongOp::asgn(SlipDatum& Y, const SlipDatum& X) const

          /*************************************************
           *       Arithmetic Assignment operators         *
          *************************************************/
   SlipDatum& SlipUnsignedLongOp::addAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator+=
      ULONG num = *getULong(Y);
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
            num += (*getOperator(X))->cast_ULONG(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1009, "SlipUnsignedLongOp+=", "", "Operation not supported for ULONG.", Y, X);
            break;
      }
      *getULong(Y) = num;
      return Y;
   }; // SlipDatum& SlipUnsignedLongOp::addAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipUnsignedLongOp::subAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator-=
      ULONG num = *getULong(Y);
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
            num -= (*getOperator(X))->cast_ULONG(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1010, "SlipUnsignedLongOp-=", "", "Operation not supported for ULONG.", Y, X);
            break;
      }
      *getULong(Y) = num;
      return Y;
   }; // SlipDatum& SlipUnsignedLongOp::subAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipUnsignedLongOp::mulAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator*=
      ULONG num = *getULong(Y);
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
            num *= (*getOperator(X))->cast_ULONG(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1011, "SlipUnsignedLongOp*=", "", "Operation not supported for ULONG.", Y, X);
            break;
      }
      *getULong(Y) = num;
      return Y;
   }; // SlipDatum& SlipUnsignedLongOp::mulAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipUnsignedLongOp::divAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator/=
      ULONG num = *getULong(Y);
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
            num /= (*getOperator(X))->cast_ULONG(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1012, "SlipUnsignedLongOp/=", "", "Operation not supported for ULONG.", Y, X);
            break;
      }
      *getULong(Y) = num;
      return Y;
   }; // SlipDatum& SlipUnsignedLongOp::divAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipUnsignedLongOp::modAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator%=
      ULONG num = *getULong(Y);
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
            num %= (*getOperator(X))->cast_ULONG(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1013, "SlipUnsignedLongOp%=", "", "Operation not supported for ULONG.", Y, X);
            break;
      }
      *getULong(Y) = num;
      return Y;
   }; // SlipDatum& SlipUnsignedLongOp::modAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipUnsignedLongOp::shlAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator<<=
      ULONG num = *getULong(Y);
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
            num <<= (*getOperator(X))->cast_ULONG(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1014, "SlipUnsignedLongOp<<=", "", "Operation not supported for ULONG.", Y, X);
            break;
      }
      *getULong(Y) = num;
      return Y;
   }; // SlipDatum& SlipUnsignedLongOp::shlAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipUnsignedLongOp::shrAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator>>=
      ULONG num = *getULong(Y);
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
            num >>= (*getOperator(X))->cast_ULONG(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1015, "SlipUnsignedLongOp>>-", "", "Operation not supported for ULONG.", Y, X);
            break;
      }
      *getULong(Y) = num;
      return Y;
   }; // SlipDatum& SlipUnsignedLongOp::shrAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipUnsignedLongOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator&=
      ULONG num = *getULong(Y);
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
            num &= (*getOperator(X))->cast_ULONG(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1016, "SlipUnsignedLongOp&=", "", "Operation not supported for ULONG.", Y, X);
            break;
      }
      *getULong(Y) = num;
      return Y;
   }; // SlipDatum& SlipUnsignedLongOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipUnsignedLongOp::borAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator|=
      ULONG num = *getULong(Y);
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
            num |= (*getOperator(X))->cast_ULONG(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1019, "SlipUnsignedLongOp|=", "", "Operation not supported for ULONG.", Y, X);
            break;
      }
      *getULong(Y) = num;
      return Y;
   }; // SlipDatum& SlipUnsignedLongOp::borAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipUnsignedLongOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator^=
      ULONG num = *getULong(Y);
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
            num ^= (*getOperator(X))->cast_ULONG(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1017, "SlipUnsignedLongOp^=", "", "Operation not supported for ULONG.", Y, X);
            break;
      }
      *getULong(Y) = num;
      return Y;
   }; // SlipDatum& SlipUnsignedLongOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const

   SlipDatum& SlipUnsignedLongOp::addAsgn (SlipDatum& Y, const DOUBLE X) const { // operator+=
       *getULong(Y) += X;
       return Y;
   }; // SlipDatum& SlipUnsignedLongOp::addAsgn (SlipDatum& Y, const DOUBLE X)
   SlipDatum& SlipUnsignedLongOp::subAsgn (SlipDatum& Y, const DOUBLE X) const { // operator-=
       *getULong(Y) -= X;
       return Y;
   }; // SlipDatum& SlipUnsignedLongOp::subAsgn (SlipDatum& Y, const DOUBLE X)
   SlipDatum& SlipUnsignedLongOp::mulAsgn (SlipDatum& Y, const DOUBLE X) const { // operator*=
       *getULong(Y) *= X;
       return Y;
   }; // SlipDatum& SlipUnsignedLongOp::mulAsgn (SlipDatum& Y, const DOUBLE X)
   SlipDatum& SlipUnsignedLongOp::divAsgn (SlipDatum& Y, const DOUBLE X) const { // operator/=
       *getULong(Y) /= X;
       return Y;
   }; // SlipDatum& SlipUnsignedLongOp::divAsgn (SlipDatum& Y, const DOUBLE X)

   SlipDatum& SlipUnsignedLongOp::addAsgn (SlipDatum& Y, const ULONG X) const { // operator+=
       *getULong(Y) += X;
       return Y;
   }; // SlipDatum& SlipUnsignedLongOp::addAsgn (SlipDatum& Y, const ULONG X)
   SlipDatum& SlipUnsignedLongOp::subAsgn (SlipDatum& Y, const ULONG X) const { // operator-=
       *getULong(Y) -= X;
       return Y;
   }; // SlipDatum& SlipUnsignedLongOp::subAsgn (SlipDatum& Y, const ULONG X)
   SlipDatum& SlipUnsignedLongOp::mulAsgn (SlipDatum& Y, const ULONG X) const { // operator*=
       *getULong(Y) *= X;
       return Y;
   }; // SlipDatum& SlipUnsignedLongOp::mulAsgn (SlipDatum& Y, const ULONG X)
   SlipDatum& SlipUnsignedLongOp::divAsgn (SlipDatum& Y, const ULONG X) const { // operator/=
       *getULong(Y) /= X;
       return Y;
   }; // SlipDatum& SlipUnsignedLongOp::divAsgn (SlipDatum& Y, const ULONG X)
   SlipDatum& SlipUnsignedLongOp::modAsgn (SlipDatum& Y, const ULONG X) const { // operator%=
       *getULong(Y) %= X;
       return Y;
   }; // SlipDatum& SlipUnsignedLongOp::modAsgn (SlipDatum& Y, const ULONG X)
   SlipDatum& SlipUnsignedLongOp::shlAsgn (SlipDatum& Y, const ULONG X) const { // operator<<=
       *getULong(Y) <<= X;
       return Y;
   }; // SlipDatum& SlipUnsignedLongOp::shlAsgn (SlipDatum& Y, const ULONG X)
   SlipDatum& SlipUnsignedLongOp::shrAsgn (SlipDatum& Y, const ULONG X) const { // operator>>=
       *getULong(Y) >>= X;
       return Y;
   }; // SlipDatum& SlipUnsignedLongOp::shrAsgn (SlipDatum& Y, const ULONG X)
   SlipDatum& SlipUnsignedLongOp::bandAsgn(SlipDatum& Y, const ULONG X) const { // operator&=
       *getULong(Y) &= X;
       return Y;
   }; // SlipDatum& SlipUnsignedLongOp::bandAsgn(SlipDatum& Y, const ULONG X)
   SlipDatum& SlipUnsignedLongOp::borAsgn (SlipDatum& Y, const ULONG X) const { // operator|=
       *getULong(Y) |= X;
       return Y;
   }; // SlipDatum& SlipUnsignedLongOp::borAsgn (SlipDatum& Y, const ULONG X)
   SlipDatum& SlipUnsignedLongOp::bxorAsgn(SlipDatum& Y, const ULONG X) const { // operator^=
       *getULong(Y) ^= X;
       return Y;
   }; // SlipDatum& SlipUnsignedLongOp::bxorAsgn(SlipDatum& Y, const ULONG X)

   SlipDatum& SlipUnsignedLongOp::addAsgn (SlipDatum& Y, const LONG X) const { // operator+=
       *getULong(Y) += X;
       return Y;
   }; // SlipDatum& SlipUnsignedLongOp::addAsgn (SlipDatum& Y, const LONG X)
   SlipDatum& SlipUnsignedLongOp::subAsgn (SlipDatum& Y, const LONG X) const { // operator-=
       *getULong(Y) -= X;
       return Y;
   }; // SlipDatum& SlipUnsignedLongOp::subAsgn (SlipDatum& Y, const LONG X)
   SlipDatum& SlipUnsignedLongOp::mulAsgn (SlipDatum& Y, const LONG X) const { // operator*=
       *getULong(Y) *= X;
       return Y;
   }; // SlipDatum& SlipUnsignedLongOp::mulAsgn (SlipDatum& Y, const LONG X)
   SlipDatum& SlipUnsignedLongOp::divAsgn (SlipDatum& Y, const LONG X) const { // operator/=
       *getULong(Y) /= X;
       return Y;
   }; // SlipDatum& SlipUnsignedLongOp::divAsgn (SlipDatum& Y, const LONG X)
   SlipDatum& SlipUnsignedLongOp::modAsgn (SlipDatum& Y, const LONG X) const { // operator%=
       *getULong(Y) %= X;
       return Y;
   }; // SlipDatum& SlipUnsignedLongOp::modAsgn (SlipDatum& Y, const LONG X)
   SlipDatum& SlipUnsignedLongOp::shlAsgn (SlipDatum& Y, const LONG X) const { // operator<<=
       *getULong(Y) <<= X;
       return Y;
   }; // SlipDatum& SlipUnsignedLongOp::shlAsgn (SlipDatum& Y, const LONG X)
   SlipDatum& SlipUnsignedLongOp::shrAsgn (SlipDatum& Y, const LONG X) const { // operator>>=
       *getULong(Y) >>= X;
       return Y;
   }; // SlipDatum& SlipUnsignedLongOp::shrAsgn (SlipDatum& Y, const LONG X)
   SlipDatum& SlipUnsignedLongOp::bandAsgn(SlipDatum& Y, const LONG X) const { // operator&=
       *getULong(Y) &= X;
       return Y;
   }; // SlipDatum& SlipUnsignedLongOp::bandAsgn(SlipDatum& Y, const LONG X)
   SlipDatum& SlipUnsignedLongOp::borAsgn (SlipDatum& Y, const LONG X) const { // operator|=
       *getULong(Y) |= X;
       return Y;
   }; // SlipDatum& SlipUnsignedLongOp::borAsgn (SlipDatum& Y, const LONG X)
   SlipDatum& SlipUnsignedLongOp::bxorAsgn(SlipDatum& Y, const LONG X) const { // operator^=
       *getULong(Y) ^= X;
       return Y;
   }; // SlipDatum& SlipUnsignedLongOp::bxorAsgn(SlipDatum& Y, const LONG X)

          /*************************************************
           *               Logical operators               *
          *************************************************/
   bool SlipUnsignedLongOp::notEqual (const SlipDatum& Y, const SlipDatum& X) const { // operator!=
      bool flag = true;
      ULONG num = *getULong(Y);
      switch(X.getClassType()) {
         case eBOOL:
            flag = num != *getBool(X);
            break;
         case eCHAR:
            flag = num != (unsigned)*getChar(X);
            break;
         case eUCHAR:
            flag = num != *getUChar(X);
            break;
         case eLONG:
            flag = num != (unsigned)*getLong(X);
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
   }; // bool SlipUnsignedLongOp::notEqual (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipUnsignedLongOp::lessThan (const SlipDatum& Y, const SlipDatum& X) const { // operator<
      bool flag = true;
      ULONG num = *getULong(Y);
      switch(X.getClassType()) {
         case eBOOL:
            flag = num < *getBool(X);
            break;
         case eCHAR:
            flag = num < (unsigned)*getChar(X);
            break;
         case eUCHAR:
            flag = num < *getUChar(X);
            break;
         case eLONG:
            flag = num < (unsigned)*getLong(X);
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
   }; // bool SlipUnsignedLongOp::lessThan (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipUnsignedLongOp::lessEqual (const SlipDatum& Y, const SlipDatum& X) const { // operator<=
      bool flag = true;
      ULONG num = *getULong(Y);
      switch(X.getClassType()) {
         case eBOOL:
            flag = num <= *getBool(X);
            break;
         case eCHAR:
            flag = num <= (unsigned)*getChar(X);
            break;
         case eUCHAR:
            flag = num <= *getUChar(X);
            break;
         case eLONG:
            flag = num <= (unsigned)*getLong(X);
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
   }; // bool SlipUnsignedLongOp::lessEqual (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipUnsignedLongOp::equal (const SlipDatum& Y, const SlipDatum& X) const { // operator==
      bool flag = false;
      ULONG num = *getULong(Y);
      switch(X.getClassType()) {
         case eBOOL:
            flag = num == *getBool(X);
            break;
         case eCHAR:
            flag = num == (unsigned)*getChar(X);
            break;
         case eUCHAR:
            flag = num == *getUChar(X);
            break;
         case eLONG:
            flag = num == (unsigned)*getLong(X);
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
   }; // bool SlipUnsignedLongOp::equal (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipUnsignedLongOp::greatEqual(const SlipDatum& Y, const SlipDatum& X) const { // operator>=
      bool flag = true;
      ULONG num = *getULong(Y);
      switch(X.getClassType()) {
         case eBOOL:
            flag = num >= *getBool(X);
            break;
         case eCHAR:
            flag = num >= (unsigned)*getChar(X);
            break;
         case eUCHAR:
            flag = num >= *getUChar(X);
            break;
         case eLONG:
            flag = num >= (unsigned)*getLong(X);
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
   }; // bool SlipUnsignedLongOp::greatEqual(const SlipDatum& Y, const SlipDatum& X) const
   bool SlipUnsignedLongOp::greatThan (const SlipDatum& Y, const SlipDatum& X) const { // operator>
      bool flag = true;
      ULONG num = *getULong(Y);
      switch(X.getClassType()) {
         case eBOOL:
            flag = num > *getBool(X);
            break;
         case eCHAR:
            flag = num > (unsigned)*getChar(X);
            break;
         case eUCHAR:
            flag = num > *getUChar(X);
            break;
         case eLONG:
            flag = num > (unsigned)*getLong(X);
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
   }; // bool SlipUnsignedLongOp::greatThan (const SlipDatum& Y, const SlipDatum& X) const

   bool SlipUnsignedLongOp::notEqual (const SlipDatum& Y, const DOUBLE X) const { // operator!=
       return *getULong(Y) != X;
   }; // bool SlipUnsignedLongOp::notEqual (const SlipDatum& Y, const DOUBLE X)
   bool SlipUnsignedLongOp::lessThan (const SlipDatum& Y, const DOUBLE X) const { // operator<
       return *getULong(Y) < X;
   }; // bool SlipUnsignedLongOp::lessThan (const SlipDatum& Y, const DOUBLE X)
   bool SlipUnsignedLongOp::lessEqual (const SlipDatum& Y, const DOUBLE X) const { // operator<=
       return *getULong(Y) <= X;
   }; // bool SlipUnsignedLongOp::lessEqual (const SlipDatum& Y, const DOUBLE X)
   bool SlipUnsignedLongOp::equal (const SlipDatum& Y, const DOUBLE X) const { // operator==
       return *getULong(Y) == X;
   }; // bool SlipUnsignedLongOp::equal (const SlipDatum& Y, const DOUBLE X)
   bool SlipUnsignedLongOp::greatEqual(const SlipDatum& Y, const DOUBLE X) const { // operator>=
       return *getULong(Y) >= X;
   }; // bool SlipUnsignedLongOp::greatEqual(const SlipDatum& Y, const DOUBLE X)
   bool SlipUnsignedLongOp::greatThan (const SlipDatum& Y, const DOUBLE X) const { // operator>
       return *getULong(Y) > X;
   }; // bool SlipUnsignedLongOp::greatThan (const SlipDatum& Y, const DOUBLE X)

   bool SlipUnsignedLongOp::notEqual (const SlipDatum& Y, const ULONG X) const { // operator!=
       return *getULong(Y) != X;
   }; // bool SlipUnsignedLongOp::notEqual (const SlipDatum& Y, const ULONG X)
   bool SlipUnsignedLongOp::lessThan (const SlipDatum& Y, const ULONG X) const { // operator<
       return *getULong(Y) < X;
   }; // bool SlipUnsignedLongOp::lessThan (const SlipDatum& Y, const ULONG X)
   bool SlipUnsignedLongOp::lessEqual (const SlipDatum& Y, const ULONG X) const { // operator<=
       return *getULong(Y) <= X;
   }; // bool SlipUnsignedLongOp::lessEqual (const SlipDatum& Y, const ULONG X)
   bool SlipUnsignedLongOp::equal (const SlipDatum& Y, const ULONG X) const { // operator==
       return *getULong(Y) == X;
   }; // bool SlipUnsignedLongOp::equal (const SlipDatum& Y, const ULONG X)
   bool SlipUnsignedLongOp::greatEqual(const SlipDatum& Y, const ULONG X) const { // operator>=
       return *getULong(Y) >= X;
   }; // bool SlipUnsignedLongOp::greatEqual(const SlipDatum& Y, const ULONG X)
   bool SlipUnsignedLongOp::greatThan (const SlipDatum& Y, const ULONG X) const { // operator>
       return *getULong(Y) > X;
   }; // bool SlipUnsignedLongOp::greatThan (const SlipDatum& Y, const ULONG X)

   bool SlipUnsignedLongOp::notEqual (const SlipDatum& Y, const LONG X) const { // operator!=
       return *getULong(Y) != (unsigned)X;
   }; // bool SlipUnsignedLongOp::notEqual (const SlipDatum& Y, const LONG X)
   bool SlipUnsignedLongOp::lessThan (const SlipDatum& Y, const LONG X) const { // operator<
       return *getULong(Y) < (unsigned)X;
   }; // bool SlipUnsignedLongOp::lessThan (const SlipDatum& Y, const LONG X)
   bool SlipUnsignedLongOp::lessEqual (const SlipDatum& Y, const LONG X) const { // operator<=
       return *getULong(Y) <= (unsigned)X;
   }; // bool SlipUnsignedLongOp::lessEqual (const SlipDatum& Y, const LONG X)
   bool SlipUnsignedLongOp::equal (const SlipDatum& Y, const LONG X) const { // operator==
       return *getULong(Y) == (unsigned)X;
   }; // bool SlipUnsignedLongOp::equal (const SlipDatum& Y, const LONG X)
   bool SlipUnsignedLongOp::greatEqual(const SlipDatum& Y, const LONG X) const { // operator>=
       return *getULong(Y) >= (unsigned)X;
   }; // bool SlipUnsignedLongOp::greatEqual(const SlipDatum& Y, const LONG X)
   bool SlipUnsignedLongOp::greatThan (const SlipDatum& Y, const LONG X) const { // operator>
       return *getULong(Y) > (unsigned)X;
   }; // bool SlipUnsignedLongOp::greatThan (const SlipDatum& Y, const LONG X)

   bool SlipUnsignedLongOp::notEqual (const SlipDatum& Y, const PTR X) const { // operator!=
       return false;
   }; // bool SlipUnsignedLongOp::notEqual (const SlipDatum& Y, const PTR X)
   bool SlipUnsignedLongOp::equal (const SlipDatum& Y, const PTR X) const { // operator==
       return false;
   }; // bool SlipUnsignedLongOp::equal (const SlipDatum& Y, const PTR X)

   bool SlipUnsignedLongOp::notEqual (const SlipDatum& Y, const string& X) const { // operator!=
       return false;
   }; // bool SlipUnsignedLongOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipUnsignedLongOp::lessThan (const SlipDatum& Y, const string& X) const { // operator<
       return false;
   }; // bool SlipUnsignedLongOp::equal (const SlipDatum& Y, const string& X) const
   bool SlipUnsignedLongOp::lessEqual (const SlipDatum& Y, const string& X) const { // operator<=
       return false;
   }; // bool SlipUnsignedLongOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipUnsignedLongOp::equal (const SlipDatum& Y, const string& X) const { // operator==
       return false;
   }; // bool SlipUnsignedLongOp::equal (const SlipDatum& Y, const string& X) const
   bool SlipUnsignedLongOp::greatEqual (const SlipDatum& Y, const string& X) const { // operator>=
       return false;
   }; // bool SlipUnsignedLongOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipUnsignedLongOp::greatThan (const SlipDatum& Y, const string& X) const { // operator>
       return false;
   }; // bool SlipUnsignedLongOp::equal (const SlipDatum& Y, const string& X) const

   string SlipUnsignedLongOp::toString(const SlipCell& Y) const { // pretty print SLIP cell
      stringstream pretty;
      pretty << *getULong((SlipDatum&)Y);
      return pretty.str();
   }; // string SlipUnsignedLongOp::toString(const SlipCell& Y) const

   string SlipUnsignedLongOp::dump(const SlipCell& Y) const {
      stringstream pretty;
      pretty << "[ulong    ] " << dumpLink(Y) << " = " << toString(Y);
      return pretty.str();
   }; // string SlipUnsignedLongOp::dump(const SlipCell& Y) const

   string SlipUnsignedLongOp::write(const SlipCell& X) const {
      return toString(X) + "U";
   }; // string SlipUnsignedLongOp::write(const SlipCell& X) const

}; // namespace slip