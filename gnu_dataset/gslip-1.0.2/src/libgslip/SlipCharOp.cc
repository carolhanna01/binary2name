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
 * @file    SlipCharOp.cpp
 * @author  A. Schwarz
 * @date    June 04, const 2012
 *
 */

 # include <iomanip>
 # include <string>
 # include <sstream>
 # include "SlipCharOp.h"
 # include "SlipDatum.h"
 # include "SlipDef.h"
 # include "SlipErr.h"

using namespace slip;

namespace slip {

   /*********************************************************
    *                  Casting Operators                    *
   *********************************************************/

   bool SlipCharOp::cast_bool(const SlipDatum& X) const {                       // (bool) X
       return (*getChar(X) == 0)? false: true;
   }; // bool SlipCharOp::cast_bool(const SlipDatum& X) const
   UCHAR SlipCharOp::cast_UCHAR(const SlipDatum& X) const {                     // (UCHAR) X
       return (UCHAR)*getChar(X);
   }; // UCHAR SlipCharOp::cast_UCHAR(SlipDatum& X) const
   CHAR SlipCharOp::cast_CHAR(const SlipDatum& X) const {                       // (CHAR) X
       return (CHAR)*getChar(X);
   }; // CHAR SlipCharOp::cast_CHAR(const SlipDatum& X) const
   ULONG SlipCharOp::cast_ULONG(const SlipDatum& X) const {                     // (ULONG) X
       return (ULONG)*getChar(X);
   }; // ULONG SlipCharOp::cast_ULONG(SlipDatum& X) const
   LONG SlipCharOp::cast_LONG(const SlipDatum& X) const {                       // (LONG) X
       return (LONG)*getChar(X);
   }; // LONG SlipCharOp::cast_LONG(const SlipDatum& X) const
   DOUBLE SlipCharOp::cast_DOUBLE(const SlipDatum& X) const {                   // (DOUBLE) X
       return (DOUBLE)*getChar(X);
   }; // DOUBLE SlipCharOp::cast_DOUBLE(const SlipDatum& X) const
   PTR SlipCharOp::cast_PTR(const SlipDatum& Y) const {                         // (PTR) Y
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipBoolOp++", "", "Operation not supported for char.", Y);
      return NULL;
   }; // PTR SlipCharOp::cast_PTR(const SlipDatum& Y) const

      /*********************************************************
       *                 Operator Overloading                  *
      *********************************************************/

          /*************************************************
           *                Unary operators                *
          *************************************************/
   ULONG SlipCharOp::bitNOT (const SlipDatum& X) const { // operator~
      return -(ULONG)(*getChar(X));
   }; // ULONG SlipCharOp::bitNOT (const SlipDatum& X) const
   bool SlipCharOp::logNOT (const SlipDatum& X) const { // operator!
      return (getChar(X) == 0)? true: false;
   }; // bool SlipCharOp::logNOT (const SlipDatum& X) const
   SlipDatum SlipCharOp::plus (const SlipDatum& X) const { // operator+
      return SlipDatum ((LONG)(*getChar(X)));
   }; // SlipDatum& SlipCharOp::plus (const SlipDatum& X) const
   SlipDatum SlipCharOp::minus (const SlipDatum& X) const { // operator-
      return SlipDatum (-(LONG)(*getChar(X)));
   }; // SlipDatum& SlipCharOp::minus (const SlipDatum& X) const
   SlipDatum& SlipCharOp::postInc (SlipDatum& X) const { // operator++
      (*getChar(X))++;
      return X;
   }; // SlipDatum& SlipCharOp::postInc (SlipDatum& X) const
   SlipDatum& SlipCharOp::postDec (SlipDatum& X) const { // operator--
      (*getChar(X))--;
      return X;
   }; // SlipDatum& SlipCharOp::postDec (SlipDatum& X) const
   SlipDatum& SlipCharOp::preInc (SlipDatum& X) const { // ++operator
      ++(*getChar(X));
      return X;
   }; // SlipDatum& SlipCharOp::preInc (SlipDatum& X) const
   SlipDatum& SlipCharOp::preDec (SlipDatum& X) const { // --operator
      --(*getChar(X));
      return X;
   }; // SlipDatum& SlipCharOp::preDec (SlipDatum& X) const

          /*************************************************
           *                Binary operators               *
          *************************************************/
   SlipDatum SlipCharOp::add(const SlipDatum& Y, const SlipDatum& X) const { // operator+
      return (*getOperator(X))->add(X, (LONG)*getChar(Y));
   }; // SlipDatum SlipCharOp::add(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipCharOp::sub(const SlipDatum& Y, const SlipDatum& X) const { // operator-
      return (*getOperator(Y))->subOP((LONG)*getChar(Y), X);
   }; // SlipDatum SlipCharOp::sub(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipCharOp::mul(const SlipDatum& Y, const SlipDatum& X) const { // operator*
      return (*getOperator(X))->mul(X, (LONG)*getChar(Y));
   }; // SlipDatum SlipCharOp::mul(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipCharOp::div(const SlipDatum& Y, const SlipDatum& X) const { // operator/
      return (*getOperator(Y))->divOP((LONG)*getChar(Y), X);
   }; // SlipDatum SlipCharOp::div(const SlipDatum& Y, const SlipDatum& X) const
   LONG SlipCharOp::mod(const SlipDatum& Y, const SlipDatum& X) const { // operator%
      return (*getOperator(Y))-> modOP((LONG)*getChar(Y), X);
   }; // SlipDatum SlipCharOp::mod(const SlipDatum& Y, const SlipDatum& X) const

   SlipDatum SlipCharOp::add(const SlipDatum& Y, const DOUBLE X) const { // operator+
       return SlipDatum((*getChar(Y) + X));
   }; // SlipDatum SlipCharOp::add(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipCharOp::sub(const SlipDatum& Y, const DOUBLE X) const { // operator-
       return SlipDatum((*getChar(Y) - X));
   }; // SlipDatum SlipCharOp::sub(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipCharOp::mul(const SlipDatum& Y, const DOUBLE X) const { // operator*
       return SlipDatum((*getChar(Y) * X));
   }; // SlipDatum SlipCharOp::mul(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipCharOp::div(const SlipDatum& Y, const DOUBLE X) const { // operator/
       return SlipDatum((*getChar(Y) / X));
   }; // SlipDatum SlipCharOp::div(const SlipDatum& Y, const DOUBLE X) const

   SlipDatum SlipCharOp::add(const SlipDatum& Y, const ULONG X) const { // operator+
       return SlipDatum((*getChar(Y) + X));
   }; // SlipDatum SlipCharOp::add(const SlipDatum& Y, const ULONG X) const

   SlipDatum SlipCharOp::sub(const SlipDatum& Y, const ULONG X) const { // operator-
       return SlipDatum((*getChar(Y) - X));
   }; // SlipDatum SlipCharOp::sub(const SlipDatum& Y, const ULONG X) const
   SlipDatum SlipCharOp::mul(const SlipDatum& Y, const ULONG X) const { // operator*
       return SlipDatum((*getChar(Y) * X));
   }; // SlipDatum SlipCharOp::mul(const SlipDatum& Y, const ULONG X) const
   SlipDatum SlipCharOp::div(const SlipDatum& Y, const ULONG X) const { // operator/
       return SlipDatum((*getChar(Y) / X));
   }; // SlipDatum SlipCharOp::div(const SlipDatum& Y, const ULONG X) const
   LONG       SlipCharOp::mod(const SlipDatum& Y, const ULONG X) const { // operator%
       return SlipDatum((*getChar(Y) % X));
   }; // SlipDatum SlipCharOp::mod(const SlipDatum& Y, const ULONG X) const

   SlipDatum SlipCharOp::add(const SlipDatum& Y, const LONG X) const { // operator+
       return SlipDatum((*getChar(Y) + X));
   }; // SlipDatum SlipCharOp::add(const SlipDatum& Y, const LONG X) const

   SlipDatum SlipCharOp::sub(const SlipDatum& Y, const LONG X) const { // operator-
       return SlipDatum((*getChar(Y) - X));
   }; // SlipDatum SlipCharOp::sub(const SlipDatum& Y, const LONG X) const
   SlipDatum SlipCharOp::mul(const SlipDatum& Y, const LONG X) const { // operator*
       return SlipDatum((*getChar(Y) * X));;
   }; // SlipDatum SlipCharOp::mul(const SlipDatum& Y, const LONG X) const
   SlipDatum SlipCharOp::div(const SlipDatum& Y, const LONG X) const { // operator/
       return SlipDatum((*getChar(Y) / X));
   }; // SlipDatum SlipCharOp::div(const SlipDatum& Y, const LONG X) const
   LONG       SlipCharOp::mod(const SlipDatum& Y, const LONG X) const { // operator%
       return SlipDatum((*getChar(Y) % X));
   }; // SlipDatum SlipCharOp::mod(const SlipDatum& Y, const LONG X) const

          /*************************************************
           *                Bit operators                  *
          *************************************************/

          /*************************************************
           *                Bit operators                  *
          *************************************************/
   LONG  SlipCharOp::shl (const SlipDatum& Y, const SlipDatum& X) const { // operator<<
      return (*getOperator(Y))->shlOP((LONG)*getChar(Y), X);
   }; // LONG  SlipCharOp::shl (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipCharOp::shr (const SlipDatum& Y, const SlipDatum& X) const { // operator>>
      return (*getOperator(Y))->shrOP((LONG)*getChar(Y), X);
   }; // LONG  SlipCharOp::shr (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipCharOp::band(const SlipDatum& Y, const SlipDatum& X) const { // operator&
      return (*getOperator(X))->band(X, (LONG)*getChar(Y));
   }; // LONG  SlipCharOp::band(const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipCharOp::bor (const SlipDatum& Y, const SlipDatum& X) const { // operator|
      return (*getOperator(X))->bor(X, (LONG)*getChar(Y));
   }; // LONG  SlipCharOp::bor (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipCharOp::bxor(const SlipDatum& Y, const SlipDatum& X) const { // operator^
      return (*getOperator(X))->bxor(X, (LONG)*getChar(Y));
   }; // LONG  SlipCharOp::bxor(const SlipDatum& Y, const SlipDatum& X) const

   LONG SlipCharOp::shl (const SlipDatum& Y, const ULONG X) const { // operator<<
       return (LONG)(*getChar(Y) << X);
   }; // LONG SlipCharOp::shl (const SlipDatum& Y, const ULONG X) const
   LONG SlipCharOp::shr (const SlipDatum& Y, const ULONG X) const { // operator>>
       return (LONG)(*getChar(Y) >> X);
   }; // LONG SlipCharOp::shr (const SlipDatum& Y, const ULONG X) const
   LONG SlipCharOp::band(const SlipDatum& Y, const ULONG X) const { // operator&
       return (LONG)(*getChar(Y) & X);
   }; // LONG SlipCharOp::band(const SlipDatum& Y, const ULONG X) const
   LONG SlipCharOp::bor (const SlipDatum& Y, const ULONG X) const { // operator|
       return (LONG)(*getChar(Y) | X);
   }; // LONG SlipCharOp::bor (const SlipDatum& Y, const ULONG X) const
   LONG SlipCharOp::bxor(const SlipDatum& Y, const ULONG X) const { // operator^
       return (LONG)(*getChar(Y) ^ X);
   }; // LONG SlipCharOp::bxor(const SlipDatum& Y, const ULONG X) const

   LONG SlipCharOp::shl (const SlipDatum& Y, const LONG X) const { // operator<<
       return (LONG)(*getChar(Y) << X);
   }; // LONG SlipCharOp::shl (const SlipDatum& Y, const LONG X) const
   LONG SlipCharOp::shr (const SlipDatum& Y, const LONG X) const { // operator>>
       return (LONG)(*getChar(Y) >> X);
   }; // LONG SlipCharOp::shr (const SlipDatum& Y, const LONG X) const
   LONG SlipCharOp::band(const SlipDatum& Y, const LONG X) const { // operator&
       return (LONG)(*getChar(Y) & X);
   }; // LONG SlipCharOp::band(const SlipDatum& Y, const LONG X) const
   LONG SlipCharOp::bor (const SlipDatum& Y, const LONG X) const { // operator|
       return (LONG)(*getChar(Y) | X);
   }; // LONG SlipCharOp::bor (const SlipDatum& Y, const LONG X) const
   LONG SlipCharOp::bxor(const SlipDatum& Y, const LONG X) const { // operator^
       return (LONG)(*getChar(Y) ^ X);
   }; // LONG SlipCharOp::bxor(const SlipDatum& Y, const LONG X) const

          /*************************************************
           *               Simple Assignment               *
          *************************************************/
   SlipDatum& SlipCharOp::asgn(SlipDatum& Y, const SlipDatum& X) const {        // operator+=
      *getChar(Y) = (CHAR)X;
      return Y;
   }; // SlipDatum& SlipCharOp::asgn(SlipDatum& Y, const SlipDatum& X) const

          /*************************************************
           *       Arithmetic Assignment operators         *
          *************************************************/
   SlipDatum& SlipCharOp::addAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator+=
      LONG num = *getChar(Y);
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
            num += (*getOperator(X))->cast_CHAR(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1009, "SlipCharOp+=", "", "Operation not supportedfor CHAR.", Y, X);
            break;
      }
      *getChar(Y) = num;
      return Y;
   }; // SlipDatum& SlipCharOp::addAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipCharOp::subAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator-=
      LONG num = *getChar(Y);
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
            num -= (*getOperator(X))->cast_CHAR(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1010, "SlipCharOp-=", "", "Operation not supportedfor CHAR.", Y, X);
            break;
      }
      *getChar(Y) = num;
      return Y;
   }; // SlipDatum& SlipCharOp::subAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipCharOp::mulAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator*=
      LONG num = *getChar(Y);
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
            num *= (*getOperator(X))->cast_CHAR(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1011, "SlipCharOp*=", "", "Operation not supportedfor CHAR.", Y, X);
            break;
      }
      *getChar(Y) = num;
      return Y;
   }; // SlipDatum& SlipCharOp::mulAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipCharOp::divAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator/=
      LONG num = *getChar(Y);
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
            num /= (*getOperator(X))->cast_CHAR(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1012, "SlipCharOp/=", "", "Operation not supportedfor CHAR.", Y, X);
            break;
      }
      *getChar(Y) = num;
      return Y;
   }; // SlipDatum& SlipCharOp::divAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipCharOp::modAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator%=
      LONG num = *getChar(Y);
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
            num %= (*getOperator(X))->cast_CHAR(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1013, "SlipCharOp%=", "", "Operation not supportedfor CHAR.", Y, X);
            break;
      }
      *getChar(Y) = num;
      return Y;
   }; // SlipDatum& SlipCharOp::modAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipCharOp::shlAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator<<=
      LONG num = *getChar(Y);
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
            num <<= (*getOperator(X))->cast_CHAR(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1014, "SlipCharOp<<=", "", "Operation not supportedfor CHAR.", Y, X);
            break;
      }
      *getChar(Y) = num;
      return Y;
   }; // SlipDatum& SlipCharOp::shlAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipCharOp::shrAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator>>=
      LONG num = *getChar(Y);
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
            num >>= (*getOperator(X))->cast_CHAR(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1015, "SlipCharOp>>=", "", "Operation not supportedfor CHAR.", Y, X);
            break;
      }
      *getChar(Y) = num;
      return Y;
   }; // SlipDatum& SlipCharOp::shrAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipCharOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator&=
      LONG num = *getChar(Y);
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
         default:
            postError(__FILE__, __LINE__, SlipErr::E1016, "SlipCharOp&=", "", "Operation not supportedfor CHAR.", Y, X);
            break;
      }
      *getChar(Y) = num;
      return Y;
   }; // SlipDatum& SlipCharOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipCharOp::borAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator|=
      LONG num = *getChar(Y);
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
            num |= (*getOperator(X))->cast_CHAR(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1019, "SlipCharOp|=", "", "Operation not supportedfor CHAR.", Y, X);
            break;
      }
      *getChar(Y) = num;
      return Y;
   }; // SlipDatum& SlipCharOp::borAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipCharOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator^=
      LONG num = *getChar(Y);
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
            num ^= (*getOperator(X))->cast_CHAR(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1017, "SlipCharOp^=", "", "Operation not supportedfor CHAR.", Y, X);
            break;
      }
      *getChar(Y) = num;
      return Y;
   }; // SlipDatum& SlipCharOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const

   SlipDatum& SlipCharOp::addAsgn (SlipDatum& Y, const DOUBLE X) const { // operator+=
       *getChar(Y) += X;
       return Y;
   }; // SlipDatum& SlipCharOp::addAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipCharOp::subAsgn (SlipDatum& Y, const DOUBLE X) const { // operator-=
       *getChar(Y) -= X;
       return Y;
   }; // SlipDatum& SlipCharOp::subAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipCharOp::mulAsgn (SlipDatum& Y, const DOUBLE X) const { // operator*=
       *getChar(Y) *= X;
       return Y;
   }; // SlipDatum& SlipCharOp::mulAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipCharOp::divAsgn (SlipDatum& Y, const DOUBLE X) const { // operator/=
       *getChar(Y) /= X;
       return Y;
   }; // SlipDatum& SlipCharOp::divAsgn (SlipDatum& Y, const DOUBLE X) const

   SlipDatum& SlipCharOp::addAsgn (SlipDatum& Y, const ULONG X) const { // operator+=
       *getChar(Y) += X;
       return Y;
   }; // SlipDatum& SlipCharOp::addAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipCharOp::subAsgn (SlipDatum& Y, const ULONG X) const { // operator-=
       *getChar(Y) -= X;
       return Y;
   }; // SlipDatum& SlipCharOp::subAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipCharOp::mulAsgn (SlipDatum& Y, const ULONG X) const { // operator*=
       *getChar(Y) *= X;
       return Y;
   }; // SlipDatum& SlipCharOp::mulAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipCharOp::divAsgn (SlipDatum& Y, const ULONG X) const { // operator/=
       *getChar(Y) /= X;
       return Y;
   }; // SlipDatum& SlipCharOp::divAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipCharOp::modAsgn (SlipDatum& Y, const ULONG X) const { // operator%=
       *getChar(Y) %= X;
       return Y;
   }; // SlipDatum& SlipCharOp::modAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipCharOp::shlAsgn (SlipDatum& Y, const ULONG X) const { // operator<<=
       *getChar(Y) <<= X;
       return Y;
   }; // SlipDatum& SlipCharOp::shlAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipCharOp::shrAsgn (SlipDatum& Y, const ULONG X) const { // operator>>=
       *getChar(Y) >>= X;
       return Y;
   }; // SlipDatum& SlipCharOp::shrAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipCharOp::bandAsgn(SlipDatum& Y, const ULONG X) const { // operator&=
       *getChar(Y) &= X;
       return Y;
   }; // SlipDatum& SlipCharOp::bandAsgn(SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipCharOp::borAsgn (SlipDatum& Y, const ULONG X) const { // operator|=
       *getChar(Y) |= X;
       return Y;
   }; // SlipDatum& SlipCharOp::borAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipCharOp::bxorAsgn(SlipDatum& Y, const ULONG X) const { // operator^=
       *getChar(Y) ^= X;
       return Y;
   }; // SlipDatum& SlipCharOp::bxorAsgn(SlipDatum& Y, const ULONG X) const

   SlipDatum& SlipCharOp::addAsgn (SlipDatum& Y, const LONG X) const { // operator+=
       *getChar(Y) += X;
       return Y;
   }; // SlipDatum& SlipCharOp::addAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipCharOp::subAsgn (SlipDatum& Y, const LONG X) const { // operator-=
       *getChar(Y) -= X;
       return Y;
   }; // SlipDatum& SlipCharOp::subAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipCharOp::mulAsgn (SlipDatum& Y, const LONG X) const { // operator*=
       *getChar(Y) *= X;
       return Y;
   }; // SlipDatum& SlipCharOp::mulAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipCharOp::divAsgn (SlipDatum& Y, const LONG X) const { // operator/=
       *getChar(Y) /= X;
       return Y;
   }; // SlipDatum& SlipCharOp::divAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipCharOp::modAsgn (SlipDatum& Y, const LONG X) const { // operator%=
       *getChar(Y) %= X;
       return Y;
   }; // SlipDatum& SlipCharOp::modAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipCharOp::shlAsgn (SlipDatum& Y, const LONG X) const { // operator<<=
       *getChar(Y) <<= X;
       return Y;
   }; // SlipDatum& SlipCharOp::shlAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipCharOp::shrAsgn (SlipDatum& Y, const LONG X) const { // operator>>=
       *getChar(Y) >>= X;
       return Y;
   }; // SlipDatum& SlipCharOp::shrAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipCharOp::bandAsgn(SlipDatum& Y, const LONG X) const { // operator&=
       *getChar(Y) &= X;
       return Y;
   }; // SlipDatum& SlipCharOp::bandAsgn(SlipDatum& Y, const LONG X) const
   SlipDatum& SlipCharOp::borAsgn (SlipDatum& Y, const LONG X) const { // operator|=
       *getChar(Y) |= X;
       return Y;
   }; // SlipDatum& SlipCharOp::borAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipCharOp::bxorAsgn(SlipDatum& Y, const LONG X) const { // operator^=
       *getChar(Y) ^= X;
       return Y;
   }; // SlipDatum& SlipCharOp::bxorAsgn(SlipDatum& Y, const LONG X) const

          /*************************************************
           *               Logical operators               *
          *************************************************/
   bool SlipCharOp::notEqual (const SlipDatum& Y, const SlipDatum& X) const { // operator!=
      bool flag = true;
      CHAR num = *getChar(Y);
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
           flag =  num != *getDouble(X);
            break;
         case ePTR:
            flag = (*getOperator(X))->notEqual(X, (LONG)num);
            break;
         default:
            flag = false;
            break;
      }
      return flag;
   }; // bool SlipCharOp::notEqual (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipCharOp::lessThan (const SlipDatum& Y, const SlipDatum& X) const { // operator<
      LONG num = *getChar(Y);
      bool flag = false;
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
           flag =  num < *getDouble(X);
            break;
         case ePTR:
            flag = (*getOperator(X))->greatThan(X, num);
            break;
         default:
            flag = false;
            break;
      }
      return flag;
   }; // bool SlipCharOp::lessThan (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipCharOp::lessEqual (const SlipDatum& Y, const SlipDatum& X) const { // operator<=
      LONG num = *getChar(Y);
      bool flag = false;
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
           flag =  num <= *getDouble(X);
            break;
         case ePTR:
            flag = (*getOperator(X))->greatEqual(X, num);
            break;
         default:
            flag = false;
            break;
      }
      return flag;
   }; // bool SlipCharOp::lessEqual (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipCharOp::equal (const SlipDatum& Y, const SlipDatum& X) const { // operator==
      LONG num = *getChar(Y);
      bool flag = false;
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
           flag =  num == *getDouble(X);
            break;
         case ePTR:
            flag = (*getOperator(X))->equal(X, num);
            break;
         default:
            flag = false;
            break;
      }
      return flag;
   }; // bool SlipCharOp::equal (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipCharOp::greatEqual(const SlipDatum& Y, const SlipDatum& X) const { // operator>=
      LONG num = *getChar(Y);
      bool flag = false;
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
           flag =  num >= *getDouble(X);
            break;
         case ePTR:
            flag = (*getOperator(X))->lessEqual(X, num);
            break;
         default:
            flag = false;
            break;
      }
      return flag;
   }; // bool SlipCharOp::greatEqual(const SlipDatum& Y, const SlipDatum& X) const
   bool SlipCharOp::greatThan (const SlipDatum& Y, const SlipDatum& X) const { // operator>
      LONG num = *getChar(Y);
      bool flag = false;
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
           flag =  num > *getDouble(X);
            break;
         case ePTR:
            flag = (*getOperator(X))->lessThan(X, num);
            break;
         default:
            flag = false;
            break;
      }
      return flag;
   }; // bool SlipCharOp::greatThan (const SlipDatum& Y, const SlipDatum& X) const

   bool SlipCharOp::notEqual (const SlipDatum& Y, const DOUBLE X) const { // operator!=
       return *getChar(Y) != X;
   }; // bool SlipCharOp::notEqual (const SlipDatum& Y, const DOUBLE X) const
   bool SlipCharOp::lessThan (const SlipDatum& Y, const DOUBLE X) const { // operator<
       return *getChar(Y) < X;
   }; // bool SlipCharOp::lessThan (const SlipDatum& Y, const DOUBLE X) const
   bool SlipCharOp::lessEqual (const SlipDatum& Y, const DOUBLE X) const { // operator<=
       return *getChar(Y) <= X;
   }; // bool SlipCharOp::lessEqual (const SlipDatum& Y, const DOUBLE X) const
   bool SlipCharOp::equal (const SlipDatum& Y, const DOUBLE X) const { // operator==
       return *getChar(Y) == X;
   }; // bool SlipCharOp::equal (const SlipDatum& Y, const DOUBLE X) const
   bool SlipCharOp::greatEqual(const SlipDatum& Y, const DOUBLE X) const { // operator>=
       return *getChar(Y) >= X;
   }; // bool SlipCharOp::greatEqual(const SlipDatum& Y, const DOUBLE X) const
   bool SlipCharOp::greatThan (const SlipDatum& Y, const DOUBLE X) const { // operator>
       return *getChar(Y) > X;
   }; // bool SlipCharOp::greatThan (const SlipDatum& Y, const DOUBLE X) const

   bool SlipCharOp::notEqual (const SlipDatum& Y, const ULONG X) const { // operator!=
       return *getChar(Y) != (LONG)X;
   }; // bool SlipCharOp::notEqual (const SlipDatum& Y, const ULONG X) const
   bool SlipCharOp::lessThan (const SlipDatum& Y, const ULONG X) const { // operator<
       return *getChar(Y) < (LONG)X;
   }; // bool SlipCharOp::lessThan (const SlipDatum& Y, const ULONG X) const
   bool SlipCharOp::lessEqual (const SlipDatum& Y, const ULONG X) const { // operator<=
       return *getChar(Y) <=(LONG) X;
   }; // bool SlipCharOp::lessEqual (const SlipDatum& Y, const ULONG X) const
   bool SlipCharOp::equal (const SlipDatum& Y, const ULONG X) const { // operator==
       return *getChar(Y) == (LONG)X;
   }; // bool SlipCharOp::equal (const SlipDatum& Y, const ULONG X) const
   bool SlipCharOp::greatEqual(const SlipDatum& Y, const ULONG X) const { // operator>=
       return *getChar(Y) >= (LONG)X;
   }; // bool SlipCharOp::greatEqual(const SlipDatum& Y, const ULONG X) const
   bool SlipCharOp::greatThan (const SlipDatum& Y, const ULONG X) const { // operator>
       return *getChar(Y) > (LONG)X;
   }; // bool SlipCharOp::greatThan (const SlipDatum& Y, const ULONG X) const

   bool SlipCharOp::notEqual (const SlipDatum& Y, const LONG X) const { // operator!=
       return *getChar(Y) != X;
   }; // bool SlipCharOp::notEqual (const SlipDatum& Y, const LONG X) const
   bool SlipCharOp::lessThan (const SlipDatum& Y, const LONG X) const { // operator<
       return *getChar(Y) < X;
   }; // bool SlipCharOp::lessThan (const SlipDatum& Y, const LONG X) const
   bool SlipCharOp::lessEqual (const SlipDatum& Y, const LONG X) const { // operator<=
       return *getChar(Y) <= X;
   }; // bool SlipCharOp::lessEqual (const SlipDatum& Y, const LONG X) const
   bool SlipCharOp::equal (const SlipDatum& Y, const LONG X) const { // operator==
       return *getChar(Y) == X;
   }; // bool SlipCharOp::equal (const SlipDatum& Y, const LONG X) const
   bool SlipCharOp::greatEqual(const SlipDatum& Y, const LONG X) const { // operator>=
       return *getChar(Y) >= X;
   }; // bool SlipCharOp::greatEqual(const SlipDatum& Y, const LONG X) const
   bool SlipCharOp::greatThan (const SlipDatum& Y, const LONG X) const { // operator>
       return *getChar(Y) > X;
   }; // bool SlipCharOp::greatThan (const SlipDatum& Y, const LONG X) const

   bool SlipCharOp::notEqual (const SlipDatum& Y, const PTR X) const { // operator!=
       return false;
   }; // bool SlipCharOp::notEqual (const SlipDatum& Y, const PTR X) const
   bool SlipCharOp::equal (const SlipDatum& Y, const PTR X) const { // operator==
       return false;
   }; // bool SlipCharOp::equal (const SlipDatum& Y, const PTR X) const

   bool SlipCharOp::notEqual (const SlipDatum& Y, const string& X) const { // operator!=
       return false;
   }; // bool SlipCharOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipCharOp::lessThan (const SlipDatum& Y, const string& X) const { // operator<
       return false;
   }; // bool SlipCharOp::equal (const SlipDatum& Y, const string& X) const
   bool SlipCharOp::lessEqual (const SlipDatum& Y, const string& X) const { // operator<=
       return false;
   }; // bool SlipCharOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipCharOp::equal (const SlipDatum& Y, const string& X) const { // operator==
       return false;
   }; // bool SlipCharOp::equal (const SlipDatum& Y, const string& X) const
   bool SlipCharOp::greatEqual (const SlipDatum& Y, const string& X) const { // operator>=
       return false;
   }; // bool SlipCharOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipCharOp::greatThan (const SlipDatum& Y, const string& X) const { // operator>
       return false;
   }; // bool SlipCharOp::equal (const SlipDatum& Y, const string& X) const

   string SlipCharOp::toString(const SlipCell& Y) const { // pretty print SLIP cell
      stringstream pretty;
      CHAR chr = *getChar((SlipDatum&)Y);
      if ((chr < 32) || (chr > 126)) pretty << "0x" << hex << setfill('0') << chr;
      else pretty << chr;
      return pretty.str();
   }; // string SlipCharOp::toString(const SlipCell& Y) const

   string SlipCharOp::dump(const SlipCell& Y) const {
      stringstream pretty;
      pretty << "[char     ] " << dumpLink(Y) << " = '" << toString(Y) << "'";
      return pretty.str();
   }; // string SlipCharOp::dump(const SlipCell& Y) const

   string SlipCharOp::write(const SlipCell& X) const {
      return "'" + toString(X) + "'";
   }; // string SlipCharOp::write(const SlipCell& X) const

}; // namespace slip