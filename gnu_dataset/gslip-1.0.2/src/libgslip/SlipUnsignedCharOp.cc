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
 * @file    SlipUnsignedCharOp.cpp
 * @author  A. Schwarz
 * @date    June 04, const 2012
 *
 */

 # include <iomanip>
 # include <string>
 # include <sstream>
 # include "SlipUnsignedCharOp.h"
 # include "SlipDatum.h"
 # include "SlipDef.h"
 # include "SlipErr.h"

using namespace slip;

namespace slip {

   /*********************************************************
    *                  Casting Operators                    *
   *********************************************************/

   bool SlipUnsignedCharOp::cast_bool(const SlipDatum& X) const {               // (bool) X
       return (*getUChar(X) == 0)? false: true;
   }; // bool SlipUnsignedCharOp::cast_bool(const SlipDatum& X) const
   UCHAR SlipUnsignedCharOp::cast_UCHAR(const SlipDatum& X) const {             // (UCHAR) X
       return (UCHAR)*getUChar(X);
   }; // UCHAR SlipUnsignedCharOp::cast_UCHAR(const SlipDatum& X) const
   CHAR SlipUnsignedCharOp::cast_CHAR(const SlipDatum& X) const {               // (CHAR) X
       return (CHAR)*getUChar(X);
   }; // CHAR SlipUnsignedCharOp::cast_CHAR(const SlipDatum& X) const
   ULONG SlipUnsignedCharOp::cast_ULONG(const SlipDatum& X) const {             // (ULONG) X
       return (ULONG)*getUChar(X);
   }; // ULONG SlipUnsignedCharOp::cast_ULONG(const SlipDatum& X) const
   LONG SlipUnsignedCharOp::cast_LONG(const SlipDatum& X) const {               // (LONG) X
       return (LONG)*getUChar(X);
   }; // LONG SlipUnsignedCharOp::cast_LONG(const SlipDatum& X) const
   DOUBLE SlipUnsignedCharOp::cast_DOUBLE(const SlipDatum& X) const {           // (DOUBLE) X
       return (DOUBLE)*getUChar(X);
   }; // DOUBLE SlipUnsignedCharOp::cast_DOUBLE(const SlipDatum& X) const
   PTR SlipUnsignedCharOp::cast_PTR(const SlipDatum& Y) const {                 // (PTR) Y
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipBoolOp++", "", "Operation not supported for unsigned char.", Y);
      return NULL;
   }; // PTR SlipUnsignedCharOp::cast_PTR(const SlipDatum& Y) const

      /*********************************************************
       *                 Operator Overloading                  *
      *********************************************************/

          /*************************************************
           *                Unary operators                *
          *************************************************/
   ULONG SlipUnsignedCharOp::bitNOT (const SlipDatum& X) const{ // operator~
      return ~(ULONG)(*getUChar(X));
   }; // ULONG SlipUnsignedCharOp::bitNOT (const SlipDatum& X) const
   bool SlipUnsignedCharOp::logNOT (const SlipDatum& X) const{ // operator!
      return (getUChar(X)== 0)? true: false;
   }; // bool SlipUnsignedCharOp::logNOT (const SlipDatum& X) const
   SlipDatum SlipUnsignedCharOp::plus (const SlipDatum& X) const{ // operator+
       return SlipDatum((ULONG)(*getUChar(X)));
   }; // SlipDatum SlipUnsignedCharOp::plus (const SlipDatum& X) const
   SlipDatum SlipUnsignedCharOp::minus (const SlipDatum& X) const{ // operator-
       return SlipDatum(-(ULONG)(*getUChar(X)));
   }; // SlipDatum SlipUnsignedCharOp::minus (const SlipDatum& X) const
   SlipDatum& SlipUnsignedCharOp::postInc (SlipDatum& X) const{ // operator++
      (*getUChar(X))++;
      return X;
   }; // SlipDatum& SlipUnsignedCharOp::postInc (SlipDatum& X) const
   SlipDatum& SlipUnsignedCharOp::postDec (SlipDatum& X) const{ // operator--
      (*getUChar(X))--;
      return X;
   }; // SlipDatum& SlipUnsignedCharOp::postDec (SlipDatum& X) const
   SlipDatum& SlipUnsignedCharOp::preInc (SlipDatum& X) const{ // ++operator
      ++(*getUChar(X));
      return X;
   }; // SlipDatum& SlipUnsignedCharOp::preInc (SlipDatum& X) const
   SlipDatum& SlipUnsignedCharOp::preDec (SlipDatum& X) const{ // --operator
       --(*getUChar(X));
       return X;
   }; // SlipDatum& SlipUnsignedCharOp::preDec (SlipDatum& X) const

          /*************************************************
           *                Binary operators               *
          *************************************************/
   SlipDatum SlipUnsignedCharOp::add(const SlipDatum& Y, const SlipDatum& X) const{ // operator+
      return (*getOperator(X))->add(X, (ULONG)*getUChar(Y));
   }; // SlipDatum SlipUnsignedCharOp::add(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipUnsignedCharOp::sub(const SlipDatum& Y, const SlipDatum& X) const{ // operator-
      return (*getOperator(Y))->subUOP((ULONG)*getUChar(Y), X);
   }; // SlipDatum SlipUnsignedCharOp::sub(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipUnsignedCharOp::mul(const SlipDatum& Y, const SlipDatum& X) const{ // operator*
      return (*getOperator(X))->mul(X, (ULONG)*getUChar(Y));
   }; // SlipDatum SlipUnsignedCharOp::mul(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipUnsignedCharOp::div(const SlipDatum& Y, const SlipDatum& X) const{ // operator/
      return (*getOperator(Y))-> divUOP((ULONG)*getUChar(Y), X);
   }; // SlipDatum SlipUnsignedCharOp::div(const SlipDatum& Y, const SlipDatum& X) const
   LONG SlipUnsignedCharOp::mod(const SlipDatum& Y, const SlipDatum& X) const{ // operator%
      return (*getOperator(Y))->modUOP((ULONG)*getUChar(Y), X);
   }; // SlipDatum SlipUnsignedCharOp::mod(const SlipDatum& Y, const SlipDatum& X) const

   SlipDatum SlipUnsignedCharOp::add(const SlipDatum& Y, const DOUBLE X) const{ // operator+
       return SlipDatum((*getUChar(Y) + X));
   }; // SlipDatum SlipUnsignedCharOp::add(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipUnsignedCharOp::sub(const SlipDatum& Y, const DOUBLE X) const{ // operator-
       return SlipDatum((*getUChar(Y) - X));
   }; // SlipDatum SlipUnsignedCharOp::sub(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipUnsignedCharOp::mul(const SlipDatum& Y, const DOUBLE X) const{ // operator*
       return SlipDatum((*getUChar(Y) * X));
   }; // SlipDatum SlipUnsignedCharOp::mul(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipUnsignedCharOp::div(const SlipDatum& Y, const DOUBLE X) const{ // operator/
       return SlipDatum((*getUChar(Y) / X));
   }; // SlipDatum SlipUnsignedCharOp::div(const SlipDatum& Y, const DOUBLE X) const

   SlipDatum SlipUnsignedCharOp::add(const SlipDatum& Y, const ULONG X) const{ // operator+
       return SlipDatum((*getUChar(Y) + X));
   }; // SlipDatum SlipUnsignedCharOp::add(const SlipDatum& Y, const ULONG X) const

   SlipDatum SlipUnsignedCharOp::sub(const SlipDatum& Y, const ULONG X) const{ // operator-
       return SlipDatum((*getUChar(Y) - X));
   }; // SlipDatum SlipUnsignedCharOp::sub(const SlipDatum& Y, const ULONG X) const
   SlipDatum SlipUnsignedCharOp::mul(const SlipDatum& Y, const ULONG X) const{ // operator*
       return SlipDatum((*getUChar(Y) * X));
   }; // SlipDatum SlipUnsignedCharOp::mul(const SlipDatum& Y, const ULONG X) const
   SlipDatum SlipUnsignedCharOp::div(const SlipDatum& Y, const ULONG X) const{ // operator/
       return SlipDatum((*getUChar(Y) / X));
   }; // SlipDatum SlipUnsignedCharOp::div(const SlipDatum& Y, const ULONG X) const
   LONG       SlipUnsignedCharOp::mod(const SlipDatum& Y, const ULONG X) const{ // operator%
       return SlipDatum((*getUChar(Y) % X));
   }; // SlipDatum SlipUnsignedCharOp::mod(const SlipDatum& Y, const ULONG X) const

   SlipDatum SlipUnsignedCharOp::add(const SlipDatum& Y, const LONG X) const{ // operator+
       return SlipDatum((*getUChar(Y) + X));
   }; // SlipDatum SlipUnsignedCharOp::add(const SlipDatum& Y, const LONG X) const

   SlipDatum SlipUnsignedCharOp::sub(const SlipDatum& Y, const LONG X) const{ // operator-
       return SlipDatum((*getUChar(Y) - X));
   }; // SlipDatum SlipUnsignedCharOp::sub(const SlipDatum& Y, const LONG X) const
   SlipDatum SlipUnsignedCharOp::mul(const SlipDatum& Y, const LONG X) const{ // operator*
       return SlipDatum((*getUChar(Y) * X));
   }; // SlipDatum SlipUnsignedCharOp::mul(const SlipDatum& Y, const LONG X) const
   SlipDatum SlipUnsignedCharOp::div(const SlipDatum& Y, const LONG X) const{ // operator/
       return SlipDatum((*getUChar(Y) / X));
   }; // SlipDatum SlipUnsignedCharOp::div(const SlipDatum& Y, const LONG X) const
   LONG       SlipUnsignedCharOp::mod(const SlipDatum& Y, const LONG X) const{ // operator%
       return SlipDatum((*getUChar(Y) % X));
   }; // SlipDatum SlipUnsignedCharOp::mod(const SlipDatum& Y, const LONG X) const

          /*************************************************
           *                Bit operators                  *
          *************************************************/
   LONG  SlipUnsignedCharOp::shl (const SlipDatum& Y, const SlipDatum& X) const{ // operator<<
      return (*getOperator(Y))->shlOP((LONG)*getUChar(Y), X);
   }; // LONG  SlipUnsignedCharOp::shl (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipUnsignedCharOp::shr (const SlipDatum& Y, const SlipDatum& X) const{ // operator>>
      return (*getOperator(Y))->shrOP((LONG)*getUChar(Y), X);
   }; // LONG  SlipUnsignedCharOp::shr (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipUnsignedCharOp::band(const SlipDatum& Y, const SlipDatum& X) const{ // operator&
      return (*getOperator(X))->band(X, (ULONG)*getUChar(Y));
   }; // LONG  SlipUnsignedCharOp::band(const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipUnsignedCharOp::bor (const SlipDatum& Y, const SlipDatum& X) const{ // operator|
      return (*getOperator(X))->bor(X, (LONG)*getUChar(Y));
   }; // LONG  SlipUnsignedCharOp::bor (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipUnsignedCharOp::bxor(const SlipDatum& Y, const SlipDatum& X) const{ // operator^
      return (*getOperator(X))->bxor(X, (LONG)*getUChar(Y));
   }; // LONG  SlipUnsignedCharOp::bxor(const SlipDatum& Y, const SlipDatum& X) const

   LONG SlipUnsignedCharOp::shl (const SlipDatum& Y, const ULONG X) const{ // operator<<
       return (LONG)(*getUChar(Y) << X);
   }; // LONG SlipUnsignedCharOp::shl (const SlipDatum& Y, const ULONG X) const
   LONG SlipUnsignedCharOp::shr (const SlipDatum& Y, const ULONG X) const{ // operator>>
       return (LONG)(*getUChar(Y) >> X);
   }; // LONG SlipUnsignedCharOp::shr (const SlipDatum& Y, const ULONG X) const
   LONG SlipUnsignedCharOp::band(const SlipDatum& Y, const ULONG X) const{ // operator&
       return (LONG)(*getUChar(Y) & X);
   }; // LONG SlipUnsignedCharOp::band(const SlipDatum& Y, const ULONG X) const
   LONG SlipUnsignedCharOp::bor (const SlipDatum& Y, const ULONG X) const{ // operator|
       return (LONG)(*getUChar(Y) | X);
   }; // LONG SlipUnsignedCharOp::bor (const SlipDatum& Y, const ULONG X) const
   LONG SlipUnsignedCharOp::bxor(const SlipDatum& Y, const ULONG X) const{ // operator^
       return (LONG)(*getUChar(Y) ^ X);
   }; // LONG SlipUnsignedCharOp::bxor(const SlipDatum& Y, const ULONG X) const

   LONG SlipUnsignedCharOp::shl (const SlipDatum& Y, const LONG X) const{ // operator<<
       return (LONG)(*getUChar(Y) << X);
   }; // LONG SlipUnsignedCharOp::shl (const SlipDatum& Y, const LONG X) const
   LONG SlipUnsignedCharOp::shr (const SlipDatum& Y, const LONG X) const{ // operator>>
       return (LONG)(*getUChar(Y) >> X);
   }; // LONG SlipUnsignedCharOp::shr (const SlipDatum& Y, const LONG X) const
   LONG SlipUnsignedCharOp::band(const SlipDatum& Y, const LONG X) const{ // operator&
       return (LONG)(*getUChar(Y) & X);
   }; // LONG SlipUnsignedCharOp::band(const SlipDatum& Y, const LONG X) const
   LONG SlipUnsignedCharOp::bor (const SlipDatum& Y, const LONG X) const{ // operator|
       return (LONG)(*getUChar(Y) | X);
   }; // LONG SlipUnsignedCharOp::bor (const SlipDatum& Y, const LONG X) const
   LONG SlipUnsignedCharOp::bxor(const SlipDatum& Y, const LONG X) const{ // operator^
       return (LONG)(*getUChar(Y) ^ X);
   }; // LONG SlipUnsignedCharOp::bxor(const SlipDatum& Y, const LONG X) const

          /*************************************************
           *               Simple Assignment               *
          *************************************************/
   SlipDatum& SlipUnsignedCharOp::asgn(SlipDatum& Y, const SlipDatum& X) const {        // operator+=
      *getUChar(Y) = (UCHAR)X;
      return Y;
   }; // SlipDatum& SlipUnsignedCharOp::asgn(SlipDatum& Y, const SlipDatum& X) const

          /*************************************************
           *       Arithmetic Assignment operators         *
          *************************************************/
   SlipDatum& SlipUnsignedCharOp::addAsgn (SlipDatum& Y, const SlipDatum& X) const{ // operator+=
      ULONG num = *getUChar(Y);
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
            num += (*getOperator(X))->cast_UCHAR(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1009, "SlipUnsignedCharOp+=", "", "Operation not supported for UCHAR.", Y, X);
            break;
      }
      *getUChar(Y) = num;
      return Y;
   }; // SlipDatum& SlipUnsignedCharOp::addAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipUnsignedCharOp::subAsgn (SlipDatum& Y, const SlipDatum& X) const{ // operator-=
      ULONG num = *getUChar(Y);
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
            num -= (*getOperator(X))->cast_UCHAR(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1010, "SlipUnsignedCharOp1=", "", "Operation not supported for UCHAR.", Y, X);
            break;
      }
      *getUChar(Y) = num;
      return Y;
   }; // SlipDatum& SlipUnsignedCharOp::subAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipUnsignedCharOp::mulAsgn (SlipDatum& Y, const SlipDatum& X) const{ // operator*=
      ULONG num = *getUChar(Y);
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
            num *= (*getOperator(X))->cast_UCHAR(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1011, "SlipUnsignedCharOp*=", "", "Operation not supported for UCHAR.", Y, X);
            break;
      }
      *getUChar(Y) = num;
      return Y;
   }; // SlipDatum& SlipUnsignedCharOp::mulAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipUnsignedCharOp::divAsgn (SlipDatum& Y, const SlipDatum& X) const{ // operator/=
      ULONG num = *getUChar(Y);
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
            num /= (*getOperator(X))->cast_UCHAR(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1012, "SlipUnsignedCharOp/=", "", "Operation not supported for UCHAR.", Y, X);
            break;
      }
      *getUChar(Y) = num;
      return Y;
   }; // SlipDatum& SlipUnsignedCharOp::divAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipUnsignedCharOp::modAsgn (SlipDatum& Y, const SlipDatum& X) const{ // operator%=
      ULONG num = *getUChar(Y);
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
            num %= (*getOperator(X))->cast_UCHAR(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1013, "SlipUnsignedCharOp%=", "", "Operation not supported for UCHAR.", Y, X);
            break;
      }
      *getUChar(Y) = num;
      return Y;
   }; // SlipDatum& SlipUnsignedCharOp::modAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipUnsignedCharOp::shlAsgn (SlipDatum& Y, const SlipDatum& X) const{ // operator<<=
      ULONG num = *getUChar(Y);
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
            num <<= (*getOperator(X))->cast_UCHAR(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1014, "SlipUnsignedCharOp<<=", "", "Operation not supported for UCHAR.", Y, X);
            break;
      }
      *getUChar(Y) = num;
      return Y;
   }; // SlipDatum& SlipUnsignedCharOp::shlAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipUnsignedCharOp::shrAsgn (SlipDatum& Y, const SlipDatum& X) const{ // operator>>=
      ULONG num = *getUChar(Y);
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
            num >>= (*getOperator(X))->cast_UCHAR(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1015, "SlipUnsignedCharOp>>=", "", "Operation not supported for UCHAR.", Y, X);
            break;
      }
      *getUChar(Y) = num;
      return Y;
   }; // SlipDatum& SlipUnsignedCharOp::shrAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipUnsignedCharOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const{ // operator&=
      ULONG num = *getUChar(Y);
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
            num &= (*getOperator(X))->cast_UCHAR(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1016, "SlipUnsignedCharOp&=", "", "Operation not supported for UCHAR.", Y, X);
            break;
      }
      *getUChar(Y) = num;
      return Y;
   }; // SlipDatum& SlipUnsignedCharOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipUnsignedCharOp::borAsgn (SlipDatum& Y, const SlipDatum& X) const{ // operator|=
      ULONG num = *getUChar(Y);
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
            num |= (*getOperator(X))->cast_UCHAR(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1019, "SlipUnsignedCharOp|=", "", "Operation not supported for UCHAR.", Y, X);
            break;
      }
      *getUChar(Y) = num;
      return Y;
   }; // SlipDatum& SlipUnsignedCharOp::borAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipUnsignedCharOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const{ // operator^=
      ULONG num = *getUChar(Y);
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
            num ^= (*getOperator(X))->cast_UCHAR(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1017, "SlipUnsignedCharOp^=", "", "Operation not supported for UCHAR.", Y, X);
            break;
      }
      *getUChar(Y) = num;
      return Y;
   }; // SlipDatum& SlipUnsignedCharOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const

   SlipDatum& SlipUnsignedCharOp::addAsgn (SlipDatum& Y, const DOUBLE X) const{ // operator+=
       *getUChar(Y) += X;
       return Y;
   }; // SlipDatum& SlipUnsignedCharOp::addAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipUnsignedCharOp::subAsgn (SlipDatum& Y, const DOUBLE X) const{ // operator-=
       *getUChar(Y) -= X;
       return Y;
   }; // SlipDatum& SlipUnsignedCharOp::subAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipUnsignedCharOp::mulAsgn (SlipDatum& Y, const DOUBLE X) const{ // operator*=
       *getUChar(Y) *= X;
       return Y;
   }; // SlipDatum& SlipUnsignedCharOp::mulAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipUnsignedCharOp::divAsgn (SlipDatum& Y, const DOUBLE X) const{ // operator/=
       *getUChar(Y) /= X;
       return Y;
   }; // SlipDatum& SlipUnsignedCharOp::divAsgn (SlipDatum& Y, const DOUBLE X) const

   SlipDatum& SlipUnsignedCharOp::addAsgn (SlipDatum& Y, const ULONG X) const{ // operator+=
       *getUChar(Y) += X;
       return Y;
   }; // SlipDatum& SlipUnsignedCharOp::addAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipUnsignedCharOp::subAsgn (SlipDatum& Y, const ULONG X) const{ // operator-=
       *getUChar(Y) -= X;
       return Y;
   }; // SlipDatum& SlipUnsignedCharOp::subAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipUnsignedCharOp::mulAsgn (SlipDatum& Y, const ULONG X) const{ // operator*=
       *getUChar(Y) *= X;
       return Y;
   }; // SlipDatum& SlipUnsignedCharOp::mulAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipUnsignedCharOp::divAsgn (SlipDatum& Y, const ULONG X) const{ // operator/=
       *getUChar(Y) /= X;
       return Y;
   }; // SlipDatum& SlipUnsignedCharOp::divAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipUnsignedCharOp::modAsgn (SlipDatum& Y, const ULONG X) const{ // operator%=
       *getUChar(Y) %= X;
       return Y;
   }; // SlipDatum& SlipUnsignedCharOp::modAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipUnsignedCharOp::shlAsgn (SlipDatum& Y, const ULONG X) const{ // operator<<=
       *getUChar(Y) <<= X;
       return Y;
   }; // SlipDatum& SlipUnsignedCharOp::shlAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipUnsignedCharOp::shrAsgn (SlipDatum& Y, const ULONG X) const{ // operator>>=
       *getUChar(Y) >>= X;
       return Y;
   }; // SlipDatum& SlipUnsignedCharOp::shrAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipUnsignedCharOp::bandAsgn(SlipDatum& Y, const ULONG X) const{ // operator&=
       *getUChar(Y) &= X;
       return Y;
   }; // SlipDatum& SlipUnsignedCharOp::bandAsgn(SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipUnsignedCharOp::borAsgn (SlipDatum& Y, const ULONG X) const{ // operator|=
       *getUChar(Y) |= X;
       return Y;
   }; // SlipDatum& SlipUnsignedCharOp::borAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipUnsignedCharOp::bxorAsgn(SlipDatum& Y, const ULONG X) const{ // operator^=
       *getUChar(Y) ^= X;
       return Y;
   }; // SlipDatum& SlipUnsignedCharOp::bxorAsgn(SlipDatum& Y, const ULONG X) const

   SlipDatum& SlipUnsignedCharOp::addAsgn (SlipDatum& Y, const LONG X) const{ // operator+=
       *getUChar(Y) += X;
       return Y;
   }; // SlipDatum& SlipUnsignedCharOp::addAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipUnsignedCharOp::subAsgn (SlipDatum& Y, const LONG X) const{ // operator-=
       *getUChar(Y) -= X;
       return Y;
   }; // SlipDatum& SlipUnsignedCharOp::subAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipUnsignedCharOp::mulAsgn (SlipDatum& Y, const LONG X) const{ // operator*=
       *getUChar(Y) *= X;
       return Y;
   }; // SlipDatum& SlipUnsignedCharOp::mulAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipUnsignedCharOp::divAsgn (SlipDatum& Y, const LONG X) const{ // operator/=
       *getUChar(Y) /= X;
       return Y;
   }; // SlipDatum& SlipUnsignedCharOp::divAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipUnsignedCharOp::modAsgn (SlipDatum& Y, const LONG X) const{ // operator%=
       *getUChar(Y) %= X;
       return Y;
   }; // SlipDatum& SlipUnsignedCharOp::modAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipUnsignedCharOp::shlAsgn (SlipDatum& Y, const LONG X) const{ // operator<<=
       *getUChar(Y) <<= X;
       return Y;
   }; // SlipDatum& SlipUnsignedCharOp::shlAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipUnsignedCharOp::shrAsgn (SlipDatum& Y, const LONG X) const{ // operator>>=
       *getUChar(Y) >>= X;
       return Y;
   }; // SlipDatum& SlipUnsignedCharOp::shrAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipUnsignedCharOp::bandAsgn(SlipDatum& Y, const LONG X) const{ // operator&=
       *getUChar(Y) &= X;
       return Y;
   }; // SlipDatum& SlipUnsignedCharOp::bandAsgn(SlipDatum& Y, const LONG X) const
   SlipDatum& SlipUnsignedCharOp::borAsgn (SlipDatum& Y, const LONG X) const{ // operator|=
       *getUChar(Y) |= X;
       return Y;
   }; // SlipDatum& SlipUnsignedCharOp::borAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipUnsignedCharOp::bxorAsgn(SlipDatum& Y, const LONG X) const{ // operator^=
       *getUChar(Y) ^= X;
       return Y;
   }; // SlipDatum& SlipUnsignedCharOp::bxorAsgn(SlipDatum& Y, const LONG X) const

          /*************************************************
           *               Logical operators               *
          *************************************************/
   bool SlipUnsignedCharOp::notEqual (const SlipDatum& Y, const SlipDatum& X) const{ // operator!=
      bool flag = true;
      UCHAR num = *getUChar(Y);
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
            flag = (*getOperator(X))->notEqual(X, (ULONG)num);
            break;
         default:
            flag = false;
            break;
      }
      return flag;
   }; // bool SlipUnsignedCharOp::notEqual (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipUnsignedCharOp::lessThan (const SlipDatum& Y, const SlipDatum& X) const{ // operator<
      bool flag = false;
      UCHAR num = *getUChar(Y);
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
            flag = (*getOperator(X))->greatThan(X, (ULONG)num);
            break;
         default:
            flag = false;
            break;
      }
      return flag;
   }; // bool SlipUnsignedCharOp::lessThan (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipUnsignedCharOp::lessEqual (const SlipDatum& Y, const SlipDatum& X) const{ // operator<=
      bool flag = false;
      UCHAR num = *getUChar(Y);
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
            flag = (*getOperator(X))->greatEqual(X, (ULONG)num);
            break;
         default:
            flag = false;
            break;
      }
      return flag;
   }; // bool SlipUnsignedCharOp::lessEqual (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipUnsignedCharOp::equal (const SlipDatum& Y, const SlipDatum& X) const{ // operator==
      bool flag = false;
      UCHAR num = *getUChar(Y);
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
            flag = (*getOperator(X))->equal(X, (ULONG)num);
            break;
         default:
            flag = false;
            break;
      }
      return flag;
   }; // bool SlipUnsignedCharOp::equal (const SlipDatum& Y, const SlipDatum& X) const
   bool SlipUnsignedCharOp::greatEqual(const SlipDatum& Y, const SlipDatum& X) const{ // operator>=
      bool flag = false;
      UCHAR num = *getUChar(Y);
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
            flag = (*getOperator(X))->lessEqual(X, (ULONG)num);
            break;
         default:
            flag = false;
            break;
      }
      return flag;
   }; // bool SlipUnsignedCharOp::greatEqual(const SlipDatum& Y, const SlipDatum& X) const
   bool SlipUnsignedCharOp::greatThan (const SlipDatum& Y, const SlipDatum& X) const{ // operator>
      bool flag = false;
      UCHAR num = *getUChar(Y);
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
            flag = (*getOperator(X))->lessThan(X, (ULONG)num);
            break;
         default:
            flag = false;
            break;
      }
      return flag;
   }; // bool SlipUnsignedCharOp::greatThan (const SlipDatum& Y, const SlipDatum& X) const

   bool SlipUnsignedCharOp::notEqual (const SlipDatum& Y, const DOUBLE X) const{ // operator!=
       return *getUChar(Y) != X;
   }; // bool SlipUnsignedCharOp::notEqual (const SlipDatum& Y, const DOUBLE X) const
   bool SlipUnsignedCharOp::lessThan (const SlipDatum& Y, const DOUBLE X) const{ // operator<
       return *getUChar(Y) < X;
   }; // bool SlipUnsignedCharOp::lessThan (const SlipDatum& Y, const DOUBLE X) const
   bool SlipUnsignedCharOp::lessEqual (const SlipDatum& Y, const DOUBLE X) const{ // operator<=
       return *getUChar(Y) <= X;
   }; // bool SlipUnsignedCharOp::lessEqual (const SlipDatum& Y, const DOUBLE X) const
   bool SlipUnsignedCharOp::equal (const SlipDatum& Y, const DOUBLE X) const{ // operator==
       return *getUChar(Y) == X;
   }; // bool SlipUnsignedCharOp::equal (const SlipDatum& Y, const DOUBLE X) const
   bool SlipUnsignedCharOp::greatEqual(const SlipDatum& Y, const DOUBLE X) const{ // operator>=
       return *getUChar(Y) >= X;
   }; // bool SlipUnsignedCharOp::greatEqual(const SlipDatum& Y, const DOUBLE X) const
   bool SlipUnsignedCharOp::greatThan (const SlipDatum& Y, const DOUBLE X) const{ // operator>
       return *getUChar(Y) > X;
   }; // bool SlipUnsignedCharOp::greatThan (const SlipDatum& Y, const DOUBLE X) const

   bool SlipUnsignedCharOp::notEqual (const SlipDatum& Y, const ULONG X) const{ // operator!=
       return *getUChar(Y) != X;
   }; // bool SlipUnsignedCharOp::notEqual (const SlipDatum& Y, const ULONG X) const
   bool SlipUnsignedCharOp::lessThan (const SlipDatum& Y, const ULONG X) const{ // operator<
       return *getUChar(Y) < X;
   }; // bool SlipUnsignedCharOp::lessThan (const SlipDatum& Y, const ULONG X) const
   bool SlipUnsignedCharOp::lessEqual (const SlipDatum& Y, const ULONG X) const{ // operator<=
       return *getUChar(Y) <= X;
   }; // bool SlipUnsignedCharOp::lessEqual (const SlipDatum& Y, const ULONG X) const
   bool SlipUnsignedCharOp::equal (const SlipDatum& Y, const ULONG X) const{ // operator==
       return *getUChar(Y) == X;
   }; // bool SlipUnsignedCharOp::equal (const SlipDatum& Y, const ULONG X) const
   bool SlipUnsignedCharOp::greatEqual(const SlipDatum& Y, const ULONG X) const{ // operator>=
       return *getUChar(Y) >= X;
   }; // bool SlipUnsignedCharOp::greatEqual(const SlipDatum& Y, const ULONG X) const
   bool SlipUnsignedCharOp::greatThan (const SlipDatum& Y, const ULONG X) const{ // operator>
       return *getUChar(Y) > X;
   }; // bool SlipUnsignedCharOp::greatThan (const SlipDatum& Y, const ULONG X) const

   bool SlipUnsignedCharOp::notEqual (const SlipDatum& Y, const LONG X) const{ // operator!=
       return *getUChar(Y) != X;
   }; // bool SlipUnsignedCharOp::notEqual (const SlipDatum& Y, const LONG X) const
   bool SlipUnsignedCharOp::lessThan (const SlipDatum& Y, const LONG X) const{ // operator<
       return *getUChar(Y) < X;
   }; // bool SlipUnsignedCharOp::lessThan (const SlipDatum& Y, const LONG X) const
   bool SlipUnsignedCharOp::lessEqual (const SlipDatum& Y, const LONG X) const{ // operator<=
       return *getUChar(Y) <= X;
   }; // bool SlipUnsignedCharOp::lessEqual (const SlipDatum& Y, const LONG X) const
   bool SlipUnsignedCharOp::equal (const SlipDatum& Y, const LONG X) const{ // operator==
       return *getUChar(Y) == X;
   }; // bool SlipUnsignedCharOp::equal (const SlipDatum& Y, const LONG X) const
   bool SlipUnsignedCharOp::greatEqual(const SlipDatum& Y, const LONG X) const{ // operator>=
       return *getUChar(Y) >= X;
   }; // bool SlipUnsignedCharOp::greatEqual(const SlipDatum& Y, const LONG X) const
   bool SlipUnsignedCharOp::greatThan (const SlipDatum& Y, const LONG X) const{ // operator>
       return *getUChar(Y) > X;
   }; // bool SlipUnsignedCharOp::greatThan (const SlipDatum& Y, const LONG X) const

   bool SlipUnsignedCharOp::notEqual (const SlipDatum& Y, const PTR X) const{ // operator!=
       return false;
   }; // bool SlipUnsignedCharOp::notEqual (const SlipDatum& Y, const PTR X) const
   bool SlipUnsignedCharOp::equal (const SlipDatum& Y, const PTR X) const{ // operator==
       return false;
   }; // bool SlipUnsignedCharOp::equal (const SlipDatum& Y, const PTR X) const

   bool SlipUnsignedCharOp::notEqual (const SlipDatum& Y, const string& X) const { // operator!=
       return false;
   }; // bool SlipUnsignedCharOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipUnsignedCharOp::lessThan (const SlipDatum& Y, const string& X) const { // operator<
       return false;
   }; // bool SlipUnsignedCharOp::equal (const SlipDatum& Y, const string& X) const
   bool SlipUnsignedCharOp::lessEqual (const SlipDatum& Y, const string& X) const { // operator<=
       return false;
   }; // bool SlipUnsignedCharOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipUnsignedCharOp::equal (const SlipDatum& Y, const string& X) const { // operator==
       return false;
   }; // bool SlipUnsignedCharOp::equal (const SlipDatum& Y, const string& X) const
   bool SlipUnsignedCharOp::greatEqual (const SlipDatum& Y, const string& X) const { // operator>=
       return false;
   }; // bool SlipUnsignedCharOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipUnsignedCharOp::greatThan (const SlipDatum& Y, const string& X) const { // operator>
       return false;
   }; // bool SlipUnsignedCharOp::equal (const SlipDatum& Y, const string& X) const

   string SlipUnsignedCharOp::toString(const SlipCell& Y) const { // pretty print SLIP cell
      stringstream pretty;
      UCHAR chr = *getUChar((SlipDatum&)Y);
      if ((chr < 32) || (chr > 126)) pretty << "0x" << hex << setfill('0') << chr;
      else pretty << chr;
      return pretty.str();
   }; // string SlipUnsignedCharOp::toString(const SlipCell& Y) const

   string SlipUnsignedCharOp::dump(const SlipCell& Y) const {
      stringstream pretty;
      pretty << "[uchar    ] " << dumpLink(Y) << " = '" << toString(Y) << "'";
      return pretty.str();
   }; // string SlipUnsignedCharOp::dump(const SlipCell& Y) const

   string SlipUnsignedCharOp::write(const SlipCell& X) const {
      return "'" + toString(X) + "'" + "UC";
   }; // string SlipUnsignedCharOp::write(const SlipCell& X) const

}; // namespace slip