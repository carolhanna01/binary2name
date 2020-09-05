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
 * @file    SlipBoolOp.cpp
 * @author  A. Schwarz
 * @date    June 04, 2012
 *
 */

 # include <string>
 # include <sstream>
 # include "SlipBoolOp.h"
 # include "SlipDatum.h"
 # include "SlipDef.h"
 # include "SlipErr.h"
using namespace std;
using namespace slip;

namespace slip {

   static string TRUE = "true";
   static string FALSE= "false";

            /*****************************************
             *       Constructors/Destructors        *
            ******************************************/
   SlipBoolOp::SlipBoolOp(const SlipBoolOp& orig) { };
   SlipBoolOp::~SlipBoolOp() { };
   SlipBoolOp::SlipBoolOp() : SlipOp
                            ( 1                                  //!< cellType
                            , 5                                  //!< ID
                            , eBOOL                              //!< classType
                            , "bool"                             //!< name
                            , true                               //!< isDataFlag
                            , true                               //!< isDiscreteFlag
                            , false                              //!< isHeaderFlag
                            , true                               //!< isNumberFlag
                            , false) { }                         //!< isSublistFlag

   /*********************************************************
    *                  Casting Operators                    *
   *********************************************************/

   bool SlipBoolOp::cast_bool(const SlipDatum& Y) const {                       // (bool) Y
      return *getBool(Y);
   }; // bool SlipBoolOp::cast_bool(const SlipDatum& Y) const
   UCHAR SlipBoolOp::cast_UCHAR(const SlipDatum& Y) const {                      // (UCHAR) Y
      return (UCHAR)(*getBool(Y));
   }; // UCHAR SlipBoolOp::cast_UCHAR(const SlipDatum& Y) const
   CHAR SlipBoolOp::cast_CHAR(const SlipDatum& Y) const {                       // (CHAR) Y
      return (CHAR)(*getBool(Y));
   }; // CHAR SlipBoolOp::cast_CHAR(const SlipDatum& Y) const
   ULONG SlipBoolOp::cast_ULONG(const SlipDatum& Y) const {                     // (ULONG) Y
      return (ULONG)(*getBool(Y));
   }; // ULONG SlipBoolOp::cast_ULONG(const SlipDatum& Y) const
   LONG SlipBoolOp::cast_LONG(const SlipDatum& Y) const {                       // (LONG) Y
      return (LONG)(*getBool(Y));
   }; // LONG SlipBoolOp::cast_LONG(const SlipDatum& Y) const
   DOUBLE SlipBoolOp::cast_DOUBLE(const SlipDatum& Y) const {                   // (DOUBLE) Y
      return (DOUBLE)(*getBool(Y));
   }; // DOUBLE SlipBoolOp::cast_DOUBLE(const SlipDatum& Y) const
   PTR SlipBoolOp::cast_PTR(const SlipDatum& Y) const {                         // (PTR) Y
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipBoolOp++", "", "Operation not supported for bool.", Y);
      return NULL;
   }; // PTR SlipBoolOp::cast_PTR(const SlipDatum& Y) const

      /*********************************************************
       *                 Operator Overloading                  *
      *********************************************************/

          /*************************************************
           *                Unary operators                *
          *************************************************/
   ULONG SlipBoolOp::bitNOT (const SlipDatum& Y) const { // operator~
      return ~((ULONG)(*getBool(Y)));
   }; // ULONG SlipBoolOp::bitNOT (const SlipDatum& Y)
   bool SlipBoolOp::logNOT (const SlipDatum& Y) const { // operator!
      return !(*getBool(Y));
   }; // bool SlipBoolOp::logNOT (const SlipDatum& Y)
   SlipDatum  SlipBoolOp::plus (const SlipDatum& Y) const { // operator+
      return SlipDatum((LONG)(*getBool(Y)));
   }; // SlipDatum& SlipBoolOp::plus (const SlipDatum& Y)
   SlipDatum  SlipBoolOp::minus (const SlipDatum& Y) const { // operator-
      return SlipDatum(-(LONG)(*getBool(Y)));
   }; // SlipDatum& SlipBoolOp::minus (const SlipDatum& Y)
   SlipDatum& SlipBoolOp::postInc (SlipDatum& Y) const { // operator++
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipBoolOp(int)++", "", "Operation not supported for bool.", Y);
      return Y;
   }; // SlipDatum& SlipBoolOp::postInc (SlipDatum& Y)
   SlipDatum& SlipBoolOp::postDec (SlipDatum& Y) const { // operator--
      postError(__FILE__, __LINE__, SlipErr::E1004, "SlipBoolOp(int)--", "", "Operation not supported for bool.", Y);
      return Y;
   }; // SlipDatum& SlipBoolOp::postDec (SlipDatum& Y)
   SlipDatum& SlipBoolOp::preInc (SlipDatum& Y) const { // ++operator
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipBoolOp++", "", "Operation not supported for bool.", Y);
      return Y;
   }; // SlipDatum& SlipBoolOp::preInc (SlipDatum& Y)
   SlipDatum& SlipBoolOp::preDec (SlipDatum& Y) const { // --operator
      postError(__FILE__, __LINE__, SlipErr::E1004, "SlipBoolOp--", "", "Operation not supported for bool.", Y);
      return Y;
   }; // SlipDatum& SlipBoolOp::preDec (SlipDatum& Y)

          /*************************************************
           *                Binary operators               *
          *************************************************/
   SlipDatum SlipBoolOp::add(const SlipDatum& Y, const SlipDatum& X) const { // operator+
      return (*getOperator(X))->add(X, (LONG)*getBool(Y));
   }; // SlipDatum SlipBoolOp::add(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipBoolOp::sub(const SlipDatum& Y, const SlipDatum& X) const { // operator-
      return (*getOperator(X))->subOP((LONG)*getBool(Y), X);
   }; // SlipDatum SlipBoolOp::sub(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipBoolOp::mul(const SlipDatum& Y, const SlipDatum& X) const { // operator*
      return (*getOperator(X))->mul(X, (LONG)*getBool(Y));
   }; // SlipDatum SlipBoolOp::mul(const SlipDatum& Y, const SlipDatum& X) const
   SlipDatum SlipBoolOp::div(const SlipDatum& Y, const SlipDatum& X) const { // operator/
      return (*getOperator(Y))->divOP((LONG)*getBool(Y), X);
   }; // SlipDatum SlipBoolOp::div(const SlipDatum& Y, const SlipDatum& X) const
   LONG SlipBoolOp::mod(const SlipDatum& Y, const SlipDatum& X) const { // operator%
      return (*getOperator(Y))->modUOP((LONG)*getBool(Y), X);
   }; // SlipDatum SlipBoolOp::mod(const SlipDatum& Y, const SlipDatum& X) const

   SlipDatum SlipBoolOp::add(const SlipDatum& Y, const DOUBLE X) const { // operator+
      return SlipDatum(*getBool(Y) + X);
   }; // SlipDatum SlipBoolOp::add(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipBoolOp::sub(const SlipDatum& Y, const DOUBLE X) const { // operator-
      return SlipDatum(*getBool(Y) - X);
   }; // SlipDatum SlipBoolOp::sub(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipBoolOp::mul(const SlipDatum& Y, const DOUBLE X) const { // operator*
      return SlipDatum(*getBool(Y) * X);
   }; // SlipDatum SlipBoolOp::mul(const SlipDatum& Y, const DOUBLE X) const
   SlipDatum SlipBoolOp::div(const SlipDatum& Y, const DOUBLE X) const { // operator/
      return SlipDatum(*getBool(Y) / X);
   }; // SlipDatum SlipBoolOp::div(const SlipDatum& Y, const DOUBLE X) const

   SlipDatum SlipBoolOp::add(const SlipDatum& Y, const ULONG X) const { // operator+
       return SlipDatum(*getBool(Y) + X);
   }; // SlipDatum SlipBoolOp::add(const SlipDatum& Y, const ULONG X) const

   SlipDatum SlipBoolOp::sub(const SlipDatum& Y, const ULONG X) const { // operator-
       return SlipDatum(*getBool(Y) - X);
   }; // SlipDatum SlipBoolOp::sub(const SlipDatum& Y, const ULONG X) const
   SlipDatum SlipBoolOp::mul(const SlipDatum& Y, const ULONG X) const { // operator*
       return SlipDatum(*getBool(Y) * X);
   }; // SlipDatum SlipBoolOp::mul(const SlipDatum& Y, const ULONG X) const
   SlipDatum SlipBoolOp::div(const SlipDatum& Y, const ULONG X) const { // operator/
       return SlipDatum(*getBool(Y) / X);
   }; // SlipDatum SlipBoolOp::div(const SlipDatum& Y, const ULONG X) const
   LONG       SlipBoolOp::mod(const SlipDatum& Y, const ULONG X) const { // operator%
       return (LONG)(*getBool(Y) % X);
   }; // SlipDatum SlipBoolOp::mod(const SlipDatum& Y, const ULONG X) const

   SlipDatum  SlipBoolOp::add(const SlipDatum& Y, const LONG X) const { // operator+
       return SlipDatum(*getBool(Y) + X);
   }; // SlipDatum SlipBoolOp::add(const SlipDatum& Y, const LONG X) const

   SlipDatum SlipBoolOp::sub(const SlipDatum& Y, const LONG X) const { // operator-
       return SlipDatum(*getBool(Y) - X);
   }; // SlipDatum SlipBoolOp::sub(const SlipDatum& Y, const LONG X) const
   SlipDatum SlipBoolOp::mul(const SlipDatum& Y, const LONG X) const { // operator*
       return SlipDatum(*getBool(Y) * X);
   }; // SlipDatum SlipBoolOp::mul(const SlipDatum& Y, const LONG X) const
   SlipDatum SlipBoolOp::div(const SlipDatum& Y, const LONG X) const { // operator/
       return SlipDatum(*getBool(Y) / X);
   }; // SlipDatum SlipBoolOp::div(const SlipDatum& Y, const LONG X) const
   LONG       SlipBoolOp::mod(const SlipDatum& Y, const LONG X) const { // operator%
       return (LONG)(*getBool(Y) % X);
   }; // SlipDatum SlipBoolOp::mod(const SlipDatum& Y, const LONG X) const

          /*************************************************
           *                Bit operators                  *
          *************************************************/
   LONG  SlipBoolOp::shl (const SlipDatum& Y, const SlipDatum& X) const { // operator<<
      return (*getOperator(Y))->shlOP((LONG)*getBool(Y), X);
   }; // LONG  SlipBoolOp::shl (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipBoolOp::shr (const SlipDatum& Y, const SlipDatum& X) const { // operator>>
      return (*getOperator(Y))->shrOP((LONG)*getBool(Y), X);
   }; // LONG  SlipBoolOp::shr (SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipBoolOp::band(const SlipDatum& Y, const SlipDatum& X) const { // operator&
      return (*getOperator(X))->band(X, (LONG)*getBool(Y));
   }; // LONG  SlipBoolOp::band(const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipBoolOp::bor (const SlipDatum& Y, const SlipDatum& X) const { // operator|
      return (*getOperator(X))->bor(X, (LONG)*getBool(Y));
   }; // LONG  SlipBoolOp::bor (const SlipDatum& Y, const SlipDatum& X) const
   LONG  SlipBoolOp::bxor(const SlipDatum& Y, const SlipDatum& X) const { // operator^
      return (*getOperator(X))->bxor(X, (LONG)*getBool(Y));
   }; // LONG  SlipBoolOp::bxor(const SlipDatum& Y, const SlipDatum& X) const

   LONG  SlipBoolOp::shl (const SlipDatum& Y, ULONG X) const { // operator<<
       return (ULONG)(*getBool(Y) << X);
   }; // ULONG  SlipBoolOp::shl (const SlipDatum& Y, const ULONG X) const
   LONG  SlipBoolOp::shr (const SlipDatum& Y, const ULONG X) const { // operator>>
       return (ULONG)(*getBool(Y) >> X);
   }; // ULONG  SlipBoolOp::shr (const SlipDatum& Y, const ULONG X) const
   LONG  SlipBoolOp::band(const SlipDatum& Y, const ULONG X) const { // operator&
       return (ULONG)(*getBool(Y) & X);
   }; // ULONG  SlipBoolOp::band(const SlipDatum& Y, const ULONG X) const
   LONG  SlipBoolOp::bor (const SlipDatum& Y, const ULONG X) const { // operator|
       return (ULONG)(*getBool(Y) | X);
   }; // ULONG  SlipBoolOp::bor (const SlipDatum& Y, const ULONG X) const
   LONG  SlipBoolOp::bxor(const SlipDatum& Y, const ULONG X) const { // operator^
       return (ULONG)(*getBool(Y) ^ X);
   }; // ULONG  SlipBoolOp::bxor(const SlipDatum& Y, const ULONG X) const

   LONG  SlipBoolOp::shl (const SlipDatum& Y, const LONG X) const {  // operator<<
       return (LONG)(*getBool(Y) << X);
   }; // LONG  SlipBoolOp::shl (const SlipDatum& Y, const LONG X) const
   LONG  SlipBoolOp::shr (const SlipDatum& Y, const LONG X) const {  // operator>>
       return (LONG)(*getBool(Y) >> X);
   }; // LONG  SlipBoolOp::shr (const SlipDatum& Y, const LONG X) const
   LONG  SlipBoolOp::band(const SlipDatum& Y, const LONG X) const {  // operator&
       return (LONG)(*getBool(Y) & X);
   }; // LONG  SlipBoolOp::band(const SlipDatum& Y, const LONG X) const
   LONG  SlipBoolOp::bor (const SlipDatum& Y, const LONG X) const {  // operator|
       return (LONG)(*getBool(Y) | X);
   }; // LONG  SlipBoolOp::bor (const SlipDatum& Y, const LONG X) const
   LONG  SlipBoolOp::bxor(const SlipDatum& Y, const LONG X) const {  // operator^
       return (LONG)(*getBool(Y) ^ X);
   }; // LONG  SlipBoolOp::bxor(const SlipDatum& Y, const LONG X) const

          /*************************************************
           *               Simple Assignment               *
          *************************************************/
   SlipDatum& SlipBoolOp::asgn(SlipDatum& Y, const SlipDatum& X) const {        // operator+=
      *getBool(Y) = (bool)X;
      return Y;
   }; // SlipDatum& SlipBoolOp::asgn(SlipDatum& Y, const SlipDatum& X) const

          /*************************************************
           *       Arithmetic Assignment operators         *
          *************************************************/
   SlipDatum& SlipBoolOp::addAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator+=
      bool num = *getBool(Y);
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
         case ePTR:
            num += (*getOperator(X))->cast_bool(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1029, "SlipBoolOp+=", "", "Operation not supported for bool.", Y, X);
            break;
      }
      *getBool(Y) = num;
      return Y;
   }; // SlipDatum& SlipBoolOp::addAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipBoolOp::subAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator-=
      bool num = *getBool(Y);
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
         case ePTR:
            num -= (*getOperator(X))->cast_bool(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1010, "SlipBoolOp-=", "", "Operation not supported for bool.", Y, X);
            break;
      }
      *getBool(Y) = num;
      return Y;
   }; // SlipDatum& SlipBoolOp::subAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipBoolOp::mulAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator*=
      bool num = *getBool(Y);
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
         case ePTR:
            num *= (*getOperator(X))->cast_bool(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1011, "SlipBoolOp*=", "", "Operation not supported for bool.", Y, X);
            break;
      }
      *getBool(Y) = num;
      return Y;
   }; // SlipDatum& SlipBoolOp::mulAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipBoolOp::divAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator/=
      bool num = *getBool(Y);
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
         case ePTR:
            num /= (*getOperator(X))->cast_bool(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1012, "SlipBoolOp/=", "", "Operation not supported for bool.", Y, X);
            break;
      }
      *getBool(Y) = num;
      return Y;
   }; // SlipDatum& SlipBoolOp::divAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipBoolOp::modAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator%=
      bool num = *getBool(Y);
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
            num %= (*getOperator(X))->cast_bool(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1013, "SlipBoolOp%=", "", "Operation not supported for bool.", Y, X);
            break;
      }
      *getBool(Y) = num;
      return Y;
   }; // SlipDatum& SlipBoolOp::modAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipBoolOp::shlAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator<<=
      bool num = *getBool(Y);
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
            num <<= (*getOperator(X))->cast_bool(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1014, "SlipBoolOp<<=", "", "Operation not supported for bool.", Y, X);
            break;
      }
      *getBool(Y) = num;
      return Y;
   }; // SlipDatum& SlipBoolOp::shlAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipBoolOp::shrAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator>>=
      bool num = *getBool(Y);
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
            num >>= (*getOperator(X))->cast_bool(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1015, "SlipBoolOp>>=", "", "Operation not supported for bool.", Y, X);
            break;
      }
      *getBool(Y) = num;
      return Y;
   }; // SlipDatum& SlipBoolOp::shrAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipBoolOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator&=
      bool num = *getBool(Y);
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
            num &= (*getOperator(X))->cast_bool(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1016, "SlipBoolOp&=", "", "Operation not supported for bool.", Y, X);
            break;
      }
      *getBool(Y) = num;
      return Y;
   }; // SlipDatum& SlipBoolOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipBoolOp::borAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator|=
      bool num = *getBool(Y);
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
            num |= (*getOperator(X))->cast_bool(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1019, "SlipBoolOp|=", "", "Operation not supported for bool.", Y, X);
            break;
      }
      *getBool(Y) = num;
      return Y;
   }; // SlipDatum& SlipBoolOp::borAsgn (SlipDatum& Y, const SlipDatum& X) const
   SlipDatum& SlipBoolOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator^=
      bool num = *getBool(Y);
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
            num ^= (*getOperator(X))->cast_bool(X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1017, "SlipBoolOp^=", "", "Operation not supported for bool.", Y, X);
            break;
      }
      *getBool(Y) = num;
      return Y;
   }; // SlipDatum& SlipBoolOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const

   SlipDatum& SlipBoolOp::addAsgn (SlipDatum& Y, const DOUBLE X) const { // operator+=
      bool val = (bool)*getBool(Y);
      val += X;
      *getBool(Y) = val;
      return Y;
   }; // SlipDatum& SlipBoolOp::addAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipBoolOp::subAsgn (SlipDatum& Y, const DOUBLE X) const { // operator-=
      bool val = *getBool(Y);
      val -= X;
      *getBool(Y) = val;
      return Y;
   }; // SlipDatum& SlipBoolOp::subAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipBoolOp::mulAsgn (SlipDatum& Y, const DOUBLE X) const { // operator*=
      bool val = *getBool(Y);
      val *= X;
      *getBool(Y) = val;
      return Y;
   }; // SlipDatum& SlipBoolOp::mulAsgn (SlipDatum& Y, const DOUBLE X) const
   SlipDatum& SlipBoolOp::divAsgn (SlipDatum& Y, const DOUBLE X) const { // operator/=
      bool val = *getBool(Y);
      val /= X;
      *getBool(Y) = val;
      return Y;
   }; // SlipDatum& SlipBoolOp::divAsgn (SlipDatum& Y, const DOUBLE X) const


   SlipDatum& SlipBoolOp::addAsgn (SlipDatum& Y, const ULONG X) const { // operator+=
      bool val = *getBool(Y);
      val += X;
      *getBool(Y) = val;
      return Y;
   }; // SlipDatum& SlipBoolOp::addAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipBoolOp::subAsgn (SlipDatum& Y, const ULONG X) const { // operator-=
      bool val = *getBool(Y);
      val -= X;
      *getBool(Y) = val;
      return Y;
   }; // SlipDatum& SlipBoolOp::subAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipBoolOp::mulAsgn (SlipDatum& Y, const ULONG X) const { // operator*=
      bool val = *getBool(Y);
      val *= X;
      *getBool(Y) = val;
      return Y;
   }; // SlipDatum& SlipBoolOp::mulAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipBoolOp::divAsgn (SlipDatum& Y, const ULONG X) const { // operator/=
      bool val = *getBool(Y);
      val /= X;
      *getBool(Y) = val;
      return Y;
   }; // SlipDatum& SlipBoolOp::divAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipBoolOp::modAsgn (SlipDatum& Y, const ULONG X) const { // operator%=
      bool val = *getBool(Y);
      val %= X;
      *getBool(Y) = val;
      return Y;
   }; // SlipDatum& SlipBoolOp::modAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipBoolOp::shlAsgn (SlipDatum& Y, const ULONG X) const { // operator<<=
      bool val = *getBool(Y);
      val <<= X;
      *getBool(Y) = val;
      return Y;
   }; // SlipDatum& SlipBoolOp::shlAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipBoolOp::shrAsgn (SlipDatum& Y, const ULONG X) const { // operator>>=
      bool val = *getBool(Y);
      val >>= X;
      *getBool(Y) = val;
      return Y;
   }; // SlipDatum& SlipBoolOp::shrAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipBoolOp::bandAsgn(SlipDatum& Y, const ULONG X) const { // operator&=
      bool val = *getBool(Y);
      val &= X;
      *getBool(Y) = val;
      return Y;
   }; // SlipDatum& SlipBoolOp::bandAsgn(SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipBoolOp::borAsgn (SlipDatum& Y, const ULONG X) const { // operator|=
      bool val = *getBool(Y);
      val |= X;
      *getBool(Y) = val;
      return Y;
   }; // SlipDatum& SlipBoolOp::borAsgn (SlipDatum& Y, const ULONG X) const
   SlipDatum& SlipBoolOp::bxorAsgn(SlipDatum& Y, const ULONG X) const { // operator^=
      bool val = *getBool(Y);
      val ^= X;
      *getBool(Y) = val;
      return Y;
   }; // SlipDatum& SlipBoolOp::bxorAsgn(SlipDatum& Y, const ULONG X) const

   SlipDatum& SlipBoolOp::addAsgn (SlipDatum& Y, const LONG X) const { // operator+=
      bool val = *getBool(Y);
      val += X;
      *getBool(Y) = val;
      return Y;
   }; // SlipDatum& SlipBoolOp::addAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipBoolOp::subAsgn (SlipDatum& Y, const LONG X) const { // operator-=
      bool val = *getBool(Y);
      val -= X;
      *getBool(Y) = val;
      return Y;
   }; // SlipDatum& SlipBoolOp::subAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipBoolOp::mulAsgn (SlipDatum& Y, const LONG X) const { // operator*=
      bool val = *getBool(Y);
      val *= X;
      *getBool(Y) = val;
      return Y;
   }; // SlipDatum& SlipBoolOp::mulAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipBoolOp::divAsgn (SlipDatum& Y, const LONG X) const { // operator/=
      bool val = *getBool(Y);
      val /= X;
      *getBool(Y) = val;
      return Y;
   }; // SlipDatum& SlipBoolOp::divAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipBoolOp::modAsgn (SlipDatum& Y, const LONG X) const { // operator%=
      bool val = *getBool(Y);
      val %= X;
      *getBool(Y) = val;
      return Y;
   }; // SlipDatum& SlipBoolOp::modAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipBoolOp::shlAsgn (SlipDatum& Y, const LONG X) const { // operator<<=
      bool val = *getBool(Y);
      val <<= X;
      *getBool(Y) = val;
      return Y;
   }; // SlipDatum& SlipBoolOp::shlAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipBoolOp::shrAsgn (SlipDatum& Y, const LONG X) const { // operator>>=
      bool val = *getBool(Y);
      val >>= X;
      *getBool(Y) = val;
      return Y;
   }; // SlipDatum& SlipBoolOp::shrAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipBoolOp::bandAsgn(SlipDatum& Y, const LONG X) const { // operator&=
      bool val = *getBool(Y);
      val &= X;
      *getBool(Y) = val;
      return Y;
   }; // SlipDatum& SlipBoolOp::bandAsgn(SlipDatum& Y, const LONG X) const
   SlipDatum& SlipBoolOp::borAsgn (SlipDatum& Y, const LONG X) const { // operator|=
      bool val = *getBool(Y);
      val |= X;
      *getBool(Y) = val;
      return Y;
   }; // SlipDatum& SlipBoolOp::borAsgn (SlipDatum& Y, const LONG X) const
   SlipDatum& SlipBoolOp::bxorAsgn(SlipDatum& Y, const LONG X) const { // operator^=
      bool val = *getBool(Y);
      val ^= X;
      *getBool(Y) = val;
      return Y;
   }; // SlipDatum& SlipBoolOp::bxorAsgn(SlipDatum& Y, const LONG X) const

          /*************************************************
           *               Logical operators               *
          *************************************************/
   bool SlipBoolOp::notEqual (const SlipDatum& Y, const SlipDatum& X)const { // operator!=
      bool flag = *getBool(Y);
      switch(X.getClassType()) {
         case eBOOL:
            flag = flag !=  *getBool(X);
            break;
         case eCHAR:
            flag = flag !=  *getChar(X);
            break;
         case eUCHAR:
            flag = flag !=  *getUChar(X);
            break;
         case eLONG:
            flag = flag !=  *getLong(X);
            break;
         case eULONG:
            flag = flag !=  *getULong(X);
            break;
         case eDOUBLE:
            flag = flag !=  *getDouble(X);
            break;
         case ePTR:
            flag = (*getOperator(X))->notEqual(X, (LONG)flag);
            break;
         default:
            flag = false;
            break;
      }
      return flag;
   }; // bool SlipBoolOp::notEqual (const SlipDatum& Y, const SlipDatum& X)const
   bool SlipBoolOp::lessThan (const SlipDatum& Y, const SlipDatum& X)const { // operator<
      bool flag = *getBool(Y);
      switch(X.getClassType()) {
         case eBOOL:
            flag = flag < *getBool(X);
            break;
         case eCHAR:
            flag = flag < *getChar(X);
            break;
         case eUCHAR:
            flag = flag < *getUChar(X);
            break;
         case eLONG:
            flag = flag < *getLong(X);
            break;
         case eULONG:
            flag = flag < *getULong(X);
            break;
         case eDOUBLE:
            flag = flag < *getDouble(X);
            break;
         case ePTR:
            flag = (*getOperator(X))->greatThan(X, (LONG)flag);
            break;
         default:
            flag = false;
            break;
      }
      return flag;
   }; // bool SlipBoolOp::lessThan (const SlipDatum& Y, const SlipDatum& X)const
   bool SlipBoolOp::lessEqual (const SlipDatum& Y, const SlipDatum& X)const { // operator<=
      bool flag = *getBool(Y);
      switch(X.getClassType()) {
         case eBOOL:
            flag = flag <= *getBool(X);
            break;
         case eCHAR:
            flag = flag <= *getChar(X);
            break;
         case eUCHAR:
            flag = flag <= *getUChar(X);
            break;
         case eLONG:
            flag = flag <= *getLong(X);
            break;
         case eULONG:
            flag = flag <= *getULong(X);
            break;
         case eDOUBLE:
            flag = flag <= *getDouble(X);
            break;
         case ePTR:
            flag = (*getOperator(X))->greatEqual(X, (LONG)flag);
            break;
         default:
            flag = false;
            break;
      }
      return flag;
   }; // bool SlipBoolOp::lessEqual (const SlipDatum& Y, const SlipDatum& X)const
   bool SlipBoolOp::equal (const SlipDatum& Y, const SlipDatum& X)const { // operator==
      bool flag = *getBool(Y);
      switch(X.getClassType()) {
         case eBOOL:
            flag = flag == *getBool(X);
            break;
         case eCHAR:
            flag = flag == *getChar(X);
            break;
         case eUCHAR:
            flag = flag == *getUChar(X);
            break;
         case eLONG:
            flag = flag == *getLong(X);
            break;
         case eULONG:
            flag = flag == *getULong(X);
            break;
         case eDOUBLE:
            flag = flag == *getDouble(X);
            break;
         case ePTR:
            flag = (*getOperator(X))->equal(X, (LONG)flag);
            break;
         default:
            flag = false;
            break;
      }
      return flag;
   }; // bool SlipBoolOp::equal (const SlipDatum& Y, const SlipDatum& X)const
   bool SlipBoolOp::greatEqual(const SlipDatum& Y, const SlipDatum& X)const { // operator>=
      bool flag = *getBool(Y);
      switch(X.getClassType()) {
         case eBOOL:
            flag = flag >= *getBool(X);
            break;
         case eCHAR:
            flag = flag >= *getChar(X);
            break;
         case eUCHAR:
            flag = flag >= *getUChar(X);
            break;
         case eLONG:
            flag = flag >= *getLong(X);
            break;
         case eULONG:
            flag = flag >= *getULong(X);
            break;
         case eDOUBLE:
            flag = flag >= *getDouble(X);
            break;
         case ePTR:
            flag = (*getOperator(X))->lessEqual(X, (LONG)flag);
            break;
         default:
            flag = false;
            break;
      }
      return flag;
   }; // bool SlipBoolOp::greatEqual(const SlipDatum& Y, const SlipDatum& X)const
   bool SlipBoolOp::greatThan (const SlipDatum& Y, const SlipDatum& X)const { // operator>
      bool flag = *getBool(Y);
      switch(X.getClassType()) {
         case eBOOL:
            flag = flag > *getBool(X);
            break;
         case eCHAR:
            flag = flag > *getChar(X);
            break;
         case eUCHAR:
            flag = flag > *getUChar(X);
            break;
         case eLONG:
            flag = flag > *getLong(X);
            break;
         case eULONG:
            flag = flag > *getULong(X);
            break;
         case eDOUBLE:
            flag = flag > *getDouble(X);
            break;
         case ePTR:
            flag = (*getOperator(X))->lessThan(X, (LONG)flag);
            break;
         default:
            flag = false;
            break;
      }
      return flag;
   }; // bool SlipBoolOp::greatThan (const SlipDatum& Y, const SlipDatum& X)const

   bool SlipBoolOp::notEqual (const SlipDatum& Y, const DOUBLE X) const { // operator!=
       return *getBool(Y) != X;
   }; // bool SlipBoolOp::notEqual (const SlipDatum& Y, const DOUBLE X) const
   bool SlipBoolOp::lessThan (const SlipDatum& Y, const DOUBLE X) const { // operator<
       return *getBool(Y) < X;
   }; // bool SlipBoolOp::lessThan (const SlipDatum& Y, const DOUBLE X) const
   bool SlipBoolOp::lessEqual (const SlipDatum& Y, const DOUBLE X) const { // operator<=
       return *getBool(Y) <= X;
   }; // bool SlipBoolOp::lessEqual (const SlipDatum& Y, const DOUBLE X) const
   bool SlipBoolOp::equal (const SlipDatum& Y, const DOUBLE X) const { // operator==
       return *getBool(Y) == X;
   }; // bool SlipBoolOp::equal (const SlipDatum& Y, const DOUBLE X) const
   bool SlipBoolOp::greatEqual(const SlipDatum& Y, const DOUBLE X) const { // operator>=
       return *getBool(Y) >= X;
   }; // bool SlipBoolOp::greatEqual(const SlipDatum& Y, const DOUBLE X) const
   bool SlipBoolOp::greatThan (const SlipDatum& Y, const DOUBLE X) const { // operator>
       return *getBool(Y) > X;
   }; // bool SlipBoolOp::greatThan (const SlipDatum& Y, const DOUBLE X) const

   bool SlipBoolOp::notEqual (const SlipDatum& Y, const ULONG X) const { // operator!=
       return *getBool(Y) != X;
   }; // bool SlipBoolOp::notEqual (const SlipDatum& Y, const ULONG X) const
   bool SlipBoolOp::lessThan (const SlipDatum& Y, const ULONG X) const { // operator<
       return *getBool(Y) < X;
   }; // bool SlipBoolOp::lessThan (const SlipDatum& Y, const ULONG X) const
   bool SlipBoolOp::lessEqual (const SlipDatum& Y, const ULONG X) const { // operator<=
       return *getBool(Y) <= X;
   }; // bool SlipBoolOp::lessEqual (const SlipDatum& Y, const ULONG X) const
   bool SlipBoolOp::equal (const SlipDatum& Y, const ULONG X) const { // operator==
       return *getBool(Y) == X;
   }; // bool SlipBoolOp::equal (const SlipDatum& Y, const ULONG X) const
   bool SlipBoolOp::greatEqual(const SlipDatum& Y, const ULONG X) const { // operator>=
       return *getBool(Y) >= X;
   }; // bool SlipBoolOp::greatEqual(const SlipDatum& Y, const ULONG X) const
   bool SlipBoolOp::greatThan (const SlipDatum& Y, const ULONG X) const { // operator>
       return *getBool(Y) > X;
   }; // bool SlipBoolOp::greatThan (const SlipDatum& Y, const ULONG X) const

   bool SlipBoolOp::notEqual (const SlipDatum& Y, const LONG X) const { // operator!=
       return *getBool(Y) != X;
   }; // bool SlipBoolOp::notEqual (const SlipDatum& Y, const LONG X) const
   bool SlipBoolOp::lessThan (const SlipDatum& Y, const LONG X) const { // operator<
       return *getBool(Y) < X;
   }; // bool SlipBoolOp::lessThan (const SlipDatum& Y, const LONG X) const
   bool SlipBoolOp::lessEqual (const SlipDatum& Y, const LONG X) const { // operator<=
       return *getBool(Y) <= X;
   }; // bool SlipBoolOp::lessEqual (const SlipDatum& Y, const LONG X) const
   bool SlipBoolOp::equal (const SlipDatum& Y, const LONG X) const { // operator==
       return *getBool(Y) == X;
   }; // bool SlipBoolOp::equal (const SlipDatum& Y, const LONG X) const
   bool SlipBoolOp::greatEqual(const SlipDatum& Y, const LONG X) const { // operator>=
       return *getBool(Y) >= X;
   }; // bool SlipBoolOp::greatEqual(const SlipDatum& Y, const LONG X) const
   bool SlipBoolOp::greatThan (const SlipDatum& Y, const LONG X) const { // operator>
       return *getBool(Y) > X;
   }; // bool SlipBoolOp::greatThan (const SlipDatum& Y, const LONG X) const

   bool SlipBoolOp::notEqual (const SlipDatum& Y, const PTR X) const { // operator!=
       return false;
   }; // bool SlipBoolOp::notEqual (const SlipDatum& Y, const PTR X) const
   bool SlipBoolOp::equal (const SlipDatum& Y, const PTR X) const { // operator==
       return false;
   }; // bool SlipBoolOp::equal (const SlipDatum& Y, const PTR X) const

   bool SlipBoolOp::notEqual (const SlipDatum& Y, const string& X) const { // operator!=
       return false;
   }; // bool SlipBoolOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipBoolOp::lessThan (const SlipDatum& Y, const string& X) const { // operator<
       return false;
   }; // bool SlipBoolOp::equal (const SlipDatum& Y, const string& X) const
   bool SlipBoolOp::lessEqual (const SlipDatum& Y, const string& X) const { // operator<=
       return false;
   }; // bool SlipBoolOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipBoolOp::equal (const SlipDatum& Y, const string& X) const { // operator==
       return false;
   }; // bool SlipBoolOp::equal (const SlipDatum& Y, const string& X) const
   bool SlipBoolOp::greatEqual (const SlipDatum& Y, const string& X) const { // operator>=
       return false;
   }; // bool SlipBoolOp::notEqual (const SlipDatum& Y, const string& X) const
   bool SlipBoolOp::greatThan (const SlipDatum& Y, const string& X) const { // operator>
       return false;
   }; // bool SlipBoolOp::equal (const SlipDatum& Y, const string& X) const

   string SlipBoolOp::toString(const SlipCell& Y) const { // pretty print SLIP cell
      static const string truth[] = {"false", "true"};
      int i = (*getBool((SlipDatum&)Y))? 1: 0;
      return truth[i];
   }; // string SlipBoolOp::toString(const SlipCell& Y) const
   string SlipBoolOp::dump(const SlipCell& Y) const {
      stringstream pretty;
      pretty << "[bool     ] " << dumpLink(Y) << " = " << toString(Y);
      return pretty.str();
   }; // string SlipBoolOp::dump(const SlipCell& Y) const

   string SlipBoolOp::write(const SlipCell& X) const {
      return toString(X);
   }; // SlipBoolOp::write(SlipCell& X)

}; // namespace slip