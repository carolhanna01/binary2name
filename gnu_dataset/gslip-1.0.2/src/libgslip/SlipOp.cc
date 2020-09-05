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
 * @file    SlipOp.cpp
 * @author  A. Schwarz
 * @date    July 15, 2009
 *
 * @brief Contains the SlipOp definitions.
 *
 */

# include <iostream>
# include <iomanip>
# include <sstream>
# include "SlipOp.h"
# include "SlipCell.h"
# include "SlipCellBase.h"
# include "SlipBoolOp.h"
# include "SlipCharOp.h"
# include "SlipDatum.h"
# include "SlipDoubleOp.h"
# include "SlipErr.h"
# include "SlipGlobal.h"
# include "SlipHeaderOp.h"
# include "SlipLongOp.h"
# include "SlipPtrOp.h"
# include "SlipReaderOp.h"
# include "SlipStringOp.h"
# include "SlipSublistOp.h"
# include "SlipUndefinedOp.h"
# include "SlipUnsignedCharOp.h"
# include "SlipUnsignedLongOp.h"

using namespace std;
namespace slip {


       /**********************************************
        *     Header Cell Data Access Functions   *
        **********************************************/
   /** 
    * @fn SlipOp::getHeadDescList(const SlipCellBase& X)
    * @copydoc SlipCellBase::getHeadDescList(const SlipCellBase&) */

   /**@fn  SlipOp::getHeadRefCnt(const SlipCellBase& X)
    * @copydoc SlipCellBase::getHeadRefCnt(const SlipCellBase&) */

   /**
    * @fn  SlipOp::getHeadMrk(const SlipCellBase& X)
    * @copydoc SlipCellBase::getHeadMrk(const SlipCellBase&) */

       /**********************************************
        *     Sublist Cell Data Access Functions     *
        **********************************************/

   /**
    * @fn SlipOp::getSublistHeader(const SlipCellBase& X)
    * @copydoc SlipCellBase::getSublistHeader(const SlipCellBase&) */

       /**********************************************
        *     Reader Cell Data Access Functions      *
        **********************************************/

   /** 
    * @fn SlipOp::getReaderTop(SlipCellBase& X)
    * @copydoc SlipCellBase::getReaderTop()
    * @param[in] X (SlipCellReader&) reference to a reader
    */

   /**  
    * @fn SlipOp::getReaderDepth(SlipCellBase& X)
    * @copydoc SlipCellBase::getReaderDepth(SlipCellBase&) */


       /**********************************************
        *   Slip Datum Cell data Access Functions    *
        **********************************************/

   /** 
    * @fn SlipOp::getBool(const SlipCellBase& X)
    * @copydoc SlipCellBase::getBool(const SlipCellBase&) */

   /** 
    * @fn SlipOp::getUChar(const SlipCellBase& X)
    * @copydoc SlipCellBase::getUChar(const SlipCellBase&) */

   /** 
    * @fn SlipOp::getChar(const SlipCellBase& X)
    * @copydoc SlipCellBase::getChar(const SlipCellBase&) */

   /** 
    * @fn SlipOp::getULong(const SlipCellBase& X)
    * @copydoc SlipCellBase::getULong(const SlipCellBase&) */

   /** 
    * @fn SlipOp::getLong(const SlipCellBase& X)
    * @copydoc SlipCellBase::getLong(const SlipCellBase&) */

   /** 
    * Wfn SlipOp::getDouble(const SlipCellBase& X)
    * @copydoc SlipCellBase::getDouble(const SlipCellBase&) */

   /** 
    * @fn SlipOp::getPtr(const SlipCellBase& X)
    * copydoc SlipCellBase::getPtr(const SlipCellBase&) */

   /** 
    * @fn SlipOp::getString(const SlipCellBase& X)
    * @copydoc SlipCellBase::getString(const SlipCellBase&) */

   /** 
    * @fn SlipOp::getData(const SlipCellBase& X)
    * @copydoc SlipCellBase::getData(const SlipCellBase&) */

   /** 
    * @fn SlipOp::resetData(const SlipCellBase& X)
    * @copydoc SlipCellBase::resetData(SlipCellBase&) */

             /**********************************************
              *        non-commutative Operators          *
              **********************************************/

   /**
    * @brief Subtract (<b>LONG</b> - <b>X</b>) a LONG and a SlipDatum
    * <p>After widening to a LONG, this method is called to support the subtraction
    *    operation.</p>
    * <p>This method supports SlipDatum::operator-(SlipDatum& Y, SlipDatum& X).
    *    After the type of Y is determined, it is converted to a LONG for a
    *    bool, CHAR or LONG. This method is called to determine the type of
    *    X and to do the appropriate subtraction and creation of a the SlipDatum
    *    object containing the result.</p>
    * <p>The SlipDatum result will be a DOUBLE if X is a DOUBLE otherwise it
    *    will be a LONG.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E1006</b> Invalid '-' Operation for X. Operation not supported.</li>
    * </ul>
    * @see SlipboolOp::sub(SlipDatum&, SlipDatum&)
    * @see SlipCharOp::sub(SlipDatum&, SlipDatum&)
    * @see SlipLongOp::sub(SlipDatum&, SlipDatum&)
    * @param[in] Y (LONG)LHS of Y - X
    * @param[in] X (SlipDatum) RHS of Y - X
    * @return (SlipDatum) type DOUBLE if Y is a DOUBLE, otherwise LONG
    */
   SlipDatum SlipOp::subOP(const LONG Y, const SlipDatum& X) const {
      LONG num = Y;
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
            return SlipDatum(num - *getDouble(X));
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1006, "subOP", "", "Operation not supported.", X);
            break;
      }
      return SlipDatum(num);
   }; // SlipDatum SlipOp::subOP(const LONG Y, const SlipDatum X)

   /**
    * @brief Utility method to subtract (<b>ULONG</b> - <b>X</b>) a ULONG and a SlipDatum
    * <p>After widening to a ULONG, this method is called to support the subtraction
    *    operation.</p>
    * <p>This method supports SlipDatum::operator-(SlipDatum& Y, SlipDatum& X).
    *    After the type of Y is determined, it is converted to a ULONG for a
    *    UCHAR or ULONG. This method is called to determine the type of
    *    X and to do the appropriate subtraction and creation of a the SlipDatum
    *    object containing the result.</p>
    * <p>The SlipDatum result will be a DOUBLE if X is a DOUBLE otherwise it
    *    will be a LONG.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E1006</b> Invalid '-' Operation for X. Operation not supported.</li>
    * </ul>
    * @see SlipUnsignedCharOp::sub(SlipDatum&, SlipDatum&)
    * @see SlipUnsignedLongOp::sub(SlipDatum&, SlipDatum&)
    * @param[in] Y (LONG)LHS of Y - X
    * @param[in] X (SlipDatum) RHS of Y - X
    * @return (SlipDatum) type DOUBLE if Y is a DOUBLE, otherwise LONG
    */
   SlipDatum SlipOp::subUOP(const ULONG Y, const SlipDatum& X) const {
      ULONG num = Y;
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
            return SlipDatum(num - *getDouble(X));
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1006, "subUOP", "", "Operation not supported.", X);
            break;
      }
      return SlipDatum(num);
   }; // SlipDatum SlipOp::subUOP(const ULONG Y, const SlipDatum X)

   /**
    * @brief Subtract (<b>Y</b> - <b>X</b>) a DOUBLE and a SlipDatum
    * <p>This method supports SlipDatum::operator-(SlipDatum& Y, SlipDatum& X).
    *    This method is called to determine the type of X and to do the
    *    appropriate subtraction and creation of a the SlipDatum object
    *    containing the result.</p>
    * <p>The SlipDatum result will be a DOUBLE</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E1006</b> Invalid '-' Operation for X. Operation not supported.</li>
    * </ul>
    * @see SlipDoubleOp::sub(SlipDatum&, SlipDatum&)
    * @param[in] Y (LONG)LHS of Y - X
    * @param[in] X (SlipDatum) RHS of Y - X
    * @return (SlipDatum) type DOUBLE if Y is a DOUBLE, otherwise LONG
    */
   SlipDatum SlipOp::subDOP(const DOUBLE Y, const SlipDatum& X) const {
      DOUBLE num = Y;
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
         default:
            postError(__FILE__, __LINE__, SlipErr::E1006, "subDOP", "", "Operation not supported.", X);
            break;
      }
      return SlipDatum(num);
   }; // SlipDatum SlipOp::subDOP(const ULONG Y, const SlipDatum X)

   /**
    * @brief Utility method to divide (<b>LONG</b> / <b>X</b>) a LONG and a SlipDatum
    * <p>After widening to a LONG, this method is called to support the division
    *    operation.</p>
    * <p>This method supports SlipDatum::operator/(SlipDatum& Y, SlipDatum& X).
    *    After the type of Y is determined, it is converted to a LONG for a
    *    bool, CHAR or LONG. This method is called to determine the type of
    *    X and to do the appropriate division and creation of a the SlipDatum
    *    object containing the result.</p>
    * <p>The SlipDatum result will be a DOUBLE if X is a DOUBLE otherwise it
    *    will be a LONG.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E1008</b> Invalid '/' Operation for X. Operation not supported.</li>
    * </ul>
    * @see SlipboolOp::div(SlipDatum&, SlipDatum&)
    * @see SlipCharOp::div(SlipDatum&, SlipDatum&)
    * @see SlipLongOp::div(SlipDatum&, SlipDatum&)
    * @param[in] Y (LONG)LHS of Y / X
    * @param[in] X (SlipDatum) RHS of Y / X
    * @return (SlipDatum) type DOUBLE if Y is a DOUBLE, otherwise LONG
    */
   SlipDatum SlipOp::divOP(const LONG Y, const SlipDatum& X) const {
      LONG num = Y;
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
            return SlipDatum(num / *getDouble(X));
            break;
         case ePTR:
            return (*getOperator(X))->divOP(Y, X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1008, "divOP", "", "Operation not supported.", X);
            break;
      }
      return SlipDatum(num);
   }; // SlipDatum SlipOp::divOP(const LONG Y, const SlipDatum X)

   /**
    * @brief Utility method to divide (<b>LONG</b> / <b>X</b>) a ULONG and a SlipDatum
    * <p>After widening to a ULONG, this method is called to support the division
    *    operation.</p>
    * <p>This method supports SlipDatum::operator/(SlipDatum& Y, SlipDatum& X).
    *    After the type of Y is determined, it is converted to a LONG for a
    *    bool, CHAR or LONG. This method is called to determine the type of
    *    X and to do the appropriate division and creation of a the SlipDatum
    *    object containing the result.</p>
    * <p>The SlipDatum result will be a DOUBLE if X is a DOUBLE otherwise it
    *    will be a LONG.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E1008</b> Invalid '/' Operation for X. Operation not supported.</li>
    * </ul>
    * @see SlipboolOp::div(SlipDatum&, SlipDatum&)
    * @see SlipCharOp::div(SlipDatum&, SlipDatum&)
    * @see SlipLongOp::div(SlipDatum&, SlipDatum&)
    * @param[in] Y (LONG)LHS of Y / X
    * @param[in] X (SlipDatum) RHS of Y / X
    * @return (SlipDatum) type DOUBLE if Y is a DOUBLE, otherwise LONG
    */
   SlipDatum SlipOp::divUOP(const ULONG Y, const SlipDatum& X) const {
      ULONG num = Y;
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
            return SlipDatum(num / *getDouble(X));
            break;
         case ePTR:
            return (*getOperator(X))->divUOP(Y, X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1008, "divUOP", "", "Operation not supported.", X);
            break;
      }
      return SlipDatum(num);
   }; // SlipDatum SlipOp::divUOP(const LONG Y, const SlipDatum X)

   /**
    * @brief Utility method to divide (<b>DOUBLE</b> / <b>X</b>) a DOUBLE and a SlipDatum
    * <p>This method supports SlipDatum::operator/(SlipDatum& Y, SlipDatum& X).
    *    This method is called to determine the type of X and to do the
    *    appropriate division and creation of a the SlipDatum object containing
    *    the result.</p>
    * <p>The SlipDatum result will be a DOUBLE.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E1008</b> Invalid '/' Operation for X. Operation not supported.</li>
    * </ul>
    * @see SlipDoubleOp::div(SlipDatum&, SlipDatum&)
    * @param[in] Y (LONG)LHS of Y / X
    * @param[in] X (SlipDatum) RHS of Y / X
    * @return (SlipDatum((DOUBLE)) result of DOUBLE / SlipDatum
    */
   SlipDatum SlipOp::divDOP(const DOUBLE Y, const SlipDatum& X) const {
      DOUBLE num = Y;
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
            return (*getOperator(X))->divDOP(Y, X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1008, "divDOP", "", "Operation not supported.", X);
            break;
      }
      return SlipDatum(num);
   }; // SlipDatum SlipOp::divDOP(const LONG Y, const SlipDatum X)

   /**
    * @brief Utility method to take the modulus (<b>LONG</b> % <b>X</b>) a LONG and a SlipDatum
    * <p>After widening to a LONG, this method is called to support the modulus
    *    operation.</p>
    * <p>This method supports SlipDatum::operator%(SlipDatum& Y, SlipDatum& X).
    *    After the type of Y is determined, it is converted to a LONG for a
    *    bool, CHAR or LONG. This method is called to determine the type of
    *    X and to do the appropriate modulus and creation of a the SlipDatum
    *    object containing the result.</p>
    * <p>The SlipDatum result will be a LONG. Use of a DOUBLE is illegal.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E1026</b> Invalid '%' Operation for X. Operation not supported.</li>
    * </ul>
    * @see SlipboolOp::mod(SlipDatum&, SlipDatum&)
    * @see SlipCharOp::mod(SlipDatum&, SlipDatum&)
    * @see SlipLongOp::mod(SlipDatum&, SlipDatum&)
    * @param[in] Y (LONG)LHS of Y % X
    * @param[in] X (SlipDatum) RHS of Y % X
    * @return (SlipDatum) type DOUBLE if Y is a DOUBLE, otherwise LONG
    */
   SlipDatum SlipOp::modOP(const LONG Y, const SlipDatum& X) const {
      LONG num = Y;
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
            return (*getOperator(X))->modOP(Y, X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1026, "modOP", "", "Operation not supported.", X);
            break;
      }
      return SlipDatum(num);
   }; // SlipDatum SlipOp::modOP(const LONG Y, const SlipDatum X)

   /**
    * @brief Utility method to take the modulus (<b>ULONG</b> % <b>X</b>) a ULONG and a SlipDatum
    * <p>After widening to a ULONG, this method is called to support the modulus
    *    operation.</p>
    * <p>This method supports SlipDatum::operator+(SlipDatum& Y, SlipDatum& X).
    *    After the type of Y is determined, it is converted to a ULONG for a
    *    UCHAR or ULONG. This method is called to determine the type of
    *    X and to do the appropriate modulus and creation of a the SlipDatum
    *    object containing the result.</p>
    * <p>The SlipDatum result will be a ULONG. Use of a DOUBLE is illegal</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E1026</b> Invalid '%' Operation for X. Operation not supported.</li>
    * </ul>
    * @see SlipUnsignedCharOp::mod(SlipDatum&, SlipDatum&)
    * @see SlipUnsignedLongOp::mod(SlipDatum&, SlipDatum&)
    * @param[in] Y (LONG)LHS of Y % X
    * @param[in] X (SlipDatum) RHS of Y % X
    * @return (SlipDatum) type DOUBLE if Y is a DOUBLE, otherwise LONG
    */
   SlipDatum SlipOp::modUOP(const ULONG Y, const SlipDatum& X) const {
      ULONG num = Y;
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
            return (*getOperator(X))->modUOP(Y, X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1026, "modUOP", "", "Operation not supported.", X);
            break;
      }
      return SlipDatum(num);
   }; // SlipDatum SlipOp::modUOP(const ULONG Y, const SlipDatum X)


   /**
    * @brief Utility method to shift left (<b>LONG</b> &lt;&lt; <b>X</b>) a LONG and a SlipDatum
    * <p>Executes SlipDatum::operator&lt;&lt;(LONG Y, SlipDatum& X) The
    *    type of X is determined and the appropriate shift is performed.</p>
    * <p>The non-member functions in Slip.cpp are supported. After widening
    *    to a LONG the shift left operation is executed.</p>
    * <p>The result will be a LONG. A DOUBLE for X is illegal.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E1020</b> Invalid '&lt;&lt;' Operation for X. Operation not supported.</li>
    * </ul>
    * @see SlipboolOp::shl(SlipDatum&, SlipDatum&)
    * @see SlipCharOp::shl(SlipDatum&, SlipDatum&)
    * @see SlipLongOp::shl(SlipDatum&, SlipDatum&)
    * @see SlipUnsignedCharOp::shl(SlipDatum&, SlipDatum&)
    * @see SlipUnsignedLongOp::shl(SlipDatum&, SlipDatum&)
    * @param[in] Y (LONG)LHS of Y &lt;&lt; X
    * @param[in] X (SlipDatum) RHS of Y &lt;&lt; X
    * @return (LONG) shift result
    */
   LONG SlipOp::shlOP(const LONG Y, const SlipDatum& X) const {                 // num << X
      LONG num = Y;
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
            return (*getOperator(X))->shlOP(Y, X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1020, "shlOP", "", "Operation not supported.", X);
            break;
      }
      return num;
   }; // LONG SlipOp::shlOP(const LONG Y, const SlipDatum X)

   /**
    * @brief Utility method to shift right (<b>LONG</b> &gt;&gt; <b>X</b>) a LONG and a SlipDatum
    * <p>This method executes SlipDatum::operator&gt;&gt;(LONG Y, SlipDatum& X).
    *    The type of X is determined and the appropriate shift is performed.</p>
    * <p>The non-member functions in Slip.cpp are supported. After widening
    *    to a LONG the shift right operation is executed.</p>
    * <p>The result will be a LONG. A DOUBLE for X is illegal.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E1021</b> Invalid '&gt;&gt;' Operation for X. Operation not supported.</li>
    * </ul>
    * @see SlipboolOp::shr(SlipDatum&, SlipDatum&)
    * @see SlipCharOp::shr(SlipDatum&, SlipDatum&)
    * @see SlipLongOp::shr(SlipDatum&, SlipDatum&)
    * @see SlipUnsignedCharOp::shr(SlipDatum&, SlipDatum&)
    * @see SlipUnsignedLongOp::shr(SlipDatum&, SlipDatum&)
    * @param[in] Y (LONG)LHS of Y &gt;&gt; X
    * @param[in] X (SlipDatum) RHS of Y &gt;&gt; X
    * @return (LONG) shift result
    */
   LONG SlipOp::shrOP(const LONG Y, const SlipDatum& X) const {                 // num >> X
      LONG num = Y;
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
            return (*getOperator(X))->shrOP(Y, X);
            break;
         default:
            postError(__FILE__, __LINE__, SlipErr::E1021, "shrOP", "", "Operation not supported.", X);
            break;
      }
      return num;
   }; // LONG SlipOp::shrOP(const LONG Y, const SlipDatum X)

          /*************************************************
           *                Miscellaneous                  *
          *************************************************/
   /**
    * @fn SlipOp::copy(const SlipDatum& X) const
    * @brief Return a new SlipDatum instance that is a copy of the input.
    * @param[in] X (SlipDatum&) object to be copied
    * @return (SlipDatum&) copy of the input
    */
   SlipDatum& SlipOp::copy(const SlipDatum& X) const { return *new SlipDatum(X); }       // Return a reference to a copy of the current cell

   /** 
    * @fn SlipOp::getClassType()const
    * @copydoc SlipCellBase::getClassType() */

   /** 
    * @fn SlipOp::getDatumCopy(const SlipDatum& X) const
    * @copydoc SlipCellBase::getDatumCopy() */
   sAllData   SlipOp::getDatumCopy(const SlipDatum& X) const { return *X.getData(); }  // Return valid datum

   /** 
    * @fn SlipOp::getName(SlipDatum* X)
    * @copydoc SlipCellBase::getName()
    * @param[in] X (SlipDatum&) object reference
    */

   /**
    * @brief Retrieve a pointer to the User Data parse function.
    * <p>Each User Data object has a parse method required to process data
    *    input from a list definition file. Each time a reference is seen
    *    to a named User Data definition, the parse method is called for
    *    the User Data with a list of attributes needed to populate a
    *    User Data object. The parse method creates an object and populates
    *    it with the data.</p>
    * <p>The getParse() method returns <b>null</b> for all non-user data
    *    types.</p>
    * <p>The default value returned is <b>null</b>. User Data objects are
    *    expected to return a non-null value.</p>
    * @return (Parse) Pointer to the User Data parse function or <b>null</b>
    */
   Parse SlipOp::getParse(SlipDatum& X) {                                       // Return a pointer to the User Data parse method
       return NULL;
   }; // Parse SlipOp::getParse()

   /** 
    * @fn SlipOp::getOperator(const SlipCellBase& X)
    * @copydoc SlipCellBase::getOperator(const SlipCellBase* X) */

   /**
    * @fn  SlipOp::isData()      const
    * @copydoc SlipCellBase::isData() */

   /** 
    * @fn SlipOp:isDiscrete()  const
    * @copydoc SlipCellBase::isDiscrete() */

   /** 
    * @fn SlipOp::isHeader()    const
    * @copydoc SlipCellBase::isHeader() */

   /** 
    * @fn SlipOp::isNumber()    const
    * @copydoc SlipCellBase::isNumber() */

   /** 
    * @fn SlipOp::isPtr()       const
    * @copydoc SlipCellBase::isPtr() */

   /** 
    * @fn SlipOp::isReal()      const
    * @copydoc SlipCellBase::isReal() */

   /** 
    * @fn SlipOp::isString()    const
    * @copydoc SlipCellBase::isString() */

   /** 
    * @fn SlipOp::isSublist()   const
    * @copydoc SlipCellBase::isSublist() */

   /** 
    * @fn SlipOp::remove(const SlipDatum& X)
    * @copydoc SlipCellBase::remove() */
   void       SlipOp::remove(const SlipDatum& X) {  };                          // void SlipOp::remove()

   /**
    * @brief Create a string with left/right link and operation values in hex.
    * <p>Extracts the left link, right link and operation field of a SLIP cell
    *    and formats them to a hexadecimal representation.</p>
    * <p>The links are replaced "NULL" if they are null or TEMPORARY if either
    *    link is temporary.</p>
    * <p>The output format looks like:</p>
    * <tt><pre>
    *   &lt;TEMPORARY TEMPORARY&gt; [operator]
    *   &lt;leftLink rightLink&gt;  [operator]
    *   &lt;NULL     rightLink&gt;  [operator]
    *   &lt;leftLink NULL     &gt;  [operator]
    * </pre></tt>
    * <p>where:</p>
    * <ul>
    *    <li><b>TEMPORARY</b> the literal string "TEMPORARY". Set if the
    *        rightLink is TEMPORARY.</li>
    *    <li><b>NULL</b> the literal string "NULL". Set when either link
    *        is null.</li>
    *    <li><b>leftLink</b> the hexadecimal value of the leftLink.</li>
    *    <li><b>rightLink</b> the hexadecimal value of the rightLink.</li>
    *    <li><b>operator</b> the hexadecimal value of the operator pointer.</li>
    * </ul>
    * @param[in] X (SlipCellBase&) cell to be dumped
    * @return string with hexed information
    */
   string     SlipOp::dumpLink(const SlipCellBase& X) {
      stringstream pretty;
      static const string null = "NULL       ";
      pretty << hex << setfill('0') << setw(8) << &X << "::";
      if (X.rightLink == (SlipCellBase*)TEMPORARY) {
         pretty << "<TEMPORARY TEMPORARY> ";
         pretty << "> ["
                << hex << setfill('0') << setw(8) << X.operation << "] ";
      } else {
         pretty << "<";
         if (X.leftLink  == NULL) pretty << null;
         else                     pretty << hex << setfill('0') << setw(8) << X.leftLink;
         if (X.rightLink == NULL) pretty << ' ' << null;
         else                     pretty << " " << hex << setfill('0') << setw(8) << X.rightLink;
         pretty << "> ["
                << hex << setfill('0') << setw(8) << X.operation << "] ";
      }
      return pretty.str();
   }; // string SlipOP::dumpLink(const SlipDatum& X)
}; // namespace slip

