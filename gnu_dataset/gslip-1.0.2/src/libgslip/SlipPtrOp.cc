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
 * @file    SlipPtrOp.cpp
 * @author  A. Schwarz
 * @date    June 04, 2012
 *
 */

 # include <string>
 # include <iomanip>
 # include <sstream>
 # include "SlipDatum.h"
 # include "SlipDef.h"
 # include "SlipErr.h"
 # include "SlipGlobal.h"
 # include "SlipOp.h"
 # include "SlipPointer.h"
 # include "SlipPtrOp.h"

using namespace slip;

namespace slip {

   /*********************************************************
    *              Constructors/Destructors                 *
   *********************************************************/
   /**
    * @brief A copy constructor is not supported for the Generic User Defined Data operations.
    * <p>The default User Defined operations are invariant and static.
    *    Only a single, static, copy of the data object exists and it
    *    can not be copied.</p>
    * @param[in] orig (SlipPtrOp&) original object
    */
   SlipPtrOp::SlipPtrOp(const SlipPtrOp& orig) { };

   /**
    * @brief Generic User Defined Data operations destructor.
    * <p>The object is never deleted and this method is never executed.</p>
    */
   SlipPtrOp::~SlipPtrOp() { };

   /**
    * @brief A constructor for the Generic User Defined Data operations.
    * <p>The constructor establishes the only invariant properties for
    *    all User Defined Data. The properties are recognized and used
    *    by the SLIP kernel. All User Defined Data operations must
    *    supply the same values for the same operations.</p>
    */
   SlipPtrOp::SlipPtrOp() : SlipOp ( 9                //!< cellType
                          , 6                         //!< ID
                          , ePTR                      //!< classType
                          , "PTR"                     //!< name
                          , true                      //!< isDataFlag
                          , false                     //!< isDiscreteFlag
                          , false                     //!< isHeaderFlag
                          , false                     //!< isNumberFlag
                          , false) { }                //!< isSublistFlag

   /*********************************************************
    *                  Casting Operators                    *
   *********************************************************/

   /** @copydoc SlipUndefinedOp::cast_bool(const SlipDatum&) const */
   bool SlipPtrOp::cast_bool(const SlipDatum& X) const {                        // X (bool)
      postError(__FILE__, __LINE__, SlipErr::E1035, "SlipPtrOp(bool)", "", "Operation not supported for pointer.", X);
      return false;
   }; // bool SlipPtrOp::cast_bool(const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp:::cast_UCHAR(const SlipDatum&) const */
   UCHAR SlipPtrOp::cast_UCHAR(const SlipDatum& X) const {                      // X (UCHAR)
      postError(__FILE__, __LINE__, SlipErr::E1036, "SlipPtrOp(UCHAR)", "", "Operation not supported for pointer.", X);
      return 0;
   }; // UCHAR SlipPtrOp::cast_UCHAR(const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::cast_CHAR(const SlipDatum&) const */
   CHAR SlipPtrOp::cast_CHAR(const SlipDatum& X) const {                        // X (CHAR)
      postError(__FILE__, __LINE__, SlipErr::E1037, "SlipPtrOp(CHAR)", "", "Operation not supported for pointer.", X);
      return 0;
   }; // CHAR SlipPtrOp::cast_CHAR(const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::cast_ULONG(const SlipDatum&) const */
   ULONG SlipPtrOp::cast_ULONG(const SlipDatum& X) const {                      // X (ULONG)
      postError(__FILE__, __LINE__, SlipErr::E1038, "SlipPtrOp(ULONG)", "", "Operation not supported for pointer.", X);
      return 0;
   }; // ULONG SlipPtrOp::cast_ULONG(const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::cast_LONG(const SlipDatum&) const */
   LONG SlipPtrOp::cast_LONG(const SlipDatum& X) const {                        // X (LONG)
      postError(__FILE__, __LINE__, SlipErr::E1039, "SlipPtrOp(LONG)", "", "Operation not supported for pointer.", X);
      return 0;
   }; // LONG SlipPtrOp::cast_LONG(const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::cast_DOUBLE(const SlipDatum&) const */
   DOUBLE SlipPtrOp::cast_DOUBLE(const SlipDatum& X) const {                    // X (DOUBLE)
      postError(__FILE__, __LINE__, SlipErr::E1040, "SlipPtrOp(DOUBLE)", "", "Operation not supported for pointer.", X);
      return 0.0;
   }; // DOUBLE SlipPtrOp::cast_DOUBLE(const SlipDatum& X) const
   PTR SlipPtrOp::cast_PTR(const SlipDatum& Y) const {                          // (PTR) Y
      return *getPtr((SlipCellBase&) Y);
   }; // PTR SlipPtrOp::cast_PTR(const SlipDatum& Y) const

      /*********************************************************
       *                 Operator Overloading                  *
      *********************************************************/

          /*************************************************
           *                Unary operators                *
          *************************************************/

   /** @copydoc SlipUndefinedOp::bitNOT (const SlipDatum&) const
    * @param[in] X (SlipDatum&) object of ~X
    */
   ULONG SlipPtrOp::bitNOT (const SlipDatum& X) const { // operator~
      postError(__FILE__, __LINE__, SlipErr::E1043, "SlipPtrOpoperator~", "", "Operation not supported for pointer.", X);
      return 0;
   }; // ULONG SlipPtrOp::bitNOT (const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::logNOT (const SlipDatum&) const */
   bool SlipPtrOp::logNOT (const SlipDatum& X) const { // operator!
      postError(__FILE__, __LINE__, SlipErr::E1025, "SlipPtrOp!", "", "Operation not supported for pointer.", X);
      return false;
   }; // bool SlipPtrOp::logNOT (const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::plus (const SlipDatum&) const */
   SlipDatum SlipPtrOp::plus (const SlipDatum& X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1001, "SlipPtrOp+", "", "Operation not supported for pointer.", X);
      return X;
   }; // SlipDatum SlipPtrOp::plus (const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::minus (const SlipDatum&) const */
   SlipDatum SlipPtrOp::minus (const SlipDatum& X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1002, "SlipPtrOp-", "", "Operation not supported for pointer.", X);
      return X;
   }; // SlipDatum SlipPtrOp::minus (const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp:::postInc (SlipDatum&) const */
   SlipDatum& SlipPtrOp::postInc (SlipDatum& X) const { // operator++
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipPtrOp(int)++", "", "Operation not supported for pointer.", X);
      return X;
   }; // SlipDatum& SlipPtrOp::postInc (SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::postDec (SlipDatum&) const */
   SlipDatum& SlipPtrOp::postDec (SlipDatum& X) const { // operator--
      postError(__FILE__, __LINE__, SlipErr::E1004, "SlipPtrOp(int)--", "", "Operation not supported for pointer.", X);
      return X;
   }; // SlipDatum& SlipPtrOp::postDec (SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::preInc (SlipDatum&) const */
   SlipDatum& SlipPtrOp::preInc (SlipDatum& X) const { // ++operator
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipPtrOp++", "", "Operation not supported for pointer.", X);
      return X;
   }; // SlipDatum& SlipPtrOp::preInc (SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::preDec (SlipDatum&) const */
   SlipDatum& SlipPtrOp::preDec (SlipDatum& X) const { // --operator
      postError(__FILE__, __LINE__, SlipErr::E1004, "SlipPtrOp--", "", "Operation not supported for pointer.", X);
      return X;
   }; // SlipDatum& SlipPtrOp::preDec (SlipDatum& X) const

          /*************************************************
           *                Binary operators               *
          *************************************************/

   /** @copydoc SlipUndefinedOp::add(const SlipDatum&, const SlipDatum&) const */
   SlipDatum SlipPtrOp::add(const SlipDatum& Y, const SlipDatum& X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1005, "SlipPtrOp+", "", "Operation not supported for pointer.", Y, X);
      return Y;
   }; // SlipDatum SlipPtrOp::add(const SlipDatum& Y, const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::sub(const SlipDatum&, const SlipDatum&) const */
   SlipDatum SlipPtrOp::sub(const SlipDatum& Y, const SlipDatum& X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1006, "SlipPtrOp-", "", "Operation not supported for pointer.", Y, X);
      return Y;
   }; // SlipDatum SlipPtrOp::sub(const SlipDatum& Y, const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::mul(const SlipDatum&, const SlipDatum&) const */
   SlipDatum SlipPtrOp::mul(const SlipDatum& Y, const SlipDatum& X) const { // operator*
      postError(__FILE__, __LINE__, SlipErr::E1007, "SlipPtrOp*", "", "Operation not supported for pointer.", Y, X);
      return Y;
   }; // SlipDatum SlipPtrOp::mul(const SlipDatum& Y, const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::div(const SlipDatum&, const SlipDatum&) const */
   SlipDatum SlipPtrOp::div(const SlipDatum& Y, const SlipDatum& X) const { // operator/
      postError(__FILE__, __LINE__, SlipErr::E1008, "SlipPtrOp/", "", "Operation not supported for pointer.", Y, X);
      return Y;
   }; // SlipDatum SlipPtrOp::div(const SlipDatum& Y, const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::mod(const SlipDatum&, const SlipDatum&) const */
   LONG SlipPtrOp::mod(const SlipDatum& Y, const SlipDatum& X) const { // operator%
      postError(__FILE__, __LINE__, SlipErr::E1026, "SlipPtrOp%", "", "Operation not supported for pointer.", Y, X);
      return 0;
   }; // SlipDatum SlipPtrOp::mod(const SlipDatum& Y, const SlipDatum& X) const


   /** @copydoc SlipUndefinedOp::add(const SlipDatum&, const DOUBLE) const */
   SlipDatum SlipPtrOp::add(const SlipDatum& Y, const DOUBLE X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1005, "SlipPtrOp+", "", "Operation not supported for pointer.", Y);;
      return Y;
   }; // SlipDatum SlipPtrOp::add(const SlipDatum& Y, const DOUBLE X) const

   /** @copydoc SlipUndefinedOp::sub(const SlipDatum&, const DOUBLE) const */
   SlipDatum SlipPtrOp::sub(const SlipDatum& Y, const DOUBLE X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1006, "SlipPtrOp-", "", "Operation not supported for pointer.", Y);;
      return Y;
   }; // SlipDatum SlipPtrOp::sub(const SlipDatum& Y, const DOUBLE X) const

   /** @copydoc SlipUndefinedOp::mul(const SlipDatum&, const DOUBLE) const */
   SlipDatum SlipPtrOp::mul(const SlipDatum& Y, const DOUBLE X) const { // operator*
      postError(__FILE__, __LINE__, SlipErr::E1007, "SlipPtrOp*", "", "Operation not supported for pointer.", Y);;
      return Y;
   }; // SlipDatum SlipPtrOp::mul(const SlipDatum& Y, const DOUBLE X) const

   /** @copydoc SlipUndefinedOp::div(const SlipDatum&, const DOUBLE) const */
   SlipDatum SlipPtrOp::div(const SlipDatum& Y, const DOUBLE X) const { // operator/
      postError(__FILE__, __LINE__, SlipErr::E1008, "SlipPtrOp/", "", "Operation not supported for pointer.", Y);;
      return Y;
   }; // SlipDatum SlipPtrOp::div(const SlipDatum& Y, const DOUBLE X) const


   /** @copydoc SlipUndefinedOp::add(const SlipDatum&, const ULONG) const */
   SlipDatum SlipPtrOp::add(const SlipDatum& Y, const ULONG X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1005, "SlipPtrOp+", "", "Operation not supported for pointer.", Y);;
      return Y;
   }; // SlipDatum SlipPtrOp::add(const SlipDatum& Y, const ULONG X) const


   /** @copydoc SlipUndefinedOp::sub(const SlipDatum&, const ULONG) const */
   SlipDatum SlipPtrOp::sub(const SlipDatum& Y, const ULONG X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1006, "SlipPtrOp-", "", "Operation not supported for pointer.", Y);;
      return Y;
   }; // SlipDatum SlipPtrOp::sub(const SlipDatum& Y, const ULONG X) const

   /** @copydoc SlipUndefinedOp::mul(const SlipDatum&, const ULONG) const */
   SlipDatum SlipPtrOp::mul(const SlipDatum& Y, const ULONG X) const { // operator*
      postError(__FILE__, __LINE__, SlipErr::E1007, "SlipPtrOp*", "", "Operation not supported for pointer.", Y);;
      return Y;
   }; // SlipDatum SlipPtrOp::mul(const SlipDatum& Y, const ULONG X) const

   /** @copydoc SlipUndefinedOp::div(const SlipDatum&, const ULONG) const */
   SlipDatum SlipPtrOp::div(const SlipDatum& Y, const ULONG X) const { // operator/
      postError(__FILE__, __LINE__, SlipErr::E1008, "SlipPtrOp/", "", "Operation not supported for pointer.", Y);;
      return Y;
   }; // SlipDatum SlipPtrOp::div(const SlipDatum& Y, const ULONG X) const

   /** @copydoc SlipUndefinedOp::mod(const SlipDatum&, const ULONG) const */
   LONG       SlipPtrOp::mod(const SlipDatum& Y, const ULONG X) const { // operator%
      postError(__FILE__, __LINE__, SlipErr::E1026, "%", "", "Operation not supported for pointer.", Y);
      return 0;
   }; // SlipDatum SlipPtrOp::mod(const SlipDatum& Y, const ULONG X) const


   /** @copydoc SlipUndefinedOp::add(const SlipDatum&, const LONG) const */
   SlipDatum SlipPtrOp::add(const SlipDatum& Y, const LONG X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1005, "SlipPtrOp+", "", "Operation not supported for pointer.");
      return Y;
   }; // SlipDatum SlipPtrOp::add(const SlipDatum& Y, const LONG X) const


   /** @copydoc SlipUndefinedOp::sub(const SlipDatum&, const LONG) const */
   SlipDatum SlipPtrOp::sub(const SlipDatum& Y, const LONG X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1006, "SlipPtrOp-", "", "Operation not supported for pointer.");
      return Y;
   }; // SlipDatum SlipPtrOp::sub(const SlipDatum& Y, const LONG X) const

   /** @copydoc SlipUndefinedOp::mul(const SlipDatum&, const LONG) const */
   SlipDatum SlipPtrOp::mul(const SlipDatum& Y, const LONG X) const { // operator*
      postError(__FILE__, __LINE__, SlipErr::E1007, "SlipPtrOp*", "", "Operation not supported for pointer.");
      return Y;
   }; // SlipDatum SlipPtrOp::mul(const SlipDatum& Y, const LONG X) const

   /** @copydoc SlipUndefinedOp::div(const SlipDatum&, const LONG) const */
   SlipDatum SlipPtrOp::div(const SlipDatum& Y, const LONG X) const { // operator/
      postError(__FILE__, __LINE__, SlipErr::E1008, "SlipPtrOp/", "", "Operation not supported for pointer.");
      return Y;
   }; // SlipDatum SlipPtrOp::div(const SlipDatum& Y, const LONG X) const

   /** @copydoc SlipUndefinedOp::mod(const SlipDatum&, const LONG) const */
   LONG       SlipPtrOp::mod(const SlipDatum& Y, const LONG X) const { // operator%
      postError(__FILE__, __LINE__, SlipErr::E1026, "SlipPtrOp%", "", "Operation not supported for pointer.", Y);
      return 0;
   }; // SlipDatum SlipPtrOp::mod(const SlipDatum& Y, const LONG X) const

          /*************************************************
           *                Bit operators                  *
          *************************************************/

   /** @copydoc SlipUndefinedOp::shl(const SlipDatum&, const SlipDatum&) const */
   LONG  SlipPtrOp::shl(const SlipDatum& Y, const SlipDatum& X) const { // operator<<
      postError(__FILE__, __LINE__, SlipErr::E1020, "SlipPtrOp<<", "", "Operation not supported for pointer.", Y, X);
      return 0;
   }; // LONG  SlipPtrOp::shl (const SlipDatum& Y, const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp:shr(const SlipDatum&, const SlipDatum&) const */
   LONG  SlipPtrOp::shr(const SlipDatum& Y, const SlipDatum& X) const { // operator>>
      postError(__FILE__, __LINE__, SlipErr::E1021, "SlipPtrOp>>", "", "Operation not supported for pointer.", Y, X);
      return 0;
   }; // LONG  SlipPtrOp::shr (const SlipDatum& Y, const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::band(const SlipDatum&, const SlipDatum&) const */
   LONG  SlipPtrOp::band(const SlipDatum& Y, const SlipDatum& X) const { // operator&
      postError(__FILE__, __LINE__, SlipErr::E1024, "SlipPtrOp&", "", "Operation not supported for pointer.", Y, X);
      return 0;
   }; // LONG  SlipPtrOp::band(const SlipDatum& Y, const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::bor(const SlipDatum&, const SlipDatum&) const */
   LONG  SlipPtrOp::bor(const SlipDatum& Y, const SlipDatum& X) const { // operator|
      postError(__FILE__, __LINE__, SlipErr::E1023, "SlipPtrOp|", "", "Operation not supported for pointer.", Y, X);
      return 0;
   }; // LONG  SlipPtrOp::bor (const SlipDatum& Y, const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::bxor(const SlipDatum&, const SlipDatum&) const */
   LONG  SlipPtrOp::bxor(const SlipDatum& Y, const SlipDatum& X) const { // operator^
      postError(__FILE__, __LINE__, SlipErr::E1022, "SlipPtrOp^", "", "Operation not supported for pointer.", Y, X);
      return 0;
   }; // LONG  SlipPtrOp::bxor(const SlipDatum& Y, const SlipDatum& X) const


   /** @copydoc SlipUndefinedOp::shl(const SlipDatum&, const ULONG) const */
   LONG  SlipPtrOp::shl(const SlipDatum& Y, const ULONG X) const { // operator<<
      postError(__FILE__, __LINE__, SlipErr::E1020, "SlipPtrOp<<", "", "Operation not supported for pointer.", Y);
      return 0;
   }; // LONG  SlipPtrOp::shl (const SlipDatum& Y, const ULONG X) const

   /** @copydoc SlipUndefinedOp::shr(const SlipDatum&, const ULONG) const */
   LONG  SlipPtrOp::shr(const SlipDatum& Y, const ULONG X) const { // operator>>
      postError(__FILE__, __LINE__, SlipErr::E1021, "SlipPtrOp>>", "",  "Operation not supported for pointer.", Y);
      return 0;
   }; // LONG  SlipPtrOp::shr (const SlipDatum& Y, const ULONG X) const

   /** @copydoc SlipUndefinedOp::band(const SlipDatum&, const ULONG) const */
   LONG  SlipPtrOp::band(const SlipDatum& Y, const ULONG X) const { // operator&
      postError(__FILE__, __LINE__, SlipErr::E1024, "SlipPtrOp&", "",  "Operation not supported for pointer.", Y);
      return false;
   }; // LONG  SlipPtrOp::band(const SlipDatum& Y, const ULONG X) const

   /** @copydoc SlipUndefinedOp::bor(const SlipDatum&, const ULONG) const */
   LONG  SlipPtrOp::bor(const SlipDatum& Y, const ULONG X) const { // operator|
      postError(__FILE__, __LINE__, SlipErr::E1023, "SlipPtrOp|", "",  "Operation not supported for pointer.", Y);
      return 0;
   }; // LONG  SlipPtrOp::bor (const SlipDatum& Y, const ULONG X) const

   /** @copydoc SlipUndefinedOp::bxor(const SlipDatum&, const ULONG) const */
   LONG  SlipPtrOp::bxor(const SlipDatum& Y, const ULONG X) const { // operator^
      postError(__FILE__, __LINE__, SlipErr::E1022, "SlipPtrOp^", "", "Operation not supported for pointer.", Y);
      return 0;
   }; // LONG  SlipPtrOp::bxor(const SlipDatum& Y, const ULONG X) const


   /** @copydoc SlipUndefinedOp:::shl(const SlipDatum&, const LONG) const */
   LONG  SlipPtrOp::shl(const SlipDatum& Y, const LONG X) const { // operator<<
      postError(__FILE__, __LINE__, SlipErr::E1020, "SlipPtrOp<<", "", "Operation not supported for pointer.", Y);
      return 0;
   }; // LONG  SlipPtrOp::shl (const SlipDatum& Y, const LONG X) const

   /** @copydoc SlipUndefinedOp::shr(const SlipDatum&, const LONG) const */
   LONG  SlipPtrOp::shr(const SlipDatum& Y, const LONG X) const { // operator>>
      postError(__FILE__, __LINE__, SlipErr::E1021, "SlipPtrOp>>", "", "Operation not supported for pointer.", Y);
      return 0;
   }; // LONG  SlipPtrOp::shr (const SlipDatum& Y, const LONG X) const

   /** @copydoc SlipUndefinedOp::band(const SlipDatum&, const LONG) const */
   LONG  SlipPtrOp::band(const SlipDatum& Y, const LONG X) const { // operator&
      postError(__FILE__, __LINE__, SlipErr::E1024, "SlipPtrOp&", "", "Operation not supported for pointer.", Y);
      return 0;
   }; // LONG  SlipPtrOp::band(const SlipDatum& Y, const LONG X) const

   /** @copydoc SlipUndefinedOp::bor(const SlipDatum&, const LONG) const */
   LONG  SlipPtrOp::bor(const SlipDatum& Y, const LONG X) const { // operator|
      postError(__FILE__, __LINE__, SlipErr::E1023, "SlipPtrOp|", "", "Operation not supported for pointer.", Y);
      return 0;
   }; // LONG  SlipPtrOp::bor (const SlipDatum& Y, const LONG X) const

   /** @copydoc SlipUndefinedOp::bxor(const SlipDatum&, const LONG) const */
   LONG  SlipPtrOp::bxor(const SlipDatum& Y, const LONG X) const { // operator^
      postError(__FILE__, __LINE__, SlipErr::E1022, "SlipPtrOp^", "", "Operation not supported for pointer.", Y);
      return 0;
   }; // LONG  SlipPtrOp::bxor(const SlipDatum& Y, const LONG X) const

          /*************************************************
           *               Simple Assignment               *
          *************************************************/

   /** @copydoc SlipUndefinedOp::asgn(SlipDatum&, const SlipDatum&) const */
   SlipDatum& SlipPtrOp::asgn(SlipDatum& Y, const SlipDatum& X) const {        // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1022, "SlipPtrOp^", "", "Operation not supported for pointer.", Y);
//      *getPtr(Y) = (PTR)X;
      return Y;
   }; // SlipDatum& SlipPtrOp::asgn(SlipDatum& Y, const SlipDatum& X) const

          /*************************************************
           *       Arithmetic Assignment operators         *
          *************************************************/

   /** @copydoc SlipUndefinedOp::addAsgn(SlipDatum&, const SlipDatum&) const */
   SlipDatum& SlipPtrOp::addAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1009, "SlipPtrOp+=", "", "Operation not supported for pointer.", Y, X);
      return Y;
   }; // SlipDatum& SlipPtrOp::addAsgn (SlipDatum& Y, const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::subAsgn(SlipDatum&, const SlipDatum&) const */
   SlipDatum& SlipPtrOp::subAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator-=
      postError(__FILE__, __LINE__, SlipErr::E1010, "SlipPtrOp-=", "", "Operation not supported for pointer.", Y, X);
      return Y;
   }; // SlipDatum& SlipPtrOp::subAsgn (SlipDatum& Y, const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::mulAsgn(SlipDatum&, const SlipDatum&) const */
   SlipDatum& SlipPtrOp::mulAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator*=
      postError(__FILE__, __LINE__, SlipErr::E1011, "SlipPtrOp*=", "", "Operation not supported for pointer.", Y, X);
      return Y;
   }; // SlipDatum& SlipPtrOp::mulAsgn (SlipDatum& Y, const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::divAsgn(SlipDatum&, const SlipDatum&) const */
   SlipDatum& SlipPtrOp::divAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator/=
      postError(__FILE__, __LINE__, SlipErr::E1012, "SlipPtrOp/=", "", "Operation not supported for pointer.", Y, X);
      return Y;
   }; // SlipDatum& SlipPtrOp::divAsgn (SlipDatum& Y, const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::modAsgn(SlipDatum&, const SlipDatum&) const */
   SlipDatum& SlipPtrOp::modAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator%=
      postError(__FILE__, __LINE__, SlipErr::E1013, "SlipPtrOp%=", "", "Operation not supported for pointer.", Y, X);
      return Y;
   }; // SlipDatum& SlipPtrOp::modAsgn (SlipDatum& Y, const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::shlAsgn(SlipDatum&, const SlipDatum&) const */
   SlipDatum& SlipPtrOp::shlAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator<<=
      postError(__FILE__, __LINE__, SlipErr::E1014, "SlipPtrOp<<=", "", "Operation not supported for pointer.", Y, X);
      return Y;
   }; // SlipDatum& SlipPtrOp::shlAsgn (SlipDatum& Y, const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::shrAsgn(SlipDatum&, const SlipDatum&) const */
   SlipDatum& SlipPtrOp::shrAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator>>=
      postError(__FILE__, __LINE__, SlipErr::E1015, "SlipPtrOp>>=", "", "Operation not supported for pointer.", Y, X);
      return Y;
   }; // SlipDatum& SlipPtrOp::shrAsgn (SlipDatum& Y, const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::bandAsgn(SlipDatum&, const SlipDatum&) const */
   SlipDatum& SlipPtrOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator&=
      postError(__FILE__, __LINE__, SlipErr::E1016, "SlipPtrOp&=", "", "Operation not supported for pointer.", Y, X);
      return Y;
   }; // SlipDatum& SlipPtrOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::borAsgn(SlipDatum&, const SlipDatum&) const */
   SlipDatum& SlipPtrOp::borAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator|=
      postError(__FILE__, __LINE__, SlipErr::E1019, "SlipPtrOp|=", "", "Operation not supported for pointer.", Y, X);
      return Y;
   }; // SlipDatum& SlipPtrOp::borAsgn (SlipDatum& Y, const SlipDatum& X) const

   /** @copydoc SlipUndefinedOp::bxorAsgn(SlipDatum&, const SlipDatum&) const */
   SlipDatum& SlipPtrOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator^=
      postError(__FILE__, __LINE__, SlipErr::E1017, "SlipPtrOp^=", "", "Operation not supported for pointer.", Y, X);
      return Y;
   }; // SlipDatum& SlipPtrOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const


   /** @copydoc SlipUndefinedOp::addAsgn(SlipDatum&, const DOUBLE) const */
   SlipDatum& SlipPtrOp::addAsgn(SlipDatum& Y, const DOUBLE X) const { // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1009, "SlipPtrOp+=", "", "Operation not supported for pointer.", Y);
      return Y;
   }; // SlipDatum& SlipPtrOp::addAsgn (SlipDatum& Y, const DOUBLE X) const

   /** @copydoc SlipUndefinedOp::subAsgn(SlipDatum&, const DOUBLE) const */
   SlipDatum& SlipPtrOp::subAsgn(SlipDatum& Y, const DOUBLE X) const { // operator-=
      postError(__FILE__, __LINE__, SlipErr::E1010, "SlipPtrOp-=", "", "Operation not supported for pointer.", Y);
      return Y;
   }; // SlipDatum& SlipPtrOp::subAsgn (SlipDatum& Y, const DOUBLE X) const

   /** @copydoc SlipUndefinedOp::mulAsgn(SlipDatum&, const DOUBLE) const */
   SlipDatum& SlipPtrOp::mulAsgn(SlipDatum& Y, const DOUBLE X) const { // operator*=
      postError(__FILE__, __LINE__, SlipErr::E1011, "SlipPtrOp*=", "", "Operation not supported for pointer.", Y);
      return Y;
   }; // SlipDatum& SlipPtrOp::mulAsgn (SlipDatum& Y, const DOUBLE X) const

   /** @copydoc SlipUndefinedOp::divAsgn(SlipDatum&, const DOUBLE) const */
   SlipDatum& SlipPtrOp::divAsgn(SlipDatum& Y, const DOUBLE X) const { // operator/=
      postError(__FILE__, __LINE__, SlipErr::E1012, "SlipPtrOp/=", "", "Operation not supported for pointer.", Y);
      return Y;
   }; // SlipDatum& SlipPtrOp::divAsgn (SlipDatum& Y, const DOUBLE X) const


   /** @copydoc SlipUndefinedOp::addAsgn(SlipDatum&, const ULONG) const */
   SlipDatum& SlipPtrOp::addAsgn(SlipDatum& Y, const ULONG X) const { // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1009, "SlipPtrOp+=", "", "Operation not supported for pointer.");
      return Y;
   }; // SlipDatum& SlipPtrOp::addAsgn (SlipDatum& Y, const ULONG X) const

   /** @copydoc SlipUndefinedOp::subAsgn(SlipDatum&, const ULONG) const */
   SlipDatum& SlipPtrOp::subAsgn(SlipDatum& Y, const ULONG X) const { // operator-=
      postError(__FILE__, __LINE__, SlipErr::E1010, "SlipPtrOp-=", "", "Operation not supported for pointer.");
      return Y;
   }; // SlipDatum& SlipPtrOp::subAsgn (SlipDatum& Y, const ULONG X) const

   /** @copydoc SlipUndefinedOp::mulAsgn(SlipDatum&, const ULONG) const */
   SlipDatum& SlipPtrOp::mulAsgn(SlipDatum& Y, const ULONG X) const { // operator*=
      postError(__FILE__, __LINE__, SlipErr::E1011, "SlipPtrOp*=", "", "Operation not supported for pointer.");
      return Y;
   }; // SlipDatum& SlipPtrOp::mulAsgn (SlipDatum& Y, const ULONG X) const

   /** @copydoc SlipUndefinedOp::divAsgn(SlipDatum&, const ULONG) const */
   SlipDatum& SlipPtrOp::divAsgn(SlipDatum& Y, const ULONG X) const { // operator/=
      postError(__FILE__, __LINE__, SlipErr::E1012, "SlipPtrOp/=", "", "Operation not supported for pointer.");
      return Y;
   }; // SlipDatum& SlipPtrOp::divAsgn (SlipDatum& Y, const ULONG X) const

   /** @copydoc SlipUndefinedOp::modAsgn(SlipDatum&, const ULONG) const */
   SlipDatum& SlipPtrOp::modAsgn(SlipDatum& Y, const ULONG X) const { // operator%=
      postError(__FILE__, __LINE__, SlipErr::E1013, "SlipPtrOp%=", "", "Operation not supported for pointer.", Y);
      return Y;
   }; // SlipDatum& SlipPtrOp::modAsgn (SlipDatum& Y, const ULONG X) const

   /** @copydoc SlipUndefinedOp::shlAsgn(SlipDatum&, const ULONG) const */
   SlipDatum& SlipPtrOp::shlAsgn(SlipDatum& Y, const ULONG X) const { // operator<<=
      postError(__FILE__, __LINE__, SlipErr::E1014, "SlipPtrOp<<=", "", "Operation not supported for pointer.", Y);
      return Y;
   }; // SlipDatum& SlipPtrOp::shlAsgn (SlipDatum& Y, const ULONG X) const

   /** @copydoc SlipUndefinedOp::shrAsgn(SlipDatum&, const ULONG) const */
   SlipDatum& SlipPtrOp::shrAsgn(SlipDatum& Y, const ULONG X) const { // operator>>=
      postError(__FILE__, __LINE__, SlipErr::E1015, "SlipPtrOp>>=", "", "Operation not supported for pointer.", Y);
      return Y;
   }; // SlipDatum& SlipPtrOp::shrAsgn (SlipDatum& Y, const ULONG X) const

   /** @copydoc SlipUndefinedOp::bandAsgn(SlipDatum&, const ULONG) const */
   SlipDatum& SlipPtrOp::bandAsgn(SlipDatum& Y, const ULONG X) const { // operator&=
      postError(__FILE__, __LINE__, SlipErr::E1016, "SlipPtrOp>>=", "", "Operation not supported for pointer.", Y);
      return Y;
   }; // SlipDatum& SlipPtrOp::bandAsgn(SlipDatum& Y, const ULONG X) const

   /** @copydoc SlipUndefinedOp::borAsgn(SlipDatum&, const ULONG) const */
   SlipDatum& SlipPtrOp::borAsgn(SlipDatum& Y, const ULONG X) const { // operator|=
      postError(__FILE__, __LINE__, SlipErr::E1019, "SlipPtrOp|=", "", "Operation not supported for pointer.", Y);
      return Y;
   }; // SlipDatum& SlipPtrOp::borAsgn (SlipDatum& Y, const ULONG X) const

   /** @copydoc SlipUndefinedOp::bxorAsgn(SlipDatum&, const ULONG) const */
   SlipDatum& SlipPtrOp::bxorAsgn(SlipDatum& Y, const ULONG X) const { // operator^=
      postError(__FILE__, __LINE__, SlipErr::E1017, "SlipPtrOp^=", "", "Operation not supported for pointer.", Y);
      return Y;
   }; // SlipDatum& SlipPtrOp::bxorAsgn(SlipDatum& Y, const ULONG X) const


   /** @copydoc SlipUndefinedOp::addAsgn(SlipDatum&, const LONG) const */
   SlipDatum& SlipPtrOp::addAsgn(SlipDatum& Y, const LONG X) const { // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1009, "SlipPtrOp+=", "", "Operation not supported for pointer.");
      return Y;
   }; // SlipDatum& SlipPtrOp::addAsgn (SlipDatum& Y, const LONG X) const

   /** @copydoc SlipUndefinedOp::subAsgn(SlipDatum&, const LONG) const */
   SlipDatum& SlipPtrOp::subAsgn(SlipDatum& Y, const LONG X) const { // operator-=
      postError(__FILE__, __LINE__, SlipErr::E1010, "SlipPtrOp-=", "", "Operation not supported for pointer.");
      return Y;
   }; // SlipDatum& SlipPtrOp::subAsgn (SlipDatum& Y, const LONG X) const

   /** @copydoc SlipUndefinedOp::mulAsgn(SlipDatum&, const LONG) const */
   SlipDatum& SlipPtrOp::mulAsgn(SlipDatum& Y, const LONG X) const { // operator*=
      postError(__FILE__, __LINE__, SlipErr::E1011, "SlipPtrOp*=", "", "Operation not supported for pointer.");
      return Y;
   }; // SlipDatum& SlipPtrOp::mulAsgn (SlipDatum& Y, const LONG X) const

   /** @copydoc SlipUndefinedOp::divAsgn(SlipDatum&, const LONG) const */
   SlipDatum& SlipPtrOp::divAsgn(SlipDatum& Y, const LONG X) const { // operator/=
      postError(__FILE__, __LINE__, SlipErr::E1012, "SlipPtrOp/=", "", "Operation not supported for pointer.");
      return Y;
   }; // SlipDatum& SlipPtrOp::divAsgn (SlipDatum& Y, const LONG X) const

   /** @copydoc SlipUndefinedOp::modAsgn(SlipDatum&, const LONG) const */
   SlipDatum& SlipPtrOp::modAsgn(SlipDatum& Y, const LONG X) const { // operator%=
      postError(__FILE__, __LINE__, SlipErr::E1013, "SlipPtrOp%=", "", "Operation not supported for pointer.", Y);
      return Y;
   }; // SlipDatum& SlipPtrOp::modAsgn (SlipDatum& Y, const LONG X) const

   /** @copydoc SlipUndefinedOp::shlAsgn(SlipDatum&, const LONG) const */
   SlipDatum& SlipPtrOp::shlAsgn(SlipDatum& Y, const LONG X) const { // operator<<=
      postError(__FILE__, __LINE__, SlipErr::E1014, "SlipPtrOp<<=", "", "Operation not supported for pointer.", Y);
      return Y;
   }; // SlipDatum& SlipPtrOp::shlAsgn (SlipDatum& Y, const LONG X) const

   /** @copydoc SlipUndefinedOp::shrAsgn(SlipDatum&, const LONG) const */
   SlipDatum& SlipPtrOp::shrAsgn(SlipDatum& Y, const LONG X) const { // operator>>=
      postError(__FILE__, __LINE__, SlipErr::E1015, "SlipPtrOp>>=", "", "Operation not supported for pointer.", Y);
      return Y;
   }; // SlipDatum& SlipPtrOp::shrAsgn (SlipDatum& Y, const LONG X) const

   /** @copydoc SlipUndefinedOp::bandAsgn(SlipDatum&, const LONG) const */
   SlipDatum& SlipPtrOp::bandAsgn(SlipDatum& Y, const LONG X) const { // operator&=
      postError(__FILE__, __LINE__, SlipErr::E1016, "SlipPtrOp&=", "", "Operation not supported for pointer.", Y);
      return Y;
   }; // SlipDatum& SlipPtrOp::bandAsgn(SlipDatum& Y, const LONG X) const

   /** @copydoc SlipUndefinedOp::borAsgn(SlipDatum&, const LONG) const */
   SlipDatum& SlipPtrOp::borAsgn(SlipDatum& Y, const LONG X) const { // operator|=
      postError(__FILE__, __LINE__, SlipErr::E1019, "SlipPtrOp|=", "", "Operation not supported for pointer.", Y);
      return Y;
   }; // SlipDatum& SlipPtrOp::borAsgn (SlipDatum& Y, const LONG X) const

   /** @copydoc SlipUndefinedOp::bxorAsgn(SlipDatum&, const LONG) const */
   SlipDatum& SlipPtrOp::bxorAsgn(SlipDatum& Y, const LONG X) const { // operator^=
      postError(__FILE__, __LINE__, SlipErr::E1017, "SlipPtrOp^=", "", "Operation not supported for pointer.", Y);
      return Y;
   }; // SlipDatum& SlipPtrOp::bxorAsgn(SlipDatum& Y, const LONG X) const

          /*************************************************
           *               Logical operators               *
          *************************************************/

   /**
    * @brief A logical comparison operation (<b>Y != X</b>).
    * <p>Compare the pointers to the User Defined Data. If the pointers
    *    are the same, return <b>false</b></p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (SlipDatum&) right comparand
    * @return <b>true</b> if the SlipPointer* is the different
    */
   bool SlipPtrOp::notEqual (const SlipDatum& Y, const SlipDatum& X) const { // operator!=
      if (X.getClassType() == ePTR) {
         return (*getPtr(Y) != *getPtr(X));
      } else {
         return false;
      }
   }; // bool SlipPtrOp::notEqual (const SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A logical comparison operation (<b>Y &lt; X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (SlipDatum&) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::lessThan (const SlipDatum& Y, const SlipDatum& X) const { // operator<
      return false;
   }; // bool SlipPtrOp::lessThan (const SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A logical comparison operation (<b>Y &lt;= X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (SlipDatum&) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::lessEqual (const SlipDatum& Y, const SlipDatum& X) const { // operator<=
      return false;
   }; // bool SlipPtrOp::lessEqual (const SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A logical comparison operation (<b>Y == X</b>)
    * <p>Compare the pointers to the User Defined Data. If the pointers
    *    are the same, return <b>true</b></p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (SlipDatum&) right comparand
    * @return <b>true</b> if the SlipPointer* is the same
    */
   bool SlipPtrOp::equal (const SlipDatum& Y, const SlipDatum& X) const { // operator==
      bool flag = false;
      if (X.isPtr())
         flag = (*getPtr(Y) == *getPtr(X));
      return flag;
   }; // bool SlipPtrOp::equal (const SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A logical comparison operation (<b>Y &gt;= X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (SlipDatum&) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::greatEqual(const SlipDatum& Y, const SlipDatum& X) const { // operator>=
      return false;
   }; // bool SlipPtrOp::greatEqual(const SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A logical comparison operation (<b>Y &gt; X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (SlipDatum&) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::greatThan (const SlipDatum& Y, const SlipDatum& X) const { // operator>
      return false;
   }; // bool SlipPtrOp::greatThan (const SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A logical comparison operation (<b>Y != X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (DOUBLE) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::notEqual (const SlipDatum& Y, const DOUBLE X) const { // operator!=
      return false;
   }; // bool SlipPtrOp::notEqual (const SlipDatum& Y, const DOUBLE X) const

   /**
    * @brief A logical comparison operation (<b>Y &lt; X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (DOUBLE) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::lessThan (const SlipDatum& Y, const DOUBLE X) const { // operator<
      return false;
   }; // bool SlipPtrOp::lessThan (const SlipDatum& Y, const DOUBLE X) const

   /**
    * @brief A logical comparison operation (<b>Y &lt;= X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (DOUBLE) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::lessEqual (const SlipDatum& Y, const DOUBLE X) const { // operator<=
      return false;
   }; // bool SlipPtrOp::lessEqual (const SlipDatum& Y, const DOUBLE X) const

   /**
    * @brief A logical comparison operation (<b>Y == X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (DOUBLE) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::equal (const SlipDatum& Y, const DOUBLE X) const { // operator==
      return false;
   }; // bool SlipPtrOp::equal (const SlipDatum& Y, const DOUBLE X) const

   /**
    * @brief A logical comparison operation (<b>Y &gt;= X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (DOUBLE) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::greatEqual(const SlipDatum& Y, const DOUBLE X) const { // operator>=
      return false;
   }; // bool SlipPtrOp::greatEqual(const SlipDatum& Y, const DOUBLE X) const

   /**
    * @brief A logical comparison operation (<b>Y &gt; X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (DOUBLE) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::greatThan (const SlipDatum& Y, const DOUBLE X) const { // operator>
      return false;
   }; // bool SlipPtrOp::greatThan (const SlipDatum& Y, const DOUBLE X) const

   /**
    * @brief A logical comparison operation (<b>Y != X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (ULONG) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::notEqual (const SlipDatum& Y, const ULONG X) const { // operator!=
      return false;
   }; // bool SlipPtrOp::notEqual (const SlipDatum& Y, const ULONG X) const

   /**
    * @brief A logical comparison operation (<b>Y &lt; X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (ULONG) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::lessThan (const SlipDatum& Y, const ULONG X) const { // operator<
      return false;
   }; // bool SlipPtrOp::lessThan (const SlipDatum& Y, const ULONG X) const

   /**
    * @brief A logical comparison operation (<b>Y &lt;= X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (ULONG) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::lessEqual (const SlipDatum& Y, const ULONG X) const { // operator<=
      return false;
   }; // bool SlipPtrOp::lessEqual (const SlipDatum& Y, const ULONG X) const

   /**
    * @brief A logical comparison operation (<b>Y == X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (ULONG) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::equal (const SlipDatum& Y, const ULONG X) const { // operator==
      return false;
   }; // bool SlipPtrOp::equal (const SlipDatum& Y, const ULONG X) const

   /**
    * @brief A logical comparison operation (<b>Y &gt;= X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (ULONG) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::greatEqual(const SlipDatum& Y, const ULONG X) const { // operator>=
      return false;
   }; // bool SlipPtrOp::greatEqual(const SlipDatum& Y, const ULONG X) const

   /**
    * @brief A logical comparison operation (<b>Y &gt; X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (ULONG) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::greatThan (const SlipDatum& Y, const ULONG X) const { // operator>
      return false;
   }; // bool SlipPtrOp::greatThan (const SlipDatum& Y, const ULONG X) const


   /**
    * @brief A logical comparison operation (<b>Y != X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (LONG) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::notEqual (const SlipDatum& Y, const LONG X) const { // operator!=
      return false;
   }; // bool SlipPtrOp::notEqual (const SlipDatum& Y, const LONG X) const

   /**
    * @brief A logical comparison operation (<b>Y &lt; X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (LONG) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::lessThan (const SlipDatum& Y, const LONG X) const { // operator<
      return false;
   }; // bool SlipPtrOp::lessThan (const SlipDatum& Y, const LONG X) const

   /**
    * @brief A logical comparison operation (<b>Y &lt;= X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (LONG) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::lessEqual (const SlipDatum& Y, const LONG X) const { // operator<=
      return false;
   }; // bool SlipPtrOp::lessEqual (const SlipDatum& Y, const LONG X) const

   /**
    * @brief A logical comparison operation (<b>Y == X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (LONG) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::equal (const SlipDatum& Y, const LONG X) const { // operator==
      return false;
   }; // bool SlipPtrOp::equal (const SlipDatum& Y, const LONG X) const

   /**
    * @brief A logical comparison operation (<b>Y &gt;= X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (LONG) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::greatEqual(const SlipDatum& Y, const LONG X) const { // operator>=
      return false;
   }; // bool SlipPtrOp::greatEqual(const SlipDatum& Y, const LONG X) const

   /**
    * @brief A logical comparison operation (<b>Y &gt; X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (LONG) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::greatThan (const SlipDatum& Y, const LONG X) const { // operator>
      return false;
   }; // bool SlipPtrOp::greatThan (const SlipDatum& Y, const LONG X) const


   /**
    * @brief A logical comparison operation (<b>Y != X</b>)
    * <p>Compare the SlipPointer* values and if not equal, return <b>true</b>.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (PTR) right comparand
    * @return (bool) <b>true</b> if SlipPointer* values not the same
    */
   bool SlipPtrOp::notEqual (const SlipDatum& Y, const PTR X) const { // operator!=
      return !(*getPtr(Y) == X);
   }; // bool SlipPtrOp::notEqual (const SlipDatum& Y, const PTR X) const

   /**
    * @brief A logical comparison operation (<b>Y &lt; X</b>)
    * <p>Compare the SlipPointer* values and if equal, return <b>true</b>.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (PTR) right comparand
    * @return (bool) <b>true</b> if SlipPointer* values the  same
    */
   bool SlipPtrOp::equal (const SlipDatum& Y, const PTR X) const { // operator==
      return (*getPtr(Y) == X);
   }; // bool SlipPtrOp::equal (const SlipDatum& Y, const PTR X) const


   /**
    * @brief A logical comparison operation (<b>Y != X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (string&) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::notEqual (const SlipDatum& Y, const string& X) const { // operator!=
       return false;
   }; // bool SlipPtrOp::notEqual (const SlipDatum& Y, const string& X) const

   /**
    * @brief A logical comparison operation (<b>Y &lt; X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (string&) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::lessThan (const SlipDatum& Y, const string& X) const { // operator<
       return false;
   }; // bool SlipPtrOp::equal (const SlipDatum& Y, const string& X) const

   /**
    * @brief A logical comparison operation (<b>Y &lt;= X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (string&) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::lessEqual (const SlipDatum& Y, const string& X) const { // operator<=
       return false;
   }; // bool SlipPtrOp::notEqual (const SlipDatum& Y, const string& X) const

   /**
    * @brief A logical comparison operation (<b>Y == X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (string&) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::equal (const SlipDatum& Y, const string& X) const { // operator==
       return false;
   }; // bool SlipPtrOp::equal (const SlipDatum& Y, const string& X) const

   /**
    * @brief A logical comparison operation (<b>Y &gt;= X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (string&) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::greatEqual (const SlipDatum& Y, const string& X) const { // operator>=
       return false;
   }; // bool SlipPtrOp::notEqual (const SlipDatum& Y, const string& X) const

   /**
    * @brief A logical comparison operation (<b>Y &gt; X</b>)
    * <p>There is no mechanism to do this comparison.</p>
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (string&) right comparand
    * @return (bool) <b>false</b>
    */
   bool SlipPtrOp::greatThan (const SlipDatum& Y, const string& X) const { // operator>
       return false;
   }; // bool SlipPtrOp::equal (const SlipDatum& Y, const string& X) const

             /**********************************************
              *         non-commutative Operators          *
              **********************************************/

   /**
    * @brief Subtract (<b>LONG</b> - <b>X</b>) a LONG and a PTR
    * <p>After widening to a LONG, this method is called to support a subtraction
    *    operation involving a User Defined Data type.</p>
    * <p>User Defined Data types must override this method if the default action
    *    is not satisfactory.</p>
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
   SlipDatum SlipPtrOp::subOP(const LONG Y, const SlipDatum& X) const {
      postError(__FILE__, __LINE__, SlipErr::E1006, "subOP", "", "Operation not supported.", X);
      return SlipDatum((LONG)0);
   }; // SlipDatum SlipPtrOp::subOP(const LONG Y, const SlipDatum X)

   /**
    * @brief Subtract (<b>ULONG</b> - <b>X</b>) a ULONG and a PTR
    * <p>After widening to a LONG, this method is called to support a subtraction
    *    operation involving a User Defined Data type.</p>
    * <p>User Defined Data types must override this method if the default action
    *    is not satisfactory.</p>
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
   SlipDatum SlipPtrOp::subUOP(const ULONG Y, const SlipDatum& X) const {
      postError(__FILE__, __LINE__, SlipErr::E1006, "subUOP", "", "Operation not supported.", X);
      return SlipDatum((ULONG)0);
   }; // SlipDatum SlipPtrOp::subUOP(const ULONG Y, const SlipDatum X)

   /**
    * @brief Subtract (<b>Y</b> - <b>X</b>) a DOUBLE and a PTR
    * <p>After widening to a LONG, this method is called to support a subtraction
    *    operation involving a User Defined Data type.</p>
    * <p>User Defined Data types must override this method if the default action
    *    is not satisfactory.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E1006</b> Invalid '-' Operation for X. Operation not supported.</li>
    * </ul>
    * @see SlipDoubleOp::sub(SlipDatum&, SlipDatum&)
    * @param[in] Y (LONG)LHS of Y - X
    * @param[in] X (SlipDatum) RHS of Y - X
    * @return (SlipDatum) type DOUBLE if Y is a DOUBLE, otherwise LONG
    */
   SlipDatum SlipPtrOp::subDOP(const DOUBLE Y, const SlipDatum& X) const {
      postError(__FILE__, __LINE__, SlipErr::E1006, "subDOP", "", "Operation not supported.", X);
      return SlipDatum((DOUBLE)0);
   }; // SlipDatum SlipPtrOp::subDOP(const ULONG Y, const SlipDatum X)

   /**
    * @brief Divide (<b>LONG</b> / <b>X</b>) a LONG and a PTR
    * <p>After widening to a LONG, this method is called to support a division
    *    operation involving a User Defined Data type.</p>
    * <p>User Defined Data types must override this method if the default action
    *    is not satisfactory.</p>
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
   SlipDatum SlipPtrOp::divOP(const LONG Y, const SlipDatum& X) const {
      postError(__FILE__, __LINE__, SlipErr::E1008, "divOP", "", "Operation not supported.", X);
      return SlipDatum((LONG)0);
   }; // SlipDatum SlipPtrOp::divOP(const LONG Y, const SlipDatum X)

   /**
    * @brief Divide (<b>ULONG</b> / <b>X</b>) a LONG and a PTR
    * <p>After widening to a ULONG, this method is called to support a division
    *    operation involving a User Defined Data type.</p>
    * <p>User Defined Data types must override this method if the default action
    *    is not satisfactory.</p>
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
   SlipDatum SlipPtrOp::divUOP(const ULONG Y, const SlipDatum& X) const {
      postError(__FILE__, __LINE__, SlipErr::E1008, "divUOP", "", "Operation not supported.", X);
      return SlipDatum((LONG)0);
   }; // SlipDatum SlipPtrOp::divUOP(const LONG Y, const SlipDatum X)

   /**
    * @brief Divide a DOUBLE and a PTR
    * <p>User Defined Data types must override this method if the default action
    *    is not satisfactory.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E1008</b> Invalid '/' Operation for X. Operation not supported.</li>
    * </ul>
    * @see SlipDoubleOp::div(SlipDatum&, SlipDatum&)
    * @param[in] Y (LONG)LHS of Y / X
    * @param[in] X (SlipDatum) RHS of Y / X
    * @return (SlipDatum((DOUBLE)) result of DOUBLE / SlipDatum
    */
   SlipDatum SlipPtrOp::divDOP(const DOUBLE Y, const SlipDatum& X) const {
      postError(__FILE__, __LINE__, SlipErr::E1008, "divDOP", "", "Operation not supported.", X);
      return SlipDatum((DOUBLE)0);
   }; // SlipDatum SlipPtrOp::divDOP(const LONG Y, const SlipDatum X)

   /**
    * @brief Take the modulus (<b>LONG</b> % <b>X</b>) a LONG and a PTR
    * <p>After widening to a LONG, this method is called to support the modulus
    *    operation with a User Defined Data type.</p>
    * <p>User Defined Data types must override this method if the default action
    *    is not satisfactory.</p>
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
   SlipDatum SlipPtrOp::modOP(const LONG Y, const SlipDatum& X) const {
      postError(__FILE__, __LINE__, SlipErr::E1026, "modOP", "", "Operation not supported.", X);
      return SlipDatum((LONG)0);
   }; // SlipDatum SlipPtrOp::modOP(const LONG Y, const SlipDatum X)

   /**
    * @brief Take the modulus (<b>ULONG</b> % <b>X</b>)
    * <p>After widening to a ULONG, this method is called to support the modulus
    *    operation with a User Defined Data type.</p>
    * <p>User Defined Data types must override this method if the default action
    *    is not satisfactory.</p>
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
   SlipDatum SlipPtrOp::modUOP(const ULONG Y, const SlipDatum& X) const {
      postError(__FILE__, __LINE__, SlipErr::E1026, "modUOP", "", "Operation not supported.", X);
      return SlipDatum((ULONG)0);
   }; // SlipDatum SlipPtrOp::modUOP(const ULONG Y, const SlipDatum X)

   /**
    * @brief Shift left (<b>LONG</b> &lt;&lt; <b>X</b>)
    * <p>After widening  to a LONG the shift left operation is with the RHS
    *    being a User Defined Data type.</p>
    * <p>User Defined Data types must override this method if the default action
    *    is not satisfactory.</p>
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
   LONG SlipPtrOp::shlOP(const LONG Y, const SlipDatum& X) const {              // num << X
      postError(__FILE__, __LINE__, SlipErr::E1020, "shlOP", "", "Operation not supported.", X);
      return 0;
   }; // LONG SlipPtrOp::shlOP(const LONG Y, const SlipDatum X)

   /**
    * @brief Shift right (<b>LONG</b> &gt;&gt; <b>X</b>)
    * <p>After widening  to a LONG the shift right operation is with the RHS
    *    being a User Defined Data type.</p>
    * <p>User Defined Data types must override this method if the default action
    *    is not satisfactory.</p>
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
   LONG SlipPtrOp::shrOP(const LONG Y, const SlipDatum& X) const {              // num >> X
      postError(__FILE__, __LINE__, SlipErr::E1021, "shrOP", "", "Operation not supported.", X);
      return 0;
   }; // LONG SlipPtrOp::shrOP(const LONG Y, const SlipDatum X)

               /*****************************************
                *             Miscellaneous             *
               ******************************************/

   SlipDatum& SlipPtrOp::copy(const SlipDatum& X) const {                       // Return a reference to a copy of the current cell
      return const_cast<SlipDatum&>(X);
   }; // SlipDatum&  SlipPtrOp::copy(const SlipDatum& X) const

   void SlipPtrOp::remove(const SlipDatum& X) {                                 // Delete the datum in this cell
      PTR ptr = *getPtr(X);
      ptr->remove();
   }; // voidvSlipPtrOp::remove(const SlipDatum& X)

   sAllData SlipPtrOp::getDatumCopy(const SlipDatum& X) const {                 // Return valid datum
      Data data = { 0, };
      data.Ptr = (*getPtr(X))->copy();
      return data.allData;
   }; // sAllData SlipPtrOp::getDatumCopy(SlipDatum& X) const

   /**
    * @brief Return the instance name of the PTR object.
    * @param X (SlipDatum&) object reference
    * @return name
    */
   const string SlipPtrOp::getName(SlipDatum* X) const {                       // string representing SlipDatumCell
      return *(*getPtr(*X))->getName();
   }; // const string SlipPtrOp::getName(SlipDatum& X) const

   /**
    * @brief Retrieve a pointer to the User Data parse function.
    * <p>Each User Data object has a parse method required to process data
    *    input from a list definition file. Each time a reference is seen
    *    to a named User Data definition, the parse method is called for
    *    the User Data with a list of attributes needed to populate a
    *    User Data object. The parse method creates an object and populates
    *    it with the data.</p>
    * @return (Parse) Pointer to the User Data parse function or <b>null</b>
    */
   Parse SlipPtrOp::getParse(SlipDatum& X) {                                    // return a pointer to the parse method
      return (*getPtr(X))->getParse();
   }; // Parse SlipPtrOp::getParse(SlipDatum& X)

   string SlipPtrOp::toString(const SlipCell& X) const { // pretty print SLIP cell
      PTR ptr = *getString(X);
      return ptr->toString();
   }; // string SlipPtrOp::toString(const SlipCell& X) const

   string SlipPtrOp::dump(const SlipCell& X)const  {
      stringstream pretty;
      PTR ptr = *getString(X);
      pretty << "[pointer  ] " << dumpLink(X)  << " = "
              << setfill('0') << setw(8) << hex << ptr
              << " -> ";
      pretty << ptr->dump();
      return pretty.str();
   }; // string SlipPtrOp::dump(const SlipCell& X) const

   string SlipPtrOp::write(const SlipCell& X) const {
      return (*getPtr(X))->write();
   }; // string SlipPtrOp::write(const SlipCell& X) const

}; // namespace slip