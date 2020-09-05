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
 * @file    SlipUndefinedOp.cpp
 * @author  A. Schwarz
 * @date    June 04, 2012
 *
 * @brief Available Space List (AVSL) implmeentation for cell operations
 *
 */

 # include <string>
 # include <sstream>
 # include "SlipDatum.h"
 # include "SlipDef.h"
 # include "SlipErr.h"
 # include "SlipUndefinedOp.h"

using namespace slip;

namespace slip {

   /*********************************************************
    *              Constructors/Destructors                 *
   *********************************************************/
   /**
    * @brief Copy Constructor.
    * <p>It is illegal to use an object of the class as an argument in a
    *    method.</p>
    * @param[in] orig (SlipUndefinedOp&) reference to an object of the class
    */
   SlipUndefinedOp::SlipUndefinedOp(const SlipUndefinedOp& orig) { };

   /**
    * @brief Destructor.
    * <p>This class is const static and will not be deleted.</p>
    */
   SlipUndefinedOp::~SlipUndefinedOp() { };

   /**
    * @brief Constructor for class object.
    * <p>The input arguments are transmitted to the SlipOp base constructor
    *    for insertion as object constants.</p>
    * <p>There is only one object of this class created as const static.</p>
    */
   SlipUndefinedOp::SlipUndefinedOp()
                         : SlipOp( 0                            //!< cellType
                                 , 0                            //!< ID
                                 , eUNDEFINED                   //!< classType
                                 , "UNDEFINED"                  //!< name
                                 , false                        //!< isDataFlag
                                 , false                        //!< isDiscreteFlag
                                 , false                        //!< isHeaderFlag
                                 , false                        //!< isNumberFlag
                                 , false) { }                   //!< isSublistFlag

   /*********************************************************
    *                  Casting Operators                    *
   *********************************************************/

   /**
    * @brief Casting to a bool is illegal.
    * @param[in] X (SlipDatum&) object to be cast
    * @return false
    */
   bool SlipUndefinedOp::cast_bool(const SlipDatum& X) const  { // (bool) X
      postError(__FILE__, __LINE__, SlipErr::E1035, "SlipUndefinedOp::(bool)", "", "", X);
      return (bool) false;
   }; // bool SlipUndefinedOp::cast_bool(const SlipDatum& X) const

   /**
    * @brief Casting to a UCHAR is illegal.
    * @param[in] X (SlipDatum&) object to be cast
    * @return (UCHAR) 0
    */
   UCHAR SlipUndefinedOp::cast_UCHAR(const SlipDatum& X) const  { // (UCHAR) X
      postError(__FILE__, __LINE__, SlipErr::E1036, "SlipUndefinedOp(UCHAR)", "", "Deleted cell.", X);
      return 0;
   }; // UCHAR SlipUndefinedOp::cast_UCHAR(const SlipDatum& X) const

   /**
    * @brief Casting to a CHAR is illegal.
    * @param[in] X (SlipDatum&) object to be cast
    * @return (CHAR) 0
    */
   CHAR SlipUndefinedOp::cast_CHAR(const SlipDatum& X) const  { // (CHAR) X
      postError(__FILE__, __LINE__, SlipErr::E1037, "SlipUndefinedOp(CHAR)", "", "Deleted cell.", X);
      return 0;
   }; // CHAR SlipUndefinedOp::cast_CHAR(const SlipDatum& X) const

   /**
    * @brief Casting to a ULONG is illegal.
    * @param[in] X (SlipDatum&) object to be cast
    * @return (ULONG) 0
    */
   ULONG SlipUndefinedOp::cast_ULONG(const SlipDatum& X) const  { // X (ULONG)
      postError(__FILE__, __LINE__, SlipErr::E1038, "SlipUndefinedOp(ULONG)", "", "Deleted cell.", X);
      return 0;
   }; // ULONG SlipUndefinedOp::cast_ULONG(const SlipDatum& X) const

   /**
    * @brief Casting to a LONG is illegal.
    * @param[in] X (SlipDatum&) object to be cast
    * @return (LONG) 0
    */
   LONG SlipUndefinedOp::cast_LONG(const SlipDatum& X) const  { // X (LONG)
      postError(__FILE__, __LINE__, SlipErr::E1039, "SlipUndefinedOp(LONG)", "", "Deleted cell.", X);
      return 0;
   }; // LONG SlipUndefinedOp::cast_LONG(const SlipDatum& X) const

   /**
    * @brief Casting to a DOUBLE is illegal.
    * @param[in] X (SlipDatum&) object to be cast
    * @return (DOUBLE) 0
    */
   DOUBLE SlipUndefinedOp::cast_DOUBLE(const SlipDatum& X) const  { // X (DOUBLE)
      postError(__FILE__, __LINE__, SlipErr::E1040, "SlipUndefinedOp(DOUBLE)", "", "Deleted cell.", X);
      return 0.0;
   }; // DOUBLE SlipUndefinedOp::cast_DOUBLE(const SlipDatum& X) const
   PTR SlipUndefinedOp::cast_PTR(const SlipDatum& Y) const {                    // (PTR) Y
      postError(__FILE__, __LINE__, SlipErr::E1040, "SlipUndefinedOp(DOUBLE)", "", "Deleted cell.", Y);
      return NULL;
   }; // PTR SlipUndefinedOp::cast_PTR(const SlipDatum& Y) const

      /*********************************************************
       *                 Operator Overloading                  *
      *********************************************************/

          /*************************************************
           *                Unary operators                *
          *************************************************/

   /**
    * @brief A bitwise 'not' (~) is illegal.
    * @param[in] X (SlipDatum&) object to be cast
    * @return (ULONG) 0
    */
   ULONG SlipUndefinedOp::bitNOT (const SlipDatum& X) const { // operator~
      postError(__FILE__, __LINE__, SlipErr::E1043, "SlipUndefinedOpoperato~r", "", "Deleted cell.", X);
      return 0;
   }; // ULONG SlipUndefinedOp::bitNOT (const SlipDatum& X) const

   /**
    * @brief A logical 'not' (!) is illegal.
    * @param[in] X (SlipDatum&) object to be cast
    * @return (bool) 0
    */
   bool SlipUndefinedOp::logNOT (const SlipDatum& X) const { // operator!
      postError(__FILE__, __LINE__, SlipErr::E1025, "SlipUndefinedOp!", "", "Deleted cell.", X);
      return false;
   }; // bool SlipUndefinedOp::logNOT (const SlipDatum& X) const

   /**
    * @brief A unary plus (+) is illegal.
    * @param[in] X (SlipDatum&) object to be cast
    * @return X (SlipDatum)
    */
   SlipDatum SlipUndefinedOp::plus (const SlipDatum& X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1001, "SlipUndefinedOp+", "", "Deleted cell.", X);
      return X;
   }; // SlipDatum SlipUndefinedOp::plus (const SlipDatum& X) const

   /**
    * @brief A unary minus (-) is illegal.
    * @param[in] X (SlipDatum&) object to be cast
    * @return X (SlipDatum)
    */
   SlipDatum SlipUndefinedOp::minus (const SlipDatum& X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1002, "SlipUndefinedOp-", "", "Deleted cell.", X);
      return X;
   }; // SlipDatum SlipUndefinedOp::minus (const SlipDatum& X) const

   /**
    * @brief A unary post increment (\++) is illegal.
    * @param[in] X (SlipDatum&) object to be cast
    * @return X (SlipDatum)
    */
   SlipDatum& SlipUndefinedOp::postInc (SlipDatum& X) const { // operator++
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipUndefinedOp(int)++", "", "Deleted cell.", X);
      return X;
   }; // SlipDatum& SlipUndefinedOp::postInc (SlipDatum& X) const

   /**
    * @brief A unary post decrement (\--) is illegal.
    * @param[in] X (SlipDatum&) object to be cast
    * @return X (SlipDatum)
    */
   SlipDatum& SlipUndefinedOp::postDec (SlipDatum& X) const { // operator--
      postError(__FILE__, __LINE__, SlipErr::E1004, "SlipUndefinedOp(int)--", "", "Deleted cell.", X);
      return X;
   }; // SlipDatum SlipUndefinedOp::postDec (SlipDatum& X) const

   /**
    * @brief A pre-increment (\++) is illegal.
    * @param[in] X (SlipDatum&) object to be cast
    * @return X (SlipDatum)
    */
   SlipDatum& SlipUndefinedOp::preInc (SlipDatum& X) const { // ++operator
      postError(__FILE__, __LINE__, SlipErr::E1003, "SlipUndefinedOp++", "", "Deleted cell.", X);
      return X;
   }; // SlipDatum& SlipUndefinedOp::preInc (SlipDatum& X) const

   /**
    * @brief A pre-decrement (\--) is illegal.
    * @param[in] X (SlipDatum&) object to be cast
    * @return X (SlipDatum)
    */
   SlipDatum& SlipUndefinedOp::preDec (SlipDatum& X) const { // --operator
      postError(__FILE__, __LINE__, SlipErr::E1004, "SlipUndefinedOp--", "", "Deleted cell.", X);
      return X;
   }; // SlipDatum& SlipUndefinedOp::preDec (SlipDatum& X) const

          /*************************************************
           *                Binary operators               *
          *************************************************/
   /**
    * @brief An addition operation (<b>Y + X</b>) is illegal
    * @param[in] Y (SlipDatum&) left addend
    * @param[in] X (SlipDatum&) right addend
    * @return Y (SlipDatum)
    */
   SlipDatum SlipUndefinedOp::add(const SlipDatum& Y, const SlipDatum& X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1005, "SlipUndefinedOp+", "", "Deleted cell.", X);
      return Y;
   }; // SlipDatum SlipUndefinedOp::add(const SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A subtraction operation (<b>Y - X</b>) is illegal
    * @param[in] Y (SlipDatum&) left subtrahend
    * @param[in] X (SlipDatum&) right subtrahend
    * @return Y (SlipDatum)
    */
   SlipDatum SlipUndefinedOp::sub(const SlipDatum& Y, const SlipDatum& X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1006, "SlipUndefinedOp-", "", "Deleted cell.", X);
      return Y;
   }; // SlipDatum SlipUndefinedOp::sub(const SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A multiplication operation (<b>Y * X</b>) is illegal
    * @param[in] Y (SlipDatum&) left multiplicand
    * @param[in] X (SlipDatum&) right multiplicand
    * @return Y (SlipDatum)
    */
   SlipDatum SlipUndefinedOp::mul(const SlipDatum& Y, const SlipDatum& X) const { // operator*
      postError(__FILE__, __LINE__, SlipErr::E1007, "SlipUndefinedOp*", "", "Deleted cell.", X);
      return Y;
   }; // SlipDatum SlipUndefinedOp::mul(const SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A division operation (<b>Y / X</b>) is illegal
    * @param[in] Y (SlipDatum&) dividend
    * @param[in] X (SlipDatum&) divisor
    * @return Y (SlipDatum)
    */
   SlipDatum SlipUndefinedOp::div(const SlipDatum& Y, const SlipDatum& X) const { // operator/
      postError(__FILE__, __LINE__, SlipErr::E1008, "SlipUndefinedOp/", "", "Deleted cell.", X);
      return Y;
   }; // SlipDatum SlipUndefinedOp::div(const SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A modulus operation (<b>Y % X</b>) is illegal
    * @param[in] Y (SlipDatum&) dividend
    * @param[in] X (SlipDatum&) modulus
    * @return (LONG) 0
    */
   LONG SlipUndefinedOp::mod(const SlipDatum& Y, const SlipDatum& X) const { // operator%
      postError(__FILE__, __LINE__, SlipErr::E1026, "SlipUndefinedOp%", "", "Deleted cell.", Y, X);
      return 0;
   }; // SlipDatum SlipUndefinedOp::mod(const SlipDatum& Y, const SlipDatum& X) const


   /**
    * @brief A addition operation (<b>Y + X</b>) is illegal
    * @param[in] Y (SlipDatum&) left addend
    * @param[in] X (DOUBLE) right addend
    * @return Y (SlipDatum)
    */
   SlipDatum SlipUndefinedOp::add(const SlipDatum& Y, const DOUBLE X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1005, "SlipUndefinedOp+", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum SlipUndefinedOp::add(const SlipDatum& Y, const DOUBLE X) const

   /**
    * @brief A subtraction operation (<b>Y - X</b>) is illegal
    * @param[in] Y (SlipDatum&) left subtrahend
    * @param[in] X (DOUBLE) right subtrahend
    * @return Y (SlipDatum)
    */
   SlipDatum SlipUndefinedOp::sub(const SlipDatum& Y, const DOUBLE X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1006, "SlipUndefinedOp-", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum SlipUndefinedOp::sub(const SlipDatum& Y, const DOUBLE X) const

   /**
    * @brief A multiplication operation (<b>Y * X</b>) is illegal
    * @param[in] Y (SlipDatum&) left multiplicand
    * @param[in] X (DOUBLE) right multiplicand
    * @return Y (SlipDatum)
    */
   SlipDatum SlipUndefinedOp::mul(const SlipDatum& Y, const DOUBLE X) const { // operator*
      postError(__FILE__, __LINE__, SlipErr::E1007, "SlipUndefinedOp*", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum SlipUndefinedOp::mul(const SlipDatum& Y, const DOUBLE X) const

   /**
    * @brief A division operation (<b>Y / X</b>) is illegal
    * @param[in] Y (SlipDatum&) dividend
    * @param[in] X (DOUBLE) divisor
    * @return Y (SlipDatum)
    */
   SlipDatum SlipUndefinedOp::div(const SlipDatum& Y, const DOUBLE X) const { // operator/
      postError(__FILE__, __LINE__, SlipErr::E1008, "SlipUndefinedOp/", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum SlipUndefinedOp::div(const SlipDatum& Y, const DOUBLE X) const


   /**
    * @brief An addition operation (<b>Y + X</b>) is illegal
    * @param[in] Y (SlipDatum&) left addend
    * @param[in] X (ULONG) right addend
    * @return Y (SlipDatum)
    */
   SlipDatum SlipUndefinedOp::add(const SlipDatum& Y, const ULONG X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1005, "SlipUndefinedOp+", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum SlipUndefinedOp::add(const SlipDatum& Y, const ULONG X) const

   /**
    * @brief A subtraction operation (<b>Y - X</b>) is illegal
    * @param[in] Y (SlipDatum&) left subtrahend
    * @param[in] X (ULONG) right subtraction
    * @return Y (SlipDatum)
    */
   SlipDatum SlipUndefinedOp::sub(const SlipDatum& Y, const ULONG X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1006, "SlipUndefinedOp-", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum SlipUndefinedOp::sub(const SlipDatum& Y, const ULONG X) const

   /**
    * @brief A multiplication operation (<b>Y * X</b>) is illegal
    * @param[in] Y (SlipDatum&) left multiplicand
    * @param[in] X (ULONG) right multiplicand
    * @return Y (SlipDatum)
    */
   SlipDatum SlipUndefinedOp::mul(const SlipDatum& Y, const ULONG X) const { // operator*
      postError(__FILE__, __LINE__, SlipErr::E1007, "SlipUndefinedOp*", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum SlipUndefinedOp::mul(const SlipDatum& Y, const ULONG X) const

   /**
    * @brief A division operation (<b>Y / X</b>) is illegal
    * @param[in] Y (SlipDatum&) dividend
    * @param[in] X (ULONG) divisor
    * @return Y (SlipDatum)
    */
   SlipDatum SlipUndefinedOp::div(const SlipDatum& Y, const ULONG X) const { // operator/
      postError(__FILE__, __LINE__, SlipErr::E1008, "SlipUndefinedOp/", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum SlipUndefinedOp::div(const SlipDatum& Y, const ULONG X) const

   /**
    * @brief A modulus operation (<b>Y % X</b>) is illegal
    * @param[in] Y (SlipDatum&) dividend
    * @param[in] X (ULONG) divisor
    * @return (LONG) 0
    */
   LONG       SlipUndefinedOp::mod(const SlipDatum& Y, const ULONG X) const { // operator%
      postError(__FILE__, __LINE__, SlipErr::E1026, "%", "", "Deleted cell.", Y);
      return 0;
   }; // SlipDatum SlipUndefinedOp::mod(const SlipDatum& Y, const ULONG X) const


   /**
    * @brief An addition operation (<b>Y + X</b>) is illegal
    * @param[in] Y (SlipDatum&) left addend
    * @param[in] X (LONG) right addend
    * @return Y (SlipDatum)
    */
   SlipDatum SlipUndefinedOp::add(const SlipDatum& Y, const LONG X) const { // operator+
      postError(__FILE__, __LINE__, SlipErr::E1005, "SlipUndefinedOp+", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum SlipUndefinedOp::add(const SlipDatum& Y, const LONG X) const

   /**
    * @brief A subtraction operation (<b>Y - X</b>) is illegal
    * @param[in] Y (SlipDatum&) left subtrahend
    * @param[in] X (LONG) right subtrahend
    * @return Y (SlipDatum)
    */
   SlipDatum SlipUndefinedOp::sub(const SlipDatum& Y, const LONG X) const { // operator-
      postError(__FILE__, __LINE__, SlipErr::E1006, "SlipUndefinedOp-", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum SlipUndefinedOp::sub(const SlipDatum& Y, const LONG X) const

   /**
    * @brief A multiplicaiton operation (<b>Y * X</b>) is illegal
    * @param[in] Y (SlipDatum&) multiplicand
    * @param[in] X (LONG) right multiplier
    * @return Y (SlipDatum)
    */
   SlipDatum SlipUndefinedOp::mul(const SlipDatum& Y, const LONG X) const { // operator*
      postError(__FILE__, __LINE__, SlipErr::E1007, "SlipUndefinedOp*", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum SlipUndefinedOp::mul(const SlipDatum& Y, const LONG X) const

   /**
    * @brief A division operation (<b>Y / X</b>) is illegal
    * @param[in] Y (SlipDatum&) dividend
    * @param[in] X (LONG) divisor
    * @return Y (SlipDatum)
    */
   SlipDatum SlipUndefinedOp::div(const SlipDatum& Y, const LONG X) const { // operator/
      postError(__FILE__, __LINE__, SlipErr::E1008, "SlipUndefinedOp/", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum SlipUndefinedOp::div(const SlipDatum& Y, const LONG X) const

   /**
    * @brief A modulus operation (<b>Y % X</b>) is illegal
    * @param[in] Y (SlipDatum&) dividend
    * @param[in] X (LONG) modulus
    * @return Y (LONG)
    */
   LONG       SlipUndefinedOp::mod(const SlipDatum& Y, const LONG X) const { // operator%
      postError(__FILE__, __LINE__, SlipErr::E1026, "SlipUndefinedOp%", "", "Deleted cell.", Y);
      return 0;
   }; // SlipDatum SlipUndefinedOp::mod(const SlipDatum& Y, const LONG X) const

          /*************************************************
           *                Bit operators                  *
          *************************************************/

   /**
    * @brief A bitwise shift left operation (<b>Y &lt;&lt; X</b>) is illegal
    * @param[in] Y (SlipDatum&) bit string
    * @param[in] X (SlipDatum&) shift amount
    * @return (LONG) 0
    */
   LONG  SlipUndefinedOp::shl (const SlipDatum& Y, const SlipDatum& X) const { // operator<<
      postError(__FILE__, __LINE__, SlipErr::E1020, "SlipUndefinedOp<<", "", "Deleted cell.", (SlipCellBase&)Y, X);
      return 0;
   }; // LONG  SlipUndefinedOp::shl (const SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A bitwise shift right operation (<b>Y &gt;&gt; X</b>) is illegal
    * @param[in] Y (SlipDatum&) bit string
    * @param[in] X (SlipDatum&) shift amount
    * @return (LONG) 0
    */
   LONG  SlipUndefinedOp::shr (const SlipDatum& Y, const SlipDatum& X) const { // operator>>
      postError(__FILE__, __LINE__, SlipErr::E1021, "SlipUndefinedOp>>", "", "Deleted cell.", (SlipCellBase&)Y, X);
      return 0;
   }; // LONG  SlipUndefinedOp::shr (const SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A bitwise and operation (<b>Y &amp; X</b>) is illegal
    * @param[in] Y (SlipDatum&) bit string
    * @param[in] X (SlipDatum&) bit stringt
    * @return (LONG) 0
    */
   LONG  SlipUndefinedOp::band(const SlipDatum& Y, const SlipDatum& X) const { // operator&
      postError(__FILE__, __LINE__, SlipErr::E1024, "SlipUndefinedOp&", "", "Deleted cell.", (SlipCellBase&)Y, X);
      return 0;
   }; // LONG  SlipUndefinedOp::band(const SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A bitwise 'or' operation (<b>Y \| X</b>) is illegal
    * @param[in] Y (SlipDatum&) bit string
    * @param[in] X (SlipDatum&) shift amount
    * @return (LONG) 0
    */
   LONG  SlipUndefinedOp::bor (const SlipDatum& Y, const SlipDatum& X) const { // operator|
      postError(__FILE__, __LINE__, SlipErr::E1023, "SlipUndefinedOp|", "", "Deleted cell.", (SlipCellBase&)Y, X);
      return 0;
   }; // LONG  SlipUndefinedOp::bor (const SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A bitwise 'exclusive or' operation (<b>Y ^ X</b>) is illegal
    * @param[in] Y (SlipDatum&) bit string
    * @param[in] X (SlipDatum&) shift amount
    * @return (LONG) 0
    */
   LONG  SlipUndefinedOp::bxor(const SlipDatum& Y, const SlipDatum& X) const { // operator^
      postError(__FILE__, __LINE__, SlipErr::E1022, "SlipUndefinedOp^", "", "Deleted cell.", (SlipCellBase&)Y, X);
      return 0;
   }; // LONG  SlipUndefinedOp::bxor(const SlipDatum& Y, const SlipDatum& X) const


   /**
    * @brief A bitwise shift left operation (<b>Y &lt;&lt; X</b>) is illegal
    * @param[in] Y (SlipDatum&) bit string
    * @param[in] X (ULONG) shift amount
    * @return (LONG) 0
    */
   LONG  SlipUndefinedOp::shl (const SlipDatum& Y, const ULONG X) const { // operator<<
      postError(__FILE__, __LINE__, SlipErr::E1020, "", "Deleted cell.", "", Y);
      return 0;
   }; // LONG  SlipUndefinedOp::shl (const SlipDatum& Y, const ULONG X) const

   /**
    * @brief A bitwise shift right operation (<b>Y &gt;&gt; X</b>) is illegal
    * @param[in] Y (SlipDatum&) bit string
    * @param[in] X (ULONG) shift amount
    * @return (LONG) 0
    */
   LONG  SlipUndefinedOp::shr (const SlipDatum& Y, const ULONG X) const { // operator>>
      postError(__FILE__, __LINE__, SlipErr::E1021, "", "Deleted cell.", "", Y);
      return 0;
   }; // LONG  SlipUndefinedOp::shr (const SlipDatum& Y, const ULONG X) const

   /**
    * @brief A bitwise 'and' operation (<b>Y &amp; X</b>) is illegal
    * @param[in] Y (SlipDatum&) bit string
    * @param[in] X (ULONG) bit string
    * @return (LONG) 0
    */
   LONG  SlipUndefinedOp::band(const SlipDatum& Y, const ULONG X) const { // operator&
      postError(__FILE__, __LINE__, SlipErr::E1024, "", "Deleted cell.", "", Y);
      return 0;
   }; // LONG  SlipUndefinedOp::band(const SlipDatum& Y, const ULONG X) const

   /**
    * @brief A bitwise 'or' operation (<b>Y \| X</b>) is illegal
    * @param[in] Y (SlipDatum&) bit string
    * @param[in] X (ULONG) bit string
    * @return (LONG) 0
    */
   LONG  SlipUndefinedOp::bor (const SlipDatum& Y, const ULONG X) const { // operator|
      postError(__FILE__, __LINE__, SlipErr::E1023, "", "Deleted cell.", "", Y);
      return 0;
   }; // LONG  SlipUndefinedOp::bor (const SlipDatum& Y, const ULONG X) const

   /**
    * @brief A bitwise 'exclusive or' operation (<b>Y ^ X</b>) is illegal
    * @param[in] Y (SlipDatum&) bit string
    * @param[in] X (ULONG) bit string
    * @return (LONG) 0
    */
   LONG  SlipUndefinedOp::bxor(const SlipDatum& Y, const ULONG X) const { // operator^
      postError(__FILE__, __LINE__, SlipErr::E1022, "SlipUndefinedOp^", "", "Deleted cell.", Y);
      return 0;
   }; // LONG  SlipUndefinedOp::bxor(const SlipDatum& Y, const ULONG X) const


   /**
    * @brief A bitwise shift left operation (<b>Y &lt;&lt; X</b>) is illegal
    * @param[in] Y (SlipDatum&) bit string
    * @param[in] X (LONG) shift amount
    * @return (LONG) 0
    */
   LONG  SlipUndefinedOp::shl (const SlipDatum& Y, const LONG X) const { // operator<<
      postError(__FILE__, __LINE__, SlipErr::E1020, "SlipUndefinedOp<<", "", "Deleted cell.", Y);
      return 0;
   }; // LONG  SlipUndefinedOp::shl (const SlipDatum& Y, const LONG X) const

   /**
    * @brief A bitwise shift right operation (<b>Y &gt;&gt; X</b>) is illegal
    * @param[in] Y (SlipDatum&) bit string
    * @param[in] X (LONG) shift amount
    * @return (LONG) 0
    */
   LONG  SlipUndefinedOp::shr (const SlipDatum& Y, const LONG X) const { // operator>>
      postError(__FILE__, __LINE__, SlipErr::E1021, "SlipUndefinedOp>>", "", "Deleted cell.", Y);
      return 0;
   }; // LONG  SlipUndefinedOp::shr (const SlipDatum& Y, const LONG X) const

   /**
    * @brief A bitwise 'and' operation (<b>Y &amp; X</b>) is illegal
    * @param[in] Y (SlipDatum&) bit string
    * @param[in] X (LONG) bit string
    * @return (LONG) 0
    */
   LONG  SlipUndefinedOp::band(const SlipDatum& Y, const LONG X) const { // operator&
      postError(__FILE__, __LINE__, SlipErr::E1024, "SlipUndefinedOp&", "", "Deleted cell.", Y);
      return 0;
   }; // LONG  SlipUndefinedOp::band(const SlipDatum& Y, const LONG X) const

   /**
    * @brief A bitwise 'or' operation (<b>Y \| X</b>) is illegal
    * @param[in] Y (SlipDatum&) bit string
    * @param[in] X (LONG) bit string
    * @return (LONG) 0
    */
   LONG  SlipUndefinedOp::bor (const SlipDatum& Y, const LONG X) const { // operator|
      postError(__FILE__, __LINE__, SlipErr::E1023, "SlipUndefinedOp|", "", "Deleted cell.", Y);
      return 0;
   }; // LONG  SlipUndefinedOp::bor (const SlipDatum& Y, const LONG X) const

   /**
    * @brief A bitwise 'exclusive or' operation (<b>Y ^ X</b>) is illegal
    * @param[in] Y (SlipDatum&) bit string
    * @param[in] X (LONG) bit string
    * @return (LONG) 0
    */
   LONG  SlipUndefinedOp::bxor(const SlipDatum& Y, const LONG X) const { // operator^
      postError(__FILE__, __LINE__, SlipErr::E1022, "SlipUndefinedOp^", "", "Deleted cell.", Y);
      return 0;
   }; // LONG  SlipUndefinedOp::bxor(const SlipDatum& Y, const LONG X) const

          /*************************************************
           *               Simple Assignment               *
          *************************************************/

   /**
    * @brief A simple assignment (<b>Y = X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (SlipDatum&) RHS value
    * @return Y (SlipDatum&)
    */
   SlipDatum& SlipUndefinedOp::asgn(SlipDatum& Y, const SlipDatum& X) const {        // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1022, "SlipUndefinedOp^", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum& SlipUndefinedOp::asgn(SlipDatum& Y, const SlipDatum& X) const

          /*************************************************
           *       Arithmetic Assignment operators         *
          *************************************************/

   /**
    * @brief A compound assignment (<b>Y += X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (SlipDatum&) addend
    * @return Y (SlipDatum&)
    */
   SlipDatum& SlipUndefinedOp::addAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1009, "SlipUndefinedOp+=", "", "Deleted cell.", X);
      return Y;
   }; // SlipDatum& SlipUndefinedOp::addAsgn (SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A compound assignment (<b>Y -= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (SlipDatum&) subtrahend
    * @return Y (SlipDatum&)
    */
   SlipDatum& SlipUndefinedOp::subAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator-=
      postError(__FILE__, __LINE__, SlipErr::E1010, "SlipUndefinedOp-=", "", "Deleted cell.", X);
      return Y;
   }; // SlipDatum& SlipUndefinedOp::subAsgn (SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A compound assignment (<b>Y *= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (SlipDatum&) multiplicand
    * @return Y (SlipDatum&)
    */
   SlipDatum& SlipUndefinedOp::mulAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator*=
      postError(__FILE__, __LINE__, SlipErr::E1011, "SlipUndefinedOp*=", "", "Deleted cell.", X);
      return Y;
   }; // SlipDatum& SlipUndefinedOp::mulAsgn (SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A compound assignment (<b>Y /= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (SlipDatum&) divisor
    * @return Y (SlipDatum&)
    */
   SlipDatum& SlipUndefinedOp::divAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator/=
      postError(__FILE__, __LINE__, SlipErr::E1012, "SlipUndefinedOp/=", "", "Deleted cell.", X);
      return Y;
   }; // SlipDatum& SlipUndefinedOp::divAsgn (SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A compound assignment (<b>Y %= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (SlipDatum&) modulus
    * @return Y (SlipDatum&)
    */
   SlipDatum& SlipUndefinedOp::modAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator%=
      postError(__FILE__, __LINE__, SlipErr::E1013, "SlipUndefinedOp%=", "", "Deleted cell.", (SlipCellBase&)Y, X);
      return Y;
   }; // SlipDatum& SlipUndefinedOp::modAsgn (SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A compound assignment (<b>Y &lt;&lt;= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (SlipDatum&) shift amount
    * @return Y (SlipDatum&)
    */
   SlipDatum& SlipUndefinedOp::shlAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator<<=
      postError(__FILE__, __LINE__, SlipErr::E1014, "SlipUndefinedOp<<=", "", "Deleted cell.", (SlipCellBase&)Y, X);
      return Y;
   }; // SlipDatum& SlipUndefinedOp::shlAsgn (SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A compound assignment (<b>Y &gt;&gt;= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (SlipDatum&) shift amount
    * @return Y (SlipDatum&)
    */
   SlipDatum& SlipUndefinedOp::shrAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator>>=
      postError(__FILE__, __LINE__, SlipErr::E1015, "SlipUndefinedOp>>=", "", "Deleted cell.", (SlipCellBase&)Y, X);
      return Y;
   }; // SlipDatum& SlipUndefinedOp::shrAsgn (SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A compound assignment (<b>Y &amp;= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (SlipDatum&) bit string
    * @return Y (SlipDatum&)
    */
   SlipDatum&  SlipUndefinedOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator&=
      postError(__FILE__, __LINE__, SlipErr::E1016, "SlipUndefinedOp&=", "", "Deleted cell.", (SlipCellBase&)Y, X);
      return Y;
   }; // SlipDatum&  SlipUndefinedOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A compound assignment (<b>Y \|= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (SlipDatum&) bit string
    * @return Y (SlipDatum&)
    */
   SlipDatum&  SlipUndefinedOp::borAsgn (SlipDatum& Y, const SlipDatum& X) const { // operator|=
      postError(__FILE__, __LINE__, SlipErr::E1019, "SlipUndefinedOp|=", "", "Deleted cell.", (SlipCellBase&)Y, X);
      return Y;
   }; // SlipDatum&  SlipUndefinedOp::borAsgn (SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A compound assignment (<b>Y ^= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (SlipDatum&) bit string
    * @return Y (SlipDatum&)
    */
   SlipDatum&  SlipUndefinedOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const { // operator^=
      postError(__FILE__, __LINE__, SlipErr::E1017, "SlipUndefinedOp^=", "", "Deleted cell.", (SlipCellBase&)Y, X);
      return Y;
   }; // SlipDatum&  SlipUndefinedOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const


   /**
    * @brief A compound assignment (<b>Y += X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (DOUBLE) addend
    * @return Y (SlipDatum&)
    */
   SlipDatum& SlipUndefinedOp::addAsgn (SlipDatum& Y, const DOUBLE X) const { // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1009, "SlipUndefinedOp+=", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum& SlipUndefinedOp::addAsgn (SlipDatum& Y, const DOUBLE X) const

   /**
    * @brief A compound assignment (<b>Y -= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (DOUBLE) subtrahend
    * @return Y (SlipDatum&)
    */
   SlipDatum& SlipUndefinedOp::subAsgn (SlipDatum& Y, const DOUBLE X) const { // operator-=
      postError(__FILE__, __LINE__, SlipErr::E1010, "SlipUndefinedOp-=", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum& SlipUndefinedOp::subAsgn (SlipDatum& Y, const DOUBLE X) const

   /**
    * @brief A compound assignment (<b>Y *= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (DOUBLE) multiplicand
    * @return Y (SlipDatum&)
    */
   SlipDatum& SlipUndefinedOp::mulAsgn (SlipDatum& Y, const DOUBLE X) const { // operator*=
      postError(__FILE__, __LINE__, SlipErr::E1011, "SlipUndefinedOp*=", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum& SlipUndefinedOp::mulAsgn (SlipDatum& Y, const DOUBLE X) const

   /**
    * @brief A compound assignment (<b>Y /= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (DOUBLE) dividend
    * @return Y (SlipDatum&)
    */
   SlipDatum& SlipUndefinedOp::divAsgn (SlipDatum& Y, const DOUBLE X) const { // operator/=
      postError(__FILE__, __LINE__, SlipErr::E1012, "SlipUndefinedOp/=", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum& SlipUndefinedOp::divAsgn (SlipDatum& Y, const DOUBLE X) const


   /**
    * @brief A compound assignment (<b>Y += X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (ULONG) addend
    * @return Y (SlipDatum&)
    */
   SlipDatum& SlipUndefinedOp::addAsgn (SlipDatum& Y, const ULONG X) const { // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1009, "SlipUndefinedOp+=", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum& SlipUndefinedOp::addAsgn (SlipDatum& Y, const ULONG X) const

   /**
    * @brief A compound assignment (<b>Y -= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (ULONG) subtrahend
    * @return Y (SlipDatum&)
    */
   SlipDatum& SlipUndefinedOp::subAsgn (SlipDatum& Y, const ULONG X) const { // operator-=
      postError(__FILE__, __LINE__, SlipErr::E1010, "SlipUndefinedOp-=", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum& SlipUndefinedOp::subAsgn (SlipDatum& Y, const ULONG X) const

   /**
    * @brief A compound assignment (<b>Y *= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (ULONG) multiplicand
    * @return Y (SlipDatum&)
    */
   SlipDatum& SlipUndefinedOp::mulAsgn (SlipDatum& Y, const ULONG X) const { // operator*=
      postError(__FILE__, __LINE__, SlipErr::E1011, "SlipUndefinedOp*=", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum& SlipUndefinedOp::mulAsgn (SlipDatum& Y, const ULONG X) const

   /**
    * @brief A compound assignment (<b>Y /= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (ULONG) divisor
    * @return Y (SlipDatum&)
    */
   SlipDatum& SlipUndefinedOp::divAsgn (SlipDatum& Y, const ULONG X) const { // operator/=
      postError(__FILE__, __LINE__, SlipErr::E1012, "SlipUndefinedOp/=", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum& SlipUndefinedOp::divAsgn (SlipDatum& Y, const ULONG X) const

   /**
    * @brief A compound assignment (<b>Y %= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (ULONG) modulus
    * @return Y (SlipDatum&)
    */
   SlipDatum& SlipUndefinedOp::modAsgn (SlipDatum& Y, const ULONG X) const { // operator%=
      postError(__FILE__, __LINE__, SlipErr::E1013, "SlipUndefinedOp%=", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum& SlipUndefinedOp::modAsgn (SlipDatum& Y, const ULONG X) const

   /**
    * @brief A compound assignment (<b>Y &lt;&lt;= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (ULONG) shift amount
    * @return Y (SlipDatum&)
    */
   SlipDatum& SlipUndefinedOp::shlAsgn (SlipDatum& Y, const ULONG X) const { // operator<<=
      postError(__FILE__, __LINE__, SlipErr::E1014, "SlipUndefinedOp<<=", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum& SlipUndefinedOp::shlAsgn (SlipDatum& Y, const ULONG X) const

   /**
    * @brief A compound assignment (<b>Y &gt;&gt;= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (ULONG) shift amount
    * @return Y (SlipDatum&)
    */
   SlipDatum& SlipUndefinedOp::shrAsgn (SlipDatum& Y, const ULONG X) const { // operator>>=
      postError(__FILE__, __LINE__, SlipErr::E1015, "SlipUndefinedOp>>=", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum& SlipUndefinedOp::shrAsgn (SlipDatum& Y, const ULONG X) const

   /**
    * @brief A compound assignment (<b>Y &amp;= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (ULONG) bit string
    * @return Y (SlipDatum&)
    */
   SlipDatum&  SlipUndefinedOp::bandAsgn(SlipDatum& Y, const ULONG X) const { // operator&=
      postError(__FILE__, __LINE__, SlipErr::E1016, "SlipUndefinedOp>>=", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum&  SlipUndefinedOp::bandAsgn(SlipDatum& Y, const ULONG X) const

   /**
    * @brief A compound assignment (<b>Y \|= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (ULONG) bit string
    * @return Y (SlipDatum&)
    */
   SlipDatum&  SlipUndefinedOp::borAsgn (SlipDatum& Y, const ULONG X) const { // operator|=
      postError(__FILE__, __LINE__, SlipErr::E1019, "SlipUndefinedOp|=", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum&  SlipUndefinedOp::borAsgn (SlipDatum& Y, const ULONG X) const

   /**
    * @brief A compound assignment (<b>Y ^= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (ULONG) bit string
    * @return Y (SlipDatum&)
    */
   SlipDatum&  SlipUndefinedOp::bxorAsgn(SlipDatum& Y, const ULONG X) const { // operator^=
      postError(__FILE__, __LINE__, SlipErr::E1017, "SlipUndefinedOp^=", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum&  SlipUndefinedOp::bxorAsgn(SlipDatum& Y, const ULONG X) const


   /**
    * @brief A compound assignment (<b>Y += X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (LONG) addend
    * @return Y (SlipDatum&)
    */
   SlipDatum& SlipUndefinedOp::addAsgn (SlipDatum& Y, const LONG X) const { // operator+=
      postError(__FILE__, __LINE__, SlipErr::E1009, "SlipUndefinedOp+=", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum& SlipUndefinedOp::addAsgn (SlipDatum& Y, const LONG X) const

   /**
    * @brief A compound assignment (<b>Y -= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (LONG) subtrahend
    * @return Y (SlipDatum&)
    */
   SlipDatum& SlipUndefinedOp::subAsgn (SlipDatum& Y, const LONG X) const { // operator-=
      postError(__FILE__, __LINE__, SlipErr::E1010, "SlipUndefinedOp-=", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum& SlipUndefinedOp::subAsgn (SlipDatum& Y, const LONG X) const

   /**
    * @brief A compound assignment (<b>Y *= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (LONG) multiplicand
    * @return Y (SlipDatum&)
    */
   SlipDatum& SlipUndefinedOp::mulAsgn (SlipDatum& Y, const LONG X) const { // operator*=
      postError(__FILE__, __LINE__, SlipErr::E1011, "SlipUndefinedOp*=", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum& SlipUndefinedOp::mulAsgn (SlipDatum& Y, const LONG X) const

   /**
    * @brief A compound assignment (<b>Y /= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (LONG) divisor
    * @return Y (SlipDatum&)
    */
   SlipDatum& SlipUndefinedOp::divAsgn (SlipDatum& Y, const LONG X) const { // operator/=
      postError(__FILE__, __LINE__, SlipErr::E1012, "SlipUndefinedOp/=", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum& SlipUndefinedOp::divAsgn (SlipDatum& Y, const LONG X) const

   /**
    * @brief A compound assignment (<b>Y %= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (LONG) modulus
    * @return Y (SlipDatum&)
    */
   SlipDatum& SlipUndefinedOp::modAsgn (SlipDatum& Y, const LONG X) const { // operator%=
      postError(__FILE__, __LINE__, SlipErr::E1013, "SlipUndefinedOp%=", "", "Deleted cell.", Y);
      return Y;
   }; // SlipDatum& SlipUndefinedOp::modAsgn (SlipDatum& Y, const LONG X) const

   /**
    * @brief A compound assignment (<b>Y &lt;&lt;= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (LONG) shift amount
    * @return Y (SlipDatum&)
    */
   SlipDatum&  SlipUndefinedOp::shlAsgn (SlipDatum& Y, const LONG X) const { // operator<<=
      postError(__FILE__, __LINE__, SlipErr::E1014, "SlipUndefinedOp<<=", "", "Deleted cell.", Y);
      return Y;
   }; //  SlipDatum&  SlipUndefinedOp::shlAsgn (SlipDatum& Y, const LONG X) const

   /**
    * @brief A compound assignment (<b>Y &gt;&gt;= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (LONG) shift amount
    * @return Y (SlipDatum&)
    */
   SlipDatum&  SlipUndefinedOp::shrAsgn (SlipDatum& Y, const LONG X) const { // operator>>=
      postError(__FILE__, __LINE__, SlipErr::E1015, "SlipUndefinedOp>>=", "", "Deleted cell.", Y);
      return Y;
   }; //  SlipDatum&  SlipUndefinedOp::shrAsgn (SlipDatum& Y, const LONG X) const

   /**
    * @brief A compound assignment (<b>Y &amp;= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (LONG) bit string
    * @return Y (SlipDatum&)
    */
   SlipDatum&  SlipUndefinedOp::bandAsgn(SlipDatum& Y, const LONG X) const { // operator&=
      postError(__FILE__, __LINE__, SlipErr::E1016, "SlipUndefinedOp&=", "", "Deleted cell.", Y);
      return Y;
   }; //  SlipDatum&  SlipUndefinedOp::bandAsgn(SlipDatum& Y, const LONG X) const

   /**
    * @brief A compound assignment (<b>Y \|= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (LONG) bit string
    * @return Y (SlipDatum&)
    */
   SlipDatum&  SlipUndefinedOp::borAsgn (SlipDatum& Y, const LONG X) const { // operator|=
      postError(__FILE__, __LINE__, SlipErr::E1019, "SlipUndefinedOp|=", "", "Deleted cell.", Y);
      return Y;
   }; //  SlipDatum&  SlipUndefinedOp::borAsgn (SlipDatum& Y, const LONG X) const

   /**
    * @brief A compound assignment (<b>Y ^= X</b>) is illegal
    * @param[in] Y (SlipDatum&) LHS value
    * @param[in] X (LONG) bit string
    * @return Y (SlipDatum&)
    */
   SlipDatum&  SlipUndefinedOp::bxorAsgn(SlipDatum& Y, const LONG X) const { // operator^=
      postError(__FILE__, __LINE__, SlipErr::E1017, "SlipUndefinedOp^=", "", "Deleted cell.", Y);
      return Y;
   }; //  SlipDatum&  SlipUndefinedOp::bxorAsgn(SlipDatum& Y, const LONG X) const

          /*************************************************
           *               Logical operators               *
          *************************************************/

   /**
    * @brief A logical comparison operation (<b>Y != X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (SlipDatum&) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::notEqual (const SlipDatum& Y, const SlipDatum& X) const { // operator!=
      postError(__FILE__, __LINE__, SlipErr::E1028, "SlipUndefinedOp!=", "", "Deleted cell.", X);
      return false;
   }; // bool SlipUndefinedOp::notEqual (const SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A logical comparison operation (<b>Y &lt; X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (SlipDatum&) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::lessThan (const SlipDatum& Y, const SlipDatum& X) const { // operator<
      postError(__FILE__, __LINE__, SlipErr::E1029, "SlipUndefinedOp<", "", "Deleted cell.", X);
      return false;
   }; // bool SlipUndefinedOp::lessThan (const SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A logical comparison operation (<b>Y &lt;= X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (SlipDatum&) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::lessEqual (const SlipDatum& Y, const SlipDatum& X) const { // operator<=
      postError(__FILE__, __LINE__, SlipErr::E1030, "SlipUndefinedOp<=", "", "Deleted cell.", X);
      return false;
   }; // bool SlipUndefinedOp::lessEqual (const SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A logical comparison operation (<b>Y == X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (SlipDatum&) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::equal (const SlipDatum& Y, const SlipDatum& X) const { // operator==
      postError(__FILE__, __LINE__, SlipErr::E1031, "SlipUndefinedOp==", "", "Deleted cell.", X);
      return false;
   }; // bool SlipUndefinedOp::equal (const SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A logical comparison operation (<b>Y &gt;= X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (SlipDatum&) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::greatEqual(const SlipDatum& Y, const SlipDatum& X) const { // operator>=
      postError(__FILE__, __LINE__, SlipErr::E1032, "SlipUndefinedOp>=", "", "Deleted cell.", X);
      return false;
   }; // bool SlipUndefinedOp::greatEqual(const SlipDatum& Y, const SlipDatum& X) const

   /**
    * @brief A logical comparison operation (<b>Y &gt; X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (SlipDatum&) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::greatThan (const SlipDatum& Y, const SlipDatum& X) const { // operator>
      postError(__FILE__, __LINE__, SlipErr::E1033, "SlipUndefinedOp>", "", "Deleted cell.", X);
      return false;
   }; // bool SlipUndefinedOp::greatThan (const SlipDatum& Y, const SlipDatum& X) const


   /**
    * @brief A logical comparison operation (<b>Y != X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (DOUBLE) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::notEqual (const SlipDatum& Y, const DOUBLE X) const { // operator!=
      postError(__FILE__, __LINE__, SlipErr::E1028, "SlipUndefinedOp!=", "", "Deleted cell.", Y);
      return false;
   }; // bool SlipUndefinedOp::notEqual (const SlipDatum& Y, const DOUBLE X) const

   /**
    * @brief A logical comparison operation (<b>Y &lt; X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (DOUBLE) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::lessThan (const SlipDatum& Y, const DOUBLE X) const { // operator<
      postError(__FILE__, __LINE__, SlipErr::E1029, "SlipUndefinedOp<", "", "Deleted cell.", Y);
      return false;
   }; // bool SlipUndefinedOp::lessThan (const SlipDatum& Y, const DOUBLE X) const

   /**
    * @brief A logical comparison operation (<b>Y &lt;= X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (DOUBLE) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::lessEqual (const SlipDatum& Y, const DOUBLE X) const { // operator<=
      postError(__FILE__, __LINE__, SlipErr::E1030, "SlipUndefinedOp<=", "", "Deleted cell.", Y);
      return false;
   }; // bool SlipUndefinedOp::lessEqual (const SlipDatum& Y, const DOUBLE X) const

   /**
    * @brief A logical comparison operation (<b>Y == X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (DOUBLE) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::equal (const SlipDatum& Y, const DOUBLE X) const { // operator==
      postError(__FILE__, __LINE__, SlipErr::E1031, "SlipUndefinedOp==", "", "Deleted cell.", Y);
      return false;
   }; // bool SlipUndefinedOp::equal (const SlipDatum& Y, const DOUBLE X) const

   /**
    * @brief A logical comparison operation (<b>Y &gt;= X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (DOUBLE) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::greatEqual(const SlipDatum& Y, const DOUBLE X) const { // operator>=
      postError(__FILE__, __LINE__, SlipErr::E1032, "SlipUndefinedOp>=", "", "Deleted cell.", Y);
      return false;
   }; // bool SlipUndefinedOp::greatEqual(const SlipDatum& Y, const DOUBLE X) const

   /**
    * @brief A logical comparison operation (<b>Y &gt; X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (DOUBLE) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::greatThan (const SlipDatum& Y, const DOUBLE X) const { // operator>
      postError(__FILE__, __LINE__, SlipErr::E1033, "SlipUndefinedOp>", "", "Deleted cell.", Y);
      return false;
   }; // bool SlipUndefinedOp::greatThan (const SlipDatum& Y, const DOUBLE X) const


   /**
    * @brief A logical comparison operation (<b>Y != X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (ULONG) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::notEqual (const SlipDatum& Y, const ULONG X) const { // operator!=
      postError(__FILE__, __LINE__, SlipErr::E1028, "SlipUndefinedOp!=", "", "Deleted cell.", Y);
      return false;
   }; // bool SlipUndefinedOp::notEqual (const SlipDatum& Y, const ULONG X) const

   /**
    * @brief A logical comparison operation (<b>Y &lt; X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (ULONG) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::lessThan (const SlipDatum& Y, const ULONG X) const { // operator<
      postError(__FILE__, __LINE__, SlipErr::E1029, "SlipUndefinedOp<", "", "Deleted cell.", Y);
      return false;
   }; // bool SlipUndefinedOp::lessThan (const SlipDatum& Y, const ULONG X) const

   /**
    * @brief A logical comparison operation (<b>Y &lt;= X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (ULONG) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::lessEqual (const SlipDatum& Y, const ULONG X) const { // operator<=
      postError(__FILE__, __LINE__, SlipErr::E1030, "SlipUndefinedOp<=", "", "Deleted cell.", Y);
      return false;
   }; // bool SlipUndefinedOp::lessEqual (const SlipDatum& Y, const ULONG X) const

   /**
    * @brief A logical comparison operation (<b>Y == X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (ULONG) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::equal (const SlipDatum& Y, const ULONG X) const { // operator==
      postError(__FILE__, __LINE__, SlipErr::E1031, "SlipUndefinedOp==", "", "Deleted cell.", Y);
      return false;
   }; // bool SlipUndefinedOp::equal (const SlipDatum& Y, const ULONG X) const

   /**
    * @brief A logical comparison operation (<b>Y &gt;= X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (ULONG) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::greatEqual(const SlipDatum& Y, const ULONG X) const { // operator>=
      postError(__FILE__, __LINE__, SlipErr::E1032, "SlipUndefinedOp>=", "", "Deleted cell.", Y);
      return false;
   }; // bool SlipUndefinedOp::greatEqual(const SlipDatum& Y, const ULONG X) const

   /**
    * @brief A logical comparison operation (<b>Y &gt; X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (ULONG) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::greatThan (const SlipDatum& Y, const ULONG X) const { // operator>
      postError(__FILE__, __LINE__, SlipErr::E1033, "SlipUndefinedOp>", "", "Deleted cell.", Y);
      return false;
   }; // bool SlipUndefinedOp::greatThan (const SlipDatum& Y, const ULONG X) const


   /**
    * @brief A logical comparison operation (<b>Y != X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (LONG) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::notEqual (const SlipDatum& Y, const LONG X) const { // operator!=
      postError(__FILE__, __LINE__, SlipErr::E1028, "SlipUndefinedOp!=", "", "Deleted cell.", Y);
      return false;
   }; // bool SlipUndefinedOp::notEqual (const SlipDatum& Y, const LONG X) const

   /**
    * @brief A logical comparison operation (<b>Y &lt; X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (LONG) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::lessThan (const SlipDatum& Y, const LONG X) const { // operator<
      postError(__FILE__, __LINE__, SlipErr::E1029, "SlipUndefinedOp<", "", "Deleted cell.", Y);
      return false;
   }; // bool SlipUndefinedOp::lessThan (const SlipDatum& Y, const LONG X) const

   /**
    * @brief A logical comparison operation (<b>Y &lt;= X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (LONG) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::lessEqual (const SlipDatum& Y, const LONG X) const { // operator<=
      postError(__FILE__, __LINE__, SlipErr::E1030, "SlipUndefinedOp<", "", "Deleted cell.", Y);
      return false;
   }; // bool SlipUndefinedOp::lessEqual (const SlipDatum& Y, const LONG X) const

   /**
    * @brief A logical comparison operation (<b>Y == X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (LONG) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::equal (const SlipDatum& Y, const LONG X) const { // operator==
      postError(__FILE__, __LINE__, SlipErr::E1031, "SlipUndefinedOp==", "", "Deleted cell.", Y);
      return false;
   }; // bool SlipUndefinedOp::equal (const SlipDatum& Y, const LONG X) const

   /**
    * @brief A logical comparison operation (<b>Y &gt;= X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (LONG) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::greatEqual(const SlipDatum& Y, const LONG X) const { // operator>=
      postError(__FILE__, __LINE__, SlipErr::E1032, "SlipUndefinedOp>=", "", "Deleted cell.", Y);
      return false;
   }; // bool SlipUndefinedOp::greatEqual(const SlipDatum& Y, const LONG X) const

   /**
    * @brief A logical comparison operation (<b>Y &gt; X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (LONG) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::greatThan (const SlipDatum& Y, const LONG X) const { // operator>
      postError(__FILE__, __LINE__, SlipErr::E1033, "SlipUndefinedOp>", "", "Deleted cell.", Y);
      return false;
   }; // bool SlipUndefinedOp::greatThan (const SlipDatum& Y, const LONG X) const


   /**
    * @brief A logical comparison operation (<b>Y != X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (PTR) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::notEqual (const SlipDatum& Y, const PTR X) const { // operator!=
       postError(__FILE__, __LINE__, SlipErr::E1028, "SlipUndefinedOp!=", "", "Deleted cell.", Y);
       return false;
   }; // bool SlipUndefinedOp::notEqual (const SlipDatum& Y, const PTR X) const

   /**
    * @brief A logical comparison operation (<b>Y == X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (PTR) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::equal (const SlipDatum& Y, const PTR X) const { // operator==
       postError(__FILE__, __LINE__, SlipErr::E1029, "SlipUndefinedOp==", "", "Deleted cell.", Y);
       return false;
   }; // bool SlipUndefinedOp::equal (const SlipDatum& Y, const PTR X) const


   /**
    * @brief A logical comparison operation (<b>Y != X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (string&) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::notEqual (const SlipDatum& Y, const string& X) const { // operator!=
      postError(__FILE__, __LINE__, SlipErr::E1028, "SlipUndefinedOp!=", "", "Deleted cell.", Y);
      return false;
   }; // bool SlipUndefinedOp::notEqual (const SlipDatum& Y, const string& X) const

   /**
    * @brief A logical comparison operation (<b>Y &lt; X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (string&) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::lessThan (const SlipDatum& Y, const string& X) const { // operator<
      postError(__FILE__, __LINE__, SlipErr::E1029, "SlipUndefinedOp<", "", "Deleted cell.", Y);
      return false;
   }; // bool SlipUndefinedOp::lessThan (const SlipDatum& Y, const string& X) const

   /**
    * @brief A logical comparison operation (<b>Y &lt;= X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (string&) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::lessEqual (const SlipDatum& Y, const string& X) const { // operator<=
      postError(__FILE__, __LINE__, SlipErr::E1030, "SlipUndefinedOp<", "", "Deleted cell.", Y);
      return false;
   }; // bool SlipUndefinedOp::lessEqual (const SlipDatum& Y, const string& X) const

   /**
    * @brief A logical comparison operation (<b>Y == X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (string&) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::equal (const SlipDatum& Y, const string& X) const { // operator==
      postError(__FILE__, __LINE__, SlipErr::E1031, "SlipUndefinedOp==", "", "Deleted cell.", Y);
      return false;
   }; // bool SlipUndefinedOp::equal (const SlipDatum& Y, const string& X) const

   /**
    * @brief A logical comparison operation (<b>Y &gt;= X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (string&) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::greatEqual(const SlipDatum& Y, const string& X) const { // operator>=
      postError(__FILE__, __LINE__, SlipErr::E1032, "SlipUndefinedOp>=", "", "Deleted cell.", Y);
      return false;
   }; // bool SlipUndefinedOp::greatEqual(const SlipDatum& Y, const string& X) const

   /**
    * @brief A logical comparison operation (<b>Y &gt; X</b>) is illegal
    * @param[in] Y (SlipDatum&) left comparand
    * @param[in] X (string&) right comparand
    * @return (bool) false
    */
   bool SlipUndefinedOp::greatThan (const SlipDatum& Y, const string& X) const { // operator>
      postError(__FILE__, __LINE__, SlipErr::E1033, "SlipUndefinedOp>", "", "Deleted cell.", Y);
      return false;
   }; // bool SlipUndefinedOp::greatThan (const SlipDatum& Y, const string& X) const

   /**
    * @brief Return an uglified string for an undefined object
    * @param[in] Y (SlipCell&) cell to be uglified
    * @return (string)
    */
   string SlipUndefinedOp::toString(const SlipCell& Y) const { // pretty print SLIP cell
      return dump(Y);
   }; // string SlipUndefinedOp::toString(const SlipCell& X) const

   /**
    * @brief Return an uglified string for an undefined object
    * @param[in] Y (SlipCell&) cell to be uglified
    * @return (string)
    */
   string SlipUndefinedOp::dump(const SlipCell& Y) const {
      stringstream pretty;
      pretty << "[undefined] " << dumpLink(Y);
      return pretty.str();
   }; // string SlipUndefinedOp::dump(const SlipCell& Y) const

   string SlipUndefinedOp::write(const SlipCell& X) const {
      return "";
   }; // string SlipUndefinedOp::write(const SlipCell& X) const

}; // namespace slip