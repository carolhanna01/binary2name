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
 * @file    SlipPtrOp.h
 * @author  A. Schwarz
 * @date    June 6, 2012
 *
 * @brief  Base class for User Defined Date invariant operations.
 */

#ifndef _SLIPPTROP_H
#define	_SLIPPTROP_H

# include "SlipDef.h"
# include "SlipOp.h"
# include <string>

using namespace std;

namespace slip {
   
/**
 * @class SlipPtrOp
 * @brief Base class for User Defined Date invariant operations.
 * <p><b>Overview</b></p><hr>
 * 
 * <p>This is the generic class of operations for a User Defined Data type.
 *    It is not aware of the internal structure of the data type and is
 *    not aware of any operations which could be performed. The generic
 *    format only provides simple assignment and equality testing. Simple
 *    assignment is done in SlipDatum and equality test is done in this
 *    class.</p>
 * <p>Equality testing consists of a comparison of the pointers to the
 *    user defined data. If the pointers are equal, they are the same,
 *    then the test succeeds, otherwise the test fails.</p>
 * <p>All other operations are occasioned with a nasty error message
 *    stating that the operation can not be done.</p>
 * 
 * <p><b>User Defined Operations</b></p><hr>
 * 
 * <p>The user can provide their own operations by creating a derived
 *    class using either SlipOp or SlipPtrOp as the base class and
 *    overriding all required virtual methods. The convention which
 *    must be followed in the constructor is:</p>
 * <tt><center>
 *    | Invariant Attribute | value |
 *    | :------------------ | :---- |
 *    | cellType            | 9     |
 *    | ID                  | 6     |
 *    | classType           | ePTR  |
 *    | name                | "PTR" |
 *    | isDataFlag          | false |
 *    | isDiscreteFlag      | false |
 *    | isHeaderFlag        | false |
 *    | isNumberFlag        | false |
 *    | isSublistFlag       | false |
 * </center></tt>
 * 
 * <p>These values are provided by the SlipPtrOp class. If this class
 *    is used as the base class, then the invariant properties will
 *    be correctly assigned.</p>
 * <p>If the user requires that additional subclassing is required
 *    in order to support multiple User Defined Data types, then
 *    this must be done in the User Defined Classes. Altering the
 *    values in the constructor will lead to untimely conditions,
 *    to wit, the SLIP kernel will not be able to recognize User
 *    Defined Data Types.</p>
 * <p>The SLIP kernel will not delete the operations pointer in 
 *    a Slip Cell. If the user creates requires multiple objects
 *    representing operations for the same User Defined Data types,
 *    the the user is required to provide a deletion mechanism. 
 *    All SLIP operations are static objects representing invariant
 *    type properties. They are never deleted. It is recommended
 *    that the user follow suit and create a static object for
 *    each User Defined Data type's operations and never require
 *    that this object be deleted.</p>
 */
   class SlipPtrOp : public SlipOp {
   private:
       SlipPtrOp(const SlipPtrOp& orig);
       ~SlipPtrOp();
   public:
      SlipPtrOp();
   public:

      /*********************************************************
       *                  Casting Operators                    *
      *********************************************************/
      virtual bool   cast_bool(const SlipDatum& X) const;                         // (bool)   X
      virtual UCHAR  cast_UCHAR(const SlipDatum& X) const;                        // (UCHAR)  X
      virtual CHAR   cast_CHAR(const SlipDatum& X) const;                         // (CHAR)   X
      virtual ULONG  cast_ULONG(const SlipDatum& X) const;                        // (ULONG)  X
      virtual LONG   cast_LONG(const SlipDatum& X) const;                         // (LONG)   X
      virtual DOUBLE cast_DOUBLE(const SlipDatum& X) const;                       // (DOUBLE) X
      virtual PTR    cast_PTR(const SlipDatum& X) const;                          // (PTR) X

      /*********************************************************
       *                 Operator Overloading                  *
      *********************************************************/

          /*************************************************
           *                Unary operators                *
          *************************************************/
      virtual ULONG      bitNOT   (const SlipDatum& X) const;                     // operator~
      virtual bool       logNOT   (const SlipDatum& X) const;                     // operator!
      virtual SlipDatum  plus     (const SlipDatum& X) const;                     // operator+
      virtual SlipDatum  minus    (const SlipDatum& X) const;                     // operator-
      virtual SlipDatum& postInc  (SlipDatum& X) const;                           // operator++
      virtual SlipDatum& postDec  (SlipDatum& X) const;                           // operator--
      virtual SlipDatum& preInc   (SlipDatum& X) const;                           // ++operator
      virtual SlipDatum& preDec   (SlipDatum& X) const;                           // --operator

          /*************************************************
           *                Binary operators               *
          *************************************************/
      virtual SlipDatum  add(const SlipDatum& Y, const SlipDatum& X) const;       // operator+
      virtual SlipDatum  sub(const SlipDatum& Y, const SlipDatum& X) const;       // operator-
      virtual SlipDatum  mul(const SlipDatum& Y, const SlipDatum& X) const;       // operator*
      virtual SlipDatum  div(const SlipDatum& Y, const SlipDatum& X) const;       // operator/
      virtual LONG       mod(const SlipDatum& Y, const SlipDatum& X) const;       // operator%

      virtual SlipDatum  add(const SlipDatum& Y, const DOUBLE X) const;           // operator+
      virtual SlipDatum  sub(const SlipDatum& Y, const DOUBLE X) const;           // operator-
      virtual SlipDatum  mul(const SlipDatum& Y, const DOUBLE X) const;           // operator*
      virtual SlipDatum  div(const SlipDatum& Y, const DOUBLE X) const;           // operator/

      virtual SlipDatum  add(const SlipDatum& Y, const ULONG X) const;            // operator+
      virtual SlipDatum  sub(const SlipDatum& Y, const ULONG X) const;            // operator-
      virtual SlipDatum  mul(const SlipDatum& Y, const ULONG X) const;            // operator*
      virtual SlipDatum  div(const SlipDatum& Y, const ULONG X) const;            // operator/
      virtual LONG       mod(const SlipDatum& Y, const ULONG X) const;            // operator%

      virtual SlipDatum  add(const SlipDatum& Y, const LONG X) const;             // operator+
      virtual SlipDatum  sub(const SlipDatum& Y, const LONG X) const;             // operator-
      virtual SlipDatum  mul(const SlipDatum& Y, const LONG X) const;             // operator*
      virtual SlipDatum  div(const SlipDatum& Y, const LONG X) const;             // operator/
      virtual LONG       mod(const SlipDatum& Y, const LONG X) const;             // operator%

          /*************************************************
           *                Bit operators                  *
          *************************************************/
      virtual LONG     shl (const SlipDatum& Y, const SlipDatum& X) const;        // operator<<
      virtual LONG     shr (const SlipDatum& Y, const SlipDatum& X) const;        // operator>>
      virtual LONG     band(const SlipDatum& Y, const SlipDatum& X) const;        // operator&
      virtual LONG     bor (const SlipDatum& Y, const SlipDatum& X) const;        // operator|
      virtual LONG     bxor(const SlipDatum& Y, const SlipDatum& X) const;        // operator^

      virtual LONG     shl (const SlipDatum& Y, const ULONG X) const;             // operator<<
      virtual LONG     shr (const SlipDatum& Y, const ULONG X) const;             // operator>>
      virtual LONG     band(const SlipDatum& Y, const ULONG X) const;             // operator&
      virtual LONG     bor (const SlipDatum& Y, const ULONG X) const;             // operator|
      virtual LONG     bxor(const SlipDatum& Y, const ULONG X) const;             // operator^

      virtual LONG     shl (const SlipDatum& Y, const LONG  X) const;             // operator<<
      virtual LONG     shr (const SlipDatum& Y, const LONG  X) const;             // operator>>
      virtual LONG     band(const SlipDatum& Y, const LONG  X) const;             // operator&
      virtual LONG     bor (const SlipDatum& Y, const LONG  X) const;             // operator|
      virtual LONG     bxor(const SlipDatum& Y, const LONG  X) const;             // operator^

          /*************************************************
           *               Simple Assignment               *
          *************************************************/
      virtual SlipDatum& asgn (SlipDatum& Y, const SlipDatum& X) const;         // operator+=

          /*************************************************
           *       Arithmetic Assignment operators         *
          *************************************************/
      virtual SlipDatum& addAsgn (SlipDatum& Y, const SlipDatum& X) const;        // operator+=
      virtual SlipDatum& subAsgn (SlipDatum& Y, const SlipDatum& X) const;        // operator-=
      virtual SlipDatum& mulAsgn (SlipDatum& Y, const SlipDatum& X) const;        // operator*=
      virtual SlipDatum& divAsgn (SlipDatum& Y, const SlipDatum& X) const;        // operator/=
      virtual SlipDatum& modAsgn (SlipDatum& Y, const SlipDatum& X) const;        // operator%=
      virtual SlipDatum& shlAsgn (SlipDatum& Y, const SlipDatum& X) const;        // operator<<=
      virtual SlipDatum& shrAsgn (SlipDatum& Y, const SlipDatum& X) const;        // operator>>=
      virtual SlipDatum& bandAsgn (SlipDatum& Y, const SlipDatum& X) const;       // operator&=
      virtual SlipDatum& borAsgn  (SlipDatum& Y, const SlipDatum& X) const;       // operator|=
      virtual SlipDatum& bxorAsgn (SlipDatum& Y, const SlipDatum& X) const;       // operator^=

      virtual SlipDatum& addAsgn (SlipDatum& Y, const DOUBLE X) const;            // operator+=
      virtual SlipDatum& subAsgn (SlipDatum& Y, const DOUBLE X) const;            // operator-=
      virtual SlipDatum& mulAsgn (SlipDatum& Y, const DOUBLE X) const;            // operator*=
      virtual SlipDatum& divAsgn (SlipDatum& Y, const DOUBLE X) const;            // operator/=

      virtual SlipDatum& addAsgn (SlipDatum& Y, const ULONG  X) const;            // operator+=
      virtual SlipDatum& subAsgn (SlipDatum& Y, const ULONG  X) const;            // operator-=
      virtual SlipDatum& mulAsgn (SlipDatum& Y, const ULONG  X) const;            // operator*=
      virtual SlipDatum& divAsgn (SlipDatum& Y, const ULONG  X) const;            // operator/=
      virtual SlipDatum& modAsgn (SlipDatum& Y, const ULONG  X) const;            // operator%=
      virtual SlipDatum& shlAsgn (SlipDatum& Y, const ULONG  X) const;            // operator<<=
      virtual SlipDatum& shrAsgn (SlipDatum& Y, const ULONG  X) const;            // operator>>=
      virtual SlipDatum& bandAsgn(SlipDatum& Y, const ULONG  X) const;            // operator&=
      virtual SlipDatum& borAsgn (SlipDatum& Y, const ULONG  X) const;            // operator|=
      virtual SlipDatum& bxorAsgn(SlipDatum& Y, const ULONG  X) const;            // operator^=

      virtual SlipDatum& addAsgn (SlipDatum& Y, const LONG   X) const;            // operator+=
      virtual SlipDatum& subAsgn (SlipDatum& Y, const LONG   X) const;            // operator-=
      virtual SlipDatum& mulAsgn (SlipDatum& Y, const LONG   X) const;            // operator*=
      virtual SlipDatum& divAsgn (SlipDatum& Y, const LONG   X) const;            // operator/=
      virtual SlipDatum& modAsgn (SlipDatum& Y, const LONG   X) const;            // operator%=
      virtual SlipDatum& shlAsgn (SlipDatum& Y, const LONG   X) const;            // operator<<=
      virtual SlipDatum& shrAsgn (SlipDatum& Y, const LONG   X) const;            // operator>>=
      virtual SlipDatum& bandAsgn(SlipDatum& Y, const LONG   X) const;            // operator&=
      virtual SlipDatum& borAsgn (SlipDatum& Y, const LONG   X) const;            // operator|=
      virtual SlipDatum& bxorAsgn(SlipDatum& Y, const LONG   X) const;            // operator^=

          /*************************************************
           *              Logical operators                *
          *************************************************/
      virtual bool       notEqual  (const SlipDatum& Y, const SlipDatum& X) const;// operator!=
      virtual bool       lessThan  (const SlipDatum& Y, const SlipDatum& X) const;// operator<
      virtual bool       lessEqual (const SlipDatum& Y, const SlipDatum& X) const;// operator<=
      virtual bool       equal     (const SlipDatum& Y, const SlipDatum& X) const;// operator==
      virtual bool       greatEqual(const SlipDatum& Y, const SlipDatum& X) const;// operator>=
      virtual bool       greatThan (const SlipDatum& Y, const SlipDatum& X) const;// operator>

      virtual bool       notEqual  (const SlipDatum& Y, const DOUBLE X) const;    // operator!=
      virtual bool       lessThan  (const SlipDatum& Y, const DOUBLE X) const;    // operator<
      virtual bool       lessEqual (const SlipDatum& Y, const DOUBLE X) const;    // operator<=
      virtual bool       equal     (const SlipDatum& Y, const DOUBLE X) const;    // operator==
      virtual bool       greatEqual(const SlipDatum& Y, const DOUBLE X) const;    // operator>=
      virtual bool       greatThan (const SlipDatum& Y, const DOUBLE X) const;    // operator>

      virtual bool       notEqual  (const SlipDatum& Y, const ULONG  X) const;    // operator!=
      virtual bool       lessThan  (const SlipDatum& Y, const ULONG  X) const;    // operator<
      virtual bool       lessEqual (const SlipDatum& Y, const ULONG  X) const;    // operator<=
      virtual bool       equal     (const SlipDatum& Y, const ULONG  X) const;    // operator==
      virtual bool       greatEqual(const SlipDatum& Y, const ULONG  X) const;    // operator>=
      virtual bool       greatThan (const SlipDatum& Y, const ULONG  X) const;    // operator>

      virtual bool       notEqual  (const SlipDatum& Y, const LONG   X) const;    // operator!=
      virtual bool       lessThan  (const SlipDatum& Y, const LONG   X) const;    // operator<
      virtual bool       lessEqual (const SlipDatum& Y, const LONG   X) const;    // operator<=
      virtual bool       equal     (const SlipDatum& Y, const LONG   X) const;    // operator==
      virtual bool       greatEqual(const SlipDatum& Y, const LONG   X) const;    // operator>=
      virtual bool       greatThan (const SlipDatum& Y, const LONG   X) const;    // operator>

      virtual bool       notEqual  (const SlipDatum& Y, const PTR    X) const;    // operator!=
      virtual bool       equal     (const SlipDatum& Y, const PTR    X) const;    // operator==

      virtual bool       notEqual  (const SlipDatum& Y, const string& X) const;   // operator!=
      virtual bool       lessThan  (const SlipDatum& Y, const string& X) const;   // operator<
      virtual bool       lessEqual (const SlipDatum& Y, const string& X) const;   // operator<=
      virtual bool       equal     (const SlipDatum& Y, const string& X) const;   // operator==
      virtual bool       greatEqual(const SlipDatum& Y, const string& X) const;   // operator>=
      virtual bool       greatThan (const SlipDatum& Y, const string& X) const;   // operator>
     
             /**********************************************
              *             Utility Functions              *
              **********************************************/   
      virtual SlipDatum subOP(const LONG Y, const SlipDatum& X) const;          // Y - X
      virtual SlipDatum subUOP(const ULONG Y, const SlipDatum& X) const;        // Y - X
      virtual SlipDatum subDOP(const DOUBLE Y, const SlipDatum& X) const;       // Y - X
      virtual SlipDatum divOP(const LONG Y, const SlipDatum& X) const;          // Y / X
      virtual SlipDatum divUOP(const ULONG Y, const SlipDatum& X) const;        // Y / X
      virtual SlipDatum divDOP(const DOUBLE Y, const SlipDatum& X) const;       // Y / X
      virtual SlipDatum modOP(const LONG Y, const SlipDatum& X) const;          // Y % X
      virtual SlipDatum modUOP(const ULONG Y, const SlipDatum& X) const;        // Y % X
      virtual LONG      shlOP(const LONG Y, const SlipDatum& X) const;          // Y << X
      virtual LONG      shrOP(const LONG Y, const SlipDatum& X) const;          // Y >> X

               /*****************************************
                *             Miscellaneous             *
               ******************************************/
      virtual SlipDatum& copy(const SlipDatum& X) const;                        // Return a reference to a copy of the current cell
      virtual const string  getName(SlipDatum* X) const;                        // string representing SlipDatumCell
      virtual sAllData  getDatumCopy(const SlipDatum& X) const;                 // Return valid datum
      virtual Parse     getParse(SlipDatum& X);                                 // return a pointer to a parser method
      virtual void      remove(const SlipDatum& X);                             // Delete the datum in this cell
      virtual string    toString(const SlipCell& X) const;                      // pretty print SLIP cell
      virtual string    dump(const SlipCell& X) const;                          // unprettyingly dump the data
      virtual string    write(const SlipCell& X) const;                         // ( user data )

   }; // class SlipPtrOp
}; // namespace slip

#endif	/* SLIPPTROP_H */

