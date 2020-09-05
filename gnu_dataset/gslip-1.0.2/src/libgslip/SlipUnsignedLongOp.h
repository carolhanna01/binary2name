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
 * @file    SlipUnsignedLongOp.h
 * @author  A. Schwarz
 * @date    June 6, 2012
 *
 */

#ifndef _SLIPUNSIGNEDLONGOP_H
#define	_SLIPUNSIGNEDLONGOP_H

# include "SlipDef.h"
# include "SlipOp.h"
# include <string>

using namespace std;

namespace slip {

   class SlipUnsignedLongOp : public SlipOp {
   private:
       SlipUnsignedLongOp(const SlipUnsignedLongOp& orig) { };
       ~SlipUnsignedLongOp() { };
   public:
      SlipUnsignedLongOp() : SlipOp ( 5                          // cellType
                                    , 5                          // ID
                                    , eULONG                     // classType
                                    , "ULONG"                    // name
                                    , true                       // isDataFlag
                                    , true                       // isDiscreteFlag
                                    , false                      // isHeaderFlag
                                    , true                       // isNumberFlag
                                    , false) { }                 // isSublistFlag
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
      virtual SlipDatum& preDec   (SlipDatum& X) const;                            // --operator

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

               /*****************************************
                *             Miscellaneous             *
               ******************************************/
      virtual string    toString(const SlipCell& X) const;                      // pretty print SLIP cell
      virtual string    dump(const SlipCell& X) const;                          // unprettyingly dump the data
      virtual string    write(const SlipCell& X) const;                         // #UL

   }; // class SlipUnsignedLongOp
}; // namespace slip

#endif	/* SLIPUNSIGNEDLONGOP_H */

