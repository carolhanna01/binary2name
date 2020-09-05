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
 * @file    Slip.h
 * @author  A. Schwarz
 * @date    December 13, 2012
 *
 */

# include "Slip.h"
# include "SlipCellBase.h"
# include "SlipErr.h"
# include "SlipOp.h"

//using namespace slip;
using namespace std;

namespace slip {
   /**
    * @class GetOperator
    * @brief Retrieve the operator field of a <em>SlipCell</em>
    *  <table>
    *    <caption>SlipCellBase structure</caption>
    *    <tr>
    *       <td></td>
    *       <td></td>
    *       <td></td>
    *       <td></td>
    *       <td></td>
    *       <td></td>
    *       <td></td>
    *       <td></td>
    *       <td></td>
    *       <td></td>
    *       <td></td>
    *       <td></td>
    *       <td></td>
    *       <td></td>
    *       <td></td>
    *       <td></td>
    *    </tr>
    *    <tr>
    *       <td colspan="8">leftLink</td>
    *       <td colspan="8">rightLink</td>
    *    </tr>
    *    <tr>
    *       <td colspan="8">operator</td>
    *       <td colspan="8">Datum</td>
    *    </tr>
    *    <tr>
    *       <td colspan="8">Datum</td>
    *    </tr>
    * </table>
    * <br />
    * <p>Where the field definitions are:</p>
    * <ul>
    *    <li><b>leftLink</b></li>
    *    <li><b>rightLink</b></li>
    *    <li><b>operator</b> pointer to invariant properties of the Slip cell type.</li>
    *    <li><b>Datum</b> defined by the derived object.</li>
    * </ul>
    *
    */
   class GetOperator : public SlipCellBase {
   public:
      /**
       * @brief Retrieve the operator field of a <em>SlipCell</em>
       * @param[in] X (SlipDatum& ) reference of cell containing a operator field
       * @return pointer to <em>SlipOp</em> subclass for the input cell
       */
      static  SlipOp* getOperator(const SlipDatum& X) { return (SlipOp*)*SlipCellBase::getOperator((SlipCellBase*)&X); }

      /**
       * @brief Required method of a <em>SlipCellBase</em> subclass
       * <p>Does nothing.</p>
       */
      virtual string toString() const { return ""; }
   }; // class GetOperator : public SlipCellBase {

   static GetOperator op;

          /*************************************************
           *                Binary operators               *
          *************************************************/
   /**
    * @brief Add <b>X</b> + <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> + <b>Y</b> is commuted to
    *    <b>Y</b> + <b>X</b>. The <b>Y</b> + <b>X</b> format is used in
    *    expression evaluation</p>
    * @copydetails slip::SlipDatum::operator+(const DOUBLE)
    * @param[in] X addend
    * @param[in] Y bool, discrete number, DOUBLE value
    * @return (SlipDatum) value of the computation
    */
   SlipDatum  operator+(const DOUBLE X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) + X; };          // X + Y

   /**
    * @brief Subtract <b>X</b> - <b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> - <b>Y</b> is not
    *    commutative with <b>Y</b> - <b>X</b>. The <b>X</b> - <b>Y</b>
    *    format is used in expression evaluation.</p>
    * @copydetails slip::SlipOp::subDOP()
    * @param[in] X (DOUBLE) 'X - Y'
    * @param[in] Y (SlipDatum&) 'X - Y'
    * @return (SlipDatum) value of the computation
    */
   SlipDatum  operator-(const DOUBLE X, const SlipDatum& Y) { return op.getOperator(Y)->subDOP(X, Y); };        // X - Y

   /**
    * @brief Multiply <b>X</b> * <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> * <b>Y</b> is commuted to
    *    <b>Y</b> * <b>X</b>. The <b>Y</b> * <b>X</b> format is used in
    *    expression evaluation.</p>
    * @copydetails slip::SlipDatum::operator*(const DOUBLE)
    * @param[in] X (DOUBLE) 'X * Y'
    * @param[in] Y (SlipDatum&) 'X * Y'
    * @return (SlipDatum) value of the computation
    */
   SlipDatum  operator*(const DOUBLE X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) * X; };          // X * Y

   /**
    * @brief Divide <b>X</b> / <b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> / <b>Y</b> is not
    *    commutative with <b>Y</b> / <b>X</b>. The <b>X</b> / <b>Y</b>
    *    format is used in expression evaluation</p>
    * @copydetails slip::SlipOp::divDOP()
    * @param[in] X (DOUBLE) 'X / Y'
    * @param[in] Y (SlipDatum&) 'X / Y'
    * @return (SlipDatum) value of the computation
    */
   SlipDatum  operator/(const DOUBLE X, const SlipDatum& Y) { return op.getOperator(Y)->divDOP(X, Y); };         // X / Y

   /**
    * @brief Add <b>X</b> + <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> + <b>Y</b> is commuted to
    *    <b>Y</b> + <b>X</b>. The <b>Y</b> + (LONG)<b>X</b> format is used in
    *    expression evaluation.</p>
    * <p>Integer promotion of the bool to a LONG is done before evaluation.</p>
    * @copydetails slip::SlipDatum::operator+(const LONG)
    * @param[in] X (bool) 'X + Y'
    * @param[in] Y (SlipDatum&) 'X + Y'
    * @return (SlipDatum) value of the computation
    */
   SlipDatum  operator+(const bool   X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) + (LONG)X; };    // X + Y

   /**
    * @brief Subtract <b>X</b> - <b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> - <b>Y</b> is not
    *    commutative with <b>Y</b> - <b>X</b>. The <b>X</b> - (LONG)<b>Y</b>
    *    format is used in expression evaluation</p>
    * <p>Integer promotion of the bool to a LONG is done before evaluation.</p>
    * @copydetails slip::SlipOp::subOP()
    * @param[in] X (bool) 'X - Y'
    * @param[in] Y (SlipDatum&) 'X - Y'
    * @return (SlipDatum) value of the computation
    */
   SlipDatum  operator-(const bool   X, const SlipDatum& Y) { return op.getOperator(Y)->subOP((LONG)X, Y); };   // X - Y

   /**
    * @brief Multiply <b>X</b> * <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> * <b>Y</b> is commuted to
    *    <b>Y</b> * <b>X</b>. The <b>Y</b> * <b>X</b> format is used in
    *    expression evaluation</p>
    * <p>Integer promotion of the bool to a LONG is done before evaluation.</p>
    * @copydetails slip::SlipDatum::operator*(const LONG)
    * @param[in] X (bool) 'X * Y'
    * @param[in] Y (SlipDatum&) 'X * Y'
    * @return (SlipDatum) value of the computation
    */
   SlipDatum  operator*(const bool   X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) * (LONG)X; };    // X * Y

   /**
    * @brief Divide <b>X</b> / <b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> / <b>Y</b> is not
    *    commutative with <b>Y</b> / <b>X</b>. The <b>X</b> / (LONG)<b>Y</b>
    *    format is used in expression evaluation</p>
    * <p>Integer promotion of the bool to a LONG is done before evaluation.</p>
    * @copydetails slip::SlipOp::divOP()
    * @param[in] X (bool) 'X / Y'
    * @param[in] Y (SlipDatum&) 'X / Y'
    * @return (SlipDatum)value of the computation
    */
   SlipDatum  operator/(const bool   X, const SlipDatum& Y) { return op.getOperator(Y)->divOP((LONG)X, Y); };   // X / Y

   /**
    * @brief Take the modulus <b>X</b> % <b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> % <b>Y</b> is not
    *    commutative with <b>Y</b> % <b>X</b>. The <b>X</b> % (LONG)<b>Y</b>
    *    format is used in expression evaluation</p>
    * <p>Integer promotion of the UCHAR to a ULONG is done before evaluation.</p>
    * @param[in] X (bool) target number
    * @param[in] Y (SlipDatum&) modulus
    * @return (LONG) result of <b>X</b> % <b>Y</b>
    */
   LONG       operator%(const bool   X, const SlipDatum& Y) { return X % (LONG)Y; };                            // X % Y

   /**
    * @brief Add <b>X</b> + <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> + <b>Y</b> is commuted to
    *    <b>Y</b> + <b>X</b>. The <b>Y</b> + (ULONG)<b>X</b> format is used in
    *    expression evaluation.</p>
    * <p>Integer promotion of the UCHAR to a ULONG is done before evaluation.</p>
    * @copydetails slip::SlipDatum::operator+(const ULONG)
    * @param[in] X (UCHAR) 'X + Y'
    * @param[in] Y (SlipDatum&) 'X + Y'
    * @return (SlipDatum) value of the computation
    */
   SlipDatum  operator+(const UCHAR  X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) + (ULONG)X; };   // X + Y

   /**
    * @brief Subtract <b>X</b> - <b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> - <b>Y</b> is not
    *    commutative with <b>Y</b> - <b>X</b>. The <b>X</b> - (ULONG)<b>Y</b>
    *    format is used in expression evaluation</p>
    * <p>Integer promotion of the UCHAR to a ULONG is done before evaluation.</p>
    * @copydetails slip::SlipOp::subUOP()
    * @param[in] X (UCHAR) 'X - Y'
    * @param[in] Y (SlipDatum&) 'X - Y'
    * @return (SlipDatum) value of the computation
    */
   SlipDatum  operator-(const UCHAR  X, const SlipDatum& Y) { return op.getOperator(Y)->subUOP((ULONG)X, Y); }; // X - Y

   /**
    * @brief Multiply <b>X</b> * <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> * <b>Y</b> is commuted to
    *    <b>Y</b> * <b>X</b>. The <b>Y</b> * (ULONG)<b>X</b> format is used in
    *    expression evaluation</p>
    * <p>Integer promotion of the UCHAR to a ULONG is done before evaluation.</p>
    * @copydetails slip::SlipDatum::operator*(const ULONG)
    * @param[in] X (UCHAR) 'X * Y'
    * @param[in] Y (SlipDatum&) 'X * Y'
    * @return (SlipDatum) value of the computation
    */
   SlipDatum  operator*(const UCHAR  X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) * (ULONG)X; };   // X * Y

   /**
    * @brief Divide <b>X</b> / <b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> / <b>Y</b> is not
    *    commutative with <b>Y</b> / <b>X</b>. The <b>X</b> / (ULONG)<b>Y</b>
    *    format is used in expression evaluation</p>
    * <p>Integer promotion of the UCHAR to a ULONG is done before evaluation.</p>
    * @copydetails slip::SlipOp::divUOP()
    * @param[in] X (UCHAR) 'X / Y'
    * @param[in] Y (SlipDatum&) 'X / Y'
    * @return (SlipDatum) value of the computation
    */
   SlipDatum  operator/(const UCHAR  X, const SlipDatum& Y) { return op.getOperator(Y)->divUOP((ULONG)X, Y); }; // X / Y

   /**
    * @brief Take the modulus <b>X</b> % <b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> % <b>Y</b> is not
    *    commutative with <b>Y</b> % <b>X</b>. The <b>X</b> % (ULONG)<b>Y</b>
    *    format is used in expression evaluation</p>
    * <p>Integer promotion of the UCHAR to a ULONG is done before evaluation.</p>
    * @param[in] X (UCHAR) target number
    * @param[in] Y (SlipDatum&) Y modulus
    * @return (ULONG) result of <b>X</b> % <b>Y</b>
    */
   LONG       operator%(const UCHAR  X, const SlipDatum& Y) { return X % (ULONG )Y; };                          // Y % V


   /**
    * @brief Add <b>X</b> + <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> + <b>Y</b> is commuted to
    *    <b>Y</b> + <b>X</b>. The <b>Y</b> + (LONG)<b>X</b> format is used in
    *    expression evaluation.</p>
    * <p>Integer promotion of the CHAR to a LONG is done before evaluation.</p>
    * @copydetails slip::SlipDatum::operator+(const LONG)
    * @param[in] X (CHAR) 'X + Y'
    * @param[in] Y (SlipDatum&) 'X + Y'
    * @return (SlipDatum) value of the computation
    */
   SlipDatum  operator+(const CHAR   X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) + (LONG)X; };    // X * Y

   /**
    * @brief Subtract <b>X</b> - <b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> - <b>Y</b> is not
    *    commutative with <b>Y</b> - <b>X</b>. The <b>X</b> - (LONG)<b>Y</b>
    *    format is used in expression evaluation</p>
    * <p>Integer promotion of the CHAR to a LONG is done before evaluation.</p>
    * @copydetails slip::SlipOp::subOP()
    * @param[in] X (CHAR) 'X - Y'
    * @param[in] Y (SlipDatum&) 'X - Y'
    * @return (SlipDatum) value of the computation
    */
   SlipDatum  operator-(const CHAR   X, const SlipDatum& Y) { return op.getOperator(Y)->subOP((LONG)X, Y); };   // X - Y

   /**
    * @brief Multiply <b>X</b> * <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> * <b>Y</b> is commuted to
    *    <b>Y</b> * <b>X</b>. The <b>Y</b> * (LONG)<b>X</b> format is used in
    *    expression evaluation</p>
    * <p>Integer promotion of the CHAR to a LONG is done before evaluation.</p>
    * @copydetails slip::SlipDatum::operator*(const LONG)
    * @param[in] X (CHAR) 'X * Y'
    * @param[in] Y (SlipDatum&) 'X * Y'
    * @return (SlipDatum) value of the computation
    */
   SlipDatum  operator*(const CHAR   X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) * (LONG)X; };    // X * Y

   /**
    * @brief Divide <b>X</b> / <b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> / <b>Y</b> is not
    *    commutative with <b>Y</b> / <b>X</b>. The <b>X</b> / (LONG)<b>Y</b>
    *    format is used in expression evaluation</p>
    * <p>Integer promotion of the CHAR to a LONG is done before evaluation.</p>
    * @copydetails slip::SlipOp::divOP()
    * @param[in] X (CHAR) 'X / Y'
    * @param[in] Y (SlipDatum&) 'X / Y'
    * @return (SlipDatum) value of the computation
    */
   SlipDatum  operator/(const CHAR   X, const SlipDatum& Y) { return op.getOperator(Y)->divOP((LONG)X, Y); };   // X / Y

   /**
    * @brief Take the modulus <b>X</b> % <b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> % <b>Y</b> is not
    *    commutative with <b>Y</b> % <b>X</b>. The <b>X</b> % (LONG)<b>Y</b>
    *    format is used in expression evaluation</p>
    * <p>Integer promotion of the CHAR to a LONG is done before evaluation.</p>
    * @param[in] X (CHAR) 'X % Y'
    * @param[in] Y (SlipDatum&) 'X % Y'
    * @return (LONG) result of <b>X</b> % <b>Y</b>
    */
   LONG       operator%(const CHAR   X, const SlipDatum& Y) { return X % (LONG )Y; };                           // Y % V


   /**
    * @brief Add <b>X</b> + <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> + <b>Y</b> is commuted to
    *    <b>Y</b> + <b>X</b>. The <b>Y</b> + <b>X</b> format is used in
    *    expression evaluation.</p>
    * @copydetails slip::SlipDatum::operator+(const ULONG)
    * @param[in] X (ULONG) 'X + Y'
    * @param[in] Y (SlipDatum&) 'X + Y'
    * @return (SlipDatum) value of the computation
    */
   SlipDatum  operator+(const ULONG  X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) + X; };          // X * Y

   /**
    * @brief Subtract <b>X</b> - <b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> - <b>Y</b> is not
    *    commutative with <b>Y</b> - <b>X</b>. The <b>X</b> - <b>Y</b>
    *    format is used in expression evaluation</p>
    * @copydetails slip::SlipOp::subUOP()
    * @param[in] X (ULONG) 'X - Y'
    * @param[in] Y (SlipDatum&) 'X - Y'
    * @return (SlipDatum) value of the computation
    */
   SlipDatum  operator-(const ULONG  X, const SlipDatum& Y) { return op.getOperator(Y)->subUOP(X, Y); };        // X - Y

   /**
    * @brief Multiply <b>X</b> * <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> * <b>Y</b> is commuted to
    *    <b>Y</b> * <b>X</b>. The <b>Y</b> * <b>X</b> format is used in
    *    expression evaluation</p>
    * @copydetails slip::SlipDatum::operator*(const ULONG)
    * @param[in] X (ULONG) 'X * Y'
    * @param[in] Y (SlipDatum&) 'X * Y'
    * @return (SlipDatum) value of the computation
    */
   SlipDatum  operator*(const ULONG  X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) * X; };          // X * Y

   /**
    * @brief Divide <b>X</b> / <b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> / <b>Y</b> is not
    *    commutative with <b>Y</b> / <b>X</b>. The <b>X</b> / <b>Y</b>
    *    format is used in expression evaluation</p>
    * @copydetails slip::SlipOp::divUOP()
    * @param[in] X (ULONG) 'X / Y'
    * @param[in] Y (SlipDatum&) 'X / Y'
    * @return (SlipDatum) value of the computation
    */
   SlipDatum  operator/(const ULONG  X, const SlipDatum& Y) { return op.getOperator(Y)->divUOP(X, Y); };        // X / Y

   /**
    * @brief Take the modulus <b>X</b> % <b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> % <b>Y</b> is not
    *    commutative with <b>Y</b> % <b>X</b>. The <b>X</b> % (ULONG)<b>Y</b>
    *    format is used in expression evaluation</p>
    * <p>Casting of the input <b>Y</b> to ULONG is done before evaluation.</p>
    * @param[in] X (ULONG) 'X % Y'
    * @param[in] Y (SlipDatum&) 'X % Y'
    * @return  (LONG) result of <b>X</b> % <b>Y</b>
    */
   LONG       operator%(const ULONG  X, const SlipDatum& Y) { return X % (ULONG )Y; };                          // Y % V

   /**
    * @brief Add <b>X</b> + <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> + <b>Y</b> is commuted to
    *    <b>Y</b> + <b>X</b>. The <b>Y</b> + <b>X</b> format is used in
    *    expression evaluation.</p>
    * @copydetails slip::SlipDatum::operator+(const LONG)
    * @param[in] X (LONG) 'X + Y'
    * @param[in] Y (SlipDatum&) 'X + Y'
    * @return (SlipDatum) value of the computation
    */
   SlipDatum  operator+(const LONG   X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) + X; };          // X * Y

   /**
    * @brief Subtract <b>X</b> - <b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> - <b>Y</b> is not
    *    commutative with <b>Y</b> - <b>X</b>. The <b>X</b> - <b>Y</b>
    *    format is used in expression evaluation</p>
    * @copydetails slip::SlipOp::subOP()
    * @param[in] X (LONG) 'X - Y'
    * @param[in] Y (SlipDatum&) 'X - Y'
    * @return (SlipDatum) value of the computation
    */
   SlipDatum  operator-(const LONG   X, const SlipDatum& Y) { return op.getOperator(Y)->subOP(X, Y); };         // X - Y

   /**
    * @brief Multiply <b>X</b> * <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> * <b>Y</b> is commuted to
    *    <b>Y</b> * <b>X</b>. The <b>Y</b> * <b>X</b> format is used in
    *    expression evaluation</p>
    * @copydetails slip::SlipDatum::operator*(const LONG)
    * @param[in] X (LONG) 'X * Y'
    * @param[in] Y (SlipDatum&) 'X * Y'
    * @return (SlipDatum) value of the computation
    */
   SlipDatum  operator*(const LONG   X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) * X; };          // X * Y

   /**
    * @brief Divide <b>X</b> / <b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> / <b>Y</b> is not
    *    commutative with <b>Y</b> / <b>X</b>. The <b>X</b> / <b>Y</b>
    *    format is used in expression evaluation</p>
    * @copydetails slip::SlipOp::divOP()
    * @param[in] X (LONG) 'X / Y'
    * @param[in] Y (SlipDatum&) 'X / Y'
    * @return (SlipDatum) value of the computation
    */
   SlipDatum  operator/(const LONG   X, const SlipDatum& Y) { return op.getOperator(Y)->divOP(X, Y); };         // X / Y

   /**
    * @brief Take the modulus <b>X</b> % <b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> % <b>Y</b> is not
    *    commutative with <b>Y</b> % <b>X</b>. The <b>X</b> % <b>Y</b>
    *    format is used in expression evaluation</p>
    * <p>Casting of the input <b>Y</b> to ULONG is done before evaluation.</p>
    * @param[in] X (LONG) target number
    * @param[in] Y (SlipDatum&) modulus
    * @return  (LONG) result of <b>X</b> % <b>Y</b>
    */
   LONG       operator%(const LONG   X, const SlipDatum& Y) { return X % (LONG)Y; };                             // Y % V

       /*************************************************
        *                Bit operators                  *
       *************************************************/

   /**
    * @brief Shift left <b>X</b> &lt;&lt; <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> &lt;&lt; <b>Y</b> is
    *    not the same as <b>Y</b> &lt;&lt; <b>X</b>.  (LONG)<b>X</b> &lt;&lt;
    *    <b>Y</b> is used for expression evaluation.</p>
    * <p>The input, (bool)<b>X</b> is widened to a (LONG)<b>X</b>.</p>
    * @copydetails slip::SlipOp::shlOP()
    * @param[in] X (bool) 'X &lt;&lt; Y'
    * @param[in] Y (SlipDatum&) 'X &lt;&lt; Y'
    * @return (SlipDatum) value of the computation
    */
   LONG       operator<<(const bool   X, const SlipDatum& Y) { return op.getOperator(Y)->shlOP((LONG)X, Y); };   // X << Y

   /**
    * @brief Shift right <b>X</b> &gt;&gt; <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> &gt;&gt; <b>Y</b> is
    *    not the same as <b>Y</b> &gt;&gt; <b>X</b>.  (LONG)<b>X</b> &gt;&gt;
    *    <b>Y</b> is used for expression evaluation.</p>
    * <p>The input, (bool)<b>X</b> is widened to a (LONG)<b>X</b>.</p>
    * @copydetails slip::SlipOp::shrOP()
    * @param[in] X (bool) 'X &gt;&gt; Y'
    * @param[in] Y (SlipDatum&) 'X &gt;&gt; Y'
    * @return (LONG) value of the computation
    */
   LONG       operator>>(const bool   X, const SlipDatum& Y) { return op.getOperator(Y)->shrOP((LONG)X, Y); };   // X >> Y

   /**
    * @brief 'and' <b>X</b> &amp; <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> &amp; <b>Y</b> is
    *    not the same as <b>Y</b> &amp; <b>X</b>.  (LONG)<b>X</b> &amp;
    *    <b>Y</b> is used for expression evaluation.</p>
    * <p>The input, (bool)<b>X</b> is widened to a (LONG)<b>X</b>.</p>
    * @copydetails SlipDatum::operator&(const LONG)
    * @param[in] X (bool) 'X &amp; Y'
    * @param[in] Y (SlipDatum&) 'X &amp; Y'
    * @return (LONG) value of the computation
    */
   LONG       operator&(const bool    X, const SlipDatum& Y) { return op.getOperator(Y)->band(Y, (LONG)X); };   // X & Y

   /**
    * @brief 'or' <b>X</b> \| <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> \| <b>Y</b> is
    *    not the same as <b>Y</b> \| <b>X</b>.  (LONG)<b>X</b> \|
    *    <b>Y</b> is used for expression evaluation.</p>
    * <p>The input, (bool)<b>X</b> is widened to a (LONG)<b>X</b>.</p>
    * @copydetails SlipDatum::operator|(const LONG)
    * @param[in] X (bool) 'X | Y'
    * @param[in] Y (SlipDatum&) 'X | Y'
    * @return (LONG) value of the computation
    */
   LONG       operator|(const bool    X, const SlipDatum& Y) { return op.getOperator(Y)->bor(Y, (LONG)X);  };   // X | Y

   /**
    * @brief 'xor' <b>X</b> ^ <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> ^ <b>Y</b> is
    *    not the same as <b>Y</b> ^ <b>X</b>.  (LONG)<b>X</b> ^
    *    <b>Y</b> is used for expression evaluation.</p>
    * <p>The input, (bool)<b>X</b> is widened to a (LONG)<b>X</b>.</p>
    * @copydetails SlipDatum::operator^(const LONG)
    * @param[in] X (bool) 'X ^ Y'
    * @param[in] Y (SlipDatum&) 'X ^ Y'
    * @return (LONG) value of the computation
    */
   LONG       operator^(const bool    X, const SlipDatum& Y) { return op.getOperator(Y)->bxor(Y, (LONG)X); };   // X ^ Y


   /**
    * @brief Shift left <b>X</b> &lt;&lt; <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> &lt;&lt; <b>Y</b> is
    *    not the same as <b>Y</b> &lt;&lt; <b>X</b>.  (LONG)<b>X</b> &lt;&lt;
    *    <b>Y</b> is used for expression evaluation.</p>
    * <p>The input, (UCHAR)<b>X</b> is widened to a (LONG)<b>X</b>.</p>
    * @copydetails slip::SlipOp::shlOP()
    * @param[in] X (UCHAR) 'X &lt;&lt; Y'
    * @param[in] Y (SlipDatum&) 'X &lt;&lt; Y'
    * @return (SlipDatum) value of the computation
    */
   LONG       operator<<(const UCHAR  X, const SlipDatum& Y) { return op.getOperator(Y)->shlOP((LONG)X, Y); };   // X << Y

   /**
    * @brief Shift right <b>X</b> &gt;&gt; <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> &gt;&gt; <b>Y</b> is
    *    not the same as <b>Y</b> &gt;&gt; <b>X</b>.  (LONG)<b>X</b> &gt;&gt;
    *    <b>Y</b> is used for expression evaluation.</p>
    * <p>The input, (UCHAR)<b>X</b> is widened to a (LONG)<b>X</b>.</p>
    * @copydetails slip::SlipOp::shrOP()
    * @param[in] X (UCHAR) 'X &gt;&gt; Y'
    * @param[in] Y (SlipDatum&) 'X &gt;&gt; Y'
    * @return (LONG) value of the computation
    */
   LONG       operator>>(const UCHAR  X, const SlipDatum& Y) { return op.getOperator(Y)->shrOP((LONG)X, Y); };   // X >> Y

   /**
    * @brief 'and' <b>X</b> &amp; <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> &amp; <b>Y</b> is
    *    not the same as <b>Y</b> &amp; <b>X</b>.  (LONG)<b>X</b> &amp;
    *    <b>Y</b> is used for expression evaluation.</p>
    * <p>The input, (UCHAR)<b>X</b> is widened to a (LONG)<b>X</b>.</p>
    * @copydetails SlipDatum::operator&(const ULONG)
    * @param[in] X (bool) 'X &amp; Y'
    * @param[in] Y (bool) 'X &amp; Y'
    * @return (LONG) value of the computation
    */
   LONG       operator&(const UCHAR   X, const SlipDatum& Y) { return op.getOperator(Y)->band(Y, (LONG)X); };   // X & Y

   /**
    * @brief 'or' <b>X</b> \| <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> \| <b>Y</b> is
    *    not the same as <b>Y</b> \| <b>X</b>.  (LONG)<b>X</b> \|
    *    <b>Y</b> is used for expression evaluation.</p>
    * <p>The input, (UCHAR)<b>X</b> is widened to a (LONG)<b>X</b>.</p>
    * @copydetails SlipDatum::operator|(const ULONG)
    * @param[in] X (UCHAR) 'X | Y'
    * @param[in] Y (SlipDatum&) 'X | Y'
    * @return (LONG) value of the computation
    */
   LONG       operator|(const UCHAR   X, const SlipDatum& Y) { return op.getOperator(Y)->bor(Y, (LONG)X);  };   // X | Y

   /**
    * @brief 'xor' <b>X</b> ^ <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> ^ <b>Y</b> is
    *    not the same as <b>Y</b> ^ <b>X</b>.  (LONG)<b>X</b> ^
    *    <b>Y</b> is used for expression evaluation.</p>
    * <p>The input, (UCHAR)<b>X</b> is widened to a (LONG)<b>X</b>.</p>
    * @copydetails SlipDatum::operator^(const LONG)
    * @param[in] X (LONG) 'X ^ Y'
    * @param[in] Y (SlipDatum&) 'X ^ Y'
    * @return (LONG) value of the computation
    */
   LONG       operator^(const UCHAR   X, const SlipDatum& Y) { return op.getOperator(Y)->bxor(Y, (LONG)X); };   // X ^ Y


   /**
    * @brief Shift left <b>X</b> &lt;&lt; <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> &lt;&lt; <b>Y</b> is
    *    not the same as <b>Y</b> &lt;&lt; <b>X</b>.  (LONG)<b>X</b> &lt;&lt;
    *    <b>Y</b> is used for expression evaluation.</p>
    * <p>The input, (CHAR)<b>X</b> is widened to a (LONG)<b>X</b>.</p>
    * @copydetails slip::SlipOp::shlOP()
    * @param[in] X (CHAR) 'X &lt;&lt; Y'
    * @param[in] Y (SlipDatum&) 'X &lt;&lt; Y'
    * @return (SlipDatum) value of the computation
    */
   LONG       operator<<(const CHAR   X, const SlipDatum& Y) { return op.getOperator(Y)->shlOP((LONG)X, Y); };   // X << Y

   /**
    * @brief Shift right <b>X</b> &gt;&gt; <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> &gt;&gt; <b>Y</b> is
    *    not the same as <b>Y</b> &gt;&gt; <b>X</b>.  (LONG)<b>X</b> &gt;&gt;
    *    <b>Y</b> is used for expression evaluation.</p>
    * <p>The input, (CHAR)<b>X</b> is widened to a (LONG)<b>X</b>.</p>
    * @copydetails slip::SlipOp::shrOP()
    * @param[in] X (CHAR) 'X &gt;&gt; Y'
    * @param[in] Y (SlipDatum&) 'X &gt;&gt; Y'
    * @return (LONG) value of the computation
    */
   LONG       operator>>(const CHAR   X, const SlipDatum& Y) { return op.getOperator(Y)->shrOP((LONG)X, Y); };   // X >> Y

   /**
    * @brief 'and' <b>X</b> &amp; <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> &amp; <b>Y</b> is
    *    not the same as <b>Y</b> &amp; <b>X</b>.  (LONG)<b>X</b> &amp;
    *    <b>Y</b> is used for expression evaluation.</p>
    * <p>The input, (CHAR)<b>X</b> is widened to a (LONG)<b>X</b>.</p>
    * @copydetails SlipDatum::operator&(const LONG)
    * @param[in] X (bool) 'X &amp; Y'
    * @param[in] Y (bool) 'X &amp; Y'
    * @return (LONG) value of the computation
    */
   LONG       operator&(const CHAR    X, const SlipDatum& Y) { return op.getOperator(Y)->band(Y, (LONG)X); };   // X & Y

   /**
    * @brief 'or' <b>X</b> \| <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> \| <b>Y</b> is
    *    not the same as <b>Y</b> \| <b>X</b>.  (LONG)<b>X</b> \|
    *    <b>Y</b> is used for expression evaluation.</p>
    * <p>The input, (CHAR)<b>X</b> is widened to a (LONG)<b>X</b>.</p>
    * @copydetails SlipDatum::operator|(const LONG)
    * @param[in] X (CHAR) 'X | Y'
    * @param[in] Y (SlipDatum&) 'X | Y'
    * @return (LONG) value of the computation
    */
   LONG       operator|(const CHAR    X, const SlipDatum& Y) { return op.getOperator(Y)->bor(Y, (LONG)X);  };   // X | Y

   /**
    * @brief 'xor' <b>X</b> ^ <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> ^ <b>Y</b> is
    *    not the same as <b>Y</b> ^ <b>X</b>.  (LONG)<b>X</b> ^
    *    <b>Y</b> is used for expression evaluation.</p>
    * <p>The input, (CHAR)<b>X</b> is widened to a (LONG)<b>X</b>.</p>
    * @copydetails SlipDatum::operator^(const LONG)
    * @param[in] X (CHAR) 'X ^ Y'
    * @param[in] Y (SlipDatum&) 'X ^ Y'
    * @return (LONG) value of the computation
    */
   LONG       operator^(const CHAR    X, const SlipDatum& Y) { return op.getOperator(Y)->bxor(Y, (LONG)X); };   // X ^ Y


   /**
    * @brief Shift left <b>X</b> &lt;&lt; <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> &lt;&lt; <b>Y</b> is
    *    not the same as <b>Y</b> &lt;&lt; <b>X</b>.  (LONG)<b>X</b> &lt;&lt;
    *    <b>Y</b> is used for expression evaluation.</p>
    * <p>The input, (ULONG)<b>X</b> is cast to a (LONG)<b>X</b>.</p>
    * @copydetails slip::SlipOp::shlOP()
    * @param[in] X (ULONG) 'X &lt;&lt; Y'
    * @param[in] Y (SlipDatum&) 'X &lt;&lt; Y'
    * @return (SlipDatum) value of the computation
    */
   LONG       operator<<(const ULONG  X, const SlipDatum& Y) { return op.getOperator(Y)->shlOP((LONG)X, Y); };   // X << Y

   /**
    * @brief Shift right <b>X</b> &gt;&gt; <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> &gt;&gt; <b>Y</b> is
    *    not the same as <b>Y</b> &gt;&gt; <b>X</b>.  (LONG)<b>X</b> &gt;&gt;
    *    <b>Y</b> is used for expression evaluation.</p>
    * <p>The input, (ULONG)<b>X</b> is cast to a (LONG)<b>X</b>.</p>
    * @copydetails slip::SlipOp::shrOP()
    * @param[in] X (ULONG) 'X &gt;&gt; Y'
    * @param[in] Y (SlipDatum&) 'X &gt;&gt; Y'
    * @return (LONG) value of the computationv
    */
   LONG       operator>>(const ULONG  X, const SlipDatum& Y) { return op.getOperator(Y)->shrOP((LONG)X, Y); };   // X >> Y

   /**
    * @brief 'and' <b>X</b> &amp; <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> &amp; <b>Y</b> is
    *    not the same as <b>Y</b> &amp; <b>X</b>.  (LONG)<b>X</b> &amp;
    *    <b>Y</b> is used for expression evaluation.</p>
    * <p>The input, (ULONG)<b>X</b> is cast to a (LONG)<b>X</b>.</p>
    * @copydetails SlipDatum::operator&(const ULONG)
    * @param[in] X (bool) 'X &amp; Y'
    * @param[in] Y (bool) 'X &amp; Y'
    * @return (LONG) value of the computation
    */
   LONG       operator&(const ULONG   X, const SlipDatum& Y) { return op.getOperator(Y)->band(Y, (LONG)X); };   // X & Y

   /**
    * @brief 'or' <b>X</b> \| <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> \| <b>Y</b> is
    *    not the same as <b>Y</b> \| <b>X</b>.  (LONG)<b>X</b> \|
    *    <b>Y</b> is used for expression evaluation.</p>
    * <p>The input, (ULONG)<b>X</b> is cast to a (LONG)<b>X</b>.</p>
    * @copydetails SlipDatum::operator|(const ULONG)
    * @param[in] X (UCHAR) 'X | Y'
    * @param[in] Y (SlipDatum&) 'X | Y'
    * @return (LONG) value of the computation
    */
   LONG       operator|(const ULONG   X, const SlipDatum& Y) { return op.getOperator(Y)->bor(Y, (LONG)X);  };   // X | Y

   /**
    * @brief 'xor' <b>X</b> ^ <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> ^ <b>Y</b> is
    *    not the same as <b>Y</b> ^ <b>X</b>.  (LONG)<b>X</b> ^
    *    <b>Y</b> is used for expression evaluation.</p>
    * <p>The input, (ULONG)<b>X</b> is cast to a (LONG)<b>X</b>.</p>
    * @copydetails SlipDatum::operator^(const ULONG)
    * @param[in] X (ULONG) 'X ^ Y'
    * @param[in] Y (SlipDatum&) 'X ^ Y'
    * @return (LONG) value of the computation
    */
   LONG       operator^(const ULONG   X, const SlipDatum& Y) { return op.getOperator(Y)->bxor(Y, (LONG)X); };   // X ^ Y


   /**
    * @brief Shift left <b>X</b> &lt;&lt; <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> &lt;&lt; <b>Y</b> is
    *    not the same as <b>Y</b> &lt;&lt; <b>X</b>.  (LONG)<b>X</b> &lt;&lt;
    *    <b>Y</b> is used for expression evaluation.</p>
    * @copydetails slip::SlipOp::shlOP()
    * @param[in] X (LONG) 'X &lt;&lt; Y'
    * @param[in] Y (SlipDatum&) 'X &lt;&lt; Y'
    * @return (SlipDatum) value of the computation
    */
   LONG       operator<<(const LONG   X, const SlipDatum& Y) { return op.getOperator(Y)->shlOP((LONG)X, Y); };   // X << Y

   /**
    * @brief Shift right <b>X</b> &gt;&gt; <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> &gt;&gt; <b>Y</b> is
    *    not the same as <b>Y</b> &gt;&gt; <b>X</b>.  (LONG)<b>X</b> &gt;&gt;
    *    <b>Y</b> is used for expression evaluation.</p>
    * @copydetails slip::SlipOp::shrOP()
    * @param[in] X (LONG) 'X &gt;&gt; Y'
    * @param[in] Y (SlipDatum&) 'X &gt;&gt; Y'
    * @return (LONG) value of the computation
    */
   LONG       operator>>(const LONG   X, const SlipDatum& Y) { return op.getOperator(Y)->shrOP((LONG)X, Y); };   // X >> Y

   /**
    * @brief 'and' <b>X</b> &amp; <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> &amp; <b>Y</b> is
    *    not the same as <b>Y</b> &amp; <b>X</b>.  (LONG)<b>X</b> &amp;
    *    <b>Y</b> is used for expression evaluation.</p>
    * @copydetails SlipDatum::operator&(const LONG)
    * @param[in] X (bool) 'X &amp; Y'
    * @param[in] Y (bool) 'X &amp; Y'
    * @return (LONG) value of the computation
    */
   LONG       operator&(const LONG    X, const SlipDatum& Y) { return op.getOperator(Y)->band(Y, (LONG)X); };   // X & Y

   /**
    * @brief 'or' <b>X</b> \| <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> \| <b>Y</b> is
    *    not the same as <b>Y</b> \| <b>X</b>.  (LONG)<b>X</b> \|
    *    <b>Y</b> is used for expression evaluation.</p>
    * @copydetails SlipDatum::operator|(const LONG)
    * @param[in] X (LONG) 'X | Y'
    * @param[in] Y (SlipDatum&) 'X | Y'
    * @return (LONG) value of the computation
    */
   LONG       operator|(const LONG    X, const SlipDatum& Y) { return op.getOperator(Y)->bor(Y, (LONG)X);  };   // X | Y

   /**
    * @brief 'xor' <b>X</b> ^ <b>Y</b>.
    * <p>The operation is commutative. <b>X</b> ^ <b>Y</b> is
    *    not the same as <b>Y</b> ^ <b>X</b>.  (LONG)<b>X</b> ^
    *    <b>Y</b> is used for expression evaluation.</p>
    * @copydetails SlipDatum::operator^(const LONG)
    * @param[in] X (LONG) 'X ^ Y'
    * @param[in] Y (SlipDatum&) 'X ^ Y'
    * @return (LONG) value of the computation
    */
   LONG       operator^(const LONG    X, const SlipDatum& Y) { return op.getOperator(Y)->bxor(Y, (LONG)X); };   // X ^ Y

       /*************************************************
        *       Arithmetic Assignment operators         *
       *************************************************/

   /**
    * @brief Execute <b>X</b> += <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> += <b>Y</b> is
    *    not the same as <b>Y</b> += <b>X</b>.  <b>X</b> += (DOUBLE)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a DOUBLE and the C++ operation is executed.</p>
    * @param[in] X (DOUBLE) LHS of 'X += Y'
    * @param[in] Y (SlipDatum&) RHS of 'X += Y'
    * @return (DOUBLE) result of assignment
    */
   DOUBLE     operator+=(DOUBLE& X, const SlipDatum& Y) { return X += (DOUBLE)Y; };                             // X += Y

   /**
    * @brief Execute <b>X</b> -= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> -= <b>Y</b> is
    *    not the same as <b>Y</b> -= <b>X</b>.  <b>X</b> -= (DOUBLE)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a DOUBLE and the C++ operation is executed.</p>
    * @param[in] X (DOUBLE) LHS of 'X -= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X -= Y'
    * @return (DOUBLE) result of assignment
    */
   DOUBLE     operator-=(DOUBLE& X, const SlipDatum& Y) { return X -= (DOUBLE)Y; };                             // X -= Y

   /**
    * @brief Execute <b>X</b> *= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> *= <b>Y</b> is
    *    not the same as <b>Y</b> *= <b>X</b>.  <b>X</b> *= (DOUBLE)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a DOUBLE and the C++ operation is executed.</p>
    * @param[in] X (DOUBLE) LHS of 'X *= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X *= Y'
    * @return (DOUBLE) result of assignment
    */
   DOUBLE     operator*=(DOUBLE& X, const SlipDatum& Y) { return X *= (DOUBLE)Y; };                             // Y *= V

   /**
    * @brief Execute <b>X</b> /= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> /= <b>Y</b> is
    *    not the same as <b>Y</b> /= <b>X</b>.  <b>X</b> /= (DOUBLE)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a DOUBLE and the C++ operation is executed.</p>
    * @param[in] X (DOUBLE) LHS of 'X /= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X /= Y'
    * @return (DOUBLE) result of assignment
    */
   DOUBLE     operator/=(DOUBLE& X, const SlipDatum& Y) { return X /= (DOUBLE)Y; };                             // X /= Y


   /**
    * @brief Execute <b>X</b> += <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> += <b>Y</b> is
    *    not the same as <b>Y</b> += <b>X</b>.  <b>X</b> += (bool)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a bool and the C++ operation is executed.
    *    The cast has the effect of converting a non-zero number to one, '1',
    *    prior to the operation. Therefore, the operation completes with the
    *    RHS being either 0 or 1, and is then converted to <b>true</b> or
    *    <b>false</b> depending on the result of <b>X</b> + <b>Y</b>.</p>
    * @param[in] X (bool&) LHS of 'X += Y'
    * @param[in] Y (SlipDatum&) RHS of 'X += Y'
    * @return (bool) result of assignment
    */
   bool       operator+=(bool&   X, const SlipDatum& Y) { return X += (bool  )Y; };                             // X += Y

   /**
    * @brief Execute <b>X</b> -= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> -= <b>Y</b> is
    *    not the same as <b>Y</b> -= <b>X</b>.  <b>X</b> -= (bool)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a bool and the C++ operation is executed.
    *    The cast has the effect of converting a non-zero number to one, '1',
    *    prior to the operation. Therefore, the operation completes with the
    *    RHS being either 0 or 1, and is then converted to <b>true</b> or
    *    <b>false</b> depending on the result of <b>X</b> - <b>Y</b>.</p>
    * @param[in] X (bool&) LHS of 'X -= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X -= Y'
    * @return (bool) result of assignment
    */
   bool       operator-=(bool&   X, const SlipDatum& Y) { return X -= (bool  )Y; };                             // X -= Y

   /**
    * @brief Execute <b>X</b> *= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> *= <b>Y</b> is
    *    not the same as <b>Y</b> *= <b>X</b>.  <b>X</b> *= (bool)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a bool and the C++ operation is executed.
    *    The cast has the effect of converting a non-zero number to one, '1',
    *    prior to the operation. Therefore, the operation completes with the
    *    RHS being either 0 or 1, and is then converted to <b>true</b> or
    *    <b>false</b> depending on the result of <b>X</b> * <b>Y</b>.</p>
    * @param[in] X (bool&) LHS of 'X *= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X *= Y'
    * @return (bool) result of assignment
    */
   bool       operator*=(bool&   X, const SlipDatum& Y) { return X *= (bool  )Y; };                             // X *= Y

   /**
    * @brief Execute <b>X</b> /= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> /= <b>Y</b> is
    *    not the same as <b>Y</b> *= <b>X</b>.  <b>X</b> /= (bool)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a bool and the C++ operation is executed.
    *    The cast has the effect of converting a non-zero number to one, '1',
    *    prior to the operation. Therefore, the operation completes with the
    *    RHS being either 0 or 1, and is then converted to <b>true</b> or
    *    <b>false</b> depending on the result of <b>X</b> / <b>Y</b>.</p>
    * @param[in] X (bool&) LHS of 'X /= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X /= Y'
    * @return (bool) result of assignment
    */
   bool       operator/=(bool&   X, const SlipDatum& Y) { return X /= (bool  )Y; };                             // X /= Y


   /**
    * @brief Execute <b>X</b> += <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> += <b>Y</b> is
    *    not the same as <b>Y</b> += <b>X</b>.  <b>X</b> += (ULONG)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a ULONG and the C++ operation is executed.</p>
    * @param[in] X (UCHAR&) LHS of 'X += Y'
    * @param[in] Y (SlipDatum&) RHS of 'X += Y'
    * @return (UCHAR) result of assignment
    */
   UCHAR      operator+=(UCHAR&  X, const SlipDatum& Y) { return X += (ULONG )Y; };                             // X += Y

   /**
    * @brief Execute <b>X</b> -= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> -= <b>Y</b> is
    *    not the same as <b>Y</b> -= <b>X</b>.  <b>X</b> -= (ULONG)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a ULONG and the C++ operation is executed.</p>
    * @param[in] X (UCHAR&) LHS of 'X -= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X -= Y'
    * @return (UCHAR) result of assignment
    */
   UCHAR      operator-=(UCHAR&  X, const SlipDatum& Y) { return X -= (ULONG )Y; };                             // X -= Y

   /**
    * @brief Execute <b>X</b> *= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> *= <b>Y</b> is
    *    not the same as <b>Y</b> *= <b>X</b>.  <b>X</b> *= (ULONG)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a ULONG and the C++ operation is executed.</p>
    * @param[in] X (UCHAR&) LHS of 'X *= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X *= Y'
    * @return (UCHAR) result of assignment
    */
   UCHAR      operator*=(UCHAR&  X, const SlipDatum& Y) { return X *= (ULONG )Y; };                             // X *= Y

   /**
    * @brief Execute <b>X</b> /= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> /= <b>Y</b> is
    *    not the same as <b>Y</b> /= <b>X</b>.  <b>X</b> /= (ULONG)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a ULONG and the C++ operation is executed.</p>
    * @param[in] X (UCHAR&) LHS of 'X /= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X /= Y'
    * @return (UCHAR) result of assignment
    */
   UCHAR      operator/=(UCHAR&  X, const SlipDatum& Y) { return X /= (ULONG )Y; };                             // X /= Y

   /**
    * @brief Execute <b>X</b> %= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> %= <b>Y</b> is
    *    not the same as <b>Y</b> %= <b>X</b>.  <b>X</b> %= (ULONG)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a ULONG and the C++ operation is executed.</p>
    * @param[in] X (UCHAR&) LHS of 'X %= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X %= Y'
    * @return (UCHAR) result of assignment
    */
   UCHAR      operator%=(UCHAR&  X, const SlipDatum& Y) { return X %= (ULONG )Y; };                             // X %= Y

   /**
    * @brief Execute <b>X</b> &lt;&lt;= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> &lt;&lt;= <b>Y</b> is
    *    not the same as <b>Y</b> &lt;&lt;= <b>X</b>.
    *    <b>X</b> &lt;&lt;= (ULONG)<b>Y</b> is used for expression
    *    evaluation.</p>
    * <p><b>Y</b> is cast to a ULONG and the C++ operation is executed.</p>
    * @param[in] X (UCHAR&) LHS of 'X &lt;&lt;= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X &lt;&lt;= Y'
    * @return (UCHAR) result of assignment
    */
   UCHAR      operator<<=(UCHAR& X, const SlipDatum& Y) { return X <<=(ULONG )Y; };                             // X <<= Y

   /**
    * @brief Execute <b>X</b> &gt;&gt;= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> &gt;&gt;= <b>Y</b> is
    *    not the same as <b>Y</b> &gt;&gt;= <b>X</b>.
    *    <b>X</b> &gt;&gt;= (ULONG)<b>Y</b> is used for expression
    *    evaluation.</p>
    * <p><b>Y</b> is cast to a ULONG and the C++ operation is executed.</p>
    * @param[in] X (UCHAR&) LHS of 'X &gt;&gt;= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X &gt;&gt;= Y'
    * @return (UCHAR) result of assignment
    */
   UCHAR      operator>>=(UCHAR& X, const SlipDatum& Y) { return X >>=(ULONG )Y; };                             // X >>= Y

   /**
    * @brief Execute <b>X</b> &amp;= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> &amp;= <b>Y</b> is
    *    not the same as <b>Y</b> &amp;= <b>X</b>.
    *    <b>X</b> &amp;= (ULONG)<b>Y</b> is used for expression
    *    evaluation.</p>
    * <p><b>Y</b> is cast to a ULONG and the C++ operation is executed.</p>
    * @param[in] X (UCHAR&) LHS of 'X &amp;= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X &amp;= Y'
    * @return (UCHAR) result of assignment
    */
   UCHAR      operator&=(UCHAR&  X, const SlipDatum& Y) { return X &= (ULONG )Y; };                             // X &= Y

   /**
    * @brief Execute <b>X</b> \|= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b>  \|= <b>Y</b> is
    *    not the same as <b>Y</b>  \|= <b>X</b>.
    *    <b>X</b>  \|= (ULONG)<b>Y</b> is used for expression
    *    evaluation.</p>
    * <p><b>Y</b> is cast to a ULONG and the C++ operation is executed.</p>
    * @param[in] X (UCHAR&) LHS of 'X \|= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X \|= Y'
    * @return (UCHAR) result of assignment
    */
   UCHAR      operator|=(UCHAR&  X, const SlipDatum& Y) { return X |= (ULONG )Y; };                             // X |= Y

   /**
    * @brief Execute <b>X</b> ^= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> ^= <b>Y</b> is
    *    not the same as <b>Y</b> ^= <b>X</b>.
    *    <b>X</b> ^= (ULONG)<b>Y</b> is used for expression
    *    evaluation.</p>
    * <p><b>Y</b> is cast to a ULONG and the C++ operation is executed.</p>
    * @param[in] X (UCHAR&) LHS of 'X ^= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X ^= Y'
    * @return (UCHAR) result of assignment
    */
   UCHAR      operator^=(UCHAR&  X, const SlipDatum& Y) { return X ^= (ULONG )Y; };                             // X ^= Y


   /**
    * @brief Execute <b>X</b> += <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> += <b>Y</b> is
    *    not the same as <b>Y</b> += <b>X</b>.  <b>X</b> += (LONG)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a LONG and the C++ operation is executed.</p>
    * @param[in] X (CHAR&) LHS of 'X += Y'
    * @param[in] Y (SlipDatum&) RHS of 'X += Y'
    * @return (CHAR) result of assignment
    */
   CHAR       operator+=(CHAR&   X, const SlipDatum& Y) { return X += (LONG  )Y; };                             // X += Y

   /**
    * @brief Execute <b>X</b> -= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> -= <b>Y</b> is
    *    not the same as <b>Y</b> -= <b>X</b>.  <b>X</b> -= (LONG)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a LONG and the C++ operation is executed.</p>
    * @param[in] X (CHAR&) LHS of 'X -= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X -= Y'
    * @return (CHAR) result of assignment
    */
   CHAR       operator-=(CHAR&   X, const SlipDatum& Y) { return X -= (LONG  )Y; };                             // X -= Y

   /**
    * @brief Execute <b>X</b> *= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> *= <b>Y</b> is
    *    not the same as <b>Y</b> *= <b>X</b>.  <b>X</b> *= (LONG)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a LONG and the C++ operation is executed.</p>
    * @param[in] X (CHAR&) LHS of 'X *= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X *= Y'
    * @return (CHAR) result of assignment
    */
   CHAR       operator*=(CHAR&   X, const SlipDatum& Y) { return X *= (LONG  )Y; };                             // X *= Y

   /**
    * @brief Execute <b>X</b> /= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> /= <b>Y</b> is
    *    not the same as <b>Y</b> /= <b>X</b>.  <b>X</b> /= (LONG)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a LONG and the C++ operation is executed.</p>
    * @param[in] X (CHAR&) LHS of 'X /= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X /= Y'
    * @return (CHAR) result of assignment
    */
   CHAR       operator/=(CHAR&   X, const SlipDatum& Y) { return X /= (LONG  )Y; };                             // X /= Y

   /**
    * @brief Execute <b>X</b> %= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> %= <b>Y</b> is
    *    not the same as <b>Y</b> %= <b>X</b>.  <b>X</b> %= (LONG)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a LONG and the C++ operation is executed.</p>
    * @param[in] X (CHAR&) LHS of 'X %= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X %= Y'
    * @return (CHAR) result of assignment
    */
   CHAR       operator%=(CHAR&   X, const SlipDatum& Y) { return X %= (LONG  )Y; };                             //  X %= Y

   /**
    * @brief Execute <b>X</b> &lt;&lt;= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> &lt;&lt;= <b>Y</b> is
    *    not the same as <b>Y</b> &lt;&lt;= <b>X</b>.
    *    <b>X</b> &lt;&lt;= (LONG)<b>Y</b> is used for expression
    *    evaluation.</p>
    * <p><b>Y</b> is cast to a LONG and the C++ operation is executed.</p>
    * @param[in] X (CHAR&) LHS of 'X &lt;&lt;= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X &lt;&lt;= Y'
    * @return (CHAR) result of assignment
    */
   CHAR       operator<<=(CHAR&  X, const SlipDatum& Y) { return X <<=(LONG  )Y; };                             // X <<= Y

   /**
    * @brief Execute <b>X</b> &gt;&gt;= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> &gt;&gt;= <b>Y</b> is
    *    not the same as <b>Y</b> &gt;&gt;= <b>X</b>.
    *    <b>X</b> &gt;&gt;= (LONG)<b>Y</b> is used for expression
    *    evaluation.</p>
    * <p><b>Y</b> is cast to a LONG and the C++ operation is executed.</p>
    * @param[in] X (CHAR&) LHS of 'X &gt;&gt;= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X &gt;&gt;= Y'
    * @return (CHAR) result of assignment
    */
   CHAR       operator>>=(CHAR&  X, const SlipDatum& Y) { return X >>=(LONG  )Y; };                             // X >>= Y

   /**
    * @brief Execute <b>X</b> &amp;= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> &amp;= <b>Y</b> is
    *    not the same as <b>Y</b> &amp;= <b>X</b>.
    *    <b>X</b> &amp;= (LONG)<b>Y</b> is used for expression
    *    evaluation.</p>
    * <p><b>Y</b> is cast to a LONG and the C++ operation is executed.</p>
    * @param[in] X (CHAR&) LHS of 'X &amp;= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X &amp;= Y'
    * @return (CHAR) result of assignment
    */
   CHAR       operator&=(CHAR&   X, const SlipDatum& Y) { return X &= (LONG  )Y; };                             // X &= Y

   /**
    * @brief Execute <b>X</b> \|= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b>  \|= <b>Y</b> is
    *    not the same as <b>Y</b>  \|= <b>X</b>.
    *    <b>X</b>  \|= (LONG)<b>Y</b> is used for expression
    *    evaluation.</p>
    * <p><b>Y</b> is cast to a LONG and the C++ operation is executed.</p>
    * @param[in] X (CHAR&) LHS of 'X \|= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X \|= Y'
    * @return (CHAR) result of assignment
    */
   CHAR       operator|=(CHAR&   X, const SlipDatum& Y) { return X |= (LONG  )Y; };                             // X |= Y

   /**
    * @brief Execute <b>X</b> ^= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> ^= <b>Y</b> is
    *    not the same as <b>Y</b> ^= <b>X</b>.
    *    <b>X</b> ^= (LONG)<b>Y</b> is used for expression
    *    evaluation.</p>
    * <p><b>Y</b> is cast to a LONG and the C++ operation is executed.</p>
    * @param[in] X (CHAR&) LHS of 'X ^= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X ^= Y'
    * @return (CHAR) result of assignment
    */
   CHAR       operator^=(CHAR&   X, const SlipDatum& Y) { return X ^= (LONG  )Y; };                             // X ^= Y


   /**
    * @brief Execute <b>X</b> += <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> += <b>Y</b> is
    *    not the same as <b>Y</b> += <b>X</b>.  <b>X</b> += (ULONG)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a ULONG and the C++ operation is executed.</p>
    * @param[in] X (ULONG&) LHS of 'X += Y'
    * @param[in] Y (SlipDatum&) RHS of 'X += Y'
    * @return (ULONG) result of assignment
    */
   ULONG      operator+=(ULONG&  X, const SlipDatum& Y) { return X += (ULONG )Y; };                             // X += Y

   /**
    * @brief Execute <b>X</b> -= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> -= <b>Y</b> is
    *    not the same as <b>Y</b> -= <b>X</b>.  <b>X</b> -= (ULONG)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a ULONG and the C++ operation is executed.</p>
    * @param[in] X (ULONG&) LHS of 'X -= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X -= Y'
    * @return (ULONG) result of assignment
    */
   ULONG      operator-=(ULONG&  X, const SlipDatum& Y) { return X -= (ULONG )Y; };                             // X -= Y

   /**
    * @brief Execute <b>X</b> *= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> *= <b>Y</b> is
    *    not the same as <b>Y</b> *= <b>X</b>.  <b>X</b> *= (ULONG)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a ULONG and the C++ operation is executed.</p>
    * @param[in] X (ULONG&) LHS of 'X *= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X *= Y'
    * @return (ULONG) result of assignment
    */
   ULONG      operator*=(ULONG&  X, const SlipDatum& Y) { return X *= (ULONG )Y; };                             // X *= Y

   /**
    * @brief Execute <b>X</b> /= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> /= <b>Y</b> is
    *    not the same as <b>Y</b> /= <b>X</b>.  <b>X</b> /= (ULONG)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a ULONG and the C++ operation is executed.</p>
    * @param[in] X (ULONG&) LHS of 'X /= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X /= Y'
    * @return (ULONG) result of assignment
    */
   ULONG      operator/=(ULONG&  X, const SlipDatum& Y) { return X /= (ULONG )Y; };                             // X /= Y

   /**
    * @brief Execute <b>X</b> %= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> %= <b>Y</b> is
    *    not the same as <b>Y</b> %= <b>X</b>.  <b>X</b> %= (ULONG)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a ULONG and the C++ operation is executed.</p>
    * @param[in] X (ULONG&) LHS of 'X %= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X %= Y'
    * @return (ULONG) result of assignment
    */
   ULONG      operator%=(ULONG&  X, const SlipDatum& Y) { return X %= (ULONG )Y; }                              //  X %= Y

   /**
    * @brief Execute <b>X</b> &lt;&lt;= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> &lt;&lt;= <b>Y</b> is
    *    not the same as <b>Y</b> &lt;&lt;= <b>X</b>.
    *    <b>X</b> &lt;&lt;= (ULONG)<b>Y</b> is used for expression
    *    evaluation.</p>
    * <p><b>Y</b> is cast to a ULONG and the C++ operation is executed.</p>
    * @param[in] X (ULONG&) LHS of 'X &lt;&lt;= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X &lt;&lt;= Y'
    * @return (ULONG) result of assignment
    */
   ULONG      operator<<=(ULONG& X, const SlipDatum& Y) { return X <<=(ULONG )Y; };                             // X <<= Y

   /**
    * @brief Execute <b>X</b> &gt;&gt;= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> &gt;&gt;= <b>Y</b> is
    *    not the same as <b>Y</b> &gt;&gt;= <b>X</b>.
    *    <b>X</b> &gt;&gt;= (ULONG)<b>Y</b> is used for expression
    *    evaluation.</p>
    * <p><b>Y</b> is cast to a ULONG and the C++ operation is executed.</p>
    * @param[in] X (ULONG&) LHS of 'X &gt;&gt;= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X &gt;&gt;= Y'
    * @return (ULONG) result of assignment
    */
   ULONG      operator>>=(ULONG& X, const SlipDatum& Y) { return X >>=(ULONG )Y; };                             // X >>= Y

   /**
    * @brief Execute <b>X</b> &amp;= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> &amp;= <b>Y</b> is
    *    not the same as <b>Y</b> &amp;= <b>X</b>.
    *    <b>X</b> &amp;= (ULONG)<b>Y</b> is used for expression
    *    evaluation.</p>
    * <p><b>Y</b> is cast to a ULONG and the C++ operation is executed.</p>
    * @param[in] X (ULONG&) LHS of 'X +&amp;= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X &amp;= Y'
    * @return (ULONG) result of assignment
    */
   ULONG      operator&=(ULONG&  X, const SlipDatum& Y) { return X &= (ULONG )Y; };                             // X &= Y

   /**
    * @brief Execute <b>X</b> \|= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b>  \|= <b>Y</b> is
    *    not the same as <b>Y</b>  \|= <b>X</b>.
    *    <b>X</b>  \|= (LONG)<b>Y</b> is used for expression
    *    evaluation.</p>
    * <p><b>Y</b> is cast to a LONG and the C++ operation is executed.</p>
    * @param[in] X (ULONG&) LHS of 'X \|= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X \|= Y'
    * @return (ULONG) result of assignment
    */
   ULONG      operator|=(ULONG&  X, const SlipDatum& Y) { return X |= (ULONG )Y; };                             // X |= Y

   /**
    * @brief Execute <b>X</b> ^= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> ^= <b>Y</b> is
    *    not the same as <b>Y</b> ^= <b>X</b>.
    *    <b>X</b> ^= (LONG)<b>Y</b> is used for expression
    *    evaluation.</p>
    * <p><b>Y</b> is cast to a LONG and the C++ operation is executed.</p>
    * @param[in] X (ULONG&) LHS of 'X ^= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X ^= Y'
    * @return (ULONG) result of assignment
    */
   ULONG      operator^=(ULONG&  X, const SlipDatum& Y) { return X ^= (ULONG )Y; };                             // X ^= Y


   /**
    * @brief Execute <b>X</b> += <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> += <b>Y</b> is
    *    not the same as <b>Y</b> += <b>X</b>.  <b>X</b> += (LONG)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a LONG and the C++ operation is executed.</p>
    * @param[in] X (LONG&) LHS of 'X += Y'
    * @param[in] Y (SlipDatum&) RHS of 'X += Y'
    * @return (LONG) result of assignment
    */
   LONG       operator+=(LONG&   X, const SlipDatum& Y) { return X += (LONG  )Y; };                             // X += Y

   /**
    * @brief Execute <b>X</b> -= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> -= <b>Y</b> is
    *    not the same as <b>Y</b> -= <b>X</b>.  <b>X</b> -= (LONG)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a LONG and the C++ operation is executed.</p>
    * @param[in] X (LONG&) LHS of 'X -= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X -= Y'
    * @return (LONG) result of assignment
    */
   LONG       operator-=(LONG&   X, const SlipDatum& Y) { return X -= (LONG  )Y; };                             // X -= Y

   /**
    * @brief Execute <b>X</b> *= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> *= <b>Y</b> is
    *    not the same as <b>Y</b> *= <b>X</b>.  <b>X</b> *= (LONG)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a LONG and the C++ operation is executed.</p>
    * @param[in] X (LONG&) LHS of 'X *= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X *= Y'
    * @return (LONG) result of assignment
    */
   LONG       operator*=(LONG&   X, const SlipDatum& Y) { return X *= (LONG  )Y; };                             // X *= Y

   /**
    * @brief Execute <b>X</b> /= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> /= <b>Y</b> is
    *    not the same as <b>Y</b> /= <b>X</b>.  <b>X</b> /= (LONG)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a LONG and the C++ operation is executed.</p>
    * @param[in] X (LONG&) LHS of 'X /= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X /= Y'
    * @return (LONG) result of assignment
    */
   LONG       operator/=(LONG&   X, const SlipDatum& Y) { return X /= (LONG  )Y; };                             // X /= Y

   /**
    * @brief Execute <b>X</b> %= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> %= <b>Y</b> is
    *    not the same as <b>Y</b> %= <b>X</b>.  <b>X</b> %= (LONG)<b>Y</b>
    *    is used for expression evaluation.</p>
    * <p><b>Y</b> is cast to a LONG and the C++ operation is executed.</p>
    * @param[in] X (LONG&) LHS of 'X %= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X %= Y'
    * @return (LONG) result of assignment
    */
   LONG       operator%=(LONG&   X, const SlipDatum& Y) { return X %= (LONG  )Y; };                             // X %= Y

   /**
    * @brief Execute <b>X</b> &lt;&lt;= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> &lt;&lt;= <b>Y</b> is
    *    not the same as <b>Y</b> &lt;&lt;= <b>X</b>.
    *    <b>X</b> &lt;&lt;= (LONG)<b>Y</b> is used for expression
    *    evaluation.</p>
    * <p><b>Y</b> is cast to a LONG and the C++ operation is executed.</p>
    * @param[in] X (LONG&) LHS of 'X &lt;&lt;= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X &lt;&lt;= Y'
    * @return (LONG) result of assignment
    */
   LONG       operator<<=(LONG&  X, const SlipDatum& Y) { return X <<=(LONG  )Y; };                             // X <<= Y

   /**
    * @brief Execute <b>X</b> &gt;&gt;= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> &gt;&gt;= <b>Y</b> is
    *    not the same as <b>Y</b> &gt;&gt;= <b>X</b>.
    *    <b>X</b> &gt;&gt;= (LONG)<b>Y</b> is used for expression
    *    evaluation.</p>
    * <p><b>Y</b> is cast to a LONG and the C++ operation is executed.</p>
    * @param[in] X (LONG&) LHS of 'X &gt;&gt;= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X &gt;&gt;= Y'
    * @return (LONG) result of assignment
    */
   LONG       operator>>=(LONG&  X, const SlipDatum& Y) { return X >>=(LONG  )Y; };                             // X >>= Y

   /**
    * @brief Execute <b>X</b> &amp;= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> &amp;= <b>Y</b> is
    *    not the same as <b>Y</b> &amp;= <b>X</b>.
    *    <b>X</b> &amp;= (LONG)<b>Y</b> is used for expression
    *    evaluation.</p>
    * <p><b>Y</b> is cast to a LONG and the C++ operation is executed.</p>
    * @param[in] X (LONG&) LHS of 'X &amp;= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X &amp;= Y'
    * @return (LONG) result of assignment
    */
   LONG       operator&=(LONG&   X, const SlipDatum& Y) { return X &= (LONG  )Y; };                             // X &= Y

   /**
    * @brief Execute <b>X</b> \|= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b>  \|= <b>Y</b> is
    *    not the same as <b>Y</b>  \|= <b>X</b>.
    *    <b>X</b>  \|= (LONG)<b>Y</b> is used for expression
    *    evaluation.</p>
    * <p><b>Y</b> is cast to a LONG and the C++ operation is executed.</p>
    * @param[in] X (LONG&) LHS of 'X \|= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X \|= Y'
    * @return (CHAR) result of assignment
    */
   LONG       operator|=(LONG&   X, const SlipDatum& Y) { return X |= (LONG  )Y; };                             // X |= Y

   /**
    * @brief Execute <b>X</b> ^= <b>Y</b>.
    * <p>The operation is non-commutative. <b>X</b> ^= <b>Y</b> is
    *    not the same as <b>Y</b> ^= <b>X</b>.
    *    <b>X</b> ^= (LONG)<b>Y</b> is used for expression
    *    evaluation.</p>
    * <p><b>Y</b> is cast to a LONG and the C++ operation is executed.</p>
    * @param[in] X (LONG&) LHS of 'X ^= Y'
    * @param[in] Y (SlipDatum&) RHS of 'X ^= Y'
    * @return (CHAR) result of assignment
    */
   LONG       operator^=(LONG&   X, const SlipDatum& Y) { return X ^= (LONG  )Y; };                             // X ^= Y

       /*************************************************
        *           Logical operators                   *
       *************************************************/

   /**
    * @brief Compare <b>X</b> != (SlipDatum&)<b>Y</b>.
    * <p>The operation is commutative. <b>X</b> != <b>Y</b> is
    *    the same as <b>Y</b> != <b>X</b>. <b>Y</b> != (LONG)<b>X</b>
    *    is used for expression evaluation.</p>
    * <p>The input, (bool)<b>X</b> is cast to a (LONG)<b>X</b>.</p>
    * @param[in] X (bool) 'X != Y'
    * @param[in] Y (SlipDatum&) 'X != Y'
    * @return
    */
   bool       operator!=(const bool   X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) != (LONG)X; };  // Y != V

   /**
    * @brief Compare <b>X</b> &lt; (SlipDatum&)<b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> &lt; <b>Y</b> is
    *    the same as <b>Y</b> &gt; <b>X</b>. <b>Y</b> &gt; (LONG)<b>X</b>
    *    is used for expression evaluation.</p>
    * <p>The input, (bool)<b>X</b> is cast to a (LONG)<b>X</b>.</p>
    * @param[in] X (bool) 'X &lt; Y'
    * @param[in] Y (SlipDatum&) 'X &lt; Y'
    * @return
    */
   bool       operator<(const bool    X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y)  > (LONG)X; };  // Y  > V

   /**
    * @brief Compare <b>X</b> &lt;= (SlipDatum&)<b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> &lt;= <b>Y</b> is
    *    the same as <b>Y</b> &gt;= <b>X</b>. <b>Y</b> &gt;= (LONG)<b>X</b>
    *    is used for expression evaluation.</p>
    * <p>The input, (bool)<b>X</b> is cast to a (LONG)<b>X</b>.</p>
    * @param[in] X (bool) 'X &lt;= Y'
    * @param[in] Y (SlipDatum&) 'X &lt;= Y'
    * @return
    */
   bool       operator<=(const bool   X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) >= (LONG)X; };  // Y >= V

   /**
    * @brief Compare <b>X</b> == (SlipDatum&)<b>Y</b>.
    * <p>The operation is commutative. <b>X</b> == <b>Y</b> is
    *    the same as <b>Y</b> == <b>X</b>. <b>Y</b> == (LONG)<b>X</b>
    *    is used for expression evaluation.</p>
    * <p>The input, (bool)<b>X</b> is cast to a (LONG)<b>X</b>.</p>
    * @param[in] X (bool) 'X == Y'
    * @param[in] Y (SlipDatum&) 'X == Y'
    * @return
    */
   bool       operator==(const bool   X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) == (LONG)X; };  // Y == V

   /**
    * @brief Compare <b>X</b> &gt;= (SlipDatum&)<b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> &gt;= <b>Y</b> is
    *    the same as <b>Y</b> &lt;= <b>X</b>. <b>Y</b> &lt;= (LONG)<b>X</b>
    *    is used for expression evaluation.</p>
    * <p>The input, (bool)<b>X</b> is cast to a (LONG)<b>X</b>.</p>
    * @param[in] X (bool) 'X &gt;= Y'
    * @param[in] Y (SlipDatum&) 'X &gt;= Y'
    * @return
    */
   bool       operator>=(const bool   X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) <= (LONG)X; };  // Y <= V

   /**
    * @brief Compare <b>X</b> &gt; (SlipDatum&)<b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> &gt; <b>Y</b> is
    *    the same as <b>Y</b> &lt; <b>X</b>. <b>Y</b> &lt; (LONG)<b>X</b>
    *    is used for expression evaluation.</p>
    * <p>The input, (bool)<b>X</b> is cast to a (LONG)<b>X</b>.</p>
    * @param[in] X (bool) 'X &gt; Y'
    * @param[in] Y (SlipDatum&) 'X &gt; Y'
    * @return
    */
   bool       operator>(const bool    X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y)  < (LONG)X; };  // Y  < V


   /**
    * @brief Compare <b>X</b> != (SlipDatum&)<b>Y</b>.
    * <p>The operation is commutative. <b>X</b> != <b>Y</b> is
    *    the same as <b>Y</b> != <b>X</b>. <b>Y</b> != <b>X</b>
    *    is used for expression evaluation.</p>
    * @param[in] X (DOUBLE) 'X != Y'
    * @param[in] Y (SlipDatum&) 'X != Y'
    * @return
    */
   bool       operator!=(const DOUBLE X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) != X; };        // Y != V

   /**
    * @brief Compare <b>X</b> &lt; (SlipDatum&)<b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> &lt; <b>Y</b> is
    *    the same as <b>Y</b> &gt; <b>X</b>. <b>Y</b> &gt; <b>X</b>
    *    is used for expression evaluation.</p>
    * @param[in] X (DOUBLE) 'X &lt; Y'
    * @param[in] Y (SlipDatum&) 'X &lt; Y'
    * @return
    */
   bool       operator<(const DOUBLE  X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y)  > X; };        // Y  > V

   /**
    * @brief Compare <b>X</b> &lt;= (SlipDatum&)<b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> &lt;= <b>Y</b> is
    *    the same as <b>Y</b> &gt;= <b>X</b>. <b>Y</b> &gt;= <b>X</b>
    *    is used for expression evaluation.</p>
    * @param[in] X (DOUBLE) 'X &lt;= Y'
    * @param[in] Y (SlipDatum&) 'X &lt;= Y'
    * @return
    */
   bool       operator<=(const DOUBLE X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) >= X; };        // Y >= V

   /**
    * @brief Compare <b>X</b> == (SlipDatum&)<b>Y</b>.
    * <p>The operation is commutative. <b>X</b> == <b>Y</b> is
    *    the same as <b>Y</b> == <b>X</b>. <b>Y</b> == <b>X</b>
    *    is used for expression evaluation.</p>
    * @param[in] X (DOUBLE) 'X == Y'
    * @param[in] Y (SlipDatum&) 'X == Y'
    * @return
    */
   bool       operator==(const DOUBLE X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) == X; };        // Y == V

   /**
    * @brief Compare <b>X</b> &gt;= (SlipDatum&)<b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> &gt;= <b>Y</b> is
    *    the same as <b>Y</b> &lt;= <b>X</b>. <b>Y</b> &lt;= <b>X</b>
    *    is used for expression evaluation.</p>
    * @param[in] X (DOUBLE) 'X &gt;= Y'
    * @param[in] Y (SlipDatum&) 'X &gt;= Y'
    * @return
    */
   bool       operator>=(const DOUBLE X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) <= X; };        // Y <= V

   /**
    * @brief Compare <b>X</b> &gt; (SlipDatum&)<b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> &gt; <b>Y</b> is
    *    the same as <b>Y</b> &lt; <b>X</b>. <b>Y</b> &lt; <b>X</b>
    *    is used for expression evaluation.</p>
    * @param[in] X (DOUBLE) 'X &gt; Y'
    * @param[in] Y (SlipDatum&) 'X &gt; Y'
    * @return
    */
   bool       operator>(const DOUBLE  X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y)  < X; };        // Y  < V


   /**
    * @brief Compare <b>X</b> != (SlipDatum&)<b>Y</b>.
    * <p>The operation is commutative. <b>X</b> != <b>Y</b> is
    *    the same as <b>Y</b> != <b>X</b>. <b>Y</b> != (ULONG)<b>X</b>
    *    is used for expression evaluation.</p>
    * <p>The input, (UCHAR)<b>X</b> is cast to a (ULONG)<b>X</b>.</p>
    * @param[in] X (UCHAR) 'X != Y'
    * @param[in] Y (SlipDatum&) 'X != Y'
    * @return
    */
   bool       operator!=(const UCHAR  X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) != (ULONG)X; }; // Y != V

   /**
    * @brief Compare <b>X</b> &lt; (SlipDatum&)<b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> &lt; <b>Y</b> is
    *    the same as <b>Y</b> &gt; <b>X</b>. <b>Y</b> &gt; (ULONG)<b>X</b>
    *    is used for expression evaluation.</p>
    * <p>The input, (UCHAR)<b>X</b> is cast to a (ULONG)<b>X</b>.</p>
    * @param[in] X (UCHAR) 'X &lt; Y'
    * @param[in] Y (SlipDatum&) 'X &lt; Y'
    * @return
    */
   bool       operator<(const UCHAR   X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y)  > (ULONG)X; }; // Y  > V

   /**
    * @brief Compare <b>X</b> &lt;= (SlipDatum&)<b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> &lt;= <b>Y</b> is
    *    the same as <b>Y</b> &gt;= <b>X</b>. <b>Y</b> &gt;= (ULONG)<b>X</b>
    *    is used for expression evaluation.</p>
    * <p>The input, (UCHAR)<b>X</b> is cast to a (ULONG)<b>X</b>.</p>
    * @param[in] X (UCHAR) 'X &lt;= Y'
    * @param[in] Y (SlipDatum&) 'X &lt;= Y'
    * @return
    */
   bool       operator<=(const UCHAR  X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) >= (ULONG)X; }; // Y >= V

   /**
    * @brief Compare <b>X</b> == (SlipDatum&)<b>Y</b>.
    * <p>The operation is commutative. <b>X</b> == <b>Y</b> is
    *    the same as <b>Y</b> == <b>X</b>. <b>Y</b> == (ULONG)<b>X</b>
    *    is used for expression evaluation.</p>
    * <p>The input, (UCHAR)<b>X</b> is cast to a (ULONG)<b>X</b>.</p>
    * @param[in] X (UCHAR) 'X == Y'
    * @param[in] Y (SlipDatum&) 'X == Y'
    * @return
    */
   bool       operator==(const UCHAR  X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) == (ULONG)X; }; // Y == V

   /**
    * @brief Compare <b>X</b> &gt;= (SlipDatum&)<b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> &gt;= <b>Y</b> is
    *    the same as <b>Y</b> &lt;= <b>X</b>. <b>Y</b> &lt;= (ULONG)<b>X</b>
    *    is used for expression evaluation.</p>
    * <p>The input, (UCHAR)<b>X</b> is cast to a (ULONG)<b>X</b>.</p>
    * @param[in] X (UCHAR) 'X &gt;= Y'
    * @param[in] Y (SlipDatum&) 'X &gt;= Y'
    * @return
    */
   bool       operator>=(const UCHAR  X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) <= (ULONG)X; }; // Y <= V

   /**
    * @brief Compare <b>X</b> &gt; (SlipDatum&)<b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> &gt; <b>Y</b> is
    *    the same as <b>Y</b> &lt; <b>X</b>. <b>Y</b> &lt; (ULONG)<b>X</b>
    *    is used for expression evaluation.</p>
    * <p>The input, (UCHAR)<b>X</b> is cast to a (ULONG)<b>X</b>.</p>
    * @param[in] X (UCHAR) 'X &gt; Y'
    * @param[in] Y (SlipDatum&) 'X &gt; Y'
    * @return
    */
   bool       operator>(const UCHAR   X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y)  < (ULONG)X; }; // Y  < V


   /**
    * @brief Compare <b>X</b> != (SlipDatum&)<b>Y</b>.
    * <p>The operation is commutative. <b>X</b> != <b>Y</b> is
    *    the same as <b>Y</b> != <b>X</b>. <b>Y</b> != (LONG)<b>X</b>
    *    is used for expression evaluation.</p>
    * <p>The input, (CHAR)<b>X</b> is cast to a (LONG)<b>X</b>.</p>
    * @param[in] X (CHAR) 'X != Y'
    * @param[in] Y (SlipDatum&) 'X != Y'
    * @return
    */
   bool       operator!=(const CHAR   X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) != (LONG)X; };  // Y != V

   /**
    * @brief Compare <b>X</b> &lt; (SlipDatum&)<b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> &lt; <b>Y</b> is
    *    the same as <b>Y</b> &gt; <b>X</b>. <b>Y</b> &gt; (LONG)<b>X</b>
    *    is used for expression evaluation.</p>
    * <p>The input, (CHAR)<b>X</b> is cast to a (LONG)<b>X</b>.</p>
    * @param[in] X (CHAR) 'X &lt; Y'
    * @param[in] Y (SlipDatum&) 'X &lt; Y'
    * @return (bool)
    */
   bool       operator<(const CHAR    X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y)  > (LONG)X; };  // Y  > V

   /**
    * @brief Compare <b>X</b> &lt;= (SlipDatum&)<b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> &lt;= <b>Y</b> is
    *    the same as <b>Y</b> &gt;= <b>X</b>. <b>Y</b> &gt;= (LONG)<b>X</b>
    *    is used for expression evaluation.</p>
    * <p>The input, (CHAR)<b>X</b> is cast to a (LONG)<b>X</b>.</p>
    * @param[in] X (CHAR) 'X &lt;= Y'
    * @param[in] Y (SlipDatum&) 'X &lt;= Y'
    * @return (bool)
    */
   bool       operator<=(const CHAR   X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) >= (LONG)X; };  // Y >= V

   /**
    * @brief Compare <b>X</b> == (SlipDatum&)<b>Y</b>.
    * <p>The operation is commutative. <b>X</b> == <b>Y</b> is
    *    the same as <b>Y</b> == <b>X</b>. <b>Y</b> == (LONG)<b>X</b>
    *    is used for expression evaluation.</p>
    * <p>The input, (CHAR)<b>X</b> is cast to a (LONG)<b>X</b>.</p>
    * @param[in] X (CHAR) 'X == Y'
    * @param[in] Y (SlipDatum&) 'X == Y'
    * @return (bool)
    */
   bool       operator==(const CHAR   X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) == (LONG)X; };  // Y == V

   /**
    * @brief Compare <b>X</b> &gt;= (SlipDatum&)<b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> &gt;= <b>Y</b> is
    *    the same as <b>Y</b> &lt;= <b>X</b>. <b>Y</b> &lt;= (LONG)<b>X</b>
    *    is used for expression evaluation.</p>
    * <p>The input, (CHAR)<b>X</b> is cast to a (LONG)<b>X</b>.</p>
    * @param[in] X (CHAR) 'X &gt;= Y'
    * @param[in] Y (SlipDatum&) 'X &gt;= Y'
    * @return (bool)
    */
   bool       operator>=(const CHAR   X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) <= (LONG)X; };  // Y <= V

   /**
    * @brief Compare <b>X</b> &gt; (SlipDatum&)<b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> &gt; <b>Y</b> is
    *    the same as <b>Y</b> &lt; <b>X</b>. <b>Y</b> &lt; (LONG)<b>X</b>
    *    is used for expression evaluation.</p>
    * <p>The input, (CHAR)<b>X</b> is cast to a (LONG)<b>X</b>.</p>
    * @param[in] X (CHAR) 'X &gt; Y'
    * @param[in] Y (SlipDatum&) 'X &gt; Y'
    * @return (bool)
    * @return
    */
   bool       operator>(const CHAR    X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y)  < (LONG)X; };  // Y  < V


   /**
    * @brief Compare <b>X</b> != (SlipDatum&)<b>Y</b>.
    * <p>The operation is commutative. <b>X</b> != <b>Y</b> is
    *    the same as <b>Y</b> != <b>X</b>. <b>Y</b> != <b>X</b>
    *    is used for expression evaluation.</p>
    * @param[in] X (ULONG) 'X != Y'
    * @param[in] Y (SlipDatum&) 'X != Y'
    * @return (bool)
    */
   bool       operator!=(const ULONG  X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) != X; };        // Y != V

   /**
    * @brief Compare <b>X</b> &lt; (SlipDatum&)<b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> &lt; <b>Y</b> is
    *    the same as <b>Y</b> &gt; <b>X</b>. <b>Y</b> &gt; <b>X</b>
    *    is used for expression evaluation.</p>
    * @param[in] X (ULONG) 'X &lt; Y'
    * @param[in] Y (SlipDatum&) 'X &lt; Y'
    * @return (bool)
    */
   bool       operator<(const ULONG   X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y)  > X; };        // Y  > V

   /**
    * @brief Compare <b>X</b> &lt;= (SlipDatum&)<b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> &lt;= <b>Y</b> is
    *    the same as <b>Y</b> &gt;= <b>X</b>. <b>Y</b> &gt;= <b>X</b>
    *    is used for expression evaluation.</p>
    * @param[in] X (ULONG) 'X &lt;= Y'
    * @param[in] Y (SlipDatum&) 'X &lt;= Y'
    * @return (bool)
    */
   bool       operator<=(const ULONG  X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) >= X; };        // Y >= V

   /**
    * @brief Compare <b>X</b> == (SlipDatum&)<b>Y</b>.
    * <p>The operation is commutative. <b>X</b> == <b>Y</b> is
    *    the same as <b>Y</b> == <b>X</b>. <b>Y</b> == <b>X</b>
    *    is used for expression evaluation.</p>
    * @param[in] X (ULONG) 'X == Y'
    * @param[in] Y (SlipDatum&) 'X == Y'
    * @return (bool)
    */
   bool       operator==(const ULONG  X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) == X; };        // Y == V

   /**
    * @brief Compare <b>X</b> &gt;= (SlipDatum&)<b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> &gt;= <b>Y</b> is
    *    the same as <b>Y</b> &lt;= <b>X</b>. <b>Y</b> &lt;= <b>X</b>
    *    is used for expression evaluation.</p>
    * @param[in] X (ULONG) 'X &gt;= Y'
    * @param[in] Y (SlipDatum&) 'X &gt;= Y'
    * @return (bool)
    */
   bool       operator>=(const ULONG  X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) <= X; };        // Y <= V

   /**
    * @brief Compare <b>X</b> &gt; (SlipDatum&)<b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> &gt; <b>Y</b> is
    *    the same as <b>Y</b> &lt; <b>X</b>. <b>Y</b> &lt; <b>X</b>
    *    is used for expression evaluation.</p>
    * @param[in] X (ULONG) 'X &gt; Y'
    * @param[in] Y (SlipDatum&) 'X &gt; Y'
    * @return (bool)
    */
   bool       operator>(const ULONG   X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y)  < X; };        // Y  < V


   /**
    * @brief Compare <b>X</b> != (SlipDatum&)<b>Y</b>.
    * <p>The operation is commutative. <b>X</b> != <b>Y</b> is
    *    the same as <b>Y</b> != <b>X</b>. <b>Y</b> != <b>X</b>
    *    is used for expression evaluation.</p>
    * @param[in] X (LONG) 'X != Y'
    * @param[in] Y (SlipDatum&) 'X != Y'
    * @return (bool)
    */
   bool       operator!=(const LONG   X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) != X; };        // Y != V

   /**
    * @brief Compare <b>X</b> &lt; (SlipDatum&)<b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> &lt; <b>Y</b> is
    *    the same as <b>Y</b> &gt; <b>X</b>. <b>Y</b> &gt; <b>X</b>
    *    is used for expression evaluation.</p>
    * @param[in] X (LONG) 'X &lt; Y'
    * @param[in] Y (SlipDatum&) 'X &lt; Y'
    * @return (bool)
    */
   bool       operator<(const LONG    X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y)  > X; };        // Y  > V

   /**
    * @brief Compare <b>X</b> &lt;= (SlipDatum&)<b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> &lt;= <b>Y</b> is
    *    the same as <b>Y</b> &gt;= <b>X</b>. <b>Y</b> &gt;= <b>X</b>
    *    is used for expression evaluation.</p>
    * @param[in] X (LONG) 'X &lt;= Y'
    * @param[in] Y (SlipDatum&) 'X &lt;= Y'
    * @return (bool)
    */
   bool       operator<=(const LONG   X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) >= X; };        // Y >= V

   /**
    * @brief Compare <b>X</b> == (SlipDatum&)<b>Y</b>.
    * <p>The operation is commutative. <b>X</b> == <b>Y</b> is
    *    the same as <b>Y</b> == <b>X</b>. <b>Y</b> == <b>X</b>
    *    is used for expression evaluation.</p>
    * @param[in] X (LONG) 'X == Y'
    * @param[in] Y (SlipDatum&) 'X == Y'
    * @return (bool)
    */
   bool       operator==(const LONG   X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) == X; };        // Y <= V

   /**
    * @brief Compare <b>X</b> &gt;= (SlipDatum&)<b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> &gt;= <b>Y</b> is
    *    the same as <b>Y</b> &lt;= <b>X</b>. <b>Y</b> &lt;= <b>X</b>
    *    is used for expression evaluation.</p>
    * @param[in] X (LONG) 'X &gt;= Y'
    * @param[in] Y (SlipDatum&) 'X &gt;= Y'
    * @return (bool)
    */
   bool       operator>=(const LONG   X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) <= X; };        // Y <= V

   /**
    * @brief Compare <b>X</b> &gt; (SlipDatum&)<b>Y</b>.
    * <p>The operation is anti-commutative. <b>X</b> &gt; <b>Y</b> is
    *    the same as <b>Y</b> &lt; <b>X</b>. <b>Y</b> &lt; <b>X</b>
    *    is used for expression evaluation.</p>
    * @param[in] X (LONG) 'X gt; Y'
    * @param[in] Y (SlipDatum&) 'X &gt; Y'
    * @return (bool)
    */
   bool       operator>(const LONG    X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y)  < X; };        // Y  < V


   /**
    * @brief Compare <b>X</b> != (SlipDatum&)<b>Y</b>.
    * <p>The operation is commutative. <b>X</b> != <b>Y</b> is
    *    the same as <b>Y</b> != <b>X</b>. <b>Y</b> != <b>X</b>
    *    is used for expression evaluation.</p>
    * <p>This operation is illegal for SlipDatum objects which are not
    *    of type PTR.</p>
    * @param[in] X (PTR) 'X != Y'
    * @param[in] Y (SlipDatum&) 'X != Y'
    * @return (bool)
    */
   bool       operator!=(const PTR    X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) != X; };        // Y != V

   /**
    * @brief Compare <b>X</b> == (SlipDatum&)<b>Y</b>.
    * <p>The operation is commutative. <b>X</b> == <b>Y</b> is
    *    the same as <b>Y</b> == <b>X</b>. <b>Y</b> == <b>X</b>
    *    is used for expression evaluation.</p>
    * <p>This operation is illegal for SlipDatum objects which are not
    *    of type PTR.</p>
    * @param[in] X (PTR) 'X == Y'
    * @param[in] Y (SlipDatum&) 'X == Y'
    * @return (bool)
    */
   bool       operator==(const PTR    X, const SlipDatum& Y) { return const_cast<SlipDatum&>(Y) == X; };        // Y == V


   /**
    * @brief Compare <b>X</b> != (SlipDatum&)<b>Y</b>.
    * <p>The operation is commutative. <b>X</b> != <b>Y</b> is
    *    the same as <b>Y</b> != <b>X</b>. <b>Y</b> != <b>X</b>
    *    is used for expression evaluation.</p>
    * <p>This operation is illegal for SlipDatum objects which are not
    *    of type string.</p>
    * @param[in] X (string&) 'X != Y'
    * @param[in] Y (SlipDatum&) 'X != Y'
    * @return (bool)
    */
   bool       operator!=(const string& X, const SlipDatum& Y){ return const_cast<SlipDatum&>(Y) != X; };        // Y != V

   /**
    * @brief Compare <b>X</b> == (SlipDatum&)<b>Y</b>.
    * <p>The operation is commutative. <b>X</b> == <b>Y</b> is
    *    the same as <b>Y</b> == <b>X</b>. <b>Y</b> == <b>X</b>
    *    is used for expression evaluation.</p>
    * <p>This operation is illegal for SlipDatum objects which are not
    *    of type string.</p>
    * @param[in] X (string&) 'X == Y'
    * @param[in] Y (SlipDatum&) 'X == Y'
    * @return (bool)
    */
   bool       operator==(const string& X, const SlipDatum& Y){ return const_cast<SlipDatum&>(Y) == X; };        // Y == V
   

   void avslHistory(bool onOFF) { SlipCellBase::avslHistory(onOFF); }             // true turns history on
   SlipState getSlipState() { return SlipCellBase::getSlipState() ;};             // return the internal Slip State
   void printAVSL(string str) { SlipCellBase::printAVSL(str); }                   // Output the AVSL junk
   void printClassSizes() { SlipCellBase::printClassSizes(); }                    // Output the size of each class
   void printFragmentList(string str) { SlipCellBase::printFragmentList(str); }   // format and output the fragment list
   void printMemory(string str) { SlipCellBase::printMemory(str); }               // Output entire memory contents
   void printState(string str) { SlipCellBase::printState(str); }                 // Output the AVSL state
   errorType  setErrorState(errorType state, string err) { return slip::SlipErr::setErrorState(state, err); }       // Set error message state
   void sysInfo(ostream& out) { SlipCellBase::sysInfo(out); }                     // Output the SLIP system info
   void slipInit() { SlipCellBase::slipInit(); }                                  // Initialize SLIP system
   void slipInit(ULONG alloc, ULONG lta) { SlipCellBase::slipInit(alloc, lta); }  // Initialize SLIP system and allocation resources
   errorCallback setCallBack(errorCallback callBack) { return userCallBack(callBack); }  // Set user defined error callback function.s

}; // namespace slip
