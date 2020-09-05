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
 * @file    SlipDatum.cpp
 * @author  A. Schwarz
 * @date    June 04, const 2012
 *
 * @brief Contains the SlipDatum definitions.
 *
 */

# include <iostream>
# include <iomanip>
# include "SlipCellBase.h"
# include "SlipCell.h"
# include "SlipOp.h"
# include "SlipDef.h"
# include "SlipErr.h"
# include "SlipGlobal.h"
# include "SlipDatum.h"
# include "SlipHeader.h"
# include "SlipStringConst.h"
# include "SlipStringNonConst.h"
# include "SlipSublist.h"

namespace slip {

    /*********************************************************
     *                   Private Functions                   *
    **********************************************************/
   /**
    * @brief Construct a SlipDatum object and perform required checks.
    * <p>Check to see if object is on the AVSL and if not, post the
    *    invariant property object pointer.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E3013</b> Invalid to create a Datum cell on the stack. Must use new. </li>
    * </ul>
    * @param[in] op (SlipOp*) pointer to invariant property object
    */
   void SlipDatum::constructDatumCell(void* op) {
      if ((getLeftLink() == getRightLink()) &&
         (getLeftLink() == (SlipCellBase*)UNDEFDATA)) {
         postError(__FILE__, __LINE__, SlipErr::E3013, "setDatumCell", "", "", (SlipDatum&)*this);
      } else {
         setOperator(op);
      }
   }; // void SlipDatum::setDatumCell(void* op)

    /*********************************************************
     *                  Protected Functions                  *
    **********************************************************/

   /**
    * @fn SlipDatum::SlipDatum(const SlipDatum* X)
    * @brief Copy constructor for a SlipDatum object with an initial SlipDatum& value
    * <p>A new SlipDatum cell is created from the available space list (AVLS).
    *    The initial data type and datum value is defined by the input SlipDatum
    *    reference. All operations legal for the data type in the input reference
    *    are legal for the new object.</p>
    * <p>Pointer use is strongly discouraged, hence, protected.</p>
    * @param[in] X (SlipDatum*) defines the initial data type and datum value
    */

   /**
    * @fn SlipDatum::copy() const
    * @brief Create a copy of the current object and return a reference to it.
    * @return reference to a copy of the current SlipDatum object
    */
//   SlipDatum& SlipDatum::copy() const { return ((SlipOp*)*getOperator())->copy(*this); } // Return a reference to a copy of the current cell


   /*********************************************************
    *                   Public Functions                    *
   **********************************************************/

       /*************************************************
        *           Constructors & Destructors          *
        ************************************************/
   /**
    * @fn SlipDatum::SlipDatum(const SlipDatum& X)
    * @brief Copy constructor for a SlipDatum object with an initial SlipDatum& value
    * <p>A new SlipDatum cell is created from the available space list (AVLS).
    *    The initial data type and datum value is defined by the input SlipDatum
    *    reference. All operations legal for the data type in the input reference
    *    are legal for the new object.</p>
    * <p></p>
    * @param[in] X (SlipDatum&) defines the initial data type and datum value
    */

   /**
    * @fn SlipDatum::SlipDatum(DOUBLE X)
    * @brief Constructor for a SlipDatum object with an initial DOUBLE value
    * <p>A new SlipDatum cell is created from the available space list (AVLS).
    *    The data type is DOUBLE and the datum value is X. All operations legal
    *    for a DOUBLE are legal.</p>
    *
    * @param[in] X (DOUBLE)
    */

   /**
    * @fn SlipDatum::SlipDatum(bool X)
    * @brief Constructor for a SlipDatum object with an initial bool value
    * <p>A new SlipDatum cell is created from the available space list (AVLS).
    *    The data type is bool and the datum value is X. All operations legal
    *    for a bool are legal.</p>
    *
    * @param[in] X (bool)
    */

   /**
    * @fn SlipDatum::SlipDatum(UCHAR X)
    * @brief Constructor for a SlipDatum object with an initial UCHAR value
    * <p>A new SlipDatum cell is created from the available space list (AVLS).
    *    The data type is UCHAR and the datum value is X. All operations legal
    *    for a UCHAR are legal.</p>
    *
    * @param[in] X (UCHAR)
    */

   /**
    * @fn SlipDatum::SlipDatum(CHAR X)
    * @brief Constructor for a SlipDatum object with an initial CHAR value
    * <p>A new SlipDatum cell is created from the available space list (AVLS).
    *    The data type is CHAR and the datum value is X. All operations legal
    *    for a CHAR are legal.</p>
    *
    * @param[in] X (CHAR)
    */

   /**
    * @fn SlipDatum::SlipDatum(ULONG X)
    * @brief Constructor for a SlipDatum object with an initial ULONG value
    * <p>A new SlipDatum cell is created from the available space list (AVLS).
    *    The data type is ULONG and the datum value is X. All operations legal
    *    for a ULONG are legal.</p>
    *
    * @param[in] X (ULONG)
    */

   /**
    * @fn SlipDatum::SlipDatum(LONG X)
    * @brief Constructor for a SlipDatum object with an initial LONG value
    * <p>A new SlipDatum cell is created from the available space list (AVLS).
    *    The data type is LONG and the datum value is X. All operations legal
    *    for a LONG are legal.</p>
    *
    * @param[in] X (LONG)
    */

   /**
    * @fn SlipDatum::SlipDatum(const PTR X, const void* operation)
    * @brief Constructor for a SlipDatum object with an initial PTR value
    * <p>A new SlipDatum cell is created from the available space list (AVLS).
    *   The data type is PTR and the datum value is X. All operations legal for
    *   a PTR are legal.</p>
    * <p>The value of the input X is the reference of the PTR object. The
    *    reference is copied. The PTR and the object pointed to are not copied
    *    and a pointer to a copy is not made.</p>
    * <p>If multiple SlipDatum objects are created using the same PTR pointer
    *    then multiple pointers to the same object will exist. Any change made
    *    to the object will be reflected in all SlipDatum object containing a
    *    pointer to the object.</p>
    * <p>The operations field provides a means to specify user defined operations.
    *    The pointer must be to a SlipOp defined class. The standard casting,
    *    unary, arithmetic, bitwise and logical operations must all be overridden.
    *    The result is that a user Defined object is treated as a atomic value
    *    in all computations.</p>
    * @param[in] X (PTR) object to be inserted
    * @param[in] operation (void*) SlipOp derived User Defined operations
    */
   SlipDatum::SlipDatum(const PTR X, const void* operation) {
      if (operation == undefinedOP) {
         setLeftLink((SlipCellBase*)UNDEFDATA);
         setRightLink((SlipCellBase*)UNDEFDATA);
      } else {
         constructDatumCell((void*)operation);
      }
      *getPtr() = X;
   }; // SlipDatum::SlipDatum(PTR X)

   /**
    * @fn SlipDatum::SlipDatum(const string& X, bool constFlag)
    * @brief Constructor for a SlipDatum object with an initial string& value
    * <p>A new SlipDatum cell is created from the available space list (AVLS).
    *    The data type is string& and the datum value is X. All operations legal
    *    for a  string are legal.</p>
    * <p>The value of the input X is the reference to the string object. The
    *    reference is copied. The string, itself, is not copied and a pointer to
    *    the new copy is not made.</p>
    * <p>If multiple SlipDatum objects are created using the same STRING reference
    *    then multiple pointers to the same string will exist. Any change made to
    *    the string will be reflected in all SlipDatum object containing a pointer
    *    to the string.</p?
    * @param[in] X (string&) input data type and initial data value
    * @param[in] constFlag <b>true</b> a constant string is input
    */
   SlipDatum::SlipDatum(const string& X, bool constFlag) {
      STRING ptr;
      constructDatumCell((void*)stringOP);
      if (constFlag)
         ptr = new SlipStringConst(&X);
      else
         ptr = new SlipStringNonConst(&X);
      *getString() = ptr;
   }; // SlipDatum::SlipDatum(string& X, bool constFlag)

   /**
    * @ fn SlipDatum::SlipDatum(const string* X, bool constFlag)
    * @brief Constructor for a SlipDatum object with an initial string* value
    * <p>A new SlipDatum cell is created from the available space list (AVLS).
    *    The data type is string* and the datum value is X. All operations legal
    *    for a  string are legal.</p>
    * <p>The value of the input X is the reference to the string object. The
    *    reference is copied. The string, itself, is not copied and a pointer to
    *    the new copy is not made.</p>
    * <p>If multiple SlipDatum objects are created using the same STRING reference
    *    then multiple pointers to the same string will exist. Any change made to
    *    the string will be reflected in all SlipDatum object containing a pointer
    *    to the string.</p?
    * @param[in] X (string*) input data type and initial data value
    * @param[in] constFlag <b>true</b> a constant string is input
    */
   SlipDatum::SlipDatum(const string* X, bool constFlag) {
      STRING ptr;
      constructDatumCell((void*)stringOP);
      if (constFlag)
         ptr = new SlipStringConst(X);
      else
          ptr = new SlipStringNonConst(X);
       *getString() = ptr;
   }; // SlipDatum::SlipDatum(string* X, bool constFlag)

   /**
    * @fn SlipDatum::~SlipDatum()
    * @brief Return SlipDatum object to the AVSL.
    * <p>Destructor returns data to the AVSL.</p>
    * <p>If the cell is linked into a list, a silent <em>unlink</em> removes
    *    the cell from the existing link. If the cell is not a valid candidate
    *    for AVSL insertion, then the request is silently ignored./p>
    * <p>Invalid AVSL objects are those already deleted, those never from the
    *    AVLS and those on the runtime stack.</p>
    */
   SlipDatum::~SlipDatum() {
      if (!isDeleted() && !isTemp() && isAVSL(this)) {
         if (getLeftLink() != NULL)  unLink();
         addAVSLCells( this, this);
      }
   }; // SlipDatum::~SlipDatum()

   /*********************************************************
    *                  Protected Methods                    *
   *********************************************************/
   
   /**
    * @fn SlipDatum::assignPreamble()
    * @brief Delete the contents of a SliDatum cell.
    * <p>A common set of operations are included here. The objective of this
    *    method is to ensure that cell reseting is valid and then to delete
    *    the contents of the cell and assign undefined invariant properties
    *    to it.</p>
    * <p>A call to this method must be followed by assignment of value to
    *    the operator and datum fields. A failure to do so will cause the
    *    heavens to part and demons to rise. SLIP will be corrupt.</p>
    * <p>If this cell has already been deleted (is on the AVSL) then it can't
    *    be deleted. Otherwise, remove (delete) SlipPointer derived objects.
    *    Note that the <em>remove</em> method is a property invariant. All
    *    Slip classes which have SlipOp derived objects have this method.</p>
    * <p>Before return, ensure that the <b>operator</b> field points to the
    *    undefined invariant property object and reset the <b>datum</b> field.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    * </ul>
    * @return <b>true</b>  operation successful
    */
   bool SlipDatum::assignPreamble() {                                           // Delete before change
      bool flag = isDeleted();
      if (flag) {                                                               // Is the current cell deleted
         postError(__FILE__, __LINE__, SlipErr::E3019, "SlipDatum::assignPreamble", "", "", *this);
      }  else ((SlipOp*)*getOperator())->remove(*this);                         // Remove SlipPointer datum {PTR STRING}
      resetData();
      return flag;
   }; // void SlipDatum::assignPreamble()

   /**
    * @fn SlipDatum::assignEpilogue(const SlipDatum& X)
    * @brief Set the type and datum field of the current object from an input object.
    * <p>The <em>type</em> field is represented by the invariant object pointer
    *    set in the <b>operator</b> field. After checking that the input (RHS)
    *    object is valid, the current (LHS) object is changed so that the type
    *    and value are the same as the RHS. </p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    * </ul>
    * @param[in] X (SlipDatum&) reference to the RHS exemplar object
    * @return a reference to the current object
    */
   SlipDatum& SlipDatum::assignEpilogue(const SlipDatum& X) {                   // Replace data in old cell
      if (!isDeleted()) {                                                       // If the lhs is deleted, do nothing
         if (X.isDeleted()) {                                                   // If the rhs is deleted, do nothing
            postError(__FILE__, __LINE__, SlipErr::E3019, "SlipDatum::assignEpilogue", "", "", *this, X);
         } else {                                                               // Perform assignment
            setOperator(*getOperator(&X));                                      // Operator same type as input
            *getData() = getDatumCopy(&X);                                      // Assume data copy is legal
         }
      }
      return *this;
   }; // SlipDatum& SlipDatum::assignEpilogue(const SlipCell& ptr)

   /*********************************************************
    *                  Casting Operators                    *
   *********************************************************/
   
   /**
    * @fn SlipDatum::operator bool() const
    * @brief Cast the SlipDatum datum to a bool.
    * <p>
    * The casts supported are given in the table below. Casts from the current
    * data type to another data type are given by reading from the row containing
    * the current data type to the column containing the desired type. If the cell
    * at the intersection of the row and column is not an error then the cast will
    * be successful.</p>
    * <p>
    * A cast to or from a <b>STRING</b> or a <b>PTR</b> to any other type is not
    * supported.</p>
    * <p>
    * When casting to or from a <b>bool</b>, the following rules apply:</p>
    * <ul>
    *   <li>When casting to a <b>bool</b>, a zero (0) is false, anything
    *       else is true.</li>
    *   <li>When cast from a <b>bool</b> to another type, false is zero (0),
    *       true is one (1).</li>
    * </ul>
    * <p>
    * Retrieving the value contained in a cell datum is done by casting the
    * cell to the data type in the datum field. For example, if the datum
    * type is <b>bool</b> then:<br></p>
    * <pre>
    *   <b>(bool) Y</b>
    * </pre>
    * <p>
    * where Y is a SlipDatum cell will yield the unconverted value.</p>
    * <p></p>
    * <center>
    * <table>
    * <caption>Supported Casts (bool) Y</caption>
    *   <tr>
    *     <th>from/to</th>
    *     <th>bool</th>
    *     <th>UCHAR</th>
    *     <th>CHAR</th>
    *     <th>ULONG</th>
    *     <th>LONG</th>
    *     <th>DOUBLE</th>
    *   </tr>
    *   <tr>
    *     <td>bool</td>
    *     <td>bool</td>
    *     <td>UCHAR</td>
    *     <td>CHAR</td>
    *     <td>ULONG</td>
    *     <td>LONG</td>
    *     <td>DOUBLE</td>
    *   </tr>
    *   <tr>
    *     <td>UCHAR</td>
    *     <td>bool</td>
    *     <td>UCHAR</td>
    *     <td>CHAR</td>
    *     <td>ULONG</td>
    *     <td>LONG</td>
    *     <td>DOUBLE</td>
    *   </tr>
    *   <tr>
    *     <td>CHAR</td>
    *     <td>bool</td>
    *     <td>UCHAR</td>
    *     <td>CHAR</td>
    *     <td>ULONG</td>
    *     <td>LONG</td>
    *     <td>DOUBLE</td>
    *   </tr>
    *   <tr>
    *     <td>ULONG</td>
    *     <td>bool</td>
    *     <td>UCHAR</td>
    *     <td>CHAR</td>
    *     <td>ULONG</td>
    *     <td>LONG</td>
    *     <td>DOUBLE</td>
    *   </tr>
    *   <tr>
    *     <td>LONG</td>
    *     <td>bool</td>
    *     <td>UCHAR</td>
    *     <td>CHAR</td>
    *     <td>ULONG</td>
    *     <td>LONG</td>
    *     <td>DOUBLE</td>
    *   </tr>
    *   <tr>
    *     <td>DOUBLE</td>
    *     <td>bool</td>
    *     <td>UCHAR</td>
    *     <td>CHAR</td>
    *     <td>ULONG</td>
    *     <td>LONG</td>
    *     <td>DOUBLE</td>
    *   </tr>
    * </table>
    * </center>
    *
    * @return  SlipDatum value cast to a bool
   */

   /**
    * @fn SlipDatum::operator UCHAR() const
    * @brief Cast the SlipDatum value to a UCHAR.</p>
    * <p>
    * The casts supported are given in the table below. Casts from the current
    * data type to another data type are given by reading from the row containing
    * the current data type to the column containing the desired type. If the cell
    * at the intersection of the row and column is not an error then the cast will
    * be successful.</p>
    * <p>
    * A cast to or from a <b>STRING</b>s or a <b>PTR</b> to any other type is
    * not supported.</p>
    * <p>
    * When casting to or from a <b>bool</b>, the following rules apply></p>
    * <ul>
    *    <li>When casting to a <b>bool</b>, a zero (0) is false, anything
    *        else is true.
    *    <li>When cast from a <b>bool</b> to another type, false is zero (0),
    *      true is one (1).
    * </ul>
    * <p>
    * Retrieving the value contained in a cell datum is done by casting the
    * cell to the data type of the datum field. For example, if the value
    * type is <b>UCHAR</b> then:<br></p>
    * <pre>
    *   <b>(UCHAR) Y</b>
    * </pre>
    * <p>
    * where Y is a SlipDatum cell will yield the unconverted value.</p>
    * <p></p>
    * <center>
    * <table>
    * <caption>Supported Casts (UCHAR) Y</caption>
   *   <tr>
   *     <th>from/to</th>
   *     <th>bool</th>
   *     <th>UCHAR</th>
   *     <th>CHAR</th>
   *     <th>ULONG</th>
   *     <th>LONG</th>
   *     <th>DOUBLE</th>
   *   </tr>
   *   <tr>
   *     <td>bool</td>
   *     <td>bool</td>
   *     <td>UCHAR</td>
   *     <td>CHAR</td>
   *     <td>ULONG</td>
   *     <td>LONG</td>
   *     <td>DOUBLE</td>
   *   </tr>
   *   <tr>
   *     <td>UCHAR</td>
   *     <td>bool</td>
   *     <td>UCHAR</td>
   *     <td>CHAR</td>
   *     <td>ULONG</td>
   *     <td>LONG</td>
   *     <td>DOUBLE</td>
   *   </tr>
   *   <tr>
   *     <td>CHAR</td>
   *     <td>bool</td>
   *     <td>UCHAR</td>
   *     <td>CHAR</td>
   *     <td>ULONG</td>
   *     <td>LONG</td>
   *     <td>DOUBLE</td>
   *   </tr>
   *   <tr>
   *     <td>ULONG</td>
   *     <td>bool</td>
   *     <td>UCHAR</td>
   *     <td>CHAR</td>
   *     <td>ULONG</td>
   *     <td>LONG</td>
   *     <td>DOUBLE</td>
   *   </tr>
   *   <tr>
   *     <td>LONG</td>
   *     <td>bool</td>
   *     <td>UCHAR</td>
   *     <td>CHAR</td>
   *     <td>ULONG</td>
   *     <td>LONG</td>
   *     <td>DOUBLE</td>
   *   </tr>
   *   <tr>
   *     <td>DOUBLE</td>
   *     <td>bool</td>
   *     <td>UCHAR</td>
   *     <td>CHAR</td>
   *     <td>ULONG</td>
   *     <td>LONG</td>
   *     <td>DOUBLE</td>
   *   </tr>
    * </table>
    * </center>
    *
    * @return SlipDatum value cast to a UCHAR
    */

   /**
    * @fn SlipDatum::operator CHAR() const
    * @brief Cast the SlipDatum value to a CHAR.
    * <p>
    * The casts supported are given in the table below. Casts from the current
    * data type to another data type are given by reading from the row containing
    * the current data type to the column containing the desired type. If the cell
    * at the intersection of the row and column is not an error then the cast will
    * be successful.</p>
    * <p>
    * A cast to or from a <b>STRING</b>s or a <b>PTR</b> to any other type is
    * not supported.</p>
    * <p>
    * When casting to or from a <b>bool</b>, the following rules apply:</p>
    * <ul>
    *    <li>When casting to a <b>bool</b>, a zero (0) is false, anything
    *       else is true.
    *   <li>When cast from a <b>bool</b> to another type, false is zero (0),
    *       true is one (1).
    * </ul>
    * <p>
    * Retrieving the value contained in a cell datum is done by casting the
    * cell to the data type in the datum field. For example, if the datum
    * type is <b>CHAR</b> then:<br></p>
    * <pre>
    *   <b>(CHAR) Y</b>
    * </pre>
    * <p>
    * where Y is a SlipDatum cell will yield the unconverted value.</p>
    * <p></p>
    * <center>
    * <table>
    * <caption>Supported Casts (CHAR) Y</caption>
   *   <tr>
   *     <th>from/to</th>
   *     <th>bool</th>
   *     <th>UCHAR</th>
   *     <th>CHAR</th>
   *     <th>ULONG</th>
   *     <th>LONG</th>
   *     <th>DOUBLE</th>
   *   </tr>
   *   <tr>
   *     <td>bool</td>
   *     <td>bool</td>
   *     <td>UCHAR</td>
   *     <td>CHAR</td>
   *     <td>ULONG</td>
   *     <td>LONG</td>
   *     <td>DOUBLE</td>
   *   </tr>
   *   <tr>
   *     <td>UCHAR</td>
   *     <td>bool</td>
   *     <td>UCHAR</td>
   *     <td>CHAR</td>
   *     <td>ULONG</td>
   *     <td>LONG</td>
   *     <td>DOUBLE</td>
   *   </tr>
   *   <tr>
   *     <td>CHAR</td>
   *     <td>bool</td>
   *     <td>UCHAR</td>
   *     <td>CHAR</td>
   *     <td>ULONG</td>
   *     <td>LONG</td>
   *     <td>DOUBLE</td>
   *   </tr>
   *   <tr>
   *     <td>ULONG</td>
   *     <td>bool</td>
   *     <td>UCHAR</td>
   *     <td>CHAR</td>
   *     <td>ULONG</td>
   *     <td>LONG</td>
   *     <td>DOUBLE</td>
   *   </tr>
   *   <tr>
   *     <td>LONG</td>
   *     <td>bool</td>
   *     <td>UCHAR</td>
   *     <td>CHAR</td>
   *     <td>ULONG</td>
   *     <td>LONG</td>
   *     <td>DOUBLE</td>
   *   </tr>
   *   <tr>
   *     <td>DOUBLE</td>
   *     <td>bool</td>
   *     <td>UCHAR</td>
   *     <td>CHAR</td>
   *     <td>ULONG</td>
   *     <td>LONG</td>
   *     <td>DOUBLE</td>
   *   </tr>
    * </table>
    * </center>
    *
    * @return SlipDatum value cast to a CHAR
    *
    */

   /**
    * @fn SlipDatum::operator ULONG() const
    * @brief Cast the SlipDatum value to a ULONG.
    * <p>
    * The casts supported are given in the table below. Casts from the current
    * data type to another data type are given by reading from the row containing
    * the current data type to the column containing the desired type. If the cell
    * at the intersection of the row and column is not an error then the cast will
    * be successful.</p>
    * <p>
    * A cast to or from a <b>STRING</b> or a <b>PTR</b> to any other type is
    * not supported.</p>
    * <p>
    * When casting to or from a <b>bool</b>, the following rules apply:</p>
    * <ul>
    *    <li>When casting to a <b>bool</b>, a zero (0) is false, anything
    *        else is true.
    *    <li>When cast from a <b>bool</b> to another type, false is zero (0),
    *        true is one (1).
    * </ul>
    * <p>
    * Retrieving the value contained in a cell datum is done by casting the
    * cell to the data type in the datum field. For example, if the datum
    * type is <b>ULONG</b> then:</p>
    * <pre>
    *   <b>(ULONG) Y</b>
    * </pre>
    * <p>
    * where Y is a SlipDatum cell will yield the unconverted value.</p>
    * <p></p>
    * <center>
    * <table>
    * <caption>Supported Casts (ULONG) Y</caption>
    *   <tr>
    *     <th>from/to</th>
    *     <th>bool</th>
    *     <th>UCHAR</th>
    *     <th>CHAR</th>
    *     <th>ULONG</th>
    *     <th>LONG</th>
    *     <th>DOUBLE</th>
    *   </tr>
    *   <tr>
    *     <td>bool</td>
    *     <td>bool</td>
    *     <td>UCHAR</td>
    *     <td>CHAR</td>
    *     <td>ULONG</td>
    *     <td>LONG</td>
    *     <td>DOUBLE</td>
    *   </tr>
    *   <tr>
    *     <td>UCHAR</td>
    *     <td>bool</td>
    *     <td>UCHAR</td>
    *     <td>CHAR</td>
    *     <td>ULONG</td>
    *     <td>LONG</td>
    *     <td>DOUBLE</td>
    *   </tr>
    *   <tr>
    *     <td>CHAR</td>
    *     <td>bool</td>
    *     <td>UCHAR</td>
    *     <td>CHAR</td>
    *     <td>ULONG</td>
    *     <td>LONG</td>
    *     <td>DOUBLE</td>
    *   </tr>
    *   <tr>
    *     <td>ULONG</td>
    *     <td>bool</td>
    *     <td>UCHAR</td>
    *     <td>CHAR</td>
    *     <td>ULONG</td>
    *     <td>LONG</td>
    *     <td>DOUBLE</td>
    *   </tr>
    *   <tr>
    *     <td>LONG</td>
    *     <td>bool</td>
    *     <td>UCHAR</td>
    *     <td>CHAR</td>
    *     <td>ULONG</td>
    *     <td>LONG</td>
    *     <td>DOUBLE</td>
    *   </tr>
    *   <tr>
    *     <td>DOUBLE</td>
    *     <td>bool</td>
    *     <td>UCHAR</td>
    *     <td>CHAR</td>
    *     <td>ULONG</td>
    *     <td>LONG</td>
    *     <td>DOUBLE</td>
    *   </tr>
    * </table>
    * </center>
    *
    * @return SlipDatum value cast to a ULONG
    *
    */

   /**
    * @fn SlipDatum::operator LONG() const
    * @brief Cast the SlipDatum value to a LONG.
    * <p>
    * The casts supported are given in the table below. Casts from the current
    * data type to another data type are given by reading from the row containing
    * the current data type to the column containing the desired type. If the cell
    * at the intersection of the row and column is not an error then the cast will
    * be successful.</p>
    * <p>
    * A cast to or from a <b>STRING</b> or a <b>PTR</b> to any other type is
    * not supported.</p>
    * <p>
    * When casting to or from a <b>bool</b>, the following rules apply>:</p?
    * <ul>
    *    <li>When casting to a <b>bool</b>, a zero (0) is false, anything
    *        else is true.
    *    <li>When cast from a <b>bool</b> to another type, false is zero (0),
    *        true is one (1).
    * </ul>
    * <p>
    * Retrieving the value contained in a cell datum is done by casting the
    * cell to the data type in the datum field. For example, if the datum
    * type is <b>LONG</b> then:</p>
    * <pre>
    *   <b>(LONG) Y</b>
    * </pre>
    * <p>
    * where Y is a SlipDatum cell will yield the unconverted value.</p>
    * <p></p>
    * <center>
    * <table>
    * <caption>Supported Casts (LONG) Y</caption>
   *   <tr>
   *     <th>from/to</th>
   *     <th>bool</th>
   *     <th>UCHAR</th>
   *     <th>CHAR</th>
   *     <th>ULONG</th>
   *     <th>LONG</th>
   *     <th>DOUBLE</th>
   *   </tr>
   *   <tr>
   *     <td>bool</td>
   *     <td>bool</td>
   *     <td>UCHAR</td>
   *     <td>CHAR</td>
   *     <td>ULONG</td>
   *     <td>LONG</td>
   *     <td>DOUBLE</td>
   *   </tr>
   *   <tr>
   *     <td>UCHAR</td>
   *     <td>bool</td>
   *     <td>UCHAR</td>
   *     <td>CHAR</td>
   *     <td>ULONG</td>
   *     <td>LONG</td>
   *     <td>DOUBLE</td>
   *   </tr>
   *   <tr>
   *     <td>CHAR</td>
   *     <td>bool</td>
   *     <td>UCHAR</td>
   *     <td>CHAR</td>
   *     <td>ULONG</td>
   *     <td>LONG</td>
   *     <td>DOUBLE</td>
   *   </tr>
   *   <tr>
   *     <td>ULONG</td>
   *     <td>bool</td>
   *     <td>UCHAR</td>
   *     <td>CHAR</td>
   *     <td>ULONG</td>
   *     <td>LONG</td>
   *     <td>DOUBLE</td>
   *   </tr>
   *   <tr>
   *     <td>LONG</td>
   *     <td>bool</td>
   *     <td>UCHAR</td>
   *     <td>CHAR</td>
   *     <td>ULONG</td>
   *     <td>LONG</td>
   *     <td>DOUBLE</td>
   *   </tr>
   *   <tr>
   *     <td>DOUBLE</td>
   *     <td>bool</td>
   *     <td>UCHAR</td>
   *     <td>CHAR</td>
   *     <td>ULONG</td>
   *     <td>LONG</td>
   *     <td>DOUBLE</td>
   *   </tr>
    * </table>
    * </center>
    *
    * @return SlipDatum value cast to a LONG
    */

   /**
    * @fn SlipDatum::operator DOUBLE()
    * @brief Cast the SlipDatum value to a DOUBLE.
    * <p>
    * The casts supported are given in the table below. Casts from the current
    * data type to another data type are given by reading from the row containing
    * the current data type to the column containing the desired type. If the cell
    * at the intersection of the row and column is not an error then the cast will
    * be successful.</p>
    * <p>
    * A cast to or from a <b>STRING</b> or a <b>PTR</b> to any other type is
    * not supported.</p>
    * <p>
    * When casting to or from a <b>bool</b>, the following rules apply>:</p>
    * <ul>
    *    <li>When casting to a <b>bool</b>, a zero (0) is false, anything
    *        else is true.
    *    <li>When cast from a <b>bool</b> to another type, false is zero (0),
    *        true is one (1).
    * </ul>
    * <p>
    * Retrieving the value contained in a cell datum is done by casting the
    * cell to the data type in the datum field. For example, if the datum
    * type is <b>DOUBLE</b> then:</p>
    * <pre>
    *   <b>(DOUBLE) Y</b>
    * </pre>
    * <p>
    * where Y is a SlipDatum cell will yield the value.</p>
    * <p></p>
    * <center>
    * <table>
    * <caption>Supported Casts (DOUBLE) Y</caption>
   *   <tr>
   *     <th>from/to</th>
   *     <th>bool</th>
   *     <th>UCHAR</th>
   *     <th>CHAR</th>
   *     <th>ULONG</th>
   *     <th>LONG</th>
   *     <th>DOUBLE</th>
   *   </tr>
   *   <tr>
   *     <td>bool</td>
   *     <td>bool</td>
   *     <td>UCHAR</td>
   *     <td>CHAR</td>
   *     <td>ULONG</td>
   *     <td>LONG</td>
   *     <td>DOUBLE</td>
   *   </tr>
   *   <tr>
   *     <td>UCHAR</td>
   *     <td>bool</td>
   *     <td>UCHAR</td>
   *     <td>CHAR</td>
   *     <td>ULONG</td>
   *     <td>LONG</td>
   *     <td>DOUBLE</td>
   *   </tr>
   *   <tr>
   *     <td>CHAR</td>
   *     <td>bool</td>
   *     <td>UCHAR</td>
   *     <td>CHAR</td>
   *     <td>ULONG</td>
   *     <td>LONG</td>
   *     <td>DOUBLE</td>
   *   </tr>
   *   <tr>
   *     <td>ULONG</td>
   *     <td>bool</td>
   *     <td>UCHAR</td>
   *     <td>CHAR</td>
   *     <td>ULONG</td>
   *     <td>LONG</td>
   *     <td>DOUBLE</td>
   *   </tr>
   *   <tr>
   *     <td>LONG</td>
   *     <td>bool</td>
   *     <td>UCHAR</td>
   *     <td>CHAR</td>
   *     <td>ULONG</td>
   *     <td>LONG</td>
   *     <td>DOUBLE</td>
   *   </tr>
   *   <tr>
   *     <td>DOUBLE</td>
   *     <td>bool</td>
   *     <td>UCHAR</td>
   *     <td>CHAR</td>
   *     <td>ULONG</td>
   *     <td>LONG</td>
   *     <td>DOUBLE</td>
   *   </tr>
    * </table>
    * </center>
    *
    * @return SlipDatum value cast to a DOUBLE
    */

   /*********************************************************
    *                 Operator Overloading                  *
   *********************************************************/

       /*************************************************
        *                Unary operators                *
       *************************************************/
   /**
    * @fn SlipDatum::operator!()
    * @brief A unary 'logical not' (<b>!Y</b>) of a SlipDatum value.
    * <p>
    * The operation returns a bool value. If the original type
    * is a bool then the logical negation is returned If the
    * original discrete (CHAR, UCHAR, LONG, ULONG) data type
    * is zero (0) then 'true' is returned, otherwise 'false'
    * is returned. The <b>logical not</b> is not defined for
    * a DOUBLE, STRING, or PTR.</p>
    * <br />
    * Example:
    * <tt><pre>
    *   !Y            // return the logical not of the value in the SlipDatum cell
    * </pre></tt>
    *
    * @return (bool) value of !Y
    */

   /**
    * @fn SlipDatum::operator~()
    * @brief A unary 'bitwise not' <b>~Y</b> is performed on discrete SlipDatum
    *        values.
    * <p> The 'bit not' operation is restricted to (UCHAR, CHAR, ULONG, LONG).
    *  It is illegal to attempt this operation on DOUBLE, PTR, and STRING.
    * an operation.</p>
    * <p>
    * The operation returns a ULONG value with the bitwise <b>not</b> of the
    * original value contained in the SlipDatum cell. The original cell is
    * unchanged.</p>
    * <p>
    * The <b>bit not</b> of a discrete value is formed by taking the one's
    * complement of the value. This is equivalent to doing an <b>exclusive
    * or</b> of the data with 0xFFFF...FF (flipping all the bits). The value
    * is cast to a ULONG and then an 'exclusive or' is performed.</p>
    * <p>If the operation is done on a <em>bool</em> then the bool is converted
    *    into a ULONG '1' for <b>true</b> and a '0' for <b>false</b> before
    *    the operations.</p>
    * If the original value is 0x21524110 then<br />
    * return value = 0x21524110 ^ 0xFFFFFFFF = 0xDEADBEEF
    * <br />
    * Example:
    * <pre>
    *     ~Y   // take the bit not of the SlipDatum value and
    *          // return it as a ULONG
    * </pre>
    * <br />
    *
    * @return (ULONG) value of ~Y
    */

   /**
    * @fn SlipDatum::operator+()
    * @brief A unary 'plus' (<b>+Y</b>) is performed on SlipDatum value
    * <p>
    * The operation returns a SlipDatum value with the value of the operand
    * unchanged. This does not alter the contents of the original object.</p>
    * <p>
    * This operation is restricted to arithmetic values (UCHAR, CHAR, ULONG,
    * LONG, DOUBLE) and bool, and is illegal for STRING and PTR.</p>
    * <p>A SlipDatum((LONG)) object is create for a bool with a value of
    *    '1' or <b>true</b> and '0' for <b>false></b>.</p>
    * <p>
    * The resultant data type of the operation depends on the data type of
    * the original SlipDatum value. The value is returned in a temporary
    * SlipDatum cell with the same data type and value as the original. </p>
    * <p>
    * Example:</p>
    * <tt><pre>
    *    SlipDatum Y = new SlipDatum((LONG)15);
    *    +Y;               // The resultant unaltered value is returned
    *                     // as a temporary SlipDatum object.
    *    SlipHeader& head = new Header();
    *    head.enqueue(+Y) // Create a temporary SlipDatum cell (+Y) and
    *                     // convert into a persistent object during the enqueue
    * </pre></tt>
    *
    * @return (SlipDatum&) reference to a temporary object
    */

   /**
    * @fn  SlipDatum::operator-()
    * @brief A unary 'minus' (<b>-Y</b>) is performed on SlipDatum value
    * <p>
    * The operation returns a SlipDatum value with the value of the operand
    * the negative of its original value. This does not alter the contents
    * of the original object.</p>
    * <p>
    * This operation is restricted to arithmetic values (UCHAR, CHAR, ULONG,
    * LONG, DOUBLE) and bool, and is illegal for STRING and PTR.</p>
    * <p>A new SlipDatum((LONG)) object is created for a bool with a value
    *    of '0' for <b>true</b> and '-1' for <b>false</b>.</p>
    * <p>
    * The resultant data type of the operation depends on the data type of
    * the original SlipDatum value. The value is returned in a temporary
    * SlipDatum cell with the same data type and value as the original. </p>
    * <p>
    * Example:</p>
    * <tt><pre>
    *    SlipDatum Y = new SlipDatum((LONG)15);
    *    -Y               // The resultant unaltered value is returned
    *                     // as a temporary SlipDatum object.
    *    SlipHeader& head = new Header();
    *    head.enqueue(-Y) // Create a temporary SlipDatum cell (+Y) and
    *                     // convert into a persistent object during the enqueue
    * </pre></tt>
    *
    * @return (SlipDatum&) reference to a temporary object
    */

   /**
    * @fn SlipDatum::operator++()
    * @brief A post-increment (<b>Y++</b>) operation on a SlipDatum value.
    * <p>The original object is incremented and returned.</p>
    * <p>
    * This operation is restricted to arithmetic values (UCHAR, CHAR, ULONG,
    * LONG) and is illegal for bool, DOUBLE, STRING and PTR.</p>
    * <p>
    * A post-increment is done on the value contained in a SlipDatum object and
    * a reference to the object returned. A one (1) is added to the value contained
    * in the SlipDatum object value.</p>
    * <p>
    * Example:</p>
    * <tt><pre>
    *    SlipDatum Y = SlipDatum((bool) true);
    *    Y++              // One (1) is added to Y.datum and
    *                     // a  reference to Y returned
    *    SlipHeader& head = new Header();
    *    head.enqueue(Y++)// One (1) is added to Y.datum and a
    *                     // reference to Y passed to enqueue
    * </pre></tt>
    * @return (SlipDatum&) an incremented object is returned
    */

   /**
    * @fn SlipDatum::operator--()
    * @brief A post-decrement (<b>Y--</b>) operation on a SlipDatum value.
    * <p>Decrement the value in the original object (except for bools).</p>
    * <p>
    * This operation is restricted to arithmetic values (UCHAR, CHAR, ULONG,
    * LONG) and is illegal for bool, DOUBLE, STRING and PTR.</p>
    * <p>
    * A post-increment is done on the value contained in a SlipDatum object and
    * a reference to the object returned. A one (1) is added to the value contained
    * in the SlipDatum object value.</p>
    * <p>
    * Example:</p>
    * <br />
    * <tt><pre>
    *    SlipDatum Y = new SlipDatum((ULONG) 3);
    *    Y--              // One (1) is subtracted from Y.datum and
    *                     // a  reference to Y returned
    *    SlipHeader& head = new Header();
    *    head.enqueue(Y--)// One (1) is subtracted from Y.datum and a
    *                     // reference to Y passed to enqueue
    * </pre></tt>
    * <p>
    * @return (SlipDatum&) a decremented object is returned
    */

   /**
    * @fn SlipDatum::operator++(int)
    * @brief A pre-increment (<b>++Y</b>) operation on a SlipDatum value.
    * <p>The original object is incremented and returned, except for bools.</p>
    * <p>
    * This operation is restricted to arithmetic values (UCHAR, CHAR, ULONG,
    * LONG) and is illegal for bool, DOUBLE, STRING and PTR.</p>
    * <p>
    * A pre-increment is done on the value contained in a SlipDatum object and
    * a reference to the object returned. A one (1) is added to the value contained
    * in the SlipDatum object value.</p>
    * <p>
    * Example:</p>
    * <br />
    * <pre>
    *    SlipDatum Y = SlipDatum((UCHAR)0x34);
    *    ++Y              // One (1) is added to Y.datum and
    *                     // a  reference to Y returned
    *    SlipHeader& head = new Header();
    *    head.enqueue(++Y)// One (1) is added to Y.datum and a
    *                     // reference to Y passed to enqueue
    * </pre>
    * <br />
    * <p>
    * @return (SlipDatum&) an incremented object is returned
    */

   /**
    * @fn SlipDatum::operator--(int)
    * @brief A pre-decrement (<b>--Y</b>) operation on a SlipDatum value.
    * <p>The original value is decremented and returned, except for bools.</p>
    * <p>
    * <p>
    * Example:</p>
    * <br />
    * <pre>
    *    SlipDatum Y = new SlipDatum((ULONG)0777);
    *    --Y              // One (1) is subtracted from Y.datum and
    *                     // a  reference to Y returned
    *    SlipHeader& head = new Header();
    *    head.enqueue(--Y)// One (1) is subtracted from Y.datum and a
    *                     // reference to Y passed to enqueue
    * </pre>
    * @return (SlipDatum&) a decremented object is returned
    */

       /*************************************************
        *                Binary operators               *
       *************************************************/

   /**
    * @fn SlipDatum::operator+(const SlipDatum& X)
    * @brief Add (<b>Y + X</b>) the value of two SlipDatum objects and return the result
    * <p>A temporary object is returned containing the result of Y + X. The
    *    type of the result is a DOUBLE if Y or X is a DOUBLE, otherwise it
    *    is the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation and if Y
    *    is a bool and X is not a DOUBLE, the result returned is a bool with
    *    <b>false</b> if the resultant computation is zero (0), otherwise
    *    <b>true</b>.</p>
    * <p>X or Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y + 2;                // SlipDatum((LONG)3)
    *    SlipHeader head();                    // create a list
    *    SlipDatum& list = head.enqueue(tmp);  // copy tmp
    *    SlipDatum& head.push(Y + 2);          // copy SlipDatum((LONG)3)
    * </pre></tt>
    *
    * @param[in] X (SlipDatum&) addend
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator-(const SlipDatum& X)
    * @brief Subtract (<b>Y - X</b>) the value of two SlipDatum objects and
    *        return the result
    * <p>A temporary object is returned containing the result of Y _ X. The
    *    type of the result is a DOUBLE if Y or X is a DOUBLE, otherwise it
    *    is the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation and if Y
    *    is a bool and X is not a DOUBLE, the result returned is a bool with
    *    <b>false</b> if the resultant computation is zero (0), otherwise
    *    <b>true</b>.</p>
    * <p>X or Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y - 2;                // SlipDatum((LONG)1)
    *    SlipHeader head();                    // create a list
    *    SlipDatum& list = head.enqueue(tmp);  // copy tmp
    *    SlipDatum& head.push(Y - 2);          // copy SlipDatum((LONG)1)
    * </pre></tt>
    *
    * @param[in] X (SlipDatum&) subtrahend
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator*(const SlipDatum& X)
    * @brief Multiply (<b>Y * X</b>) the value of two SlipDatum objects and
    *        return the result
    * <p>A temporary object is returned containing the result of Y * X. The
    *    type of the result is a DOUBLE if Y or X is a DOUBLE, otherwise it
    *    is the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation and if Y
    *    is a bool and X is not a DOUBLE, the result returned is a bool with
    *    <b>false</b> if the resultant computation is zero (0), otherwise
    *    <b>true</b>.</p>
    * <p>X or Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y * 2;                // SlipDatum((LONG)6)
    *    SlipHeader head();                    // create a list
    *    SlipDatum& list = head.enqueue(tmp);  // copy tmp
    *    SlipDatum& head.push(Y * 2);          // copy SlipDatum((LONG)6)
    * </pre></tt>
    *
    * @param[in] X (SlipDatum&) multiplicand
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator/(const SlipDatum& X)
    * @brief Divide (<b>Y / X</b>) the value of two SlipDatum objects and
    *        return the result
    * <p>A temporary object is returned containing the result of Y / X. The
    *    type of the result is a DOUBLE if Y or X is a DOUBLE, otherwise it
    *    is the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation and if Y
    *    is a bool and X is not a DOUBLE, the result returned is a bool with
    *    <b>false</b> if the resultant computation is zero (0), otherwise
    *    <b>true</b>.</p>
    * <p>X or Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y / 2;                // SlipDatum((LONG)1)
    *    SlipHeader head();                    // create a list
    *    SlipDatum& list = head.enqueue(tmp);  // copy tmp
    *    SlipDatum& head.push(Y / 2);          // copy SlipDatum((LONG)1)
    * </pre></tt>
    *
    * @param[in] X (SlipDatum&) dividend
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator%(const SlipDatum& X)
    * @brief Take the modulus (<b>Y % X</b>) of two SlipDatum objects and
    *        return the result
    * <p>
    * The input SlipDatum value (X) is the modulus of the operation.
    * <p>A temporary object is returned containing the result of Y % X. The
    *    type of the result is a DOUBLE if Y or X is a DOUBLE, otherwise it
    *    is the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation and if Y
    *    is a bool and X is not a DOUBLE, the result returned is a bool with
    *    <b>false</b> if the resultant computation is zero (0), otherwise
    *    <b>true</b>.</p>
    * <p>X or Y can not be a DOUBLE, PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y % 2;                // SlipDatum((LONG)1)
    *    SlipHeader head();                    // create a list
    *    SlipDatum& list = head.enqueue(tmp);  // copy tmp
    *    SlipDatum& head.push(Y % 2);          // copy SlipDatum((LONG)1)
    * </pre></tt>
    *
    * @param[in] X (SlipDatum&) subtrahend
    * @return (SlipDatum&) result of the operation
    *
    */

   /**
    * @fn SlipDatum::operator+(const DOUBLE X)
    * @brief Add (<b>Y + X</b>) and return the result
    * <p>Add Y and (DOUBLE) x. The result will be a SlipDatum((DOUBLE)).</p>
    * <p>A temporary object is returned containing the result of Y + X. The
    *    type of the result is a DOUBLE.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.</p>
    * <p>Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y + 2.0;              // SlipDatum((DOUBLE)5.0)
    *    SlipHeader head();                    // create a list
    *    SlipDatum& list = head.enqueue(tmp);  // copy tmp
    *    SlipDatum& head.push(Y + 2.0);        // copy SlipDatum((DOUBLE)5.0)
    * </pre></tt>
    *
    * @param[in] X (DOUBLE) addend
    * @return (SlipDatum((DOUBLE))&) result of the operation
    */

   /**
    * @fn SlipDatum::operator-(const DOUBLE X)
    * @brief Subtract (<b>Y - X</b>) and return the result
    * <p>Subtract Y and (DOUBLE) X. The result will be a SlipDatum((DOUBLE)).</p>
    * <p>A temporary object is returned containing the result of Y - X. The
    *    type of the result is a DOUBLE.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.</p>
    * <p>Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y - 2.0;              // SlipDatum((DOUBLE)1.0)
    *    SlipHeader head();                    // create a list
    *    SlipDatum& list = head.enqueue(tmp);  // copy tmp
    *    SlipDatum& head.push(Y - 2.0);        // copy SlipDatum((DOUBLE)1.0)
    * </pre></tt>
    *
    * @param[in] X (DOUBLE) subtrahend
    * @return (SlipDatum((DOUBLE))&) result of the operation
    */

   /**
    * @fn SlipDatum::operator*(const DOUBLE X)
    * @brief Multiply (<b>Y * X</b>) and return the result
    * <p>Multiply Y and (DOUBLE) X. The result will be a SlipDatum((DOUBLE)).</p>
    * <p>A temporary object is returned containing the result of Y * X. The
    *    type of the result is a DOUBLE.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.</p>
    * <p>Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y * 2.0;              // SlipDatum((DOUBLE)6.0)
    *    SlipHeader head();                    // create a list
    *    SlipDatum& list = head.enqueue(tmp);  // copy tmp
    *    SlipDatum& head.push(Y * 2.0);        // copy SlipDatum((DOUBLE)6.0)
    * </pre></tt>
    *
    * @param[in] X (DOUBLE) multiplicand
    * @return (SlipDatum((DOUBLE))&) result of the operation
    */

   /**
    * @fn SlipDatum::operator/(const DOUBLE X)
    * @brief Divide (<b>Y / X</b>) and return the result
    * <p>Divide Y by (DOUBLE) X. The result will be a SlipDatum((DOUBLE)).</p>
    * <p>A temporary object is returned containing the result of Y / X. The
    *    type of the result is a DOUBLE.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.</p>
    * <p>Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y / 2.0;              // SlipDatum((DOUBLE)1.5)
    *    SlipHeader head();                    // create a list
    *    SlipDatum& list = head.enqueue(tmp);  // copy tmp
    *    SlipDatum& head.push(Y / 2.0);        // copy SlipDatum((DOUBLE)1.5)
    * </pre></tt>
    *
    * @param[in] X (DOUBLE) dividend
    * @return (SlipDatum((DOUBLE))&) result of the operation
    */

   /**
    * @fn SlipDatum::operator+(const bool   X)
    * @brief Add (<b>Y + X</b>) and return the result
    * <p>Add Y and (bool) X. </p>
    * <p>A temporary object is returned containing the result of Y + X.
    *    The result will be the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y + true;             // SlipDatum((LONG)4)
    *    SlipHeader head();                    // create a list
    *    SlipDatum& list = head.enqueue(tmp);  // copy tmp
    *    SlipDatum& head.push(Y + true);       // copy SlipDatum((ONG)4)
    * </pre></tt>
    *
    * @param[in] X (bool) addend
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator-(const bool   X)
    * @brief Subtract (<b>Y - X</b>) and return the result
    * <p>Subtract Y and (bool) X. </p>
    * <p>A temporary object is returned containing the result of Y - X.
    *    The result will be the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y - true;             // SlipDatum((LONG)2)
    *    SlipHeader head();                    // create a list
    *    SlipDatum& list = head.enqueue(tmp);  // copy tmp
    *    SlipDatum& head.push(Y - true);       // copy SlipDatum((ONG)2)
    * </pre></tt>
    *
    * @param[in] X (bool) subtrahend
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator*(const bool   X)
    * @brief Add (<b>Y * X</b>) and return the result
    * <p>Mulltiply Y and (bool) X. </p>
    * <p>A temporary object is returned containing the result of Y * X.
    *    The result will be the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y * true;             // SlipDatum((LONG)3)
    *    SlipHeader head();                    // create a list
    *    SlipDatum& list = head.enqueue(tmp);  // copy tmp
    *    SlipDatum& head.push(Y * true);       // copy SlipDatum((ONG)3)
    * </pre></tt>
    *
    * @param[in] X (bool) multiplicand
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator/(const bool   X)
    * @brief Divide (<b>Y / X</b>) and return the result
    * <p>Divide Y and (bool) X. </p>
    * <p>A temporary object is returned containing the result of Y / X.
    *    The result will be the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y / true;             // SlipDatum((LONG)3)
    *    SlipHeader head();                    // create a list
    *    SlipDatum& list = head.enqueue(tmp);  // copy tmp
    *    SlipDatum& head.push(Y / true);       // copy SlipDatum((ONG)3)
    * </pre></tt>
    *
    * @note the result is undefined for X == <b>false</b>.
    *
    * @param[in] X (bool) dividend
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator%(const bool   X)
    * @brief Take the modulus (<b>Y % X</b>) and return the result
    * <p>Y % (bool) X. </p>
    * <p>A temporary object is returned containing the result of Y % X.
    *    The result will be the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a DOUBLE, PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y % true;             // SlipDatum((LONG)3)
    *    SlipHeader head();                    // create a list
    *    SlipDatum& list = head.enqueue(tmp);  // copy tmp
    *    SlipDatum& head.push(Y % true);       // copy SlipDatum((ONG)3)
    * </pre></tt>
    *
    * @note the result is undefined for X == <b>false</b>.
    *
    * @param[in] X (bool) modulus
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator+(const UCHAR  X)
    * @brief Add (<b>Y + X</b>) and return the result
    * <p>Add Y and (UCHAR) X. X is converted to a ULONG before the operation.</p>
    * <p>A temporary object is returned containing the result of Y + X.
    *    The result will be the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y + (UCHAR)2;          // SlipDatum((LONG)5)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y + (UCHAR)2);    // copy SlipDatum((ONG)5)
    * </pre></tt>
    *
    * @param[in] X (UCHAR) addend
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator-(const UCHAR  X)
    * @brief Subtract (<b>Y - X</b>) and return the result
    * <p>Subtract Y and (UCHAR) X. X is converted to a ULONG before the operation.</p>
    * <p>A temporary object is returned containing the result of Y - X.
    *    The result will be the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y - (UCHAR)2;          // SlipDatum((LONG)1)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y - (UCHAR)2);    // copy SlipDatum((ONG)1)
    * </pre></tt>
    *
    * @param[in] X (UCHAR) subtrahend
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator*(const UCHAR  X)
    * @brief Multiply (<b>Y * X</b>) and return the result
    * <p>Multiply Y and (UCHAR) X. X is converted to a ULONG before the operation.</p>
    * <p>A temporary object is returned containing the result of Y * X.
    *    The result will be the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y * (UCHAR)2;          // SlipDatum((LONG)6)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y * (UCHAR)2);    // copy SlipDatum((ONG)6)
    * </pre></tt>
    *
    * @param[in] X (UCHAR) multiplicand
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator/(const UCHAR  X)
    * @brief Divide (<b>Y / X</b>) and return the result
    * <p>Divide Y and (UCHAR) X. X is converted to a ULONG before the operation.</p>
    * <p>A temporary object is returned containing the result of Y / X.
    *    The result will be the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y / (UCHAR)2;          // SlipDatum((LONG)1)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y / (UCHAR)2);    // copy SlipDatum((ONG)1)
    * </pre></tt>
    *
    * @param[in] X (UCHAR) dividend
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator%(const UCHAR  X)
    * @brief Take the modulus (<b>Y % X</b>) and return the result
    * <p>Modulus Y % (UCHAR) X. X is converted to a ULONG before the operation.</p>
    * <p>A LONG will be returned with the result of taking the modulus.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a DOUBLE, PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y % (UCHAR)2;          // SlipDatum((LONG)1)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y % (UCHAR)2); // copy SlipDatum((ONG)1)
    * </pre></tt>
    *
    * @param[in] X (UCHAR) modulus
    * @return (SlipDatum&) result of the operation
    */


   /**
    * @fn SlipDatum::operator+(const CHAR   X)
    * @brief Add (<b>Y + X</b>) and return the result
    * <p>Add Y and (CHAR) X.</p>
    * <p>A temporary object is returned containing the result of Y + X.
    *    The result will be the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y + (CHAR)2;           // SlipDatum((LONG)5)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y + (CHAR)2);     // copy SlipDatum((ONG)5)
    * </pre></tt>
    *
    * @param[in] X (CHAR) addend
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator-(const CHAR   X)
    * @brief Subtract (<b>Y - X</b>) and return the result
    * <p>Subtract Y and (CHAR) X.</p>
    * <p>A temporary object is returned containing the result of Y - X.
    *    The result will be the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y - (CHAR)2;           // SlipDatum((LONG)1)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y - (CHAR)2);     // copy SlipDatum((ONG)1)
    * </pre></tt>
    *
    * @param[in] X (CHAR) subtrahend
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator*(const CHAR   X)
    * @brief Multiply (<b>Y * X</b>) and return the result
    * <p>Multiiply Y and (CHAR) X.</p>
    * <p>A temporary object is returned containing the result of Y * X.
    *    The result will be the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y * (CHAR)2;           // SlipDatum((LONG)6)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y * (CHAR)2);     // copy SlipDatum((ONG)6)
    * </pre></tt>
    *
    * @param[in] X (CHAR) multiplicand
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator/(const CHAR   X)
    * @brief Divide (<b>Y / X</b>) and return the result
    * <p>Divide Y and (CHAR) X.</p>
    * <p>A temporary object is returned containing the result of Y / X.
    *    The result will be the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y / (CHAR)2;           // SlipDatum((LONG)1)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y / (CHAR)2);     // copy SlipDatum((ONG)1)
    * </pre></tt>
    *
    * @param[in] X (CHAR) dividend
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator%(const CHAR   X)
    * @brief Take the modulus (<b>Y % X</b>) and return the result
    * <p>Modulus Y % (CHAR) X. X is converted to a ULONG before the operation.</p>
    * <p>A LONG will be returned with the result of taking the modulus.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a DOUBLE, PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y % (UCHAR)2;          // SlipDatum((LONG)1)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y % (UCHAR)2); // copy SlipDatum((ONG)1)
    * </pre></tt>
    *
    * @param[in] X (CHAR) modulus
    * @return (SlipDatum&) result of the operation
    */


   /**
    * @fn SlipDatum::operator+(const ULONG  X)
    * @brief Add (<b>Y + X</b>) and return the result
    * <p>Add Y and (ULONG) X.</p>
    * <p>A temporary object is returned containing the result of Y + X.
    *    The result will be the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y + (ULONG)2;          // SlipDatum((LONG)5)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y + (ULONG)2);    // copy SlipDatum((ONG)5)
    * </pre></tt>
    *
    * @param[in] X (ULONG) addend
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator-(const ULONG  X)
    * @brief Subtract (<b>Y + X</b>) and return the result
    * <p>Subtract Y and (ULONG) X.</p>
    * <p>A temporary object is returned containing the result of Y - X.
    *    The result will be the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y - (ULONG)2;          // SlipDatum((LONG)1)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y - (ULONG)2);    // copy SlipDatum((ONG)1)
    * </pre></tt>
    *
    * @param[in] X (ULONG) subtrahend
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator*(const ULONG  X)
    * @brief Multiply (<b>Y * X</b>) and return the result
    * <p>Multiply Y and (ULONG) X.</p>
    * <p>A temporary object is returned containing the result of Y * X.
    *    The result will be the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y * (ULONG)2;          // SlipDatum((LONG)6)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y * (ULONG)2);    // copy SlipDatum((ONG)6)
    * </pre></tt>
    *
    * @param[in] X (ULONG) multiplicand
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator/(const ULONG  X)
    * @brief Divide (<b>Y / X</b>) and return the result
    * <p>Divide Y and (ULONG) X.</p>
    * <p>A temporary object is returned containing the result of Y / X.
    *    The result will be the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y / (ULONG)2;          // SlipDatum((LONG)1)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y / (ULONG)2);    // copy SlipDatum((ONG)1)
    * </pre></tt>
    *
    * @param[in] X (ULONG) dividend
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator%(const ULONG  X)
    * @brief Take the modulus (<b>Y % X</b>) and return the result
    * <p>Modulus Y % (ULONG) X.</p>
    * <p>A LONG will be returned with the result of taking the modulus.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a DOUBLE, PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y % (ULONG)2;          // SlipDatum((LONG)1)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y % (ULONG)2);    // copy SlipDatum((ONG)1)
    * </pre></tt>
    *
    * @param[in] X (ULONG) modulus
    * @return (SlipDatum&) result of the operation
    */


   /**
    * @fn SlipDatum::operator+(const LONG   X)
    * @brief Add (<b>Y + X</b>) and return the result
    * <p>Add Y and (LONG) X.</p>
    * <p>A temporary object is returned containing the result of Y + X.
    *    The result will be the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y + (LONG)2;           // SlipDatum((LONG)5)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y + (LONG)2);     // copy SlipDatum((ONG)5)
    * </pre></tt>
    *
    * @param[in] X (LONG) addend
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator-(const LONG   X)
    * @brief Subtract (<b>Y + X</b>) and return the result
    * <p>Add Y and (LONG) X.</p>
    * <p>A temporary object is returned containing the result of Y - X.
    *    The result will be the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y - (LONG)2;           // SlipDatum((LONG)1)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y - (LONG)2);     // copy SlipDatum((ONG)1)
    * </pre></tt>
    *
    * @param[in] X (LONG) subtrahend
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator*(const LONG   X)
    * @brief Multiply (<b>Y * X</b>) and return the result
    * <p>Multiply Y and (LONG) X.</p>
    * <p>A temporary object is returned containing the result of Y * X.
    *    The result will be the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y * (LONG)2;           // SlipDatum((LONG)6)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y * (LONG)2);     // copy SlipDatum((ONG)6)
    * </pre></tt>
    *
    * @param[in] X (LONG) multiplicand
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator/(const LONG   )
    * @brief Divide (<b>Y / X</b>) and return the result
    * <p>Divide Y and (LONG) X.</p>
    * <p>A temporary object is returned containing the result of Y / X.
    *    The result will be the same type as Y.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y / (LONG)2;           // SlipDatum((LONG)1)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y / (LONG)2);     // copy SlipDatum((ONG)1)
    * </pre></tt>
    *
    * @param[in] X (LONG) dividend
    * @return (SlipDatum&) result of the operation
    */

   /**
    * @fn SlipDatum::operator%(const LONG   X)
    * @brief Take the modulus (<b>Y % X</b>) and return the result
    * <p>Modulus Y % (LONG) X.</p>
    * <p>A LONG will be returned with the result of taking the modulus.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    *    A bool object is returned if Y is a bool with a result of
    *    <b>false</b> if the computation is zero (0) or <b>true</b> otherwise.</p>
    * <p>Y can not be a DOUBLE, PTR, or STRING.</p>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  tmp= Y % (ULONG)2;          // SlipDatum((LONG)1)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y % (ULONG)2);    // copy SlipDatum((ONG)1)
    * </pre></tt>
    *
    * @param[in] X (LONG) modulus
    * @return (SlipDatum&) result of the operation
    */

       /*************************************************
        *                Bit operators                  *
       *************************************************/
   /**
    * @fn SlipDatum::operator<<(const SlipDatum&  X)
    * @brief Left shift <b>Y &lt;&lt; X</b> and return its value.
    * <p>Left shift Y by X bits. The original value of X and Y is unchanged.</p>
    * <p>Only the discrete types can be used (bool, CHAR, UCHAR, LONG, and
    *    ULONG) for either Y or X. DOUBLE, PTR, and STRING are not valid.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b></p>.
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a  SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>
    * Left shifts treat the Y data type as unsigned, either UCHAR or ULONG.
    * The sign bit (the most significant bit (MSB) has no special meaning.
    * A left shift shifts all bits in Y one bit to the left and inserts a
    * zero (0) in place of the rightmost bit for each bit shifted. The
    * leftmost bit is shifted out for all X values &gt; 0.</p>
    * <p>For example:</p>
    * <p>
    * <tt><pre>
    *               8 bit octet
    *      0101 0101 &lt;&lt; 1 => 1010 1010
    * </pre></tt>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)7);
    *    SlipDatum  X(ULONG)2);
    *    SlipDatum  tmp= Y &lt;&lt; X;               // SlipDatum((LONG)28)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y &lt;&lt; X);           // new SlipDatum((LONG)28)
    * </pre></tt>
    *
    * @param[in] X (SlipDatum&) number of bits to shift left
    * @return LONG result of Y &lt;&lt; X.
    */

   /**
    * @fn SlipDatum::operator>>(const SlipDatum&  X)
    * @brief Right shift <b>Y &gt;&gt; X</b> and return its value.
    * <p>Right shift Y by X bits. The original value of X and Y is unchanged.</p>
    * <p>Only the discrete types can be used (bool, CHAR, UCHAR, LONG, and
    *    ULONG) for either Y or X. DOUBLE, PTR, and STRING are not valid.<p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    * <p>Right shifts treat the Y data type as unsigned, either UCHAR or ULONG.
    *    The sign bit (the most significant bit (MSB) has no special meaning.
    *    A right shift shifts all bits in Y to the right and inserts a zero (0)
    *    in the MSB for each bit shifted. The rightmost bit is shifted out for
    *    all X values &gt; sizeof(ULONG).</p>
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a  SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>For example:</p>
    * <p>
    * <tt><pre>
    *               8 bit octet
    *      0101 0101 &gt;&gt; 1 => 0010 1010
    * </pre></tt>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)7);
    *    SlipDatum  X(ULONG)2);
    *    SlipDatum  tmp= Y &gt;&gt; X;                // SlipDatum((LONG)1)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y &gt;&gt; X);           // new SlipDatum((LONG)1)
    * </pre></tt>
    *
    * @param[in] X (SlipDatum&) number of bits to shift right
    * @return LONG result of Y &gt;&gt; X.
    */

   /**
    * @fn SlipDatum::operator&(const SlipDatum&  X)
    * @brief And Y with X (<b>Y &amp; X</b>) and return a temporary object.
    * <p>
    * Signed and unsigned data are treated identically. There is no
    * significance given to the sign bit.</p>
    * <p>Only the discrete types can be used (bool, CHAR, UCHAR, LONG, and
    *    ULONG) for either Y or X. DOUBLE, PTR, and STRING are not valid.<p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a  SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>The bitwise and truth table is:</p>
    * <center><b>&amp; Truth Table</b></center>
    * <center>
    * |  & | 0  | 1  |
    * | :: | :: | :: |
    * |  0 | 0  | 0  |
    * |  1 | 0  | 1  |
    * </center>
    * <br />
    * <p>Some examples:</p>
    * <center><b>X &amp; Y</b></center></p>
    * <center>
    * | BitField  | BitField  |   Result  |
    * | --------- | --------- | --------- |
    * | 0101 0101 | 0101 0101 | 0101 0101 |
    * | 1010 1010 | 1100 0000 | 0100 0000 |
    * | 0010 1010 | 1110 1010 | 0010 1010 |
    * </center>
    * <br />
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  X((ULONG)2);
    *    SlipDatum  tmp= Y &amp; X;                 // SlipDatum((LONG)2)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y &amp; X);           // new SlipDatum((LONG)2)
    * </pre></tt>
    *
    * @param[in] X (SlipDatum&) value to be logically anded
    * @return (LONG) result of X &amp; Y
    */

   /**
    * @fn SlipDatum::operator|(const SlipDatum&  X)
    * @brief Or Y with X (<b>Y \| X</b>) and return a temporary object.
    * <p>
    * Signed and unsigned data are treated identically. There is no
    * significance given to the sign bit.</p>
    * <p>Only the discrete types can be used (bool, CHAR, UCHAR, LONG, and
    *    ULONG) for either Y or X. DOUBLE, PTR, and STRING are not valid.<p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a  SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>The bitwise or truth table is:</p>
    * <center><b>&amp; Truth Table</b></center>
    * <center>
    * |  \| | 0  | 1  |
    * | :: | :: | :: |
    * |  0 | 0  | 1  |
    * |  1 | 1  | 1  |
    * </center>
    * <br />
    * <p>Some examples:</p>
    * <center><b>X \| Y</b></center></p>
    * <center>
    * | BitField  | BitField  |   Result  |
    * | --------- | --------- | --------- |
    * | 0101 0101 | 0101 0101 | 0101 0101 |
    * | 1010 1010 | 1100 0000 | 1110 1010 |
    * | 0010 1010 | 1110 1010 | 1110 1010 |
    * </center>
    * <br />
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  X((ULONG)2);
    *    SlipDatum  tmp= Y \| X;                 // SlipDatum((LONG)3)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y \| X);           // new SlipDatum((LONG)3)
    * </pre></tt>
    *
    * @param[in] X (SlipDatum&) value to be logically or'd
    * @return (LONG) result of X \| Y
    */

   /**
    * @fn SlipDatum::operator^(const SlipDatum&  X)
    * @brief Exclusive Y with X (<b>Y ^ X</b>) and return a temporary object.
    * <p>
    * Signed and unsigned data are treated identically. There is no
    * significance given to the sign bit.</p>
    * <p>Only the discrete types can be used (bool, CHAR, UCHAR, LONG, and
    *    ULONG) for either Y or X. DOUBLE, PTR, and STRING are not valid.<p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a  SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>The bitwise or truth table is:</p>
    * <center><b>&amp; Truth Table</b></center>
    * <center>
    * |  ^ | 0  | 1  |
    * | :: | :: | :: |
    * |  0 | 0  | 1  |
    * |  1 | 1  | 0  |
    * </center>
    * <br />
    * <p>Some examples:</p>
    * <center><b>X &amp; Y</b></center></p>
    * <center>
    * | BitField  | BitField  |   Result  |
    * | --------- | --------- | --------- |
    * | 0101 0101 | 0101 0101 | 0000 0000 |
    * | 1010 1010 | 1100 0000 | 0110 1010 |
    * | 0010 1010 | 1110 1010 | 1100 0000 |
    * </center>
    * <br />
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  X((ULONG)2);
    *    SlipDatum  tmp= Y ^ X;                 // SlipDatum((LONG)1)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y ^ X);           // new SlipDatum((LONG)1)
    * </pre></tt>
    *
    * @param[in] X (SlipDatum&) value to be logically exclusive or'd
    * @return (LONG) result of X ^ Y
    */

   /**
    * @fn SlipDatum::operator<<(const bool  X) 
    * @brief Left shift <b>Y &lt;&lt; X</b> and return its value.
    * <p>Left shift Y by X bits. The original value of X and Y is unchanged.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a  SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>
    * Left shifts treat the Y data type as unsigned, either UCHAR or ULONG.
    * The sign bit (the most significant bit (MSB) has no special meaning.
    * A left shift shifts all bits in Y one bit to the left and inserts a
    * zero (0) in place of the rightmost bit for each bit shifted. The
    * leftmost bit is shifted out for all X values &gt; 0.</p>
    * <p>For example:</p>
    * <p>
    * <tt><pre>
    *               8 bit octet
    *      0101 0101 &lt;&lt; 1 => 1010 1010
    * </pre></tt>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)7);
    *    SlipDatum  X(bool)true);
    *    SlipDatum  tmp= Y &lt;&lt; X;               // SlipDatum((LONG)14)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y &lt;&lt; X);           // new SlipDatum((LONG)14)
    * </pre></tt>
    *
    * @param[in] X (bool) number of bits to shift left
    * @return LONG result of Y &lt;&lt; X.
    */

   /**
    * @fn SlipDatum::operator>>(const bool  X)
    * @brief Right shift <b>Y &gt;&gt; X</b> and return its value.
    * <p>Right shift Y by X bits. The original value of X and Y is unchanged.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    * <p>Right shifts treat the Y data type as unsigned, either UCHAR or ULONG.
    *    The sign bit (the most significant bit (MSB) has no special meaning.
    *    A right shift shifts all bits in Y to the right and inserts a zero (0)
    *    in the MSB for each bit shifted. The rightmost bit is shifted out for
    *    all X values &gt; sizeof(ULONG).</p>
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a  SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>For example:</p>
    * <p>
    * <tt><pre>
    *               8 bit octet
    *      0101 0101 &gt;&gt; 1 => 0010 1010
    * </pre></tt>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)7);
    *    SlipDatum  X(bool)true);
    *    SlipDatum  tmp= Y &gt;&gt; X;                // SlipDatum((LONG)3)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y &gt;&gt; X);           // new SlipDatum((LONG)3)
    * </pre></tt>
    *
    * @param[in] X (bool) number of bits to shift right
    * @return LONG result of Y &gt;&gt; X.
    */

   /**
    * @fn SlipDatum::operator&(const bool   X)
    * @brief And Y with X (<b>Y &amp; X</b>) and return a temporary object.
    * <p>
    * Signed and unsigned data are treated identically. There is no
    * significance given to the sign bit.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a  SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>The bitwise and truth table is:</p>
    * <center><b>&amp; Truth Table</b></center>
    * <center>
    * |  & | 0  | 1  |
    * | :: | :: | :: |
    * |  0 | 0  | 0  |
    * |  1 | 0  | 1  |
    * </center>
    * <br />
    * <p>Some examples:</p>
    * <center><b>X &amp; Y</b></center></p>
    * <center>
    * | BitField  | BitField  |   Result  |
    * | --------- | --------- | --------- |
    * | 0101 0101 | 0101 0101 | 0101 0101 |
    * | 1010 1010 | 1100 0000 | 0100 0000 |
    * | 0010 1010 | 1110 1010 | 0010 1010 |
    * </center>
    * <br />
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  X((bool)true);
    *    SlipDatum  tmp= Y &amp; X;                 // SlipDatum((LONG)1)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y &amp; X);           // new SlipDatum((LONG)1)
    * </pre></tt>
    *
    * @param[in] X (bool) value to be logically anded
    * @return (LONG) result of X &amp; Y
    */

   /**
    * @fn SlipDatum::operator|(const bool   X)
    * @brief Or Y with X (<b>Y \| X</b>) and return a temporary object.
    * <p>
    * Signed and unsigned data are treated identically. There is no
    * significance given to the sign bit.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a  SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>The bitwise or truth table is:</p>
    * <center><b>&amp; Truth Table</b></center>
    * <center>
    * |  \| | 0  | 1  |
    * | :: | :: | :: |
    * |  0 | 0  | 1  |
    * |  1 | 1  | 1  |
    * </center>
    * <br />
    * <p>Some examples:</p>
    * <center><b>X \| Y</b></center></p>
    * <center>
    * | BitField  | BitField  |   Result  |
    * | --------- | --------- | --------- |
    * | 0101 0101 | 0101 0101 | 0101 0101 |
    * | 1010 1010 | 1100 0000 | 1110 1010 |
    * | 0010 1010 | 1110 1010 | 1110 1010 |
    * </center>
    * <br />
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  X((bool)true);
    *    SlipDatum  tmp= Y \| X;                 // SlipDatum((LONG)3)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y \| X);           // new SlipDatum((LONG)3)
    * </pre></tt>
    *
    * @param[in] X (bool) value to be logically or'd
    * @return (LONG) result of X \| Y
    */

   /**
    * @fn SlipDatum::operator^(const bool   X)
    * @brief Exclusive Y with X (<b>Y ^ X</b>) and return a temporary object.
    * <p>
    * Signed and unsigned data are treated identically. There is no
    * significance given to the sign bit.</p>
    * <p>A bool object is converted to a LONG before the operation with a
    *    value of zero (0) for <b>false</b> and one (1) for <b>true</b>.
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a  SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>The bitwise or truth table is:</p>
    * <center><b>&amp; Truth Table</b></center>
    * <center>
    * |  ^ | 0  | 1  |
    * | :: | :: | :: |
    * |  0 | 0  | 1  |
    * |  1 | 1  | 0  |
    * </center>
    * <br />
    * <p>Some examples:</p>
    * <center><b>X &amp; Y</b></center></p>
    * <center>
    * | BitField  | BitField  |   Result  |
    * | --------- | --------- | --------- |
    * | 0101 0101 | 0101 0101 | 0000 0000 |
    * | 1010 1010 | 1100 0000 | 0110 1010 |
    * | 0010 1010 | 1110 1010 | 1100 0000 |
    * </center>
    * <br />
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  X((bool)1);
    *    SlipDatum  tmp= Y ^ X;                 // SlipDatum((LONG)2)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y ^ X);           // new SlipDatum((LONG)2)
    * </pre></tt>
    *
    * @param[in] X (bool) value to be logically exclusive or'd
    * @return (LONG) result of X ^ Y
    */

   /**
    * @fn SlipDatum::operator<<(const UCHAR X)
    * @brief Left shift <b>Y &lt;&lt; X</b> and return its value.
    * <p>Left shift Y by X bits. The original value of X and Y is unchanged.</p>
    * <p>The input (UCHAR)X is widened to a (ULONG).</p>.
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a  SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>
    * Left shifts treat the Y data type as unsigned, either UCHAR or ULONG.
    * The sign bit (the most significant bit (MSB) has no special meaning.
    * A left shift shifts all bits in Y one bit to the left and inserts a
    * zero (0) in place of the rightmost bit for each bit shifted. The
    * leftmost bit is shifted out for all X values &gt; 0.</p>
    * <p>For example:</p>
    * <p>
    * <tt><pre>
    *               8 bit octet
    *      0101 0101 &lt;&lt; 1 => 1010 1010
    * </pre></tt>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)7);
    *    SlipDatum  X(UCHAR)2);
    *    SlipDatum  tmp= Y &lt;&lt; X;               // SlipDatum((LONG)28)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y &lt;&lt; X);           // new SlipDatum((LONG)28)
    * </pre></tt>
    *
    * @param[in] X (UCHAR) number of bits to shift left
    * @return LONG result of Y &lt;&lt; X.
    */

   /**
    * @fn SlipDatum::operator>>(const UCHAR X)
    * @brief Right shift <b>Y &gt;&gt; X</b> and return its value.
    * <p>Right shift Y by X bits. The original value of X and Y is unchanged.</p>
    * <p>The input (UCHAR)X is widened to a (ULONG).</p>.
    * <p>Right shifts treat the Y data type as unsigned, either UCHAR or ULONG.
    *    The sign bit (the most significant bit (MSB) has no special meaning.
    *    A right shift shifts all bits in Y to the right and inserts a zero (0)
    *    in the MSB for each bit shifted. The rightmost bit is shifted out for
    *    all X values &gt; sizeof(ULONG).</p>
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a  SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>For example:</p>
    * <p>
    * <tt><pre>
    *               8 bit octet
    *      0101 0101 &gt;&gt; 1 => 0010 1010
    * </pre></tt>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)7);
    *    SlipDatum  X(UCHAR)2);
    *    SlipDatum  tmp= Y &gt;&gt; X;                // SlipDatum((LONG)1)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y &gt;&gt; X);           // new SlipDatum((LONG)1)
    * </pre></tt>
    *
    * @param[in] X (UCHAR) number of bits to shift right
    * @return LONG result of Y &gt;&gt; X.
    */

   /**
    * @fn SlipDatum::operator&(const UCHAR  X)
    * @brief And Y with X (<b>Y &amp; X</b>) and return a temporary object.
    * <p>
    * Signed and unsigned data are treated identically. There is no
    * significance given to the sign bit.</p>
    * <p>The input (UCHAR)X is widened to a (ULONG).</p>.
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a  SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>The bitwise and truth table is:</p>
    * <center><b>&amp; Truth Table</b></center>
    * <center>
    * |  & | 0  | 1  |
    * | :: | :: | :: |
    * |  0 | 0  | 0  |
    * |  1 | 0  | 1  |
    * </center>
    * <br />
    * <p>Some examples:</p>
    * <center><b>X &amp; Y</b></center></p>
    * <center>
    * | BitField  | BitField  |   Result  |
    * | --------- | --------- | --------- |
    * | 0101 0101 | 0101 0101 | 0101 0101 |
    * | 1010 1010 | 1100 0000 | 0100 0000 |
    * | 0010 1010 | 1110 1010 | 0010 1010 |
    * </center>
    * <br />
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  X((UCHAR)2);
    *    SlipDatum  tmp= Y &amp; X;                 // SlipDatum((LONG)2)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y &amp; X);           // new SlipDatum((LONG)2)
    * </pre></tt>
    *
    * @param[in] X (UCHAR) value to be logically anded
    * @return (LONG) result of X &amp; Y
    */

   /**
    * @fn SlipDatum::operator|(const UCHAR  X)
    * @brief Or Y with X (<b>Y \| X</b>) and return a temporary object.
    * <p>
    * Signed and unsigned data are treated identically. There is no
    * significance given to the sign bit.</p>
    * <p>The input (UCHAR)X is widened to a (ULONG).</p>.
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a  SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>The bitwise or truth table is:</p>
    * <center><b>&amp; Truth Table</b></center>
    * <center>
    * |  \| | 0  | 1  |
    * | :: | :: | :: |
    * |  0 | 0  | 1  |
    * |  1 | 1  | 1  |
    * </center>
    * <br />
    * <p>Some examples:</p>
    * <center><b>X \| Y</b></center></p>
    * <center>
    * | BitField  | BitField  |   Result  |
    * | --------- | --------- | --------- |
    * | 0101 0101 | 0101 0101 | 0101 0101 |
    * | 1010 1010 | 1100 0000 | 1110 1010 |
    * | 0010 1010 | 1110 1010 | 1110 1010 |
    * </center>
    * <br />
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  X((UCHAR)2);
    *    SlipDatum  tmp= Y \| X;                 // SlipDatum((LONG)3)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y \| X);           // new SlipDatum((LONG)3)
    * </pre></tt>
    *
    * @param[in] X (UCHAR) value to be logically or'd
    * @return (LONG) result of X \| Y
    */

   /**
    * @fn SlipDatum::operator^(const UCHAR  X)
    * @brief Exclusive Y with X (<b>Y ^ X</b>) and return a temporary object.
    * <p>
    * Signed and unsigned data are treated identically. There is no
    * significance given to the sign bit.</p>
    * <p>The input (UCHAR)X is widened to a (ULONG).</p>.
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a  SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>The bitwise or truth table is:</p>
    * <center><b>&amp; Truth Table</b></center>
    * <center>
    * |  ^ | 0  | 1  |
    * | :: | :: | :: |
    * |  0 | 0  | 1  |
    * |  1 | 1  | 0  |
    * </center>
    * <br />
    * <p>Some examples:</p>
    * <center><b>X &amp; Y</b></center></p>
    * <center>
    * | BitField  | BitField  |   Result  |
    * | --------- | --------- | --------- |
    * | 0101 0101 | 0101 0101 | 0000 0000 |
    * | 1010 1010 | 1100 0000 | 0110 1010 |
    * | 0010 1010 | 1110 1010 | 1100 0000 |
    * </center>
    * <br />
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  X((UCHAR)2);
    *    SlipDatum  tmp= Y ^ X;                 // SlipDatum((LONG)1)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y ^ X);           // new SlipDatum((LONG)1)
    * </pre></tt>
    *
    * @param[in] X (UCHAR) value to be logically exclusive or'd
    * @return (LONG) result of X ^ Y
    */

   /**
    * @fn SlipDatum::operator<<(const CHAR  X)
    * @brief Left shift <b>Y &lt;&lt; X</b> and return its value.
    * <p>Left shift Y by X bits. The original value of X and Y is unchanged.</p>
    * <p>The input (CHAR)X is widened to a (LONG).</p>.
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>
    * Left shifts treat the Y data type as unsigned, either UCHAR or ULONG.
    * The sign bit (the most significant bit (MSB) has no special meaning.
    * A left shift shifts all bits in Y one bit to the left and inserts a
    * zero (0) in place of the rightmost bit for each bit shifted. The
    * leftmost bit is shifted out for all X values &gt; 0.</p>
    * <p>For example:</p>
    * <p>
    * <tt><pre>
    *               8 bit octet
    *      0101 0101 &lt;&lt; 1 => 1010 1010
    * </pre></tt>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)7);
    *    SlipDatum  X(CHAR)2);
    *    SlipDatum  tmp= Y &lt;&lt; X;               // SlipDatum((LONG)28)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y &lt;&lt; X);           // new SlipDatum((LONG)28)
    * </pre></tt>
    *
    * @param[in] X (CHAR) number of bits to shift left
    * @return LONG result of Y &lt;&lt; X.
    */

   /**
    * @fn SlipDatum::operator>>(const CHAR  X)
    * @brief Right shift <b>Y &gt;&gt; X</b> and return its value.
    * <p>Right shift Y by X bits. The original value of X and Y is unchanged.</p>
    * <p>The input (CHAR)X is widened to a (LONG).</p>.
    * <p>Right shifts treat the Y data type as unsigned, either UCHAR or ULONG.
    *    The sign bit (the most significant bit (MSB) has no special meaning.
    *    A right shift shifts all bits in Y to the right and inserts a zero (0)
    *    in the MSB for each bit shifted. The rightmost bit is shifted out for
    *    all X values &gt; sizeof(ULONG).</p>
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>For example:</p>
    * <p>
    * <tt><pre>
    *               8 bit octet
    *      0101 0101 &gt;&gt; 1 => 0010 1010
    * </pre></tt>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)7);
    *    SlipDatum  X(CHAR)2);
    *    SlipDatum  tmp= Y &gt;&gt; X;                // SlipDatum((LONG)1)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y &gt;&gt; X);           // new SlipDatum((LONG)1)
    * </pre></tt>
    *
    * @param[in] X (CHAR) number of bits to shift right
    * @return LONG result of Y &gt;&gt; X.
    */

   /**
    * @fn SlipDatum::operator&(const CHAR   X)
    * @brief And Y with X (<b>Y &amp; X</b>) and return a temporary object.
    * <p>
    * Signed and unsigned data are treated identically. There is no
    * significance given to the sign bit.</p>
    * <p>The input (CHAR)X is widened to a (LONG).</p>.
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>The bitwise and truth table is:</p>
    * <center><b>&amp; Truth Table</b></center>
    * <center>
    * |  & | 0  | 1  |
    * | :: | :: | :: |
    * |  0 | 0  | 0  |
    * |  1 | 0  | 1  |
    * </center>
    * <br />
    * <p>Some examples:</p>
    * <center><b>X &amp; Y</b></center></p>
    * <center>
    * | BitField  | BitField  |   Result  |
    * | --------- | --------- | --------- |
    * | 0101 0101 | 0101 0101 | 0101 0101 |
    * | 1010 1010 | 1100 0000 | 0100 0000 |
    * | 0010 1010 | 1110 1010 | 0010 1010 |
    * </center>
    * <br />
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  X((CHAR)2);
    *    SlipDatum  tmp= Y &amp; X;                 // SlipDatum((LONG)2)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y &amp; X);           // new SlipDatum((LONG)2)
    * </pre></tt>
    *
    * @param[in] X (CHAR) value to be logically anded
    * @return (LONG) result of X &amp; Y
    */

   /**
    * @fn SlipDatum::operator|(const CHAR   X)
    * @brief Or Y with X (<b>Y \| X</b>) and return a temporary object.
    * <p>
    * Signed and unsigned data are treated identically. There is no
    * significance given to the sign bit.</p>
    * <p>The input (CHAR)X is widened to a (LONG).</p>.
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>The bitwise or truth table is:</p>
    * <center><b>&amp; Truth Table</b></center>
    * <center>
    * |  \| | 0  | 1  |
    * | :: | :: | :: |
    * |  0 | 0  | 1  |
    * |  1 | 1  | 1  |
    * </center>
    * <br />
    * <p>Some examples:</p>
    * <center><b>X \| Y</b></center></p>
    * <center>
    * | BitField  | BitField  |   Result  |
    * | --------- | --------- | --------- |
    * | 0101 0101 | 0101 0101 | 0101 0101 |
    * | 1010 1010 | 1100 0000 | 1110 1010 |
    * | 0010 1010 | 1110 1010 | 1110 1010 |
    * </center>
    * <br />
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  X((CHAR)2);
    *    SlipDatum  tmp= Y \| X;                 // SlipDatum((LONG)3)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y \| X);           // new SlipDatum((LONG)3)
    * </pre></tt>
    *
    * @param[in] X (CHAR) value to be logically or'd
    * @return (LONG) result of X \| Y
    */

   /**
    * @fn SlipDatum::operator^(const CHAR   X)
    * @brief Exclusive Y with X (<b>Y ^ X</b>) and return a temporary object.
    * <p>
    * Signed and unsigned data are treated identically. There is no
    * significance given to the sign bit.</p>
    * <p>The input (CHAR)X is widened to a (LONG).</p>.
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>The bitwise or truth table is:</p>
    * <center><b>&amp; Truth Table</b></center>
    * <center>
    * |  ^ | 0  | 1  |
    * | :: | :: | :: |
    * |  0 | 0  | 1  |
    * |  1 | 1  | 0  |
    * </center>
    * <br />
    * <p>Some examples:</p>
    * <center><b>X &amp; Y</b></center></p>
    * <center>
    * | BitField  | BitField  |   Result  |
    * | --------- | --------- | --------- |
    * | 0101 0101 | 0101 0101 | 0000 0000 |
    * | 1010 1010 | 1100 0000 | 0110 1010 |
    * | 0010 1010 | 1110 1010 | 1100 0000 |
    * </center>
    * <br />
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  X((CHAR)2);
    *    SlipDatum  tmp= Y ^ X;                 // SlipDatum((LONG)1)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y ^ X);           // new SlipDatum((LONG)1)
    * </pre></tt>
    *
    * @param[in] X (CHAR) value to be logically exclusive or'd
    * @return (LONG) result of X ^ Y
    */

   /**
    * @fn SlipDatum::operator<<(const ULONG X)
    * @brief Left shift <b>Y &lt;&lt; X</b> and return its value.
    * <p>Left shift Y by X bits. The original value of X and Y is unchanged.</p>
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>
    * Left shifts treat the Y data type as unsigned, either UCHAR or ULONG.
    * The sign bit (the most significant bit (MSB) has no special meaning.
    * A left shift shifts all bits in Y one bit to the left and inserts a
    * zero (0) in place of the rightmost bit for each bit shifted. The
    * leftmost bit is shifted out for all X values &gt; 0.</p>
    * <p>For example:</p>
    * <p>
    * <tt><pre>
    *               8 bit octet
    *      0101 0101 &lt;&lt; 1 => 1010 1010
    * </pre></tt>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)7);
    *    SlipDatum  X(ULONG)2);
    *    SlipDatum  tmp= Y &lt;&lt; X;               // SlipDatum((LONG)28)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y &lt;&lt; X);           // new SlipDatum((LONG)28)
    * </pre></tt>
    *
    * @param[in] X (ULONG) number of bits to shift left
    * @return LONG result of Y &lt;&lt; X.
    */

   /**
    * @fn SlipDatum::operator>>(const ULONG X)
    * @brief Right shift <b>Y &gt;&gt; X</b> and return its value.
    * <p>Right shift Y by X bits. The original value of X and Y is unchanged.</p>
    * <p>Right shifts treat the Y data type as unsigned, either UCHAR or ULONG.
    *    The sign bit (the most significant bit (MSB) has no special meaning.
    *    A right shift shifts all bits in Y to the right and inserts a zero (0)
    *    in the MSB for each bit shifted. The rightmost bit is shifted out for
    *    all X values &gt; sizeof(ULONG).</p>
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>For example:</p>
    * <p>
    * <tt><pre>
    *               8 bit octet
    *      0101 0101 &gt;&gt; 1 => 0010 1010
    * </pre></tt>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)7);
    *    SlipDatum  X(ULONG)2);
    *    SlipDatum  tmp= Y &gt;&gt; X;                // SlipDatum((LONG)1)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y &gt;&gt; X);           // new SlipDatum((LONG)1)
    * </pre></tt>
    *
    * @param[in] X (ULONG) number of bits to shift right
    * @return LONG result of Y &gt;&gt; X.
    */

   /**
    * @fn SlipDatum::operator&(const ULONG  X)
    * @brief And Y with X (<b>Y &amp; X</b>) and return a temporary object.
    * <p>
    * Signed and unsigned data are treated identically. There is no
    * significance given to the sign bit.</p>
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>The bitwise and truth table is:</p>
    * <center><b>&amp; Truth Table</b></center>
    * <center>
    * |  & | 0  | 1  |
    * | :: | :: | :: |
    * |  0 | 0  | 0  |
    * |  1 | 0  | 1  |
    * </center>
    * <br />
    * <p>Some examples:</p>
    * <center><b>X &amp; Y</b></center></p>
    * <center>
    * | BitField  | BitField  |   Result  |
    * | --------- | --------- | --------- |
    * | 0101 0101 | 0101 0101 | 0101 0101 |
    * | 1010 1010 | 1100 0000 | 0100 0000 |
    * | 0010 1010 | 1110 1010 | 0010 1010 |
    * </center>
    * <br />
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  X((ULONG)2);
    *    SlipDatum  tmp= Y &amp; X;                 // SlipDatum((LONG)2)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y &amp; X);           // new SlipDatum((LONG)2)
    * </pre></tt>
    *
    * @param[in] X (ULONG) value to be logically anded
    * @return (LONG) result of X &amp; Y
    */

   /**
    * @fn SlipDatum::operator|(const ULONG  X)
    * @brief Or Y with X (<b>Y \| X</b>) and return a temporary object.
    * <p>
    * Signed and unsigned data are treated identically. There is no
    * significance given to the sign bit.</p>
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>The bitwise or truth table is:</p>
    * <center><b>&amp; Truth Table</b></center>
    * <center>
    * |  \| | 0  | 1  |
    * | :: | :: | :: |
    * |  0 | 0  | 1  |
    * |  1 | 1  | 1  |
    * </center>
    * <br />
    * <p>Some examples:</p>
    * <center><b>X \| Y</b></center></p>
    * <center>
    * | BitField  | BitField  |   Result  |
    * | --------- | --------- | --------- |
    * | 0101 0101 | 0101 0101 | 0101 0101 |
    * | 1010 1010 | 1100 0000 | 1110 1010 |
    * | 0010 1010 | 1110 1010 | 1110 1010 |
    * </center>
    * <br />
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  X((ULONG)2);
    *    SlipDatum  tmp= Y \| X;                 // SlipDatum((LONG)3)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y \| X);           // new SlipDatum((LONG)3)
    * </pre></tt>
    *
    * @param[in] X (ULONG) value to be logically or'd
    * @return (LONG) result of X \| Y
    */

   /**
    * @fn SlipDatum::operator^(const ULONG  X)
    * @brief Exclusive Y with X (<b>Y ^ X</b>) and return a temporary object.
    * <p>
    * Signed and unsigned data are treated identically. There is no
    * significance given to the sign bit.</p>
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>The bitwise or truth table is:</p>
    * <center><b>&amp; Truth Table</b></center>
    * <center>
    * |  ^ | 0  | 1  |
    * | :: | :: | :: |
    * |  0 | 0  | 1  |
    * |  1 | 1  | 0  |
    * </center>
    * <br />
    * <p>Some examples:</p>
    * <center><b>X &amp; Y</b></center></p>
    * <center>
    * | BitField  | BitField  |   Result  |
    * | --------- | --------- | --------- |
    * | 0101 0101 | 0101 0101 | 0000 0000 |
    * | 1010 1010 | 1100 0000 | 0110 1010 |
    * | 0010 1010 | 1110 1010 | 1100 0000 |
    * </center>
    * <br />
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  X((ULONG)2);
    *    SlipDatum  tmp= Y ^ X;                 // SlipDatum((LONG)1)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y ^ X);           // new SlipDatum((LONG)1)
    * </pre></tt>
    *
    * @param[in] X (ULONG) value to be logically exclusive or'd
    * @return (LONG) result of X ^ Y
    */

   /**
    * @fn SlipDatum::operator<<(const LONG  X)
    * @brief Left shift <b>Y &lt;&lt; X</b> and return its value.
    * <p>Left shift Y by X bits. The original value of X and Y is unchanged.</p>
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>
    * Left shifts treat the Y data type as unsigned, either UCHAR or ULONG.
    * The sign bit (the most significant bit (MSB) has no special meaning.
    * A left shift shifts all bits in Y one bit to the left and inserts a
    * zero (0) in place of the rightmost bit for each bit shifted. The
    * leftmost bit is shifted out for all X values &gt; 0.</p>
    * <p>For example:</p>
    * <p>
    * <tt><pre>
    *               8 bit octet
    *      0101 0101 &lt;&lt; 1 => 1010 1010
    * </pre></tt>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)7);
    *    SlipDatum  X(LONG)2);
    *    SlipDatum  tmp= Y &lt;&lt; X;               // SlipDatum((LONG)28)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y &lt;&lt; X);           // new SlipDatum((LONG)28)
    * </pre></tt>
    *
    * @param[in] X (LONG) number of bits to shift left
    * @return LONG result of Y &lt;&lt; X.
    */

   /**
    * @fn SlipDatum::operator>>(const LONG  X)
    * @brief Right shift <b>Y &gt;&gt; X</b> and return its value.
    * <p>Right shift Y by X bits. The original value of X and Y is unchanged.</p>
    * <p>Right shifts treat the Y data type as unsigned, either UCHAR or ULONG.
    *    The sign bit (the most significant bit (MSB) has no special meaning.
    *    A right shift shifts all bits in Y to the right and inserts a zero (0)
    *    in the MSB for each bit shifted. The rightmost bit is shifted out for
    *    all X values &gt; sizeof(ULONG).</p>
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>For example:</p>
    * <p>
    * <tt><pre>
    *               8 bit octet
    *      0101 0101 &gt;&gt; 1 => 0010 1010
    * </pre></tt>
    * <p>A computation can be used in the list functions <em>replace</em> or
    *    <em>insert</em> or the header functions <em>enqueue</em> or
    *    <em>push</em>. A copy of the generated result object will be into the
    *    list.</p>
    * <tt><pre>
    *    SlipDatum  Y((LONG)7);
    *    SlipDatum  X(LONG)2);
    *    SlipDatum  tmp= Y &gt;&gt; X;                // SlipDatum((LONG)1)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y &gt;&gt; X);           // new SlipDatum((LONG)1)
    * </pre></tt>
    *
    * @param[in] X (LONG) number of bits to shift right
    * @return LONG result of Y &gt;&gt; X.
    */

   /**
    * @fn SlipDatum::operator&(const LONG   X)
    * @brief And Y with X (<b>Y &amp; X</b>) and return a temporary object.
    * <p>
    * Signed and unsigned data are treated identically. There is no
    * significance given to the sign bit.</p>
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>The bitwise and truth table is:</p>
    * <center><b>&amp; Truth Table</b></center>
    * <center>
    * |  & | 0  | 1  |
    * | :: | :: | :: |
    * |  0 | 0  | 0  |
    * |  1 | 0  | 1  |
    * </center>
    * <br />
    * <p>Some examples:</p>
    * <center><b>X &amp; Y</b></center></p>
    * <center>
    * | BitField  | BitField  |   Result  |
    * | --------- | --------- | --------- |
    * | 0101 0101 | 0101 0101 | 0101 0101 |
    * | 1010 1010 | 1100 0000 | 0100 0000 |
    * | 0010 1010 | 1110 1010 | 0010 1010 |
    * </center>
    * <br />
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  X((LONG)2);
    *    SlipDatum  tmp= Y &amp; X;                 // SlipDatum((LONG)2)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y &amp; X);           // new SlipDatum((LONG)2)
    * </pre></tt>
    *
    * @param[in] X (LONG) value to be logically anded
    * @return (LONG) result of X &amp; Y
    */

   /**
    * @fn SlipDatum::operator|(const LONG   X)
    * @brief Or Y with X (<b>Y \| X</b>) and return a temporary object.
    * <p>
    * Signed and unsigned data are treated identically. There is no
    * significance given to the sign bit.</p>
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>The bitwise or truth table is:</p>
    * <center><b>&amp; Truth Table</b></center>
    * <center>
    * |  \| | 0  | 1  |
    * | :: | :: | :: |
    * |  0 | 0  | 1  |
    * |  1 | 1  | 1  |
    * </center>
    * <br />
    * <p>Some examples:</p>
    * <center><b>X \| Y</b></center></p>
    * <center>
    * | BitField  | BitField  |   Result  |
    * | --------- | --------- | --------- |
    * | 0101 0101 | 0101 0101 | 0101 0101 |
    * | 1010 1010 | 1100 0000 | 1110 1010 |
    * | 0010 1010 | 1110 1010 | 1110 1010 |
    * </center>
    * <br />
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  X((LONG)2);
    *    SlipDatum  tmp= Y \| X;                 // SlipDatum((LONG)3)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y \| X);           // new SlipDatum((LONG)3)
    * </pre></tt>
    *
    * @param[in] X (LONG) value to be logically or'd
    * @return (LONG) result of X \| Y
    */

   /**
    * @fn SlipDatum::operator^(const LONG   X)
    * @brief Exclusive Y with X (<b>Y ^ X</b>) and return a temporary object.
    * <p>
    * Signed and unsigned data are treated identically. There is no
    * significance given to the sign bit.</p>
    * <p>The value of the SlipDatum objects remain unchanged. The result is
    *    returned on the runtime stack. Insertion of the result to a list
    *    causes a new SlipDatum object to be created and inserted. Replacement
    *    of an existing item on a list causes a new SlipDatum object to be
    *    created to replace the existing list object. For a SlipDatum replace,
    *    an assignment is performed (like-to-like). Assignment of the
    *    result to a SlipDatum object causes the type (LONG) and value to
    *    replace the existing type and value.</p>
    * <p>The bitwise or truth table is:</p>
    * <center><b>&amp; Truth Table</b></center>
    * <center>
    * |  ^ | 0  | 1  |
    * | :: | :: | :: |
    * |  0 | 0  | 1  |
    * |  1 | 1  | 0  |
    * </center>
    * <br />
    * <p>Some examples:</p>
    * <center><b>X &amp; Y</b></center></p>
    * <center>
    * | BitField  | BitField  |   Result  |
    * | --------- | --------- | --------- |
    * | 0101 0101 | 0101 0101 | 0000 0000 |
    * | 1010 1010 | 1100 0000 | 0110 1010 |
    * | 0010 1010 | 1110 1010 | 1100 0000 |
    * </center>
    * <br />
    * <tt><pre>
    *    SlipDatum  Y((LONG)3);
    *    SlipDatum  X((LONG)2);
    *    SlipDatum  tmp= Y ^ X;                 // SlipDatum((LONG)1)
    *    SlipHeader head();                     // create a list
    *    SlipDatum& list = head.enqueue(tmp);   // copy tmp
    *    SlipDatum& head.push(Y ^ X);           // new SlipDatum((LONG)1)
    * </pre></tt>
    *
    * @param[in] X (LONG) value to be logically exclusive or'd
    * @return (LONG) result of X ^ Y
    */

       /*************************************************
        *             Assignment operators              *
       *************************************************/
   /**
    * @fn SlipDatum::operator=(const SlipDatum& X)
    * @brief Assign (<b>Y = X</b>).
    * <p>For purposes of this section, <b>Y = X</b> refers to the left hand
    *    side (LHS) being replaced by the right hand side (RHS) of the
    *    assignment symbol (<b>=</b>). The LHS refers to the current object
    *    and the RHS refers to the input object.</p>
    * <p>Assignments are class invariant. Assignments do not preserve type.
    *    Under assignment the LHS <em>datum</em> field will be
    *    changed according to the RHS <em>datum</em> field and the LHS type
    *    field will be changed to correspond to the RHS.</p>
    * <p>When the assignment is complete, the RHS will not have changed and
    *    the LHS type and value will be the same as the RHS. LHS == RHS will
    *    be <b>true</b>.
    * <p>If the LHS is a SlipPointer object (PTR or STRING) then the object
    *    pointed to must be deleted (for PTR if it is a user defined object and
    *    for STRING if it is a string). This decision is deferred to the
    *    SlipPointer <em>remove()</em> method. If the object is treated as a
    *    constant in SLIP, then <em>remove</em> does nothing. If the object is
    *    not a constant, the <em>remove()</em> deletes it. <em>remove()</em> is a
    *    proxy for <em>delete (void*)</em> within the context of SlipPointer.<p>
    * <p>If the RHS is a SlipPointer object then the object pointed to must
    *    be copied (for PTR it is a user defined object and for STRING it is
    *    a string). If the SlipPointer object is constant within SLIP, the
    *    the copy copies the pointer to the SlipPointer object. If the
    *    SlipPointer object is not a constant within SLIP, then the copy
    *    operation creates a new SlipPointer object. The result of the copy
    *    is inserted into the LHS <em>datum</em> field. This decision is
    *    deferred to the SlipPointer <em>copy()</em> method.</p>
    * @param[in] X (SlipDatum&) the right hand side of the assignment
    * @return SlipDatum current object with X assigned
    */

   /**
    * @fn SlipDatum::operator=(const SlipCell& X)
    * @brief Assign (<b>Y = X</b>).
    * <p>For purposes of this section, <b>Y = X</b> refers to the left hand
    *    side (LHS) being replaced by the right hand side (RHS) of the
    *    assignment symbol (<b>=</b>). The LHS refers to the current object
    *    and the RHS refers to the input object.</p>
    * <p>Assignments are class invariant. Assignments do not preserve type.
    *    Under assignment the LHS <em>datum</em> field will be
    *    changed according to the RHS <em>datum</em> field and the LHS type
    *    field will be changed to correspond to the RHS.</p>
    * <p>Passing a SlipHeader or SlipSublist object will generate an error.
    *    Supporting an assignment of SlipHeader or SlipSublist requires a
    *    change of class and is unsupported. The <em>replace</em>method must
    *    be used when this is required.</p>
    * <p>When the assignment is complete, the RHS will not have changed and
    *    the LHS type and value will be the same as the RHS. LHS == RHS will
    *    be <b>true</b>.
    * <p>If the LHS is a SlipPointer object (PTR or STRING) then the object
    *    pointed to must be deleted (for PTR if it is a user defined object and
    *    for STRING if it is a string). This decision is deferred to the
    *    SlipPointer <em>remove()</em> method. If the object is treated as a
    *    constant in SLIP, then <em>remove</em> does nothing. If the object is
    *    not a constant, the <em>remove()</em> deletes it. <em>remove()</em> is a
    *    proxy for <em>delete (void*)</em> within the context of SlipPointer.<p>
    * <p>If the RHS is a SlipPointer object then the object pointed to must
    *    be copied (for PTR it is a user defined object and for STRING it is
    *    a string). If the SlipPointer object is constant within SLIP, the
    *    the copy copies the pointer to the SlipPointer object. If the
    *    SlipPointer object is not a constant within SLIP, then the copy
    *    operation creates a new SlipPointer object. The result of the copy
    *    is inserted into the LHS <em>datum</em> field. This decision is
    *    deferred to the SlipPointer <em>copy()</em> method.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2016</b> Sublist not allowed.  Use Replace.</li>
    *    <li><b>E3015</b> SlipHeader must be assigned to a SlipHeader. </li>
    * </ul>
    * @param[in] X (SlipCell&) the right hand side of the assignment
    * @return SlipDatum current object with X assigned
    */
   SlipCell&  SlipDatum::operator=(const SlipCell& X)  {                        // Y = V
       bool flag = X.isData();
       if (flag) {
         assignPreamble();
         assignEpilogue((SlipDatum&)X);
       } else if (X.isHeader()) {
          postError(__FILE__, __LINE__, SlipErr::E3015, "SlipDatum=", "", "", *this, X);
       } else {
          postError(__FILE__, __LINE__, SlipErr::E2016, "SlipDatum=", "", ". Use replace", *this, X);
       }
       return *(SlipCell*)this;
   }; // SlipCell&  SlipDatum::operator=(SlipCell& X)

   /**
    * @fn SlipDatum::operator=(const DOUBLE X)
    * @brief Assign (<b>Y = X</b>).
    * <p>For purposes of this section, <b>Y = X</b> refers to the left hand
    *    side (LHS) being replaced by the right hand side (RHS) of the
    *    assignment symbol (<b>=</b>). The LHS refers to the current object
    *    and the RHS refers to the input object.</p>
    * <p>Assignments are class invariant. Assignments do not preserve type.
    *    Under assignment the LHS <em>datum</em> field will be
    *    changed according to the RHS <em>datum</em> field and the LHS type
    *    field will be changed to correspond to the RHS.</p>
    * <p>When the assignment is complete, the RHS will not have changed and
    *    the LHS type will be DOUBLE and the value will be the same as the RHS.
    *    LHS == RHS will be <b>true</b>.
    * <p>If the LHS is a SlipPointer object (PTR or STRING) then the object
    *    pointed to must be deleted (for PTR if it is a user defined object and
    *    for STRING if it is a string). This decision is deferred to the
    *    SlipPointer <em>remove()</em> method. If the object is treated as a
    *    constant in SLIP, then <em>remove</em> does nothing. If the object is
    *    not a constant, the <em>remove()</em> deletes it. <em>remove()</em> is a
    *    proxy for <em>delete (void*)</em> within the context of SlipPointer.<p>
    * @param[in] X (DOUBLE) the right hand side of the assignment
    * @return SlipDatum current object with X assigned
    */
   SlipDatum& SlipDatum::operator=(const DOUBLE X)     { assignPreamble(); setOperator((void*)doubleOP); *getDouble() = X; return *this; }    // Y = V

   /**
    * @fn SlipDatum::operator=(const bool   X)
    * @brief Assign (<b>Y = X</b>).
    * <p>For purposes of this section, <b>Y = X</b> refers to the left hand
    *    side (LHS) being replaced by the right hand side (RHS) of the
    *    assignment symbol (<b>=</b>). The LHS refers to the current object
    *    and the RHS refers to the input object.</p>
    * <p>Assignments are class invariant. Assignments do not preserve type.
    *    Under assignment the LHS <em>datum</em> field will be
    *    changed according to the RHS <em>datum</em> field and the LHS type
    *    field will be changed to correspond to the RHS.</p>
    * <p>When the assignment is complete, the RHS will not have changed and
    *    the LHS type will be bool and the value will be the same as the RHS.
    *    LHS == RHS will be <b>true</b>.
    * <p>If the LHS is a SlipPointer object (PTR or STRING) then the object
    *    pointed to must be deleted (for PTR if it is a user defined object and
    *    for STRING if it is a string). This decision is deferred to the
    *    SlipPointer <em>remove()</em> method. If the object is treated as a
    *    constant in SLIP, then <em>remove</em> does nothing. If the object is
    *    not a constant, the <em>remove()</em> deletes it. <em>remove()</em> is a
    *    proxy for <em>delete (void*)</em> within the context of SlipPointer.<p>
    * @param[in] X (bool) the right hand side of the assignment
    * @return SlipDatum current object with X assigned
    */
   SlipDatum& SlipDatum::operator=(const bool   X)     { assignPreamble(); setOperator((void*)boolOP);   *getBool()   = X; return *this; }

   /**
    * @brief Assign (<b>Y = X</b>).
    * <p>For purposes of this section, <b>Y = X</b> refers to the left hand
    *    side (LHS) being replaced by the right hand side (RHS) of the
    *    assignment symbol (<b>=</b>). The LHS refers to the current object
    *    and the RHS refers to the input object.</p>
    * <p>Assignments are class invariant. Assignments do not preserve type.
    *    Under assignment the LHS <em>datum</em> field will be
    *    changed according to the RHS <em>datum</em> field and the LHS type
    *    field will be changed to correspond to the RHS.</p>
    * <p>When the assignment is complete, the RHS will not have changed and
    *    the LHS type will be UCHAR and the value will be the same as the RHS.
    *    LHS == RHS will be <b>true</b>.
    * <p>If the LHS is a SlipPointer object (PTR or STRING) then the object
    *    pointed to must be deleted (for PTR if it is a user defined object and
    *    for STRING if it is a string). This decision is deferred to the
    *    SlipPointer <em>remove()</em> method. If the object is treated as a
    *    constant in SLIP, then <em>remove</em> does nothing. If the object is
    *    not a constant, the <em>remove()</em> deletes it. <em>remove()</em> is a
    *    proxy for <em>delete (void*)</em> within the context of SlipPointer.<p>
    * @param[in] X (UCHAR) the right hand side of the assignment
    * @return SlipDatum current object with X assigned
    */
   SlipDatum& SlipDatum::operator=(const UCHAR  X)     { assignPreamble(); setOperator((void*)ucharOP);  *getUChar()  = X; return *this; }

   /**
    * @brief Assign (<b>Y = X</b>).
    * <p>For purposes of this section, <b>Y = X</b> refers to the left hand
    *    side (LHS) being replaced by the right hand side (RHS) of the
    *    assignment symbol (<b>=</b>). The LHS refers to the current object
    *    and the RHS refers to the input object.</p>
    * <p>Assignments are class invariant. Assignments do not preserve type.
    *    Under assignment the LHS <em>datum</em> field will be
    *    changed according to the RHS <em>datum</em> field and the LHS type
    *    field will be changed to correspond to the RHS.</p>
    * <p>When the assignment is complete, the RHS will not have changed and
    *    the LHS type will be CHAR and the value will be the same as the RHS.
    *    LHS == RHS will be <b>true</b>.
    * <p>If the LHS is a SlipPointer object (PTR or STRING) then the object
    *    pointed to must be deleted (for PTR if it is a user defined object and
    *    for STRING if it is a string). This decision is deferred to the
    *    SlipPointer <em>remove()</em> method. If the object is treated as a
    *    constant in SLIP, then <em>remove</em> does nothing. If the object is
    *    not a constant, the <em>remove()</em> deletes it. <em>remove()</em> is a
    *    proxy for <em>delete (void*)</em> within the context of SlipPointer.<p>
    * @param[in] X (CHAR) the right hand side of the assignment
    * @return SlipDatum current object with X assigned
    */
   SlipDatum& SlipDatum::operator=(const CHAR   X)     { assignPreamble(); setOperator((void*)charOP);   *getChar()   = X; return *this; }

   /**
    * @brief Assign (<b>Y = X</b>).
    * <p>For purposes of this section, <b>Y = X</b> refers to the left hand
    *    side (LHS) being replaced by the right hand side (RHS) of the
    *    assignment symbol (<b>=</b>). The LHS refers to the current object
    *    and the RHS refers to the input object.</p>
    * <p>Assignments are class invariant. Assignments do not preserve type.
    *    Under assignment the LHS <em>datum</em> field will be
    *    changed according to the RHS <em>datum</em> field and the LHS type
    *    field will be changed to correspond to the RHS.</p>
    * <p>When the assignment is complete, the RHS will not have changed and
    *    the LHS type will be ULONG and the value will be the same as the RHS.
    *    LHS == RHS will be <b>true</b>.
    * <p>If the LHS is a SlipPointer object (PTR or STRING) then the object
    *    pointed to must be deleted (for PTR if it is a user defined object and
    *    for STRING if it is a string). This decision is deferred to the
    *    SlipPointer <em>remove()</em> method. If the object is treated as a
    *    constant in SLIP, then <em>remove</em> does nothing. If the object is
    *    not a constant, the <em>remove()</em> deletes it. <em>remove()</em> is a
    *    proxy for <em>delete (void*)</em> within the context of SlipPointer.<p>
    * @param[in] X (ULONG) the right hand side of the assignment
    * @return SlipDatum current object with X assigned
    */
   SlipDatum& SlipDatum::operator=(const ULONG  X)     { assignPreamble(); setOperator((void*)ulongOP);  *getULong()  = X; return *this; }

   /**
    * @brief Assign (<b>Y = X</b>).
    * <p>For purposes of this section, <b>Y = X</b> refers to the left hand
    *    side (LHS) being replaced by the right hand side (RHS) of the
    *    assignment symbol (<b>=</b>). The LHS refers to the current object
    *    and the RHS refers to the input object.</p>
    * <p>Assignments are class invariant. Assignments do not preserve type.
    *    Under assignment the LHS <em>datum</em> field will be
    *    changed according to the RHS <em>datum</em> field and the LHS type
    *    field will be changed to correspond to the RHS.</p>
    * <p>When the assignment is complete, the RHS will not have changed and
    *    the LHS type will be LONG and the value will be the same as the RHS.
    *    LHS == RHS will be <b>true</b>.
    * <p>If the LHS is a SlipPointer object (PTR or STRING) then the object
    *    pointed to must be deleted (for PTR if it is a user defined object and
    *    for STRING if it is a string). This decision is deferred to the
    *    SlipPointer <em>remove()</em> method. If the object is treated as a
    *    constant in SLIP, then <em>remove</em> does nothing. If the object is
    *    not a constant, the <em>remove()</em> deletes it. <em>remove()</em> is a
    *    proxy for <em>delete (void*)</em> within the context of SlipPointer.<p>
    * @param[in] X (LONG) the right hand side of the assignment
    * @return SlipDatum current object with X assigned
    */
   SlipDatum& SlipDatum::operator=(const LONG   X)     { assignPreamble(); setOperator((void*)longOP);   *getLong()   = X; return *this; }

   /**
    * @brief Assign (<b>Y = X</b>).
    * <p>For purposes of this section, <b>Y = X</b> refers to the left hand
    *    side (LHS) being replaced by the right hand side (RHS) of the
    *    assignment symbol (<b>=</b>). The LHS refers to the current object
    *    and the RHS refers to the input object.</p>
    * <p>Assignments are class invariant. Assignments do not preserve type.
    *    Under assignment the LHS <em>datum</em> field will be
    *    changed according to the RHS <em>datum</em> field and the LHS type
    *    field will be changed to correspond to the RHS.</p>
    * <p>When the assignment is complete, the RHS will not have changed and
    *    the LHS type will be PTR and the value will be the same as the RHS.
    *    LHS == RHS will be <b>true</b>.
    * <p>If the LHS is a SlipPointer object (PTR or STRING) then the object
    *    pointed to must be deleted (for PTR if it is a user defined object and
    *    for STRING if it is a string). This decision is deferred to the
    *    SlipPointer <em>remove()</em> method. If the object is treated as a
    *    constant in SLIP, then <em>remove</em> does nothing. If the object is
    *    not a constant, the <em>remove()</em> deletes it. <em>remove()</em> is a
    *    proxy for <em>delete (void*)</em> within the context of SlipPointer.<p>
    * @param[in] X (PTR) the right hand side of the assignment
    * @return SlipDatum current object with X assigned
    */
   SlipDatum& SlipDatum::operator=(const PTR    X)     {
      assignPreamble();
      setOperator((void*)ptrOP);
      *getPtr() = X->copy();
      return *this;   }; //  SlipDatum& SlipDatum::operator=(const PTR X)

   /**
    * @brief Assign (<b>Y = X</b>).
    * <p>For purposes of this section, <b>Y = X</b> refers to the left hand
    *    side (LHS) being replaced by the right hand side (RHS) of the
    *    assignment symbol (<b>=</b>). The LHS refers to the current object
    *    and the RHS refers to the input object.</p>
    * <p>Assignments are class invariant. Assignments do not preserve type.
    *    Under assignment the LHS <em>datum</em> field will be
    *    changed according to the RHS <em>datum</em> field and the LHS type
    *    field will be changed to correspond to the RHS.</p>
    * <p>When the assignment is complete, the RHS will not have changed and
    *    the LHS type will be STRING and the value will be the same as the RHS.
    *    LHS == RHS will be <b>true</b>.
    * <p>If the LHS is a SlipPointer object (PTR or STRING) then the object
    *    pointed to must be deleted (for PTR if it is a user defined object and
    *    for STRING if it is a string). This decision is deferred to the
    *    SlipPointer <em>remove()</em> method. If the object is treated as a
    *    constant in SLIP, then <em>remove</em> does nothing. If the object is
    *    not a constant, the <em>remove()</em> deletes it. <em>remove()</em> is a
    *    proxy for <em>delete (void*)</em> within the context of SlipPointer.<p>
    * @param[in] X (string&) the right hand side of the assignment
    * @return SlipDatum current object with X assigned
    */
   SlipDatum& SlipDatum::operator=(const string& X)    {
      assignPreamble();
      setOperator((void*)stringOP);
      *getString() = new SlipStringNonConst(&X);
      return *this;
   }; // SlipDatum& SlipDatum::operator=(const string& X)

   /**
    * @brief Assign (<b>Y = X</b>).
    * <p>For purposes of this section, <b>Y = X</b> refers to the left hand
    *    side (LHS) being replaced by the right hand side (RHS) of the
    *    assignment symbol (<b>=</b>). The LHS refers to the current object
    *    and the RHS refers to the input object.</p>
    * <p>Assignments are class invariant. Assignments do not preserve type.
    *    Under assignment the LHS <em>datum</em> field will be
    *    changed according to the RHS <em>datum</em> field and the LHS type
    *    field will be changed to correspond to the RHS.</p>
    * <p>When the assignment is complete, the RHS will not have changed and
    *    the LHS type will be STRING and the value will be the same as the RHS.
    *    LHS == RHS will be <b>true</b>.
    * <p>If the LHS is a SlipPointer object (PTR or STRING) then the object
    *    pointed to must be deleted (for PTR if it is a user defined object and
    *    for STRING if it is a string). This decision is deferred to the
    *    SlipPointer <em>remove()</em> method. If the object is treated as a
    *    constant in SLIP, then <em>remove</em> does nothing. If the object is
    *    not a constant, the <em>remove()</em> deletes it. <em>remove()</em> is a
    *    proxy for <em>delete (void*)</em> within the context of SlipPointer.<p>
    * @param[in] X (string*) the right hand side of the assignment
    * @return SlipDatum current object with X assigned
    */
   SlipDatum& SlipDatum::operator=(const string* X)    { return this->operator=(*X); }

       /*************************************************
        *        Compound Assignment operators          *
       *************************************************/

   /**
    * @fn SlipDatum::operator+=(const SlipDatum& X)
    * @brief Add X to Y(<b>Y += X</b>) and assign  the result to Y.
    * <p>The values of Y and X are added and replace Y. X is unchanged
    * and the Y type is invariant.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (SlipDatum&) addend
    * @return (SlipDatum&) current object with X added
    */

   /**
    * @fn SlipDatum::operator-=(const SlipDatum& X)
    * @brief Subtract Y from X (<b>Y -= X</b>)  and assign the result to Y.
    * <p>The value of X is subtracted from the value of Y and the result
    *    replaces Y. X is unchanged and the Y type is invariant.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (SlipDatum&) subtrahend
    * @return (SlipDatum&) current object with X subtracted
    */

   /**
    * @fn SlipDatum::operator*=(const SlipDatum& X)
    * @brief Multiply  X and Y (<b>Y *= X</b>) and assign the result to Y.
    * <p>The value of X and Y are multiplied and the result replaces Y.
    * X is unchanged and the Y type is invariant.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (SlipDatum&) multiplicand
    * @return (SlipDatum&) current object multiplied by X
    */

   /**
    * @fn SlipDatum::operator/=(const SlipDatum& X)
    * @brief Divide  Y by X (<b>Y /= X</b>) and assign the result to Y.
    * <p>The value of Y is divided by the value of X and the result
    *    replaces Y. X is unchanged and the Y type is invariant.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (SlipDatum&) dividend
    * @return (SlipDatum&) current object divided by X
    */

   /**
    * @fn SlipDatum::operator%=(const SlipDatum& X)
    * @brief Take the modulus of Y with respect to X (<b>Y %= X</b>) and assign the result to Y.
    * <p>Take the modulus of the value of Y by the value of X and the
    *    result replaces Y. X is unchanged and the Y type is invariant.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. DOUBLE, STRING and PTR are not compatible.</p>
    *
    * @param[in] X (SlipDatum&) modulus
    * @return (SlipDatum&) result of Y %= X
    */

   /**
    * @fn SlipDatum::operator<<=(const SlipDatum& X)
    * @brief Left shift Y by X (<b>Y &lt;&lt;= X</b>)  and assign the result to Y.
    * <p>The value of Y shifted by the value of X and the result
    *    replaces Y. X is unchanged and the Y type is invariant.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. DOUBLE, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (SlipDatum&) shift amount
    * @return (SlipDatum&) current object shifted by X
    */

   /**
    * Wfn SlipDatum::operator>>=(const SlipDatum& X)
    * @brief Right shift Y by X (<b>Y &gt;&gt;= X</b>)  and assign the result to Y.
    * <p>The value of Y shifted by the value of X and the result
    *    replaces Y. X is unchanged and the Y type is invariant.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. DOUBLE, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (SlipDatum&) shift amount
    * @return (SlipDatum&) current object shifted by X
    */

   /**
    * @fn SlipDatum::operator&=(const SlipDatum& X)
    * @brief And Y and X (<b>Y &amp;= X</b>)  and assign the result to Y.
    * <p>The value of Y is anded with the value of X and the result
    *    replaces Y. X is unchanged and the Y type is invariant.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. DOUBLE, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (SlipDatum&) bit value
    * @return (SlipDatum&) result of Y &amp; X
    */

   /**
    * @fn SlipDatum::operator|=(const SlipDatum& X) 
    * @brief Or Y and X (<b>Y \|= X</b>)  and assign the result to Y.
    * <p>The value of Y is or'd with the value of X and the result
    *    replaces Y. X is unchanged and the Y type is invariant.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. DOUBLE, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (SlipDatum&) bit value
    * @return (SlipDatum&) result of Y \|= X
    */

   /**
    * @fn SlipDatum::operator^=(const SlipDatum& X)
    * @brief Exclusive or Y and X (<b>Y ^= X</b>)  and assign the result to Y.
    * <p>The value of Y is exclusive or'd with the value of X and the result
    *    replaces Y. X is unchanged and the Y type is invariant.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. DOUBLE, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (SlipDatum&) bit value
    * @return (SlipDatum&) result of Y ^= X
    */


   /**
    * @fn SlipDatum::operator+=(const DOUBLE X)
    * @brief Add X to Y(<b>Y += X</b>) and assign  the result to Y.
    * <p>The values of Y and X are added and replace Y. X is unchanged
    * and the Y type is invariant.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (DOUBLE) addend
    * @return (SlipDatum&) current object with X added
    */

   /**
    * @fn SlipDatum::operator-=(const DOUBLE X)
    * @brief Subtract Y from X (<b>Y -= X</b>)  and assign the result to Y.
    * <p>The value of X is subtracted from the value of Y and the result
    *    replaces Y. X is unchanged and the Y type is invariant.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (DOUBLE) subtrahend
    * @return (SlipDatum&) current object with X subtracted
    */

   /**
    * @fn SlipDatum::operator*=(const DOUBLE X)
    * @brief Multiply  X and Y (<b>Y *= X</b>) and assign the result to Y.
    * <p>The value of X and Y are multiplied and the result replaces Y.
    * X is unchanged and the Y type is invariant.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (DOUBLE) multiplicand
    * @return (SlipDatum&) current object multiplied by X
    */

   /**
    * @fn SlipDatum::operator/=(const DOUBLE X)
    * @brief Divide  Y by X (<b>Y /= X</b>) and assign the result to Y.
    * <p>The value of Y is divided by the value of X and the result
    *    replaces Y. X is unchanged and the Y type is invariant.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (DOUBLE) dividend
    * @return (SlipDatum&) current object divided by X
    */


   /**
    * @fn SlipDatum::operator+=(const bool    X)
    * @brief Add X to Y(<b>Y += X</b>) and assign  the result to Y.
    * <p>The values of Y and X are added and replace Y. X is unchanged
    * and the Y type is invariant. A bool type takes on the value
    * '1' for <b>true</b> and '0' for <b>false</b>.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (bool) addend
    * @return (SlipDatum&) current object with X added
    */

   /**
    * @fn SlipDatum::operator-=(const bool    X)
    * @brief Subtract Y from X (<b>Y -= X</b>)  and assign the result to Y.
    * <p>The value of X is subtracted from the value of Y and the result
    *    replaces Y. A bool takes on the value '1' for <b>true</b> and '0'
    *    for <b>false</b>X is unchanged and the Y type is invariant.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (bool) subtrahend
    * @return (SlipDatum&) current object with X subtracted
    */

   /**
    * @fn SlipDatum::operator+=(const UCHAR X)
    * @brief Add X to Y(<b>Y += X</b>) and assign  the result to Y.
    * <p>The values of Y and X are added and replace Y. X is cast
    * to a ULONG before the operation. At operation end, X is
    * unchanged and the Y type is unchanged.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (UCHAR) addend
    * @return (SlipDatum&) current object with X added
    */

   /**
    * @fn SlipDatum::operator-=(const UCHAR X)
    * @brief Subtract Y from X (<b>Y -= X</b>)  and assign the result to Y.
    * <p>The value of X is subtracted from the value of Y and the result
    *    replaces Y. X is cast  to a ULONG before the operation. At
    *    operation end, X is unchanged and the Y type is unchanged.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (UCHAR) subtrahend
    * @return (SlipDatum&) current object with X subtracted
    */

   /**
    * @fn SlipDatum::operator*=(const UCHAR X)
    * @brief Multiply  X and Y (<b>Y *= X</b>) and assign the result to Y.
    * <p>The value of X and Y are multiplied and the result replaces Y.
    *    X is cast  to a ULONG before the operation. At operation end,
    *    X is unchanged and the Y type is unchanged.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (UCHAR) multiplicand
    * @return (SlipDatum&) current object multiplied by X
    */

   /**
    * @fn SlipDatum::operator/=(const UCHAR X)
    * @brief Divide  Y by X (<b>Y /= X</b>) and assign the result to Y.
    * <p>The value of Y is divided by the value of X and the result
    *    replaces Y.  X is cast  to a ULONG before the operation. At
    *     operation end, X is unchanged and the Y type is unchanged.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (UCHAR) dividend
    * @return (SlipDatum&) current object divided by X
    */

   /**
    * @fn SlipDatum::operator%=(const UCHAR X)
    * @brief Take the modulus of Y with respect to X (<b>Y %= X</b>) and assign the result to Y.
    * <p>Take the modulus of the value of Y by the value of X and the
    *    result replaces Y. X is cast to a ULONG before the operation.
    *    At operation end, X is unchanged and the Y type is unchanged.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. UCHAR, STRING and PTR are not compatible.</p>
    *
    * @param[in] X (UCHAR) modulus
    * @return (SlipDatum&) result of Y %= X
    */

   /**
    * @fn SlipDatum::operator<<=(const UCHAR X)
    * @brief Left shift Y by X (<b>Y &lt;&lt;= X</b>)  and assign the result to Y.
    * <p>The value of Y shifted by the value of X and the result
    *    replaces Y. X is cast to a ULONG before the operation. At
    *    operation end, X is unchanged and the Y type is unchanged.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. UCHAR, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (UCHAR) shift amount
    * @return (SlipDatum&) current object shifted by X
    */

   /**
    * @fn SlipDatum::operator>>=(const UCHAR X)
    * @brief Right shift Y by X (<b>Y &gt;&gt;= X</b>)  and assign the result to Y.
    * <p>The value of Y shifted by the value of X and the result
    *    replaces Y. X is cast to a ULONG before the operation. At
    *    operation end, X is unchanged and the Y type is unchanged.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. UCHAR, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (UCHAR) shift amount
    * @return (SlipDatum&) current object shifted by X
    */

   /**
    * @fn SlipDatum::operator&=(const UCHAR X)
    * @brief And Y and X (<b>Y &amp;= X</b>)  and assign the result to Y.
    * <p>The value of Y is anded with the value of X and the result
    *    replaces Y. X is cast to a ULONG before the operation. At
    *    operation end, X is unchanged and the Y type is unchanged.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. UCHAR, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (UCHAR) bit value
    * @return (SlipDatum&) result of Y &amp;= X
    */

   /**
    * @brief Or Y and X (<b>Y \|= X</b>)  and assign the result to Y.
    * <p>The value of Y is or'd with the value of X and the result
    *    replaces Y. X is cast to a ULONG before the operation. At
    *    operation end, X is unchanged and the Y type is unchanged.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. UCHAR, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (UCHAR) bit value
    * @return (SlipDatum&) result of Y \= X
    */

   /**
    * @fn SlipDatum::operator^=(const UCHAR X)
    * @brief Exclusive or Y and X (<b>Y ^= X</b>)  and assign the result to Y.
    * <p>The value of Y is exclusive or'd with the value of X and the result
    *    replaces Y. X is cast to a ULONG before the operation. At operation
    *    end, X is  unchanged and the Y type is unchanged.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. UCHAR, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (UCHAR) bit value
    * @return (SlipDatum&) result of Y ^ X
    */

   /**
    * @fn SlipDatum::operator+=(const CHAR X)
    * @brief Add X to Y(<b>Y += X</b>) and assign  the result to Y.
    * <p>The values of Y and X are added and replace Y. X is cast
    * to a LONG before the operation. At operation end, X is
    * unchanged and the Y type is unchanged.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (CHAR) addend
    * @return (SlipDatum&) current object with X added
    */

   /**
    * @fn SlipDatum::operator-=(const CHAR X)
    * @brief Subtract Y from X (<b>Y -= X</b>)  and assign the result to Y.
    * <p>The value of X is subtracted from the value of Y and the result
    *    replaces Y. X is cast  to a LONG before the operation. At
    *    operation end, X is unchanged and the Y type is unchanged.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (CHAR) subtrahend
    * @return (SlipDatum&) current object with X subtracted
    */

   /**
    * @fn SlipDatum::operator*=(const CHAR X)
    * @brief Multiply  X and Y (<b>Y *= X</b>) and assign the result to Y.
    * <p>The value of X and Y are multiplied and the result replaces Y.
    *    X is cast  to a LONG before the operation. At operation end,
    *    X is unchanged and the Y type is unchanged.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (CHAR) multiplicand
    * @return (SlipDatum&) current object multiplied by X
    */

   /**
    * @fn SlipDatum::operator/=(const CHAR X)
    * @brief Divide  Y by X (<b>Y /= X</b>) and assign the result to Y.
    * <p>The value of Y is divided by the value of X and the result
    *    replaces Y.  X is cast  to a LONG before the operation. At
    *     operation end, X is unchanged and the Y type is unchanged.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (CHAR) dividend
    * @return (SlipDatum&) current object divided by X
    */

   /**
    * @fn SlipDatum::operator%=(const CHAR X)
    * @brief Take the modulus of Y with respect to X (<b>Y %= X</b>) and assign the result to Y.
    * <p>Take the modulus of the value of Y by the value of X and the
    *    result replaces Y. X is cast to a LONG before the operation.
    *    At operation end, X is unchanged and the Y type is unchanged.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. CHAR, STRING and PTR are not compatible.</p>
    *
    * @param[in] X (CHAR) modulus
    * @return (SlipDatum&) result of Y %= X
    */

   /**
    * @fn SlipDatum::operator<<=(const CHAR X)
    * @brief Left shift Y by X (<b>Y &lt;&lt;= X</b>)  and assign the result to Y.
    * <p>The value of Y shifted by the value of X and the result
    *    replaces Y. X is cast to a LONG before the operation. At
    *    operation end, X is unchanged and the Y type is unchanged.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. CHAR, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (CHAR) shift amount
    * @return (SlipDatum&) current object shifted by X
    */

   /**
    * @fn SlipDatum::operator>>=(const CHAR X)
    * @brief Right shift Y by X (<b>Y &gt;&gt;= X</b>)  and assign the result to Y.
    * <p>The value of Y shifted by the value of X and the result
    *    replaces Y. X is cast to a LONG before the operation. At
    *    operation end, X is unchanged and the Y type is unchanged.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. CHAR, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (CHAR) shift amount
    * @return (SlipDatum&) current object shifted by X
    */

   /**
    * @fn SlipDatum::operators&=(const CHAR X)
    * @brief And Y and X (<b>Y &amp;= X</b>)  and assign the result to Y.
    * <p>The value of Y is anded with the value of X and the result
    *    replaces Y. X is cast to a LONG before the operation. At
    *    operation end, X is unchanged and the Y type is unchanged.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. CHAR, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (CHAR) bit value
    * @return (SlipDatum&) result of &amp;= X
    */

   /**
    * @fn SlipDatum::operator|=(const CHAR)
    * @brief Or Y and X (<b>Y \|= X</b>)  and assign the result to Y.
    * <p>The value of Y is or'd with the value of X and the result
    *    replaces Y. X is cast to a LONG before the operation. At
    *    operation end, X is unchanged and the Y type is unchanged.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. CHAR, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (CHAR) bit value
    * @return (SlipDatum&) result of Y\= X
    */

   /**
    * @fn SlipDatum::operator^=(const CHAR X)
    * @brief Exclusive or Y and X (<b>Y ^= X</b>)  and assign the result to Y.
    * <p>The value of Y is exclusive or'd with the value of X and the result
    *    replaces Y. X is cast to a LONG before the operation. At operation
    *    end, X is  unchanged and the Y type is unchanged.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. CHAR, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (CHAR) bit value
    * @return (SlipDatum&) result of Y ^ X
    */

   /**
    * @fn SlipDatum::operator+=(const LONG X)
    * @brief Add X to Y(<b>Y += X</b>) and assign  the result to Y.
    * <p>The values of Y and X are added and replace Y. At operation
    * end, X is unchanged and the Y type is unchanged.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (LONG) addend
    * @return (SlipDatum&) current object with X added
    */

   /**
    * @fn SlipDatum::operator-=(const LONG X)
    * @brief Subtract Y from X (<b>Y -= X</b>)  and assign the result to Y.
    * <p>The value of X is subtracted from the value of Y and the result
    *    replaces Y. At operation end, X is unchanged and the Y type is
    *    unchanged.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (LONG) subtrahend
    * @return (SlipDatum&) current object with X subtracted
    */

   /**
    * @fn SlipDatum::operator*=(const LONG X)
    * @brief Multiply  X and Y (<b>Y *= X</b>) and assign the result to Y.
    * <p>The value of X and Y are multiplied and the result replaces Y.
    *    At operation end, X is unchanged and the Y type is unchanged.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (LONG) multiplicand
    * @return (SlipDatum&) current object multiplied by X
    */

   /**
    * @fn SlipDatum::operator/=(const LONG X)
    * @brief Divide  Y by X (<b>Y /= X</b>) and assign the result to Y.
    * <p>The value of Y is divided by the value of X and the result
    *    replaces Y. At operation end, X is unchanged and the Y type
    *    is unchanged.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (LONG) dividend
    * @return (SlipDatum&) current object divided by X
    */

   /**
    * @fn SlipDatum::operator%=(const LONG X)
    * @brief Take the modulus of Y with respect to X (<b>Y %= X</b>) and assign the result to Y.
    * <p>Take the modulus of the value of Y by the value of X and the
    *    result replaces Y. At operation end, X is unchanged and the Y
    *    type is unchanged.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. LONG, STRING and PTR are not compatible.</p>
    *
    * @param[in] X (LONG) modulus
    * @return (SlipDatum&) result of Y %= X
    */

   /**
    * @fn SlipDatum::operator<<=(const LONG X)
    * @brief Left shift Y by X (<b>Y &lt;&lt;= X</b>)  and assign the result to Y.
    * <p>The value of Y shifted by the value of X and the result
    *    replaces Y. At operation end, X is unchanged and the Y type
    *    is unchanged.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. LONG, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (LONG) shift amount
    * @return (SlipDatum&) current object shifted by X
    */

   /**
    * @fn SlipDatum::operator>>=(const LONG X)
    * @brief Right shift Y by X (<b>Y &gt;&gt;= X</b>)  and assign the result to Y.
    * <p>The value of Y shifted by the value of X and the result
    *    replaces Y. At operation end, X is unchanged and the Y
    *    type is unchanged.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. LONG, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (LONG) shift amount
    * @return (SlipDatum&) current object shifted by X
    */

   /**
    * @fn SlipDatum::operator&=(const LONG X)
    * @brief And Y and X (<b>Y &amp;= X</b>)  and assign the result to Y.
    * <p>The value of Y is anded with the value of X and the result
    *    replaces Y. At operation end, X is unchanged and the Y type
    *    is unchanged.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. LONG, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (LONG) bit value
    * @return (SlipDatum&) result of Y &amp; X
    */

   /**
    * @fn SlipDatum::operator|=(const LONG X)
    * @brief Or Y and X (<b>Y \|= X</b>)  and assign the result to Y.
    * <p>The value of Y is or'd with the value of X and the result
    *    replaces Y. At operation end, X is unchanged and the Y type
    *    is unchanged.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. LONG, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (LONG) bit value
    * @return (SlipDatum&) result of Y \= X
    */

   /**
    * @fn SlipDatum::operator^=(const LONG X)
    * @brief Exclusive or Y and X (<b>Y ^= X</b>)  and assign the result to Y.
    * <p>The value of Y is exclusive or'd with the value of X and the result
    *    replaces Y. At operation end, X is  unchanged and the Y type is
    *    unchanged.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. LONG, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (LONG) bit value
    * @return (SlipDatum&) result of Y ^ X
    */

   /**
    * @fn SlipDatum::operator+=(const ULONG X)
    * @brief Add X to Y(<b>Y += X</b>) and assign  the result to Y.
    * <p>The values of Y and X are added and replace Y. At operation
    * end, X is unchanged and the Y type is unchanged.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (ULONG) addend
    * @return (SlipDatum&) current object with X added
    */

   /**
    * @fn SlipDatum::operator-=(const ULONG X)
    * @brief Subtract Y from X (<b>Y -= X</b>)  and assign the result to Y.
    * <p>The value of X is subtracted from the value of Y and the result
    *    replaces Y. At operation end, X is unchanged and the Y type is
    *    unchanged.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (ULONG) subtrahend
    * @return (SlipDatum&) current object with X subtracted
    */

   /**
    * @fn SlipDatum::operator*=(const ULONG X)
    * @brief Multiply  X and Y (<b>Y *= X</b>) and assign the result to Y.
    * <p>The value of X and Y are multiplied and the result replaces Y.
    *    At operation end, X is unchanged and the Y type is unchanged.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (ULONG) multiplicand
    * @return (SlipDatum&) current object multiplied by X
    */

   /**
    * @fn SlipDatum::operator/=(const ULONG X)
    * @brief Divide  Y by X (<b>Y /= X</b>) and assign the result to Y.
    * <p>The value of Y is divided by the value of X and the result
    *    replaces Y. At operation end, X is unchanged and the Y type
    *    is unchanged.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. STRING and PTR are not compatible.</p>
    *
    * @param[in] X (ULONG) dividend
    * @return (SlipDatum&) current object divided by X
    */

   /**
    * @fn SlipDatum::operator%=(const ULONG X)
    * @brief Take the modulus of Y with respect to X (<b>Y %= X</b>) and assign the result to Y.
    * <p>Take the modulus of the value of Y by the value of X and the
    *    result replaces Y. At operation end, X is unchanged and the Y
    *    type is unchanged.</p>
    * <p>
    * Both X and Y must be of a compatible data types in order for the
    * operation to complete successfully. If they are compatible but
    * different, then a silent cast is made of the RHS to the LHS type
    * before the operation. ULONG, STRING and PTR are not compatible.</p>
    *
    * @param[in] X (ULONG) modulus
    * @return (SlipDatum&) result of Y %= X
    */

   /**
    * @fn SlipDatum::operator<<=(const ULONG X)
    * @brief Left shift Y by X (<b>Y &lt;&lt;= X</b>)  and assign the result to Y.
    * <p>The value of Y shifted by the value of X and the result
    *    replaces Y. At operation end, X is unchanged and the Y type
    *    is unchanged.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. ULONG, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (ULONG) shift amount
    * @return (SlipDatum&) current object shifted by X
    */

   /**
    * @fn SlipDatum::operator>>=(const ULONG X)
    * @brief Right shift Y by X (<b>Y &gt;&gt;= X</b>)  and assign the result to Y.
    * <p>The value of Y shifted by the value of X and the result
    *    replaces Y. At operation end, X is unchanged and the Y
    *    type is unchanged.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. ULONG, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (ULONG) shift amount
    * @return (SlipDatum&) current object shifted by X
    */

   /**
    * @fn SlipDatum::operator&=(const ULONG X)
    * @brief And Y and X (<b>Y &amp;= X</b>)  and assign the result to Y.
    * <p>The value of Y is anded with the value of X and the result
    *    replaces Y. At operation end, X is unchanged and the Y type
    *    is unchanged.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. ULONG, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (ULONG) bit value
    * @return (SlipDatum&) result of Y &amp; X
    */

   /**
    * @fn SlipDatum::operator|=(const ULONG X)
    * @brief Or Y and X (<b>Y \|= X</b>)  and assign the result to Y.
    * <p>The value of Y is or'd with the value of X and the result
    *    replaces Y. At operation end, X is unchanged and the Y type
    *    is unchanged.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. ULONG, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (ULONG) bit value
    * @return (SlipDatum&) result of Y \= X
    */

   /**
    * @fn SlipDatum::operator^=(const ULONG X)
    * @brief Exclusive or Y and X (<b>Y ^= X</b>)  and assign the result to Y.
    * <p>The value of Y is exclusive or'd with the value of X and the result
    *    replaces Y. At operation end, X is  unchanged and the Y type is
    *    unchanged.</p>
    * <p>Both X and Y must be of a compatible data types in order for
    *    the operation to complete successfully. ULONG, STRING and PTR
    *    are not compatible.</p>
    *
    * @param[in] X (ULONG) bit value
    * @return (SlipDatum&) result of Y ^= X
    */

       /*************************************************
        *              Logical operators                *
       *************************************************/

   /**
    * @fn SlipDatum::operator==(const SlipCell& X)
    * @brief The equal to' (==) operator compares a SlipDatum and a  SlipCell (<b>Y</b> == <b>X</b>).
    * <p>
    * The values of a SlipDatum and a SlipCell cell are compared. The comparison
    * is equivalent to the C++ operation (&lt;type1&gt;)Y == (&lt;type2&gt;)X. The
    * result of the comparison is the same as the C++ equivalent with <b>true</b>
    * returned if Y and X are equal and <b>false</b> otherwise.</p>
    * <p>Comparison of SlipDatum objects to SlipHeader or SlipSublist objects will
    *    return a <b>false</b>.</p>
    * <p>If a STRING is not compared to a STRING or a PTR to a PTR, a <b>false</b>
    *    is returned.</p>
    * <p>If two STRINGs are compared then the string values are compared, that is,
    *    (&lt;string&gt;)Y == (&lt;string&gt;)X. If the two strings are equal
    *    <b>true</b> is returned otherwise <b>false</b> is returned</p>
    * <p>Comparison of two user defined types (PTRs) compares the SlipPointers. If
    *    the SlipPointers point to the same object, then the comparison returns
    *    <b>true</b> otherwise <b>false</b>.</p>
    * <p>If the types are not compatible then <b>false</b> is returned, otherwise
    *    silent casting is done as required and the resultants compared.</p>
    * <p><center><b>Type Comparison Table</b></center></p>
    * <center>
    * | LHS/RHS     |   Header    |   Sublist   | bool        | CHAR        | UCHAR       | LONG        | ULONG       | PTR         | STRING      |
    * | :---------- | :---------: | :---------: | :---------: | :---------: | :---------: | :---------: | :---------: | :---------: | :---------: |
    * | SlipHeader  |      <b>X</b>      |             |             |             |             |             |             |             |             |
    * | SlipSublist |             |      <b>X</b>      |             |             |             |             |             |             |             |
    * | bool        |             |             |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
    * | CHAR        |             |             |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
    * | UCHAR       |             |             |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
    * | LONG        |             |             |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
    * | ULONG       |             |             |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
    * | PTR         |             |             |             |             |             |             |             |      <b>X</b>      |             |
    * | STRING      |             |             |             |             |             |             |             |             |      <b>X</b>      |
    * </center>
    * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
    *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
    * <p>
    * Some examples of use are:</p>
    * <tt><pre>
    *    SlipHeader * X = new SlipHeader();
    *    SlipDatum    Y((LONG)3);
    *    SlipDatum    Z((LONG)3);
    *    SlipDatum    S1("less");
    *    SlipDatum    S2("more");
    *
    *    bool x = Y == X;                      // false is saved
    *    cout << std::boolalpha << (S1 == S2) << endl; // output false
    *    cout << std::boolalpha << (Y == Z) << endl;  // output true
    *    X->insLeft(Y == X);                  // insert Left (bool)false
    *
    *    X.deleteList();
    * </pre></tt>
    *
    * @param[in] X (SlipCell&) comparator
    * @return true if Y == X, false otherwise
    */
   bool SlipDatum::operator==(const SlipCell& X)        { return ((SlipOp*)*getOperator())->equal(*this, (SlipDatum&)X); }  // Y == X


 /**
  * @fn SlipDatum::operator!=(const SlipDatum& X)
  * @brief The 'not equal to' (!=) operator compares two SlipDatum cells (<b>Y</b> != <b>X</b>).
  * <p>
  * The values of two SlipDatum cells are compared. The comparison is equivalent
  * to the C++ operation (&lt;type1&gt;)Y != (&lt;type2&gt;)X. The result of the
  * comparison is the same as the C++ equivalent with <b>true</b> returned if Y
  * and X are not equal and <b>false</b> otherwise.</p>
  * <p>
  * If a STRING is not compared to a STRING or a PTR to a PTR, a <b>true</b>
  *    is returned.</p>
  * <p>If two STRINGs are compared then the string values are compared, that is,
  *    (&lt;string&gt;)Y != (&lt;string&gt;)X. If the two strings are not equal
  *    <b>true</b> is returned otherwise <b>false</b> is returned</p>
  * <p>Comparison of two user defined types (PTRs) compares the SlipPointers. If
  *    the SlipPointers point to the same object, then the comparison returns
  *    <b>false</b> otherwise <b>true</b>.</p>
  * <p>If the types are not compatible then <b>false</b> is returned, otherwise
  *    silent casting is done as required and the resultants compared.</p>
  * <p><center><b>Type Comparison Table</b></center></p>
  * <center>
  * | LHS/RHS     | bool        | CHAR        | UCHAR       | LONG        | ULONG       | PTR         | STRING      |
  * | :---------- | :---------: | :---------: | :---------: | :---------: | :---------: | :---------: | :---------: |
  * | bool        |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
  * | CHAR        |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
  * | UCHAR       |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
  * | LONG        |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
  * | ULONG       |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
  * | PTR         |             |             |             |             |             |      <b>X</b>      |             |
  * | STRING      |             |             |             |             |             |             |      <b>X</b>      |
  * </center>
  * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
  *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
  * <p>
  * Some examples of use are:</p>
  * <tt><pre>
  *    SlipHeader * X = new SlipHeader();
  *    SlipDatum    Y((LONG)3);
  *    SlipDatum    Z((LONG)3);
  *    SlipDatum    S1("less");
  *    SlipDatum    S2("more");
  *
  *    bool x = Y != S1;                     // true is saved
  *    cout << std::boolalpha << (S1 != S2) << endl; // output true
  *    cout << std::boolalpha << (Y != Z) << endl;  // output false
  *    X->insLeft(Y != Z);                  // insert Left (bool)false
  *
  *    X.deleteList();
  * </pre></tt>
  *
  * @param[in] X (SlipDatum&) comparator
  * @return true if Y != X, false otherwise
  */

 /**
  * @fn SlipDatum::operator<(const SlipDatum&  X)
  * @brief The 'less than' (&lt;) operator compares two SlipDatum cells (<b>Y</b> &lt; <b>X</b>).
  * <p>
  * The values of two SlipDatum cells are compared. The comparison is equivalent
  * to the C++ operation (&lt;type1&gt;)Y &lt; (&lt;type2&gt;)X. The result of the
  * comparison is the same as the C++ equivalent with <b>true</b> returned if Y
  * is less than X and <b>false</b> otherwise.</p>
  * <p>
  * If a STRING is not compared to a STRING or a PTR to a PTR, a <b>false</b>
  *    is returned.</p>
  * <p>If two STRINGs are compared then the string values are compared, that is,
  *    (&lt;string&gt;)Y &lt; (&lt;string&gt;)X. If the Y is lexicographically
  *   less than X then <b>true</b> is returned otherwise <b>false</b> is
  *    returned</p>
  * <p>Comparison of two user defined types (PTRs) returns <b>false</b>.</p>
  * <p>If the types are not compatible then <b>false</b> is returned, otherwise
  *    silent casting is done as required and the resultants compared.</p>
  * <p><center><b>Type Comparison Table</b></center></p>
  * <center>
  * | LHS/RHS     | bool        | CHAR        | UCHAR       | LONG        | ULONG       | PTR         | STRING      |
  * | :---------- | :---------: | :---------: | :---------: | :---------: | :---------: | :---------: | :---------: |
  * | bool        |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
  * | CHAR        |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
  * | UCHAR       |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
  * | LONG        |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
  * | ULONG       |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
  * | PTR         |             |             |             |             |             |            |             |
  * | STRING      |             |             |             |             |             |             |      <b>X</b>      |
  * </center>
  * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
  *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
  * <p>
  * Some examples of use are:</p>
  * <tt><pre>
  *    SlipHeader * X = new SlipHeader();
  *    SlipDatum    Y((LONG)3);
  *    SlipDatum    Z((LONG)3);
  *    SlipDatum    S1("less");
  *    SlipDatum    S2("more");
  *
  *    bool x = Y < S1;                      // false is saved
  *    cout << std::boolalpha << (S1 < S2) << endl;  // output true
  *    cout << std::boolalpha << (Y < Z) << endl;   // output false
  *    X->insLeft(Y < Z);                   // insert Left (bool)false
  *
  *    X.deleteList();
  * </pre></tt>
  *
  * @param[in] X (SlipDatum&) comparator
  * @return true if Y &lt; X, false otherwise
  */

   /**
    * @fn SlipDatum::operator<=(const SlipDatum& X) 
    * @brief The 'less than or equal to' (&lt;=) operator compares two SlipDatum cellsl(<b>Y</b> &lt;= <b>X</b>).
    * <p>
    * The values of two SlipDatum cells are compared. The comparison is equivalent
    * to the C++ operation (&lt;type1&gt;)Y &lt;= (&lt;type2&gt;)X. The result of the
    * comparison is the same as the C++ equivalent with <b>true</b> returned if Y
    * is less than or equal to X and <b>false</b> otherwise.</p>
    * <p>
    * If a STRING is not compared to a STRING or a PTR to a PTR, a <b>false</b>
    *    is returned.</p>
    * <p>If two STRINGs are compared then the string values are compared, that is,
    *    (&lt;string&gt;)Y &lt;= (&lt;string&gt;)X. If Y is lexicographically
    *    less than or equal to X then <b>true</b> is returned otherwise
    *    <b>false</b> is returned</p>
    * <p>Comparison of two user defined types (PTRs) returns <b>false</b>.</p>
    * <p>If the types are not compatible then <b>false</b> is returned, otherwise
    *    silent casting is done as required and the resultants compared.</p>
    * <p><center><b>Type Comparison Table</b></center></p>
    * <center>
    * | LHS/RHS     | bool        | CHAR        | UCHAR       | LONG        | ULONG       | PTR         | STRING      |
    * | :---------- | :---------: | :---------: | :---------: | :---------: | :---------: | :---------: | :---------: |
    * | bool        |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
    * | CHAR        |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
    * | UCHAR       |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
    * | LONG        |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
    * | ULONG       |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
    * | PTR         |             |             |             |             |             |            |             |
    * | STRING      |             |             |             |             |             |             |      <b>X</b>      |
    * </center>
    * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
    *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
    * <p>
    * Some examples of use are:</p>
    * <tt><pre>
    *    SlipHeader * X = new SlipHeader();
    *    SlipDatum    Y((LONG)3);
    *    SlipDatum    Z((LONG)3);
    *    SlipDatum    S1("less");
    *    SlipDatum    S2("more");
    *
    *    bool x = Y <= S1;                      // false is saved
    *    cout << std::boolalpha << (S1 <= S2) << endl;  // output true
    *    cout << std::boolalpha << (Y <= Z) << endl;   // output true
    *    X->insLeft(Y <= Z);                   // insert Left (bool)true
    *
    *    X.deleteList();
    * </pre></tt>
    *
    * @param[in] X (SlipDatum&) comparator
    * @return true if Y &lt;= X, false otherwise
    */

   /**
    * @fn SlipDatum::operator==(const SlipDatum& X)
    * @brief The 'equal to' (==) operator compares two SlipDatum cells (<b>Y</b> == <b>X</b>).
    * <p>
    * The values of two SlipDatum cells are compared. The comparison is equivalent
    * to the C++ operation (&lt;type1&gt;)Y == (&lt;type2&gt;)X. The result of the
    * comparison is the same as the C++ equivalent with <b>true</b> returned if Y
    * and X are equal and <b>false</b> otherwise.</p>
    * <p>
    * If a STRING is not compared to a STRING or a PTR to a PTR, a <b>false</b>
    *    is returned.</p>
    * <p>If two STRINGs are compared then the string values are compared, that is,
    *    (&lt;string&gt;)Y == (&lt;string&gt;)X. If the two strings are equal
    *    <b>true</b> is returned otherwise <b>false</b> is returned</p>
    * <p>Comparison of two user defined types (PTRs) compares the SlipPointers. If
    *    the SlipPointers point to the same object, then the comparison returns
    *    <b>true</b> otherwise <b>false</b>.</p>
    * <p>If the types are not compatible then <b>false</b> is returned, otherwise
    *    silent casting is done as required and the resultants compared.</p>
    * <p><center><b>Type Comparison Table</b></center></p>
    * <center>
    * | LHS/RHS     | bool        | CHAR        | UCHAR       | LONG        | ULONG       | PTR         | STRING      |
    * | :---------- | :---------: | :---------: | :---------: | :---------: | :---------: | :---------: | :---------: |
    * | bool        |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
    * | CHAR        |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
    * | UCHAR       |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
    * | LONG        |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
    * | ULONG       |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
    * | PTR         |             |             |             |             |             |      <b>X</b>      |             |
    * | STRING      |             |             |             |             |             |             |      <b>X</b>      |
    * </center>
    * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
    *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
    * <p>
    * Some examples of use are:</p>
    * <tt><pre>
    *    SlipHeader * X = new SlipHeader();
    *    SlipDatum    Y((LONG)3);
    *    SlipDatum    Z((LONG)3);
    *    SlipDatum    S1("less");
    *    SlipDatum    S2("more");
    *
    *    bool x = Y == S1;                     // false is saved
    *    cout << std::boolalpha << (S1 == S2) << endl; // output false
    *    cout << std::boolalpha << (Y == Z) << endl;  // output true
    *    X->insLeft(Y == X);                  // insert Left (bool)false
    *
    *    X.deleteList();
    * </pre></tt>
    *
    * @param[in] X (SlipDatum&) comparator
    * @return true if Y == X, false otherwise
    */

    /**
     * @fn SlipDatum::operator>=(const SlipDatum& X)
     * @brief The 'greater than or equal to' (&gt;=) operator compares two SlipDatum cellsl(<b>Y</b> &gt;= <b>X</b>).
     * <p>
     * The values of two SlipDatum cells are compared. The comparison is equivalent
     * to the C++ operation (&lt;type1&gt;)Y &gt;= (&lt;type2&gt;)X. The result of the
     * comparison is the same as the C++ equivalent with <b>true</b> returned if Y
     * is greater than or equal to X and <b>false</b> otherwise.</p>
     * <p>
     * If a STRING is not compared to a STRING a <b>false</b> is returned.</p>
     * <p>If two STRINGs are compared then the string values are compared, that is,
     *    (&lt;string&gt;)Y &gt;= (&lt;string&gt;)X. If Y is lexicographically
     *    greater than or equal to X then <b>true</b> is returned otherwise
     *    <b>false</b> is returned</p>
     * <p>Comparison of two user defined types (PTRs) returns <b>false</b>.</p>
     * <p>If the types are not compatible then <b>false</b> is returned, otherwise
     *    silent casting is done as required and the resultants compared.</p>
     * <p><center><b>Type Comparison Table</b></center></p>
     * <center>
     * | LHS/RHS     | bool        | CHAR        | UCHAR       | LONG        | ULONG       | PTR         | STRING      |
     * | :---------- | :---------: | :---------: | :---------: | :---------: | :---------: | :---------: | :---------: |
     * | bool        |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
     * | CHAR        |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
     * | UCHAR       |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
     * | LONG        |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
     * | ULONG       |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
     * | PTR         |             |             |             |             |             |            |             |
     * | STRING      |             |             |             |             |             |             |      <b>X</b>      |
     * </center>
     * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
     *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
     * <p>
     * Some examples of use are:</p>
     * <tt><pre>
     *    SlipHeader * X = new SlipHeader();
     *    SlipDatum    Y((LONG)3);
     *    SlipDatum    Z((LONG)3);
     *    SlipDatum    S1("less");
     *    SlipDatum    S2("more");
     *
     *    bool x = Y >= S1;                      // false is saved
     *    cout << std::boolalpha << (S1 >= S2) << endl;  // output true
     *    cout << std::boolalpha << (Y >= Z) << endl;   // output true
     *    X->insLeft(Y >= Z);                   // insert Left (bool)true
     *
     *    X.deleteList();
     * </pre></tt>
     *
     * @param[in] X (SlipDatum&) comparator
     * @return true if Y &gt;= X, false otherwise
     */

    /**
     * @fn SlipDatum::operator>(const SlipDatum&  X)
     * @brief The 'greater than' (&gt;) operator compares two SlipDatum cellsl(<b>Y</b> &gt; <b>X</b>).
     * <p>
     * The values of two SlipDatum cells are compared. The comparison is equivalent
     * to the C++ operation (&lt;type1&gt;)Y &gt; (&lt;type2&gt;)X. The result of the
     * comparison is the same as the C++ equivalent with <b>true</b> returned if Y
     * is greater than X and <b>false</b> otherwise.</p>
     * <p>
     * If a STRING is not compared to a STRING a <b>false</b> is returned.</p>
     * <p>If two STRINGs are compared then the string values are compared, that is,
     *    (&lt;string&gt;)Y &gt; (&lt;string&gt;)X. If Y is lexicographically
     *    greater than X then <b>true</b> is returned otherwise <b>false</b> is
     *    returned</p>
     * <p>Comparison of two user defined types (PTRs) returns <b>false</b>.</p>
     * <p>If the types are not compatible then <b>false</b> is returned, otherwise
     *    silent casting is done as required and the resultants compared.</p>
     * <p><center><b>Type Comparison Table</b></center></p>
     * <center>
     * | LHS/RHS     | bool        | CHAR        | UCHAR       | LONG        | ULONG       | PTR         | STRING      |
     * | :---------- | :---------: | :---------: | :---------: | :---------: | :---------: | :---------: | :---------: |
     * | bool        |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
     * | CHAR        |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
     * | UCHAR       |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
     * | LONG        |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
     * | ULONG       |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |      <b>X</b>      |             |             |
     * | PTR         |             |             |             |             |             |            |             |
     * | STRING      |             |             |             |             |             |             |      <b>X</b>      |
     * </center>
     * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
     *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
     * <p>
     * Some examples of use are:</p>
     * <tt><pre>
     *    SlipHeader * X = new SlipHeader();
     *    SlipDatum    Y((LONG)3);
     *    SlipDatum    Z((LONG)3);
     *    SlipDatum    S1("less");
     *    SlipDatum    S2("more");
     *
     *    bool x = Y > S1;                       // false is saved
     *    cout << std::boolalpha << (S1 > S2) << endl;   // output false
     *    cout << std::boolalpha << (Y > Z) << endl;    // output false
     *    X->insLeft(Y > Z);                   // insert Left (bool)false
     *
     *    X.deleteList();
     * </pre></tt>
     *
     * @param[in] X (SlipDatum&) comparator
     * @return true if Y &gt; X, false otherwise
     */


  /**
   * @fn SlipDatum::operator!=(const bool     X)
   * @brief The 'not equal to' (!=) operator compares two SlipDatum cells (<b>Y</b> != (bool)<b>X</b>).
   * <p>The bool X is cast to a  LONG prior to the comparison.</p>
   * <p>
   * The values of a SlipDatum cell and a bool are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y != (LONG)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y and (LONG)X are not equal and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     | bool        |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((LONG)3);
   *    SlipDatum    U((LONG)1);
   *
   *    bool x = Y != true;                      // true is saved
   *    cout << std::boolalpha << (Y != true) << endl; // output true
   *    cout << std::boolalpha << (U != true) << endl;  // output false
   *    X->insLeft(Y != true);                  // insert Left (bool)true
   *
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (bool) comparator
   * @return true if Y != X, false otherwise
   */

  /**
   * @fn SlipDatum::operator<(const bool      X)
   * @brief The 'less than' (&lt;) operator compares two SlipDatum cells (<b>Y</b> &lt; (bool)<b>X</b>).
   * <p>X is cast to a  LONG prior to the comparison.</p>
   * <p>
   * The values of a SlipDatum cell and a bool are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y &lt; (LONG)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is less than (LONG)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     | bool        |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((LONG)3);
   *    SlipDatum    U((LONG)1);
   *
   *    bool x = Y < true;                       // false is saved
   *    cout << std::boolalpha << (Y < true) << endl;  // output false
   *    cout << std::boolalpha << (U < true) << endl;   // output false
   *    X->insLeft(Y < true);                   // insert Left (bool)false
   *
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (bool) comparator
   * @return true if Y &lt; X, false otherwise
   */

  /**
   * @fn SlipDatum::operator<=(const bool     X)
   * @brief The 'less than or equal to' (&lt;=) operator compares two SlipDatum cells (<b>Y</b> &lt;= (bool)<b>X</b>).
   * <p>X is cast to a  LONG prior to the comparison.</p>
   * <p>
   * The values of a SlipDatum cell and a bool are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y &lt;= (LONG)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is less than or equal to (LONG)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     | bool        |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((LONG)3);
   *    SlipDatum    U((LONG)1);
   *
   *    bool x = Y <= true;                      // false is saved
   *    cout << std::boolalpha << (Y <= true) << endl; // output false
   *    cout << std::boolalpha << (U <= true) << endl;   // output true
   *    X->insLeft(Y <= true);                  // insert Left (bool)false
   *
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (bool) comparator
   * @return true if Y &lt;= X, false otherwise
   */

  /**
   * @fn SlipDatum::operator==(const bool     X)
   * @brief The 'equal to' (==) operator compares two SlipDatum cells (<b>Y</b> == (bool)<b>X</b>).
   * <p>X is cast to a  LONG prior to the comparison.</p>
   * <p>
   * The values of a SlipDatum cell and a bool are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y == (LONG)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is equal to (LONG)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     | bool        |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((LONG)3);
   *    SlipDatum    U((LONG)1);
   *
   *    bool x = Y == true;                      // false is saved
   *    cout << std::boolalpha << (Y == true) << endl; // output false
   *    cout << std::boolalpha << (U == true) << endl;   // output true
   *    X->insLeft(Y == true);                  // insert Left (bool)false
   *
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (bool) comparator
   * @return true if Y == X, false otherwise
   */

  /**
   * Wfn SlipDatum::operator>=(const bool     X)
   * @brief The 'greater than or equal to' (&gt;=) operator compares two SlipDatum cells (<b>Y</b> &gt;= (bool)<b>X</b>).
   * <p>X is cast to a  LONG prior to the comparison.</p>
   * <p>
   * The values of a SlipDatum cell and a bool are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y &gt;= (LONG)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is greater than or equal to (LONG)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     | bool        |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((LONG)3);
   *    SlipDatum    U((LONG)1);
   *
   *    bool x = Y >= true;                      // true is saved
   *    cout << std::boolalpha << (Y >= true) << endl; // output true
   *    cout << std::boolalpha << (U >= true) << endl;   // output true
   *    X->insLeft(Y >= true);                  // insert Left (bool)true
   *
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (bool) comparator
   * @return true if Y &gt;= X, false otherwise
   */

  /**
   * @fn SlipDatum::operator>(const bool      X)
   * @brief The 'greater than' (&gt;) operator compares two SlipDatum cells (<b>Y</b> &gt; (bool)<b>X</b>).
   * <p>X is cast to a  LONG prior to the comparison.</p>
   * <p>
   * The values of a SlipDatum cell and a bool are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y &gt; (LONG)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is greater than (LONG)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     | bool        |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((LONG)3);
   *    SlipDatum    U((LONG)1);
   *
   *    bool x = Y > true;                       // true is saved
   *    cout << std::boolalpha << (Y > true) << endl;  // output true
   *    cout << std::boolalpha << (U > true) << endl;    // output false
   *    X->insLeft(Y > true);                   // insert Left (bool)true
   *
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (bool) comparator
   * @return true if Y &gt; X, false otherwise
   */


  /**
   * @fn SlipDatum::operator!=(const DOUBLE     X)
   * @brief The 'not equal to' (!=) operator compares two SlipDatum cells (<b>Y</b> != (DOUBLE)<b>X</b>).
   * <p>
   * The values of a SlipDatum cell and a DOUBLE are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y != (DOUBLE)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y and (DOUBLE)X are not equal and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  DOUBLE     |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((DOUBLE)3);
   *    SlipDatum    Z((DOUBLE)3);
   *    SlipDatum    U((DOUBLE)1);
   *
   *    bool x = Y != Z;                             // false is saved
   *    cout << std::boolalpha << (Y != Z) << endl;  // output true
   *    cout << std::boolalpha << (U != Z) << endl;  // output true
   *    X->insLeft(Y != Z);                          // insert Left (bool)false
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (DOUBLE) comparator
   * @return true if Y != X, false otherwise
   */

  /**
   * @fn SlipDatum::operator<(const DOUBLE      X)
   * @brief The 'less than' (&lt;) operator compares two SlipDatum cells (<b>Y</b> &lt; (DOUBLE)<b>X</b>).
   * <p>
   * The values of a SlipDatum cell and a DOUBLE are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y &lt; (DOUBLE)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is less than (DOUBLE)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  DOUBLE     |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((DOUBLE)3);
   *    DOUBLE       Z = 3.0;
   *    SlipDatum    U((DOUBLE)1);
   *
   *    bool x = Y < Z;                              // false is saved
   *    cout << std::boolalpha << (Y < Z) << endl;   // output false
   *    cout << std::boolalpha << (U < Z) << endl;   // output true
   *    X->insLeft(Y < Z);                           // insert Left (bool)false
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (DOUBLE) comparator
   * @return true if Y &lt; X, false otherwise
   */

  /**
   * @fn SlipDatum::operator<=(const DOUBLE     X)
   * @brief The 'less than or equal to' (&lt;=) operator compares two SlipDatum cells (<b>Y</b> &lt;= (DOUBLE)<b>X</b>).
   * <p>
   * The values of a SlipDatum cell and a DOUBLE are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y &lt;= (DOUBLE)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is less than or equal to (DOUBLE)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  DOUBLE     |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((DOUBLE)3);
   *    DOUBLE       Z = 3.0;
   *    SlipDatum    U((DOUBLE)1);
   *
   *    bool x = Y <= Z;                             // true is saved
   *    cout << std::boolalpha << (Y <= Z) << endl;  // output true
   *    cout << std::boolalpha << (U <= Z) << endl;  // output true
   *    X->insLeft(Y <= Z);                          // insert Left (bool)true
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (DOUBLE) comparator
   * @return true if Y &lt;= X, false otherwise
   */

  /**
   * @fn SlipDatum::operator==(const DOUBLE     X)
   * @brief The 'equal to' (==) operator compares two SlipDatum cells (<b>Y</b> == (DOUBLE)<b>X</b>).
   * <p>
   * The values of a SlipDatum cell and a DOUBLE are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y == (DOUBLE)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is equal to (DOUBLE)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  DOUBLE     |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((DOUBLE)3);
   *    DOUBLE       Z = 3.0;
   *    SlipDatum    U((DOUBLE)1);
   *
   *    bool x = Y == Z;                             // true is saved
   *    cout << std::boolalpha << (Y == Z) << endl;  // output true
   *    cout << std::boolalpha << (U == Z) << endl;  // output false
   *    X->insLeft(Y == Z);                          // insert Left (bool)true
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (DOUBLE) comparator
   * @return true if Y == X, false otherwise
   */

  /**
   * @fn SlipDatum::operator>=(const DOUBLE     X)
   * @brief The 'greater than or equal to' (&gt;=) operator compares two SlipDatum cells (<b>Y</b> &gt;= (DOUBLE)<b>X</b>).
   * <p>
   * The values of a SlipDatum cell and a DOUBLE are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y &gt;= (DOUBLE)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is greater than or equal to (DOUBLE)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  DOUBLE     |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((DOUBLE)3);
   *    DOUBLE       Z = 3.0;
   *    SlipDatum    U((DOUBLE)1);
   *
   *    bool x = Y >= Z;                             // true is saved
   *    cout << std::boolalpha << (Y >= Z) << endl;  // output true
   *    cout << std::boolalpha << (U >= Z) << endl;  // output false
   *    X->insLeft(Y >= Z);                          // insert Left (bool)true
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (DOUBLE) comparator
   * @return true if Y &gt;= X, false otherwise
   */

  /**
   * @fn SlipDatum::operator>(const DOUBLE      X)
   * @brief The 'greater than' (&gt;) operator compares two SlipDatum cells (<b>Y</b> &gt; (DOUBLE)<b>X</b>).
   * <p>
   * The values of a SlipDatum cell and a DOUBLE are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y &gt; (DOUBLE)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is greater than (DOUBLE)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  DOUBLE     |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((DOUBLE)3);
   *    DOUBLE       Z = 3.0;
   *    SlipDatum    U((DOUBLE)1);
   *
   *    bool x = Y > Z;                              // false is saved
   *    cout << std::boolalpha << (Y > Z) << endl;   // output false
   *    cout << std::boolalpha << (U > Z) << endl;   // output false
   *    X->insLeft(Y > Z);                           // insert Left (bool)false
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (DOUBLE) comparator
   * @return true if Y &gt; X, false otherwise
   */

  /**
   * @fn SlipDatum::operator!=(const UCHAR     X)
   * @brief The 'not equal to' (!=) operator compares two SlipDatum cells (<b>Y</b> != (UCHAR)<b>X</b>).
   * <p>X is cast to a ULONG prior to the comparison.</p>
   * <p>
   * The values of a SlipDatum cell and a UCHAR are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y != (UCHAR)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y and (UCHAR)X are not equal and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     | ULONG       |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((UCHAR)3);
   *    UCHAR        Z = 3;
   *    SlipDatum    U((UCHAR)1);
   *
   *    bool x = Y != Z;                             // false is saved
   *    cout << std::boolalpha << (Y != Z) << endl;  // output true
   *    cout << std::boolalpha << (U != Z) << endl;  // output true
   *    X->insLeft(Y != Z);                          // insert Left (bool)false
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (UCHAR) comparator
   * @return true if Y != X, false otherwise
   */

  /**
   * @fn SlipDatum::operator<(const UCHAR      X)
   * @brief The 'less than' (&lt;) operator compares two SlipDatum cells (<b>Y</b> &lt; (UCHAR)<b>X</b>).
   * <p>X is cast to a ULONG prior to the comparison.</p>
   * <p>
   * The values of a SlipDatum cell and a UCHAR are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y &lt; (UCHAR)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is less than (UCHAR)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     | ULONG       |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((UCHAR)3);
   *    UCHAR        Z = 3;
   *    SlipDatum    U((UCHAR)1);
   *
   *    bool x = Y < Z;                              // false is saved
   *    cout << std::boolalpha << (Y < Z) << endl;   // output false
   *    cout << std::boolalpha << (U < Z) << endl;   // output true
   *    X->insLeft(Y < Z);                           // insert Left (bool)false
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (UCHAR) comparator
   * @return true if Y &lt; X, false otherwise
   */

  /**
   * @fn SlipDatum::operator<=(const UCHAR     X)
   * @brief The 'less than or equal to' (&lt;=) operator compares two SlipDatum cells (<b>Y</b> &lt;= (UCHAR)<b>X</b>).
   * <p>X is cast to a ULONG prior to the comparison.</p>
   * <p>
   * The values of a SlipDatum cell and a UCHAR are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y &lt;= (UCHAR)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is less than or equal to (UCHAR)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     | ULONG       |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((UCHAR)3);
   *    UCHAR        Z = 3;
   *    SlipDatum    U((UCHAR)1);
   *
   *    bool x = Y <= Z;                             // true is saved
   *    cout << std::boolalpha << (Y <= Z) << endl;  // output true
   *    cout << std::boolalpha << (U <= Z) << endl;  // output true
   *    X->insLeft(Y <= Z);                          // insert Left (bool)true
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (UCHAR) comparator
   * @return true if Y &lt;= X, false otherwise
   */

  /**
   * @fn SlipDatum::operator==(const UCHAR     X)
   * @brief The 'equal to' (==) operator compares two SlipDatum cells (<b>Y</b> == (UCHAR)<b>X</b>).
   * <p>X is cast to a ULONG prior to the comparison.</p>
   * <p>
   * The values of a SlipDatum cell and a UCHAR are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y == (UCHAR)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is equal to (UCHAR)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     | ULONG       |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((UCHAR)3);
   *    UCHAR        Z = 3;
   *    SlipDatum    U((UCHAR)1);
   *
   *    bool x = Y == Z;                             // true is saved
   *    cout << std::boolalpha << (Y == Z) << endl;  // output true
   *    cout << std::boolalpha << (U == Z) << endl;  // output false
   *    X->insLeft(Y == Z);                          // insert Left (bool)true
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (UCHAR) comparator
   * @return true if Y == X, false otherwise
   */

  /**
   * @fn  SlipDatum::operator>=(const UCHAR     X)
   * @brief The 'greater than or equal to' (&gt;=) operator compares two SlipDatum cells (<b>Y</b> &gt;= (UCHAR)<b>X</b>).
   * <p>X is cast to a ULONG prior to the comparison.</p>
   * <p>
   * The values of a SlipDatum cell and a UCHAR are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y &gt;= (UCHAR)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is greater than or equal to (UCHAR)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     | ULONG       |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((UCHAR)3);
   *    UCHAR        Z = 3;
   *    SlipDatum    U((UCHAR)1);
   *
   *    bool x = Y >= Z;                             // true is saved
   *    cout << std::boolalpha << (Y >= Z) << endl;  // output true
   *    cout << std::boolalpha << (U >= Z) << endl;  // output false
   *    X->insLeft(Y >= Z);                          // insert Left (bool)true
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (UCHAR) comparator
   * @return true if Y &gt;= X, false otherwise
   */

  /**
   * @fn SlipDatum::operator>(const UCHAR      X)
   * @brief The 'greater than' (&gt;) operator compares two SlipDatum cells (<b>Y</b> &gt; (UCHAR)<b>X</b>).
   * <p>X is cast to a ULONG prior to the comparison.</p>
   * <p>
   * The values of a SlipDatum cell and a UCHAR are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y &gt; (UCHAR)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is greater than (UCHAR)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     | ULONG       |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((UCHAR)3);
   *    UCHAR        Z = 3;
   *    SlipDatum    U((UCHAR)1);
   *
   *    bool x = Y > Z;                              // false is saved
   *    cout << std::boolalpha << (Y > Z) << endl;   // output false
   *    cout << std::boolalpha << (U > Z) << endl;   // output false
   *    X->insLeft(Y > Z);                           // insert Left (bool)false
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (UCHAR) comparator
   * @return true if Y &gt; X, false otherwise
   */


  /**
   * @fn SlipDatum::operator!=(const CHAR     X)
   * @brief The 'not equal to' (!=) operator compares two SlipDatum cells (<b>Y</b> != (CHAR)<b>X</b>).
   * <p>X is cast to a LONG prior to the comparison.</p>
   * <p>
   * The values of a SlipDatum cell and a CHAR are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y != (CHAR)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y and (CHAR)X are not equal and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  LONG       |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((CHAR)3);
   *    CHAR         Z = 3;
   *    SlipDatum    U((CHAR)1);
   *
   *    bool x = Y != Z;                             // false is saved
   *    cout << std::boolalpha << (Y != Z) << endl;  // output true
   *    cout << std::boolalpha << (U != Z) << endl;  // output true
   *    X->insLeft(Y != Z);                          // insert Left (bool)false
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (CHAR) comparator
   * @return true if Y != X, false otherwise
   */

  /**
   * @fn SlipDatum::operator<(const CHAR      X)
   * @brief The 'less than' (&lt;) operator compares two SlipDatum cells (<b>Y</b> &lt; (CHAR)<b>X</b>).
   * <p>X is cast to a LONG prior to the comparison.</p>
   * <p>
   * The values of a SlipDatum cell and a CHAR are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y &lt; (CHAR)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is less than (CHAR)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  LONG       |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((CHAR)3);
   *    CHAR         Z = 3;
   *    SlipDatum    U((CHAR)1);
   *
   *    bool x = Y < Z;                              // false is saved
   *    cout << std::boolalpha << (Y < Z) << endl;   // output false
   *    cout << std::boolalpha << (U < Z) << endl;   // output true
   *    X->insLeft(Y < Z);                           // insert Left (bool)false
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (CHAR) comparator
   * @return true if Y &lt; X, false otherwise
   */

  /**
   * @fn SlipDatum::operator<=(const CHAR     X)
   * @brief The 'less than or equal to' (&lt;=) operator compares two SlipDatum cells (<b>Y</b> &lt;= (CHAR)<b>X</b>).
   * <p>X is cast to a LONG prior to the comparison.</p>
   * <p>
   * The values of a SlipDatum cell and a CHAR are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y &lt;= (CHAR)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is less than or equal to (CHAR)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  LONG       |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((CHAR)3);
   *    CHAR         Z = 3;
   *    SlipDatum    U((CHAR)1);
   *
   *    bool x = Y <= Z;                             // true is saved
   *    cout << std::boolalpha << (Y <= Z) << endl;  // output true
   *    cout << std::boolalpha << (U <= Z) << endl;  // output true
   *    X->insLeft(Y <= Z);                          // insert Left (bool)true
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (CHAR) comparator
   * @return true if Y &lt;= X, false otherwise
   */

  /**
   * @fn SlipDatum::operator==(const CHAR     X)
   * @brief The 'equal to' (==) operator compares two SlipDatum cells (<b>Y</b> == (CHAR)<b>X</b>).
   * <p>X is cast to a LONG prior to the comparison.</p>
   * <p>
   * The values of a SlipDatum cell and a CHAR are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y == (CHAR)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is equal to (CHAR)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  LONG       |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((CHAR)3);
   *    CHAR         Z = 3;
   *    SlipDatum    U((CHAR)1);
   *
   *    bool x = Y == Z;                             // true is saved
   *    cout << std::boolalpha << (Y == Z) << endl;  // output true
   *    cout << std::boolalpha << (U == Z) << endl;  // output false
   *    X->insLeft(Y == Z);                          // insert Left (bool)true
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (CHAR) comparator
   * @return true if Y == X, false otherwise
   */

  /**
   * @fn SlipDatum::operator>=(const CHAR     X)
   * @brief The 'greater than or equal to' (&gt;=) operator compares two SlipDatum cells (<b>Y</b> &gt;= (CHAR)<b>X</b>).
   * <p>X is cast to a LONG prior to the comparison.</p>
   * <p>
   * The values of a SlipDatum cell and a CHAR are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y &gt;= (CHAR)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is greater than or equal to (CHAR)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  LONG       |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((CHAR)3);
   *    CHAR         Z = 3;
   *    SlipDatum    U((CHAR)1);
   *
   *    bool x = Y >= Z;                             // true is saved
   *    cout << std::boolalpha << (Y >= Z) << endl;  // output true
   *    cout << std::boolalpha << (U >= Z) << endl;  // output false
   *    X->insLeft(Y >= Z);                          // insert Left (bool)true
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (CHAR) comparator
   * @return true if Y &gt;= X, false otherwise
   */

  /**
   * @fn SlipDatum::operator>(const CHAR      X)
   * @brief The 'greater than' (&gt;) operator compares two SlipDatum cells (<b>Y</b> &gt; (CHAR)<b>X</b>).
   * <p>X is cast to a LONG prior to the comparison.</p>
   * <p>
   * The values of a SlipDatum cell and a CHAR are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y &gt; (CHAR)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is greater than (CHAR)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  LONG       |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((CHAR)3);
   *    CHAR         Z = 3;
   *    SlipDatum    U((CHAR)1);
   *
   *    bool x = Y > Z;                              // false is saved
   *    cout << std::boolalpha << (Y > Z) << endl;   // output false
   *    cout << std::boolalpha << (U > Z) << endl;   // output false
   *    X->insLeft(Y > Z);                           // insert Left (bool)false
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (CHAR) comparator
   * @return true if Y &gt; X, false otherwise
   */


  /**
   * @fn SlipDatum::operator!=(const ULONG     X)
   * @brief The 'not equal to' (!=) operator compares two SlipDatum cells (<b>Y</b> != (ULONG)<b>X</b>).
   * <p>
   * The values of a SlipDatum cell and a ULONG are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y != (ULONG)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y and (ULONG)X are not equal and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  ULONG      |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((ULONG)3);
   *    ULONG        Z = 3;
   *    SlipDatum    U((ULONG)1);
   *
   *    bool x = Y != Z;                             // false is saved
   *    cout << std::boolalpha << (Y != Z) << endl;  // output true
   *    cout << std::boolalpha << (U != Z) << endl;  // output true
   *    X->insLeft(Y != Z);                          // insert Left (bool)false
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (ULONG) comparator
   * @return true if Y != X, false otherwise
   */

  /**
   * @fn SlipDatum::operator<(const ULONG      X) 
   * @brief The 'less than' (&lt;) operator compares two SlipDatum cells (<b>Y</b> &lt; (ULONG)<b>X</b>).
   * <p>
   * The values of a SlipDatum cell and a ULONG are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y &lt; (ULONG)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is less than (ULONG)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  ULONG      |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((ULONG)3);
   *    ULONG        Z = 3;
   *    SlipDatum    U((ULONG)1);
   *
   *    bool x = Y < Z;                              // false is saved
   *    cout << std::boolalpha << (Y < Z) << endl;   // output false
   *    cout << std::boolalpha << (U < Z) << endl;   // output true
   *    X->insLeft(Y < Z);                           // insert Left (bool)false
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (ULONG) comparator
   * @return true if Y &lt; X, false otherwise
   */

  /**
   * @fn SlipDatum::operator<=(const ULONG     X)
   * @brief The 'less than or equal to' (&lt;=) operator compares two SlipDatum cells (<b>Y</b> &lt;= (ULONG)<b>X</b>).
   * <p>
   * The values of a SlipDatum cell and a ULONG are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y &lt;= (ULONG)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is less than or equal to (ULONG)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  ULONG      |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((ULONG)3);
   *    ULONG        Z = 3;
   *    SlipDatum    U((ULONG)1);
   *
   *    bool x = Y <= Z;                             // true is saved
   *    cout << std::boolalpha << (Y <= Z) << endl;  // output true
   *    cout << std::boolalpha << (U <= Z) << endl;  // output true
   *    X->insLeft(Y <= Z);                          // insert Left (bool)true
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (ULONG) comparator
   * @return true if Y &lt;= X, false otherwise
   */

  /**
   * @fn SlipDatum::operator==(const ULONG     X)
   * @brief The 'equal to' (==) operator compares two SlipDatum cells (<b>Y</b> == (ULONG)<b>X</b>).
   * <p>
   * The values of a SlipDatum cell and a ULONG are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y == (ULONG)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is equal to (ULONG)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  ULONG      |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((ULONG)3);
   *    ULONG        Z = 3;
   *    SlipDatum    U((ULONG)1);
   *
   *    bool x = Y == Z;                             // true is saved
   *    cout << std::boolalpha << (Y == Z) << endl;  // output true
   *    cout << std::boolalpha << (U == Z) << endl;  // output false
   *    X->insLeft(Y == Z);                          // insert Left (bool)true
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (ULONG) comparator
   * @return true if Y == X, false otherwise
   */

  /**
   * @fn SlipDatum::operator>=(const ULONG     X)
   * @brief The 'greater than or equal to' (&gt;=) operator compares two SlipDatum cells (<b>Y</b> &gt;= (ULONG)<b>X</b>).
   * <p>
   * The values of a SlipDatum cell and a ULONG are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y &gt;= (ULONG)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is greater than or equal to (ULONG)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  ULONG      |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    ULONG        Z = 3;
   *    SlipDatum    Z((ULONG)3);
   *    SlipDatum    U((ULONG)1);
   *
   *    bool x = Y >= Z;                             // true is saved
   *    cout << std::boolalpha << (Y >= Z) << endl;  // output true
   *    cout << std::boolalpha << (U >= Z) << endl;  // output false
   *    X->insLeft(Y >= Z);                          // insert Left (bool)true
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (ULONG) comparator
   * @return true if Y &gt;= X, false otherwise
   */

  /**
   * @fn SlipDatum::operator>(const ULONG      X)
   * @brief The 'greater than' (&gt;) operator compares two SlipDatum cells (<b>Y</b> &gt; (ULONG)<b>X</b>).
   * <p>
   * The values of a SlipDatum cell and a ULONG are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y &gt; (ULONG)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is greater than (ULONG)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  ULONG      |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((ULONG)3);
   *    ULONG        Z = 3;
   *    SlipDatum    U((ULONG)1);
   *
   *    bool x = Y > Z;                              // false is saved
   *    cout << std::boolalpha << (Y > Z) << endl;   // output false
   *    cout << std::boolalpha << (U > Z) << endl;   // output false
   *    X->insLeft(Y > Z);                           // insert Left (bool)false
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (ULONG) comparator
   * @return true if Y &gt; X, false otherwise
   */


  /**
   * @fn SlipDatum::operator!=(const LONG     X)
   * @brief The 'not equal to' (!=) operator compares two SlipDatum cells (<b>Y</b> != (LONG)<b>X</b>).
   * <p>
   * The values of a SlipDatum cell and a LONG are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y != (LONG)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y and (LONG)X are not equal and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  LONG       |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((LONG)3);
   *    LONG         Z = 3;
   *    SlipDatum    U((LONG)1);
   *
   *    bool x = Y != Z;                             // false is saved
   *    cout << std::boolalpha << (Y != Z) << endl;  // output true
   *    cout << std::boolalpha << (U != Z) << endl;  // output true
   *    X->insLeft(Y != Z);                          // insert Left (bool)false
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (LONG) comparator
   * @return true if Y != X, false otherwise
   */

  /**
   * @fn SlipDatum::operator<(const LONG      X)
   * @brief The 'less than' (&lt;) operator compares two SlipDatum cells (<b>Y</b> &lt; (LONG)<b>X</b>).
   * <p>
   * The values of a SlipDatum cell and a LONG are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y &lt; (LONG)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is less than (LONG)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  LONG       |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((LONG)3);
   *    LONG         Z = 3;
   *    SlipDatum    U((LONG)1);
   *
   *    bool x = Y < Z;                              // false is saved
   *    cout << std::boolalpha << (Y < Z) << endl;   // output false
   *    cout << std::boolalpha << (U < Z) << endl;   // output true
   *    X->insLeft(Y < Z);                           // insert Left (bool)false
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (LONG) comparator
   * @return true if Y &lt; X, false otherwise
   */

  /**
   * @fn SlipDatum::operator<=(const LONG     X)
   * @brief The 'less than or equal to' (&lt;=) operator compares two SlipDatum cells (<b>Y</b> &lt;= (LONG)<b>X</b>).
   * <p>
   * The values of a SlipDatum cell and a LONG are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y &lt;= (LONG)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is less than or equal to (LONG)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  LONG       |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((LONG)3);
   *    LONG         Z = 3;
   *    SlipDatum    U((LONG)1);
   *
   *    bool x = Y <= Z;                             // true is saved
   *    cout << std::boolalpha << (Y <= Z) << endl;  // output true
   *    cout << std::boolalpha << (U <= Z) << endl;  // output true
   *    X->insLeft(Y <= Z);                          // insert Left (bool)true
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (LONG) comparator
   * @return true if Y &lt;= X, false otherwise
   */

  /**
   * @fn SlipDatum::operator==(const LONG     X)
   * @brief The 'equal to' (==) operator compares two SlipDatum cells (<b>Y</b> == (LONG)<b>X</b>).
   * <p>
   * The values of a SlipDatum cell and a LONG are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y == (LONG)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is equal to (LONG)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  LONG       |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((LONG)3);
   *    LONG         Z = 3;
   *    SlipDatum    U((LONG)1);
   *
   *    bool x = Y == Z;                             // true is saved
   *    cout << std::boolalpha << (Y == Z) << endl;  // output true
   *    cout << std::boolalpha << (U == Z) << endl;  // output false
   *    X->insLeft(Y == Z);                          // insert Left (bool)true
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (LONG) comparator
   * @return true if Y == X, false otherwise
   */

  /**
   * @fn SlipDatum::operator>=(const LONG     X)
   * @brief The 'greater than or equal to' (&gt;=) operator compares two SlipDatum cells (<b>Y</b> &gt;= (LONG)<b>X</b>).
   * <p>
   * The values of a SlipDatum cell and a LONG are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y &gt;= (LONG)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is greater than or equal to (LONG)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  LONG       |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((LONG)3);
   *    LONG         Z = 3;
   *    SlipDatum    U((LONG)1);
   *
   *    bool x = Y >= Z;                             // true is saved
   *    cout << std::boolalpha << (Y >= Z) << endl;  // output true
   *    cout << std::boolalpha << (U >= Z) << endl;  // output false
   *    X->insLeft(Y >= Z);                          // insert Left (bool)true
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (LONG) comparator
   * @return true if Y &gt;= X, false otherwise
   */

  /**
   * @fn SlipDatum::operator>(const LONG      X)
   * @brief The 'greater than' (&gt;) operator compares two SlipDatum cells (<b>Y</b> &gt; (LONG)<b>X</b>).
   * <p>
   * The values of a SlipDatum cell and a LONG are compared. The comparison
   * is equivalent to the C++ operation (&lt;type1&gt;)Y &gt; (LONG)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is greater than (LONG)X and <b>false</b>
   * otherwise.</p>
   * <p>If the types are not compatible then <b>false</b> is returned, otherwise
   *    silent casting is done as required and the resultants compared.</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  LONG       |
   * | :---------- | :---------: |
   * | bool        |      <b>X</b>      |
   * | CHAR        |      <b>X</b>      |
   * | UCHAR       |      <b>X</b>      |
   * | LONG        |      <b>X</b>      |
   * | ULONG       |      <b>X</b>      |
   * | PTR         |             |
   * | STRING      |             |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible. <em>bool</em> is
   *    converted to '1' if <b>true</b> and '0' if <b>false</b>.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y((LONG)3);
   *    LONG         Z = 3;
   *    SlipDatum    U((LONG)1);
   *
   *    bool x = Y > Z;                              // false is saved
   *    cout << std::boolalpha << (Y > Z) << endl;   // output false
   *    cout << std::boolalpha << (U > Z) << endl;   // output false
   *    X->insLeft(Y > Z);                           // insert Left (bool)false
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (LONG) comparator
   * @return true if Y &gt; X, false otherwise
   */



  /**
   * @brief The 'not equal to' (!=) operator compares two SlipDatum cells (<b>Y</b> != (string&)<b>X</b>).
   * <p>
   * The values of a SlipDatum cell and a <em>string</em> are compared. The
   * comparison  is equivalent to the C++ operation (&lt;type1&gt;)Y != (string&)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y and (<em>string</em>)X are not equal and
   * <b>false</b> otherwise.</p>
   * <p>If the type of Y (the SlipDatum cell) is not STRING then <b>false</b>
   * is returned</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  STRING     |
   * | :---------- | :---------: |
   * | bool        |             |
   * | CHAR        |             |
   * | UCHAR       |             |
   * | LONG        |             |
   * | ULONG       |             |
   * | PTR         |             |
   * | STRING      |      <b>X</b>      |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y("abc");
   *    string       Z("abd");
   *    SlipDatum    U((ULONG)1);
   *
   *    bool x = Y != "abc";                             // false is saved
   *    cout << std::boolalpha << (Y != "abd") << endl;  // output false
   *    cout << std::boolalpha << (U != "abc") << endl;  // output false
   *    X->insLeft(Y != "abc");                          // insert Left (bool)false
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (string&) comparator
   * @return true if Y != X, false otherwise
   */
   bool       SlipDatum::operator!=(const string&  X) { return !((SlipOp*)*getOperator())->equal(*this,     X); }// Y != X

  /**
   * @brief The 'less than' (&lt;) operator compares two SlipDatum cells (<b>Y</b> &lt; (string&)<b>X</b>).
   * <p>
   * The values of a SlipDatum cell and a <em>string</em> are compared. The
   * comparison  is equivalent to the C++ operation (&lt;type1&gt;)Y &lt; (string&)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is less than (<em>string</em>)X <b>false</b>
   * otherwise.</p>
   * <p>If the type of Y (the SlipDatum cell) is not STRING then <b>false</b>
   * is returned</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  STRING     |
   * | :---------- | :---------: |
   * | bool        |             |
   * | CHAR        |             |
   * | UCHAR       |             |
   * | LONG        |             |
   * | ULONG       |             |
   * | PTR         |             |
   * | STRING      |      <b>X</b>      |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y("abc");
   *    string       Z("abd");
   *    SlipDatum    U((ULONG)1);
   *
   *    bool x = Y < "abc";                              // false is saved
   *    cout << std::boolalpha << (Y < "abd") << endl;   // output true
   *    cout << std::boolalpha << (U < "abc") << endl;   // output false
   *    X->insLeft(Y < "abc");                           // insert Left (bool)false
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (string&) comparator
   * @return true if Y &lt; X, false otherwise
   */
   bool       SlipDatum::operator<(const string&   X) { return ((SlipOp*)*getOperator())->lessThan(*this,   X); }// Y <  X

  /**
   * @brief The 'less than or equal to' (&lt;=) operator compares two SlipDatum cells (<b>Y</b> &lt;= (string&)<b>X</b>).
   * <p>
   * The values of a SlipDatum cell and a <em>string</em> are compared. The
   * comparison  is equivalent to the C++ operation (&lt;type1&gt;)Y &lt;= (string&)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is less than or equal to (<em>string</em>)X
   * <b>false</b> otherwise.</p>
   * <p>If the type of Y (the SlipDatum cell) is not STRING then <b>false</b>
   * is returned</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  STRING     |
   * | :---------- | :---------: |
   * | bool        |             |
   * | CHAR        |             |
   * | UCHAR       |             |
   * | LONG        |             |
   * | ULONG       |             |
   * | PTR         |             |
   * | STRING      |      <b>X</b>      |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y("abc");
   *    string       Z("abd");
   *    SlipDatum    U((ULONG)1);
   *
   *    bool x = Y <= "abc";                              // true is saved
   *    cout << std::boolalpha << (Y <= "abd") << endl;   // output true
   *    cout << std::boolalpha << (U <= "abc") << endl;   // output false
   *    X->insLeft(Y <= "abc");                           // insert Left (bool)true
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (string&) comparator
   * @return true if Y &lt;= X, false otherwise
   */
   bool       SlipDatum::operator<=(const string&  X) { return ((SlipOp*)*getOperator())->lessEqual(*this,  X); }// Y <= X

  /**
   * @brief The 'equal to' (==) operator compares two SlipDatum cells (<b>Y</b> == (string&)<b>X</b>).
   * <p>
   * The values of a SlipDatum cell and a <em>string</em> are compared. The
   * comparison  is equivalent to the C++ operation (&lt;type1&gt;)Y == (string&)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is equal to (<em>string</em>)X <b>false</b>
   * otherwise.</p>
   * <p>If the type of Y (the SlipDatum cell) is not STRING then <b>false</b>
   * is returned</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  STRING     |
   * | :---------- | :---------: |
   * | bool        |             |
   * | CHAR        |             |
   * | UCHAR       |             |
   * | LONG        |             |
   * | ULONG       |             |
   * | PTR         |             |
   * | STRING      |      <b>X</b>      |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y("abc");
   *    string       Z("abd");
   *    SlipDatum    U((ULONG)1);
   *
   *    bool x = Y == "abc";                              // true is saved
   *    cout << std::boolalpha << (Y == "abd") << endl;   // output false
   *    cout << std::boolalpha << (U == "abc") << endl;   // output false
   *    X->insLeft(Y == "abc");                           // insert Left (bool)true
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (string&) comparator
   * @return true if Y == X, false otherwise
   */
   bool       SlipDatum::operator==(const string&  X) { return ((SlipOp*)*getOperator())->equal(*this,      X); }// Y == X

  /**
   * @brief The 'greater than or equal to' (&gt;=) operator compares two SlipDatum cells (<b>Y</b> &gt;= (string&)<b>X</b>).
   * <p>
   * The values of a SlipDatum cell and a <em>string</em> are compared. The
   * comparison  is equivalent to the C++ operation (&lt;type1&gt;)Y &gt;= (string&)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is greater than or equal to (<em>string</em>)X
   * <b>false</b> otherwise.</p>
   * <p>If the type of Y (the SlipDatum cell) is not STRING then <b>false</b>
   * is returned</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  STRING     |
   * | :---------- | :---------: |
   * | bool        |             |
   * | CHAR        |             |
   * | UCHAR       |             |
   * | LONG        |             |
   * | ULONG       |             |
   * | PTR         |             |
   * | STRING      |      <b>X</b>      |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y("abc");
   *    string       Z("abd");
   *    SlipDatum    U((ULONG)1);
   *
   *    bool x = Y >= "abc";                              // true is saved
   *    cout << std::boolalpha << (Y >= "abd") << endl;   // output true
   *    cout << std::boolalpha << (U >= "abc") << endl;   // output false
   *    X->insLeft(Y >= "abc");                           // insert Left (bool)true
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (string&) comparator
   * @return true if Y &gt;= X, false otherwise
   */
   bool       SlipDatum::operator>=(const string&  X) { return ((SlipOp*)*getOperator())->greatEqual(*this, X); }// Y >= X

  /**
   * @brief The 'greater than' (&gt;) operator compares two SlipDatum cells (<b>Y</b> &gt; (string&)<b>X</b>).
   * <p>
   * The values of a SlipDatum cell and a <em>string</em> are compared. The
   * comparison  is equivalent to the C++ operation (&lt;type1&gt;)Y &gt; (string&)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is less than or equal to (<em>string</em>)X
   * <b>false</b> otherwise.</p>
   * <p>If the type of Y (the SlipDatum cell) is not STRING then <b>false</b>
   * is returned</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  STRING     |
   * | :---------- | :---------: |
   * | bool        |             |
   * | CHAR        |             |
   * | UCHAR       |             |
   * | LONG        |             |
   * | ULONG       |             |
   * | PTR         |             |
   * | STRING      |      <b>X</b>      |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y("abc");
   *    string       Z("abd");
   *    SlipDatum    U((ULONG)1);
   *
   *    bool x = Y > "abc";                               // false is saved
   *    cout << std::boolalpha << (Y > "abd") << endl;    // output false
   *    cout << std::boolalpha << (U > "abc") << endl;    // output false
   *    X->insLeft(Y > "abc");                            // insert Left (bool)false
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (string&) comparator
   * @return true if Y &gt; X, false otherwise
   */
   bool       SlipDatum::operator>(const string&   X) { return ((SlipOp*)*getOperator())->greatThan(*this,  X); }// Y >  X


  /**
   * @brief The 'not equal to' (!=) operator compares two SlipDatum cells (<b>Y</b> != (string*)<b>X</b>).
   * <p>X is cast into a reference (string&) before the operation begines.</p>
   * <p>
   * The values of a SlipDatum cell and a <em>string</em> are compared. The
   * comparison  is equivalent to the C++ operation (&lt;type1&gt;)Y != (string*)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y and (<em>string</em>)X are not equal and
   * <b>false</b> otherwise.</p>
   * <p>If the type of Y (the SlipDatum cell) is not STRING then <b>false</b>
   * is returned</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  STRING     |
   * | :---------- | :---------: |
   * | bool        |             |
   * | CHAR        |             |
   * | UCHAR       |             |
   * | LONG        |             |
   * | ULONG       |             |
   * | PTR         |             |
   * | STRING      |      <b>X</b>      |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y("abc");
   *    string*      Z  = new string("abd");
   *    SlipDatum    U((ULONG)1);
   *
   *    bool x = Y != Z;                                 // false is saved
   *    cout << std::boolalpha << (Y != Z)<< endl;       // output false
   *    cout << std::boolalpha << (U != Z)<< endl;       // output false
   *    X->insLeft(Y != Z);                              // insert Left (bool)false
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (string*) comparator
   * @return true if Y != X, false otherwise
   */
   bool       SlipDatum::operator!=(const string*  X) { return !((SlipOp*)*getOperator())->equal(*this,    *X); }// Y != X

  /**
   * @brief The 'less than' (&lt;) operator compares two SlipDatum cells (<b>Y</b> &lt; (string*)<b>X</b>).
   * <p>X is cast into a reference (string&) before the operation begines.</p>
   * <p>
   * The values of a SlipDatum cell and a <em>string</em> are compared. The
   * comparison  is equivalent to the C++ operation (&lt;type1&gt;)Y &lt; (string*)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is less than (<em>string</em>)X <b>false</b>
   * otherwise.</p>
   * <p>If the type of Y (the SlipDatum cell) is not STRING then <b>false</b>
   * is returned</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  STRING     |
   * | :---------- | :---------: |
   * | bool        |             |
   * | CHAR        |             |
   * | UCHAR       |             |
   * | LONG        |             |
   * | ULONG       |             |
   * | PTR         |             |
   * | STRING      |      <b>X</b>      |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y("abc");
   *    string*      Z  = new string("abd");
   *    SlipDatum    U((ULONG)1);
   *
   *    bool x = Y < Z;                                  // false is saved
   *    cout << std::boolalpha << (Y < Z)<< endl;        // output true
   *    cout << std::boolalpha << (U < Z)<< endl;        // output false
   *    X->insLeft(Y < Z);                               // insert Left (bool)false
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (string*) comparator
   * @return true if Y &lt; X, false otherwise
   */
   bool       SlipDatum::operator<(const string*   X) { return ((SlipOp*)*getOperator())->lessThan(*this,  *X); }// Y <  X

  /**
   * @brief The 'less than or equal to' (&lt;=) operator compares two SlipDatum cells (<b>Y</b> &lt;= (string*)<b>X</b>).
   * <p>X is cast into a reference (string&) before the operation begines.</p>
   * <p>
   * The values of a SlipDatum cell and a <em>string</em> are compared. The
   * comparison  is equivalent to the C++ operation (&lt;type1&gt;)Y &lt;= (string*)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is less than or equal to (<em>string</em>)X
   * <b>false</b> otherwise.</p>
   * <p>If the type of Y (the SlipDatum cell) is not STRING then <b>false</b>
   * is returned</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  STRING     |
   * | :---------- | :---------: |
   * | bool        |             |
   * | CHAR        |             |
   * | UCHAR       |             |
   * | LONG        |             |
   * | ULONG       |             |
   * | PTR         |             |
   * | STRING      |      <b>X</b>      |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y("abc");
   *    string*      Z  = new string("abd");
   *    SlipDatum    U((ULONG)1);
   *
   *    bool x = Y <= Z;                                  // true is saved
   *    cout << std::boolalpha << (Y <= Z)<< endl;        // output true
   *    cout << std::boolalpha << (U <= Z)<< endl;        // output false
   *    X->insLeft(Y <= Z);                               // insert Left (bool)true
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (string*) comparator
   * @return true if Y &lt;= X, false otherwise
   */
   bool       SlipDatum::operator<=(const string*  X) { return ((SlipOp*)*getOperator())->lessEqual(*this, *X); }// Y <= X

  /**
   * @brief The 'equal to' (==) operator compares two SlipDatum cells (<b>Y</b> == (string*)<b>X</b>).
   * <p>X is cast into a reference (string&) before the operation begines.</p>
   * <p>
   * The values of a SlipDatum cell and a <em>string</em> are compared. The
   * comparison  is equivalent to the C++ operation (&lt;type1&gt;)Y == (string*)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is equal to (<em>string</em>)X <b>false</b>
   * otherwise.</p>
   * <p>If the type of Y (the SlipDatum cell) is not STRING then <b>false</b>
   * is returned</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  STRING     |
   * | :---------- | :---------: |
   * | bool        |             |
   * | CHAR        |             |
   * | UCHAR       |             |
   * | LONG        |             |
   * | ULONG       |             |
   * | PTR         |             |
   * | STRING      |      <b>X</b>      |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y("abc");
   *    string*      Z  = new string("abd");
   *    SlipDatum    U((ULONG)1);
   *
   *    bool x = Y == Z;                                  // true is saved
   *    cout << std::boolalpha << (Y == Z)<< endl;        // output false
   *    cout << std::boolalpha << (U == Z)<< endl;        // output false
   *    X->insLeft(Y == Z);                               // insert Left (bool)true
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (string*) comparator
   * @return true if Y == X, false otherwise
   */
   bool       SlipDatum::operator==(const string*  X) { return ((SlipOp*)*getOperator())->equal(*this,     *X); }// Y == X

  /**
   * @brief The 'greater than or equal to' (&gt;=) operator compares two SlipDatum cells (<b>Y</b> &gt;= (string*)<b>X</b>).
   * <p>X is cast into a reference (string&) before the operation begines.</p>
   * <p>
   * The values of a SlipDatum cell and a <em>string</em> are compared. The
   * comparison  is equivalent to the C++ operation (&lt;type1&gt;)Y &gt;= (string*)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is greater than or equal to (<em>string</em>)X
   * <b>false</b> otherwise.</p>
   * <p>If the type of Y (the SlipDatum cell) is not STRING then <b>false</b>
   * is returned</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  STRING     |
   * | :---------- | :---------: |
   * | bool        |             |
   * | CHAR        |             |
   * | UCHAR       |             |
   * | LONG        |             |
   * | ULONG       |             |
   * | PTR         |             |
   * | STRING      |      <b>X</b>      |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y("abc");
   *    string*      Z  = new string("abd");
   *    SlipDatum    U((ULONG)1);
   *
   *    bool x = Y >= Z;                                  // true is saved
   *    cout << std::boolalpha << (Y >= Z)<< endl;        // output true
   *    cout << std::boolalpha << (U >= Z)<< endl;        // output false
   *    X->insLeft(Y >= Z);                               // insert Left (bool)true
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (string*) comparator
   * @return true if Y &gt;= X, false otherwise
   */
   bool       SlipDatum::operator>=(const string*  X) { return ((SlipOp*)*getOperator())->greatEqual(*this,*X); }// Y >= X

  /**
   * @brief The 'greater than' (&gt;) operator compares two SlipDatum cells (<b>Y</b> &gt; (string*)<b>X</b>).
   * <p>X is cast into a reference (string&) before the operation begines.</p>
   * <p>
   * The values of a SlipDatum cell and a <em>string</em> are compared. The
   * comparison  is equivalent to the C++ operation (&lt;type1&gt;)Y &gt; (string*)X.
   * The result of the comparison is the same as the C++ equivalent with
   * <b>true</b> returned if Y is less than or equal to (<em>string</em>)X
   * <b>false</b> otherwise.</p>
   * <p>If the type of Y (the SlipDatum cell) is not STRING then <b>false</b>
   * is returned</p>
   * <p><center><b>Type Comparison Table</b></center></p>
   * <center>
   * | LHS/RHS     |  STRING     |
   * | :---------- | :---------: |
   * | bool        |             |
   * | CHAR        |             |
   * | UCHAR       |             |
   * | LONG        |             |
   * | ULONG       |             |
   * | PTR         |             |
   * | STRING      |      <b>X</b>      |
   * </center>
   * <p>Where <b>X</b> denotes that the types are compatible.</p>
   * <p>
   * Some examples of use are:</p>
   * <tt><pre>
   *    SlipHeader * X = new SlipHeader();
   *    SlipDatum    Y("abc");
   *    string*      Z  = new string("abd");
   *    SlipDatum    U((ULONG)1);
   *
   *    bool x = Y > Z;                                   // false is saved
   *    cout << std::boolalpha << (Y > Z)<< endl;         // output false
   *    cout << std::boolalpha << (U > Z)<< endl;         // output false
   *    X->insLeft(Y > Z);                                // insert Left (bool)false
   *    X.deleteList();
   * </pre></tt>
   *
   * @param[in] X (string*) comparator
   * @return true if Y &gt; X, false otherwise
   */
   bool       SlipDatum::operator>(const string*   X) { return ((SlipOp*)*getOperator())->greatThan(*this, *X); }// Y >  X


               /*****************************************
                *          Manipulating Lists           *
               ******************************************/

   /**
    * @brief Replace the current list object (LHS) with X (RHS).
    * <p>If X is Y then ignore the request.</p>
    * <p>Do a <em>remove()</em> of data pointers if Y is a PTR or STRING type.</p>
    * <p> If X is a SlipDatum type then effectively do a Y = X, i.e.,
    *    replace the operator and value fields in the SLIP cell with the
    *    those of X. This step does not require the deletion or creation
    *    of a Slip cell.</p>
    * <p>If X is a SlipSublist or SlipHeader type, delete Y and replace it with
    *    a copy of X. If X is of type SlipSublist, then a new sublist is created
    *    referencing the same header (replacing the Y with a sublist). If X is
    *    of type SlipHeader then a new sublist is created as above, with X
    *    provided as the header reference.</p>
    * <p>In all cases the new (or reused) object is placed into the
    *    same location as Y.</p>
    * <p>References and pointers to Y are valid only if X is not a
    *    SlipHeader or SlipSublist.</p>
    * @param[in] X (SlipCell&) Right Hand Side of the replacement
    * @return a reference to the new object
    */
   SlipCell& SlipDatum::replace(const SlipCell& X) {                            // Replace a cell on a list with a new cell
      SlipCell* cell = this;
      if (cell != &X) {
         if (X.isData()) {                                                      // Y = X
            assignPreamble();
            assignEpilogue((SlipDatum&)X);
         } else {                                                               // Y = replace(Header or Sublist)
            cell = new SlipSublist(const_cast<SlipCell&>(X));
            replaceLinks(*cell);
            delete this;
         }
      }
      return *cell;
   };

   /**
    * @brief Replace the current list object (LHS) with X (RHS).
    * <p>Do a <em>remove()</em> of data pointers if Y is a PTR or STRING type.</p>
    * <p> Effectively do a Y = X, i.e., replace the operator field with a
    *     bool operator and the value field with the value of X.</p>
    * @param[in] X (bool) Right Hand Side of the replacement
    * @return a reference to the object in the list
    */
   SlipCell& SlipDatum::replace(bool X)  { assignPreamble(); setOperator((void*)boolOP);   *getBool()  = X; return *this; }    // Y = V

   /**
    * @brief Replace the current list object (LHS) with X (RHS).
    * <p>Do a <em>remove()</em> of data pointers if Y is a PTR or STRING type.</p>
    * <p> Effectively do a Y = X, i.e., replace the operator field with a
    *     CHAR operator and the value field with the value of X.</p>
    * @param[in] X (CHAR) Right Hand Side of the replacement
    * @return a reference to the object in the list
    */
   SlipCell& SlipDatum::replace(CHAR X)  { assignPreamble(); setOperator((void*)charOP);   *getChar()  = X; return *this; }    // Y = V

   /**
    * @brief Replace the current list object (LHS) with X (RHS).
    * <p>Do a <em>remove()</em> of data pointers if Y is a PTR or STRING type.</p>
    * <p> Effectively do a Y = X, i.e., replace the operator field with a
    *     UCHAR operator and the value field with the value of X.</p>
    * @param[in] X (UCHAR) Right Hand Side of the replacement
    * @return a reference to the object in the list
    */
   SlipCell& SlipDatum::replace(UCHAR X) { assignPreamble(); setOperator((void*)ucharOP);  *getUChar() = X; return *this; }    // Y = V

   /**
    * @brief Replace the current list object (LHS) with X (RHS).
    * <p>Do a <em>remove()</em> of data pointers if Y is a PTR or STRING type.</p>
    * <p> Effectively do a Y = X, i.e., replace the operator field with a
    *     LONG operator and the value field with the value of X.</p>
    * @param[in] X (LONG) Right Hand Side of the replacement
    * @return a reference to the object in the list
    */
   SlipCell& SlipDatum::replace(LONG X)  { assignPreamble(); setOperator((void*)longOP);   *getLong()  = X; return *this; }    // Y = V

   /**
    * @brief Replace the current list object (LHS) with X (RHS).
    * <p>Do a <em>remove()</em> of data pointers if Y is a PTR or STRING type.</p>
    * <p> Effectively do a Y = X, i.e., replace the operator field with a
    *     ULONG operator and the value field with the value of X.</p>
    * @param[in] X (ULONG) Right Hand Side of the replacement
    * @return a reference to the object in the list
    */
   SlipCell& SlipDatum::replace(ULONG X) { assignPreamble(); setOperator((void*)ulongOP);  *getULong() = X; return *this; }    // Y = V

   /**
    * @brief Replace the current list object (LHS) with X (RHS).
    * <p>Do a <em>remove()</em> of data pointers if Y is a PTR or STRING type.</p>
    * <p> Effectively do a Y = X, i.e., replace the operator field with a
    *     DOUBLE operator and the value field with the value of X.</p>
    * @param[in] X (DOUBLE) Right Hand Side of the replacement
    * @return a reference to the object in the list
    */
   SlipCell& SlipDatum::replace(DOUBLE X){ assignPreamble(); setOperator((void*)doubleOP); *getDouble() = X; return *this; }   // Y = V

   /**
    * @brief Replace the current list object (LHS) with X (RHS).
    * <p>Do a <em>remove()</em> of data pointers if Y is a PTR or STRING type.</p>
    * <p> Effectively do a Y = X, i.e., replace the operator field with a
    *     PTR operator and the value field with the copy of X (<em>X-&gt;copy()</em>).
    * <p>The operations field provides a means to specify user defined operations.
    *    The pointer must be to a SlipOp defined class. The standard casting,
    *    unary, arithmetic, bitwise and logical operations must all be overridden.
    *    The result is that a user Defined object is treated as a atomic value
    *    in all computations.</p>
    * @param[in] X (PTR) Right Hand Side of the replacement
    * @param[in] operation (void*) SlipOp derived User Defined operations
    * @return reference to the inserted object
    */
   SlipCell& SlipDatum::replace(const PTR X, const void* operation) {           // Replace a cell on a list with a new cell
      assignPreamble();
      setOperator((void*)operation);
      *getPtr() = X->copy();
      return *this;
   }; // SlipCell& SlipDatum::replace(const PTR X, const void* operation)

   /**
    * @brief Replace the current list object (LHS) with X (RHS).
    * <p>Do a <em>remove()</em> of data pointers if Y is a PTR or STRING type.</p>
    * <p> Effectively do a Y = X, i.e., replace the operator field with a
    *     STRING operator and insert a new SlipPointer type in the value
    *     field. The new SlipPointer either defines a constant string or
    *     a variable string with respect to the SLIP kernel depending on
    *     the user supplied <em>constFlag</em> . A 'constant' string
    *     implies that a SlipPointer <em>copy()</em> operation will return
    *     a pointer to itself. A variable ('non-constant') string requires
    *     that a <em>copy()</em> operation create and return a new SlipPointer
    *     object with a new copy of the string.</p>
    * @param[in] X (string&) Right Hand Side of the replacement
    * @param[in] constFlag (bool) <b>true></b> constant string <b>false</b> inconstant string
    * @return a reference to the object in the list
    */
   SlipCell& SlipDatum::replace(const string& X, bool constFlag) {              // Replace a cell on a list with a new cell
      assignPreamble();
      setOperator((void*)stringOP);
      if (constFlag) *getString() = new SlipStringConst(&X);
      else           *getString() = new SlipStringNonConst(&X);
      return *this;
   }; // SlipCell& SlipDatum::replace(const string& X, bool constFlag)

   /**
    * @brief Replace the current list object (LHS) with X (RHS).
    * <p>Do a <em>remove()</em> of data pointers if Y is a PTR or STRING type.</p>
    * <p> Effectively do a Y = X, i.e., replace the operator field with a
    *     STRING operator and insert a new SlipPointer type in the value
    *     field. The new SlipPointer either defines a constant string or
    *     a variable string with respect to the SLIP kernel depending on
    *     the user supplied <em>constFlag</em> . A 'constant' string
    *     implies that a SlipPointer <em>copy()</em> operation will return
    *     a pointer to itself. A variable ('non-constant') string requires
    *     that a <em>copy()</em> operation create and return a new SlipPointer
    *     object with a new copy of the string.</p>
    * @param[in] X (string*) Right Hand Side of the replacement
    * @param[in] constFlag (bool) <b>true></b> constant string <b>false</b> inconstant string
    * @return a reference to the object in the list
    */
   SlipCell& SlipDatum::replace(const string* X, bool constFlag) {              // Replace a cell on a list with a new cell
      assignPreamble();
      setOperator((void*)stringOP);
      if (constFlag) *getString() = new SlipStringConst(X);
      else           *getString() = new SlipStringNonConst(X);
      return *this;
   }; // SlipCell& SlipDatum::replace(const string* X, bool constFlag)

            /*****************************************
             *             Miscellaneous             *
            ******************************************/

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
    * @return (Parse) Pointer to the User Data parse function or <b>null</b>
    */
   Parse     SlipDatum::getParse() {                                            // Return a pointer to the User Data parse method
       return ((SlipOp*)*getOperator())->getParse(*this);
   }; // Parse SlipDatum::getParse()

   /**
    * @brief Return a string formatted for output.
    * <p>Each object formats a string for output. The output formats
    *    are:</p>
    * <center><table border="0">
    *    <tr>
    *       <th>SlipDatum()</th>
    *       <th>write</th>
    *    </tr>
    *    <tr>
    *       <td>bool</td>
    *       <td>true | false</td>
    *    </tr>
    *    <tr>
    *       <td>CHAR</td>
    *       <td>\#C</td>
    *    </tr>
    *    <tr>
    *       <td>UCHAR</td>
    *       <td>\#UC</td>
    *    </tr>
    *    <tr>
    *       <td>LONG</td>
    *       <td>\#L</td>
    *    </tr>
    *    <tr>
    *       <td>ULONG</td>
    *       <td>\#U</td>
    *    </tr>
    *    <tr>
    *       <td>DOUBLE</td>
    *       <td>\#.\#E&plusmn;\#</td>
    *    </tr>
    *    <tr>
    *       <td>string</td>
    *       <td>"string"</td>
    *    </tr>
    *    <tr>
    *       <td>PTR</td>
    *       <td>(user data)</td>
    *    </tr>
    * </table></center>
    * where '\#' stands for a decimal number and if the number is negative
    * it is preceded by a minus.
    * @return string suitable for outputting
    */
   string     SlipDatum::write() {
      return ((SlipOp*)*getOperator())->write(*this);
   }; // string     SlipDatum::write()

   /**
    * @brief Uglify the SlipDatum cell.
    * <p>All cell properties are output (left link, right link, operator
    *    pointer, and value).</p>
    * @param[in] out output stream
    * @param[in,out] col column in current line
    */
   void       SlipDatum::dump(ostream& out, int& col) {                        // Output the current cell
      string prefix = "";
      if (isDeleted()) {
         prefix = "DELETED ";
      }
      out << prefix << dump() << '\n';
   }; //  void SlipDatum::dump(ostream& out, int& col)

   //string     SlipDatum::toString() const  { return ((SlipOp*)*getOperator())->toString(*this); } // pretty print SLIP cell
   string     SlipDatum::dump() const { return SlipCellBase::dump( ); }         // dump cell

} // namespace slip