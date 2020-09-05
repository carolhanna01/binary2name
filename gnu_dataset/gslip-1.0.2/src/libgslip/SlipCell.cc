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
 * @file   SlipCell.cpp
 * @author A. Schwarz
 * @date   June 15, 2012
 *
 * @brief Contains the SlipCell definitions.
 */

# include "SlipDef.h"
# include "SlipCellBase.h"
# include "SlipCell.h"
# include "SlipDatum.h"
# include "SlipException.h"
# include "SlipErr.h"
# include "SlipGlobal.h"
# include "SlipHeader.h"
# include "SlipOp.h"
# include "SlipSublist.h"
# include "SlipHeader.h"

namespace slip {

   const int LINELENGTH = 100;                                                  // Maximum line length in output

       /*************************************************
        *                Utility Methods                *
        ************************************************/

   /**
    * @brief Assume the input must be a SlipHeader object and post an error if not.
    * <p>Assumes a header is needed and issues a error message if a valid
    *    SlipHeader object is not found.</p>
    * <p>This method will fail if:</p>
    * <ul>
    *    <li>The input reference (X) is not a SlipHeader object.</li>
    *    <li>The input reference (X) is a SlipHeader object but has been
    *        deleted - it is on the AVSL.</li>
    * </ul>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2038</b> Header required.</li>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    * </ul>
    * @sa checkThat
    * @param[in] X (SlipCell&) object reference
    * @param[in] methodName (string) to be used in error posting
    * @return <b>true</b> if X is a header and can be used
    */
   bool SlipCell::checkHeader(SlipCell& X,  string methodName) {
      bool flag = false;
      if (!X.isHeader()) {
         postError(__FILE__, __LINE__, SlipErr::E2038, methodName, "", "", X);
      } else if (checkThat(X, methodName)) {
         flag = true;
      }
      return flag;
   }; // bool SlipCell::checkHeader(SlipCell& X)

   /**
    * @brief Check if the input object can be a valid insert source.
    * <p>The input SlipCell object is not valid if:</p>
    * <ul>
    *    <li>The object is not from the AVSL originally.</li>
    *    <li>The object is on the stack.</li>
    *    <li>The object has been deleted and is on the AVSL.</li>
    * </ul>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2006</b> Cell can not be a local temporary. Create with 'new'.</li>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    *    <li><b>E3022</b> Can not insert a non-AVSL cell into a list.</li>
    * </ul>
    * @param[in] X (SlipCell&) object reference
    * @param[in] methodName (string) to be used in error posting
    * @return <b>true</b> if the input is in a list
    */
   bool SlipCell::checkThat(const SlipCell& X, string methodName) {             // Check if this is legal
      bool flag = false;
      if (!isAVSL(&X)) {
         postError(__FILE__, __LINE__, SlipErr::E3022, methodName, "", "", X);
      } else if (X.isTemp()) {
         postError(__FILE__, __LINE__, SlipErr::E2006, methodName, "", "", X);
      } else if (X.isDeleted()) {
         postError(__FILE__, __LINE__, SlipErr::E3019, methodName, "", "", X);
      } else {
         flag = true;
      }
      return flag;
   }; // bool SlipCell::checkThat(const string methodName)

   /**
    * @brief Check if the input object can be a valid insert destination.
    * <p>The input SlipCell object is not valid if:</p>
    * <ul>
    *    <li>The object is not from the AVSL originally.</li>
    *    <li>The object is on the stack.</li>
    *    <li>The object is not part of a list.</li>
    *    <li>The object has been deleted and is on the AVSL.</li>
    * </ul>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2006</b> Cell can not be a local temporary. Create with 'new'.</li>
    *    <li><b>E2013</b> Cell must be in a list. Cell is not in a list.</li>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    *    <li><b>E3022</b> Can not insert a non-AVSL cell into a list.</li>
    * </ul>
    * @param[in] X (SlipCell&) object reference
    * @param[in] methodName (string) to be used in error posting
    * @return <b>true</b> if the input is in a list
    */
   bool SlipCell::checkThis(const SlipCell& X, string methodName) {             // Check if this is legal
      bool flag = false;
      if (!isAVSL(&X)) {
         postError(__FILE__, __LINE__, SlipErr::E3022, methodName, "", "", X);
      } else if (X.isTemp()) {
         postError(__FILE__, __LINE__, SlipErr::E2006, methodName, "", "", X);
      } else if (X.isUnlinked()) {
         postError(__FILE__, __LINE__, SlipErr::E2013, methodName, "", "", X);
      } else if (X.isDeleted()) {
         postError(__FILE__, __LINE__, SlipErr::E3019, methodName, "", "", X);
      } else {
         flag = true;
      }
      return flag;
   }; // bool SlipCell::checkThis(const string methodName)

   /**
    * @fn SlipCell::copy(SlipCell& X)
    * @brief Create a copy of a cell.
    * @param X (SlipCell&) SlipCell to be copied
    * @return reference to a copy of a SlipCell
    */

   /**
    * @fn SlipCell::constructSlipCell(SlipCellBase& X)
    * @brief Create a new SlipCell object.
    * <p>The object creation method (<em>factory</em>) is a pure abstract
    *    method (PAM). The creation function has not the vaguest idea about
    *    what is about to happen and remains happily oblivious to its
    *    consequences.</p>
    * <p>A copy of the input object is created from the AVSL, and made
    *    available to the user.</p>
    * @sa factory
    * @param[in] X (SlipCell&) object reference
    * @return reference to a new SlipCell object
    */

            /*****************************************
             *           Manipulating Lists          *
            ******************************************/
   /**
    * @fn SlipCell::insertLeft(SlipCell&    X)
    * @brief Insert an input SlipCell object to the left of <b>this</b>.
    * <p>The input object must be a valid source, and the current (<b>this</b>)
    *    object a valid destination for the insertion. All checks to ensure
    *    validity must have already been done.</p>
    * <p>The input object link pointers are changed, then the leftLink pointer
    *    of the following and the rightLink pointer of the previous object (to
    *    <b>this</b>) are modified. The order of operations are important.</p>
    * @param[in] X (SlipCell&) object reference
    * @return reference to the altered input object
    */

   /**
    * @brief Insert a SlipCell to the left of the current SlipCell.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then checks are made to ensure that
    *    the input cell is valid. If both are valid then the operation is
    *    legitimate and the following will be done:</p>
    * <ul>
    *    <li>If the input cell is a SlipHeader object and linked then a
    *        SlipSublist will be created and inserted. The reference count
    *        of the referenced list header will be incremented. Both lists
    *        share a reference to the same list.</li>
    *    <li>If the input cell is a SlipHeader object and unlinked then the
    *        sublist will be inserted. Responsibility for deleted the cell
    *        must be done in the context of the list. If the cell is deleted
    *        then it will be removed from the list.</li>
    *    <li>If the input cell is linked onto a list, then a copy will be
    *        created and inserted.</li>
    *    <li>If the input cell is on the stack, then a copy will be created
    *        and inserted.</li>
    *    <li>Otherwise the input cell is inserted.</li>
    * </ul>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2006</b> Cell can not be a local temporary. Create with 'new'.</li>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    *    <li><b>E3022</b> Can not insert a non-AVSL cell into a list.</li>
    * </ul>
    * @sa checkThis
    * @sa checkThat
    * @sa factory
    * @param[in] X (SlipCell&) object reference
    * @return reference to the inserted object
    */
   SlipCell& SlipCell::insLeft(SlipCell&  X) {                                  // Insert X to current object left
      SlipCell* cell = &X;
      if (!checkThis(*this, "insLeft)")) return *this;
      if (!checkThat(X, "insLeft")) return *this;
      if (X.isHeader()) {
         cell = new SlipSublist((SlipHeader&)X);
      } else if (!X.isUnlinked() || X.isTemp()) {
         cell = &X.factory();
      }
      return insertLeft(*cell);
   }; // SlipCell& SlipCell::insLeft(SlipCell& X)

   /**
    * @brief Insert a bool object to the left of the current SlipCell.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa checkThis
    * @param[in] X (bool) object to be inserted
    * @return reference to the inserted object
    */
   SlipCell& SlipCell::insLeft(bool X) {                                        // Insert X to current object left
      if (!checkThis(*this, "insLeft")) return *this;
      SlipDatum* cell = new SlipDatum(X);
      return insertLeft(*cell);
   }; // SlipCell& SlipCell::insLeft(bool X)

   /**
    * @brief Insert a DOUBLE object to the left of the current SlipCell.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa checkThis
    * @param[in] X (DOUBLE) object to be inserted
    * @return reference to the inserted object
    */
   SlipCell& SlipCell::insLeft(DOUBLE X) {                                      // Insert X to current object left
      if (!checkThis(*this, "insLeft")) return *this;
      SlipDatum* cell = new SlipDatum(X);
      return insertLeft(*cell);
   }; // SlipCell& SlipCell::insLeft(DOUBLE X)

   /**
    * @brief Insert a ULONG object to the left of the current SlipCell.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa checkThis
    * @param[in] X (ULONG) object to be inserted
    * @return reference to the inserted object
    */
   SlipCell& SlipCell::insLeft(ULONG X) {                                       // Insert X to current object left
      if (!checkThis(*this, "insLeft")) return *this;
      SlipDatum* cell = new SlipDatum(X);
      return insertLeft(*cell);
   }; // SlipCell& SlipCell::insLeft(ULONG X)

   /**
    * @brief Insert a LONG object to the left of the current SlipCell.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa checkThis
    * @param[in] X (LONG) object to be inserted
    * @return reference to the inserted object
    */
   SlipCell& SlipCell::insLeft(LONG X) {                                        // Insert X to current object left
      if (!checkThis(*this, "insLeft")) return *this;
      SlipDatum* cell = new SlipDatum(X);
      return insertLeft(*cell);
   }; // SlipCell& SlipCell::insLeft(LONG X)

   /**
    * @brief Insert a UCHAR object to the left of the current SlipCell.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa checkThis
    * @param[in] X (UCHAR) object to be inserted
    * @return reference to the inserted object
    */
   SlipCell& SlipCell::insLeft(UCHAR X) {                                       // Insert X to current object left
      if (!checkThis(*this, "insLeft")) return *this;
      SlipDatum* cell = new SlipDatum(X);
      return insertLeft(*cell);
   }; // SlipCell& SlipCell::insLeft(UCHAR X)

   /**
    * @brief Insert a CHAR object to the left of the current SlipCell.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa checkThis
    * @param[in] X (CHAR) object to be inserted
    * @return reference to the inserted object
    */
   SlipCell& SlipCell::insLeft(CHAR X) {                                        // Insert X to current object left
      if (!checkThis(*this, "insLeft")) return *this;
      SlipDatum* cell = new SlipDatum(X);
      return insertLeft(*cell);
   }; // SlipCell& SlipCell::insLeft(CHAR XX)

   /**
    * @brief Insert a PTR object to the left of the current SlipCell.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * <p>The operations field provides a means to specify user defined operations.
    *    The pointer must be to a SlipOp defined class. The standard casting,
    *    unary, arithmetic, bitwise and logical operations must all be overridden.
    *    The result is that a user Defined object is treated as a atomic value
    *    in all computations.</p>
    * @sa checkThis
    * @param[in] X (PTR) object to be inserted
    * @param[in] operation (void*) SlipOp derived User Defined operations
    * @return reference to the inserted object
    */
   SlipCell& SlipCell::insLeft(PTR X, const void* operation) {                  // Insert X to current object left
      if (!checkThis(*this, "insLeft")) return *this;
      SlipDatum* cell = new SlipDatum(X, operation);
      return insertLeft(*cell);
   }; // SlipCell& SlipCell::insLeft(PTR X, const SlipOp* operation)

   /**
    * @brief Insert a string& object to the left of the current SlipCell.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa checkThis
    * @param[in] X (string&) object to be inserted
    * @param[in] constFlag <b>true</b> string is a <b>const</b>, <b>false</b> otherwise
    * @return reference to the inserted object
    */
   SlipCell& SlipCell::insLeft(const string& X, bool constFlag) {               // Insert X to current object left
      if (!checkThis(*this, "insLeft")) return *this;
      SlipDatum* cell = new SlipDatum(X, constFlag);
      return insertLeft(*cell);
   }; // SlipCell& SlipCell::insLeft((const string& X, bool constFlag)

   /**
    * @brief Insert a string* object to the left of the current SlipCell.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa checkThis
    * @param[in] X (string*) object to be inserted
    * @param[in] constFlag <b>true</b> string is a <b>const</b>, <b>false</b> otherwise
    * @return reference to the inserted object
    */
   SlipCell& SlipCell::insLeft(const string* X, bool constFlag) {               // Insert X to current object left
      if (!checkThis(*this, "insLeft")) return *this;
      SlipDatum* cell = new SlipDatum(X, constFlag);
      return insertLeft(*cell);
   }; // SlipCell& SlipCell::insLeft((const string* X, bool constFlag)

   /**
    * @fn SlipCell::insertRight(SlipCell&   X)
    * @brief Insert an input SlipCell object to the right of <b>this</b>.
    * <p>The input object must be a valid source, and the current (<b>this</b>)
    *    object a valid destination for the insertion. All checks to ensure
    *    validity must have already been done.</p>
    * <p>The input object link pointers are changed, then the rightLink pointer
    *    of the previous object then the leftLink pointer of the following object
    *    (to <b>this</b>) are modified. The order of operations are important.</p>
    * @param[in] X (SlipCell&) object reference
    * @return reference to a new the input object
    */

   /**
    * @brief Insert a SlipCell to the right of the current SlipCell.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then checks are made to ensure that
    *    the input cell is valid. If both are valid then the operation is
    *    legitimate and the following will be done:</p>
    * <ul>
    *    <li>If the input cell is a SlipHeader object and linked then a
    *        SlipSublist will be created and inserted. The reference count
    *        of the referenced list header will be incremented. Both lists
    *        share a reference to the same list.</li>
    *    <li>If the input cell is a SlipHeader object and unlinked then the
    *        sublist will be inserted. Responsibility for deleted the cell
    *        must be done in the context of the list. If the cell is deleted
    *        then it will be removed from the list.</li>
    *    <li>If the input cell is linked onto a list, then a copy will be
    *        created and inserted.</li>
    *    <li>If the input cell is on the stack, then a copy will be created
    *        and inserted.</li>
    *    <li>Otherwise the input cell is inserted.</li>
    * </ul>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2006</b> Cell can not be a local temporary. Create with 'new'.</li>
    *    <li><b>E2013</b> Cell must be in a list. Cell is not in a list.</li>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    *    <li><b>E3022</b> Can not insert a non-AVSL cell into a list.</li>
    * </ul>
    * @sa checkThis
    * @sa checkThat
    * @param[in] X (SlipCell&) object reference
    * @return reference to the inserted object
    */
   SlipCell& SlipCell::insRight(SlipCell&  X) {                                 // Insert X to current object right
      SlipCell* cell = &X;
      if (!checkThis(*this, "insRight)")) return *this;
      if (!checkThat(X, "insRight")) return *this;
      if (X.isHeader()) {
         cell = new SlipSublist((SlipHeader&)X);
      } else if (!X.isUnlinked() || X.isTemp()) {
         cell = &X.factory();
      }
      return insertRight(*cell);
   }; // SlipCell& SlipCell::insRight(SlipCell&  X) {

   /**
    * @brief Insert a bool object to the right of the current SlipCell.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa checkThis
    * @param[in] X (bool) object to be inserted
    * @return reference to the inserted object
    */
   SlipCell& SlipCell::insRight(bool X) {                                       // Insert X to current object Right
      if (!checkThis(*this, "insRight")) return *this;
      SlipDatum* cell = new SlipDatum(X);
      return insertRight(*cell);
   }; // SlipCell& SlipCell::insRight(bool X)

   /**
    * @brief Insert a DOUBLE object to the right of the current SlipCell.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa checkThis
    * @param[in] X (DOUBLE) object to be inserted
    * @return reference to the inserted object
    */
   SlipCell& SlipCell::insRight(DOUBLE X) {                                     // Insert X to current object Right
      if (!checkThis(*this, "insRight")) return *this;
      SlipDatum* cell = new SlipDatum(X);
      return insertRight(*cell);
   }; // SlipCell& SlipCell::insRight(DOUBLE X)

   /**
    * @brief Insert a ULONG object to the right of the current SlipCell.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa checkThis
    * @param[in] X (ULONG) object to be inserted
    * @return reference to the inserted object
    */
   SlipCell& SlipCell::insRight(ULONG X) {                                      // Insert X to current object Right
      if (!checkThis(*this, "insRight")) return *this;
      SlipDatum* cell = new SlipDatum(X);
      return insertRight(*cell);
   }; // SlipCell& SlipCell::insRight(ULONG X)

   /**
    * @brief Insert a LONG object to the right of the current SlipCell.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa checkThis
    * @param[in] X (LONG) object to be inserted
    * @return reference to the inserted object
    */
   SlipCell& SlipCell::insRight(LONG X) {                                       // Insert X to current object Right
      if (!checkThis(*this, "insRight")) return *this;
      SlipDatum* cell = new SlipDatum(X);
      return insertRight(*cell);
   }; // SlipCell& SlipCell::insRight(LONG X)

   /**
    * @brief Insert a UCHAR object to the right of the current SlipCell.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa checkThis
    * @param[in] X (UCHAR) object to be inserted
    * @return reference to the inserted object
    */
   SlipCell& SlipCell::insRight(UCHAR X) {                                      // Insert X to current object Right
      if (!checkThis(*this, "insRight")) return *this;
      SlipDatum* cell = new SlipDatum(X);
      return insertRight(*cell);
   }; // SlipCell& SlipCell::insRight(UCHAR X)

   /**
    * @brief Insert a CHAR object to the right of the current SlipCell.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa checkThis
    * @param[in] X (CHAR) object to be inserted
    * @return reference to the inserted object
    */
   SlipCell& SlipCell::insRight(CHAR X) {                                       // Insert X to current object Right
      if (!checkThis(*this, "insRight")) return *this;
      SlipDatum* cell = new SlipDatum(X);
      return insertRight(*cell);
   }; // SlipCell& SlipCell::insRight(CHAR XX)

   /**
    * @brief Insert a PTR object to the right of the current SlipCell.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * <p>The operations field provides a means to specify user defined operations.
    *    The pointer must be to a SlipOp defined class. The standard casting,
    *    unary, arithmetic, bitwise and logical operations must all be overridden.
    *    The result is that a user Defined object is treated as a atomic value
    *    in all computations.</p>
    * @sa checkThis
    * @param[in] X (PTR) object to be inserted
    * @param[in] operation (void*) SlipOp derived User Defined operations
    * @return reference to the inserted object
    */
   SlipCell& SlipCell::insRight(PTR X, const void* operation) {                 // Insert X to current object Right
      if (!checkThis(*this, "insRight")) return *this;
      SlipDatum* cell = new SlipDatum(X, operation);
      return insertRight(*cell);
   }; // SlipCell& SlipCell::insRight(PTR X)

   /**
    * @brief Insert a string& object to the right of the current SlipCell.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    *
    * @sa checkThis
    * @param[in] X (string&) object to be inserted
    * @param[in] constFlag <b>true</b> string is a <b>const</b>, <b>false</b> otherwise
    * @return reference to the inserted object
    */
   SlipCell& SlipCell::insRight(const string& X, bool constFlag) {              // Insert X to current object Right
      if (!checkThis(*this, "insRight")) return *this;
      SlipDatum* cell = new SlipDatum(X, constFlag);
      return insertRight(*cell);
   }; // SlipCell& SlipCell::insRight((const string& X, bool constFlag)

   /**
    * @brief Insert a string* object to the right of the current SlipCell.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    *
    * @sa checkThis
    * @param[in] X (string*) object to be inserted
    * @param[in] constFlag <b>true</b> string is a <b>const</b>, <b>false</b> otherwise
    * @return reference to the inserted object
    */
   SlipCell& SlipCell::insRight(const string* X, bool constFlag) {              // Insert X to current object Right
      if (!checkThis(*this, "insRight")) return *this;
      SlipDatum* cell = new SlipDatum(X, constFlag);
      return insertRight(*cell);
   }; // SlipCell& SlipCell::insRight((const string* X, bool constFlag)

   /**
    * @brief move the source cell to the left of the current object.
    * <p>Check to see if the source and destination of the move are valid. If
    *    valid, then:</p>
    * <ul>
    *    <li>If the source is a list, then the entire list is moved to the
    *        left of the current object and the list. The list is empty after
    *        the move.</li>
    *    <li>If the source is linked, then it is unlinked and moved to the
    *        left of the current object.</li>
    *    <li>If the source is unlinked, then it is inserted to the left of
    *        the current object.</li>
    * </ul>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2006</b> Cell can not be a local temporary. Create with 'new'.</li>
    *    <li><b>E2013</b> Cell must be in a list. Cell is not in a list.</li>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    *    <li><b>E3022</b> Can not insert a non-AVSL cell into a list.</li>
    * </ul>
    * @sa checkThis
    * @sa checkThat
    * @param[in] X (SlipCell&) to be inserted to the left
    * @return reference to the current object
    */
   SlipCell& SlipCell::moveLeft(SlipCell& X) {                                  // Move X to the left of the current object
      if (this != &X) {
         if (!checkThis(*this, "moveLeft")) return *this;
         if (!checkThat(X, "moveLeft")) return *this;
         if (isUnlinked()) {
            postError(__FILE__, __LINE__, SlipErr::E2013, "moveLeft", "", "", *this);
         } else if (X.isUnlinked()) {
            insertLeft(X);
         } else if (X.isHeader()) {                                             // Move list
            return moveListLeft(X);
         } else {
            setRightLink(X.getLeftLink(), X.getRightLink());
            setLeftLink(X.getRightLink(), X.getLeftLink());
            insertLeft(X);
         }
      }
      return *this;
   }; //SlipCell& SlipCell::moveLeft(SlipCell& X)

   /**
    * @brief Move the source list to the left of the destination object.
    * <p>Both source and destination objects are checked to see that they are
    *    valid. If valid, move the list contents. The source list is empty
    *    after the move.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2006</b> Cell can not be a local temporary. Create with 'new'.</li>
    *    <li><b>E2013</b> Cell must be in a list. Cell is not in a list.</li>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    *    <li><b>E3022</b> Can not insert a non-AVSL cell into a list.</li>
    * </ul>
    * @sa checkThis
    * @sa checkThat
    * @sa checkHeader
    * @param[in] X (SlipCell&) source list reference
    * @return reference to the current object
    */
   SlipCell& SlipCell::moveListLeft(SlipCell& X) {                              // Move the list X to the left of the current object
      if (checkThis(*this, "moveListLeft") && checkThat(X, "moveListLeft")) {
         SlipHeader* headerPtr = (SlipHeader*)&X;
         if (X.isSublist()) {
            headerPtr = &(((SlipSublist&)X).getHeader());
         }
         SlipHeader& header = *headerPtr;
         if (checkHeader(X, "moveListLeft") && !header.isEmpty()) {
            setLeftLink(&header.getTop(), getLeftLink());
            setRightLink(&header.getBot(), this);
            setRightLink(getLeftLink(), &header.getTop());
            setLeftLink(this, &header.getBot());
            setLeftLink(&header, &header);
            setRightLink(&header, &header);
         }
      }
      return *this;
   }; // SlipCell& SlipCell::moveListLeft(SlipDatum& X)

   /**
    * @brief Move the source list to the right of the destination object.
    * <p>Both source and destination objects are checked to see that they are
    *    valid. If valid, move the list contents. The source list is empty
    *    after the move.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2006</b> Cell can not be a local temporary. Create with 'new'.</li>
    *    <li><b>E2013</b> Cell must be in a list. Cell is not in a list.</li>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    *    <li><b>E3022</b> Can not insert a non-AVSL cell into a list.</li>
    * </ul>
    * @sa checkThis
    * @sa checkThat
    * @sa checkHeader
    * @param[in] X (SlipCell&) source list reference
    * @return reference to the current object
    */
   SlipCell& SlipCell::moveListRight(SlipCell& X) {                             // Move list contents to right of X
      if (checkThis(*this, "moveListLeft") && checkThat(X, "moveListLeft")) {
         SlipHeader* headerPtr = (SlipHeader*)&X;
         if (X.isSublist()) {
            headerPtr = &(((SlipSublist&)X).getHeader());
         }
         SlipHeader& header = *headerPtr;
         if (checkHeader(X, "moveListRight") && !header.isEmpty()) {
            setLeftLink(&header.getTop(), this);
            setRightLink(&header.getBot(), getRightLink());
            setLeftLink(getRightLink(), &header.getBot());
            setRightLink(this, &header.getTop());
            setLeftLink(&header, &header);
            setRightLink(&header, &header);
         }
      }
      return *this;
   }; // SlipCell& SlipCell:::moveListRight(SlipDatum& X)

   /**
    * @brief move the source cell to the right of the current object.
    * <p>Check to see if the source and destination of the move are valid. If
    *    valid, then:</p>
    * <ul>
    *    <li>If the source is a list, then the entire list is moved to the
    *        right of the current object and the list. The list is empty after
    *        the move.</li>
    *    <li>If the source is linked, then it is unlinked and moved to the
    *        right of the current object.</li>
    *    <li>If the source is unlinked, then it is inserted to the right of
    *        the current object.</li>
    * </ul>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2006</b> Cell can not be a local temporary. Create with 'new'.</li>
    *    <li><b>E2013</b> Cell must be in a list. Cell is not in a list.</li>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    *    <li><b>E3022</b> Can not insert a non-AVSL cell into a list.</li>
    * </ul>
    * @sa checkThat
    * @sa checkThis
    * @param[in] X (SlipCell&) to be inserted to the right
    * @return reference to the current object
    */
   SlipCell& SlipCell::moveRight(SlipCell& X) {                                 // Move the current object to the left of X
      if (this != &X) {
         if (!checkThis(*this, "moveRight")) return *this;
         if (!checkThat(X, "moveRight")) return *this;
         if (isUnlinked()) {
            postError(__FILE__, __LINE__, SlipErr::E2013, "moveRight", "", "", *this);
         } else if (X.isUnlinked()) {
            insertRight(X);
         } else if (X.isHeader()) {                                             // Move list
            return moveListRight(X);
         } else {
            setRightLink(X.getLeftLink(), X.getRightLink());
            setLeftLink(X.getRightLink(), X.getLeftLink());
            insertRight(X);
         }
      }
      return *this;
   }; //SlipCell& SlipCell::moveRight(SlipCell& X)

   /**
    * @brief Issue a newline to the output if the line length is exceeded.
    * @param[in] out (ostream&) output stream
    * @param[in,out] col (int&) current column number
    * @param[in] extent (int) characters to be output
    */
   void SlipCell::printCheck(ostream& out, int& col, int extent) {              // Check if enough room on current line
      if ((col + extent) >= LINELENGTH) {
         col = 0;
         out << '\n';
      }
   }; // void static SlipCell::printCheck(ostream& out, int& col, int& extent)

   /**
    * @brief Replace the current object with the new one.
    * <p>The current object is unlinked and the new object replaces it in its
    *    former location.</p>
    * @note No object verification is performed. It is assumed the the current
    *       object and its replacement are valid.
    * @param[in,out] X (SlipCell&) replacement object
    */
   void SlipCell::replaceLinks(SlipCell& X) {                                   // Set links of <prev, next, X) & unlink this
      setRightLink(getLeftLink(), &X);
      setLeftLink(&X, getLeftLink());
      setLeftLink(getRightLink(), &X);
      setRightLink(&X, getRightLink());
      setRightLink(NULL);
      setLeftLink(NULL);

   }; // void SlipCell::replaceLinks(SlipCell& X)

   /**
    * @brief Unlink the current object from a list.
    * <p>The current object can not be a header, must be already linked to a
    *    list, and must be a valid object. If the object is valid, it is
    *    unlinked.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2010</b> Can not use header cell. Only list cells can be unlinked</li>
    * </ul>
    * @sa checkThis
    * @return reference to the current object
    */
   SlipCell& SlipCell::unLink() {                                               // Remove cell from list and return
      if (isHeader()) {
         postError(__FILE__, __LINE__, SlipErr::E2010, "SlipCell::unlink()", "", "Only list cells can be unlinked", (SlipDatum&)*this);
      } else if (checkThis(*this, "unLink")) {
         setRightLink(getLeftLink(), getRightLink());
         setLeftLink(getRightLink(), getLeftLink());
         setLeftLink(NULL);
         setRightLink(NULL);
      }
      return *this;
   }; // SlipCell& SlipCell::unLink()

   /**
    * @brief Uglify the input.
    * <p>Output the leftLink, rightLink, and operation in hex. Output the datum
    *    value according to it's type</p>
    * @note There is a visibility problem with referencing SlipCellBase::dump.
    *       When this is resolved, this method will go the way of the woolly
    *       mammoth.
    * @return ugly string
    */
   string     SlipCell::dump() const     { return SlipCellBase::dump(); }              // Uglify SLIP cell

   /**
    * @brief prettify the output according to its type.
    * <p>In general this means:</p>
    * <ul>
    *    <li>SlipHeader and SlipSublist objects are uglified.</li>
    *    <li>SlipDatum object output the datum field according to the datum
    *        fields type.</li>
    * </ul>
    * @return a pretty-printed string of the object
    */
   string     SlipCell::toString() const { return ((SlipOp*)*getOperator())->toString(*this);} // pretty print SLIP cell


   /**
    * @brief Return a string formatted for output
    * @return return a string formatted for output
    */
   string     SlipCell::write() const { return ((SlipOp*)*getOperator())->write(*this);} // String in output format


} // namespace slip

