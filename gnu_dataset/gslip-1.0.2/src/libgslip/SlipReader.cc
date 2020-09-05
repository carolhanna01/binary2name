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
 * @file   SlipReader.h
 * @author A. Schwarz
 * @date   July 18, 2012
 *
 * @brief Implementation of the user interface to the reader list iterator.
 *
 */

# include <sstream>
# include <iomanip>
# include "SlipCellBase.h"
# include "SlipGlobal.h"
# include "SlipHeader.h"
# include "SlipReader.h"
# include "SlipReaderCell.h"
# include "SlipSequencer.h"
# include "SlipSublist.h"

namespace slip {
    /*********************************************************
     *                   Private Functions                   *
    **********************************************************/
   /**
    * @brief The null constructor is not supported.
    * <p>Constructing a reader without supplying a header is not supported.'
    *    All readers must have a current list (header).</p>
    */
   SlipReader::SlipReader() { };

    /*********************************************************
     *                  Protected Functions                  *
    **********************************************************/
   
   /**
    * @fn SlipReader::getLink(const SlipReader& X)
    * @brief Returns the <em>SlipReaderCell</em> pointer in a reader.
    * @param [in] X (SlipReader&) reader
    * @return <em>SlipReaderCell</em> pointer
    */

   /*********************************************************
    *                   Public Functions                    *
   **********************************************************/

            /*****************************************
             *       Constructors/Destructors        *
            ******************************************/
   
   /**
    * @fn SlipReader::SlipReader(const SlipHeader& header)
    * @brief Construct a <em>SlipReader</em> instance.
    * <p>Creates a new instance of a <em>SlipReaderCell</em> using the input
    *    header as the topmost instance and links the reader field of the
    *    <em>SlipReader</em> to it.</p>
    * <p>The current cell = the current list, the list depth = 0, and the
    *    reader chain back pointer is <b>null</b>.</p>
    * @param [in] header (SlipHeader&) topmost list
    */

   /**
    * @fn SlipReader::SlipReader(const SlipSublist& sublist)
    * @brief Construct a <em>SlipReader</em> instance.
    * <p>Creates a new instance of a <em>SlipReaderCell</em> using the input
    *    <em>SlipSulist</em> header pointer as the topmost instance and links
    *    the reader field of the <em>SlipReader</em> to it.</p>
    * <p>The current cell = the current list, the list depth = 0, and the
    *    reader chain back pointer is <b>null</b>.</p>
    * @param [in] sublist (SlipSublist&&) header topmost list
    */

   /**
    * @fn SlipReader::~SlipReader()
    * @brief Deletes the <em>SlipReaderCell</em> chain.
    * <p>During tear down of the <em>SlipReader</em> the <em>SlipReaderCell</em>
    *    instance and reader chain is deleted.</p>
    */

            /*****************************************
             *           Reader Assignment           *
            ******************************************/
   /**
    * @fn SlipReader::operator=(const SlipHeader& X)
    * @brief Reinitialize a reader from a <em>SlipHeader</em>.
    * <p>Delete the current <em>SlipReaderCell</em> chain and create a new
    *    <em>SlipReaderCell</em> instance with the <em>SlipHeader</em>. The
    *    current reader topmost list is the current list of the input reader.</p>
    * @param[in] X (SlipHeader&) RHS of assignment
    * @return reference to this reader
    */

            /*****************************************
             *        Current Cell Assignment        *
            ******************************************/
   
   /**
    * @fn SlipReader::operator=(const SlipDatum& X)
    * @brief Assign (<b>Y = X</b>) where Y is the current cell.
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
    * @param[in] X (SlipDatum&) RHS of the assignment
    * @return reference to the current <em>SlipReader</em>
    */

   /**
    * @fn SlipReader& SlipReader::operator=(const SlipReader& X)
    * @brief Reinitialize a reader from another reader.
    * @brief Assign (<b>Y = X</b>) where Y is the current cell.
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
    * @param[in] X (SlipReader&) RHS of the assignment
    * @return reference to the current <em>SlipReader</em>
    */

   /**
    * @fn SlipReader & SlipReader::operator=(const SlipSequencer& X)
    * @brief Assign (<b>Y = X</b>) where Y is the current cell.
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
    * @param[in] X (SlipSequencer&) RHS of the assignment
    * @return reference to the current <em>SlipReader</em>
    */
   SlipReader&  SlipReader::operator=(const SlipSequencer& X) {  
      currentCell() = X.currentCell();
      return *this; 
   }; // SlipReader&  SlipReader::operator=(const SlipSequencer& X)

   /**
    * @fn SlipReader::operator=(const bool    X)
    * @brief Assign (<b>Y = X</b>) where Y is the current cell.
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
    * @return reference to the current <em>SlipReader</em>
    */

   /**
    * @fn SlipReader::operator=(const CHAR    X)
    * @brief Assign (<b>Y = X</b>) where Y is the current cell.
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
    * @return reference to the current <em>SlipReader</em>
    */

   /**
    * @fn SlipReader::operator=(const DOUBLE  X)
    * @brief Assign (<b>Y = X</b>) where Y is the current cell.
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
    * @return reference to the current <em>SlipReader</em>
    */

   /**
    * @fn SlipReader::operator=(const LONG    X)
    * @brief Assign (<b>Y = X</b>) where Y is the current cell.
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
    * @return reference to the current <em>SlipReader</em>
    */

   /**
    * @fn SlipReader::operator=(const UCHAR   X)
    * @brief Assign (<b>Y = X</b>) where Y is the current cell.
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
    * @return reference to the current <em>SlipReader</em>
    */

   /**
    * @fn SlipReader::operator=(const ULONG   X) 
    * @brief Assign (<b>Y = X</b>) where Y is the current cell.
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
    * @return reference to the current <em>SlipReader</em>
    */

   /** 
    * @fn SlipReader::operator=(const PTR     X)
    * @brief Assign (<b>Y = X</b>) where Y is the current cell.
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
    * @return reference to the current <em>SlipReader</em>
    */

   /**
    * @fn SlipReader::operator=(const string& X)
    * @brief Assign (<b>Y = X</b>) where Y is the current cell.
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
    * @return reference to the current <em>SlipReader</em>
    */

   /**
    * @fn SlipReader::operator=(const string* X)
    * @brief Assign (<b>Y = X</b>) where Y is the current cell.
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
    * @return reference to the current <em>SlipReader</em>
    */

            /*****************************************
             *            Reader Equality            *
            ******************************************/
   
   /**
    * @fn SlipReader::operator==(const SlipReader&  X)
    * @brief Compare two <em>SlipReaders</em>.
    * @param [in] X (SlipReader&) comparand
    * @return <b>true</b> if both the current list and current cell are equal
    */

   /**
    * @fn SlipReader::operator==(const SlipSequencer&  X)
    * @brief Compare the current cells of a read and  SlipSequencer .
    * @param [in] X (SlipSequencer&) comparand
    * @return <b>true</b> if the current cells are equal
    */

   /** 
    * @fn SlipReader::operator==(const SlipCell& X)
    * @brief Compares the current cell with a <em>SlipCell</em>.
    * <p>If the type or values or the current cell and the input <em>SlipCell</em>
    *    are different then the result if <b>false</b>.</p>
    * @param [in] X (SlipCell&) comparand
    * @return <b>true</b> if the cells are equal
    */

   /**
    * @fn lipReader::operator==(const bool    X)
    * @brief Compares the current cell with a bool.
    * <p>If the type or value of the current cell is not the same as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param [in] X (bool) X comparand
    * @return <b>true</b> if the type and values are the same
    */

   /**
    * @fn SlipReader::operator==(const CHAR    X)
    * @brief Compares the current cell with a CHAR.
    * <p>If the type or value of the current cell is not the same as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param [in] X (CHAR) X comparand
    * @return <b>true</b> if the type and values are the same
    */

   /**
    * @fn SlipReader::operator==(const DOUBLE  X)
    * @brief Compares the current cell with a DOUBLE.
    * <p>If the type or value of the current cell is not the same as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param [in] X (DOUBLE) X comparand
    * @return <b>true</b> if the type and values are the same
    */

   /**
    * @fn SlipReader::operator==(const LONG    X)
    * @brief Compares the current cell with a LONG.
    * <p>If the type or value of the current cell is not the same as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param [in] X (LONG) X comparand
    * @return <b>true</b> if the type and values are the same
    */

   /**
    * @fn SlipReader::operator==(const UCHAR   X
    * @brief Compares the current cell with a UCHAR.
    * <p>If the type or value of the current cell is not the same as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param [in] X (UCHAR) X comparand
    * @return <b>true</b> if the type and values are the same
    */

   /**
    * @fn SlipReader::operator==(const ULONG   X)
    * @brief Compares the current cell with a ULONG.
    * <p>If the type or value of the current cell is not the same as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param [in] X (ULONG) X comparand
    * @return <b>true</b> if the type and values are the same
    */

   /**
    * @fn SlipReader::operator==(const PTR X)
    * @brief Compares the current cell with a user defined pointer object.
    * <p>If the type or value of the current cell is not the same as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param [in] X (PTR) X comparand
    * @return <b>true</b> if the type and values are the same
    */

   /**
    * @fn SlipReader::operator==(const string& X
    * @brief Compares the current cell with a string.
    * <p>If the type or value of the current cell is not the same as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param [in] X (string&) X comparand
    * @return <b>true</b> if the type and values are the same
    */

   /**
    * @fn SlipReader::operator==(const string* X)
    * @brief Compares the current cell with a string.
    * <p>If the type or value of the current cell is not the same as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param [in] X (string*) X comparand
    * @return <b>true</b> if the type and values are the same
    */

            /*****************************************
             *           Reader Inequality           *
            ******************************************/
   /**
    * @fn SlipReader::operator!=(const SlipReader&  X)
    * @brief Compare two <em>SlipReaders</em>.
    * @param [in] X (SlipReader&) comparand
    * @return <b>true</b> if either the current list or current cell are unequal
    */

   /**
    * @fn SlipReader::operator!=(const SlipSequencer&  X)
    * @brief Compare the current cell of the reader and a SlipSequencer.
    * @param [in] X (SlipSequencer&) comparand
    * @return <b>true</b> if the current cells are not equal
    */
   bool SlipReader::operator!=(const SlipSequencer&  X) {                       // Y == V
      return (currentCell() != X.currentCell());
   }; //  SlipReader::operator!=(const SlipSequencer&  X)

   /**
    * @fn SlipReader::operator!=(const SlipCell& X)
    * @brief Compares the current cell with a <em>SlipCell</em>.
    * <p>If the type or values or the current cell and the input <em>SlipCell</em>
    *    are different then the result is <b>true</b>.</p>
    * @param [in] X (SlipCell&) comparand
    * @return <b>true</b> if the cells are unequal
    */

   /**
    * @fn SlipReader::operator!=(const bool    X
    * @brief Compares the current cell with a bool.
    * <p>If the type or value of the current cell is not different as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param [in] X (bool) X comparand
    * @return <b>true</b> if the type and values are different
    */

   /**
    * @fn SlipReader::operator!=(const CHAR    X)
    * @brief Compares the current cell with a CHAR.
    * <p>If the type or value of the current cell is not different as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param [in] X (CHAR) X comparand
    * @return <b>true</b> if the type and values are different
    */

   /**
    * @fn SlipReader::operator!=(const DOUBLE  X)
    * @brief Compares the current cell with a DOUBLE.
    * <p>If the type or value of the current cell is not different as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param [in] X (DOUBLE) X comparand
    * @return <b>true</b> if the type and values are different
    */

   /**
    * @fn SlipReader::operator!=(const LONG    X)
    * @brief Compares the current cell with a LONG.
    * <p>If the type or value of the current cell is not different as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param [in] X (LONG) X comparand
    * @return <b>true</b> if the type and values are different
    */

   /**
    * @fn SlipReader::operator!=(const UCHAR   X)
    * @brief Compares the current cell with a UCHAR.
    * <p>If the type or value of the current cell is not different as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param [in] X (UCHAR) X comparand
    * @return <b>true</b> if the type and values are different
    */

   /**
    * @fn SlipReader::operator!=(const ULONG   X)
    * @brief Compares the current cell with a ULONG.
    * <p>If the type or value of the current cell is not different as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param [in] X (ULONG) X comparand
    * @return <b>true</b> if the type and values are different
    */

   /**
    * @fn SlipReader::operator!=(const PTR X)
    * @brief Compares the current cell with a user defined pointer object.
    * <p>If the type or value of the current cell is not different as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param [in] X (PTR) X comparand
    * @return <b>true</b> if the type and values are different
    */

   /**
    * @fn SlipReader::operator!=(const string& X)
    * @brief Compares the current cell with a string.
    * <p>If the type or value of the current cell is not different as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param [in] X (string&) X comparand
    * @return <b>true</b> if the type and values are different
    */

   /**
    * @fn SlipReader::operator!=(const string* X)
    * @brief Compares the current cell with a string.
    * <p>If the type or value of the current cell is not different as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param [in] X (string*) X comparand
    * @return <b>true</b> if the type and values are different
    */

            /*****************************************
             *            Reader Iteration           *
            ******************************************/
   /**
    * @fn SlipReader::advanceLEL()
    * @brief Advance to the preceeding <em>SlipDatum</em> cell in the current list.
    * <p>Iterate over all cells in a list until a <em>SlipDatum</em> cell is
    *    found. Terminate on the list header (<em>SlipHeader</em>) if there
    *    are no more <em>SlipDatum</em> cells in the list.</p>
    * <p>Iteration starts on the cell preceeding the current cell.</p>
    * @return reference to the reader
    */

   /**
    * @fn SlipReader::advanceLER()
    * @brief Advance to the next <em>SlipDatum</em> cell in the current list.
    * <p>Iterate over all cells in a list until a <em>SlipDatum</em> cell is
    *    found. Terminate on the list header (<em>SlipHeader</em>) if there
    *    are no more <em>SlipDatum</em> cells in the list.</p>
    * <p>Iteration starts on the cell following the current cell. </p>
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::advanceLNL()
    * @brief Advance to the preceeding <em>SlipSublist</em> cell in the current list.
    * <p>Iterate over all cells in a list until a <em>SlipSublist</em> cell is
    *    found. Terminate on the list header (<em>SlipHeader</em>) if there are
    *    no more <em>SlipSublist</em> cells in the list.</p>
    * <p>Iteration starts on the cell preceeding the current cell.</p>
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::advanceLNR(
    * @brief Advance to the next <em>SlipSublist</em> cell in the current list.
    * <p>Iterate over all cell in a list until a <em>SlipSublist</em> cell is
    *    found. Terminate on the list header (<em>SlipHeader</em>) if there
    *    are no more <em>SlipDatum</em> cells in the list.</p>
    * <p>Iteration starts on the cell following the current cell.</p>
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::advanceLWL()
    * @brief Advance to the preceeding cell in the current list.
    * <p>Iteration starts on the cell preceeding the current cell.</p>
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::advanceLWR()
    * @brief Advance to the preceeding cell in the current list.
    * <p>Iteration starts on the cell following the current cell.</p>
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::advanceSEL()
    * @brief Structurally advance to the preceeding <em>SlipDatum</em> cell.
    * <p>Iterate over all cells in a list until either a <em>SlipDatum</em>
    *    or a <em>SlipSublist</em> cell is found.
    * <ul>
    *    <li>If the current cell is a <em>SlipSublist</em> cell continue
    *        the search in the referenced sublist.</li>
    *    <li>If a <em>SlipDatum</em> cell is found, terminate the search.</li>
    *    <li>If a <em>SlipSublist</em> cell is found then continue the search
    *        in the referenced sublist.</li>
    *    <li>If a list header (<em>SlipHeader</em>) is found in a sublist
    *        then the search continues in the containing list.</li>
    *    <li>If a list header (<em>SlipHeader</em>) is found in the topmost
    *        list then the search stops.</li>
    * </ul>
    * <p>Iteration starts on the cell preceeding the current cell. If there
    *    are no <em>SlipDatum</em> cells in the list, will terminate when
    *    the list header is found.</p>
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::advanceSER()
    * @brief Structurally advance to the next <em>SlipDatum</em> cell.
    * <p>Iterate over all cells in a list until either a <em>SlipDatum</em>
    *    or a <em>SlipSublist</em> cell is found.
    * <ul>
    *    <li>If the current cell is a <em>SlipSublist</em> cell continue
    *        the search in the referenced sublist.</li>
    *    <li>If a <em>SlipDatum</em> cell is found, terminate the search.</li>
    *    <li>If a <em>SlipSublist</em> cell is found then continue the search
    *        in the referenced sublist.</li>
    *    <li>If a list header (<em>SlipHeader</em>) is found in a sublist
    *        then the search continues in the containing list.</li>
    *    <li>If a list header (<em>SlipHeader</em>) is found in the topmost
    *        list then the search stops.</li>
    * </ul>
    * <p>Iteration starts on the cell following the current cell. If there
    *    are no <em>SlipDatum</em> cells in the list, will terminate when
    *    the list header is found.</p>
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::advanceSNL()
    * @brief Structurally advance to the preceeding <em>SlipSublist</em> cell.
    * <p>Iterate over all cells in a list until a <em>SlipSublist</em> cell is
    *    found. </p>
    * <ul>
    *    <li>If the current cell is a <em>SlipSublist</em> cell continue
    *        the search in the referenced sublist.</li>
    *    <li>If a <em>SlipSublist</em> cell is found, terminate the search.</li>
    *    <li>If a list header (<em>SlipHeader</em>) is found in a sublist
    *        then the search continues in the containing list.</li>
    *    <li>If a list header (<em>SlipHeader</em>) is found in the topmost
    *        list then the search stops.</li>
    * </ul>
    * <p>Iteration starts on the current cell.</p>
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::advanceSNR()
    * @brief Structurally advance to the next <em>SlipSublist</em> cell.
    * <p>Iterate over all cells in a list until a <em>SlipSublist</em> cell is
    *    found. </p>
    * <ul>
    *    <li>If the current cell is a <em>SlipSublist</em> cell continue
    *        the search in the referenced sublist.</li>
    *    <li>If a <em>SlipSublist</em> cell is found, terminate the search.</li>
    *    <li>If a list header (<em>SlipHeader</em>) is found in a sublist
    *        then the search continues in the containing list.</li>
    *    <li>If a list header (<em>SlipHeader</em>) is found in the topmost
    *        list then the search stops.</li>
    * </ul>
    * <p>Iteration starts on the current cell.</p>
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::advanceSWL()
    * @brief Structurally advance to the previous cell.
    * <p>Iteration will advance only one cell.
    * <ul>
    *    <li>If the current cell is a sublist (<em>SlipSublist</em>) then
    *        iteration will follow the nested list header.</li>
    *    <li>If the current cell is a list header (<em>SlipHeader</em>) and
    *        not the topmost list, then iteration will continue in the
    *        containing list.</li>
    *    <li>If the current cell is the topmost list header ((<em>SlipHeader</em>)
    *        then iteration will continue in the topmost list.</li>
    *    <li>If the preceeding cell is the topmost list header, then
    *        iteration will stop.</li>
    *    <li>Otherwise iteration will stop at the preceeding cell.</li>
    * </ul>
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn  SlipReader::advanceSWR()
    * @brief Structurally advance to the next cell.
    * <p>Iteration will advance only one cell.
    * <ul>
    *    <li>If the current cell is a sublist (<em>SlipSublist</em>) then
    *        iteration will follow the nested list header.</li>
    *    <li>If the current cell is a list header (<em>SlipHeader</em>) and
    *        not the topmost list, then iteration will continue in the
    *        containing list.</li>
    *    <li>If the current cell is the topmost list header ((<em>SlipHeader</em>)
    *        then iteration will continue in the topmost list.</li>
    *    <li>If the next cell is the topmost list header, then
    *        iteration will stop.</li>
    *    <li>Otherwise iteration will stop at the next cell.</li>
    * </ul>
    * @return (SlipReader&) reference to the reader
    */

            /*****************************************
             *     Miscellaneous Reader Methods      *
            ******************************************/
   /**
    * @fn SlipReader::currentCell()const
    * @brief Reference the current cell.
    * @return (SlipCell&) current cell
    */

   /**
    * @fn SlipReader::listDepth()  const
    * @brief Current list depth.
    * <p>The topmost list has a list depth of zero (0). Each nested sublist
    *    increments the list depth by one (1).</p>
    * @return (USHORT) list depth
    */

   /**
    * @fn SlipReader::currentList() const
    * @brief Current list header.
    * @return (SlipHeader&) current list header
    */

   /**
    * @fn SlipReader::reset()
    * @brief Make the current cell the list header.
    * @return (SlipHeader&) current list header
    */

   /**
    * @fn SlipReader::resetTop()
    * @brief Reset the reader to the topmost list.
    * <p>If at the topmost list, do nothing. The current cell will be unchanged.</p>
    * <p>If not at the topmost list, remove all memorized sublists until the
    *    topmost list. The current cell will be the sublist cell to the first
    *    referenced contained list.
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::upLevel()
    * @brief Return to the containing list.
    * <p>If at the topmost list, do nothing. The current cell will be
    *    unchanged.</p>
    * <p>If not at the topmost list, backup the memory chain to the containing
    *    list.  The current cell will be the sublist which caused a sublist
    *    traversal.</p>
    * @return (SlipReader&) reference to the reader
    */

               /*****************************************
                *            Descriptor Lists           *
               ******************************************/
   /**
    * @fn SlipReader::contains(SlipCell& value)
    * @brief Search the current list Descriptor List for a value.
    * @copydetails SlipHeader::contains
    */

   /**
    * @fn SlipReader::containsKey(SlipCell& key)
    * @brief Search the current list Descriptor List for a key.
    * @copydetails SlipHeader::containsKey
    */

   /**
    * @fn SlipReader::create_dList()
    * @brief Create a dDescriptor List on the current list.
    * @copydetails SlipHeader::create_dList
    */

   /**
    * @fn SlipReader::deleteAttribute(SlipCell& key)
    * @brief Delete an attribute from the current list Descriptor List.
    * @copydetails SlipHeader::deleteAttribute
    */

   /**
    * @fn SlipReader::delete_dList()
    * @brief Delete the current list Descriptor List.
    * @copydetails SlipHeader::delete_dList
    */

   /**
    * @fn SlipReader::flush_dList()
    * @brief Delete the current list Descriptor List contents.
    * @copydetails SlipHeader::flush_dList
    */

   /**
    * @fn SlipReader::get(SlipCell& key)
    * @brief Return the value of a key in the current list Descriptor List.
    * @copydetails SlipHeader::get
    */

   /**
    * @fn SlipReader::keys()
    * @brief Return all keys in the current list Descriptor List.
    * @copydetails SlipHeader::keys
    */

   /**
    * @fn SlipReader::put(SlipCell& key, SlipCell& value)
    * @brief Put a new &lt;key value&gt; pair in the current list Descriptor List.
    * @copydetails SlipHeader::put
    */

   /**
    * @fn SlipReader::size_dList()
    * @brief Return the number of in &lt;key value&gt; pairs in the current list Descriptor List.
    * @copydetails SlipHeader::size_dList
    */

            /*****************************************
             *           Manipulating Cells          *
            ******************************************/
   /**
    * @fn SlipReader::deleteCell()
    * @brief Delete the current cell.
    * <p>The current cell is deleted and the reader current cell points to
    *    the previous cell.</p>
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::insLeft(SlipReader&  X)
    * @brief Insert the input current cell to the left of the current cell.
    * @param[in] X (SlipReader&) SlipCell object reference
    * @return (SlipCell&) reference to the inserted cell
    */
   SlipCell& SlipReader::insLeft(SlipReader&  X) {
      SlipCell& Y = X.currentCell();
      if (!X.currentCell().isHeader()) X.advanceLWL();
      return currentCell().insLeft(Y);
   }; // SlipCell& SlipReader::insLeft(SlipReader&  X)

   /**
    * @fn SlipReader::insLeft(SlipSequencer&  X)
    * @brief Insert the input current cell to the left of the current cell.
    * @param[in] X (SlipSequencer&) SlipCell object reference
    * @return (SlipCell&) reference to the inserted cell
    */
   SlipCell& SlipReader::insLeft(SlipSequencer&  X) {
      SlipCell& Y = X.currentCell();
      if (!X.currentCell().isHeader()) X.advanceLWL();
      return currentCell().insLeft(Y);
   }; // SlipCell& SlipReader::insLeft(SlipSequencer&  X)

   /**
    * @fn SlipReader::insLeft(SlipCell&  X)
    * @brief Insert an input SlipCell object to the left of the current cell.
    * <p>The input object must be a valid source, and the current (<b>this</b>)
    *    object a valid destination for the insertion.</p>
    * @param[in] X (SlipCell&) SlipCell object reference
    * @return (SlipCell&) reference to the inserted cell
    */

   /**
    * @fn SlipReader::insLeft(bool       X)
    * @brief Insert a bool to the left of the current cell.
    * <p>A <em>SlipDatum</em> cell with type bool containing the input value
    *    is inserted to the left of the current cell.</p>
    * @param[in] X (bool) value to be inserted
    * @return (SlipCell&) reference to the inserted cell
    */

   /**
    * @fn SlipReader::insLeft(DOUBLE     X)
    * @brief Insert a DOUBLE to the left of the current cell.
    * <p>A <em>SlipDatum</em> cell with type DOUBLE containing the input value
    *    is inserted to the left of the current cell.</p>
    * @param[in] X (DOUBLE) value to be inserted
    * @return (SlipCell&) reference to the inserted cell
    */

   /**
    * @fn SlipReader::insLeft(ULONG      X)
    * @brief Insert a ULONG to the left of the current cell.
    * <p>A <em>SlipDatum</em> cell with type ULONG containing the input value
    *    is inserted to the left of the current cell.</p>
    * @param[in] X (ULONG) value to be inserted
    * @return (SlipCell&) reference to the inserted cell
    */

   /**
    * @fn SlipReader::insLeft(LONG       X)
    * @brief Insert a LONG to the left of the current cell.
    * <p>A <em>SlipDatum</em> cell with type LONG containing the input value
    *    is inserted to the left of the current cell.</p>
    * @param[in] X (LONG) value to be inserted
    * @return (SlipCell&) reference to the inserted cell
    */

   /**
    * @fn SlipReader::insRight(SlipReader&  X)
    * @brief Insert an input SlipCell object to the right of the current cell.
    * <p>The input object must be a valid source, and the current (<b>this</b>)
    *    object a valid destination for the insertion.</p>
    * @param[in] X (SlipReader&) SlipCell object reference
    * @return (SlipCell&) reference to the inserted cell
    */

   /**
    * @fn SlipReader::insRight(SlipSequencer&  X)
    * @brief Insert an input SlipCell object to the right of the current cell.
    * <p>The input object must be a valid source, and the current (<b>this</b>)
    *    object a valid destination for the insertion.</p>
    * @param[in] X (SlipSequencer&) SlipCell object reference
    * @return (SlipCell&) reference to the inserted cell
    */
   SlipCell& SlipReader::insRight(SlipSequencer&  X) {                          // Insert X to return current cell right
      currentCell().insRight(X.currentCell());
      return currentCell();
   }; // SlipCell& SlipReader::insRight(SlipSequencer&  X)

   /**
    * @fn SlipReader::insRight(SlipCell&  X
    * @brief Insert an input SlipCell object to the right of the current cell.
    * <p>The input object must be a valid source, and the current (<b>this</b>)
    *    object a valid destination for the insertion.</p>
    * @param[in] X (SlipCell&) SlipCell object reference
    * @return (SlipCell&) reference to the inserted cell
    */

   /**
    * @fn SlipReader::insRight(bool       X)
    * @brief Insert a bool to the right of the current cell.
    * <p>A <em>SlipDatum</em> cell with type bool containing the input value
    *    is inserted to the right of the current cell.</p>
    * @param[in] X (bool) value to be inserted
    * @return (SlipCell&) reference to the inserted cell
    */

   /**
    * @fn SlipReader::insRight(DOUBLE     X)
    * @brief Insert a DOUBLE to the right of the current cell.
    * <p>A <em>SlipDatum</em> cell with type DOUBLE containing the input value
    *    is inserted to the right of the current cell.</p>
    * @param[in] X (DOUBLE) value to be inserted
    * @return (SlipCell&) reference to the inserted cell
    */

   /**
    * @fn SlipReader::insRight(ULONG      X)
    * @brief Insert a ULONG to the right of the current cell.
    * <p>A <em>SlipDatum</em> cell with type ULONG containing the input value
    *    is inserted to the right of the current cell.</p>
    * @param[in] X (ULONG) value to be inserted
    * @return (SlipCell&) reference to the inserted cell
    */

   /**
    * @fn SlipReader::insRight(LONG       X)
    * @brief Insert a LONG to the right of the current cell.
    * <p>A <em>SlipDatum</em> cell with type LONG containing the input value
    *    is inserted to the right of the current cell.</p>
    * @param[in] X (LONG) value to be inserted
    * @return (SlipCell&) reference to the inserted cell
    */

   /**
    * @fn SlipReader::insLeft(UCHAR      X)
    * @brief Insert a UCHAR to the left of the current cell.
    * <p>A <em>SlipDatum</em> cell with type UCHAR containing the input value
    *    is inserted to the left of the current cell.</p>
    * @param[in] X (UCHAR) value to be inserted
    * @return (SlipCell&) reference to the inserted cell
    */

   /**
    * @fn SlipReader::insLeft(CHAR       X)
    * @brief Insert a CHAR to the left of the current cell.
    * <p>A <em>SlipDatum</em> cell with type CHAR containing the input value
    *    is inserted to the left of the current cell.</p>
    * @param[in] X (CHAR) value to be inserted
    * @return (SlipCell&) reference to the inserted cell
    */

   /**
    * @fn SlipReader::insLeft(const PTR  X, const void* operation)
    * @brief Insert a user defined object to the left of the current cell.
    * <p>A <em>SlipDatum</em> cell with type PTR containing the input value
    *    is inserted to the left of the current cell.</p>
    * <p>The operations field provides a means to specify user defined operations.
    *    The pointer must be to a SlipOp defined class. The standard casting,
    *    unary, arithmetic, bitwise and logical operations must all be overridden.
    *    The result is that a user Defined object is treated as a atomic value
    *    in all computations.</p>
    * @param[in] X (PTR) X object to be inserted
    * @param[in] operation (void*) SlipOp derived User Defined operations
    * @return (SlipCell&) reference to the inserted cell
    */

   /**
    * @fn SlipReader::insLeft(const string& X, bool constFlag
    * @brief Insert a string to the left of the current cell.
    * <p>A <em>SlipDatum</em> cell with type string containing the input value
    *    is inserted to the left of the current cell.</p>
    * @param[in] X (string&) value to be inserted
    * @param[in] constFlag <b>true</b> string is a <b>const</b>, <b>false</b> otherwise
    * @return (SlipCell&) reference to the inserted cell
    */

   /**
    * @fn SlipReader::insLeft(const string* X, bool constFlag)
    * @brief Insert a string to the left of the current cell.
    * <p>A <em>SlipDatum</em> cell with type string containing the input value
    *    is inserted to the left of the current cell.</p>
    * @param[in] X (string*) value to be inserted
    * @param[in] constFlag <b>true</b> string is a <b>const</b>, <b>false</b> otherwise
    * @return (SlipCell&) reference to the inserted cell
    */

   /**
    * @fn SlipReader::insRight(UCHAR      X)
    * @brief Insert a UCHAR to the right of the current cell.
    * <p>A <em>SlipDatum</em> cell with type UCHAR containing the input value
    *    is inserted to the right of the current cell.</p>
    * @param[in] X (UCHAR) value to be inserted
    * @return (SlipCell&) reference to the inserted cell
    */

   /**
    * @fn SlipReader::insRight(CHAR       X)
    * @brief Insert a CHAR to the right of the current cell.
    * <p>A <em>SlipDatum</em> cell with type CHAR containing the input value
    *    is inserted to the right of the current cell.</p>
    * @param[in] X (CHAR) value to be inserted
    * @return (SlipCell&) reference to the inserted cell
    */

   /**
    * @fn SlipReader::insRight(const PTR  X, const void* operation)
    * @brief Insert a user defined object to the right of the current cell.
    * <p>A <em>SlipDatum</em> cell with type PTR containing the input value
    *    is inserted to the right of the current cell.</p>
    * <p>The operations field provides a means to specify user defined operations.
    *    The pointer must be to a SlipOp defined class. The standard casting,
    *    unary, arithmetic, bitwise and logical operations must all be overridden.
    *    The result is that a user Defined object is treated as a atomic value
    *    in all computations.</p>
    * @param[in] X (PTR) X object to be inserted
    * @param[in] operation (void*) SlipOp derived User Defined operations
    * @return (SlipCell&) reference to the inserted cell
    */

   /**
    * @fn SlipReader::insRight(const string& X, bool constFlag)
    * @brief Insert a string to the right of the current cell.
    * <p>A <em>SlipDatum</em> cell with type string containing the input value
    *    is inserted to the right of the current cell.</p>
    * @param[in] X (string&) value to be inserted
    * @param[in] constFlag <b>true</b> string is a <b>const</b>, <b>false</b> otherwise
    * @return (SlipCell&) reference to the inserted cell
    */

   /**
    * @fn SlipReader::insRight(const string* X, bool constFlag
    * @brief Insert a string to the right of the current cell.
    * <p>A <em>SlipDatum</em> cell with type string containing the input value
    *    is inserted to the right of the current cell.</p>
    * @param[in] X (string*) value to be inserted
    * @param[in] constFlag <b>true</b> string is a <b>const</b>, <b>false</b> otherwise
    * @return (SlipCell&) reference to the inserted cell
    */

   /**
    * @fn SlipReader::moveLeft(SlipCell& X)
    * @brief Move the input cell to the left of the current cell.
    * <p>Check to see if the source and destination of the move are valid. If
    *    valid, then:</p>
    * <ul>
    *    <li>If the source is a list, then the entire list is moved to the
    *        left of the current object and the list is empty after the move.</li>
    *    <li>If the source is linked, then it is unlinked and moved to the
    *        left of the current object.</li>
    *    <li>If the source is unlinked, then it is inserted to the left of
    *        the current object.</li>
    * </ul>
    * <p>The sequencer current cell reference is unchanged.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2006</b> Cell can not be a local temporary. Create with 'new'.</li>
    *    <li><b>E2013</b> Cell must be in a list. Cell is not in a list.</li>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    *    <li><b>E3022</b> Can not insert a non-AVSL cell into a list.</li>
    * </ul>
    * @param[in] X (SlipCell&) to be inserted to the left
    * @return (SlipCell&) reference to the current cell
    */


   /**
    * @fn SlipReader::moveLeft(SlipReader& X)
    * @brief Move the reader current cell to the left of the current cell.
    * <ul>
    *    <li>If the source is a list, then the entire list is moved to the
    *        left of the current object and the list is empty after the move.
    *        The input SlipSequencer references an empty list.</li>
    *    <li>If the source is linked, then it is unlinked and moved to the
    *        left of the current object. The reader is updated to point to the
    *        cell preceding the moved cell.</li>
    * </ul>
    * <p>The sequencer current cell reference is unchanged.</p>
    * @param[in] X (SlipReader&) current cell to be inserted to the left
    * @return (SlipCell&) reference to the current cell
    */
   SlipCell& SlipReader::moveLeft(SlipReader& X) {                               // Move the current cell to the left of X
      SlipCell& cell = X.currentCell();
      if (!X.isHeader()) X.advanceLWL();
      currentCell().moveLeft(cell);
      return currentCell();
   }; // SlipCell& SlipReader::moveLeft(SlipReader& X)

   /**
    * @brief Move the sequencer current cell to the left of the current cell.
    * <ul>
    *    <li>If the source is a list, then the entire list is moved to the
    *        left of the current object and the list is empty after the move.
    *        The input SlipSequencer references an empty list.</li>
    *    <li>If the source is linked, then it is unlinked and moved to the
    *        left of the current object. The input sequencer current cell is
    *        changed to the preceding cell.</li>
    * </ul>
    * @param[in] X (SlipSequencer&) current cell to be inserted to the left
    * @return (SlipCell&) reference to the current cell
    */
   SlipCell& SlipReader::moveLeft(SlipSequencer& X) {                           // Move the current cell to the left of X
      SlipCell& cell = X.currentCell();
      if (!X.isHeader()) X.advanceLWL();
      currentCell().moveLeft(cell);
      return currentCell();
   }; // SlipCell& SlipReader::moveLeft(SlipSequencer& X)

   /**
    * @fn  SlipReader::moveRight(SlipCell& X)
    * @brief Move the input cell to the right of the current cell.
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
    * @param[in] X (SlipCell&) to be inserted to the right of the current cell
    * @return (SlipCell&) reference to the current cell
    */

   /**
    * @fn SlipReader::moveRight(SlipReader& X)
    * @brief Move the reader current cell to the right of the current cell.
    * <ul>
    *    <li>If the source is a list, then the entire list is moved to the
    *        right of the current object and the list is empty after the move.
    *        The input SlipReader references an empty list.</li>
    *    <li>If the source is linked, then it is unlinked and moved to the
    *        right of the current object. The reader is updated to point to the
    *        cell preceding the moved cell.</li>
    * </ul>
    * <p>The sequencer current cell reference is unchanged.</p>
    * @param[in] X (SlipReader&) current cell to be inserted to the left
    * @return (SlipCell&) reference to the current cell
    */
   SlipCell& SlipReader::moveRight(SlipReader& X) {                             // Move the current cell to the left of X
      SlipCell& cell = X.currentCell();
      if (!X.isHeader()) X.advanceLWL();
      currentCell().moveRight(cell);
      return currentCell();
   }; // SlipCell& SlipReader::moveRight(SlipReader& X)

   /**
    * @brief Move the sequencer current cell to the right of the current cell.
    * <ul>
    *    <li>If the source is a list, then the entire list is moved to the
    *        right of the current object and the list is empty after the move.
    *        The input SlipSequencer references an empty list.</li>
    *    <li>If the source is linked, then it is unlinked and moved to the
    *        left of the current object. The input sequencer current cell is
    *        changed to the preceding cell.</li>
    * </ul>
    * @param[in] X (SlipSequencer&) current cell to be inserted to the left
    * @return (SlipCell&) reference to the current cell
    */
   SlipCell& SlipReader::moveRight(SlipSequencer& X) {                          // Move the current cell to the left of X
      SlipCell& cell = X.currentCell();
      if (!X.isHeader()) X.advanceLWL();
      currentCell().moveRight(cell);
      return currentCell();
   }; // SlipCell& SlipReader::moveRight(SlipSequencer& X)

   /**
    * @fn SlipReader::replace(const SlipReader& X)
    * @brief Replace the current cell with the input value.
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param[in] X (SlipReader&) replacement value
    * @return (SlipCell&) reference to the changed cell
    */

   /**
    * @brief Replace the current cell with the input value.
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param[in] X (SlipSequencer&) replacement value
    * @return (SlipCell&) reference to the changed cell
    */
   SlipCell& SlipReader::replace(const SlipSequencer& X) {                    // Replace a cell on a list with a new cell
      reader->replace(X.currentCell());
      return currentCell();
   }; // SlipCell& SlipReader::replace(const SlipSequencer& X)

   /**
    * @fn SlipReader::replace(const SlipCell& X)
    * @brief Replace the current cell with the input value.
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param[in] X (SlipCell&) replacement value
    * @return (SlipCell&) reference to the changed cell
    */

   /**
    * @fn SlipReader::replace(bool X)
    * @brief Replace the current cell with the input value.
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param [in] X (bool) replacement value
    * @return (SlipCell&) reference to the changed cell
    */

   /**
    * @fn SlipReader::replace(CHAR X)
    * @brief Replace the current cell with the input value.
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param [in] X (CHAR) replacement value
    * @return (SlipCell&) reference to the changed cell
    */

   /**
    * @fn SlipReader::replace(UCHAR X)
    * @brief Replace the current cell with the input value.
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param [in] X (UCHAR) replacement value
    * @return (SlipCell&) reference to the changed cell
    */

   /**
    * @fn SlipReader::replace(LONG X)
    * @brief Replace the current cell with the input value.
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param [in] X (LONG) replacement value
    * @return (SlipCell&) reference to the changed cell
    */

   /**
    * @fn SlipReader::replace(ULONG X)
    * @brief Replace the current cell with the input value.
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param [in] X (ULONG) replacement value
    * @return (SlipCell&) reference to the changed cell
    */

   /**
    * @fn SlipReader::replace(DOUBLE X)
    * @brief Replace the current cell with the input value.
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param [in] X (DOUBLE) replacement value
    * @return (SlipCell&) reference to the changed cell
    */

   /**
    * @fn SlipReader::replace(const PTR X, const void* operation)
    * @brief Replace the current cell with the input value.
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p>The operations field provides a means to specify user defined operations.
    *    The pointer must be to a SlipOp defined class. The standard casting,
    *    unary, arithmetic, bitwise and logical operations must all be overridden.
    *    The result is that a user Defined object is treated as a atomic value
    *    in all computations.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param[in] X (PTR) X object to be inserted
    * @param[in] operation (void*) SlipOp derived User Defined operations
    * @return (SlipCell&) reference to the changed cell
    */

   /**
    * @fn SlipReader::replace(const string& X, bool constFlag)
    * @brief Replace the current cell with the input value.
    * <p>The SlipReaderCell will point to the new cell.</p>
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param [in] X (string&) replacement value
    * @param[in] constFlag <b>true</b> string and object referenced not copied
    * @return (SlipCell&) reference to the changed cell
    */

   /**
    * @fn SlipReader::replace(const string* X, bool constFlag)
    * @brief Replace the current cell with the input value.
    * <p>The SlipReaderCell will point to the new cell.</p>
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param [in] X (string*) replacement value
    * @param[in] constFlag <b>true</b> string and object referenced not copied
    * @return (SlipCell&) reference to the changed cell
    */

   /**
    * @fn SlipReader::unlink()
    * @brief Unlink the current cell from a list and fixup the reader.
    * <p>The current cell is unlinked and the reader is advanced one cell
    *    to the left.</p>
    * @return (SlipCell&) a reference to the unlinked cell.
    */
   SlipCell& SlipReader::unlink() {                                             // unlink cell from list
       SlipCell& cell = currentCell();
       advanceLWL();
       return cell.unLink();
   }; // SlipCell& SlipReader::unlink()


               /*****************************************
                *          Manipulating Lists           *
               ******************************************/
   /**
    * @brief Delete the current list.
    * <p>If the current list is the topmost list then also delete the
    *    reader chain, otherwise, use the chain to go up (<em>upLevel()</em>)
    *    to the containing list and delete the referencing sublist cell.</p>
    * @return (SlipReader&) reference to the reader
    */
   SlipReader& SlipReader::deleteList() {                                       // delete the current list
      currentList().deleteList();
      if (listDepth() != 0) {
         upLevel();
         deleteCell();
      } else {
         delete reader;
         reader = NULL;
      }
      return *this;
   }; // SlipReader& SlipReader::deleteList()

   /**
    * @brief Remove the current list bottom cell from the list.
    * <p>If the current list bottom cell is the current cell, then
    *    <em>advanceLWL()</em> before the delete.</p>
    * @return (SlipCell&) reference to the dequeued cell.
    */
   SlipCell& SlipReader:: dequeue() {                                          // Remove and return list bottom
      SlipCell* cell = &getBot();
      if (cell == &currentCell()) {
         reader->advanceLWL();
      }
      return currentList().dequeue();
   }; // SlipReader& SlipReader:: dequeue()

   /**
    * @fn SlipReader::enqueue(SlipCell& X)
    * @brief Enqueue a cell onto the bottom of the current list.
    * <ul>
    *    <li>If the input cell is a sublist then a SlipSublist will be created
    *        and inserted. The reference count of the referenced list header
    *        will be incremented.</li>
    *    <li>If the input cell is a SlipHeader then a sublist will be
    *        inserted. The reference count of the referenced list header
    *        will be incremented.</li>
    *    <li>If the input cell is linked onto a list, then a copy will be
    *        created and inserted.</li>
    *    <li>If the input cell is on the stack, then a copy will be created
    *        and inserted.</li>
    *    <li>Otherwise the input cell is inserted.</li>
    * </ul>
    * @param[in] X (SlipCell&) object reference
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::enqueue(bool X)
    * @brief Insert a bool object to the bottom of the list.
    * <p>A <em>SlipDatum</em> instance is created of type bool containing the
    *    input value and inserted to the bottom of the current list.</p>
    * @param[in] X (bool) object to be inserted
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::enqueue(UCHAR X)
    * @brief Insert a UCHAR object to the bottom of the list.
    * <p>A <em>SlipDatum</em> instance is created of type UCHAR containing the
    *    input value and inserted to the bottom of the current list.</p>
    * @param[in] X (UCHAR) object to be inserted
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::enqueue(CHAR X)
    * @brief Insert a CHAR object to the bottom of the list.
    * <p>A <em>SlipDatum</em> instance is created of type CHAR containing the
    *    input value and inserted to the bottom of the current list.</p>
    * @param[in] X (CHAR) object to be inserted
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::enqueue(ULONG X)
    * @brief Insert a ULONG object to the bottom of the list.
    * <p>A <em>SlipDatum</em> instance is created of type ULONG containing the
    *    input value and inserted to the bottom of the current list.</p>
    * @param[in] X (ULONG) object to be inserted
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::enqueue(LONG X)
    * @brief Insert a LONG object to the bottom of the list.
    * <p>A <em>SlipDatum</em> instance is created of type LONG containing the
    *    input value and inserted to the bottom of the current list.</p>
    * @param[in] X (LONG) object to be inserted
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::enqueue(DOUBLE X)
    * @brief Insert a DOUBLE object to the bottom of the list.
    * <p>A <em>SlipDatum</em> instance is created of type DOUBLE containing the
    *    input value and inserted to the bottom of the current list.</p>
    * @param[in] X (DOUBLE) object to be inserted
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::enqueue(const PTR X, const void* operation)
    * @brief Insert a string object to the bottom of the list.
    * <p>A <em>SlipDatum</em> instance is created of type STRING containing the
    *    input value and inserted to the bottom of the current list.</p>
    * <p>The operations field provides a means to specify user defined operations.
    *    The pointer must be to a SlipOp defined class. The standard casting,
    *    unary, arithmetic, bitwise and logical operations must all be overridden.
    *    The result is that a user Defined object is treated as a atomic value
    *    in all computations.</p>
    * @param[in] X (PTR) X object to be inserted
    * @param[in] operation (void*) SlipOp derived User Defined operations
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::enqueue(const string& X, bool constFlag)
    * @brief Insert a string object to the bottom of the list.
    * <p>A <em>SlipDatum</em> instance is created of type STRING containing the
    *    input value and inserted to the bottom of the current list.</p>
    * @param[in] X (string&) object to be inserted
    * @param[in] constFlag <b>true</b> string and object referenced not copied
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::enqueue(const string* X, bool constFlag)
    * @brief Insert a string object to the bottom of the list.
    * <p>A <em>SlipDatum</em> instance is created of type STRING containing the
    *    input value and inserted to the bottom of the current list.</p>
    * @param[in] X (string*) object to be inserted
    * @param[in] constFlag <b>true</b> string and object referenced not copied
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::flush()
    * @brief Empty the current list.
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::getBot()
    * @brief Return the current list bottom cell.
    * @return (SlipCell&) reference to cell on current list bottom
    */


   /**
    * @fn SlipReader::getTop()
    * @brief Return the current list top cell.
    * @return (SlipCell&) reference to cell on current list top
    */


   /**
    * @brief Move the current list to the left of the destination object.
    * <p>Move the current list contents to the left of the destination
    *    <em>SlipCell</em>. The current list will be empty (<em>flush()</em>)
    *    at operation completion.</p>
    * @param[in, out] X (SlipReader&) destination list reference
    * @return (SlipReader&) reference to the reader
    */
   SlipReader& SlipReader::moveListLeft(SlipCell& X) {                          // Move list contents to left of X
      reset();
      X.moveListLeft(currentList());
      return *this;
   }; //  SlipHeader& SlipReader::moveListLeft(SlipCell& X)

   /**
    * @brief Move the current list to the right of the destination object.
    * <p>Move the current list contents to the right of the destination
    *    <em>SlipCell</em>. The current list will be empty (<em>flush()</em>)
    *    at operation completion.</p>
    * @param[in, out] X (SlipReader&) destination list reference
    * @return (SlipReader&) reference to the reader
    */
   SlipReader &SlipReader::moveListRight(SlipCell& X) {                       // Move list contents to right of X
      reset();
      X.moveListRight(currentList());
      return *this;
   }; // SlipHeader &SlipReader::moveListRight(SlipCell& X)

   /**
    * @brief Remove and return the current list top cell.
    * @return (SlipCell&) reference to the popped cell
    */
   SlipCell& SlipReader::pop() {                                               // Remove and return list top
      SlipCell* cell = &getTop();
      if (cell == &currentCell()) {
         reader->advanceLWL();
      }
      return currentList().pop();
   }; // SlipCell& SlipReader::pop()

   /**
    * @fn SlipReader::push(SlipCell& X)
    * @brief Push an object ont the current list.
    * <ul>
    *    <li>If the input cell is a SlipHeader object then a SlipSublist will
    *        be created and inserted. The reference count of the referenced
    *        list header will be incremented. </li>
    *    <li>If the input cell is a SlipSublist object and unlinked then the
    *        sublist will be inserted.</li>
    *    <li>If the input cell is linked onto a list, then a copy will be
    *        created and inserted.</li>
    *    <li>If the input cell is on the stack, then a copy will be created
    *        and inserted.</li>
    *    <li>Otherwise the input cell is inserted.</li>
    * </ul>
    * @param[in] X (SlipCell&) object to be pushed
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::push(bool X
    * @brief Insert a bool object to the top of the current list.
    * <p>A new <em>SlipDatum</em> instance is created with the same type
    *    and value as the input and inserted to the top of the current
    *    list.</p>
    * @param[in] X (bool) object to be inserted
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::push(UCHAR X)
    * @brief Insert a UCHAR object to the top of the current list.
    * <p>A new <em>SlipDatum</em> instance is created with the same type
    *    and value as the input and inserted to the top of the current
    *    list.</p>
    * @param[in] X (UCHAR) object to be inserted
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::push(CHAR X)
    * @brief Insert a CHAR object to the top of the current list.
    * <p>A new <em>SlipDatum</em> instance is created with the same type
    *    and value as the input and inserted to the top of the current
    *    list.</p>
    * @param[in] X (CHAR) object to be inserted
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::push(ULONG X)
    * @brief Insert a ULONG object to the top of the current list.
    * <p>A new <em>SlipDatum</em> instance is created with the same type
    *    and value as the input and inserted to the top of the current
    *    list.</p>
    * @param[in] X (ULONG) object to be inserted
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::push(LONG X)
    * @brief Insert a LONG object to the top of the current list.
    * <p>A new <em>SlipDatum</em> instance is created with the same type
    *    and value as the input and inserted to the top of the current
    *    list.</p>
    * @param[in] X (LONG) object to be inserted
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::push(DOUBLE X)
    * @brief Insert a DOUBLE object to the top of the current list.
    * <p>A new <em>SlipDatum</em> instance is created with the same type
    *    and value as the input and inserted to the top of the current
    *    list.</p>
    * @param[in] X (DOUBLE) object to be inserted
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::push(const PTR X, const void* operation)
    * @brief Insert a user defined object to the top of the current list.
    * <p>A new <em>SlipDatum</em> instance is created with the same type
    *    and value as the input and inserted to the top of the current
    *    list.</p>
    * <p>The operations field provides a means to specify user defined operations.
    *    The pointer must be to a SlipOp defined class. The standard casting,
    *    unary, arithmetic, bitwise and logical operations must all be overridden.
    *    The result is that a user Defined object is treated as a atomic value
    *    in all computations.</p>
    * @param[in] X (PTR) X object to be inserted
    * @param[in] operation (void*) SlipOp derived User Defined operations
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::push(const string& X, bool constFlag)
    * @brief Insert a string object to the top of the current list.
    * <p>A <em>SlipDatum</em> instance is created of type STRING containing the
    *    input value and inserted to the top of the current list.</p>
    * @param[in] X (string&) object to be inserted
    * @param[in] constFlag <b>true</b> string and object referenced not copied
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::push(const string* X, bool constFlag)
    * @brief Insert a string object to the top of the current list.
    * <p>A <em>SlipDatum</em> instance is created of type STRING containing the
    *    input value and inserted to the top of the current list.</p>
    * @param[in] X (string*) object to be inserted
    * @param[in] constFlag <b>true</b> string and object referenced not copied
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::replaceTop(SlipHeader& X)
    * @brief Replace the top cell in the current list with a sublist cell.
    * <p>Delete the current list top and replace it with a <em>SlipSublist</em>
    *    instance referencing the <em>SlipHeader</em>. If the current list is
    *    empty, issue a diagnostic message.</p>
    * @param[in] X (SlipHeader&) replacement value, list to be referenced
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::replaceTop(SlipCell& X)
    * @brief Replace the top cell in the current list with the input cell.
    * <p>Delete the current list top and replace it with:
    * <ul>
    *    <li>The input object if the object is unlinked and not temporary.</li>
    *    <li>A new instance of the input object in all other cases.</li>
    * </ul>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * @param[in] X (SlipCell&) replacement value, list to be referenced
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::replaceTop(SlipReader& X)
    * @brief Replace the top cell in the current list with the reader current cell.
    * <p>Delete the current list top and replace it with:
    * <ul>
    *    <li>A new instance of the input object in all other cases.</li>
    * </ul>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * @param[in] X (SlipReader&) replacement value, list to be referenced
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::replaceTop(SlipSequencer& X)
    * @brief Replace the top cell in the current list with the sequencer current cell.
    * <p>Delete the current list top and replace it with:
    * <ul>
    *    <li>A new instance of the input object in all other cases.</li>
    * </ul>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * @param[in] X (SlipSequencer&) replacement value, list to be referenced
    * @return (SlipReader&) reference to the reader
    */
   SlipReader&  SlipReader::replaceTop(SlipSequencer& X) {                      // Substitute cell TOP value
      reader->replaceTop(X.currentCell());
      return *this;
   }; // SlipReader&  SlipReader::replaceTop(SlipSequencer& X)

   /**
    * @fn SlipReader::replaceBot(SlipHeader& X)
    * @brief Replace the bottom cell in the current list with a sublist cell.
    * <p>Delete the current list bottom and replace it with a <em>SlipSublist</em>
    *    instance referencing the <em>SlipHeader</em>. If the current list is
    *    empty, issue a diagnostic message.</p>
    * @param[in] X (SlipHeader&) replacement value, list to be referenced
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::replaceBot(SlipCell& X)
    * @brief Replace the bottom cell in the current list with the input cell.
    * <p>Delete the current list bottom and replace it with:
    * <ul>
    *    <li>The input object if the object is unlinked and not temporary.</li>
    *    <li>A new instance of the input object in all other cases.</li>
    * </ul>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * @param[in] X (SlipCell&) replacement value, list to be referenced
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::replaceBot(SlipReader& X)
    * @brief Replace the bottom cell in the current list with the sequencer current cell.
    * <p>Delete the current list top and replace it with:
    * <ul>
    *    <li>A new instance of the input object in all other cases.</li>
    * </ul>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * @param[in] X (SlipReader&) replacement value, list to be referenced
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::replaceBot(SlipSequencer& X)
    * @brief Replace the bottom cell in the current list with the sequencer current cell.
    * <p>Delete the current list top and replace it with:
    * <ul>
    *    <li>A new instance of the input object in all other cases.</li>
    * </ul>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * @param[in] X (SlipSequencer&) replacement value, list to be referenced
    * @return (SlipReader&) reference to the reader
    */
   SlipReader&  SlipReader::replaceBot(SlipSequencer& X) {                      // Substitute cell TOP value
      reader->replaceBot(X.currentCell());
      return *this;
   }; // SlipReader&  SlipReader::replaceBot(SlipSequencer& X)

   /**
    * @fn SlipReader::replaceBot(bool X)
    * @brief Replace the bottom cell in the current list with a bool.
    * <p>Delete the current list bottom and replace it with a new
    *    instance of a <em>SlipDatum</em> cell with the same type
    *    and value as the input.
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * @param[in] X (bool) replacement value, list to be referenced
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::replaceBot(UCHAR X)
    * @brief Replace the bottom cell in the current list with a UCHAR.
    * <p>Delete the current list bottom and replace it with a new
    *    instance of a <em>SlipDatum</em> cell with the same type
    *    and value as the input.
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * @param[in] X (UCHAR) replacement value, list to be referenced
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::replaceBot(CHAR X)
    * @brief Replace the bottom cell in the current list with a CHAR.
    * <p>Delete the current list bottom and replace it with a new
    *    instance of a <em>SlipDatum</em> cell with the same type
    *    and value as the input.
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * @param[in] X (CHAR) replacement value, list to be referenced
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::replaceBot(ULONG X)
    * @brief Replace the bottom cell in the current list with a ULONG.
    * <p>Delete the current list bottom and replace it with a new
    *    instance of a <em>SlipDatum</em> cell with the same type
    *    and value as the input.
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * @param[in] X (ULONG) replacement value, list to be referenced
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::replaceBot(LONG X)
    * @brief Replace the bottom cell in the current list with a LONG.
    * <p>Delete the current list bottom and replace it with a new
    *    instance of a <em>SlipDatum</em> cell with the same type
    *    and value as the input.
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * @param[in] X (LONG) replacement value, list to be referenced
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::replaceBot(DOUBLE X)
    * @brief Replace the bottom cell in the current list with a DOUBLE.
    * <p>Delete the current list bottom and replace it with a new
    *    instance of a <em>SlipDatum</em> cell with the same type
    *    and value as the input.
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * @param[in] X (DOUBLE) replacement value, list to be referenced
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::replaceBot(const PTR X, const void* operation)
    * @brief Replace the bottom cell in the current list with user defined object.
    * <p>Delete the current list bottom and replace it with a new
    *    instance of a <em>SlipDatum</em> cell with the same type
    *    and value as the input.
    * <p>The operations field provides a means to specify user defined operations.
    *    The pointer must be to a SlipOp defined class. The standard casting,
    *    unary, arithmetic, bitwise and logical operations must all be overridden.
    *    The result is that a user Defined object is treated as a atomic value
    *    in all computations.</p>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * @param[in] X (PTR) X object to be inserted
    * @param[in] operation (void*) SlipOp derived User Defined operations
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::replaceBot(const string& X, bool constFlag)
    * @brief Replace the bottom cell in the current list with a string.
    * <p>Delete the current list bottom and replace it with a new
    *    instance of a <em>SlipDatum</em> cell with the same type
    *    and value as the input.
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * @param[in] X (string&) replacement value, list to be referenced
    * @param[in] constFlag <b>true</b> string and object referenced not copied
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::replaceBot(const string* X, bool constFlag)
    * @brief Replace the bottom cell in the current list with a string.
    * <p>Delete the current list bottom and replace it with a new
    *    instance of a <em>SlipDatum</em> cell with the same type
    *    and value as the input.
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * @param[in] X (string*) replacement value, list to be referenced
    * @param[in] constFlag <b>true</b> string and object referenced not copied
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::replaceTop(bool X)
    * @brief Replace the top cell in the current list with a bool.
    * <p>Delete the current list top and replace it with a new
    *    instance of a <em>SlipDatum</em> cell with the same type
    *    and value as the input.
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * @param[in] X (bool) replacement value, list to be referenced
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::replaceTop(UCHAR X)
    * @brief Replace the top cell in the current list with a UCHAR.
    * <p>Delete the current list top and replace it with a new
    *    instance of a <em>SlipDatum</em> cell with the same type
    *    and value as the input.
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * @param[in] X (UCHAR) replacement value, list to be referenced
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::replaceTop(CHAR X)
    * @brief Replace the top cell in the current list with a CHAR.
    * <p>Delete the current list top and replace it with a new
    *    instance of a <em>SlipDatum</em> cell with the same type
    *    and value as the input.
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * @param[in] X (CHAR) replacement value, list to be referenced
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::replaceTop(ULONG X)
    * @brief Replace the top cell in the current list with a ULONG.
    * <p>Delete the current list top and replace it with a new
    *    instance of a <em>SlipDatum</em> cell with the same type
    *    and value as the input.
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * @param[in] X (ULONG) replacement value, list to be referenced
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::replaceTop(LONG X)
    * @brief Replace the top cell in the current list with a LONG.
    * <p>Delete the current list top and replace it with a new
    *    instance of a <em>SlipDatum</em> cell with the same type
    *    and value as the input.
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * @param[in] X (LONG) replacement value, list to be referenced
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::replaceTop(DOUBLE X)
    * @brief Replace the top cell in the current list with a DOUBLE.
    * <p>Delete the current list top and replace it with a new
    *    instance of a <em>SlipDatum</em> cell with the same type
    *    and value as the input.
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * @param[in] X (DOUBLE) replacement value, list to be referenced
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::replaceTop(const PTR X, const void* operation)
    * @brief Replace the top cell in the current list with user defined object.
    * <p>Delete the current list top and replace it with a new
    *    instance of a <em>SlipDatum</em> cell with the same type
    *    and value as the input.
    * <p>The operations field provides a means to specify user defined operations.
    *    The pointer must be to a SlipOp defined class. The standard casting,
    *    unary, arithmetic, bitwise and logical operations must all be overridden.
    *    The result is that a user Defined object is treated as a atomic value
    *    in all computations.</p>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * @param[in] X (PTR) X object to be inserted
    * @param[in] operation (void*) SlipOp derived User Defined operations
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::replaceTop(const string& X, bool constFlag)
    * @brief Replace the top cell in the current list with a string.
    * <p>Delete the current list top and replace it with a new
    *    instance of a <em>SlipDatum</em> cell with the same type
    *    and value as the input.
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * @param[in] X (string&) replacement value, list to be referenced
    * @param[in] constFlag <b>true</b> string and object referenced not copied
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @fn SlipReader::replaceTop(const string* X, bool constFlag)
    * @brief Replace the top cell in the current list with a string.
    * <p>Delete the current list top and replace it with a new
    *    instance of a <em>SlipDatum</em> cell with the same type
    *    and value as the input.
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * @param[in] X (string*) replacement value, list to be referenced
    * @param[in] constFlag <b>true</b> string and object referenced not copied
    * @return (SlipReader&) reference to the reader
    */

   /**
    * @brief Move all cells from the current list top to the current cell a new list.
    * <p>A new list is created and all cells from the list top to and including
    *    the current cell are moved to a new list.</p>
    * <p>If the current list is empty then the new list will be empty.</p>
    * <p>If the input SlipCell reference is to the header then a nasty diagnostic
    *    message will be output. Use <b>moveListLeft</b> or <b>moveListRight</b>.</p>
    * <p>After removal of all cells, the current list may be empty.</p>
    * @return (SlipHeader&) return a reference to the new list
    */
   SlipHeader&  SlipReader::splitLeft() {                                       // Split cells to left of X & X to a new list
      SlipCell& cell = currentCell();
      reset();
      return currentList().splitLeft(cell);
   }; // SlipHeader&  SlipReader::splitLeft()

   /**
    * @brief Move all cells from the current cell to the list bottom a new list.
    * <p>A new list is created and all cells from the current cell to and
    *    including  the current list bottom are moved to a new list.</p>
    * <p>If the current list is empty then the new list will be empty.</p>
    * <p>If the input SlipCell reference is to the header then a nasty diagnostic
    *    message will be output. Use <b>moveListLeft</b> or <b>moveListRight</b>.</p>
    * <p>After removal of all cells, the current list may be empty.</p>
    * @return (SlipHeader&) return a reference to the new list
    */
   SlipHeader&  SlipReader::splitRight() {                                      // Split cells to right of X & X to a new list
      SlipCell& cell = currentCell();
      reset();
      return currentList().splitRight(cell);
   }; // SlipHeader&  SlipReader::splitRight()

            /*****************************************
             *     Miscellaneous Reader Methods      *
            ******************************************/

            /*****************************************
             *             Miscellaneous             *
            ******************************************/
   /**
    * @fn SlipReader::getClassType() const
    * @brief Return current cell class type.
    * <p>Valid cell types are:</p>
    * | Cell  Type |   data type   |
    * | :--------- | :------------ |
    * | <b>eUNDEFINED</b>   | UNDEFINED     |
    * | <b>eBOOL</b>        | bool          |
    * | <b>eCHAR</b>        | CHAR          |
    * | <b>eUCHAR</b>       | UCHAR         |
    * | <b>eLONG</b>        | LONG          |
    * | <b>eULONG</b>       | ULONG         |
    * | <b>eDOUBLE</b>      | DOUBLE        |
    * | <b>eSUBLIST</b>     | SUBLIST       |
    * | <b>eSTRING</b>      | STRING        |
    * | <b>ePTR</b>         | PTR           |
    * | <b>eREADER</b>      | READER        |
    * | <b>eHEADER</b>      | HEADER        |
    *
    * @return class type of the object
    * @return (ClassType) class type
    */

   /**
    * @fn SlipReader::getLeftLink()  const
    * @brief Return the cell leftLink.
    * @return Return the cell leftLink
    */

   /**
    * @fn SlipReader::getName()     const
    * @brief ASCII name of current cell
    * @return (string) ASCII name of current cell
    */

   /**
    * @fn SlipReader::getRightLink() const
    * @brief Return the rightLink.
    * @return Return the rightLink
    */

   /**
    * @fn SlipReader:: getMark()     const
    * @brief Current list mark
    * @return (USHORT) current list mark
    */

   /**
    * @fn SlipReader::getRefCount() const
    * @brief Current list reference count
    * @return (USHORT) current list reference count
    */

   /**
    * @fn SlipReader::isData()      const
    * @brief <b>true</b> if current cell is a <em>SlipDatum</em> cell.
    * @return (bool) <b>true</b> if current cell is a <em>SlipDatum</em> cell.
    */

   /**
    * @fn SlipReader::isDiscrete()      const
    * @brief <b>true</b> if current cell contains a discrete number.
    * @return (bool) <b>true</b> if current cell contains a discrete number.
    */

   /**
    * @fn SlipReader::isDList() const
    * @brief <b>true</b> if current list has a Descriptor List
    * @return (bool) <b>true</b> if current list has a Descriptor List
    */

   /**
    * @fn SlipReader::isEmpty()     const
    * @brief <b>true</b> if current list is empty.
    * @return (bool) <b>true</b> if current list is empty
    */

   /**
    * @fn SlipReader::isEqual(const SlipHeader& X) const
    * @brief <b>true</b> if two lists are structurally equal.
    * @return (bool) <b>true</b> if two lists are structurally equal
    */

   /**
    * @fn SlipReader::isHeader()    const
    * @brief <b>true</b> if current cell is a <em>SlipHeader</em> cell.
    * @return (bool) <b>true</b> if current cell is a <em>SlipHeader</em> cell
    */

   /**
    * @fn SlipReader::isName()      const
    * @brief <b>true</b> if current cell is a <em>SlipSublist</em> cell.
    * @return (bool) <b>true</b> if current cell is a <em>SlipSublist</em> cell
    */

   /**
    * @fn SlipReader::isNumber()    const 
    * @brief <b>true</b> if current cell is a numeric type.
    * @return (bool) <b>true</b> if current cell is a numeric type
    */

   /**
    * @fn SlipReader::isPtr()       const
    * @brief <b>true</b> if current cell is a user defined type.
    * @return (bool) <b>true</b> if current cell is a user defined type
    */

   /**
    * @fn SlipReader::isReal()       const
    * @brief <b>true</b> if current cell contains a real number.
    * @return (bool) <b>true</b> if current cell contains a real number
    */

   /**
    * @fn SlipReader::isString()       const
    * @brief <b>true</b> if current cell contains a string.
    * @return (bool) <b>true</b> if current cell contains a string
    */

   /**
    * @fn SlipReader::isSublist()   const
    * @brief <b>true</b> if current cell is a <em>SlipSublist</em> cell.
    * @return (bool) <b>true</b> if current cell is a <em>SlipSublist</em> cell
    */

   /**
    * @fn SlipReader::putMark(const USHORT X)
    * @brief Change the current list mark value.
    * @param[in] X (USHORT) new mark value
    * @return (USHORT) previous mark value
    */

   /**
    * @fn SlipReader::size()        const
    * @brief Number of cells in the current list.
    * @return (unsigned) number of cells in the current list
    */

   /**
    * @fn SlipReader::dumpList()
    * @brief Uglify and output the current list.
    */

   /**
    * @fn SlipReader::write()
    * @brief Output the current list to cout.
    * @return <b>true</b> output was successful
    */

   /**
    * @fn SlipReader::write(ostream& out)
    * @brief Output the current list to a ostream.
    * @param[in] out (ostream&) output stream
    * @return <b>true</b> output was successful
    */

   /**
    * @fn SlipReader::write(string& filename)
    * @brief Output the current list to a file given by filename.
    * @param[in] filename (string&) output filename
    * @return <b>true</b> file was opened and output successful
    */

   /**
    * @fn SlipReader::writeQuick()
    * @brief Output a list definition to cout
    * <p>The list definition is not a list file. It does not support
    *    round-trip processing. It is a quick, pretty-printed, view
    *    of the list.</p>
    * <p><b>EXAMPLE </b></p>
    * <tt><pre>
    *    SlipHeader* share = new SlipHeader();
    *    SlipHeader* list  = new SlipHeader();
    *    share->enqueue(2)
    *    list->enqueue(1).enqueue(share).enqueue(3);
    *    SlipReader* reader = new SlipReader(*list);
    *    reader->writeQuick();                         // ( 1 ( 2 ) 3 )
    *    list->deleteList();
    *    share->deleteList();
    *    delete reader;
    * </pre></tt>
    */

   /**
    * @fn SlipReader::writeQuick(ostream& out)
    * @brief Output a list definition to a stream
    * <p>The list definition is not a list file. It does not support
    *    round-trip processing. It is a quick, pretty-printed, view
    *    of the list.</p>
    * <p><b>EXAMPLE </b></p>
    * <tt><pre>
    *    std::ofstream ofs;
    *    ofs.open ("test.txt", std::ofstream::out);
    *    ostream& stream = new ostream();
    *    SlipHeader* share = new SlipHeader();
    *    SlipHeader* list  = new SlipHeader();
    *    share->enqueue(2)
    *    list->enqueue(1).enqueue(share).enqueue(3);
    *    SlipReader* reader = new SlipReader(*list);
    *    reader->writeQuick(ofs);                      // ( 1 ( 2 ) 3 )
    *    list->deleteList();
    *    share->deleteList();
    *    delete reader;
    * </pre></tt>
    * @param[in] out (ostream&) output stream
    */

   /**
    * @fn SlipReader::writeQuick(string& filename)
    * @brief Output a list definition to a file
    * <p>The list definition is not a list file. It does not support
    *    round-trip processing. It is a quick, pretty-printed, view
    *    of the list.</p>
    * <p><b>EXAMPLE </b></p>
    * <tt><pre>
    *    SlipHeader* share = new SlipHeader();
    *    SlipHeader* list  = new SlipHeader();
    *    share->enqueue(2)
    *    list->enqueue(1).enqueue(share).enqueue(3);
    *    SlipReader* reader = new SlipReader(*list);
    *    reader->writeQuick("test.txt");              // ( 1 ( 2 ) 3 )
    *    list->deleteList();
    *    share->deleteList();
    *    delete reader;
    * </pre></tt>
    * @param[in] filename (string&) output file
    */

   /**
    * @fn SlipReader::writeToString()
    * @brief Return a list definition
    * <p>The list definition is not a list file. It does not support
    *    round-trip processing. It is a quick, pretty-printed, view
    *    of the list.</p>
    * <p><b>EXAMPLE </b></p>
    * <tt><pre>
    *    SlipHeader* share = new SlipHeader();
    *    SlipHeader* list  = new SlipHeader();
    *    share->enqueue(2)
    *    list->enqueue(1).enqueue(share).enqueue(3);
    *    SlipReader* reader = new SlipReader(*list);
    *    string str = reader->writeToString();              // ( 1 ( 2 ) 3 )
    *    list->deleteList();
    *    share->deleteList();
    *    delete reader;
    * </pre></tt>
    */

   /**
    * @brief The pretty-printed value of the current cell.
    * @return (string) pretty-printed current cell value
    */
   string    SlipReader::toString() {                                           // prettifie the reader thingie
      SlipReaderCell* reader = this->reader;
      stringstream pretty;
      pretty << reader->toString();
      return pretty.str();
   }; // SlipCell& SlipReader::toString()

   /**
    * @fn SlipReader::dump()
    * @brief Ugly representation of the current cell.
    * @return (string) uglified cell representation
    */

}; // namespace slip
