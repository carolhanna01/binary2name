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
 * @file   SlipSequencer.cpp
 * @author A. Schwarz
 * @date   August 14, 2012
 *
 * @brief Implementation of the user interface to the sequencer iterator.
 */

# include <string>
# include <sstream>
# include <iomanip>
# include "SlipSequencer.h"
# include "SlipCell.h"
# include "SlipCellBase.h"
# include "SlipDatum.h"
# include "SlipGlobal.h"
# include "SlipHeader.h"
# include "SlipPointer.h"
# include "SlipReader.h"
# include "SlipSublist.h"
# include "SlipUndefinedOp.h"

namespace slip {

            /*****************************************
             *        Constructors/Destructors       *
            ******************************************/
   /**
    * @brief The <em>SlipSequencer</em> must created with a a <em>SlipHeader</em>.
    * <p>Construction of a <em>SlipSequencer</em> without a <em>SlipHeader</em> is
    *    not allowed. This constructor can not be used by the user.</p>
    */
   SlipSequencer::SlipSequencer() { };

   /**
    * @brief Constructor for a <em>SlipSequencer</em>.
    * <p>A <em>SlipSequencer</em> is constructing initally pointing to a
    *    list header.</p>
    * @param[in] header (SlipHeader&) initial cell in the sequencer
    */
   SlipSequencer::SlipSequencer(SlipHeader& header) {
      if (header.leftLink == UNDEFDATA)
         postError(__FILE__, __LINE__, SlipErr::E3026, "SlipSequncer::SlipSequencer", "", "", header);
      link = &header;
   }; // SlipSequencer::SlipSequencer(SlipHeader& header)

   /**
    * @brief Constructor for a <em>SlipSequencer</em>.
    * <p>A <em>SlipSequencer</em> is constructing initally pointing to a
    *    list header extracted from the sublist cell.</p>
    * @param[in] sublist (SlipSublist&) reference to a header
    */
   SlipSequencer::SlipSequencer(SlipSublist& sublist) {
      link = (SlipCell*)*(sublist.getSublistHeader());
   }; // SlipSequencer::SlipSequencer(SlipSublist& sublist)

   /**
    * @brief Destructor
    * <p>A <em>SlipSequencer</em> has no internal body other than a pointer
    *    to a <em>SlipCell</em>. There isn't an associated structure to
    *    be deleted.</p>
    */
   SlipSequencer::~SlipSequencer() { }

            /*****************************************
             *          Sequencer Iteration          *
            ******************************************/
   /**
    * @brief Advance to the preceeding <em>SlipDatum</em> cell in the current list.
    * <p>Iterate over all cells in a list until a <em>SlipDatum</em> cell is
    *    found. Terminate on the list header (<em>SlipHeader</em>) if there
    *    are no more <em>SlipDatum</em> cells in the list.</p>
    * <p>Iteration starts on the cell preceeding the current cell.</p>
    * @return reference to the sequencer
    */
   SlipSequencer& SlipSequencer::advanceLEL() {                                 // advance linear element left,  true if element found
      SlipCellBase* cell = (SlipCellBase*)link;
      for (cell = cell->getLeftLink(); !cell->isHeader(); cell = cell->getLeftLink())
         if (cell->isData()) break;
      link = (SlipCell*)cell;
      return *this;
   }; // SlipSequencer& SlipSequencer::advanceLEL()

   /**
    * @brief Advance to the next <em>SlipDatum</em> cell in the current list.
    * <p>Iterate over all cells in a list until a <em>SlipDatum</em> cell is
    *    found. Terminate on the list header (<em>SlipHeader</em>) if there
    *    are no more <em>SlipDatum</em> cells in the list.</p>
    * <p>Iteration starts on the cell following the current cell. </p>
    * @return (SlipSequencer&) reference to the sequencer
    */
   SlipSequencer& SlipSequencer::advanceLER() {                                 // advance linear element right, true if element found
      SlipCellBase* cell = (SlipCellBase*)link;
      for (cell = cell->getRightLink(); !cell->isHeader(); cell = cell->getRightLink())
         if (cell->isData()) break;
      link = (SlipCell*)cell;
      return *this;
   }; // SlipSequencer& SlipSequencer::advanceLER()

   /**
    * @brief Advance to the preceeding <em>SlipSublist</em> cell in the current list.
    * <p>Iterate over all cells in a list until a <em>SlipSublist</em> cell is
    *    found. Terminate on the list header (<em>SlipHeader</em>) if there are
    *    no more <em>SlipSublist</em> cells in the list.</p>
    * <p>Iteration starts on the cell preceeding the current cell.</p>
    * @return (SlipSequencer&) reference to the sequencer
    */
   SlipSequencer& SlipSequencer::advanceLNL() {                                 // advance linear name left,     true if element found
      SlipCellBase* cell = (SlipCellBase*)link;
      for (cell = cell->getLeftLink(); !cell->isHeader(); cell = cell->getLeftLink())
         if ((cell->isSublist())) break;
      link = (SlipCell*)cell;
      return *this;
   }; // SlipSequencer& SlipSequencer::advanceLNL()

   /**
    * @brief Advance to the next <em>SlipSublist</em> cell in the current list.
    * <p>Iterate over all cell in a list until a <em>SlipSublist</em> cell is
    *    found. Terminate on the list header (<em>SlipHeader</em>) if there
    *    are no more <em>SlipDatum</em> cells in the list.</p>
    * <p>Iteration starts on the cell following the current cell.</p>
    * @return (SlipSequencer&) reference to the sequencer
    */
   SlipSequencer& SlipSequencer::advanceLNR() {                                 // advance linear name right,    true if element found
      SlipCellBase* cell = (SlipCellBase*)link;
      for (cell = cell->getRightLink(); !cell->isHeader(); cell = cell->getRightLink())
         if ((cell->isSublist())) break;
      link = (SlipCell*)cell;
      return *this;
   }; // SlipSequencer& SlipSequencer::advanceLNR()

   /**
    * @brief Advance to the preceeding cell in the current list.
    * <p>Iteration starts on the cell preceeding the current cell.</p>
    * @return (SlipSequencer&) reference to the sequencer
    */
   SlipSequencer& SlipSequencer::advanceLWL() {                                 // advance linear word right,    true if element found
      SlipCellBase* cell = (SlipCellBase*)link;
      cell = cell->getLeftLink();
      link = (SlipCell*)cell;
      return *this;
   }; // SlipSequencer& SlipSequencer::advanceLWL()

   /**
    * @brief Advance to the following cell in the current list.
    * <p>Iteration starts on the cell following the current cell.</p>
    * @return (SlipSequencer&) reference to the sequencer
    */
   SlipSequencer& SlipSequencer::advanceLWR() {                                 // advance linear word right,    true if element found
      SlipCellBase* cell = (SlipCellBase*)link;
      cell = cell->getRightLink();
      link = (SlipCell*)cell;
      return *this;
   }; // SlipSequencer& SlipSequencer::advanceLWR()

   /**
    * @brief Structurally advance to the preceeding <em>SlipDatum</em> cell.
    * <p>Iterate over all cells in a list until either a <em>SlipDatum</em>
    *    or a <em>SlipSublist</em> cell is found.
    * <ul>
    *    <li>If the current cell is a <em>SlipSublist</em> cell continue
    *        the search in the referenced sublist.</li>
    *    <li>If a <em>SlipDatum</em> cell is found, terminate the search.</li>
    *    <li>If a <em>SlipSublist</em> cell is found then continue the search
    *        in the referenced sublist.</li>
    *    <li>If a list header (<em>SlipHeader</em>) is found, stop.</li>
    * </ul>
    * @return (SlipSequencer&) reference to the sequencer
    */
   SlipSequencer& SlipSequencer::advanceSEL() {                                 // advance structural element left,  true if element found
      SlipCellBase* cell = (SlipCellBase*)link;
      cell = cell->getLeftLink();
      while(!cell->isData()) {
         if (cell->isSublist()) {
            cell = (SlipCell*)*(SlipHeader**)SlipCellBase::getSublistHeader(*cell);
         } else if (cell->isHeader()) {
            break;
         }
         cell = cell->getLeftLink();
      }
      link = (SlipCell*)cell;
      return *this;
   }; // SlipSequencer& SlipSequencer::advanceSEL()

   /**
    * @brief Structurally advance to the following <em>SlipDatum</em> cell.
    * <p>Iterate over all cells in a list until either a <em>SlipDatum</em>
    *    or a <em>SlipSublist</em> cell is found.
    * <ul>
    *    <li>If the current cell is a <em>SlipSublist</em> cell continue
    *        the search in the referenced sublist.</li>
    *    <li>If a <em>SlipDatum</em> cell is found, terminate the search.</li>
    *    <li>If a <em>SlipSublist</em> cell is found then continue the search
    *        in the referenced sublist.</li>
    *    <li>If a list header (<em>SlipHeader</em>) is found, stop.</li>
    * </ul>
    * @return (SlipSequencer&) reference to the sequencer
    */
   SlipSequencer& SlipSequencer::advanceSER() {                                 // advance structural element right, true if element found
      SlipCellBase* cell = (SlipCellBase*)link;
      cell = cell->getRightLink();
      while(!cell->isData()) {
         if (cell->isSublist()) {
            cell = (SlipCell*)*(SlipHeader**)SlipCellBase::getSublistHeader(*cell);
         } else if (cell->isHeader()) {
           break;
         }
         cell = cell->getRightLink();
      }
      link = (SlipCell*)cell;
      return *this;
   }; // SlipSequencer& SlipSequencer::advanceSER()

   /**
    * @brief Structurally advance to the preceeding <em>SlipSublist</em> cell.
    * <p>Iterate over all cells in a list until a <em>SlipSublist</em> cell is
    *    found. </p>
    * <ul>
    *    <li>If the current cell is a <em>SlipSublist</em> cell continue
    *        the search in the referenced sublist.</li>
    *    <li>If a <em>SlipSublist</em> cell is found, terminate the search.</li>
    *    <li>If a list header (<em>SlipHeader</em>), stop.</li>
    * </ul>
    * <p>Iteration starts on the current cell.</p>
    * @return (SlipSequencer&) reference to the sequencer
    */
   SlipSequencer& SlipSequencer::advanceSNL() {                                 // advance structural name left,     true if element found
      SlipCellBase* cell = (SlipCellBase*)link;
      if (cell->isSublist())
         cell = (SlipCell*)*(SlipHeader**)SlipCellBase::getSublistHeader(*cell);
      if (cell->isHeader())
         cell   = cell->getLeftLink();
      while(!cell->isHeader()) {
         if (cell->isSublist())
            break;
         cell = cell->getLeftLink();
      }
      link = (SlipCell*)cell;
      return *this;
   }; // SlipSequencer& SlipSequencer::advanceSNL()

   /**
    * @brief Structurally advance to the following <em>SlipSublist</em> cell.
    * <p>Iterate over all cells in a list until a <em>SlipSublist</em> cell is
    *    found. </p>
    * <ul>
    *    <li>If the current cell is a <em>SlipSublist</em> cell continue
    *        the search in the referenced sublist.</li>
    *    <li>If a <em>SlipSublist</em> cell is found, terminate the search.</li>
    *    <li>If a list header (<em>SlipHeader</em>), stop.</li>
    * </ul>
    * <p>Iteration starts on the current cell.</p>
    * @return (SlipSequencer&) reference to the sequencer
    */
   SlipSequencer& SlipSequencer::advanceSNR() {                                 // advance structural name right,    true if element found
      SlipCellBase* cell = (SlipCellBase*)link;
      if (cell->isSublist())
         cell = (SlipCell*)*(SlipHeader**)SlipCellBase::getSublistHeader(*cell);
      if (cell->isHeader())
         cell   = cell->getRightLink();
      while(!cell->isHeader()) {
         if (cell->isSublist())
            break;
         cell = cell->getRightLink();
      }
      link = (SlipCell*)cell;
      return *this;
   }; // SlipSequencer& SlipSequencer::advanceSNR()

   /**
    * @brief Structurally advance to the previous cell.
    * <p>Iteration will advance only one cell. </p>
    * <ul>
    *    <li>If the current cell is a sublist (<em>SlipSublist</em>) then
    *        iteration will follow the nested list header.</li>
    *    <li>If the current cell is a list header (<em>SlipHeader</em>), stop.</li>
    *    <li>Otherwise iteration will stop at the preceeding cell.</li>
    * </ul>
    * @return (SlipSequencer&) reference to the sequencer
    */
   SlipSequencer& SlipSequencer::advanceSWL() {                                 // advance structural word right,    true if element found
      SlipCellBase* cell = (SlipCellBase*)link;
      if (cell->isSublist())
         cell = (SlipCell*)*(SlipHeader**)SlipCellBase::getSublistHeader(*cell);
      cell = cell->getLeftLink();
      link = (SlipCell*)cell;
      return *this;
   }; // SlipSequencer& SlipSequencer::advanceSWL()

   /**
    * @brief Structurally advance to the following cell.
    * <p>Iteration will advance only one cell. </p>
    * <ul>
    *    <li>If the current cell is a sublist (<em>SlipSublist</em>) then
    *        iteration will follow the nested list header.</li>
    *    <li>If the current cell is a list header (<em>SlipHeader</em>), stop.</li>
    *    <li>Otherwise iteration will stop at the following cell.</li>
    * </ul>
    * @return (SlipSequencer&) reference to the sequencer
    */
   SlipSequencer& SlipSequencer::advanceSWR() {                                 // advance structural word right,    true if element found
      SlipCellBase* cell = (SlipCellBase*)link;
      if (cell->isSublist())
         cell = (SlipCell*)*(SlipHeader**)SlipCellBase::getSublistHeader(*cell);
      cell = cell->getRightLink();
      link = (SlipCell*)cell;
      return *this;
   }; // SlipSequencer& SlipSequencer::advanceSWR()

            /*****************************************
             *         Sequencer Assignment          *
            ******************************************/

   /**
    * @brief Reinitialize a sequencer from a <em>SlipHeader</em>.
    * <p>Set the current cell pointer to the <em>SlipHeader</em> address.</p>
    * @param[in] X (SlipHeader&) RHS of assignment
    * (SlipSequencer&) reference to the sequencer
    */
   SlipSequencer& SlipSequencer::operator=(const SlipHeader& X) {               // Y = V
      link = const_cast<SlipHeader*>(&X);
      return *this;
   }; // SlipSequencer& SlipSequencer::operator=(const SlipHeader& X)

            /*****************************************
             *            Cell Assignment            *
            ******************************************/
   /**
    * @fn SlipSequencer::operator=(const SlipDatum& X)
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
    * @return reference to the current <em>SlipSequencer</em>
    */

   /**
    * @fn SlipSequencer::operator=(const SlipSequencer& X)
    * @brief Assign (<b>currentCell() = X.currentCell()</b>).
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
    * @param[in] X (SlipSequencer&) RHS of assignment
    * (SlipSequencer&) reference to the sequencer
    */

   /**
    * @fn SlipSequencer::operator=(const bool    X)
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
    * @return reference to the current <em>SlipSequencer</em>
    */

   /**
    * @fn  SlipSequencer::operator=(const CHAR    X)
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
    * @return reference to the current <em>SlipSequencer</em>
    */

   /**
    * @fn SlipSequencer::operator=(const DOUBLE  X)
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
    * @return reference to the current <em>SlipSequencer</em>
    */

   /**
    * @fn SlipSequencer::operator=(const LONG    X)
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
    * @return reference to the current <em>SlipSequencer</em>
    */

   /**
    * @fn SlipSequencer::operator=(const UCHAR   X)
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
    * @return reference to the current <em>SlipSequencer</em>
    */

   /**
    * @fn SlipSequencer::operator=(const ULONG   X)
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
    * @return reference to the current <em>SlipSequencer</em>
    */

   /**
    * @fn SlipSequencer::operator=(const PTR     X)
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
    * @return reference to the current <em>SlipSequencer</em>
    */

   /**
    * @fn SlipSequencer::operator=(const string& X)
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
    * @return reference to the current <em>SlipSequencer</em>
    */

   /**
    * @fn SlipSequencer::operator=(const string* X)
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
    * @return reference to the current <em>SlipSequencer</em>
    */

            /*****************************************
             *             Cell Equality             *
            ******************************************/
   /**
    * @fn SlipSequencer::operator==(const SlipReader& X)
    * @brief Compare the current cell to a SlipReader..
    * @param[in] X (SlipReaderr&) comparand
    * @return <b>true</b> if both the current list and current cell are equal
    */
   bool SlipSequencer::operator==(const SlipReader&  X) {                       // Y != V
      return currentCell() == X.currentCell();
   }; // bool SlipSequencer::operator!=(const SlipReader&  X)

   /**
    * @fn SlipSequencer::operator==(const SlipSequencer& X)
    * @brief Compare two <em>SlipSequencer</em>s.
    * @param[in] X (SlipSequencer&) comparand
    * @return <b>true</b> if both the current list and current cell are equal
    */

   /**
    * @fn SlipSequencer::operator==(const SlipCell& X)
    * @brief Compares the current cell with a <em>SlipCell</em>.
    * <p>If the type or values or the current cell and the input <em>SlipCell</em>
    *    are different then the result if <b>false</b>.</p>
    * @param[in] X (SlipCell&) comparand
    * @return <b>true</b> if the cells are equal
    */

   /**
    * @fn SlipSequencer::operator==(const bool    X)
    * @brief Compares the current cell with a bool.
    * <p>If the type or value of the current cell is not the same as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param[in] X (bool) X comparand
    * @return <b>true</b> if the type and values are the same
    */

   /**
    * @fn SlipSequencer::operator==(const CHAR    X)
    * @brief Compares the current cell with a CHAR.
    * <p>If the type or value of the current cell is not the same as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param[in] X (CHAR) X comparand
    * @return <b>true</b> if the type and values are the same
    */

   /**
    * @fn SlipSequencer::operator==(const DOUBLE  X)
    * @brief Compares the current cell with a DOUBLE.
    * <p>If the type or value of the current cell is not the same as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param[in] X (DOUBLE) X comparand
    * @return <b>true</b> if the type and values are the same
    */

   /**
    * @fn SlipSequencer::operator==(const LONG    X)
    * @brief Compares the current cell with a LONG.
    * <p>If the type or value of the current cell is not the same as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param[in] X (LONG) X comparand
    * @return <b>true</b> if the type and values are the same
    */

   /**
    * @fn SlipSequencer::operator==(const UCHAR   X)
    * @brief Compares the current cell with a UCHAR.
    * <p>If the type or value of the current cell is not the same as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param[in] X (UCHAR) X comparand
    * @return <b>true</b> if the type and values are the same
    */

   /**
    * @fn SlipSequencer::operator==(const ULONG   X)
    * @brief Compares the current cell with a ULONG.
    * <p>If the type or value of the current cell is not the same as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param[in] X (ULONG) X comparand
    * @return <b>true</b> if the type and values are the same
    */

   /**
    * @fn SlipSequencer::operator==(const PTR X)
    * @brief Compares the current cell with a user defined pointer object.
    * <p>If the type or value of the current cell is not the same as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param[in] X (PTR) X comparand
    * @return <b>true</b> if the type and values are the same
    */

   /**
    * @fn SlipSequencer::operator==(const string& X)
    * @brief Compares the current cell with a string.
    * <p>If the type or value of the current cell is not the same as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param[in] X (string&) X comparand
    * @return <b>true</b> if the type and values are the same
    */

   /**
    * @fn SlipSequencer::operator==(const string* X)
    * @brief Compares the current cell with a string.
    * <p>If the type or value of the current cell is not the same as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param[in] X (string*) X comparand
    * @return <b>true</b> if the type and values are the same
    */

            /*****************************************
             *            Cell Inequality            *
            ******************************************/
   /**
    * @brief Compare two <em>SlipSequencer</em>s.
    * @param[in] X (SlipReader&) comparand
    * @return <b>true</b> if either the current list or current cell are unequal
    */
   bool SlipSequencer::operator!=(const SlipReader&  X) {                       // Y != V
      return currentCell() != X.currentCell();
   }; // bool SlipSequencer::operator!=(const SlipReader&  X)

   /**
    * @fn SlipSequencer::operator!=(const SlipSequencer&  X)
    * @brief Compare two <em>SlipSequencer</em>s.
    * @param[in] X (SlipSequencer&) comparand
    * @return <b>true</b> if either the current list or current cell are unequal
    */
   bool SlipSequencer::operator!=(const SlipSequencer&  X) {                    // Y != V
      return currentCell() != X.currentCell();
   }; // bool SlipSequencer::operator!=(const SlipSequencer&  X)

   /** 
    * @fn SlipSequencer::operator!=(const SlipCell& X)
    * @brief Compares the current cell with a <em>SlipCell</em>.
    * <p>If the type or values or the current cell and the input <em>SlipCell</em>
    *    are different then the result is <b>true</b>.</p>
    * @param[in] X (SlipCell&) comparand
    * @return <b>true</b> if the cells are unequal
    */
   bool SlipSequencer::operator!=(const SlipCell& X) {                          // Y != V
      return currentCell() != X;
   }; // bool SlipSequencer::operator!=(const SlipCell& X)

   /**
    * @fn SlipSequencer::operator!=(const bool    X)
    * @brief Compares the current cell with a bool.
    * <p>If the type or value of the current cell is not different as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param[in] X (bool) X comparand
    * @return <b>true</b> if the type and values are different
    */

   /**
    * @fn SlipSequencer::operator!=(const CHAR    X)
    * @brief Compares the current cell with a CHAR.
    * <p>If the type or value of the current cell is not different as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param[in] X (CHAR) X comparand
    * @return <b>true</b> if the type and values are different
    */

   /**
    * @fn SlipSequencer::operator!=(const DOUBLE  X)
    * @brief Compares the current cell with a DOUBLE.
    * <p>If the type or value of the current cell is not different as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param[in] X (DOUBLE) X comparand
    * @return <b>true</b> if the type and values are different
    */

   /**
    * @fn SlipSequencer::operator!=(const LONG    X)
    * @brief Compares the current cell with a LONG.
    * <p>If the type or value of the current cell is not different as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param[in] X (LONG) X comparand
    * @return <b>true</b> if the type and values are different
    */

   /**
    * @fn SlipSequencer::operator!=(const UCHAR   X)
    * @brief Compares the current cell with a UCHAR.
    * <p>If the type or value of the current cell is not different as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param[in] X (UCHAR) X comparand
    * @return <b>true</b> if the type and values are different
    */

   /**
    * @fn SlipSequencer::operator!=(const ULONG   X
    * @brief Compares the current cell with a ULONG.
    * <p>If the type or value of the current cell is not different as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param[in] X (ULONG) X comparand
    * @return <b>true</b> if the type and values are different
    */

   /**
    * @fn SlipSequencer::operator!=(const PTR X)
    * @brief Compares the current cell with a user defined pointer object.
    * <p>If the type or value of the current cell is not different as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param[in] X (PTR) X comparand
    * @return <b>true</b> if the type and values are different
    */

   /**
    * @fn SlipSequencer::operator!=(const string& X)
    * @brief Compares the current cell with a string.
    * <p>If the type or value of the current cell is not different as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param[in] X (string&) X comparand
    * @return <b>true</b> if the type and values are different
    */

   /**
    * @fn SlipSequencer::operator!=(const string* X)
    * @brief Compares the current cell with a string.
    * <p>If the type or value of the current cell is not different as the input
    *    then the result of the comparison is <b>false</b> otherwise <b>true</b>.
    *    </p>
    * @param[in] X (string*) X comparand
    * @return <b>true</b> if the type and values are different
    */

            /*****************************************
             *           Manipulating Cells          *
            ******************************************/

   /**
    * @fn SlipSequencer::insLeft(SlipReader&       X)
    * @brief Insert the reader current cell to the left of the current cell.
    * @param[in] X (SlipReader&) value to be inserted
    * @return (SlipCell&) reference to the inserted cell
    */
   SlipCell& SlipSequencer::insLeft(SlipReader&       X) {                      // insert X to current cell left
      SlipCell& Y = X.currentCell();
      if (!X.currentCell().isHeader()) X.advanceLWL();
      return link->insLeft(Y);
   }; // SlipCell& SlipSequencer::insLeft(SlipReader&  X)

   /**
    * @fn SlipSequencer::insLeft(SlipSequencer&    X)
    * @brief Insert a sequencer current cell to the left of the current cell.
    * @param[in] X (SlipReader&) value to be inserted
    * @return (SlipCell&) reference to inserted cell
    */
   SlipCell& SlipSequencer::insLeft(SlipSequencer&    X) {                      // insert X to current cell left
      SlipCell& Y = X.currentCell();
      if (!X.currentCell().isHeader()) X.advanceLWL();
      return link->insLeft(Y);
   }; // SlipCell& SlipSequencer::insLeft(SlipSequencer&    X)

   /**
    * @fn SlipSequencer::insLeft(SlipCell&  X)
    * @brief Insert an input SlipCell object to the left of the current cell.
    * <p>The input object must be a valid source, and the current (<b>this</b>)
    *    object a valid destination for the insertion.</p>
    * @param[in] X (SlipCell&) SlipCell object reference
    * @return (SlipCell&) reference to inserted cell
    */

   /**
    * @fn SlipSequencer::insLeft(bool       X)
    * @brief Insert a bool to the left of the current cell.
    * <p>A <em>SlipDatum</em> cell with type bool containing the input value
    *    is inserted to the left of the current cell.</p>
    * @param[in] X (bool) value to be inserted
    * @return (SlipCell&) reference to inserted cell
    */

   /**
    * @fn SlipSequencer::insLeft(DOUBLE     X)
    * @brief Insert a DOUBLE to the left of the current cell.
    * <p>A <em>SlipDatum</em> cell with type DOUBLE containing the input value
    *    is inserted to the left of the current cell.</p>
    * @param[in] X (DOUBLE) value to be inserted
    * @return (SlipSequencer&) reference to the reader
    */ 
   
   /**
    * @fn SlipSequencer::insLeft(DOUBLE     X)
    * @brief Insert a DOUBLE to the left of the current cell.
    * <p>A <em>SlipDatum</em> cell with type DOUBLE containing the input value
    *    is inserted to the left of the current cell.</p>
    * @param[in] X (DOUBLE) value to be inserted
    * @return (SlipCell&) reference to inserted cell
    */

   /**
    * @fn SlipSequencer::insLeft(ULONG      X)
    * @brief Insert a ULONG to the left of the current cell.
    * <p>A <em>SlipDatum</em> cell with type ULONG containing the input value
    *    is inserted to the left of the current cell.</p>
    * @param[in] X (ULONG) value to be inserted
    * @return (SlipCell&) reference to inserted cell
    */

   /**
    * @fn SlipSequencer::insLeft(LONG       X)
    * @brief Insert a LONG to the left of the current cell.
    * <p>A <em>SlipDatum</em> cell with type LONG containing the input value
    *    is inserted to the left of the current cell.</p>
    * @param[in] X (LONG) value to be inserted
    * @return (SlipCell&) reference to inserted cell
    */

   /**
    * @fn SlipSequencer::insLeft(UCHAR      X)
    * @brief Insert a UCHAR to the left of the current cell.
    * <p>A <em>SlipDatum</em> cell with type UCHAR containing the input value
    *    is inserted to the left of the current cell.</p>
    * @param[in] X (UCHAR) value to be inserted
    * @return (SlipCell&) reference to inserted cell
    */

   /**
    * @fn SlipSequencer::insLeft(CHAR       X)
    * @brief Insert a CHAR to the left of the current cell.
    * <p>A <em>SlipDatum</em> cell with type CHAR containing the input value
    *    is inserted to the left of the current cell.</p>
    * @param[in] X (CHAR) value to be inserted
    * @return (SlipSequencer&) reference to the reader
    */

   /**
    * @fn SlipSequencer::insLeft(const PTR  X, const void* operation)
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
    * @return (SlipCell&) reference to inserted cell
    */

   /**
    * @fn SlipSequencer::insLeft(const string& X, bool constFlag)
    * @brief Insert a string to the left of the current cell.
    * <p>A <em>SlipDatum</em> cell with type string containing the input value
    *    is inserted to the left of the current cell.</p>
    * @param[in] X (string&) value to be inserted
    * @param[in] constFlag <b>true</b> string is a <b>const</b>, <b>false</b> otherwise
    * @return (SlipCell&) reference to inserted cell
    */

   /**
    * @fn SlipSequencer::insLeft(const string* X, bool constFlag)
    * @brief Insert a string to the left of the current cell.
    * <p>A <em>SlipDatum</em> cell with type string containing the input value
    *    is inserted to the left of the current cell.</p>
    * @param[in] X (string*) value to be inserted
    * @param[in] constFlag <b>true</b> string is a <b>const</b>, <b>false</b> otherwise
    * @return (SlipCell&) reference to inserted cell
    */
                                                                                // List Cells must be unlinked

   /**
    * @fn SlipSequencer::insRight(SlipReader&       X)
    * @brief Insert the reader current cell to the right of the current cell.
    * @param[in] X (SlipReader&) value to be inserted
    * @return (SlipCell&) reference to inserted cell
    */
   SlipCell& SlipSequencer::insRight(SlipReader&       X) {                      // insert X to current cell left
      SlipCell& Y = X.currentCell();
      if (!X.currentCell().isHeader()) X.advanceLWL();
      return link->insRight(Y);
   }; // SlipCell& SlipSequencer::insRight(SlipReader&  X)

   /**
    * @fn SlipSequencer::insRight(SlipSequencer&    X)
    * @brief Insert a sequencer current cell to the right of the current cell.
    * @param[in] X (SlipReader&) value to be inserted
    * @return (SlipCell&) reference to inserted cell
    */
   SlipCell& SlipSequencer::insRight(SlipSequencer&    X) {                     // insert X to current cell left
      SlipCell& Y = X.currentCell();
      if (!X.currentCell().isHeader()) X.advanceLWL();
      return link->insRight(Y);
   }; // SlipCell& SlipSequencer::insRight(SlipSequencer&    X)

   /**
    * @fn SlipSequencer::insRight(SlipCell&  X)
    * @brief Insert an input SlipCell object to the right of the current cell.
    * <p>The input object must be a valid source, and the current (<b>this</b>)
    *    object a valid destination for the insertion.</p>
    * @param[in] X (SlipCell&) SlipCell object reference
    * @return (SlipCell&) reference to inserted cell
    */

   /**
    * @fn SlipSequencer::insRight(bool       X)
    * @brief Insert a bool to the right of the current cell.
    * <p>A <em>SlipDatum</em> cell with type bool containing the input value
    *    is inserted to the right of the current cell.</p>
    * @param[in] X (bool) value to be inserted
    * @return (SlipCell&) reference to inserted cell
    */

   /**
    * @fn SlipSequencer::insRight(DOUBLE       X)
    * @brief Insert a DOUBLE to the right of the current cell.
    * <p>A <em>SlipDatum</em> cell with type DOUBLE containing the input value
    *    is inserted to the right of the current cell.</p>
    * @param[in] X (DOUBLE) value to be inserted
    * @return (SlipCell&) reference to inserted cell
    */

   /**
    * @fn SlipSequencer::insRight(ULONG      X)
    * @brief Insert a ULONG to the right of the current cell.
    * <p>A <em>SlipDatum</em> cell with type ULONG containing the input value
    *    is inserted to the right of the current cell.</p>
    * @param[in] X (ULONG) value to be inserted
    * @return (SlipCell&) reference to inserted cell
    */

   /**
    * @fn SlipSequencer::insRight(LONG       X)
    * @brief Insert a LONG to the right of the current cell.
    * <p>A <em>SlipDatum</em> cell with type LONG containing the input value
    *    is inserted to the right of the current cell.</p>
    * @param[in] X (LONG) value to be inserted
    * @return (SlipCell&) reference to inserted cell
    */

   /**
    * @fn SlipSequencer::insRight(UCHAR      X)
    * @brief Insert a UCHAR to the left of the current cell.
    * <p>A <em>SlipDatum</em> cell with type UCHAR containing the input value
    *    is inserted to the left of the current cell.</p>
    * @param[in] X (UCHAR) value to be inserted
    * @return (SlipCell&) reference to inserted cell
    */

   /**
    * @fn SlipSequencer::insRight(CHAR       X)
    * @brief Insert a CHAR to the left of the current cell.
    * <p>A <em>SlipDatum</em> cell with type CHAR containing the input value
    *    is inserted to the left of the current cell.</p>
    * @param[in] X (CHAR) value to be inserted
    * @return (SlipCell&) reference to inserted cell
    */

   /**
    * @fn SlipSequencer::insRight(const PTR X, const void* operation)
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
    * @return (SlipCell&) reference to inserted cell
    */

   /**
    * @fn SlipSequencer::insRight(const string& X, bool constFlag)
    * @brief Insert a string to the left of the current cell.
    * <p>A <em>SlipDatum</em> cell with type string containing the input value
    *    is inserted to the left of the current cell.</p>
    * @param[in] X (string&) value to be inserted
    * @param[in] constFlag <b>true</b> string is a <b>const</b>, <b>false</b> otherwise
    * @return (SlipCell&) reference to inserted cell
    */

   /**
    * @fn SlipSequencer::insRight(const string*    X, bool constFlag)
    * @brief Insert a string to the left of the current cell.
    * <p>A <em>SlipDatum</em> cell with type string containing the input value
    *    is inserted to the left of the current cell.</p>
    * @param[in] X (string*) value to be inserted
    * @param[in] constFlag <b>true</b> string is a <b>const</b>, <b>false</b> otherwise
    * @return (SlipCell&) reference to inserted cell
    */

   /**
    * @fn SlipSequencer::moveLeft(SlipCell& X)
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
   SlipCell& SlipSequencer::moveLeft(SlipReader& X) {                           // Move the current cell to the left of X
      SlipCell& cell = X.currentCell();
      if (!X.isHeader()) X.advanceLWL();
      currentCell().moveLeft(cell);
      return currentCell();
   }; // SlipCell& SlipSequencer::moveLeft(SlipReader& X)

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
   SlipCell& SlipSequencer::moveLeft(SlipSequencer& X) {                        // Move the current cell to the left of X
      SlipCell& cell = X.currentCell();
      if (!X.isHeader()) X.advanceLWL();
      currentCell().moveLeft(cell);
      return currentCell();
   }; // SlipCell& SlipSequencer::moveLeft(SlipSequencer& X)

   /**
    * @fn SlipSequencer::moveRight(SlipCell& X)
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
    * @param[in] X (SlipCell&) to be inserted to the right
    * @return (SlipCell&) reference to the current cell
    */

   /**
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
   SlipCell& SlipSequencer::moveRight(SlipReader& X) {                          // Move the current cell to the left of X
      SlipCell& cell = X.currentCell();
      if (!X.isHeader()) X.advanceLWL();
      currentCell().moveRight(cell);
      return currentCell();
   }; // SlipCell& SlipSequencer::moveRight(SlipReader& X)

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
    * @return (SlipCell&) reference to the sequencer
    */
   SlipCell& SlipSequencer::moveRight(SlipSequencer& X) {                       // Move the current cell to the left of X
      SlipCell& cell = X.currentCell();
      if (!X.isHeader()) X.advanceLWL();
      currentCell().moveRight(cell);
      return currentCell();
   }; // SlipCell& SlipSequencer::moveRight(SlipSequencer& X)


   /**
    * @fn SlipSequencer::moveListLeft(SlipCell& X)
    * @brief Move the input list to the left of the current cell.
    * <p>Check to see if the source and destination of the move are valid. If
    *    valid, then:</p>
    * <ul>
    *    <li>The entire list is moved to the right of the current cell and the
    *        list is empty after the move.</li>
    *    <li>If the input is a list header, then the header has an empty list.</li>
    *    <li>If the list is a sublist then the sublist reference is to an
    *        empty list.</li>
    * </ul>
    * <p>The sequencer current cell reference is unchanged.</p>
    * @sa slip::SlipCell::moveListLeft()
    * @param[in] X (SlipCell&) list to be inserted to the left
    * @return (SlipCell&) reference to the current cell
    */

   /**
    * @brief Move the reader current list to the left of the current cell.
    * <ul>
    *    <li>The entire list is moved to the left of the current object and
    *        the list is empty after the move. The input SlipReader current
    *        cell and current list are the same.</li>
    * </ul>
    * <p>The sequencer current cell reference is unchanged.</p>
    * @sa slip::SlipCell::moveListLeft()
    * @param[in] X (SlipReader&) current list to be inserted to the left
    * @return (SlipCell&) reference to the current cell
    */
   SlipCell& SlipSequencer::moveListLeft(SlipReader& X) {                       // Move the current cell to the left of X
      X.reset();
      currentCell().moveListLeft(X.currentList());
      return currentCell();
   }; // SlipCell& SlipSequencer::moveListLeft(SlipReader& X)

   /**
    * @brief Move the sequencer current list to the left of the current cell.
    * <ul>
    *    <li>The entire list is moved to the left of the current object and
    *        the list is empty after the move. The input SlipSequencer
    *        references an empty list.</li>
    *    <li>If the input is not a list then post an error message. The
    *        input sequencer current cell is unchanged.</li>
    * </ul>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E1034</b> Invalid Operation for Data Cell Type.</li>
    * </ul>
    * @sa slip::SlipCell::moveListLeft()
    * @param[in] X (SlipSequencer&) current list to be inserted to the left
    * @return (SlipCell&) reference to the current cell
    */
   SlipCell& SlipSequencer::moveListLeft(SlipSequencer& X) {                    // Move the current cell to the left of X
      if (X.currentCell().isData()) {
         postError(__FILE__, __LINE__, SlipErr::E1034, "moveListLeft", "", "", X.currentCell());
      } else {
         SlipCell& cell = X.currentCell();
         currentCell().moveListLeft(cell);
      }
      return currentCell();
   }; // SlipCell& SlipSequencer::moveListLeft(SlipSequencer& X)

   /**
    * @fn SlipSequencer::moveListRight(SlipCell& X)
    * @brief Move the input cell to the right of the current cell.
    * <p>Check to see if the source and destination of the move are valid. If
    *    valid, then:</p>
    * <ul>
    *    <li>The entire list is moved to the right of the current cell and the
    *        list is empty after the move.</li>
    *    <li>If the input is a list header, then the header has an empty list.</li>
    *    <li>If the list is a sublist then the sublist reference is to an
    *        empty list.</li>
    * </ul>
    * @sa slip::SlipCell::moveListRight()
    *
    * @param[in] X (SlipCell&) to be inserted to the right
    * @return (SlipCell&) reference to the current cell
    */

   /**
    * @brief Move the reader current cell to the right of the current cell.
    * <ul>
    *    <li>The entire list is moved to the right of the current object and
    *        the list is empty after the move. The input SlipReader references
    *        an empty list and the current cell is set to the list header.</li>
    * </ul>
    * <p>The sequencer current cell reference is unchanged.</p>
    * @sa slip::SlipCell::moveListRight()
    * @param[in] X (SlipReader&) current cell to be inserted to the left
    * @return (SlipCell&) reference to the current cell
    */
   SlipCell& SlipSequencer::moveListRight(SlipReader& X) {                      // Move the current cell to the left of X
      X.reset();
      currentCell().moveListRight(X.currentList());
      return currentCell();
   }; // SlipCell& SlipSequencer::moveListRight(SlipReader& X)

   /**
    * @brief Move the sequencer current cell to the right of the current cell.
    * <ul>
    *    <li>The entire list is moved to the right of the current object and
    *        the list is empty after the move. The input SlipSequencer
    *        references an empty list.</li>
    *    <li>If the input is not a list then post an error message. The
    *        input sequencer current cell is unchanged.</li>
    * </ul>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E1034</b> Invalid Operation for Data Cell Type.</li>
    * </ul>
    * @sa slip::SlipCell::moveListLeft()
    * @param[in] X (SlipSequencer&) current cell to be inserted to the left
    * @return (SlipCell&) reference to the sequencer
    */
   SlipCell& SlipSequencer::moveListRight(SlipSequencer& X) {                   // Move the current cell to the left of X
      if (X.currentCell().isData()) {
         postError(__FILE__, __LINE__, SlipErr::E1034, "moveListLeft", "", "", X.currentCell());
      } else {
         SlipCell& cell = X.currentCell();
         currentCell().moveListRight(cell);
      }
      return currentCell();
   }; // SlipCell& SlipSequencer::moveListRight(SlipSequencer& X)


   /**
    * @fn SlipSequencer::replace(const SlipReader& X)
    * @brief Replace the current cell with the current cell of the input reader.
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param[in] X (SlipSequencer&) replacement value
    * @return (SlipCell&) reference to changed cell
    */

   /**
    * @fn SlipSequencer::replace(const SlipSequencer& X)
    * @brief Replace the current cell with the current cell of the input sequencer.
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param[in] X (SlipSequencer&) replacement value
    * @return (SlipCell&) reference to changed cell
    */

   /**
    * @fn SlipSequencer::replace(const SlipCell& X)
    * @brief Replace the current cell with the input value.
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param[in] X (SlipCell&) replacement value
    * @return (SlipCell&) reference to changed cell
    */

   /**
    * @fn SlipSequencer::replace(bool X)
    * @brief Replace the current cell with the input value.
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param[in] X (bool) replacement value
    * @return (SlipCell&) reference to changed cell
    */

   /**
    * @fn SlipSequencer::replace(CHAR X)
    * @brief Replace the current cell with the input value.
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param[in] X (CHAR) replacement value
    * @return (SlipCell&) reference to changed cell
    */

   /**
    * @fn SlipSequencer::replace(UCHAR X)
    * @brief Replace the current cell with the input value.
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param[in] X (UCHAR) replacement value
    * @return (SlipCell&) reference to changed cell
    */

   /**
    * @fn SlipSequencer::replace(LONG X)
    * @brief Replace the current cell with the input value.
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param[in] X (LONG) replacement value
    * @return (SlipCell&) reference to changed cell
    */

   /**
    * @fn SlipSequencer::replace(ULONG X)
    * @brief Replace the current cell with the input value.
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param[in] X (ULONG) replacement value
    * @return (SlipCell&) reference to changed cell
    */

   /**
    * @fn SlipSequencer::replace(DOUBLE X)
    * @brief Replace the current cell with the input value.
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param[in] X (DOUBLE) replacement value
    * @return (SlipCell&) reference to changed cell
    */

   /**
    * @fn SlipSequencer::replace(const PTR X, const void* operation) {
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
    * @return (SlipCell&) reference to changed cell
    */

   /**
    * @fn SlipSequencer::replace(const string& X, bool constFlag)
    * @brief Replace the current cell with the input value.
    * <p>The sequencer will point to the new cell.</p>
    * @param[in] X (string&) replacement value
    * @param[in] constFlag <b>true</b> string and object referenced not copied
    * @return (SlipCell&) reference to changed cell
    */

   /**
    * @fn SlipSequencer::replace(const string* X, bool constFlag)
    * @brief Replace the current cell with the input value.
    * <p>The sequencer will point to the new cell.</p>
    * @param[in] X (string*) replacement value
    * @param[in] constFlag <b>true</b> string and object referenced not copied
    * @return (SlipCell&) reference to changed cell
    */

   /**
    * @brief Unlink the current cell from a list and fixup sequencer.
    * <p>The current cell is unlinked and the sequencer is advanced one cell
    *    to the left.</p>
    * @return (SlipCell&) a reference to the unlinked cell.
    */
   SlipCell& SlipSequencer::unlink() {                                          // unlink cell from list
       SlipCell& cell = currentCell();
       advanceLWL();
       return cell.unLink();
   }; // SlipCell& SlipSequencer::unlink()

               /*****************************************
               *             Miscellaneous              *
               ******************************************/

   /**
    * @fn SlipSequencer::deleteCell()
    * @brief Delete the current cell.
    * <p>The current cell is deleted and the sequencer current cell points to
    *    the previous cell.</p>
    * @return (SlipSequencer&) reference to the sequencer
    */
   SlipSequencer& SlipSequencer::deleteCell() {                                 // delete the current cell
      SlipCell* cell = link;
      advanceLWL();
      delete cell;
      return *this;
   }; // SlipCell& SlipSequencer::deleteCell()

   /**
    * @fn SlipSequencer::getClassType() const
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
    * @fn SlipSequencer::getName() const
    * @brief ASCII name of current cell
    * @return (string) ASCII name of current cell
    */

   /**
    * @fn SlipSequencer::isData() const
    * @brief <b>true</b> if current cell is a <em>SlipDatum</em> cell.
    * @return (bool) <b>true</b> if current cell is a <em>SlipDatum</em> cell.
    */

   /**
    * @fn SlipSequencer::isDiscrete() const
    * @brief <b>true</b> if current cell cell contains a discrete number.
    * @return (bool) <b>true</b> if current cell contains a discrete number.
    */

   /**
    * @fn SlipSequencer::isHeader() const
    * @brief <b>true</b> if current cell is a <em>SlipHeader</em> cell.
    * @return (bool) <b>true</b> if current cell is a <em>SlipHeader</em> cell
    */

   /**
    * @fn SlipSequencer::isName() const
    * @brief <b>true</b> if current cell is a <em>SlipSublist</em> cell.
    * @return (bool) <b>true</b> if current cell is a <em>SlipSublist</em> cell
    */

   /**
    * @fn SlipSequencer::isNumber() const
    * @brief <b>true</b> if current cell is a numeric type.
    * @return (bool) <b>true</b> if current cell is a numeric type
    */

   /**
    * @fn SlipSequencer::isPtr() const
    * @brief <b>true</b> if current cell is a user defined type.
    * @return (bool) <b>true</b> if current cell is a user defined type
    */

   /**
    * @fn SlipSequencer::isReal() const
    * @brief <b>true</b> if current cell contains a real number.
    * @return (bool) <b>true</b> if current cell contains a real number.
    */

   /**
    * @fn SlipSequencer::isString() const
    * @brief <b>true</b> if current cell contains a real number.
    * @return (bool) <b>true</b> if current cell contains a real number.
    */

   /**
    * @fn SlipSequencer::isSublist() const
    * @brief <b>true</b> if current cell is a <em>SlipSublist</em> cell.
    * @return (bool) <b>true</b> if current cell is a <em>SlipSublist</em> cell
    */

            /*****************************************
             *    Miscellaneous Sequencer Methods    *
            ******************************************/
   /**
    * @brief Change current cell to reference the input SlipCell.
    * <p>The input SlipCell is made the current reference of the
    *    SlipSequencer. If the input SlipCell is illegal, then
    *    the SlipSequencer link will point to an illegally
    *    constructed SlipDatum. Any operations with this cell
    *    should yield an error.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2006</b> Cell can not be a local temporary. Create with 'new'.</li>
    *    <li><b>E2013</b> Cell must be in a list. Cell is not in a list.</li>
    *    <li><b>E3019</b> Slip Cell has been deleted and can not be used.</li>
    * </ul>
    * @param[in] X (SlipCell&) new current cell reference
    * @return (SlipSequencer&) reference to the sequencer
    */
   SlipSequencer& SlipSequencer::reset(SlipCell& X) {
      link = (SlipDatum*)illegalDatum;
      if (X.isDeleted()) {
         postError(__FILE__, __LINE__, SlipErr::E3019, "SlipSequencer::reset", "", "", X);
      } else if (X.isUnlinked()){
         postError(__FILE__, __LINE__, SlipErr::E2006, "SlipSequencer::reset", "", "", X);
      } else if (X.isTemp())    {
         postError(__FILE__, __LINE__, SlipErr::E2013, "SlipSequencer::reset", "", "", X);
      } else {
         link = &X;
      }
      return *this;
   }; // SlipSequencer& SlipSequencer::reset(SlipCell& X)

   /**
    * @fn SlipSequencer::reset(SlipHeader& header)
    * @brief Reference the list header as the current cell.
    * @param[in] header (SlipHeader&) sequencer reset value
    * @return (SlipSequencer&) reference to sequencer
    */
   SlipSequencer& SlipSequencer::reset(SlipHeader& header) { link = &header; return *this; }

   /**
    * @brief Change the current cell to be the same as the SlipReader current cell.
    * <p>The sequencer references the current cell of the reader after
    *    completion. If the current cell of the reader is not legal, then
    *    a diagnostic message is output. </p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E3019</b> Slip Cell has been deleted and can not be used.</li>
    * </ul>
    * @param[in] X (SlipReader&) sequencer reset value
    * @return (SlipSequencer&) reference to sequencer
    */
   SlipSequencer& SlipSequencer::reset(SlipReader& X) {
      if (X.currentCell().isDeleted()) {
         postError(__FILE__, __LINE__, SlipErr::E3019, "SlipSequencer::reset", "", "", X.currentCell());
      }
      link = &X.currentCell();
      return *this;
   }; // SlipSequencer& SlipSequencer::reset(SlipReader& X)

   /**
    * @brief Change the current cell to the input current cell.
    * <p>The sequencer references the current cell of the reader after
    *    completion. If the current cell of the reader is not legal, then
    *    a diagnostic message is output. </p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E3019</b> Slip Cell has been deleted and can not be used.</li>
    * </ul>
    * @param[in] X (SlipSequencer&) sequencer reset value
    * @return (SlipSequencer&) reference to sequencer
    */
   SlipSequencer& SlipSequencer::reset(SlipSequencer& X) {
      if (X.currentCell().isDeleted()) {
         postError(__FILE__, __LINE__, SlipErr::E3019, "SlipSequencer::reset", "", "", X.currentCell());
      }
      link = &X.currentCell();
      return *this;
   }; // SlipSequencer& SlipSequencer::reset(SlipSequencer& X)

   /**
    * @fn SlipSequencer::currentCell() const
    * @brief Reference the current cell.
    * @return (SlipCell&) current cell
    */

            /*****************************************
             *         Miscellaneous Methods         *
            ******************************************/

   /**
    * @fn SlipSequencer::getLeftLink()  const
    * @brief Return the leftLink.
    * @return Return the leftLink
    */

   /**
    * @fn SlipSequencer::getRightLink()
    * @brief Return the rightLink.
    * @return Return the rightLink
    */

   /**
    * @fn SlipSequencer::toString()
    * @brief The pretty-printed value of the current cell.
    * @return (string) pretty-printed current cell value
    */

   /**
    * @fn SlipSequencer::dump()
    * @brief Ugly representation of the current cell.
    * @return (string) uglified cell representation
    */
   
}; // namespace slip
