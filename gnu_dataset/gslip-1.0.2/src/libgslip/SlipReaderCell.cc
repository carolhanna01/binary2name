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
 * @file   SlipReaderCell.cpp
 * @author A. Schwarz
 * @date   June 24, 2012
 */

# include <string>
# include <sstream>
# include <iomanip>
# include "SlipCellBase.h"
# include "SlipDef.h"
# include "SlipHeader.h"
# include "SlipOp.h"
# include "SlipReaderCell.h"
# include "SlipReader.h"

namespace slip {

    /*********************************************************
     *                   Private Functions                   *
    **********************************************************/
   /**
    * @brief Return the back link pointer on a SlipReaderCell chain.
    * <p>Recursion into a list causes a new SlipReaderCell object to be
    *    created. The new object contains a pointer to the previous object
    *    on the chain. The first SlipReaderCell object on the chain has
    *    a <b>null</b> back link pointer value.</p>
    * <p>This reference is an invariant property of a SlipReaderCell object.</p>
    * @return SlipReaderCell* chain back link pointer
    */
   inline
   SlipReaderCell* SlipReaderCell::getLink()            { return (SlipReaderCell*)getRightLink(); }

   /**
    * @brief Set the back link pointer in the current SlipReaderCell object.
    * <p>Recursion into a list causes a new SlipReaderCell object to be
    *    created. The new object contains a pointer to the previous object
    *    on the chain. The first SlipReaderCell object on the chain has
    *    a <b>null</b> back link pointer value.</p>
    * <p>This reference is an invariant property of a SlipReaderCell object.</p>
    */
   inline
   void SlipReaderCell::setLink(SlipReaderCell& X)      { setRightLink(&X);        }         // Set link to previous reader cell

   /**
    * @fn SlipReaderCell::setCurrentCell(SlipCellBase& X)
    * @brief Set the current SlipCell object pointer in the current SlipReaderCell object.
    * <p>Each SlipReaderCell object contains a pointer to the current list
    *    SlipCell object being traversed. For each advance the current cell pointer
    *    is updated.</p>
    * @param[in] X (SlipCellBase &) reference of the SlipCell object
    */

   /**
    * @brief Set the current list depth.
    * <p>Each entry into a subordinate list causes the list depth to be
    *    increased by one. The topmost list depth is zero.</p>
    * <p>This field represents the nesting depth of the current sublist. Each
    *    entry into a sublist causes a new SlipReaderCell object to be created
    *    with a sublist depth one more than the depth of the next higher
    *    sublist.</p>
    * <p>The current depth is an invariant property of a SlipReaderCell object.</p>
    * @param[in] X (USHORT) value of the current depth.
    */
   inline
   void SlipReaderCell::setListDepth(USHORT X)          { *getReaderDepth() = X;   }         // Set list depth

   /**
    * @brief Set the SlipHeader pointer in the current SlipReaderCell object.
    * <p>Set the current list pointer (SlipHeader) in the current SlipReaderCell
    *    object. Each SlipReaderCell contains a pointer to the current list
    *    being referenced/advanced.</p>
    * <p>This reference is an invariant property of a SlipReaderCell object.</p>
    * @param[in] X (SlipHeader &) reference to the current list header
    */
   inline
   void SlipReaderCell::setListHeader(const SlipHeader& X){ *getReaderTop() = (void*)&X; }   // Set list header for current level


    /*********************************************************
     *                  Protected Functions                  *
    **********************************************************/

   /**
    * @fn SlipReaderCell::setLink(SlipReaderCell& reader, SlipReaderCell& X)
    * @brief Set the back link pointer to the preceeding SlipReaderCell in the
    *    chain.
    * <p>Recursion into a list causes a new SlipReaderCell object to be
    *    created. The new object contains a pointer to the previous object
    *    on the chain. The first SlipReaderCell object on the chain has
    *    a <b>null</b> back link pointer value.</p>
    * @param[in] reader (SlipReaderCell &) cell to be set
    * @param[in] X (SlipReaderCell &) back link pointer value
    */

   /**@fn SlipReaderCell::setCurrentCell(SlipReaderCell& reader, SlipCellBase& X)
    * @brief Set the current SlipCell object pointer in the current SlipReaderCell object.
    * <p>Each SlipReaderCell object contains a pointer to the current list
    *    SlipCell object being traversed. For each advance the current cell pointer
    *    is updated.</p>
    * @param[in] reader (SlipReaderCell &) cell to be set
    * @param[in] X (SlipCellBase &) reference of the new SlipCell object
    */

   /**
    * @fn SlipReaderCell::setListDepth(SlipReaderCell& reader, USHORT X)
    * @brief Set the current list depth.
    * <p>Each entry into a subordinate list causes the list depth to be
    *    increased by one. The topmost list depth is zero.</p>
    * <p>This field represents the nesting depth of the current sublist. Each
    *    entry into a sublist causes a new SlipReaderCell object to be created
    *    with a sublist depth one more than the depth of the next higher
    *    sublist.</p>
    * <p>The current depth is an invariant property of a SlipReaderCell object.</p>
    * @param[in] reader (SlipReaderCell &) cell to be set
    * @param[in] X (USHORT) value of the current depth.
    */

   /**
    * @fn SlipReaderCell::setListHeader(SlipReaderCell& reader, SlipHeader& X)
    * @brief Set the SlipHeader pointer in the current SlipReaderCell object.
    * <p>Set the current list pointer (SlipHeader) in the current SlipReaderCell
    *    object. Each SlipReaderCell contains a pointer to the current list
    *    being referenced/advanced.</p>
    * <p>This reference is an invariant property of a SlipReaderCell object.</p>
    * @param[in] reader (SlipReaderCell &) cell to be set
    * @param[in] X (SlipHeader &) reference to the current list header
    */

   /**
    * @fn SlipReaderCell::getLink(SlipReaderCell& reader)
    * @brief Return the back link pointer on a SlipReaderCell chain.
    * <p>Recursion into a list causes a new SlipReaderCell object to be
    *    created. The new object contains a pointer to the previous object
    *    on the chain. The first SlipReaderCell object on the chain has
    *    a <b>null</b> back link pointer value.</p>
    * <p>This reference is an invariant property of a SlipReaderCell object.</p>
    * @param[in] reader (SlipReaderCell &) cell containing back link pointer
    * @return SlipReaderCell* chain back link pointer
    */

   /*********************************************************
    *                   Public Functions                    *
   **********************************************************/

   /**
    * @fn SlipReaderCell::SlipReaderCell(SlipCell* header)
    * @brief Constructor for a SlipReaderCell object.
    * <p>A new SlipReaderCell object is created with the current list and
    *    current cell pointing to the input lise header. The depth is zero
    *    and the back link pointer is <b>null</b>.</p>
    * @param[in] header (SlipCell *) pointer to a list header
    */

   /**
    * @fn SlipReaderCell::SlipReaderCell(const SlipCell& header)
    * @brief Constructor for a SlipReaderCell object.
    * <p>A new SlipReaderCell object is created with the current list and
    *    current cell pointing to the input lise header. The depth is zero
    *    and the back link pointer is <b>null</b>.</p>
    * @param[in] header (SlipCell &) pointer to a list header
    */

   /**
    * @fn SlipReaderCell::~SlipReaderCell()
    * @brief Destructor of a SlipReaderCell object.
    * <p>If the current SlipReaderCell is not deleted, then, its body is
    *    returned to the AVSL.</p>
    */

            /*****************************************
             *           Reader Assignment           *
            ******************************************/

   /**
    * @brief Assign the LHS to the SlipReaderCell object in the RHS.
    * <p>Creates a reference to the RHS list and cell. At the end of
    *    the operation, the existing LHS reader chain is deleted and the
    *    topmost reader cell will reference the same list and list cell
    *    as the input.</p>
    * <p>The LHS side will not copy the RHS SlipReaderCell chain. The LHS
    *    will be the topmost and only SlipReaderCell object referencing
    *    the RHS list and list cell pointers. This has the effect of:</p>
    * <ol>
    *    <li>Deleting the LHS SlipReaderCell chain.</li>
    *    <li>The LHS list depth will be zero.</li>
    *    <li>The LHS back link pointer will be <b>null</b>.</li>
    *    <li>The LHS SlipHeader pointer will be the same as the RHS.</li>
    *    <li>The LHS SlipCell pointer will be the same as the RHS.</li>
    * </ol>
    * <p>And note that if the RHS is nested, the LHS will not be. Therefore,
    *    the LHS will be the topmost reference to a nested (RHS) reference.</p>
    * @param[in] X (SlipReaderCell &) RHS of assignment
    * @return a SlipReaderCell pointer pointing to the input  list and cell
    */
   SlipReaderCell& SlipReaderCell::operator=(const SlipReaderCell& X) {         // Assign the input to the SlipReaderCell
      SlipReaderCell* reader = resetTop();
      setCurrentCell(X.currentCell());
      setListHeader(X.currentList());
      return *reader;
   }; // SlipReaderCell& SlipReaderCell::operator=(const SlipReaderCell& X)

   /**
    * @brief Assign the LHS to a SlipHeader (list) pointer.
    * <p>Creates a reference to the RHS list header. At the end of
    *    the operation, the existing LHS reader chain is deleted and the
    *    topmost reader cell will reference the input list.</p>
    * <p>The LHS will be the topmost and only SlipReaderCell object referencing
    *    the RHS list and list cell pointers. This has the effect of:</p>
    * <ol>
    *    <li>Deleting the LHS SlipReaderCell chain.</li>
    *    <li>The LHS list depth will be zero.</li>
    *    <li>The LHS back link pointer will be <b>null</b>.</li>
    *    <li>The LHS SlipHeader pointer will be the same as the RHS.</li>
    *    <li>The LHS SlipCell pointer will be the same as the RHS.</li>
    * </ol>
    * @param[in] X (SlipReaderCell &) RHS of assignment
    * @return a SlipReaderCell pointer pointing to the input list and cell
    */
   SlipReaderCell& SlipReaderCell::operator=(const SlipHeader& X) {             // Assign the input to the SlipReaderCell
      SlipReaderCell* reader = resetTop();
      setCurrentCell((SlipCellBase&)X);
      setListHeader(X);
      return *reader;
   }; // SlipReaderCell& SlipReaderCell::operator=(const SlipHeader& X)

            /*****************************************
             *            Reader Iteration           *
            ******************************************/

   /**
    * @brief Advance linearly left to the preceeding data cell.
    * <p>This method will advance left (use the <em>leftLink</em> field) to the
    *    preceeding data cell (SlipDatum). If the preceeding cell is not a data
    *    cell then movement will continue until either a data cell is found or
    *    the list header is found.</p>
    * <p>All movement is in the current list. Movement to a sublist cell
    *    does not cause traversal of the indentured list.</p>
    * <p>At exit, the current cell pointer is modified to point to a
    *    data cell or the list header.</p>
    * @return (SlipREaderCell *) pointer to the current SlipReaderCell
    */
   SlipReaderCell* SlipReaderCell::advanceLEL() {                               // advance linear element left
      SlipCellBase* link = &currentCell();
      for (link = link->getLeftLink(); !link->isHeader(); link = link->getLeftLink()) {
         if (link->isData()) {
            break;
         }
      }
      setCurrentCell(*link);
      return this;
   }; // SlipReaderCell* SlipReaderCell::advanceLEL()


   /**
    * @brief Advance linearly right to the next data cell.
    * <p>This method will advance right (use the <em>rightLink</em> field) to the
    *    succeeding data cell (SlipDatum). If the succeeding cell is not a data
    *    cell then movement will continue until either a data cell is found or
    *    the list header is found.</p>
    * <p>All movement is in the current list. Movement to a sublist cell
    *    does not cause traversal of the indentured list.</p>
    * <p>At exit, the current cell pointer is modified to point to a
    *    data cell or the list header.</p>
    * @return (SlipREaderCell *) pointer to the current SlipReaderCell
    */
   SlipReaderCell* SlipReaderCell::advanceLER() {                               // advance linear element right
      SlipCellBase* link = &currentCell();
      for (link = link->getRightLink(); !link->isHeader(); link = link->getRightLink()) {
         if (link->isData()) {
            break;
         }
      }
      setCurrentCell(*link);
      return this;
   }; // SlipReaderCell* SlipReaderCell::advanceLER()

   /**
    * @brief Advance linearly left to the preceeding sublist cell.
    * <p>This method will advance left (use the <em>leftLink</em> field) to the
    *    preceeding sublist cell (SlipSublist). If the preceeding cell is not a
    *    sublist cell then movement will continue until either a sublist cell
    *    is found or the list header is found.</p>
    * <p>All movement is in the current list. Movement to a sublist cell
    *    does not cause traversal of the indentured list.</p>
    * <p>At exit, the current cell pointer is modified to point to a
    *    sublist cell or the list header.</p>
    * @return (SlipREaderCell *) pointer to the current SlipReaderCell
    */
   SlipReaderCell* SlipReaderCell::advanceLNL() {                               // advance linear name left
      SlipCellBase* link = &currentCell();
      for (link = link->getLeftLink(); !link->isHeader(); link = link->getLeftLink()) {
         if ((link->isSublist())) {
            break;
         }
      }
      setCurrentCell(*link);
      return this;
   }; // SlipReaderCell* SlipReaderCell::advanceLNL()

   /**
    * @brief Advance linearly right to the next sublist cell.
    * <p>This method will advance right (use the <em>rightLink</em> field) to the
    *    succeeding sublist cell (SlipSublist). If the succeeding cell is not a
    *    sublist cell then movement will continue until either a sublist cell
    *    is found or the list header is found.</p>
    * <p>All movement is in the current list. Movement to a sublist cell
    *    does not cause traversal of the indentured list.</p>
    * <p>At exit, the current cell pointer is modified to point to a
    *    sublist cell or the list header.</p>
    * @return (SlipREaderCell *) pointer to the current SlipReaderCell
    */
   SlipReaderCell* SlipReaderCell::advanceLNR() {                               // advance linear name right
      SlipCellBase* link = &currentCell();
      for (link = link->getRightLink(); !link->isHeader(); link = link->getRightLink()) {
         if ((link->isSublist())) {
            break;
         }
      }
      setCurrentCell(*link);
      return this;
   }; // SlipReaderCell* SlipReaderCell::advanceLNR()

   /**
    * @brief Advance linearly left to the preceeding cell.
    * <p>This method will advance left (use the <em>leftLink</em> field) to the
    *    preceeding cell (SlipDatum, SlipSublist or SlipHeader).</p>
    * <p>All movement is in the current list. Movement to a sublist cell
    *    does not cause traversal of the indentured list.</p>
    * <p>At exit, the current cell pointer is modified to point to the
    *    preceeding cell.</p>
    * @return (SlipREaderCell *) pointer to the current SlipReaderCell
    */
   SlipReaderCell* SlipReaderCell::advanceLWL() {                               // advance linear word right
      SlipCellBase* link = &currentCell();
      link = link->getLeftLink();
      setCurrentCell(*link);
      return this;
   }; // SlipReaderCell* SlipReaderCell::advanceLWL()

   /**
    * @brief Advance linearly right to the preceeding cell.
    * <p>This method will advance left (use the <em>rightLink</em> field) to the
    *    preceeding cell (SlipDatum, SlipSublist or SlipHeader).</p>
    * <p>All movement is in the current list. Movement to a sublist cell
    *    does not cause traversal of the indentured list.</p>
    * <p>At exit, the current cell pointer is modified to point to the
    *    preceeding cell.</p>
    * @return (SlipREaderCell *) pointer to the current SlipReaderCell
    */
   SlipReaderCell* SlipReaderCell::advanceLWR() {                               // advance linear word right
      SlipCellBase* link = &currentCell();
      link = link->getRightLink();
      setCurrentCell(*link);
      return this;
   }; // SlipReaderCell* SlipReaderCell::advanceLWR()

   /**
    * @brief Advance structurally left to the preceeding data cell.
    * <p>This method will advance structurally left (use the <em>leftLink</em>
    *    field) to the preceeding data cell (SlipDatum).</p>
    * <ol>
    *    <li>Advance left (<em>leftLink</em> field).</li>
    *    <li>If the current cell is a sublist (SlipSublist) cell then:</li>
    *    <ol>
    *       <li>The sublist is 'entered'.</li>
    *       <li>A new SlipReaderCell is created representing the indentured
    *           list and linked to the existing SlipReaderCell.</li>
    *       <li>The search continues at step <b>1</b> using the created
    *           SlipReaderCell.</li>
    *    </ol>
    *    <li>If the current cell is a list header (SlipHeader) then:</li>
    *    <ol>
    *       <li>If at the topmost list, return.</li>
    *       <li>Return to the previous list.</li>
    *       <li>Delete the current SlipReaderCell.</li>
    *       <li>Make the preceeding SlipReaderCell current.</li>
    *       <li>Continue at step <b>1</b>.</li>
    *    </ol>
    *    <li>If the current cell is a data cell return.</li>
    * </ol>
    * <p>Entry to a subordinate list is made if the preceeding list cell is
    *    a sublist cell. Exit from the current list is made if the preceeding
    *    cell is a list header. Return from the method is done if either a
    *    data cell is found or the topmost list header is found.</p>
    * <p>The algorithm performs a depth first search for a data cell and
    *    returns the SlipReaderCell referencing the found data cell.</p>
    * <p>At exit, the current cell pointer is modified to point to a
    *    data cell or the topmost list header.</p>
    * @return (SlipReaderCell *) pointer to the current SlipReaderCell
    */
   SlipReaderCell* SlipReaderCell::advanceSEL() {                               // advance structural element left
      SlipReaderCell* reader = this;
      SlipCellBase*   link   = ((reader->currentCell()).getLeftLink());
      while(!link->isData()) {
         if (link->isSublist()) {
            setCurrentCell(*reader, *link);
            reader = downLevel(*reader, link);
            link   = (SlipCellBase*)&reader->currentCell();
         } else if (reader->listDepth() == 0) {
            break;
         } else {
            reader = reader->upLevel();
            link   = (SlipCellBase*)&reader->currentCell();
         }
         link = link->getLeftLink();
      }
      reader->setCurrentCell(*link);
      return reader;
   }; // SlipReaderCell* SlipReaderCell::advanceSEL()

   /**
    * @brief Advance structurally right to the next data cell.
    * <p>This method will advance structurally right (use the <em>rightLink</em>
    *    field) to the succeeding data cell (SlipDatum).</p>
    * <ol>
    *    <li>Advance right (<em>rightLink</em> field).</li>
    *    <li>If the current cell is a sublist (SlipSublist) cell then:</li>
    *    <ol>
    *       <li>The sublist is 'entered'.</li>
    *       <li>A new SlipReaderCell is created representing the indentured
    *           list and linked to the existing SlipReaderCell.</li>
    *       <li>The search continues at step <b>1</b> using the created
    *           SlipReaderCell.</li>
    *    </ol>
    *    <li>If the current cell is a list header (SlipHeader) then:</li>
    *    <ol>
    *       <li>If at the topmost list, return.</li>
    *       <li>Return to the previous list.</li>
    *       <li>Delete the current SlipReaderCell.</li>
    *       <li>Make the preceeding SlipReaderCell current.</li>
    *       <li>Continue at step <b>1</b>.</li>
    *    </ol>
    *    <li>If the current cell is a data cell return.</li>
    * </ol>
    * <p>Entry to a subordinate list is made if the succeeding list cell is
    *    a sublist cell. Exit from the current list is made if the succeeding
    *    cell is a list header. Return from the method is done if either a
    *    data cell is found or the topmost list header is found.</p>
    * <p>The algorithm performs a reverse depth first search for a data cell and
    *    returns the SlipReaderCell referencing the found data cell.</p>
    * <p>At exit, the current cell pointer is modified to point to a
    *    data cell or the topmost list header.</p>
    * @return (SlipREaderCell *) pointer to the current SlipReaderCell
    */
   SlipReaderCell* SlipReaderCell::advanceSER() {                               // advance structural element right
      SlipReaderCell* reader = this;
      SlipCellBase*   link   = (SlipCellBase*)&reader->currentCell();
      if ((reader->currentCell()).isHeader())
         link   = link->getRightLink();
      do {
         if (link->isSublist()) {
            reader->setCurrentCell(*link);
            reader = downLevel(*reader, link);
            link   = (SlipCellBase*)&reader->currentCell();
         } else if (link->isHeader()) {
            if (reader->listDepth() == 0) {
               break;
            } else {
               reader = reader->upLevel();
               link   = (SlipCellBase*)&reader->currentCell();
            }
         }
         link = link->getRightLink();
      } while(!link->isData());
      reader->setCurrentCell(*link);
      return reader;
   }; // SlipReaderCell* SlipReaderCell::advanceSER()

   /**
    * @brief Advance structurally left to the preceeding sublist cell.
    * <p>This method will advance structurally left (use the <em>leftLink</em>
    *    field) to the preceeding sublist cell (SlipSublist). This is a
    *    depth first search of a acyclic graph to find a sublist cell.</p>
    * <ol>
    *    <li>If the current cell is a sublist (SlipSublist) cell then:</li>
    *    <ol>
    *       <li>The sublist is 'entered'.</li>
    *       <li>A new SlipReaderCell is created representing the indentured
    *           list and linked to the existing SlipReaderCell.</li>
    *    </ol>
    *    <li>Advance left to the preceeding cell.</li>
    *    <li>Continue until a sublist cell is found:</li>
    *    <ol>
    *       <li>If the current cell the topmost list header, we are done.</li>
    *       <li>If the current cell is a list header, return to the previous
    *           list and delete the current SlipReaderCell.</li>
    *       <li>Advance to the preceeding cell.</li>
    *    </ol>
    * </ol>
    * <p>Entry to a subordinate list is made if the  current list cell is
    *    a sublist cell. Exit from the current list is made if the current
    *    cell is a list header. Return from the method is done if either a
    *    sublist cell is found or the topmost list header is found.</p>
    * <p>The algorithm performs a depth first search for a sublist cell and
    *    returns the SlipReaderCell referencing the found sublist cell.</p>
    * <p>At exit, the current cell pointer is modified to point to a
    *    sublist cell or the topmost list header.</p>
    * @return (SlipREaderCell *) pointer to the current SlipReaderCell
    */
   SlipReaderCell* SlipReaderCell::advanceSNL() {                               // advance structural name left
      SlipReaderCell* reader = this;
      SlipCellBase*   link   = (SlipCellBase*)&reader->currentCell();
      if (reader->currentCell().isSublist()) {
         reader = downLevel(*reader, link);
         link   = (SlipCellBase*)&reader->currentCell();
      }
      if ((reader->currentCell()).isHeader())
         link   = link->getLeftLink();
      while(!link->isSublist()) {
         if (link->isHeader()) {
            if (reader->listDepth() == 0) {
               break;
            } else {
               reader = reader->upLevel();
               link   = (SlipCellBase*)&reader->currentCell();
            }
         }
         link = link->getLeftLink();
      }
      reader->setCurrentCell(*link);
      return reader;
   }; // SlipReaderCell* SlipReaderCell::advanceSNL()

   /**
    * @brief Advance structurally right to the next sublist cell.
    * <p>This method will advance structurally right (use the <em>rightLink</em>
    *    field) to the succeeding sublist cell (SlipSublist). This is a
    *    depth first search of a acyclic graph to find a sublist cell.</p>
    * <ol>
    *    <li>If the current cell is a sublist (SlipSublist) cell then:</li>
    *    <ol>
    *       <li>The sublist is 'entered'.</li>
    *       <li>A new SlipReaderCell is created representing the indentured
    *           list and linked to the existing SlipReaderCell.</li>
    *    </ol>
    *    <li>Advance right to the succeeding cell.</li>
    *    <li>Continue until a sublist cell is found:</li>
    *    <ol>
    *       <li>If the current cell the topmost list header, we are done.</li>
    *       <li>If the current cell is a list header, return to the previous
    *           list and delete the current SlipReaderCell.</li>
    *       <li>Advance to the succeeding cell.</li>
    *    </ol>
    * </ol>
    * <p>Entry to a subordinate list is made if the  current list cell is
    *    a sublist cell. Exit from the current list is made if the current
    *    cell is a list header. Return from the method is done if either a
    *    sublist cell is found or the topmost list header is found.</p>
    * <p>The algorithm performs a depth first search for a sublist cell and
    *    returns the SlipReaderCell referencing the found sublist cell.</p>
    * <p>At exit, the current cell pointer is modified to point to a
    *    sublist cell or the topmost list header.</p>
    * @return (SlipREaderCell *) pointer to the current SlipReaderCell
    */
   SlipReaderCell* SlipReaderCell::advanceSNR() {                               // advance structural name right
      SlipReaderCell* reader = this;
      SlipCellBase*   link   = (SlipCellBase*)&reader->currentCell();
      if (reader->currentCell().isSublist()) {
         reader = downLevel(*reader, link);
         link   = (SlipCellBase*)&reader->currentCell();
      }
      if ((reader->currentCell()).isHeader())
         link   = link->getRightLink();
      while(!link->isSublist()) {
         if (link->isHeader()) {
            if (reader->listDepth() == 0) {
               break;
            } else {
               reader = reader->upLevel();
               link   = (SlipCellBase*)&reader->currentCell();
            }
         }
         link = link->getRightLink();
      }
      reader->setCurrentCell(*link);
      return reader;
   }; // SlipReaderCell* SlipReaderCell::advanceSNR()

   /**
    * @brief Advance structurally left to the preceeding cell.
    * <p>This method will advance structurally left (use the <em>leftLink</em>
    *    field) to the preceeding cell (SlipSublist or SlipDatum).
    *    This is a depth first search of a acyclic graph to find a data or
    *    sublist cell.</p>
    * <ol>
    *    <li>If the current cell is a sublist (SlipSublist) cell then:</li>
    *    <ol>
    *       <li>The sublist is 'entered'.</li>
    *       <li>A new SlipReaderCell is created representing the indentured
    *           list and linked to the existing SlipReaderCell.</li>
    *    </ol>
    *    <li>Advance left to the preceeding cell.</li>
    *    <li>If the current cell is a list header</li>
    *    <ol>
    *       <li>If this is the topmost list, we are done.</li>
    *       <li>Else return to the preceeding list and unlink and delete the
    *           current SlipReaderCell, and advance to the preceeding
    *           cell in the list.</li>
    *    </ol>
    *    <li>If the current cell is not a list header, we are done.</li>
    * </ol>
    * <p>Entry to a subordinate list is made if the  current list cell is
    *    a sublist cell. Exit from the current list is made if the current
    *    cell is a list header. Return from the method is done if a sublist
    *    cell (SlipSublist) or a data cell (SlipDatum) is found or the topmost
    *    list header is found.</p>
    * <p>The algorithm performs a depth first search for a sublist cell and
    *    returns the SlipReaderCell referencing the found sublist cell.</p>
    * <p>At exit, the current cell pointer is modified to point to a
    *    sublist cell or the topmost list header.</p>
    * @return (SlipREaderCell *) pointer to the current SlipReaderCell
    */
   SlipReaderCell* SlipReaderCell::advanceSWL() {                               // advance structural word right
      SlipReaderCell* reader = this;
      SlipCellBase*   link   = (SlipCellBase*)&reader->currentCell();
      if (reader->currentCell().isSublist()) {
         reader = downLevel(*reader, link);
         link   = (SlipCellBase*)&reader->currentCell();
      }
      link = link->getLeftLink();
      while(link->isHeader()) {
         if (reader->listDepth() == 0) {
            break;
         } else {
            reader = reader->upLevel();
            link   = (SlipCellBase*)&reader->currentCell();
            link = link->getLeftLink();
         }
      }
      reader->setCurrentCell(*link);
      return reader;
   }; // SlipReaderCell* SlipReaderCell::advanceSWL()

   /**
    * @brief Advance structurally right to the succeeding cell.
    * <p>This method will advance structurally right (use the <em>rightLink</em>
    *    field) to the succeeding cell (SlipSublist or SlipDatum).
    *    This is a depth first search of a acyclic graph to find a data or
    *    sublist cell.</p>
    * <ol>
    *    <li>If the current cell is a sublist (SlipSublist) cell then:</li>
    *    <ol>
    *       <li>The sublist is 'entered'.</li>
    *       <li>A new SlipReaderCell is created representing the indentured
    *           list and linked to the existing SlipReaderCell.</li>
    *    </ol>
    *    <li>Advance right to the succeeding cell.</li>
    *    <li>If the current cell is a list header</li>
    *    <ol>
    *       <li>If this is the topmost list, we are done.</li>
    *       <li>Else return to the preceeding list and unlink and delete the
    *           current SlipReaderCell, and advance to the succeeding
    *           cell in the list.</li>
    *    </ol>
    *    <li>If the current cell is not a list header, we are done.</li>
    * </ol>
    * <p>Entry to a subordinate list is made if the  current list cell is
    *    a sublist cell. Exit from the current list is made if the current
    *    cell is a list header. Return from the method is done if a sublist
    *    cell (SlipSublist) or a data cell (SlipDatum) is found or the topmost
    *    list header is found.</p>
    * <p>The algorithm performs a depth first search for a sublist cell and
    *    returns the SlipReaderCell referencing the found sublist cell.</p>
    * <p>At exit, the current cell pointer is modified to point to a
    *    sublist cell or the topmost list header.</p>
    * @return (SlipREaderCell *) pointer to the current SlipReaderCell
    */
   SlipReaderCell* SlipReaderCell::advanceSWR() {                               // advance structural word right
      SlipReaderCell* reader = this;
      SlipCellBase*   link   = (SlipCellBase*)&reader->currentCell();
      if (reader->currentCell().isSublist()) {
         reader = downLevel(*reader, link);
         link   = (SlipCellBase*)&reader->currentCell();
      }
      link = link->getRightLink();
      while(link->isHeader()) {
         if (reader->listDepth() == 0) {
            break;
         } else {
            reader = reader->upLevel();
            link   = (SlipCellBase*)&reader->currentCell();
            link = link->getRightLink();
         }
      }
      reader->setCurrentCell(*link);
      return reader;
   }; // SlipReaderCell* SlipReaderCell::advanceSWR()

               /*****************************************
                *   Header List Manipulation Methods    *
               ******************************************/

   /**
    * @brief Replace the top cell in a list with a sublist cell and fixup reader.
    * <p>Replace the top cell in the current list with a sublist cell
    *    referencing SlipHeader. If the current list is empty, issue a
    *    diagnostic message.</p>
    * <p>If the top cell is the current cell, fix the SlipReaderCell to point
    *    to the new top cell.</p>
    * <p>The input cell is unchanged.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replaceTop(SlipHeader&)
    * @param[in] X (SlipHeader&) replacement value, list to be referenced
    * @return reference to the new list top
    */
   SlipCell& SlipReaderCell::replaceTop(SlipHeader& X) {                        // Substitute list TOP value
      bool flag = (&(currentList().getTop()) == &currentCell());
      SlipCell* cell = &currentList().replaceTop(X);
      if (flag)
         setCurrentCell(*cell);
      return *cell;
   }; // SlipCell& SlipReaderCell::replaceTop(SlipHeader& X)

   /**
    * @brief Replace the top cell in a list with the input cell value and fixup reader.
    * <p>Replace the top cell in the current list with the input cell value. If
    *    the current list is empty, issue a diagnostic message.</p>
    * <p>If the top cell is the current cell, fix the SlipReaderCell to point
    *    to the new top cell.</p>
    * <p>The input cell is unchanged.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replaceTop(SlipCell&)
    * @param[in] X (SlipCell&) replacement value
    * @return reference to the new list top
    */
   SlipCell& SlipReaderCell::replaceTop(SlipCell& X) {                          // Substitute list TOP value
      bool flag = (&(currentList().getTop()) == &currentCell());
      SlipCell* cell = &currentList().replaceTop(X);
      if (flag)
         setCurrentCell(*cell);
      return *cell;
   }; // SlipCell& SlipReaderCell::replaceTop(SlipCell& X)

   /**
    * @brief Replace the bot cell in a list with a sublist cell and fixup reader.
    * <p>Replace the bot cell in the current list with a sublist cell
    *    referencing SlipHeader. If the current list is empty, issue a
    *    diagnostic message.</p>
    * <p>If the top cell is the current cell, fix the SlipReaderCell to point
    *    to the new top cell.</p>
    * <p>The input cell is unchanged.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replaceBot(SlipHeader&)
    * @param[in] X (SlipHeader&) replacement value, list to be referenced
    * @return reference to the new list top
    */
   SlipCell& SlipReaderCell::replaceBot(SlipHeader& X) {                        // Substitute list BOT value
      bool flag = (&(currentList().getBot()) == &currentCell());
      SlipCell* cell = &currentList().replaceBot(X);
      if (flag)
         setCurrentCell(*cell);
      return *cell;
   }; // SlipCell& SlipReaderCell::replaceBot(SlipHeader& X)

   /**
    * @brief Replace the bot cell in a list with the input cell value and fixup reader.
    * <p>Replace the bot cell in the current list with the value of the input
    *    cell. If the current list is empty, issue a diagnostic message.</p>
    * <p>If the top cell is the current cell, fix the SlipReaderCell to point
    *    to the new top cell.</p>
    * <p>The input cell is unchanged.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replaceBot(SlipCell&)
    * @param[in] X (SlipCell&) replacement value
    * @return reference to the new list top
    */
   SlipCell& SlipReaderCell::replaceBot(SlipCell& X) {                          // Substitute list BOT value
      bool flag = (&(currentList().getBot()) == &currentCell());
      SlipCell* cell = &currentList().replaceBot(X);
      if (flag)
         setCurrentCell(*cell);
      return *cell;
   }; // SlipCell& SlipReaderCell::replaceBot(SlipCell& X)

   /**
    * @brief Replace the top cell in a list with the input value and fixup reader.
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replaceTop(bool)
    * @param[in] X (bool) replacement value
    * @return reference to the new list top
    */
   SlipCell& SlipReaderCell::replaceTop(bool X) {                               // Substitute list TOP value
      bool flag = (&(currentList().getTop()) == &currentCell());
      SlipCell* cell = &currentList().replaceTop(X);
      if (flag)
         setCurrentCell(*cell);
      return *cell;
   }; // SlipCell& SlipReaderCell::replaceTop(bool X)

   /**
    * @brief Replace the top cell in a list with the input value and fixup reader.
    * <p>If the top cell is the current cell, fix the SlipReaderCell to point
    *    to the new top cell.</p>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replaceTop(UCHAR)
    * @param[in] X (UCHAR) replacement value
    * @return reference to the new list top
    */
   SlipCell& SlipReaderCell::replaceTop(UCHAR X) {                              // Substitute list TOP value
      bool flag = (&(currentList().getTop()) == &currentCell());
      SlipCell* cell = &currentList().replaceTop(X);
      if (flag)
         setCurrentCell(*cell);
      return *cell;
   }; // SlipCell& SlipReaderCell::replaceTop(UCHAR X)

   /**
    * @brief Replace the top cell in a list with the input value and fixup reader.
    * <p>If the top cell is the current cell, fix the SlipReaderCell to point
    *    to the new top cell.</p>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replaceTop(CHAR)
    * @param[in] X (CHAR) replacement value
    * @return reference to the new list top
    */
   SlipCell& SlipReaderCell::replaceTop(CHAR X) {                               // Substitute list TOP value
      bool flag = (&(currentList().getTop()) == &currentCell());
      SlipCell* cell = &currentList().replaceTop(X);
      if (flag)
         setCurrentCell(*cell);
      return *cell;
   }; // SlipCell& SlipReaderCell::replaceTop(CHAR X)

   /**
    * @brief Replace the top cell in a list with the input value and fixup reader.
    * <p>If the top cell is the current cell, fix the SlipReaderCell to point
    *    to the new top cell.</p>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replaceTop(ULONG)
    * @param[in] X (ULONG) replacement value
    * @return reference to the new list top
    */
   SlipCell& SlipReaderCell::replaceTop(ULONG X) {                              // Substitute list TOP value
      bool flag = (&(currentList().getTop()) == &currentCell());
      SlipCell* cell = &currentList().replaceTop(X);
      if (flag)
         setCurrentCell(*cell);
      return *cell;
   }; // SlipCell& SlipReaderCell::replaceTop(ULONG X)

   /**
    * @brief Replace the top cell in a list with the input value and fixup reader.
    * <p>If the top cell is the current cell, fix the SlipReaderCell to point
    *    to the new top cell.</p>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replaceTop(LONG)
    * @param[in] X (LONG) replacement value
    * @return reference to the new list top
    */
   SlipCell& SlipReaderCell::replaceTop(LONG X) {                               // Substitute list TOP value
      bool flag = (&(currentList().getTop()) == &currentCell());
      SlipCell* cell = &currentList().replaceTop(X);
      if (flag)
         setCurrentCell(*cell);
      return *cell;
   }; // SlipCell& SlipReaderCell::replaceTop(LONG X)

   /**
    * @brief Replace the top cell in a list with the input value and fixup reader.
    * <p>If the top cell is the current cell, fix the SlipReaderCell to point
    *    to the new top cell.</p>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replaceTop(DOUBLE)
    * @param[in] X (DOUBLE) replacement value
    * @return reference to the new list top
    */
   SlipCell& SlipReaderCell::replaceTop(DOUBLE X) {                             // Substitute list TOP value
      bool flag = (&(currentList().getTop()) == &currentCell());
      SlipCell* cell = &currentList().replaceTop(X);
      if (flag)
         setCurrentCell(*cell);
      return *cell;
   }; // SlipCell& SlipReaderCell::replaceTop(DOUBLE X)

   /**
    * @brief Replace the top cell in a list with the input value and fixup reader.
    * <p>If the top cell is the current cell, fix the SlipReaderCell to point
    *    to the new top cell.</p>
    * <p>The operations field provides a means to specify user defined operations.
    *    The pointer must be to a SlipOp defined class. The standard casting,
    *    unary, arithmetic, bitwise and logical operations must all be overridden.
    *    The result is that a user Defined object is treated as a atomic value
    *    in all computations.</p>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replaceTop(PTR, void*)
    * @param[in] X (PTR) replacement value
    * @param[in] operation (void*) SlipOp derived User Defined operations
    * @return reference to the new list top
    */
   SlipCell& SlipReaderCell::replaceTop(const PTR X, const void* operation) {   // Substitute list TOP value
      bool flag = (&(currentList().getTop()) == &currentCell());
      SlipCell* cell = &currentList().replaceTop(X, operation);
      if (flag)
         setCurrentCell(*cell);
      return *cell;
   }; // SlipCell& SlipReaderCell::replaceTop(const PTR X, const void* operation)

   /**
    * @brief Replace the top cell in a list with the input value and fixup reader.
    * <p>If the top cell is the current cell, fix the SlipReaderCell to point
    *    to the new top cell.</p>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replaceTop(string&, bool)
    * @param[in] X (string&) replacement value
    * @param[in] constFlag <b>true</b> string and object referenced not copied
    * @return reference to the new list top
    */
   SlipCell& SlipReaderCell::replaceTop(const string& X, bool constFlag) {      // Substitute list TOP value
      bool flag = (&(currentList().getTop()) == &currentCell());
      SlipCell* cell = &currentList().replaceTop(X, constFlag);
      if (flag)
         setCurrentCell(*cell);
      return *cell;
   }; // SlipCell& SlipReaderCell::replaceTop(const string& X, bool constFlag)

   /**
    * @brief Replace the top cell in a list with the input value and fixup reader.
    * <p>If the top cell is the current cell, fix the SlipReaderCell to point
    *    to the new top cell.</p>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replaceTop(string*, bool)
    * @param[in] X (string*) replacement value
    * @param[in] constFlag <b>true</b> string and object referenced not copied
    * @return reference to the new list top
    */
   SlipCell& SlipReaderCell::replaceTop(const string* X, bool constFlag) {      // Substitute list TOP value
      bool flag = (&(currentList().getTop()) == &currentCell());
      SlipCell* cell = &currentList().replaceTop(X, constFlag);
      if (flag)
         setCurrentCell(*cell);
      return *cell;
   }; // SlipCell& SlipReaderCell::replaceTop(const string* X, bool constFlag)


   /**
    * @brief Replace the bot cell in a list with the input value and fixup reader.
    * <p>If the bot cell is the current cell, fix the SlipReaderCell to point
    *    to the new bot cell.</p>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replaceBot(bool)
    * @param[in] X (bool) replacement value
    * @return reference to the new list top
    */
   SlipCell& SlipReaderCell::replaceBot(bool X) {                               // Substitute list BOT value
      bool flag = (&(currentList().getBot()) == &currentCell());
      SlipCell* cell = &currentList().replaceBot(X);
      if (flag)
         setCurrentCell(*cell);
      return *cell;
   }; // SlipCell& SlipReaderCell::replaceBot(bool X)

   /**
    * @brief Replace the bot cell in a list with the input value and fixup reader.
    * <p>If the bot cell is the current cell, fix the SlipReaderCell to point
    *    to the new bot cell.</p>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replaceBot(UCHAR)
    * @param[in] X (UCHAR) replacement value
    * @return reference to the new list top
    */
   SlipCell& SlipReaderCell::replaceBot(UCHAR X) {                              // Substitute list BOT value
      bool flag = (&(currentList().getBot()) == &currentCell());
      SlipCell* cell = &currentList().replaceBot(X);
      if (flag)
         setCurrentCell(*cell);
      return *cell;
   }; // SlipCell& SlipReaderCell::replaceBot(UCHAR X)

   /**
    * @brief Replace the bot cell in a list with the input value and fixup reader.
    * <p>If the bot cell is the current cell, fix the SlipReaderCell to point
    *    to the new bot cell.</p>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replaceBot(CHAR)
    * @param[in] X (CHAR) replacement value
    * @return reference to the new list top
    */
   SlipCell& SlipReaderCell::replaceBot(CHAR X) {                               // Substitute list BOT value
      bool flag = (&(currentList().getBot()) == &currentCell());
      SlipCell* cell = &currentList().replaceBot(X);
      if (flag)
         setCurrentCell(*cell);
      return *cell;
   }; // SlipCell& SlipReaderCell::replaceBot(CHAR X)

   /**
    * @brief Replace the bot cell in a list with the input value and fixup reader.
    * <p>If the bot cell is the current cell, fix the SlipReaderCell to point
    *    to the new bot cell.</p>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replaceBot(ULONG)
    * @param[in] X (ULONG) replacement value
    * @return reference to the new list top
    */
   SlipCell& SlipReaderCell::replaceBot(ULONG X) {                              // Substitute list BOT value
      bool flag = (&(currentList().getBot()) == &currentCell());
      SlipCell* cell = &currentList().replaceBot(X);
      if (flag)
         setCurrentCell(*cell);
      return *cell;
   }; // SlipCell& SlipReaderCell::replaceBot(ULONG X)

   /**
    * @brief Replace the bot cell in a list with the input value and fixup reader.
    * <p>If the bot cell is the current cell, fix the SlipReaderCell to point
    *    to the new bot cell.</p>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replaceBot(LONG)
    * @param[in] X (LONG) replacement value
    * @return reference to the new list top
    */
   SlipCell& SlipReaderCell::replaceBot(LONG X) {                               // Substitute list BOT value
      bool flag = (&(currentList().getBot()) == &currentCell());
      SlipCell* cell = &currentList().replaceBot(X);
      if (flag)
         setCurrentCell(*cell);
      return *cell;
   }; // SlipCell& SlipReaderCell::replaceBot(LONG X)

   /**
    * @brief Replace the bot cell in a list with the input value and fixup reader.
    * <p>If the bot cell is the current cell, fix the SlipReaderCell to point
    *    to the new bot cell.</p>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replaceBot(DOUBLE)
    * @param[in] X (DOUBLE) replacement value
    * @return reference to the new list top
    */
   SlipCell& SlipReaderCell::replaceBot(DOUBLE X) {                             // Substitute list BOT value
      bool flag = (&(currentList().getBot()) == &currentCell());
      SlipCell* cell = &currentList().replaceBot(X);
      if (flag)
         setCurrentCell(*cell);
      return *cell;
   }; // SlipCell& SlipReaderCell::replaceBot(DOUBLE X)

   /**
    * @brief Replace the bot cell in a list with the input value and fixup reader.
    * <p>If the bot cell is the current cell, fix the SlipReaderCell to point
    *    to the new bot cell.</p>
    * <p>The operations field provides a means to specify user defined operations.
    *    The pointer must be to a SlipOp defined class. The standard casting,
    *    unary, arithmetic, bitwise and logical operations must all be overridden.
    *    The result is that a user Defined object is treated as a atomic value
    *    in all computations.</p>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replaceBot(PTR, void*)
    * @param[in] X (PTR) replacement value
    * @param[in] operation (void*) SlipOp derived User Defined operations
    * @return reference to the new list top
    */
   SlipCell& SlipReaderCell::replaceBot(const PTR X, const void* operation) {   // Substitute list BOT value
      bool flag = (&(currentList().getBot()) == &currentCell());
      SlipCell* cell = &currentList().replaceBot(X, operation);
      if (flag)
         setCurrentCell(*cell);
      return *cell;
   }; // SlipCell& SlipReaderCell::replaceBot(const PTR X, const void* operation)

   /**
    * @brief Replace the bot cell in a list with the input value and fixup reader.
    * <p>If the bot cell is the current cell, fix the SlipReaderCell to point
    *    to the new bot cell.</p>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replaceBot(string&, bool)
    * @param[in] X (string&) replacement value
    * @param[in] constFlag <b>true</b> string and object referenced not copied
    * @return reference to the new list top
    */
   SlipCell& SlipReaderCell::replaceBot(const string& X, bool constFlag) {      // Substitute list BOT value
      bool flag = (&(currentList().getBot()) == &currentCell());
      SlipCell* cell = &currentList().replaceBot(X, constFlag);
      if (flag)
         setCurrentCell(*cell);
      return *cell;
   }; // SlipCell& SlipReaderCell::replaceBot(const string& X, bool constFlag)

   /**
    * @brief Replace the bot cell in a list with the input value and fixup reader.
    * <p>If the bot cell is the current cell, fix the SlipReaderCell to point
    *    to the new bot cell.</p>
    * <p>If the current list is empty, issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replaceBot(string*, bool)
    * @param[in] X (string*) replacement value
    * @param[in] constFlag <b>true</b> string and object referenced not copied
    * @return reference to the new list top
    */
   SlipCell& SlipReaderCell::replaceBot(const string* X, bool constFlag) {      // Substitute list BOT value
      bool flag = (&(currentList().getBot()) == &currentCell());
      SlipCell* cell = &currentList().replaceBot(X, constFlag);
      if (flag)
         setCurrentCell(*cell);
      return *cell;
   }; // SlipCell& SlipReaderCell::replaceBot(const string* X, bool constFlag)

            /*****************************************
             *           Manipulating Lists          *
            ******************************************/

   /**
    * @fn SlipReaderCell::replace(const SlipCell& X)
    * @brief Replace the current cell with the input value.
    * <p>The SlipReaderCell will point to the new cell.</p>
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @sa SlipDatum::replace(SlipCell&)
    * @sa SlipHeader::replace(SlipCell&)
    * @sa SlipSublist::replace(SlipCell&)
    * @param[in] X (SlipCell&) replacement value
    * @return reference to the new list top
    */

   /**
    * @fn SlipReaderCell::replace(bool X)
    * @brief Replace the current cell with the input value.
    * <p>The SlipReaderCell will point to the new cell.</p>
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @sa SlipDatum::replace(bool)
    * @sa SlipHeader::replace(bool)
    * @sa SlipSublist::replace(bool)
    * @param[in] X (bool) replacement value
    * @return reference to the new list top
    */

   /**
    * @fn SlipReaderCell::replace(CHAR X)
    * @brief Replace the current cell with the input value.
    * <p>The SlipReaderCell will point to the new cell.</p>
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @sa SlipDatum::replace(CHAR)
    * @sa SlipHeader::replace(CHAR)
    * @sa SlipSublist::replace(CHAR)
    * @param[in] X (CHAR) replacement value
    * @return reference to the new list top
    */

   /**
    * @fn SlipReaderCell::replace(UCHAR X)
    * @brief Replace the current cell with the input value.
    * <p>The SlipReaderCell will point to the new cell.</p>
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @sa SlipDatum::replace(UCHAR)
    * @sa SlipHeader::replace(UCHAR)
    * @sa SlipSublist::replace(UCHAR)
    * @param[in] X (UCHAR) replacement value
    * @return reference to the new list top
    */

   /**
    * @fn SlipReaderCell::replace(LONG X)
    * @brief Replace the current cell with the input value.
    * <p>The SlipReaderCell will point to the new cell.</p>
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @sa SlipDatum::replace(LONG)
    * @sa SlipHeader::replace(LONG)
    * @sa SlipSublist::replace(LONG)
    * @param[in] X (LONG) replacement value
    * @return reference to the new list top
    */

   /**
    * @fn SlipReaderCell::replace(ULONG X)
    * @brief Replace the current cell with the input value.
    * <p>The SlipReaderCell will point to the new cell.</p>
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @sa SlipDatum::replace(ULONG)
    * @sa SlipHeader::replace(ULONG)
    * @sa SlipSublist::replace(ULONG)
    * @param[in] X (ULONG) replacement value
    * @return reference to the new list top
    */

   /**
    * @fn SlipReaderCell::replace(DOUBLE X)
    * @brief Replace the current cell with the input value.
    * <p>The SlipReaderCell will point to the new cell.</p>
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @sa SlipDatum::replace(DOUBLE)
    * @sa SlipHeader::replace(DOUBLE)
    * @sa SlipSublist::replace(DOUBLE)
    * @param[in] X (DOUBLE) replacement value
    * @return reference to the new list top
    */

   /**
    * @fn SlipReaderCell::replace(const PTR X, const void* operation)
    * @brief Replace the current cell with the input value.
    * <p>The SlipReaderCell will point to the new cell.</p>
    * <p>The operations field provides a means to specify user defined operations.
    *    The pointer must be to a SlipOp defined class. The standard casting,
    *    unary, arithmetic, bitwise and logical operations must all be overridden.
    *    The result is that a user Defined object is treated as a atomic value
    *    in all computations.</p>
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @sa SlipDatum::replace(PTR, void*)
    * @sa SlipHeader::replace(PTR, void*)
    * @sa SlipSublist::replace(PTR, void*)
    * @param[in] X (PTR) replacement value
    * @param[in] operation (void*) SlipOp derived User Defined operations
    * @return reference to the new list top
    */

   /**
    * @fn SlipReaderCell::replace(const string& X, bool constFlag)
    * @brief Replace the current cell with the input value.
    * <p>The SlipReaderCell will point to the new cell.</p>
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @sa SlipDatum::replace(string&, bool)
    * @sa SlipHeader::replace(string&, bool)
    * @sa SlipSublist::replace(string&, bool)
    * @param[in] X (string&) replacement value
    * @param[in] constFlag <b>true</b> string and object referenced not copied
    * @return reference to the new list top
    */

   /**
    * @fn SlipReaderCell::replace(const string* X, bool constFlag)
    * @brief Replace the current cell with the input value.
    * <p>The SlipReaderCell will point to the new cell.</p>
    * <p>If the current list is empty or the current cell is the current list,
    *    issue a diagnostic message.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @sa SlipDatum::replace(string*, bool)
    * @sa SlipHeader::replace(string*, bool)
    * @sa SlipSublist::replace(string*, bool)
    * @param[in] X (string*) replacement value
    * @param[in] constFlag <b>true</b> string and object referenced not copied
    * @return reference to the new list top
    */

            /*****************************************
             *     Miscellaneous Reader Methods      *
            ******************************************/

   /**
    * @brief Create a copy of the current reader chain.
    * @return (SlipReaderCell*) pointer to the reader chain copy
    */
   SlipReaderCell* SlipReaderCell::copy() {                                     // Return a copy of the reader list
      SlipReaderCell* oldReader = this;
      SlipReaderCell* newReader = new SlipReaderCell(oldReader->currentList());
      SlipReaderCell* prvReader;
      SlipReaderCell* reader    = newReader;
      setCurrentCell(*newReader, oldReader->currentCell());
      while(oldReader->listDepth() != 0) {
         setListDepth(*newReader, oldReader->listDepth());
         prvReader = newReader;
         oldReader = oldReader->getLink();
         newReader = new SlipReaderCell(oldReader->currentList());
         setLink(*prvReader, *newReader);
         setCurrentCell(*newReader, oldReader->currentCell());
      }
      return reader;
   }; // SlipReader& copy()

   /**
    * @fn SlipReaderCell::currentCell()cons
    * @brief Return a reference to the current list cell.
    * @return (SlipCell&) reference to the current list cell
    */

   /**
    * @brief Return a reference to the current list.
    * @return (SlipHeader&) reference to the current list
    */
   SlipHeader&       SlipReaderCell::currentList()const { return **(SlipHeader**)getReaderTop();}// Return current SLIP list header being traversed

   /**
    * @fn SlipReaderCell::listDepth()
    * @brief Return reader depth field value.
    * @return (USHORT) reader depth
    */

   /**
    * !@brief Initialize an instance of a SlipReaderCell.
    * <p>Set the operator pointer, the chain link (<b>null</b>), and the
    *    list depth (0).</p>
    */
   inline
   void SlipReaderCell::init() {
        setOperator((void*)readerOP);
        setRightLink(NULL);
        setListDepth(0);
   }; // void SlipReaderCell::init()

   /**
    * @brief Delete the current list cell.
    * <p>The current list cell is deleted and the current cell is set to
    *    the cell preceeding the deleted cell.</p>
    * <p>The current list header can not be deleted.</p>
    * @return (SilpReaderCell*) pointer to the current SlipReaderCell
    */
   SlipReaderCell* SlipReaderCell::deleteCell() {                               // Delete the current cell and move left
      SlipCell* cell    = &currentCell();
      advanceLWL();
      cell->unLink();
      delete cell;
      return this;
   }; // SlipCell& SlipReaderCell::deleteCell()

   /**
    * @brief Recurse into an indentured list.
    * <p>Create a new SlipReaderCell and chain it to the current SlipREaderCell.
    *    The new SlipReaderCell fields will have:</p>
    * <ul>
    *    <li><b>operator</b> points to SlipReaderOp.</li>
    *    <li><b>previous reader</b> chains to the current reader.</li>
    *    <li><b>current list</b> points to the header in the sublist.</li>
    *    <li><b>current cell</b> points to the header in the sublist.</li>
    *    <li><b>depth</b> one more than the current reader.</li>
    * </ul>
    * @param[in,out] reader (SlipReaderCell&)
    * @param[in] link (SlipCellBase*) pointer to the current sublist cell
    * @return (SlipReaderCell*) pointer to the new SlipReaderCell
    */
   SlipReaderCell* SlipReaderCell::downLevel(SlipReaderCell& reader, SlipCellBase* link) {
      USHORT depth = reader.listDepth() + 1;
      SlipHeader*     header     = *(SlipHeader**)getSublistHeader(*link);
      SlipReaderCell* newReader  = new SlipReaderCell(*header);
      setLink(*newReader, reader);
      setListDepth(*newReader, depth);
      return newReader;
   }; // SlipReaderCell downLevel(SlipReaderCell* reader)

   /**
    * @fn SlipReaderCell::reset()
    * @brief Set the reader current cell to the current list.
    * @return (SlipReader*) current reader
    */

   /**
    * @brief Return to the topmost list.
    * <p>Unchain and delete all readers up to but not including the topmost
    *    reader. At the end, the topmost reader will be current and the
    *    reader current cell will point to the sublist which caused chaining
    *    to indentured lists. No change is made tothe topmost reader.</p>
    * @return (SlipReaderCell*) topmost reader
    */
   SlipReaderCell* SlipReaderCell::resetTop() {                                 // Return to start of list descent sublist (return to top)
      SlipReaderCell* nxtReader = this;
      SlipReaderCell* curReader = getLink();
      if ( curReader != (SlipReaderCell*)NULL) {
         if (listDepth() > 0) {
            do {
               curReader = nxtReader;
               nxtReader = getLink(*curReader);
            } while(listDepth() > 0);
            addAVSLCells(this, curReader);
         }
      }
      return nxtReader;
   } // SlipReader& SlipReaderCell::resetTop()

   /**
    * @brief Setup a SlipReaderCell to an initial state.\
    * <p>Initializes the current list pointer and the current cell pointer
    *    in the SlipReaderCell to point to the input list header.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2031</b>  Slip Reader must be created with a SlipHeader list cell</li>
    *    <li><b>E2032</b>  Slip Reader can not be on the stack but must be taken from AVLS</li>
    *    <li><b>E2033</b>  INTERNAL ERROR new SlipReader cell from AVLS is marked UNDEFINED</li>
    * </ul>
    * @param[in] header (SlipCell*) pointer
    */
   void SlipReaderCell::setup(const SlipCell* header) {
      init();
      if (!header->isHeader()) {
         postError(__FILE__, __LINE__, SlipErr::E2031, "SlipReaderCell::setup", "", "", *header);
      } else if (isTemp()) {
         postError(__FILE__, __LINE__, SlipErr::E2032, "SlipReaderCell::setup", "", "", *header);
      } else if (header->getLeftLink() == UNDEFDATA) {
         postError(__FILE__, __LINE__, SlipErr::E3026, "SlipSequncer::SlipSequencer", "", "", *header);
      } else if (getRightLink() == NULL) {
         SlipHeader& castHeader = const_cast<SlipHeader&>(*(SlipHeader*)header);
         setCurrentCell(castHeader);
         setListHeader(castHeader);
     } else {
         postError(__FILE__, __LINE__, SlipErr::E2033, "SlipReaderCell::setup", "", "", *header);
     }
   }; // void SlipReaderCell::setup(SlipHeader* header)

   /**
    * @brief Return to the superior list.
    * <p>The current reader is deleted and the preceeding reader on the
    *    chain is returned. If this is the topmost reader then nothing
    *    is done.</p>
    * @return (SlipReaderCell*) pointer to reader of superior list
    */
   SlipReaderCell* SlipReaderCell::upLevel() {                                  // Return to previous level
     SlipReaderCell* reader = this;
     if (listDepth() > 0) {
        reader = getLink(*this);
        addAVSLCells(this, this);
     }
     return reader;
  } // SlipReader& SlipReaderCell::upLevel() {

  /**
   * @brief Return a pretty-printed ASCII version of a SlipReaderCell
   * <p>The contents of a SlipReaderCell are output as:</p>
   * <tt><pre>
   *   Depth&lt;#&gt; &lt;chain&gt; currentCell currentList
   * </pre></tt>
   * @return (string) pretty-printed SlipReaderCell
   */
   string SlipReaderCell::toString() const {
     stringstream pretty;
     pretty << getName() << " Depth<" << *getReaderDepth()
                         << "> <" << hex << setfill('0') << setw(8) << getRightLink()
                         << "> "  << currentCell().toString()
                         << "  "  << currentList().toString();
     return pretty.str();
  }; // string SlipReaderCell::toString() const

  /**
   * @brief Provide an exact representation of the SlipReaderCell as a string.
   * @return (string) uglified SlipReaderCell
   */
  string SlipReaderCell::dump() const {
     return ((SlipOp*)*getOperator())->dump((SlipDatum&)*this);
  }; // string SlipReaderCell::dump() const
}  // namespace slip
