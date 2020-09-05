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
 * @file    SlipHeader.cpp
 * @author  A. Schwarz
 * @date    May 29, 2012
 *
 * @brief Contains the SlipHeader definitions.
*/

# include <fstream>
# include <iostream>
# include <cmath>
# include <ostream>
# include <sstream>
# include <iomanip>
# include <iostream>
# include <string>
# include "SlipCellBase.h"
# include "SlipDef.h"
# include "SlipGlobal.h"
# include "SlipErr.h"
# include "SlipHeader.h"
# include "SlipOp.h"
# include "SlipReader.h"
# include "SlipSequencer.h"
# include "SlipSublist.h"
# include "SlipWrite.h"

using namespace std;

namespace slip {

   const int    HEADERMIN =  20;                                       // Minimum columns on existing line for new sublist
   const int    NORMALMIN =  11;                                       // Minimum columns on existing line for entry
   const USHORT VISITMASK = 0x8000;                                    // List visit mask

   /**
    * @brief Construct a header
    * <p>Fail if the created object is either owned by the AVSL or has been
    *    created on the stack. Otherwise fill in the header fields to represent
    *    an empty list. The field values are:</p>
    * <ul>
    *    <li><b>leftLink</b> points to the object address.</li>
    *    <li><b>rightLink</b> points to the object address.</li>
    *    <li><b>operator</b> points to the header invariant properties object.</li>
    *    <li><b>Descriptor Link</b> no Descriptor List present</li>
    *    <li><b>V</b>-bit list not visited</li>
    *    <li><b>refCnt</b> '0' by default, '1' if the user sets the refCnt flag
    *         true.</li>
    *    <li><b>mark</b> set to zero.</li>
    * </ul>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E3011</b> Illegal to create a Header cell on the stack. Must use new.</li>
    * </ul>
    * @param refCnt <b>true</b> make the SlipHeader object permanent
    */
   void SlipHeader::constructHeader(bool refCnt) {
     if ((getLeftLink() == getRightLink()) &&
        ((getLeftLink() == (SlipCellBase*)UNDEFDATA) ||
         (getLeftLink() == (SlipCellBase*)TEMPORARY))) {
        postError(__FILE__, __LINE__, SlipErr::E3011, "SlipHeader::constructHeader", "", "", *this);
     } else {
       setLeftLink(this);
       setRightLink(this);
       setOperator((void*)headOP);
       putMark(0);
       *(getHeadRefCnt()) = (refCnt)? 1: 0;
     }
   }; // void SlipHeader::constructHeader(ULONG refCnt)

   /**
    * @brief Return the SlipHeader object and list to the AVSL.
    * <p>If the header refCont is zero (0) then return the header and the list
    *    to the AVSL, otherwise decrement the refCnt and do nothing.</p>
    * <p>Returning the list header and list cells to the AVSL makes them
    *    immediately available but not necessarily immediately used. The header
    *    and list cells retain their identity until used. If the user retains
    *    a reference or pointer to the header or to any list cells the reference
    *    or pointer may appear to be valid. It is the users responsibility to
    *    ensure that all references to the list are invalidated after the list
    *    is deleted.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2010</b> Can not use header cell. Header left link and right link are null.</li>
    * </ul>
    */
   void SlipHeader::deleteList() {
      if (isDeleted()) return;
      if (*(getHeadRefCnt()) < 1) {
         if (isUnlinked()) {
            postError(__FILE__, __LINE__, SlipErr::E2010, "SlipHeader::deleteHeader", "", "Header left link and right link are null.", *this);
         } else {
            if (isDList()) getDList().deleteList();
            addAVSLCells( this              // pointer to first cell
                        , getLeftLink());  // pointer to last cell
         }
      } else
         *(getHeadRefCnt()) -= 1;
   }; // void SlipHeader::deleteHeader()

   /**
    * @brief Create a copy of the current list.
    * <p>A new SlipHeader object is created and points to a copy of the original
    *    list.</p>
    * @return reference to a created SlipHeader object
    */
   SlipCell& SlipHeader::factory() {                                   // Create a SlipCell
         return *(new SlipHeader(this));
      }; // SlipCell& SlipHeader::factory()

   /**
    * @fn SlipHeader::SlipHeader(SlipHeader* header)
    * @brief Create a new SlipHeader object and copy the original list.
    * <p>A new object is created and contains  a copy of the original list,
    *    the same user mark (refmark) and a copy of the descriptor list..</p>
    * @param header pointer to an existing SlipHeader object
    */

   /**
    * @fn SlipHeader::SlipHeader(bool refCnt)
    * @brief Create an empty SlipHeader object.
    * <p>Setting the refCnt flag to <b>true</b> implies that the user will
    *    delete the list separately from normal operations. The list is
    *    permanent until explicitely deleted.</p>
    * <p>Setting the refCnt flag to <b>true</b> does not invalidate the
    *    requirement that the user delete the list (deleteList()) in the
    *    same scope as created.</p>
    * <p>For example</p>
    * <tt>
    *    <pre>
    *    {
    *       SlipHeader * head1() = new SlipHeader();
    *       SlipHeader * head2() = new SlipHeader(true);
    *       head1-&gt;deleteList();             // list returned to AVSL
    *       head2-&gt;deleteList();             // list not returned to AVSL
    *       head2-&gt;deleteList();             // list returned to AVSL
    *    }
    *    </pre>
    * </tt>
    * <p>And the head1 and first head2 deleteList() calls are required. The
    *    second head2-&gt;deleteList() deletes head2 and returns the object and
    *    the list to the AVSL. The first head2->deleteList() only decrements
    *    the refCnt.</p>
    * @param refCnt <b>true</b> set the refCnt to '1'
    */

   /**
    * @brief <b>delete header</b> is illegal.
    * <p>A <b>delete header</b> is trapped by the SlipHeader destructor. When
    *    the trap is triggered, an diagnostic message is output.</p>
    * <p>Using <b>delte header</b> instead of <b>header.deleteList()</b>
    *    causes the list identity to be immediately erased. If there are any
    *    sublist references to this list they will not work. The list header
    *    and the list cells are not put into the AVSL, they are never available.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E3024</b> Delete header is illegal. header.deleteList() must be used.</li>
    * </ul>
    */
   SlipHeader::~SlipHeader() {                                         // ~SlipHeader() destructor
      postError(__FILE__, __LINE__, SlipErr::E3024, "SlipHeader::~SlipHeader", "", "", *this);
   }; // SlipHeader::~SlipHeader()


            /*****************************************
             *            Descriptor Lists           *
            ******************************************/

   /**
    * @brief Append an input list to a descriptor list
    * <p>If no descriptor list exists then on is created. A deep copy is
    *    performed, that is, the SlipDatum objects of the current input
    *    descriptor list is appended to the current descriptor list. If
    *    the input descriptor list has a descriptor list then this is
    *    appended to the descriptor list of the current header descriptor
    *    list, and a new descriptor list is created if required. This
    *    continues until there are no more descriptor lists in the input
    *    chain.</p>
    * <p>This can be seen in the following diagram:</p>
    * <pre><tt>
    *    Current List
    *       \
    *        \
    *         o Descriptor list append    input descriptor list
    *          \                             \
    *           \                             \
    *            o Descriptor list append      o descriptor list
    * </tt></pre>
    * <p>The user mark field of the header is unchanged.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E3025</b>  Input descriptor list has too few entries.</li>
    * </ul>
    * @note The use of insertRightLink instead of enqueue is a performance
    *       issue. It is assumed that all entries in the descriptor list
    *       are check for validity when the object is placed on the list
    *       and that no further checking is required.
    * @note Checks for descriptor list validity (that there are <key, value>
    *       pairs in the input, are made only on the input. It is assumed that
    *       the entries on the descriptor chain have been validated.
    * @param dList (SlipHeader&) list to be appended
    * @return reference to the input descriptor list
    */
   SlipHeader& SlipHeader::appendDList(SlipHeader& dList) {            // append a description list
      if (!&dList) return dList;
      if ((dList.size() % 2) != 0) {
         postError(__FILE__, __LINE__, SlipErr::E3025, "SlipHeader::~SlipHeader", "", "", dList);
      } else {
         SlipHeader* header = this;
         SlipHeader* myDList = &dList;
         while (myDList) {
            if (!header->isDList())  header->create_dList();
            SlipHeader* descList = (SlipHeader*)*getHeadDescList(*header);
            SlipCell* link = (SlipCell*)myDList->getLeftLink();
            while(!(link->isHeader())) {
               descList->insertRight(copy(*link));
               link = (SlipCell*)link->getLeftLink();
            }
            header = descList;
            myDList = (SlipHeader*)*getHeadDescList(*myDList);
         }
      }
      return dList;
   }; // SlipHeader& SlipHeader::appendDList(SlipHeader& dList)



   /**
    * @brief Returns <b>true</b> if the value is in the Descriptor List.
    * <p>The Descriptor List values are searched for any occurrence of value. the
    *    first such occurrence stops the search and <b>true</b> is returned.</p>
    * <p>Search for equality uses "operator==' as defined for each object.
    *    Search does not attempt to determine what the SlipCell is or is not.
    *    It can be any of the SlipCell derived classes, except SlipHeader because
    *    SlipHeader can not be a member of any list. If you search for equality
    *    on a SlipHeader object then the search will be successful if any
    *    sublist in the Descriptor List points to the header.</p>
    * <p>Search does not care if the value is a object on the stack, in a list
    *    or not in a list. Search continues without making this determination.<p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2038</b> Header required. Missing Descriptor List.</li>
    * </ul>
    * @param[in] value (SlipCell) search value
    * @return <b>true</b> if the value is found
    */
   bool SlipHeader::contains(SlipCell& value) {                        // returns true if the value is in the list
      SlipHeader*   dList = *(SlipHeader**)getHeadDescList();
      bool flag = false;
      if (dList == NULL) {
         postError(__FILE__, __LINE__, SlipErr::E2038, "SlipHeader::contains", "Missing Descriptor List. ", "", *this);
      } else {
         SlipCellBase* link     = dList->getRightLink();
         for (; !link->isHeader(); link = link->getRightLink()) {
            link = link->getRightLink();
            SlipCell& cell = *(SlipCell*)link;
            flag  = (cell == value);
            if (flag) break;
         }
      }
      return flag;
   }; // bool SlipHeader::contains(SlipCell& value)

   /**
    * @brief Returns <b>true</b> if the key is found.
    * <p>The Descriptor List keys are searched for the occurrence of the  input
    *    key. The only such occurrence stops the search and <b>true</b> is
    *    returned.</p>
    * <p>Search for equality uses "operator==' as defined for each object.
    *    Search does not attempt to determine what the SlipCell is or is not.
    *    It can be any of the SlipCell derived classes, except SlipHeader because
    *    SlipHeader can not be a member of any list. If you search for equality
    *    on a SlipHeader object then the search will be successful if any
    *    sublist in the Descriptor List points to the header.</p>
    * <p>Search does not care if the value is a object on the stack, in a list
    *    or not in a list. Search continues without making this determination.<p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2038</b> Header required. Missing Descriptor List.</li>
    * </ul>
    * @param[in] key (SlipCell&) object containing the search key
    * @return <b>true</b> if the key is found
    */
   bool SlipHeader::containsKey(SlipCell& key) {                       // returns true if the key is in the list
      SlipHeader*   dList = *(SlipHeader**)getHeadDescList();
      bool flag = false;
      if (dList == NULL) {
         postError(__FILE__, __LINE__, SlipErr::E2038, "SlipHeader::containsKey", "Missing Descriptor List. ", "", *this);
      } else {
         SlipCellBase* link     = dList->getRightLink();
         for (; !link->isHeader(); link = link->getRightLink()) {
            SlipCell& cell = *(SlipCell*)link;
            flag  = (cell == key);
            if (flag) break;
            link = link->getRightLink();
         }
      }
      return flag;
   }; // SlipHeader::constainsKey(SlipCell& key)

   /**
    * @brief Create a Description List for the current headaer.
    * <p>If there is no Description List, one is created, otherwise a
    *    diagnostic message is issued and there is not creation.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2041</b> Header exists.  Can not create a Descriptor list.</li>
    * </ul>
    */
   SlipHeader& SlipHeader::create_dList() {                            // create/change a descriptor list for this header
      SlipHeader* dList = *(SlipHeader**)getHeadDescList();
      if (dList != NULL) {
         postError(__FILE__, __LINE__, SlipErr::E2041, "SlipHeader::create_dList", " ", "Can not create a Descriptor list.", *this);
      } else {
         dList = new SlipHeader();
         *(SlipHeader**)getHeadDescList() = dList;
      }
      return *dList;
   }; // SlipHeader& SlipHeader::create_dList()

   /**
    * @brief Delete the key and value from the Description List.p]
    *
    * <p>The Descriptor List keys are searched for any occurrence of input key.
    *    The only such occurrence stops the search, The key and value are deleted
    *    from the Description List, and <b>true</b> is returned.</p>
    * <p>Search for equality uses "operator==' as defined for each object.
    *    Search does not attempt to determine what the SlipCell is or is not.
    *    It can be any of the SlipCell derived classes, except SlipHeader because
    *    SlipHeader can not be a member of any list. If you search for equality
    *    on a SlipHeader object then the search will be successful if any
    *    sublist in the Descriptor List points to the header.</p>
    * <p>Search does not care if the value is a object on the stack, in a list
    *    or not in a list. Search continues without making this determination.<p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2038</b> Header required. Missing Descriptor List.</li>
    * </ul>
    * @param[in] key (SlipCell&) object containing the search key
    */
   bool SlipHeader::deleteAttribute(SlipCell& key) {                   // delete attribute and value from list
      bool flag = false;
      SlipHeader* dList = *(SlipHeader**)getHeadDescList();
      if (dList == NULL) {
         postError(__FILE__, __LINE__, SlipErr::E2038, "SlipHeader::deleteAttribute", "Missing Descriptor List. ", "", *this);
      } else {
         if (dList->isEmpty()) return flag;
         SlipCell* link = (SlipCell*)dList->getRightLink();
         while(!(link->isHeader()) && (*link != key)) {
            link = (SlipCell*)(link->getRightLink())->getRightLink();
         }
         if ((!link->isHeader())) {
            flag = true;
            SlipCell& value = ((SlipCell*)link->getRightLink())->unLink();
            SlipCell& dCell = link->unLink();
            delete &value;
            delete &dCell;
         }
      }
      return flag;
   }; // void SlipHeader::deleteAttribute(SlipCell& cell)

   /**
    * @brief The Description List is deleted and all cells returned to the AVSL.
    * <p>If there is a Description List then it is deleted (dList->deleteList()).
    * </p>
    * <p>If there is no Description List then the request is ignored.</p>
    * <p>At termination the Description List field of the object is set to
    *    <b>null</b>.</p>
    * @return reference to the current object (<b>*this</b>)
    */
   SlipHeader& SlipHeader::delete_dList() {                            // delete the descriptor list
      SlipHeader* dList = *(SlipHeader**)getHeadDescList();
      if (dList != NULL) dList->deleteList();
      *(SlipHeader**)getHeadDescList() = NULL;
      return *this;
   }; // SlipHeader& SlipHeader::delete_dList()

   /**
    * @brief Uglify the Description List.
    * <p>dump() is called on the description list. Description List brackets are
    *    output with the dump (&lt; dump &gt;)</p>
    * @see dump()
    */
   void SlipHeader::dumpDList() {                                      // dump the descriptor list
      cout << "<";
      (*(SlipHeader**)getHeadDescList())->dumpList();
      cout << ">" << endl;
   }; // void SlipHeader::dumpDList()

   /**
    * @brief The Description List is flushed.
    * <p>On return the Descriptor List cells are return to the AVSL and the
    *    Description List is empty. The header will return <b>true</b> if
    *    headaer.isDList() is executed.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2038</b> Header required. Missing Descriptor List.</li>
    * </ul>
    * @return reference to the header (<b>*this</b>)
    */
   SlipHeader& SlipHeader::flush_dList() {                              // delete contents of a descriptor list
       SlipHeader* dList = *(SlipHeader**)getHeadDescList();
       if (dList == NULL) {
          postError(__FILE__, __LINE__, SlipErr::E2038, "SlipHeader::flush_dList", "Missing Descriptor List. ", "", *this);
       } else
          dList->flush();
       return *this;
    }; // SlipHeader& SlipHeader::flush_dList()

   /**
    * @brief Return the value associated with the key from the Description List
    * <p>The Descriptor List keys are searched for any occurrence of input key.
    *    The only such occurrence stops the search, If the search is successful
    *    a reference to the associated value is returned. If the search is
    *    unsuccessful then a reference to the input key is returned.</p>
    * <p>Search for equality uses "operator==' as defined for each object.
    *    Search does not attempt to determine what the SlipCell is or is not.
    *    It can be any of the SlipCell derived classes, except SlipHeader because
    *    SlipHeader can not be a member of any list. If you search for equality
    *    on a SlipHeader object then the search will be successful if any
    *    sublist in the Descriptor List points to the header.</p>
    * <p>Search does not care if the value is a object on the stack, in a list
    *    or not in a list. Search continues without making this determination.<p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2038</b> Header required. Missing Descriptor List.</li>
    * </ul>
    * @param[in] key (SlipCell&) search key
    * @return reference to the associated value or reference to the key
    */
   SlipCell& SlipHeader::get(SlipCell& key) {                          // return value of an attribute
      SlipHeader* dList = *(SlipHeader**)getHeadDescList();
      if (dList == NULL) {
         postError(__FILE__, __LINE__, SlipErr::E2038, "SlipHeader::get", "Missing Descriptor List. ", "", *this);
      } else {
         SlipCellBase* link     = dList->getRightLink();
         for (; !link->isHeader(); link->getRightLink()) {
            if (*(SlipCell*)link == key) return *(SlipCell*)link->getRightLink();
            link->getRightLink();
         }
      }
      return key;
   }; // SlipCell& SlipHeader::get(SlipCell& cell)

   /**
    * @brief REturn a reference to the list Descriptor List.
    * <p>If there is no Descriptor List then the results are undefined,
    *    otherwise a reference to the list is returned.</p>
    * @return (SlipHeader&) a reference to the Descriptor List.
    */
   SlipHeader& SlipHeader::getDList() const {                                   // return a reference to the Descriptor List header
      return *(SlipHeader*)*getHeadDescList();
   }; // SlipHeader& SlipHeader::getDList() const

   /**
    * @brief Return an array containing pointers to the Description List keys.
    * <p>An array containing pointers to the keys is returned. If the Description
    *    List is empty, <b>null</b> is returned.</p>
    * <p>The array is taken from the heap. The user must delete the array after use.
    *    Heap holes may occur as a result of use of this method.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2038</b> Header required. Missing Descriptor List.</li>
    * </ul>
    * @return pointer to an array containing pointers to the Description List keys
    */
   SlipCell* SlipHeader::keys() {                                      // returns an array of keys in the descriptor list
      SlipHeader*   dList = *(SlipHeader**)getHeadDescList();
      if (dList == NULL) {
         postError(__FILE__, __LINE__, SlipErr::E2038, "SlipHeader::keys", "Missing Descriptor List. ", "", *this);
      } else {
         SlipCellBase* link     = dList->getRightLink();
         unsigned  count = size_dList();
         SlipCell* retKeys = (SlipCell*)new char(sizeof(SlipCell*) * count);
         link = dList->getRightLink();
         for (unsigned ndx = 0; count > 0; link = link->getRightLink()) {
            retKeys[ndx++] = (SlipCell*)link;
            link = link->getRightLink();
            count--;
         }
         return retKeys;
      }
      return NULL;
   }; // SlipCell* SlipHeader::keys()

   /**
    * @brief Insert a new $lt;key value&gt; pair into the Description List
    * @param[in] key (SlipCell&) to be inserted
    * @param[in] value (SlipCell&) value to be inserted
    * @return reference to the header
    */
   SlipCell& SlipHeader::put(SlipCell& key, SlipCell& value) {         // add attribute to description list
      SlipHeader* dList = *(SlipHeader**)getHeadDescList();
      if (dList == NULL) {
        postError(__FILE__, __LINE__, SlipErr::E2038, "SlipHeader::put", "Missing Descriptor List. ", "", *this);
      } else if (!containsKey(key)) {
         dList->push(value);
         dList->push(key);
      }
      return *this;
   }; // SlipCell& SlipHeader::put(SlipCell& key, SlipCell& value)

   /**
    * @brief Return the number of &lt;key value&gt; pairs
    * <p>Count the number of pairs in the Description List. This method isn't
    *    the fastest in the world, but it is the fastest that can be made.</p>
    * @return the number number of &lt;key value&gt; pairs
    */
   unsigned SlipHeader::size_dList() {                                 // number of <key, value> pairs
      SlipHeader*   dList = *(SlipHeader**)getHeadDescList();
      unsigned  count = 0;
     if (dList == NULL) {
         postError(__FILE__, __LINE__, SlipErr::E2038, "SlipHeader::size_dList", "Missing Descriptor List. ", "", *this);
      } else {
         SlipCellBase* link     = dList->getRightLink();
         for (; !link->isHeader(); link = link->getRightLink()) count++;
      }
      return count/2;
   }; // unsigned SlipHeader::size_dList()

            /*****************************************
             *           Manipulating Lists          *
            ******************************************/
   /**
    * @brief Remove and return a cell from the list bottom.
    * <p>If the list is not empty the cell on the list bottom is removed and
    *    a reference to the cell returned. If the list is empty an error
    *    message is posted and an exception raised. If exceptions are turned
    *    off, a reference to the list header is returned.</p>
    * @note The user is responsible for deleting the removed header when done.
    *       The following code will remove and delete at the same time unless
    *       the list is empty:
    * <tt>
    *    <pre>
    *       delete &header.dequeue();
    *    </pre>
    * </tt>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2008</b> dequeue of empty list not allowed. </li>
    * </ul>
    * @return reference to the dequeued or the SlipHeader object
    */
   SlipCell& SlipHeader::dequeue() {                                   // Remove and return list bottom
      SlipCellBase* cell = this;
      if (isEmpty()) {
         postError(__FILE__, __LINE__, SlipErr::E2008, "SlipHeader::dequeue", "", "", *this);
      } else {
         cell  = getLeftLink();
         setRightLink(cell->getLeftLink(), cell->getRightLink());
         setLeftLink(cell->getRightLink(), cell->getLeftLink());
         setLeftLink(cell, NULL);
         setRightLink(cell, NULL);
      }
      return (SlipCell&)*cell;
   }; // SlipCell& SlipHeader::dequeue()

   /**
    * @brief Insert a SlipCell to the bottom of the list.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then checks are mad to ensure that
    *    the input cell is valid. If both are valid then the operation is
    *    legitimate and the following will be done:</p>
    * <ul>
    *    <li>If the input cell is a sublist and linked then a SlipSublist will
    *        be created and inserted. The reference count of the referenced
    *        list header will be incremented. Both lists share a reference to
    *        the same list.</li>
    *    <li>If the input cell is a SlipHeader then a sublist will be created
    *        and inserted. The reference count of the referenced list header
    *        will be incremented. Both lists share a reference to the same
    *        list.</li>
    *    <li>If the input cell is linked onto a list, then a copy will be
    *        created and inserted.</li>
    *    <li>If the input cell is on the stack, then a copy will be created
    *        and inserted.</li>
    *    <li>Otherwise the input cell is inserted.</li>
    * </ul>
    * @sa SlipCell::insLeft(SlipCell&)
    * @param[in] X SlipCell object reference
    * @return reference to the SlipHeader
    */
   SlipHeader& SlipHeader::enqueue(SlipCell& X) { insLeft(X); return *this;}    // Insert X on bottom of list

   /**
    * @brief Insert a bool object to the bottom of the list.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa SlipCell::insLeft(bool)
    * @param[in] X (bool) object to be inserted
    * @return reference to the SlipHeader
    */
   SlipHeader& SlipHeader::enqueue(bool X)      { insLeft(X); return *this;}    // Insert X on bottom of list

   /**
    * @brief Insert a UCHAR object to the bottom of the list.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa SlipCell::insLeft(UCHAR)
    * @param[in] X (UCHAR) object to be inserted
    * @return reference to the SlipHeader
    */
   SlipHeader& SlipHeader::enqueue(UCHAR X)     { insLeft(X); return *this;}    // Insert X on bottom of list

   /**
    * @brief Insert a CHAR object to the bottom of the list.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa SlipCell::insLeft(CHAR)
    * @param[in] X (CHAR) object to be inserted
    * @return reference to the SlipHeader
    */
   SlipHeader& SlipHeader::enqueue(CHAR X)      { insLeft(X); return *this;}    // Insert X on bottom of list

   /**
    * @brief Insert a ULONG object to the bottom of the list.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa SlipCell::insLeft(ULONG)
    * @param[in] X (ULONG) object to be inserted
    * @return reference to the inserted object
    */
   SlipHeader& SlipHeader::enqueue(ULONG X)     { insLeft(X); return *this;}    // Insert X on bottom of list

   /**
    * @brief Insert a LONG object to the bottom of the list.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa SlipCell::insLeft(LONG)
    * @param[in] X (LONG) object to be inserted
    * @return reference to the SlipHeader
    */
   SlipHeader& SlipHeader::enqueue(LONG X)      { insLeft(X); return *this;}    // Insert X on bottom of list

   /**
    * @brief Insert a DOUBLE object to the bottom of the list.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa SlipCell::insLeft(DOUBLE)
    * @param[in] X (DOUBLE) object to be inserted
    * @return reference to the SlipHeader
    */
   SlipHeader& SlipHeader::enqueue(DOUBLE X)    { insLeft(X); return *this;}    // Insert X on bottom of list

   /**
    * @brief Insert a PTR object to the bottom of the list.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * <p>The operations field provides a means to specify user defined operations.
    *    The pointer must be to a SlipOp defined class. The standard casting,
    *    unary, arithmetic, bitwise and logical operations must all be overridden.
    *    The result is that a user Defined object is treated as a atomic value
    *    in all computations.</p>
    * @sa SlipCell::insLeft(PTR, void*)
    * @param[in] X (PTR) object to be inserted
    * @param[in] operation (void*) SlipOp derived User Defined operations
    * @return reference to the SlipHeader
    */
   SlipHeader& SlipHeader::enqueue(const PTR X, const void* operation) { // Insert X on bottom of list
      insLeft((PTR)X, operation); return *this;
   }; // SlipHeader::enqueue(const PTR X,const void* operation)

   /**
    * @brief Insert a string& object to the bottom of the list.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa SlipCell::insLeft(string&)
    * @param[in] X (string&) object to be inserted
    * @param[in] constFlag <b>true</b> string is a <b>const</b>, <b>false</b> otherwise
    * @return reference to the SlipHeader
    */
   SlipHeader& SlipHeader::enqueue(const string& X, bool constFlag) {  // Insert X on bottom of list
      insLeft(X, constFlag); return *this;
   }; // SlipHeader::enqueue(const string& X, bool constFlag)

   /**
    * @brief Insert a string* object to the bottom of the list.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa SlipCell::insLeft(string*)
    * @param[in] X (string*) object to be inserted
    * @param[in] constFlag <b>true</b> string is a <b>const</b>, <b>false</b> otherwise
    * @return reference to the SlipHeader
    */
   SlipHeader& SlipHeader::enqueue(const string* X, bool constFlag) {  // Insert X on bottom of list
      insLeft(X, constFlag); return *this;
   }; // SlipHeader::enqueue(const string* X, bool constFlag)

   /**
    * @brief Empty the list.
    * <p>After execution the list is empty (<em>isEmpty()</em> is <b>true.</b>.
    *    All the cells are returned to the AVSL. The list leftLink and rightLink
    *    point to the SlipHeader object.</p>
    * @return reference to the SlipHeader object
    */
   SlipCell& SlipHeader::flush() {                                     // Flush list of contents
      if (!isEmpty()) {
         addAVSLCells(&getTop(), &getBot());                           // delete cells
         setLeftLink(this);
         setRightLink(this);
      }
       return *this;
   }; // SlipCell& SlipHeader::flush()

   /**
    * @brief Peek at the list bottom cell.
    * <p>If the list is empty then a reference to the list header is returned.</p>
    * @return reference to the list bottom cell.
    */
   SlipCell& SlipHeader::getBot() const { return (SlipCell&)*getLeftLink(); }   // Return pointer to list bottom

   /**
    * @brief Peek at the list top cell.
    * <p>If the list is empty then a reference to the list header is returned.</p>
    * @return reference to the list bottom cell.
    */
   SlipCell& SlipHeader::getTop() const { return (SlipCell&)*getRightLink();}   // Return pointer to list top

   /**
    * @brief Returns <b>true</b> if the list 'visit' bit is set.
    * @param[in] X SlipHeader object
    * @return <b>true</b> if the list 'visit' bit is set
    */
   bool SlipHeader::isVisited(const SlipHeader& X) {                   // Has the list been visited
      return ((*(X.getHeadRefCnt()) & VISITMASK) != 0);
   }; // bool SlipHeader::isVisited()

   /**
    * @fn SlipHeader::leave(SlipHeader* X)
    * @brief Unmark list
    * <p>For recursive list traversal a list header is marked on entry and
    *    unmarked on exit. When the mark is seen during the traversal, the
    *    list is not entered.</p>
    * @param[in] X (SlipHeader* X) list being exited
    */
   void SlipHeader::leave(SlipHeader* X) {
      *(X->getHeadRefCnt()) &= ~VISITMASK;
   }; // void SlipHeader::leave()

   /**
    * @brief Remove the list top and return it's reference.
    * <p>If the list is not empty the cell on the list bottom is removed and
    *    a reference to the cell returned. If the list is empty an error
    *    message is posted and an exception raised. If exceptions are turned
    *    off, a reference to the list header is returned.</p>
    * @note The user is responsible for deleting the removed header when done.
    *       The following code will remove and delete at the same time unless
    *       the list is empty:
    * <tt>
    *    <pre>
    *       delete &header.dequeue();
    *    </pre>
    * </tt>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2009</b> pop of empty list not allowed. </li>
    * </ul>
    * @return reference to the SlipHeader
    */
   SlipCell& SlipHeader::pop() {                                       // Remove and return list top
      SlipCellBase* cell = this;
      if (isEmpty()) {
         postError(__FILE__, __LINE__, SlipErr::E2009, "SlipHeader::pop", "", "", *this);
      } else {
         cell  = getRightLink();
         setLeftLink(cell->getRightLink(), cell->getLeftLink());
         setRightLink(cell->getLeftLink(), cell->getRightLink());
         setLeftLink(cell, NULL);
         setRightLink(cell, NULL);
      }
      return (SlipCell&)*cell;
   }; // SlipCell& SlipHeader::pop()

   /**
    * @brief Insert a SlipCell to the top of the list.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then checks are mad to ensure that
    *    the input cell is valid. If both are valid then the operation is
    *    legitimate and the following will be done:</p>
    * <ul>
    *    <li>If the input cell is a SlipHeader object then a SlipSublist will
    *        be created and inserted. The reference count of the referenced
    *        list header will be incremented. </li>
    *    <li>If the input cell is a SlipHeader object and unlinked then the
    *        sublist will be inserted.</li>
    *    <li>If the input cell is linked onto a list, then a copy will be
    *        created and inserted.</li>
    *    <li>If the input cell is on the stack, then a copy will be created
    *        and inserted.</li>
    *    <li>Otherwise the input cell is inserted.</li>
    * </ul>
    * @sa SlipCell::insRight(SlipCell&)
    * @param[in] X SlipCell object reference
    * @return reference to the SlipHeader
    */
   SlipHeader& SlipHeader::push(SlipCell& X) { insRight(X); return *this; }     // Insert X on top of list

   /**
    * @brief Insert a bool object to the top of the list.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa SlipCell::insRight(bool)
    * @param[in] X (bool) object to be inserted
    * @return reference to the SlipHeader
    */
   SlipHeader& SlipHeader::push(bool X)      { insRight(X); return *this; }     // Insert X on top of list

   /**
    * @brief Insert a UCHAR object to the top of the list.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa SlipCell::insRight(UCHAR)
    * @param[in] X (UCHAR) object to be inserted
    * @return reference to the SlipHeader
    */
   SlipHeader& SlipHeader::push(UCHAR X)     { insRight(X); return *this; }     // Insert X on top of list

   /**
    * @brief Insert a CHAR object to the top of the list.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa SlipCell::insRight(CHAR)
    * @param[in] X (CHAR) object to be inserted
    * @return reference to the SlipHeader
    */
   SlipHeader& SlipHeader::push(CHAR X)      { insRight(X); return *this; }     // Insert X on top of list

   /**
    * @brief Insert a ULONG object to the top of the list.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa SlipCell::insRight(ULONG)
    * @param[in] X (ULONG) object to be inserted
    * @return reference to the SlipHeader
    */
   SlipHeader& SlipHeader::push(ULONG X)     { insRight(X); return *this; }     // Insert X on top of list

   /**
    * @brief Insert a LONG object to the top of the list.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa SlipCell::insRight(LONG)
    * @param[in] X (LONG) object to be inserted
    * @return reference to the SlipHeader
    */
   SlipHeader& SlipHeader::push(LONG X)      { insRight(X); return *this; }     // Insert X on top of list

   /**
    * @brief Insert a DOUBLE object to the top of the list.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * @sa SlipCell::insRight(DOUBLE)
    * @param[in] X (DOUBLE) object to be inserted
    * @return reference to the SlipHeader
    */
   SlipHeader& SlipHeader::push(DOUBLE X)    { insRight(X); return *this; }     // Insert X on top of list

   /**
    * @brief Insert a PTR object to the top of the list.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    * <p>The operations field provides a means to specify user defined operations.
    *    The pointer must be to a SlipOp defined class. The standard casting,
    *    unary, arithmetic, bitwise and logical operations must all be overridden.
    *    The result is that a user Defined object is treated as a atomic value
    *    in all computations.</p>
    * @sa SlipCell::insRight(PTR, void*)
    * @param[in] X (PTR) object to be inserted
    * @param[in] operation (void*) SlipOp derived User Defined operations
    * @return reference to the SlipHeader
    */
   SlipHeader& SlipHeader::push(const PTR X, const void* operation) {           // Insert X on top of list
      insRight(X, operation); return *this;
   }; // SlipHeader::push(const PTR X, const void* operation)

   /**
    * @brief Insert a string& object to the top of the list.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    *
    * @sa SlipCell::insRight(string&)
    * @param[in] X (string&) object to be inserted
    * @param[in] constFlag <b>true</b> string is a <b>const</b>, <b>false</b> otherwise
    * @return reference to the SlipHeader
    */
   SlipHeader& SlipHeader::push(const string& X, bool constFlag) {              // Insert X on top of list
      insRight(X, constFlag); return *this;
   }; // SlipHeader::push(const string& X, bool constFlag)

   /**
    * @brief Insert a string* object to the top of the list.
    * <p>Checks are made to ensure the the current object (<b>this</b>) is a valid
    *    cell. If the current object is valid then a new SlipDatum object is
    *    created and inserted.</p>
    *
    * @sa SlipCell::insRight(string*)
    * @param[in] X (string*) object to be inserted
    * @param[in] constFlag <b>true</b> string is a <b>const</b>, <b>false</b> otherwise
    * @return reference to the SlipHeader
    */
   SlipHeader& SlipHeader::push(const string* X, bool constFlag) {              // Insert X on top of list
      insRight(*X, constFlag); return *this;
   }; // SlipHeader::push(const string* X, bool constFlag)

   /**
    * @brief Replacement of a SlipHeader object is not supported.
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param[in] X (SlipCell&) replacement object
    * @return reference to the existing list header
    */
   SlipCell& SlipHeader::replace(const SlipCell& X) {                  // Replace a cell on a list with a new cell
      postError(__FILE__, __LINE__, SlipErr::E2015, "SlipHeader::replace", "", "", *this, X);
      return *this;
   }; // SlipCell& SlipHeader::replace(SlipCell& X)

   /**
    * @brief Replacement of a SlipHeader object is not supported.
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param[in] X (bool) replacement object
    * @return reference to the existing list header
    */
   SlipCell& SlipHeader::replace(bool X) {                             // Replace a cell on a list with a new cell
      postError(__FILE__, __LINE__, SlipErr::E2015, "SlipHeader::replace", "", "", *this);
      return *this;
   }; // SlipCell& SlipHeader::replace(bool X)

   /**
    * @brief Replacement of a SlipHeader object is not supported.
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param[in] X (CHAR) replacement object
    * @return reference to the existing list header
    */
   SlipCell& SlipHeader::replace(CHAR X) {                             // Replace a cell on a list with a new cell
      postError(__FILE__, __LINE__, SlipErr::E2015, "SlipHeader::replace", "", "", *this);
      return *this;
   }; // SlipCell& SlipHeader::replace(CHAR X)

   /**
    * @brief Replacement of a SlipHeader object is not supported.
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param[in] X (UCHAR) replacement object
    * @return reference to the existing list header
    */
   SlipCell& SlipHeader::replace(UCHAR X) {                            // Replace a cell on a list with a new cell
      postError(__FILE__, __LINE__, SlipErr::E2015, "SlipHeader::replace", "", "", *this);
      return *this;
   }; // SlipCell& SlipHeader::replace(UCHAR X)

   /**
    * @brief Replacement of a SlipHeader object is not supported.
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param[in] X (LONG) replacement object
    * @return reference to the existing list header
    */
   SlipCell& SlipHeader::replace(LONG X) {                             // Replace a cell on a list with a new cell
      postError(__FILE__, __LINE__, SlipErr::E2015, "SlipHeader::replace", "", "", *this);
      return *this;
   }; // SlipCell& SlipHeader::replace(LONG X)

   /**
    * @brief Replacement of a SlipHeader object is not supported.
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param[in] X (ULONG) replacement object
    * @return reference to the existing list header
    */
   SlipCell& SlipHeader::replace(ULONG X) {                            // Replace a cell on a list with a new cell
      postError(__FILE__, __LINE__, SlipErr::E2015, "SlipHeader::replace", "", "", *this);
      return *this;
   }; // SlipCell& SlipHeader::replace(ULONG X)

   /**
    * @brief Replacement of a SlipHeader object is not supported.
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param[in] X (DOUBLE) replacement object
    * @return reference to the existing list header
    */
   SlipCell& SlipHeader::replace(DOUBLE X) {                           // Replace a cell on a list with a new cell
      postError(__FILE__, __LINE__, SlipErr::E2015, "SlipHeader::replace", "", "", *this);
      return *this;
   }; // SlipCell& SlipHeader::replace(DOUBLE X)

   /**
    * @brief Replacement of a SlipHeader object is not supported.
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param[in] X (PTR) replacement object
    * @param[in] operation (void*) SlipOp derived User Defined operations
    * @return reference to the existing list header
    */
   SlipCell& SlipHeader::replace(PTR X, const void* operation) {       // Replace a cell on a list with a new cell
      postError(__FILE__, __LINE__, SlipErr::E2015, "SlipHeader::replace", "", "", *this);
      return *this;
   }; // SlipCell& SlipHeader::replace(PTR X, const void* operation)

   /**
    *
    * @brief Replacement of a SlipHeader object is not supported.
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param[in] X (string&) replacement object
    * @param[in] constFlag determines copy and delete characteristics of the string
    * @return reference to the existing list header
    */
   SlipCell& SlipHeader::replace(const string& X, bool constFlag) {    // Replace a cell on a list with a new cell
      postError(__FILE__, __LINE__, SlipErr::E2015, "SlipHeader::replace", "", "", *this);
      return *this;
   }; // SlipCell& SlipHeader::replace(string& X)

   /**
    * @brief Replacement of a SlipHeader object is not supported.
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2015</b>Header not allowed in replace method</li>
    * </ul>
    * @param[in] X (string*) replacement object
    * @param[in] constFlag determines copy and delete characteristics of the string
    * @return reference to the existing list header
    */
   SlipCell& SlipHeader::replace(const string* X, bool constFlag) {    // Replace a cell on a list with a new cell
      postError(__FILE__, __LINE__, SlipErr::E2015, "SlipHeader::replace", "", "", *this);
      return *this;
   }; // SlipCell& SlipHeader::replace(string* X, bool constFlag)

   /**
    * @brief Replace the list bottom object with a sublist.
    * <p>If the list is not empty, the bottom object is replaced. Otherwise,
    *    the request is ignored.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replace(SlipHeader&)
    * @param[in] X right hand side (RHS) replacement SlipHeader reference
    * @return reference to the current list header
    */
   SlipCell& SlipHeader::replaceBot(SlipHeader& X) {                   // Substitute list TOP value
      if (isEmpty())
         postError(__FILE__, __LINE__, SlipErr::E2012, "SlipHeader::replaceBot", "", "", *this);
      else ((SlipCell*)getLeftLink())->replace(X);
      return *this;
   }; // SlipCell& SlipHeader::replaceBot(SlipHeader& X)

   /**
    * @brief Replace the list bottom object in the list.
    * <p>If the list is not empty, the bottom object is replaced. Otherwise,
    *    the request is ignored.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replace(SlipCell&)
    * @param[in] X right hand side (RHS) replacement object
    * @return reference to the list header
    */
   SlipCell& SlipHeader::replaceBot(SlipCell& X) {                     // Substitute list TOP value
      if (isEmpty())
         postError(__FILE__, __LINE__, SlipErr::E2012, "SlipHeader::replaceBot", "", "", *this);
      else ((SlipCell*)getLeftLink())->replace(X);
      return *this;
   }; // SlipCell& SlipHeader::replaceBot(SlipCell* X)

   /**
    * @brief Replace the list bottom object with a bool value.
    * <p>If the list is not empty, the bottom object is replaced. Otherwise,
    *    the request is ignored.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replace(bool)
    * @param[in] X (bool) right hand side (RHS) replacement value
    * @return reference to the list header
    */
   SlipCell& SlipHeader::replaceBot(bool X) {                          // Substitute list TOP value
      if (isEmpty())
         postError(__FILE__, __LINE__, SlipErr::E2012, "SlipHeader::replaceBot", "", "", *this);
      else ((SlipCell*)getLeftLink())->replace(X);
      return *this;
   }; // SlipCell& SlipHeader::replaceBot(bool X)

   /**
    * @brief Replace the list bottom object with a UCHAR value.
    * <p>If the list is not empty, the bottom object is replaced. Otherwise,
    *    the request is ignored.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replace(UCHAR)
    * @param[in] X (UCHAR)right hand side (RHS) replacement value
    * @return reference to the list header
    */
   SlipCell& SlipHeader::replaceBot(UCHAR X) {                         // Substitute list TOP value
      if (isEmpty())
         postError(__FILE__, __LINE__, SlipErr::E2012, "SlipHeader::replaceBot", "", "", *this);
      else ((SlipCell*)getLeftLink())->replace(X);
      return *this;
   }; // SlipCell& SlipHeader::replaceBot(UCHAR X

   /**
    * @brief Replace the list bottom object with a CHAR value.
    * <p>If the list is not empty, the bottom object is replaced. Otherwise,
    *    the request is ignored.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replace(CHAR)
    * @param[in] X (CHAR) right hand side (RHS) replacement value
    * @return reference to the list header
    */
   SlipCell& SlipHeader::replaceBot(CHAR X) {                          // Substitute list TOP value
      if (isEmpty())
         postError(__FILE__, __LINE__, SlipErr::E2012, "SlipHeader::replaceBot", "", "", *this);
      else ((SlipCell*)getLeftLink())->replace(X);
      return *this;
   }; // SlipCell& SlipHeader::replaceBot(CHAR X)

   /**
    * @brief Replace the list bottom object with a ULONG value.
    * <p>If the list is not empty, the bottom object is replaced. Otherwise,
    *    the request is ignored.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replace(ULONG)
    * @param[in] X (ULONG) right hand side (RHS) replacement value
    * @return reference to the list header
    */
   SlipCell& SlipHeader::replaceBot(ULONG X) {                         // Substitute list TOP value
      if (isEmpty())
         postError(__FILE__, __LINE__, SlipErr::E2012, "SlipHeader::replaceBot", "", "", *this);
      else ((SlipCell*)getLeftLink())->replace(X);
      return *this;
   }; // SlipCell& SlipHeader::replaceBot(ULONG X)

   /**
    * @brief Replace the list bottom object with a LONG value.
    * <p>If the list is not empty, the bottom object is replaced. Otherwise,
    *    the request is ignored.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replace(LONG)
    * @param[in] X (LONG) right hand side (RHS) replacement value
    * @return reference to the list header
    */
   SlipCell& SlipHeader::replaceBot(LONG X) {                          // Substitute list TOP value
      if (isEmpty())
         postError(__FILE__, __LINE__, SlipErr::E2012, "SlipHeader::replaceBot", "", "", *this);
      else ((SlipCell*)getLeftLink())->replace(X);
      return *this;
   }; // SlipCell& SlipHeader::replaceBot(LONG X)

   /**
    * @brief Replace the list bottom object with a DOUBLE value.
    * <p>If the list is not empty, the bottom object is replaced. Otherwise,
    *    the request is ignored.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replace(DOUBLE)
    * @param[in] X (DOUBLE) right hand side (RHS) replacement value
    * @return reference to the list header
    */
   SlipCell& SlipHeader::replaceBot(DOUBLE X) {                        // Substitute list TOP value
      if (isEmpty())
         postError(__FILE__, __LINE__, SlipErr::E2012, "SlipHeader::replaceBot", "", "", *this);
      else ((SlipCell*)getLeftLink())->replace(X);
      return *this;
   }; // SlipCell& SlipHeader::replaceBot(DOUBLE X)

   /**
    * @brief Replace the list bottom object with a PTR value.
    * <p>If the list is not empty, the bottom object is replaced. Otherwise,
    *    the request is ignored.</p>
    * <p>The operations field provides a means to specify user defined operations.
    *    The pointer must be to a SlipOp defined class. The standard casting,
    *    unary, arithmetic, bitwise and logical operations must all be overridden.
    *    The result is that a user Defined object is treated as a atomic value
    *    in all computations.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replace(PTR, void*)
    * @param[in] X (PTR) object to be inserted
    * @param[in] operation (void*) SlipOp derived User Defined operations
    * @return reference to the inserted object
    */
   SlipCell& SlipHeader::replaceBot(PTR X, const void* operation) {    // Substitute list TOP value
      if (isEmpty())
         postError(__FILE__, __LINE__, SlipErr::E2012, "SlipHeader::replaceBot", "", "", *this);
      else ((SlipCell*)getLeftLink())->replace(X, operation);
      return *this;
   }; // SlipCell& SlipHeader::replaceBot(PTR X, const void* operation)

   /**
    * @brief Replace the list bottom object with a string& value.
    * <p>If the list is not empty, the bottom object is replaced. Otherwise,
    *    the request is ignored.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replace(string&, bool)
    * @param[in] X (string&) right hand side (RHS) replacement value
    * @param[in] constFlag <b>true</b> string and object referenced not copied
    * @return reference to the list header
    */
   SlipCell& SlipHeader::replaceBot(const string& X, bool constFlag) { // Substitute list TOP value
      if (isEmpty())
         postError(__FILE__, __LINE__, SlipErr::E2012, "SlipHeader::replaceBot", "", "", *this);
      else ((SlipCell*)getLeftLink())->replace(X, constFlag);
      return *this;
   }; // SlipCell& SlipHeader::replaceBot(const string& X, bool constFlag)

   /**
    * @brief Replace the list bottom object with a string* value.
    * <p>If the list is not empty, the bottom object is replaced. Otherwise,
    *    the request is ignored.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replace(string*, bool)
    * @param[in] X (string*) right hand side (RHS) replacement value
    * @param[in] constFlag <b>true</b> string and object referenced not copied
    * @return reference to the list header
    */
   SlipCell& SlipHeader::replaceBot(const string* X, bool constFlag) { // Substitute list TOP value
      if (isEmpty())
         postError(__FILE__, __LINE__, SlipErr::E2012, "SlipHeader::replaceBot", "", "", *this);
      else ((SlipCell*)getLeftLink())->replace(X, constFlag);
      return *this;
   }; // SlipCell& SlipHeader::replaceBot(const string* X, bool constFlag)

   /**
    * @brief Replace the list top object with a sublist object.
    * <p>If the list is not empty, the bottom object is replaced. Otherwise,
    *    the request is ignored.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replace(SlipHeader&)
    * @param[in] X (SlipHeader&) right hand side (RHS) replacement SlipHeader reference
    * @return reference to the current list header
    */
   SlipCell& SlipHeader::replaceTop(SlipHeader& X) {                   // Substitute list TOP value
      if (isEmpty())
         postError(__FILE__, __LINE__, SlipErr::E2012, "SlipHeader::replaceBot", "", "", *this);
      else ((SlipCell*)getRightLink())->replace(X);
      return *this;
   }; // SlipCell& SlipHeader::replaceTop(SlipHeader& X)

   /**
    * @brief Replace the list top object with a SlipCell object.
    * <p>If the list is not empty, the bottom object is replaced. Otherwise,
    *    the request is ignored.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replace(SlipCell&)
    * @param[in] X (SlipCell&) right hand side (RHS) replacement SlipHeader reference
    * @return reference to the current list header
    */
   SlipCell& SlipHeader::replaceTop(SlipCell& X) {                     // Substitute list TOP value
      if (isEmpty())
         postError(__FILE__, __LINE__, SlipErr::E2012, "SlipHeader::replaceBot", "", "", *this);
      else ((SlipCell*)getRightLink())->replace(X);
      return *this;
   }; // SlipCell& SlipHeader::replaceTop(SlipCell* X)

   /**
    * @brief Replace the list top object with a bool value.
    * <p>If the list is not empty, the bottom object is replaced. Otherwise,
    *    the request is ignored.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replace(bool)
    * @param[in] X (bool) right hand side (RHS) replacement SlipHeader reference
    * @return reference to the current list header
    */
   SlipCell& SlipHeader::replaceTop(bool X) {                          // Substitute list TOP value
      if (isEmpty())
         postError(__FILE__, __LINE__, SlipErr::E2012, "SlipHeader::replaceBot", "", "", *this);
      else ((SlipCell*)getRightLink())->replace(X);
      return *this;
   }; // SlipCell& SlipHeader::replaceTop(bool X)

   /**
    * @brief Replace the list top object with a UCHAR value.
    * <p>If the list is not empty, the bottom object is replaced. Otherwise,
    *    the request is ignored.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replace(UCHAR)
    * @param[in] X (UCHAR) right hand side (RHS) replacement SlipHeader reference
    * @return reference to the current list header
    */
   SlipCell& SlipHeader::replaceTop(UCHAR X) {                         // Substitute list TOP value
      if (isEmpty())
         postError(__FILE__, __LINE__, SlipErr::E2012, "SlipHeader::replaceBot", "", "", *this);
      else ((SlipCell*)getRightLink())->replace(X);
      return *this;
   }; // SlipCell& SlipHeader::replaceTop(UCHAR X)

   /**
    * @brief Replace the list top object with a CHAR value.
    * <p>If the list is not empty, the bottom object is replaced. Otherwise,
    *    the request is ignored.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replace(CHAR)
    * @param[in] X (CHAR) right hand side (RHS) replacement SlipHeader reference
    * @return reference to the current list header
    */
   SlipCell& SlipHeader::replaceTop(CHAR X) {                          // Substitute list TOP value
      if (isEmpty())
         postError(__FILE__, __LINE__, SlipErr::E2012, "SlipHeader::replaceBot", "", "", *this);
      else ((SlipCell*)getRightLink())->replace(X);
      return *this;
   }; // SlipCell& SlipHeader::replaceTop(CHAR X)

   /**
    * @brief Replace the list top object with a ULONG va;ue.
    * <p>If the list is not empty, the bottom object is replaced. Otherwise,
    *    the request is ignored.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replace(ULONG)
    * @param[in] X (ULONG) right hand side (RHS) replacement SlipHeader reference
    * @return reference to the current list header
    */
   SlipCell& SlipHeader::replaceTop(ULONG X) {                         // Substitute list TOP value
      if (isEmpty())
         postError(__FILE__, __LINE__, SlipErr::E2012, "SlipHeader::replaceBot", "", "", *this);
      else ((SlipCell*)getRightLink())->replace(X);
      return *this;
   }; // SlipCell& SlipHeader::replaceTop(ULONG X)

   /**
    * @brief Replace the list top object with a LONG value.
    * <p>If the list is not empty, the bottom object is replaced. Otherwise,
    *    the request is ignored.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replace(LONG)
    * @param[in] X (LONG) right hand side (RHS) replacement SlipHeader reference
    * @return reference to the current list header
    */
   SlipCell& SlipHeader::replaceTop(LONG X) {                         // Substitute list TOP value
      if (isEmpty())
         postError(__FILE__, __LINE__, SlipErr::E2012, "SlipHeader::replaceBot", "", "", *this);
      else ((SlipCell*)getRightLink())->replace(X);
      return *this;
   }; // SlipCell& SlipHeader::replaceTop(LONG X)

   /**
    * @brief Replace the list top object with a OOUBLE value.
    * <p>If the list is not empty, the bottom object is replaced. Otherwise,
    *    the request is ignored.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replace(DOUBLE)
    * @param[in] X (DOUBLE) right hand side (RHS) replacement SlipHeader reference
    * @return reference to the current list header
    */
   SlipCell& SlipHeader::replaceTop(DOUBLE X) {                       // Substitute list TOP value
      if (isEmpty())
         postError(__FILE__, __LINE__, SlipErr::E2012, "SlipHeader::replaceBot", "", "", *this);
      else ((SlipCell*)getRightLink())->replace(X);
      return *this;
   }; // SlipCell& SlipHeader::replaceTop(DOUBLE X)

   /**
    * @brief Replace the list top object with a PTR value.
    * <p>If the list is not empty, the bottom object is replaced. Otherwise,
    *    the request is ignored.</p>
    * <p>The operations field provides a means to specify user defined operations.
    *    The pointer must be to a SlipOp defined class. The standard casting,
    *    unary, arithmetic, bitwise and logical operations must all be overridden.
    *    The result is that a user Defined object is treated as a atomic value
    *    in all computations.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replace(PTR)
    * @param[in] X (PTR) object to be inserted
    * @param[in] operation (void*) SlipOp derived User Defined operations
    */
   SlipCell& SlipHeader::replaceTop(const PTR X, const void* operation) {       // Substitute list TOP value
      if (isEmpty())
         postError(__FILE__, __LINE__, SlipErr::E2012, "SlipHeader::replaceBot", "", "", *this);
      else ((SlipCell*)getRightLink())->replace(X, operation);
      return *this;
   }; // SlipCell& SlipHeader::replaceTop(PTR X, const void* operation)

   /**
    * @brief Replace the list top object with a string& value.
    * <p>If the list is not empty, the bottom object is replaced. Otherwise,
    *    the request is ignored.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replace(string&, bool)
    * @param[in] X (string&) right hand side (RHS) replacement SlipHeader reference
    * @param[in] constFlag <b>true</b> string and object referenced not copied
    * @return reference to the current list header
    */
   SlipCell& SlipHeader::replaceTop(const string& X, bool constFlag) { // Substitute list TOP value
      if (isEmpty())
         postError(__FILE__, __LINE__, SlipErr::E2012, "SlipHeader::replaceBot", "", "", *this);
      else ((SlipCell*)getRightLink())->replace(X, constFlag);
      return *this;
   }; // SlipCell& SlipHeader::replaceTop(const string& X, bool constFlag)

   /**
    * @brief Replace the list top object with a string* value.
    * <p>If the list is not empty, the bottom object is replaced. Otherwise,
    *    the request is ignored.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2012</b> Can not changeTop/Bot( of an empty list for  </li>
    * </ul>
    * @sa SlipHeader::replace(string*, bool)
    * @param[in] X (string*) right hand side (RHS) replacement SlipHeader reference
    * @param[in] constFlag <b>true</b> string and object referenced not copied
    * @return reference to the current list header
    */
   SlipCell& SlipHeader::replaceTop(const string* X, bool constFlag) { // Substitute list TOP value
      if (isEmpty())
         postError(__FILE__, __LINE__, SlipErr::E2012, "SlipHeader::replaceBot", "", "", *this);
      else ((SlipCell*)getRightLink())->replace(X, constFlag);
      return *this;
   }; // SlipCell& SlipHeader::replaceTop(const string* X, bool constFlag)

   /**
    * @brief Return the number of cells in the list.
    * <p>The list cells counted only include the cell in the top level. Sublists
    *    are not counted,</p>
    * @return number of cells in the list
    */
   unsigned SlipHeader::size() const {                                 // Number of cells in list
      unsigned count = 0;
      SlipCellBase* link = this->getRightLink();
      if (!(link == NULL) || (link == UNDEFDATA))
         for(; !link->isHeader(); link = link->getRightLink()) count++;
      return count;
   }; // unsigned SlipHeader::size()

   /**
    * @brief Move all cells from the list top including the input to a new list.
    * <p>This is front-end move. All cells from the list top to and including
    *    the cell input are moved to a new list. The cell reference input must
    *    be in the current cell. This is not checked! If the input cell is not
    *    in the current list, the operation is undefined.</p>
    * <p>If the current list is empty then the new list will be empty.</p>
    * <p>If the input SlipCell reference is to the header then a nasty diagnostic
    *    message will be output. Use <b>moveListLeft</b> or <b>moveListRight</b>.</p>
    * <p>After removal of all cells, the current list may be empty.</p>
    * <tt>
    *    <pre>
    *       SlipHeader& newList = this->slipLeft(this->getBot); // moves all cells
    *    </pre>
    * </tt>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2010</b> Can not use header cell.</li>
    * </ul>
    * @param[in] X (SlipCell&) reference to the rightmost cell to be moved.
    * @return return a reference to the new cell
    */
   SlipHeader& SlipHeader::splitLeft(SlipCell&  X) {                   // Split cells to left of X & X to a new list
      SlipHeader* header = new SlipHeader();
      if (checkHeader(*this, "splitLeft")) {
         if (checkThis(X, "splitLeft")) {
            if (X.isHeader()) {
               postError(__FILE__, __LINE__, SlipErr::E2010, "SlipHeader::splitLeft", "", "as argument", X);
            } else {
               if (isEmpty()) return *header;
               header->setRightLink(&getTop());
               header->setLeftLink(&X);

               setLeftLink(&getTop(), header);

               setRightLink(X.getRightLink());
               setLeftLink(getBot().getRightLink());

               setLeftLink(X.getRightLink(), this);
               setRightLink(&X, header);
            }
         }
      }
      return *header;
   }; // SlipHeader& SlipHeader::SplitLeft(SlipCell&  Cell)

   /**
    * @brief Move all cells from the current cell to the list bottom to a new list.
    * <p>This is back-end move. All cells from the input cell including the
    *    input cell to and including the list bottom are moved to a new list.
    *    The cell reference input must be in the current cell. This is not
    *    checked! If the input cell is not in the current list, the operation
    *    is undefined.</p>
    * <p>If the current list is empty then the new list will be empty.</p>
    * <p>If the input SlipCell reference is to the header then a nasty diagnostic
    *    message will be output. Use <b>moveListLeft</b> or <b>moveListRight</b>.</p>
    * <p>After removal of all cells, the current list may be empty.</p>
    * <tt>
    *    <pre>
    *       SlipHeader& newList = this->slipLeft(this->getBot); // moves all cells
    *    </pre>
    * </tt>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2010</b> Can not use header cell.</li>
    * </ul>
    * @param[in] X (SlipCell&) reference to the rightmost cell to be moved.
    * @return return a reference to the new cell
    */
   SlipHeader&  SlipHeader::splitRight(SlipCell&  X) {          // Split cells to right of X & X to a new list
      SlipHeader* header = new SlipHeader();
      if (checkHeader(*this, "splitRight")) {
         if (checkThis(X, "splitRight")) {
            if (X.isHeader()) {
               postError(__FILE__, __LINE__, SlipErr::E2010, "SlipHeader::splitRight", "", "as argument", X);
            } else {
               if (isEmpty()) return *header;
               header->setRightLink(&X);
               header->setLeftLink(&getBot());

               setRightLink(&getBot(), header);

               setRightLink(X.getLeftLink(), this);
               setLeftLink(X.getLeftLink());

               setLeftLink(&X, header);
            }
         }
      }
      return *header;
   }; // SlipHeader& SlipHeader::

       /*************************************************
        *                   operators                   *
       *************************************************/
   /**
    * @brief Copy the list of a header or sublist to the current list.
    * <p>If the input is a SlipHeader then:</p>
    * <tt>
    *    <pre>
    *    `  *this = X;
    *    </pre>
    * </tt>
    * <p>If the input is a SlipSublist then:</p>
    * <tt>
    *    <pre>
    *    `  *this = getSublistHeader(X);
    *    </pre>
    * </tt>
    * causing the input list to replace the existing list.
    * <p>Otherwise a diagnostic message is issued.</p>
    * <p><b>ERROR</b></p>
    * <ul>
    *    <li><b>E3015</b> SlipHeader must be assigned to a SlipHeader. </li>
    * </ul>
    * @sa operator=(const SlipHeader& X)
    * @param[in] X (SlipCell&) reference to a SlipCell to replace the current cell
    * @return reference to the current list
    */
   SlipCell&  SlipHeader::operator=(const SlipCell& X) {               // Y = V
      if (X.isHeader())
         return *this = (SlipHeader&)X;
      else if (X.isSublist())
         return *this = (SlipHeader&)*getSublistHeader(X);
      else
         postError(__FILE__, __LINE__, SlipErr::E3015, "SlipHeader::SlipHeader=", "", "", *this, X);
      return *this;
   }; // SlipCell&  SlipHeader::operator=(const SlipDatum& X)

   /**
    * @brief Flush the current list and copy the contents of the new list.
    * <p>If the new list and the current list are the same, the current list
    *    is not flushed. The value of the current list in this case, is the
    *    current list.</p>
    * <p>If the new list is not the same as the current list, then </p>
    * <ul>
    *    <li>the current list is flushed and the list cells of the new list
    *        are copied.</li>
    *    <li>the mark of the old list is copied to the new list.</li>
    *    <li>the old descriptor list is copied to the new list.</li>
    * </ul>
    * <p>The new list is an identical and independent copy of the old list.
    *    A change made to the SlipDatum cells of the old list or old
    *    descriptor list is not reflected in the new list and vica versa.
    *    A change made to a sublist of the old list effects the same sublist
    *    of the new list, that is, a copy of a sublist causes a new reference
    *    to the sublist but not a copy of the cells in the sublist.</p>
    * <p>This operation is not recursive. There is no descent into sublists
    *    referenced by the new list.</p>
    * @param[in] X (SlipHeader&) reference to a SlipHeader object
    * @return reference to the current list
    */
   SlipHeader& SlipHeader::operator=(const SlipHeader& X) {            // Y = V
      if (this != &X) {
         flush();
         SlipHeader* to = this;
         SlipHeader* from = const_cast<SlipHeader*>(&X);
         to->putMark(from->getMark());
         SlipCellBase* cell = from->getRightLink();
         for(; !cell->isHeader(); cell = cell->getRightLink())
            to->insertLeft(copy((SlipCell&)*cell));

         /**********************************************
          * Copying Descriptor Lists:
          * A Descriptor List can contain another
          * Descriptor List. The copying operation
          * is iterative.
          *
          * List file input processing uses the
          * Descriptor List field in a SlipHeader
          * object for a forward reference. When
          * used this way instead of a Descriptor
          * List containing SlipDatum or SliSublist
          * object, the list contains SlipHeader
          * objects. Detecting this condition
          * terminates the copy operation.
          **********************************************/
         while(from->isDList()) {
            to->create_dList();
            from = &from->getDList();
            cell = from->getRightLink();
            if (cell->isHeader()) break;
            to = &(to->getDList());
            for(; !cell->isHeader(); cell = cell->getRightLink())
               to->insertLeft(copy((SlipCell&)*cell));
         }
      }
      return *this;
   }; // SlipCell&  SlipHeader::operator=(const SlipDatum& X)
   SlipHeader& SlipHeader::operator=(const bool    X) {                // Y = V
      stringstream str;
      str << "Value of bool " << X << " is illegal.";
      postError(__FILE__, __LINE__, SlipErr::E3015, "SlipHeader::SlipHeader=", "", str.str(), *this);
      return *this;
   }; // SlipHeader& SlipHeader::operator=(const X)

   /**
    * @brief A header can not be replaced by a CHAR.
    * <p><b>ERROR</b></p>
    * <ul>
    *    <li><b>E3015</b> SlipHeader must be assigned to a SlipHeader.Value of CHAR is illegal. </li>
    * </ul>
    * @param[in] X (CHAR) value
    * @return reference to the current list
    */
   SlipHeader& SlipHeader::operator=(const CHAR    X) {                // Y = V
      stringstream str;
      str << "Value of CHAR " << X << " is illegal.";
      postError(__FILE__, __LINE__, SlipErr::E3015, "SlipHeader=", "", str.str(), *this);
      return *this;
   }; // SlipHeader& SlipHeader::operator=(const X)

   /**
    * @brief A header can not be replaced by a DOUBLE.
    * <p><b>ERROR</b></p>
    * <ul>
    *    <li><b>E3015</b> SlipHeader must be assigned to a SlipHeader.Value of DOUBLE is illegal. </li>
    * </ul>
    * @param[in] X (DOUBLE) value
    * @return reference to the current list
    */
   SlipHeader& SlipHeader::operator=(const DOUBLE  X) {                // Y = V
      stringstream str;
      str << "Value of DOUBLE " << X << " is illegal.";
      postError(__FILE__, __LINE__, SlipErr::E3015, "SlipHeader=", "", str.str(), *this);
      return *this;
   }; // SlipHeader& SlipHeader::operator=(const X)

   /**
    * @brief A header can not be replaced by a LONG.
    * <p><b>ERROR</b></p>
    * <ul>
    *    <li><b>E3015</b> SlipHeader must be assigned to a SlipHeader.Value of LONG is illegal. </li>
    * </ul>
    * @param[in] X (LONG) value
    * @return reference to the current list
    */
   SlipHeader& SlipHeader::operator=(const LONG    X) {                // Y = V
      stringstream str;
      str << "Value of LONG " << X << " is illegal.";
      postError(__FILE__, __LINE__, SlipErr::E3015, "SlipHeader=", "", str.str(), *this);
      return *this;
   }; // SlipHeader& SlipHeader::operator=(const X)

   /**
    * @brief A header can not be replaced by a UCHAR.
    * <p><b>ERROR</b></p>
    * <ul>
    *    <li><b>E3015</b> SlipHeader must be assigned to a SlipHeader.Value of UCHAR is illegal. </li>
    * </ul>
    * @param[in] X (UCHAR) value
    * @return reference to the current list
    */
   SlipHeader& SlipHeader::operator=(const UCHAR   X) {                // Y = V
      stringstream str;
      str << "Value of UCHAR " << X << " is illegal.";
      postError(__FILE__, __LINE__, SlipErr::E3015, "SlipHeader=", "", str.str(), *this);
      return *this;
   }; // SlipHeader& SlipHeader::operator=(const X)

   /**
    * @brief A header can not be replaced by a ULONG.
    * <p><b>ERROR</b></p>
    * <ul>
    *    <li><b>E3015</b> SlipHeader must be assigned to a SlipHeader.Value of ULONG is illegal. </li>
    * </ul>
    * @param[in] X (ULONG) value
    * @return reference to the current list
    */
   SlipHeader& SlipHeader::operator=(const ULONG   X) {                // Y = V
      stringstream str;
      str << "Value of ULONG " << X << " is illegal.";
      postError(__FILE__, __LINE__, SlipErr::E3015, "SlipHeader=", "", str.str(), *this);
      return *this;
   }; // SlipHeader& SlipHeader::operator=(const X)

   /**
    * @brief A header can not be replaced by a PTR.
    * <p><b>ERROR</b></p>
    * <ul>
    *    <li><b>E3015</b> SlipHeader must be assigned to a SlipHeader.Value of PTR is illegal. </li>
    * </ul>
    * @param[in] X (PTR) value
    * @return reference to the current list
    */
   SlipHeader& SlipHeader::operator=(const PTR     X) {                // Y = V
      stringstream str;
      str << "Value of PTR " << X << " is illegal.";
      postError(__FILE__, __LINE__, SlipErr::E3015, "SlipHeader=", "", str.str(), *this);
      return *this;
   }; // SlipHeader& SlipHeader::operator=(const X)

   /**
    * @brief A header can not be replaced by a string&.
    * <p><b>ERROR</b></p>
    * <ul>
    *    <li><b>E3015</b> SlipHeader must be assigned to a SlipHeader.Value of string& is illegal. </li>
    * </ul>
    * @param[in] X (string&) value
    * @return reference to the current list
    */
   SlipHeader& SlipHeader::operator=(const string&  X) {               // Y = V
      stringstream str;
      str << "Value of STRING " << X << " is illegal.";
      postError(__FILE__, __LINE__, SlipErr::E3015, "SlipHeader=", "", str.str(), *this);
      return *this;
   }; // SlipHeader& SlipHeader::operator=(const string&  X)

   /**
    * @brief A header can not be replaced by a string*.
    * <p><b>ERROR</b></p>
    * <ul>
    *    <li><b>E3015</b> SlipHeader must be assigned to a SlipHeader.Value of string* is illegal. </li>
    * </ul>
    * @param[in] X (string*) value
    * @return reference to the current list
    */
   SlipHeader& SlipHeader::operator=(const string*  X) {               // Y = V
      stringstream str;
      str << "Value of STRING " << X << " is illegal.";
      postError(__FILE__, __LINE__, SlipErr::E3015, "SlipHeader=", "", str.str(), *this);
      return *this;
   }; // SlipHeader& SlipHeader::operator=(const string*  X)

   /**
    * @brief The address of the sublist reference or header is compared.
    * <p>The address of the referenced list head in a sublist or the
    *    address of a list header is compared with the address of the
    *    current list.</p>
    * @param[in] X (SlipCell&) reference to a SlipCell
    * @return <b>true</b> the addresses compare
    */
   bool SlipHeader::operator==(const SlipCell& X) {                    // Y == X
       SlipHeader* head1 = this;
       SlipHeader* head2 = NULL;
       if (X.isSublist()) {
           head2 = *(SlipHeader**)getSublistHeader(X);
       } else if (isHeader()) {
           head2 = &(SlipHeader&)X;
       }
       return (head1 == head2);
   }; // bool SlipHeader::operator==(const SlipCell& X)

   /**
    * @brief The address of the current list head and the input are compared.
    * @param[in] X (SlipHeader&) reference to a SlipHead object
    * @return <b>true</b> if the addresses compare
    */
   bool SlipHeader::operator==(const SlipHeader& X) { return (this == &X);}     // Y == X


            /*****************************************
             *             Miscellaneous             *
            ******************************************/

   /**
    * @fn SlipHeader::getMark() const
    * @brief Return the header mark field value.
    * @return the header mark field value
    */
   USHORT SlipHeader::getMark() const {                                // value of list mark
       return *getHeadMrk() & REFMARKMASK;
   }; // USHORT SlipHeader::getMark()

   /**
    * @fn SlipHeader::isDList() const
    * @brief Check whether the current SlipHeader object has a Descriptor List.
    * @return <b>true></b> The current SlipHeader object has a Descriptor List
    */

   /**
    * @fn SlipHeader::isEmpty() const
    * @brief Check whether the current SlipHeader object is empty.
    * @return <b>true</b> the current SlipHeader object is empty
    */

   /**
    * @brief Two lists are compared for structural equality.
    * <p>Identical structural components have identical SlipCell types at
    *    each successive location. This definition continues recursively
    *    to all sublists.</p>
    * <p>The content on any object is not compared. The type is compared
    *    using SlipCellBase::getID. The result is:</p>
    *
    * | ID  |            Associated Objects          |
    * | --: | :------------------------------------- |
    * |   0 | SlipUndefined (illegal)                |
    * |   1 | SlipSublist                            |
    * |   2 | SlipHeader                             |
    * |   5 | bool, CHAR, UCHAR, LONG, ULONG, DOUBLE |
    * |   6 | PTR, STRING                            |
    * @param[in] Y (SlipHeader&) reference to a SlipHead object
    * @return two lists are structurally identical
    */
   bool SlipHeader::isEqual(const SlipHeader& Y) const {               // true the the lists are structurally identical
      SlipReader* curList = new SlipReader(*this);
      SlipReader* othList = new SlipReader(Y);
      bool flag = true;
      curList->advanceSWR();
      othList->advanceSWR();
      while(curList->isHeader() && othList->isHeader()) {
         if (curList->currentCell().getClassType() != othList->currentCell().getClassType()) {
            flag = false;
            break;
         }
         curList->advanceSWR();
         othList->advanceSWR();
      };
      return flag;
   }; // bool SlipHeader::isEqual(SlipHeader& Y)

   /**
    * @fn SlipHeader::getRefCount() const
    * @brief Return the value of the SlipHead refCnt field.
    * @return value of the SlipHeader refCnt field
    */

   /**
    * @brief Overwrite the SlipHeader mark field with a new value.
    * <p>The least significant 15-bits are masked and inserted into the least
    *    significant bits on the mark field. The upper bit is ignored and if
    *    set, is silently removed.</p>
    * @param[in] X (USHORT) new SlipHeader mark field
    * @return the value of the old mark field
    */
   USHORT SlipHeader::putMark(const USHORT X) {                        // Mark the list
      USHORT oldMark = getMark();
      *getHeadMrk() = (oldMark & ~REFMARKMASK) | (X & REFMARKMASK);
      return oldMark & REFMARKMASK;
   }; // SlipHeader& SlipHeader::putMark(LONG X)

   /**
    * @brief Output a list definition to a cout.
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
    *    list->writeQuick();                           // ( 1 ( 2 ) 3 )
    *    list->deleteList();
    *    share->deleteList();
    *    delete reader;
    * </pre></tt>
    */
   void SlipHeader::writeQuick() {                                              // Output the current list - quickly
      writeQuick(cout);
   }; // void SlipHeader::writeQuick()

   /**
    * @brief Output a list definition to a stream.
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
    *    list->writeQuick(ofs);                        // ( 1 ( 2 ) 3 )
    *    list->deleteList();
    *    share->deleteList();
    *    delete reader;
    * </pre></tt>
    * @param[in] out (ostream&) output stream
    */
   void SlipHeader::writeQuick(ostream& out) {                                  // Output the current list - quickly
      int col = 2;
      out << " (";
      writeQuick(out, *this, col);
      out << " )" << endl;
   }; // void SlipHeader::writeQuick(ostream& out)

   /**
    * @brief Output a list definition to a file.
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
    *    list->writeQuick("test.txt");                // ( 1 ( 2 ) 3 )
    *    list->deleteList();
    *    share->deleteList();
    *    delete reader;
    * </pre></tt>
    * @param[in] filename (string&) output filename
    */
   void SlipHeader::writeQuick(string& filename) {                              // Output the current list - quickly
      ofstream out;
      out.clear(ios::badbit | ios::failbit | ios::eofbit);
      out.open(filename.c_str(), ios::trunc);
      if (!out.is_open()) {
        postError(__FILE__, __LINE__, SlipErr::E4014, "SlipHeader::writeQuick", filename, "", *this);
      } else {
         int col = 1;
         out << '(';
         writeQuick(out, *this, col);
         out << ')';
         out.close();
      }
   }; // void SlipHeader::writeQuick(string& filename)

   /**
    * @brief Output as a formatted list to a stream.
    * <p>Recursive method which formats and outputs an ASCII version of the
    *    list. The output does not support multiple references to the same
    *    list. Each sublist reference to the same listis treated as an
    *    anonymous and independent list. Input of a quick kformatted output
    *    will (therefore) be different than the original list.</p>
    * <p>In like manner declarations to User Defined classes are not
    *    generated. Before input of a quick formatted list the application
    *    must register these classes.</p>
    * <p>A simple example is:</p>
    * <tt><pre>
    *    SlipHeader* head = new SlipHeader();
    *    SlipHeader* sublist = new SlipHeadaer();
    *    head ->enqueue((long)1).enqueue((PTR)ptr).enqueue((SlipSublist)sublist)enqueue((SlipSublist)sublist);
    *
    *    head.write();     // 'normal' write
    *    User userdata;    // assuming the class name is "userdata"
    *    list1 ();         // shared sublist
    *    list2 ( 1 userdata() {list1} {list1}
    *    {list2}
    *
    *    head.writeQuick();
    *    ( 1 userdata() () () )
    * </pre></tt>
    * @param[in] out (ostream&) output stram
    * @param[in] list (SlipHeader&) current sublilst being processed
    * @param[inout] col (int&) column number
    */
   void SlipHeader::writeQuick(ostream& out, SlipHeader& list, int& col) {      // Output the current list - quickly
      enum { MAXCOL = 120 };
      SlipSequencer seq(list);

      if (col > (MAXCOL - 10)) {
         col = 0;
         out << endl;
      }

      if (list.getMark()) {
         col += 9;
         out << " { 0x" << setfill('0') << setw(2) << hex << list.getMark() << " }";
      }

      if (list.isDList()) {
         col += 2;
         out << " <";
         writeQuick(out, list.getDList(), col);
         out << " >";
         col += 2;
      }

      if (col > (MAXCOL - 10)) {
         col = 0;
         out << endl;
      }

      for (seq.advanceLWR(); !seq.isHeader(); seq.advanceLWR()) {
         if (seq.isSublist()) {
            SlipHeader& head = ((SlipSublist&)(seq.currentCell())).getHeader();
            col += 2;
            out << " (";
            writeQuick(out, head, col);
            out << " )";
            col += 2;
         } else if (seq.isPtr()) {
            string datum = seq.toString();
            if ((col + datum.length() + 3) > MAXCOL) {
               col = 0;
               out << endl;
            }
            out << " (" << datum << ')';
            col += datum.length() + 3;
         } else {
            string datum = seq.currentCell().write();
            col += datum.length() + 1;
            out << ' ' << datum;
         }

         if (col > (MAXCOL - 10)) {
            col = 0;
            out << endl;
         }
      }
   }; // void SlipHeader::writeQuick(ostream& ou, int& colt)

   /**
    * @brief Output as a formatted list to a string.
    * @return (string) contains the formatted s
    */
   string SlipHeader::writeToString() {                                         // Return an ASCII formated list as a string
      stringstream pretty;
      writeQuick(pretty);
      string temp = pretty.str();
      return temp.substr(0, max((int)temp.length()-1, 0));
   }; // string SlipHeader::writeToString()

   /**
    * @brief Output the list to cout.
    * @return <b>true</b>
    * @param[in] testFlag (bool) <b>true</b> turn debug output on
    */
   bool SlipHeader::write(bool testFlag) {                                      // Output the current list
      SlipWrite write(*this, testFlag);
      return true;
   }; // void SlipHeader::write(testFlag)

   /**
    * @brief Output the list to the user provided stream.
    * @param out (ostream&) output stream
    * @return <b>true</b>
    * @param[in] testFlag (bool) <b>true</b> turn debug output on
    */
   bool SlipHeader::write(ostream& out, bool testFlag) {                        // Output the current list
      SlipWrite write(*this, out, testFlag);
      return true;
   }; // void SlipHeader::write(ostream& out, testFlag)

   /**
    * @brief Output the list to a file.
    * @param filename (string&) output file name
    * @return <b>true</b> filename was opened successfully
    */
   bool SlipHeader::write(string& filename) {                                   // Output the current list
      bool retval = true;
      ofstream out;
      out.clear(ios::badbit | ios::failbit | ios::eofbit);
      out.open(filename.c_str(), ios::trunc);
      if (!out.is_open()) {
        postError(__FILE__, __LINE__, SlipErr::E4014, "SlipHeader::write", filename, "", *this);
        retval = false;
      } else {
         SlipWrite write(*this, out);
         out.close();
      }
      return retval;
   }; // void SlipHeader::write(string& filename)


            /*****************************************
             *             Dump Methods              *
            ******************************************/


   /**
    * @brief Uglify a list.
    * <p>Begin the output process by setting the initial output column (for
    *    calculation of newlines) and setting the default output stream
    *    properties. Then continue by calling <em>dumpList(ostream&, num)</em>.</p>
    * @sa dumpList(ostream&, num)
    */
   void SlipHeader::dumpList() {                                       // Print List
      int col = 0;
      dumpList(cout, col);
      cout << endl;
   }; // void SlipHeader::dumpList(ostream& out)

   /**
    * @brief Recursively uglify and output a list.
    * @sa dumpList()
    * @param[in] out output stream
    * @param[in,out] col column number used to generate a newline
    */
   void SlipHeader::dump(ostream& out, int& col) {                     // Output the current cell
      ios_base::fmtflags ioFlags = out.ios_base::flags();
      dumpList(out, col);
      out.ios_base::flags(ioFlags);
   }; // void SlipHeader::dump(ostream& out, int& col)

   /**
    * @brief Uglify a list.
    * <p>This method uses recursion. The algorithm is:</p>
    * <ul>
    *    <li>If the list has been entered, output "CYCLE" and return.</li>
    *    <li>Visit the list (set the 'visit' bit in the header).</li>
    *    <li>If there is a Description List, output the list unless it is
    *        part of a cycle.</li>
    *    <li>Output the list.</li>
    *    <li>Reset the 'visit' bit.</li>
    *    <li>Return.</li>
    * </ul>
    * <p>The output is one line per list cell with bracketing symbols.</p>
    * @note Cycles in graphs are not supported. Acyclic Graphs (networks),
    *       trees, and simple lists are supported. However, (Cyclic) Graphs
    *       will be output correctly.
    * @sa dump
    * @sa dumpDescriptionList
    * @sa dumpHeadList
    * @param[in] out output stream
    * @param[in,out] col column number used to generate a newline
    */
   void SlipHeader::dumpList(ostream& out, int& col) {                 // Output the current Header and list constants
      if (isDeleted()) {
         out << "DELETED " << dump() << '\n';
      } else {
         out << "(";
         if (isVisited(*this)) {
            out << "CYCLE " << dump() << '\n';
         } else {
            out << dump() << '\n';
            visit(this);
            if (*getHeadDescList() != NULL) {
               SlipHeader* dList = (SlipHeader*)*getHeadDescList();
               out << '<';
               if (dList->isDeleted()) {
                  col += 65;
                  out << "DELETED DESCRIPTION LIST " << dList->dump() << '\n';
               } else if (!(dList->isEmpty())) {
                  if (!(isVisited(*dList)))
                     dumpDescriptorList(out, col);
               }
               out << ">\n";
            }
            dumpHeadList(out, col);
            leave(this);
            out << ')';
         }
      }
   }; // void SlipHeader::dumpList(ostream& out, int& col)

   /**
    * @brief Output a formatted, uglified version of the Descriptor List.
    * <p>The behavior of this method is identical to dumpList except for the
    *    inclusion of Descriptor List bracketing. In this regard:</p>
    * <ul>
    *    <li>The Descriptor List is bracketed by &lt; and &gt.</li>
    *    <li>A key, value pair is bracketed by (key value)</li>
    *    <li>Any key or values which are a sublist invoke <em>printList</em>.</li>
    *    <li>This definition is recursive (a sublist header can have a
    *        Descriptor List).</li>
    * </ul>
    * <p>This is all effected by calling <em>dump</em> for each of the key and
    *    value.</p>
    * @sa dump()
    * @param[in] out output stream
    * @param[in,out] col column number used to generate a newline
    */
   void SlipHeader::dumpDescriptorList(ostream& out, int& col) {       // Output Descriptor List
      SlipCell*   dList = (SlipCell*)*getHeadDescList();
      SlipCell*   link  = (SlipCell*)SlipCellBase::getRightLink(*dList);
      visit((SlipHeader*)dList);
      for (
          ; !link->isHeader()
          ; link = (SlipCell*)SlipCellBase::getRightLink(*link)) {
         cout << '(';
         link->dump(out, col);
         link = (SlipCell*)SlipCellBase::getRightLink(*link);
         cout << ' ';
         link->dump(out, col);
         cout << ')';
      }
      leave((SlipHeader*)dList);
   }; // void SlipHeader::dumpDescriptorList(ostream& out, int& col)

   /**
    * @brief Uglify list cells.
    * @sa dump(ostream&, int&)
    * @param[in] out output stream
    * @param[in,out] col column number used to generate a newline
    */
   void SlipHeader::dumpHeadList(ostream& out, int& col) {
      if (isEmpty()) return;
      SlipCell*   link   = (SlipCell*)getRightLink();
      do {
         out << ' ';
         link->dump(out, col);
         link   = (SlipCell*)SlipCellBase::getRightLink(*link);
      } while(link != this);
      out << ' ';
   }; // void SlipHeader::dumpHead(ostream& out, int& col)

   /**
    * @brief Uglify the input.
    * <p>Output the leftLink, rightLink, and operation in hex. Output the datum
    *    value according to it's type</p>
    * @note There is a visibility problem with referencing SlipCellBase::dump.
    *       When this is resolved, this method will go the way of the woolly
    *       mammoth.
    * @return ugly string
    */
   string SlipHeader::dump() const { return SlipCellBase::dump(); }    // pretty print SLIP Header cell

   /**
    * @fn SlipHeader::visit(SlipHeader* X)
    * @brief Set the 'visit' bit of a list head.
    * @param[in] X SlipHeader object whose 'visit' bit is set
    */
   void SlipHeader::visit(SlipHeader* X) {
      *(X->getHeadRefCnt()) |= VISITMASK;
   }; // void SlipHeader::visit()

}; // namespace slip
