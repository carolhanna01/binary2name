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
 * @file   SlipSublist.cpp
 * @author A. Schwarz
 * @date   June 17, 2012
 *
 * @brief Contains the SlipSublist definitions.
 */

# include <sstream>
# include <iostream>
# include "SlipCell.h"
# include "SlipCellBase.h"
# include "SlipDatum.h"
# include "SlipGlobal.h"
# include "SlipHeader.h"
# include "SlipSublist.h"

# include "SlipBoolOp.h"       // required but I have not idea why???


namespace slip {

       /*************************************************
        *           Constructors & Destructors          *
        ************************************************/

   /**
    * @fn SlipSublist::SlipSublist(SlipHeader& X)
    * @brief Create a SlipSublist object from a SlipHeader.
    * <p>Defer construction to constructSublist(SlipHeader&)</p>
    * @see SlipSublist::constructSublist(SlipHeader&)
    * @param[in] X (SlipHeader&) reference to a SlipSublist or SlipHeader object
    */

   /**
    * @fn SlipSublist::SlipSublist(SlipSublist& X)
    * @brief Create a SlipSublist object from a SlipHeader.
    * <p>Defer construction to constructSublist(SlipSublist&)</p>
    * @see SlipSublist::constructSublist(SlipSublist&)
    * @param[in] X (SlipSublist&) reference to a SlipSublist or SlipHeader object
    */


   /**
    * @brief Create a SlipSublist object from a SlipSublist or SlipHeader object.
    * <p>If the input is not a SlipHeader or SlipSublist then issue an diagnostic
    *    message otherwise defer construction.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2011</b>  SlipSublist constructor must be passed a SlipHeader cell.</li>
    * </ul>
    * @see SlipSublist::constructSublist(SlipHeader&)
    * @see SlipSublist::constructSublist(SlipSublist&)
    * @param[in] X reference to a SlipSublist or SlipHeader object
    */
   SlipSublist::SlipSublist(SlipCell& X) {
      if (X.isHeader()) constructSublist((SlipHeader&)X);
      else if (X.isSublist()) constructSublist(*(SlipHeader*)*getSublistHeader(X));
      else {
         constructSublist();
         postError(__FILE__, __LINE__, SlipErr::E2011, "SlipSublist::SlipSublist", "", "SlipDatum not allowed", X);
      }
   }; // SlipSublist::SlipSublist(SlipCell& X)

   /**
    * @fn SlipSublist::~SlipSublist()
    * @brief SlipSublist destructor - return cell to AVSL.
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2010</b> Can not use header cell. Header in sublist is not a header</p>
    *    <li><b>E2034</b> Attempt to delete a SlipSublist cell that does not have a SlipHeader. </p>
    * </ul>
    */
   SlipSublist::~SlipSublist() {
      if (!isDeleted()) {
         SlipHeader* header = *(SlipHeader**)getSublistHeader();
         if (header == NULL)
            postError(__FILE__, __LINE__, SlipErr::E2034, "~SlipSublist", "", "", *this);
         else if (!(header->isHeader())) {
            postError(__FILE__, __LINE__, SlipErr::E2010, "~SlipSublist", "", "Header in sublist is not a header.", *this, *header);
         } else {
            if (getRightLink() != NULL) unLink();
            addAVSLCells( this, this);
         }
      }
   }; // SlipSublist::~SlipSublist()

   /**
    * @fn SlipSublist::constructSublist()
    * @brief Construct a SlipSublist template.
    * <p>NULL the <b>leftLink</b> and <b>rightLink</b> fields and put a pointer
    *    to the invariant <em>SlipOp</em> object into the <b>operator</b> field.
    *    </p>
    */

   /**
    * @brief Create a SlipSublist object from a SlipHeader.
    * <p>Check the input to see if it is valid, and if so, a SlipSublist
    *    object is created. If not, a SlipSublist object is created without a
    *    pointer to an associated SlipHeader object. Sooner or later the end
    *    will occur, and it won't be pleasant</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2006</b> Cell can not be a local temporary. Create with 'new'.</li>
    *    <li><b>E2035</b> SlipSublist cell is a TEMPORARY.  </li>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    *    <li><b>E3022</b> Can not insert a non-AVSL cell into a list.</li>s
    * </ul>
    * @param[in] X (SlipHeader&) reference to a SlipSublist or SlipHeader object
    */
   void SlipSublist::constructSublist(SlipHeader& X) {                          // Complete creating a sublist
      constructSublist();
      if (isTemp()) {
         postError(__FILE__, __LINE__, SlipErr::E2035, "SlipSublist::SlipSublist", "", "Must use \'new\'.", *this);
      } else if (checkThat(X, "SlipSublist")) {                                 // the header must be valid
         *getSublistHeader() = &X;
         (*getHeadRefCnt(X)) += 1;
      }

   }; // void SlipSublist::constructSublist(SlipHeader& X)

       /*************************************************
        *               Utility Methods                 *
       *************************************************/

   /**
    * @brief Create a copy of the current SlipSublist object.
    * <p>A new SlipSublist object based on the current object.</p>
    * @return reference to a created SlipHeader object
    */
   SlipCell& SlipSublist::factory() { return *(new SlipSublist(*this)); }       // Create a SlipCell

               /*****************************************
                *            Descriptor Lists           *
               ******************************************/
   /**
    * @fn SlipSublist::contains(SlipCell& value)
    * @copydoc SlipHeader::contains(SlipCell& ) */

    /**
     * @fn SlipSublist::containsKey(SlipCell& key)
     * @copydoc SlipHeader::containsKey(SlipCell& )
     */

   /**
    * @fn SlipSublist::create_dList()
    * @copydoc SlipHeader::create_dList( )
    */

   /**
    * @fn SlipSublist::deleteAttribute(SlipCell& key)
    * @copydoc SlipHeader::deleteAttribute(SlipCell&)
    */

   /**
    * @fn SlipSublist::delete_dList()
    * @copydoc SlipHeader::delete_dList()
    */

   /**
    * @fn SlipSublist::flush_dList()
    * @copydoc SlipHeader::flush_dList()
    */

   /**
    * @fn SlipSublist::get(SlipCell& key)
    * @copydoc SlipHeader::get(SlipCell&)
    */

   /**
    * @fn SlipSublist::keys()
    * @copydoc SlipHeader::keys()
    */

   /**
    * @fn SlipSublist::dumpDList()
    * @copydoc SlipHeader::dumpDList()
    */

   /**
    * @fn SlipSublist::put(SlipCell& key, SlipCell& value)
    * @copydoc SlipHeader::put(SlipCell&, SlipCell&)
    */

   /**
    * @fn SlipSublist::size_dList()
    * @brief Number of entries in the Descriptor List.
    * @return Number of entries in the Descriptor List
    */

               /*****************************************
                *      Header Manipulating Lists      *
               ******************************************/

      /**
       * @fn SlipSublist::dequeue()
       * @copydoc SlipHeader::dequeue()
       */

      /**
       * @fn SlipSublist::enqueue(SlipCell& X)
       * @copydoc SlipHeader::enqueue(SlipCell&)
       */

      /**
       * @fn SlipSublist::enqueue(bool X)
       * @copydoc SlipHeader::enqueue(bool)
       */

      /**
       * @fn SlipSublist::enqueue(UCHAR X)
       * @copydoc SlipHeader::enqueue(UCHAR)
       */

      /**
       * @fn SlipSublist::enqueue(CHAR X)
       * @copydoc SlipHeader::enqueue(CHAR)
       */

      /**
       * @fn SlipSublist::enqueue(ULONG X)
       * @copydoc SlipHeader::enqueue(ULONG)
       */

      /**
       * @fn SlipSublist::enqueue(LONG X)
       * @copydoc SlipHeader::enqueue(LONG)
       */

      /**
       * @fn SlipSublist::enqueue(DOUBLE X)
       * @copydoc SlipHeader::enqueue(DOUBLE)
       */

      /**
       * @fn SlipSublist::enqueue(const PTR X, const void* operation)
       * @copydoc SlipHeader::enqueue(const PTR, const void*)
       */

      /**
       * @fn SlipSublist::enqueue(const string& X, bool constFlag)
       * @copydoc SlipHeader::enqueue(const string&, bool)
       */

      /**
       * @fn SlipSublist::enqueue(const string* X, bool constFlag)
       * @copydoc SlipHeader::enqueue(const string*, bool)
       */

      /**
       * @fn SlipSublist::flush()
       * @copydoc SlipHeader::flush()
       */

      /**
       * @fn SlipSublist::getBot() const
       * @copydoc SlipHeader::getBot()
       */

      /**
       * @fn SlipSublist::getTop() const
       * @copydoc SlipHeader::getTop()
       */

      /**
       * @fn SlipSublist::pop()
       * @copydoc SlipHeader::pop()
       */

      /**
       * @fn SlipSublist::push(SlipCell& X)
       * @copydoc SlipHeader::push(SlipCell&)
       */

      /**
       * @fn SlipSublist::push(bool X)
       * @copydoc SlipHeader::push(bool)
       */

      /**
       * @fn SlipSublist::push(UCHAR X)
       * @copydoc SlipHeader::push(UCHAR)
       */

      /**
       * @fn SlipSublist::push(CHAR X)
       * @copydoc SlipHeader::push(CHAR)
       */

      /**
       * @fn SlipSublist::push(ULONG X)
       * @copydoc SlipHeader::push(ULONG)
       */

      /**
       * @fn SlipSublist::push(LONG X)
       * @copydoc SlipHeader::push(LONG)
       */

      /**
       * @fn SlipSublist::push(DOUBLE X)
       * @copydoc SlipHeader::push(DOUBLE)
       */

      /**
       * @fn SlipSublist::push(const PTR X, const void* operation)
       * @copydoc SlipHeader::push(const PTR, const void*)
       */

      /**
       * @fn SlipSublist::push(const string& X, bool constFlag)
       * @copydoc SlipHeader::push(const string&, bool)
       */

      /**
       * @fn SlipSublist::push(const string* X, bool constFlag)
       * @copydoc SlipHeader::push(const string*, bool)
       */

      /**
       * @fn SlipSublist::replaceTop(SlipHeader& X)
       * @copydoc SlipHeader::replaceTop(SlipHeader&)
       */

      /**
       * @fn SlipSublist::replaceTop(SlipCell& X)
       * @copydoc SlipHeader::replaceTop(SlipCell&)
       */

      /**
       * @fn SlipSublist::replaceBot(SlipHeader& X)
       * @copydoc SlipHeader::replaceBot(SlipHeader&)
       */

      /**
       * @fn SlipSublist::replaceBot(SlipCell& X)
       * @copydoc SlipHeader::replaceBot(SlipCell&)
       */

      /**
       * @fn lipSublist::replaceTop(bool X)
       * @copydoc SlipHeader::replaceTop(bool)
       */

      /**
       * @fn SlipSublist::replaceTop(UCHAR X)
       * @copydoc SlipHeader::replaceTop(UCHAR)
       */

      /**
       * @fn lipSublist::replaceTop(CHAR X)
       * @copydoc SlipHeader::replaceTop(CHAR)
       */

      /**
       * @fn SlipSublist::replaceTop(ULONG X)
       * @copydoc SlipHeader::replaceTop(ULONG)
       */

      /**
       * @fn SlipSublist::replaceTop(LONG X)
       * @copydoc SlipHeader::replaceTop(LONG)
       */

      /**
       * @fn SlipSublist::replaceTop(DOUBLE X)
       * @copydoc SlipHeader::replaceTop(DOUBLE)
       */

      /**
       * @fn SlipSublist::replaceTop(const PTR X, const void* operation)
       * @copydoc SlipHeader::replaceTop(const PTR, const void*)
       */

      /**
       * @fn SlipSublist::replaceTop(const string& X, bool constFlag)
       * @copydoc SlipHeader::replaceTop(const string&, bool)
       */

      /**
       * @fn SlipSublist::replaceTop(const string* X, bool constFlag)
       * @copydoc SlipHeader::replaceTop(const string*, bool)
       */

      /**
       * @fn SlipSublist::replaceBot(bool X)
       * @copydoc SlipHeader::replaceBot(bool)
       */

      /**
       * @fn SlipSublist::replaceBot(UCHAR X)
       * @copydoc SlipHeader::replaceBot(UCHAR)
       */

      /**
       * @fn SlipSublist::replaceBot(CHAR X)
       * @copydoc SlipHeader::replaceBot(CHAR)
       */

      /**
       * @fn SlipSublist::replaceBot(ULONG X)
       * @copydoc SlipHeader::replaceBot(ULONG)
       */

      /**
       * @fn SlipSublist::replaceBot(LONG X)
       * @copydoc SlipHeader::replaceBot(LONG)
       */

      /**
       * @fn SlipSublist::replaceBot(DOUBLE X)
       * @copydoc SlipHeader::replaceBot(DOUBLE)
       */

      /**
       * @fn SlipSublist::replaceBot(const PTR X, const void* operation)
       * @copydoc SlipHeader::replaceBot(const PTR, const void*)
       */

      /**
       * @fn SlipSublist::replaceBot(const string& X, bool constFlag)
       * @copydoc SlipHeader::replaceBot(const string&, bool)
       */

      /**
       * @fn SlipSublist::replaceBot(const string* X, bool constFlag)
       * @copydoc SlipHeader::replaceBot(const string*, bool)
       */

      /**
       * @fn SlipSublist::splitLeft(SlipCell&  X)
       * @copydoc SlipHeader::splitLeft(SlipCell&)
       */

      /**
       * @fn SlipSublist::splitRight(SlipCell&  X)
       * @copydoc SlipHeader::splitRight(SlipCell& )
       */


               /*****************************************
                *     Header Miscellaneous Methods      *
               ******************************************/

      /**
       * @fn SlipSublist::dumpList()
       * @copydoc SlipHeader::dumpList()
       */

      /**
       * @fn SlipSublist::isDList() const
       * @brief Return <b>true</b> if the Descriptor List is present.
       * <p>This is a convenience method for the user. It allows director
       *    interrogation of the referenced SlipHeader object.</p>
       * @return <b>true</b> if the Descriptor List is present
       */

      /**
       * @fn SlipSublist::isEmpty() const
       * @brief Return <b>true</b> if the list is empty.
       * <p>This is a convenience method for the user. It allows director
       *    interrogation of the referenced SlipHeader object.</p>
       * @return <b>true</b> if the list is empty
       */

      /**
       * @fn SlipSublist::isEqual(const SlipCell& X) const
       * @brief Return <b>true</b> if the two lists are structurally identical.
       * <p>This is a convenience method for the user. It allows director
       *    interrogation of the referenced SlipHeader object.</p>
       * @param[in] X reference the the comparand header list
       * @return <b>true</b> if the two lists are structurally identical
       */

      /**
       * @fn SlipSublist::getMark() const
       * @brief Return the header mark field contents.
       * <p>This is a convenience method for the user. It allows director
       *    interrogation of the referenced SlipHeader object.</p>
       * @return the header mark field contents
       */

      /**
       * @fn SlipSublist::getRefCount() const
       * @brief Return the header refCnt field contents.
       * <p>This is a convenience method for the user. It allows director
       *    interrogation of the referenced SlipHeader object.</p>
       * @return the header refCnt field contents
       */

      /**
       * @fn SlipSublist::putMark(const USHORT X)
       * @brief Change the header mark field.
       * <p>This is a convenience method for the user. It allows director
       *    interrogation of the referenced SlipHeader object.</p>
       * @param[in] X new mark field value
       * @return the old contents of the header mark field
       */

      /**
       * @fn SlipSublist::size() const
       * @brief Return the number cells in the header list.
       * <p>This is a convenience method for the user. It allows director
       *    interrogation of the referenced SlipHeader object.</p>
       * @return the number cells in the header list
       */

      /**
       * @fn SlipSublist::getHeader() const
       * @brief Return a reference to the header reference in the sublist.
       * <p>A reference to the contained SlipHeader object reference is returned.
       *    This give the user access to all public SlipHeader methods.</p>
       * @return a reference to the header reference in the sublist
       */

       /*************************************************
        *             Assignment operators              *
       *************************************************/
   /**
    * @brief Replace the header reference in the current SlipSublist object.
    * <p>The current header reference is replaced with a new header reference.
    *    The current header is deleted and the refCnt field of the new header
    *    incremented.</p>
    * <p>The existing object is reused. All pointers and references to the
    *    object are valid after the assignment.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2006</b> Cell can not be a local temporary. Create with 'new'.</li>
    *    <li><b>E2013</b> Cell must be in a list. Cell is not in a list.</li>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    *    <li><b>E3017</b> SlipDatum must be assigned to a SlipDatum.</li>
    *    <li><b>E3022</b> Can not insert a non-AVSL cell into a list.</li>
    * </ul>
    * @param[in] X reference to a SlipHeader or SlipSublist object
    * @return reference to the existing object
    */
   SlipCell&  SlipSublist::operator=(const SlipCell& X) {                       // Y = V
      if (checkThat(*this, "operator=") && checkThat(X, "operator=")) {
         bool flag = true;
         SlipHeader& header = getHeader();                                       // retain reference for subsequent deletion
         SlipHeader* newHead =  (SlipHeader*) &X;                                // guess that the input is a header
         if (X.isSublist())
            newHead = (SlipHeader*)*getSublistHeader(X);
         else if (!X.isHeader()) {
            flag = false;
            postError(__FILE__, __LINE__, SlipErr::E3017, "operator=", "", "", *this, X);
         }
         if (flag) {
            *getSublistHeader() = newHead;
            *getHeadRefCnt(*newHead) += 1;
            header.deleteList();                                                // this must follow the reference increment (*this == &X))
         }
      }
      return *this;
   }; // SlipCell&  SlipSublist::operator=(const SlipDatum& X)

   /**
    * @brief Replace the header reference in the current SlipSublist object.
    * <p>The current header reference is replaced with a new header reference.
    *    The current header is deleted and the refCnt field of the new header
    *    incremented.</p>
    * <p>The existing object is reused. All pointers and references to the
    *    object are valid after the assignment.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2006</b> Cell can not be a local temporary. Create with 'new'.</li>
    *    <li><b>E2013</b> Cell must be in a list. Cell is not in a list.</li>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    *    <li><b>E3022</b> Can not insert a non-AVSL cell into a list.</li>
    * </ul>
    * @param[in] X reference to a SlipSublist object
    * @return reference to the existing object
    */
   SlipSublist& SlipSublist::operator=(const SlipSublist& X) {
      if (checkThat(*this, "operator=") && checkThat(X, "operator=")) {
         SlipHeader& header = getHeader();                                       // retain reference for subsequent deletion
         *getSublistHeader() = &X.getHeader();
         *getHeadRefCnt(X) += 1;
         header.deleteList();
      }
      return *this;
   }; // SlipSublist& SlipSublist::operator=(const SlipSublist& X)

   /**
    * @brief A sublist can not be replaced by a bool.
    * <p><b>ERROR</b></p>
    * <ul>
    *    <li><b>E3017</b> SlipDatum must be assigned to a SlipDatum.</li>
    * </ul>
    * @param[in] X bool value
    * @return reference to the current list
    */
   SlipSublist& SlipSublist::operator=(const bool    X) {                       // Y = V
      postError(__FILE__, __LINE__, SlipErr::E3017,  "SlipSublist=", "", "Use replace", *this);
      return *this;
   }; // SlipSublist& SlipSublist::operator=(const bool X)

   /**
    * @brief A sublist can not be replaced by a CHAR.
    * <p><b>ERROR</b></p>
    * <ul>
    *    <li><b>E3017</b> SlipDatum must be assigned to a SlipDatum.</li>
    * </ul>
    * @param[in] X CHAR value
    * @return reference to the current list
    */
   SlipSublist& SlipSublist::operator=(const CHAR    X) {                       // Y = V
      postError(__FILE__, __LINE__, SlipErr::E3017,  "SlipSublist=", "", "Use replace", *this);
      return *this;
   }; // SlipSublist& SlipSublist::operator=(const CHAR X)

   /**
    * @brief A sublist can not be replaced by a DOUBLE.
    * <p><b>ERROR</b></p>
    * <ul>
    *    <li><b>E3017</b> SlipDatum must be assigned to a SlipDatum.</li>
    * </ul>
    * @param[in] X DOUBLE value
    * @return reference to the current list
    */
   SlipSublist& SlipSublist::operator=(const DOUBLE  X) {                       // Y = V
      postError(__FILE__, __LINE__, SlipErr::E3017,  "SlipSublist=", "", "Use replace", *this);
      return *this;
   }; // SlipSublist& SlipSublist::operator=(const DOUBLE X)

   /**
    * @brief A sublist can not be replaced by a LONG.
    * <p><b>ERROR</b></p>
    * <ul>
    *    <li><b>E3017</b> SlipDatum must be assigned to a SlipDatum.</li>
    * </ul>
    * @param[in] X LONG value
    * @return reference to the current list
    */
   SlipSublist& SlipSublist::operator=(const LONG    X) {                       // Y = V
      postError(__FILE__, __LINE__, SlipErr::E3017,  "SlipSublist=", "", "Use replace", *this);
      return *this;
   }; // SlipSublist& SlipSublist::operator=(const LONG X)

   /**
    * @brief A sublist can not be replaced by a UCHAR.
    * <p><b>ERROR</b></p>
    * <ul>
    *    <li><b>E3017</b> SlipDatum must be assigned to a SlipDatum.</li>
    * </ul>
    * @param[in] X UCHAR value
    * @return reference to the current list
    */
   SlipSublist& SlipSublist::operator=(const UCHAR   X) {                       // Y = V
      postError(__FILE__, __LINE__, SlipErr::E3017,  "SlipSublist=", "", "Use replace", *this);
      return *this;
   }; // SlipSublist& SlipSublist::operator=(const UCHAR X)

   /**
    * @brief A sublist can not be replaced by a ULONG.
    * <p><b>ERROR</b></p>
    * <ul>
    *    <li><b>E3017</b> SlipDatum must be assigned to a SlipDatum.</li>
    * </ul>
    * @param[in] X ULONG value
    * @return reference to the current list
    */
   SlipSublist& SlipSublist::operator=(const ULONG   X) {                       // Y = V
      postError(__FILE__, __LINE__, SlipErr::E3017,  "SlipSublist=", "", "Use replace", *this);
      return *this;
   }; // SlipSublist& SlipSublist::operator=(const ULONG X)

   /**
    * @brief A sublist can not be replaced by a PTR.
    * <p><b>ERROR</b></p>
    * <ul>
    *    <li><b>E3017</b> SlipDatum must be assigned to a SlipDatum.</li>
    * </ul>
    * @param[in] X PTR value
    * @return reference to the current list
    */
   SlipSublist& SlipSublist::operator=(const PTR     X) {                       // Y = V
      postError(__FILE__, __LINE__, SlipErr::E3017,  "SlipSublist=", "", "Use replace", *this);
      return *this;
   }; // SlipSublist& SlipSublist::operator=(const PTR X)

   /**
    * @brief A sublist can not be replaced by a strng&.
    * <p><b>ERROR</b></p>
    * <ul>
    *    <li><b>E3017</b> SlipDatum must be assigned to a SlipDatum.</li>
    * </ul>
    * @param[in] X string& value
    * @return reference to the current list
    */
   SlipSublist& SlipSublist::operator=(const string&  X) {                      // Y = V
      postError(__FILE__, __LINE__, SlipErr::E3017,  "SlipSublist=", "", "Use replace", *this);
      return *this;
   }; // SlipSublist& SlipSublist::operator=(const string& X)

   /**
    * @brief A sublist can not be replaced by a string*.
    * <p><b>ERROR</b></p>
    * <ul>
    *    <li><b>E3017</b> SlipDatum must be assigned to a SlipDatum.</li>
    * </ul>
    * @param[in] X string* value
    * @return reference to the current list
    */
   SlipSublist& SlipSublist::operator=(const string*  X) {                      // Y = V
      postError(__FILE__, __LINE__, SlipErr::E3017,  "SlipSublist=", "", "Use replace", *this);
      return *this;
   }; // SlipSublist& SlipSublist::operator=(const string* X)


               /*****************************************
                *          Manipulating Lists           *
               ******************************************/
   /**
    * @brief Replace the value of a list cell.
    * <p></p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2006</b> Cell can not be a local temporary. Create with 'new'.</li>
    *    <li><b>E2013</b> Cell must be in a list. Cell is not in a list.</li>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    *    <li><b>E3022</b> Can not insert a non-AVSL cell into a list.</li>
    * </ul>
    * @param[in] X reference to RHS of replacement
    * @return reference to the cell in the list
    */
   SlipCell& SlipSublist::replace(const SlipCell& X) {                          // Replace a cell on a list with a new cell
      SlipCell* cell = this;
      if (checkThis(*this, "replace") && checkThat(X, "replace")) {
         if (cell != &X) {
            if (X.isData()) {                                                      // Y = replace(SlipDatum)
               cell = new SlipDatum((SlipDatum&)X);
               replaceLinks(*cell);
               delete this;
            } else  {                                                              // Y = replace(Header or Sublist)
               *this = X;
            }
         }
      }
      return *cell;
   }; // SlipCell& SlipSublist::replace(const SlipCell& X)

   /**
    * @brief Replace the current list object (LHS) with a bool (RHS).
    * <p>The LHS becomes a SlipDatum object. The existing SlipSublist object
    *    is deleted.</p>
    * <p>A new SlipDatum object is created. All pointers and references to
    *    the old SlipSublist object are state and can not be used.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2006</b> Cell can not be a local temporary. Create with 'new'.</li>
    *    <li><b>E2013</b> Cell must be in a list. Cell is not in a list.</li>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    *    <li><b>E3022</b> Can not insert a non-AVSL cell into a list.</li>
    * </ul>
    * @param[in] X Right Hand Side of the replacement
    * @return a reference to the new object in the list
    */
   SlipCell& SlipSublist::replace(bool X) {                    // Replace a cell on a list with a new cell
      SlipCell* cell = this;
      if (checkThis(*this, "replace")) {
         cell = new SlipDatum(X);
         replaceLinks(*cell);
         delete this;
      }
      return *cell;
   };  // SlipCell& SlipSublist::replace(bool X)

   /**
    * @brief Replace the current list object (LHS) with a CHAR (RHS).
    * <p>The LHS becomes a SlipDatum object. The existing SlipSublist object
    *    is deleted.</p>
    * <p>A new SlipDatum object is created. All pointers and references to
    *    the old SlipSublist object are state and can not be used.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2006</b> Cell can not be a local temporary. Create with 'new'.</li>
    *    <li><b>E2013</b> Cell must be in a list. Cell is not in a list.</li>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    *    <li><b>E3022</b> Can not insert a non-AVSL cell into a list.</li>
    * </ul>
    * @param[in] X Right Hand Side of the replacement
    * @return a reference to the new object in the list
    */
   SlipCell& SlipSublist::replace(CHAR X) {                    // Replace a cell on a list with a new cell
      SlipCell* cell = this;
      if (checkThis(*this, "replace")) {
         cell = new SlipDatum(X);
         replaceLinks(*cell);
         delete this;
      }
      return *cell;
   }; // SlipCell& SlipSublist::replace(CHAR X)

   /**
    * @brief Replace the current list object (LHS) with a LONG (RHS).
    * <p>The LHS becomes a SlipDatum object. The existing SlipSublist object
    *    is deleted.</p>
    * <p>A new SlipDatum object is created. All pointers and references to
    *    the old SlipSublist object are state and can not be used.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2006</b> Cell can not be a local temporary. Create with 'new'.</li>
    *    <li><b>E2013</b> Cell must be in a list. Cell is not in a list.</li>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    *    <li><b>E3022</b> Can not insert a non-AVSL cell into a list.</li>
    * </ul>
    * @param[in] X Right Hand Side of the replacement
    * @return a reference to the new object in the list
    */
   SlipCell& SlipSublist::replace(LONG X) {                    // Replace a cell on a list with a new cell
      SlipCell* cell = this;
      if (checkThis(*this, "replace")) {
         cell = new SlipDatum(X);
         replaceLinks(*cell);
         delete this;
      }
      return *cell;
   }; // SlipCell& SlipSublist::replace(LONG X)

   /**
    * @brief Replace the current list object (LHS) with a DOUBLE (RHS).
    * <p>The LHS becomes a SlipDatum object. The existing SlipSublist object
    *    is deleted.</p>
    * <p>A new SlipDatum object is created. All pointers and references to
    *    the old SlipSublist object are state and can not be used.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2006</b> Cell can not be a local temporary. Create with 'new'.</li>
    *    <li><b>E2013</b> Cell must be in a list. Cell is not in a list.</li>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    *    <li><b>E3022</b> Can not insert a non-AVSL cell into a list.</li>
    * </ul>
    * @param[in] X Right Hand Side of the replacement
    * @return a reference to the new object in the list
    */
   SlipCell& SlipSublist::replace(DOUBLE X) {                  // Replace a cell on a list with a new cell
      SlipCell* cell = this;
      if (checkThis(*this, "replace")) {
         cell = new SlipDatum(X);
         replaceLinks(*cell);
         delete this;
      }
      return *cell;
   }; // SlipCell& SlipSublist::replace(DOUBLE X)

   /**
    * @brief Replace the current list object (LHS) with a UCHAR (RHS).
    * <p>The LHS becomes a SlipDatum object. The existing SlipSublist object
    *    is deleted.</p>
    * <p>A new SlipDatum object is created. All pointers and references to
    *    the old SlipSublist object are state and can not be used.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2006</b> Cell can not be a local temporary. Create with 'new'.</li>
    *    <li><b>E2013</b> Cell must be in a list. Cell is not in a list.</li>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    *    <li><b>E3022</b> Can not insert a non-AVSL cell into a list.</li>
    * </ul>
    * @param[in] X Right Hand Side of the replacement
    * @return a reference to the new object in the list
    */
   SlipCell& SlipSublist::replace(UCHAR X) {                   // Replace a cell on a list with a new cell
      SlipCell* cell = this;
      if (checkThis(*this, "replace")) {
         cell = new SlipDatum(X);
         replaceLinks(*cell);
         delete this;
      }
      return *cell;
   }; // SlipCell& SlipSublist::replace(UCHAR X)

   /**
    * @brief Replace the current list object (LHS) with a ULONG (RHS).
    * <p>The LHS becomes a SlipDatum object. The existing SlipSublist object
    *    is deleted.</p>
    * <p>A new SlipDatum object is created. All pointers and references to
    *    the old SlipSublist object are state and can not be used.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2006</b> Cell can not be a local temporary. Create with 'new'.</li>
    *    <li><b>E2013</b> Cell must be in a list. Cell is not in a list.</li>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    *    <li><b>E3022</b> Can not insert a non-AVSL cell into a list.</li>
    * </ul>
    * @param[in] X Right Hand Side of the replacement
    * @return a reference to the new object in the list
    */
   SlipCell& SlipSublist::replace(ULONG X) {                   // Replace a cell on a list with a new cell
      SlipCell* cell = this;
      if (checkThis(*this, "replace")) {
         cell = new SlipDatum(X);
         replaceLinks(*cell);
         delete this;
      }
      return *cell;
   }; // SlipCell& SlipSublist::replace(ULONG X)

   /**
    * @brief Replace the current list object (LHS) with a PTR (RHS).
    * <p>The LHS becomes a SlipDatum object. The existing SlipSublist object
    *    is deleted.</p>
    * <p>A new SlipDatum object is created. All pointers and references to
    *    the old SlipSublist object are state and can not be used.</p>
    * <p>The operations field provides a means to specify user defined operations.
    *    The pointer must be to a SlipOp defined class. The standard casting,
    *    unary, arithmetic, bitwise and logical operations must all be overridden.
    *    The result is that a user Defined object is treated as a atomic value
    *    in all computations.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2006</b> Cell can not be a local temporary. Create with 'new'.</li>
    *    <li><b>E2013</b> Cell must be in a list. Cell is not in a list.</li>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    *    <li><b>E3022</b> Can not insert a non-AVSL cell into a list.</li>
    * </ul>
    * @sa SlipCell::insRight(PTR, void*)
    * @param[in] X (PTR) Right Hand Side of the replacement
    * @param[in] operation (void*) SlipOp derived User Defined operations
    * @return reference to the inserted object
    */
   SlipCell& SlipSublist::replace(const PTR X, const void* operation) {         // Replace a cell on a list with a new cell
      SlipCell* cell = this;
      if (checkThis(*this, "replace")) {
         cell = new SlipDatum(X, operation);
         replaceLinks(*cell);
         delete this;
      }
      return *cell;
   }; // SlipCell& SlipSublist::replace(const PTR X, const void* operation)

   /**
    * @brief Replace the current list object (LHS) with a string& (RHS).
    * <p>The LHS becomes a SlipDatum object. The existing SlipSublist object
    *    is deleted.</p>
    * <p>A new SlipDatum object is created. All pointers and references to
    *    the old SlipSublist object are state and can not be used.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2006</b> Cell can not be a local temporary. Create with 'new'.</li>
    *    <li><b>E2013</b> Cell must be in a list. Cell is not in a list.</li>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    *    <li><b>E3022</b> Can not insert a non-AVSL cell into a list.</li>
    * </ul>
    * @param[in] X Right Hand Side of the replacement
    * @param[in] constFlag determines copy and delete characteristics of the string
    * @return a reference to the new object in the list
    */
   SlipCell& SlipSublist::replace(const string& X, bool constFlag) {            // Replace a cell on a list with a new cell
      SlipCell* cell = this;
      if (checkThis(*this, "replace")) {
         cell = new SlipDatum(X, constFlag);
         replaceLinks(*cell);
         delete this;
      }
      return *cell;
   }; // SlipCell& SlipSublist::replace(const string& X, bool constFlag

   /**
    * @brief Replace the current list object (LHS) with a string* (RHS).
    * <p>The LHS becomes a SlipDatum object. The existing SlipSublist object
    *    is deleted.</p>
    * <p>A new SlipDatum object is created. All pointers and references to
    *    the old SlipSublist object are state and can not be used.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2006</b> Cell can not be a local temporary. Create with 'new'.</li>
    *    <li><b>E2013</b> Cell must be in a list. Cell is not in a list.</li>
    *    <li><b>E3019</b> Cell has been deleted. Reference to cell is stale.</li>
    *    <li><b>E3022</b> Can not insert a non-AVSL cell into a list.</li>
    * </ul>
    * @param[in] X Right Hand Side of the replacement
    * @param[in] constFlag determines copy and delete characteristics of the string
    * @return a reference to the new object in the list
    */
   SlipCell& SlipSublist::replace(const string* X, bool constFlag) {            // Replace a cell on a list with a new cell
      SlipCell* cell = this;
      if (checkThis(*this, "replace")) {
         cell = new SlipDatum(X, constFlag);
         replaceLinks(*cell);
         delete this;
      }
      return *cell;
   }; // SlipCell& SlipSublist::replace(const string* X, bool constFlag

       /*************************************************
        *              Logical operators                *
       *************************************************/
   /**
    * @brief Return <b>true</b> if the same header is referenced.
    * <p>If the input is a SlipHeader or SlipSublist object, then the headaer
    *    addresses are compared for equality. Otherwise <b>false</b> is
    *    returned.</p>
    * @param[in] X comparand
    * @return <b>true</b> if the same header is referenced
    */
   bool SlipSublist::operator==(const SlipCell& X) {                            // Y == X
       SlipHeader* head1 = &getHeader();
       SlipHeader* head2 = NULL;
       if (X.isSublist()) {
           head2 = *(SlipHeader**)getSublistHeader(X);
       } else if (X.isHeader()) {
           head2 = &(SlipHeader&)X;
       }
       return (head1 == head2);
   }; // bool SlipSublist::operator==(const SlipCell& X)

   /**
    * @brief Uglify the current SlipSublist boject.
    * @return a string representing the uglified internals.
    */
   string SlipSublist::dump() const { return SlipCellBase::dump(); }            // pretty print SLIP Header cell


   /**
    * @fn void slip::SlipSublist::dump(ostream& out, int& col)
    * @brief Uglify a SlipSublist object and continue by uglifying its associated list.
    * <p>This method uses recursion. Output the ugly contents of the SlipSublist
    *    object and continue with the referenced list.</p>
    * @sa dump()
    * @sa dump(ostream&, int&)
    * @param[in] out output stream
    * @param[in,out] col column number used to generate a newline
    */
   void SlipSublist::dump(ostream& out, int& col) {                             // Output the current cell
      string prefix = "";
      if (isDeleted()) {
         prefix = "DELETED ";
         col += 7;
      }
      cout << prefix << SlipCellBase::dump() << '\n';
      if (!isDeleted())  getHeader().dump(out, col);
   }; // void SlipSublist::dump(ostream& out, int& col)

}; // namespace slip

