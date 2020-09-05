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
 * @file    SlipHeader.h
 * @author  A. Schwarz
 * @date    November 4, 2011
 * 
 * @brief Contains the SlipHeader declarations
 */

#ifndef _SLIPHEADER_H
#define	_SLIPHEADER_H

# include <ostream>
# include <string>
# include "SlipCell.h"
# include "SlipGlobal.h"

using namespace std;

namespace slip {
   
   class SlipOp;

/**
 * @class slip::SlipHeader
 * @brief Declarations for the unique list header.
 * 
 * <p>Every list has one and only one unique header. The list header is 
 *    responsible for remembering the first and last cells on the list,
 *    tracking the membership count of the number of lists the current list
 *    is a member of, and providing the user an associative list (called
 *    the Descriptor List), and providing the application the ability to mark 
 *    the list.</p>
 * <p>A list header and each cell in the list must be from the AVSL. No list 
 *    and no list cell can be from the heap or the runtime stack or from any 
 *    application data store. The list must be created using <b>new</b>.<p>
 * <p>Once a list is created, the application is responsible for deleting the 
 *    list. If the list becomes a member of another list this initial deletion
 *    will not cause the list to be destroyed. If the list is not a member of 
 *    another list then the list header and all list cell will be returned to 
 *    the AVSL. When the final list that the current list is a member of is 
 *    deleted, the current list will be deleted. That is, the current list must 
 *    be deleted once. If the current list is not subordinate to another list, 
 *    then this initial deletion will delete the list and its contents to the 
 *    AVSL. If the current list is a member of other lists, then deletion of 
 *    the current list is automatic when all the other lists are deleted.</p>
 * <p>Deletion of a list must used the deleteList() method and not the operator
 *    <em>delete</em>. Using the operator delete causes the list to be 
 *    inaccessible in all lists that it is a member of (and a diagnostic 
 *    message and exception will be generated).</p>
 * <p>The SlipHeader structure looks like:</p>
 *  <table>
 *    <caption>SlipHeader cell structure</caption>
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
 *       <td colspan="8">top</td>
 *       <td colspan="8">bottom</td>
 *    </tr>
 *    <tr>
 *       <td colspan="8">operator</td>
 *       <td colspan="8">Descriptor List</td>
 *    </tr>
 *    <tr>
 *       <td colspan="1">V</td>
 *       <td colspan="3">mark</td>
 *       <td colspan="4">count</td>
 *    </tr>
 * </table>
 * <br />
 * The field description is:
 * <ul>
 *    <li><b>top</b> pointer to the first (top) cell in a list. If the list is
 *        empty, the top pointer points to the header object.</li>
 *    <li><b>bottom</b> pointer to to the last (bottom) cell in a list. If the
 *        list is empty, the bottom pointer points to the header object.</li>
 *    <li><b>Descriptor List</b> pointer to the SlipHeader object defining
 *        the header associative list. The Descriptor List header acts in the
 *        same way as the SlipHeader except for the sofwtarer controlled 
 *        constraint that the list contains a even number of cells 
 *        (&lt;key value&gt; pairs ).</li>
 *    <li><b>operator</b> pointer to a SlipOp derived class (SlipHeaderOp)
 *        containing the invariant properties shared by every SlipHeader object.</li>
 *    <li><b>V</b> the list visit bit. During internal recursive processing of
 *        lists the visit bit is set to indicate that the list has been processed.
 *        This prevents reprocessing of a list.</li>
 *        @note this will be fully realized when graphs are implemented 
 *              containing cycles. </li>
 *    <li><b>mark</b> 15-bit user mark. An arbitrary mark set by the user. No
 *        diagnostic message is issued if the mark exceeds 14 bits, the remaining
 *        bits are ignored.</li>
 *    <li><b>count</b> a 16-bit count of the number of references to the current
 *        list. Any given list can be in a maximum of 65535 other lists.</li>
 * </ul>
 * <p>The SlipHeader object and all cells in the list are persistent. They are
 *    all from the AVSL and the all must be explicitely deleted. When the header
 *    a deleteList() is called and this is the last reference to the list, all
 *    cells are returned to the AVSL (see addAVSLCells). Actual recover of the
 *    delete cells, including the SlipHeadaer cell, occurs when a new cell is
 *    needed (see operator new). This latency between return of a cell to the 
 *    AVSL and the actual repurposing of the cell provides an opportunity for
 *    error. If the user maintains a reference to the deleted SlipHeader object
 *    the reference will appear valid until the SlipHeader cell is reused. This
 *    condition is explicitly checked for but the user should feel obligated
 *    to recognize when references become stale.</p>
 * <p>SlipHeader cells are list cells for use in iterators. An iterator will
 *    stop or continue through the list header depending of the operation being
 *    performed. see SlipReader and SlipSequencer.</p>
 * <p>A SlipHeader object can not be created on the stack (as a temporary). It
 *    must be created using <em>new</em> as in</p>
 *    <br />
 *    <tt>
 *       <b>SlipHeader *header = new SlipHeader();</b>, or
 *       <b>SlipHeader &header = *new SlipHeader();</b>
 *    </tt>
 * <p>The user has the option of making the SlipHeader object (and its list)
 *    eternally present. In effect this means that in order to ensure that a
 *    list is deleted the user must delete it twice (two deleteList()s). This
 *    selection occurs at header creation by:</p>
 *    <br />
 *    <tt>
 *       <b>SlipHeader *header = new SlipHeader(<em>true</em>);</b>, or
 *       <b>SlipHeader &header = *new SlipHeader(<em>true</em>);</b>
 *    </tt>
 * <p>Operations which are unique to a SlipHeader are:</p>
 * <ul>
 *    <li><b>Descriptor List</b> and associative list of &lt;key value&gt;
 *        pairs. Searches on a key will yield its value. Keys must be unique.
 *        Methods used are:</li>
 *    <ul>
 *       <li><b>appendDList</b> append an externally created description list 
 *           to a description. Create a new list if one doesn't exist;</li>
 *       <li><b>contains</b> return <b>true</b> if a value is found. If there
 *           are more values then the first one will terminate the search.</li>
 *       <li><b>containsKey</b> return <b>true</b> if a key is found. Keys
 *           must be unique.</li>
 *       <li><b>create_dList</b> create an empty Description List.</li>
 *       <li><b>deleteAttribute</b> Delete a &lt;key value &gt; pair.</li>
 *       <li><b>delete_dList</b> delete a Description List. This is analogous
 *           to deleteList().</li>
 *       <li><b>flush_dList</b> empty all &lt;key value&gt; pairs from the 
 *           Description List. This is analogous to flush().</li>
 *       <li><b>get</b> given a key return it's associated attribute. Keys are
 *           unique.</li>
 *       <li><b>getDList</b> return a reference to the Descriptor List
 *           header. This exposes the Descriptor List to the user. This 
 *           exposure is unsafe in that the user can manipulate the raw
 *           lists in ways to destroy the list integrity. If there is no
 *           Descriptor List the results are undefined.</li>
 *       <li><b>isDList</b> <b>true</b> if there is a Descriptor List.</li>
 *       <li><b>keys</b> return an array of keys</li>
 *       <li><b>dumpDList</b> dump (uglify) all &lt;key value&gt; pairs in the
 *           Description List.</li>
 *       <li><b>put</b> add a &lt;key value&gt; to the Description list.</li>
 *       <li><b>size_dList</b> number of &lt;key value&gt; pairs in the 
 *           Description List.</li>
 *    </ul>
 *    <li><b>dequeue</b> remove the last cell on the list.</li>
 *    <li><b>deleteList</b> delete the list. If the reference count is zero,
 *        return the SlipHeader object and all list cells to the AVSL.</li>
 *    <li><b>dumpList</b> uglify and print the list contents.</li>
 *    <li><b>enqueue</b> insert a new list cell onto the bottom of the list.</li>
 *    <li><b>flush</b> empty the list (and return the cells to the AVSL).</li>
 *    <li><b>getBot</b> return a reference to the last cell on the list.</li>
 *    <li><b>getMark</b> return the value of the list mark.</li>
 *    <li><b>getRefCount</b> return the number of list references there are. This
 *        is effected by the refcnt flag  setting at list creation.</li>
 *    <li><b>getTop</b> return a reference to the first cell on the list.</li>
 *    <li><b>isEmpty</b> <b>true</b> if the list is empty.</li>
 *    <li><b>isEqual</b> <b>true</b> if two lists are structurally equal.</li>
 *    <li><b>pop</b> remove the first cell on the list.</li>
 *    <li><b>push</b> insert a new list cell to the top of the list.</li>
 *    <li><b>putMark</b> put a user defined mark value into the list mark field.</li>
 *    <li><b>replaceBot</b> replace the list bottom cell with another one.</li>
 *    <li><b>replaceTop</b> replace the list top cell with another one.</li>
 *    <li><b>size</b> number of cells in the list. This is a top-level count and
 *        does not traverse nested lists.</li>
 *    <li><b>splitLeft</b> create a new list by splitting the current cell and
 *        all cells to the left. This may yield an empty list.</li>
 *    <li><b>splitRight</b> create a new list by splitting the current cell and
 *        all cells to the right. This may yield an empty list.</li>
 *    <li><b>write</b> output a list and preserve list reuse in sublilsts.</li>
 *    <li><b>writeQuick</b> output a list and do not preserve list reuse in
 *        sublists. The generated list can be input with the following proviso's:</li>
 *       <ul>
 *          <li>Sublists can not be reused. On input each sublist becomes a new 
 *              list.</li>
 *          <li>User Data classes are not declared. The user is responsible for
 *              registering User Data classes before input but there is non 
 *              checking to see if the classes have been declared.</li>
 *       </ul>
 *    <li><b>writeToString</b> direct the output of writeQuick to a string.</li>
 * </ul>
 * <p>Local implementation of methods inherited from SlipCell:</p>
 * <ul>
 *    <li><b>dump</b> uglify the SlipHeader object.</li>
 *    <li><b>operator=</b> copy the list (except the list header) from the
 *        right hand side (RHS) to the left hand side (LHS) of the assignment
 *        statement.</li>
 *    <li><b>operator==</b> compare the list object addresses for identity</li>
 *    <li><b>replace</b> illegal. It is illegal to replace the SlipHeader object.</li>
 * </ul>
 * 
 */

   class SlipHeader : public SlipCell {
   private:
       /*********************************************************
        *                   Private Functions                   *
       **********************************************************/
         SlipHeader(const SlipHeader& header) { };                              // Copy constructors not allowed
         void   constructHeader(bool refCnt = false);
         
         void   leave(SlipHeader* X);                                           // Exit a list visit
         void   dumpDescriptorList(ostream& out, int& col);                     // Output Descriptor List
         void   dumpHeadList(ostream& out, int& col); 
         void   dumpList(ostream& out, int& col);                               // Output the current Header and list constants
         void   visit(SlipHeader* X);                                           // Visit a list
         static void writeQuick(ostream& out, SlipHeader& list, int& col);      // Output the current list - quickly
         static bool isVisited(const SlipHeader& X);                            // Has a list been visited

       /*********************************************************
        *                  Protected Functions                  *
       **********************************************************/
   protected:

      virtual SlipCell& factory();                                              // Create a SlipCell

   public:
            SlipHeader(SlipHeader* header) {
               constructHeader();
               *this = *header;
            }

            SlipHeader(bool refCnt = false) { constructHeader(refCnt); }        // SlipHeader constructor

     virtual ~SlipHeader();                                                     // ~SlipHeader() destructor

     void deleteList();                                                         // destructor

      /*********************************************************
       *                   Public Functions                    *
      **********************************************************/

          /*************************************************
           *             Assignment operators              *
          *************************************************/
      virtual SlipCell&   operator=(const SlipCell& X);                         // Illegal for non-SlipHeader
      virtual SlipHeader& operator=(const SlipHeader& X);                       // Flush Y and copy X to Y
      virtual SlipHeader& operator=(const bool    X);                           // Illegal
      virtual SlipHeader& operator=(const CHAR    X);                           // Illegal
      virtual SlipHeader& operator=(const DOUBLE  X);                           // Illegal
      virtual SlipHeader& operator=(const LONG    X);                           // Illegal
      virtual SlipHeader& operator=(const UCHAR   X);                           // Illegal
      virtual SlipHeader& operator=(const ULONG   X);                           // Illegal
      virtual SlipHeader& operator=(const PTR     X);                           // Illegal
      virtual SlipHeader& operator=(const string& X);                           // Illegal
      virtual SlipHeader& operator=(const string* X);                           // Illegal

          /*************************************************
           *              Logical operators                *
          *************************************************/
      virtual bool       operator==(const SlipCell& X);                         // Y == X
              bool       operator==(const SlipHeader& X);                       // Y == X

               /*****************************************
                *            Descriptor Lists           *
               ******************************************/
      SlipHeader&       appendDList(SlipHeader& dList);                         // append a description list
      bool              contains(SlipCell& value);                              // returns true if the value is in the list
      bool              containsKey(SlipCell& key);                             // returns true if the key is in the list
      SlipHeader&       create_dList();                                         // create/change a descriptor list for this header
      bool              deleteAttribute(SlipCell& key);                         // delete attribute and value from list
      SlipHeader&       delete_dList();                                         // delete the descriptor list
      SlipHeader&       flush_dList();                                          // delete contents of a descriptor list
      SlipCell&         get(SlipCell& key);                                     // return value of an attribute
      SlipHeader&       getDList() const;                                       // return a reference to the Descriptor List header
      SlipCell*         keys();                                                 // returns an array of keys in the descriptor list
      void              dumpDList();                                            // dump the descriptor list
      SlipCell&         put(SlipCell& key, SlipCell& value);                    // add attribute to description list
      unsigned          size_dList();                                           // number of <key, value> pairs

               /*****************************************
                *           Manipulating Lists          *
               ******************************************/


      SlipCell&         dequeue();                                              // Remove and return list bottom

      SlipHeader&       enqueue(SlipCell& X);                                   // Insert X on bottom of list
      SlipHeader&       enqueue(bool X);                                        // Insert X on bottom of list
      SlipHeader&       enqueue(UCHAR X);                                       // Insert X on bottom of list
      SlipHeader&       enqueue(CHAR X);                                        // Insert X on bottom of list
      SlipHeader&       enqueue(ULONG X);                                       // Insert X on bottom of list
      SlipHeader&       enqueue(LONG X);                                        // Insert X on bottom of list
      SlipHeader&       enqueue(DOUBLE X);                                      // Insert X on bottom of list
      SlipHeader&       enqueue(const PTR X, const void* operation = ptrOP);    // Insert X on bottom of list
      SlipHeader&       enqueue(const string& X, bool constFlag = false);       // Insert X on bottom of list
      SlipHeader&       enqueue(const string* X, bool constFlag = false);       // Insert X on bottom of list
      SlipCell&         flush();                                                // Flush list 

      SlipCell&         getBot() const;                                         // Return pointer to list bottom
      SlipCell&         getTop() const;                                         // Return pointer to list top

      SlipCell&         pop();                                                  // Remove and return list top

      SlipHeader&       push(SlipCell& X);                                      // Insert X on top of list
      SlipHeader&       push(bool X);                                           // Insert X on top of list
      SlipHeader&       push(UCHAR X);                                          // Insert X on top of list
      SlipHeader&       push(CHAR X);                                           // Insert X on top of list
      SlipHeader&       push(ULONG X);                                          // Insert X on top of list
      SlipHeader&       push(LONG X);                                           // Insert X on top of list
      SlipHeader&       push(DOUBLE X);                                         // Insert X on top of list
      SlipHeader&       push(const PTR X, const void* operation = ptrOP);       // Insert X on top of list
      SlipHeader&       push(const string& X, bool constFlag = false);          // Insert x on top of list
      SlipHeader&       push(const string* X, bool constFlag = false);          // Insert X on top of list

      virtual SlipCell& replace(const SlipCell& X);                             // Illegal. List can not be replaced.
      virtual SlipCell& replace(bool X);                                        // Illegal. List can not be replaced.
      virtual SlipCell& replace(CHAR X);                                        // Illegal. List can not be replaced.
      virtual SlipCell& replace(UCHAR X);                                       // Illegal. List can not be replaced.
      virtual SlipCell& replace(LONG X);                                        // Illegal. List can not be replaced.
      virtual SlipCell& replace(ULONG X);                                       // Illegal. List can not be replaced.
      virtual SlipCell& replace(DOUBLE X);                                      // Illegal. List can not be replaced.
      virtual SlipCell& replace(const PTR X, const void* operation = ptrOP);    // Illegal. List can not be replaced.
      virtual SlipCell& replace(const string& X, bool constFlag = false);       // Illegal. List can not be replaced.
      virtual SlipCell& replace(const string* X, bool constFlag = false);       // Illegal. List can not be replaced.
     
      SlipCell&         replaceTop(SlipHeader& X);                              // Substitute list TOP value
      SlipCell&         replaceTop(SlipCell& X);                                // Substitute list TOP value
      SlipCell&         replaceBot(SlipHeader& X);                              // Substitute list BOT value
      SlipCell&         replaceBot(SlipCell& X);                                // Substitute list BOT value

      SlipCell&         replaceTop(bool X);                                     // Substitute list TOP value
      SlipCell&         replaceTop(UCHAR X);                                    // Substitute list TOP value
      SlipCell&         replaceTop(CHAR X);                                     // Substitute list TOP value
      SlipCell&         replaceTop(ULONG X);                                    // Substitute list TOP value
      SlipCell&         replaceTop(LONG X);                                     // Substitute list TOP value
      SlipCell&         replaceTop(DOUBLE X);                                   // Substitute list TOP value
      SlipCell&         replaceTop(const PTR X, const void* operation = ptrOP); // Substitute list TOP value
      SlipCell&         replaceTop(const string& X, bool constFlag = false);    // Substitute list TOP value
      SlipCell&         replaceTop(const string* X, bool constFlag = false);    // Substitute list TOP value

      SlipCell&         replaceBot(bool X);                                     // Substitute list BOT value
      SlipCell&         replaceBot(UCHAR X);                                    // Substitute list BOT value
      SlipCell&         replaceBot(CHAR X);                                     // Substitute list BOT value
      SlipCell&         replaceBot(ULONG X);                                    // Substitute list BOT value
      SlipCell&         replaceBot(LONG X);                                     // Substitute list BOT value
      SlipCell&         replaceBot(DOUBLE X);                                   // Substitute list BOT value
      SlipCell&         replaceBot(const PTR X, const void* operation = ptrOP); // Substitute list BOT value
      SlipCell&         replaceBot(const string& X, bool constFlag = false);    // Substitute list BOT value
      SlipCell&         replaceBot(const string* X, bool constFlag = false);    // Substitute list BOT value

      SlipHeader&       splitLeft(SlipCell&  Cell);                             // Split cells to left of X & X to a new list
      SlipHeader&       splitRight(SlipCell&  Cell);                            // Split cells to right of X & X to a new list

               /*****************************************
                *             Miscellaneous             *
               ******************************************/
      bool              isDList() const { return *(SlipHeader**)getHeadDescList() != NULL; } // true if there is a descriptor list
      bool              isEmpty() const { return (getRightLink() == this); }    // true if the list is empty
      bool              isEqual(const SlipHeader& Y) const;                     // true the the lists are structurally and data identical
      USHORT            getMark() const;                                        // value of list mark
      USHORT            getRefCount() const { return *(getHeadRefCnt()); }      // get the list reference count
      USHORT            putMark(const USHORT X);                                // Mark the list
      unsigned          size() const;                                           // Number of cells in list
      void              writeQuick();                                           // Output the current list - quickly
      void              writeQuick(ostream& out);                               // Output the current list - quickly
      void              writeQuick(string& filename);                           // Output the current list - quickly
      string            writeToString();                                        // Return an ASCII formated list as a string
      bool              write(bool testFlag = false);                           // Output the current list
      bool              write(ostream& out, bool testFlag = false);             // Output the current list
      bool              write(string& filename);                                // Output the current list

      virtual string    dump() const;                                           // Dump Header
      virtual void      dump(ostream& out, int& col);                           // Output the current cell
              void      dumpList();                                             // dump the current list

   }; // class SlipHeader
}; // namespace slip


#endif	/* SLIPHEADER_H */

