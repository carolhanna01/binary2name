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
 * @file   SlipSublist.h
 * @author A. Schwarz
 * @date   November 4, 2011
 *
 * @brief contains the SlipSublist declarations
 */

#ifndef SLIPSUBLIST_H
#define	SLIPSUBLIST_H

# include "SlipDef.h"
# include "SlipCell.h"
# include "SlipGlobal.h"

namespace slip {

/**
 * @class slip::SlipSublist
 * 
 * @brief A sublist is a list cell which provides a reference to a list.
 * 
 * <p>A sublist allows lists to be referenced within lists. It provides the
 *    basic functionality for constructing networks, acyclic graphs, and trees.
 * </p>
 * <p>A sublist cell is a reference to a SlipHeader object. This cell is 
 *    traversable using the Slip iterators, <em>SlipReader</em> and 
 *    <em>SlipSequencer</em>, and with some additional difficulty, by explicit
 *    user code.</p>
 * <p>A SlipSublist object can not be created on the stack. A SlipSublist object 
 *    must be created using <em>operator new</em>.</p>
 * <p>A SlipSublist object must be created referencing a list (SlipHeader object).
 *    At no time during processing can a Slipheader reference be <b>null</b> nor
 *    can it ever point to an object which is not a list. Operational errors which 
 *    can create a violation to this condition are given below.</p>
 * <p>In all other respects, a SlipSublist object is a SlipCell. It can be moved,
 *    inserted, and assigned to, and replaced. Other than SlipSublist constructors
 *    and destructors, the only method unique is the ability to get a reference
 *    to the sublist header.</p>
 * <p>The SlipSublist class provides all SlipHeader methods. In those cases 
 *     where a methods can apply to both a SlipHeader and a SlipSublist object, 
 *     the method will apply to the SlipSublist object. This includes all 
 *     <b>replace()</b> methods, and <b>dump()</b>.<p>
 * <p>If you delete the SlipSublist object, then the object is deleted and a
 *    <em>deleteList()</em> is executed against the referenced header.  If you 
 *    delete the header instead of deleting the SlipSublist object then the
 *    SlipSublist object will be out of synchrony with the header. Any of the 
 *    following conditions will (not may) occur:</p>
 *    <ul>
 *       <li>The SlipSublist header reference is stale and points to the AVSL..</li>
 *       <li>Another <em>deleteList()</em> operation causes the header to
 *           be restored to the AVSL.</li>
 *       <li>If the pointer is stale, then iterators (or iteration) will be
 *           to the AVSL, or</li>
 *       <li>If the header cell from the AVSL has been used to create another 
 *           SlipSublist or SlipDatum object (<em>operator new</em>) then 
 *           iterators (or  iteration) will be to objects on another list.</li>
 *       <li>If the header cell from the AVSLhas been used to create a SlipHeader 
 *           object (<em>operator new</em>) then iterators (or iteration) is to
 *           cells on another list and this may be undetectable.</li>
 *       <li>Ill effects are latent and may not be 'seen' for some time after
 *           the initial <em>deleteList()</em>. Ill effects are latent and may 
 *           not be 'seen' for some time after the initial <em>deleteList()</em> or 
 *           <em>delete SlipDatumObject</em>. </li>
 *       <li>The application is guaranteed an inexplicable and hard to track errors.</li>
 *    </ul>
 * <p>The refCnt field of the header is decremented on execution of a 
 *    <em>deleteList()</em>.If this count falls to zero then the header and all 
 *    list cells are restored to the AVSL. If the refCnt is not zero then at
 *    some unknown future time when a <em>delete (SlipSublist&)object</em> or
 *    <em>(SlipHeader&)header.deleteList()</em> the header and list cells will
 *    mysteriously disappear onto the AVSL.</p>
 * <p>The representation of a SlipSublist cell is:</p>
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
 *       <td colspan="8">rightLinkf</td>
 *    </tr>
 *    <tr>
 *       <td colspan="8">operator</td>
 *       <td colspan="8">header</td>
 *    </tr>
 *    <tr>
 *       <td colspan="8">unused</td>
 *    </tr>
 * </table>
 * <p>Where the field definitions are:</p>
 * <ul>
 *    <li><b>leftLink</b></li>
 *    <ul>
 *       <li>Pointer to the preceding list cell when in a list.</li>
 *       <li>Null when not in a list.</li>
 *       <li>TEMPORARY (0xFFFFFFFF) when on the stack.</li>
 *       <li>UNDEFDATA (0xDeadBeef) when in the available space list (AVSL).</li>
 *    </ul>
 *    <li><b>rightLink</b></li>
 *    <ul>
 *       <li>Pointer to the next list cell when in a list.</li>
 *       <li>Null when not in a list.</li>
 *       <li>TEMPORARY (0xFFFFFFFF) when on the stack.</li>
 *       <li>Pointer to the next list cell when in the available space list 
 *           (AVSL).</li>
 *    </ul>
 *    <li><b>operator</b> pointer to invariant properties of the Slip cell type.</li>
 *    <li><b>header</b> pointer to the embedded SlipHeader object.</li>
 * </ul>
 * <p><b>SlipSublist Operations</b></p><hr>
 * <p>The SlipSublist provides access to all SlipHeader operations which are
 *    not in conflict with SlipSublist operations.. The conflict is defined
 *    as all operations which can have a legitimate meaning for both
 *    SlipHeader objects and SlipSublist objects. In the case of a conflict,
 *    the interpretation is that the operation applies to SlipSublist
 *    objects and not SlipHeader objects.</p>
 * <p>The nature and meaning of each defined operation is defined in the
 *    following two sections:</p>
 * <ul>
 *    <li>SlipHeader Operations define all methods which apply to the referenced 
 *        SlipHeader object.</li>
 *    <li>SlipSublist Operations define all methods which apply to the current
 *        SlipSublist object.</li>
 * </ul>
 * <p>In all cases method calls are given as:</p>
 * <tt>
 *       SlipSublist obj(someHeader);
 *       obj.method();
 * </tt>
 * <p><b>SlipHeader Operations</b></p>
 * <p>The following operations apply to the referenced SlipHeader object. 
 *    The SlipSublist object which acts as a carrier to the reference is
 *    unaffected. Note that <em>deleteList()</em> is not allowed. To 
 *    delete a subordinated list either the SlipSublist object must be
 *    deleted, <b>delete obj</b>, or a replace or assign to the object
 *    must be done.</p>
 * <p>For a more thorough description of the operation of these methods 
 *    @see SlipHeader</p>
 * <ul>
 *    <li><b>Descriptor List</b> and associative list of &lt;key value&gt;
 *        pairs. Searches on a key will yield its value. Keys must be unique.
 *        Methods used are:</li>
 *    <ul>
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
 *       <li><b>isDList</b> <b>true</b> if there is a Descriptor List.</li>
 *       <li><b>keys</b> return an array of keys</li>
 *       <li><b>dumpDList</b> dump (uglify) all &lt;key valut&gt; pairs in the
 *           Description List.</li>
 *       <li><b>put</b> add a &lt;key value&gt; to the Description list.</li>
 *       <li><b>size_dList</b> number of &lt;key value&gt; pairs in the 
 *           Description List.</li>
 *    </ul>
 *    <li><b>dequeue</b> remove the last cell on the list.</li>
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
 * </ul>
 * <p><b>SlipSublist Operations</b></p>
 * <p> The methods available (and specific to SlipSublist) are:</p>
 * <ul>
 *    <li><b>getHeader</b> returns a reference to the include SlipHeader
 *        reference. This is the mechanism which allows public header methods to
 *        be used.</li>
 *    <li><b>move</b> move a SlipSublist object (hence, the subordinate list)
 *        from one list to another. The SlipSublist object must be in a list. 
 *        The command removes the subordinated list from its initial list and 
 *        inserts it into a new list (or new position in the same list.
 *        <p>For example, if we take the two lists below:<p>
 *        <tt>
 *           list1: (a b (c d) e f), and <br />
 *           list2: (1 2 3 4)<br />
 *        </tt>
 *        <p>and moveLeft the list <tt>(c d)</tt> for the list1 to the left of
 *           list2:'3', we get</p>
 *        <tt>
 *           list1: (a b e f), and <br />
 *           list2: (1 2 (c d) 3 4)<br /><br />
 *        </tt>
 *    </li>
 *    <li><b>moveList</b> move the list contents of a list. After the move the 
 *       source list is dry as a bone.</li>
 *    <li><b>insert</b> a SlipSublist object unlinked from a list, or created
 *        without explicit linkage, must be used. </li>
 *      <ul>
 *          <li>If it is linked a new SlipSublist object is created and linked.
 *              If it is unlinked then a new object is not created and the
 *              existing object is linked.</li>
 *            <p>A new SlipSublist object causes the reference count of the 
 *               list to be incremented. A copied SlipSublist object implies 
 *               multiple references to the same list.</p>
 *          <li>A temporary is never inserted into a list.</li>
 *       </ul>
 *    <li><b>replace</b> replace the object in a list by another object. Replace
 *        is not type invariant. If the right hand side (RHS) of the replace
 *        can not be cast into the left hand side (LHS) type, then replace
 *        deletes the LHS and creates a new SlipCell containing the RHS.</li>
 *        <p>The user can not depend on the pointers and references to the LHS
 *        remaining valid after the replace operation. The operation can only
 *        be performed when the LHS is linked into a list. The RHS may or may
 *        not be linked. The RHS does not change after the replace is complete.
 *        </p>
 *     <p>Replace acts like a grown up assignment statement. An assignment 
 *        statement replaces like-to-like, it is type invariant. Replace 
 *        replaces like-to-like and unlike-to-unlike, it is not type invariant.</p>
 *        <p>If the LHS is:</p>
 *        <ul>
 *          <li>SlipSublist object: If the RHS is a SlipHeader pointer or 
 *              reference or the RHS is a SlipSublist object, it is silently 
 *              cast to a  SlipHeader pointer and replaces the existing pointer.
 *              The new header refCnt field is incremented and the original 
 *              referenced list is deleted (<em>deleteList()</em>).</li>
 *           <p>If the RHS is a SlipDatum, or an object which can be converted
 *              into a SlipDatum, then the SlipSublist object is deleted
 *              (<em>delete (SlipSublist*)object</em>) and a new SlipDatum object
 *              created to replace the SlipSublist in the list.</p>
 *          <li>SlipDatum object: The object does not remain invariant under
 *              replacement.</li>
 *          <ul>
 *              <li>If the new datum is a SlipHeader reference, then the old
 *                  object is deleted (<em>delete (SlipDatum*)object</em>) and a
 *                  new SlipSublist object created with the SlipHeader reference
 *                  and inserted into the list. The header refCnt field is 
 *                  incremented.</li>
 *               <li>If the new datum is a SlipSublist reference, then the old
 *                   object is deleted (<em>delete (SlipDatum*)object</em>) and 
 *                   a new SlipSublist object created with the SlipSublist 
 *                   inserted into the list. The header refCnt field is 
 *                   incremented.</li>
 *          </ul>
 *       </ul>
 *    <li><b>operator=</b> The left hand side (LHS) is type invariant. If the
 *        right hand side (RHS) is not a SlipHeader or SlipSublist object, then
 *        the operation is illegal.</li>
 *    <li><b>operator==</b> a like-as-like operation. If the RHS is
 *        a SlipHeader or SlipSublist object then the address of the referenced
 *        header object and the current referenced header object are compared.
 *        If they are the same, then they are equal. In all other cases 
 *        <b>false</b> will be returned.</li>
 *    <li><b>unlink</b> unlink a list object from a list.</li>
 * </ul>
 * <center>
 * <table>
 *    <caption> Replace, Insert, Move Cross-Reference</caption>
 *    <tr>
 *       <th><b></b></th>
 *       <th><center><b>Replace</b></center></th>
 *       <th><center><b>Insert</b></center></th>
 *       <th><center><b>Move</b></center></th>
 *       <th><center><b>Description</b></center></th>
 *    </tr>
 *    <tr>
 *       <th align=left><b>Source is a Temporary</b></th>
 *       <td><center>Yes</center></td>
 *       <td><center>Yes</center></td>
 *       <td><center>No&nbsp;</center></td>
 *       <td>A copy will be created and inserted</td>
 *    </tr>
 *    <tr>
 *       <th align=left><b>Source is a primitive</b></th>
 *       <td><center>Yes</center></td>
 *       <td><center>Yes</center></td>
 *       <td><center>No&nbsp;</center></td>
 *       <td>A SlipDatum(value) object will be created for insert</td>
 *    </tr>
 *    <tr>
 *       <th align=left><b>Source in a List</b></th>
 *       <td><center>Yes</center></td>
 *       <td><center>No&nbsp;</center></td>
 *       <td><center>Yes</center></td>
 *       <td>Source cell in list</td>
 *    </tr>
 *    <tr>
 *       <th align=left><b>Source not in a List</b></th>
 *       <td><center>Yes</center></td>
 *       <td><center>Yes</center></td>
 *       <td><center>No&nbsp;</center></td>
 *       <td>Source cell is not in a list</td>
 *    </tr>
 *    <tr>
 *       <th align=left><b>Source Moved from List</b></th>
 *       <td><center>No&nbsp;</center></td>
 *       <td><center>No&nbsp;</center></td>
 *       <td><center>Yes</center></td>
 *       <td>Source cell moved to a new list</td>
 *    </tr>
 *    <tr>
 *       <th align=left><b>Source is a List</b></th>
 *       <td><center>Yes</center></td>
 *       <td><center>Yes</center></td>
 *       <td><center>Yes</center></td>
 *       <td>Sublist created &amp; header reference count incremented.<br />
 *           Move will flush the old list after the move.</td>
 *    </tr>
 *    <tr>
 *       <th align=left><b>Source is a Sublist</b></th>
 *       <td><center>Yes</center></td>
 *       <td><center>Yes</center></td>
 *       <td><center>yes</center></td>
 *       <td>Header reference count not incremented for Move</td>
 *    </tr>
 *    <tr>
 *       <th align=left><b>Destination is a list</b></th>
 *       <td><center>No&nbsp;</center></td>
 *       <td><center>Yes</center></td>
 *       <td><center>Yes</center></td>
 *       <td>Only for move &amp; insert Left/Right</td>
 *    </tr>
 *    <tr>
 *       <th align=left><b>Destination a Sublist</b></th>
 *       <td><center>Yes</center></td>
 *       <td><center>Yes</center></td>
 *       <td><center>Yes</center></td>
 *       <td>For Replace, Sublist cell deleted and reference count of header decremented</td>
 *    </tr>
 *    <tr>
 *       <th align=left><b>Destination a TEMPORARY</b></th>
 *       <td><center>Yes</center></td>
 *       <td><center>No&nbsp;</center></td>
 *       <td><center>No&nbsp;</center></td>
 *       <td>Only available for datum. Better to use '='</td>
 *    </tr>
 *    <tr>
 *       <th align=left><b>Return value Destination cell</b></th>
 *       <td><center>Yes</center></td>
 *       <td><center>No&nbsp;</center></td>
 *       <td><center>No&nbsp;</center></td>
 *       <td>Return value original destination value</td>
 *    </tr>
 *    <tr>
 *       <th align=left><b>Return value Source cell</b></th>
 *       <td><center>No&nbsp;</center></td>
 *       <td><center>Yes</center></td>
 *       <td><center>Yes</center></td>
 *       <td>Return valuet he input source value</td>
 *    </tr>
 * </table>
 * </center>
 * <p>Some convenience methods are included. These allow reference to header
 *    methods as if they were native to the SlipSublist class.</p>
 * <ul>
 *    <li><b>getMark</b> return the value of the list mark.</li>
 *    <li><b>getRefCount</b> return the number of list references there are. This
 *        is effected by the refcnt flag  setting at list creation.</li>
 *    <li><b>isDList</b> <b>true</b> if there is a Descriptor List.</li>
 *    <li><b>isEmpty</b> <b>true</b> if the list is empty.</li>
 *    <li><b>isEqual</b> <b>true</b> if two lists are structurally equal.</li>
 *    <li><b>putMark</b> put a user defined mark value into the list mark field.</li>
 *    <li><b>size</b> number of cells in the list. This is a top-level count and
 *        does not traverse nested lists.</li>
 * </ul>
 * 
 */
   class SlipHeader;
    
   class SlipSublist : public SlipCell {
       /*********************************************************
        *                   Private Functions                   *
       **********************************************************/
   private:
      SlipSublist(const SlipSublist& sublist) { };                              // Copy constructors not allowed

      void constructSublist() {                                                 // Complete creating a sublist
         setLeftLink(NULL);
         setRightLink(NULL);
         setOperator((void*)sublistOP);
      }
      void constructSublist(SlipHeader& X);                                     // Complete creating a sublist

       /*********************************************************
        *                  Protected Functions                  *
       **********************************************************/
   protected:

      virtual SlipCell& factory();                                              // Create a SlipCell

          /*************************************************
           *           Constructors & Destructors          *
           ************************************************/

   public:
      /*********************************************************
       *                   Public Functions                    *
      **********************************************************/

          /*************************************************
           *           Constructors & Destructors          *
           ************************************************/
      SlipSublist(SlipHeader& X)  { constructSublist(X); }
      SlipSublist(SlipSublist& X) { constructSublist(*(SlipHeader*)*getSublistHeader(X)); }
      SlipSublist(SlipCell& X);
      virtual ~SlipSublist();

          /*************************************************
           *             Assignment operators              *
          *************************************************/
      virtual SlipCell&    operator=(const SlipCell& X);                        // Y = X
              SlipSublist& operator=(const SlipSublist& X);                     // Y = X
      virtual SlipSublist& operator=(const bool    X);                          // Y = V
      virtual SlipSublist& operator=(const CHAR    X);                          // Y = V
      virtual SlipSublist& operator=(const DOUBLE  X);                          // Y = V
      virtual SlipSublist& operator=(const LONG    X);                          // Y = V
      virtual SlipSublist& operator=(const UCHAR   X);                          // Y = V
      virtual SlipSublist& operator=(const ULONG   X);                          // Y = V
      virtual SlipSublist& operator=(const PTR     X);                          // Y = V
      virtual SlipSublist& operator=(const string& X);                          // Y = V
      virtual SlipSublist& operator=(const string* X);                          // Y = V

          /*************************************************
           *              Logical operators                *
          *************************************************/
      virtual bool       operator==(const SlipCell& X);                         // Y == X

                  /*****************************************
                   *          Manipulating Lists           *
                  ******************************************/
      virtual SlipCell& replace(const SlipCell& X);                             // Replace a cell on a list with a new cell
      virtual SlipCell& replace(bool X);                                        // Replace a cell on a list with a new cell
      virtual SlipCell& replace(CHAR X);                                        // Replace a cell on a list with a new cell
      virtual SlipCell& replace(UCHAR X);                                       // Replace a cell on a list with a new cell
      virtual SlipCell& replace(LONG X);                                        // Replace a cell on a list with a new cell
      virtual SlipCell& replace(ULONG X);                                       // Replace a cell on a list with a new cell
      virtual SlipCell& replace(DOUBLE X);                                      // Replace a cell on a list with a new cell
      virtual SlipCell& replace(const PTR X, const void* operation = ptrOP);    // Replace a cell on a list with a new cell
      virtual SlipCell& replace(const string& X, bool constFlag = false);       // Replace a cell on a list with a new cell
      virtual SlipCell& replace(const string* X, bool constFlag = false);       // Replace a cell on a list with a new cell

               /*****************************************
                *            Descriptor Lists           *
               ******************************************/
      bool              contains(SlipCell& value) { return getHeader().contains(value);   } // returns true if the value is in the list
      bool              containsKey(SlipCell& key){ return getHeader().containsKey(key);  } // returns true if the key is in the list
      SlipHeader&       create_dList(){           { return getHeader().create_dList(); }  } // create/change a descriptor list for this header
      bool              deleteAttribute(SlipCell& key) { return getHeader().deleteAttribute(key); } // delete attribute and value from list
      SlipHeader&       delete_dList()            { return getHeader().delete_dList(); }    // delete the descriptor list
      SlipHeader&       flush_dList()             { return getHeader().flush_dList();  }    // delete contents of a descriptor list
      SlipCell&         get(SlipCell& key)        { return getHeader().get(key);       }    // return value of an attribute
      SlipCell*         keys()                    { return getHeader().keys();         }    // returns an array of keys in the descriptor list
      void              dumpDList()               { return getHeader().dumpDList();    }    // dump the descriptor list
      SlipCell&         put(SlipCell& key, SlipCell& value) { return getHeader().put(key, value); }// add attribute to description list
      unsigned          size_dList()              { return getHeader().size_dList();   }   // number of <key, value> pairs

               /*****************************************
                *      Header Manipulating Lists      *
               ******************************************/

      SlipCell&         dequeue()            { return getHeader().dequeue();  } // Remove and return list bottom

      SlipHeader&       enqueue(SlipCell& X) { return getHeader().enqueue(X); } // Insert X on bottom of list
      SlipHeader&       enqueue(bool X)      { return getHeader().enqueue(X); } // Insert X on bottom of list
      SlipHeader&       enqueue(UCHAR X)     { return getHeader().enqueue(X); } // Insert X on bottom of list
      SlipHeader&       enqueue(CHAR X)      { return getHeader().enqueue(X); } // Insert X on bottom of list
      SlipHeader&       enqueue(ULONG X)     { return getHeader().enqueue(X); } // Insert X on bottom of list
      SlipHeader&       enqueue(LONG X)      { return getHeader().enqueue(X); } // Insert X on bottom of list
      SlipHeader&       enqueue(DOUBLE X)    { return getHeader().enqueue(X); } // Insert X on bottom of list
      SlipHeader&       enqueue(const PTR X, const void* operation = ptrOP)     // Insert X on bottom of list
                                             { return getHeader().enqueue(X, operation); }
      SlipHeader&       enqueue(const string& X, bool constFlag = false)        // Insert X on bottom of list
                                             { return getHeader().enqueue(X, constFlag); }
      SlipHeader&       enqueue(const string* X, bool constFlag = false)        // Insert X on bottom of list
                                             { return getHeader().enqueue(X, constFlag); }
      SlipCell&         flush()              { return getHeader().flush(); }    // Flush list 

      SlipCell&         getBot() const       { return getHeader().getBot(); }   // Return pointer to list bottom
      SlipCell&         getTop() const       { return getHeader().getTop(); }   // Return pointer to list top

      SlipCell&         pop()                { return getHeader().pop(); }      // Remove and return list top

      SlipHeader&       push(SlipCell& X)    { return getHeader().push(X); }    // Insert X on top of list
      SlipHeader&       push(bool X)         { return getHeader().push(X); }    // Insert X on top of list
      SlipHeader&       push(UCHAR X)        { return getHeader().push(X); }    // Insert X on top of list
      SlipHeader&       push(CHAR X)         { return getHeader().push(X); }    // Insert X on top of list
      SlipHeader&       push(ULONG X)        { return getHeader().push(X); }    // Insert X on top of list
      SlipHeader&       push(LONG X)         { return getHeader().push(X); }    // Insert X on top of list
      SlipHeader&       push(DOUBLE X)       { return getHeader().push(X); }    // Insert X on top of list
      SlipHeader&       push(const PTR X, const void* operation = ptrOP)        // Insert X on top of list
                                             { return getHeader().push(X, operation); }
      SlipHeader&       push(const string& X, bool constFlag = false)           // Insert x on top of list
                                             { return getHeader().push(X, constFlag); }
      SlipHeader&       push(const string* X, bool constFlag = false)           // Insert X on top of list
                                             { return getHeader().push(X, constFlag); }
     
      SlipCell&         replaceTop(SlipHeader& X) { return getHeader().replaceTop(X); }  // Substitute list TOP value
      SlipCell&         replaceTop(SlipCell& X)   { return getHeader().replaceTop(X); }  // Substitute list TOP value
      SlipCell&         replaceBot(SlipHeader& X) { return getHeader().replaceBot(X); }  // Substitute list BOT value
      SlipCell&         replaceBot(SlipCell& X)   { return getHeader().replaceBot(X); }  // Substitute list BOT value

      SlipCell&         replaceTop(bool X)         { return getHeader().replaceTop(X); } // Substitute list TOP value
      SlipCell&         replaceTop(UCHAR X)        { return getHeader().replaceTop(X); } // Substitute list TOP value
      SlipCell&         replaceTop(CHAR X)         { return getHeader().replaceTop(X); } // Substitute list TOP value
      SlipCell&         replaceTop(ULONG X)        { return getHeader().replaceTop(X); } // Substitute list TOP value
      SlipCell&         replaceTop(LONG X)         { return getHeader().replaceTop(X); } // Substitute list TOP value
      SlipCell&         replaceTop(DOUBLE X)       { return getHeader().replaceTop(X); } // Substitute list TOP value
      SlipCell&         replaceTop(const PTR X, const void* operation = ptrOP)           // Substitute list TOP value
                                                   { return getHeader().replaceTop(X, operation); }
      SlipCell&         replaceTop(const string& X, bool constFlag = false)              // Substitute list TOP value
                                                   { return getHeader().replaceTop(X, constFlag); }
      SlipCell&         replaceTop(const string* X, bool constFlag = false)             // Substitute list TOP value
                                                   { return getHeader().replaceTop(X, constFlag); }

      SlipCell&         replaceBot(bool X)         { return getHeader().replaceBot(X); } // Substitute list TOP value
      SlipCell&         replaceBot(UCHAR X)        { return getHeader().replaceBot(X); } // Substitute list TOP value
      SlipCell&         replaceBot(CHAR X)         { return getHeader().replaceBot(X); } // Substitute list TOP value
      SlipCell&         replaceBot(ULONG X)        { return getHeader().replaceBot(X); } // Substitute list TOP value
      SlipCell&         replaceBot(LONG X)         { return getHeader().replaceBot(X); } // Substitute list TOP value
      SlipCell&         replaceBot(DOUBLE X)       { return getHeader().replaceBot(X); } // Substitute list TOP value
      SlipCell&         replaceBot(const PTR X, const void* operation = ptrOP)           // Substitute list TOP value
                                                   { return getHeader().replaceBot(X, operation); }
      SlipCell&         replaceBot(const string& X, bool constFlag = false)              // Substitute list TOP value
                                                   { return getHeader().replaceBot(X, constFlag); }
      SlipCell&         replaceBot(const string* X, bool constFlag = false)             // Substitute list TOP value
                                                   { return getHeader().replaceBot(X, constFlag); }

      SlipHeader&       splitLeft(SlipCell&  X)    { return getHeader().splitLeft(X); } // Split cells to left of X & X to a new list
      SlipHeader&       splitRight(SlipCell&  X)   { return getHeader().splitRight(X);} // Split cells to right of X & X to a new list

                  /*****************************************
                   *     Header Miscellaneous Methods      *
                  ******************************************/
      bool              isDList() const { return (*(SlipHeader**)getSublistHeader())->isDList(); }  // Does the header have a description list
      bool              isEmpty() const { return (*(SlipHeader**)getSublistHeader())->isEmpty(); }  // Is the list empty
      bool              isEqual(const SlipCell& X) const { return (*(SlipHeader**)getSublistHeader())->isEqual((SlipHeader&)X); }// true the lists are structurally identical
      USHORT            getMark() const { return (*(SlipHeader**)getSublistHeader())->getMark(); }  // value of list mark
      USHORT            getRefCount() const { return (*(SlipHeader**)getSublistHeader())->getRefCount(); } // get list reference count
      USHORT            putMark(const USHORT X) { return (*(SlipHeader**)getSublistHeader())->putMark(X); } // Mark the list
      unsigned          size() const   { return (*(SlipHeader**)getSublistHeader())->size(); }      // Number of cells in list
      SlipHeader&       getHeader() const       { return (**(SlipHeader**)getSublistHeader()); }    // Return a SlipHeader reference
      void              dumpList() {             { getHeader().dumpList(); } }  // dump the current list
      
               /*****************************************
                *             Miscellaneous             *
               ******************************************/
      virtual string    dump() const;                                           // pretty print SLIP Header cell
      virtual void      dump(ostream& out, int& col);                           // Output the current cell

   }; // class SlipSublist
}; // namespace slip
#endif	/* SLIPSUBLIST_H */

