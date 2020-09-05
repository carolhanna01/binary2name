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
 * @file   SlipReaderCell.h
 * @author A. Schwarz
 * @date   July 18, 2012
 *
 * @brief contains the SlipCell declarations
 */

#ifndef SLIPREADERCELL_H
#define	SLIPREADERCELL_H

# include <string>
# include "SlipDef.h"
# include "SlipGlobal.h"
# include "SlipCellBase.h"
# include "SlipCell.h"
# include "SlipDatum.h"
# include "SlipGlobal.h"
# include "SlipHeader.h"
namespace slip {
   
/**
 * @class slip::SlipReaderCell
 * 
 * @brief Implementation of the structured iterator for SLIP.
 * 
 * <p><b>Overview</b></p><hr>
 * 
 * <p>There are two types of iterators in SLIP, a structured iterator and a
 *    linear iterator. The structured iterator provides a means to traverse 
 *    sublists (lists contained with lists) and a memory to enable return
 *    to a containing list. The linear iterator provides a means to travers
 *    sublists but no means to return to a containing list.</p>
 * <p>This is the implementation code necessary to support a structured
 *    reader. The user interface (API) is given in SlipReader. This code
 *    provides support of list iteration. The user interface hides the
 *    nasties (the dynamic nature of the reader cell bottom) from the
 *    user and provides other convenience methods. The user should not
 *    ever use this class.</p>
 * <p>The structured iterator can go left or right within a list, down to
 *    an included list, return to the preceding level or to the topmost
 *    level in a list.</p>
 * <p>The list structure contains a pointer to the current list header,
 *    a pointer to the current cell being traversed, a pointer to the
 *    preceeding reader cell representing the list containing the current
 *    list, and the list depth. The topmost reader cell has the preceeding 
 *    list pointer set to null ad a list depth of zero.</p>
 * <br />
 *  <table>
 *    <caption align = "top"><b>SlipReaderCell Structure</b></caption>
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
 *       <td colspan="8">current cell</td>
 *       <td colspan="8">previous reader</td>
 *    </tr>
 *    <tr>
 *       <td colspan="8">operator</td>
 *       <td colspan="8">current list</td>
 *    </tr>
 *    <tr>
 *       <td colspan="4">depth</td>
 *       <td colspan="4">not used</td>
 *    </tr>
 * </table>
 * <br />
 * The field description is:
 * <ul>
 *    <li><b>current cell</b> pointer to the current cell being traversed.</li>
 *    <li><b>previous reader</b> pointer to the previous reader cell on the 
 *        reader cell chain or null if this is the top of the chain.</li>
 *    <li><b>operator</b> pointer to a SlipOp derived class (SlipHeaderOp)
 *        containing the invariant properties shared by every SlipHeader object.
 *        </li>
 *    <li><b>current list</b> pointer to the header cell of the current list 
 *        being traversed.</li> 
 *    <li><b>depth</b> number of lists (reader cells) traversed. Zero (0) if
 *        this is the topmost reader cell.</li>
 *    <li><b>not used.</b></li>
 * </ul>
 * <p>Construction of a SlipReader causes the SlipReader to create a
 *    SlipReaderCell passing the input SlipHeader object to the constructor.
 *    The SlipReaderCell takes space from the AVSL and creates a new
 *    SlipReaderCell with a depth of zero (0), the previous reader pointer
 *    set to null, and the current cell and current list both pointing
 *    to the input SlipHeader object. A pointer to the constructed SlipReaderCell
 *    object is passed to the SlipReader constructor.</p>
 *  <table>
 *    <caption align = "top"><b>SlipReaderCell Structure</b></caption>
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
 *       <td colspan="8">SlipHeader object</td>
 *       <td colspan="8">null</td>
 *    </tr>
 *    <tr>
 *       <td colspan="8">operator</td>
 *       <td colspan="8">SlipHeader object</td>
 *    </tr>
 *    <tr>
 *       <td colspan="4"><center>0</center></td>
 *       <td colspan="4">not used</td>
 *    </tr>
 * </table>
 * <br />
 * <p>If during iteration a new list is entered then a new SlipReaderCell is
 *    created with the depth field incremented by one, the previous pointer
 *    set to the current SlipReaderCell, and the current list and the current
 *    cell both pointing to the SlipHeader object defining the new list. A 
 *    pointer to the constructed SlipReaderCell object is returned to the
 *    invoking SlipReader method.</p>
 * <p>If during iteration a list is exited, i.e., iteration proceeds in the
 *    list containing the current list, then the current SlipReaderCell object
 *    is deleted and a pointer returned to the SlipReader invoking method to
 *    the previous (upper) list SlipReaderCell object.</p>
 * <p>The topmost list can not be 'exited'. If the user is iterating the
 *    topmost list, then short of deleting the SlipReader, there is no
 *    way to exit it. Once at the top, always at the top.</p>
 * <p>When the SlipReader object is deleted, then the SlipReaderCell is 
 *    deleted and all SlipReaderCell objects are returned to the AVSL. If
 *    the user is at the topmost list (depth = 0) then only one cell is
 *    deleted. If the user is iterating at some nested level (depth > 0)
 *    then <em>depth + 1</em> SlipReaderCell objects are returned.</p>
 * <p>Note that on initial creation of a SlipReader object a SlipReaderCell
 *    object is created and its pointer returned to the SlipReader constructor.
 *    On successive iterations into a nested list, a SlipReaderCell object is 
 *    created automatically and its pointer is returned to the SlipReader
 *    invoking method. On 'exit' from a list to its containing list, a 
 *    SlipReaderCell object is automatically deleted and a pointer to the
 *    new 'bottom' of the SlipReaderCell chain is returned to the invoking
 *    method.</p>
 * <p>If this all looks like recursive descent over a graph, why, it is.
 *    The SlipReader iteration method replaces recursive entry into a
 *    method by chaining SlipReaderCell objects together. The effect is
 *    the same but the mechanisms are different. This 'linearizes' the
 *    process of recursion (with some improvements to stack usage if not
 *    speed) and allows the user to perform linear processing on a 
 *    recursive problem.</p>
 * <p>The methods in the SlipReaderCell class can be divided into:</p>
 * <ul>
 *    <li><b>Constructors/Destructors</b> create or destroy a SlipReaderCell
 *        object.</li>
 *    <li><b>Utility</b> support methods which are used to enable other
 *        methods to fulfill their lawful obligations.</li>
 *    <li><b>Iteration</b> methods called by the SlipReader methods
 *        supporting iteration.</li>
 *    <li><b>Convenience</b> methods called by the SlipReader methods
 *        provided as a convenience to the user.</li>
 *    <li><b>Miscellaneous</b> methods of random purpose.</li>
 * </ul>
 * 
 * <p><b>Constructors/Destructors</b></p><hr>
 * <p>There are two constructors and there aare two distinct uses:</p>
 * <ul>
 *    <li><b>SlipReaderCell()</b> used internally to create a SlipReaderCell
 *        to be linked to the current reader chain. This cell is created 
 *        during a downlink to support iteration over a nested list and
 *        return from the list to the previous list.</li>
 *    <li><b>SlipReaderCell(SlipHeader*)</b> this is from the user interface
 *        SlipReader class and only used on SlipReader creation. It creates
 *        the top SlipReaderCell and references the top list iterated over.</li>
 * </ul>
 * <p>The destructors are"</p>: 
 * <ul>
 *    <li><b>~SlipReaderCell()</b> when the iterator is deleted (<em>SlipReader</em>).
 *         All SlipReaderCells on the reader chain are put into the AVSL.</p>
 *    <li><b>upLevel()</b> anytime the the user or the iterator methods want
 *        to return to a previous level (the parent of the current list). If the
 *        the current depth (list level) is the topmost list, then the request
 *        is ignored. You can't go higher than the top.</li>
 * </ul>
 * 
 * <p><b>Utility</b></p>
 * <p>Utility functions support the SlipReaderCells in doing their work. They
 *    are not called by the user interface.</p>
 * <ul>
 *    <li><b>downLevel()</b> Recurse into a list.</li>
 *       <ol>
 *          <li>Create a new SlipReaderCell.</li>
 *          <li>Insert the new SlipReaderCell into the current ReaderCell chain.</li>
 *          <li>Setup the list depth, current header, current cell in the new
 *              SlipReaderCell.</li>
 *       </ol>
 *    <li><b>getLink()</b> Return the back link to the previous SlipReaderCell</li>
 *    <li><b>init()</b> Initialize a SlipReaderCell object</li>
 *    <li><b>setCurrentCell()</b> Set a pointer to the current SlipCell object
 *        in the current SlipReaderCell object.</li>
 *    <li><b>setLink()</b> Set the SlipReaderCell back link pointer in the current
 *        SlipReaderCell object.</li>
 *    <li><b>setLink()</b> Set the back link pointer in the current SlipReaderCell
 *        object.</li>
 *    <li><b>setListDepth()</b> Set the SlipReaderCell chain depth in the current
 *        SlilpReaderCell object.</li>
 *    <li><b>setListHeader()</b> Set a SlipHeader object pointer in the currenct
 *        SlipReaderCell object</li>
 *    <li><b>setup()</b> Create a SlipReaderCell chain.</li>
 *    <li><b>SlipReaderCell()</b> Construct a SlipReaderCell object (from the
 *        SLIP AVSL</li>
 * </ul>
 * 
 * <p><b>Iteration</b></p>
 * <p>Iteration directly support structured iteration of a list. The iteration
 *    methods descend into sublists and ascend from sublists when structured
 *    advances are performed, and stay with the current sublist, if non-structured
 *    iteration methods are called. The user interface (SlipReader) is shielded
 *    from messy internal processing. The SlipReader iterator vies iteration at
 *    the functional level and not the operational level.</p>
 * <p>The method names are the same as those in the SlipReader class, and
 *    perform the mechanics of the functional operation. The SlipReader class
 *    has a shallow interface to the operational methods.</p>
 * <p>The nomenclature for the iteration methods is:</p>
 * <tt><pre>
 *    advanceTxD
 *    |      |||
 *    |      ||o-- Direction [L:left R:right]
 *    |      ||
 *    |      |o-- Search Type [E: data N: Sublist W: E or N]
 *    |      |
 *    |      o-- Iteration Type [S: structured L: linear]
 *    |
 *    o-- iteration specifier
 * </pre></tt>
 * Where:
 * <ul style="list-style-type:none">
 *    <li><b>advance</b> specifies iteration.</li>
 *    <li><b>Iteration Type</b> either descend (S) or do not descend into a list.</li>
 *    <li><b>Search Type</b> stop if an item of the search class is seen. If
 *        this is a structured search, then the search will descend to the lowest
 *        indentured list searching, and if not found, ascend. This is recursive
 *        and is a depth-first search. If the search is not successful, then the 
 *        search will terminate on the topmost list header.
 *        <br />
 *        Linear searches will not descend into subordinate lists or ascend from
 *        a subordinate list. If the selected item of the search class is not
 *        found, the search will terminate on the current list header.</li>
 *        <br />
 *        The SlipReaderCell object will be set to the current list and the
 *        current cell. If the search is successful, the current cell will be
 *        either a sublist object or SlipDatum object. If the search is unsuccessful
 *        then the current cell will be the list header (the current cell and 
 *        current header will be the same.)
 *    <li><b>Search Type</b> the search will be successful if a sublist (N) or 
 *        a SlipDatum object (E) or either a sublist or SlipDatum object (W)) is
 *        found. </li>
 *    <li><b>Direction</b> direction of the search. Search previous items (L)
 *        or successor items (R). That is, search to the Left or Right.</li>
 * </ul>
 * <p>At search end the current SlipReaderCell object will be in a consistent 
 *    state. The depth will represent the current depth of the search, the 
 *    current list will point to the list header of the current list, and the 
 *    current cell will point to the cell that the list terminated on.</p>
 * <p>The supported search methods are:</p>
 * <ul>
 *    <li><b>advanceLEL()</b> perform a linear left search for a SlipDatum 
 *        object, Stop the search at the list header of the current list for 
 *        failure or a SlipDatum object if successful.</li>
 *    <li><b>advanceLER()</b> perform a linear right search for a SlipDatum 
 *        object, Stop the search at the list header of the current list for 
 *        failure or a SlipDatum object if successful.</li>
 *    <li><b>advanceLNL()</b> perform a linear left search for a sublist 
 *        object, Stop the search at the list header of the current list for 
 *        failure or a sublist object if successful.</li>
 *    <li><b>advanceLNR()</b> perform a linear right search for a sublist 
 *        object, Stop the search at the list header of the current list for 
 *        failure or a sublist object if successful.</li>
 *    <li><b>advanceLWL()</b> perform a linear left search for a SlipDatum 
 *        object or sublist object, Stop the search at the list header of the
 *        current list for failure or a SlipDatum or sublist object if 
 *        successful.</li>
 *    <li><b>advanceLWR()</b> perform a linear right search for a SlipDatum 
 *        object or sublist object, Stop the search at the list header of the
 *        current list for failure or a SlipDatum or sublist object if 
 *        successful.</li></li>
 *    <li><b>advanceSEL()</b> perform a depth first left search for a SlipDatum 
 *        object, Stop the search at the list header of the topmost list for 
 *        failure or a SlipDatum object if successful.</li>
 *    <li><b>advanceSER()</b> perform a depth first right search for a SlipDatum 
 *        object, Stop the search at the list header of the topmost list for 
 *        failure or a SlipDatum object if successful.</li>
 *    <li><b>advanceSNL()</b> perform a depth first left search for a sublist 
 *        object, Stop the search at the list header of the topmost list for 
 *        failure or a SlipDatum object if successful.</li>
 *    <li><b>advanceSNR()</b> perform a depth first right search for a sublist 
 *        object, Stop the search at the list header of the topmost list for 
 *        failure or a SlipDatum object if successful.</li>
 *    <li><b>advanceSWL()</b> perform a depth first left search for a sublist 
 *        or SlipDatum object, Stop the search at the list header of the topmost 
 *        list for failure or a SlipDatum object if successful.</li>
 *    <li><b>advanceSWR()</b> perform a depth first right  search for a sublist 
 *        or SlipDatum object, Stop the search at the list header of the topmost 
 *        list for failure or a SlipDatum object if successful.</li>
 * </ul>
 * 
 * <p><b>Convenience</b></p><hr>
 * <p>The replace methods are provided as a convenience to the SlipReader cell
 *    (and to reduce the interrelationship between classes). The umbrella
 *    formed by being a derived class of SlipCellBase allows the method to
 *    complete processing without additional 'hardware', links, friends,
 *    and some enemies.</p>
 * <p>The method 'replaces' the object pointed to by the current cell and
 *    updates the current cell pointer. The semantics of operations are the 
 *    same as documented in the SlipDatum, SlipHeader, and SlipSublist classes 
 *    respectively. </p>
 * <p><em>replace</em> is a cell centric operation. When it terminates 
 *    successfully, the current cell pointer in the current SlipReadeCell 
 *    object will be set to the new object.</p>
 * <p><em>replaceTop</em> and <em>replaceBot</em> are list based operations.
 *    If the list top/bot is not the current cell then at termination, the
 *    current cell pointer is unaffected. If the list bot/top is the current
 *    cell, then at termination the current cell pointerd will be changed to
 *    its new value.</p>
 * <p>At the end of the operation, the SlipReaderCell object is valid. The
 *    depth and current list pointer are unchanged. The the current cell 
 *    pointer is changed only if the operation affects the current cell.</p>
 * <p>In summary:</p>
 * <ul style="list-style-type:none">
 *    <li><b>SlipHeader</b> replaces are not allowed. A nasty message will be
 *        output.</li>
 *    <li><b>like-to-like</b> sublist-to-sublist, header-to-sublist,
 *        and SlipDatum-to-SlipDatum replaces are valid.</li>
 *    <li><b>unlike-to-unlike</b> sublist-to-SlipDatum is valid.</li>
 *    <li><b>SlipReaderCell object</b> is valid at operation 
 *        termination with the depth and current list pointer unchanged, and
 *        the current cell pointer changed only if the operation affects the
 *        current cell.</li>
 * </ul>
 * <p>The replace functions are:</p>
 * <ul>
 *    <li><b>replace()</b> this is a cell-centric operation The current cell
 *        is replaced.</li>
 *    <li><b>replaceBot()</b> this is a list-centric operation. The current
 *        list bottom cell is replaced. If the bottom cell is the current cell,
 *        then the current cell pointer is changed.</li>
 *    <li><b>replaceTop()</b> this is a list-centric operation. The current
 *        list top cell is replaced. If the top cell is the current cell,
 *        then the current cell pointer is changed.</li>
 * </ul>
 * 
 * <p><b>Miscellaneous</b></p><hr>
 * <p>A collection of methods which have no underlying consistent definition.
 *    They are provided to allow consistent functioning of the SlipReader 
 *    user interface in providing a service to the user.</p>
 * <p>At termination of each operation, the SlipReaderCell object is in a 
 *    consistent state.</p>
 * <ul>
 *    <li><b>operator=()</b> Reset the SlipReaderCell chain to the input
 *        SlipReaderCell. Return the chain to the AVSL and set the current
 *        SlipReaderCell chain top to the input (RHS) SlipReaderCell list and
 *        cell pointers.</li>
 *    <li><b>copy()</b> Copy the SlipReaderCell chain.</li>
 *    <li><b>copyCell()</b> Return a copy of hte current SlipCell object.</li>
 *    <li><b>currentCell()</b> Return a reference to the current SlipCell object.</li>
 *    <li><b>currentList()</b> Return a reference to the current list header</li>
 *    <li><b>deleteCell()</b> Delete the current SlipCell object.</li>
 *    <li><b>dump()</b> Uglify and print the current SlipReaderCell.</li>
 *    <li><b>listDepth()</b> Return the current list depth.</li>
 *    <li><b>reset()</b> Reset to current SlipCell object to the list header.</li>
 *    <li><b>resetTop()</b> Return to the topmost list. The current SlipCell
 *        object pointer will be the SlipSublist which caused recursion into the
 *        indentured list.</li>
 *    <li><b>upLevel ()</b> Return to the next higher SlipReaderCell object on
 *        the chain. The current SlipCell object pointer will be the the 
 *        SlipSublist object causing recursion into the indentured list. Tthis
 *        is equivalent to a return from recursion.</li>
 *    <li><b>toString()</b> Pretty-print a SlipReaderCell object.</li>
 * </ul>
 * 
 */

class SlipReaderCell : public SlipCellBase {
    private:

    /*********************************************************
     *                   Private Functions                   *
    **********************************************************/
      SlipReaderCell* getLink();                                                // Link to preceeding list cell
      void init();                                                              // Initialize cell core
      void setup(const SlipCell* header);                                       // Utility method for creation
      void setLink(SlipReaderCell& X);                                          // Set link to previous reader cell
      void setCurrentCell(SlipCellBase& X) { setLeftLink(&X);         }         // Set current Slip cell
      void setListDepth(USHORT X);                                              // Set list depth
      void setListHeader(const SlipHeader& X);                                  // Set list header for current level

    /*********************************************************
     *                  Protected Functions                  *
    **********************************************************/

    protected:
       SlipReaderCell() { init(); }
       static void setLink(SlipReaderCell& reader, SlipReaderCell& X)     { reader.setRightLink(&X); } // Set link to previous reader cell
       static void setCurrentCell(SlipReaderCell& reader, SlipCellBase& X){ reader.setLeftLink(&X);  } // Set current Slip cell
       static void setListDepth(SlipReaderCell& reader, USHORT X)         { *getReaderDepth(reader) = X; } // Set list depth
       static void setListHeader(SlipReaderCell& reader, SlipHeader& X)   { reader.setListHeader(X); } // Set list header for current level
       static SlipReaderCell* getLink(SlipReaderCell& reader)             { return reader.getLink(); }
       static SlipReaderCell* downLevel(SlipReaderCell& reader, SlipCellBase* cell);

   /*********************************************************
    *                   Public Functions                    *
   **********************************************************/

    public:
       SlipReaderCell(SlipCell* header)                                  { setup(header); }
       SlipReaderCell(const SlipCell& header)                            { setup(&header); }
       ~SlipReaderCell()                                                 { if (!isDeleted()) addAVSLCells( this, this); }

            /*****************************************
             *           Reader Assignment           *
            ******************************************/
      SlipReaderCell& operator=(const SlipReaderCell& X);                       // Return a copy of the current lists
      SlipReaderCell& operator=(const SlipHeader& X);                           // Return a copy of the current lists
   
            /*****************************************
             *            Reader Iteration           *
            ******************************************/
      SlipReaderCell*   advanceLEL();                                           // advance linear element left
      SlipReaderCell*   advanceLER();                                           // advance linear element right
      SlipReaderCell*   advanceLNL();                                           // advance linear name left
      SlipReaderCell*   advanceLNR();                                           // advance linear name right
      SlipReaderCell*   advanceLWL();                                           // advance linear word right
      SlipReaderCell*   advanceLWR();                                           // advance linear word right
      SlipReaderCell*   advanceSEL();                                           // advance structural element left
      SlipReaderCell*   advanceSER();                                           // advance structural element right
      SlipReaderCell*   advanceSNL();                                           // advance structural name left
      SlipReaderCell*   advanceSNR();                                           // advance structural name right
      SlipReaderCell*   advanceSWL();                                           // advance structural word right
      SlipReaderCell*   advanceSWR();                                           // advance structural word right

               /*****************************************
                *   Header List Manipulation Methods    *
               ******************************************/
     
       SlipCell&        replaceTop(SlipHeader& X);                              // Substitute list TOP value
       SlipCell&        replaceTop(SlipCell& X);                                // Substitute list TOP value
       SlipCell&        replaceBot(SlipHeader& X);                              // Substitute list BOT value
       SlipCell&        replaceBot(SlipCell& X);                                // Substitute list BOT value

       SlipCell&        replaceTop(bool X);                                     // Substitute list TOP value
       SlipCell&        replaceTop(UCHAR X);                                    // Substitute list TOP value
       SlipCell&        replaceTop(CHAR X);                                     // Substitute list TOP value
       SlipCell&        replaceTop(ULONG X);                                    // Substitute list TOP value
       SlipCell&        replaceTop(LONG X);                                     // Substitute list TOP value
       SlipCell&        replaceTop(DOUBLE X);                                   // Substitute list TOP value
       SlipCell&        replaceTop(const PTR X, const void* operation = ptrOP); // Substitute list TOP value
       SlipCell&        replaceTop(const string& X, bool constFlag = false);    // Substitute list TOP value
       SlipCell&        replaceTop(const string* X, bool constFlag = false);    // Substitute list TOP value

       SlipCell&        replaceBot(bool X);                                     // Substitute list BOT value
       SlipCell&        replaceBot(UCHAR X);                                    // Substitute list BOT value
       SlipCell&        replaceBot(CHAR X);                                     // Substitute list BOT value
       SlipCell&        replaceBot(ULONG X);                                    // Substitute list BOT value
       SlipCell&        replaceBot(LONG X);                                     // Substitute list BOT value
       SlipCell&        replaceBot(DOUBLE X);                                   // Substitute list BOT value
       SlipCell&        replaceBot(const PTR X, const void* operation = ptrOP); // Substitute list BOT value
       SlipCell&        replaceBot(const string& X, bool constFlag = false);    // Substitute list BOT value
       SlipCell&        replaceBot(const string* X, bool constFlag = false);    // Substitute list BOT value

            /*****************************************
             *           Manipulating Lists          *
            ******************************************/
   
      SlipCell&         replace(const SlipCell& X) {                            // Replace a cell on a list with a new cell
                                                      setCurrentCell(currentCell().replace(X));
                                                      return currentCell(); }
      SlipCell&         replace(bool X)            {                            // Replace a cell on a list with a new cell
                                                      setCurrentCell(currentCell().replace(X));
                                                      return currentCell(); }
      SlipCell&         replace(CHAR X)            {                            // Replace a cell on a list with a new cell
                                                      setCurrentCell(currentCell().replace(X));
                                                      return currentCell(); }
      SlipCell&         replace(UCHAR X)            {                            // Replace a cell on a list with a new cell
                                                      setCurrentCell(currentCell().replace(X));
                                                      return currentCell(); }
      SlipCell&         replace(LONG X)            {                            // Replace a cell on a list with a new cell
                                                      setCurrentCell(currentCell().replace(X));
                                                      return currentCell(); }
      SlipCell&         replace(ULONG X)            {                            // Replace a cell on a list with a new cell
                                                      setCurrentCell(currentCell().replace(X));
                                                      return currentCell(); }
      SlipCell&         replace(DOUBLE X)            {                            // Replace a cell on a list with a new cell
                                                      setCurrentCell(currentCell().replace(X));
                                                      return currentCell(); }
      SlipCell&         replace(const PTR X, const void* operation = ptrOP) {   // Replace a cell on a list with a new cell
                                                      setCurrentCell(currentCell().replace(X, operation));
                                                      return currentCell(); }
      SlipCell&         replace(const string& X, bool constFlag) {              // Replace a cell on a list with a new cell
                                                      setCurrentCell(currentCell().replace(X, constFlag));
                                                      return currentCell();
                                                   }
      SlipCell&         replace(const string* X, bool constFlag) {              // Replace a cell on a list with a new cell
                                                      setCurrentCell(currentCell().replace(X, constFlag));
                                                      return currentCell(); }

            /*****************************************
             *     Miscellaneous Reader Methods      *
            ******************************************/
      SlipReaderCell*   copy();                                                 // Return a copy of the reader list
      SlipCell&         currentCell() const { return *(SlipCell*)getLeftLink(); } // Return current SLIP cell being referenced
      SlipReaderCell*   reset() {                                                // Break reader to current list header
                                    setCurrentCell(currentList());
                                    return this;
                                }
      USHORT            listDepth()         { return *getReaderDepth(); }       // Current list depth being traversed
      SlipHeader&       currentList() const;                                    // Return current SLIP list header being traversed
      SlipReaderCell*   resetTop();                                             // Return to start of list descent sublist (return to top)
      SlipReaderCell*   upLevel ();                                             // Return to previous level

            /*****************************************
             *             Miscellaneous             *
            ******************************************/
      SlipReaderCell*     deleteCell();                                         // Delete the current cell and move left
      virtual string      toString() const;                                     // Pretty-print the Reader
      virtual string      dump() const;                                         // return complete reader description
   }; // class SlipReader
}; // namespace slip

#endif	/* SLIPREADER_H */

