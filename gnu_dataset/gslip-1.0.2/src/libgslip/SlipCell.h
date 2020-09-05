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
 * @file    SlipCell.h
 * @author  A. Schwarz
 * @date    October 23, 2011
 *
 * @brief contains the SlipCell declarations
 */

#ifndef _SLIPCELL_H
#define	_SLIPCELL_H

# include <string>
# include <ostream>
# include "SlipDef.h"
# include "SlipCellBase.h"
# include "SlipErr.h"
# include "SlipException.h"
# include "SlipGlobal.h"

using namespace slip;

namespace slip {

   class SlipOp;
   
/**
 * @class slip::SlipCell
 * 
 * @brief Base class for SLIP cells contained in a list.
 * <p>SlipDatum, SlipHeader, and SlipSublist objects are all derived classes.
 *    They are all candidates for list inclusion and they are the only objects
 *    allowed in a list. The nature of the inclusion and the semantics of 
 *    operation are dependant on the class. List iteration will successfully
 *    iterate over all object of the given classes.</p> 
 * <p>SlipCell provides the utility and general methods required to support list
 *    operations, assignments, and logical equality and inequality. It provides
 *    a contract that all derived classes must adhere to. SlipCell can not be 
 *    instantiated.</p>
 * <p>Some general comments about the supported operations are included below.
 *    The semantics of the pure abstract methods 
 *       (<tt><em>&lt;type&gt; name() = 0:</em></tt>)
 *    depend on the derived class. The remaining methods have a known semantics
 *    as defined in SlipCell.</p>
 * <p>For purposes of this discussion, a primitive value is any literal of type
 *    bool (<em>true/false</em>, CHAR, UCHAR, LONG, ULONG, DOUBLE or STRING.</p>
 * <p> The methods available are:</p>
 * <ul>
 *    <li><b>move</b> move an object from one list to another. The object must
 *        be in a list to be moved. The move command removes the object from its
 *        initial list and inserts it into a new list (or new position in the 
 *        same list. The move functionality is similar to insert functionality.
 *    <ul>
 *       <li>A SlipHeader object is always considered linked. A header object can
 *           not be moved. A move request causes the list contents to be moved.
 *           It performs as a moveList. The list is empty after the move.</li>
 *       <li>An unlinked object is inserted as is. After the insertion, the 
 *           object is the property of the list. The user should not try to 
 *           delete it.</li>
 *       <li>A linked object is moved from one list to another. After the move,
 *           the object is no longer part of the same list put part of the new
 *           list. A move of an object from one part of a list to another part
 *           of the same list is legal.</li>
 *       <li>If the source is a SlipSublist object then, if it is linked in a 
 *           list, it is moved the same as a linked object. If it is unlinked
 *           then it is inserted as an unlinked object. In neither case is
 *           the reference count of the referenced list header changed.</li>
 *       <li>A temporary is never moved into a list. A copy of the object
 *           is made and inserted into the list. 
 *           @note This only applies to SlipDatum objects. SlipHeaders and 
 *                 SlipSublists can not be temporaries.</li>
 *       <li>SlipDatum objects are moved or not depending on whether they are
 *           linked, temporary or unlinked. Unlinked objects are inserted and
 *           are 'owned' by the list (user deletion causes all sorts of issues).
 *           Temporary objects are copied. Linked objects are moved. The user is 
 *           responsible for the care and feeding of the original object.</li>
 *       <li>A SlipDatum object is always created for a primitive value, and
 *           the object is inserted into a list. Note that special care is
 *           required for handling PTR and STRING primitives.</li>
 *    </ul>
 *    <li><b>moveList</b> move the list contents of a list. After the move the 
 *       source list is dry as a bone.</li>
 *    <ul>
 *    <li><b>insert</b> insert primitive value or a linked or unlinked SlipCell 
 *        into a list. Insert functionality is similar to move functionality.</li>
 *    </ul>
 *    <ul>
 *       <li>A header object is always considered linked. When a request to
 *         insert a header into list creates a SlipSublist object referencing
 *         the header and increments the header reference count by one. The
 *         caller is responsible for deleting the header 
 *         (<em>header.deleteList()</em>) in the scope that it was created.</li>
 *       <li>An unlinked object is inserted as is. After the insertion, the 
 *           object is the property of the list. The user should not try to 
 *           delete it.</li>
 *       <li>A linked object is copied and then linked.</li>
 *       <li>If the source is a SlipSublist object then, if it is linked a new
 *           SlipSublist object is copied otherwise it is linked in place as
 *           an unlinked object. A new SlipSublist object causes the reference
 *           count of the referenced header to be incremented. A copied
 *           SlipSublist object implies multiple references to the same list.</li>
 *       <li>A temporary is never inserted into a list. A copy of the object
 *           is made and inserted into the list. 
 *           @note This only applies to SlipDatum objects. SlipHeaders and 
 *                 SlipSublists can not be temporaries.</li>
 *       <li>SlipDatum objects are copied or not depending on whether it is
 *           linked, temporary or unlinked. Unlinked objects are inerted and
 *           are 'owned' by the list (user deletion causes all sorts of issues).
 *           Temporary or linked objects are copied. The user is responsible for
 *           the care and feeding of the original object.</li>
 *       <li>A SlipDatum object is always created for a primitive value, and
 *           the object is inserted into a list. Note that special care is
 *           required for handling PTR and STRING primitives.</li>
 *    </ul>
 *    <li><b>replace</b> replace the object in a list by another object. Replace
 *        is not type invariant. If the right hand side (RHS) of the replace
 *        can not be cast into the left hand side (LHS) type, then replace
 *        deletes the LHS and creates a new SlipCell containing the RHS.
 *        <p>The user can not depend on the pointers and references to the LHS
 *        remaining valid after the replace operation. The operation can only
 *        be performed when the LHS is linked into a list. The RHS may or may
 *        not be linked. The RHS does not change after the replace is complete.
 *        </p>
 *     <p>Replace acts like a grown up assignment statement. An assignment 
 *        statement replaces like-to-like, it is type invariant. Replace 
 *        replaces like-to-like and unlike-to-unlike, it is not type invariant.</p>
 *     <p>If the LHS is:</p>
 *        <ul>
 *          <li>SlipHeader object: This is not allowed. Once a header always a 
 *              header. A SlipHeader object, although on a list, has this one
 *              distinction, it can't be replaced. The semantics of replacement
 *              are undefined.</li>
 *          <li>SlipSublist object: If the RHS is a SlipHeader pointer or 
 *              reference or the RHS is a SlipSublist object, it is silently 
 *              cast to a  SlipHeader pointer and replaces the existing pointer.
 *              The new header refCnt field is incremented and the original 
 *              referenced list is deleted (<em>deleteList()</em>).
 *              <p>If the RHS is a SlipDatum, or an object which can be 
 *                 converted into a SlipDatum, then the SlipSublist object 
 *                 is deleted (<em>delete (SlipSublist*)object</em>) and a 
 *                 new SlipDatum object created to replace the SlipSublist 
 *                 in the list.</p>
 *          <p>In summary if the RHS is a:</p></li>
 *          <ul>
 *             <li>primitive: a new SlipDatum object is inserted.</li>
 *             <li>SlipDatum: a new SlipDatum object is inserted.</li>
 *             <li>SlipSublist: <em>deleteList()</em> is executed on the 
 *                 referenced SlipHeader and a new reference supplied. The
 *                 new SlipHeader refCnt is incremented.</p>
 *              <p>If the referenced SlipHeader is the same for the old and new
 *                 SlipSublist, then this becomes a do-nothing operation.</li>
 *             <li>SlipHeader: <em>deleteList()</em> is executed on the 
 *                 referenced SlipHeader and a new reference supplied. The
 *                 new SlipHeader refCnt is incremented.</p></li>
 *          </ul> 
 *          <li>SlipDatum object: The object does not remain invariant under
 *              replacement. 
 *          <ul>
 *              <li>If the new value is, or can be converted to, a SlipDatum 
 *                  object then the type and value of the new datum replaces 
 *                  the type and value of the old datum.</li>
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
 *        right hand side (RHS) can not be cast into a type compatible with the
 *        LHS, then the operation is illegal (and a nasty message will be output).
 *        Unlike a replace, an assignment is from-like to-like.
 *        <p>If the destination (LHS) is a:</p></li>
 *        <ul>
 *          <li>SlipDatum then the source must be a SlipDatum object or primitive.</li>
 *          <li>SlipSublist then the source must be a SlipSublist or SlipHeader
 *              object. In both cases the original SlipHeader reference is deleted
 *              (<em>deleteList()</em>) and replaced by a new SlipHeader pointer.
 *              The refCnt field of the new header is incremented.</li>
 *          <li>SlipHeader assignments are not allowed.</li>
 *        </ul>
 *    <li><b>operator==</b> basically a like-as-like operation. Objects which
 *        are unequal types are unequal (false). If they are SlipDatum objects
 *        then a logical comparison is done. For SlipSublist object the SlipHeader
 *        pointers are compared and for SlipHeader objects the addresses of 
 *        the two objects are compared.</li>
 *    <ul>
 *       <li><b>SlipDatum == primitive</b>: the SlipDatum type and value is
 *           converted to a primitive object and compared.</li>
 *       <li><b>SlipDatum1 == SlipDatum2</b>: the SlipDatum types and values are
 *           converted to a primitive objects and compared.</li>
 *       <li><b>SlipSublist == SlipSublist</b>: the referenced SlipHeader object
 *           pointers are compared.</li>
 *       <li><b>SlipSublist == SlipHeader</b>: the SlipSublist referenced 
 *           SlipHeader pointer is compared to the SlipHeader address.</li>
 *       <li><b>SlipHeader == SlipHeader</b>: the object addresses are compared.</li>
 *    </ul>
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
 *       <td>Return value the input source value</td>
 *    </tr>
 * </table>
 * </center>
 * <br />
 * <br />
 */

class SlipCell : public SlipCellBase {
/**
 * @fn virtual string dump() const;
 * @brief Return hexadecimal string of object.
 * <p>Each derived cell returns a hexadecimal string of the derived object. A
 *    "dump" is an invariant property of an object and the dump method is
 *    deferred to a derived class of SlipOp. "dump" call the object method
 *    in the appropriate derived class.</p>
 * @return hexadecimal string representing the object
 * 
 * @fn virtual SlipCell& factory() = 0
 * @brief Construct a duplicate of the current cell.
 * <p>Usage: <tt><b>object.factory()</b></tt></p>
 * <p>A blind copy of a cell is created. The using method has an object of type
 *    SlipCell and wants to create a copy of it. The <em>factory</em> is a 
 *    virtual method and determines what derived object is and then executes
 *    the derived class's version of <em>factory</em> to create the copy.</p>
 * <p>For each derived class, <em>factory</em> becomes:</p>
 * <ul>
 *    <li><b>SlipDatum</b> <tt>SlipDatum(object)</tt> create a new SlipDatum 
 *        object containing a copy of the input object.</li>
 *    <li><b>SlipHeader</b> <tt>SlipHeader(object)</tt> create a new SlipHeader
 *        object and then copy all the list cells of the original object to
 *        the new list (see operator=(SlipHeader &)).</li>
 *    <li><b>SlipSublist</b> <tt>SlipSublist(object)</tt> create a new
 *        SlipSublist object referencing the same SlipHeader object as the
 *        original. The reference count of the new SlipHeader object is 
 *        incremented.</li>
 * </ul>
 * @return SlipCell reference to a copy of the object
 * 
 * @fn SlipCell& replace(const SlipCell& X) = 0
 * @brief Replace the current list object (LHS) with X (RHS).
 * <p></p>
 * <center>
 * <table>
 *    <caption>Replace Operations</caption>
 *    <tr>
 *       <th></th>
 *       <th colspan="4"><center><b>RHS</b></center></th>
 *       <th></th>
 *    </tr>
 *    <tr>
 *       <th><center><b>LHS</b></center></th>
 *       <th><center><b>Header</b></center></th>
 *       <th><center><b>Sublist</b></center></th>
 *       <th><center><b>Datum</b></center></th>
 *       <th><center><b>Primitive</b></center></th>
 *       <th><b>Description</b></th>
 *    </tr>
 *    <tr> 
 *       <th><b>Header</b></th>
 *       <td>illegal</td>
 *       <td>illegal</td>
 *       <td>illegal</td>
 *       <td>illegal</td>
 *       <td></td>
 *    </tr>
 *    <tr> 
 *       <th><b>Sublist</b></th>
 *       <td>replace</td>
 *       <td>replace</td>
 *       <td>new SlipDatum</td>
 *       <td>new SlipDatum</td>
 *       <td>Old object deleted</td>
 *    </tr>
 *    <tr> 
 *       <th><b>Datum</b></th>
 *       <td>new SlipSublist</td>
 *       <td>new SlipSublist</td>
 *       <td>copy</td>
 *       <td>copy</td>
 *       <td>new deletes old. copy copies type and value.</td>
 *    </tr>
 * </table>
 * </center>
 * @param[in] X Right Hand Side of the replacement
 * @return a reference to the object in the list
 * 
 * @fn SlipCell& replace(bool X)      = 0
 * @brief Replace the current list object (LHS) with X (RHS).
 * <p>The LHS becomes a SlipDatum object. If it is not an object initially, it
 *    is deleted and a new SlipDatum object is created. If it is a SlipDatum
 *    object and is a PTR or a STRING, then the PTR and STRING contents are
 *    removed by calling the SlipPointer.remove() virtual method. Otherwise,
 *    the existing SlipDatum type and value is replaced.</p>
 * <p>There is no guarantee that the same physical cell will be in the list
 *    after the replace is complete. All user pointers and references to the
 *    LHS should be considered invalid. The only persistent objects in which it
 *    is guaranteed to have a valid reference are the iterators, SlipReader and
 *    SlipSequencer.</p>
 * @param[in] X Right Hand Side of the replacement
 * @return a reference to the object in the list
 * 
 * @fn SlipCell& replace(CHAR X)      = 0
 * @brief Replace the current list object (LHS) with X (RHS).
 * <p>The LHS becomes a SlipDatum object. If it is not an object initially, it
 *    is deleted and a new SlipDatum object is created. If it is a SlipDatum
 *    object and is a PTR or a STRING, then the PTR and STRING contents are
 *    removed by calling the SlipPointer.remove() virtual method. Otherwise,
 *    the existing SlipDatum type and value is replaced.</p>
 * <p>There is no guarantee that the same physical cell will be in the list
 *    after the replace is complete. All user pointers and references to the
 *    LHS should be considered invalid. The only persistent objects in which it
 *    is guaranteed to have a valid reference are the iterators, SlipReader and
 *    SlipSequencer.</p>
 * @param[in] X Right Hand Side of the replacement
 * @return a reference to the object in the list
 * 
 * @fn SlipCell& replace(UCHAR X)      = 0
 * @brief Replace the current list object (LHS) with X (RHS).
 * <p>The LHS becomes a SlipDatum object. If it is not an object initially, it
 *    is deleted and a new SlipDatum object is created. If it is a SlipDatum
 *    object and is a PTR or a STRING, then the PTR and STRING contents are
 *    removed by calling the SlipPointer.remove() virtual method. Otherwise,
 *    the existing SlipDatum type and value is replaced.</p>
 * <p>There is no guarantee that the same physical cell will be in the list
 *    after the replace is complete. All user pointers and references to the
 *    LHS should be considered invalid. The only persistent objects in which it
 *    is guaranteed to have a valid reference are the iterators, SlipReader and
 *    SlipSequencer.</p>
 * @param[in] X Right Hand Side of the replacement
 * @return a reference to the object in the list
 * 
 * @fn SlipCell& replace(LONG X)      = 0
 * @brief Replace the current list object (LHS) with X (RHS).
 * <p>The LHS becomes a SlipDatum object. If it is not an object initially, it
 *    is deleted and a new SlipDatum object is created. If it is a SlipDatum
 *    object and is a PTR or a STRING, then the PTR and STRING contents are
 *    removed by calling the SlipPointer.remove() virtual method. Otherwise,
 *    the existing SlipDatum type and value is replaced.</p>
 * <p>There is no guarantee that the same physical cell will be in the list
 *    after the replace is complete. All user pointers and references to the
 *    LHS should be considered invalid. The only persistent objects in which it
 *    is guaranteed to have a valid reference are the iterators, SlipReader and
 *    SlipSequencer.</p>
 * @param[in] X Right Hand Side of the replacement
 * @return a reference to the object in the list
 * 
 * @fn SlipCell& replace(ULONG X)      = 0
 * @brief Replace the current list object (LHS) with X (RHS).
 * <p>The LHS becomes a SlipDatum object. If it is not an object initially, it
 *    is deleted and a new SlipDatum object is created. If it is a SlipDatum
 *    object and is a PTR or a STRING, then the PTR and STRING contents are
 *    removed by calling the SlipPointer.remove() virtual method. Otherwise,
 *    the existing SlipDatum type and value is replaced.</p>
 * <p>There is no guarantee that the same physical cell will be in the list
 *    after the replace is complete. All user pointers and references to the
 *    LHS should be considered invalid. The only persistent objects in which it
 *    is guaranteed to have a valid reference are the iterators, SlipReader and
 *    SlipSequencer.</p>
 * @param[in] X Right Hand Side of the replacement
 * @return a reference to the object in the list
 * 
 * @fn SlipCell& replace(DOUBLE X)    = 0
 * @brief Replace the current list object (LHS) with X (RHS).
 * <p>The LHS becomes a SlipDatum object. If it is not an object initially, it
 *    is deleted and a new SlipDatum object is created. If it is a SlipDatum
 *    object and is a PTR or a STRING, then the PTR and STRING contents are
 *    removed by calling the SlipPointer.remove() virtual method. Otherwise,
 *    the existing SlipDatum type and value is replaced.</p>
 * <p>There is no guarantee that the same physical cell will be in the list
 *    after the replace is complete. All user pointers and references to the
 *    LHS should be considered invalid. The only persistent objects in which it
 *    is guaranteed to have a valid reference are the iterators, SlipReader and
 *    SlipSequencer.</p>
 * @param[in] X Right Hand Side of the replacement
 * @return a reference to the object in the list
 * 
 * @fn SlipCell& replace(const PTR X, const void* operation = ptrOP) = 0
 * @brief Replace the current list object (LHS) with X (RHS).
 * <p>The LHS becomes a SlipDatum object. If it is not an object initially, it
 *    is deleted and a new SlipDatum object is created. If it is a SlipDatum
 *    object and is a PTR or a STRING, then the PTR and STRING contents are
 *    removed by calling the SlipPointer.remove() virtual method. Otherwise,
 *    the existing SlipDatum type and value is replaced.</p>
 * <p>There is no guarantee that the same physical cell will be in the list
 *    after the replace is complete. All user pointers and references to the
 *    LHS should be considered invalid. The only persistent objects in which it
 *    is guaranteed to have a valid reference are the iterators, SlipReader and
 *    SlipSequencer.</p>
 * <p>The operations field provides a means to specify user defined operations.
 *    The pointer must be to a SlipOp defined class. The standard casting,
 *    unary, arithmetic, bitwise and logical operations must all be overridden.
 *    The result is that a user Defined object is treated as a atomic value
 *    in all computations.</p>
 * @param[in] X (PTR) Right Hand Side of the replacement
 * @param[in] operation (void*) SlipOp derived User Defined operations
 * @return a reference to the object in the list
 * 
 * @fn SlipCell& replace(const string& X, bool constFlag = false) = 0
 * @brief Replace the current list object (LHS) with X (RHS).
 * <p>The LHS becomes a SlipDatum object. If it is not an object initially, it
 *    is deleted and a new SlipDatum object is created. If it is a SlipDatum
 *    object and is a PTR or a STRING, then the PTR and STRING contents are
 *    removed by calling the SlipPointer.remove() virtual method. Otherwise,
 *    the existing SlipDatum type and value is replaced.</p>
 * <p>There is no guarantee that the same physical cell will be in the list
 *    after the replace is complete. All user pointers and references to the
 *    LHS should be considered invalid. The only persistent objects in which it
 *    is guaranteed to have a valid reference are the iterators, SlipReader and
 *    SlipSequencer.</p>
 * @param[in] X Right Hand Side of the replacement
 * @param[in] constFlag determines copy and delete characteristics of the string
 * @return a reference to the object in the list
 * 
 * @fn SlipCell& replace(const string* X, bool constFlag = false) = 0
 * @brief Replace the current list object (LHS) with X (RHS).
 * <p>The LHS becomes a SlipDatum object. If it is not an object initially, it
 *    is deleted and a new SlipDatum object is created. If it is a SlipDatum
 *    object and is a PTR or a STRING, then the PTR and STRING contents are
 *    removed by calling the SlipPointer.remove() virtual method. Otherwise,
 *    the existing SlipDatum type and value is replaced.</p>
 * <p>There is no guarantee that the same physical cell will be in the list
 *    after the replace is complete. All user pointers and references to the
 *    LHS should be considered invalid. The only persistent objects in which it
 *    is guaranteed to have a valid reference are the iterators, SlipReader and
 *    SlipSequencer.</p>
 * @param[in] X Right Hand Side of the replacement
 * @param[in] constFlag determines copy and delete characteristics of the string
 * @return a reference to the object in the list
 */
/**
 * @fn SlipCell&  operator=(const SlipCell& X) = 0
 * @brief Assign the RHS to the LHS.
 * <p>If the LHS and the RHS have the same type then an assignment will be made.
 *    Otherwise the operation is in error.</p>
 * <p>Neither the LHS nor the RHS need be in a list. The contents of the LHS
 *    will be changed. The LHS address (object) will be the same after the 
 *    assignment and all pointers and references to this address will be valid.</p>
 * <p>The RHS will not be changed. All pointers and references to the RHS will
 *    be valid after the assignment and the value of the RHS will be unchanged.</p>
 * <p>In summary:</p>
 * <center>
 * <table>
 *    <caption>Assignment Operations</caption>
 *    <tr>
 *       <th></th>
 *       <th colspan="4"><center><b>RHS</b></center></th>
 *       <th></th>
 *    </tr>
 *    <tr>
 *       <th><center><b>LHS</b></center></th>
 *       <th><center><b>Header</b></center></th>
 *       <th><center><b>Sublist</b></center></th>
 *       <th><center><b>Datum</b></center></th>
 *       <th><center><b>Primitive</b></center></th>
 *       <th><b>Description</b></th>
 *    </tr>
 *    <tr> 
 *       <th><b>Header</b></th>
 *       <td>illegal</td>
 *       <td>illegal</td>
 *       <td>illegal</td>
 *       <td>illegal</td>
 *       <td>A SlipHeader can not be replaced</td>
 *    </tr>
 *    <tr> 
 *       <th><b>Sublist</b></th>
 *       <td>substitute</td>
 *       <td>substitute</td>
 *       <td>illegal</td>
 *       <td>illegal</td>
 *       <td>deleteList() old header, increment reference count of new</td>
 *    </tr>
 *    <tr> 
 *       <th><b>Datum</b></th>
 *       <td>illegal</td>
 *       <td>illegal</td>
 *       <td>copy</td>
 *       <td>copy</td>
 *       <td>remove() STRING or PTR, change type/value</td>
 *    </tr>
 * </table>
 * </center>
 * @param[in] X RHS of the assignment is unchanged
 * 
 * @fn SlipCell&  operator=(const bool    X) = 0
 * @brief Assign the RHS to the LHS.
 * <p>The LHS must be a SlipDatum object.</p>
 * <p>Neither the LHS nor the RHS need be in a list. The contents of the LHS
 *    will be changed. The LHS address (object) will be the same after the 
 *    assignment and all pointers and references to this address will be valid.</p>
 * <p>The RHS will not be changed. All pointers and references to the RHS will
 *    be valid after the assignment and the value of the RHS will be unchanged.</p>
 * @param[in] X RHS of the assignment is unchanged
 * 
 * @fn SlipCell&  operator=(const CHAR    X) = 0
 * @brief Assign the RHS to the LHS.
 * <p>The LHS must be a SlipDatum object.</p>
 * <p>Neither the LHS nor the RHS need be in a list. The contents of the LHS
 *    will be changed. The LHS address (object) will be the same after the 
 *    assignment and all pointers and references to this address will be valid.</p>
 * <p>The RHS will not be changed. All pointers and references to the RHS will
 *    be valid after the assignment and the value of the RHS will be unchanged.</p>
 * @param[in] X RHS of the assignment is unchanged
 * 
 * @fn SlipCell&  operator=(const DOUBLE  X) = 0
 * @brief Assign the RHS to the LHS.
 * <p>The LHS must be a SlipDatum object.</p>
 * <p>Neither the LHS nor the RHS need be in a list. The contents of the LHS
 *    will be changed. The LHS address (object) will be the same after the 
 *    assignment and all pointers and references to this address will be valid.</p>
 * <p>The RHS will not be changed. All pointers and references to the RHS will
 *    be valid after the assignment and the value of the RHS will be unchanged.</p>
 * @param[in] X RHS of the assignment is unchanged
 * 
 * @fn SlipCell&  operator=(const LONG    X) = 0
 * @brief Assign the RHS to the LHS.
 * <p>The LHS must be a SlipDatum object.</p>
 * <p>Neither the LHS nor the RHS need be in a list. The contents of the LHS
 *    will be changed. The LHS address (object) will be the same after the 
 *    assignment and all pointers and references to this address will be valid.</p>
 * <p>The RHS will not be changed. All pointers and references to the RHS will
 *    be valid after the assignment and the value of the RHS will be unchanged.</p>
 * @param[in] X RHS of the assignment is unchanged
 * 
 * @fn SlipCell&  operator=(const UCHAR   X) = 0
 * @brief Assign the RHS to the LHS.
 * <p>The LHS must be a SlipDatum object.</p>
 * <p>Neither the LHS nor the RHS need be in a list. The contents of the LHS
 *    will be changed. The LHS address (object) will be the same after the 
 *    assignment and all pointers and references to this address will be valid.</p>
 * <p>The RHS will not be changed. All pointers and references to the RHS will
 *    be valid after the assignment and the value of the RHS will be unchanged.</p>
 * @param[in] X RHS of the assignment is unchanged
 * 
 * @fn SlipCell&  operator=(const ULONG   X) = 0
 * @brief Assign the RHS to the LHS.
 * <p>The LHS must be a SlipDatum object.</p>
 * <p>Neither the LHS nor the RHS need be in a list. The contents of the LHS
 *    will be changed. The LHS address (object) will be the same after the 
 *    assignment and all pointers and references to this address will be valid.</p>
 * <p>The RHS will not be changed. All pointers and references to the RHS will
 *    be valid after the assignment and the value of the RHS will be unchanged.</p>
 * @param[in] X RHS of the assignment is unchanged
 * 
 * @fn SlipCell&  operator=(const PTR     X) = 0
 * @brief Assign the RHS to the LHS.
 * <p>The LHS must be a SlipDatum object.</p>
 * <p>Neither the LHS nor the RHS need be in a list. The contents of the LHS
 *    will be changed. The LHS address (object) will be the same after the 
 *    assignment and all pointers and references to this address will be valid.</p>
 * <p>The RHS will not be changed. All pointers and references to the RHS will
 *    be valid after the assignment and the value of the RHS will be unchanged.</p>
 * @param[in] X RHS of the assignment is unchanged
 * 
 * @fn SlipCell&  operator=(const string& X) = 0
 * @brief Assign the RHS to the LHS.
 * <p>The LHS must be a SlipDatum object.</p>
 * <p>Neither the LHS nor the RHS need be in a list. The contents of the LHS
 *    will be changed. The LHS address (object) will be the same after the 
 *    assignment and all pointers and references to this address will be valid.</p>
 * <p>The RHS will not be changed. All pointers and references to the RHS will
 *    be valid after the assignment and the value of the RHS will be unchanged.</p>
 * @param[in] X RHS of the assignment is unchanged
 * 
 * @fn SlipCell&  operator=(const string* X) = 0
 * @brief Assign the RHS to the LHS.
 * <p>The LHS must be a SlipDatum object.</p>
 * <p>Neither the LHS nor the RHS need be in a list. The contents of the LHS
 *    will be changed. The LHS address (object) will be the same after the 
 *    assignment and all pointers and references to this address will be valid.</p>
 * <p>The RHS will not be changed. All pointers and references to the RHS will
 *    be valid after the assignment and the value of the RHS will be unchanged.</p>
 * @param[in] X RHS of the assignment is unchanged
 * 
 */
    /*********************************************************
     *                   Private Functions                   *
    **********************************************************/
private:

    /*********************************************************
     *                  Protected Functions                  *
     *********************************************************/
protected:

       /*************************************************
        *           Constructors & Destructors          *
        ************************************************/

   SlipCell(SlipCell& X) { *this = X; };                                        // Constructor

       /*************************************************
        *                Utility Methods                *
        ************************************************/
    static bool      checkHeader(SlipCell& X, string methodName);
    static bool      checkThat(const SlipCell& X, string methodName);           // Check if this is legal
    static bool      checkThis(const SlipCell& X, string methodName);           // Check if this is legal
 
   SlipCell&         constructSlipCell(SlipCellBase& X) { return ((SlipCell&)X).factory();}   // Use a factory to construct a cell
   
   SlipCell&         copy(SlipCell& X)  { return X.factory(); }                 // Create a copy of the input cell
   virtual SlipCell& factory() = 0;                                             // Create a SlipCell
   SlipCell&         insertLeft(SlipCell&    X) {                               // Insert X to current cell left
                        setRightLink((SlipCellBase*)&X, this);
                        setLeftLink((SlipCellBase*)&X, getLeftLink());
                        setRightLink(getLeftLink(), &X);
                        setLeftLink(&X);
                        return X;
                     }
   SlipCell&         insertRight(SlipCell&   X) {                               // Insert X to current cell right
                        X.setLeftLink(this);
                        X.setRightLink(getRightLink());
                        setLeftLink(getRightLink(), &X);
                        setRightLink(&X);
                        return X;
                     }
    
   void static       printCheck(ostream& out, int& col, int extent);            // Check if enough room on current line
   
   void              replaceLinks(SlipCell& X);                                 // Insert X into current list
public:

    /*********************************************************
     *                   Public Functions                    *
    **********************************************************/

       /*************************************************
        *           Constructors & Destructors          *
        ************************************************/
   SlipCell() { }
   virtual ~SlipCell() { }

       /*************************************************
        *                Utility Methods                *
        ************************************************/

            /*****************************************
             *           Manipulating Lists          *
            ******************************************/
                                                                                // List Cells must be unlinked
   SlipCell&         insLeft(SlipCell&  X);                                     // Insert X to current cell left
   SlipCell&         insLeft(bool       X);                                     // Insert X to current cell left
   SlipCell&         insLeft(DOUBLE     X);                                     // Insert X to current cell left
   SlipCell&         insLeft(ULONG      X);                                     // Insert X to current cell left
   SlipCell&         insLeft(LONG       X);                                     // Insert X to current cell left
   SlipCell&         insLeft(UCHAR      X);                                     // Insert X to current cell left
   SlipCell&         insLeft(CHAR       X);                                     // Insert X to current cell left
   SlipCell&         insLeft(const PTR  X, const void* operation = ptrOP);      // Insert X to current cell left
   SlipCell&         insLeft(const string& X, bool constFlag = false);          // Insert X to current cell left
   SlipCell&         insLeft(const string* X, bool constFlag = false);          // Insert X to current cell left
                                                                                // List Cells must be unlinked
   SlipCell&         insRight(SlipCell&  X);                                    // Insert X to current cell left
   SlipCell&         insRight(bool       X);                                    // Insert X to current cell right
   SlipCell&         insRight(DOUBLE     X);                                    // Insert X to current cell right
   SlipCell&         insRight(ULONG      X);                                    // Insert X to current cell right
   SlipCell&         insRight(LONG       X);                                    // Insert X to current cell right
   SlipCell&         insRight(UCHAR      X);                                    // Insert X to current cell right
   SlipCell&         insRight(CHAR       X);                                    // Insert X to current cell right
   SlipCell&         insRight(const PTR  X, const void* operation = ptrOP);     // Insert X to current cell right
   SlipCell&         insRight(const string&    X, bool constFlag = false);      // Insert X to current cell right
   SlipCell&         insRight(const string*    X, bool constFlag = false);      // Insert X to current cell right

   SlipCell&         moveLeft(SlipCell& X);                                     // Move the current cell to the left of X
   SlipCell&         moveRight(SlipCell& X);                                    // Move the current cell to the right of X

   SlipCell&         moveListLeft(SlipCell& X);                                 // Move the list X to the left of the current cell
   SlipCell&         moveListRight(SlipCell& X);                                // Move the list X to the right of the current cell

   virtual SlipCell& replace(const SlipCell& X) = 0;                            // Replace a cell on a list with a new cell
   virtual SlipCell& replace(bool X)      = 0;                                  // Replace a cell on a list with a new cell
   virtual SlipCell& replace(CHAR X)      = 0;                                  // Replace a cell on a list with a new cell
   virtual SlipCell& replace(UCHAR X)     = 0;                                  // Replace a cell on a list with a new cell
   virtual SlipCell& replace(LONG X)      = 0;                                  // Replace a cell on a list with a new cell
   virtual SlipCell& replace(ULONG X)     = 0;                                  // Replace a cell on a list with a new cell
   virtual SlipCell& replace(DOUBLE X)    = 0;                                  // Replace a cell on a list with a new cell
   virtual SlipCell& replace(const PTR X, const void* operation = ptrOP) = 0;   // Replace a cell on a list with a new cell
   virtual SlipCell& replace(const string& X, bool constFlag = false) = 0;      // Replace a cell on a list with a new cell
   virtual SlipCell& replace(const string* X, bool constFlag = false) = 0;      // Replace a cell on a list with a new cell

           SlipCell& unLink();                                                  // Remove cell from list and return

       /*************************************************
        *             Assignment operators              *
       *************************************************/
   virtual SlipCell& operator=(const SlipCell& X) = 0;                          // Y = V
   virtual SlipCell& operator=(const bool    X) = 0;                            // Y = V
   virtual SlipCell& operator=(const CHAR    X) = 0;                            // Y = V
   virtual SlipCell& operator=(const DOUBLE  X) = 0;                            // Y = V
   virtual SlipCell& operator=(const LONG    X) = 0;                            // Y = V
   virtual SlipCell& operator=(const UCHAR   X) = 0;                            // Y = V
   virtual SlipCell& operator=(const ULONG   X) = 0;                            // Y = V
   virtual SlipCell& operator=(const PTR     X) = 0;                            // Y = V
   virtual SlipCell& operator=(const string& X) = 0;                            // Y = V
   virtual SlipCell& operator=(const string* X) = 0;                            // Y = V

       /*************************************************
        *              Logical operators                *
       *************************************************/
   virtual bool      operator==(const SlipCell& X) = 0;                         // Y == X
           bool      operator!=(const SlipCell& X) { return !(*this == X); }    // Y == X

            /*****************************************
             *             Miscellaneous             *
             *****************************************/
   virtual string    toString() const;                                          // pretty print SLIP cell
   virtual string    write() const;                                             // String in output format
   virtual string    dump()  const;                                             // return complete string data
   virtual void      dump(ostream& out, int& col)  = 0;                         // Output the current cell
};  // SlipCell

};  // namespace slip
#endif	/* _SLIPCELL_H */

