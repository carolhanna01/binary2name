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
 * @file    SlipDatum.h
 * @author  A. Schwarz
 * @date November 16, 2011
 *
 * @brief contains the SlipDatum declarations
 *<p>
 * The datum class provides the functionality required to treat the datum in a
 * SlipDatum cell as a primitive type in arithmetic, bit, relational,
 * and casting operations. This functionality is achieved by overriding the
 * associated operators in C++ and substituting appropriate datum retrieval
 * operations with casting as required.</p>
 * <p>
 * Operations such as 'Y + X', where Y is a SlipDatum cell and X can be a
 * SlipDatum cell or a primitive value (bool, UCHAR, CHAR, ULONG, LONG, DOUBLE)
 * are supported as if Y &lt; op&gt;  X were native to the language. The full range of
 * internal types and C++ operations (unary, arithmetic, bitwise, and relational)
 * are supported.</p>
 * <p>
 * This file does not support 'X + Y', where X is a primitive value. This needed
 * functionality is supported by (...).</p>
 * <p>
 * The list of supported data based operator overrides is:</p>
 * <p>
 * <center>
 * <table>
 * <caption align="top">SlipDatum Operations Supported</caption align="top">
 *   <tr>
 *     <th>Operation</th>
 *     <th>Usage</th>
 *     <th>Overrides</th>
 *   </tr>
 *   <tr>
 *     <td>(bool)</td>
 *     <td>(bool) X</td>
 *     <td>operator (bool) ()</td>
 *   </tr>
 *   <tr>
 *     <td>(UCHAR)</td>
 *     <td>(UCHAR) X</td>
 *     <td>operator(UCHAR) ()</td>
 *   </tr>
 *   <tr>
 *     <td>(CHAR)</td>
 *     <td>(CHAR) X</td>
 *     <td>operator(CHAR) ()</td>
 *   </tr>
 *   <tr>
 *     <td>(ULONG)</td>
 *     <td>(ULONG) X</td>
 *     <td>operator(ULONG) ()</td>
 *   </tr>
 *   <tr>
 *     <td>(LONG)</td>
 *     <td>(LONG) X</td>
 *     <td>operator(LONG) ()</td>
 *   </tr>
 *   <tr>
 *     <td>(DOUBLE)</td>
 *     <td>(DOUBLE) X</td>
 *     <td>operator(DOUBLE) ()</td>
 *   </tr>
 *   <tr>
 *     <td>(PTR)</td>
 *     <td>(PTR) X</td>
 *     <td>operator(PTR) ()</td>
 *   </tr>
 *   <tr>
 *     <td>(STRING)</td>
 *     <td>(STRING) X</td>
 *     <td>operator(STRING) ()</td>
 *   </tr>
 *   <tr>
 *     <td>~</td>
 *     <td><tt>~X</tt></td>
 *     <td>operator~ ()</td>
 *   </tr>
 *   <tr>
 *     <td>!</td>
 *     <td><tt>!X</tt></td>
 *     <td>operator! ()</td>
 *   </tr>
 *   <tr>
 *     <td>+</td>
 *     <td>+X</td>
 *     <td>operator+ ()</td>
 *   </tr>
 *   <tr>
 *     <td>-</td>
 *     <td><tt>-X</tt></td>
 *     <td>operator- ()</td>
 *   </tr>
 *   <tr>
 *     <td>++</td>
 *     <td><tt>X++</tt></td>
 *     <td>operator++ ()</td>
 *   </tr>
 *   <tr>
 *     <td>--</td>
 *     <td><tt>X--</tt></td>
 *     <td>operator-- ()</td>
 *   </tr>
 *   <tr>
 *     <td>++</td>
 *     <td><tt>++X</tt></td>
 *     <td>operator++ (int)</td>
 *   </tr>
 *   <tr>
 *     <td>--</td>
 *     <td><tt>--X</tt></td>
 *     <td>operator-- (int)</td>
 *   </tr>
 *   <tr>
 *     <td>+</td>
 *     <td><tt>X +  V</tt></td>
 *     <td>operator+ (&lt; data type&gt; )</td>
 *   </tr>
 *   <tr>
 *     <td>-</td>
 *     <td><tt>X -  V</tt></td>
 *     <td>operator- (&lt; data type&gt; )</td>
 *   </tr>
 *   <tr>
 *     <td>*</td>
 *     <td><tt>X *  V</tt></td>
 *     <td>operator* (&lt; data type&gt; )/td>
 *   </tr>
 *   <tr>
 *     <td>/</td>
 *     <td><tt>X /  V</tt></td>
 *     <td>operator/ (&lt; data type&gt; )</td>
 *   </tr>
 *   <tr>
 *     <td>\%</td>
 *     <td><tt>X \%  V</tt></td>
 *     <td>operator\% (&lt; data type&gt; )</td>
 *   </tr>
 *   <tr>
 *     <td>&lt; &lt; </td>
 *     <td><tt>X &lt; &lt;  V</tt></td>
 *     <td>operator&lt; &lt;  (&lt; data type&gt; )</td>
 *   </tr>
 *   <tr>
 *     <td>&gt; &gt; </td>
 *     <td><tt>X &gt; &gt;  V</tt></td>
 *     <td>operator&gt; &gt;  (&lt; data type&gt; )</td>
 *   </tr>
 *   <tr>
 *     <td>\&</td>
 *     <td><tt>X \&  V</tt></td>
 *     <td>operator\& (&lt; data type&gt; )</td>
 *   </tr>
 *   <tr>
 *     <td>|</td>
 *     <td><tt>X |  V</tt></td>
 *     <td>operator| (&lt; data type&gt; )</td>
 *   </tr>
 *   <tr>
 *     <td>^</td>
 *     <td><tt>X ^  V</tt></td>
 *     <td>operator^ (&lt; data type&gt; )</td>
 *   </tr>
 *   <tr>
 *     <td>=</td>
 *     <td><tt>X =  V</tt></td>
 *     <td>operator= (&lt; data type&gt; ))</td>
 *   </tr>
 *   <tr>
 *     <td>+=</td>
 *     <td><tt>X += V</tt></td>
 *     <td>operator+= (&lt; data type&gt; )</td>
 *   </tr>
 *   <tr>
 *     <td>-=</td>
 *     <td><tt>X -= V</tt></td>
 *     <td>operator-= (&lt; data type&gt; )</td>
 *   </tr>
 *   <tr>
 *     <td>/=</td>
 *     <td><tt>X /= V</tt></td>
 *     <td>operator/= (&lt; data type&gt; )</td>
 *   </tr>
 *   <tr>
 *     <td>*=</td>
 *     <td><tt>X *= V</tt></td>
 *     <td>operator*= (&lt; data type&gt; ))</td>
 *   </tr>
 *   <tr>
 *     <td>\%=</td>
 *     <td><tt>X \%= V</tt></td>
 *     <td>operator\% (&lt; data type&gt; )</td>
 *   </tr>
 *   <tr>
 *     <td>\&=</td>
 *     <td><tt>X \&= V</tt></td>
 *     <td>operator\& (&lt; data type&gt; )</td>
 *   </tr>
 *   <tr>
 *     <td>|=</td>
 *     <td><tt>X |= V</tt></td>
 *     <td>operator|= (&lt; data type&gt; )</td>
 *   </tr>
 *   <tr>
 *     <td>^=</td>
 *     <td><tt>X ^= V</tt></td>
 *     <td>operator^= (&lt; data type&gt; )</td>
 *   </tr>
 *   <tr>
 *     <td>&lt; &lt; =</td>
 *     <td><tt>X &lt; &lt; = V</tt></td>
 *     <td>operator&lt; &lt; = (&lt; data type&gt; )</td>
 *   </tr>
 *   <tr>
 *     <td>&gt; &gt; =</td>
 *     <td><tt>X &gt; &gt; = V</tt></td>
 *     <td>operator&gt; &gt; = (&lt; data type&gt; )</td>
 *   </tr>
 *   <tr>
 *     <td>!=</td>
 *     <td><tt>X != V</tt></td>
 *     <td>operator!= (&lt; data type&gt; )</td>
 *   </tr>
 *   <tr>
 *     <td>&lt; </td>
 *     <td><tt>X &lt;   V</tt></td>
 *     <td>operator&lt;  (&lt; data type&gt; )</td>
 *   </tr>
 *   <tr>
 *     <td>&lt; =</td>
 *     <td><tt>X &lt; = V</tt></td>
 *     <td>operator&lt; = (&lt; data type&gt; )</td>
 *   </tr>
 *   <tr>
 *     <td>==</td>
 *     <td><tt>X == V</tt></td>
 *     <td>operator== (&lt; data type&gt; )</td>
 *   </tr>
 *   <tr>
 *     <td>&gt; =</td>
 *     <td><tt>X &gt; = V</tt></td>
 *     <td>operator&gt; = (&lt; data type&gt; )</td>
 *   </tr>
 *   <tr>
 *     <td>&gt; </td>
 *     <td><tt>X &gt;   V</tt></td>
 *     <td>operator&gt;  (&lt; data type&gt; )</td>
 *   </tr>
 * </table>
 * </center>
 * <p></p>
 * <b>LEGEND</b>
 * <ul>
 * <li>&lt; data type&gt;  can be any on of a SlipDatum cell reference or an atomic
 *      type (bool, UCHAR, CHAR, ULONG, LONG, DOUBLE).</li>
 * <li>&lt; data type&gt;  of PTR and STRING are limited to casts, (PTR) or (STRING),
 *     or the != and ==  comparisons operators. No other operation is permitted.</li>
 * </ul>
 * <p><b>NOTE</b></p>
 * <ul>
 * <li>  To retrieve the value contained in a SlipDatum cell the casting operator
 *       associated with the data type must be used. For example, if the datum is
 *       a LONG then (LONG X, where X is the SlipDatum cell.</li>
 * <li> Arithmetic data types are bool, UCHAR, CHAR ULONG, LONG, and DOUBLE.
 *       Discrete data types are UCHAR, CHAR, ULONG, and LONG.</li>
 * </ul>
 */

#ifndef SLIPDATUM_H
#define SLIPDATUM_H

# include <sstream>
# include <ostream>
# include "SlipCellBase.h"
# include "SlipCell.h"
# include "SlipDef.h"
# include "SlipErr.h"
# include "SlipException.h"
# include "SlipGlobal.h"
# include "SlipOp.h"

namespace slip {

/**
 * @class SlipDatum
 * @brief Arithmetic, Assignment, Bit, Casting, Relational, and Unary  
 *         operations are supported on SlipDatum cells.
 * <p>
 * SLIP operations are divided into two basic classes; SLIP cell management and
 * SLIP cell data operations.</p>
 * <p>
 * The SLIP cell management functions manipulate SLIP cells without regard to
 * cell values. These are the list management operations associated with SLIP
 * cells and are described in SlipCell. The operations include migration,
 * deletion, and creation of SLIP cells. Modification of cell values is the
 * responsibility of SlipCell classes specific to the value being modified
 * (SlipDatum, SlipHeader and SlipSublist).</p>
 * <p>The Slip cell data operations perform operations specific to the 
 *    SlipDatum cell values. These include arithmetic, boolean, relational, 
 *    and bit operations. SlipDatum cells are created with a specific value 
 *    or modified via an assignment statement, note that the assignment 
 *    statement can not change the SlipDatum data class. Under simple
 *    assignment both the type and value change. Under compound assignment
 *    the type remains invariant and the indicated operation is performed
 *    by silently casting the right hand side.</p>
 * @note For purposes of this discussion user defined types are represented as
 *       <b>PTR</b> in the text and represent either an object of type SlipPointer*
 *       or a class of type SlipPointer. Hopefully the context will make which
 *       one is meant clear.
 * <p><b>Overview</b></p><hr>
 * <p>
 * SlipDatum introduces the ability to treat SlipCells (see SlipCell) as
 * atomic types in arithmetic, boolean, bit, and logical operations. It provides
 * the mechanisms required to support operations and the safeguards needed to do
 * the work correctly. The SlipDatum methods in this class are member functions
 * which override C++ operators. Non-member overrides are treated in the Slip
 * class.</p>
 * <p>
 * The SlipDatum cells contain heterogeneous data. A cell can contain any of the 
 * allowed data types bool, char, unsigned char, long, ussigned long and double. 
 * The data types for a given cell can change in time with <em>replace</em> and 
 * with a simple <em>assign</em>. SlipDatum types do not change under compound
 * assignment.  Any SlipDatum cell can be involved in any C++ operation and the 
 * legitimacy of that operation against the datum in the cell is determined at 
 * runtime. All C++ data operations are allowed providing the full panoply of 
 * operations available to atomic C++ data types.</p>
 * <p>
 * Support for dynamic control of heterogeneous data and the association with the
 * data and its viable operations is done with the <i>operations</i> field in the
 * SlipCell (see SlipCellBase). When a SlipDatum cell is created or the datum
 * in a cell is changed, the SlipCellBase <i>operations</i> field is populated 
 * with a SlipOP derived class specific to the SlipCell datum data type which 
 * contains the class invariant properties, including the boolean, arithmetic, 
 * bit and logical operations. </p>
 * <p>
 * A representation of the SlipDatum internal structure is given below.</p>
 * <p></p>
 *  <table>
 *    <caption align="top">SlipCellBase structure</caption align="top">
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
 *       <td colspan="8">rightLink</td>
 *    </tr>
 *    <tr>
 *       <td colspan="8">operator</td>
 *       <td colspan="8">Datum</td>
 *    </tr>
 *    <tr>
 *       <td colspan="8">Datum</td>
 *    </tr>
 * </table>
 * <br />
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
 *    <li><b>Datum</b> defined by the derived object.</li>
 * </ul>
 * <p>Each of the available data types use the Datum field differently. Each
 *    usage is distinguished by a different <b>operation</b> field pointer to
 *    the invariant properties of that type. <p>
 * <p>
 *    Thes following diagrams illustrate the different uses:</p>
 * <p></p>
 *  <table>
 *    <caption align="top"><b>bool</b>, <b>char</b> and <b>unsigned char</b></caption align="top">
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
 *       <td colspan="8">rightLink</td>
 *    </tr>
 *    <tr>
 *       <td colspan="8">operator</td>
 *       <td colspan="2">byte</td>
 *       <td colspan="6">unused</td>
 *    </tr>
 *    <tr>
 *       <td colspan="8">unused</td>
 *    </tr>
 * </table>
 * <br />
 * <p></p>
 *  <table>
 *    <caption align="top"><b>long</b> and <b>unsigned Long</b> structure</caption align="top">
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
 *       <td colspan="8">rightLink</td>
 *    </tr>
 *    <tr>
 *       <td colspan="8">operator</td>
 *       <td colspan="8">long</td>
 *    </tr>
 *    <tr>
 *       <td colspan="8">unused</td>
 *    </tr>
 * </table>
 * <br />
 * <p></p>
 *  <table>
 *    <caption align="top"><b>double</b> structure</caption align="top">
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
 *       <td colspan="8">rightLink</td>
 *    </tr>
 *    <tr>
 *       <td colspan="8">operator</td>
 *       <td colspan="8">double</td>
 *    </tr>
 *    <tr>
 *       <td colspan="8">double</td>
 *    </tr>
 * </table>
 * <br />
* <p></p>
 *  <table>
 *    <caption align="top"><b>string</b> and <b>PTR</b> structure</caption align="top">
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
 *       <td colspan="8">rightLink</td>
 *    </tr>
 *    <tr>
 *       <td colspan="8">operator</td>
 *       <td colspan="8">SlipPointer*</td>
 *    </tr>
 *    <tr>
 *       <td colspan="8">unused</td>
 *    </tr>
 * </table>
 * <br />
 * <p>SlipPointer types deserve some special mention. Both strings and user
 *    defined object are SlipPointer types. Strings have well defined meanings
 *    and are integrated into the SLIP kernel. User defined types have
 *    meanings defined by the user. The SLIP kernel is aware of user types but
 *    has no SLIP defined semantics associated with any object. In addition, 
 *    in any given implementation, the user can define more than one type. The
 *    SLIP kernel doesn't care.</p>
 * <p>Strings are defined as being either constant (to SLIP) or inconstant.<p>
 * <ul>
 *    <li>Constant types require SLIP the same string. Each time a copy of the 
 *        SlipDatum object is needed, a copy of the SlipDatum SlipPointer is 
 *        made. All SlipDatum objects point to the same SlipPointer object. 
 *        Changes to any instance of the string is felt in all uses. Deletion 
 *        of any or all SlipDatum cells will not cause the string to be deleted. 
 *        To the SLIP kernel, the care and handling of these strings is the 
 *        users responsibility. If the user deletes the string and does not 
 *        delete all SlipDatum cells pointing to the string, stale pointers 
 *        will exist and the behavior when using stale pointers is undefined.
 *        If the user deletes all SlipDatum object referring to the string but
 *        does not delete the string, then the string is persistent and is
 *        unusable if no other user references exist.</li>
 *    <li>Inconstant types cause SLIP to create a copy of the string. Each
 *        time a copy of the SlipDatum object is needed, a new copy of the
 *        string is created. Change of one instance of the string does not 
 *        effect any other instance. Deletion of the original string or any
 *        one SlipDatum object does not effect any other SlipDatum object. 
 *        Responsibility for the care and feeding of inconstant data is the 
 *        responsibility of SLIP.</li>
 * </ul>
 * <p>Strings and user defined object are defined for the <b>replace</b>,
 *    <b>assign</b>, and <b>logical equality</b> operations.</p>
 * <p></p>
 * <p><b>SlipDatum and Lists</b></p><hr>
 * <p>A SlipDatum object can exist on only one list. Moving an object from one 
 *    list to another will cause the existing SlipDatum object to be removed 
 *    from the first list and inserted in the second. Any references or pointers 
 *    to the SlipDatum object will still point to the same object, but the 
 *    location of the object in a list will have changed. Any changes made to 
 *    a list object through use of a pointer or reference will be reflected to 
 *    the (same) object as stored in a list.</p>
 * <p>Replacing a SlipDatum object in a list does not guarantee that the old
 *    object will not be recovered in the AVSL and a new object inserted. You
 *    can not depend on pointers or references to the old object as being valid
 *    after the replace. Attempts to use stale pointers can destroy the integrity
 *    of the AVSL and the integrity of SLIP operations.</p>
 * <p>The user can create temporary object and during expression evaluation 
 *    temporary SlipDatum objects are created. These temporaries are never 
 *    placed in a list. Attempts to insert a temporary into a list will cause 
 *    a new SlipDatum object to be created and the new object to be inserted 
 *    into the list. Each time the same temporary is inserted into a list a 
 *    new object will be created and inserted in its stead. Any change made
 *    to a temporary will not be reflected into a list cell, and any changes
 *    to a list cell will not be reflected to a temporary. Further, any
 *    changes to a copy of a temporary in a list will not be reflected to
 *    and other copies of the temporary.</p>
 * <tt><pre>
 *     SlipHeader& head  = new SlipHeader(); // create a list
 *     SlipReader& reader = LRDROV(head);    // create a list reader
 *     head.push(5);                         // create aad push an (INT) SlipDatum objed
 *     SlipDatum   obj(10);                  // create an (INT) SlipDatum object
 *     SlipDatum&  ref = obj;                // create a reference
 *     enqueue(obj);                         // place obj after (5)
 *     head.push(ref);                       // move obj before (5)
 *     reader.advanceLER();                  // references the first list cell (obj)
 *     SlipDatum& readCell = reader.currentCell; // reference to the first list cell
 * </pre></tt>
 * <p><b>Constructors and Destructors</b></p>
 * <p>User creation of SlipDatum cells can be on the stack 
 *    (<em>SlipDatum cell(value)</em> or from the available space list (AVSL
 *    (<em>SlipDatum cell = new SlipDatum(value)</em>.</p>
 * <p>Stack based cells are not persistent. They can not be used in a list or
 *    become part of any SLIP kernel controlled persistent environment. The 
 *    runtime C++ system will delete these objects automatically. The user 
 *    should not delete these cells.</p>
 * <p>Persistent SlipDatum cells can be used in a list. If they are not used in
 *    a list then the user must delete them (<em>delete SlipDatum*)cell</em>). 
 *    If they are used in a list, then the user should unlink them from the list
 *    to delete them. If deletion is done while the cell is on a list then a
 *    the cell will be silently unlinked and deleted.<p>
 * <p>
 * SlipDatum cells are automatically created on the stack during execution of 
 * expressions. Intermediate terms in an expression create temporary SlipDatum 
 * cells. These cells are automatically deleted at scope termination.</p>
 * <p>Temporary cells may be input arguments to functions <em>fn(X + Y)</em>
 *    where X and Y are references to SlipDatum objects is a valid statement.</p>
 * <p>A copy temporary cell is inserted into a list for insert operations. The
 *    type and datum of a temporary cell is copied into an existing SlipDatum
 *    cell for a replace but will generate an error if an attempt is made to
 *    replace a SlipSublist object.
 * <p>
 * An example of automatic creation of a temporary cell</p>
 * <tt>:
 * <pre>
 *    SlipDatum result(<LONG>value);     // create a temporary on the stack
 *    result = X + Y + Z;
 *
 *    where X, Y, and Z are all SlipDatum cells.
 * </pre>
 * </tt>
 * <p>
 * This expression decomposes to:</p>
 * <tt>
 * <pre>
 *   SlipDatum temp1 = X + Y;        // temporary created for partial expression
 *   SlipDatum temp2 = temp1 + Z;    // temporary created for partial expression
 *   SlipDatum result= temp2;        // assignment of a temporary to a temporary
 * </pre>
 * </tt>
 * <p>
 * The expression value and the associated operations (SlipCellBase <i>operations</i>) field)
 * are copied to <em>result</em>.
 * <p>
 * And as chance would have it, the destructors for result, temp1 and temp2 are 
 * called automatically when they go out of scope. 
 * <p><b>Operations</b></p><hr>
 * <p>The operations supported are: Binary, Assignment, Bit, Casting,
 *    Relational and Unary. These operations are invariant, that is, each
 *    SlipDatum object is able to process each of these operations. Where
 *    an operation is invalid, then a diagnostic message is issued.</p>
 * <p>A SlipDatum object has two fundamental characteristics, a type and a
 *    value. The type is invariant except under <em>replace</em>. 
 *    <em>replace</em> can change both the type and the object, i.e., the
 *    final object after the replace operation is complete may not be a
 *    SlipDatum object.</p>
 * <p>The supported types and values are:</p>
 * <p></p>
 * <center>
 * |  type  |           value           |
 * | :----- | :------------------------ |
 * | bool   | 0 <b>false</b> 1 <b>true</b>             |
 * | CHAR   | -128 to 127               |
 * | UCHAR  | 0 to 255                  |
 * | LONG   | -2147483648 to 2147483647 |
 * | ULONG  | 0 to 4294967295           |
 * | DOUBLE | +/- 1.7*10<sup>+/- 308</sup>         |
 * | PTR    | (SlipPointer*)            |
 * | STRING | (SlipPointer*)            |
 * </center>
 * <br />
 * <p>Unless otherwise noted, if the RHS of an operation is a discrete (bool, 
 *    UCHAR, CHAR, LONG,  ULONG) then it is widened to a LONG (for bool and 
 *    CHAR) or a ULONG (for UCHAR)v before use. If the RHS is DOUBLE, PTR, or 
 *    STRING then the type is retained.</p>
 * <p>This can be seen as:</p>
 * <br />
 * <center>
 * <table>
 *    <caption align="top">Widening Rules</caption>
 *    <tr>
 *       <th></th>
 *       <center><th><b>input</b></th></center>
 *    </tr>
 *    <tr>
 *       <th><b>convert to</b></th>
 *       <center><th>y</th></center>
 *    </tr>
 *    <tr>
 *       <td>LONG</td>
 *       <td>bool</td>
 *    <tr>
 *       <td>LONG</td>
 *       <td>CHAR</td>
 *    </tr>
 *    <tr>
 *       <td>ULONG</td>
 *       <td>UCHAR</td>
 *    </tr>
 *    <tr>
 *       <td>LONG</td>
 *       <td>LONG</td>
 *    </tr>
 *    <tr>
 *       <td>ULONG</td>
 *       <td>ULONG</td>
 *    </tr>
 *    <tr>
 *       <td>DOUBLE</td>
 *       <td>DOUBLE</td>
 *    </tr>
 *    <tr>
 *       <td>PTR</td>
 *       <td>PTR</td>
 *    </tr>
 *    <tr>
 *       <td>STRING</td>
 *       <td>STRING</td>
 *    </tr>
 * </table>
 * </center>
 * <p><b>Binary Operations</b></p><hr>
 * <p>The arguments which are legal in an operation depend on the operation 
 *    being performed. STRING and user defined (PTR) types are not legal in 
 *    any arithmetic operation. DOUBLE is not defined for any operations other 
 *    than addition, subtraction, multiplication and division. Discretes (bool, 
 *    CHAR, UCHAR, LONG, ULONG) are valid in all operations. An input bool is 
 *    converted to zero (0) for false and one (1) for true prior to use.</p>
 * <p>If we defined <em>x + y</em> as being <em><b>LHS</b> + <b>RHS</b></em>
 *    then we can say that if the RHS is a bool, CHAR or LONG then it is
 *    widened to a LONG before the operation is performed, and if the RHS is
 *    a UCHAR or ULONG it is widened to a ULONG before the operation. DOUBLE
 *    is unchanged and PTR and STRING are not allowed.</p>
 * <p>The return value of an operation is a temporary placed onto the runtime
 *    stack. Value can be assigned, used as an input to functions, or used
 *    in further computations. It acts in a manner semantically similar to
 *    a C++ arithmetic expression except that except for mod, the return is
 *    a SlipDatum object.</p>
 * <p>Therefore:</p>
 * <tt><pre>
 *    SlipDatum x;
 *    &#133; x + 1 &#133;
 * </pre></tt>
 * Is functionally the same as:
 * <tt><pre>
 *    int x;
 *    &#133; x + 1 &#133; 
 * </pre></tt>
 * <p>and if the SlipDatum type and value is the same as the <em>int x</em>
 *    value, will yield the same result.</p>
 * <p>The result of any operation involving primitives (bool, CHAR, UCHAR, LONG,
 *    ULONG, and DOUBLE) is a SlipDatum object with the same type and value
 *    which would be obtained if the value of the LHS and the primitive were
 *    executed in C++. In other words, if the LHS side was a bool and RHS was
 *    a long, then the result type and value is the same as the result and type
 *    of <tt>bool op long</tt> executed by C++.</p>
 * <p>If the the LHS and RHS are SlipDatum objects, then the type and result
 *    of a computation is defined in the SLIP kernel.  This is summarized by:</p>
 * <br />
 * <center>
 * <b>Result of type op type</b>
 * <tt>
 *        op   |   bool  |   CHAR  |   UCHAR |   LONG  |   ULONG | DOUBLE  |   PTR   | STRING  |
 *    | :----- | :------ | :------ | :------ | :------ | :------ | :------ | :------ | :------ |
 *    | bool   | LONG    | LONG    | LONG    | LONG    | LONG    | DOUBLE  | illegal | illegal |
 *    | CHAR   | LONG    | LONG    | LONG    | LONG    | LONG    | DOUBLE  | illegal | illegal |
 *    | UCHAR  | ULONG   | ULONG   | ULONG   | ULONG   | ULONG   | DOUBLE  | illegal | illegal |
 *    | LONG   | LONG    | LONG    | LONG    | LONG    | LONG    | DOUBLE  | illegal | illegal |
 *    | ULONG  | ULONG   | ULONG   | ULONG   | ULONG   | ULONG   | DOUBLE  | illegal | illegal |
 *    | DOUBLE | DOUBLE  | DOUBLE  | DOUBLE  | DOUBLE  | DOUBLE  | DOUBLE  | illegal | illegal |
 *    | PTR    | illegal | illegal | illegal | illegal | illegal | illegal | illegal | illegal |
 *    | STRING | illegal | illegal | illegal | illegal | illegal | illegal | illegal | illegal |
 * </tt></center>
 * <br />
 * <center>
 * <b>Result of type % type</b>
 *  <tt>
 *        %    |   bool  |   CHAR  |   UCHAR |   LONG  |   ULONG | DOUBLE  |   PTR   | STRING  |
 *    | :----- | :------ | :------ | :------ | :------ | :------ | :------ | :------ | :------ |
 *    | bool   | LONG    | LONG    | LONG    | LONG    | LONG    | illegal | illegal | illegal |
 *    | CHAR   | LONG    | LONG    | LONG    | LONG    | LONG    | illegal | illegal | illegal |
 *    | UCHAR  | ULONG   | ULONG   | ULONG   | ULONG   | ULONG   | illegal | illegal | illegal |
 *    | LONG   | LONG    | LONG    | LONG    | LONG    | LONG    | illegal | illegal | illegal |
 *    | ULONG  | ULONG   | ULONG   | ULONG   | ULONG   | ULONG   | illegal | illegal | illegal |
 *    | DOUBLE | illegal | illegal | illegal | illegal | illegal | illegal | illegal | illegal |
 *    | PTR    | illegal | illegal | illegal | illegal | illegal | illegal | illegal | illegal |
 *    | STRING | illegal | illegal | illegal | illegal | illegal | illegal | illegal | illegal |
 * </tt></center>
 * <br />
 * where 
 * <ul>
 *    <li><em>op</em> is any of the +, -, *, / operations</li>
 *    <li>The row headings represent LHS the type of a SlipDatum object</li>
 *    <li>The column headings represent the RHS type of a SlipDatum object</li>
 *    <li>The intersection values represent the resultant type of LHS op RHS</li>
 * </ul>
 * <p><b>Bit Operations</b></p><hr>
 * <p>Bit binary operations return a LONG as a result. The operations are 
 *    semantically identical to the same operations performed in C++ and 
 *    yield the same result (cast to a LONG). DOUBLE, PTR, and STRING are
 *    not allowed.</p>
 * <p>If the RHS is bool, CHAR, or LONG, then it is widened to a LONG before 
 *    the bitwise operation is performed. If the RHS is UCHAR or ULONG then
 *    it is widened to a ULONG before the bitwise operation is performed.</p>
 * <p>The supported operations are:</p>
 * <ul>
 *    <li><b>&amp;</b> bitwise <em>and</em> of the LHS and RHS.</li>
 *    <li><b>|</b> bitwise <em>or</em> of the LHS and RHS.</li>
 *    <li><b>^</b> bitwise <em>exlusive or</em> of the LHS and RHS.</li>
 *    <li><b>&lt;&lt;</b> left-shift the LHS by the RHS bits.</li>
 *    <li><b>&gt;&gt;</b> right-shift the LHS by the RHS bits.</li>
 * </ul>
 * <p><b>Assignment Operations</b></p><hr>
 * <p>Assignment statements can either be a simple assignment (LHS = RHS) or a 
 *    compound assignment (LHS op= RHS). The RHS is unchanged. The LHS value is
 *    changed in all cases, and the LHS type may be changed in simple assignment.
 *    We say the type invariance is preserved in compound assignments and lost
 *    in simple assignment.</p>
 *    Therefor, simple assignments allow the RHS and LHS to be any type and have 
 *    the assignment succeed. At the end of the assignment the conditional
 *    <em>LHS == RHS</em> is <b>true</b>.</p>
 * <p>mod (%) and bit operations require a discrete value for the LHS and RHS.
 *    Addition, subtraction, multiplication and division can have discrete or
 *    indiscrete values for either the RHS or LHS. PTR and STRING are only
 *    allowed in simple assignment statements.</p>
 * <p>If the RHS is a primitive type the value of the computation is the same
 *    as would occur in C++ with the same SlipDatum value and RHS. If the RHS
 *    is a SlipDatum object, then the value of the computation is as defined
 *    above, where <em>op</em> now takes on operations of +=, -=, *= /=, &lt;&lt=,
 *    &gt;&gt;=, |=, &amp;=, and ^=. The operation %= can not have a RHS or LHS of
 *    DOUBLE.</p>
 * <p>For example:</p>
 * <tt><pre>
 *                                          // result is
 *    SlipDatum v((LONG) 5)       += 3;     // (LONG)    8
 *    SlipDatum w((DOUBLE) 6.0)    * 5;     // (DOUBLE) 30.0
 *    SlipDatum x((UCHAR) 0x03)   |= 4;     // (UCHAR) 0x07
 *    SlipDatum y((CHAR)  0x01)  &lt;&lt;= 4;     // (CHAR) 16
 *    SlipDatum z((bool) true)     = 4 + 5; // (bool)  true
 *    SlipDatum a((DOUBLE) 1.0)   &= 0xFF;  // illegal
 *    SlipDatum b((SlipPointer&)  += 1      // illegal
 *    SlipDatum c("string")       += 'C';   // illegal
 * </pre></tt>
 * <p>A simple table describes the legal operations and types:</p>
 * <center>
 * <b>Legal types in Simple and Compound Assignments</b>
 * <tt>
 * |  op  |   bool  |   CHAR  |  UCHAR  |  LONG   |  ULONG  | DOUBLE  |   PTR   |  STRING |
 * | ---: | :------ | :------ | :------ | :------ | :------ | :------ | :------ | :------ |
 * |   =  | legal   | legal   | legal   | legal   | legal   | legal   | legal   | legal   |
 * |  +=  | legal   | legal   | legal   | legal   | legal   | legal   | illegal | illegal |
 * |  -=  | legal   | legal   | legal   | legal   | legal   | legal   | illegal | illegal |
 * |  *=  | legal   | legal   | legal   | legal   | legal   | legal   | illegal | illegal |
 * |  /=  | legal   | legal   | legal   | legal   | legal   | legal   | illegal | illegal |
 * |  %=  | legal   | legal   | legal   | legal   | legal   | illegal | illegal | illegal |
 * |  &amp;=  | legal   | legal   | legal   | legal   | legal   | illegal | illegal | illegal |
 * |  \|=  | legal   | legal   | legal   | legal   | legal   | illegal | illegal | illegal |
 * |  ^=  | legal   | legal   | legal   | legal   | legal   | illegal | illegal | illegal |
 * | &lt;&lt;=  | legal   | legal   | legal   | legal   | legal   | illegal | illegal | illegal |
 * | &gt;&gt;=  | legal   | legal   | legal   | legal   | legal   | illegal | illegal | illegal |
 * </tt></center>
 * <p><b>Relational Operations</b></p><hr>
 * <p>There are 6 relational operators (!=, &lt;, &lt;=, == &gt;=, &gt;) 
 *    supported in SLIP. SLIP returns the result of relational operations
 *    with the same semantics as in C++. The result of a comparison is a
 *    bool, with <b>true</b> returned if the comparison is successful, 
 *    and <b>false</b> returned otherwise.</p>
 * <p>The STRING and PTR types can only participate in the <em>!=</em>
 *    and <em>==</em> relations. For STRINGs the comparison compares the
 *    string value of a string or STRING. For PTRs the comparison compares
 *    the address of one PTR to another.</p>
 * <p>If we use the terminology <em>x op y</em> means <em>LHS op RHS</em>
 *    then if the RHS is a discrete (bool, CHAR, LONG) is converted to
 *    a LONG before the comparison, and (UCHAR, ULONG) are converted to
 *    a ULONG. DOUBLEs remain unchanged. The LHS is not converted.</p>
 * <p><b>Casting Operations</b></p><hr>
 * <p>Casting converts an object of one type into another. The result of the
 *    cast is placed on the stack and is a primitive type (bool, CHAR, UCHAR,
 *    LONG, ULONG or DOUBLE) depending on the cast performed. PTR and STRING 
 *    types can not be cast.</p>
 * <p>
 * A cast to the same data type as the SlipDatum value retrieves the value
 * of the object. That is, if the SlipDatum value is an INT, then (INT) object
 * returns the object value.</p>
 * <p>
 * Casting does not change the value contained in the SlipDatum object. It
 * returns the object value cast to the specified type.</p>
 * <p>Some examples of casting are:</p>
 * <p>
 * <tt><pre>
 *      SlipDatum a(LONG) 3);       // 'a' has a LONG data value
 *      long   x = (LONG)a;         // x == 3
 *      double y = (DOUBLE)a;       // y == 3.0
 * 
 *      SlipDatum b(bool)true);     // 'b' is a bool with value <b>true</b>
 *      char   z = (CHAR)b;         // z == 1
 *      double u = (DOUBLE)b;       // u == 1.0
 * </pre></tt>
 * <p><b>User Defined Operations</b></p><hr>
 * <p>The standard behavior for a User Defined Data type is to not allow
 *    any operations except direct assignment and checks for equality.
 *    The user can override this behavior by providing a pointer to a
 *    SlipOp or a SlipPtrOp derived class overriding the default operations
 *    with User Defined operations. The SlipPtrOp is the standard default.
 *    In this, all operations, except equality, are disallowed.</p>
 * <p>The operation object pointer can be inserted in any method in which
 *    a SlipDatum(PTR) object will be created. This includes the constructor
 *    as well as insert and replace operations, but not direct assignment.</p>
 * 
 */

   class SlipDatum : public SlipCell {
       private:
       /*********************************************************
        *                   Private Functions                   *
       **********************************************************/
         void constructDatumCell(void* op);                                     // Fill in the Datum Cell fields
       protected:

       /*********************************************************
        *                  Protected Functions                  *
       **********************************************************/
         bool assignPreamble();                                                // Delete before change
         SlipDatum& assignEpilogue(const SlipDatum& X);                        // Replace data

         SlipDatum(const SlipDatum* X) {                                       // Create cell based on old cell
            this->setOperator(X->getOperator());
            *this->getData() = ((SlipOp*)*(X->getOperator()))->getDatumCopy(X); }

         virtual SlipCell& factory() {                                         // Create a SlipCell
            return *(new SlipDatum(*this));
         }
         
         SlipDatum& copy() const { return ((SlipOp*)*getOperator())->copy(*this); }   // Return a reference to a copy of the current cell

      public:


      /*********************************************************
       *                   Public Functions                    *
      **********************************************************/

          /*************************************************
           *           Constructors & Destructors          *
           ************************************************/

         SlipDatum(const SlipDatum& X) {      
            void* op = *(X.getOperator());
            constructDatumCell(op);
            *getData() = ((SlipOp*)op)->getDatumCopy(X); }

         SlipDatum(DOUBLE X) {
            constructDatumCell((void*)doubleOP);
            *getDouble() = X; }
         SlipDatum(bool X) {
            constructDatumCell((void*)boolOP);
            *getBool() = X; }
         SlipDatum(UCHAR X) {
            constructDatumCell((void*)ucharOP);
            *getUChar() = X; }
         SlipDatum(CHAR X) {
             constructDatumCell((void*)charOP);
            *getChar() = X; }
         SlipDatum(ULONG X) {
            constructDatumCell((void*)ulongOP);
           *getULong() = X; }
         SlipDatum(LONG X) {
            constructDatumCell((void*)longOP);
           *getLong() = X; }
         SlipDatum(const PTR X, const void* operation = ptrOP);
         SlipDatum(const string& X, bool constFlag = false);
         SlipDatum(const string* X, bool constFlag = false);
         virtual ~SlipDatum();

      /*********************************************************
       *                  Casting Operators                    *
      *********************************************************/
         operator bool() const   { return ((SlipOp*)*getOperator())->cast_bool(*this);   }
         operator UCHAR() const  { return ((SlipOp*)*getOperator())->cast_UCHAR(*this);  }
         operator CHAR() const   { return ((SlipOp*)*getOperator())->cast_CHAR(*this);   }
         operator ULONG() const  { return ((SlipOp*)*getOperator())->cast_ULONG(*this);  }
         operator LONG() const   { return ((SlipOp*)*getOperator())->cast_LONG(*this);   }
         operator DOUBLE() const { return ((SlipOp*)*getOperator())->cast_DOUBLE(*this); }
         operator PTR() const { return ((SlipOp*)*getOperator())->cast_PTR(*this); }

      /*********************************************************
       *                 Operator Overloading                  *
      *********************************************************/

          /*************************************************
           *                Unary operators                *
          *************************************************/
         bool       operator!()     { return ((SlipOp*)*getOperator())->logNOT(*this);  }
         ULONG      operator~()     { return ((SlipOp*)*getOperator())->bitNOT(*this);  }
         SlipDatum  operator+()     { return ((SlipOp*)*getOperator())->plus(*this);    }
         SlipDatum  operator-()     { return ((SlipOp*)*getOperator())->minus(*this);   }
         SlipDatum& operator++()    { return ((SlipOp*)*getOperator())->preInc(*this);  }
         SlipDatum& operator--()    { return ((SlipOp*)*getOperator())->preDec(*this);  }
         SlipDatum& operator++(int) { return ((SlipOp*)*getOperator())->postInc(*this); }
         SlipDatum& operator--(int) { return ((SlipOp*)*getOperator())->postDec(*this); }

          /*************************************************
           *                Binary operators               *
          *************************************************/
         SlipDatum  operator+(const SlipDatum& X) { return ((SlipOp*)*getOperator())->add(*this,  X);}
         SlipDatum  operator-(const SlipDatum& X) { return ((SlipOp*)*getOperator())->sub(*this,  X);}
         SlipDatum  operator*(const SlipDatum& X) { return ((SlipOp*)*getOperator())->mul(*this,  X);}
         SlipDatum  operator/(const SlipDatum& X) { return ((SlipOp*)*getOperator())->div(*this,  X);}
         LONG       operator%(const SlipDatum& X) { return ((SlipOp*)*getOperator())->mod(*this,  X);}

         SlipDatum  operator+(const DOUBLE X)     { return ((SlipOp*)*getOperator())->add(*this, X); }
         SlipDatum  operator-(const DOUBLE X)     { return ((SlipOp*)*getOperator())->sub(*this, X); }
         SlipDatum  operator*(const DOUBLE X)     { return ((SlipOp*)*getOperator())->mul(*this, X); }
         SlipDatum  operator/(const DOUBLE X)     { return ((SlipOp*)*getOperator())->div(*this, X); }

         SlipDatum  operator+(const bool   X)     { return ((SlipOp*)*getOperator())->add(*this, (LONG)X); }
         SlipDatum  operator-(const bool   X)     { return ((SlipOp*)*getOperator())->sub(*this, (LONG)X); }
         SlipDatum  operator*(const bool   X)     { return ((SlipOp*)*getOperator())->mul(*this, (LONG)X); }
         SlipDatum  operator/(const bool   X)     { return ((SlipOp*)*getOperator())->div(*this, (LONG)X); }
         SlipDatum  operator%(const bool   X)     { return ((SlipOp*)*getOperator())->mod(*this, (LONG)X); }

         SlipDatum  operator+(const UCHAR  X)     { return ((SlipOp*)*getOperator())->add(*this, (ULONG)X); } 
         SlipDatum  operator-(const UCHAR  X)     { return ((SlipOp*)*getOperator())->sub(*this, (ULONG)X); }
         SlipDatum  operator*(const UCHAR  X)     { return ((SlipOp*)*getOperator())->mul(*this, (ULONG)X); }
         SlipDatum  operator/(const UCHAR  X)     { return ((SlipOp*)*getOperator())->div(*this, (ULONG)X); }
         LONG       operator%(const UCHAR  X)     { return ((SlipOp*)*getOperator())->mod(*this, (ULONG)X); }

         SlipDatum  operator+(const CHAR   X)     { return ((SlipOp*)*getOperator())->add(*this, (LONG)X); }
         SlipDatum  operator-(const CHAR   X)     { return ((SlipOp*)*getOperator())->sub(*this, (LONG)X); }
         SlipDatum  operator*(const CHAR   X)     { return ((SlipOp*)*getOperator())->mul(*this, (LONG)X); }
         SlipDatum  operator/(const CHAR   X)     { return ((SlipOp*)*getOperator())->div(*this, (LONG)X); }
         LONG       operator%(const CHAR   X)     { return ((SlipOp*)*getOperator())->mod(*this, (LONG)X); }

         SlipDatum  operator+(const ULONG  X)     { return ((SlipOp*)*getOperator())->add(*this, X); }
         SlipDatum  operator-(const ULONG  X)     { return ((SlipOp*)*getOperator())->sub(*this, X); }
         SlipDatum  operator*(const ULONG  X)     { return ((SlipOp*)*getOperator())->mul(*this, X); }
         SlipDatum  operator/(const ULONG  X)     { return ((SlipOp*)*getOperator())->div(*this, X); }
         LONG       operator%(const ULONG  X)     { return ((SlipOp*)*getOperator())->mod(*this, X); }

         SlipDatum  operator+(const LONG   X)     { return ((SlipOp*)*getOperator())->add(*this, X); }
         SlipDatum  operator-(const LONG   X)     { return ((SlipOp*)*getOperator())->sub(*this, X); }
         SlipDatum  operator*(const LONG   X)     { return ((SlipOp*)*getOperator())->mul(*this, X); }
         SlipDatum  operator/(const LONG   X)     { return ((SlipOp*)*getOperator())->div(*this, X); }
         LONG       operator%(const LONG   X)     { return ((SlipOp*)*getOperator())->mod(*this, X); }

          /*************************************************
           *                Bit operators                  *
          *************************************************/
         LONG operator<<(const SlipDatum& X)      { return ((SlipOp*)*getOperator())->shl(*this,  X); }
         LONG operator>>(const SlipDatum& X)      { return ((SlipOp*)*getOperator())->shr(*this,  X); }
         LONG operator&(const SlipDatum&  X)      { return ((SlipOp*)*getOperator())->band(*this, X); }
         LONG operator|(const SlipDatum&  X)      { return ((SlipOp*)*getOperator())->bor(*this,  X); }
         LONG operator^(const SlipDatum&  X)      { return ((SlipOp*)*getOperator())->bor(*this,  X); }

         LONG operator<<(const bool   X)          { return (X)?((SlipOp*)*getOperator())->shl(*this, (LONG)1): (LONG)*this; }
         LONG operator>>(const bool   X)          { return (X)?((SlipOp*)*getOperator())->shr(*this, (LONG)1): (LONG)*this; }
         LONG operator&(const bool    X)          { return (X)?((SlipOp*)*getOperator())->band(*this, (LONG)1): 0; }
         LONG operator|(const bool    X)          { return (X)?((SlipOp*)*getOperator())->bor(*this, (LONG)1): (LONG)*this; }
         LONG operator^(const bool    X)          { return (X)?((SlipOp*)*getOperator())->bxor(*this, (LONG)1): (LONG)*this;}

         LONG operator<<(const UCHAR  X)          { return ((SlipOp*)*getOperator())->shl(*this,  (ULONG)X); }
         LONG operator>>(const UCHAR  X)          { return ((SlipOp*)*getOperator())->shr(*this,  (ULONG)X); }
         LONG operator&(const UCHAR   X)          { return ((SlipOp*)*getOperator())->band(*this, (ULONG)X); }
         LONG operator|(const UCHAR   X)          { return ((SlipOp*)*getOperator())->bor(*this,  (ULONG)X); }
         LONG operator^(const UCHAR   X)          { return ((SlipOp*)*getOperator())->bxor(*this, (ULONG)X); }

         LONG operator<<(const CHAR   X)          { return ((SlipOp*)*getOperator())->shl(*this,  (LONG)X); } 
         LONG operator>>(const CHAR   X)          { return ((SlipOp*)*getOperator())->shr(*this,  (LONG)X); }
         LONG operator&(const CHAR    X)          { return ((SlipOp*)*getOperator())->band(*this, (LONG)X); }
         LONG operator|(const CHAR    X)          { return ((SlipOp*)*getOperator())->bor(*this,  (LONG)X); }
         LONG operator^(const CHAR    X)          { return ((SlipOp*)*getOperator())->bxor(*this, (LONG)X); }

         LONG operator<<(const ULONG  X)          { return ((SlipOp*)*getOperator())->shl(*this,  X); }
         LONG operator>>(const ULONG  X)          { return ((SlipOp*)*getOperator())->shr(*this,  X); }
         LONG operator&(const ULONG   X)          { return ((SlipOp*)*getOperator())->band(*this, X); }
         LONG operator|(const ULONG   X)          { return ((SlipOp*)*getOperator())->bor(*this,  X); }
         LONG operator^(const ULONG   X)          { return ((SlipOp*)*getOperator())->bxor(*this, X); }

         LONG operator<<(const LONG   X)          { return ((SlipOp*)*getOperator())->shl(*this,  X); }
         LONG operator>>(const LONG   X)          { return ((SlipOp*)*getOperator())->shr(*this,  X); }
         LONG operator&(const LONG    X)          { return ((SlipOp*)*getOperator())->band(*this, X); }
         LONG operator|(const LONG    X)          { return ((SlipOp*)*getOperator())->bor(*this,  X); }
         LONG operator^(const LONG    X)          { return ((SlipOp*)*getOperator())->bxor(*this, X); }
         
          /*************************************************
           *             Assignment operators              *
          *************************************************/
         virtual SlipCell&  operator=(const SlipCell& X);                       // Y = V
         virtual SlipDatum& operator=(const bool    X);                         // Y = V
         virtual SlipDatum& operator=(const CHAR    X);                         // Y = V
         virtual SlipDatum& operator=(const DOUBLE  X);                         // Y = V
         virtual SlipDatum& operator=(const LONG    X);                         // Y = V
         virtual SlipDatum& operator=(const UCHAR   X);                         // Y = V
         virtual SlipDatum& operator=(const ULONG   X);                         // Y = V
         virtual SlipDatum& operator=(const PTR     X);                         // Y = V
         virtual SlipDatum& operator=(const string& X);                         // Y = V
         virtual SlipDatum& operator=(const string* X);                         // Y = V
         SlipDatum& operator=(const SlipDatum& X) { return (SlipDatum&)operator=((SlipCell&)X); }

          /*************************************************
           *        Compound Assignment operators          *
          *************************************************/
         SlipDatum& operator+=(const SlipDatum&  X) { return ((SlipOp*)*getOperator())->addAsgn(*this, X); }
         SlipDatum& operator-=(const SlipDatum&  X) { return ((SlipOp*)*getOperator())->subAsgn(*this, X); }
         SlipDatum& operator*=(const SlipDatum&  X) { return ((SlipOp*)*getOperator())->mulAsgn(*this, X); }
         SlipDatum& operator/=(const SlipDatum&  X) { return ((SlipOp*)*getOperator())->divAsgn(*this, X); }
         SlipDatum& operator%=(const SlipDatum&  X) { return ((SlipOp*)*getOperator())->modAsgn(*this, X); }
         SlipDatum& operator<<=(const SlipDatum& X) { return ((SlipOp*)*getOperator())->shlAsgn(*this, X); }
         SlipDatum& operator>>=(const SlipDatum& X) { return ((SlipOp*)*getOperator())->shrAsgn(*this, X); }
         SlipDatum& operator&=(const SlipDatum&  X) { return ((SlipOp*)*getOperator())->bandAsgn(*this, X);}
         SlipDatum& operator|=(const SlipDatum&  X) { return ((SlipOp*)*getOperator())->borAsgn(*this,  X);}
         SlipDatum& operator^=(const SlipDatum&  X) { return ((SlipOp*)*getOperator())->bxorAsgn(*this, X);}

         SlipDatum& operator+=(const DOUBLE  X)   { return ((SlipOp*)*getOperator())->addAsgn(*this, X); }
         SlipDatum& operator-=(const DOUBLE  X)   { return ((SlipOp*)*getOperator())->subAsgn(*this, X); }
         SlipDatum& operator*=(const DOUBLE  X)   { return ((SlipOp*)*getOperator())->mulAsgn(*this, X); }
         SlipDatum& operator/=(const DOUBLE  X)   { return ((SlipOp*)*getOperator())->divAsgn(*this, X); }

         SlipDatum& operator+=(const bool    X)   { return ((SlipOp*)*getOperator())->addAsgn(*this, (LONG)X); }
         SlipDatum& operator-=(const bool    X)   { return ((SlipOp*)*getOperator())->subAsgn(*this, (LONG)X); }

         SlipDatum& operator+=(const UCHAR   X)   { return ((SlipOp*)*getOperator())->addAsgn(*this, (ULONG)X); }
         SlipDatum& operator-=(const UCHAR   X)   { return ((SlipOp*)*getOperator())->subAsgn(*this, (ULONG)X); }
         SlipDatum& operator*=(const UCHAR   X)   { return ((SlipOp*)*getOperator())->mulAsgn(*this, (ULONG)X); }
         SlipDatum& operator/=(const UCHAR   X)   { return ((SlipOp*)*getOperator())->divAsgn(*this, (ULONG)X); }
         SlipDatum& operator%=(const UCHAR   X)   { return ((SlipOp*)*getOperator())->modAsgn(*this, (ULONG)X); }
         SlipDatum& operator<<=(const UCHAR  X)   { return ((SlipOp*)*getOperator())->shlAsgn(*this, (ULONG)X); }
         SlipDatum& operator>>=(const UCHAR  X)   { return ((SlipOp*)*getOperator())->shrAsgn(*this, (ULONG)X); }
         SlipDatum& operator&=(const UCHAR   X)   { return ((SlipOp*)*getOperator())->bandAsgn(*this, (ULONG)X);}
         SlipDatum& operator|=(const UCHAR   X)   { return ((SlipOp*)*getOperator())->borAsgn(*this, (ULONG)X); }
         SlipDatum& operator^=(const UCHAR   X)   { return ((SlipOp*)*getOperator())->bxorAsgn(*this, (ULONG)X);}

         SlipDatum& operator+=(const CHAR    X)   { return ((SlipOp*)*getOperator())->addAsgn(*this, (LONG)X); }
         SlipDatum& operator-=(const CHAR    X)   { return ((SlipOp*)*getOperator())->subAsgn(*this, (LONG)X); }
         SlipDatum& operator*=(const CHAR    X)   { return ((SlipOp*)*getOperator())->mulAsgn(*this, (LONG)X); }
         SlipDatum& operator/=(const CHAR    X)   { return ((SlipOp*)*getOperator())->divAsgn(*this, (LONG)X); }
         SlipDatum& operator%=(const CHAR    X)   { return ((SlipOp*)*getOperator())->modAsgn(*this, (LONG)X); }
         SlipDatum& operator<<=(const CHAR   X)   { return ((SlipOp*)*getOperator())->shlAsgn(*this, (LONG)X); }
         SlipDatum& operator>>=(const CHAR   X)   { return ((SlipOp*)*getOperator())->shrAsgn(*this, (LONG)X); }
         SlipDatum& operator&=(const CHAR    X)   { return ((SlipOp*)*getOperator())->bandAsgn(*this, (LONG)X);}
         SlipDatum& operator|=(const CHAR    X)   { return ((SlipOp*)*getOperator())->borAsgn(*this, (LONG)X); }
         SlipDatum& operator^=(const CHAR    X)   { return ((SlipOp*)*getOperator())->bxorAsgn(*this, (LONG)X);}

         SlipDatum& operator+=(const ULONG   X)   { return ((SlipOp*)*getOperator())->addAsgn(*this, X); }
         SlipDatum& operator-=(const ULONG   X)   { return ((SlipOp*)*getOperator())->subAsgn(*this, X); }
         SlipDatum& operator*=(const ULONG   X)   { return ((SlipOp*)*getOperator())->mulAsgn(*this, X); }
         SlipDatum& operator/=(const ULONG   X)   { return ((SlipOp*)*getOperator())->divAsgn(*this, X); }
         SlipDatum& operator%=(const ULONG   X)   { return ((SlipOp*)*getOperator())->modAsgn(*this, X); }
         SlipDatum& operator<<=(const ULONG  X)   { return ((SlipOp*)*getOperator())->shlAsgn(*this, X); }
         SlipDatum& operator>>=(const ULONG  X)   { return ((SlipOp*)*getOperator())->shrAsgn(*this, X); }
         SlipDatum& operator&=(const ULONG   X)   { return ((SlipOp*)*getOperator())->bandAsgn(*this, X);}
         SlipDatum& operator|=(const ULONG   X)   { return ((SlipOp*)*getOperator())->borAsgn(*this, X); }
         SlipDatum& operator^=(const ULONG   X)   { return ((SlipOp*)*getOperator())->bxorAsgn(*this, X);}

         SlipDatum& operator+=(const LONG    X)   { return ((SlipOp*)*getOperator())->addAsgn(*this, X); }
         SlipDatum& operator-=(const LONG    X)   { return ((SlipOp*)*getOperator())->subAsgn(*this, X); }
         SlipDatum& operator*=(const LONG    X)   { return ((SlipOp*)*getOperator())->mulAsgn(*this, X); }
         SlipDatum& operator/=(const LONG    X)   { return ((SlipOp*)*getOperator())->divAsgn(*this, X); }
         SlipDatum& operator%=(const LONG    X)   { return ((SlipOp*)*getOperator())->modAsgn(*this, X); }
         SlipDatum& operator<<=(const LONG   X)   { return ((SlipOp*)*getOperator())->shlAsgn(*this, X); }
         SlipDatum& operator>>=(const LONG   X)   { return ((SlipOp*)*getOperator())->shrAsgn(*this, X); }
         SlipDatum& operator&=(const LONG    X)   { return ((SlipOp*)*getOperator())->bandAsgn(*this, X);}
         SlipDatum& operator|=(const LONG    X)   { return ((SlipOp*)*getOperator())->borAsgn(*this, X);}
         SlipDatum& operator^=(const LONG    X)   { return ((SlipOp*)*getOperator())->bxorAsgn(*this, X);}

          /*************************************************
           *              Logical operators                *
          *************************************************/
         virtual bool  operator==(const SlipCell& X);                           // Y == X

         bool       operator!=(const SlipDatum& X) { return !((SlipOp*)*getOperator())->equal(*this,     X); }
         bool       operator<(const SlipDatum&  X) { return ((SlipOp*)*getOperator())->lessThan(*this,   X); }
         bool       operator<=(const SlipDatum& X) { return ((SlipOp*)*getOperator())->lessEqual(*this,  X); }
         bool       operator==(const SlipDatum& X) { return ((SlipOp*)*getOperator())->equal(*this,      X); }
         bool       operator>=(const SlipDatum& X) { return ((SlipOp*)*getOperator())->greatEqual(*this, X); }
         bool       operator>(const SlipDatum&  X) { return ((SlipOp*)*getOperator())->greatThan(*this,  X); }

         bool       operator!=(const bool   X)    { return !((SlipOp*)*getOperator())->equal(*this,     (LONG)X); }
         bool       operator<(const bool    X)    { return ((SlipOp*)*getOperator())->lessThan(*this,   (LONG)X); }
         bool       operator<=(const bool   X)    { return ((SlipOp*)*getOperator())->lessEqual(*this,  (LONG)X); }
         bool       operator==(const bool   X)    { return ((SlipOp*)*getOperator())->equal(*this,      (LONG)X); }
         bool       operator>=(const bool   X)    { return ((SlipOp*)*getOperator())->greatEqual(*this, (LONG)X); }
         bool       operator>(const bool    X)    { return ((SlipOp*)*getOperator())->greatThan(*this,  (LONG)X); }

         bool       operator!=(const DOUBLE X)    { return !((SlipOp*)*getOperator())->equal(*this,     X); }
         bool       operator<(const DOUBLE  X)    { return ((SlipOp*)*getOperator())->lessThan(*this,   X); }
         bool       operator<=(const DOUBLE X)    { return ((SlipOp*)*getOperator())->lessEqual(*this,  X); }
         bool       operator==(const DOUBLE X)    { return ((SlipOp*)*getOperator())->equal(*this,      X); }
         bool       operator>=(const DOUBLE X)    { return ((SlipOp*)*getOperator())->greatEqual(*this, X); }
         bool       operator>(const DOUBLE  X)    { return ((SlipOp*)*getOperator())->greatThan(*this,  X); }

         bool       operator!=(const UCHAR  X)    { return !((SlipOp*)*getOperator())->equal(*this,     (ULONG)X); }
         bool       operator<(const UCHAR   X)    { return ((SlipOp*)*getOperator())->lessThan(*this,   (ULONG)X); }
         bool       operator<=(const UCHAR  X)    { return ((SlipOp*)*getOperator())->lessEqual(*this,  (ULONG)X); }
         bool       operator==(const UCHAR  X)    { return ((SlipOp*)*getOperator())->equal(*this,      (ULONG)X); }
         bool       operator>=(const UCHAR  X)    { return ((SlipOp*)*getOperator())->greatEqual(*this, (ULONG)X); }
         bool       operator>(const UCHAR   X)    { return ((SlipOp*)*getOperator())->greatThan(*this,  (ULONG)X); }

         bool       operator!=(const CHAR   X)    { return !((SlipOp*)*getOperator())->equal(*this,     (LONG)X); }
         bool       operator<(const CHAR    X)    { return ((SlipOp*)*getOperator())->lessThan(*this,   (LONG)X); }
         bool       operator<=(const CHAR   X)    { return ((SlipOp*)*getOperator())->lessEqual(*this,  (LONG)X); }
         bool       operator==(const CHAR   X)    { return ((SlipOp*)*getOperator())->equal(*this,      (LONG)X); }
         bool       operator>=(const CHAR   X)    { return ((SlipOp*)*getOperator())->greatEqual(*this, (LONG)X); }
         bool       operator>(const CHAR    X)    { return ((SlipOp*)*getOperator())->greatThan(*this,  (LONG)X); }

         bool       operator!=(const ULONG  X)    { return !((SlipOp*)*getOperator())->equal(*this,     X); }
         bool       operator<(const ULONG   X)    { return ((SlipOp*)*getOperator())->lessThan(*this,   X); }
         bool       operator<=(const ULONG  X)    { return ((SlipOp*)*getOperator())->lessEqual(*this,  X); }
         bool       operator==(const ULONG  X)    { return ((SlipOp*)*getOperator())->equal(*this,      X); }
         bool       operator>=(const ULONG  X)    { return ((SlipOp*)*getOperator())->greatEqual(*this, X); }
         bool       operator>(const ULONG   X)    { return ((SlipOp*)*getOperator())->greatThan(*this,  X); }

         bool       operator!=(const LONG   X)    { return !((SlipOp*)*getOperator())->equal(*this,     X); }
         bool       operator<(const LONG    X)    { return ((SlipOp*)*getOperator())->lessThan(*this,   X); }
         bool       operator<=(const LONG   X)    { return ((SlipOp*)*getOperator())->lessEqual(*this,  X); }
         bool       operator==(const LONG   X)    { return ((SlipOp*)*getOperator())->equal(*this,      X); }
         bool       operator>=(const LONG   X)    { return ((SlipOp*)*getOperator())->greatEqual(*this, X); }
         bool       operator>(const LONG    X)    { return ((SlipOp*)*getOperator())->greatThan(*this,  X); }

         bool       operator!=(const PTR    X)    { return !((SlipOp*)*getOperator())->equal(*this,     X); }
         bool       operator==(const PTR    X)    { return ((SlipOp*)*getOperator())->equal(*this,      X); }

         bool       operator!=(const string&  X);                               // Y != V
         bool       operator<(const string&   X);                               // Y <  V
         bool       operator<=(const string&  X);                               // Y != V
         bool       operator==(const string&  X);                               // Y <= V
         bool       operator>=(const string&  X);                               // Y >= V
         bool       operator>(const string&   X);                               // Y >  V

         bool       operator!=(const string*  X);                               // Y != V
         bool       operator<(const string*   X);                               // Y <  V
         bool       operator<=(const string*  X);                               // Y != V
         bool       operator==(const string*  X);                               // Y <= V
         bool       operator>=(const string*  X);                               // Y >= V
         bool       operator>(const string*   X);                               // Y >  V

               /*****************************************
                *          Manipulating Lists           *
               ******************************************/
         virtual SlipCell& replace(const SlipCell& X);                          // Replace a cell on a list with a new cell
         virtual SlipCell& replace(bool X);                                     // Replace a cell on a list with a new cell
         virtual SlipCell& replace(CHAR X);                                     // Replace a cell on a list with a new cell
         virtual SlipCell& replace(UCHAR X);                                    // Replace a cell on a list with a new cell
         virtual SlipCell& replace(LONG X);                                     // Replace a cell on a list with a new cell
         virtual SlipCell& replace(ULONG X);                                    // Replace a cell on a list with a new cell
         virtual SlipCell& replace(DOUBLE X);                                   // Replace a cell on a list with a new cell
         virtual SlipCell& replace(const PTR X, const void* operation = ptrOP); // Replace a cell on a list with a new cell
         virtual SlipCell& replace(const string& X, bool constFlag = false);    // Replace a cell on a list with a new cell
         virtual SlipCell& replace(const string* X, bool constFlag = false);    // Replace a cell on a list with a new cell

               /*****************************************
                *             Miscellaneous             *
               ******************************************/
         Parse              getParse();                                         // Return a pointer to the User Data parse method
         virtual string     write();                                            // Return a write formatted string
         virtual string     dump() const;                                       // dump cell
         virtual void       dump(ostream& out, int& col);                       // Output the current cell
   }; // class SlipDatum
}; // namespace slip
#endif  /* SLIPDATUM_H */

