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
 * @file    SlipOp.h
 * @author: A. Schwarz
 * @date November 16, 2011, 10:59 AM
 *
 * @brief File containing the invariant declarations for all SlipCellBase derived classes.
 * @see SlipCellBase.h
 * @see SlipHeader.h
 * @see SlipSublist.h
 * @see SlipDatum.h
 * @see SlipReaderCell.h
 *
 */

#ifndef _SLIPOP_H
#define	_SLIPOP_H

# include <string>
# include "SlipDef.h"
# include "SlipCellBase.h"
# include "SlipErr.h"

using namespace std;
using namespace slip;

namespace slip {

   class SlipDatum;
   class SlipCell;
   
/**
 * @class SlipOp
 * @brief Base class for SlipCellBase derived class invariants and operations.
 * <p><b>Overview</b></p><hr>
 * <p>Invariant properties and operations shared by all members of a given class
 *    are identified here and implemented in classes derived from SlipOp. The
 *    properties identify characteristics of a class necessary to identify it
 *    and retrieve classwide properties, and the operations provide a means for
 *    class members to participate in unary, arithmetic, bit, cast, and relational 
 *    operations.</p>
 * <p>Invariant membership is determined by the operations field of all available
 *    space list (AVSL) objects. The operations field is set when a class object
 *    is instantiated, either on the stack (SlipName()) or from the AVSL
 *    (new SlipName()). The AVSL structure is given here:</p>
 *  <table>
 *    <caption align="top">SlipCellBase structure</caption>
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
 *    <li><b>operator</b> pointer to a SlipOp derived object. There is one
 *        and only one copy of each SlipOp derived object. They are located
 *        in SlipGlobal.h. This field is referenced internally by the SLIP
 *        kernel.</li>
 *    <li><b>Datum</b> defined by the derived object.</li>
 * </ul>
 * 
 * <p>Class objects are defined as a <b>SlipClass.type</b> where the class is 
 *    the SLIP class the object is a member of, and class defines any subclassing
 *    associated with the object. In object sharing a unique class but having
 *    differing types, all SLIP operations that the base class has is shared
 *    by all objects derived from the class. However, where types differ the
 *    invariant properties and operations differ and are distinguished by 
 *    having a pointer to differing SlipOp derived objects.</p>
 * <p>For SlipHeader, SlipReaderCell, and SlipSublist the class is self-defining
 *    and all objects instantiated from these classes share in the  same 
 *    invariants. The terminology is SlipClass.type, as in SlipHeader.SlipHeader.
 *    </p>
 * <p>For the SlipDatum class the invariants deal with distinguishable members 
 *    of this class which have differing properties and operations. The 
 *    terminology is SlipDatum.type, where type is any of bool, CHAR, UCHAR, 
 *    LONG, ULONG, DOUBLE, PTR, and STRING.</p>
 * <p>In SLIP, the list operations are static by class (or SlipClass.type) and 
 *    are instantiated once. The operations pointer placed in the operations 
 *    field is the same for each object of the same SlipClass.type.</p>
 * <p>All objects have the ability to do comparisons (<em>==</em>, <em>!=</em>)
 *    but only SlipDatum.type objects have operations (unary, arithmetic, bit, 
 *    cast, and relational).</p>
 * <p>This is summarized in the following table:</p>
 * <p><center><b>SlipOp Derived Objects</b></center></p>
 * <tt><center>
 * | SlipClass.type                | Object Name | operations | relational |
 * | :---------------------------- | :---------- | :--------: | :--------: |
 * | SlipHeader.SlipHeader         | headOP      |            |            |
 * | SlipReaderCell.SlipReaderCell | readerOP    |            |            |
 * | SlipSublist.SlipSublist       | sublistOP   |            |            |
 * | SlipDatum.bool                | boolOP      |      X     |      X     |
 * | SlipDatum.CHAR                | charOP      |      X     |      X     |
 * | SlipDatum.UCHAR               | ucharOP     |      X     |      X     |
 * | SlipDatum.LONG                | longOP      |      X     |      X     |
 * | SlipDatum.ULONG               | ulongOP     |      X     |      X     |
 * | SlipDatum.DOUBLE              | doubleOP    |      X     |      X     |
 * | SlipDatum.PTR                 | ptrOP       |            |            |
 * | SlipDatum.STRING              | stringOP    |            |      X     |
 * | undefined                     | undefinedOP |            |            |
 * </center></tt>
 * <p>There is, of course, one exception. <em>undefined</em> is not an object
 *    of any class. It is a cell located in the AVSL which has not been assigned.
 *    There are no valid operations associated with it although it does have
 *    properties. This cell is never a part of any SLIP defined list or process
 *    except its removal from the AVSL during object creation during 
 *    (<em>new</em>).</p>
 * <p>Where all SlipClass.types have <em>==</em>, and <em>!=</em> and:</p>
 * <ul>
 *    <li><b>SlipClass.type</b> the Slip class name and the associated type. If
 *        the class does not have any associated types, then the class name is
 *        repeated.</li>
 *    <li><b>Object Name</b> the name of the <em>const</em> object in 
 *        SlipGlobal.h containing a pointer to the SlipOp derived object   
 *        containing the class invariances.</li>
 *    <li><b>Operations</b> a check (<b>X</b>) indicates that the SlipClass.type
 *        has unary, arithmetic, bit, and cast operations.</li>
 *    <li><b>Relational</b> a check (<b>X</b>) indicates that the SlipClass.type
 *        has relational (<em>&lt;, &lt;=, %gt;=, %gt;</em>) operations.</li>
 * </ul>
 * <p><b>Invariant Properties</b></p><hr>
 * <p>A list of the invariant properties is:</p>
 * <center><b>Invariant Property List</b></center>
 * <center>
 * | property       | description                                                   |
 * | :------------- | :------------------------------------------------------------ |
 * | dump()         | return a string representing the AVSL cell for the object     |
 * | getCellType()  | return the cell subtype                                       |
 * | getClassType() | return the unique class type                                  |
 * | getID()        | return the unique Cell ID                                     |
 * | getName()      | string for SlipCell.type                                      |
 * | isData()       | <b>true</b> if cell is SlipDatum.{}                                  |
 * | isDiscrete()   | <b>true</b> for SlipDatum.{bool, CHAR, UCHAR, LONG,OR ULONG}         |
 * | isHeader()     | <b>true</b> for SlipHeader.SlipHeader                                |
 * | isNumber()     | <b>true</b> for SlipDatum.{DOUBLE, bool, CHAR, UCHAR, LONG,or ULONG} |
 * | isPtr()        | <b>true</b> if cell is SlipDatum.PTR                                 |
 * | isReal()       | <b>true</b> if cell is SlipDatum.DOUBLE                              |
 * | isString()     | <b>true</b> if cell is SlipDatum.STRING                              |
 * | isSublist()    | <b>true</b> for SlipSublist.SlipSublist                              |
 * | toString()     | return a string representing the pretty-printed SLIP object   |
 * | write()        } return a string formatted for a write                         |
 * </center>
 * <p>The invariant properties are available from each SlipCellBase derived 
 *    object. For SlipHeader, SlipSublit, and SlipDatum objects the returned
 *    value represents a property of the object. For the SlipReader (not the
 *    SlipReaderCell) the returned value represents the property of the SlipCell
 *    referenced by the reader.</p>
 * <p>Some small examples of usage are:</p>
 * <tt><pre>
 *    SlipDatum     datum((LONG) 5);             // create a SlipDatum.LONG object
 *    SlipHeader  * header = new SlipHeader();   // create a SlipHeader.SlipHeader object
 *                                               // create a SlipSublist.SlipSublist object
 *    SlipSublist * sublist = new SlipSublist(*header);
 *    SlipReader    reader(*header);             // create a SlipReader object
 *    
 *    bool isdata   = datum.isData();            // yields <b>true</b>
 *    bool isnumber = datum.isNumber();          // yields <b>true</b>
 *    bool isheader = header->isHeader();        // yields <b>true</b>
 *    bool ishead   = reader.isHeader();         // yields <b>true</b>
 *    bool issublist= sublist->isSublist();      // yields <b>true</b>
 *    
 *    reader.insRight(datum);
 *    bool whatAmI  = reader.isData();           // yields <b>false</b>
 *    reader.advanceLWR();
 *    bool andNow   = reader.isData();           // yields <b>true</b>
 * </pre></tt>
 * <p>In the following table the type in SlipName.type is presented. The table
 *    provides the values return for each property for each SlipName.type.</p>
 * <center><b>Invariant Property Values by Type</b></center>
 * <center>
 * | property       | header | sublist | reader | bool  |  CHAR  | UCHAR   | LONG  | ULONG   | DOUBLE | PTR  | STRING |
 * | :------------- | :----- | :------ | :------| :---- | :----- | :------ | :---- | :------ | :----- | :--- | :----- |
 * | dump()         |        |         |        |       |        |         |       |         |        |      |        |
 * | getClassType() | eHEADER| eSUBLIST| eREADER| eBOOL | eCHAR  | eUCHAR  | eLONG | eULONG  | eDOUBLE| ePTR | eSTRING|
 * | getName()      | HEADER | SUBLIST | READER | bool  | CHAR   | UCHAR   | LONG  | ULONG   | DOUBLE | PTR  | STRING |
 * | isData()       | false  | false   | false  | true  | true   | true    | true  | true    | true   | true | true   |
 * | isDiscrete()   | false  | false   | false  | true  | true   | true    | true  | true    | false  | false| false  |
 * | isHeader()     | true   | false   | false  | false | false  | false   | false | false   | false  | false| false  |
 * | isNumber()     | false  | false   | false  | true  | true   | true    | true  | true    | true   | false| false  |
 * | isPtr()        | false  | false   | false  | false | false  | false   | false | false   | false  | true | false  |
 * | isReal()       | false  | false   | false  | false | false  | false   | false | false   | true   | false| false  |
 * | isString()     | false  | false   | false  | false | false  | false   | false | false   | false  | false| true   |
 * | isSublist()    | false  | true    | false  | false | false  | false   | false | false   | false  | false| false  |
 * | toString()     | dump() | dump()  | dump() | T/F   | number | number  | number| number  | number | user | string |
 * | write()        | ( )    | { )     |        | T/F   | numberC| numberUC| number| numberUL| number | ( )  |"string"|
 * </center>
 * <p>The table rows are:</p>
 * <ul>
 *    <li><b>getClassType()</b> a unique enumerated type in SlipDef.h 
 *        uniquely identifying the type.</li>
 *    <li><b>getName()</b> a string value representing the class.</li>
 *    <li><b>isData()</b> <b>true</b> if this is a SlipDatum object.</li>
 *    <li><b>isDiscrete()</b> <b>true</b> if this is a discrete number.</li>
 *    <li><b>isHeader()</b> <b>true</b> if this is a SlipHeader object.</li>
 *    <li><b>isNumber()</b> <b>true</b> if this is numeric data or a bool.</li>
 *    <li><b>isPtr()</b> <b>true</b> if this is a user defined object
 *        (SlipDatum.PTR)</li>
 *    <li><b>isReal()</b> <b>true</b> true if this is a SlipDatum.DOUBLE object.</li>
 *    <li><b>isString()</b> <b>true</b> if this is a SlipDatum.STRING object.</li>
 *    <li><b>isSublist()</b> <b>true</b> if this is a SlipSublist object.</li>
 *    <li><b>toString()</b> return a string representing the pretty-printing
 *        version of the object. SlipHeader, SlipReader, and SlipSublist
 *        return a dump(). SlipDatum.PTR returns a user defined string. The
 *        remainder are numeric strings, true/false, the SlipDatum.STRING
 *        value.</li>
 *    <li><b>dump()</b> all objects except SlipReader have their left link, 
 *        right link, and operator pointer printed in hex followed by a type 
 *        specific output.</li>
 *    <li><b>write()</b> value formatted for a write to file.</li>
 * </ul>
 * 
 * <p><b>Operations</b></p><hr>
 * <p></p>
 * 
 * <p>This class is the template for the implementation specific execution of 
 *    all SlipDatum object operation overrides. The operations include all the 
 *    object specific unary, cast,  and binary operations that a cell can be a 
 *    participate in. This includes unary and binary arithmetic, bit, cast, and 
 *    relational operations.
 * </p>
 * <p>Access to the SlipOp derived classes is through the operator field in the 
 *    SlipCellBase AVSL cell. The operator field is a direct pointer to a SlipOP
 *    derived object and each object supports the required operations specific 
 *    to the type of SlipDatum.type.</p>
 * <p>The SlipOp derived classes is the implementation of the operation defined
 *    in SlipDatum and will not be redefined here. Access to type specific
 *    operations is made through the operator field, as in:
 * <ul>
 *    <li><b>unary operator</b> operator->name(SlipDatum& X).</li>
 *    <li><b>cast operator</b> operator.name(SlipDatum& X).</li>
 *    <li><b>binary operator</b> operator->name(SlipDatum& LHS, SlipDatum& RHS).</li>
 *    <li><b>relational operator</b> operator->name(SlipDatum& LHS, SlipDatum& RHS).</li>
 *    <li><b>assignment operator</b> operator->name(SlipDatum& LHS, SlipDatum& RHS).</li>
 * </ul>
 *    
 * <p>
 * Casting an object returns the casting type if possible. If the type of the
 * SlipDatum data is the same as the cast, the value of the data returned will
 * be bitwise identical to the datum in the cell. If there is a need to 
 * explicitely retrieve the value of a cell then cast the SlipDatum cell to the 
 * type of cell. With all of the C++ operations supported there may be little
 * need for this.
 * <p>
 * Post/Pre- increment/decrement unary operations alter the datum value in the
 * SlipCell and return a reference to the input SlipDatum cell to the user. 
 * Unary operations which do not alter the contents of the input cell
 * (<em>+</em> and <em>-</em>) return a temporary SlipDatum cell or a ULONG
 * (<em>~</em>) or bool (<em>!</em>).</p>
 * <p> 
 * relational operations (&lt;, &lt;=, ==, &gt;=, &gt;, !=) return a bool 
 * representing the result of the comparison. The SlipDatum Datum is not 
 * changed.</p>
 * <p>Bit (<em>&amp;</em>, <em>|</em>, <em>^</em>) and the modulus 
 * (<em>%</em>) operation return a LONG.</p>
 * <p>The remaining operations return a SlipDatum cell. For assignment operations
 * a reference the input cell (the LHS) is returned. For the remaining binary
 * operations, a temporary SlipDatum object is created on the stack and 
 * returned.</p>
 * <p>
 * Bools are treated as '1' if true or '0' if false as needed.</p>
 * <p>Examples:</p>
 * <tt><pre>
 *    SlipDatum U((LONG)4);
 *    SlipDatum X((bool)true);
 *    SlipDatum Y((bool)true);
 *    SlipDatum Z((ULONG)0);
 *    Z = Y + Z;              // Z == 2 (1 + 1) 
 *                            //   operator=(Z, ooperator+((LONG)1 + (LONG)1))
 *                            //   operator=(Z, SlipDatum((LONG)2)
 *    (ULONG)Z;               // casts to 1
 *                            //   operator(ULONG)(Z)
 * 
 *    Z = (ULONG)1 + (CHAR)3; // Z = 4;
 *                            //   operator=(Z, operator+(ULONG(1) + (CHAR)3)
 *                            //   operator=(Z, operator+(ULONG(1) + (LONG)3)
 *                            //   operator=(Z, SlipDatum((LONG)4)
 *    ++U;                    //   U == 5
 *                            //   operator++(U);
 * </pre></tt>
 * 
 * <p>
 * The operations supported,s their usage and general syntax are:
 * <p>
 * <center>
 * <table border = "1">
 * <caption align="top">SlipDatum Operations Supported</caption>
 * <tr>
 *   <th>Return Type</th>
 *   <th>Usage</th>
 *   <th>SlipOp Method</th>
 * </tr>
 *   <tr>
 *     <td>bool</td>
 *     <td>(bool) Y</td>
 *     <td>cast_bool(Y)</td>
 *   </tr>
 *   <tr>
 *     <td>UCHAR</td>
 *     <td>(UCHAR) Y</td>
 *     <td>cast_UCHAR(Y)</td>
 *   </tr>
 *   <tr>
 *     <td>CHAR</td>
 *     <td>(CHAR) Y</td>
 *     <td>cast_UCHAR(Y)</td>
 *   </tr>
 *   <tr>
 *     <td>ULONG</td>
 *     <td>(ULONG) Y</td>
 *     <td>cast_ULONG(Y)</td>
 *   </tr>
 *   <tr>
 *     <td>LONG</td>
 *     <td>(LONG) Y</td>
 *     <td>cast_LONG(Y)</td>
 *   </tr>
 *   <tr>
 *     <td>DOUBLE</td>
 *     <td>(DOUBLE) Y</td>
 *     <td>cast_DOUBLE(Y)</td>
 *   </tr>
 *   <tr>
 *     <td>LONG</td>
 *     <td>~Y</td>
 *     <td>bitNOT(SlipDatum&)</td>
 *   </tr>
 *   <tr>
 *     <td>bool</td>
 *     <td>!Y</td>
 *     <td>logNOT(SlipDatum&)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum</td>
 *     <td>+Y</td>
 *     <td>plus(SlipDatum&)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum</td>
 *     <td>-Y</td>
 *     <td>minus(Y)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum&amp;</td>
 *     <td>Y++</td>
 *     <td>postInc(Y)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum&amp;</td>
 *     <td>Y--</td>
 *     <td>postDec(Y)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum&amp;</td>
 *     <td>++Y</td>
 *     <td>preInc(Y)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum&amp;</td>
 *     <td>--Y</td>
 *     <td>preDec(Y)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum</td>
 *     <td>Y + X</td>
 *     <td>add(const Y, const X)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum</td>
 *     <td>Y - X</td>
 *     <td>sub(const Y, const X)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum</td>
 *     <td>Y * X</td>
 *     <td>mul(const Y, const X)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum</td>
 *     <td>Y / X</td>
 *     <td>diX(const Y, const X)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum</td>
 *     <td>Y % X</td>
 *     <td>mod(const Y, const X)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum</td>
 *     <td>Y &lt;&lt; X</td>
 *     <td>shl(const Y, const X)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum</td>
 *     <td>Y &gt;&gt; X</td>
 *     <td>shr(const Y, const X)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum</td>
 *     <td>Y &amp; X</td>
 *     <td>band(const Y, const X)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum</td>
 *     <td>Y | X</td>
 *     <td>bor(const Y, const X)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum</td>
 *     <td>Y ^ X</td>
 *     <td>Yor(Y, const X)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum&amp;</td>
 *     <td>LHS = RHS</td>
 *     <td>asgn(LHS, const RHS)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum&amp;</td>
 *     <td>LHS += RHS</td>
 *     <td>addAsgn(LHS, const RHS)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum&amp;</td>
 *     <td>LHS -= RHS</td>
 *     <td>minAsgn(LHS, const RHS)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum&amp;</td>
 *     <td>LHS /= RHS</td>
 *     <td>diRHSAsgn(LHS, const RHS)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum&amp;</td>
 *     <td>LHS *= RHS</td>
 *     <td>mulAsgn(LHS, const RHS)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum&amp;</td>
 *     <td>LHS \%= RHS</td>
 *     <td>modAsgn(LHS, const RHS)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum&amp;</td>
 *     <td>LHS &amp;= RHS</td>
 *     <td>bandAsgn(LHS, const RHS)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum&amp;</td>
 *     <td>LHS |= RHS</td>
 *     <td>borAsgn(LHS, const RHS)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum&amp;</td>
 *     <td>LHS ^= RHS</td>
 *     <td>bRHSorAsgn(LHS, const RHS)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum&amp;</td>
 *     <td>LHS &lt;&lt;= RHS</td>
 *     <td>shlAsgn(LHS, const RHS)</td>
 *   </tr>
 *   <tr>
 *     <td>SlipDatum&amp;</td>
 *     <td>LHS &gt;&gt;= RHS</td>
 *     <td>shrAsgn(LHS, const RHS)</td>
 *   </tr>
 *   <tr>
 *     <td>bool</td>
 *     <td>LHS != RHS</td>
 *     <td>notEqual(LHS, const RHS)</td>
 *   </tr>
 *   <tr>
 *     <td>bool</td>
 *     <td>LHS &lt; RHS</td>
 *     <td>less(LHS, const RHS)</td>
 *   </tr>
 *   <tr>
 *     <td>bool</td>
 *     <td>LHS &lt;= </td>
 *     <td>lessEqual(LHS, const RHS)</td>
 *   </tr>
 *   <tr>
 *     <td>bool</td>
 *     <td>LHS == RHS</td>
 *     <td>equal(LHS, const RHS)</td>
 *   </tr>
 *   <tr>
 *     <td>bool</td>
 *     <td>LHS &gt;= RHS</td>
 *     <td>greatEqual(LHS, const RHS)</td>
 *   </tr>
 *   <tr>
 *     <td>bool</td>
 *     <td>LHS &gt; RHS</td>
 *     <td>greatThan(LHS, const RHS)</td>
 *   </tr>
 * </table>
 * </center>
 * <p>
 * <b>LEGEND</b>
 * <table>
 *   <tr>
 *     <td>Return type</td>
 *     <td>The object returned.</td>
 *   </tr>
 *   <tr>
 *     <td>usage</td>
 *     <td>Exemplar syntax showing the general form of the operation in source code.</td>
 *   </tr>
 *   <tr>
 *     <td>SlipOp Method</td>
 *     <td>Virtual SlipOP method used to implement the operation.</td>
 *   </tr>
 *   <tr>
 *     <td>Y</td>
 *     <td>A reference to the LHS SlipDatum& operand.</td>
 *   </tr>
 *   <tr>
 *     <td>X</td>
 *     <td>The RHS operand. This can be a native object (bool, CHAR, UCHAR,
 *         LONG, ULONG or DOUBLE), a user object (PTR), a STRING or a 
 *         SlipDatum reference as appropriate.</td>
 *   </tr>
 *   <tr>
 *     <td>LHS</td>
 *     <td>Left hand side of an assignement operator.</td>
 *   </tr>
 *   <tr>
 *     <td>RHS</td>
 *     <td>Right hand side ofan assignement operator.</td>
 *   </tr>
 * </table>
 * <p><b>Non-Commutative Methods</b></p><hr>
 * <p>Non-commutative methods support operations where <em>X op Y</em> is not 
 *    the same as <em>Y op X</em>, op being and operation. For example,
 *    <em>X + Y</em> has the same result as <em>Y + X</em> but
 *    <em>X &lt;&lt; Y</em> and <em>Y &lt;&lt;</em> is different.</p>
 * <p>If X is a value and Y is a SlipDatum cell then <em>Y op X</em> is 
 *    treated by SlipOp directly, but where X is a SlipDatum object and
 *    Y is a value there are some operations where <em>Y op X</em> is
 *    a non-commutative operation. These situations occur in non-member 
 *    method evaluation involving a SlipDatum object and a value.</p>
 * <p>Member evaluation occurs when the left side of a binary expression is
 *    a SlipDatum object. Non-member evaluations occur when the SlipDatum
 *    object is the right side of a binary equation. The non-Comumutative
 *    methods directly support the non-member methods in slip.cpp.</p>
 * <p>User Defined Data types must implement the non-member methods.</p>
 * 
 * <center><b>Non-Commutative Methods</b></center>
 * <tt><center>
 * | Return Type |             Method Prototype               |               Description                |
 * | :---------- | :----------------------------------------- | :--------------------------------------- |
 * | SlipDatum   | divOP(const LONG Y, const SlipDatum X)     | (LONG)  / Y; return LONG or DOUBLE       |
 * | SlipDatum   | divUOP(const ULONG Y, const SlipDatum X)   | (ULONG) / Y; return LONG or DOUBLE       |
 * | SlipDatum   | modOP(const LONG Y, const SlipDatum X)     | (LONG)  % Y; return LONG or DOUBLE       |
 * | SlipDatum   | modUOP(const ULONG Y, const SlipDatum X)   | (ULONG) % Y; return LONG or DOUBLE       |
 * | SlipDatum   | subOP(const LONG Y, const SlipDatum X)     | (LONG)  - Y; return LONG or DOUBLE       |
 * | SlipDatum   | subUOP(const ULONG Y, const SlipDatum X)   | (ULONG) - Y; return LONG or DOUBLE       |
 * | LONG        | shlOP(const LONG Y, const SlipDatum X)     | (LONG)  &lt;&lt; Y;                         |
 * | LONG        | shrOP(const LONG Y, const SlipDatum X)     | (LONG)  &gt;&gt; Y;                            |
 * </center></tt>
 * <p><b>Utility Methods</b></p><hr>
 * <p>The utility methods support implementation. They are all static members,
 *    globally accessible to all derived SlipOp objects performing some function
 *    required for execution. They can be roughly aggregated into convenience
 *    methods which extract data from a cell, and deferred methods which 
 *    have some role in deriving a useful result.</p>
 * <p>Convenience methods return a result based on the input cell. They are 
 *    provided here to override the same method in SlipCellBase. This is a 
 *    pragmatic decision which minimizes the number of <em>friends</em> 
 *    required in SlipCellBase to only SlipOp. They are then a 'convenience' 
 *    which avoids 'dirty and irremediable' code.</p>
 *<p>The methods, description and return type are given below.</p>
 * <center><b>Convenience Methods</b></center>
 * <tt><center>
 * | Return Type |             Method Prototype               |               Description                |
 * | :---------- | :----------------------------------------- | :--------------------------------------- |
 * | string      | dumpLink(const SlipCellBase& X)            | Cell left/right link & operation pointer |
 * | CHAR*       | getChar(const SlipCellBase& X)             | Cell CHAR data value                     |
 * | bool*       | getBool(const SlipCellBase& X)             | Cell bool data value                     |
 * | sAllData*   | getData(const SlipCellBase& X)             | Cell data value field                    |
 * | DOUBLE*     | getDouble(const SlipCellBase& X)           | Cell DOUBLE data value                   |
 * | void**      | getHeadDescList(const SlipCellBase& X)     | Cell Description List header pointer     |
 * | USHORT*     | getHeadMrk(const SlipCellBase& X)          | Cell Header Mark field data value        |
 * | USHORT*     | getHeadRefCnt(const SlipCellBase& X)       | Cell Header Ref field data value         |
 * | LONG*       | getLong(const SlipCellBase& X)             | Cell LONG data value                     |
 * | PTR*        | getPtr(const SlipCellBase& X)              | Cell SlipPointer pointer                 |
 * | void**      | getOperator(const SlipCellBase& X)         | Cell operator field value                |
 * | USHORT*     | getReaderDepth(SlipCellBase& X)            | Reader Depth field value                 |
 * | void**      | getReaderTop(SlipCellBase& X)              | Reader Header List Pointer               |
 * | void        | resetData(const SlipCellBase& X)           | Reset the data field to zero             |
 * | STRING*     | getString(const SlipCellBase& X)           | Cell SlipStringConst/SlipStringNonConst  |
 * | void**      | getSublistHeader(const SlipCellBase& X)    | Cell Sublist Header pointer data value   |
 * | UCHAR*      | getUChar(const SlipCellBase& X)            | Cell UCHAR data value                    |
 * | ULONG*      | getULong(const SlipCellBase& X)            | Cell ULONG data value                    |
 * </center></tt>
 */
/**
 * @fn virtual bool SlipOp::cast_bool(const SlipDatum& X) const 
 * @copydoc SlipDatum::operator bool()
 * @see SlipDatum::operator bool()
 * @param[in] X (SlipDatum&) operand
 *
 * @fn virtual UCHAR SlipOp::cast_UCHAR(const SlipDatum& X) const
 * @copydoc SlipDatum::operator UCHAR()
 * @see SlipDatum::operato UCHAR()
 * @param[in] X (SlipDatum&) operand
 *
 * @fn virtual CHAR SlipOp::cast_CHAR(const SlipDatum& X) const 
 * @copydoc SlipDatum::operator CHAR()
 * @see SlipDatum::operator CHAR()
 * @param[in] X (SlipDatum&) operand
 *
 * @fn virtual ULONG SlipOp::cast_ULONG(const SlipDatum& X) const
 * @copydoc SlipDatum::operator ULONG()
 * @see SlipDatum::operator ULONG()
 * @param[in] X (SlipDatum&) operand
 *
 * @fn virtual LONG SlipOp::cast_LONG(const SlipDatum& X) const 
 * @copydoc SlipDatum::operator LONG()
 * @see SlipDatum::operator LONG()
 * @param[in] X (SlipDatum&) operand
 *
 * @fn virtual DOUBLE SlipOp::cast_DOUBLE(const SlipDatum& X) const
 * @copydoc SlipDatum::operator DOUBLE()
 * @see SlipDatum::operator DOUBLE()
 * @param[in] X (SlipDatum&) operand
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual ULONG SlipOp::bitNOT(const SlipDatum& X) const  
 * @copydoc SlipDatum::operator~()
 * @see SlipDatum::operator~()
 * @param[in] X (SlipDatum&) operand
 * 
 * @fn virtual bool SlipOp::logNOT(const SlipDatum& X) const  
 * @copydoc SlipDatum::operator!()
 * @see SlipDatum::operator!()
 * @param[in] X (SlipDatum&) operand
 * 
 * @fn virtual SlipDatum  SlipOp::plus(const SlipDatum& X) const  
 * @copydoc SlipDatum::operator+()
 * @see SlipDatum::operator+()
 * @param[in] X (SlipDatum&) operand
 * 
 * @fn virtual SlipDatum  SlipOp::minus(const SlipDatum& X) const  
 * @copydoc SlipDatum::operator-()
 * @see SlipDatum::operator-()
 * @param[in] X (SlipDatum&) operand
 * 
 * @fn virtual SlipDatum& SlipOp::postInc(SlipDatum& X) const
 * @copydoc SlipDatum::operator++(int)
 * @see SlipDatum::operator++(int)
 * @param[in] X (SlipDatum&) operand
 * 
 * @fn virtual SlipDatum& SlipOp::postDec(SlipDatum& X) const
 * @copydoc SlipDatum::operator--(int)
 * @see SlipDatum::operator--(int)
 * @param[in] X (SlipDatum&) operand
 * 
 * @fn virtual SlipDatum& SlipOp::preInc(SlipDatum& X) const
 * @copydoc SlipDatum::operator++()
 * @see SlipDatum::operator++()
 * @param[in] X (SlipDatum&) operand
 * 
 * @fn virtual SlipDatum& SlipOp::preDec(SlipDatum& X) const
 * @copydoc SlipDatum::operator--()
 * @see SlipDatum::operator--()
 * @param[in] X (SlipDatum&) operand
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual SlipDatum& SlipOp::copy(const SlipDatum& X) const
 * @copydoc SlipDatum::copy()
 * @see SlipDatum::copy()
 * @param[in] X (SlipDatum&) cell to be copied
 * 
 * @fn virtual string SlipOp::dump(SlipDatum& X) const
 * @copydoc SlipCellBase::dump()
 * @see SlipCellBase::dump()
 * @param[in] X (SlipDatum&) operand
 * 
 * @fn virtual string SlipOp::write(const SlipCell& X) const
 * @copydoc SlipDatum::write
 * @see SlipDatum::copy()
 * @param[in] X (SlipDatum&) operand
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual SlipDatum& SlipOp::add(const SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator+(const SlipDatum&)
 * @see SlipDatum::operator+(const SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of '+'
 * @param[in] X (SlipDatum&) RHS of '+'
 * 
 * @fn virtual SlipDatum& SlipOp::add(const SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator+(const LONG)
 * @see SlipDatum::operator+(const LONG X)
 * @param[in] Y (SlipDatum&) LHS of '+'
 * @param[in] X (SlipDatum&) RHS of '+'
 * 
 * @fn virtual SlipDatum& SlipOp::add(const SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator+(const ULONG)
 * @see SlipDatum::operator+(const ULONG X)
 * @param[in] Y (SlipDatum&) LHS of '+'
 * @param[in] X (SlipDatum&) RHS of '+'
 * 
 * @fn virtual SlipDatum& SlipOp::add(const SlipDatum& Y, const DOUBLE X) const = 0
 * @copydoc SlipDatum::operator+(const DOUBLE)
 * @see SlipDatum::operator+(const DOUBLE)
 * @param[in] Y (SlipDatum&) LHS of '+'
 * @param[in] X (SlipDatum&) RHS of '+'
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual SlipDatum& SlipOp::sub(const SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator-(const SlipDatum&)
 * @see SlipDatum::operator-(const SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of '-'
 * @param[in] X (SlipDatum&) RHS of '-'
 * 
 * @fn virtual SlipDatum& SlipOp::sub(const SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator-(const LONG)
 * @see SlipDatum::operator-(const LONG X)
 * @param[in] Y (SlipDatum&) LHS of '-'
 * @param[in] X (SlipDatum&) RHS of '-'
 * 
 * @fn virtual SlipDatum& SlipOp::sub(const SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator-(const ULONG)
 * @see SlipDatum::operator-(const ULONG X)
 * @param[in] Y (SlipDatum&) LHS of '-'
 * @param[in] X (SlipDatum&) RHS of '-'
 * 
 * @fn virtual SlipDatum& SlipOp::sub(const SlipDatum& Y, const DOUBLE X) const = 0
 * @copydoc SlipDatum::operator-(const DOUBLE)
 * @see SlipDatum::operator-(const DOUBLE)
 * @param[in] Y (SlipDatum&) LHS of '-'
 * @param[in] X (SlipDatum&) RHS of '-'
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual SlipDatum& SlipOp::mul(const SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator*(const SlipDatum&)
 * @see SlipDatum::operator*(const SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of '*'
 * @param[in] X (SlipDatum&) RHS of '*'
 * 
 * @fn virtual SlipDatum& SlipOp::mul(const SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator*(const LONG)
 * @see SlipDatum::operator*(const LONG X)
 * @param[in] Y (SlipDatum&) LHS of '*'
 * @param[in] X (SlipDatum&) RHS of '*'
 * 
 * @fn virtual SlipDatum& SlipOp::mul(const SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator*(const ULONG)
 * @see SlipDatum::operator*(const ULONG X)
 * @param[in] Y (SlipDatum&) LHS of '*'
 * @param[in] X (SlipDatum&) RHS of '*'
 * 
 * @fn virtual SlipDatum& SlipOp::mul(const SlipDatum& Y, const DOUBLE X) const = 0
 * @copydoc SlipDatum::operator*(const DOUBLE)
 * @see SlipDatum::operator*(const DOUBLE)
 * @param[in] Y (SlipDatum&) LHS of '*'
 * @param[in] X (SlipDatum&) RHS of '*'
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual SlipDatum& SlipOp::div(const SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator/(const SlipDatum&)
 * @see SlipDatum::operator/(const SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of '/'
 * @param[in] X (SlipDatum&) RHS of '/'
 * 
 * @fn virtual SlipDatum& SlipOp::div(const SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator/(const LONG)
 * @see SlipDatum::operator/(const LONG X)
 * @param[in] Y (SlipDatum&) LHS of '/'
 * @param[in] X (SlipDatum&) RHS of '/'
 * 
 * @fn virtual SlipDatum& SlipOp::div(const SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator/(const ULONG)
 * @see SlipDatum::operator/(const ULONG X)
 * @param[in] Y (SlipDatum&) LHS of '/'
 * @param[in] X (SlipDatum&) RHS of '/'
 * 
 * @fn virtual SlipDatum& SlipOp::div(const SlipDatum& Y, const DOUBLE X) const = 0
 * @copydoc SlipDatum::operator/(const DOUBLE X)
 * @see SlipDatum::operator/(const DOUBLE)
 * @param[in] Y (SlipDatum&) LHS of '/'
 * @param[in] X (SlipDatum&) RHS of '/'
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual LONG SlipOp::mod(const SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator%(const SlipDatum&)
 * @see SlipDatum::operator%(const SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of '%'
 * @param[in] X (SlipDatum&) RHS of '%'
 * 
 * @fn virtual LONG SlipOp::mod(const SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator%(const LONG)
 * @see SlipDatum::operator%(const LONG X)
 * @param[in] Y (SlipDatum&) LHS of '%'
 * @param[in] X (SlipDatum&) RHS of '%'
 * 
 * @fn virtual LONG SlipOp::mod(const SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator%(const ULONG)
 * @see SlipDatum::operator%(const ULONG X)
 * @param[in] Y (SlipDatum&) LHS of '%'
 * @param[in] X (SlipDatum&) RHS of '%'
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual LONG SlipOp::shl(const SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator<<(const SlipDatum&)
 * @see SlipDatum::operator<<(const SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of 'shl'
 * @param[in] X (SlipDatum&) RHS of 'shl'
 * 
 * @fn virtual LONG SlipOp::shl(const SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator<<(const LONG)
 * @see SlipDatum::operator<<(const LONG X)
 * @param[in] Y (SlipDatum&) LHS of 'shl'
 * @param[in] X (SlipDatum&) RHS of 'shl'
 * 
 * @fn virtual LONG SlipOp::shl(const SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator<<(const ULONG)
 * @see SlipDatum::operator<<(const ULONG X)
 * @param[in] Y (SlipDatum&) LHS of 'shl'
 * @param[in] X (SlipDatum&) RHS of 'shl'
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual LONG SlipOp::shr(const SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator>>(const SlipDatum&)
 * @see SlipDatum::operator>>(const SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of 'shr'
 * @param[in] X (SlipDatum&) RHS of 'shr'
 * 
 * @fn virtual LONG SlipOp::shr(const SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator>>(const LONG)
 * @see SlipDatum::operator>>(const LONG X)
 * @param[in] Y (SlipDatum&) LHS of 'shr'
 * @param[in] X (SlipDatum&) RHS of 'shr'
 * 
 * @fn virtual LONG SlipOp::shr(const SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator>>(const ULONG)
 * @see SlipDatum::operator>>(const ULONG X)
 * @param[in] Y (SlipDatum&) LHS of 'shr'
 * @param[in] X (SlipDatum&) RHS of 'shr'
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual LONG SlipOp::band(const SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator&(const SlipDatum&)
 * @see SlipDatum::operator&(const SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of 'and'
 * @param[in] X (SlipDatum&) RHS of 'and'
 * 
 * @fn virtual LONG SlipOp::band(const SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator&(const LONG)
 * @see SlipDatum::operator&(const LONG X)
 * @param[in] Y (SlipDatum&) LHS of 'and'
 * @param[in] X (SlipDatum&) RHS of 'and'
 * 
 * @fn virtual LONG SlipOp::band(const SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator&(const ULONG)
 * @see SlipDatum::operator&(const ULONG X)
 * @param[in] Y (SlipDatum&) LHS of 'and'
 * @param[in] X (SlipDatum&) RHS of 'and'
 * 
 * --------------------------------------------------------------------
 *
 * @fn virtual LONG SlipOp::bor(const SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator|(const SlipDatum&)
 * @see SlipDatum::operator|(SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of 'or'
 * @param[in] X (SlipDatum&) RHS of 'or'
 *
 * @fn virtual LONG SlipOp::bor(const SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator|(const LONG)
 * @param[in] Y (SlipDatum&) LHS of 'or'
 * @param[in] X (SlipDatum&) RHS of 'or'
 *
 * @fn virtual LONG SlipOp::bor(const SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator|(const ULONG)
 * @param[in] Y (SlipDatum&) LHS of 'or'
 * @param[in] X (SlipDatum&) RHS of 'or'
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual LONG SlipOp::bxor(const SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator^(const SlipDatum&)
 * @see SlipDatum::operator|(SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of 'xor'
 * @param[in] X (SlipDatum&) RHS of 'xor'
 *
 * @fn virtual LONG SlipOp::bxor(const SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator^(const LONG)
 * @param[in] Y (SlipDatum&) LHS of 'xor'
 * @param[in] X (SlipDatum&) RHS of 'xor'
 *
 * @fn virtual LONG SlipOp::bxor(const SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator^(const ULONG)
 * @param[in] Y (SlipDatum&) LHS of 'xor'
 * @param[in] X (SlipDatum&) RHS of 'xor'
 * 
 * --------------------------------------------------------------------
 *
 * @fn virtual SlipDatum& SlipOp::asgn(SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator=(const SlipDatum&)
 * @see SlipDatum::operator=(SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual SlipDatum& SlipOp::addAsgn(SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator+=(const SlipDatum&)
 * @see SlipDatum::operator+=(const SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * @fn virtual SlipDatum& SlipOp::addAsgn(SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator+=(const LONG)
 * @see SlipDatum::operator+=(const LONG X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * @fn virtual SlipDatum& SlipOp::addAsgn(SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator+=(const ULONG)
 * @see SlipDatum::operator+=(const ULONG X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * @fn virtual SlipDatum& SlipOp::addAsgn(SlipDatum& Y, const DOUBLE X) const = 0
 * @copydoc SlipDatum::operator+=(const DOUBLE)
 * @see SlipDatum::operator+=(const DOUBLE)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual SlipDatum& SlipOp::subAsgn(SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator-=(const SlipDatum&)
 * @see SlipDatum::operator-=(const SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * @fn virtual SlipDatum& SlipOp::subAsgn(SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator-=(const LONG)
 * @see SlipDatum::operator-=(const LONG X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * @fn virtual SlipDatum& SlipOp::subAsgn(SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator-=(const ULONG)
 * @see SlipDatum::operator-=(const ULONG X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * @fn virtual SlipDatum& SlipOp::subAsgn(SlipDatum& Y, const DOUBLE X) const = 0
 * @copydoc SlipDatum::operator-=(const DOUBLE)
 * @see SlipDatum::operator-=(const DOUBLE)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual SlipDatum& SlipOp::mulAsgn(SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator*=(const SlipDatum&)
 * @see SlipDatum::operator*=(const SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * @fn virtual SlipDatum& SlipOp::mulAsgn(SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator*=(const LONG)
 * @see SlipDatum::operator*=(const LONG X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * @fn virtual SlipDatum& SlipOp::mulAsgn(SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator*=(const ULONG)
 * @see SlipDatum::operator*=(const ULONG X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * @fn virtual SlipDatum& SlipOp::mulAsgn(SlipDatum& Y, const DOUBLE X) const = 0
 * @copydoc SlipDatum::operator*=(const DOUBLE)
 * @see SlipDatum::operator*=(const DOUBLE)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual SlipDatum& SlipOp::divAsgn(SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator/=(const SlipDatum&)
 * @see SlipDatum::operator/=(const SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * @fn virtual SlipDatum& SlipOp::divAsgn(SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator/=(const LONG)
 * @see SlipDatum::operator/=(const LONG X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * @fn virtual SlipDatum& SlipOp::divAsgn(SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator/=(const ULONG)
 * @see SlipDatum::operator/=(const ULONG X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * @fn virtual SlipDatum& SlipOp::divAsgn(SlipDatum& Y, const DOUBLE X) const = 0
 * @copydoc SlipDatum::operator/=(const DOUBLE)
 * @see SlipDatum::operator/=(const ULONG X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual SlipDatum& SlipOp::modAsgn(SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator%=(const SlipDatum&)
 * @see SlipDatum::operator%=(const SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * @fn virtual SlipDatum& SlipOp::modAsgn(SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator%=(const LONG)
 * @see SlipDatum::operator%=(const LONG X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * @fn virtual SlipDatum& SlipOp::modAsgn(SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator%=(const ULONG)
 * @see SlipDatum::operator%=(const ULONG X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual SlipDatum& SlipOp::bandAsgn(SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator&=(const SlipDatum&)
 * @see SlipDatum::operator&=(const SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * @fn virtual SlipDatum& SlipOp::bandAsgn(SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator&=(const LONG)
 * @see SlipDatum::operator&=(const LONG X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * @fn virtual SlipDatum& SlipOp::bandAsgn(SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator&=(const ULONG)
 * @see SlipDatum::operator&=(const ULONG X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual SlipDatum& SlipOp::borAsgn(SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator|=(const SlipDatum&)
 * @see SlipDatum::operator|=(const SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * @fn virtual SlipDatum& SlipOp::borAsgn(SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator|=(const LONG)
 * @see SlipDatum::operator|=(const LONG X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * @fn virtual SlipDatum& SlipOp::borAsgn(SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator|=(const ULONG)
 * @see SlipDatum::operator|=(const ULONG X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual SlipDatum& SlipOp::bxorAsgn(SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator^=(const SlipDatum&)
 * @see SlipDatum::operator^=(const SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * @fn virtual SlipDatum& SlipOp::bxorAsgn(SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator^=(const LONG)
 * @see SlipDatum::operator^=(const LONG X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * @fn virtual SlipDatum& SlipOp::bxorAsgn(SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator^=(const ULONG)
 * @see SlipDatum::operator^=(const ULONG X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual SlipDatum& SlipOp::shlAsgn(SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator<<(const SlipDatum&)
 * @see SlipDatum::operator<<(const SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * @fn virtual SlipDatum& SlipOp::shlAsgn(SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator<<(const LONG)
 * @see SlipDatum::operator<<(const LONG X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * @fn virtual SlipDatum& SlipOp::shlAsgn(SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator<<(const ULONG)
 * @see SlipDatum::operator<<(const ULONG X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual SlipDatum& SlipOp::shrAsgn(SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator>>=(const SlipDatum&)
 * @see SlipDatum::operator>>=(const SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of assignment
 * @param[in] X (SlipDatum&) RHS of assignment
 * 
 * @fn virtual SlipDatum& SlipOp::shrAsgn(SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator>>=(const LONG)
 * @see SlipDatum::operator>>=(const LONG X)
 * @param[in] X (SlipDatum&) RHS of assignment
 * @param[in] Y (SlipDatum&) LHS of assignment
 * 
 * @fn virtual SlipDatum& SlipOp::shrAsgn(SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator>>=(const ULONG)
 * @see SlipDatum::operator>>=(const ULONG X)
 * @param[in] X (SlipDatum&) RHS of assignment
 * @param[in] Y (SlipDatum&) LHS of assignment
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual bool SlipOp::notEqual(const SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator!=(const SlipDatum&)
 * @see SlipDatum::operator!=(const SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of '!='
 * @param[in] X (SlipDatum&) RHS of '!='
 * 
 * @fn virtual bool SlipOp::notEqual(const SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator!=(const LONG)
 * @see SlipDatum::operator!=(const LONG X)
 * @param[in] Y (SlipDatum&) LHS of '!='
 * @param[in] X (SlipDatum&) RHS of '!='
 * 
 * @fn virtual bool SlipOp::notEqual(const SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator!=(const ULONG)
 * @see SlipDatum::operator!=(const ULONG X)
 * @param[in] Y (SlipDatum&) LHS of '!='
 * @param[in] X (SlipDatum&) RHS of '!='
 * 
 * @fn virtual bool SlipOp::notEqual(const SlipDatum& Y, const DOUBLE X) const = 0
 * @copydoc SlipDatum::operator!=(const DOUBLE)
 * @see SlipDatum::operator!=(const DOUBLE X)
 * @param[in] Y (SlipDatum&) LHS of '!='
 * @param[in] X (SlipDatum&) RHS of '!='
 * 
 * @fn virtual bool SlipOp::notEqual(const SlipDatum& Y, const PTR X) const = 0
 * @copydoc SlipDatum::operator!=(const PTR)
 * @see SlipDatum::operator!=(const PTR X)
 * @param[in] Y (SlipDatum&) LHS of '!='
 * @param[in] X (SlipDatum&) RHS of '!='
 * 
 * @fn virtual bool SlipOp::notEqual(const SlipDatum& Y, const string& X) const = 0
 * @copydoc SlipDatum::operator!=(const string&)
 * @see SlipDatum::operator!=(const string& X)
 * @param[in] Y (SlipDatum&) LHS of '!='
 * @param[in] X (SlipDatum&) RHS of '!='
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual bool SlipOp::lessThan(const SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator<(const SlipDatum&)
 * @see SlipDatum::operator<(const SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of '&lt;'
 * @param[in] X (SlipDatum&) RHS of '&lt;'
 * 
 * @fn virtual bool SlipOp::lessThan(const SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator<(const LONG)
 * @see SlipDatum::operator<(const LONG X)
 * @param[in] Y (SlipDatum&) LHS of '&lt;'
 * @param[in] X (SlipDatum&) RHS of '&lt;'
 * 
 * @fn virtual bool SlipOp::lessThan(const SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator<(const ULONG)
 * @see SlipDatum::operator<(const ULONG X)
 * @param[in] Y (SlipDatum&) LHS of '&lt;'
 * @param[in] X (SlipDatum&) RHS of '&lt;'
 * 
 * @fn virtual bool SlipOp::lessThan(const SlipDatum& Y, const DOUBLE X) const = 0
 * @copydoc SlipDatum::operator<(const DOUBLE)
 * @see SlipDatum::operator<(const DOUBLE X)
 * @param[in] Y (SlipDatum&) LHS of '&lt;'
 * @param[in] X (SlipDatum&) RHS of '&lt;'
 * 
 * @fn virtual bool SlipOp::lessThan(const SlipDatum& Y, const string& X) const = 0
 * @copydoc SlipDatum::operator<(const string&)
 * @see SlipDatum::operator:lessThan&lt;(const string& X)
 * @param[in] Y (SlipDatum&) LHS of '&lt;'
 * @param[in] X (SlipDatum&) RHS of '&lt;'
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual bool SlipOp::lessEqual(const SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator<=(const SlipDatum&)
 * @see SlipDatum::operator&lt;=(const SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of '&lt;='
 * @param[in] X (SlipDatum&) RHS of '&lt;='
 * 
 * @fn virtual bool SlipOp::lessEqual(const SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator&lt;=(const LONG)
 * @see SlipDatum::operator&lt;=(const LONG X)
 * @param[in] Y (SlipDatum&) LHS of '&lt;='
 * @param[in] X (SlipDatum&) RHS of '&lt;='
 * 
 * @fn virtual bool SlipOp::lessEqual(const SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator&lt;=(const ULONG)
 * @see SlipDatum::operator&lt;=(const ULONG X)
 * @param[in] Y (SlipDatum&) LHS of '&lt;='
 * @param[in] X (SlipDatum&) RHS of '&lt;='
 * 
 * @fn virtual bool SlipOp::lessEqual(const SlipDatum& Y, const DOUBLE X) const = 0
 * @copydoc SlipDatum::operator&lt;=(const DOUBLE)
 * @see SlipDatum::operator&lt;=(const DOUBLE X)
 * @param[in] Y (SlipDatum&) LHS of '&lt;='
 * @param[in] X (SlipDatum&) RHS of '&lt;='
 * 
 * @fn virtual bool SlipOp::lessEqual(const SlipDatum& Y, const string& X) const = 0
 * @copydoc SlipDatum::operator&lt;=(const string&)
 * @see SlipDatum::operator&lt;=(const string& X)
 * @param[in] Y (SlipDatum&) LHS of '&lt;='
 * @param[in] X (SlipDatum&) RHS of '&lt;='
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual bool SlipOp::equal(const SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator==(const SlipDatum&)
 * @see SlipDatum::operator==(const SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of '='
 * @param[in] X (SlipDatum&) RHS of '='
 * 
 * @fn virtual bool SlipOp::equal(const SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator==(const LONG)
 * @see SlipDatum::operator==(const LONG X)
 * @param[in] Y (SlipDatum&) LHS of '='
 * @param[in] X (SlipDatum&) RHS of '='
 * 
 * @fn virtual bool SlipOp::equal(const SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator==(const ULONG)
 * @see SlipDatum::operator==(const ULONG X)
 * @param[in] Y (SlipDatum&) LHS of '='
 * @param[in] X (SlipDatum&) RHS of '='
 * 
 * @fn virtual bool SlipOp::equal(const SlipDatum& Y, const DOUBLE X) const = 0
 * @copydoc SlipDatum::operator==(const DOUBLE)
 * @see SlipDatum::operator==(const DOUBLE X)
 * @param[in] Y (SlipDatum&) LHS of '='
 * @param[in] X (SlipDatum&) RHS of '='
 * 
 * @fn virtual bool SlipOp::equal(const SlipDatum& Y, const PTR X) const = 0
 * @copydoc SlipDatum::operator==(const PTR)
 * @see SlipDatum::operator==(const PTR X)
 * @param[in] Y (SlipDatum&) LHS of '='
 * @param[in] X (SlipDatum&) RHS of '='
 * 
 * @fn virtual bool SlipOp::equal(const SlipDatum& Y, const string& X) const = 0
 * @copydoc SlipDatum::operator==(const string&)
 * @see SlipDatum::operator==(const string& X)
 * @param[in] Y (SlipDatum&) LHS of '='
 * @param[in] X (SlipDatum&) RHS of '='
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual bool SlipOp::greatEqual(const SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator&gt;=(const SlipDatum&)
 * @see SlipDatum::operator&gt;=(const SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of '&gt;='
 * @param[in] X (SlipDatum&) RHS of '&gt;='
 * 
 * @fn virtual bool SlipOp::greatEqual(const SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator&gt;=(const LONG)
 * @see SlipDatum::operator&gt;=(const LONG X)
 * @param[in] Y (SlipDatum&) LHS of '&gt;='
 * @param[in] X (SlipDatum&) RHS of '&gt;='
 * 
 * @fn virtual bool SlipOp::greatEqual(const SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator&gt;=(const ULONG)
 * @see SlipDatum::operator&gt;=(const ULONG X)
 * @param[in] Y (SlipDatum&) LHS of '&gt;='
 * @param[in] X (SlipDatum&) RHS of '&gt;='
 * 
 * @fn virtual bool SlipOp::greatEqual(const SlipDatum& Y, const DOUBLE X) const = 0
 * @copydoc SlipDatum::operator&gt;=(const DOUBLE)
 * @see SlipDatum::operator&gt;=(const DOUBLE X)
 * @param[in] Y (SlipDatum&) LHS of '&gt;='
 * @param[in] X (SlipDatum&) RHS of '&gt;='
 * 
 * @fn virtual bool SlipOp::greatEqual(const SlipDatum& Y, const string& X) const = 0
 * @copydoc SlipDatum::operator&gt;=(const string&)
 * @see SlipDatum::operator&gt;=(const string& X)
 * @param[in] Y (SlipDatum&) LHS of '&gt;='
 * @param[in] X (SlipDatum&) RHS of '&gt;='
 * 
 * --------------------------------------------------------------------
 * 
 * @fn virtual bool SlipOp::greatThan(const SlipDatum& Y, const SlipDatum& X) const = 0
 * @copydoc SlipDatum::operator&gt;(const SlipDatum&)
 * @see SlipDatum::operator&gt;(const SlipDatum& X)
 * @param[in] Y (SlipDatum&) LHS of '&gt;'
 * @param[in] X (SlipDatum&) RHS of '&gt;'
 * 
 * @fn virtual bool SlipOp::greatThan(const SlipDatum& Y, const LONG X) const = 0
 * @copydoc SlipDatum::operator&gt;(const LONG)
 * @see SlipDatum::operator&gt;(const LONG X)
 * @param[in] Y (SlipDatum&) LHS of '&gt;'
 * @param[in] X (SlipDatum&) RHS of '&gt;'
 * 
 * @fn virtual bool SlipOp::greatThan(const SlipDatum& Y, const ULONG X) const = 0
 * @copydoc SlipDatum::operator&gt;(const ULONG)
 * @see SlipDatum::operator&gt;(const ULONG X)
 * @param[in] Y (SlipDatum&) LHS of '&gt;'
 * @param[in] X (SlipDatum&) RHS of '&gt;'
 * 
 * @fn virtual bool SlipOp::greatThan(const SlipDatum& Y, const DOUBLE X) const = 0
 * @copydoc SlipDatum::operator&gt;(const DOUBLE)
 * @see SlipDatum::operator&gt;(const DOUBLE X)
 * @param[in] Y (SlipDatum&) LHS of '&gt;'
 * @param[in] X (SlipDatum&) RHS of '&gt;'
 * 
 * @fn virtual bool SlipOp::greatThan(const SlipDatum& Y, const string& X) const = 0
 * @copydoc SlipDatum::operator&gt;(const string&)
 * @see SlipDatum::operator&gt;(const string& X)
 * @param[in] Y (SlipDatum&) LHS of '&gt;'
 * @param[in] X (SlipDatum&) RHS of '&gt;'
 * 
 * --------------------------------------------------------------------
 */

   class SlipOp {
   private:
   protected:
      struct sCell {
         void*         vptr;                                                    //!< Class vptr (this may be compiler dependent)
         void*         operation;                                               //!< Operations cell can perform
         SlipCellBase* leftLink;                                                //!< Pointer to preceding cell
         SlipCellBase* rightLink;                                               //!< Pointer to following cell
         Data          datum;                                                   //!< SLIP cell data field
      }; // struct sCell

      const ULONG      cellType;                                                //!< cell subtype
      const ULONG      ID;                                                      //!< Cell ID
      const string     name;                                                    //!< string representing SlipDatumCell
      const ClassType  classType;                                               //!< class number
      const bool       isDataFlag;                                              //!< true if cell is data
      const bool       isDiscreteFlag;                                          //!< true if cell is a discrete data type
      const bool       isHeaderFlag;                                            //!< true if cell is a header
      const bool       isNumberFlag;                                            //!< true if cell is numeric data
      const bool       isPtrFlag;                                               //!< true if cell is a pointer
      const bool       isStringFlag;                                            //!< true if cell is a STRING
      const bool       isSublistFlag;                                           //!< true if cell is sublist

      /****************************************
       *       SLIP Protected Functions       *
      *****************************************/
   private:

      SlipOp(const SlipOp& orig) : cellType(0)
                                 , ID(0)
                                 , classType(eUNDEFINED)
                                 , name("SlipOp")
                                 , isDataFlag(true)
                                 , isDiscreteFlag(false)
                                 , isHeaderFlag(false)
                                 , isNumberFlag(false)
                                 , isPtrFlag(false)
                                 , isStringFlag(false)
                                 , isSublistFlag(false)
      { }


      /****************************************
       *       SLIP Protected Functions       *
      *****************************************/
   protected:
      SlipOp() : cellType(0)
               , ID(0)
               , classType(eUNDEFINED)
               , name("SlipOp")
               , isDataFlag(true)
               , isDiscreteFlag(false)
               , isHeaderFlag(false)
               , isNumberFlag(false)
               , isPtrFlag(false)
               , isStringFlag(false)
               , isSublistFlag(false)
      { }
      SlipOp ( ULONG cellType, ULONG ID, ClassType classType, string name, bool isDataFlag
             , bool isDiscreteFlag, bool  isHeaderFlag,       bool isNumberFlag
             , bool isSublistFlag )
               : cellType(cellType)
               , ID(ID)
               , classType(classType)
               , name(name)
               , isDataFlag(isDataFlag)
               , isDiscreteFlag(isDiscreteFlag)
               , isHeaderFlag(classType == eHEADER)
               , isNumberFlag(isNumberFlag)
               , isPtrFlag(classType == ePTR)
               , isStringFlag(classType == eSTRING)
               , isSublistFlag(classType == eSUBLIST)
      { }
     ~SlipOp() { }
     
          /*************************************************
           *                Miscellaneous                  *
          *************************************************/
      static  SlipOp**  getOperator(const SlipCellBase& X) { return (SlipOp**)&(const_cast<SlipCellBase*>(&X)->operation); }
     
             /**********************************************
              *     Header Cell Data Access Functions      *
              **********************************************/   
      static void**  getHeadDescList(const SlipCellBase& X) { return X.getHeadDescList(); }
      static USHORT* getHeadMrk(const SlipCellBase& X)      { return X.getHeadMrk();      }
      static USHORT* getHeadRefCnt(const SlipCellBase& X)   { return X.getHeadRefCnt();   }

             /**********************************************
              *     Sublist Cell Data Access Functions      *
              **********************************************/
      static void** getSublistHeader(const SlipCellBase& X) { return X.getSublistHeader();}

             /**********************************************
              *     Reader Cell Data Access Functions      *
              **********************************************/
      static void**  getReaderTop(SlipCellBase& X)          { return X.getReaderTop();   }
      static USHORT* getReaderDepth(SlipCellBase& X)        { return X.getReaderDepth(); }


             /**********************************************
              *   Slip Datum Cell data Access Functions    *
              **********************************************/
      static bool*      getBool(const SlipCellBase& X)      { return X.getBool();   }
      static UCHAR*     getUChar(const SlipCellBase& X)     { return X.getUChar();  }
      static CHAR*      getChar(const SlipCellBase& X)      { return X.getChar();   }
      static ULONG*     getULong(const SlipCellBase& X)     { return X.getULong();  }
      static LONG*      getLong(const SlipCellBase& X)      { return X.getLong();   }
      static DOUBLE*    getDouble(const SlipCellBase& X)    { return X.getDouble(); }
      static PTR*       getPtr(const SlipCellBase& X)       { return X.getPtr();    }
      static STRING*    getString(const SlipCellBase& X)    { return X.getString(); }
      static sAllData*  getData(const SlipCellBase& X)      { return X.getData();   }
      static void       resetData(const SlipCellBase& X)    { const_cast<SlipCellBase&>(X).resetData(); }

   public:
      /*********************************************************
       *                  Casting Operators                    *
      *********************************************************/
      virtual bool       cast_bool(const SlipDatum& X) const  = 0;              // (bool)   X
      virtual UCHAR      cast_UCHAR(const SlipDatum& X) const = 0;              // (UCHAR)  X
      virtual CHAR       cast_CHAR(const SlipDatum& X) const  = 0;              // (CHAR)   X
      virtual ULONG      cast_ULONG(const SlipDatum& X) const = 0;              // (ULONG)  X
      virtual LONG       cast_LONG(const SlipDatum& X) const  = 0;              // (LONG)   X
      virtual DOUBLE     cast_DOUBLE(const SlipDatum& X) const= 0;              // (DOUBLE) X
      virtual PTR        cast_PTR(const SlipDatum& X) const= 0;                 // (PTR) X

      /*********************************************************
       *                 Operator Overloading                  *
      *********************************************************/

          /*************************************************
           *                Unary operators                *
          *************************************************/
      virtual ULONG      bitNOT   (const SlipDatum& X) const   = 0;             // operator~
      virtual bool       logNOT   (const SlipDatum& X) const   = 0;             // operator!
      virtual SlipDatum  plus     (const SlipDatum& X) const   = 0;             // operator+
      virtual SlipDatum  minus    (const SlipDatum& X) const   = 0;             // operator-
      virtual SlipDatum& postInc  (SlipDatum& X) const = 0;                     // operator++
      virtual SlipDatum& postDec  (SlipDatum& X) const = 0;                     // operator--
      virtual SlipDatum& preInc   (SlipDatum& X) const = 0;                     // ++operator
      virtual SlipDatum& preDec   (SlipDatum& X) const = 0;                     // --operator

          /*************************************************
           *                Binary operators               *
          *************************************************/
      virtual SlipDatum  add(const SlipDatum& Y, const SlipDatum& X) const   = 0; // operator+
      virtual SlipDatum  sub(const SlipDatum& Y, const SlipDatum& X) const   = 0; // operator-
      virtual SlipDatum  mul(const SlipDatum& Y, const SlipDatum& X) const   = 0; // operator*
      virtual SlipDatum  div(const SlipDatum& Y, const SlipDatum& X) const   = 0; // operator/
      virtual LONG       mod(const SlipDatum& Y, const SlipDatum& X) const   = 0; // operator%

      virtual SlipDatum  add(const SlipDatum& Y, const DOUBLE X) const   = 0;     // operator+
      virtual SlipDatum  sub(const SlipDatum& Y, const DOUBLE X) const   = 0;     // operator-
      virtual SlipDatum  mul(const SlipDatum& Y, const DOUBLE X) const   = 0;     // operator*
      virtual SlipDatum  div(const SlipDatum& Y, const DOUBLE X) const   = 0;     // operator/

      virtual SlipDatum  add(const SlipDatum& Y, const ULONG X) const    = 0;     // operator+
      virtual SlipDatum  sub(const SlipDatum& Y, const ULONG X) const    = 0;     // operator-
      virtual SlipDatum  mul(const SlipDatum& Y, const ULONG X) const    = 0;     // operator*
      virtual SlipDatum  div(const SlipDatum& Y, const ULONG X) const    = 0;     // operator/
      virtual LONG       mod(const SlipDatum& Y, const ULONG X) const    = 0;     // operator%

      virtual SlipDatum  add(const SlipDatum& Y, const LONG X) const     = 0;     // operator+
      virtual SlipDatum  sub(const SlipDatum& Y, const LONG X) const     = 0;     // operator-
      virtual SlipDatum  mul(const SlipDatum& Y, const LONG X) const     = 0;     // operator*
      virtual SlipDatum  div(const SlipDatum& Y, const LONG X) const     = 0;     // operator/
      virtual LONG       mod(const SlipDatum& Y, const LONG X) const     = 0;     // operator%

          /*************************************************
           *                Bit operators                  *
          *************************************************/
      virtual LONG     shl (const SlipDatum& Y, const SlipDatum& X) const   = 0;  // operator<<
      virtual LONG     shr (const SlipDatum& Y, const SlipDatum& X) const   = 0;  // operator>>
      virtual LONG     band(const SlipDatum& Y, const SlipDatum& X) const   = 0;  // operator&
      virtual LONG     bor (const SlipDatum& Y, const SlipDatum& X) const   = 0;  // operator|
      virtual LONG     bxor(const SlipDatum& Y, const SlipDatum& X) const   = 0;  // operator^

      virtual LONG     shl (const SlipDatum& Y, const ULONG X) const        = 0;  // operator<<
      virtual LONG     shr (const SlipDatum& Y, const ULONG X) const        = 0;  // operator>>
      virtual LONG     band(const SlipDatum& Y, const ULONG X) const        = 0;  // operator&
      virtual LONG     bor (const SlipDatum& Y, const ULONG X) const        = 0;  // operator|
      virtual LONG     bxor(const SlipDatum& Y, const ULONG X) const        = 0;  // operator^

      virtual LONG     shl (const SlipDatum& Y, const LONG  X) const        = 0;  // operator<<
      virtual LONG     shr (const SlipDatum& Y, const LONG  X) const        = 0;  // operator>>
      virtual LONG     band(const SlipDatum& Y, const LONG  X) const        = 0;  // operator&
      virtual LONG     bor (const SlipDatum& Y, const LONG  X) const        = 0;  // operator|
      virtual LONG     bxor(const SlipDatum& Y, const LONG  X) const        = 0;  // operator^

          /*************************************************
           *               Simple Assignment               *
          *************************************************/
      virtual SlipDatum& asgn (SlipDatum& Y, const SlipDatum& X) const    = 0;  // operator+=

          /*************************************************
           *       Arithmetic Assignment operators         *
          *************************************************/
      virtual SlipDatum& addAsgn (SlipDatum& Y, const SlipDatum& X) const    = 0; // operator+=
      virtual SlipDatum& subAsgn (SlipDatum& Y, const SlipDatum& X) const    = 0; // operator-=
      virtual SlipDatum& mulAsgn (SlipDatum& Y, const SlipDatum& X) const    = 0; // operator*=
      virtual SlipDatum& divAsgn (SlipDatum& Y, const SlipDatum& X) const    = 0; // operator/=
      virtual SlipDatum& modAsgn (SlipDatum& Y, const SlipDatum& X) const    = 0; // operator%=
      virtual SlipDatum& shlAsgn (SlipDatum& Y, const SlipDatum& X) const    = 0; // operator<<=
      virtual SlipDatum& shrAsgn (SlipDatum& Y, const SlipDatum& X) const    = 0; // operator>>=
      virtual SlipDatum& bandAsgn (SlipDatum& Y, const SlipDatum& X) const    = 0;// operator&=
      virtual SlipDatum& borAsgn  (SlipDatum& Y, const SlipDatum& X) const    = 0;// operator|=
      virtual SlipDatum& bxorAsgn (SlipDatum& Y, const SlipDatum& X) const    = 0;// operator^=

      virtual SlipDatum& addAsgn (SlipDatum& Y, const DOUBLE X) const    = 0; // operator+=
      virtual SlipDatum& subAsgn (SlipDatum& Y, const DOUBLE X) const    = 0; // operator-=
      virtual SlipDatum& mulAsgn (SlipDatum& Y, const DOUBLE X) const    = 0; // operator*=
      virtual SlipDatum& divAsgn (SlipDatum& Y, const DOUBLE X) const    = 0; // operator*=

      virtual SlipDatum& addAsgn (SlipDatum& Y, const ULONG  X) const    = 0; // operator+=
      virtual SlipDatum& subAsgn (SlipDatum& Y, const ULONG  X) const    = 0; // operator-=
      virtual SlipDatum& mulAsgn (SlipDatum& Y, const ULONG  X) const    = 0; // operator*=
      virtual SlipDatum& divAsgn (SlipDatum& Y, const ULONG  X) const    = 0; // operator/=
      virtual SlipDatum& modAsgn (SlipDatum& Y, const ULONG  X) const    = 0; // operator%=
      virtual SlipDatum& shlAsgn (SlipDatum& Y, const ULONG  X) const    = 0; // operator<<=
      virtual SlipDatum& shrAsgn (SlipDatum& Y, const ULONG  X) const    = 0; // operator>>=
      virtual SlipDatum& bandAsgn(SlipDatum& Y, const ULONG  X) const    = 0;// operator&=
      virtual SlipDatum& borAsgn (SlipDatum& Y, const ULONG  X) const    = 0;// operator|=
      virtual SlipDatum& bxorAsgn(SlipDatum& Y, const ULONG  X) const    = 0;// operator^=

      virtual SlipDatum& addAsgn (SlipDatum& Y, const LONG   X) const    = 0; // operator+=
      virtual SlipDatum& subAsgn (SlipDatum& Y, const LONG   X) const    = 0; // operator-=
      virtual SlipDatum& mulAsgn (SlipDatum& Y, const LONG   X) const    = 0; // operator*=
      virtual SlipDatum& divAsgn (SlipDatum& Y, const LONG   X) const    = 0; // operator/=
      virtual SlipDatum& modAsgn (SlipDatum& Y, const LONG   X) const    = 0; // operator%=
      virtual SlipDatum& shlAsgn (SlipDatum& Y, const LONG   X) const    = 0; // operator<<=
      virtual SlipDatum& shrAsgn (SlipDatum& Y, const LONG   X) const    = 0; // operator>>=
      virtual SlipDatum& bandAsgn(SlipDatum& Y, const LONG   X) const    = 0;// operator&=
      virtual SlipDatum& borAsgn (SlipDatum& Y, const LONG   X) const    = 0;// operator|=
      virtual SlipDatum& bxorAsgn(SlipDatum& Y, const LONG   X) const    = 0;// operator^=

          /*************************************************
           *              Logical operators                *
          *************************************************/
      virtual bool       notEqual  (const SlipDatum& Y, const SlipDatum& X) const  = 0; // operator!=
      virtual bool       lessThan  (const SlipDatum& Y, const SlipDatum& X) const  = 0; // operator<
      virtual bool       lessEqual (const SlipDatum& Y, const SlipDatum& X) const  = 0; // operator<=
      virtual bool       equal     (const SlipDatum& Y, const SlipDatum& X) const  = 0; // operator==
      virtual bool       greatEqual(const SlipDatum& Y, const SlipDatum& X) const  = 0; // operator>=
      virtual bool       greatThan (const SlipDatum& Y, const SlipDatum& X) const  = 0; // operator>

      virtual bool       notEqual  (const SlipDatum& Y, const DOUBLE X) const  = 0; // operator!=
      virtual bool       lessThan  (const SlipDatum& Y, const DOUBLE X) const  = 0; // operator<
      virtual bool       lessEqual (const SlipDatum& Y, const DOUBLE X) const  = 0; // operator<=
      virtual bool       equal     (const SlipDatum& Y, const DOUBLE X) const  = 0; // operator==
      virtual bool       greatEqual(const SlipDatum& Y, const DOUBLE X) const  = 0; // operator>=
      virtual bool       greatThan (const SlipDatum& Y, const DOUBLE X) const  = 0; // operator>

      virtual bool       notEqual  (const SlipDatum& Y, const ULONG  X) const  = 0; // operator!=
      virtual bool       lessThan  (const SlipDatum& Y, const ULONG  X) const  = 0; // operator<
      virtual bool       lessEqual (const SlipDatum& Y, const ULONG  X) const  = 0; // operator<=
      virtual bool       equal     (const SlipDatum& Y, const ULONG  X) const  = 0; // operator==
      virtual bool       greatEqual(const SlipDatum& Y, const ULONG  X) const  = 0; // operator>=
      virtual bool       greatThan (const SlipDatum& Y, const ULONG  X) const  = 0; // operator>

      virtual bool       notEqual  (const SlipDatum& Y, const LONG   X) const  = 0; // operator!=
      virtual bool       lessThan  (const SlipDatum& Y, const LONG   X) const  = 0; // operator<
      virtual bool       lessEqual (const SlipDatum& Y, const LONG   X) const  = 0; // operator<=
      virtual bool       equal     (const SlipDatum& Y, const LONG   X) const  = 0; // operator==
      virtual bool       greatEqual(const SlipDatum& Y, const LONG   X) const  = 0; // operator>=
      virtual bool       greatThan (const SlipDatum& Y, const LONG   X) const  = 0; // operator>

      virtual bool       notEqual  (const SlipDatum& Y, const PTR    X) const  = 0; // operator!=
      virtual bool       equal     (const SlipDatum& Y, const PTR    X) const  = 0; // operator==

      virtual bool       notEqual  (const SlipDatum& Y, const string& X) const  = 0; // operator!=
      virtual bool       lessThan  (const SlipDatum& Y, const string& X) const  = 0; // operator<
      virtual bool       lessEqual (const SlipDatum& Y, const string& X) const  = 0; // operator<=
      virtual bool       equal     (const SlipDatum& Y, const string& X) const  = 0; // operator==
      virtual bool       greatEqual(const SlipDatum& Y, const string& X) const  = 0; // operator>=
      virtual bool       greatThan (const SlipDatum& Y, const string& X) const  = 0; // operator>
     
             /**********************************************
              *         non-commutative Operators          *
              **********************************************/   
      virtual SlipDatum divOP(const LONG Y, const SlipDatum& X) const;          // Y / X
      virtual SlipDatum divUOP(const ULONG Y, const SlipDatum& X) const;        // Y / X
      virtual SlipDatum divDOP(const DOUBLE Y, const SlipDatum& X) const;       // Y / X
      virtual SlipDatum modOP(const LONG Y, const SlipDatum& X) const;          // Y % X
      virtual SlipDatum modUOP(const ULONG Y, const SlipDatum& X) const;        // Y % X
      virtual SlipDatum subOP(const LONG Y, const SlipDatum& X) const;          // Y - X
      virtual SlipDatum subUOP(const ULONG Y, const SlipDatum& X) const;        // Y - X
      virtual SlipDatum subDOP(const DOUBLE Y, const SlipDatum& X) const;       // Y - X
      virtual LONG      shlOP(const LONG Y, const SlipDatum& X) const;          // Y << X
      virtual LONG      shrOP(const LONG Y, const SlipDatum& X) const;          // Y >> X

          /*************************************************
           *                Miscellaneous                  *
          *************************************************/
      virtual SlipDatum& copy(const SlipDatum& X) const;                        // Return a reference to a copy of the current cell
      const ClassType getClassType()const       { return classType;    }        // return the internal class type
      virtual sAllData getDatumCopy(const SlipDatum& X) const;                  // Return valid datum
      virtual
      const string    getName(SlipDatum* X = NULL) const { return name; }       // string representing SlipDatumCell
      const bool      isData()      const       { return isDataFlag;    }       // true if cell is data
      const bool      isDiscrete()  const       { return isDiscreteFlag;}       // true if cell is a discrete numeric type
      const bool      isHeader()    const       { return isHeaderFlag;  }       // true if cell is a header
      const bool      isNumber()    const       { return isNumberFlag;  }       // true if cell is numeric data
      const bool      isPtr()       const       { return isPtrFlag;     }       // true if cell is a pointer
      const bool      isReal()      const       { return classType == eDOUBLE; }// true if data is a real type
      const bool      isString()    const       { return isStringFlag;  }       // true if cell a string
      const bool      isSublist()   const       { return isSublistFlag; }       // true if cell is sublist
      virtual string  dump(const SlipCell& X) const     = 0;                    // unprettyingly dump the data
      static  string  dumpLink(const SlipCellBase& X);                          // format links
      virtual Parse   getParse(SlipDatum& X);                                   // return a pointer to a parser method
      virtual void    remove(const SlipDatum& X);                               // Delete the datum in this cell
      virtual string  toString(const SlipCell& X) const = 0;                    // pretty print SLIP cell
      virtual string  write(const SlipCell& X) const = 0;                       // formatted string for write

   }; // class SlipOp
}; // namespace slip

#endif	/* _SLIPOP_H */
