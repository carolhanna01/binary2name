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
 * @file    SlipPointer.h
 * @author  A. Schwarz
 * @date    Feb 22, 2013
 *
 * @brief User Defined Data Type base class.
 *
 */

#ifndef SLIPPOINTER_H
#define	SLIPPOINTER_H

# include <string>
# include <iostream>
# include <iomanip>
# include "SlipDatum.h"
# include "SlipDef.h"
# include "SlipHeader.h"

using namespace std;

namespace slip {
   
/**
 * @class SlipPointer
 * @brief Base class for User Defined Date type.
 * <p><b>Overview</b></p><hr>
 * <p>User Data is handled by SLIP in a SlipDatum object. SLIP expects
 *    the User Data to follow certain rules. These rules are spelled out
 *    as virtual methods in the base class.</p>
 * <p>Embedded in a SlipDatum object, a User Data object is handled in
 *    an identical fashion as any other SlipDatum object. It participates
 *    in list operations in an identical fashion. Handling of operations
 *    (arithmetic, boolean, bit, assignment, logical, casting, and unary)
 *    is established during SlipDatum instance creation and is associated
 *    with User Data at that time. It is not part of the interface, it is
 *    part of the SlipDatum creation process.</p>
 * <p>Definition of User Data Content and behavior is under the control of
 *    the user. The interface is sufficient to handle SLIP procesing. The
 *    user subclass can expand the given handling in any way that seems
 *    appropriate, including additional data items and operational functions.
 *    The SLIP requirements deal with the handling of User Data Deletion 
 *    (but not creation) and copying the User Data object as required.</p>
 * <p>SLIP assumes that the User Data is persistent. When the data is placed
 *    into a list, it is assumed that the data will last until the list is
 *    deleted, and until the list is again needed for use. SLIP does not
 *    guarantee that this condition is satisfied; the user must be aware
 *    of this and perform required operations to ensure that it is true.
 *    This means that User Data can not be placed on the stack. It must
 *    come from either the heap (part of dynamic creation) or be from a
 *    static are of memory, or from a user defined space allocator. If
 *    the User Data is constructed on the stack, when the current scope
 *    is exited the SlipDatum object containing the User DAta will have
 *    a stale pointer. Who knows what damage will be done (only the Shadow
 *    does).</p>
 * <p><b>Invariant Properties</b></p><hr>
 * <p>There is only one local invariant, the <b>name</b> field. The name
 *    field serves to identify the User Data to SLIP during binary/ASCII
 *    dumps/list output. During input of (a) list(s), the <b>name</b> 
 *    will be dynamically associated with known User Data classes. The
 *    associated classes will be responsible for correctly associating
 *    the input binary/ASCII with the correct User Defined input routine.</p>
 * <p>The <b>name</b> must be unique to a given User Data class. Within
 *    each instance of the class, the User Data <b>name</b> must be the
 *    same. Multiple unique User Data classes can not have the same name.<p>
 * <p>The <b>name</b> is persistent. It will not be deleted when the SlipPointer
 *    instance is deleted, and will not be modified. The user is responsible
 *    for ensuring that the string is persistent, and the user is responsible
 *    for deletion and space management of the string.</p>
 * <p>If the <b>name</b> field is not included then the User Data can
 *    not be output to a binary destination or read in and interpreted 
 *    correctly from an ASCII or binary source.</p>
 * @todo program  binary output/input
 * <p><b>Constructors/Destructors</b></p><hr>
 * <p>This is a pure virtual data type. The constructor can not be used
 *    directly but must be called through a constructor in a derived 
 *    class.</p>
 * <ul type="none">
 *    <li><b>SlipPointer</b> responsible for posting the User name field
 *        into the current instance. If a name field is not provided, a
 *        default will be used. The default name is not unique.</li>
 *    <li><b>~SlipPointer</b> does nothing. The name field is the only
 *        property in the base class and it is never deleted.</li>
 * </ul>
 * <p><b>Operations</b></p><hr>
 * <p>The pure virtual methods define the 'contract' between the base
 *    class and derived classes. This contract assumes that all
 *    required methods are implemented with semantics which are
 *    consistent with the definitions provided here. They provide 
 *    the ability for the SLIP systems to function correctlyl</p>
 * <p>The remaining methods allows retrieval of the user provided
 *    user data name. This name will be used by the ASCII and
 *    binary input methods to associate the input with an 
 *    appropriate parser.</p>
 * <ul>
 *    <li><b>copy</b> returns a copy of the User Defined Data.</li>
 *    <li><b>dump</b> dumps an ASCII representation of the User Defined Data.</li>
 *    <li><b>getName</b> returns the User Defined Data name.</li>
 *    <li><b>remove</b> deletes the User Defined Data object.</li>
 *    <li><b>toString</b> returns a prettyfied string of the User Defined Data.</li>
 * </ul>
 * <p><b>Required Methods</b></p><hr>
 * <p><b>static SlipCatum& parse(SlipHeader& head)</b>
 * <p>Parses the input list. All derived classes must include this method and
 *    a pointer to this method (Parse) is returned by <i>Parse getParse()</i>.</p>
 * <p>This class method acts as a factory for SlipDatum objects of the 
 *    required type. The method is responsible for creating a SlipDatum
 *    object. this object will be inserted into the current list or 
 *    sublist by the input reader (via an enqueue operation). If an
 *    error is detected during processing then the user method must
 *    output an error message. If the error makes it impossible to
 *    create a valid SlipDatum object, then a SlipDatum object with a
 *    SlipPointer field (PTR) of NULL must be created. The input reader
 *    checks the returned SlipDatum object, and if the SlipPointer field
 *    is NULL will issue a "failure to create a SlipDatum object" message.</p>
 * <p>A list is created from the input ASCII data file and passed to the
 *    user supplied parse method. The parser takes the input list and 
 *    performs user defined actions. The result is that the user defined 
 *    SlipPointer object is populated with the values required. If the
 *    user requires no input data then the input list must be empty.</p>
 * <p>The user can not perform any irretrievable action to the input list.
 *    The user can not delete or insert any item onto the input list. Any 
 *    user requirement for cells on the list should be done by copying, 
 *    for SlipDatum objects, or by by direct insertion, for SlipSublist 
 *    objects.</p>
 * <p>At the end of user processing the list reader will delete the list.</p>
 * <p>The following operations with the following consequences will occur. 
 *    All operations involve a SlipCell in the input list.</p>
 * <ul>
 *    <li>insert(head): Will cause a sublist to be inserted. The list will
 *        be preserved after system deletion. this applies to any insert
 *        operation</li>
 *    <li>insert(SlipDatum): A SlipDatum cell in the input list will be
 *        copied before insertion. The SlipDatum object will be destroyed
 *        in head by the system. This applies to any insert operation.</li>
 *    <li>move(SlipDatum): This destroys the integrity of the input list
 *        with uncertain effect. Do not do this. This applies to any move
 *        operation.</li>
 *    <li>replace(SlipDatum). this destroys the integrity of the  input list
 *       with uncertain effect. Do not do this. This applies to any replace
 *        operation.</li>
 *    <li>insert(SlipSublist): Will create a new sublist object and will
 *        preserve the referenced list. This operation is benign. This 
 *        applies to any insert operation.</li>
 *    <li>No operation from a user defined SlipCell to the input list is
 *        appropriate unless the input list is guaranteed to be preserved
 *        after system deletion. That is, if the altered input list or 
 *        contained sublist does not have a insert(head) or insert(sublist)
 *        operation then the user can expect bedlam to ensue.</li>
 * </ul>
 * @see SlipDatum
 */   

   class SlipPointer { 
   /**
 * @fn SlipPointer* copy() const
 * @brief Create a user data object identical to the current one.
 * <p>The user intentions on a copy are specifically defined in
 *    this method. There are two general categories of copy:</p>
 * <ul>
 *    <li><b>Constant</b> the data is persistent. The data lifetime
 *        is controlled by the user and there is only a single copy
 *        of this data in existence at any one time. The copy method
 *        returns a pointer to the current object.</li>
 *    <li><b>Non-Constant</b> the data is not persistent. The data 
 *        lifetime is controlled by the SLIP system. Each request 
 *        for a copy requires a new object to be created and returned. 
 *        The user maintains control over the contents of the copy.</li>
 * </ul>
 * 
 * @fn dump() const
 * @brief Uglify the User Defined Data
 * <p>The user data is returned in a meaningful way to do debugging.
 *    This output will never be used as an input and can be formatted
 *    in any way meaningful to the implementer.</P>
 * <p>The recommendation is that the returned string be prefixed
 *    with the User Defined Data name, as in:</p>
 * <tt><pre>
 *   ( name ) data
 * </pre></tt>
 * <p>An example of this is given in SlipStringNonConst and
 *    SlipStringConst, where the output is respectively:</p>
 * <tt><pre>
 *   ( SlipStringNonConst ) string
 *   ( SlipStringConst ) string
 * </pre></tt>
 * 
 * @return  (SlipDatum&) constructed SlipDatum cell.s
 * 
 * @fn remove() const
 * @brief Delete the current user data object.
 * <p>The user intentions on a delete are specifically defined in
 *    this method. There are two general categories of copy:</p>
 * <ul>
 *    <li><b>Constant</b> the data in persistent. SLIP will not
 *        delete the User Defined Data. The remove operation
 *        should do nothing (zip, zilch, nada).</li>
 *    <li><b>Non-Constant</b> the data is not persistent. The data 
 *        lifetime is controlled by the SLIP system. User Defined
 *        Data deletion must be supplied.</li>
 * </ul>
 * 
 * @fn string  write() const
 * <p>On output of a User Data object, all user data is captured in 
 *    a list. The output format is <tt>( user data )</tt>.</p>
 * <p>One way to construct an appropriately formatted string is to:</p>
 * <ol>
 *    <li>Construct a list of the user data items.</li>
 *    <li>Create a string using SlipWrite(header, stringstream out).</li>
 *    <li>return out.str();</li>
 * </ol>
 * @return string formatted for a write
 * 
 * @fn toString() const
 * @brief Return a pretty version of the User Defined Data object.
 * <p>It is  expected that the string can be output to a file and
 *    that this file can be expected to be input and used to 
 *    recreate a list at some future time. The format of the
 *    string must comply with SLIP standard TBD conventions or 
 *    it will not be possible to parse the input correctly.</p>
 * @note
 * <ol>
 * <li>When these standard become know they will be promptly be
 *     disseminated.</li>
 * </ol>
 */
   protected:
      const string* name;                                                        //!< User Data Definition Name
      static const string nullName;                                              //!< default User Data Definition Name
      
   /********************************************************
   *                      Public Methods                   *
   *********************************************************/
   public:
      SlipPointer(const string* name = &nullName);                              // constructor
      virtual ~SlipPointer();                                                   // destructor
      virtual SlipPointer* copy()  = 0;                                         // return a copy of SlipPointer
      virtual string dump() const = 0;                                          // uglify contents
      virtual Parse  getParse() = 0;                                            // return a pointer to the parse method
      virtual void   remove() const = 0;                                        // delete object and/or contents
      virtual string toString() const = 0;                                      // pretty-print contents
      const   string* const getName() const;                                    // return the object pointer
      virtual string  write() const = 0;                                        // string formatted for a write
   }; // class SlipPointer
} // namespace slip

#endif	/* SLIPPOINTER_H */

