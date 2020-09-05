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

/* 
 * File:   SlipStringNonConst.h
 * Author: skidmarks
 *
 * Created on February 24, 2013, 9:11 AM
 */

#ifndef SLIPSTRINGNONCONST_H
#define	SLIPSTRINGNONCONST_H

   
# include "SlipPointer.h"
# include <string>

using namespace std;

namespace slip {
      
   /**
    * @class SlipStringNonConst
    * @brief SLIP Defined Data type for a transient string.
    * <p><b>Overview</b></p><hr>
    * <p><b>User Data Type</b></p><hr>
    * <p>There can be multiple instances of the user string. The input
    *    string is copied into local space. Each instance of the current
    *    cell contains a new copy of the string (as it exists at the
    *    time of the copy). Any change to one instance does not cause
    *    a change to any other instance of the string. Once a copy is
    *    made then it is independent of its creator.</p>
    * <p>If one instance of the User Defined Data object containing the
    *    string is deleted, then other instances are unaffected. No
    *    instance is aware of any other.</p>
    * <p>When SLIP creates a new <em>SlipDatum</em> cell using the current
    *    cell as a model, string object in the current object is copied to
    *    the new object.</p>
    * <p><b>Slip Datum Overview</b></p><hr>
    * <p>SLIP requires fixed sizes for each SLIP object. SLIP supports the
    *    acquisition of a SLIP cell by managing its own object space, the
    *    Available Space List (AVSL), without garbage collection, but with
    *    cell recovery. Strings are inherently not fixed size. The creation
    *    and deletion of strings requires the managing of a data space in
    *    which each object is of varying size, and managing this space
    *    requires a garbage collector. Avoiding the requirement of a
    *    garbage collector required the creation of a user controlled
    *    means to acquire and release space; SLIP is not involved. But
    *    to allow the incorporation of strings in SLIP cells required
    *    a means to reference user strings without being responsible for
    *    managing string space and to provide the user some flexibility 
    *    in defining string semantics, either as a persistent object or 
    *    a transient object, with a need for a separate means to 
    *    create/delete strings. The result is to host strings in a User 
    *    Defined Data type container and provide defined operations within 
    *    this container for SLIP and user use.</p>
    * <p>I'm sure you'll agree this is very elegant.</p>
    * <p><b>Properties</b></p><hr>
    * <p>There are two properties:</p>
    * <ol>
    *    <li><b>name</b> and invariant property which is the internal 
    *        name of the User Defined Data type for a constant string.</li>
    *    <li><b>str</b> the object defined property which is a copy
    *        of the input string.</li>
    * </ol>
    * <p><b>Constructors/Destructors</b></p><hr>
    * <ul>
    *    <li><b>SlipStringConst</b> stores the input string into
    *        local (transient) storage and invokes the base class
    *        constructor with the User Defined Data name.</li>
    *    <li><b>~SlipStringConst</b> does nothing.</li>
    * </ul>
    * <p><b>SlipStringConst Operations</b></p><hr>
    * <ul>
    *    <li><b>copy</b> returns a new instance of a StringNonConst object
    *        containing a copy of the string value as it exists at that 
    *        time.</li>
    *    <li><b>dump></b> returns the string prefixed with the User
    *        Defined type name.</li>
    *    <li><b>remove</b> deletes the current object. This has a side
    *        effect of deleting the string.</li>
    *    <li><b>toString</b> returns an unadorned string.</li>
    * </ul>
    * 
    * @see slip::SlipDatum
    * @see slip::SlipPointer
    */
   
   class SlipStringNonConst : public SlipPointer {
      const string str;                                                         //!< string value
      static const string name;                                                 //!< type name
   public:
      SlipStringNonConst(const string* str);
      virtual ~SlipStringNonConst();                                            // Delete the string data
      virtual SlipStringNonConst* copy();                                       // Now object
      virtual Parse  getParse();                                                // return a pointer to the parse method
      static SlipDatum& parse(SlipHeader& head);                                // parser input
      virtual void   remove()   const;                                          // Delete object and data
      virtual string dump()     const;                                          // Uglify
      virtual string toString() const;                                          // Prettify
      virtual string  write() const;                                            // string formatted for a write
   }; // class SlipStringConst : SlipPointer

} // namespace slip


#endif	/* SLIPSTRINGNONCONST_H */

