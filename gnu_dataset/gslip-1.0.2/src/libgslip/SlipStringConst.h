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
 * @file    SlipStringConst.h
 * @author  A. Schwarz
 * @date    February 22, 2013
 *
 */

#ifndef SLIPSTRINGCONST_H
#define	SLIPSTRINGCONST_H
   
# include "SlipPointer.h"
# include <string>

using namespace std;

namespace slip {
      
   /**
    * @class SlipStringConst
    * @brief SLIP Defined Data type for a constant string.
    * <p><b>Overview</b></p><hr>
    * <p><b>User Data Type</b></p><hr>
    * <p>There is one instance of the user string. When all SLIP references
    *    to this string are deleted, the user can delete the string. If the
    *    user deletes the string before all uses are deleted, then the  
    *    string pointer will be stale and any usage within Slip may lead
    *    to a runtime fatal error.</p>
    * <p>When SLIP creates a new <em>SlipDatum</em> cell using the current
    *    cell as a model, the pointer to the user input string is copied.
    *    The string value is not recreated. During deletion of the current
    *    cell by SLIP, the string will not be deleted but the containing
    *    object, the current cell, will be excised from the living.</p>
    * <p>If multiple instances of the current string exist, then a change 
    *    made to one instance affects all of them.</p>
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
    *    <li><b>str</b> the object defined property which is a pointer to
    *        the input string.</li>
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
    *    <li><b>copy</b> returns a pointer to the current object.</li>
    *    <li><b>dump></b> returns the string prefixed with the User
    *        Defined type name.</li>
    *    <li><b>remove</b> does nothing. Constant User Defined Data
    *        objects are not deleted.</li>
    *    <li><b>toString</b> returns an unadorned string.</li>
    * </ul>
    * 
    * @see slip::SlipDatum
    * @see slip::SlipPointer
    */
   class SlipStringConst : public SlipPointer {
   protected:
      const string* str;                                                        //!< string value
      static const string name;                                                 //!< type name
   public:
      SlipStringConst(const string* str);
      virtual ~SlipStringConst();                                               // Perversely, do not delete the data
      virtual SlipStringConst* copy();                                          // Copy the current object and data
      virtual string dump()     const;                                          // Uglify 
      virtual Parse  getParse();                                                // return a pointer to the parse method
      static SlipDatum& parse(SlipHeader& head);                                // input parser
      virtual void   remove()   const;                                          // Delete the object not the data
      virtual string toString() const;                                          // Prettify
      virtual string  write() const;                                            // string formatted for a write
   }; // class SlipStringConst : SlipPointer

} // namespace slip
#endif	/* SLIPSTRING_H */

