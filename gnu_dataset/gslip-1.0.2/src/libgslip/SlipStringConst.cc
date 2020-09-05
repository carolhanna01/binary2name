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
 * @file    SlipStringConst.cpp
 * @author  A. Schwarz
 * @date    February 24, 2013
 *
 */

 # include <string>
 # include <sstream>
 # include "SlipStringConst.h"
 # include "SlipDatum.h"

namespace slip {
   const string SlipStringConst::name = "StringConst";                           //!< User Defined Data name

   /**
    * @brief Constructor for a User Defined Data type.
    * <p>The input string pointer is stored locally and a pointer to the
    *    User Defined Data name is propagated to the base constructor.</p>
    *
    * @param[in] str (string*) user string to be inserted into a SlipDatum cell
    */
   SlipStringConst::SlipStringConst(const string* str): SlipPointer(&name), str(str) { }

   /**
    * @brief Destructor does nothing.
    * <p>For a constant string, the deletion of the constant string object
    *    is under the control of the user.</p>
    * <p>All instances of this object point to the same object. All SLIP
    *    usages of this object must be removed from lists prior to deletion.</p>
    */
   SlipStringConst::~SlipStringConst()            {  }                                // Perversely, do not delete the data

   /**
    * @brief Return a pointer to the current object.
    * <p>Constant objects have one and only one instance in SLIP. All
    *    uses point to this same instance.</p>
    * @return (SlipStringConst*) a pointer to the current object.
    */
   SlipStringConst* SlipStringConst::copy()       { return this; }                    // Copy the current object and data

   /**
    * @brief A string prefixed with the User Defined Data name is returned.
    * @return (string) return the string prefixed with the Defined Data name
    */
   string SlipStringConst::dump()     const       { return  "(" + name + ")" + *str; }; // Uglify

   /**
    * @brief Return <b>null</b> for a pointer to the Parse method
    * <p>The SlipString classes do not have a valid parser by definition.
    *    Creation of a new SlipString object is done through the
    *    constructor input arguments.</p>
    * @return <b></b>
    */
   Parse  SlipStringConst::getParse() {                                            // return a pointer to the parse method
      return NULL;
   }; // Parse  SlipStringConst::getParse()

   /**
    * @brief Parse the input read list.
    * <P>This operation is undefined for strings.</p>
    * @return (SlipPointer&) return
    */
    SlipDatum& SlipStringConst::parse(SlipHeader& head) {                          // input parser
         return *new SlipDatum("");
   }; // SlipPointer& SlipStringConst::parse(SlipHeader& head)

   /**
    * @brief SLIP requests for deletion are ignored.
    * <p>The user is responsible for deleting a constant object.</p>
    */
   void   SlipStringConst::remove()   const       {  };                               // Delete the object not the data

   /**
    * @brief The stored string is returned.
    * @return (string) return the stored string
    */
   string SlipStringConst::toString() const       { return *str; }                    // Prettify

   /**
    * @brief A string formatted for output
    * @return string sorrounded by double quotes, "string"
    */
   string SlipStringConst::write() const {                                            // Output formatted string
      return '"' + toString() + '"';
   }; // string SlipStringConst::write() const

} // namespace slip