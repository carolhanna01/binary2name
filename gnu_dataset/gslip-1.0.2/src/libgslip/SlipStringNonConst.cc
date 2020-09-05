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
 * @file    SlipStringNonConst.cpp
 * @author  A. Schwarz
 * @date    February 24, 2013
 *
 */

 # include <string>
 # include <sstream>
 # include "SlipStringNonConst.h"
 # include "SlipDatum.h"

namespace slip {
   const string SlipStringNonConst::name = "StringNonConst";                              //!< User Defined Data name

   /**
    * @brief Constructor for a User Defined Data type.
    * <p>The input string pointer is copied locally and a pointer to the
    *    User Defined Data name is propagated to the base constructor.</p>
    *
    * @param[in] str (string*) user string to be inserted into a SlipDatum cell
    */

   SlipStringNonConst::SlipStringNonConst(const string* str): SlipPointer(&name), str(*str) { }

   /**
    * @brief Destructor does nothing.
    * <p>Deletion of the object and the string are done in <em>remove()</em></p>
    */
   SlipStringNonConst::~SlipStringNonConst()         {  }                               // Delete the string data

   /**
    * @brief Return a new object with a copy of the string.
    * @return (SlipStringConst*) a pointer to the new object.
    */
   SlipStringNonConst* SlipStringNonConst::copy()    { return new SlipStringNonConst(&str); } // Now object

   /**
    * @brief A string prefixed with the User Defined Data name is returned.
    * @return (string) return the string prefixed with the Defined Data name
    */
   string SlipStringNonConst::dump()     const       { return  "(" + name + ")" + str; }; // Uglify

   /**
    * @brief Return <b>null</b> for a pointer to the Parse method
    * <p>The SlipString classes do not have a valid parser by definition.
    *    Creation of a new SlipString object is done through the
    *    constructor input arguments.</p>
    * @return <b></b>
    */
   Parse  SlipStringNonConst::getParse() {                                            // return a pointer to the parse method
      return NULL;
   }; // Parse  SlipStringNonConst::getParse()

   /**
    * @brief Parse the input read list.
    * <P>This operation is undefined for strings.</p>
    * @return (SlipPointer&) return
    */
    SlipDatum& SlipStringNonConst::parse(SlipHeader& head) {                          // input parser
         return *new SlipDatum("");
   }; // SlipPointer& SlipStringNonConst::parse(SlipHeader& head)

   /**
    * @brief SLIP requests for deletion cause deletion of the object and string.
    */
   void   SlipStringNonConst::remove()   const       { delete this; }                    // Delete object and data

   /**
    * @brief The stored string is returned.
    * @return (string) return the stored string
    */
   string SlipStringNonConst::toString() const       { return str; }                     // Prettify

   /**
    * @brief A string formatted for output
    * @return string sorrounded by double quotes, "string"
    */
   string SlipStringNonConst::write() const {                                            // Output formatted string
      return '"' + toString() + '"';
   }; // string SlipStringConst::write() const

} // namespace slip