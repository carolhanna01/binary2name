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
 * @file    SlipPointer.cpp
 * @author  A. Schwarz
 * @date    June 18, 2013
 *
 */

# include "SlipPointer.h"

namespace slip {
   /**
    * <p>Each User Defined Data type (a derived class using SlipPointer as
    *    a base class) must supply a printable string for the user class.
    *    If none is provided, then the default is the null string, a
    *    string containing no characters.</p>
    * <p>When opportunity presents itself and the output format of the
    *    name is defined, the User Defined Data type name will be used
    *    in reconstructing the data during binary and ASCII parsing of
    *    an input file.</p>
    */
   const string SlipPointer::nullName = "";

   /**
    * @brief Base constructor for a User Defined Data type.
    * <p>This constructor must be referenced in any constructor for a
    *    User Defined Data type. Its sole purpose is to be a carrier
    *    of the type name for posting in the constructed object.</p>
    * <p>The syntax of the constructor reference is:</p>
    * <tt><pre>
    *    class UserDefinedData : SlipPointer
    *       UserDefinedData( ... ) : SlipPointer(name);
    * </pre></tt>
    *
    * @param[in] name (string*) User Defined Data type name
    */
   SlipPointer::SlipPointer(const string* name): name(name)                      // constructor
   { }

   /**
    * @brief SlipPointer deletion method.
    * <p>User supplied deletion semantics. There are two user semantic
    *    categories:</p>
    * <ul>
    *    <li><b>Constant</b> the user creates a persistent data object
    *        whose lifetime is controlled by the user. This allows a
    *        user to propagate a single instance of an object among
    *        several lists or several times in the same list. The user
    *        assumes all control over deletions of the created object.</p>
    *    <li><b>Non-constant</b> The user creates a unique instance
    *        of a User Defined Data type. Use of this object in several
    *        lists or multiple times in the same list requires a new
    *        instance of the data to be created. Each data instance is
    *        under SLIP control and will be deleted by SLIP usin</p>
    * </ul>
    * <p>These two means of treating data are in conflict. For the
    *    first method, SLIP can not delete the user data object
    *    (<em>delete object</em>). The the second method requires
    *    SLIP to delet the object. However, because of the conflict
    *    a mechanism separate from <em>delete object</em> is used.
    *    This destructor does nothing. The user provides required
    *    deletion functionality in <b>remove()</b>.</p>
    */
   SlipPointer::~SlipPointer() { }                                              // destructor

   /**
    * @brief Return the User Defined Data name.
    * @return (string*) containing the User Definition Data name
    */
   const string* const SlipPointer::getName() const { return name; }
} // namespace slip
