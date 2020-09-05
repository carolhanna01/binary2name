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
 * @file    SlipAsciiEntry.cpp
 * @author  A. Schwarz
 * @date    December 30, 2013
 *
 * @brief Class for Hash entry items specific to Slip
 */

# include <iostream>
# include <string>
# include "SlipDef.h"
# include "SlipDescription.h"
# include "SlipHashEntry.h"
# include "SlipAsciiEntry.h"
# include "SlipDatum.h"
# include "SlipHeader.h"

using namespace std;

namespace slip {

       /*************************************************
        *           Constructors & Destructors          *
        ************************************************/

   SlipAsciiEntry::SlipAsciiEntry(const string& name, void * const ptr, Type type, int debugFlag)
                  : SlipHashEntry(name, ptr, type, debugFlag) {
      setHash();
   }

   /**
    * @brief Create an entry to allow searches.
    * @param[in] name (string&) search string
    * @param[in] type (Type) type of entry being created
    * @param[in] debugFlag (int) debug flag mask
    */
   SlipAsciiEntry::SlipAsciiEntry(const string& name, Type type, int debugFlag)
                 : SlipHashEntry(name, NULL, type, debugFlag) {
      setHash();
   }; // SlipAsciiEntry::SlipAsciiEntry(const string& name)

   /**
    * @brief Construct an ASCII entry without a name.
    * <p>When input parsing determines that a list must be created it does
    *    not have a name. Therefore, a nameless entry is created with the
    *    name to be filled in at a later time.</p>
    * @param header (SlipHeader*) ointer to a list
    * @param type (Type) [UNKNOWN] entry type
    * @param[in] debugFlag (int) debug flag mask
    */
   SlipAsciiEntry::SlipAsciiEntry(SlipHeader* header, Type type, int debugFlag)
                 : SlipHashEntry(header, type, debugFlag) {
   }; // SlipAsciiEntry::SlipAsciiEntry(const SlipHeader* header, Type type

   /**
    * @brief Create a hash table entry;
    * <p>This is private. Users are not allowed to specify the entry type.
    *    The entry type is inferred from the type of data supplied.</p>
    * @param[in] desc (SlipDescription*) parser determined information
    * @param[in] type (enum Type) type of object being created
    * @param[in] debugFlag (int) debug flag mask
    */
   SlipAsciiEntry::SlipAsciiEntry(SlipDescription* desc, Type type, int debugFlag)
                 : SlipHashEntry(desc, type, debugFlag) {
      setHash();
   }; // SlipAsciiEntry::SlipAsciiEntry(const string& name, const void * const ptr, Type type)

   /**
    * @brief Create a hash table entry for User Data.
    * @param[in] userData (Sl&)
    * @param[in] parse (Parse) pointer to the input user data parse method
    * @param[in] type (Type) type of entry being created
    * @param[in] debugFlag (int) debug flag mask
    */
   SlipAsciiEntry::SlipAsciiEntry(const SlipDatum& userData, Parse parse, Type type, int debugFlag)
                 : SlipHashEntry(userData.getName(), (void*) parse, type, debugFlag) {
      setHash();
   }; // SlipAsciiEntry::SlipAsciiEntry(const SlipDatum& userData, Parse parse)

   /**
    * @brief Destructor for a hash table entry.
    * <p>This class is only used for reading a list file.</p>
    * <p>The value of the <i>ptr</i> field is either a pointer to the User Data
    *    input method (<b>parse</b>) or a pointer to a SlipHeader. If it is a
    *    pointer to a SlipHeader then it is always deleted, otherwise it is
    *    not deleted.</p>
    * <p>On input and parsing of a list file lists (SlipHeader objects) are
    *    created on the fly. If they are used then deleting (<b>deleteList</b>
    *    will not cause the list to be deleted but will make the list
    *    programmatically eligible for deletion by the user. If the deletion
    *    is not done here then programmatically the list can never be deleted
    *    unless the application deletes it one more time (than seems
    *    necessary).</p>
    * <p>This is an example of "sneaky pete" code. The SlipBinaryEntry
    *    destructor does not delete a list. The deletion is in this place as
    *    an optimization issue since otherwise the reader code would have to
    *    make a separate pass over the HashTable to perform the deletion.
    *    Placing the deletion here requires a single pass over the Hash Table.</p>
    */
   SlipAsciiEntry::~SlipAsciiEntry() {
   }; // SlipAsciiEntry::~SlipAsciiEntry()

       /*************************************************
        *                   Operators                   *
        ************************************************/
   /**
    * @brief Compare the current entry with another entry.
    * @param X (SlipAsciiEntry&) comparand
    * @return <b>true</b> current string name != input string name
    */
   bool  SlipAsciiEntry::operator!=(const SlipHashEntry& X) const {
      return (getAsciiKey() != X.getAsciiKey());
   }; // bool SlipAsciiEntry::operator!=(const SlipHashEntry& X) const

   /**
    * @brief Compare the current entry with another entry.
    * @param X (SlipAsciiEntry&) comparand
    * @return <b>true</b> current string name < input string name
    */
   bool  SlipAsciiEntry::operator<(const SlipHashEntry& X) const {
      return (getAsciiKey() < X.getAsciiKey());
   }; // bool SlipAsciiEntry::operator<(const SlipHashEntry& X) const

   /**
    * @brief Compare the current entry with another entry.
    * @param X (SlipAsciiEntry&) comparand
    * @return <b>true</b> current string name <= input string name
    */
   bool  SlipAsciiEntry::operator<=(const SlipHashEntry& X) const {
      return (getAsciiKey() <= X.getAsciiKey());
   }; // bool SlipAsciiEntry::operator<=(const SlipHashEntry& X) const

   /**
    * @brief Compare the current entry with another entry.
    * @param X (SlipAsciiEntry&) comparand
    * @return <b>true</b> current string name == input string name
    */
   bool  SlipAsciiEntry::operator==(const SlipHashEntry& X) const {
      return (getAsciiKey() == X.getAsciiKey());
   }; // bool SlipAsciiEntry::operator==(const SlipHashEntry& X) const

   /**
    * @brief Compare the current entry with another entry.
    * @param X (SlipAsciiEntry&) comparand
    * @return <b>true</b> current string name >= input string name
    */
   bool  SlipAsciiEntry::operator>=(const SlipHashEntry& X) const {
      return (getAsciiKey() >= X.getAsciiKey());
   }; // bool SlipAsciiEntry::operator>=(const SlipHashEntry& X) const

   /**
    * @brief Compare the current entry with another entry.
    * @param X (SlipAsciiEntry&) comparand
    * @return <b>true</b> current string name > input string name
    */
   bool  SlipAsciiEntry::operator>(const SlipHashEntry& X) const {
      return (getAsciiKey() > X.getAsciiKey());
   }; // bool SlipAsciiEntry::operator>(const SlipHashEntry& X) const

       /*************************************************
        *                    Methods                    *
        ************************************************/
   /**
    * @brief Create a new entry using the input hash entry.
    * <p>The input hash entry acts as template for the creation of a new
    *    hash entry. The new hash entry is an exact duplicate of the
    *    input.</p>
    * @note The input pointer (<b>otr</b>) is a pointer to a <i>SlipHeader</i>
    *       object. The created <i>SlipAsciiEntry</i> will point to this
    *       same object. If the current or new <i>SlipAsciiEntry</i> is
    *       deleted the <i>SlipHeader</i> object will not be deleted. The
    *       list header must be deleted explicitly by the applicaton.
    * @return new entry created from the heap
    */
   SlipHashEntry* SlipAsciiEntry::createHashEntry() const {
      return new SlipAsciiEntry(*name, ptr, type, debugFlag);
   }; // SlipHashEntry const * const SlipAsciiEntry::createHashEntry() const

   /**
    * @brief Delete an associated list.
    * <p>If the  entry is for a list then the list is deleted.</p>
    * <p>SlipAsciiEntry items are used for list input and represent named
    *    lists.  During the input operation a list is created, and for
    *    each use as a sublist, the list reference count (<b>refCnt</b>)
    *    is incremented. The application level owner of the list is the
    *    reader. After the reader is complete, the reader, as owner, must
    *    be deleted. Deleting the list deletes this relationship</p>
    * <p>Named lists are also used to defined Descriptor Lists. A linked
    *    list of headers is formed for each list referencing a named list
    *    before the definition is found. Each header is attached to the
    *    Descriptor field of a referencing list. If the named list definition
    *    is not found then the chain of header is broken and each list
    *    header in the chain becomes an empty Descriptor List to its
    *    referencing header. This condition is an error but will not
    *    cause damage to the integrity of SLIP.</p>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li>warning: &lt;name&gt; referenced but not defined.</li>
    *    <li>warning: &lt;name&gt; defined but not used.</li>
    *    <li>INTERNAL ERROR: Description List forward references not resolved for &lt;name&gt;.</li>
    * </ul>
    * <p></p>
    */
   void SlipAsciiEntry::deleteList() {
      if (ptr) {
         if (type == REFERENCED) {
            cout << "warning: " << *name << " referenced but not defined." << endl;
         } else if (type == DEFINED) {
            cout << "warning: " << *name << " defined but not used." << endl;
         } else if (type != R_AND_D) return;

         if (descriptorChain) {
            cout << "INTERNAL ERROR: Description List forward references not resolved for " << *name << '.' << endl;
            while (descriptorChain) {
               SlipHashEntry* next = getNext();
               delete descriptorChain;
               descriptorChain = next;
            }
         }
         ((SlipHeader*)ptr)->deleteList();
      }
   }; // void SlipAsciiEntry::deleteList()

   /**
    * Wbrief Return the ascii srring representing the hash key.
    * <p>Each hash table entry consists of a 2-tuple consisting of
    *    &lt;key, value&gt;. The key is an Ascii string and stored
    *    in the hash table entry. The key is used to hash the entry
    *    and place it into the hash table.</p>
    * @return the key Ascii string
    */
   const string& SlipAsciiEntry::getAsciiKey() const {
      return *name;
   }; // const string& SlipAsciiEntry::getAsciiKey()

   /**
    * Wbrief There is no binary hash key.
    * <p>Each hash table entry consists of a 2-tuple consisting of
    *    &lt;key, value&gt;. The key is an Ascii string and stored
    *    in the hash table entry. The key is used to hash the entry
    *    and place it into the hash table.</p>
    * @return <b>null</b>
    */
   const void * const SlipAsciiEntry::getBinaryKey() const {
      return NULL;
   }; //  const void*   SlipAsciiEntry::getBinaryKey()

   /**
    * @brief There is no Ascii value.
    * <p>Each hash table entry consists of a 2-tuple consisting of
    *    &lt;key, value&gt;. The key is an Ascii string and stored
    *    in the hash table entry. The key is used to hash the entry
    *    and place it into the hash table.</p>
    * <p>The value for all entries is a pointer. The pointer can
    *    be to either a <i>SlipHeadaer</i> object or to a User Data
    *    method. It is not Ascii.</p>
    * @return a <b>null</b> string
    */
   const string& SlipAsciiEntry::getAsciiValue() const {
      static const string tmp("");
      return tmp;
   }; // const string& SlipAsciiEntry::getAsciiValue()

   /**
    * Wbrief Return the ascii srring representing the hash key.
    * <p>Each hash table entry consists of a 2-tuple consisting of
    *    &lt;key, value&gt;. The key is an Ascii string and stored
    *    in the hash table entry. The key is used to hash the entry
    *    and place it into the hash table.</p>
    * <p>The value is a pointer. If the hash entry is for a User Data
    *    type, then the pointer is to a parse function to parse an
    *    input list representing the User Data object contents, or
    *    to generate a list representing the User Data data.</p>
    * <p>If the key represents the name of a list, then the value is
    *    a pointer to the <i>SlipHeader</i> object for this list.</p>
    * @return a pointer to a User Data method or to a <i>SlipHeader</i> object
    */
   void const * const SlipAsciiEntry::getBinaryValue() const {
      return ptr;
   }; // const void*   SlipAsciiEntry::getBinaryValue()


   /**
    * @brief Execute the primary hash function (fnv1a).
    * <p>Execute and return the value of the primary hash function.</p>
    * @return (uint32_t) the hash key
    */
   uint32_t SlipAsciiEntry::primaryHashFunction() {
      return sdbm(*name);
   }; // uint32_t SlipAsciiEntry::primaryHashFunction()

   /**
    * @brief Execute the secondary hash function (fnv1a).
    * <p>Execute and return the value of the secondary hash function.</p>
    * @return (uint32_t) the hash key
    */
   uint32_t SlipAsciiEntry::secondaryHashFuncion() {
      return fnv1a(*name);
   }; // uint32_t SlipAsciiEntry::secondaryHashFuncion() {

   /**
    * @brief Change the ASCII name of a sublist.
    * <p>The primary key of a User Data type is ascii. This key can not be
    *    changed. The request is ignored and <b>false</b> is returned.</p>
    * @param[in] name (string&) name to be changed
    * @return <b>false</b> the name can not be changed.
    */
   bool SlipAsciiEntry::rename(const string& name) {
      return false;
   }; // void SlipAsciiEntry::rename(const string& name) const

   /**
    * @brief Change the name.
    * @param key (string*) pointer to the ASCII name
    */
   void SlipAsciiEntry::setKey(const void* key) {
      name = new string(*(string*)key);
   }; // void SlipAsciiEntry::setKey(const void* key)

   /**
    * @brief Generate the primary and secondary hash values from the ASCII key.
    */
   void SlipAsciiEntry::setHash() {
      primaryHash   = primaryHashFunction();
      secondaryHash = secondaryHashFuncion();
   }; // void SlipAsciiEntry::setHash()

}; // namespace slip