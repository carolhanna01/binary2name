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
 * @file    SlipBinaryEntry.cpp
 * @author  A. Schwarz
 * @date    January 3, 2014
 *
 * @brief Class for SlipHeader Hash entry items
 */

# include <string>
# include "SlipDef.h"
# include "SlipHashEntry.h"
# include "SlipBinaryEntry.h"
#include "SlipCellBase.h"
# include "SlipDatum.h"
# include "SlipHeader.h"

using namespace std;

namespace slip {

       /*************************************************
        *           Constructors & Destructors          *
        ************************************************/

   /**
    * @brief Create an entry to allow searches.
    * <p>This is not supported in SlipBinaryEntry. SlipBinaryEntry
    *    supports registering of lists during output. The list
    *    header address is used for a hash key, not the name.</p>
    * @param[in] name (string&) search string
    * @param[in] debugFlag (int) debug flag mask
    */
   SlipBinaryEntry::SlipBinaryEntry(const string& name, int debugFlag)
                  : SlipHashEntry(name, NULL, USERDATA, debugFlag) {
      primaryHash   = 0;
      secondaryHash = 0;
   }; // SlipBinaryEntry::SlipBinaryEntry(const string& name)

   /**
    * @brief Create an entry to allow searches.
    * <p>Search the hash table using the address of a list as the primary
    *    key.</p>
    * @param[in] head (SlipHeader*) pointer to a list
    * @param[in] type (Type) type of entry
    * @param[in] debugFlag (int) debug flag mask
    */
   SlipBinaryEntry::SlipBinaryEntry(SlipHeader* head, Type type, int debugFlag)
                  : SlipHashEntry(string(""), (void*)head, type, debugFlag) {
      setHash();
   }; // SlipBinaryEntry::SlipBinaryEntry(const string& name)

   /**
    * @brief Create a hash table entry;
    * <p>This is private. Users are not allowed to specify the entry type.
    *    The entry type is inferred from the type of data supplied.</p>
    * @param[in] name (string&)
    * @param[in] ptr (void*) pointer to either a list header or a User Data method
    * @param[in] type (Type) type of object being created
    * @param[in] debugFlag (int) debug flag mask
    */
   SlipBinaryEntry::SlipBinaryEntry(const string& name, void * const ptr, Type type, int debugFlag)
                  : SlipHashEntry(name, ptr, type, debugFlag) {
      primaryHash   = 0;
      secondaryHash = 0;
   }; // SlipBinaryEntry::SlipBinaryEntry(const string& name)

   /**
    * @brief Create a hash table entry for User Data.
    * <p>This is not supported in SlipBinaryEntry. SlipBinaryEntry
    *    supports registering of lists during output. The list
    *    header address is used for a hash key, not the name.</p>
    * @param[in] userData (SlipDatum&) user data object
    * @param[in] parser (Parse) pointer to user data parse method
    * @param[in] debugFlag (int) debug flag mask
    */
   SlipBinaryEntry::SlipBinaryEntry(const SlipDatum& userData, Parse parser, int debugFlag)
                  : SlipHashEntry(userData.getName(), (void*) parser, USERDATA, debugFlag) {
      primaryHash   = 0;
      secondaryHash = 0;
   }; // SlipBinaryEntry::SlipBinaryEntry(const SlipDatum& userData, Parse parser)

   /**
    * @brief Create a hash table entry for a sublist.
    * @param[in] name (string) user data name of item
    * @param[in] head (SlipHeader*) pointer to the list header
    * @param[in] debugFlag (int) debug flag mask
    */
   SlipBinaryEntry::SlipBinaryEntry(const string& name, SlipHeader* head, int debugFlag)
                  : SlipHashEntry(name, (void*)head, R_AND_D, debugFlag) {
      setHash();
      primaryHash   = primaryHashFunction();
      secondaryHash = secondaryHashFuncion();
   }; // SlipBinaryEntry::SlipBinaryEntry(const string& name)

   /**
    * @brief Destructor for a hash table entry.
    * <p>This class is only used for writing a list file.</p>
    * <p>The value of the <i>ptr</i> field is either a pointer to the User Data
    *    output method (<b>parse</b>) or a pointer to a SlipHeader. If it is a
    *    pointer to a SlipHeader then it points to an existing list, one not
    *    created by the writer, and is not deleted. If it points to a User Data
    *    output method then this is not deleted. The destructor does nothing.</p>
    */
   SlipBinaryEntry::~SlipBinaryEntry() { }

       /*************************************************
        *                   Operators                   *
        ************************************************/
   /**
    * @brief Compare the current entry with another entry.
    * @param X (SlipBinaryEntry&) comparand
    * @return <b>true</b> current string name != input string name
    */
   bool  SlipBinaryEntry::operator!=(const SlipHashEntry& X) const {
      return (getBinaryKey() != X.getBinaryKey());
   }; // bool SlipBinaryEntry::operator!=(const SlipHashEntry& X) const

   /**
    * @brief Compare the current entry with another entry.
    * @param X (SlipBinaryEntry&) comparand
    * @return <b>true</b> current string name < input string name
    */
   bool  SlipBinaryEntry::operator<(const SlipHashEntry& X) const {
      return (getBinaryKey() < X.getBinaryKey());
   }; // bool SlipBinaryEntry::operator<(const SlipHashEntry& X) const

   /**
    * @brief Compare the current entry with another entry.
    * @param X (SlipBinaryEntry&) comparand
    * @return <b>true</b> current string name <= input string name
    */
   bool  SlipBinaryEntry::operator<=(const SlipHashEntry& X) const {
      return (getBinaryKey() <= X.getBinaryKey());
   }; // bool SlipBinaryEntry::operator<=(const SlipHashEntry& X) const

   /**
    * @brief Compare the current entry with another entry.
    * @param X (SlipBinaryEntry&) comparand
    * @return <b>true</b> current string name == input string name
    */
   bool  SlipBinaryEntry::operator==(const SlipHashEntry& X) const {
      return (getBinaryKey() == X.getBinaryKey());
   }; // bool SlipBinaryEntry::operator==(const SlipHashEntry& X) const

   /**
    * @brief Compare the current entry with another entry.
    * @param X (SlipBinaryEntry&) comparand
    * @return <b>true</b> current string name >= input string name
    */
   bool  SlipBinaryEntry::operator>=(const SlipHashEntry& X) const {
      return (getBinaryKey() >= X.getBinaryKey());
   }; // bool SlipBinaryEntry::operator>=(const SlipHashEntry& X) const

   /**
    * @brief Compare the current entry with another entry.
    * @param X (SlipBinaryEntry&) comparand
    * @return <b>true</b> current string name > input string name
    */
   bool  SlipBinaryEntry::operator>(const SlipHashEntry& X) const {
      return (getBinaryKey() > X.getBinaryKey());
   }; // bool SlipBinaryEntry::operator>(const SlipHashEntry& X) const

       /*************************************************
        *                    Methods                    *
        ************************************************/

   /**
    * @brief Create a new entry using the input hash entry.
    * <p>The input hash entry acts as template for the creation of a new
    *    hash entry. The new hash entry is an exact duplicate of the
    *    input.</p>
    * @return new entry created from the heap
    */
   SlipHashEntry* SlipBinaryEntry::createHashEntry() const {
      SlipBinaryEntry* entry = new SlipBinaryEntry(*name, ptr, type, debugFlag);
      entry->primaryHash = primaryHash;
      entry->secondaryHash = secondaryHash;
      return entry;
   }; // SlipHashEntry const * const SlipBinaryEntry::createHashEntry(const SlipHashEntry& entry)

   /**
    * @brief Delete any associated list.
    * <p>This method does not delete any associated list.</p>
    * <p>SlipBinaryEntry item are only relevant durint list output. Any
    *    list contained in an entry is a duplicate pointer to a list.
    *    Deletion of the list will cause the list to be poorly formed
    *    in the application and eventually could cause a failure.</p>
    */
   void SlipBinaryEntry::deleteList() {

   }; // void SlipBinaryEntry::deleteList()

   /**
    * Wbrief There is not ascii key.
    * <p>Each hash table entry consists of a 2-tuple consisting of
    *    &lt;key, value&gt;. The key is an Ascii string and stored
    *    in the hash table entry. The key is used to hash the entry
    *    and place it into the hash table.</p>
    * @return the key Ascii string
    */
   const string& SlipBinaryEntry::getAsciiKey() const {
      static const string tmp("");
      return tmp;
   }; // const string& SlipBinaryEntry::getAsciiKey()

   /**
    * Wbrief The binary key is the address of the list header.
    * <p>Each hash table entry consists of a 2-tuple consisting of
    *    &lt;key, value&gt;. The key is an Ascii string and stored
    *    in the hash table entry. The key is used to hash the entry
    *    and place it into the hash table.</p>
    * @return <b>null</b>
    */
   const void * const SlipBinaryEntry::getBinaryKey() const {
      return ptr;
   }; //  const void*   SlipBinaryEntry::getBinaryKey()

   /**
    * @brief The ascii value is the name of the list.
    * <p>Each hash table entry consists of a 2-tuple consisting of
    *    &lt;key, value&gt;. The key is an Ascii string and stored
    *    in the hash table entry. The key is used to hash the entry
    *    and place it into the hash table.</p>
    * @return a <b>null</b> string
    */
   const string& SlipBinaryEntry::getAsciiValue() const {
      return *name;
   }; // const string& SlipBinaryEntry::getAsciiValue()

   /**
    * @brief Execute the primary hash function (sdbm).
    * <p>Execute and return the value of the primary hash function.</p>
    * @return (uint32_t) the hash key
    */
   uint32_t SlipBinaryEntry::primaryHashFunction() {
      HashData data;
      data.ptr = const_cast<void*>(ptr);
      return sdbm(data.name, sizeof(void*));
   }; // uint32_t SlipBinaryEntry::primaryHashFunction()

   /**
    * @brief Execute the secondary hash function (fnv1a).
    * <p>Execute and return the value of the secondary hash function.</p>
    * @return (uint32_t) the hash key
    */
   uint32_t SlipBinaryEntry::secondaryHashFuncion() {
      HashData data;
      data.ptr = const_cast<void*>(ptr);
      return fnv1a(data.name, sizeof(void*));
   }; // uint32_t SlipBinaryEntry::secondaryHashFuncion()

   /**
    * Wbrief There is no binary value.
    * <p>Each hash table entry consists of a 2-tuple consisting of
    *    &lt;key, value&gt;. The key is an Ascii string and stored
    *    in the hash table entry. The key is used to hash the entry
    *    and place it into the hash table.</p>
    * @return <b>null</b>
    */
   void const * const SlipBinaryEntry::getBinaryValue() const {
      return NULL;
   }; // const void*   SlipBinaryEntry::getBinaryValue()

   /**
    * @brief Change the ASCII name of a sublist.
    * <p>The primary key of a sublist is binary. The value is the ASCII name
    *    of the sublist. This value is changed.
    *    succeed.</p>
    * @param[in] name (string&) name to be changed
    * @return <b>true</b> name changed
    */
   bool SlipBinaryEntry::rename(const string& name)  {
      delete SlipHashEntry::name;
      SlipHashEntry::name = new string(name);
      return true;
   }; // bool SlipBinaryEntry::rename(const string& name) const

   /**
    * @brief Change the list pointer value.
    * @param key (void*) pointer to a list
    */
   void SlipBinaryEntry::setKey(const void* key) {
      ptr = const_cast<void*>(key);
   }; // void SlipBinaryEntry::setKey(const void* ptr)

   /**
    * @brief Generate the primary and secondary hash values from the binary key.
    */
   void SlipBinaryEntry::setHash() {
      primaryHash   = primaryHashFunction();
      secondaryHash = secondaryHashFuncion();
   }; // void SlipBinaryEntry::setHash()

}; // namespace slip