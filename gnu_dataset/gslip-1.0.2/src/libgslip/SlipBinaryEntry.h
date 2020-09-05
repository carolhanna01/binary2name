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
 * File:   SlipBinaryEntry.h
 * Author: skidmarks
 *
 * Created on January 3, 2014, 10:33 AM
 */

#ifndef SLIPBINARYENTRY_H
#define	SLIPBINARYENTRY_H

using namespace std;

namespace slip {
   
   /**
    * @class SlipBinaryEntry
    * @brief Manage hash entries with a binary key.
    * <p><b>Overview</b></p><hr>
    * <p>This class supports the creation and deletion of hash table entries
    *    where the search key is a pointer to a list. Entries from this class 
    *    are used in input and output operations for sublists. This class is 
    *    not used for input operations.</p>
    * <p>Primary key and secondary key values are pre-calculated during
    *    object construction. The primary key value is used by hash
    *    table management for initial search of the hash table and
    *    the secondary key value is used if the primary search fails.</p>
    * <p>An entry consists of a tuple:</p>
    * <table>
    *    <tr>
    *       <th><b>key</b></th>
    *       <td>The key is a pointer to a list.</td>
    *    </tr>
    *    <tr>
    *       <th><b>value</b></th>
    *       <td>Each key has an associated value. The value is a pointer 
    *           to a string representing the name of the list.</td>
    *    </tr>
    *    <tr>
    *       <th><b>primary hash value</b></th>
    *       <td>On construction the primary hash value is calculated.
    *           The value is used by the hash maintenence (<i>SlipHash</i>
    *           methods to initially probe into the hash table and
    *           determine if there is an empty slot or if the hash table
    *           entry matches a search condition.</td>
    *    </tr>
    *    <tr>
    *       <th><b>secondary hash value</b></th>
    *       <td>On construction the secondary hash value is calculated.
    *           If the primary hash value does not yield a usable result
    *           then the secondary hash value is used in a quadratic 
    *           search.</td>
    *    </tr>
    * </table>
    * <p><b>Constructors and Destructors</b></p><hr>
    * <ul>
    *  <li><b>SlipBinaryEntry</b> Create an entry.</li>
    *  <li><b>~SlipBinaryEntry</b> Destroys an entry. The list (<b>deleteList()</b>)
    *      is deleted and the list names are deleted. Note that if the
    *      list has been inserted into a sublist that the delete
    *      operation does not delete the list. </li>
    * </ul>
    * <p><b>Operators</b></p><hr>
    * <ul>
    *    <li><b>operator!=()</b> Compare two list pointers.</li>
    *    <li><b>operator&lt;()</b> Compare two list pointers.</li>
    *    <li><b>operator&lt;=()</b> Compare two list pointers.</li>
    *    <li><b>operator==()</b> Compare two list pointers.</li>
    *    <li><b>operator&gt;=()</b> Compare two list pointers.</li>
    *    <li><b>operator&gt;()</b> Compare two list pointers.</li>
    * </ul>
    * <p><b>Methods</b></p><hr>
    * <ul>
    *    <li><b>createHashEntry</b> Create a new hash entry using an input
    *        hash entry as a template.</li>
    *    <li><b>getAsciiKey</b> Return an empty string.</li>
    *    <li><b>getAsciiValue</b> Return the ascii (string) value used 
    *        in an entry.</li>
    *    <li><b>getBinaryKey</b> Return the list pointer.</li>
    *    <li><b>getBinaryValue</b> Return <b>null</b>.</li>
    *    <li<b>setKey</b> Set the binary key, the <b>ptr</b> field which
    *       points to a header.</li>
    *    <li><b>setHash</b> Calculate the primary and secondary hash key. The
    *        primary key must be present for this to be successful.</li>
    * </ul>
    */
   
   class SlipHeader;
   class SlipDatum;
   
   class SlipBinaryEntry : public SlipHashEntry {
      union HashData {
         void* ptr;
         char name[sizeof(void*)];
      };

   private:
      SlipBinaryEntry(const string& name, void * const ptr, Type type, int debugFlag = 0);
      virtual  uint32_t primaryHashFunction();
      virtual  uint32_t secondaryHashFuncion();
   public:
      SlipBinaryEntry(const string& name, int debugFlag = 0);
      SlipBinaryEntry(SlipHeader* head, Type type = R_AND_D, int debugFlag = 0);
      SlipBinaryEntry(const string& name, SlipHeader* header, int debugFlag = 0);
      SlipBinaryEntry(const SlipDatum&  userData, Parse parse, int debugFlag = 0);
      virtual ~SlipBinaryEntry();
      virtual bool  operator!=(const SlipHashEntry& X) const;
      virtual bool  operator<(const SlipHashEntry& X) const;
      virtual bool  operator<=(const SlipHashEntry& X) const;
      virtual bool  operator==(const SlipHashEntry& X) const;
      virtual bool  operator>=(const SlipHashEntry& X) const;
      virtual bool  operator>(const SlipHashEntry& X) const;
      virtual SlipHashEntry*    createHashEntry() const;
      virtual void              deleteList();
      virtual const string&     getAsciiKey() const;
      virtual const void* const getBinaryKey() const;
      virtual const string&     getAsciiValue() const;
      virtual const void* const getBinaryValue() const;
      virtual bool              rename(const string& name);
      virtual void              setKey(const void* key);
      virtual void              setHash();
    }; // class SlipBinaryEntry

}; // namespace slip


#endif	/* SLIPBINARYENTRY_H */

