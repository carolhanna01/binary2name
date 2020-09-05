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
 * @file    SlipHash.h
 * @author  A. Schwarz
 * @date    December 20, 2013
 * 
 */

#ifndef SLIPHASH_H
#define	SLIPHASH_H

# include "SlipHashEntry.h"

/**
 * @brief Hash table management class.
 * 
 * <p>The hash function class maintains a hash table and hashing 
 *    functions for use in parsing and list output. During output 
 *    parsing the list headers referenced in a sublist and the 
 *    User Data object names are hashed. During input User Data 
 *    names and sublists are hashed.</p>
 * <p>Hash entry contents is not maintained by the hash management
 *    class.</p>
 * <p>There are some important differences between User Data and sublist 
 *    references.</p> 
 * <ul>
 *    <li>Sublist references name a list. The list will be inserted into
 *        another list as a sublist. During output parsing the list name
 *        is generated on-the-fly and the address of the list header is
 *        used as the hash key. During input processing, the list name
 *        is used as the hash key.</li>
 *    </li>
 *    <li>User Data names are used as the hash key for both input and
 *        output processing. During output processing the key value is
 *        ignored. During input processing the key value is a pointer
 *        to the User Data parsing function.</li>
 * </ul>
 */

# include <string>
# include "SlipDatum.h"
# include "SlipDef.h"
# include "SlipHeader.h"

using namespace std;

namespace slip {
/**
 * @class SlipHash
 * @brief Hash table maintenance.
 * <p><b>Overview</b></p><hr>
 * <p>The hash table is used during both input and output list 
 *    processing. During output of a list, it is the repository
 *    for the User Data names and the address of list headers
 *    used in a sublist. During output processing the User Data
 *    name are used to guarantee uniqueness and during input
 *    processing they become requirements to get and use User
 *    Data input parsing functions.<p>
 * <p>The list addresses are used during output processing to 
 *    guarantee that lists are unique, and during input processing 
 *    to allow expansion of sublists into their containing list.</p>
 * <p>User Data names and sublist names must be distinct. Duplicate
 *    names are not allowed.</p>
 * <p>The hash table is dynamically expandable. An initial allocation 
 *    is made and then when the table population and probe statistics
 *    reach a threshold, the table size is doubled. The maximum table
 *    size is >= 2**31 - 2 entries. Note that the space requirement
 *    minimum during rehashing is 2**32.
 * <p>Expansion is harsh. Each entry in the old table is rehashed into 
 *    the new table prior to deletion of the old table.</p>
 * <p>A closed form hashing algorithm is used. If a table probe for an
 *    empty slot fails because the slot has an entry, rehashing is done
 *    and will continue until an entry is found.</p>
 * <p>It is expected (statistically) that when the hash table occupancy is
 *    between 70% and 80% then a new hash table will have to be created
 *    because probing statistics will exceed an internal statistical
 *    threshold.</p>
 * <p><b>Constructors and Destructors</b></p><hr>
 * <ul>
 *    <li><b>SlipHash()</b> Create a hash table and give it an initial allocation.</li>
 *    <li><b>~SlipHash()</b> Delete all entries in the hash table and then
 *       delete the hash table.</li>
 *    <li><b>SlipHash(const SlipHash&)</b> Copy constructor is not supported.</li>
 * </ul>
 * <p><b>Operators</b></p><hr>
 * <ul>
 *    <li><b>operator[]</b> Allow direct indexing to an entry in the hash
 *       table.</li>
 * </ul>
 * <p><b>Methods</b></p><hr>
 * <ul>
 *    <li><b>insert</b> The function will insert the input into the table.</li>
 *    <li><b>toString()</b> Format and return all hash table entries.</li>
 *    <li><b>toString(int ndx)</b> Format and return a hash table entry
 *       for the given indexed item.</li>
 *    <li><b>toStringFull()</b> Format and return the full hash table (including
 *       empty entries).</li>
 *    <li><b>size()</b> Number of entries in the hash table.</li>
 * </ul>
*/

   class SlipHash {
   public:
       /**
        * @brief SlipHash return container.
        * <p>Return values from <i>SlipHash</i> methods are multi-valued.
        *    Method users are return the state of a method invocation and 
        *    the value returned as a result of a method invocation.</p>
        * <p>The Insert and search methods have two types of information
        *   to return: the <i>SlipHashEntry</i> that exists in the hash 
        *   table and the status of the hash table.</p>
        * <p>The following conditions can exist:</p>
        * <ul>
        *   <li>FOUND: The an entry with the same name has been found
        *       in the hash table. The entry is returned and the hash
        *       table index saved.</li>
        *   <li>INSERTED: No entry has been found and the input entry
        *       was inserted into the hash table. The entry is returned
        *       and the hash table index saved.</li>
        *   <li>FULL: The has table can not get anymore space. No entry
        *       was found and no entry was inserted.</li>
        * </ul>
        */
      class ReturnTuple {
      public:
         SlipHashEntry* entry;
         int ndx;                                             //<! index into hash table
         enum CONDITION{ ILLEGAL                              //!< invalid value
                       , EMPTY                                //!< empty slot found
                       , FOUND                                //!< entry found
                       , FULL                                 //!< no hash table space
                       , INSERTED                             //!< entry not found
                       , NOTEMPTY                             //!< slot not empty
         } condition;
      public:
         ReturnTuple(int ndx, SlipHashEntry* entry, CONDITION condition);
         ~ReturnTuple();
         string toString();
      }; // class ReturnTuple

   private:
      SlipHashEntry** hashTable;                              //!< hash table
      int nProbes;                                            //!< number of times more than one table probe needed
      int nEntries;                                           //!< number of entries in the table
      int tableSize;                                          //!< maximum number of entries in the table
      int nMaxProbes;                                         //!< maximum number of probes
      bool inputDebugFlag;                                    //!< input debug flag
      bool leakDebugFlag;                                     //!< memory leak flag
      bool hashDebugFlag;                                     //!< hash table debug flag
      bool debugFlag;                                         //!< all debug flags
      enum { INITIALTABLESIZE = 1024                          //!< initial number of entries
           , MAXPROBES        =   10                          //!< maximum nProbe before hash table rebuild
           };
   private:
      SlipHash(const SlipHash& X);
      int32_t  primaryHash(SlipHashEntry const* const entry);
      int32_t  quadraticHash(SlipHashEntry const* const entry);
      bool rebuild();
      string toString(const int ndx) const;
  public:
     SlipHash(int debugFlag = 0);
     ~SlipHash();
     SlipHashEntry& operator[](int ndx) const;
     void        deleteEntry(const ReturnTuple& tuple);
     ReturnTuple insert(SlipHashEntry& entry);
     void        outputHashTable();
     ReturnTuple search(SlipHashEntry& entry);
     void        setDebugOFF(int debugFlag = 0);
     void        setDebugON(int debugFlag = 0);
     int         size();
     void        write() const;
     void        writeFull() const;
  }; // class SlipHash
} // namespace slip
#endif	/* SLIPHASH_H */

