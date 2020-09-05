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
 * @file    SlipHash.cpp
 * @author  A. Schwarz
 * @date    December 20, 2013
 */

# include <string>
# include <iomanip>
# include <iostream>
# include <sstream>
# include "SlipHash.h"
# include "SlipHashEntry.h"
# include "SlipRead.h"

namespace slip {

#ifdef DEBUG_IO
   #undef DEBUG
   #define DEBUG(flag, stmt) if (flag) { stmt }
#else
   #define DEBUG(flag, stmt)
#endif

   SlipHash::ReturnTuple::ReturnTuple(int ndx, SlipHashEntry* entry, CONDITION condition)
              : ndx(ndx)
              , entry(entry)
              , condition(condition) {}
   SlipHash::ReturnTuple::~ReturnTuple() {};

   string SlipHash::ReturnTuple::toString() {
      const string sCondition[]
                     = { "ILLEGAL  "
                       , "EMPTY    "
                       , "FOUND    "
                       , "FULL     "
                       , "INSERTED "
                       , "NOTEMPTY "
                       };

      stringstream pretty;
      pretty << setw(4) << ndx << " " << sCondition[condition] << " ";
      if (entry) pretty << entry->toString();
      return pretty.str();
   };

       /*************************************************
        *           Constructors & Destructors          *
        ************************************************/

   /**
    * @brief The copy constructor is not allowed.
    * <p>The copy constructor is a private method and not available to mortals.</p>
    * @param[in] X (SlipHash&) an original version of a SlipHash object
    */
   SlipHash::SlipHash(const SlipHash& X) 
                        : inputDebugFlag(false)    // no debug output
                        , leakDebugFlag(false)
                        , hashDebugFlag(false)     // no debug output
                        , debugFlag(0)
                        , nEntries(0)              // no entries in hash table
                        , nProbes(0)               // no  probes > threshold
                        , nMaxProbes(-1)           // maximum number of probes
                        , tableSize(0)             // initial table size
   {
   }; // SlipHash::SlipHash(const SlipHash&)

   /**
    * @brief Create the initial version of the hash table.
    * <p>The hash table is created with <b>null</b> pointers for each entry.
    *    the hash table is expandable and the initial size need not be the
    *    final size.</p>
    */
   SlipHash::SlipHash(int debugFlag)
                        : inputDebugFlag((bool)(debugFlag & SlipRead::INPUT))       // no debug output
                        , leakDebugFlag((bool)(debugFlag & SlipRead::LEAK))
                        , hashDebugFlag((bool)(debugFlag & SlipRead::HASH))         // no debug output
                        , debugFlag(debugFlag)
                        , nEntries(0)              // no entries in hash table
                        , nProbes(0)               // no  probes > threshold
                        , nMaxProbes(-1)           // maximum number of probes
                        , tableSize(INITIALTABLESIZE) // initial table size
   {
      hashTable = new SlipHashEntry*[INITIALTABLESIZE];
      for( int i = 0; i < INITIALTABLESIZE; i++ )
         hashTable[i] = NULL;
   }; // SlipHash::SlipHash()

   /**
    * @brief delete all entries in the hash table then delete the hash table.
    */
   SlipHash::~SlipHash() {
      ios_base::fmtflags fmtflg = cout.flags();
      const int size = min(nEntries, tableSize);
      for ( int i = 0, j = 0; j < size; i++ ) {
         if (hashTable[i]) {
            j++;
            DEBUG(leakDebugFlag, 
            cout << left << setw(27) << "SlipHash::~SlipHash[" 
                 << dec << i << "] "
                 << hashTable[i]->toString() << endl;)
            delete hashTable[i];
            hashTable[i] = NULL;
         }
      }
      cout.flags(fmtflg);
      nEntries = 0;
      delete hashTable;
   }; // SlipHash::~SlipHash()

       /*************************************************
        *                    Methods                    *
        ************************************************/

   /**
    * @brief Return the nth has table value
    * @param ndx (int) indexing variable
    * @return (SlipHashEntry&) hash table entry
    */
   SlipHashEntry& SlipHash::operator[](int ndx) const {
      return *hashTable[ndx];
   }; // SlipHash::SlipHashEntry& operator[](int ndx) const
   
   /**
    * @brief delete an entry from the hash table and fixup the count of live entries.
    * @param[in] tuple (ReturnTuple&) packet containing the hash table index
    */
   void SlipHash::deleteEntry(const ReturnTuple& tuple) {
      if (nEntries) nEntries--;
      hashTable[tuple.ndx] = NULL;
   }; // void SlipHash::deleteEntry(const ReturnTuple& tuple)
   
   /**
    * @brief Driver for insertion into the hash table.
    * <p>The hashing algorithm attempts to find either and empty slot for
    *    a new entry or a non-empty slot containing the same name as the
    *    input name. The hashing algorithm does not populate an empty
    *    slot with a value, that functionality belongs to the caller method.</p>
    * <p>If the initial attempt to find an empty slot or a slot containing
    *    the same name as in the input argument is not successful, then a
    *    rehash and quadratic search is performed. At the end of the search
    *    if the number of probes requiring a quadratic search exceeds some
    *    threshold, then the hash table is rebuilt.</p>
    * <p>If during a hash table rebuild and attempt to rebuild (a recursive
    *    rebuild) occurs, then the allocation will fail and the hash table
    *    will be left in an uncertain state. This is a fatal error (however
    *    unlikely).</p>
    * <b>NOTE</b>
    * The hash method assumes that the caller will use the hash table index
    * that is returned. Based on this assumption, the number of entries is
    * updated. If the slot is not filled with an entry then the number of
    * entries count will be incorrect and bedlam will ensue.
    * @param[in] entry (SlipHashEntry*) hash table entry.
    * @return ReturnTuple containing the hash table index, return condition and entry
    */
   SlipHash::ReturnTuple SlipHash::insert(SlipHashEntry& entry) {
      static int activeFlag = false;
      ReturnTuple::CONDITION condition = ReturnTuple::FOUND;
      DEBUG(inputDebugFlag,
      cout << left << setw(27) << setfill(' ') << "SlipHash::insert" << entry.toString() << endl;)
      DEBUG(hashDebugFlag,
      cout << "Enter " << setw(24) << left
           << "SlipHash::hash(" << setw(8) << entry.getAsciiKey() << ") "
           << "nProbes " << setw(4) << nProbes
           << " nEntries " << setw(4) << nEntries << endl;)
      int32_t ndx = primaryHash(&entry);
      if (ndx < 0) {                                                            // first guess wrong
         ndx = quadraticHash(&entry);
      }

      if (ndx >= 0) {
         if (nProbes > MAXPROBES) {                                             // double table size
            if ((float)nEntries/(float)tableSize > .70) {
               nEntries--;
               if (activeFlag) {
                  activeFlag = false;
                  ndx = -1;
               } else if (rebuild() == false) {                                 // no more heap
                  ndx = -1;
               } else {                                                         // recalculate ndx
                  activeFlag = true;                                            // recursion flag
                  ndx = insert(entry).ndx;
                  activeFlag = false;
               }
            }
         }
      }

      if (ndx >= 0) {
         if (hashTable[ndx] == NULL) {
            nEntries++;
            condition = ReturnTuple::INSERTED;
              hashTable[ndx] = entry.createHashEntry();
         }
      } else condition = ReturnTuple::FULL;
      DEBUG(hashDebugFlag,
      cout << "Exit  " << setw(24) << left << "SlipHash::hash("
           << setw(8) << entry.getAsciiKey() << ") => " << setw(10) << ndx << endl;)
      return ReturnTuple(ndx, hashTable[ndx], condition);
   }; // SlipHash::ReturnTuple SlipHash::insert(SlipHashEntry& entry)

   /**
    * @brief Search hash table for an entry.
    * <p>Search the hash table. If the search is unsuccessful return a
    *    <b>-1</b>.</p>
    * @param[in] inputEntry (SlipHashEntry&) search entry.
    * @return ReturnTuple containing the hash table index, return condition and entry
    */
    SlipHash::ReturnTuple SlipHash::search(SlipHashEntry& inputEntry) {
      ReturnTuple::CONDITION condition = ReturnTuple::FOUND;
      SlipHashEntry* entry = &inputEntry;
      DEBUG(hashDebugFlag,
      cout << setw(27) << left << "Enter SlipHash::search" 
           << setw(10) << right << entry->getAsciiKey()
           << " nProbes "  << setw(4) << nProbes
           << " nEntries " << setw(4) << nEntries << endl;)
      int32_t ndx = primaryHash(entry);
      if (ndx < 0) {
         ndx = quadraticHash(entry);
      }

      if (ndx >= 0) {
         if (hashTable[ndx] == NULL)
            condition = ReturnTuple::EMPTY;
         else entry = hashTable[ndx];
      }

      DEBUG(hashDebugFlag,
      cout << setw(27) << left << "Exit  SlipHash::search"
           << setw(10) << right << entry->getAsciiKey() 
           << " => " << setw(10) << ndx << endl;)
      return ReturnTuple(ndx, entry, condition);
   }; //  SlipHash::ReturnTuple SlipHash::search(SlipHashEntry& entry)

   /**
    * @brief Turn debugging output on.
    */
   void SlipHash::setDebugOFF(int debugFlag) {
      inputDebugFlag = (bool)(debugFlag & SlipRead::INPUT)? false: inputDebugFlag;
      hashDebugFlag  = (bool)(debugFlag & SlipRead::HASH)? false: inputDebugFlag;
   }; // void SlipHash::setDebugOFF(int debugFlag)

   /**
    * @brief Turn debugging output off;
    * <p>Debug logs will be created if this is a debug build.</p>
    */
   void SlipHash::setDebugON(int debugFlag){
      inputDebugFlag = (bool)(debugFlag & SlipRead::INPUT);
      hashDebugFlag  = (bool)(debugFlag & SlipRead::HASH);
      leakDebugFlag  = (bool)(debugFlag & SlipRead::LEAK);
   }; // void SlipHash::setDebugON(int debugFlag)

   /**
    * @brief Current Hash Table size.
    * <p>The Hash Table automatically grows when space is needed. The
    *    Hash Table size at the time of call is not permanent and can
    *    change each time an entry is posted into the table.</p>
    * @return Hash Table Size
    */
   int SlipHash::size() {
      return tableSize;
   }; // int SlipHash::size()

   /**
    * @brief Hash the name and see if slot NULL.
    * <p>An fnv1a hashing algorithm is used on the input name. The
    *    hash table is looked at to determine if the slot is open
    *    (the entry is <b>null</b>) or if the slot contains an
    *    entry with the same name. If it does, fine, otherwise
    *    return -1.</p>
    * @param[in] entry (SlipHashEntry*) hash table entry.
    * @return -1, unable to find open slot otherwise slot index
    */
   inline
   int32_t SlipHash::primaryHash(SlipHashEntry const* const entry) {
      int32_t ndx = entry->getPrimaryHash() % tableSize;
      DEBUG(hashDebugFlag, {
      cout << setw(27) << left << "SlipHash::primaryHash"
           << setw(10) << right << entry->getAsciiKey() 
           << " => hashTable[" << ndx << "] = ";
      if (!hashTable[ndx]) cout << "NULL";
      else cout << hashTable[ndx]->getAsciiKey();
      cout << " nEntries " << nEntries;})
      if (hashTable[ndx] == NULL) {
      } else if (*(hashTable[ndx]) != *entry) {
         ndx = -1;
      }
      DEBUG(hashDebugFlag,
      cout << " return ndx " << ndx << endl;)
      return ndx;
   }; // int32_t SlipHash::primaryHash(string const* const name)

   /**
    * @brief Rehash the input and perform a quadratic search.
    * <p>The input name is rehashed using a different hashing algorithm
    *    then the primary hash function.</p>
    * <p>The quadratic algorithm chosen yields a complete table search when
    *    the table size is a power of 2. This is a tailored result using
    *    the fact that we know that the power of 2 requirement is satisfied.
    *    If the table is not a power of two then the search minimum limit
    *    is 1/2 the table size.</p>
    * @param[in] entry (SlipHashEntry*) hash table entry.
    * @return -1, unable to find open slot, otherwise the entry index
    */
   int32_t SlipHash::quadraticHash(SlipHashEntry const* const entry) {
      enum { THRESHOLD = 2 };                               // probe threshold
      int i = 0;
      uint32_t base = entry->getSecondaryHash();
      int32_t ndx = base % tableSize;
      DEBUG(hashDebugFlag,
      cout << setw(27) << left  << setfill(' ') << "SlipHash::quadraticHash"
           << setw(10) << right << entry->getAsciiKey() << " <= " << ndx << endl;)

      while(i < tableSize - 1) {
         if (hashTable[ndx] == NULL) {
            nProbes += (i > THRESHOLD)? 1: 0;
            nMaxProbes = max(nMaxProbes, i+1);
            break;
         } else if (*hashTable[ndx] == *entry) {
            break;
         }
         ndx = (base + (i*(i + 1))/2) % tableSize;
         i++;
      }
      DEBUG(hashDebugFlag,
      cout << setw(27) << left << setfill(' ') << "Exit SlipHash::quadraticHash"
           << setw(19) << left << entry->getAsciiKey()
           << setw(6) << right << ndx
           << setw(6) << "  nEntries " << nEntries
           << setw(2) << " nProbes " << nProbes
           << setw(6) << " tries " << i
           << setw(4) << " nMaxProbes " << nMaxProbes
           << " tableSize " << tableSize
           << endl;)
      if (i >= tableSize) ndx = -1;
      return ndx;
   }; //uint32_t SlipHash::quadraticHash(string const* const name)
   
   /**
    * @brief Brute force hash table entry output.
    * <p>Outputs the hex address of each entry in the hash table.</p>
    */
   void SlipHash::outputHashTable() {
      ios_base::fmtflags fmtflg = cout.flags();
      const int size = min(nEntries, tableSize);
      for ( int i = 0, j = 0; j < size; i++ ) {
         if (hashTable[i]) {
            j++;
            cout << left << setw(27) << "SlipHash::outputHashTable[" << dec << i << "] "
                 << hex << hashTable[i] << endl;
         }
      }
      cout.flags(fmtflg);
   }; // void SlipHash::outputHashTable()

   /**
    * @brief Increase the hash table size and rebuild the entries.
    * <p>The current hash table size is doubled and entries from the old
    *    hash table are inserted into the new hash table, then the old
    *    hash table is deleted.</p>
    * <p>The rebuild algorithm works by:</p>
    * <ol>
    *    <li>Setting the recursion flag (active flag) to true.</li>
    *    <li>Allocating space for the new hash table.</li>
    *    <li>Resetting external hash table variable.</li>
    *    <li>Setting all entries in the new hash table to <b>null</b>.</li>
    *    <li>Rehashing the old entries into the new table.</li>
    *    <li>Fixing up the environment (deleting the old hash table and
    *        doing fixup).</li>
    * </ol>
    * <p>If during rehash an attempt is made to rebuild the new hash table
    *    then we have experienced a fatal error and will return <b>false</b>.</p>
    *
    * @return <b>true</b> rebuild successful <b>false</b> rebuild failed
    */
   bool SlipHash::rebuild() {
      static bool activeFlag = false;
      bool retval = true;
      DEBUG(hashDebugFlag,
      cout << setw(30) << "SlipHash::rebuild()"
           << "activeFlag " << boolalpha<< activeFlag << endl;)
      if (activeFlag) {
         /*
          * Catastrophic error. Attempt to rebuild the hash table
          * recursively. This means that the hashing algorithm is
          * faulty and that it is not possible to avoid permanent
          * collisions. Get a life and get a new hashing algorithm!
          */
         activeFlag = false;
         retval = false;
      } else {
         SlipHashEntry** oldHashTable  = hashTable;
         try {
            const int numEntries = nEntries;
            hashTable = new SlipHashEntry*[2*tableSize];
            activeFlag = true;
            nProbes = 0;
            nEntries = 0;
            nMaxProbes = 0;
            for( int i = 0; i < 2*tableSize; i++ )
               hashTable[i] = NULL;
            for( int i = 0, j = 0;
               (i < tableSize) && (j < numEntries);
               i++) {
               if (oldHashTable[i] != NULL) {
                  j++;
                  int32_t ndx = insert(*(oldHashTable[i])).ndx;
                  if (ndx < 0) {
                     retval = false;
                     delete hashTable;
                     hashTable = oldHashTable;
                     break;
                  }
                  hashTable[ndx] = oldHashTable[i];
               }
            }
            if (retval) {
               tableSize = 2 * tableSize;
            }
         } catch(std::bad_alloc& ba) {
            cout << "ERROR SlipHash::rebuild bad_alloc caught: " << ba.what() << '\n';
            hashTable = oldHashTable;
            retval = false;
         } catch(...) {
            cout << "ERROR SlipHash::rebuild default caught: "  << '\n';
            hashTable = oldHashTable;
            retval = false;
         }
         activeFlag = false;
      }
      return retval;
   }; // bool SlipHash::rebuild()

   /**
    * @brief Output a single hash table entry.
    * @param ndx (int) index into the hash table
    */
   string SlipHash::toString(int ndx) const {
      stringstream pretty;
      pretty << "[" << ndx << "] ";
      if (hashTable[ndx]) pretty << hashTable[ndx]->toString();
      else                pretty << "NULL";
      return pretty.str();
   }; // SlipHash::toString(int ndx) const

   /**
    * @brief Output all non-NULL hash table entries.
    */
   void SlipHash::write() const {
      for( int ndx = 0, j = 0;
           (ndx < tableSize) && (j < nEntries);
           ndx++)
         if (hashTable[ndx] != NULL) {
            cout << toString(ndx) << endl;;
            j++;
         }
   }; // SlipHash::write() const

   /**
    * @brief Output the entire hash table
    */
   void SlipHash::writeFull() const {
      for (int ndx = 0; ndx < tableSize; ndx++) {
         cout << "[" << ndx << "] " <<  toString(ndx) << endl;;
      }
   }; // SlipHash::writeFull() const

} // namespace slip
