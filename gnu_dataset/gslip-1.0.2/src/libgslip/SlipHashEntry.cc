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
 * @file    SlipHashEntry.cpp
 * @author  A. Schwarz
 * @date    December 30, 2013
 *
 * @brief Implementation of Class for Hash entry items.
 */

# include <iostream>
# include <iomanip>
# include <string>
# include "SlipCell.h"
# include "SlipDescription.h"
# include "SlipErr.h"
# include "SlipHashEntry.h"
# include "SlipHeader.h"
# include "SlipRead.h"
# include "SlipRegister.h"

using namespace std;

namespace slip {

#ifdef DEBUG_IO
   #undef DEBUG
   #define DEBUG(flag, stmt) if (flag) { stmt }
#else
   #define DEBUG(flag, stmt)
#endif

   /**
    * @class SlipHashEntryAnonymous
    * @brief An anonymous <b>SlipHashEntry</b>.
    * <p>Anonymous list with a forward reference to a <b>namedList</b>
    *    require a <i>SlipHashEntry</i> object to be placed in the
    *    <b>namedList</b> descriptor chain. That is when we have:</p>
    * <tt><pre>
    *    list ( ( &lt; (name} &gt; ) )
    * </pre></tt>
    * <p>In order for the forward reference to <i>name</i> to be resolved
    *    as the definition of the Descriptor List, the anonymous list
    *    containing <i>name</i> must be placed on the <i>name</i>
    *    descriptor chain. The anonymous list has no representation in
    *    the hash table (it has no name) and this class must be used
    *    to simulate a hash table entry.</p>
    * <p>When the forward reference is resolved in  the <b>namedList</b>
    *    <b>resolveDescriptorReferences</b> is called deleting this
    *    object.</p>
    */
   class SlipHashEntryAnonymous : public SlipHashEntry {
      static const string gbg;
   protected:
      virtual uint32_t primaryHashFunction() { return 0; };
      virtual uint32_t secondaryHashFuncion() { return 0; };
      void resolveDescriptorReferences() { delete this; };
   public:
      SlipHashEntryAnonymous(void* ptr, int debugFlag)
                           : SlipHashEntry(ptr, DEFINED, debugFlag) {
         completeFlag = true;
      }
      virtual ~SlipHashEntryAnonymous() { };
      virtual bool  operator!=(const SlipHashEntry& X) const  { return true; }
      virtual bool  operator<(const SlipHashEntry& X) const  { return true; }
      virtual bool  operator<=(const SlipHashEntry& X) const  { return true; }
      virtual bool  operator==(const SlipHashEntry& X) const  { return true; }
      virtual bool  operator>=(const SlipHashEntry& X) const  { return true; }
      virtual bool  operator>(const SlipHashEntry& X) const  { return true; }

      bool                       copyDList(SlipDescription* to);
      virtual SlipHashEntry*     createHashEntry() const  { return NULL; }
      virtual void               deleteList()  { }
      virtual const string&      getAsciiKey() const  { return gbg;  }
      virtual const string&      getAsciiValue() const  { return gbg; }
      virtual const void * const getBinaryValue() const  { return NULL; }
      virtual const void * const getBinaryKey() const  { return NULL; }
      virtual bool               rename(const string& name)  { return true; }
      virtual void               setKey(const void* key)  { }
      virtual void               setHash()  { }
   }; // class SlipHashEntryAnonymous

   const string SlipHashEntryAnonymous::gbg("");

       /*************************************************
        *           Constructors & Destructors          *
        ************************************************/
   /**
    * @brief Create an interim format <i>SlipHash</i> entry.
    * <p>During input processing a temporary entry is created for
    *    <i>SlipDatum</i> objects and list headers (<i>SlipHeader</i>).
    *    This state and deletion of this temporary entry is controlled
    *    by the input processor as mediated by <i>SlipRegister</i>.</p>
    * <p>The input type is not checked for validity. It is assumed that
    *    <i>SlipRegister</i> knows what it is doing (I know the coder and
    *    this may not be a safe assumption).</p>
    * @param[in] ptr (void*) pointer to a <i>SlipHeader</i>or <i>SlipDatum</i>
    * @param[in] type (Type) UNKNOWN or DATA
    * @param[in] debugFlag (int) debug flag mask
    */
   SlipHashEntry::SlipHashEntry(void* ptr, Type type, int debugFlag)
                               : completeFlag(false)
                               , debugFlag(debugFlag)
                               , hashDebugFlag((bool)(debugFlag & SlipRead::HASH))
                               , inputDebugFlag((bool)(debugFlag & SlipRead::INPUT))
                               , leakDebugFlag((bool)(debugFlag & SlipRead::LEAK))
                               , descriptorChain(NULL)
                               , link(NULL)
                               , name(NULL)
                               , nestedPtr(NULL)
                               , ptr(ptr)
                               , type(type) { 
      DEBUG(leakDebugFlag,
      cout << left << setw(27) << setfill(' ') << "SlipHashEntry::create " << hex 
           << setw(sizeof(void*)) << setfill('0') << (void*)this << ' ' << toString() << endl;)
   }; // SlipHashEntry::SlipHashEntry(void* ptr, Type type, int debugFlag)

   /**
    * @brief Create a hash table entry for User Data.
    * @param[in] name (string&) name of named list
    * @param[in] ptr (SlipHeader*) pointer to list
    * @param[in] type (Type) type of entry
    * @param[in] debugFlag (int) debug flag mask
    */
   SlipHashEntry::SlipHashEntry(const string& name, void* ptr, Type type, int debugFlag)
                               : completeFlag(false)
                               , debugFlag(debugFlag)
                               , hashDebugFlag((bool)(debugFlag & SlipRead::HASH))
                               , inputDebugFlag((bool)(debugFlag & SlipRead::INPUT))
                               , leakDebugFlag((bool)(debugFlag & SlipRead::LEAK))                               , descriptorChain(NULL)
                               , link(NULL)
                               , name(new string(name))
                               , nestedPtr(NULL)
                               , ptr(ptr)
                               , type(type) { 
      DEBUG(leakDebugFlag,
      cout << left << setw(27) << setfill(' ') << "SlipHashEntry::create " << hex << setw(sizeof(void*)) 
           << setfill('0') << (void*)this << ' ' << toString() <<endl;)
   }; // SlipHashEntry::SlipHashEntry(const string& name, void* ptr, Type type, int debugFlag)

   /**
    * @brief Create a hash table entry for User Data.
    * <p>Creates an entry during parsing.</p>
    * @param[in] desc (SlipDescription*)
    * @param[in] type (Type) type of entry being created
    * @param[in] debugFlag (int) debug flag mask
    */
   SlipHashEntry::SlipHashEntry(SlipDescription* desc, Type type, int debugFlag)
                               : completeFlag(false)
                               , debugFlag(debugFlag)
                               , hashDebugFlag((bool)(debugFlag & SlipRead::HASH))
                               , inputDebugFlag((bool)(debugFlag & SlipRead::INPUT))
                               , leakDebugFlag((bool)(debugFlag & SlipRead::LEAK))                               , descriptorChain(NULL)
                               , link(NULL)
                               , name((desc->getName())? new string(*desc->getName()): NULL)
                               , nestedPtr(NULL)
                               , ptr(desc->getPtr())
                               , type(type) { 
      DEBUG(leakDebugFlag,
      cout << left << setw(27) << setfill(' ') << "SlipHashEntry::create " << hex 
           << setfill('0') << setw(sizeof(void*)) << (void*)this << ' ' << toString() << endl;)
   }; // SlipHashEntry::SlipHashEntryconst SlipDescription* desc, Type type, int debugFlag)

   /**
    * @brief Destructor for a hash table entry.
    * <p>The <i>name</i> field is a copy of the input name and is always deleted
    *    when the class is deleted.</p>
    * <p>If <b>nestedPtr</b> if defined then delete it if the object does not
    *    have a name, otherwise do not delete. If object has a name then it
    *    refers to something on the hash table and will be deleted when the
    *    hash table is deleted. If the object does not have a name, then it
    *    refers to an anonymous list and the list container (<b>nestedPtr</b>)
    *    must be deleted.</p>
    */
   SlipHashEntry::~SlipHashEntry() {
      DEBUG(leakDebugFlag, {
      cout << left << setw(27) << setfill(' ') << "SlipHashEntry::delete " << hex << setw(sizeof(void*)) 
           << setfill('0') << (void*)this << ' ' << this->toString() << endl;
      
      if (name) {
         cout << left << setw(27) << setfill(' ') << "SlipHashEntry::name " << hex << setw(sizeof(void*)) 
              << setfill('0') << name << ' ' << setfill(' ') << setw(10) << right << *name << endl;
         delete name;
         name = NULL;
      }
      cout << endl;})

      if (descriptorChain) {
         while(descriptorChain) {
            SlipHashEntry* next = descriptorChain->link;
            delete descriptorChain;
            descriptorChain = next;
         }
      }
      if (nestedPtr && !(nestedPtr->name)) {
         delete nestedPtr;
      }
      type = ILLEGAL;
      nestedPtr = NULL;
   }; // SlipHashEntry::~hashItem()      type = ILLEGAL;


       /*************************************************
        *                    Methods                    *
        ************************************************/
   /**
    * @brief Process anonymous lists containing Descriptor Lists.
    * <p>A <i>SlipHashEntry</i> object is created for an anonymous list with
    *    a Descriptor List associated with it if the Descriptor List is a
    *    forward reference to a <b>namedList</b>, The <i>SlipHashEntry</i>
    *    becomes the container for the Descriptor List header.</p>
    * <p>If the <b>namedList</b> is defined then copy the list to the
    *    input and return, otherwise create and insert a <i>SlipHashEntry</i>
    *    object into the <b>namedList</b> descriptor chain.</p>
    * @see copyDList(SlipHashEntry* to)
    * @param[in] to (SlipDescription*) Descriptor containing anonymous list reference
    * @return <b>true</b> if input is not <b>null</b>
    */
   bool SlipHashEntry::copyDList(SlipDescription* to) {
      bool flag = false;
      if (to){
         if (isComplete()) {
            *to->getPtr() = *(SlipHeader*)ptr;
         } else {
            insert(new SlipHashEntryAnonymous(to->getPtr(), debugFlag));
         }
      }
      DEBUG(inputDebugFlag,
      cout << left << setw(27) << setfill(' ') << "SlipHashEntry::copyDList" << to->toString() << endl;)
      return flag;
   }; // bool SlipHashEntry::copyDList(SlipDescription* to)

   /**
    * @brief Copy the current list or the Description List.
    * <p>The 'from' (this object) is a <i>SlipHashEntry</i> objec of the
    *    source list for the 'to' Descriptor List. If the from  <b>namedList</b>
    *    is defined then copy its contents to the 'from' Descriptor List
    *    list. IF the <b>namedList</b> is not defined, then insert the input
    *    <i>SlipHashEntry</i> object onto the descriptor chain.</p>
    * @param[in] to ((SlipHashEntry*) reference to a Descriptor List header
    * @return <b>true</b> if input is not <b>null</b> or <b>USERDATA</b>
    */
   bool SlipHashEntry::copyDList(SlipHashEntry* to) {
      bool flag = false;

      if (type != USERDATA) {
         if (!to) return flag;

         SlipHeader* head = (SlipHeader*)(to->ptr);
         SlipHeader* dList = *(SlipHeader**)(head->getHeadDescList());
         if (dList == NULL) {
            postError(__FILE__, __LINE__, SlipErr::E2041, "SlipHashEntry::copyDList", " ", "Descriptor List missing in list.");
         } else if (type != ILLEGAL) {
            flag = true;
            if (isComplete()) {
               *dList = *(SlipHeader*)ptr;
            } else {
               insert(to);
            }
         }
      }
      DEBUG(inputDebugFlag,
      cout << left << setw(27) << setfill(' ') << "SlipHashEntry::copyDList" << to->toString() << endl;)
      return flag;
   }; // bool SlipHashEntry::copyDList(SlipDescription* to)

   /**
    * @brief Implement a 32-bit hashing algorithm using FNV1a.
    *
    * @author Glenn Fowler
    * @author Phong Vo
    * @author Landon Curt Noll
    * @param[in] name (string&) string to be hashed
    * @return hash value
    */
   uint32_t SlipHashEntry::fnv1a(const string& name) {
      static const uint32_t fnv_Seed  = 0x811C9DC5;
      static const uint32_t fnv_Prime = 0x01000193;
      int const size = name.length();
      int hash = fnv_Seed;
      for(int i = 0; i < size; i++) {
         hash = ((uint8_t)name[i] ^ hash) * fnv_Prime;
      }
      DEBUG(hashDebugFlag,
      cout << setw(27) << setfill(' ') << left << "SlipHashEntry::fnv1a"
           << setw(10) << right << name << " => " << hash << endl;)
      return (hash == (int)0xFFFFFFFF)? hash--: hash;
   }; // uint32_t SlipHashEntry::fnv1a(const string& name)

   /**
    * @brief Implement a 32-bit hashing algorithm using FNV1a.
    *
    * @author Glenn Fowler
    * @author Phong Vo
    * @author Landon Curt Noll
    * @param[in] name (string&) string to be hashed
    * @param[in] size (int) number of characters in name
    * @return hash value
    */
   uint32_t SlipHashEntry::fnv1a(char name[], int size) {
      static const uint32_t fnv_Seed  = 0x811C9DC5;
      static const uint32_t fnv_Prime = 0x01000193;
      int hash = fnv_Seed;
      for(int i = 0; i < size; i++) {
         hash = ((uint8_t)name[i] ^ hash) * fnv_Prime;
      }
      DEBUG(hashDebugFlag,
      cout << setw(27) << setfill(' ') << left << "SlipHashEntry::fnv1a"
           << setw(10) << right << name << " => " << hash << endl;)
      return (hash == (int)0xFFFFFFFF)? hash--: hash;
   }; // uint32_t SlipHashEntry::fnv1a(char name[], int size)

   /**
    * @brief Return a pointer to the Descriptor List <i>SlipHashEntry</i> object
    * <p>If the input <i>SlipDescription</i> object has a Descriptor List reference
    *    then it is a <b>namedList</b>. Set the global variable <b>nestedPtr</b>
    *    to the <b>namedList</b> a <i>SlipHshEntry</i> object.</p>
    * @param[in] reg (SlipRegister*) pointer to the register/hash table
    * @param[in] desc (SlipDescription*) description entry
    * @return (SlipHashEntry*) the input <b>namedList</b> pointer or <b>null</b>
    */
   SlipHashEntry* SlipHashEntry::getEntry(void* reg, SlipDescription* desc) {
      nestedPtr = NULL;

      if (desc->getDesc()) {
         SlipHash::ReturnTuple tuple = ((SlipRegister*)reg)->search(*((desc->getDesc())->getName()));
         if (tuple.condition != SlipHash::ReturnTuple::FOUND) {
            postError(__FILE__, __LINE__, SlipErr::E4017, "SlipHashEntry::resolveForwardReferences", *name, "");
         } else {
            nestedPtr = tuple.entry;
            DEBUG(inputDebugFlag,
            cout << left << setw(27) << setfill(' ') << "SlipHashEntry::getEntry"
                 << nestedPtr->toString() << endl;)
         }
      } else {
         DEBUG(inputDebugFlag,
         cout << left << setw(27) << setfill(' ') << "SlipHashEntry::getEntry" << endl;)
      }
      return nestedPtr;
   }; // SlipHashEntry* SlipHashEntry::getEntry(void* reg, SlipDescription* desc)

   /**
    * @brief Return the next <i>SlipHashEntry</i> object on the descriptor chain
    * @return (SlipHashEntry*) next entry on the descriptor chain
    */
   SlipHashEntry* SlipHashEntry::getNext() {
      return descriptorChain->link;
   }; // SlipHashEntry* SlipHashEntry::getNext()
   /**
    * @brief Return the primary hash value.
    * <p>The primary hash value is used to determine the position in
    *    the hash table for the entry. If the index is, well, not
    *    <b>null</b> then the secondary hash value is used.</p>
    * @return (uint32_t) primary hash value.
    */
   uint32_t SlipHashEntry::getPrimaryHash() const {
      return primaryHash;
   }; // uint32_t SlipHashEntry::getPrimaryHash() const

   /**
    * @brief Return the secondary hash value.
    * <p>If the hash table is not empty using the primary hash as an
    *    index, then quadratic searching is done using the secondary
    *    hash value.</p>
    * @return (uint32_t) secondary hash value
    */
   uint32_t SlipHashEntry::getSecondaryHash() const {
      return secondaryHash;
   }; // uint32_t SlipHashEntry::getSecondaryHash() const

   /**
    * @brief Return the type of hash table entry.
    * <p>Each hash table entry represents a list pointer or a pointer
    *    to a User Data method. The Type distbuishes between them.</p>
    * <table border="0">
    * <tr>
    *    <td>USERDATA</td>
    *    <td>The entry is a pointer to a User Data Method.</td>
    * </tr>
    * <tr>
    *    <td>SUBLIST</td>
    *    <td>The entry is a pointer to a list.</td>
    * </tr>
    * <tr>
    *    <td>DEFINED</td>
    *    <td>The entry is a pointer to a iist and the list has been used.</td>
    * </tr>
    * </table>
    * @return the entry type.
    */
   const SlipHashEntry::Type SlipHashEntry::getType() const {
      return type;
   }; // const Type SlipHashEntry::getType()

   /**
    * @brief Insert the input into the <b>namedList</b> descriptor chain.
    * @param[in] entry (SlipHashEntry*) ptr to a list header
    */
   void SlipHashEntry::insert(SlipHashEntry* entry) {
      if (!entry) return;
      entry->link = descriptorChain;
      descriptorChain = entry;
      DEBUG(inputDebugFlag,
      cout << left  << setw(27) << setfill(' ') << "SlipHashEntry::insert"
           << entry->toString() << endl;)
   }; // void SlipHashEntry::insertLeft(SlipHashEntry* entry)

   /**
    * @brief Return the defined status of the <b>namedLis</b>
    * <p>When the <b>namedList</b> is defined the <b>completeFlag</b> is
    *    set to <b>true</b>. otherwise the flag is <b>false</b>.</p>
    * @return <b>true</b> if the <b>namedList</b> is defined and complete
    */
   bool SlipHashEntry::isComplete() const {
      return completeFlag;
   }; // bool SlipHashEntry::isComplete() const

   /**
    * @brief Copy a list to the current <b>namedList</b>
    * @param[in] ptr (SlipHeader*)
    */
   void SlipHashEntry::putPtr(void* ptr) {
      if (!this->ptr) ptr = new SlipHeader();
      *((SlipHeader*)this->ptr) = *(SlipHeader*)ptr;
   }; // void SlipHashEntry::putPtr(void* ptr)

   /**
   * @brief murmurHash2A hashing algorithm, by Austin Appleby
   * <br />
   * <b>Note</b> - This code makes a few assumptions about how your machine behaves -
   * <ol>
   *    <li>We can read a 4-byte value from any address without crashing</li>
   *    <li>sizeof(int) == 4</li>
   * </ol>
   * <p>And it has a few limitations -</p>
   * <ol>
   *    <li>It will not work incrementally./li>
   *    <li>It will not produce the same results on little-endian and big-endian
   *        machines.</li>
   * </ol>
   *
   * <p>This is a variant of MurmurHash2 modified to use the Merkle-Damgard
   * construction. Bulk speed should be identical to Murmur2, small-key speed
   * will be 10%-20% slower due to the added overhead at the end of the hash.</p>

   * <p>This variant fixes a minor issue where null keys were more likely to
   * collide with each other than expected, and also makes the algorithm
   * more amenable to incremental implementations. All other caveats from
   * MurmurHash2 still apply.</p>
   *
   * @author Austin Appleby
   * @param[in] name (string&) string to be hashed
   * @return hashed input string
   */
   uint32_t SlipHashEntry::murmurHash2A (string& name) {
      char const * const key = name.c_str();
      static uint32_t const seed = 0xDeadBeef;
      int len = name.length();
      #define mmix(hash,k) { k *= m; k ^= k >> r; k *= m; hash *= m; hash ^= k; }
      static const uint32_t m = 0x5bd1e995;
      static const int32_t r = 24;
      uint32_t l = len;

      const unsigned char * data = (const unsigned char *)key;

      uint32_t hash = seed;

      while(len >= 4)
      {
         uint32_t k = *(uint32_t*)data;

         mmix(hash,k);

         data += 4;
         len -= 4;
      }

      uint32_t t = 0;

      switch(len)
      {
      case 3: t ^= data[2] << 16;
      case 2: t ^= data[1] << 8;
      case 1: t ^= data[0];
      };

      mmix(hash,t);
      mmix(hash,l);

      hash ^= hash >> 13;
      hash *= m;
      hash ^= hash >> 15;
      #undef mmix

      DEBUG(hashDebugFlag,
      cout << setw(27) << left << setfill(' ') << "SlipHash::murmurHash2A"
           << (const char*) key << ", "
           << len << "  => " << hash << endl;)

      return (hash == 0xFFFFFFFF)? hash--: hash;
   }; // uint32_t SlipHashEntry::murmurHash2A (string& name)

   /**
    * @brief Implementation of the sdbm hashing algorithm;
    * <p>The algorithm has been altered to give a best performance
    *    for input names structured as "list#", where "#" is a
    *    decimal number. Experimentally when 1000 names are
    *    generated of the form "list1", "list2", ... "list999"
    *    there are 100 collisions using the below formulation.
    *    This result is much better than fnv1a (342 collisions)
    *    and murmurHash2A (359 collisions).</p>
    * @param[in] name (string&) hash table entry.
    * @param[in] size (int) number bytess in input name
    * @return 32-bit hash value
    */
   uint32_t SlipHashEntry::sdbm(char name[], int size) {
      uint32_t hash = 0;
      static const uint32_t shift6  =    36;   // << 6     36/100
      static const uint32_t shift16 = 65511;   // << 16 65511/0
      for(int i = 0; i < size; i++) {
         hash = (uint32_t)(name)[i] + (hash * shift6) + (hash * shift16) - hash;
      }
      DEBUG(hashDebugFlag,
      cout << setw(27) << left  << setfill(' ') << "SlipHash""sdbm(" 
           << setw(10) << right << *name << ") => " << hash << endl;)
      return hash;
   }; // uint32_t SlipHashEntry::sdbm(char name[], int size)

   /**
    * @brief Implementation of the sdbm hashing algorithm;
    * <p>This algorithm is experimental.</p>
    * <p>The algorithm has been altered to give a best performance
    *    for input names structured as "list#", where "#" is a
    *    decimal number. Experimentally when 1000 names are
    *    generated of the form "list1", "list2", ... "list999"
    *    there are 100 collisions using the below formulation.
    *    This result is much better than fnv1a (342 collisions)
    *    and murmurHash2A (359 collisions).</p>
    * @param[in] name (string&) hash table entry.
    * @return 32-bit hash value
    */
   uint32_t SlipHashEntry::sdbm(const string& name) {
      int const size = name.length();
      uint32_t hash = 0;
      static const uint32_t shift6  =    36;   // << 6     36/100
      static const uint32_t shift16 = 65511;   // << 16 65511/0
      for(int i = 0; i < size; i++) {
         hash = (uint32_t)(name)[i] + (hash * shift6) + (hash * shift16) - hash;
      }
      DEBUG(hashDebugFlag,
      cout << setw(27) << left  <<  setfill(' ') << "SlipHash::sdbm"
           << setw(10) << right << name << " => " << hash << endl;)
      return hash;
   }; // uint32_t SlipHashEntry::sdbm(const string& name)

   /**
    * @brief Resolve forward references for Descriptor Lists.
    * <p>Descriptor Lists which are defined by a forward reference to a
    *    <b>namedList</b> are resolved. When the <b>namedList</b> is
    *    defined, the descriptor chain is traversed. Each <i>SlipHashEntry</i>
    *    object on the chain references a list defined as a Descriptor List.
    *    The <b>namedList</b> definition is copied into each Descriptor List.
    *    The <i>SlipHashEntry</i> is marked as complete and resolution of its
    *    forward references are recursively made.</p>
    */
   void SlipHashEntry::resolveDescriptorReferences() {
      DEBUG(inputDebugFlag,
      cout << left << setw(27) << setfill(' ') << "resolveDescriptorReferences" << toString() << endl;)

      if (!descriptorChain) return;

      SlipHeader& from = *(SlipHeader*)ptr;
      SlipHeader& to   = *((SlipHeader*)(descriptorChain->ptr));

      while (descriptorChain) {
         SlipHashEntry* next = descriptorChain->link;
         if (!to.isDList()) to.create_dList();
         SlipHeader& dList = to.getDList();
         dList = from;
         descriptorChain->completeFlag = true;
         descriptorChain->resolveDescriptorReferences();
         descriptorChain = next;
      };
      descriptorChain = NULL;
   }; // void SlipHashEntry::resolveDescriptorReferences()

   /**
    * @brief Descriptor List and sublist forward references to the named list are resolved.
    * <p>When the <b>namedList</b> definition is found, the definition is
    *    saved in the <i>SlipHashEntry</i> object and all Descriptor List
    *    forward references are resolved.</p>
    * <p>If the list definition shows that it has an associated Descriptor
    *    list, then the Descriptor List definition is remembered in the
    *    internal <b>nestedPtr</b> field.</p>
    * @param[in] reg (SlipRegister*) pointer to the hash table
    * @param[in] desc (SlipDescription*) pointer to a list definition
    */
   void SlipHashEntry::resolveForwardReferences(void* reg, SlipDescription* desc) {
      if ((desc->getDesc()) && (desc->getDesc())->getType() == SlipDescription::ANONYMOUS) {
//         nestedPtr = new SlipAsciiEntry((SlipHeader*)(desc->getDesc())->getPtr(), DEFINED, debugFlag);
         nestedPtr = new SlipHashEntryAnonymous((SlipHeader*)(desc->getDesc())->getPtr(), debugFlag);
         nestedPtr->completeFlag = true;
      } else {
         nestedPtr = getEntry(reg, desc);
      }
      if (resolveListReferences())
         resolveDescriptorReferences();
   }; // void SlipHashEntry::resolveForwardReferences(void* header)

   /**
    * @brief Resolve forward references to the named list.
    * <p>An empty list is created when an entry object is created.
    *    All sublist references are to this empty list. When the list
    *    definition is found, the definition list contents are moved into
    *    the empty list and the definition list is made empty. The new
    *    list Descriptor List pointer will point to the definition list
    *    Descriptor List and the definition list Descriptor List pointer
    *    will be made <b><NULL</b>.</p>
    */
   bool SlipHashEntry::resolveListReferences() {
      DEBUG(inputDebugFlag,
      cout << left << setw(27) << setfill(' ') << "resolveListReferences" << toString() << endl;)
      completeFlag = true;
      if (!nestedPtr)  return completeFlag;

      SlipHeader& list = *(SlipHeader*)ptr;

      if (nestedPtr->isComplete()) {
         SlipHeader* from = ((SlipHeader*)(nestedPtr->ptr));
         if (!list.isDList()) list.create_dList();
         list.getDList() = *from;
         DEBUG(inputDebugFlag,
         cout << left << setw(27) << setfill(' ') << "resolveListReferences" << toString() << endl;)
      } else {
         completeFlag = false;
         nestedPtr->insert(this);
      }
      return completeFlag;
   }; // bool SlipHashEntry::resolveListReferences(void* header

   /**
    * @brief Modify the entry type.
    * <p>Ignore the input if it is not a legal type.</p>
    * <p>Set the entry type to the new value.</p>
    * @param[in] type (Type) new type value
    * @return
    */
   const SlipHashEntry::Type  SlipHashEntry::setType(SlipHashEntry::Type type) {
      if ((type >= ILLEGAL) && (type < SIZE)) SlipHashEntry::type = type;
      return type;
   }; // const SlipHashEntry::Type  SlipHashEntry::putType(SlipHashEntry::Type type)

   /**
    * @brief Pretty-print a hash table entry.
    * <p>This method handles both binary and ascii entries. The hash entry
    *    fields are the same for all entries. The interpretation of the
    *    fields are the same and the output is the same.</p>
    * @return pretty-printed string
    */
   string SlipHashEntry::toString() const {
      stringstream pretty;
      if (name) pretty << right << setw(10) << setfill(' ') << *name << ' ';
      else      pretty << "ANONYMOUS  ";
      if (nestedPtr && (nestedPtr->name)) {
         string dName = *(nestedPtr->name);
         pretty << right << setw(10) << dName;
      } else                              pretty << setw(10) << "NULL ";
      if (completeFlag) pretty << " COMPLETE ";
      else              pretty << "          ";
      pretty << toString(type) << " ";
      if ((type != USERDATA) && ptr) pretty << ((SlipHeader*)ptr)->writeToString();
      return pretty.str();
   }; // string SlipHashEntry::toString() const

   /**
    * @brief Return a valid string representing a <b>SlipHashEntry</b> type.
    * @param[in] type (Type) type to be resolved
    */
   string SlipHashEntry::toString(const Type type) {
      static const string stype[] = { "ILLEGAL   "
                                    , "USERDATA  "
                                    , "REFERENCED"
                                    , "DEFINED   "
                                    , "R_AND_D   "
      };
      return ((type >= ILLEGAL) && (type < SIZE))? stype[type]: "NOT LEGAL";
   }; // string SlipHashEntry::toString(const Type type)
}; // namespace slip