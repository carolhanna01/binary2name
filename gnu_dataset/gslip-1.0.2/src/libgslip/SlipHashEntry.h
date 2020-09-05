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
 * @file    SlipHashEntry.h
 * @author  A. Schwarz
 * @date    December 30, 2013
 *
 * @brief Base class for Hash entry items.
 */

#ifndef SLIPHASHENTRY_H
#define	SLIPHASHENTRY_H

# include <string>
# include "SlipDescription.h"

using namespace std;

namespace slip {

   /**
    * @class SlipHashEntry
    * @brief Hash Entry management class.
    * <p><b>Overview</b></p><hr>
    * <p>A <i>SlipHashEntry</i> is the object stored in the hash table,
    *    <i>SlipHash.</i> This class supports the creation and deletion 
    *    of hash table entries and maintenance of the descriptor list 
    *    forward reference chain. An object of this class is used to 
    *    perform hash table searches. It is a container of data and an
    *    interface to objects external to the hash table.</p>
    * <p>During output operations, <i>SlipHashEntry</i> objects are 
    *    containers of information, they passively support the storage 
    *    and retrieval of data.</p>
    * <table border="0">
    *    <tr>
    *       <th><b>key</b></th>
    *       <td>The key is the search value used to search the hash table
    *           (<i>SlipHash</i>) and can be a pointer to a <i>SlipHeader</i> 
    *           object, or an ASCII name. In output the key is either the name
    *           of a User Data class or a pointer to a <i>SlipHeader</i>
    *           object. In input the key is the name of a User Data
    *           class or the name of a named list.</td>
    *    </tr>
    *    <tr>
    *       <th><b>value</b></th>
    *       <td>The value associated with the search key is either a pointer
    *           to a list header (<i>SlipHeader</i>) or a pointer to a User
    *           Data write or parse method. During output the value is either
    *           a <i>SlipHeader</i> name or User Data write method pointer. During
    *           input the value is either a <i>SlipHeader</i> ointer or a pointer
    *           to a User Data parse method.</td>
    *    </tr>
    *    <tr>
    *       <th><b>descriptorChain</b></th>
    *       <td>Pointer to a list of <i>i>SlipHashEntry</i> objects. Each 
    *           member of the list is a named list participating in a 
    *           nested Descriptor List chain.</td>
    *    </tr>
    *    <tr>
    *       <th><b>primary hash value</b></th>
    *       <td>The binary or Ascii hash value used as the primary search
    *           index.</td>
    *    </tr>
    *    <tr>
    *       <th><b>secondary hash value</b></th>
    *       <td>The binary or Ascii hash value used as the secondary search
    *           index. When the primary search is unsuccessful the secondary
    *           key is used for closed form searching.</td>
    *    </tr>
    * </table>
    * <p>Derived classes handle issues of differences between binary and 
    *    Ascii keys.</p>
    * <p><b>Sublist Reference Processing</b></p>
    * <p><b>namedList</b> forward reference make use of a <i>SlipSublist</i>
    *    to reuse a list. Each sublist using the same list has a sublist
    *    object whose contents point to this list. The first forward reference
    *    establishes an empty list. Each use of a <b>namedList</b> use uses
    *    the same list. When the <b>namedList</b> is defined, the empty list
    *    is replaced by the definition. Since all created references point to
    *    the same list, changing the list value is 'felt' by all references.</p>
    * <p><b>EXAMPLE<sup>1</sup></b></p>
    * <tt><pre>
    *  Input List File:
    *    list1 ( {list2} {list2} )
    *    list2 ( 1 2 3 )
    *    {list1}
    * 
    *  Before list 2 is evaluated the internal representation is:
    *     <i>SlipHeader</i> &lt;--&gt; o &lt;--------&gt; o 
    *                     |            |
    *                     . --- . ---- v
    *                           |
    *                           v
    *                       <i>SlipHeader</i> 
    * 
    * where 'o' stands for a unique <i>SlipSublist</i> object
    * </pre></tt>
    * <p>Before the definition of list2, the <i>SliSublist</i> object point
    *    to a common, empty list. When list2 is defined, the definition of
    *    list2 replaces the empty list, the <i>SlipHeaer</i> object stays
    *    the same.</p>
    * <p><b>Descriptor Reference Chain</b></p>
    * <p>The Descriptor Chain is a linked list of <i>SlipHashEntry</i> objects. 
    *    This type of list is most unnatural and is only used here. Each 
    *    <i>SlipHashEntry</i> on the chain represents a <b>namedList</b>.
    *    The total of all the entries on the chain become a description of
    *    all <b>namedLists</b> in a nested Descriptor List.</p>
    * <p><b>EXAMPLE<sup>2</sup></b></p>
    * <p>A Descriptor List containing a reference to a single <b>namedList</b>/p>
    * <tt><pre>
    *  Input List File:
    *    list1 ( &lt; {list2} &gt; 1 2 )
    *    list2 ( 3 4 )
    *    {list1}
    * </pre></tt>
    * <p>Which is interpreted as <b>list1</b> containing the Descriptor
    *    list <b>list2</b>, and <b>list1</b> being the list returned to
    *    the application. The desired list, after being read,  will look 
    *    like:</p>
    * <tt><pre>
    *   ( &lt; 3 4 &gt; 1 2 )
    * </pre></tt>
    * <p><b>list2</b> is a forward reference in <b>list1</b>. The desired 
    *    result returned to the application is:<p>
    * <tt><pre>
    *     <i>list1</i> &lt;--&gt; 1 &lt;--&gt; 2
    *        |
    *        |
    *        v
    *   <i>list2</i> &lt;--&gt; 3 &lt;--&gt; 4
    * </pre></tt>
    * <p>Internally the <b>list2</b> header in the Descriptor List is maintained
    *    separately from the any reference as a sublist. When list2 is defined,
    *    the definition is copied to each reference in each Descriptor List. As
    *    a sublist, the definition is copied once into the sublist archetype.</p>
    * <p>Processing for this list is:</p>
    * <ol>
    *    <li>When the list2 reference is seen a <i>SlipHashEntry</i> object
    *        is put into the hash table. The object has an empty list and
    *        the descriptor chain is <b>null</b>.</li>
    *    <li>When the list1 definition is seen:</li>
    *    <ul>
    *       <li>A <i>SlipHashEntry</i> object is created and placed in the hash
    *           table.</li>
    *       <li>The object is populated with the list1 list (1 2) with</li>
    *       <li>A Descriptor List <i>SlipHeader</i> object to an empty list.</li>
    *       <li>A <i>SlipHashEntry</i> object is created and placed in the hash
    *           table</li>
    *       <ul>
    *          <li>A pointer to the list1 Descriptor List list header is
    *              inserted into the list2 Descriptor List chain.</li>
    *       </ul>
    *    </ul>
    *    <li>When the list2 definition is seen:</li>
    *    <ul>
    *       <li>The list definition (3 4) is copied into the empty list
    *           representing the forward definition for list2.</li>
    *       <li>The list definition is copied into the list1 Descriptor
    *           List header.</li>
    *    </ul>
    * </ol>
    * <p>At the end of step 2, list1 is completely defined. It contains a list
    *    definition, as described in list1 processing, and a Descriptor List
    *    with the appropriate values. List2 is completely defined. The list2
    *    definition is not used in this example.</p>
    * <p>Processing by the steps we get the following figures:</p>
    * <b>Step 2</b>
    * <tt><pre>
    *    SlipHashEntry<sub>1</sub> --- list1 &lt;---&gt;  1 &lt;---&gt;  2
    *                         |
    *                         | --- Descriptor List for list1
    *                         |
    *                         v
    *                       list
    *                         ^
    *                         |
    *                         | --- list2 Descriptor Chain
    *                              |
    *                       SlipHashEntry<sub>2</sub> --- list2 ---
    * </pre></tt>
    * 
    * <b>Step 3</b>
    * <tt><pre>
    *    SlipHashEntry<sub>1</sub> --- list1 &lt;---&gt;  1 &lt;---&gt;  2
    *                         |
    *                         | --- Descriptor List for list1
    *                         |
    *                         v
    *                        list &lt;---&gt;  3 &lt;---&gt;  4
    *                         ^
    *                         |
    *                         | --- lis2 Descriptor Chain
    *                         |
    *                       SlipHashEntry<sub>2</sub> --- list2 &lt;---&gt;  3 &lt;---&gt;  4
    * </pre></tt>
    * <p><b>EXAMPLE<sup>3</sup></b></p>
    * <p>A list containing two references to  a <b>namedList</b>.</p>
    * <tt><pre>
    *  Input List File:
    *    list1 ( {list2} ( &lt; list2 &gt; 5 6) 1 2 )
    *    list2 ( 3 4 )
    *    {list1}
    * </pre></tt>
    * <p>Where <b>list1</b> has a sublist reference to <b>list2</b>, an
    *    anonymous list and two <i>SlipDatum</i> objects at the list top. 
    *    The anonymous list has a Descriptor List referenced to <b>list2</b>
    *    and two <i>SlipDatum</i> objects. Note that the references to 
    *    <b>list2</b> are expanded differently and expansions are not shared.
    *    All sublists referencing <b>list2</b> will reference the same 
    *    expanded list. Each Descriptor List referencing <b>list2</b> will
    *    have and independent copy of <b>list2</b>. Any change to the sublist
    *    will have an immediate impact on all other sublists but not impact
    *    on any Descriptor List. Any change to a Descriptor List will have
    *    no impact on any other Descriptor Lists and no impact on any sublist.
    *    The two uses are disjoint.</p>
    * <p>Which will look like:</p>
    * <tt><pre>
    *    ( (3 4) ( &lt; 3 4 &gt; 5 6) 1 2 )
    * </pre></tt>
    * <p>The desired result returned to the application is:<p>
    * <tt><pre>
    *     <i>list1</i> &lt;---&gt; o &lt;---&gt; o &lt;---&gt; 1 &lt;---&gt; 2
    *                 |       |
    *                 |       . &lt;---&gt; 5 &lt;---&gt; 6
    *                 |       |
    *                 |       v
    *                 |     <i>list2</i> &lt;---&gt; 3 &lt;---&gt; 4
    *                 v     
    *             <i>list2</i> &lt;---&gt; 3 &lt;---&gt; 4
    * </pre></tt>
    * <p>And we can see that <b>list2</b> has two independent representations.</p>
    * <p>Processing for this list is:</p>
    * <ol>
    *    <li>The first list2 is a sublist reference. A <i>SlipHashEntry</i> object 
    *        is created with an empty list and a <b>null</b> descriptor
    *        chain.</li>
    *    <li><The second list2 is a Descriptor List, part of an onymous list..</li>
    *    <ul> 
    *       <li>An anonymous <i>SlipHashEntry</i> object will be created for
    *           the anonymsous list.</li>
    *       <li>An empty <i>SlipHeader</i> object is created and attached to 
    *           the anonymous list.</li>
    *       <li>The newly created Descriptor List header is assigned as part
    *           of the list2 descriptor chain.</li>
    *    </ul>
    *    <li>When the list1 definition is seen:</li>
    *    <ul>
    *       <li>A <i>SlipHashEntry</i> object is created and placed in
    *           the hash table.</li>
    *       <li>The entry is populated with all parts of the list. The
    *           list2 sublist and the list2 Descriptor List references
    *           are forward references.</li>
    *       <li>The list2 sublist forward reference has a <i>SlipSublist</i>
    *           object in the list referencing the list2 list (currently
    *           an empty list).</li>
    *       <li>The list2 Descriptor List reference in the anonymous list
    *           has the Descriptor List header referenced as part of the 
    *           list2 descriptor chain.</li>
    *    </ul>
    *    <li>When the list2 definition is seen:</li>
    *    <ul>
    *       <li>The definition details for list2 are moved into the list2
    *           owned <i>SlipHeader</i>o object.</li>
    *       <li>The list2 sublist is now defined. The list1 reference is now
    *           valid.</li>
    *       <li>The list2 definition is copied into the Descriptor List 
    *           reference in the list2 descriptor chain. AFter the copy,
    *           the anonymous list Descriptor List will have the correct 
    *           value and this value will be independent of any any other
    *           uses of list2.</li>
    *       <li></li>
    *    </ul>
    * </ol>
    * <b>Step 3</b>
    * <tt><pre>
    *    SlipHashEntry<sub>1</sub> &lt;----&gt; list1 &lt;----&gt; o &lt;------------&gt; o &lt;----&gt; 1 &lt;----&gt; 2
    *                                       |                 |
    *                                       |                 |
    *                                       |                 |
    *                                       v                 .  &lt;----&gt; 5 &lt;----&gt; 6
    *                                     list2  ---.         |
    *                                       |       |         |
    *                                       |       |         |
    *                                       |   descriptor    |
    *                                       |     chain       |
    *                                       |       |         |
    *                                       |       v         v
    *                                       |       . - SlipHashEntry<sub>3</sub>
    *                                       | 
    *                                       v
    *                                 SlipHashEntry<sub>2</sub>
    * </pre></tt>
    * 
    * <b>Step 4</b>
    * <tt><pre>
    *    SlipHashEntry<sub>1</sub> &lt;----&gt; list1 &lt;----&gt; o &lt;------------&gt; o &lt;----&gt; 1 &lt;----&gt; 2
    *                                       |                 |
    *                                       |                 |
    *                                       |                 |
    *                                       v                 .  &lt;----&gt; 5 &lt;----&gt; 6
    *                                     list2  ---.         |
    *                                       |       |         |
    *                                       |       |         |
    *                                       |   descriptor    |
    *                                       |     chain       |
    *                                       |                 |
    *                                       |                 v
    *                                       |           SlipHashEntry<sub>3</sub>  &lt;----&gt; 3 &lt;----&gt; 4
    *                                       | 
    *                                       v
    *                                 SlipHashEntry<sub></sub> &lt;----&gt; 3 &lt;----&gt; 4
    * </pre></tt>
    * 
    * <p>In summary. Declaration Lists (on input) are recursive  
    *    structures. Each Declaration List may contain a Declaration List, 
    *    and each Declaration List may be defined with respect to a 
    *    <b>namedList</b>. If the <b>namedList</b> is resolved at the time
    *    of the current <b>namedList</b> definition, then nothing need be done.
    *    If the <b>namedList</b> is not yet defined, then the Declaration  
    *    List of all forward references in the chain must be propagated to the 
    *    <b>namedList</b> and the current list must be removed. Access
    *    to the <i>SlipHashEntry</i> container for the <b>named list</b> can
    *    only be made using the primary key (name) of the list.</p>
    * @note Recursive Declaration Lists are not supported in the runtime 
    *       software. Although they can be created on input, they can not
    *       be used nor created during normal operation.
    * <p><b>Constructors and Destructors</b></p><hr>
    * <ul>
    *  <li><b>SlipHashEntry</b> Create an entry.</li>
    *  <li><b>~SlipHashEntry</b> Destroys an entry. </li>
    * </ul>
    * <p><b>Operators</b></p><hr>
    * <ul>
    *    <li><b>operator!=()</b> Compare two entries.</li>
    *    <li><b>operator&lt;()</b> Compare two entries.</li>
    *    <li><b>operator&lt;=()</b> Compare two entries.</li>
    *    <li><b>operator==()</b> Compare two entries.</li>
    *    <li><b>operator&gt;=()</b> Compare two entries.</li>
    *    <li><b>operator&gt;()</b> Compare two entries.</li>
    * </ul>
    * <p><b>Methods</b></p><hr>
    * <ul>
    *    <li><b>createHashEntry</b> Create a new hash entry using an input
    *        hash entry as a template.</li>
    *    <li<b>deleteList</b> delete the entry list if it exists. This is
    *        an asymmetric method which acts differently for <i>SlipAsciiEntry</i> 
    *        and <i>SlipAsciiEntry.</i> For <i>SlipBinaryEntry</i> any associated
    *        list is never deleted. For <i>SlipAsciiEntry</i> any associated list
    *        is always deleted.</li>
    *    <li><b>fnv1a</b> Hash function used for the primary hash. The
    *        primary hash is the first determiner (the decider) that 
    *        there is an empty hash table slot or that the existing
    *        entry does not satisfy a search.</li>
    *    <li><b>getAsciiKey</b> Return the ascii (string) key used in 
    *        the entry. If there is no ascii key, return an empty
    *        string.</li>
    *    <li><b>getAsciiValue</b> Return the ascii (string) value used 
    *        in an entry. If there is no ascii key, return an empty
    *        string.</li>
    *    <li><b>getBinaryKey</b> Return the binary key used in the entry. 
    *        If there is no binary key return <b>null</b></li>
    *    <li><b>getBinaryValue</b> Return the binary value used in an 
    *        entry. If there is no binary value return <b>null</b>.</li>
    *    <li><b>getType</b> Return the entry type.</li>
    *    <li><b>isDefined</b> Return true if the named list has been defined.</li>
    *    <li><b>insertLeft</b> Insert the input <i>SlipHeader</i> to the left of
    *        the current <i>DescriptorChain</i> list.</li>
    *    <li><b>putType</b> Change the entry type.</li>
    *    <li><b>getPrimaryHash</b> Return the value of the primary hash.
    *        The hash value is created on entry construction.</li>
    *    <li><b>getSecondaryHash</b> Return the value of the secondary
    *        hash base. The hash base is created on entry construction.</li>
    *    <li><b>murmurHash2A</b> Secondary hash base. If the primary hash
    *        is not successful in finding an empty slot or a search is
    *        not successful, then a secondary (quadratic) search is 
    *        started using the base returned by this function..</li>
    *    <li><b>primaryHashFunction</b> Prepare the input for use in
    *        the fnv1a hash algorithm and return the result of execution 
    *        of fnv1a.</li>
    *    <li><b>rename</b> Allows the value in a sublist &lt;key value&gt; pair
    *        to be renamed. </li>
    *    <li><b>resolveDescriptorReferences</b> Resolve forward references for 
    *        Descriptor Lists.</li>
    *    <li><b>resolveForwardReferences></b> After a <b>named list</b> 
    *        definition has been found, resolve all forward references.</li>
    *    <li><b>resolveListReferences</b> Resolve forward references to
    *        a sublist.</li>
    *    <li><b>sdbm</b> Experimental hash algorithm. Not used in production.</li>
    *    <li><b>secondaryHashFunction</b> Prepare the input for use in
    *        the murnurHash2A hash algorithm and return the result of 
    *        execution of murnurHash2A.</li>
    *    <li><b>setKey</b> Change or set the primary key in an entry. If the
    *        entry is <i>SlipAsciiEntry</i> then the key is <i>const string*</i>
    *        and represents the name. If the entry is <i>SlipBinaryEntry</i>
    *        then the key is <i>const void*</i> and represents <b>ptr</b>, a
    *        pointer to a header.</li>
    *    <li><b>toString()</b> Pretty-print an entry (return the name).</li>
    * </ul>
    */
   class SlipHashEntry {
   private:
   public:
      /**
       * @brief Type strings for enum Type.
       */
       const static string typeString[];
      
      /**
       * @brief Hash Table Entry types
       * <p>Each hash table entry has an associated type which identifies
       *    it's usage. The legal types are:</p>
       * <ul>
       *    <li><b>ILLEGAL</b> Illegal entry type.</li>
       *    <li><b>USERDATA</b> The entry refers to a User Data type. The
       *      value stored into the data field for the entry is a pointer
       *      to the User Data parse function.</li>
       *    <li><b>REFERENCED</b> A named list has been referenced and registered
       *        but no definition has been created. The list reference is a 
       *        forward reference. If no definition has been found when parsing 
       *        is complete then this is an error and the list will be empty.</li>
       *    <li><b>DEFINED</b> A list name has been registered and defined by a 
       *        list. The list has not been referenced at the time of 
       *        association. If no reference was found when parsing is complete
       *        then this is an error.</li>
       *    <li><b>R_AND_D</b> A list has been registered, DEFINED and REFERENCED.
       *        This is the only valid state for a named list at parse completion.</li>
       * </ul>
       * @note Registered lists can only be deleted at parse end. 
       */
      enum Type { ILLEGAL    = 0                 //!< illegal value
                , USERDATA                       //!< User Data entry
                                                 /**********************************/
                                                 /*      List specific types       */
                                                 /**********************************/
                , REFERENCED                     //!< List referenced but not defined
                , DEFINED                        //!< List defined
                , R_AND_D                        //!< List Referenced and Defined
                , SIZE                           //!< Sentinel (# of enums))
      };
   protected:
      SlipHashEntry* link;                       //!< link to next item on descriptorChan
      SlipHashEntry* descriptorChain;            //!< pointer to Description List chain
      SlipHashEntry* nestedPtr;                  //!< pointer to Description List hash table entry
      void *         ptr;                        //!< pointer to parser method or SlipHeader
      const string*  name;                       //!< name of item
      uint32_t       primaryHash;                //!< value of primary hash
      uint32_t       secondaryHash;              //!< value of secondary hash
      const int      debugFlag;                  //!< debug flags
      const bool     hashDebugFlag;              //!< hash debug flag
      const bool     inputDebugFlag;             //!< input debug flag
      const bool     leakDebugFlag;              //!< memory leak flag
      bool           completeFlag;               //!< defined and Descriptor List valid
      enum Type      type;                       //!< type of entry 
   private:
      virtual void resolveDescriptorReferences();
      bool     resolveListReferences();
   protected:
      uint32_t fnv1a(const string& name);
      uint32_t fnv1a(char name[], int size);
      uint32_t murmurHash2A(string& name);
      uint32_t sdbm(const string& name);
      uint32_t sdbm(char name[], int size);
      virtual uint32_t primaryHashFunction() = 0;
      virtual uint32_t secondaryHashFuncion() = 0;
      void        insert(SlipHashEntry* entry);
      SlipHashEntry* getNext();
      SlipHashEntry(void* ptr, Type type, int debugFlag = 0);
      SlipHashEntry(const string& name, void* ptr, Type type, int debugFlag = 0);
      SlipHashEntry(SlipDescription* desc, Type type, int debugFlag = 0);
   public:
      virtual ~SlipHashEntry();
      
      virtual bool  operator!=(const SlipHashEntry& X) const = 0;
      virtual bool  operator<(const SlipHashEntry& X) const = 0;
      virtual bool  operator<=(const SlipHashEntry& X) const = 0;
      virtual bool  operator==(const SlipHashEntry& X) const = 0;
      virtual bool  operator>=(const SlipHashEntry& X) const = 0;
      virtual bool  operator>(const SlipHashEntry& X) const = 0;
      
      bool                       copyDList(SlipDescription* to);
      bool                       copyDList(SlipHashEntry* to);
      virtual SlipHashEntry*     createHashEntry() const = 0;
      virtual void               deleteList() = 0;
      virtual const string&      getAsciiKey() const = 0;
      virtual const string&      getAsciiValue() const = 0;
      virtual const void * const getBinaryValue() const = 0;
      virtual const void * const getBinaryKey() const = 0;
      SlipHashEntry*             getEntry(void* reg, SlipDescription* desc);
      uint32_t                   getPrimaryHash() const;
      uint32_t                   getSecondaryHash() const;
      const Type                 getType()  const;
      bool                       isComplete() const;
      void                       putPtr(void* ptr);
      virtual bool               rename(const string& name) = 0;
      void                       resolveForwardReferences(void* reg, SlipDescription* desc);
      virtual void               setKey(const void* key) = 0;
      virtual void               setHash() = 0;
      virtual const Type         setType(Type type);
      virtual string             toString() const;
      static  string             toString(const Type type);
   }; // class SlipHashEntry

}; // namespace slip
#endif	/* SLIPHASHENTRY_H */

