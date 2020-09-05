/** 
 * @file    SlipRegister.h
 * @author  A. Schwarz
 * @date    December 31, 2013
 *
 * @brief Register User Data and list data during input/output.
 */

#ifndef SLIPREGISTER_H
#define	SLIPREGISTER_H

# include <string>
# include "SlipAsciiEntry.h"
# include "SlipDatum.h"
# include "SlipDef.h"
# include "SlipHash.h"
# include "SlipHashEntry.h"
# include "SlipHeader.h"
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


using namespace std;

namespace slip {
   /**
    * @class SlipRegister
    * @brief Register User Data parse methods and list headers.
    * <p><b>Overview</b></p><hr>
    * <p>This class registers entries, searches the hash table, and retrieves
    *    data from the hash table. It is the interface between SLIP kernel 
    *    methods and the hash table. It contains a single hash table object.</p>
    * <p>For Input or Output, a single <i>SlipRegister</i> object is
    *    created. This object contains a SLIP hash table and interfaces
    *    with this table.</p>
    * <p>There is a different sequence of events for Input and Output. For
    *    Output the <i>SlipHeader</i> write methods create a <i>SlipRegister</i>
    *    object and use this object to store the addresses of sublists and
    *    User Data class names. For Input the <i>SlipRead</i> object creates
    *    a <i>SlipRegister</i> object and stores application supplied User
    *    Data class names and input parsing functions, then the parser uses
    *    the <i>SlipRegister</i> object to register, search, and retrieve
    *    list information and User Data information. the difference is
    *    that for Output the application is unconcerned with User Data object 
    *    methods but for Input the application must supply User Data class 
    *    information (via a <i>SlipRead</i> object).</p>
    * <p>Registering data, using the class <b>post</b> methods, consists of
    *    inserting a tuple (<i>SlipHashEntry</i>) to the hash table 
    *    (<i>SlipHash</i>). The tuple is a key value pair containing an
    *    ASCII string and an associated pointer, either to a list header
    *    or to a User Data class parsing method, &lt;ASCII name, address&gt;.</p>
    * <p>On Output, User Data class ASCII names and list addresses 
    *    (<i>SlipHeader</i> object addresses) are used as the key in searches.
    *    On input, User Data class ASCII names and ASCII names of lists are
    *    used as keys. Class methods determine which key to use and what data
    *    to retrieve.</p>
    * <p>This can be seen in the following table:</p>
    * <br /><center>
    * <table>
    * <caption>&lt;key, value&gt; pairs</caption>
    *    <tr>
    *       <th><b>Input/Output</b></th>
    *       <th colspan="2"><center><b>User Data class</b></center></th>
    *       <th colspan="2"><center><b>List object</b></center></th>
    *    </tr>
    *    <tr>
    *       <th></th>
    *       <th><b>key</b></th>
    *       <th><b>value</b></th>
    *       <th><b>key</b></th>
    *       <th><b>value</b></th>
    *    </tr>
    *    <tr>
    *       <th><b>Input</b></th>
    *       <td>ASCII name</td>
    *       <td>User Data class parser</td>
    *       <td>ASCII name</td>
    *       <td><i>SlipHeader</i> address</td>
    *    </tr>
    *    <tr>
    *       <th><b>Output</b></th>
    *       <td>ASCII name</td>
    *       <td>ignored</td>
    *       <td><i>SlipHeader</i> address</td>
    *       <td>ASCII name</td>
    *    </tr>
    * </table></center>
    * <br>
    * <b>LEGEND</b>
    * <table border="0">
    *  <tr>
    *     <td>Input</td>
    *     <td>The application is reading an input list file.</td>
    *  </tr>
    *  <tr>
    *     <td>Output</td>
    *     <td>The application is outputting an input list file.</td>
    *  </tr>
    *  <tr>
    *     <td>ASCII name</td>
    *     <td>A string containing the name of an object.</td>
    *  </tr>
    *  <tr>
    *     <td>User Data class parser</td>
    *     <td>A Pointer to a User Data method which creates a <i>SlipDatum</i> object from a list.</td>
    *  </tr>
    *  <tr>
    *     <td><i>SlipHeader</i> address</td>
    *     <td>A pointer to a <i>SlipHeader</i> object.</td>
    *  </tr>
    * </table>
    * @see <i>SlipHashEntry</i>
    * @see <i>SlipAsciiEntry</i>
    * @see <i>SlipBinaryEntry</i>
    * <p>All ASCII names must conform to the following formatting rules:</p>
    * <ul>
    *    <li>The leading character must the an upper or lower case character or an
    *         underscore ('_') or dollar sign ('$').</li>
    *    <li>The remainder of the name must be an upper or lower case character,
    *        a number (0- 9), or an underscore or dollar sign.</li>
    *    <li>Embedded blanks are not allowed.</li>
    *    <li>Single character names are legal.</li>
    * </ul>
    * <p>The regular expression representation is: [a-zA-Z$_][a-zA-Z0-9$_]*.</p>
    * <p><b>List Output Processing</b></p>
    * <p>During output, shared lists are arbitrarily assigned a unique name. This
    *    name, its associated list, are output into the output file. The list is
    *    called a 'named list' and is referenced by '{name}'. Not all lists are
    *    named.</p>
    * <p>User Data class names are output without analysis. If the name is not
    *    properly constructed then on input the User Data class name will not be
    *    recognized and the User Data will not be parsed.</p>
    * <p>The User Data class name and the pointer to <i>SlipHeader</i> object
    *    addresses are the key in any hash table search or post (registration).</p>
    * <p>An object of this class is used to mediate all hash function operations.</p>
    * @see <i>SlipWrite</i>
    * <p><b>List Input Processing</b></p>
    * <p>Before processing of an input list file, the application must register
    *    all User Data classes used. This registration will store the User Data
    *    class name and the associated input parsing function in the hash table.
    *    After this is done, the application can begin parsing. The parser will
    *    register named lists, resolve forward references to these lists, and
    *    create Descriptor Lists associated with <i>SlipHeader</i> objects.
    *    Names which are not legally constructed will not be recognized and may
    *    cause a syntax or semantic error.</p>
    * <p>An object of this class is used to mediate all hash function operations.</p>
    * <p><b>Constructors and Destructors</b></p><hr>
    * <ul>
    *    <li><b>SlipRegister()</b> Create a hash table and give it an initial 
    *        allocation.</li>
    *    <li><b>~SlipRegister()</b> Delete all entries in the hash table and then
    *        delete the hash table.</li>
    *    <li><b>SlipRegister(const SlipHash&)</b> Copy constructor is not 
    *        supported.</li>
    * </ul>
    * <p><b>Operators</b></p><hr>
    * <ul> 
    *   <li><b>operator[]()</b> Return the <i>SlipHashEntry</i> at the indexed
    *       position.</li>
    * </ul>
    * <p><b>Methods</b></p><hr>
    * <p>The methods have full knowledge of the execution time environment they
    *    work in. this has the effect that there is a incestuous relationship
    *    between parsing, during input, output list processing and the 
    *    <i>SlipRegister</i> class. This has the effect that changes made to
    *    input or output processing must be sensitive to SlipRegistor
    *    functioning, and at the same time, SlipRegister functioning is
    *    aware of processing requirements in input parsing and output list 
    *    generation.</p>
    * <ul>
    *    <li><b>copyDList</b> Copy a named list to the Descriptor List field
    *        of a list.</li>
    *    <li><b>getParse()</b>Return a pointer to the parsing method for the 
    *       input name. If the name can not be found in the hash table or
    *       if the name is not a user data type, return <b>NULL</b>.</li>
    *    <li><b>getSublist()</b>Return a pointer to the list head <i>SlipHeader</i>
    *       for the given input name. If the name does not refer to a list head or
    *       the name is not found, return <b>NULL</b></li>
    *    <li><b>post()</b> Post a name and associated data into the hash table. 
    *       <b>true</b> is returned if the name and data are posted. 
    *       <b>false</b> is returned if this is double post (the name is already
    *       in the hash table) or the table was full and an attempt to allocate 
    *       more space failed.</li>
    *   <li><b>setDebugON()</b> Output debug information to cout.</li>
    *   <li><b>setDebugOFF()</b> Terminate debug output</li>
    *    <li><b>size()</b> Return the number of entries in the hash table.
    *       This is not the size of the hash table. It is the number of 
    *       positions in the hash table which are not <b>null</b>.</li>
    *    <li><b>toString()</b> Format and return all hash table entries.</li>
    *    <li><b>toStringFull()</b> Format and return the full hash table (including
    *       empty entries).</li>
    * </ul>
    * <p><b>EXAMPLE</b></p><hr>
    * <p>Let's look at a simple example:</p>
    * <tt><pre>
    * Generation of the list:
    *   SlipHeader* list  = new SlipHeader();
    *   SlipHeader* list1 = new SlipHeader();
    *   SlipHeader* list2 = new SlipHeader();
    *   
    *   list1->enqueue((LONG) 1).enqueue((LONG) 2);
    *   list2->enqueue((Long0 2).enqueue((LONG) 3);
    *   
    *   SlipDatum* user = new SlipDatum(new myData); // with cclass name "COORD"
    *   
    *   list->enqueue((UCHAR)1.enqueue((bool)true).enqueue(*list1)
    *        .enqueue((string)"b").enqueue(*list2).enqueue(*list1)
    *        .enqueue(*user);
    *
    *   list.writeQuick();   // yields
    *   
    *   
    *   ( 1UC true (1 2) "b" (2 3) (1 2) COORD(10., 11.))
    *   |          |         |     |     |
    *   |          |         |     |     o - User Data object named 'COORD'
    *   |          |         |     |
    *   |          |         |     o - sublist named 'list1'
    *   |          |         |
    *   |          |         o - sublist named 'list2'
    *   |          |
    *   |          o - sublist named 'list1'
    *   |
    *   o - list to be output
    * 
    * where 1UC  is an unsigned character SlipDatum object
    *       true is a boolean SlipDatum object
    *       "b"  is a string  SlipDatum object
    *       1, 2, 3 are SlipDatum objects
    *       User\ Data 'user' stores rectangular coordinated of the form <x, y>
    * 
    * This becomes the list file:
    *    
    *   user COORD;
    *   sublist list1 (1 2);
    *   sublist list2 (2 3);
    * 
    *   ( 1UL true {list1} "b" {list2} {list1} COORD(10., 11.) ) 
    *   |          |           |       |       |    |
    *   |          |           |       |       |    o - User Data generated list
    *   |          |           |       |       |
    *   |          |           |       |       o - User Data name
    *   |          |           |       |
    *   |          |           |       o - sublist named 'list1'
    *   |          |           |
    *   |          |           o - sublist named 'list2'
    *   |          |
    *   |          o - sublist named 'list1'
    *   |
    *   o - list
    * 
    * </pre></tt>
    * <p>During output the tuple &lt;address, "list1"&gt; and &lt;address, "list2"&gt;
    *    are posted into the hash table using a <i>SlipRegister</i> object, where the address
    *    is used as the primary key. On input a <i>SlipRegister</i> object is used to store
    *    the same tuples, but the ASCII list name is used as the primary key.</p>
    * <p>User data is handled differently. On output the class name is stored in
    *    the hash table using the a <i>SlipRegister</i> object and any associated binary 
    *    value is ignored. Input is a two step operation. The application uses the 
    *    <i>SlipRegister</i> object to registers each instance of a User Data class
    *    and then the parser is called.</p>
    * <p>Registration stores the tuple &lt;parse address, "class name"&gt; into the 
    *    hash table, where the "class name" is used as the primary key. Parsing
    *    retrieved the address of the application supplied user data parsing method
    *    to parse the user data.
    */
    class SlipRegister {
      SlipHash* hashTable;                                      //!< pointer to the hash class
      int  debugFlag;                                           //!< dombined debug flags
      bool hashDebugFlag;                                       //!< hash operation debug flag
      bool inputDebugFlag;                                      //!< debug input flag
    public:
    private:
      SlipRegister(const SlipRegister& X);
      bool           fsm(SlipHashEntry& entry, SlipHashEntry::Type type) const;
      SlipHash::ReturnTuple insert(const string& name);
    public:
      SlipRegister(int debugFrlag = 0);
      virtual ~SlipRegister();
      SlipHashEntry* operator[](int ndx) const;
      bool           copyDList(SlipDescription* from, SlipDescription* to);
      void           deleteEntry(const SlipHash::ReturnTuple& tuple) { hashTable->deleteEntry(tuple); }
      Parse          getParse(const string& name) const;
      SlipHeader*    getSublist(const string& name) const;
      string         getSublist(SlipHeader& head) const;
      bool           post(const SlipDatum& userData);
      bool           post(const string& name);
      bool           post(SlipDescription* definition);
      bool           post(SlipHeader& head, string& name);
      void           outputHashTable() { hashTable->outputHashTable(); }
      SlipHash::ReturnTuple search(const string& name) const;
      bool           search(SlipHeader& head) const;
      void           setDebugOFF(int debugFlag = 0);
      void           setDebugON(int debugFlag= 0);
      int            size();
      void           write() const;
      void           writeFull() const;
    }; // class SlipRegister
} // namespace slip


#endif	/* SLIPREGISTER_H */

