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
 * @file    SlipRead.h
 * @author  A. Schwarz
 * @date    January 13, 2014, 2012
 * 
 * @brief Read a list file.
 *
 */

#ifndef SLIPREAD_H
#define	SLIPREAD_H

# include "SlipDatum.h"
# include "SlipHeader.h"
# include "SlipRegister.h"

namespace slip {
   
   /**
    * @class SlipRead
    * @brief Read and parse a list file.
    * <p><b>Overview</b></p>
    * <p>This class initialize the read environment and provides the mechanisms
    *    for the user to input required information before starting a read. It 
    *    forms the bridge between the user application and list input. </p>
    * <p>The result of a read is a list. The input operations complete withou
    *    exception. Error and warning messages are output. On any failure, an
    *    empty list is returned, this can be distinguished from an input list
    *    file which contains an empty list by accessing the internal error
    *    flag. A value of '0' is succdss, '1' is a failure in parsing, and 
    *    '2' is a failure in opening the input list file.</p>
    * <p><b>Constructors and Destructors</b></p>
    * <ul>
    *    <li><b>SlipRead()</b> Constructs an input list file reading environment.</li>
    *    <li><b>~SlipRead()</b> Destroys everything. Just everything.</li>
    *    <li><b>SlipReader(const)</b> A copy constructor is not supported.</li>
    * </ul>
    * <p><b>Methods</b></p>
    * <ul>
    *    <li><b>getError()</b> Return the current error value.</li>
    *    <ol start="0">
    *       <li>No error.</li>
    *       <li>One or more errors detected during parsing the input list file.</li>
    *       <li>Unable to open the input list file.</li>
    *    </ol>
    *    <li><b>read()</b> Read the input list file.</li>
    *    <li><b>registerUserData()</b> Register User Data parse functions.</li>
    * </ul>
    * <p><b>Examples</b></p>
    * <p>We assume throughout that:</p>
    * <tt><pre>string filename("myFile");</pre></tt>
    *    is the input file.
    * <ul>
    *    <li>Simple Invocation. No user data.</li>
    *       <tt><pre>
    *    SlipRead read();
    *    SlipHeader* list = read.read(filename);
    * 
    *       or
    * 
    *    SlipRead* read = new SlipRead();
    *    SlipHeader* list = read-&gt;read();
    *    delete read;
    *       </pre></tt>
    *    <li>Read with one User Data class.</li>
    *       <p>We assume throughout that:</p>
    *       <tt><pre>
    *    class Pointer: SlipPointer()
    *       </pre></tt>
    *       defines a pointer and that this pointer has name "Ptr_1".
    *       <tt><pre>
    *    SlipDatum userData(new Pointer());
    *    SlipRead read();
    *    read.registerUserData(userData);
    *    SlipHeader* list = read.read(filename);
    * 
    *       or
    * 
    *    SlipDatum userData(new Pointer());
    *    SlipRead* read = new SlipRead();
    *    read->registerUserData(userData);
    *    SlipHeader* list = read-gt;read();
    *    delete read;
    * 
    *    SlipDatum userData = new SlipDatum(new Pointer());
    *    SlipRead* read();
    *    read->registerUserData(*userData);
    *    SlipHeader* list = read-&gt;read();
    *    delete read;
    *    delete userData;
    *       </pre></tt>
    *    <li>Read with more than one User Data class.</li>
    *       <p>We assume throughout that:</p>
    *       <tt><pre>
    *    class Pointer_1: SlipPointer()
    *    class Pointer_2: SlipPointer()
    *              o o o
    *    class Pointer_N: SlipPointer()
    *       </pre></tt>
    *       defines multiple pointers and that each pointer has name "Ptr_i"
    *       where 'i' refers to the number in Pointer_i above. Then we can
    *       define the multiple classes as either:
    *       <tt><pre>
    *    SlipDatum userData[] = { SlipDatum(new Pointer_I)
    *                           , SlipDatum(new Pointer_2)
    *                                   o o o
    *                           , SlipDatum(new Pointer_N)
    *                           };
    *       </pre></tt>
    *     <br />
    *       or
    *       <tt><pre>
    *    SlipDatum userData_1(new Pointer_1);
    *    SlipDatum userData_2(new Pointer_2);
    *                   o o o
    *    SlipDatum userData_N(new Pointer_N);
    *       </pre></tt>
    *       This gives us several options. The options mimic the single
    *       User Data example. Some of the options are:
    *       <tt><pre>
    *    SlipRead read();
    *    read.registerUserData(sizeof(userData)/sizeof(userData[0]), userData);
    *    SlipHeader* list = read.read(filename);
    * 
    *       or
    * 
    *    SlipRead read(userData);
    *    SlipHeader* list = read.read(filename);
    * 
    *       or
    * 
    *    SlipRead read();
    *    read.registerUerData(userData[0]);
    *          o o o
    *    read.registerUerData(userData[n-1]);  // n = sizeof(userData)/sizeof(userData[0]
    *    SlipHeader* list = read.read(filename);
    * 
    *       or
    * 
    *    SlipRead read();
    *    for( int i = 0; i &lt; sizeof(userData)/sizeof(userData[0], i++)
    *       read.registerUserData(userData[i]);
    *    SlipHeader* list = read.read(filename);
    *       </pre></tt>
    * </ul>
    * <p><b>Notes</b></p>
    * <ol> 
    *    <li>The read, parse, register, and lex methods are flagrantly abusive
    *        of the heap. Be cautious in reading an input file in an embedded
    *        system or in any time sensitive environment.</li>
    * </ol>
    * <b>Description of the SLIP input grammar</b>
    * <pre><tt>
    * Syntax and tokens for SlipReadList.lex.cpp
    * Sublist Declaration
    * list NAME;                                      // forward declaration for a list
    * list, NAME, NAME, ...;                          // list of forward declarations
    *                                                 // list Definitions 
    * NAME( {mark} &lt;Descriptor list&gt; ( ) );           // general list definition, insert mark and Descriptor list
    * NAME( &lt;Descriptor list&gt; ( ) );                  // list Definition insert Descriptor list
    * NAME( {mark} ( ) );                             // list Definition insert user mark
    * NAME( );                                        // list Definition of an empty list
    * NAME( {mark} &lt;Descriptor list&gt; {NAME};          // expand NAME and insert mark and Descriptor list
    * NAME( {mark} {listNAME} );                      // expand listNAME and insert mark
    * NAME( &lt;Descriptor list&gt; {listNAME} );           // expand listNAME and insert Descriptor list
    *
    * User Data Declaration
    * user userName;                                  // Single user data name
    * user userName, ...;                             // sublist of user data names
    * userName{ (user data list) }                    // User Data converted to SlipDatum(PTR);
    *
    * \#include "path/name"                            // Include file
    *
    *  Syntax                                         // list Stuff
    *
    * 'c'                                             // char           SlipDatum((char)'c')
    * c                                               // char           SlipDatum((char)'c')
    * "string"                                        // string         SlipDatum((string)string, false)
    * true                                            // bool           SlipDatum((bool)true)
    * false                                           // bool           SlipDatum((bool)true)
    * numberUC                                        // unsigned char  SlipDatum((UCHAR)number)
    * numberC                                         // signed char    SlipDatum((CHAR)number)
    * numberUL                                        // unsigned long  SlipDatum((ULONG)number)
    * numberL                                         // signed long    SlipDatum((LONG)number)
    * number                                          // signed long    SlipDatum((LONG)number)
    * double        1.0E+0                            // double         SlipDatum(DOUBLE)number)
    * double        1.E+0                             // double         SlipDatum(DOUBLE)number)
    * double        1.                                // double         SlipDatum(DOUBLE)number)
    * double         .1E+0                            // double         SlipDatum(DOUBLE)number)
    * double         .1                               // double         SlipDatum(DOUBLE)number)
    * double        1E+0                              // double         SlipDatum(DOUBLE)number)
    * ( {mark} &lt;Descriptor list&gt; ( ) )                // Anonymous list SlipHeader()
    * ( &lt;Descriptor list&gt;( ) )                        // Anonymous list SlipHeader()
    * ( {mark} ( ) )                                  // Anonymous list SlipHeader()
    *
    * {NAME}                                          // list expansion SlipSublist(listName)
    *
    * Comments
    * / *   * /                                      // 'C' comments
    * //                                             // 'C++' comments
    *
    * Special Symbols  '(' ')' '{' '}' ';' '/' '&lt;' '&gt;' '\"'
    * Keywords         include list user
    *
    * EXAMPLE
    *
    * list mylist;                         // forward declaration of mylist
    *
    * \#include "myFileName"                // include the file "myFileName" 
    *
    * list list1, list2;                   // list of forward declarations
    *
    * user myData;                         // name of a User Data class
    *
    * list3 ( {0x15} &lt; 0 (1 2 {list1}) '3' 4 &gt; () );
    *         |      | | |             |   |    |
    *         |      | | |             |   |    . anonymous sublist part of list3
    *         |      | | |             |   |
    *         |      | | |             |   . value
    *         |      | | |             |
    *         |      | | |             . key
    *         |      | | |
    *         |      | | . anonymous list value in &lt;key v&gt;
    *         |      | |
    *         |      | . key in &lt;k v&gt;
    *         |      |
    *         |      . Descriptor list
    *         |
    *         . user mark for list3
    *
    * list4 ( &lt; "string" { 0x22 (5)} &gt; 9 10 11)
    *         | |        | |    |       |
    *         | |        | |    |       . list items in list4
    *         | |        | |    |
    *         | |        | |    . anonymous list
    *         | |        | |
    *         | |        | . user mark in anonymous list
    *         | |        |
    *         | |        . anonymous list value in &lt;key v&gt;
    *         | |
    *         | . key
    *         |
    *         . Descriptor list &lt;key v&gt; pairs
    *
    * list5 ( key1 value1 key2 value2 )
    *
    * list6 ( &lt;{list5}&gt; "one" 1.3 5UC 6L)    
    *         | |         |
    *         | |         . start of list items
    *         | |
    *         | . Descriptor defined by a list of &lt;key, value&gt; pairs
    *         |
    *         . list
    * 
    * list7 { myData( 3 4 ) };
    *         |     |
    *         |     . list containing user data
    *         |
    *         . user data name
    *
    * list8 { 230 ( able ( baker charlie (foxtrot) one {list1} ) ) };
    *
    * ( this "is" a "list" {list1} (1 2 3) {list5} {10 &lt;1 2&gt; (able)} ) // returned list
    *
    * </tt></pre>
    *
    * <b>Backus Naur Form (BNF)</b>
    * <pre><tt>
    * 
    * <b>Syntax</b>
    * state # LHS: RHS
    *            | RHS_alternative
    * 
    * <b>Grammar</b>
    * state
    *  0 \$accept: sublistDeclarations_Definition $end
    *
    *  1 sublistDeclarations_Definition: declarations listDefinition END
    *  2                               | declarations expandName END
    *  3                               | declarations
    *  4                               | listDefinition END
    *  5                               | expandName END
    *
    *  6 declarations: declarations declarationItem
    *  7             | declarationItem
    *  8             | END
    *
    *  9 declarationItem: LIST forwardReferenceList ';'
    * 10                | name listDefinition ';'
    * 11                | USER userDataList ';'
    * 12                | include
    *
    * 13 forwardReferenceList: forwardReferenceList ',' NAME
    * 14                     | NAME
    *
    * 15 listDefinition: '(' mark description list ')'
    * 16               | '(' mark list ')'
    * 17               | '(' mark ')'
    * 18               | '(' description list ')'
    * 19               | '(' list ')'
    * 20               | '(' ')'
    *
    * 21 description: '<' descriptionList '>'
    * 22            | '<' '>'
    * 23            | '<' mark '>'
    * 24            | '<' description descriptionList '>'
    *
    * 25 descriptionList: descriptionItemlist
    * 26                | mark descriptionItemlist
    * 27                | expandName
    *
    * 28 descriptionItemlist: descriptionItemlist descriptionElement descriptionElement
    * 29                    | descriptionElement descriptionElement
    *
    * 30 descriptionElement: expandName
    * 31                   | datum
    * 32                   | '(' listItemList ')'
    *
    * 33 include: '#' INCLUDE '"' pathList '"'
    *
    * 34 pathList: pathList '/' NAME
    * 35         | NAME
    *
    * 36 userDataList: userDataList ',' NAME
    * 37             | NAME
    *
    * 38 list: listItemList
    *
    * 39 listItemList: listItemList listItem
    * 40             | listItem
    *
    * 41 listItem: datum
    * 42         | expandName
    * 43         | userData
    * 44         | listDefinition
    *
    * 45 mark: '{' number '}'
    *
    * 46 expandName: '{' NAME '}'
    *
    * 47 userData: NAME listDefinition
    *
    * 48 datum: BOOL
    * 49      | CHAR
    * 50      | UCHAR
    * 51      | number
    * 52      | FLOAT
    * 53      | STRING
    *
    * 54 number: INTEGER
    * 55       | UINTEGER
    * 56       | CHARS
    * 57       | CHARU
    *
    * 58 name: NAME
    *  </tt></pre>
    */
   class SlipRead {
   private:
      SlipRegister* reg;                                                        //!< Pointer to hash table
      int errorReturn;                                                          //!< error flag
      int debugFlag;                                                            //!< Debug on/off
      bool inputDebugFlag;                                                      //!< input debug
      bool leakDebugFlag;                                                       //!< check for memory leaks
      bool lexerDebugFlag;                                                      //!< lexer debug
      bool parserDebugFlag;                                                     //!< Parser debug
   private:
      SlipRead(const SlipRead& orig);
      SlipHeader& parse();
   public:
      enum { INPUT  = 0x01                                                      //!< debug inputt
           , PARSER = 0x02                                                      //!< debug parser
           , LEXER  = 0x04                                                      //!< debug lexer
           , HASH   = 0x08                                                      //!< debug hash funcitons
           , LEAK   = 0x10                                                      //!< check for memory leaks
           , ALL    = 0xFF                                                      //!< debug all
      };
   public:
      SlipRead(int debugFlag = 0);
      SlipRead(SlipDatum& userData, int debugFlag = 0);
      SlipRead(const int size, SlipDatum const userData[], int debugFlag = 0);
      SlipRead(const int size, SlipDatum  * const userData[], int debugFlag = 0);
      ~SlipRead();
      int getError();
      SlipHeader& read(string filename);
      bool registerUserData(const SlipDatum& userData);
      bool registerUserData(const int size, SlipDatum  * const userData[]);
      bool registerUserData(const int size, SlipDatum const userData[]);
      void setDebugOFF(int debugFlag = 0);
      void setDebugON(int debugFlag = INPUT);
      
   }; // class SlipRead
}

#endif	/* SLIPREAD_H */

