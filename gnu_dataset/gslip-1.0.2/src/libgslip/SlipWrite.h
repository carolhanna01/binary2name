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
 * @file    SlipWrite.h
 * @author  A. Schwarz
 * @date    January 19, 2014
 *
 * @brief Format and output a list
 */

#ifndef SLIPWRITE_H
#define	SLIPWRITE_H

# include <iostream>
# include <string>
# include "SlipHeader.h"
# include "SlipReader.h"
# include "SlipRegister.h"

using namespace std;

namespace slip {
   /**
    * @class SlipWrite
    * <p><b>Overview</b></p>
    * <p>Output a single list onto a stream. The stream can be to a file or
    *    to an internal buffer. The output consists of three sections:</p>
    * <ul>
    *    <li><b>User Data Declarations:</b> The class names for User Data 
    *        objects which form part of the input list are declared. The
    *        application must register the classes before the list can
    *        be read.</li>
    *    <li><b>Sublist Definitions:</b> Sublists contained with the list
    *        are declared and defined. Each sublist is treated as a list
    *        in terms of creating User Data Declarations and additional
    *        Sublist Definitions. That is, the definitional mechanism is
    *        recursive.</li>
    *    <li><b>List:</b> Either a reference to a Sublist Definition which
    *        defines the list or the list definition.</li>
    * </ul>
    * <p>Write outputs a list from the constructor. When an instance of 
    *    SlipWrite is created write starts. The algorithm makes two passes
    *    over the input file. In the first pass data is collected concerning
    *    the User Data classes used and the sublists contained in the list.
    *    During the second pass the User Data Declarations, Sublist Definitions
    *    and the list are output. The algorithm on each pass is O(n), where
    *    'n' is the number of SLIP cells in the list.</p>
    * <p>Other than the constructors, there are no other public methods.
    *    The methods are recursive and use is made of the heap. This class
    *    does not do its own garbage collection.</p>
    * <p>There are some minor optimizations of the output format:</p>
    * <ul>
    *    <li>Anonymous lists: an empty list with a zero user mark and no
    *        descriptor list is output as "<b(>( )</b>" and does not 
    *        have a 'name'.</li>
    *    <li>Anonymous descriptor lists: an empty descriptor list with a
    *        zero user mark and no chained descriptor lists is output as 
    *        "<b>&lt; &gt;</b>" and does not have a name.</li>
    *    <li></li>
    *    <li></li>
    *    <li></li>
    * </ul>
    * <p>@sa SlipRead to see examples of output and a description of the
    *    syntax.</p>
    * <p><b>Constructors and Destructors</b></p>
    * <ul>
    *    <li><b>SlipWrite</b> Output a list to an output stream.</li>
    * </ul>
    * <p><b>Methods</b></p>
    * <ul>
    *    <li><b>fitOutput()</b> Fit a string to multiple lines. If a string
    *        is too large to fit on the current line completely or the next
    *        line completely a forward scan of the string is made to find
    *        substrings which can be safely written without injuring the
    *        integrity of the line contents.</li>
    *    <li><b>getSublistName()</b> Retrieve the generated name for a 
    *        sublist from the registry.</li>
    *    <li><b>output()</b> Output a string. The output method determines
    *        when to output a new line and indents the new line appropriately.</li>
    *    <li><b>pass1()</b> Name and register sublists and register User Data
    *        class names from the input list.</li>
    *    <li><b>pass2()</b> Manage the output process.</li>
    *    <li><b>registerSublist()</b> Register (and name) a sublist.</li>
    *    <li><b>registerUserData()</b> Register a User Data class.</li>
    *    <li><b>writeDriver()</b> Executor of write methods.</li>
    *    <li><b>writeList()</b> Output sublist contents.</li>
    *    <li><b>writeDescriptionList()</b> Output a list description list.</li>
    *    <li><b>writeListDeclarations()</b> Output the sublist declarations and
    *        manage sublist output.</li>
    *    <li><b>writeListDefinition()</b> Output a sublist.</li>
    *    <li><b>writeUserDeclaration()</b> Output the User Data declaration
    *        list.</li>
    * </ul>
    */
   class SlipWrite {
      enum { MAXCOL    = 120                                                    //!< Line length
           , MAXINDENT =  40                                                    //!< maximun imdent level
      };
      int col;                                                                  //!< Current column
      int indent;                                                               //!< Indentation
      int listNo;                                                               //!< Unique list number
      int nameSize;                                                             //!< Maximum User Data name size
      int numEntries;                                                           //!< Number of entries in the hash table
      int errorReturn;                                                          //!< Error flag
      int userEntries;                                                          //!< Number of user data entries in hash table
      ostream& out;                                                             //!< Output file
      SlipHeader* head;                                                         //!< List start
      SlipRegister* reg;                                                        //!< Pointer to hash table
      string line;                                                              //!< Output line
   private:
      SlipWrite(const SlipWrite& orig);
      void   fitOutput(string& str);                                            // Output an input string
      string getSublistName(SlipHeader& head) const;                            // Have we visited this sublist
      void   output(string str);                                                // Output an input string
      void   outputEOL();                                                       // Output EOL 
      void   pass1(SlipHeader& head);                                           // Extract User Data and Sublists
      void   pass2();                                                           // Output deffinitions and declarations
      bool   registerSublist(SlipHeader& header);                               // Register list header into hash table
      bool   registerUserData(const SlipDatum& userData) const;                 // Register user data into the hash table
      void   renameListNames();                                                 // Provide an ordering for lists
      void   writeDriver();                                                     // Execute write actions
      void   writeList(SlipHeader& head);                                       // Output the list
      void   writeDescriptionList(SlipHeader& head);                            // Output description list
      void   writeListDeclarations();                                           // Output sublist ceclarations
      void   writeListDefinition(SlipHeader& head);                             // Output sublist definition
      void   writeUserDeclaration() const;                                      // Output USER declarations
   public:
      SlipWrite(SlipHeader& head, int debugFlag = 0);
      SlipWrite(SlipHeader& head, ostream& cout, int debugFlag = 0);
      virtual ~SlipWrite();
   private:

   };
}; // namespace slip
#endif	/* SLIPWRITE_H */

