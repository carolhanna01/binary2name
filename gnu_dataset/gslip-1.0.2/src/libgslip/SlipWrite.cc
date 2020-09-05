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
 * @file    SlipWrite.cpp
 * @author  A. Schwarz
 * @date    January 19, 2014
 *
 * @brief Output a list
 */

# include <iomanip>
# include <iostream>
# include <string>
# include "SlipCellBase.h"
# include "SlipErr.h"
# include "SlipHashEntry.h"
# include "SlipHeader.h"
# include "SlipReader.h"
# include "SlipRegister.h"
# include "SlipSequencer.h"
# include "SlipWrite.h"

using namespace std;

namespace slip {

   /*********************************************************
    *               Constructors & Destructors              *
   **********************************************************/

   /**
    * @brief Constructor for the write class.
    * <p>Output defaults to cout. The list is output to the standard out stream.</p>
    * @param[in] head SlipHeader&) list to be output
    * @param[in] debugFlag (int) debug mask
    */
   SlipWrite::SlipWrite(SlipHeader& head, int debugFlag)
                       : errorReturn(0)
                       , head(&head)
                       , listNo(0)
                       , line(string(MAXCOL, ' '))
                       , nameSize(-1)
                       , numEntries(0)
                       , out(cout)
                       , reg(new SlipRegister(debugFlag))
                       , userEntries(0) {
      writeDriver();
   }; // SlipWrite::SlipWrite(SlipHeader& head, bool testFlag)

   /**
    * @brief Constructor for the write class.
    * <p>Output is to the application provided output stream.</p>
    * @param[in] head (slipHeader&) list to be output
    * @param[in] out (ostream&) output stream
    * @param[in] debugFlag (int) debug mask
    */
   SlipWrite::SlipWrite(SlipHeader& head, ostream& out, int debugFlag)
                       : errorReturn(0)
                       , head(&head)
                       , listNo(0)
                       , line(string(MAXCOL, ' '))
                       , nameSize(-1)
                       , numEntries(0)
                       , out(out)
                       , reg(new SlipRegister(debugFlag))
                       , userEntries(0) {
      writeDriver();
   }; // SlipWrite::SlipWrite(SlipHeader head, ostream& out, bool testFlag)

   /**
    * @brief Copy constructor not allowed.
    * @param[in] orig (SlipWrite&) reference to a SlipWrite object
    */
   SlipWrite::SlipWrite(const SlipWrite& orig) : out(cout), indent(0) {
   }; // SlipWrite::SlipWrite(const SlipWrite& orig)

   /**
    * @brief Destructor.
    * <p>The SlipRegister object is deleted.</p>
    */
   SlipWrite::~SlipWrite() {
      delete reg;
   }; // SlipWrite::~SlipWrite()

   /*********************************************************
    *                        Methods                        *
   **********************************************************/

   /**
    * @brief Fit the current string onto a line.
    * <p>If the current string can not fit on the current line and is too
    *    large to fit on an empty line, attempt to find a blank or special
    *    character in the string that we can use for reducing the size of
    *    the string.</p>
    * <p>The string is iteratively searched for the largest substring that
    *    can fit on a blank line. If no substring can be found, the entire
    *    string is output.</p>
    * <p>The search proceeds in the forward direction. If a character ('')
    *    or string ("") is found in the input, then it is ignored.</p>
    * @param str (string&) input string
    */
   void SlipWrite::fitOutput(string& str) {                                     // Output an input string

   /**********************************************************************
    *                    Character Translation Table:.                   *
    **********************************************************************/
      enum { IGN = 0                         //!< ignore character
           , QUO = 1                         //!< double quote (string)
           , CHR = 2                         //!< single quote (character)
           , ESC = 3                         //!< escape next character
           , SPC = 4                         //!< space
      };
     static const char xTable[256] =             /* class translation table*/
     {
   /* nul  soh  stx  etx  eot  enq  ack  bel  bs   hs   lf   vt   ff   cr   so   si */
      IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN,IGN, IGN, IGN, IGN, IGN,  0,

   /* dle  dc1  dc2  dc3  dc4  nak  syn  etb  can  em   sub  esc  fs   gs   rs   us */
      IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN,  0,

   /* sp    !    "    #    $    %    &    '    (    )    *    +    -    ,    .    / */
      SPC, IGN, QUO, IGN, IGN, IGN, IGN, CHR, IGN, IGN, IGN, IGN, IGN, IGN, IGN,  0,

   /*  0    1    2    3    4    5    6    7    8    9    :    ;    <    =    >    ? */
      IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN,  0,

   /*  @    A    B    C    D    E    F    G    H    I    J    K    L    M    N    O */
      IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN,  0,

   /*  P    Q    R    S    T    U    V    W    X    Y    Z    [    \    ]    ^    _ */
      IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, ESC, IGN, IGN,  0,

   /*  `    a    b    c    d    e    f    g    h    i    j    k    l    m    n    o */
      IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN,  0,

   /*  p    q    r    s    t    u    v    w    x    y    z    {    |    }    ~  del */
      IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN, IGN,  0,

   /* the remaining 128 characters are not part of the legal ASCII character set*/
     };
     enum STATE { STS                        //!< Start state
                , DBL                        //!< double quote state
                , SNG                        //!< single quote state
                , XIT                        //!< exit
     };
     enum STATE fsm[][SPC+1] =
           /*              STATE      */
           /*  INPUT   STS  DBL  SNG  */
           { /*IGN*/ { STS, DBL, SNG }
           , /*QUO*/ { DBL, STS, SNG }
           , /*CHR*/ { SNG, DBL, STS }
           , /*ESC*/ { STS, DBL, SNG }
           , /*SPC*/ { XIT, DBL, SNG }
           };
     enum ACTION { NADA                      //!< do nothing
                 , ESCP                      //!< skip next character
                 , REC                       //!< record column number
     };
     enum ACTION act[][SPC+1] =
           /*               STATE       */
           /*  INPUT   STS   DBL   SNG  */
           { /*IGN*/ { NADA, NADA, NADA }
           , /*QUO*/ { NADA, NADA, NADA }
           , /*CHR*/ { NADA, NADA, NADA }
           , /*ESC*/ { ESCP, ESCP, ESCP }
           , /*SPC*/ { REC,  NADA, NADA }
           };
      int size = str.size();
      int lineSize = MAXCOL - min(indent, (int)MAXINDENT);
      int ndx = 0;
      while(size > 0) {
         int spc = -1;
         enum STATE state = STS;
         for(; ndx < min(size, lineSize); ndx++) {
            int curState = state;
            int map      = xTable[(int)str[ndx]];
            state        = fsm[map][curState];
            switch(act[map][curState]) {
               case NADA:
                  break;
               case ESCP:
                  ndx++;
                  break;
               case REC:
                  spc = ndx;
                  break;
            }
            spc = (size <= lineSize)? size: spc;
            if (spc > -1) {
               int len = spc - ndx + 1;
               size -= len;
               out << str.substr(ndx, len);
               if (size > 0) out << endl;
               else  col = len;
            } else {
               /************************************************
                * Deep thoughts lead to deep problems. there is
                * no space in the input and the input exceeds
                * the maximum line size. Attempt to output the
                * entire input to the output. If that fails,
                * well, issue a nasty warning message and pretend
                * it didn't happen.
                *************************************************/
               try {
                  out << str << endl;
               } catch(...) {
                  postError(__FILE__, __LINE__, SlipErr::E4013, "SlipWrite::fitOutput", "", "");
               }
            }
         }
      }

   }; // void SlipWrite::fitOutput(string& str)

   /**
    * @brief Output a string.
    * <p>If the string (plus a preceding blank) fits on the current line
    *    then output the string. If it fits on the next line (indented)
    *    then output the string to the next line. If it doesn't fit on
    *    either line then attempt to find a substrings which will fit
    *    wholly on a line and output them.</p>
    * @param[in] str (string&) output string
    */
   void SlipWrite::output(string str) {
      int size = str.size();
      int const remainingSize = (MAXCOL - col);
      int lineSize = MAXCOL - min(indent, (int)MAXINDENT);
      if ( size < remainingSize) {                                              // input fits on line
         col += 1 + size;
         line += " " + str;
      } else {                                                                  // no more space
         col = min(indent, (int)MAXINDENT);
         out << line << endl;
         line.clear();
         col = indent;
         line.replace(0, col, col, ' ');
         if (size <= lineSize) {                                                // fits on next line
            col += size;
            line += str;
         } else if (size <= MAXCOL) {                                           // ignore indentation
            out << str << endl;
         } else {
            /*********************************************************
             * We're in deep weeds at this point. The input string
             * is too large to fit on a line. What we do is is hope
             * that there is a blank not contained in a quote, if
             * there is then we output. This search and output continues
             * until the input string is exhausted. If the string can
             * not be output 'gracefully' then we output the entire
             * string and hope for the best. If this fails then a
             * warning is issued and we go on as if this tawdry event
             * never occurred. Ol' Man Mose has spoken.
             *********************************************************/
            fitOutput(str);
         }
      }
   }; // SlipWrite::output(const string& str)

   /**
    * @brief Output current line and prepare for next line.
    */
   void SlipWrite::outputEOL() {                                                // Output EOL
      out << line << endl;
      col = indent;
   }; // void SlipWrite::outputEOL()

   /**
    * @brief Collect and register list headers and User Data names.
    * <p>This is a recursive method. When a sublist object is found, the
    *    function recurses with the sublist referenced list header.</p>
    * <p>Data collection provides information for constructing User Data
    *    references ("user ...") in the output list file and to output
    *    list entries ("list#(...)") and detect cyclic graphs.</p>
    * <p>If the list is empty then it is not registered. On reading the
    *    input list file it becomes an anonymous list. This applies to
    *    the descriptor list attached to the list also.</p>
    * <p>When pass1 is complete, all User Data names and output methods
    *    and all non-empty lists will be registered.</p>
    * @param[in] head (SlipHeader&) list header
    */
   void SlipWrite::pass1(SlipHeader& head)  {

      if (head.isDList()) {                                                    // is Descriptor List
         SlipHeader& dList  = **(SlipHeader**)SlipCellBase::getHeadDescList((SlipCellBase&)head);
         pass1(dList);
      } else if (head.isEmpty() && !head.getMark() && !head.getRefCount()) return;

      if (registerSublist(head)) numEntries++;
      else return;

      SlipSequencer* seq = new SlipSequencer(head);
      seq->advanceLWR();
      while(*seq != head) {
         if (seq->isSublist()) {
            SlipSublist& sublist = (SlipSublist&)seq->currentCell();
            SlipHeader* header = *(SlipHeader**)SlipCellBase::getSublistHeader(sublist);
//            if ((!header->isEmpty()) || (header->isDList()))
               pass1(*header);
         } else if (seq->isPtr()) {
            SlipDatum& userData = (SlipDatum&)(seq->currentCell());
            if (registerUserData(userData)) {
               numEntries++;
               userEntries++;
               nameSize = max(nameSize, (int)((userData.getName()).size()));
            }
         }
         seq->advanceLWR();
      }
      delete seq;
   }; // void SlipWrite::pass1(SlipHeader& header)

   /**
    * @brief Output the User Data name forward references and lists.
    */
   void SlipWrite::pass2() {

      renameListNames();
      writeUserDeclaration();
      writeListDeclarations();
   }; // void SlipWrite::pass2()

   /**
    * @brief Create a unique list name and register name an list pointer.
    * <p>Operationally lists do not have names. But in order to create
    *    a list definition ("listName()") a name is required. This method
    *    creates a unique list name and registers the name and pointer to
    *    the list (SlipHeader*). The format of the list name is "list#"
    *    where '#' is a unique integer number.</p>
    * <p>Registration fails when the hash table used to store registered
    *    values needs to be expanded and no more heap space is available.</p>
    * @param[in] header (SlipHeader&)
    * @return <b>true</b> <name, SlipHeader*> registered
    */
   bool SlipWrite::registerSublist(SlipHeader& header) {                        // register list header into hash table
      if (!reg->search(header)) {
         stringstream pretty;
         pretty << "list" << ++listNo;
         string name(pretty.str());
         return reg->post(header, name);
      }
      return false;
   }; // bool SlipWrite::registerSublist(const SlipHeader& head) const

   /**
    * @brief Register the User Data name.
    * <p>Each User Data class is required to have a name and to have a
    *    method which allows retrieval of the name. Each instance of the
    *    same User Data class will return the same name.</p>
    * <p>The application must ensure that each User Data class has a unique
    *    name and that the User Data output method returns a string representing
    *    a string ("(...)").</p>
    * <p>Registration will fail if the User Data class has been  registered
    *    or if hap allocation for the registration hash table is not successful.</p>
    * @param[in] userData (SlipDatum&) user data object
    * @return <b>true</b> <User Data Name, write method> were registered
    */
   bool SlipWrite::registerUserData(const SlipDatum& userData) const {          // register user data into the hash table
      return reg->post(userData);
   }; // SlipWrite::registerUserData(const SlipDatum& userData)

   /**
    * @brief Return the list name.
    * <p>List/sublist references are stored in the registration table
    *    as an ordered pair, <SlipHeader*, name>. If the the input
    *    list reference is found in the table then the constructed
    *    list name ("list#") is returned, otherwise "" is returned.</p>
    * @param[in] head (SlipHeader&) reference to list
    * @return <b>list name</b> if the list has been registered.
    */
   string SlipWrite::getSublistName(SlipHeader& head) const {                   // Have we visited this sublist
      return reg->getSublist(head);
   }; //bool SlipWrite::search(SlipHeader& head) const

   /**
    * @brief Output a description list.
    * <p>This is a recursive method.</p>
    * <p>Output formats are one of:</p>
    * <ul>
    *    <li>&lt; &gt; empty list.</li>
    *    <li>&lt;{name}&gt; separate list definition given.</li>
    *    <li>&lt; ooo &gt; list values given.</li>
    *    <li>&lt; &lt; ooo &gt; &gt; nested declaration list. Each nested
    *        level is defined as above.</li>
    * </ul>
    * @param[in] head (SlipHeader&) reference to a description list
    */
   void SlipWrite::writeDescriptionList(SlipHeader& head) {                     // Output description list
      output("<");
      string name = getSublistName(head);
      if (name != "") output("{" + name + "}");
      else writeList(head);
      output(">");
   }; // void SlipWrite::writeDescriptionList(SlipHeader& head)

   /**
    * @brief Provide an ordering relation to list names in the registration table.
    * <p>The hash table associated with sublist registration contains names in
    *    a random order. Linearly search through this table and rename the sublists
    *    as an ordered sequences, list1, list2, ... listN.</p>
    */
   void SlipWrite::renameListNames() {

      if (listNo) {
         const int size = reg->size();

         /*************************************************
          * Rename Output list definitions
          *************************************************/
         int i = 0;
         for( int ndx = 0; ndx < size; ndx++) {
            SlipHashEntry* entry = (*reg)[ndx];
            if (entry != NULL) {
               if (entry->getType() != SlipHashEntry::USERDATA) {
                  i++;
                  stringstream pretty;
                  pretty  << "list" << i;
                  string str = pretty.str();
                  entry->rename(str);
                  if (i > listNo) break;
               }
            }
         }
      }
   }; //  void SlipWrite::renameListNames()

   /**
    * @brief Execute write methods.
    * <p>Execute the write methods and perform some required services.</p>
    * <p>This is the single point where execution occurs. Write operations
    *    start when SlipWrite is instantiated. All constructors reference
    *    this method to complete the write.</p>
    */
   void SlipWrite::writeDriver() {                                              // Execute write actions
      pass1(*head);
      pass2();
      if (indent != col) out << line << endl;
      line.clear();
      col = 0;
      string name = getSublistName(*head);
      if (name != "") {
         output("{" + name + "}");
      } else {
         output("(");
         writeList(*head);
         output(")");
      }
      out << line << endl;
   }; // void SlipWrite::writeDriver()

   /**
    * @brief Output a list.
    * <p>Each item in a list is output.</p>
    * <ul>
    *    <li>The write method for each SlipDatum object is called. This
    *        output the datum in the instance formatted for a list file.</li>
    *    <li>The format for a User DAta object is {name(ooo)} where the
    *        <i>name</i> is the class name the the list ("(ooo)") is the
    *        returned User Data.</li>
    *    <li>A sublist either is output as "{name}" or "()".</li>
    * </ul>
    * @param[in] header (SlipHeader&) list header
    */
   void SlipWrite::writeList(SlipHeader& header) {
      if (header.getMark()) {
         output("{");
         stringstream pretty;
         pretty << "0x" << setw(2) << setfill('0') << hex << header.getMark();
         output(pretty.str());
         output("}");
      }
      if (header.isDList()) {
         SlipHeader& dList = **(SlipHeader**)SlipCellBase::getHeadDescList((SlipCellBase&)header);
         writeDescriptionList(dList);
      }
      if (!header.isEmpty()) {
         SlipSequencer* seq = new SlipSequencer(header);
         seq->advanceLWR();
         while(!seq->isHeader()) {
            SlipCell& cell = seq->currentCell();
            if (seq->isPtr()) {
               SlipDatum& datum = (SlipDatum&)cell;
               output(datum.getName());
               output(datum.write());
            } else if (seq->isData()) {
               SlipDatum& datum = (SlipDatum&)cell;
               output(datum.write());
            } else {                                                            // sublist
               SlipHeader& sublist = (SlipHeader&)cell;
               SlipHeader& header = **(SlipHeader**)SlipCellBase::getSublistHeader(sublist);
               string name = getSublistName(header);
               if (name != "") output("{" + name + "}");
               else {
                  output("(");
                  if (header.isDList()) writeDescriptionList(header);
                  output(")");
               }
            }
            seq->advanceLWR();
         }
         delete seq;
      }
   }; // void SlipWrite::writeList(SlipHeader& header))

   /**
    * @brief Output the list declaration and definition.
    * <p>The registration hash table is accessed. Each entry identified as
    *    a list causes the list name (the declaration) and contents (the
    *    definition) to be output.</p>
    */
   void SlipWrite::writeListDeclarations() {                                    // Output sublist definitions

      if (listNo) {
         const int size = reg->size();

         /*************************************************
          * Calculate the maximum characters in a list name
          *************************************************/
         int log = 1;
         int tmp = listNo;
         while(tmp /= 10) log++;
         int sublistSize = 4 + log;
         indent = 5 + sublistSize + 1;
         col    = indent;

         /*************************************************
          * Output list definitions
          *************************************************/
         int i = 0;
         for( int ndx = 0; ndx < size; ndx++) {
            SlipHashEntry* entry = (*reg)[ndx];
            if (entry != NULL) {
               if (entry->getType() != SlipHashEntry::USERDATA) {
                  string name = entry->getAsciiValue();
                  line = name;
                  line.replace(name.size(), (indent - name.size()), " ");
                  col    = indent;
                  SlipHeader* header = (SlipHeader*)entry->getBinaryKey();
                  writeListDefinition(*header);
                  if ((++i) > listNo) break;
               }
            }
         }
      }
   }; // void SlipWrite::writeListDefinitions()

   /**
    * @brief Output the list declaration and definition.
    * <p>The lislt declaration is the list name. The list definition is the
    *    list body.</p>
    * @param[in] header (SlipHeader&) reference to the list
    */
   void SlipWrite::writeListDefinition(SlipHeader& header) {                    // Output sublist definition
      output("(");
      writeList(header);
      output(");");
      outputEOL();

   }; // void SlipWrite::writeListDefinition(SlipHeader& head)

   /**
    * @brief Output the User Data references.
    * <p>Each unique User Data class used in the list is output in a declaration
    *    statement. The declaration statement serves to identify the required
    *    User Data classes needed to input (read) the list file. If the
    *    declared User Data classes are not present, the user data will not
    *    be parsed.</p>
    */
   void SlipWrite::writeUserDeclaration() const {

      /**************************************************
       * Calculate the maximum User Data names in a line
       **************************************************/
      if (userEntries) {
         const int size = reg->size();
         const int entriesPerLine = ((MAXCOL - 5) + nameSize)/(nameSize + 1);
         string  delim = " ";
         int i = 0;                                                             // number entries/line
         int ndx = 0;                                                           // Hash Table Index
         int entry = 1;                                                         // number of user entries
         out << "user ";

         /****************************************
          * Output the first user data entry
          ****************************************/
         for(;ndx < size; ndx++) {
            if ((*reg)[ndx] != NULL) {
               if ((*reg)[ndx]->getType() ==SlipHashEntry::USERDATA) {
                  out << delim << setw(nameSize) << (*reg)[ndx]->getAsciiKey();
                  delim = ", ";
                  if ((++entry) >= userEntries) break;
                  if ((++i) >= entriesPerLine) break;
               }
            }
         }

         /****************************************
          * Output the remaining user data entries
          ****************************************/
         if (entry < userEntries) {
            for(; ndx < size; ndx++) {
               if ((*reg)[ndx] != NULL) {
                  if ((*reg)[ndx]->getType() ==SlipHashEntry::USERDATA) {
                     if (i >= entriesPerLine) {
                        out << endl << "    ,";
                        i = 0;
                     }
                     out << setw(nameSize) << (*reg)[ndx]->getAsciiKey();
                     if ( (++entry) >= userEntries) break;
                     i++;
                  }
               }
            }
         }
         out << ";" << endl;
      }
   }; // void SlipWrite::writeUserDeclaration() const
}; // namespace slip
