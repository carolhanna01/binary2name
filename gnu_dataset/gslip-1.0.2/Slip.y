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
 * @brief Description of the SLIP grammar.
 * <pre><tt>
 * Syntax and tokens for SlipReadList.lex.cpp
 * Sublist Declaration
 *                                                 // list Definitions 
 * NAME( {mark} <Descriptor list> ( ) );           // general list definition, insert mark and Descriptor list
 * NAME( <Descriptor list> ( ) );                  // list Definition insert Descriptor list
 * NAME( {mark} ( ) );                             // list Definition insert user mark
 * NAME( );                                        // list Definition of an empty list
 * NAME( {mark} <Descriptor list> {NAME};          // expand NAME and insert mark and Descriptor list
 * NAME( {mark} {listNAME} );                      // expand listNAME and insert mark
 * NAME( <Descriptor list> {listNAME} );           // expand listNAME and insert Descriptor list
 *
 * User Data Declaration
 * user userName;                                  // Single user data name
 * user userName, ...;                             // sublist of user data names
 * userName{ (list) }                              // User Data converted to SlipDatum(PTR);
 *
 * #include "path/name"                            // Include file
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
 * ( {mark} <Descriptor list> ( ) )                // Anonymous list SlipHeader()
 * ( <Descriptor list>( ) )                        // Anonymous list SlipHeader()
 * ( {mark} ( ) )                                  // Anonymous list SlipHeader()
 *
 * {NAME}                                          // list expansion SlipSublist(listName)
 *
 * Comments
 * / *   * /                                      // 'C' comments
 * //                                             // 'C++' comments
 *
 * Special Symbols  '(' ')' '{' '}' ';' '/' '<' '>' '\"'
 * Keywords         include user
 *
 * EXAMPLE
 *
 * list mylist;                         // forward declaration of mylist
 *
 * #include "myFileName"                // include the file "myFileName" 
 *
 * list list1, list2;                   // list of forward declarations
 *
 * user myData;                         // name of a User Data class
 *
 * list3 ( {0x15} < 0 (1 2 {list1}) '3' 4 > () );
 *         |      | | |             |   |    |
 *         |      | | |             |   |    . anonymous sublist part of list3
 *         |      | | |             |   |
 *         |      | | |             |   . value
 *         |      | | |             |
 *         |      | | |             . key
 *         |      | | |
 *         |      | | . anonymous list value in <key v>
 *         |      | |
 *         |      | . key in <k v>
 *         |      |
 *         |      . Descriptor list
 *         |
 *         . user mark for list3
 *
 * list4 ( < "string" { 0x22 (5)} > 9 10 11)
 *         | |        | |    |       |
 *         | |        | |    |       . list items in list4
 *         | |        | |    |
 *         | |        | |    . anonymous list
 *         | |        | |
 *         | |        | . user mark in anonymous list
 *         | |        |
 *         | |        . anonymous list value in <key v>
 *         | |
 *         | . key
 *         |
 *         . Descriptor list <key v> pairs
 *
 * list5 ( key1 value1 key2 value2 )
 *
 * list6 ( <{list5}> "one" 1.3 5UC 6L)    
 *         | |         |
 *         | |         . start of list items
 *         | |
 *         | . Descriptor defined by a list of <key, value> pairs
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
 * ( this "is" a "list" {list1} (1 2 3) {list5} {10 <1 2> (able)} ) // returned list
 *
 * </tt></pre>
 */
 
 /*
  * Maintenance:
  * Compile: bison -d -t --no-lines --warnings=all --language=c++ --report=all slip.y
  *          cp *.hh ../slip/; cp Slip.Bison.cpp ../slip/; cp Slip.Bison.hpp ../slip/;
 */
 
 /*************************************
  *    Declarations and Options
  *************************************/
 
%code requires {
   # include <stdint.h>
   # include <string>
   # include "SlipCell.h"
   # include "SlipDatum.h"
   # include "SlipDescription.h"
   # include "SlipHeader.h"
   # include "SlipInputMethods.h"
   # include "SlipSublist.h"

   using namespace std;
}

 %file-prefix "Slip.bison"
 %language "C++"
 %parse-param {SlipHeader** head}              // Parser return value
 %parse-param {SlipInputMethods* methods}      // Methods used by parser
 %name-prefix "Slip"                           // Change 'yy' prefix to Slip
 %output "Slip.Bison.cpp"                      // Output Slip.Bison files
 %debug                                        // Enable runtime debug information
 /*%define parse.trace                           // Generate parse trace information (debug)*/
 %define api.namespace { slip }                // Namespace for parser
 %defines                                      // Generate I/F files (bison -d)
 %no-lines                                     // Suppress #line in generated files
 %locations                                    // location information required
 %{
   # include "lex.Slip.h"
   
   using namespace std;
 %}
 
%code requires {

#ifdef DEBUG_IO
   #undef DEBUG
   #define DEBUG(flag, stmt) if (flag) { stmt }
#else
   #define DEBUG(flag, stmt)
#endif
}

%union YYSTYPE {                               //!< Token values
   bool          BOOL;                         //!< token boolean true / false
   int8_t        CHAR;                         //!< 8-bit  token value
   double        FLOAT;                        //!< 64-bit token value
   int32_t       INTEGER;                      //!< 32-bit token value
   string*       STRING;                       //!< pointer to new string()
   uint8_t       UCHAR;                        //!< 8-bit unsigned value
   uint32_t      UINTEGER;                     //!< 32-bit unsigned value
   SlipDatum*    SLIPDATUM;                    //!< data carrying SLIP cell
   SlipHeader*   SLIPHEADER;                   //!< list header
   SlipSublist*  SLIPSUBLIST;                  //!< list sublist
   SlipCell*     SLIPCELL;                     //!< general slip cell
   SlipDescription*  DESCRIPTION;                  //!< descriptionList data passage
}

 %start sublistDeclarations_Definition
 
 // Types associated with terminals
 %token <BOOL>          BOOL                   /* boolean value TRUE or FALSE       */
 %token <INTEGER>       END                    /* End-Of-File indication            */
 %token <CHAR>          CHAR                   /* a character 'c'                   */
 %token <CHAR>          CHARS                  /* a signed 8-bit number             */
 %token <UCHAR>         CHARU                  /* an unsigned 8-bit number          */
 %token <FLOAT>         FLOAT                  /* double value                      */
 %token <INTEGER>       INCLUDE                /* the literal 'include'             */
 %token <INTEGER>       INTEGER                /* a signed long integer             */
 %token <STRING>        NAME                   /* a string [a-zA-Z_$][a-zA-Z0-9_$]* */
 %token <STRING>        STRING                 /* a string "string"                 */
 %token <INTEGER>       LIST                   /* the literal 'list'                */
 %token <UCHAR>         UCHAR                  /* an unsigned character 'c'         */
 %token <UINTEGER>      UINTEGER               /* an unsigned long integer          */
 %token <INTEGER>       USER                   /* the literal 'user'                */

// Types associated with non-terminals
 %type <DESCRIPTION>    datum
 %type <DESCRIPTION>    description
 %type <INTEGER>        declarations
 %type <DESCRIPTION>    descriptionElement
 %type <INTEGER>        declarationItem
 %type <DESCRIPTION>    descriptionItemlist
 %type <DESCRIPTION>    descriptionList
 %type <DESCRIPTION>    namedList
 %type <DESCRIPTION>    listItem
 %type <DESCRIPTION>    number
 %type <DESCRIPTION>    list
 %type <DESCRIPTION>    listDefinition
 %type <DESCRIPTION>    listItemList
 %type <SLIPHEADER>     sublistDeclarations_Definition
 %type <DESCRIPTION>    userData
 %type <INTEGER>        include
 %type <UCHAR>          mark
 %type <STRING>         name
 %type <INTEGER>        userDataList
 
// destruct on errors
%destructor {  methods->incErrorCount();
               $$->deleteData(); 
               $$ = NULL;
            }  datum description descriptionElement 
               descriptionItemlist descriptionList
               namedList listItem number list listDefinition
               listItemList userData

 %%
 /*************************************
  *              BNF
  *************************************/
sublistDeclarations_Definition:   
            declarations listDefinition END    /* list defined, we are done         */
                     {  *head = $$ = (SlipHeader*)($2->getPtr()); 
                        DEBUG(methods->getInputFlag(), methods->debugOut("declarations listDefinition END", $2);)
                        delete $2;
                        YYACCEPT; 
                     }
          | declarations namedList END         /* list defined, we are done         */
                     {  *head = $$ = (SlipHeader*)($2->getPtr()); 
                        DEBUG(methods->getInputFlag(), methods->debugOut("declarations namedList END", $2);)
                        delete $2;
                        YYACCEPT; 
                     }
          | declarations                       /* declaration section               */
                     {  $$ = NULL; 
                        DEBUG(methods->getInputFlag(), methods->debugOut("declarations END", NULL);)
                     }
          | listDefinition END                 /* list defined, we are done         */
                     {  *head = $$ = (SlipHeader*)($1->getPtr()); 
                        DEBUG(methods->getInputFlag(), methods->debugOut("listDefinition END", $1);)
                        delete $1;
                        YYACCEPT; 
                     }
          | namedList END                      /* list defined, we are done         */
                     {  *head = $$ = (SlipHeader*)($1->getPtr());
                        DEBUG(methods->getInputFlag(), methods->debugOut("namedList END", $1);)
                        delete $1;
                        YYACCEPT; 
                     }
          ;

declarations:       
            declarations declarationItem       /* list of declarations              */
                     { DEBUG(methods->getInputFlag(), methods->debugOut("declarations declarationItem", NULL);)
                     }
          | declarationItem                    /* single declaration                */
                     { DEBUG(methods->getInputFlag(), methods->debugOut("declarationItem", NULL);)
                     }
          ;

declarationItem:
            name listDefinition ';'            /* list definition                   */
                     {  $$ = 0;
                        methods->declarationItem("name listDefinition", $1, $2);
                     }
          | USER userDataList ';'              /* user data names                   */
          | include                            /* include file def'n                */
          | END                                /* EOF check if in an include file   */
                     {  if (!isTop())  restoreLexState(); 
                        DEBUG(methods->getInputFlag(), methods->debugOut("END", NULL);)
                     }
          ;

listDefinition:   
            '(' mark description list ')'       /* full list declaration              */
                     {  $$ = methods->markDescriptionList("(mark description list)", $2, $3, $4);
                     }
          | '(' mark description ')'            /* mark and description w/o list      */
                     {  $$ = methods->markDescription("(mark description)", $2, $3);
                     }
          | '(' description list ')'            /* no mark in list                    */
                     {  $$ = methods->descriptionList("(description list)", $2, $3);
                     }
          | '(' description ')'                 /* no list just a description         */
                     {  $$ = methods->descriptionNULLList("(description)", $2);
                     }
          | '(' mark list ')'                   /* no descriptor in list              */
                     {  $$ = methods->markList("(mark list)", $2, $3);
                     }
          | '(' mark ')'                        /* list contains only a mark          */
                     {  $$ = methods->mark("(mark)", $2);
                     }
                                                /* list declaration                  */
          | '(' list ')'                        /* list declaration w/o description list */
                     {  $$ = methods->listDefinition("(mark list)", $2);
                     }
          | '(' ')'                             /* empty list                        */
                     {  $$ = methods->nullList("(list)");
                     }
          ;

description:   
             '<' description descriptionList '>'   /* < < > 1 2>                 */
                     {  $$ = methods->descriptionDescriptionList("<description descriptionList>", $2, $3);
                     }
         | '<' descriptionList '>'            /* Association list               */
                     {  $$ = methods->simpleReturn("< descriptionList >", $2);
                     }
          | '<' description '>'
                     {  $$ = methods->description("< description >", $2);
                     }
          | '<' mark '>'                       /* almost empty association list  */
                     {  $$ = methods->markDescription("< mark > ", $2);
                     }
          | '<' '>'                            /* empty association list         */
                     {  $$ = methods->nullDescription("< mark > ");
                     }
          ;
          
descriptionList: 
             descriptionItemlist
                     {  $$ = methods->simpleReturn("descriptionItemlist", $1);
                     }
          |  mark descriptionItemlist
                     {  $$ = methods->markDescriptionItemList("mark descriptionItemlist", $1, $2);
                     }
          | namedList
                     {  $$ = methods->simpleReturn("namedList", $1);
                     }
          ;
descriptionItemlist:
                                               /* descriptionItemlist          */
            descriptionItemlist descriptionElement descriptionElement
                     {  $$ = methods->descriptionItemlist("descriptionItemlist", $1, $2, $3);
                     }
                                               /* description item               */
          | descriptionElement descriptionElement
                     {  $$ = methods->descriptionItem("descriptionElement", $1, $2);
                     }
          ;  

descriptionElement:
            namedList                          /* {NAME} list expansion          */
                     {  $$ = $1; }
          | datum                              /* SlipDatum()                    */
                     {  $$ = $1; }
          | '(' listItemList ')'               /* SlipDatum() or SlipHeader      */
                     {  $$ = $2; }
          ;

include:    '#' INCLUDE datum                 /* include file path and name     */
                     {  $$ = 0;
                        methods->includeFile("INCLUDE datum", $3);
                     }
          ;
          
userDataList:                                  /* list of User Data names        */
            userDataList ',' name
                     {  methods->userDataList("", $3);
                     }
          | name
                     {  methods->userDataList("", $1);
                     }
          ;

list:
            listItemList
                     {  $$ = methods->simpleReturn("list", $1);
                     }
            ;
          
listItemList:
            listItemList listItem              /* listHeader.enqueue(listItem)   */
                     {  $$ = methods->listItemList("listItemList listItem", $1, $2);
                     }
          | listItem                           /* listHeader.enqueue(listItem)   */
                     {  $$ = methods->listItem("listItem", $1);
                     }
          ;

listItem:   
            datum                              /* SlipDatum(datum)               */
                     {  $$ = $1; }
          | namedList                          /* SlipSublist(namedList)         */
                     {  $$ = $1; }
          | userData                           /* SlipDatum(PTR)                 */
                     {  $$ = $1; }
          | listDefinition                     /* SlipSubsublist(list)           */
                     {  $$ = methods->listItemListDefinition("listDefinition", $1);
                     }
          ;

mark:                                          /* list user mark                 */
          '{' number '}'
                     {  $$ = (UCHAR)(*(SlipDatum*)($2->getPtr()));
                        delete $2;
                     }
          ;
          
namedList: '{' name '}'                        /* SlipSubsublist(name)           */
                     {  $$ = methods->namedList("{ name }", $2);
                     }
          ;

userData:   name  listDefinition               /* SlipDatum(PTR)                 */
                       { $$ = methods->userData("name listDefinition", $1, $2); }
          | name  namedList
                       { $$ = methods->userData("name {namedList}", $1, $2); }
          ;

datum:      BOOL                               /* SlipDatum((bool)boolValue)     */
                     { $$ = new SlipDescription(new SlipDatum($1), SlipDescription::DATA, methods->getDebugFlag()); }
          | CHAR                               /* SlipDatum((char)chr)           */
                     { $$ = new SlipDescription(new SlipDatum($1), SlipDescription::DATA, methods->getDebugFlag()); }
          | UCHAR                              /* SlipDatum((unsigned char)unsignedChr)*/
                     { $$ = new SlipDescription(new SlipDatum($1), SlipDescription::DATA, methods->getDebugFlag()); }
          | number                             /* discrete number                */
                     { $$ = $1; }
          | FLOAT                              /* SlipDatum((double)floatNum)    */
                     { $$ = new SlipDescription(new SlipDatum($1), SlipDescription::DATA, methods->getDebugFlag()); }
          | STRING                             /* SlipDatum((string)string)      */
                     { $$ = new SlipDescription(new SlipDatum($1), SlipDescription::DATA, methods->getDebugFlag()); }
          ;
          
number:     INTEGER                         /* SlipDatum((long)integer)          */
                     { $$ = new SlipDescription(new SlipDatum($1), SlipDescription::DATA, methods->getDebugFlag()); }
          | UINTEGER                        /* SlipDatum((unsigned long)unsignedInteger)*/
                     { $$ = new SlipDescription(new SlipDatum($1), SlipDescription::DATA, methods->getDebugFlag()); }
          | CHARS                           /* SlipDatum((char)chr)              */
                     { $$ = new SlipDescription(new SlipDatum($1), SlipDescription::DATA, methods->getDebugFlag()); }
          | CHARU                           /* SlipDatum((unsigned char)unsignedChr)*/
                     { $$ = new SlipDescription(new SlipDatum($1), SlipDescription::DATA, methods->getDebugFlag()); }
          ;
 name     : NAME
                     { $$ = $1; }
          ;
%%


void parser::error(const location_type& loc, const std::string& msg) {
//   fprintf(stderr, "%s %4d:%4d %s\n"
//                 , getFileName().c_str()
   fprintf(stderr, "%4d:%4d %s\n"
                 , loc.begin.line
                 , loc.begin.column
                 , msg.c_str());
}; // parser::error(const location_type& loc, const std::string& msg)
