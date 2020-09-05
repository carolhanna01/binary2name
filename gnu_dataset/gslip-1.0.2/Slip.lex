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
  * Maintenance:
  * Compile: flex --noline slip.lex
  * Fixup:   cp lex.Slip.c ../slip/lex.Slip.cpp;
 */
 
%option noyywrap
%option prefix="Slip"

%{
# include <stdio.h>
# include <cerrno>
# include <cstdlib>
# include <cstring>
# include <ctype.h>
# include <string>
# include <stdint.h>
# include "lex.Slip.h"
# include "Slip.Bison.hpp"

using namespace std;
using namespace slip;

#define YY_DECL int Sliplex( parser::semantic_type* yylval\
                           , parser::location_type* yylloc)
                           
#undef DEBUG
#ifdef DEBUG_IO
   #define DEBUG(flag, stmt) if (flag) stmt
#else
   #define DEBUG(flag, stmt)
#endif

# define ATOF myatof
double myatof(char * const);

#define TOKEN slip::parser::token
#define MAX_STR_CONST 1025
   
   using namespace std;
   
   bool  lexerDebugFlag = false;

   string getFileName();                           /* get current file name */

   int  col = 1;                                   /* start column of token */
   int  lineno = 1;                                /* line number of token  */
   

%}
  
%x  COMMENT EOLCOMMENT STRING_CHAR

WHITESPACE  [[:space:]]{-}[\n]
EXP         [eE][+-]?[[:digit:]]+
CHAR        [cC]
LONG        [lL]
SIGN        [+|-]?
SIGNED      ({CHAR}|{LONG})
UNSIGNED    ([uU]({CHAR}|{LONG})?)
DECIMAL     {SIGN}[1-9][[:digit:]]*
HEXADECIMAL 0[xX][[:xdigit:]]+
OCTAL       0[0-7]* 
FLOAT1      {SIGN}[[:digit:]]+\.[[:digit:]]*{EXP}?
FLOAT2      {SIGN}\.[[:digit:]]+{EXP}?
FLOAT3      {SIGN}[[:digit:]]+{EXP}
FOLLOW      [,;:\(\)<>/{}|[:cntrl:]| ]
HEXFLOAT    0[xX]1.[:xdigit:]+[[p|P][+|-][0-9]+]?
NAMEEND     ([[:graph:]_\$]{-}[,;:\(\)<>/{}])
NAMESTART   [[:alpha:]_\$]
NAME        {NAMESTART}{NAMEEND}*
FALSE       [fF][aA][lL][sS][eE]
TRUE        [tT][rR][uU][eE]
%%
     bool stringFlag = false;                        /* false; quoted char    */
     string *string_buf = NULL;                      /* input string buffer   */

,   |
;   |
:   |
\(  |
\)  |
\{  |
\}  |
\<  |
\>  |
#                                { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d %c                  %s\n", lineno, col, yytext[0], yytext);) 
                                   col++; 
                                   return yytext[0]; }
\\a                              { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d \\a                %s\n", lineno, col, yytext);) col++; 
                                   yylval->CHAR = '\a';
                                   return TOKEN::CHAR;}
\\b                              { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d \\b                %s\n", lineno, col, yytext);) col++;
                                   yylval->CHAR = '\b';
                                   return TOKEN::CHAR;}
\\f                              { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d \\f                %s\n", lineno, col, yytext);) col++;
                                   yylval->CHAR = '\f';
                                   return TOKEN::CHAR;}
\\n                              { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d \\n                %s\n", lineno, col, yytext);) col++;
                                   yylval->CHAR = '\n';
                                   return TOKEN::CHAR;}
\\r                              { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d \\r                %s\n", lineno, col, yytext);) col++;
                                   yylval->CHAR = '\r';
                                   return TOKEN::CHAR;}
\\t                              { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d \\t                %s\n", lineno, col, yytext);) col++;
                                   yylval->CHAR = '\t';
                                   return TOKEN::CHAR;}
\\.                              { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d \\.                %s\n", lineno, col, yytext);) col++;
                                   yylval->CHAR = '.';
                                   return TOKEN::CHAR;}
\"  |
\'                               { string whoWeAre = (yytext[0] == '"')? "STRING": "CHAR  ";
                                   DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d BEGIN %-s       %s\n", lineno, col, whoWeAre.c_str(), yytext);) 
                                   col++;
                                   string_buf = new string();
                                   stringFlag = (yytext[0] == '"')? true: false;
                                   BEGIN(STRING_CHAR); }
"/*"                             { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d /* BEGIN COMMENT   %s\n", lineno, col, yytext);) col++; 
                                   BEGIN(COMMENT); }
"//"                             { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d EOL COMMENT        %s\n", lineno, col, yytext);) col++; 
                                   BEGIN(EOLCOMMENT); }
     
{WHITESPACE}+                    { col += yyleng; }
\n                               { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d \\n\n", lineno, col); lineno++; col=1;) }
{OCTAL}/{FOLLOW}                 { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d OCTAL              %s\n", lineno, col, yytext);) col += yyleng; 
                                   long int value = strtol(yytext, NULL, 0);
                                   yylval->INTEGER = value;
                                   return TOKEN::INTEGER; }
{OCTAL}{SIGNED}/{FOLLOW}         { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d OCTAL[C|L]         %s\n", lineno, col, yytext);) col += yyleng;
                                   long int value = strtol(yytext, NULL, 0);
                                   yylval->INTEGER = value;
                                   TOKEN::yytokentype token    = TOKEN::INTEGER;
                                   if ((yytext[yyleng-1] | 0x20) == 'c') {
                                       token      = TOKEN::CHAR;
                                       yylval->CHAR = yylval->INTEGER;
                                   }
                                   return token;
                                 }
{OCTAL}{UNSIGNED}/{FOLLOW}       { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d OCTAL[UC|UL]       %s\n", lineno, col, yytext);) col += yyleng;
                                   long unsigned int value = strtoul(yytext, NULL, 0);
                                   yylval->UINTEGER = value;
                                   TOKEN::yytokentype token     = TOKEN::UINTEGER;
                                   if ((yytext[yyleng-1] | 0x20) == 'c') {
                                       token       = TOKEN::UCHAR;
                                       yylval->UCHAR = yylval->UINTEGER;
                                   }
                                   return token;
                                 }
{DECIMAL}/{FOLLOW}               { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d DECIMAL            %s\n", lineno, col, yytext);) col += yyleng;
                                   yylval->INTEGER = atoi(yytext);
                                   return TOKEN::INTEGER; }
{DECIMAL}{SIGNED}/{FOLLOW}       { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d DECIMAL[C|L]       %s\n", lineno, col, yytext);) col += yyleng;
                                   yylval->INTEGER = atoi(yytext);
                                   TOKEN::yytokentype token    = TOKEN::INTEGER;
                                   if ((yytext[yyleng-1] | 0x20) == 'c') {
                                       token      = TOKEN::CHAR;
                                       yylval->CHAR = yylval->INTEGER;
                                   }
                                   return token;
                                 }
{DECIMAL}{UNSIGNED}/{FOLLOW}     { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d DECIMAL[UC|UL]     %s\n", lineno, col, yytext);) col += yyleng;
                                   long unsigned int value = strtoul(yytext, NULL, 0);
                                   yylval->UINTEGER = value;
                                   TOKEN::yytokentype token     = TOKEN::UINTEGER;
                                   if ((yytext[yyleng-1] | 0x20) == 'c') {
                                       token       = TOKEN::UCHAR;
                                       yylval->UCHAR = yylval->UINTEGER;
                                   }
                                   return token;
                                 }
{HEXADECIMAL}/{FOLLOW}           { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d HEXADECIMAL        %s\n", lineno, col, yytext);) col += yyleng;
                                   long int value = strtol(yytext, NULL, 0);
                                   yylval->UINTEGER = value;
                                   return TOKEN::INTEGER; }
{HEXADECIMAL}{SIGNED}/{FOLLOW}   { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d HEXADECIMAL[C|L]   %s\n", lineno, col, yytext);) col += yyleng;
                                   long int value = strtol(yytext, NULL, 0);
                                   yylval->UINTEGER = value;
                                   TOKEN::yytokentype token    = TOKEN::INTEGER;
                                   if ((yytext[yyleng-1] | 0x20) == 'c') {
                                       token      = TOKEN::CHAR;
                                       yylval->CHAR = yylval->INTEGER;
                                   }
                                   return token;
                                 }
{HEXADECIMAL}{UNSIGNED}/{FOLLOW} { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d HEXADECIMAL[UC|UL] %s\n", lineno, col, yytext);) col += yyleng;
                                   long unsigned int value = strtoul(yytext, NULL, 0);
                                   yylval->UINTEGER = value;
                                   TOKEN::yytokentype token     = TOKEN::UINTEGER;
                                   if ((yytext[yyleng-1] | 0x20) == 'c') {
                                       token       = TOKEN::UCHAR;
                                       yylval->UCHAR = yylval->UINTEGER;
                                   }
                                   return token;
                                 }
{FLOAT1}/{FOLLOW}                { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d 1.E1               %s\n", lineno, col, yytext);) col += yyleng;
                                   yylval->FLOAT = atof(yytext);
                                   return TOKEN::FLOAT; }
{FLOAT2}/{FOLLOW}                { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d  .E2               %s\n", lineno, col, yytext);) col += yyleng;
                                   yylval->FLOAT = atof(yytext);
                                   return TOKEN::FLOAT; }
{FLOAT3}/{FOLLOW}                { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d 3E3                %s\n", lineno, col, yytext);) col += yyleng;
                                   yylval->FLOAT = atof(yytext);
                                   return TOKEN::FLOAT; }
{HEXFLOAT}/{FOLLOW}              { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d hexfloat           %s\n", lineno, col, yytext);) col += yyleng;
                                   yylval->FLOAT = ATOF(yytext);
                                   return TOKEN::FLOAT; }
{FALSE}/{FOLLOW}                 { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d FALSE              %s\n", lineno, col, yytext);) col += yyleng;
                                   yylval->BOOL = false;
                                   return TOKEN::BOOL; }
{TRUE}/{FOLLOW}                  { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d TRUE               %s\n", lineno, col, yytext);) col += yyleng;
                                   yylval->BOOL = true;
                                   return TOKEN::BOOL; }
{NAME}/{FOLLOW}                  { 
                                   col += yyleng; 
                                   if (!strcmp("include", yytext)) {
                                      DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d include            %s\n", lineno, col, yytext);) 
                                      return TOKEN::INCLUDE;
                                   } else if (!strcmp("user", yytext)) {
                                      DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d user               %s\n", lineno, col, yytext);) 
                                      return TOKEN::USER;
                                   } else {
                                      DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d NAME               %s\n", lineno, col, yytext);) 
                                      yylval->STRING = new string(yytext);
                                      return TOKEN::NAME;
                                   }
                                 }
.                                { 
                                   /* collect all illegal characters into buffer  */
                                   /* This is the default junk collector. When    */
                                   /* an illegal construct is discovered this     */
                                   /* little jewel puts all illegal characters    */
                                   /* into a buffer, tokenizes it and then ships  */
                                   /* it to the parser.                           */

                                   /**********************************************************************/
                                   /* Character Translation Table: '1' character is not legal.           */
                                   /**********************************************************************/
                                   
                                     static const char xTable[256] =             /* class translation table*/
                                     {
                                   /*
                                   nul  soh  stx  etx  eot  enq  ack  bel  bs   hs   lf   vt   ff   cr   so   si*/
                                     1,   1,   1,   1,   1,   1,   1,   0,   1,   1,  0,   0,   0,   0,   1,   1,
                                   
                                   /*
                                   dle  dc1  dc2  dc3  dc4  nak  syn  etb  can  em   sub  esc  fs   gs   rs   us*/
                                     1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
                                   
                                   /*
                                   sp    !    "    #    $    %    &    '    (    )    *    +    -    ,    .    /*/
                                     0,   1,   1,   1,   1,   1,   1,   1,   0,   0,   1,   1,   1,   0,   1,   0,
                                   
                                   /*
                                    0    1    2    3    4    5    6    7    8    9    :    ;    <    =    >    ?*/
                                     1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   0,   0,   0,   1,   0,   1,
                                   
                                   /*
                                    @    A    B    C    D    E    F    G    H    I    J    K    L    M    N    O*/
                                     1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
                                   
                                   /*
                                    P    Q    R    S    T    U    V    W    X    Y    Z    [    \    ]    ^    _*/
                                     1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
                                   
                                   /*
                                    `    a    b    c    d    e    f    g    h    i    j    k    l    m    n    o*/
                                     1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
                                   
                                   /*
                                    p    q    r    s    t    u    v    w    x    y    z    {    |    }    ~  del*/
                                     1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   0,   1,   0,   1,   1,
                                   
                                   /* the remaining 128 characters are not part of the legal ASCII character set*/
                                     };                                   
                                   char  buffer[256];
                                   char* ptr = buffer;
                                   *ptr = yytext[0];
                                   do {
                                      if (ptr < &buffer[255])
                                         *(++ptr) = yyinput();
                                      else break;
                                   } while (xTable[(int)*ptr]);
                                   unput(*ptr);
                                   *ptr = 0;
                                   printf("Lex: %s:%d.%d illegal character(s) in file \"%s\"\n"
                                         , getFileName().c_str(), lineno, col, buffer);
                                   col += (ptr - buffer);
                                 }
<INITIAL><<EOF>>                 { return TOKEN::END; }

<COMMENT>{
"*"+                             { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d COMMENT ***        `%s`\n", lineno, col, yytext);) col += yyleng; }
.*"*/"                           { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d END COMMENT        `%s`\n", lineno, col, yytext);) col += yyleng; 
                                   BEGIN(INITIAL); }
[^*\n]+                          { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d COMMENT [^*\\n]    `%s`\n", lineno, col, yytext);) col += yyleng; }
<<EOF>>                          { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d COMMENT EOL\n", lineno, col);) lineno++; 
                                   BEGIN(INITIAL);
                                   return TOKEN::END; }
\n                               { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d COMMENT \\n\n", lineno, col);) lineno++; col=1; }
} /* <COMMENT> */


<EOLCOMMENT>{
.**                              { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d EOL COMMENT        `%s`\n", lineno, col, yytext);) col += yyleng; }
<<EOF>>                          { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d END COMMENT \\n\n", lineno, col);) lineno++; col=1; 
                                   BEGIN(INITIAL);
                                   return TOKEN::END; }
\n                               { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d END COMMENT \\n\n", lineno, col);) lineno++; col = 1; 
                                   BEGIN(INITIAL); }
}  /* <EOLCOMMENT> */

<STRING_CHAR>{   
\"/{FOLLOW}                      {  if (stringFlag) {
                                       DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d END STRING         \"%s\"\n", lineno, col, string_buf->c_str());) 
                                       col += yyleng;
                                       BEGIN(INITIAL); 
                                       yylval->STRING = string_buf;
                                       return TOKEN::STRING;
                                    } else {
                                       DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d CHARACTER \"        \'%s\'\n", lineno, col, yytext);) 
                                       col += yyleng;
                                       string_buf->append(yytext);
                                    }
                                 }
                                 
\'/{FOLLOW}                      {  if (!stringFlag) {
                                       DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d END CHARACTER      \'%s\'\n", lineno, col, string_buf->c_str());) 
                                       col += yyleng;
                                       BEGIN(INITIAL); 
                                       if (string_buf->size() > 1) {
                                          printf("Lex: %s:%d.%d converting to string \"%s\"\n"
                                                        , getFileName().c_str(), lineno, col, string_buf->c_str());
                                          yylval->STRING = string_buf;
                                          return TOKEN::STRING;
                                       } else {
                                          yylval->CHAR = (string_buf->c_str())[0];
                                          delete string_buf;
                                          return TOKEN::CHAR;
                                       }
                                    } else {
                                       DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d STRING   '            \"%s\"\n", lineno, col, yytext);) 
                                       col += yyleng;
                                       string_buf->append(yytext);
                                    }
                                 }
                                 
<<EOF>>                          { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d END STRING FAIL    \"%s\"\n", lineno, col, string_buf->c_str());) 
                                   col += yyleng;
                                   printf("Lex: %s:%d.%d EOF file before terminal quote \"%s\": ignored\n"
                                                 , getFileName().c_str(), lineno, col, string_buf->c_str());
                                   BEGIN(INITIAL); 
                                   yylval->STRING = string_buf;
                                   return TOKEN::STRING;
                                 }
                                 
\n                               { col = 1; lineno++; }

\\0[0-7]{1,3}                    { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d \\0[0-7]{1,3}       \"%s\"\n", lineno, col, yytext);) 
                                   col += yyleng;
                                   char result[2] = {0, 0};
                                   (void) sscanf( (yytext + 1), "%o", (unsigned int*)&result[0] );
                                   string_buf->append(result);
                                }
\\x[[:xdigit:]]{1,2}            { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d \\x[[:xdigit:]]{1,2}   \"%s\"\n", lineno, col, yytext);) 
                                   col += yyleng;
                                   char result[2] = {0, 0};
                                   (void) sscanf( (yytext + 2), "%x", (unsigned int*)&result[0] );
                                   string_buf->append(result);
                                 }
     
\\[1-9][[:digit:]]{1,3}          { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d [1-9][[:digit:]]{1,2}   \"%s\"\n", lineno, col, yytext);) 
                                   col += yyleng;
                                   union {
                                     int32_t result;
                                     char    array[5];
                                   } r;
                                   r.array[4] = 0;
                                   (void) sscanf( (yytext + 1), "%d", &r.result );
                                   if (r.result > 0xFF) {
                                     printf("Lex: %s:%d.%d illegal decimal number in file \"%s\": ignored\n"
                                                   , getFileName().c_str(), lineno, col, yytext);
                                   } else string_buf->append(&r.array[0]);
                                 }
     
\\b                             { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d STRING \\b            \"%s\"\n", lineno, col, yytext);) col += yyleng;
                                  string_buf->append("\b");
                                }
\\f                             { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d STRING \\f            \"%s\"\n", lineno, col, yytext);) col += yyleng;
                                  string_buf->append("\f");
                                }
\\n                             { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d STRING \\n            \"%s\"\n", lineno, col, yytext);) col += yyleng;
                                  string_buf->append("\n");
                                }
\\r                             { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d STRING \\r            \"%s\"\n", lineno, col, yytext);) col += yyleng;
                                  string_buf->append("\r");
                                }
\\t                             { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d STRING \\t            \"%s\"\n", lineno, col, yytext);) col += yyleng;
                                  string_buf->append("\t");
                                }
     
\\.                             { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d STRING \\n         \"%s\"\n", lineno, col, yytext);) col += yyleng;
                                  string_buf->append(yytext);
                                }
     
[^\\\n\"\']+                    { DEBUG(lexerDebugFlag, printf("Lex: %3d:%4d STRING [^\\\\n\"]+    \"%s\"\n", lineno, col, yytext);) col += yyleng;
                                  string_buf->append(yytext);
                                }
} /* <STRING_CHAR> */

%%

# include <string>
using namespace std;
/**
 * @file    lex.Slip
 * @author  A. Schwarz
 * @date November 16, 2013
 *
 * @brief Lexer regular expressions and integrated classes.
 *
 */

 /**
 * @brief Instantiation of state push/pop functionality
 */
class LexStateVariables;
LexStateVariables * variables = NULL;

/**
 * @class LexStateVariables
 * @brief Save, manage, and restore lexer state in include file processing.
 *
 *<p>When an include file is seen, the lexer state is saved. When an include
 *   file is exited, the lexer state is restored. The lexer state are those
 *   state variables not managed by the lexer, the current line nunber and
 *   column number.</p>
 * <p>On first entry to the lexer the line number and column number are stacked.
 *    On each instance of an include, the line number and column number are
 *    stacked, and on an include file EOF, the line numbers and column numbers
 *    of the previous context are restored. When processing of the inital
 *    lexer input file is terminated (with and EOF) then the file state is
 *    popped and the lexer exits.</p>
 */
class LexStateVariables {
  /**
   * @brief Structure maintains the lexer state.
   */
   struct Variables {
      int lineno;                                //!< lexor line number
      int col;                                   //!< lexer column number
      FILE * file;                               //!< fopen parameter
      string fileName;                           //!< filename
   }; // struct Variables
   static const int MAX_ARRAY_SIZE = 64;         //!< Initial allocation size
   Variables * stack;                            //!< Pointer to state stack
   int currentAllocation;                        //!< Current size of state stack
   int ndx;                                      //!< Next available state stack location
   public:
   /**
    * @brief Constructor for state stacking class.
    */
   LexStateVariables() : ndx(0)
                       , stack(new Variables[MAX_ARRAY_SIZE]) //!< Allocate initial stack size
                       , currentAllocation(MAX_ARRAY_SIZE)    //!< Initial allocation size
   { }
   
   /*
    * @brief Destructor for state stacking class.
    *
    * <p>The state stack is deleted.</p>
    */
   ~LexStateVariables() { delete stack; }
   
   /**
    * @brief Return the current state stack depth.
    * 
    * @return current depth of stack
    */
   int getDepth() { return ndx; }
   
   /**
    * @brief Return the filename of the currently open context.
    * @return (string) fileName of open context
    */
   string getFileName() { return stack[ndx-1].fileName; }
   
   /**
    * @brief Return the state of the stack;
    * <p>Each time an include file is seen, the current state is
    *    saved onto a stack. When the include file is existed, the
    *    current stack top is popped.</p>
    * <p>Prior to the first time that the lexer is entered, the
    *    current state is saved. This becomes the stack top.
    *    When all include files have been exited, the stack
    *    top is the stack state before first execution of 
    *    the lexer. When this state is popped, the lexer has
    *    seen the last End-Of-File, the End-Of-File of the
    *    initial file being processed. The lexer can not be
    *    entered again.</p>
    * <p>When there are no more entries on the state stack, we
         are done.</p>
    * @return <b>true</b> there are no more entries on the stack
    */
   bool isEmpty() {
      return (ndx <= 0);
   }; // bool isEmpty()
   
   /**
    * @brief Return <b>true</b> if the current state is the initial state.
    * <p>Each time an include file is seen, the current state is
    *    saved onto a stack. When the include file is existed, the
    *    current stack top is popped.</p>
    * <p>Prior to the first time that the lexer is entered, the
    *    current state is saved. This becomes the stack top.
    *    When all include files have been exited, the stack
    *    top is the stack state before first execution of 
    *    the lexer./p>
    * <p>When the current stack entry is the only entry, we are
    *    at the entry state to the lexer.</p>
    * @return <b>true</b> 
    */
   bool isTop() {
      return (ndx == 1);
   }; // bool isTop()
   
   /**
    * @brief Pop the last entry in the state stack.
    *
    * <p>The state, the line number and column number, is returned.
    *    If the stack is empty, the line number and column number
    *    are set to zero but no diagnostic message is output. This
    *    function is meant to perform in a well-behaved environment.</p>
    * @param[out] lineno (int) line number in the current file being scanned
    * @param[out] col (int) column number in the current file being scanned
    */
   void pop(int* lineno, int* col) {
      if (ndx >= 0) {
         toString(ndx-1, "pop");
         *lineno = stack[--ndx].lineno;
         *col    = stack[ndx].col;
      } else {
         *lineno = *col = 0;
      }
      return;
   }; // void push(int lineno, int col)
   
   /**
    * @brief Push a state stack entry.
    * <p>The lexer line number and column number are stacked. If the current
    *    stack is too small to hold another entry then a new stack is allocated
    *    with twice the size of the previous stack, all entries are copied and
    *    the old stack is deleted.</p>
    * @param[in] lineno (int) line number in the current file being scanned
    * @param[in] col (int) column number in the current file being scanned
    * @param[in] file (FILE*) open file object
    * @param[in] fileName (string) ASCII name of file
    */
   void push(int lineno, int col, string fileName, FILE * file) {
      if (ndx >= currentAllocation) {
         Variables * localStack = new Variables[2*currentAllocation];
         int localNdx = 0;
         while(localNdx < currentAllocation) 
            localStack[localNdx] = stack[localNdx++];
         currentAllocation *= 2;
         delete stack;
         stack = localStack;
      }
      stack[ndx].lineno   = lineno;
      stack[ndx].col      = col;
      stack[ndx].file     = file;
      stack[ndx].fileName = fileName;
      ndx++;
      toString(ndx-1, "push");
   }; // void push(int lineno, int col)

   /**
    * @brief Recover the previous lexical state.
    * <p>When an EOF is seen at the end of the lexer input file or
    *    the end of an 'include' file, the previous context is
    *    restored and the currently open file is closed. 
    *    Restoration restores the saved lexer state and
    *    the saved line number and column number.</p>
    *
    * @param[out] lineno (int) line number of previous context
    * @param[out] col (int) column number of previous context
    */
   void restoreLexState(int* lineno, int* col) {
      fclose(stack[ndx-1].file);
      Slippop_buffer_state();
      pop(lineno, col);
      return;
   }; // void restoreLexState()
       
   /*
    * @brief For each include file, stack the current state and open the file.
    *
    * <p>The input file is opened and the lexer state and column number and
    *    line number are stacked.</p>
    *
    * @param[in] fileName (string) include file name
    * @return <b>true</b> open of input file successful
    */
   bool saveLexState(string fileName) {
      bool retval = true;
      errno = 0;
      FILE * file = fopen(fileName.c_str(), "r");
      if (!file) {
         printf("Lexer:saveLexState Open File Error %s\n", strerror(errno));
         errno = 0;
         retval = false;
      } else {
         YY_BUFFER_STATE state = Slip_create_buffer ( file, YY_BUF_SIZE );
         Slippush_buffer_state(state);
         push(lineno, col, fileName, file);
         lineno = col = 1;
      }
      return retval;
   }; // bool saveLexState(string fileName)
   
   /**
    * @brief pretty print an entry from the stack.
    *
    * @param[in] ndx (int) stack entry index
    * @param[in] text (string&) title of output
    */
   void toString(const int ndx, const string& text) {
       DEBUG(lexerDebugFlag, printf("Lex: %4s stack[%d], %s:<%3d %3d>\n"
             , text.c_str(), ndx
             , stack[ndx].fileName.c_str()
             , stack[ndx].lineno
             , stack[ndx].col);)
   }; // void toString(const int ndx, const string& text)
}; // class LexStateVariables

/**
 * @brief Cleanup scanner by deleting allocated storage.
 */
void deleteLexState() {
   if (variables) delete variables;
   while(YY_CURRENT_BUFFER) Slippop_buffer_state ( );
}; // void deleteLexState()

/**
 * @brief Check lexer state for empty;
 * <p>Each time that an include file is processed, the lexer
      state gets stacked. Each time that an End-Of-File for
      the current processing input is reached, the lexer
      pops the current lexer state and processes the previous
      state. When the topmost state is popped, we are done.</p>
 * <p>This function returns <b>true</b> when the topmost lexer
      state has been popped.</p>
 * @return <b>true</b> if lexer topmost lexer state removed
 */
bool isEmpty() {
   return (variables->isEmpty());
}; // bool isEmpty

/**
 * @brief <b>true</b> if this is the initial file being processing.
 * <p>As each include file is processed, the current state is</p>
 *    pushed. As each include file is exited, the current state
 *    is restored to the previous state. If the current state is
 *    the initial state, we are at the top.
 * @return <b>true</b> the initial lexer file is being processed
 */
bool isTop() {
   return variables->isTop();
}; // bool isTop()

/**
 * @brief Get the file name of the current input file.
 * @return (string) file name
 */
string getFileName() {
   return variables->getFileName();
}; // string getFileName()

/**
 * @brief Open a file;
 * @param filename (string) file name and path.
 * @return File descriptor. If <b>null</b> then open failed.
 */
FILE* openFile(string filename) {
   FILE * file = fopen(filename.c_str(), "r");
   if (!file) {
      printf("Lexer:saveLexState Open File Error %s\n", strerror(errno));
   }
   return file;
}; // FILE* openFile(string filename)

/**
 * @brief Restore the previous lexer state.
 * <p>External interface which allows the previous lexer state to be 
 *    restored. Restoration allows the previous context to be recovered
 *    as if an intervening 'include' had not occurred.</p>
 * @note
 * <p>The lineno and col have file scope. They are not part of the 
 *    local scope and their values must be made available to the
 *    lexer. The lexer places the values in file scope and this
 *    little jewel uses it. It is a violation of basic development
 *    principles, but there you have it.</p>
 * @return <b>true</b> restore previous state
 */
bool restoreLexState() {
   bool flag = YY_CURRENT_BUFFER;
   if (!flag) delete variables;
   else variables->restoreLexState(&lineno, &col);
   return flag;
}; // bool restoreLexState()

/**
 * @brief For each include file, stack the current state.
 *
 * <p>This function accesses methods within the scope of the scanner
 *    file. This access to file scoped variables allows which does
 *    not require any file scoped variables allows usage outside of
 *    the scanner file, most particular in the parser. This function
 *    is the glue which allows the parser to perform lexer-centric
 *    operations.</p>
 *
 * @param[in] filename (string) include file name
 */
bool saveLexState(const string filename) {
   FILE* file = openFile(filename);
   if (!file) {
      return false;
   } else {
      if (variables == 0)
         variables = new LexStateVariables();
   }
   return variables->saveLexState(filename);
}; // bool saveLexState(string filename)

/**
 * @brief Turn debugging on.
*/
void setLexerDebugON() {
   lexerDebugFlag = true;
}; // void setLexerDebugON()

/**
 * @brief Turn debugging off.
*/
void setLexerDebugOFF() {
   lexerDebugFlag = false;
}; // void setLexerDebugOFF()

/**
 * @brief Converts a <i>hexfloat</i> into a double.
 * <p>The conforms to the IEEE 754 layout of double precision 64-bit) floating
 *    point numbers. A hardware or software implementation different from this
 *    standard will cause the algorithm to fail.</p>
 * <p>The algorithm assumes that a 64-bit memory representation is available
 *    in the hardware or through the compiler, and that the 64-bits are
 *    contiguous. Any operation made on the 64-bit memory item assumes that
 *    all 64-bits take part therefore a bit shift will shit within a 64-bit
 *    contiguous memory object.</p>
 * <p>It is assumed tht the input ASCII representation is legal. that is that
 *    it conforms exactly to a valid <i>hexfloat</i> representation. With this
 *    in mind:</p>
 * <ul>
 *    <li>Ignore leading non-graphic ASCII characters (except a <b>NUL</b>.</li>
 *    <li>Check for a signed <i>hexfloat</i>.</li>
 *    <ul>
 *       <li>If the leading character is a '+', ignore and increment over it.</li>
 *       <li>If the leading character is a '-', shift a '<b>1</b>' into the
 *           most signmificat bit of the 64-bit number and increment over the
 *           character.</li>
 *    </ul>
 *    <li>Set the mantissa to zero. For all characters in the mantissa:</li>
 *    <ul>
 *       <li>Convert the character into an integer.</li>
 *       <li>Shift the previous mantissa value by 4-bits.</li>
 *       <li>'or' the current value into the mantissa.</li>
 *       <li>Remember how many bits have been shifted.</li>
 *    </ul>
 *    <li>If the number of bits shifted is less than the allowable maximum,
 *        then shift the mantissa. If the number exceed the maximum, ignore,
 *        the returned value will not be legal.</li>
 *    <li>Process the exponent:</li>
 *    <ul>
 *       <li>Skip of the exponent indicator, '<b>p</b>'.</li>
 *       <li>Remember the exponsent sign (+, -).</li>
 *       <li>Construct the exponen valuet. The exponent is a decimal number,
 *           -1023 &lt; x &lt; 1023. </li>
 *       <li>Multiply by the sign, add the bias, and shift.</li>
 *       <li>'or' the exponent in to the double precision integer cognate.</li>
 *    </ul>
 * </ul>
 * @param str (char *) <i>hexfloat</i> string "0x1.#p#"
 * @return floating point value
 */
double myatof(char * str) {

      /**********************************************************************/
      /* Character Translation Table: '255' character is not legal.         */
      /**********************************************************************/

        static const uint8_t xTable[256] =             /* class translation table*/
        {
      /*
      nul  soh  stx  etx  eot  enq  ack  bel  bs   hs   lf   vt   ff   cr   so   si*/
      255, 255, 255, 255, 255, 255, 255, 255, 255, 255,255, 255, 255, 255, 255, 255,

      /*
      dle  dc1  dc2  dc3  dc4  nak  syn  etb  can  em   sub  esc  fs   gs   rs   us*/
      255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,

      /*
      sp    !    "    #    $    %    &    '    (    )    *    +    -    ,    .    /*/
      255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,

      /*
       0    1    2    3    4    5    6    7    8    9    :    ;    <    =    >    ?*/
        0,   1,   2,   3,   4,   5,   6,   7,   8,   9, 255, 255, 255, 255, 255, 255,

      /*
       @    A    B    C    D    E    F    G    H    I    J    K    L    M    N    O*/
      255,  10,  11,  12,  13,  14,  15, 255, 255, 255, 255, 255, 255, 255, 255, 255,

      /*
       P    Q    R    S    T    U    V    W    X    Y    Z    [    \    ]    ^    _*/
      255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,

      /*
       `    a    b    c    d    e    f    g    h    i    j    k    l    m    n    o*/
      255,  10,  11,  12,  13,  14,  15, 255, 255, 255, 255, 255, 255, 255, 255, 255,

      /*
       p    q    r    s    t    u    v    w    x    y    z    {    |    }    ~  del*/
      255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,

      /* the remaining 128 characters are not part of the legal ASCII character set*/
        };
   static const int MANTISSA_SHIFT =    52;
   static const int EXP_MASK       = 0x7ff;
   static const int EXP_BIAS       = 0x3ff;
   union {
      double     dbl;
      uint64_t dblDef;
   } result;
   uint64_t mantissa = 0;
   int      shift    = 0;
   int      digit    = 0;
   char*    p        = str;
   result.dbl = 0.0;
   while((*p > 0) && (*p <= (uint8_t)' ')) p++;
   if (*p != '0') {
      if (*p == '-')
         result.dblDef = (uint64_t)1 << 63;
      p++;
   }
   p += 4;

   while((digit = xTable[(uint8_t)*p]) != 255) {
      mantissa = (mantissa << 4) | digit;
      shift += 4;
      p++;
   }
   if (shift < MANTISSA_SHIFT) mantissa = mantissa << (MANTISSA_SHIFT - shift);
   result.dblDef |= mantissa;
   if (*p == 'p' || *p == 'P') {
      uint64_t exp = 0;
      p++;
      int sign = 1;
      if (*p == '+') p++;
      else if (*p == '-') {
         sign = -1;
         p++;
      }
      while((digit = xTable[(uint8_t)*p]) != 255) {
         exp = exp*10 + digit;
         p++;
      }
      exp   = sign*exp + EXP_BIAS;
      exp  &= EXP_MASK;
      exp <<= MANTISSA_SHIFT;
      result.dblDef |= exp;
   }
   return result.dbl;
}; // myatof
