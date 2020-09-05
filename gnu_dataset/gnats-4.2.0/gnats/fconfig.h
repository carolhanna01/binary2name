
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton interface for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     FIELD = 258,
     STRINGTYPE = 259,
     QDEFAULT = 260,
     MATCHING = 261,
     ENUM = 262,
     MULTIENUMTOK = 263,
     VALUES = 264,
     DEFAULT = 265,
     EXACT_REGEXP = 266,
     INEXACT_REGEXP = 267,
     ALL = 268,
     FORMAT = 269,
     ENUMSEPARATORSTOK = 270,
     MULTITEXTTYPE = 271,
     DATETYPE = 272,
     ENUM_IN_FILE = 273,
     MULTI_ENUM_IN_FILE = 274,
     PATHTOK = 275,
     FIELDSTOK = 276,
     KEYTOK = 277,
     QSTRING = 278,
     INTVAL = 279,
     TEXTSEARCH = 280,
     QUERYTOK = 281,
     FORMATTOK = 282,
     INDEXTOK = 283,
     SEPARATORTOK = 284,
     RESTRICTEDTOK = 285,
     NOSPACESTOK = 286,
     INTEGERTOK = 287,
     INPUTDEFAULTTOK = 288,
     BUILTINTOK = 289,
     ALLOWANYVALUETOK = 290,
     REQUIRETOK = 291,
     APPENDFIELDTOK = 292,
     SETFIELDTOK = 293,
     CHANGETOK = 294,
     DESCRIPTIONTOK = 295,
     INPUTTOK = 296,
     DATABASEINFOTOK = 297,
     DEBUGMODETOK = 298,
     KEEPRECTOK = 299,
     NOTIFYEXPTOK = 300,
     LIBEXECDIRTOK = 301,
     SUBMITTERACKTOK = 302,
     BUSINESSDAYTOK = 303,
     BUSINESSWEEKTOK = 304,
     CREATECATEGORYDIRSTOK = 305,
     FALSETOK = 306,
     TRUETOK = 307,
     MAILFORMATTOK = 308,
     TOADDRESSESTOK = 309,
     FROMADDRESSTOK = 310,
     REPLYTOTOK = 311,
     FIXEDTOK = 312,
     BODYTOK = 313,
     HEADERTOK = 314,
     AUDITTRAILFMTTOK = 315,
     ADDAUDITTRAILTOK = 316,
     REQUIRECHANGEREASONTOK = 317,
     READONLYTOK = 318,
     BINARYINDEXTOK = 319,
     RAWTOK = 320,
     BADTOK = 321,
     AUXFLAGSTOK = 322,
     PRLISTTOK = 323,
     MAXPRSTOK = 324,
     EDITONLYTOK = 325,
     VIRTUALFORMATTOK = 326,
     CATPERMSTOK = 327
   };
#endif
/* Tokens.  */
#define FIELD 258
#define STRINGTYPE 259
#define QDEFAULT 260
#define MATCHING 261
#define ENUM 262
#define MULTIENUMTOK 263
#define VALUES 264
#define DEFAULT 265
#define EXACT_REGEXP 266
#define INEXACT_REGEXP 267
#define ALL 268
#define FORMAT 269
#define ENUMSEPARATORSTOK 270
#define MULTITEXTTYPE 271
#define DATETYPE 272
#define ENUM_IN_FILE 273
#define MULTI_ENUM_IN_FILE 274
#define PATHTOK 275
#define FIELDSTOK 276
#define KEYTOK 277
#define QSTRING 278
#define INTVAL 279
#define TEXTSEARCH 280
#define QUERYTOK 281
#define FORMATTOK 282
#define INDEXTOK 283
#define SEPARATORTOK 284
#define RESTRICTEDTOK 285
#define NOSPACESTOK 286
#define INTEGERTOK 287
#define INPUTDEFAULTTOK 288
#define BUILTINTOK 289
#define ALLOWANYVALUETOK 290
#define REQUIRETOK 291
#define APPENDFIELDTOK 292
#define SETFIELDTOK 293
#define CHANGETOK 294
#define DESCRIPTIONTOK 295
#define INPUTTOK 296
#define DATABASEINFOTOK 297
#define DEBUGMODETOK 298
#define KEEPRECTOK 299
#define NOTIFYEXPTOK 300
#define LIBEXECDIRTOK 301
#define SUBMITTERACKTOK 302
#define BUSINESSDAYTOK 303
#define BUSINESSWEEKTOK 304
#define CREATECATEGORYDIRSTOK 305
#define FALSETOK 306
#define TRUETOK 307
#define MAILFORMATTOK 308
#define TOADDRESSESTOK 309
#define FROMADDRESSTOK 310
#define REPLYTOTOK 311
#define FIXEDTOK 312
#define BODYTOK 313
#define HEADERTOK 314
#define AUDITTRAILFMTTOK 315
#define ADDAUDITTRAILTOK 316
#define REQUIRECHANGEREASONTOK 317
#define READONLYTOK 318
#define BINARYINDEXTOK 319
#define RAWTOK 320
#define BADTOK 321
#define AUXFLAGSTOK 322
#define PRLISTTOK 323
#define MAXPRSTOK 324
#define EDITONLYTOK 325
#define VIRTUALFORMATTOK 326
#define CATPERMSTOK 327




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 1676 of yacc.c  */
#line 25 "fconfig.y"

  int intval;
  char *sval;
  struct qstring *qstr;
  AdmFieldDesc *adm_field_des;
  FieldList flist;
  StringList *stringlist;
  InputTemplate *inputlist;
  MailAddress *mailaddr;
  MailAddressList *mailaddrlist;



/* Line 1676 of yacc.c  */
#line 210 "fconfig.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE fconflval;


