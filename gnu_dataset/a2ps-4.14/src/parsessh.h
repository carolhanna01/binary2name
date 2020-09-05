/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

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
     tSTYLE = 258,
     tIS = 259,
     tEND = 260,
     tKEYWORDS = 261,
     tARE = 262,
     tIN = 263,
     tOPERATORS = 264,
     tSEQUENCES = 265,
     tFIRST = 266,
     tSECOND = 267,
     tALPHABET = 268,
     tALPHABETS = 269,
     tDOCUMENTATION = 270,
     tEXCEPTIONS = 271,
     tCASE = 272,
     tCSTRING = 273,
     tCCHAR = 274,
     tOPTIONAL = 275,
     tCLOSERS = 276,
     tWRITTEN = 277,
     tBY = 278,
     tVERSION = 279,
     tREQUIRES = 280,
     tA2PS = 281,
     tANCESTORS = 282,
     tFACE = 283,
     tFFLAGS = 284,
     tSTRING = 285,
     tLATEXSYMBOL = 286,
     tREGEX = 287,
     tSENSITIVENESS = 288,
     tBACK_REF = 289
   };
#endif
/* Tokens.  */
#define tSTYLE 258
#define tIS 259
#define tEND 260
#define tKEYWORDS 261
#define tARE 262
#define tIN 263
#define tOPERATORS 264
#define tSEQUENCES 265
#define tFIRST 266
#define tSECOND 267
#define tALPHABET 268
#define tALPHABETS 269
#define tDOCUMENTATION 270
#define tEXCEPTIONS 271
#define tCASE 272
#define tCSTRING 273
#define tCCHAR 274
#define tOPTIONAL 275
#define tCLOSERS 276
#define tWRITTEN 277
#define tBY 278
#define tVERSION 279
#define tREQUIRES 280
#define tA2PS 281
#define tANCESTORS 282
#define tFACE 283
#define tFFLAGS 284
#define tSTRING 285
#define tLATEXSYMBOL 286
#define tREGEX 287
#define tSENSITIVENESS 288
#define tBACK_REF 289




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 81 "parsessh.y"
{
  int integer;
  uchar * string;
  struct pattern * pattern;
  struct style_sheet * sheet;
  struct rule * rule;
  struct sequence * sequence;
  struct darray * array;
  struct words * words;
  struct faced_string * faced_string;
  enum face_e face;			/* Face			*/
  enum fflag_e fflags;			/* Flags for faces	*/
  struct fface_s fface;			/* Flagged face		*/
  enum case_sensitiveness sensitiveness;
}
/* Line 1489 of yacc.c.  */
#line 133 "parsessh.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

