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
     HACTIVATE = 258,
     HAFTER = 259,
     HARRAY = 260,
     HAT = 261,
     HBEFORE = 262,
     HBEGIN = 263,
     HBOOLEAN = 264,
     HCHARACTER = 265,
     HCLASS = 266,
     HCONC = 267,
     HDELAY = 268,
     HDO = 269,
     HELSE = 270,
     HEND = 271,
     HEQ = 272,
     HEXTERNAL = 273,
     HFOR = 274,
     HGE = 275,
     HGO = 276,
     HGOTO = 277,
     HGT = 278,
     HHIDDEN = 279,
     HIF = 280,
     HIN = 281,
     HINNER = 282,
     HINSPECT = 283,
     HINTEGER = 284,
     HIS = 285,
     HLABEL = 286,
     HLE = 287,
     HLONG = 288,
     HLT = 289,
     HNAME = 290,
     HNE = 291,
     HNEW = 292,
     HNONE = 293,
     HNOTEXT = 294,
     HOTHERWISE = 295,
     HPRIOR = 296,
     HPROCEDURE = 297,
     HPROTECTED = 298,
     HQUA = 299,
     HREACTIVATE = 300,
     HREAL = 301,
     HREF = 302,
     HSHORT = 303,
     HSTEP = 304,
     HSWITCH = 305,
     HTEXT = 306,
     HTHEN = 307,
     HTHIS = 308,
     HTO = 309,
     HUNTIL = 310,
     HVALUE = 311,
     HVAR = 312,
     HVIRTUAL = 313,
     HWHEN = 314,
     HWHILE = 315,
     HASSIGNVALUE = 316,
     HASSIGNREF = 317,
     HPAREXPSEPARATOR = 318,
     HLABELSEPARATOR = 319,
     HSTATEMENTSEPARATOR = 320,
     HBEGPAR = 321,
     HENDPAR = 322,
     HEQR = 323,
     HNER = 324,
     HADD = 325,
     HSUB = 326,
     HMUL = 327,
     HDIV = 328,
     HINTDIV = 329,
     HEXP = 330,
     HDOTDOTDOT = 331,
     HIDENTIFIER = 332,
     HBOOLEANKONST = 333,
     HINTEGERKONST = 334,
     HCHARACTERKONST = 335,
     HREALKONST = 336,
     HTEXTKONST = 337,
     HASSIGN = 338,
     HORELSE = 339,
     HANDTHEN = 340,
     HEQV = 341,
     HIMP = 342,
     HOR = 343,
     HAND = 344,
     HNOT = 345,
     HOBJRELOPERATOR = 346,
     HREFRELOPERATOR = 347,
     HVALRELOPERATOR = 348,
     HTERMOPERATOR = 349,
     UNEAR = 350,
     HFACTOROPERATOR = 351,
     HPRIMARYOPERATOR = 352,
     HDOT = 353
   };
#endif
/* Tokens.  */
#define HACTIVATE 258
#define HAFTER 259
#define HARRAY 260
#define HAT 261
#define HBEFORE 262
#define HBEGIN 263
#define HBOOLEAN 264
#define HCHARACTER 265
#define HCLASS 266
#define HCONC 267
#define HDELAY 268
#define HDO 269
#define HELSE 270
#define HEND 271
#define HEQ 272
#define HEXTERNAL 273
#define HFOR 274
#define HGE 275
#define HGO 276
#define HGOTO 277
#define HGT 278
#define HHIDDEN 279
#define HIF 280
#define HIN 281
#define HINNER 282
#define HINSPECT 283
#define HINTEGER 284
#define HIS 285
#define HLABEL 286
#define HLE 287
#define HLONG 288
#define HLT 289
#define HNAME 290
#define HNE 291
#define HNEW 292
#define HNONE 293
#define HNOTEXT 294
#define HOTHERWISE 295
#define HPRIOR 296
#define HPROCEDURE 297
#define HPROTECTED 298
#define HQUA 299
#define HREACTIVATE 300
#define HREAL 301
#define HREF 302
#define HSHORT 303
#define HSTEP 304
#define HSWITCH 305
#define HTEXT 306
#define HTHEN 307
#define HTHIS 308
#define HTO 309
#define HUNTIL 310
#define HVALUE 311
#define HVAR 312
#define HVIRTUAL 313
#define HWHEN 314
#define HWHILE 315
#define HASSIGNVALUE 316
#define HASSIGNREF 317
#define HPAREXPSEPARATOR 318
#define HLABELSEPARATOR 319
#define HSTATEMENTSEPARATOR 320
#define HBEGPAR 321
#define HENDPAR 322
#define HEQR 323
#define HNER 324
#define HADD 325
#define HSUB 326
#define HMUL 327
#define HDIV 328
#define HINTDIV 329
#define HEXP 330
#define HDOTDOTDOT 331
#define HIDENTIFIER 332
#define HBOOLEANKONST 333
#define HINTEGERKONST 334
#define HCHARACTERKONST 335
#define HREALKONST 336
#define HTEXTKONST 337
#define HASSIGN 338
#define HORELSE 339
#define HANDTHEN 340
#define HEQV 341
#define HIMP 342
#define HOR 343
#define HAND 344
#define HNOT 345
#define HOBJRELOPERATOR 346
#define HREFRELOPERATOR 347
#define HVALRELOPERATOR 348
#define HTERMOPERATOR 349
#define UNEAR 350
#define HFACTOROPERATOR 351
#define HPRIMARYOPERATOR 352
#define HDOT 353




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 99 "../../src/parser.y"
{
	long token;
	long ival;
        long arrdim;
	double rval;
	char *ident;
	char *tval;
	char stat_decl;
  	char kind;
       }
/* Line 1529 of yacc.c.  */
#line 256 "../../src/parser.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

