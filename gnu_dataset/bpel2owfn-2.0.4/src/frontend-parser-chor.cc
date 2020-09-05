/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0

/* Substitute the variable and function names.  */
#define yyparse frontend_chor_parse
#define yylex   frontend_chor_lex
#define yyerror frontend_chor_error
#define yylval  frontend_chor_lval
#define yychar  frontend_chor_char
#define yydebug frontend_chor_debug
#define yynerrs frontend_chor_nerrs


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     APOSTROPHE = 258,
     EQUAL = 259,
     GREATER = 260,
     GREATEROREQUAL = 261,
     K_AND = 262,
     K_ASSIGN = 263,
     K_BRANCHES = 264,
     K_CASE = 265,
     K_CATCH = 266,
     K_CATCHALL = 267,
     K_COMPENSATE = 268,
     K_COMPENSATESCOPE = 269,
     K_COMPENSATIONHANDLER = 270,
     K_COMPLETIONCONDITION = 271,
     K_CONDITION = 272,
     K_COPY = 273,
     K_CORRELATION = 274,
     K_CORRELATIONS = 275,
     K_CORRELATIONSET = 276,
     K_CORRELATIONSETS = 277,
     K_ELSE = 278,
     K_ELSEIF = 279,
     K_EMPTY = 280,
     K_EVENTHANDLERS = 281,
     K_EXIT = 282,
     K_EXTENSION = 283,
     K_EXTENSIONACTIVITY = 284,
     K_EXTENSIONASSIGNOPERATION = 285,
     K_EXTENSIONS = 286,
     K_FAULTHANDLERS = 287,
     K_FINALCOUNTERVALUE = 288,
     K_FLOW = 289,
     K_FOR = 290,
     K_FOREACH = 291,
     K_FROM = 292,
     K_FROMPART = 293,
     K_FROMPARTS = 294,
     K_GETLINKSTATUS = 295,
     K_IF = 296,
     K_IMPORT = 297,
     K_INVOKE = 298,
     K_JOINCONDITION = 299,
     K_LINK = 300,
     K_LINKS = 301,
     K_LITERAL = 302,
     K_MESSAGEEXCHANGE = 303,
     K_MESSAGEEXCHANGES = 304,
     K_ONALARM = 305,
     K_ONEVENT = 306,
     K_ONMESSAGE = 307,
     K_OPAQUEACTIVITY = 308,
     K_OPAQUEFROM = 309,
     K_OR = 310,
     K_OTHERWISE = 311,
     K_PARTNER = 312,
     K_PARTNERLINK = 313,
     K_PARTNERLINKS = 314,
     K_PARTNERS = 315,
     K_PICK = 316,
     K_PROCESS = 317,
     K_QUERY = 318,
     K_RECEIVE = 319,
     K_REPEATEVERY = 320,
     K_REPEATUNTIL = 321,
     K_REPLY = 322,
     K_RETHROW = 323,
     K_SCOPE = 324,
     K_SEQUENCE = 325,
     K_SOURCE = 326,
     K_SOURCES = 327,
     K_STARTCOUNTERVALUE = 328,
     K_SWITCH = 329,
     K_TARGET = 330,
     K_TARGETS = 331,
     K_TERMINATE = 332,
     K_TERMINATIONHANDLER = 333,
     K_THROW = 334,
     K_TO = 335,
     K_TOPART = 336,
     K_TOPARTS = 337,
     K_TRANSITIONCONDITION = 338,
     K_UNTIL = 339,
     K_VALIDATE = 340,
     K_VARIABLE = 341,
     K_VARIABLES = 342,
     K_WAIT = 343,
     K_WHILE = 344,
     LBRACKET = 345,
     LESS = 346,
     LESSOREQUAL = 347,
     NOTEQUAL = 348,
     RBRACKET = 349,
     X_CLOSE = 350,
     X_EQUALS = 351,
     X_NEXT = 352,
     X_OPEN = 353,
     X_SLASH = 354,
     K_TOPOLOGY = 355,
     K_PARTICIPANTTYPES = 356,
     K_PARTICIPANTTYPE = 357,
     K_PARTICIPANTS = 358,
     K_PARTICIPANT = 359,
     K_PARTICIPANTSET = 360,
     K_MESSAGELINKS = 361,
     K_MESSAGELINK = 362,
     K_TYPES = 363,
     K_PORTTYPE = 364,
     K_FAULT = 365,
     K_OPERATION = 366,
     K_DEFINITIONS = 367,
     K_OUTPUT = 368,
     K_INPUT = 369,
     K_MESSAGE = 370,
     K_PART = 371,
     K_BINDING = 372,
     K_SERVICE = 373,
     K_PORT = 374,
     K_PARTNERLINKTYPE = 375,
     K_ROLE = 376,
     K_SCHEMA = 377,
     K_PROPERTY = 378,
     K_PROPERTYALIAS = 379,
     P_NET = 380,
     P_PLACE = 381,
     P_GRAPHICS = 382,
     P_NAME = 383,
     P_DESCRIPTION = 384,
     P_TRANSITION = 385,
     P_PAGE = 386,
     P_POSITION = 387,
     P_TEXT = 388,
     P_INITIALMARKING = 389,
     P_DIMENSION = 390,
     P_PNML = 391,
     P_ARC = 392,
     P_INSCRIPTION = 393,
     P_OFFSET = 394,
     P_REFERENCEPLACE = 395,
     P_TYPE = 396,
     P_TRANSFORMATION = 397,
     P_TOOLSPECIFIC = 398,
     NUMBER = 399,
     X_NAME = 400,
     VARIABLENAME = 401,
     X_STRING = 402,
     X_TEXT = 403
   };
#endif
/* Tokens.  */
#define APOSTROPHE 258
#define EQUAL 259
#define GREATER 260
#define GREATEROREQUAL 261
#define K_AND 262
#define K_ASSIGN 263
#define K_BRANCHES 264
#define K_CASE 265
#define K_CATCH 266
#define K_CATCHALL 267
#define K_COMPENSATE 268
#define K_COMPENSATESCOPE 269
#define K_COMPENSATIONHANDLER 270
#define K_COMPLETIONCONDITION 271
#define K_CONDITION 272
#define K_COPY 273
#define K_CORRELATION 274
#define K_CORRELATIONS 275
#define K_CORRELATIONSET 276
#define K_CORRELATIONSETS 277
#define K_ELSE 278
#define K_ELSEIF 279
#define K_EMPTY 280
#define K_EVENTHANDLERS 281
#define K_EXIT 282
#define K_EXTENSION 283
#define K_EXTENSIONACTIVITY 284
#define K_EXTENSIONASSIGNOPERATION 285
#define K_EXTENSIONS 286
#define K_FAULTHANDLERS 287
#define K_FINALCOUNTERVALUE 288
#define K_FLOW 289
#define K_FOR 290
#define K_FOREACH 291
#define K_FROM 292
#define K_FROMPART 293
#define K_FROMPARTS 294
#define K_GETLINKSTATUS 295
#define K_IF 296
#define K_IMPORT 297
#define K_INVOKE 298
#define K_JOINCONDITION 299
#define K_LINK 300
#define K_LINKS 301
#define K_LITERAL 302
#define K_MESSAGEEXCHANGE 303
#define K_MESSAGEEXCHANGES 304
#define K_ONALARM 305
#define K_ONEVENT 306
#define K_ONMESSAGE 307
#define K_OPAQUEACTIVITY 308
#define K_OPAQUEFROM 309
#define K_OR 310
#define K_OTHERWISE 311
#define K_PARTNER 312
#define K_PARTNERLINK 313
#define K_PARTNERLINKS 314
#define K_PARTNERS 315
#define K_PICK 316
#define K_PROCESS 317
#define K_QUERY 318
#define K_RECEIVE 319
#define K_REPEATEVERY 320
#define K_REPEATUNTIL 321
#define K_REPLY 322
#define K_RETHROW 323
#define K_SCOPE 324
#define K_SEQUENCE 325
#define K_SOURCE 326
#define K_SOURCES 327
#define K_STARTCOUNTERVALUE 328
#define K_SWITCH 329
#define K_TARGET 330
#define K_TARGETS 331
#define K_TERMINATE 332
#define K_TERMINATIONHANDLER 333
#define K_THROW 334
#define K_TO 335
#define K_TOPART 336
#define K_TOPARTS 337
#define K_TRANSITIONCONDITION 338
#define K_UNTIL 339
#define K_VALIDATE 340
#define K_VARIABLE 341
#define K_VARIABLES 342
#define K_WAIT 343
#define K_WHILE 344
#define LBRACKET 345
#define LESS 346
#define LESSOREQUAL 347
#define NOTEQUAL 348
#define RBRACKET 349
#define X_CLOSE 350
#define X_EQUALS 351
#define X_NEXT 352
#define X_OPEN 353
#define X_SLASH 354
#define K_TOPOLOGY 355
#define K_PARTICIPANTTYPES 356
#define K_PARTICIPANTTYPE 357
#define K_PARTICIPANTS 358
#define K_PARTICIPANT 359
#define K_PARTICIPANTSET 360
#define K_MESSAGELINKS 361
#define K_MESSAGELINK 362
#define K_TYPES 363
#define K_PORTTYPE 364
#define K_FAULT 365
#define K_OPERATION 366
#define K_DEFINITIONS 367
#define K_OUTPUT 368
#define K_INPUT 369
#define K_MESSAGE 370
#define K_PART 371
#define K_BINDING 372
#define K_SERVICE 373
#define K_PORT 374
#define K_PARTNERLINKTYPE 375
#define K_ROLE 376
#define K_SCHEMA 377
#define K_PROPERTY 378
#define K_PROPERTYALIAS 379
#define P_NET 380
#define P_PLACE 381
#define P_GRAPHICS 382
#define P_NAME 383
#define P_DESCRIPTION 384
#define P_TRANSITION 385
#define P_PAGE 386
#define P_POSITION 387
#define P_TEXT 388
#define P_INITIALMARKING 389
#define P_DIMENSION 390
#define P_PNML 391
#define P_ARC 392
#define P_INSCRIPTION 393
#define P_OFFSET 394
#define P_REFERENCEPLACE 395
#define P_TYPE 396
#define P_TRANSFORMATION 397
#define P_TOOLSPECIFIC 398
#define NUMBER 399
#define X_NAME 400
#define VARIABLENAME 401
#define X_STRING 402
#define X_TEXT 403




/* Copy the first part of user declarations.  */
#line 26 "frontend-parser-chor.yy"

/*!
 * \file    frontend-parser-chor.cc
 *
 * \brief   BPEL4Chor topology parser
 *
 * \author  Niels Lohmann <nlohmann@informatik.hu-berlin.de>,
 *          last changes of: \$Author: znamirow $
 *
 * \since   2007/04/29
 *
 * \date    \$Date: 2007/07/25 09:28:45 $
 * 
 * \note    This file is part of the tool BPEL2oWFN and was created during the
 *          project "Tools4BPEL" at the Humboldt-Universität zu Berlin. See
 *          http://www.informatik.hu-berlin.de/top/tools4bpel for details.
 *
 * \note    This file was created using GNU Bison reading file
 *          frontend-parser-chor.yy.
 *          See http://www.gnu.org/software/bison/bison.html for details
 *
 * \version \$Revision: 1.16 $
 *
 * \ingroup frontend
 */
#line 118 "frontend-parser-chor.yy"

#include <cassert>
#include <map>

#include "ast-config.h"
#include "helpers.h"
#include "globals.h"
#include "extension-chor.h"


using std::cerr;
using std::endl;



/******************************************************************************
 * External variables
 *****************************************************************************/

extern int frontend_lex();	// from flex: the lexer funtion
extern int frontend_lineno;

// use the functions of the BPEL parser
#define frontend_chor_lex() frontend_lex()
#define frontend_chor_error(a) frontend_error(a)
#define frontend_chor_in frontend_in


/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 1
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 464 "frontend-parser-chor.cc"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  4
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   62

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  149
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  15
/* YYNRULES -- Number of rules.  */
#define YYNRULES  23
/* YYNRULES -- Number of states.  */
#define YYNSTATES  68

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   403

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint8 yyprhs[] =
{
       0,     0,     3,    14,    22,    24,    28,    32,    40,    42,
      44,    48,    52,    58,    62,    63,    72,    76,    84,    86,
      90,    91,    96,    97
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
     150,     0,    -1,    98,   100,   163,    97,   151,   154,   159,
      99,   100,    95,    -1,   101,    97,   152,    97,    99,   101,
      97,    -1,   153,    -1,   152,    97,   153,    -1,   102,   163,
      99,    -1,   103,    97,   155,    97,    99,   103,    97,    -1,
     156,    -1,   157,    -1,   155,    97,   156,    -1,   155,    97,
     157,    -1,   104,   163,    97,    99,   104,    -1,   104,   163,
      99,    -1,    -1,   105,   163,   158,    97,   155,    97,    99,
     105,    -1,   105,   163,    99,    -1,   106,    97,   160,    97,
      99,   106,    97,    -1,   161,    -1,   160,    97,   161,    -1,
      -1,   107,   162,   163,    99,    -1,    -1,   145,    96,   147,
     163,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,   153,   153,   163,   167,   168,   172,   182,   186,   187,
     188,   189,   193,   195,   201,   200,   204,   213,   217,   218,
     222,   222,   232,   234
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "APOSTROPHE", "EQUAL", "GREATER",
  "GREATEROREQUAL", "K_AND", "K_ASSIGN", "K_BRANCHES", "K_CASE", "K_CATCH",
  "K_CATCHALL", "K_COMPENSATE", "K_COMPENSATESCOPE",
  "K_COMPENSATIONHANDLER", "K_COMPLETIONCONDITION", "K_CONDITION",
  "K_COPY", "K_CORRELATION", "K_CORRELATIONS", "K_CORRELATIONSET",
  "K_CORRELATIONSETS", "K_ELSE", "K_ELSEIF", "K_EMPTY", "K_EVENTHANDLERS",
  "K_EXIT", "K_EXTENSION", "K_EXTENSIONACTIVITY",
  "K_EXTENSIONASSIGNOPERATION", "K_EXTENSIONS", "K_FAULTHANDLERS",
  "K_FINALCOUNTERVALUE", "K_FLOW", "K_FOR", "K_FOREACH", "K_FROM",
  "K_FROMPART", "K_FROMPARTS", "K_GETLINKSTATUS", "K_IF", "K_IMPORT",
  "K_INVOKE", "K_JOINCONDITION", "K_LINK", "K_LINKS", "K_LITERAL",
  "K_MESSAGEEXCHANGE", "K_MESSAGEEXCHANGES", "K_ONALARM", "K_ONEVENT",
  "K_ONMESSAGE", "K_OPAQUEACTIVITY", "K_OPAQUEFROM", "K_OR", "K_OTHERWISE",
  "K_PARTNER", "K_PARTNERLINK", "K_PARTNERLINKS", "K_PARTNERS", "K_PICK",
  "K_PROCESS", "K_QUERY", "K_RECEIVE", "K_REPEATEVERY", "K_REPEATUNTIL",
  "K_REPLY", "K_RETHROW", "K_SCOPE", "K_SEQUENCE", "K_SOURCE", "K_SOURCES",
  "K_STARTCOUNTERVALUE", "K_SWITCH", "K_TARGET", "K_TARGETS",
  "K_TERMINATE", "K_TERMINATIONHANDLER", "K_THROW", "K_TO", "K_TOPART",
  "K_TOPARTS", "K_TRANSITIONCONDITION", "K_UNTIL", "K_VALIDATE",
  "K_VARIABLE", "K_VARIABLES", "K_WAIT", "K_WHILE", "LBRACKET", "LESS",
  "LESSOREQUAL", "NOTEQUAL", "RBRACKET", "X_CLOSE", "X_EQUALS", "X_NEXT",
  "X_OPEN", "X_SLASH", "K_TOPOLOGY", "K_PARTICIPANTTYPES",
  "K_PARTICIPANTTYPE", "K_PARTICIPANTS", "K_PARTICIPANT",
  "K_PARTICIPANTSET", "K_MESSAGELINKS", "K_MESSAGELINK", "K_TYPES",
  "K_PORTTYPE", "K_FAULT", "K_OPERATION", "K_DEFINITIONS", "K_OUTPUT",
  "K_INPUT", "K_MESSAGE", "K_PART", "K_BINDING", "K_SERVICE", "K_PORT",
  "K_PARTNERLINKTYPE", "K_ROLE", "K_SCHEMA", "K_PROPERTY",
  "K_PROPERTYALIAS", "P_NET", "P_PLACE", "P_GRAPHICS", "P_NAME",
  "P_DESCRIPTION", "P_TRANSITION", "P_PAGE", "P_POSITION", "P_TEXT",
  "P_INITIALMARKING", "P_DIMENSION", "P_PNML", "P_ARC", "P_INSCRIPTION",
  "P_OFFSET", "P_REFERENCEPLACE", "P_TYPE", "P_TRANSFORMATION",
  "P_TOOLSPECIFIC", "NUMBER", "X_NAME", "VARIABLENAME", "X_STRING",
  "X_TEXT", "$accept", "tTopology", "tParticipantTypes",
  "tParticipantType_list", "tParticipantType", "tParticipants",
  "tParticipant_list", "tParticipant", "tParticipantSet", "@1",
  "tMessageLinks", "tMessageLink_list", "tMessageLink", "@2",
  "arbitraryAttributes", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385,   386,   387,   388,   389,   390,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   402,   403
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,   149,   150,   151,   152,   152,   153,   154,   155,   155,
     155,   155,   156,   156,   158,   157,   157,   159,   160,   160,
     162,   161,   163,   163
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,    10,     7,     1,     3,     3,     7,     1,     1,
       3,     3,     5,     3,     0,     8,     3,     7,     1,     3,
       0,     4,     0,     4
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,    22,     1,     0,     0,     0,     0,    22,
       0,     0,    23,     0,     0,     0,    22,     0,     4,     0,
       0,     0,     0,     0,    22,    22,     0,     8,     9,     0,
       0,     6,     0,     5,     0,    14,     0,    20,     0,    18,
       0,     0,     0,    13,    16,     0,     0,    10,    11,    22,
       0,     2,     3,     0,     0,     0,     0,     0,    19,    12,
       0,     7,    21,     0,     0,    17,     0,    15
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     2,    11,    17,    18,    15,    26,    27,    28,    45,
      21,    38,    39,    49,     6
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -124
static const yytype_int8 yypact[] =
{
     -86,   -77,    18,  -123,  -124,   -72,   -71,  -122,   -74,  -123,
     -69,   -70,  -124,   -68,   -66,   -67,  -123,   -65,  -124,   -99,
     -62,   -63,   -61,   -82,  -123,  -123,   -60,  -124,  -124,   -64,
     -59,  -124,   -57,  -124,   -78,   -54,   -95,  -124,   -55,  -124,
     -49,   -50,   -51,  -124,  -124,   -48,   -53,  -124,  -124,  -123,
     -96,  -124,  -124,   -52,   -99,   -46,   -45,   -47,  -124,  -124,
     -44,  -124,  -124,   -42,   -91,  -124,   -43,  -124
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
    -124,  -124,  -124,  -124,    33,  -124,     3,   -35,   -34,  -124,
    -124,  -124,     8,  -124,    -9
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint8 yytable[] =
{
      12,    47,    48,    57,    46,    24,    25,    22,    66,    24,
      25,    37,     1,    24,    25,    34,    35,    32,     4,    42,
      16,    43,     5,     3,     7,     9,     8,    10,    13,    47,
      48,    19,    23,    14,    16,    29,    30,    36,    31,    20,
      56,    40,    50,    37,    41,    44,    51,    52,    53,    54,
      55,    61,    59,    64,    62,    65,    33,    60,    58,    63,
       0,     0,    67
};

static const yytype_int16 yycheck[] =
{
       9,    36,    36,    99,    99,   104,   105,    16,    99,   104,
     105,   107,    98,   104,   105,    24,    25,    99,     0,    97,
     102,    99,   145,   100,    96,   147,    97,   101,    97,    64,
      64,    97,    97,   103,   102,    97,    99,    97,    99,   106,
      49,   100,    97,   107,   101,    99,    95,    97,    99,    97,
     103,    97,   104,    97,    99,    97,    23,    54,    50,   106,
      -1,    -1,   105
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    98,   150,   100,     0,   145,   163,    96,    97,   147,
     101,   151,   163,    97,   103,   154,   102,   152,   153,    97,
     106,   159,   163,    97,   104,   105,   155,   156,   157,    97,
      99,    99,    99,   153,   163,   163,    97,   107,   160,   161,
     100,   101,    97,    99,    99,   158,    99,   156,   157,   162,
      97,    95,    97,    99,    97,   103,   163,    99,   161,   104,
     155,    97,    99,   106,    97,    97,    99,   105
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 154 "frontend-parser-chor.yy"
    { globals::BPEL4ChorInfo.print_information(); }
    break;

  case 6:
#line 173 "frontend-parser-chor.yy"
    { globals::BPEL4ChorInfo.add_participantType(globals::tempAttributes); }
    break;

  case 12:
#line 194 "frontend-parser-chor.yy"
    { globals::BPEL4ChorInfo.add_participant(globals::tempAttributes); }
    break;

  case 13:
#line 196 "frontend-parser-chor.yy"
    { globals::BPEL4ChorInfo.add_participant(globals::tempAttributes); }
    break;

  case 14:
#line 201 "frontend-parser-chor.yy"
    { globals::BPEL4ChorInfo.start_participantSet(globals::tempAttributes); }
    break;

  case 15:
#line 203 "frontend-parser-chor.yy"
    { globals::BPEL4ChorInfo.end_participantSet(); }
    break;

  case 20:
#line 222 "frontend-parser-chor.yy"
    { globals::tempAttributes.clear(); }
    break;

  case 21:
#line 223 "frontend-parser-chor.yy"
    { globals::BPEL4ChorInfo.add_messageLink(globals::tempAttributes);
      globals::tempAttributes.clear(); }
    break;

  case 23:
#line 235 "frontend-parser-chor.yy"
    { globals::tempAttributes[strip_namespace((yyvsp[(1) - (4)].yt_casestring)->name)] = strip_namespace((yyvsp[(3) - (4)].yt_casestring)->name); }
    break;


/* Line 1267 of yacc.c.  */
#line 1803 "frontend-parser-chor.cc"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}



