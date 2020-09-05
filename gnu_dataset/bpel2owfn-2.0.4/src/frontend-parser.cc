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
#define yyparse frontend_parse
#define yylex   frontend_lex
#define yyerror frontend_error
#define yylval  frontend_lval
#define yychar  frontend_char
#define yydebug frontend_debug
#define yynerrs frontend_nerrs


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
#line 26 "frontend-parser.yy"

/*!
 * \file    frontend-parser.cc
 *
 * \brief   BPEL parser
 *
 *          This file defines and implements the grammar of BPEL using standard
 *          BNF-rules to describe the originally XML-based syntax as it is
 *          specified in the WS-BPEL 2.0 and the BPEL4WS 1.1 specification. All
 *          terminals are passed from the lexer (implemented in
 *          \ref frontend-lexer.cc). Besides simple syntax-checking the grammar
 *          is used to build the abstract syntax tree as it is defined in
 *          ast-grammar.k.
 * 
 * \author  Niels Lohmann <nlohmann@informatik.hu-berlin.de>,
 *          Christian Gierds <gierds@informatik.hu-berlin.de>,
 *          Martin Znamirowski <znamirow@informatik.hu-berlin.de>,
 *          last changes of: \$Author: znamirow $
 *
 * \since   2005/11/10
 *
 * \date    \$Date: 2007/07/25 09:28:07 $
 * 
 * \note    This file is part of the tool BPEL2oWFN and was created during the
 *          project "Tools4BPEL" at the Humboldt-Universität zu Berlin. See
 *          http://www.informatik.hu-berlin.de/top/tools4bpel for details.
 *
 * \note    This file was created using GNU Bison reading file
 *          frontend-parser.yy.
 *          See http://www.gnu.org/software/bison/bison.html for details
 *
 * \version \$Revision: 1.321 $
 *
 * \ingroup frontend
 */
#line 64 "frontend-parser.yy"
	
/*!
 * \file frontend-parser.h
 * \brief BPEL parser
 *
 * See \ref frontend-parser.cc for more information.
 * \ingroup frontend
 */

/*!
 * \fn frontend_parse
 * \brief parse the input file
 *
 * \note C LALR(1) parser skeleton written by Richard Stallman, by simplifying
 *       the original so-called "semantic" parser.
 *
 * \return 0 if no parse error occurs
 * \return 1 if any error occurs
 * \ingroup frontend
 */

/*!
 * \enum yytokentype
 * \brief list of possible tokens
 * \ingroup frontend
 */
#line 159 "frontend-parser.yy"

// Options for Bison (1): Enable debug mode for verbose messages during
// parsing. The messages can be enabled by using command-line parameter
// "-d bison".
#define YYDEBUG 1
#define YYERROR_VERBOSE 1

// Options for Bison (2): To avoid the message "parser stack overflow". or
// "memory exhausted". These values are just guesses. Increase them if
// necessary.
#define YYMAXDEPTH 1000000
#define YYINITDEPTH 10000


#include <cassert>
#include <map>

#include "ast-config.h"
#include "helpers.h"
#include "debug.h"
#include "globals.h"
#include "ast-details.h"


using namespace kc;
using std::cerr;
using std::endl;



/******************************************************************************
 * External variables
 *****************************************************************************/

extern char *frontend_text;	// from flex: the current token
extern int frontend_lex();	// from flex: the lexer funtion
extern int frontend_lineno;	// from flex: the current line number



/******************************************************************************
 * Global variables
 *****************************************************************************/

/// A pointer to the current join condition.
impl_joinCondition* currentJoinCondition = standardJoinCondition();


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
#line 521 "frontend-parser.cc"

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
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1250

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  149
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  111
/* YYNRULES -- Number of rules.  */
#define YYNRULES  261
/* YYNRULES -- Number of states.  */
#define YYNSTATES  945

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
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     4,    25,    27,    30,    32,    34,    36,
      38,    40,    42,    44,    46,    48,    50,    52,    54,    56,
      58,    60,    62,    64,    66,    68,    70,    72,    74,    77,
      81,    82,    88,    89,    96,    99,   103,   108,   114,   115,
     123,   126,   130,   136,   140,   141,   148,   153,   156,   160,
     167,   168,   175,   178,   182,   188,   192,   193,   201,   202,
     205,   214,   215,   224,   225,   233,   234,   242,   243,   251,
     252,   256,   260,   261,   265,   275,   285,   294,   304,   314,
     315,   324,   325,   332,   335,   339,   345,   353,   357,   358,
     365,   368,   372,   378,   382,   383,   390,   393,   397,   403,
     407,   408,   415,   418,   422,   428,   432,   433,   440,   443,
     447,   453,   457,   466,   470,   479,   483,   487,   500,   508,
     511,   515,   525,   531,   539,   547,   555,   563,   567,   572,
     575,   582,   586,   594,   600,   608,   616,   624,   628,   633,
     640,   644,   651,   655,   662,   666,   674,   682,   689,   697,
     705,   709,   718,   727,   734,   738,   745,   749,   756,   760,
     767,   771,   778,   782,   789,   793,   801,   813,   822,   828,
     837,   842,   843,   846,   856,   857,   865,   868,   872,   880,
     881,   885,   894,   903,   913,   923,   935,   945,   954,   963,
     964,   971,   980,   991,  1000,  1001,  1009,  1012,  1016,  1022,
    1026,  1034,  1042,  1050,  1058,  1066,  1073,  1084,  1096,  1108,
    1120,  1133,  1146,  1160,  1173,  1190,  1206,  1221,  1235,  1249,
    1262,  1275,  1288,  1289,  1290,  1296,  1299,  1302,  1312,  1322,
    1340,  1341,  1345,  1351,  1355,  1356,  1360,  1367,  1371,  1372,
    1380,  1384,  1385,  1393,  1397,  1401,  1408,  1410,  1416,  1422,
    1426,  1430,  1434,  1436,  1438,  1440,  1442,  1444,  1446,  1448,
    1451,  1455
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
     150,     0,    -1,    -1,   151,    98,    62,   246,    97,   156,
     155,   159,   162,   165,   181,   184,   168,   172,   174,   152,
      97,    99,    62,    95,    -1,   153,    -1,     1,   153,    -1,
     196,    -1,   197,    -1,   198,    -1,   199,    -1,   206,    -1,
     207,    -1,   208,    -1,   209,    -1,   210,    -1,   213,    -1,
     214,    -1,   215,    -1,   216,    -1,   217,    -1,   218,    -1,
     219,    -1,   227,    -1,   228,    -1,   229,    -1,   235,    -1,
     234,    -1,   245,    -1,   152,    97,    -1,   152,    97,   154,
      -1,    -1,    42,   246,    99,    97,   155,    -1,    -1,    31,
      97,   157,    99,    31,    97,    -1,   158,    97,    -1,   158,
      97,   157,    -1,    28,   246,    99,    97,    -1,    28,   246,
      97,    99,    28,    -1,    -1,    59,   246,    97,   160,    99,
      59,    97,    -1,   161,    97,    -1,   161,    97,   160,    -1,
      58,   246,    97,    99,    58,    -1,    58,   246,    99,    -1,
      -1,    60,    97,   163,    99,    60,    97,    -1,    60,     1,
      60,    97,    -1,   164,    97,    -1,   164,    97,   163,    -1,
      57,   246,    97,   160,    99,    57,    -1,    -1,    49,    97,
     166,    99,    49,    97,    -1,   167,    97,    -1,   167,    97,
     166,    -1,    48,   246,    97,    99,    48,    -1,    48,   246,
      99,    -1,    -1,    32,    97,   169,   171,    99,    32,    97,
      -1,    -1,   170,   169,    -1,    11,   246,    97,   152,    97,
      99,    11,    97,    -1,    -1,    12,   246,    97,   152,    97,
      99,    12,    97,    -1,    -1,    15,    97,   152,    97,    99,
      15,    97,    -1,    -1,    78,    97,   152,    97,    99,    78,
      97,    -1,    -1,    26,    97,   175,   176,    99,    26,    97,
      -1,    -1,   177,    97,   175,    -1,   178,    97,   175,    -1,
      -1,   179,    97,   176,    -1,    52,   246,    97,   187,   193,
     152,    97,    99,    52,    -1,    51,   246,    97,   187,   193,
     152,    97,    99,    51,    -1,    50,   246,    97,   180,   152,
      97,    99,    50,    -1,    50,   246,    97,   211,   180,   152,
      97,    99,    50,    -1,    50,   246,    97,   212,   180,   152,
      97,    99,    50,    -1,    -1,    65,   246,    95,   259,    98,
      99,    65,    97,    -1,    -1,    87,    97,   182,    99,    87,
      97,    -1,   183,    97,    -1,   183,    97,   182,    -1,    86,
     246,    97,    99,    86,    -1,    86,   246,    97,   202,    97,
      99,    86,    -1,    86,   246,    99,    -1,    -1,    22,    97,
     185,    99,    22,    97,    -1,   186,    97,    -1,   186,    97,
     185,    -1,    21,   246,    97,    99,    21,    -1,    21,   246,
      99,    -1,    -1,    20,    97,   188,    99,    20,    97,    -1,
     189,    97,    -1,   189,    97,   188,    -1,    19,   246,    97,
      99,    19,    -1,    19,   246,    99,    -1,    -1,    82,    97,
     191,    99,    82,    97,    -1,   192,    97,    -1,   192,    97,
     191,    -1,    81,   246,    97,    99,    81,    -1,    81,   246,
      99,    -1,    -1,    39,    97,   194,    99,    39,    97,    -1,
     195,    97,    -1,   195,    97,   194,    -1,    38,   246,    97,
      99,    38,    -1,    38,   246,    99,    -1,    64,   246,    97,
     248,   187,   193,    99,    64,    -1,    64,   246,    99,    -1,
      67,   246,    97,   248,   187,   190,    99,    67,    -1,    67,
     246,    99,    -1,    43,   246,    99,    -1,    43,   246,    97,
     248,   187,   169,   171,   172,   190,   193,    99,    43,    -1,
       8,   246,    97,   248,   200,    99,     8,    -1,   201,    97,
      -1,   201,    97,   200,    -1,    18,   246,    97,   202,    97,
     205,    97,    99,    18,    -1,    37,   246,    97,    99,    37,
      -1,    37,   246,    95,   259,    98,    99,    37,    -1,    37,
     246,    95,   146,    98,    99,    37,    -1,    37,   246,    97,
     203,    97,    99,    37,    -1,    37,   246,    97,   204,    97,
      99,    37,    -1,    37,   246,    99,    -1,    37,   246,     1,
      37,    -1,    54,    99,    -1,    47,    95,   259,    98,    99,
      47,    -1,    47,     1,    47,    -1,    63,   246,    95,   145,
      98,    99,    63,    -1,    80,   246,    97,    99,    80,    -1,
      80,   246,    95,   259,    98,    99,    80,    -1,    80,   246,
      95,   146,    98,    99,    80,    -1,    80,   246,    97,   204,
      97,    99,    80,    -1,    80,   246,    99,    -1,    80,   246,
       1,    80,    -1,    85,   246,    97,   248,    99,    85,    -1,
      85,   246,    99,    -1,    25,   246,    97,   248,    99,    25,
      -1,    25,   246,    99,    -1,    53,   246,    97,   248,    99,
      53,    -1,    53,   246,    99,    -1,    29,   246,    97,   152,
      97,    99,    29,    -1,    29,   246,    97,     1,    97,    99,
      29,    -1,    88,   246,    97,   248,    99,    88,    -1,    88,
     246,    97,   211,   248,    99,    88,    -1,    88,   246,    97,
     212,   248,    99,    88,    -1,    88,   246,    99,    -1,    35,
     246,    95,   259,    98,    99,    35,    97,    -1,    84,   246,
      95,   259,    98,    99,    84,    97,    -1,    27,   246,    97,
     248,    99,    27,    -1,    27,   246,    99,    -1,    77,   246,
      97,   248,    99,    77,    -1,    77,   246,    99,    -1,    79,
     246,    97,   248,    99,    79,    -1,    79,   246,    99,    -1,
      68,   246,    97,   248,    99,    68,    -1,    68,   246,    99,
      -1,    13,   246,    97,   248,    99,    13,    -1,    13,   246,
      99,    -1,    14,   246,    97,   248,    99,    14,    -1,    14,
     246,    99,    -1,    70,   246,    97,   248,   154,    99,    70,
      -1,    41,   246,    97,   248,   220,   152,    97,   221,   223,
      99,    41,    -1,    74,   246,    97,   248,   224,   226,    99,
      74,    -1,    17,   246,     1,    17,    97,    -1,    17,   246,
      95,   145,    98,    99,    17,    97,    -1,    17,   246,    99,
      97,    -1,    -1,   222,   221,    -1,    24,   246,    97,   220,
     152,    97,    99,    24,    97,    -1,    -1,    23,    97,   152,
      97,    99,    23,    97,    -1,   225,    97,    -1,   225,    97,
     224,    -1,    10,   246,    97,   152,    97,    99,    10,    -1,
      -1,    56,   246,    99,    -1,    56,   246,    97,   152,    97,
      99,    56,    97,    -1,    89,   246,    97,   248,   152,    97,
      99,    89,    -1,    89,   246,    97,   248,   220,   152,    97,
      99,    89,    -1,    66,   246,    97,   248,   152,    97,   220,
      99,    66,    -1,    36,   246,    97,   248,   230,   231,   232,
     245,    97,    99,    36,    -1,    36,   246,    97,   248,     1,
     245,    97,    99,    36,    -1,    73,   246,    95,   259,    98,
      99,    73,    97,    -1,    33,   246,    95,   259,    98,    99,
      33,    97,    -1,    -1,    16,    97,   233,    99,    16,    97,
      -1,     9,   246,    95,   259,    98,    99,     9,    97,    -1,
      61,   246,    97,   248,   177,    97,   175,   176,    99,    61,
      -1,    34,   246,    97,   248,   236,   154,    99,    34,    -1,
      -1,    46,   246,    97,   237,    99,    46,    97,    -1,   238,
      97,    -1,   238,    97,   237,    -1,    45,   246,    97,    99,
      45,    -1,    45,   246,    99,    -1,    78,    97,   152,    97,
      99,    78,    97,    -1,    15,    97,   152,    97,    99,    15,
      97,    -1,    32,    97,   169,   171,    99,    32,    97,    -1,
      26,    97,   175,   176,    99,    26,    97,    -1,    59,   246,
      97,   160,    99,    59,    97,    -1,    49,    97,   166,    99,
      49,    97,    -1,    69,   246,    97,   248,   181,   184,   152,
      97,    99,    69,    -1,    69,   246,    97,   248,   181,   184,
     241,   152,    97,    99,    69,    -1,    69,   246,    97,   248,
     181,   184,   240,   152,    97,    99,    69,    -1,    69,   246,
      97,   248,   181,   184,   242,   152,    97,    99,    69,    -1,
      69,   246,    97,   248,   181,   184,   241,   242,   152,    97,
      99,    69,    -1,    69,   246,    97,   248,   181,   184,   240,
     242,   152,    97,    99,    69,    -1,    69,   246,    97,   248,
     181,   184,   241,   240,   242,   152,    97,    99,    69,    -1,
      69,   246,    97,   248,   181,   184,   241,   240,   152,    97,
      99,    69,    -1,    69,   246,    97,   248,   181,   243,   165,
     184,   174,   168,   172,   173,   152,    97,    99,    69,    -1,
      69,   246,    97,   248,   181,   244,   184,   174,   168,   172,
     173,   152,    97,    99,    69,    -1,    69,   246,    97,   248,
     181,   184,   242,   241,   172,   173,   152,    97,    99,    69,
      -1,    69,   246,    97,   248,   181,   184,   242,   240,   173,
     152,    97,    99,    69,    -1,    69,   246,    97,   248,   181,
     184,   241,   240,   239,   152,    97,    99,    69,    -1,    69,
     246,    97,   248,   181,   184,   242,   239,   152,    97,    99,
      69,    -1,    69,   246,    97,   248,   181,   184,   241,   239,
     152,    97,    99,    69,    -1,    69,   246,    97,   248,   181,
     184,   240,   239,   152,    97,    99,    69,    -1,    -1,    -1,
     145,   247,    96,   147,   246,    -1,   255,   246,    -1,   249,
     251,    -1,    76,    97,   254,   250,    97,   249,    99,    76,
      97,    -1,    72,    97,   254,   252,    97,   251,    99,    72,
      97,    -1,    76,    97,   254,   250,    97,   249,    99,    76,
      97,    72,    97,   252,    97,   251,    99,    72,    97,    -1,
      -1,   250,    97,   249,    -1,    75,   246,    97,    99,    75,
      -1,    75,   246,    99,    -1,    -1,   252,    97,   251,    -1,
      71,   246,    97,   253,    99,    71,    -1,    71,   246,    99,
      -1,    -1,    83,    95,   257,    98,    99,    83,    97,    -1,
      83,   246,    99,    -1,    -1,    44,    95,   256,    98,    99,
      44,    97,    -1,    44,    96,   256,    -1,    44,    96,   147,
      -1,    40,    90,     3,   145,     3,    94,    -1,   146,    -1,
      90,   256,     7,   256,    94,    -1,    90,   256,    55,   256,
      94,    -1,   256,     7,   256,    -1,   256,    55,   256,    -1,
     146,   258,   259,    -1,     6,    -1,     5,    -1,    91,    -1,
      92,    -1,     4,    -1,    93,    -1,   145,    -1,   145,   259,
      -1,     3,   145,     3,    -1,   144,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   327,   327,   327,   361,   363,   369,   370,   371,   372,
     373,   374,   375,   376,   377,   378,   379,   380,   381,   382,
     383,   384,   385,   386,   387,   388,   389,   390,   394,   396,
     405,   407,   410,   412,   416,   417,   421,   422,   432,   433,
     438,   440,   445,   447,   458,   459,   461,   466,   468,   473,
     484,   485,   490,   492,   497,   499,   509,   510,   516,   517,
     522,   528,   529,   540,   541,   552,   553,   564,   565,   571,
     572,   574,   580,   581,   586,   591,   596,   598,   600,   606,
     607,   621,   622,   627,   629,   634,   636,   638,   649,   650,
     655,   657,   662,   664,   675,   676,   681,   683,   688,   690,
     701,   702,   707,   709,   714,   716,   722,   723,   728,   730,
     735,   737,   750,   752,   762,   764,   774,   776,   786,   791,
     793,   798,   803,   805,   807,   809,   811,   813,   815,   818,
     823,   825,   831,   835,   837,   839,   841,   843,   845,   856,
     858,   868,   870,   880,   883,   894,   896,   907,   909,   911,
     913,   918,   922,   934,   936,   938,   940,   950,   952,   962,
     964,   979,   984,   993,   995,  1005,  1015,  1017,  1022,  1025,
    1027,  1033,  1034,  1039,  1045,  1046,  1052,  1054,  1059,  1066,
    1067,  1069,  1079,  1081,  1096,  1111,  1113,  1118,  1123,  1129,
    1130,  1135,  1145,  1155,  1161,  1162,  1167,  1169,  1174,  1176,
    1188,  1193,  1198,  1203,  1208,  1213,  1220,  1224,  1228,  1232,
    1236,  1241,  1246,  1251,  1256,  1261,  1266,  1271,  1275,  1280,
    1284,  1288,  1313,  1316,  1315,  1322,  1335,  1338,  1341,  1344,
    1351,  1352,  1357,  1359,  1365,  1366,  1371,  1373,  1377,  1379,
    1380,  1385,  1386,  1391,  1394,  1399,  1401,  1403,  1405,  1407,
    1409,  1414,  1418,  1419,  1420,  1421,  1422,  1423,  1427,  1429,
    1431,  1433
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
  "X_TEXT", "$accept", "tProcess", "@1", "activity", "activity2",
  "activity_list", "imports", "tExtensions", "tExtension_list",
  "tExtension", "tPartnerLinks", "tPartnerLink_list", "tPartnerLink",
  "tPartners", "tPartner_list", "tPartner", "tMessageExchanges",
  "tMessageExchange_list", "tMessageExchange", "tFaultHandlers",
  "tCatch_list", "tCatch", "tCatchAll", "tCompensationHandler",
  "tTerminationHandler", "tEventHandlers", "tOnMessage_list",
  "tOnAlarm_list", "tOnMessage", "tOnEvent", "tOnAlarm", "tRepeatEvery",
  "tVariables", "tVariable_list", "tVariable", "tCorrelationSets",
  "tCorrelationSet_list", "tCorrelationSet", "tCorrelations",
  "tCorrelation_list", "tCorrelation", "tToParts", "tToPart_list",
  "tToPart", "tFromParts", "tFromPart_list", "tFromPart", "tReceive",
  "tReply", "tInvoke", "tAssign", "tCopy_list", "tCopy", "tFrom",
  "tLiteral", "tQuery", "tTo", "tValidate", "tEmpty", "tOpaqueActivity",
  "tExtensionActivity", "tWait", "tFor", "tUntil", "tExit", "tThrow",
  "tRethrow", "tCompensate", "tCompensateScope", "tSequence", "tIf",
  "tCondition", "tElseIf_list", "tElseIf", "tElse", "tCase_list", "tCase",
  "tOtherwise", "tWhile", "tRepeatUntil", "tForEach", "tStartCounterValue",
  "tFinalCounterValue", "tCompletionCondition", "tBranches", "tPick",
  "tFlow", "tLinks", "tLink_list", "tLink", "truetTerminationHandler",
  "truetCompensationHandler", "truetFaultHandlers", "truetEventHandlers",
  "truetPartnerLinks", "truetMessageExchanges", "tScope",
  "arbitraryAttributes", "@2", "standardElements", "tTarget_list",
  "tTarget", "tSource_list", "tSource", "tTransitionCondition",
  "tJoinCondition", "joinCondition", "booleanLinkCondition",
  "transitionCondition", "comparison", "constant", 0
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
static const yytype_uint16 yyr1[] =
{
       0,   149,   151,   150,   152,   152,   153,   153,   153,   153,
     153,   153,   153,   153,   153,   153,   153,   153,   153,   153,
     153,   153,   153,   153,   153,   153,   153,   153,   154,   154,
     155,   155,   156,   156,   157,   157,   158,   158,   159,   159,
     160,   160,   161,   161,   162,   162,   162,   163,   163,   164,
     165,   165,   166,   166,   167,   167,   168,   168,   169,   169,
     170,   171,   171,   172,   172,   173,   173,   174,   174,   175,
     175,   175,   176,   176,   177,   178,   179,   179,   179,   180,
     180,   181,   181,   182,   182,   183,   183,   183,   184,   184,
     185,   185,   186,   186,   187,   187,   188,   188,   189,   189,
     190,   190,   191,   191,   192,   192,   193,   193,   194,   194,
     195,   195,   196,   196,   197,   197,   198,   198,   199,   200,
     200,   201,   202,   202,   202,   202,   202,   202,   202,   202,
     203,   203,   204,   205,   205,   205,   205,   205,   205,   206,
     206,   207,   207,   208,   208,   209,   209,   210,   210,   210,
     210,   211,   212,   213,   213,   213,   213,   214,   214,   215,
     215,   216,   216,   217,   217,   218,   219,   219,   220,   220,
     220,   221,   221,   222,   223,   223,   224,   224,   225,   226,
     226,   226,   227,   227,   228,   229,   229,   230,   231,   232,
     232,   233,   234,   235,   236,   236,   237,   237,   238,   238,
     239,   240,   241,   242,   243,   244,   245,   245,   245,   245,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   246,   247,   246,   246,   248,   248,   248,   248,
     249,   249,   250,   250,   251,   251,   252,   252,   253,   253,
     253,   254,   254,   255,   255,   256,   256,   256,   256,   256,
     256,   257,   258,   258,   258,   258,   258,   258,   259,   259,
     259,   259
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,    20,     1,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     2,     3,
       0,     5,     0,     6,     2,     3,     4,     5,     0,     7,
       2,     3,     5,     3,     0,     6,     4,     2,     3,     6,
       0,     6,     2,     3,     5,     3,     0,     7,     0,     2,
       8,     0,     8,     0,     7,     0,     7,     0,     7,     0,
       3,     3,     0,     3,     9,     9,     8,     9,     9,     0,
       8,     0,     6,     2,     3,     5,     7,     3,     0,     6,
       2,     3,     5,     3,     0,     6,     2,     3,     5,     3,
       0,     6,     2,     3,     5,     3,     0,     6,     2,     3,
       5,     3,     8,     3,     8,     3,     3,    12,     7,     2,
       3,     9,     5,     7,     7,     7,     7,     3,     4,     2,
       6,     3,     7,     5,     7,     7,     7,     3,     4,     6,
       3,     6,     3,     6,     3,     7,     7,     6,     7,     7,
       3,     8,     8,     6,     3,     6,     3,     6,     3,     6,
       3,     6,     3,     6,     3,     7,    11,     8,     5,     8,
       4,     0,     2,     9,     0,     7,     2,     3,     7,     0,
       3,     8,     8,     9,     9,    11,     9,     8,     8,     0,
       6,     8,    10,     8,     0,     7,     2,     3,     5,     3,
       7,     7,     7,     7,     7,     6,    10,    11,    11,    11,
      12,    12,    13,    12,    16,    15,    14,    13,    13,    12,
      12,    12,     0,     0,     5,     2,     2,     9,     9,    17,
       0,     3,     5,     3,     0,     3,     6,     3,     0,     7,
       3,     0,     7,     3,     3,     6,     1,     5,     5,     3,
       3,     3,     1,     1,     1,     1,     1,     1,     1,     2,
       3,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,     0,     1,     0,   222,     0,   223,     0,   222,
       0,     0,    32,   225,     0,     0,   246,   244,   243,     0,
       0,    30,     0,     0,     0,     0,   222,     0,   222,    38,
       0,     0,     0,   249,   250,   224,   222,     0,     0,     0,
     222,    44,     0,   249,   250,     0,     0,    34,     0,     0,
       0,    50,     0,   247,   248,     0,     0,     0,    35,    30,
       0,     0,     0,     0,    81,   245,     0,    36,    33,    31,
     222,     0,     0,     0,   222,     0,     0,     0,     0,    88,
      37,     0,     0,    40,    46,     0,     0,    47,   222,     0,
       0,     0,     0,    56,     0,    43,     0,    41,     0,     0,
      48,     0,     0,    52,   222,     0,     0,     0,     0,    63,
       0,    39,     0,    45,     0,    55,     0,    53,     0,     0,
      83,   222,     0,     0,    58,     0,    67,    42,     0,     0,
      51,     0,    87,     0,    84,     0,     0,    90,   222,    61,
      58,     0,     0,     0,    49,    54,   222,     0,     0,     0,
      82,     0,    93,     0,    91,     0,   222,     0,    59,     0,
     222,   222,   222,   222,   222,   222,   222,   222,   222,   222,
     222,   222,   222,   222,   222,   222,   222,   222,   222,   222,
     222,   222,   222,   222,     0,     4,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    26,    25,    27,    69,     0,
       0,   129,    85,     0,     0,    89,     0,     0,     0,     5,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   222,   222,    72,     0,     0,
       0,     0,     0,     0,   127,     0,    92,     0,     0,     0,
     230,   230,   162,   230,   164,   230,   142,   230,   154,     0,
     230,   230,   230,   230,   116,   230,   144,   230,   230,   113,
     230,   230,   115,   230,   160,   230,   230,   230,   230,   156,
     230,   158,   230,   140,   230,   150,   230,     0,     0,     0,
     222,     0,     0,    69,    69,     0,   128,     0,   261,   258,
       0,     0,     0,   222,     0,     0,     0,    86,     0,     0,
      57,     0,   222,     0,     0,   234,     0,     0,     0,     0,
       0,     0,     0,   194,     0,     0,    94,     0,     0,    94,
       0,    94,     0,    81,     0,     0,     0,     0,     0,   222,
     222,   230,   230,     0,     0,     0,    94,    94,     0,     0,
      72,    70,    71,     0,     0,   259,     0,     0,     0,     0,
       0,   122,     0,     0,     0,     0,   241,     0,   241,   222,
       0,     0,   222,   226,     0,   230,     0,     0,     0,     0,
       0,     0,   222,     0,     0,   222,     0,   222,     0,     0,
      58,     0,     0,   106,     0,   100,     0,    88,     0,     0,
     222,   179,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    64,   106,   106,    79,     0,    73,     3,
     260,     0,     0,   131,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   233,     0,     0,     0,   119,     0,   234,
     231,   161,   163,   141,   153,     0,     0,     0,     0,     0,
       0,   222,   189,     0,     0,     0,    61,   143,    69,     0,
       0,     0,     0,     0,   159,     0,   222,     0,    50,    88,
       0,     0,     0,   222,     0,   176,   155,   157,   139,     0,
       0,     0,     0,   147,     0,     0,     0,     0,   222,     0,
      79,    79,    68,   124,   123,     0,     0,   125,   126,    60,
       0,     0,     0,     0,     0,     0,   118,   120,   238,   237,
     235,   146,   145,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   171,   222,     0,     0,    63,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    88,    67,    29,   165,     0,     0,     0,
     177,     0,     0,   148,   149,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    62,     0,   234,   232,   230,
       0,   222,     0,   222,     0,     0,   193,     0,     0,     0,
       0,     0,     0,     0,   170,   222,   174,   171,     0,     0,
      96,   100,     0,   222,     0,     0,   112,     0,   222,     0,
       0,   114,     0,     0,     0,    69,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    65,    63,
      67,    56,     0,     0,   180,   167,     0,     0,   182,     0,
       0,     0,     0,     0,     0,     0,   130,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   196,   186,     0,
       0,   222,     0,     0,   168,     0,     0,     0,     0,   172,
       0,    99,     0,    97,   106,     0,     0,     0,   108,   184,
       0,     0,   102,     0,     0,     0,    72,    61,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    65,    56,    63,     0,     0,     0,     0,
     183,     0,     0,     0,     0,     0,     0,   132,     0,     0,
       0,   222,     0,     0,     0,   240,   236,     0,   199,     0,
     197,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    95,     0,   192,     0,   111,     0,   109,     0,   105,
       0,   103,     0,     0,     0,     0,     0,   206,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    65,     0,     0,     0,     0,    75,
      74,     0,    76,     0,     0,     0,     0,     0,     0,     0,
     256,   253,   252,   254,   255,   257,     0,     0,     0,   195,
       0,     0,     0,     0,   185,     0,     0,     0,   166,    98,
       0,     0,   107,     0,   101,   205,     0,     0,     0,     0,
       0,   208,     0,     0,   207,     0,     0,     0,     0,     0,
     209,     0,     0,     0,     0,    65,     0,   178,     0,   151,
     152,     0,    77,    78,   242,   228,   227,     0,     0,     0,
     137,     0,   251,     0,   198,   187,     0,     0,   190,   169,
       0,     0,   117,   110,   104,   204,     0,     0,     0,     0,
     221,   211,   220,   213,     0,     0,   210,   219,     0,     0,
       0,     0,     0,     0,     0,     0,   138,     0,     0,     0,
       0,   121,     0,   188,     0,     0,     0,   201,   203,   202,
       0,   218,   212,     0,   217,     0,     0,     0,   181,    80,
       0,     0,     0,   133,     0,   239,     0,     0,     0,   200,
       0,   216,     0,     0,     0,     0,     0,     0,     0,     0,
     175,    66,     0,   215,   234,   135,   134,   136,   191,   173,
     214,     0,     0,     0,   229
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,   408,   185,   409,    29,    21,    37,    38,
      41,    71,    72,    51,    75,    76,    64,    89,    90,   109,
     139,   140,   157,   126,   702,   143,   247,   301,   248,   249,
     302,   499,    79,   105,   106,    93,   122,   123,   400,   535,
     536,   473,   609,   610,   470,   604,   605,   186,   187,   188,
     189,   380,   381,   149,   315,   316,   722,   190,   191,   192,
     193,   194,   351,   352,   195,   196,   197,   198,   199,   200,
     201,   398,   596,   597,   668,   411,   412,   484,   202,   203,
     204,   396,   462,   529,   662,   205,   206,   393,   584,   585,
     620,   550,   551,   552,   478,   479,   207,     8,    11,   324,
     325,   326,   383,   384,   582,   441,     9,    18,   724,   796,
     311
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -653
static const yytype_int16 yypact[] =
{
    -653,    82,   -42,  -653,    33,    -9,     4,  -653,    11,    -9,
     -14,    55,   169,  -653,    88,    -7,  -653,  -653,    70,    49,
     111,   177,   208,   142,    -7,    -7,    -9,   195,    -9,   180,
      91,    -7,    -7,  -653,   226,  -653,    -9,   141,   156,   159,
      -9,   206,   269,   182,    20,   -32,   243,   195,   181,   183,
      37,   230,   188,  -653,  -653,   184,   189,   191,  -653,   177,
     227,   229,   236,   193,   204,  -653,   266,  -653,  -653,  -653,
      -9,   197,   200,   202,    -9,   207,   210,   261,   213,   290,
    -653,    -5,   256,   227,  -653,   219,   257,   236,    -9,   221,
     224,   232,   228,   292,   231,  -653,   235,  -653,   227,   237,
    -653,     0,   278,   261,    -9,   240,   238,   307,   246,   314,
     273,  -653,   241,  -653,   245,  -653,   248,  -653,    10,   259,
     232,    -9,   249,   250,   338,   253,   325,  -653,   295,   305,
    -653,    51,  -653,   260,  -653,    89,   332,   307,    -9,   347,
     338,  1024,   263,  1024,  -653,  -653,    -9,   262,   276,   267,
    -653,   264,  -653,   268,  -653,   270,    -9,   271,  -653,  1161,
      -9,    -9,    -9,    -9,    -9,    -9,    -9,    -9,    -9,    -9,
      -9,    -9,    -9,    -9,    -9,    -9,    -9,    -9,    -9,    -9,
      -9,    -9,    -9,    -9,   272,  -653,  -653,  -653,  -653,  -653,
    -653,  -653,  -653,  -653,  -653,  -653,  -653,  -653,  -653,  -653,
    -653,  -653,  -653,  -653,  -653,  -653,  -653,  -653,    22,   275,
      24,  -653,  -653,   274,   345,  -653,  1024,   279,   336,  -653,
     280,   106,   107,   110,   115,   281,   282,   283,   284,   118,
     119,   285,   123,   286,   127,   128,   288,   289,   291,   131,
     132,   135,   144,   293,   294,    -9,    -9,   341,   297,   298,
     299,   359,    36,     5,  -653,   311,  -653,   302,  1024,   303,
     126,   126,  -653,   126,  -653,   126,  -653,   126,  -653,  1094,
     126,   126,   126,   126,  -653,   126,  -653,   126,   126,  -653,
     126,   126,  -653,   126,  -653,   126,   126,   126,   126,  -653,
     126,  -653,   126,  -653,    26,  -653,   126,   360,   304,   306,
      -9,   308,   309,    22,    22,   342,  -653,   277,  -653,    50,
     310,   315,    48,    -9,   368,   317,   318,  -653,   313,   320,
    -653,   321,    -9,   322,   402,   350,   326,   327,   328,   329,
     330,   538,   335,   378,    56,   408,   413,   337,   382,   413,
    1024,   413,   339,   204,  1024,   425,   340,   343,   344,    -9,
      -9,   126,   126,   346,   954,   349,   413,   413,   351,   411,
     341,  -653,  -653,   352,   437,  -653,   353,   355,   394,    50,
     356,  -653,   357,   358,   433,   361,   406,   147,   406,    -9,
     362,   365,    -9,  -653,   366,   380,   445,   450,   434,   438,
     367,   370,    -9,  1024,   398,    -9,   439,    -9,  1024,   373,
     338,   418,   377,   446,   381,   395,   419,    32,   392,   396,
      -9,   443,   404,   416,   423,   422,   417,   420,   429,   431,
     436,   435,  1024,  -653,   446,   446,    28,   440,  -653,  -653,
    -653,   480,   485,  -653,   441,   386,   496,   497,   444,   523,
     447,   350,   448,  -653,   380,   452,   530,   402,   152,   350,
    -653,  -653,  -653,  -653,  -653,   511,   515,   453,   454,   457,
     462,    -9,   529,    42,   461,   540,   347,  -653,    22,   463,
     465,   408,   469,   470,  -653,   471,    -9,   632,   230,   290,
     234,   492,   473,    -9,   472,   425,  -653,  -653,  -653,    50,
      50,   488,   489,  -653,   479,   483,  1024,  1024,    -9,  1024,
     517,   517,  -653,  -653,  -653,   484,   490,  -653,  -653,  -653,
     493,    -7,   495,   514,   499,    18,  -653,  -653,   503,  -653,
    -653,  -653,  -653,   548,   560,   502,    50,   508,   501,   398,
     592,   466,   513,   589,    -9,   519,   522,   314,   341,   576,
     552,   521,   541,   554,   261,   527,   528,   531,   533,   535,
     875,   717,   796,   290,   325,  -653,  -653,  1024,   153,   562,
    -653,   536,   539,  -653,  -653,   549,   542,   545,   546,   544,
     547,  1024,  1024,   601,   550,  -653,    29,   350,  -653,   380,
     553,   -10,   555,    -9,   556,   559,  -653,   615,   564,    50,
     643,   563,   566,   569,  -653,    -9,   646,   589,   157,   650,
     540,   395,   572,    -9,   573,   577,  -653,   610,    -9,   578,
     581,  -653,   580,   227,  1024,    22,   338,   582,   583,   585,
    1024,  1024,   586,  1024,   875,  1024,   587,  1024,   608,   314,
     325,   292,   590,  1024,  -653,  -653,   591,   593,  -653,   599,
     595,   598,    50,   604,   594,   607,  -653,   626,   606,   609,
     611,   627,   568,   613,   644,   165,   670,   548,  -653,   620,
     624,    -9,   625,   628,  -653,   629,   636,   637,   630,  -653,
     638,  -653,   639,  -653,   446,   662,   170,   687,   576,  -653,
     174,   653,   541,   689,   640,   648,   341,   347,   672,  1024,
     649,   652,   655,   651,   657,   658,  1024,  1024,   659,   660,
     664,   665,  1024,   608,   292,   314,   666,   667,   712,   673,
    -653,   715,   711,   669,   718,   674,   675,  -653,   725,   699,
     696,    -9,   678,    54,   679,  -653,  -653,   677,  -653,   682,
    -653,   707,   683,   693,   773,   754,   775,   408,  1024,   752,
     779,  -653,   700,  -653,   701,  -653,   704,  -653,   708,  -653,
     706,  -653,   716,   749,   713,   719,   720,  -653,   723,   745,
     727,   728,   746,   730,   732,   736,   737,   739,   747,   741,
    1024,   738,  1024,   314,   608,   807,   742,   748,   750,  -653,
    -653,   743,  -653,   772,   774,   751,   753,   755,    45,   744,
    -653,  -653,  -653,  -653,  -653,  -653,    50,   756,   791,  -653,
     757,   811,    50,   759,  -653,   761,  1024,   762,  -653,  -653,
     803,   813,  -653,   780,  -653,  -653,   770,   838,   842,   837,
     778,  -653,   802,   809,  -653,   810,   817,   781,   783,   818,
    -653,   821,   794,   793,   797,   608,  1024,  -653,   816,  -653,
    -653,   828,  -653,  -653,  -653,  -653,   823,   819,    47,     7,
    -653,   878,  -653,   814,  -653,  -653,   801,   805,  -653,  -653,
     808,   815,  -653,  -653,  -653,  -653,   820,   822,   824,   829,
    -653,  -653,  -653,  -653,   839,   841,  -653,  -653,   825,   843,
     826,  1024,   830,   832,   833,   834,  -653,   835,   836,   840,
     849,  -653,   850,  -653,   827,   851,   883,  -653,  -653,  -653,
     854,  -653,  -653,   844,  -653,   846,   859,   858,  -653,  -653,
     350,   860,   862,  -653,   866,  -653,   904,   899,   861,  -653,
     869,  -653,   870,   863,   873,   855,   857,   868,   876,   877,
    -653,  -653,   871,  -653,   350,  -653,  -653,  -653,  -653,  -653,
    -653,   879,   900,   880,  -653
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -653,  -653,  -653,  -141,  -142,  -356,   916,  -653,   891,  -653,
    -653,   -77,  -653,  -653,   889,  -653,   504,   -95,  -653,  -589,
    -135,  -653,  -442,  -518,  -652,  -514,  -289,  -348,   642,  -653,
    -653,  -422,   641,   865,  -653,  -376,   852,  -653,  -270,   387,
    -653,   385,   312,  -653,  -414,   323,  -653,  -653,  -653,  -653,
    -653,   551,  -653,   476,  -653,   143,  -653,  -653,  -653,  -653,
    -653,  -653,   567,   570,  -653,  -653,  -653,  -653,  -653,  -653,
    -653,  -345,   403,  -653,  -653,   518,  -653,  -653,  -653,  -653,
    -653,  -653,  -653,  -653,  -653,  -653,  -653,  -653,   348,  -653,
    -504,  -462,   456,  -506,  -653,  -653,  -353,    -8,  -653,   233,
    -369,   558,  -446,  -434,  -653,   621,  -653,    -2,  -653,  -653,
    -305
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -29
static const yytype_int16 yytable[] =
{
     184,    13,   209,   520,   365,   158,    97,   512,   117,   422,
     496,   497,   428,    23,   361,   362,   450,   219,    35,   601,
      39,   112,    33,    34,   537,   251,    14,    24,    45,    43,
      44,   477,    49,    14,     6,     6,    24,   458,    61,   307,
     631,   459,   705,   530,   621,   625,   847,   623,   627,   368,
     307,   772,   312,   307,    92,   146,     4,   394,   790,   791,
     792,   349,    81,   349,   434,    55,    85,    56,   313,   403,
     313,   405,   147,   245,   246,   257,    15,    24,   571,   572,
     101,   475,     3,    15,    25,   652,   424,   425,   146,   624,
     628,   476,    94,   498,    95,     5,   118,   114,   321,   115,
      10,   322,   323,   554,   314,   147,   889,   131,    12,   132,
     350,   703,   350,   135,    54,   773,   704,   319,   697,   252,
     696,   253,   836,   254,   555,    25,   541,   648,   332,   395,
     155,   649,    16,    17,    62,     7,     7,   531,   210,    16,
     848,   532,   849,   369,   850,   793,   794,   795,   217,    31,
     148,    19,   220,   221,   222,   223,   224,   225,   226,   227,
     228,   229,   230,   231,   232,   233,   234,   235,   236,   237,
     238,   239,   240,   241,   242,   243,   591,   630,    22,   538,
     308,   309,   310,   881,   561,   562,   151,   774,   152,   219,
     602,   308,   309,   887,   308,   309,    26,    32,   321,   404,
      20,   322,   323,   261,   263,   262,   264,   265,    27,   266,
     650,    30,   267,   421,   268,   273,   275,   274,   276,    28,
     278,   588,   279,    36,   281,   283,   282,   284,   288,   290,
     289,   291,   292,    24,   293,   159,    42,   298,   299,    40,
      46,   294,   160,   295,   442,   756,   443,   161,   162,   518,
     633,   519,   634,    47,   670,   835,   671,   464,    48,   163,
     742,   164,   727,   165,   728,   466,    50,   744,   166,   745,
     167,   748,    52,   749,    57,   168,    53,   169,    59,    63,
      60,   495,    65,    66,   660,    70,    67,   170,    68,    73,
      77,    78,   358,    74,    80,   171,    82,    83,   172,    84,
     173,   174,   175,   176,   177,   370,    86,    87,   178,    88,
      91,   179,    92,   180,   377,    96,    98,    99,   104,   181,
     102,   103,   182,   183,   108,   107,   686,   116,   121,   125,
     110,   127,   111,   -28,   113,   120,   549,   713,   755,   119,
     128,   416,   417,   124,   129,   130,   133,   137,   136,   138,
     141,   142,   144,   145,   153,   567,   568,   150,   570,   156,
     208,   211,   212,   214,   213,   215,   256,   216,   259,   244,
     218,   445,   250,   255,   448,   355,   258,   260,   269,   270,
     271,   272,   277,   280,   457,   285,   286,   460,   287,   463,
     296,   300,   806,   297,   303,   304,   306,   317,   305,   318,
     320,   356,   482,   357,   363,   371,   360,   359,   366,   619,
     622,   626,   374,   367,   372,   373,   632,   375,   376,   378,
     379,   382,   364,   385,   392,   397,   386,   387,   388,   389,
     644,   645,   391,   399,   246,   410,   401,   427,   406,   413,
     430,   433,   414,   415,   438,   420,   423,   429,   426,   612,
     440,   435,   431,   527,   432,   322,   436,   437,   451,   453,
     439,   446,   447,   449,   452,   454,   455,   176,   545,   456,
     465,   467,   461,   685,   468,   558,   924,   472,   471,   691,
     692,   687,   694,   695,   698,   469,   700,   474,   941,   480,
     569,   852,   707,   486,   327,   481,   328,   857,   329,   483,
     330,   485,   487,   333,   334,   335,   336,   488,   337,   576,
     338,   339,   489,   340,   341,   490,   342,   503,   343,   344,
     345,   346,   504,   347,   493,   348,   598,   353,   491,   354,
     492,   506,   494,   507,   508,   510,   684,   502,   516,   505,
     521,   509,   511,   888,   522,   528,   160,   513,   758,   515,
     523,   161,   162,   524,   525,   765,   766,   526,   533,   534,
     539,   771,   556,   163,   540,   164,   542,   165,   544,   543,
     557,   559,   166,   653,   167,   655,   563,   564,   565,   168,
     566,   169,   498,   573,   418,   419,   581,   666,   574,   578,
     575,   170,   577,   583,   586,   676,   579,   807,   590,   171,
     680,   587,   172,   589,   173,   174,   175,   176,   177,   592,
     594,   593,   178,   595,   603,   179,   606,   180,   599,   600,
     607,   611,   608,   181,   613,   614,   182,   183,   615,   832,
     616,   834,   617,   159,   636,   390,   635,   637,   638,   642,
     160,   639,   640,   641,   643,   161,   162,   546,   646,   647,
     651,   658,   661,   733,   654,   656,   657,   163,   547,   164,
     663,   165,   659,   664,   548,   860,   166,   665,   167,   667,
     672,   675,   677,   168,   678,   169,   679,   681,   682,   683,
     689,   688,   690,   693,   699,   170,   701,   706,   710,   717,
     708,   715,   709,   171,   711,   882,   172,   712,   173,   174,
     175,   176,   177,   714,   716,   718,   178,   721,   719,   179,
     720,   180,   725,   788,   723,   726,   729,   181,   159,   731,
     182,   183,   732,   743,   734,   160,   746,   735,   736,   739,
     161,   162,   546,   737,   738,   750,   741,   740,   752,   753,
     906,   757,   163,   547,   164,   754,   165,   777,   759,   760,
     762,   166,   761,   167,   763,   764,   767,   778,   168,   768,
     169,   769,   770,   780,   776,   775,   779,   781,   782,   785,
     170,   786,   787,   783,   784,   789,   798,   797,   171,   799,
     800,   172,   801,   173,   174,   175,   176,   177,   802,   803,
     804,   178,   805,   808,   179,   618,   180,   159,   809,   810,
     811,   812,   181,   814,   160,   182,   183,   813,   816,   161,
     162,   546,   817,   815,   821,   824,   830,   837,   818,   819,
     820,   163,   842,   164,   843,   165,   822,   823,   548,   825,
     166,   826,   167,   827,   828,   833,   854,   168,   829,   169,
     831,   838,   841,   851,   856,   839,   862,   840,   844,   170,
     845,   863,   846,   866,   855,   853,   858,   171,   859,   861,
     172,   864,   173,   174,   175,   176,   177,   865,   867,   868,
     178,   870,   883,   179,   618,   180,   159,   869,   871,   872,
     874,   181,   875,   160,   182,   183,   873,   876,   161,   162,
     877,   878,   879,   884,   880,   885,   891,   892,   893,   886,
     163,   547,   164,   894,   165,   895,   918,   900,   901,   166,
     902,   167,   904,   928,   896,   921,   168,   897,   169,   898,
     913,   899,   920,   929,   903,   905,   916,   907,   170,   908,
     909,   910,   933,   911,   912,   935,   171,   936,    58,   172,
     940,   173,   174,   175,   176,   177,   914,   915,   937,   178,
     917,   919,   179,   618,   180,   159,   922,   923,   930,   925,
     181,   926,   160,   182,   183,   927,   931,   161,   162,   932,
     934,   397,   943,   938,   939,    69,   100,   944,   942,   163,
     402,   164,   553,   165,   407,   134,   674,   673,   166,   154,
     167,   580,   890,   500,   751,   168,   501,   169,   517,   444,
     669,   747,   514,   560,     0,   730,     0,   170,   629,     0,
       0,     0,     0,     0,     0,   171,     0,     0,   172,     0,
     173,   174,   175,   176,   177,   159,     0,     0,   178,     0,
       0,   179,   160,   180,     0,     0,     0,   161,   162,   181,
       0,     0,   182,   183,     0,     0,     0,     0,     0,   163,
       0,   164,     0,   165,     0,     0,     0,     0,   166,     0,
     167,     0,     0,     0,     0,   168,     0,   169,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   170,     0,     0,
       0,     0,     0,     0,     0,   171,     0,     0,   172,     0,
     173,   174,   175,   176,   177,   331,     0,     0,   178,     0,
       0,   179,   160,   180,     0,     0,     0,   161,   162,   181,
       0,     0,   182,   183,     0,     0,     0,     0,     0,   163,
       0,   164,     0,   165,     0,     0,     0,     0,   166,     0,
     167,     0,     0,     0,     0,   168,     0,   169,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   170,     0,     0,
       0,     0,     0,     0,     0,   171,     0,     0,   172,     0,
     173,   174,   175,   176,   177,     0,     0,     0,   178,   160,
       0,   179,     0,   180,   161,   162,     0,     0,     0,   181,
       0,     0,   182,   183,     0,     0,   163,     0,   164,     0,
     165,     0,     0,     0,     0,   166,     0,   167,     0,     0,
       0,     0,   168,     0,   169,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   170,     0,     0,     0,     0,     0,
       0,     0,   171,     0,     0,   172,     0,   173,   174,   175,
     176,   177,     0,     0,     0,   178,     0,     0,   179,     0,
     180,     0,     0,     0,     0,     0,   181,     0,     0,   182,
     183
};

static const yytype_int16 yycheck[] =
{
     141,     9,   143,   449,   309,   140,    83,   441,   103,   354,
     424,   425,   360,    15,   303,   304,   385,   159,    26,   537,
      28,    98,    24,    25,   466,     1,    40,     7,    36,    31,
      32,   407,    40,    40,    44,    44,     7,   393,     1,     3,
     554,   394,   631,     1,   550,   551,     1,   551,   552,     1,
       3,   703,    47,     3,    22,    37,    98,     1,     4,     5,
       6,    35,    70,    35,   369,    97,    74,    99,    63,   339,
      63,   341,    54,    51,    52,   216,    90,     7,   500,   501,
      88,    49,     0,    90,    55,    95,   356,   357,    37,   551,
     552,    59,    97,    65,    99,    62,   104,    97,    72,    99,
      96,    75,    76,   479,    99,    54,    99,    97,    97,    99,
      84,   629,    84,   121,    94,   704,   630,   258,   624,    95,
     624,    97,   774,    99,   480,    55,   471,    98,   269,    73,
     138,   577,   146,   147,    97,   145,   145,    95,   146,   146,
      95,    99,    97,    95,    99,    91,    92,    93,   156,     7,
      99,    96,   160,   161,   162,   163,   164,   165,   166,   167,
     168,   169,   170,   171,   172,   173,   174,   175,   176,   177,
     178,   179,   180,   181,   182,   183,   529,   553,    90,   468,
     144,   145,   146,   835,   489,   490,    97,   705,    99,   331,
     538,   144,   145,   146,   144,   145,   147,    55,    72,   340,
      31,    75,    76,    97,    97,    99,    99,    97,    97,    99,
     579,     3,    97,   354,    99,    97,    97,    99,    99,    42,
      97,   526,    99,    28,    97,    97,    99,    99,    97,    97,
      99,    99,    97,     7,    99,     1,   145,   245,   246,    59,
      99,    97,     8,    99,    97,   687,    99,    13,    14,    97,
      97,    99,    99,    97,    97,   773,    99,   398,    99,    25,
     674,    27,    97,    29,    99,   400,    60,    97,    34,    99,
      36,    97,     3,    99,    31,    41,    94,    43,    97,    49,
      97,   422,    94,    99,   589,    58,    97,    53,    97,    60,
      97,    87,   300,    57,    28,    61,    99,    97,    64,    97,
      66,    67,    68,    69,    70,   313,    99,    97,    74,    48,
      97,    77,    22,    79,   322,    59,    97,    60,    86,    85,
      99,    97,    88,    89,    32,    97,   615,    49,    21,    15,
      99,    58,    97,    99,    97,    97,   477,   642,   686,    99,
      99,   349,   350,    97,    99,    97,    87,    97,    99,    11,
      97,    26,    57,    48,    22,   496,   497,    97,   499,    12,
      97,    99,    86,    99,    97,    97,    21,    97,    32,    97,
      99,   379,    97,    99,   382,    15,    97,    97,    97,    97,
      97,    97,    97,    97,   392,    97,    97,   395,    97,   397,
      97,    50,   737,    99,    97,    97,    37,    86,    99,    97,
      97,    97,   410,    97,    62,    37,    97,    99,    98,   550,
     551,   552,    99,    98,    97,    97,   557,    97,    97,    97,
      18,    71,   145,    97,    46,    17,    99,    99,    99,    99,
     571,   572,    97,    20,    52,    10,    99,    26,    99,    99,
       3,    47,    99,    99,    11,    99,    97,    95,    97,   544,
      44,    95,    99,   461,    99,    75,    99,    99,    13,    25,
      99,    99,    97,    97,    14,    27,    99,    69,   476,    99,
      97,    53,    33,   614,    97,   483,   910,    82,    97,   620,
     621,   616,   623,   624,   625,    39,   627,    68,   934,    97,
     498,   796,   633,    77,   261,    99,   263,   802,   265,    56,
     267,    97,    79,   270,   271,   272,   273,    85,   275,   511,
     277,   278,    95,   280,   281,    95,   283,    37,   285,   286,
     287,   288,    37,   290,    88,   292,   534,   294,    99,   296,
      99,   145,    97,    37,    37,    12,   613,    97,     8,    98,
      29,    97,    95,   848,    29,    16,     8,    99,   689,    97,
      97,    13,    14,    99,    97,   696,   697,    95,    97,    19,
      97,   702,    70,    25,    99,    27,    97,    29,    97,    99,
      97,    99,    34,   581,    36,   583,    88,    88,    99,    41,
      97,    43,    65,    99,   351,   352,    83,   595,    98,    75,
      97,    53,    97,    45,    34,   603,    97,   738,    97,    61,
     608,    99,    64,    95,    66,    67,    68,    69,    70,    17,
      97,   145,    74,    24,    38,    77,    64,    79,    99,    97,
      99,    67,    81,    85,    97,    97,    88,    89,    97,   770,
      97,   772,    97,     1,    98,    97,    74,    98,    89,    95,
       8,    99,    97,    97,    97,    13,    14,    15,    47,    99,
      97,    36,     9,   661,    99,    99,    97,    25,    26,    27,
      97,    29,    98,    97,    32,   806,    34,    98,    36,    23,
      20,    99,    99,    41,    97,    43,    66,    99,    97,    99,
      97,    99,    97,    97,    97,    53,    78,    97,    89,    63,
      99,    97,    99,    61,    99,   836,    64,    99,    66,    67,
      68,    69,    70,    99,    97,    99,    74,    80,    99,    77,
      99,    79,    99,   721,   146,    71,    46,    85,     1,    99,
      88,    89,    98,    61,    99,     8,    39,    99,    99,    99,
      13,    14,    15,    97,    97,    82,    97,    99,    49,    99,
     881,    69,    25,    26,    27,    97,    29,    35,    99,    97,
      99,    34,    97,    36,    97,    97,    97,    84,    41,    99,
      43,    97,    97,    52,    97,    99,    51,    98,    50,    44,
      53,    72,    76,    99,    99,    97,    99,    98,    61,    97,
      73,    64,    99,    66,    67,    68,    69,    70,    95,    16,
      36,    74,    17,    41,    77,    78,    79,     1,    19,    99,
      99,    97,    85,    97,     8,    88,    89,    99,    59,    13,
      14,    15,    99,    97,    69,    69,    69,    10,    99,    99,
      97,    25,    50,    27,    50,    29,    99,    99,    32,    99,
      34,    99,    36,    97,    97,    97,    45,    41,    99,    43,
      99,    99,    99,    99,    33,    97,    43,    97,    97,    53,
      97,    38,    97,    15,    97,    99,    97,    61,    97,    97,
      64,    81,    66,    67,    68,    69,    70,    97,    26,    32,
      74,    69,    56,    77,    78,    79,     1,    99,    69,    69,
      99,    85,    99,     8,    88,    89,    69,    69,    13,    14,
      69,    97,    99,    65,    97,    72,    18,    83,    97,    80,
      25,    26,    27,    98,    29,    97,    23,    78,    69,    34,
      69,    36,    69,     9,    99,    69,    41,    97,    43,    97,
      80,    97,    78,    24,    99,    99,    99,    97,    53,    97,
      97,    97,    69,    98,    98,    80,    61,    80,    47,    64,
      69,    66,    67,    68,    69,    70,    97,    97,    80,    74,
      99,    97,    77,    78,    79,     1,    97,    99,    97,    99,
      85,    99,     8,    88,    89,    99,    97,    13,    14,    99,
      97,    17,    72,    97,    97,    59,    87,    97,    99,    25,
     338,    27,   478,    29,   343,   120,   601,   600,    34,   137,
      36,   515,   849,   426,   682,    41,   426,    43,   447,   378,
     597,   678,   444,   485,    -1,   657,    -1,    53,   552,    -1,
      -1,    -1,    -1,    -1,    -1,    61,    -1,    -1,    64,    -1,
      66,    67,    68,    69,    70,     1,    -1,    -1,    74,    -1,
      -1,    77,     8,    79,    -1,    -1,    -1,    13,    14,    85,
      -1,    -1,    88,    89,    -1,    -1,    -1,    -1,    -1,    25,
      -1,    27,    -1,    29,    -1,    -1,    -1,    -1,    34,    -1,
      36,    -1,    -1,    -1,    -1,    41,    -1,    43,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    53,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    61,    -1,    -1,    64,    -1,
      66,    67,    68,    69,    70,     1,    -1,    -1,    74,    -1,
      -1,    77,     8,    79,    -1,    -1,    -1,    13,    14,    85,
      -1,    -1,    88,    89,    -1,    -1,    -1,    -1,    -1,    25,
      -1,    27,    -1,    29,    -1,    -1,    -1,    -1,    34,    -1,
      36,    -1,    -1,    -1,    -1,    41,    -1,    43,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    53,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    61,    -1,    -1,    64,    -1,
      66,    67,    68,    69,    70,    -1,    -1,    -1,    74,     8,
      -1,    77,    -1,    79,    13,    14,    -1,    -1,    -1,    85,
      -1,    -1,    88,    89,    -1,    -1,    25,    -1,    27,    -1,
      29,    -1,    -1,    -1,    -1,    34,    -1,    36,    -1,    -1,
      -1,    -1,    41,    -1,    43,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    53,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    61,    -1,    -1,    64,    -1,    66,    67,    68,
      69,    70,    -1,    -1,    -1,    74,    -1,    -1,    77,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,    88,
      89
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   150,   151,     0,    98,    62,    44,   145,   246,   255,
      96,   247,    97,   246,    40,    90,   146,   147,   256,    96,
      31,   156,    90,   256,     7,    55,   147,    97,    42,   155,
       3,     7,    55,   256,   256,   246,    28,   157,   158,   246,
      59,   159,   145,   256,   256,   246,    99,    97,    99,   246,
      60,   162,     3,    94,    94,    97,    99,    31,   157,    97,
      97,     1,    97,    49,   165,    94,    99,    97,    97,   155,
      58,   160,   161,    60,    57,   163,   164,    97,    87,   181,
      28,   246,    99,    97,    97,   246,    99,    97,    48,   166,
     167,    97,    22,   184,    97,    99,    59,   160,    97,    60,
     163,   246,    99,    97,    86,   182,   183,    97,    32,   168,
      99,    97,   160,    97,    97,    99,    49,   166,   246,    99,
      97,    21,   185,   186,    97,    15,   172,    58,    99,    99,
      97,    97,    99,    87,   182,   246,    99,    97,    11,   169,
     170,    97,    26,   174,    57,    48,    37,    54,    99,   202,
      97,    97,    99,    22,   185,   246,    12,   171,   169,     1,
       8,    13,    14,    25,    27,    29,    34,    36,    41,    43,
      53,    61,    64,    66,    67,    68,    69,    70,    74,    77,
      79,    85,    88,    89,   152,   153,   196,   197,   198,   199,
     206,   207,   208,   209,   210,   213,   214,   215,   216,   217,
     218,   219,   227,   228,   229,   234,   235,   245,    97,   152,
     246,    99,    86,    97,    99,    97,    97,   246,    99,   153,
     246,   246,   246,   246,   246,   246,   246,   246,   246,   246,
     246,   246,   246,   246,   246,   246,   246,   246,   246,   246,
     246,   246,   246,   246,    97,    51,    52,   175,   177,   178,
      97,     1,    95,    97,    99,    99,    21,   152,    97,    32,
      97,    97,    99,    97,    99,    97,    99,    97,    99,    97,
      97,    97,    97,    97,    99,    97,    99,    97,    97,    99,
      97,    97,    99,    97,    99,    97,    97,    97,    97,    99,
      97,    99,    97,    99,    97,    99,    97,    99,   246,   246,
      50,   176,   179,    97,    97,    99,    37,     3,   144,   145,
     146,   259,    47,    63,    99,   203,   204,    86,    97,   152,
      97,    72,    75,    76,   248,   249,   250,   248,   248,   248,
     248,     1,   152,   248,   248,   248,   248,   248,   248,   248,
     248,   248,   248,   248,   248,   248,   248,   248,   248,    35,
      84,   211,   212,   248,   248,    15,    97,    97,   246,    99,
      97,   175,   175,    62,   145,   259,    98,    98,     1,    95,
     246,    37,    97,    97,    99,    97,    97,   246,    97,    18,
     200,   201,    71,   251,   252,    97,    99,    99,    99,    99,
      97,    97,    46,   236,     1,    73,   230,    17,   220,    20,
     187,    99,   177,   187,   152,   187,    99,   181,   152,   154,
      10,   224,   225,    99,    99,    99,   246,   246,   248,   248,
      99,   152,   220,    97,   187,   187,    97,    26,   176,    95,
       3,    99,    99,    47,   259,    95,    99,    99,    11,    99,
      44,   254,    97,    99,   254,   246,    99,    97,   246,    97,
     249,    13,    14,    25,    27,    99,    99,   246,   154,   245,
     246,    33,   231,   246,   152,    97,   169,    53,    97,    39,
     193,    97,    82,   190,    68,    49,    59,   184,   243,   244,
      97,    99,   246,    56,   226,    97,    77,    79,    85,    95,
      95,    99,    99,    88,    97,   152,   193,   193,    65,   180,
     211,   212,    97,    37,    37,    98,   145,    37,    37,    97,
      12,    95,   252,    99,   250,    97,     8,   200,    97,    99,
     251,    29,    29,    97,    99,    97,    95,   246,    16,   232,
       1,    95,    99,    97,    19,   188,   189,   171,   175,    97,
      99,   220,    97,    99,    97,   246,    15,    26,    32,   152,
     240,   241,   242,   165,   184,   154,    70,    97,   246,    99,
     224,   259,   259,    88,    88,    99,    97,   152,   152,   246,
     152,   180,   180,    99,    98,    97,   256,    97,    75,    97,
     202,    83,   253,    45,   237,   238,    34,    99,   259,    95,
      97,   245,    17,   145,    97,    24,   221,   222,   246,    99,
      97,   172,   176,    38,   194,   195,    64,    99,    81,   191,
     192,    67,   166,    97,    97,    97,    97,    97,    78,   152,
     239,   242,   152,   239,   240,   242,   152,   239,   240,   241,
     184,   174,   152,    97,    99,    74,    98,    98,    89,    99,
      97,    97,    95,    97,   152,   152,    47,    99,    98,   251,
     249,    97,    95,   246,    99,   246,    99,    97,    36,    98,
     259,     9,   233,    97,    97,    98,   246,    23,   223,   221,
      97,    99,    20,   188,   190,    99,   246,    99,    97,    66,
     246,    99,    97,    99,   160,   152,   175,   169,    99,    97,
      97,   152,   152,    97,   152,   152,   239,   242,   152,    97,
     152,    78,   173,   172,   174,   168,    97,   152,    99,    99,
      89,    99,    99,   259,    99,    97,    97,    63,    99,    99,
      99,    80,   205,   146,   257,    99,    71,    97,    99,    46,
     237,    99,    98,   246,    99,    99,    99,    97,    97,    99,
      99,    97,   193,    61,    97,    99,    39,   194,    97,    99,
      82,   191,    49,    99,    97,   176,   171,    69,   152,    99,
      97,    97,    99,    97,    97,   152,   152,    97,    99,    97,
      97,   152,   173,   168,   172,    99,    97,    35,    84,    51,
      52,    98,    50,    99,    99,    44,    72,    76,   246,    97,
       4,     5,     6,    91,    92,    93,   258,    98,    99,    97,
      73,    99,    95,    16,    36,    17,   220,   152,    41,    19,
      99,    99,    97,    99,    97,    97,    59,    99,    99,    99,
      97,    69,    99,    99,    69,    99,    99,    97,    97,    99,
      69,    99,   152,    97,   152,   172,   173,    10,    99,    97,
      97,    99,    50,    50,    97,    97,    97,     1,    95,    97,
      99,    99,   259,    99,    45,    97,    33,   259,    97,    97,
     152,    97,    43,    38,    81,    97,    15,    26,    32,    99,
      69,    69,    69,    69,    99,    99,    69,    69,    97,    99,
      97,   173,   152,    56,    65,    72,    80,   146,   259,    99,
     204,    18,    83,    97,    98,    97,    99,    97,    97,    97,
      78,    69,    69,    99,    69,    99,   152,    97,    97,    97,
      97,    98,    98,    80,    97,    97,    99,    99,    23,    97,
      78,    69,    97,    99,   252,    99,    99,    99,     9,    24,
      97,    97,    99,    69,    97,    80,    80,    80,    97,    97,
      69,   251,    99,    72,    97
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
#line 327 "frontend-parser.yy"
    {
      // initialisation (for multiple input files, i.e. `consistency' mode)
      frontend_lineno = 1;
      currentJoinCondition = standardJoinCondition();
      globals::temporaryAttributeMap.clear();
      globals::ASTEmap.clear();
      globals::ASTEid = 1;
      globals::ASTE_inputChannels.clear();
      globals::ASTE_outputChannels.clear();
      globals::ASTE_correlationSetNames.clear();
      globals::ASTE_variableNames.clear();
      globals::ASTE_partnerLinkNames.clear();
      globals::ASTE_linkNames.clear();
      globals::ASTE_startActivities.clear();
      globals::ASTE_linkIdMap.clear();
      globals::ASTE_scopeNames.clear();
      globals::ASTE_partnerLinks.clear();
      globals::joinCondition_links.clear();
      globals::joinCondition_linkStatus.clear();
      globals::joinCondition_result.clear();
      globals::unknown_elements.clear();
      globals::parsing = true;
      globals::static_analysis_errors = 0;
      globals::other_errors = 0;
    }
    break;

  case 3:
#line 355 "frontend-parser.yy"
    { globals::AST = (yyval.yt_tProcess) = Process((yyvsp[(8) - (20)].yt_tPartnerLink_list), (yyvsp[(9) - (20)].yt_tPartner_list), (yyvsp[(10) - (20)].yt_tMessageExchange_list), (yyvsp[(11) - (20)].yt_tVariable_list), (yyvsp[(12) - (20)].yt_tCorrelationSet_list), (yyvsp[(13) - (20)].yt_tFaultHandlers), (yyvsp[(15) - (20)].yt_tEventHandlers), (yyvsp[(16) - (20)].yt_activity), (yyvsp[(4) - (20)].yt_integer)); }
    break;

  case 4:
#line 362 "frontend-parser.yy"
    { (yyval.yt_activity) = (yyvsp[(1) - (1)].yt_activity); }
    break;

  case 5:
#line 364 "frontend-parser.yy"
    { (yyval.yt_activity) = (yyvsp[(2) - (2)].yt_activity);
      genericError(100, globals::last_error_token, globals::last_error_line, ERRORLEVEL_NOTICE); }
    break;

  case 6:
#line 369 "frontend-parser.yy"
    { (yyval.yt_activity) = activityReceive((yyvsp[(1) - (1)].yt_tReceive));		}
    break;

  case 7:
#line 370 "frontend-parser.yy"
    { (yyval.yt_activity) = activityReply((yyvsp[(1) - (1)].yt_tReply));		}
    break;

  case 8:
#line 371 "frontend-parser.yy"
    { (yyval.yt_activity) = activityInvoke((yyvsp[(1) - (1)].yt_tInvoke));		}
    break;

  case 9:
#line 372 "frontend-parser.yy"
    { (yyval.yt_activity) = activityAssign((yyvsp[(1) - (1)].yt_tAssign));		}
    break;

  case 10:
#line 373 "frontend-parser.yy"
    { (yyval.yt_activity) = activityValidate((yyvsp[(1) - (1)].yt_tValidate));		}
    break;

  case 11:
#line 374 "frontend-parser.yy"
    { (yyval.yt_activity) = activityEmpty((yyvsp[(1) - (1)].yt_tEmpty));		}
    break;

  case 12:
#line 375 "frontend-parser.yy"
    { (yyval.yt_activity) = activityOpaqueActivity((yyvsp[(1) - (1)].yt_tOpaqueActivity));	}
    break;

  case 13:
#line 376 "frontend-parser.yy"
    { (yyval.yt_activity) = (yyvsp[(1) - (1)].yt_activity);				}
    break;

  case 14:
#line 377 "frontend-parser.yy"
    { (yyval.yt_activity) = activityWait((yyvsp[(1) - (1)].yt_tWait));		}
    break;

  case 15:
#line 378 "frontend-parser.yy"
    { (yyval.yt_activity) = activityExit((yyvsp[(1) - (1)].yt_tExit));		}
    break;

  case 16:
#line 379 "frontend-parser.yy"
    { (yyval.yt_activity) = activityThrow((yyvsp[(1) - (1)].yt_tThrow));		}
    break;

  case 17:
#line 380 "frontend-parser.yy"
    { (yyval.yt_activity) = activityRethrow((yyvsp[(1) - (1)].yt_tRethrow));		}
    break;

  case 18:
#line 381 "frontend-parser.yy"
    { (yyval.yt_activity) = activityCompensate((yyvsp[(1) - (1)].yt_tCompensate));		}
    break;

  case 19:
#line 382 "frontend-parser.yy"
    { (yyval.yt_activity) = activityCompensate((yyvsp[(1) - (1)].yt_tCompensate));		}
    break;

  case 20:
#line 383 "frontend-parser.yy"
    { (yyval.yt_activity) = activitySequence((yyvsp[(1) - (1)].yt_tSequence));		}
    break;

  case 21:
#line 384 "frontend-parser.yy"
    { (yyval.yt_activity) = activityIf((yyvsp[(1) - (1)].yt_tIf));			}
    break;

  case 22:
#line 385 "frontend-parser.yy"
    { (yyval.yt_activity) = activityWhile((yyvsp[(1) - (1)].yt_tWhile));		}
    break;

  case 23:
#line 386 "frontend-parser.yy"
    { (yyval.yt_activity) = activityRepeatUntil((yyvsp[(1) - (1)].yt_tRepeatUntil));		}
    break;

  case 24:
#line 387 "frontend-parser.yy"
    { (yyval.yt_activity) = activityForEach((yyvsp[(1) - (1)].yt_tForEach));		}
    break;

  case 25:
#line 388 "frontend-parser.yy"
    { (yyval.yt_activity) = activityFlow((yyvsp[(1) - (1)].yt_tFlow));		}
    break;

  case 26:
#line 389 "frontend-parser.yy"
    { (yyval.yt_activity) = activityPick((yyvsp[(1) - (1)].yt_tPick));		}
    break;

  case 27:
#line 390 "frontend-parser.yy"
    { (yyval.yt_activity) = activityScope((yyvsp[(1) - (1)].yt_tScope));		}
    break;

  case 28:
#line 395 "frontend-parser.yy"
    { (yyval.yt_activity_list) = Consactivity_list((yyvsp[(1) - (2)].yt_activity), Nilactivity_list()); }
    break;

  case 29:
#line 397 "frontend-parser.yy"
    { (yyval.yt_activity_list) = Consactivity_list((yyvsp[(1) - (3)].yt_activity), (yyvsp[(3) - (3)].yt_activity_list)); }
    break;

  case 38:
#line 432 "frontend-parser.yy"
    { (yyval.yt_tPartnerLink_list) = NiltPartnerLink_list(); }
    break;

  case 39:
#line 434 "frontend-parser.yy"
    { (yyval.yt_tPartnerLink_list) = (yyvsp[(4) - (7)].yt_tPartnerLink_list); }
    break;

  case 40:
#line 439 "frontend-parser.yy"
    { (yyval.yt_tPartnerLink_list) = ConstPartnerLink_list((yyvsp[(1) - (2)].yt_tPartnerLink), NiltPartnerLink_list()); }
    break;

  case 41:
#line 441 "frontend-parser.yy"
    { (yyval.yt_tPartnerLink_list) = ConstPartnerLink_list((yyvsp[(1) - (3)].yt_tPartnerLink), (yyvsp[(3) - (3)].yt_tPartnerLink_list)); }
    break;

  case 42:
#line 446 "frontend-parser.yy"
    { (yyval.yt_tPartnerLink) = PartnerLink((yyvsp[(2) - (5)].yt_integer)); }
    break;

  case 43:
#line 448 "frontend-parser.yy"
    { (yyval.yt_tPartnerLink) = PartnerLink((yyvsp[(2) - (3)].yt_integer)); }
    break;

  case 44:
#line 458 "frontend-parser.yy"
    { (yyval.yt_tPartner_list) = NiltPartner_list(); }
    break;

  case 45:
#line 460 "frontend-parser.yy"
    { (yyval.yt_tPartner_list) = (yyvsp[(3) - (6)].yt_tPartner_list); }
    break;

  case 46:
#line 462 "frontend-parser.yy"
    { (yyval.yt_tPartner_list) = NiltPartner_list(); genericError(101, "", toString(frontend_lineno-1), ERRORLEVEL_NOTICE); }
    break;

  case 47:
#line 467 "frontend-parser.yy"
    { (yyval.yt_tPartner_list) = ConstPartner_list((yyvsp[(1) - (2)].yt_tPartner), NiltPartner_list()); }
    break;

  case 48:
#line 469 "frontend-parser.yy"
    { (yyval.yt_tPartner_list) = ConstPartner_list((yyvsp[(1) - (3)].yt_tPartner), (yyvsp[(3) - (3)].yt_tPartner_list)); }
    break;

  case 49:
#line 474 "frontend-parser.yy"
    { (yyval.yt_tPartner) = Partner((yyvsp[(4) - (6)].yt_tPartnerLink_list), (yyvsp[(2) - (6)].yt_integer)); }
    break;

  case 50:
#line 484 "frontend-parser.yy"
    { (yyval.yt_tMessageExchange_list) = NiltMessageExchange_list(); }
    break;

  case 51:
#line 486 "frontend-parser.yy"
    { (yyval.yt_tMessageExchange_list) = (yyvsp[(3) - (6)].yt_tMessageExchange_list); }
    break;

  case 52:
#line 491 "frontend-parser.yy"
    { (yyval.yt_tMessageExchange_list) = ConstMessageExchange_list((yyvsp[(1) - (2)].yt_tMessageExchange), NiltMessageExchange_list()); }
    break;

  case 53:
#line 493 "frontend-parser.yy"
    { (yyval.yt_tMessageExchange_list) = ConstMessageExchange_list((yyvsp[(1) - (3)].yt_tMessageExchange), (yyvsp[(3) - (3)].yt_tMessageExchange_list)); }
    break;

  case 54:
#line 498 "frontend-parser.yy"
    { (yyval.yt_tMessageExchange) = MessageExchange((yyvsp[(2) - (5)].yt_integer)); }
    break;

  case 55:
#line 500 "frontend-parser.yy"
    { (yyval.yt_tMessageExchange) = MessageExchange((yyvsp[(2) - (3)].yt_integer)); }
    break;

  case 56:
#line 509 "frontend-parser.yy"
    { (yyval.yt_tFaultHandlers) = volatile_standardFaultHandlers(mkinteger(0)); }
    break;

  case 57:
#line 511 "frontend-parser.yy"
    { (yyval.yt_tFaultHandlers) = FaultHandlers((yyvsp[(3) - (7)].yt_tCatch_list), (yyvsp[(4) - (7)].yt_tCatchAll), mkinteger(0)); }
    break;

  case 58:
#line 516 "frontend-parser.yy"
    { (yyval.yt_tCatch_list) = NiltCatch_list(); }
    break;

  case 59:
#line 518 "frontend-parser.yy"
    { (yyval.yt_tCatch_list) = ConstCatch_list((yyvsp[(1) - (2)].yt_tCatch), (yyvsp[(2) - (2)].yt_tCatch_list)); }
    break;

  case 60:
#line 523 "frontend-parser.yy"
    { (yyval.yt_tCatch) = Catch((yyvsp[(4) - (8)].yt_activity), (yyvsp[(2) - (8)].yt_integer)); }
    break;

  case 61:
#line 528 "frontend-parser.yy"
    { (yyval.yt_tCatchAll) = NoCatchAll(); }
    break;

  case 62:
#line 530 "frontend-parser.yy"
    { (yyval.yt_tCatchAll) = CatchAll((yyvsp[(4) - (8)].yt_activity), (yyvsp[(2) - (8)].yt_integer)); }
    break;

  case 63:
#line 540 "frontend-parser.yy"
    { (yyval.yt_tCompensationHandler) = volatile_standardCompensationHandler(mkinteger(0)); }
    break;

  case 64:
#line 542 "frontend-parser.yy"
    { (yyval.yt_tCompensationHandler) = CompensationHandler((yyvsp[(3) - (7)].yt_activity), mkinteger(0)); }
    break;

  case 65:
#line 552 "frontend-parser.yy"
    { (yyval.yt_tTerminationHandler) = volatile_standardTerminationHandler(mkinteger(0)); }
    break;

  case 66:
#line 554 "frontend-parser.yy"
    { (yyval.yt_tTerminationHandler) = TerminationHandler((yyvsp[(3) - (7)].yt_activity), mkinteger(0)); }
    break;

  case 67:
#line 564 "frontend-parser.yy"
    { (yyval.yt_tEventHandlers) = emptyEventHandlers(mkinteger(0)); }
    break;

  case 68:
#line 566 "frontend-parser.yy"
    { (yyval.yt_tEventHandlers) = EventHandlers((yyvsp[(3) - (7)].yt_tOnMessage_list), (yyvsp[(4) - (7)].yt_tOnAlarm_list), mkinteger(0)); }
    break;

  case 69:
#line 571 "frontend-parser.yy"
    { (yyval.yt_tOnMessage_list) = NiltOnMessage_list(); }
    break;

  case 70:
#line 573 "frontend-parser.yy"
    { (yyval.yt_tOnMessage_list) = ConstOnMessage_list((yyvsp[(1) - (3)].yt_tOnMessage), (yyvsp[(3) - (3)].yt_tOnMessage_list)); }
    break;

  case 71:
#line 575 "frontend-parser.yy"
    { (yyval.yt_tOnMessage_list) = ConstOnMessage_list((yyvsp[(1) - (3)].yt_tOnMessage), (yyvsp[(3) - (3)].yt_tOnMessage_list)); }
    break;

  case 72:
#line 580 "frontend-parser.yy"
    { (yyval.yt_tOnAlarm_list) = NiltOnAlarm_list(); }
    break;

  case 73:
#line 582 "frontend-parser.yy"
    { (yyval.yt_tOnAlarm_list) = ConstOnAlarm_list((yyvsp[(1) - (3)].yt_tOnAlarm), (yyvsp[(3) - (3)].yt_tOnAlarm_list)); }
    break;

  case 74:
#line 587 "frontend-parser.yy"
    { (yyval.yt_tOnMessage) = OnMessage((yyvsp[(4) - (9)].yt_tCorrelation_list), (yyvsp[(5) - (9)].yt_tFromPart_list), (yyvsp[(6) - (9)].yt_activity), (yyvsp[(2) - (9)].yt_integer)); }
    break;

  case 75:
#line 592 "frontend-parser.yy"
    { (yyval.yt_tOnMessage) = OnMessage((yyvsp[(4) - (9)].yt_tCorrelation_list), (yyvsp[(5) - (9)].yt_tFromPart_list), (yyvsp[(6) - (9)].yt_activity), (yyvsp[(2) - (9)].yt_integer)); }
    break;

  case 76:
#line 597 "frontend-parser.yy"
    { (yyval.yt_tOnAlarm) = OnAlarm((yyvsp[(4) - (8)].yt_tRepeatEvery), (yyvsp[(5) - (8)].yt_activity), (yyvsp[(2) - (8)].yt_integer)); }
    break;

  case 77:
#line 599 "frontend-parser.yy"
    { (yyval.yt_tOnAlarm) = OnAlarm((yyvsp[(5) - (9)].yt_tRepeatEvery), (yyvsp[(6) - (9)].yt_activity), (yyvsp[(2) - (9)].yt_integer)); }
    break;

  case 78:
#line 601 "frontend-parser.yy"
    { (yyval.yt_tOnAlarm) = OnAlarm((yyvsp[(5) - (9)].yt_tRepeatEvery), (yyvsp[(6) - (9)].yt_activity), (yyvsp[(2) - (9)].yt_integer)); }
    break;

  case 79:
#line 606 "frontend-parser.yy"
    { (yyval.yt_tRepeatEvery) = emptyRepeatEvery(mkinteger(globals::ASTEid++)); }
    break;

  case 80:
#line 608 "frontend-parser.yy"
    { (yyval.yt_tRepeatEvery) = RepeatEvery(mkinteger(globals::ASTEid++)); }
    break;

  case 81:
#line 621 "frontend-parser.yy"
    { (yyval.yt_tVariable_list) = NiltVariable_list(); }
    break;

  case 82:
#line 623 "frontend-parser.yy"
    { (yyval.yt_tVariable_list) = (yyvsp[(3) - (6)].yt_tVariable_list); }
    break;

  case 83:
#line 628 "frontend-parser.yy"
    { (yyval.yt_tVariable_list) = ConstVariable_list((yyvsp[(1) - (2)].yt_tVariable), NiltVariable_list()); }
    break;

  case 84:
#line 630 "frontend-parser.yy"
    { (yyval.yt_tVariable_list) = ConstVariable_list((yyvsp[(1) - (3)].yt_tVariable), (yyvsp[(3) - (3)].yt_tVariable_list)); }
    break;

  case 85:
#line 635 "frontend-parser.yy"
    { (yyval.yt_tVariable) = Variable((yyvsp[(2) - (5)].yt_integer)); }
    break;

  case 86:
#line 637 "frontend-parser.yy"
    { (yyval.yt_tVariable) = Variable((yyvsp[(2) - (7)].yt_integer)); }
    break;

  case 87:
#line 639 "frontend-parser.yy"
    { (yyval.yt_tVariable) = Variable((yyvsp[(2) - (3)].yt_integer)); }
    break;

  case 88:
#line 649 "frontend-parser.yy"
    { (yyval.yt_tCorrelationSet_list) = NiltCorrelationSet_list(); }
    break;

  case 89:
#line 651 "frontend-parser.yy"
    { (yyval.yt_tCorrelationSet_list) = (yyvsp[(3) - (6)].yt_tCorrelationSet_list); }
    break;

  case 90:
#line 656 "frontend-parser.yy"
    { (yyval.yt_tCorrelationSet_list) = ConstCorrelationSet_list((yyvsp[(1) - (2)].yt_tCorrelationSet), NiltCorrelationSet_list()); }
    break;

  case 91:
#line 658 "frontend-parser.yy"
    { (yyval.yt_tCorrelationSet_list) = ConstCorrelationSet_list((yyvsp[(1) - (3)].yt_tCorrelationSet), (yyvsp[(3) - (3)].yt_tCorrelationSet_list)); }
    break;

  case 92:
#line 663 "frontend-parser.yy"
    { (yyval.yt_tCorrelationSet) = CorrelationSet((yyvsp[(2) - (5)].yt_integer)); }
    break;

  case 93:
#line 665 "frontend-parser.yy"
    { (yyval.yt_tCorrelationSet) = CorrelationSet((yyvsp[(2) - (3)].yt_integer)); }
    break;

  case 94:
#line 675 "frontend-parser.yy"
    { (yyval.yt_tCorrelation_list) = NiltCorrelation_list(); }
    break;

  case 95:
#line 677 "frontend-parser.yy"
    { (yyval.yt_tCorrelation_list) = (yyvsp[(3) - (6)].yt_tCorrelation_list); }
    break;

  case 96:
#line 682 "frontend-parser.yy"
    { (yyval.yt_tCorrelation_list) = ConstCorrelation_list((yyvsp[(1) - (2)].yt_tCorrelation), NiltCorrelation_list()); }
    break;

  case 97:
#line 684 "frontend-parser.yy"
    { (yyval.yt_tCorrelation_list) = ConstCorrelation_list((yyvsp[(1) - (3)].yt_tCorrelation), (yyvsp[(3) - (3)].yt_tCorrelation_list)); }
    break;

  case 98:
#line 689 "frontend-parser.yy"
    { (yyval.yt_tCorrelation) = Correlation((yyvsp[(2) - (5)].yt_integer)); }
    break;

  case 99:
#line 691 "frontend-parser.yy"
    { (yyval.yt_tCorrelation) = Correlation((yyvsp[(2) - (3)].yt_integer)); }
    break;

  case 100:
#line 701 "frontend-parser.yy"
    { (yyval.yt_tToPart_list) = NiltToPart_list(); }
    break;

  case 101:
#line 703 "frontend-parser.yy"
    { (yyval.yt_tToPart_list) = (yyvsp[(3) - (6)].yt_tToPart_list); }
    break;

  case 102:
#line 708 "frontend-parser.yy"
    { (yyval.yt_tToPart_list) = ConstToPart_list((yyvsp[(1) - (2)].yt_tToPart), NiltToPart_list()); }
    break;

  case 103:
#line 710 "frontend-parser.yy"
    { (yyval.yt_tToPart_list) = ConstToPart_list((yyvsp[(1) - (3)].yt_tToPart), (yyvsp[(3) - (3)].yt_tToPart_list)); }
    break;

  case 104:
#line 715 "frontend-parser.yy"
    { (yyval.yt_tToPart) = ToPart((yyvsp[(2) - (5)].yt_integer)); }
    break;

  case 105:
#line 717 "frontend-parser.yy"
    { (yyval.yt_tToPart) = ToPart((yyvsp[(2) - (3)].yt_integer)); }
    break;

  case 106:
#line 722 "frontend-parser.yy"
    { (yyval.yt_tFromPart_list) = NiltFromPart_list(); }
    break;

  case 107:
#line 724 "frontend-parser.yy"
    { (yyval.yt_tFromPart_list) = (yyvsp[(3) - (6)].yt_tFromPart_list); }
    break;

  case 108:
#line 729 "frontend-parser.yy"
    { (yyval.yt_tFromPart_list) = ConstFromPart_list((yyvsp[(1) - (2)].yt_tFromPart), NiltFromPart_list()); }
    break;

  case 109:
#line 731 "frontend-parser.yy"
    { (yyval.yt_tFromPart_list) = ConstFromPart_list((yyvsp[(1) - (3)].yt_tFromPart), (yyvsp[(3) - (3)].yt_tFromPart_list)); }
    break;

  case 110:
#line 736 "frontend-parser.yy"
    { (yyval.yt_tFromPart) = FromPart((yyvsp[(2) - (5)].yt_integer)); }
    break;

  case 111:
#line 738 "frontend-parser.yy"
    { (yyval.yt_tFromPart) = FromPart((yyvsp[(2) - (3)].yt_integer)); }
    break;

  case 112:
#line 751 "frontend-parser.yy"
    { (yyval.yt_tReceive) = Receive((yyvsp[(4) - (8)].yt_standardElements), (yyvsp[(5) - (8)].yt_tCorrelation_list), (yyvsp[(6) - (8)].yt_tFromPart_list), (yyvsp[(2) - (8)].yt_integer)); }
    break;

  case 113:
#line 753 "frontend-parser.yy"
    { (yyval.yt_tReceive) = Receive(NoStandardElements(), NiltCorrelation_list(), NiltFromPart_list(), (yyvsp[(2) - (3)].yt_integer)); }
    break;

  case 114:
#line 763 "frontend-parser.yy"
    { (yyval.yt_tReply) = Reply((yyvsp[(4) - (8)].yt_standardElements), (yyvsp[(5) - (8)].yt_tCorrelation_list), (yyvsp[(6) - (8)].yt_tToPart_list), (yyvsp[(2) - (8)].yt_integer)); }
    break;

  case 115:
#line 765 "frontend-parser.yy"
    { (yyval.yt_tReply) = Reply(NoStandardElements(), NiltCorrelation_list(), NiltToPart_list(), (yyvsp[(2) - (3)].yt_integer)); }
    break;

  case 116:
#line 775 "frontend-parser.yy"
    { (yyval.yt_tInvoke) = volatile_Invoke(NoStandardElements(), NiltCorrelation_list(), NiltToPart_list(), NiltFromPart_list(), (yyvsp[(2) - (3)].yt_integer)); }
    break;

  case 117:
#line 777 "frontend-parser.yy"
    { (yyval.yt_tInvoke) = volatile_annotatedInvoke((yyvsp[(4) - (12)].yt_standardElements), (yyvsp[(5) - (12)].yt_tCorrelation_list), (yyvsp[(6) - (12)].yt_tCatch_list), (yyvsp[(7) - (12)].yt_tCatchAll), (yyvsp[(8) - (12)].yt_tCompensationHandler), (yyvsp[(9) - (12)].yt_tToPart_list), (yyvsp[(10) - (12)].yt_tFromPart_list), (yyvsp[(2) - (12)].yt_integer)); }
    break;

  case 118:
#line 787 "frontend-parser.yy"
    { (yyval.yt_tAssign) = Assign((yyvsp[(4) - (7)].yt_standardElements), (yyvsp[(5) - (7)].yt_tCopy_list), (yyvsp[(2) - (7)].yt_integer)); }
    break;

  case 119:
#line 792 "frontend-parser.yy"
    { (yyval.yt_tCopy_list) = ConstCopy_list((yyvsp[(1) - (2)].yt_tCopy), NiltCopy_list()); }
    break;

  case 120:
#line 794 "frontend-parser.yy"
    { (yyval.yt_tCopy_list) = ConstCopy_list((yyvsp[(1) - (3)].yt_tCopy), (yyvsp[(3) - (3)].yt_tCopy_list)); }
    break;

  case 121:
#line 799 "frontend-parser.yy"
    { (yyval.yt_tCopy) = Copy((yyvsp[(4) - (9)].yt_tFrom), (yyvsp[(6) - (9)].yt_tTo), (yyvsp[(2) - (9)].yt_integer)); }
    break;

  case 122:
#line 804 "frontend-parser.yy"
    { (yyval.yt_tFrom) = From((yyvsp[(2) - (5)].yt_integer)); }
    break;

  case 123:
#line 806 "frontend-parser.yy"
    { globals::temporaryAttributeMap[(yyvsp[(2) - (7)].yt_integer)->value]["has_expression"] = "true"; (yyval.yt_tFrom) = From((yyvsp[(2) - (7)].yt_integer)); }
    break;

  case 124:
#line 808 "frontend-parser.yy"
    { globals::temporaryAttributeMap[(yyvsp[(2) - (7)].yt_integer)->value]["has_expression"] = "true"; (yyval.yt_tFrom) = From((yyvsp[(2) - (7)].yt_integer)); }
    break;

  case 125:
#line 810 "frontend-parser.yy"
    { globals::temporaryAttributeMap[(yyvsp[(2) - (7)].yt_integer)->value]["has_literal"] = "true"; (yyval.yt_tFrom) =  From((yyvsp[(2) - (7)].yt_integer)); }
    break;

  case 126:
#line 812 "frontend-parser.yy"
    { globals::temporaryAttributeMap[(yyvsp[(2) - (7)].yt_integer)->value]["has_query"] = "true"; (yyval.yt_tFrom) = From((yyvsp[(2) - (7)].yt_integer)); }
    break;

  case 127:
#line 814 "frontend-parser.yy"
    { (yyval.yt_tFrom) = From((yyvsp[(2) - (3)].yt_integer)); }
    break;

  case 128:
#line 816 "frontend-parser.yy"
    { genericError(102, "from", toString(frontend_lineno-1), ERRORLEVEL_NOTICE);
      (yyval.yt_tFrom) = From((yyvsp[(2) - (4)].yt_integer)); }
    break;

  case 129:
#line 819 "frontend-parser.yy"
    { (yyval.yt_tFrom) = From(mkinteger(0)); }
    break;

  case 130:
#line 824 "frontend-parser.yy"
    { (yyval.yt_casestring) = (yyvsp[(3) - (6)].yt_casestring); }
    break;

  case 131:
#line 826 "frontend-parser.yy"
    { genericError(112, "", toString(frontend_lineno-1), ERRORLEVEL_NOTICE);
      (yyval.yt_casestring) = mkcasestring(""); }
    break;

  case 133:
#line 836 "frontend-parser.yy"
    { (yyval.yt_tTo) = To((yyvsp[(2) - (5)].yt_integer)); }
    break;

  case 134:
#line 838 "frontend-parser.yy"
    { globals::temporaryAttributeMap[(yyvsp[(2) - (7)].yt_integer)->value]["has_expression"] = "true"; (yyval.yt_tTo) = To((yyvsp[(2) - (7)].yt_integer)); }
    break;

  case 135:
#line 840 "frontend-parser.yy"
    { globals::temporaryAttributeMap[(yyvsp[(2) - (7)].yt_integer)->value]["has_expression"] = "true"; (yyval.yt_tTo) = To((yyvsp[(2) - (7)].yt_integer)); }
    break;

  case 136:
#line 842 "frontend-parser.yy"
    { globals::temporaryAttributeMap[(yyvsp[(2) - (7)].yt_integer)->value]["has_query"] = "true"; (yyval.yt_tTo) = To((yyvsp[(2) - (7)].yt_integer)); }
    break;

  case 137:
#line 844 "frontend-parser.yy"
    { (yyval.yt_tTo) = To((yyvsp[(2) - (3)].yt_integer)); }
    break;

  case 138:
#line 846 "frontend-parser.yy"
    { genericError(102, "to", toString(frontend_lineno-1), ERRORLEVEL_NOTICE);
      (yyval.yt_tTo) = To((yyvsp[(2) - (4)].yt_integer)); }
    break;

  case 139:
#line 857 "frontend-parser.yy"
    { (yyval.yt_tValidate) = Validate((yyvsp[(4) - (6)].yt_standardElements), (yyvsp[(2) - (6)].yt_integer)); }
    break;

  case 140:
#line 859 "frontend-parser.yy"
    { (yyval.yt_tValidate) = Validate(NoStandardElements(), (yyvsp[(2) - (3)].yt_integer)); }
    break;

  case 141:
#line 869 "frontend-parser.yy"
    { (yyval.yt_tEmpty) = Empty((yyvsp[(4) - (6)].yt_standardElements), (yyvsp[(2) - (6)].yt_integer)); }
    break;

  case 142:
#line 871 "frontend-parser.yy"
    { (yyval.yt_tEmpty) = Empty(NoStandardElements(), (yyvsp[(2) - (3)].yt_integer)); }
    break;

  case 143:
#line 881 "frontend-parser.yy"
    { (yyval.yt_tOpaqueActivity) = OpaqueActivity((yyvsp[(4) - (6)].yt_standardElements), (yyvsp[(2) - (6)].yt_integer));
      genericError(116, "", toString(frontend_lineno-1), ERRORLEVEL_NOTICE); }
    break;

  case 144:
#line 884 "frontend-parser.yy"
    { (yyval.yt_tOpaqueActivity) = OpaqueActivity(NoStandardElements(), (yyvsp[(2) - (3)].yt_integer));
      genericError(116, "", toString(frontend_lineno-1), ERRORLEVEL_NOTICE); }
    break;

  case 145:
#line 895 "frontend-parser.yy"
    { (yyval.yt_activity) = (yyvsp[(4) - (7)].yt_activity); }
    break;

  case 146:
#line 897 "frontend-parser.yy"
    { (yyval.yt_activity) = (yyval.yt_activity) = activityOpaqueActivity(OpaqueActivity(NoStandardElements(), (yyvsp[(2) - (7)].yt_integer)));
      genericError(133, "", toString(frontend_lineno-1), ERRORLEVEL_NOTICE); }
    break;

  case 147:
#line 908 "frontend-parser.yy"
    { (yyval.yt_tWait) = Wait((yyvsp[(4) - (6)].yt_standardElements), (yyvsp[(2) - (6)].yt_integer)); }
    break;

  case 148:
#line 910 "frontend-parser.yy"
    { (yyval.yt_tWait) = Wait((yyvsp[(5) - (7)].yt_standardElements), (yyvsp[(2) - (7)].yt_integer)); }
    break;

  case 149:
#line 912 "frontend-parser.yy"
    { (yyval.yt_tWait) = Wait((yyvsp[(5) - (7)].yt_standardElements), (yyvsp[(2) - (7)].yt_integer)); }
    break;

  case 150:
#line 914 "frontend-parser.yy"
    { (yyval.yt_tWait) = Wait(NoStandardElements(), (yyvsp[(2) - (3)].yt_integer)); }
    break;

  case 153:
#line 935 "frontend-parser.yy"
    { (yyval.yt_tExit) = Exit((yyvsp[(4) - (6)].yt_standardElements), (yyvsp[(2) - (6)].yt_integer)); }
    break;

  case 154:
#line 937 "frontend-parser.yy"
    { (yyval.yt_tExit) = Exit(NoStandardElements(), (yyvsp[(2) - (3)].yt_integer)); }
    break;

  case 155:
#line 939 "frontend-parser.yy"
    { (yyval.yt_tExit) = Exit((yyvsp[(4) - (6)].yt_standardElements), (yyvsp[(2) - (6)].yt_integer)); }
    break;

  case 156:
#line 941 "frontend-parser.yy"
    { (yyval.yt_tExit) = Exit(NoStandardElements(), (yyvsp[(2) - (3)].yt_integer)); }
    break;

  case 157:
#line 951 "frontend-parser.yy"
    { (yyval.yt_tThrow) = Throw((yyvsp[(4) - (6)].yt_standardElements), (yyvsp[(2) - (6)].yt_integer)); }
    break;

  case 158:
#line 953 "frontend-parser.yy"
    { (yyval.yt_tThrow) = Throw(NoStandardElements(), (yyvsp[(2) - (3)].yt_integer)); }
    break;

  case 159:
#line 963 "frontend-parser.yy"
    { (yyval.yt_tRethrow) = Rethrow((yyvsp[(4) - (6)].yt_standardElements), (yyvsp[(2) - (6)].yt_integer)); }
    break;

  case 160:
#line 965 "frontend-parser.yy"
    { (yyval.yt_tRethrow) = Rethrow(NoStandardElements(), (yyvsp[(2) - (3)].yt_integer)); }
    break;

  case 161:
#line 980 "frontend-parser.yy"
    { if(globals::temporaryAttributeMap[(yyvsp[(2) - (6)].yt_integer)->value]["scope"] == "")
        (yyval.yt_tCompensate) = Compensate((yyvsp[(4) - (6)].yt_standardElements), (yyvsp[(2) - (6)].yt_integer));
      else
        (yyval.yt_tCompensate) = CompensateScope((yyvsp[(4) - (6)].yt_standardElements), mkcasestring(globals::temporaryAttributeMap[(yyvsp[(2) - (6)].yt_integer)->value]["scope"].c_str()), (yyvsp[(2) - (6)].yt_integer)); }
    break;

  case 162:
#line 985 "frontend-parser.yy"
    { if(globals::temporaryAttributeMap[(yyvsp[(2) - (3)].yt_integer)->value]["scope"] == "")
        (yyval.yt_tCompensate) = Compensate(NoStandardElements(), (yyvsp[(2) - (3)].yt_integer));
      else
        (yyval.yt_tCompensate) = CompensateScope(NoStandardElements(), mkcasestring(globals::temporaryAttributeMap[(yyvsp[(2) - (3)].yt_integer)->value]["scope"].c_str()), (yyvsp[(2) - (3)].yt_integer)); }
    break;

  case 163:
#line 994 "frontend-parser.yy"
    { (yyval.yt_tCompensate) = CompensateScope((yyvsp[(4) - (6)].yt_standardElements), mkcasestring(globals::temporaryAttributeMap[(yyvsp[(2) - (6)].yt_integer)->value]["target"].c_str()), (yyvsp[(2) - (6)].yt_integer)); }
    break;

  case 164:
#line 996 "frontend-parser.yy"
    { (yyval.yt_tCompensate) = CompensateScope(NoStandardElements(), mkcasestring(globals::temporaryAttributeMap[(yyvsp[(2) - (3)].yt_integer)->value]["target"].c_str()), (yyvsp[(2) - (3)].yt_integer)); }
    break;

  case 165:
#line 1006 "frontend-parser.yy"
    { (yyval.yt_tSequence) = Sequence((yyvsp[(4) - (7)].yt_standardElements), (yyvsp[(5) - (7)].yt_activity_list), (yyvsp[(2) - (7)].yt_integer)); }
    break;

  case 166:
#line 1016 "frontend-parser.yy"
    { (yyval.yt_tIf) = If((yyvsp[(4) - (11)].yt_standardElements), ConstElseIf_list(ElseIf((yyvsp[(6) - (11)].yt_activity), mkinteger(0)), (yyvsp[(8) - (11)].yt_tElseIf_list)), (yyvsp[(9) - (11)].yt_tElse), (yyvsp[(2) - (11)].yt_integer)); }
    break;

  case 167:
#line 1018 "frontend-parser.yy"
    { (yyval.yt_tIf) = If((yyvsp[(4) - (8)].yt_standardElements), (yyvsp[(5) - (8)].yt_tElseIf_list), (yyvsp[(6) - (8)].yt_tElse), (yyvsp[(2) - (8)].yt_integer)); }
    break;

  case 168:
#line 1023 "frontend-parser.yy"
    { genericError(103, "", toString(frontend_lineno-1), ERRORLEVEL_NOTICE);
      (yyval.yt_casestring) = mkcasestring(""); }
    break;

  case 169:
#line 1026 "frontend-parser.yy"
    { (yyval.yt_casestring) = (yyvsp[(4) - (8)].yt_casestring); }
    break;

  case 170:
#line 1028 "frontend-parser.yy"
    { (yyval.yt_casestring) = mkcasestring("opaque"); }
    break;

  case 171:
#line 1033 "frontend-parser.yy"
    { (yyval.yt_tElseIf_list) = NiltElseIf_list(); }
    break;

  case 172:
#line 1035 "frontend-parser.yy"
    { (yyval.yt_tElseIf_list) = ConstElseIf_list((yyvsp[(1) - (2)].yt_tElseIf), (yyvsp[(2) - (2)].yt_tElseIf_list)); }
    break;

  case 173:
#line 1040 "frontend-parser.yy"
    { (yyval.yt_tElseIf) = ElseIf((yyvsp[(5) - (9)].yt_activity), (yyvsp[(2) - (9)].yt_integer)); }
    break;

  case 174:
#line 1045 "frontend-parser.yy"
    { (yyval.yt_tElse) = NoElse(mkinteger(0)); }
    break;

  case 175:
#line 1047 "frontend-parser.yy"
    { (yyval.yt_tElse) = Else((yyvsp[(3) - (7)].yt_activity), mkinteger(0)); }
    break;

  case 176:
#line 1053 "frontend-parser.yy"
    { (yyval.yt_tElseIf_list) = ConstElseIf_list((yyvsp[(1) - (2)].yt_tElseIf), NiltElseIf_list()); }
    break;

  case 177:
#line 1055 "frontend-parser.yy"
    { (yyval.yt_tElseIf_list) = ConstElseIf_list((yyvsp[(1) - (3)].yt_tElseIf), (yyvsp[(3) - (3)].yt_tElseIf_list)); }
    break;

  case 178:
#line 1060 "frontend-parser.yy"
    { (yyval.yt_tElseIf) = ElseIf((yyvsp[(4) - (7)].yt_activity), (yyvsp[(2) - (7)].yt_integer)); }
    break;

  case 179:
#line 1066 "frontend-parser.yy"
    { (yyval.yt_tElse) = Else(activityEmpty(Empty(NoStandardElements(), mkinteger(0))), mkinteger(0)); }
    break;

  case 180:
#line 1068 "frontend-parser.yy"
    { (yyval.yt_tElse) = Else(activityEmpty(Empty(NoStandardElements(), mkinteger(0))), mkinteger(0)); }
    break;

  case 181:
#line 1070 "frontend-parser.yy"
    { (yyval.yt_tElse) = Else((yyvsp[(4) - (8)].yt_activity), (yyvsp[(2) - (8)].yt_integer)); }
    break;

  case 182:
#line 1080 "frontend-parser.yy"
    { (yyval.yt_tWhile) = While((yyvsp[(4) - (8)].yt_standardElements), (yyvsp[(5) - (8)].yt_activity), (yyvsp[(2) - (8)].yt_integer)); }
    break;

  case 183:
#line 1082 "frontend-parser.yy"
    {
      (yyval.yt_tWhile) = While((yyvsp[(4) - (9)].yt_standardElements), (yyvsp[(6) - (9)].yt_activity), (yyvsp[(2) - (9)].yt_integer));
      // "copy" condition to attributes      
      assert(globals::ASTEmap[(yyvsp[(2) - (9)].yt_integer)->value] != NULL);
      globals::ASTEmap[(yyvsp[(2) - (9)].yt_integer)->value]->attributes["condition"] = (yyvsp[(5) - (9)].yt_casestring)->name;
    }
    break;

  case 184:
#line 1097 "frontend-parser.yy"
    {
      (yyval.yt_tRepeatUntil) = RepeatUntil((yyvsp[(4) - (9)].yt_standardElements), (yyvsp[(5) - (9)].yt_activity), (yyvsp[(2) - (9)].yt_integer));
      // "copy" condition to attributes      
      assert(globals::ASTEmap[(yyvsp[(2) - (9)].yt_integer)->value] != NULL);
      globals::ASTEmap[(yyvsp[(2) - (9)].yt_integer)->value]->attributes["condition"] = (yyvsp[(7) - (9)].yt_casestring)->name;
    }
    break;

  case 185:
#line 1112 "frontend-parser.yy"
    { (yyval.yt_tForEach) = ForEach((yyvsp[(4) - (11)].yt_standardElements), (yyvsp[(5) - (11)].yt_casestring), (yyvsp[(6) - (11)].yt_casestring), (yyvsp[(7) - (11)].yt_casestring), (yyvsp[(8) - (11)].yt_tScope), (yyvsp[(2) - (11)].yt_integer)); }
    break;

  case 186:
#line 1114 "frontend-parser.yy"
    { (yyval.yt_tForEach) = ForEach((yyvsp[(4) - (9)].yt_standardElements), mkcasestring("opaque"), mkcasestring("opaque"), mkcasestring(""), (yyvsp[(6) - (9)].yt_tScope), (yyvsp[(2) - (9)].yt_integer)); }
    break;

  case 187:
#line 1119 "frontend-parser.yy"
    { (yyval.yt_casestring) = (yyvsp[(4) - (8)].yt_casestring); }
    break;

  case 188:
#line 1124 "frontend-parser.yy"
    { (yyval.yt_casestring) = (yyvsp[(4) - (8)].yt_casestring); }
    break;

  case 189:
#line 1129 "frontend-parser.yy"
    { (yyval.yt_casestring) = mkcasestring(""); }
    break;

  case 190:
#line 1131 "frontend-parser.yy"
    { (yyval.yt_casestring) = (yyvsp[(3) - (6)].yt_casestring); }
    break;

  case 191:
#line 1136 "frontend-parser.yy"
    { (yyval.yt_casestring) = (yyvsp[(4) - (8)].yt_casestring); }
    break;

  case 192:
#line 1146 "frontend-parser.yy"
    { (yyval.yt_tPick) = Pick((yyvsp[(4) - (10)].yt_standardElements), ConstOnMessage_list((yyvsp[(5) - (10)].yt_tOnMessage), (yyvsp[(7) - (10)].yt_tOnMessage_list)), (yyvsp[(8) - (10)].yt_tOnAlarm_list), (yyvsp[(2) - (10)].yt_integer)); }
    break;

  case 193:
#line 1156 "frontend-parser.yy"
    { (yyval.yt_tFlow) = Flow((yyvsp[(4) - (8)].yt_standardElements), (yyvsp[(5) - (8)].yt_tLink_list), (yyvsp[(6) - (8)].yt_activity_list), (yyvsp[(2) - (8)].yt_integer)); }
    break;

  case 194:
#line 1161 "frontend-parser.yy"
    { (yyval.yt_tLink_list) = NiltLink_list(); }
    break;

  case 195:
#line 1163 "frontend-parser.yy"
    { (yyval.yt_tLink_list) = (yyvsp[(4) - (7)].yt_tLink_list); }
    break;

  case 196:
#line 1168 "frontend-parser.yy"
    { (yyval.yt_tLink_list) = ConstLink_list((yyvsp[(1) - (2)].yt_tLink), NiltLink_list()); }
    break;

  case 197:
#line 1170 "frontend-parser.yy"
    { (yyval.yt_tLink_list) = ConstLink_list((yyvsp[(1) - (3)].yt_tLink), (yyvsp[(3) - (3)].yt_tLink_list)); }
    break;

  case 198:
#line 1175 "frontend-parser.yy"
    { (yyval.yt_tLink) = Link((yyvsp[(2) - (5)].yt_integer)); }
    break;

  case 199:
#line 1177 "frontend-parser.yy"
    { (yyval.yt_tLink) = Link((yyvsp[(2) - (3)].yt_integer)); }
    break;

  case 200:
#line 1189 "frontend-parser.yy"
    { (yyval.yt_tTerminationHandler) = TerminationHandler((yyvsp[(3) - (7)].yt_activity), mkinteger(0)); }
    break;

  case 201:
#line 1194 "frontend-parser.yy"
    { (yyval.yt_tCompensationHandler) = CompensationHandler((yyvsp[(3) - (7)].yt_activity), mkinteger(0)); }
    break;

  case 202:
#line 1199 "frontend-parser.yy"
    { (yyval.yt_tFaultHandlers) = FaultHandlers((yyvsp[(3) - (7)].yt_tCatch_list), (yyvsp[(4) - (7)].yt_tCatchAll), mkinteger(0)); }
    break;

  case 203:
#line 1204 "frontend-parser.yy"
    { (yyval.yt_tEventHandlers) = EventHandlers((yyvsp[(3) - (7)].yt_tOnMessage_list), (yyvsp[(4) - (7)].yt_tOnAlarm_list), mkinteger(0)); }
    break;

  case 204:
#line 1209 "frontend-parser.yy"
    { (yyval.yt_tPartnerLink_list) = (yyvsp[(4) - (7)].yt_tPartnerLink_list); }
    break;

  case 205:
#line 1214 "frontend-parser.yy"
    { (yyval.yt_tMessageExchange_list) = (yyvsp[(3) - (6)].yt_tMessageExchange_list); }
    break;

  case 206:
#line 1223 "frontend-parser.yy"
    { (yyval.yt_tScope) = Scope((yyvsp[(4) - (10)].yt_standardElements), (yyvsp[(5) - (10)].yt_tVariable_list), NiltMessageExchange_list(), volatile_standardFaultHandlers(mkinteger(0)), volatile_standardCompensationHandler(mkinteger(0)), volatile_standardTerminationHandler(mkinteger(0)), emptyEventHandlers(mkinteger(0)), (yyvsp[(6) - (10)].yt_tCorrelationSet_list), (yyvsp[(7) - (10)].yt_activity), NiltPartnerLink_list(), (yyvsp[(2) - (10)].yt_integer)); }
    break;

  case 207:
#line 1227 "frontend-parser.yy"
    { (yyval.yt_tScope) = Scope((yyvsp[(4) - (11)].yt_standardElements), (yyvsp[(5) - (11)].yt_tVariable_list), NiltMessageExchange_list(), (yyvsp[(7) - (11)].yt_tFaultHandlers), volatile_standardCompensationHandler(mkinteger(0)), volatile_standardTerminationHandler(mkinteger(0)), emptyEventHandlers(mkinteger(0)), (yyvsp[(6) - (11)].yt_tCorrelationSet_list), (yyvsp[(8) - (11)].yt_activity), NiltPartnerLink_list(), (yyvsp[(2) - (11)].yt_integer)); }
    break;

  case 208:
#line 1231 "frontend-parser.yy"
    { (yyval.yt_tScope) = Scope((yyvsp[(4) - (11)].yt_standardElements), (yyvsp[(5) - (11)].yt_tVariable_list), NiltMessageExchange_list(), volatile_standardFaultHandlers(mkinteger(0)), (yyvsp[(7) - (11)].yt_tCompensationHandler), volatile_standardTerminationHandler(mkinteger(0)), emptyEventHandlers(mkinteger(0)), (yyvsp[(6) - (11)].yt_tCorrelationSet_list), (yyvsp[(8) - (11)].yt_activity), NiltPartnerLink_list(), (yyvsp[(2) - (11)].yt_integer)); }
    break;

  case 209:
#line 1235 "frontend-parser.yy"
    { (yyval.yt_tScope) = Scope((yyvsp[(4) - (11)].yt_standardElements), (yyvsp[(5) - (11)].yt_tVariable_list), NiltMessageExchange_list(), volatile_standardFaultHandlers(mkinteger(0)), volatile_standardCompensationHandler(mkinteger(0)), volatile_standardTerminationHandler(mkinteger(0)), (yyvsp[(7) - (11)].yt_tEventHandlers), (yyvsp[(6) - (11)].yt_tCorrelationSet_list), (yyvsp[(8) - (11)].yt_activity), NiltPartnerLink_list(), (yyvsp[(2) - (11)].yt_integer)); }
    break;

  case 210:
#line 1240 "frontend-parser.yy"
    { (yyval.yt_tScope) = Scope((yyvsp[(4) - (12)].yt_standardElements), (yyvsp[(5) - (12)].yt_tVariable_list), NiltMessageExchange_list(), (yyvsp[(7) - (12)].yt_tFaultHandlers), volatile_standardCompensationHandler(mkinteger(0)), volatile_standardTerminationHandler(mkinteger(0)), (yyvsp[(8) - (12)].yt_tEventHandlers), (yyvsp[(6) - (12)].yt_tCorrelationSet_list), (yyvsp[(9) - (12)].yt_activity), NiltPartnerLink_list(), (yyvsp[(2) - (12)].yt_integer)); }
    break;

  case 211:
#line 1245 "frontend-parser.yy"
    { (yyval.yt_tScope) = Scope((yyvsp[(4) - (12)].yt_standardElements), (yyvsp[(5) - (12)].yt_tVariable_list), NiltMessageExchange_list(), volatile_standardFaultHandlers(mkinteger(0)), (yyvsp[(7) - (12)].yt_tCompensationHandler), volatile_standardTerminationHandler(mkinteger(0)), (yyvsp[(8) - (12)].yt_tEventHandlers), (yyvsp[(6) - (12)].yt_tCorrelationSet_list), (yyvsp[(9) - (12)].yt_activity), NiltPartnerLink_list(), (yyvsp[(2) - (12)].yt_integer)); }
    break;

  case 212:
#line 1250 "frontend-parser.yy"
    { (yyval.yt_tScope) = Scope((yyvsp[(4) - (13)].yt_standardElements), (yyvsp[(5) - (13)].yt_tVariable_list), NiltMessageExchange_list(), (yyvsp[(7) - (13)].yt_tFaultHandlers), (yyvsp[(8) - (13)].yt_tCompensationHandler), volatile_standardTerminationHandler(mkinteger(0)), (yyvsp[(9) - (13)].yt_tEventHandlers), (yyvsp[(6) - (13)].yt_tCorrelationSet_list), (yyvsp[(10) - (13)].yt_activity), NiltPartnerLink_list(), (yyvsp[(2) - (13)].yt_integer)); }
    break;

  case 213:
#line 1255 "frontend-parser.yy"
    { (yyval.yt_tScope) = Scope((yyvsp[(4) - (12)].yt_standardElements), (yyvsp[(5) - (12)].yt_tVariable_list), NiltMessageExchange_list(), (yyvsp[(7) - (12)].yt_tFaultHandlers), (yyvsp[(8) - (12)].yt_tCompensationHandler), volatile_standardTerminationHandler(mkinteger(0)), emptyEventHandlers(mkinteger(0)), (yyvsp[(6) - (12)].yt_tCorrelationSet_list), (yyvsp[(9) - (12)].yt_activity), NiltPartnerLink_list(), (yyvsp[(2) - (12)].yt_integer)); }
    break;

  case 214:
#line 1260 "frontend-parser.yy"
    { (yyval.yt_tScope) = Scope((yyvsp[(4) - (16)].yt_standardElements), (yyvsp[(5) - (16)].yt_tVariable_list), (yyvsp[(7) - (16)].yt_tMessageExchange_list), (yyvsp[(10) - (16)].yt_tFaultHandlers), (yyvsp[(11) - (16)].yt_tCompensationHandler), (yyvsp[(12) - (16)].yt_tTerminationHandler), (yyvsp[(9) - (16)].yt_tEventHandlers), (yyvsp[(8) - (16)].yt_tCorrelationSet_list), (yyvsp[(13) - (16)].yt_activity), (yyvsp[(6) - (16)].yt_tPartnerLink_list), (yyvsp[(2) - (16)].yt_integer)); }
    break;

  case 215:
#line 1265 "frontend-parser.yy"
    { (yyval.yt_tScope) = Scope((yyvsp[(4) - (15)].yt_standardElements), (yyvsp[(5) - (15)].yt_tVariable_list), (yyvsp[(6) - (15)].yt_tMessageExchange_list), (yyvsp[(9) - (15)].yt_tFaultHandlers), (yyvsp[(10) - (15)].yt_tCompensationHandler), (yyvsp[(11) - (15)].yt_tTerminationHandler), (yyvsp[(8) - (15)].yt_tEventHandlers), (yyvsp[(7) - (15)].yt_tCorrelationSet_list), (yyvsp[(12) - (15)].yt_activity), NiltPartnerLink_list(), (yyvsp[(2) - (15)].yt_integer)); }
    break;

  case 216:
#line 1270 "frontend-parser.yy"
    { (yyval.yt_tScope) = Scope((yyvsp[(4) - (14)].yt_standardElements), (yyvsp[(5) - (14)].yt_tVariable_list), NiltMessageExchange_list(), (yyvsp[(8) - (14)].yt_tFaultHandlers), (yyvsp[(9) - (14)].yt_tCompensationHandler), (yyvsp[(10) - (14)].yt_tTerminationHandler), (yyvsp[(7) - (14)].yt_tEventHandlers), (yyvsp[(6) - (14)].yt_tCorrelationSet_list), (yyvsp[(11) - (14)].yt_activity), NiltPartnerLink_list(), (yyvsp[(2) - (14)].yt_integer)); }
    break;

  case 217:
#line 1274 "frontend-parser.yy"
    { (yyval.yt_tScope) = Scope((yyvsp[(4) - (13)].yt_standardElements), (yyvsp[(5) - (13)].yt_tVariable_list), NiltMessageExchange_list(), volatile_standardFaultHandlers(mkinteger(0)), (yyvsp[(8) - (13)].yt_tCompensationHandler), (yyvsp[(9) - (13)].yt_tTerminationHandler), (yyvsp[(7) - (13)].yt_tEventHandlers), (yyvsp[(6) - (13)].yt_tCorrelationSet_list), (yyvsp[(10) - (13)].yt_activity), NiltPartnerLink_list(), (yyvsp[(2) - (13)].yt_integer)); }
    break;

  case 218:
#line 1279 "frontend-parser.yy"
    { (yyval.yt_tScope) = Scope((yyvsp[(4) - (13)].yt_standardElements), (yyvsp[(5) - (13)].yt_tVariable_list), NiltMessageExchange_list(), (yyvsp[(7) - (13)].yt_tFaultHandlers), (yyvsp[(8) - (13)].yt_tCompensationHandler), (yyvsp[(9) - (13)].yt_tTerminationHandler), emptyEventHandlers(mkinteger(0)), (yyvsp[(6) - (13)].yt_tCorrelationSet_list), (yyvsp[(10) - (13)].yt_activity), NiltPartnerLink_list(), (yyvsp[(2) - (13)].yt_integer)); }
    break;

  case 219:
#line 1283 "frontend-parser.yy"
    { (yyval.yt_tScope) = Scope((yyvsp[(4) - (12)].yt_standardElements), (yyvsp[(5) - (12)].yt_tVariable_list), NiltMessageExchange_list(), volatile_standardFaultHandlers(mkinteger(0)), volatile_standardCompensationHandler(mkinteger(0)), (yyvsp[(8) - (12)].yt_tTerminationHandler), (yyvsp[(7) - (12)].yt_tEventHandlers), (yyvsp[(6) - (12)].yt_tCorrelationSet_list), (yyvsp[(9) - (12)].yt_activity), NiltPartnerLink_list(), (yyvsp[(2) - (12)].yt_integer)); }
    break;

  case 220:
#line 1287 "frontend-parser.yy"
    { (yyval.yt_tScope) = Scope((yyvsp[(4) - (12)].yt_standardElements), (yyvsp[(5) - (12)].yt_tVariable_list), NiltMessageExchange_list(), (yyvsp[(7) - (12)].yt_tFaultHandlers), volatile_standardCompensationHandler(mkinteger(0)), (yyvsp[(8) - (12)].yt_tTerminationHandler), emptyEventHandlers(mkinteger(0)), (yyvsp[(6) - (12)].yt_tCorrelationSet_list), (yyvsp[(9) - (12)].yt_activity), NiltPartnerLink_list(), (yyvsp[(2) - (12)].yt_integer)); }
    break;

  case 221:
#line 1291 "frontend-parser.yy"
    { (yyval.yt_tScope) = Scope((yyvsp[(4) - (12)].yt_standardElements), (yyvsp[(5) - (12)].yt_tVariable_list), NiltMessageExchange_list(), volatile_standardFaultHandlers(mkinteger(0)), (yyvsp[(7) - (12)].yt_tCompensationHandler), (yyvsp[(8) - (12)].yt_tTerminationHandler), emptyEventHandlers(mkinteger(0)), (yyvsp[(6) - (12)].yt_tCorrelationSet_list), (yyvsp[(9) - (12)].yt_activity), NiltPartnerLink_list(), (yyvsp[(2) - (12)].yt_integer)); }
    break;

  case 222:
#line 1313 "frontend-parser.yy"
    { (yyval.yt_integer) = mkinteger(globals::ASTEid++); // generate a new id
     }
    break;

  case 223:
#line 1316 "frontend-parser.yy"
    { if (globals::temporaryAttributeMap[globals::ASTEid]["referenceLine"] == "")
         globals::temporaryAttributeMap[globals::ASTEid]["referenceLine"] = toString(frontend_lineno); // remember the file position       
     }
    break;

  case 224:
#line 1320 "frontend-parser.yy"
    { globals::temporaryAttributeMap[(yyvsp[(5) - (5)].yt_integer)->value][strip_namespace((yyvsp[(1) - (5)].yt_casestring)->name)] = strip_namespace((yyvsp[(4) - (5)].yt_casestring)->name);
       (yyval.yt_integer) = (yyvsp[(5) - (5)].yt_integer); }
    break;

  case 225:
#line 1323 "frontend-parser.yy"
    { (yyval.yt_integer) = (yyvsp[(2) - (2)].yt_integer); }
    break;

  case 226:
#line 1336 "frontend-parser.yy"
    { (yyval.yt_standardElements) = StandardElements((yyvsp[(1) - (2)].yt_tTarget_list), (yyvsp[(2) - (2)].yt_tSource_list), currentJoinCondition);
      currentJoinCondition = standardJoinCondition(); }
    break;

  case 227:
#line 1339 "frontend-parser.yy"
    { (yyval.yt_standardElements) = StandardElements(ConstTarget_list((yyvsp[(4) - (9)].yt_tTarget), (yyvsp[(6) - (9)].yt_tTarget_list)), NiltSource_list(), currentJoinCondition);
      currentJoinCondition = standardJoinCondition(); }
    break;

  case 228:
#line 1342 "frontend-parser.yy"
    { (yyval.yt_standardElements) = StandardElements(NiltTarget_list(), ConstSource_list((yyvsp[(4) - (9)].yt_tSource), (yyvsp[(6) - (9)].yt_tSource_list)), currentJoinCondition);
      currentJoinCondition = standardJoinCondition(); }
    break;

  case 229:
#line 1345 "frontend-parser.yy"
    { (yyval.yt_standardElements) = StandardElements(ConstTarget_list((yyvsp[(4) - (17)].yt_tTarget), (yyvsp[(6) - (17)].yt_tTarget_list)), ConstSource_list((yyvsp[(12) - (17)].yt_tSource), (yyvsp[(14) - (17)].yt_tSource_list)), currentJoinCondition);
      currentJoinCondition = standardJoinCondition(); }
    break;

  case 230:
#line 1351 "frontend-parser.yy"
    { (yyval.yt_tTarget_list) = NiltTarget_list(); }
    break;

  case 231:
#line 1353 "frontend-parser.yy"
    { (yyval.yt_tTarget_list) = ConstTarget_list((yyvsp[(1) - (3)].yt_tTarget), (yyvsp[(3) - (3)].yt_tTarget_list)); }
    break;

  case 232:
#line 1358 "frontend-parser.yy"
    { (yyval.yt_tTarget) = Target((yyvsp[(2) - (5)].yt_integer)); }
    break;

  case 233:
#line 1360 "frontend-parser.yy"
    { (yyval.yt_tTarget) = Target((yyvsp[(2) - (3)].yt_integer)); }
    break;

  case 234:
#line 1365 "frontend-parser.yy"
    { (yyval.yt_tSource_list) = NiltSource_list(); }
    break;

  case 235:
#line 1367 "frontend-parser.yy"
    { (yyval.yt_tSource_list) = ConstSource_list((yyvsp[(1) - (3)].yt_tSource), (yyvsp[(3) - (3)].yt_tSource_list)); }
    break;

  case 236:
#line 1372 "frontend-parser.yy"
    { (yyval.yt_tSource) = Source((yyvsp[(2) - (6)].yt_integer)); }
    break;

  case 237:
#line 1374 "frontend-parser.yy"
    { (yyval.yt_tSource) = Source((yyvsp[(2) - (3)].yt_integer)); }
    break;

  case 241:
#line 1385 "frontend-parser.yy"
    { (yyval.yt_joinCondition) = standardJoinCondition(); }
    break;

  case 242:
#line 1387 "frontend-parser.yy"
    { (yyval.yt_joinCondition) = currentJoinCondition = userDefinedJoinCondition((yyvsp[(3) - (7)].yt_expression)); }
    break;

  case 243:
#line 1392 "frontend-parser.yy"
    { currentJoinCondition = userDefinedJoinCondition((yyvsp[(3) - (3)].yt_expression));
      currentJoinCondition->print(); }
    break;

  case 244:
#line 1395 "frontend-parser.yy"
    { cerr << "ignoring given join condition: \"" << (yyvsp[(3) - (3)].yt_casestring)->name << "\"" << endl; }
    break;

  case 245:
#line 1400 "frontend-parser.yy"
    { (yyval.yt_expression) = Term((yyvsp[(4) - (6)].yt_casestring)); }
    break;

  case 246:
#line 1402 "frontend-parser.yy"
    { (yyval.yt_expression) = Term((yyvsp[(1) - (1)].yt_casestring)); }
    break;

  case 247:
#line 1404 "frontend-parser.yy"
    { (yyval.yt_expression) = Conjunction((yyvsp[(2) - (5)].yt_expression), (yyvsp[(4) - (5)].yt_expression)); }
    break;

  case 248:
#line 1406 "frontend-parser.yy"
    { (yyval.yt_expression) = Disjunction((yyvsp[(2) - (5)].yt_expression), (yyvsp[(4) - (5)].yt_expression)); }
    break;

  case 249:
#line 1408 "frontend-parser.yy"
    { (yyval.yt_expression) = Conjunction((yyvsp[(1) - (3)].yt_expression), (yyvsp[(3) - (3)].yt_expression)); }
    break;

  case 250:
#line 1410 "frontend-parser.yy"
    { (yyval.yt_expression) = Disjunction((yyvsp[(1) - (3)].yt_expression), (yyvsp[(3) - (3)].yt_expression)); }
    break;

  case 258:
#line 1428 "frontend-parser.yy"
    { (yyval.yt_casestring) = (yyvsp[(1) - (1)].yt_casestring); }
    break;

  case 259:
#line 1430 "frontend-parser.yy"
    { (yyval.yt_casestring) = mkcasestring((string((yyvsp[(1) - (2)].yt_casestring)->name) + string((yyvsp[(2) - (2)].yt_casestring)->name)).c_str()); }
    break;

  case 260:
#line 1432 "frontend-parser.yy"
    { (yyval.yt_casestring) = (yyvsp[(2) - (3)].yt_casestring); }
    break;

  case 261:
#line 1434 "frontend-parser.yy"
    { (yyval.yt_casestring) = (yyvsp[(1) - (1)].yt_casestring); }
    break;


/* Line 1267 of yacc.c.  */
#line 3840 "frontend-parser.cc"
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



