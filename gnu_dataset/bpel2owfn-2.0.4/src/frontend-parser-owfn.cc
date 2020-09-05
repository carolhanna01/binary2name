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
#define yyparse frontend_owfn_parse
#define yylex   frontend_owfn_lex
#define yyerror frontend_owfn_error
#define yylval  frontend_owfn_lval
#define yychar  frontend_owfn_char
#define yydebug frontend_owfn_debug
#define yynerrs frontend_owfn_nerrs


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     KEY_SAFE = 258,
     KEY_PLACE = 259,
     KEY_INTERNAL = 260,
     KEY_INPUT = 261,
     KEY_OUTPUT = 262,
     KEY_MARKING = 263,
     KEY_FINALMARKING = 264,
     KEY_FINALCONDITION = 265,
     KEY_TRANSITION = 266,
     KEY_CONSUME = 267,
     KEY_PRODUCE = 268,
     KEY_ALL_OTHER_PLACES_EMPTY = 269,
     KEY_ALL_OTHER_INTERNAL_PLACES_EMPTY = 270,
     KEY_ALL_OTHER_EXTERNAL_PLACES_EMPTY = 271,
     KEY_MAX_UNIQUE_EVENTS = 272,
     KEY_ON_LOOP = 273,
     KEY_MAX_OCCURRENCES = 274,
     KEY_TRUE = 275,
     KEY_FALSE = 276,
     LCONTROL = 277,
     RCONTROL = 278,
     COMMA = 279,
     COLON = 280,
     SEMICOLON = 281,
     IDENT = 282,
     NUMBER = 283,
     NEGATIVE_NUMBER = 284,
     LPAR = 285,
     RPAR = 286,
     OP_OR = 287,
     OP_AND = 288,
     OP_NOT = 289,
     OP_LE = 290,
     OP_GE = 291,
     OP_LT = 292,
     OP_GT = 293,
     OP_NE = 294,
     OP_EQ = 295
   };
#endif
/* Tokens.  */
#define KEY_SAFE 258
#define KEY_PLACE 259
#define KEY_INTERNAL 260
#define KEY_INPUT 261
#define KEY_OUTPUT 262
#define KEY_MARKING 263
#define KEY_FINALMARKING 264
#define KEY_FINALCONDITION 265
#define KEY_TRANSITION 266
#define KEY_CONSUME 267
#define KEY_PRODUCE 268
#define KEY_ALL_OTHER_PLACES_EMPTY 269
#define KEY_ALL_OTHER_INTERNAL_PLACES_EMPTY 270
#define KEY_ALL_OTHER_EXTERNAL_PLACES_EMPTY 271
#define KEY_MAX_UNIQUE_EVENTS 272
#define KEY_ON_LOOP 273
#define KEY_MAX_OCCURRENCES 274
#define KEY_TRUE 275
#define KEY_FALSE 276
#define LCONTROL 277
#define RCONTROL 278
#define COMMA 279
#define COLON 280
#define SEMICOLON 281
#define IDENT 282
#define NUMBER 283
#define NEGATIVE_NUMBER 284
#define LPAR 285
#define RPAR 286
#define OP_OR 287
#define OP_AND 288
#define OP_NOT 289
#define OP_LE 290
#define OP_GE 291
#define OP_LT 292
#define OP_GT 293
#define OP_NE 294
#define OP_EQ 295




/* Copy the first part of user declarations.  */
#line 36 "frontend-parser-owfn.yy"

/* Prologue: Syntax and usage of the prologue.
 * Bison Declarations: Syntax and usage of the Bison declarations section.
 * Grammar Rules: Syntax and usage of the grammar rules section.
 * Epilogue: Syntax and usage of the epilogue.  */

// options for Bison
#define YYDEBUG 1
#define YYERROR_VERBOSE 1  // for verbose error messages


// to avoid the message "parser stack overflow"
#define YYMAXDEPTH 1000000
#define YYINITDEPTH 10000


// from flex
extern char* frontend_owfn_text;
extern int frontend_owfn_lex();

// defined in "debug.h"
extern int frontend_owfn_error(const char *);




#include<stdio.h>
#include "globals.h"
#include "petrinet.h"
#include "helpers.h"
#include<limits.h>
#include<set>
#include <string>
using namespace std;
using namespace PNapi;

extern PetriNet PN;

set<string> in;
set<string> out;
string nodename;
int readmode=0;
PNapi::Transition *t = NULL; 





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
typedef union YYSTYPE
#line 93 "frontend-parser-owfn.yy"
{
    kc::casestring yt_casestring;
}
/* Line 193 of yacc.c.  */
#line 236 "frontend-parser-owfn.cc"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 249 "frontend-parser-owfn.cc"

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
#define YYLAST   144

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  41
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  42
/* YYNRULES -- Number of rules.  */
#define YYNRULES  85
/* YYNRULES -- Number of states.  */
#define YYNSTATES  141

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   295

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
      35,    36,    37,    38,    39,    40
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint8 yyprhs[] =
{
       0,     0,     3,     5,     6,     7,    17,    21,    25,    29,
      31,    32,    37,    38,    39,    44,    45,    46,    51,    52,
      53,    57,    60,    65,    66,    69,    73,    77,    79,    80,
      83,    85,    87,    88,    92,    93,    98,   103,   108,   113,
     118,   119,   121,   125,   129,   131,   132,   134,   138,   142,
     144,   147,   148,   149,   150,   151,   164,   166,   168,   169,
     173,   175,   176,   181,   182,   184,   188,   192,   194,   195,
     196,   201,   202,   207,   211,   215,   219,   220,   225,   226,
     230,   234,   238,   242,   246,   250
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      42,     0,    -1,    43,    -1,    -1,    -1,    44,     4,    47,
       8,    45,    63,    26,    46,    67,    -1,     9,    65,    26,
      -1,    10,    78,    26,    -1,    52,    48,    50,    -1,    54,
      -1,    -1,    49,     6,    56,    26,    -1,    -1,    -1,    51,
       7,    56,    26,    -1,    -1,    -1,    53,     5,    56,    26,
      -1,    -1,    -1,    55,    56,    26,    -1,    57,    58,    -1,
      56,    26,    57,    58,    -1,    -1,     3,    25,    -1,     3,
      28,    25,    -1,    58,    24,    59,    -1,    59,    -1,    -1,
      60,    61,    -1,    27,    -1,    28,    -1,    -1,    22,    62,
      23,    -1,    -1,    17,    40,    28,    62,    -1,    18,    40,
      20,    62,    -1,    18,    40,    21,    62,    -1,    19,    40,
      28,    62,    -1,    19,    40,    29,    62,    -1,    -1,    64,
      -1,    63,    24,    64,    -1,    60,    25,    28,    -1,    60,
      -1,    -1,    66,    -1,    65,    24,    66,    -1,    60,    25,
      28,    -1,    60,    -1,    67,    68,    -1,    -1,    -1,    -1,
      -1,    11,    72,    69,    73,    12,    70,    76,    26,    13,
      71,    76,    26,    -1,    27,    -1,    28,    -1,    -1,    22,
      74,    23,    -1,    27,    -1,    -1,    27,    75,    24,    74,
      -1,    -1,    77,    -1,    77,    24,    76,    -1,    60,    25,
      28,    -1,    60,    -1,    -1,    -1,    30,    79,    78,    31,
      -1,    -1,    78,    33,    80,    78,    -1,    78,    33,    14,
      -1,    78,    33,    15,    -1,    78,    33,    16,    -1,    -1,
      78,    32,    81,    78,    -1,    -1,    34,    82,    78,    -1,
      60,    40,    28,    -1,    60,    39,    28,    -1,    60,    37,
      28,    -1,    60,    38,    28,    -1,    60,    36,    28,    -1,
      60,    35,    28,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   130,   130,   135,   138,   135,   147,   148,   151,   152,
     156,   156,   158,   162,   162,   164,   168,   168,   170,   174,
     174,   178,   179,   182,   183,   184,   188,   189,   190,   194,
     206,   207,   210,   212,   215,   217,   221,   225,   229,   233,
     239,   241,   242,   246,   250,   256,   257,   258,   262,   266,
     273,   274,   278,   283,   287,   277,   296,   297,   300,   302,
     306,   307,   307,   310,   311,   312,   316,   323,   332,   334,
     333,   344,   343,   348,   352,   356,   361,   360,   366,   365,
     370,   374,   378,   382,   386,   390
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "KEY_SAFE", "KEY_PLACE", "KEY_INTERNAL",
  "KEY_INPUT", "KEY_OUTPUT", "KEY_MARKING", "KEY_FINALMARKING",
  "KEY_FINALCONDITION", "KEY_TRANSITION", "KEY_CONSUME", "KEY_PRODUCE",
  "KEY_ALL_OTHER_PLACES_EMPTY", "KEY_ALL_OTHER_INTERNAL_PLACES_EMPTY",
  "KEY_ALL_OTHER_EXTERNAL_PLACES_EMPTY", "KEY_MAX_UNIQUE_EVENTS",
  "KEY_ON_LOOP", "KEY_MAX_OCCURRENCES", "KEY_TRUE", "KEY_FALSE",
  "LCONTROL", "RCONTROL", "COMMA", "COLON", "SEMICOLON", "IDENT", "NUMBER",
  "NEGATIVE_NUMBER", "LPAR", "RPAR", "OP_OR", "OP_AND", "OP_NOT", "OP_LE",
  "OP_GE", "OP_LT", "OP_GT", "OP_NE", "OP_EQ", "$accept", "input", "net",
  "@1", "@2", "final", "place_area", "place_area_input", "@3",
  "place_area_output", "@4", "place_area_internal", "@5",
  "place_area_lola", "@6", "placelists", "capacity", "placelist", "place",
  "nodeident", "controlcommands", "commands", "markinglist", "marking",
  "finalmarkinglist", "finalmarking", "transitionlist", "transition", "@7",
  "@8", "@9", "tname", "annotation", "annotation_list", "@10", "arclist",
  "arc", "statepredicate", "@11", "@12", "@13", "@14", 0
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
     295
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    41,    42,    44,    45,    43,    46,    46,    47,    47,
      49,    48,    48,    51,    50,    50,    53,    52,    52,    55,
      54,    56,    56,    57,    57,    57,    58,    58,    58,    59,
      60,    60,    61,    61,    62,    62,    62,    62,    62,    62,
      63,    63,    63,    64,    64,    65,    65,    65,    66,    66,
      67,    67,    69,    70,    71,    68,    72,    72,    73,    73,
      74,    75,    74,    76,    76,    76,    77,    77,    78,    79,
      78,    80,    78,    78,    78,    78,    81,    78,    82,    78,
      78,    78,    78,    78,    78,    78
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     0,     0,     9,     3,     3,     3,     1,
       0,     4,     0,     0,     4,     0,     0,     4,     0,     0,
       3,     2,     4,     0,     2,     3,     3,     1,     0,     2,
       1,     1,     0,     3,     0,     4,     4,     4,     4,     4,
       0,     1,     3,     3,     1,     0,     1,     3,     3,     1,
       2,     0,     0,     0,     0,    12,     1,     1,     0,     3,
       1,     0,     4,     0,     1,     3,     3,     1,     0,     0,
       4,     0,     4,     3,     3,     3,     0,     4,     0,     3,
       3,     3,     3,     3,     3,     3
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       3,     0,     2,     0,     1,    19,     0,    12,     0,     9,
      23,     4,    13,     0,    23,     0,     0,    28,    40,     8,
       0,    23,     0,    24,     0,    23,    30,    31,    21,    27,
      32,    44,     0,    41,    23,     0,    23,    25,    28,     0,
      34,    29,     0,     0,     0,     0,    23,    22,    26,     0,
       0,     0,     0,    43,    42,    45,    68,    51,    23,     0,
       0,     0,    33,    49,     0,    46,    69,    78,     0,     0,
       5,    34,    34,    34,    34,    34,     0,     0,     6,    68,
      68,     0,     0,     0,     0,     0,     0,     7,    76,    71,
       0,    50,    35,    36,    37,    38,    39,    48,    47,     0,
      79,    85,    84,    82,    83,    81,    80,    68,    73,    74,
      75,    68,    56,    57,    52,    70,    77,    72,    58,     0,
       0,    60,     0,    53,     0,    59,    63,     0,    67,     0,
      64,    62,     0,     0,    63,    66,    54,    65,    63,     0,
      55
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,     3,    18,    57,     6,    12,    13,    19,
      20,     7,     8,     9,    10,    16,    17,    28,    29,    68,
      41,    52,    32,    33,    64,    65,    70,    91,   118,   126,
     138,   114,   120,   122,   124,   129,   130,    69,    79,   111,
     107,    80
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -102
static const yytype_int8 yypact[] =
{
    -102,    12,  -102,    27,  -102,    56,    26,    46,    70,  -102,
      78,  -102,    76,    79,    78,   -22,    60,    20,    20,  -102,
      80,    78,    62,  -102,    64,    16,  -102,  -102,    66,  -102,
      69,    67,    -6,  -102,    78,    68,    22,  -102,    20,    20,
      48,  -102,    65,    20,    49,    71,    42,    66,  -102,    58,
      59,    61,    72,  -102,  -102,    20,   -19,  -102,    43,    74,
      57,    51,  -102,    75,    50,  -102,  -102,  -102,     4,   -16,
      85,    48,    48,    48,    48,    48,    77,    20,  -102,   -19,
     -19,    82,    83,    84,    86,    87,    88,  -102,  -102,    54,
      55,  -102,  -102,  -102,  -102,  -102,  -102,  -102,  -102,    40,
    -102,  -102,  -102,  -102,  -102,  -102,  -102,   -19,  -102,  -102,
    -102,   -19,  -102,  -102,  -102,  -102,    73,  -102,    81,    91,
      92,    89,    96,  -102,    98,  -102,    20,    91,    95,    97,
     100,  -102,    99,    94,    20,  -102,  -102,  -102,    20,   102,
    -102
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
    -102,  -102,  -102,  -102,  -102,  -102,  -102,  -102,  -102,  -102,
    -102,  -102,  -102,  -102,  -102,    -7,   -23,    93,    90,   -17,
    -102,   -18,  -102,   101,  -102,    31,  -102,  -102,  -102,  -102,
    -102,  -102,  -102,    -2,  -102,  -101,  -102,   -75,  -102,  -102,
    -102,  -102
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -62
static const yytype_int16 yytable[] =
{
      30,    31,    38,    23,    99,   100,    24,    22,    26,    27,
      87,    66,     4,    38,    35,    67,    88,    89,    43,    15,
      44,    30,    30,    38,   -20,    15,    31,    45,   -17,   -17,
     -17,     5,   116,   137,    11,    38,   117,   139,    63,    81,
      82,    83,    84,    85,    86,    15,    15,    26,    27,   -11,
     -11,   -14,   -10,    92,    93,    94,    95,    96,    55,    56,
      63,   -16,   -18,   -18,   -18,    49,    50,    51,   108,   109,
     110,   115,    88,    89,    77,    14,    78,    72,    73,    74,
      75,    15,   112,   113,   -15,    21,    25,    34,    36,    37,
      39,    40,    42,    53,    46,    62,    90,    58,    59,    60,
      76,    61,    71,   119,   123,    97,    89,   136,    98,   128,
     101,   102,   103,   -61,   104,   105,   106,   128,   121,   125,
     132,   128,   127,   133,   134,   131,     0,   135,   140,    48,
       0,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    54
};

static const yytype_int16 yycheck[] =
{
      17,    18,    25,    25,    79,    80,    28,    14,    27,    28,
      26,    30,     0,    36,    21,    34,    32,    33,    24,     3,
      26,    38,    39,    46,     8,     3,    43,    34,     6,     7,
       8,     4,   107,   134,     8,    58,   111,   138,    55,    35,
      36,    37,    38,    39,    40,     3,     3,    27,    28,     7,
       8,     8,     6,    71,    72,    73,    74,    75,     9,    10,
      77,     5,     6,     7,     8,    17,    18,    19,    14,    15,
      16,    31,    32,    33,    24,     5,    26,    20,    21,    28,
      29,     3,    27,    28,     8,     6,    26,     7,    26,    25,
      24,    22,    25,    28,    26,    23,    11,    26,    40,    40,
      25,    40,    28,    22,    12,    28,    33,    13,    77,   126,
      28,    28,    28,    24,    28,    28,    28,   134,    27,    23,
      25,   138,    24,    26,    24,   127,    -1,    28,    26,    39,
      -1,    38,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    43
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    42,    43,    44,     0,     4,    47,    52,    53,    54,
      55,     8,    48,    49,     5,     3,    56,    57,    45,    50,
      51,     6,    56,    25,    28,    26,    27,    28,    58,    59,
      60,    60,    63,    64,     7,    56,    26,    25,    57,    24,
      22,    61,    25,    24,    26,    56,    26,    58,    59,    17,
      18,    19,    62,    28,    64,     9,    10,    46,    26,    40,
      40,    40,    23,    60,    65,    66,    30,    34,    60,    78,
      67,    28,    20,    21,    28,    29,    25,    24,    26,    79,
      82,    35,    36,    37,    38,    39,    40,    26,    32,    33,
      11,    68,    62,    62,    62,    62,    62,    28,    66,    78,
      78,    28,    28,    28,    28,    28,    28,    81,    14,    15,
      16,    80,    27,    28,    72,    31,    78,    78,    69,    22,
      73,    27,    74,    12,    75,    23,    70,    24,    60,    76,
      77,    74,    25,    26,    24,    28,    13,    76,    71,    76,
      26
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
#line 130 "frontend-parser-owfn.yy"
    { 
}
    break;

  case 3:
#line 135 "frontend-parser-owfn.yy"
    {
		}
    break;

  case 4:
#line 138 "frontend-parser-owfn.yy"
    {
		}
    break;

  case 5:
#line 142 "frontend-parser-owfn.yy"
    {
			// fill in arcs
		}
    break;

  case 10:
#line 156 "frontend-parser-owfn.yy"
    {readmode = 0;}
    break;

  case 13:
#line 162 "frontend-parser-owfn.yy"
    {readmode = 1;}
    break;

  case 16:
#line 168 "frontend-parser-owfn.yy"
    {readmode = 2;}
    break;

  case 19:
#line 174 "frontend-parser-owfn.yy"
    {readmode = 2;}
    break;

  case 29:
#line 195 "frontend-parser-owfn.yy"
    {
    switch (readmode)
    {
      case 0:    PN.newPlace(nodename, IN); break;
      case 1:    PN.newPlace(nodename, OUT); break;
      case 2:    PN.newPlace(nodename); break;
      case 3:    break;
    }
  }
    break;

  case 30:
#line 206 "frontend-parser-owfn.yy"
    {nodename = strip_namespace((yyvsp[(1) - (1)].yt_casestring)->name);}
    break;

  case 31:
#line 207 "frontend-parser-owfn.yy"
    {nodename = strip_namespace((yyvsp[(1) - (1)].yt_casestring)->name);}
    break;

  case 35:
#line 218 "frontend-parser-owfn.yy"
    {
      globals::owfn_commands[nodename] = globals::owfn_commands[nodename] + "MAX_UNIQUE_EVENTS = " + strip_namespace((yyvsp[(3) - (4)].yt_casestring)->name) + " ";
    }
    break;

  case 36:
#line 222 "frontend-parser-owfn.yy"
    {
      globals::owfn_commands[nodename] = globals::owfn_commands[nodename] + "ON_LOOPS = TRUE ";
    }
    break;

  case 37:
#line 226 "frontend-parser-owfn.yy"
    {
      globals::owfn_commands[nodename] = globals::owfn_commands[nodename] + "ON_LOOPS = FALSE ";
    }
    break;

  case 38:
#line 230 "frontend-parser-owfn.yy"
    {
      globals::owfn_commands[nodename] = globals::owfn_commands[nodename] + "MAX_OCCURENCES = " + strip_namespace((yyvsp[(3) - (4)].yt_casestring)->name) + " ";
    }
    break;

  case 39:
#line 234 "frontend-parser-owfn.yy"
    {
      globals::owfn_commands[nodename] = globals::owfn_commands[nodename] + "MAX_OCCURENCES = " + strip_namespace((yyvsp[(3) - (4)].yt_casestring)->name) + " ";
    }
    break;

  case 43:
#line 247 "frontend-parser-owfn.yy"
    {
       (PN.findPlace(nodename))->mark(); // BAM
      }
    break;

  case 44:
#line 251 "frontend-parser-owfn.yy"
    {
       (PN.findPlace(nodename))->mark();
      }
    break;

  case 48:
#line 263 "frontend-parser-owfn.yy"
    {
       (PN.findPlace(nodename))->isFinal = true; // BAM
      }
    break;

  case 49:
#line 267 "frontend-parser-owfn.yy"
    {
       (PN.findPlace(nodename))->isFinal = true;
      }
    break;

  case 52:
#line 278 "frontend-parser-owfn.yy"
    {
	  t = PN.newTransition(nodename);
	}
    break;

  case 53:
#line 283 "frontend-parser-owfn.yy"
    {
	  readmode = 4;
	}
    break;

  case 54:
#line 287 "frontend-parser-owfn.yy"
    {
	  readmode = 5;
	}
    break;

  case 55:
#line 291 "frontend-parser-owfn.yy"
    {
	}
    break;

  case 56:
#line 296 "frontend-parser-owfn.yy"
    {nodename = strip_namespace((yyvsp[(1) - (1)].yt_casestring)->name);}
    break;

  case 57:
#line 297 "frontend-parser-owfn.yy"
    {nodename = strip_namespace((yyvsp[(1) - (1)].yt_casestring)->name);}
    break;

  case 60:
#line 306 "frontend-parser-owfn.yy"
    { t->add_label(string((yyvsp[(1) - (1)].yt_casestring)->name)); }
    break;

  case 61:
#line 307 "frontend-parser-owfn.yy"
    { t->add_label(string((yyvsp[(1) - (1)].yt_casestring)->name)); }
    break;

  case 66:
#line 317 "frontend-parser-owfn.yy"
    { 
      if (readmode == 4)
        PN.newArc(PN.findPlace(nodename), t, STANDARD, toInt(strip_namespace((yyvsp[(3) - (3)].yt_casestring)->name)) );
      if (readmode == 5) 
        PN.newArc(t, PN.findPlace(nodename), STANDARD, toInt(strip_namespace((yyvsp[(3) - (3)].yt_casestring)->name)) );
    }
    break;

  case 67:
#line 324 "frontend-parser-owfn.yy"
    { 
      if (readmode == 4)
        PN.newArc(PN.findPlace(nodename), t, STANDARD, 1);
      if (readmode == 5) 
        PN.newArc(t, PN.findPlace(nodename), STANDARD, 1);
    }
    break;

  case 69:
#line 334 "frontend-parser-owfn.yy"
    {
    globals::owfn_statepredicate += "(";  
  }
    break;

  case 70:
#line 339 "frontend-parser-owfn.yy"
    {
    globals::owfn_statepredicate += ")";  
  }
    break;

  case 71:
#line 344 "frontend-parser-owfn.yy"
    {
    globals::owfn_statepredicate = globals::owfn_statepredicate + " AND ";  
  }
    break;

  case 73:
#line 349 "frontend-parser-owfn.yy"
    {
    globals::owfn_statepredicate = globals::owfn_statepredicate + " AND ALL_OTHER_PLACES_EMPTY";  
  }
    break;

  case 74:
#line 353 "frontend-parser-owfn.yy"
    {
    globals::owfn_statepredicate = globals::owfn_statepredicate + " AND ALL_OTHER_INTERNAL_PLACES_EMPTY";  
  }
    break;

  case 75:
#line 357 "frontend-parser-owfn.yy"
    {
    globals::owfn_statepredicate = globals::owfn_statepredicate + " AND ALL_OTHER_EXTERNAL_PLACES_EMPTY";  
  }
    break;

  case 76:
#line 361 "frontend-parser-owfn.yy"
    {
    globals::owfn_statepredicate = globals::owfn_statepredicate + " OR ";  
  }
    break;

  case 78:
#line 366 "frontend-parser-owfn.yy"
    {
    globals::owfn_statepredicate = globals::owfn_statepredicate + "NOT ";  
  }
    break;

  case 80:
#line 371 "frontend-parser-owfn.yy"
    {
    globals::owfn_statepredicate = globals::owfn_statepredicate + nodename + " = " + strip_namespace((yyvsp[(3) - (3)].yt_casestring)->name);  
  }
    break;

  case 81:
#line 375 "frontend-parser-owfn.yy"
    {
    globals::owfn_statepredicate = globals::owfn_statepredicate + nodename + " != " + strip_namespace((yyvsp[(3) - (3)].yt_casestring)->name);  
  }
    break;

  case 82:
#line 379 "frontend-parser-owfn.yy"
    {
    globals::owfn_statepredicate = globals::owfn_statepredicate + nodename + " < " + strip_namespace((yyvsp[(3) - (3)].yt_casestring)->name);  
  }
    break;

  case 83:
#line 383 "frontend-parser-owfn.yy"
    {
    globals::owfn_statepredicate = globals::owfn_statepredicate + nodename + " > " + strip_namespace((yyvsp[(3) - (3)].yt_casestring)->name);  
  }
    break;

  case 84:
#line 387 "frontend-parser-owfn.yy"
    {
    globals::owfn_statepredicate = globals::owfn_statepredicate + nodename + " >= " + strip_namespace((yyvsp[(3) - (3)].yt_casestring)->name);  
  }
    break;

  case 85:
#line 391 "frontend-parser-owfn.yy"
    {
    globals::owfn_statepredicate = globals::owfn_statepredicate + nodename + " <= " + strip_namespace((yyvsp[(3) - (3)].yt_casestring)->name);  
  }
    break;


/* Line 1267 of yacc.c.  */
#line 1881 "frontend-parser-owfn.cc"
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



