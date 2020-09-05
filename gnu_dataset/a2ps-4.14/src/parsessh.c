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




/* Copy the first part of user declarations.  */
#line 1 "parsessh.y"
 /* -*- c -*- */
/*
 * Grammar for parsing the style sheets
 *
 * Copyright (c) 1988-1993 Miguel Santana
 * Copyright (c) 1995-1999 Akim Demaille, Miguel Santana
 *
 */

/*
 * This file is part of a2ps
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING.  If not, write to
 * the Free Software Foundation, 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*
 * $Id: parsessh.y,v 1.1.1.1.2.2 2007/12/29 01:58:35 mhatta Exp $
 */

#include "a2ps.h"
#include "jobs.h"
#include "ffaces.h"
#include "ssheet.h"
#include "message.h"
#include "routines.h"
#include "yy2ssh.h"
#include "regex.h"

#define YYDEBUG 1
#define YYERROR_VERBOSE 1
#define YYPRINT(file, type, value)   yyprint (file, type, value)

/* We need to use the same `const' as bison, to avoid the following
   prototypes to diverge from the function declarations */
#undef const
#ifndef __cplusplus
# ifndef __STDC__
#  define const
# endif
#endif

/* Comes from the parser */
extern int sshlineno;

/* Comes from the caller */
extern FILE * sshin;
extern struct a2ps_job * job;
extern const char * sshfilename;

/* Local prototypes */
void yyerror PARAMS ((const char *msg));
static void yyprint ();

/* Initilizes the obstacks */
void sshlex_initialize PARAMS ((void));

/* Comes from main.c */
extern int highlight_level;

int yylex PARAMS ((void));
struct style_sheet * parse_style_sheet PARAMS ((const char * filename));

/* Defines the style sheet being loaded */
static struct style_sheet * parsed_style_sheet = NULL;



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
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
# define YYTOKEN_TABLE 0
#endif

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
/* Line 187 of yacc.c.  */
#line 260 "parsessh.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 273 "parsessh.c"

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
#define YYFINAL  5
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   218

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  39
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  30
/* YYNRULES -- Number of rules.  */
#define YYNRULES  92
/* YYNRULES -- Number of states.  */
#define YYNSTATES  200

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   289

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      36,    37,     2,    38,    35,     2,     2,     2,     2,     2,
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
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,    12,    13,    19,    24,    30,    35,
      39,    42,    45,    50,    56,    62,    65,    68,    71,    74,
      77,    81,    86,    92,    94,    97,   101,   105,   108,   114,
     116,   120,   123,   125,   128,   130,   133,   135,   137,   140,
     142,   144,   146,   150,   152,   154,   158,   160,   162,   166,
     170,   173,   175,   180,   186,   190,   192,   194,   198,   202,
     205,   207,   212,   218,   222,   224,   226,   230,   234,   237,
     239,   244,   250,   252,   256,   262,   269,   275,   281,   286,
     293,   299,   305,   310,   318,   326,   328,   330,   331,   333,
     335,   339,   340
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      40,     0,    -1,    41,    -1,     3,    30,     4,    42,     5,
       3,    -1,    -1,    42,    20,     6,    58,     6,    -1,    42,
       6,    58,     6,    -1,    42,    20,     9,    61,     9,    -1,
      42,     9,    61,     9,    -1,    42,    20,    64,    -1,    42,
      64,    -1,    42,    48,    -1,    42,    14,     7,    30,    -1,
      42,    11,    13,     4,    30,    -1,    42,    12,    13,     4,
      30,    -1,    42,    50,    -1,    42,    44,    -1,    42,    46,
      -1,    42,    47,    -1,    42,    43,    -1,    25,    26,    30,
      -1,    25,    26,    24,    30,    -1,    15,     4,    45,     5,
      15,    -1,    30,    -1,    45,    30,    -1,    22,    23,    30,
      -1,    24,     4,    30,    -1,    24,    30,    -1,    27,     7,
      49,     5,    27,    -1,    30,    -1,    49,    35,    30,    -1,
      17,    33,    -1,    32,    -1,    51,    32,    -1,    53,    -1,
      30,    55,    -1,    30,    -1,    55,    -1,    34,    55,    -1,
      34,    -1,    31,    -1,    53,    -1,    54,    35,    53,    -1,
      28,    -1,    29,    -1,    36,    56,    37,    -1,    28,    -1,
      29,    -1,    56,    38,    28,    -1,    56,    38,    29,    -1,
      30,    52,    -1,    30,    -1,    36,    30,    54,    37,    -1,
       8,    55,     7,    59,     5,    -1,     7,    59,     5,    -1,
      57,    -1,    60,    -1,    59,    35,    57,    -1,    59,    35,
      60,    -1,    51,    52,    -1,    51,    -1,    36,    51,    54,
      37,    -1,     8,    55,     7,    62,     5,    -1,     7,    62,
       5,    -1,    57,    -1,    63,    -1,    62,    35,    57,    -1,
      62,    35,    63,    -1,    51,    52,    -1,    51,    -1,    36,
      51,    54,    37,    -1,    10,     7,    65,     5,    10,    -1,
      66,    -1,    65,    35,    66,    -1,    30,    31,    55,    67,
      68,    -1,    30,    30,    55,    55,    67,    68,    -1,    30,
      55,    55,    67,    68,    -1,    30,    30,    55,    67,    68,
      -1,    30,    55,    67,    68,    -1,    51,    30,    55,    55,
      67,    68,    -1,    51,    55,    55,    67,    68,    -1,    51,
      30,    55,    67,    68,    -1,    51,    55,    67,    68,    -1,
      36,    30,    54,    37,    55,    67,    68,    -1,    36,    51,
      54,    37,    55,    67,    68,    -1,    18,    -1,    19,    -1,
      -1,    57,    -1,    63,    -1,    21,    61,    21,    -1,    -1,
      16,    61,    16,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   129,   129,   136,   145,   148,   157,   163,   172,   178,
     186,   191,   196,   204,   211,   218,   221,   224,   227,   230,
     245,   246,   249,   251,   252,   267,   270,   271,   276,   283,
     291,   298,   306,   310,   322,   330,   334,   338,   342,   346,
     350,   357,   363,   374,   379,   386,   396,   401,   406,   412,
     426,   431,   437,   449,   453,   460,   465,   470,   475,   487,
     492,   499,   509,   513,   520,   526,   532,   537,   549,   554,
     560,   570,   573,   579,   602,   610,   618,   626,   634,   642,
     650,   658,   666,   674,   681,   689,   693,   701,   709,   715,
     721,   730,   733
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "tSTYLE", "tIS", "tEND", "tKEYWORDS",
  "tARE", "tIN", "tOPERATORS", "tSEQUENCES", "tFIRST", "tSECOND",
  "tALPHABET", "tALPHABETS", "tDOCUMENTATION", "tEXCEPTIONS", "tCASE",
  "tCSTRING", "tCCHAR", "tOPTIONAL", "tCLOSERS", "tWRITTEN", "tBY",
  "tVERSION", "tREQUIRES", "tA2PS", "tANCESTORS", "tFACE", "tFFLAGS",
  "tSTRING", "tLATEXSYMBOL", "tREGEX", "tSENSITIVENESS", "tBACK_REF",
  "','", "'('", "')'", "'+'", "$accept", "file", "style_sheet",
  "definition_list", "requirement", "documentation", "long_string",
  "authors", "version", "ancestors_def", "ancestors_list", "case_def",
  "regex", "rhs", "a_rhs", "rhs_list", "fface", "fface_sxp", "rule",
  "keywords_def", "keywords_rule_list", "keyword_regex", "operators_def",
  "operators_rule_list", "operator_regex", "sequence_def", "sequence_list",
  "sequence", "closers_opt", "exception_def_opt", 0
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
     285,   286,   287,   288,   289,    44,    40,    41,    43
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    39,    40,    41,    42,    42,    42,    42,    42,    42,
      42,    42,    42,    42,    42,    42,    42,    42,    42,    42,
      43,    43,    44,    45,    45,    46,    47,    47,    48,    49,
      49,    50,    51,    51,    52,    53,    53,    53,    53,    53,
      53,    54,    54,    55,    55,    55,    56,    56,    56,    56,
      57,    57,    57,    58,    58,    59,    59,    59,    59,    60,
      60,    60,    61,    61,    62,    62,    62,    62,    63,    63,
      63,    64,    65,    65,    66,    66,    66,    66,    66,    66,
      66,    66,    66,    66,    66,    66,    66,    67,    67,    67,
      67,    68,    68
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     6,     0,     5,     4,     5,     4,     3,
       2,     2,     4,     5,     5,     2,     2,     2,     2,     2,
       3,     4,     5,     1,     2,     3,     3,     2,     5,     1,
       3,     2,     1,     2,     1,     2,     1,     1,     2,     1,
       1,     1,     3,     1,     1,     3,     1,     1,     3,     3,
       2,     1,     4,     5,     3,     1,     1,     3,     3,     2,
       1,     4,     5,     3,     1,     1,     3,     3,     2,     1,
       4,     5,     1,     3,     5,     6,     5,     5,     4,     6,
       5,     5,     4,     7,     7,     1,     1,     0,     1,     1,
       3,     0,     3
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,     2,     0,     1,     4,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    19,    16,    17,    18,    11,    15,    10,     3,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    31,     0,     0,     9,     0,     0,    27,     0,     0,
      51,    32,     0,    60,    55,     0,    56,    43,    44,     0,
       0,     6,     0,    69,    64,     0,    65,     0,     8,    85,
      86,     0,     0,     0,     0,    72,     0,     0,    12,    23,
       0,     0,     0,    25,    26,     0,    20,    29,     0,    36,
      40,    39,    50,    34,    37,     0,     0,    33,    59,    54,
       0,    46,    47,     0,     0,     0,    68,    63,     0,     0,
       0,     0,    87,     0,     0,     0,    87,     0,     0,    13,
      14,     0,    24,     5,     7,    21,     0,     0,    35,    38,
      41,     0,     0,    57,    58,    45,     0,     0,     0,    66,
      67,     0,    87,    87,     0,     0,    87,    88,    89,    91,
       0,     0,    87,    87,    91,    71,    73,    22,    28,    30,
       0,    52,    61,    48,    49,    53,    70,    62,    87,    91,
      91,     0,    91,     0,    78,     0,     0,    87,    91,    91,
      82,    42,    91,    77,    74,    90,    76,     0,    87,    87,
      91,    81,    80,    75,    92,    91,    91,    79,    83,    84
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,     3,     7,    22,    23,    80,    24,    25,    26,
      88,    27,    63,    92,   130,   131,    94,   103,   147,    32,
      55,    56,    35,    65,   148,    28,    74,    75,   149,   174
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -150
static const yytype_int16 yypact[] =
{
      11,    -2,    31,  -150,    39,  -150,  -150,    98,    66,   126,
     183,    56,    62,    65,    75,    91,    40,   175,    77,     7,
      80,   109,  -150,  -150,  -150,  -150,  -150,  -150,  -150,  -150,
      69,    60,   132,    94,    60,   133,    35,   131,   142,   140,
     167,  -150,   126,   183,  -150,   168,   169,  -150,     8,   170,
     129,  -150,    18,   120,  -150,     1,  -150,  -150,  -150,   154,
     189,  -150,    18,   120,  -150,     4,  -150,   194,  -150,  -150,
    -150,   138,    34,   143,    10,  -150,   172,   173,  -150,  -150,
       5,   198,   196,  -150,  -150,   176,  -150,  -150,    14,    60,
    -150,    60,  -150,  -150,  -150,   129,   120,  -150,  -150,  -150,
      69,  -150,  -150,   155,    69,   120,  -150,  -150,    94,    94,
      60,    60,   111,   129,   120,    60,   111,   197,    35,  -150,
    -150,   146,  -150,  -150,  -150,  -150,   181,   179,  -150,  -150,
    -150,    92,   118,  -150,  -150,  -150,   166,    21,   127,  -150,
    -150,    22,   111,    38,   183,   148,    38,  -150,  -150,   195,
     151,   152,   111,    38,   195,  -150,  -150,  -150,  -150,  -150,
     129,  -150,  -150,  -150,  -150,  -150,  -150,  -150,    38,   195,
     195,   191,   195,   183,  -150,    60,    60,    38,   195,   195,
    -150,  -150,   195,  -150,  -150,  -150,  -150,   199,    38,    38,
     195,  -150,  -150,  -150,  -150,   195,   195,  -150,  -150,  -150
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -150,  -150,  -150,  -150,  -150,  -150,  -150,  -150,  -150,  -150,
    -150,  -150,   -28,   -41,   -46,    23,   -31,  -150,   -17,   171,
     106,   114,   -42,   107,   -15,   200,  -150,   100,   -91,  -149
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint8 yytable[] =
{
      60,    82,    53,    67,    93,   180,    99,    93,    73,   107,
     121,    46,    98,    54,     1,   117,    64,    93,    66,   126,
     183,   184,   106,   186,    96,   154,   165,   167,     4,   191,
     192,     5,    85,   193,   105,   122,   100,    47,    86,   108,
     112,   197,   116,     6,   114,   118,   198,   199,    95,   127,
      51,   169,   170,    69,    70,   172,   100,   108,   128,   144,
     129,   178,   179,    36,   113,    71,    51,    51,    50,    29,
      51,    72,    53,    41,    62,    37,    53,   182,    38,   142,
     143,   146,    39,   133,   152,   153,   190,    54,    57,    58,
      73,   139,    64,   140,    66,    40,    59,   195,   196,    50,
      45,    51,   171,     8,     9,    52,    48,    10,    11,    12,
      13,   168,    14,    15,   181,    16,    49,   105,    17,   132,
      18,   177,    19,    20,    50,    21,    51,   160,   138,   161,
      62,   187,   144,    30,    31,    76,   150,   151,    61,    57,
      58,    50,    68,    51,   188,   189,    77,   145,    57,    58,
      89,    90,    97,   160,    91,   162,    59,    57,    58,    89,
      90,   157,   160,    91,   166,    59,    57,    58,   110,   111,
      78,    57,    58,   115,    59,    97,   101,   102,    95,    59,
      51,    42,   101,   102,    43,    11,   160,   160,   175,   176,
      33,    34,   135,   136,   163,   164,   104,    79,    83,    84,
      87,   109,   119,   120,   123,   124,   125,   155,   158,   159,
     137,   173,   185,    81,   134,   194,   141,    44,   156
};

static const yytype_uint8 yycheck[] =
{
      31,    43,    30,    34,    50,   154,     5,    53,    36,     5,
       5,     4,    53,    30,     3,     5,    33,    63,    33,     5,
     169,   170,    63,   172,    52,   116,     5,     5,    30,   178,
     179,     0,    24,   182,    62,    30,    35,    30,    30,    35,
      71,   190,    73,     4,    72,    35,   195,   196,    30,    35,
      32,   142,   143,    18,    19,   146,    35,    35,    89,    21,
      91,   152,   153,     7,    30,    30,    32,    32,    30,     3,
      32,    36,   100,    33,    36,    13,   104,   168,    13,   110,
     111,   112,     7,   100,   115,   116,   177,   104,    28,    29,
     118,   108,   109,   108,   109,     4,    36,   188,   189,    30,
      23,    32,   144,     5,     6,    36,    26,     9,    10,    11,
      12,   142,    14,    15,   160,    17,     7,   145,    20,    96,
      22,   152,    24,    25,    30,    27,    32,    35,   105,    37,
      36,   173,    21,     7,     8,     4,   113,   114,     6,    28,
      29,    30,     9,    32,   175,   176,     4,    36,    28,    29,
      30,    31,    32,    35,    34,    37,    36,    28,    29,    30,
      31,    15,    35,    34,    37,    36,    28,    29,    30,    31,
      30,    28,    29,    30,    36,    32,    28,    29,    30,    36,
      32,     6,    28,    29,     9,    10,    35,    35,    37,    37,
       7,     8,    37,    38,    28,    29,     7,    30,    30,    30,
      30,     7,    30,    30,     6,     9,    30,    10,    27,    30,
     104,    16,    21,    42,   100,    16,   109,    17,   118
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,    40,    41,    30,     0,     4,    42,     5,     6,
       9,    10,    11,    12,    14,    15,    17,    20,    22,    24,
      25,    27,    43,    44,    46,    47,    48,    50,    64,     3,
       7,     8,    58,     7,     8,    61,     7,    13,    13,     7,
       4,    33,     6,     9,    64,    23,     4,    30,    26,     7,
      30,    32,    36,    51,    57,    59,    60,    28,    29,    36,
      55,     6,    36,    51,    57,    62,    63,    55,     9,    18,
      19,    30,    36,    51,    65,    66,     4,     4,    30,    30,
      45,    58,    61,    30,    30,    24,    30,    30,    49,    30,
      31,    34,    52,    53,    55,    30,    51,    32,    52,     5,
      35,    28,    29,    56,     7,    51,    52,     5,    35,     7,
      30,    31,    55,    30,    51,    30,    55,     5,    35,    30,
      30,     5,    30,     6,     9,    30,     5,    35,    55,    55,
      53,    54,    54,    57,    60,    37,    38,    59,    54,    57,
      63,    62,    55,    55,    21,    36,    55,    57,    63,    67,
      54,    54,    55,    55,    67,    10,    66,    15,    27,    30,
      35,    37,    37,    28,    29,     5,    37,     5,    55,    67,
      67,    61,    67,    16,    68,    37,    37,    55,    67,    67,
      68,    53,    67,    68,    68,    21,    68,    61,    55,    55,
      67,    68,    68,    68,    16,    67,    67,    68,    68,    68
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
#line 130 "parsessh.y"
    {
      parsed_style_sheet = (yyvsp[(1) - (1)].sheet);
    }
    break;

  case 3:
#line 137 "parsessh.y"
    {
	(yyvsp[(4) - (6)].sheet)->name = (yyvsp[(2) - (6)].string);
	(yyvsp[(4) - (6)].sheet)->key = "<No key yet>";
	(yyval.sheet) = (yyvsp[(4) - (6)].sheet);
      }
    break;

  case 4:
#line 145 "parsessh.y"
    {
	  (yyval.sheet) = new_style_sheet ((const uchar *) "<no name>");
	}
    break;

  case 5:
#line 149 "parsessh.y"
    {
	  if (highlight_level == 2) {
	    words_set_no_face ((yyvsp[(4) - (5)].words), Plain_fface);
	    words_merge_rules_unique ((yyvsp[(1) - (5)].sheet)->keywords, (yyvsp[(4) - (5)].words));
	  } else
	    words_free ((yyvsp[(4) - (5)].words));
	  (yyval.sheet) = (yyvsp[(1) - (5)].sheet);
	}
    break;

  case 6:
#line 158 "parsessh.y"
    {
	  words_set_no_face ((yyvsp[(3) - (4)].words), Plain_fface);
	  words_merge_rules_unique ((yyvsp[(1) - (4)].sheet)->keywords, (yyvsp[(3) - (4)].words));
	  (yyval.sheet) = (yyvsp[(1) - (4)].sheet);
	}
    break;

  case 7:
#line 164 "parsessh.y"
    {
	  if (highlight_level == 2) {
	    words_set_no_face ((yyvsp[(4) - (5)].words), Plain_fface);
	    words_merge_rules_unique ((yyvsp[(1) - (5)].sheet)->operators, (yyvsp[(4) - (5)].words));
	  } else
	    words_free ((yyvsp[(4) - (5)].words));
	  (yyval.sheet) = (yyvsp[(1) - (5)].sheet);
	}
    break;

  case 8:
#line 173 "parsessh.y"
    {
	  words_set_no_face ((yyvsp[(3) - (4)].words), Plain_fface);
	  words_merge_rules_unique ((yyvsp[(1) - (4)].sheet)->operators, (yyvsp[(3) - (4)].words));
	  (yyval.sheet) = (yyvsp[(1) - (4)].sheet);
	}
    break;

  case 9:
#line 178 "parsessh.y"
    {
	  if (highlight_level == 2) {
	    da_concat ((yyvsp[(1) - (3)].sheet)->sequences, (yyvsp[(3) - (3)].array));
	    da_erase ((yyvsp[(3) - (3)].array));
	  } else
	    da_free ((yyvsp[(3) - (3)].array), (da_map_func_t) free_sequence);
	  (yyval.sheet) = (yyvsp[(1) - (3)].sheet);
	}
    break;

  case 10:
#line 186 "parsessh.y"
    {
	  da_concat ((yyvsp[(1) - (2)].sheet)->sequences, (yyvsp[(2) - (2)].array));
	  da_erase ((yyvsp[(2) - (2)].array));
	  (yyval.sheet) = (yyvsp[(1) - (2)].sheet);
	}
    break;

  case 11:
#line 191 "parsessh.y"
    {
	  da_concat ((yyvsp[(1) - (2)].sheet)->ancestors, (yyvsp[(2) - (2)].array));
	  da_erase ((yyvsp[(2) - (2)].array));
	  (yyval.sheet) = (yyvsp[(1) - (2)].sheet);
	}
    break;

  case 12:
#line 196 "parsessh.y"
    {
	  string_to_array ((yyvsp[(1) - (4)].sheet)->alpha1, (yyvsp[(4) - (4)].string));
	  string_to_array ((yyvsp[(1) - (4)].sheet)->alpha2, (yyvsp[(4) - (4)].string));
	  /* This is the syntax table used by regex */
	  free ((yyvsp[(4) - (4)].string));
	  (yyvsp[(4) - (4)].string) = NULL;
	  (yyval.sheet) = (yyvsp[(1) - (4)].sheet);
	}
    break;

  case 13:
#line 204 "parsessh.y"
    {
	  string_to_array ((yyvsp[(1) - (5)].sheet)->alpha1, (yyvsp[(5) - (5)].string));
	  /* This is the syntax table used by regex */
	  free ((yyvsp[(5) - (5)].string));
	  (yyvsp[(5) - (5)].string) = NULL;
	  (yyval.sheet) = (yyvsp[(1) - (5)].sheet);
	}
    break;

  case 14:
#line 211 "parsessh.y"
    {
	  string_to_array ((yyvsp[(1) - (5)].sheet)->alpha2, (yyvsp[(5) - (5)].string));
	  /* This is the syntax table used by regex */
	  free ((yyvsp[(5) - (5)].string));
	  (yyvsp[(5) - (5)].string) = NULL;
	  (yyval.sheet) = (yyvsp[(1) - (5)].sheet);
	}
    break;

  case 15:
#line 218 "parsessh.y"
    {
	  (yyvsp[(1) - (2)].sheet)->sensitiveness = (yyvsp[(2) - (2)].sensitiveness);
	}
    break;

  case 16:
#line 221 "parsessh.y"
    {
	  (yyvsp[(1) - (2)].sheet)->documentation = (yyvsp[(2) - (2)].string);
	}
    break;

  case 17:
#line 224 "parsessh.y"
    {
	  (yyvsp[(1) - (2)].sheet)->author = (yyvsp[(2) - (2)].string);
	}
    break;

  case 18:
#line 227 "parsessh.y"
    {
	  style_sheet_set_version ((yyvsp[(1) - (2)].sheet), (const char *) (yyvsp[(2) - (2)].string));
	}
    break;

  case 19:
#line 230 "parsessh.y"
    {
	  /* Make sure now that we won't encounter new tokens.
	   * This avoids nasty error messages, or worse:
	   * unexpected behavior at run time */
	  if (!style_sheet_set_requirement ((yyvsp[(1) - (2)].sheet), (const char *) (yyvsp[(2) - (2)].string)))
	    error (1, 0,
		   _("cannot process `%s' which requires a2ps version %s"),
		   sshfilename, (yyvsp[(2) - (2)].string));
	}
    break;

  case 20:
#line 245 "parsessh.y"
    { (yyval.string) = (yyvsp[(3) - (3)].string) ; }
    break;

  case 21:
#line 246 "parsessh.y"
    { (yyval.string) = (yyvsp[(4) - (4)].string) ; }
    break;

  case 22:
#line 249 "parsessh.y"
    { (yyval.string) = (yyvsp[(3) - (5)].string) ; }
    break;

  case 23:
#line 251 "parsessh.y"
    { (yyval.string) = (yyvsp[(1) - (1)].string); }
    break;

  case 24:
#line 253 "parsessh.y"
    {
	  size_t len1;
	  size_t len2;

	  len1 = ustrlen ((yyvsp[(1) - (2)].string));
	  (yyvsp[(1) - (2)].string)[len1] = '\n';
	  len2 = ustrlen ((yyvsp[(2) - (2)].string));
	  (yyval.string) = XMALLOC (uchar, len1 + len2 + 2);
	  ustpcpy (ustpncpy ((yyval.string), (yyvsp[(1) - (2)].string), len1 + 1), (yyvsp[(2) - (2)].string));
	  free ((yyvsp[(1) - (2)].string));
	  free ((yyvsp[(2) - (2)].string));
	}
    break;

  case 25:
#line 267 "parsessh.y"
    { (yyval.string) = (yyvsp[(3) - (3)].string) ; }
    break;

  case 26:
#line 270 "parsessh.y"
    { (yyval.string) = (yyvsp[(3) - (3)].string) ; }
    break;

  case 27:
#line 271 "parsessh.y"
    { (yyval.string) = (yyvsp[(2) - (2)].string) ; }
    break;

  case 28:
#line 277 "parsessh.y"
    {
	  /* The list of keys of style sheets from which it inherits */
  	  (yyval.array) = (yyvsp[(3) - (5)].array);
	}
    break;

  case 29:
#line 284 "parsessh.y"
    {
	  /* Create a list of ancestors, and drop the new one in */
	  (yyval.array) = da_new ("Ancestors tmp", 2,
		       da_linear, 2,
		       (da_print_func_t) da_str_print, NULL);
	  da_append ((yyval.array), (yyvsp[(1) - (1)].string));
	}
    break;

  case 30:
#line 292 "parsessh.y"
    {
	  da_append ((yyvsp[(1) - (3)].array), (yyvsp[(3) - (3)].string));
	  (yyval.array) = (yyvsp[(1) - (3)].array);
	}
    break;

  case 31:
#line 298 "parsessh.y"
    { (yyval.sensitiveness) = (yyvsp[(2) - (2)].sensitiveness) ; }
    break;

  case 32:
#line 307 "parsessh.y"
    {
	  (yyval.pattern) = (yyvsp[(1) - (1)].pattern);
	}
    break;

  case 33:
#line 311 "parsessh.y"
    {
	  /* Concatenate $2 to $1 makes $$ */
	  (yyval.pattern) = (yyvsp[(1) - (2)].pattern);
	  (yyval.pattern)->pattern = XREALLOC ((yyval.pattern)->pattern, char, (yyvsp[(1) - (2)].pattern)->len + (yyvsp[(2) - (2)].pattern)->len + 1);
	  strncpy ((yyval.pattern)->pattern + (yyval.pattern)->len, (yyvsp[(2) - (2)].pattern)->pattern, (yyvsp[(2) - (2)].pattern)->len);
	  (yyval.pattern)->len += (yyvsp[(2) - (2)].pattern)->len;
	  free ((yyvsp[(2) - (2)].pattern)->pattern);
	}
    break;

  case 34:
#line 323 "parsessh.y"
    {
	  (yyval.array) = rhs_new ();
	  rhs_add ((yyval.array), (yyvsp[(1) - (1)].faced_string));
	}
    break;

  case 35:
#line 331 "parsessh.y"
    {
	  (yyval.faced_string) = faced_string_new  ((yyvsp[(1) - (2)].string), 0, (yyvsp[(2) - (2)].fface));
	}
    break;

  case 36:
#line 335 "parsessh.y"
    {
	  (yyval.faced_string) = faced_string_new  ((yyvsp[(1) - (1)].string), 0, No_fface);
	}
    break;

  case 37:
#line 339 "parsessh.y"
    {
	  (yyval.faced_string) = faced_string_new  (NULL, 0, (yyvsp[(1) - (1)].fface));
	}
    break;

  case 38:
#line 343 "parsessh.y"
    {
	  (yyval.faced_string) = faced_string_new  (UNULL, (yyvsp[(1) - (2)].integer), (yyvsp[(2) - (2)].fface));
	}
    break;

  case 39:
#line 347 "parsessh.y"
    {
	  (yyval.faced_string) = faced_string_new  (UNULL, (yyvsp[(1) - (1)].integer), No_fface);
	}
    break;

  case 40:
#line 351 "parsessh.y"
    {
	  (yyval.faced_string) = faced_string_new ((yyvsp[(1) - (1)].string), 0, Symbol_fface);
	}
    break;

  case 41:
#line 358 "parsessh.y"
    {
	  (yyval.array) = rhs_new ();
	  rhs_add ((yyval.array), (yyvsp[(1) - (1)].faced_string));
	}
    break;

  case 42:
#line 364 "parsessh.y"
    {
	  rhs_add ((yyvsp[(1) - (3)].array), (yyvsp[(3) - (3)].faced_string));
	  (yyval.array) = (yyvsp[(1) - (3)].array);
	}
    break;

  case 43:
#line 375 "parsessh.y"
    {
	  fface_set_face ((yyval.fface), (yyvsp[(1) - (1)].face));
	  fface_reset_flags ((yyval.fface));
	}
    break;

  case 44:
#line 380 "parsessh.y"
    {
	  fface_reset_face ((yyval.fface));
	  fface_set_flags ((yyval.fface), (yyvsp[(1) - (1)].fflags));
	  /* If there is no face, then set Invisible */
	  fface_add_flags ((yyval.fface), ff_Invisible);
	}
    break;

  case 45:
#line 387 "parsessh.y"
    {
	  (yyval.fface) = (yyvsp[(2) - (3)].fface);
	  /* If there is no face, then set Invisible */
	  if (fface_get_face ((yyval.fface)) == No_face)
	    fface_add_flags ((yyval.fface), ff_Invisible);
	}
    break;

  case 46:
#line 397 "parsessh.y"
    {
	  fface_set_face((yyval.fface), (yyvsp[(1) - (1)].face));
	  fface_reset_flags((yyval.fface));
	}
    break;

  case 47:
#line 402 "parsessh.y"
    {
	  fface_reset_face((yyval.fface));
	  fface_set_flags((yyval.fface), (yyvsp[(1) - (1)].fflags));
	}
    break;

  case 48:
#line 407 "parsessh.y"
    {
	  /* FIXME: Overloading of the face should be forbidden */
	  (yyval.fface) = (yyvsp[(1) - (3)].fface);
	  fface_set_face((yyval.fface), (yyvsp[(3) - (3)].face));
	}
    break;

  case 49:
#line 413 "parsessh.y"
    {
	  (yyval.fface) = (yyvsp[(1) - (3)].fface);
	  fface_add_flags((yyval.fface), (yyvsp[(3) - (3)].fflags));
	}
    break;

  case 50:
#line 427 "parsessh.y"
    {
	  (yyval.rule) = rule_new ((yyvsp[(1) - (2)].string), NULL, (yyvsp[(2) - (2)].array),
			 sshfilename, sshlineno);
	}
    break;

  case 51:
#line 432 "parsessh.y"
    {
	  (yyval.rule) = rule_new ((yyvsp[(1) - (1)].string), NULL,
			 rhs_new_single (UNULL, 0, No_fface),
			 sshfilename, sshlineno);
	}
    break;

  case 52:
#line 438 "parsessh.y"
    {
	  (yyval.rule) = rule_new ((yyvsp[(2) - (4)].string), NULL, (yyvsp[(3) - (4)].array),
			 sshfilename, sshlineno);
	}
    break;

  case 53:
#line 449 "parsessh.y"
    {
	  words_set_no_face ((yyvsp[(4) - (5)].words), (yyvsp[(2) - (5)].fface));
	  (yyval.words) = (yyvsp[(4) - (5)].words);
	}
    break;

  case 54:
#line 453 "parsessh.y"
    {
  	  /* First of all, the No_face must be turned into Plain */
  	  (yyval.words) = (yyvsp[(2) - (3)].words);
	}
    break;

  case 55:
#line 461 "parsessh.y"
    {
	  (yyval.words) = words_new ("Keywords: Strings", "Keywords: Regexps", 100, 100);
	  words_add_string ((yyval.words), (yyvsp[(1) - (1)].rule));
	}
    break;

  case 56:
#line 466 "parsessh.y"
    {
	  (yyval.words) = words_new ("Keywords: Strings", "Keywords: Regexps", 100, 100);
	  words_add_regex ((yyval.words), (yyvsp[(1) - (1)].rule));
	}
    break;

  case 57:
#line 471 "parsessh.y"
    {
	  words_add_string ((yyvsp[(1) - (3)].words), (yyvsp[(3) - (3)].rule));
	  (yyval.words) = (yyvsp[(1) - (3)].words);
	}
    break;

  case 58:
#line 476 "parsessh.y"
    {
	  words_add_regex ((yyvsp[(1) - (3)].words), (yyvsp[(3) - (3)].rule));
	  (yyval.words) = (yyvsp[(1) - (3)].words);
	}
    break;

  case 59:
#line 488 "parsessh.y"
    {
	  (yyval.rule) = keyword_rule_new (UNULL, (yyvsp[(1) - (2)].pattern), (yyvsp[(2) - (2)].array),
				 sshfilename, sshlineno);
	}
    break;

  case 60:
#line 493 "parsessh.y"
    {
	  (yyval.rule) = keyword_rule_new (UNULL, (yyvsp[(1) - (1)].pattern),
				   rhs_new_single (UNULL, 0,
							    No_fface),
				 sshfilename, sshlineno);
	}
    break;

  case 61:
#line 500 "parsessh.y"
    {
	  (yyval.rule) = keyword_rule_new (UNULL, (yyvsp[(2) - (4)].pattern), (yyvsp[(3) - (4)].array),
				 sshfilename, sshlineno);
	}
    break;

  case 62:
#line 509 "parsessh.y"
    {
	  words_set_no_face ((yyvsp[(4) - (5)].words), (yyvsp[(2) - (5)].fface));
	  (yyval.words) = (yyvsp[(4) - (5)].words);
	}
    break;

  case 63:
#line 513 "parsessh.y"
    {
  	  /* First of all, the No_face must be turned into Plain */
  	  (yyval.words) = (yyvsp[(2) - (3)].words);
	}
    break;

  case 64:
#line 521 "parsessh.y"
    {
	  (yyval.words) = words_new ("Operators: Strings", "Operators: Regexps",
			  100, 100);
	  words_add_string ((yyval.words), (yyvsp[(1) - (1)].rule));
	}
    break;

  case 65:
#line 527 "parsessh.y"
    {
	  (yyval.words) = words_new ("Operators: Strings", "Operators: Regexps",
			  100, 100);
	  words_add_regex ((yyval.words), (yyvsp[(1) - (1)].rule));
	}
    break;

  case 66:
#line 533 "parsessh.y"
    {
	  words_add_string ((yyvsp[(1) - (3)].words), (yyvsp[(3) - (3)].rule));
	  (yyval.words) = (yyvsp[(1) - (3)].words);
	}
    break;

  case 67:
#line 538 "parsessh.y"
    {
	  words_add_regex ((yyvsp[(1) - (3)].words), (yyvsp[(3) - (3)].rule));
	  (yyval.words) = (yyvsp[(1) - (3)].words);
	}
    break;

  case 68:
#line 550 "parsessh.y"
    {
	  (yyval.rule) = rule_new (UNULL, (yyvsp[(1) - (2)].pattern), (yyvsp[(2) - (2)].array),
			 sshfilename, sshlineno);
	}
    break;

  case 69:
#line 555 "parsessh.y"
    {
	  (yyval.rule) = rule_new (UNULL, (yyvsp[(1) - (1)].pattern),
			   rhs_new_single (UNULL, 0, No_fface),
			 sshfilename, sshlineno);
	}
    break;

  case 70:
#line 561 "parsessh.y"
    {
	  (yyval.rule) = rule_new (UNULL, (yyvsp[(2) - (4)].pattern), (yyvsp[(3) - (4)].array),
			 sshfilename, sshlineno);
	}
    break;

  case 71:
#line 570 "parsessh.y"
    { (yyval.array) = (yyvsp[(3) - (5)].array); }
    break;

  case 72:
#line 573 "parsessh.y"
    {
	  (yyval.array) = da_new ("Sequence tmp", 100,
		       da_linear, 100,
		       (da_print_func_t) sequence_self_print, NULL);
	  da_append ((yyval.array), (yyvsp[(1) - (1)].sequence));
	}
    break;

  case 73:
#line 579 "parsessh.y"
    {
	  da_append ((yyvsp[(1) - (3)].array), (yyvsp[(3) - (3)].sequence));
	  (yyval.array) = (yyvsp[(1) - (3)].array);
	}
    break;

  case 74:
#line 603 "parsessh.y"
    {
	  struct rule * open_rule;
	  open_rule = rule_new ((yyvsp[(1) - (5)].string), NULL,
				rhs_new_single ((yyvsp[(2) - (5)].string), 0, Symbol_fface),
				sshfilename, sshlineno);
	  (yyval.sequence) = sequence_new (open_rule, (yyvsp[(3) - (5)].fface), (yyvsp[(4) - (5)].words), (yyvsp[(5) - (5)].words));
	}
    break;

  case 75:
#line 611 "parsessh.y"
    {
	  struct rule * open_rule;
	  open_rule = rule_new ((yyvsp[(1) - (6)].string), NULL,
				rhs_new_single ((yyvsp[(2) - (6)].string), 0, (yyvsp[(3) - (6)].fface)),
				sshfilename, sshlineno);
	  (yyval.sequence) = sequence_new (open_rule, (yyvsp[(4) - (6)].fface), (yyvsp[(5) - (6)].words), (yyvsp[(6) - (6)].words));
	}
    break;

  case 76:
#line 619 "parsessh.y"
    {
	  struct rule * open_rule;
	  open_rule = rule_new ((yyvsp[(1) - (5)].string), NULL,
				rhs_new_single (UNULL, 0, (yyvsp[(2) - (5)].fface)),
				sshfilename, sshlineno);
	  (yyval.sequence) = sequence_new (open_rule, (yyvsp[(3) - (5)].fface), (yyvsp[(4) - (5)].words), (yyvsp[(5) - (5)].words));
	}
    break;

  case 77:
#line 627 "parsessh.y"
    {
	  struct rule * open_rule;
	  open_rule = rule_new ((yyvsp[(1) - (5)].string), NULL,
				rhs_new_single ((yyvsp[(2) - (5)].string), 0, (yyvsp[(3) - (5)].fface)),
				sshfilename, sshlineno);
	  (yyval.sequence) = sequence_new (open_rule, (yyvsp[(3) - (5)].fface), (yyvsp[(4) - (5)].words), (yyvsp[(5) - (5)].words));
	}
    break;

  case 78:
#line 635 "parsessh.y"
    {
	  struct rule * open_rule;
	  open_rule = rule_new ((yyvsp[(1) - (4)].string), NULL,
				rhs_new_single (UNULL, 0, (yyvsp[(2) - (4)].fface)),
				sshfilename, sshlineno);
	  (yyval.sequence) = sequence_new (open_rule, (yyvsp[(2) - (4)].fface), (yyvsp[(3) - (4)].words), (yyvsp[(4) - (4)].words));
	}
    break;

  case 79:
#line 643 "parsessh.y"
    {
	  struct rule * open_rule;
	  open_rule = rule_new (UNULL, (yyvsp[(1) - (6)].pattern),
				rhs_new_single ((yyvsp[(2) - (6)].string), 0, (yyvsp[(3) - (6)].fface)),
				sshfilename, sshlineno);
	  (yyval.sequence) = sequence_new (open_rule, (yyvsp[(4) - (6)].fface), (yyvsp[(5) - (6)].words), (yyvsp[(6) - (6)].words));
	}
    break;

  case 80:
#line 651 "parsessh.y"
    {
	  struct rule * open_rule;
	  open_rule = rule_new (UNULL, (yyvsp[(1) - (5)].pattern),
				rhs_new_single (UNULL, 0, (yyvsp[(2) - (5)].fface)),
				sshfilename, sshlineno);
	  (yyval.sequence) = sequence_new (open_rule, (yyvsp[(3) - (5)].fface), (yyvsp[(4) - (5)].words), (yyvsp[(5) - (5)].words));
	}
    break;

  case 81:
#line 659 "parsessh.y"
    {
	  struct rule * open_rule;
	  open_rule = rule_new (UNULL, (yyvsp[(1) - (5)].pattern),
				rhs_new_single ((yyvsp[(2) - (5)].string), 0, (yyvsp[(3) - (5)].fface)),
				sshfilename, sshlineno);
	  (yyval.sequence) = sequence_new (open_rule, (yyvsp[(3) - (5)].fface), (yyvsp[(4) - (5)].words), (yyvsp[(5) - (5)].words));
	}
    break;

  case 82:
#line 667 "parsessh.y"
    {
	  struct rule * open_rule;
	  open_rule = rule_new (UNULL, (yyvsp[(1) - (4)].pattern),
				rhs_new_single (UNULL, 0, No_fface),
				sshfilename, sshlineno);
	  (yyval.sequence) = sequence_new (open_rule, (yyvsp[(2) - (4)].fface), (yyvsp[(3) - (4)].words), (yyvsp[(4) - (4)].words));
	}
    break;

  case 83:
#line 675 "parsessh.y"
    {
	  struct rule * open_rule;
	  open_rule = rule_new ((yyvsp[(2) - (7)].string), NULL, (yyvsp[(3) - (7)].array),
				sshfilename, sshlineno);
	  (yyval.sequence) = sequence_new (open_rule, (yyvsp[(5) - (7)].fface), (yyvsp[(6) - (7)].words), (yyvsp[(7) - (7)].words));
	}
    break;

  case 84:
#line 682 "parsessh.y"
    {
	  struct rule * open_rule;
	  open_rule = rule_new (UNULL, (yyvsp[(2) - (7)].pattern), (yyvsp[(3) - (7)].array),
				sshfilename, sshlineno);
	  (yyval.sequence) = sequence_new (open_rule, (yyvsp[(5) - (7)].fface), (yyvsp[(6) - (7)].words), (yyvsp[(7) - (7)].words));
	}
    break;

  case 85:
#line 690 "parsessh.y"
    {
	  (yyval.sequence) = new_C_string_sequence ("\"");
	}
    break;

  case 86:
#line 694 "parsessh.y"
    {
	  (yyval.sequence) = new_C_string_sequence ("\'");
	}
    break;

  case 87:
#line 701 "parsessh.y"
    {
	  /* This is a shortcut which means "up to the end of the line". */
	  (yyval.words) = words_new ("Closing: Strings", "Closing: Regexps", 2, 2);
	  words_add_string ((yyval.words), rule_new (xustrdup ("\n"), NULL,
					  rhs_new_single (NULL, 0,
							  No_fface),
					  sshfilename, sshlineno));
	}
    break;

  case 88:
#line 710 "parsessh.y"
    {
	  /* Only one */
	  (yyval.words) = words_new ("Closing: Strings", "Closing: Regexps", 2, 2);
	  words_add_string ((yyval.words), (yyvsp[(1) - (1)].rule));
	}
    break;

  case 89:
#line 716 "parsessh.y"
    {
	  /* Only one */
	  (yyval.words) = words_new ("Closing: Strings", "Closing: Regexps", 2, 2);
	  words_add_regex ((yyval.words), (yyvsp[(1) - (1)].rule));
	}
    break;

  case 90:
#line 722 "parsessh.y"
    {
	  /* Several, comma separated, between () */
	  (yyval.words) = (yyvsp[(2) - (3)].words);
	}
    break;

  case 91:
#line 730 "parsessh.y"
    {
	  (yyval.words) = words_new ("Exceptions: Strings", "Exceptions: Regexps", 1, 1);
	}
    break;

  case 92:
#line 734 "parsessh.y"
    {
	  (yyval.words) = (yyvsp[(2) - (3)].words);
	}
    break;


/* Line 1267 of yacc.c.  */
#line 2423 "parsessh.c"
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


#line 738 "parsessh.y"


void
yyerror (const char *msg)
{
  error_at_line (1, 0, sshfilename, sshlineno, msg);
}

/*
 * FIXME: Cover the other relevant types
 */
static void
yyprint (FILE *file, int type, YYSTYPE value)
{
  switch (type) {
  case tBACK_REF:
    fprintf (file, " \\%d", value.integer);
    break;

  case tFFLAGS:
    putc (' ', file);
    fflag_self_print (value.fflags, file);
    break;

  case tFACE:
    fprintf (file, " %s", face_to_string (value.face));
    break;

  case tREGEX:
    fprintf (file, " /%s/", value.pattern->pattern);
    break;

  case tSTRING:
    fprintf (file, " \"%s\"", value.string);
    break;
  }
}

struct style_sheet *
parse_style_sheet (const char * filename)
{
  int res;

  sshfilename = filename;
  sshlineno = 1;
  sshin = xrfopen (sshfilename);

  message (msg_file | msg_sheet | msg_parse,
	   (stderr, "Parsing file `%s'\n", sshfilename));

  sshlex_initialize ();

  if (msg_test (msg_parse))
    yydebug = true;
  res = yyparse ();  /* FIXME: test the result of parsing */

  if (msg_test (msg_sheet)) {
    fprintf (stderr, "---------- Right after parsing of %s\n",
	     parsed_style_sheet->key);
    style_sheet_self_print (parsed_style_sheet, stderr);
    fprintf (stderr, "---------- End of after parsing of %s\n",
	     parsed_style_sheet->key);
  }

  fclose (sshin);
  return parsed_style_sheet;
}

