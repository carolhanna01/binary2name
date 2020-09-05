/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

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
#define YYBISON_VERSION "3.0.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 1 "getdate.y" /* yacc.c:339  */

/*
 * March 2005: Further modified and simplified by Tim Kientzle:
 * Eliminate minutes-based calculations (just do everything in
 * seconds), have lexer only recognize unsigned integers (handle '+'
 * and '-' characters in grammar), combine tables into one table with
 * explicit abbreviation notes, do am/pm adjustments in the grammar
 * (eliminate some state variables and post-processing).  Among other
 * things, these changes eliminated two shift/reduce conflicts.  (Went
 * from 10 to 8.)
 * All of Tim Kientzle's changes to this file are public domain.
 */

/*
**  Originally written by Steven M. Bellovin <smb@research.att.com> while
**  at the University of North Carolina at Chapel Hill.  Later tweaked by
**  a couple of people on Usenet.  Completely overhauled by Rich $alz
**  <rsalz@bbn.com> and Jim Berets <jberets@bbn.com> in August, 1990;
**
**  This grammar has 10 shift/reduce conflicts.
**
**  This code is in the public domain and has no copyright.
*/
/* SUPPRESS 287 on yaccpar_sccsid *//* Unused static variable */
/* SUPPRESS 288 on yyerrlab *//* Label unused */

#ifdef __FreeBSD__
#include <sys/cdefs.h>
__FBSDID("$FreeBSD: src/usr.bin/tar/getdate.y,v 1.9 2007/07/20 01:27:50 kientzle Exp $");
#endif

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
/* Bison tries to redefine malloc() and free() without the next define. */
#define	YYINCLUDED_STDLIB_H
#include <string.h>
#include <time.h>

#define yyparse getdate_yyparse
#define yylex getdate_yylex
#define yyerror getdate_yyerror

static int yyparse(void);
static int yylex(void);
static int yyerror(const char *);

time_t get_date(char *);

#define EPOCH		1970
#define HOUR(x)		((time_t)(x) * 60)
#define SECSPERDAY	(24L * 60L * 60L)

/*
**  Daylight-savings mode:  on, off, or not yet known.
*/
typedef enum _DSTMODE {
    DSTon, DSToff, DSTmaybe
} DSTMODE;

/*
**  Meridian:  am or pm.
*/
enum { tAM, tPM };

/*
**  Global variables.  We could get rid of most of these by using a good
**  union as the yacc stack.  (This routine was originally written before
**  yacc had the %union construct.)  Maybe someday; right now we only use
**  the %union very rarely.
*/
static char	*yyInput;

static DSTMODE	yyDSTmode;
static time_t	yyDayOrdinal;
static time_t	yyDayNumber;
static int	yyHaveDate;
static int	yyHaveDay;
static int	yyHaveRel;
static int	yyHaveTime;
static int	yyHaveZone;
static time_t	yyTimezone;
static time_t	yyDay;
static time_t	yyHour;
static time_t	yyMinutes;
static time_t	yyMonth;
static time_t	yySeconds;
static time_t	yyYear;
static time_t	yyRelMonth;
static time_t	yyRelSeconds;


#line 159 "getdate.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif


/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    tAGO = 258,
    tDAY = 259,
    tDAYZONE = 260,
    tAMPM = 261,
    tMONTH = 262,
    tMONTH_UNIT = 263,
    tSEC_UNIT = 264,
    tUNUMBER = 265,
    tZONE = 266,
    tDST = 267
  };
#endif
/* Tokens.  */
#define tAGO 258
#define tDAY 259
#define tDAYZONE 260
#define tAMPM 261
#define tMONTH 262
#define tMONTH_UNIT 263
#define tSEC_UNIT 264
#define tUNUMBER 265
#define tZONE 266
#define tDST 267

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE YYSTYPE;
union YYSTYPE
{
#line 94 "getdate.y" /* yacc.c:355  */

    time_t		Number;

#line 224 "getdate.c" /* yacc.c:355  */
};
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);



/* Copy the second part of user declarations.  */

#line 239 "getdate.c" /* yacc.c:358  */

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
#else
typedef signed char yytype_int8;
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
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
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
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
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
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
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
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  2
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   50

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  18
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  11
/* YYNRULES -- Number of rules.  */
#define YYNRULES  41
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  59

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   267

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    13,    16,    14,     2,    17,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    15,     2,
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
       5,     6,     7,     8,     9,    10,    11,    12
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   106,   106,   107,   110,   111,   112,   113,   114,   115,
     118,   128,   131,   138,   143,   150,   155,   162,   166,   170,
     176,   180,   185,   192,   197,   216,   222,   235,   240,   246,
     251,   259,   263,   266,   270,   274,   278,   282,   286,   290,
     294,   300
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "tAGO", "tDAY", "tDAYZONE", "tAMPM",
  "tMONTH", "tMONTH_UNIT", "tSEC_UNIT", "tUNUMBER", "tZONE", "tDST", "'+'",
  "'-'", "':'", "','", "'/'", "$accept", "spec", "item", "time",
  "bare_time", "zone", "day", "date", "rel", "relunit", "number", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,    43,    45,    58,    44,    47
};
# endif

#define YYPACT_NINF -8

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-8)))

#define YYTABLE_NINF -1

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int8 yypact[] =
{
      -8,     0,    -8,    14,    -8,     2,    -8,    -8,    11,    15,
      19,    21,    -8,    -8,    10,    -8,    -8,    -8,    -8,    29,
      -8,    -8,    17,    -8,    -8,    24,    -8,    -8,    -4,    25,
      26,    -8,    -7,    13,    -8,    27,    28,    -8,    30,    -8,
      31,    32,    33,    22,    -8,    -8,    -8,    -8,    -8,    -8,
      -8,    34,    37,    39,    40,    -8,    -8,    -8,    -8
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,     0,     1,    20,    18,     0,    40,    36,    41,    17,
       0,     0,     3,     4,    11,     5,     7,     6,     8,    32,
       9,    21,    27,    22,    10,    29,    39,    35,     0,     0,
       0,    19,     0,     0,    12,     0,     0,    31,     0,    30,
       0,     0,    15,    23,    38,    34,    37,    33,    13,    14,
      28,     0,     0,     0,     0,    26,    25,    16,    24
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
      -8,    -8,    -8,    -8,    -8,    -8,    -8,    -8,    -8,    -8,
      -8
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     1,    12,    13,    14,    15,    16,    17,    18,    19,
      20
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
       2,    44,    45,    40,     3,     4,    41,     5,     6,     7,
       8,     9,    22,    10,    11,    23,    34,    24,    25,    26,
      27,    46,    47,    35,    36,    28,    29,    31,    30,    32,
      21,    33,    37,    38,    39,    42,    43,    48,    49,    54,
      50,     0,     0,     0,    55,    51,    52,    56,    53,    57,
      58
};

static const yytype_int8 yycheck[] =
{
       0,     8,     9,     7,     4,     5,    10,     7,     8,     9,
      10,    11,    10,    13,    14,     4,     6,     6,     7,     8,
       9,     8,     9,    13,    14,    14,    15,    12,    17,    10,
      16,    10,     3,    16,    10,    10,    10,    10,    10,    17,
      10,    -1,    -1,    -1,    10,    14,    14,    10,    15,    10,
      10
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    19,     0,     4,     5,     7,     8,     9,    10,    11,
      13,    14,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    16,    10,     4,     6,     7,     8,     9,    14,    15,
      17,    12,    10,    10,     6,    13,    14,     3,    16,    10,
       7,    10,    10,    10,     8,     9,     8,     9,    10,    10,
      10,    14,    14,    15,    17,    10,    10,    10,    10
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    18,    19,    19,    20,    20,    20,    20,    20,    20,
      21,    21,    21,    21,    21,    22,    22,    23,    23,    23,
      24,    24,    24,    25,    25,    25,    25,    25,    25,    25,
      25,    26,    26,    27,    27,    27,    27,    27,    27,    27,
      27,    28
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     1,     1,     1,     1,     1,     1,
       2,     1,     2,     3,     3,     3,     5,     1,     1,     2,
       1,     2,     2,     3,     5,     5,     5,     2,     4,     2,
       3,     2,     1,     3,     3,     2,     1,     3,     3,     2,
       1,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

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
#ifndef YYINITDEPTH
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
static YYSIZE_T
yystrlen (const char *yystr)
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
static char *
yystpcpy (char *yydest, const char *yysrc)
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

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
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
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
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

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
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
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

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
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 4:
#line 110 "getdate.y" /* yacc.c:1646  */
    { yyHaveTime++; }
#line 1347 "getdate.c" /* yacc.c:1646  */
    break;

  case 5:
#line 111 "getdate.y" /* yacc.c:1646  */
    { yyHaveZone++; }
#line 1353 "getdate.c" /* yacc.c:1646  */
    break;

  case 6:
#line 112 "getdate.y" /* yacc.c:1646  */
    { yyHaveDate++; }
#line 1359 "getdate.c" /* yacc.c:1646  */
    break;

  case 7:
#line 113 "getdate.y" /* yacc.c:1646  */
    { yyHaveDay++; }
#line 1365 "getdate.c" /* yacc.c:1646  */
    break;

  case 8:
#line 114 "getdate.y" /* yacc.c:1646  */
    { yyHaveRel++; }
#line 1371 "getdate.c" /* yacc.c:1646  */
    break;

  case 10:
#line 118 "getdate.y" /* yacc.c:1646  */
    {
		/* "7am" */
		yyHour = (yyvsp[-1].Number);
		if (yyHour == 12)
			yyHour = 0;
		yyMinutes = 0;
		yySeconds = 0;
		if ((yyvsp[0].Number) == tPM)
			yyHour += 12;
	}
#line 1386 "getdate.c" /* yacc.c:1646  */
    break;

  case 11:
#line 128 "getdate.y" /* yacc.c:1646  */
    {
		/* "7:12:18" "19:17" */
	}
#line 1394 "getdate.c" /* yacc.c:1646  */
    break;

  case 12:
#line 131 "getdate.y" /* yacc.c:1646  */
    {
		/* "7:12pm", "12:20:13am" */
		if (yyHour == 12)
			yyHour = 0;
		if ((yyvsp[0].Number) == tPM)
			yyHour += 12;
	}
#line 1406 "getdate.c" /* yacc.c:1646  */
    break;

  case 13:
#line 138 "getdate.y" /* yacc.c:1646  */
    {
		/* "7:14+0700" */
		yyDSTmode = DSToff;
		yyTimezone = - ((yyvsp[0].Number) % 100 + ((yyvsp[0].Number) / 100) * 60);
	}
#line 1416 "getdate.c" /* yacc.c:1646  */
    break;

  case 14:
#line 143 "getdate.y" /* yacc.c:1646  */
    {
		/* "19:14:12-0530" */
		yyDSTmode = DSToff;
		yyTimezone = + ((yyvsp[0].Number) % 100 + ((yyvsp[0].Number) / 100) * 60);
	}
#line 1426 "getdate.c" /* yacc.c:1646  */
    break;

  case 15:
#line 150 "getdate.y" /* yacc.c:1646  */
    {
		yyHour = (yyvsp[-2].Number);
		yyMinutes = (yyvsp[0].Number);
		yySeconds = 0;
	}
#line 1436 "getdate.c" /* yacc.c:1646  */
    break;

  case 16:
#line 155 "getdate.y" /* yacc.c:1646  */
    {
		yyHour = (yyvsp[-4].Number);
		yyMinutes = (yyvsp[-2].Number);
		yySeconds = (yyvsp[0].Number);
	}
#line 1446 "getdate.c" /* yacc.c:1646  */
    break;

  case 17:
#line 162 "getdate.y" /* yacc.c:1646  */
    {
		yyTimezone = (yyvsp[0].Number);
		yyDSTmode = DSToff;
	}
#line 1455 "getdate.c" /* yacc.c:1646  */
    break;

  case 18:
#line 166 "getdate.y" /* yacc.c:1646  */
    {
		yyTimezone = (yyvsp[0].Number);
		yyDSTmode = DSTon;
	}
#line 1464 "getdate.c" /* yacc.c:1646  */
    break;

  case 19:
#line 170 "getdate.y" /* yacc.c:1646  */
    {
		yyTimezone = (yyvsp[-1].Number);
		yyDSTmode = DSTon;
	}
#line 1473 "getdate.c" /* yacc.c:1646  */
    break;

  case 20:
#line 176 "getdate.y" /* yacc.c:1646  */
    {
		yyDayOrdinal = 1;
		yyDayNumber = (yyvsp[0].Number);
	}
#line 1482 "getdate.c" /* yacc.c:1646  */
    break;

  case 21:
#line 180 "getdate.y" /* yacc.c:1646  */
    {
		/* "tue," "wednesday," */
		yyDayOrdinal = 1;
		yyDayNumber = (yyvsp[-1].Number);
	}
#line 1492 "getdate.c" /* yacc.c:1646  */
    break;

  case 22:
#line 185 "getdate.y" /* yacc.c:1646  */
    {
		/* "second tues" "3 wed" */
		yyDayOrdinal = (yyvsp[-1].Number);
		yyDayNumber = (yyvsp[0].Number);
	}
#line 1502 "getdate.c" /* yacc.c:1646  */
    break;

  case 23:
#line 192 "getdate.y" /* yacc.c:1646  */
    {
		/* "1/15" */
		yyMonth = (yyvsp[-2].Number);
		yyDay = (yyvsp[0].Number);
	}
#line 1512 "getdate.c" /* yacc.c:1646  */
    break;

  case 24:
#line 197 "getdate.y" /* yacc.c:1646  */
    {
		if ((yyvsp[-4].Number) >= 13) {
			/* First number is big:  2004/01/29, 99/02/17 */
			yyYear = (yyvsp[-4].Number);
			yyMonth = (yyvsp[-2].Number);
			yyDay = (yyvsp[0].Number);
		} else if (((yyvsp[0].Number) >= 13) || ((yyvsp[-2].Number) >= 13)) {
			/* Last number is big:  01/07/98 */
			/* Middle number is big:  01/29/04 */
			yyMonth = (yyvsp[-4].Number);
			yyDay = (yyvsp[-2].Number);
			yyYear = (yyvsp[0].Number);
		} else {
			/* No significant clues: 02/03/04 */
			yyMonth = (yyvsp[-4].Number);
			yyDay = (yyvsp[-2].Number);
			yyYear = (yyvsp[0].Number);
		}
	}
#line 1536 "getdate.c" /* yacc.c:1646  */
    break;

  case 25:
#line 216 "getdate.y" /* yacc.c:1646  */
    {
		/* ISO 8601 format.  yyyy-mm-dd.  */
		yyYear = (yyvsp[-4].Number);
		yyMonth = (yyvsp[-2].Number);
		yyDay = (yyvsp[0].Number);
	}
#line 1547 "getdate.c" /* yacc.c:1646  */
    break;

  case 26:
#line 222 "getdate.y" /* yacc.c:1646  */
    {
		if ((yyvsp[-4].Number) > 31) {
			/* e.g. 1992-Jun-17 */
			yyYear = (yyvsp[-4].Number);
			yyMonth = (yyvsp[-2].Number);
			yyDay = (yyvsp[0].Number);
		} else {
			/* e.g. 17-JUN-1992.  */
			yyDay = (yyvsp[-4].Number);
			yyMonth = (yyvsp[-2].Number);
			yyYear = (yyvsp[0].Number);
		}
	}
#line 1565 "getdate.c" /* yacc.c:1646  */
    break;

  case 27:
#line 235 "getdate.y" /* yacc.c:1646  */
    {
		/* "May 3" */
		yyMonth = (yyvsp[-1].Number);
		yyDay = (yyvsp[0].Number);
	}
#line 1575 "getdate.c" /* yacc.c:1646  */
    break;

  case 28:
#line 240 "getdate.y" /* yacc.c:1646  */
    {
		/* "June 17, 2001" */
		yyMonth = (yyvsp[-3].Number);
		yyDay = (yyvsp[-2].Number);
		yyYear = (yyvsp[0].Number);
	}
#line 1586 "getdate.c" /* yacc.c:1646  */
    break;

  case 29:
#line 246 "getdate.y" /* yacc.c:1646  */
    {
		/* "12 Sept" */
		yyDay = (yyvsp[-1].Number);
		yyMonth = (yyvsp[0].Number);
	}
#line 1596 "getdate.c" /* yacc.c:1646  */
    break;

  case 30:
#line 251 "getdate.y" /* yacc.c:1646  */
    {
		/* "12 Sept 1997" */
		yyDay = (yyvsp[-2].Number);
		yyMonth = (yyvsp[-1].Number);
		yyYear = (yyvsp[0].Number);
	}
#line 1607 "getdate.c" /* yacc.c:1646  */
    break;

  case 31:
#line 259 "getdate.y" /* yacc.c:1646  */
    {
		yyRelSeconds = -yyRelSeconds;
		yyRelMonth = -yyRelMonth;
	}
#line 1616 "getdate.c" /* yacc.c:1646  */
    break;

  case 33:
#line 266 "getdate.y" /* yacc.c:1646  */
    {
		/* "-3 hours" */
		yyRelSeconds -= (yyvsp[-1].Number) * (yyvsp[0].Number);
	}
#line 1625 "getdate.c" /* yacc.c:1646  */
    break;

  case 34:
#line 270 "getdate.y" /* yacc.c:1646  */
    {
		/* "+1 minute" */
		yyRelSeconds += (yyvsp[-1].Number) * (yyvsp[0].Number);
	}
#line 1634 "getdate.c" /* yacc.c:1646  */
    break;

  case 35:
#line 274 "getdate.y" /* yacc.c:1646  */
    {
		/* "1 day" */
		yyRelSeconds += (yyvsp[-1].Number) * (yyvsp[0].Number);
	}
#line 1643 "getdate.c" /* yacc.c:1646  */
    break;

  case 36:
#line 278 "getdate.y" /* yacc.c:1646  */
    {
		/* "hour" */
		yyRelSeconds += (yyvsp[0].Number);
	}
#line 1652 "getdate.c" /* yacc.c:1646  */
    break;

  case 37:
#line 282 "getdate.y" /* yacc.c:1646  */
    {
		/* "-3 months" */
		yyRelMonth -= (yyvsp[-1].Number) * (yyvsp[0].Number);
	}
#line 1661 "getdate.c" /* yacc.c:1646  */
    break;

  case 38:
#line 286 "getdate.y" /* yacc.c:1646  */
    {
		/* "+5 years" */
		yyRelMonth += (yyvsp[-1].Number) * (yyvsp[0].Number);
	}
#line 1670 "getdate.c" /* yacc.c:1646  */
    break;

  case 39:
#line 290 "getdate.y" /* yacc.c:1646  */
    {
		/* "2 years" */
		yyRelMonth += (yyvsp[-1].Number) * (yyvsp[0].Number);
	}
#line 1679 "getdate.c" /* yacc.c:1646  */
    break;

  case 40:
#line 294 "getdate.y" /* yacc.c:1646  */
    {
		/* "6 months" */
		yyRelMonth += (yyvsp[0].Number);
	}
#line 1688 "getdate.c" /* yacc.c:1646  */
    break;

  case 41:
#line 300 "getdate.y" /* yacc.c:1646  */
    {
		if (yyHaveTime && yyHaveDate && !yyHaveRel)
			yyYear = (yyvsp[0].Number);
		else {
			if((yyvsp[0].Number)>10000) {
				/* "20040301" */
				yyHaveDate++;
				yyDay= ((yyvsp[0].Number))%100;
				yyMonth= ((yyvsp[0].Number)/100)%100;
				yyYear = (yyvsp[0].Number)/10000;
			}
			else {
				/* "513" is same as "5:13" */
				yyHaveTime++;
				if ((yyvsp[0].Number) < 100) {
					yyHour = (yyvsp[0].Number);
					yyMinutes = 0;
				}
				else {
					yyHour = (yyvsp[0].Number) / 100;
					yyMinutes = (yyvsp[0].Number) % 100;
				}
				yySeconds = 0;
			}
		}
	}
#line 1719 "getdate.c" /* yacc.c:1646  */
    break;


#line 1723 "getdate.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
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

  /* Else will try to reuse lookahead token after shifting the error
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

  /* Do not reclaim the symbols of the rule whose action triggered
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
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
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

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


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

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
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
  return yyresult;
}
#line 329 "getdate.y" /* yacc.c:1906  */


static struct TABLE {
	size_t		abbrev;
	const char	*name;
	int		type;
	time_t		value;
} const TimeWords[] = {
	/* am/pm */
	{ 0, "am",		tAMPM,	tAM },
	{ 0, "pm",		tAMPM,	tPM },

	/* Month names. */
	{ 3, "january",		tMONTH,  1 },
	{ 3, "february",	tMONTH,  2 },
	{ 3, "march",		tMONTH,  3 },
	{ 3, "april",		tMONTH,  4 },
	{ 3, "may",		tMONTH,  5 },
	{ 3, "june",		tMONTH,  6 },
	{ 3, "july",		tMONTH,  7 },
	{ 3, "august",		tMONTH,  8 },
	{ 3, "september",	tMONTH,  9 },
	{ 3, "october",		tMONTH, 10 },
	{ 3, "november",	tMONTH, 11 },
	{ 3, "december",	tMONTH, 12 },

	/* Days of the week. */
	{ 2, "sunday",		tDAY, 0 },
	{ 3, "monday",		tDAY, 1 },
	{ 2, "tuesday",		tDAY, 2 },
	{ 3, "wednesday",	tDAY, 3 },
	{ 2, "thursday",	tDAY, 4 },
	{ 2, "friday",		tDAY, 5 },
	{ 2, "saturday",	tDAY, 6 },

	/* Timezones: Offsets are in minutes. */
	{ 0, "gmt",  tZONE,     HOUR( 0) }, /* Greenwich Mean */
	{ 0, "ut",   tZONE,     HOUR( 0) }, /* Universal (Coordinated) */
	{ 0, "utc",  tZONE,     HOUR( 0) },
	{ 0, "wet",  tZONE,     HOUR( 0) }, /* Western European */
	{ 0, "bst",  tDAYZONE,  HOUR( 0) }, /* British Summer */
	{ 0, "wat",  tZONE,     HOUR( 1) }, /* West Africa */
	{ 0, "at",   tZONE,     HOUR( 2) }, /* Azores */
	/* { 0, "bst", tZONE, HOUR( 3) }, */ /* Brazil Standard: Conflict */
	/* { 0, "gst", tZONE, HOUR( 3) }, */ /* Greenland Standard: Conflict*/
	{ 0, "nft",  tZONE,     HOUR(3)+30 }, /* Newfoundland */
	{ 0, "nst",  tZONE,     HOUR(3)+30 }, /* Newfoundland Standard */
	{ 0, "ndt",  tDAYZONE,  HOUR(3)+30 }, /* Newfoundland Daylight */
	{ 0, "ast",  tZONE,     HOUR( 4) }, /* Atlantic Standard */
	{ 0, "adt",  tDAYZONE,  HOUR( 4) }, /* Atlantic Daylight */
	{ 0, "est",  tZONE,     HOUR( 5) }, /* Eastern Standard */
	{ 0, "edt",  tDAYZONE,  HOUR( 5) }, /* Eastern Daylight */
	{ 0, "cst",  tZONE,     HOUR( 6) }, /* Central Standard */
	{ 0, "cdt",  tDAYZONE,  HOUR( 6) }, /* Central Daylight */
	{ 0, "mst",  tZONE,     HOUR( 7) }, /* Mountain Standard */
	{ 0, "mdt",  tDAYZONE,  HOUR( 7) }, /* Mountain Daylight */
	{ 0, "pst",  tZONE,     HOUR( 8) }, /* Pacific Standard */
	{ 0, "pdt",  tDAYZONE,  HOUR( 8) }, /* Pacific Daylight */
	{ 0, "yst",  tZONE,     HOUR( 9) }, /* Yukon Standard */
	{ 0, "ydt",  tDAYZONE,  HOUR( 9) }, /* Yukon Daylight */
	{ 0, "hst",  tZONE,     HOUR(10) }, /* Hawaii Standard */
	{ 0, "hdt",  tDAYZONE,  HOUR(10) }, /* Hawaii Daylight */
	{ 0, "cat",  tZONE,     HOUR(10) }, /* Central Alaska */
	{ 0, "ahst", tZONE,     HOUR(10) }, /* Alaska-Hawaii Standard */
	{ 0, "nt",   tZONE,     HOUR(11) }, /* Nome */
	{ 0, "idlw", tZONE,     HOUR(12) }, /* Intl Date Line West */
	{ 0, "cet",  tZONE,     -HOUR(1) }, /* Central European */
	{ 0, "met",  tZONE,     -HOUR(1) }, /* Middle European */
	{ 0, "mewt", tZONE,     -HOUR(1) }, /* Middle European Winter */
	{ 0, "mest", tDAYZONE,  -HOUR(1) }, /* Middle European Summer */
	{ 0, "swt",  tZONE,     -HOUR(1) }, /* Swedish Winter */
	{ 0, "sst",  tDAYZONE,  -HOUR(1) }, /* Swedish Summer */
	{ 0, "fwt",  tZONE,     -HOUR(1) }, /* French Winter */
	{ 0, "fst",  tDAYZONE,  -HOUR(1) }, /* French Summer */
	{ 0, "eet",  tZONE,     -HOUR(2) }, /* Eastern Eur, USSR Zone 1 */
	{ 0, "bt",   tZONE,     -HOUR(3) }, /* Baghdad, USSR Zone 2 */
	{ 0, "it",   tZONE,     -HOUR(3)-30 },/* Iran */
	{ 0, "zp4",  tZONE,     -HOUR(4) }, /* USSR Zone 3 */
	{ 0, "zp5",  tZONE,     -HOUR(5) }, /* USSR Zone 4 */
	{ 0, "ist",  tZONE,     -HOUR(5)-30 },/* Indian Standard */
	{ 0, "zp6",  tZONE,     -HOUR(6) }, /* USSR Zone 5 */
	/* { 0, "nst",  tZONE, -HOUR(6.5) }, */ /* North Sumatra: Conflict */
	/* { 0, "sst", tZONE, -HOUR(7) }, */ /* So Sumatra, USSR 6: Conflict */
	{ 0, "wast", tZONE,     -HOUR(7) }, /* West Australian Standard */
	{ 0, "wadt", tDAYZONE,  -HOUR(7) }, /* West Australian Daylight */
	{ 0, "jt",   tZONE,     -HOUR(7)-30 },/* Java (3pm in Cronusland!)*/
	{ 0, "cct",  tZONE,     -HOUR(8) }, /* China Coast, USSR Zone 7 */
	{ 0, "jst",  tZONE,     -HOUR(9) }, /* Japan Std, USSR Zone 8 */
	{ 0, "cast", tZONE,     -HOUR(9)-30 },/* Central Australian Std */
	{ 0, "cadt", tDAYZONE,  -HOUR(9)-30 },/* Central Australian Daylt */
	{ 0, "east", tZONE,     -HOUR(10) }, /* Eastern Australian Std */
	{ 0, "eadt", tDAYZONE,  -HOUR(10) }, /* Eastern Australian Daylt */
	{ 0, "gst",  tZONE,     -HOUR(10) }, /* Guam Std, USSR Zone 9 */
	{ 0, "nzt",  tZONE,     -HOUR(12) }, /* New Zealand */
	{ 0, "nzst", tZONE,     -HOUR(12) }, /* New Zealand Standard */
	{ 0, "nzdt", tDAYZONE,  -HOUR(12) }, /* New Zealand Daylight */
	{ 0, "idle", tZONE,     -HOUR(12) }, /* Intl Date Line East */

	{ 0, "dst",  tDST,		0 },

	/* Time units. */
	{ 4, "years",		tMONTH_UNIT,	12 },
	{ 5, "months",		tMONTH_UNIT,	1 },
	{ 9, "fortnights",	tSEC_UNIT,	14 * 24 * 60 * 60 },
	{ 4, "weeks",		tSEC_UNIT,	7 * 24 * 60 * 60 },
	{ 3, "days",		tSEC_UNIT,	1 * 24 * 60 * 60 },
	{ 4, "hours",		tSEC_UNIT,	60 * 60 },
	{ 3, "minutes",		tSEC_UNIT,	60 },
	{ 3, "seconds",		tSEC_UNIT,	1 },

	/* Relative-time words. */
	{ 0, "tomorrow",	tSEC_UNIT,	1 * 24 * 60 * 60 },
	{ 0, "yesterday",	tSEC_UNIT,	-1 * 24 * 60 * 60 },
	{ 0, "today",		tSEC_UNIT,	0 },
	{ 0, "now",		tSEC_UNIT,	0 },
	{ 0, "last",		tUNUMBER,	-1 },
	{ 0, "this",		tSEC_UNIT,	0 },
	{ 0, "next",		tUNUMBER,	2 },
	{ 0, "first",		tUNUMBER,	1 },
	{ 0, "1st",		tUNUMBER,	1 },
/*	{ 0, "second",		tUNUMBER,	2 }, */
	{ 0, "2nd",		tUNUMBER,	2 },
	{ 0, "third",		tUNUMBER,	3 },
	{ 0, "3rd",		tUNUMBER,	3 },
	{ 0, "fourth",		tUNUMBER,	4 },
	{ 0, "4th",		tUNUMBER,	4 },
	{ 0, "fifth",		tUNUMBER,	5 },
	{ 0, "5th",		tUNUMBER,	5 },
	{ 0, "sixth",		tUNUMBER,	6 },
	{ 0, "seventh",		tUNUMBER,	7 },
	{ 0, "eighth",		tUNUMBER,	8 },
	{ 0, "ninth",		tUNUMBER,	9 },
	{ 0, "tenth",		tUNUMBER,	10 },
	{ 0, "eleventh",	tUNUMBER,	11 },
	{ 0, "twelfth",		tUNUMBER,	12 },
	{ 0, "ago",		tAGO,		1 },

	/* Military timezones. */
	{ 0, "a",	tZONE,	HOUR(  1) },
	{ 0, "b",	tZONE,	HOUR(  2) },
	{ 0, "c",	tZONE,	HOUR(  3) },
	{ 0, "d",	tZONE,	HOUR(  4) },
	{ 0, "e",	tZONE,	HOUR(  5) },
	{ 0, "f",	tZONE,	HOUR(  6) },
	{ 0, "g",	tZONE,	HOUR(  7) },
	{ 0, "h",	tZONE,	HOUR(  8) },
	{ 0, "i",	tZONE,	HOUR(  9) },
	{ 0, "k",	tZONE,	HOUR( 10) },
	{ 0, "l",	tZONE,	HOUR( 11) },
	{ 0, "m",	tZONE,	HOUR( 12) },
	{ 0, "n",	tZONE,	HOUR(- 1) },
	{ 0, "o",	tZONE,	HOUR(- 2) },
	{ 0, "p",	tZONE,	HOUR(- 3) },
	{ 0, "q",	tZONE,	HOUR(- 4) },
	{ 0, "r",	tZONE,	HOUR(- 5) },
	{ 0, "s",	tZONE,	HOUR(- 6) },
	{ 0, "t",	tZONE,	HOUR(- 7) },
	{ 0, "u",	tZONE,	HOUR(- 8) },
	{ 0, "v",	tZONE,	HOUR(- 9) },
	{ 0, "w",	tZONE,	HOUR(-10) },
	{ 0, "x",	tZONE,	HOUR(-11) },
	{ 0, "y",	tZONE,	HOUR(-12) },
	{ 0, "z",	tZONE,	HOUR(  0) },

	/* End of table. */
	{ 0, NULL,	0,	0 }
};




/* ARGSUSED */
static int
yyerror(const char *s)
{
	(void)s;
	return 0;
}

static time_t
ToSeconds(time_t Hours, time_t Minutes, time_t Seconds)
{
	if (Minutes < 0 || Minutes > 59 || Seconds < 0 || Seconds > 59)
		return -1;
	if (Hours < 0 || Hours > 23)
		return -1;
	return (Hours * 60L + Minutes) * 60L + Seconds;
}


/* Year is either
 * A number from 0 to 99, which means a year from 1970 to 2069, or
 * The actual year (>=100).  */
static time_t
Convert(time_t Month, time_t Day, time_t Year,
	time_t Hours, time_t Minutes, time_t Seconds, DSTMODE DSTmode)
{
	static int DaysInMonth[12] = {
		31, 0, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
	};
	time_t	tod;
	time_t	Julian;
	int	i;

	if (Year < 69)
		Year += 2000;
	else if (Year < 100)
		Year += 1900;
	DaysInMonth[1] = Year % 4 == 0 && (Year % 100 != 0 || Year % 400 == 0)
	    ? 29 : 28;
	/* Checking for 2038 bogusly assumes that time_t is 32 bits.  But
	   I'm too lazy to try to check for time_t overflow in another way.  */
	if (Year < EPOCH || Year > 2038
	    || Month < 1 || Month > 12
	    /* Lint fluff:  "conversion from long may lose accuracy" */
	    || Day < 1 || Day > DaysInMonth[(int)--Month])
		return -1;

	Julian = Day - 1;
	for (i = 0; i < Month; i++)
		Julian += DaysInMonth[i];
	for (i = EPOCH; i < Year; i++)
		Julian += 365 + (i % 4 == 0);
	Julian *= SECSPERDAY;
	Julian += yyTimezone * 60L;
	if ((tod = ToSeconds(Hours, Minutes, Seconds)) < 0)
		return -1;
	Julian += tod;
	if (DSTmode == DSTon
	    || (DSTmode == DSTmaybe && localtime(&Julian)->tm_isdst))
		Julian -= 60 * 60;
	return Julian;
}


static time_t
DSTcorrect(time_t Start, time_t Future)
{
	time_t	StartDay;
	time_t	FutureDay;

	StartDay = (localtime(&Start)->tm_hour + 1) % 24;
	FutureDay = (localtime(&Future)->tm_hour + 1) % 24;
	return (Future - Start) + (StartDay - FutureDay) * 60L * 60L;
}


static time_t
RelativeDate(time_t Start, time_t DayOrdinal, time_t DayNumber)
{
	struct tm	*tm;
	time_t	now;

	now = Start;
	tm = localtime(&now);
	now += SECSPERDAY * ((DayNumber - tm->tm_wday + 7) % 7);
	now += 7 * SECSPERDAY * (DayOrdinal <= 0 ? DayOrdinal : DayOrdinal - 1);
	return DSTcorrect(Start, now);
}


static time_t
RelativeMonth(time_t Start, time_t RelMonth)
{
	struct tm	*tm;
	time_t	Month;
	time_t	Year;

	if (RelMonth == 0)
		return 0;
	tm = localtime(&Start);
	Month = 12 * (tm->tm_year + 1900) + tm->tm_mon + RelMonth;
	Year = Month / 12;
	Month = Month % 12 + 1;
	return DSTcorrect(Start,
	    Convert(Month, (time_t)tm->tm_mday, Year,
		(time_t)tm->tm_hour, (time_t)tm->tm_min, (time_t)tm->tm_sec,
		DSTmaybe));
}

static int
yylex(void)
{
	char	c;
	char	buff[64];

	for ( ; ; ) {
		while (isspace((unsigned char)*yyInput))
			yyInput++;

		/* Skip parenthesized comments. */
		if (*yyInput == '(') {
			int Count = 0;
			do {
				c = *yyInput++;
				if (c == '\0')
					return c;
				if (c == '(')
					Count++;
				else if (c == ')')
					Count--;
			} while (Count > 0);
			continue;
		}

		/* Try the next token in the word table first. */
		/* This allows us to match "2nd", for example. */
		{
			char *src = yyInput;
			const struct TABLE *tp;
			unsigned i = 0;

			/* Force to lowercase and strip '.' characters. */
			while (*src != '\0'
			    && (isalnum((unsigned char)*src) || *src == '.')
			    && i < sizeof(buff)-1) {
				if (*src != '.') {
					if (isupper((unsigned char)*src))
						buff[i++] = tolower((unsigned char)*src);
					else
						buff[i++] = *src;
				}
				src++;
			}
			buff[i++] = '\0';

			/*
			 * Find the first match.  If the word can be
			 * abbreviated, make sure we match at least
			 * the minimum abbreviation.
			 */
			for (tp = TimeWords; tp->name; tp++) {
				size_t abbrev = tp->abbrev;
				if (abbrev == 0)
					abbrev = strlen(tp->name);
				if (strlen(buff) >= abbrev
				    && strncmp(tp->name, buff, strlen(buff))
				    	== 0) {
					/* Skip over token. */
					yyInput = src;
					/* Return the match. */
					yylval.Number = tp->value;
					return tp->type;
				}
			}
		}

		/*
		 * Not in the word table, maybe it's a number.  Note:
		 * Because '-' and '+' have other special meanings, I
		 * don't deal with signed numbers here.
		 */
		if (isdigit((unsigned char)(c = *yyInput))) {
			for (yylval.Number = 0; isdigit((unsigned char)(c = *yyInput++)); )
				yylval.Number = 10 * yylval.Number + c - '0';
			yyInput--;
			return (tUNUMBER);
		}

		return (*yyInput++);
	}
}

#define TM_YEAR_ORIGIN 1900

/* Yield A - B, measured in seconds.  */
static long
difftm (struct tm *a, struct tm *b)
{
	int ay = a->tm_year + (TM_YEAR_ORIGIN - 1);
	int by = b->tm_year + (TM_YEAR_ORIGIN - 1);
	int days = (
		/* difference in day of year */
		a->tm_yday - b->tm_yday
		/* + intervening leap days */
		+  ((ay >> 2) - (by >> 2))
		-  (ay/100 - by/100)
		+  ((ay/100 >> 2) - (by/100 >> 2))
		/* + difference in years * 365 */
		+  (long)(ay-by) * 365
		);
	return (60*(60*(24*days + (a->tm_hour - b->tm_hour))
	    + (a->tm_min - b->tm_min))
	    + (a->tm_sec - b->tm_sec));
}

time_t
get_date(char *p)
{
	struct tm	*tm;
	struct tm	gmt, *gmt_ptr;
	time_t		Start;
	time_t		tod;
	time_t		nowtime;
	long		tzone;

	memset(&gmt, 0, sizeof(gmt));
	yyInput = p;

	(void)time (&nowtime);

	gmt_ptr = gmtime (&nowtime);
	if (gmt_ptr != NULL) {
		/* Copy, in case localtime and gmtime use the same buffer. */
		gmt = *gmt_ptr;
	}

	if (! (tm = localtime (&nowtime)))
		return -1;

	if (gmt_ptr != NULL)
		tzone = difftm (&gmt, tm) / 60;
	else
		/* This system doesn't understand timezones; fake it. */
		tzone = 0;
	if(tm->tm_isdst)
		tzone += 60;

	yyYear = tm->tm_year + 1900;
	yyMonth = tm->tm_mon + 1;
	yyDay = tm->tm_mday;
	yyTimezone = tzone;
	yyDSTmode = DSTmaybe;
	yyHour = 0;
	yyMinutes = 0;
	yySeconds = 0;
	yyRelSeconds = 0;
	yyRelMonth = 0;
	yyHaveDate = 0;
	yyHaveDay = 0;
	yyHaveRel = 0;
	yyHaveTime = 0;
	yyHaveZone = 0;

	if (yyparse()
	    || yyHaveTime > 1 || yyHaveZone > 1
	    || yyHaveDate > 1 || yyHaveDay > 1)
		return -1;

	if (yyHaveDate || yyHaveTime || yyHaveDay) {
		Start = Convert(yyMonth, yyDay, yyYear,
		    yyHour, yyMinutes, yySeconds, yyDSTmode);
		if (Start < 0)
			return -1;
	} else {
		Start = nowtime;
		if (!yyHaveRel)
			Start -= ((tm->tm_hour * 60L + tm->tm_min) * 60L) + tm->tm_sec;
	}

	Start += yyRelSeconds;
	Start += RelativeMonth(Start, yyRelMonth);

	if (yyHaveDay && !yyHaveDate) {
		tod = RelativeDate(Start, yyDayOrdinal, yyDayNumber);
		Start += tod;
	}

	/* Have to do *something* with a legitimate -1 so it's
	 * distinguishable from the error return value.  (Alternately
	 * could set errno on error.) */
	return Start == -1 ? 0 : Start;
}


#if	defined(TEST)

/* ARGSUSED */
int
main(int argc, char **argv)
{
    time_t	d;

    while (*++argv != NULL) {
	    (void)printf("Input: %s\n", *argv);
	    d = get_date(*argv);
	    if (d == -1)
		    (void)printf("Bad format - couldn't convert.\n");
	    else
		    (void)printf("Output: %s\n", ctime(&d));
    }
    exit(0);
    /* NOTREACHED */
}
#endif	/* defined(TEST) */
