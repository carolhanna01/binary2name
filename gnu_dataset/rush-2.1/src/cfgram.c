/* A Bison parser, made by GNU Bison 2.7.  */

/* Bison implementation for Yacc-like parsers in C
   
      Copyright (C) 1984, 1989-1990, 2000-2012 Free Software Foundation, Inc.
   
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
#define YYBISON_VERSION "2.7"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
/* Line 371 of yacc.c  */
#line 17 "cfgram.y"

#include <rush.h>
#include <cf.h>
static int errors;
int re_flags = REG_EXTENDED;
static struct rush_rule *current_rule;
struct asgn {
	struct asgn *next;
	char *name;
	char *value;
};
static void add_asgn_list(struct asgn *head, enum envar_type type);
static struct transform_node *new_set_node(enum transform_node_type type,
					   char *varname,
					   struct cfloc const *loc);

/* Line 371 of yacc.c  */
#line 85 "cfgram.c"

# ifndef YY_NULL
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULL nullptr
#  else
#   define YY_NULL 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* In a future release of Bison, this section will be replaced
   by #include "y.tab.h".  */
#ifndef YY_YY_CFGRAM_H_INCLUDED
# define YY_YY_CFGRAM_H_INCLUDED
/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     STRING = 258,
     IDENT = 259,
     NUMBER = 260,
     RUSH = 261,
     T_VERSION = 262,
     RULE = 263,
     GLOBAL = 264,
     EOL = 265,
     SET = 266,
     INSERT = 267,
     REMOPT = 268,
     MAP = 269,
     UNSET = 270,
     MATCH = 271,
     FALLTHROUGH = 272,
     INCLUDE = 273,
     LIMITS = 274,
     CLRENV = 275,
     SETENV = 276,
     UNSETENV = 277,
     KEEPENV = 278,
     EVALENV = 279,
     DELETE = 280,
     EXIT = 281,
     ATTRIB = 282,
     GLATTRIB = 283,
     BOGUS = 284,
     OR = 285,
     AND = 286,
     NOT = 287,
     EQ = 288,
     NE = 289,
     LT = 290,
     LE = 291,
     GT = 292,
     GE = 293,
     XF = 294,
     NM = 295,
     IN = 296,
     GROUP = 297
   };
#endif
/* Tokens.  */
#define STRING 258
#define IDENT 259
#define NUMBER 260
#define RUSH 261
#define T_VERSION 262
#define RULE 263
#define GLOBAL 264
#define EOL 265
#define SET 266
#define INSERT 267
#define REMOPT 268
#define MAP 269
#define UNSET 270
#define MATCH 271
#define FALLTHROUGH 272
#define INCLUDE 273
#define LIMITS 274
#define CLRENV 275
#define SETENV 276
#define UNSETENV 277
#define KEEPENV 278
#define EVALENV 279
#define DELETE 280
#define EXIT 281
#define ATTRIB 282
#define GLATTRIB 283
#define BOGUS 284
#define OR 285
#define AND 286
#define NOT 287
#define EQ 288
#define NE 289
#define LT 290
#define LE 291
#define GT 292
#define GE 293
#define XF 294
#define NM 295
#define IN 296
#define GROUP 297



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{
/* Line 387 of yacc.c  */
#line 37 "cfgram.y"

	char *str;
	struct cfnumber num;
	int intval;
	regex_t regex;
	struct rush_rule rule;
	struct test_node *node;
	struct strlist {
		char **argv;
		size_t argc;
	} strlist;
	struct {
		int start;
		int end;
	} range;
	struct asgn *asgn;
	struct {
		struct asgn *head;
		struct asgn *tail;
	} asgn_list;
	struct limits_rec *lrec;
	rule_attrib_setter_t attrib;
	struct global_attrib *global_attrib;
	struct argval *arg;
	struct {
		int argc;
		struct argval *head;
		struct argval *tail;
	} arglist;
	struct { unsigned major, minor; } version;


/* Line 387 of yacc.c  */
#line 245 "cfgram.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;
# define yyltype YYLTYPE /* obsolescent; will be withdrawn */
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;
extern YYLTYPE yylloc;
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

#endif /* !YY_YY_CFGRAM_H_INCLUDED  */

/* Copy the second part of user declarations.  */

/* Line 390 of yacc.c  */
#line 286 "cfgram.c"

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

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(N) (N)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
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
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
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
#   if ! defined malloc && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
	     && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE) + sizeof (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

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
      while (YYID (0))
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  5
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   206

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  49
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  45
/* YYNRULES -- Number of rules.  */
#define YYNRULES  108
/* YYNRULES -- Number of states.  */
#define YYNSTATES  194

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   297

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      44,    45,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    46,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    47,     2,    48,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,    43,     2,     2,     2,
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
      35,    36,    37,    38,    39,    40,    41,    42
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     6,    10,    12,    16,    17,    19,    21,
      24,    25,    27,    29,    32,    35,    38,    41,    43,    46,
      50,    52,    55,    57,    59,    63,    64,    66,    68,    71,
      74,    77,    80,    83,    86,    89,    92,    95,    98,   101,
     103,   106,   108,   112,   116,   118,   121,   125,   129,   133,
     137,   141,   145,   149,   153,   157,   161,   165,   169,   172,
     175,   177,   179,   181,   183,   185,   187,   192,   197,   202,
     209,   216,   221,   226,   233,   236,   239,   248,   257,   258,
     260,   264,   266,   268,   272,   276,   277,   279,   282,   284,
     287,   291,   294,   296,   299,   303,   304,   306,   308,   313,
     316,   319,   322,   324,   327,   329,   333,   336,   337
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      50,     0,    -1,    53,    51,    -1,    52,    53,    55,    -1,
      29,    -1,     6,     7,    10,    -1,    -1,    54,    -1,    10,
      -1,    54,    10,    -1,    -1,    56,    -1,    57,    -1,    56,
      57,    -1,    63,    65,    -1,    58,    59,    -1,     9,    54,
      -1,    60,    -1,    59,    60,    -1,    28,    61,    54,    -1,
      62,    -1,    61,    62,    -1,    71,    -1,     5,    -1,     8,
      64,    54,    -1,    -1,    72,    -1,    66,    -1,    65,    66,
      -1,    67,    54,    -1,    74,    54,    -1,    75,    54,    -1,
      81,    54,    -1,    84,    54,    -1,    88,    54,    -1,    79,
      54,    -1,    91,    54,    -1,    86,    54,    -1,    83,    53,
      -1,     1,    -1,    16,    68,    -1,    69,    -1,    68,    31,
      69,    -1,    68,    30,    69,    -1,    70,    -1,    32,    69,
      -1,    44,    68,    45,    -1,    72,    43,    73,    -1,    72,
      40,    73,    -1,    72,    33,    71,    -1,    72,    34,    71,
      -1,    72,    33,     5,    -1,    72,    34,     5,    -1,    72,
      35,     5,    -1,    72,    36,     5,    -1,    72,    37,     5,
      -1,    72,    38,     5,    -1,    72,    41,    92,    -1,    42,
      72,    -1,    42,    92,    -1,     4,    -1,     3,    -1,     4,
      -1,     3,    -1,     5,    -1,    72,    -1,    11,    77,    46,
      78,    -1,    12,    77,    46,    78,    -1,    11,    77,    39,
      78,    -1,    11,    77,    46,    72,    43,    78,    -1,    12,
      77,    46,    72,    43,    78,    -1,    11,     4,    46,    78,
      -1,    11,     4,    39,    78,    -1,    11,     4,    46,    72,
      43,    78,    -1,    15,     4,    -1,    15,    77,    -1,    14,
      77,    78,    78,    78,     5,     5,    76,    -1,    14,     4,
      78,    78,    78,     5,     5,    76,    -1,    -1,    72,    -1,
      47,     5,    48,    -1,    72,    -1,    17,    -1,    26,    80,
       3,    -1,    26,    80,     4,    -1,    -1,     5,    -1,    25,
      82,    -1,     5,    -1,     5,     5,    -1,    18,    72,    10,
      -1,    19,    85,    -1,     4,    -1,    85,     4,    -1,    13,
      72,    87,    -1,    -1,    72,    -1,    20,    -1,    21,     4,
      46,    72,    -1,    24,    72,    -1,    22,    89,    -1,    23,
      89,    -1,    90,    -1,    89,    90,    -1,    71,    -1,     4,
      46,    78,    -1,    27,    72,    -1,    -1,    44,    93,    61,
      45,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   130,   130,   133,   138,   145,   156,   157,   160,   161,
     164,   165,   168,   169,   172,   173,   176,   179,   180,   183,
     193,   198,   206,   214,   225,   235,   238,   241,   242,   245,
     246,   247,   248,   249,   250,   251,   252,   253,   254,   255,
     268,   280,   281,   287,   295,   296,   301,   307,   314,   324,
     331,   338,   345,   352,   359,   366,   373,   380,   387,   394,
     401,   402,   405,   406,   407,   413,   428,   439,   450,   462,
     474,   486,   495,   506,   517,   525,   542,   560,   579,   582,
     585,   592,   598,   602,   607,   620,   623,   633,   650,   655,
     670,   681,   687,   707,   732,   770,   773,   779,   783,   792,
     800,   804,   810,   814,   821,   828,   840,   847,   847
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "\"string\"", "\"identifier\"",
  "\"number\"", "\"rush\"", "T_VERSION", "\"rule\"", "\"global\"",
  "\"end of line\"", "\"set\"", "\"insert\"", "\"remopt\"", "\"map\"",
  "\"unset\"", "\"match\"", "\"fallthrough\"", "\"include\"", "\"limits\"",
  "\"clrenv\"", "\"setenv\"", "\"unsetenv\"", "\"keepenv\"", "\"evalenv\"",
  "\"delete\"", "\"exit\"", "\"rule attribute\"", "\"global attribute\"",
  "\"erroneous token\"", "\"||\"", "\"&&\"", "\"!\"", "\"==\"", "\"!=\"",
  "\"<\"", "\"<=\"", "\">\"", "\">=\"", "\"=~\"", "\"!~\"", "\"in\"",
  "\"group\"", "'~'", "'('", "')'", "'='", "'['", "']'", "$accept",
  "rcfile", "select", "preface", "skipeol", "eol", "content", "rulelist",
  "rule", "globhdr", "globbody", "glob_stmt", "arglist", "arg", "rulehdr",
  "ruleid", "rulebody", "stmt", "match_stmt", "compound_cond",
  "simple_cond", "expr", "literal", "string", "regex", "set_stmt",
  "map_stmt", "defval", "index", "value", "flowctl_stmt", "fdescr",
  "delete_stmt", "range", "include_stmt", "limits_stmt", "resource_limits",
  "remopt_stmt", "optstring", "environ_stmt", "asgn_list", "asgn",
  "attrib_stmt", "strlist", "$@1", YY_NULL
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
     295,   296,   297,   126,    40,    41,    61,    91,    93
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    49,    50,    51,    51,    52,    53,    53,    54,    54,
      55,    55,    56,    56,    57,    57,    58,    59,    59,    60,
      61,    61,    62,    62,    63,    64,    64,    65,    65,    66,
      66,    66,    66,    66,    66,    66,    66,    66,    66,    66,
      67,    68,    68,    68,    69,    69,    69,    70,    70,    70,
      70,    70,    70,    70,    70,    70,    70,    70,    70,    70,
      71,    71,    72,    72,    72,    73,    74,    74,    74,    74,
      74,    74,    74,    74,    74,    74,    75,    75,    76,    76,
      77,    78,    79,    79,    79,    80,    80,    81,    82,    82,
      83,    84,    85,    85,    86,    87,    87,    88,    88,    88,
      88,    88,    89,    89,    90,    90,    91,    93,    92
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     2,     3,     1,     3,     0,     1,     1,     2,
       0,     1,     1,     2,     2,     2,     2,     1,     2,     3,
       1,     2,     1,     1,     3,     0,     1,     1,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     1,
       2,     1,     3,     3,     1,     2,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     2,     2,
       1,     1,     1,     1,     1,     1,     4,     4,     4,     6,
       6,     4,     4,     6,     2,     2,     8,     8,     0,     1,
       3,     1,     1,     3,     3,     0,     1,     2,     1,     2,
       3,     2,     1,     2,     3,     0,     1,     1,     4,     2,
       2,     2,     1,     2,     1,     3,     2,     0,     4
};

/* YYDEFACT[STATE-NAME] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       6,     8,     0,     0,     7,     1,     0,     4,     2,     6,
       9,     0,    10,     5,    25,     0,     3,    11,    12,     0,
       0,    63,    62,    64,     0,    26,    16,    13,     0,    15,
      17,    39,     0,     0,     0,     0,     0,     0,    82,     0,
       0,    97,     0,     0,     0,     0,     0,    85,     0,     0,
      27,     0,     0,     0,     0,     0,     6,     0,     0,     0,
       0,    24,    61,    60,    23,     0,    20,    22,    18,     0,
       0,     0,     0,    95,     0,     0,    74,    75,     0,     0,
       0,    40,    41,    44,     0,     0,    92,    91,     0,    60,
     104,   100,   102,   101,    99,    88,    87,    86,     0,   106,
      28,    29,    30,    31,    35,    32,    38,    33,    37,    34,
      36,    19,    21,     0,     0,     0,     0,     0,     0,    96,
      94,    81,     0,     0,    45,   107,    58,    59,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      90,    93,     0,     0,   103,    89,    83,    84,    72,    81,
      71,    80,    68,    81,    66,    81,    67,     0,     0,     0,
      46,    43,    42,    51,    49,    52,    50,    53,    54,    55,
      56,    65,    48,    57,    47,    98,   105,     0,     0,     0,
       0,     0,     0,    73,    69,    70,     0,     0,   108,    78,
      78,    79,    77,    76
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,     8,     9,     3,     4,    16,    17,    18,    19,
      29,    30,    65,    66,    20,    24,    49,    50,    51,    81,
      82,    83,    67,   121,   172,    52,    53,   192,    71,   122,
      54,    98,    55,    96,    56,    57,    87,    58,   120,    59,
      91,    92,    60,   127,   159
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -73
static const yytype_int16 yypact[] =
{
       1,   -73,    27,    10,    22,   -73,    17,   -73,   -73,     1,
     -73,    30,    64,   -73,   105,     1,   -73,    64,   -73,    15,
     166,   -73,   -73,   -73,     1,   -73,    22,   -73,   108,    15,
     -73,   -73,     6,    21,   105,     8,     9,    25,   -73,   105,
      58,   -73,    75,    11,    11,   105,    79,    82,   105,   121,
     -73,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,    22,   -73,   -73,   -73,    86,   -73,   -73,   -73,   -13,
      89,    24,    53,   105,   105,   105,   -73,   -73,    25,    14,
      25,    71,   -73,   -73,   116,   104,   -73,   113,    73,    74,
     -73,    11,   -73,    11,   -73,   119,   -73,   -73,   123,   -73,
     -73,    22,    22,    22,    22,    22,   -73,    22,    22,    22,
      22,    22,   -73,   105,   105,    83,   105,   105,   105,   -73,
     -73,   -73,   105,   105,   -73,   -73,   -73,   -73,    -9,    25,
      25,   157,   160,   150,   153,   161,   163,   105,   125,   105,
     -73,   -73,   105,   105,   -73,   -73,   -73,   -73,   -73,   127,
     -73,   -73,   -73,   128,   -73,   129,   -73,   105,   105,   108,
     -73,   -73,   -73,   -73,   -73,   -73,   -73,   -73,   -73,   -73,
     -73,   -73,   -73,   -73,   -73,   -73,   -73,   105,   105,   105,
     168,   169,     3,   -73,   -73,   -73,   189,   190,   -73,   105,
     105,   -73,   -73,   -73
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -73,   -73,   -73,   -73,    -7,    23,   -73,   -73,   179,   -73,
     -73,   170,    38,   -64,   -73,   -73,   -73,   149,   -73,   120,
     -69,   -73,   -39,   -14,    63,   -73,   -73,    13,    62,   -72,
     -73,   -73,   -73,   -73,   -73,   -73,   -73,   -73,   -73,   -73,
     162,   -56,   -73,    66,   -73
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -15
static const yytype_int16 yytable[] =
{
      25,   112,    12,   123,    90,    90,    62,    63,    64,   124,
      69,     1,    74,    76,    62,    89,     6,    21,    22,    23,
      73,   129,   130,    84,    11,    85,   113,     5,    21,    22,
      23,    94,    10,   114,    99,   144,   160,   144,    26,     7,
      13,   148,   150,    28,   152,   154,   156,    61,   188,   106,
     157,   158,    90,    70,    90,    70,    70,    78,   125,   119,
     161,   162,    86,   116,    84,   126,    84,    79,    70,    80,
     117,   176,    14,    15,   101,   102,   103,   104,   105,    88,
     107,   108,   109,   110,    95,   180,   181,    97,   111,    62,
      63,    64,   164,   166,   115,    72,     1,    75,    77,   118,
     149,   129,   130,   153,   155,   183,   184,   185,    21,    22,
      23,    62,    63,    64,   140,    84,    84,   141,   112,   142,
     143,   -14,    31,   171,   145,   171,   146,   147,   175,   -14,
     -14,   151,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,   131,
     132,   133,   134,   135,   136,   167,   137,   138,   168,   139,
      62,    63,   163,    62,    63,   165,   169,    31,   170,   125,
     177,   178,   179,   186,   187,   191,   191,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,   189,   190,    27,   182,   100,    68,
     128,     0,   174,   193,   173,     0,    93
};

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-73)))

#define yytable_value_is_error(Yytable_value) \
  YYID (0)

static const yytype_int16 yycheck[] =
{
      14,    65,     9,    75,    43,    44,     3,     4,     5,    78,
       4,    10,     4,     4,     3,     4,     6,     3,     4,     5,
      34,    30,    31,    37,     7,    39,    39,     0,     3,     4,
       5,    45,    10,    46,    48,    91,    45,    93,    15,    29,
      10,   113,   114,    28,   116,   117,   118,    24,    45,    56,
     122,   123,    91,    47,    93,    47,    47,    32,    44,    73,
     129,   130,     4,    39,    78,    79,    80,    42,    47,    44,
      46,   143,     8,     9,    51,    52,    53,    54,    55,     4,
      57,    58,    59,    60,     5,   157,   158,     5,    65,     3,
       4,     5,   131,   132,     5,    33,    10,    35,    36,    46,
     114,    30,    31,   117,   118,   177,   178,   179,     3,     4,
       5,     3,     4,     5,    10,   129,   130,     4,   182,    46,
      46,     0,     1,   137,     5,   139,     3,     4,   142,     8,
       9,    48,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    33,
      34,    35,    36,    37,    38,     5,    40,    41,     5,    43,
       3,     4,     5,     3,     4,     5,     5,     1,     5,    44,
      43,    43,    43,     5,     5,   189,   190,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,     5,     5,    17,   159,    49,    29,
      80,    -1,   139,   190,   138,    -1,    44
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    10,    50,    53,    54,     0,     6,    29,    51,    52,
      10,     7,    53,    10,     8,     9,    55,    56,    57,    58,
      63,     3,     4,     5,    64,    72,    54,    57,    28,    59,
      60,     1,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    65,
      66,    67,    74,    75,    79,    81,    83,    84,    86,    88,
      91,    54,     3,     4,     5,    61,    62,    71,    60,     4,
      47,    77,    77,    72,     4,    77,     4,    77,    32,    42,
      44,    68,    69,    70,    72,    72,     4,    85,     4,     4,
      71,    89,    90,    89,    72,     5,    82,     5,    80,    72,
      66,    54,    54,    54,    54,    54,    53,    54,    54,    54,
      54,    54,    62,    39,    46,     5,    39,    46,    46,    72,
      87,    72,    78,    78,    69,    44,    72,    92,    68,    30,
      31,    33,    34,    35,    36,    37,    38,    40,    41,    43,
      10,     4,    46,    46,    90,     5,     3,     4,    78,    72,
      78,    48,    78,    72,    78,    72,    78,    78,    78,    93,
      45,    69,    69,     5,    71,     5,    71,     5,     5,     5,
       5,    72,    73,    92,    73,    72,    78,    43,    43,    43,
      78,    78,    61,    78,    78,    78,     5,     5,    45,     5,
       5,    72,    76,    76
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
   Once GCC version 2 has supplanted version 1, this can go.  However,
   YYFAIL appears to be in use.  Nevertheless, it is formally deprecated
   in Bison 2.4.2's NEWS entry, where a plan to phase it out is
   discussed.  */

#define YYFAIL		goto yyerrlab
#if defined YYFAIL
  /* This is here to suppress warnings from the GCC cpp's
     -Wunused-macros.  Normally we don't worry about that warning, but
     some users do, and we want to make it easy for users to remove
     YYFAIL uses, which will produce warnings from Bison 2.5.  */
#endif

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
      YYERROR;							\
    }								\
while (YYID (0))

/* Error token number */
#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (YYID (N))                                                     \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (YYID (0))
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

__attribute__((__unused__))
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static unsigned
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
#else
static unsigned
yy_location_print_ (yyo, yylocp)
    FILE *yyo;
    YYLTYPE const * const yylocp;
#endif
{
  unsigned res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += fprintf (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += fprintf (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += fprintf (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += fprintf (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += fprintf (yyo, "-%d", end_col);
    }
  return res;
 }

#  define YY_LOCATION_PRINT(File, Loc)          \
  yy_location_print_ (File, &(Loc))

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
		  Type, Value, Location); \
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
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    YYLTYPE const * const yylocationp;
#endif
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
  YYUSE (yylocationp);
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
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep, yylocationp)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    YYLTYPE const * const yylocationp;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  YY_LOCATION_PRINT (yyoutput, *yylocationp);
  YYFPRINTF (yyoutput, ": ");
  yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
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
yy_reduce_print (YYSTYPE *yyvsp, YYLTYPE *yylsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yylsp, yyrule)
    YYSTYPE *yyvsp;
    YYLTYPE *yylsp;
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
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       , &(yylsp[(yyi + 1) - (yynrhs)])		       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, yylsp, Rule); \
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
  YYSIZE_T yysize0 = yytnamerr (YY_NULL, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULL;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - Assume YYFAIL is not used.  It's too flawed to consider.  See
       <http://lists.gnu.org/archive/html/bison-patches/2009-12/msg00024.html>
       for details.  YYERROR is fine as it does not invoke this
       function.
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
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULL, yytname[yyx]);
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

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, YYLTYPE *yylocationp)
#else
static void
yydestruct (yymsg, yytype, yyvaluep, yylocationp)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
    YYLTYPE *yylocationp;
#endif
{
  YYUSE (yyvaluep);
  YYUSE (yylocationp);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
        break;
    }
}




/* The lookahead symbol.  */
int yychar;


#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval YY_INITIAL_VALUE(yyval_default);

/* Location data for the lookahead symbol.  */
YYLTYPE yylloc
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;


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
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.
       `yyls': related to locations.

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

    /* The location stack.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls;
    YYLTYPE *yylsp;

    /* The locations where the error started and ended.  */
    YYLTYPE yyerror_range[3];

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yylsp = yyls = yylsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  yylsp[0] = yylloc;
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
	YYLTYPE *yyls1 = yyls;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yyls1, yysize * sizeof (*yylsp),
		    &yystacksize);

	yyls = yyls1;
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
	YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

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
  *++yylsp = yylloc;
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

  /* Default location.  */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 3:
/* Line 1792 of yacc.c  */
#line 134 "cfgram.y"
    {
		     if (errors)
			     YYERROR;
	     }
    break;

  case 4:
/* Line 1792 of yacc.c  */
#line 139 "cfgram.y"
    {
		     if (parse_old_rc())
			     YYERROR;
	     }
    break;

  case 5:
/* Line 1792 of yacc.c  */
#line 146 "cfgram.y"
    {
		     if ((yyvsp[(2) - (3)].version).major == 2 && (yyvsp[(2) - (3)].version).minor == 0) {
			     cflex_normal();
		     } else {
			     cferror(&(yylsp[(2) - (3)]), _("unsupported configuration file version"));
			     YYERROR;
		     }
	     }
    break;

  case 19:
/* Line 1792 of yacc.c  */
#line 184 "cfgram.y"
    {
		     struct cfloc loc;
		     loc.beg = (yylsp[(1) - (3)]).beg;
		     loc.end = (yylsp[(2) - (3)]).end;
		     global_attrib_set((yyvsp[(1) - (3)].global_attrib), (yyvsp[(2) - (3)].arglist).argc, (yyvsp[(2) - (3)].arglist).head, &loc);
		     arglist_free((yyvsp[(2) - (3)].arglist).head);
	     }
    break;

  case 20:
/* Line 1792 of yacc.c  */
#line 194 "cfgram.y"
    {
		     (yyval.arglist).head = (yyval.arglist).tail = (yyvsp[(1) - (1)].arg);
		     (yyval.arglist).argc = 1;
	     }
    break;

  case 21:
/* Line 1792 of yacc.c  */
#line 199 "cfgram.y"
    {
		     LIST_APPEND((yyvsp[(2) - (2)].arg), (yyvsp[(1) - (2)].arglist).head, (yyvsp[(1) - (2)].arglist).tail);
		     (yyvsp[(1) - (2)].arglist).argc++;
		     (yyval.arglist) = (yyvsp[(1) - (2)].arglist);
	     }
    break;

  case 22:
/* Line 1792 of yacc.c  */
#line 207 "cfgram.y"
    {
		     (yyval.arg) = xcalloc(1, sizeof(*(yyval.arg)));
		     (yyval.arg)->next = NULL;
		     (yyval.arg)->loc = (yylsp[(1) - (1)]);
		     (yyval.arg)->isnum = 0;
		     (yyval.arg)->strval = (yyvsp[(1) - (1)].str);
	     }
    break;

  case 23:
/* Line 1792 of yacc.c  */
#line 215 "cfgram.y"
    {
		     (yyval.arg) = xcalloc(1, sizeof(*(yyval.arg)));
		     (yyval.arg)->next = NULL;
		     (yyval.arg)->loc = (yylsp[(1) - (1)]);
		     (yyval.arg)->isnum = 1;
		     (yyval.arg)->strval = (yyvsp[(1) - (1)].num).strval;
		     (yyval.arg)->intval = (yyvsp[(1) - (1)].num).intval;
	     }
    break;

  case 24:
/* Line 1792 of yacc.c  */
#line 226 "cfgram.y"
    {
		     current_rule = new_rush_rule((yyvsp[(2) - (3)].str));
		     current_rule->file = (yylsp[(1) - (3)]).beg.filename;
		     current_rule->line = (yylsp[(1) - (3)]).beg.line;
		     free((yyvsp[(2) - (3)].str));
	     }
    break;

  case 25:
/* Line 1792 of yacc.c  */
#line 235 "cfgram.y"
    {
		     (yyval.str) = NULL;
	     }
    break;

  case 39:
/* Line 1792 of yacc.c  */
#line 256 "cfgram.y"
    {
		     skiptoeol();
		     restorenormal();
		     yyerrok;
		     yyclearin;
		     errors = 1;
	     }
    break;

  case 40:
/* Line 1792 of yacc.c  */
#line 269 "cfgram.y"
    {
		     if (current_rule->test_node) {
			     struct test_node *np = new_test_node(test_and);
			     np->v.arg[0] = current_rule->test_node;
			     np->v.arg[1] = (yyvsp[(2) - (2)].node);
			     current_rule->test_node = np;
		     } else
			     current_rule->test_node = (yyvsp[(2) - (2)].node);
	     }
    break;

  case 42:
/* Line 1792 of yacc.c  */
#line 282 "cfgram.y"
    {
		     (yyval.node) = new_test_node(test_and);
		     (yyval.node)->v.arg[0] = (yyvsp[(1) - (3)].node);
		     (yyval.node)->v.arg[1] = (yyvsp[(3) - (3)].node);
	     }
    break;

  case 43:
/* Line 1792 of yacc.c  */
#line 288 "cfgram.y"
    {
		     (yyval.node) = new_test_node(test_or);
		     (yyval.node)->v.arg[0] = (yyvsp[(1) - (3)].node);
		     (yyval.node)->v.arg[1] = (yyvsp[(3) - (3)].node);
	     }
    break;

  case 45:
/* Line 1792 of yacc.c  */
#line 297 "cfgram.y"
    {
		     (yyval.node) = new_test_node(test_not);
		     (yyval.node)->v.arg[0] = (yyvsp[(2) - (2)].node);
	     }
    break;

  case 46:
/* Line 1792 of yacc.c  */
#line 302 "cfgram.y"
    {
		     (yyval.node) = (yyvsp[(2) - (3)].node);
	     }
    break;

  case 47:
/* Line 1792 of yacc.c  */
#line 308 "cfgram.y"
    {
		     (yyval.node) = new_test_node(test_cmps);
		     (yyval.node)->v.cmp.op = cmp_match;
		     (yyval.node)->v.cmp.larg = (yyvsp[(1) - (3)].str);
		     (yyval.node)->v.cmp.rarg.rx = (yyvsp[(3) - (3)].regex);
	     }
    break;

  case 48:
/* Line 1792 of yacc.c  */
#line 315 "cfgram.y"
    {
		     struct test_node *np = new_test_node(test_cmps);
		     np->v.cmp.op = cmp_match;
		     np->v.cmp.larg = (yyvsp[(1) - (3)].str);
		     np->v.cmp.rarg.rx = (yyvsp[(3) - (3)].regex);

		     (yyval.node) = new_test_node(test_not);
		     (yyval.node)->v.arg[0] = np;
	     }
    break;

  case 49:
/* Line 1792 of yacc.c  */
#line 325 "cfgram.y"
    {
		     (yyval.node) = new_test_node(test_cmps);
		     (yyval.node)->v.cmp.op = cmp_eq;
		     (yyval.node)->v.cmp.larg = (yyvsp[(1) - (3)].str);
		     (yyval.node)->v.cmp.rarg.str = (yyvsp[(3) - (3)].str);
	     }
    break;

  case 50:
/* Line 1792 of yacc.c  */
#line 332 "cfgram.y"
    {
		     (yyval.node) = new_test_node(test_cmps);
		     (yyval.node)->v.cmp.op = cmp_ne;
		     (yyval.node)->v.cmp.larg = (yyvsp[(1) - (3)].str);
		     (yyval.node)->v.cmp.rarg.str = (yyvsp[(3) - (3)].str);
	     }
    break;

  case 51:
/* Line 1792 of yacc.c  */
#line 339 "cfgram.y"
    {
		     (yyval.node) = new_test_node(test_cmpn);
		     (yyval.node)->v.cmp.op = cmp_eq;
		     (yyval.node)->v.cmp.larg = (yyvsp[(1) - (3)].str);
		     (yyval.node)->v.cmp.rarg.num = (yyvsp[(3) - (3)].num).intval;
	     }
    break;

  case 52:
/* Line 1792 of yacc.c  */
#line 346 "cfgram.y"
    {
		     (yyval.node) = new_test_node(test_cmpn);
		     (yyval.node)->v.cmp.op = cmp_ne;
		     (yyval.node)->v.cmp.larg = (yyvsp[(1) - (3)].str);
		     (yyval.node)->v.cmp.rarg.num = (yyvsp[(3) - (3)].num).intval;
	     }
    break;

  case 53:
/* Line 1792 of yacc.c  */
#line 353 "cfgram.y"
    {
		     (yyval.node) = new_test_node(test_cmpn);
		     (yyval.node)->v.cmp.op = cmp_lt;
		     (yyval.node)->v.cmp.larg = (yyvsp[(1) - (3)].str);
		     (yyval.node)->v.cmp.rarg.num = (yyvsp[(3) - (3)].num).intval;
	     }
    break;

  case 54:
/* Line 1792 of yacc.c  */
#line 360 "cfgram.y"
    {
		     (yyval.node) = new_test_node(test_cmpn);
		     (yyval.node)->v.cmp.op = cmp_le;
		     (yyval.node)->v.cmp.larg = (yyvsp[(1) - (3)].str);
		     (yyval.node)->v.cmp.rarg.num = (yyvsp[(3) - (3)].num).intval;
	     }
    break;

  case 55:
/* Line 1792 of yacc.c  */
#line 367 "cfgram.y"
    {
		     (yyval.node) = new_test_node(test_cmpn);
		     (yyval.node)->v.cmp.op = cmp_gt;
		     (yyval.node)->v.cmp.larg = (yyvsp[(1) - (3)].str);
		     (yyval.node)->v.cmp.rarg.num = (yyvsp[(3) - (3)].num).intval;
	     }
    break;

  case 56:
/* Line 1792 of yacc.c  */
#line 374 "cfgram.y"
    {
		     (yyval.node) = new_test_node(test_cmpn);
		     (yyval.node)->v.cmp.op = cmp_ge;
		     (yyval.node)->v.cmp.larg = (yyvsp[(1) - (3)].str);
		     (yyval.node)->v.cmp.rarg.num = (yyvsp[(3) - (3)].num).intval;
	     }
    break;

  case 57:
/* Line 1792 of yacc.c  */
#line 381 "cfgram.y"
    {
		     (yyval.node) = new_test_node(test_in);
		     (yyval.node)->v.cmp.op = cmp_in;
		     (yyval.node)->v.cmp.larg = (yyvsp[(1) - (3)].str);
		     (yyval.node)->v.cmp.rarg.strv = (yyvsp[(3) - (3)].strlist).argv;
	     }
    break;

  case 58:
/* Line 1792 of yacc.c  */
#line 388 "cfgram.y"
    {
		     (yyval.node) = new_test_node(test_group);
		     (yyval.node)->v.groups = xcalloc(2, sizeof((yyval.node)->v.groups[0]));
		     (yyval.node)->v.groups[0] = (yyvsp[(2) - (2)].str);
		     (yyval.node)->v.groups[1] = NULL;
	     }
    break;

  case 59:
/* Line 1792 of yacc.c  */
#line 395 "cfgram.y"
    {
		     (yyval.node) = new_test_node(test_group);
		     (yyval.node)->v.groups = (yyvsp[(2) - (2)].strlist).argv;
	     }
    break;

  case 64:
/* Line 1792 of yacc.c  */
#line 408 "cfgram.y"
    {
		     (yyval.str) = (yyvsp[(1) - (1)].num).strval;
	     }
    break;

  case 65:
/* Line 1792 of yacc.c  */
#line 414 "cfgram.y"
    {
		     int rc = regcomp(&(yyval.regex), (yyvsp[(1) - (1)].str), re_flags);
		     if (rc) {
			     char errbuf[512];
			     regerror(rc, &(yyval.regex), errbuf, sizeof(errbuf));
			     cferror(&(yylsp[(1) - (1)]), _("invalid regexp: %s"), (yyvsp[(1) - (1)].str));
			     YYERROR;
		     }
	     }
    break;

  case 66:
/* Line 1792 of yacc.c  */
#line 429 "cfgram.y"
    {
		     struct transform_node *node;

		     node = new_transform_node(current_rule, transform_set);
		     node->target.type = target_arg;
		     node->target.v.arg.ins = 0;
		     node->target.v.arg.idx = (yyvsp[(2) - (4)].intval);
		     node->v.xf.pattern = (yyvsp[(4) - (4)].str);
		     node->v.xf.trans = NULL;
	     }
    break;

  case 67:
/* Line 1792 of yacc.c  */
#line 440 "cfgram.y"
    {
		     struct transform_node *node;

		     node = new_transform_node(current_rule, transform_set);
		     node->target.type = target_arg;
		     node->target.v.arg.ins = 1;
		     node->target.v.arg.idx = (yyvsp[(2) - (4)].intval);
		     node->v.xf.pattern = (yyvsp[(4) - (4)].str);
		     node->v.xf.trans = NULL;
	     }
    break;

  case 68:
/* Line 1792 of yacc.c  */
#line 451 "cfgram.y"
    {
		     struct transform_node *node;

		     node = new_transform_node(current_rule, transform_set);
		     node->target.type = target_arg;
		     node->target.v.arg.ins = 0;
		     node->target.v.arg.idx = (yyvsp[(2) - (4)].intval);
		     node->v.xf.pattern = NULL;
		     node->v.xf.trans = compile_transform_expr((yyvsp[(4) - (4)].str), re_flags,
							       &(yylsp[(4) - (4)]));
	     }
    break;

  case 69:
/* Line 1792 of yacc.c  */
#line 463 "cfgram.y"
    {
		     struct transform_node *node;

		     node = new_transform_node(current_rule, transform_set);
		     node->target.type = target_arg;
		     node->target.v.arg.ins = 0;
		     node->target.v.arg.idx = (yyvsp[(2) - (6)].intval);
		     node->v.xf.pattern = (yyvsp[(4) - (6)].str);
		     node->v.xf.trans = compile_transform_expr((yyvsp[(6) - (6)].str), re_flags,
							       &(yylsp[(6) - (6)]));
	     }
    break;

  case 70:
/* Line 1792 of yacc.c  */
#line 475 "cfgram.y"
    {
		     struct transform_node *node;

		     node = new_transform_node(current_rule, transform_set);
		     node->target.type = target_arg;
		     node->target.v.arg.ins = 1;
		     node->target.v.arg.idx = (yyvsp[(2) - (6)].intval);
		     node->v.xf.pattern = (yyvsp[(4) - (6)].str);
		     node->v.xf.trans = compile_transform_expr((yyvsp[(6) - (6)].str), re_flags,
							       &(yylsp[(6) - (6)]));
	     }
    break;

  case 71:
/* Line 1792 of yacc.c  */
#line 487 "cfgram.y"
    {
		     struct transform_node *node =
			     new_set_node(transform_set, (yyvsp[(2) - (4)].str), &(yylsp[(2) - (4)]));
		     if (node) {
			     node->v.xf.pattern = (yyvsp[(4) - (4)].str);
			     node->v.xf.trans = NULL;
		     }
	     }
    break;

  case 72:
/* Line 1792 of yacc.c  */
#line 496 "cfgram.y"
    {
		     struct transform_node *node =
			     new_set_node(transform_set, (yyvsp[(2) - (4)].str), &(yylsp[(2) - (4)]));
		     if (node) {
			     node->v.xf.pattern = NULL;
			     node->v.xf.trans = compile_transform_expr((yyvsp[(4) - (4)].str),
								       re_flags,
								       &(yylsp[(4) - (4)]));
		     }
	     }
    break;

  case 73:
/* Line 1792 of yacc.c  */
#line 507 "cfgram.y"
    {
		     struct transform_node *node =
			     new_set_node(transform_set, (yyvsp[(2) - (6)].str), &(yylsp[(2) - (6)]));
		     if (node) {
			     node->v.xf.pattern = (yyvsp[(4) - (6)].str);
			     node->v.xf.trans = compile_transform_expr((yyvsp[(6) - (6)].str),
								       re_flags,
								       &(yylsp[(6) - (6)]));
		     }
	     }
    break;

  case 74:
/* Line 1792 of yacc.c  */
#line 518 "cfgram.y"
    {
		     struct transform_node *node =
			     new_set_node(transform_delete, (yyvsp[(2) - (2)].str), &(yylsp[(2) - (2)]));
		     if (node) {
			     node->target.v.name = (yyvsp[(2) - (2)].str);
		     }
	     }
    break;

  case 75:
/* Line 1792 of yacc.c  */
#line 526 "cfgram.y"
    {
		     if ((yyvsp[(2) - (2)].intval) == 0) {
			     cferror(&(yylsp[(2) - (2)]), _("$0 cannot be unset"));
			     errors++;
		     } else {
			     struct transform_node *node =
				     new_transform_node(current_rule,
							transform_delete);
			     node->target.type = target_arg;
			     node->target.v.arg.ins = 0;
			     node->target.v.arg.idx = (yyvsp[(2) - (2)].intval);
			     node->v.arg_end = (yyvsp[(2) - (2)].intval);
		     }
	     }
    break;

  case 76:
/* Line 1792 of yacc.c  */
#line 543 "cfgram.y"
    {
		     struct transform_node *node;

		     node = new_transform_node(current_rule, transform_map);
		     node->target.type = target_arg;
		     node->target.v.arg.ins = 0;
		     node->target.v.arg.idx = (yyvsp[(2) - (8)].intval);
		     node->v.map.file = (yyvsp[(3) - (8)].str);
		     node->v.map.delim = (yyvsp[(4) - (8)].str);
		     node->v.map.key = (yyvsp[(5) - (8)].str);
		     node->v.map.key_field = (yyvsp[(6) - (8)].num).intval;
		     node->v.map.val_field = (yyvsp[(7) - (8)].num).intval;
		     node->v.map.defval = (yyvsp[(8) - (8)].str);

		     free((yyvsp[(6) - (8)].num).strval);
		     free((yyvsp[(7) - (8)].num).strval);
	     }
    break;

  case 77:
/* Line 1792 of yacc.c  */
#line 561 "cfgram.y"
    {
		     struct transform_node *node;

		     node = new_set_node(transform_map, (yyvsp[(2) - (8)].str), &(yylsp[(2) - (8)]));
		     node->target.v.name = (yyvsp[(2) - (8)].str);
		     node->v.map.file = (yyvsp[(3) - (8)].str);
		     node->v.map.delim = (yyvsp[(4) - (8)].str);
		     node->v.map.key = (yyvsp[(5) - (8)].str);
		     node->v.map.key_field = (yyvsp[(6) - (8)].num).intval;
		     node->v.map.val_field = (yyvsp[(7) - (8)].num).intval;
		     node->v.map.defval = (yyvsp[(8) - (8)].str);

		     free((yyvsp[(6) - (8)].num).strval);
		     free((yyvsp[(7) - (8)].num).strval);
	     }
    break;

  case 78:
/* Line 1792 of yacc.c  */
#line 579 "cfgram.y"
    {
		     (yyval.str) = NULL;
	     }
    break;

  case 80:
/* Line 1792 of yacc.c  */
#line 586 "cfgram.y"
    {
		     (yyval.intval) = (yyvsp[(2) - (3)].num).intval;
		     free((yyvsp[(2) - (3)].num).strval);
	     }
    break;

  case 82:
/* Line 1792 of yacc.c  */
#line 599 "cfgram.y"
    {
		     current_rule->fall_through = 1;
	     }
    break;

  case 83:
/* Line 1792 of yacc.c  */
#line 603 "cfgram.y"
    {
		     current_rule->error = new_error((yyvsp[(2) - (3)].intval), (yyvsp[(3) - (3)].str), 0);
		     free((yyvsp[(3) - (3)].str));
	     }
    break;

  case 84:
/* Line 1792 of yacc.c  */
#line 608 "cfgram.y"
    {
		     int n = string_to_error_index((yyvsp[(3) - (3)].str));
		     if (n == -1) {
			     cferror(&(yylsp[(1) - (3)]), _("Unknown message reference"));
			     YYERROR;
		     } else
			     current_rule->error = new_standard_error((yyvsp[(2) - (3)].intval), n);
		     free((yyvsp[(3) - (3)].str));
	     }
    break;

  case 85:
/* Line 1792 of yacc.c  */
#line 620 "cfgram.y"
    {
		     (yyval.intval) = 2;
	     }
    break;

  case 86:
/* Line 1792 of yacc.c  */
#line 624 "cfgram.y"
    {
		     (yyval.intval) = (yyvsp[(1) - (1)].num).intval;
		     free((yyvsp[(1) - (1)].num).strval);
	     }
    break;

  case 87:
/* Line 1792 of yacc.c  */
#line 634 "cfgram.y"
    {
		     if ((yyvsp[(2) - (2)].range).start == 0 || (yyvsp[(2) - (2)].range).end == 0) {
			     cferror(&(yylsp[(2) - (2)]), _("$0 cannot be deleted"));
			     errors++;
		     } else {
			     struct transform_node *node =
				     new_transform_node(current_rule,
							transform_delete);
			     node->target.type = target_arg;
			     node->target.v.arg.ins = 0;
			     node->target.v.arg.idx = (yyvsp[(2) - (2)].range).start;
			     node->v.arg_end = (yyvsp[(2) - (2)].range).end;
		     }
	     }
    break;

  case 88:
/* Line 1792 of yacc.c  */
#line 651 "cfgram.y"
    {
		     (yyval.range).start = (yyval.range).end = (yyvsp[(1) - (1)].num).intval;
		     free((yyvsp[(1) - (1)].num).strval);
	     }
    break;

  case 89:
/* Line 1792 of yacc.c  */
#line 656 "cfgram.y"
    {
		     (yyval.range).start = (yyvsp[(1) - (2)].num).intval;
		     (yyval.range).end = (yyvsp[(2) - (2)].num).intval;
		     free((yyvsp[(1) - (2)].num).strval);
		     free((yyvsp[(2) - (2)].num).strval);
	     }
    break;

  case 90:
/* Line 1792 of yacc.c  */
#line 671 "cfgram.y"
    {
		     if (cflex_include((yyvsp[(2) - (3)].str), &(yylsp[(2) - (3)])))
			     YYERROR;
		     free((yyvsp[(2) - (3)].str));
	      }
    break;

  case 91:
/* Line 1792 of yacc.c  */
#line 682 "cfgram.y"
    {
		     current_rule->limits = (yyvsp[(2) - (2)].lrec);
	      }
    break;

  case 92:
/* Line 1792 of yacc.c  */
#line 688 "cfgram.y"
    {
		     char *p;
		     (yyval.lrec) = limits_record_create();
		     switch (limits_record_add((yyval.lrec), (yyvsp[(1) - (1)].str), &p)) {
		     case lrec_ok:
			     break;
		     case lrec_error:
			     cferror(&(yylsp[(1) - (1)]),
				     _("unrecognized resource limit: %s"),
				     p);
			     break;
		     case lrec_badval:
			     cferror(&(yylsp[(1) - (1)]),
				     _("bad value: %s"),
				     p);
			     break;
		     }
		     free((yyvsp[(1) - (1)].str));
	      }
    break;

  case 93:
/* Line 1792 of yacc.c  */
#line 708 "cfgram.y"
    {
		     char *p;
		     switch (limits_record_add((yyvsp[(1) - (2)].lrec), (yyvsp[(2) - (2)].str), &p)) {
		     case lrec_ok:
			     break;
		     case lrec_error:
			     cferror(&(yylsp[(1) - (2)]),
				     _("unrecognized resource limit: %s"),
				     p);
			     break;
		     case lrec_badval:
			     cferror(&(yylsp[(1) - (2)]),
				     _("bad value: %s"),
				     p);
			     break;
		     }
		     free((yyvsp[(2) - (2)].str));
		     (yyval.lrec) = (yyvsp[(1) - (2)].lrec);
	     }
    break;

  case 94:
/* Line 1792 of yacc.c  */
#line 733 "cfgram.y"
    {
		     struct transform_node *node;
		     size_t n;

		     n = strspn((yyvsp[(2) - (3)].str) + 1, ":");
		     if ((yyvsp[(2) - (3)].str)[n + 1]) {
			     struct cfloc loc;
			     loc.beg = (yylsp[(2) - (3)]).beg;
			     loc.beg.column += n + 1;
			     loc.end = loc.beg;
			     cferror(&loc,
				     _("invalid character in short option designator"));
			     cferror(&loc,
				     _("short option letter can be followed only by zero to two colons"));
			     errors++;
		     } else {
			     if (n > 2) {
				     struct cfloc loc;
				     loc.beg = (yylsp[(2) - (3)]).beg;
				     loc.beg.column += n;
				     loc.end = loc.beg;
				     cferror(&loc,
					     _("ignoring extra character in short option designator"));
				     cferror(&loc,
					     _("short option letter can be followed only by zero to two colons"));
			     }

			     node = new_transform_node(current_rule,
						       transform_remopt);
			     node->target.type = target_command;
			     node->v.remopt.s_opt = (yyvsp[(2) - (3)].str);
			     node->v.remopt.l_opt = (yyvsp[(3) - (3)].str);
		     }
	     }
    break;

  case 95:
/* Line 1792 of yacc.c  */
#line 770 "cfgram.y"
    {
		     (yyval.str) = NULL;
	     }
    break;

  case 97:
/* Line 1792 of yacc.c  */
#line 780 "cfgram.y"
    {
		     current_rule->clrenv = 1;
	      }
    break;

  case 98:
/* Line 1792 of yacc.c  */
#line 784 "cfgram.y"
    {
		     new_envar(current_rule,
			       (yyvsp[(2) - (4)].str), strlen((yyvsp[(2) - (4)].str)),
			       (yyvsp[(4) - (4)].str), strlen((yyvsp[(4) - (4)].str)),
			       envar_set);
		     free((yyvsp[(2) - (4)].str));
		     free((yyvsp[(4) - (4)].str));
	      }
    break;

  case 99:
/* Line 1792 of yacc.c  */
#line 793 "cfgram.y"
    {
		      new_envar(current_rule,
				"", 0,
				(yyvsp[(2) - (2)].str), strlen((yyvsp[(2) - (2)].str)),
				envar_eval);
		      free((yyvsp[(2) - (2)].str));
	      }
    break;

  case 100:
/* Line 1792 of yacc.c  */
#line 801 "cfgram.y"
    {
		      add_asgn_list((yyvsp[(2) - (2)].asgn_list).head, envar_unset);
	      }
    break;

  case 101:
/* Line 1792 of yacc.c  */
#line 805 "cfgram.y"
    {
		      add_asgn_list((yyvsp[(2) - (2)].asgn_list).head, envar_keep);
	      }
    break;

  case 102:
/* Line 1792 of yacc.c  */
#line 811 "cfgram.y"
    {
		      (yyval.asgn_list).head = (yyval.asgn_list).tail = (yyvsp[(1) - (1)].asgn);
	      }
    break;

  case 103:
/* Line 1792 of yacc.c  */
#line 815 "cfgram.y"
    {
		      LIST_APPEND((yyvsp[(2) - (2)].asgn), (yyvsp[(1) - (2)].asgn_list).head, (yyvsp[(1) - (2)].asgn_list).tail);
		      (yyval.asgn_list) = (yyvsp[(1) - (2)].asgn_list);
	      }
    break;

  case 104:
/* Line 1792 of yacc.c  */
#line 822 "cfgram.y"
    {
		     (yyval.asgn) = xmalloc(sizeof(*(yyval.asgn)));
		     (yyval.asgn)->next = NULL;
		     (yyval.asgn)->name = (yyvsp[(1) - (1)].str);
		     (yyval.asgn)->value = NULL;
	      }
    break;

  case 105:
/* Line 1792 of yacc.c  */
#line 829 "cfgram.y"
    {
		     (yyval.asgn) = xmalloc(sizeof(*(yyval.asgn)));
		     (yyval.asgn)->next = NULL;
		     (yyval.asgn)->name = (yyvsp[(1) - (3)].str);
		     (yyval.asgn)->value = (yyvsp[(3) - (3)].str);
	      }
    break;

  case 106:
/* Line 1792 of yacc.c  */
#line 841 "cfgram.y"
    {
		      (yyvsp[(1) - (2)].attrib)(current_rule, (yyvsp[(2) - (2)].str), &(yylsp[(2) - (2)]));
		      free((yyvsp[(2) - (2)].str));
	      }
    break;

  case 107:
/* Line 1792 of yacc.c  */
#line 847 "cfgram.y"
    { cflex_pushargs(); }
    break;

  case 108:
/* Line 1792 of yacc.c  */
#line 848 "cfgram.y"
    {
		     int i;
		     struct argval *arg;

		     cflex_popargs();
		     (yyval.strlist).argc = (yyvsp[(3) - (4)].arglist).argc;
		     (yyval.strlist).argv = xcalloc((yyvsp[(3) - (4)].arglist).argc + 1, sizeof((yyval.strlist).argv[0]));
		     for (i = 0, arg = (yyvsp[(3) - (4)].arglist).head; i < (yyvsp[(3) - (4)].arglist).argc; i++, arg = arg->next) {
			     (yyval.strlist).argv[i] = arg->strval;
			     arg->strval = NULL;
		     }
		     arglist_free((yyvsp[(3) - (4)].arglist).head);
	      }
    break;


/* Line 1792 of yacc.c  */
#line 2639 "cfgram.c"
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
  *++yylsp = yyloc;

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

  yyerror_range[1] = yylloc;

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
		      yytoken, &yylval, &yylloc);
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

  yyerror_range[1] = yylsp[1-yylen];
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

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp, yylsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  /* Using YYLLOC is tempting, but would change the location of
     the lookahead.  YYLOC is available though.  */
  YYLLOC_DEFAULT (yyloc, yyerror_range, 2);
  *++yylsp = yyloc;

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
                  yytoken, &yylval, &yylloc);
    }
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp, yylsp);
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


/* Line 2055 of yacc.c  */
#line 863 "cfgram.y"

void
yyerror(char const *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	vcferror(&curloc, fmt, ap);
	va_end(ap);
	errors = 1;
}

void
cfgram_debug(int v)
{
#ifdef YYDEBUG
	yydebug = v;
#endif
}

struct rush_rule *
new_rush_rule(char const *tag)
{
	struct rush_rule *p = xzalloc(sizeof(*p));
	LIST_APPEND(p, rule_head, rule_tail);
	static unsigned rule_num = 0;

	rule_num++;
	if (tag && tag[0])
		p->tag = xstrdup(tag);
	else {
		char buf[INT_BUFSIZE_BOUND(unsigned)];
		char *s = uinttostr(rule_num, buf);
		p->tag = xmalloc(strlen(s) + 2);
		p->tag[0] = '#';
		strcpy(p->tag + 1, s);
	}

	p->mask = NO_UMASK;
	p->gid = NO_GID;
	p->fork = rush_undefined;
	p->acct = rush_undefined;
	return p;
}

struct transform_node *
new_transform_node(struct rush_rule *rule, enum transform_node_type type)
{
	struct transform_node *p = xzalloc(sizeof(*p));
	LIST_APPEND(p, rule->transform_head, rule->transform_tail);
	p->type = type;
	return p;
}

struct test_node *
new_test_node(enum test_type type)
{
	struct test_node *p = xzalloc(sizeof(*p));
	p->type = type;
	return p;
}

struct envar *
new_envar(struct rush_rule *rule,
	  char const *name, size_t nlen,
	  char const *value, size_t vlen,
	  enum envar_type type)
{
	struct envar *p = xmalloc(sizeof(*p)
				  + nlen + 1
				  + (value ? vlen + 1 : 0));
	p->next = NULL;
	p->name = (char*)(p + 1);
	memcpy(p->name, name, nlen);
	p->name[nlen] = 0;
	if (value) {
		p->value = p->name + nlen + 1;
		memcpy(p->value, value, vlen);
		p->value[vlen] = 0;
	} else {
		p->value = NULL;
	}

	p->type = type;
	LIST_APPEND(p, rule->envar_head, rule->envar_tail);
	return p;
}

static void
add_asgn_list(struct asgn *head, enum envar_type type)
{
	for (; head; head = head->next) {
		new_envar(current_rule,
			  head->name, strlen(head->name),
			  head->value, head->value ? strlen(head->value) : 0,
			  type);
		free(head->name);
		free(head->value);
	}
}

static struct transform_node *
new_set_node(enum transform_node_type type,
	     char *varname,
	     struct cfloc const *loc)
{
	struct transform_node *node;
	enum transform_target_type tgt;

	tgt = rush_variable_target(varname);
	if (tgt == target_readonly) {
		cferror(loc, _("attempt to modify a read-only variable"));
		errors++;
		return NULL;
	}
	node = new_transform_node(current_rule, type);
	node->target.type = tgt;
	switch (tgt) {
	case target_command:
	case target_program:
		free(varname);
		if (type == transform_delete) {
			cferror(loc,
				_("attempt to unset a read-only variable"));
			errors++;
			return NULL;
		}
		break;
	case target_var:
		node->target.v.name = varname;
		break;
	default:
		die(system_error, NULL,
		    _("INTERNAL ERROR at %s:%d: invalid target type %d"),
		    __FILE__, __LINE__,
		    tgt);
	}
	return node;
}
