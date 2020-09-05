/* A Bison parser, made by GNU Bison 2.5.1.  */

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
#define YYBISON_VERSION "2.5.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 1



/* Copy the first part of user declarations.  */

/* Line 268 of yacc.c  */
#line 1 "rc-gram.y"

/*
   rc-gram.y

   This file is part of GNU Anubis.
   Copyright (C) 2003-2014 The Anubis Team.

   GNU Anubis is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3 of the License, or (at your
   option) any later version.

   GNU Anubis is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with GNU Anubis.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdlib.h>
#include <string.h>
#include <setjmp.h>	
#include "headers.h"
#include "extern.h"
#include "rcfile.h"

extern int yylex (void);
int yyerror (const char *s);

static RC_SECTION *rc_section_create (char *, RC_LOC *, RC_STMT *);
static void rc_section_destroy (RC_SECTION **);
static void rc_section_print (RC_SECTION *);
static void rc_asgn_destroy (RC_ASGN *);
static void rc_bool_destroy (RC_BOOL *);
static void rc_level_print (int, char *);
static RC_NODE *rc_node_create (enum rc_node_type, struct rc_loc *loc);
static void rc_node_destroy (RC_NODE *);
static void rc_node_print (RC_NODE *);
static void rc_rule_destroy (RC_RULE *);
static void rc_cond_destroy (RC_COND *);
 static RC_STMT *rc_stmt_create (enum rc_stmt_type, struct rc_loc *loc);
static void rc_stmt_destroy (RC_STMT *);
static void rc_stmt_list_destroy (RC_STMT *);
static void rc_stmt_print (RC_STMT *, int);
 static int reg_modifier_add (int *, char *, struct rc_loc *);
static int check_kw (char *ident, int *flags);
static int is_prog_allowed (struct rc_loc *);
 
static RC_SECTION *rc_section;
static int debug_level;
static int error_count;
static int def_regex_modifier = R_POSIX;
static struct rc_secdef *rc_secdef;

#define YYLLOC_DEFAULT(Current, Rhs, N)			  \
  do							  \
    {							  \
      if (N)						  \
	{						  \
	  (Current).beg = YYRHSLOC(Rhs, 1).beg;		  \
	  (Current).end = YYRHSLOC(Rhs, N).end;		  \
	}						  \
      else						  \
	{						  \
	  (Current).beg = YYRHSLOC(Rhs, 0).end;		  \
	  (Current).end = (Current).beg;		  \
	}						  \
    }							  \
  while (0)

#define YY_LOCATION_PRINT(File, Loc)			    \
  do							    \
    {							    \
      if (RC_LOCUS_FILE_EQ(&(Loc).beg, &(Loc).end))	    \
	fprintf(File, "%s:%lu.%lu-%lu.%lu",		    \
		(Loc).beg.file,				    \
		(unsigned long) (Loc).beg.line,		    \
		(unsigned long) (Loc).beg.column,	    \
		(unsigned long) (Loc).end.line,		    \
		(unsigned long) (Loc).end.column);	    \
      else						    \
	fprintf(File, "%s:%lu.%lu-%s:%lu.%lu",		    \
		(Loc).beg.file,				    \
		(unsigned long) (Loc).beg.line,		    \
		(unsigned long) (Loc).beg.column,	    \
		(Loc).end.file,				    \
		(unsigned long) (Loc).end.line,		    \
		(unsigned long) (Loc).end.column);	    \
    }							    \
  while (0)
 


/* Line 268 of yacc.c  */
#line 171 "rc-gram.c"

# ifndef YY_NULL
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULL nullptr
#  else
#   define YY_NULL 0
#  endif
# endif

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     EOL = 258,
     T_BEGIN = 259,
     T_END = 260,
     AND = 261,
     OR = 262,
     NE = 263,
     IF = 264,
     FI = 265,
     ELSE = 266,
     ELIF = 267,
     RULE = 268,
     DONE = 269,
     CALL = 270,
     STOP = 271,
     ADD = 272,
     REMOVE = 273,
     MODIFY = 274,
     IDENT = 275,
     STRING = 276,
     REGEX = 277,
     D_BEGIN = 278,
     T_MSGPART = 279,
     NOT = 280
   };
#endif
/* Tokens.  */
#define EOL 258
#define T_BEGIN 259
#define T_END 260
#define AND 261
#define OR 262
#define NE 263
#define IF 264
#define FI 265
#define ELSE 266
#define ELIF 267
#define RULE 268
#define DONE 269
#define CALL 270
#define STOP 271
#define ADD 272
#define REMOVE 273
#define MODIFY 274
#define IDENT 275
#define STRING 276
#define REGEX 277
#define D_BEGIN 278
#define T_MSGPART 279
#define NOT 280




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 295 of yacc.c  */
#line 104 "rc-gram.y"

  char *string;
  RC_SECTION *section;
  RC_STMT *stmt;
  struct
  {
    RC_STMT *head;
    RC_STMT *tail;
  } stmtlist;
  RC_COND cond;
  RC_RULE rule;
  RC_NODE *node;
  RC_REGEX *regex;
  int num;
  struct
  {
    int part;
    RC_REGEX *key;
    char *string;
    char *sep;
  } msgpart;
  RC_LOC loc;
  char *begin_sec;
  ANUBIS_LIST list;
  int eq;



/* Line 295 of yacc.c  */
#line 294 "rc-gram.c"
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


/* Copy the second part of user declarations.  */


/* Line 345 of yacc.c  */
#line 319 "rc-gram.c"

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
#define YYFINAL  10
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   190

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  32
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  36
/* YYNRULES -- Number of rules.  */
#define YYNRULES  70
/* YYNRULES -- Number of states.  */
#define YYNSTATES  119

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   280

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      26,    27,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    31,     2,
       2,    28,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    29,     2,    30,     2,     2,     2,     2,     2,     2,
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
      25
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint8 yyprhs[] =
{
       0,     0,     3,     5,     7,    10,    13,    15,    19,    22,
      23,    28,    31,    34,    36,    39,    41,    44,    47,    50,
      53,    56,    59,    62,    64,    66,    69,    71,    77,    78,
      83,    86,    88,    92,    96,   100,   103,   104,   106,   108,
     110,   114,   115,   117,   120,   122,   123,   127,   129,   131,
     136,   137,   141,   147,   149,   152,   153,   155,   158,   160,
     165,   169,   171,   173,   175,   177,   180,   184,   187,   192,
     196
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      33,     0,    -1,    34,    -1,    35,    -1,    34,    35,    -1,
      34,     1,    -1,     3,    -1,    36,    39,    38,    -1,    36,
      38,    -1,    -1,     4,    37,    65,     3,    -1,    23,     3,
      -1,     5,     3,    -1,    40,    -1,    39,    40,    -1,     3,
      -1,    41,     3,    -1,    45,     3,    -1,    62,     3,    -1,
      66,     3,    -1,    67,     3,    -1,     1,     3,    -1,    42,
      43,    -1,    20,    -1,    44,    -1,    43,    44,    -1,    65,
      -1,    61,    47,    39,    46,    10,    -1,    -1,    12,    47,
      39,    46,    -1,    11,    39,    -1,    57,    -1,    26,    47,
      27,    -1,    47,     6,    47,    -1,    47,     7,    47,    -1,
      25,    47,    -1,    -1,    28,    -1,     8,    -1,    55,    -1,
      29,    65,    30,    -1,    -1,    49,    -1,    24,    50,    -1,
      49,    -1,    -1,    26,    65,    27,    -1,    51,    -1,    51,
      -1,    58,    29,    65,    30,    -1,    -1,    29,    65,    30,
      -1,    53,    52,    48,    59,    65,    -1,    60,    -1,    58,
      60,    -1,    -1,    58,    -1,    31,    20,    -1,     9,    -1,
      63,     3,    39,    14,    -1,    64,    59,    65,    -1,    13,
      -1,    21,    -1,    20,    -1,    16,    -1,    15,    65,    -1,
      17,    53,    65,    -1,    18,    54,    -1,    19,    54,    56,
      65,    -1,    19,    54,    56,    -1,    22,    58,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   155,   155,   160,   164,   179,   191,   195,   199,   205,
     205,   212,   221,   224,   228,   245,   249,   250,   251,   252,
     253,   254,   263,   285,   291,   296,   303,   306,   316,   319,
     326,   332,   333,   337,   344,   351,   361,   364,   368,   374,
     380,   389,   393,   396,   402,   406,   409,   415,   424,   435,
     449,   452,   458,   478,   484,   493,   496,   499,   505,   512,
     520,   530,   537,   538,   541,   553,   565,   583,   601,   613,
     629
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "EOL", "T_BEGIN", "T_END", "AND", "OR",
  "NE", "IF", "FI", "ELSE", "ELIF", "RULE", "DONE", "CALL", "STOP", "ADD",
  "REMOVE", "MODIFY", "IDENT", "STRING", "REGEX", "D_BEGIN", "T_MSGPART",
  "NOT", "'('", "')'", "'='", "'['", "']'", "':'", "$accept", "input",
  "seclist", "section", "begin", "$@1", "end", "stmtlist", "stmt",
  "asgn_stmt", "keyword", "arglist", "arg", "cond_stmt", "else_cond",
  "cond", "meq", "key", "opt_key", "msgpart", "opt_sep", "s_msgpart",
  "r_msgpart", "regex", "string_key", "expr", "modlist", "opt_modlist",
  "modifier", "if", "rule_stmt", "rule_start", "rule", "string",
  "inst_stmt", "modf_stmt", YY_NULL
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,    40,    41,    61,    91,
      93,    58
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    32,    33,    34,    34,    34,    35,    35,    35,    37,
      36,    36,    38,    39,    39,    40,    40,    40,    40,    40,
      40,    40,    41,    42,    43,    43,    44,    45,    46,    46,
      46,    47,    47,    47,    47,    47,    48,    48,    48,    49,
      49,    50,    50,    51,    51,    52,    52,    53,    54,    55,
      56,    56,    57,    58,    58,    59,    59,    60,    61,    62,
      63,    64,    65,    65,    66,    66,    66,    66,    66,    66,
      67
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     1,     2,     2,     1,     3,     2,     0,
       4,     2,     2,     1,     2,     1,     2,     2,     2,     2,
       2,     2,     2,     1,     1,     2,     1,     5,     0,     4,
       2,     1,     3,     3,     3,     2,     0,     1,     1,     1,
       3,     0,     1,     2,     1,     0,     3,     1,     1,     4,
       0,     3,     5,     1,     2,     0,     1,     2,     1,     4,
       3,     1,     1,     1,     1,     2,     3,     2,     4,     3,
       2
};

/* YYDEFACT[STATE-NAME] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     6,     9,     0,     0,     0,     3,     0,     0,    11,
       1,     5,     4,     0,    15,     0,    58,    61,     0,    64,
       0,     0,     0,    23,     0,     8,     0,    13,     0,     0,
       0,     0,     0,     0,    55,     0,     0,    63,    62,     0,
      21,    12,    65,    41,     0,     0,    44,    47,     0,    39,
       0,    53,    48,    67,    50,    70,     7,    14,    16,    22,
      24,    26,    17,     0,     0,     0,    45,    31,    18,     0,
      56,     0,    19,    20,    10,    42,    43,     0,    57,    66,
       0,    54,     0,    69,    25,    35,     0,     0,     0,     0,
       0,    36,     0,    60,    40,     0,     0,    68,    32,    33,
      34,     0,     0,     0,     0,    38,    37,    55,    59,    49,
      51,     0,     0,    27,    46,     0,     0,    52,    29
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     4,     5,     6,     7,     8,    25,    26,    27,    28,
      29,    59,    60,    30,   103,    65,   107,    46,    76,    47,
      91,    66,    53,    49,    83,    67,    50,    71,    51,    31,
      32,    33,    34,    61,    35,    36
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -63
static const yytype_int16 yypact[] =
{
      18,   -63,   -63,    13,    19,    14,   -63,    29,   -12,   -63,
     -63,   -63,   -63,    28,   -63,    32,   -63,   -63,   -12,   -63,
      -4,    -4,    -4,   -63,    26,   -63,    29,   -63,    55,   -12,
      62,    30,    66,    67,    26,    68,    73,   -63,   -63,    74,
     -63,   -63,   -63,    31,   -12,    58,   -63,   -63,   -12,   -63,
      37,   -63,   -63,   -63,    50,    26,   -63,   -63,   -63,   -12,
     -63,   -63,   -63,    30,    30,   108,    54,   -63,   -63,   168,
      26,   -12,   -63,   -63,   -63,   -63,   -63,    51,   -63,   -63,
     -12,   -63,   -12,   -12,   -63,   -63,    46,    30,    30,    86,
     -12,     5,   128,   -63,   -63,    53,    60,   -63,   -63,   -63,
      78,   168,    30,    82,    61,   -63,   -63,    26,   -63,   -63,
     -63,   148,   108,   -63,   -63,   -12,    86,   -63,   -63
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -63,   -63,   -63,    88,   -63,   -63,    84,   -62,   -25,   -63,
     -63,   -63,    35,   -63,   -16,   -59,   -63,    69,   -63,     2,
     -63,    93,    94,   -63,   -63,   -63,   -22,    11,   -44,   -63,
     -63,   -63,   -63,    -8,   -63,   -63
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -31
static const yytype_int8 yytable[] =
{
      39,    57,    55,    89,    85,    86,    81,    92,    37,    38,
      42,    81,    70,   105,    -2,    11,     9,     1,     2,    10,
      43,     1,     2,    52,    52,    44,    81,    45,    99,   100,
      13,    40,    14,   106,    15,    41,    77,     3,    16,   111,
      79,     3,    17,   112,    18,    19,    20,    21,    22,    23,
     116,    24,    87,    88,    43,    63,    64,    45,    58,    44,
      44,    45,    45,    93,    57,    62,    80,    57,    45,    68,
      69,    72,    95,    98,    96,    97,    73,    74,    78,    82,
      90,    94,   104,   109,    87,    70,    57,    13,   114,    14,
     110,    57,   113,    12,    84,    16,   -28,   101,   102,    17,
     118,    18,    19,    20,    21,    22,    23,   117,    24,    13,
      56,    14,    75,    48,    87,    88,    54,    16,   115,     0,
       0,    17,     0,    18,    19,    20,    21,    22,    23,    13,
      24,    14,     0,     0,     0,     0,     0,    16,     0,     0,
       0,    17,   108,    18,    19,    20,    21,    22,    23,    13,
      24,    14,     0,     0,     0,     0,     0,    16,   -30,     0,
       0,    17,     0,    18,    19,    20,    21,    22,    23,    13,
      24,    14,     0,     0,     0,     0,     0,    16,     0,     0,
       0,    17,     0,    18,    19,    20,    21,    22,    23,     0,
      24
};

#define yypact_value_is_default(yystate) \
  ((yystate) == (-63))

#define yytable_value_is_error(yytable_value) \
  YYID (0)

static const yytype_int8 yycheck[] =
{
       8,    26,    24,    65,    63,    64,    50,    69,    20,    21,
      18,    55,    34,     8,     0,     1,     3,     3,     4,     0,
      24,     3,     4,    21,    22,    29,    70,    31,    87,    88,
       1,     3,     3,    28,     5,     3,    44,    23,     9,   101,
      48,    23,    13,   102,    15,    16,    17,    18,    19,    20,
     112,    22,     6,     7,    24,    25,    26,    31,     3,    29,
      29,    31,    31,    71,    89,     3,    29,    92,    31,     3,
       3,     3,    80,    27,    82,    83,     3,     3,    20,    29,
      26,    30,    90,    30,     6,   107,   111,     1,    27,     3,
      30,   116,    10,     5,    59,     9,    10,    11,    12,    13,
     116,    15,    16,    17,    18,    19,    20,   115,    22,     1,
      26,     3,    43,    20,     6,     7,    22,     9,   107,    -1,
      -1,    13,    -1,    15,    16,    17,    18,    19,    20,     1,
      22,     3,    -1,    -1,    -1,    -1,    -1,     9,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    18,    19,    20,     1,
      22,     3,    -1,    -1,    -1,    -1,    -1,     9,    10,    -1,
      -1,    13,    -1,    15,    16,    17,    18,    19,    20,     1,
      22,     3,    -1,    -1,    -1,    -1,    -1,     9,    -1,    -1,
      -1,    13,    -1,    15,    16,    17,    18,    19,    20,    -1,
      22
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,    23,    33,    34,    35,    36,    37,     3,
       0,     1,    35,     1,     3,     5,     9,    13,    15,    16,
      17,    18,    19,    20,    22,    38,    39,    40,    41,    42,
      45,    61,    62,    63,    64,    66,    67,    20,    21,    65,
       3,     3,    65,    24,    29,    31,    49,    51,    53,    55,
      58,    60,    51,    54,    54,    58,    38,    40,     3,    43,
      44,    65,     3,    25,    26,    47,    53,    57,     3,     3,
      58,    59,     3,     3,     3,    49,    50,    65,    20,    65,
      29,    60,    29,    56,    44,    47,    47,     6,     7,    39,
      26,    52,    39,    65,    30,    65,    65,    65,    27,    47,
      47,    11,    12,    46,    65,     8,    28,    48,    14,    30,
      30,    39,    47,    10,    27,    59,    39,    65,    46
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
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
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
  YYSIZE_T yysize1;
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
                yysize1 = yysize + yytnamerr (YY_NULL, yytname[yyx]);
                if (! (yysize <= yysize1
                       && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                  return 2;
                yysize = yysize1;
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

  yysize1 = yysize + yystrlen (yyformat);
  if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
    return 2;
  yysize = yysize1;

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


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Location data for the lookahead symbol.  */
YYLTYPE yylloc;

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
  int yytoken;
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

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yyls = yylsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;
  yylsp = yyls;

#if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  /* Initialize the default location before parsing starts.  */
  yylloc.first_line   = yylloc.last_line   = 1;
  yylloc.first_column = yylloc.last_column = 1;
#endif

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
  *++yyvsp = yylval;
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
        case 2:

/* Line 1810 of yacc.c  */
#line 156 "rc-gram.y"
    {
           }
    break;

  case 3:

/* Line 1810 of yacc.c  */
#line 161 "rc-gram.y"
    {
	     (yyval.section) = rc_section = (yyvsp[(1) - (1)].section);
	   }
    break;

  case 4:

/* Line 1810 of yacc.c  */
#line 165 "rc-gram.y"
    {
	     if ((yyvsp[(2) - (2)].section))
	       {
		 if (rc_section == NULL)
		   {
		     (yyval.section) = rc_section = (yyvsp[(2) - (2)].section);
		   }
		 else
		   {
		     (yyvsp[(1) - (2)].section)->next = (yyvsp[(2) - (2)].section);
		     (yyval.section) = (yyvsp[(2) - (2)].section);
		   }
	       }
	   }
    break;

  case 5:

/* Line 1810 of yacc.c  */
#line 180 "rc-gram.y"
    {
	     lex_clear_state ();
	     yychar = error_sync_begin ();
	     if (yychar > 0)
	       {
		 yyerrok;
		 yyclearin;
	       }
	   }
    break;

  case 6:

/* Line 1810 of yacc.c  */
#line 192 "rc-gram.y"
    {
	     (yyval.section) = NULL;
	   }
    break;

  case 7:

/* Line 1810 of yacc.c  */
#line 196 "rc-gram.y"
    {
	     (yyval.section) = rc_section_create ((yyvsp[(1) - (3)].begin_sec), &(yylsp[(1) - (3)]).beg, (yyvsp[(2) - (3)].stmtlist).head);
	   }
    break;

  case 8:

/* Line 1810 of yacc.c  */
#line 200 "rc-gram.y"
    {
	     (yyval.section) = NULL;
	   }
    break;

  case 9:

/* Line 1810 of yacc.c  */
#line 205 "rc-gram.y"
    { verbatim (); }
    break;

  case 10:

/* Line 1810 of yacc.c  */
#line 206 "rc-gram.y"
    {
	     (yyval.begin_sec) = (yyvsp[(3) - (4)].string);
	     if (rc_section_lookup (rc_section, (yyvsp[(3) - (4)].string))) 
	       parse_error (&(yylsp[(3) - (4)]).beg, _("Section %s already defined"), (yyvsp[(3) - (4)].string));
	     rc_secdef = anubis_find_section ((yyvsp[(3) - (4)].string));
	   }
    break;

  case 11:

/* Line 1810 of yacc.c  */
#line 213 "rc-gram.y"
    {
	     (yyval.begin_sec) = (yyvsp[(1) - (2)].string);
	     if (rc_section_lookup (rc_section, (yyvsp[(1) - (2)].string))) 
	       parse_error (&(yylsp[(1) - (2)]).beg, _("Section %s already defined"), (yyvsp[(1) - (2)].string));
	     rc_secdef = anubis_find_section ((yyvsp[(1) - (2)].string));
	   }
    break;

  case 13:

/* Line 1810 of yacc.c  */
#line 225 "rc-gram.y"
    {
	     (yyval.stmtlist).head = (yyval.stmtlist).tail = (yyvsp[(1) - (1)].stmt);
	   }
    break;

  case 14:

/* Line 1810 of yacc.c  */
#line 229 "rc-gram.y"
    {
	     if ((yyvsp[(2) - (2)].stmt))
	       {
		 if ((yyval.stmtlist).head == NULL)
		   {
		     (yyval.stmtlist).head = (yyval.stmtlist).tail = (yyvsp[(2) - (2)].stmt);
		   }
		 else
		   {
		     (yyval.stmtlist).tail->next = (yyvsp[(2) - (2)].stmt);
		     (yyval.stmtlist).tail = (yyvsp[(2) - (2)].stmt);
		   } 
	       }
	   }
    break;

  case 15:

/* Line 1810 of yacc.c  */
#line 246 "rc-gram.y"
    {
	     (yyval.stmt) = NULL;
	   }
    break;

  case 21:

/* Line 1810 of yacc.c  */
#line 255 "rc-gram.y"
    {
	     lex_clear_state ();
	     yyerrok;
	     yyclearin;
	     (yyval.stmt) = NULL;
	   }
    break;

  case 22:

/* Line 1810 of yacc.c  */
#line 264 "rc-gram.y"
    {
	     int flags;
	     if (!check_kw ((yyvsp[(1) - (2)].string), &flags))
	       {
		 parse_error (&(yylsp[(1) - (2)]).beg, _("unknown keyword: %s"), (yyvsp[(1) - (2)].string));
		 YYERROR;
	       }

	     (yyval.stmt) = rc_stmt_create (rc_stmt_asgn, &(yylsp[(1) - (2)]).beg);
	     (yyval.stmt)->v.asgn.lhs = (yyvsp[(1) - (2)].string);
	     if (list_count ((yyvsp[(2) - (2)].list)))
	       {
		 char *s = list_item ((yyvsp[(2) - (2)].list), 0);
		 if (s && !strcmp (s, "="))
		   list_remove ((yyvsp[(2) - (2)].list), s, NULL);
	       }
	     (yyval.stmt)->v.asgn.rhs = (yyvsp[(2) - (2)].list);
	     (yyval.stmt)->v.asgn.flags = flags;
	   }
    break;

  case 23:

/* Line 1810 of yacc.c  */
#line 286 "rc-gram.y"
    {
	     verbatim ();
	   }
    break;

  case 24:

/* Line 1810 of yacc.c  */
#line 292 "rc-gram.y"
    {
	     (yyval.list) = list_create ();
	     list_append ((yyval.list), (yyvsp[(1) - (1)].string));
	   }
    break;

  case 25:

/* Line 1810 of yacc.c  */
#line 297 "rc-gram.y"
    {
	     list_append ((yyvsp[(1) - (2)].list), (yyvsp[(2) - (2)].string));
	     (yyval.list) = (yyvsp[(1) - (2)].list);
	   }
    break;

  case 27:

/* Line 1810 of yacc.c  */
#line 307 "rc-gram.y"
    {
	     (yyval.stmt) = rc_stmt_create (rc_stmt_cond, &(yylsp[(1) - (5)]).beg);
	     (yyval.stmt)->v.cond.node = (yyvsp[(2) - (5)].node);
	     (yyval.stmt)->v.cond.iftrue = (yyvsp[(3) - (5)].stmtlist).head;
	     (yyval.stmt)->v.cond.iffalse = (yyvsp[(4) - (5)].stmt);
	   }
    break;

  case 28:

/* Line 1810 of yacc.c  */
#line 316 "rc-gram.y"
    {
	     (yyval.stmt) = NULL;
	   }
    break;

  case 29:

/* Line 1810 of yacc.c  */
#line 320 "rc-gram.y"
    {
	     (yyval.stmt) = rc_stmt_create (rc_stmt_cond, &(yylsp[(1) - (4)]).beg);
	     (yyval.stmt)->v.cond.node = (yyvsp[(2) - (4)].node);
	     (yyval.stmt)->v.cond.iftrue = (yyvsp[(3) - (4)].stmtlist).head;
	     (yyval.stmt)->v.cond.iffalse = (yyvsp[(4) - (4)].stmt);
	   }
    break;

  case 30:

/* Line 1810 of yacc.c  */
#line 327 "rc-gram.y"
    {
	     (yyval.stmt) = (yyvsp[(2) - (2)].stmtlist).head;
	   }
    break;

  case 32:

/* Line 1810 of yacc.c  */
#line 334 "rc-gram.y"
    {
	     (yyval.node) = (yyvsp[(2) - (3)].node);
	   }
    break;

  case 33:

/* Line 1810 of yacc.c  */
#line 338 "rc-gram.y"
    {
	     (yyval.node) = rc_node_create (rc_node_bool, &(yylsp[(2) - (3)]).beg);
	     (yyval.node)->v.bool.op = bool_and;
	     (yyval.node)->v.bool.left = (yyvsp[(1) - (3)].node);
	     (yyval.node)->v.bool.right = (yyvsp[(3) - (3)].node);
	   }
    break;

  case 34:

/* Line 1810 of yacc.c  */
#line 345 "rc-gram.y"
    {
	     (yyval.node) = rc_node_create (rc_node_bool, &(yylsp[(2) - (3)]).beg);
	     (yyval.node)->v.bool.op = bool_or;
	     (yyval.node)->v.bool.left = (yyvsp[(1) - (3)].node);
	     (yyval.node)->v.bool.right = (yyvsp[(3) - (3)].node);
	   }
    break;

  case 35:

/* Line 1810 of yacc.c  */
#line 352 "rc-gram.y"
    {
	     (yyval.node) = rc_node_create (rc_node_bool, &(yylsp[(1) - (2)]).beg);
	     (yyval.node)->v.bool.op = bool_not;
	     (yyval.node)->v.bool.left = (yyvsp[(2) - (2)].node);
	     (yyval.node)->v.bool.right = NULL;
	   }
    break;

  case 36:

/* Line 1810 of yacc.c  */
#line 361 "rc-gram.y"
    {
	     (yyval.eq) = 1;
	   }
    break;

  case 37:

/* Line 1810 of yacc.c  */
#line 365 "rc-gram.y"
    {
	     (yyval.eq) = 1;
	   }
    break;

  case 38:

/* Line 1810 of yacc.c  */
#line 369 "rc-gram.y"
    {
	     (yyval.eq) = 0;
	   }
    break;

  case 39:

/* Line 1810 of yacc.c  */
#line 375 "rc-gram.y"
    {
	     (yyval.msgpart).part = HEADER;
	     (yyval.msgpart).key = (yyvsp[(1) - (1)].regex);
	     (yyval.msgpart).string = NULL;
	   }
    break;

  case 40:

/* Line 1810 of yacc.c  */
#line 381 "rc-gram.y"
    {
	     (yyval.msgpart).part = HEADER;
	     (yyval.msgpart).key = NULL;
	     (yyval.msgpart).string = (yyvsp[(2) - (3)].string);
	   }
    break;

  case 41:

/* Line 1810 of yacc.c  */
#line 389 "rc-gram.y"
    {
	     (yyval.msgpart).string = NULL;
	     (yyval.msgpart).key = NULL;
           }
    break;

  case 43:

/* Line 1810 of yacc.c  */
#line 397 "rc-gram.y"
    {
	     (yyval.msgpart) = (yyvsp[(2) - (2)].msgpart);
	     (yyval.msgpart).sep = NULL;
	     (yyval.msgpart).part = (yyvsp[(1) - (2)].num);
	   }
    break;

  case 45:

/* Line 1810 of yacc.c  */
#line 406 "rc-gram.y"
    {
	     (yyval.string) = NULL;
	   }
    break;

  case 46:

/* Line 1810 of yacc.c  */
#line 410 "rc-gram.y"
    {
	     (yyval.string) = (yyvsp[(2) - (3)].string);
	   }
    break;

  case 47:

/* Line 1810 of yacc.c  */
#line 416 "rc-gram.y"
    {
	     (yyval.msgpart) = (yyvsp[(1) - (1)].msgpart);
	     if ((yyval.msgpart).key)
	       parse_error (&(yylsp[(1) - (1)]).beg,
			    _("regexp is not allowed in this context"));
	   }
    break;

  case 48:

/* Line 1810 of yacc.c  */
#line 425 "rc-gram.y"
    {
	     (yyval.msgpart) = (yyvsp[(1) - (1)].msgpart);
	     if (!(yyval.msgpart).key)
	       {
		 (yyval.msgpart).key = anubis_regex_compile ((yyval.msgpart).string, R_EXACT);
		 xfree ((yyval.msgpart).string);
	       }
	   }
    break;

  case 49:

/* Line 1810 of yacc.c  */
#line 436 "rc-gram.y"
    {
	     (yyval.regex) = anubis_regex_compile ((yyvsp[(3) - (4)].string), (yyvsp[(1) - (4)].num));
	     free ((yyvsp[(3) - (4)].string));
	     if (!(yyval.regex))
	       {
		 parse_error (&(yylsp[(3) - (4)]).beg,
			      _("Invalid regular expression (see the above message)"));
		 YYERROR;
	       }
	   }
    break;

  case 50:

/* Line 1810 of yacc.c  */
#line 449 "rc-gram.y"
    {
	     (yyval.string) = NULL;
	   }
    break;

  case 51:

/* Line 1810 of yacc.c  */
#line 453 "rc-gram.y"
    {
	     (yyval.string) = (yyvsp[(2) - (3)].string);
	   }
    break;

  case 52:

/* Line 1810 of yacc.c  */
#line 459 "rc-gram.y"
    {
	     RC_NODE *node = rc_node_create (rc_node_expr, &(yylsp[(1) - (5)]).beg);
	     node->v.expr.part = (yyvsp[(1) - (5)].msgpart).part;
	     node->v.expr.key = (yyvsp[(1) - (5)].msgpart).string;
	     node->v.expr.sep = (yyvsp[(2) - (5)].string);
	     node->v.expr.re = anubis_regex_compile ((yyvsp[(5) - (5)].string), (yyvsp[(4) - (5)].num));
	     free ((yyvsp[(5) - (5)].string));
	     if ((yyvsp[(3) - (5)].eq))
	       (yyval.node) = node;
	     else
	       {
		 (yyval.node) = rc_node_create (rc_node_bool, &(yylsp[(1) - (5)]).beg);
		 (yyval.node)->v.bool.op = bool_not;
		 (yyval.node)->v.bool.left = node;
		 (yyval.node)->v.bool.right = NULL;
	       }
	   }
    break;

  case 53:

/* Line 1810 of yacc.c  */
#line 479 "rc-gram.y"
    {
	     (yyval.num) = def_regex_modifier;
	     reg_modifier_add (&(yyval.num), (yyvsp[(1) - (1)].string), &(yylsp[(1) - (1)]).beg);
	     xfree ((yyvsp[(1) - (1)].string));
	   }
    break;

  case 54:

/* Line 1810 of yacc.c  */
#line 485 "rc-gram.y"
    {
	     reg_modifier_add (&(yyvsp[(1) - (2)].num), (yyvsp[(2) - (2)].string), &(yylsp[(1) - (2)]).beg);
	     xfree ((yyvsp[(2) - (2)].string));
	     (yyval.num) = (yyvsp[(1) - (2)].num);
	   }
    break;

  case 55:

/* Line 1810 of yacc.c  */
#line 493 "rc-gram.y"
    {
	     (yyval.num) = def_regex_modifier;
	   }
    break;

  case 57:

/* Line 1810 of yacc.c  */
#line 500 "rc-gram.y"
    {
	     (yyval.string) = (yyvsp[(2) - (2)].string);
	   }
    break;

  case 58:

/* Line 1810 of yacc.c  */
#line 506 "rc-gram.y"
    {
	     if (!is_prog_allowed (&(yylsp[(1) - (1)]).beg))
	       YYERROR;
	   }
    break;

  case 59:

/* Line 1810 of yacc.c  */
#line 513 "rc-gram.y"
    {
	     (yyval.stmt) = rc_stmt_create (rc_stmt_rule, &(yylsp[(1) - (4)]).beg);
	     (yyval.stmt)->v.rule.node = (yyvsp[(1) - (4)].node);
	     (yyval.stmt)->v.rule.stmt = (yyvsp[(3) - (4)].stmtlist).head;
	   }
    break;

  case 60:

/* Line 1810 of yacc.c  */
#line 521 "rc-gram.y"
    {
	     (yyval.node) = rc_node_create (rc_node_expr, &(yylsp[(1) - (3)]).beg);
	     (yyval.node)->v.expr.part = HEADER;
	     (yyval.node)->v.expr.key = strdup (X_ANUBIS_RULE_HEADER);
	     (yyval.node)->v.expr.re = anubis_regex_compile ((yyvsp[(3) - (3)].string), (yyvsp[(2) - (3)].num));
	     free ((yyvsp[(3) - (3)].string));
	   }
    break;

  case 61:

/* Line 1810 of yacc.c  */
#line 531 "rc-gram.y"
    {
	     if (!is_prog_allowed (&(yylsp[(1) - (1)]).beg))
	       YYERROR;
	   }
    break;

  case 64:

/* Line 1810 of yacc.c  */
#line 542 "rc-gram.y"
    {
	     if (!is_prog_allowed (&(yylsp[(1) - (1)]).beg))
	       YYERROR;

	     (yyval.stmt) = rc_stmt_create (rc_stmt_inst, &(yylsp[(1) - (1)]).beg);
	     (yyval.stmt)->v.inst.opcode = inst_stop;
	     (yyval.stmt)->v.inst.part = NIL;
	     (yyval.stmt)->v.inst.key  = NULL;
	     (yyval.stmt)->v.inst.key2 = NULL;
	     (yyval.stmt)->v.inst.arg  = NULL;
	   }
    break;

  case 65:

/* Line 1810 of yacc.c  */
#line 554 "rc-gram.y"
    {
	     if (!is_prog_allowed (&(yylsp[(1) - (2)]).beg))
	       YYERROR;

	     (yyval.stmt) = rc_stmt_create (rc_stmt_inst, &(yylsp[(1) - (2)]).beg);
	     (yyval.stmt)->v.inst.opcode = inst_call;
	     (yyval.stmt)->v.inst.key = NULL;
	     (yyval.stmt)->v.inst.part = NIL;
	     (yyval.stmt)->v.inst.key2 = NULL;
	     (yyval.stmt)->v.inst.arg  = (yyvsp[(2) - (2)].string);
	   }
    break;

  case 66:

/* Line 1810 of yacc.c  */
#line 566 "rc-gram.y"
    {
	     if (!is_prog_allowed (&(yylsp[(1) - (3)]).beg))
	       YYERROR;

	     if ((yyvsp[(2) - (3)].msgpart).part == COMMAND)
	       {
		 parse_error (&(yylsp[(2) - (3)]).beg, _("command part is not allowed"));
		 YYERROR;
	       }
	     
	     (yyval.stmt) = rc_stmt_create (rc_stmt_inst, &(yylsp[(1) - (3)]).beg);
	     (yyval.stmt)->v.inst.opcode = inst_add;
	     (yyval.stmt)->v.inst.part = (yyvsp[(2) - (3)].msgpart).part;
	     (yyval.stmt)->v.inst.key  = NULL;
	     (yyval.stmt)->v.inst.key2 = (yyvsp[(2) - (3)].msgpart).string;
	     (yyval.stmt)->v.inst.arg  = (yyvsp[(3) - (3)].string);
	   }
    break;

  case 67:

/* Line 1810 of yacc.c  */
#line 584 "rc-gram.y"
    {
	     if (!is_prog_allowed (&(yylsp[(1) - (2)]).beg))
	       YYERROR;

	     if ((yyvsp[(2) - (2)].msgpart).part == COMMAND)
	       {
		 parse_error (&(yylsp[(2) - (2)]).beg, _("command part is not allowed"));
		 YYERROR;
	       }
	     
	     (yyval.stmt) = rc_stmt_create (rc_stmt_inst, &(yylsp[(1) - (2)]).beg);
	     (yyval.stmt)->v.inst.opcode = inst_remove;
	     (yyval.stmt)->v.inst.part = (yyvsp[(2) - (2)].msgpart).part;
	     (yyval.stmt)->v.inst.key = (yyvsp[(2) - (2)].msgpart).key;
	     (yyval.stmt)->v.inst.key2 = NULL;
	     (yyval.stmt)->v.inst.arg  = NULL;
	   }
    break;

  case 68:

/* Line 1810 of yacc.c  */
#line 602 "rc-gram.y"
    {
	     if (!is_prog_allowed (&(yylsp[(1) - (4)]).beg))
	       YYERROR;

	     (yyval.stmt) = rc_stmt_create (rc_stmt_inst, &(yylsp[(1) - (4)]).beg);
	     (yyval.stmt)->v.inst.opcode = inst_modify;
	     (yyval.stmt)->v.inst.part = (yyvsp[(2) - (4)].msgpart).part;
	     (yyval.stmt)->v.inst.key  = (yyvsp[(2) - (4)].msgpart).key;
	     (yyval.stmt)->v.inst.key2 = (yyvsp[(3) - (4)].string);
	     (yyval.stmt)->v.inst.arg  = (yyvsp[(4) - (4)].string);
	   }
    break;

  case 69:

/* Line 1810 of yacc.c  */
#line 614 "rc-gram.y"
    {
	     if (!is_prog_allowed (&(yylsp[(1) - (3)]).beg))
	       YYERROR;

	     (yyval.stmt) = rc_stmt_create (rc_stmt_inst, &(yylsp[(1) - (3)]).beg);
	     (yyval.stmt)->v.inst.opcode = inst_modify;
	     (yyval.stmt)->v.inst.part = (yyvsp[(2) - (3)].msgpart).part;
	     (yyval.stmt)->v.inst.key  = (yyvsp[(2) - (3)].msgpart).key;
	     if ((yyvsp[(3) - (3)].string) == NULL && anubis_regex_refcnt ((yyvsp[(2) - (3)].msgpart).key))
	       parse_error (&(yylsp[(2) - (3)]).end, _("missing replacement value"));
	     (yyval.stmt)->v.inst.key2 = (yyvsp[(3) - (3)].string);
	     (yyval.stmt)->v.inst.arg  = NULL;
	   }
    break;

  case 70:

/* Line 1810 of yacc.c  */
#line 630 "rc-gram.y"
    {
	     if (!is_prog_allowed (&(yylsp[(1) - (2)]).beg))
	       YYERROR;

	     def_regex_modifier = (yyvsp[(2) - (2)].num);
	     (yyval.stmt) = NULL;
	   }
    break;



/* Line 1810 of yacc.c  */
#line 2401 "rc-gram.c"
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

  *++yyvsp = yylval;

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



/* Line 2071 of yacc.c  */
#line 639 "rc-gram.y"


static void
default_error_printer (void *data, 
		       struct rc_loc *loc,
		       const char *pfx,
		       const char *fmt, va_list ap)
{
  char buf[LINEBUFFER];
  vsnprintf (buf, sizeof buf, fmt, ap);
  if (topt & T_LOCATION_COLUMN)
    anubis_error (0, 0, "%s:%lu.%lu: %s%s%s",
		  loc->file,
		  (unsigned long)loc->line,
		  (unsigned long)loc->column,
		  pfx ? pfx : "",
		  pfx ? ": " : "",
		  buf);
  else
    anubis_error (0, 0, "%s:%lu: %s%s%s",
		  loc->file, (unsigned long)loc->line,
		  pfx ? pfx : "",
		  pfx ? ": " : "",
		  buf);
}	

static void *rc_error_printer_data;
static RC_ERROR_PRINTER rc_error_printer = default_error_printer;

void
parse_error (struct rc_loc *loc, const char *fmt, ...)
{
  va_list ap;
  
  va_start (ap, fmt);
  rc_error_printer (rc_error_printer_data, loc ? loc : &rc_locus, NULL,
		    fmt, ap);
  va_end (ap);
  error_count++;
}

int
yyerror (const char *s)
{
  parse_error (NULL, "%s", s);
  return 0;
}

RC_SECTION *
rc_parse (char *name)
{
  int status;

  yydebug = yy_flex_debug = 0;
  if (debug_level > 1)
    {
      yydebug = 1;
      if (debug_level > 2)
	yy_flex_debug = 1;
    }

  if (rc_open (name))
    return NULL;

  rc_section = NULL;
  error_count = 0;
  status = yyparse ();
  if (status || error_count) 
    rc_section_list_destroy (&rc_section);
  if (debug_level)
    rc_section_print (rc_section);
  return rc_section;
}

/* Same as rc_parse() but also allows user to specify his own
   error printer function */
RC_SECTION *
rc_parse_ep (char *name, RC_ERROR_PRINTER errprn, void *data)
{
  void *save_ep_data = rc_error_printer_data;
  void *save_ep_handler = rc_error_printer;
  RC_SECTION *sec;
  rc_error_printer = errprn;
  rc_error_printer_data = data;
  sec = rc_parse (name);
  rc_error_printer = save_ep_handler;
  rc_error_printer_data = save_ep_data;
  return sec;
}

void
rc_set_debug_level (char *arg)
{
  if (!arg)
    debug_level = 0;
  else if (arg[1] != 0 || !isdigit (arg[0]))
    {
      mprintf (_("Not a valid debugging level: %s"), arg);
      return;
    }
  else
    debug_level = arg[0] - '0';
}


/* Locations */

/* To save space, each filename is allocated only once. Each filename
   has a reference count associated with it. It is incremented
   with each new allocation of the same string. It is decremented
   with each attempt to free the string. Only when the reference count
   drops to zero is the storage actually reclaimed */

struct strobj {
  char *value;          /* String value */
  size_t refcnt;        /* Reference count */
};

/* A list of string objects */
static ANUBIS_LIST /* of struct strobj */ string_list;

static int
string_comparator (void *item, void *data)
{
  struct strobj *s = item;
  return strcmp (s->value, (char*) data);
}

static int
value_comparator (void *item, void *data)
{
  struct strobj *s = item;
  return s->value != data;
}

/* Looks up a string object with the given value. If not found, a
   new object is created and added to the list. In any case the
   reference count of the objet is incremented.
   The return value is the string value associated with the object. */
char *
string_create (char *str)
{
  struct strobj *s = list_locate (string_list, str, string_comparator);
  if (!s)
    {
      s = xmalloc (sizeof (*s));
      s->value = strdup (str);
      s->refcnt = 0;
      list_prepend (string_list, s);
    }
  s->refcnt++;
  return s->value;
}

/* Destroys the object with the given string value */
void
string_destroy (char *str)
{
  struct strobj *s = list_locate (string_list, str, value_comparator);
  if (s)
    {
      if (--s->refcnt == 0)
	{
	  free (s->value);
	  list_remove (string_list, str, value_comparator);
	}
    }
}

/* Initializes LOC with the current location. If the second argument
   is not zero, it overrides the current line number. */
void
rc_mark_loc (RC_LOC *dst, RC_LOC *src)
{
  dst->file = string_create (src->file);
  dst->line = src->line;
  dst->column = src->column;
}

/* Reclaims the memory associated with the LOC */
void
rc_destroy_loc (RC_LOC *loc)
{
  string_destroy (loc->file);
}


/* Section manipulation */

RC_SECTION *
rc_section_create (char *name, struct rc_loc *loc, RC_STMT *stmt)
{
  RC_SECTION *p = xmalloc (sizeof (*p));
  rc_mark_loc (&p->loc, loc);
  p->next = NULL;
  p->name = name;
  p->stmt = stmt;
  return p;
}

void
rc_section_destroy (RC_SECTION **s)
{
  rc_stmt_list_destroy ((*s)->stmt);
  rc_destroy_loc (&(*s)->loc);
  xfree ((*s)->name);
  xfree (*s);
}

void
rc_section_list_destroy (RC_SECTION **s)
{
  while (*s)
    {
      RC_SECTION *next = (*s)->next;
      rc_section_destroy (s);
      *s = next;
    }
}

void
rc_section_print (RC_SECTION *sect)
{
  for (; sect; sect = sect->next)
    {
      printf ("BEGIN SECTION %s\n", sect->name);
      rc_stmt_print (sect->stmt, 1);
      printf ("END SECTION %s\n", sect->name);
    }
}

RC_SECTION *
rc_section_lookup (RC_SECTION *sec, char *name)
{
  for (; sec; sec = sec->next)
    if (strcmp (sec->name, name) == 0)
      break;
  return sec;
}

void
rc_section_link (RC_SECTION **ap, RC_SECTION *b)
{
  RC_SECTION *a, *prev;

  /* Remove all sections with prio == override (the default) */
  a = *ap;
  prev = NULL;
  while (a)
    {
      RC_SECTION *next = a->next;
      struct rc_secdef *sd = anubis_find_section (a->name);
      if (sd && sd->prio == prio_user_only)
	{
	  if (prev)
	    prev->next = next;
	  else
	    *ap = next;
	  rc_section_destroy (&a);
	} else
	  prev = a;
      a = next;
    }
		
  if (!*ap)
    {
      *ap = b;
      return;
    }

  for (a = *ap; a->next; a = a->next)
    ;

  while (b)
    {
      struct rc_secdef *sd;
      RC_SECTION *nxtptr = b->next;

      sd = anubis_find_section (b->name);
      if (sd)
	{
	  switch (sd->prio)
	    {
	    case prio_user:
	      b->next = *ap;
	      *ap = b;
	      break;
	    
	    case prio_system_only:
	      rc_section_destroy (&b);
	      break;
	    
	    default:
	      b->next = NULL;
	      a->next = b;
	      a = b;
	    }
	}
      else
	{
	  b->next = NULL;
	  a->next = b;
	  a = b;
	}
      b = nxtptr;
    }
}

/* Assignment manipulations */

void
rc_asgn_destroy (RC_ASGN *asgn)
{
  xfree (asgn->lhs);
  list_destroy (&asgn->rhs, anubis_free_list_item, NULL);
}

/* Bools */

void
rc_bool_destroy (RC_BOOL *bool)
{
  rc_node_destroy (bool->left);
  rc_node_destroy (bool->right);
}

/* Nodes */

/* FIXME: 2nd should better be struct rc_yyltype */
RC_NODE *
rc_node_create (enum rc_node_type t, struct rc_loc *loc)
{
  RC_NODE *p = xmalloc (sizeof (*p));
  memset (p, 0, sizeof (*p));
  rc_mark_loc (&p->loc, loc);
  p->type = t;
  return p;
}

void
rc_node_destroy (RC_NODE *node)
{
  if (!node)
    return;
  switch (node->type)
    {
    case rc_node_bool:
      rc_bool_destroy (&node->v.bool);
      break;
    
    case rc_node_expr:
      free (node->v.expr.key);
      anubis_regex_free (&node->v.expr.re);
    }
  rc_destroy_loc (&node->loc);
  xfree (node);
}

static char *
part_string (int part)
{
  switch (part)
    {
    case NIL:
      return "NIL";
    case COMMAND:
      return "COMMAND";
    case HEADER:
      return "HEADER";
    case BODY:
      return "BODY";
    default:
      return "UNKNOWN";
    }
}

void
rc_node_print (RC_NODE *node)
{
  switch (node->type)
    {
    case rc_node_expr:
      printf ("%s", part_string (node->v.expr.part));
      if (node->v.expr.key && node->v.expr.key[0] != '\n')
	printf ("[%s]",node->v.expr.key);
      if (node->v.expr.sep)
	printf ("(%s)", node->v.expr.sep);
      printf (" ");
      anubis_regex_print (node->v.expr.re);
      break;
		
    case rc_node_bool:
      switch (node->v.bool.op)
	{
	case bool_not:
	  printf ("NOT (");
	  rc_node_print (node->v.bool.left);
	  printf (")");
	  break;
      
	case bool_and:
	  printf ("AND (");
	  rc_node_print (node->v.bool.left);
	  printf (",");
	  rc_node_print (node->v.bool.right);
	  printf (")");
	  break;
      
	case bool_or:
	  printf ("OR (");
	  rc_node_print (node->v.bool.left);
	  printf (",");
	  rc_node_print (node->v.bool.right);
	  printf (")");
	  break;
	}
    }
}

/* Rules */

void
rc_rule_destroy (RC_RULE *rule)
{
  rc_node_destroy (rule->node);
  rc_stmt_list_destroy (rule->stmt);
}

/* Conditionals */

void
rc_cond_destroy (RC_COND *cond)
{
  rc_node_destroy (cond->node);
  rc_stmt_list_destroy (cond->iftrue);
  rc_stmt_list_destroy (cond->iffalse);
}

/* Instructions */

void
rc_inst_destroy (RC_INST *inst)
{
  anubis_regex_free (&inst->key);
  free (inst->key2);
  free (inst->arg);
}

static char *
inst_name (enum rc_inst_opcode opcode)
{
  switch (opcode)
    {
    case inst_stop:
      return "STOP";
    case inst_call:
      return "CALL";
    case inst_add:
      return "ADD";
    case inst_remove:
      return "REMOVE";
    case inst_modify:
      return "MODIFY";
    }
  return "UNKNOWN";
}

void
rc_inst_print (RC_INST *inst, int level)
{
  rc_level_print (level, inst_name (inst->opcode));
  switch (inst->opcode)
    {
    case inst_stop:
      break;
    
    case inst_call:
      printf (" %s", inst->arg);
      break;
    
    case inst_add:
      printf (" %s[%s]", part_string (inst->part), inst->key2);
      if (inst->arg)
	printf (" \"%s\"", inst->arg);
      break;
    
    default:
      printf (" %s ", part_string (inst->part));
      if (inst->key)
	anubis_regex_print (inst->key);
      if (inst->key2)
	printf (" [%s]", inst->key2);
      if (inst->arg)
	printf (" \"%s\"", inst->arg);
    }
}

/* Statements */
/* FIXME: See rc_node_create */
RC_STMT *
rc_stmt_create (enum rc_stmt_type type, struct rc_loc *loc)
{
  RC_STMT *p = xmalloc (sizeof (*p));
  memset (p, 0, sizeof (*p));
  rc_mark_loc (&p->loc, loc);
  p->type = type;
  return p;
}

void
rc_stmt_destroy (RC_STMT *stmt)
{
  switch (stmt->type)
    {
    case rc_stmt_asgn:
      rc_asgn_destroy (&stmt->v.asgn);
      break;
    
    case rc_stmt_rule:
      rc_rule_destroy (&stmt->v.rule);
      break;
    
    case rc_stmt_cond:
      rc_cond_destroy (&stmt->v.cond);
      break;
    
    case rc_stmt_inst:
      rc_inst_destroy (&stmt->v.inst);
    }
  rc_destroy_loc (&stmt->loc);
  xfree (stmt);
}

void
rc_stmt_list_destroy (RC_STMT *stmt)
{
  while (stmt)
    {
      RC_STMT *next = stmt->next;
      rc_stmt_destroy (stmt);
      stmt = next;
    }
}

void
rc_level_print (int level, char *str)
{
  int i;
  
  for (i = 0; i < level*2; i++)
    putchar (' ');
  printf ("%s", str);
}

static int
_print_str (void *item, void *data)
{
  printf (" %s", (char*)item);
  return 0;
}

static int
_print_stars (void *item, void *data)
{
  printf (" ***");
  return 0;
}

void
rc_stmt_print (RC_STMT *stmt, int level)
{
  for (; stmt; stmt = stmt->next)
    {
      switch (stmt->type)
	{
	case rc_stmt_asgn:
	  rc_level_print (level, "ASGN: ");
	  printf ("%s =", stmt->v.asgn.lhs);
	  list_iterate (stmt->v.asgn.rhs,
			(stmt->v.asgn.flags & KWF_HIDDEN) ?
			_print_stars : _print_str, NULL);
	  break;
	
	case rc_stmt_cond:
	  rc_level_print (level, "COND: ");
	  rc_node_print (stmt->v.cond.node);
	  printf ("\n");
	  rc_level_print (level, "IFTRUE:\n");
	  rc_stmt_print (stmt->v.cond.iftrue, level+1);
	  if (stmt->v.cond.iffalse)
	    {
	      rc_level_print (level, "IFFALSE:\n");
	      rc_stmt_print (stmt->v.cond.iffalse, level+1);
	    }
	  rc_level_print (level, "END COND");
	  break;
	
	case rc_stmt_rule:
	  rc_level_print (level, "RULE: ");
	  rc_node_print (stmt->v.rule.node);
	  printf ("\n");
	  rc_level_print (level, "BODY\n");
	  rc_stmt_print (stmt->v.rule.stmt, level+1);
	  rc_level_print (level, "END RULE");
	  break;
	
	case rc_stmt_inst:
	  rc_inst_print (&stmt->v.inst, level);
	  break;
	
	default:
	  abort ();
	}
      printf ("\n");
    }
}

int
reg_modifier_add (int *flag, char *opt, struct rc_loc *loc)
{
  /* Regex types: */
  if (!strcasecmp (opt, "re") || !strcasecmp (opt, "regex"))
    {
      re_set_type (*flag, re_typeof (def_regex_modifier));
    }
  else if (!strcasecmp (opt, "posix"))
    {
      re_set_type (*flag, R_POSIX);
    }
#ifdef HAVE_PCRE
  else if (!strcasecmp (opt, "perlre")
	   || !strcasecmp (opt, "perl"))
    {
      re_set_type (*flag, R_PERLRE);
    }
#endif /* HAVE_PCRE */
  else if (!strcasecmp (opt, "ex") || !strcasecmp (opt, "exact"))
    {
      re_set_type (*flag, R_EXACT);
    }

  /* Modifiers: */
  else if (!strcasecmp (opt, "basic"))
    {
      re_set_type (*flag, R_POSIX);
      re_set_flag (*flag, R_BASIC);
    }
  else if (!strcasecmp (opt, "extended"))
    {
      re_set_type (*flag, R_POSIX);
      re_clear_flag (*flag, R_BASIC);
    }
  else if (!strcasecmp (opt, "scase"))
    re_set_flag (*flag, R_SCASE);
  else if (!strcasecmp (opt, "icase"))
    re_clear_flag (*flag, R_SCASE);
  else
    {
      parse_error (loc, _("Unknown regexp modifier"));
      return 1;
    }
  return 0;
}


/* ******************************* Runtime ********************************* */

static struct rc_secdef_child *
child_copy (struct rc_secdef_child *p)
{
  struct rc_secdef_child *newp = xmalloc (sizeof (*newp));
  memcpy (newp, p, sizeof (*newp));
  newp->next = NULL;
  return newp;
}	

void
rc_secdef_add_child (struct rc_secdef *def,
		     struct rc_secdef_child *child)
{
  struct rc_secdef_child *p = child_copy (child);
  if (!def->child)
    def->child = p;
  else
    {
      struct rc_secdef_child *last;
      for (last = def->child; last->next; last = last->next)
	;
      last->next = p;
    }
}

struct rc_secdef_child *
rc_child_lookup (struct rc_secdef_child *child, char *str,
		 int method, int *key, int *flags)
{
  for (; child; child = child->next)
    {
      if (child->method & method)
	{
	  struct rc_kwdef *kw;
	  for (kw = child->kwdef; kw->name; kw++)
	    if (!strcmp (kw->name, str))
	      {
		*key = kw->tok;
		if (flags)
		  *flags = kw->flags;
		return child;
	      }
	}
    }
  return NULL;
}


struct disabled_keyword
{
  int method_mask;
  char *keyword;
};


static ANUBIS_LIST disabled_keyword_list;

void
rc_disable_keyword (int mask, const char *kw)
{
  ITERATOR itr;
  struct disabled_keyword *p;
  
  if (!disabled_keyword_list)
    disabled_keyword_list = list_create ();

  itr = iterator_create (disabled_keyword_list);
  for (p = iterator_first (itr); p; p = iterator_next (itr))
    {
      if (strcmp (p->keyword, kw) == 0)
	{
	  p->method_mask |= mask;
	  break;
	}
    }
  iterator_destroy (&itr);
  
  if (!p)
    {
      p = xmalloc (sizeof (*p));
      p->method_mask = mask;
      p->keyword = xstrdup (kw);
      list_append (disabled_keyword_list, p);
    }
}

int
rc_keyword_is_disabled (int mask, const char *kw)
{
  int rc = 0;
  if (disabled_keyword_list)
    {
      ITERATOR itr = iterator_create (disabled_keyword_list);
      struct disabled_keyword *p;

      for (p = iterator_first (itr); p; p = iterator_next (itr))
	{
	  if ((rc = (p->method_mask & mask) && strcmp (p->keyword, kw) == 0))
	    break;
	}
      iterator_destroy (&itr);
    }
  return rc;
}


struct eval_env
{
  int method;
  int cmp_method;
  struct rc_secdef_child *child;
  MESSAGE msg;
  void *data;
  int refcnt;
  char **refstr;
  jmp_buf jmp;
  RC_LOC loc;
  int traceable;
};

struct rc_loc const *
eval_env_locus (struct eval_env *env)
{
  return &env->loc;
}

int
eval_env_method (struct eval_env *env)
{
  return env->method;
}

MESSAGE 
eval_env_message (struct eval_env *env)
{
  return env->msg;
}

void *
eval_env_data (struct eval_env *env)
{
  return env->data;
}

void
eval_error (int retcode, struct eval_env *env, const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  rc_error_printer (rc_error_printer_data, &env->loc, NULL, fmt, ap);
  va_end(ap);
  if (retcode)
    longjmp(env->jmp, retcode);
}

void
eval_warning (struct eval_env *env, const char *fmt, ...)
{
  va_list ap;
  
  va_start(ap, fmt);
  rc_error_printer (rc_error_printer_data, &env->loc, _("warning"),
		    fmt, ap);
  va_end(ap);
}

static void asgn_eval (struct eval_env *env, RC_ASGN *asgn);
static int node_eval (struct eval_env *env, RC_NODE *node);
static int bool_eval (struct eval_env *env, RC_BOOL *bool);
static void cond_eval (struct eval_env *env, RC_COND *cond);
static void rule_eval (struct eval_env *env, RC_RULE *rule);
static void stmt_list_eval (struct eval_env *env, RC_STMT *stmt);
static void inst_eval (struct eval_env *env, RC_INST *inst);

#define VALID_STR(s) ((s)?(s):"NULL")

void
inst_eval (struct eval_env *env, RC_INST *inst)
{
  char *arg = NULL, *argp = NULL;
  
  if (!env->msg)
    return; /* FIXME: bail out? */
	
  if (inst->arg)
    {
      if (env->refstr)
	arg = argp = substitute (inst->arg, env->refstr);
      else
	arg = inst->arg;
    }
  
  switch (inst->opcode)
    {
    case inst_stop:
      tracefile (&env->loc, _("STOP"));
      longjmp (env->jmp, 1);
      break;
    
    case inst_call:
      tracefile (&env->loc, _("Calling %s"), inst->arg);
      rcfile_call_section (env->method, inst->arg, "RULE",
			   env->data, env->msg);
      break;
    
    case inst_add:
      tracefile (&env->loc, _("ADD %s [%s] %s"),
		 part_string (inst->part),
		 VALID_STR (inst->key2), arg);
      if (inst->part == BODY) 
	message_add_body (env->msg, inst->key2, arg);
      else if (inst->part == HEADER)
	message_add_header (env->msg, inst->key2, arg);
      break;
    
    case inst_modify:
      tracefile (&env->loc, _("MODIFY %s [%s] [%s] %s"),
		 part_string (inst->part),
		 anubis_regex_source (inst->key), 
		 VALID_STR (inst->key2), arg);

      switch (inst->part)
	{
	case BODY:
	  message_modify_body (env->msg, inst->key, arg);
	  break;
	  
	case HEADER:
	  message_modify_headers (env->msg, inst->key, inst->key2, arg);
	  break;

	case COMMAND:
	  message_modify_command (env->msg, inst->key, inst->key2, arg);
	  break;
	}
      break;
    
    case inst_remove:
      tracefile (&env->loc, _("REMOVE HEADER [%s]"),
		 anubis_regex_source (inst->key));
      message_remove_headers (env->msg, inst->key);
      break;
    
    default:
      abort ();
    }
  
  if (argp)
    free (argp);
}
	
void
asgn_eval (struct eval_env *env, RC_ASGN *asgn)
{
  int key;
  struct rc_secdef_child *p = rc_child_lookup (env->child, asgn->lhs,
					       env->method, &key, NULL);
  if (!p)
    return;

  if (rc_keyword_is_disabled (env->method, asgn->lhs))
    {
      eval_warning (env,
		    _("ignoring statement overridden from the command line"));
      return;
    }

  if (env->traceable)
    tracefile (&env->loc, _("Executing %s"), asgn->lhs);

  if (env->refstr)
    {
      char *s;
      ANUBIS_LIST arg = list_create ();
      ITERATOR itr = iterator_create (asgn->rhs);
      for (s = iterator_first (itr); s; s = iterator_next (itr))
	{
	  char *str = substitute (s, env->refstr);
	  list_append (arg, str);
	}
      iterator_destroy (&itr);
      p->parser (env, key, arg, p->data);
      list_destroy (&arg, anubis_free_list_item, NULL);
    }
  else
    p->parser (env, key, asgn->rhs, p->data);
}


int
re_eval_list (struct eval_env *env, char *key, char *sep,
	      RC_REGEX *re, ANUBIS_LIST list)
{
  ASSOC *p;
  ITERATOR itr;
  int rc = 0;

  itr = iterator_create (list);
  if (sep)
    {
      char *tmpbuf = NULL;
      size_t tmpsize = 0;
      size_t seplen = strlen (sep);
      char *first_val = NULL;
      
      for (p = iterator_first (itr); p; p = iterator_next (itr))
	{
	  if (!p->key || !strcasecmp (p->key, key))
	    {
	      if (tmpsize == 0)
		{
		  if (!first_val)
		    {
		      first_val = p->value; /* Initialize copy on write */
		      continue;
		    }
		  else
		    {
		      tmpbuf = xstrdup (first_val);
		      tmpsize = strlen (tmpbuf);
		      first_val = NULL;
		    }
		}
	      tmpsize += seplen + strlen (p->value);
	      tmpbuf = xrealloc (tmpbuf, tmpsize + 1);
	      strcat (tmpbuf, sep);
	      strcat (tmpbuf, p->value);
	    }
	}
      rc = anubis_regex_match (re, first_val ? first_val : tmpbuf,
			       &env->refcnt, &env->refstr);
      free (tmpbuf);
    }
  else
    {
      for (p = iterator_first (itr); rc == 0 && p; p = iterator_next (itr))
	{
	  if (!p->key || !strcasecmp (p->key, key))
	    rc = anubis_regex_match (re, p->value,
				     &env->refcnt, &env->refstr);
	}
    }
  iterator_destroy (&itr);
  return rc;
}

int
re_eval_text (struct eval_env *env, RC_REGEX *re, const char *text)
{
  /* FIXME */
  return anubis_regex_match (re, text, &env->refcnt, &env->refstr);
}

int
expr_eval (struct eval_env *env, RC_EXPR *expr)
{
  int rc;

  if (env->refstr && anubis_regex_refcnt (expr->re))
    {
      argcv_free (-1, env->refstr);
      env->refcnt = 0;
      env->refstr = NULL;
    }
  
  switch (expr->part)
    {
    case COMMAND:
      rc = re_eval_list (env, expr->key, expr->sep, expr->re,
			 message_get_commands (env->msg));
      break;
    
    case HEADER:
      rc = re_eval_list (env, expr->key, expr->sep, expr->re,
			 message_get_header (env->msg));
      break;
    
    case BODY:
      rc = re_eval_text (env, expr->re, message_get_body (env->msg));
      break;
    
    default:
      abort ();
    }

  if (rc)
    {
      if (!strcmp (VALID_STR (expr->key), X_ANUBIS_RULE_HEADER))
	tracefile (&env->loc, _("Matched trigger \"%s\""),
		   anubis_regex_source (expr->re));
      else
	tracefile (&env->loc, 
		   _("Matched condition %s[%s] \"%s\""),
		   part_string (expr->part),
		   VALID_STR (expr->key),
		   anubis_regex_source (expr->re));
    }
  return rc;
}

int
node_eval (struct eval_env *env, RC_NODE *node)
{
  int rc; /* It won't be used uninitialized despite what cc says.
	     Note default: branch below */
  
  env->loc = node->loc;
  switch (node->type)
    {
    case rc_node_bool:
      rc = bool_eval (env, &node->v.bool);
      break;
    
    case rc_node_expr:
      rc = expr_eval (env, &node->v.expr);
      break;
    
    default:
      abort ();
    }
  
  return rc;
}

int
bool_eval (struct eval_env *env, RC_BOOL *bool)
{
  int rc = node_eval (env, bool->left);

  switch (bool->op)
    {
    case bool_not:
      return !rc;
    
    case bool_and:
      if (!rc)
	return 0;
      break;
    
    case bool_or:
      if (rc)
	return 1;
      break;
    }
  return node_eval (env, bool->right);
}

void
cond_eval (struct eval_env *env, RC_COND *cond)
{
  if (node_eval (env, cond->node))
    stmt_list_eval (env, cond->iftrue);
  else
    stmt_list_eval (env, cond->iffalse);
}

void
rule_eval (struct eval_env *env, RC_RULE *rule)
{
  if (node_eval (env, rule->node))
    stmt_list_eval (env, rule->stmt);
}

void
stmt_list_eval (struct eval_env *env, RC_STMT *stmt)
{
  for (; stmt; stmt = stmt->next)
    {
      env->loc = stmt->loc;
      
      switch (stmt->type)
	{
	case rc_stmt_asgn:
	  asgn_eval (env, &stmt->v.asgn);
	  break;
	
	case rc_stmt_cond:
	  cond_eval (env, &stmt->v.cond);
	  break;
	
	case rc_stmt_rule:
	  rule_eval (env, &stmt->v.rule);
	  break;
	
	case rc_stmt_inst:
	  inst_eval (env, &stmt->v.inst);
	}
    }
}

void
eval_section (int method, RC_SECTION *sec, struct rc_secdef *secdef,
	      void *data, MESSAGE msg)
{
  struct eval_env env;
  env.method = method;
  env.child = secdef->child;
  env.refcnt = 0;
  env.refstr = NULL;
  env.msg = msg;
  env.data = data;
  env.loc = sec->loc;
  env.traceable = secdef->allow_prog;

  if (env.traceable)
    tracefile (&sec->loc, _("Section %s"), sec->name);
  
  if (setjmp (env.jmp) == 0)
    stmt_list_eval (&env, sec->stmt);
  
  if (env.refstr)
    argcv_free (-1, env.refstr);
}	

void
rc_run_section (int method, RC_SECTION *sec, struct rc_secdef *secdef,
		const char *class_name, 
		void *data, MESSAGE msg)
{
  if (!sec)
    return;
  if (!class_name)
    class_name = sec->name;
  
  for (; secdef->name; secdef++)
    {
      if (!strcmp (secdef->name, class_name))
	{
	  eval_section (method, sec, secdef, data, msg);
	  return;
	}
    }
  anubis_error (0, 0, _("Unknown section: %s"), sec->name);
}


static int
check_kw (char *ident, int *flags)
{
  struct rc_secdef *p = rc_secdef;
  int key;
  
  if (!p)
    p = anubis_find_section ("RULE");
  return rc_child_lookup (p->child, ident, CF_ALL, &key, flags) != NULL;
}

static int
is_prog_allowed (struct rc_loc *loc)
{
  struct rc_secdef *p = rc_secdef;
  if (!p)
    p = anubis_find_section ("RULE");
  
  if (!p->allow_prog)
    parse_error (loc, _("program is not allowed in this section"));
  return p->allow_prog;
}

