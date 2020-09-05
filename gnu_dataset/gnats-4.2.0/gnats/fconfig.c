
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
   
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
#define YYBISON_VERSION "2.4.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0

/* Substitute the variable and function names.  */
#define yyparse         fconfparse
#define yylex           fconflex
#define yyerror         fconferror
#define yylval          fconflval
#define yychar          fconfchar
#define yydebug         fconfdebug
#define yynerrs         fconfnerrs


/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
#line 1 "fconfig.y"

#include "gnats.h"
#include "field.h"
#ifdef DS_FILE
#include "ds-file/index.h"
#endif
  extern DatabaseInfo databaseBeingDefined;
  static FieldDef currField;
  static ChangeActions currChange;
  static FieldEdit *currEdit;
  static QueryFormat *qformat;
  static FieldList requiredFlds;
  static InputTemplate *inputTemplate;
  static MailMessageFormat mailFormat;
  static int badFile;
  struct qstring;
#ifdef DS_FILE
  IndexDesc indexEntry;
#endif

  extern char *takeQString (struct qstring *str);
  extern char *qStrVal (struct qstring *str);


/* Line 189 of yacc.c  */
#line 106 "fconfig.c"

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
# define YYTOKEN_TABLE 0
#endif


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

/* Line 214 of yacc.c  */
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



/* Line 214 of yacc.c  */
#line 300 "fconfig.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 312 "fconfig.c"

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

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  8
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   400

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  77
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  90
/* YYNRULES -- Number of rules.  */
#define YYNRULES  199
/* YYNRULES -- Number of states.  */
#define YYNSTATES  349

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   327

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,    75,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    73,    76,    74,     2,     2,     2,     2,
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
      65,    66,    67,    68,    69,    70,    71,    72
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     7,    16,    21,    26,    27,    29,
      31,    34,    37,    40,    43,    46,    49,    54,    59,    62,
      65,    67,    69,    71,    72,    74,    77,    82,    87,    90,
      92,    95,    97,    99,   101,   103,   106,   108,   110,   113,
     115,   118,   121,   123,   129,   135,   137,   140,   145,   150,
     155,   160,   165,   170,   171,   174,   178,   182,   186,   188,
     189,   191,   194,   199,   204,   207,   210,   212,   215,   217,
     219,   222,   224,   230,   235,   237,   239,   240,   242,   244,
     247,   248,   252,   256,   257,   261,   264,   266,   269,   271,
     274,   276,   279,   284,   289,   291,   293,   296,   298,   301,
     304,   311,   316,   318,   320,   322,   325,   327,   330,   333,
     336,   338,   341,   343,   346,   349,   351,   353,   355,   358,
     360,   363,   365,   370,   372,   374,   377,   378,   380,   382,
     385,   390,   395,   398,   401,   404,   407,   409,   412,   415,
     416,   418,   420,   423,   428,   433,   435,   438,   440,   445,
     450,   453,   454,   459,   464,   466,   469,   474,   479,   481,
     484,   486,   489,   494,   499,   502,   504,   507,   509,   511,
     516,   521,   526,   529,   532,   536,   540,   542,   544,   547,
     550,   552,   554,   558,   560,   563,   565,   570,   575,   577,
     579,   581,   584,   587,   592,   597,   599,   601,   604,   607
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
      78,     0,    -1,    79,    -1,    81,    -1,    80,    85,   128,
     159,   146,    96,   160,   140,    -1,    42,    73,    82,    74,
      -1,    42,    73,    81,    74,    -1,    -1,     1,    -1,    83,
      -1,    82,    83,    -1,    43,    84,    -1,    44,    84,    -1,
      45,    84,    -1,    46,    23,    -1,    47,    84,    -1,    48,
      24,    75,    24,    -1,    49,    24,    75,    24,    -1,    50,
      84,    -1,    72,    23,    -1,    51,    -1,    52,    -1,    86,
      -1,    -1,    87,    -1,    86,    87,    -1,    88,    73,    89,
      74,    -1,    88,    73,    81,    74,    -1,     3,    23,    -1,
      90,    -1,    89,    90,    -1,    92,    -1,   126,    -1,   125,
      -1,    91,    -1,    71,   153,    -1,    93,    -1,    95,    -1,
      16,   110,    -1,    17,    -1,    32,   110,    -1,    68,   111,
      -1,     4,    -1,     4,     6,    73,    94,    74,    -1,     4,
       6,    73,    81,    74,    -1,    23,    -1,    94,    23,    -1,
       7,    73,   113,    74,    -1,    18,    73,   118,    74,    -1,
      19,    73,   120,    74,    -1,     8,    73,   114,    74,    -1,
       7,    73,    81,    74,    -1,    18,    73,    81,    74,    -1,
      -1,    96,    97,    -1,    99,   101,    74,    -1,    99,   101,
      74,    -1,    39,   100,    73,    -1,    23,    -1,    -1,   102,
      -1,   101,   102,    -1,    36,    73,   103,    74,    -1,    36,
      73,    81,    74,    -1,    38,   105,    -1,    37,   105,    -1,
      61,    -1,    60,   153,    -1,    62,    -1,   104,    -1,   103,
     104,    -1,    23,    -1,   106,    73,   107,   108,    74,    -1,
     106,    73,    81,    74,    -1,    23,    -1,    23,    -1,    -1,
     109,    -1,    23,    -1,   109,    23,    -1,    -1,    73,   122,
      74,    -1,    73,    81,    74,    -1,    -1,    73,   112,    74,
      -1,    69,    24,    -1,   116,    -1,   113,   116,    -1,   115,
      -1,   114,   115,    -1,   116,    -1,    15,    23,    -1,     9,
      73,   117,    74,    -1,     9,    73,    81,    74,    -1,   122,
      -1,    23,    -1,   117,    23,    -1,   119,    -1,   118,   119,
      -1,    20,    23,    -1,    21,    73,   123,    74,    22,    23,
      -1,    21,    73,    81,    74,    -1,   122,    -1,    35,    -1,
     121,    -1,   118,   121,    -1,   119,    -1,    15,    23,    -1,
      10,    23,    -1,    33,    23,    -1,   124,    -1,   123,   124,
      -1,    23,    -1,     5,    11,    -1,     5,    12,    -1,    25,
      -1,    30,    -1,    31,    -1,    34,    23,    -1,    98,    -1,
      40,    23,    -1,    63,    -1,    67,    73,   127,    74,    -1,
      70,    -1,    23,    -1,   127,    23,    -1,    -1,   129,    -1,
     130,    -1,   129,   130,    -1,   131,    73,   132,    74,    -1,
     131,    73,    81,    74,    -1,    26,    23,    -1,   133,   134,
      -1,   135,   133,    -1,    21,    13,    -1,   137,    -1,   136,
     137,    -1,   137,   136,    -1,    -1,   135,    -1,    65,    -1,
      27,    23,    -1,    21,    73,   138,    74,    -1,    21,    73,
      81,    74,    -1,   139,    -1,   138,   139,    -1,    23,    -1,
      41,    73,   141,    74,    -1,    41,    73,    81,    74,    -1,
     144,   142,    -1,    -1,    36,    73,   143,    74,    -1,    36,
      73,    81,    74,    -1,   139,    -1,   143,   139,    -1,    21,
      73,   145,    74,    -1,    21,    73,    81,    74,    -1,    23,
      -1,   145,    23,    -1,   147,    -1,   146,   147,    -1,   148,
      73,   149,    74,    -1,   148,    73,    81,    74,    -1,    53,
      23,    -1,   150,    -1,   149,   150,    -1,   151,    -1,   152,
      -1,    54,    73,   155,    74,    -1,    55,    73,   156,    74,
      -1,    56,    73,   155,    74,    -1,    58,   153,    -1,    59,
     153,    -1,   154,   132,    74,    -1,   154,    81,    74,    -1,
      73,    -1,   156,    -1,   155,   156,    -1,    57,    23,    -1,
     157,    -1,   158,    -1,   157,    76,   158,    -1,    23,    -1,
      60,   153,    -1,   161,    -1,   162,    73,   163,    74,    -1,
     162,    73,    81,    74,    -1,    81,    -1,    28,    -1,   164,
      -1,   163,   164,    -1,    20,    23,    -1,    21,    73,   165,
      74,    -1,    21,    73,    81,    74,    -1,   166,    -1,   139,
      -1,   165,   139,    -1,    29,    23,    -1,    64,    84,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    66,    66,    67,    70,    73,    74,    75,    80,    82,
      83,    86,    89,    92,    95,    98,   101,   104,   107,   110,
     115,   116,   119,   120,   125,   126,   129,   132,   137,   152,
     153,   156,   157,   158,   159,   162,   168,   169,   170,   174,
     178,   182,   188,   191,   194,   197,   201,   207,   211,   216,
     221,   230,   231,   234,   235,   238,   244,   255,   264,   267,
     272,   273,   276,   277,   278,   282,   287,   290,   294,   299,
     300,   303,   311,   312,   315,   326,   331,   334,   338,   343,
     350,   351,   352,   355,   356,   359,   364,   365,   368,   369,
     372,   373,   378,   379,   380,   383,   387,   393,   394,   397,
     400,   427,   428,   429,   434,   435,   438,   439,   444,   447,
     452,   457,   464,   472,   475,   480,   483,   486,   489,   499,
     500,   503,   506,   509,   514,   518,   524,   525,   528,   529,
     532,   536,   542,   552,   553,   556,   557,   558,   559,   562,
     563,   566,   571,   576,   577,   580,   583,   589,   603,   604,
     607,   612,   613,   616,   622,   625,   631,   635,   640,   662,
     689,   690,   693,   697,   703,   717,   718,   721,   725,   729,
     732,   735,   740,   743,   746,   747,   753,   763,   768,   778,
     783,   790,   793,   803,   809,   816,   819,   823,   827,   830,
     835,   836,   839,   842,   843,   844,   847,   850,   855,   858
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "FIELD", "STRINGTYPE", "QDEFAULT",
  "MATCHING", "ENUM", "MULTIENUMTOK", "VALUES", "DEFAULT", "EXACT_REGEXP",
  "INEXACT_REGEXP", "ALL", "FORMAT", "ENUMSEPARATORSTOK", "MULTITEXTTYPE",
  "DATETYPE", "ENUM_IN_FILE", "MULTI_ENUM_IN_FILE", "PATHTOK", "FIELDSTOK",
  "KEYTOK", "QSTRING", "INTVAL", "TEXTSEARCH", "QUERYTOK", "FORMATTOK",
  "INDEXTOK", "SEPARATORTOK", "RESTRICTEDTOK", "NOSPACESTOK", "INTEGERTOK",
  "INPUTDEFAULTTOK", "BUILTINTOK", "ALLOWANYVALUETOK", "REQUIRETOK",
  "APPENDFIELDTOK", "SETFIELDTOK", "CHANGETOK", "DESCRIPTIONTOK",
  "INPUTTOK", "DATABASEINFOTOK", "DEBUGMODETOK", "KEEPRECTOK",
  "NOTIFYEXPTOK", "LIBEXECDIRTOK", "SUBMITTERACKTOK", "BUSINESSDAYTOK",
  "BUSINESSWEEKTOK", "CREATECATEGORYDIRSTOK", "FALSETOK", "TRUETOK",
  "MAILFORMATTOK", "TOADDRESSESTOK", "FROMADDRESSTOK", "REPLYTOTOK",
  "FIXEDTOK", "BODYTOK", "HEADERTOK", "AUDITTRAILFMTTOK",
  "ADDAUDITTRAILTOK", "REQUIRECHANGEREASONTOK", "READONLYTOK",
  "BINARYINDEXTOK", "RAWTOK", "BADTOK", "AUXFLAGSTOK", "PRLISTTOK",
  "MAXPRSTOK", "EDITONLYTOK", "VIRTUALFORMATTOK", "CATPERMSTOK", "'{'",
  "'}'", "'-'", "'|'", "$accept", "config", "configEnts", "databaseInfo",
  "parseError", "databaseInfoList", "databaseInfoEnt", "booleanVal",
  "fieldDecStmt", "fieldDecList", "fieldDec", "startFieldDec",
  "fieldDataList", "fieldData", "virtualFieldFormat", "fieldDataType",
  "stringType", "regexpList", "enumType", "globalChangeEnts",
  "globalChangeEnt", "changeClause", "changeHeader", "optChangeExpr",
  "changeOpts", "changeOpt", "reqFieldNameList", "reqFieldNameEnt",
  "fieldEditOpts", "fieldEditName", "fieldEditFormat",
  "optFieldEditFieldList", "fieldEditFieldList", "optSimple", "prListOpts",
  "prListOptList", "enumcontents", "multienumcontents", "multiEnumItem",
  "enumItem", "enumValueList", "enumFileContents", "enumFileItem",
  "multiEnumFileContents", "multiEnumFileItem", "defaultFieldVal",
  "enumFieldList", "enumFieldMember", "queryDefault", "miscOptions",
  "auxFlagsList", "optQueryList", "queryList", "query", "queryBegin",
  "queryFmt", "queryFieldDesc", "optQueryOpts", "queryOpts", "queryPrintf",
  "queryfields", "queryFieldsList", "FieldListMember", "inputDescription",
  "inputEnt", "requiredFields", "requiredFieldsList", "inputFields",
  "inputFieldsList", "mailFormatList", "mailFormat", "mailFormatHeader",
  "mailFormatBody", "mailFormatEnt", "bodyFormat", "headerFormat",
  "plainFormat", "plainFormatHeader", "mailAddressList", "mailAddress",
  "mailAddressTries", "MailAddressMember", "auditTrailFmt",
  "dsDescription", "indexDescription", "indexheader", "indexlist",
  "indexEnt", "indexFieldList", "indexSep", 0
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
     325,   326,   327,   123,   125,    45,   124
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    77,    78,    78,    79,    80,    80,    80,    81,    82,
      82,    83,    83,    83,    83,    83,    83,    83,    83,    83,
      84,    84,    85,    85,    86,    86,    87,    87,    88,    89,
      89,    90,    90,    90,    90,    91,    92,    92,    92,    92,
      92,    92,    93,    93,    93,    94,    94,    95,    95,    95,
      95,    95,    95,    96,    96,    97,    98,    99,   100,   100,
     101,   101,   102,   102,   102,   102,   102,   102,   102,   103,
     103,   104,   105,   105,   106,   107,   108,   108,   109,   109,
     110,   110,   110,   111,   111,   112,   113,   113,   114,   114,
     115,   115,   116,   116,   116,   117,   117,   118,   118,   119,
     119,   119,   119,   119,   120,   120,   121,   121,   122,   122,
     123,   123,   124,   125,   125,   126,   126,   126,   126,   126,
     126,   126,   126,   126,   127,   127,   128,   128,   129,   129,
     130,   130,   131,   132,   132,   133,   133,   133,   133,   134,
     134,   135,   136,   137,   137,   138,   138,   139,   140,   140,
     141,   142,   142,   142,   143,   143,   144,   144,   145,   145,
     146,   146,   147,   147,   148,   149,   149,   150,   150,   150,
     150,   150,   151,   152,   153,   153,   154,   155,   155,   156,
     156,   157,   157,   158,   159,   160,   161,   161,   161,   162,
     163,   163,   164,   164,   164,   164,   165,   165,   166,   166
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     1,     8,     4,     4,     0,     1,     1,
       2,     2,     2,     2,     2,     2,     4,     4,     2,     2,
       1,     1,     1,     0,     1,     2,     4,     4,     2,     1,
       2,     1,     1,     1,     1,     2,     1,     1,     2,     1,
       2,     2,     1,     5,     5,     1,     2,     4,     4,     4,
       4,     4,     4,     0,     2,     3,     3,     3,     1,     0,
       1,     2,     4,     4,     2,     2,     1,     2,     1,     1,
       2,     1,     5,     4,     1,     1,     0,     1,     1,     2,
       0,     3,     3,     0,     3,     2,     1,     2,     1,     2,
       1,     2,     4,     4,     1,     1,     2,     1,     2,     2,
       6,     4,     1,     1,     1,     2,     1,     2,     2,     2,
       1,     2,     1,     2,     2,     1,     1,     1,     2,     1,
       2,     1,     4,     1,     1,     2,     0,     1,     1,     2,
       4,     4,     2,     2,     2,     2,     1,     2,     2,     0,
       1,     1,     2,     4,     4,     1,     2,     1,     4,     4,
       2,     0,     4,     4,     1,     2,     4,     4,     1,     2,
       1,     2,     4,     4,     2,     1,     2,     1,     1,     4,
       4,     4,     2,     2,     3,     3,     1,     1,     2,     2,
       1,     1,     3,     1,     2,     1,     4,     4,     1,     1,
       1,     2,     2,     4,     4,     1,     1,     2,     2,     2
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     8,     0,     0,     2,    23,     3,     0,     1,     0,
     126,    22,    24,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     9,    28,     0,     0,   127,
     128,     0,    25,     0,    20,    21,    11,    12,    13,    14,
      15,     0,     0,    18,    19,     6,     5,    10,   132,     0,
       0,   129,     0,    42,     0,     0,     0,    80,    39,     0,
       0,   115,   116,   117,    80,     0,    59,     0,   121,     0,
      83,   123,     0,     0,     0,    29,    34,    31,    36,    37,
     119,     0,    33,    32,     0,     0,   176,   184,     0,     0,
      53,   160,     0,     0,     0,   141,     0,     0,   139,     0,
       0,   136,     0,   113,   114,     0,     0,     0,    38,     0,
       0,    40,   118,    58,     0,   120,     0,     0,    41,    35,
      27,    26,    30,     0,     0,     0,     0,    66,    68,     0,
      60,    16,    17,     0,     0,   164,     0,   161,     0,   135,
       0,   142,   131,   130,   133,   140,   134,     0,   137,   138,
       0,     0,     0,     0,     0,     0,    86,    94,     0,     0,
      88,    90,     0,     0,     0,     0,   103,     0,     0,    97,
     102,     0,     0,    97,     0,   104,    57,   124,     0,     0,
       0,     0,    74,    65,     0,    64,    67,    56,    61,   175,
     174,   189,   188,    54,     0,     0,   185,     0,     0,     0,
       0,     0,     0,     0,     0,   165,   167,   168,   147,     0,
       0,   145,    45,     0,     0,     0,   108,   109,    51,    47,
      87,    91,    50,    89,    82,    81,    99,     0,    52,    48,
      98,   107,    98,   105,    49,   125,   122,    85,    84,    71,
       0,     0,    69,     0,     0,     0,     4,     0,     0,     0,
       0,   172,   173,   163,   162,   166,   144,   143,   146,    44,
      46,    43,    95,     0,     0,   112,     0,     0,   110,    63,
      62,    70,    75,     0,    76,    55,     0,     0,     0,     0,
       0,     0,     0,   190,   195,   183,     0,     0,   177,   180,
     181,     0,     0,    93,    96,    92,   101,     0,   111,    73,
      78,     0,    77,     0,     0,     0,   151,   192,     0,   198,
     199,   187,   186,   191,   179,   169,   178,     0,   170,   171,
       0,    72,    79,     0,   149,   148,     0,   150,     0,   196,
       0,   182,   100,   158,     0,     0,     0,   194,   193,   197,
     157,   159,   156,     0,   154,     0,   153,   152,   155
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     3,     4,     5,     6,    24,    25,    36,    10,    11,
      12,    13,    74,    75,    76,    77,    78,   214,    79,   136,
     193,    80,    81,   114,   129,   130,   241,   242,   183,   184,
     274,   301,   302,   108,   118,   180,   155,   159,   160,   161,
     264,   168,   169,   174,   175,   157,   267,   268,    82,    83,
     178,    28,    29,    30,    31,    97,    98,   144,    99,   100,
     101,   210,   211,   246,   305,   327,   345,   306,   335,    90,
      91,    92,   204,   205,   206,   207,    87,    88,   287,   288,
     289,   290,    50,   195,   196,   197,   282,   283,   330,   284
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -205
static const yytype_int16 yypact[] =
{
     116,  -205,   -53,    30,  -205,    43,  -205,     7,  -205,    93,
      79,    43,  -205,   -40,   -36,   -36,   -36,    99,   -36,   141,
     146,   -36,   150,   103,   179,  -205,  -205,   159,   126,    79,
    -205,   125,  -205,    57,  -205,  -205,  -205,  -205,  -205,  -205,
    -205,   130,   138,  -205,  -205,  -205,  -205,  -205,  -205,   134,
     162,  -205,    82,   246,   169,   185,   186,   187,  -205,   194,
     198,  -205,  -205,  -205,   187,   250,   251,   252,  -205,   203,
     205,  -205,   134,   206,   132,  -205,  -205,  -205,  -205,  -205,
    -205,   201,  -205,  -205,   255,   257,  -205,  -205,    82,   259,
     162,  -205,   210,    -3,   261,  -205,   212,   213,   220,    -4,
     267,   262,   217,  -205,  -205,   145,   239,   105,  -205,   211,
     235,  -205,  -205,  -205,   219,  -205,   268,   224,  -205,  -205,
    -205,  -205,  -205,   221,   272,   272,   134,  -205,  -205,   156,
    -205,  -205,  -205,   222,   223,  -205,    10,  -205,   129,  -205,
     166,  -205,  -205,  -205,  -205,  -205,  -205,   225,  -205,  -205,
     167,   226,   277,   279,   229,    33,  -205,  -205,   281,     3,
    -205,  -205,   231,   232,   284,   236,  -205,   234,     4,  -205,
    -205,   287,   235,   237,   238,  -205,  -205,  -205,    18,   289,
     240,   174,  -205,  -205,   244,  -205,  -205,  -205,  -205,  -205,
    -205,  -205,  -205,  -205,   201,   274,  -205,   245,   247,   248,
     249,   134,   134,   253,   -27,  -205,  -205,  -205,  -205,   254,
      21,  -205,  -205,   258,    25,   178,  -205,  -205,  -205,  -205,
    -205,  -205,  -205,  -205,  -205,  -205,  -205,   218,  -205,  -205,
    -205,  -205,   237,  -205,  -205,  -205,  -205,  -205,  -205,  -205,
     260,    37,  -205,   241,   173,   263,  -205,    92,    87,    87,
      87,  -205,  -205,  -205,  -205,  -205,  -205,  -205,  -205,  -205,
    -205,  -205,  -205,   264,    40,  -205,   265,    49,  -205,  -205,
    -205,  -205,  -205,   266,   296,  -205,   256,   300,   269,   301,
     -36,   270,   140,  -205,  -205,  -205,   302,    11,  -205,   271,
    -205,   275,    12,  -205,  -205,  -205,  -205,   304,  -205,  -205,
    -205,   276,   307,   273,   278,   280,   295,  -205,   166,  -205,
    -205,  -205,  -205,  -205,  -205,  -205,  -205,   310,  -205,  -205,
     312,  -205,  -205,   242,  -205,  -205,   282,  -205,   283,  -205,
      71,  -205,  -205,  -205,   285,    78,   166,  -205,  -205,  -205,
    -205,  -205,  -205,   286,  -205,    85,  -205,  -205,  -205
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -205,  -205,  -205,  -205,    -7,  -205,   313,   -14,  -205,  -205,
     330,  -205,  -205,   288,  -205,  -205,  -205,  -205,  -205,  -205,
    -205,  -205,   207,  -205,   151,  -126,  -205,   107,   228,  -205,
    -205,  -205,  -205,   292,  -205,  -205,  -205,  -205,   192,   -65,
    -205,   290,  -101,  -205,   189,   -88,  -205,    91,  -205,  -205,
    -205,  -205,  -205,   334,  -205,   291,   293,  -205,   297,   298,
     294,  -205,  -204,  -205,  -205,  -205,  -205,  -205,  -205,  -205,
     299,  -205,  -205,   160,  -205,  -205,   -67,  -205,   115,   -96,
    -205,    50,  -205,  -205,  -205,  -205,  -205,    84,  -205,  -205
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -107
static const yytype_int16 yytable[] =
{
      23,    37,    38,   188,    40,   119,   258,    43,     1,   173,
     139,     1,   151,   152,   152,    34,    35,    93,   158,   163,
       7,   170,   170,    94,   164,   165,    73,   198,   199,   200,
       8,   201,   202,    33,   285,   285,   153,   153,   191,   166,
     156,   235,   151,   152,   208,    96,     9,   254,   260,    66,
      14,    15,    16,    17,    18,    19,    20,    21,     1,   186,
     239,    53,    54,   294,    55,    56,   153,   230,   286,   286,
     140,   232,   265,    57,    58,    59,    60,   222,   229,    22,
     170,   133,    61,     1,   170,   315,   319,    62,    63,    64,
     220,    65,   236,     1,   208,   257,    66,    67,   154,   261,
     162,   341,   167,    93,   329,    27,     1,   219,   208,    94,
     285,   270,   277,   278,   295,   152,    26,     1,   188,    -7,
      68,   279,    39,   297,    69,    70,   339,    71,    72,   192,
       1,   203,   344,   209,   251,   252,    53,    54,   153,    55,
      56,   348,    -7,   213,   286,   338,     1,    95,    57,    58,
      59,    60,   342,   291,   151,   152,   280,    61,     2,   347,
     277,   278,    62,    63,    64,    41,    65,     1,     1,   279,
      42,    66,    67,    44,   240,     1,    -7,    45,   153,     1,
     103,   104,    48,   198,   199,   200,    49,   201,   202,   208,
     212,   316,   123,   124,   125,    68,   316,   239,    52,    69,
      70,   262,    71,    72,   280,    84,   121,    86,   263,   123,
     124,   125,     1,    85,   312,    89,   126,   127,   128,     1,
     266,   152,    14,    15,    16,    17,    18,    19,    20,    21,
     187,   164,   165,   126,   127,   128,   273,   123,   124,   125,
     281,   265,     1,     1,   153,   152,   166,   275,   151,   152,
     171,    22,   102,    46,   158,   164,   165,     1,   105,   106,
     107,   126,   127,   128,   272,   333,   310,   109,   153,   304,
     166,   110,   153,   112,   113,   115,   116,   303,   117,   131,
     120,   132,   135,   138,   141,    95,   142,   143,   147,    94,
     150,   177,   176,   179,   181,   182,   189,   190,   140,   215,
     216,   328,   217,   218,   221,   224,   225,   226,   228,   227,
     231,  -106,   234,   237,   238,   245,   334,   243,   247,   300,
     248,   249,   250,   307,   309,   314,   320,   253,   256,   343,
     322,   326,   259,   285,   269,   332,   276,    47,   293,   296,
     299,    32,   308,   194,   311,   244,   323,   317,   271,   318,
     321,   223,   324,   185,   325,   336,   111,   337,   298,   340,
     346,   233,   122,    51,   255,   292,   313,   331,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   134,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   137,
       0,     0,   146,     0,   148,   145,     0,     0,     0,   149,
     172
};

static const yytype_int16 yycheck[] =
{
       7,    15,    16,   129,    18,    72,   210,    21,     1,   110,
      13,     1,     9,    10,    10,    51,    52,    21,    15,   107,
      73,   109,   110,    27,    20,    21,    33,    54,    55,    56,
       0,    58,    59,    73,    23,    23,    33,    33,    28,    35,
     105,    23,     9,    10,    23,    52,     3,    74,    23,    39,
      43,    44,    45,    46,    47,    48,    49,    50,     1,   126,
      23,     4,     5,    23,     7,     8,    33,   168,    57,    57,
      73,   172,    23,    16,    17,    18,    19,    74,    74,    72,
     168,    88,    25,     1,   172,    74,    74,    30,    31,    32,
     155,    34,    74,     1,    23,    74,    39,    40,   105,    74,
     107,    23,   109,    21,   308,    26,     1,    74,    23,    27,
      23,    74,    20,    21,    74,    10,    23,     1,   244,     3,
      63,    29,    23,    74,    67,    68,   330,    70,    71,   136,
       1,   138,   336,   140,   201,   202,     4,     5,    33,     7,
       8,   345,    26,   150,    57,    74,     1,    65,    16,    17,
      18,    19,    74,   249,     9,    10,    64,    25,    42,    74,
      20,    21,    30,    31,    32,    24,    34,     1,     1,    29,
      24,    39,    40,    23,   181,     1,    60,    74,    33,     1,
      11,    12,    23,    54,    55,    56,    60,    58,    59,    23,
      23,   287,    36,    37,    38,    63,   292,    23,    73,    67,
      68,    23,    70,    71,    64,    75,    74,    73,   215,    36,
      37,    38,     1,    75,    74,    53,    60,    61,    62,     1,
     227,    10,    43,    44,    45,    46,    47,    48,    49,    50,
      74,    20,    21,    60,    61,    62,   243,    36,    37,    38,
     247,    23,     1,     1,    33,    10,    35,    74,     9,    10,
      15,    72,     6,    74,    15,    20,    21,     1,    73,    73,
      73,    60,    61,    62,    23,    23,   280,    73,    33,   276,
      35,    73,    33,    23,    23,    23,    73,    21,    73,    24,
      74,    24,    23,    73,    23,    65,    74,    74,    21,    27,
      73,    23,    73,    69,    73,    23,    74,    74,    73,    73,
      23,   308,    23,    74,    23,    74,    74,    23,    74,    73,
      23,    74,    74,    24,    74,    41,   323,    73,    73,    23,
      73,    73,    73,    23,    23,    23,    22,    74,    74,   336,
      23,    36,    74,    23,    74,    23,    73,    24,    74,    74,
      74,    11,    73,   136,    74,   194,    73,    76,   241,    74,
      74,   159,    74,   125,    74,    73,    64,    74,   267,    74,
      74,   172,    74,    29,   204,   250,   282,   317,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,    -1,    99,    -1,   100,    98,    -1,    -1,    -1,   101,
     110
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     1,    42,    78,    79,    80,    81,    73,     0,     3,
      85,    86,    87,    88,    43,    44,    45,    46,    47,    48,
      49,    50,    72,    81,    82,    83,    23,    26,   128,   129,
     130,   131,    87,    73,    51,    52,    84,    84,    84,    23,
      84,    24,    24,    84,    23,    74,    74,    83,    23,    60,
     159,   130,    73,     4,     5,     7,     8,    16,    17,    18,
      19,    25,    30,    31,    32,    34,    39,    40,    63,    67,
      68,    70,    71,    81,    89,    90,    91,    92,    93,    95,
      98,    99,   125,   126,    75,    75,    73,   153,   154,    53,
     146,   147,   148,    21,    27,    65,    81,   132,   133,   135,
     136,   137,     6,    11,    12,    73,    73,    73,   110,    73,
      73,   110,    23,    23,   100,    23,    73,    73,   111,   153,
      74,    74,    90,    36,    37,    38,    60,    61,    62,   101,
     102,    24,    24,    81,   132,    23,    96,   147,    73,    13,
      73,    23,    74,    74,   134,   135,   133,    21,   137,   136,
      73,     9,    10,    33,    81,   113,   116,   122,    15,   114,
     115,   116,    81,   122,    20,    21,    35,    81,   118,   119,
     122,    15,   118,   119,   120,   121,    73,    23,   127,    69,
     112,    73,    23,   105,   106,   105,   153,    74,   102,    74,
      74,    28,    81,    97,    99,   160,   161,   162,    54,    55,
      56,    58,    59,    81,   149,   150,   151,   152,    23,    81,
     138,   139,    23,    81,    94,    73,    23,    23,    74,    74,
     116,    23,    74,   115,    74,    74,    23,    73,    74,    74,
     119,    23,   119,   121,    74,    23,    74,    24,    74,    23,
      81,   103,   104,    73,   101,    41,   140,    73,    73,    73,
      73,   153,   153,    74,    74,   150,    74,    74,   139,    74,
      23,    74,    23,    81,   117,    23,    81,   123,   124,    74,
      74,   104,    23,    81,   107,    74,    73,    20,    21,    29,
      64,    81,   163,   164,   166,    23,    57,   155,   156,   157,
     158,   156,   155,    74,    23,    74,    74,    74,   124,    74,
      23,   108,   109,    21,    81,   141,   144,    23,    73,    23,
      84,    74,    74,   164,    23,    74,   156,    76,    74,    74,
      22,    74,    23,    73,    74,    74,    36,   142,    81,   139,
     165,   158,    23,    23,    81,   145,    73,    74,    74,   139,
      74,    23,    74,    81,   139,   143,    74,    74,   139
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
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
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


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*-------------------------.
| yyparse or yypush_parse.  |
`-------------------------*/

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

       Refer to the stacks thru separate pointers, to allow yyoverflow
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
  int yytoken;
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

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
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
  if (yyn == YYPACT_NINF)
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
      if (yyn == 0 || yyn == YYTABLE_NINF)
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
        case 7:

/* Line 1455 of yacc.c  */
#line 75 "fconfig.y"
    {
		    fconferror ("Missing/bad database info section");
		}
    break;

  case 8:

/* Line 1455 of yacc.c  */
#line 80 "fconfig.y"
    { badFile = 1; }
    break;

  case 11:

/* Line 1455 of yacc.c  */
#line 86 "fconfig.y"
    { 
		    setDebugMode (databaseBeingDefined, (yyvsp[(2) - (2)].intval));
		}
    break;

  case 12:

/* Line 1455 of yacc.c  */
#line 89 "fconfig.y"
    {
		    setKeepReceivedHeaders (databaseBeingDefined, (yyvsp[(2) - (2)].intval));
		}
    break;

  case 13:

/* Line 1455 of yacc.c  */
#line 92 "fconfig.y"
    {
		    setNotifyExpire (databaseBeingDefined, (yyvsp[(2) - (2)].intval));
		}
    break;

  case 14:

/* Line 1455 of yacc.c  */
#line 95 "fconfig.y"
    {
		    setBinDir (databaseBeingDefined, qStrVal ((yyvsp[(2) - (2)].qstr))); 
		}
    break;

  case 15:

/* Line 1455 of yacc.c  */
#line 98 "fconfig.y"
    {
		    setSubmitterAck (databaseBeingDefined, (yyvsp[(2) - (2)].intval)); 
		}
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 101 "fconfig.y"
    {
		  setBusinessDay (databaseBeingDefined, (yyvsp[(2) - (4)].intval), (yyvsp[(4) - (4)].intval));
		}
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 104 "fconfig.y"
    {
		    setBusinessWeek(databaseBeingDefined,(yyvsp[(2) - (4)].intval), (yyvsp[(4) - (4)].intval));
		}
    break;

  case 18:

/* Line 1455 of yacc.c  */
#line 107 "fconfig.y"
    {
		    setCreateCategoryDirs (databaseBeingDefined, (yyvsp[(2) - (2)].intval));
		}
    break;

  case 19:

/* Line 1455 of yacc.c  */
#line 110 "fconfig.y"
    {
		    setCategoryDirPerms (databaseBeingDefined, qStrVal ((yyvsp[(2) - (2)].qstr)));
		}
    break;

  case 20:

/* Line 1455 of yacc.c  */
#line 115 "fconfig.y"
    { (yyval.intval) = 0; }
    break;

  case 21:

/* Line 1455 of yacc.c  */
#line 116 "fconfig.y"
    { (yyval.intval) = 1; }
    break;

  case 23:

/* Line 1455 of yacc.c  */
#line 120 "fconfig.y"
    {
		    fconferror ("Missing/bad field declarations");
		}
    break;

  case 26:

/* Line 1455 of yacc.c  */
#line 129 "fconfig.y"
    {
		    currField = NULL;
		}
    break;

  case 27:

/* Line 1455 of yacc.c  */
#line 132 "fconfig.y"
    {
		    currField = NULL;
		}
    break;

  case 28:

/* Line 1455 of yacc.c  */
#line 137 "fconfig.y"
    {
		    char *fname = takeQString ((yyvsp[(2) - (2)].qstr));
		    currField = newFieldDef (databaseBeingDefined, fname);
		    if (currField == NULL)
		      {
			char *msg;
			asprintf (&msg, "Duplicate field definition for %s\n",
				  fname);
			fconferror (msg);
			free (msg);
		      }
		    currField->default_value = NULL;
		}
    break;

  case 35:

/* Line 1455 of yacc.c  */
#line 162 "fconfig.y"
    {
		    currField->virtualFormat = qformat;
		    qformat = NULL;
		}
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 170 "fconfig.y"
    {
		    currField->datatype = MultiText;
		    currField->defaultSearchType = NilSearch;
		}
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 174 "fconfig.y"
    {
		  currField->datatype = Date;
		  currField->defaultSearchType = LessThan;
		}
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 178 "fconfig.y"
    {
		    currField->datatype = Integer;
		    currField->defaultSearchType = NilSearch;
		}
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 182 "fconfig.y"
    {
		    currField->datatype = PRListType;
		    currField->defaultSearchType = RegCmp;
		}
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 188 "fconfig.y"
    {
		    currField->datatype = Text;
		}
    break;

  case 43:

/* Line 1455 of yacc.c  */
#line 191 "fconfig.y"
    {
		    currField->datatype = TextWithRegex;
		}
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 197 "fconfig.y"
    {
		    (yyval.stringlist) = new_string_list_ent (takeQString ((yyvsp[(1) - (1)].qstr)), NULL);
		    currField->regex = (yyval.stringlist);
		}
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 201 "fconfig.y"
    {
		    (yyvsp[(1) - (2)].stringlist)->next = new_string_list_ent (takeQString ((yyvsp[(2) - (2)].qstr)), NULL);
		    (yyval.stringlist) = (yyvsp[(1) - (2)].stringlist)->next;
		}
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 207 "fconfig.y"
    {
		    currField->datatype = Enum;
		    currField->defaultSearchType = RegCmp;
		}
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 211 "fconfig.y"
    {
		    currField->datatype = Enum;
		    currField->defaultSearchType = RegCmp;
		    initAdmField (currField);
		}
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 216 "fconfig.y"
    {
		    currField->datatype = MultiEnum;
		    currField->defaultSearchType = RegCmp;
		    initAdmField (currField);
		}
    break;

  case 50:

/* Line 1455 of yacc.c  */
#line 221 "fconfig.y"
    {
		   currField->datatype = MultiEnum;
		   currField->defaultSearchType = RegCmp;
		   if (currField->multiEnumSeparator == NULL)
		     {
		       currField->multiEnumSeparator
			 = xstrdup (DEFAULT_MULTIENUM_SEPARATOR);
		     }
		}
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 238 "fconfig.y"
    {
		    addGlobalChangeActions (databaseBeingDefined, currChange);
		    currChange = NULL;
		}
    break;

  case 56:

/* Line 1455 of yacc.c  */
#line 244 "fconfig.y"
    {
		    ChangeActions *p = &(currField->changeActions);
		    while (*p != NULL)
		      {
			p = &((*p)->next);
		      }
		    *p = currChange;
		    currChange = NULL;
		}
    break;

  case 57:

/* Line 1455 of yacc.c  */
#line 255 "fconfig.y"
    {
		    currChange = newChangeAction (databaseBeingDefined, (yyvsp[(2) - (3)].sval));
		    if ((yyvsp[(2) - (3)].sval) != NULL) 
		      {
			free ((yyvsp[(2) - (3)].sval));
		      }
		}
    break;

  case 58:

/* Line 1455 of yacc.c  */
#line 264 "fconfig.y"
    {
		    (yyval.sval) = takeQString ((yyvsp[(1) - (1)].qstr));
		}
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 267 "fconfig.y"
    {
		    (yyval.sval) = NULL;
		}
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 278 "fconfig.y"
    {
		    currChange->edits = currEdit;
		    currEdit = NULL;
		}
    break;

  case 65:

/* Line 1455 of yacc.c  */
#line 282 "fconfig.y"
    {
		    currEdit->append = 1;
		    currChange->edits = currEdit;
		    currEdit = NULL;
		}
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 287 "fconfig.y"
    {
		    currChange->addAuditTrail = 1;
		}
    break;

  case 67:

/* Line 1455 of yacc.c  */
#line 290 "fconfig.y"
    {
		    currChange->auditTrailFormat = qformat;
		    qformat = NULL;
		}
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 294 "fconfig.y"
    {
		    currChange->requireChangeReason = 1;
		}
    break;

  case 71:

/* Line 1455 of yacc.c  */
#line 303 "fconfig.y"
    {
		    FieldList foo 
		      = newFieldListEnt (databaseBeingDefined, qStrVal ((yyvsp[(1) - (1)].qstr)),
					 currChange->requiredFields);
		    currChange->requiredFields = foo;
		}
    break;

  case 74:

/* Line 1455 of yacc.c  */
#line 315 "fconfig.y"
    {
		    currEdit = (FieldEdit *) xmalloc (sizeof (FieldEdit));
		    currEdit->expr = NULL;
		    currEdit->fieldToEditName = takeQString ((yyvsp[(1) - (1)].qstr));
		    currEdit->append = 0;
		    currEdit->textFormat = NULL;
		    currEdit->fieldsForFormat = NULL;
		    currEdit->next = NULL;
		}
    break;

  case 75:

/* Line 1455 of yacc.c  */
#line 326 "fconfig.y"
    {
		    currEdit->textFormat = takeQString ((yyvsp[(1) - (1)].qstr));
		}
    break;

  case 76:

/* Line 1455 of yacc.c  */
#line 331 "fconfig.y"
    {
		    currEdit->fieldsForFormat = NULL;
		}
    break;

  case 77:

/* Line 1455 of yacc.c  */
#line 334 "fconfig.y"
    {
		}
    break;

  case 78:

/* Line 1455 of yacc.c  */
#line 338 "fconfig.y"
    {
		    (yyval.flist) = newFieldListEnt (databaseBeingDefined, qStrVal ((yyvsp[(1) - (1)].qstr)),
					  NULL);
		    currEdit->fieldsForFormat = (yyval.flist);
		}
    break;

  case 79:

/* Line 1455 of yacc.c  */
#line 343 "fconfig.y"
    {
		    (yyval.flist) = newFieldListEnt (databaseBeingDefined, qStrVal ((yyvsp[(2) - (2)].qstr)),
					  NULL);
		    (yyvsp[(1) - (2)].flist)->next = (yyval.flist);
		}
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 359 "fconfig.y"
    {
		    currField->maxPrsPerLine = (yyvsp[(2) - (2)].intval);
		}
    break;

  case 91:

/* Line 1455 of yacc.c  */
#line 373 "fconfig.y"
    {
		   currField->multiEnumSeparator = takeQString ((yyvsp[(2) - (2)].qstr));
		}
    break;

  case 95:

/* Line 1455 of yacc.c  */
#line 383 "fconfig.y"
    {
		    (yyval.stringlist) = new_string_list_ent (takeQString ((yyvsp[(1) - (1)].qstr)), NULL);
		    currField->enumValues = (yyval.stringlist);
		}
    break;

  case 96:

/* Line 1455 of yacc.c  */
#line 387 "fconfig.y"
    {
		    (yyvsp[(1) - (2)].stringlist)->next = new_string_list_ent (takeQString ((yyvsp[(2) - (2)].qstr)), NULL);
		    (yyval.stringlist) = (yyvsp[(1) - (2)].stringlist)->next;
		}
    break;

  case 99:

/* Line 1455 of yacc.c  */
#line 397 "fconfig.y"
    {
		    currField->adm_db_name = takeQString ((yyvsp[(2) - (2)].qstr));
		}
    break;

  case 100:

/* Line 1455 of yacc.c  */
#line 400 "fconfig.y"
    {
		    AdmFieldDesc *p;
		    int which = 0;

		    for (p = currField->adm_field_des; p != NULL; p = p->next)
		      {
			if (strcmp (p->name, qStrVal ((yyvsp[(6) - (6)].qstr))) == 0)
			  {
			    break;
			  }
			which++;
		      }

		    if (p != NULL)
		      {
			currField->key_field = which;
		      }
		    else
		      {
			char *msg;

			asprintf (&msg, "Invalid adm subfield %s\n",
				  qStrVal ((yyvsp[(6) - (6)].qstr)));
			fconferror (msg);
			free (msg);
		      }
		}
    break;

  case 103:

/* Line 1455 of yacc.c  */
#line 429 "fconfig.y"
    {
		    currField->allow_any_value = 1;
		}
    break;

  case 107:

/* Line 1455 of yacc.c  */
#line 439 "fconfig.y"
    {
		   currField->multiEnumSeparator = takeQString ((yyvsp[(2) - (2)].qstr));
		}
    break;

  case 108:

/* Line 1455 of yacc.c  */
#line 444 "fconfig.y"
    {
		    currField->default_value = takeQString ((yyvsp[(2) - (2)].qstr));
		}
    break;

  case 109:

/* Line 1455 of yacc.c  */
#line 447 "fconfig.y"
    {
		    currField->input_default_value = takeQString ((yyvsp[(2) - (2)].qstr));
		}
    break;

  case 110:

/* Line 1455 of yacc.c  */
#line 452 "fconfig.y"
    {
		    currField->adm_db_fields = 1;
		    currField->adm_field_des = (yyvsp[(1) - (1)].adm_field_des);
		    (yyval.adm_field_des) = (yyvsp[(1) - (1)].adm_field_des);
		}
    break;

  case 111:

/* Line 1455 of yacc.c  */
#line 457 "fconfig.y"
    {
		    (yyvsp[(1) - (2)].adm_field_des)->next = (yyvsp[(2) - (2)].adm_field_des);
		    (yyval.adm_field_des) = (yyvsp[(2) - (2)].adm_field_des);
		    currField->adm_db_fields++;
		}
    break;

  case 112:

/* Line 1455 of yacc.c  */
#line 464 "fconfig.y"
    {
		    (yyval.adm_field_des) = (AdmFieldDesc *) 
		      xmalloc (sizeof (AdmFieldDesc));
		    (yyval.adm_field_des)->name = takeQString ((yyvsp[(1) - (1)].qstr));
		    (yyval.adm_field_des)->next = NULL;
		}
    break;

  case 113:

/* Line 1455 of yacc.c  */
#line 472 "fconfig.y"
    {
		    currField->defaultSearchType = RegCmp;
		}
    break;

  case 114:

/* Line 1455 of yacc.c  */
#line 475 "fconfig.y"
    {
		    currField->defaultSearchType = RegFind;
		}
    break;

  case 115:

/* Line 1455 of yacc.c  */
#line 480 "fconfig.y"
    {
		    currField->textsearch = 1;
		}
    break;

  case 116:

/* Line 1455 of yacc.c  */
#line 483 "fconfig.y"
    {
		    currField->restricted = 1;
		}
    break;

  case 117:

/* Line 1455 of yacc.c  */
#line 486 "fconfig.y"
    {
		     currField->nospaces = 1;
		}
    break;

  case 118:

/* Line 1455 of yacc.c  */
#line 489 "fconfig.y"
    {
		    if (setBuiltinField (currField, qStrVal ((yyvsp[(2) - (2)].qstr))) != 0)
		      {
			char *msg;
			asprintf (&msg, "Invalid builtin fieldname %s",
				  qStrVal ((yyvsp[(2) - (2)].qstr)));
			fconferror (msg);
			free (msg);
		      }
		}
    break;

  case 120:

/* Line 1455 of yacc.c  */
#line 500 "fconfig.y"
    {
		    currField->description = takeQString ((yyvsp[(2) - (2)].qstr));
		}
    break;

  case 121:

/* Line 1455 of yacc.c  */
#line 503 "fconfig.y"
    {
		    currField->readonly = 1;
		}
    break;

  case 122:

/* Line 1455 of yacc.c  */
#line 506 "fconfig.y"
    {
		    currField->auxFlags = (yyvsp[(3) - (4)].stringlist);
		}
    break;

  case 123:

/* Line 1455 of yacc.c  */
#line 509 "fconfig.y"
    {
		    currField->editonly = 1;
		}
    break;

  case 124:

/* Line 1455 of yacc.c  */
#line 514 "fconfig.y"
    {
		    (yyval.stringlist) = new_string_list_ent (takeQString ((yyvsp[(1) - (1)].qstr)), NULL);
		    currField->auxFlags = (yyval.stringlist);
		}
    break;

  case 125:

/* Line 1455 of yacc.c  */
#line 518 "fconfig.y"
    {
		    (yyvsp[(1) - (2)].stringlist)->next = new_string_list_ent (takeQString ((yyvsp[(2) - (2)].qstr)), NULL);
		    (yyval.stringlist) = (yyvsp[(1) - (2)].stringlist)->next;
		}
    break;

  case 130:

/* Line 1455 of yacc.c  */
#line 532 "fconfig.y"
    {
		    addQueryFormat (databaseBeingDefined, qformat);
		    qformat = NULL;
		}
    break;

  case 131:

/* Line 1455 of yacc.c  */
#line 536 "fconfig.y"
    {
		    freeQueryFormat (qformat);
		    qformat = NULL;
		}
    break;

  case 132:

/* Line 1455 of yacc.c  */
#line 542 "fconfig.y"
    {
		    qformat = (QueryFormat *) xmalloc (sizeof (QueryFormat));
		    qformat->name = takeQString ((yyvsp[(2) - (2)].qstr));
		    qformat->printf = NULL;
		    qformat->separator = NULL;
		    qformat->fields = NULL;
		    qformat->next = NULL;
		}
    break;

  case 141:

/* Line 1455 of yacc.c  */
#line 566 "fconfig.y"
    {
		    qformat->rawQuery = 1;
		}
    break;

  case 142:

/* Line 1455 of yacc.c  */
#line 571 "fconfig.y"
    {
		    qformat->printf = takeQString ((yyvsp[(2) - (2)].qstr));
		}
    break;

  case 145:

/* Line 1455 of yacc.c  */
#line 580 "fconfig.y"
    {
		    qformat->fields = (yyvsp[(1) - (1)].flist);
		}
    break;

  case 146:

/* Line 1455 of yacc.c  */
#line 583 "fconfig.y"
    {
		    (yyvsp[(1) - (2)].flist)->next = (yyvsp[(2) - (2)].flist);
		    (yyval.flist) = (yyvsp[(2) - (2)].flist);
		}
    break;

  case 147:

/* Line 1455 of yacc.c  */
#line 589 "fconfig.y"
    {
		    (yyval.flist) = newFieldListEnt (databaseBeingDefined, qStrVal ((yyvsp[(1) - (1)].qstr)),
					  NULL);
		    if (parseComplexFieldIndex ((yyval.flist)->ent) != 0)
		      {
			char *msg;

			asprintf (&msg, "Field %s is invalid\n", qStrVal ((yyvsp[(1) - (1)].qstr)));
			fconferror (msg);
			free (msg);
		      }
		}
    break;

  case 150:

/* Line 1455 of yacc.c  */
#line 607 "fconfig.y"
    {
		    setInputTemplate (databaseBeingDefined, (yyvsp[(1) - (2)].inputlist));
		}
    break;

  case 152:

/* Line 1455 of yacc.c  */
#line 613 "fconfig.y"
    {
		    setRequiredInputFields (databaseBeingDefined, requiredFlds);
                }
    break;

  case 153:

/* Line 1455 of yacc.c  */
#line 616 "fconfig.y"
    {
                    freeFieldList (requiredFlds);
                    requiredFlds = NULL;
                }
    break;

  case 154:

/* Line 1455 of yacc.c  */
#line 622 "fconfig.y"
    {
		    requiredFlds = (yyvsp[(1) - (1)].flist);
                }
    break;

  case 155:

/* Line 1455 of yacc.c  */
#line 625 "fconfig.y"
    {
                    (yyvsp[(1) - (2)].flist)->next = (yyvsp[(2) - (2)].flist);
                    (yyval.flist) = (yyvsp[(2) - (2)].flist);
                }
    break;

  case 156:

/* Line 1455 of yacc.c  */
#line 631 "fconfig.y"
    {
		    (yyval.inputlist) = inputTemplate;
		    inputTemplate = NULL;
		}
    break;

  case 157:

/* Line 1455 of yacc.c  */
#line 635 "fconfig.y"
    {
		    (yyval.inputlist) = NULL;
		}
    break;

  case 158:

/* Line 1455 of yacc.c  */
#line 640 "fconfig.y"
    {
		     (yyval.inputlist) = (InputTemplate *) 
		       xmalloc (sizeof (InputTemplate));
		     (yyval.inputlist)->index = find_field_index (databaseBeingDefined,
						   qStrVal ((yyvsp[(1) - (1)].qstr)));
		     if ((yyval.inputlist)->index == InvalidFieldIndex)
		       {
			 char *msg;

			 asprintf (&msg, "Field %s is invalid\n",
				   qStrVal ((yyvsp[(1) - (1)].qstr)));
			 fconferror (msg);
			 free ((yyval.inputlist));
			 free (msg);
			 inputTemplate = NULL;
		       }
		     else
		       {
			 inputTemplate = (yyval.inputlist);
			 (yyval.inputlist)->next = NULL;
		       }
		}
    break;

  case 159:

/* Line 1455 of yacc.c  */
#line 662 "fconfig.y"
    {
		     (yyval.inputlist) = (InputTemplate *)
		       xmalloc (sizeof (InputTemplate));
		     (yyval.inputlist)->index = find_field_index (databaseBeingDefined,
						   qStrVal ((yyvsp[(2) - (2)].qstr)));
		     if ((yyval.inputlist)->index == InvalidFieldIndex)
		       {
			 char *msg;

			 asprintf (&msg, "Field %s is invalid\n",
				   qStrVal ((yyvsp[(2) - (2)].qstr)));
			 fconferror (msg);
			 free (msg);
			 free ((yyval.inputlist));
		       }
		     else
		       {
			 (yyval.inputlist)->next = NULL;
			 (yyvsp[(1) - (2)].inputlist)->next = (yyval.inputlist);
			 if (inputTemplate == NULL)
			   {
			     inputTemplate = (yyval.inputlist);
			   }
		       }
		}
    break;

  case 162:

/* Line 1455 of yacc.c  */
#line 693 "fconfig.y"
    {
		    addMessageFormat (databaseBeingDefined, mailFormat);
		    mailFormat = NULL;
		}
    break;

  case 163:

/* Line 1455 of yacc.c  */
#line 697 "fconfig.y"
    {
		    freeMessageFormat (mailFormat);
		    mailFormat = NULL;
		}
    break;

  case 164:

/* Line 1455 of yacc.c  */
#line 703 "fconfig.y"
    {
  		    mailFormat 
		      = (MailMessageFormat)
		      xmalloc (sizeof (struct mail_message_format));
		    mailFormat->name = takeQString ((yyvsp[(2) - (2)].qstr));
		    mailFormat->toAddresses = NULL;
		    mailFormat->fromAddress = NULL;
		    mailFormat->replyTo = NULL;
		    mailFormat->body = NULL;
		    mailFormat->header = NULL;
		    mailFormat->next = NULL;
		}
    break;

  case 167:

/* Line 1455 of yacc.c  */
#line 721 "fconfig.y"
    {
		    mailFormat->body = qformat;
		    qformat = NULL;
		}
    break;

  case 168:

/* Line 1455 of yacc.c  */
#line 725 "fconfig.y"
    {
		    mailFormat->header = qformat;
		    qformat = NULL;
		}
    break;

  case 169:

/* Line 1455 of yacc.c  */
#line 729 "fconfig.y"
    {
		    mailFormat->toAddresses = (yyvsp[(3) - (4)].mailaddrlist);
		}
    break;

  case 170:

/* Line 1455 of yacc.c  */
#line 732 "fconfig.y"
    {
		    mailFormat->fromAddress = (yyvsp[(3) - (4)].mailaddr);
		}
    break;

  case 171:

/* Line 1455 of yacc.c  */
#line 735 "fconfig.y"
    {
		  mailFormat->replyTo = (yyvsp[(3) - (4)].mailaddrlist);
		}
    break;

  case 175:

/* Line 1455 of yacc.c  */
#line 747 "fconfig.y"
    {
		    freeQueryFormat (qformat);
		    qformat = NULL;
		}
    break;

  case 176:

/* Line 1455 of yacc.c  */
#line 753 "fconfig.y"
    {
		    qformat = (QueryFormat *) xmalloc (sizeof (QueryFormat));
		    qformat->name = NULL;
		    qformat->printf = NULL;
		    qformat->separator = NULL;
		    qformat->fields = NULL;
		    qformat->next = NULL;
		}
    break;

  case 177:

/* Line 1455 of yacc.c  */
#line 763 "fconfig.y"
    {
		   (yyval.mailaddrlist) = (MailAddressList *) xmalloc (sizeof (MailAddressList));
		   (yyval.mailaddrlist)->address = (yyvsp[(1) - (1)].mailaddr);
		   (yyval.mailaddrlist)->next = NULL;
		}
    break;

  case 178:

/* Line 1455 of yacc.c  */
#line 768 "fconfig.y"
    {
		   MailAddressList *lp = (yyval.mailaddrlist);
		   while (lp->next != NULL) { lp = lp->next; }
		   lp->next =
		     (MailAddressList *) xmalloc (sizeof (MailAddressList));
		   lp->next->address = (yyvsp[(2) - (2)].mailaddr);
		   lp->next->next = NULL;
		}
    break;

  case 179:

/* Line 1455 of yacc.c  */
#line 778 "fconfig.y"
    {
		    (yyval.mailaddr) = (MailAddress *) xmalloc (sizeof (MailAddress));
		    (yyval.mailaddr)->fixedAddress = takeQString ((yyvsp[(2) - (2)].qstr));
		    (yyval.mailaddr)->addresses = NULL;
		}
    break;

  case 180:

/* Line 1455 of yacc.c  */
#line 783 "fconfig.y"
    {
		    (yyval.mailaddr) = (MailAddress *) xmalloc (sizeof (MailAddress));
		    (yyval.mailaddr)->fixedAddress = NULL;
		    (yyval.mailaddr)->addresses = (yyvsp[(1) - (1)].flist);
		}
    break;

  case 181:

/* Line 1455 of yacc.c  */
#line 790 "fconfig.y"
    {
		    (yyval.flist) = (yyvsp[(1) - (1)].flist);
		}
    break;

  case 182:

/* Line 1455 of yacc.c  */
#line 793 "fconfig.y"
    {
		    FieldList p = (yyval.flist);
		    while (p->next != NULL) 
		      {
			p = p->next;
		      }
		    p->next = (yyvsp[(3) - (3)].flist);
		}
    break;

  case 183:

/* Line 1455 of yacc.c  */
#line 803 "fconfig.y"
    {
		    (yyval.flist) = newFieldListEnt (databaseBeingDefined, qStrVal ((yyvsp[(1) - (1)].qstr)),
					  NULL);
		}
    break;

  case 184:

/* Line 1455 of yacc.c  */
#line 809 "fconfig.y"
    {
		    setAuditTrailFormat (databaseBeingDefined, qformat);
		}
    break;

  case 186:

/* Line 1455 of yacc.c  */
#line 819 "fconfig.y"
    {
		    setIndexDesc (databaseBeingDefined, indexEntry);
		    indexEntry = NULL;
		}
    break;

  case 187:

/* Line 1455 of yacc.c  */
#line 823 "fconfig.y"
    {
		    freeIndexDesc (indexEntry);
		    indexEntry = NULL;
		}
    break;

  case 189:

/* Line 1455 of yacc.c  */
#line 830 "fconfig.y"
    {
		    indexEntry = newIndexDesc (databaseBeingDefined);
		}
    break;

  case 192:

/* Line 1455 of yacc.c  */
#line 839 "fconfig.y"
    {
		    setIndexDescPath (indexEntry, qStrVal ((yyvsp[(2) - (2)].qstr)));
		}
    break;

  case 196:

/* Line 1455 of yacc.c  */
#line 847 "fconfig.y"
    {
		    addFieldToIndex (indexEntry, (yyvsp[(1) - (1)].flist));
		}
    break;

  case 197:

/* Line 1455 of yacc.c  */
#line 850 "fconfig.y"
    {
		    addFieldToIndex (indexEntry, (yyvsp[(2) - (2)].flist));
		}
    break;

  case 198:

/* Line 1455 of yacc.c  */
#line 855 "fconfig.y"
    {
		    setIndexDescSeparator (indexEntry, takeQString ((yyvsp[(2) - (2)].qstr)));
		}
    break;

  case 199:

/* Line 1455 of yacc.c  */
#line 858 "fconfig.y"
    {
		    setIndexDescBinary (indexEntry, (yyvsp[(2) - (2)].intval));
		}
    break;



/* Line 1455 of yacc.c  */
#line 3099 "fconfig.c"
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

#if !defined(yyoverflow) || YYERROR_VERBOSE
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



