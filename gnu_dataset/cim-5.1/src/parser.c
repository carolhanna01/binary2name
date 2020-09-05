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




/* Copy the first part of user declarations.  */
#line 1 "../../src/parser.y"

/* $Id: cparser.y,v 1.9 1994/10/15 13:30:36 cim Exp $ */

/* Copyright (C) 1994, 1998 Sverre Hvammen Johansen,
 * Department of Informatics, University of Oslo.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */

/* Beskrivelse til YACC som genererer en parser. */

/* Konstanter til bruk */
#include "const.h"
#include "dekl.h"
#include "cimcomp.h"
#include "lex.h"
#include "name.h"
#include "mellbuilder.h"
#include <stdio.h>
#include <obstack.h>
char *xmalloc();
void yyerror (char s[]);
#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

static struct obstack os_block;
static char *first_object_allocated_ptr_block;

#define EXTDECLARATION 0
#define DECLARATION 1
#define STATEMENT 2
#define EMPTYSTATEMENT 3

static char type, kind, categ;

char *Ckind;
char *varargsid;
char *activateid;
char *simsetident,*simulationident,*fileident,*imagefileident,
     *outfileident,*infileident,*directfileident,*printfileident,
     *bytefileident,*inbytefileident,*outbytefileident,*directbytefileident;

extern char sensitive;
char ysensitive;

/* DEBUGING KAN KANSKJE \NSKES */
/* DA MAA YYDEBUG SETTES LIK 1 */
#ifdef DEBUB
#define YYDEBUG
#endif
#if YACC_YYOVERFLOW
#define yyoverflow
#endif

/* Mulighet for utskrift av symboler fra lex */
#define yylex ylex


/* Kontakt med lex */

/* Makroer for registrering av blokker */
typedef struct _blockstack blockstack_t;
struct _blockstack
{
  char rem;
  blockstack_t *prev;
}  *blockp;

#define OBSBLOCK() { blockstack_t *prev= blockp;\
     blockp= (blockstack_t *) \
       obstack_alloc (&os_block,sizeof (blockstack_t));\
     blockp->prev= prev;\
     blockp->rem=TRUE;}

#define MBEENEWBLOCK() if (blockp->rem == TRUE) {\
       mout(MBLOCK);\
       blockp->rem= FALSE;begin_block(KBLOKK);}

#define MBEEENDBLOCK() { blockstack_t *prev= blockp->prev;\
     if(blockp->rem==FALSE)\
       {mout(MENDBLOCK);end_block(NULL,CCNO);}\
     obstack_free (&os_block, blockp);\
     blockp= prev;}

#define STOPOBSBLOCK() if(blockp->rem==TRUE)blockp->rem=MAYBEE

/* Spesifikasjonene til YACC */


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
/* Line 193 of yacc.c.  */
#line 402 "../../src/parser.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 415 "../../src/parser.c"

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
#define YYLAST   1135

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  99
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  124
/* YYNRULES -- Number of rules.  */
#define YYNRULES  262
/* YYNRULES -- Number of states.  */
#define YYNSTATES  443

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   353

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
      95,    96,    97,    98
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     4,     7,    11,    12,    18,    19,    20,
      28,    29,    34,    35,    36,    46,    47,    50,    52,    56,
      59,    60,    61,    65,    66,    68,    70,    71,    77,    79,
      81,    83,    86,    88,    90,    93,    94,    95,    99,   101,
     105,   108,   109,   112,   117,   120,   122,   124,   125,   129,
     130,   136,   137,   144,   145,   146,   150,   152,   154,   155,
     156,   161,   162,   166,   168,   170,   172,   174,   175,   177,
     178,   184,   185,   186,   194,   195,   196,   205,   208,   209,
     210,   217,   219,   220,   225,   226,   227,   235,   242,   247,
     249,   253,   254,   259,   260,   267,   268,   269,   279,   280,
     287,   289,   290,   291,   293,   294,   301,   305,   306,   311,
     312,   318,   320,   324,   330,   334,   336,   338,   342,   344,
     348,   350,   354,   359,   361,   365,   367,   369,   373,   377,
     379,   383,   384,   385,   386,   387,   398,   399,   401,   406,
     407,   409,   411,   414,   415,   420,   422,   426,   431,   432,
     434,   437,   439,   443,   447,   450,   453,   456,   459,   460,
     461,   462,   469,   471,   473,   474,   479,   480,   482,   484,
     486,   488,   491,   494,   497,   500,   503,   506,   510,   514,
     518,   522,   526,   530,   531,   536,   537,   542,   543,   548,
     549,   551,   553,   556,   560,   561,   569,   572,   577,   584,
     586,   589,   591,   593,   594,   595,   603,   604,   607,   608,
     610,   614,   619,   621,   623,   626,   629,   630,   632,   636,
     638,   642,   645,   650,   651,   652,   656,   658,   665,   669,
     673,   678,   683,   687,   691,   695,   699,   702,   706,   710,
     714,   717,   721,   725,   729,   733,   735,   737,   739,   741,
     743,   745,   746,   750,   753,   757,   761,   765,   766,   770,
     771,   775,   777
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
     100,     0,    -1,    -1,   101,   163,    -1,     1,    65,   161,
      -1,    -1,    18,   116,    42,   103,   111,    -1,    -1,    -1,
      18,    77,    42,   104,    77,   105,   107,    -1,    -1,    18,
      11,   106,   111,    -1,    -1,    -1,   113,    91,   108,   116,
      42,    77,   109,   171,   110,    -1,    -1,     8,    16,    -1,
     112,    -1,   111,    63,   112,    -1,    77,   113,    -1,    -1,
      -1,    93,   114,    82,    -1,    -1,   115,    -1,   117,    -1,
      -1,    47,    66,    77,   118,    67,    -1,    51,    -1,     9,
      -1,    10,    -1,    48,    29,    -1,    29,    -1,    46,    -1,
      33,    46,    -1,    -1,    -1,    15,   120,   160,    -1,   122,
      -1,   122,    63,   121,    -1,   217,   123,    -1,    -1,    60,
     217,    -1,    49,   217,    55,   217,    -1,    21,    54,    -1,
      22,    -1,   127,    -1,    -1,    14,   126,   160,    -1,    -1,
      59,    77,    14,   128,   160,    -1,    -1,   127,    59,    77,
      14,   129,   160,    -1,    -1,    -1,    40,   131,   160,    -1,
       3,    -1,    45,    -1,    -1,    -1,   136,   217,   134,   138,
      -1,    -1,   137,   135,   217,    -1,     6,    -1,    13,    -1,
       7,    -1,     4,    -1,    -1,    41,    -1,    -1,    60,   217,
      14,   140,   160,    -1,    -1,    -1,    25,   217,    52,   141,
     160,   142,   119,    -1,    -1,    -1,    19,    77,    83,   143,
     121,    14,   144,   160,    -1,   124,   217,    -1,    -1,    -1,
      28,   217,   145,   125,   146,   130,    -1,    27,    -1,    -1,
      77,    64,   147,   156,    -1,    -1,    -1,   218,     8,   148,
     155,   149,   161,    16,    -1,   218,     8,     1,    65,   161,
      16,    -1,   218,     8,     1,    16,    -1,   218,    -1,   132,
     217,   133,    -1,    -1,     8,   150,   161,    16,    -1,    -1,
     116,    42,    77,   151,   171,   160,    -1,    -1,    -1,    77,
      11,   115,   152,   155,    77,   153,   171,   160,    -1,    -1,
      11,   115,    77,   154,   171,   160,    -1,   102,    -1,    -1,
      -1,   139,    -1,    -1,   117,    77,   215,    63,   157,   214,
      -1,   117,    77,   215,    -1,    -1,   116,     5,   158,   164,
      -1,    -1,    50,    77,    83,   159,   170,    -1,   156,    -1,
       8,   161,    16,    -1,     8,     1,    65,   161,    16,    -1,
       8,     1,    16,    -1,   162,    -1,   156,    -1,   162,    65,
     156,    -1,   139,    -1,   163,    65,   139,    -1,   165,    -1,
     164,    63,   165,    -1,   166,    66,   168,    67,    -1,   167,
      -1,   167,    63,   166,    -1,    77,    -1,   169,    -1,   169,
      63,   168,    -1,   217,    64,   217,    -1,   217,    -1,   217,
      63,   170,    -1,    -1,    -1,    -1,    -1,   176,    65,   172,
     191,   173,   199,   174,   208,   175,   211,    -1,    -1,   177,
      -1,    66,   115,   178,    67,    -1,    -1,   179,    -1,    77,
      -1,   185,    76,    -1,    -1,    77,   180,    63,   179,    -1,
     184,    -1,   184,    63,   179,    -1,    66,   115,   182,    67,
      -1,    -1,   183,    -1,   185,    76,    -1,   184,    -1,   184,
      63,   179,    -1,   185,   203,    77,    -1,   185,   186,    -1,
      35,    64,    -1,    56,    64,    -1,    57,    64,    -1,    -1,
      -1,    -1,   116,    42,    77,   187,   181,   188,    -1,    77,
      -1,    76,    -1,    -1,    77,   190,    63,   189,    -1,    -1,
     192,    -1,   193,    -1,   197,    -1,   195,    -1,   193,   197,
      -1,   197,   193,    -1,   193,   195,    -1,   195,   193,    -1,
     197,   195,    -1,   195,   197,    -1,   195,   193,   197,    -1,
     193,   195,   197,    -1,   193,   197,   195,    -1,   195,   197,
     193,    -1,   197,   195,   193,    -1,   197,   193,   195,    -1,
      -1,    35,   194,   189,    65,    -1,    -1,    57,   196,   189,
      65,    -1,    -1,    56,   198,   189,    65,    -1,    -1,   200,
      -1,   201,    -1,   200,   201,    -1,   203,   213,    65,    -1,
      -1,   115,    42,    77,    91,   202,   204,    65,    -1,   186,
      65,    -1,   116,    42,    77,    65,    -1,   116,    42,    77,
      63,   213,    65,    -1,   117,    -1,   116,     5,    -1,    31,
      -1,    50,    -1,    -1,    -1,   116,    42,    77,   205,   171,
     206,   207,    -1,    -1,     8,    16,    -1,    -1,   209,    -1,
     210,   213,    65,    -1,   209,   210,   213,    65,    -1,    24,
      -1,    43,    -1,    24,    43,    -1,    43,    24,    -1,    -1,
     212,    -1,    58,    64,   199,    -1,    77,    -1,   213,    63,
      77,    -1,    77,   215,    -1,   214,    63,    77,   215,    -1,
      -1,    -1,    93,   216,   217,    -1,   218,    -1,    25,   217,
      52,   217,    15,   217,    -1,   218,    83,   217,    -1,   218,
      12,   218,    -1,   218,    88,    15,   218,    -1,   218,    89,
      52,   218,    -1,   218,    86,   218,    -1,   218,    87,   218,
      -1,   218,    88,   218,    -1,   218,    89,   218,    -1,    90,
     218,    -1,   218,    93,   218,    -1,   218,    92,   218,    -1,
     218,    91,   218,    -1,    94,   218,    -1,   218,    94,   218,
      -1,   218,    96,   218,    -1,   218,    97,   218,    -1,    66,
     217,    67,    -1,    82,    -1,    80,    -1,    81,    -1,    79,
      -1,    78,    -1,    38,    -1,    -1,    77,   219,   221,    -1,
      53,    77,    -1,    37,    77,   220,    -1,   218,    98,   218,
      -1,   218,    44,    77,    -1,    -1,    66,   222,    67,    -1,
      -1,    66,   222,    67,    -1,   217,    -1,   217,    63,   222,
      -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   177,   177,   177,   180,   185,   182,   192,   199,   189,
     205,   203,   212,   215,   210,   238,   239,   241,   242,   244,
     248,   249,   249,   254,   256,   257,   261,   259,   264,   265,
     266,   267,   268,   269,   270,   274,   283,   283,   286,   289,
     294,   297,   298,   300,   306,   308,   310,   311,   311,   318,
     316,   326,   323,   332,   333,   333,   337,   339,   342,   349,
     349,   351,   351,   356,   358,   361,   363,   366,   368,   374,
     372,   380,   382,   378,   387,   390,   385,   397,   401,   405,
     400,   408,   412,   410,   422,   424,   420,   430,   433,   437,
     440,   453,   452,   459,   457,   467,   470,   464,   480,   477,
     487,   488,   490,   521,   526,   522,   531,   538,   537,   543,
     541,   550,   551,   552,   553,   555,   558,   559,   565,   568,
     580,   581,   585,   591,   594,   598,   604,   607,   612,   616,
     618,   622,   623,   624,   625,   622,   629,   630,   632,   635,
     636,   638,   639,   640,   640,   642,   643,   646,   649,   650,
     652,   653,   654,   657,   659,   661,   663,   665,   667,   671,
     675,   669,   678,   679,   680,   680,   683,   684,   686,   687,
     688,   689,   690,   691,   692,   693,   694,   695,   696,   697,
     698,   699,   700,   702,   702,   706,   706,   710,   710,   713,
     714,   716,   717,   719,   721,   720,   723,   724,   726,   730,
     731,   733,   735,   740,   744,   738,   748,   749,   751,   752,
     754,   756,   759,   760,   761,   763,   766,   767,   769,   773,
     774,   777,   780,   785,   787,   786,   798,   799,   807,   813,
     816,   820,   824,   826,   828,   830,   832,   833,   844,   849,
     854,   858,   863,   869,   872,   874,   876,   878,   880,   882,
     884,   886,   885,   888,   890,   894,   897,   901,   902,   905,
     908,   912,   914
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "HACTIVATE", "HAFTER", "HARRAY", "HAT",
  "HBEFORE", "HBEGIN", "HBOOLEAN", "HCHARACTER", "HCLASS", "HCONC",
  "HDELAY", "HDO", "HELSE", "HEND", "HEQ", "HEXTERNAL", "HFOR", "HGE",
  "HGO", "HGOTO", "HGT", "HHIDDEN", "HIF", "HIN", "HINNER", "HINSPECT",
  "HINTEGER", "HIS", "HLABEL", "HLE", "HLONG", "HLT", "HNAME", "HNE",
  "HNEW", "HNONE", "HNOTEXT", "HOTHERWISE", "HPRIOR", "HPROCEDURE",
  "HPROTECTED", "HQUA", "HREACTIVATE", "HREAL", "HREF", "HSHORT", "HSTEP",
  "HSWITCH", "HTEXT", "HTHEN", "HTHIS", "HTO", "HUNTIL", "HVALUE", "HVAR",
  "HVIRTUAL", "HWHEN", "HWHILE", "HASSIGNVALUE", "HASSIGNREF",
  "HPAREXPSEPARATOR", "HLABELSEPARATOR", "HSTATEMENTSEPARATOR", "HBEGPAR",
  "HENDPAR", "HEQR", "HNER", "HADD", "HSUB", "HMUL", "HDIV", "HINTDIV",
  "HEXP", "HDOTDOTDOT", "HIDENTIFIER", "HBOOLEANKONST", "HINTEGERKONST",
  "HCHARACTERKONST", "HREALKONST", "HTEXTKONST", "HASSIGN", "HORELSE",
  "HANDTHEN", "HEQV", "HIMP", "HOR", "HAND", "HNOT", "HOBJRELOPERATOR",
  "HREFRELOPERATOR", "HVALRELOPERATOR", "HTERMOPERATOR", "UNEAR",
  "HFACTOROPERATOR", "HPRIMARYOPERATOR", "HDOT", "$accept", "MAIN_MODULE",
  "@1", "EXT_DECLARATION", "@2", "@3", "@4", "@5", "EXTERNAL_KIND_ITEM",
  "@6", "@7", "EMPTY_BLOCK", "EXT_LIST", "EXT_ITEM", "EXT_IDENT", "@8",
  "NO_TYPE", "MBEE_TYPE", "TYPE", "@9", "MBEE_ELSE_PART", "@10",
  "FOR_LIST", "FOR_LIST_ELEMENT", "MBEE_F_L_EL_R_PT", "GOTO",
  "CONN_STATE_R_PT", "@11", "WHEN_CLAUSE_LIST", "@12", "@13",
  "MBEE_OTWI_CLAUS", "@14", "ACTIVATOR", "SCHEDULE", "@15", "@16",
  "ATDELAY", "BEFOREAFTER", "PRIOR", "MODULSTATEMENT", "@17", "@18", "@19",
  "@20", "@21", "@22", "@23", "@24", "@25", "@26", "@27", "@28", "@29",
  "@30", "@31", "IMPORT_SPEC_MODULE", "DECLSTATEMENT", "@32", "@33", "@34",
  "BLOCK", "MBEE_DECLSTMS", "MBEE_DECLSTMSU", "MODULS", "ARR_SEGMENT_LIST",
  "ARR_SEGMENT", "ARRAY_SEGMENT", "ARRAY_SEGMENT_EL", "BAUND_PAIR_LIST",
  "BAUND_PAIR", "SWITCH_LIST", "HEADING", "@35", "@36", "@37", "@38",
  "MBEE_FMAL_PAR_P", "FMAL_PAR_PART", "MBEE_LISTV", "LISTV", "@39",
  "FPP_HEADING", "FPP_MBEE_LISTV", "FPP_LISTV", "FPP_SPEC", "FPP_CATEG",
  "FPP_PROC_DECL_IN_SPEC", "@40", "@41", "IDENTIFIER_LISTV", "@42",
  "MBEE_MODE_PART", "MODE_PART", "NAME_PART", "@43", "VAR_PART", "@44",
  "VALUE_PART", "@45", "MBEE_SPEC_PART", "SPEC_PART", "ONE_SPEC", "@46",
  "SPECIFIER", "PROC_DECL_IN_SPEC", "@47", "@48", "MBEE_BEGIN_END",
  "MBEE_PROT_PART", "PROTECTION_PART", "PROT_SPECIFIER", "MBEE_VIRT_PART",
  "VIRTUAL_PART", "IDENTIFIER_LIST", "IDENTIFIER_LISTC", "MBEE_CONSTANT",
  "@49", "EXPRESSION", "EXPRESSION_SIMP", "@50", "ARG_R_PT",
  "MBEE_ARG_R_PT", "ARGUMENT_LIST", 0
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
     345,   346,   347,   348,   349,   350,   351,   352,   353
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    99,   101,   100,   100,   103,   102,   104,   105,   102,
     106,   102,   108,   109,   107,   110,   110,   111,   111,   112,
     113,   114,   113,   115,   116,   116,   118,   117,   117,   117,
     117,   117,   117,   117,   117,   119,   120,   119,   121,   121,
     122,   123,   123,   123,   124,   124,   125,   126,   125,   128,
     127,   129,   127,   130,   131,   130,   132,   132,   133,   134,
     133,   135,   133,   136,   136,   137,   137,   138,   138,   140,
     139,   141,   142,   139,   143,   144,   139,   139,   145,   146,
     139,   139,   147,   139,   148,   149,   139,   139,   139,   139,
     139,   150,   139,   151,   139,   152,   153,   139,   154,   139,
     139,   139,   155,   156,   157,   156,   156,   158,   156,   159,
     156,   160,   160,   160,   160,   161,   162,   162,   163,   163,
     164,   164,   165,   166,   166,   167,   168,   168,   169,   170,
     170,   172,   173,   174,   175,   171,   176,   176,   177,   178,
     178,   179,   179,   180,   179,   179,   179,   181,   182,   182,
     183,   183,   183,   184,   184,   185,   185,   185,   185,   187,
     188,   186,   189,   189,   190,   189,   191,   191,   192,   192,
     192,   192,   192,   192,   192,   192,   192,   192,   192,   192,
     192,   192,   192,   194,   193,   196,   195,   198,   197,   199,
     199,   200,   200,   201,   202,   201,   201,   201,   201,   203,
     203,   203,   203,   205,   206,   204,   207,   207,   208,   208,
     209,   209,   210,   210,   210,   210,   211,   211,   212,   213,
     213,   214,   214,   215,   216,   215,   217,   217,   218,   218,
     218,   218,   218,   218,   218,   218,   218,   218,   218,   218,
     218,   218,   218,   218,   218,   218,   218,   218,   218,   218,
     218,   219,   218,   218,   218,   218,   218,   220,   220,   221,
     221,   222,   222
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     3,     0,     5,     0,     0,     7,
       0,     4,     0,     0,     9,     0,     2,     1,     3,     2,
       0,     0,     3,     0,     1,     1,     0,     5,     1,     1,
       1,     2,     1,     1,     2,     0,     0,     3,     1,     3,
       2,     0,     2,     4,     2,     1,     1,     0,     3,     0,
       5,     0,     6,     0,     0,     3,     1,     1,     0,     0,
       4,     0,     3,     1,     1,     1,     1,     0,     1,     0,
       5,     0,     0,     7,     0,     0,     8,     2,     0,     0,
       6,     1,     0,     4,     0,     0,     7,     6,     4,     1,
       3,     0,     4,     0,     6,     0,     0,     9,     0,     6,
       1,     0,     0,     1,     0,     6,     3,     0,     4,     0,
       5,     1,     3,     5,     3,     1,     1,     3,     1,     3,
       1,     3,     4,     1,     3,     1,     1,     3,     3,     1,
       3,     0,     0,     0,     0,    10,     0,     1,     4,     0,
       1,     1,     2,     0,     4,     1,     3,     4,     0,     1,
       2,     1,     3,     3,     2,     2,     2,     2,     0,     0,
       0,     6,     1,     1,     0,     4,     0,     1,     1,     1,
       1,     2,     2,     2,     2,     2,     2,     3,     3,     3,
       3,     3,     3,     0,     4,     0,     4,     0,     4,     0,
       1,     1,     2,     3,     0,     7,     2,     4,     6,     1,
       2,     1,     1,     0,     0,     7,     0,     2,     0,     1,
       3,     4,     1,     1,     2,     2,     0,     1,     3,     1,
       3,     2,     4,     0,     0,     3,     1,     6,     3,     3,
       4,     4,     3,     3,     3,     3,     2,     3,     3,     3,
       2,     3,     3,     3,     3,     1,     1,     1,     1,     1,
       1,     0,     3,     2,     3,     3,     3,     0,     3,     0,
       3,     1,     3
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       0,     0,     0,   101,    23,     1,    56,    91,    29,    30,
      23,    23,     0,     0,    45,     0,    81,     0,    32,     0,
       0,   250,    57,    33,     0,     0,    28,     0,     0,     0,
     251,   249,   248,   246,   247,   245,     0,     0,   100,    24,
       0,    25,     0,     0,   118,     3,    89,     0,     0,    25,
     103,   116,     4,   115,    23,     0,    10,     0,     0,     0,
      44,     0,   251,     0,   226,    78,    34,   257,     0,    31,
     253,     0,     0,    23,    82,   259,   236,   240,     0,    77,
      58,   101,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   107,   223,
     101,     0,    98,     0,     7,     5,    74,     0,    71,     0,
       0,   254,    26,    69,   244,    95,   101,     0,   252,    93,
      66,    63,    65,    64,    90,     0,    61,   119,     0,   102,
     229,   256,   228,   232,   233,     0,   234,     0,   235,   239,
     238,   237,   241,   242,   243,   255,   109,     0,   224,   106,
     117,    92,   136,    20,    11,    17,     0,     0,     0,     0,
     101,    47,     0,    79,    46,   261,     0,     0,   101,   102,
      83,     0,   136,    59,     0,    88,    23,    85,   230,   231,
       0,   125,   108,   120,     0,   123,     0,   104,    23,   101,
       0,   137,    21,    19,     0,     8,     6,     0,    38,    41,
       0,     0,   111,    72,   101,     0,    53,     0,     0,   258,
      27,    70,     0,   260,   101,    67,    62,     0,    23,   110,
     129,     0,     0,     0,   225,     0,   158,    99,   131,     0,
      18,    20,    75,     0,     0,     0,    40,     0,     0,     0,
      35,    48,    49,    54,    80,     0,   262,    96,    94,    68,
      60,    87,     0,     0,   121,     0,   126,     0,   124,   223,
     105,     0,     0,     0,   141,     0,   140,   145,    23,   166,
      22,     9,     0,   101,    39,     0,    42,   227,   114,    23,
     112,    36,    73,   101,   101,    51,   136,    86,   130,   122,
       0,     0,   221,     0,   155,   156,   157,     0,   138,   158,
     201,   202,   142,     0,    25,   154,     0,   183,   187,   185,
     132,   167,   168,   170,   169,    12,    76,     0,     0,   101,
      50,    55,   101,   101,   127,   128,   223,   158,   146,   200,
       0,   153,     0,     0,     0,   189,   173,   171,   174,   176,
     172,   175,    23,    43,   113,    37,    52,    97,   222,   144,
     159,   163,   162,     0,     0,     0,    24,     0,     0,   133,
     190,   191,     0,   178,   179,   177,   180,   182,   181,     0,
       0,     0,   184,   188,   186,     0,     0,   196,   208,   192,
     219,     0,     0,    23,   160,     0,     0,   159,   212,   213,
     134,   209,     0,     0,   193,    13,   158,   161,   165,   194,
       0,   197,   214,   215,   216,     0,     0,   220,   136,     0,
     149,   151,    23,    23,     0,     0,   135,   217,     0,   210,
      15,   147,   158,   150,     0,     0,   198,   189,   211,     0,
      14,   152,     0,   195,   218,    16,   203,   136,   204,   206,
       0,   205,   207
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,     3,    38,   157,   156,   231,   103,   271,   342,
     408,   430,   154,   155,   193,   229,    39,    48,    49,   167,
     282,   319,   197,   198,   236,    42,   163,   204,   164,   283,
     322,   244,   284,    43,   124,   215,   174,   125,   126,   250,
      50,   168,   160,   240,   158,   273,   109,   206,   116,   129,
     218,    54,   172,   169,   286,   152,   177,   202,   225,   147,
     180,   203,    52,    53,    45,   182,   183,   184,   185,   255,
     256,   219,   189,   269,   335,   378,   404,   190,   191,   265,
     266,   297,   384,   409,   410,   267,   268,   358,   370,   397,
     353,   371,   310,   311,   312,   332,   313,   334,   314,   333,
     359,   360,   361,   413,   362,   425,   437,   439,   441,   390,
     391,   392,   416,   417,   381,   260,   149,   186,   165,    64,
      75,   111,   118,   166
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -316
static const yytype_int16 yypact[] =
{
     419,   -40,    59,  1035,   507,  -316,  -316,  -316,  -316,  -316,
    -316,   354,   -34,    60,  -316,   396,  -316,   396,  -316,    76,
      53,  -316,  -316,  -316,    40,   111,  -316,    73,   396,   396,
       3,  -316,  -316,  -316,  -316,  -316,   572,   572,  -316,  -316,
     119,  -316,   396,   396,  -316,   108,   166,    86,    15,    87,
    -316,  -316,  -316,   123,   771,   117,  -316,   156,   158,   122,
    -316,   396,  -316,   154,   284,  -316,  -316,   141,   132,  -316,
    -316,   197,   145,  -316,  -316,   149,   146,    99,   139,  -316,
     265,  1035,   595,   572,   143,   396,   572,   572,    31,  1041,
     572,   572,   572,   572,   572,   572,   572,   138,  -316,   129,
     859,   207,  -316,   147,  -316,  -316,  -316,   175,  -316,     1,
     396,  -316,  -316,  -316,  -316,  -316,   859,   396,  -316,  -316,
    -316,  -316,  -316,  -316,  -316,   396,  -316,  -316,     6,  -316,
      88,  -316,  -316,   256,   217,   572,   835,   572,   146,     5,
       5,     5,    99,    29,   -23,  -316,  -316,   153,  -316,   169,
    -316,  -316,   167,   163,   171,  -316,   159,   147,   396,   396,
     947,  -316,   174,  -316,   215,   212,   209,   210,   947,  -316,
    -316,   213,   167,  -316,   396,  -316,   771,  -316,   201,   201,
     396,  -316,   218,  -316,   216,   222,   396,  -316,  -316,   947,
     226,  -316,  -316,  -316,   147,  -316,   171,   287,   239,   -25,
     292,   683,  -316,  -316,   947,   298,   276,   240,   396,  -316,
    -316,  -316,   241,  -316,   947,   279,  -316,   305,   771,  -316,
     261,   153,   396,   153,  -316,   248,    98,  -316,  -316,   245,
    -316,   163,  -316,   396,   396,   396,  -316,   396,    17,   313,
     316,  -316,  -316,  -316,  -316,   319,  -316,  -316,  -316,  -316,
    -316,  -316,   320,   396,  -316,   268,   274,   278,  -316,   129,
     275,   282,   291,   294,   277,   289,  -316,   297,   473,    -6,
    -316,  -316,   270,   947,  -316,   307,  -316,  -316,  -316,   771,
    -316,  -316,  -316,   947,   947,  -316,   167,  -316,  -316,  -316,
     396,   396,  -316,   302,  -316,  -316,  -316,   303,  -316,    89,
    -316,  -316,  -316,    35,   308,  -316,   309,  -316,  -316,  -316,
    -316,  -316,   -18,    23,   -12,  -316,  -316,   396,   352,   947,
    -316,  -316,   947,   947,  -316,  -316,   129,    89,  -316,  -316,
     311,  -316,   -11,   -11,   -11,   708,   328,   312,   328,   355,
     312,   355,   649,  -316,  -316,  -316,  -316,  -316,  -316,  -316,
    -316,  -316,   326,   327,   329,   330,   349,    43,   331,  -316,
     708,  -316,   321,  -316,  -316,  -316,  -316,  -316,  -316,   351,
     333,   334,  -316,  -316,  -316,   336,   337,  -316,   105,  -316,
    -316,   -31,   338,  -316,  -316,   -11,   315,    52,   360,   380,
    -316,   105,   321,   339,  -316,  -316,    -4,  -316,  -316,  -316,
     321,  -316,  -316,  -316,   350,   321,    55,  -316,   167,   340,
    -316,   362,   532,   649,    84,   353,  -316,  -316,   104,  -316,
     415,  -316,    89,  -316,   390,   370,  -316,   708,  -316,   420,
    -316,  -316,   365,  -316,  -316,  -316,  -316,   167,  -316,   431,
     427,  -316,  -316
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -316,  -316,  -316,  -316,  -316,  -316,  -316,  -316,  -316,  -316,
    -316,  -316,   288,   257,   219,  -316,    -9,    -3,    -1,  -316,
    -316,  -316,   220,  -316,  -316,  -316,  -316,  -316,  -316,  -316,
    -316,  -316,  -316,  -316,  -316,  -316,  -316,  -316,  -316,  -316,
      24,  -316,  -316,  -316,  -316,  -316,  -316,  -316,  -316,  -316,
    -316,  -316,  -316,  -316,  -316,  -316,   285,     7,  -316,  -316,
    -316,  -142,   -48,  -316,  -316,  -316,   234,   235,  -316,   170,
    -316,   206,  -167,  -316,  -316,  -316,  -316,  -316,  -316,  -316,
    -283,  -316,  -316,  -316,  -316,    67,    72,  -261,  -316,  -316,
    -315,  -316,  -316,  -316,  -122,  -316,  -178,  -316,  -110,  -316,
      42,  -316,   120,  -316,  -256,  -316,  -316,  -316,  -316,  -316,
    -316,    80,  -316,  -316,  -248,  -316,  -250,  -316,    13,     0,
    -316,  -316,  -316,  -104
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -200
static const yytype_int16 yytable[] =
{
      40,    55,    41,    46,    46,   214,   101,   305,    58,   292,
      41,    51,   306,   171,    73,   161,   328,    83,   354,   355,
      98,    84,   175,   307,   234,     4,   211,    44,    63,   307,
      65,   261,   393,   278,   394,   235,    76,    77,   308,   309,
     329,    71,    72,    59,   349,   309,   135,   227,   329,    84,
     308,   309,   262,   263,    46,    79,    80,    78,   307,     5,
     162,    51,   241,  -148,   115,   351,   352,    74,    20,    21,
     398,   176,   248,    84,   107,    96,   348,   330,    40,   308,
      41,    46,   279,   130,    27,   376,   133,   134,   136,   138,
     139,   140,   141,   142,   143,   144,   145,    29,   132,    93,
      46,    94,    95,    96,   246,   127,    68,   150,    62,    31,
      32,    33,    34,    35,    60,   400,    46,   401,   393,   323,
     419,    36,    66,   170,   261,    37,    95,    96,   217,   388,
      67,   316,    84,   261,   336,   178,   341,   179,   173,   431,
      69,   320,   321,    84,   406,   262,   263,   393,   389,   426,
      70,   305,   414,   239,   262,   263,   306,   418,    83,   364,
      46,    78,   367,    97,    99,  -139,   264,   393,    46,   428,
     252,   199,   200,    81,    82,   264,    46,   345,    83,   226,
     346,   347,    93,    51,    94,    95,    96,   216,   100,    46,
      84,   338,   340,   220,   102,    94,    95,    96,   104,   224,
     105,    46,   337,   339,    46,   106,   108,   110,    51,   112,
      84,   113,   114,    83,    46,   117,   119,   366,    46,   368,
     131,   146,   148,   151,   153,    51,   363,   159,   365,    83,
     181,   318,   187,   188,   194,   257,   195,    90,    91,    92,
      93,   420,    94,    95,    96,    84,   199,   275,   276,    85,
     277,   205,    86,    87,    88,    89,   192,    90,    91,    92,
      93,    84,    94,    95,    96,   303,   220,   304,    83,   120,
     438,   121,   122,    46,   207,   208,   209,   210,   123,    46,
     213,   221,   222,    46,    46,   223,    51,    86,    87,    88,
      89,   228,    90,    91,    92,    93,    83,    94,    95,    96,
      84,   232,   233,   257,   325,    88,    89,   237,    90,    91,
      92,    93,   242,    94,    95,    96,   243,   245,   247,    46,
     249,   251,    46,    46,   253,   259,   356,   270,    84,   280,
     343,   281,   357,   285,   304,   289,   287,   290,   293,   369,
    -143,    41,   291,    87,    88,    89,   294,    90,    91,    92,
      93,   356,    94,    95,    96,   295,   298,   357,   296,   304,
     299,   315,   317,     8,     9,    56,   327,    85,   344,   309,
      86,    87,    88,    89,   396,    90,    91,    92,    93,   326,
      94,    95,    96,    18,   308,  -199,   331,    19,   350,  -164,
     307,   375,   372,   382,   373,   374,   377,   385,   380,   383,
      23,    24,    25,   402,   403,    26,   399,   421,   415,   303,
     424,   304,    41,   386,   387,   395,   407,   427,   356,    -2,
       1,    61,    -2,   429,   357,   422,   304,    -2,    -2,    -2,
      -2,    57,   432,    20,    21,   433,   435,    -2,    -2,   440,
      -2,    -2,   436,   442,    -2,   196,    -2,    -2,    -2,    27,
     272,   230,    -2,   274,   212,   254,    -2,    -2,   258,   288,
     324,    -2,    29,   411,    -2,    -2,    -2,    -2,   412,   434,
      -2,   405,    -2,    62,    31,    32,    33,    34,    35,    -2,
     379,     0,     8,     9,    -2,    -2,    36,     0,     0,     0,
      37,     0,     0,     0,     0,     0,    -2,    -2,    -2,    -2,
      -2,    -2,    18,     0,   300,     0,    19,  -101,     0,    -2,
       6,     0,     0,    -2,     0,     7,     8,     9,    10,    23,
      24,    25,     0,   301,    26,    11,    12,     0,    13,    14,
       0,     0,    15,     0,    16,    17,    18,     0,     0,     0,
      19,     8,     9,     0,    20,    21,     0,     0,     0,   302,
       0,     0,    22,    23,    24,    25,     0,    47,    26,     0,
      27,    18,     0,   300,     0,    19,     0,    28,     0,     0,
       0,     0,  -101,    29,     0,     0,     0,     0,    23,    24,
      25,     0,   301,    26,    30,    31,    32,    33,    34,    35,
       0,     0,     0,     0,     0,     0,   128,    36,   -84,     0,
     -84,    37,     0,   -84,   -84,   -84,   -84,     0,   423,    20,
      21,   -84,     0,   -84,   -84,     0,   -84,   -84,     0,     0,
     -84,     0,   -84,   -84,   -84,    27,     0,     0,   -84,     0,
       0,     0,   -84,   -84,     0,     0,     0,   -84,    29,     0,
     -84,   -84,   -84,   -84,     0,   -84,   -84,     0,   -84,    62,
      31,    32,    33,    34,    35,   -84,     0,     0,     8,     9,
     -84,   -84,    36,     0,     0,     0,    37,     0,     0,     0,
       0,     0,   -84,   -84,   -84,   -84,   -84,   -84,    18,     0,
       0,     0,    19,     0,   238,   -84,     6,     0,   -23,   -84,
       0,     7,     8,     9,    10,    23,    24,    25,     0,   -91,
      26,    11,    12,     0,    13,    14,     0,     0,    15,     0,
      16,    17,    18,   -23,     0,     0,    19,     8,     9,     0,
      20,    21,     0,     0,     0,   -23,     0,     0,    22,    23,
      24,    25,     0,    47,    26,     0,    27,    18,     0,   300,
       0,    19,     0,    28,     0,     0,     0,     0,   -91,    29,
     -23,     0,     0,     0,    23,    24,    25,     0,   301,    26,
      30,    31,    32,    33,    34,    35,     0,     0,     0,     0,
       0,     0,     0,    36,     6,     0,     0,    37,     0,     7,
       8,     9,    10,     0,     0,     0,     0,  -101,     0,    11,
      12,     0,    13,    14,     0,     0,    15,     0,    16,    17,
      18,     0,     0,     0,    19,     0,     0,     0,    20,    21,
       0,     0,     0,     0,     0,     0,    22,    23,    24,    25,
       0,    47,    26,     0,    27,     0,     0,     0,     0,     0,
       0,    28,     0,     0,     0,     0,  -101,    29,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    83,    30,    31,
      32,    33,    34,    35,     0,     0,     0,     0,     0,     0,
       0,    36,     6,     0,   -23,    37,     0,     7,     8,     9,
      10,     0,     0,     0,     0,     0,     0,    11,    12,    84,
      13,    14,     0,     0,    15,     0,    16,    17,    18,     0,
       0,     0,    19,     0,     0,     0,    20,    21,     0,     0,
       0,   -23,     0,     0,    22,    23,    24,    25,     0,    47,
      26,     0,    27,     0,     0,     0,     0,     0,     0,    28,
       0,     0,     0,     0,    89,    29,    90,    91,    92,    93,
       0,    94,    95,    96,     0,     0,    30,    31,    32,    33,
      34,    35,     0,     0,     0,     0,     0,     0,     0,    36,
       6,     0,   -23,    37,     0,   201,     8,     9,    10,     0,
       0,     0,     0,     0,     0,    11,    12,     0,    13,    14,
       0,     0,    15,     0,    16,    17,    18,     0,     0,     0,
      19,     0,     0,     0,    20,    21,     0,     0,     0,   -23,
       0,     0,    22,    23,    24,    25,     0,    47,    26,     0,
      27,     0,     0,     0,     0,     0,     0,    28,     0,     0,
       0,     0,     0,    29,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    30,    31,    32,    33,    34,    35,
       0,     0,     0,     0,     0,     0,     0,    36,     6,     0,
       0,    37,     0,     7,     8,     9,    10,     0,     0,     0,
       0,     0,     0,    11,    12,     0,    13,    14,     0,     0,
      15,     0,    16,    17,    18,     0,     0,     0,    19,     0,
       0,     0,    20,    21,     0,     0,     0,   -23,    20,    21,
      22,    23,    24,    25,     0,     0,    26,     0,    27,     0,
       0,     0,     0,   137,    27,    28,     0,     0,     0,     0,
       0,    29,     0,     0,     0,     0,     0,    29,     0,     0,
       0,     0,    30,    31,    32,    33,    34,    35,    62,    31,
      32,    33,    34,    35,     0,    36,     0,     0,     0,    37,
       0,    36,     0,     0,     0,    37
};

static const yytype_int16 yycheck[] =
{
       3,    10,     3,     3,     4,   172,    54,   268,    11,   259,
      11,     4,   268,   117,    11,    14,   299,    12,   333,   334,
       5,    44,    16,    35,    49,    65,   168,     3,    15,    35,
      17,    35,    63,    16,    65,    60,    36,    37,    56,    57,
       5,    28,    29,    77,   327,    57,    15,   189,     5,    44,
      56,    57,    56,    57,    54,    42,    43,    42,    35,     0,
      59,    54,   204,    67,    73,    76,    77,    64,    37,    38,
     385,    65,   214,    44,    61,    98,   326,    42,    81,    56,
      81,    81,    65,    83,    53,    42,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    66,    85,    94,
     100,    96,    97,    98,   208,    81,    66,   100,    77,    78,
      79,    80,    81,    82,    54,    63,   116,    65,    63,   286,
      65,    90,    46,   116,    35,    94,    97,    98,   176,    24,
      77,   273,    44,    35,   312,   135,   314,   137,   125,   422,
      29,   283,   284,    44,   392,    56,    57,    63,    43,    65,
      77,   412,   400,   201,    56,    57,   412,   405,    12,   337,
     160,    42,   340,    77,    77,    67,    77,    63,   168,    65,
     218,   158,   159,    65,     8,    77,   176,   319,    12,   188,
     322,   323,    94,   176,    96,    97,    98,   174,    65,   189,
      44,   313,   314,   180,    77,    96,    97,    98,    42,   186,
      42,   201,   312,   313,   204,    83,    52,    66,   201,    77,
      44,    14,    67,    12,   214,    66,    77,   339,   218,   341,
      77,    83,    93,    16,    77,   218,   336,    52,   338,    12,
      77,   279,    63,    66,    63,   222,    77,    91,    92,    93,
      94,   408,    96,    97,    98,    44,   233,   234,   235,    83,
     237,    77,    86,    87,    88,    89,    93,    91,    92,    93,
      94,    44,    96,    97,    98,   268,   253,   268,    12,     4,
     437,     6,     7,   273,    59,    63,    67,    67,    13,   279,
      67,    63,    66,   283,   284,    63,   279,    86,    87,    88,
      89,    65,    91,    92,    93,    94,    12,    96,    97,    98,
      44,    14,    63,   290,   291,    88,    89,    15,    91,    92,
      93,    94,    14,    96,    97,    98,    40,    77,    77,   319,
      41,    16,   322,   323,    63,    77,   335,    82,    44,    16,
     317,    15,   335,    14,   335,    67,    16,    63,    63,   342,
      63,   342,    64,    87,    88,    89,    64,    91,    92,    93,
      94,   360,    96,    97,    98,    64,    67,   360,    64,   360,
      63,    91,    55,     9,    10,    11,    63,    83,    16,    57,
      86,    87,    88,    89,   383,    91,    92,    93,    94,    77,
      96,    97,    98,    29,    56,    77,    77,    33,    77,    63,
      35,    42,    65,    42,    65,    65,    65,    63,    77,    66,
      46,    47,    48,    43,    24,    51,    91,    67,    58,   412,
     413,   412,   413,    77,    77,    77,    77,    64,   427,     0,
       1,    25,     3,     8,   427,    63,   427,     8,     9,    10,
      11,    77,    42,    37,    38,    65,    16,    18,    19,     8,
      21,    22,    77,    16,    25,   157,    27,    28,    29,    53,
     231,   194,    33,   233,   169,   221,    37,    38,   223,   253,
     290,    42,    66,   396,    45,    46,    47,    48,   396,   427,
      51,   391,    53,    77,    78,    79,    80,    81,    82,    60,
     360,    -1,     9,    10,    65,    66,    90,    -1,    -1,    -1,
      94,    -1,    -1,    -1,    -1,    -1,    77,    78,    79,    80,
      81,    82,    29,    -1,    31,    -1,    33,     0,    -1,    90,
       3,    -1,    -1,    94,    -1,     8,     9,    10,    11,    46,
      47,    48,    -1,    50,    51,    18,    19,    -1,    21,    22,
      -1,    -1,    25,    -1,    27,    28,    29,    -1,    -1,    -1,
      33,     9,    10,    -1,    37,    38,    -1,    -1,    -1,    76,
      -1,    -1,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    29,    -1,    31,    -1,    33,    -1,    60,    -1,    -1,
      -1,    -1,    65,    66,    -1,    -1,    -1,    -1,    46,    47,
      48,    -1,    50,    51,    77,    78,    79,    80,    81,    82,
      -1,    -1,    -1,    -1,    -1,    -1,     1,    90,     3,    -1,
       5,    94,    -1,     8,     9,    10,    11,    -1,    76,    37,
      38,    16,    -1,    18,    19,    -1,    21,    22,    -1,    -1,
      25,    -1,    27,    28,    29,    53,    -1,    -1,    33,    -1,
      -1,    -1,    37,    38,    -1,    -1,    -1,    42,    66,    -1,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    77,
      78,    79,    80,    81,    82,    60,    -1,    -1,     9,    10,
      65,    66,    90,    -1,    -1,    -1,    94,    -1,    -1,    -1,
      -1,    -1,    77,    78,    79,    80,    81,    82,    29,    -1,
      -1,    -1,    33,    -1,     1,    90,     3,    -1,     5,    94,
      -1,     8,     9,    10,    11,    46,    47,    48,    -1,    16,
      51,    18,    19,    -1,    21,    22,    -1,    -1,    25,    -1,
      27,    28,    29,     5,    -1,    -1,    33,     9,    10,    -1,
      37,    38,    -1,    -1,    -1,    42,    -1,    -1,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    29,    -1,    31,
      -1,    33,    -1,    60,    -1,    -1,    -1,    -1,    65,    66,
      42,    -1,    -1,    -1,    46,    47,    48,    -1,    50,    51,
      77,    78,    79,    80,    81,    82,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,     3,    -1,    -1,    94,    -1,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,    16,    -1,    18,
      19,    -1,    21,    22,    -1,    -1,    25,    -1,    27,    28,
      29,    -1,    -1,    -1,    33,    -1,    -1,    -1,    37,    38,
      -1,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    -1,    -1,    -1,    -1,
      -1,    60,    -1,    -1,    -1,    -1,    65,    66,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    77,    78,
      79,    80,    81,    82,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,     3,    -1,     5,    94,    -1,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,    -1,    -1,    18,    19,    44,
      21,    22,    -1,    -1,    25,    -1,    27,    28,    29,    -1,
      -1,    -1,    33,    -1,    -1,    -1,    37,    38,    -1,    -1,
      -1,    42,    -1,    -1,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,    60,
      -1,    -1,    -1,    -1,    89,    66,    91,    92,    93,    94,
      -1,    96,    97,    98,    -1,    -1,    77,    78,    79,    80,
      81,    82,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
       3,    -1,     5,    94,    -1,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    18,    19,    -1,    21,    22,
      -1,    -1,    25,    -1,    27,    28,    29,    -1,    -1,    -1,
      33,    -1,    -1,    -1,    37,    38,    -1,    -1,    -1,    42,
      -1,    -1,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    -1,    -1,    -1,    -1,    -1,    60,    -1,    -1,
      -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    77,    78,    79,    80,    81,    82,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,     3,    -1,
      -1,    94,    -1,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,    -1,    -1,    18,    19,    -1,    21,    22,    -1,    -1,
      25,    -1,    27,    28,    29,    -1,    -1,    -1,    33,    -1,
      -1,    -1,    37,    38,    -1,    -1,    -1,    42,    37,    38,
      45,    46,    47,    48,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    -1,    52,    53,    60,    -1,    -1,    -1,    -1,
      -1,    66,    -1,    -1,    -1,    -1,    -1,    66,    -1,    -1,
      -1,    -1,    77,    78,    79,    80,    81,    82,    77,    78,
      79,    80,    81,    82,    -1,    90,    -1,    -1,    -1,    94,
      -1,    90,    -1,    -1,    -1,    94
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     1,   100,   101,    65,     0,     3,     8,     9,    10,
      11,    18,    19,    21,    22,    25,    27,    28,    29,    33,
      37,    38,    45,    46,    47,    48,    51,    53,    60,    66,
      77,    78,    79,    80,    81,    82,    90,    94,   102,   115,
     116,   117,   124,   132,   139,   163,   218,    50,   116,   117,
     139,   156,   161,   162,   150,   115,    11,    77,   116,    77,
      54,    25,    77,   217,   218,   217,    46,    77,    66,    29,
      77,   217,   217,    11,    64,   219,   218,   218,    42,   217,
     217,    65,     8,    12,    44,    83,    86,    87,    88,    89,
      91,    92,    93,    94,    96,    97,    98,    77,     5,    77,
      65,   161,    77,   106,    42,    42,    83,   217,    52,   145,
      66,   220,    77,    14,    67,   115,   147,    66,   221,    77,
       4,     6,     7,    13,   133,   136,   137,   139,     1,   148,
     218,    77,   217,   218,   218,    15,   218,    52,   218,   218,
     218,   218,   218,   218,   218,   218,    83,   158,    93,   215,
     156,    16,   154,    77,   111,   112,   104,   103,   143,    52,
     141,    14,    59,   125,   127,   217,   222,   118,   140,   152,
     156,   222,   151,   217,   135,    16,    65,   155,   218,   218,
     159,    77,   164,   165,   166,   167,   216,    63,    66,   171,
     176,   177,    93,   113,    63,    77,   111,   121,   122,   217,
     217,     8,   156,   160,   126,    77,   146,    59,    63,    67,
      67,   160,   155,    67,   171,   134,   217,   161,   149,   170,
     217,    63,    66,    63,   217,   157,   115,   160,    65,   114,
     112,   105,    14,    63,    49,    60,   123,    15,     1,   161,
     142,   160,    14,    40,   130,    77,   222,    77,   160,    41,
     138,    16,   161,    63,   165,   168,   169,   217,   166,    77,
     214,    35,    56,    57,    77,   178,   179,   184,   185,   172,
      82,   107,   113,   144,   121,   217,   217,   217,    16,    65,
      16,    15,   119,   128,   131,    14,   153,    16,   170,    67,
      63,    64,   215,    63,    64,    64,    64,   180,    67,    63,
      31,    50,    76,   116,   117,   186,   203,    35,    56,    57,
     191,   192,   193,   195,   197,    91,   160,    55,   161,   120,
     160,   160,   129,   171,   168,   217,    77,    63,   179,     5,
      42,    77,   194,   198,   196,   173,   195,   197,   193,   197,
     193,   195,   108,   217,    16,   160,   160,   160,   215,   179,
      77,    76,    77,   189,   189,   189,   115,   116,   186,   199,
     200,   201,   203,   197,   195,   197,   193,   195,   193,   116,
     187,   190,    65,    65,    65,    42,    42,    65,   174,   201,
      77,   213,    42,    66,   181,    63,    77,    77,    24,    43,
     208,   209,   210,    63,    65,    77,   115,   188,   189,    91,
      63,    65,    43,    24,   175,   210,   213,    77,   109,   182,
     183,   184,   185,   202,   213,    58,   211,   212,   213,    65,
     171,    67,    63,    76,   116,   204,    65,    64,    65,     8,
     110,   179,    42,    65,   199,    16,    77,   205,   171,   206,
       8,   207,    16
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
#line 177 "../../src/parser.y"
    {	  categ=CLOCAL; mout(MBLOCK);
                                  begin_block(KBLOKK);separat_comp=FALSE;}
    break;

  case 3:
#line 179 "../../src/parser.y"
    { end_block(NULL,CCNO);   mout(MENDBLOCK);}
    break;

  case 5:
#line 185 "../../src/parser.y"
    { MBEENEWBLOCK();        
				  kind=KPROC;}
    break;

  case 7:
#line 192 "../../src/parser.y"
    { MBEENEWBLOCK();
				  type=TNOTY;
				  kind=KPROC;
				  if((yyvsp[(2) - (3)].ident)==Ckind)categ=CCPROC;else
				  yerror (1);
                                  ysensitive=sensitive;
                                  sensitive=ON;}
    break;

  case 8:
#line 199 "../../src/parser.y"
    { (yyval.ident)=(yyvsp[(5) - (5)].ident);
                                  sensitive=ysensitive;}
    break;

  case 9:
#line 202 "../../src/parser.y"
    { categ=CLOCAL;}
    break;

  case 10:
#line 205 "../../src/parser.y"
    { MBEENEWBLOCK();        
				  kind=KCLASS;}
    break;

  case 12:
#line 212 "../../src/parser.y"
    { if((yyvsp[(2) - (2)].token)!=HIS)yerror (2);}
    break;

  case 13:
#line 215 "../../src/parser.y"
    {         reg_decl((yyvsp[(6) - (6)].ident), type, KPROC, CCPROC);
                                          begin_block(kind);}
    break;

  case 14:
#line 218 "../../src/parser.y"
    { categ=CLOCAL;
				  end_block((yyvsp[(1) - (9)].tval)==NULL?(yyvsp[(0) - (9)].ident):tag((yyvsp[(1) - (9)].tval)),CCCPROC);}
    break;

  case 19:
#line 246 "../../src/parser.y"
    { lesinn_external_spec((yyvsp[(1) - (2)].ident),(yyvsp[(2) - (2)].tval), kind);}
    break;

  case 20:
#line 248 "../../src/parser.y"
    { (yyval.tval)=NULL;}
    break;

  case 21:
#line 249 "../../src/parser.y"
    { if((yyvsp[(1) - (1)].token)!=HEQ)yerror (9);
                                          external=TRUE;}
    break;

  case 22:
#line 251 "../../src/parser.y"
    { (yyval.tval)=(yyvsp[(3) - (3)].tval);external=FALSE;}
    break;

  case 23:
#line 254 "../../src/parser.y"
    { type=TNOTY;}
    break;

  case 26:
#line 261 "../../src/parser.y"
    { prefquantident=(yyvsp[(3) - (3)].ident); 
                                          type=TREF;}
    break;

  case 28:
#line 264 "../../src/parser.y"
    { type=TTEXT;}
    break;

  case 29:
#line 265 "../../src/parser.y"
    { type=TBOOL;}
    break;

  case 30:
#line 266 "../../src/parser.y"
    { type=TCHAR;}
    break;

  case 31:
#line 267 "../../src/parser.y"
    { type=TSHORT;}
    break;

  case 32:
#line 268 "../../src/parser.y"
    { type=TINTG;}
    break;

  case 33:
#line 269 "../../src/parser.y"
    { type=TREAL;}
    break;

  case 34:
#line 270 "../../src/parser.y"
    { type=TLONG;}
    break;

  case 36:
#line 283 "../../src/parser.y"
    { OBSBLOCK();     mout(MELSE);}
    break;

  case 37:
#line 284 "../../src/parser.y"
    { MBEEENDBLOCK();}
    break;

  case 38:
#line 286 "../../src/parser.y"
    { mout(MENDSEP);
                                                  mout(MLISTSEP);
						  (yyval.kind)=KFOR;}
    break;

  case 39:
#line 291 "../../src/parser.y"
    { mout(MLISTSEP);
						  (yyval.kind)=KFORLIST;}
    break;

  case 42:
#line 299 "../../src/parser.y"
    { mout(MFORWHILE);}
    break;

  case 43:
#line 303 "../../src/parser.y"
    { mout(MUNTIL);
                                                  mout(MSTEP);}
    break;

  case 47:
#line 311 "../../src/parser.y"
    {   begin_block(KCON);   mout(MDO);
                              OBSBLOCK(); }
    break;

  case 48:
#line 313 "../../src/parser.y"
    {   end_block(NULL,CCNO);  
                                  MBEEENDBLOCK();  mout(MENDDO);}
    break;

  case 49:
#line 318 "../../src/parser.y"
    {   begin_block(KCON);  mout(MIDENTIFIER);
                                   OBSBLOCK();     mout_id((yyvsp[(2) - (3)].ident));
				   		   mout(MWHEN);}
    break;

  case 50:
#line 321 "../../src/parser.y"
    {   end_block(NULL,CCNO);   
                              MBEEENDBLOCK(); mout(MENDWHEN);}
    break;

  case 51:
#line 326 "../../src/parser.y"
    { begin_block(KCON);	   mout(MIDENTIFIER);
                                 OBSBLOCK();       mout_id((yyvsp[(3) - (4)].ident));
				 		   mout(MWHEN);}
    break;

  case 52:
#line 329 "../../src/parser.y"
    { end_block(NULL,CCNO); 
                              MBEEENDBLOCK();    mout(MENDWHEN);}
    break;

  case 54:
#line 333 "../../src/parser.y"
    {OBSBLOCK();    mout(MOTHERWISE);}
    break;

  case 55:
#line 335 "../../src/parser.y"
    {MBEEENDBLOCK();mout(MENDOTHERWISE);}
    break;

  case 56:
#line 337 "../../src/parser.y"
    { mout(MBOOLEANKONST);
						  mout_ival(FALSE);}
    break;

  case 57:
#line 339 "../../src/parser.y"
    { mout(MBOOLEANKONST);
						  mout_ival(TRUE);}
    break;

  case 58:
#line 342 "../../src/parser.y"
    { mout(MCHARACTERKONST);
						  mout_ival(DIRECT);
						  mout(MINTEGERKONST);
						  mout_ival(0);
						  mout(MNONE);
						  mout(MBOOLEANKONST);
						  mout_ival(FALSE);}
    break;

  case 59:
#line 349 "../../src/parser.y"
    { mout(MNONE);}
    break;

  case 61:
#line 351 "../../src/parser.y"
    { mout(MINTEGERKONST);
						  mout_ival(0);}
    break;

  case 62:
#line 353 "../../src/parser.y"
    { mout(MBOOLEANKONST);
						  mout_ival(FALSE);}
    break;

  case 63:
#line 356 "../../src/parser.y"
    { mout(MCHARACTERKONST);
						  mout_ival(AT);}
    break;

  case 64:
#line 358 "../../src/parser.y"
    { mout(MCHARACTERKONST);
						  mout_ival(DELAYS);}
    break;

  case 65:
#line 361 "../../src/parser.y"
    { mout(MCHARACTERKONST);
						  mout_ival(BEFORE);}
    break;

  case 66:
#line 363 "../../src/parser.y"
    { mout(MCHARACTERKONST);
						  mout_ival(AFTER);}
    break;

  case 67:
#line 366 "../../src/parser.y"
    { mout(MBOOLEANKONST);
						  mout_ival(FALSE);}
    break;

  case 68:
#line 368 "../../src/parser.y"
    { mout(MBOOLEANKONST);
						  mout_ival(TRUE);}
    break;

  case 69:
#line 374 "../../src/parser.y"
    { STOPOBSBLOCK(); mout(MWHILE);
                                  OBSBLOCK();}
    break;

  case 70:
#line 376 "../../src/parser.y"
    { MBEEENDBLOCK(); mout(MENDWHILE);
					  	          (yyval.stat_decl)=STATEMENT;}
    break;

  case 71:
#line 380 "../../src/parser.y"
    { STOPOBSBLOCK(); mout(MIF);
                                  OBSBLOCK();}
    break;

  case 72:
#line 382 "../../src/parser.y"
    { MBEEENDBLOCK();}
    break;

  case 73:
#line 383 "../../src/parser.y"
    { mout(MENDIF);
							  (yyval.stat_decl)=STATEMENT;}
    break;

  case 74:
#line 387 "../../src/parser.y"
    { STOPOBSBLOCK(); mout(MIDENTIFIER);
                                                  mout_id((yyvsp[(2) - (3)].ident));}
    break;

  case 75:
#line 390 "../../src/parser.y"
    { begin_block((yyvsp[(5) - (6)].kind));
                        if((yyvsp[(3) - (6)].token)==HASSIGNVALUE)      mout(MFOR);
                                        else      mout(MFORR);
                                  OBSBLOCK();     mout(MFORDO);}
    break;

  case 76:
#line 394 "../../src/parser.y"
    { MBEEENDBLOCK();
                                  end_block(NULL,CCNO); mout(MENDFOR);
							  (yyval.stat_decl)=STATEMENT;}
    break;

  case 77:
#line 398 "../../src/parser.y"
    { mout(MGOTO);
                                  STOPOBSBLOCK();	  (yyval.stat_decl)=STATEMENT;}
    break;

  case 78:
#line 401 "../../src/parser.y"
    { mout(MINSPECT);
                                  STOPOBSBLOCK();
                                  begin_block(KINSP);}
    break;

  case 79:
#line 405 "../../src/parser.y"
    { end_block(NULL,CCNO);}
    break;

  case 80:
#line 406 "../../src/parser.y"
    { mout(MENDINSPECT);
							  (yyval.stat_decl)=STATEMENT;}
    break;

  case 81:
#line 408 "../../src/parser.y"
    { STOPOBSBLOCK(); mout(MINNER);
                                  reg_inner();		  (yyval.stat_decl)=STATEMENT;}
    break;

  case 82:
#line 412 "../../src/parser.y"
    { STOPOBSBLOCK();
                                  reg_decl((yyvsp[(1) - (2)].ident), TLABEL, KSIMPLE, categ);    mout(MLABEL);
                                                  mout_id((yyvsp[(1) - (2)].ident));
                                                  mout(MENDLABEL);}
    break;

  case 83:
#line 416 "../../src/parser.y"
    { if((yyvsp[(4) - (4)].stat_decl)<=DECLARATION)
                                            { yerror (27);
                                              (yyval.stat_decl)=DECLARATION;}
                                          else (yyval.stat_decl)=(yyvsp[(4) - (4)].stat_decl);}
    break;

  case 84:
#line 422 "../../src/parser.y"
    { (yyval.ident)=(yyvsp[(1) - (2)].ident); }
    break;

  case 85:
#line 424 "../../src/parser.y"
    { mout(MPRBLOCK);
				  prefquantident=(yyvsp[(1) - (4)].ident);
                                  begin_block(KPRBLK);}
    break;

  case 86:
#line 428 "../../src/parser.y"
    { end_block(NULL,CCNO); mout(MENDPRBLOCK);
                                                          (yyval.stat_decl)=STATEMENT;}
    break;

  case 87:
#line 431 "../../src/parser.y"
    { (yyval.stat_decl)=STATEMENT;
			         end_block(NULL,CCNO); mout(MENDPRBLOCK);}
    break;

  case 88:
#line 434 "../../src/parser.y"
    { (yyval.stat_decl)=STATEMENT;
			         end_block(NULL,CCNO); mout(MENDPRBLOCK);}
    break;

  case 89:
#line 438 "../../src/parser.y"
    { STOPOBSBLOCK();         (yyval.stat_decl)=STATEMENT;
                                                  mout(MENDASSIGN);}
    break;

  case 90:
#line 441 "../../src/parser.y"
    {      	  (yyval.stat_decl)=STATEMENT;
						  mout(MENDSEP);
						  mout(MARGUMENTSEP);
						  mout(MARGUMENTSEP);
						  mout(MARGUMENTSEP);
						  mout(MARGUMENTSEP);
						  mout(MARGUMENTSEP);
						  mout(MARGUMENTSEP);
						  mout(MARGUMENT);
						  mout_id(activateid);
						  mout(MENDASSIGN);}
    break;

  case 91:
#line 453 "../../src/parser.y"
    { STOPOBSBLOCK();
                                  OBSBLOCK();}
    break;

  case 92:
#line 456 "../../src/parser.y"
    { MBEEENDBLOCK();         (yyval.stat_decl)=STATEMENT;}
    break;

  case 93:
#line 459 "../../src/parser.y"
    { MBEENEWBLOCK(); mout(MPROCEDURE);
                                          reg_decl((yyvsp[(3) - (3)].ident), type, KPROC, categ);
                                          begin_block(KPROC);}
    break;

  case 94:
#line 462 "../../src/parser.y"
    { end_block(NULL,CCNO); (yyval.stat_decl)=DECLARATION;
                                                  mout(MENDPROCEDURE);}
    break;

  case 95:
#line 467 "../../src/parser.y"
    { (yyval.ident)=(yyvsp[(1) - (3)].ident); }
    break;

  case 96:
#line 470 "../../src/parser.y"
    { prefquantident=(yyvsp[(1) - (6)].ident);
				  mout(MCLASS);
                                          reg_decl((yyvsp[(6) - (6)].ident), TNOTY, KCLASS, categ);
                                          begin_block(KCLASS);}
    break;

  case 97:
#line 475 "../../src/parser.y"
    { end_block(NULL,CCNO); (yyval.stat_decl)=DECLARATION;
                                                  mout(MENDCLASS);}
    break;

  case 98:
#line 480 "../../src/parser.y"
    { prefquantident=0;
                                  MBEENEWBLOCK(); mout(MCLASS);
                                          reg_decl((yyvsp[(3) - (3)].ident), TNOTY, KCLASS, categ);
                                          begin_block(KCLASS);}
    break;

  case 99:
#line 485 "../../src/parser.y"
    { end_block(NULL,CCNO); (yyval.stat_decl)=DECLARATION;
                                                  mout(MENDCLASS);}
    break;

  case 100:
#line 487 "../../src/parser.y"
    { (yyval.stat_decl)=EXTDECLARATION;}
    break;

  case 101:
#line 488 "../../src/parser.y"
    { STOPOBSBLOCK();	  (yyval.stat_decl)=EMPTYSTATEMENT;}
    break;

  case 102:
#line 490 "../../src/parser.y"
    { MBEENEWBLOCK(); 
                                  kind=KCLASS;
				  if((yyvsp[(0) - (0)].ident)==simsetident && 
				     find_decl(simsetident,cblock,FALSE)==NULL)
				    lesinn_external_spec(simsetident,
					SIMSETATRFILE, kind);
				  if((yyvsp[(0) - (0)].ident)==simulationident && find_decl(
				    simulationident,cblock,FALSE)==NULL)
				    lesinn_external_spec(simulationident,
					SIMULATIONATRFILE, kind);
				  if(((yyvsp[(0) - (0)].ident)==fileident && find_decl(
				      fileident,cblock,FALSE)==NULL) ||
				     ((yyvsp[(0) - (0)].ident)==outfileident && find_decl(
				      outfileident,cblock,FALSE)==NULL) ||
				     ((yyvsp[(0) - (0)].ident)==infileident && find_decl(
				      infileident,cblock,FALSE)==NULL) ||
				     ((yyvsp[(0) - (0)].ident)==directfileident && find_decl(
				      directfileident,cblock,FALSE)==NULL) ||
				     ((yyvsp[(0) - (0)].ident)==printfileident && find_decl(
				      printfileident,cblock,FALSE)==NULL) ||
				     ((yyvsp[(0) - (0)].ident)==bytefileident && find_decl(
				      bytefileident,cblock,FALSE)==NULL) ||
				     ((yyvsp[(0) - (0)].ident)==inbytefileident && find_decl(
				      inbytefileident,cblock,FALSE)==NULL) ||
				     ((yyvsp[(0) - (0)].ident)==outbytefileident && find_decl(
				      outbytefileident,cblock,FALSE)==NULL) ||
				     ((yyvsp[(0) - (0)].ident)==directbytefileident && find_decl(
				      directbytefileident,cblock,FALSE)==NULL))
				    lesinn_external_spec(fileident,
					FILEATRFILE, kind);}
    break;

  case 104:
#line 526 "../../src/parser.y"
    { MBEENEWBLOCK();
                                          kind=KSIMPLE;
                                          reg_decl((yyvsp[(2) - (4)].ident), type, KSIMPLE, categ);
					  categ=CLOCAL;}
    break;

  case 105:
#line 530 "../../src/parser.y"
    { (yyval.stat_decl)=DECLARATION;}
    break;

  case 106:
#line 534 "../../src/parser.y"
    { MBEENEWBLOCK();
                                          reg_decl((yyvsp[(2) - (3)].ident), type, KSIMPLE, categ);
					  categ=CLOCAL;	  (yyval.stat_decl)=DECLARATION;}
    break;

  case 107:
#line 538 "../../src/parser.y"
    { MBEENEWBLOCK();
                                          kind=KARRAY;}
    break;

  case 108:
#line 540 "../../src/parser.y"
    { (yyval.stat_decl)=DECLARATION;}
    break;

  case 109:
#line 543 "../../src/parser.y"
    { MBEENEWBLOCK(); mout(MIDENTIFIER);
                                                  mout_id((yyvsp[(2) - (3)].ident));
                                          reg_decl((yyvsp[(2) - (3)].ident), TLABEL, KARRAY, categ);}
    break;

  case 110:
#line 546 "../../src/parser.y"
    { (yyval.stat_decl)=DECLARATION;
                                                   mout(MSWITCH);
                                                   mout(MENDSWITCH);}
    break;

  case 111:
#line 550 "../../src/parser.y"
    { if((yyvsp[(1) - (1)].stat_decl)<=DECLARATION)yerror (29);}
    break;

  case 115:
#line 555 "../../src/parser.y"
    { if((yyvsp[(1) - (1)].stat_decl)<=DECLARATION)yerror (28);
					  (yyval.stat_decl)=(yyvsp[(1) - (1)].stat_decl);}
    break;

  case 116:
#line 558 "../../src/parser.y"
    { (yyval.stat_decl)=(yyvsp[(1) - (1)].stat_decl);}
    break;

  case 117:
#line 561 "../../src/parser.y"
    { if((yyvsp[(1) - (3)].stat_decl)>=STATEMENT && (yyvsp[(3) - (3)].stat_decl)<=DECLARATION)
                                            yerror (26);
                                          (yyval.stat_decl)=(yyvsp[(3) - (3)].stat_decl);}
    break;

  case 118:
#line 565 "../../src/parser.y"
    { if((yyvsp[(1) - (1)].stat_decl)==DECLARATION)
					  {separat_comp=TRUE;gettimestamp();}
                                          (yyval.stat_decl)=(yyvsp[(1) - (1)].stat_decl);}
    break;

  case 119:
#line 569 "../../src/parser.y"
    { if((yyvsp[(1) - (3)].stat_decl)>=STATEMENT && (yyvsp[(3) - (3)].stat_decl)<=DECLARATION)
                                            yerror (26);else
       					  if((yyvsp[(1) - (3)].stat_decl)>=STATEMENT 
					     && (yyvsp[(3) - (3)].stat_decl)!=EMPTYSTATEMENT)yerror (25);
                                          if(separat_comp && (yyvsp[(3) - (3)].stat_decl)==STATEMENT)
                                             yerror (25);
                                          if((yyvsp[(3) - (3)].stat_decl)==DECLARATION && !separat_comp)
					  {separat_comp=TRUE;gettimestamp();}
					  (yyval.stat_decl)=(yyvsp[(3) - (3)].stat_decl);}
    break;

  case 122:
#line 587 "../../src/parser.y"
    { mout(MARRAY);
                                                  mout(MENDARRAY);
                                                  set_array_dim((yyvsp[(3) - (4)].arrdim));}
    break;

  case 123:
#line 591 "../../src/parser.y"
    { mout(MENDSEP);
                                                  mout(MARRAYSEP);}
    break;

  case 124:
#line 596 "../../src/parser.y"
    { mout(MARRAYSEP);}
    break;

  case 125:
#line 598 "../../src/parser.y"
    { mout(MIDENTIFIER);
                                                  mout_id((yyvsp[(1) - (1)].ident));
                                                  reg_decl((yyvsp[(1) - (1)].ident), type, kind, categ);
				   if(last_array==NULL)
                                     last_array=cblock->lastparloc;}
    break;

  case 126:
#line 604 "../../src/parser.y"
    { mout(MENDSEP);
                                                  mout(MBOUNDSEP);
                                                  (yyval.arrdim)=1;}
    break;

  case 127:
#line 609 "../../src/parser.y"
    { mout(MBOUNDSEP);
                                                  (yyval.arrdim)=(yyvsp[(3) - (3)].arrdim)+1;}
    break;

  case 128:
#line 614 "../../src/parser.y"
    { mout(MBOUNDPARSEP);}
    break;

  case 129:
#line 616 "../../src/parser.y"
    { mout(MENDSEP);
                                                  mout(MSWITCHSEP);}
    break;

  case 130:
#line 620 "../../src/parser.y"
    { mout(MSWITCHSEP);}
    break;

  case 131:
#line 622 "../../src/parser.y"
    { kind=KNOKD;}
    break;

  case 132:
#line 623 "../../src/parser.y"
    { categ=CSPEC;}
    break;

  case 133:
#line 624 "../../src/parser.y"
    { kind=KNOKD;}
    break;

  case 134:
#line 625 "../../src/parser.y"
    { categ=CVIRT;}
    break;

  case 135:
#line 627 "../../src/parser.y"
    { categ=CLOCAL;}
    break;

  case 141:
#line 638 "../../src/parser.y"
    { reg_decl((yyvsp[(1) - (1)].ident), type, KNOKD, CDEFLT);}
    break;

  case 142:
#line 639 "../../src/parser.y"
    { reg_decl(varargsid, TVARARGS, KNOKD, categ);}
    break;

  case 143:
#line 640 "../../src/parser.y"
    { reg_decl((yyvsp[(1) - (1)].ident), type, KNOKD, CDEFLT);}
    break;

  case 150:
#line 652 "../../src/parser.y"
    { reg_decl(varargsid, TVARARGS, KNOKD, categ);}
    break;

  case 153:
#line 658 "../../src/parser.y"
    { reg_decl((yyvsp[(3) - (3)].ident), type, kind, categ);}
    break;

  case 155:
#line 662 "../../src/parser.y"
    { categ=CNAME;}
    break;

  case 156:
#line 664 "../../src/parser.y"
    { categ=CVALUE;}
    break;

  case 157:
#line 666 "../../src/parser.y"
    { categ=CVAR;}
    break;

  case 158:
#line 667 "../../src/parser.y"
    { categ=CDEFLT;}
    break;

  case 159:
#line 671 "../../src/parser.y"
    { (yyval.ival)=categ;
                                          reg_decl((yyvsp[(3) - (3)].ident), type, KPROC, categ);
                                          begin_block(KPROC);}
    break;

  case 160:
#line 675 "../../src/parser.y"
    { categ=(yyvsp[(4) - (5)].ival); /* M} settes tilbake*/}
    break;

  case 161:
#line 676 "../../src/parser.y"
    { end_block(NULL,CCNO);}
    break;

  case 162:
#line 678 "../../src/parser.y"
    { reg_decl((yyvsp[(1) - (1)].ident), type, kind, categ);}
    break;

  case 163:
#line 679 "../../src/parser.y"
    {	  reg_decl(varargsid, TVARARGS, kind, categ);}
    break;

  case 164:
#line 680 "../../src/parser.y"
    { reg_decl((yyvsp[(1) - (1)].ident), type, kind, categ);}
    break;

  case 183:
#line 702 "../../src/parser.y"
    { categ=CNAME;}
    break;

  case 185:
#line 706 "../../src/parser.y"
    { categ=CVAR;}
    break;

  case 187:
#line 710 "../../src/parser.y"
    { categ=CVALUE;}
    break;

  case 194:
#line 721 "../../src/parser.y"
    { if((yyvsp[(4) - (4)].token)!=HIS) yerror (8);}
    break;

  case 197:
#line 725 "../../src/parser.y"
    { yerror (45);}
    break;

  case 198:
#line 728 "../../src/parser.y"
    { yerror (45);}
    break;

  case 199:
#line 730 "../../src/parser.y"
    { kind=KSIMPLE;}
    break;

  case 200:
#line 732 "../../src/parser.y"
    { kind=KARRAY;}
    break;

  case 201:
#line 733 "../../src/parser.y"
    { type=TLABEL;
                                          kind=KSIMPLE;}
    break;

  case 202:
#line 735 "../../src/parser.y"
    { type=TLABEL;
                                          kind=KARRAY;}
    break;

  case 203:
#line 740 "../../src/parser.y"
    { (yyval.ival)=categ;
                                          reg_decl((yyvsp[(3) - (3)].ident), type, KPROC, categ);
                                          begin_block(KPROC);}
    break;

  case 204:
#line 744 "../../src/parser.y"
    { categ=(yyvsp[(4) - (5)].ival); /* M} settes tilbake*/}
    break;

  case 205:
#line 746 "../../src/parser.y"
    { end_block(NULL,CCNO);}
    break;

  case 212:
#line 759 "../../src/parser.y"
    { categ=CHIDEN;}
    break;

  case 213:
#line 760 "../../src/parser.y"
    { categ=CPROT;}
    break;

  case 214:
#line 762 "../../src/parser.y"
    { categ=CHIPRO;}
    break;

  case 215:
#line 764 "../../src/parser.y"
    { categ=CHIPRO;}
    break;

  case 219:
#line 773 "../../src/parser.y"
    { reg_decl((yyvsp[(1) - (1)].ident), type, kind, categ);}
    break;

  case 220:
#line 775 "../../src/parser.y"
    { reg_decl((yyvsp[(3) - (3)].ident), type, kind, categ);}
    break;

  case 221:
#line 778 "../../src/parser.y"
    { reg_decl((yyvsp[(1) - (2)].ident), type, kind, categ);
					  categ=CLOCAL;}
    break;

  case 222:
#line 782 "../../src/parser.y"
    { reg_decl((yyvsp[(3) - (4)].ident), type, kind, categ);
					  categ=CLOCAL;}
    break;

  case 224:
#line 787 "../../src/parser.y"
    { MBEENEWBLOCK();
			    if((yyvsp[(1) - (1)].token)!=HEQ) yerror (8);
					  if(type==TREF)yerror (7);
					  categ=CCONSTU;
						  mout(MIDENTIFIER);
						  mout_id((yyvsp[(0) - (1)].ident));}
    break;

  case 225:
#line 793 "../../src/parser.y"
    { mout(MASSIGN);
						  mout(MCONST);}
    break;

  case 226:
#line 798 "../../src/parser.y"
    {}
    break;

  case 227:
#line 804 "../../src/parser.y"
    { mout(MELSEE);
                                                  mout(MIFE);}
    break;

  case 228:
#line 809 "../../src/parser.y"
    { if((yyvsp[(2) - (3)].token)==HASSIGNREF)mout(MASSIGNR);
                                          else    mout(MASSIGN);(yyval.ident)=NULL;}
    break;

  case 229:
#line 815 "../../src/parser.y"
    { mout(MCONC);(yyval.ident)=NULL;}
    break;

  case 230:
#line 819 "../../src/parser.y"
    { mout(MORELSEE);(yyval.ident)=NULL;}
    break;

  case 231:
#line 823 "../../src/parser.y"
    { mout(MANDTHENE);(yyval.ident)=NULL;}
    break;

  case 232:
#line 825 "../../src/parser.y"
    { mout(MEQV);(yyval.ident)=NULL;}
    break;

  case 233:
#line 827 "../../src/parser.y"
    { mout(MIMP);(yyval.ident)=NULL;}
    break;

  case 234:
#line 829 "../../src/parser.y"
    { mout(MOR);(yyval.ident)=NULL;}
    break;

  case 235:
#line 831 "../../src/parser.y"
    { mout(MAND);(yyval.ident)=NULL;}
    break;

  case 236:
#line 832 "../../src/parser.y"
    { mout(MNOT);(yyval.ident)=NULL;}
    break;

  case 237:
#line 836 "../../src/parser.y"
    { switch((yyvsp[(2) - (3)].token))
                                    {   case HEQ: mout(MEQ);break;
                                        case HNE: mout(MNE);break;
                                        case HLT: mout(MLT);break;
                                        case HLE: mout(MLE);break;
                                        case HGT: mout(MGT);break;
                                        case HGE: mout(MGE);break;
                                    }(yyval.ident)=NULL;}
    break;

  case 238:
#line 847 "../../src/parser.y"
    { if((yyvsp[(2) - (3)].token)==HNER)    mout(MNER);
                                        else      mout(MEQR);(yyval.ident)=NULL;}
    break;

  case 239:
#line 852 "../../src/parser.y"
    { if((yyvsp[(2) - (3)].token)==HIS)     mout(MIS);
                                        else      mout(MINS);(yyval.ident)=NULL;}
    break;

  case 240:
#line 856 "../../src/parser.y"
    { if((yyvsp[(1) - (2)].token)==HADD)    mout(MUADD);
                                        else      mout(MUSUB);(yyval.ident)=NULL;}
    break;

  case 241:
#line 861 "../../src/parser.y"
    { if((yyvsp[(2) - (3)].token)==HADD)    mout(MADD);
                                        else      mout(MSUB);(yyval.ident)=NULL;}
    break;

  case 242:
#line 866 "../../src/parser.y"
    { if((yyvsp[(2) - (3)].token)==HMUL)    mout(MMUL); else
                                  if((yyvsp[(2) - (3)].token)==HDIV)    mout(MDIV);
                                        else      mout(MINTDIV);(yyval.ident)=NULL;}
    break;

  case 243:
#line 871 "../../src/parser.y"
    { mout(MPRIMARY);(yyval.ident)=NULL;}
    break;

  case 244:
#line 873 "../../src/parser.y"
    { mout(MNOOP);(yyval.ident)=NULL;}
    break;

  case 245:
#line 874 "../../src/parser.y"
    { mout(MTEXTKONST);
                                                  mout_tval((yyvsp[(1) - (1)].tval));(yyval.ident)=NULL;}
    break;

  case 246:
#line 876 "../../src/parser.y"
    { mout(MCHARACTERKONST);
                                                  mout_ival((yyvsp[(1) - (1)].ival));(yyval.ident)=NULL;}
    break;

  case 247:
#line 878 "../../src/parser.y"
    { mout(MREALKONST);
                                                  mout_rval((yyvsp[(1) - (1)].rval));(yyval.ident)=NULL;}
    break;

  case 248:
#line 880 "../../src/parser.y"
    { mout(MINTEGERKONST);
                                                  mout_ival((yyvsp[(1) - (1)].ival));(yyval.ident)=NULL;}
    break;

  case 249:
#line 882 "../../src/parser.y"
    { mout(MBOOLEANKONST);
                                                  mout_ival((yyvsp[(1) - (1)].ival));(yyval.ident)=NULL;}
    break;

  case 250:
#line 884 "../../src/parser.y"
    { mout(MNONE);(yyval.ident)=NULL;}
    break;

  case 251:
#line 886 "../../src/parser.y"
    { (yyval.ident)=(yyvsp[(1) - (1)].ident);}
    break;

  case 253:
#line 888 "../../src/parser.y"
    { mout(MTHIS);
                                                  mout_id((yyvsp[(2) - (2)].ident));(yyval.ident)=NULL;}
    break;

  case 254:
#line 892 "../../src/parser.y"
    { mout(MNEWARG);
                                                  mout_id((yyvsp[(2) - (3)].ident));(yyval.ident)=NULL;}
    break;

  case 255:
#line 896 "../../src/parser.y"
    { mout(MDOT);(yyval.ident)=NULL;}
    break;

  case 256:
#line 898 "../../src/parser.y"
    { mout(MQUA);
                                                  mout_id((yyvsp[(3) - (3)].ident));(yyval.ident)=NULL;}
    break;

  case 257:
#line 901 "../../src/parser.y"
    { mout(MENDSEP);}
    break;

  case 259:
#line 905 "../../src/parser.y"
    { mout(MIDENTIFIER);
                                                  mout_id((yyvsp[(0) - (0)].ident));
						  (yyval.ident)=(yyvsp[(0) - (0)].ident);}
    break;

  case 260:
#line 909 "../../src/parser.y"
    { mout(MARGUMENT);
                                                  mout_id((yyvsp[(0) - (3)].ident));}
    break;

  case 261:
#line 912 "../../src/parser.y"
    { mout(MENDSEP);
                                                  mout(MARGUMENTSEP);}
    break;

  case 262:
#line 916 "../../src/parser.y"
    { mout(MARGUMENTSEP);}
    break;


/* Line 1267 of yacc.c.  */
#line 3323 "../../src/parser.c"
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


#line 918 "../../src/parser.y"

/******************************************************************************
                                                                YYERROR      */

/* Oppdages feil så blir denne procedyren kalt */

void yyerror (char s[])
  {
     yaccerror=TRUE;
#if 0
     if(s[0]=='s')yerror (13);else
     if(s[0]=='y')yerror (14);else
       yerror (16);
#else
     yerror (21,s);
#endif
     mout(MERROR);
  }

/******************************************************************************
                                                                YLEX         */
                                                                             
#ifdef yylex
#undef yylex
int ylex(void)
{
  long i;
  i=yylex();
#ifdef DEBUG
  if(option_lex)
    {
      printf("line: %ld yylex:",lineno);
      print_lexsymbol(i,&yylval);
      printf("\n");
    }
#endif
  return(i);
}
#define yylex ylex
#endif

/******************************************************************************
                                                                 INIT_PARSER */

void parser_init (void)
{
  obstack_init (&os_block);
  first_object_allocated_ptr_block= obstack_alloc (&os_block, 0);
}

void parser_init_pass1 (void)
{
  activateid=tag("activat");
  varargsid=tag("...");
  Ckind=tag("C");
  simsetident=tag("SIMSET");
  simulationident=tag("SIMULATION");
  fileident=tag("FILE");
  imagefileident=tag("IMAGEFILE");
  outfileident=tag("OUTFILE");
  infileident=tag("INFILE");
  directfileident=tag("DIRECTFILE");
  printfileident=tag("PRINTFILE");
  bytefileident=tag("BYTEFILE");
  inbytefileident=tag("INBYTEFILE");
  outbytefileident=tag("OUTBYTEFILE");
  directbytefileident=tag("DIRECTBYTEFILE");
  blockp= (blockstack_t *)
    obstack_alloc (&os_block,sizeof (blockstack_t));
  blockp->prev= NULL;
  blockp->rem=FALSE;
}

void parser_reinit (void)
{
  obstack_free (&os_block, first_object_allocated_ptr_block);
}

