/* A Bison parser, made by GNU Bison 2.7.  */

/* Bison interface for Yacc-like parsers in C
   
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
/* Line 2058 of yacc.c  */
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


/* Line 2058 of yacc.c  */
#line 174 "cfgram.h"
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
