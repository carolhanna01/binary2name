/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

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

#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int mu_sieve_yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    IDENT = 258,
    TAG = 259,
    NUMBER = 260,
    STRING = 261,
    MULTILINE = 262,
    REQUIRE = 263,
    IF = 264,
    ELSIF = 265,
    ELSE = 266,
    ANYOF = 267,
    ALLOF = 268,
    NOT = 269,
    FALSE = 270,
    TRUE = 271
  };
#endif
/* Tokens.  */
#define IDENT 258
#define TAG 259
#define NUMBER 260
#define STRING 261
#define MULTILINE 262
#define REQUIRE 263
#define IF 264
#define ELSIF 265
#define ELSE 266
#define ANYOF 267
#define ALLOF 268
#define NOT 269
#define FALSE 270
#define TRUE 271

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 43 "sieve-gram.y" /* yacc.c:1909  */

  char *string;
  size_t number;
  size_t idx;
  struct mu_sieve_slice slice;
  struct
  {
    char *ident;
    struct mu_locus_range idloc;
    size_t first;
    size_t count;
  } command;
  struct mu_sieve_node_list node_list;
  struct mu_sieve_node *node;

#line 102 "sieve-gram.h" /* yacc.c:1909  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif


extern YYSTYPE mu_sieve_yylval;
extern YYLTYPE mu_sieve_yylloc;
int mu_sieve_yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
