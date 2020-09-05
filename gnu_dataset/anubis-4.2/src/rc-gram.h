/* A Bison parser, made by GNU Bison 2.5.1.  */

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

/* Line 2072 of yacc.c  */
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



/* Line 2072 of yacc.c  */
#line 129 "rc-gram.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE yylval;

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

extern YYLTYPE yylloc;

