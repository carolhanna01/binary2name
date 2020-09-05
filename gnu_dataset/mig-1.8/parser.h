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

#ifndef YY_YY_PARSER_H_INCLUDED
# define YY_YY_PARSER_H_INCLUDED
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
    sySkip = 258,
    syRoutine = 259,
    sySimpleRoutine = 260,
    sySubsystem = 261,
    syKernelUser = 262,
    syKernelServer = 263,
    syMsgOption = 264,
    syMsgSeqno = 265,
    syWaitTime = 266,
    syNoWaitTime = 267,
    syErrorProc = 268,
    syServerPrefix = 269,
    syUserPrefix = 270,
    syServerDemux = 271,
    syRCSId = 272,
    syImport = 273,
    syUImport = 274,
    sySImport = 275,
    syIn = 276,
    syOut = 277,
    syInOut = 278,
    syRequestPort = 279,
    syReplyPort = 280,
    sySReplyPort = 281,
    syUReplyPort = 282,
    syType = 283,
    syArray = 284,
    syStruct = 285,
    syOf = 286,
    syInTran = 287,
    syOutTran = 288,
    syDestructor = 289,
    syCType = 290,
    syCUserType = 291,
    syCServerType = 292,
    syCString = 293,
    syColon = 294,
    sySemi = 295,
    syComma = 296,
    syPlus = 297,
    syMinus = 298,
    syStar = 299,
    syDiv = 300,
    syLParen = 301,
    syRParen = 302,
    syEqual = 303,
    syCaret = 304,
    syTilde = 305,
    syLAngle = 306,
    syRAngle = 307,
    syLBrack = 308,
    syRBrack = 309,
    syBar = 310,
    syError = 311,
    syNumber = 312,
    sySymbolicType = 313,
    syIdentifier = 314,
    syString = 315,
    syQString = 316,
    syFileName = 317,
    syIPCFlag = 318,
    syInTranPayload = 319
  };
#endif
/* Tokens.  */
#define sySkip 258
#define syRoutine 259
#define sySimpleRoutine 260
#define sySubsystem 261
#define syKernelUser 262
#define syKernelServer 263
#define syMsgOption 264
#define syMsgSeqno 265
#define syWaitTime 266
#define syNoWaitTime 267
#define syErrorProc 268
#define syServerPrefix 269
#define syUserPrefix 270
#define syServerDemux 271
#define syRCSId 272
#define syImport 273
#define syUImport 274
#define sySImport 275
#define syIn 276
#define syOut 277
#define syInOut 278
#define syRequestPort 279
#define syReplyPort 280
#define sySReplyPort 281
#define syUReplyPort 282
#define syType 283
#define syArray 284
#define syStruct 285
#define syOf 286
#define syInTran 287
#define syOutTran 288
#define syDestructor 289
#define syCType 290
#define syCUserType 291
#define syCServerType 292
#define syCString 293
#define syColon 294
#define sySemi 295
#define syComma 296
#define syPlus 297
#define syMinus 298
#define syStar 299
#define syDiv 300
#define syLParen 301
#define syRParen 302
#define syEqual 303
#define syCaret 304
#define syTilde 305
#define syLAngle 306
#define syRAngle 307
#define syLBrack 308
#define syRBrack 309
#define syBar 310
#define syError 311
#define syNumber 312
#define sySymbolicType 313
#define syIdentifier 314
#define syString 315
#define syQString 316
#define syFileName 317
#define syIPCFlag 318
#define syInTranPayload 319

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 137 "../release/parser.y" /* yacc.c:1909  */

    u_int number;
    identifier_t identifier;
    const_string_t string;
    statement_kind_t statement_kind;
    ipc_type_t *type;
    struct
    {
	u_int innumber;		/* msgt_name value, when sending */
	const_string_t instr;
	u_int outnumber;	/* msgt_name value, when receiving */
	const_string_t outstr;
	u_int size;		/* 0 means there is no default size */
    } symtype;
    routine_t *routine;
    arg_kind_t direction;
    argument_t *argument;
    ipc_flags_t flag;

#line 202 "parser.h" /* yacc.c:1909  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_PARSER_H_INCLUDED  */
