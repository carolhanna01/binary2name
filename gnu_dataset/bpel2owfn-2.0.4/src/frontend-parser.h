/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

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

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     APOSTROPHE = 258,
     EQUAL = 259,
     GREATER = 260,
     GREATEROREQUAL = 261,
     K_AND = 262,
     K_ASSIGN = 263,
     K_BRANCHES = 264,
     K_CASE = 265,
     K_CATCH = 266,
     K_CATCHALL = 267,
     K_COMPENSATE = 268,
     K_COMPENSATESCOPE = 269,
     K_COMPENSATIONHANDLER = 270,
     K_COMPLETIONCONDITION = 271,
     K_CONDITION = 272,
     K_COPY = 273,
     K_CORRELATION = 274,
     K_CORRELATIONS = 275,
     K_CORRELATIONSET = 276,
     K_CORRELATIONSETS = 277,
     K_ELSE = 278,
     K_ELSEIF = 279,
     K_EMPTY = 280,
     K_EVENTHANDLERS = 281,
     K_EXIT = 282,
     K_EXTENSION = 283,
     K_EXTENSIONACTIVITY = 284,
     K_EXTENSIONASSIGNOPERATION = 285,
     K_EXTENSIONS = 286,
     K_FAULTHANDLERS = 287,
     K_FINALCOUNTERVALUE = 288,
     K_FLOW = 289,
     K_FOR = 290,
     K_FOREACH = 291,
     K_FROM = 292,
     K_FROMPART = 293,
     K_FROMPARTS = 294,
     K_GETLINKSTATUS = 295,
     K_IF = 296,
     K_IMPORT = 297,
     K_INVOKE = 298,
     K_JOINCONDITION = 299,
     K_LINK = 300,
     K_LINKS = 301,
     K_LITERAL = 302,
     K_MESSAGEEXCHANGE = 303,
     K_MESSAGEEXCHANGES = 304,
     K_ONALARM = 305,
     K_ONEVENT = 306,
     K_ONMESSAGE = 307,
     K_OPAQUEACTIVITY = 308,
     K_OPAQUEFROM = 309,
     K_OR = 310,
     K_OTHERWISE = 311,
     K_PARTNER = 312,
     K_PARTNERLINK = 313,
     K_PARTNERLINKS = 314,
     K_PARTNERS = 315,
     K_PICK = 316,
     K_PROCESS = 317,
     K_QUERY = 318,
     K_RECEIVE = 319,
     K_REPEATEVERY = 320,
     K_REPEATUNTIL = 321,
     K_REPLY = 322,
     K_RETHROW = 323,
     K_SCOPE = 324,
     K_SEQUENCE = 325,
     K_SOURCE = 326,
     K_SOURCES = 327,
     K_STARTCOUNTERVALUE = 328,
     K_SWITCH = 329,
     K_TARGET = 330,
     K_TARGETS = 331,
     K_TERMINATE = 332,
     K_TERMINATIONHANDLER = 333,
     K_THROW = 334,
     K_TO = 335,
     K_TOPART = 336,
     K_TOPARTS = 337,
     K_TRANSITIONCONDITION = 338,
     K_UNTIL = 339,
     K_VALIDATE = 340,
     K_VARIABLE = 341,
     K_VARIABLES = 342,
     K_WAIT = 343,
     K_WHILE = 344,
     LBRACKET = 345,
     LESS = 346,
     LESSOREQUAL = 347,
     NOTEQUAL = 348,
     RBRACKET = 349,
     X_CLOSE = 350,
     X_EQUALS = 351,
     X_NEXT = 352,
     X_OPEN = 353,
     X_SLASH = 354,
     K_TOPOLOGY = 355,
     K_PARTICIPANTTYPES = 356,
     K_PARTICIPANTTYPE = 357,
     K_PARTICIPANTS = 358,
     K_PARTICIPANT = 359,
     K_PARTICIPANTSET = 360,
     K_MESSAGELINKS = 361,
     K_MESSAGELINK = 362,
     K_TYPES = 363,
     K_PORTTYPE = 364,
     K_FAULT = 365,
     K_OPERATION = 366,
     K_DEFINITIONS = 367,
     K_OUTPUT = 368,
     K_INPUT = 369,
     K_MESSAGE = 370,
     K_PART = 371,
     K_BINDING = 372,
     K_SERVICE = 373,
     K_PORT = 374,
     K_PARTNERLINKTYPE = 375,
     K_ROLE = 376,
     K_SCHEMA = 377,
     K_PROPERTY = 378,
     K_PROPERTYALIAS = 379,
     P_NET = 380,
     P_PLACE = 381,
     P_GRAPHICS = 382,
     P_NAME = 383,
     P_DESCRIPTION = 384,
     P_TRANSITION = 385,
     P_PAGE = 386,
     P_POSITION = 387,
     P_TEXT = 388,
     P_INITIALMARKING = 389,
     P_DIMENSION = 390,
     P_PNML = 391,
     P_ARC = 392,
     P_INSCRIPTION = 393,
     P_OFFSET = 394,
     P_REFERENCEPLACE = 395,
     P_TYPE = 396,
     P_TRANSFORMATION = 397,
     P_TOOLSPECIFIC = 398,
     NUMBER = 399,
     X_NAME = 400,
     VARIABLENAME = 401,
     X_STRING = 402,
     X_TEXT = 403
   };
#endif
/* Tokens.  */
#define APOSTROPHE 258
#define EQUAL 259
#define GREATER 260
#define GREATEROREQUAL 261
#define K_AND 262
#define K_ASSIGN 263
#define K_BRANCHES 264
#define K_CASE 265
#define K_CATCH 266
#define K_CATCHALL 267
#define K_COMPENSATE 268
#define K_COMPENSATESCOPE 269
#define K_COMPENSATIONHANDLER 270
#define K_COMPLETIONCONDITION 271
#define K_CONDITION 272
#define K_COPY 273
#define K_CORRELATION 274
#define K_CORRELATIONS 275
#define K_CORRELATIONSET 276
#define K_CORRELATIONSETS 277
#define K_ELSE 278
#define K_ELSEIF 279
#define K_EMPTY 280
#define K_EVENTHANDLERS 281
#define K_EXIT 282
#define K_EXTENSION 283
#define K_EXTENSIONACTIVITY 284
#define K_EXTENSIONASSIGNOPERATION 285
#define K_EXTENSIONS 286
#define K_FAULTHANDLERS 287
#define K_FINALCOUNTERVALUE 288
#define K_FLOW 289
#define K_FOR 290
#define K_FOREACH 291
#define K_FROM 292
#define K_FROMPART 293
#define K_FROMPARTS 294
#define K_GETLINKSTATUS 295
#define K_IF 296
#define K_IMPORT 297
#define K_INVOKE 298
#define K_JOINCONDITION 299
#define K_LINK 300
#define K_LINKS 301
#define K_LITERAL 302
#define K_MESSAGEEXCHANGE 303
#define K_MESSAGEEXCHANGES 304
#define K_ONALARM 305
#define K_ONEVENT 306
#define K_ONMESSAGE 307
#define K_OPAQUEACTIVITY 308
#define K_OPAQUEFROM 309
#define K_OR 310
#define K_OTHERWISE 311
#define K_PARTNER 312
#define K_PARTNERLINK 313
#define K_PARTNERLINKS 314
#define K_PARTNERS 315
#define K_PICK 316
#define K_PROCESS 317
#define K_QUERY 318
#define K_RECEIVE 319
#define K_REPEATEVERY 320
#define K_REPEATUNTIL 321
#define K_REPLY 322
#define K_RETHROW 323
#define K_SCOPE 324
#define K_SEQUENCE 325
#define K_SOURCE 326
#define K_SOURCES 327
#define K_STARTCOUNTERVALUE 328
#define K_SWITCH 329
#define K_TARGET 330
#define K_TARGETS 331
#define K_TERMINATE 332
#define K_TERMINATIONHANDLER 333
#define K_THROW 334
#define K_TO 335
#define K_TOPART 336
#define K_TOPARTS 337
#define K_TRANSITIONCONDITION 338
#define K_UNTIL 339
#define K_VALIDATE 340
#define K_VARIABLE 341
#define K_VARIABLES 342
#define K_WAIT 343
#define K_WHILE 344
#define LBRACKET 345
#define LESS 346
#define LESSOREQUAL 347
#define NOTEQUAL 348
#define RBRACKET 349
#define X_CLOSE 350
#define X_EQUALS 351
#define X_NEXT 352
#define X_OPEN 353
#define X_SLASH 354
#define K_TOPOLOGY 355
#define K_PARTICIPANTTYPES 356
#define K_PARTICIPANTTYPE 357
#define K_PARTICIPANTS 358
#define K_PARTICIPANT 359
#define K_PARTICIPANTSET 360
#define K_MESSAGELINKS 361
#define K_MESSAGELINK 362
#define K_TYPES 363
#define K_PORTTYPE 364
#define K_FAULT 365
#define K_OPERATION 366
#define K_DEFINITIONS 367
#define K_OUTPUT 368
#define K_INPUT 369
#define K_MESSAGE 370
#define K_PART 371
#define K_BINDING 372
#define K_SERVICE 373
#define K_PORT 374
#define K_PARTNERLINKTYPE 375
#define K_ROLE 376
#define K_SCHEMA 377
#define K_PROPERTY 378
#define K_PROPERTYALIAS 379
#define P_NET 380
#define P_PLACE 381
#define P_GRAPHICS 382
#define P_NAME 383
#define P_DESCRIPTION 384
#define P_TRANSITION 385
#define P_PAGE 386
#define P_POSITION 387
#define P_TEXT 388
#define P_INITIALMARKING 389
#define P_DIMENSION 390
#define P_PNML 391
#define P_ARC 392
#define P_INSCRIPTION 393
#define P_OFFSET 394
#define P_REFERENCEPLACE 395
#define P_TYPE 396
#define P_TRANSFORMATION 397
#define P_TOOLSPECIFIC 398
#define NUMBER 399
#define X_NAME 400
#define VARIABLENAME 401
#define X_STRING 402
#define X_TEXT 403




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE frontend_lval;

