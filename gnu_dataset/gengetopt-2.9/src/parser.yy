/**
 * Copyright (C) 1999, 2000, 2001  Free Software Foundation, Inc.
 *
 * This file is part of GNU gengetopt 
 *
 * GNU gengetopt is free software; you can redistribute it and/or modify 
 * it under the terms of the GNU General Public License as published by 
 * the Free Software Foundation; either version 2, or (at your option) 
 * any later version. 
 *
 * GNU gengetopt is distributed in the hope that it will be useful, but 
 * WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
 * Public License for more details. 
 *
 * You should have received a copy of the GNU General Public License along 
 * with gengetopt; see the file COPYING. If not, write to the Free Software 
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. 
 */


%{
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "argsdef.h"

#include "gengetopt.h"

extern int gengetopt_count_line;
extern char * gengetopt_input_filename;

static int gengetopt_package_given = 0;
static int gengetopt_version_given = 0;
static int gengetopt_purpose_given = 0;

extern void yyerror ( char *error ) ;
extern void yyerror2 ( char *error ) ;
extern int yylex () ;

//#define YYERROR_VERBOSE 1
#define check_result \
	if (o) { switch (o) { case 1: yyerror2 ("not enough memory"); break; case 2: yyerror2 ("long option redefined"); break; case 3: yyerror2 ("short option redefined"); break; case 4: yyerror2 ("bug found!!"); break; case 5: yyerror2 ("group undefined"); break; case 6: yyerror2("invalid default value"); break;} YYERROR; }
%}

%union {
char * str;
char chr;
int argtype;
int boolean;
}

%token           TOK_PACKAGE
%token           TOK_VERSION
%token           TOK_OPTION
%token           TOK_DEFGROUP
%token           TOK_GROUPOPTION
%token           TOK_YES
%token           TOK_NO
%token           TOK_FLAG
%token           TOK_PURPOSE
%token <boolean>    TOK_ONOFF
%token <str>     TOK_STRING
%token           TOK_DEFAULT
%token           TOK_GROUP
%token <str>     TOK_MLSTRING
%token <chr>     TOK_CHAR
%token <argtype> TOK_ARGTYPE
%token           TOK_MULTIPLE

%type  <str>     exp_str
%type  <str>     exp_mlstr
%type  <boolean>    exp_yesno
%type  <boolean>    exp_multiple
%type  <str>     grp_value
%type  <str>     def_value


%%


input:	
	| input exp
;


exp_str:	 TOK_STRING             { $$ = $1; }
;

exp_mlstr:	 TOK_MLSTRING           { $$ = $1; }
;


exp_yesno:	  TOK_YES  { $$ = 1; }
		| TOK_NO   { $$ = 0; }
;

exp_multiple:	{ $$ = 0; }
		| TOK_MULTIPLE   { $$ = 1; }
;

exp: TOK_PACKAGE TOK_STRING { if (gengetopt_package_given) { yyerror ("package redefined"); YYERROR; } else { gengetopt_package_given = 1; if (gengetopt_define_package ($2)) { yyerror ("not enough memory"); YYERROR; } } }
;


exp: TOK_VERSION TOK_STRING { if (gengetopt_version_given) { yyerror ("version redefined"); YYERROR; } else { gengetopt_version_given = 1; if (gengetopt_define_version ($2)) { yyerror ("not enough memory"); YYERROR; } } }
;

exp: TOK_PURPOSE exp_mlstr { if (gengetopt_purpose_given) { yyerror ("purpose redefined"); YYERROR; } else { gengetopt_purpose_given = 1; if (gengetopt_define_purpose ($2)) { yyerror ("not enough memory"); YYERROR; } } }
;

exp: TOK_PURPOSE exp_str { if (gengetopt_purpose_given) { yyerror ("purpose redefined"); YYERROR; } else { gengetopt_purpose_given = 1; if (gengetopt_define_purpose ($2)) { yyerror ("not enough memory"); YYERROR; } } }
;

exp: TOK_DEFGROUP exp_str exp_yesno
{ 
  if (gengetopt_add_group ($2,$3))
    {
      yyerror ("group redefined"); 
      YYERROR;      
    }	
}
;

exp: TOK_DEFGROUP exp_str 
{ 
  if (gengetopt_add_group ($2,0))
    {
      yyerror ("group redefined"); 
      YYERROR;      
    }	
}
;

exp: TOK_OPTION TOK_STRING TOK_CHAR exp_str TOK_NO { int o = gengetopt_add_option ($2, $3, $4, ARG_NO, 0, 0, 0, 0, 0); check_result; }
;

exp: TOK_OPTION TOK_STRING TOK_CHAR exp_str TOK_FLAG TOK_ONOFF { int o = gengetopt_add_option ($2, $3, $4, ARG_FLAG, $6, 0, 0, 0, 0); check_result; }
;

exp: TOK_GROUPOPTION TOK_STRING TOK_CHAR exp_str grp_value { int o = gengetopt_add_option ($2, $3, $4, 0, 0, 0, 0, $5, 0); check_result; }
;

exp: TOK_GROUPOPTION TOK_STRING TOK_CHAR exp_str TOK_ARGTYPE def_value grp_value 
{ 
  int o = gengetopt_add_option ($2, $3, $4, $5, 0, 0, $6, $7, 0);
  check_result;
}
;

exp: TOK_OPTION TOK_STRING TOK_CHAR exp_str TOK_ARGTYPE def_value exp_yesno exp_multiple
{ 
  int o = gengetopt_add_option ($2, $3, $4, $5, 0, $7, $6, 0, $8); 
  check_result;
}
;

def_value: { $$ = 0; }
         | TOK_DEFAULT '=' TOK_STRING { $$ = $3; }
;

grp_value: { $$ = 0; }
         | TOK_GROUP '=' TOK_STRING { $$ = $3; }
;

%%


