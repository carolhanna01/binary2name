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
#include <string.h>
#include "argsdef.h"
#include "parser.h"
extern int gengetopt_count_line;

static void update_count_line (char *str);
static void updateTokenInfo (int pos);

#define LINEBUF_LEN 1024

int tokenpos = 0;
char linebuf[LINEBUF_LEN] ; /* current line */

%}

ws [ ]+
tabs [\t]+

%%

[Pp][Aa][Cc][Kk][Aa][Gg][Ee]	updateTokenInfo (-1); return TOK_PACKAGE;
[Vv][Ee][Rr][Ss][Ii][Oo][Nn]	updateTokenInfo (-1); return TOK_VERSION;
[Gg][Rr][Oo][Uu][Pp][Oo][Pp][Tt][Ii][Oo][Nn]   updateTokenInfo (-1); return TOK_GROUPOPTION;
[Oo][Pp][Tt][Ii][Oo][Nn]	updateTokenInfo (-1); return TOK_OPTION;
[Ss][Tt][Rr][Ii][Nn][Gg]	updateTokenInfo (-1); yylval.argtype = ARG_STRING; return TOK_ARGTYPE;
[Ii][Nn][Tt]			updateTokenInfo (-1); yylval.argtype = ARG_INT; return TOK_ARGTYPE;
[Ss][Hh][Oo][Rr][Tt]		updateTokenInfo (-1); yylval.argtype = ARG_SHORT; return TOK_ARGTYPE;
[Ll][Oo][Nn][Gg]		updateTokenInfo (-1); yylval.argtype = ARG_LONG; return TOK_ARGTYPE;
[Ff][Ll][Oo][Aa][Tt]		updateTokenInfo (-1); yylval.argtype = ARG_FLOAT; return TOK_ARGTYPE;
[Dd][Oo][Uu][Bb][Ll][Ee]	updateTokenInfo (-1); yylval.argtype = ARG_DOUBLE; return TOK_ARGTYPE;
[Ll][Oo][Nn][Gg][Dd][Oo][Uu][Bb][Ll][Ee]   updateTokenInfo (-1); yylval.argtype = ARG_LONGDOUBLE; return TOK_ARGTYPE;
[Ll][Oo][Nn][Gg][Ll][Oo][Nn][Gg]   updateTokenInfo (-1); yylval.argtype = ARG_LONGLONG; return TOK_ARGTYPE;
[Yy][Ee][Ss]			   updateTokenInfo (-1); return TOK_YES;
[Nn][Oo]			   updateTokenInfo (-1); return TOK_NO;
[Ff][Ll][Aa][Gg]		   updateTokenInfo (-1); return TOK_FLAG;
[Oo][Nn]			   updateTokenInfo (-1); yylval.boolean = 1; return TOK_ONOFF;
[Oo][Ff][Ff]			   updateTokenInfo (-1); yylval.boolean = 0; return TOK_ONOFF;
[Pp][Uu][Rr][Pp][Oo][Ss][Ee]	   updateTokenInfo (-1); return TOK_PURPOSE;
[Dd][Ee][Ff][Aa][Uu][Ll][Tt]       updateTokenInfo (-1); return TOK_DEFAULT;
[Gg][Rr][Oo][Uu][Pp]               updateTokenInfo (-1); return TOK_GROUP;
[Dd][Ee][Ff][Gg][Rr][Oo][Uu][Pp]   updateTokenInfo (-1); return TOK_DEFGROUP;
[Mm][Uu][Ll][Tt][Ii][Pp][Ll][Ee]   updateTokenInfo (-1); return TOK_MULTIPLE;

"=" { updateTokenInfo (-1); return '='; }

[[:alnum:]-]	 updateTokenInfo (-1); yylval.chr = yytext[0]; return TOK_CHAR;
\"[^\"\n]*\"	{
  /* if you add or remove symbols, change canonize_vars
     function */
  updateTokenInfo (-1); 
  yytext [strlen(yytext) - 1] = 0;
  yylval.str = yytext + 1;
  return TOK_STRING;
}

\"[^\"]*\"	{
  /* if you add or remove symbols, change canonize_vars
     function */
  updateTokenInfo (-1); 
  yytext [strlen(yytext) - 1] = 0;
  yylval.str = yytext + 1;
  update_count_line (yylval.str);
  return TOK_MLSTRING;
}

{ws}           updateTokenInfo (-1);
{tabs}         updateTokenInfo (8*yyleng);

#[^\n]*		/* comments begin with # in any place */
\n.*		update_count_line (0); yyless(1) ; /* give back all but the \n to rescan */

\r              {}

.		{ 
  return 1000; /* little hack to produce a parse error too. */ 
}

%%

/*
  Otherwise '\n' within a TOK_MLSTRING would not be counted
*/
void
update_count_line (char *str)
{
  if (str)
    {
      char *p;
      for (p = str; *p; ++p)
        if (*p == '\n')
          ++gengetopt_count_line;
    }
  else
    {
      ++gengetopt_count_line;
      tokenpos = 0 ; /* reset token position */
      strncpy (linebuf, yytext+1, LINEBUF_LEN - 1); /* save the next line */
    }
}

void 
updateTokenInfo( int pos ) 
{
  if ( pos >= 0 )
    tokenpos += pos ;
  else
    tokenpos += yyleng ;
}
