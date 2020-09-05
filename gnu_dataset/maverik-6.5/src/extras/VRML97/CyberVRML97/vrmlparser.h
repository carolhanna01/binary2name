/*
   GNU Maverik - a system for managing display and interaction in 
              Virtual Environment applications.
   Copyright (C) 2008  Advanced Interfaces Group

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.

   The authors can be contacted via:
   www   - http://aig.cs.man.ac.uk
   email - maverik@aig.cs.man.ac.uk
   mail  - Advanced Interfaces Group, Room 2.94, Kilburn Building, 
        University of Manchester, Manchester, M13 9PL, UK
*/


#include "mavlib_cvcomp.h"

/******************************************************************
*
*	VRML library for C++
*
*	Copyright (C) Satoshi Konno 1996-1997
*
*	File:	vrmlparser.h
*
******************************************************************/

#ifndef _VRMLPARSER_H_
#define _VRMLPARSER_H_

#include <stdio.h>
#include "SceneGraph.h"

/******************************************************************
*
*	For lex action 
*
******************************************************************/

class	PROTO;

PROTO	*AddPROTOInfo(char *name, char *string, char *fieldString);
PROTO	*IsPROTOName(char *name);

void	AddDEFInfo(char *name, char *string);
char	*GetDEFSrting(char *name);

void	SetDEFName(char *name);
char	*GetDEFName(void);

void	MakeLexerBuffers(int lexBufferSize, int lineBufferSize);
void	DeleteLexerBuffers(void);

void	SetLexCallbackFn(void (*func)(int nLine, void *info), void *fnInfo);

/******************************************************************
*
*	For yacc action 
*
******************************************************************/

class Parser;
class Node;

void	PushParserObject(Parser *parser);
void	PopParserObject();
Parser	*GetParserObject();
/*
void	SetParserObject(Parser *parser);
Parser	*GetParserObject(void);
*/

int		GetCurrentNodeType(void);
int		GetPrevNodeType(void);
Node	*GetCurrentNodeObject(void);

void	PushNode(int parserType, Node *node);
void	PopNode(void);

void	AddNode(Node *node);

char	*GetDEFName(void);

int		yyparse();

void	AddRouteInfo(char *string);

/******************************************************************
*
*	For yacc action 
*
******************************************************************/

int		GetCurrentLineNumber(void);
void	SetInputFile(FILE *fp);
char	*GetErrorLineString(void);

#endif //JMC
