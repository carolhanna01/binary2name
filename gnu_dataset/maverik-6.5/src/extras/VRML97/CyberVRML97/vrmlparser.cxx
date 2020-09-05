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

/******************************************************************
*
*	VRML library for C++
*
*	Copyright (C) Satoshi Konno 1996-1997
*
*	File:	vrmlparser.cpp
*
******************************************************************/

#include <string.h>
#include "SceneGraph.h"

/******************************************************************
*
*	lex action
*
******************************************************************/

/******************************************************************
*	DEF action
******************************************************************/

void AddDEFInfo(
char *name,
char *string)
{
	((Parser *)GetParserObject())->addDEF(name, string);
}

char *GetDEFSrting(
char *name)
{
	return ((Parser *)GetParserObject())->getDEFString(name);
}

/******************************************************************
*	PROTO action
******************************************************************/

PROTO *AddPROTOInfo(
char *name,
char *string,
char *fieldString)
{
	PROTO *proto = new PROTO(name, string, fieldString);
	((Parser *)GetParserObject())->addPROTO(proto);
	return proto;
}

PROTO *IsPROTOName(
char *name)
{
	return ((Parser *)GetParserObject())->getPROTO(name);
}

/******************************************************************
*
*	Node Name
*
******************************************************************/

#define	MAX_DEFNAME	128
static char	gDEFName[MAX_DEFNAME];

void SetDEFName(char *name)
{
	((Parser *)GetParserObject())->setDefName(name);
}

char *GetDEFName(void)
{
	char *defName = ((Parser *)GetParserObject())->getDefName();
	if (defName)
		strcpy(gDEFName, defName);
	SetDEFName(NULL);
	if (defName)
		return gDEFName;
	else
		return NULL;
}

/******************************************************************
*
*	AddRouteInfo
*
******************************************************************/

#define ROUTE_STRING_MAX	512

void AddRouteInfo(char *string)
{
	static char targetNodeName[ROUTE_STRING_MAX];
	static char sourceNodeName[ROUTE_STRING_MAX];
	static char targetNodeTypeName[ROUTE_STRING_MAX];
	static char sourceNodeTypeName[ROUTE_STRING_MAX];

	if (!string || !strlen(string))
		return;

	for (int n=0; n<(int)strlen(string); n++) {
		if (string[n] == '.')
			string[n] = ' ';
	}

	sscanf(string, "%s %s TO %s %s", sourceNodeName, sourceNodeTypeName, targetNodeName, targetNodeTypeName);

	((Parser *)GetParserObject())->addRoute(sourceNodeName, sourceNodeTypeName, targetNodeName, targetNodeTypeName);
}

/******************************************************************
*
*	New for yacc action
*
******************************************************************/

static CLinkedList<Parser> mParserList;

void PushParserObject(Parser *parser)
{
	mParserList.addNode(parser);
}

void PopParserObject()
{
	Parser *lastNode = mParserList.getLastNode(); 
	lastNode->remove();
}

Parser *GetParserObject()
{
	return mParserList.getLastNode(); 
}

/*

static Parser *gParserObject = NULL;

void SetParserObject(Parser *parser)
{
	gParserObject = parser; 
}

Parser *GetParserObject(void)
{
	return gParserObject; 
}
*/

int GetCurrentNodeType(void)
{
	return ((Parser *)GetParserObject())->getCurrentNodeType();
}

int GetPrevNodeType(void)
{
	return ((Parser *)GetParserObject())->getPrevNodeType();
}

Node *GetCurrentNodeObject(void)
{
	return ((Parser *)GetParserObject())->getCurrentNode();
}

void PushNode(int parserType, Node *node)
{
	((Parser *)GetParserObject())->pushNode(node, parserType);
}

void PopNode(void)
{
	((Parser *)GetParserObject())->popNode();
}

void AddNode(Node *node)
{
	((Parser *)GetParserObject())->addNode(node, 0);
}
