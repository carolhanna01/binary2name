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
*	File:	Event.cpp
*
******************************************************************/

#include "SceneGraph.h"
#include "UrlFile.h"

////////////////////////////////////////////////
//	find node
////////////////////////////////////////////////

Node *Parser::findNodeByType(char *typeName) {
	if (!typeName)
		return NULL;
	if (strlen(typeName) <= 0)
		return NULL;
	Node *node = getRootNode()->nextTraversalByType(typeName);
	if (node) {
		while (node->isInstanceNode() == true)
			node = node->getReferenceNode();
	}
	return node;
}

Node *Parser::findNodeByName(char *name) {
	if (!name)
		return NULL;
	if (strlen(name) <= 0)
		return NULL;
	Node *node = getRootNode()->nextTraversalByName(name);
	if (node) {
		while (node->isInstanceNode() == true)
			node = node->getReferenceNode();
	}
	return node;
}

////////////////////////////////////////////////
//	Parser::getNLines
////////////////////////////////////////////////

int Parser::getNLines(char *fileName) 
{
	FILE *fp;
	if ((fp = fopen(fileName, "rt")) == NULL){
		fprintf(stderr, "Cannot open data file %s\n", fileName);
		return 0;
	}

	char lineBuffer[DEFAULT_LEX_LINE_BUFFER_SIZE+1];

	int nLine = 0;
	while (fgets(lineBuffer, DEFAULT_LEX_LINE_BUFFER_SIZE, fp))
		nLine++;

	fclose(fp);

	return nLine;
}

////////////////////////////////////////////////
//	Parser::load
////////////////////////////////////////////////

void Parser::load(char *fileName, void (*callbackFn)(int nLine, void *info), void *callbackFnInfo) 
{
	FILE *fp = fopen(fileName, "rt");

#ifdef SUPPORT_URL
	SceneGraph *sg = (SceneGraph *)this; //JMC
	if (fp == NULL){
		if (sg->getUrlStream(fileName)) {
			char *outputFilename = sg->getUrlOutputFilename();
			fp = fopen(outputFilename, "rt");
			sg->setUrl(fileName);
		}
	}
#endif

	if (fp == NULL) {
		fprintf(stderr, "Cannot open data file %s\n", fileName);
		setParserResult(false);
		setParseringState(false);
		return;
	}

	fseek(fp, 0, SEEK_END);
	int lexBufferSize = ftell(fp);
	fseek(fp, 0, SEEK_SET);

	if (GetParserObject() == NULL)
		MakeLexerBuffers(lexBufferSize, DEFAULT_LEX_LINE_BUFFER_SIZE);

	mParserNodeList.deleteNodes();
	deleteDEFs();
	deletePROTOs();

	PushParserObject(this);

	SetLexCallbackFn(callbackFn, callbackFnInfo);

	setErrorLineNumber(0);
	setErrorLineString("");
	setErrorToken("");

    SetInputFile(fp);

	setParseringState(true);

    setParserResult(!yyparse() ? true : false);

	setParseringState(false);

	PopParserObject();

	if (GetParserObject() == NULL)
		DeleteLexerBuffers();


	fclose(fp);

#ifdef SUPPORT_URL
	sg->deleteUrlOutputFilename();
#endif
}

////////////////////////////////////////////////
//	Parser::addNode
////////////////////////////////////////////////

void Parser::addNode(Node *node, bool initialize) {
	moveNode(node);
	if (initialize)
		node->initialize();
}

////////////////////////////////////////////////
//	Parser::addNodeAtFirst
////////////////////////////////////////////////

void Parser::addNodeAtFirst(Node *node, bool initialize) {
	moveNodeAtFirst(node);
	if (initialize)
		node->initialize();
}

////////////////////////////////////////////////
//	Parser::moveNode
////////////////////////////////////////////////

void Parser::moveNode(Node *node) {
	Node *parentNode = getCurrentNode();
	if (!parentNode || !getParseringState())
		mNodeList.addNode(node);
	else
		parentNode->moveChildNode(node);

	node->setParentNode(parentNode);
	node->setSceneGraph((SceneGraph *)this);
}

////////////////////////////////////////////////
//	Parser::moveNodeAtFirst
////////////////////////////////////////////////

void Parser::moveNodeAtFirst(Node *node) {
	Node *parentNode = getCurrentNode();
	if (!parentNode || !getParseringState())
		mNodeList.addNodeAtFirst(node);
	else
		parentNode->moveChildNodeAtFirst(node);

	node->setParentNode(parentNode);
	node->setSceneGraph((SceneGraph *)this);
}
