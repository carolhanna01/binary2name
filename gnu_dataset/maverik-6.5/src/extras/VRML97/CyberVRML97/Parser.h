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
*	File:	Parser.h
*
******************************************************************/

#ifndef _PARSER_H_
#define _PARSER_H_

#include <assert.h>
#include "vrmlfields.h"
#include "vrmlnodes.h"
#include "vrmlparser.h"
#include "NodeList.h"
#include "Route.h"
#include "ParserNode.h"
#include "JString.h"
#include "DEF.h"
#include "PROTO.h"

#define DEFAULT_LEX_LINE_BUFFER_SIZE	1024

class	SceneGraph;

class Parser : public CLinkedListNode<Parser>{

	NodeList				mNodeList;
	CLinkedList<Route>		mRouteList;
	CLinkedList<ParserNode>	mParserNodeList;
	CLinkedList<DEF>		mDEFList;
	CLinkedList<PROTO>		mPROTOList;
	JString					mDefName;

	int						mErrorLineNumber;
	JString					mErrorToken;
	JString					mErrorLineString;
	JString					mErrorReason; // JMC
	bool					mIsOK;
	bool					mbParsering;
public:

	Parser () {
		setParserResult(false);
		setParseringState(false);
		mErrorLineNumber=-1; // JMC
	}

	Node *getRootNode() {
		return (Node *)mNodeList.getRootNode();		
	}

	Node *getNodes() {
		return (Node *)mNodeList.getNodes();		
	}

	///////////////////////////////////////////////
	//	Load
	///////////////////////////////////////////////

	void clearNodeList() {
		mNodeList.deleteNodes();		
	}

	void clearRouteList() {
		mRouteList.deleteNodes();		
	}

	////////////////////////////////////////////////
	//	find node
	////////////////////////////////////////////////

	Node *findNodeByType(char *typeName);

	Node *findNodeByName(char *name);

	///////////////////////////////////////////////
	//	Praser action
	///////////////////////////////////////////////

	void addNode(Node *node, bool initialize = true);
	void addNodeAtFirst(Node *node, bool initialize = true);

	void moveNode(Node *node);
	void moveNodeAtFirst(Node *node);

	void pushNode(Node *node, int type)
	{
		ParserNode *parserNode = new ParserNode(node, type);
		mParserNodeList.addNode(parserNode);
	}

	void popNode()
	{
		ParserNode *lastNode = mParserNodeList.getLastNode(); 
		delete lastNode;
	}

	Node *getCurrentNode() {
		ParserNode *lastNode = mParserNodeList.getLastNode(); 
		if (!lastNode)
			return NULL;
		else
			return lastNode->getNode();
	}

	int getCurrentNodeType() {
		ParserNode *lastNode = mParserNodeList.getLastNode(); 
		if (!lastNode)
			return 0;
		else
			return lastNode->getType();
	}

	int getPrevNodeType() {
		ParserNode *lastNode = mParserNodeList.getLastNode(); 
		if (!lastNode)
			return 0;
		else {
			ParserNode *prevNode = lastNode->prev(); 
			if (prevNode->isHeaderNode())
				return 0;
			else
				return prevNode->getType();
		}
	}

	///////////////////////////////////////////////
	//	DEF
	///////////////////////////////////////////////

	void setDefName(char *name) {
		mDefName.setValue(name);
	}

	char *getDefName() {
		return mDefName.getValue();
	}

	///////////////////////////////////////////////
	//	for lex & yacc
	///////////////////////////////////////////////

	void setParserResult(bool bOK) { 
		mIsOK = bOK; 
	}
	bool isOK(void) {
		return mIsOK; 
	}

	void setErrorLineNumber(int n) { 
		mErrorLineNumber = n; 
	}
	int	getErrorLineNumber(void){
		return mErrorLineNumber; 
	}

	void setErrorToken(char *error) {
		mErrorToken.setValue(error); 
	}
	char *getErrorToken(void) { 
		return mErrorToken.getValue(); 
	}

	void setErrorReason(char *error) {//JMC
		mErrorReason.setValue(error); //JMC
	}//JMC
	char *getErrorReason(void) { //JMC
		return mErrorReason.getValue(); //JMC
	}//JMC

	void setErrorLineString(char *error) { 
		mErrorLineString.setValue(error); 
	}
	char *getErrorLineString(void) {
		return mErrorLineString.getValue(); 
	}

	///////////////////////////////////////////////
	//	Load
	///////////////////////////////////////////////

	void	setParseringState(bool state)	{ mbParsering = state; }
	bool	getParseringState()				{ return mbParsering; }

	int		getNLines(char *fileName);
	void	load(char *fileName, void (*callbackFn)(int nLine, void *info) = NULL, void *callbackFnInfo = NULL);

	///////////////////////////////////////////////
	//	DEF
	///////////////////////////////////////////////

	DEF *getDEFs() {
		return (DEF *)mDEFList.getNodes();
	}

	char *getDEFString(char *name) {
		for (DEF *def=getDEFs(); def; def=def->next()) {
			char *defName = def->getName();
			if (defName && !strcmp(defName, name))
				return def->getString();
		}
		return NULL;
	}

	void addDEF(DEF *def) {
		mDEFList.addNode(def);
	}
	
	void addDEF(char *name, char *string) {
		DEF *def = new DEF(name, string);
		addDEF(def);
	}

	void deleteDEFs() {
		mDEFList.deleteNodes();
	}

	///////////////////////////////////////////////
	//	PROTO
	///////////////////////////////////////////////

	PROTO *getPROTOs() {
		return (PROTO *)mPROTOList.getNodes();
	}

	PROTO *getPROTO(char *name) {
		if (!name || !strlen(name))
			return NULL;

		for (PROTO *proto=getPROTOs(); proto; proto=proto->next()) {
			char *protoName = proto->getName();
			if (protoName && !strcmp(protoName, name))
				return proto;
		}
		return NULL;
	}

	void addPROTO(PROTO *proto) {
		mPROTOList.addNode(proto);
	}

	void deletePROTOs() {
		mPROTOList.deleteNodes();
	}
	
	///////////////////////////////////////////////
	//	ROUTE
	///////////////////////////////////////////////

	Route *getRoutes() {
		return (Route *)mRouteList.getNodes();
	}

	Route *getRoute(Node *eventOutNode, Field *eventOutField, Node *eventInNode, Field *eventInField)
	{
		for (Route *route=getRoutes(); route; route=route->next()) {
			if (eventOutNode == route->getEventOutNode() && eventOutField == route->getEventOutField() &&
				eventInNode == route->getEventInNode() && eventInField == route->getEventInField() ) {
				return route;
			}
		}
		return NULL;
	}

	void addRoute(Route *route) {
		if (route->getEventOutNode() == route->getEventInNode())
			return;
		if (getRoute(route->getEventOutNode(), route->getEventOutField(), route->getEventInNode(), route->getEventInField()))
			return;
		mRouteList.addNode(route);
	}

	void addRoute(char *eventOutNodeName, char *eventOutFieldName, char *eventInNodeName, char *eventInFieldName)
	{
		Node *eventInNode = findNodeByName(eventInNodeName);
		Node *eventOutNode = findNodeByName(eventOutNodeName);

		Field *eventOutField = NULL;

		if (eventOutNode) {
			eventOutField = eventOutNode->getEventOut(eventOutFieldName);
			if (!eventOutField)
				eventOutField = eventOutNode->getExposedField(eventOutFieldName);
		}

		Field *eventInField = NULL;

		if (eventInNode) {
			eventInField = eventInNode->getEventIn(eventInFieldName);
			if (!eventInField)
				eventInField = eventInNode->getExposedField(eventInFieldName);
		}
		
		if (!eventInNode || !eventOutNode || !eventInField || !eventOutField)
			return;

		Route *route = new Route(eventOutNode, eventOutField, eventInNode, eventInField);
		addRoute(route);
	}

	void addRoute(Node *eventOutNode, Field *eventOutField, Node *eventInNode, Field *eventInField)
	{
		Route *route = new Route(eventOutNode, eventOutField, eventInNode, eventInField);
		addRoute(route);
	}

	void deleteRoute(Node *eventOutNode, Field *eventOutField, Node *eventInNode, Field *eventInField)
	{
		Route *route  = getRoute(eventOutNode, eventOutField, eventInNode, eventInField);
		if (route)
			delete route;
	}

	void deleteRoutes(Node *node) {
		Route *route = getRoutes();
		while (route) {
			Route *nextRoute = route->next();
			if (node == route->getEventInNode() || node == route->getEventOutNode())
				delete route;
			route = nextRoute;
		}
	}

	void deleteEventInFieldRoutes(Node *node, Field *field)
	{
		Route	*route = getRoutes();
		while (route) {
			Route *nextRoute = route->next();
			if (route->getEventInNode() == node && route->getEventInField() == field)
				delete route;
			route = nextRoute;
		}
	}

	void deleteEventOutFieldRoutes(Node *node, Field *field)
	{
		Route	*route = getRoutes();
		while (route) {
			Route *nextRoute = route->next();
			if (route->getEventOutNode() == node && route->getEventOutField() == field)
				delete route;
			route = nextRoute;
		}
	}

	void deleteRoutes(Node *node, Field *field)
	{
		deleteEventInFieldRoutes(node, field);
		deleteEventOutFieldRoutes(node, field);
	}

	void deleteRoute(Route *deleteRoute)
	{
		for (Route *route=getRoutes(); route; route=route->next()) {
			if (deleteRoute == route) {
				delete route;
				return;
			}
		}
	}
};

#endif


