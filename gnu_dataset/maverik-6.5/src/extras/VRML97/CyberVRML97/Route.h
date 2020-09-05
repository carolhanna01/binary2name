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
*	File:	Route.h
*
******************************************************************/

#ifndef _ROUTE_H_
#define _ROUTE_H_

#include <iostream.h>
#include "CLinkedList.h"
#include "vrmlfields.h"
#include "vrmlnodes.h"
#include "CJavaVM.h"

#ifdef SUPPORT_JSAI
class Route : public CLinkedListNode<Route>, public CJavaVM {
#else
class Route : public CLinkedListNode<Route> {
#endif

	Node	*mEventOutNode;
	Node	*mEventInNode;
	Field	*mEventOutField;
	Field	*mEventInField;
	
	int		mIsActive;

public:

	Route(Node *eventOutNode, Field *eventOutField, Node *eventInNode, Field *eventInField) {
		setHeaderFlag(false);
		setEventOutNode(eventOutNode);
		setEventInNode(eventInNode);
		setEventOutField(eventOutField);
		setEventInField(eventInField);

		initialize();
	}

	~Route() { 
		remove();
	}

	void		setEventOutNode(Node *node)			{ mEventOutNode = node; }
	void		setEventInNode(Node *node)			{ mEventInNode = node; }
	Node		*getEventOutNode()					{ return mEventOutNode; }
	Node		*getEventInNode()					{ return mEventInNode; }
	void		setEventOutField(Field *field)		{ mEventOutField = field; }
	Field		*getEventOutField()					{ return mEventOutField; }
	void		setEventInField(Field *field)		{ mEventInField = field; }
	Field		*getEventInField()					{ return mEventInField; }

	////////////////////////////////////////////////
	//	Active
	////////////////////////////////////////////////

	void	setIsActive(int active) {
		mIsActive = active;
	}

	int		isActive() {
		return mIsActive;
	}

	////////////////////////////////////////////////
	//	update
	////////////////////////////////////////////////

	void	initialize() {
		setIsActive(0);
	}

	void	update();

	////////////////////////////////////////////////
	//	output
	////////////////////////////////////////////////

	void output(ostream& printStream) {
		
		if (!getEventOutNode() && !getEventOutField() && !getEventInNode() && !getEventInField())
			return;

		printStream << "ROUTE ";
		printStream << getEventOutNode()->getName() << "." << getEventOutField()->getName() << " TO ";
		printStream << getEventInNode()->getName() << "." << getEventInField()->getName();
		printStream << endl; 
	}
};

#endif
