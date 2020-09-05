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
*	File:	DefNode.h
*
******************************************************************/

#ifndef _DEFNODE_H_
#define _DEFNODE_H_

#include "vrmlfields.h"
#include "Node.h"
#include "GroupingNode.h"

class DefNode : public Node {

public:

	DefNode() {
		setHeaderFlag(false);
		setType(defNodeString);
	}

	~DefNode() {
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	DefNode *next() {
		return (DefNode *)Node::next(getType());
	}

	DefNode *nextTraversal() {
		return (DefNode *)Node::nextTraversalByType(getType());
	}

	////////////////////////////////////////////////
	//	functions
	////////////////////////////////////////////////
	
	bool isChildNodeType(Node *node){
		return false;
	}

	void initialize();

	void uninitialize();

	void update() {
	}

	////////////////////////////////////////////////
	//	Infomation
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
	}
};

#endif

