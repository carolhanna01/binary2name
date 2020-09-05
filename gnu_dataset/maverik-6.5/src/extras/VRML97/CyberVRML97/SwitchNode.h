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
*	File:	SwitchNode.h
*
******************************************************************/

#ifndef _SWITCH_H_
#define _SWITCH_H_

#include "vrmlfields.h"
#include "Node.h"
#include "JVector.h"

void UpdateSwitch(SwitchNode *snode);
void InitializeSwitch(SwitchNode *snode);
void UninitializeSwitch(SwitchNode *snode);

class SwitchNode : public Node {

public:

	SwitchNode() {
		setHeaderFlag(false);
		setType(switchNodeString);

		// whichChoice field
		SFInt32 *whichChoice = new SFInt32(-1);
		addField(whichChoiceFieldString, whichChoice);
	}

	~SwitchNode() {
	}

	////////////////////////////////////////////////
	//	whichChoice
	////////////////////////////////////////////////

	void setWhichChoice(int value) {
		SFInt32 *whichChoice = (SFInt32 *)getField(whichChoiceFieldString);
		whichChoice->setValue(value);
	}

	int getWhichChoice() {
		SFInt32 *whichChoice = (SFInt32 *)getField(whichChoiceFieldString);
		return whichChoice->getValue();
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	SwitchNode *next() {
		return (SwitchNode *)Node::next(getType());
	}

	SwitchNode *nextTraversal() {
		return (SwitchNode *)Node::nextTraversalByType(getType());
	}

	////////////////////////////////////////////////
	//	functions
	////////////////////////////////////////////////
	
	bool isChildNodeType(Node *node){
		if (node->isCommonNode() || node->isBindableNode() ||node->isInterpolatorNode() || node->isSensorNode() || node->isGroupingNode() || node->isSpecialGroupNode())
			return true;
		else
			return false;
	}

	void initialize();

	void uninitialize();

	void update();

	////////////////////////////////////////////////
	//	Infomation
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
		printStream << indentString << "\t" << "whichChoice " << getWhichChoice() << endl;
	}
};

#endif

