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
*	File:	CollisionNode.h
*
******************************************************************/

#ifndef _COLLISION_H_
#define _COLLISION_H_

#include "vrmlfields.h"
#include "Node.h"

class CollisionNode : public GroupingNode {

public:

	CollisionNode() {
		setHeaderFlag(false);
		setType(collisionNodeString);

		// collide exposed field
		SFBool *collide = new SFBool(true);
		addExposedField(collideFieldString, collide);

		// collide event out
		SFTime *collideTime = new SFTime(-1.0);
		addEventOut(collideTimeFieldString, collideTime);
	}

	~CollisionNode() {
	}

	////////////////////////////////////////////////
	//	collide
	////////////////////////////////////////////////

	void setCollide(bool  value) {
		SFBool *collide = (SFBool *)getExposedField(collideFieldString);
		collide->setValue(value);
	}

	void setCollide(int value) {
		setCollide(value ? true : false);
	}

	bool  getCollide() {
		SFBool *collide = (SFBool *)getExposedField(collideFieldString);
		return collide->getValue();
	}

	////////////////////////////////////////////////
	//	collideTime
	////////////////////////////////////////////////

	void setcollideTime(double value) {
		SFTime *collideTime = (SFTime *)getEventOut(collideTimeFieldString);
		collideTime->setValue(value);
	}

	double getcollideTime() {
		SFTime *collideTime = (SFTime *)getEventOut(collideTimeFieldString);
		return collideTime->getValue();
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	CollisionNode *next() {
		return (CollisionNode *)Node::next(getType());
	}

	CollisionNode *nextTraversal() {
		return (CollisionNode *)Node::nextTraversalByType(getType());
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

	void initialize() {
		//recomputeBoundingBox();JMC
	}

	void uninitialize() {
	}

	void update() {
	}

	////////////////////////////////////////////////
	//	Infomation
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
		SFBool *collide = (SFBool *)getExposedField(collideFieldString);
		printStream << indentString << "\t" << "collide " << collide << endl;
	}
};

#endif

