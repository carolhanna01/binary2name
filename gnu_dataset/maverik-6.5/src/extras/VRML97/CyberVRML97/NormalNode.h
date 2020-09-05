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
*	File:	NormalNode.h
*
******************************************************************/

#ifndef _NORMAL_H_
#define _NORMAL_H_

#include "vrmlfields.h"
#include "Node.h"

class NormalNode : public Node {

public:

        MFVec3f *mf; // JMC

	NormalNode () {
	        mf=NULL; // JMC

		setHeaderFlag(false);
		setType(normalNodeString);

		// vector exposed field
		MFVec3f *vector = new MFVec3f();
		vector->setName(vectorFieldString);
		addExposedField(vector);
	}

	~NormalNode () {
	}

	////////////////////////////////////////////////
	//	vector
	////////////////////////////////////////////////
	
	void addVector(float value[]) {
		MFVec3f *vector = (MFVec3f *)getExposedField(vectorFieldString);
		vector->addValue(value);
	}
	int getNVectors() {
		MFVec3f *vector = (MFVec3f *)getExposedField(vectorFieldString);
		return vector->getSize();
	}
	void getVector(int index, float value[]) {
		MFVec3f *vector = (MFVec3f *)getExposedField(vectorFieldString);
		vector->get1Value(index, value);
	}
	void getVectorNext(float value[]) { // JMC
	  if (!mf) mf= (MFVec3f *)getExposedField(vectorFieldString); // JMC
	  mf->get1ValueNext(value); // JMC
	} // JMC

	////////////////////////////////////////////////
	//	functions
	////////////////////////////////////////////////
	
	bool isChildNodeType(Node *node){
		return false;
	}

	void initialize() {
	}

	void uninitialize() {
	}

	void update() {
	}

	////////////////////////////////////////////////
	//	Output
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
		if (0 < getNVectors()) {
			MFVec3f *vector = (MFVec3f *)getExposedField(vectorFieldString);
			printStream <<  indentString << "\t" << "vector ["  << endl;
			vector->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	NormalNode *next() {
		return (NormalNode *)Node::next(getType());
	}

	NormalNode *nextTraversal() {
		return (NormalNode *)Node::nextTraversalByType(getType());
	}

};

#endif

