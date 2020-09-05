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
*	File:	WorldInfoNode.h
*
******************************************************************/

#ifndef _WORLDINFO_H_
#define _WORLDINFO_H_

#include "vrmlfields.h"
#include "Node.h"

class WorldInfoNode : public Node {

public:

	WorldInfoNode() {
		setHeaderFlag(false);
		setType(worldInfoNodeString);

		// title exposed field
		SFString *title = new SFString("");
		addField(titleFieldString, title);

		// info exposed field
		MFString *info = new MFString();
		addField(infoFieldString, info);
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	WorldInfoNode *next() {
		return (WorldInfoNode *)Node::next(getType());
	}

	WorldInfoNode *nextTraversal() {
		return (WorldInfoNode *)Node::nextTraversalByType(getType());
	}

	////////////////////////////////////////////////
	//	Title
	////////////////////////////////////////////////
	
	void setTitle(String value) {
		SFString *title = (SFString *)getField(titleFieldString);
		title->setValue(value);
	}
	String getTitle() {
		SFString *title = (SFString *)getField(titleFieldString);
		return title->getValue();
	}

	////////////////////////////////////////////////
	// Info
	////////////////////////////////////////////////

	void addInfo(String value) {
		MFString *info = (MFString *)getField(infoFieldString);
		info->addValue(value);
	}
	int getNInfos() {
		MFString *info = (MFString *)getField(infoFieldString);
		return info->getSize();
	}
	String getInfo(int index) {
		MFString *info = (MFString *)getField(infoFieldString);
		return info->get1Value(index);
	}

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
	//	Infomation
	////////////////////////////////////////////////

	void outputContext(ostream& printStream, String indentString) {
		SFString *title = (SFString *)getField(titleFieldString);
		printStream << indentString << "\t" << "title " << title << endl;

		if (0 < getNInfos()) {
			MFString *info = (MFString *)getField(infoFieldString);
			printStream <<  indentString << "\t" << "info ["  << endl;
			info->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}
	}
};

#endif

