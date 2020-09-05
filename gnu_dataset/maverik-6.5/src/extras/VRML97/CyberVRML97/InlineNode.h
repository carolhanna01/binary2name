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
*	File:	InlineNode.h
*
******************************************************************/

#ifndef _INLINE_H_
#define _INLINE_H_

#include "vrmlfields.h"
#include "Node.h"
#include "GroupingNode.h"

class InlineNode : public GroupingNode {

public:

	InlineNode() {
		setHeaderFlag(false);
		setType(inlineNodeString);

		// url exposed field
		MFString *url = new MFString();
		addExposedField(urlFieldString, url);
	}

	~InlineNode() {
	}

	////////////////////////////////////////////////
	// Url
	////////////////////////////////////////////////

	void addUrl(String value) {
		MFString *url = (MFString *)getExposedField(urlFieldString);
		url->addValue(value);
	}
	int getNUrls() {
		MFString *url = (MFString *)getExposedField(urlFieldString);
		return url->getSize();
	}
	String getUrl(int index) {
		MFString *url = (MFString *)getExposedField(urlFieldString);
		return url->get1Value(index);
	}
	void setUrl(int index, char *urlString) {
		MFString *url = (MFString *)getExposedField(urlFieldString);
		url->set1Value(index, urlString);
	}


	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	InlineNode *next() {
		return (InlineNode *)Node::next(getType());
	}

	InlineNode *nextTraversal() {
		return (InlineNode *)Node::nextTraversalByType(getType());
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
		if (0 < getNUrls()) {
			MFString *url = (MFString *)getExposedField(urlFieldString);
			printStream << indentString << "\t" << "url [" << endl;
			url->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}
	}
};

#endif

