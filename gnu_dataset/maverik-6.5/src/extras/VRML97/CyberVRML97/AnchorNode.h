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
*	File:	AnchorNode.h
*
******************************************************************/

#ifndef _ANCHOR_H_
#define _ANCHOR_H_

#include "vrmlfields.h"
#include "GroupingNode.h"

//// ExposedField ////////////////
#define	descriptionFieldString		"description"
#define	parameterFieldString		"parameter"
#define	urlFieldString				"url"

class AnchorNode : public GroupingNode {

public:

	AnchorNode() {
		setHeaderFlag(false);
		setType(anchorNodeString);

		///////////////////////////
		// Exposed Field 
		///////////////////////////

		// description exposed field
		SFString *description = new SFString("");
		addExposedField(descriptionFieldString, description);

		// parameter exposed field
		MFString *parameter = new MFString();
		addExposedField(parameterFieldString, parameter);

		// url exposed field
		MFString *url = new MFString();
		addExposedField(urlFieldString, url);
	}

	~AnchorNode() {
	}

	////////////////////////////////////////////////
	//	Description
	////////////////////////////////////////////////

	void setDescription(String value) {
		SFString *description = (SFString *)getExposedField(descriptionFieldString);
		description->setValue(value);
	}

	String getDescription() {
		SFString *description = (SFString *)getExposedField(descriptionFieldString);
		return description->getValue();
	}

	////////////////////////////////////////////////
	// Parameter
	////////////////////////////////////////////////

	void addParameter(String value) {
		MFString *parameter = (MFString *)getExposedField(parameterFieldString);
		parameter->addValue(value);
	}
	int getNParameters() {
		MFString *parameter = (MFString *)getExposedField(parameterFieldString);
		return parameter->getSize();
	}
	String getParameter(int index) {
		MFString *parameter = (MFString *)getExposedField(parameterFieldString);
		return parameter->get1Value(index);
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

	AnchorNode *next() {
		return (AnchorNode *)Node::next(getType());
	}

	AnchorNode *nextTraversal() {
		return (AnchorNode *)Node::nextTraversalByType(getType());
	}

	////////////////////////////////////////////////
	//	virtual functions
	////////////////////////////////////////////////

	bool isChildNodeType(Node *node){
		if (node->isCommonNode() || node->isBindableNode() ||node->isInterpolatorNode() || node->isSensorNode() || node->isGroupingNode() || node->isSpecialGroupNode())
			return true;
		else
			return false;
	}

	void initialize() {
		recomputeBoundingBox();
	}

	void uninitialize() {
	}

	void update() {
	}

	void outputContext(ostream &printStream, String indentString) {
		SFString *description = (SFString *)getExposedField(descriptionFieldString);
		printStream << indentString << "\t" << "description " << description << endl;
		
		if (0 < getNParameters()) {
			MFString *parameter = (MFString *)getExposedField(parameterFieldString);
			printStream << indentString << "\t" << "parameter ["  << endl;
			parameter->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}

		if (0 < getNUrls()) {
			MFString *url = (MFString *)getExposedField(urlFieldString);
			printStream << indentString << "\t" << "url [" << endl;
			url->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}
	}

};

#endif
