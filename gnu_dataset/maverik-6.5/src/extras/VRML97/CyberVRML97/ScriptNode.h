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
*	File:	ScriptNode.h
*
******************************************************************/

#ifndef _SCRIPT_H_
#define _SCRIPT_H_

#include "vrmlfields.h"
#include "Node.h"
#include "JString.h"
#include "CJavaVM.h"
#include "JScript.h"

#ifdef SUPPORT_JSAI
class ScriptNode : public Node, public CJavaVM { 
#else
class ScriptNode : public Node { 
#endif

#ifdef SUPPORT_JSAI
	JScript			*mpJScriptNode;
#endif

public:

	ScriptNode();

	~ScriptNode();

	////////////////////////////////////////////////
	// Initialization
	////////////////////////////////////////////////

	// This method is called before any event is generated
	void initialize();

	void uninitialize();

	////////////////////////////////////////////////
	// DirectOutput
	////////////////////////////////////////////////

	void setDirectOutput(bool  value) {
		SFBool *directOutput = (SFBool *)getField(directOutputFieldString);
		directOutput->setValue(value);
	}
	void setDirectOutput(int value) {
		setDirectOutput(value ? true : false);
	}
	bool  getDirectOutput() {
		SFBool *directOutput = (SFBool *)getField(directOutputFieldString);
		return directOutput->getValue();
	}

	////////////////////////////////////////////////
	// MustEvaluate
	////////////////////////////////////////////////

	void setMustEvaluate(bool  value) {
		SFBool *mustEvaluate = (SFBool *)getField(mustEvaluateFieldString);
		mustEvaluate->setValue(value);
	}
	void setMustEvaluate(int value) {
		setMustEvaluate(value ? true : false);
	}
	bool  getMustEvaluate() {
		SFBool *mustEvaluate = (SFBool *)getField(mustEvaluateFieldString);
		return mustEvaluate->getValue();
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

	ScriptNode *next() {
		return (ScriptNode *)Node::next(getType());
	}

	ScriptNode *nextTraversal() {
		return (ScriptNode *)Node::nextTraversalByType(getType());
	}

	////////////////////////////////////////////////
	//	virtual function
	////////////////////////////////////////////////

	bool isChildNodeType(Node *node){
		return false;
	}

	////////////////////////////////////////////////
	//	update
	////////////////////////////////////////////////

	void update() {
	}

	////////////////////////////////////////////////
	//	output
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
		SFBool *directOutput = (SFBool *)getField(directOutputFieldString);
		SFBool *mustEvaluate = (SFBool *)getField(mustEvaluateFieldString);

		printStream << indentString << "\t" << "directOutput " << directOutput << endl;
		printStream << indentString << "\t" << "mustEvaluate " << mustEvaluate << endl;

		if (0 < getNUrls()) {
			MFString *url = (MFString *)getExposedField(urlFieldString);
			printStream << indentString << "\t" << "url [" << endl;
			url->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}
		
		int	n;

		for (n=0; n<getNEventIn(); n++) {
			Field *field = getEventIn(n);
			printStream << indentString << "\t" << "eventIn " << field->getTypeName() << " " << ((field->getName() && strlen(field->getName())) ? field->getName() : "NONE") << endl;
		}

		for (n=0; n<getNFields(); n++) {
			Field *field = getField(n);
			JString fieldName(field->getName());
			if (fieldName.compareTo(directOutputFieldString) != 0 && fieldName.compareTo(mustEvaluateFieldString) != 0) {
				if (field->getType() == fieldTypeSFNode) {
					Node	*node = ((SFNode *)field)->getValue();
					char	*nodeName = NULL;
					if (node)
						nodeName = node->getName();
					if (nodeName && strlen(nodeName))
						printStream << indentString << "\t" << "field " << "SFNode" << " " << ((field->getName() && strlen(field->getName())) ? field->getName() : "NONE") << " USE " << nodeName << endl;
					else
						printStream << indentString << "\t" << "field " << "SFNode" << " " << ((field->getName() && strlen(field->getName())) ? field->getName() : "NONE") << " NULL" << endl;
				}
				else
					printStream << indentString << "\t" << "field " << field->getTypeName() << " " << ((field->getName() && strlen(field->getName())) ? field->getName() : "NONE") << " " << field << endl;
			}
		}

		for (n=0; n<getNEventOut(); n++) {
			Field *field = getEventOut(n);
			printStream << indentString << "\t" << "eventOut " << field->getTypeName() << " " << ((field->getName() && strlen(field->getName())) ? field->getName() : "NONE") << endl;
		}
	}

	////////////////////////////////////////////////
	// JSAI
	////////////////////////////////////////////////

#ifdef SUPPORT_JSAI

	int hasScript() {
		return getJavaNode() ? 1 : 0;
	}

	JScript	*getJavaNode()	{return mpJScriptNode;}

#endif

	////////////////////////////////////////////////
	// Update Java Fields
	////////////////////////////////////////////////

	void	update(Field *eventInField);
	void	updateFields();
};

#endif

