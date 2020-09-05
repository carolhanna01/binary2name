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

/******************************************************************
*
*	VRML library for C++
*
*	Copyright (C) Satoshi Konno 1996-1997
*
*	File:	ScriptNode.cpp
*
******************************************************************/

#include "ScriptNode.h"
#include "Event.h"

////////////////////////////////////////////////
// ScriptNode::ScriptNode
////////////////////////////////////////////////

ScriptNode::ScriptNode() 
{
	setHeaderFlag(false);
	setType(scriptNodeString);

	// directOutput exposed field
	SFBool *directOutput = new SFBool(false);
	Node::addField(directOutputFieldString, directOutput);

	// directOutput exposed field
	SFBool *mustEvaluate = new SFBool(false);
	Node::addField(mustEvaluateFieldString, mustEvaluate);

	// url exposed field
	MFString *url = new MFString();
	addExposedField(urlFieldString, url);

	// Clear Java object
#ifdef SUPPORT_JSAI
	mpJScriptNode = NULL;
#endif
}


////////////////////////////////////////////////
// ScriptNode::~ScriptNode
////////////////////////////////////////////////

ScriptNode::~ScriptNode() 
{
#ifdef SUPPORT_JSAI
	if (mpJScriptNode)
		delete mpJScriptNode;
#endif
}

////////////////////////////////////////////////
// ScriptNode::initialize
////////////////////////////////////////////////

void ScriptNode::initialize() 
{
#ifdef SUPPORT_JSAI
	if (!isInitialized()) {

		if (mpJScriptNode) {
			delete mpJScriptNode;
			mpJScriptNode = NULL;
		}

		JScript *sjnode = new JScript(this);
	
		assert(sjnode);

		if (sjnode->isOK()) {
			mpJScriptNode = sjnode;
		}
		else
			delete sjnode;

		setInitialized(true);
	}

	if (mpJScriptNode) {
		mpJScriptNode->setValue(this);
		mpJScriptNode->initialize();
		mpJScriptNode->getValue(this);
	}

#endif
}

////////////////////////////////////////////////
// ScriptNode::initialize
////////////////////////////////////////////////

void ScriptNode::uninitialize() 
{
	setInitialized(false);

#ifdef SUPPORT_JSAI

	if (hasScript()) {
		JScript *jscript = getJavaNode();
		jscript->setValue(this);
		jscript->shutdown();
		jscript->getValue(this);
	}

#endif
}

////////////////////////////////////////////////
// ScriptNode::update
////////////////////////////////////////////////

void ScriptNode::update(Field *eventInField) {

#ifdef SUPPORT_JSAI

	if (hasScript()) {

		JScript *jscript = getJavaNode();

		jscript->setValue(this);

		Event event(eventInField);
		jscript->processEvent(&event);

		jscript->getValue(this);

		int nEventOut = getNEventOut();
		for (int n=0; n<nEventOut; n++) {
			Field *field = getEventOut(n);
			sendEvent(field);
		}
	}

#endif

}

////////////////////////////////////////////////
// ScriptNode::updateFields
////////////////////////////////////////////////

void ScriptNode::updateFields() {

#ifdef SUPPORT_JSAI
	if (hasScript()) {
		JScript *jscript = getJavaNode();
		jscript->setValue(this);
		jscript->processEvent(NULL);
		jscript->getValue(this);
	}
#endif

}









