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
*	File:	JScript.cpp
*
******************************************************************/

#ifdef SUPPORT_JSAI

#include "SceneGraph.h"
#include "JScript.h"
#include "ScriptNode.h"
#include "Event.h"

////////////////////////////////////////////////
//	JScript::JScript
////////////////////////////////////////////////

static char *getClassName(char *classFileName, char *buffer)
{
	strcpy(buffer, classFileName);
	int classNameLen = strlen(buffer);
	for (int n=0; n<classNameLen; n++) {
		if (buffer[(classNameLen-1)-n] == '.')
			break;
	}
	buffer[(classNameLen-1)-n] = '\0';

	return buffer;
}

JScript::JScript(ScriptNode *node)
{
	if (0 < node->getNUrls()) {

		JNIEnv		*jniEnv	= getJniEnv();
		SceneGraph	*sg		= node->getSceneGraph();

		// get script class
		char	buffer[1024];

		// Class IDs
		mNodeObjectClassID = jniEnv->FindClass(getClassName(node->getUrl(0), buffer));
#ifdef SUPPORT_URL
		if (mNodeObjectClassID == NULL) {
			if (sg->getUrlStream(node->getUrl(0))) {
				mNodeObjectClassID = jniEnv->FindClass(getClassName(sg->getUrlOutputFilename(), buffer));
				sg->deleteUrlOutputFilename();
			}
		}
#endif

		if (!mNodeObjectClassID)
			return;

		// MethodIDs
		jclass classid				= getNodeObjectClassID();
		mInitMethodID				= jniEnv->GetMethodID(classid, "<init>",	"()V");

		mSetNameMethodID			= jniEnv->GetMethodID(classid, "setName",	"(Ljava/lang/String;)V");
		mAddEventInMethodID			= jniEnv->GetMethodID(classid, "addEventIn",		"(Ljava/lang/String;Lvrml/Field;)V");
		mAddEventOutMethodID		= jniEnv->GetMethodID(classid, "addEventOut",		"(Ljava/lang/String;Lvrml/Field;)V");
		mAddFieldMethodID			= jniEnv->GetMethodID(classid, "addField",			"(Ljava/lang/String;Lvrml/Field;)V");
		mAddExposedFieldMethodID	= jniEnv->GetMethodID(classid, "addExposedField",	"(Ljava/lang/String;Lvrml/Field;)V");

		mGetEventInMethodID			= jniEnv->GetMethodID(classid, "getEventIn",		"(Ljava/lang/String;)Lvrml/Field;");
		mGetEventOutMethodID		= jniEnv->GetMethodID(classid, "getEventOut",		"(Ljava/lang/String;)Lvrml/Field;");
		mGetFieldMethodID			= jniEnv->GetMethodID(classid, "getField",			"(Ljava/lang/String;)Lvrml/Field;");
		mGetExposedFieldMethodID	= jniEnv->GetMethodID(classid, "getExposedField",	"(Ljava/lang/String;)Lvrml/Field;");

		mGetNEventInMethodID		= jniEnv->GetMethodID(classid, "getNEventIn",		"()I");
		mGetNEventOutMethodID		= jniEnv->GetMethodID(classid, "getNEventOut",		"()I");
		mGetNFieldsMethodID			= jniEnv->GetMethodID(classid, "getNFields",		"()I");
		mGetNExposedFieldsMethodID	= jniEnv->GetMethodID(classid, "getNExposedFields",	"()I");

		// MethodIDs
		mInitializeMethodID			= jniEnv->GetMethodID(classid, "initialize",	"()V");
		mShutdownMethodID			= jniEnv->GetMethodID(classid, "shutdown",		"()V");
		mProcessEventMethodID		= jniEnv->GetMethodID(classid, "processEvent",	"(Lvrml/Event;)V");

		assert(mInitMethodID);
		assert(mGetEventInMethodID && mGetEventOutMethodID && mGetFieldMethodID);
		assert(mInitializeMethodID && mShutdownMethodID && mProcessEventMethodID);
/*
		assert(mSetNameMethodID);
		assert(mAddEventInMethodID && mAddEventOutMethodID && mAddFieldMethodID && mAddExposedFieldMethodID);
		assert(mGetExposedFieldMethodID);
		assert(mGetNEventInMethodID && mGetNEventOutMethodID && mGetNFieldsMethodID && mGetNExposedFieldsMethodID);
*/

		// Create object
		jobject object = jniEnv->NewObject(classid, getInitMethodID());
		setNodeObject(object);
		addFields(node);

		assert(object);

		initialize();
	}
	else {
		mNodeObjectClassID			= NULL;
		mInitMethodID				= NULL;
		mSetNameMethodID			= NULL;
		mAddEventInMethodID			= NULL;
		mAddEventOutMethodID		= NULL;
		mAddFieldMethodID			= NULL;
		mAddExposedFieldMethodID	= NULL;
		mGetEventInMethodID			= NULL;
		mGetEventOutMethodID		= NULL;
		mGetFieldMethodID			= NULL;
		mGetExposedFieldMethodID	= NULL;

		setNodeObject(NULL);
	}
}

////////////////////////////////////////////////
//	JScript::~JScript
////////////////////////////////////////////////

JScript::~JScript()
{
	shutdown();
}

////////////////////////////////////////////////
//	JScript::initialize
////////////////////////////////////////////////

void JScript::initialize()
{
	jobject object = getNodeObject();
	if (object && getInitializeMethodID())
		getJniEnv()->CallVoidMethod(object, getInitializeMethodID());
}

////////////////////////////////////////////////
// JScript::update
////////////////////////////////////////////////

void JScript::processEvent(Event *event) {
	if (isOK()) {
		jobject eventObject = NULL;
		
		if (event)
			eventObject = event->toJavaObject();
		
		getJniEnv()->CallVoidMethod(getNodeObject(), getProcessEventMethodID(), eventObject);
		
		if (eventObject)
			getJniEnv()->DeleteLocalRef(eventObject);
	}
}

////////////////////////////////////////////////
//	JScript::initialize
////////////////////////////////////////////////

void JScript::shutdown()
{
	if (getNodeObject() && getShutdownMethodID())
		getJniEnv()->CallVoidMethod(getNodeObject(), getShutdownMethodID());
}

////////////////////////////////////////////////
//	JScript::setValue
////////////////////////////////////////////////

void JScript::setValue(Node *node)
{
	if (!node)
		return;

	Field	*field;
	jobject	jfield;
	int		n;

	int nEventIn = node->getNEventIn();
	for (n=0; n<nEventIn; n++) {
		field	= node->getEventIn(n);
		jfield	= getEventIn(field);
		if (jfield != NULL)
			field->getValue(jfield);
	}

	int nEventOut = node->getNEventOut();
	for (n=0; n<nEventOut; n++) {
		field	= node->getEventOut(n);
		jfield = getEventOut(field);
		if (jfield != NULL)
			field->getValue(jfield);
	}

	int nField = node->getNFields();
	for (n=0; n<nField; n++) {
		field	= node->getField(n);
		jfield = getField(field);
		if (jfield != NULL)
			field->getValue(jfield);
	}

	int nExposedField = node->getNExposedFields();
	for (n=0; n<nExposedField; n++) {
		field	= node->getExposedField(n);
		jfield	= getExposedField(field);
		if (jfield != NULL)
			field->getValue(jfield);
	}
}


////////////////////////////////////////////////
//	JScript::getValue
////////////////////////////////////////////////

void JScript::getValue(Node *node)
{
	if (!node)
		return;

	Field	*field;
	jobject	jfield;
	int		n;

	int nEventIn = node->getNEventIn();
	for (n=0; n<nEventIn; n++) {
		field	= node->getEventIn(n);
		jfield	= getEventIn(field);
		if (jfield != NULL)
			field->setValue(jfield);
	}

	int nEventOut = node->getNEventOut();
	for (n=0; n<nEventOut; n++) {
		field	= node->getEventOut(n);
		jfield = getEventOut(field);
		if (jfield != NULL)
			field->setValue(jfield);
	}

	int nField = node->getNFields();
	for (n=0; n<nField; n++) {
		field	= node->getField(n);
		jfield = getField(field);
		if (jfield != NULL)
			field->setValue(jfield);
	}

	int nExposedField = node->getNExposedFields();
	for (n=0; n<nExposedField; n++) {
		field	= node->getExposedField(n);
		jfield	= getExposedField(field);
		if (jfield != NULL)
			field->setValue(jfield);
	}
}

#endif //JMC
