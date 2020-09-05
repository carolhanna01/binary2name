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
*	File:	SFNode.cpp
*
******************************************************************/

#include "SFNode.h"
#include "JNode.h"
#include "Node.h"

#ifdef SUPPORT_JSAI

int			SFNode::mInit = 0;

jclass		SFNode::mFieldClassID = 0;
jclass		SFNode::mConstFieldClassID = 0;

jmethodID	SFNode::mInitMethodID = 0;
jmethodID	SFNode::mSetValueMethodID = 0;
jmethodID	SFNode::mGetValueMethodID = 0;
jmethodID	SFNode::mSetNameMethodID = 0;

jmethodID	SFNode::mConstInitMethodID = 0;
jmethodID	SFNode::mConstSetValueMethodID = 0;
jmethodID	SFNode::mConstGetValueMethodID = 0;
jmethodID	SFNode::mConstSetNameMethodID = 0;

////////////////////////////////////////////////
//	SFNode::toJavaObject
////////////////////////////////////////////////

void SFNode::setJavaIDs() {

	if (!mInit) {
		JNIEnv *jniEnv = getJniEnv();

		if (jniEnv == NULL)
			return;

		// Class IDs
		mFieldClassID		= jniEnv->FindClass("vrml/field/SFNode");
		mConstFieldClassID	= jniEnv->FindClass("vrml/field/ConstSFNode");

		assert(mFieldClassID && mConstFieldClassID);

		// MethodIDs
		jclass classid = getFieldID();
		mInitMethodID		= jniEnv->GetMethodID(classid, "<init>", "(Lvrml/BaseNode;)V");
		mGetValueMethodID	= jniEnv->GetMethodID(classid, "getValue", "()Lvrml/BaseNode;");
		mSetValueMethodID	= jniEnv->GetMethodID(classid, "setValue", "(Lvrml/BaseNode;)V");
		mSetNameMethodID	= jniEnv->GetMethodID(classid, "setName", "(Ljava/lang/String;)V");

		assert(mInitMethodID && mGetValueMethodID && mSetValueMethodID && mSetNameMethodID);

		// MethodIDs
		classid	 = getConstFieldID();
		mConstInitMethodID		= jniEnv->GetMethodID(classid, "<init>", "(Lvrml/BaseNode;)V");
		mConstGetValueMethodID	= jniEnv->GetMethodID(classid, "getValue", "()Lvrml/BaseNode;");
		mConstSetValueMethodID	= jniEnv->GetMethodID(classid, "setValue", "(Lvrml/BaseNode;)V");
		mConstSetNameMethodID	= jniEnv->GetMethodID(classid, "setName", "(Ljava/lang/String;)V");

		assert(mConstInitMethodID && mConstGetValueMethodID && mConstSetValueMethodID && mConstSetNameMethodID);

		mInit = 1;
	}
}

////////////////////////////////////////////////
//	SFNode::toJavaObject
////////////////////////////////////////////////

jobject SFNode::toJavaObject(int bConstField) {
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	initMethod		= bConstField ? getConstInitMethodID() : getInitMethodID();
	JNode		*node			= new JNode(getValue());
	jobject		jnode			= node->getNodeObject();
	jobject		eventField		= jniEnv->NewObject(classid, initMethod, jnode);
	jmethodID	setNameMethod	= bConstField ? getConstSetNameMethodID() : getSetNameMethodID();

	char		*fieldName		= getName();
	jstring		jfieldName		= NULL;
	if (fieldName && strlen(fieldName))
		jfieldName = jniEnv->NewStringUTF(getName());
	jniEnv->CallVoidMethod(eventField, setNameMethod, jfieldName);
	if (jfieldName)
		jniEnv->DeleteLocalRef(jfieldName);
	
	return eventField;
}

////////////////////////////////////////////////
//	SFNode::setValue
////////////////////////////////////////////////

void SFNode::setValue(jobject field, int bConstField) {
	Node	*node = getValue();

	JNIEnv		*jniEnv			= getJniEnv();
	jmethodID	getValueMethod	= bConstField ? getConstGetValueMethodID() : getGetValueMethodID();
	jobject value = jniEnv->CallObjectMethod(field, getValueMethod);

	JNode	jnode(value);
	jnode.getValue(node);
}

////////////////////////////////////////////////
//	SFNode::getValue
////////////////////////////////////////////////

void SFNode::getValue(jobject field, int bConstField) {
	Node	*node = getValue();

	JNIEnv		*jniEnv			= getJniEnv();
	jmethodID	getValueMethod	= bConstField ? getConstGetValueMethodID() : getGetValueMethodID();
	jobject value = jniEnv->CallObjectMethod(field, getValueMethod);

	JNode	jnode(value);
	jnode.setValue(node);
}

#endif




////////////////////////////////////////////////
//	SFNode::setValue
////////////////////////////////////////////////

void SFNode::setValue(char *buffer) {
}

////////////////////////////////////////////////
//	SFNode::getValue
////////////////////////////////////////////////

char *SFNode::getValue(char *buffer, int bufferLen) {
	sprintf(buffer, "%s", getValue()->getName());
	return buffer;
}
