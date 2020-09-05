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
*	File:	MFColor.cpp
*
******************************************************************/

#include "MFColor.h"

#ifdef SUPPORT_JSAI

int			MFColor::mInit = 0;

jclass		MFColor::mFieldClassID = 0;
jclass		MFColor::mConstFieldClassID = 0;

jmethodID	MFColor::mInitMethodID = 0;
jmethodID	MFColor::mSetValueMethodID = 0;
jmethodID	MFColor::mGetValueMethodID = 0;
jmethodID	MFColor::mSetNameMethodID = 0;

jmethodID	MFColor::mConstInitMethodID = 0;
jmethodID	MFColor::mConstSetValueMethodID = 0;
jmethodID	MFColor::mConstGetValueMethodID = 0;
jmethodID	MFColor::mConstSetNameMethodID = 0;

////////////////////////////////////////////////
//	MFColor::setJavaIDs
////////////////////////////////////////////////

void MFColor::setJavaIDs() {

	if (!mInit) {
		JNIEnv *jniEnv = getJniEnv();

		if (jniEnv == NULL)
			return;

		// Class IDs
		mFieldClassID		= jniEnv->FindClass("vrml/field/MFColor");
		mConstFieldClassID	= jniEnv->FindClass("vrml/field/ConstMFColor");

		assert(mFieldClassID && mConstFieldClassID);

		// MethodIDs
		jclass classid		= getFieldID();
		mInitMethodID		= jniEnv->GetMethodID(classid, "<init>", "()V");
		mSetNameMethodID	= jniEnv->GetMethodID(classid, "setName", "(Ljava/lang/String;)V");

		assert(mInitMethodID && mSetNameMethodID);

		// Const MethodIDs
		classid = getConstFieldID();
		mConstInitMethodID		= jniEnv->GetMethodID(classid, "<init>", "()V");
		mConstSetNameMethodID	= jniEnv->GetMethodID(classid, "setName", "(Ljava/lang/String;)V");

		assert(mConstInitMethodID && mConstSetNameMethodID);

		mInit = 1;
	}
}

////////////////////////////////////////////////
//	MFColor.cpp::toJavaObject
////////////////////////////////////////////////

jobject MFColor::toJavaObject(int bConstField) {
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	initMethod		= bConstField ? getConstInitMethodID() : getInitMethodID();
	jobject		eventField		= jniEnv->NewObject(classid, initMethod);
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
//	MFColor::setValue
////////////////////////////////////////////////

void MFColor::setValue(jobject field, int bConstField) {
}

////////////////////////////////////////////////
//	MFColor::getValue
////////////////////////////////////////////////

void MFColor::getValue(jobject field, int bConstField) {
}

#endif




////////////////////////////////////////////////
//	MFColor::setValue
////////////////////////////////////////////////

void MFColor::setValue(MFColor *colors)
{
	clear();

	float value[3];
	int size = colors->getSize();
	for (int n=0; n<size; n++) {
		colors->get1Value(n, value);
		addValue(value);
	}
}

void MFColor::setValue(MField *mfield)
{
	if (mfield->getType() == fieldTypeMFColor)
		setValue((MFColor *)mfield);
}

void MFColor::setValue(int size, float colors[][3])
{
	clear();

	for (int n=0; n<size; n++)
		addValue(colors[n]);
}

