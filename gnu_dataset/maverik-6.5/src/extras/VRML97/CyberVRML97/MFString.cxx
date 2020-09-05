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
*	File:	MFString.cpp
*
******************************************************************/

#include "MFString.h"

#ifdef SUPPORT_JSAI

int			MFString::mInit = 0;

jclass		MFString::mFieldClassID = 0;
jclass		MFString::mConstFieldClassID = 0;

jmethodID	MFString::mInitMethodID = 0;
jmethodID	MFString::mSetValueMethodID = 0;
jmethodID	MFString::mGetValueMethodID = 0;
jmethodID	MFString::mSetNameMethodID = 0;

jmethodID	MFString::mConstInitMethodID = 0;
jmethodID	MFString::mConstSetValueMethodID = 0;
jmethodID	MFString::mConstGetValueMethodID = 0;
jmethodID	MFString::mConstSetNameMethodID = 0;

////////////////////////////////////////////////
//	MFString::setJavaIDs
////////////////////////////////////////////////

void MFString::setJavaIDs() {

	if (!mInit) {
		JNIEnv *jniEnv = getJniEnv();

		if (jniEnv == NULL)
			return;

		// Class IDs
		mFieldClassID		= jniEnv->FindClass("vrml/field/MFString");
		mConstFieldClassID	= jniEnv->FindClass("vrml/field/ConstMFString");

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
//	MFString::toJavaObject
////////////////////////////////////////////////

jobject MFString::toJavaObject(int bConstField) {
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
//	MFString::setValue
////////////////////////////////////////////////

void MFString::setValue(jobject field, int bConstField) {
}

////////////////////////////////////////////////
//	MFString::getValue
////////////////////////////////////////////////

void MFString::getValue(jobject field, int bConstField) {
}

#endif 





////////////////////////////////////////////////
//	MFString::setValue
////////////////////////////////////////////////

void MFString::setValue(MFString *values)
{
	clear();

	int size = values->getSize();
	for (int n=0; n<size; n++) {
		addValue(values->get1Value(n));
	}
}

void MFString::setValue(MField *mfield)
{
	if (mfield->getType() == fieldTypeMFString)
		setValue((MFString *)mfield);
}

void MFString::setValue(int size, char *values[])
{
	clear();

	for (int n=0; n<size; n++)
		addValue(values[n]);
}


