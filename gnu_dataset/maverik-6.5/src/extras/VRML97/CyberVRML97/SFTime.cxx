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
*	File:	SFTime.cpp
*
******************************************************************/

#include <time.h>
#ifdef WIN32
#include <sys/timeb.h>
#else
#include <sys/time.h> //JMC
#if MAV_LINUX
struct timezone { // JMC
  int  tz_minuteswest; /* minutes W of Greenwich */ //JMC
  int  tz_dsttime;     /* type of dst correction */ //JMC
       };
#endif
#endif
#include "SFTime.h"

#ifdef SUPPORT_JSAI

int			SFTime::mInit = 0;

jclass		SFTime::mFieldClassID = 0;
jclass		SFTime::mConstFieldClassID = 0;

jmethodID	SFTime::mInitMethodID = 0;
jmethodID	SFTime::mSetValueMethodID = 0;
jmethodID	SFTime::mGetValueMethodID = 0;
jmethodID	SFTime::mSetNameMethodID = 0;

jmethodID	SFTime::mConstInitMethodID = 0;
jmethodID	SFTime::mConstSetValueMethodID = 0;
jmethodID	SFTime::mConstGetValueMethodID = 0;
jmethodID	SFTime::mConstSetNameMethodID = 0;

////////////////////////////////////////////////
//	SFTime::toJavaObject
////////////////////////////////////////////////

void SFTime::setJavaIDs() {

	if (!mInit) {
		JNIEnv *jniEnv = getJniEnv();

		if (jniEnv == NULL)
			return;

		// Class IDs
		mFieldClassID		= jniEnv->FindClass("vrml/field/SFTime");
		mConstFieldClassID	= jniEnv->FindClass("vrml/field/ConstSFTime");

		assert(mFieldClassID && mConstFieldClassID);

		// MethodIDs
		jclass classid = getFieldID();
		mInitMethodID		= jniEnv->GetMethodID(classid, "<init>", "(D)V");
		mGetValueMethodID	= jniEnv->GetMethodID(classid, "getValue", "()D");
		mSetValueMethodID	= jniEnv->GetMethodID(classid, "setValue", "(D)V");
		mSetNameMethodID	= jniEnv->GetMethodID(classid, "setName", "(Ljava/lang/String;)V");

		assert(mInitMethodID && mGetValueMethodID && mSetValueMethodID && mSetNameMethodID);

		// MethodIDs
		classid	 = getConstFieldID();
		mConstInitMethodID		= jniEnv->GetMethodID(classid, "<init>", "(D)V");
		mConstGetValueMethodID	= jniEnv->GetMethodID(classid, "getValue", "()D");
		mConstSetValueMethodID	= jniEnv->GetMethodID(classid, "setValue", "(D)V");
		mConstSetNameMethodID	= jniEnv->GetMethodID(classid, "setName", "(Ljava/lang/String;)V");

		assert(mConstInitMethodID && mConstGetValueMethodID && mConstSetValueMethodID && mConstSetNameMethodID);

		mInit = 1;
	}
}

////////////////////////////////////////////////
//	SFTime::toJavaObject
////////////////////////////////////////////////

jobject SFTime::toJavaObject(int bConstField) {
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	initMethod		= bConstField ? getConstInitMethodID() : getInitMethodID();
	jdouble		value			= getValue();
	jobject		eventField		= jniEnv->NewObject(classid, initMethod, value);
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
//	SFTime::setValue
////////////////////////////////////////////////

void SFTime::setValue(jobject field, int bConstField) {
	assert(field);
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	getValueMethod	= bConstField ? getConstGetValueMethodID() : getGetValueMethodID();
	assert(classid && getValueMethod);
	jdouble		value			= jniEnv->CallDoubleMethod(field, getValueMethod);
	setValue(value);
}

////////////////////////////////////////////////
//	SFTime::getValue
////////////////////////////////////////////////

void SFTime::getValue(jobject field, int bConstField) {
	assert(field);
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	setValueMethod	= bConstField ? getConstSetValueMethodID() : getSetValueMethodID();
	assert(classid && setValueMethod);
	jdouble		value			= getValue();
	jniEnv->CallVoidMethod(field, setValueMethod, value);
}

#endif




////////////////////////////////////////////////
//	GetCurrentSystemTime
////////////////////////////////////////////////

double GetCurrentSystemTime()
{
	long	second;
	int		milisecond;

#ifdef WIN32
	struct	timeb timebuffer; //JMC

	ftime( &timebuffer ); //JMC
	second = timebuffer.time;// + timebuffer.timezone*60;
	milisecond = timebuffer.millitm ;
#else
	struct timeval	time;
	struct timezone	timeZone;

	gettimeofday(&time, &timeZone); //JMC
	second = time.tv_sec + time.tv_usec;
	milisecond = time.tv_usec / 1000;
#endif

	return ((double)second + ((double)milisecond / 1000.0));
}
