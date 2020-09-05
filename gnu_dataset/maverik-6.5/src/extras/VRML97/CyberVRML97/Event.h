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
*	File:	Event.h
*
******************************************************************/

#ifndef _EVENT_H_
#define _EVENT_H_

#include <time.h>
#include "JString.h"
#include "Field.h"
#include "SFTime.h"
#include "CJavaVM.h"

#ifdef SUPPORT_JSAI
class Event : public CJavaVM {
#else
class Event {
#endif

#ifdef SUPPORT_JSAI
	jclass		mEventClass;
#endif

	JString		mName;
	double		mTime;
	Field		*mField;

public:

	Event(Field *field) {
		setName(field->getName());
		setTimeStamp(GetCurrentSystemTime());
		setField(field);
		InitializeJavaIDs();
	}

	Event(char *name, double time, Field *field) {
		setName(name);
		setTimeStamp(time);
		setField(field);
		InitializeJavaIDs();
	}

	void InitializeJavaIDs() {
#ifdef SUPPORT_JSAI
		JNIEnv *jniEnv = getJniEnv();
		setEventClass(jniEnv->FindClass("vrml/Event"));
#endif
	}


	////////////////////////////////////////////////
	//	Name
	////////////////////////////////////////////////

	void setName(char *name) {
		mName.setValue(name);
	}

	char *getName() {
		return mName.getValue();
	}

	////////////////////////////////////////////////
	//	Time
	////////////////////////////////////////////////
	
	void setTimeStamp(double time) {
		mTime = time;
	}

	double getTimeStamp() {
		return mTime;
	}

	////////////////////////////////////////////////
	//	ConstField
	////////////////////////////////////////////////

	void setField(Field *field) {
		mField = field;
	}

	Field *getField() {
		return mField;
	}

	////////////////////////////////////////////////
	//	for Java
	////////////////////////////////////////////////

#ifdef SUPPORT_JSAI

	void setEventClass(jclass eventClass) {
		mEventClass = eventClass;
	}

	jclass getEventClass() {
		return mEventClass;
	}

	jobject toJavaObject();

#endif

};

#endif
