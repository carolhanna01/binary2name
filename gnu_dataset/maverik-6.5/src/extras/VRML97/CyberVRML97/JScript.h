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
*	File:	JScript.h
*
******************************************************************/

#ifndef _JSCRIPT_H_
#define _JSCRIPT_H_

#ifdef SUPPORT_JSAI

#include "JNode.h"

class ScriptNode;
class Event;

class JScript : public JNode  {

	jclass		mNodeObjectClassID;
	jmethodID	mInitMethodID;
	jmethodID	mSetNameMethodID;

	jmethodID	mAddEventInMethodID;
	jmethodID	mAddEventOutMethodID;
	jmethodID	mAddFieldMethodID;
	jmethodID	mAddExposedFieldMethodID;

	jmethodID	mGetEventInMethodID;
	jmethodID	mGetEventOutMethodID;
	jmethodID	mGetFieldMethodID;
	jmethodID	mGetExposedFieldMethodID;

	jmethodID	mGetNEventInMethodID;
	jmethodID	mGetNEventOutMethodID;
	jmethodID	mGetNFieldsMethodID;
	jmethodID	mGetNExposedFieldsMethodID;

	jmethodID	mInitializeMethodID;
	jmethodID	mShutdownMethodID;
	jmethodID	mProcessEventMethodID;

public:

	JScript(ScriptNode *node);

	~JScript();

	int			isOK()							{return getNodeObject() ? 1 : 0;}

	////////////////////////////////////////////////
	//	class ID	
	////////////////////////////////////////////////

	jclass		getNodeObjectClassID()			{return mNodeObjectClassID;}
	jmethodID	getInitMethodID()				{return mInitMethodID;}
	jmethodID	getSetNameMethodID()			{return mSetNameMethodID;}

	////////////////////////////////////////////////
	//	method ID
	////////////////////////////////////////////////

	jmethodID	getAddEventInMethodID()			{return mAddEventInMethodID;}
	jmethodID	getAddEventOutMethodID()		{return mAddEventOutMethodID;}
	jmethodID	getAddFieldMethodID()			{return mAddFieldMethodID;}
	jmethodID	getAddExposedFieldMethodID()	{return mAddExposedFieldMethodID;}

	jmethodID	getGetEventInMethodID()			{return mGetEventInMethodID;}
	jmethodID	getGetEventOutMethodID()		{return mGetEventOutMethodID;}
	jmethodID	getGetFieldMethodID()			{return mGetFieldMethodID;}
	jmethodID	getGetExposedFieldMethodID()	{return mGetExposedFieldMethodID;}

	jmethodID	getGetNEventInMethodID()		{return mGetNEventInMethodID;}
	jmethodID	getGetNEventOutMethodID()		{return mGetNEventOutMethodID;}
	jmethodID	getGetNFieldsMethodID()			{return mGetNFieldsMethodID;}
	jmethodID	getGetNExposedFieldsMethodID()	{return mGetNExposedFieldsMethodID;}

	jmethodID	getInitializeMethodID()			{return mInitializeMethodID;}
	jmethodID	getShutdownMethodID()			{return mShutdownMethodID;}
	jmethodID	getProcessEventMethodID()		{return mProcessEventMethodID;}

	////////////////////////////////////////////////
	//	initialize
	////////////////////////////////////////////////

	void		initialize();

	////////////////////////////////////////////////
	//	processEvent
	////////////////////////////////////////////////

	void		processEvent(Event *event);

	////////////////////////////////////////////////
	//	shutdown
	////////////////////////////////////////////////

	void		shutdown();

	////////////////////////////////////////////////
	//	add*	
	////////////////////////////////////////////////

	void addEventOut(Field *field) {
		addFieldObject(getAddEventOutMethodID(), field);
	}

	////////////////////////////////////////////////
	//	set/getValue	
	////////////////////////////////////////////////

	void setValue(Node *node);
	void getValue(Node *node);
};

#endif

#endif

