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
*	File:	Field.h
*
******************************************************************/

#ifndef _FIELD_H_
#define _FIELD_H_

#include <stdlib.h>
#include <string.h>
#include <iostream.h>
#include <assert.h>
#include "JString.h"
#include "CJavaVM.h"

enum {
fieldTypeNone,
fieldTypeSFBool,
fieldTypeSFFloat,
fieldTypeSFInt32,
fieldTypeSFVec2f,
fieldTypeSFVec3f,
fieldTypeSFString,
fieldTypeSFColor,
fieldTypeSFTime,
fieldTypeSFRotation,
fieldTypeSFImage,
fieldTypeSFNode,
fieldTypeMFFloat,
fieldTypeMFInt32,
fieldTypeMFVec2f,
fieldTypeMFVec3f,
fieldTypeMFString,
fieldTypeMFColor,
fieldTypeMFTime,
fieldTypeMFRotation,
fieldTypeMFNode,
fieldTypeMaxNum,
};

class	SFBool;
class	SFFloat;
class	SFInt32;
class	SFVec2f;
class	SFVec3f;
class	SFString;
class	SFColor;
class	SFTime;
class	SFRotation;
//class	SFNode;
class	MFFloat;
class	MFInt32;
class	MFVec2f;
class	MFVec3f;
class	MFString;
class	MFColor;
class	MFTime;
class	MFRotation;
//class	MFNode;

#define	eventInStripString		"set_"
#define eventOutStripString		"_changed"

#define JAVAOBJECT_FIELD		0
#define JAVAOBJECT_CONSTFIELD	1

#define FIELD_BUFFERSIZE		1024

#ifdef SUPPORT_JSAI
class Field : public CJavaVM {
#else
class Field {
#endif

	JString	mName;
	int		mType;

public:

	Field() {
		mType = fieldTypeNone;
	}	

	virtual ~Field() {
	}	

	char *getTypeName();

	void setType(int type) {
		mType = type;
	}

	void setType(char *type);

	int getType() {
		return mType;
	}

	void setName(char *name) {
		mName.setValue(name);
	}

	char *getName() {
		return mName.getValue();
	}

	friend ostream& operator<<(ostream &s, Field &value);
	friend ostream& operator<<(ostream &s, Field *value);

	////////////////////////////////////////////////
	//	String
	////////////////////////////////////////////////

	virtual void setValue(char *value){
	}

	virtual char *getValue(char *buffer, int bufferLen = -1){
		buffer[0] = '\0';
		return buffer;
	}

	////////////////////////////////////////////////
	//	Compare
	////////////////////////////////////////////////

	virtual bool equals(Field *field) {
		return false;
	}

	////////////////////////////////////////////////
	//	Java
	////////////////////////////////////////////////

#ifdef SUPPORT_JSAI
	virtual jobject toJavaObject(int bConstField = 0) {
		assert(0);
		return NULL;
	};
	virtual void setValue(jobject field, int bConstField = 0) {
		assert(0);
	};
	virtual void getValue(jobject field, int bConstField = 0) {
		assert(0);
	};
#endif
};

#endif
