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
*	File:	SFColor.h
*
******************************************************************/

#ifndef _SFCOLOR_H_
#define _SFCOLOR_H_

#include <stdio.h>
#include <iostream.h>
#include "Field.h"

class SFColor : public Field {

	static	int	mInit;

	float mValue[3]; 

public:

	SFColor() {
		setType(fieldTypeSFColor);
		setValue(1.0f, 1.0f, 1.0f);
		InitializeJavaIDs();
	}

	SFColor(float r, float g, float b) {
		setType(fieldTypeSFColor);
		setValue(r, g, b);
		InitializeJavaIDs();
	}

	SFColor(float value[]) {
		setType(fieldTypeSFColor);
		setValue(value);
		InitializeJavaIDs();
	}

	SFColor(SFColor *color) {
		setType(fieldTypeSFColor);
		setValue(color);
		InitializeJavaIDs();
	}

	void InitializeJavaIDs() {
#ifdef SUPPORT_JSAI
		setJavaIDs();
#endif
	}

	////////////////////////////////////////////////
	//	get value
	////////////////////////////////////////////////

	void getValue(float value[]) {
		value[0] = mValue[0];
		value[1] = mValue[1];
		value[2] = mValue[2];
	}

	float *getValue() {
		return mValue;
	}

	float getRed() {
		return mValue[0];
	}

	float getGreen() {
		return mValue[1];
	}

	float getBlue() {
		return mValue[2];
	}

	////////////////////////////////////////////////
	//	set value
	////////////////////////////////////////////////

	void setValue(float r, float g, float b) {
		mValue[0] = r;
		mValue[1] = g;
		mValue[2] = b;
	}

	void setValue(float value[]) {
		mValue[0] = value[0];
		mValue[1] = value[1];
		mValue[2] = value[2];
	}

	void setValue(SFColor *color) {
		setValue(color->getRed(), color->getGreen(), color->getBlue());
	}

	////////////////////////////////////////////////
	//	add value
	////////////////////////////////////////////////

	void add(float x, float y, float z) {
		mValue[0] += x;
		mValue[1] += y;
		mValue[2] += z;
		mValue[0] /= 2.0f;
		mValue[1] /= 2.0f;
		mValue[2] /= 2.0f;
	}

	void add(float value[]) {
		add(value[0], value[1], value[2]);
	}

	void add(SFColor value) {
		add(value.getValue());
	}

	////////////////////////////////////////////////
	//	sub value
	////////////////////////////////////////////////

	void sub(float x, float y, float z) {
		mValue[0] -= x;
		mValue[1] -= y;
		mValue[2] -= z;
		mValue[0] /= 2.0f;
		mValue[1] /= 2.0f;
		mValue[2] /= 2.0f;
	}

	void sub(float value[]) {
		sub(value[0], value[1], value[2]);
	}

	void sub(SFColor value) {
		sub(value.getValue());
	}

	////////////////////////////////////////////////
	//	Output
	////////////////////////////////////////////////

	friend ostream& operator<<(ostream &s, SFColor &vector) {
		return s << vector.getRed() << " " << vector.getGreen() << " " << vector.getBlue();
	}

	friend ostream& operator<<(ostream &s, SFColor *vector) {
		return s << vector->getRed() << " " << vector->getGreen() << " " << vector->getBlue();
	}

	////////////////////////////////////////////////
	//	String
	////////////////////////////////////////////////

	void setValue(char *value) {
		if (!value)
			return;
		float	r, g, b;
		if (sscanf(value,"%f %f %f", &r, &g, &b) == 3) 
			setValue(r, g, b);
	}

	char *getValue(char *buffer, int bufferLen) {
		sprintf(buffer, "%g %g %g", getRed(), getGreen(), getBlue());
		return buffer;
	}

	////////////////////////////////////////////////
	//	scale
	////////////////////////////////////////////////

	void scale(float scale) {
		mValue[0] *= scale;
		mValue[1] *= scale;
		mValue[2] *= scale;
	}

	////////////////////////////////////////////////
	//	Compare
	////////////////////////////////////////////////

	bool equals(Field *field) {
		SFColor *color = (SFColor *)field;
		if (getRed() == color->getRed() && getGreen() == color->getGreen() && getBlue() == color->getBlue())
			return true;
		else
			return false;
	}

	////////////////////////////////////////////////
	//	Java
	////////////////////////////////////////////////

#ifdef SUPPORT_JSAI

private:

	static jclass		mFieldClassID;
	static jclass		mConstFieldClassID;

	static jmethodID	mInitMethodID;
	static jmethodID	mGetRedMethodID;
	static jmethodID	mGetGreenMethodID;
	static jmethodID	mGetBlueMethodID;
	static jmethodID	mSetValueMethodID;
	static jmethodID	mSetNameMethodID;

	static jmethodID	mConstInitMethodID;
	static jmethodID	mConstGetRedMethodID;
	static jmethodID	mConstGetGreenMethodID;
	static jmethodID	mConstGetBlueMethodID;
	static jmethodID	mConstSetValueMethodID;
	static jmethodID	mConstSetNameMethodID;

public:

	void		setJavaIDs();

	jclass		getFieldID()				{return mFieldClassID;}
	jclass		getConstFieldID()			{return mConstFieldClassID;}

	jmethodID	getInitMethodID()			{return mInitMethodID;}
	jmethodID	getGetRedMethodID()			{return mGetRedMethodID;}
	jmethodID	getGetGreenMethodID()			{return mGetGreenMethodID;}
	jmethodID	getGetBlueMethodID()			{return mGetBlueMethodID;}
	jmethodID	getSetValueMethodID()		{return mSetValueMethodID;}
	jmethodID	getSetNameMethodID()		{return mSetNameMethodID;}

	jmethodID	getConstInitMethodID()		{return mConstInitMethodID;}
	jmethodID	getConstGetRedMethodID()		{return mConstGetRedMethodID;}
	jmethodID	getConstGetGreenMethodID()		{return mConstGetGreenMethodID;}
	jmethodID	getConstGetBlueMethodID()		{return mConstGetBlueMethodID;}
	jmethodID	getConstSetValueMethodID()	{return mConstSetValueMethodID;}
	jmethodID	getConstSetNameMethodID()	{return mConstSetNameMethodID;}

	jobject toJavaObject(int bConstField = 0);
	void setValue(jobject field, int bConstField = 0);
	void getValue(jobject field, int bConstField = 0);

#endif
};

#endif //JMC
