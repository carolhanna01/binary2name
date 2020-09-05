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
*	File:	SFVec2f.h
*
******************************************************************/

#ifndef _SFVEC2F_H_
#define _SFVEC2F_H_

#include <math.h>
#include <stdio.h>
#include <iostream.h>
#include "Field.h"

class SFVec2f : public Field {

	static	int	mInit;

	float mValue[2]; 

public:

	SFVec2f() {
		setType(fieldTypeSFVec2f);
		setValue(0.0f, 0.0f);
		InitializeJavaIDs();
	}

	SFVec2f(float x, float y) {
		setType(fieldTypeSFVec2f);
		setValue(x, y);
		InitializeJavaIDs();
	}

	SFVec2f(float value[]) {
		setType(fieldTypeSFVec2f);
		setValue(value);
		InitializeJavaIDs();
	}

	SFVec2f(SFVec2f *value) {
		setType(fieldTypeSFVec2f);
		setValue(value);
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
	}

	float *getValue() {
		return mValue;
	}

	float getX() {
		return mValue[0];
	}

	float getY() {
		return mValue[1];
	}

	////////////////////////////////////////////////
	//	set value
	////////////////////////////////////////////////

	void setValue(float x, float y) {
		mValue[0] = x;
		mValue[1] = y;
	}

	void setValue(float value[]) {
		mValue[0] = value[0];
		mValue[1] = value[1];
	}

	void setValue(SFVec2f *vector) {
		setValue(vector->getX(), vector->getY());
	}

	void setX(float x) {
		setValue(x, getY());
	}

	void setY(float y) {
		setValue(getX(), y);
	}

	////////////////////////////////////////////////
	//	add value
	////////////////////////////////////////////////

	void add(float x, float y) {
		mValue[0] += x;
		mValue[1] += y;
	}

	void add(float value[]) {
		mValue[0] += value[0];
		mValue[1] += value[1];
	}

	void add(SFVec2f value) {
		add(value.getValue());
	}

	void translate(float x, float y) {
		add(x, y);
	}

	void translate(float value[]) {
		add(value);
	}

	void translate(SFVec2f value) {
		add(value);
	}

	////////////////////////////////////////////////
	//	sub value
	////////////////////////////////////////////////

	void sub(float x, float y) {
		mValue[0] -= x;
		mValue[1] -= y;
	}

	void sub(float value[]) {
		mValue[0] -= value[0];
		mValue[1] -= value[1];
	}

	void sub(SFVec2f value) {
		sub(value.getValue());
	}

	////////////////////////////////////////////////
	//	scale
	////////////////////////////////////////////////

	void scale(float value) {
		mValue[0] *= value;
		mValue[1] *= value;
		//JMC		mValue[2] *= value; 
	}	
	
	void scale(float xscale, float yscale) {
		mValue[0] *= xscale;
		mValue[1] *= yscale;
	}

	void scale(float value[2]) {
		scale(value[0], value[1]);
	}

	////////////////////////////////////////////////
	//	invert
	////////////////////////////////////////////////

	void invert() {
		mValue[0] = -mValue[0];
		mValue[1] = -mValue[1];
	}

	////////////////////////////////////////////////
	//	scalar
	////////////////////////////////////////////////

	float getScalar()
	{
		return (float)sqrt(mValue[0]*mValue[0]+mValue[1]*mValue[1]);
	}

	////////////////////////////////////////////////
	//	normalize
	////////////////////////////////////////////////

	void normalize()
	{
		float scale = getScalar();
		if (scale != 0.0f) {
			mValue[0] /= scale;
			mValue[1] /= scale;
		}
	}

	////////////////////////////////////////////////
	//	Output
	////////////////////////////////////////////////

	friend ostream& operator<<(ostream &s, SFVec2f &vector) {
		return s << vector.getX() << " " << vector.getY();
	}

	friend ostream& operator<<(ostream &s, SFVec2f *vector) {
		return s << vector->getX() << " " << vector->getY();
	}

	////////////////////////////////////////////////
	//	String
	////////////////////////////////////////////////

	void setValue(char *value) {
		if (!value)
			return;
		float	x, y;
		if (sscanf(value,"%f %f", &x, &y) == 2) 
			setValue(x, y);
	}

	char *getValue(char *buffer, int bufferLen) {
		sprintf(buffer, "%g %g", getX(), getY());
		return buffer;
	}

	////////////////////////////////////////////////
	//	Compare
	////////////////////////////////////////////////

	bool equals(Field *field) {
		SFVec2f *vector = (SFVec2f *)field;
		if (getX() == vector->getX() && getY() == vector->getY())
			return true;
		else
			return false;
	}

	bool equals(float value[2]) {
		SFVec2f vector(value);
		return equals(&vector);
	}

	bool equals(float x, float y) {
		SFVec2f vector(x, y);
		return equals(&vector);
	}

	////////////////////////////////////////////////
	//	Java
	////////////////////////////////////////////////

#ifdef SUPPORT_JSAI

private:

	static jclass		mFieldClassID;
	static jclass		mConstFieldClassID;

	static jmethodID	mInitMethodID;
	static jmethodID	mGetXMethodID;
	static jmethodID	mGetYMethodID;
	static jmethodID	mSetValueMethodID;
	static jmethodID	mSetNameMethodID;

	static jmethodID	mConstInitMethodID;
	static jmethodID	mConstGetXMethodID;
	static jmethodID	mConstGetYMethodID;
	static jmethodID	mConstSetValueMethodID;
	static jmethodID	mConstSetNameMethodID;

public:

	void		setJavaIDs();

	jclass		getFieldID()				{return mFieldClassID;}
	jclass		getConstFieldID()			{return mConstFieldClassID;}

	jmethodID	getInitMethodID()			{return mInitMethodID;}
	jmethodID	getGetXMethodID()			{return mGetXMethodID;}
	jmethodID	getGetYMethodID()			{return mGetYMethodID;}
	jmethodID	getSetValueMethodID()		{return mSetValueMethodID;}
	jmethodID	getSetNameMethodID()		{return mSetNameMethodID;}

	jmethodID	getConstInitMethodID()		{return mConstInitMethodID;}
	jmethodID	getConstGetXMethodID()		{return mConstGetXMethodID;}
	jmethodID	getConstGetYMethodID()		{return mConstGetYMethodID;}
	jmethodID	getConstSetValueMethodID()	{return mConstSetValueMethodID;}
	jmethodID	getConstSetNameMethodID()	{return mConstSetNameMethodID;}

	jobject toJavaObject(int bConstField = 0);
	void setValue(jobject field, int bConstField = 0);
	void getValue(jobject field, int bConstField = 0);

#endif

};

#endif //JMC
