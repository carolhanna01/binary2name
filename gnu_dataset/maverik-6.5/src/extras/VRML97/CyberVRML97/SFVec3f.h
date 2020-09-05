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
*	File:	SFVec3f.h
*
******************************************************************/

#ifndef _SFVEC3F_H_
#define _SFVEC3F_H_

#include <math.h>
#include <stdio.h>
#include <iostream.h>
#include "Field.h"

class SFRotation;

class SFVec3f : public Field {

	static	int	mInit;

	float mValue[3]; 

public:

	SFVec3f() {
		setType(fieldTypeSFVec3f);
		setValue(0.0f, 0.0f, 0.0f);
		InitializeJavaIDs();
	}

	SFVec3f(float x, float y, float z) {
		setType(fieldTypeSFVec3f);
		setValue(x, y, z);
		InitializeJavaIDs();
	}

	SFVec3f(float value[]) {
		setType(fieldTypeSFVec3f);
		setValue(value);
		InitializeJavaIDs();
	}

	SFVec3f(SFVec3f *value) {
		setType(fieldTypeSFVec3f);
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
		value[2] = mValue[2];
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

	float getZ() {
		return mValue[2];
	}

	////////////////////////////////////////////////
	//	set value
	////////////////////////////////////////////////

	void setValue(float x, float y, float z) {
		mValue[0] = x;
		mValue[1] = y;
		mValue[2] = z;
	}

	void setValue(float value[]) {
		mValue[0] = value[0];
		mValue[1] = value[1];
		mValue[2] = value[2];
	}

	void setValue(SFVec3f *vector) {
		setValue(vector->getX(), vector->getY(), vector->getZ());
	}

	void setX(float x) {
		setValue(x, getY(), getZ());
	}

	void setY(float y) {
		setValue(getX(), y, getZ());
	}

	void setZ(float z) {
		setValue(getX(), getY(), z);
	}

	////////////////////////////////////////////////
	//	add value
	////////////////////////////////////////////////

	void add(float x, float y, float z) {
		mValue[0] += x;
		mValue[1] += y;
		mValue[2] += z;
	}

	void add(float value[]) {
		mValue[0] += value[0];
		mValue[1] += value[1];
		mValue[2] += value[2];
	}

	void add(SFVec3f value) {
		add(value.getValue());
	}

	void translate(float x, float y, float z) {
		add(x, y, z);
	}

	void translate(float value[]) {
		add(value);
	}

	void translate(SFVec3f value) {
		add(value);
	}

	////////////////////////////////////////////////
	//	sub value
	////////////////////////////////////////////////

	void sub(float x, float y, float z) {
		mValue[0] -= x;
		mValue[1] -= y;
		mValue[2] -= z;
	}

	void sub(float value[]) {
		mValue[0] -= value[0];
		mValue[1] -= value[1];
		mValue[2] -= value[2];
	}

	void sub(SFVec3f value) {
		sub(value.getValue());
	}

	////////////////////////////////////////////////
	//	scale
	////////////////////////////////////////////////

	void scale(float value) {
		mValue[0] *= value;
		mValue[1] *= value;
		mValue[2] *= value;
	}

	void scale(float xscale, float yscale, float zscale) {
		mValue[0] *= xscale;
		mValue[1] *= yscale;
		mValue[2] *= zscale;
	}

	void scale(float value[3]) {
		scale(value[0], value[1], value[2]);
	}

	////////////////////////////////////////////////
	//	rotate
	////////////////////////////////////////////////

	void rotate(SFRotation *rotation);

	void rotate(float x, float y, float z, float angle);

	void rotate(float value[3]) {
		rotate(value[0], value[1], value[2], value[3]);
	}

	////////////////////////////////////////////////
	//	invert
	////////////////////////////////////////////////

	void invert() {
		mValue[0] = -mValue[0];
		mValue[1] = -mValue[1];
		mValue[2] = -mValue[2];
	}

	////////////////////////////////////////////////
	//	scalar
	////////////////////////////////////////////////

	float getScalar()
	{
		return (float)sqrt(mValue[0]*mValue[0]+mValue[1]*mValue[1]+mValue[2]*mValue[2]);
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
			mValue[2] /= scale;
		}
	}

	////////////////////////////////////////////////
	//	Output
	////////////////////////////////////////////////

	friend ostream& operator<<(ostream &s, SFVec3f &vector) {
		return s << vector.getX() << " " << vector.getY() << " " << vector.getZ();
	}

	friend ostream& operator<<(ostream &s, SFVec3f *vector) {
		return s << vector->getX() << " " << vector->getY() << " " << vector->getZ();
	}

	////////////////////////////////////////////////
	//	String
	////////////////////////////////////////////////

	void setValue(char *value) {
		if (!value)
			return;
		float	x, y, z;
		if (sscanf(value,"%f %f %f", &x, &y, &z) == 3) 
			setValue(x, y, z);
	}

	char *getValue(char *buffer, int bufferLen) {
		sprintf(buffer, "%g %g %g", getX(), getY(), getZ());
		return buffer;
	}

	////////////////////////////////////////////////
	//	Compare
	////////////////////////////////////////////////

	bool equals(Field *field) {
		SFVec3f *vector = (SFVec3f *)field;
		if (getX() == vector->getX() && getY() == vector->getY() && getZ() == vector->getZ())
			return true;
		else
			return false;
	}

	bool equals(float value[3]) {
		SFVec3f vector(value);
		return equals(&vector);
	}

	bool equals(float x, float y, float z) {
		SFVec3f vector(x, y, z);
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
	static jmethodID	mGetZMethodID;
	static jmethodID	mSetValueMethodID;
	static jmethodID	mSetNameMethodID;

	static jmethodID	mConstInitMethodID;
	static jmethodID	mConstGetXMethodID;
	static jmethodID	mConstGetYMethodID;
	static jmethodID	mConstGetZMethodID;
	static jmethodID	mConstSetValueMethodID;
	static jmethodID	mConstSetNameMethodID;

public:

	void		setJavaIDs();

	jclass		getFieldID()				{return mFieldClassID;}
	jclass		getConstFieldID()			{return mConstFieldClassID;}

	jmethodID	getInitMethodID()			{return mInitMethodID;}
	jmethodID	getGetXMethodID()			{return mGetXMethodID;}
	jmethodID	getGetYMethodID()			{return mGetYMethodID;}
	jmethodID	getGetZMethodID()			{return mGetZMethodID;}
	jmethodID	getSetValueMethodID()		{return mSetValueMethodID;}
	jmethodID	getSetNameMethodID()		{return mSetNameMethodID;}

	jmethodID	getConstInitMethodID()		{return mConstInitMethodID;}
	jmethodID	getConstGetXMethodID()		{return mConstGetXMethodID;}
	jmethodID	getConstGetYMethodID()		{return mConstGetYMethodID;}
	jmethodID	getConstGetZMethodID()		{return mConstGetZMethodID;}
	jmethodID	getConstSetValueMethodID()	{return mConstSetValueMethodID;}
	jmethodID	getConstSetNameMethodID()	{return mConstSetNameMethodID;}

	jobject toJavaObject(int bConstField = 0);
	void setValue(jobject field, int bConstField = 0);
	void getValue(jobject field, int bConstField = 0);

#endif

};

#endif //JMC
