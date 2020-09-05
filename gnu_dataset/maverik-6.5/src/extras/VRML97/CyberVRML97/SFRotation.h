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
*	File:	SFRotation.h
*
******************************************************************/

#ifndef _SFROTATION_H_
#define _SFROTATION_H_

#include <iostream.h>
#include "SFVec3f.h"
#include "Field.h"

class SFMatrix;

class SFRotation : public Field {

	static	int	mInit;

	SFVec3f		mVector;
	float		mAngle;

public:

	SFRotation() {
		setType(fieldTypeSFRotation);
		setValue(0.0f, 0.0f, 1.0f, 0.0f);
		InitializeJavaIDs();
	}

	SFRotation(float x, float y, float z, float rot) {
		setType(fieldTypeSFRotation);
		setValue(x, y, z, rot);
		InitializeJavaIDs();
	}

	SFRotation(float value[]) {
		setType(fieldTypeSFRotation);
		setValue(value);
		InitializeJavaIDs();
	}

	SFRotation(SFRotation *value) {
		setType(fieldTypeSFRotation);
		setValue(value);
		InitializeJavaIDs();
	}

	SFRotation(SFMatrix *matrix) {
		setType(fieldTypeSFRotation);
		setValue(matrix);
		InitializeJavaIDs();
	}

	void InitializeJavaIDs() {
#ifdef SUPPORT_JSAI
		setJavaIDs();
#endif
	}

	////////////////////////////////////////////////
	//	set value
	////////////////////////////////////////////////

	void setValue(float x, float y, float z, float rot) {
		mVector.setValue(x, y, z);
		mVector.normalize();
		mAngle = rot;
	}

	void setValue(float value[]) {
		mVector.setValue(value);
		mVector.normalize();
		mAngle = value[3];
	}

	void setValue(SFRotation *rotation) {
		setValue(rotation->getX(), rotation->getY(), rotation->getZ(), rotation->getAngle());
	}

	void setValue(SFMatrix *matrix);

	////////////////////////////////////////////////
	//	get value
	////////////////////////////////////////////////

	void getValue(float value[]) {
		mVector.getValue(value);
		value[3] = mAngle;
	}
/*
	float[] getValue() {
		float value[] = new float[4];
		getValue(value);
		return value;
	}
*/
	void getVector(float vector[]) {
		mVector.getValue(vector);
	}
/*
	float[] getVector(float vector) {
		float vector[] = new float[3];
		mVector.getValue(vector);
		return vector;
	}
*/
	float getX() {
		return mVector.getX();
	}

	float getY() {
		return mVector.getY();
	}

	float getZ() {
		return mVector.getZ();
	}

	float getAngle() {
		return mAngle;
	}

	////////////////////////////////////////////////
	//	add 
	////////////////////////////////////////////////

	void add(SFRotation *rot);

	void add(float rotationValue[]) {
		SFRotation rotation(rotationValue);
		add(&rotation);
	}

	void add(float x, float y, float z, float rot) {
		float rotationValue[4];
		rotationValue[0] = x;
		rotationValue[1] = y;
		rotationValue[2] = z;
		rotationValue[3] = rot;
		add(rotationValue);
	}

	////////////////////////////////////////////////
	//	multi 
	////////////////////////////////////////////////

	void multi(float vector[]);
	void multi(float *x, float *y, float *z);
	void multi(SFVec3f *vector);

	////////////////////////////////////////////////
	//	convert
	////////////////////////////////////////////////

	void getSFMatrix(SFMatrix *matrix);

	////////////////////////////////////////////////
	//	invert
	////////////////////////////////////////////////

	void invert() {
		mAngle = -mAngle;
	}

	////////////////////////////////////////////////
	//	Output
	////////////////////////////////////////////////

	friend ostream& operator<<(ostream &s, SFRotation &rotation) {
		return s << rotation.getX() << " " << rotation.getY() << " " << rotation.getZ() << " " << rotation.getAngle();
	}

	friend ostream& operator<<(ostream &s, SFRotation *rotation) {
		return s << rotation->getX() << " " << rotation->getY() << " " << rotation->getZ()  << " " << rotation->getAngle();
	}

	////////////////////////////////////////////////
	//	String
	////////////////////////////////////////////////

	void setValue(char *value) {
		if (!value)
			return;
		float	x, y, z, angle;
		if (sscanf(value,"%f %f %f %f", &x, &y, &z, &angle) == 4) 
			setValue(x, y, z, angle);
	}

	char *getValue(char *buffer, int bufferLen) {
		sprintf(buffer, "%g %g %g %g", getX(), getY(), getZ(), getAngle());
		return buffer;
	}

	////////////////////////////////////////////////
	//	initialize
	////////////////////////////////////////////////

	void initValue()
	{
		setValue(0.0f, 0.0f, 1.0f, 0.0f);
	}

	////////////////////////////////////////////////
	//	Compare
	////////////////////////////////////////////////

	bool equals(Field *field) {
		SFRotation *rotation = (SFRotation *)field;
		if (getX() == rotation->getX() && getY() == rotation->getY() && getZ() == rotation->getZ() && getAngle() == rotation->getAngle())
			return true;
		else
			return false;
	}

	bool equals(float value[4]) {
		SFRotation rotation(value);
		return equals(&rotation);
	}

	bool equals(float x, float y, float z, float angle) {
		SFRotation rotation(x, y, z, angle);
		return equals(&rotation);
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
	static jmethodID	mGetAngleMethodID;
	static jmethodID	mSetValueMethodID;
	static jmethodID	mSetNameMethodID;

	static jmethodID	mConstInitMethodID;
	static jmethodID	mConstGetXMethodID;
	static jmethodID	mConstGetYMethodID;
	static jmethodID	mConstGetZMethodID;
	static jmethodID	mConstGetAngleMethodID;
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
	jmethodID	getGetAngleMethodID()		{return mGetAngleMethodID;}
	jmethodID	getSetValueMethodID()		{return mSetValueMethodID;}
	jmethodID	getSetNameMethodID()		{return mSetNameMethodID;}

	jmethodID	getConstInitMethodID()		{return mConstInitMethodID;}
	jmethodID	getConstGetXMethodID()		{return mConstGetXMethodID;}
	jmethodID	getConstGetYMethodID()		{return mConstGetYMethodID;}
	jmethodID	getConstGetZMethodID()		{return mConstGetZMethodID;}
	jmethodID	getConstGetAngleMethodID()	{return mConstGetAngleMethodID;}
	jmethodID	getConstSetValueMethodID()	{return mConstSetValueMethodID;}
	jmethodID	getConstSetNameMethodID()	{return mConstSetNameMethodID;}

	jobject toJavaObject(int bConstField = 0);
	void setValue(jobject field, int bConstField = 0);
	void getValue(jobject field, int bConstField = 0);

#endif

};

#endif //JMC
