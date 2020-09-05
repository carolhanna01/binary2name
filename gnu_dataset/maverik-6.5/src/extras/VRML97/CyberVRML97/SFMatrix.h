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
*	File:	SFMatrix.h
*
******************************************************************/

#ifndef _SFMatrix_H_
#define _SFMatrix_H_

#include <math.h>
#include "Field.h"
#include "SFVec3f.h"
#include "SFRotation.h"

class SFRotation;

class SFMatrix : public Field {

	float mValue[4][4]; 

public:

	SFMatrix() {
		init();
	}

	SFMatrix(float value[4][4]) {
		setValue(value);
	}

	SFMatrix(SFMatrix *value) {
		setValue(value);
	}

	SFMatrix(SFRotation *rot);

	SFMatrix(float x, float y, float z, float angle);

	SFMatrix(float x, float y, float z);

	////////////////////////////////////////////////
	//	set value 
	////////////////////////////////////////////////

	void setValue(float value[4][4]) {
		for (int i=0; i<4; i++) {
			for (int j=0; j<4; j++)
				mValue[i][j] = value[i][j];
		}
	}

	void setValue(SFMatrix *matrix) {
		float	value[4][4];
		matrix->getValue(value);
		setValue(value);
	}

	////////////////////////////////////////////////
	//	set as scaling value 
	////////////////////////////////////////////////

	void setScaling(SFVec3f *vector) {
		setScaling(vector->getX(), vector->getY(), vector->getZ());
	}

	void setScaling(float value[]) {
		init();
		setScaling(value[0], value[1], value[2]);
	}

	void setScaling(float x, float y, float z) {
		init();
		mValue[0][0] = x;
		mValue[1][1] = y;
		mValue[2][2] = z;
	}

	////////////////////////////////////////////////
	//	set as translation value 
	////////////////////////////////////////////////

	void setTranslation(SFVec3f *vector) {
		setTranslation(vector->getX(), vector->getY(), vector->getZ());
	}

	void setTranslation(float value[]) {
		setTranslation(value[0], value[1], value[2]);
	}

	void setTranslation(float x, float y, float z) {
		init();
		mValue[3][0] = x;
		mValue[3][1] = y;
		mValue[3][2] = z;
	}

	////////////////////////////////////////////////
	//	set as direction value 
	////////////////////////////////////////////////

	void setDirection(SFVec3f *vector) {
		setDirection(vector->getX(), vector->getY(), vector->getZ());
	}

	void setDirection(float value[]) {
		setDirection(value[0], value[1], value[2]);
	}

	void setDirection(float x, float y, float z);

	////////////////////////////////////////////////
	//	set as rotation value 
	////////////////////////////////////////////////

	void setRotation(SFRotation *rotation) {
		setRotation(rotation->getX(), rotation->getY(), rotation->getZ(), rotation->getAngle());
	}

	void setRotation(float value[]) {
		setRotation(value[0], value[1], value[2], value[3]);
	}

	void setRotation(float x, float y, float z, float rot);
	
	////////////////////////////////////////////////
	//	get value
	////////////////////////////////////////////////
/*
	float **getValue() {
		return mValue;
	}
*/
	void getValue(float value[4][4]) {
		for (int i=0; i<4; i++) {
			for (int j=0; j<4; j++)
				value[i][j] = mValue[i][j];
		}
	}

	////////////////////////////////////////////////
	//	get value only translation
	////////////////////////////////////////////////
/*
	float[] getValueOnlyTranslation() {
		float value[] = new float[3];
		getValueOnlyTranslation(value);
		return value;
	}
*/
	void getTranslation(float value[]) {
		value[0] = mValue[3][0];
		value[1] = mValue[3][1];
		value[2] = mValue[3][2];
	}

	////////////////////////////////////////////////
	//	add 
	////////////////////////////////////////////////

	void add(SFMatrix *matrix);

	////////////////////////////////////////////////
	//	multi 
	////////////////////////////////////////////////

	void multi(float vector[]);
	void multi(float *x, float *y, float *z);
	void multi(SFVec3f *vector);

	////////////////////////////////////////////////
	//	convert
	////////////////////////////////////////////////

	void getSFRotation(SFRotation *rotation);

	////////////////////////////////////////////////
	//	toString
	////////////////////////////////////////////////

	void setValue(char *value) {
	}

	char *getValue(char *buffer, int bufferLen) {
		buffer[0] = '\0';
		return buffer;
	}

	////////////////////////////////////////////////
	//	other
	////////////////////////////////////////////////
	
	void invert();
    
	float determinant();

	void init()
	{
		for (int i=0; i<4; i++) {
			for (int j=0; j<4; j++)
				mValue[i][j] = 0.0f;
		}

		for (int n=0; n<4; n++)
			mValue[n][n] = 1.0f;
	}

	////////////////////////////////////////////////
	//	Java
	////////////////////////////////////////////////

#ifdef SUPPORT_JSAI

	jobject toJavaObject(int bConstField = 0);
	void setValue(jobject field, int bConstField = 0);
	void getValue(jobject field, int bConstField = 0);

#endif
};

#endif //JMC
