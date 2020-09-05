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
*	File:	CString.h
*
******************************************************************/

#ifndef _JSTRING_H_
#define _JSTRING_H_
 
#include <stdlib.h>
#include <string.h>
#include <ctype.h> //JMC

class  JString 
{
	char	*mValue;

public:

	// Constructors
	JString() {
		mValue = NULL;
	}
	JString(char value[]) {
		mValue = NULL;
		setValue(value);
	}
	JString(char value[], int offset, int count) { 
		mValue = NULL;
		setValue(value, offset, count);
	}

	// Destructor
	~JString() {
		deleteValue();
	}

	// Methods
	void setValue(char value[]) {
		deleteValue();
		if (!value)
			return;
		if (strlen(value) <= 0)
			return;
		mValue = new char[strlen(value)+1];
		strcpy(mValue, value);
	}

	void setValue(char value[], int offset, int count) { 
		deleteValue();
		if (!value && (int)strlen(value) < (offset + count)) 
			return;
		mValue = new char[count+1];
		strncpy(mValue, &value[offset], count);
	}

	char *getValue() {
		return mValue;
	}

	void deleteValue() {
		delete[] mValue;
		mValue = NULL;
	}

	int length() {
		if (!mValue)
			return 0;
		return strlen(mValue);
	}

	char charAt(int  index) {
		return mValue[index];
	}

	int compareTo(char *anotherString) {
		if (!mValue || !anotherString)
			return -1;
		return strcmp(mValue, anotherString);
	}

	int compareToIgnoreCase(char *anotherString) {
		if (!mValue || !anotherString)
			return -1;
	
		int n;

		char *value1 = new char[strlen(mValue)+1]; 
		strcpy(value1, mValue);
		for (n=0; n<(int)strlen(mValue); n++)
			value1[n] = (char)toupper(value1[n]);

		char *value2 = new char[strlen(anotherString)+1]; 
		strcpy(value2, anotherString);
		for (n=0; n<(int)strlen(anotherString); n++)
			value2[n] = (char)toupper(value2[n]);
		
		int ret = strcmp(value1, value2);

		delete value1;
		delete value2;

		return ret;
	}

	void concat(char *str) {
		if (!str)
			return;
		char *value = new char [length()+strlen(str)+1];
		if (mValue) {
			strcpy(value, mValue);
			strcat(value, str);
		}
		else
			strcpy(value, str);
		delete mValue;
		mValue = value;
	}

	void copyValueOf(char data[]) {
		if (!data || !mValue)
			return;
		strcpy(data, mValue);
	}

	void copyValueOf(char  data[], int  offset, int count) {
		if (!data || !mValue)
			return;
		strncpy(data, &mValue[offset], count);
	}

	int regionMatches(int toffset, char *other, int ooffset, int len) {
		if (!mValue || !other)
			return -1;
		if (length() < toffset)
			return -1;
		if ((int)strlen(other) < ooffset + len)
			return -1;
		return strncmp(&mValue[toffset], &other[ooffset], len);
	}

	int regionMatchesIgnoreCase(int toffset, char *other, int ooffset, int len){
		if (!mValue || !other)
			return -1;
	
		int n;

		char *value1 = new char[strlen(mValue)+1]; 
		strcpy(value1, mValue);
		for (n=0; n<(int)strlen(mValue); n++)
			value1[n] = (char)toupper(value1[n]);

		char *value2 = new char[strlen(other)+1]; 
		strcpy(value2, other);
		for (n=0; n<(int)strlen(other); n++)
			value2[n] = (char)toupper(value2[n]);
		
		int ret = regionMatches(toffset, other, ooffset, len);

		delete value1;
		delete value2;

		return ret;
	}

	int startsWith(char *prefix) {
		if (!prefix || !mValue)
			return -1;
		return regionMatches(0, prefix, 0, strlen(prefix));
	}
	int endsWith(char *suffix) {
		if (!suffix || !mValue)
			return -1;
		return regionMatches(strlen(mValue)-strlen(suffix), suffix, 0, strlen(suffix));
	}
};

#endif
