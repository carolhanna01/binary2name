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
*	File:	PROTO.h
*
******************************************************************/

#ifndef _PROTO_H_
#define _PROTO_H_

#include "CLinkedList.h"
#include "JString.h"
#include "Field.h"
#include "JVector.h"

#define FIELD_SEPARATORS	" \t\n"

class PROTO : public CLinkedListNode<PROTO> {
	JString				mName;
	JString				mString;
	JVector<Field>		mDefaultFieldVector;
	JVector<Field>		mFieldVector;
public:

	PROTO(char *name, char *string, char *fieldString);
	~PROTO(void);

	void		setName(char *name)					{mName.setValue(name);}
	char		*getName(void)						{return mName.getValue();}

	void		setString(char *string)				{mString.setValue(string);}
	char		*getString()						{return mString.getValue();}
	void		getString(char *returnString);

	void		addDefaultField(Field *field)		{mDefaultFieldVector.addElement(field);}
	void		addField(Field *field)				{mFieldVector.addElement(field);}

	int			getNDefaultFields()					{return mDefaultFieldVector.size();}
	int			getNFields()						{return mFieldVector.size();}

	Field		*getDefaultField(int n)				{return (Field *)mDefaultFieldVector.elementAt(n);}
	Field		*getField(int n)					{return (Field *)mFieldVector.elementAt(n);}

	void		addFieldValues(char *fieldString, int bDefaultField);
	void		addDefaultFields(char *fieldString)	{deleteDefaultFields(); addFieldValues(fieldString, 1);}
	void		addFields(char *fieldString)		{deleteFields(); addFieldValues(fieldString, 0);}
	void		deleteDefaultFields(void)			{mDefaultFieldVector.removeAllElements();}
	void		deleteFields(void)					{mFieldVector.removeAllElements();}

	Field		*getField(char *name);
	int			getFieldType(char *name);
};


#endif


