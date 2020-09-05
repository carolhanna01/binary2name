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
*	File:	DEF.h
*
******************************************************************/

#ifndef _DEF_H_
#define _DEF_H_

#include "JString.h"

class DEF : public CLinkedListNode<DEF> {

	JString		mName;
	JString		mString;

public:

	DEF (char *name, char *string) {
		setName(name);
		setString(string);
	}

	~DEF() {
		remove();
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
	//	Name
	////////////////////////////////////////////////

	void setString(char *string) {
		mString.setValue(string);
	}

	char *getString() {
		return mString.getValue();
	}
};

#endif


