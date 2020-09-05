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
*	File:	BindableNode.h
*
******************************************************************/

#ifndef _BINDABLENODE_H_
#define _BINDABLENODE_H_

#include "vrmlfields.h"
#include "Node.h"

class BindableNode : public Node {

public:

	BindableNode() {
		// set_bind
		SFBool *setBind = new SFBool(true);
		addEventIn(setBindFieldString, setBind);

		// cybleInterval exposed field
		SFTime *bindTime = new SFTime(1.0);
		addEventOut(bindTimeFieldString, bindTime);

		// isBind
		SFBool *isBound = new SFBool(true);
		addEventOut(isBoundFieldString, isBound);
	}

	virtual ~BindableNode() {
	}

	////////////////////////////////////////////////
	//	bind
	////////////////////////////////////////////////

	void setBind(bool value) {
		SFBool *bind = (SFBool *)getEventIn(setBindFieldString);
		bind->setValue(value);
	}
	bool  getBind() {
		SFBool *bind = (SFBool *)getEventIn(setBindFieldString);
		return bind->getValue();
	}
	Field *getBindField() {
		return getEventIn(setBindFieldString);
	}

	////////////////////////////////////////////////
	//	bindTime
	////////////////////////////////////////////////
	
	void setBindTime(double value) {
		SFTime *cycle = (SFTime *)getEventOut(bindTimeFieldString);
		cycle->setValue(value);
	}
	double getBindTime() {
		SFTime *cycle = (SFTime *)getEventOut(bindTimeFieldString);
		return cycle->getValue();
	}
	Field *getBindTimeField() {
		return getEventOut(bindTimeFieldString);
	}

	////////////////////////////////////////////////
	//	isBound
	////////////////////////////////////////////////

	void setIsBound(bool  value) {
		SFBool *isBound = (SFBool *)getEventOut(isBoundFieldString);
		isBound->setValue(value);
	}
	bool  getIsBound() {
		SFBool *isBound = (SFBool *)getEventOut(isBoundFieldString);
		return isBound->getValue();
	}
	bool  isBound() {
		return getIsBound();
	}
	Field *getIsBoundField() {
		return getEventOut(isBoundFieldString);
	}
};

#endif

