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
*	File:	TimeSensorNode.h
*
******************************************************************/

#ifndef _TIMESENSOR_H_
#define _TIMESENSOR_H_

#include <time.h>
#ifdef WIN32
#include <sys/timeb.h>
#endif
#include "vrmlfields.h"
#include "Node.h"

class TimeSensorNode : public Node {

public:

	TimeSensorNode() {
		setHeaderFlag(false);
		setType(timeSensorNodeString);

		// enabled exposed field
		SFBool *enabled = new SFBool(true);
		addExposedField(enabledFieldString, enabled);

		// loop exposed field
		SFBool *loop = new SFBool(false);
		addExposedField(loopFieldString, loop);

		// cybleInterval exposed field
		SFTime *cybleInterval = new SFTime(1.0);
		addExposedField(cybleIntervalFieldString, cybleInterval);

		// startTime exposed field
		SFTime *startTime = new SFTime(0.0f);
		addExposedField(startTimeFieldString, startTime);

		// stopTime exposed field
		SFTime *stopTime = new SFTime(0.0f);
		addExposedField(stopTimeFieldString, stopTime);

	
		// cycleTime eventOut field
		SFTime *cycleTime = new SFTime(-1.0f);
		addEventOut(cycleTimeFieldString, cycleTime);

		// time eventOut field
		SFTime *time = new SFTime(-1.0f);
		addEventOut(timeFieldString, time);

		// isActive eventOut field
		SFBool *isActive = new SFBool(false);
		addEventOut(isActiveFieldString, isActive);

		// fraction_changed eventOut field
		SFFloat *fractionChanged = new SFFloat(0.0f);
		addEventOut(fractionFieldString, fractionChanged);
	}

	~TimeSensorNode() {
	}

	////////////////////////////////////////////////
	//	Enabled
	////////////////////////////////////////////////
	
	void setEnabled(bool value) {
		SFBool *bEnabled = (SFBool *)getExposedField(enabledFieldString);
		bEnabled->setValue(value);
	}
	void setEnabled(int value) {
		setEnabled(value ? true : false);
	}
	bool getEnabled() {
		SFBool *bEnabled = (SFBool *)getExposedField(enabledFieldString);
		return bEnabled->getValue();
	}
	bool isEnabled() {
		return getEnabled();
	}
	Field *getEnabledField() {
		return getExposedField(enabledFieldString);
	}

	////////////////////////////////////////////////
	//	Loop
	////////////////////////////////////////////////
	
	void setLoop(bool value) {
		SFBool *loop = (SFBool *)getExposedField(loopFieldString);
		loop->setValue(value);
	}
	void setLoop(int value) {
		setLoop(value ? true : false);
	}
	bool getLoop() {
		SFBool *loop = (SFBool *)getExposedField(loopFieldString);
		return loop->getValue();
	}
	bool isLoop() {
		return getLoop();
	}
	Field *getLoopField() {
		return getExposedField(loopFieldString);
	}

	////////////////////////////////////////////////
	//	Cyble Interval
	////////////////////////////////////////////////
	
	void setCycleInterval(double value) {
		SFTime *cycle = (SFTime *)getExposedField(cybleIntervalFieldString);
		cycle->setValue(value);
	}
	double getCycleInterval() {
		SFTime *cycle = (SFTime *)getExposedField(cybleIntervalFieldString);
		return cycle->getValue();
	}
	Field *getCycleIntervalField() {
		return getExposedField(cybleIntervalFieldString);
	}

	////////////////////////////////////////////////
	//	Start time
	////////////////////////////////////////////////
	
	void setStartTime(double value) {
		SFTime *time = (SFTime *)getExposedField(startTimeFieldString);
		time->setValue(value);
	}
	double getStartTime() {
		SFTime *time = (SFTime *)getExposedField(startTimeFieldString);
		return time->getValue();
	}
	Field *getStartTimeField() {
		return getExposedField(startTimeFieldString);
	}

	////////////////////////////////////////////////
	//	Stop time
	////////////////////////////////////////////////
	
	void setStopTime(double value) {
		SFTime *time = (SFTime *)getExposedField(stopTimeFieldString);
		time->setValue(value);
	}
	double getStopTime() {
		SFTime *time = (SFTime *)getExposedField(stopTimeFieldString);
		return time->getValue();
	}
	Field *getStopTimeField() {
		return getExposedField(stopTimeFieldString);
	}

	////////////////////////////////////////////////
	//	isActive
	////////////////////////////////////////////////
	
	void setIsActive(bool  value) {
		SFBool *field = (SFBool *)getEventOut(isActiveFieldString);
		field->setValue(value);
	}
	bool  getIsActive() {
		SFBool *field = (SFBool *)getEventOut(isActiveFieldString);
		return field->getValue();
	}
	bool  isActive() {
		SFBool *field = (SFBool *)getEventOut(isActiveFieldString);
		return field->getValue();
	}
	Field *getIsActiveField() {
		return getEventOut(isActiveFieldString);
	}

	////////////////////////////////////////////////
	//	fraction_changed
	////////////////////////////////////////////////
	
	void setFractionChanged(float value) {
		SFFloat *field = (SFFloat *)getEventOut(fractionFieldString);
		field->setValue(value);
	}
	float getFractionChanged() {
		SFFloat *field = (SFFloat *)getEventOut(fractionFieldString);
		return field->getValue();
	}
	Field *getFractionChangedField() {
		return getEventOut(fractionFieldString);
	}

	////////////////////////////////////////////////
	//	Cycle time
	////////////////////////////////////////////////
	
	void setCycleTime(double value) {
		SFTime *time = (SFTime *)getEventOut(cycleTimeFieldString);
		time->setValue(value);
	}
	double getCycleTime() {
		SFTime *time = (SFTime *)getEventOut(cycleTimeFieldString);
		return time->getValue();
	}
	Field *getCycleTimeField() {
		return getEventOut(cycleTimeFieldString);
	}

	////////////////////////////////////////////////
	//	Time
	////////////////////////////////////////////////
	
	void setTime(double value) {
		SFTime *time = (SFTime *)getEventOut(timeFieldString);
		time->setValue(value);
	}
	double getTime() {
		SFTime *time = (SFTime *)getEventOut(timeFieldString);
		return time->getValue();
	}
	Field *getTimeField() {
		return getEventOut(timeFieldString);
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	TimeSensorNode *next() {
		return (TimeSensorNode *)Node::next(getType());
	}

	TimeSensorNode *nextTraversal() {
		return (TimeSensorNode *)Node::nextTraversalByType(getType());
	}

	////////////////////////////////////////////////
	//	Virtual functions
	////////////////////////////////////////////////
	
	bool isChildNodeType(Node *node){
		return false;
	}

	void initialize() {
		setCycleTime(-1.0f);
		setIsActive(false);
	}

	void uninitialize() {
	}

	void update();

	void outputContext(ostream &printStream, String indentString) {
		SFBool *bEnabled = (SFBool *)getExposedField(enabledFieldString);
		SFBool *loop = (SFBool *)getExposedField(loopFieldString);

		printStream << indentString << "\t" << "cycleInterval " << getCycleInterval() << endl;
		printStream << indentString << "\t" << "enabled " << bEnabled << endl;
		printStream << indentString << "\t" << "loop " << loop << endl;
		printStream << indentString << "\t" << "startTime " << getStartTime() << endl;
		printStream << indentString << "\t" << "stopTime " << getStopTime() << endl;
	}

};

#endif

