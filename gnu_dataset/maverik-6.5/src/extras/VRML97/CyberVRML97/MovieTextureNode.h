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
*	File:	MovieTextureNode.h
*
******************************************************************/

#ifndef _MOVIETEXTURE_H_
#define _MOVIETEXTURE_H_

#include "vrmlfields.h"
#include "Node.h"

class MovieTextureNode : public Node {
	
public:

	MovieTextureNode() {
		setHeaderFlag(false);
		setType(movieTextureNodeString);

		///////////////////////////
		// Exposed Field 
		///////////////////////////

		// url field
		MFString *url = new MFString();
		addExposedField(urlFieldString, url);

		// loop exposed field
		SFBool *loop = new SFBool(false);
		addExposedField(loopFieldString, loop);

		// startTime exposed field
		SFTime *startTime = new SFTime(0.0f);
		addExposedField(startTimeFieldString, startTime);

		// stopTime exposed field
		SFTime *stopTime = new SFTime(0.0f);
		addExposedField(stopTimeFieldString, stopTime);

		// speed exposed field
		SFFloat *speed = new SFFloat(1.0f);
		addExposedField(speedTimeFieldString, speed);

		///////////////////////////
		// Field 
		///////////////////////////

		// repeatS field
		SFBool *repeatS = new SFBool(true);
		addField(repeatSFieldString, repeatS);

		// repeatT field
		SFBool *repeatT = new SFBool(true);
		addField(repeatTFieldString, repeatT);

		///////////////////////////
		// EventOut
		///////////////////////////

		// isActive eventOut field
		SFBool *isActive = new SFBool(false);
		addEventOut(isActiveFieldString, isActive);

		// time eventOut field
		SFTime *durationChanged = new SFTime(-1.0f);
		addEventOut(durationFieldString, durationChanged);
	}

	~MovieTextureNode() {
	}

	////////////////////////////////////////////////
	//	RepeatS
	////////////////////////////////////////////////
	
	void setRepeatS(bool value) {
		SFBool *repeatS = (SFBool *)getField(repeatSFieldString);
		repeatS->setValue(value);
	}
	void setRepeatS(int value) {
		setRepeatS(value ? true : false);
	}
	bool getRepeatS() {
		SFBool *repeatS = (SFBool *)getField(repeatSFieldString);
		return repeatS->getValue();
	}

	////////////////////////////////////////////////
	//	RepeatT
	////////////////////////////////////////////////
	
	void setRepeatT(bool value) {
		SFBool *repeatT = (SFBool *)getField(repeatTFieldString);
		repeatT->setValue(value);
	}
	void setRepeatT(int value) {
		setRepeatT(value ? true : false);
	}
	bool getRepeatT() {
		SFBool *repeatT = (SFBool *)getField(repeatTFieldString);
		return repeatT->getValue();
	}

	////////////////////////////////////////////////
	// Url
	////////////////////////////////////////////////

	void addUrl(String value) {
		MFString *url = (MFString *)getExposedField(urlFieldString);
		url->addValue(value);
	}
	int getNUrls() {
		MFString *url = (MFString *)getExposedField(urlFieldString);
		return url->getSize();
	}
	String getUrl(int index) {
		MFString *url = (MFString *)getExposedField(urlFieldString);
		return url->get1Value(index);
	}
	void setUrl(int index, char *urlString) {
		MFString *url = (MFString *)getExposedField(urlFieldString);
		url->set1Value(index, urlString);
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
	bool IsLoop() {
		return getLoop();
	}

	////////////////////////////////////////////////
	//	Speed
	////////////////////////////////////////////////
	
	void setSpeed(float value) {
		SFFloat *time = (SFFloat *)getExposedField(speedTimeFieldString);
		time->setValue(value);
	}
	float getSpeed() {
		SFFloat *time = (SFFloat *)getExposedField(speedTimeFieldString);
		return time->getValue();
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

	////////////////////////////////////////////////
	//	isActive
	////////////////////////////////////////////////
	
	void setIsActive(bool value) {
		SFBool *field = (SFBool *)getEventOut(isActiveFieldString);
		field->setValue(value);
	}
	bool getIsActive() {
		SFBool *field = (SFBool *)getEventOut(isActiveFieldString);
		return field->getValue();
	}
	bool isActive() {
		SFBool *field = (SFBool *)getEventOut(isActiveFieldString);
		return field->getValue();
	}

	////////////////////////////////////////////////
	//	duration_changed
	////////////////////////////////////////////////
	
	void setDurationChanged(double value) {
		SFTime *time = (SFTime *)getEventOut(durationFieldString);
		time->setValue(value);
	}
	double getDurationChanged() {
		SFTime *time = (SFTime *)getEventOut(durationFieldString);
		return time->getValue();
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	MovieTextureNode *next() {
		return (MovieTextureNode *)Node::next(getType());
	}

	MovieTextureNode *nextTraversal() {
		return (MovieTextureNode *)Node::nextTraversalByType(getType());
	}

	////////////////////////////////////////////////
	//	functions
	////////////////////////////////////////////////
	
	bool isChildNodeType(Node *node){
		return false;
	}

	void initialize() {
	}

	void uninitialize() {
	}

	void update() {
	}

	////////////////////////////////////////////////
	//	Urlmation
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
		SFBool *loop = (SFBool *)getExposedField(loopFieldString);
		SFBool *repeatS = (SFBool *)getField(repeatSFieldString);
		SFBool *repeatT = (SFBool *)getField(repeatTFieldString);

		printStream << indentString << "\t" << "loop " << loop << endl;
		printStream << indentString << "\t" << "speed " << getSpeed() << endl;
		printStream << indentString << "\t" << "startTime " << getStartTime() << endl;
		printStream << indentString << "\t" << "stopTime " << getStopTime() << endl;
		printStream << indentString << "\t" << "repeatS " << repeatS << endl;
		printStream << indentString << "\t" << "repeatT " << repeatT << endl;

		if (0 < getNUrls()) {
			MFString *url = (MFString *)getExposedField(urlFieldString);
			printStream << indentString << "\t" << "url [" << endl;
			url->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}
	}
};

#endif
