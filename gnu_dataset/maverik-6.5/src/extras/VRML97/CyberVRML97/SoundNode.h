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
*	File:	SoundNode.h
*
******************************************************************/

#ifndef _SOUND_H_
#define _SOUND_H_

#include "vrmlfields.h"
#include "Node.h"

class SoundNode : public Node {
	
public:

	SoundNode() {
		setHeaderFlag(false);
		setType(soundNodeString);

		///////////////////////////
		// Exposed Field 
		///////////////////////////

		// minFront exposed field
		SFFloat *minFront = new SFFloat(1.0f);
		addExposedField(minFrontFieldString, minFront);

		// maxFront exposed field
		SFFloat *maxFront = new SFFloat(10.0f);
		addExposedField(maxFrontFieldString, maxFront);

		// minBack exposed field
		SFFloat *minBack = new SFFloat(1.0f);
		addExposedField(minBackFieldString, minBack);

		// maxBack exposed field
		SFFloat *maxBack = new SFFloat(10.0f);
		addExposedField(maxBackFieldString, maxBack);

		// intensity exposed field
		SFFloat *intensity = new SFFloat(10.0f);
		addExposedField(intensityFieldString, intensity);

		// priority exposed field
		SFFloat *priority = new SFFloat(0.0f);
		addExposedField(priorityFieldString, priority);

		// direction exposed field
		SFVec3f *direction = new SFVec3f(0.0f, 0.0f, 1.0f);
		addExposedField(directionFieldString, direction);

		// location exposed field
		SFVec3f *location = new SFVec3f(0.0f, 0.0f, 0.0f);
		addExposedField(locationFieldString, location);

		///////////////////////////
		// Field 
		///////////////////////////

		// spatialize exposed field
		SFBool *spatialize = new SFBool(true);
		addField(spatializeFieldString, spatialize);
	}

	~SoundNode() {
	}

	////////////////////////////////////////////////
	//	Direction
	////////////////////////////////////////////////

	void setDirection(float value[]) {
		SFVec3f *direction = (SFVec3f *)getExposedField(directionFieldString);
		direction->setValue(value);
	}
	void setDirection(float x, float y, float z) {
		SFVec3f *direction = (SFVec3f *)getExposedField(directionFieldString);
		direction->setValue(x, y, z);
	}
	void getDirection(float value[]) {
		SFVec3f *direction = (SFVec3f *)getExposedField(directionFieldString);
		direction->getValue(value);
	}

	////////////////////////////////////////////////
	//	Location
	////////////////////////////////////////////////

	void setLocation(float value[]) {
		SFVec3f *location = (SFVec3f *)getExposedField(locationFieldString);
		location->setValue(value);
	}
	void setLocation(float x, float y, float z) {
		SFVec3f *location = (SFVec3f *)getExposedField(locationFieldString);
		location->setValue(x, y, z);
	}
	void getLocation(float value[]) {
		SFVec3f *location = (SFVec3f *)getExposedField(locationFieldString);
		location->getValue(value);
	}

	////////////////////////////////////////////////
	//	MinFront
	////////////////////////////////////////////////
	
	void setMinFront(float value) {
		SFFloat *sffloat = (SFFloat *)getExposedField(minFrontFieldString);
		sffloat->setValue(value);
	}
	float getMinFront() {
		SFFloat *sffloat = (SFFloat *)getExposedField(minFrontFieldString);
		return sffloat->getValue();
	}

	////////////////////////////////////////////////
	//	MaxFront
	////////////////////////////////////////////////
	
	void setMaxFront(float value) {
		SFFloat *sffloat = (SFFloat *)getExposedField(maxFrontFieldString);
		sffloat->setValue(value);
	}
	float getMaxFront() {
		SFFloat *sffloat = (SFFloat *)getExposedField(maxFrontFieldString);
		return sffloat->getValue();
	}

	////////////////////////////////////////////////
	//	MinBack
	////////////////////////////////////////////////
	
	void setMinBack(float value) {
		SFFloat *sffloat = (SFFloat *)getExposedField(minBackFieldString);
		sffloat->setValue(value);
	}
	float getMinBack() {
		SFFloat *sffloat = (SFFloat *)getExposedField(minBackFieldString);
		return sffloat->getValue();
	}

	////////////////////////////////////////////////
	//	MaxBack
	////////////////////////////////////////////////
	
	void setMaxBack(float value) {
		SFFloat *sffloat = (SFFloat *)getExposedField(maxBackFieldString);
		sffloat->setValue(value);
	}
	float getMaxBack() {
		SFFloat *sffloat = (SFFloat *)getExposedField(maxBackFieldString);
		return sffloat->getValue();
	}

	////////////////////////////////////////////////
	//	Intensity
	////////////////////////////////////////////////
	
	void setIntensity(float value) {
		SFFloat *sffloat = (SFFloat *)getExposedField(intensityFieldString);
		sffloat->setValue(value);
	}
	float getIntensity() {
		SFFloat *sffloat = (SFFloat *)getExposedField(intensityFieldString);
		return sffloat->getValue();
	}

	////////////////////////////////////////////////
	//	Priority
	////////////////////////////////////////////////
	
	void setPriority(float value) {
		SFFloat *sffloat = (SFFloat *)getExposedField(priorityFieldString);
		sffloat->setValue(value);
	}
	float getPriority() {
		SFFloat *sffloat = (SFFloat *)getExposedField(priorityFieldString);
		return sffloat->getValue();
	}


	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	SoundNode *next() {
		return (SoundNode *)Node::next(getType());
	}

	SoundNode *nextTraversal() {
		return (SoundNode *)Node::nextTraversalByType(getType());
	}

	////////////////////////////////////////////////
	//	Spatialize
	////////////////////////////////////////////////
	
	void setSpatialize(bool value) {
		SFBool *bSpatialize = (SFBool *)getField(spatializeFieldString);
		bSpatialize->setValue(value);
	}
	void setSpatialize(int value) {
		setSpatialize(value ? true : false);
	}
	bool getSpatialize() {
		SFBool *bSpatialize = (SFBool *)getField(spatializeFieldString);
		return bSpatialize->getValue();
	}

	////////////////////////////////////////////////
	//	functions
	////////////////////////////////////////////////
	
	bool isChildNodeType(Node *node){
		if (node->isAudioClipNode() || node->isMovieTextureNode())
			return true;
		else
			return false;
	}

	void initialize() {
	}

	void uninitialize() {
	}

	void update() {
	}

	////////////////////////////////////////////////
	//	Infomation
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
		SFBool *spatialize = (SFBool *)getField(spatializeFieldString);
		SFVec3f *direction = (SFVec3f *)getExposedField(directionFieldString);
		SFVec3f *location = (SFVec3f *)getExposedField(locationFieldString);

		printStream << indentString << "\t" << "direction " << direction << endl;
		printStream << indentString << "\t" << "location " << location << endl;
		printStream << indentString << "\t" << "maxFront " << getMaxFront() << endl;
		printStream << indentString << "\t" << "minFront " << getMinFront() << endl;
		printStream << indentString << "\t" << "maxBack " << getMaxBack() << endl;
		printStream << indentString << "\t" << "minBack " << getMinBack() << endl;
		printStream << indentString << "\t" << "intensity " << getIntensity() << endl;
		printStream << indentString << "\t" << "priority " << getPriority() << endl;
		printStream << indentString << "\t" << "spatialize " << spatialize << endl;

		AudioClipNode *aclip = getAudioClipNodes();
		if (aclip != NULL) {
			if (aclip->isInstanceNode() == false) {
				if (aclip->getName() != NULL && strlen(aclip->getName()))
					printStream << indentString << "\t" << "source " << "DEF " << aclip->getName() << " AudioClip {" << endl;
				else
					printStream << indentString << "\t" << "source AudioClip {" << endl;
				aclip->Node::outputContext(printStream, indentString, "\t");
				printStream << indentString << "\t" << "}" << endl;
			}
			else 
				printStream << indentString << "\t" << "source USE " << aclip->getName() << endl;
		}

		MovieTextureNode *mtexture = getMovieTextureNodes();
		if (mtexture != NULL) {
			if (mtexture->isInstanceNode() == false) {
				if (mtexture->getName() != NULL && strlen(mtexture->getName()))
					printStream << indentString << "\t" << "source " << "DEF " << mtexture->getName() << " MovieTexture {" << endl;
				else
					printStream << indentString << "\t" << "source MovieTexture {" << endl;
				mtexture->Node::outputContext(printStream, indentString, "\t");
				printStream << indentString << "\t" << "}" << endl;
			}
			else 
				printStream << indentString << "\t" << "source USE " << mtexture->getName() << endl;
		}
	}
};

#endif

