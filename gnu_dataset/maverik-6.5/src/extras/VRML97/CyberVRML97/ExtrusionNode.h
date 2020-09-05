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
*	File:	ExtrusionNode.h
*
******************************************************************/

#ifndef _EXTRUSION_H_
#define _EXTRUSION_H_

#include "GeometryNode.h"

class ExtrusionNode : public GeometryNode {

public:

	ExtrusionNode() {

		setHeaderFlag(false);
		setType(extrusionNodeString);

		///////////////////////////
		// Field 
		///////////////////////////
		
		// beginCap field
		SFBool *beginCap = new SFBool(true);
		addField(beginCapFieldString, beginCap);

		// endCap field
		SFBool *endCap = new SFBool(true);
		addField(endCapFieldString, endCap);

		// ccw field
		SFBool *ccw = new SFBool(true);
		ccw->setName(ccwFieldString);
		addField(ccw);

		// convex field
		SFBool *convex = new SFBool(true);
		convex->setName(convexFieldString);
		addField(convex);

		// creaseAngle field
		SFFloat *creaseAngle = new SFFloat(0.0f);
		creaseAngle->setName(creaseAngleFieldString);
		addField(creaseAngle);

		// solid field
		SFBool *solid = new SFBool(true);
		solid->setName(solidFieldString);
		addField(solid);

		// orientation field
		MFRotation *orientation = new MFRotation();
		orientation->setName(orientationFieldString);
		addField(orientation);

		// scale field
		MFVec2f *scale = new MFVec2f();
		scale->setName(scaleFieldString);
		addField(scale);

		// crossSection field
		MFVec2f *crossSection = new MFVec2f();
		addField(crossSectionFieldString, crossSection);

		// spine field
		MFVec3f *spine = new MFVec3f();
		addField(spineFieldString, spine);

		///////////////////////////
		// EventIn
		///////////////////////////

		// orientation EventIn
		orientation = new MFRotation();
		orientation->setName(orientationFieldString);
		addEventIn(orientation);

		// scale EventIn
		scale = new MFVec2f();
		scale->setName(scaleFieldString);
		addEventIn(scale);

		// crossSection EventIn
		crossSection = new MFVec2f();
		addEventIn(crossSectionFieldString, crossSection);

		// spine EventIn
		spine = new MFVec3f();
		addEventIn(spineFieldString, spine);
	}

	~ExtrusionNode() {
	}

	////////////////////////////////////////////////
	//	BeginCap
	////////////////////////////////////////////////
	
	void setBeginCap(bool value) {
		SFBool *beginCap = (SFBool *)getField(beginCapFieldString);
		beginCap->setValue(value);
	}

	void setBeginCap(int value) {
		setBeginCap(value ? true : false);
	}

	bool getBeginCap() {
		SFBool *beginCap = (SFBool *)getField(beginCapFieldString);
		return beginCap->getValue();
	}

	////////////////////////////////////////////////
	//	EndCap
	////////////////////////////////////////////////
	
	void setEndCap(bool value) {
		SFBool *endCap = (SFBool *)getField(endCapFieldString);
		endCap->setValue(value);
	}

	void setEndCap(int value) {
		setEndCap(value ? true : false);
	}

	bool getEndCap() {
		SFBool *endCap = (SFBool *)getField(endCapFieldString);
		return endCap->getValue();
	}

	////////////////////////////////////////////////
	//	CCW
	////////////////////////////////////////////////
	
	void setCCW(bool value) {
		SFBool *ccw = (SFBool *)getField(ccwFieldString);
		ccw->setValue(value);
	}

	void setCCW(int value) {
		setCCW(value ? true : false);
	}

	bool getCCW() {
		SFBool *ccw = (SFBool *)getField(ccwFieldString);
		return ccw->getValue();
	}

	////////////////////////////////////////////////
	//	Convex
	////////////////////////////////////////////////
	
	void setConvex(bool value) {
		SFBool *convex = (SFBool *)getField(convexFieldString);
		convex->setValue(value);
	}

	void setConvex(int value) {
		setConvex(value ? true : false);
	}

	bool getConvex() {
		SFBool *convex = (SFBool *)getField(convexFieldString);
		return convex->getValue();
	}

	////////////////////////////////////////////////
	//	CreaseAngle
	////////////////////////////////////////////////
	
	void setCreaseAngle(float value) {
		SFFloat *creaseAngle = (SFFloat *)getField(creaseAngleFieldString);
		creaseAngle->setValue(value);
	}

	float getCreaseAngle() {
		SFFloat *creaseAngle = (SFFloat *)getField(creaseAngleFieldString);
		return creaseAngle->getValue();
	}

	////////////////////////////////////////////////
	//	Solid
	////////////////////////////////////////////////
	
	void setSolid(bool value) {
		SFBool *solid = (SFBool *)getField(solidFieldString);
		solid->setValue(value);
	}

	void setSolid(int value) {
		setSolid(value ? true : false);
	}

	bool getSolid() {
		SFBool *solid = (SFBool *)getField(solidFieldString);
		return solid->getValue();
	}

	////////////////////////////////////////////////
	// orientation
	////////////////////////////////////////////////

	void addOrientation(float value[]) {
		MFRotation *orientation = (MFRotation *)getField(orientationFieldString);
		orientation->addValue(value);
	}
	void addOrientation(float x, float y, float z, float angle) {
		MFRotation *orientation = (MFRotation *)getField(orientationFieldString);
		orientation->addValue(x, y, z, angle);
	}
	int getNOrientations() {
		MFRotation *orientation = (MFRotation *)getField(orientationFieldString);
		return orientation->getSize();
	}
	void getOrientation(int index, float value[]) {
		MFRotation *orientation = (MFRotation *)getField(orientationFieldString);
		orientation->get1Value(index, value);
	}

	////////////////////////////////////////////////
	// scale
	////////////////////////////////////////////////

	void addScale(float value[]) {
		MFVec2f *scale = (MFVec2f *)getField(scaleFieldString);
		scale->addValue(value);
	}
	void addScale(float x, float z) {
		MFVec2f *scale = (MFVec2f *)getField(scaleFieldString);
		scale->addValue(x, z);
	}
	int getNScales() {
		MFVec2f *scale = (MFVec2f *)getField(scaleFieldString);
		return scale->getSize();
	}
	void getScale(int index, float value[]) {
		MFVec2f *scale = (MFVec2f *)getField(scaleFieldString);
		scale->get1Value(index, value);
	}

	////////////////////////////////////////////////
	// crossSection
	////////////////////////////////////////////////

	void addCrossSection(float value[]) {
		MFVec2f *crossSection = (MFVec2f *)getField(crossSectionFieldString);
		crossSection->addValue(value);
	}
	void addCrossSection(float x, float z) {
		MFVec2f *crossSection = (MFVec2f *)getField(crossSectionFieldString);
		crossSection->addValue(x, z);
	}
	int getNCrossSections() {
		MFVec2f *crossSection = (MFVec2f *)getField(crossSectionFieldString);
		return crossSection->getSize();
	}
	void getCrossSection(int index, float value[]) {
		MFVec2f *crossSection = (MFVec2f *)getField(crossSectionFieldString);
		crossSection->get1Value(index, value);
	}

	////////////////////////////////////////////////
	// spine
	////////////////////////////////////////////////

	void addSpine(float value[]) {
		MFVec3f *spine = (MFVec3f *)getField(spineFieldString);
		spine->addValue(value);
	}
	void addSpine(float x, float y, float z) {
		MFVec3f *spine = (MFVec3f *)getField(spineFieldString);
		spine->addValue(x, y, z);
	}
	int getNSpines() {
		MFVec3f *spine = (MFVec3f *)getField(spineFieldString);
		return spine->getSize();
	}
	void getSpine(int index, float value[]) {
		MFVec3f *spine = (MFVec3f *)getField(spineFieldString);
		spine->get1Value(index, value);
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	ExtrusionNode *next() {
		return (ExtrusionNode *)Node::next(getType());
	}

	ExtrusionNode *nextTraversal() {
		return (ExtrusionNode *)Node::nextTraversalByType(getType());
	}

	////////////////////////////////////////////////
	//	functions
	////////////////////////////////////////////////
	
	bool isChildNodeType(Node *node){
		return false;
	}

	void initialize();

	void uninitialize() {
	}

	void update() {
	}

	////////////////////////////////////////////////
	//	BoundingBox
	////////////////////////////////////////////////

	void recomputeBoundingBox();

	////////////////////////////////////////////////
	//	recomputeDisplayList
	////////////////////////////////////////////////

	void recomputeDisplayList();

	////////////////////////////////////////////////
	//	Infomation
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
		SFBool *beginCap = (SFBool *)getField(beginCapFieldString);
		SFBool *endCap = (SFBool *)getField(endCapFieldString);
		SFBool *ccw = (SFBool *)getField(ccwFieldString);
		SFBool *convex = (SFBool *)getField(convexFieldString);
		SFBool *solid = (SFBool *)getField(solidFieldString);

		printStream << indentString << "\t" << "beginCap " << beginCap << endl;
		printStream << indentString << "\t" << "endCap " << endCap << endl;
		printStream << indentString << "\t" << "solid " << solid << endl;
		printStream << indentString << "\t" << "ccw " << ccw << endl;
		printStream << indentString << "\t" << "convex " << convex << endl;
		printStream << indentString << "\t" << "creaseAngle " << getCreaseAngle() << endl;

		if (0 < getNCrossSections()) {
			MFVec2f *crossSection = (MFVec2f *)getField(crossSectionFieldString);
			printStream << indentString << "\t" << "crossSection [" << endl;
			crossSection->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}

		if (0 < getNOrientations()) {
			MFRotation *orientation = (MFRotation *)getField(orientationFieldString);
			printStream << indentString << "\t" << "orientation [" << endl;
			orientation->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}

		if (0 < getNScales()) {
			MFVec2f *scale = (MFVec2f *)getField(scaleFieldString);
			printStream << indentString << "\t" << "scale [" << endl;
			scale->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}

		if (0 < getNSpines()) {
			MFVec3f *spine = (MFVec3f *)getField(spineFieldString);
			printStream << indentString << "\t" << "spine [" << endl;
			spine->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}
	}
};

#endif

