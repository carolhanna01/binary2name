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
*	File:	BackgroundNode.h
*
******************************************************************/

#ifndef _BACKGROUND_H_
#define _BACKGROUND_H_

#include "BindableNode.h"

class BackgroundNode : public BindableNode {
	
public:

	BackgroundNode() {
		setHeaderFlag(false);
		setType(backgroundNodeString);

		// groundColor exposed field
		MFColor *groundColor = new MFColor();
		addExposedField(groundColorFieldString, groundColor);

		// skyColor exposed field
		MFColor *skyColor = new MFColor();
		addExposedField(skyColorFieldString, skyColor);

		// groundAngle exposed field
		MFFloat *groundAngle = new MFFloat();
		addExposedField(groundAngleFieldString, groundAngle);

		// skyAngle exposed field
		MFFloat *skyAngle = new MFFloat();
		addExposedField(skyAngleFieldString, skyAngle);

		// url exposed field
		MFString *frontUrl = new MFString();
		addExposedField(frontUrlFieldString, frontUrl);

		// url exposed field
		MFString *backUrl = new MFString();
		addExposedField(backUrlFieldString, backUrl);

		// url exposed field
		MFString *leftUrl = new MFString();
		addExposedField(leftUrlFieldString, leftUrl);

		// url exposed field
		MFString *rightUrl = new MFString();
		addExposedField(rightUrlFieldString, rightUrl);

		// url exposed field
		MFString *topUrl = new MFString();
		addExposedField(topUrlFieldString, topUrl);

		// url exposed field
		MFString *bottomUrl = new MFString();
		addExposedField(bottomUrlFieldString, bottomUrl);
	}

	~BackgroundNode() {
	}

	////////////////////////////////////////////////
	// groundColor
	////////////////////////////////////////////////

	void addGroundColor(float value[]) {
		MFColor *groundColor = (MFColor *)getExposedField(groundColorFieldString);
		groundColor->addValue(value);
	}
	int getNGroundColors() {
		MFColor *groundColor = (MFColor *)getExposedField(groundColorFieldString);
		return groundColor->getSize();
	}
	void getGroundColor(int index, float value[]) {
		MFColor *groundColor = (MFColor *)getExposedField(groundColorFieldString);
		groundColor->get1Value(index, value);
	}

	////////////////////////////////////////////////
	// skyColor
	////////////////////////////////////////////////

	void addSkyColor(float value[]) {
		MFColor *skyColor = (MFColor *)getExposedField(skyColorFieldString);
		skyColor->addValue(value);
	}
	int getNSkyColors() {
		MFColor *skyColor = (MFColor *)getExposedField(skyColorFieldString);
		return skyColor->getSize();
	}
	void getSkyColor(int index, float value[]) {
		MFColor *skyColor = (MFColor *)getExposedField(skyColorFieldString);
		skyColor->get1Value(index, value);
	}

	////////////////////////////////////////////////
	// groundAngle
	////////////////////////////////////////////////

	void addGroundAngle(float value) {
		MFFloat *groundAngle = (MFFloat *)getExposedField(groundAngleFieldString);
		groundAngle->addValue(value);
	}
	int getNGroundAngles() {
		MFFloat *groundAngle = (MFFloat *)getExposedField(groundAngleFieldString);
		return groundAngle->getSize();
	}
	float getGroundAngle(int index) {
		MFFloat *groundAngle = (MFFloat *)getExposedField(groundAngleFieldString);
		return groundAngle->get1Value(index);
	}

	////////////////////////////////////////////////
	// skyAngle
	////////////////////////////////////////////////

	void addSkyAngle(float value) {
		MFFloat *skyAngle = (MFFloat *)getExposedField(skyAngleFieldString);
		skyAngle->addValue(value);
	}
	int getNSkyAngles() {
		MFFloat *skyAngle = (MFFloat *)getExposedField(skyAngleFieldString);
		return skyAngle->getSize();
	}
	float getSkyAngle(int index) {
		MFFloat *skyAngle = (MFFloat *)getExposedField(skyAngleFieldString);
		return skyAngle->get1Value(index);
	}

	////////////////////////////////////////////////
	// frontUrl
	////////////////////////////////////////////////

	void addFrontUrl(String value) {
		MFString *frontUrl = (MFString *)getExposedField(frontUrlFieldString);
		frontUrl->addValue(value);
	}
	int getNFrontUrls() {
		MFString *frontUrl = (MFString *)getExposedField(frontUrlFieldString);
		return frontUrl->getSize();
	}
	String getFrontUrl(int index) {
		MFString *frontUrl = (MFString *)getExposedField(frontUrlFieldString);
		return frontUrl->get1Value(index);
	}

	////////////////////////////////////////////////
	// backUrl
	////////////////////////////////////////////////

	void addBackUrl(String value) {
		MFString *backUrl = (MFString *)getExposedField(backUrlFieldString);
		backUrl->addValue(value);
	}
	int getNBackUrls() {
		MFString *backUrl = (MFString *)getExposedField(backUrlFieldString);
		return backUrl->getSize();
	}
	String getBackUrl(int index) {
		MFString *backUrl = (MFString *)getExposedField(backUrlFieldString);
		return backUrl->get1Value(index);
	}

	////////////////////////////////////////////////
	// leftUrl
	////////////////////////////////////////////////

	void addLeftUrl(String value) {
		MFString *leftUrl = (MFString *)getExposedField(leftUrlFieldString);
		leftUrl->addValue(value);
	}
	int getNLeftUrls() {
		MFString *leftUrl = (MFString *)getExposedField(leftUrlFieldString);
		return leftUrl->getSize();
	}
	String getLeftUrl(int index) {
		MFString *leftUrl = (MFString *)getExposedField(leftUrlFieldString);
		return leftUrl->get1Value(index);
	}

	////////////////////////////////////////////////
	// rightUrl
	////////////////////////////////////////////////

	void addRightUrl(String value) {
		MFString *rightUrl = (MFString *)getExposedField(rightUrlFieldString);
		rightUrl->addValue(value);
	}
	int getNRightUrls() {
		MFString *rightUrl = (MFString *)getExposedField(rightUrlFieldString);
		return rightUrl->getSize();
	}
	String getRightUrl(int index) {
		MFString *rightUrl = (MFString *)getExposedField(rightUrlFieldString);
		return rightUrl->get1Value(index);
	}

	////////////////////////////////////////////////
	// topUrl
	////////////////////////////////////////////////

	void addTopUrl(String value) {
		MFString *topUrl = (MFString *)getExposedField(topUrlFieldString);
		topUrl->addValue(value);
	}
	int getNTopUrls() {
		MFString *topUrl = (MFString *)getExposedField(topUrlFieldString);
		return topUrl->getSize();
	}
	String getTopUrl(int index) {
		MFString *topUrl = (MFString *)getExposedField(topUrlFieldString);
		return topUrl->get1Value(index);
	}

	////////////////////////////////////////////////
	// bottomUrl
	////////////////////////////////////////////////

	void addBottomUrl(String value) {
		MFString *bottomUrl = (MFString *)getExposedField(bottomUrlFieldString);
		bottomUrl->addValue(value);
	}
	int getNBottomUrls() {
		MFString *bottomUrl = (MFString *)getExposedField(bottomUrlFieldString);
		return bottomUrl->getSize();
	}
	String getBottomUrl(int index) {
		MFString *bottomUrl = (MFString *)getExposedField(bottomUrlFieldString);
		return bottomUrl->get1Value(index);
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	BackgroundNode *next() {
		return (BackgroundNode *)Node::next(getType());
	}

	BackgroundNode *nextTraversal() {
		return (BackgroundNode *)Node::nextTraversalByType(getType());
	}

	////////////////////////////////////////////////
	//	Virtual functions
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

	void outputContext(ostream &printStream, String indentString) {

		if (0 < getNGroundColors()) {
			MFColor *groundColor = (MFColor *)getExposedField(groundColorFieldString);
			printStream << indentString << "\t" << "groundColor [" << endl;
			groundColor->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}

		if (0 < getNSkyColors()) {
			MFColor *skyColor = (MFColor *)getExposedField(skyColorFieldString);
			printStream << indentString << "\t" << "skyColor [" << endl;
			skyColor->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}

		if (0 < getNGroundAngles()) {
			MFFloat *groundAngle = (MFFloat *)getExposedField(groundAngleFieldString);
			printStream << indentString << "\t" << "groundAngle [" << endl;
			groundAngle->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}

		if (0 < getNSkyAngles()) {
			MFFloat *skyAngle = (MFFloat *)getExposedField(skyAngleFieldString);
			printStream << indentString << "\t" << "skyAngle [" << endl;
			skyAngle->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}

		if (0 < getNFrontUrls()) {
			MFString *frontUrl = (MFString *)getExposedField(frontUrlFieldString);
			printStream << indentString << "\t" << "frontUrl [" << endl;
			frontUrl->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}

		if (0 < getNBackUrls()) {
			MFString *backUrl = (MFString *)getExposedField(backUrlFieldString);
			printStream << indentString << "\t" << "backUrl [" << endl;
			backUrl->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}

		if (0 < getNLeftUrls()) {
			MFString *leftUrl = (MFString *)getExposedField(leftUrlFieldString);
			printStream << indentString << "\t" << "leftUrl [" << endl;
			leftUrl->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}

		if (0 < getNRightUrls()) {
			MFString *rightUrl = (MFString *)getExposedField(rightUrlFieldString);
			printStream << indentString << "\t" << "rightUrl [" << endl;
			rightUrl->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}

		if (0 < getNTopUrls()) {
			MFString *topUrl = (MFString *)getExposedField(topUrlFieldString);
			printStream << indentString << "\t" << "topUrl [" << endl;
			topUrl->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}

		if (0 < getNBottomUrls()) {
			MFString *bottomUrl = (MFString *)getExposedField(bottomUrlFieldString);
			printStream << indentString << "\t" << "bottomUrl [" << endl;
			bottomUrl->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}
	}
};

#endif

