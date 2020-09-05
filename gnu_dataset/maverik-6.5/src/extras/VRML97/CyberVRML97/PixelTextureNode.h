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
*	File:	PixelTextureNode.h
*
******************************************************************/

#ifndef _PIXELTEXTURE_H_
#define _PIXELTEXTURE_H_

#include "vrmlfields.h"
#include "Node.h"

class PixelTextureNode : public Node {
	
public:

        SFImage *ci; // JMC

	PixelTextureNode() {
	        ci=NULL; // JMC
		setHeaderFlag(false);
		setType(pixelTextureNodeString);

		///////////////////////////
		// Exposed Field 
		///////////////////////////

		// image field
		SFImage *image = new SFImage();
		addExposedField(imageFieldString, image);

		///////////////////////////
		// Field 
		///////////////////////////

		// repeatS field
		SFBool *repeatS = new SFBool(true);
		addField(repeatSFieldString, repeatS);

		// repeatT field
		SFBool *repeatT = new SFBool(true);
		addField(repeatTFieldString, repeatT);
	}

	~PixelTextureNode() {
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
	// Image
	////////////////////////////////////////////////

	void addImage(int value) {
		SFImage *image = (SFImage *)getExposedField(imageFieldString);
		image->addValue(value);
	}
	int getNImages() {
		SFImage *image = (SFImage *)getExposedField(imageFieldString);
		return image->getSize();
	}
	int getImage(int index) {
		SFImage *image = (SFImage *)getExposedField(imageFieldString);
		return image->get1Value(index);
	}
	int getImageNext() { // JMC
	  if (!ci) ci = (SFImage *)getExposedField(imageFieldString); // JMC
	  return ci->get1ValueNext(); // JMC
	} // JMC

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	PixelTextureNode *next() {
		return (PixelTextureNode *)Node::next(getType());
	}

	PixelTextureNode *nextTraversal() {
		return (PixelTextureNode *)Node::nextTraversalByType(getType());
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
	//	Imagemation
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
		SFBool *repeatS = (SFBool *)getField(repeatSFieldString);
		SFBool *repeatT = (SFBool *)getField(repeatTFieldString);

		if (0 < getNImages()) {
			SFImage *image = (SFImage *)getExposedField(imageFieldString);
			printStream << indentString << "\t" << "image [" << endl;
			image->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]"<< endl;
		}

		printStream << indentString << "\t" << "repeatS " << repeatS << endl;
		printStream << indentString << "\t" << "repeatT " << repeatT << endl;
	}
};

#endif

