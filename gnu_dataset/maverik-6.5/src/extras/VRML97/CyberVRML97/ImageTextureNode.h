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
*	File:	ImageTextureNode.h
*
******************************************************************/

#ifndef _IMAGETEXTURE_H_
#define _IMAGETEXTURE_H_

#include "vrmlfields.h"
#include "Node.h"
#include "FileImage.h"

#ifdef SUPPORT_OPENGL
#define	textureNamePrivateFieldString			"oglTextureName"
#define	hasTransparencyColorPrivateFieldString	"hasTransparencyColor"
#endif

class ImageTextureNode : public Node {

	int			mWidth;
	int			mHeight;
	FileImage	*mFileImage;
	RGBAColor	*mImageBuffer;

public:

	ImageTextureNode();

	~ImageTextureNode();

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
	//	List
	////////////////////////////////////////////////

	ImageTextureNode *next() {
		return (ImageTextureNode *)Node::next(getType());
	}

	ImageTextureNode *nextTraversal() {
		return (ImageTextureNode *)Node::nextTraversalByType(getType());
	}

	////////////////////////////////////////////////
	//	Image
	////////////////////////////////////////////////

	bool		createImage();
	int			getWidth()		{ return mWidth; }
	int			getHeight()		{ return mHeight; }
	RGBAColor	*getImage()		{ return mImageBuffer;}
	FileImage	*getFileImage()	{ return mFileImage;}

	////////////////////////////////////////////////
	//	virtual functions
	////////////////////////////////////////////////
	
	bool isChildNodeType(Node *node){
		return false;
	}

	void initialize();

	void uninitialize();

	void update() {
	}

#ifdef SUPPORT_OPENGL

	////////////////////////////////////////////////
	//	TextureName
	////////////////////////////////////////////////

	void setTextureName(unsigned int n) {
		SFInt32 *texName = (SFInt32 *)getPrivateField(textureNamePrivateFieldString);
		texName->setValue((int)n);
	}

	unsigned int getTextureName() {
		SFInt32 *texName = (SFInt32 *)getPrivateField(textureNamePrivateFieldString);
		return (unsigned int)texName->getValue();
	} 

	////////////////////////////////////////////////
	//	TextureName
	////////////////////////////////////////////////

	void setHasTransparencyColor(bool value) {
		SFBool *transCol = (SFBool *)getPrivateField(hasTransparencyColorPrivateFieldString);
		transCol->setValue(value);
	}

	bool hasTransparencyColor() {
		SFBool *transCol = (SFBool *)getPrivateField(hasTransparencyColorPrivateFieldString);
		return transCol->getValue();
	} 

#endif

	////////////////////////////////////////////////
	//	infomation
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
		SFBool *repeatS = (SFBool *)getField(repeatSFieldString);
		SFBool *repeatT = (SFBool *)getField(repeatTFieldString);

		printStream << indentString << "\t" << "repeatS " << repeatS  << endl;
		printStream << indentString << "\t" << "repeatT " << repeatT  << endl;

		if (0 < getNUrls()) {
			MFString *url = (MFString *)getExposedField(urlFieldString);
			printStream << indentString << "\t" << "url [" << endl;
			url->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]"  << endl;
		}
	}
};

#endif

