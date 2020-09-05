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
*	File:	TextNode.h
*
******************************************************************/

#ifndef _TEXT_H_
#define _TEXT_H_

#include "GeometryNode.h"
#include "FontStyleNode.h"

#if defined(SUPPORT_OPENGL) && defined(WIN32)
class OGLFontOutline : public CLinkedListNode<OGLFontOutline> {
private:
	int				mFamily;
	int				mStyle;
	unsigned int	mListBaseID;

public:

	OGLFontOutline(int family, int style, unsigned int id) {
		setFamily(family);
		setStyle(style);
		setListBaseID(id);
	}

	void setFamily(int family) {
		mFamily = family;
	}
	int getFamily() {
		return mFamily;
	}

	void setStyle(int style) {
		mStyle = style;
	}
	int getStyle() {
		return mStyle;
	}

	void setListBaseID(unsigned int id) {
		mListBaseID = id;
	}
	int getListBaseID() {
		return mListBaseID;
	}

	OGLFontOutline *next() {
		return CLinkedListNode<OGLFontOutline>::next(); 
	}
};
#endif

class TextNode : public GeometryNode {
	
public:

	TextNode() {
		setHeaderFlag(false);
		setType(textNodeString);

		///////////////////////////
		// ExposedField 
		///////////////////////////

		// maxExtent exposed field
		SFFloat *maxExtent = new SFFloat(1.0f);
		addExposedField(maxExtentFieldString, maxExtent);

		// length exposed field
		MFFloat *length = new MFFloat();
		addExposedField(lengthFieldString, length);

		// string exposed field
		MFString *string = new MFString();
		addExposedField(stringFieldString, string);
	}

	~TextNode() {
	}

	////////////////////////////////////////////////
	//	MaxExtent
	////////////////////////////////////////////////
	
	void setMaxExtent(float value) {
		SFFloat *sffloat = (SFFloat *)getExposedField(maxExtentFieldString);
		sffloat->setValue(value);
	}
	float getMaxExtent() {
		SFFloat *sffloat = (SFFloat *)getExposedField(maxExtentFieldString);
		return sffloat->getValue();
	} 

	////////////////////////////////////////////////
	// String
	////////////////////////////////////////////////

	void addString(String value) {
		MFString *string = (MFString *)getExposedField(stringFieldString);
		string->addValue(value);
	}
	int getNStrings() {
		MFString *string = (MFString *)getExposedField(stringFieldString);
		return string->getSize();
	}
	String getString(int index) {
		MFString *string = (MFString *)getExposedField(stringFieldString);
		return string->get1Value(index);
	}
	void setString(int index, char* value) {
		MFString *string = (MFString *)getExposedField(stringFieldString);
		string->set1Value(index, value);
	}

	////////////////////////////////////////////////
	// length
	////////////////////////////////////////////////

	void addLength(float value) {
		MFFloat *length = (MFFloat *)getExposedField(lengthFieldString);
		length->addValue(value);
	}
	int getNLengths() {
		MFFloat *length = (MFFloat *)getExposedField(lengthFieldString);
		return length->getSize();
	}
	float getLength(int index) {
		MFFloat *length = (MFFloat *)getExposedField(lengthFieldString);
		return length->get1Value(index);
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	TextNode *next() {
		return (TextNode *)Node::next(getType());
	}

	TextNode *nextTraversal() {
		return (TextNode *)Node::nextTraversalByType(getType());
	}

	////////////////////////////////////////////////
	//	functions
	////////////////////////////////////////////////
	
	bool isChildNodeType(Node *node){
		if (node->isFontStyleNode())
			return true;
		else
			return false;
	}

	void initialize() {
		recomputeBoundingBox();
#ifdef SUPPORT_OPENGL
		recomputeDisplayList();
#endif
	}

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

#ifdef SUPPORT_OPENGL
	void recomputeDisplayList();
#endif

	////////////////////////////////////////////////
	//	FontStyle
	////////////////////////////////////////////////

	int getFontStyleFamilyNumber() {
		FontStyleNode *fontStyle = getFontStyleNodes();
		
		if (fontStyle == NULL)
			return FONTSTYLE_FAMILY_SERIF;

		return fontStyle->getFamilyNumber();
	}

	int getFontStyleStyleNumber() {
		FontStyleNode *fontStyle = getFontStyleNodes();
		
		if (fontStyle == NULL)
			return FONTSTYLE_STYLE_PLAIN;

		return fontStyle->getStyleNumber();
	}

	////////////////////////////////////////////////
	//	SUPPORT_OPENGL
	////////////////////////////////////////////////

#ifdef SUPPORT_OPENGL
	void draw();
#endif

#if defined(SUPPORT_OPENGL) && defined(WIN32)
	static CLinkedList<OGLFontOutline>	*mOGLFontOutlines;

	OGLFontOutline *getOGLFontOutlines() {
		return mOGLFontOutlines->getNodes();
	}

	OGLFontOutline *getOGLFontOutline(int family, int style) {
		for (OGLFontOutline *node = getOGLFontOutlines(); node != NULL; node = node->next()) {
			if (family == node->getFamily() && style == node->getStyle())
				return node;
		}
		return NULL;
	}

	void addOGLFontOutline(OGLFontOutline *node) {
		mOGLFontOutlines->addNode(node); 
	}

	unsigned int createUseFontOutline(int family, int style);

	void addOGLFontOutline(int family, int style, unsigned int id) {
		addOGLFontOutline(new OGLFontOutline(family, style, id));
	}

	int getNOGLFontOutlines() {
		return mOGLFontOutlines->getNNodes();
	}
#endif

	////////////////////////////////////////////////
	//	Stringmation
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
		printStream << indentString << "\t" << "maxExtent " << getMaxExtent() << endl;

		if (0 < getNStrings()) {
			MFString *string = (MFString *)getExposedField(stringFieldString);
			printStream << indentString << "\t" << "string [" << endl;
			string->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]"<< endl;
		}

		if (0 < getNLengths()) {
			MFFloat *length = (MFFloat *)getExposedField(lengthFieldString);
			printStream << indentString << "\t" << "length [" << endl;
			length->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]"<< endl;
		}

		FontStyleNode *fontStyle = getFontStyleNodes();
		if (fontStyle != NULL) {
			if (fontStyle->isInstanceNode() == false) {
				if (fontStyle->getName() != NULL && strlen(fontStyle->getName()))
					printStream << indentString << "\t" << "fontStyle " << "DEF " << fontStyle->getName() << " FontStyle {" << endl;
				else
					printStream << indentString << "\t" << "fontStyle FontStyle {"<< endl;
				fontStyle->Node::outputContext(printStream, indentString, "\t");
				printStream << indentString << "\t" << "}" << endl;
			}
			else 
				printStream << indentString << "\t" << "fontStyle USE " << fontStyle->getName() << endl;
		}
	}
};

#endif

