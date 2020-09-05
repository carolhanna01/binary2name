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
*	File:	FontStyleNode.h
*
******************************************************************/

#ifndef _FONTSTYLE_H_
#define _FONTSTYLE_H_

#include "vrmlfields.h"
#include "Node.h"

enum {
FONTSTYLE_FAMILY_SERIF,
FONTSTYLE_FAMILY_SANS,
FONTSTYLE_FAMILY_TYPEWRITER,
};

enum {
FONTSTYLE_STYLE_PLAIN,
FONTSTYLE_STYLE_BOLD,
FONTSTYLE_STYLE_ITALIC,
FONTSTYLE_STYLE_BOLDITALIC,
};

enum {
FONTSTYLE_JUSTIFY_BEGIN,
FONTSTYLE_JUSTIFY_MIDDLE,
FONTSTYLE_JUSTIFY_END,
FONTSTYLE_JUSTIFY_FIRST,
};

class FontStyleNode : public Node {
	
public:

	FontStyleNode() {
		setHeaderFlag(false);
		setType(fontStyleNodeString);

		///////////////////////////
		// Field 
		///////////////////////////

		// family field
		SFString *family = new SFString("SERIF");
		family->setName(familyFieldString);
		addField(family);

		// style field
		SFString *style = new SFString("PLAIN");
		style->setName(styleFieldString);
		addField(style);

		// language field
		SFString *language = new SFString("");
		language->setName(languageFieldString);
		addField(language);

		// justify field
		MFString *justify = new MFString();
		justify->setName(justifyFieldString);
		addField(justify);

		// size field
		SFFloat *size = new SFFloat(1.0f);
		addField(sizeFieldString, size);

		// spacing field
		SFFloat *spacing = new SFFloat(1.0f);
		addField(spacingFieldString, spacing);

		// horizontal field
		SFBool *horizontal = new SFBool(true);
		addField(horizontalFieldString, horizontal);

		// leftToRight field
		SFBool *leftToRight = new SFBool(true);
		addField(leftToRightFieldString, leftToRight);

		// topToBottom field
		SFBool *topToBottom = new SFBool(true);
		addField(topToBottomFieldString, topToBottom);
	}

	~FontStyleNode() {
	}

	////////////////////////////////////////////////
	//	Size
	////////////////////////////////////////////////

	void setSize(float value) {
		SFFloat *size = (SFFloat *)getField(sizeFieldString);
		size->setValue(value);
	}
	float getSize() {
		SFFloat *size = (SFFloat *)getField(sizeFieldString);
		return size->getValue();
	}

	////////////////////////////////////////////////
	//	Family
	////////////////////////////////////////////////
	
	void setFamily(String value) {
		SFString *family = (SFString *)getField(familyFieldString);
		family->setValue(value);
	}
	String getFamily() {
		SFString *family = (SFString *)getField(familyFieldString);
		return family->getValue();
	}
	int getFamilyNumber();

	////////////////////////////////////////////////
	//	Style
	////////////////////////////////////////////////
	
	void setStyle(String value) {
		SFString *style = (SFString *)getField(styleFieldString);
		style->setValue(value);
	}
	String getStyle() {
		SFString *style = (SFString *)getField(styleFieldString);
		return style->getValue();
	}
	int getStyleNumber();

	////////////////////////////////////////////////
	//	Language
	////////////////////////////////////////////////
	
	void setLanguage(String value) {
		SFString *language = (SFString *)getField(languageFieldString);
		language->setValue(value);
	}
	String getLanguage() {
		SFString *language = (SFString *)getField(languageFieldString);
		return language->getValue();
	}

	////////////////////////////////////////////////
	//	Horizontal
	////////////////////////////////////////////////
	
	void setHorizontal(bool value) {
		SFBool *horizontal = (SFBool *)getField(horizontalFieldString);
		horizontal->setValue(value);
	}
	void setHorizontal(int value) {
		setHorizontal(value ? true : false);
	}
	bool getHorizontal() {
		SFBool *horizontal = (SFBool *)getField(horizontalFieldString);
		return horizontal->getValue();
	}

	////////////////////////////////////////////////
	//	LeftToRight
	////////////////////////////////////////////////
	
	void setLeftToRight(bool value) {
		SFBool *leftToRight = (SFBool *)getField(leftToRightFieldString);
		leftToRight->setValue(value);
	}
	void setLeftToRight(int value) {
		setLeftToRight(value ? true : false);
	}
	bool getLeftToRight() {
		SFBool *leftToRight = (SFBool *)getField(leftToRightFieldString);
		return leftToRight->getValue();
	}

	////////////////////////////////////////////////
	//	TopToBottom
	////////////////////////////////////////////////
	
	void setTopToBottom(bool value) {
		SFBool *topToBottom = (SFBool *)getField(topToBottomFieldString);
		topToBottom->setValue(value);
	}
	void setTopToBottom(int value) {
		setTopToBottom(value ? true : false);
	}
	bool getTopToBottom() {
		SFBool *topToBottom = (SFBool *)getField(topToBottomFieldString);
		return topToBottom->getValue();
	}

	////////////////////////////////////////////////
	// Justify
	////////////////////////////////////////////////

	void addJustify(String value) {
		MFString *justify = (MFString *)getField(justifyFieldString);
		justify->addValue(value);
	}
	int getNJustifys() {
		MFString *justify = (MFString *)getField(justifyFieldString);
		return justify->getSize();
	}
	String getJustify(int index) {
		MFString *justify = (MFString *)getField(justifyFieldString);
		return justify->get1Value(index);
	}

	////////////////////////////////////////////////
	//	Spacing
	////////////////////////////////////////////////

	void setSpacing(float value) {
		SFFloat *spacing = (SFFloat *)getField(spacingFieldString);
		spacing->setValue(value);
	}
	float getSpacing() {
		SFFloat *spacing = (SFFloat *)getField(spacingFieldString);
		return spacing->getValue();
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	FontStyleNode *next() {
		return (FontStyleNode *)Node::next(getType());
	}

	FontStyleNode *nextTraversal() {
		return (FontStyleNode *)Node::nextTraversalByType(getType());
	}

	////////////////////////////////////////////////
	//	functions
	////////////////////////////////////////////////
	
	bool isChildNodeType(Node *node){
		return false;
	}

	void initialize() {
		Node *parentNode = getParentNode();
		if (parentNode != NULL) {
			if (parentNode->isTextNode())
				parentNode->initialize();
		}
	}

	void uninitialize() {
	}

	void update() {
	}

	////////////////////////////////////////////////
	//	Justifymation
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
		SFString *family = (SFString *)getField(familyFieldString);
		SFBool *horizontal = (SFBool *)getField(horizontalFieldString);
		SFBool *leftToRight = (SFBool *)getField(leftToRightFieldString);
		SFBool *topToBottom = (SFBool *)getField(topToBottomFieldString);
		SFString *style = (SFString *)getField(styleFieldString);
		SFString *language = (SFString *)getField(languageFieldString);

		printStream << indentString << "\t" << "size " << getSize() << endl;
		printStream << indentString << "\t" << "family " << family << endl;
		printStream << indentString << "\t" << "style " << style << endl;
		printStream << indentString << "\t" << "horizontal " << horizontal << endl;
		printStream << indentString << "\t" << "leftToRight " << leftToRight << endl;
		printStream << indentString << "\t" << "topToBottom " << topToBottom << endl;
		printStream << indentString << "\t" << "language " << language << endl;
		printStream << indentString << "\t" << "spacing " << getSpacing() << endl;

		if (0 < getNJustifys()) { 
			MFString *justify = (MFString *)getField(justifyFieldString);
			printStream << indentString << "\t" << "justify [" << endl;
			justify->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}
	}
};

#endif

