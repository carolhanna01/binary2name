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
*	File:	AppearanceNode.h
*
******************************************************************/

#ifndef _APPEARANCE_H_
#define _APPEARANCE_H_

#include "vrmlfields.h"
#include "Node.h"
#include "MaterialNode.h"
#include "TextureTransformNode.h"

class AppearanceNode : public Node {

public:

	AppearanceNode() {
		setHeaderFlag(false);
		setType(appearanceNodeString);
	}

	~AppearanceNode() {
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	AppearanceNode *next() {
		return (AppearanceNode *)Node::next(getType());
	}

	AppearanceNode *nextTraversal() {
		return (AppearanceNode *)Node::nextTraversalByType(getType());
	}

	////////////////////////////////////////////////
	//	virtual functions
	////////////////////////////////////////////////
	
	bool isChildNodeType(Node *node){
		if (node->isMaterialNode() || node->isTextureNode() || node->isTextureTransformNode())
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

	void outputContext(ostream &printStream, String indentString) {
		MaterialNode *material = getMaterialNodes();
		if (material != NULL) {
			if (material->isInstanceNode() == false) {
				if (material->getName() != NULL && strlen(material->getName()))
					printStream << indentString << "\t" << "material " << "DEF " << material->getName() << " Material {" << endl;
				else
					printStream << indentString << "\t" << "material Material {" << endl;
				material->Node::outputContext(printStream, indentString, "\t");
				printStream << indentString << "\t" << "}" << endl;
			}
			else 
				printStream << indentString << "\t" << "material USE " << material->getName() << endl;
		}

		Node *texture = getTextureNode();
		if (texture != NULL) {
			if (texture->isInstanceNode() == false) {
				if (texture->getName() != NULL && strlen(texture->getName()))
					printStream << indentString << "\t" << "texture " << "DEF " << texture->getName() << " " << texture->Node::getType() << " {" << endl;
				else
					printStream << indentString << "\t" << "texture " << texture->Node::getType() << " {" << endl;
				texture->Node::outputContext(printStream, indentString, "\t");
				printStream << indentString << "\t" << "}" << endl;
			}
			else 
				printStream << indentString << "\t" << "texture USE " << texture->getName() << endl;
		}

		TextureTransformNode *textureTransform = getTextureTransformNodes();
		if (textureTransform != NULL) {
			if (textureTransform->isInstanceNode() == false) {
				if (textureTransform->getName() != NULL && strlen(textureTransform->getName()))
					printStream << indentString << "\t" << "textureTransform " << "DEF " << textureTransform->getName() << " TextureTransform {" << endl;
				else
					printStream << indentString << "\t" << "textureTransform TextureTransform {" << endl;
				textureTransform->Node::outputContext(printStream, indentString, "\t");
				printStream << indentString << "\t" << "}" << endl;
			}
			else 
				printStream << indentString << "\t" << "textureTransform USE " << textureTransform->getName() << endl;
		}
	}
};

#endif
