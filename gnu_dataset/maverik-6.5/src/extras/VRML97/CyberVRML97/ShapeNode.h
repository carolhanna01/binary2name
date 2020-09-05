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
*	File:	ShapeNode.h
*
******************************************************************/

#ifndef _SHAPE_H_
#define _SHAPE_H_

#include "vrmlfields.h"
#include "Node.h"
#include "AppearanceNode.h"
#include "GeometryNode.h"

class ShapeNode : public Node {

public:

	ShapeNode() {
		setHeaderFlag(false);
		setType(shapeNodeString);
	}

	~ShapeNode() {
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	ShapeNode *next() {
		return (ShapeNode *)Node::next(getType());
	}

	ShapeNode *nextTraversal() {
		return (ShapeNode *)Node::nextTraversalByType(getType());
	}

	////////////////////////////////////////////////
	//	Geometry
	////////////////////////////////////////////////

	GeometryNode *getGeometry() {
		for (Node *node=getChildNodes(); node; node=node->next()) {
			if (node->isGeometryNode())
				return (GeometryNode *)node;
		}
		return NULL;
	}

	////////////////////////////////////////////////
	//	functions
	////////////////////////////////////////////////
	
	bool isChildNodeType(Node *node){
		if (node->isAppearanceNode() || node->isGeometryNode())
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
		AppearanceNode *appearance = getAppearanceNodes();
		if (appearance != NULL) {
			if (appearance->isInstanceNode() == false) {
				if (appearance->getName() != NULL && strlen(appearance->getName()))
					printStream << indentString << "\t" << "appearance " << "DEF " << appearance->getName() << " Appearance {" << endl;
				else
					printStream << indentString << "\t" << "appearance Appearance {" << endl;
				appearance->Node::outputContext(printStream, indentString, "\t");
				printStream << indentString << "\t" << "}" << endl;
			}
			else 
				printStream << indentString << "\t" << "appearance USE " << appearance->getName() << endl;
		}
		
		Node *node = getGeometryNode();
		if (node != NULL) {
			if (node->isInstanceNode() == false) {
				if (node->getName() != NULL && strlen(node->getName()))
					printStream << indentString << "\t" << "geometry " << "DEF " << node->getName() << " " << node->Node::getType() << " {" << endl;
				else
					printStream << indentString << "\t" << "geometry " << node->getType() << " {" << endl;
				node->Node::outputContext(printStream, indentString, "\t");
				printStream << indentString << "\t" << "}" << endl;
			}
			else 
				printStream << indentString << "\t" << "geometry USE " << node->getName() << endl;
		}
	}
};

#endif

