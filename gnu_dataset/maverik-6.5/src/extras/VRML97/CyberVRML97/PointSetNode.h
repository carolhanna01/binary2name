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
*	File:	PointSetNode.h
*
******************************************************************/

#ifndef _POINTSET_H_
#define _POINTSET_H_

#include "GeometryNode.h"
#include "ColorNode.h"
#include "CoordinateNode.h"

class PointSetNode : public GeometryNode {

public:

	PointSetNode() {
		setHeaderFlag(false);
		setType(pointSetNodeString);
	}

	~PointSetNode() {
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	PointSetNode *next() {
		return (PointSetNode *)Node::next(getType());
	}

	PointSetNode *nextTraversal() {
		return (PointSetNode *)Node::nextTraversalByType(getType());
	}

	////////////////////////////////////////////////
	//	functions
	////////////////////////////////////////////////
	
	bool isChildNodeType(Node *node){
		if (node->isCoordinateNode() || node->isColorNode())
			return true;
		else
			return false;
	}

	void initialize() {
		if (!isInitialized()) {
#ifdef SUPPORT_OPENGL
			recomputeDisplayList();
#endif
			recomputeBoundingBox();
			setInitialized(1);
		}
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
	//	Infomation
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
		ColorNode *color = getColorNodes();
		if (color != NULL) {
			if (color->isInstanceNode() == false) {
				if (color->getName() != NULL && strlen(color->getName()))
					printStream << indentString << "\t" << "color " << "DEF " << color->getName() << " Color {" << endl;
				else
					printStream << indentString << "\t" << "color Color {" << endl;
				color->Node::outputContext(printStream, indentString, "\t");
				printStream << indentString << "\t" << "}" << endl;
			}
			else 
				printStream << indentString << "\t" << "color USE " << color->getName() << endl;
		}

		CoordinateNode *coord = getCoordinateNodes();
		if (coord != NULL) {
			if (coord->isInstanceNode() == false) {
				if (coord->getName() != NULL && strlen(coord->getName()))
					printStream << indentString << "\t" << "coord " << "DEF " << coord->getName() << " Coordinate {" << endl;
				else
					printStream << indentString << "\t" << "coord Coordinate {" << endl;
				coord->Node::outputContext(printStream, indentString, "\t");
				printStream << indentString << "\t" << "}" << endl;
			}
			else 
				printStream << indentString << "\t" << "coord USE " << coord->getName() << endl;
		}
	}
};

#endif

